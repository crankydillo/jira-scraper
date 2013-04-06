package org.beeherd.project.management.jira

import scala.xml.XML

import org.apache.http.{
  HttpException, HttpHost, HttpRequest => ApacheHttpRequest, HttpRequestInterceptor
}
import org.apache.http.auth._
import org.apache.http.client.CredentialsProvider
import org.apache.http.client.protocol.ClientContext
import org.apache.http.impl.auth.BasicScheme
import org.apache.http.protocol.{
  ExecutionContext, HttpContext
}
import org.apache.log4j.Logger

import org.beeherd.cli.utils.Tablizer
import org.beeherd.client.XmlResponse
import org.beeherd.client.http._
import org.beeherd.metrics.sonar._
import org.beeherd.metrics.vcs.{
  CommitterMetrics, SubversionMetrics
}
import org.joda.time.DateTime
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class JiraApp

/**
 * A CLI that uses a VCS client to determine what projects, represented by
 * paths, have changed over some period of time.  If the projects (paths) can be
 * used to identify Sonar projects, Sonar metrics can be displayed.
 */
object JiraApp {
  import net.liftweb.json.parse

  private val Log = Logger.getLogger(classOf[JiraApp])
  private val DateFormat = "yyyy-MM-dd"

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    conf.initialize {
      case Help(c) => conf.printHelp; System.exit(0)
      case Version => println("JIRA Stuff 1.0"); System.exit(0)
      case RequiredOptionNotFound(o) => {
        println(o + " is required."); 
        println("Use --help for more information")
        System.exit(1)
      }
      case ScallopException(m) => println(m); System.exit(1);
    }

    val jiraUrl = conf.jiraUrl.apply

    val (protocol, server, port, _) = HttpRequest.parseUrl(jiraUrl)

    val apacheClient = ClientFactory.createClient
    val client = new HttpClient(apacheClient)

    val password = pwd(conf)

    conf.username.get match {
      case Some(u) =>
        apacheClient.getCredentialsProvider().setCredentials(
          new AuthScope(server, port)
          , new UsernamePasswordCredentials(u, password.get)
        )
      case _ => {}
    }

    val until = conf.until.get match {
      case Some(s) => DateTime.parse(s)
      case _ => new DateTime
    }
    val since = conf.since.get match {
      case Some(s) => DateTime.parse(s)
      case _ => until.minusWeeks(2)
    }

    def fmt(d: DateTime) = d.toString(DateFormat)

    apacheClient.addRequestInterceptor(new PreemptiveAuthInterceptor(), 0)

    val urlBase = jiraUrl + "/rest/api/2"

    try {
      val resp = client.get(urlBase + "/search"
        , Map(
          "jql" -> ("labels=escalation and timeSpent > 0 and updatedDate >= " +
               fmt(since) + " and updatedDate <= " + fmt(until))
        )
      )
      val jsonStr = resp.content.get.toString

      case class Result(maxResults: Int, issues: List[Issue])
      case class Issue(key: String, self: String, fields: Field)
      case class Field(
        description: String
        , created: DateTime
        , updated: DateTime
        , labels: List[String]
      )

      import net.liftweb.json.{NoTypeHints, Serialization}
      import net.liftweb.json.Serialization.read
      implicit val formats = Serialization.formats(NoTypeHints)
      
      val result = read[Result](jsonStr)

      case class Author(name: String, displayName: String)
      case class WorkLog(author: Author, timeSpentSeconds: Long, created: DateTime)
      case class WorkLogResult(worklogs: List[WorkLog])
      case class IssueWithWorkLog(issue: Issue, workLog: Map[String, List[WorkLog]])

      val issues = result.issues.map { issue => 
        val issueUrl = issue.self
        val workLogRes = read[WorkLogResult](
          client.get(issueUrl + "/worklog").content.get.toString
        )

        val prunedLogs = 
          conf.workers.get match {
            case Some(ws) if !ws.isEmpty => 
              workLogRes.worklogs.filter { wl => ws.contains(wl.author.name) }
            case _ => workLogRes.worklogs
          }
 
        IssueWithWorkLog(issue, prunedLogs.groupBy(_.author.name))
      }

      val prunedIssues = issues.filter { i => !i.workLog.isEmpty }

      def hours(seconds: Long) = "%.2f" format (seconds / 3600.0)

      val tablizer = new Tablizer("  ");

      prunedIssues.foreach { case IssueWithWorkLog(issue, workLogs) =>
        val title = issue.key
        println(title)

        // Print user work log 
        val workHours = 
          workLogs
          .toList
          .sortBy { case (n, _) => n }
          .map { case(n, ls) => 
            (n, ls.foldLeft (0L) { (sum, l) => sum + l.timeSpentSeconds }) }
          .map { case (name, time) => List(name, hours(time)) }

        val rows = tablizer.tablize(workHours, List("Worker", "Hours"))
        rows.foreach { r => println("    " + r.mkString) }

        println()
      }

      println()

      // Now print totals by user
      val totals = 
        prunedIssues
        .flatMap { _.workLog }
        .groupBy { case (n, _) => n }
        .map { case (n, wl) => (n, wl.flatMap { case (_, ls) => ls }) }
        .toList
        .sortBy { case (n, _) => n }
        .map { case (n, ls) =>
            (n, ls.foldLeft (0L) { (sum, l) => sum + l.timeSpentSeconds }) }
        .map { case (name, total) => List(name, hours(total)) }

      val total = totals.map(_(1).toFloat).sum + "" // ugh
      val rows = tablizer.tablize(
        totals ++ 
          List(List("------", "-" * total.size)) ++ 
          List(List("TOTALS", total))
        , List("Worker", "Total Hours")
      )
      rows.foreach { r => println(r.mkString) }

      println()
      println("Logged from " + fmt(since) + " until " + fmt(until))
    } catch {
      case e: Exception => 
        Console.err.println(e.getMessage)
        Log.error("Exception", e)
    } finally {
      apacheClient.getConnectionManager.shutdown
    }
  }

  private def pwd(conf: Conf): Option[String] = {
    // TODO Research the security implications of storing a password in a string
    conf.password.get match {
      case Some(p) => Some(p)
      case _ =>
        if (conf.passwordPrompt.isSupplied) {
          Some(
            new String(System.console.readPassword("%s", "Password: "))
          )
        } else {
          None
        }
    }
  }

  private class Conf(arguments: Seq[String]) extends LazyScallopConf(arguments) {
    version("JIRA Scraper 1.0-SNAPSHOT")
    val jiraUrl = opt[String](
      "server"
      , short = 's'
      , required = true
      , descr = "JIRA URL."
    )
    val username = opt[String](
      "user"
      , descr = "JIRA user."
    )
    val password = opt[String](
      "password"
      , descr = "JIRA user password."
    )
    val since = opt[String](
      "since"
      , descr = "Start date for updated JIRAs before --until value. Format: yyyy-MM-dd"
    )
    val until = opt[String](
      "until"
      , noshort = true
      , descr = "End date for updated JIRAs.  Format: yyyy-MM-dd"
    )
    val workers = opt[List[String]](
      "workers"
      , descr = "The workers for which to gather data."
    )

    val passwordPrompt = toggle("pp", descrYes = "Prompt for password.")

    mutuallyExclusive(password, passwordPrompt)
  }

  private class PreemptiveAuthInterceptor extends HttpRequestInterceptor {
    def process(request: ApacheHttpRequest , context: HttpContext): Unit = {
      val authState = context.getAttribute(
        ClientContext.TARGET_AUTH_STATE).asInstanceOf[AuthState]

      // If no auth scheme avaialble yet, try to initialize it
      // preemptively
      if (authState.getAuthScheme() == null) {
        val credsProvider = context.getAttribute(
          ClientContext.CREDS_PROVIDER).asInstanceOf[CredentialsProvider]
        val targetHost = context.getAttribute(
          ExecutionContext.HTTP_TARGET_HOST).asInstanceOf[HttpHost]
        val creds = credsProvider.getCredentials(
          new AuthScope(targetHost.getHostName, targetHost.getPort))
        if (creds == null) 
          throw new HttpException("No credentials for preemptive authentication");
        authState.setAuthScheme(new BasicScheme);
        authState.setCredentials(creds);
      }
    }
  }
}
