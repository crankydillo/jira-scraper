package org.beeherd.jira

import scala.xml.XML

import org.apache.log4j.Logger
import org.beeherd.cli.utils.Tablizer
import org.beeherd.client.XmlResponse
import org.beeherd.client.http._
import org.joda.time.{
  DateTime, Interval
}
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

object Tester {
  def main(args: Array[String]): Unit = {
    val jiraUrl = "https://issues.apache.org/jira"
    val urlBase = jiraUrl + "/rest/api/2"

    val (protocol, server, port, _) = HttpRequest.parseUrl(jiraUrl)

    val apacheClient = ClientFactory.createClient
    val client = new HttpClient(apacheClient)

    try {
      val issues = new JiraSearcher(client, urlBase).issues(
        "timeSpent > 0 and updated < 2013-03-27"
      )

      println(issues(0))
      println(issues(0).fields.issuetype.name)

      /*
      val hasSub = issues.filter { i => !i.subtasks.isEmpty }
      println(hasSub(0).subtasks(0))

      val worklogs = new JiraWorklog(client).worklogs(issues(0))
      println(worklogs(0))
      */
    } finally {
      apacheClient.getConnectionManager.shutdown
    }
  }
}

class EscalationWorklog

/** 
 * A CLI that displays how much work was logged on escalations.  Escalations
 * are JIRAs that have the label, escalation.
 */
object EscalationWorklog {
  import JiraApp._
  import RestFormatters.{ fmt => restFmt }

  private val Log = Logger.getLogger(classOf[EscalationWorklog])

  class EscalationConf(args: Seq[String]) extends Conf(args) {
    val workers = opt[List[String]](
      "workers"
      , descr = "The workers for which to gather data."
    )
  }

  def main(args: Array[String]): Unit = {
    val conf = new EscalationConf(args)
    initialize(conf)

    useClient(conf, (client: HttpClient, jiraUrl: String) => {
      val urlBase = jiraUrl + "/rest/api/2"

      val until = conf.until.get match {
        case Some(s) => DateTime.parse(s)
        case _ => new DateTime
      }
      val since = conf.since.get match {
        case Some(s) => DateTime.parse(s)
        case _ => until.minusWeeks(2)
      }

      val issues = new JiraSearcher(client, urlBase).issues(
        "labels = escalation and timeSpent > 0 and updatedDate >= " +
          restFmt(since) + " and updatedDate <= " + restFmt(until)
      )

      val worklogRetriever = new JiraWorklog(client, urlBase)

      val issuesWithWorkLogs = issues.map { issue => 
        val worklogs = worklogRetriever.worklogs(issue)

        val prunedLogs = 
          conf.workers.get match {
            case Some(ws) if !ws.isEmpty => 
              worklogs.filter { wl => ws.contains(wl.author.name) }
            case _ => worklogs
          }
        IssueWithWorkLog(issue, prunedLogs.groupBy(_.author.name))
      }

      val prunedIssues = issuesWithWorkLogs.filter { i => !i.workLog.isEmpty }

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
    })
  }
}

class SprintWorklog
/** 
 * A CLI that displays how much work was logged on Greenhopper spring JIRAs.
 */
object SprintWorklog {
  import JiraApp._

  private val Log = Logger.getLogger(classOf[SprintWorklog])

  class SprintConf(args: Seq[String]) extends Conf(args) {
    val team = opt[String](
      "team"
      , required = true
    )
  }

  def main(args: Array[String]): Unit = {
    val conf = new SprintConf(args)
    initialize(conf)

    useClient(conf, (client: HttpClient, jiraUrl: String) => {
      val urlBase = jiraUrl + "/rest/greenhopper/1.0"

      val until = conf.until.get match {
        case Some(s) => DateTime.parse(s)
        case _ => new DateTime
      }
      val since = conf.since.get match {
        case Some(s) => DateTime.parse(s)
        case _ => until.minusWeeks(2)
      }

      val teamsResource = new GreenhopperTeams(client, urlBase)
      val teamName = conf.team.apply
      val team = teamsResource.team(teamName)

      team match {
        case Some(t) =>
        case _ =>
          println("The team, " + teamName + ", was not found.")
          System.exit(1)
      }

      val sprintsResource = new GreenhopperSprints(client, urlBase)
      val sprints = sprintsResource.sprints(team.get.id)

      val sprintReportResource = new GreenhopperSprintReport(client, urlBase)
      val jiraRestUrl = jiraUrl + "/rest/api/2"
      val worklogRetriever = new JiraWorklog(client, jiraRestUrl)
      val issueRetriever = new JiraIssue(client, jiraRestUrl)
      val tablizer = new Tablizer("  ")

      sprints.foreach { sprint =>
        val sprintReport = sprintReportResource.sprintReport(team.get.id, sprint.id)

        println(sprint.name)
        println("-" * sprint.name.size)

        tablizer.tablize(
          List(List("Start:", fmt(sprintReport.startDate.get))) ++
            List(List("End:", fmt(sprintReport.endDate.get)))
        ).foreach { r => println(r.mkString) }

        println()

        val allIssues = 
          sprintReport
          .issues
          .map { i => issueRetriever.issue(i.key) }
          .flatMap { i => 
            (i.key, i.issueType) :: i.subtasks.map { s => (s.id, i.issueType.toLowerCase) } 
          }

        val sprintInterval = 
          new Interval(sprintReport.startDate.get, sprintReport.endDate.get)

        val logsByUser =
          allIssues
          .flatMap { case (id, itype) => 
            worklogRetriever.worklogs(id).map { l => (id, l, itype) } 
          }
          .filter { case (id, wl, itype) => sprintInterval.contains(wl.created) }
          .groupBy { case (id, wls, itype) => wls.author.name }

        if (Log.isDebugEnabled) {
          logsByUser
          .toList
          .flatMap { _._2 }
          .sortWith { (a, b) => a._2.created.isBefore(b._2.created) }
          .foreach { println }
        }

        val totals = 
          logsByUser
          .toList
          .sortBy { case (n, _) => n }
          .map { case (n, ls) => (n, ls.groupBy { _._3 }) }  // group by issuetype
          .map { case (n, ls) => 
            val storySecs = ls.getOrElse("story", Nil).map { _._2.timeSpentSeconds }.sum
            val totalSecs = ls.flatMap( _._2).map { _._2.timeSpentSeconds }.sum
            val percentStories = "%.2f" format (storySecs * 100.0 / totalSecs)
            //List(n, hours(totalSecs), percentStories)
            (n, totalSecs, storySecs)
          }

          /*
          .map { case (n, ls) =>
                (n, ls.foldLeft (0L) { (sum, l) => sum + l._2.timeSpentSeconds }) 
          }.map { case (name, total) => List(name, hours(total)) }
          */

         // Could just iterate once...
        val total = totals.map { _._2 }.sum
        val totalStory = totals.map { _._3 }.sum
        val totalPercentStory = "%.2f" format (totalStory * 100.0 / total)

        val totalStr = hours(total)
        val totalStoryStr = hours(totalStory)
        val totalPercentStoryStr = totalPercentStory + ""

        val headers = List("Worker", "Total Hours", "Story Hours", "% Story Work")
      
        val dataRows = totals.map { case(n, totSecs, storySecs) => 
          List(n, hours(totSecs), hours(storySecs), 
            "%.2f" format (storySecs * 100.0 / totSecs))
        }

        val separatorRow = List(List("------", "-" * totalStr.size, 
          "-" * totalStoryStr.size, "-" * totalPercentStoryStr.size))

        val totalRow = List(List("TOTALS", totalStr, totalStoryStr, totalPercentStoryStr))

        val rows = tablizer.tablize(dataRows ++ separatorRow ++ totalRow, headers)

        rows.foreach { r => println(r.mkString) }
        println()
      }

      println("Logged from " + fmt(since) + " until " + fmt(until))
    })
  }
}

class JiraApp 

/**
 * Contains functions and classes used by the CLIs
 */
object JiraApp {
  val Log = Logger.getLogger(classOf[JiraApp])

  class Conf(arguments: Seq[String]) extends LazyScallopConf(arguments) {
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
    val passwordPrompt = toggle("pp", descrYes = "Prompt for password.")

    mutuallyExclusive(password, passwordPrompt)
  }

  def initialize(conf: Conf): Unit = {
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
  }

  def pwd(conf: Conf): Option[String] = {
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

  def fmt(date: DateTime) = date.toString("EEE, dd MMM yyyy HH:mm:ss z")

  def useClient(conf: Conf, fn: (HttpClient, String) => Unit): Unit = {
    val jiraUrl = {
      val tmp = conf.jiraUrl.apply
      if (tmp.endsWith("/")) tmp.dropRight(1)
      else tmp
    }
    val (protocol, server, port, _) = HttpRequest.parseUrl(jiraUrl)
    val password = pwd(conf)

    val apacheClient = conf.username.get match {
      case Some(u) => ClientFactory.createClient(server, u, password.get, port, true)
      case _ => ClientFactory.createClient
    }

    val client = new HttpClient(apacheClient)

    try {
      fn(client, jiraUrl)
    } catch {
      case e: Exception => 
        e.printStackTrace
        Log.error("Exception", e)
    } finally {
      apacheClient.getConnectionManager.shutdown
    }
  }

  def hours(seconds: Long) = "%.2f" format (seconds / 3600.0)
  def percent(d: Double) = "%.2f" format d

  def prettyJson(json: String): String = {
    import net.liftweb.json.Printer.pretty 
    import net.liftweb.json._
    pretty(render(parse(json)))
  }
}


object RestFormatters {
  private val DateFormat = "yyyy-MM-dd"

  def fmt(d: DateTime) = d.toString(DateFormat)
}
