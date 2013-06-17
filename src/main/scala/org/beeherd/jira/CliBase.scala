package org.beeherd.jira

import scala.xml.XML

import org.apache.log4j.Logger
import org.beeherd.client.http._
import org.joda.time.DateTime
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

object Tester {
  import org.beeherd.jira.rest._

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

class JiraApp 

/**
 * Contains functions and classes used by the CLIs
 */
object JiraApp {
  val Log = Logger.getLogger(classOf[JiraApp])

  class Conf(arguments: Seq[String]) 
  extends LazyScallopConf(arguments, true) {
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
    val passwordPrompt = toggle("pp", descrYes = "Prompt for password.")

    mutuallyExclusive(password, passwordPrompt)

    validateOpt(username, password, passwordPrompt) {
      case (Some(u), None, None) => 
        Left("You must use either -p or --pp if -u/--user is used")
      case _ => Right(Unit)
    }
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
          val passwd = System.console.readPassword("%s", "Password: ")
          Some(new String(passwd))
        } else {
          None
        }
    }
  }

  def fmt(date: DateTime) = date.toString("EEE, dd MMM yyyy HH:mm:ss z")

  def useClient[T](conf: Conf, fn: (HttpClient, String) => T): T = {
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
