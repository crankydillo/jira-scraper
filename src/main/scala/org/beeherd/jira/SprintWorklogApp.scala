package org.beeherd.jira

import org.apache.log4j.Logger
import org.beeherd.cli.utils.Tablizer
import org.beeherd.client.http._
import org.joda.time.{
  DateTime, Interval
}

import org.beeherd.jira.model._
import org.beeherd.jira.rest._

object SprintReporter {
  val Log = Logger.getLogger(classOf[SprintReporter])
}

class SprintReporter(
  urlBase: String
  , teamsResource: GreenhopperTeams
  , sprintsResource: GreenhopperSprints
  , sprintReportResource: GreenhopperSprintReport
  , worklogRetriever: JiraWorklog
  , issueRetriever: JiraIssue
  , tablizer: Tablizer = new Tablizer("  ")
) {
  import SprintReporter.Log
  import JiraApp.{
    fmt, hours
  }

  def report(teamName: String) {
    val team = teamsResource.team(teamName)

    team match {
      case Some(t) =>
      case _ =>
        throw new IllegalArgumentException("The team, " + teamName + ", was not found.")
    }

    val sprints = sprintsResource.sprints(team.get.id)

    sprints.foreach { sprint => 
      try {
        sprintReport(team.get.id, sprint)
      } catch {
        case e: Exception =>
          Log.error(e)
          println("There was a problem retrieving the sprint report for " +
            "sprint " + sprint.id);
      }
      println()
    }
  }

  def sprintReport(teamId: Long, sprint: Sprint): Unit = {
    val sprintReport = sprintReportResource.sprintReport(teamId, sprint.id)

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
        (n, totalSecs, storySecs)
      }

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
  }
}

class SprintWorklogApp 

/** 
 * A CLI that displays how much work was logged on Greenhopper spring JIRAs.
 */
object SprintWorklogApp {
  import JiraApp._

  private val Log = Logger.getLogger(classOf[SprintWorklogApp])

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
      val sprintsResource = new GreenhopperSprints(client, urlBase)
      val sprintReportResource = new GreenhopperSprintReport(client, urlBase)
      val jiraRestUrl = jiraUrl + "/rest/api/2"
      val worklogRetriever = new JiraWorklog(client, jiraRestUrl)
      val issueRetriever = new JiraIssue(client, jiraRestUrl)
      val tablizer = new Tablizer("  ")

      try {
        new SprintReporter(urlBase, teamsResource, sprintsResource, sprintReportResource,
          worklogRetriever, issueRetriever, tablizer).report(teamName)
      } catch {
        case e: Exception =>
          Log.error(e)
          println(e.getMessage)
      }
    })
  }
}
