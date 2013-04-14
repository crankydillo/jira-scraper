package org.beeherd.jira

import org.apache.log4j.Logger
import org.beeherd.cli.utils.Tablizer
import org.beeherd.client.http._
import org.joda.time.{
  DateTime, Interval
}

import org.beeherd.jira.model._
import org.beeherd.jira.rest._

case class SprintsReport(
  reports: List[SprintReport]
)

case class SprintReport(
  name: String
  , startDate: DateTime
  , endDate: DateTime
  , userWorklogs: Map[String, WorklogSummary]
)

case class WorklogSummary(
  totalSeconds: Long
  , totalStorySeconds: Long
)

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
) {
  import SprintReporter.Log
  import JiraApp.{
    fmt, hours
  }

  def reports(teamName: String): Stream[SprintReport] = {
    val team = teamsResource.team(teamName)

    team match {
      case Some(t) =>
      case _ =>
        throw new IllegalArgumentException("The team, " + teamName + ", was not found.")
    }

    val sprints = sprintsResource.sprints(team.get.id)

    sprints.toStream.map { sprint => 
      try {
        Some(sprintReport(team.get.id, sprint))
      } catch {
        case e: Exception =>
          Log.error(e)
          println("There was a problem retrieving the sprint report for " +
            "sprint " + sprint.id)
          None
      }
    }.flatten
  }

  def sprintReport(teamId: Long, sprint: Sprint): SprintReport = {
    val sprintReport = sprintReportResource.sprintReport(teamId, sprint.id)

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

    val worklogSummaries = 
      logsByUser
      .toList
      .map { case (n, ls) => (n, ls.groupBy { _._3 }) }  // group by issuetype
      .map { case (n, ls) => 
        val storySecs = ls.getOrElse("story", Nil).map { _._2.timeSpentSeconds }.sum
        val totalSecs = ls.flatMap( _._2).map { _._2.timeSpentSeconds }.sum
        val percentStories = "%.2f" format (storySecs * 100.0 / totalSecs)
        (n -> WorklogSummary(totalSecs, storySecs))
      }.toMap

    SprintReport(sprint.name, sprintReport.startDate.get, sprintReport.endDate.get,
      worklogSummaries)
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
        val reporter = new SprintReporter(urlBase, teamsResource, 
          sprintsResource, sprintReportResource, worklogRetriever, issueRetriever)

        reporter.reports(teamName).foreach { report =>
          println(report.name)
          println("-" * report.name.size)

          tablizer.tablize(
            List(List("Start:", fmt(report.startDate))) ++
            List(List("End:", fmt(report.endDate)))
          ).foreach { r => println(r.mkString) }

          println()

          val totals = 
            report.userWorklogs
            .toList
            .sortBy { _._1 }

          val total = totals.map { _._2.totalSeconds }.sum
          val totalStory = totals.map { _._2.totalStorySeconds }.sum
          val totalPercentStory = "%.2f" format (totalStory * 100.0 / total)
          val totalStr = hours(total)
          val totalStoryStr = hours(totalStory)
          val totalPercentStoryStr = totalPercentStory + ""

          val headers = List("Worker", "Total Hours", "Story Hours", "% Story Work")
        
          val dataRows = totals.map { case(n, WorklogSummary(totSecs, storySecs)) => 
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
      } catch {
        case e: Exception =>
          Log.error(e)
          println(e.getMessage)
      }
    })
  }
}
