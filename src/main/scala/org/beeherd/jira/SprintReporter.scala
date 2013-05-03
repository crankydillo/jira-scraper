package org.beeherd.jira

import org.apache.log4j.Logger
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
) {
  import SprintReporter.Log
  import JiraApp.{
    fmt, hours
  }

  def reports(
    teamName: String
    , workers: Option[List[String]] = None
  ): Stream[SprintSummary] = {
    val team = teamsResource.team(teamName)

    team match {
      case Some(t) =>
      case _ =>
        throw new IllegalArgumentException("The team, " + teamName + ", was not found.")
    }

    // ASSume these come in chronological order; however, we want the
    // latest first..
    val sprints = sprintsResource.sprints(team.get.id).reverse

    sprints.toStream.map { sprint => 
      try {
        Some(sprintSummary(team.get.id, sprint, workers))
      } catch {
        case e: Exception =>
          Log.error(e)
          println("There was a problem retrieving the sprint report for " +
            "sprint " + sprint.id)
          None
      }
    }.flatten
  }

  def sprintSummary(
    teamId: Long
    , sprint: Sprint
    , workers: Option[List[String]] = None
  ): SprintSummary = {
    val sprintReport = sprintReportResource.sprintReport(teamId, sprint.id)

    val allIssues = 
      sprintReport
      .issues
      .map { i => issueRetriever.issue(i.key) }
      .flatMap { i => 
        (i.key, i.issueType.toLowerCase) :: 
        i.subtasks.map { s => (s.id, i.issueType.toLowerCase) } 
      }

    val sprintInterval = 
      new Interval(sprintReport.startDate.get, sprintReport.endDate.get)

    val logsByUser =
      allIssues
      .flatMap { case (id, itype) => 
        worklogRetriever.worklogs(id).map { l => (id, l, itype) } 
      }.filter { case (id, wl, itype) => 
        sprintInterval.contains(wl.started) && 
          (!workers.isDefined || workers.get.contains(wl.author.name))
      }.groupBy { case (id, wls, itype) => wls.author.name }

    if (Log.isDebugEnabled) {
      logsByUser
      .toList
      .flatMap { _._2 }
      .sortWith { (a, b) => a._2.started.isBefore(b._2.started) }
      .foreach {l => Log.debug(l) }
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

    SprintSummary(sprintReport, worklogSummaries)
  }
}
