package org.beeherd.jira

import org.apache.log4j.Logger
import org.beeherd.cli.utils.Tablizer
import org.beeherd.client.http._
import org.joda.time.DateTime

import org.beeherd.jira.model._
import org.beeherd.jira.rest._

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
    val workers = opt[List[String]](
      "workers"
      , descr = "The workers for which to gather data."
    )
    val since = opt[String](
      "since"
      , descr = "Retrieve sprints that start after this date. Format: yyyy-MM-dd"
    )
  }

  def main(args: Array[String]): Unit = {
    val conf = new SprintConf(args)
    initialize(conf)

    val since = conf.since.get match {
      case Some(s) => DateTime.parse(s)
      case _ => new DateTime(0)
    }

    createReport(conf, conf.team.apply, conf.workers.get, since)
  }

  private def createReport(
    conf: SprintConf
    , teamName: String
    , workers: Option[List[String]] = None
    , since: DateTime = new DateTime(0)
  ): Unit = {
    useClient(conf, (client: HttpClient, jiraUrl: String) => {
      val urlBase = jiraUrl + "/rest/greenhopper/1.0"

      val teamsResource = new GreenhopperTeams(client, urlBase)
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

        val reports = 
          reporter
          .reports(teamName, workers)
          .takeWhile { _.report.startDate.get.isAfter(since) }
          .map { report => 
            print(report, tablizer)
            report
          }

        type SS = SprintSummary
        def ratio(f: (SS) => Fraction) =
          reports.foldLeft(Fraction(0, 0)) { (acc, s) => acc + f(s) }

        // I don't understand why the mapping over the stream doesn't
        // *appear* to finish before we get here.  Anyway, I'll for it's hand.
        
        val overallStoryPointRatio = ratio((s: SS) => s.storyPointRatio)

        println()
        println("OVERALL TOTALS")
        println("--------------")
        println()

        tablizer.tablize(
          List(List("Story Points:", overallStoryPointRatio + "")) ++
          List(List("Stories:", ratio((s: SS) => s.storyRatio) + "")) ++
          List(List("Other Issues:", ratio((s: SS) => s.nonStoryRatio) + ""))
        ).foreach { r => println(r.mkString) }

        println()

        val userWork = 
          reports
          .flatMap { _.userWorklogs.toList }
          .groupBy { _._1 }
          .map { case (n, wls) => 
            (n, wls.foldLeft ((0L, 0L)) { case ((totSecs, totStorySecs), (_, wl)) => 
                (totSecs + wl.totalSeconds, totStorySecs + wl.totalStorySeconds)
              })
          }.map { case (n, (totSec, totStorySec)) => 
            (n, WorklogSummary(totSec, totStorySec))
          }.toList
          .sortBy { _._1 }

        print(userWork, tablizer)

        println()

      } catch {
        case e: Exception =>
          Log.error(e)
          println(e.getMessage)
      }
    })

  }

  private def print(
    summary: SprintSummary
    , tablizer: Tablizer = new Tablizer("  ")
  ): Unit = {
    println(summary.report.name)
    println("-" * summary.report.name.size)

    tablizer.tablize(
      List(List("Start:", fmt(summary.report.startDate.get))) ++
      List(List("End:", fmt(summary.report.endDate.get)))
    ).foreach { r => println(r.mkString) }

    println()

    tablizer.tablize(
      List(List("Story Points:", summary.storyPointRatio + "")) ++
      List(List("Stories:", summary.storyRatio + "")) ++
      List(List("Other Issues:", summary.nonStoryRatio + ""))
    ).foreach { r => println(r.mkString) }

    println()

    print(summary.userWorklogs.toList, tablizer)

    println()
  }

  def print(
    workerStats: List[(String, WorklogSummary)]
    , tablizer: Tablizer
  ): Unit = {
    val total = workerStats.map { _._2.totalSeconds }.sum
    val totalStory = workerStats.map { _._2.totalStorySeconds }.sum
    val totalStr = hours(total)
    val totalStoryStr = hours(totalStory)
    val totalPercentStoryStr = 
      if (total == 0) "N/A"
      else "%.2f" format (totalStory * 100.0 / total)

    val headers = List("Worker", "Total Hours", "Story Hours", "% Story Work")
  
    val dataRows = workerStats.map { case(n, WorklogSummary(totSecs, storySecs)) => 
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

case class Fraction(numer: Long, denom: Long) { 
  override def toString = numer + "/" + denom

  def +(f: Fraction) = new Fraction(numer + f.numer, denom + f.denom)

}

case class SprintSummary(
  report: SprintReport
  , userWorklogs: Map[String, WorklogSummary]
) {
  lazy val storyRatio =
    Fraction(report.completedStories.size, report.stories.size)

  lazy val nonStoryRatio =
    Fraction(report.completedNonStories.size, report.nonStories.size)

  lazy val storyPointRatio =
    Fraction(report.storyPointsCompleted.toLong, report.storyPoints.toLong)
}

case class WorklogSummary(
  totalSeconds: Long
  , totalStorySeconds: Long
)
