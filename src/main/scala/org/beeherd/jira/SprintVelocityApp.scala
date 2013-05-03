package org.beeherd.jira

import java.io.File

import org.apache.log4j.Logger
import org.beeherd.cli.utils.Tablizer
import org.beeherd.client.http._
import org.joda.time.{
  DateTime, Interval
}

import org.beeherd.jira.model._
import org.beeherd.jira.rest._

class SprintVelocityApp 

/** 
 * A CLI that displays how much work was logged on Greenhopper spring JIRAs.
 */
object SprintVelocityApp {
  import JiraApp._

  private val Log = Logger.getLogger(classOf[SprintVelocityApp])

  class SprintConf(args: Seq[String]) extends Conf(args) {
    version("Sprint Velocity Report 1.0-SNAPSHOT")
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
    val out = opt[String](
      "out"
      , descr = "Write the results to the specified file."
    )
  }

  def main(args: Array[String]): Unit = {
    val conf = new SprintConf(args)
    initialize(conf)

    val since = conf.since.get match {
      case Some(s) => DateTime.parse(s)
      case _ => new DateTime(0)
    }

    val reports = sprintReports(conf, conf.team.apply, conf.workers.get, since)

    conf.out.get match {
      case Some(o) => write(reports, new File(o))
      case _ => output(reports)
    }
  }

  private def sprintReports(
    conf: SprintConf
    , teamName: String
    , workers: Option[List[String]] = None
    , since: DateTime = new DateTime(0)
  ): List[SprintReport] = {
    useClient(conf, (client: HttpClient, jiraUrl: String) => {
      val urlBase = jiraUrl + "/rest/greenhopper/1.0"

      val teamsResource = new GreenhopperTeams(client, urlBase)
      val team = teamsResource.team(teamName)
      val sprintsResource = new GreenhopperSprints(client, urlBase)
      val reporter = new GreenhopperSprintReport(client, urlBase)

      val teamId = team.get.id

      val sprints = sprintsResource.sprints(teamId).reverse
      sprints.toStream
      .map { sprint => reporter.sprintReport(teamId, sprint.id) }
      .takeWhile { _.startDate.get.isAfter(since) }
      .toList
    })
  }

  private def write(reports: List[SprintReport], out: File): Unit = {
  }

  private def output(reports: List[SprintReport]): Unit = {
    import grizzled.math.stats._

    val tablizer = new Tablizer("  ")

    val headers = List("Sprint Name", "Completed", "Committed")
  
    val dataRows = reports.map { r => 
      List(r.name, r.storyPointsCompleted + "", r.storyPoints + "")
    }

    val longestName = reports.map { _.name }.sort { _.size > _.size }.head
    val completed = reports.map { _.storyPointsCompleted }
    val totalCompleted = completed.sum.toString
    val totalCommitted = reports.map { _.storyPoints }.sum.toString

    val separatorRow = List(List("-" * longestName.size, "-" * totalCompleted.size, 
      "-" * totalCommitted.size))

    val totalRow = List(List("TOTALS", totalCompleted, totalCommitted))

    val rows = tablizer.tablize(dataRows ++ separatorRow ++ totalRow, headers)

    rows.foreach { r => println(r.mkString) }

    println()
    tablizer.tablize(
      List(
        List("Mean", "%.2f" format mean(completed:_*))
        , List("Standard Deviation", "%.2f" format popStdDev(completed:_*))
        , List("Median", median(completed:_*) + "")
      )
    ).foreach { r => println(r.mkString) }
  }
}
