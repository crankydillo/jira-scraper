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
 * A CLI that displays sprint velocity statistics.
 */
object SprintVelocityApp {
  import JiraApp._

  private val Log = Logger.getLogger(classOf[SprintVelocityApp])

  class SprintConf(args: Seq[String]) extends Conf(args) {
    version("Sprint Velocity Report 1.0-SNAPSHOT")
    val teams = opt[List[String]](
      "teams"
      , required = true
      , descr = "Team(s) for which to gather data."
    )
    val since = opt[String](
      "since"
      , descr = "Retrieve sprints that start after this date. Format: yyyy-MM-dd"
    )
    val last = opt[Int](
      "last"
      , descr = "Retrieve the last N sprints."
    )
    val out = opt[String](
      "out"
      , descr = "Write the results to the specified file."
    )
    mutuallyExclusive(since, last)
  }

  def main(args: Array[String]): Unit = {
    val conf = new SprintConf(args)
    initialize(conf)

    val since = conf.since.get match {
      case Some(s) => DateTime.parse(s)
      case _ => new DateTime(0)
    }

    val last = conf.last.get.getOrElse(Int.MaxValue)

    useClient(conf, (client: HttpClient, jiraUrl: String) => {
      val urlBase = jiraUrl + "/rest/greenhopper/1.0"
      conf.teams.get.get.toStream.foreach { team =>
        output(team, sprintReports(client, urlBase, team, last, since).toList)
      }
    })
  }

  private def sprintReports(
    client: HttpClient
    , urlBase: String
    , teamName: String
    , last: Int = Int.MaxValue
    , since: DateTime = new DateTime(0)
  ): Stream[SprintReport] = {
    val teamsResource = new GreenhopperTeams(client, urlBase)
    val team = teamsResource.team(teamName)
    val sprintsResource = new GreenhopperSprints(client, urlBase)
    val reporter = new GreenhopperSprintReport(client, urlBase)

    val teamId = team.get.id

    val completedSprints = 
      sprintsResource.sprints(teamId).reverse.filter { _.closed }

    completedSprints
    .toStream
    .map { sprint => 
      try {
        Some(reporter.sprintReport(teamId, sprint.id))
      } catch {
        case e: Exception => 
          Log.error("There was a problem retrieving the " +
            "sprint, " + sprint.id + ", for teamId.", e)
          None
      }
    }.flatten
    .take(last)
    .takeWhile { _.startDate.get.isAfter(since) }
  }

  private def write(reports: Map[String, List[SprintReport]], out: File): Unit = {
  }

  private def output(team: String, reports: List[SprintReport]): Unit = {
    import grizzled.math.stats

    val tablizer = new Tablizer("  ")

    println(team)
    println("-" * team.size)
    println()

    val headers = List("Sprint Name", "Completed", "Committed")
  
    val dataRows = reports.map { r => 
      List(r.name, r.storyPointsCompleted + "", r.storyPoints + "")
    }

    val longestName = reports.map { _.name }.sort { _.size > _.size }.head
    val totalCompleted = reports.map { _.storyPointsCompleted }.sum.toString
    val totalCommitted = reports.map { _.storyPoints }.sum.toString

    val separatorRow = List(List("-" * longestName.size, "-" * totalCompleted.size, 
      "-" * totalCommitted.size))

    val totalRow = List(List("TOTALS", totalCompleted, totalCommitted))

    val rows = tablizer.tablize(dataRows ++ separatorRow ++ totalRow, headers)

    rows.foreach { r => println(r.mkString) }

    val pointsForStats = 
      reports
      .filter { r => r.storyPoints > 0 || r.storyPointsCompleted > 0 }
      .map { _.storyPointsCompleted }

    println()

    val (mean, stdDev, median) = pointsForStats match {
      case Nil => ("N/A", "N/A", "N/A")
      case List(points) => (points + "", points + "", points + "")
      case l => 
        ("%.2f" format stats.mean(l:_*)
          , "%.2f" format stats.popStdDev(l:_*)
          , stats.median(l:_*) + "")
    }

    tablizer.tablize(
      List(
        List("Mean", mean)
        , List("Standard Deviation", stdDev)
        , List("Median", median)
      )
    ).foreach { r => println(r.mkString) }

    println()
  }
}
