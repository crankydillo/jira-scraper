package org.beeherd.jira.rest

import org.apache.log4j.Logger
import org.joda.time.DateTime
import org.beeherd.client.http.HttpClient
import net.liftweb.json.Serialization.read

import org.beeherd.jira.model._

object GreenhopperJsonResults {
  case class RapidViewList(views: List[Team])
  case class Sprints(sprints: List[Sprint])

  // I'm not sure how to dig down without having all the classes..
  case class SprintReportResp(contents: SRContent, sprint: SRSprint)
  case class SRContent(
    completedIssues: List[SprintReportIssue]
    , incompletedIssues: List[SprintReportIssue]
    , puntedIssues: List[SprintReportIssue]
  )
  case class SRSprint(
    id: Long
    , name: String
    , startDate: Option[DateTime]
    , endDate: Option[DateTime]
    , completedDate: Option[DateTime]
  )
}

/**
 * Access Greenhopper's ../rapidviews/list resource.
 */
class GreenhopperTeams(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) extends RestResource {
  import GreenhopperJsonResults.RapidViewList

  private val baseUrl = jiraUrlBase + "/rapidviews/list"

  def teams: List[Team] = 
    read[RapidViewList](json(client.get(baseUrl))).views

  def team(name: String): Option[Team] = teams.find { _.name == name }
}

class GreenhopperSprints(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) extends RestResource {
  import GreenhopperJsonResults.Sprints

  private val baseUrl = jiraUrlBase + "/sprints"

  def sprints(teamId: Long): List[Sprint] =
    read[Sprints](json(client.get(baseUrl + "/" + teamId))).sprints
}

object GreenhopperSprintReport {
  val Log = Logger.getLogger(classOf[GreenhopperSprintReport])
}

case class GreenhopperSprintReport(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) extends RestResource {
  import GreenhopperSprintReport.Log
  import java.text.SimpleDateFormat
  import net.liftweb.json.DefaultFormats
  import net.liftweb.json.ext.JodaTimeSerializers
  import GreenhopperJsonResults.SprintReportResp

  implicit override val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat("dd/MMM/yy HH:mm a")
  } ++ JodaTimeSerializers.all

  private val baseUrl = jiraUrlBase + "/rapid/charts/sprintreport"

  def sprintReport(teamId: Long, sprintId: Long): SprintReport = {
    val params = Map("rapidViewId" -> (teamId + ""), "sprintId" -> (sprintId + ""))
    val jsonStr = json(client.get(baseUrl, params))
    Log.debug(org.beeherd.jira.JiraApp.prettyJson(jsonStr))
    val srResp = read[SprintReportResp](jsonStr)
    val c = srResp.contents
    SprintReport(
      srResp.sprint.id
      , srResp.sprint.name
      , srResp.sprint.startDate
      , srResp.sprint.endDate
      , srResp.sprint.completedDate
      , c.completedIssues
      , c.incompletedIssues 
      , c.puntedIssues
    )
  }
}
