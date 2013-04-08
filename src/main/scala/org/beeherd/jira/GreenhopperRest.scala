package org.beeherd.jira

import org.joda.time.DateTime
import org.beeherd.client.http.HttpClient
import net.liftweb.json.Serialization.read

object GreenhopperJsonResults {
  case class RapidViewList(views: List[Team])
  case class Sprints(sprints: List[Sprint])
}

/**
 * Access Greenhopper's ../rapidviews/list resource.
 */
class GreenhopperTeams(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) extends RestResource {
  import GreenhopperJsonResults.RapidViewList

  private val searchResourceUrl = jiraUrlBase + "/rapidviews/list"

  def teams: List[Team] =
    read[RapidViewList](
      client.get(searchResourceUrl).content.get.toString
    ).views

  def team(name: String): Option[Team] = teams.find { _.name == name }
}

class GreenhopperSprints(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) extends RestResource {
  import GreenhopperJsonResults.Sprints

  private val searchResourceUrl = jiraUrlBase + "/sprints"

  def sprints(teamId: Long): List[Sprint] =
    read[Sprints](
      client.get(searchResourceUrl + "/" + teamId).content.get.toString
    ).sprints
}

