package org.beeherd.jira.rest

import org.joda.time.DateTime
import org.beeherd.client.http.HttpClient
import net.liftweb.json.{
  DefaultFormats, NoTypeHints, Serialization, ShortTypeHints
}

object JiraSearcher {
  case class SearchResult(maxResults: Int, issues: List[Issue])
}

/**
 * Access Jira's ../search resource.
 */
class JiraSearcher(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) {
  import net.liftweb.json.Serialization.read
  import JiraSearcher.SearchResult

  implicit val formats = DefaultFormats

  private val searchResourceUrl = jiraUrlBase + "/search"
  private val dateFormat = "yyyy-MM-dd"

  def issues(jql: String, includeSubtasks: Boolean = false): List[Issue] =
    read[SearchResult](
      client.get(searchResourceUrl, Map("jql" -> jql)).content.get.toString
    ).issues
}
