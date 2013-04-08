package org.beeherd.jira.rest

import java.text.SimpleDateFormat

import org.joda.time.DateTime
import org.beeherd.client.http.HttpClient
import net.liftweb.json.{
  DefaultFormats, NoTypeHints, Serialization, ShortTypeHints
}
import net.liftweb.json.ext.JodaTimeSerializers

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

  //implicit val formats = Serialization.formats(NoTypeHints)
  implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat(
      "yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  } ++ JodaTimeSerializers.all

  private val searchResourceUrl = jiraUrlBase + "/search"

  def issues(jql: String, includeSubtasks: Boolean = false): List[Issue] =
    read[SearchResult](
      client.get(searchResourceUrl, Map("jql" -> jql)).content.get.toString
    ).issues
}
