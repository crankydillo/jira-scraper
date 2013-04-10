package org.beeherd.jira

import java.text.SimpleDateFormat

import org.joda.time.DateTime
import org.beeherd.client.http.HttpClient
import org.beeherd.client.{
  Response, StringResponse
}
import net.liftweb.json.DefaultFormats
import net.liftweb.json.Serialization.read
import net.liftweb.json.ext.JodaTimeSerializers

object JsonResults {
  case class SearchResult(maxResults: Int, issues: List[Issue])
  case class WorkLogResult(worklogs: List[WorkLog])
}

trait RestResource {
  implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat(
      "yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  } ++ JodaTimeSerializers.all

  /**
   * Extract JSON from the response or throw a runtime exception.
   */
  def json(resp: Response): String = {
    resp match {
      case StringResponse(s) => s
      case _ => throw new RuntimeException("Unexpected HTTP response: " + resp)
    }
  }
}

/**
 * Access Jira's ../search resource.
 */
class JiraSearcher(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) extends RestResource {
  import JsonResults.SearchResult

  private val searchResourceUrl = jiraUrlBase + "/search"

  def issues(jql: String, includeSubtasks: Boolean = false): List[Issue] =
    read[SearchResult](
      client.get(searchResourceUrl, Map("jql" -> jql)).content.get.toString
    ).issues
}

class JiraIssue(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) extends RestResource {

  private val baseUrl = jiraUrlBase + "/issue"

  def issue(id: String): Issue = 
    read[Issue](
      client.get(baseUrl + "/" + id).content.get.toString
    )
}

/**
 * Access JIRA's ../worklog resource.
 */
class JiraWorklog(
  client: HttpClient
) extends RestResource {
  import JsonResults.WorkLogResult

  def worklogs(issue: Issue): List[WorkLog] =
    read[WorkLogResult](
      client.get(issue.url + "/worklog").content.get.toString
    ).worklogs
}
