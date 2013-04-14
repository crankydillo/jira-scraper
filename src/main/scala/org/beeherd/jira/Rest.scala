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

  def issues(jql: String, includeSubtasks: Boolean = false): List[Issue] = {
    val jsn = json(client.get(searchResourceUrl, Map("jql" -> jql)))
    println(JiraApp.prettyJson(jsn))
    read[SearchResult](jsn).issues
  }
}

class JiraIssue(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) extends RestResource {

  private val baseUrl = jiraUrlBase + "/issue"

  def issue(id: String): Issue = {
    val jsn = json(client.get(baseUrl + "/" + id))
    read[Issue](jsn)
  }
}

/**
 * Access JIRA's ../worklog resource.
 */
class JiraWorklog(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) extends RestResource {
  import JsonResults.WorkLogResult

  def worklogs(issue: Issue): List[WorkLog] =
    worklogs_h(issue.url)

  def worklogs(idOrKey: String): List[WorkLog] =
    worklogs_h(jiraUrlBase + "/issue/" + idOrKey)

  private def worklogs_h(issueUrl: String): List[WorkLog] = {
    val jsn = json(client.get(issueUrl + "/worklog"))
    read[WorkLogResult](jsn).worklogs
  }
}
