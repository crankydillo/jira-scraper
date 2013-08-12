package org.beeherd.jira.rest

import java.text.SimpleDateFormat

import org.apache.log4j.Logger
import org.joda.time.DateTime
import org.beeherd.client.http.{
  ClientFactory, HttpClient, HttpRequest
}
import org.beeherd.client.{
  BadResponse, Response, StringResponse
}
import net.liftweb.json.DefaultFormats
import net.liftweb.json.Serialization.read
import net.liftweb.json.ext.JodaTimeSerializers

import org.beeherd.jira.model._
import org.beeherd.jira.JiraApp

object JsonResults {
  case class SearchResult(maxResults: Int, issues: List[Issue])
  case class WorkLogResult(worklogs: List[WorkLog])
}

object RestResource {
  val Log = Logger.getLogger(classOf[RestResource])

  def useClient[T](
    jiraUrl: String
    , fn: (HttpClient, String) => T
    , creds: Option[(String, String)] = None
  ): T = {
    val (protocol, server, port, _) = HttpRequest.parseUrl(jiraUrl)

    val apacheClient = creds match {
      case Some((user, pass)) => 
        ClientFactory.createClient(server, user, pass, port, true)
      case _ => ClientFactory.createClient
    }

    val client = new HttpClient(apacheClient)

    try {
      fn(client, jiraUrl)
    } finally {
      apacheClient.getConnectionManager.shutdown
    }
  }

}

trait RestResource {
  import RestResource.Log
  import JiraApp.prettyJson

  implicit val formats = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat(
      "yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  } ++ JodaTimeSerializers.all

  /**
   * Extract JSON from the response or throw a runtime exception.
   */
  def json(resp: Response): String = {
    resp match {
      case StringResponse(str) => 
        if (Log.isTraceEnabled)
          Log.trace(prettyJson(str))
        str
      case BadResponse(msg) => throw new RuntimeException(msg + 
        " If authentication is required, please check your credentials.")
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
      json(client.get(searchResourceUrl, Map("jql" -> jql)))
    ).issues
}

class JiraIssue(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) extends RestResource {

  private val baseUrl = jiraUrlBase + "/issue"

  def issue(id: String): Issue = 
    read[Issue](json(client.get(baseUrl + "/" + id)))
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

  private def worklogs_h(issueUrl: String): List[WorkLog] = 
    read[WorkLogResult](json(client.get(issueUrl + "/worklog"))).worklogs
}
