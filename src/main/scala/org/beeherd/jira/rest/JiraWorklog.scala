package org.beeherd.jira.rest

import org.beeherd.client.http.HttpClient
import net.liftweb.json.{
  DefaultFormats, Serialization
}

object JiraWorklog {
  case class WorkLogResult(worklogs: List[WorkLog])
}

/**
 * Access JIRA's ../worklog resource.
 */
class JiraWorklog(
  client: HttpClient
) {
  import net.liftweb.json.Serialization.read
  import JiraWorklog.WorkLogResult

  implicit val formats = DefaultFormats

  def worklogs(issue: Issue): List[WorkLog] = {

    read[WorkLogResult](
      client.get(issue.url + "/worklog").content.get.toString
    ).worklogs
  }

}
