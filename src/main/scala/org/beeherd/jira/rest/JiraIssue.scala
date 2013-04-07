package org.beeherd.jira.rest

import org.beeherd.client.http.HttpClient
import net.liftweb.json.{
  NoTypeHints, Serialization
}

class JiraIssue(
  client: HttpClient
  , jiraUrlBase: String // string?? base URL for jira REST api
) {
  import net.liftweb.json.Serialization.read

  private val baseUrl = jiraUrlBase + "/issue"

  def issue(id: String): Issue = {
    implicit val formats = Serialization.formats(NoTypeHints)
    read[Issue](
      client.get(baseUrl + "/" + id).content.get.toString
    )
  }
}
