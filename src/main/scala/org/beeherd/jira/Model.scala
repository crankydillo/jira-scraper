package org.beeherd.jira.rest

import org.joda.time.DateTime

case class Issue(key: String, self: String, fields: Field) {
  def url = self
}

case class Field(
  description: String
  , created: DateTime
  , updated: DateTime
  , labels: List[String]
  )

case class Author(name: String, displayName: String)
case class WorkLog(author: Author, timeSpentSeconds: Long, created: DateTime)
case class IssueWithWorkLog(issue: Issue, workLog: Map[String, List[WorkLog]])
