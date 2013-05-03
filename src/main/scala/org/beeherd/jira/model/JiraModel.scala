package org.beeherd.jira.model

import org.joda.time.DateTime

case class Issue(
  key: String
  , self: String
  , fields: Field
) {
  def url = self
  def subtasks = fields.subtasks
  def issueType = fields.issuetype.name
}

case class Field(
  summary: String
  , description: String
  , created: DateTime
  , updated: DateTime
  , labels: List[String]
  , timetracking: Option[TimeTracking]
  , subtasks: List[Subtask] 
  , issuetype: IssueType
  )

case class Subtask(id: String, self: String ) {
  def url = self
}

case class IssueType(name: String)
case class Author(name: String, displayName: String)
case class WorkLog(
  author: Author
  , timeSpentSeconds: Long
  , created: DateTime
  , started: DateTime
)
case class IssueWithWorkLog(issue: Issue, workLog: Map[String, List[WorkLog]])
case class TimeTracking(timeSpentSeconds: Long, remainingEstimateSeconds: Long)
