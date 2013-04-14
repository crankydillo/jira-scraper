package org.beeherd.jira.model

import org.joda.time.DateTime

case class Team(id: Long, name: String)
case class Sprint(id: Long, name: String)

case class SprintReport(
  id: Long
  , name: String
  , startDate: Option[DateTime]
  , endDate: Option[DateTime]
  , completedDate: Option[DateTime]
  , issues: List[SprintReportIssue]
)

case class SprintReportIssue(key: String)

