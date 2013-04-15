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
  , completedIssues: List[SprintReportIssue]
  , incompletedIssues: List[SprintReportIssue]
  , puntedIssues: List[SprintReportIssue]
) {
  def issues = completedIssues ++ incompletedIssues ++ puntedIssues

  def completedStories = 
    completedIssues.filter { _.typeName.toLowerCase == "story" }

  def incompletedStories = 
    incompletedIssues.filter { _.typeName.toLowerCase == "story" }
}

case class SprintReportIssue(key: String, typeName: String)

