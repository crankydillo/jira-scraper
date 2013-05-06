package org.beeherd.jira.model

import org.apache.log4j.Logger
import org.joda.time.DateTime

// TODO Figure out how to 'skip' things with lift-json

case class Team(id: Long, name: String)
case class Sprint(id: Long, name: String, closed: Boolean)

object SprintReport {
  val Log = Logger.getLogger(classOf[SprintReport])
}

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
  import SprintReport.Log

  def issues = completedIssues ++ incompletedIssues ++ puntedIssues
  def completedStories = completedIssues.filter { isStory }
  def incompletedStories = incompletedIssues.filter { isStory }
  def stories = issues.filter { isStory  }
  def nonStories = issues.filter { i => !isStory(i) }
  def completedNonStories = completedIssues.filter { i => !isStory(i) }
  def incompletedNonStories = incompletedIssues.filter { i => !isStory(i) }

  def storyPointsCompleted = {
    completedStories
    .map { _.estimateStatistic.statFieldValue }
    .flatten
    .map { _.value }
    .sum
  }

  def storyPoints = {
    if (Log.isDebugEnabled) {
      stories
      .filter { _.estimateStatistic.statFieldValue.isEmpty }
      .foreach {s => Log.info(s.key + " does not have story points.") }
    }

    stories 
    .map { _.estimateStatistic.statFieldValue }
    .flatten
    .map { _.value }
    .sum
  }

  private def isStory(issue: SprintReportIssue) = 
    issue.typeName.toLowerCase == "story" 
}

case class SprintReportIssue(
  key: String
  , typeName: String
  , estimateStatistic: EstimateStatistic
)

case class EstimateStatistic(statFieldId: String, statFieldValue: Option[StatValue])
case class StatValue(value: Float)

