package org.beeherd.jira

import org.apache.log4j.Logger
import org.beeherd.cli.utils.Tablizer
import org.beeherd.client.http._
import org.joda.time.DateTime

import org.beeherd.jira.rest._
import org.beeherd.jira.model._

class EscalationWorklogApp

/** 
 * A CLI that displays how much work was logged on escalations.  Escalations
 * are JIRAs that have the label, escalation.
 */
object EscalationWorklogApp {
  import JiraApp._
  import RestFormatters.{ fmt => restFmt }

  private val Log = Logger.getLogger(classOf[EscalationWorklogApp])

  class EscalationConf(args: Seq[String]) extends Conf(args) {
    val workers = opt[List[String]](
      "workers"
      , descr = "The workers for which to gather data."
    )
    val since = opt[String](
      "since"
      , descr = "Start date for updated JIRAs before --until value. Format: yyyy-MM-dd"
    )
    val until = opt[String](
      "until"
      , noshort = true
      , descr = "End date for updated JIRAs.  Format: yyyy-MM-dd"
    )
  }

  def main(args: Array[String]): Unit = {
    val conf = new EscalationConf(args)
    initialize(conf)

    useClient(conf, (client: HttpClient, jiraUrl: String) => {
      val urlBase = jiraUrl + "/rest/api/2"

      val until = conf.until.get match {
        case Some(s) => DateTime.parse(s)
        case _ => new DateTime().plusDays(1)
      }
      val since = conf.since.get match {
        case Some(s) => DateTime.parse(s)
        case _ => until.minusWeeks(2)
      }

      val issues = new JiraSearcher(client, urlBase).issues(
        "labels = escalation and timeSpent > 0 and updatedDate >= " +
          restFmt(since) + " and updatedDate <= " + restFmt(until)
      )

      val worklogRetriever = new JiraWorklog(client, urlBase)

      val issuesWithWorkLogs = issues.map { issue => 
        val worklogs = worklogRetriever.worklogs(issue)

        val prunedLogs = 
          conf.workers.get match {
            case Some(ws) if !ws.isEmpty => 
              worklogs.filter { wl => ws.contains(wl.author.name) }
            case _ => worklogs
          }
        IssueWithWorkLog(issue, prunedLogs.groupBy(_.author.name))
      }

      val prunedIssues = issuesWithWorkLogs.filter { i => !i.workLog.isEmpty }

      val tablizer = new Tablizer("  ");
      val indent = "    "

      prunedIssues.foreach { case IssueWithWorkLog(issue, workLogs) =>
        val title = issue.key
        println(title)
        println("-" * title.size)
        println(indent + "Summary: " + issue.fields.summary)
        println()

        // Print user work log 
        val workHours = 
          workLogs
          .toList
          .sortBy { case (n, _) => n }
          .map { case(n, ls) => 
            (n, ls.foldLeft (0L) { (sum, l) => sum + l.timeSpentSeconds }) }
          .map { case (name, time) => List(name, hours(time)) }

        val rows = tablizer.tablize(workHours, List("Worker", "Hours"))
        rows.foreach { r => println(indent + r.mkString) }

        println()
      }

      println()

      // Now print totals by user
      val totals = 
        prunedIssues
        .flatMap { _.workLog }
        .groupBy { case (n, _) => n }
        .map { case (n, wl) => (n, wl.flatMap { case (_, ls) => ls }) }
        .toList
        .sortBy { case (n, _) => n }
        .map { case (n, ls) =>
            (n, ls.foldLeft (0L) { (sum, l) => sum + l.timeSpentSeconds }) }
        .map { case (name, total) => List(name, hours(total)) }

      val total = totals.map(_(1).toFloat).sum + "" // ugh
      val rows = tablizer.tablize(
        totals ++ 
          List(List("------", "-" * total.size)) ++ 
          List(List("TOTALS", total))
        , List("Worker", "Total Hours")
      )
      rows.foreach { r => println(r.mkString) }

      println()
      println("Logged from " + fmt(since) + " until " + fmt(until))
    })
  }
}

