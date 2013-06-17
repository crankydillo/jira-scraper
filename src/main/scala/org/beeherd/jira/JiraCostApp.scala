package org.beeherd.jira

import org.apache.log4j.Logger
import org.beeherd.cli.utils.Tablizer
import org.beeherd.client.http._
import org.joda.time.DateTime

import org.beeherd.jira.rest._
import org.beeherd.jira.model._

class JiraCostApp

/** 
 * A CLI that tries to capture cost (i.e. work) based on
 * classifiers (e.g. labels).
 */
object JiraCostApp {
  import JiraApp._
  import RestFormatters.{ fmt => restFmt }

  sealed trait Filter
  case class LabelFilter(labels: List[String]) {
    override def toString = labels.mkString(" ")
  }
  case class JqlFilter(jql: String) {
    override def toString = jql
  }

  private val Log = Logger.getLogger(classOf[JiraCostApp])

  class JiraCostConf(args: Seq[String]) extends Conf(args) {
    val since = opt[String](
      "since"
      , descr = "Start date for updated JIRAs before --until value. Format: yyyy-MM-dd"
    )
    val until = opt[String](
      "until"
      , noshort = true
      , descr = "End date for updated JIRAs.  Format: yyyy-MM-dd"
    )
    val labels = opt[List[String]](
      "labels"
      , descr = "Labels to use for classifiers"
    )
    val jqls = opt[List[String]](
      "jqls"
      , descr = "JQLs to use for classifiers"
    )
    val showWorkerTotals = opt[Boolean](
      "swt"
      , descr = "Show worker totals."
    )
  }

  def main(args: Array[String]): Unit = {
    val conf = new JiraCostConf(args)
    initialize(conf)

    val until = conf.until.get match {
      case Some(s) => DateTime.parse(s)
      case _ => new DateTime().plusDays(1)
    }

    val since = conf.since.get match {
      case Some(s) => DateTime.parse(s)
      case _ => new DateTime(0)
    }


    useClient(conf, (client: HttpClient, jiraUrl: String) => {
      val urlBase = jiraUrl + "/rest/api/2"

      println("Period: " + fmt(since) + " - " + fmt(until))
      println()

      val searcher = new JiraSearcher(client, urlBase)
      val worklogRetriever = new JiraWorklog(client, urlBase)
      val tablizer = new Tablizer("  ");
      val indent = "    "

      val labelFilters = 
        conf.labels.get match {
          case Some(ls) => 
            ls.map { label =>
              LabelFilter(label.split(" ").map { _.trim }.toList)
            }
          case _ => Nil
        }

      val jqls =
        conf.jqls.get match {
          case Some(jqls) => jqls.map { jql => JqlFilter(jql) }
          case _ => Nil
        }

      val totals = (labelFilters ++ jqls).map { filter =>

        val filterTxt = filter match {
          case LabelFilter(ls) => ls.map { l => "labels = " + l }.mkString(" and ")
          case JqlFilter(jql) => jql
        }

        val issues = searcher.issues(
          filterTxt + 
          " and timeSpent > 0 and updatedDate >= " +
          restFmt(since) + " and updatedDate <= " + restFmt(until)
        )

        val issuesWithWorkLogs = issues.map { issue => 
          val worklogs = worklogRetriever.worklogs(issue)
          IssueWithWorkLog(issue, worklogs.groupBy(_.author.name))
        }

        val prunedIssues = issuesWithWorkLogs.filter { i => !i.workLog.isEmpty }

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

        val total = totals.map(_(1).toFloat).sum

        if (conf.showWorkerTotals.apply) {
          val title = filter.toString
          println(title)
          println("-" * title.size)
          println()
          val rows = tablizer.tablize(totals, List("Worker", "Hours"))
          rows.foreach { r => println((" " * 4) + r.mkString) }

          println()
        }

        (filter, total)
      }

      val overallTotal = "%.2f" format (totals.map { _._2 }.sum)

      tablizer.tablize(
        totals.map { case (l, t) => List(l.toString, "%.2f" format t) } ++
        List(List("-----", "-" * overallTotal.size)) ++
        List(List("TOTAL", overallTotal))
        , List("Label/JQL", "Hours")
      ).foreach { r => println(r.mkString) }
    })
  }
}
