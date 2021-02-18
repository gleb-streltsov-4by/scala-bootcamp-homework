package com.bootcamp

import sbt._
import Keys._

object BulkySourcesPlugin extends AutoPlugin {
  override def trigger = allRequirements

  private val defaultBulkyThreshold = 100

  lazy val bulkyThresholdInLines = settingKey[Int]("Min threshold in lines")
  lazy val bulkySources = taskKey[Seq[(Int, File)]](
    "Sequence of source code files with number of lines greater then `bulkyThresholdInLines`")

  override val projectSettings: Seq[Setting[_]] = Seq(
    bulkyThresholdInLines := defaultBulkyThreshold,

    bulkySources := bulkyLines((Compile / sources).value, bulkyThresholdInLines.value),
    (Test / bulkySources) := bulkyLines((Test / sources).value, bulkyThresholdInLines.value)
  )

  def bulkyLines(files: Seq[File], threshold: Int): Seq[(Int, File)] = {
    val result = files
      .map(f => (sbt.IO.readLines(f).length, f))
      .filter { case (len, _) => len >= threshold }

    result.sorted.reverse
  }
}