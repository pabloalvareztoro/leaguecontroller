package models

import models.TeamStatistics.EnumVal

case class Team (name: String, statistics: scala.collection.mutable.Map[EnumVal, Int]) {

}

object TeamStatistics {
  sealed trait EnumVal
  case object Points extends EnumVal
  case object Wins extends EnumVal
  case object Losses extends EnumVal
  case object Draws extends EnumVal
  case object GoalsScored extends EnumVal
  case object GoalsConceded extends EnumVal
}
