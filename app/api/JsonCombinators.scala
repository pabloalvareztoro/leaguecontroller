package api

import models.TeamStatistics._
import models._
import play.api.libs.functional.syntax._
import play.api.libs.json._

object JsonCombinators {

  implicit val teamWrites = new OWrites[Team] {
    def writes(t: Team) = Json.obj(
      "name" -> t.name,
      "points" -> t.statistics(Points),
      "wins" -> t.statistics(Wins),
      "draws" -> t.statistics(Draws),
      "losses" -> t.statistics(Losses),
      "goalsScored" -> t.statistics(GoalsScored),
      "goalsConceded" -> t.statistics(GoalsConceded)
    )
  }

  implicit val teamReads: Reads[Team] = (JsPath \ "name").read[String].map(name => Team(name, scala.collection.mutable.Map[EnumVal, Int]()))

  implicit val leagueWrites = new OWrites[League] {
    def writes(m: League) = Json.obj(
      "teams" -> m.teams
    )
  }

  implicit val leagueReads: Reads[League] = (JsPath \ "league").read[Seq[Team]].map(teams => League(teams))

  implicit val contenderWrites = new OWrites[Contender] {
    def writes(m: Contender) = Json.obj(
      "team" -> m.name,
      "score" -> m.score
    )
  }

  implicit val contenderReads: Reads[Contender] = (
    (JsPath \ "name").read[String] and
    (JsPath \ "score").read[Int]
    )(Contender.apply _)

  implicit val matchWrites = new OWrites[Match] {
    def writes(m: Match) = Json.obj(
      "teams" -> m.teams
    )
  }

  implicit val matchReads: Reads[Match] = (JsPath \ "teams").read[Seq[Contender]].map(contenders => Match(contenders))

  implicit val weekWrites = new OWrites[Week] {
    def writes(m: Week) = Json.obj(
      "matches" -> m.matches
    )
  }

  implicit val weekReads: Reads[Week] = (JsPath).read[Seq[Match]].map(week => Week(week))

  implicit val fixtureWrites = new OWrites[Fixture] {
    def writes(m: Fixture) = Json.obj(
      "fixture" -> m.fixture
    )
  }

  implicit val fixtureReads: Reads[Fixture] = (JsPath).read[Seq[Week]].map(weeks => Fixture(weeks))

}
