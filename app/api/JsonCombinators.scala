package api

import models.db.DBLeague
import models.{League, Team}
import play.api.libs.json._

object JsonCombinators {

  implicit val teamWrites = new OWrites[Team] {
    def writes(t: Team) = Json.obj(
      "name" -> t.name
    )
  }

  implicit val teamReads: Reads[Team] = (JsPath \ "name").read[String].map(name => Team(name))

  implicit val leagueWrites = new OWrites[League] {
    def writes(m: League) = Json.obj(
      "teams" -> m.teams
    )
  }

  implicit val leagueReads: Reads[League] = (JsPath \ "teams").read[Seq[Team]].map(teams => League(teams))

  implicit val dbLeagueReads: Reads[DBLeague] = (JsPath \ "teams").read[String].map(teams => DBLeague(teams))
}
