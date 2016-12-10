package controllers

import javax.inject.Inject

import models.TeamStatistics.{GoalsConceded, GoalsScored, _}
import models._
import api.JsonCombinators._
import play.api.libs.json.{JsArray, JsValue, Json}
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.mvc.{Action, Controller}
import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.{MongoController, ReactiveMongoApi, ReactiveMongoComponents}

import scala.collection.mutable.ListBuffer

class TableController @Inject() (val ws: WSClient, val reactiveMongoApi: ReactiveMongoApi) extends Controller with MongoController with ReactiveMongoComponents {

  def positionCollection = reactiveMongoApi.db.collection[JSONCollection]("positions");

  def resultTable = Action(parse.json) { implicit request =>
    val jsonRequest: JsValue = request.body
    val fixtureId: String = (jsonRequest \ "fixtureId").validate[String].get
    val leagueId: String = (jsonRequest \ "leagueId").validate[String].get
    val fixture: Fixture = (jsonRequest \ "results").validate[Fixture].get
    val leagues: ListBuffer[JsValue] = ListBuffer[JsValue]()
    val teams: scala.collection.mutable.Map[String, Team] = scala.collection.mutable.Map[String, Team]()
    for(week <- fixture.fixture){
      weekResults(week, teams)
      val league = League(teams.values.toSeq.sortWith(_.statistics(Points) > _.statistics(Points)))
      val leagueAsString: String = Json.toJson(league).toString()
      leagues.append(Json.parse(leagueAsString))
    }
    Ok(Json.obj("leagueId" -> leagueId, "fixtureId" -> fixtureId, "tables" -> Json.toJson(leagues.toSeq)))
  }

  def weekResultTable = Action(parse.json) { implicit request =>
    val results: JsValue = request.body
    val week: Week = (results \ "results").validate[Week].get
    val teams: scala.collection.mutable.Map[String, Team] = scala.collection.mutable.Map[String, Team]()
    weekResults(week, teams)
    val league = League(teams.values.toSeq.sortWith(_.statistics(Points) > _.statistics(Points)))
    Ok(Json.obj("table" -> Json.toJson(league)))
  }

  def weekResults(week: Week, teams: scala.collection.mutable.Map[String, Team]): scala.collection.mutable.Map[String, Team] = {
    for(clash <- week.matches){
      val contenderOne: Contender = clash.teams head
      val contenderTwo: Contender = clash.teams last
      val teamOne: Team = teams.getOrElse(contenderOne.name, {
        val team: Team = Team(contenderOne.name, createStatsMap())
        team
      })
      val teamTwo: Team = teams.getOrElse(contenderTwo.name, {
        val team: Team = Team(contenderTwo.name, createStatsMap())
        team
      })
      updateTeam(teamOne, contenderOne.score, contenderTwo.score)
      updateTeam(teamTwo, contenderTwo.score, contenderOne.score)
      teams += (contenderOne.name -> teamOne)
      teams += (contenderTwo.name -> teamTwo)
    }
    return teams
  }

  def createStatsMap(): scala.collection.mutable.Map[EnumVal, Int] = {
    val map = scala.collection.mutable.Map[EnumVal, Int]()
    map += (Points -> 0)
    map += (Wins -> 0)
    map += (Draws -> 0)
    map += (Losses -> 0)
    map += (GoalsScored -> 0)
    map += (GoalsConceded -> 0)
    return map
  }

  def updateTeam(team: Team, goalsScored: Int, goalsConceded: Int): Team = {
    val result = goalsScored - goalsConceded
    result match{
      case x if x > 0 => {
        updateStat(team.statistics, Points, 3)
        updateStat(team.statistics, Wins, 1)
      }
      case 0 => {
        updateStat(team.statistics, Points, 1)
        updateStat(team.statistics, Draws, 1)
      }
      case x if x < 0 => {
        updateStat(team.statistics, Points, 0)
        updateStat(team.statistics, Losses, 1)
      }
    }
    updateStat(team.statistics, GoalsScored, goalsScored)
    updateStat(team.statistics, GoalsConceded, goalsConceded)
    return team
  }

  def updateStat(stats: scala.collection.mutable.Map[EnumVal, Int], enum: EnumVal, added: Int): scala.collection.mutable.Map[EnumVal, Int] ={
    val stat: Int = stats.getOrElse(enum, {
      stats += (enum -> 0)
      0
    })
    stats += (enum -> stat.+(added))
    return stats
  }
}
