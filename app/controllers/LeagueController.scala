package controllers

import play.api.libs.json.{JsArray, JsValue, Json}
import play.api.libs.ws._
import play.api.mvc._
import javax.inject.Inject

import models._
import models.TeamStatistics._
import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.{MongoController, ReactiveMongoApi, ReactiveMongoComponents}
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import api.JsonCombinators._
import play.api.Play
import play.modules.reactivemongo.json._

import scala.concurrent.{ExecutionContext, Future}

class LeagueController @Inject() (val ws: WSClient, val reactiveMongoApi: ReactiveMongoApi, val configuration: play.api.Configuration) extends Controller with MongoController with ReactiveMongoComponents {

  def leagueCollection = db.collection[JSONCollection]("teams");
  def fixtureCollection = db.collection[JSONCollection]("fixtures");
  def resultCollection = db.collection[JSONCollection]("results");

  implicit val context = play.api.libs.concurrent.Execution.Implicits.defaultContext

  def createLeague = Action.async { implicit request =>
    val leagueId: String = BSONObjectID.generate.stringify
    val futureResponse: Future[WSResponse] = getFutureTeamsCreator(20, leagueId, false)
    futureResponse.map { response =>
      Ok(Json.obj("leagueId" -> leagueId, "league" -> response.json))
    }
  }

  def createLeagueFixedTeams(teams: Int) = Action.async { implicit request =>
    val leagueId: String = BSONObjectID.generate.stringify
    val futureResponse: Future[WSResponse] = getFutureTeamsCreator(teams, leagueId, false)
    futureResponse.map { response =>
      Ok(Json.obj("leagueId" -> leagueId, "league" -> response.json))
    }
  }

  def createRealLeague(league: String) = Action.async { implicit request =>
    val leagueId: String = BSONObjectID.generate.stringify
    val futureResponse: Future[WSResponse] = getFutureTeamsCreator(20, leagueId, true, league)
    futureResponse.map { response =>
      Ok(Json.obj("leagueId" -> leagueId, "league" -> response.json))
    }
  }

  def createRealLeagueFixedTeams(league: String, teams: Int) = Action.async { implicit request =>
    val leagueId: String = BSONObjectID.generate.stringify
    val futureResponse: Future[WSResponse] = getFutureTeamsCreator(teams, leagueId, true, league)
    futureResponse.map { response =>
      Ok(Json.obj("leagueId" -> leagueId, "league" -> response.json))
    }
  }

  def getFutureTeamsCreator(teams: Int, leagueId: String, isReal: Boolean, league: String): Future[WSResponse] = {
    val leagueType = isReal ? "realteams" : "nonexistentteams"
    var endpoint =  + "/teamcreator/" + leagueType + "/" + teams
    if isReal{
      if(league != "all") {
        endpoint = endpoint + "/" + league
      }
    }
    val futureResponse: Future[WSResponse] = for {
      league <- ws.url(configuration.underlying.getString("leaguemaker.teamscreator.uri") + endpoint).get()
      saveLeague <- leagueCollection.insert(Json.obj("leagueId" -> leagueId, "league" -> league.json))
        .map {response => Created}
        .recover {case _ => InternalServerError("DB Failure")}
    } yield league
    return futureResponse
  }

  def createFixture = Action.async(parse.json) { implicit request =>
    val jsonRequest: JsValue = request.body
    val fixtureId: String = BSONObjectID.generate.stringify
    val leagueId: String = (jsonRequest \ "leagueId").validate[String].get
    val league: JsValue = (jsonRequest \ "league").get
    val futureResponse: Future[WSResponse] = for {
      fixture <- ws.url(configuration.underlying.getString("leaguemaker.fixturecreator.uri") + "/createfixture").post(league)
      saveFixture <- fixtureCollection.insert(Json.obj("leagueId" -> leagueId, "fixtureId" -> fixtureId, "fixture" -> fixture.json))
        .map {response => Created}
        .recover {case _ => InternalServerError("DB Failure")}
    } yield fixture
    futureResponse.map { response =>
      Ok(Json.obj("fixtureId" -> fixtureId, "leagueId" -> leagueId, "fixture" -> response.json))
    }
  }

  def playFixture = Action.async(parse.json) { implicit request =>
    val jsonRequest: JsValue = request.body
    val fixture: JsValue = (jsonRequest \ "fixture").get
    val fixtureId: String = (jsonRequest \ "fixtureId").validate[String].get
    val leagueId: String = (jsonRequest \ "leagueId").validate[String].get
    val futureResponse: Future[WSResponse] = for {
      fixtureResults <- ws.url(configuration.underlying.getString("leaguemaker.matchresult.uri") + "/fixtureresult").post(fixture)
      saveFixture <- resultCollection.insert(Json.obj("leagueId" -> leagueId, "fixtureId" -> fixtureId, "fixture" -> fixtureResults.json))
        .map {response => Created}
        .recover {case _ => InternalServerError("DB Failure")}
    } yield fixtureResults
    futureResponse.map { response =>
      Ok(Json.obj("leagueId" -> leagueId, "fixtureId" -> fixtureId, "results" -> response.json))
    }
  }

  def playWeek = Action.async(parse.json) { implicit request =>
    val week: JsValue = request.body
    val futureResponse: Future[WSResponse] = for {
      weekResults <- ws.url(configuration.underlying.getString("leaguemaker.matchresult.uri") + "/matchresult").post(week)
    } yield weekResults
    futureResponse.map { response =>
      Ok(Json.obj("results" -> response.json))
    }
  }
}