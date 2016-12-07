package controllers

import play.api.libs.json.{JsArray, JsValue, Json}
import play.api.libs.ws._
import play.api.mvc._
import javax.inject.Inject

import models.League
import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.{MongoController, ReactiveMongoApi, ReactiveMongoComponents}
import reactivemongo.api.commands.WriteResult
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import api.JsonCombinators._
import play.modules.reactivemongo.json._

import scala.concurrent.{ExecutionContext, Future}

class LeagueController @Inject() (val ws: WSClient, val reactiveMongoApi: ReactiveMongoApi) extends Controller with MongoController with ReactiveMongoComponents {

  def collection = reactiveMongoApi.db.collection[JSONCollection]("teams");

  implicit val context = play.api.libs.concurrent.Execution.Implicits.defaultContext

  def createLeague = Action.async { implicit request =>
    val leagueId: String = BSONObjectID.generate.stringify
    val futureResponse: Future[WSResponse] = for {
      league <- ws.url("http://localhost:3000/teamcreator/10").get()
      saveLeague <- collection.insert(Json.obj("leagueId" -> leagueId, "teams" -> league.body)).map { response =>
        Created
      }
    } yield league
    futureResponse.map { response =>
      Ok(Json.obj("leagueId" -> leagueId, "league" -> response.body))
    }
  }
}