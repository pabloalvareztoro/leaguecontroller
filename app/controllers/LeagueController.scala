package controllers

import play.api.libs.json.{JsArray, JsValue, Json}
import play.api.libs.json.Json._
import play.api.libs.ws._
import play.api.mvc._
import play.libs.F.Promise
import play.mvc.Http.Response
import javax.inject.Inject

import scala.concurrent.{ExecutionContext, Future}

class LeagueController @Inject() (ws: WSClient) extends Controller {

  implicit val context = play.api.libs.concurrent.Execution.Implicits.defaultContext

  def createFixture = Action.async { implicit request =>
      val request: WSRequest = ws.url("http://localhost:3000/teamcreator/20");
      val complexRequest: WSRequest =
        request.withHeaders("Accept" -> "application/json")
      complexRequest.get().map { response =>
        Ok(response.body)
      }
  }
}