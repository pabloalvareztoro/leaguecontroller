package controllers

import play.api.libs.json.{JsArray, JsValue, Json}
import play.api.libs.ws._
import play.api.mvc._
import javax.inject.Inject

import scala.concurrent.{ExecutionContext, Future}

class LeagueController @Inject() (ws: WSClient) extends Controller {

  implicit val context = play.api.libs.concurrent.Execution.Implicits.defaultContext

  def createFixture = Action.async { implicit request =>
    val futureResponse: Future[WSResponse] = for {
      responseOne <- ws.url("http://localhost:3000/teamcreator/10").get()
      responseTwo <- ws.url("http://localhost:3001/createfixture").post(responseOne.json)
    } yield responseTwo
    futureResponse map { response =>
      Ok(response.body)
    }
  }
}