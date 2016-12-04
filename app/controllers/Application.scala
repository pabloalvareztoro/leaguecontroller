package controllers

import play.api.mvc._
import play.api.libs.json.Json._

object Application extends Controller {

  def index = Action { implicit request =>
    Ok("League Controller service")
  }
}
