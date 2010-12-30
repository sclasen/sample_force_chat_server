package bootstrap.liftweb

import net.liftweb._
import common._
import http._
import sitemap._
import akka.actor.Supervisor
import akka.actor.Actor._
import akka.config.Supervision.{Supervise, OneForOneStrategy, SupervisorConfig, Permanent}
import com.force.sample.chat.api.ChatRoomListActor


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {

    // where to search snippet
    LiftRules.addToPackages("com.force.sample.chat")
    // Build SiteMap
    val entries = List(
      Menu.i("Lift Chat Server") / "index")
    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMap(SiteMap(entries: _*))
    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)
    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))
    //Start the supervisor for the ChatRoomListActor and the ChatRoomListActor
    val supervisor = Supervisor(
      SupervisorConfig(OneForOneStrategy(List(classOf[Exception]), 3, 100), Supervise(actorOf[ChatRoomListActor].start, Permanent) :: Nil))

  }
}
