package com.force.sample.chat.snippet

import xml.NodeSeq
import net.liftweb.common.Empty
import _root_.java.text.{ParseException, SimpleDateFormat}

import _root_.scala.xml.{NodeSeq, Text}

import _root_.net.liftweb.util.{Helpers}
import _root_.net.liftweb.common.{Box, Empty, Full, Loggable}
import Helpers._
import net.liftweb.http._
import js.JE.JsRaw
import js.jquery.JqJsCmds
import js.JsCmd
import js.JsCmds.{SetHtml, Replace}
import net.liftweb.http.S._
import com.force.sample.chat.model.{Message, ChatRoom, Model}

class ChatRoomOps {

  import SelectedChatRoom._

  def getRooms = Model.createQuery[ChatRoom]("select c from ChatRoom c order by c.name").getResultList

  def list(xhtml: NodeSeq): NodeSeq = {

    val rooms = getRooms

    def go() = {
      redirectTo("index.html")
    }

    val roomChoices = rooms.map(room => (room.id -> room.name)).toList
    bind("room", xhtml,
      "select" -> SHtml.select(roomChoices, Box(roomId), SelectedChatRoom(_)),
      "submit" -> <input type="submit" value="go"/>)
  }

  def setUsername(xhtml: NodeSeq): NodeSeq = {

    SHtml.ajaxForm(bind("name", xhtml, "username" -> SHtml.text(username, UsernameVar(_)),
      "submit" -> <input type="submit" value="Set"/>)
    )
  }

  def username = UsernameVar.is

  def roomId = SelectedChatRoom.is

  object roomVar extends RequestVar(new ChatRoom)

  def room = roomVar.is

  def add(xhtml: NodeSeq): NodeSeq = {
    def doAdd() = {
      Model.mergeAndFlush(room)
      redirectTo("index.html")
    }

    bind("add", xhtml,
      "name" -> SHtml.text("Room Name", room.name = _),
      "submit" -> SHtml.submit(?("Create Room"), doAdd)
    )

  }


  def debug(xhtml: NodeSeq): NodeSeq = {
    val rooms = getRooms
    var debug = ""
    rooms.foreach{
      room => {
        debug += room.name + " messages:" + room.messages.size.toString + "<br/>"
      }
    }
    bind("debug", xhtml, "debug" -> Text(debug))
  }

  def msgId = msgVar.is

  object msgVar extends RequestVar("")

  def deleteMessage(xhtml: NodeSeq): NodeSeq = {
    def delete() = {
      Model.find(classOf[Message], msgId) match {
        case Some(msg) => {
          Model.removeAndFlush(msg)
          redirectTo("debug.html")
        }
        case None => {
          redirectTo("debug.html")
        }
      }

    }

    bind("delete", xhtml,
      "message" -> SHtml.text("message id", msgVar(_)),
      "submit" -> SHtml.submit(?("Delete Message"), delete)
    )

  }

}

object UsernameVar extends SessionVar("anonymous") {
  override def apply(what: String) = {
    logger.info("User:" + what)
    super.apply(what)
  }
}

object SelectedChatRoom extends SessionVar[String]("") {
  override def apply(what: String) = {
    logger.info("ChatRoom:" + what)
    super.apply(what)
  }

  def room: ChatRoom = {
    Model.find(classOf[ChatRoom], this.is).get
  }
}

object ChatStart {

  def render(xthml: NodeSeq): NodeSeq = {
    if (SelectedChatRoom.is ne "") {
      template
    } else {
      Text("")
    }
  }

  def template(): NodeSeq = {
    <div>Chat Room: {SelectedChatRoom.room.name}</div>
      <lift:comet type="Chat" name={SelectedChatRoom.is}>
        <ul id="ul_dude">
          <chat:line>
            <li>
                <chat:user/>:
                <chat:msg/>
                <chat:btn/>
            </li>
          </chat:line>
        </ul>
        <lift:form>
            <chat:input/>
            <input type="submit" value="chat"/>
        </lift:form>
      </lift:comet>
  }

  def start(): NodeSeq = {
    S.render(template(), S.request.get.request)
  }

}

