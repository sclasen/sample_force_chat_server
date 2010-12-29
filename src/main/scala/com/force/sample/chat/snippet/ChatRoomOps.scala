package com.force.sample.chat.snippet

import _root_.scala.xml.{NodeSeq, Text}

import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.common.Box
import Helpers._
import net.liftweb.http._
import net.liftweb.http.S._
import com.force.sample.chat.model.ChatRoom
import com.force.sample.chat.api.{ChatStorage, JpaChatStorage, AkkaChatStorage}

class ChatRoomOps {

  def list(xhtml: NodeSeq): NodeSeq = {

    val roomChoices = BackendVar.storage.getChatRoomSelection
    bind("room", xhtml,
      "select" -> SHtml.select(roomChoices, Box(roomId), SelectedChatRoom(_)),
      "label" -> <label>Select {BackendVar.descRoom}</label>,
      "submit" -> <input type="submit" class="submit" value="go"/>)
  }

  def setUsername(xhtml: NodeSeq): NodeSeq = {
    SHtml.ajaxForm(bind("name", xhtml, "username" -> SHtml.text(username, UsernameVar(_)),
      "submit" -> <input type="submit" class="submit" value="Set"/>)
    )
  }

  def setBackend(xhtml: NodeSeq): NodeSeq = {


    val backends = List("Jpa" -> "Jpa Storage Backend", "Akka" -> "Akka StorageBackend")

    bind("storage", xhtml,
      "select" -> SHtml.select(backends, Box(backend), {
        BackendVar(_)
      }),
      "submit" -> <input type="submit" class="submit" value="go"/>)
  }

  def username = UsernameVar.is

  def roomId = SelectedChatRoom.is

  object roomVar extends RequestVar(new ChatRoom)

  def room = roomVar.is

  def backend = BackendVar.is

  def add(xhtml: NodeSeq): NodeSeq = {
    def doAdd() = {
      BackendVar.storage.createChatRoom(room)
      redirectTo("index.html")
    }

    bind("add", xhtml,
      "name" -> SHtml.text("Room Name", room.name = _),
      "label" -> <label>Or Create {BackendVar.descRoom}</label>,
      "submit" -> SHtml.submit(?("Create Room"), doAdd, "class" -> "submit")
    )

  }

}

object UsernameVar extends SessionVar("anonymous") {
  override def apply(what: String) = {
    logger.info("User:" + what)
    super.apply(what)
  }

}

object BackendVar extends SessionVar("Jpa") {
  override def apply(what: String) = {
    logger.info("Backend:" + what)
    SelectedChatRoom("")
    super.apply(what)
  }

  private def isAkka = this.is == "Akka"

  def storage(): ChatStorage = {
    if (isAkka) {
      AkkaChatStorage
    } else {
      JpaChatStorage
    }
  }

  def comet() = {
    if (isAkka) {
      "AkkaChat"
    } else {
      "JpaChat"
    }
  }

  def descRoom = {
    if(isAkka) "an Akka ChatRoom" else "a Jpa ChatRoom"
  }

}

object SelectedChatRoom extends SessionVar[String]("") {
  override def apply(what: String) = {
    logger.info("ChatRoom:" + what)
    super.apply(what)
  }

  def room: ChatRoom = {
    BackendVar.storage.getChatRoom(this.is)
  }
}

object ChatStart {

  def render(xthml: NodeSeq): NodeSeq = {
    if (SelectedChatRoom.is ne "") {
      template
    } else {
      <h2>Select or Create {BackendVar.descRoom}</h2>
    }
  }

  def template(): NodeSeq = {
    <div id="chatHeader">
      <h2>{BackendVar.comet} Room: {SelectedChatRoom.room.name}</h2>
    </div>
      <div id="chatDiv" class="chatContent">
        <lift:comet type={BackendVar.comet} name={SelectedChatRoom.is}>
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
      </div>

  }

  def start(): NodeSeq = {
    S.render(template(), S.request.get.request)
  }

}

