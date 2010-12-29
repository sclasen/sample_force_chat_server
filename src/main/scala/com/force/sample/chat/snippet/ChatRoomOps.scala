package com.force.sample.chat.snippet

import _root_.scala.xml.{NodeSeq, Text}

import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.common.Box
import Helpers._
import net.liftweb.http._
import net.liftweb.http.S._
import com.force.sample.chat.model.ChatRoom
import com.force.sample.chat.api.ChatStorage

class ChatRoomOps {

  def list(xhtml: NodeSeq): NodeSeq = {

    def go() = {
      redirectTo("index.html")
    }

    val roomChoices = ChatStorage.it.getChatRoomSelection
    bind("room", xhtml,
      "select" -> SHtml.select(roomChoices, Box(roomId), SelectedChatRoom(_)),
      "submit" -> <input type="submit" class="submit" value="go"/>)
  }

  def setUsername(xhtml: NodeSeq): NodeSeq = {
    SHtml.ajaxForm(bind("name", xhtml, "username" -> SHtml.text(username, UsernameVar(_)),
      "submit" -> <input type="submit" class="submit" value="Set"/>)
    )
  }

  def username = UsernameVar.is

  def roomId = SelectedChatRoom.is

  object roomVar extends RequestVar(new ChatRoom)

  def room = roomVar.is

  def add(xhtml: NodeSeq): NodeSeq = {
    def doAdd() = {
      ChatStorage.it.createChatRoom(room)
      redirectTo("index.html")
    }

    bind("add", xhtml,
      "name" -> SHtml.text("Room Name", room.name = _),
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

object SelectedChatRoom extends SessionVar[String]("") {
  override def apply(what: String) = {
    logger.info("ChatRoom:" + what)
    super.apply(what)
  }

  def room: ChatRoom = {
    ChatStorage.it.getChatRoom(this.is)
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
    <div id="header">
      <h3>Chat Room: {SelectedChatRoom.room.name}</h3>
    </div>
      <div id="chatDiv" class="chatContent">
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
      </div>

  }

  def start(): NodeSeq = {
    S.render(template(), S.request.get.request)
  }

}

