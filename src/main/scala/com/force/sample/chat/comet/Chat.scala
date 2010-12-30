package com.force.sample.chat.comet

import net.liftweb._
import common.Loggable
import http._
import actor._
import js._
import js.JE.JsRaw
import JsCmds._
import js.jquery.JqJsCmds.{AppendHtml, FadeOut, Hide, FadeIn}
import scala.xml._
import util.Helpers._
import collection.JavaConversions
import java.util.concurrent.{ConcurrentHashMap => JCHMap}
import com.force.sample.chat.model.{Message, ChatRoom}
import ChatCmd._
import com.force.sample.chat.snippet.Username
import com.force.sample.chat.api.{AkkaChatStorage, JpaChatStorage, ChatStorage}

sealed trait ChatCmd

object ChatCmd {
  //for creating a new yet-to-be-persisted message
  def strToMsg(msg: String, user: String): AddMessage =
    new AddMessage(null, msg, System.currentTimeMillis, user)
}

final case class AddMessage(guid: String, msg: String, time: Long, user: String) extends ChatCmd {
  def toMessage: Message = {
    val mess = new Message
    mess.message = msg
    mess.created = time
    mess.user = user
    mess
  }
}

final case class RemoveMessage(guid: String) extends ChatCmd


object ChatServer {

  private var chatRooms = JavaConversions.asScalaConcurrentMap(new JCHMap[(String, ChatStorage), ChatServer])

  def getServer(id: String, storage: ChatStorage): ChatServer = {
    chatRooms.getOrElse((id,storage), {
      val room = storage.getChatRoomWithMessages(id)
      val server = new ChatServer(room, storage)
      chatRooms.putIfAbsent((id,storage),server) match {
        case Some(other) => other
        case None => server
      }
    })
  }

  def message2AddMessage(msg: Message): AddMessage = {
    new AddMessage(msg.id, msg.message, msg.created, msg.user)
  }
}

class ChatServer(var room: ChatRoom, storage: ChatStorage) extends LiftActor with ListenerManager with Loggable {

  import ChatServer._

  private var messages: List[ChatCmd] = Nil ++ JavaConversions.asScalaBuffer(room.messages).sortBy(_.created).map(msg => message2AddMessage(msg))

  def createUpdate = messages

  override def lowPriority = {
    case add: AddMessage => {
      val message = add.toMessage
      room = storage.addMessage(room, message)
      messages ::= message2AddMessage(message)
      updateListeners()
    }
    case d: RemoveMessage => {
      messages ::= d
      if (storage.removeMessage(room, d.guid)) {
        updateListeners()
      }

    }
  }


}

class JpaChat extends Chat {
  def chatServer = ChatServer.getServer(name.get, JpaChatStorage)
}

class AkkaChat extends Chat {
  def chatServer = ChatServer.getServer(name.get, AkkaChatStorage)
}


trait Chat extends CometActor with CometListener {

  private var msgs: List[ChatCmd] = Nil
  private var bindLine: NodeSeq = Nil
  lazy val server = chatServer

  def chatServer: ChatServer

  def registerWith = server

  override def lowPriority = {
    case m: List[ChatCmd] => {
      val delta = m diff msgs
      msgs = m
      updateDeltas(delta)
    }
  }

  def updateDeltas(what: List[ChatCmd]) {
    val list = what.reverse.foldRight(Noop) {
      case (m: AddMessage, x) =>
        x & AppendHtml("ul_dude", doLine(m)) &
          Hide(m.guid) & FadeIn(m.guid, TimeSpan(0), TimeSpan(500)) & After(TimeSpan(500), JsRaw("scroll()").cmd)
      case (RemoveMessage(guid), x) =>
        x & FadeOut(guid, TimeSpan(0), TimeSpan(500)) &
          After(TimeSpan(500), Replace(guid, NodeSeq.Empty)) & After(TimeSpan(500), JsRaw("scroll()").cmd)
    }
    partialUpdate(list)
  }

  def render =
    bind("chat", // the namespace for binding
      "line" -> lines _, // bind the function lines
      "input" -> SHtml.text("", s => server ! strToMsg(s, Username.is))
    )

  // the input

  private def lines(xml: NodeSeq): NodeSeq = {
    bindLine = xml
    val deleted = Set((for {
      RemoveMessage(guid) <- msgs
    } yield guid): _*)

    for {
      m@AddMessage(guid, msg, date, user) <- msgs.reverse if !deleted.contains(guid)
      node <- doLine(m)
    } yield node
  }

  private def doLine(m: AddMessage): NodeSeq =
    bind("chat", addId(bindLine, m.guid),
      "msg" -> m.msg,
      "user" -> m.user,
      "btn" -> SHtml.ajaxButton("delete",
        () => {
          server !
            RemoveMessage(m.guid)
          Noop
        }))


  private def addId(in: NodeSeq, id: String): NodeSeq = in map {
    case e: Elem => e % ("id" -> id)
    case x => x
  }
}
