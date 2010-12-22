package com.force.sample.chat.comet

import net.liftweb._
import common.Loggable
import http._
import actor._
import js._
import js.JE.JsRaw
import JsCmds._
import js.jquery.JqJsCmds.{AppendHtml, FadeOut, Hide, FadeIn}
import java.util.Date
import scala.xml._
import util.Helpers
import util.Helpers._
import collection.JavaConversions
import java.util.concurrent.{ConcurrentHashMap => JCHMap}
import com.force.sample.chat.model.{Message, ChatRoom, Model}
import ChatCmd._
import javax.persistence.LockModeType
import org.scala_libs.jpa.{ScalaEntityManager, ScalaEMFactory, LocalEMF}
import com.force.sample.chat.snippet.UsernameVar

sealed trait ChatCmd

object ChatCmd {
  def strToMsg(msg: String, user: String): AddMessage =
    new AddMessage(null, msg, System.currentTimeMillis, user)

  //null right now, fix, hacking around required  GenerationType
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


object ChatServer extends LocalEMF("chatServer") with ScalaEntityManager {

  private var chatRooms = JavaConversions.asScalaConcurrentMap(new JCHMap[String, ChatServer])
  val factory = this

  protected def em = openEM

  def getServer(id: String): ChatServer = {
    chatRooms.getOrElse(id, {
      val room = Model.find(classOf[ChatRoom], id).get
      val server = new ChatServer(room)
      chatRooms.putIfAbsent(id, server) match {
        case Some(other) => other
        case None => server
      }
    })
  }

  def withEM(block: ScalaEntityManager => Unit) {
    val mgr = newEM
    block(mgr)
    mgr.flush
    mgr.clear
    mgr.close
  }

  def message2AddMessage(msg: Message): AddMessage = {
    new AddMessage(msg.id, msg.message, msg.created, msg.user)
  }
}

class ChatServer(var room: ChatRoom) extends LiftActor with ListenerManager with Loggable {

  import ChatServer._

  private var messages: List[ChatCmd] = Nil ++ JavaConversions.asScalaBuffer(room.messages).sortBy(_.created).map(msg => message2AddMessage(msg))

  def createUpdate = messages

  override def lowPriority = {
    case add: AddMessage => withEM{
      em =>
        room = em.merge(room)
        val message = add.toMessage
        em.persist(message)
        room.messages.add(message)
        messages ::= message2AddMessage(message)
        updateListeners()
    }
    case d: RemoveMessage => withEM{
      em =>
        messages ::= d
        val idx = JavaConversions.asScalaBuffer(room.messages).findIndexOf({
          msg => {
            msg.id eq d.guid
          }
        })
        if (idx != -1) {
          room.messages.remove(idx)
          em.merge(room)
          updateListeners()
        }
    }
  }


}


class Chat extends CometActor with CometListener {

  private var msgs: List[ChatCmd] = Nil
  private var bindLine: NodeSeq = Nil
  private lazy val server = ChatServer.getServer(name.get)

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
      "input" -> SHtml.text("", s => server ! strToMsg(s, UsernameVar.is))

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
