package com.force.sample.chat.api

import org.scala_libs.jpa.{ScalaEntityManager, LocalEMF}
import com.force.sample.chat.model.{Message, ChatRoom}
import collection.JavaConversions
import akka.persistence.force.ForceStorage
import akka.actor.Actor
import java.io.{ObjectInputStream, ByteArrayInputStream, ObjectOutputStream, ByteArrayOutputStream}


trait ChatStorage {

  def createChatRoom(room: ChatRoom): Unit

  def getChatRoom(id: String): ChatRoom

  def getChatRoomWithMessages(id: String): ChatRoom

  def getChatRoomSelection(): List[(String, String)]

  def addMessage(room: ChatRoom, msg: Message): ChatRoom

  def removeMessage(room: ChatRoom, msgId: String): Boolean

}


object Jpa extends LocalEMF("chatServer") with ScalaEntityManager {
  protected def em = openEM

  val factory = this

  def withEM[T](block: ScalaEntityManager => T): T = {
    val mgr = newEM
    val res = block(mgr)
    mgr.flush
    mgr.clear
    mgr.close
    res
  }
}

object JpaChatStorage extends ChatStorage {

  import Jpa._


  def getChatRoomSelection(): List[(String, String)] = {
    withEM(_.createQuery[ChatRoom]("select c from ChatRoom c order by c.name").getResultList.map(room => (room.id -> room.name)).toList)
  }

  def createChatRoom(room: ChatRoom) = {
    withEM(_.mergeAndFlush(room))
  }

  def getChatRoom(id: String): ChatRoom = {
    withEM(_.find(classOf[ChatRoom], id)).get
  }

  def getChatRoomWithMessages(id: String) = {
    withEM{
      em => {
        val room = em.find(classOf[ChatRoom], id).get
        room.messages.size
        room
      }
    }
  }

  def addMessage(room: ChatRoom, msg: Message): ChatRoom = {
    withEM{
      em => {
        val theRoom = em.merge(room)
        em.persist(msg)
        theRoom.messages.add(msg)
        theRoom
      }
    }
  }

  def removeMessage(room: ChatRoom, msgId: String): Boolean = {
    withEM{
      em => {
        val idx = JavaConversions.asScalaBuffer(room.messages).findIndexOf({
          m => {
            m.id eq msgId
          }
        })
        if (idx != -1) {
          room.messages.remove(idx)
          em.merge(room)
          true
        } else {
          false
        }
      }
    }
  }
}

object AkkaChatStorage extends ChatStorage {

  import akka.actor._
  import akka.actor.Supervisor
  import akka.actor.Actor._
  import akka.config.Supervision.{OneForOneStrategy, SupervisorConfig}


  val supervisor = Supervisor(
    SupervisorConfig(OneForOneStrategy(List(classOf[Exception]), 3, 100), Nil))

  private def getChatActor(roomid: String): ActorRef = {
    val actors = Actor.registry.actorsFor(roomid)
    log.debug("found %d actors for %s", actors.size, roomid)
    actors.size match {
      case 0 => synchronized{
        val ref = actorOf(new ChatStorageActor(roomid)).start
        supervisor.link(ref)
        ref
      }
      case _ => actors(0)
    }
  }

  private def getChatRoomListActor(): ActorRef = {
    Actor.registry.actorFor[ChatRoomListActor] match {
      case Some(ref) => ref
      case None => throw new RuntimeException("Got None while looking up ChatRoomListActor")
    }
  }

  def removeMessage(room: ChatRoom, msgId: String) = {
    val idx = JavaConversions.asScalaBuffer(room.messages).findIndexOf({
      m => {
        m.id eq msgId
      }
    })
    if (idx != -1) {
      room.messages.remove(idx)
      getChatActor(room.id) ! RemoveChatMessage(msgId)
      true
    } else {
      false
    }

  }

  def addMessage(room: ChatRoom, msg: Message) = {
    msg.id = newUuid.toString
    getChatActor(room.id) ! AddChatMessage(msg)
    room.messages.add(msg)
    room
  }

  def getChatRoomSelection() = {
    getChatRoomListActor !! GetRoomSelection match {
      case Some(GetRoomSelectionList(list)) => list
      case None => throw new RuntimeException("timed out waiting for chat room list")
    }
  }

  def getChatRoomWithMessages(id: String) = {
    getChatActor(id) !! GetRoomWithMessages match {
      case Some(GetRoomWithMessagesResponse(room)) => room
      case None => throw new RuntimeException("timed out waiting for chat room and messages")
    }
  }

  def getChatRoom(id: String) = {
    getChatActor(id) !! GetRoom match {
      case Some(GetRoomResponse(room)) => room
      case None => throw new RuntimeException("timed out waiting for chat room")
    }
  }

  def createChatRoom(room: ChatRoom) = {
    getChatRoomListActor ! AddRoom(room.name)
  }


}

case class AddRoom(name: String)

case class GetRoomSelection

case class GetRoomSelectionList(list: List[(String, String)])


class ChatRoomListActor extends Actor {

  import akka.stm._

  val name = "-->chatRooms<--"
  val rooms = ForceStorage.getMap(name)

  protected def receive = {
    case AddRoom(roomName) => atomic{
      if (roomName != name)
        rooms.put(roomName.getBytes, roomName.getBytes)
    }

    case GetRoomSelection => atomic{
      val list = rooms.map{
        kv => {
          (new String(kv._1), new String(kv._2))
        }
      }.toList
      self.reply(GetRoomSelectionList(list))
    }

  }
}

case class GetRoomWithMessages

case class GetRoomWithMessagesResponse(room: ChatRoom)

case class GetRoom

case class GetRoomResponse(room: ChatRoom)

case class AddChatMessage(msg: Message)

case class RemoveChatMessage(msgId: String)

class ChatStorageActor(id: String) extends Actor {

  import akka.stm._

  val messages = ForceStorage.getMap(id)

  protected def receive = {
    case AddChatMessage(msg) => atomic{
      messages.put(msg.id.getBytes, msg2bytes(msg))
    }
    case RemoveChatMessage(id) => atomic{
      messages.remove(id.getBytes)
    }
    case GetRoomWithMessages => atomic{
      val room = new ChatRoom
      room.id = id
      room.name = id
      room.messages.addAll(JavaConversions.asJavaCollection(messages.values.map(b => bytes2msg(b)).toList.sortBy(m => m.created)))
      self.reply(GetRoomWithMessagesResponse(room))
    }
    case GetRoom => atomic{
      val room = new ChatRoom
      room.name = id
      room.id = id
      self.reply(GetRoomResponse(room))
    }
  }

  def msg2bytes(msg: Message): Array[Byte] = {
    val bo = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(bo)
    oos.writeObject(msg)
    oos.close
    bo.toByteArray
  }

  def bytes2msg(bytes: Array[Byte]): Message = {
    val bi = new ByteArrayInputStream(bytes)
    val ois = new ObjectInputStream(bi)
    val m = ois.readObject().asInstanceOf[Message]
    ois.close
    m
  }
}





