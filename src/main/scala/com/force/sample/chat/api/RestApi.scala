package com.force.sample.chat.api

import net.liftweb.http.rest._
import net.liftweb.http.{GetRequest, Req}
import net.liftweb.json.JsonAST._
import net.liftweb.json.Extraction
import com.force.sample.chat.comet.{ChatServer, AddMessage}
import collection.mutable.Buffer
import collection.JavaConversions._

object RestApi extends RestHelper {


  def getStorage(api: String): ChatStorage = {
    api match {
      case "akka" => AkkaChatStorage
      case _ => JpaChatStorage
    }
  }

  serve{
    case Req("api" :: "transcript" :: api :: room, "json", GetRequest) => {
      val storage = getStorage(api)
      val robj = storage.getChatRoomWithMessages(room(0))
      val msgs: Buffer[AddMessage] = asScalaBuffer(robj.messages.map(ChatServer.message2AddMessage(_)))
      JObject(List(JField("room", JString(room(0))),
        JField("api", JString(api)),
        JField("messages", JArray(msgs.map(Extraction.decompose(_)).toList))))
    }
    case Req("api" :: "rooms" :: api, "json", GetRequest) => {
      val storage = getStorage(api(0))
      val rooms: List[(String, String)] = storage.getChatRoomSelection
      JObject(rooms.map(kv => JField(kv._2, JString(kv._1))))
    }

  }


}