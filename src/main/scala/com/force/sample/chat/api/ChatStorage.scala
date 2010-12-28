package com.force.sample.chat.api

import com.force.sample.chat.model.{Message, ChatRoom}

trait ChatStorage {

  def createChatRoom(name:String):ChatRoom

  def getChatRoom(name:String):ChatRoom

  def addMessage(room:ChatRoom, msg:Message)

  def removeMessage(room:ChatRoom, msg:Message)

}