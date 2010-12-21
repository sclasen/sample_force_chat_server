package com.force.sample.chat.model

import java.util.{List => JList, ArrayList => JAList}
import javax.persistence._

@Entity
class ChatRoom {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  var id: String = ""

  @Column(unique = true,  nullable = false)
  var name: String = ""

  @OneToMany(targetEntity = classOf[Message], cascade = Array(CascadeType.ALL), mappedBy="chatRoom")
  @OrderBy("created ASC")
  var messages: JList[Message] = new JAList[Message]



}