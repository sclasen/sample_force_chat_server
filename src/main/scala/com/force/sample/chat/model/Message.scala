package com.force.sample.chat.model

import javax.persistence._

@Entity
class Message {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  var id: String = ""
  @Column
  var message: String = ""
  @Column
  var user: String = ""
  @Column
  var created: Long = _
  @Column(name="chatRoom")
  @ManyToOne
  var chatRoom: ChatRoom = _

}