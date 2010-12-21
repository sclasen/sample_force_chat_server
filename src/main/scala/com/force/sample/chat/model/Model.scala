package com.force.sample.chat.model

import org.scala_libs.jpa.LocalEMF
import net.liftweb.jpa.RequestVarEM


object Model extends LocalEMF("chatServer") with RequestVarEM