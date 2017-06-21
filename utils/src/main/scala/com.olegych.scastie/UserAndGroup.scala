package com.olegych.scastie

sealed trait UserAndGroup
case class SomeUserAndGroup(user: String, group: String) extends UserAndGroup
case object NoUserAndGroup extends UserAndGroup

