package com.olegych.scastie.backendapi

import akka.actor.DeadLetterSuppression

case object SbtPing extends DeadLetterSuppression
case object SbtPong

