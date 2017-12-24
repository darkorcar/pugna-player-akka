/*
 * Copyright 2017 Carlos Peña
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.craftycoder.pugna.player
import akka.typed.{ActorRef, Behavior}
import akka.typed.scaladsl.Actor
import org.apache.logging.log4j.scala.Logging

import scala.util.Random

object PlayerEngine extends Logging {

  val Name = "Dummy-Player-Engine"

  def nextMovement(movementRequest: MovementRequest)(
      replyTo: ActorRef[CalculateNextMovementReply]): CalculateNextMovement =
    CalculateNextMovement(movementRequest, replyTo)

  def apply(): Behavior[Command] = Actor.immutable {
    case (ctx, CalculateNextMovement(movementRequest, replyTo)) =>
      replyTo ! NextMovement(getRandomMovement)
      Actor.same
  }

  sealed trait Command
  case class CalculateNextMovement(
      movementRequest: MovementRequest,
      actorRef: ActorRef[CalculateNextMovementReply])
      extends Command

  sealed trait CalculateNextMovementReply
  case class NextMovement(movement: Movement) extends CalculateNextMovementReply

  private def getRandomMovement: Movement = {
    ALL_MOVEMENTS(Random.nextInt(ALL_MOVEMENTS.size))
  }

  private val ALL_MOVEMENTS =
    Seq(STAY, UP, UP_LEFT, UP_RIGHT, RIGHT, LEFT, DOWN, DOWN_LEFT, DOWN_RIGHT)

}
