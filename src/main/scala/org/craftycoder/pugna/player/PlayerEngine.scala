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
import akka.typed.scaladsl.Actor
import akka.typed.{ActorRef, Behavior}
import org.apache.logging.log4j.scala.Logging

import scala.collection.mutable

object PlayerEngine extends Logging {

  val Name = "Dummy-Player-Engine"

  val lastMoveSequence: mutable.LinkedHashMap[Position, Int] =
    mutable.LinkedHashMap.empty

  def nextMovement(movementRequest: MovementRequest)(
    replyTo: ActorRef[CalculateNextMovementReply]): CalculateNextMovement =
    CalculateNextMovement(movementRequest, replyTo)

  def apply(): Behavior[Command] = Actor.immutable {
    case (ctx, CalculateNextMovement(movementRequest, replyTo)) =>
      replyTo ! NextMovement(getRandomMovement(movementRequest))
      Actor.same
  }

  sealed trait Command
  case class CalculateNextMovement(
                                    movementRequest: MovementRequest,
                                    actorRef: ActorRef[CalculateNextMovementReply])
    extends Command

  sealed trait CalculateNextMovementReply
  case class NextMovement(movement: Movement) extends CalculateNextMovementReply

  var lastOffsetMovement: Movement = DOWN_LEFT

  private def getRandomMovement(movementRequest: MovementRequest): Movement = {
    lastMoveSequence.get(movementRequest.positionToMove) match {
      case Some(lastPositionSequence) =>
        lastPositionSequence match {
          case -1 =>
            val newCoord = calculateTargetCoordinates(
              movementRequest.positionToMove.coordinate,
              lastOffsetMovement,
              movementRequest.boardState.boardSize)
            newCoord match {
              case Some(c) =>
                lastMoveSequence(
                  Position(newCoord.get,
                    movementRequest.positionToMove.playerName)) = 0
                lastMoveSequence.remove(movementRequest.positionToMove)
                lastOffsetMovement
              case None =>
                lastMoveSequence(movementRequest.positionToMove) = 0
                lastOffsetMovement = DOWN_RIGHT
                lastOffsetMovement
            }
          case 0 =>
            val newCoord = calculateTargetCoordinates(
              movementRequest.positionToMove.coordinate,
              UP,
              movementRequest.boardState.boardSize)
            newCoord match {
              case Some(c) =>
                lastMoveSequence(
                  Position(newCoord.get,
                    movementRequest.positionToMove.playerName)) = 1
                lastMoveSequence.remove(movementRequest.positionToMove)
                UP
              case None =>
                lastMoveSequence(movementRequest.positionToMove) = 1
                STAY
            }
          case 1 =>
            val newCoord = calculateTargetCoordinates(
              movementRequest.positionToMove.coordinate,
              RIGHT,
              movementRequest.boardState.boardSize)
            newCoord match {
              case Some(c) =>
                lastMoveSequence(
                  Position(newCoord.get,
                    movementRequest.positionToMove.playerName)) = 2
                lastMoveSequence.remove(movementRequest.positionToMove)
                RIGHT
              case None =>
                lastMoveSequence(movementRequest.positionToMove) = 2
                STAY
            }
          case 2 =>
            val newCoord = calculateTargetCoordinates(
              movementRequest.positionToMove.coordinate,
              DOWN,
              movementRequest.boardState.boardSize)
            newCoord match {
              case Some(c) =>
                lastMoveSequence(
                  Position(newCoord.get,
                    movementRequest.positionToMove.playerName)) = 3
                lastMoveSequence.remove(movementRequest.positionToMove)
                DOWN
              case None =>
                lastMoveSequence(movementRequest.positionToMove) = 3
                STAY
            }
          case 3 =>
            val newCoord = calculateTargetCoordinates(
              movementRequest.positionToMove.coordinate,
              DOWN,
              movementRequest.boardState.boardSize)
            newCoord match {
              case Some(c) =>
                lastMoveSequence(
                  Position(newCoord.get,
                    movementRequest.positionToMove.playerName)) = 4
                lastMoveSequence.remove(movementRequest.positionToMove)
                DOWN
              case None =>
                lastMoveSequence(movementRequest.positionToMove) = 4
                STAY
            }
          case 4 =>
            val newCoord = calculateTargetCoordinates(
              movementRequest.positionToMove.coordinate,
              LEFT,
              movementRequest.boardState.boardSize)
            newCoord match {
              case Some(c) =>
                lastMoveSequence(
                  Position(newCoord.get,
                    movementRequest.positionToMove.playerName)) = 5
                lastMoveSequence.remove(movementRequest.positionToMove)
                LEFT
              case None =>
                lastMoveSequence(movementRequest.positionToMove) = 5
                STAY
            }
          case 5 =>
            val newCoord = calculateTargetCoordinates(
              movementRequest.positionToMove.coordinate,
              LEFT,
              movementRequest.boardState.boardSize)
            newCoord match {
              case Some(c) =>
                lastMoveSequence(
                  Position(newCoord.get,
                    movementRequest.positionToMove.playerName)) = 6
                lastMoveSequence.remove(movementRequest.positionToMove)
                LEFT
              case None =>
                lastMoveSequence(movementRequest.positionToMove) = 6
                STAY
            }
          case 6 =>
            val newCoord = calculateTargetCoordinates(
              movementRequest.positionToMove.coordinate,
              UP,
              movementRequest.boardState.boardSize)
            newCoord match {
              case Some(c) =>
                lastMoveSequence(
                  Position(newCoord.get,
                    movementRequest.positionToMove.playerName)) = 7
                lastMoveSequence.remove(movementRequest.positionToMove)
                UP
              case None =>
                lastMoveSequence(movementRequest.positionToMove) = 7
                STAY
            }
          case 7 =>
            val newCoord = calculateTargetCoordinates(
              movementRequest.positionToMove.coordinate,
              UP,
              movementRequest.boardState.boardSize)
            newCoord match {
              case Some(c) =>
                lastMoveSequence(
                  Position(newCoord.get,
                    movementRequest.positionToMove.playerName)) = 8
                lastMoveSequence.remove(movementRequest.positionToMove)
                UP
              case None =>
                lastMoveSequence(movementRequest.positionToMove) = 8
                STAY
            }
          case 8 =>
            val newCoord = calculateTargetCoordinates(
              movementRequest.positionToMove.coordinate,
              RIGHT,
              movementRequest.boardState.boardSize)
            newCoord match {
              case Some(c) =>
                lastMoveSequence(
                  Position(newCoord.get,
                    movementRequest.positionToMove.playerName)) = 1
                lastMoveSequence.remove(movementRequest.positionToMove)
                RIGHT
              case None =>
                lastMoveSequence(movementRequest.positionToMove) = 1
                STAY
            }
        }
      case None =>
        val newCoord = calculateTargetCoordinates(
          movementRequest.positionToMove.coordinate,
          UP,
          movementRequest.boardState.boardSize)
        newCoord match {
          case Some(c) =>
            lastMoveSequence(
              Position(newCoord.get,
                movementRequest.positionToMove.playerName)) = 0
            UP
          case None =>
            lastMoveSequence(movementRequest.positionToMove) = 0
            STAY
        }
    }

  }

  private val ALL_MOVEMENTS =
    Seq(STAY, UP, UP_LEFT, UP_RIGHT, RIGHT, LEFT, DOWN, DOWN_LEFT, DOWN_RIGHT)

  private def calculateTargetCoordinates(coordinate: Coordinate,
                                         movement: Movement,
                                         boardSize: Int): Option[Coordinate] = {
    val newCoordinates = movement match {
      case UP =>
        coordinate.copy(y = coordinate.y + 1)
      case UP_LEFT =>
        coordinate.copy(x = coordinate.x - 1, y = coordinate.y + 1)
      case UP_RIGHT =>
        coordinate.copy(x = coordinate.x + 1, y = coordinate.y + 1)
      case LEFT =>
        coordinate.copy(x = coordinate.x - 1)
      case RIGHT =>
        coordinate.copy(x = coordinate.x + 1)
      case DOWN =>
        coordinate.copy(y = coordinate.y - 1)
      case DOWN_LEFT =>
        coordinate.copy(x = coordinate.x - 1, y = coordinate.y - 1)
      case DOWN_RIGHT =>
        coordinate.copy(x = coordinate.x + 1, y = coordinate.y - 1)
      case STAY =>
        coordinate

    }

    newCoordinates match {
      case Coordinate(x, y) if x < 0 || y < 0                   => None
      case Coordinate(x, y) if x >= boardSize || y >= boardSize => None
      case validCoordinates                                     => Some(validCoordinates)
    }
  }

}
