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

  type PositionSequence = Int

  val lastMoveSequence: mutable.LinkedHashMap[Position, PositionSequence] =
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

  private def getRandomMovement(movementRequest: MovementRequest): Movement = {
    movementTowardsEnemy(movementRequest.positionToMove,
                         movementRequest.boardState) match {
      case Some(moveToEnemy) => moveToEnemy
      case None =>
        getGenericMovement(movementRequest.positionToMove,
                           movementRequest.boardState)
    }
  }

  private def movementTowardsEnemy(positionToMove: Position,
                                   boardState: BoardState): Option[Movement] = {
    ALL_MOVEMENTS.find { move =>
      isEnemyOccupiedCoordinate(move,
                                positionToMove.coordinate,
                                boardState,
                                positionToMove.playerName)
    }
  }

  private def isEnemyOccupiedCoordinate(move: Movement,
                                        coordinate: Coordinate,
                                        boardState: BoardState,
                                        playerName: String): Boolean = {
    val potentialEnemyCoordinate =
      calculateTargetCoordinates(coordinate, move, boardState.boardSize)
    potentialEnemyCoordinate match {
      case Some(coord) =>
        boardState.positions
          .exists(p => p.playerName != playerName && p.coordinate == coord)
      case None =>
        false
    }
  }

  private def newFriendlyUnoccupiedCoordinate(
      move: Movement,
      coordinate: Coordinate,
      boardState: BoardState,
      playerName: String): Option[Coordinate] = {
    for {
      newCoord <- calculateTargetCoordinates(coordinate, move, boardState.boardSize)
      if !boardState.positions
        .exists(p => p.playerName == playerName && p.coordinate == newCoord)
    } yield newCoord
  }

  private def move(
      lastMoveSeq: mutable.LinkedHashMap[Position, PositionSequence],
      positionToMove: Position,
      boardState: BoardState,
      sequence: PositionSequence,
      movement: Movement) = {
    val newCoord = newFriendlyUnoccupiedCoordinate(movement,
                                                positionToMove.coordinate,
                                                boardState,
                                                positionToMove.playerName)

    newCoord match {
      case Some(c) =>
        lastMoveSeq(Position(newCoord.get, positionToMove.playerName)) =
          sequence
        lastMoveSeq.remove(positionToMove)
        movement
      case None =>
        lastMoveSequence(positionToMove) = sequence
        STAY
    }
  }

  private def getGenericMovement(positionToMove: Position,
                                 boardState: BoardState) = {
    lastMoveSequence.get(positionToMove) match {
      case Some(lastPositionSequence) =>
        lastPositionSequence match {
          case 0 =>
            move(lastMoveSequence, positionToMove, boardState, 1, UP)
          case 1 =>
            move(lastMoveSequence, positionToMove, boardState, 2, RIGHT)
          case 2 =>
            move(lastMoveSequence, positionToMove, boardState, 3, DOWN)
          case 3 =>
            move(lastMoveSequence, positionToMove, boardState, 4, DOWN)
          case 4 =>
            move(lastMoveSequence, positionToMove, boardState, 5, LEFT)
          case 5 =>
            move(lastMoveSequence, positionToMove, boardState, 6, LEFT)
          case 6 =>
            move(lastMoveSequence, positionToMove, boardState, 7, UP)
          case 7 =>
            move(lastMoveSequence, positionToMove, boardState, 8, UP)
          case 8 =>
            move(lastMoveSequence, positionToMove, boardState, 1, RIGHT)
        }
      case None =>
        move(lastMoveSequence, positionToMove, boardState, 1, UP)
    }
  }

  private val ALL_MOVEMENTS =
    Seq(UP, UP_LEFT, UP_RIGHT, RIGHT, LEFT, DOWN, DOWN_LEFT, DOWN_RIGHT)

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

  private def calculateMovementToTargetCoordinates(coordinate: Coordinate,
                                                   boardSize: Int): Movement = {
    UP
  }

}
