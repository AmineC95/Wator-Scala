import scalafx.application.{JFXApp3, Platform}
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle

import scala.concurrent.Future
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends JFXApp3 {

  private val WINDOW_SIZE = 600
  private val CELL_SIZE = 5
  private val NUMBER_OF_CELLS = WINDOW_SIZE / CELL_SIZE

  private type Coordinates = (Int, Int)
  private type Tuna = List[Coordinates]
  private type Shark = List[(Coordinates, Int)] // (Coordinates, Energy)

  private val nTunas: Int = 1000 // Nombre de thons
  private val nSharks: Int = 2 // Nombre de requins

  private val tBreed: Int = 15 // Nombre de cycles avant la reproduction des thons
  private val sBreed: Int = 10 // Nombre de cycles avant la reproduction des requins
  private val sEnergy: Int = 1 // Barre d'énergie initiale des requins

  private val initialTunas: Tuna = List.fill(nTunas)((Random.nextInt(NUMBER_OF_CELLS), Random.nextInt(NUMBER_OF_CELLS)))
  private val initialSharks: Shark = List.fill(nSharks)((Random.nextInt(NUMBER_OF_CELLS), Random.nextInt(NUMBER_OF_CELLS)) -> sEnergy)

  private val allCells: List[Rectangle] = drawTunas(initialTunas) ++ drawSharks(initialSharks)

  private def gameLoop(update: () => Unit): Unit =
    for {
      _ <- Future {
        update()
        Thread.sleep(10)
      }
      _ <- Future(gameLoop(update))
    } yield ()

  private def drawTunas(tunas: Tuna): List[Rectangle] =
    tunas.map { coordTuna =>
      new Rectangle {
        x = coordTuna._1 * CELL_SIZE
        y = coordTuna._2 * CELL_SIZE
        width = CELL_SIZE
        height = CELL_SIZE
        fill = Blue
      }
    }

  private def drawSharks(sharks: Shark): List[Rectangle] =
    sharks.map { case (coordShark, energy) =>
      new Rectangle {
        x = coordShark._1 * CELL_SIZE
        y = coordShark._2 * CELL_SIZE
        width = CELL_SIZE
        height = CELL_SIZE
        fill = Red
      }
    }

  private def isCellFree(x: Int, y: Int, tunas: Tuna, sharks: Shark): Boolean =
    !tunas.contains((x, y)) && !sharks.map(_._1).contains((x, y))

  private def moveTuna(tuna: Coordinates, tunas: Tuna, sharks: Shark): Coordinates = {
    val possibleMoves = List(
      (tuna._1 - 1, tuna._2), // Left
      (tuna._1 + 1, tuna._2), // Right
      (tuna._1, tuna._2 - 1), // Up
      (tuna._1, tuna._2 + 1)  // Down
    )

    val validMoves = possibleMoves.filter { case (x, y) =>
      x >= 0 && x < NUMBER_OF_CELLS && y >= 0 && y < NUMBER_OF_CELLS && isCellFree(x, y, tunas, sharks)
    }

    if (validMoves.nonEmpty) {
      val randomMove = Random.shuffle(validMoves).head
      (randomMove._1, randomMove._2)
    } else {
      tuna
    }
  }

  private def eatTuna(shark: (Coordinates, Int), tunas: Tuna, sharks: Shark): (Coordinates, Int) = {
    val possibleMoves = List(
      (shark._1._1 - 1, shark._1._2), // Left
      (shark._1._1 + 1, shark._1._2), // Right
      (shark._1._1, shark._1._2 - 1), // Up
      (shark._1._1, shark._1._2 + 1)  // Down
    )

    val tunaInNeighborhood = tunas.find { case (x, y) =>
      possibleMoves.contains((x, y))
    }

    tunaInNeighborhood match {
      case Some(tuna) => (tuna, shark._2 + 1)
      case None => moveTuna(shark._1, tunas, sharks) -> (shark._2 - 1)
    }
  }

  private def reproduceTunas(tunas: Tuna, cycle: Int): Tuna =
    if (cycle % tBreed == 0) tunas.flatMap(tuna => List(tuna, moveTuna(tuna, tunas, Nil)))
    else tunas

  private def reproduceSharks(sharks: Shark, cycle: Int, tunas: Tuna): Shark =
    if (cycle % sBreed == 0) sharks.flatMap(shark => List((shark._1, shark._2), eatTuna(shark, tunas, sharks)))
    else sharks

  override def start(): Unit = {
    val tunasState = ObjectProperty(initialTunas)
    val sharksState = ObjectProperty(initialSharks)
    val frame = IntegerProperty(0)

    frame.onChange {
      val updatedTunas = reproduceTunas(tunasState.value, frame.value)
      val updatedSharks = reproduceSharks(sharksState.value, frame.value, updatedTunas)
      val eatenTunas = updatedTunas.flatMap { tuna =>
        val (newShark, newSEnergy) = eatTuna((tuna, 0), updatedTunas, updatedSharks)
        if (newSEnergy <= 0) None else Some((newShark, newSEnergy))
      }

      tunasState.value = updatedTunas.diff(eatenTunas.map(_._1))
      sharksState.value = updatedSharks ++ eatenTunas
    }

    stage = new PrimaryStage {
      title = "Wator Simulation"
      width = WINDOW_SIZE
      height = WINDOW_SIZE
      scene = new Scene {
        fill = White
        content = allCells

        frame.onChange(Platform.runLater {
          val updatedCells = drawTunas(tunasState.value) ++ drawSharks(sharksState.value)
          content = updatedCells
        })
      }
    }

    gameLoop(() => frame.update(frame.value + 1))
  }
}
