import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.util.Duration

abstract class Creature(val energy: Int, val breedTime: Int, val color: Color) {
  def copy(energy: Int): Creature
}

case class Tuna(override val energy: Int, tBreed: Int) extends Creature(energy, tBreed, Color.Blue) {
  def copy(energy: Int): Creature = Tuna(energy, tBreed)
}

case class Shark(override val energy: Int, sBreed: Int, var starvationTime: Int) extends Creature(energy, sBreed, Color.Red) {
  def copy(energy: Int): Creature = Shark(energy, sBreed, starvationTime)
}

object Main extends JFXApp3 {
  val nTunas = 100
  val nSharks = 50
  val sEnergy = 15
  val sBreed = 20
  val tBreed = 15
  var starvationTime = 25

  val grid = Array.fill[Option[Creature]](50, 50)(None)
  var rects = Array.ofDim[Rectangle](50, 50)

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title.value = "WaTor"
      scene = new Scene(500, 500) {
        content = for (i <- 0 until 50; j <- 0 until 50) yield {
          val r = new Rectangle {
            x = j * 10
            y = i * 10
            width = 10
            height = 10
            fill = Color.White
          }
          rects(i)(j) = r
          r
        }
      }
    }

    // Initialize the world with random tunas and sharks
    for (_ <- 1 to nTunas) {
      var placed = false
      while (!placed) {
        val i = scala.util.Random.nextInt(50)
        val j = scala.util.Random.nextInt(50)
        if (grid(i)(j).isEmpty) {
          grid(i)(j) = Some(Tuna(0, tBreed))
          rects(i)(j).fill = Color.Blue
          placed = true
        }
      }
    }
    for (_ <- 1 to nSharks) {
      var placed = false
      while (!placed) {
        val i = scala.util.Random.nextInt(50)
        val j = scala.util.Random.nextInt(50)
        if (grid(i)(j).isEmpty) {
          grid(i)(j) = Some(Shark(sEnergy, sBreed, starvationTime))
          rects(i)(j).fill = Color.Red
          placed = true
        }
      }
    }

    val timeline = new Timeline {
      cycleCount = Timeline.Indefinite
      keyFrames = KeyFrame(Duration(500), onFinished = _ => updateWorld())
    }

    timeline.play()
  }

  def updateWorld(): Unit = {
    val oldGrid = grid.map(_.clone())

    for (i <- 0 until 50; j <- 0 until 50) {
      oldGrid(i)(j) match {
        case Some(tuna: Tuna) =>
          val possibleMoves = List((i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j + 1), (i - 1, j - 1), (i + 1, j + 1), (i + 1, j - 1))
            .filter { case (x, y) => x >= 0 && x < 50 && y >= 0 && y < 50 && grid(x)(y).isEmpty }

          if (possibleMoves.nonEmpty) {
            val (newI, newJ) = possibleMoves(scala.util.Random.nextInt(possibleMoves.length))
            grid(newI)(newJ) = Some(tuna.copy(energy = tuna.energy + 1))
            rects(newI)(newJ).fill = Color.Blue

            if (tuna.energy >= tuna.tBreed) {
              grid(i)(j) = Some(tuna.copy(energy = 0))
              rects(i)(j).fill = Color.Blue
            } else {
              grid(i)(j) = None
              rects(i)(j).fill = Color.White
            }
          }
        case Some(shark: Shark) =>
          val possibleMoves = List((i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j + 1), (i - 1, j - 1), (i + 1, j + 1), (i + 1, j - 1))
            .filter { case (x, y) => x >= 0 && x < 50 && y >= 0 && y < 50 }
          val tunaMoves = possibleMoves.filter { case (x, y) => grid(x)(y).exists(_.isInstanceOf[Tuna]) }
          val emptyMoves = possibleMoves.filter { case (x, y) => grid(x)(y).isEmpty }

          if (tunaMoves.nonEmpty) {
            val (newI, newJ) = tunaMoves(scala.util.Random.nextInt(tunaMoves.length))
            grid(newI)(newJ) = Some(shark.copy(energy = shark.energy + 1, starvationTime = starvationTime))
            rects(newI)(newJ).fill = Color.Red

            // If the shark eats a tuna, it reproduces immediately
            grid(i)(j) = Some(shark.copy(energy = 0))
            rects(i)(j).fill = Color.Red
          } else if (emptyMoves.nonEmpty) {
            val (newI, newJ) = emptyMoves(scala.util.Random.nextInt(emptyMoves.length))
            if (shark.starvationTime <= 0) {
              grid(i)(j) = None
              rects(i)(j).fill = Color.White
            } else {
              grid(newI)(newJ) = Some(shark.copy(starvationTime = shark.starvationTime - 1))
              rects(newI)(newJ).fill = Color.Red
              grid(i)(j) = None
              rects(i)(j).fill = Color.White
            }
          } else {
            grid(i)(j) = None
            rects(i)(j).fill = Color.White
          }
        case _ =>
      }
    }
  }
}

object WatorApp extends App {
  Main.main(Array())
}
