package gui

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.canvas.Canvas
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import javafx.stage.Stage

import logic.Renderer.{Box, Rectangle}
import logic.{Renderer, Types}

/**
  * Created by federico on 16/08/17.
  */

class MainPane(val width:Int, val height:Int) extends Pane {
  //General scaling
  val unitX = 20
  val unitY = 20
  //Used to separate things
  val smallX = 4
  val smallY = 4
  val canvas = new Canvas(width,height)
  this.getChildren.add(canvas)

  def draw(primitives:Iterable[Renderer.PrimitiveRenderer]) = {
    val gc = this.canvas.getGraphicsContext2D
    gc.clearRect(0, 0, width, height)
    primitives.foreach(this.drawPrimitive)
  }

  private def drawPrimitive(primitive:Renderer.PrimitiveRenderer) = {
    val gc = this.canvas.getGraphicsContext2D
    primitive match {
      case Rectangle(x, y, w, h) =>
        gc.setFill(Color.GREEN)
        gc.fillRect(x*unitX + smallX*2, y*unitY + smallY*2, w*unitX - 4*smallX, h*unitY - 4*smallY)
      case Box(x, y, w, h) =>
        gc.setStroke(Color.RED)
        gc.strokeRect(x*unitX + smallX, y*unitY + smallY, w*unitX - 2*smallX, h*unitY - 2*smallY)
    }
  }
}

class Main extends Application {
  override def start(stage: Stage): Unit = {
    val mainPane = new MainPane(800, 600)
    stage.setScene(new Scene(mainPane, mainPane.width, mainPane.height))
    stage.setTitle("lift-paternoster")
    stage.show()

    //val initialType = Types.Array(Types.Array(Types.Float(), 5), 5)
    val initialType = Types.Array(Types.Array(Types.Array(Types.Float(),2), 5), 5)
    //val initialType = Types.Array(Types.Array(Types.Array(Types.Array(Types.Array(Types.Float(),2),5),5), 7), 7)

    val typeRenderer = Renderer.typeRenderer(initialType)
    val primitives = Renderer.makePrimitives(typeRenderer)

    println(initialType)
    println(typeRenderer)
    println(primitives)

    mainPane.draw(primitives)
  }
}

object Main {
  def main(args:Array[String]) = {
    Application.launch(classOf[Main], args:_*)
  }
}
