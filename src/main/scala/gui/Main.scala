package gui

import javafx.application.Application
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import javafx.stage.Stage

import logic.{Graphics, LiftAST, Scene}
import logic.Graphics.{Arrow, Box, Rectangle}

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

  def draw(primitives:Iterable[Graphics.GraphicalPrimitive]) = {
    val gc = this.canvas.getGraphicsContext2D
    gc.clearRect(0, 0, width, height)
    primitives.foreach(this.drawPrimitive)
  }

  private def drawPrimitive(primitive:Graphics.GraphicalPrimitive) = {
    val gc = this.canvas.getGraphicsContext2D
    primitive match {
      case Rectangle(x, y, w, h) =>
        gc.setFill(Color.GREEN)
        gc.fillRect(x*unitX + smallX*2, y*unitY + smallY*2, w*unitX - 4*smallX, h*unitY - 4*smallY)
      case Box(x, y, w, h) =>
        gc.setStroke(Color.RED)
        gc.strokeRect(x*unitX + smallX, y*unitY + smallY, w*unitX - 2*smallX, h*unitY - 2*smallY)
      case Arrow(startX, startY, endX, endY) =>
        gc.setStroke(Color.BLACK)
        drawArrow(gc, startX*unitX, startY*unitY, endX*unitX, endY*unitY)
    }
  }

  private def drawArrow(gc:GraphicsContext, node1X:Double, node1Y:Double, node2X:Double, node2Y:Double) {
    val arrowAngle = Math.toRadians(45.0)
    val arrowLength = 10.0
    val dx = node1X - node2X
    val dy = node1Y - node2Y
    val angle = Math.atan2(dy, dx)
    val x1 = Math.cos(angle + arrowAngle) * arrowLength + node2X
    val y1 = Math.sin(angle + arrowAngle) * arrowLength + node2Y

    val x2 = Math.cos(angle - arrowAngle) * arrowLength + node2X
    val y2 = Math.sin(angle - arrowAngle) * arrowLength + node2Y
    gc.strokeLine(node1X, node1Y, node2X, node2Y)
    gc.strokeLine(node2X, node2Y, x1, y1)
    gc.strokeLine(node2X, node2Y, x2, y2)
  }
}

class Main extends Application {
  override def start(stage: Stage): Unit = {
    val mainPane = new MainPane(800, 600)
    stage.setScene(new javafx.scene.Scene(mainPane, mainPane.width, mainPane.height))
    stage.setTitle("lift-paternoster")
    stage.show()

    import LiftAST._
    val operation = LiftAST.Map(Float, Array(Float, 2), 10)

    mainPane.draw(Scene.drawOperation(Scene.operationNode(operation)))
  }
}

object Main {
  def main(args:Array[String]) = {
    Application.launch(classOf[Main], args:_*)
  }
}
