package com.gw.pendulum

import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.{CanvasRenderingContext2D, Event}

case class Point(x: Int, y: Int)

object Main {

  var angle = 0.0
  var x = 0
  var direction = 1

  var width = 0.0
  var height = 0.0
  var scaleFactor = 1.0

  def angler = {
    angle = angle + Math.PI / 45
    x = x + 3 * direction
    if (x < -300 || Math.abs(x) > 300) direction = direction * -1
  }

  def scale(e: Event) = {
    width = Math.max(400, dom.window.innerWidth)
    height = Math.max(300, dom.window.innerHeight)
    scaleFactor = height / 1500
  }

  def draw(rail: HTMLImageElement, mount: HTMLImageElement, wheel: HTMLImageElement, pole: HTMLImageElement)(d: Double): Unit = {

    val canvas = dom.document.getElementById("canvas").asInstanceOf[Canvas]
    implicit val context: CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    canvas.width = width.toInt
    canvas.height = height.toInt
    val center = Point(canvas.width / 2, canvas.height / 2)

    context.fillStyle = "black"
    context.font = "14pt sans-serif"

    context.fillText(s" width: $width", 20, 60)
    context.fillText(s"height: $height", 20, 80)
    context.fillText(s"scaleFactor: $scaleFactor", 20, 100)

    context.translate(center.x, center.y)
    context.scale(scaleFactor, scaleFactor)

    drawAssembly(rail, mount, wheel, pole)(x, angle)

    dom.window.requestAnimationFrame(draw(rail, mount, wheel, pole))
  }

  def drawAssembly(rail: HTMLImageElement, mount: HTMLImageElement, wheel: HTMLImageElement, pole: HTMLImageElement)
                  (x: Double, theta: Double)
                  (implicit context: CanvasRenderingContext2D) = {
    context.drawImage(rail, -rail.width / 2, -rail.height)
    context.save()
    context.translate(x, 0.0)
    context.drawImage(mount, -mount.width / 2, -mount.height - rail.height / 2)
    context.save()
    context.translate(0, -wheel.width / 2)
    context.rotate(x / wheel.width)
    context.drawImage(wheel, -wheel.width / 2, -wheel.height / 2)
    context.restore()
    context.translate(0, -100 - rail.height / 2)
    context.rotate(theta)
    context.drawImage(pole, -pole.width / 2, pole.height * -1 + 20)
    context.restore()
  }

  def loadImage(fileName: String) = {
    val image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    image.src = fileName
    image
  }

  def main(args: Array[String]): Unit = {

    val rail: HTMLImageElement = loadImage("rail.svg")
    val mount: HTMLImageElement = loadImage("mount.svg")
    val wheel: HTMLImageElement = loadImage("wheel.svg")
    val pole: HTMLImageElement = loadImage("pole.svg")

    dom.window.setInterval(() => angler, 10)
    dom.window.onload = (e: Event) => scale(e)
    dom.window.onresize = (e: Event) => scale(e)
    dom.window.requestAnimationFrame(draw(rail, mount, wheel, pole))
  }
}
