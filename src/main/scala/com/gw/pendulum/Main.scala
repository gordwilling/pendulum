package com.gw.pendulum

import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.{CanvasRenderingContext2D, Event, MouseEvent, WheelEvent}

case class Point(x: Int, y: Int)

object Main {

  var dynamics: Dynamics = _
  var state = State(0.0, 0.0, -Math.PI / 1800.0, 0.0)
  var priorMillis: Long = System.currentTimeMillis()

  var width = 0.0
  var height = 0.0
  var scaleFactor = 1.0
  var pixelsPerMeter = 1e-9
  var metersPerPixel = 1e-9
  var button: Int = -1

  def rungeKutta(h: Double)(d: Dynamics, s: State, u: Double): State = {
    val k1 = d.solve(s, u)
    val k2 = d.solve(s + (k1 * (0.5 * h)), u)
    val k3 = d.solve(s + (k2 * (0.5 * h)), u)
    val k4 = d.solve(s + (k3 * h), u)

    s + ((k1 + (k2 * 2.0) + (k3 * 2.0) + k4) * (h / 6.0))
  }

  def updateState(): Unit = {
    val elapsedSeconds = (System.currentTimeMillis() - priorMillis) / 1000.0
    this.state = rungeKutta(elapsedSeconds)(dynamics, state, 0.0)
    priorMillis = System.currentTimeMillis()
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

    context.fillText(s"     x: ${state.x}", 20, 40)
    context.fillText(s"     v: ${state.v}", 20, 60)
    context.fillText(s"button: $button", 20, 80)
    context.fillText(s" theta: ${state.theta}", 20, 120)
    context.fillText(s" omega: ${state.omega}", 20, 140)

    context.translate(center.x, center.y)
    context.scale(scaleFactor, scaleFactor)

    updateState()
    drawAssembly(rail, mount, wheel, pole)(state.x, state.theta)

    dom.window.requestAnimationFrame(draw(rail, mount, wheel, pole))
  }

  def drawAssembly(rail: HTMLImageElement, mount: HTMLImageElement, wheel: HTMLImageElement, pole: HTMLImageElement)
                  (x: Double, theta: Double)
                  (implicit context: CanvasRenderingContext2D) = {

    val xInPixels = pixelsPerMeter * x
    context.save()
    context.drawImage(rail, -rail.width / 2, -rail.height)
    context.translate(xInPixels, 0.0)
    context.save()
    context.drawImage(mount, -mount.width / 2, -mount.height - rail.height / 2)
    context.translate(0, -wheel.width / 2)
    context.rotate(xInPixels / wheel.width)
    context.drawImage(wheel, -wheel.width / 2, -wheel.height / 2)
    context.restore()
    context.translate(0, -mount.height - rail.height / 2)
    context.rotate(theta)
    context.drawImage(pole, -pole.width / 2, pole.height * -1 + mount.height / 4)
    context.restore()
  }

  def loadImage(fileName: String) = {
    val image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    image.src = fileName
    image
  }

  def main(args: Array[String]): Unit = {

    priorMillis = System.currentTimeMillis()

    val rail: HTMLImageElement = loadImage("rail.svg")
    val mount: HTMLImageElement = loadImage("mount.svg")
    val wheel: HTMLImageElement = loadImage("wheel.svg")
    val pole: HTMLImageElement = loadImage("pole.svg")

    dynamics = Dynamics(-9.81, 0.8, 1.0, 2.0, 0.06f, 0.5f)
    pixelsPerMeter = pole.height / dynamics.lp

    dom.window.onload = (e: Event) => scale(e)
    dom.window.onresize = (e: Event) => scale(e)
    dom.window.onmousedown = (e: MouseEvent) => {
      val adjustX = if (e.button == 0) -0.3 else 0.3
      val adjustTheta = Math.atan(adjustX / dynamics.lp)
      this.state = State(state.x + adjustX, state.v, state.theta + adjustTheta, state.omega)
      e.preventDefault()
      false
    }



    dom.window.requestAnimationFrame(draw(rail, mount, wheel, pole))
  }
}
