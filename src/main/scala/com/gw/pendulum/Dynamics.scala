package com.gw.pendulum

import java.lang.Math._

case class State(x: Double, v: Double, theta: Double, omega: Double) {
  def +(dot: StateDot) = State(x + dot.v, v + dot.a, theta + dot.omega, omega + dot.alpha)

  def *(d: Double) = State(x * d, v * d, theta * d, omega * d)
}

case class StateDot(v: Double, a: Double, omega: Double, alpha: Double) {
  def +(that: StateDot) = StateDot(v + that.v, a + that.a, omega + that.omega, alpha + that.alpha)

  def *(d: Double) = StateDot(v * d, a * d, omega * d, alpha * d)
}

case class Dynamics(g: Double, lp: Double, mp: Double, mc: Double, fp: Double, fc: Double) {
  val mt = mp + mc

  def solve(s: State, u: Double): StateDot = {

    // linear velocity of pendulum and cart on x
    val v = s.v

    // angular velocity of pendulum about pin
    val omega = s.omega

    // linear acceleration on x
    //
    //     mp * lp * sin(theta) * w^2 + mp * g * sin(theta)cos(theta) - dv + (b/l)w * cos(theta) + u
    // a = ------------------------------------------------------------------------
    //                              M + m * sin^2(theta)
    //
    val a = (mp * lp * sin(s.theta) * s.omega * s.omega
      + mp * g * sin(s.theta) * cos(s.theta)
      + (fp / lp) * s.omega * cos(s.theta)
      - (fc * s.v)
      + u
      ) / (mc + (mp * sin(s.theta) * sin(s.theta)))

    // angular acceleration of pendulum about pin
    //
    //         mp * lp * sin(theta)cos(theta) * w^2 - u * cos(theta) + dv * cos(theta) - (1 + M/m)(b/l)w - (M + m) * gsin(theta)
    // alpha = ------------------------------------------------------------------------------------------------
    //                                          l[M + msin(theta)]
    //
    val alpha = (mp * lp * sin(s.theta) * cos(s.theta) * (s.omega * s.omega)
      - u * cos(s.theta)
      + fc * s.v * cos(s.theta)
      - (1 + mc / mp) * (fp / lp) * s.omega
      - mt * g * sin(s.theta)
      ) / (lp * (mc + (mp * sin(s.theta) * sin(s.theta))))

    StateDot(v, a, omega, alpha)
  }
}
