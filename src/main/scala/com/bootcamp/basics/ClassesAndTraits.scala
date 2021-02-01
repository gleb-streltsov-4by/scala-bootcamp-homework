package com.bootcamp.basics

object ClassesAndTraits {

  // Common

  sealed trait Shape extends Located with Bounded with Movable

  sealed trait Movable {
    def move(dx: Double, dy: Double): Movable
  }

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  // 2D Shapes

  sealed trait Area {
    def area: Double
  }

  sealed trait Shape2D extends Shape with Area

  final case class Square(x: Double, y: Double, side: Double) extends Shape2D {

    override def area: Double = side * side
    override def move(dx: Double, dy: Double): Square = Square(x + dx, y + dy, side)

    override def minX: Double = x - side / 2
    override def maxX: Double = x + side / 2
    override def minY: Double = y - side / 2
    override def maxY: Double = y + side / 2
  }

  final case class Triangle2D(x1: Double, y1: Double,
                              x2: Double, y2: Double,
                              x3: Double, y3: Double) extends Shape2D {

    override def area: Double = {
      val height = maxY - minY
      val base = maxX - minX

      1/2 * base * height
    }

    override def move(dx: Double, dy: Double): Triangle2D =
      Triangle2D(
        x1 + dx, y1 + dy,
        x2 + dx, y2 + dy,
        x3 + dx, y3 + dy)

    override def x: Double = (x1 + x2 + x3) / 3
    override def y: Double = (y1 + y2 + y3) / 3

    override def minX: Double = x1 min x2 min x3
    override def maxX: Double = x1 max x2 max x3
    override def minY: Double = y1 min y2 min y3
    override def maxY: Double = y1 max y2 max y3
  }

  // 3D Shapes

  sealed trait SurfaceArea {
    def surfaceArea: Double
  }

  sealed trait Volume {
    def volume: Double
  }

  sealed trait Shape3D extends Shape with SurfaceArea with Volume {
    def z: Double
  }

  final case class Point(x: Double, y: Double, z: Double) extends Shape3D {
  }

  final case class Sphere() extends Shape3D {
  }

  final case class Cube() extends Shape3D {
  }

  final case class Cuboid() extends Shape3D {
  }

  final case class Triangle3D() extends Shape3D {
  }

}
