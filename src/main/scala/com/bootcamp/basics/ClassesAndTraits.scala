package com.bootcamp.basics

object ClassesAndTraits {

  // Common

  sealed trait Shape extends Located with Bounded

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

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Movable2D
  }

  sealed trait Shape2D extends Shape with Area with Movable2D

  final case class Square(centerX: Double, centerY: Double, side: Double) extends Shape2D {

    override def x: Double = centerX
    override def y: Double = centerY

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

      base * height / 2
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

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Movable3D
  }

  sealed trait Located3D extends Located {
    def z: Double
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Shape3D extends Located3D
    with Bounded3D
    with Movable3D
    with SurfaceArea
    with Volume {
  }

  object Origin extends Located3D {

    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

  final case class Point(x: Double, y: Double, z: Double) extends Located3D with Movable3D {

    override def move(dx: Double, dy: Double, dz: Double): Point =
      Point(x + dx, y + dy, z + dz)
  }

  final case class Sphere(centerX: Double,
                          centerY: Double,
                          centerZ: Double,
                          radius: Double) extends Shape3D {

    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ

    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius

    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)
    override def volume: Double = (4 * Math.PI * Math.pow(radius, 3)) / 3

    override def move(dx: Double, dy: Double, dz: Double): Sphere =
      Sphere(x + dx, y + dy, z + dz, radius)
  }

  final case class Cube(centerX: Double,
                        centerY: Double,
                        centerZ: Double,
                        side: Double) extends Shape3D {

    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ

    override def minX: Double = x - side / 2
    override def maxX: Double = x + side / 2
    override def minY: Double = y - side / 2
    override def maxY: Double = y + side / 2
    override def minZ: Double = z - side / 2
    override def maxZ: Double = z + side / 2

    override def surfaceArea: Double = 6 * Math.pow(side, 2)
    override def volume: Double = Math.pow(side, 3)

    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(x + dx, y + dy, z + dz, side)
  }

  final case class Cuboid(centerX: Double,
                          centerY: Double,
                          centerZ: Double,
                          length: Double,
                          width: Double,
                          height: Double) extends Shape3D {

    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ

    override def minX: Double = x - length / 2
    override def maxX: Double = x + length / 2
    override def minY: Double = y - width / 2
    override def maxY: Double = y + width / 2
    override def minZ: Double = z - height / 2
    override def maxZ: Double = z + height / 2

    override def surfaceArea: Double = 2 * (length * width + length * height + width * height)
    override def volume: Double = length * width * height

    override def move(dx: Double, dy: Double, dz: Double): Cuboid =
      Cuboid(x + dx, y + dy, z + dz, length, width, height)
  }

  final case class Triangle3D(x1: Double, y1: Double, z1: Double,
                              x2: Double, y2: Double, z2: Double,
                              x3: Double, y3: Double, z3: Double)
    extends Located3D
      with Movable3D
      with Bounded3D
      with Area {

    override def x: Double = (x1 + x2 + x3) / 3
    override def y: Double = (y1 + y2 + y3) / 3
    override def z: Double = (z1 + z2 + z3) / 3


    override def minX: Double = x1 min x2 min x3
    override def maxX: Double = x1 max x2 max x3
    override def minY: Double = y1 min y2 min y3
    override def maxY: Double = y1 max y2 max y3
    override def minZ: Double = z1 min z2 min z3
    override def maxZ: Double = z1 max z2 max z3

    override def area: Double = {
      val x12 = x2 - x1
      val x13 = x3 - x1

      val y12 = y2 - y1
      val z13 = z3 - z1

      val z12 = z2 - z1
      val y13 = y3 - y1


      Math.sqrt(
        Math.pow(y12 * z13 - z12 * y13, 2) +
        Math.pow(z12 * x13 - x12 * z13, 2) +
        Math.pow(x12 * y13 - y12 * x13, 2) / 2
      )
    }

    override def move(dx: Double, dy: Double, dz: Double): Triangle3D =
      Triangle3D(
        x1 + dx, y1 + dy, z1 + dz,
        x2 + dx, y2 + dy, z2 + dz,
        x3 + dx, y3 + dy, z3 + dz)
  }

}
