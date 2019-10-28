package scanimation.processing

import scanimation.common.{Rec2d, Vec2d}

object packer {
  /** Packs given rectangles into a single area, returning the total area size and the list of areas occupied by original rectangles */
  def pack(list: List[Vec2d], padding: Vec2d): (Vec2d, List[Rec2d]) = {
    val (total, placements) = list
      .zipWithIndex
      .map { case (v, i) => (v + padding * 2, i) }
      .sortBy { case (v, i) => -(v.x * v.y) }
      .foldLeft[(Rec2d, List[(Rec2d, Int)])](Rec2d.Zero, Nil) { case ((totalArea, placed), (current, index)) =>
      val optimalRect = placed
        // find possible placements neat previously placed rectangles
        .flatMap { case (area, i) => area.pointAt(Vec2d.TopRight) :: area.pointAt(Vec2d.BottomLeft) :: Nil }
        // remove places which intersect with previous rectangles
        .filter { place => placed.forall { case (p, i) => !p.intersects(Rec2d(place, current)) } }
        // map to rectangle
        .map { place => Rec2d(place, current) }
        // find optimal place - place which extends the max total side the least
        .minByOpt { area => (Rec2d.include(totalArea, area).size - totalArea.size).max }
        // no places means the rectangle is the first one, so it goes at 0:0
        .getOrElse(Rec2d(Vec2d.Zero, current))
      Rec2d.include(totalArea, optimalRect) -> (placed :+ (optimalRect, index))
    }
    total.size -> placements
      .sortBy { case (p, i) => i }
      .map { case (p, i) => p.offsetBy(padding).resizeTo(p.size - padding * 2) }
  }
}