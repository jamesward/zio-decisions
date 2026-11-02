package com.jamesward.zio_decisions.orchestration

/**
 * Context as an algebra: a sequence of titled sections, each with a priority and a floor, combined by concatenation
 * (a monoid, [[Sections.empty]] its identity) and fitted to a budget by [[Sections.fit]], a projection with these laws
 * (checked by `SectionsSpec`):
 *
 *   - bounded: `fit(b).size <= b` for every `b >= 0`;
 *   - identity under budget: `s.size <= b` implies `s.fit(b) == s`;
 *   - idempotent: `s.fit(b).fit(b) == s.fit(b)`;
 *   - monotone: for `b1 <= b2`, every section keeps at most as much under `b1` as under `b2` (and one dropped under
 *     `b2` is dropped under `b1`);
 *   - priority-ordered: sections are cut in [[Sections.order]] (lower priority first, then older first), each to its
 *     floor before the next is touched, and dropped, in the same order, only once every section is at its floor.
 *
 * A cut section keeps the head of its text and ends with [[Sections.Marker]], so its length is exactly what the fit
 * chose. Sizes are in characters, as rendered ([[Sections.render]]).
 */
final case class Sections(sections: Vector[Sections.Section]):
  import Sections.*

  def ++(other: Sections): Sections = Sections(sections ++ other.sections)
  def :+(section: Section): Sections = Sections(sections :+ section)
  def isEmpty: Boolean = sections.isEmpty

  /** The rendered size: each section's size, with a separator between sections. */
  def size: Int = if sections.isEmpty then 0 else sections.map(_.size).sum + Separator.length * (sections.size - 1)

  /** The sections, titled, separated by blank lines. */
  def render: String = sections.map(_.render).mkString(Separator)

  def renderOption: Option[String] = Option.when(sections.nonEmpty)(render)

  /** The sections fitted to `budget` characters, as the class describes. */
  def fit(budget: Int): Sections =
    if size <= budget then this
    else
      val order = Sections.order(sections)
      // phase 1: cut texts toward their floors, in order, until the excess is gone
      val (cutLengths, excess) = order.foldLeft((sections.map(_.text.length), size - budget.max(0))):
        case ((lengths, left), i) if left > 0 =>
          val s = sections(i)
          val reducible = (s.text.length - s.minText).max(0)
          val r = reducible.min(left)
          (lengths.updated(i, s.text.length - r), left - r)
        case (acc, _) => acc
      // phase 2: drop whole sections, in order, until it fits
      val (dropped, _) = order.foldLeft((Set.empty[Int], excess)):
        case ((gone, left), i) if left > 0 =>
          val freed = sections(i).sizeWith(cutLengths(i)) + (if sections.size - gone.size > 1 then Separator.length else 0)
          (gone + i, left - freed)
        case (acc, _) => acc
      Sections(sections.indices.filterNot(dropped).map(i => sections(i).cutTo(cutLengths(i))).toVector)

object Sections:
  /**
   * @param priority higher is kept longer
   * @param floor    the fewest characters of text kept before the section is dropped (`Int.MaxValue`: never cut, only
   *                 dropped)
   */
  final case class Section(title: String, text: String, priority: Int, floor: Int = 0):
    /** The least text a cut keeps: the floor, and room for the marker; a text that short isn't cut. */
    def minText: Int = text.length.min(floor.max(Marker.length + 1))
    def size: Int = sizeWith(text.length)
    def sizeWith(textLength: Int): Int = if title.isEmpty then textLength else title.length + 2 + textLength
    /** `title:` and the text on the next line; an untitled section is its text. */
    def render: String = if title.isEmpty then text else s"$title:\n$text"

    /** The text cut to exactly `length` characters (a head and [[Marker]]), or whole when it's no longer. */
    def cutTo(length: Int): Section =
      if length >= text.length then this
      else copy(text = text.take(length - Marker.length) + Marker)

  val Marker: String = "\n… (cut)"
  val Separator: String = "\n\n"
  val empty: Sections = Sections(Vector.empty)

  def apply(sections: Section*): Sections = Sections(sections.toVector)

  /** One section, or none for an empty text. */
  def of(title: String, text: String, priority: Int, floor: Int = 0): Sections =
    if text.trim.isEmpty then empty else Sections(Section(title, text, priority, floor))

  /** The order in which sections are cut and dropped: lower priority first, then earlier first. */
  def order(sections: Vector[Section]): Vector[Int] = sections.indices.toVector.sortBy(i => (sections(i).priority, i))
