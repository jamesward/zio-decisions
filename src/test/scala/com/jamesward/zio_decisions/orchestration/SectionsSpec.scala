package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.orchestration.Sections.Section
import zio.test.*

/** The laws of [[Sections.fit]], as properties over generated sections and budgets. */
object SectionsSpec extends ZIOSpecDefault:

  private val section: Gen[Any, Section] =
    for
      title    <- Gen.oneOf(Gen.const(""), Gen.stringBounded(1, 12)(Gen.alphaNumericChar))
      text     <- Gen.oneOf(Gen.stringBounded(0, 40)(Gen.printableChar), Gen.stringBounded(0, 2000)(Gen.alphaNumericChar))
      priority <- Gen.int(0, 5)
      floor    <- Gen.oneOf(Gen.const(0), Gen.int(0, 300), Gen.const(Int.MaxValue))
    yield Section(title, text, priority, floor)

  private val sections: Gen[Any, Sections] = Gen.vectorOfBounded(0, 8)(section).map(Sections(_))
  private val budget: Gen[Any, Int] = Gen.oneOf(Gen.int(0, 200), Gen.int(0, 20000))

  /** What each original section keeps under a fit: its text length, or -1 when dropped (matched by position). */
  private def kept(original: Sections, fitted: Sections): Vector[Int] =
    // fit keeps the order and drops sections, never reorders: match greedily by title and text prefix
    val out = fitted.sections
    original.sections.foldLeft((Vector.empty[Int], 0)):
      case ((acc, j), s) =>
        if j < out.size && out(j).title == s.title && out(j).priority == s.priority &&
            (out(j).text == s.text || s.text.startsWith(out(j).text.dropRight(Sections.Marker.length)))
        then (acc :+ out(j).text.length, j + 1)
        else (acc :+ -1, j)
    ._1

  def spec = suite("Sections")(
    test("bounded: a fit is never larger than its budget") {
      check(sections, budget)((s, b) => assertTrue(s.fit(b).size <= b))
    },
    test("identity under budget, and idempotent") {
      check(sections, budget): (s, b) =>
        val f = s.fit(b)
        assertTrue(s.fit(s.size) == s, f.fit(b) == f)
    },
    test("a cut text has exactly the length the fit chose, and its size is what render gives") {
      check(sections, budget): (s, b) =>
        val f = s.fit(b)
        assertTrue(f.render.length == f.size, s.render.length == s.size)
    },
    test("monotone: a larger budget keeps at least as much of every section") {
      check(sections, budget, budget): (s, b1, b2) =>
        val (lo, hi) = (b1.min(b2), b1.max(b2))
        val small = kept(s, s.fit(lo))
        val large = kept(s, s.fit(hi))
        assertTrue(small.zip(large).forall((a, b) => a <= b))
    },
    test("priority-ordered: a section is cut only once every section before it in the order is at its floor") {
      check(sections, budget): (s, b) =>
        val k = kept(s, s.fit(b))
        val order = Sections.order(s.sections)
        val touched = order.filter(i => k(i) != s.sections(i).text.length)
        // every section before a touched one (in the order) is at its floor or dropped
        val ok = touched.forall: i =>
          order.takeWhile(_ != i).forall(j => k(j) == -1 || k(j) <= s.sections(j).minText)
        // a section is dropped only when all sections are at their floor or dropped
        val drops = k.indices.filter(k(_) == -1)
        val dropsOk = drops.isEmpty || k.indices.forall(j => k(j) == -1 || k(j) <= s.sections(j).minText)
        assertTrue(ok, dropsOk)
    },
    test("a section with floor Int.MaxValue is kept whole or dropped") {
      check(sections, budget): (s, b) =>
        val k = kept(s, s.fit(b))
        assertTrue(s.sections.indices.forall(i => s.sections(i).floor != Int.MaxValue || k(i) == -1 || k(i) == s.sections(i).text.length))
    },
    test("concatenation is associative and empty is its identity") {
      check(sections, sections, sections): (a, b, c) =>
        assertTrue((a ++ b) ++ c == a ++ (b ++ c), a ++ Sections.empty == a, Sections.empty ++ a == a)
    },
  )
