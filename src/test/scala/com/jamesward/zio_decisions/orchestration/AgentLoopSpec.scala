package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.{DecisionModel, Decisions, TypeSafeMock}
import com.jamesward.zio_decisions.TypeSafeMock.MockBehavior.Respond
import com.jamesward.zio_decisions.typesafe.Wire
import com.jamesward.zio_decisions.orchestration.AgentLoop.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object AgentLoopSpec extends ZIOSpecDefault:

  private given CanEqual[Json, Json] = CanEqual.derived

  private def schema(properties: (String, String)*): Json.Obj =
    Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(properties.map((name, tpe) => name -> (Json.Obj("type" -> Json.Str(tpe)): Json))*),
      "required" -> Json.Arr(properties.map(p => Json.Str(p._1))*),
    )

  private val listOrders = Tool(
    "list_orders",
    "Lists a customer's orders.",
    schema("customerId" -> "string"),
    Some(schema("orders" -> "array")),
    Some(Json.Obj("readOnlyHint" -> Json.Bool(true))),
  )
  private val tools0: Tools = (_, _) => ZIO.succeed(Json.Obj())

  private val projectInfo = Tool("get_project_info", "Describes the open project.", Json.Obj("type" -> Json.Str("object")))

  /** Jev picks `id`. */
  private def choose(id: String) = Respond(Map("next_action" -> Wire.Answer.Choice(id, Map(id -> 0.9), 0.9)), Wire.Usage(100, 5))

  /** A generative model that writes `arguments` and replies with the ending and the steps it was given. */
  private final class FakeLlm(arguments: Json.Obj, seen: Ref[Vector[Any]]) extends Llm:
    def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
      seen.update(_ :+ request).as(LlmResult(arguments, 10, 2))
    def reply(request: ReplyRequest): Task[LlmResult[String]] =
      seen.update(_ :+ request).as(LlmResult(s"${request.ending} after ${request.steps.size} steps", 20, 4))

  /** Tools that answer `list_orders` and fail anything else, recording each call. */
  private def tools(calls: Ref[Vector[(String, Json.Obj)]]): Tools =
    (name, arguments) =>
      calls.update(_ :+ (name -> arguments)) *>
        (if name == "list_orders" then ZIO.succeed(Json.Obj("orders" -> Json.Arr(Json.Str("O1"), Json.Str("O2"))))
         else ZIO.fail(RuntimeException(s"Tool '$name' not found")))

  /** Each request's choice: option id to its description, in order. */
  private def criteria(requests: List[Wire.SystemOneRequest]): List[List[(String, Json)]] =
    requests.map: request =>
      request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).flatMap(_.get("next_action")).flatMap(_.asObject)
        .flatMap(_.get("criteria")).flatMap(_.asObject).toList.flatMap(_.fields)

  private def state(request: Wire.SystemOneRequest): Json.Obj =
    request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).getOrElse(Json.Obj())

  private def field(json: Json, name: String): Option[Json] = json.asObject.flatMap(_.get(name))

  private def setup(behaviors: TypeSafeMock.MockBehavior*) =
    for
      jev   <- TypeSafeMock.tracked(behaviors*)
      calls <- Ref.make(Vector.empty[(String, Json.Obj)])
      seen  <- Ref.make(Vector.empty[Any])
    yield (jev, calls, seen, FakeLlm(Json.Obj("customerId" -> Json.Str("C1")), seen))

  def spec = suite("AgentLoop")(
    test("documents are offered by title and description; reading one shows Jev its text and offers what it links to") {
      val toolDoc = Document("skill/reference/list_orders.md", "list_orders", "How to call list_orders.",
        ZIO.succeed(DocumentContent("Pass --customerId.")))
      val skill = Document("skill/SKILL.md", "orders", "Working with orders.",
        ZIO.succeed(DocumentContent("Read reference/list_orders.md before calling it.", List(toolDoc))), Map("allowed-tools" -> "list_orders"))
      for
        (jev, calls, seen, llm) <- setup(choose("doc_000"), choose("doc_000"), choose("tool_000"), choose("end_answer"))
        result   <- AgentLoop.run("orders of C1?", List(listOrders), tools(calls), llm, documents = List(skill)).provideLayer(jev.decisions)
        requests <- jev.requests
        options   = criteria(requests)
        llmSeen  <- seen.get
      yield assertTrue(
        options(0).map(_._1) == List("tool_000", "doc_000", "end_answer", "end_unable"),
        field(options(0)(1)._2, "action").contains(Json.Str("read_document")),
        field(options(0)(1)._2, "title").contains(Json.Str("orders")),
        field(options(0)(1)._2, "description").contains(Json.Str("Working with orders.")),
        field(options(0)(1)._2, "metadata").exists(_.toJson.contains("allowed-tools")),
        // after reading the skill, its linked reference is offered and the skill is not
        options(1).map(_._1) == List("tool_000", "doc_000", "end_answer", "end_unable"),
        field(options(1)(1)._2, "document").contains(Json.Str("skill/reference/list_orders.md")),
        field(state(requests(1)), "documentsRead").exists(_.toJson.contains("Read reference/list_orders.md")),
        // with both read, only the tool and the endings remain
        options(2).map(_._1) == List("tool_000", "end_answer", "end_unable"),
        result.readings.map(_.document) == Vector("skill/SKILL.md", "skill/reference/list_orders.md"),
        result.steps.size == 1,
        // the arguments writer and the reply writer get the documents read
        llmSeen.collect { case r: ArgumentsRequest => r.readings.size } == Vector(2),
        llmSeen.collect { case r: ReplyRequest => r.readings.flatMap(_.text.toOption) } ==
          Vector(Vector("Read reference/list_orders.md before calling it.", "Pass --customerId.")),
      )
    },
    test("a document that can't be read is a reading with its error; the reading budget limits how many are offered") {
      val broken = Document("a", "A", "", ZIO.fail(RuntimeException("gone")))
      val other = Document("b", "B", "", ZIO.succeed(DocumentContent("b")))
      for
        (jev, calls, _, llm) <- setup(choose("doc_000"), choose("end_answer"))
        result   <- AgentLoop.run("x", Nil, tools(calls), llm, AgentLoop.Config(maxReadings = 1), List(broken, other))
                      .provideLayer(jev.decisions)
        options  <- jev.requests.map(criteria)
      yield assertTrue(
        result.readings.map(r => (r.document, r.text)) == Vector(("a", Left("gone"))),
        options(1).map(_._1) == List("end_answer", "end_unable"),
      )
    },
    test("documents linked from several places are offered once") {
      val shared = Document("shared", "Shared", "", ZIO.succeed(DocumentContent("s")))
      val one = Document("one", "One", "", ZIO.succeed(DocumentContent("1", List(shared))))
      for
        (jev, calls, _, llm) <- setup(choose("doc_000"), choose("end_answer"))
        _       <- AgentLoop.run("x", Nil, tools(calls), llm, documents = List(one, shared)).provideLayer(jev.decisions)
        options <- jev.requests.map(criteria)
      yield assertTrue(options(1).map(o => field(o._2, "document")).flatten == List(Json.Str("shared")))
    },
    test("offers every tool, with its schemas and annotations, and the two endings") {
      for
        (jev, calls, seen, llm) <- setup(choose("end_answer"))
        result   <- AgentLoop.run("hi", List(listOrders, projectInfo), tools(calls), llm).provideLayer(jev.decisions)
        options  <- jev.requests.map(criteria)
        llmSeen  <- seen.get
      yield assertTrue(
        options.size == 1,
        options.head.map(_._1) == List("tool_000", "tool_001", "end_answer", "end_unable"),
        field(options.head(0)._2, "action").contains(Json.Str("call_tool")),
        field(options.head(0)._2, "tool").contains(Json.Str("list_orders")),
        field(options.head(0)._2, "description").contains(Json.Str("Lists a customer's orders.")),
        field(options.head(0)._2, "inputSchema").contains(listOrders.inputSchema),
        field(options.head(0)._2, "outputSchema") == listOrders.outputSchema,
        field(options.head(0)._2, "annotations") == listOrders.annotations,
        field(options.head(2)._2, "outcome").contains(Json.Str("answer")),
        field(options.head(3)._2, "outcome").contains(Json.Str("unable")),
        result.ending == Ending.Answered,
        result.reply == "Answered after 0 steps",
        llmSeen.collect { case r: ReplyRequest => r.tools.map(_.name) } == Vector(List("list_orders", "get_project_info")),
        result.steps.isEmpty,
      )
    },
    test("a tool offered without schemas is offered by name, description, annotations and short parameters; its arguments still follow its schema") {
      val lean = listOrders.copy(schemasInChoice = false)
      for
        (jev, calls, seen, llm) <- setup(choose("tool_000"), choose("end_answer"))
        _        <- AgentLoop.run("orders of C1?", List(lean), tools(calls), llm).provideLayer(jev.decisions)
        options  <- jev.requests.map(criteria)
        llmSeen  <- seen.get
      yield assertTrue(
        field(options.head(0)._2, "tool").contains(Json.Str("list_orders")),
        field(options.head(0)._2, "description").contains(Json.Str("Lists a customer's orders.")),
        field(options.head(0)._2, "annotations") == listOrders.annotations,
        field(options.head(0)._2, "inputSchema").isEmpty,
        field(options.head(0)._2, "parameters").contains(Json.Arr(AgentLoop.parameters(listOrders.inputSchema).map(Json.Str(_))*)),
        AgentLoop.parameters(listOrders.inputSchema).nonEmpty,
        field(options.head(0)._2, "outputSchema").isEmpty,
        llmSeen.collect { case r: ArgumentsRequest => r.tool.inputSchema } == Vector(listOrders.inputSchema),
      )
    },
    test("the host's context is shown to Jev with the request, and given to the generative model") {
      for
        (jev, calls, seen, llm) <- setup(choose("tool_000"), choose("end_answer"))
        _        <- AgentLoop.run("orders of C1?", List(listOrders), tools(calls), llm, context = Some("shop: orders by customer"))
                      .provideLayer(jev.decisions)
        states   <- jev.requests.map(_.map(_.body.toString))
        llmSeen  <- seen.get
      yield assertTrue(
        states.forall(_.contains("shop: orders by customer")),
        llmSeen.collect { case r: ArgumentsRequest => r.context } == Vector(Some("shop: orders by customer")),
        llmSeen.collect { case r: ReplyRequest => r.context } == Vector(Some("shop: orders by customer")),
      )
    },
    test("catalogInState: the tools are described once, in the state, and their options only name them") {
      val config = AgentLoop.Config(strategy = Strategy(catalogInState = true))
      for
        (jev, calls, seen, llm) <- setup(choose("end_answer"))
        _        <- AgentLoop.run("hi", List(listOrders, projectInfo), tools(calls), llm, config).provideLayer(jev.decisions)
        requests <- jev.requests
        options   = criteria(requests).head
      yield assertTrue(
        options.head._2.toJson == """{"action":"call_tool","tool":"list_orders","earlierCalls":0,"earlierFailures":0}""",
        state(requests.head).get("tools").exists(_.toJson.contains("Lists a customer's orders.")),
        state(requests.head).get("tools").exists(_.toJson.contains("customerId: string (required)")),
      )
    },
    test("relevanceFilter: one request of yes-or-no questions keeps the likely tools; more_tools offers the rest") {
      val many = (1 to 14).map(i => Tool(s"t$i", s"Tool $i.", Json.Obj("type" -> Json.Str("object")))).toList
      val noul = Respond(
        (0 until 14).map(i => f"tool_$i%03d" -> (Wire.Answer.Noul(if i < 2 then 0.9 else 0.05): Wire.Answer)).toMap,
        Wire.Usage(50, 14),
      )
      val config = AgentLoop.Config(strategy = Strategy(relevanceFilter = true))
      for
        (jev, calls, seen, llm) <- setup(noul, choose("more_tools"), choose("end_answer"))
        result   <- AgentLoop.run("hi", many, tools(calls), llm, config).provideLayer(jev.decisions)
        requests <- jev.requests
        options   = criteria(requests)
      yield assertTrue(
        requests.head.body.toJson.contains("\"type\":\"noul\""),
        // the 2 likely ones and the next 4 (at least 6 kept), more_tools, the endings
        options(1).map(_._1).count(_.startsWith("tool_")) == AgentLoop.Strategy.MinRelevant,
        options(1).map(_._1).contains("more_tools"),
        options(2).map(_._1).count(_.startsWith("tool_")) == 14,
        result.decisions.size == 3,
        result.decisionUsage.inputTokens == 250,
      )
    },
    test("boundCalls: a value an earlier result holds is offered as a ready-made call, called with no generative model") {
      val search = Tool("search", "Finds files.", schema("q" -> "string"))
      val read = Tool("read_file", "Reads a file.", schema("file_path" -> "string"))
      val found: Tools = (name, arguments) =>
        if name == "search" then ZIO.succeed(Json.Obj("text" -> Json.Str("""{"items":[{"filePath":"src/A.scala"}]}""")))
        else ZIO.succeed(Json.Obj("text" -> Json.Str("L1: object A")))
      val config = AgentLoop.Config(strategy = Strategy(boundCalls = true))
      for
        (jev, _, seen, llm) <- setup(choose("tool_000"), choose("bound_000"), choose("end_answer"))
        result   <- AgentLoop.run("read A", List(search, read), found, llm, config).provideLayer(jev.decisions)
        requests <- jev.requests
        llmSeen  <- seen.get
      yield assertTrue(
        criteria(requests)(1).find(_._1 == "bound_000").exists(_._2.toJson.contains("\"file_path\":\"src/A.scala\"")),
        result.steps.map(s => s.tool -> s.arguments.toJson) ==
          Vector("search" -> """{"customerId":"C1"}""", "read_file" -> """{"file_path":"src/A.scala"}"""),
        // the fake model's customerId doesn't fit search's schema (q): written once more, told so; the bound read needs none
        llmSeen.collect { case r: ArgumentsRequest => r.tool.name } == Vector("search", "search"),
      )
    },
    test("a tool whose arguments couldn't be written twice isn't offered again") {
      val failing = new Llm:
        def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] = ZIO.fail(RuntimeException("called another tool"))
        def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult("no", 1, 1))
      for
        (jev, calls, _, _) <- setup(choose("tool_000"), choose("tool_000"), choose("end_unable"))
        _        <- AgentLoop.run("orders?", List(listOrders, projectInfo), tools(calls), failing).provideLayer(jev.decisions)
        options  <- jev.requests.map(criteria)
      yield assertTrue(options(1).map(_._1).contains("tool_000"), !options(2).map(_._1).contains("tool_000"), options(2).map(_._1).contains("tool_001"))
    },
    test("noRepeats: a call that already succeeded isn't made again; the step points at the earlier result") {
      val config = AgentLoop.Config(strategy = Strategy(noRepeats = true))
      for
        (jev, calls, seen, llm) <- setup(choose("tool_000"), choose("tool_000"), choose("end_answer"))
        result <- AgentLoop.run("orders of C1?", List(listOrders), tools(calls), llm, config).provideLayer(jev.decisions)
        called <- calls.get
      yield assertTrue(
        called.size == 1,
        result.steps(1).outcome == Outcome.Failed("not called again: the same call already ran in step 1; use its result"),
      )
    },
    test("noRepeats: a call runs again when a call that isn't read-only succeeded since") {
      val config = AgentLoop.Config(strategy = Strategy(noRepeats = true))
      val write = Tool("write", "Writes.", Json.Obj("type" -> Json.Str("object")))
      val ok: Tools = (name, arguments) => ZIO.succeed(Json.Obj("ok" -> Json.Str(name)))
      for
        (jev, _, _, llm) <- setup(choose("tool_000"), choose("tool_001"), choose("tool_000"), choose("end_answer"))
        result <- AgentLoop.run("go", List(listOrders, write), ok, llm, config).provideLayer(jev.decisions)
      yield assertTrue(result.steps.map(_.outcome.isInstanceOf[Outcome.Succeeded]) == Vector(true, true, true))
    },
    test("parallelCalls: a second, independent call Jev picks in the same request runs too, as its own step") {
      val config = AgentLoop.Config(strategy = Strategy(parallelCalls = true))
      val both = Respond(Map(
        "next_action" -> Wire.Answer.Choice("tool_000", Map("tool_000" -> 0.9), 0.9),
        "also_call" -> Wire.Answer.Choice("tool_001", Map("tool_001" -> 0.8), 0.8),
      ), Wire.Usage(100, 5))
      for
        _        <- ZIO.unit
        end       = Respond(Map(
                      "next_action" -> Wire.Answer.Choice("end_answer", Map("end_answer" -> 0.9), 0.9),
                      "also_call" -> Wire.Answer.Choice("none", Map("none" -> 0.9), 0.9),
                    ), Wire.Usage(100, 5))
        (jev, calls, seen, llm) <- setup(both, end)
        result   <- AgentLoop.run("orders and info", List(listOrders, projectInfo), tools(calls), llm, config).provideLayer(jev.decisions)
        requests <- jev.requests
        called   <- calls.get
      yield assertTrue(
        requests.head.body.toJson.contains("also_call"),
        called.map(_._1).toSet == Set("list_orders", "get_project_info"),
        result.steps.map(_.number) == Vector(1, 2),
        result.decisions.size == 2,
      )
    },
    test("over the state budget, older steps' results are cut shorter for Jev; the newest keep resultPreview") {
      val big: Tools = (_, _) => ZIO.succeed(Json.Obj("text" -> Json.Str("x" * 5000)))
      val config = AgentLoop.Config(resultPreview = 3000, stateBudget = 5000)
      for
        (jev, _, _, llm) <- setup(choose("tool_000"), choose("tool_000"), choose("tool_000"), choose("end_answer"))
        _        <- AgentLoop.run("go", List(listOrders), big, llm, config).provideLayer(jev.decisions)
        requests <- jev.requests
        lengths   = state(requests.last).get("steps").flatMap(_.asArray).toList.flatten
                      .map(s => s.asObject.flatMap(_.get("result")).flatMap(_.asString).fold(0)(_.length))
      yield assertTrue(lengths.size == 3, lengths(2) > 3000, lengths(1) < 3000, lengths(0) < 400)
    },
    test("keepContext: Jev's no on an earlier step leaves its result out of Jev's and the LLM's context; the transcript keeps it") {
      val big: Tools = (_, _) => ZIO.succeed(Json.Obj("text" -> Json.Str("x" * 1000)))
      val config = AgentLoop.Config(strategy = Strategy(context = ContextChoice.Keep))
      val noOnStep1 = Respond(Map(
        "next_action" -> Wire.Answer.Choice("tool_000", Map("tool_000" -> 0.9), 0.9),
        "keep_step_1" -> Wire.Answer.Noul(0.1),
      ), Wire.Usage(100, 5))
      val end = Respond(Map(
        "next_action" -> Wire.Answer.Choice("end_answer", Map("end_answer" -> 0.9), 0.9),
        "keep_step_2" -> Wire.Answer.Noul(0.9),
      ), Wire.Usage(100, 5))
      for
        (jev, _, seen, llm) <- setup(choose("tool_000"), choose("tool_000"), noOnStep1, end)
        result   <- AgentLoop.run("go", List(listOrders), big, llm, config).provideLayer(jev.decisions)
        requests <- jev.requests
        llmSeen  <- seen.get
        steps     = state(requests.last).get("steps").flatMap(_.asArray).toList.flatten
      yield assertTrue(
        // asked about step 1 once there were two steps (never the newest); then only about 2 (1 is out already)
        !requests(1).body.toString.contains("keep_step"),
        requests(2).body.toString.contains("keep_step_1"), !requests(2).body.toString.contains("keep_step_2"),
        requests(3).body.toString.contains("keep_step_2"), !requests(3).body.toString.contains("keep_step_1"),
        steps.head.toJson.contains("left out of context"), !steps.head.toJson.contains("xxxx"),
        steps(1).toJson.contains("xxxx"),
        llmSeen.collect { case r: ReplyRequest => r.steps.head.outcome.toString }.head.contains("left out of context"),
        // the transcript itself is whole
        result.steps.head.outcome.toString.contains("xxxx"), result.leftOut == Set(1),
      )
    },
    test("rateContext: a step rated below the threshold is left out; one rated above stays") {
      val big: Tools = (_, _) => ZIO.succeed(Json.Obj("text" -> Json.Str("x" * 1000)))
      val config = AgentLoop.Config(strategy = Strategy(context = ContextChoice.Rate))
      val rated = Respond(Map(
        "next_action" -> Wire.Answer.Choice("tool_000", Map("tool_000" -> 0.9), 0.9),
        "keep_step_1" -> Wire.Answer.Score(0.1, Map.empty, Map("0" -> 0.9, "1" -> 0.1, "2" -> 0.0, "3" -> 0.0), 0.9),
        "keep_step_2" -> Wire.Answer.Score(2.5, Map.empty, Map("0" -> 0.0, "1" -> 0.0, "2" -> 0.5, "3" -> 0.5), 0.9),
      ), Wire.Usage(100, 5))
      for
        _    <- ZIO.unit
        end2 = Respond(Map(
                 "next_action" -> Wire.Answer.Choice("end_answer", Map("end_answer" -> 0.9), 0.9),
                 "keep_step_2" -> Wire.Answer.Score(2.5, Map.empty, Map("2" -> 0.5, "3" -> 0.5), 0.9),
                 "keep_step_3" -> Wire.Answer.Score(2.5, Map.empty, Map("2" -> 0.5, "3" -> 0.5), 0.9),
               ), Wire.Usage(100, 5))
        first = Respond(Map(
                  "next_action" -> Wire.Answer.Choice("tool_000", Map("tool_000" -> 0.9), 0.9),
                  "keep_step_1" -> Wire.Answer.Score(2.5, Map.empty, Map("2" -> 0.5, "3" -> 0.5), 0.9),
                ), Wire.Usage(100, 5))
        (jev, _, _, llm) <- setup(choose("tool_000"), choose("tool_000"), rated, end2)
        result <- AgentLoop.run("go", List(listOrders), big, llm, config).provideLayer(jev.decisions)
      yield assertTrue(result.leftOut == Set(1))
    },
    test("a Jev request stays within the request budget: the options as they are, the steps and the context fitted") {
      val many = (1 to 40).map(i => Tool(s"tool_$i", s"Tool $i. " + "d" * 600, schema("customerId" -> "string")))
      val big: Tools = (_, _) => ZIO.succeed(Json.Obj("text" -> Json.Str("x" * 9000)))
      val config = AgentLoop.Config(requestBudget = 60000)
      for
        (jev, _, seen, llm) <- setup(choose("tool_000"), choose("tool_000"), choose("tool_000"), choose("tool_000"), choose("end_answer"))
        _        <- AgentLoop.run("go", many, big, llm, config, context = Some("p" * 100000),
                      live = ZIO.succeed(Sections.of("Live", "l" * 50000, 50))).provideLayer(jev.decisions)
        requests <- jev.requests
        llmSeen  <- seen.get
      yield assertTrue(
        requests.size == 5,
        requests.forall(_.body.toJson.length <= 60000),
        // the context is cut, not dropped: the higher-priority live section outlasts the project context
        requests.forall(r => state(r).get("context").exists(_.toJson.contains("lll"))),
        llmSeen.collect { case r: ArgumentsRequest => r.context }.forall(_.forall(_.length <= config.llmContextBudget)),
      )
    },
    test("the writer proposing another offered tool's call: the step isn't made, and Jev is offered that call ready-made") {
      val build = Tool("build", "Builds.", Json.Obj("type" -> Json.Str("object"), "properties" -> Json.Obj("all" -> Json.Obj("type" -> Json.Str("boolean")))))
      val patch = Tool("patch", "Patches.", schema("diff" -> "string"))
      val proposing = new Llm:
        def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
          if request.tool.name == "build" then ZIO.fail(Proposed("patch", Json.Obj("diff" -> Json.Str("+x"))))
          else ZIO.succeed(LlmResult(Json.Obj("all" -> Json.Bool(true)), 1, 1))
        def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult("done", 1, 1))
      for
        jev      <- TypeSafeMock.tracked(choose("tool_000"), choose("proposed_000"), choose("end_answer"))
        calls    <- Ref.make(Vector.empty[(String, Json.Obj)])
        result   <- AgentLoop.run("go", List(build, patch), (n, a) => calls.update(_ :+ (n -> a)).as(Json.Obj()), proposing)
                      .provideLayer(jev.decisions)
        requests <- jev.requests
        made     <- calls.get
        options   = criteria(requests)
      yield assertTrue(
        made == Vector("patch" -> Json.Obj("diff" -> Json.Str("+x"))),
        options(1).map(_._1).contains("proposed_000"), !options(2).map(_._1).contains("proposed_000"),
        result.steps.head.outcome.toString.contains("would call patch instead"),
        result.steps(1).tool == "patch",
      )
    },
    test("correct: Jev assesses each step with the decision; after a failure it can choose a move, whose guidance stays until a step works") {
      val test = Tool("run_tests", "Runs the tests.", schema("which" -> "string"))
      // run 1 fails (Jev: didn't work, uninformative); Jev picks diagnose; run 2 fails again (repeats step 1); run 3 works
      val script = TypeSafeMock.MockBehavior.Always: request =>
        val qs = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
        val steps = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("steps")).flatMap(_.asArray).map(_.size).getOrElse(0)
        val options = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).flatMap(_.get("next_action")).flatMap(_.asObject)
          .flatMap(_.get("criteria")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
        val next =
          if options.contains("move_diagnose") && !request.body.toString.contains("Course correction: diagnose") then "move_diagnose"
          else if steps < 3 then "tool_000" else "end_answer"
        Respond((qs.map:
          case "next_action"        => "next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)
          case "newest_worked"      => "newest_worked" -> Wire.Answer.Noul(if steps >= 3 then 0.95 else 0.05)
          case "newest_informative" => "newest_informative" -> Wire.Answer.Noul(0.1)
          case "newest_repeats"     => "newest_repeats" -> Wire.Answer.Choice("step_001", Map("step_001" -> 0.9), 0.9)
          case other                => other -> Wire.Answer.Noul(0.5)
        ).toMap, Wire.Usage(100, 5))
      for
        jev      <- TypeSafeMock.tracked(script)
        seen     <- Ref.make(Vector.empty[Any])
        runs     <- Ref.make(0)
        tools     = new Tools:
                      def call(name: String, arguments: Json.Obj): Task[Json] =
                        runs.updateAndGet(_ + 1).map(n => Json.Str(if n < 3 then "FAILED: timed out" else "PASSED"))
        result   <- AgentLoop.run("make the test pass", List(test), tools, FakeLlm(Json.Obj("which" -> Json.Str("all")), seen),
                      AgentLoop.Config(strategy = Strategy(correct = true))).provideLayer(jev.decisions)
        requests <- jev.requests
        llmSeen  <- seen.get
        args      = llmSeen.collect { case r: ArgumentsRequest => r.context.getOrElse("") }
      yield assertTrue(
        result.steps.size == 3, result.ending == Ending.Answered,
        // the move: offered after the failure, chosen, and its guidance in the next arguments' context, not the first's
        criteria(requests).exists(_.exists(_._1 == "move_diagnose")),
        !args.head.contains("Course correction"), args(1).contains("Course correction: diagnose"),
        // the steps carry the assessment
        state(requests.last).toJson.contains("repeats step 1's failure"),
        state(requests.last).toJson.contains("says only that it failed"),
      )
    },
    test("correct: only checks are assessed; an edit that succeeded is taken as worked, and offers no move") {
      val edit = Tool("apply_patch", "Edits.", schema("patch" -> "string"))
      val script = TypeSafeMock.MockBehavior.Always: request =>
        val qs = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
        val steps = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("steps")).flatMap(_.asArray).map(_.size).getOrElse(0)
        val next = if steps < 2 then "tool_000" else "end_answer"
        Respond(qs.map(q => q -> (if q == "next_action" then Wire.Answer.Choice(next, Map(next -> 0.9), 0.9) else Wire.Answer.Noul(0.1))).toMap,
          Wire.Usage(10, 1))
      for
        jev      <- TypeSafeMock.tracked(script)
        seen     <- Ref.make(Vector.empty[Any])
        _        <- AgentLoop.run("edit it", List(edit), tools0, FakeLlm(Json.Obj("patch" -> Json.Str("x")), seen),
                      AgentLoop.Config(strategy = Strategy(correct = true))).provideLayer(jev.decisions)
        requests <- jev.requests
      yield assertTrue(
        !requests.exists(_.body.toString.contains("newest_worked")),
        !criteria(requests).exists(_.exists(_._1.startsWith("move_"))),
      )
    },
    test("giveUp: from the third failed check, Jev judging the work not getting closer leaves only the endings") {
      val check = Tool("run_tests", "Runs the tests.", schema("which" -> "string"))
      val script = TypeSafeMock.MockBehavior.Always: request =>
        val qs = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
        val options = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).flatMap(_.get("next_action")).flatMap(_.asObject)
          .flatMap(_.get("criteria")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
        val next = if options.contains("tool_000") then "tool_000" else "end_unable"
        Respond(qs.map:
          case "next_action"      => "next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)
          case "newest_repeats"   => "newest_repeats" -> Wire.Answer.Choice("none", Map("none" -> 0.9), 0.9)
          case "work_progressing" => "work_progressing" -> Wire.Answer.Noul(0.1)
          case other              => other -> Wire.Answer.Noul(0.05)
        .toMap, Wire.Usage(10, 1))
      for
        jev      <- TypeSafeMock.tracked(script)
        seen     <- Ref.make(Vector.empty[Any])
        result   <- AgentLoop.run("make it pass", List(check), (_, _) => ZIO.succeed(Json.Str("FAILED")), FakeLlm(Json.Obj("which" -> Json.Str("a")), seen),
                      AgentLoop.Config(strategy = Strategy(correct = true, giveUp = true))).provideLayer(jev.decisions)
        requests <- jev.requests
      yield assertTrue(
        result.steps.size == 3, result.ending == Ending.Unable,
        requests.count(_.body.toString.contains("work_progressing")) == 1,
      )
    },
    test("correct: escalating hands the writing to the stronger model for the rest of the loop") {
      val script = TypeSafeMock.MockBehavior.Always: request =>
        val qs = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
        val steps = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("steps")).flatMap(_.asArray).map(_.size).getOrElse(0)
        val options = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).flatMap(_.get("next_action")).flatMap(_.asObject)
          .flatMap(_.get("criteria")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
        val next = if options.contains("move_escalate") then "move_escalate" else if steps < 2 then "tool_000" else "end_answer"
        Respond((qs.map:
          case "next_action"   => "next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)
          case "newest_worked" => "newest_worked" -> Wire.Answer.Noul(if steps >= 2 then 0.9 else 0.1)
          case "newest_repeats" => "newest_repeats" -> Wire.Answer.Choice("none", Map("none" -> 0.9), 0.9)
          case other           => other -> Wire.Answer.Noul(0.9)
        ).toMap, Wire.Usage(100, 5))
      val check = Tool("run_check", "Runs the check.", schema("customerId" -> "string"))
      for
        jev     <- TypeSafeMock.tracked(script)
        weak    <- Ref.make(Vector.empty[Any])
        strong  <- Ref.make(Vector.empty[Any])
        result  <- AgentLoop.run("go", List(check), tools0, FakeLlm(Json.Obj("customerId" -> Json.Str("C1")), weak),
                     AgentLoop.Config(strategy = Strategy(correct = true),
                       escalation = Some(FakeLlm(Json.Obj("customerId" -> Json.Str("C2")), strong)))).provideLayer(jev.decisions)
        w       <- weak.get
        st      <- strong.get
      yield assertTrue(
        w.count(_.isInstanceOf[ArgumentsRequest]) == 1, st.count(_.isInstanceOf[ArgumentsRequest]) == 1,
        st.exists(_.isInstanceOf[ReplyRequest]), result.steps(1).arguments == Json.Obj("customerId" -> Json.Str("C2")),
      )
    },
    test("arguments that don't fit the schema are written again, once, told what's wrong") {
      val search = Tool("search_file", "Finds files.", schema("q" -> "string"))
      val writes = Unsafe.unsafely(Ref.unsafe.make(Vector.empty[ArgumentsRequest]))
      val llm = new Llm:
        def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
          writes.update(_ :+ request).as(LlmResult(if request.problem.isEmpty then Json.Obj("query" -> Json.Str("x")) else Json.Obj("q" -> Json.Str("x")), 1, 1))
        def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult("ok", 1, 1))
      for
        jev    <- TypeSafeMock.tracked(choose("tool_000"), choose("end_answer"))
        calls  <- Ref.make(Vector.empty[(String, Json.Obj)])
        result <- AgentLoop.run("find x", List(search), tools(calls), llm).provideLayer(jev.decisions)
        made   <- calls.get
        w      <- writes.get
      yield assertTrue(
        made == Vector("search_file" -> Json.Obj("q" -> Json.Str("x"))),
        w.size == 2, w(1).problem.exists(p => p.contains("missing q") && p.contains("it has no query")),
        AgentLoop.misfit(schema("q" -> "string"), Json.Obj("q" -> Json.Str("y"))).isEmpty,
      )
    },
    test("boundCalls: a check made before is offered again, ready-made, once something changed since") {
      val check = Tool("run_tests", "Runs the tests.", schema("which" -> "string"))
      val edit = Tool("apply_patch", "Edits.", schema("patch" -> "string"))
      // run the tests (written), edit (written), then the ready-made rerun, then end
      val script = TypeSafeMock.MockBehavior.Always: request =>
        val steps = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("steps")).flatMap(_.asArray).map(_.size).getOrElse(0)
        val next = steps match
          case 0 => "tool_000"
          case 1 => "tool_001"
          case 2 => "again_000"
          case _ => "end_answer"
        Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)), Wire.Usage(10, 1))
      for
        jev      <- TypeSafeMock.tracked(script)
        seen     <- Ref.make(Vector.empty[Any])
        written  = new Llm:
                     def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
                       seen.update(_ :+ request).as(LlmResult(if request.tool.name == "run_tests" then Json.Obj("which" -> Json.Str("all")) else Json.Obj("patch" -> Json.Str("x")), 1, 1))
                     def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult("ok", 1, 1))
        result   <- AgentLoop.run("go", List(check, edit), tools0, written, AgentLoop.Config(strategy = Strategy(boundCalls = true)))
                      .provideLayer(jev.decisions)
        requests <- jev.requests
        writes   <- seen.get
      yield assertTrue(
        result.steps.map(_.tool) == Vector("run_tests", "apply_patch", "run_tests"),
        result.steps(2).arguments == Json.Obj("which" -> Json.Str("all")),
        writes.size == 2,
        !criteria(requests).head.exists(_._1.startsWith("again_")), criteria(requests)(2).exists(_._1 == "again_000"),
      )
    },
    test("a writer that keeps proposing a call already made doesn't hold the loop: each repeat is a counted step") {
      val edit = Tool("edit_file", "Edits.", schema("path" -> "string"))
      val read = Tool("read_file", "Reads.", schema("path" -> "string"), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      // Jev reads once, then keeps choosing the edit; the writer keeps proposing the same read instead
      val script = TypeSafeMock.MockBehavior.Always: request =>
        val steps = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("steps")).flatMap(_.asArray).map(_.size).getOrElse(0)
        val body = request.body.toString
        val next = if steps == 0 then "tool_001" else if body.contains("\"end_answer\"") && !body.contains("tool_000") then "end_answer" else "tool_000"
        Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)), Wire.Usage(10, 1))
      val writer = new Llm:
        def arguments(request: ArgumentsRequest) =
          if request.tool.name == "edit_file" then ZIO.fail(Proposed("read_file", Json.Obj("path" -> Json.Str("A"))))
          else ZIO.succeed(LlmResult(Json.Obj("path" -> Json.Str("A"))))
        def reply(request: ReplyRequest) = ZIO.succeed(LlmResult("done"))
      for
        jev    <- TypeSafeMock.tracked(script)
        tools   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Str("text"))
        result <- AgentLoop.run("edit A", List(edit, read), tools, writer, AgentLoop.Config(maxSteps = 5)).provideLayer(jev.decisions)
      yield assertTrue(
        result.ending == Ending.BudgetExhausted || result.ending == Ending.Answered,
        result.steps.size == 5,
        result.steps.drop(1).forall(_.outcome.toString.contains("would repeat step 1")),
      )
    },
    test("endsTurn: after a successful call of such a tool only the endings are offered") {
      val watch = Tool("watch_log", "Starts a watch.", schema("log" -> "string"))
      val read = Tool("read_file", "Reads.", schema("path" -> "string"))
      for
        jev    <- TypeSafeMock.tracked(TypeSafeMock.MockBehavior.Always: request =>
                    val body = request.body.toString
                    val next = if body.contains("\"tool_000\"") then "tool_000" else "end_answer"
                    Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)), Wire.Usage(10, 1)))
        tools   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Str("started"))
        result <- AgentLoop.run("watch it", List(watch, read), tools, FakeLlm(Json.Obj("log" -> Json.Str("x"), "path" -> Json.Str("y")),
                    Unsafe.unsafely(Ref.unsafe.make(Vector.empty[Any]))), AgentLoop.Config(endsTurn = _.name == "watch_log")).provideLayer(jev.decisions)
        asked  <- jev.requests
      yield assertTrue(result.steps.map(_.tool) == Vector("watch_log"), result.ending == Ending.Answered,
        !asked.last.body.toString.contains("\"tool_000\""))
    },
    test("exploreLimit: after that many reads in a row, Jev judges readiness; ready, the loop isn't offered reading tools until it acts") {
      val read = Tool("read_file", "Reads.", schema("path" -> "string"), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      val edit = Tool("edit_file", "Edits.", schema("path" -> "string"))
      // Jev reads whenever reading is offered, edits when it isn't, ends after an edit; it's ready to act when asked
      def script(ready: Double) = TypeSafeMock.MockBehavior.Always: request =>
        val body = request.body.toString
        val steps = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("steps")).flatMap(_.asArray).getOrElse(Chunk.empty)
        val edited = steps.exists(_.toString.contains("edit_file"))
        val next = if edited then "end_answer" else if body.contains("\"tool_000\"") then "tool_000"
                   else if body.contains("\"tool_001\"") then "tool_001" else "end_answer"
        Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9), "ready_to_act" -> Wire.Answer.Noul(ready),
          "asks_change" -> Wire.Answer.Noul(0.9)))
      for
        jev    <- TypeSafeMock.tracked(script(0.9))
        calls  <- Ref.make(Vector.empty[String])
        tools   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] = calls.update(_ :+ name).as(Json.Str("text"))
        writer  = FakeLlm(Json.Obj("path" -> Json.Str("A")), Unsafe.unsafely(Ref.unsafe.make(Vector.empty[Any])))
        result <- AgentLoop.run("change A", List(read, edit), tools, writer, AgentLoop.Config(exploreLimit = 3)).provideLayer(jev.decisions)
        asked  <- jev.requests
        off    <- TypeSafeMock.tracked(script(0.9))
        _      <- AgentLoop.run("change A", List(read, edit), tools, writer, AgentLoop.Config(maxSteps = 6)).provideLayer(off.decisions)
        offAsked <- off.requests
        unready <- TypeSafeMock.tracked(script(0.1))
        stayed <- AgentLoop.run("change A", List(read, edit), tools, writer, AgentLoop.Config(maxSteps = 6, exploreLimit = 3)).provideLayer(unready.decisions)
        capped <- TypeSafeMock.tracked(script(0.1))
        forced <- AgentLoop.run("change A", List(read, edit), tools, writer, AgentLoop.Config(maxSteps = 12, exploreLimit = 2)).provideLayer(capped.decisions)
      yield
        val bodies = asked.map(_.body.toString)
        assertTrue(
          // three reads, then readiness asked (and the fourth read still offered), then no reading: the edit
          result.steps.map(_.tool) == Vector("read_file", "read_file", "read_file", "read_file", "edit_file"),
          bodies.take(3).forall(!_.contains("ready_to_act")), bodies(3).contains("ready_to_act"),
          !bodies(4).contains("\"tool_000\""), bodies(4).contains("Enough reading: make the change"),
          // after the edit, reading is offered again
          bodies(5).contains("\"tool_000\""),
          !offAsked.exists(_.body.toString.contains("ready_to_act")),
          // not ready, with 6 steps: the loop reads until ActReserve steps are left, then it's told to act (a change request)
          stayed.steps.map(_.tool) == Vector.fill(4)("read_file") :+ "edit_file",
          forced.steps.map(_.tool) == Vector.fill(4)("read_file") :+ "edit_file",
        )
    },
    test("reviewEvery: every that many calls Jev judges whether they moved the work forward; judged not, the loop ends") {
      val read = Tool("read_file", "Reads.", schema("path" -> "string"), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      // Jev reads whenever reading is offered, and judges the review `forward`
      def script(forward: Double) = TypeSafeMock.MockBehavior.Always: request =>
        val next = if request.body.toString.contains("\"tool_000\"") then "tool_000" else "end_answer"
        Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9), "work_moving_forward" -> Wire.Answer.Noul(forward)))
      val tools = new Tools:
        def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Str("same text"))
      val writer = FakeLlm(Json.Obj("path" -> Json.Str("A")), Unsafe.unsafely(Ref.unsafe.make(Vector.empty[Any])))
      for
        stuck   <- TypeSafeMock.tracked(script(0.1))
        halted  <- AgentLoop.run("find X", List(read), tools, writer, AgentLoop.Config(maxSteps = 12, reviewEvery = 3, checkDone = true))
                     .provideLayer(stuck.decisions)
        asked   <- stuck.requests
        moving  <- TypeSafeMock.tracked(script(0.9))
        full    <- AgentLoop.run("find X", List(read), tools, writer, AgentLoop.Config(maxSteps = 8, reviewEvery = 3)).provideLayer(moving.decisions)
        movingAsked <- moving.requests
      yield assertTrue(
        // reviewed once three calls were in: that decision's call runs, then only the endings
        halted.steps.size == 4, asked.take(3).forall(!_.body.toString.contains("work_moving_forward")),
        asked(3).body.toString.contains("work_moving_forward"),
        full.steps.size == 8, movingAsked.count(_.body.toString.contains("work_moving_forward")) == 2,
      )
    },
    test("reviewEvery with exploreLimit: a reading phase is readiness's to judge; reads judged no progress don't end a loop ready to act") {
      val read = Tool("read_file", "Reads.", schema("path" -> "string"), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      val edit = Tool("edit_file", "Edits.", schema("path" -> "string"))
      // reads while reading is offered, then edits, then ends; a review would say no progress; ready from the 4th read
      def script = TypeSafeMock.MockBehavior.Always: request =>
        val body = request.body.toString
        val reads = "\"tool\":\"read_file\"".r.findAllMatchIn(body).size
        val next = if body.contains("\"edit_file\"") && reads > 0 && body.contains("\"tool_001\"") && !body.contains("\"tool_000\"") then "tool_001"
          else if body.contains("\"tool_000\"") then "tool_000" else "end_answer"
        Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9), "work_moving_forward" -> Wire.Answer.Noul(0.01),
          "ready_to_act" -> Wire.Answer.Noul(0.98), "asks_change" -> Wire.Answer.Noul(1.0)))
      val tools = new Tools:
        def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Str("text"))
      val writer = FakeLlm(Json.Obj("path" -> Json.Str("A")), Unsafe.unsafely(Ref.unsafe.make(Vector.empty[Any])))
      for
        jev    <- TypeSafeMock.tracked(script)
        result <- AgentLoop.run("change X", List(read, edit), tools, writer,
                    AgentLoop.Config(maxSteps = 12, reviewEvery = 3, exploreLimit = 3)).provideLayer(jev.decisions)
        asked  <- jev.requests
      yield
        val withReadiness = asked.filter(_.body.toString.contains("ready_to_act"))
        assertTrue(
          // the loop wasn't ended by a review of its reads: it went on to the edit
          result.steps.map(_.tool).contains("edit_file"),
          withReadiness.nonEmpty, withReadiness.forall(!_.body.toString.contains("work_moving_forward")),
        )
    },
    test("goals: a loop with a goal can revise it (the tree's node gets it, and the decisions see it), and dropping it ends the loop") {
      val read = Tool("read_file", "Reads.", schema("path" -> "string"), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      // reads first; then revises the goal, then drops it; then ends
      def script = TypeSafeMock.MockBehavior.Always: request =>
        val body = request.body.toString
        val revised = body.contains("The goal now")
        val next =
          if !body.contains("\"step\":1") then "tool_000"
          else if body.contains("\"revise_goal\"") then "revise_goal"
          else "end_answer"
        Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)))
      final class Writing(asked: Ref[Vector[ArgumentsRequest]]) extends Llm:
        def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
          asked.updateAndGet(_ :+ request).map: all =>
            if request.tool.name != "revise_goal" then LlmResult(Json.Obj("path" -> Json.Str("A")))
            else if all.count(_.tool.name == "revise_goal") == 1 then
              LlmResult(Json.Obj("goal" -> Json.Str("fix only the discount rounding"), "why" -> Json.Str("the rest already works")))
            else LlmResult(Json.Obj("drop" -> Json.Bool(true), "why" -> Json.Str("the rounding was already fixed upstream")))
        def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult("done"))
      val tools = new Tools:
        def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Str("text"))
      for
        jev    <- TypeSafeMock.tracked(script)
        asked  <- Ref.make(Vector.empty[ArgumentsRequest])
        tree   <- ProcessTree.make
        seen   <- Ref.make(Vector.empty[ProcessTree.Node])
        result <- tree.child(None, ProcessTree.Kind.Task, "task-1", goal = Some("fix the checkout"))(id =>
                    AgentLoop.run("fix the checkout", List(read), tools, Writing(asked), AgentLoop.Config(maxSteps = 8))
                      .provideLayer(jev.decisions) <* tree.snapshot.flatMap(ns => seen.set(ns)))
        nodes  <- seen.get
        reqs   <- jev.requests
      yield
        val task = nodes.find(_.name == "task-1").get
        assertTrue(
          result.ending == Ending.Answered, result.steps.map(_.tool) == Vector("read_file"),
          // revised, then dropped, each with why
          task.goals.map(_.goal) == Vector(Some("fix the checkout"), Some("fix only the discount rounding"), None),
          task.goals.map(_.why).drop(1) == Vector("the decision model revised it: the rest already works",
            "the decision model dropped it: the rounding was already fixed upstream"),
          // the decision after the revision sees the goal as it now is
          reqs.exists(r => r.body.toString.contains("The goal now") && r.body.toString.contains("fix only the discount rounding")),
          // dropped: only the endings after that
          !reqs.last.body.toString.contains("\"tool_000\""),
        )
    },
    test("without a goal (no process tree), revising isn't offered") {
      for
        (jev, calls, _, llm) <- setup(choose("tool_000"), choose("end_answer"))
        _       <- AgentLoop.run("orders?", List(listOrders), tools(calls), llm).provideLayer(jev.decisions)
        options <- jev.requests.map(criteria)
      yield assertTrue(options.forall(!_.exists(_._1 == "revise_goal")))
    },
    test("endsTurn with checkDone: a reply that says the started work goes on isn't sent back as unfinished") {
      val start = Tool("self_improve", "Starts rounds in the background.", schema("log" -> "string"))
      val read = Tool("read_file", "Reads.", schema("path" -> "string"))
      for
        jev    <- TypeSafeMock.tracked(TypeSafeMock.MockBehavior.Always: request =>
                    val body = request.body.toString
                    // Jev would call the reply unfinished (the rounds still run), and would go on reading if asked
                    if body.contains("reply_says_unfinished") then Respond(Map("reply_says_unfinished" -> Wire.Answer.Noul(0.9)))
                    else
                      val next = if body.contains("\"tool_000\"") && !body.contains("The work isn't done") then "tool_000" else "end_answer"
                      Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9))))
        calls  <- Ref.make(Vector.empty[String])
        tools   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] = calls.update(_ :+ name).as(Json.Str("started"))
        writer  = new Llm:
                    def arguments(request: ArgumentsRequest) = ZIO.succeed(LlmResult(Json.Obj("log" -> Json.Str("x"), "path" -> Json.Str("y"))))
                    def reply(request: ReplyRequest) = ZIO.succeed(LlmResult("Started: the rounds run in the background."))
        result <- AgentLoop.run("improve yourself", List(start, read), tools, writer,
                    AgentLoop.Config(endsTurn = _.name == "self_improve", checkDone = true)).provideLayer(jev.decisions)
        made   <- calls.get
        asked  <- jev.requests
      yield assertTrue(made == Vector("self_improve"), result.ending == Ending.Answered,
        !asked.exists(_.body.toString.contains("reply_says_unfinished")))
    },
    test("a request too long for Jev is made again smaller, that request only: no step is replayed, the next starts at the full budget") {
      val read = Tool("read_file", "Reads.", schema("path" -> "string"), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      val edit = Tool("edit_file", "Edits.", schema("path" -> "string"))
      // Jev rejects any request over 30000 characters; the steps' results are long; it reads, edits, reads, then ends
      val script = TypeSafeMock.MockBehavior.AlwaysOr: request =>
        val body = request.body.toJson
        val steps = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("steps")).flatMap(_.asArray).map(_.size).getOrElse(0)
        if body.length > 30000 then Left(Decisions.Error.BadRequest("max_tokens_exceeded"))
        else
          val next = List("tool_000", "tool_001", "tool_000").lift(steps).getOrElse("end_answer")
          Right(Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)), Wire.Usage(10, 1)))
      for
        jev    <- TypeSafeMock.tracked(script)
        calls  <- Ref.make(Vector.empty[String])
        tools   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] = calls.update(_ :+ name).as(Json.Str("x" * 12000))
        result <- AgentLoop.run("go", List(read, edit), tools, FakeLlm(Json.Obj("path" -> Json.Str("A")), Unsafe.unsafely(Ref.unsafe.make(Vector.empty[Any]))),
                    AgentLoop.Config(requestBudget = 40000)).provideLayer(jev.decisions)
        made   <- calls.get
      yield assertTrue(made == Vector("read_file", "edit_file", "read_file"), result.steps.size == 3, result.ending == Ending.Answered)
    },
    test("a large catalog's descriptions are cut to fit the options' share of the request, so the steps keep their room") {
      val many = (0 until 60).map(i => Tool(f"tool$i%02d", ("A very long description of what this tool does. " * 40).trim, schema("x" -> "string")))
      for
        jev    <- TypeSafeMock.tracked(Respond(Map("next_action" -> Wire.Answer.Choice("end_answer", Map("end_answer" -> 0.9), 0.9))))
        tools   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Str("ok"))
        _      <- AgentLoop.run("go", many, tools, FakeLlm(Json.Obj(), Unsafe.unsafely(Ref.unsafe.make(Vector.empty[Any]))),
                    AgentLoop.Config(requestBudget = 100000)).provideLayer(jev.decisions)
        asked  <- jev.requests
        options = asked.head.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).flatMap(_.get("next_action")).map(_.toJson.length).getOrElse(0)
      yield assertTrue(options <= 45000, asked.head.body.toJson.contains("A very long description of what this tool does."),
        AgentLoop.cutDescription("One. Two three. Four.", 10) == "One.", AgentLoop.cutDescription("abcdefghijkl", 5) == "abcde…")
    },
    test("checkDone: an answer whose reply says the work isn't done sends the loop on, told so; a done one ends") {
      val edit = Tool("edit_file", "Edits.", schema("path" -> "string"))
      // Jev ends at once each time; the first reply says the selector is still there
      val script = TypeSafeMock.MockBehavior.Always: request =>
        val body = request.body.toString
        if body.contains("reply_says_unfinished") then
          Respond(Map("reply_says_unfinished" -> Wire.Answer.Noul(if body.contains("still there") then 0.9 else 0.1)))
        else
          val told = body.contains("The work isn't done")
          val steps = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("steps")).flatMap(_.asArray).map(_.size).getOrElse(0)
          val next = if told && steps == 0 then "tool_000" else "end_answer"
          Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)))
      val writer = new Llm:
        def arguments(request: ArgumentsRequest) = ZIO.succeed(LlmResult(Json.Obj("path" -> Json.Str("A"))))
        def reply(request: ReplyRequest) =
          ZIO.succeed(LlmResult(if request.steps.isEmpty then "The selector is still there." else "Removed the selector in A."))
      for
        jev    <- TypeSafeMock.tracked(script)
        tools   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Str("edited"))
        result <- AgentLoop.run("drop the selector", List(edit), tools, writer, AgentLoop.Config(checkDone = true)).provideLayer(jev.decisions)
        off    <- AgentLoop.run("drop the selector", List(edit), tools, writer, AgentLoop.Config()).provideLayer(jev.decisions)
      yield assertTrue(result.reply == "Removed the selector in A.", result.steps.map(_.tool) == Vector("edit_file"),
        off.reply == "The selector is still there.", off.steps.isEmpty)
    },
    test("checkDone: a loop out of steps whose report is a plan of what's left gets another budget and does it") {
      val read = Tool("read_file", "Reads.", schema("path" -> "string"))
      val edit = Tool("edit_file", "Edits.", schema("path" -> "string"))
      val script = TypeSafeMock.MockBehavior.Always: request =>
        val body = request.body.toString
        if body.contains("reply_says_unfinished") then
          Respond(Map("reply_says_unfinished" -> Wire.Answer.Noul(if body.contains("still to change") then 0.9 else 0.1)))
        else
          // reads until told the work isn't done, then edits
          val next =
            if !body.contains("\"tool_000\"") then "end_answer"
            else if body.contains("The work isn't done") then (if body.contains("edit_file\",\"arguments") then "end_answer" else "tool_001")
            else "tool_000"
          Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)))
      val writer = new Llm:
        def arguments(request: ArgumentsRequest) = ZIO.succeed(LlmResult(Json.Obj("path" -> Json.Str(s"F${request.steps.size}"))))
        def reply(request: ReplyRequest) =
          ZIO.succeed(LlmResult(if request.steps.exists(_.tool == "edit_file") then "Changed A." else "Found A; still to change: A."))
      for
        jev    <- TypeSafeMock.tracked(script)
        tools   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Str("ok"))
        result <- AgentLoop.run("change A", List(read, edit), tools, writer, AgentLoop.Config(maxSteps = 3, checkDone = true)).provideLayer(jev.decisions)
      yield assertTrue(result.reply == "Changed A.", result.steps.map(_.tool) == Vector("read_file", "read_file", "read_file", "edit_file"))
    },
    test("checkReply: a reply Jev judges the agent's planning is written again, told so; a report is kept") {
      val script = TypeSafeMock.MockBehavior.Always: request =>
        val body = request.body.toString
        if body.contains("reply_is_planning") then
          Respond(Map("reply_is_planning" -> Wire.Answer.Noul(if body.contains("call open_file.") then 0.9 else 0.1)), Wire.Usage(5, 1))
        else Respond(Map("next_action" -> Wire.Answer.Choice("end_answer", Map("end_answer" -> 0.9), 0.9)), Wire.Usage(10, 1))
      def writer(first: String, asked: Ref[Vector[Option[String]]]) = new Llm:
        def arguments(request: ArgumentsRequest) = ZIO.succeed(LlmResult(Json.Obj()))
        def reply(request: ReplyRequest) =
          asked.updateAndGet(_ :+ request.problem).map(a => LlmResult(if a.size == 1 then first else "The file is A.scala.", 10, 2))
      for
        jev     <- TypeSafeMock.tracked(script)
        noTools  = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.fail(RuntimeException("no tools"))
        asked1  <- Ref.make(Vector.empty[Option[String]])
        planned <- AgentLoop.run("where?", Nil, noTools, writer("We need to call open_file.", asked1), AgentLoop.Config(checkReply = true))
                     .provideLayer(jev.decisions)
        asked2  <- Ref.make(Vector.empty[Option[String]])
        report  <- AgentLoop.run("where?", Nil, noTools, writer("It's in A.scala.", asked2), AgentLoop.Config(checkReply = true))
                     .provideLayer(jev.decisions)
        a1      <- asked1.get
        a2      <- asked2.get
      yield assertTrue(
        planned.reply == "The file is A.scala.", a1 == Vector(None, Some(AgentLoop.PlanningReply)), planned.llmOutputTokens == 4,
        report.reply == "It's in A.scala.", a2 == Vector(None),
      )
    },
    test("background: a check started in the background runs while the loop reads; waiting takes the next decision when it ends") {
      val check = Tool("run_tests", "Runs the tests.", schema("which" -> "string"))
      val read = Tool("read_file", "Reads.", schema("path" -> "string"), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      // start the tests in the background, read while they run, wait, then end
      val script = TypeSafeMock.MockBehavior.Always: request =>
        val body = request.body.toString
        val steps = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("steps")).flatMap(_.asArray).map(_.size).getOrElse(0)
        val next =
          if steps == 0 && !body.contains("Running in the background") then "bg_000"
          else if steps == 0 then "tool_001"
          else if body.contains("Running in the background") then "wait"
          else "end_answer"
        Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)), Wire.Usage(10, 1))
      for
        jev      <- TypeSafeMock.tracked(script)
        seen     <- Ref.make(Vector.empty[Any])
        order    <- Ref.make(Vector.empty[String])
        tools     = new Tools:
                      def call(name: String, arguments: Json.Obj): Task[Json] =
                        order.update(_ :+ s"start $name") *> (if name == "run_tests" then ZIO.sleep(30.seconds) else ZIO.unit) *>
                          order.update(_ :+ s"end $name").as(Json.Str(s"$name done"))
        fiber    <- AgentLoop.run("go", List(check, read), tools, FakeLlm(Json.Obj("which" -> Json.Str("all"), "path" -> Json.Str("A")), seen),
                      AgentLoop.Config(strategy = Strategy(background = true))).provideLayer(jev.decisions).fork
        // the background call's fiber and the read race to start: both have, and the read has ended, before the tests do
        _        <- order.get.repeatUntil(o => o.contains("end read_file") && o.contains("start run_tests"))
        mid      <- order.get
        _        <- TestClock.adjust(30.seconds)
        result   <- fiber.join
      yield assertTrue(
        mid.toSet == Set("start run_tests", "start read_file", "end read_file"),
        result.steps.map(_.tool) == Vector("read_file", "run_tests"),
        result.steps(1).outcome.toString.contains("ranInBackgroundFromStep"),
        result.ending == Ending.Answered,
      )
    },
    test("workflows: one writing, several calls in order, stopping at the first that fails") {
      val edit = Tool("apply_patch", "Edits.", schema("patch" -> "string"))
      val check = Tool("run_tests", "Runs the tests.", schema("which" -> "string"))
      val writing = new Llm:
        def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] = ZIO.fail(RuntimeException("not asked"))
        def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult("ok", 1, 1))
        override def workflow(request: WorkflowRequest): Task[LlmResult[List[PlannedCall]]] =
          ZIO.succeed(LlmResult(List(PlannedCall("apply_patch", Json.Obj("patch" -> Json.Str("x"))),
            PlannedCall("run_tests", Json.Obj("which" -> Json.Str("all"))), PlannedCall("apply_patch", Json.Obj("patch" -> Json.Str("y")))), 5, 5))
      for
        jev    <- TypeSafeMock.tracked(choose("workflow"), choose("end_answer"))
        result <- AgentLoop.run("go", List(edit, check), (name, _) => if name == "run_tests" then ZIO.fail(RuntimeException("FAILED")) else ZIO.succeed(Json.Str("1")),
                    writing, AgentLoop.Config(strategy = Strategy(workflows = true))).provideLayer(jev.decisions)
        asked  <- jev.requests
      yield assertTrue(
        result.steps.map(_.tool) == Vector("apply_patch", "run_tests"), result.steps(1).outcome.isInstanceOf[Outcome.Failed],
        criteria(asked).head.exists(_._1 == "workflow"), result.llmCalls == 2,
      )
    },
    test("batch: one writing, calls of any tools at the same time, each a step, one step of the budget; what can't be called isn't") {
      val read = Tool("read_file", "Reads.", schema("path" -> "string"), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      val fetch = Tool("web_fetch", "Fetches a page.", schema("url" -> "string"), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      val edit = Tool("edit_file", "Edits.", schema("path" -> "string"))
      val writing = new Llm:
        def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] = ZIO.fail(RuntimeException("not asked"))
        def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult("ok", 1, 1))
        override def workflow(request: WorkflowRequest): Task[LlmResult[List[PlannedCall]]] =
          ZIO.succeed(LlmResult(List(PlannedCall("read_file", Json.Obj("path" -> Json.Str("A"))),
            PlannedCall("web_fetch", Json.Obj("url" -> Json.Str("https://x"))), PlannedCall("edit_file", Json.Obj("path" -> Json.Str("C"))),
            PlannedCall("read_file", Json.Obj("nope" -> Json.Str("D"))), PlannedCall("rm_rf", Json.Obj())).filter(_ => request.concurrent), 5, 5))
      for
        jev     <- TypeSafeMock.tracked(choose("batch"), choose("end_answer"))
        started <- Ref.make(0)
        most    <- Ref.make(0)
        result  <- AgentLoop.run("read A, fetch x, edit C", List(read, fetch, edit), (name, _) =>
                     (started.updateAndGet(_ + 1).flatMap(n => most.update(_ max n)) *> ZIO.sleep(50.millis) *> started.update(_ - 1))
                       .as(Json.Str(name)), writing, AgentLoop.Config(maxSteps = 2, strategy = Strategy(batch = true)))
                     .provideLayer(jev.decisions).fork.flatMap(f => TestClock.adjust(1.second) *> f.join)
        asked   <- jev.requests
        atOnce  <- most.get
      yield assertTrue(
        criteria(asked).head.exists(_._1 == "batch"),
        result.steps.map(s => (s.tool, s.outcome.isInstanceOf[Outcome.Succeeded])) ==
          Vector(("read_file", true), ("web_fetch", true), ("edit_file", true), ("read_file", false), ("rm_rf", false)),
        atOnce == 3, result.llmCalls == 2,
        // five steps, one of the budget's two: the next decision still offers the tools
        criteria(asked)(1).exists(_._1 == "batch"),
      )
    },
    test("exploreLimit: a change request that has only read is told to act with ActReserve steps left, ready or not") {
      val read = Tool("read_file", "Reads.", schema("path" -> "string"), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      val edit = Tool("edit_file", "Edits.", schema("path" -> "string"))
      def script(change: Double) = TypeSafeMock.MockBehavior.Always: request =>
        val body = request.body.toString
        val steps = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("steps")).flatMap(_.asArray).getOrElse(Chunk.empty)
        val next = if steps.exists(_.toString.contains("edit_file")) then "end_answer" else if body.contains("\"tool_000\"") then "tool_000"
                   else if body.contains("\"tool_001\"") then "tool_001" else "end_answer"
        Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9), "ready_to_act" -> Wire.Answer.Noul(0.1),
          "asks_change" -> Wire.Answer.Noul(change)))
      val writer = FakeLlm(Json.Obj("path" -> Json.Str("A")), Unsafe.unsafely(Ref.unsafe.make(Vector.empty[Any])))
      for
        change   <- TypeSafeMock.tracked(script(0.9))
        acted    <- AgentLoop.run("change A", List(read, edit), (_, _) => ZIO.succeed(Json.Str("t")), writer,
                      AgentLoop.Config(maxSteps = 10, exploreLimit = 6)).provideLayer(change.decisions)
        asked    <- change.requests
        question <- TypeSafeMock.tracked(script(0.1))
        read     <- AgentLoop.run("what is A?", List(read, edit), (_, _) => ZIO.succeed(Json.Str("t")), writer,
                      AgentLoop.Config(maxSteps = 10, exploreLimit = 6)).provideLayer(question.decisions)
      yield assertTrue(
        // 6 reads, then readiness and asks_change; 7 reads leave 3 steps: the edit
        acted.steps.map(_.tool) == Vector.fill(7)("read_file") :+ "edit_file",
        asked.count(_.body.toString.contains("asks_change")) == 1,
        // a question isn't pushed to edit
        read.steps.forall(_.tool == "read_file"),
      )
    },
    test("checkDone: an ending after an edit no check has run since goes on to check it, without writing a reply first") {
      val edit = Tool("apply_patch", "Edits.", schema("patch" -> "string"))
      val check = Tool("compile", "Compiles.", Json.Obj("type" -> Json.Str("object")))
      for
        jev     <- TypeSafeMock.tracked(choose("tool_000"), choose("end_answer"), choose("tool_001"), choose("end_answer"),
                     Respond(Map("reply_says_unfinished" -> Wire.Answer.Noul(0.1))))
        replies <- Ref.make(0)
        writing  = new Llm:
                     def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] = ZIO.succeed(LlmResult(Json.Obj("patch" -> Json.Str("x")), 1, 1))
                     def reply(request: ReplyRequest): Task[LlmResult[String]] = replies.update(_ + 1).as(LlmResult("done", 1, 1))
        result  <- AgentLoop.run("change A", List(edit, check), (_, _) => ZIO.succeed(Json.Str("ok")), writing,
                     AgentLoop.Config(checkDone = true)).provideLayer(jev.decisions)
        n       <- replies.get
      yield assertTrue(result.steps.map(_.tool) == Vector("apply_patch", "compile"), n == 1, result.ending == Ending.Answered)
    },
    test("a burst of reset connections (five in a row) is retried through, with backoff") {
      val reset = TypeSafeMock.MockBehavior.Fail(com.jamesward.zio_decisions.Decisions.Error.Transport(RuntimeException("Connection reset by peer")))
      for
        jev      <- TypeSafeMock.tracked(reset, reset, reset, reset, reset, choose("end_answer"))
        seen     <- Ref.make(Vector.empty[Any])
        fiber    <- AgentLoop.run("go", List(listOrders), tools0, FakeLlm(Json.Obj(), seen)).provideLayer(jev.decisions).fork
        _        <- TestClock.adjust(1.minute)
        result   <- fiber.join
        requests <- jev.requests
      yield assertTrue(result.ending == Ending.Answered, requests.size == 6)
    },
    test("a request Jev rejects as too long is made again, smaller") {
      for
        jev      <- TypeSafeMock.tracked(
                      TypeSafeMock.MockBehavior.Fail(com.jamesward.zio_decisions.Decisions.Error.BadRequest("""{"detail":{"error_type":"max_tokens_exceeded"}}""")),
                      choose("end_answer"))
        seen     <- Ref.make(Vector.empty[Any])
        result   <- AgentLoop.run("go", List(listOrders), tools0, FakeLlm(Json.Obj(), seen), AgentLoop.Config(requestBudget = 20000),
                      context = Some("c" * 50000)).provideLayer(jev.decisions)
        requests <- jev.requests
      yield assertTrue(result.ending == Ending.Answered, requests.size == 2,
        requests(1).body.toJson.length < requests(0).body.toJson.length, requests(1).body.toJson.length <= 15000)
    },
    test("ready-made calls bind short values only: a log in an earlier result isn't offered as an argument") {
      val run = Tool("run_tests", "Runs the tests.", schema("pattern" -> "string"))
      val turns = Tool("transcript_turns", "Lists a log's turns.", schema("log" -> "string"))
      val open = Tool("open_file", "Opens a file.", schema("path" -> "string"))
      // the test run's result names a file (short: bound) and carries its log (long: not bound)
      val tools: Tools = (name, _) => ZIO.succeed(if name == "run_tests" then
        Json.Obj("path" -> Json.Str("src/A.scala"), "log" -> Json.Str("+ a test passed\n" * 400)) else Json.Obj())
      for
        jev      <- TypeSafeMock.tracked(choose("tool_000"), choose("end_answer"))
        seen     <- Ref.make(Vector.empty[Any])
        _        <- AgentLoop.run("run the tests", List(run, turns, open), tools, FakeLlm(Json.Obj("pattern" -> Json.Str("*")), seen),
                      AgentLoop.Config(strategy = AgentLoop.Strategy(boundCalls = true))).provideLayer(jev.decisions)
        requests <- jev.requests
        second    = requests(1).body.toJson
      yield assertTrue(second.contains("\"call_tool_with\""), second.contains("src/A.scala"), !second.contains("a test passed\\n+ a test passed"))
    },
    test("options too long for Jev are made fewer on the retry; too long however small, the loop ends with a reply, not a failure") {
      // forty tools with long descriptions: their options are most of the request
      val many = (1 to 40).toList.map(i => Tool(f"tool_$i%02d", s"Tool $i. " + ("It does a great many things, at length. " * 40),
        schema("x" -> "string")))
      val tooLong = TypeSafeMock.MockBehavior.Fail(com.jamesward.zio_decisions.Decisions.Error.BadRequest("""{"detail":{"error_type":"max_tokens_exceeded"}}"""))
      for
        once     <- TypeSafeMock.tracked(tooLong, choose("end_answer"))
        seen     <- Ref.make(Vector.empty[Any])
        r1       <- AgentLoop.run("go", many, tools0, FakeLlm(Json.Obj(), seen), AgentLoop.Config(requestBudget = 60000)).provideLayer(once.decisions)
        asked    <- once.requests
        always   <- TypeSafeMock.tracked(tooLong, tooLong, tooLong, tooLong)
        r2       <- AgentLoop.run("go", many, tools0, FakeLlm(Json.Obj(), seen), AgentLoop.Config(requestBudget = 60000)).provideLayer(always.decisions)
        tried    <- always.requests
      yield assertTrue(
        r1.ending == Ending.Answered, asked.size == 2,
        asked(1).body.toJson.length < asked(0).body.toJson.length * 7 / 10,
        // three smaller requests and the least there is, then a reply from what the loop has
        r2.ending == Ending.Answered, tried.size == 4,
      )
    },
    test("groupAbove: a large catalog's grouped tools are offered as their group; opening one (no step) offers its tools") {
      val db = AgentLoop.ToolGroup("databases", "SQL and connections")
      val dbg = AgentLoop.ToolGroup("debugger", "Debug sessions")
      val core = List(Tool("read_file", "Reads.", schema("path" -> "string")), Tool("search_text", "Searches.", schema("q" -> "string")))
      val query = Tool("execute_sql_query", "Runs SQL.", schema("sql" -> "string"), group = Some(db))
      val grouped = query :: (1 to 30).toList.map(i => Tool(s"xdebug_$i", s"Debugger tool $i.", schema("x" -> "string"), group = Some(dbg)))
      // Jev opens the databases group, calls its query tool, then ends
      val script = TypeSafeMock.MockBehavior.Always: request =>
        val criteria = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).flatMap(_.get("next_action"))
          .flatMap(_.asObject).flatMap(_.get("criteria")).flatMap(_.asObject).toList.flatMap(_.fields)
        def id(p: Json => Boolean) = criteria.collectFirst { case (k, v) if p(v) => k }
        val steps = request.body.toString.contains("\"step\":1")
        val next = if steps then "end_answer"
                   else id(_.toString.contains("\"tool\":\"execute_sql_query\"")).orElse(id(_.toString.contains("\"group\":\"databases\""))).getOrElse("end_answer")
        Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)))
      for
        jev      <- TypeSafeMock.tracked(script)
        seen     <- Ref.make(Vector.empty[Any])
        result   <- AgentLoop.run("how many orders in the db?", core ++ grouped, tools0, FakeLlm(Json.Obj("sql" -> Json.Str("select 1")), seen),
                      AgentLoop.Config(groupAbove = 24)).provideLayer(jev.decisions)
        asked    <- jev.requests
        first     = asked.head.body.toString
        second    = asked(1).body.toString
        flat     <- TypeSafeMock.tracked(choose("end_answer"))
        _        <- AgentLoop.run("go", core ++ grouped.take(5), tools0, FakeLlm(Json.Obj(), seen), AgentLoop.Config(groupAbove = 24))
                      .provideLayer(flat.decisions)
        small    <- flat.requests
      yield assertTrue(
        // the first decision: the ungrouped tools, and each group as one option naming its tools, not the tools
        first.contains("\"tool\":\"read_file\""), first.contains("\"group\":\"databases\""), first.contains("\"group\":\"debugger\""),
        !first.contains("\"tool\":\"xdebug_1\""), !first.contains("Debugger tool 1."), first.contains("xdebug_30"),
        // opened, its tools are offered; the step budget isn't spent
        second.contains("\"tool\":\"execute_sql_query\""), !second.contains("\"group\":\"databases\""), second.contains("\"group\":\"debugger\""),
        result.steps.map(_.tool) == Vector("execute_sql_query"), second.contains("\"stepsRemaining\":12"),
        // a small catalog is offered whole
        small.head.body.toString.contains("\"tool\":\"xdebug_1\""), !small.head.body.toString.contains("open_group"),
      )
    },
    test("briefArguments: the writer gets only the steps and context Jev judges it needs; asking for more, it gets everything") {
      val read = Tool("read_file", "Reads.", schema("path" -> "string"))
      val edit = Tool("edit_file", "Edits.", schema("path" -> "string"))
      val files: Tools = (name, arguments) => ZIO.succeed(Json.Obj("text" -> Json.Str(s"text of ${arguments.toJson}")))
      // reads A, reads B, edits, ends; of the steps the edit's writer could need, only step 2 (B), and not the context
      def jev = TypeSafeMock.MockBehavior.Always: request =>
        val qs = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields).map(_._1)
        if qs.exists(q => q.startsWith("step_") || q == "context") then
          Respond(qs.map(q => q -> Wire.Answer.Noul(if q == "step_002" then 0.9 else 0.1)).toMap)
        else
          val n = """"step":(\d+)""".r.findAllMatchIn(request.body.toString).map(_.group(1).toInt).maxOption.getOrElse(0)
          val next = n match { case 0 | 1 => "tool_000"; case 2 => "tool_001"; case _ => "end_answer" }
          Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)))
      // writes A, then B, then asks for more on the edit, then writes it
      final class Asking(seen: Ref[Vector[ArgumentsRequest]]) extends Llm:
        def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
          seen.updateAndGet(_ :+ request).map: all =>
            val path = if all.size == 1 then "A" else "B"
            if request.tool.name == "edit_file" && request.problem.isEmpty then
              LlmResult(Json.Obj("path" -> Json.Str("?"), AgentLoop.AskForMore -> Json.Str("what's in file A")), 10, 2)
            else LlmResult(Json.Obj("path" -> Json.Str(path)), 10, 2)
        def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult("done", 1, 1))
      for
        mock   <- TypeSafeMock.tracked(jev)
        seen   <- Ref.make(Vector.empty[ArgumentsRequest])
        result <- AgentLoop.run("change B", List(read, edit), files, Asking(seen), AgentLoop.Config(briefArguments = true),
                    context = Some("PROJECT CONTEXT")).provideLayer(mock.decisions)
        asked  <- seen.get
      yield
        val edits = asked.filter(_.tool.name == "edit_file")
        assertTrue(
          // the first read had nothing to brief: the context was offered and Jev judged it not needed
          asked.head.context.isEmpty,
          edits.size == 2,
          edits.head.steps.map(_.number) == Vector(2), edits.head.context.isEmpty,
          edits.head.tool.inputSchema.toJson.contains(AgentLoop.AskForMore),
          // asked for more: everything, told why
          edits(1).steps.map(_.number) == Vector(1, 2), edits(1).context.contains("PROJECT CONTEXT"),
          edits(1).problem.exists(_.contains("what's in file A")),
          result.steps.find(_.tool == "edit_file").exists(s => s.arguments.get(AgentLoop.AskForMore).isEmpty),
        )
    },
    test("briefArguments: an edit's writer keeps what the reads found; one that needs what the work lacks isn't called, and the step says what to get") {
      val read = Tool("read_file", "Reads.", schema("path" -> "string"), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      val edit = Tool("edit_file", "Edits.", schema("path" -> "string"))
      def files(calls: Ref[Vector[String]]): Tools = (name, arguments) => calls.update(_ :+ name).as(Json.Obj("text" -> Json.Str("class A")))
      // reads A, edits, ends; Jev judges no step needed, which an edit's reads overrule
      def jev = TypeSafeMock.MockBehavior.Always: request =>
        val qs = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields).map(_._1)
        if qs.exists(q => q.startsWith("step_") || q == "context") then Respond(qs.map(q => q -> Wire.Answer.Noul(0.1)).toMap)
        else
          val n = """"step":(\d+)""".r.findAllMatchIn(request.body.toString).map(_.group(1).toInt).maxOption.getOrElse(0)
          val next = n match { case 0 => "tool_000"; case 1 => "tool_001"; case _ => "end_answer" }
          Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)))
      final class Needing(seen: Ref[Vector[ArgumentsRequest]]) extends Llm:
        def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
          seen.update(_ :+ request).as:
            if request.tool.name == "edit_file" then LlmResult(Json.Obj(AgentLoop.AskForMore -> Json.Str("the definition of B")), 10, 2)
            else LlmResult(Json.Obj("path" -> Json.Str("A")), 10, 2)
        def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult("done", 1, 1))
      for
        mock   <- TypeSafeMock.tracked(jev)
        seen   <- Ref.make(Vector.empty[ArgumentsRequest])
        calls  <- Ref.make(Vector.empty[String])
        result <- AgentLoop.run("change A", List(read, edit), files(calls), Needing(seen), AgentLoop.Config(briefArguments = true))
                    .provideLayer(mock.decisions)
        asked  <- seen.get
        called <- calls.get
      yield
        val edits = asked.filter(_.tool.name == "edit_file")
        assertTrue(
          // the read is kept for the edit though Jev judged it not needed; with nothing left out, there's no second writing
          edits.size == 1, edits.head.steps.map(_.number) == Vector(1),
          called == Vector("read_file"),
          result.steps.exists(s => s.tool == "edit_file" && (s.outcome match
            case Outcome.Failed(e) => e.contains("the definition of B") && e.contains("Get that first")
            case _                 => false)),
        )
    },
    test("briefArguments: a refused project-context question keeps the context and still briefs the steps") {
      val read = Tool("read_file", "Reads.", schema("path" -> "string"))
      val edit = Tool("edit_file", "Edits.", schema("path" -> "string"))
      val files: Tools = (name, arguments) => ZIO.succeed(Json.Obj("text" -> Json.Str(s"text of ${arguments.toJson}")))
      // reads A, reads B, edits, ends; Jev refuses any request that asks about the context, and of the steps judges only
      // step 2 (B) needed
      def jev = TypeSafeMock.MockBehavior.AlwaysOr: request =>
        val qs = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields).map(_._1)
        if qs.contains("context") then Left(Decisions.Error.Refused(Decisions.QuestionId("context")))
        else if qs.exists(_.startsWith("step_")) then
          Right(Respond(qs.map(q => q -> Wire.Answer.Noul(if q == "step_002" then 0.9 else 0.1)).toMap))
        else
          val n = """"step":(\d+)""".r.findAllMatchIn(request.body.toString).map(_.group(1).toInt).maxOption.getOrElse(0)
          val next = n match { case 0 | 1 => "tool_000"; case 2 => "tool_001"; case _ => "end_answer" }
          Right(Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9))))
      final class Writing(seen: Ref[Vector[ArgumentsRequest]]) extends Llm:
        def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
          seen.updateAndGet(_ :+ request).map(all => LlmResult(Json.Obj("path" -> Json.Str(if all.size == 1 then "A" else "B")), 10, 2))
        def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult("done", 1, 1))
      for
        mock   <- TypeSafeMock.tracked(jev)
        seen   <- Ref.make(Vector.empty[ArgumentsRequest])
        _      <- AgentLoop.run("change B", List(read, edit), files, Writing(seen), AgentLoop.Config(briefArguments = true),
                    context = Some("PROJECT CONTEXT")).provideLayer(mock.decisions)
        asked  <- seen.get
      yield
        val edits = asked.filter(_.tool.name == "edit_file")
        assertTrue(
          edits.size == 1,
          // the steps are still briefed: only step 2, though the context question was refused
          edits.head.steps.map(_.number) == Vector(2),
          // the refused context is kept
          edits.head.context.contains("PROJECT CONTEXT"),
          asked.forall(_.context.contains("PROJECT CONTEXT")),
        )
    },
    test("short parameters: name and type, an enum's values, required ones marked") {
      val schema = Json.Obj(
        "type" -> Json.Str("object"),
        "properties" -> Json.Obj(
          "file_path" -> Json.Obj("type" -> Json.Str("string")),
          "kind" -> Json.Obj("type" -> Json.Str("string"), "enum" -> Json.Arr(Json.Str("IN"), Json.Str("OUT"))),
          "limit" -> Json.Obj("type" -> Json.Str("integer")),
          "x" -> Json.Obj(),
        ),
        "required" -> Json.Arr(Json.Str("file_path"), Json.Str("kind")),
      )
      assertTrue(AgentLoop.parameters(schema) ==
        List("file_path: string (required)", "kind: IN|OUT (required)", "limit: integer", "x: any"))
    },
    test("a chosen tool gets its arguments from the generative model, is called, and its result is shown to Jev") {
      for
        (jev, calls, seen, llm) <- setup(choose("tool_000"), choose("end_answer"))
        result   <- AgentLoop.run("orders of C1?", List(listOrders, projectInfo), tools(calls), llm).provideLayer(jev.decisions)
        called   <- calls.get
        requests <- jev.requests
        second    = state(requests(1))
        step      = field(second, "steps").flatMap(_.asArray).flatMap(_.headOption)
        llmSeen  <- seen.get
      yield assertTrue(
        result.llmLatencyMs >= 0, result.toolLatencyMs >= 0, result.decisionLatencyMs >= 0,
        called == Vector("list_orders" -> Json.Obj("customerId" -> Json.Str("C1"))),
        result.steps == Vector(Step(1, "list_orders", Json.Obj("customerId" -> Json.Str("C1")),
          Outcome.Succeeded(Json.Obj("orders" -> Json.Arr(Json.Str("O1"), Json.Str("O2")))))),
        field(second, "request").contains(Json.Str("orders of C1?")),
        step.flatMap(field(_, "outcome")).contains(Json.Str("succeeded")),
        step.flatMap(field(_, "result")).exists(_.toJson.contains("O2")),
        step.flatMap(field(_, "arguments")).contains(Json.Obj("customerId" -> Json.Str("C1"))),
        // the tool's option now counts its call
        criteria(requests).apply(1).head._2.toJson.contains("\"earlierCalls\":1"),
        llmSeen.collect { case r: ArgumentsRequest => r.tool.name } == Vector("list_orders"),
        llmSeen.collect { case r: ReplyRequest => (r.ending, r.steps.size) } == Vector((Ending.Answered, 1)),
        result.reply == "Answered after 1 steps",
        result.decisions.size == 2,
        result.decisionUsage.inputTokens == 200,
        result.llmCalls == 2,
        result.llmInputTokens == 30L,
      )
    },
    test("arguments that can't be written are a failed step without a call, and the loop goes on") {
      val broken = new Llm:
        def arguments(request: ArgumentsRequest) = ZIO.fail(RuntimeException("decode failed"))
        def reply(request: ReplyRequest) = ZIO.succeed(LlmResult(s"${request.ending}: ${request.steps.size}"))
      for
        (jev, calls, _, _) <- setup(choose("tool_000"), choose("end_unable"))
        result   <- AgentLoop.run("orders?", List(listOrders), tools(calls), broken).provideLayer(jev.decisions)
        called   <- calls.get
        requests <- jev.requests
      yield assertTrue(
        called.isEmpty,
        result.steps.map(_.outcome) == Vector(Outcome.Failed("the arguments could not be written, so the tool wasn't called: decode failed")),
        field(state(requests(1)), "steps").exists(_.toJson.contains("could not be written")),
        result.reply == "Unable: 1",
      )
    },
    test("arguments repeating a failed call are rewritten; if they still repeat it, the tool isn't called again") {
      val stuck = new Llm:
        def arguments(request: ArgumentsRequest) = ZIO.succeed(LlmResult(Json.Obj("customerId" -> Json.Str("nope")), 1, 1))
        def reply(request: ReplyRequest) = ZIO.succeed(LlmResult("done"))
      for
        (jev, _, _, _) <- setup(choose("tool_000"), choose("tool_000"), choose("end_unable"))
        calls    <- Ref.make(0)
        failing   = new Tools:
                      def call(name: String, arguments: Json.Obj) = calls.update(_ + 1) *> ZIO.fail(RuntimeException("no such customer"))
        result   <- AgentLoop.run("orders?", List(listOrders), failing, stuck).provideLayer(jev.decisions)
        made     <- calls.get
      yield assertTrue(
        made == 1,
        result.steps.map(_.outcome) == Vector(
          Outcome.Failed("no such customer"),
          Outcome.Failed("not called again: the same arguments already failed in step 1 (no such customer)"),
        ),
        // one write for the first call; for the second, the first write and two rewrites
        result.llmCalls == 1 + 3 + 1,
      )
    },
    test("a rewrite is told which arguments repeated a failure, and a different one is called") {
      for
        seen   <- Ref.make(Vector.empty[ArgumentsRequest])
        count  <- Ref.make(0)
        llm     = new Llm:
                    def arguments(request: ArgumentsRequest) =
                      seen.update(_ :+ request) *> count.getAndUpdate(_ + 1).map: n =>
                        LlmResult(Json.Obj("customerId" -> Json.Str(if n < 2 then "nope" else "C1")))
                    def reply(request: ReplyRequest) = ZIO.succeed(LlmResult("done"))
        (jev, calls, _, _) <- setup(choose("tool_000"), choose("tool_000"), choose("end_answer"))
        broken  = new Tools:
                    def call(name: String, arguments: Json.Obj) =
                      calls.update(_ :+ (name -> arguments)) *>
                        (if arguments.toJson.contains("nope") then ZIO.fail(RuntimeException("no such customer")) else ZIO.succeed(Json.Obj()))
        result <- AgentLoop.run("orders?", List(listOrders), broken, llm).provideLayer(jev.decisions)
        asked  <- seen.get
        called <- calls.get
      yield assertTrue(
        called.map(_._2.toJson) == Vector("""{"customerId":"nope"}""", """{"customerId":"C1"}"""),
        asked.map(_.rejected.size) == Vector(0, 0, 1),
        asked(1).failures.map(_.number) == Vector(1),
        result.steps.last.outcome == Outcome.Succeeded(Json.Obj()),
      )
    },
    test("a tool without inputs is called without asking for arguments") {
      for
        (jev, calls, seen, llm) <- setup(choose("tool_001"), choose("end_answer"))
        _      <- AgentLoop.run("project?", List(listOrders, projectInfo), tools(calls), llm).provideLayer(jev.decisions)
        called <- calls.get
        llmSeen <- seen.get
      yield assertTrue(
        called == Vector("get_project_info" -> Json.Obj()),
        llmSeen.collect { case r: ArgumentsRequest => r }.isEmpty,
      )
    },
    test("a failed call is a step Jev sees, and the loop can end as unable") {
      for
        (jev, calls, seen, llm) <- setup(choose("tool_001"), choose("end_unable"))
        result   <- AgentLoop.run("say hello", List(listOrders, projectInfo), tools(calls), llm).provideLayer(jev.decisions)
        requests <- jev.requests
        step      = field(state(requests(1)), "steps").flatMap(_.asArray).flatMap(_.headOption)
        llmSeen  <- seen.get
      yield assertTrue(
        result.steps.map(_.outcome) == Vector(Outcome.Failed("Tool 'get_project_info' not found")),
        step.flatMap(field(_, "outcome")).contains(Json.Str("failed")),
        step.flatMap(field(_, "error")).contains(Json.Str("Tool 'get_project_info' not found")),
        criteria(requests).apply(1).head._2.toJson.contains("\"earlierCalls\":0"),
        criteria(requests).apply(1)(1)._2.toJson.contains("\"earlierFailures\":1"),
        result.ending == Ending.Unable,
        llmSeen.collect { case r: ReplyRequest => r.ending } == Vector(Ending.Unable),
      )
    },
    test("a decision model that refuses the next step (OpenAI's refusal) is asked again with less; refusing still, the loop replies") {
      final class Refusing(asked: Ref[Int]) extends DecisionModel:
        val provider = "refusing"
        val defaultModel = Decisions.ModelId("refusing")
        def decide(request: DecisionModel.Request) =
          asked.update(_ + 1) *> ZIO.fail(Decisions.Error.Refused(Decisions.QuestionId("next_action")))
      for
        (_, calls, seen, llm) <- setup()
        asked  <- Ref.make(0)
        result <- AgentLoop.run("orders?", List(listOrders), tools(calls), llm).provideLayer(ZLayer.succeed[DecisionModel](Refusing(asked)))
        n      <- asked.get
        made   <- calls.get
      yield assertTrue(result.ending == Ending.Answered, n == 2, made.isEmpty)
    },
    test("without tools, only the endings are offered") {
      for
        (jev, calls, _, llm) <- setup(choose("end_answer"))
        result  <- AgentLoop.run("say hello", Nil, tools(calls), llm).provideLayer(jev.decisions)
        options <- jev.requests.map(criteria)
      yield assertTrue(options.head.map(_._1) == List("end_answer", "end_unable"), result.ending == Ending.Answered)
    },
    test("after the last step of the budget only the endings are offered, so the loop always replies") {
      for
        (jev, calls, _, llm) <- setup(choose("tool_000"), choose("tool_000"), choose("end_answer"))
        result  <- AgentLoop.run("orders?", List(listOrders), tools(calls), llm, AgentLoop.Config(maxSteps = 2)).provideLayer(jev.decisions)
        options <- jev.requests.map(criteria)
      yield assertTrue(
        options.map(_.map(_._1)) == List(
          List("tool_000", "end_answer", "end_unable"),
          List("tool_000", "end_answer", "end_unable"),
          List("end_answer", "end_unable"),
        ),
        result.steps.size == 2,
        result.ending == Ending.BudgetExhausted,
      )
    },
    test("long results are cut in what Jev sees, but kept whole in the steps") {
      val big = Json.Str("x" * 100)
      for
        (jev, _, _, llm) <- setup(choose("tool_000"), choose("end_answer"))
        result   <- AgentLoop.run("orders?", List(listOrders), (_, _) => ZIO.succeed(big), llm, AgentLoop.Config(resultPreview = 10))
                      .provideLayer(jev.decisions)
        requests <- jev.requests
        preview   = field(state(requests(1)), "steps").flatMap(_.asArray).flatMap(_.headOption).flatMap(field(_, "result"))
      yield assertTrue(
        preview.contains(Json.Str("\"xxxxxxxxx… (92 more characters)")),
        result.steps.head.outcome == Outcome.Succeeded(big),
      )
    },
    test("more tools than a choice can offer, or duplicate names, fail before Jev is asked") {
      val many = (1 to (MaxTools + 1)).map(i => Tool(s"t$i", "", Json.Obj()))
      for
        (jev, calls, _, llm) <- setup()
        tooMany  <- AgentLoop.run("x", many, tools(calls), llm).provideLayer(jev.decisions).flip
        dupes    <- AgentLoop.run("x", List(listOrders, listOrders), tools(calls), llm).provideLayer(jev.decisions).flip
        requests <- jev.requestCount
      yield assertTrue(
        tooMany == TooManyTools(MaxTools + 1),
        dupes.getMessage.contains("tool names must be unique: list_orders"),
        requests == 0,
      )
    },
  )
