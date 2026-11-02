## Guidance

- Sometimes sbt incremental compilation fails with errors like `ClassNotFoundException` - just `./sbta clean` and try again.

## Project facts

- `zio-decisions` (`com.jamesward` %% `zio-decisions`, github.com/jamesward/zio-decisions): a provider-neutral decision-model API (`Decisions`, the `DecisionModel` trait, `Middleware`) with providers in `typesafe/` (TypeSafe AI's Jev) and `openai/` (OpenAI's Decisions API). Everything else, `orchestration/` included, depends only on `DecisionModel`; keep provider wire details inside the provider packages. `SPEC.md` is the design.
- Live integration specs (`TypeSafeIntegrationSpec`, `OpenAIIntegrationSpec`) run when `TYPESAFE_API_KEY` / `OPENAI_API_KEY` are set and are paid. Locally they come from the gitignored `.env` (sbt-dotenv passes it to the forked test JVM); CI uses repository secrets of the same names.

## Agent tooling

- Follow the `zen-of-projects` Skill (extract with `./sbta extractSkillsJars` into the gitignored `.kiro/skills/`); this file records only project-specific facts and exceptions. Read the relevant Skills before working.
- For Scala 3 and ZIO code (writing or reviewing it), follow `zen-of-scala`.
- For design, domain modeling, effects and testing strategy, follow `zen-of-james`.
- MCP server `sbta` (this build's `./sbta --mcp`, configured in `.mcp.json` and `.kiro/settings/mcp.json`, approved in `.claude/settings.json`): use it for all sbt work: `compile`, `test` (`full` for all), `run`, `check` after editing Scala 3 sources, `glob-search` / `inspect` / `symbol-location` for symbols, `sbt-task` for any other command. Its tools are deferred in Claude Code: load them with ToolSearch (search `sbta`). If it's unavailable, say so and use `./sbta <command>`. Logs: `project/target/sbta/mcp.log`.
- Full validation (library): `./sbta "Test / compile; testFull"`. CI runs `./sbta test`, which this build sets to `testFull`. Tests need no Docker; the live specs above are the only paid ones.
- Maintenance routine: `.factory/MAINTENANCE.md` (weekly), following the `zen-of-projects` Skill.
