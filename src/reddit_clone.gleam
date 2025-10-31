// src/reddit_clone.gleam
// Main simulation orchestrator with Zipf distribution

import client_simulator
import gleam/erlang/process
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import reddit_engine
import types.{
  type EngineMessage, type SimulatorConfig, type SubredditName, CreateSubreddit,
  GetStats, RegisterUser, SimulatorConfig, StatsData, Success,
}
import utils

pub fn small_test_config() -> SimulatorConfig {
  SimulatorConfig(
    num_users: 10,
    num_subreddits: 3,
    simulation_duration_ms: 10_000,
    zipf_alpha: 1.2,
    connection_probability: 0.95,
    post_probability: 0.05,
    comment_probability: 0.15,
    vote_probability: 0.25,
    dm_probability: 0.02,
    repost_probability: 0.1,
  )
}

pub fn default_config() -> SimulatorConfig {
  SimulatorConfig(
    num_users: 100,
    num_subreddits: 20,
    simulation_duration_ms: 30_000,
    zipf_alpha: 1.2,
    connection_probability: 0.95,
    post_probability: 0.03,
    comment_probability: 0.1,
    vote_probability: 0.2,
    dm_probability: 0.01,
    repost_probability: 0.05,
  )
}

pub fn run_simulation(config: SimulatorConfig) -> Nil {
  io.println("\n🚀 Starting Reddit Clone Simulation (Actor Model)")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  print_config(config)

  let start_time = utils.current_timestamp()

  io.println("\n⚙️  Starting engine actor...")
  let assert Ok(engine_subject) = reddit_engine.start()
  io.println("✓ Engine actor started successfully")

  // ✅ FIX: Register admin user FIRST and wait longer
  io.println("\n👤 Registering admin user...")
  process.send(engine_subject, RegisterUser("admin"))
  utils.sleep(500)
  // Wait for registration to complete
  io.println("✓ Admin user registered")

  io.println("\n📁 Creating subreddits...")
  let subreddit_names = create_subreddits(engine_subject, config.num_subreddits)
  io.println(
    "✓ Created " <> int.to_string(list.length(subreddit_names)) <> " subreddits",
  )

  // ✅ Wait for subreddits to be fully created
  utils.sleep(1000)

  io.println(
    "\n👥 Spawning " <> int.to_string(config.num_users) <> " client actors...",
  )
  let _clients = start_clients(engine_subject, config, subreddit_names)
  io.println("✓ All client actors spawned")

  io.println(
    "\n🎮 Simulation running for "
    <> utils.format_duration(config.simulation_duration_ms)
    <> "...\n",
  )

  print_periodic_stats(engine_subject, config.simulation_duration_ms / 3)
  print_periodic_stats(engine_subject, config.simulation_duration_ms / 3)
  print_periodic_stats(engine_subject, config.simulation_duration_ms / 3)

  let end_time = utils.current_timestamp()
  let duration = end_time - start_time

  io.println("\n📊 Final Statistics")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  print_final_stats(engine_subject)

  io.println("\n✅ Simulation completed in " <> utils.format_duration(duration))
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  Nil
}

fn print_config(config: SimulatorConfig) -> Nil {
  io.println("\n📋 Configuration:")
  io.println("  • Users: " <> int.to_string(config.num_users))
  io.println("  • Subreddits: " <> int.to_string(config.num_subreddits))
  io.println(
    "  • Duration: " <> utils.format_duration(config.simulation_duration_ms),
  )
  io.println("  • Zipf α: " <> float.to_string(config.zipf_alpha))
  io.println("  • Activity probabilities:")
  io.println("    - Post: " <> float.to_string(config.post_probability))
  io.println("    - Comment: " <> float.to_string(config.comment_probability))
  io.println("    - Vote: " <> float.to_string(config.vote_probability))
}

fn create_subreddits(
  engine_subject: process.Subject(EngineMessage),
  num_subreddits: Int,
) -> List(SubredditName) {
  list.range(1, num_subreddits)
  |> list.map(fn(i) {
    let name = utils.random_subreddit_name() <> "_" <> int.to_string(i)
    let desc = "A community for discussing " <> name

    // ✅ Now "admin" exists in the users dict!
    process.send(engine_subject, CreateSubreddit("admin", name, desc))

    utils.sleep(100)
    // Increased sleep time
    name
  })
}

fn start_clients(
  engine_subject: process.Subject(EngineMessage),
  config: SimulatorConfig,
  subreddits: List(SubredditName),
) -> List(process.Subject(client_simulator.ClientControl)) {
  list.range(1, config.num_users)
  |> list.map(fn(i) {
    // Small stagger to prevent overwhelming
    case i % 20 {
      0 -> utils.sleep(100)
      _ -> Nil
    }
    client_simulator.start_client(engine_subject, config, i, subreddits)
  })
}

fn print_periodic_stats(
  engine_subject: process.Subject(EngineMessage),
  wait_ms: Int,
) -> Nil {
  utils.sleep(wait_ms)

  let response_subject = process.new_subject()
  process.send(engine_subject, GetStats(response_subject))

  case process.receive(response_subject, 2000) {
    Ok(Success(StatsData(stats))) -> {
      io.println("📈 Current Stats:")
      io.println("  • Active users: " <> int.to_string(stats.active_sessions))
      io.println("  • Total posts: " <> int.to_string(stats.total_posts))
      io.println("  • Total comments: " <> int.to_string(stats.total_comments))
      io.println(
        "  • Total votes: "
        <> int.to_string(stats.performance_metrics.total_votes_cast),
      )
      io.println(
        "  • Messages processed: "
        <> int.to_string(stats.performance_metrics.messages_processed),
      )
      io.println("")
    }
    _ -> io.println("⚠️  Failed to get stats\n")
  }
}

fn print_final_stats(engine_subject: process.Subject(EngineMessage)) -> Nil {
  let response_subject = process.new_subject()
  process.send(engine_subject, GetStats(response_subject))

  case process.receive(response_subject, 2000) {
    Ok(Success(StatsData(stats))) -> {
      io.println("• Total users: " <> int.to_string(stats.total_users))
      io.println(
        "• Total subreddits: " <> int.to_string(stats.total_subreddits),
      )
      io.println("• Total posts: " <> int.to_string(stats.total_posts))
      io.println("• Total comments: " <> int.to_string(stats.total_comments))
      io.println("• Total messages: " <> int.to_string(stats.total_messages))
      io.println(
        "• Total votes: "
        <> int.to_string(stats.performance_metrics.total_votes_cast),
      )
      io.println("• Active sessions: " <> int.to_string(stats.active_sessions))
      io.println(
        "• Uptime: " <> int.to_string(stats.uptime_seconds) <> " seconds",
      )

      io.println("\n🏆 Most Popular Subreddits (Zipf Distribution):")
      list.take(stats.most_popular_subreddits, 10)
      |> list.each(fn(pair) {
        let #(name, count) = pair
        io.println("  • " <> name <> ": " <> int.to_string(count) <> " members")
      })

      io.println("\n⚡ Performance Metrics:")
      io.println(
        "  • Posts created: "
        <> int.to_string(stats.performance_metrics.total_posts_created),
      )
      io.println(
        "  • Comments created: "
        <> int.to_string(stats.performance_metrics.total_comments_created),
      )
      io.println(
        "  • Votes cast: "
        <> int.to_string(stats.performance_metrics.total_votes_cast),
      )
      io.println(
        "  • Messages processed: "
        <> int.to_string(stats.performance_metrics.messages_processed),
      )
    }
    _ -> io.println("⚠️  Failed to get final stats")
  }
}

pub fn main() {
  run_simulation(default_config())
}
