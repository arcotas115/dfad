// src/reddit_clone/simulator.gleam
// Main simulation orchestrator

import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import reddit_clone/client_simulator
import reddit_clone/engine
import reddit_clone/types.{
  type EngineMessage, type EngineResponse, type EngineStats,
  type SimulatorConfig, type SubredditName, CreateSubreddit, Error, GetStats,
  RegisterUser, SimulatorConfig, StatsData, SubredditCreated, Success,
  UserRegistered,
}
import reddit_clone/utils

// Configuration presets
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

pub fn large_scale_config() -> SimulatorConfig {
  SimulatorConfig(
    num_users: 1000,
    num_subreddits: 100,
    simulation_duration_ms: 60_000,
    zipf_alpha: 1.5,
    connection_probability: 0.9,
    post_probability: 0.02,
    comment_probability: 0.08,
    vote_probability: 0.15,
    dm_probability: 0.005,
    repost_probability: 0.03,
  )
}

pub fn run_simulation(config: SimulatorConfig) -> Nil {
  io.println("\n🚀 Starting Reddit Clone Simulation")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  print_config(config)

  let start_time = utils.current_timestamp()

  // Start the engine
  io.println("\n⚙️  Starting engine...")
  let engine = engine.start()

  // Create subreddits
  io.println("📁 Creating subreddits...")
  let subreddit_names = create_subreddits(engine, config.num_subreddits)

  // Start client simulators
  io.println(
    "👥 Starting " <> int.to_string(config.num_users) <> " client simulators...",
  )
  let clients = start_clients(engine, config, subreddit_names)

  // Run simulation
  io.println(
    "\n🎮 Simulation running for "
    <> utils.format_duration(config.simulation_duration_ms)
    <> "...",
  )

  // Periodically print stats
  print_periodic_stats(engine, config.simulation_duration_ms / 3)
  print_periodic_stats(engine, config.simulation_duration_ms / 3)
  print_periodic_stats(engine, config.simulation_duration_ms / 3)

  // Get final statistics
  let end_time = utils.current_timestamp()
  let duration = end_time - start_time

  io.println("\n📊 Final Statistics")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  print_final_stats(engine)

  io.println("\n✅ Simulation completed in " <> utils.format_duration(duration))
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  Nil
}

fn print_config(config: SimulatorConfig) -> Nil {
  io.println("📋 Configuration:")
  io.println("  • Users: " <> int.to_string(config.num_users))
  io.println("  • Subreddits: " <> int.to_string(config.num_subreddits))
  io.println(
    "  • Duration: " <> utils.format_duration(config.simulation_duration_ms),
  )
  io.println("  • Zipf α: " <> float.to_string(config.zipf_alpha))
  io.println(
    "  • Connection probability: "
    <> float.to_string(config.connection_probability),
  )
  io.println("  • Activity probabilities:")
  io.println("    - Post: " <> float.to_string(config.post_probability))
  io.println("    - Comment: " <> float.to_string(config.comment_probability))
  io.println("    - Vote: " <> float.to_string(config.vote_probability))
  io.println("    - DM: " <> float.to_string(config.dm_probability))
  io.println("    - Repost: " <> float.to_string(config.repost_probability))
}

fn create_subreddits(
  engine: Subject(EngineMessage),
  num_subreddits: Int,
) -> List(SubredditName) {
  list.range(1, num_subreddits)
  |> list.map(fn(i) {
    let name = utils.random_subreddit_name() <> "_" <> int.to_string(i)
    let response_subject = process.new_subject()

    // Use a default admin user for creating subreddits
    process.send(
      engine,
      RegisterUser("admin_" <> int.to_string(i), response_subject),
    )

    case process.receive(response_subject, 1000) {
      Ok(Success(UserRegistered(user_id))) -> {
        let desc = "A community for discussing " <> name
        process.send(
          engine,
          CreateSubreddit(user_id, name, desc, response_subject),
        )

        case process.receive(response_subject, 1000) {
          Ok(Success(SubredditCreated(subreddit_name))) -> subreddit_name
          _ -> name
          // Fallback to name even if creation failed
        }
      }
      _ -> name
      // Fallback
    }
  })
}

fn start_clients(
  engine: Subject(EngineMessage),
  config: SimulatorConfig,
  subreddits: List(SubredditName),
) -> List(Subject(client_simulator.ClientControl)) {
  list.range(1, config.num_users)
  |> list.map(fn(i) {
    client_simulator.start_client(engine, config, i, subreddits)
  })
}

fn print_periodic_stats(engine: Subject(EngineMessage), wait_ms: Int) -> Nil {
  utils.sleep(wait_ms)

  let response_subject = process.new_subject()
  process.send(engine, GetStats(response_subject))

  case process.receive(response_subject, 1000) {
    Ok(Success(StatsData(stats))) -> {
      io.println("\n📈 Current Stats:")
      io.println("  • Active users: " <> int.to_string(stats.active_sessions))
      io.println("  • Total posts: " <> int.to_string(stats.total_posts))
      io.println("  • Total comments: " <> int.to_string(stats.total_comments))
    }
    _ -> io.println("⚠️  Failed to get stats")
  }
}

fn print_final_stats(engine: Subject(EngineMessage)) -> Nil {
  let response_subject = process.new_subject()
  process.send(engine, GetStats(response_subject))

  case process.receive(response_subject, 1000) {
    Ok(Success(StatsData(stats))) -> {
      io.println("• Total users: " <> int.to_string(stats.total_users))
      io.println(
        "• Total subreddits: " <> int.to_string(stats.total_subreddits),
      )
      io.println("• Total posts: " <> int.to_string(stats.total_posts))
      io.println("• Total comments: " <> int.to_string(stats.total_comments))
      io.println("• Total messages: " <> int.to_string(stats.total_messages))
      io.println("• Active sessions: " <> int.to_string(stats.active_sessions))

      io.println("\n🏆 Most Popular Subreddits:")
      list.take(stats.most_popular_subreddits, 5)
      |> list.each(fn(pair) {
        let #(name, count) = pair
        io.println("  • " <> name <> ": " <> int.to_string(count) <> " members")
      })
    }
    _ -> io.println("⚠️  Failed to get final stats")
  }
}
