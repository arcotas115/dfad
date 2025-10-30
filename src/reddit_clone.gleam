// src/reddit_clone.gleam
// Main entry point for the Reddit Clone simulation

import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import reddit_clone/simulator
import reddit_clone/types.{SimulatorConfig}

pub fn main() {
  io.println(
    "
╔════════════════════════════════════════╗
║        Reddit Clone Simulator          ║
║         Built with Gleam               ║
╚════════════════════════════════════════╝
  ",
  )

  // Parse command line arguments or use defaults
  let config = parse_args()

  // Print ASCII art logo
  print_logo()

  // Run the simulation
  simulator.run_simulation(config)

  io.println("\n👋 Thank you for using Reddit Clone Simulator!")
}

fn parse_args() -> SimulatorConfig {
  // In a real implementation, would parse actual command line args
  // For now, we'll provide options via code

  io.println("Select simulation configuration:")
  io.println("1. Small Test (10 users, 3 subreddits, 10 seconds)")
  io.println("2. Default (100 users, 20 subreddits, 30 seconds)")
  io.println("3. Large Scale (1000 users, 100 subreddits, 60 seconds)")
  io.println("")

  // For automated testing, we'll use the default config
  // In a real implementation, would read from stdin
  let choice = 2

  case choice {
    1 -> {
      io.println("→ Selected: Small Test Configuration")
      simulator.small_test_config()
    }
    3 -> {
      io.println("→ Selected: Large Scale Configuration")
      simulator.large_scale_config()
    }
    _ -> {
      io.println("→ Selected: Default Configuration")
      simulator.default_config()
    }
  }
}

fn print_logo() {
  io.println(
    "
       🚀 Reddit Clone Engine 🚀
    ┌─────────────────────────────┐
    │  ▲ Upvote                   │
    │  ┃                          │
    │  ● ━━━━━ Posts & Comments   │
    │  ┃                          │
    │  ▼ Downvote                 │
    └─────────────────────────────┘
  ",
  )
}

// Additional CLI functions that could be implemented
pub fn run_with_config(
  users: Int,
  subreddits: Int,
  duration_seconds: Int,
) -> Nil {
  let config =
    SimulatorConfig(
      num_users: users,
      num_subreddits: subreddits,
      simulation_duration_ms: duration_seconds * 1000,
      zipf_alpha: 1.2,
      connection_probability: 0.95,
      post_probability: 0.05,
      comment_probability: 0.15,
      vote_probability: 0.25,
      dm_probability: 0.02,
      repost_probability: 0.1,
    )

  simulator.run_simulation(config)
}

// Benchmark function for testing performance
pub fn benchmark() -> Nil {
  io.println("🔬 Running benchmark suite...")

  let configs = [
    #(
      "Minimal",
      SimulatorConfig(
        num_users: 5,
        num_subreddits: 2,
        simulation_duration_ms: 5000,
        zipf_alpha: 1.0,
        connection_probability: 1.0,
        post_probability: 0.2,
        comment_probability: 0.3,
        vote_probability: 0.4,
        dm_probability: 0.1,
        repost_probability: 0.1,
      ),
    ),
    #("Small", simulator.small_test_config()),
    #("Medium", simulator.default_config()),
  ]

  list.each(configs, fn(config_tuple) {
    let #(name, config) = config_tuple
    io.println("\n📊 Running " <> name <> " benchmark...")
    let start = current_time_ms()
    simulator.run_simulation(config)
    let end = current_time_ms()
    let duration = end - start
    io.println(
      "✅ " <> name <> " completed in " <> int.to_string(duration) <> "ms",
    )
  })

  io.println("\n🎯 Benchmark suite complete!")
}

@external(erlang, "erlang", "system_time")
fn system_time(unit: atom.Atom) -> Int

fn current_time_ms() -> Int {
  system_time(atom.create("millisecond"))
}

// Test function to verify basic functionality
pub fn test_basic() -> Nil {
  io.println("🧪 Running basic functionality test...")

  let test_config =
    SimulatorConfig(
      num_users: 3,
      num_subreddits: 2,
      simulation_duration_ms: 2000,
      zipf_alpha: 1.0,
      connection_probability: 1.0,
      post_probability: 0.5,
      comment_probability: 0.5,
      vote_probability: 0.5,
      dm_probability: 0.0,
      repost_probability: 0.0,
    )

  simulator.run_simulation(test_config)
  io.println("✅ Basic test complete!")
}
