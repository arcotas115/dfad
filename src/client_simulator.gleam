// src/client_simulator.gleam
// Multi-client concurrent simulator for Reddit Clone with Zipf distribution

import gleam/dynamic
import gleam/erlang/process
import gleam/float
import gleam/http
import gleam/http/request
import gleam/http/response
import gleam/httpc
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import utils

// ============================================================================
// Simulator Configuration
// ============================================================================

pub type SimConfig {
  SimConfig(
    num_users: Int,
    num_subreddits: Int,
    simulation_duration_ms: Int,
    actions_per_user: Int,
    zipf_alpha: Float,
    connection_probability: Float,
    post_probability: Float,
    comment_probability: Float,
    vote_probability: Float,
    dm_probability: Float,
    base_url: String,
  )
}

pub fn default_config() -> SimConfig {
  SimConfig(
    num_users: 10,
    num_subreddits: 5,
    simulation_duration_ms: 30_000,
    actions_per_user: 5,
    zipf_alpha: 1.0,
    connection_probability: 0.8,
    post_probability: 0.3,
    comment_probability: 0.4,
    vote_probability: 0.5,
    dm_probability: 0.2,
    base_url: "http://localhost:8080",
  )
}

// ============================================================================
// Simulated Client State
// ============================================================================

pub type ClientState {
  ClientState(
    user_id: String,
    username: String,
    password: String,
    public_key: String,
    private_key: String,
    joined_subreddits: List(String),
    created_posts: List(String),
    is_connected: Bool,
    actions_remaining: Int,
  )
}

// ============================================================================
// Main Simulation Entry Point
// ============================================================================

pub fn main() {
  // Start inets for HTTP client
  let _ = start_inets()

  io.println(
    "\n╔═══════════════════════════════════════════════════════════════╗",
  )
  io.println("║     Reddit Clone - Multi-Client Concurrent Simulator         ║")
  io.println(
    "╚═══════════════════════════════════════════════════════════════╝\n",
  )

  let config = default_config()

  io.println("📋 Simulation Configuration:")
  io.println("   - Users: " <> int.to_string(config.num_users))
  io.println("   - Subreddits: " <> int.to_string(config.num_subreddits))
  io.println("   - Actions per user: " <> int.to_string(config.actions_per_user))
  io.println(
    "   - Duration: " <> int.to_string(config.simulation_duration_ms) <> "ms",
  )
  io.println(
    "   - Zipf alpha: " <> float.to_string(config.zipf_alpha) <> "\n",
  )

  // Check server health
  io.println("🔍 Checking server health...")
  case check_health(config.base_url) {
    Ok(_) -> {
      io.println("✅ Server is healthy\n")
      run_simulation(config)
    }
    Error(msg) -> {
      io.println("❌ Server health check failed: " <> msg)
      io.println("   Make sure the server is running with: gleam run")
    }
  }
}

// ============================================================================
// Simulation Runner
// ============================================================================

fn run_simulation(config: SimConfig) -> Nil {
  let start_time = utils.current_timestamp()

  // Phase 1: Create subreddits
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("📌 Phase 1: Creating Subreddits")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  // First register an admin user to create subreddits
  let admin_username = "admin_" <> int.to_string(utils.random_int(9999))
  let #(admin_pub, _admin_priv) = utils.generate_key_pair()
  let _ = register_user(config.base_url, admin_username, "admin123", admin_pub)

  let subreddit_names =
    list.range(1, config.num_subreddits)
    |> list.map(fn(i) {
      let name = "subreddit_" <> int.to_string(i)
      let desc = "Test subreddit number " <> int.to_string(i)
      let _ = create_subreddit(config.base_url, admin_username, name, desc)
      io.println("   Created: r/" <> name)
      name
    })

  utils.sleep(500)

  // Phase 2: Register users
  io.println("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("👥 Phase 2: Registering Users")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  let clients =
    list.range(1, config.num_users)
    |> list.map(fn(i) {
      let username = "user_" <> int.to_string(i)
      let password = "pass_" <> int.to_string(i)
      let #(pub_key, priv_key) = utils.generate_key_pair()

      let _ = register_user(config.base_url, username, password, pub_key)
      io.println("   Registered: " <> username)

      // Join subreddits based on Zipf distribution
      let num_to_join = utils.zipf_sample(config.num_subreddits, config.zipf_alpha)
      let subs_to_join = list.take(subreddit_names, num_to_join)

      list.each(subs_to_join, fn(sub) {
        let _ = join_subreddit(config.base_url, username, sub)
      })

      ClientState(
        user_id: username,
        username: username,
        password: password,
        public_key: pub_key,
        private_key: priv_key,
        joined_subreddits: subs_to_join,
        created_posts: [],
        is_connected: True,
        actions_remaining: config.actions_per_user,
      )
    })

  utils.sleep(500)

  // Phase 2.5: Create initial seed posts for commenting/voting
  io.println("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("📝 Phase 2.5: Creating Seed Posts")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  // Have first few users create initial posts
  list.take(clients, 3)
  |> list.each(fn(client) {
    case client.joined_subreddits {
      [sub, ..] -> {
        let title = utils.random_post_title()
        let content = utils.random_post_content()
        let signature = utils.sign_content(title <> content, client.private_key)
        case create_post_with_signature(
          config.base_url,
          client.username,
          sub,
          title,
          content,
          signature,
        ) {
          Ok(_) -> io.println("   " <> client.username <> " created seed post in r/" <> sub)
          Error(_) -> io.println("   Failed to create seed post")
        }
      }
      _ -> Nil
    }
  })

  utils.sleep(300)

  // Phase 3: Spawn concurrent client processes
  io.println("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("🚀 Phase 3: Running Concurrent Client Simulation")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  io.println(
    "   Spawning "
    <> int.to_string(config.num_users)
    <> " concurrent client processes...\n",
  )

  // Create a subject to receive completion signals
  let completion_subject = process.new_subject()

  // Spawn a process for each client
  list.each(clients, fn(client) {
    process.spawn_unlinked(fn() {
      run_client_actions(client, config, subreddit_names)
      process.send(completion_subject, client.username)
    })
  })

  // Wait for all clients to complete
  io.println("   Waiting for all clients to complete...")
  wait_for_completions(completion_subject, config.num_users, [])

  // Phase 4: Collect and display statistics
  let end_time = utils.current_timestamp()
  let duration_ms = end_time - start_time

  io.println("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("📊 Phase 4: Simulation Results")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  // Get final statistics from server
  case get_stats(config.base_url) {
    Ok(stats_body) -> {
      io.println("   Server Statistics:")
      io.println("   " <> truncate_string(stats_body, 500))
    }
    Error(_) -> io.println("   Failed to fetch final statistics")
  }

  io.println("\n   Simulation Duration: " <> int.to_string(duration_ms) <> "ms")
  io.println(
    "   Total Client Processes: " <> int.to_string(config.num_users),
  )
  io.println(
    "   Actions per Client: " <> int.to_string(config.actions_per_user),
  )

  io.println(
    "\n╔═══════════════════════════════════════════════════════════════╗",
  )
  io.println(
    "║              Simulation Completed Successfully!               ║",
  )
  io.println(
    "╚═══════════════════════════════════════════════════════════════╝\n",
  )
}

fn wait_for_completions(
  subject: process.Subject(String),
  remaining: Int,
  completed: List(String),
) -> List(String) {
  case remaining {
    0 -> {
      io.println(
        "   ✅ All "
        <> int.to_string(list.length(completed))
        <> " clients completed",
      )
      completed
    }
    n -> {
      case process.receive(subject, 60_000) {
        Ok(username) -> {
          wait_for_completions(subject, n - 1, [username, ..completed])
        }
        Error(_) -> {
          io.println("   ⚠️ Timeout waiting for clients, " <> int.to_string(n) <> " remaining")
          completed
        }
      }
    }
  }
}

// ============================================================================
// Client Action Runner
// ============================================================================

fn run_client_actions(
  client: ClientState,
  config: SimConfig,
  all_subreddits: List(String),
) -> Nil {
  case client.actions_remaining {
    0 -> {
      io.println("   [" <> client.username <> "] Completed all actions")
    }
    _ -> {
      // Simulate connection/disconnection
      let is_connected =
        utils.random_float() <. config.connection_probability

      case is_connected {
        False -> {
          io.println(
            "   [" <> client.username <> "] Disconnected, skipping action",
          )
          utils.sleep(100)
          run_client_actions(
            ClientState(..client, actions_remaining: client.actions_remaining
              - 1),
            config,
            all_subreddits,
          )
        }
        True -> {
          // Choose a random action
          let action = choose_action(config)
          let new_client =
            execute_action(action, client, config, all_subreddits)
          utils.sleep(utils.random_int(500) + 100)
          run_client_actions(
            ClientState(..new_client, actions_remaining: client.actions_remaining
              - 1),
            config,
            all_subreddits,
          )
        }
      }
    }
  }
}

fn choose_action(config: SimConfig) -> String {
  let rand = utils.random_float()
  
  // Normalized probabilities that add up to 1.0
  // post: 0.25, comment: 0.25, vote: 0.30, dm: 0.20
  let post_threshold = 0.25
  let comment_threshold = post_threshold +. 0.25  // 0.50
  let vote_threshold = comment_threshold +. 0.30  // 0.80
  // dm is the remaining 0.20 (0.80 to 1.0)
  
  case rand {
    r if r <. post_threshold -> "post"
    r if r <. comment_threshold -> "comment"
    r if r <. vote_threshold -> "vote"
    _ -> "dm"
  }
}

fn execute_action(
  action: String,
  client: ClientState,
  config: SimConfig,
  _all_subreddits: List(String),
) -> ClientState {
  case action {
    "post" -> {
      case client.joined_subreddits {
        [] -> client
        subs -> {
          let sub_index = utils.random_int(list.length(subs))
          case list.drop(subs, sub_index - 1) {
            [subreddit, ..] -> {
              let title = utils.random_post_title()
              let content = utils.random_post_content()
              // Sign the post
              let signature =
                utils.sign_content(title <> content, client.private_key)
              case
                create_post_with_signature(
                  config.base_url,
                  client.username,
                  subreddit,
                  title,
                  content,
                  signature,
                )
              {
                Ok(_) -> {
                  io.println(
                    "   [" <> client.username <> "] Posted in r/" <> subreddit,
                  )
                  client
                }
                Error(_) -> {
                  io.println(
                    "   [" <> client.username <> "] Failed to post in r/" <> subreddit,
                  )
                  client
                }
              }
            }
            _ -> client
          }
        }
      }
    }
    "comment" -> {
      // Get posts from a joined subreddit and comment on one
      case client.joined_subreddits {
        [] -> client
        [sub, ..] -> {
          case get_subreddit_posts(config.base_url, sub) {
            Ok(posts_json) -> {
              // Try to extract a post ID from the response
              case extract_post_id(posts_json) {
                Ok(post_id) -> {
                  let content = utils.random_comment()
                  case create_comment(config.base_url, client.username, post_id, content) {
                    Ok(_) -> {
                      io.println(
                        "   [" <> client.username <> "] Commented on " <> post_id,
                      )
                    }
                    Error(_) -> {
                      io.println(
                        "   [" <> client.username <> "] Failed to comment",
                      )
                    }
                  }
                  client
                }
                Error(_) -> {
                  io.println(
                    "   [" <> client.username <> "] No posts to comment on",
                  )
                  client
                }
              }
            }
            Error(_) -> client
          }
        }
      }
    }
    "vote" -> {
      // Get posts and vote on one
      case client.joined_subreddits {
        [] -> client
        [sub, ..] -> {
          case get_subreddit_posts(config.base_url, sub) {
            Ok(posts_json) -> {
              case extract_post_id(posts_json) {
                Ok(post_id) -> {
                  let vote_type = case utils.random_float() <. 0.7 {
                    True -> "upvote"
                    False -> "downvote"
                  }
                  case vote_on_post(config.base_url, client.username, post_id, vote_type) {
                    Ok(_) -> {
                      io.println(
                        "   [" <> client.username <> "] " <> vote_type <> "d " <> post_id,
                      )
                    }
                    Error(_) -> {
                      io.println(
                        "   [" <> client.username <> "] Failed to vote",
                      )
                    }
                  }
                  client
                }
                Error(_) -> {
                  io.println(
                    "   [" <> client.username <> "] No posts to vote on",
                  )
                  client
                }
              }
            }
            Error(_) -> client
          }
        }
      }
    }
    "dm" -> {
      // Send a direct message to a random user
      let recipient_num = utils.random_int(config.num_users)
      let recipient_num_safe = case recipient_num {
        0 -> 1
        n -> n
      }
      let recipient = "user_" <> int.to_string(recipient_num_safe)
      let content = utils.random_dm_content()
      case send_dm(config.base_url, client.username, recipient, content) {
        Ok(_) -> {
          io.println(
            "   [" <> client.username <> "] Sent DM to " <> recipient,
          )
        }
        Error(_) -> {
          io.println(
            "   [" <> client.username <> "] Failed to send DM",
          )
        }
      }
      client
    }
    _ -> client
  }
}

// ============================================================================
// HTTP Client Functions
// ============================================================================

fn check_health(base_url: String) -> Result(String, String) {
  make_request(http.Get, base_url <> "/health", "", "")
  |> result.map(fn(resp) { resp.body })
}

fn register_user(
  base_url: String,
  username: String,
  password: String,
  public_key: String,
) -> Result(String, String) {
  let body =
    "{\"username\":\""
    <> username
    <> "\",\"password\":\""
    <> password
    <> "\",\"public_key\":\""
    <> public_key
    <> "\"}"
  make_request(http.Post, base_url <> "/api/register", body, "")
  |> result.map(fn(resp) { resp.body })
}

fn create_subreddit(
  base_url: String,
  username: String,
  name: String,
  description: String,
) -> Result(String, String) {
  let body =
    "{\"name\":\"" <> name <> "\",\"description\":\"" <> description <> "\"}"
  let auth = "Bearer " <> username
  make_request(http.Post, base_url <> "/api/subreddit", body, auth)
  |> result.map(fn(resp) { resp.body })
}

fn join_subreddit(
  base_url: String,
  username: String,
  subreddit: String,
) -> Result(String, String) {
  let auth = "Bearer " <> username
  make_request(
    http.Post,
    base_url <> "/api/subreddit/" <> subreddit <> "/join",
    "",
    auth,
  )
  |> result.map(fn(resp) { resp.body })
}

fn create_post_with_signature(
  base_url: String,
  username: String,
  subreddit: String,
  title: String,
  content: String,
  signature: String,
) -> Result(String, String) {
  let body =
    "{\"subreddit\":\""
    <> subreddit
    <> "\",\"title\":\""
    <> escape_json_string(title)
    <> "\",\"content\":\""
    <> escape_json_string(content)
    <> "\",\"signature\":\""
    <> signature
    <> "\"}"
  let auth = "Bearer " <> username
  make_request(http.Post, base_url <> "/api/post", body, auth)
  |> result.map(fn(resp) { resp.body })
}

fn get_subreddit_posts(
  base_url: String,
  subreddit: String,
) -> Result(String, String) {
  make_request(
    http.Get,
    base_url <> "/api/subreddit/" <> subreddit <> "/posts",
    "",
    "",
  )
  |> result.map(fn(resp) { resp.body })
}

fn create_comment(
  base_url: String,
  username: String,
  post_id: String,
  content: String,
) -> Result(String, String) {
  let body =
    "{\"post_id\":\""
    <> post_id
    <> "\",\"content\":\""
    <> escape_json_string(content)
    <> "\"}"
  let auth = "Bearer " <> username
  make_request(http.Post, base_url <> "/api/comment", body, auth)
  |> result.map(fn(resp) { resp.body })
}

fn vote_on_post(
  base_url: String,
  username: String,
  post_id: String,
  vote_type: String,
) -> Result(String, String) {
  let auth = "Bearer " <> username
  make_request(
    http.Post,
    base_url <> "/api/post/" <> post_id <> "/" <> vote_type,
    "",
    auth,
  )
  |> result.map(fn(resp) { resp.body })
}

fn send_dm(
  base_url: String,
  username: String,
  recipient: String,
  content: String,
) -> Result(String, String) {
  let body =
    "{\"recipient\":\""
    <> recipient
    <> "\",\"content\":\""
    <> escape_json_string(content)
    <> "\"}"
  let auth = "Bearer " <> username
  make_request(http.Post, base_url <> "/api/message", body, auth)
  |> result.map(fn(resp) { resp.body })
}

fn get_stats(base_url: String) -> Result(String, String) {
  make_request(http.Get, base_url <> "/api/stats", "", "")
  |> result.map(fn(resp) { resp.body })
}

fn make_request(
  method: http.Method,
  url: String,
  body: String,
  auth: String,
) -> Result(response.Response(String), String) {
  let path = string.replace(url, "http://localhost:8080", "")

  let req =
    request.new()
    |> request.set_method(method)
    |> request.set_scheme(http.Http)
    |> request.set_host("localhost")
    |> request.set_port(8080)
    |> request.set_path(path)
    |> request.set_body(body)
    |> request.set_header("content-type", "application/json")

  let req = case auth {
    "" -> req
    token -> request.set_header(req, "authorization", token)
  }

  httpc.send(req)
  |> result.map_error(fn(_) { "HTTP request failed" })
}

// ============================================================================
// Helper Functions
// ============================================================================

fn escape_json_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}

fn truncate_string(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len) <> "..."
    False -> s
  }
}

/// Extract a post ID from the JSON response
/// Looks for "post_" pattern in the response
fn extract_post_id(json_response: String) -> Result(String, Nil) {
  // Simple extraction: find "id":"post_X" pattern
  case string.split(json_response, "\"id\":\"post_") {
    [_, rest, ..] -> {
      case string.split(rest, "\"") {
        [id_num, ..] -> Ok("post_" <> id_num)
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

@external(erlang, "inets", "start")
fn start_inets() -> dynamic.Dynamic
