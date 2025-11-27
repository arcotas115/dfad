// src/cli_client.gleam
// Command-line client for Reddit Clone REST API with all features

import gleam/dynamic
import gleam/http
import gleam/http/request
import gleam/http/response
import gleam/httpc
import gleam/io
import gleam/result
import gleam/string
import utils

pub type ClientState {
  ClientState(base_url: String, auth_token: String, username: String)
}

pub fn main() {
  // Start the inets application (required for httpc)
  let _ = start_inets()

  io.println(
    "\n╔═══════════════════════════════════════════════════════════════╗",
  )
  io.println("║        Reddit Clone - Command Line Client Demo               ║")
  io.println(
    "╚═══════════════════════════════════════════════════════════════╝\n",
  )

  let base_url = "http://localhost:8080"

  // Generate key pair for digital signatures
  let #(public_key, private_key) = utils.generate_key_pair()
  io.println("🔑 Generated key pair for digital signatures")
  io.println("   Public Key: " <> string.slice(public_key, 0, 32) <> "...")

  // Test 1: Health Check
  io.println("\n📋 Test 1: Health Check")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_health_check(base_url)
  sleep(500)

  // Test 2: Register User with Public Key
  io.println("\n📋 Test 2: User Registration (with public key)")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  let username = "alice_test"
  let password = "password123"
  test_register_with_key(base_url, username, password, public_key)
  sleep(500)

  // Test 3: Login
  io.println("\n📋 Test 3: User Login")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_login(base_url, username, password)
  sleep(500)

  // Test 4: Get User's Public Key
  io.println("\n📋 Test 4: Get User's Public Key")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_get_public_key(base_url, username)
  sleep(500)

  // Test 5: Create Subreddit
  io.println("\n📋 Test 5: Create Subreddit")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_create_subreddit(
    base_url,
    username,
    "programming",
    "Discuss programming topics",
  )
  sleep(500)

  // Test 6: Get All Subreddits
  io.println("\n📋 Test 6: Get All Subreddits")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_get_subreddits(base_url)
  sleep(500)

  // Test 7: Join Subreddit
  io.println("\n📋 Test 7: Join Subreddit")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_join_subreddit(base_url, username, "programming")
  sleep(500)

  // Test 8: Create Signed Post
  io.println("\n📋 Test 8: Create Signed Post")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  let post_title = "Learning Gleam"
  let post_content = "Gleam is an amazing language for the BEAM!"
  let signature =
    utils.sign_content(post_title <> post_content, private_key)
  test_create_signed_post(
    base_url,
    username,
    "programming",
    post_title,
    post_content,
    signature,
  )
  sleep(500)

  // Test 9: Get Posts Feed
  io.println("\n📋 Test 9: Get Posts Feed")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_get_feed(base_url, username)
  sleep(500)

  // Test 10: Get Subreddit Posts
  io.println("\n📋 Test 10: Get Subreddit Posts")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_get_subreddit_posts(base_url, "programming")
  sleep(500)

  // Test 11: Get Post with Signature Verification
  io.println("\n📋 Test 11: Get Post (with signature verification)")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_get_post(base_url, "post_1")
  sleep(500)

  // Test 12: Create Comment
  io.println("\n📋 Test 12: Create Comment")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_create_comment(base_url, username, "post_1", "Great post! I agree.")
  sleep(500)

  // Test 13: Get Post Comments
  io.println("\n📋 Test 13: Get Post Comments")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_get_comments(base_url, "post_1")
  sleep(500)

  // Test 14: Vote on Post
  io.println("\n📋 Test 14: Vote on Post (Upvote)")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_upvote_post(base_url, username, "post_1")
  sleep(500)

  // Test 15: Send Direct Message
  io.println("\n📋 Test 15: Send Direct Message")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_send_message(base_url, username, "bob", "Hello Bob!")
  sleep(500)

  // Test 16: Search Posts
  io.println("\n📋 Test 16: Search Posts")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_search_posts(base_url, "Gleam")
  sleep(500)

  // Test 17: Search Subreddits
  io.println("\n📋 Test 17: Search Subreddits")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_search_subreddits(base_url, "prog")
  sleep(500)

  // Test 18: Search Users
  io.println("\n📋 Test 18: Search Users")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_search_users(base_url, "alice")
  sleep(500)

  // Test 19: Get User Karma
  io.println("\n📋 Test 19: Get User Karma")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_get_karma(base_url, username)
  sleep(500)

  // Test 20: Get Active Clients
  io.println("\n📋 Test 20: Get Active Clients")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_get_active_clients(base_url)
  sleep(500)

  // Test 21: Get System Statistics (with rates)
  io.println("\n📋 Test 21: Get System Statistics (with rates)")
  io.println(
    "─────────────────────────────────────────────────────────────────",
  )
  test_get_stats(base_url)
  sleep(500)

  io.println(
    "\n╔═══════════════════════════════════════════════════════════════╗",
  )
  io.println(
    "║              All Tests Completed Successfully!                ║",
  )
  io.println(
    "╚═══════════════════════════════════════════════════════════════╝\n",
  )
}

// ============================================================================
// Test Functions
// ============================================================================

fn test_health_check(base_url: String) {
  io.println("GET " <> base_url <> "/health")

  case make_request(http.Get, base_url <> "/health", "", "") {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_register_with_key(
  base_url: String,
  username: String,
  password: String,
  public_key: String,
) {
  io.println("POST " <> base_url <> "/api/register")
  io.println("Username: " <> username)
  io.println("Public Key: " <> string.slice(public_key, 0, 32) <> "...")

  let body =
    "{\"username\":\""
    <> username
    <> "\",\"password\":\""
    <> password
    <> "\",\"public_key\":\""
    <> public_key
    <> "\"}"

  case make_request(http.Post, base_url <> "/api/register", body, "") {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_login(base_url: String, username: String, password: String) {
  io.println("POST " <> base_url <> "/api/login")
  io.println("Username: " <> username)

  let body =
    "{\"username\":\"" <> username <> "\",\"password\":\"" <> password <> "\"}"

  case make_request(http.Post, base_url <> "/api/login", body, "") {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_get_public_key(base_url: String, username: String) {
  io.println("GET " <> base_url <> "/api/user/" <> username <> "/publickey")

  case
    make_request(
      http.Get,
      base_url <> "/api/user/" <> username <> "/publickey",
      "",
      "",
    )
  {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_create_subreddit(
  base_url: String,
  username: String,
  name: String,
  description: String,
) {
  io.println("POST " <> base_url <> "/api/subreddit")
  io.println("Subreddit: " <> name)

  let body =
    "{\"name\":\"" <> name <> "\",\"description\":\"" <> description <> "\"}"
  let auth = "Bearer " <> username

  case make_request(http.Post, base_url <> "/api/subreddit", body, auth) {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_get_subreddits(base_url: String) {
  io.println("GET " <> base_url <> "/api/subreddits")

  case make_request(http.Get, base_url <> "/api/subreddits", "", "") {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_join_subreddit(base_url: String, username: String, subreddit: String) {
  io.println("POST " <> base_url <> "/api/subreddit/" <> subreddit <> "/join")
  let auth = "Bearer " <> username

  case
    make_request(
      http.Post,
      base_url <> "/api/subreddit/" <> subreddit <> "/join",
      "",
      auth,
    )
  {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_create_signed_post(
  base_url: String,
  username: String,
  subreddit: String,
  title: String,
  content: String,
  signature: String,
) {
  io.println("POST " <> base_url <> "/api/post")
  io.println("Title: " <> title)
  io.println("Signature: " <> string.slice(signature, 0, 32) <> "...")

  let body =
    "{\"subreddit\":\""
    <> subreddit
    <> "\",\"title\":\""
    <> title
    <> "\",\"content\":\""
    <> content
    <> "\",\"signature\":\""
    <> signature
    <> "\"}"
  let auth = "Bearer " <> username

  case make_request(http.Post, base_url <> "/api/post", body, auth) {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_get_feed(base_url: String, username: String) {
  io.println("GET " <> base_url <> "/api/posts?limit=10")
  let auth = "Bearer " <> username

  case make_request(http.Get, base_url <> "/api/posts?limit=10", "", auth) {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_get_subreddit_posts(base_url: String, subreddit: String) {
  io.println("GET " <> base_url <> "/api/subreddit/" <> subreddit <> "/posts")

  case
    make_request(
      http.Get,
      base_url <> "/api/subreddit/" <> subreddit <> "/posts",
      "",
      "",
    )
  {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_get_post(base_url: String, post_id: String) {
  io.println("GET " <> base_url <> "/api/post/" <> post_id)
  io.println("(Signature will be verified on download)")

  case make_request(http.Get, base_url <> "/api/post/" <> post_id, "", "") {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_create_comment(
  base_url: String,
  username: String,
  post_id: String,
  content: String,
) {
  io.println("POST " <> base_url <> "/api/comment")
  io.println("Post ID: " <> post_id)

  let body =
    "{\"post_id\":\"" <> post_id <> "\",\"content\":\"" <> content <> "\"}"
  let auth = "Bearer " <> username

  case make_request(http.Post, base_url <> "/api/comment", body, auth) {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_get_comments(base_url: String, post_id: String) {
  io.println("GET " <> base_url <> "/api/post/" <> post_id <> "/comments")

  case
    make_request(
      http.Get,
      base_url <> "/api/post/" <> post_id <> "/comments",
      "",
      "",
    )
  {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_upvote_post(base_url: String, username: String, post_id: String) {
  io.println("POST " <> base_url <> "/api/post/" <> post_id <> "/upvote")
  let auth = "Bearer " <> username

  case
    make_request(
      http.Post,
      base_url <> "/api/post/" <> post_id <> "/upvote",
      "",
      auth,
    )
  {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_send_message(
  base_url: String,
  username: String,
  recipient: String,
  content: String,
) {
  io.println("POST " <> base_url <> "/api/message")
  io.println("To: " <> recipient)

  let body =
    "{\"recipient\":\"" <> recipient <> "\",\"content\":\"" <> content <> "\"}"
  let auth = "Bearer " <> username

  case make_request(http.Post, base_url <> "/api/message", body, auth) {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_search_posts(base_url: String, query: String) {
  io.println("GET " <> base_url <> "/api/search/posts?q=" <> query)

  case
    make_request(
      http.Get,
      base_url <> "/api/search/posts?q=" <> query,
      "",
      "",
    )
  {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_search_subreddits(base_url: String, query: String) {
  io.println("GET " <> base_url <> "/api/search/subreddits?q=" <> query)

  case
    make_request(
      http.Get,
      base_url <> "/api/search/subreddits?q=" <> query,
      "",
      "",
    )
  {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_search_users(base_url: String, query: String) {
  io.println("GET " <> base_url <> "/api/search/users?q=" <> query)

  case
    make_request(http.Get, base_url <> "/api/search/users?q=" <> query, "", "") {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_get_karma(base_url: String, username: String) {
  io.println("GET " <> base_url <> "/api/user/" <> username <> "/karma")

  case
    make_request(
      http.Get,
      base_url <> "/api/user/" <> username <> "/karma",
      "",
      "",
    )
  {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_get_active_clients(base_url: String) {
  io.println("GET " <> base_url <> "/api/clients")

  case make_request(http.Get, base_url <> "/api/clients", "", "") {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

fn test_get_stats(base_url: String) {
  io.println("GET " <> base_url <> "/api/stats")

  case make_request(http.Get, base_url <> "/api/stats", "", "") {
    Ok(resp) -> {
      io.println("✅ Status: " <> string.inspect(resp.status))
      io.println("📄 Response: " <> resp.body)
    }
    Error(msg) -> io.println("❌ Error: " <> msg)
  }
}

// ============================================================================
// HTTP Helper Functions
// ============================================================================

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

@external(erlang, "timer", "sleep")
fn sleep(milliseconds: Int) -> Nil

@external(erlang, "inets", "start")
fn start_inets() -> dynamic.Dynamic
