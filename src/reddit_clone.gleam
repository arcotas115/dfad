// src/reddit_clone.gleam
// Main server orchestrator with all features

import api_server
import gleam/erlang/process
import gleam/io
import reddit_engine

pub fn main() {
  io.println(
    "\n╔═══════════════════════════════════════════════════════════════╗",
  )
  io.println("║          Reddit Clone - REST API Server Starting             ║")
  io.println("║       With Search, Digital Signatures & Request Logging      ║")
  io.println(
    "╚═══════════════════════════════════════════════════════════════╝\n",
  )

  // Start the Reddit engine
  io.println("🚀 Starting Reddit engine actor...")
  let assert Ok(engine_subject) = reddit_engine.start()
  io.println("✅ Reddit engine started successfully\n")

  // Start the REST API server
  io.println("🌐 Starting REST API server on http://localhost:8080...")
  let assert Ok(_) = api_server.start(engine_subject, 8080)
  io.println("✅ REST API server started successfully\n")

  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("  Server is ready to accept requests!")
  io.println("  API Base URL: http://localhost:8080/api")
  io.println("  Health Check: http://localhost:8080/health")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  io.println("📚 Available Endpoints:")
  io.println("")
  io.println("  User Management:")
  io.println("    POST   /api/register              - Register user (with optional public_key)")
  io.println("    POST   /api/login                 - Authenticate user")
  io.println("    GET    /api/user/:username/karma  - Get user karma")
  io.println("    GET    /api/user/:username/publickey - Get user's public key")
  io.println("")
  io.println("  Subreddit Management:")
  io.println("    GET    /api/subreddits            - List all subreddits")
  io.println("    GET    /api/subreddit/:name       - Get specific subreddit")
  io.println("    POST   /api/subreddit             - Create subreddit")
  io.println("    POST   /api/subreddit/:name/join  - Join subreddit")
  io.println("    POST   /api/subreddit/:name/leave - Leave subreddit")
  io.println("")
  io.println("  Posts:")
  io.println("    GET    /api/posts                 - Get user's feed")
  io.println("    GET    /api/subreddit/:name/posts - Get subreddit posts")
  io.println("    GET    /api/post/:id              - Get post (verifies signature)")
  io.println("    POST   /api/post                  - Create post (with optional signature)")
  io.println("    POST   /api/post/:id/upvote       - Upvote post")
  io.println("    POST   /api/post/:id/downvote     - Downvote post")
  io.println("")
  io.println("  Comments:")
  io.println("    GET    /api/post/:id/comments     - Get post comments")
  io.println("    POST   /api/comment               - Create comment")
  io.println("    POST   /api/comment/:id/upvote    - Upvote comment")
  io.println("    POST   /api/comment/:id/downvote  - Downvote comment")
  io.println("")
  io.println("  Messages:")
  io.println("    GET    /api/messages              - Get messages")
  io.println("    POST   /api/message               - Send message")
  io.println("")
  io.println("  Search:")
  io.println("    GET    /api/search/posts?q=       - Search posts")
  io.println("    GET    /api/search/subreddits?q=  - Search subreddits")
  io.println("    GET    /api/search/users?q=       - Search users")
  io.println("")
  io.println("  Statistics:")
  io.println("    GET    /api/stats                 - Get system stats (with rates)")
  io.println("    GET    /api/clients               - Get active clients count")
  io.println("")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("📝 Request logging is ENABLED - all requests will be logged")
  io.println("🔐 Digital signatures are SUPPORTED for posts")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  io.println("Press Ctrl+C to stop the server\n")

  // Keep the server running forever
  process.sleep_forever()
}
