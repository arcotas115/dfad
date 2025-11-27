// src/api_server.gleam
// REST API server for Reddit Clone with logging, search, and digital signatures

import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/process.{type Subject}
import gleam/http.{Get, Post}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import json_codec
import mist.{type Connection, type ResponseData}
import types
import utils

// ============================================================================
// API Server Types
// ============================================================================

pub type ApiContext {
  ApiContext(engine: Subject(types.EngineMessage))
}

// ============================================================================
// Start API Server
// ============================================================================

pub fn start(
  engine: Subject(types.EngineMessage),
  port: Int,
) -> Result(Nil, Nil) {
  let context = ApiContext(engine: engine)

  io.println("Starting Reddit Clone REST API on port " <> int.to_string(port))

  let assert Ok(_) =
    mist.new(fn(req: Request(Connection)) -> Response(ResponseData) {
      handle_request(req, context)
    })
    |> mist.port(port)
    |> mist.start

  Ok(Nil)
}

// ============================================================================
// Request Router with Logging
// ============================================================================

fn handle_request(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  let start_time = utils.current_timestamp()
  let method_str = http_method_to_string(req.method)
  let path = req.path

  // Log incoming request
  log_request(method_str, path, "RECEIVED")

  // Handle CORS preflight
  let response = case req.method {
    http.Options -> handle_cors_preflight()
    _ -> route_request(req, context)
  }

  // Calculate response time
  let end_time = utils.current_timestamp()
  let duration_ms = end_time - start_time

  // Log response
  log_response(method_str, path, response.status, duration_ms)

  // Add CORS headers to all responses
  response
  |> response.prepend_header("access-control-allow-origin", "*")
  |> response.prepend_header(
    "access-control-allow-methods",
    "GET, POST, PUT, DELETE, OPTIONS",
  )
  |> response.prepend_header(
    "access-control-allow-headers",
    "Content-Type, Authorization",
  )
}

fn log_request(method: String, path: String, status: String) -> Nil {
  let timestamp = utils.current_timestamp()
  io.println(
    "["
    <> int.to_string(timestamp)
    <> "] "
    <> status
    <> " "
    <> method
    <> " "
    <> path,
  )
}

fn log_response(method: String, path: String, status: Int, duration_ms: Int) -> Nil {
  let timestamp = utils.current_timestamp()
  let status_emoji = case status {
    s if s >= 200 && s < 300 -> "✅"
    s if s >= 400 && s < 500 -> "⚠️"
    s if s >= 500 -> "❌"
    _ -> "📋"
  }
  io.println(
    "["
    <> int.to_string(timestamp)
    <> "] "
    <> status_emoji
    <> " "
    <> method
    <> " "
    <> path
    <> " -> "
    <> int.to_string(status)
    <> " ("
    <> int.to_string(duration_ms)
    <> "ms)",
  )
}

fn http_method_to_string(method: http.Method) -> String {
  case method {
    http.Get -> "GET"
    http.Post -> "POST"
    http.Put -> "PUT"
    http.Delete -> "DELETE"
    http.Options -> "OPTIONS"
    http.Patch -> "PATCH"
    http.Head -> "HEAD"
    http.Connect -> "CONNECT"
    http.Trace -> "TRACE"
    http.Other(s) -> s
  }
}

fn handle_cors_preflight() -> Response(ResponseData) {
  response.new(204)
  |> response.set_body(mist.Bytes(bytes_tree.new()))
}

fn route_request(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  let path_segments = request.path_segments(req)

  case req.method, path_segments {
    // Health check
    Get, ["health"] -> json_response(200, "{\"status\":\"ok\"}")

    // Auth endpoints
    Post, ["api", "register"] -> handle_register(req, context)
    Post, ["api", "login"] -> handle_login(req, context)

    // User endpoints
    Get, ["api", "user", username, "karma"] ->
      handle_get_user_karma(username, context)
    Get, ["api", "user", username, "publickey"] ->
      handle_get_user_public_key(username, context)

    // Subreddit endpoints
    Get, ["api", "subreddits"] -> handle_get_subreddits(context)
    Get, ["api", "subreddit", name] -> handle_get_subreddit(name, context)
    Post, ["api", "subreddit"] -> handle_create_subreddit(req, context)
    Post, ["api", "subreddit", name, "join"] ->
      handle_join_subreddit(name, req, context)
    Post, ["api", "subreddit", name, "leave"] ->
      handle_leave_subreddit(name, req, context)

    // Feed endpoints
    Get, ["api", "posts"] -> handle_get_feed(req, context)
    Get, ["api", "subreddit", subreddit, "posts"] ->
      handle_get_subreddit_feed(subreddit, req, context)

    // Post endpoints
    Get, ["api", "post", post_id] -> handle_get_post(post_id, context)
    Post, ["api", "post"] -> handle_create_post(req, context)
    Post, ["api", "post", post_id, "upvote"] ->
      handle_vote_post(post_id, types.Upvote, req, context)
    Post, ["api", "post", post_id, "downvote"] ->
      handle_vote_post(post_id, types.Downvote, req, context)

    // Comment endpoints
    Get, ["api", "post", post_id, "comments"] ->
      handle_get_post_comments(post_id, context)
    Post, ["api", "comment"] -> handle_create_comment(req, context)
    Post, ["api", "comment", comment_id, "upvote"] ->
      handle_vote_comment(comment_id, types.Upvote, req, context)
    Post, ["api", "comment", comment_id, "downvote"] ->
      handle_vote_comment(comment_id, types.Downvote, req, context)

    // Message endpoints
    Get, ["api", "messages"] -> handle_get_messages(req, context)
    Post, ["api", "message"] -> handle_send_message(req, context)

    // Search endpoints
    Get, ["api", "search", "posts"] -> handle_search_posts(req, context)
    Get, ["api", "search", "subreddits"] ->
      handle_search_subreddits(req, context)
    Get, ["api", "search", "users"] -> handle_search_users(req, context)

    // Stats endpoint
    Get, ["api", "stats"] -> handle_get_stats(context)

    // Active clients endpoint
    Get, ["api", "clients"] -> handle_get_active_clients(context)

    // 404
    _, _ -> handle_not_found()
  }
}

// ============================================================================
// Auth Handlers
// ============================================================================

fn handle_register(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use body <- with_body(req)

  case json_codec.decode_register_request(body) {
    Ok(#(username, password, public_key)) -> {
      // RegisterUser now includes public_key
      process.send(
        context.engine,
        types.RegisterUser(username, password, public_key),
      )

      let response_body =
        json_codec.encode_success_response("User registered successfully")
      json_response(201, response_body)
    }
    Error(msg) -> error_response(400, msg)
  }
}

fn handle_login(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use body <- with_body(req)

  case json_codec.decode_login_request(body) {
    Ok(#(username, password)) -> {
      let response_subject = process.new_subject()
      process.send(
        context.engine,
        types.AuthenticateUser(username, password, response_subject),
      )

      case process.receive(response_subject, 5000) {
        Ok(types.Success(types.UserAuthenticated(user_id, uname))) -> {
          let response_body = json_codec.encode_user_response(user_id, uname)
          json_response(200, response_body)
        }
        Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
        Ok(types.EngineError(msg)) -> error_response(401, msg)
        Error(_) -> error_response(500, "Request timeout")
      }
    }
    Error(msg) -> error_response(400, msg)
  }
}

fn handle_get_user_karma(
  username: String,
  context: ApiContext,
) -> Response(ResponseData) {
  let response_subject = process.new_subject()
  process.send(context.engine, types.GetUserKarma(username, response_subject))

  case process.receive(response_subject, 5000) {
    Ok(types.Success(types.KarmaData(karma))) -> {
      let response_body = json_codec.encode_karma_response(karma)
      json_response(200, response_body)
    }
    Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
    Ok(types.EngineError(msg)) -> error_response(404, msg)
    Error(_) -> error_response(500, "Request timeout")
  }
}

fn handle_get_user_public_key(
  username: String,
  context: ApiContext,
) -> Response(ResponseData) {
  let response_subject = process.new_subject()
  process.send(
    context.engine,
    types.GetUserPublicKey(username, response_subject),
  )

  case process.receive(response_subject, 5000) {
    Ok(types.Success(types.PublicKeyData(public_key))) -> {
      let response_body = json_codec.encode_public_key_response(public_key)
      json_response(200, response_body)
    }
    Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
    Ok(types.EngineError(msg)) -> error_response(404, msg)
    Error(_) -> error_response(500, "Request timeout")
  }
}

// ============================================================================
// Subreddit Handlers
// ============================================================================

fn handle_get_subreddits(context: ApiContext) -> Response(ResponseData) {
  let response_subject = process.new_subject()
  process.send(context.engine, types.GetAllSubreddits(response_subject))

  case process.receive(response_subject, 5000) {
    Ok(types.Success(types.SubredditsData(subreddits))) -> {
      let response_body = json_codec.encode_subreddits_response(subreddits)
      json_response(200, response_body)
    }
    Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
    Ok(types.EngineError(msg)) -> error_response(500, msg)
    Error(_) -> error_response(500, "Request timeout")
  }
}

fn handle_get_subreddit(
  name: String,
  context: ApiContext,
) -> Response(ResponseData) {
  // Get all subreddits and filter for the specific one
  let response_subject = process.new_subject()
  process.send(context.engine, types.GetAllSubreddits(response_subject))

  case process.receive(response_subject, 5000) {
    Ok(types.Success(types.SubredditsData(subreddits))) -> {
      case list.find(subreddits, fn(s) { s.name == name }) {
        Ok(subreddit) -> {
          let response_body =
            json_codec.encode_subreddits_response([subreddit])
          json_response(200, response_body)
        }
        Error(_) -> error_response(404, "Subreddit not found")
      }
    }
    Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
    Ok(types.EngineError(msg)) -> error_response(500, msg)
    Error(_) -> error_response(500, "Request timeout")
  }
}

fn handle_create_subreddit(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use body <- with_body(req)
  use user_id <- with_auth_header(req)

  case json_codec.decode_create_subreddit_request(body) {
    Ok(#(name, description)) -> {
      process.send(
        context.engine,
        types.CreateSubreddit(user_id, name, description),
      )

      let response_body =
        json_codec.encode_success_response("Subreddit '" <> name <> "' created")
      json_response(201, response_body)
    }
    Error(msg) -> error_response(400, msg)
  }
}

fn handle_join_subreddit(
  subreddit: String,
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use user_id <- with_auth_header(req)

  process.send(context.engine, types.JoinSubreddit(user_id, subreddit))

  let response_body =
    json_codec.encode_success_response("Joined r/" <> subreddit)
  json_response(200, response_body)
}

fn handle_leave_subreddit(
  subreddit: String,
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use user_id <- with_auth_header(req)

  process.send(context.engine, types.LeaveSubreddit(user_id, subreddit))

  let response_body = json_codec.encode_success_response("Left r/" <> subreddit)
  json_response(200, response_body)
}

// ============================================================================
// Feed Handlers
// ============================================================================

fn handle_get_feed(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use user_id <- with_auth_header(req)

  let limit = get_query_param(req, "limit") |> parse_limit(25)

  let response_subject = process.new_subject()
  process.send(context.engine, types.GetFeed(user_id, limit, response_subject))

  case process.receive(response_subject, 5000) {
    Ok(types.Success(types.FeedData(posts))) -> {
      let response_body = json_codec.encode_posts_response(posts)
      json_response(200, response_body)
    }
    Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
    Ok(types.EngineError(msg)) -> error_response(500, msg)
    Error(_) -> error_response(500, "Request timeout")
  }
}

fn handle_get_subreddit_feed(
  subreddit: String,
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  let limit = get_query_param(req, "limit") |> parse_limit(25)

  let response_subject = process.new_subject()
  process.send(
    context.engine,
    types.GetSubredditFeed(subreddit, limit, response_subject),
  )

  case process.receive(response_subject, 5000) {
    Ok(types.Success(types.FeedData(posts))) -> {
      let response_body = json_codec.encode_posts_response(posts)
      json_response(200, response_body)
    }
    Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
    Ok(types.EngineError(msg)) -> error_response(500, msg)
    Error(_) -> error_response(500, "Request timeout")
  }
}

fn handle_get_post(
  post_id: String,
  context: ApiContext,
) -> Response(ResponseData) {
  let response_subject = process.new_subject()
  process.send(context.engine, types.GetPost(post_id, response_subject))

  case process.receive(response_subject, 5000) {
    Ok(types.Success(types.PostData(post, comments))) -> {
      // Verify signature if present
      let signature_valid = verify_post_signature(post, context)
      let response_body =
        json_codec.encode_post_with_comments_verified(
          post,
          comments,
          signature_valid,
        )
      json_response(200, response_body)
    }
    Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
    Ok(types.EngineError(msg)) -> error_response(404, msg)
    Error(_) -> error_response(500, "Request timeout")
  }
}

fn verify_post_signature(
  post: types.Post,
  context: ApiContext,
) -> Option(Bool) {
  case post.signature {
    Some(signature) -> {
      // Get author's public key
      let response_subject = process.new_subject()
      process.send(
        context.engine,
        types.GetUserPublicKey(post.author, response_subject),
      )

      case process.receive(response_subject, 2000) {
        Ok(types.Success(types.PublicKeyData(Some(public_key)))) -> {
          let content_to_verify = post.title <> post.content
          let is_valid =
            utils.verify_signature(content_to_verify, signature, public_key)
          Some(is_valid)
        }
        _ -> None
      }
    }
    None -> None
  }
}

fn handle_create_post(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use body <- with_body(req)
  use user_id <- with_auth_header(req)

  case json_codec.decode_create_post_request(body) {
    Ok(#(subreddit, title, content, signature)) -> {
      process.send(
        context.engine,
        types.CreatePost(user_id, subreddit, title, content, signature),
      )

      let response_body = json_codec.encode_success_response("Post created")
      json_response(201, response_body)
    }
    Error(msg) -> error_response(400, msg)
  }
}

fn handle_vote_post(
  post_id: String,
  vote: types.Vote,
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use user_id <- with_auth_header(req)

  process.send(context.engine, types.VotePost(user_id, post_id, vote))

  let response_body = json_codec.encode_success_response("Vote cast")
  json_response(200, response_body)
}

// ============================================================================
// Comment Handlers
// ============================================================================

fn handle_get_post_comments(
  post_id: String,
  context: ApiContext,
) -> Response(ResponseData) {
  let response_subject = process.new_subject()
  process.send(context.engine, types.GetComments(post_id, response_subject))

  case process.receive(response_subject, 5000) {
    Ok(types.Success(types.CommentsData(comments))) -> {
      let response_body = json_codec.encode_comments_response(comments)
      json_response(200, response_body)
    }
    Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
    Ok(types.EngineError(msg)) -> error_response(500, msg)
    Error(_) -> error_response(500, "Request timeout")
  }
}

fn handle_create_comment(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use body <- with_body(req)
  use user_id <- with_auth_header(req)

  case json_codec.decode_create_comment_request(body) {
    Ok(#(post_id, parent_id, content)) -> {
      process.send(
        context.engine,
        types.CreateComment(user_id, post_id, parent_id, content),
      )

      let response_body = json_codec.encode_success_response("Comment created")
      json_response(201, response_body)
    }
    Error(msg) -> error_response(400, msg)
  }
}

fn handle_vote_comment(
  comment_id: String,
  vote: types.Vote,
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use user_id <- with_auth_header(req)

  process.send(context.engine, types.VoteComment(user_id, comment_id, vote))

  let response_body = json_codec.encode_success_response("Vote cast")
  json_response(200, response_body)
}

// ============================================================================
// Message Handlers
// ============================================================================

fn handle_get_messages(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use user_id <- with_auth_header(req)

  let response_subject = process.new_subject()
  process.send(context.engine, types.GetMessages(user_id, response_subject))

  case process.receive(response_subject, 5000) {
    Ok(types.Success(types.MessagesData(messages))) -> {
      let response_body = json_codec.encode_messages_response(messages)
      json_response(200, response_body)
    }
    Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
    Ok(types.EngineError(msg)) -> error_response(500, msg)
    Error(_) -> error_response(500, "Request timeout")
  }
}

fn handle_send_message(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  use body <- with_body(req)
  use user_id <- with_auth_header(req)

  case json_codec.decode_send_message_request(body) {
    Ok(#(recipient, content, reply_to)) -> {
      process.send(
        context.engine,
        types.SendDirectMessage(user_id, recipient, content, reply_to),
      )

      let response_body = json_codec.encode_success_response("Message sent")
      json_response(201, response_body)
    }
    Error(msg) -> error_response(400, msg)
  }
}

// ============================================================================
// Search Handlers
// ============================================================================

fn handle_search_posts(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  let query = get_query_param(req, "q") |> option.unwrap("")
  let limit = get_query_param(req, "limit") |> parse_limit(25)

  case string.is_empty(query) {
    True -> error_response(400, "Search query 'q' is required")
    False -> {
      let response_subject = process.new_subject()
      process.send(
        context.engine,
        types.SearchPosts(query, limit, response_subject),
      )

      case process.receive(response_subject, 5000) {
        Ok(types.Success(types.PostSearchResults(posts))) -> {
          let response_body = json_codec.encode_posts_response(posts)
          json_response(200, response_body)
        }
        Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
        Ok(types.EngineError(msg)) -> error_response(500, msg)
        Error(_) -> error_response(500, "Request timeout")
      }
    }
  }
}

fn handle_search_subreddits(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  let query = get_query_param(req, "q") |> option.unwrap("")
  let limit = get_query_param(req, "limit") |> parse_limit(25)

  case string.is_empty(query) {
    True -> error_response(400, "Search query 'q' is required")
    False -> {
      let response_subject = process.new_subject()
      process.send(
        context.engine,
        types.SearchSubreddits(query, limit, response_subject),
      )

      case process.receive(response_subject, 5000) {
        Ok(types.Success(types.SubredditSearchResults(subreddits))) -> {
          let response_body = json_codec.encode_subreddits_response(subreddits)
          json_response(200, response_body)
        }
        Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
        Ok(types.EngineError(msg)) -> error_response(500, msg)
        Error(_) -> error_response(500, "Request timeout")
      }
    }
  }
}

fn handle_search_users(
  req: Request(Connection),
  context: ApiContext,
) -> Response(ResponseData) {
  let query = get_query_param(req, "q") |> option.unwrap("")
  let limit = get_query_param(req, "limit") |> parse_limit(25)

  case string.is_empty(query) {
    True -> error_response(400, "Search query 'q' is required")
    False -> {
      let response_subject = process.new_subject()
      process.send(
        context.engine,
        types.SearchUsers(query, limit, response_subject),
      )

      case process.receive(response_subject, 5000) {
        Ok(types.Success(types.UserSearchResults(users))) -> {
          let response_body = json_codec.encode_user_search_response(users)
          json_response(200, response_body)
        }
        Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
        Ok(types.EngineError(msg)) -> error_response(500, msg)
        Error(_) -> error_response(500, "Request timeout")
      }
    }
  }
}

// ============================================================================
// Stats Handler
// ============================================================================

fn handle_get_stats(context: ApiContext) -> Response(ResponseData) {
  let response_subject = process.new_subject()
  process.send(context.engine, types.GetStats(response_subject))

  case process.receive(response_subject, 5000) {
    Ok(types.Success(types.StatsData(stats))) -> {
      let response_body = json_codec.encode_stats_response(stats)
      json_response(200, response_body)
    }
    Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
    Ok(types.EngineError(msg)) -> error_response(500, msg)
    Error(_) -> error_response(500, "Request timeout")
  }
}

// ============================================================================
// Active Clients Handler
// ============================================================================

fn handle_get_active_clients(context: ApiContext) -> Response(ResponseData) {
  let response_subject = process.new_subject()
  process.send(context.engine, types.GetActiveClients(response_subject))

  case process.receive(response_subject, 5000) {
    Ok(types.Success(types.ActiveClientsData(count, users))) -> {
      let response_body =
        json_codec.encode_active_clients_response(count, users)
      json_response(200, response_body)
    }
    Ok(types.Success(_)) -> error_response(500, "Unexpected response type")
    Ok(types.EngineError(msg)) -> error_response(500, msg)
    Error(_) -> error_response(500, "Request timeout")
  }
}

// ============================================================================
// 404 Handler
// ============================================================================

fn handle_not_found() -> Response(ResponseData) {
  error_response(404, "Endpoint not found")
}

// ============================================================================
// Helper Functions
// ============================================================================

fn json_response(status: Int, body: String) -> Response(ResponseData) {
  response.new(status)
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
  |> response.prepend_header("content-type", "application/json")
}

fn error_response(status: Int, message: String) -> Response(ResponseData) {
  let body = json_codec.encode_error_response(message)
  json_response(status, body)
}

fn with_body(
  req: Request(Connection),
  handler: fn(String) -> Response(ResponseData),
) -> Response(ResponseData) {
  case mist.read_body(req, 1_048_576) {
    Ok(req_with_body) -> {
      case bit_array.to_string(req_with_body.body) {
        Ok(body_string) -> handler(body_string)
        Error(_) -> error_response(400, "Invalid UTF-8 in request body")
      }
    }
    Error(_) -> error_response(400, "Failed to read request body")
  }
}

fn with_auth_header(
  req: Request(Connection),
  handler: fn(String) -> Response(ResponseData),
) -> Response(ResponseData) {
  case request.get_header(req, "authorization") {
    Ok(auth) -> {
      // Simple auth: Bearer <username>
      case string.starts_with(auth, "Bearer ") {
        True -> {
          let user_id = string.drop_start(auth, 7)
          handler(user_id)
        }
        False -> error_response(401, "Invalid authorization header")
      }
    }
    Error(_) -> error_response(401, "Missing authorization header")
  }
}

fn get_query_param(req: Request(Connection), key: String) -> Option(String) {
  case request.get_query(req) {
    Ok(query) -> {
      list.find(query, fn(pair) { pair.0 == key })
      |> result.map(fn(pair) { pair.1 })
      |> option.from_result
    }
    Error(_) -> None
  }
}

fn parse_limit(opt: Option(String), default: Int) -> Int {
  case opt {
    Some(limit_str) -> {
      case int.parse(limit_str) {
        Ok(limit) -> limit
        Error(_) -> default
      }
    }
    None -> default
  }
}
