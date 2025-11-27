// src/json_codec.gleam
// JSON encoding and decoding for Reddit API with search and digital signatures

import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import types

// ============================================================================
// Request Decoders  
// ============================================================================

pub fn decode_register_request(
  json_string: String,
) -> Result(#(String, String, Option(String)), String) {
  let decoder = {
    use username <- decode.field("username", decode.string)
    use password <- decode.field("password", decode.string)
    use public_key <- decode.optional_field(
      "public_key",
      None,
      decode.string |> decode.map(Some),
    )
    decode.success(#(username, password, public_key))
  }

  json.parse(from: json_string, using: decoder)
  |> result.map_error(fn(_) { "Invalid JSON for register request" })
}

pub fn decode_login_request(
  json_string: String,
) -> Result(#(String, String), String) {
  let decoder = {
    use username <- decode.field("username", decode.string)
    use password <- decode.field("password", decode.string)
    decode.success(#(username, password))
  }

  json.parse(from: json_string, using: decoder)
  |> result.map_error(fn(_) { "Invalid JSON for login request" })
}

pub fn decode_create_subreddit_request(
  json_string: String,
) -> Result(#(String, String), String) {
  let decoder = {
    use name <- decode.field("name", decode.string)
    use description <- decode.field("description", decode.string)
    decode.success(#(name, description))
  }

  json.parse(from: json_string, using: decoder)
  |> result.map_error(fn(_) { "Invalid JSON for create subreddit request" })
}

pub fn decode_create_post_request(
  json_string: String,
) -> Result(#(String, String, String, Option(String)), String) {
  let decoder = {
    use subreddit <- decode.field("subreddit", decode.string)
    use title <- decode.field("title", decode.string)
    use content <- decode.field("content", decode.string)
    use signature <- decode.optional_field(
      "signature",
      None,
      decode.string |> decode.map(Some),
    )
    decode.success(#(subreddit, title, content, signature))
  }

  json.parse(from: json_string, using: decoder)
  |> result.map_error(fn(_) { "Invalid JSON for create post request" })
}

pub fn decode_create_comment_request(
  json_string: String,
) -> Result(#(String, Option(String), String), String) {
  let decoder = {
    use post_id <- decode.field("post_id", decode.string)
    use parent_id <- decode.optional_field(
      "parent_id",
      None,
      decode.string |> decode.map(Some),
    )
    use content <- decode.field("content", decode.string)
    decode.success(#(post_id, parent_id, content))
  }

  json.parse(from: json_string, using: decoder)
  |> result.map_error(fn(_) { "Invalid JSON for create comment request" })
}

pub fn decode_vote_request(json_string: String) -> Result(types.Vote, String) {
  let decoder = {
    use vote_str <- decode.field("vote", decode.string)
    case vote_str {
      "up" -> decode.success(types.Upvote)
      "down" -> decode.success(types.Downvote)
      _ -> decode.success(types.Upvote)
    }
  }

  json.parse(from: json_string, using: decoder)
  |> result.map_error(fn(_) { "Invalid JSON for vote request" })
}

pub fn decode_send_message_request(
  json_string: String,
) -> Result(#(String, String, Option(String)), String) {
  let decoder = {
    use recipient <- decode.field("recipient", decode.string)
    use content <- decode.field("content", decode.string)
    use reply_to <- decode.optional_field(
      "reply_to",
      None,
      decode.string |> decode.map(Some),
    )
    decode.success(#(recipient, content, reply_to))
  }

  json.parse(from: json_string, using: decoder)
  |> result.map_error(fn(_) { "Invalid JSON for send message request" })
}

pub fn decode_search_request(
  json_string: String,
) -> Result(#(String, Int), String) {
  let decoder = {
    use query <- decode.field("query", decode.string)
    use limit <- decode.optional_field("limit", 25, decode.int)
    decode.success(#(query, limit))
  }

  json.parse(from: json_string, using: decoder)
  |> result.map_error(fn(_) { "Invalid JSON for search request" })
}

// ============================================================================
// Response Encoders
// ============================================================================

pub fn encode_success_response(message: String) -> String {
  json.object([
    #("status", json.string("success")),
    #("message", json.string(message)),
  ])
  |> json.to_string
}

pub fn encode_error_response(message: String) -> String {
  json.object([
    #("status", json.string("error")),
    #("message", json.string(message)),
  ])
  |> json.to_string
}

pub fn encode_user_response(user_id: String, username: String) -> String {
  json.object([
    #("status", json.string("success")),
    #(
      "data",
      json.object([
        #("user_id", json.string(user_id)),
        #("username", json.string(username)),
      ]),
    ),
  ])
  |> json.to_string
}

pub fn encode_subreddit(subreddit: types.Subreddit) -> json.Json {
  json.object([
    #("name", json.string(subreddit.name)),
    #("description", json.string(subreddit.description)),
    #("creator", json.string(subreddit.creator)),
    #("member_count", json.int(list.length(subreddit.members))),
    #("post_count", json.int(subreddit.post_count)),
    #("created_at", json.int(subreddit.created_at)),
  ])
}

pub fn encode_subreddits_response(subreddits: List(types.Subreddit)) -> String {
  json.object([
    #("status", json.string("success")),
    #(
      "data",
      json.object([
        #("subreddits", json.array(subreddits, of: encode_subreddit)),
        #("count", json.int(list.length(subreddits))),
      ]),
    ),
  ])
  |> json.to_string
}

pub fn encode_post(post: types.Post) -> json.Json {
  json.object([
    #("id", json.string(post.id)),
    #("subreddit", json.string(post.subreddit)),
    #("author", json.string(post.author)),
    #("title", json.string(post.title)),
    #("content", json.string(post.content)),
    #("upvotes", json.int(post.upvotes)),
    #("downvotes", json.int(post.downvotes)),
    #("score", json.int(post.upvotes - post.downvotes)),
    #("created_at", json.int(post.created_at)),
    #("is_repost", json.bool(post.is_repost)),
    #("signature", encode_optional_string(post.signature)),
    #("signature_valid", json.null()),
  ])
}

pub fn encode_post_with_verification(
  post: types.Post,
  signature_valid: Option(Bool),
) -> json.Json {
  json.object([
    #("id", json.string(post.id)),
    #("subreddit", json.string(post.subreddit)),
    #("author", json.string(post.author)),
    #("title", json.string(post.title)),
    #("content", json.string(post.content)),
    #("upvotes", json.int(post.upvotes)),
    #("downvotes", json.int(post.downvotes)),
    #("score", json.int(post.upvotes - post.downvotes)),
    #("created_at", json.int(post.created_at)),
    #("is_repost", json.bool(post.is_repost)),
    #("signature", encode_optional_string(post.signature)),
    #("signature_valid", encode_optional_bool(signature_valid)),
  ])
}

pub fn encode_posts_response(posts: List(types.Post)) -> String {
  json.object([
    #("status", json.string("success")),
    #(
      "data",
      json.object([
        #("posts", json.array(posts, of: encode_post)),
        #("count", json.int(list.length(posts))),
      ]),
    ),
  ])
  |> json.to_string
}

pub fn encode_comment(comment: types.Comment) -> json.Json {
  json.object([
    #("id", json.string(comment.id)),
    #("post_id", json.string(comment.post_id)),
    #("parent_id", encode_optional_string(comment.parent_id)),
    #("author", json.string(comment.author)),
    #("content", json.string(comment.content)),
    #("upvotes", json.int(comment.upvotes)),
    #("downvotes", json.int(comment.downvotes)),
    #("score", json.int(comment.upvotes - comment.downvotes)),
    #("created_at", json.int(comment.created_at)),
    #("reply_count", json.int(list.length(comment.replies))),
  ])
}

pub fn encode_comments_response(comments: List(types.Comment)) -> String {
  json.object([
    #("status", json.string("success")),
    #(
      "data",
      json.object([
        #("comments", json.array(comments, of: encode_comment)),
        #("count", json.int(list.length(comments))),
      ]),
    ),
  ])
  |> json.to_string
}

pub fn encode_post_with_comments(
  post: types.Post,
  comments: List(types.Comment),
) -> String {
  json.object([
    #("status", json.string("success")),
    #(
      "data",
      json.object([
        #("post", encode_post(post)),
        #("comments", json.array(comments, of: encode_comment)),
      ]),
    ),
  ])
  |> json.to_string
}

pub fn encode_post_with_comments_verified(
  post: types.Post,
  comments: List(types.Comment),
  signature_valid: Option(Bool),
) -> String {
  json.object([
    #("status", json.string("success")),
    #(
      "data",
      json.object([
        #("post", encode_post_with_verification(post, signature_valid)),
        #("comments", json.array(comments, of: encode_comment)),
      ]),
    ),
  ])
  |> json.to_string
}

pub fn encode_message(message: types.DirectMessage) -> json.Json {
  json.object([
    #("id", json.string(message.id)),
    #("sender", json.string(message.sender)),
    #("recipient", json.string(message.recipient)),
    #("content", json.string(message.content)),
    #("reply_to", encode_optional_string(message.reply_to)),
    #("created_at", json.int(message.created_at)),
    #("is_read", json.bool(message.is_read)),
  ])
}

pub fn encode_messages_response(messages: List(types.DirectMessage)) -> String {
  json.object([
    #("status", json.string("success")),
    #(
      "data",
      json.object([
        #("messages", json.array(messages, of: encode_message)),
        #("count", json.int(list.length(messages))),
      ]),
    ),
  ])
  |> json.to_string
}

pub fn encode_stats_response(stats: types.EngineStats) -> String {
  json.object([
    #("status", json.string("success")),
    #(
      "data",
      json.object([
        #("total_users", json.int(stats.total_users)),
        #("total_subreddits", json.int(stats.total_subreddits)),
        #("total_posts", json.int(stats.total_posts)),
        #("total_comments", json.int(stats.total_comments)),
        #("total_messages", json.int(stats.total_messages)),
        #("active_sessions", json.int(stats.active_sessions)),
        #("uptime_seconds", json.int(stats.uptime_seconds)),
        #("comments_with_replies", json.int(stats.comments_with_replies)),
        #("max_comment_depth", json.int(stats.max_comment_depth)),
        #("threaded_messages", json.int(stats.threaded_messages)),
        #("repost_count", json.int(stats.repost_count)),
        #("start_time", json.int(stats.start_time)),
        #("last_update_time", json.int(stats.last_update_time)),
        #(
          "rates",
          json.object([
            #("posts_per_second", json.float(stats.posts_per_second)),
            #("comments_per_second", json.float(stats.comments_per_second)),
            #("votes_per_second", json.float(stats.votes_per_second)),
          ]),
        ),
        #(
          "performance",
          json.object([
            #(
              "total_posts_created",
              json.int(stats.performance_metrics.total_posts_created),
            ),
            #(
              "total_comments_created",
              json.int(stats.performance_metrics.total_comments_created),
            ),
            #(
              "total_votes_cast",
              json.int(stats.performance_metrics.total_votes_cast),
            ),
            #(
              "messages_processed",
              json.int(stats.performance_metrics.messages_processed),
            ),
          ]),
        ),
      ]),
    ),
  ])
  |> json.to_string
}

pub fn encode_karma_response(karma: Int) -> String {
  json.object([
    #("status", json.string("success")),
    #("data", json.object([#("karma", json.int(karma))])),
  ])
  |> json.to_string
}

pub fn encode_public_key_response(public_key: Option(String)) -> String {
  json.object([
    #("status", json.string("success")),
    #(
      "data",
      json.object([#("public_key", encode_optional_string(public_key))]),
    ),
  ])
  |> json.to_string
}

pub fn encode_user_info(user: types.UserInfo) -> json.Json {
  json.object([
    #("id", json.string(user.id)),
    #("username", json.string(user.username)),
    #("karma", json.int(user.karma)),
    #("joined_subreddits_count", json.int(user.joined_subreddits_count)),
    #("created_at", json.int(user.created_at)),
    #("has_public_key", json.bool(user.has_public_key)),
  ])
}

pub fn encode_user_search_response(users: List(types.UserInfo)) -> String {
  json.object([
    #("status", json.string("success")),
    #(
      "data",
      json.object([
        #("users", json.array(users, of: encode_user_info)),
        #("count", json.int(list.length(users))),
      ]),
    ),
  ])
  |> json.to_string
}

pub fn encode_active_clients_response(count: Int, users: List(String)) -> String {
  json.object([
    #("status", json.string("success")),
    #(
      "data",
      json.object([
        #("active_clients", json.int(count)),
        #("users", json.array(users, of: json.string)),
      ]),
    ),
  ])
  |> json.to_string
}

// ============================================================================
// Helper Functions
// ============================================================================

fn encode_optional_string(opt: Option(String)) -> json.Json {
  case opt {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn encode_optional_bool(opt: Option(Bool)) -> json.Json {
  case opt {
    Some(value) -> json.bool(value)
    None -> json.null()
  }
}
