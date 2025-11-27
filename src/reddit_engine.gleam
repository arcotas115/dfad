// src/reddit_engine.gleam
// Reddit engine with REST API support, search, and digital signatures

import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option
import gleam/otp/actor
import types.{
  type EngineState, type SubredditName, type UserId, type Vote, Downvote,
  EngineState, Post, Subreddit, UpdateStatsCache, Upvote, User, UserInfo,
}
import utils

// ============================================================================
// Engine Startup
// ============================================================================

pub fn start() -> Result(Subject(types.EngineMessage), actor.StartError) {
  let result =
    actor.new_with_initialiser(1000, fn(self_subject) {
      let start_time = utils.current_timestamp()
      let state =
        EngineState(
          self_subject: self_subject,
          users: dict.new(),
          subreddits: dict.new(),
          posts: dict.new(),
          comments: dict.new(),
          messages: dict.new(),
          user_votes: dict.new(),
          user_sessions: dict.new(),
          next_post_id: 1,
          next_comment_id: 1,
          next_message_id: 1,
          metrics: types.PerformanceMetrics(
            total_posts_created: 0,
            total_comments_created: 0,
            total_votes_cast: 0,
            total_messages_sent: 0,
            messages_processed: 0,
            last_rate_check: start_time,
            posts_since_last_check: 0,
            comments_since_last_check: 0,
            votes_since_last_check: 0,
            posts_per_second: 0.0,
            comments_per_second: 0.0,
            votes_per_second: 0.0,
          ),
          start_time: start_time,
          cached_stats: option.None,
          last_stats_update: start_time,
        )

      let initial_stats = calculate_stats(state)
      let state_with_cache =
        EngineState(..state, cached_stats: option.Some(initial_stats))

      process.send_after(self_subject, 3000, UpdateStatsCache)

      state_with_cache
      |> actor.initialised
      |> actor.returning(self_subject)
      |> Ok
    })
    |> actor.on_message(handle_message)
    |> actor.start

  case result {
    Ok(actor_started) -> Ok(actor_started.data)
    Error(e) -> Error(e)
  }
}

// ============================================================================
// Message Handler
// ============================================================================

fn handle_message(
  state: EngineState,
  message: types.EngineMessage,
) -> actor.Next(EngineState, types.EngineMessage) {
  let updated_metrics = case message {
    UpdateStatsCache -> state.metrics
    _ ->
      types.PerformanceMetrics(
        ..state.metrics,
        messages_processed: state.metrics.messages_processed + 1,
      )
  }
  let state = EngineState(..state, metrics: updated_metrics)

  let new_state = case message {
    types.RegisterUser(username, password, public_key) ->
      handle_register_user(username, password, public_key, state)
    types.AuthenticateUser(username, password, reply_to) -> {
      handle_authenticate_user(username, password, reply_to, state)
      state
    }
    types.LoginUser(user_id, session) ->
      handle_login_user(user_id, session, state)
    types.LogoutUser(user_id) -> handle_logout_user(user_id, state)
    types.CreateSubreddit(user_id, name, description) ->
      handle_create_subreddit(user_id, name, description, state)
    types.JoinSubreddit(user_id, subreddit) ->
      handle_join_subreddit(user_id, subreddit, state)
    types.LeaveSubreddit(user_id, subreddit) ->
      handle_leave_subreddit(user_id, subreddit, state)
    types.CreatePost(user_id, subreddit, title, content, signature) ->
      handle_create_post(user_id, subreddit, title, content, signature, state)
    types.CreateRepost(user_id, original_post_id, subreddit) ->
      handle_create_repost(user_id, original_post_id, subreddit, state)
    types.VotePost(user_id, post_id, vote) ->
      handle_vote_post(user_id, post_id, vote, state)
    types.CreateComment(user_id, post_id, parent_id, content) ->
      handle_create_comment(user_id, post_id, parent_id, content, state)
    types.VoteComment(user_id, comment_id, vote) ->
      handle_vote_comment(user_id, comment_id, vote, state)
    types.GetFeed(user_id, limit, reply_to) -> {
      handle_get_feed(user_id, limit, reply_to, state)
      state
    }
    types.GetSubredditFeed(subreddit, limit, reply_to) -> {
      handle_get_subreddit_feed(subreddit, limit, reply_to, state)
      state
    }
    types.GetPost(post_id, reply_to) -> {
      handle_get_post(post_id, reply_to, state)
      state
    }
    types.GetComments(post_id, reply_to) -> {
      handle_get_comments(post_id, reply_to, state)
      state
    }
    types.SendDirectMessage(sender, recipient, content, reply_msg_id) ->
      handle_send_dm(sender, recipient, content, reply_msg_id, state)
    types.GetMessages(user_id, reply_to) -> {
      handle_get_messages(user_id, reply_to, state)
      state
    }
    types.GetStats(reply_to) -> {
      handle_get_stats_cached(reply_to, state)
      state
    }
    types.GetUserKarma(user_id, reply_to) -> {
      handle_get_user_karma(user_id, reply_to, state)
      state
    }
    types.GetAllSubreddits(reply_to) -> {
      handle_get_all_subreddits(reply_to, state)
      state
    }
    // Search handlers
    types.SearchPosts(query, limit, reply_to) -> {
      handle_search_posts(query, limit, reply_to, state)
      state
    }
    types.SearchSubreddits(query, limit, reply_to) -> {
      handle_search_subreddits(query, limit, reply_to, state)
      state
    }
    types.SearchUsers(query, limit, reply_to) -> {
      handle_search_users(query, limit, reply_to, state)
      state
    }
    // Digital signature handlers
    types.GetUserPublicKey(user_id, reply_to) -> {
      handle_get_user_public_key(user_id, reply_to, state)
      state
    }
    // Active clients
    types.GetActiveClients(reply_to) -> {
      handle_get_active_clients(reply_to, state)
      state
    }
    UpdateStatsCache -> update_stats_cache(state)
  }

  actor.continue(new_state)
}

// ============================================================================
// Stats Cache Management with Rate Calculations
// ============================================================================

fn update_stats_cache(state: EngineState) -> EngineState {
  let current_time = utils.current_timestamp()

  // Calculate time elapsed since last rate check (in seconds)
  let time_elapsed_ms = current_time - state.metrics.last_rate_check
  let time_elapsed_sec = int.to_float(time_elapsed_ms) /. 1000.0

  // Calculate rates
  let posts_per_sec = case time_elapsed_sec >. 0.0 {
    True ->
      int.to_float(state.metrics.posts_since_last_check) /. time_elapsed_sec
    False -> 0.0
  }
  let comments_per_sec = case time_elapsed_sec >. 0.0 {
    True ->
      int.to_float(state.metrics.comments_since_last_check) /. time_elapsed_sec
    False -> 0.0
  }
  let votes_per_sec = case time_elapsed_sec >. 0.0 {
    True ->
      int.to_float(state.metrics.votes_since_last_check) /. time_elapsed_sec
    False -> 0.0
  }

  // Update metrics with new rates and reset counters
  let new_metrics =
    types.PerformanceMetrics(
      ..state.metrics,
      last_rate_check: current_time,
      posts_since_last_check: 0,
      comments_since_last_check: 0,
      votes_since_last_check: 0,
      posts_per_second: posts_per_sec,
      comments_per_second: comments_per_sec,
      votes_per_second: votes_per_sec,
    )

  let state_with_metrics = EngineState(..state, metrics: new_metrics)
  let stats = calculate_stats(state_with_metrics)

  process.send_after(state.self_subject, 3000, UpdateStatsCache)

  EngineState(
    ..state_with_metrics,
    cached_stats: option.Some(stats),
    last_stats_update: current_time,
  )
}

fn handle_get_stats_cached(
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  let stats = case state.cached_stats {
    option.Some(cached) -> cached
    option.None -> calculate_stats(state)
  }

  process.send(reply_to, types.Success(types.StatsData(stats)))
}

fn calculate_stats(state: EngineState) -> types.EngineStats {
  let popular_subreddits =
    state.subreddits
    |> dict.values
    |> list.map(fn(s) { #(s.name, list.length(s.members)) })
    |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
    |> list.take(10)

  let current_time = utils.current_timestamp()
  let uptime = current_time - state.start_time
  let uptime_seconds = uptime / 1000

  let comments_with_replies =
    state.comments
    |> dict.values
    |> list.filter(fn(c) { c.replies != [] })
    |> list.length

  let max_depth = get_max_comment_depth(state.comments)

  let threaded_messages =
    state.messages
    |> dict.values
    |> list.filter(fn(m) { option.is_some(m.reply_to) })
    |> list.length

  let repost_count =
    state.posts
    |> dict.values
    |> list.filter(fn(p) { p.is_repost })
    |> list.length

  let users_list = dict.values(state.users)
  let low_karma =
    list.filter(users_list, fn(u) { u.karma < 10 })
    |> list.length
  let medium_karma =
    list.filter(users_list, fn(u) { u.karma >= 10 && u.karma < 50 })
    |> list.length
  let high_karma =
    list.filter(users_list, fn(u) { u.karma >= 50 })
    |> list.length

  types.EngineStats(
    total_users: dict.size(state.users),
    total_subreddits: dict.size(state.subreddits),
    total_posts: dict.size(state.posts),
    total_comments: dict.size(state.comments),
    total_messages: dict.size(state.messages),
    active_sessions: dict.size(state.user_sessions),
    most_popular_subreddits: popular_subreddits,
    performance_metrics: state.metrics,
    uptime_seconds: uptime_seconds,
    comments_with_replies: comments_with_replies,
    max_comment_depth: max_depth,
    threaded_messages: threaded_messages,
    repost_count: repost_count,
    karma_distribution: #(low_karma, medium_karma, high_karma),
    posts_per_second: state.metrics.posts_per_second,
    comments_per_second: state.metrics.comments_per_second,
    votes_per_second: state.metrics.votes_per_second,
    start_time: state.start_time,
    last_update_time: state.last_stats_update,
  )
}

fn get_max_comment_depth(
  comments: dict.Dict(types.CommentId, types.Comment),
) -> Int {
  let top_level_comments =
    comments
    |> dict.values
    |> list.filter(fn(c) { option.is_none(c.parent_id) })

  case top_level_comments {
    [] -> 0
    _ -> {
      let depths =
        list.map(top_level_comments, fn(c) {
          calculate_comment_depth(comments, c.id, 1)
        })
      case list.reduce(depths, int.max) {
        Ok(max) -> max
        Error(_) -> 0
      }
    }
  }
}

fn calculate_comment_depth(
  comments: dict.Dict(types.CommentId, types.Comment),
  comment_id: types.CommentId,
  current_depth: Int,
) -> Int {
  case dict.get(comments, comment_id) {
    Ok(comment) -> {
      case comment.replies {
        [] -> current_depth
        replies -> {
          let depths =
            list.map(replies, fn(reply_id) {
              calculate_comment_depth(comments, reply_id, current_depth + 1)
            })
          case list.reduce(depths, int.max) {
            Ok(max) -> max
            Error(_) -> current_depth
          }
        }
      }
    }
    Error(_) -> current_depth
  }
}

// ============================================================================
// User Management Handlers
// ============================================================================

fn handle_register_user(
  username: String,
  password: String,
  public_key: option.Option(String),
  state: EngineState,
) -> EngineState {
  let user_id = username
  let timestamp = utils.current_timestamp()

  case dict.has_key(state.users, user_id) {
    True -> state
    False -> {
      let password_hash = utils.hash_password(password)
      let user =
        User(
          id: user_id,
          username: username,
          password_hash: password_hash,
          karma: 0,
          joined_subreddits: [],
          created_at: timestamp,
          public_key: public_key,
        )

      let new_users = dict.insert(state.users, user_id, user)
      EngineState(..state, users: new_users)
    }
  }
}

fn handle_authenticate_user(
  username: String,
  password: String,
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  case dict.get(state.users, username) {
    Ok(user) -> {
      case utils.verify_password(password, user.password_hash) {
        True ->
          process.send(
            reply_to,
            types.Success(types.UserAuthenticated(user.id, user.username)),
          )
        False ->
          process.send(reply_to, types.EngineError("Invalid credentials"))
      }
    }
    Error(_) -> process.send(reply_to, types.EngineError("User not found"))
  }
}

fn handle_login_user(
  user_id: UserId,
  session: Subject(types.ClientMessage),
  state: EngineState,
) -> EngineState {
  let new_sessions = dict.insert(state.user_sessions, user_id, session)
  EngineState(..state, user_sessions: new_sessions)
}

fn handle_logout_user(user_id: UserId, state: EngineState) -> EngineState {
  let new_sessions = dict.delete(state.user_sessions, user_id)
  EngineState(..state, user_sessions: new_sessions)
}

fn handle_get_user_karma(
  user_id: UserId,
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  case dict.get(state.users, user_id) {
    Ok(user) -> {
      process.send(reply_to, types.Success(types.KarmaData(user.karma)))
    }
    Error(_) -> process.send(reply_to, types.EngineError("User not found"))
  }
}

fn handle_get_user_public_key(
  user_id: UserId,
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  case dict.get(state.users, user_id) {
    Ok(user) -> {
      process.send(reply_to, types.Success(types.PublicKeyData(user.public_key)))
    }
    Error(_) -> process.send(reply_to, types.EngineError("User not found"))
  }
}

// ============================================================================
// Subreddit Management Handlers
// ============================================================================

fn handle_create_subreddit(
  user_id: UserId,
  name: String,
  description: String,
  state: EngineState,
) -> EngineState {
  case dict.get(state.users, user_id) {
    Ok(_user) -> {
      case dict.get(state.subreddits, name) {
        Ok(_) -> state
        Error(_) -> {
          let timestamp = utils.current_timestamp()
          let subreddit =
            Subreddit(
              name: name,
              description: description,
              creator: user_id,
              members: [user_id],
              post_count: 0,
              created_at: timestamp,
            )

          let new_subreddits = dict.insert(state.subreddits, name, subreddit)

          let new_users = case dict.get(state.users, user_id) {
            Ok(user) -> {
              let updated_user =
                User(..user, joined_subreddits: [name, ..user.joined_subreddits])
              dict.insert(state.users, user_id, updated_user)
            }
            Error(_) -> state.users
          }

          EngineState(..state, subreddits: new_subreddits, users: new_users)
        }
      }
    }
    Error(_) -> state
  }
}

fn handle_join_subreddit(
  user_id: UserId,
  subreddit_name: SubredditName,
  state: EngineState,
) -> EngineState {
  case
    dict.get(state.users, user_id),
    dict.get(state.subreddits, subreddit_name)
  {
    Ok(user), Ok(subreddit) -> {
      case list.contains(subreddit.members, user_id) {
        True -> state
        False -> {
          let updated_subreddit =
            Subreddit(..subreddit, members: [user_id, ..subreddit.members])
          let updated_user =
            User(..user, joined_subreddits: [
              subreddit_name,
              ..user.joined_subreddits
            ])

          EngineState(
            ..state,
            subreddits: dict.insert(
              state.subreddits,
              subreddit_name,
              updated_subreddit,
            ),
            users: dict.insert(state.users, user_id, updated_user),
          )
        }
      }
    }
    _, _ -> state
  }
}

fn handle_leave_subreddit(
  user_id: UserId,
  subreddit_name: SubredditName,
  state: EngineState,
) -> EngineState {
  case
    dict.get(state.users, user_id),
    dict.get(state.subreddits, subreddit_name)
  {
    Ok(user), Ok(subreddit) -> {
      let updated_subreddit =
        Subreddit(
          ..subreddit,
          members: list.filter(subreddit.members, fn(id) { id != user_id }),
        )
      let updated_user =
        User(
          ..user,
          joined_subreddits: list.filter(user.joined_subreddits, fn(s) {
            s != subreddit_name
          }),
        )

      EngineState(
        ..state,
        subreddits: dict.insert(
          state.subreddits,
          subreddit_name,
          updated_subreddit,
        ),
        users: dict.insert(state.users, user_id, updated_user),
      )
    }
    _, _ -> state
  }
}

fn handle_get_all_subreddits(
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  let subreddits = dict.values(state.subreddits)
  process.send(reply_to, types.Success(types.SubredditsData(subreddits)))
}

// ============================================================================
// Post Management Handlers
// ============================================================================

fn handle_create_post(
  user_id: UserId,
  subreddit: SubredditName,
  title: String,
  content: String,
  signature: option.Option(String),
  state: EngineState,
) -> EngineState {
  case dict.get(state.subreddits, subreddit) {
    Ok(sub) -> {
      case list.contains(sub.members, user_id) {
        True -> {
          let post_id = "post_" <> int.to_string(state.next_post_id)
          let timestamp = utils.current_timestamp()

          let post =
            Post(
              id: post_id,
              subreddit: subreddit,
              author: user_id,
              title: title,
              content: content,
              upvotes: 1,
              downvotes: 0,
              created_at: timestamp,
              is_repost: False,
              original_post_id: option.None,
              signature: signature,
            )

          let new_posts = dict.insert(state.posts, post_id, post)

          let vote_key = user_id <> "_" <> post_id
          let new_votes = dict.insert(state.user_votes, vote_key, Upvote)

          let updated_sub = Subreddit(..sub, post_count: sub.post_count + 1)
          let new_subreddits =
            dict.insert(state.subreddits, subreddit, updated_sub)

          broadcast_to_subreddit_members(
            sub.members,
            types.NewPost(post),
            state,
          )

          let new_metrics =
            types.PerformanceMetrics(
              ..state.metrics,
              total_posts_created: state.metrics.total_posts_created + 1,
              posts_since_last_check: state.metrics.posts_since_last_check + 1,
            )

          EngineState(
            ..state,
            posts: new_posts,
            user_votes: new_votes,
            subreddits: new_subreddits,
            next_post_id: state.next_post_id + 1,
            metrics: new_metrics,
          )
        }
        False -> state
      }
    }
    Error(_) -> state
  }
}

fn handle_create_repost(
  user_id: UserId,
  original_post_id: types.PostId,
  subreddit: SubredditName,
  state: EngineState,
) -> EngineState {
  case
    dict.get(state.posts, original_post_id),
    dict.get(state.subreddits, subreddit)
  {
    Ok(original_post), Ok(sub) -> {
      case list.contains(sub.members, user_id) {
        True -> {
          let post_id = "post_" <> int.to_string(state.next_post_id)
          let timestamp = utils.current_timestamp()

          let post =
            Post(
              id: post_id,
              subreddit: subreddit,
              author: user_id,
              title: "[REPOST] " <> original_post.title,
              content: original_post.content,
              upvotes: 1,
              downvotes: 0,
              created_at: timestamp,
              is_repost: True,
              original_post_id: option.Some(original_post_id),
              signature: option.None,
            )

          let new_posts = dict.insert(state.posts, post_id, post)
          let vote_key = user_id <> "_" <> post_id
          let new_votes = dict.insert(state.user_votes, vote_key, Upvote)

          let updated_sub = Subreddit(..sub, post_count: sub.post_count + 1)
          let new_subreddits =
            dict.insert(state.subreddits, subreddit, updated_sub)

          broadcast_to_subreddit_members(
            sub.members,
            types.NewPost(post),
            state,
          )

          let new_metrics =
            types.PerformanceMetrics(
              ..state.metrics,
              total_posts_created: state.metrics.total_posts_created + 1,
              posts_since_last_check: state.metrics.posts_since_last_check + 1,
            )

          EngineState(
            ..state,
            posts: new_posts,
            user_votes: new_votes,
            subreddits: new_subreddits,
            next_post_id: state.next_post_id + 1,
            metrics: new_metrics,
          )
        }
        False -> state
      }
    }
    _, _ -> state
  }
}

fn handle_vote_post(
  user_id: UserId,
  post_id: types.PostId,
  vote: Vote,
  state: EngineState,
) -> EngineState {
  case dict.get(state.posts, post_id) {
    Ok(post) -> {
      let vote_key = user_id <> "_" <> post_id
      let previous_vote = dict.get(state.user_votes, vote_key)

      let #(new_upvotes, new_downvotes) =
        update_vote_counts(
          post.upvotes,
          post.downvotes,
          previous_vote,
          option.Some(vote),
        )

      let updated_post =
        Post(..post, upvotes: new_upvotes, downvotes: new_downvotes)
      let new_posts = dict.insert(state.posts, post_id, updated_post)
      let new_votes = dict.insert(state.user_votes, vote_key, vote)

      let new_users =
        update_user_karma(
          state.users,
          post.author,
          new_upvotes - new_downvotes - post.upvotes + post.downvotes,
        )

      case dict.get(state.subreddits, post.subreddit) {
        Ok(sub) ->
          broadcast_to_subreddit_members(
            sub.members,
            types.PostVoteUpdate(post_id, new_upvotes, new_downvotes),
            state,
          )
        Error(_) -> Nil
      }

      let new_metrics =
        types.PerformanceMetrics(
          ..state.metrics,
          total_votes_cast: state.metrics.total_votes_cast + 1,
          votes_since_last_check: state.metrics.votes_since_last_check + 1,
        )

      EngineState(
        ..state,
        posts: new_posts,
        user_votes: new_votes,
        users: new_users,
        metrics: new_metrics,
      )
    }
    Error(_) -> state
  }
}

fn handle_get_feed(
  user_id: UserId,
  limit: Int,
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  case dict.get(state.users, user_id) {
    Ok(user) -> {
      let posts =
        state.posts
        |> dict.values
        |> list.filter(fn(p) {
          list.contains(user.joined_subreddits, p.subreddit)
        })
        |> list.sort(fn(a, b) { int.compare(b.created_at, a.created_at) })
        |> list.take(limit)

      process.send(reply_to, types.Success(types.FeedData(posts)))
    }
    Error(_) -> process.send(reply_to, types.EngineError("User not found"))
  }
}

fn handle_get_subreddit_feed(
  subreddit: SubredditName,
  limit: Int,
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  let posts =
    state.posts
    |> dict.values
    |> list.filter(fn(p) { p.subreddit == subreddit })
    |> list.sort(fn(a, b) { int.compare(b.created_at, a.created_at) })
    |> list.take(limit)

  process.send(reply_to, types.Success(types.FeedData(posts)))
}

fn handle_get_post(
  post_id: types.PostId,
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  case dict.get(state.posts, post_id) {
    Ok(post) -> {
      let comments =
        state.comments
        |> dict.values
        |> list.filter(fn(c) {
          c.post_id == post_id && c.parent_id == option.None
        })

      process.send(reply_to, types.Success(types.PostData(post, comments)))
    }
    Error(_) -> process.send(reply_to, types.EngineError("Post not found"))
  }
}

// ============================================================================
// Comment Management Handlers
// ============================================================================

fn handle_create_comment(
  user_id: UserId,
  post_id: types.PostId,
  parent_id: option.Option(types.CommentId),
  content: String,
  state: EngineState,
) -> EngineState {
  case dict.get(state.posts, post_id) {
    Ok(post) -> {
      let comment_id = "comment_" <> int.to_string(state.next_comment_id)
      let timestamp = utils.current_timestamp()

      let comment =
        types.Comment(
          id: comment_id,
          post_id: post_id,
          parent_id: parent_id,
          author: user_id,
          content: content,
          upvotes: 1,
          downvotes: 0,
          created_at: timestamp,
          replies: [],
        )

      let new_comments = dict.insert(state.comments, comment_id, comment)

      let vote_key = user_id <> "_" <> comment_id
      let new_votes = dict.insert(state.user_votes, vote_key, Upvote)

      let updated_comments = case parent_id {
        option.Some(pid) -> {
          case dict.get(new_comments, pid) {
            Ok(parent_comment) -> {
              let updated_parent =
                types.Comment(..parent_comment, replies: [
                  comment_id,
                  ..parent_comment.replies
                ])
              dict.insert(new_comments, pid, updated_parent)
            }
            Error(_) -> new_comments
          }
        }
        option.None -> new_comments
      }

      case dict.get(state.subreddits, post.subreddit) {
        Ok(sub) ->
          broadcast_to_subreddit_members(
            sub.members,
            types.NewComment(comment),
            state,
          )
        Error(_) -> Nil
      }

      let new_metrics =
        types.PerformanceMetrics(
          ..state.metrics,
          total_comments_created: state.metrics.total_comments_created + 1,
          comments_since_last_check: state.metrics.comments_since_last_check + 1,
        )

      EngineState(
        ..state,
        comments: updated_comments,
        user_votes: new_votes,
        next_comment_id: state.next_comment_id + 1,
        metrics: new_metrics,
      )
    }
    Error(_) -> state
  }
}

fn handle_vote_comment(
  user_id: UserId,
  comment_id: types.CommentId,
  vote: Vote,
  state: EngineState,
) -> EngineState {
  case dict.get(state.comments, comment_id) {
    Ok(comment) -> {
      let vote_key = user_id <> "_" <> comment_id
      let previous_vote = dict.get(state.user_votes, vote_key)

      let #(new_upvotes, new_downvotes) =
        update_vote_counts(
          comment.upvotes,
          comment.downvotes,
          previous_vote,
          option.Some(vote),
        )

      let updated_comment =
        types.Comment(..comment, upvotes: new_upvotes, downvotes: new_downvotes)
      let new_comments =
        dict.insert(state.comments, comment_id, updated_comment)
      let new_votes = dict.insert(state.user_votes, vote_key, vote)

      let new_users =
        update_user_karma(
          state.users,
          comment.author,
          new_upvotes - new_downvotes - comment.upvotes + comment.downvotes,
        )

      let new_metrics =
        types.PerformanceMetrics(
          ..state.metrics,
          total_votes_cast: state.metrics.total_votes_cast + 1,
          votes_since_last_check: state.metrics.votes_since_last_check + 1,
        )

      EngineState(
        ..state,
        comments: new_comments,
        user_votes: new_votes,
        users: new_users,
        metrics: new_metrics,
      )
    }
    Error(_) -> state
  }
}

fn handle_get_comments(
  post_id: types.PostId,
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  let comments =
    state.comments
    |> dict.values
    |> list.filter(fn(c) { c.post_id == post_id })

  process.send(reply_to, types.Success(types.CommentsData(comments)))
}

// ============================================================================
// Direct Message Handlers
// ============================================================================

fn handle_send_dm(
  sender: UserId,
  recipient: UserId,
  content: String,
  reply_msg_id: option.Option(types.MessageId),
  state: EngineState,
) -> EngineState {
  let message_id = "msg_" <> int.to_string(state.next_message_id)
  let timestamp = utils.current_timestamp()

  let message =
    types.DirectMessage(
      id: message_id,
      sender: sender,
      recipient: recipient,
      content: content,
      reply_to: reply_msg_id,
      created_at: timestamp,
      is_read: False,
    )

  let new_messages = dict.insert(state.messages, message_id, message)

  case dict.get(state.user_sessions, recipient) {
    Ok(session) -> process.send(session, types.NewDirectMessage(message))
    Error(_) -> Nil
  }

  EngineState(
    ..state,
    messages: new_messages,
    next_message_id: state.next_message_id + 1,
    metrics: types.PerformanceMetrics(
      ..state.metrics,
      total_messages_sent: state.metrics.total_messages_sent + 1,
    ),
  )
}

fn handle_get_messages(
  user_id: UserId,
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  let messages =
    state.messages
    |> dict.values
    |> list.filter(fn(m) { m.recipient == user_id || m.sender == user_id })
    |> list.sort(fn(a, b) { int.compare(b.created_at, a.created_at) })

  process.send(reply_to, types.Success(types.MessagesData(messages)))
}

// ============================================================================
// Search Handlers
// ============================================================================

fn handle_search_posts(
  query: String,
  limit: Int,
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  let matching_posts =
    state.posts
    |> dict.values
    |> list.filter(fn(p) {
      utils.contains_lowercase(p.title, query)
      || utils.contains_lowercase(p.content, query)
    })
    |> list.sort(fn(a, b) { int.compare(b.created_at, a.created_at) })
    |> list.take(limit)

  process.send(reply_to, types.Success(types.PostSearchResults(matching_posts)))
}

fn handle_search_subreddits(
  query: String,
  limit: Int,
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  let matching_subreddits =
    state.subreddits
    |> dict.values
    |> list.filter(fn(s) {
      utils.contains_lowercase(s.name, query)
      || utils.contains_lowercase(s.description, query)
    })
    |> list.sort(fn(a, b) {
      int.compare(list.length(b.members), list.length(a.members))
    })
    |> list.take(limit)

  process.send(
    reply_to,
    types.Success(types.SubredditSearchResults(matching_subreddits)),
  )
}

fn handle_search_users(
  query: String,
  limit: Int,
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  let matching_users =
    state.users
    |> dict.values
    |> list.filter(fn(u) { utils.contains_lowercase(u.username, query) })
    |> list.map(fn(u) {
      UserInfo(
        id: u.id,
        username: u.username,
        karma: u.karma,
        joined_subreddits_count: list.length(u.joined_subreddits),
        created_at: u.created_at,
        has_public_key: option.is_some(u.public_key),
      )
    })
    |> list.sort(fn(a, b) { int.compare(b.karma, a.karma) })
    |> list.take(limit)

  process.send(reply_to, types.Success(types.UserSearchResults(matching_users)))
}

// ============================================================================
// Active Clients Handler
// ============================================================================

fn handle_get_active_clients(
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  let count = dict.size(state.user_sessions)
  let users = dict.keys(state.user_sessions)
  process.send(reply_to, types.Success(types.ActiveClientsData(count, users)))
}

// ============================================================================
// Helper Functions
// ============================================================================

fn update_vote_counts(
  upvotes: Int,
  downvotes: Int,
  previous: Result(Vote, Nil),
  new: option.Option(Vote),
) -> #(Int, Int) {
  case previous, new {
    Ok(Upvote), option.Some(Downvote) -> #(upvotes - 1, downvotes + 1)
    Ok(Downvote), option.Some(Upvote) -> #(upvotes + 1, downvotes - 1)
    Error(_), option.Some(Upvote) -> #(upvotes + 1, downvotes)
    Error(_), option.Some(Downvote) -> #(upvotes, downvotes + 1)
    _, _ -> #(upvotes, downvotes)
  }
}

fn update_user_karma(
  users: dict.Dict(UserId, types.User),
  user_id: UserId,
  karma_change: Int,
) -> dict.Dict(UserId, types.User) {
  case dict.get(users, user_id) {
    Ok(user) -> {
      let updated_user = User(..user, karma: user.karma + karma_change)
      dict.insert(users, user_id, updated_user)
    }
    Error(_) -> users
  }
}

fn broadcast_to_subreddit_members(
  members: List(UserId),
  message: types.ClientMessage,
  state: EngineState,
) -> Nil {
  list.each(members, fn(member_id) {
    case dict.get(state.user_sessions, member_id) {
      Ok(session) -> process.send(session, message)
      Error(_) -> Nil
    }
  })
}
