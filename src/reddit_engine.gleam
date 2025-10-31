// src/reddit_engine.gleam
// Reddit engine using proper OTP actor model

import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/otp/actor
import types.{
  type EngineState, type SubredditName, type UserId, type Vote, Downvote,
  EngineState, Post, Subreddit, Upvote, User,
}
import utils

// Start the Reddit engine actor
pub fn start() -> Result(Subject(types.EngineMessage), actor.StartError) {
  let result =
    actor.new_with_initialiser(1000, fn(self_subject) {
      // Initialize state
      let state =
        EngineState(
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
          ),
          start_time: utils.current_timestamp(),
        )

      state
      |> actor.initialised
      |> actor.returning(self_subject)
      |> Ok
    })
    |> actor.on_message(handle_message)
    |> actor.start

  // Extract the subject from actor.Started
  case result {
    Ok(actor_started) -> Ok(actor_started.data)
    Error(e) -> Error(e)
  }
}

// Message handler - STATE FIRST, MESSAGE SECOND!
fn handle_message(
  state: EngineState,
  message: types.EngineMessage,
) -> actor.Next(EngineState, types.EngineMessage) {
  // Update metrics
  let updated_metrics =
    types.PerformanceMetrics(
      ..state.metrics,
      messages_processed: state.metrics.messages_processed + 1,
    )
  let state = EngineState(..state, metrics: updated_metrics)

  // Route messages to handlers
  let new_state = case message {
    types.RegisterUser(username) -> handle_register_user(username, state)
    types.LoginUser(user_id, session) ->
      handle_login_user(user_id, session, state)
    types.LogoutUser(user_id) -> handle_logout_user(user_id, state)
    types.CreateSubreddit(user_id, name, description) ->
      handle_create_subreddit(user_id, name, description, state)
    types.JoinSubreddit(user_id, subreddit) ->
      handle_join_subreddit(user_id, subreddit, state)
    types.LeaveSubreddit(user_id, subreddit) ->
      handle_leave_subreddit(user_id, subreddit, state)
    types.CreatePost(user_id, subreddit, title, content) ->
      handle_create_post(user_id, subreddit, title, content, state)
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
      handle_get_stats(reply_to, state)
      state
    }
    types.GetUserKarma(user_id, reply_to) -> {
      handle_get_user_karma(user_id, reply_to, state)
      state
    }
  }

  actor.continue(new_state)
}

// Handler functions

fn handle_register_user(username: String, state: EngineState) -> EngineState {
  // ✅ FIX: Use username as user_id (not generate_id)
  let user_id = username
  let timestamp = utils.current_timestamp()

  case dict.has_key(state.users, user_id) {
    True -> {
      io.println("⚠ User " <> username <> " already exists")
      state
    }
    False -> {
      let user =
        User(
          id: user_id,
          username: username,
          karma: 0,
          joined_subreddits: [],
          created_at: timestamp,
        )

      let new_users = dict.insert(state.users, user_id, user)
      io.println("✓ Registered user: " <> username)
      EngineState(..state, users: new_users)
    }
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

fn handle_create_subreddit(
  user_id: UserId,
  name: String,
  description: String,
  state: EngineState,
) -> EngineState {
  case dict.get(state.users, user_id) {
    Ok(_user) -> {
      case dict.get(state.subreddits, name) {
        Ok(_) -> {
          io.println("⚠ Subreddit r/" <> name <> " already exists")
          state
        }
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

          io.println("✓ Created subreddit: r/" <> name)
          EngineState(..state, subreddits: new_subreddits, users: new_users)
        }
      }
    }
    Error(_) -> {
      io.println("⚠ User not found for subreddit creation")
      state
    }
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

fn handle_create_post(
  user_id: UserId,
  subreddit: SubredditName,
  title: String,
  content: String,
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

          EngineState(
            ..state,
            posts: new_posts,
            user_votes: new_votes,
            subreddits: new_subreddits,
            next_post_id: state.next_post_id + 1,
            metrics: types.PerformanceMetrics(
              ..state.metrics,
              total_posts_created: state.metrics.total_posts_created + 1,
            ),
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

          EngineState(
            ..state,
            posts: new_posts,
            user_votes: new_votes,
            subreddits: new_subreddits,
            next_post_id: state.next_post_id + 1,
            metrics: types.PerformanceMetrics(
              ..state.metrics,
              total_posts_created: state.metrics.total_posts_created + 1,
            ),
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

      EngineState(
        ..state,
        posts: new_posts,
        user_votes: new_votes,
        users: new_users,
        metrics: types.PerformanceMetrics(
          ..state.metrics,
          total_votes_cast: state.metrics.total_votes_cast + 1,
        ),
      )
    }
    Error(_) -> state
  }
}

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

      EngineState(
        ..state,
        comments: updated_comments,
        user_votes: new_votes,
        next_comment_id: state.next_comment_id + 1,
        metrics: types.PerformanceMetrics(
          ..state.metrics,
          total_comments_created: state.metrics.total_comments_created + 1,
        ),
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

      EngineState(
        ..state,
        comments: new_comments,
        user_votes: new_votes,
        users: new_users,
        metrics: types.PerformanceMetrics(
          ..state.metrics,
          total_votes_cast: state.metrics.total_votes_cast + 1,
        ),
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

fn handle_get_stats(
  reply_to: Subject(types.EngineResponse),
  state: EngineState,
) -> Nil {
  let popular_subreddits =
    state.subreddits
    |> dict.values
    |> list.map(fn(s) { #(s.name, list.length(s.members)) })
    |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
    |> list.take(10)

  let uptime = utils.current_timestamp() - state.start_time
  let uptime_seconds = uptime / 1000

  let stats =
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
    )

  process.send(reply_to, types.Success(types.StatsData(stats)))
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

// Helper functions
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
