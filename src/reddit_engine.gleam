// src/engine.gleam
// Main Reddit engine that handles all operations

import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import types.{
  type ClientMessage, type CommentId, type EngineMessage, type EngineResponse,
  type EngineState, type MessageId, type PostId, type SubredditName, type User,
  type UserId, type Vote, Comment, CommentCreated, CommentsData, CreateComment,
  CreatePost, CreateRepost, CreateSubreddit, DirectMessage, Downvote,
  EngineError, EngineState, EngineStats, FeedData, GetComments, GetFeed,
  GetMessages, GetPost, GetStats, GetSubredditFeed, GetUserKarma, JoinSubreddit,
  JoinedSubreddit, KarmaData, LeaveSubreddit, LeftSubreddit, LoginUser,
  LogoutUser, MessageSent, MessagesData, NewComment, NewDirectMessage, NewPost,
  Post, PostCreated, PostData, PostVoteUpdate, RegisterUser, SendDirectMessage,
  StatsData, Subreddit, SubredditCreated, Success, Upvote, User, UserRegistered,
  VoteCast, VoteComment, VotePost,
}
import utils

pub fn start() -> Subject(EngineMessage) {
  let subject = process.new_subject()

  process.spawn_unlinked(fn() {
    let initial_state =
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
      )
    loop(subject, initial_state)
  })

  subject
}

fn loop(subject: Subject(EngineMessage), state: EngineState) -> Nil {
  let message = process.receive_forever(subject)
  let new_state = handle_message(message, state)
  loop(subject, new_state)
}

fn handle_message(message: EngineMessage, state: EngineState) -> EngineState {
  case message {
    RegisterUser(username, reply_to) ->
      handle_register_user(username, reply_to, state)
    LoginUser(user_id, session) -> handle_login_user(user_id, session, state)
    LogoutUser(user_id) -> handle_logout_user(user_id, state)
    CreateSubreddit(user_id, name, description, reply_to) ->
      handle_create_subreddit(user_id, name, description, reply_to, state)
    JoinSubreddit(user_id, subreddit, reply_to) ->
      handle_join_subreddit(user_id, subreddit, reply_to, state)
    LeaveSubreddit(user_id, subreddit, reply_to) ->
      handle_leave_subreddit(user_id, subreddit, reply_to, state)
    CreatePost(user_id, subreddit, title, content, reply_to) ->
      handle_create_post(user_id, subreddit, title, content, reply_to, state)
    CreateRepost(user_id, original_post_id, subreddit, reply_to) ->
      handle_create_repost(
        user_id,
        original_post_id,
        subreddit,
        reply_to,
        state,
      )
    VotePost(user_id, post_id, vote, reply_to) ->
      handle_vote_post(user_id, post_id, vote, reply_to, state)
    CreateComment(user_id, post_id, parent_id, content, reply_to) ->
      handle_create_comment(
        user_id,
        post_id,
        parent_id,
        content,
        reply_to,
        state,
      )
    VoteComment(user_id, comment_id, vote, reply_to) ->
      handle_vote_comment(user_id, comment_id, vote, reply_to, state)
    GetFeed(user_id, limit, reply_to) ->
      handle_get_feed(user_id, limit, reply_to, state)
    GetSubredditFeed(subreddit, limit, reply_to) ->
      handle_get_subreddit_feed(subreddit, limit, reply_to, state)
    GetPost(post_id, reply_to) -> handle_get_post(post_id, reply_to, state)
    GetComments(post_id, reply_to) ->
      handle_get_comments(post_id, reply_to, state)
    SendDirectMessage(sender, recipient, content, reply_msg_id, reply_to) ->
      handle_send_dm(sender, recipient, content, reply_msg_id, reply_to, state)
    GetMessages(user_id, reply_to) ->
      handle_get_messages(user_id, reply_to, state)
    GetStats(reply_to) -> handle_get_stats(reply_to, state)
    GetUserKarma(user_id, reply_to) ->
      handle_get_user_karma(user_id, reply_to, state)
  }
}

fn handle_register_user(
  username: String,
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
  let user_id = utils.generate_id("user")
  let timestamp = utils.current_timestamp()

  let user =
    User(
      id: user_id,
      username: username,
      karma: 0,
      joined_subreddits: [],
      created_at: timestamp,
    )

  let new_users = dict.insert(state.users, user_id, user)
  let new_state = EngineState(..state, users: new_users)

  process.send(reply_to, Success(UserRegistered(user_id)))
  new_state
}

fn handle_login_user(
  user_id: UserId,
  session: Subject(ClientMessage),
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
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
  case dict.get(state.users, user_id) {
    Ok(_user) -> {
      case dict.get(state.subreddits, name) {
        Ok(_) -> {
          process.send(reply_to, EngineError("Subreddit already exists"))
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

          let new_state =
            EngineState(..state, subreddits: new_subreddits, users: new_users)

          process.send(reply_to, Success(SubredditCreated(name)))
          new_state
        }
      }
    }
    Error(_) -> {
      process.send(reply_to, EngineError("User not found"))
      state
    }
  }
}

fn handle_join_subreddit(
  user_id: UserId,
  subreddit_name: SubredditName,
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
  case
    dict.get(state.users, user_id),
    dict.get(state.subreddits, subreddit_name)
  {
    Ok(user), Ok(subreddit) -> {
      case list.contains(subreddit.members, user_id) {
        True -> {
          process.send(reply_to, EngineError("Already a member"))
          state
        }
        False -> {
          let updated_subreddit =
            Subreddit(..subreddit, members: [user_id, ..subreddit.members])
          let updated_user =
            User(..user, joined_subreddits: [
              subreddit_name,
              ..user.joined_subreddits
            ])

          let new_state =
            EngineState(
              ..state,
              subreddits: dict.insert(
                state.subreddits,
                subreddit_name,
                updated_subreddit,
              ),
              users: dict.insert(state.users, user_id, updated_user),
            )

          process.send(reply_to, Success(JoinedSubreddit(subreddit_name)))
          new_state
        }
      }
    }
    _, _ -> {
      process.send(reply_to, EngineError("User or subreddit not found"))
      state
    }
  }
}

fn handle_leave_subreddit(
  user_id: UserId,
  subreddit_name: SubredditName,
  reply_to: Subject(EngineResponse),
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

      let new_state =
        EngineState(
          ..state,
          subreddits: dict.insert(
            state.subreddits,
            subreddit_name,
            updated_subreddit,
          ),
          users: dict.insert(state.users, user_id, updated_user),
        )

      process.send(reply_to, Success(LeftSubreddit(subreddit_name)))
      new_state
    }
    _, _ -> {
      process.send(reply_to, EngineError("User or subreddit not found"))
      state
    }
  }
}

fn handle_create_post(
  user_id: UserId,
  subreddit: SubredditName,
  title: String,
  content: String,
  reply_to: Subject(EngineResponse),
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
              original_post_id: None,
            )

          let new_posts = dict.insert(state.posts, post_id, post)

          let vote_key = user_id <> "_" <> post_id
          let new_votes = dict.insert(state.user_votes, vote_key, Upvote)

          let updated_sub = Subreddit(..sub, post_count: sub.post_count + 1)
          let new_subreddits =
            dict.insert(state.subreddits, subreddit, updated_sub)

          let new_state =
            EngineState(
              ..state,
              posts: new_posts,
              user_votes: new_votes,
              subreddits: new_subreddits,
              next_post_id: state.next_post_id + 1,
            )

          broadcast_to_subreddit_members(sub.members, NewPost(post), state)

          process.send(reply_to, Success(PostCreated(post_id)))
          new_state
        }
        False -> {
          process.send(reply_to, EngineError("Not a member of this subreddit"))
          state
        }
      }
    }
    Error(_) -> {
      process.send(reply_to, EngineError("Subreddit not found"))
      state
    }
  }
}

fn handle_create_repost(
  user_id: UserId,
  original_post_id: PostId,
  subreddit: SubredditName,
  reply_to: Subject(EngineResponse),
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
              original_post_id: Some(original_post_id),
            )

          let new_posts = dict.insert(state.posts, post_id, post)
          let vote_key = user_id <> "_" <> post_id
          let new_votes = dict.insert(state.user_votes, vote_key, Upvote)

          let updated_sub = Subreddit(..sub, post_count: sub.post_count + 1)
          let new_subreddits =
            dict.insert(state.subreddits, subreddit, updated_sub)

          let new_state =
            EngineState(
              ..state,
              posts: new_posts,
              user_votes: new_votes,
              subreddits: new_subreddits,
              next_post_id: state.next_post_id + 1,
            )

          broadcast_to_subreddit_members(sub.members, NewPost(post), state)

          process.send(reply_to, Success(PostCreated(post_id)))
          new_state
        }
        False -> {
          process.send(reply_to, EngineError("Not a member of this subreddit"))
          state
        }
      }
    }
    _, _ -> {
      process.send(
        reply_to,
        EngineError("Original post or subreddit not found"),
      )
      state
    }
  }
}

fn handle_vote_post(
  user_id: UserId,
  post_id: PostId,
  vote: Vote,
  reply_to: Subject(EngineResponse),
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
          Some(vote),
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

      let new_state =
        EngineState(
          ..state,
          posts: new_posts,
          user_votes: new_votes,
          users: new_users,
        )

      case dict.get(state.subreddits, post.subreddit) {
        Ok(sub) ->
          broadcast_to_subreddit_members(
            sub.members,
            PostVoteUpdate(post_id, new_upvotes, new_downvotes),
            state,
          )
        Error(_) -> Nil
      }

      process.send(reply_to, Success(VoteCast))
      new_state
    }
    Error(_) -> {
      process.send(reply_to, EngineError("Post not found"))
      state
    }
  }
}

fn handle_create_comment(
  user_id: UserId,
  post_id: PostId,
  parent_id: Option(CommentId),
  content: String,
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
  case dict.get(state.posts, post_id) {
    Ok(post) -> {
      let comment_id = "comment_" <> int.to_string(state.next_comment_id)
      let timestamp = utils.current_timestamp()

      let comment =
        Comment(
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
        Some(pid) -> {
          case dict.get(new_comments, pid) {
            Ok(parent_comment) -> {
              let updated_parent =
                Comment(..parent_comment, replies: [
                  comment_id,
                  ..parent_comment.replies
                ])
              dict.insert(new_comments, pid, updated_parent)
            }
            Error(_) -> new_comments
          }
        }
        None -> new_comments
      }

      let new_state =
        EngineState(
          ..state,
          comments: updated_comments,
          user_votes: new_votes,
          next_comment_id: state.next_comment_id + 1,
        )

      case dict.get(state.subreddits, post.subreddit) {
        Ok(sub) ->
          broadcast_to_subreddit_members(
            sub.members,
            NewComment(comment),
            state,
          )
        Error(_) -> Nil
      }

      process.send(reply_to, Success(CommentCreated(comment_id)))
      new_state
    }
    Error(_) -> {
      process.send(reply_to, EngineError("Post not found"))
      state
    }
  }
}

fn handle_vote_comment(
  user_id: UserId,
  comment_id: CommentId,
  vote: Vote,
  reply_to: Subject(EngineResponse),
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
          Some(vote),
        )

      let updated_comment =
        Comment(..comment, upvotes: new_upvotes, downvotes: new_downvotes)
      let new_comments =
        dict.insert(state.comments, comment_id, updated_comment)
      let new_votes = dict.insert(state.user_votes, vote_key, vote)

      let new_users =
        update_user_karma(
          state.users,
          comment.author,
          new_upvotes - new_downvotes - comment.upvotes + comment.downvotes,
        )

      let new_state =
        EngineState(
          ..state,
          comments: new_comments,
          user_votes: new_votes,
          users: new_users,
        )

      process.send(reply_to, Success(VoteCast))
      new_state
    }
    Error(_) -> {
      process.send(reply_to, EngineError("Comment not found"))
      state
    }
  }
}

fn handle_get_feed(
  user_id: UserId,
  limit: Int,
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
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

      process.send(reply_to, Success(FeedData(posts)))
      state
    }
    Error(_) -> {
      process.send(reply_to, EngineError("User not found"))
      state
    }
  }
}

fn handle_get_subreddit_feed(
  subreddit: SubredditName,
  limit: Int,
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
  let posts =
    state.posts
    |> dict.values
    |> list.filter(fn(p) { p.subreddit == subreddit })
    |> list.sort(fn(a, b) { int.compare(b.created_at, a.created_at) })
    |> list.take(limit)

  process.send(reply_to, Success(FeedData(posts)))
  state
}

fn handle_get_post(
  post_id: PostId,
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
  case dict.get(state.posts, post_id) {
    Ok(post) -> {
      let comments =
        state.comments
        |> dict.values
        |> list.filter(fn(c) { c.post_id == post_id && c.parent_id == None })

      process.send(reply_to, Success(PostData(post, comments)))
      state
    }
    Error(_) -> {
      process.send(reply_to, EngineError("Post not found"))
      state
    }
  }
}

fn handle_get_comments(
  post_id: PostId,
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
  let comments =
    state.comments
    |> dict.values
    |> list.filter(fn(c) { c.post_id == post_id })

  process.send(reply_to, Success(CommentsData(comments)))
  state
}

fn handle_send_dm(
  sender: UserId,
  recipient: UserId,
  content: String,
  reply_msg_id: Option(MessageId),
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
  let message_id = "msg_" <> int.to_string(state.next_message_id)
  let timestamp = utils.current_timestamp()

  let message =
    DirectMessage(
      id: message_id,
      sender: sender,
      recipient: recipient,
      content: content,
      reply_to: reply_msg_id,
      created_at: timestamp,
      is_read: False,
    )

  let new_messages = dict.insert(state.messages, message_id, message)
  let new_state =
    EngineState(
      ..state,
      messages: new_messages,
      next_message_id: state.next_message_id + 1,
    )

  case dict.get(state.user_sessions, recipient) {
    Ok(session) -> process.send(session, NewDirectMessage(message))
    Error(_) -> Nil
  }

  process.send(reply_to, Success(MessageSent(message_id)))
  new_state
}

fn handle_get_messages(
  user_id: UserId,
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
  let messages =
    state.messages
    |> dict.values
    |> list.filter(fn(m) { m.recipient == user_id || m.sender == user_id })
    |> list.sort(fn(a, b) { int.compare(b.created_at, a.created_at) })

  process.send(reply_to, Success(MessagesData(messages)))
  state
}

fn handle_get_stats(
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
  let popular_subreddits =
    state.subreddits
    |> dict.values
    |> list.map(fn(s) { #(s.name, list.length(s.members)) })
    |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
    |> list.take(10)

  let stats =
    EngineStats(
      total_users: dict.size(state.users),
      total_subreddits: dict.size(state.subreddits),
      total_posts: dict.size(state.posts),
      total_comments: dict.size(state.comments),
      total_messages: dict.size(state.messages),
      active_sessions: dict.size(state.user_sessions),
      most_popular_subreddits: popular_subreddits,
    )

  process.send(reply_to, Success(StatsData(stats)))
  state
}

fn handle_get_user_karma(
  user_id: UserId,
  reply_to: Subject(EngineResponse),
  state: EngineState,
) -> EngineState {
  case dict.get(state.users, user_id) {
    Ok(user) -> {
      process.send(reply_to, Success(KarmaData(user.karma)))
      state
    }
    Error(_) -> {
      process.send(reply_to, EngineError("User not found"))
      state
    }
  }
}

// Helper functions
fn update_vote_counts(
  upvotes: Int,
  downvotes: Int,
  previous: Result(Vote, Nil),
  new: Option(Vote),
) -> #(Int, Int) {
  case previous, new {
    Ok(Upvote), Some(Downvote) -> #(upvotes - 1, downvotes + 1)
    Ok(Downvote), Some(Upvote) -> #(upvotes + 1, downvotes - 1)
    Error(_), Some(Upvote) -> #(upvotes + 1, downvotes)
    Error(_), Some(Downvote) -> #(upvotes, downvotes + 1)
    _, _ -> #(upvotes, downvotes)
  }
}

fn update_user_karma(
  users: dict.Dict(UserId, User),
  user_id: UserId,
  karma_change: Int,
) -> dict.Dict(UserId, User) {
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
  message: ClientMessage,
  state: EngineState,
) -> Nil {
  list.each(members, fn(member_id) {
    case dict.get(state.user_sessions, member_id) {
      Ok(session) -> process.send(session, message)
      Error(_) -> Nil
    }
  })
}
