// src/reddit_clone/types.gleam
// Core data types for the Reddit clone

import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}

// User related types
pub type UserId =
  String

pub type User {
  User(
    id: UserId,
    username: String,
    karma: Int,
    joined_subreddits: List(String),
    created_at: Int,
  )
}

// Subreddit types
pub type SubredditName =
  String

pub type Subreddit {
  Subreddit(
    name: SubredditName,
    description: String,
    creator: UserId,
    members: List(UserId),
    post_count: Int,
    created_at: Int,
  )
}

// Post types
pub type PostId =
  String

pub type Post {
  Post(
    id: PostId,
    subreddit: SubredditName,
    author: UserId,
    title: String,
    content: String,
    upvotes: Int,
    downvotes: Int,
    created_at: Int,
    is_repost: Bool,
    original_post_id: Option(PostId),
  )
}

// Comment types
pub type CommentId =
  String

pub type Comment {
  Comment(
    id: CommentId,
    post_id: PostId,
    parent_id: Option(CommentId),
    author: UserId,
    content: String,
    upvotes: Int,
    downvotes: Int,
    created_at: Int,
    replies: List(CommentId),
  )
}

// Vote type
pub type Vote {
  Upvote
  Downvote
}

// Direct Message types
pub type MessageId =
  String

pub type DirectMessage {
  DirectMessage(
    id: MessageId,
    sender: UserId,
    recipient: UserId,
    content: String,
    reply_to: Option(MessageId),
    created_at: Int,
    is_read: Bool,
  )
}

// Engine State
pub type EngineState {
  EngineState(
    users: Dict(UserId, User),
    subreddits: Dict(SubredditName, Subreddit),
    posts: Dict(PostId, Post),
    comments: Dict(CommentId, Comment),
    messages: Dict(MessageId, DirectMessage),
    user_votes: Dict(String, Vote),
    // "userid_postid" or "userid_commentid" -> Vote
    user_sessions: Dict(UserId, Subject(ClientMessage)),
    next_post_id: Int,
    next_comment_id: Int,
    next_message_id: Int,
  )
}

// Messages between client and engine
pub type EngineMessage {
  // User management
  RegisterUser(username: String, reply_to: Subject(EngineResponse))
  LoginUser(user_id: UserId, session: Subject(ClientMessage))
  LogoutUser(user_id: UserId)

  // Subreddit operations
  CreateSubreddit(
    user_id: UserId,
    name: String,
    description: String,
    reply_to: Subject(EngineResponse),
  )
  JoinSubreddit(
    user_id: UserId,
    subreddit: SubredditName,
    reply_to: Subject(EngineResponse),
  )
  LeaveSubreddit(
    user_id: UserId,
    subreddit: SubredditName,
    reply_to: Subject(EngineResponse),
  )

  // Post operations
  CreatePost(
    user_id: UserId,
    subreddit: SubredditName,
    title: String,
    content: String,
    reply_to: Subject(EngineResponse),
  )
  CreateRepost(
    user_id: UserId,
    original_post_id: PostId,
    subreddit: SubredditName,
    reply_to: Subject(EngineResponse),
  )
  VotePost(
    user_id: UserId,
    post_id: PostId,
    vote: Vote,
    reply_to: Subject(EngineResponse),
  )

  // Comment operations
  CreateComment(
    user_id: UserId,
    post_id: PostId,
    parent_id: Option(CommentId),
    content: String,
    reply_to: Subject(EngineResponse),
  )
  VoteComment(
    user_id: UserId,
    comment_id: CommentId,
    vote: Vote,
    reply_to: Subject(EngineResponse),
  )

  // Feed and content retrieval
  GetFeed(user_id: UserId, limit: Int, reply_to: Subject(EngineResponse))
  GetSubredditFeed(
    subreddit: SubredditName,
    limit: Int,
    reply_to: Subject(EngineResponse),
  )
  GetPost(post_id: PostId, reply_to: Subject(EngineResponse))
  GetComments(post_id: PostId, reply_to: Subject(EngineResponse))

  // Direct messages
  SendDirectMessage(
    sender: UserId,
    recipient: UserId,
    content: String,
    reply_to: Option(MessageId),
    reply_subject: Subject(EngineResponse),
  )
  GetMessages(user_id: UserId, reply_to: Subject(EngineResponse))

  // Stats and monitoring
  GetStats(reply_to: Subject(EngineResponse))
  GetUserKarma(user_id: UserId, reply_to: Subject(EngineResponse))
}

// Responses from engine
pub type EngineResponse {
  Success(data: ResponseData)
  Error(message: String)
}

pub type ResponseData {
  UserRegistered(user_id: UserId)
  SubredditCreated(name: SubredditName)
  JoinedSubreddit(name: SubredditName)
  LeftSubreddit(name: SubredditName)
  PostCreated(post_id: PostId)
  CommentCreated(comment_id: CommentId)
  VoteCast
  FeedData(posts: List(Post))
  PostData(post: Post, comments: List(Comment))
  CommentsData(comments: List(Comment))
  MessagesData(messages: List(DirectMessage))
  MessageSent(message_id: MessageId)
  StatsData(stats: EngineStats)
  KarmaData(karma: Int)
}

// Messages to clients (live updates)
pub type ClientMessage {
  NewPost(post: Post)
  NewComment(comment: Comment)
  PostVoteUpdate(post_id: PostId, upvotes: Int, downvotes: Int)
  NewDirectMessage(message: DirectMessage)
}

// Statistics
pub type EngineStats {
  EngineStats(
    total_users: Int,
    total_subreddits: Int,
    total_posts: Int,
    total_comments: Int,
    total_messages: Int,
    active_sessions: Int,
    most_popular_subreddits: List(#(SubredditName, Int)),
  )
}

// Client simulator types
pub type ClientState {
  ClientState(
    user_id: UserId,
    username: String,
    is_connected: Bool,
    joined_subreddits: List(SubredditName),
    activity_level: Float,
    // 0.0 to 1.0, influenced by Zipf distribution
  )
}

pub type SimulatorConfig {
  SimulatorConfig(
    num_users: Int,
    num_subreddits: Int,
    simulation_duration_ms: Int,
    zipf_alpha: Float,
    connection_probability: Float,
    post_probability: Float,
    comment_probability: Float,
    vote_probability: Float,
    dm_probability: Float,
    repost_probability: Float,
  )
}
