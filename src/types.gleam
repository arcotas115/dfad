// src/types.gleam
// Core data types for the Reddit clone with REST API support and digital signatures

import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}

// ============================================================================
// User Types
// ============================================================================

pub type UserId =
  String

pub type User {
  User(
    id: UserId,
    username: String,
    password_hash: String,
    karma: Int,
    joined_subreddits: List(String),
    created_at: Int,
    // Digital signature fields (bonus feature)
    public_key: Option(String),
  )
}

// ============================================================================
// Subreddit Types
// ============================================================================

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

// ============================================================================
// Post Types
// ============================================================================

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
    // Digital signature (bonus feature)
    signature: Option(String),
  )
}

// ============================================================================
// Comment Types
// ============================================================================

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

// ============================================================================
// Vote Type
// ============================================================================

pub type Vote {
  Upvote
  Downvote
}

// ============================================================================
// Direct Message Types
// ============================================================================

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

// ============================================================================
// Engine State
// ============================================================================

pub type EngineState {
  EngineState(
    self_subject: Subject(EngineMessage),
    users: Dict(UserId, User),
    subreddits: Dict(SubredditName, Subreddit),
    posts: Dict(PostId, Post),
    comments: Dict(CommentId, Comment),
    messages: Dict(MessageId, DirectMessage),
    user_votes: Dict(String, Vote),
    user_sessions: Dict(UserId, Subject(ClientMessage)),
    next_post_id: Int,
    next_comment_id: Int,
    next_message_id: Int,
    metrics: PerformanceMetrics,
    start_time: Int,
    cached_stats: Option(EngineStats),
    last_stats_update: Int,
  )
}

// ============================================================================
// Performance Metrics
// ============================================================================

pub type PerformanceMetrics {
  PerformanceMetrics(
    total_posts_created: Int,
    total_comments_created: Int,
    total_votes_cast: Int,
    total_messages_sent: Int,
    messages_processed: Int,
    // New rate tracking fields
    last_rate_check: Int,
    posts_since_last_check: Int,
    comments_since_last_check: Int,
    votes_since_last_check: Int,
    // Calculated rates (per second)
    posts_per_second: Float,
    comments_per_second: Float,
    votes_per_second: Float,
  )
}

// ============================================================================
// Engine Messages (Internal Actor Communication)
// ============================================================================

pub type EngineMessage {
  RegisterUser(username: String, password: String, public_key: Option(String))
  LoginUser(user_id: UserId, session: Subject(ClientMessage))
  LogoutUser(user_id: UserId)
  CreateSubreddit(user_id: UserId, name: String, description: String)
  JoinSubreddit(user_id: UserId, subreddit: SubredditName)
  LeaveSubreddit(user_id: UserId, subreddit: SubredditName)
  CreatePost(
    user_id: UserId,
    subreddit: SubredditName,
    title: String,
    content: String,
    signature: Option(String),
  )
  CreateRepost(
    user_id: UserId,
    original_post_id: PostId,
    subreddit: SubredditName,
  )
  VotePost(user_id: UserId, post_id: PostId, vote: Vote)
  CreateComment(
    user_id: UserId,
    post_id: PostId,
    parent_id: Option(CommentId),
    content: String,
  )
  VoteComment(user_id: UserId, comment_id: CommentId, vote: Vote)
  GetFeed(user_id: UserId, limit: Int, reply_to: Subject(EngineResponse))
  GetSubredditFeed(
    subreddit: SubredditName,
    limit: Int,
    reply_to: Subject(EngineResponse),
  )
  GetPost(post_id: PostId, reply_to: Subject(EngineResponse))
  GetComments(post_id: PostId, reply_to: Subject(EngineResponse))
  SendDirectMessage(
    sender: UserId,
    recipient: UserId,
    content: String,
    reply_to: Option(MessageId),
  )
  GetMessages(user_id: UserId, reply_to: Subject(EngineResponse))
  GetStats(reply_to: Subject(EngineResponse))
  GetUserKarma(user_id: UserId, reply_to: Subject(EngineResponse))
  UpdateStatsCache
  AuthenticateUser(
    username: String,
    password: String,
    reply_to: Subject(EngineResponse),
  )
  GetAllSubreddits(reply_to: Subject(EngineResponse))
  // Search functionality
  SearchPosts(query: String, limit: Int, reply_to: Subject(EngineResponse))
  SearchSubreddits(query: String, limit: Int, reply_to: Subject(EngineResponse))
  SearchUsers(query: String, limit: Int, reply_to: Subject(EngineResponse))
  // Digital signature support
  GetUserPublicKey(user_id: UserId, reply_to: Subject(EngineResponse))
  // Active clients tracking
  GetActiveClients(reply_to: Subject(EngineResponse))
}

// ============================================================================
// Engine Responses
// ============================================================================

pub type EngineResponse {
  Success(data: ResponseData)
  EngineError(message: String)
}

pub type ResponseData {
  UserRegistered(user_id: UserId)
  UserAuthenticated(user_id: UserId, username: String)
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
  SubredditsData(subreddits: List(Subreddit))
  // Search results
  PostSearchResults(posts: List(Post))
  SubredditSearchResults(subreddits: List(Subreddit))
  UserSearchResults(users: List(UserInfo))
  // Public key
  PublicKeyData(public_key: Option(String))
  // Active clients
  ActiveClientsData(count: Int, users: List(String))
}

// ============================================================================
// User Info (for search results - doesn't expose sensitive data)
// ============================================================================

pub type UserInfo {
  UserInfo(
    id: UserId,
    username: String,
    karma: Int,
    joined_subreddits_count: Int,
    created_at: Int,
    has_public_key: Bool,
  )
}

// ============================================================================
// Client Messages (Live Updates)
// ============================================================================

pub type ClientMessage {
  NewPost(post: Post)
  NewComment(comment: Comment)
  PostVoteUpdate(post_id: PostId, upvotes: Int, downvotes: Int)
  NewDirectMessage(message: DirectMessage)
}

// ============================================================================
// Statistics
// ============================================================================

pub type EngineStats {
  EngineStats(
    total_users: Int,
    total_subreddits: Int,
    total_posts: Int,
    total_comments: Int,
    total_messages: Int,
    active_sessions: Int,
    most_popular_subreddits: List(#(SubredditName, Int)),
    performance_metrics: PerformanceMetrics,
    uptime_seconds: Int,
    comments_with_replies: Int,
    max_comment_depth: Int,
    threaded_messages: Int,
    repost_count: Int,
    karma_distribution: #(Int, Int, Int),
    // New rate metrics
    posts_per_second: Float,
    comments_per_second: Float,
    votes_per_second: Float,
    start_time: Int,
    last_update_time: Int,
  )
}

// ============================================================================
// Client Simulator Types
// ============================================================================

pub type ClientState {
  ClientState(
    user_id: UserId,
    username: String,
    is_connected: Bool,
    joined_subreddits: List(SubredditName),
    activity_level: Float,
    seen_posts: List(PostId),
    seen_comments: List(#(CommentId, PostId)),
    received_messages: List(DirectMessage),
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

// ============================================================================
// REST API Types
// ============================================================================

pub type ApiRequest {
  // User endpoints
  ApiRegisterUser(
    username: String,
    password: String,
    public_key: Option(String),
  )
  ApiLoginUser(username: String, password: String)

  // Subreddit endpoints
  ApiCreateSubreddit(user_id: String, name: String, description: String)
  ApiJoinSubreddit(user_id: String, subreddit: String)
  ApiLeaveSubreddit(user_id: String, subreddit: String)
  ApiGetSubreddits
  ApiGetSubreddit(name: String)

  // Post endpoints
  ApiCreatePost(
    user_id: String,
    subreddit: String,
    title: String,
    content: String,
    signature: Option(String),
  )
  ApiGetFeed(user_id: String, limit: Int)
  ApiGetSubredditFeed(subreddit: String, limit: Int)
  ApiGetPost(post_id: String)
  ApiVotePost(user_id: String, post_id: String, vote: Vote)

  // Comment endpoints
  ApiCreateComment(
    user_id: String,
    post_id: String,
    parent_id: Option(String),
    content: String,
  )
  ApiGetComments(post_id: String)
  ApiVoteComment(user_id: String, comment_id: String, vote: Vote)

  // Message endpoints
  ApiSendMessage(
    sender: String,
    recipient: String,
    content: String,
    reply_to: Option(String),
  )
  ApiGetMessages(user_id: String)

  // Stats
  ApiGetStats
  ApiGetUserKarma(user_id: String)

  // Search
  ApiSearchPosts(query: String, limit: Int)
  ApiSearchSubreddits(query: String, limit: Int)
  ApiSearchUsers(query: String, limit: Int)

  // Digital signatures
  ApiGetUserPublicKey(user_id: String)
}
