// src/reddit_clone/client_simulator.gleam
// Client simulator that simulates multiple Reddit users

import gleam/list
import gleam/dict
import gleam/io
import gleam/int
import gleam/float
import gleam/string
import gleam/option.{type Option, None, Some}
import gleam/erlang/atom
import gleam/result
import gleam/erlang/process.{type Subject}
import reddit_clone/types.{
  type EngineMessage, type EngineResponse, type ClientMessage,
  type ClientState, type SimulatorConfig, type UserId, type SubredditName,
  type PostId, type Vote,
  ClientState, SimulatorConfig,
  RegisterUser, LoginUser, LogoutUser, CreateSubreddit, JoinSubreddit,
  CreatePost, CreateRepost, VotePost, CreateComment, GetFeed,
  SendDirectMessage, GetMessages,
  Success, Error, UserRegistered, PostCreated,
  NewPost, NewComment, PostVoteUpdate, NewDirectMessage,
  Upvote, Downvote,
}
import reddit_clone/utils

// Client process that simulates a single user
pub fn start_client(
  engine: Subject(EngineMessage),
  config: SimulatorConfig,
  client_id: Int,
  all_subreddits: List(SubredditName)
) -> Subject(ClientControl) {
  process.spawn(fn() {
    // Register the user
    let username = utils.random_username() <> "_" <> int.to_string(client_id)
    let response_subject = process.new_subject()
    
    process.send(engine, RegisterUser(username, response_subject))
    
    let assert Ok(response) = process.receive(response_subject, 5000)
    let assert Success(UserRegistered(user_id)) = response
    
    // Calculate activity level based on Zipf distribution
    let activity_level = calculate_activity_level(client_id, config.num_users, config.zipf_alpha)
    
    let initial_state = ClientState(
      user_id: user_id,
      username: username,
      is_connected: True,
      joined_subreddits: [],
      activity_level: activity_level,
    )
    
    // Create subject for receiving messages from engine
    let client_messages = process.new_subject()
    process.send(engine, LoginUser(user_id, client_messages))
    
    // Join some subreddits based on activity level
    let num_subreddits_to_join = float.round(
      activity_level *. int.to_float(list.length(all_subreddits))
    ) |> int.max(1) |> int.min(list.length(all_subreddits))
    
    let state_with_subreddits = join_random_subreddits(
      engine, initial_state, all_subreddits, num_subreddits_to_join
    )
    
    client_loop(engine, config, state_with_subreddits, client_messages)
  })
  |> process.new_subject
}

pub type ClientControl {
  SimulateActivity
  Disconnect
  Reconnect
  GetStatus(reply_to: Subject(ClientState))
  Stop
}

fn client_loop(
  engine: Subject(EngineMessage),
  config: SimulatorConfig,
  state: ClientState,
  messages: Subject(ClientMessage)
) -> Nil {
  // Simplified version - handle messages sequentially
  case process.receive(messages, 100) {
    Ok(msg) -> {
      handle_engine_message(msg, state)
      client_loop(engine, config, state, messages)
    }
    Error(_) -> {
      // Timeout - perform random activity if connected
      case state.is_connected {
        True -> {
          let new_state = perform_random_activity(engine, config, state)
          client_loop(engine, config, new_state, messages)
        }
        False -> client_loop(engine, config, state, messages)
      }
    }
  }
}

fn handle_engine_message(message: ClientMessage, state: ClientState) -> Nil {
  case message {
    NewPost(post) -> {
      // Could react to new posts here
      Nil
    }
    NewComment(comment) -> {
      // Could react to new comments here
      Nil
    }
    PostVoteUpdate(post_id, upvotes, downvotes) -> {
      // Could track vote updates
      Nil
    }
    NewDirectMessage(dm) -> {
      // Could respond to DMs
      Nil
    }
  }
}

fn perform_random_activity(
  engine: Subject(EngineMessage),
  config: SimulatorConfig,
  state: ClientState
) -> ClientState {
  let activity_roll = utils.random_float()
  let adjusted_probability = fn(base: Float) { base *. state.activity_level }
  
  // Pre-calculate adjusted probabilities to avoid function calls in guards
  let post_prob = adjusted_probability(config.post_probability)
  let comment_prob = adjusted_probability(config.comment_probability)
  let vote_prob = adjusted_probability(config.vote_probability)
  let dm_prob = adjusted_probability(config.dm_probability)
  
  case activity_roll {
    _ if activity_roll <. post_prob -> {
      create_random_post(engine, state)
      state
    }
    _ if activity_roll <. post_prob +. comment_prob -> {
      create_random_comment(engine, state)
      state
    }
    _ if activity_roll <. post_prob +. comment_prob +. vote_prob -> {
      vote_randomly(engine, state)
      state
    }
    _ if activity_roll <. post_prob +. comment_prob +. vote_prob +. dm_prob -> {
      send_random_dm(engine, state)
      state
    }
    _ -> state
  }
}

fn join_random_subreddits(
  engine: Subject(EngineMessage),
  state: ClientState,
  available_subreddits: List(SubredditName),
  num_to_join: Int
) -> ClientState {
  // Simple random selection without shuffle
  let subreddits_to_join = select_random_elements(available_subreddits, num_to_join)
  
  let joined_subreddits = list.fold(subreddits_to_join, state.joined_subreddits, fn(acc, subreddit) {
    let response_subject = process.new_subject()
    process.send(engine, JoinSubreddit(state.user_id, subreddit, response_subject))
    
    case process.receive(response_subject, 1000) {
      Ok(Success(_)) -> [subreddit, ..acc]
      _ -> acc
    }
  })
  
  ClientState(..state, joined_subreddits: joined_subreddits)
}

fn select_random_elements(lst: List(a), count: Int) -> List(a) {
  do_select_random(lst, count, [])
}

fn do_select_random(lst: List(a), remaining: Int, acc: List(a)) -> List(a) {
  case remaining, lst {
    0, _ -> acc
    _, [] -> acc
    n, items -> {
      let index = utils.random_int(list.length(items))
      case list_at_index(items, index - 1) {
        Some(item) -> {
          let new_list = list.filter(items, fn(x) { x != item })
          do_select_random(new_list, n - 1, [item, ..acc])
        }
        None -> acc
      }
    }
  }
}

fn list_at_index(lst: List(a), index: Int) -> Option(a) {
  case index, lst {
    _, [] -> None
    0, [head, ..] -> Some(head)
    n, [_, ..tail] if n > 0 -> list_at_index(tail, n - 1)
    _, _ -> None
  }
}

fn create_random_post(engine: Subject(EngineMessage), state: ClientState) -> Nil {
  case state.joined_subreddits {
    [] -> Nil
    subreddits -> {
      let subreddit = list_random_element(subreddits)
      let response_subject = process.new_subject()
      
      // Occasionally create reposts for active users
      let should_repost = utils.random_float() <. 0.2 *. state.activity_level
      
      case should_repost {
        True -> {
          // Try to repost (would need to track existing posts)
          let title = utils.random_post_title()
          let content = utils.random_post_content()
          process.send(engine, CreatePost(
            state.user_id, subreddit, title, content, response_subject
          ))
        }
        False -> {
          let title = utils.random_post_title()
          let content = utils.random_post_content()
          process.send(engine, CreatePost(
            state.user_id, subreddit, title, content, response_subject
          ))
        }
      }
      Nil
    }
  }
}

fn create_random_comment(engine: Subject(EngineMessage), state: ClientState) -> Nil {
  // Get feed and comment on a random post
  let response_subject = process.new_subject()
  process.send(engine, GetFeed(state.user_id, 10, response_subject))
  
  case process.receive(response_subject, 1000) {
    Ok(Success(types.FeedData(posts))) -> {
      case posts {
        [] -> Nil
        _ -> {
          let post = list_random_element(posts)
          let content = utils.random_comment()
          process.send(engine, CreateComment(
            state.user_id, post.id, None, content, response_subject
          ))
          Nil
        }
      }
    }
    _ -> Nil
  }
}

fn vote_randomly(engine: Subject(EngineMessage), state: ClientState) -> Nil {
  let response_subject = process.new_subject()
  process.send(engine, GetFeed(state.user_id, 20, response_subject))
  
  case process.receive(response_subject, 1000) {
    Ok(Success(types.FeedData(posts))) -> {
      case posts {
        [] -> Nil
        _ -> {
          let post = list_random_element(posts)
          let vote = case utils.random_float() <. 0.8 {
            True -> Upvote
            False -> Downvote
          }
          process.send(engine, VotePost(state.user_id, post.id, vote, response_subject))
          Nil
        }
      }
    }
    _ -> Nil
  }
}

fn send_random_dm(engine: Subject(EngineMessage), state: ClientState) -> Nil {
  // Would need a way to get other users - simplified for now
  Nil
}

fn calculate_activity_level(client_id: Int, total_clients: Int, zipf_alpha: Float) -> Float {
  // Use inverse of Zipf rank for activity level
  let rank = utils.zipf_sample(total_clients, zipf_alpha)
  let assert Ok(rank_float) = int.to_float(rank)
  let assert Ok(total_float) = int.to_float(total_clients)
  1.0 -. (rank_float /. total_float)
}

fn list_random_element(lst: List(a)) -> a {
  let index = utils.random_int(list.length(lst))
  case list_at_index(lst, index - 1) {
    Some(element) -> element
    None -> {
      // Fallback to first element if something goes wrong
      let assert [first, ..] = lst
      first
    }
  }
}