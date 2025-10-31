// src/client_simulator.gleam
// Client simulator with Zipf distribution for subreddit popularity

import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import types.{
  type ClientMessage, type ClientState, type EngineMessage, type SubredditName,
  ClientState, CreateComment, CreatePost, Downvote, GetFeed, JoinSubreddit,
  LoginUser, NewComment, NewDirectMessage, NewPost, PostVoteUpdate, RegisterUser,
  Upvote, VotePost,
}
import utils

pub type ClientControl {
  Stop
}

// Start client with Zipf distribution
pub fn start_client(
  engine: Subject(EngineMessage),
  config: types.SimulatorConfig,
  client_id: Int,
  all_subreddits: List(SubredditName),
) -> Subject(ClientControl) {
  let control_subject = process.new_subject()

  process.spawn_unlinked(fn() {
    let username = utils.random_username() <> "_" <> int.to_string(client_id)

    // FIRE-AND-FORGET registration
    process.send(engine, RegisterUser(username))

    utils.sleep(20 + utils.random_int(80))

    // Calculate activity based on Zipf
    let activity_level =
      calculate_activity_level(client_id, config.num_users, config.zipf_alpha)

    let initial_state =
      ClientState(
        user_id: username,
        username: username,
        is_connected: True,
        joined_subreddits: [],
        activity_level: activity_level,
      )

    let client_messages = process.new_subject()

    // FIRE-AND-FORGET login
    process.send(engine, LoginUser(username, client_messages))

    utils.sleep(50)

    // Join subreddits using Zipf distribution (popular subreddits more likely)
    let num_subreddits_to_join =
      calculate_subreddit_count(activity_level, list.length(all_subreddits))

    let state_with_subreddits =
      join_subreddits_zipf(
        engine,
        initial_state,
        all_subreddits,
        num_subreddits_to_join,
        config.zipf_alpha,
      )

    io.println(
      "✓ Client "
      <> username
      <> " ready (activity: "
      <> float.to_string(activity_level)
      <> ")",
    )

    client_loop(engine, config, state_with_subreddits, client_messages)
  })

  control_subject
}

fn client_loop(
  engine: Subject(EngineMessage),
  config: types.SimulatorConfig,
  state: ClientState,
  messages: Subject(ClientMessage),
) -> Nil {
  case process.receive(messages, 100) {
    Ok(msg) -> {
      let _ = handle_engine_message(msg)
      client_loop(engine, config, state, messages)
    }
    _ -> {
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

fn handle_engine_message(message: ClientMessage) -> Nil {
  case message {
    NewPost(_) -> Nil
    NewComment(_) -> Nil
    PostVoteUpdate(_, _, _) -> Nil
    NewDirectMessage(_) -> Nil
  }
}

fn perform_random_activity(
  engine: Subject(EngineMessage),
  config: types.SimulatorConfig,
  state: ClientState,
) -> ClientState {
  let activity_roll = utils.random_float()
  let adjusted_probability = fn(base: Float) { base *. state.activity_level }

  let post_prob = adjusted_probability(config.post_probability)
  let comment_prob = adjusted_probability(config.comment_probability)
  let vote_prob = adjusted_probability(config.vote_probability)

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
    _ -> state
  }
}

// Join subreddits using Zipf distribution (popular ones more likely)
fn join_subreddits_zipf(
  engine: Subject(EngineMessage),
  state: ClientState,
  available_subreddits: List(SubredditName),
  num_to_join: Int,
  zipf_alpha: Float,
) -> ClientState {
  let subreddits_to_join =
    select_subreddits_zipf(available_subreddits, num_to_join, zipf_alpha)

  // FIRE-AND-FORGET joins
  list.each(subreddits_to_join, fn(subreddit) {
    process.send(engine, JoinSubreddit(state.user_id, subreddit))
    utils.sleep(10)
  })

  ClientState(..state, joined_subreddits: subreddits_to_join)
}

// Select subreddits using Zipf - first subreddits are most popular
fn select_subreddits_zipf(
  subreddits: List(SubredditName),
  count: Int,
  zipf_alpha: Float,
) -> List(SubredditName) {
  let total = list.length(subreddits)

  list.range(1, count)
  |> list.map(fn(_) {
    // Use Zipf to pick subreddit rank (1 is most popular)
    let rank = utils.zipf_sample(total, zipf_alpha)
    list_at_index(subreddits, rank - 1)
  })
  |> list.filter_map(fn(opt) {
    case opt {
      Some(s) -> Ok(s)
      None -> Error(Nil)
    }
  })
  |> deduplicate
}

// Remove duplicates
fn deduplicate(lst: List(a)) -> List(a) {
  do_deduplicate(lst, [])
}

fn do_deduplicate(lst: List(a), acc: List(a)) -> List(a) {
  case lst {
    [] -> list.reverse(acc)
    [head, ..tail] -> {
      case list.contains(acc, head) {
        True -> do_deduplicate(tail, acc)
        False -> do_deduplicate(tail, [head, ..acc])
      }
    }
  }
}

fn calculate_subreddit_count(
  activity_level: Float,
  total_subreddits: Int,
) -> Int {
  let base_count = float.round(activity_level *. int.to_float(total_subreddits))
  int.max(1, int.min(base_count, total_subreddits))
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
      case list_random_element(subreddits) {
        Some(subreddit) -> {
          let title = utils.random_post_title()
          let content = utils.random_post_content()
          process.send(
            engine,
            CreatePost(state.user_id, subreddit, title, content),
          )
          Nil
        }
        None -> Nil
      }
    }
  }
}

fn create_random_comment(
  engine: Subject(EngineMessage),
  state: ClientState,
) -> Nil {
  let response_subject = process.new_subject()
  process.send(engine, GetFeed(state.user_id, 10, response_subject))

  case process.receive(response_subject, 200) {
    Ok(types.Success(types.FeedData(posts))) -> {
      case list_random_element(posts) {
        Some(post) -> {
          let content = utils.random_comment()
          process.send(
            engine,
            CreateComment(state.user_id, post.id, None, content),
          )
          Nil
        }
        None -> Nil
      }
    }
    _ -> Nil
  }
}

fn vote_randomly(engine: Subject(EngineMessage), state: ClientState) -> Nil {
  let response_subject = process.new_subject()
  process.send(engine, GetFeed(state.user_id, 20, response_subject))

  case process.receive(response_subject, 200) {
    Ok(types.Success(types.FeedData(posts))) -> {
      case list_random_element(posts) {
        Some(post) -> {
          let vote = case utils.random_float() <. 0.8 {
            True -> Upvote
            False -> Downvote
          }
          process.send(engine, VotePost(state.user_id, post.id, vote))
          Nil
        }
        None -> Nil
      }
    }
    _ -> Nil
  }
}

// Calculate activity level using Zipf distribution
// ✅ FIX: Prefix unused parameter with underscore
fn calculate_activity_level(
  _client_id: Int,
  total_clients: Int,
  zipf_alpha: Float,
) -> Float {
  let rank = utils.zipf_sample(total_clients, zipf_alpha)
  let rank_float = int.to_float(rank)
  let total_float = int.to_float(total_clients)
  // Higher rank = lower activity (rank 1 is most active)
  1.0 -. { rank_float /. total_float }
}

fn list_random_element(lst: List(a)) -> Option(a) {
  case lst {
    [] -> None
    items -> {
      let index = utils.random_int(list.length(items))
      list_at_index(items, index)
    }
  }
}
