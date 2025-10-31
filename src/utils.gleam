// src/reddit_clone/utils.gleam
// Utility functions for the Reddit clone

import gleam/crypto
import gleam/erlang/atom
import gleam/float
import gleam/int
import gleam/list
import gleam/string

// Generate a unique ID
pub fn generate_id(prefix: String) -> String {
  let random_bytes = crypto.strong_random_bytes(8)
  let hex = encode_hex(random_bytes)
  prefix <> "_" <> hex
}

// Encode bytes to hex string
fn encode_hex(bytes: BitArray) -> String {
  bytes
  |> bit_array_to_list
  |> list.map(fn(byte) {
    let hex = int.to_base16(byte)
    case string.length(hex) {
      1 -> "0" <> hex
      _ -> hex
    }
  })
  |> string.join("")
}

// Convert BitArray to list of ints
fn bit_array_to_list(bits: BitArray) -> List(Int) {
  do_bit_array_to_list(bits, [])
  |> list.reverse
}

fn do_bit_array_to_list(bits: BitArray, acc: List(Int)) -> List(Int) {
  case bits {
    <<byte:8, rest:bytes>> -> do_bit_array_to_list(rest, [byte, ..acc])
    <<>> -> acc
    _ -> acc
  }
}

// Get current timestamp in milliseconds
@external(erlang, "erlang", "system_time")
fn erlang_system_time(unit: atom.Atom) -> Int

pub fn current_timestamp() -> Int {
  erlang_system_time(atom.create("millisecond"))
}

// Random number generator
@external(erlang, "rand", "uniform")
fn rand_uniform() -> Float

@external(erlang, "rand", "uniform")
fn rand_uniform_int(n: Int) -> Int

pub fn random_float() -> Float {
  rand_uniform()
}

pub fn random_int(max: Int) -> Int {
  rand_uniform_int(max)
}

// Zipf distribution generator
// Returns a value between 1 and n following Zipf distribution with parameter alpha
pub fn zipf_sample(n: Int, alpha: Float) -> Int {
  let h_n = harmonic_number(n, alpha)
  let u = random_float() *. h_n
  find_zipf_value(1, n, u, alpha, 0.0)
}

fn harmonic_number(n: Int, alpha: Float) -> Float {
  case n {
    0 -> 0.0
    _ -> {
      list.range(1, n)
      |> list.map(fn(i) {
        let i_float = int.to_float(i)
        let assert Ok(power_result) = float.power(i_float, alpha)
        1.0 /. power_result
      })
      |> list.fold(0.0, float.add)
    }
  }
}

fn find_zipf_value(i: Int, n: Int, u: Float, alpha: Float, acc: Float) -> Int {
  case i > n {
    True -> n
    False -> {
      let i_float = int.to_float(i)
      let assert Ok(power_result) = float.power(i_float, alpha)
      let prob = 1.0 /. power_result
      let new_acc = acc +. prob
      case new_acc >=. u {
        True -> i
        False -> find_zipf_value(i + 1, n, u, alpha, new_acc)
      }
    }
  }
}

// Generate random string for usernames
pub fn random_username() -> String {
  let adjectives = [
    "Happy", "Clever", "Swift", "Brave", "Wise", "Noble", "Mighty", "Gentle",
    "Bold", "Bright", "Cool", "Epic", "Fierce", "Grand",
  ]
  let nouns = [
    "Tiger", "Eagle", "Wolf", "Bear", "Lion", "Hawk", "Falcon", "Dragon",
    "Phoenix", "Warrior", "Knight", "Wizard", "Sage", "Hero",
  ]

  let adj = list_at(adjectives, random_int(list.length(adjectives)))
  let noun = list_at(nouns, random_int(list.length(nouns)))
  let number = int.to_string(random_int(9999))

  adj <> noun <> number
}

fn list_at(lst: List(String), index: Int) -> String {
  case list.drop(lst, index - 1) {
    [head, ..] -> head
    [] -> "Unknown"
  }
}

// Generate random subreddit names
pub fn random_subreddit_name() -> String {
  let prefixes = [
    "Ask", "Learn", "Share", "Discuss", "Show", "Tell", "Find", "Explore",
    "Discover", "Master", "Build", "Create",
  ]
  let topics = [
    "Programming", "Science", "Art", "Music", "Gaming", "Books", "Movies",
    "Food", "Travel", "Tech", "Sports", "History", "Philosophy", "Nature",
    "Space", "Math", "Design",
  ]

  let prefix = list_at(prefixes, random_int(list.length(prefixes)))
  let topic = list_at(topics, random_int(list.length(topics)))

  prefix <> topic
}

// Generate random post content
pub fn random_post_title() -> String {
  let templates = [
    "What's your opinion on ",
    "Just discovered ",
    "Tips for improving ",
    "My experience with ",
    "Anyone else think ",
    "Best practices for ",
    "How to master ",
    "The truth about ",
    "Why I love ",
    "Discussion: ",
    "Unpopular opinion: ",
    "PSA: Remember to ",
  ]

  let topics = [
    "this new framework",
    "modern development",
    "learning techniques",
    "productivity tips",
    "industry trends",
    "recent updates",
    "community guidelines",
    "best practices",
    "common mistakes",
    "advanced features",
    "beginner resources",
    "career advice",
  ]

  let template = list_at(templates, random_int(list.length(templates)))
  let topic = list_at(topics, random_int(list.length(topics)))

  template <> topic
}

pub fn random_post_content() -> String {
  let paragraphs = [
    "I've been thinking about this topic for a while now, and I wanted to share my thoughts with the community.",
    "After extensive research and experimentation, I've come to some interesting conclusions.",
    "This might be controversial, but I believe it's important to discuss different perspectives.",
    "Based on my experience over the past few years, here's what I've learned.",
    "I'm curious to hear what others think about this. Please share your opinions!",
    "This has been a game-changer for me, and I think it could help others too.",
    "Let's have a constructive discussion about the pros and cons.",
    "I've compiled some resources that might be helpful for anyone interested in this topic.",
  ]

  let num_paragraphs = random_int(3) + 1
  list.range(1, num_paragraphs)
  |> list.map(fn(_) { list_at(paragraphs, random_int(list.length(paragraphs))) })
  |> string.join("\n\n")
}

pub fn random_comment() -> String {
  let comments = [
    "Great post! I completely agree with your points.",
    "Interesting perspective. Have you considered the alternative?",
    "Thanks for sharing! This is really helpful.",
    "I had a similar experience. Here's what worked for me...",
    "Could you elaborate on that last point?",
    "This is exactly what I needed to read today!",
    "I respectfully disagree. Here's why...",
    "Adding to your point, I think it's also important to consider...",
    "Has anyone tried this approach in production?",
    "Sources? I'd love to read more about this.",
    "This deserves more upvotes!",
    "Quality content. Thanks for taking the time to write this.",
  ]

  list_at(comments, random_int(list.length(comments)))
}

pub fn random_dm_content() -> String {
  let messages = [
    "Hey! I saw your post and wanted to reach out.",
    "Thanks for your contribution to the community!",
    "Quick question about your recent comment...",
    "I really appreciated your insights on that topic.",
    "Would you be interested in collaborating?",
    "Just wanted to say thanks for the helpful advice!",
    "Could you recommend any resources on this subject?",
    "Your post inspired me to try something new!",
  ]

  list_at(messages, random_int(list.length(messages)))
}

// Calculate metrics
pub fn calculate_engagement_rate(
  posts: Int,
  comments: Int,
  votes: Int,
  users: Int,
) -> Float {
  case users {
    0 -> 0.0
    n -> {
      let total_float = int.to_float(posts + comments + votes)
      let users_float = int.to_float(n)
      total_float /. users_float
    }
  }
}

// Format time duration
pub fn format_duration(milliseconds: Int) -> String {
  let seconds = milliseconds / 1000
  let minutes = seconds / 60
  let hours = minutes / 60

  case hours {
    0 ->
      case minutes {
        0 -> int.to_string(seconds) <> "s"
        m -> int.to_string(m) <> "m " <> int.to_string(seconds % 60) <> "s"
      }
    h -> int.to_string(h) <> "h " <> int.to_string(minutes % 60) <> "m"
  }
}

// Sleep for specified milliseconds
@external(erlang, "timer", "sleep")
pub fn sleep(milliseconds: Int) -> Nil
