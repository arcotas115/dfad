# Reddit Clone - Gleam Implementation

A Reddit-like engine with REST API, digital signatures, search functionality, and multi-client simulation.

## Features

### Core Features (Part 1 & 2)
- ✅ User registration and authentication
- ✅ Create and join subreddits
- ✅ Post in subreddits
- ✅ Hierarchical comments (comment on comments)
- ✅ Upvote/downvote with karma calculation
- ✅ User feed based on subscriptions
- ✅ Direct messages with reply support
- ✅ REST API interface
- ✅ Request logging (visible in server console)
- ✅ Search functionality (posts, subreddits, users)
- ✅ Multi-client concurrent simulation
- ✅ Zipf distribution for subreddit membership
- ✅ Enhanced metrics with rate calculations

### Bonus Features
- ✅ Public key registration during user signup
- ✅ Digital signature on posts
- ✅ Signature verification when downloading posts
- ✅ Public key retrieval endpoint

## Project Structure

```
reddit_clone/
├── src/
│   ├── reddit_clone.gleam      # Main server entry point
│   ├── reddit_engine.gleam     # Core engine actor with all logic
│   ├── api_server.gleam        # REST API with logging
│   ├── types.gleam             # All type definitions
│   ├── json_codec.gleam        # JSON encoding/decoding
│   ├── utils.gleam             # Utilities including crypto
│   ├── cli_client.gleam        # Single-client test runner
│   └── client_simulator.gleam  # Multi-client concurrent simulator
├── test/
│   └── reddit_clone_test.gleam # Unit tests
├── gleam.toml                  # Project configuration
└── README.md
```

## How to Run

### 1. Start the Server

```bash
gleam run
```

This starts the REST API server on `http://localhost:8080`.

You'll see request logging in the console showing all API communication.

### 2. Run the CLI Client Tests

In a separate terminal:

```bash
gleam run -m cli_client
```

This runs through all 21 test cases demonstrating:
- User registration with public key
- Login/authentication
- Subreddit creation and joining
- Signed post creation
- Post retrieval with signature verification
- Comments and voting
- Direct messages
- Search functionality
- Statistics with rate metrics

### 3. Run Multi-Client Simulation

In a separate terminal:

```bash
gleam run -m client_simulator
```

This spawns 10 concurrent client processes that:
- Register users with key pairs
- Join subreddits based on Zipf distribution
- Create signed posts
- Comment on posts
- Vote on content
- Send direct messages

### 4. Run Tests

```bash
gleam test
```

## API Endpoints

### User Management
| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/api/register` | Register user (with optional `public_key`) |
| POST | `/api/login` | Authenticate user |
| GET | `/api/user/:username/karma` | Get user karma |
| GET | `/api/user/:username/publickey` | Get user's public key |

### Subreddit Management
| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/subreddits` | List all subreddits |
| GET | `/api/subreddit/:name` | Get specific subreddit |
| POST | `/api/subreddit` | Create subreddit |
| POST | `/api/subreddit/:name/join` | Join subreddit |
| POST | `/api/subreddit/:name/leave` | Leave subreddit |

### Posts
| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/posts` | Get user's feed |
| GET | `/api/subreddit/:name/posts` | Get subreddit posts |
| GET | `/api/post/:id` | Get post (verifies signature) |
| POST | `/api/post` | Create post (with optional `signature`) |
| POST | `/api/post/:id/upvote` | Upvote post |
| POST | `/api/post/:id/downvote` | Downvote post |

### Comments
| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/post/:id/comments` | Get post comments |
| POST | `/api/comment` | Create comment |
| POST | `/api/comment/:id/upvote` | Upvote comment |
| POST | `/api/comment/:id/downvote` | Downvote comment |

### Messages
| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/messages` | Get user's messages |
| POST | `/api/message` | Send direct message |

### Search
| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/search/posts?q=` | Search posts |
| GET | `/api/search/subreddits?q=` | Search subreddits |
| GET | `/api/search/users?q=` | Search users |

### Statistics
| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/stats` | Get system stats with rates |
| GET | `/api/clients` | Get active clients count |

## Digital Signature Flow (Bonus Feature)

1. **Registration**: User provides a public key during registration
   ```json
   {
     "username": "alice",
     "password": "secret123",
     "public_key": "abc123..."
   }
   ```

2. **Get Public Key**: Any user can retrieve another user's public key
   ```
   GET /api/user/alice/publickey
   ```

3. **Create Signed Post**: Posts include a signature
   ```json
   {
     "subreddit": "programming",
     "title": "Hello World",
     "content": "This is my post",
     "signature": "signed_hash..."
   }
   ```

4. **Verify on Download**: When fetching a post, the signature is verified
   ```json
   {
     "post": {
       "title": "Hello World",
       "signature": "signed_hash...",
       "signature_valid": true
     }
   }
   ```

## Request Logging

The server logs all requests in the format:
```
[timestamp] RECEIVED GET /api/posts
[timestamp] ✅ GET /api/posts -> 200 (5ms)
```

Status indicators:
- ✅ Success (2xx)
- ⚠️ Client error (4xx)
- ❌ Server error (5xx)

## Statistics Response

The `/api/stats` endpoint returns comprehensive metrics:
```json
{
  "status": "success",
  "data": {
    "total_users": 10,
    "total_subreddits": 5,
    "total_posts": 25,
    "total_comments": 50,
    "active_sessions": 3,
    "uptime_seconds": 120,
    "start_time": 1700000000000,
    "last_update_time": 1700000120000,
    "rates": {
      "posts_per_second": 0.5,
      "comments_per_second": 1.2,
      "votes_per_second": 2.0
    },
    "performance": {
      "total_posts_created": 25,
      "total_comments_created": 50,
      "total_votes_cast": 100,
      "messages_processed": 500
    }
  }
}
```

## Video Demo Checklist

For the required 5-minute demo video, show:

1. **Starting the server** - `gleam run`
2. **Show request logging** - Point out the console output
3. **Run CLI client** - `gleam run -m cli_client`
4. **Demonstrate REST API communication** - Show logs matching requests
5. **Run multi-client simulator** - `gleam run -m client_simulator`
6. **Show concurrent client activity** - Multiple users acting simultaneously
7. **Show statistics** - Call `/api/stats` to show metrics

For the bonus demo, additionally show:
1. Registration with public key
2. Creating a signed post
3. Retrieving the post with signature verification
4. Getting a user's public key

## Dependencies

- gleam_stdlib >= 0.65.0
- gleam_otp >= 1.2.0
- gleam_erlang >= 1.3.0
- gleam_json >= 3.0.2
- gleam_http >= 4.3.0
- gleam_crypto >= 1.5.1
- mist >= 2.0.0
- gleam_httpc >= 5.0.0
