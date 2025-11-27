-record(performance_metrics, {
    total_posts_created :: integer(),
    total_comments_created :: integer(),
    total_votes_cast :: integer(),
    total_messages_sent :: integer(),
    messages_processed :: integer(),
    last_rate_check :: integer(),
    posts_since_last_check :: integer(),
    comments_since_last_check :: integer(),
    votes_since_last_check :: integer(),
    posts_per_second :: float(),
    comments_per_second :: float(),
    votes_per_second :: float()
}).
