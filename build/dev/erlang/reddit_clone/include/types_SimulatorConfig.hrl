-record(simulator_config, {
    num_users :: integer(),
    num_subreddits :: integer(),
    simulation_duration_ms :: integer(),
    zipf_alpha :: float(),
    connection_probability :: float(),
    post_probability :: float(),
    comment_probability :: float(),
    vote_probability :: float(),
    dm_probability :: float(),
    repost_probability :: float()
}).
