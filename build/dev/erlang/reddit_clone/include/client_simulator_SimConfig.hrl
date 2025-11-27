-record(sim_config, {
    num_users :: integer(),
    num_subreddits :: integer(),
    simulation_duration_ms :: integer(),
    actions_per_user :: integer(),
    zipf_alpha :: float(),
    connection_probability :: float(),
    post_probability :: float(),
    comment_probability :: float(),
    vote_probability :: float(),
    dm_probability :: float(),
    base_url :: binary()
}).
