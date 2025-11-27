{application, reddit_clone, [
    {vsn, "2.0.0"},
    {applications, [gleam_crypto,
                    gleam_erlang,
                    gleam_http,
                    gleam_httpc,
                    gleam_json,
                    gleam_otp,
                    gleam_stdlib,
                    gleeunit,
                    mist]},
    {description, "A Reddit clone with REST API, search, and digital signatures"},
    {modules, []},
    {registered, []}
]}.
