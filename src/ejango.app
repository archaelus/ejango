{application, ejango,
 [{description, "Erlang Django web framework and utilities"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib, crypto, erlydtl]},
  {modules, [ ejango.domain
             ,ejango.email_address
             ,ejango.email_tokens
             ,ejango.form
             ,ejango.form_validator
             ,ejango.passwords
             ,ejango.url_routes
             ]}
 ]}.
