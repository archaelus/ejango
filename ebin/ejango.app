{application, ejango,
 [{description, "Erlang Django web framework and utilities"},
  {vsn, "0.2"},
  {applications, [kernel, stdlib, crypto, erlydtl]},
  {modules, [ ej_domain
             ,ej_email_address
             ,ej_email_tokens
             ,ej_form
             ,ej_form_validator
             ,ej_form_validator_tests
             ,ej_passwords
             ,ej_static
             ,ej_tokens
             ,ej_url_routes
             ]}
 ]}.
