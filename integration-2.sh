
# create project

# echo -e "Create project!\n"

# curl -f 'http://localhost:8999/project/create'

# create expression

# echo -e "Create expression\n"

# curl -f -XPOST -H "Content-type: application/json" -d '{"beProjectHash":"d968854d47ef92676b3c32c4c5c90329aabfac7935f6cb9de645c8cbe9cfc09f","beExpression":"let update =\n  \\state ->\n    \\url ->\n      if (url == \"up\")\n      then\n        ((state + 1,\n          ({ data: \"Up\",\n             status: 200 })))\n      else\n        ((state,\n          ({ data: \"No\",\n             status: 400 })));\n\n{ init: 0, next: update }","beBindingName":"server1"}' 'http://127.0.0.1:8999/project/bind'

# exprhash: 7c64f7d21a941300bdf2575ba3a1daa9c105471d38f82e59a8b9ee353b0d587a

# create server

echo -e "Create server\n"

curl -f -XPOST -H "Content-type: application/json" -d '{ "feExprHash": "b961573105148986af6e132aeacbed24c996a7209c41759965c8ac24b923f351" }' 'http://127.0.0.1:8666/fetch/expr'

# run api

echo -e "Call API\n"

curl 'localhost:8666/1/pug' | grep "jpg" && echo -e "Success" || echo "Failure!"

