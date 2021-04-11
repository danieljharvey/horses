
# create project

echo "Create project!"

curl -f 'http://localhost:8999/project/create'

# create expression

echo "Create expression"

curl -f -XPOST -H "Content-type: application/json" -d '{"beProjectHash":"d968854d47ef92676b3c32c4c5c90329aabfac7935f6cb9de645c8cbe9cfc09f","beExpression":"let update =\n  \\state ->\n    \\url ->\n      if (url == \"up\")\n      then\n        ((state + 1,\n          ({ data: \"Up\",\n             status: 200 })))\n      else\n        ((state,\n          ({ data: \"No\",\n             status: 400 })));\n\n{ init: 0, next: update }","beBindingName":"server1"}' 'http://127.0.0.1:8999/project/bind'

# exprhash: 7c64f7d21a941300bdf2575ba3a1daa9c105471d38f82e59a8b9ee353b0d587a

# create server

echo "Create server"

curl -f -XPOST -H "Content-type: application/json" -d '{ "feExprHash": "7c64f7d21a941300bdf2575ba3a1daa9c105471d38f82e59a8b9ee353b0d587a" }' 'http://127.0.0.1:8666/fetch/expr'

# sleep for a moment

echo "SLEEP"

sleep 0.5

# run api

echo "Call API"

curl 'localhost:8666/2/up'

curl 'localhost:8666/2/up' | grep "Up" && echo "Success" || echo "Failure!"

