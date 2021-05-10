
# create project

echo -e "Create project!\n"

curl -f 'http://localhost:8999/project/create'

# create expression

echo -e "Create expression\n"

curl -f -XPOST -H "Content-type: application/json" \
  -d '{"beProjectHash":"52c6f3211de9d7dbcb73bceddb313c8ea3199c51202ff76218d53c00991cff54", "beExpression":"\\a -> Task (\\r -> r(a))","beBindingName":"pureTask"}' \
  'http://127.0.0.1:8999/project/bind'

# new project hash: a0ba85488ec6b94af9c5f4af4c87aaafe2cbcaf4f356e1c029075607fed4cae5

curl -f -XPOST -H "Content-type: application/json" \
  -d '{"beProjectHash":"a0ba85488ec6b94af9c5f4af4c87aaafe2cbcaf4f356e1c029075607fed4cae5", "beExpression":"\\url -> if (url == \"healthz\") then pureTask(({ data: \"OK\", status: 200 })) else pureTask(({ data: \"Error\", status: 500 }))","beBindingName":"api1"}' \
  'http://127.0.0.1:8999/project/bind'

# exprhash: 7c64f7d21a941300bdf2575ba3a1daa9c105471d38f82e59a8b9ee353b0d587a

# create server

echo -e "Create server\n"

curl -f -XPOST -H "Content-type: application/json" -d '{ "feExprHash": "473b2818430785f170ccf76d36b2209eccd0c774604791cd110cd69bb0765437" }' 'http://127.0.0.1:8666/fetch/expr'

# run api

echo -e "Call API expecting 200: OK\n"

curl 'localhost:8666/1/healthz' | grep "OK" && echo -e "Success" || echo "Failure!"

echo -e "Call API expecting 500: Error\n"

curl 'localhost:8666/1/other' | grep "Error" && echo -e "Success" || echo "Failure!"

