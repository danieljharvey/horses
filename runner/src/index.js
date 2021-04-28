var http = require("http");

const port = parseInt(process.env.PORT,10) || 8080;

const volumePath = process.env["VOLUME_PATH"] || "";

// serverId is the hash of the js file in question
const createResponder = serverId => {
  const srcPath = `${volumePath}/cjs-${serverId}.js`;

  try {
    const mimsaServer = require(srcPath).main;

    let mutableState = mimsaServer.init;

    function respond(cleanUrl, callback) {
      const [state, response] = mimsaServer.next(mutableState)(cleanUrl);
      mutableState = state;
      callback(response)
    }

    return {
      serverId,
      respond
    };
  } catch {
    console.error(`Could not find JS files at ${srcPath}`);
    return undefined;
  }
};

const writeResponse = (res) => ({status,data}) => {
  // write response
  res.writeHead(status, { "Content-Type": "text/html" });
  res.write(data);
  res.end();
}

const createServer = () => {
  let servers = {};

  function masterRespond(req, res) {
    const cleanUrl = req.url.substring(1);
    const serverId = req.headers["mimsa-root-exprhash"];

    // if server is not there, try and add it
    if (!(serverId in servers)) {
      const responder = createResponder(serverId);
      servers[serverId] = responder;
    }

    const server = servers[serverId];
    if (server) {
      server.respond(cleanUrl, writeResponse(res))
    } else {
      writeResponse(res)(  { status: 500, data: `Server ${serverId} not found` })
    }
  }

  //create a server object:
  const app = http.createServer(masterRespond).listen(port); //the server object listens on port 8080
  console.log(`Javascript runner listening on ${port}`);

  process.on("SIGTERM", () => {
    console.info("SIGTERM signal received.");
    console.log("Closing http server.");
    app.close(() => {
      console.log("Http server closed.");
    });
  });
};

createServer();
