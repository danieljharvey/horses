const main = require('./cjs-b961573105148986af6e132aeacbed24c996a7209c41759965c8ac24b923f351.js').main;
const server = (url) => (cb) => {
  // run the function, passing the url
  const t = main(url)
  // pass the callback to the generated Task to make it All Happen
  t.vars[0](cb)
}

module.exports = { main: server };
