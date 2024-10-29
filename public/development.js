const DEBOUNCE_TIME = 500;

let RELOAD_TIMEOUT;

function connect() {
  let socket = new WebSocket("ws://localhost:10000/ws");

  socket.onopen = function (e) {
    console.log("HMR is ready.");
  };

  socket.onmessage = function (event) {
    console.log("Reloading...");
    clearTimeout(RELOAD_TIMEOUT);
    RELOAD_TIMEOUT = setTimeout(() => {
      location.reload();
    }, DEBOUNCE_TIME);
  };

  socket.onclose = function (e) {
    connect();
  };

  socket.onerror = function (err) {
    socket.close();
  };
}

connect();
