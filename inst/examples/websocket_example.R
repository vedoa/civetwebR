# A stateful WebSocket example:
# 1. Keeps track of active clients
# 2. Echoes messages back to the sender
# 3. Broadcasts "Join/Leave" notifications to everyone

library(civetwebR)

# Internal state to track client IDs
active_clients <- new.env()

broadcast <- function(msg) {
  client_ids <- names(active_clients)
  for (id_str in client_ids) {
    success <- ws_send(as.integer(id_str), msg)
    if (!success) {
      # Connection is dead, cleanup if close event hasn't arrived yet
      rm(list = id_str, envir = active_clients)
    }
  }
}

on_ws("open", function(req) {
  client_id <- as.character(req$id)
  active_clients[[client_id]] <- TRUE

  message(sprintf("Client %s connected", client_id))
  broadcast(sprintf("System: User %s joined the room", client_id))
})

on_ws("message", function(req) {
  msg <- rawToChar(req$body)
  client_id <- req$id

  message(sprintf("Message from %s: %s", client_id, msg))

  # Echo back to the sender
  ws_send(client_id, paste("You said:", msg))
})

on_ws("close", function(req) {
  client_id <- as.character(req$id)
  if (exists(client_id, envir = active_clients)) {
    rm(list = client_id, envir = active_clients)
  }

  message(sprintf("Client %s disconnected", client_id))
  broadcast(sprintf("System: User %s left the room", client_id))
})

# Serve a simple HTML page that connects to the websocket
handle("GET", "/", function(req) {
  list(
    status = 200L,
    headers = list("Content-Type" = "text/html"),
    body = '
    <style>
      body { font-family: sans-serif; padding: 20px; }
      #log { border:1px solid #ccc; height: 300px; overflow: auto; background: #f9f9f9; padding: 10px; margin-bottom: 10px; }
      .status { font-size: 0.8em; margin-bottom: 5px; }
    </style>
    <h3>civetwebR WebSocket Demo</h3>
    <div id="status" class="status">Connecting...</div>
    <div id="log"></div>
    <input id="inp" placeholder="Type a message..." style="width: 80%" onkeypress="if(event.key===\'Enter\' && ws.readyState === 1){ws.send(this.value); this.value=\'\'}">
    <script>
      var log = document.getElementById("log");
      var status = document.getElementById("status");
      var ws = new WebSocket("ws://" + location.host + "/ws");
      ws.onmessage = function(e) { 
        var div = document.createElement("div");
        div.textContent = e.data;
        log.appendChild(div);
        log.scrollTop = log.scrollHeight; 
      };
      ws.onopen = function() { 
        status.innerHTML = "<b style=\'color:green\'>Connected</b>";
      };
      ws.onclose = function() { 
        status.innerHTML = "<b style=\'color:red\'>Disconnected</b>";
      };
      ws.onerror = function() { 
        status.innerHTML = "<b style=\'color:orange\'>Error</b>";
      };
    </script>
  '
  )
})

message("Starting WebSocket demo at http://127.0.0.1:8080")
serve(port = 8080, timeout_ms = 10L)
