test_that("integration: websocket handshake and echo work", {
  skip_on_cran()
  skip_if_not_installed("callr")

  port <- sample(10000:20000, 1)
  pkg_path <- normalizePath(test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(port, lib_paths, pkg_path) {
      .libPaths(lib_paths)
      if (file.exists(file.path(pkg_path, "DESCRIPTION"))) {
        pkgload::load_all(pkg_path)
      } else {
        library(civetwebR)
      }

      # Register an echo handler for testing
      on_ws("message", function(req) {
        msg <- rawToChar(req$body)
        # Send the message back to the same client ID
        ws_send(req$id, paste0("ECHO:", msg))
      })

      serve(
        port = port,
        host = "127.0.0.1",
        quiet = TRUE,
        timeout_ms = 10L # High responsiveness for tests
      )
    },
    args = list(port, .libPaths(), pkg_path)
  )

  on.exit(
    {
      if (p$is_alive()) p$kill()
    },
    add = TRUE
  )

  wait_for_server(port, p)

  # Connect via raw socket to simulate a WebSocket client
  con <- socketConnection(
    host = "127.0.0.1",
    port = port,
    open = "r+b",
    blocking = TRUE
  )
  on.exit(close(con), add = TRUE)

  # 1. Send WebSocket Handshake
  writeLines(
    c(
      "GET /ws HTTP/1.1",
      "Host: 127.0.0.1",
      "Upgrade: websocket",
      "Connection: Upgrade",
      "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==", # standard sample key
      "Sec-WebSocket-Version: 13",
      ""
    ),
    con,
    sep = "\r\n"
  )
  flush(con)

  # 2. Verify 101 Switching Protocols response
  status_line <- readLines(con, n = 1, warn = FALSE)
  expect_match(status_line, "101 Switching Protocols")

  # Consume the rest of the response headers
  repeat {
    line <- readLines(con, n = 1, warn = FALSE)
    if (line == "" || line == "\r") break
  }

  # 3. Send a Masked Text Frame (RFC 6455 requirement for clients)
  # Payload: "PING" (4 bytes)
  payload <- charToRaw("PING")
  mask_key <- as.raw(c(0x01, 0x02, 0x03, 0x04))
  masked_payload <- as.raw(bitwXor(as.integer(payload), as.integer(mask_key)))

  # Frame: [Opcode 0x81 (Fin + Text)] [Mask=1, Len=4] [4-byte Mask Key] [Payload]
  writeBin(as.raw(c(0x81, 0x84)), con)
  writeBin(mask_key, con)
  writeBin(masked_payload, con)
  flush(con)

  # 4. Read Response Frame (Servers MUST NOT mask)
  # Expected Payload: "ECHO:PING" (9 bytes)
  header <- readBin(con, "raw", 2)
  expect_equal(header[1], as.raw(0x81)) # Opcode check
  len <- as.integer(header[2])
  expect_equal(len, 9)

  res_body <- readBin(con, "raw", len)
  expect_equal(rawToChar(res_body), "ECHO:PING")
})
