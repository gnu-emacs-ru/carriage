#!/usr/bin/env bash
set -euo pipefail

EMACS="${EMACS:-emacs}"
EMACSCLIENT="${EMACSCLIENT:-emacsclient}"

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LISP_DIR="${REPO_ROOT}/lisp"
DAEMON="carriage-web-ci-$$"

cleanup() {
  set +e
  "${EMACSCLIENT}" -s "${DAEMON}" -a "" --eval "(kill-emacs)" >/dev/null 2>&1 || true
}
trap cleanup EXIT

echo "[smoke-web] starting Emacs daemon: ${DAEMON}"
"${EMACS}" --quick --daemon="${DAEMON}" >/dev/null 2>&1

# Prepare and start server in the daemon; return the bound port
read_port() {
  local eval_form
  eval_form=$(cat <<'ELISP'
(progn
  (add-to-list 'load-path "%LISP_DIR%")
  (require 'carriage-web)
  (setq carriage-web-enabled t
        carriage-web-bind "127.0.0.1"
        carriage-web-port 0
        carriage-web-close-delay 0.0)
  (let ((tok (getenv "CARRIAGE_WEB_TOKEN")))
    (when (and tok (> (length tok) 0))
      (setq carriage-web-auth-token tok)))
  (carriage-web-start)
  carriage-web-port)
ELISP
)
  eval_form="${eval_form//%LISP_DIR%/${LISP_DIR}}"
  "${EMACSCLIENT}" -s "${DAEMON}" -a "" --eval "${eval_form}"
}

PORT="$(read_port | tr -d '"')"
if [[ -z "${PORT}" ]]; then
  echo "[smoke-web] ERROR: failed to obtain server port" >&2
  exit 1
fi

echo "[smoke-web] server started on http://127.0.0.1:${PORT}"

curl_common=(--max-time 5 -sv)
base_url="http://127.0.0.1:${PORT}"
auth_header=()
if [[ -n "${CARRIAGE_WEB_TOKEN:-}" ]]; then
  auth_header=(-H "X-Auth: ${CARRIAGE_WEB_TOKEN}")
fi

tmp_root_hdrs="$(mktemp -t cw_root_hdrs.XXXXXX)"
tmp_root_body="$(mktemp -t cw_root_body.XXXXXX)"
tmp_health_hdrs="$(mktemp -t cw_health_hdrs.XXXXXX)"
tmp_health_body="$(mktemp -t cw_health_body.XXXXXX)"
cleanup_tmp() { rm -f "${tmp_root_hdrs}" "${tmp_root_body}" "${tmp_health_hdrs}" "${tmp_health_body}"; }
trap 'cleanup_tmp; cleanup' EXIT

echo ">>> GET /"
if ! curl "${curl_common[@]}" "${base_url}/" -o "${tmp_root_body}" -D "${tmp_root_hdrs}" "${auth_header[@]}"; then
  echo "[smoke-web] ERROR: curl to / failed" >&2
  exit 1
fi
if ! grep -qE '^HTTP/1\.[01] 200' "${tmp_root_hdrs}"; then
  echo "[smoke-web] ERROR: / status is not 200" >&2
  sed -n '1,40p' "${tmp_root_hdrs}" >&2
  exit 1
fi
if ! grep -qi '^Content-Type:.*text/html' "${tmp_root_hdrs}"; then
  echo "[smoke-web] ERROR: / Content-Type is not text/html" >&2
  sed -n '1,40p' "${tmp_root_hdrs}" >&2
  exit 1
fi

# Optionally validate Content-Length equals body length (bytes)
if CL_HDR=$(grep -i '^Content-Length:' "${tmp_root_hdrs}" | awk '{print $2}' | tr -d '\r'); then
  if [[ -n "${CL_HDR}" ]]; then
    body_len=$(wc -c < "${tmp_root_body}" | tr -d '[:space:]')
    if [[ "${CL_HDR}" != "${body_len}" ]]; then
      echo "[smoke-web] ERROR: / Content-Length=${CL_HDR} != body-bytes=${body_len}" >&2
      exit 1
    fi
  fi
fi

echo ">>> GET /api/health"
if ! curl "${curl_common[@]}" "${base_url}/api/health" -o "${tmp_health_body}" -D "${tmp_health_hdrs}" "${auth_header[@]}"; then
  echo "[smoke-web] ERROR: curl to /api/health failed" >&2
  exit 1
fi
if ! grep -qE '^HTTP/1\.[01] 200' "${tmp_health_hdrs}"; then
  echo "[smoke-web] ERROR: /api/health status is not 200" >&2
  sed -n '1,80p' "${tmp_health_hdrs}" >&2
  exit 1
fi
if ! grep -qi '^Content-Type:.*application/json' "${tmp_health_hdrs}"; then
  echo "[smoke-web] ERROR: /api/health Content-Type is not application/json" >&2
  sed -n '1,80p' "${tmp_health_hdrs}" >&2
  exit 1
fi
if ! grep -q '"ok"[[:space:]]*:[[:space:]]*true' "${tmp_health_body}"; then
  echo "[smoke-web] ERROR: /api/health body missing ok:true" >&2
  sed -n '1,80p' "${tmp_health_body}" >&2
  exit 1
fi

# Optional SSE checks
if [[ -n "${CARRIAGE_WEB_TOKEN:-}" ]]; then
  echo ">>> GET /stream (unauthorized, no header)"
  if curl "${curl_common[@]}" "${base_url}/stream" -o /dev/null -D - >/tmp/cw_sse_unauth_hdrs 2>/dev/null; then
    :
  fi
  if ! grep -qE '^HTTP/1\.[01] 401' /tmp/cw_sse_unauth_hdrs; then
    echo "[smoke-web] ERROR: /stream without token should be 401" >&2
    sed -n '1,80p' /tmp/cw_sse_unauth_hdrs >&2
    exit 1
  fi

  echo ">>> GET /stream (authorized)"
  if ! curl "${curl_common[@]}" "${base_url}/stream" -o /dev/null -D - "${auth_header[@]}" >/tmp/cw_sse_auth_hdrs 2>/dev/null; then
    echo "[smoke-web] ERROR: curl to /stream (auth) failed" >&2
    exit 1
  fi
  if ! grep -qE '^HTTP/1\.[01] 200' /tmp/cw_sse_auth_hdrs; then
    echo "[smoke-web] ERROR: /stream status is not 200 (auth)" >&2
    sed -n '1,80p' /tmp/cw_sse_auth_hdrs >&2
    exit 1
  fi
  if ! grep -qi '^Content-Type:.*text/event-stream' /tmp/cw_sse_auth_hdrs; then
    echo "[smoke-web] ERROR: /stream Content-Type is not text/event-stream (auth)" >&2
    sed -n '1,80p' /tmp/cw_sse_auth_hdrs >&2
    exit 1
  fi
else
  echo ">>> GET /stream (no token required)"
  if ! curl "${curl_common[@]}" "${base_url}/stream" -o /dev/null -D - >/tmp/cw_sse_hdrs 2>/dev/null; then
    echo "[smoke-web] ERROR: curl to /stream failed" >&2
    exit 1
  fi
  if ! grep -qE '^HTTP/1\.[01] 200' /tmp/cw_sse_hdrs; then
    echo "[smoke-web] ERROR: /stream status is not 200" >&2
    sed -n '1,80p' /tmp/cw_sse_hdrs >&2
    exit 1
  fi
  if ! grep -qi '^Content-Type:.*text/event-stream' /tmp/cw_sse_hdrs; then
    echo "[smoke-web] ERROR: /stream Content-Type is not text/event-stream" >&2
    sed -n '1,80p' /tmp/cw_sse_hdrs >&2
    exit 1
  fi
fi

echo ">>> GET /favicon.ico"
if ! curl "${curl_common[@]}" "${base_url}/favicon.ico" -o "${tmp_health_body}" -D "${tmp_health_hdrs}"; then
  echo "[smoke-web] ERROR: curl to /favicon.ico failed" >&2
  exit 1
fi
if ! grep -qE '^HTTP/1\.[01] 204' "${tmp_health_hdrs}"; then
  echo "[smoke-web] ERROR: /favicon.ico status is not 204" >&2
  sed -n '1,80p' "${tmp_health_hdrs}" >&2
  exit 1
fi

echo ">>> GET /api/metrics"
if ! curl "${curl_common[@]}" "${base_url}/api/metrics" -o "${tmp_health_body}" -D "${tmp_health_hdrs}" "${auth_header[@]}"; then
  echo "[smoke-web] ERROR: curl to /api/metrics failed" >&2
  exit 1
fi
if ! grep -qE '^HTTP/1\.[01] 200' "${tmp_health_hdrs}"; then
  echo "[smoke-web] ERROR: /api/metrics status is not 200" >&2
  sed -n '1,80p' "${tmp_health_hdrs}" >&2
  exit 1
fi
if ! grep -qi '^Content-Type:.*application/json' "${tmp_health_hdrs}"; then
  echo "[smoke-web] ERROR: /api/metrics Content-Type is not application/json" >&2
  sed -n '1,80p' "${tmp_health_hdrs}" >&2
  exit 1
fi
if ! grep -q '"ok"[[:space:]]*:[[:space:]]*true' "${tmp_health_body}"; then
  echo "[smoke-web] ERROR: /api/metrics body missing ok:true" >&2
  sed -n '1,120p' "${tmp_health_body}" >&2
  exit 1
fi

# Optional SSE checks
if [[ -n "${CARRIAGE_WEB_TOKEN:-}" ]]; then
  echo ">>> GET /stream (unauthorized, no header)"
  if curl "${curl_common[@]}" "${base_url}/stream" -o /dev/null -D - >/tmp/cw_sse_unauth_hdrs 2>/dev/null; then
    :
  fi
  if ! grep -qE '^HTTP/1\.[01] 401' /tmp/cw_sse_unauth_hdrs; then
    echo "[smoke-web] ERROR: /stream without token should be 401" >&2
    sed -n '1,80p' /tmp/cw_sse_unauth_hdrs >&2
    exit 1
  fi

  echo ">>> GET /stream (authorized)"
  if ! curl "${curl_common[@]}" "${base_url}/stream" -o /dev/null -D - "${auth_header[@]}" >/tmp/cw_sse_auth_hdrs 2>/dev/null; then
    echo "[smoke-web] ERROR: curl to /stream (auth) failed" >&2
    exit 1
  fi
  if ! grep -qE '^HTTP/1\.[01] 200' /tmp/cw_sse_auth_hdrs; then
    echo "[smoke-web] ERROR: /stream status is not 200 (auth)" >&2
    sed -n '1,80p' /tmp/cw_sse_auth_hdrs >&2
    exit 1
  fi
  if ! grep -qi '^Content-Type:.*text/event-stream' /tmp/cw_sse_auth_hdrs; then
    echo "[smoke-web] ERROR: /stream Content-Type is not text/event-stream (auth)" >&2
    sed -n '1,80p' /tmp/cw_sse_auth_hdrs >&2
    exit 1
  fi
else
  echo ">>> GET /stream (no token required)"
  if ! curl "${curl_common[@]}" "${base_url}/stream" -o /dev/null -D - >/tmp/cw_sse_hdrs 2>/dev/null; then
    echo "[smoke-web] ERROR: curl to /stream failed" >&2
    exit 1
  fi
  if ! grep -qE '^HTTP/1\.[01] 200' /tmp/cw_sse_hdrs; then
    echo "[smoke-web] ERROR: /stream status is not 200" >&2
    sed -n '1,80p' /tmp/cw_sse_hdrs >&2
    exit 1
  fi
  if ! grep -qi '^Content-Type:.*text/event-stream' /tmp/cw_sse_hdrs; then
    echo "[smoke-web] ERROR: /stream Content-Type is not text/event-stream" >&2
    sed -n '1,80p' /tmp/cw_sse_hdrs >&2
    exit 1
  fi
fi

echo ">>> GET /favicon.ico"
if ! curl "${curl_common[@]}" "${base_url}/favicon.ico" -o "${tmp_health_body}" -D "${tmp_health_hdrs}"; then
  echo "[smoke-web] ERROR: curl to /favicon.ico failed" >&2
  exit 1
fi
if ! grep -qE '^HTTP/1\.[01] 204' "${tmp_health_hdrs}"; then
  echo "[smoke-web] ERROR: /favicon.ico status is not 204" >&2
  sed -n '1,80p' "${tmp_health_hdrs}" >&2
  exit 1
fi

echo ">>> GET /api/metrics"
if ! curl "${curl_common[@]}" "${base_url}/api/metrics" -o "${tmp_health_body}" -D "${tmp_health_hdrs}" "${auth_header[@]}"; then
  echo "[smoke-web] ERROR: curl to /api/metrics failed" >&2
  exit 1
fi
if ! grep -qE '^HTTP/1\.[01] 200' "${tmp_health_hdrs}"; then
  echo "[smoke-web] ERROR: /api/metrics status is not 200" >&2
  sed -n '1,80p' "${tmp_health_hdrs}" >&2
  exit 1
fi
if ! grep -qi '^Content-Type:.*application/json' "${tmp_health_hdrs}"; then
  echo "[smoke-web] ERROR: /api/metrics Content-Type is not application/json" >&2
  sed -n '1,80p' "${tmp_health_hdrs}" >&2
  exit 1
fi
if ! grep -q '"ok"[[:space:]]*:[[:space:]]*true' "${tmp_health_body}"; then
  echo "[smoke-web] ERROR: /api/metrics body missing ok:true" >&2
  sed -n '1,120p' "${tmp_health_body}" >&2
  exit 1
fi

echo "[smoke-web] OK"
