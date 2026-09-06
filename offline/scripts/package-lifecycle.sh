#!/usr/bin/env bash
# Trusted package-directory cleanup. Never source or execute files from the
# previous generated package; only a verified process executable may be sent a
# signal before its old directory is removed.

package_process_executable() {
    local pid="$1" executable
    case "$(detect_os)" in
        linux)
            executable="$(readlink "/proc/${pid}/exe" 2>/dev/null || true)"
            ;;
        macos)
            executable="$(
                lsof -a -p "$pid" -d txt -Fn 2>/dev/null \
                    | sed -n 's/^n//p' \
                    | head -1
            )"
            ;;
        *) return 1 ;;
    esac
    [ -n "$executable" ] || return 1
    printf '%s\n' "$executable"
}

stop_verified_package_process() {
    local label="$1" pid_file="$2" expected_executable="$3"
    local pid executable attempts=0
    [ -f "$pid_file" ] && [ ! -L "$pid_file" ] || return 0
    pid="$(tr -d '[:space:]' < "$pid_file" 2>/dev/null || true)"
    case "$pid" in
        ''|*[!0-9]*) warn "Ignoring unsafe ${label} PID file: $pid_file"; return 0 ;;
    esac
    kill -0 "$pid" 2>/dev/null || return 0
    executable="$(package_process_executable "$pid" 2>/dev/null || true)"
    if [ "$executable" != "$expected_executable" ]; then
        warn "Refusing to signal ${label} PID ${pid}: executable is not owned by the prior package"
        return 0
    fi
    info "Stopping prior package ${label} (PID ${pid})"
    kill -TERM "$pid" 2>/dev/null || true
    while kill -0 "$pid" 2>/dev/null && [ "$attempts" -lt 25 ]; do
        sleep 0.2
        attempts=$((attempts + 1))
    done
    if kill -0 "$pid" 2>/dev/null; then
        warn "Prior package ${label} did not stop; sending SIGKILL"
        kill -KILL "$pid" 2>/dev/null || true
    fi
}

stop_previous_package_services() {
    local game_dir="${1}/game"
    [ -d "$game_dir" ] && [ ! -L "$game_dir" ] || return 0
    stop_verified_package_process "nginx" \
        "${game_dir}/data/nginx.pid" "${game_dir}/bin/nginx"
    stop_verified_package_process "arkham-api" \
        "${game_dir}/data/arkham-api.pid" "${game_dir}/bin/arkham-api"
    # Legacy package-local pgdata has a trusted PID location. Current packages
    # place live pgdata in the user's data directory and do not expose it here.
    stop_verified_package_process "postgres" \
        "${game_dir}/data/pgdata/postmaster.pid" "${game_dir}/pgsql/bin/postgres"
}
