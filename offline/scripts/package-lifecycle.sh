#!/usr/bin/env bash
# Trusted package-directory cleanup. Never source or execute files from the
# previous generated package; only a verified process executable may be sent a
# signal before its old directory is removed.

package_process_exists() {
    local pid="$1"
    case "$(detect_os)" in
        linux) [ -d "/proc/${pid}" ] ;;
        macos) /bin/ps -p "$pid" -o pid= 2>/dev/null | grep -q '[0-9]' ;;
        *) return 1 ;;
    esac
}

package_privileged() {
    [ -x /usr/bin/sudo ] \
        || die "A prior package process requires privilege, but /usr/bin/sudo is unavailable"
    /usr/bin/sudo -- "$@"
}

package_process_executable() {
    local pid="$1" privileged="${2:-false}" executable lsof_binary
    case "$(detect_os)" in
        linux)
            if [ "$privileged" = true ]; then
                executable="$(package_privileged /usr/bin/readlink "/proc/${pid}/exe" 2>/dev/null || true)"
            else
                executable="$(/usr/bin/readlink "/proc/${pid}/exe" 2>/dev/null || true)"
            fi
            ;;
        macos)
            if [ -x /usr/sbin/lsof ]; then
                lsof_binary="/usr/sbin/lsof"
            elif [ -x /usr/bin/lsof ]; then
                lsof_binary="/usr/bin/lsof"
            else
                return 1
            fi
            if [ "$privileged" = true ]; then
                executable="$(
                    package_privileged "$lsof_binary" -a -p "$pid" -d txt -Fn 2>/dev/null \
                        | sed -n 's/^n//p' \
                        | head -1
                )"
            else
                executable="$(
                    "$lsof_binary" -a -p "$pid" -d txt -Fn 2>/dev/null \
                        | sed -n 's/^n//p' \
                        | head -1
                )"
            fi
            ;;
        *) return 1 ;;
    esac
    [ -n "$executable" ] || return 1
    printf '%s\n' "$executable"
}

package_process_alive() {
    local pid="$1" privileged="$2"
    if [ "$privileged" = true ]; then
        package_process_exists "$pid"
    else
        kill -0 "$pid" 2>/dev/null
    fi
}

package_signal_process() {
    local signal="$1" pid="$2" privileged="$3"
    if [ "$privileged" = true ]; then
        package_privileged /bin/kill "-${signal}" "$pid"
    else
        kill "-${signal}" "$pid"
    fi
}

stop_verified_package_process() {
    local label="$1" pid_file="$2" expected_executable="$3"
    local pid executable attempts=0 privileged=false
    [ -f "$pid_file" ] && [ ! -L "$pid_file" ] || return 0
    pid=""
    if ! IFS= read -r pid < "$pid_file"; then
        [ -n "$pid" ] || return 0
    fi
    pid="${pid%$'\r'}"
    case "$pid" in
        ''|*[!0-9]*) warn "Ignoring unsafe ${label} PID file: $pid_file"; return 0 ;;
    esac
    if ! kill -0 "$pid" 2>/dev/null; then
        package_process_exists "$pid" || return 0
        privileged=true
        warn "Prior package ${label} PID ${pid} requires privileged inspection"
    fi
    executable="$(package_process_executable "$pid" "$privileged" 2>/dev/null || true)"
    [ -n "$executable" ] \
        || die "Could not inspect live prior package ${label} process ${pid}"
    if [ "$executable" != "$expected_executable" ]; then
        warn "Refusing to signal ${label} PID ${pid}: executable is not owned by the prior package"
        return 0
    fi
    info "Stopping prior package ${label} (PID ${pid})"
    if ! package_signal_process TERM "$pid" "$privileged" 2>/dev/null \
        && package_process_alive "$pid" "$privileged"; then
        die "Could not signal prior package ${label} process ${pid}"
    fi
    while package_process_alive "$pid" "$privileged" && [ "$attempts" -lt 25 ]; do
        sleep 0.2
        attempts=$((attempts + 1))
    done
    if package_process_alive "$pid" "$privileged"; then
        executable="$(package_process_executable "$pid" "$privileged" 2>/dev/null || true)"
        [ -n "$executable" ] || return 0
        if [ "$executable" != "$expected_executable" ]; then
            warn "Prior package ${label} PID ${pid} changed ownership before SIGKILL"
            return 0
        fi
        warn "Prior package ${label} did not stop; sending SIGKILL"
        if ! package_signal_process KILL "$pid" "$privileged" 2>/dev/null \
            && package_process_alive "$pid" "$privileged"; then
            die "Could not stop prior package ${label} process ${pid}"
        fi
        attempts=0
        while package_process_alive "$pid" "$privileged" && [ "$attempts" -lt 25 ]; do
            sleep 0.2
            attempts=$((attempts + 1))
        done
        if package_process_alive "$pid" "$privileged"; then
            executable="$(package_process_executable "$pid" "$privileged" 2>/dev/null || true)"
            [ -n "$executable" ] || return 0
            die "Prior package ${label} process ${pid} survived SIGKILL"
        fi
    fi
    return 0
}

package_runtime_pgdata_for_home() {
    local home="$1"
    [ -n "$home" ] || die "HOME is unavailable; cannot locate the prior package PostgreSQL data directory"
    case "$(detect_os)" in
        macos) printf '%s\n' "${home}/Library/Application Support/ArkhamHorror/pgdata" ;;
        linux) printf '%s\n' "${home}/.local/share/ArkhamHorror/pgdata" ;;
    esac
}

package_is_wsl() {
    [ "$(detect_os)" = linux ] && grep -qi microsoft /proc/version 2>/dev/null
}

package_arkham_home() {
    printf '%s\n' "/home/arkham"
}

stop_previous_package_services() {
    local game_dir="${1}/game" runtime_pgdata arkham_pgdata
    [ -d "$game_dir" ] && [ ! -L "$game_dir" ] || return 0
    stop_verified_package_process "nginx" \
        "${game_dir}/data/nginx.pid" "${game_dir}/bin/nginx"
    stop_verified_package_process "arkham-api" \
        "${game_dir}/data/arkham-api.pid" "${game_dir}/bin/arkham-api"
    runtime_pgdata="$(package_runtime_pgdata_for_home "${HOME:-}")"
    stop_verified_package_process "postgres" \
        "${runtime_pgdata}/postmaster.pid" "${game_dir}/pgsql/bin/postgres"
    if package_is_wsl; then
        arkham_pgdata="$(package_runtime_pgdata_for_home "$(package_arkham_home)")"
        if [ "$arkham_pgdata" != "$runtime_pgdata" ]; then
            stop_verified_package_process "postgres" \
                "${arkham_pgdata}/postmaster.pid" "${game_dir}/pgsql/bin/postgres"
        fi
    fi
    # Preserve cleanup compatibility with older packages whose live pgdata was
    # still inside game/data.
    stop_verified_package_process "postgres" \
        "${game_dir}/data/pgdata/postmaster.pid" "${game_dir}/pgsql/bin/postgres"
}
