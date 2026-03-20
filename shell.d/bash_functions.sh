sbuild-purge() {
    if [ ! -d "debian" ]; then
        echo "This script should only be executed within a package's source directory, with the build artifacts (e.g., *.ppa.upload, *.changes, etc.) in the parent directory."
        exit 1
    fi

    quilt pop -a 2>/dev/null || echo "No patches applied"
    schroot -e --all-sessions
    rm -rf /var/lib/sbuild/build/*
    rm -vf ../*.{debian.tar.xz,dsc,buildinfo,changes,ppa.upload,build,deb,ddeb}
    rm -vf debian/files
    rm -rf .pc
}

# create and change to a directory
mkcd() {
        mkdir -p -- "$1" && cd -P -- "$1" || exit
}

# the option to exec adds a "-", which causes the new
# process image to be a login shell
restart-shell() {
        exec -l $SHELL
}

mplaunch() {
    local args=("$@")
    local name=""

    # Extract --name/-n value if provided
    for i in "${!args[@]}"; do
        if [[ "${args[$i]}" == "--name" || "${args[$i]}" == "-n" ]]; then
            name="${args[$((i+1))]}"
        elif [[ "${args[$i]}" == --name=* ]]; then
            name="${args[$i]#--name=}"
        fi
    done

    echo "Launching multipass instance..."
    local launch_output
    launch_output=$(multipass launch "${args[@]}" 2>&1)
    local rc=$?
    echo "$launch_output"
    [[ $rc -ne 0 ]] && return 1

    # If no --name/-n was given, parse it from launch output ("Launched: <name>")
    if [[ -z "$name" ]]; then
        name=$(echo "$launch_output" | grep -oP '(?<=Launched: )\S+')
    fi

    if [[ -z "$name" ]]; then
        echo "Warning: could not determine instance name, skipping terminfo install." >&2
        return 0
    fi

    echo "Installing Ghostty terminfo on '$name'..."
    infocmp -x | multipass exec "$name" -- bash -c 'tic -x /dev/stdin' || {
        echo "Warning: failed to install Ghostty terminfo on '$name'." >&2
    }

    echo "Done. Connect with: multipass shell $name"
}

lxdlaunch() {
    local args=("$@")
    local name=""

    # In lxc launch, the instance name is the second positional arg: lxc launch <image> <name>
    local positional=()
    local skip_next=false
    for arg in "${args[@]}"; do
        if $skip_next; then
            skip_next=false
            continue
        fi
        case "$arg" in
            -p|--profile|-c|--config|-n|--network|-s|--storage|-t|--target)
                skip_next=true ;;
            -*) ;;
            *) positional+=("$arg") ;;
        esac
    done
    # positional[0] = image, positional[1] = name (if provided)
    if [[ ${#positional[@]} -ge 2 ]]; then
        name="${positional[1]}"
    fi

    echo "Launching LXD instance..."
    lxc launch "${args[@]}" || return 1

    if [[ -z "$name" ]]; then
        echo "Warning: could not determine instance name, skipping terminfo install." >&2
        return 0
    fi

    # Wait for instance to be ready enough to exec
    echo "Waiting for instance '$name' to be ready..."
    local retries=10
    until lxc exec "$name" -- true 2>/dev/null || [[ $retries -eq 0 ]]; do
        sleep 1
        ((retries--))
    done

    if [[ $retries -eq 0 ]]; then
        echo "Warning: timed out waiting for '$name', skipping terminfo install." >&2
        return 0
    fi

    echo "Installing Ghostty terminfo on '$name'..."
    infocmp -x | lxc exec "$name" -- bash -c 'tic -x /dev/stdin' || {
        echo "Warning: failed to install Ghostty terminfo on '$name'." >&2
    }

    echo "Done. Connect with: lxc shell $name"
}

test-snap() {
        SNAP_NAME=$(grep "name:" snapcraft.yaml | cut -d ' ' -f2)
        CONTAINER_NAME="snap-test-$(date +%s)"
        SNAP_FILE="${SNAP_NAME}_*.snap"

        snapcraft pack

        BUILT_SNAP_FILE=$(ls -1 "${SNAP_NAME}"_*.snap | head -n 1)

        if [ ! -f "$BUILT_SNAP_FILE" ]; then
                echo "Error: Snap package not found."
                exit 1
        fi

        lxc lts "${CONTAINER_NAME}"

        echo "Waiting for the container to come online..."
        until lxc exec "${CONTAINER_NAME}" -- /bin/true &>/dev/null; do
                echo -n "."
                sleep 1
        done
        sleep 5

        echo "Copying the snap to the container..."
        lxc file push "${BUILT_SNAP_FILE}" "${CONTAINER_NAME}/home/ubuntu/"

        lxc exec "${CONTAINER_NAME}" -- sudo --user ubuntu --login -- /bin/bash -c "sudo snap install /home/ubuntu/${BUILT_SNAP_FILE} --dangerous ${1}"

        lxc ubuntu "${CONTAINER_NAME}"

        echo "Cleaning up: Deleting the container..."
        lxc delete "${CONTAINER_NAME}" --force
}
