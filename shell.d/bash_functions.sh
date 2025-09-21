  # create and change to a directory
  mkcd() {
    mkdir -p -- "$1" && cd -P -- "$1" || exit;
  }

  # the option to exec adds a "-", which causes the new
  # process image to be a login shell
  restart-shell() {
    exec -l $SHELL
  }

  apt-no-proxy() {
    sudo apt-get -o Acquire::http::proxy=false "$1"
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
