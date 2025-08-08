# create and change to a directory
mkcd() {
  mkdir -p -- "$1" && cd -P -- "$1" || exit;
}

# the option to exec adds a "-", which causes the new
# process image to be a login shell
restart-shell() {
  exec -l $SHELL
}
