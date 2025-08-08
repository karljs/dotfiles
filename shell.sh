case $- in
*i*) ;;
*) return ;;
esac

if [ -d ~/.shell.d ] && [ -x ~/.shell.d ]; then
  for shellfile in ~/.shell.d/*; do
    [ -r "$shellfile" ] && source "$shellfile"
  done
  unset shellfile
fi

# Provide some fine words of wisdom
if command -v cowsay fortune > /dev/null 2>&1; then
  fortune | cowsay
fi
