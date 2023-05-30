set -e

OUT="main.CV"
dasm main.asm -lmain.lst -smain.sym -f3 -v5 -o$OUT

printf "ROM size: %x\n" `stat -c "%s" $OUT`