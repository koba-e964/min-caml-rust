success=0
total=0

ARCH=${ARCH:-x86_64}

NECESSARY_FILES="assets/stub.c assets/${ARCH}/libmincaml.S"

if cargo build --release; then
  for i in test/*.ml; do
    echo "---- ${i} ----"
    if cargo run --release -q -- ${i} ${i}.asm >/dev/null 2>&1; then
      if cc ${i}.asm ${NECESSARY_FILES} -o ${i}.x; then
        if ./${i}.x >${i}.out; then
          # testrun ml
          ocaml ${i} >${i}.ans
          if diff ${i}.out ${i}.ans; then
            echo "[OK] $i"
            success=$((success+1))
          else
            echo "[Output Error] ${i}"
          fi
        fi
      else
  	    echo "[Link Error] $i"
      fi
    else
      echo "[Compile Error] $i"
    fi
    total=$((total+1))
  done
  echo "Success rate: $success/$total"
else
    echo "build error"
    exit 1
fi
