success=0
total=0
if cargo build --release; then
    for i in test/*.ml; do
	if cargo run --release -q -- $i >/dev/null; then
	    echo "[OK] $i"
	    success=`expr $success + 1`
	else
	    echo "[Error] $i"
	fi
	total=`expr $total + 1`
    done
    echo "Success rate: $success/$total"
else
    echo "build error"
    exit 1
fi
