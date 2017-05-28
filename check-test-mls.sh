for i in test/*.ml; do
    if cargo run --release -q -- $i >/dev/null; then
	echo "[OK] $i"
    else
	echo "[Error] $i"
    fi
done
