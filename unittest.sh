RDC="rdmd"
TARGET="$1"
PARSER_DIR="source/parser"

echo "Running unittests for source code.."

report() {
    echo "  Testing $1"
}

if [ -z "$TARGET" ]
then
    for f in $PARSER_DIR/*.d
    do
        report $f
        $RDC --main -unittest $f
    done
else
    report $TARGET
    $RDC --main -unittest $TARGET
fi

echo "Tests finished."