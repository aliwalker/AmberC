RDC="rdmd"
TARGET="$1"
SOURCE_DIR="source"
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
        $RDC --main -unittest -I=$SOURCE_DIR $f
    done
else
    report "source/$TARGET"
    $RDC --main -unittest -I=$SOURCE_DIR "source/$TARGET"
fi

echo "Tests finished."