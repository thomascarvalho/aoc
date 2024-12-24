
clj -J-Xmx16g -J-Xms16g \
    -J-XX:+UseG1GC \
    -J-XX:MaxGCPauseMillis=200 \
    -J-XX:+UseCompressedOops \
    -J-XX:G1HeapRegionSize=32m \
    -J-XX:InitialHeapSize=16g \
    -J-XX:MinHeapFreeRatio=10 \
    -J-XX:MaxHeapFreeRatio=20 \
    -M src/aoc/2024/11.clj

