/^--$START GRAPH/d
/^--$END GRAPH/d
/^--$START DIGRAPH/,/^--$END DIGRAPH/d
1,$s/GRAPHTYPE/Graph/g