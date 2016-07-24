# Token parsing
1 2 3 4 5
"a" "bc" "def"
{a} {bc} {def}
# this is a comment
1 2 3 # comment
1 2 3 if # -> 2
0 2 {1.} if # -> 1 1

# Stack arithmetic
1 2 3 * + # 7
