;The version is less efficient because the stream doesn't have a self-reference and as such
;calculates an entire new stream each time. Therefore, the memo-proc while in effect goes wasted.
;If we didn't have memo-proc, of course, then the efficiency would be the same... and terrible! Hah!
