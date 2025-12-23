# Test Code
`test_trig_hist.ml`
# Test Result
`dist_atan.csv`
# Test
```
ocamlopt -I +unix unix.cmxa -O3 -o test_trig_hist test_trig_hist.ml
~/fpu$ ./test_trig_hist
```
