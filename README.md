# Test Code
`test_trig_hist.ml`
# Test Result
`dist_atan.csv`
# Test
```
ocamlopt -I +unix unix.cmxa -O3 -o test_trig_hist test_trig_hist.ml
~/fpu$ ./test_trig_hist
```
# Test
```
ocamlopt -O3 -I +unix unix.cmxa check.ml -o check
./check *** -domain-count 8
(Change from *** to atan, sin, or cos)
```
