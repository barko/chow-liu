Based on the [ancient
work](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.133.9772&rep=rep1&type=pdf)
of Chow and Liu, Chow-Liu is an OCaml package to train and evaluate
probabilistic graphical models over training sets of fixed-length
binary observations.

To install, clone this repository, then build:

```
$ dune build
```

Inspect the training data:
```
$ head -n 4 data/train.csv
0,0,0,0
0,0,0,0
0,0,0,1
0,0,0,1
```

Build a model:
```
$ _build/default/src/cl.exe train -i data/train.csv -o /tmp/model
```

Inspect the test data:
```
$ head -n 4 data/test.csv
0,0,0,0
0,0,0,1
0,0,1,0
0,0,1,1
```

Evalute the model over the test data set:
```
$ _build/default/src/cl.exe complete -m /tmp/model -i data/test.csv -v
1.037e-01 -2.266
1.296e-01 -2.043
3.636e-02 -3.314
3.030e-02 -3.497
1.212e-02 -4.413
1.515e-02 -4.190
6.694e-02 -2.704
5.579e-02 -2.886
5.185e-02 -2.959
6.481e-02 -2.736
1.818e-02 -4.007
1.515e-02 -4.190
3.232e-02 -3.432
4.040e-02 -3.209
1.785e-01 -1.723
1.488e-01 -1.905
mean LL = -3.092156
```

# Dependencies:

* [cosovo](https://opam.ocaml.org/packages/cosovo/), to read csv files
* [atdgen](https://opam.ocaml.org/packages/atdgen/), to encode/decode model files
* [pringo](https://opam.ocaml.org/packages/pringo/), to randomly sample the generated distributions
* [cmdliner](https://opam.ocaml.org/packages/cmdliner/), to parse the command line
* [ocamlgraph](https://opam.ocaml.org/packages/ocamlgraph/), to build a spanning tree
* [bitv](https://opam.ocaml.org/packages/bitv/), to compactly represents binary matrices

# License

BSD
