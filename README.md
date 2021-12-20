```sh
rlwrap ocaml

# 駅名リスト
ocamlc -o _build/metro -c src/metro.ml

# 21.3
ocamlc -o _build/21_3 -c src/21.3.ml -I _build/
ocamlc -o _build/dijkstra_21_3 _build/metro.cmo _build/21_3.cmo
_build/dijkstra_21_3 ikebukuro myogadani
```

