# Toe wrestling

### What the hell is this?
This piece of code is able to determine whether it's possible (i.e. not impossible) for a player to win a tournament consisting of group-phase matches (no elimination, no draw).

This implementation is based on the Ford-Fulkerson algorithm (returns the maximum flow in a flow network) and is applied to a specific sport: toe wrestling.

Basically, it consists of distributing the remaining matches among every player's score, under the constraint of not exceeding the maximum reachable score of the targeted player.

---

### Requirements
+ OCaml compiler  
+ Linux: a very crucial feature relies on it (the *random bullshit engine*)
+ [`graphviz`](http://www.graphviz.org/) (optional): utility for rendering graphs
  ```
  dot -Tpng residual.gviz > residual.png
  ```

### How to
+ Build with `make`
+ Run
  ```
  ./toe_wrestling Cambacérès possible.graph residual.gviz
  ```
  See examples (`*.graph` files) to get an idea of the input file format.