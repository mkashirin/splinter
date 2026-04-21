# ⚙️ Splinter

Splinter (Sequence Processing Language Interpreter) is a tree-walking
interpreter for the Sequence Processing language.

## 🚧 Project Status

Splinter is a work-in-progress project. For now, it builds an AST for the
program, which then gets evaluated by the tree-walking interpreter.
+ The next stage would be replacing the tree-walking interperter with a
  register-based virtual machine to gain speed.
+ The last stage is fully dedicated to wrtiting down the specification.

## 👀 Examples

### Dyck-1 PTF

As an example, there is an SPL program in main.spl, which solves the
Dyck-1 PTF problem. Here is the source code for it:
```python
def f(bools) {
    indices = Indices(bools);
    diag_mat = Select(indices, indices, <=);
    plus_one = (Indices + 1)(bools);
    indicated = Indicator(bools);
    agg = plus_one * Aggregate(diag_mat, indicated);
    return agg;
}

INPUT = "(())";

num_open = (Tokens == "(")(INPUT);
num_closed = (Tokens == ")")(INPUT);
opens = f(num_open);
closed = f(num_closed);

balance = opens - closed;
imbalances = [true if e < 0 else false for e in balance];
f_imbalances = f(imbalances);

pt = ["T" if e == 0 else "P" for e in balance];
indices = Indices(pt);
for i in indices {
    ptf = "F" if f_imbalances[i] > 0 else pt[i];
    Print(ptf);
}
```
The output should look like this:
```shell
String(P)
String(P)
String(P)
String(T)
```

### Less obvious features

There's also a possibility to do non-trivial stuff in SPL, like Higher Order
Functions or Comprehensions:
```python
def hof(f, a) {
    return f(a);
}

def pow(a) {
    return a ^ 2;
}

result = hof(pow, 2);
Print(result);

list = [0, 2, 3, 1];
comp = [true if i < 1 else false for i in list];
Print(comp);
```
