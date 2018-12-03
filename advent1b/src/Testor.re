type t = {
  name: string,
  f: (int) => int
};

let example:t = {name: "square", f: (x: 'a): 'a => x};

Js.log(example.f(3));
