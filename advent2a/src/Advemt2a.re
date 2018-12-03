module StrComparator =
 Belt.Id.MakeComparable({
 type t = string;
 let cmp = compare;
});

let rec makeMap = (s:string, m: array(int)): array(int) => {
  if (s == "") {
    m
  } else {
    let n = Js.String.codePointAt(0, s);
    switch (n) {
      | Some(code) => {
          Belt.Array.set(m, code - 97, Belt.Array.getUnsafe(m, code - 97) + 1) |> ignore
          makeMap(Js.String.sliceToEnd(~from=1, s),m)
        }
      | None =>
          makeMap(Js.String.sliceToEnd(~from=1, s), m)
    }
  }
};

let data = Node.Fs.readFileAsUtf8Sync("input.txt")
  -> Js.String.split("\n", _)
  -> Belt.Array.keep(_, (x) => x != "")

let (nTwo, nThree) = Belt.Array.reduce(data, (0, 0), 
  ((acc2, acc3), item) => {
    let m = makeMap(item, Belt.Array.make(26, 0));
    (acc2 + ((Belt.Array.some(m, (x) => (x == 2))) ? 1 : 0),
     acc3 + ((Belt.Array.some(m, (x) => (x == 3))) ? 1 : 0))
  });
  
Js.log2(nTwo, nThree);
Js.log(nTwo * nThree);


