module IntComparator =
 Belt.Id.MakeComparable({
 type t = int;
 let cmp = compare;
});


let toInt = (s: string): int => {
  if (s != "") {
    if (Js.String.charAt(0, s) == "+") {
      int_of_string(Js.String.sliceToEnd(~from=1, s));
    } else {
      int_of_string(s);
    }
  } else {
    0;
  };
};

/*
 * Recursively go through a list of data at position p, with a hash
 * of sums and a running total until you get to an item already in the
 * sum array, returning that item.
 */
 
let rec findDuplicate = (data: array(int), position: int, sums, total: int): int => {
  let n = Belt.Array.length(data);
  if (position == 0) {
    Js.log("Repeating list " ++ string_of_int(Belt.Map.size(sums)));
  }
  let newTotal = total + data[position];
  if (Belt.Map.has(sums, newTotal)) {
    newTotal;
  } else {
    findDuplicate(data, (position + 1) mod n, Belt.Map.set(sums, newTotal, 1), newTotal);
  }
}


let data = Node.Fs.readFileAsUtf8Sync("frequencies.txt")
  -> Js.String.split("\n", _)
  -> Belt.Array.map(_, toInt)
  -> Belt.Array.keep(_, (x) => x != 0)
  
let result = findDuplicate(data, 0, Belt.Map.make(~id=(module IntComparator)), 0);

Js.log2("Result", result);
