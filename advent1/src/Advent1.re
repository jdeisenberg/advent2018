
let calcSum = (runningTotal: int, oneLine: string): int => {
  let result = if (oneLine != "") {
    if (Js.String.charAt(0, oneLine) == "+") {
      runningTotal + int_of_string(Js.String.sliceToEnd(~from=1, oneLine));
    } else {
      runningTotal + int_of_string(oneLine);
    }
  } else {
    runningTotal;
  }
  Js.log("Running total after adding " ++ oneLine ++ " is " ++ string_of_int(runningTotal))
  result
};

let result = Node.Fs.readFileAsUtf8Sync("frequencies.txt")
  -> Js.String.split("\n", _)
  -> Belt.Array.reduce(0, calcSum)
  
Js.log2("Total: ", result);
