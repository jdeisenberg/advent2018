/*
 * Compare s1 to s2, starting at given position
 * returns a total number of differences and the position at which
 * the two strings are different.  It stops when there are more than
 * two different letters.
*/
let rec nDiffs = (s1: string, s2: string, pos:int, (total, diffAt): (int, int)): (int, int) => {
  if (pos == Js.String.length(s1) || total > 1) {
    (total, diffAt)
  } else {
    let c1 = Js.String.charAt(pos, s1);
    let c2 = Js.String.charAt(pos, s2);
    if (c1 != c2) {
      nDiffs(s1, s2, pos + 1, (total + 1, pos))
    } else {
      nDiffs(s1, s2, pos + 1, (total, diffAt))
    }
  }
};


let rec findOneDiff = (data: array(string), pos1: int, pos2:int): (string, string, int) => {
  let n = Belt.Array.length(data);
  if (pos1 == n && pos2 == n) {
    ("", "", 0)
  } else {
    Js.log2(data[pos1], data[pos2]);
    let (diffCount, diffAt) = nDiffs(data[pos1], data[pos2], 0, (0,0));
    if (diffCount == 1) {
      (data[pos1], data[pos2], diffAt)
    } else {
      let new2 = pos2 + 1;
      if (new2 == n) {
        findOneDiff(data, pos1 + 1, pos1 + 2)
      } else {
        findOneDiff(data, pos1, new2)
      }
    }
  }
};
    


let data = Node.Fs.readFileAsUtf8Sync("input.txt")
  -> Js.String.split("\n", _)
  -> Belt.Array.keep(_, (x) => x != "")


Js.log(findOneDiff(data, 0, 0));

