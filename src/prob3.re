let ex1 = ["R8", "U5", "L5", "D3"];
let ex2 = ["U7", "R6", "D4", "L4"];

type instruction =
  | Right(int)
  | Left(int)
  | Up(int)
  | Down(int);

let parseInstruction = (str: string) =>
  switch (
    str.[0],
    String.sub(str, 1, String.length(str) - 1)->int_of_string,
  ) {
  | ('R', rest) => Right(rest)
  | ('L', rest) => Left(rest)
  | ('U', rest) => Up(rest)
  | ('D', rest) => Down(rest)
  | _ => Up(0)
  };

type point = {
  x: int,
  y: int,
  stepCount: int
};

let doTheThing = (xOperation, yOperation) => {
  let rec fn = ({x, y, stepCount}: point, n: int) => {
    let point = {x: xOperation(x), y: yOperation(y), stepCount: succ(stepCount)};
    switch (n) {
    | n when n <= 1 => [point]
    | _ => List.append(fn(point, n - 1), [point])
    };
  };
  fn;
};

let identity = v => v;

let pointsFromInstruction = (p: point, instr: instruction) =>
  switch (instr) {
  | Right(n) => doTheThing(succ, identity, p, n)
  | Left(n) => doTheThing(pred, identity, p, n)
  | Up(n) => doTheThing(identity, succ, p, n)
  | Down(n) => doTheThing(identity, pred, p, n)
  };

let manhattanDistance = ({x, y}) => {
   abs(x) + abs(y);
};


module PointHash= {
  type t = point;
  let equal = (i, j) => i.x === j.x && i.y === j.y
  let hash = (i) => i.x * 10000 + i.y;
};
module PointHashTable = Hashtbl.Make(PointHash);

let findDistance = (l1, l2) => {
  let origin = {x: 0, y: 0, stepCount: 0};
  let toInstructions = List.map(parseInstruction);
  let getAllPoints =
    List.fold_left(
      (points, instruction) => {
        let newPoints = pointsFromInstruction(List.hd(points), instruction);
        List.append(newPoints, points);
      },
      [origin],
    );

  let instructions1 = toInstructions(l1)->getAllPoints;
  let instructions2 = toInstructions(l2)->getAllPoints;

  let tbl = PointHashTable.create(List.length(instructions1));
  let addToTable = PointHashTable.add(tbl);
  let tblContains = v =>
    switch (PointHashTable.find_opt(tbl, v)) {
    | Some(_) => true
    | None => false
    };
  List.iter((a) => addToTable(a, a), instructions1);

  let intersectingPoints =
    List.filter(
      element => element != origin && tblContains(element),
      instructions2,
    );
  let maxOfList = List.fold_left(min, max_int);


let stepDistance = (p) => {PointHashTable.find(tbl, p).stepCount + p.stepCount;}
  let distances = List.map(stepDistance, intersectingPoints);
  maxOfList(distances);
};

let things = Node.Fs.readFileSync("./src/stuff.txt", `ascii);
let garbage = Js.String.split("\n", things);
let [|stuff1, stuff2|] = Array.map(Js.String.split(","), garbage);
findDistance(Array.to_list(stuff1), Array.to_list(stuff2))->Js.log;