let input = [
  1,
  0,
  0,
  3,
  1,
  1,
  2,
  3,
  1,
  3,
  4,
  3,
  1,
  5,
  0,
  3,
  2,
  1,
  10,
  19,
  2,
  9,
  19,
  23,
  2,
  23,
  10,
  27,
  1,
  6,
  27,
  31,
  1,
  31,
  6,
  35,
  2,
  35,
  10,
  39,
  1,
  39,
  5,
  43,
  2,
  6,
  43,
  47,
  2,
  47,
  10,
  51,
  1,
  51,
  6,
  55,
  1,
  55,
  6,
  59,
  1,
  9,
  59,
  63,
  1,
  63,
  9,
  67,
  1,
  67,
  6,
  71,
  2,
  71,
  13,
  75,
  1,
  75,
  5,
  79,
  1,
  79,
  9,
  83,
  2,
  6,
  83,
  87,
  1,
  87,
  5,
  91,
  2,
  6,
  91,
  95,
  1,
  95,
  9,
  99,
  2,
  6,
  99,
  103,
  1,
  5,
  103,
  107,
  1,
  6,
  107,
  111,
  1,
  111,
  10,
  115,
  2,
  115,
  13,
  119,
  1,
  119,
  6,
  123,
  1,
  123,
  2,
  127,
  1,
  127,
  5,
  0,
  99,
  2,
  14,
  0,
  0,
];

let example = [|1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50|];

let arrayToString =
  Array.fold_left((acc, v) => acc ++ ", " ++ string_of_int(v), "");

let program = (i: array(int)) => {
  let handleOperation = (op, pc) => {
    let result = op(i[i[pc + 1]], i[i[pc + 2]]);
    let outputPosition = i[pc + 3];
    i[outputPosition] = result;
    pc + 4;
  };
  let handleAddition = handleOperation((+));
  let handleMultiplication = handleOperation(( * ));
  let pc = ref(0);

  while (i[pc^] != 99) {
    let curPc = pc^;
    let pcOption =
      switch (i[curPc]) {
      | 1 => Some(handleAddition(curPc))
      | 2 => Some(handleMultiplication(curPc))
      | _ => None
      };

    pc :=
      (
        switch (pcOption) {
        | Some(pc) => pc
        | None => 99
        }
      );

    ();
  };
  i[0];
};

let runProgram = (noun, verb) => {
  let memory = Array.of_list(input);
  memory[1] = noun;
  memory[2] = verb;
  program(memory);
};

let findMatchingNounAndVerb = () => {
  let rec tryValue = (noun, verb) =>
    switch (noun, verb, runProgram(noun, verb)) {
    | (_, _, 19690720) => Some(100 * noun + verb)
    | (100, 100, _) => None
    | (n, 100, _) => tryValue(n + 1, 0)
    | (n, v, _) => tryValue(n, v + 1)
    };
  switch (tryValue(0, 0)) {
  | Some(value) => string_of_int(value)
  | None => "Not found!"
  };
};

runProgram(12, 2)->Js.log;
findMatchingNounAndVerb()->Js.log;