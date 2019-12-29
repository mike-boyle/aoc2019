let isWithinRange = (minRange, maxRange, n) => n >= minRange && n <= maxRange;
let isSixDigitNumber = isWithinRange(100000, 1000000);
let wordRange = Array.init(6, a => a)->Array.to_list;
let twoAdjacentDigitsAreTheSame = str => {
  let foundGood = ref(false);
  for (i in 1 to 5) {
    foundGood :=
      foundGood^
      || str.[i] === str.[i - 1]
      && (i <= 1 || str.[i - 2] !== str.[i - 1])
      && (i === 5 || str.[i] !== str.[i + 1]);
  };
  foundGood^;
};
let digitsNeverDecrease = str => {
  let highestDigit = ref(0);
  switch (
    List.find_opt(
      a => {
        let charVal = int_of_char(str.[a]);
        let isDecreasing = charVal < highestDigit^;
        highestDigit := charVal;
        isDecreasing;
      },
      wordRange,
    )
  ) {
  | Some(_) => false
  | None => true
  };
};

let program = (minRange, maxRange) => {
  Array.init(maxRange - minRange, a => a + minRange)
  |> Array.to_list
  |> List.filter(a =>
       isSixDigitNumber(a)
       && twoAdjacentDigitsAreTheSame(string_of_int(a))
       && digitsNeverDecrease(string_of_int(a))
     )
  |> List.length;
};

program(264793, 803935)->Js.log;