type result 'success 'error =
  | Ok 'success
  | Error 'error;

let map fn result => switch result {
    | Ok value => Ok (fn value)
    | Error e => Error e
};

let flatMap fn result => switch result {
    | Ok value => fn value
    | Error error => Error error
};

let all a b => switch (a, b) {
    | (Ok a, Ok b) => Ok (a, b)
    | (Error e, _) | (_, Error e) => Error e
};
