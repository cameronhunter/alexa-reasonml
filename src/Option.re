let map fn o => switch o {
    | Some value => Some (fn value)
    | None => None
};

let flatten list => {
    let fn a b => switch b {
        | Some value => List.append a [value]
        | None => a
    };

    List.fold_left fn [] list;
};

let flatMap fn o => switch o {
    | Some value => fn value
    | None => None
};

let getOrElse default o => switch o {
    | Some value => value
    | None => default
};
