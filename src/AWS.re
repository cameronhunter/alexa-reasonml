include Result;
include Request;

let to_request event => {
    let request_type = event##request##_type;
    let intent = Js.Undefined.to_opt event##request##intent;
    let slots = intent |> Option.map (fun i => i##slots);

    switch (request_type, intent, slots) {
        | ("LaunchRequest", _intent, _slots) => Ok Launch
        | ("SessionEndedRequest", _intent, _slots) => Ok SessionEnded
        | ("IntentRequest", Some intent, Some slots) => Ok (Intent intent##name slots)
        | ("IntentRequest", None, _slots) => Error "Expected an intent name for an intent request"
        | ("IntentRequest", Some _intent, None) => Error "Expected slots for intent request"
        | _ => Error ("Unknown request type: " ^ request_type)
    }
};

let lambda route event callback => {
    let response = event |> to_request |> Result.flatMap (fun request => route request event##session);

    switch (response) {
        | Ok response => callback Js.Null.empty (Js.Undefined.return response)
        | Error response => callback (Js.Null.return response) Js.Undefined.empty
    }
};
