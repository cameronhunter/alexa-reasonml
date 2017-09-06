include Request;
include Result;
include ASK;

let skill request _session => switch request {
    | Launch => <Say speech=(PlainText "Welcome to the Hello World skill!") />
    | Intent "HelloWorld" slots => HelloWorld.handler name::(Js.Undefined.to_opt slots##name)
    | _ => Error "No route found";
};

let handler event _context callback => AWS.lambda skill event callback;
