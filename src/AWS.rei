let lambda:
    (Request.request 'slots => ASK.session 'attributes => Result.result ASK.response string) /* Router */
    => ASK.event 'slots 'attributes /* Event */
    => (Js.null string => Js.undefined ASK.response => unit) /* Callback */
    => unit;
