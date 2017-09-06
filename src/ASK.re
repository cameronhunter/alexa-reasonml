type intent 'slots = Js.t {
    .
    name: string,
    slots: Js.undefined 'slots
};

type session 'attributes = Js.t {
    .
    application: Js.t {
        .
        applicationId: string
    },
    attributes: Js.undefined 'attributes
};

type event 'slots 'attributes = Js.t {
    .
    request: Js.t {
        .
        _type: string,
        intent: Js.undefined (intent 'slots)
    },
    session: session 'attributes
};

type speech = Js.t {
    .
    outputSpeech: Js.t {
        .
        _type: string,
        text: Js.undefined string,
        ssml: Js.undefined string
    }
};

type image = Js.t {
    .
    smallImageUrl: string,
    largeImageUrl: string
};

type card = Js.t {
    .
    _type: string,
    content: Js.undefined string,
    image: Js.undefined image,
    text: Js.undefined string,
    title: Js.undefined string
};

type response = Js.t {
    .
    version: string,
    response: Js.t {
        .
        ask: Js.undefined speech,
        say: Js.undefined speech,
        reprompt: Js.undefined speech,
        card: Js.undefined card,
        shouldEndSession: Js.boolean
    }
};

module Card = {
    type title = string;
    type content = string;
    type url = string;

    type types =
      | Simple title content
      | Standard title content url url
      | LinkAccount;

    let to_js_object card => switch card {
        | Simple title content => {
            "_type": "Simple",
            "content": Js.Undefined.return content,
            "image": Js.Undefined.empty,
            "text": Js.Undefined.empty,
            "title": Js.Undefined.return title
        }
        | Standard title text smallImageUrl largeImageUrl => {
            "_type": "Standard",
            "content": Js.Undefined.empty,
            "image": Js.Undefined.return {
                "smallImageUrl": smallImageUrl,
                "largeImageUrl": largeImageUrl
            },
            "text": Js.Undefined.return text,
            "title": Js.Undefined.return title
        }
        | LinkAccount => {
            "_type": "LinkAccount",
            "content": Js.Undefined.empty,
            "image": Js.Undefined.empty,
            "text": Js.Undefined.empty,
            "title": Js.Undefined.empty
        }
    };
};

module Speech = {
    type types =
        | PlainText string
        | SSML (Result.result speech string);

    let to_js_object speech => switch speech {
        | PlainText speech => Result.Ok (Js.Undefined.return {
            "outputSpeech": {
                "_type": "PlainText",
                "text": Js.Undefined.return speech,
                "ssml": Js.Undefined.empty
            }
        })
        | SSML speech => speech |> Result.map Js.Undefined.return
    };
};

module Ask = {
    let createElement speech::speech reprompt::reprompt=? card::card=? children::_children () => {
        let s = speech |> Speech.to_js_object;
        let r = reprompt |> Option.map Speech.to_js_object |> Option.getOrElse (Result.Ok Js.Undefined.empty);

        Result.all s r |> Result.map (fun (speech, reprompt) => {
            "version": "1.0",
            "response": {
                "ask": speech,
                "say": Js.Undefined.empty,
                "reprompt": reprompt,
                "card": card |> Option.map Card.to_js_object |> Js.Undefined.from_opt,
                "shouldEndSession": Js.Boolean.to_js_boolean false
            }
        });
    };
};

module Say = {
    let createElement speech::speech card::card=? children::_children () => {
        speech |> Speech.to_js_object |> Result.map (fun speech => {
            "version": "1.0",
            "response": {
                "ask": Js.Undefined.empty,
                "say": speech,
                "reprompt": Js.Undefined.empty,
                "card": card |> Option.map Card.to_js_object |> Js.Undefined.from_opt,
                "shouldEndSession": Js.Boolean.to_js_boolean true
            }
        });
    };
};
