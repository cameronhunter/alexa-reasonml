include Result;

let attr value to_string name => value |> Option.map to_string |> Option.map (fun v => name ^ "=\"" ^ v ^ "\"");

let join list => String.trim (List.fold_left (fun a b => a ^ b) "" list);

module Audio = {
    let createElement ::src ::children=? () => {
        switch children {
            | Some [] => Ok ("<audio src=\"" ^ src ^ "\" />")
            | _ => Error "Audio tag does not support child nodes"
        }
    };
};

module Break = {
    type strength =
      | None          /* No pause should be outputted. This can be used to remove a pause that would normally occur (such as after a period). */
      | ExtraWeak     /* No pause should be outputted (same as none). */
      | Weak          /* Treat adjacent words as if separated by a single comma (equivalent to medium). */
      | Medium        /* Treat adjacent words as if separated by a single comma. */
      | Strong        /* Make a sentence break (equivalent to using the <s> tag). */
      | ExtraStrong   /* Make a paragraph break (equivalent to using the <p> tag). */
      ;

    let strength_to_string s => switch s {
      | None => "none"
      | ExtraWeak => "x-weak"
      | Weak => "weak"
      | Medium => "medium"
      | Strong => "strong"
      | ExtraStrong => "x-strong"
    };

    type duration =
      | Milliseconds int
      | Seconds int
      ;

    let duration_to_string d => switch d {
        | Milliseconds value => (string_of_int value) ^ "ms"
        | Seconds value => (string_of_int value) ^ "s"
    };

    let createElement ::strength=? ::time=? ::children=? () => {
        switch (children, strength, time) {
            | (Some [], Some s, _) => Ok ("<break strength=\"" ^ (strength_to_string s) ^ "\" />")
            | (Some [], None, Some d) => Ok ("<break time=\"" ^ (duration_to_string d) ^ "\" />")
            | (Some [], _, _) => Ok ("<break strength=\"" ^ (strength_to_string Medium) ^ "\" />")
            | _ => Error "Break tag does not support child nodes"
        }
    };
};

module Emphasis = {
    type level =
      | Strong
      | Moderate
      | Reduced;

    let level_to_string l => switch l {
        | Strong => "strong"
        | Moderate => "moderate"
        | Reduced => "reduced"
    };

    let createElement level::level children::(children: list string) () => {
        switch children {
            | [] => Error "Emphasis tag requires at least one child"
            | values => Ok ("<emphasis level=\"" ^ (level_to_string level) ^ "\">" ^ (join values) ^ "</emphasis>")
        }
    };
};

module P = {
    let createElement children::(children: list string) () => {
        switch children {
            | [] => Error "P tag requires at least one child"
            | values => Ok ("<p>" ^ (join values) ^ "</p>")
        }
    };
};

module Phoneme = {
    type alphabet =
      | IPA
      | X_SAMPA;

    let alphabet_to_string a => switch a {
        | IPA => "ipa"
        | X_SAMPA => "x-sampa"
    };

    type ph = string;

    let createElement alphabet::alphabet ph::(ph: ph) children::(children: list string) () => {
        switch children {
            | [] => Error "Phoneme tag requires at least one child"
            | values => Ok ("<phoneme alphabet=\"" ^ (alphabet_to_string alphabet) ^ "\" ph=\"" ^ ph ^ "\">" ^ (join values) ^ "</phoneme>")
        }
    };
};

module Prosody = {
    type rate =
      | ExtraSlow
      | Slow
      | Medium
      | Fast
      | ExtraFast
      | Percent int;

    let rate_to_string r => switch r {
        | ExtraSlow => "x-slow"
        | Slow => "slow"
        | Medium => "medium"
        | Fast => "fast"
        | ExtraFast => "x-fast"
        | Percent value => (string_of_int value) ^ "%"
    };

    type pitch =
      | ExtraLow
      | Low
      | Medium
      | High
      | ExtraHigh
      | Percent int;

    let pitch_to_string p => switch p {
        | ExtraLow => "x-low"
        | Low => "low"
        | Medium => "medium"
        | High => "high"
        | ExtraHigh => "x-high"
        | Percent value when value < 0 => "-" ^ (string_of_int value) ^ "%"
        | Percent value when value > 0 => "+" ^ (string_of_int value) ^ "%"
        | Percent _ => "0%"
    };

    type volume =
      | Silent
      | ExtraSoft
      | Soft
      | Medium
      | Loud
      | ExtraLoud
      | Decibels float;

    let volume_to_string v => switch v {
        | Silent => "silent"
        | ExtraSoft => "x-soft"
        | Soft => "soft"
        | Medium => "medium"
        | Loud => "loud"
        | ExtraLoud => "x-loud"
        | Decibels value when value < 0.0 => "-" ^ (string_of_float value) ^ "dB"
        | Decibels value when value > 0.0 => "+" ^ (string_of_float value) ^ "dB"
        | Decibels _ => "0dB"
    };

    let createElement ::rate=? ::pitch=? ::volume=? children::(children: list string) () => {
        let _rate = attr rate rate_to_string "rate";
        let _pitch = attr pitch pitch_to_string "pitch";
        let _volume = attr volume volume_to_string "volume";

        let attributes = switch (Option.flatten [_rate, _pitch, _volume]) {
            | [] => None
            | values => Some (List.fold_left (fun a b => a ^ " " ^ b) "" values)
        };

        switch (children, attributes) {
            | ([], _) => Error "Prosody tag requires at least one child"
            | (_, None) => Error "Prosody tag requires at least one attribute: rate, pitch, volume"
            | (children, Some attributes) => Ok ("<prosody" ^ attributes ^ ">" ^ (join children) ^ "</prosody>")
        };
    };
};

module S = {
    let createElement children::(children: list string) () => {
        switch children {
            | [] => Error "S tag requires at least one child"
            | children => Ok ("<s>" ^ (join children) ^ "</s>")
        }
    };
};

module SayAs = {
    type interpretation =
      | Characters      /* Spell out each letter */
      | SpellOut        /* Spell out each letter */
      | Cardinal        /* Interpret the value as a cardinal number */
      | Number          /* Interpret the value as a cardinal number */
      | Ordinal         /* Interpret the value as an ordinal number */
      | Digits          /* Spell each digit separately */
      | Fraction        /* Interpret the value as a fraction */
      | Unit            /* Interpret a value as a measurement */
      | Date            /* Interpret the value as a date. Specify the format with the format attribute */
      | Time            /* Interpret a value such as 1'21 as duration in minutes and seconds */
      | Telephone       /* Interpret a value as a 7-digit or 10-digit telephone number */
      | Address         /* Interpret a value as part of street address */
      | Interjection    /* Interpret a value as an interjection */
      | Expletive       /* "Bleep" out the content inside the tag */
      ;

    let interpretation_to_string i => switch i {
        | Characters => "characters"
        | SpellOut => "spell-out"
        | Cardinal => "cardinal"
        | Number => "number"
        | Ordinal => "ordinal"
        | Digits => "digits"
        | Fraction => "fraction"
        | Unit => "unit"
        | Date => "date"
        | Time => "time"
        | Telephone => "telephone"
        | Address => "address"
        | Interjection => "interjection"
        | Expletive => "expletive"
    };

    let createElement interpretAs::interpretAs children::(children: list string) () => {
        switch children {
            | [] => Error "SayAs tag requires at least one child"
            | children => Ok ("<say-as interpret-as=\"" ^ (interpretation_to_string interpretAs) ^ "\">" ^ (join children) ^ "<say-as>")
        }
    };
};

module Speak = {
    let to_object result => {
        "outputSpeech": {
            "_type": "SSML",
            "text": Js.Undefined.empty,
            "ssml": Js.Undefined.return ("<speak>" ^ result ^ "</speak>")
        }
    };

    let createElement children::children () => {
        switch children {
            | [] => ASK.Speech.SSML (Error "Speak tag requires at least one child")
            | children => {
                let combine a b => switch (a, b) {
                    | (Ok a, Ok b) => Ok (a ^ b)
                    | (Error a, _) => Error a
                    | (_, Error b) => Error b
                };

                let result = children |> List.fold_left combine (Ok "") |> Result.map to_object;

                switch result {
                    | Ok speech => ASK.Speech.SSML (Ok speech)
                    | Error error => ASK.Speech.SSML (Error error)
                };
            }
        }
    };
};

module Sub = {
    type alias = string;

    let createElement alias::(alias: alias) children::(children: list string) () => {
        switch children {
            | [] => Error "Sub tag requires at least one child"
            | children => Ok ("<sub alias=\"" ^ alias ^ "\">" ^ (join children) ^ "</sub>")
        }
    };
};

module W = {
    type amazon_role =
      | VB
      | VBD
      | NN
      | SENSE_1;

    let amazon_role_to_string r => switch r {
        | VB => "amazon:VB"
        | VBD => "amazon:VBD"
        | NN => "amazon:NN"
        | SENSE_1 => "amazon:SENSE_1"
    };

    type role =
      | Amazon amazon_role;

    let role_to_string r => switch r {
        | Amazon role => (amazon_role_to_string role)
    };

    let createElement role::role children::(children: list string) () => {
        switch children {
            | [] => Error "W tag requires at least one child"
            | children => Ok ("<w role=\"" ^ (role_to_string role) ^ "\">" ^ (join children) ^ "</w>")
        }
    };
};
