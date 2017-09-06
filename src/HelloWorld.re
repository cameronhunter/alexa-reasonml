include SSML;
include ASK;

let handler name::optionalName => {
    let message = "Hello " ^ (Option.getOrElse "world" optionalName);

    let speech = (
        <Speak>
            <S>message</S>
        </Speak>
    );

    let card = Card.Simple "Hello!" message;

    <Ask speech reprompt=speech card />;
};
