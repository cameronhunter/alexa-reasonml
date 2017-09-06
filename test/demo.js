const Skill = require('../lib/js/src');

var event = {
    request: {
        type: "IntentRequest",
        intent: {
            name: "HelloWorld",
            slots: {
                name: "Cameron"
            }
        }
    },
    session: {
        application: {
            applicationId: "cameron"
        }
    }
};

Skill.handler(event, null, (error, response) => {
    if (error) {
        console.log(`Error: ${error}`);
    } else {
        console.log(JSON.stringify(response, null, 2));
    }
});
