# Reasonable Alexa Skill

_This is a rough first attempt at building an Alexa skill using ReasonML. It's messy and my first project in the language, any tips/tricks/suggestions are more than welcome._

The "skill" part of this project is contained within `src/Index.re` and `src/HelloWorld.re`. The index file
contains a router which handles both a `LaunchRequest` and an `IntentRequest` called `HelloWorld`. The implementation of the
`HelloWorld` intent handler is in `src/HelloWorld.re`.

There are no tests, however, there is a demo file that can be run using `npm run demo`. This will build the project and run
the skill with an example request; output is shown in the console.

The rest of the files provide SSML support and attempt to wrap the Alexa Skill Kit and AWS Lambda. These should all be
extracted into a separate installable package (when I figure out how to do that) which will significantly improve the cleanliness of the codebase.
