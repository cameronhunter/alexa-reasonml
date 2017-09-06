# Reasonable Alexa Skill

The actual "skill" part of this project is contained within `src/Index.re` and `src/HelloWorld.re`. The index file
contains a simple router which handles both a `LaunchRequest` and an `IntentRequest` called `HelloWorld`.

There are no tests, however, there is a demo file that can be run using `npm test`. This will build the project and run
the skill with an example request; output is shown in the console.

The rest of the files provide SSML support and attempt to wrap the Alexa Skill Kit. These should all be extracted into a
separate installable package when I figure out how to do that.
