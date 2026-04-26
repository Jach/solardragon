# SolarDragon

For a long time I've wanted to make an arcade style game like [SolarWolf](https://www.pygame.org/shredwheat/solarwolf/index.shtml), which itself is an homage to an old Atari game SolarFox. This is that project.

(You can watch me play SolarWolf somewhat badly [here](https://www.youtube.com/watch?v=PpiQmBPAmOE).)

Initially, however, this is just going to begin as a reimplementation of SolarWolf, with its original assets too, but in Common Lisp with my lgame library.

As of this readme update, the main gaps remaining are:

* Implementing the rest of the menu functionality (setup screen mainly, but the cool credits effect too)
* Add music and sfx everywhere
* Enemy guardians firing
* Powerups and their effects
* Tutorial boxes
* Asteroid obstacles
* Mines
* Player death, game overs, scores, continuing
* Game beat
* Performance profiling and testing on lesser machines

Bonus / stretch goals:

* The game is artificially capped at 60 FPS, allow it to be uncapped (will require reworking frame-count logic)
* Make a simple in-game level editor
* Try making a few of my own levels
* Replace graphics / sounds with something new
* Consider new or different gameplay mechanics / powerups / modes (2 player? Netplay? Modern controller support? Steam deck?)
* Go through the effort of creating binaries for distribution on Linux and Windows,
  conditionally filtering out debug helpers,
  maybe even try to release on steam or itch.io...

# License

SolarDragon was written by Jach. It's released under the LGPL, same as SolarWolf.

Most code was written without reference to SolarWolf's code, though, and is architecturally quite different.
If the license is a barrier to you for some reason, contact me.
