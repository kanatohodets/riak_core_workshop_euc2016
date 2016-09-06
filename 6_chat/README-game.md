# Games with Riak Core

Riak Core's sharding and failover make it a good match for applications that
benefit both from CPU and state distribution. One example of this is a game
backend. 

A game backend often needs to contain the authoratative game state for clients
in order to mitigate cheating: nothing from the clients can be fully trusted,
so their input (moves in a game, for example) must be validated against the
game rules and server-side state.

Luckily, netcat/telnet are the ultimate thin client, so let's build a game
backend on top of the chat application we began in the other document.

# Stage 1: simple guessing game
Imagine a simple guessing game: all users in room FOO race to guess a number
between 1-20. The game starts when a user says `start-game <roomname> <number-to-guess>`.
The game continues until a user guesses the correct number.

    $ nc localhost 4040
    connected! 'set-name <name>' and 'join <chat name>' to get started
    > set-name Bob
    ok your name is Bob
    > join playground
    joined playground! 'say playground <message>' to chat
    > start-game playground 9
    playground - Bob has started a game. place your guesses!
    ... (time passes)
    playground - Jane guessed 3, but that's not it
    playground - Jane guessed 9! Jane wins!

    # in another window
    $$ nc localhost 4040
    connected! 'set-name <name>' and 'join <chat name>' to get started
    > set-name Jane
    ok your name is Jane
    > join playground
    joined playground! 'say playground <message>' to chat
    playground - Bob has started a game. place your guesses, 0-20!
    > guess playground 3
    playground - Jane guessed 3, but that's not it
    > guess playground 9
    playground - Jane guessed 9! Jane wins!
    >

# Stage 2: a strategic action game

After each guess, the server broadcasts: 

* the user who guessed
* the value
* a hint: "higher", "lower", or "winner!". 

Limit each user to 3 guesses.

# Stage 3: scoreboard
Maintain a scoreboard for users in a given room. Give to to users who request
it:

    > scoreboard playground
    Top playground guessers are:
        Jane: 2 wins, 0 losses
        Bob: 0 wins, 2 losses

# Stage 4: variable points

Let's make the game more strategic: award the winning guess a variable number
of points depending on how many guesses have already been placed by all users.

Use the following scheme to award points:

* a correct guess that is the first guess in the game gets 10 points
* second or third guess get 8 points
* fourth guess gets 5 points
* fifth guess gets 3 points
* sixth and beyond get 1 point

Adjust the leaderboard to maintain point totals along with wins.

# Stage 5
What is sensible handoff? How about coordination for game actions? Should game
states be replicated on multiple vnodes?
