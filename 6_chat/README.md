# Let's build: Zap Chat

Now that we've built a watered-down version of Riak, let's look at doing
something more valuable and less like rolling your own crypto: we'll build
a useful-sounding service on top of Riak Core.

This service will be... a chat app!

(...I'm sorry, it was either that or tic-tac-toe)

# Stage 1 - basic chat, description
The frontend is a simple TCP server. The client is telnet or netcat. A client
connects like this:

    $ nc localhost 4040
    connected! 'set-name <name>' and 'join <chat name>' to get started
    >

Now that I'm connected, I can set my name:

    > set-name Alex
    ok your name is Alex
    > 

And with a name, I can join some rooms:

    > join super-cool-room
    joined super-cool-room! 'say super-cool-room <message>' to chat
    >

Finally, I can chat:

    > say super-cool-room hurray!
    super-cool-room - Alex: hurray!
    >

Naturally, this is more interesting with more users, so feel free to connect as
many users as you like (or script up something to test with):

    $$ nc localhost 4040
    connected! 'set-name <name>' and 'join <chat name>' to get started
    > set-name Ash
    ok your name is Ash
    > join super-cool-room
    joined super-cool-room! 'say super-cool-room <message>' to chat
    > say super-cool-room hi everyone!
    super-cool-room - Ash: hi everyone!
    >

    # and in the other window, with Alex:
    > super-cool-room - Ash: hi everyone!
    >

## To implement

* `zap_core_service:join(channel, client)` / `ZapCore.Service.join(room, client)`
    * this should take a user's room join request and subscribe them to
      further messages in this room. if you like, add a "* user foo has joined <room>" message
* `zap_core_service:say(channel, msg)` / `ZapCore.Service.say(room, msg)` 
    * broadcast message to all clients subscribed to a given room

## Consider:

* Does this application need an op coordinator? Perhaps only for some operations (joins vs broadcasts)?
* Does this application need handoff?

# Stage 2 - user join/part messages
Let's add some simple 'room join', 'room left' messages.

    $ nc localhost 4040
    connected! 'set-name <name>' and 'join <chat name>' to get started
    > set-name Alex
    ok your name is Alex
    > join super-cool-room
    joined super-cool-room! 'say super-cool-room <message>' to chat
    * Alex has joined super-cool-room
    ^C

    # in another window
    * Alex has left super-cool-room

## To implement:

* change `zap_frontend_client`/`ZapFrontend.Client` to call
  `zap_core_service:leave(room, client, name)` function when the connection is
  closed.

* change `zap_core_vnode`/`ZapCore.Vnode` to 
    * remove the client from the list of subscribed clients
    * notify other members that this client left

# Choose your own adventure!

To continue with this exercise, follow this document. If you're interested
in games, or an exercise that manages more complex multi-user interactions, switch to
README-game.md.


# Stage 3 - per-room history

Let's add some state to Zap Chat: in order to keep them from leaving for Slack, the users
need to have persistent chat history on the server side.

    $ nc localhost 4040
    connected! 'set-name <name>' and 'join <chat name>' to get started
    > set-name Alex
    ok your name is Alex
    > join super-cool-room
    joined super-cool-room! 'say super-cool-room <message>' to chat
    the most recent messages are:

    super-cool-room - Alex: hurray!
    super-cool-room - Ash: hi everyone!
    >

## To implement:
* change `zap_core_vnode`/`ZapCore.Vnode` so it keeps per-room message history,
  and sends it on client connection.

## Consider:
* is the need for op coordination/handoff the same as before? 

# Stage 4 - unread messages

Global unread messages for a given room is nifty and all, but modern chat
services can tell me exactly where I need to start in the backlog to catch up
on unread messages -- they know whether I've viewed a particular message.

    $ nc localhost 4040
    connected! 'set-name <name>' and 'join <chat name>' to get started
    > set-name Alex
    ok your name is Alex
    > show-unread super-cool-room
    You missed the following messages:
        Ash: ... so when you type hunter2, all I see is *******
        Jack: really? that's funny! you can hunter2 my hunter2-ing hunter2!
    > show-unread super-cool-room
    no unread messages

## To implement:
* add a 'show-unread' command to the Zap frontend
* implement `show_unread(room, user)` in the Zap Core service
* track user connection status in Zap Core Vnode broadcast: if they're connected with
  a message arrives, they have read it, so their unread message history does
  not increase
* change `zap_core_vnode`/`ZapCore.Vnode` so it keeps a per-user position in
  the room chat history. this position should reset to latest when the users is
  shown their unread messages
* add a handler to the Zap Core Vnode to return a user's unread chat history
  and reset their unread pointer to latest

# Stage 5 - chat ops / chat UI

The latest fad in user interfaces is chat-based interactions: this can be
operations chat in a work slack, like "@hubot: backup primary db --slow", or
something more commercially oriented, like having the user navigate a "chat
tree" to purchase a product or similar. 

Here's one idea to try:

    $ nc localhost 4040
    connected! 'set-name <name>' and 'join <chat name>' to get started
    > set-name Alex
    ok your name is Alex
    > shop hotels
    Where would you like to book a hotel?
        a) stockholm
        b) bilbao
    > b
    Here are some hotels in bilbao:
        a) nice surfing lodge (*)
        b) fancy guggenheim palace (******)
    > a
    nice surfing lodge (*) is 20 euro / night. is that ok? (y/n)
    > y
    ok, you booked a hotel!

This is a silly example, but you can see how the server needs to interpret the
user's answer differently based on their current state.

## Consider: 
* what's the best sharding key for this exercise? should all "hotel" interactions
  happen on the same vnode? Hotels in bilbao? Which data should be a part of
  the chat service, and which data should live in an external DB?
