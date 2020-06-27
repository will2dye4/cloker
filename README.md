# cloker

Cloker is a rudimentary poker engine written in Clojure. It started as a Clojure port
of my Python-based poker engine, [pyker](https://github.com/will2dye4/pyker).

**NOTE:** This project is a work in progress. It is currently not very fully featured. Sorry about that.

## Installing

This project is currently not published anywhere. You may clone this repository and run
locally using [Leiningen](https://leiningen.org).

## Playing a Hand

Pyker currently only supports one variation of poker: [Texas hold 'em](https://en.wikipedia.org/wiki/Texas_hold_%27em).

Start a game with the `play-game` function. The game will have four players (who all start with 
10,000 chips), no ante, and blinds of 100 (small blind) and 200 (big blind). The game will deal
a hand to all players and, by default, prompt for input asking how each player should act at each 
stage of the game.

The game will tell you what actions are legal for a given player. For example, if the prompt says
`Player 4 may fold, call, raise`, then you may type `fold` to fold Player 4's hand, `call` to call
the current bet, or `raise N` to raise the current bet by `N` chips. (You may also say `raise to N`
to raise the current bet to exactly `N` chips.) Betting works the same: type `bet N` to make an 
initial bet of `N` chips. The other option that is sometimes available is `check`, which means
to pass the action to the next player without folding or betting (only allowed when there are no previous
bets in the current round of betting).

Optional keyword arguments:
* `:mode` - Control the level of interactivity (default: `:interactive`). Three modes exist:
  `:auto`, `:interactive`, and `:single-player`. Interactive mode is the default behavior described
  above. In "auto" mode, every player will automatically call the big blind and check the remaining 
  rounds, folding if they do not have enough chips. (This is a stand-in for an "AI" with any actual 
  *intelligence*.) In single-player mode, all players except Player 1 are in auto mode, and you control
  Player 1 in interactive mode. 

### Example

```
$ lein repl
nREPL server started on port 52010 on host 127.0.0.1 - nrepl://127.0.0.1:52010
REPL-y 0.4.4, nREPL 0.6.0
Clojure 1.10.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_77-b03
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

cloker.core=> (play-game)
Welcome! A new game is starting.

============= Ante / Blinds =============
--> Player 2 bets 100
--> Player 3 bets 200

================== Pre-Flop ==================
Player 4  	[6♠︎ 6♣]  	10,000	  Pair
[200] Player 4 may fold, call, raise: call
--> Player 4 bets 200
Player 1  	[K♠︎ Q♥︎] 	10,000	  High card
[200] Player 1 (D) may fold, call, raise: call
--> Player 1 bets 200
Player 2  	[J♣ 9♠︎]  	 9,900	  High card
[200] Player 2 (SB) may fold, call, raise: fold
--> Player 2 folds
Player 3  	[7♠︎ 7♣]  	 9,800	  Pair
[200] Player 3 (BB) may fold, check, bet: check

================== Flop ==================
[10♣ 4♥︎ 3♣]        	Pot: 700
Player 3  	[7♠︎ 7♣]  	 9,800	  Pair
[0] Player 3 (BB) may fold, check, bet: bet 400
--> Player 3 bets 400
Player 4  	[6♠︎ 6♣]  	 9,800	  Pair
[400] Player 4 may fold, call, raise: call
--> Player 4 bets 400
Player 1  	[K♠︎ Q♥︎] 	 9,800	  High card
[400] Player 1 (D) may fold, call, raise: fold
--> Player 1 folds

================== Turn ==================
[10♣ 4♥︎ 3♣ Q♠︎]    	Pot: 1,500
Player 3  	[7♠︎ 7♣]  	 9,400	  Pair
[0] Player 3 (BB) may fold, check, bet: bet 400
--> Player 3 bets 400
Player 4  	[6♠︎ 6♣]  	 9,400	  Pair
[400] Player 4 may fold, call, raise: raise 600
--> Player 4 bets 1,000
Player 3  	[7♠︎ 7♣]  	 9,000	  Pair
[1,000] Player 3 (BB) may fold, call, raise: call
--> Player 3 bets 600

================== River ==================
[10♣ 4♥︎ 3♣ Q♠︎ 5♥︎]	Pot: 3,500
Player 3  	[7♠︎ 7♣]  	 8,400	  Pair
[0] Player 3 (BB) may fold, check, bet: check
Player 4  	[6♠︎ 6♣]  	 8,400	  Pair
[0] Player 4 may fold, check, bet: check

================ Showdown ================
Player 3 wins with a pair of sevens

Player 1	   9,800	0 wins
Player 2	   9,900	0 wins
Player 3	  11,900	1 win
Player 4	   8,400	0 wins


Would you like to play another hand? no
Goodbye!
```

## Simulating Hands

See the statistics for the outcomes of multiple simulated hands with the `hand-stats` function.
By default, this will simulate 1,000 hands with four players in each hand. The entire board
will be dealt (flop, turn, and river) and each player's hand will be evaluated. (There is no betting.)
After simulating all hands, a summary of outcomes will be printed, along with the strength
of each hand type, i.e., the percentage of the total number of times the hand type occurred that
that hand type won the hand.

Optional keyword arguments:
* `:n` - Number of hands to simulate (default: 1,000)
* `:num-players` - Number of players in each hand (default: 4)

### Example

```
$ lein repl
nREPL server started on port 52010 on host 127.0.0.1 - nrepl://127.0.0.1:52010
REPL-y 0.4.4, nREPL 0.6.0
Clojure 1.10.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_77-b03
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

cloker.core=> (hand-stats)
Simulating 1,000 hands with 4 players...

============== All Outcomes ==============
       High card      515         (12.88%)
            Pair    1,698         (42.45%)
        Two pair    1,048         (26.20%)
 Three of a kind      252          (6.30%)
        Straight      230          (5.75%)
           Flush      115          (2.88%)
      Full house      131          (3.28%)
  Four of a kind       10          (0.25%)
  Straight flush        1          (0.03%)
     Royal flush        0          (0.00%)
------------------------------------------
           Total    4,000        (100.00%)

============ Winning Outcomes ============
       High card        1          (0.10%)
            Pair      176         (16.96%)
        Two pair      311         (29.96%)
 Three of a kind      150         (14.45%)
        Straight      191         (18.40%)
           Flush       87          (8.38%)
      Full house      111         (10.69%)
  Four of a kind       10          (0.96%)
  Straight flush        1          (0.10%)
     Royal flush        0          (0.00%)
------------------------------------------
           Total    1,038        (100.00%)

================= Hand Strength ==================
       High card            1 / 515        (0.19%)
            Pair        176 / 1,698       (10.37%)
        Two pair        311 / 1,048       (29.68%)
 Three of a kind          150 / 252       (59.52%)
        Straight          191 / 230       (83.04%)
           Flush           87 / 115       (75.65%)
      Full house          111 / 131       (84.73%)
  Four of a kind            10 / 10      (100.00%)
  Straight flush              1 / 1      (100.00%)
     Royal flush              0 / 0        (0.00%)
```
