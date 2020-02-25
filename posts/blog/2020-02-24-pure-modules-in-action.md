---
title: Pure Modules in Action
author: Adam Fiedler
teaser: "In an <a href='/blog/2019-08-15-pure-modules'>earlier post</a> I wrote
about what I imagine under pure modules and reasons why one should write 
as much JS code as possible in this paradigm. 
Then I described some techniques how to get around potential pitfalls. 
One thing is theory, another is practice. 
Let's look at one particular example of using pure modules to demonstrate their 
feasibility and advantages."
---

TicTacToe has been a famous game known for centuries and incindentally, also used in the official [React tutorial](https://reactjs.org/tutorial/tutorial.html).
We can surely revisit the tutorial and rethink it to show how separate game logic from event handling/rendering into a *pure* module.
Once there, we will have all of the advantages mentioned in the article on pure modules.
And that not only through separation of concerns: the pureness of the module will allow us to easily reason about the module, unit test it and parallelize it (as will be shown later).

# Implementation

A blueprint of the task is saved in [one of my repositaries](https://github.com/fiedlr/fp-workshop).
Feel free to work it out on your own and compare your solution with mine.
If you have some experience with functional programming, by now you should have all the knowledge necessary to solve it.
For the rest of us, we start first with how the model looks:

```{.javascript .numberLines .line-anchors startFrom="1"}
{
  // 0 stands for not played yet, 1 for player X, 2 for player O
  board: [
    0,0,0, 
    0,0,0, 
    0,0,0
  ],
  // number of won games by each player
  scoreOfPlayer1: 0,
  scoreOfPlayer2: 0
}
```

Notice that the board is simply represented as an array preserving for each tile the id of the player who played it.
We do not need to differentiate between rows and columns, that can be taken care of with CSS.
The model attributes are passed to us *for us* in the corresponding functions that we will be implementing.

We cannot change the model attributes *in any way*, they should be taken as constants -- we can only decide what we *return* in the functions.
Remember also that since we will be implementing a pure module, we cannot use anything but *arrow expressions*.

There are two essential functions needed to be done before the game starts functioning:

```{.javascript .numberLines .line-anchors startFrom="1"}
import React from 'react'
import Tile from './tile'

const render = board => handleClick => 
  <div className="TicTacToe"></div>
 
const nextStep = tileId => (state, _) => state

export {render, nextStep}
```

The `render` function takes care of rendering the board according to its state given in `board` and a `handleClick` handler, which just calls `nextStep` when a tile is clicked.
In comparison, `nextStep` takes the id of the clicked tile (which we will need to generate somehow) and the `state`, which contains the model we just went over (the `_` argument stands for the React props which are not needed here).
We want to use *arrow expressions* only to implement these functions.
As will be soon evident, this is possible without any nasty hacking or tricks.

## Rendering
 
For the `renderGame` function, we first need to know what props the component `Tile` takes.

```html
<Tile playerId={...} onClick={...} />
```

From the model we know that the prop `playerId` can take three values:

- `0` denotes the tile hasn't been played by any player yet,
- `1` denotes Player 1,
- `2` denotes Player 2.

The `onClick` prop takes the handler for clicking the tile. Voila, we are provided
with such a handler already in one of our arguments, namely `handleClick`.
If we look at its definition, it takes `tileId` to know what changes to do in the model, which we *here* decide how to generate. 
And since the game CSS handles for us that only 3 tiles fit in a row, we only need to render 9 tiles with 9 unique IDs according to who played them (none, player 1 or 2).

How to create several tiles when we cannot use any loops and don't want to copy-paste?
This is no problem at all because we have all the available tiles in the `board` array. 
We can *map* each abstract tile onto its corresponding DOM tile.
The `map` function also provides the index of the tile in the array, which will serve ok as an ID for our purposes.
We will pass the index into the `handleClick` function.

```{.javascript .numberLines .line-anchors startFrom="1"}
const render = board => handleClick => 
  <div className="TicTacToe">
    board.map((tilePlayedBy, tileIndex) => 
      <Tile 
        playerId={tilePlayedBy} 
        onClick={() => handleClick(tileIndex)} 
        id={id} // for React purposes
      />)
  </div> 
```

The `Tile` component handles the styles for us, which makes the `render` function finished.

## Managing state

Although the `render` function seemed easy to do, `nextStep` will be trickier.
We can *decompose* the game into three main steps that are made in any particular moment. 
This is perfect for using function decomposition not to get lost in the process.

1. Determining whose turn it is ~> `getIdOfPlayerOnTurn`
2. Determining the game is finished and getting the winner id ~> `getWinnerId`
3. Resetting the game and incrementing the score of the winner ~> `newGame`

### `getIdOfPlayerOnTurn`
This one is probably the least obvious.
Since we cannot pass any helper variable to tell us whose turn it is, we must only blindly believe in the insight of the blueprint's creator (me :-D) and assume that the data we have in hand tell us already.
This is indeed true: the board reveals how many turns we have so far played in the number of non-zero tiles. 
And since there are two players, player X will play each *even* turn and player O each *odd* turn. 

```{.javascript .numberLines .line-anchors startFrom="1"}
const getNumberOfTurns = board => board.reduce(
  (tilePlayedBy, count) => count + Math.ceil(tilePlayedBy / 2),
  0
)

const getIdOfPlayerOnTurn = board => getNumberOfTurns(board) % 2 + 1
```

### `getWinnerId`
All of us have probably played TicTacToe in some point of our lives.
We need three X's (O's) in a row (~> `getRow`), column (~> `getCol`), or a diagonal (~> `getDia`).
If in one of these cases there are three identical playerIds to the playerId on turn, we have a winner with this id (`winnerRowOrColExists`)
We use an IIFE to create a constant `winnerArray` to be compared with both of a given type's neighbors and save us the trouble to compare them separately.

```{.javascript .numberLines .line-anchors startFrom="1"}
const getRow = board => tileIndex => (rowId => [
  board[rowId], board[rowId + 1], board[rowId + 2]
])(tileIndex / 3)

const getCol = board => tileIndex => (colId => [
  board[colId], board[colId + 3], board[colId + 6]
])(tileIndex % 3)

const winnerRowOrColExists = board => winCombo =>
  board.reduce((_, _, tileIndex) => 
    getRow(board, tileIndex) === winCombo || 
    getCol(board, tileIndex) === winCombo,
    False
  )

const getDia1 = board => [board[0], board[4], board[8]]

const getDia2 = board => [board[2], board[4], board[6]]

const winnerDiagonalExists = board => winCombo => 
  getDia1(board) === winCombo || getDia2(board) === winCombo

const getWinnerId = board => playerOnTurn => (winCombo => 
  winnerRowOrColExists(board, winCombo) ||
  winnerDiagonalExists(board, winCombo)  ? playerOnTurn : 0
)([playerOnTurn, playerOnTurn, playerOnTurn])
```

Although `getWinnerId` is a little more inefficient that it need be, for brevity
we will keep it as it is... We are still only working with a 3x3 grid.
If we needed bigger grids, we could use several techniques (e.g. lazy evaluation with boolean operators, checking only from the top left corners, winner configuration imprints...).

### `newGame`
We just need to return a reset game with incremented score of the winner.
This is straightforward:

```{.javascript .numberLines .line-anchors startFrom="1"}
const newGame = board => winnerId => {
  board: board.fill(0),
  `scoreOfPlayer${winnerId}`: state[`scoreOfPlayer${winnerId}`] + 1
}
```

### `nextStep`
Putting it all together, we are finally able to implement `nextStep` in a clear manner.

# Testing

# Parallelization?
