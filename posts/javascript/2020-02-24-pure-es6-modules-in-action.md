---
title: Pure ES6 Modules in Action
author: Adam Fiedler
teaser: "In an <a href='/javascript/2019-08-15-pure-modules'>earlier post</a> I wrote
about what I imagine under pure ES6 modules and listed reasons why one should write
as much JS code as possible in this paradigm.
Then I described some techniques how to get around potential pitfalls.
One thing is theory, another is practice.
Let's look at one particular example of using pure modules to demonstrate their
feasibility and advantages."
modified: true
tags: pure-modules, functional-programming, es6
---

TicTacToe has been a famous game known for centuries and incindentally, also used in the official [React tutorial](https://reactjs.org/tutorial/tutorial.html).
We can surely revisit the tutorial and rethink it to show how separate game logic into a pure module and away from any event handling/rendering.
Once there, we will have all of the advantages mentioned in the article on pure modules.
And that not only through separation of concerns: the pureness of the module will allow us to easily reason about the module, type, unit test and parallelize it (as will hopefully be shown in a follow-up article).

# Implementation

How exactly can we implement the logic of TicTacToe in a pure module?
We should first start thinking about the *model*, or more explicitly, the state of the game.
It does not have much to do with pure modules per se, but it will greatly influence how our resulting code will look.
This state will be maintained by a React component (why not).
For our simple purposes, the React component will handle the state directly.
That does not mean it couldn't be extracted to a Redux container or something similar.
Our state will not be so different from the one in the React tutorial, besides for my personal preference of saving the played tiles with numbers:

```javascript
{
  // 0 stands for not played yet, 1 for player X, 2 for player O
  board: [
    0, 0, 0,
    0, 0, 0,
    0, 0, 0
  ],
  // number of won games by each player
  scoreOfPlayer1: 0,
  scoreOfPlayer2: 0
}
```

Notice that the board is simply represented as an array preserving for each tile the id of the player who played it.
There is no need to differentiate between rows and columns, that can be taken care of with CSS.
We will pass the model attributes from our React component to the functions that we will be implementing in our pure module intended for game logic.
The idea is that our React component will *only* handle events and re-rendering.
In a way, it will work similarly as an integrated Redux reducer with the mouse clicking as our action.
After getting a click on a tile, it will set a new state based on the previous state.
That's it.
The calculation of the new state will be treated in our pure module.

## Rendering

First let us briefly overview the implementation of our main React component.

```{.javascript .numberLines .line-anchors .nowrap startFrom="1"}
import React, { useState } from 'react'
import { nextStep, renderGame } from './game.pure'

export default function TicTacToe(props) {
  const [state, updateState] = useState({
    board: [
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    ],
    scoreOfPlayer1: 0,
    scoreOfPlayer2: 0
  });

  const onClick = tileId => updateState(nextStep(tileId));

  return <div className="TicTacToe">
    {renderGame(state.board)(onClick)}
    <p>Player X: {state.scoreOfPlayer1} wins</p>
    <p>Player O: {state.scoreOfPlayer2} wins</p>
  </div>;
}
```

Since React is not the subject of this tutorial, we'll just leave it as it is.
I think the commentary above is sufficient to understand it with little previous React experience.
The functions `nextStep` and `renderGame` will be implemented in our pure module, which we can call `game.pure.js`.

```javascript
import React from 'react'
import Tile from './Tile'

const renderGame = board => handleClick =>
  <div className="game"></div>

const nextStep = tileId => (state, _) => state

export { renderGame, nextStep }
```

The `renderGame` function takes care of rendering the board according to its state given in `board` and a `handleClick` handler, which just calls `nextStep` when a tile is clicked and saves the new state into the model.
In comparison, `nextStep` takes the id of the clicked tile (which we will need to generate somehow) and the `state`, which contains the model we just went over (the `_` argument stands for the React props which are not needed here), and returns a new state with the clicked tile fixed to the player that clicked it.
We cannot change the model attributes *in any way*, they should be taken as constants -- we can only decide what we *return* in the functions.
We want to use *arrow expressions only* to implement these functions.
As will be soon evident, this is possible without any nasty hacking or tricks.

We first need a small functional component which will be used for Tile rendering. Let's call it just `Tile` and implement it as follows.

```javascript
import React from 'react';

export default function Tile(props) {
  return <button
          className={"Tile" + props.playerId}
          onClick={props.onClick}
        />;
}
```

From the model we know that the prop `playerId` can take three values:

- `0` denotes the tile hasn't been played by any player yet,
- `1` denotes Player X,
- `2` denotes Player O.

The `onClick` prop takes the handler for clicking the tile. Voila, in `renderGame` we are provided
with such a handler already in one of our arguments, namely `handleClick`.
If we look at its definition, it takes `tileId` to know what changes to do in the model, which we *here* in `renderGame` decide how to generate.
And since the game CSS handles for us that only 3 tiles fit in a row, we only need to render 9 tiles with 9 unique IDs and set according to who played them (no one, player X or O).

How to create several tiles when we cannot use any loops and don't want to copy-paste?
This is no problem at all because we have all the available tiles in the `board` array.
We can *map* each abstract tile onto its corresponding DOM tile.
The `map` function also provides the index of the tile in the array, which will serve ok as an ID for our purposes.
We will pass the index into the `handleClick` function.

```javascript
const renderGame = board => handleClick =>
  <div className="TicTacToe">{
    board.map((tilePlayedBy, tileIndex) =>
      <Tile
        playerId={tilePlayedBy}
        onClick={() => handleClick(tileIndex)}
        key={tileIndex} // for React purposes
      />)
  }</div>
```

The `Tile` component handles the styles for us, which makes the `renderGame` function finished.

## Managing state

Although the `renderGame` function seemed easy to do, `nextStep` will be trickier.
We can *decompose* the game into three main steps that are made in any particular moment.
This is perfect for using function decomposition not to get lost in the process.

1. Determining whose turn it is ~> `getIdOfPlayerOnTurn`.
2. Determining the new state if a tile with `tileId` is clicked ~> `newState`.
3. Determining if the game is finished *and* getting the winner id ~> `getWinnerId`.
4. Resetting the game and incrementing the score of the winner ~> `newGame`.

### `getIdOfPlayerOnTurn`
This one is probably the least plain to see.
Since we cannot pass any helper variable to tell us whose turn it is, we must only blindly believe in the insight of the blueprint's creator (me :-D) and assume that the data we have in hand tell us already.
This is indeed true: the board reveals how many turns we have so far played in the number of non-zero tiles.
And since there are two players, player X will play each *even* turn and player O each *odd* turn.

```javascript
const getNumberOfTurns = board => board.reduce(
  (count, tilePlayedBy) => count + Math.ceil(tilePlayedBy / 2),
  0
)

const getIdOfPlayerOnTurn = board => getNumberOfTurns(board) % 2 + 1
```

### `newState`
This function should take the id of the clicked tile and returning a *new state* with a board, where the given tile is marked with the player whose turn it was.
We *do not change* the original state in any way!
Since we cannot use any loops, we'll just map over the original board and return one where the clicked tile has the corresponding playerId if the tile hasn't been played yet.
The rest of the state remains unchanged.

```javascript
const newState = state => clickedTileId => playerOnTurn => ({
  ...state,
  board: state.board.map((_, tileIndex) =>
    tileIndex === clickedTileId && !state.board[tileIndex]
    ? playerOnTurn
    : state.board[tileIndex])
})
```

### `getWinnerId`
All of us have probably played 3x3 TicTacToe in some point of our lives.
We need three X's (O's) in a row (~> `getRow`), column (~> `getCol`), or a diagonal (~> `getDiag`).
If in one of these cases there are three identical playerIds to the playerId on turn, we have a winner with this id (`winnerRowOrColExists`).

We need only check if any row, column or a diagonal contains the `playerOnTurn`.
Because Javascript is dumb enough not to have any built-in array comparator, we'll create a simple checker `containsOnlyVal` to check if an array contains only a given value of a *trivial* data type.
Here the magic of curried functions will be made evident: using our IIFE trick, we create a local function called `isWinning`, which checks if an array contains only `playerOnTurn` values.
`isWinning` is nothing but a partially applied `containsOnlyVal`!

```javascript
const getWinnerId = board => playerOnTurn => (isWinning =>
  winnerRowOrColExists(board)(isWinning) ||
  winnerDiagonalExists(board)(isWinning)  ? playerOnTurn : 0
)(containsOnlyVal(playerOnTurn))

const containsOnlyVal = val => arr => arr.every(elem => elem === val)
```

For rows and columns, we'll go through each tile and check its corresponding row and column, if it satisfies `isWinning`.
I'm not really sure here if JS behaves lazily in the following uses of `reduce`.
Even if we were doing everything everything thrice, it doesn't matter in a 3x3 grid.

```javascript
const getRow = board => tileIndex => (firstOnRow => [
  board[firstOnRow], board[firstOnRow + 1], board[firstOnRow + 1 + 1]
])(3 * Math.floor(tileIndex / 3))

const getCol = board => tileIndex => (firstOnCol => [
  board[firstOnCol], board[firstOnCol + 3], board[firstOnCol + 3 + 3]
])(tileIndex % 3)

const winnerRowOrColExists = board => isWinning =>
  board.reduce((prev, _, tileIndex) => prev
    || isWinning(getRow(board)(tileIndex))
    || isWinning(getCol(board)(tileIndex)),
    false
  )
```

For diagonals, we just extract the corresponding tile values by hand.
If we had bigger grids, we could abstract this in a similar way as we abstracted getting a row or a col for a particular tile.
There would be some problems with boundaries though, hence the simple attitude in this case.

```javascript
const getDiag1 = board => [board[0], board[4], board[8]]

const getDiag2 = board => [board[2], board[4], board[6]]

const winnerDiagonalExists = board => isWinning =>
  isWinning(getDiag1(board)) || isWinning(getDiag2(board))
```

Although `getWinnerId` is a little more inefficient that it need be, for brevity
we will keep it as it is...
We are still only working with a 3x3 grid.
If we needed bigger grids, we could use several techniques (e.g. actually making sure the evaluation is lazy, checking only from the top left corners, winner configuration imprints...).

### `newGame`
We just need to return a reset game with incremented score of the winner.
This is straightforward:

```javascript
const newGame = state => winnerId => ({
  ...state,
  board: state.board.fill(0),
  [`scoreOfPlayer${winnerId}`]: state[`scoreOfPlayer${winnerId}`] + 1
})
```

### `nextStep`
Putting it all together, we are finally able to implement `nextStep` in a clear manner.
Each turn will look as follows.

1. If there is no winner, we return a new state with the clicked tile changed to the player on turn.
2. Otherwise start a new game.

By now it should be obvious that in order to achieve this, we need functions `newState`, `newGame` we've just implemented and pass them the arguments that they take.
It should also be obvious now why they take some arguments, which they would not really need (such as `playerOnTurn`): among others, to prevent recounting and simplify unit testing.

```javascript
const nextStep = tileId => (state, _) => (
  playerOnTurn => (
    newState => (
      winnerId => !winnerId
        ? newState
        : newGame(state)(winnerId)
      )(getWinnerId(newState.board)(playerOnTurn))
    )(newState(state)(tileId)(playerOnTurn))
  )(getIdOfPlayerOnTurn(state.board))
```

Notice how we need to count the newState *before* deciding whether to return it or the new game.
This is because all of the functions we've implemented actually count with the fact that a turn has just been made.

# Result

That's really all there is to it in terms of implementation.
Adding a couple of lines of CSS, the whole result can be seen running in the following snippet.

<div class="glitch-embed-wrap" style="height: 420px; width: 100%;"><iframe
    src="https://glitch.com/embed/#!/embed/pm-tictactoe?path=src/game.pure.js&previewSize=100"
    title="pm-tictactoe on Glitch"
    style="height: 100%; width: 100%; border: 0;"></iframe></div>

A seemingly easy function `nextStep` turned into quite a beast.
We wouldn't have made it without function decomposition, which is *essential*
not only in programming but especially in FP.
However, it was really worth it.
The entire `game.pure.js` is *without any side effects*.
Each one of its functions is easy to parallelize, unit test, type and much more.
Once the time comes, this will be covered in one of the next articles.
