---
title: Pure ES6 Modules in Action
author: Adam Fiedler
teaser: "In an <a href='/blog/2019-08-15-pure-modules'>earlier post</a> I wrote
about what I imagine under pure modules and listed reasons why one should write 
as much JS code as possible in this paradigm. 
Then I described some techniques how to get around potential pitfalls. 
One thing is theory, another is practice. 
Let's look at one particular example of using pure modules to demonstrate their 
feasibility and advantages."
---

TicTacToe has been a famous game known for centuries and incindentally, also used in the official [React tutorial](https://reactjs.org/tutorial/tutorial.html).
We can surely revisit the tutorial and rethink it to show how separate game logic into a *pure* module away from event handling/rendering.
Once there, we will have all of the advantages mentioned in the article on pure modules.
And that not only through separation of concerns: the pureness of the module will allow us to easily reason about the module, unit test it and parallelize it (as will hopefully be shown in a follow-up article).

# Implementation

How exactly can we implement the logic of TicTacToe in a pure module?
We should first start thinking about the *model*, or more explicitly, the state of the game.
It does not have much to do with pure modules per se, but it will greatly influence how our resulting code will look.
This state will be maintained by a React component (why not).
For our simple purposes, the React component will handle the state directly.
That does not mean it couldn't be extracted to a Redux container or something similar.
Our state will not be so different from the one in the React tutorial, besides for my personal preference of saving the played tiles with numbers:

```{.javascript .numberLines .line-anchors startFrom="1"}
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
We will pass the model attributes from our React component to the functions that we will be implementing in our pure module.
The idea is that our React component will *only* handle events and re-rendering.
In a way, it will serve as an integrated Redux reducer with the mouse clicking as our action.
After getting a click on a tile, it will set a new state based on the previous state.
That's it.
The calculation of the new state will be treated in our pure module.

Our main component that will be 

```{.javascript .numberLines .line-anchors startFrom="1"}
import { Component } from 'react'
import { nextStep, renderGame } from './game.pure'

export default class TicTacToe extends Component {
  constructor() {
    super();

    this.state = {
      board: [
        0, 0, 0, 
        0, 0, 0, 
        0, 0, 0
      ],
      scoreOfPlayer1: 0,
      scoreOfPlayer2: 0
    };

    this.onClick = this.onClick.bind(this);
  }

  onClick(tileId) {
    this.setState(nextStep(tileId));
  }

  render() {
    return renderGame(this.state.board)(this.onClick);
  }
}
```

We will also need a small functional component for the tiles. Let's call it `Tile.js` and it can look like this.

```{.javascript .numberLines .line-anchors startFrom="1"}
import React from 'react';

export default function Tile(props) {
  return <button className={"Tile" + props.playerId} onClick={props.onClick} />;
}
```

The functions `nextStep` and `render` will be implemented in our pure module, which we can call `game.pure.js`.

```{.javascript .numberLines .line-anchors startFrom="1"}
import React from 'react'
import Tile from './Tile'

const renderGame = board => handleClick => 
  <div className="TicTacToe"></div>
 
const nextStep = tileId => (state, _) => state

export { renderGame, nextStep }
```

The `render` function takes care of rendering the board according to its state given in `board` and a `handleClick` handler, which just calls `nextStep` when a tile is clicked and saves the new state into the model.
In comparison, `nextStep` takes the id of the clicked tile (which we will need to generate somehow) and the `state`, which contains the model we just went over (the `_` argument stands for the React props which are not needed here), and returns a new state with the clicked tile fixed to the player that clicked it.
We cannot change the model attributes *in any way*, they should be taken as constants -- we can only decide what we *return* in the functions.
We want to use *arrow expressions only* to implement these functions.
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

The `Tile` component handles the styles for us, which makes the `render` function finished.

## Managing state

Although the `render` function seemed easy to do, `nextStep` will be trickier.
We can *decompose* the game into three main steps that are made in any particular moment. 
This is perfect for using function decomposition not to get lost in the process.

1. Determining whose turn it is ~> `getIdOfPlayerOnTurn`
2. Determining the game is finished and getting the winner id ~> `getWinnerId`
3. Determining the new state if a tile with `tileId` is clicked ~> `newState`
4. Resetting the game and incrementing the score of the winner ~> `newGame`

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
  board.reduce((_, __, tileIndex) => 
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

### `newState`
This function should take the id of the clicked tile and returning a *new state* with a board, where the given tile is marked with the player whose turn it was.
We *do not change* the original state in any way!
Since we cannot use any loops, we'll just map over the original board and return one where the clicked tile has the assigned playerId.
The rest of the state remains unchanged.

```{.javascript .numberLines .line-anchors startFrom="1"}
const newState = state => clickedTileId => playerOnTurn => ({
  ...state,
  board: state.board.map((_, tileIndex) => 
    tileIndex === clickedTileId 
    ? playerOnTurn
    : state.board[clickedTileId])
})
```

### `newGame`
We just need to return a reset game with incremented score of the winner.
This is straightforward:

```{.javascript .numberLines .line-anchors startFrom="1"}
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
It should also be obvious now why they take some arguments, which they would not really need (such as `playerOnTurn`): among others, to prevent recounting and simplifying unit testing.

```{.javascript .numberLines .line-anchors startFrom="1"}
const nextStep = tileId => (state, _) => (
  playerOnTurn => (
    winnerId => !winnerId 
      ? newState(state, tileId, playerOnTurn) 
      : newGame(state, winnerId)
    )(getWinnerId(state.board, playerOnTurn))
  )(getIdOfPlayerOnTurn(state.board))
```

That's really all there is to it in terms of implementation.
Notice how a seemingly easy function turned into quite a beast.
We wouldn't have made it without function decomposition, which is *essential*
not only in programming but especially in FP.
The whole result can be seen running in the following snippet.