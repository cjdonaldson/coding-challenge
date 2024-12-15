# Name
blurp

<!-- <details> -->
<!-- <summary>Challenge ??</summary> -->

### Coding Challenge #?? - Name

Level up as a software engineer by building a classic game.

### The Challenge - Building Your Own ???

The rules of Connect Four are relatively simple. The board is divided into a grid.
To win, you need to drop a token into one of the columns such that you can connect
four tokens vertically, horizontally, or diagonally. Your token can be placed on
top of your opponents.

<div align="center">
<img alt="classic board" src="docs/imgs/ConnectFour.jpeg" width="50%"/>
</div>

#### Step Zero

In this step, pick the programming language and development environment you’re
going to use. Consider trying something different - this would be a great
project to try a front-end stack if you’re a back-end developer and vice versa.

If you’re from a data engineering or site-reliability engineering background
you could leverage your knowledge of Python with PyGame or Go with Ebitenegine.
Rustaceans can check out [are we game yet](https://substack.com/redirect/57393075-698a-41a4-96c4-b43a50b881f3?j=eyJ1IjoiM2kweHp3In0.eqoqIq9l1pkHHeZL7RZc_Dhwm19HgvanBSy9MG77Yzk) for useful crates.

Connect Four is relatively simple and as such, it’s a great platform for
learning a new technology, or programming language.

You could create your own graphics for the game or there are some on
opengameart.org that you can use.

#### Step 1

In this step your goal is to draw the grid for the initial game state. It
should look something like the above image.

<div align="center">
<img alt="classic board" src="docs/imgs/ConnectFour_board-only.jpeg" width="50%"/>
</div>

#### Step 2

In this step your goal is to accept a column index to drop a players token into
presenting that token in the next open slot from the bottom.

#### Step 3

In this step your goal is to detect a winning sequence of four similar tokens in
one of the following directions: vertical, horizontal, or diagonal.

#### Going Further

You can take this further by offering different size playing areas, the bigger
the area the harder the game. You could also add a league table so people can
record their best scores.
<!-- </details> -->

<hr/>

My discussion and notes:

## running the game

- docker run
- `docker build -t my-app .`
- `podman machine init`
- `podman machine start`
- `podman build -t connectfour .`
- `podman run --name connectfour -i connectfour:latest`

## Layout
- the board:
    - an M x N matrix of cells
    - cell types empty, player one's token, player two's token
    - cells have presentation: blank or a players token
- state enhancements:
    -
- the rules:
    - player can indicate the column (file) to drop their token

## Project files structure
- board/
    - mod.rs -- the board struct and exposer of other modules
      - cell
      - player
      -
- docs/
    - imgs/ -- images used in the docs
- game_rules/
    -
- main.rs
- README.md -- this file
