# [Minesweeper](https://codingchallenges.substack.com/p/coding-challenge-65-minesweeper)

<details>
<summary>Challenge 65</summary>

### Coding Challenge #65 - Minesweeper

Level up as a software engineer by building a classic game.

### The Challenge - Building Your Own Minesweeper Game

The rules of Minesweeper are relatively simple. The board is divided into a
grid, with mines randomly distributed in the grid cells. To win, you need to
open all the cells that you can without triggering a mine. You mark off where
the mines are.

The number on a cell shows the number of cells adjacent to it that contain
mines. Using this information, you can determine if a cell is safe or if it
contains a mine. Cells believed to contain a mine can be marked with a flag.

<div align="center">
<img alt="classic board" src="docs/imgs/the-classic-board.png" width="50%"/>
</div>

#### Step Zero

In this step, pick the programming language and development environment you’re
going to use. Consider trying something different - this would be a great
project to try a front-end stack if you’re a back-end developer and vice versa.

If you’re from a data engineering or site-reliability engineering background
you could leverage your knowledge of Python with PyGame or Go with Ebitenegine.
Rustaceans can check out [are we game yet](https://substack.com/redirect/57393075-698a-41a4-96c4-b43a50b881f3?j=eyJ1IjoiM2kweHp3In0.eqoqIq9l1pkHHeZL7RZc_Dhwm19HgvanBSy9MG77Yzk) for useful crates.

Minesweeper is relatively simple and as such, it’s a great platform for
learning a new technology, or programming language.

You could create your own graphics for the game or there are some on
opengameart.org that you can use.

#### Step 1

In this step your goal is to draw the grid for the initial game state. It
should look something like this:

<div align="center">
<img alt="initial board" src="docs/imgs/initial-board.png" width="50%"/>
</div>

The left hand number - 99 here - shows the number of mines left to find and the
right hand number is the time in seconds since starting the game.

#### Step 2

In this step your goal is to reveal the mine / safe state after a cell is
clicked on. That should look something like this:

<div align="center">
<img alt="first cell" src="docs/imgs/first-cell.png" width="50%"/>
</div>

#### Step 3

In this step your goal is to detect hitting a mine, reveal the mines not found
and offer to play again. For example:

<div align="center">
<img alt="kaboom" src="docs/imgs/kaboom.png" width="50%"/>
</div>

Opting to play again should re-start the game.

#### Step 4

In this step your goal is to detect a win. A win is when all the mines are
found. For example:

<div align="center">
<img alt="winning" src="docs/imgs/winning.png" width="25%"/>
</div>

#### Going Further

You can take this further by offering different size playing areas, the bigger
the area the harder the game. You could also add a league table so people can
record their best scores.
</details>

## running the game

- docker run
- `docker build -t my-app .`
- `podman machine init`
- `podman machine start`
- `podman build -t minesweeper .`
- `podman run --name minesweeper -i minesweeper:latest`

## Layout
- the board:
    - an M x N matrix of cells
    - cell types bomb, empty, count of neighboring bombs
    - cells have presentation: covered, uncovered, flagged (as a bomb)
    - a game time in seconds or minutes:seconds
    - an uncovered bomb counter - counts down as bomb flags are planted
- state enhancements:
    - a private map of cell -> (valid / legal) neighboring cells
    -
- the rules:
    - player can uncover a covered cell
    - play can mark / unmark a cell with a flag
    - player uncovering an empty cell triggers uncovering neighboring count cells

## Project files structure
- board/
    - mod.rs -- the board struct and exposer of other modules
    - Difficulty.rs -- difficulty settings
    - cells
- docs/
    - imgs/ -- images used in the docs
- game_rules/
    - bomb_count.rs
    - select_bomb_locations.rs
    - process_move
    - exspose.rs neighboring cells
- main.rs
- README.md -- this file
-


https://c.r74n.com/emoji#Nature
🚩
🪀
☠
☹
0️⃣ 1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 7️⃣ 8️⃣ 9️⃣ 🔟
0️⃣
