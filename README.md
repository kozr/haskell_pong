# haskell_pong

Pong is a classic arcade game that has been around since the early 1970s. It is a two-player game in which each player controls a paddle on the opposite side of the screen. The goal of the game is to hit the ball back and forth across the screen, and each player must prevent the other from scoring a point by hitting the ball with their paddle. This code implements an AI opponent instead of a second player.  

## How to run
```Haskell
ghci
:l main
main
```
### Troubleshooting
If `Exception: user error (unknown GLUT entry glutInit)` is thrown, try copying `Main.hs` file to the root directory and running the above command again. 
If it still does not work, you might have to install freeglut (or adding `freeglut.dll` and `freeglut.lib` files to the project's root directory).
