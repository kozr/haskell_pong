module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

type Radius = Float

type Position = (Float, Float)

data GameState = Game
  { ballPosition :: Position,
    ballVelocity :: (Float, Float),
    paddle1Position :: Position,
    paddle2Position :: Position,
    paddlesCooldown :: Int,
    player1Score :: Int,
    player2Score :: Int
  }
  deriving (Show)

-- Window settings
windowWidth, windowHeight :: Int
windowWidth = 300
windowHeight = 300

windowOffsetX, windowOffsetY :: Int
windowOffsetX = 10
windowOffsetY = 0

windowMiddle :: Float
windowMiddle = fromIntegral windowWidth / 2

windowBottom :: Float
windowBottom = - fromIntegral windowHeight / 2

windowTop :: Float
windowTop = fromIntegral windowHeight / 2

window :: Display
window = InWindow "Pong" (windowWidth, windowHeight) (windowOffsetX, windowOffsetY)

-- Drawings settings

background :: Color
background = black

playerOffsetX :: Float
playerOffsetX = windowMiddle - (fromIntegral $ windowOffsetX * 2)

paddleWidth :: Float
paddleWidth = 10

paddleHeight :: Float
paddleHeight = 50

ballRadius :: Radius
ballRadius = 5

-- Game settings

angleSensitivity :: Float
angleSensitivity = 25

ballSpeed :: Float
ballSpeed = 50

initialState :: GameState
initialState =
  Game
    { ballPosition = (0, 0),
      ballVelocity = (ballSpeed, 0),
      paddle1Position = (- playerOffsetX, 0),
      paddle2Position = (playerOffsetX, 0),
      paddlesCooldown = 0,
      player1Score = 0,
      player2Score = 0
    }

render :: GameState -> Picture
render game =
  pictures
    [ ball,
      net,
      makePaddle $ paddle1Position game,
      makePaddle $ paddle2Position game
    ]
  where
    ball = uncurry translate (ballPosition game) $ color ballColor $ circleSolid 5
    ballColor = white

    -- like ping pong net
    net :: Picture
    net =
      translate 0 0 $
        color white $
          rectangleSolid ballRadius (fromIntegral windowHeight)

    makePaddle :: Position -> Picture
    makePaddle (x, y) = pictures
      [ translate x y $ color white $ rectangleSolid paddleWidth paddleHeight ]

-- Functions
moveBall :: Float -> GameState -> GameState
moveBall seconds game = game {ballPosition = (x', y')}
  where
    (x, y) = ballPosition game
    (vx, vy) = ballVelocity game
    x' = x + vx * seconds
    y' = y + vy * seconds

-- Game engine
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

update :: Float -> GameState -> GameState
update seconds = wallBounce . paddleBounce . moveBall seconds

-- Game logic
paddleBounce :: GameState -> GameState
paddleBounce game = game {ballVelocity = (vx', vy'), paddlesCooldown = cooldown }
  where
    (vx, vy) = ballVelocity game
    cooldown = if paddleCollision game then 5 else max 0 (paddlesCooldown game - 1) -- To prevent the ball from bouncing off the paddle multiple times
    vx' = if paddleCollision game && paddlesCooldown game == 0 then (- vx) else vx
    vy' = if paddleCollision game then calculateBounceY game else vy

calculateBounceY :: GameState -> Float
calculateBounceY game = vy'
  where
    (_, vy) = ballVelocity game
    -- calculate the new velocity based on the position of the ball and where it hit the paddle
    distAwayFromMiddle = snd (ballPosition game) - snd (paddle1Position game)
    vy' = distAwayFromMiddle / (paddleHeight / 2) * (-angleSensitivity)

paddleCollision :: GameState -> Bool
paddleCollision game = leftCollision || rightCollision
  where
    (x, y) = ballPosition game
    (p1x, p1y) = paddle1Position game
    (p2x, p2y) = paddle2Position game
    withinPaddle1 = (y + ballRadius) <= p1y + paddleHeight / 2 && (y - ballRadius) >= p1y - paddleHeight / 2
    withinPaddle2 = (y + ballRadius) <= p2y + paddleHeight / 2 && (y - ballRadius) >= p2y - paddleHeight / 2
    withinRange1 = (x - ballRadius) <= p1x && (x - ballRadius) >= p1x - paddleWidth
    withinRange2 = (x + ballRadius) >= p2x && (x + ballRadius) <= p2x + paddleWidth
    leftCollision = withinRange1 && withinPaddle1
    rightCollision = withinRange2 && withinPaddle2

wallBounce :: GameState -> GameState
wallBounce game = game {ballVelocity = (vx, vy')}
  where
    (vx, vy) = ballVelocity game
    vy' = if wallCollision (ballPosition game) then (- vy) else vy

wallCollision :: Position -> Bool
wallCollision (_, y) = topCollision || bottomCollision
  where
    topCollision = (y + ballRadius) >= windowTop
    bottomCollision = (y - ballRadius) <= windowBottom

-- Event handling
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char 'w') Down _ _) game = game {paddle1Position = (fst(paddle1Position game), min (windowTop - paddleHeight / 2) (snd(paddle1Position game) + 10))}
handleKeys (EventKey (Char 's') Down _ _) game = game {paddle1Position = (fst(paddle1Position game), max (windowBottom + paddleHeight / 2) (snd(paddle1Position game) - 10))}
handleKeys (EventKey (Char 'o') Down _ _) game = game {paddle2Position = (fst(paddle2Position game), min (windowTop - paddleHeight / 2) (snd(paddle2Position game) + 10))}
handleKeys (EventKey (Char 'l') Down _ _) game = game {paddle2Position = (fst(paddle2Position game), max (windowBottom + paddleHeight / 2) (snd(paddle2Position game) - 10))}
handleKeys (EventKey (Char 'r') Down _ _) game = game {ballPosition = (0, 0)}
handleKeys (EventKey (Char 't') Down _ _) game = game {ballVelocity = (- fst (ballVelocity game), snd (ballVelocity game))}
handleKeys _ game = game