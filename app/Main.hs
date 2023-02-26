module Main where

import System.Exit     ( exitSuccess )
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

type Radius = Float

type Position = (Float, Float)

data PaddleState = Still | MovingUp | MovingDown
  deriving (Show, Eq)

data GameState = Game
  { ballPosition :: Position,
    ballVelocity :: (Float, Float),
    paddle1Position :: Position,
    paddle2Position :: Position,
    paddlesCooldown :: Int,
    player1Score :: Int,
    player2Score :: Int,
    paddle1State :: PaddleState,
    paddle2State :: PaddleState
  }
  deriving (Show)

-- Window settings
scaleFactor :: Float
scaleFactor = 3

windowWidth, windowHeight :: Int
windowWidth = 300 * round scaleFactor
windowHeight = 300 * round scaleFactor

windowOffsetX, windowOffsetY :: Int
windowOffsetX = 10 * round scaleFactor
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
paddleWidth = 10 * scaleFactor

paddleHeight :: Float
paddleHeight = 50 * scaleFactor

paddleTolerance :: Float
paddleTolerance = 10 * scaleFactor

ballRadius :: Radius
ballRadius = 3 * scaleFactor

-- Game settings

angleSensitivity :: Float
angleSensitivity = 25 * scaleFactor

ballSpeed :: Float
ballSpeed = 50 * scaleFactor

initialState :: GameState
initialState =
  Game
    { ballPosition = (0, 0),
      ballVelocity = (ballSpeed, 0),
      paddle1Position = (- playerOffsetX, 0),
      paddle2Position = (playerOffsetX, 0),
      paddlesCooldown = 0,
      player1Score = 0,
      player2Score = 0,
      paddle1State = Still,
      paddle2State = Still
    }

render :: GameState -> Picture
render game =
  pictures
    [ ball,
      net,
      scoreCounter,
      makePaddle $ paddle1Position game,
      makePaddle $ paddle2Position game
    ]
  where
    ball = uncurry translate (ballPosition game) $ color ballColor $ circleSolid ballRadius
    ballColor = white

    -- like ping pong net
    net :: Picture
    net =
      translate 0 0 $
        color white $
          rectangleSolid (ballRadius / 2) (fromIntegral windowHeight)

    -- render a paddle
    makePaddle :: Position -> Picture
    makePaddle (x, y) =
      pictures
        [translate x y $ color white $ rectangleSolid paddleWidth paddleHeight]

    -- render the score counter
    scoreCounter :: Picture
    scoreCounter = translate (- windowMiddle / 2) 0 $ color white $ text $ show (player1Score game) ++ " - " ++ show (player2Score game)

-- Game engine
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

update :: Float -> GameState -> GameState
update seconds = endzoneHandler . wallBounce . paddleBounce . movePaddle . moveBall seconds

-- Game logic

moveBall :: Float -> GameState -> GameState
moveBall seconds game = game {ballPosition = (x', y')}
  where
    (x, y) = ballPosition game
    (vx, vy) = ballVelocity game
    x' = x + vx * seconds
    y' = y + vy * seconds

movePaddle :: GameState -> GameState
movePaddle game = game {paddle1Position = paddle1Position', paddle2Position = paddle2Position'}
  where
    paddle1Position'
      | paddle1State game == MovingUp     = (fst (paddle1Position game), min (windowTop - paddleHeight / 2) (snd (paddle1Position game) + 2))
      | paddle1State game == MovingDown   = (fst (paddle1Position game), max (windowBottom + paddleHeight / 2) (snd (paddle1Position game) - 2))
      | otherwise                         = paddle1Position game
    paddle2Position' 
      | paddle2State game == MovingUp     = (fst (paddle2Position game), min (windowTop - paddleHeight / 2) (snd (paddle2Position game) + 2))
      | paddle2State game == MovingDown   = (fst (paddle2Position game), max (windowBottom + paddleHeight / 2) (snd (paddle2Position game) - 2))
      | otherwise                         = paddle2Position game

paddleBounce :: GameState -> GameState
paddleBounce game = game {ballVelocity = (vx', vy'), paddlesCooldown = cooldown}
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
    distAwayFromMiddle =
      if (fst $ ballPosition game) > 0
        then snd (ballPosition game) - snd (paddle2Position game)
        else snd (ballPosition game) - snd (paddle1Position game)
    vy' = distAwayFromMiddle / (paddleHeight / 2) * angleSensitivity

paddleCollision :: GameState -> Bool
paddleCollision game = leftCollision || rightCollision
  where
    (x, y) = ballPosition game
    (p1x, p1y) = paddle1Position game
    (p2x, p2y) = paddle2Position game
    withinPaddleY1 = (y + ballRadius) <= p1y + paddleHeight / 2 + paddleTolerance && (y - ballRadius) >= p1y - paddleHeight / 2 - paddleTolerance
    withinPaddleY2 = (y + ballRadius) <= p2y + paddleHeight / 2 + paddleTolerance && (y - ballRadius) >= p2y - paddleHeight / 2 - paddleTolerance
    withinXRange1 = (x - ballRadius) <= p1x && (x - ballRadius) >= p1x - paddleWidth
    withinXRange2 = (x + ballRadius) >= p2x && (x + ballRadius) <= p2x + paddleWidth
    leftCollision = withinXRange1 && withinPaddleY1
    rightCollision = withinXRange2 && withinPaddleY2

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

endzoneHandler :: GameState -> GameState
endzoneHandler game = newGame
  where
    (x, _) = ballPosition game
    player1Score' = if endzoneCollision game then player1Score game + 1 else player1Score game
    player2Score' = if endzoneCollision game then player2Score game + 1 else player2Score game
    newGame = if endzoneCollision game then initialState {player1Score = player1Score', player2Score = player2Score'} else game

endzoneCollision :: GameState -> Bool
endzoneCollision game = leftCollision || rightCollision
  where
    (x, _) = ballPosition game
    leftCollision = (x - ballRadius) <= (- windowMiddle - fromIntegral windowOffsetX)
    rightCollision = (x + ballRadius) >= windowMiddle + fromIntegral windowOffsetX

-- Event handling
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char 'w') Down _ _) game = game {paddle1State = MovingUp}
handleKeys (EventKey (Char 'w') Up _ _) game = game {paddle1State = Still}
handleKeys (EventKey (Char 's') Down _ _) game = game {paddle1State = MovingDown}
handleKeys (EventKey (Char 's') Up _ _) game = game {paddle1State = Still}
handleKeys (EventKey (Char 'o') Down _ _) game = game {paddle2State = MovingUp}
handleKeys (EventKey (Char 'o') Up _ _) game = game {paddle2State = Still}
handleKeys (EventKey (Char 'l') Down _ _) game = game {paddle2State = MovingDown}
handleKeys (EventKey (Char 'l') Up _ _) game = game {paddle2State = Still}
handleKeys (EventKey (Char 'r') Down _ _) game = game {ballPosition = (0, 0)}
handleKeys (EventKey (Char 't') Down _ _) game = game {ballVelocity = (- fst (ballVelocity game), snd (ballVelocity game))}
handleKeys _ game = game