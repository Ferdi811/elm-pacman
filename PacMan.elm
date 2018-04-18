module Pacman exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Keyboard
import Time
import Random


-- Model

type Direction = Left | Right | Top | Down

type EnemyMode = Chase | Scatter | Frightened | Rnd

type LevelElement = Wall | Floor | Dot | Pill | Pac Direction Int | Clyde Direction LevelElement EnemyMode | Inky Direction LevelElement EnemyMode | Pinky Direction LevelElement EnemyMode | Blinky Direction LevelElement EnemyMode

type alias Model =
    { score: Int
    , levelData: List (List LevelElement)
    , lastKeyPressed: Direction
    , maxScore: Int
    , seed: Random.Seed
    }


init:  (Model, Cmd Msg)
init =
  update GenMap (genLevel 1337)


genRndInt: Model -> Int -> Int -> (Int, Model)
genRndInt model min max =
  let
    (rndInt, newSeed) = Random.step (Random.int min max) model.seed
  in
    (rndInt, {model | seed = newSeed})

genLevel: Int -> Model
genLevel seed =
  let
    mapWidth = 28 // 2

    mapHeight = 32

    growWallDendrid: Int -> Model -> Int -> Int -> Model
    growWallDendrid seed model row col =
      let
        wallDendridMaxLenght: Model -> Int -> Int -> Int -> Direction -> Int
        wallDendridMaxLenght model acc row col dir =
          let
            isWayFree: Model -> Int -> Int -> Direction -> Bool
            isWayFree model row col dir =
              case dir of
                Top ->
                  case ((getElement model row (col - 1)), (getElement model row col), (getElement model row (col + 1))) of
                    (Just(Dot), Just(Dot), Just(Dot)) -> True
                    _ -> False
                Down ->
                  case ((getElement model row (col - 1)), (getElement model row col), (getElement model row (col + 1))) of
                    (Just(Dot), Just(Dot), Just(Dot)) -> True
                    _ -> False
                Left ->
                  case ((getElement model (row - 1) col), (getElement model row col), (getElement model (row + 1) col)) of
                    (Just(Dot), Just(Dot), Just(Dot)) -> True
                    _ -> False
                Right ->
                  case ((getElement model (row - 1) col), (getElement model row col), (getElement model (row + 1) col)) of
                    (Just(Dot), Just(Dot), Just(Dot)) -> True
                    _ -> False

            delRow = case dir of
              Top -> -1
              Down -> 1
              _ -> 0
            delCol = case dir of
              Left -> -1
              Right -> 1
              _ -> 0
            nextRow = row + delRow
            nextCol = col + delCol
            isOkay = isWayFree model nextRow nextCol dir
          in
            if isOkay then
              wallDendridMaxLenght model (acc + 1) nextRow nextCol dir
            else
              Basics.max 0 (acc - 1)

        wallBuilder: Int -> Int -> Int -> Int -> Direction -> Model -> Model
        wallBuilder acc maxDepth row col dir model =
          if acc == maxDepth then
            model
          else
            let
              delRow = case dir of
                Top -> -1
                Down -> 1
                _ -> 0
              delCol = case dir of
                Left -> -1
                Right -> 1
                _ -> 0
              nextRow = row + delRow
              nextCol = col + delCol
            in
              wallBuilder (acc + 1) maxDepth nextRow nextCol dir (updateLevel model row col Wall)


        (rnd1, seed1) = Random.step (Random.bool) (Random.initialSeed seed)

        (topCap, leftCap) = case rnd1 of
          True -> (5, 3)
          _ -> (3, 5)

        leftGrowDepth = Basics.min (leftCap) (wallDendridMaxLenght model 0 row col Left)
        rightGrowDepth = Basics.min (leftCap) (wallDendridMaxLenght model 0 row col Right)
        topGrowDepth = Basics.min (topCap) (wallDendridMaxLenght model 0 row col Top)
        downGrowDepth = Basics.min (topCap) (wallDendridMaxLenght model 0 row col Down)

        step1 = wallBuilder 0 leftGrowDepth row col Left model
        step2 = wallBuilder 0 rightGrowDepth row col Right step1
        step3 = wallBuilder 0 topGrowDepth row col Top step2
        step4 = wallBuilder 0 downGrowDepth row col Down step3

      in
        case ((getElement model (row-1) (col-1)), (getElement model (row-1) (col)), (getElement model (row-1) (col+1)), (getElement model (row) (col-1)), (getElement model (row) (col)), (getElement model (row) (col+1)), (getElement model (row+1) (col-1)), (getElement model (row+1) (col)), (getElement model (row+1) (col-1))) of
        (Just(Dot), Just(Dot), Just(Dot), Just(Dot), Just(Dot), Just(Dot), Just(Dot), Just(Dot), Just(Dot)) -> step4
        _ -> model

    -- To generate the outer walls
    genTopBottomWalls: Int -> Int -> Direction -> Model -> Model
    genTopBottomWalls colIdx seed dir model =
      if colIdx == mapWidth then
        model
      else
        let
          (row, sgn) = case dir of
            Top -> (Just(0), 1)
            Down -> (Just(mapHeight - 1), -1)
            _ -> (Nothing, 0)
          (isSideWall, seed1) = Random.step (Random.float 0.0 1.0) (Random.initialSeed seed)
          (sideWallDepth, seed2) = Random.step (Random.int 0 3) seed1
          (newSeed, seed3) = Random.step (Random.int Random.minInt Random.maxInt) seed2
        in
          case row of
            Just(rowIdx) ->
              if isSideWall <= 0.25 then
                genTopBottomWalls (colIdx + 1) newSeed dir (model |> (\mod -> updateLevel mod rowIdx colIdx Wall)
                        |> (\mod -> updateLevel mod (rowIdx + sgn * (Basics.min 1 sideWallDepth)) colIdx Wall)
                        |> (\mod -> updateLevel mod (rowIdx + sgn * (Basics.min 2 sideWallDepth)) colIdx Wall)
                        |> (\mod -> updateLevel mod (rowIdx + sgn * (Basics.min 3 sideWallDepth)) colIdx Wall))
              else
                genTopBottomWalls (colIdx + 1) newSeed dir (updateLevel model rowIdx colIdx Wall)
            _ -> model

    genLeftRightWalls: Int -> Int -> Direction -> Model -> Model
    genLeftRightWalls rowIdx seed dir model =
      if rowIdx == mapHeight then
        model
      else if rowIdx <= 5 then
        genLeftRightWalls (rowIdx + 1) seed dir (updateLevel model rowIdx 0 Wall)
      else if mapHeight - rowIdx <= 5 then
        genLeftRightWalls (rowIdx + 1) seed dir (updateLevel model rowIdx 0 Wall)
      else
        let
          (col, sgn) = case dir of
            Left -> (Just(0), 1)
            Right -> (Just(mapWidth - 1), -1)
            _ -> (Nothing, 0)
          (isSideWall, seed1) = Random.step (Random.float 0.0 1.0) (Random.initialSeed seed)
          (sideWallDepth, seed2) = Random.step (Random.int 0 3) seed1
          (newSeed, seed3) = Random.step (Random.int Random.minInt Random.maxInt) seed2
        in
          case col of
            Just(colIdx) ->
              if isSideWall <= 0.25 then
                genLeftRightWalls (rowIdx + 1) newSeed dir (model |> (\mod -> updateLevel mod rowIdx colIdx Wall)
                        |> (\mod -> updateLevel mod rowIdx (colIdx + sgn * (Basics.min 1 sideWallDepth)) Wall)
                        |> (\mod -> updateLevel mod rowIdx (colIdx + sgn * (Basics.min 2 sideWallDepth)) Wall)
                        |> (\mod -> updateLevel mod rowIdx (colIdx + sgn * (Basics.min 3 sideWallDepth)) Wall))
              else
                genLeftRightWalls (rowIdx + 1) newSeed dir (updateLevel model rowIdx colIdx Wall)
            _ -> model

    -- To generate some random inner walls
    genRandomInnerWalls: Int -> Int -> Int -> Model -> Model
    genRandomInnerWalls acc maxInnerWalls seed model =
      if acc == maxInnerWalls then
        model
      else
        let
          (row, seed1) = Random.step (Random.int 0 mapHeight) (Random.initialSeed seed)
          (col, seed2) = Random.step (Random.int 0 mapWidth) seed1
          (newSeed, seed3) = Random.step (Random.int 0 100) seed2
        in
          genRandomInnerWalls (acc + 1) maxInnerWalls newSeed (growWallDendrid newSeed model row col)

    genRegularInnerWalls: Int -> Int -> Int -> Model -> Model
    genRegularInnerWalls row col seed model =
      let
        (newSeed, _) = Random.step (Random.int Random.minInt Random.maxInt) (Random.initialSeed seed)
      in
        if row >= mapHeight then
          model
        else
          if col >= mapWidth then
            genRegularInnerWalls (row + 1) 0 newSeed model
          else
            genRegularInnerWalls row (col + 1) newSeed (growWallDendrid newSeed model row col)

    mirrorLevel: Model -> Int -> Model
    mirrorLevel model rowIdx =
      if rowIdx > mapHeight then
        model
      else
        let
          rowUpdater: Int -> List LevelElement -> Int -> List LevelElement -> List LevelElement
          rowUpdater replaceIdx replaceElt colIdx elt =
            if colIdx == replaceIdx then
              replaceElt
            else
              elt
          currRow = Maybe.withDefault [] (List.head (List.drop rowIdx model.levelData))
          newRow = (List.take (mapWidth - 3) currRow) ++ (List.drop 3 (List.reverse currRow))
          newLevelData = List.indexedMap (rowUpdater rowIdx newRow) model.levelData
        in
         mirrorLevel {model | levelData = newLevelData} (rowIdx + 1)

    genGhostHouse: Model -> Model
    genGhostHouse model =
      let
        rowStart = (mapHeight // 2) - 4
        colStart = mapWidth - 4
      in
        model
        |> \modl -> updateLevel modl rowStart (colStart - 0) Floor
        |> \modl -> updateLevel modl rowStart (colStart - 1) Floor
        |> \modl -> updateLevel modl rowStart (colStart - 2) Floor
        |> \modl -> updateLevel modl rowStart (colStart - 3) Floor
        |> \modl -> updateLevel modl (rowStart + 1) (colStart - 0) Floor
        |> \modl -> updateLevel modl (rowStart + 1) (colStart - 1) Wall
        |> \modl -> updateLevel modl (rowStart + 1) (colStart - 2) Wall
        |> \modl -> updateLevel modl (rowStart + 1) (colStart - 3) Floor
        |> \modl -> updateLevel modl (rowStart + 2) (colStart - 0) (Floor)
        |> \modl -> updateLevel modl (rowStart + 2) (colStart - 1) (Floor)
        |> \modl -> updateLevel modl (rowStart + 2) (colStart - 2) Wall
        |> \modl -> updateLevel modl (rowStart + 2) (colStart - 3) Floor
        |> \modl -> updateLevel modl (rowStart + 3) (colStart - 0) Wall
        |> \modl -> updateLevel modl (rowStart + 3) (colStart - 1) Wall
        |> \modl -> updateLevel modl (rowStart + 3) (colStart - 2) Wall
        |> \modl -> updateLevel modl (rowStart + 3) (colStart - 3) Floor
        |> \modl -> updateLevel modl (rowStart + 4) (colStart - 0) Floor
        |> \modl -> updateLevel modl (rowStart + 4) (colStart - 1) Floor
        |> \modl -> updateLevel modl (rowStart + 4) (colStart - 2) Floor
        |> \modl -> updateLevel modl (rowStart + 4) (colStart - 3) Floor

    genEnemySpawnArea: Model -> Model
    genEnemySpawnArea model =
      let
        rowStart = (mapHeight // 2) - 4
        colStart = mapWidth - 4
      in
        model
        |> \modl -> updateLevel modl (rowStart + 2) (colStart - 0) (Clyde Top Floor Rnd)
        |> \modl -> updateLevel modl (rowStart + 2) (colStart - 1) (Pinky Top Floor Rnd)
        |> \modl -> updateLevel modl (rowStart + 2) (colStart + 1) (Blinky Top Floor Rnd)
        |> \modl -> updateLevel modl (rowStart + 2) (colStart + 2) (Inky Top Floor Rnd)

    genPacSpawnArea: Model -> Model
    genPacSpawnArea model =
      let
        rowStart = mapHeight - (mapHeight // 3)
        colStart = mapWidth - 4
      in
        model
        |> \modl -> updateLevel modl rowStart (colStart - 1) Floor
        |> \modl -> updateLevel modl rowStart (colStart - 0) Floor
        |> \modl -> updateLevel modl rowStart (colStart + 1) Floor
        |> \modl -> updateLevel modl (rowStart + 1) (colStart - 1) Floor
        |> \modl -> updateLevel modl (rowStart + 1) (colStart - 0) (Pac Left 3)
        |> \modl -> updateLevel modl (rowStart + 1) (colStart + 1) Floor
        |> \modl -> updateLevel modl (rowStart + 2) (colStart - 1) Floor
        |> \modl -> updateLevel modl (rowStart + 2) (colStart - 0) Floor
        |> \modl -> updateLevel modl (rowStart + 2) (colStart + 1) Floor

    getMaxScore: Model -> Int
    getMaxScore model =
      let
        counter: Model -> Int -> Int -> Int -> Int
        counter model currRow currCol acc =
          if currRow > (List.length model.levelData) then
            acc
          else
            let
              currElt = getElement model currRow currCol
              firstRow = Maybe.withDefault [] (List.head model.levelData)
              numCols = List.length firstRow
              numRows = List.length model.levelData
              nextCol = (currCol + 1) % numCols
              nextRow = (currRow + ((currCol + 1) // numCols))
            in
              if currElt == Just(Dot) then
                counter model nextRow nextCol (acc + 10)
              else
                counter model nextRow nextCol acc
      in
        counter model 0 0 0
  in
    (Model 0 (List.repeat mapHeight (List.repeat mapWidth Dot)) Left 0 (Random.initialSeed seed))      -- generate an empty map
    |> \modl -> genTopBottomWalls 0 seed Top modl                                 -- generate top walls
    |> \modl -> genTopBottomWalls 0 (seed + 1) Down modl                          -- generate bottom walls
    |> \modl -> genLeftRightWalls 0 (seed + 2) Left modl                          -- generate left walls
    |> \modl -> genLeftRightWalls 0 (seed + 3) Right modl                         -- generate right walls
    |> \modl -> genRandomInnerWalls 0 20 (seed + 4) modl                          -- generate some random inner walls
    |> \modl -> genRegularInnerWalls 0 0 (seed + 5) modl                          -- generate some regular inner walls
    |> \modl -> genGhostHouse modl                                                -- generate the enemy spawn area
    |> \modl -> mirrorLevel modl 0                                                -- mirror the level
    |> \modl -> genPacSpawnArea modl                                              -- put PacMan in place
    |> \modl -> genEnemySpawnArea modl                                            -- put enemies in place
    |> \modl -> {modl | maxScore = (getMaxScore modl)}                            -- save the max score

-- View


view : Model -> Html Msg
view model =
    let
      (_, pacPosRow, pacPosCol) = findPacMan model
      lives = case getElement model pacPosRow pacPosCol of
        Just(Pac dir lives) -> lives
        _ -> 0

      -- Render the header
      renderHeaderBar: Html Msg
      renderHeaderBar =
        div [ class "headerBar" ]
          [ h1 [] [text "PacMan V0.01"] ]

      -- Render the debug infos
      renderScoreBar: Html Msg
      renderScoreBar =
        let
          hearts =  (List.append (List.repeat (3 - lives) (span [class "skulls"] [text "☠"])) (List.repeat lives (span [class "hearts"] [text "♥"])))
        in
          div [ id "scoreBar" ]
          [ span [id "scoreField"] [text ("Score = " ++ (toString model.score) ++ "/" ++ (toString model.maxScore))], span [class "seperator"] [], span [id "livesField"] hearts ]

      -- Render the debug infos
      renderDebugBar: Html Msg
      renderDebugBar =
        div [ class "debugBar" ]
          [ text (toString model.lastKeyPressed) ]

      -- Render a level element
      renderLevelElement: LevelElement -> Html Msg
      renderLevelElement levelElement =
        case levelElement of
          Wall -> td [class "mapTableCell", class "wall"] [text "⁣"]
          Floor -> td [class "mapTableCell"] []
          Dot -> td [class "mapTableCell", class "dot"] [text("•")]
          Pill -> td [class "mapTableCell"] [text("+")]
          Pac dir lives ->
            case dir of
              Top -> td [class "mapTableCell", class "pacUp"] []
              Down -> td [class "mapTableCell", class "pacDown"] []
              Left -> td [class "mapTableCell", class "pacLeft"] []
              Right -> td [class "mapTableCell", class "pacRight"] []
          Clyde _ _ _ -> td [class "mapTableCell", class "clyde"] []
          Inky _ _ _ -> td [class "mapTableCell", class "inky"] []
          Pinky _ _ _ -> td [class "mapTableCell", class "pinky"] []
          Blinky _ _ _ -> td [class "mapTableCell", class "blinky"] []

      -- Render a row of a level
      renderLevelRow: List LevelElement -> Html Msg
      renderLevelRow row =
        tr [class "mapTableRow"] (List.map renderLevelElement row)

      loadCss = "var head = document.getElementsByTagName('head')[0]; \n var link = document.createElement('link'); \n link.rel = 'stylesheet'; \n link.type  = 'text/css'; \n link.href  = 'http://localhost:8000/style.css'; \n link.media = 'all'; \n head.appendChild(link);"

      outerContainer: List (Html msg) -> Html msg
      outerContainer cont =
         div [id "outerContainer"] [div [id "middleContainer"] [div [id "innerContainer"] ((node "script" [] [text loadCss]) :: cont) ]]
    in
      if lives > 0 then
        if model.score >= model.maxScore then
          outerContainer [ div [id "winContainer"] [text "Win!"]]
        else
          outerContainer [renderHeaderBar, renderScoreBar, table [id "mapTable"] (List.map renderLevelRow model.levelData), div [id "codeLink"] [a [href "code.html"] [text "Source Code"]]]
      else
        outerContainer [ div [id "gameOverContainer"] [text "Game Over!"]]


-- Update


type Msg
    = KeyMsg Keyboard.KeyCode
    | Tick Time.Time
    | GenMap
    | MapSeed Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyMsg code ->
          case code of
            37 -> ({model | lastKeyPressed = Left}, Cmd.none)
            38 -> ({model | lastKeyPressed = Top}, Cmd.none)
            39 -> ({model | lastKeyPressed = Right}, Cmd.none)
            40 -> ({model | lastKeyPressed = Down}, Cmd.none)
            _ -> (model, Cmd.none)
        Tick time ->
          (stepLevel model, Cmd.none)
        GenMap ->
          (model, Random.generate MapSeed (Random.int Random.minInt Random.maxInt))
        MapSeed newSeed ->
          (genLevel newSeed, Cmd.none)


findCharacter: Model -> (LevelElement -> Bool) -> Maybe (LevelElement, Int, Int)
findCharacter model filterRule =
  let
    flatter: (Int, List (Int, LevelElement)) -> List (Int, Int, LevelElement)
    flatter x =
      let
        (rowIdx, col) = x
      in
        List.map (\y -> (rowIdx, Tuple.first y, Tuple.second y)) col

    shiftedFilterRule: (Int, Int, LevelElement) -> Bool
    shiftedFilterRule (rowIdx, colIdx, elt) =
      filterRule elt

    indexedList = List.concat (List.map flatter (List.indexedMap (\rowIdx row -> (rowIdx, (List.indexedMap (\colIdx elt -> (colIdx, elt)) row))) model.levelData))
    res = List.filter shiftedFilterRule indexedList
  in
    case res of
      [] -> Nothing
      [first] ->
        let
          (resRow, resCol, resElt) = first
        in
          Just (resElt,resRow, resCol)
      first :: rest ->
        let
          (resRow, resCol, resElt) = first
        in
          Just (resElt,resRow, resCol)

findPacMan: Model -> (LevelElement, Int, Int)
findPacMan model =
  let
    filterRule: LevelElement -> Bool
    filterRule element =
      case element of
        Pac _ _ -> True
        _ -> False

    res = findCharacter model filterRule
  in
    case res of
      Just(elt, row, col) -> (elt, row, col)
      Nothing ->
        ((Pac Left 3), 0, 0)


updateLevel: Model -> Int -> Int -> LevelElement -> Model
updateLevel model row col newElement =
  let
    colUpdater: Int -> LevelElement -> Int -> LevelElement -> LevelElement
    colUpdater replaceIdx replaceElt colIdx elt =
      if colIdx == replaceIdx then
        replaceElt
      else
        elt

    rowUpdater: Int -> List LevelElement -> Int -> List LevelElement -> List LevelElement
    rowUpdater replaceIdx replaceElt colIdx elt =
      if colIdx == replaceIdx then
        replaceElt
      else
        elt

    oldRow = Maybe.withDefault [] (List.head (List.drop row model.levelData))
    newRow = List.indexedMap (colUpdater col newElement) oldRow
    newLevelData = List.indexedMap (rowUpdater row newRow) model.levelData
  in
    {model | levelData = newLevelData}

getElement: Model -> Int -> Int -> Maybe LevelElement
getElement model row col =
  if ((row >= 0) && (col >= 0)) then
    List.head (List.drop col (Maybe.withDefault [] (List.head (List.drop row model.levelData))))
  else
    Nothing

getNextElement: Model -> Int -> Int -> Direction -> (Maybe LevelElement, Int, Int)
getNextElement model currPosRow currPosCol dir =
  let
    numCols = List.length (Maybe.withDefault [] (List.head model.levelData))
    numRows = List.length model.levelData
    newPosRow = case dir of
      Left ->
        currPosRow
      Right ->
        currPosRow
      Top ->
        Basics.max 0 (currPosRow - 1)
      Down ->
        Basics.min numRows (currPosRow + 1)
    newPosCol = case dir of
      Left ->
        Basics.max 0 (currPosCol - 1)
      Right ->
        Basics.min numCols (currPosCol + 1)
      Top ->
        currPosCol
      Down ->
        currPosCol
  in
    (getElement model newPosRow newPosCol, newPosRow, newPosCol)

stepLevel: Model -> Model
stepLevel model =
  if model.score >= model.maxScore then
    model
  else
    model
    |> \mdl -> stepPacMan mdl
    |> \mdl -> stepClyde mdl
    |> \mdl -> stepBlinky mdl
    |> \mdl -> stepPinky mdl
    |> \mdl -> stepInky mdl


stepClyde: Model -> Model
stepClyde model =
  let
    findClyde: Model -> (LevelElement, Int, Int)
    findClyde model =
      let
        filterRule: LevelElement -> Bool
        filterRule element =
          case element of
            Clyde _ _ _ -> True
            _ -> False

        res = findCharacter model filterRule
      in
        case res of
          Just(elt, row, col) -> (elt, row, col)
          Nothing ->
            ((Pac Left 3), 0, 0)
    (clyde, _, _) = findClyde model
    (dir, lvlElt, mode) = case clyde of
      Clyde dir lvlElt mode -> (dir, lvlElt, mode)
      _ -> (Left, Floor, Rnd)
  in
    case mode of
      Rnd ->
        stepEnemyRnd model findClyde dir lvlElt (\x y z -> Clyde x y z)
      _ ->
        stepEnemyRnd model findClyde dir lvlElt (\x y z -> Clyde x y z)


stepBlinky: Model -> Model
stepBlinky model =
  let
    findBlinky: Model -> (LevelElement, Int, Int)
    findBlinky model =
      let
        filterRule: LevelElement -> Bool
        filterRule element =
          case element of
            Blinky _ _ _ -> True
            _ -> False

        res = findCharacter model filterRule
      in
        case res of
          Just(elt, row, col) -> (elt, row, col)
          Nothing ->
            ((Pac Left 3), 0, 0)
    (blinky, _, _) = findBlinky model
    (dir, lvlElt, mode) = case blinky of
      Blinky dir lvlElt mode -> (dir, lvlElt, mode)
      _ -> (Left, Floor, Rnd)
  in
    case mode of
      Rnd ->
        stepEnemyRnd model findBlinky dir lvlElt (\x y z -> Blinky x y z)
      _ ->
        stepEnemyRnd model findBlinky dir lvlElt (\x y z -> Blinky x y z)


stepPinky: Model -> Model
stepPinky model =
  let
    findPinky: Model -> (LevelElement, Int, Int)
    findPinky model =
      let
        filterRule: LevelElement -> Bool
        filterRule element =
          case element of
            Pinky _ _ _ -> True
            _ -> False

        res = findCharacter model filterRule
      in
        case res of
          Just(elt, row, col) -> (elt, row, col)
          Nothing ->
            ((Pac Left 3), 0, 0)
    (pinky, _, _) = findPinky model
    (dir, lvlElt, mode) = case pinky of
      Pinky dir lvlElt mode -> (dir, lvlElt, mode)
      _ -> (Left, Floor, Rnd)
  in
    case mode of
      Rnd ->
        stepEnemyRnd model findPinky dir lvlElt (\x y z -> Pinky x y z)
      _ ->
        stepEnemyRnd model findPinky dir lvlElt (\x y z -> Pinky x y z)


stepInky: Model -> Model
stepInky model =
  let
    findInky: Model -> (LevelElement, Int, Int)
    findInky model =
      let
        filterRule: LevelElement -> Bool
        filterRule element =
          case element of
            Inky _ _ _ -> True
            _ -> False

        res = findCharacter model filterRule
      in
        case res of
          Just(elt, row, col) -> (elt, row, col)
          Nothing ->
            ((Pac Left 3), 0, 0)
    (inky, _, _) = findInky model
    (dir, lvlElt, mode) = case inky of
      Inky dir lvlElt mode -> (dir, lvlElt, mode)
      _ -> (Left, Floor, Rnd)
  in
    case mode of
      Rnd ->
        stepEnemyRnd model findInky dir lvlElt (\x y z -> Inky x y z)
      _ ->
        stepEnemyRnd model findInky dir lvlElt (\x y z -> Inky x y z)


stepEnemyRnd: Model -> (Model -> (LevelElement, Int, Int)) -> Direction -> LevelElement -> (Direction -> LevelElement -> EnemyMode -> LevelElement) -> Model
stepEnemyRnd model findEnemy dir lvlElt enemyConstructor =
  let
    (clyde, currPosRow, currPosCol) = findEnemy model
    (nextElement, newPosRow, newPosCol) = getNextElement model currPosRow currPosCol dir
  in
    case nextElement of
      Just(Floor) ->
        let
          (rndInt, newModel) = genRndInt model 1 10
          altDir = case dir of
              Left ->
                  if rndInt < 5 then Top else Down
              Right ->
                if rndInt < 5 then Top else Down
              Top ->
                if rndInt < 5 then Left else Right
              Down ->
                if rndInt < 5 then Left else Right
        in
          if rndInt <= 2 then --x% chance to spontaniously change the walk-direction
            newModel
            |> \mdl -> updateLevel mdl newPosRow newPosCol (enemyConstructor altDir Floor Rnd)
            |> \mdl -> updateLevel mdl currPosRow currPosCol lvlElt
          else
            newModel
            |> \mdl -> updateLevel mdl newPosRow newPosCol (enemyConstructor dir Floor Rnd)
            |> \mdl -> updateLevel mdl currPosRow currPosCol lvlElt
      Just(Dot) ->
        let
          (rndInt, newModel) = genRndInt model 1 10
          altDir = case dir of
              Left ->
                  if rndInt < 5 then Top else Down
              Right ->
                if rndInt < 5 then Top else Down
              Top ->
                if rndInt < 5 then Left else Right
              Down ->
                if rndInt < 5 then Left else Right
        in
          if rndInt <= 2 then --x% chance to spontaniously change the walk-direction
            newModel
            |> \mdl -> updateLevel mdl newPosRow newPosCol (enemyConstructor altDir Dot Rnd)
            |> \mdl -> updateLevel mdl currPosRow currPosCol lvlElt
          else
            newModel
            |> \mdl -> updateLevel mdl newPosRow newPosCol (enemyConstructor dir Dot Rnd)
            |> \mdl -> updateLevel mdl currPosRow currPosCol lvlElt
      Just(Pac _ _) ->
        loseLife model
      _ ->
        let
          (newDir, newModel) = case dir of
            Left ->
              let
                (rndInt, newModel) = genRndInt model 1 2
              in
                if rndInt == 1 then
                  (Top, newModel)
                else
                  (Down, newModel)
            Right ->
              let
                (rndInt, newModel) = genRndInt model 1 2
              in
                if rndInt == 1 then
                  (Top, newModel)
                else
                  (Down, newModel)
            Top ->
              let
                (rndInt, newModel) = genRndInt model 1 2
              in
                if rndInt == 1 then
                  (Left, newModel)
                else
                  (Right, newModel)
            Down ->
              let
                (rndInt, newModel) = genRndInt model 1 2
              in
                if rndInt == 1 then
                  (Left, newModel)
                else
                  (Right, newModel)
          (nextElement, newPosRow, newPosCol) = getNextElement newModel currPosRow currPosCol newDir
        in
          case nextElement of
            Just(Floor) ->
              newModel
              |> \mdl -> updateLevel mdl newPosRow newPosCol (enemyConstructor newDir Floor Rnd)
              |> \mdl -> updateLevel mdl currPosRow currPosCol lvlElt
            Just(Dot) ->
              newModel
              |> \mdl -> updateLevel mdl newPosRow newPosCol (enemyConstructor newDir Dot Rnd)
              |> \mdl -> updateLevel mdl currPosRow currPosCol lvlElt
            _ ->
              updateLevel newModel currPosRow currPosCol (enemyConstructor newDir lvlElt Rnd)


stepPacMan: Model -> Model
stepPacMan model =
  let
    (_, pacPosRow, pacPosCol) = findPacMan model
    numCols = List.length (Maybe.withDefault [] (List.head model.levelData))
    numRows = List.length model.levelData
    newPacPosRow = case model.lastKeyPressed of
      Left ->
        pacPosRow
      Right ->
        pacPosRow
      Top ->
        Basics.max 0 (pacPosRow - 1)
      Down ->
        Basics.min numRows (pacPosRow + 1)
    newPacPosCol = case model.lastKeyPressed of
      Left ->
        Basics.max 0 (pacPosCol - 1)
      Right ->
        Basics.min numCols (pacPosCol + 1)
      Top ->
        pacPosCol
      Down ->
        pacPosCol
    (_, lives) = case (List.head (List.drop pacPosCol (Maybe.withDefault [] (List.head (List.drop pacPosRow model.levelData))))) of
      Just(Pac dir lives) -> (dir, lives)
      _ -> (Top,0)
    nextElement = (List.head (List.drop newPacPosCol (Maybe.withDefault [] (List.head (List.drop newPacPosRow model.levelData)))))
  in
    case nextElement of
      Just(Pac dir lives) ->
        -- Replace old pacMan by new pacMan
        updateLevel model pacPosRow pacPosCol (Pac model.lastKeyPressed lives)
      Just(Wall) ->
        -- Replace old pacMan by new pacMan
        updateLevel model pacPosRow pacPosCol (Pac model.lastKeyPressed lives)
      Just(Dot) ->
        let
          -- Replace Dot by pacMan
          step1 = updateLevel model newPacPosRow newPacPosCol (Pac model.lastKeyPressed lives)
          -- Replace pacMan by Floor
          step2 = updateLevel step1 pacPosRow pacPosCol Floor
          -- Update the score
          step3 = {step2 | score = step2.score + 10}
        in
          step3
      Just(Floor) ->
        let
          -- Replace Floor by pacMan
          step1 = updateLevel model newPacPosRow newPacPosCol (Pac model.lastKeyPressed lives)
          -- Replace pacMan by Floor
          step2 = updateLevel step1 pacPosRow pacPosCol Floor
        in
          step2
      Just(Clyde _ _ _) ->
        loseLife model
      Just(Blinky _ _ _) ->
        loseLife model
      Just(Inky _ _ _) ->
        loseLife model
      Just(Pinky _ _ _) ->
        loseLife model

      _ ->
        -- Replace old pacMan by new pacMan
        updateLevel model pacPosRow pacPosCol (Pac model.lastKeyPressed lives)


loseLife: Model -> Model
loseLife model =
  let
    (pac, pacPosRow, pacPosCol) = findPacMan model
    lives = case pac of
      Pac _ lives -> lives
      _ -> 0
    mapWidth = List.length (Maybe.withDefault [] (List.head model.levelData))
    mapHeight = List.length model.levelData
    rowStart = mapHeight - (mapHeight // 3)
    colStart = (mapWidth // 2) - 1
  in
    updateLevel model pacPosRow pacPosCol Floor
    |> \mdl -> updateLevel mdl (rowStart + 1) colStart (Pac model.lastKeyPressed (lives - 1))

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [Keyboard.downs KeyMsg, Time.every (300 * Time.millisecond) Tick]



-- Entry point


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Day 1: 2h33
-- Day 2: 0h16
--        0h27
-- Day 3: 0h31
-- Day 4: 6h15
-- Day 5: 2h31
-- Day 6: 0h42
-- Day 7: 1h56
-- Day 8: 0h25
--        0h58
--        2h15
