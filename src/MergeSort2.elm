module MergeSort2 exposing (main, init, update, view)
import Browser
import Html exposing (Html, Attribute, div, input, button, text)
import Html.Attributes exposing (type_, value, style, disabled)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array)



-- MAIN
main: Program () Model Msg
main = 
  Browser.sandbox {init = init, update = update, view = view}



-- MODEL
type alias Model = {
    a: Array Int,
    i: Int,
    j: Int
  }

init: Model
init =
  Model initArray -1 -1

initArray: Array Int
initArray =
  Array.fromList [7, 5, 9, 4, 2, 1, 8]



-- UPDATE
type Msg =
  Change Int String |
  Pick Int |
  Order

update: Msg -> Model -> Model
update msg model =
  let {a, i, j} = model in
  case msg of
    Change k s ->
      {model | a = Array.set k (toInt s) a}
    Pick k ->
      updatePick (
      if k == i then
        {model | i = -1}
      else if k == j then
        {model | j = -1}
      else if i == -1 then
        {model | i = k}
      else
        {model | j = k}
      )
    Order ->
      if i>=0 && j>=0 && arrayGet i a > arrayGet j a then
        {model | a = arraySwap i j a, i = -1, j = -1}
      else
        model

updatePick: Model -> Model
updatePick model =
  let {i, j} = model in
  {model | i = min i j, j = max i j}



-- VIEW
view: Model -> Html Msg
view model =
  let {a, i, j} = model in
  div styleBody [
    div [] (Array.toList (Array.indexedMap
      (\k v -> viewNum v (Change k) (Pick k) (k==i || k==j)) a
    )),
    div styleSubmit [
      viewBtn (i<0 || j<0 || arrayGet i a < arrayGet j a) Order
    ]
  ]

viewNum: Int -> (String -> msg) -> msg -> Bool -> Html msg
viewNum v inp clk h =
  input (styleNum h ++ [
    type_ "number",
    value (String.fromInt v),
    onInput inp,
    onClick clk
  ]) []

viewBtn: Bool -> msg -> Html msg
viewBtn d msg =
  button (styleBtn ++ [
    disabled d,
    onClick msg
  ]) [text "Order"]



-- STYLES
styleNum: Bool -> List (Attribute m)
styleNum h = [
    style "border-bottom" (if h then "3px solid green" else ""),
    style "font-size" "1.5em",
    style "width" "3em"
  ]

styleBody: List (Attribute m)
styleBody = [
    style "text-align" "center",
    style "margin" "2em"
  ]

styleSubmit: List (Attribute m)
styleSubmit = [
    style "margin-top" "1em"
  ]

styleBtn: List (Attribute m)
styleBtn = [
    style "font-size" "1.5em"
  ]



-- UTIL
toInt: String -> Int
toInt s =
  Maybe.withDefault 0 (String.toInt s)

arrayGet: Int -> Array Int -> Int
arrayGet i x =
  Maybe.withDefault 0 (Array.get i x)

arraySwap: Int -> Int -> Array Int -> Array Int
arraySwap i j x =
  let a = arrayGet i x
      b = arrayGet j x
  in Array.set i b (Array.set j a x)
