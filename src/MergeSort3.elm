module MergeSort3 exposing (main, init, update, view)
import Browser
import Html exposing (Html, Attribute, div, input, button, text)
import Html.Attributes exposing (type_, name, value, style, selected, disabled)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array)
import List exposing (all, map, range)



-- MAIN
main: Program () Model Msg
main = 
  Browser.sandbox {init = init, update = update, view = view}



-- MODEL
type alias Model = {
    a: Array Int,
    b: Array Bool,
    i: Int,
    j: Int,
    m: Int
  }

init: Model
init =
  Model initArray initBorder -1 -1 -1

initArray: Array Int
initArray =
  Array.fromList [7, 5, 9, 4]

initBorder: Array Bool
initBorder =
  let al = Array.length initArray in
  Array.fromList (map (\_ -> True) (range 0 (al-2)))



-- UPDATE
type Msg =
  Change Int String |
  OrderPick Int |
  Order |
  MergePick Int |
  Merge

update: Msg -> Model -> Model
update msg model =
  let {a, b, i, j, m} = model in
  case msg of
    Change k s ->
      {model | a = Array.set k (toInt s) a}
    OrderPick k ->
      sortOrderPick (
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
      if canOrder model then
        {model | a = arraySwap 0 i j a, i = -1, j = -1}
      else model
    MergePick k ->
      {model | m = k}
    Merge ->
      if canMerge model then
        {model | b = Array.set m False b}
      else model

sortOrderPick: Model -> Model
sortOrderPick model =
  let {i, j} = model in
  {model | i = min i j, j = max i j}

canOrder: Model -> Bool
canOrder model =
  let {a, b, i, j} = model
      ai = arrayGet 0 i a
      aj = arrayGet 0 j a
      bij = Array.toList (Array.slice i j b) in
  i >= 0 &&
  j >= 0 &&
  i < j &&
  ai > aj &&
  all not bij

canMerge: Model -> Bool
canMerge model =
  let {a, b, m} = model
      bm = arrayGet False m b
      bi = arraySearchRight True (Array.slice 0 m b)
      bj = arraySearch True (Array.slice (m+1) (Array.length b) b)
      ai = if bi < 0 then 0 else bi+1
      aj = if bj < 0 then Array.length a else m+bj+2
      aim = Array.slice ai (m+1) a
      amj = Array.slice (m+1) aj a in
  bm &&
  arraySorted 0 aim &&
  arraySorted 0 amj



-- VIEW
view: Model -> Html Msg
view model =
  let {a, b} = model
      al = Array.length a
      bl = Array.length b in
  div styleBody [
    div [] (map (viewNumber model) (range 0 (al-1))),
    div [] (map (viewMerge model) (range 0 (bl-1))),
    div styleSubmit [
      viewButton "Order" Order (canOrder model),
      viewButton "Merge" Merge (canMerge model),
      viewStatus (arraySorted 0 a)
    ]
  ]

viewNumber: Model -> Int -> Html Msg
viewNumber model k = 
  let {a, b, i, j} = model
      n = arrayGet 0 k a
      picked = k == i || k == j
      lborder = arrayGet False (k-1) b
      rborder = arrayGet False k b in
  input (styleNumber picked lborder rborder ++ [
    type_ "number",
    value (String.fromInt n),
    onInput (Change k),
    onClick (OrderPick k)
  ]) []

viewMerge: Model -> Int -> Html Msg
viewMerge model k =
  let {m} = model in
  input (styleMerge ++ [
    type_ "radio",
    name "merge",
    selected (k == m),
    onClick (MergePick k)
  ]) []

viewButton: String -> msg -> Bool -> Html msg
viewButton v msg e =
  button (styleButton ++ [
    disabled (not e),
    onClick msg
  ]) [text v]

viewStatus: Bool -> Html Msg
viewStatus s =
  let m = if s then "You sorted it." else "Some elements are not in order." in
  div (styleStatus s) [text m]



-- STYLES
styleNumber: Bool -> Bool -> Bool -> List (Attribute m)
styleNumber p bl br = [
    style "border-bottom" (if p then "3px solid green" else ""),
    style "border-left"   (if bl then "3px solid red" else ""),
    style "border-right"  (if br then "3px solid red" else ""),
    style "font-size" "1.5em",
    style "width" "3em"
  ]

styleMerge: List (Attribute m)
styleMerge = [
    style "margin" "0 2.4em"
  ]

styleButton: List (Attribute m)
styleButton = [
    style "font-size" "1.5em"
  ]

styleStatus: Bool -> List (Attribute m)
styleStatus s = [
    style "background-color" (if s then "green" else "yellow"),
    style "margin-top" "1em"
  ]

styleSubmit: List (Attribute m)
styleSubmit = [
    style "margin-top" "1em"
  ]

styleBody: List (Attribute m)
styleBody = [
    style "text-align" "center",
    style "margin" "2em"
  ]



-- UTIL
toInt: String -> Int
toInt s =
  Maybe.withDefault 0 (String.toInt s)

arrayGet: d -> Int -> Array d -> d
arrayGet d i x =
  Maybe.withDefault d (Array.get i x)

arraySwap: d -> Int -> Int -> Array d -> Array d
arraySwap d i j x =
  let a = arrayGet d i x
      b = arrayGet d j x
  in Array.set i b (Array.set j a x)

arraySorted: comparable -> Array comparable -> Bool
arraySorted d x =
  let x0 = arrayGet d 0 x
      smaller v (u, s) = (v, s && u <= v) in
  Tuple.second (Array.foldl smaller (x0, True) x)

arraySearch: d -> Array d -> Int
arraySearch v x =
  let match u (i, f) = (i-1, if v==u then i-1 else f) in
  Tuple.second (Array.foldr match (Array.length x, -1) x)

arraySearchRight: d -> Array d -> Int
arraySearchRight v x =
  let match u (i, f) = (i+1, if v==u then i else f) in
  Tuple.second (Array.foldl match (0, -1) x)
