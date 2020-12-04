module MergeSort5 exposing (main, init, update, view)
import Browser
import Html exposing (Html, Attribute, div, input, button, text)
import Html.Attributes exposing (type_, name, value, style, checked, disabled)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array)
import List exposing (any, map, foldl, map2, drop, range)



-- MAIN
main: Program () Model Msg
main = 
  Browser.sandbox {init = init, update = update, view = view}



-- MODEL
type alias Model = {
    a: Array Int,
    b: Array Bool,
    i: Int, -- [i .. j .. k] (reorder)
    j: Int,
    k: Int,
    p: Int, -- [p <-> q] (swap)
    q: Int,
    m: Int
  }

init: Model
init =
  Model initArray initBorder -1 -1 -1 -1 -1 -1

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
  Next

update: Msg -> Model -> Model
update msg model =
  let {a} = model in
  case msg of
    Change n s ->
      {model | a = Array.set n (toInt s) a}
    Next ->
      if canReorder model then
        doReorder model
      else if canMerge model then
        doMerge model
      else if canNext model then
        {model | m = smallestMerge model}
      else model

canNext: Model -> Bool
canNext model =
  let {b} = model in
  any identity (Array.toList b) || canReorder model

canMerge: Model -> Bool
canMerge model =
  let {b, i, j, k, m} = model in
  i < 0 && j < 0 && k < 0 &&
  arrayGet False m b

smallestMerge: Model -> Int
smallestMerge model =
  let {b} = model
      bl = Array.length b
      length v (ls,l) = if v then (ls++[l],1) else (ls,l+1)
      lengths (ls,l) = ls++[l]
      minPair (l0,l1) (mi,ml,i) = (if l0+l1<=ml then i+l0 else mi,min (l0+l1) ml,i+l0)
      minPairAt (mi,_,_) = mi-1 in
  b |> Array.foldl length ([],1) |> lengths |> mapShift 1 toTuple |>
    foldl minPair (0,2*bl,0) |> minPairAt

doMerge: Model -> Model
doMerge model =
  let {a, b, m} = model
      bx = Array.set m False b
      bi = arraySearchRight True (Array.slice 0 m b)
      bj = arraySearch True (Array.slice (m+1) (Array.length b) b)
      ai = if bi < 0 then 0 else bi+1
      aj = if bj < 0 then Array.length a else m+bj+2 in
  {model | b = bx, i = ai, j = m+1, k = aj}

canReorder: Model -> Bool
canReorder model =
  let {i, j, k} = model in
  i >= 0 && j >= 0 && k >= 0

doReorder: Model -> Model
doReorder model =
  let {a, i, j, k, p, q} = model
      ai = arrayGet 0 i a
      aj = arrayGet 0 j a
      apq = arraySwap 0 p q a in
  if p > i then
    {model | a = apq, p = p-1, q = q-1}
  else if p == i then
    {model | a = apq, i = i+1, j = j+1, p = -1, q = -1}
  else if i >= k-1 then
    {model | i = -1, j = -1, k = -1}
  else if (i < j && ai <= aj) || j >= k then
    {model | i = i+1}
  else if i >= j then
    {model | i = i+1, j=j+1}
  else
    {model | p = j-1, q = j}



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
      viewButton "Next" Next (canNext model),
      viewStatus (arraySorted 0 a)
    ]
  ]

viewNumber: Model -> Int -> Html Msg
viewNumber model n = 
  let {a, b, i, j, p, q} = model
      v = arrayGet 0 n a
      picked = n == p || n == q
      lborder = n == i || n == j
      rborder = arrayGet False n b in
  input (styleNumber picked lborder rborder ++ [
    type_ "number",
    value (String.fromInt v),
    onInput (Change n)
  ]) []

viewMerge: Model -> Int -> Html Msg
viewMerge model k =
  let {m} = model in
  input (styleMerge ++ [
    type_ "radio",
    name "merge",
    checked (k == m),
    disabled True
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
    style "border-left"   (if bl then "3px solid blue" else ""),
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

toTuple: a -> b -> (a,b)
toTuple a b = (a, b)

mapShift: Int -> (a -> a -> b) -> List a -> List b
mapShift n fn x =
  map2 fn x (drop n x)

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
