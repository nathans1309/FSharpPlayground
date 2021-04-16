module Array
  let swap (arr:int[]) left right = 
    let el = arr.[left]
    arr.[left] <- arr.[right]
    arr.[right] <- el

  let partition (arr:int[]) _left _right pivot =
    let mutable left = _left
    let mutable right = _right

    while left <= right do
      while arr.[left] < arr.[pivot] do
        left <- left + 1
      while arr.[right] > arr.[pivot] do
       right <- right - 1
    
      if left <= right
      then
        swap arr left right
        left <- left + 1
        right <- right - 1

    left

  let rec quickSort arr left right =
    //if left and right overlap or past, then stop
    if left >= right
    then ()
    else 
      //pick random pivot
      //start with one index at leftmost and another rightmost
      //move left index right until a value can be found > than pivot value
      //move right index left until a value can be found < than pivot value
      //once left and right are found, swap them and continue the search for more matches
      //if the left and right meet, then recurse with divide and conquer where the final left index can be used to set the boundary for the next searches
      let pivot = (left + right)/2
      let bound = partition arr left right pivot
      quickSort arr left (bound-1)
      quickSort arr bound right

  let rec mergeSort (_left:int) (_right:int) (arr:int[]) =
    //split array in half
    //sort the left and the right
    //base case: if there is only 1 in the array

    let rec nextSmallest sortedLeft sortedRight =
      match sortedLeft, sortedRight with
      | sortedLeft, [] -> sortedLeft
      | [], sortedRight -> sortedRight
      | headLeft::tailLeft, headRight::tailRight ->
        if headLeft < headRight
        then headLeft::(nextSmallest tailLeft sortedRight)
        else headRight::(nextSmallest sortedLeft tailRight)

    if _left = _right
    then [ arr.[_left] ]
    else
      let mid = (_left + _right)/2
      let sortedLeft = mergeSort _left mid arr
      let sortedRight = mergeSort (mid+1) _right arr
      nextSmallest sortedLeft sortedRight

  let mergeSortWrapper (arr:int[]) =
    mergeSort 0 (arr.Length-1) arr
    |> List.toArray

