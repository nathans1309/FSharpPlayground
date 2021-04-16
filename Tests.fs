module Tests

open System
open Xunit
open FsUnit

[<Fact>]
let ``QuickSort`` () =
  let arr = [|5;4;2;3;1;|]
  Array.quickSort arr 0 (arr.Length-1) |> ignore
  let expected = [|1;2;3;4;5;|]
  expected |> should equal arr


[<Fact>]
let ``MergeSort`` () =
  let arr = [|5;4;2;3;1;|]
  let sorted = Array.mergeSortWrapper arr
  let expected = [|1;2;3;4;5;|]
  sorted |> should equal expected
