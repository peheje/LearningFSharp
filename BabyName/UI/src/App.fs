module App.Main

open Browser

open BabyNames
open Compare
open Alcohol

match window.location.pathname with
| "/babynames.html" -> initBabyNames ()
| "/compare.html" -> initCompare ()
| "/alcohol.html" -> initAlcohol ()
| _ -> failwith "unknown site!"