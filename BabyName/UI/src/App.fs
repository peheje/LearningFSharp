module App.Main

open Browser

open BabyNames
open Compare
open Alcohol
open Heartbeat
open Unique

match window.location.pathname with
| "/babynames.html" -> initBabyNames ()
| "/compare.html" -> initCompare ()
| "/alcohol.html" -> initAlcohol ()
| "/heartbeat.html" -> initHeartbeat ()
| "/unique.html" -> initUnique ()
| _ -> failwith "unknown site!"