module App.Main

open Browser

open BabyNames
open Compare
open Alcohol
open Heartbeat
open Unique
open Memory

match window.location.pathname with
| "/babynames.html" -> initBabyNames ()
| "/compare.html" -> initCompare ()
| "/alcohol.html" -> initAlcohol ()
| "/heartbeat.html" -> initHeartbeat ()
| "/unique.html" -> initUnique ()
| "/memory.html" -> initMemory ()
| _ -> failwith "unknown site!"