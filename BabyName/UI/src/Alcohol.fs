module Alcohol

open Html

let private mlInput = inputFromId "millilitres"
let private pcInput = inputFromId "percentage"

let private calculateAndShowUnits () =
    let percentage = pcInput.valueAsNumber / 100.0
    let millilitresAlcohol = mlInput.valueAsNumber * percentage
    let dkUnits = millilitresAlcohol / 15.0
    (fromId "dk-units").innerHTML <- dkUnits.ToString("#.##")

let initAlcohol () =
    mlInput.addEventListener (
        "input",
        (fun _ ->
            if mlInput.valueAsNumber < 0 then
                mlInput.valueAsNumber <- 0

            calculateAndShowUnits ())
    )

    pcInput.addEventListener (
        "input",
        (fun _ ->
            if pcInput.valueAsNumber > 100 then
                pcInput.valueAsNumber <- 100
            elif pcInput.valueAsNumber < 0 then
                pcInput.valueAsNumber <- 0

            calculateAndShowUnits ())
    )

    calculateAndShowUnits ()
