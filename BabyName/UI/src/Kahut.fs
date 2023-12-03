module Kahut

open Browser.Types
open Html

let private initImageSelector () =

    let image = fromId "image" :?> HTMLImageElement
    let imageSelector = fromId "image-selector" :?> HTMLInputElement

    image.onload <- (fun _ ->
        let ratio = image.naturalHeight / image.naturalWidth
        image.width <- image.width * 0.5
        image.height <- image.height * 0.5
    )

    imageSelector |> onChange (fun () ->
        let reader = Browser.Dom.FileReader.Create()
        reader.onload <- (fun _ ->
            image.src <- reader.result.ToString()
        )
        reader.readAsDataURL(imageSelector.files[0])
    )

let initKahut () =
    printfn "Hello kahut"
    initImageSelector ()
    let text = fromId "text"
    text.innerText <- "Hello kahuut"



