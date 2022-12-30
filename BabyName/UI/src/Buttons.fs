module Buttons

open Browser.Types
open Browser

let showCopiedAnimation (btn: HTMLButtonElement) =
    let mutable notInAnimation = true
    let btnText = btn.textContent

    fun _ ->
        if notInAnimation then
            notInAnimation <- false
            btn.textContent <- "Copied!"

            let reset _ =
                btn.textContent <- btnText
                notInAnimation <- true

            window.setTimeout (reset, 1000) |> ignore