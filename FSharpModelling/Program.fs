﻿module EmailAddress =
    open System.Text.RegularExpressions

    type EmailAddress = private EmailAddress of string

    let private make (s: string) success failure =
        if Regex.IsMatch(s, @"^\S+@\S+\.\S+$") then
            success (EmailAddress s)
        else
            failure "e-mail did not match target format"

    let makeWithOption s =
        make s (fun email -> Some email) (fun _ -> None)

    let makeWithException s =
        make s (fun email -> email) (fun _ -> failwith "Not valid email")

    type CreationResult<'T> =
        | Success of 'T
        | Error of string

    let makeWithResult s =
        make s (fun email -> Success email) (fun error -> Error error)

module PersonName =
    open System

    type PersonName =
        private
            { FirstName: string
              MiddleName: string
              LastName: string }

    let private make firstName middleName lastName success failure =
        if String.IsNullOrWhiteSpace(firstName) then
            failure "first name cannot be empty"
        elif String.IsNullOrWhiteSpace(lastName) then
            failure "last name cannot be empty"
        else
            success
                { FirstName = firstName
                  MiddleName = middleName
                  LastName = lastName }

    let makeWithException firstName middleName lastName =
        make firstName middleName lastName (fun name -> name) (fun error -> failwith error)


// Consumers could open the module, or specify it directly
// open EmailAddress

match EmailAddress.makeWithResult "my@email.com" with
| EmailAddress.CreationResult.Success email -> printfn "successfully matched the email with value %A" email
| EmailAddress.CreationResult.Error error -> printfn "Failed to create email with error %s" error

let email2 = EmailAddress.makeWithOption "huehue"
printfn "%A" email2

let name1 = PersonName.makeWithException "Peter" "" "Jensen"

let name2 = PersonName.makeWithException "" "" "Jensen"

let email3 = EmailAddress.makeWithException "huehue BOOM"

// Compiler error:
// let email3 = (EmailAddress.EmailAddress "huehue")
// let name3 = (PersonName.PersonName "Peter" "" "")
