open System

type Customer =
    { customerId: string
      email: string
      eligible: string
      registered: string
      dateRegistered: DateTimeOffset option
      discount: string }

let inputData =
    """CustomerId|Email|Eligible|Registered|DateRegistered|Discount
John|john@test.com|1|1|2015-01-23|0.1
Mary|mary@test.com|1|1|2018-12-12|0.1
Richard|richard@nottest.com|0|1|2016-03-23|0.0
Sarah||0|0||
Peter||0|0|"""

let rows = inputData.Split ("\n")

let parseDateTimeOffset (dateTimeOffsetString: string) =
    match DateTimeOffset.TryParse(dateTimeOffsetString) with
    | (true, v) -> Some v
    | (false, _) -> None

type ParseCustomerError = { message: string; row: string }

let parseCustomer (row: string) =
    let expectedColumns = 6

    match row.Split("|") with
    | [| id; email; eligible; registered; dateRegistered; discount |] ->
        Ok
            { customerId = id
              email = email
              eligible = eligible
              registered = registered
              dateRegistered = parseDateTimeOffset dateRegistered
              discount = discount }
    | sx ->
        Error
            { message = $"parseCustomer error, saw {sx.Length} columns but expected {expectedColumns}"
              row = row }

let splitByOkAndErrors (oks, errors) result =
    match result with
    | Ok value -> (value :: oks, errors)
    | Error error -> (oks, error :: errors)

let parsedOkcustomers, failedToParseErrors =
    rows
    |> Array.map parseCustomer
    |> Array.fold splitByOkAndErrors ([], [])

let customersJson = Text.Json.JsonSerializer.Serialize(parsedOkcustomers)

let outpath = "/Users/phj/Code/F-Sharp-Advent-of-Code-2021/Playground/out.json"

IO.File.WriteAllText(outpath, customersJson)
