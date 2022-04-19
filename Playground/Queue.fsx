// https://stackoverflow.com/questions/33464319/implement-a-queue-type-in-f

type queue<'a> =
    | Queue of 'a list * 'a list

let empty = Queue([], [])

let enqueue el queue =
    match queue with
    | Queue(fs, bs) -> Queue(el :: fs, bs)

let dequeue queue =
    match queue with
    | Queue([], []) -> failwith "Empty queue"
    | Queue(fs, b :: bs) -> b, Queue(fs, bs)
    | Queue(fs, []) ->
        let bs = List.rev fs
        bs.Head, Queue([], bs.Tail)

let q = empty

q
|> enqueue 1
|> enqueue 2
|> enqueue 3
|> dequeue
|> snd
|> enqueue 4