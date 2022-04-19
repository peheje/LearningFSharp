let q = new System.Collections.Generic.Queue<int>()

q.Enqueue 1
q.Enqueue 2
q.Enqueue 3

printfn "%A" q

printfn "%A" (q.Dequeue())