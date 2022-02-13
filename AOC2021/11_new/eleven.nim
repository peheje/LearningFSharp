import std/strutils

var octos = newSeq[int]()
var size = 0
for line in lines("sample.txt"):
    size += 1
    for c in line:
        octos.add(parseInt($c))

proc toIndex (row: int, col: int): int = row * size + col

proc anyFlashing(data: seq[int]): bool =
    for x in data:
        if x > 9: return true
    return false

proc surroundingIndices(row: int; col: int): seq[int] =
    let neighbors = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]
    for neighbor in neighbors:
        let r = neighbor[0] + row
        let c = neighbor[1] + col
        let index = toIndex(r, c)
        if index >= 0 and index < octos.len:
            result.add(index)

proc print(octos: seq[int]) =
    for i in 0..<size:
        for j in 0..<size:
            let index = toIndex(i, j)
            stdout.write octos[index]
        echo ""
    echo "----------"

var flashes = 0
for step in 0..<99:
    for i in 0..<octos.len:
        octos[i] += 1

    while (anyFlashing(octos)):
        for i in 0..<size:
            for j in 0..<size:
                let index = toIndex(i, j)
                if octos[index] > 9:
                    flashes += 1
                    octos[index] = -1
                    for ni in surroundingIndices(i, j):
                        if octos[ni] != -1:
                            octos[ni] += 1

    for i in 0..<octos.len:
        if octos[i] == -1:
            octos[i] = 0
    
    print octos

echo flashes