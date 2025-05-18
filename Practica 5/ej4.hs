tad PQ (A: Set, B: OrderedSet) where
    import Bool
    vacia :: PQ A,B
    poner :: A -> B -> PQ A,B -> PQ A,B
    primero :: PQ A,B -> A
    sacar :: PQ A,B -> PQ A,B
    esVacia :: PQ A,B -> Bool
    union :: PQ A,B -> PQ A,B -> PQ A,B

primero :: PQ A,B -> A

sacar :: PQ A,B -> PQ A,B

esVacia :: PQ A,B -> Bool
esVacia vacia = True
esVacia (poner e p pq) = False

union :: PQ A,B -> PQ A,B -> PQ A,B