------------------------------
-- LIBRARY for NATURAL NUMBERS
------------------------------

data NN = O | S NN
    deriving (Eq,Show)

-- addition
add :: NN -> NN -> NN
add O n = n
add (S n) m = S (add n m)

-- multiplication
mult :: NN -> NN -> NN
mult O n = O
mult (S O) n = n
mult (S n) m = add m (mult n m)

-- subtract
subtr :: NN -> NN -> NN
subtr O n = O
subtr n O = n
subtr (S n) (S m) = subtr n m

-- less n m if n < m
-- use recursion on NN
less :: NN -> NN -> Bool
less n O = False
less O n = True
less (S n) (S m) = less n m



-- greatest common divisor
-- implement Euclid's algorithm, allow gcdN O O = O to make it simpler
-- use recursion on NN and subtr (you don't need modulo)
gcdN :: NN -> NN -> NN
gcdN m n = n
gcdN less (m n) = gcdN (sub (m n), n)
gcdN less (n m) = gcdN (m, sub(n m))

-------------------------------
-- LIBRARY for POSITIVE NUMBERS
-------------------------------

data PN = I | T PN
    deriving (Eq,Show)

-- addition of positive numbers
-- use recursion over PN
addP :: PN -> PN -> PN
addP I n = n
addP (T n) m = T (addP n m)

-- multiplication of positive numbers
-- use recursion over PN

multP :: PN -> PN -> PN
multP I n = I
multP (T n) m = add m (multP n m)

-- lessP n m if n < m
-- use recursion over PN
lessP :: PN -> PN -> Bool
lessP n I = False
lessP I n = True
lessP (T n) (T m) = lessP n m

-- subtract a positive number from a natural number
-- use recursion over NN
ssubtrNP :: NN -> PN -> NN
subtrNP O n = O
subtrNP S n I = n
subtrNP (S n) (T m) = subtrNP n m


less :: NN -> PN -> Bool
less n I = False
less I n = True
less (T n) (T m) = less n m

-- divide a natural number by a positive number
-- use recursion over NN
divP :: NN -> PN -> NN
divi n m  = if n==m then S O
else if less n m then O
else add (S O) (divi (subtr n m) m) -- n/m = 1 + (n-m)/m

-- convert from PN to NN
-- use recursion over PN
p2n :: PN -> NN
p2n I = (S O)
p2n T n = S p2n n

-- convert from NN to PN
-- use recursion over NN
-- allow runtime error if NN is O
n2p :: NN ->PN
n2p (S 0) = I
n2p S n = T n2p n

------------
-- FRACTIONS
------------

-- non-negative fractions
-- a fraction is a pair (numerator,denominator)
type Frac = (NN,PN)

-- multiply fractions
-- recall from school how to multiply fractions
multF :: Frac -> Frac -> Frac
multF p q = ( (mult (fst p) (fst q)), (multF (snd p) (snd q)))


-- add fractions
-- recall from school how to add fractions
addF :: Frac -> Frac -> Frac
addF p q = ( ( add (mult (fst p) ( p2n (snd q))) ( mult (fst q) (p2n (snd p))), multP (snd p) (snd q)))

-- equality of fractions
-- recall from school how to check that two fractions are equal
equalF :: Frac -> Frac -> Bool
equalF p q = subtr ( divP (fst p) (snd p)) ( divP (fst p) (snd q)) == 0
equalF p q = False
-- simplify fractions
-- divide numerator and denominator by the gcd of both
simplifyF :: Frac -> Frac
simplifyF  p = (divP (fst p) ( n2p (gcdN( (fst p) (snd q))))) , (divP (snd) ( n2p (gcdN( (fst p) (snd q)))

--------------
-- FOR TESTING
--------------

-- use recursion on NN
nn2int :: NN -> Int
nn2int (S 0) = 1
nn2int S n = 1 + nn2int n

-- use recursion on Int
-- allow runtime error for negative numbers
int2nn :: Int -> NN
int2nn 1 = (S 0)
int2nn 1 + n = S int2nn n

-- use recursion on Int
-- allow runtime error for non-positive numbers
int2pn :: Int -> PN
int2pn 1 = I
int2pn 1 + i = I  int2pn i

-- use int2nn and int2pn
ints2frac :: (Int,Int) -> Frac
ints2frac (n,p) = (int2nn n, int2pn p)
ints2frac 1 = (n,p)
ints2frac (1+n, 1+p) = S ints2frac(p, n)
-- use nn2int
frac2ints :: Frac -> (Int,Int)
frac2ints x = (nn2int (fst x), nn2int(p2n(snd x)))

-- Some examples (make your own):
--
-- frac2ints (addF (ints2frac (2,3)) (ints2frac (6,8)))
-- equalF (ints2frac (2,6)) (ints2frac (1,3))
-- addF (ints2frac (36,60)) (ints2frac (24,45))
-- simplifyF (addF (ints2frac (36,60)) (ints2frac (24,45)))
-- frac2ints(simplifyF (addF (ints2frac (36,60)) (ints2frac (24,45))))
