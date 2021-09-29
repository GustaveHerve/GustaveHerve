       
 let rec binom n k = 
 match k with 
 |k when k = n -> 1
 |0 -> 1
 |1 -> n
 |_ -> binom (n-1) k + binom (n-1) (k-1);;


let palindrome s = let last = (String.length(s)-1) in 
 let rec test n i = match n with
 |n when n >= i -> true
 |n when s.[n] = s.[i] -> test (n+1) (i-1)
 |_ -> false
 in test 0 (last);;

