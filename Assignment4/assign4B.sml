fun sumList [] = 0
|   sumList (h::t) = h + sumList t;
sumList [2,4,6,8,12];

fun fibonacci 0 = 0
|   fibonacci 1 = 1
|   fibonacci 2 = 1
|   fibonacci x = fibonacci(x-1) + fibonacci(x-2);

fibonacci 6;

fun reverse [] = []
|   reverse [h] = [h]
|   reverse (h::t) = reverse t @ [h];

reverse [1,2,3,4];

fun rotate [] x = []
|   rotate L x = let
                val len = length(L);
                val x1 = x mod len;
                val x2 = len - x1;
                val L1 = List.drop(L,x2);
                val L2 = List.take(L,x2);
                in
                L1@L2
                end;
                
rotate [1,2,3,4,5,6,7] 12;


fun split [] = ([],[])
|   split L = let
                val n = length(L) div 2;
                val L1 = List.take(L,n);
                val L2 = List.drop(L,n);
              in
                (L1,L2)
              end;

# split [1,2,3,5,6,7]



fun merge ([],[]) = []
|   merge(x,[]) = x
|   merge([],x) = x
|   merge(h1::t1, h2::t2) = if h1 < h2 then h1::(merge(t1,h2::t2)) else h2::(merge(h1::t1,t2));

fun msort [] = []
|   msort [l] = [l]
|   msort L = let
                val (l,r) = split L
              in
                merge(msort l, msort r)
              end;
              
# msort([2, 5, 3, 4, 1]);


fun hanoi (n,p1,p2,p3) = if n = 0 then [] 
                        else 
                            hanoi(n-1,p1,p3,p2) @ ((p1,p3)::hanoi(n-1,p2,p1,p3));
# hanoi(3, 1, 2, 3);

