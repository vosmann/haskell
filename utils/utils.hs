partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = part p ([],[]) xs
                 where part p (ts,fs)     [] = (ts,fs)
                       part p (ts,fs) (x:xs) = if p x then part p (x:ts,fs) xs else part p (ts,x:fs) xs

