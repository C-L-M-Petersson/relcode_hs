module QState.Utility.Parse
(   filterNAN
,   filterInfinity
) where


filterNAN :: String -> String
filterNAN ('N':'a':'N':cs) = '0':filterNAN cs
filterNAN           (c:cs) =  c :filterNAN cs
filterNAN              []  = []

filterInfinity :: String -> String
filterInfinity ('I':'n':'f':'i':'n':'i':'t':'y':cs) = '0':filterInfinity cs
filterInfinity                               (c:cs) =  c :filterInfinity cs
filterInfinity                                  []  = []
