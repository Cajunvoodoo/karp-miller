vars
    b g r w y
rules
    y >= 1 ->
         b' = b+2,
         y' = y-1;
    b >= 2 ->
         b' = b-2,
         y' = y+1;
    r >= 1 ->
         r' = r-1,
         y' = y+2;
    y >= 2 ->
         y' = y-2,
         r' = r+1;
    # g >= 1 ->
    #      g' = g-1,
    #      r' = r+1;
    # r >= 1 ->
    #      g' = g+1,
    #      r' = r-1;
init
    b=1, g=2, r=2, w=0, y=1
target
    b >= 1
