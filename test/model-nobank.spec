vars
     b  g  r w y # wallet
    # bb bg br bw by # bank
rules
    # t_0
    b >= 1 ->
      w' = w+3,
      y' = y-1,
      r' = r-1,
      g' = g-2;

    b >= 1 ->
      w' = w-3,
      y' = y+1,
      r' = r+1,
      g' = g+2;

    # t_1
    g >= 0 ->
      w' = w-4,
      b' = b+2,
      g' = g+1;

    g >= 0 ->
      w' = w+4,
      b' = b-2,
      g' = g-1;

    # t_2
    g >= 0 ->
      g' = g-1,
      r' = r-1,
      b' = b+2,
      w' = w+1;

    g >= 0 ->
      g' = g+1,
      r' = r+1,
      b' = b-2,
      w' = w-1;

    # t_3
    g >= 0 ->
      b' = b-2,
      y' = y-2,
      r' = r+2;

    g >= 0 ->
      b' = b+2,
      y' = y+2,
      r' = r-2;

    # t_4
    g >= 0 ->
      g' = g-1,
      w' = w-1,
      y' = y-1,
      b' = b+1;

    g >= 0 ->
      g' = g+1,
      w' = w+1,
      y' = y+1,
      b' = b-1;

    # t_5
    g >= 0 ->
      g' = g-1,
      r' = r+1,
      w' = w+2,
      y' = y+1;

    g >= 0 ->
      g' = g+1,
      r' = r-1,
      w' = w-2,
      y' = y-1;

    # t_6
    g >= 0 ->
      b' = b-1,
      y' = y-2,
      g' = g+1,
      r' = r+1,
      w' = w+1;

    g >= 0 ->
      b' = b+1,
      y' = y+2,
      g' = g-1,
      r' = r-1,
      w' = w-1;

    # t_7
    g >= 0 ->
      b' = b-2,
      r' = r+2;

    g >= 0 ->
      b' = b+2,
      r' = r-2;

    # t_8
    g >= 0 ->
      w' = w-1,
      b' = b+2,
      g' = g+1;

    g >= 0 ->
      w' = w+1,
      b' = b-2,
      g' = g-1;

    # t_9
    g >= 0 ->
      g' = g-4,
      r' = r+1,
      w' = w+1;

    g >= 0 ->
      g' = g+4,
      r' = r-1,
      w' = w-1;
init
    # b=0, g=2, r=2, w=0, y=0
    b=1, g=1, r=2, w=3, y=1
    # bb=100, bg=10, br=10, bw=10, by=10
    #bb=5, bg=0, br=0, bw=0, by=0
target
    b >= 5, r >= 2, w >= 4, y >= 3
    # bb>=0, bg>=0, br>=0, bw>=0, by>=0
