vars
     b  g  r w y # wallet
    bb bg br bw by # bank
rules
    # t_0
    b >= 1 ->
      w' = w+3,
      y' = y-1,
      r' = r-1,
      g' = g-2,

      bw' = bw-3,
      by' = by+1,
      br' = br+1,
      bg' = bg+2;

    # t_1
    b >= 1 ->
      w' = w-3,
      y' = y+1,
      r' = r+1,
      g' = g+2,

      bw' = bw+3,
      by' = by-1,
      br' = br-1,
      bg' = bg-2;

    # t_2
    g >= 0 ->
      w' = w-4,
      b' = b+2,
      g' = g+1,

      bw' = bw+4,
      bb' = bb-2,
      bg' = bg-1;

    # t_3
    g >= 0 ->
      w' = w+4,
      b' = b-2,
      g' = g-1,

      bw' = bw-4,
      bb' = bb+2,
      bg' = bg+1;

    # t_4
    g >= 0 ->
      g' = g-1,
      r' = r-1,
      b' = b+2,
      w' = w+1,

      bg' = bg+1,
      br' = br+1,
      bb' = bb-2,
      bw' = bw-1;

    # t_5
    g >= 0 ->
      g' = g+1,
      r' = r+1,
      b' = b-2,
      w' = w-1,

      bg' = bg-1,
      br' = br-1,
      bb' = bb+2,
      bw' = bw+1;

    # t_6
    g >= 0 ->
      b' = b-2,
      y' = y-2,
      r' = r+2,

      bb' = bb+2,
      by' = by+2,
      br' = br-2;

    # t_7
    g >= 0 ->
      b' = b+2,
      y' = y+2,
      r' = r-2,

      bb' = bb-2,
      by' = by-2,
      br' = br+2;

    # t_8
    g >= 0 ->
      g' = g-1,
      w' = w-1,
      y' = y-1,
      b' = b+1,

      bg' = bg+1,
      bw' = bw+1,
      by' = by+1,
      bb' = bb-1;

    # t_9
    g >= 0 ->
      g' = g+1,
      w' = w+1,
      y' = y+1,
      b' = b-1,

      bg' = bg-1,
      bw' = bw-1,
      by' = by-1,
      bb' = bb+1;

    # t_10
    g >= 0 ->
      g' = g-1,
      r' = r+1,
      w' = w+2,
      y' = y+1,

      bg' = bg+1,
      br' = br-1,
      bw' = bw-2,
      by' = by-1;

    g >= 0 ->
      g' = g+1,
      r' = r-1,
      w' = w-2,
      y' = y-1,

      bg' = bg-1,
      br' = br+1,
      bw' = bw+2,
      by' = by+1;

    # t_12
    g >= 0 ->
      b' = b-1,
      y' = y-2,
      g' = g+1,
      r' = r+1,
      w' = w+1,

      bb' = bb+1,
      by' = by+2,
      bg' = bg-1,
      br' = br-1,
      bw' = bw-1;

    g >= 0 ->
      b' = b+1,
      y' = y+2,
      g' = g-1,
      r' = r-1,
      w' = w-1,

      bb' = bb-1,
      by' = by-2,
      bg' = bg+1,
      br' = br+1,
      bw' = bw+1;

    # t_14
    g >= 0 ->
      b' = b-2,
      r' = r+2,

      bb' = bb+2,
      br' = br-2;

    g >= 0 ->
      b' = b+2,
      r' = r-2,

      bb' = bb-2,
      br' = br+2;

    # t_16
    g >= 0 ->
      w' = w-1,
      b' = b+2,
      g' = g+1,

      bw' = bw+1,
      bb' = bb-2,
      bg' = bg-1;

    g >= 0 ->
      w' = w+1,
      b' = b-2,
      g' = g-1,

      bw' = bw-1,
      bb' = bb+2,
      bg' = bg+1;

    # t_18
    g >= 0 ->
      g' = g-4,
      r' = r+1,
      w' = w+1,

      bg' = bg+4,
      br' = br-1,
      bw' = bw-1;

    g >= 0 ->
      g' = g+4,
      r' = r-1,
      w' = w-1,

      bg' = bg-4,
      br' = br+1,
      bw' = bw+1;

    # r >= 1 ->
    #      r' = r-1,
    #      y' = y+2;
    # # t_3
    # y >= 2 ->
    #      y' = y-2,
    #      r' = r+1;
    # # t_4
    # g >= 1 ->
    #      g' = g-1,
    #      r' = r+1;
    # # t_5
    # r >= 1 ->
    #      g' = g+1,
    #      r' = r-1;
init
    b=0, g=1, r=2, w=0, y=0,
    bb=50, bg=50, br=50, bw=50, by=50
    #bb=5, bg=0, br=0, bw=0, by=0
target
    b >= 5
