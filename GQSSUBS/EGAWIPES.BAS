DEFINT A-Z

'---- BASIC subs
DECLARE SUB Crossing (R, Delay)
DECLARE SUB Explode (R, Delay)
DECLARE SUB Fade (R, Delay)
DECLARE SUB Implode (R, Delay)
DECLARE SUB Instant ()
DECLARE SUB LargeBoxes (Delay)
DECLARE SUB LineCrush (R, Delay)
DECLARE SUB SmallBoxes (R, Delay)
DECLARE SUB SlideFan ()
DECLARE SUB SlideD ()
DECLARE SUB SlideL ()
DECLARE SUB SlideR ()
DECLARE SUB SlideU ()
DECLARE SUB Split ()

'---- ASM subs
DECLARE SUB TransferEGA (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%)
DECLARE SUB SlideDown (BYVAL ULCol, BYVAL ULLine, BYVAL LRCol, BYVAL LRLine, BYVAL DestCol, BYVAL DestLine)
DECLARE SUB SlideUp (BYVAL ULCol, BYVAL ULLine, BYVAL LRCol, BYVAL LRLine, BYVAL DestCol, BYVAL DestLine)
DECLARE SUB SlideRight (BYVAL ULCol, BYVAL ULLine, BYVAL LRCol, BYVAL LRLine, BYVAL DestCol, BYVAL DestLine)
DECLARE SUB SlideLeft (BYVAL ULCol, BYVAL ULLine, BYVAL LRCol, BYVAL LRLine, BYVAL DestCol, BYVAL DestLine)
DECLARE SUB SplitHorizontal (BYVAL ULCol, BYVAL ULLine, BYVAL LRCol, BYVAL LRLine, BYVAL DestCol, BYVAL DestLine)

'---- ASM functions
DECLARE FUNCTION PDQTimer& ()

SUB Crossing (R, Delay) STATIC

    x1 = 0
    y1 = 0
    U2 = 350 \ (80 \ R)
    U3 = U2 + 1
    U1 = 350 \ U3 \ 20
    T = 0
    WHILE (T < 350)
        WHILE PDQTimer& < NumTicks&: WEND
        NumTicks& = PDQTimer& + Delay \ 20
        S = 0
        WHILE (T < 350) AND (S <= U1)
          X2 = x1
          Y2 = y1
          WHILE (Y2 >= 0)
            CALL TransferEGA(X2, Y2, R, U3)
            Y2 = Y2 - U3
            X2 = X2 + R
          WEND
          T = T + U3
          y1 = y1 + U3
          S = S + 1
        WEND
    WEND

    y1 = y1 - U3
    x1 = x1 + R
    WHILE (X2 + R < 80)
        WHILE PDQTimer& < NumTicks&: WEND
        NumTicks& = PDQTimer& + Delay \ 20
        S = 0
        WHILE (X2 + R < 80) AND (S <= U1)
          X2 = x1
          Y2 = y1
          WHILE (Y2 >= 0)
            CALL TransferEGA(X2, Y2, R, U3)
            Y2 = Y2 - U3
            X2 = X2 + R
          WEND
          x1 = x1 + R
          S = S + 1
        WEND
    WEND

    T = 0
    x1 = X2
    y1 = 0
    WHILE (T < 350)
        WHILE PDQTimer& < NumTicks&: WEND
        NumTicks& = PDQTimer& + Delay \ 20
        S = 0
        WHILE (T < 350) AND (S <= U1)
          X2 = x1
          Y2 = y1
          WHILE (Y2 < 350)
            CALL TransferEGA(X2, Y2, R, U3)
            Y2 = Y2 + U3
            X2 = X2 - R
          WEND
          T = T + U3
          y1 = y1 + U3
          S = S + 1
        WEND
    WEND

END SUB

SUB Explode (R, Delay) STATIC

    T2 = 4 * R
    T3 = 40 \ R - 1
    T4 = (T3 / 20) + 1

    T = T3
    WHILE T >= 0
       WHILE PDQTimer& < NumTicks&: WEND
       NumTicks& = PDQTimer& + Delay \ 20
       S = 1
       WHILE (T >= 0) AND (S <= T4)
         CALL TransferEGA(T * R, T * T2, 80 - 2 * T * R, T2)
         CALL TransferEGA(T * R, 350 - (T + 1) * T2, 80 - 2 * T * R, T2)
         CALL TransferEGA(T * R, (T + 1) * T2, R, 350 - 2 * (T + 1) * T2)
         CALL TransferEGA(80 - (T + 1) * R, (T + 1) * T2, R, 350 - 2 * (T + 1) * T2)
         T = T - 1
         S = S + 1
       WEND
    WEND


END SUB

SUB Fade (R, Delay) STATIC

   x1 = 0
   y1 = 0
   T = 1
   WHILE T <= 28000
      WHILE PDQTimer& < NumTicks&: WEND
      NumTicks& = PDQTimer& + Delay \ 5
      S = 0
      WHILE (T <= 28000) AND (S <= 175)
        t1 = (t1 + 2433) MOD 28000
        x1 = t1 MOD 80
        y1 = t1 \ 80
        CALL TransferEGA(x1, y1, 1, 1)
        T = T + 1
      WEND
   WEND

END SUB

SUB Implode (R, Delay) STATIC

    T2 = 4 * R
    T3 = 40 \ R - 1
    T4 = (T3 / 20) + 1

    T = 0
    WHILE T <= T3
       WHILE PDQTimer& < NumTicks&: WEND
       NumTicks& = PDQTimer& + Delay \ 20
       S = 1
       WHILE (T <= T3) AND (S <= T4)
         CALL TransferEGA(T * R, T * T2, 80 - 2 * T * R, T2)
         CALL TransferEGA(T * R, 350 - (T + 1) * T2, 80 - 2 * T * R, T2)
         CALL TransferEGA(T * R, (T + 1) * T2, R, 350 - 2 * (T + 1) * T2)
         CALL TransferEGA(80 - (T + 1) * R, (T + 1) * T2, R, 350 - 2 * (T + 1) * T2)
         T = T + 1
         S = S + 1
       WEND
    WEND

END SUB

SUB Instant

  PCOPY 1, 0

END SUB

SUB LargeBoxes (Delay) STATIC

    T = 15
    FOR S = 0 TO 39

        'CALL LineBVE(((T * 8) MOD 64) * 10, (T \ 8) * 70, ((T * 80 + 79) MOD 640), ((T \ 8) + 1) * 70, 15)

       WHILE PDQTimer& < NumTicks&: WEND
       NumTicks& = PDQTimer& + Delay \ 20



        CALL TransferEGA((T * 10) MOD 80, (T \ 8) * 70, 10, 70)
        T = (T + 17) MOD 40
    NEXT

END SUB

SUB LineCrush (R, Delay) STATIC

    IF R = 1 THEN
        T = 0
        WHILE T <= 174
            WHILE PDQTimer& < NumTicks&: WEND
            NumTicks& = PDQTimer& + Delay \ 20
            S = 0
            WHILE (T <= 174) AND (S <= 4)
              CALL TransferEGA(0, T, 80, 2)
              CALL TransferEGA(0, 349 - T, 80, 2)
              T = T + 2
              S = S + 1
            WEND
        WEND
    ELSEIF R = 2 THEN
        T = 0
        WHILE T <= 174
            WHILE PDQTimer& < NumTicks&: WEND
            NumTicks& = PDQTimer& + Delay \ 20
            S = 0
            WHILE (T <= 174) AND (S <= 4)
              CALL TransferEGA(0, T, 80, 1)
              CALL TransferEGA(0, 175 + T, 80, 1)
              T = T + 1
              S = S + 1
            WEND
        WEND
    ELSEIF R = 3 THEN
        T = 0
        WHILE T <= 87
            WHILE PDQTimer& < NumTicks&: WEND
            NumTicks& = PDQTimer& + Delay \ 20
            S = 0
            WHILE (T <= 87) AND (S <= 4)
              CALL TransferEGA(0, T, 80, 1)
              CALL TransferEGA(0, 349 - T, 80, 1)
              CALL TransferEGA(0, 87 + T, 80, 1)
              CALL TransferEGA(0, 262 - T, 80, 1)
              T = T + 1
              S = S + 1
            WEND
        WEND
    ELSEIF R = 4 THEN
        T = 0
        WHILE T <= 87
            WHILE PDQTimer& < NumTicks&: WEND
            NumTicks& = PDQTimer& + Delay \ 20
            S = 0
            WHILE (T <= 87) AND (S <= 4)
              CALL TransferEGA(0, T, 80, 1)
              CALL TransferEGA(0, 87 + T, 80, 1)
              CALL TransferEGA(0, 175 + T, 80, 1)
              CALL TransferEGA(0, 262 + T, 80, 1)
              T = T + 1
              S = S + 1
            WEND
        WEND
    ELSEIF R = 5 THEN
        T = 0
        WHILE T <= 49
            WHILE PDQTimer& < NumTicks&: WEND
            NumTicks& = PDQTimer& + Delay \ 20
            S = 0
            WHILE (T <= 49) AND (S <= 2)
              FOR M = 0 TO 300 STEP 50
                CALL TransferEGA(0, M + T, 80, 1)
              NEXT
              T = T + 1
              S = S + 1
            WEND
        WEND
    ELSEIF R = 6 THEN
        T = 0
        WHILE T <= 24
            WHILE PDQTimer& < NumTicks&: WEND
            NumTicks& = PDQTimer& + Delay \ 20
            S = 0
            WHILE (T <= 24) AND (S <= 1)
              FOR M = 0 TO 325 STEP 25
                CALL TransferEGA(0, M + T, 80, 1)
              NEXT
              T = T + 1
              S = S + 1
            WEND
        WEND
    END IF

END SUB

SUB SlideD

  SlideDown 1, 0, 80, 349, 1, 0

END SUB

SUB SlideFan

   SlideUp 1, 0, 40, 175, 1, 0
   SlideRight 41, 0, 80, 175, 41, 0
   SlideDown 41, 176, 80, 349, 41, 176
   SlideLeft 1, 176, 40, 349, 1, 176

END SUB

SUB SlideL

  SlideLeft 1, 0, 80, 349, 1, 0

END SUB

SUB SlideR

  SlideRight 1, 0, 80, 349, 1, 0

END SUB

SUB SlideU

  SlideUp 1, 0, 80, 349, 1, 0

END SUB

SUB SmallBoxes (R, Delay) STATIC

    IF R = 1 THEN
        t1 = 13: T2 = 18: T3 = 43: T4 = 34
    ELSEIF R = 2 THEN
        t1 = 13: T2 = 18: T3 = 83: T4 = 42
    ELSEIF R = 3 THEN
        t1 = 2: T2 = 13: T3 = 42: T4 = 75
    ELSEIF R = 4 THEN
        t1 = 46: T2 = 13: T3 = 42: T4 = 23
    END IF
    T5 = 5   'Width of boxes in bytes
    T6 = 35  'Height of boxes in pixel lines

    T = 2 * INT(RND * 30) + 1
    S = 0
    WHILE (S <= 39)
        WHILE PDQTimer& < NumTicks&: WEND
        NumTicks& = PDQTimer& + Delay \ 20
        M = 0
        WHILE (S <= 39) AND (M <= 2)
          CALL TransferEGA((T * 5) MOD 80, (T \ 16) * 35, T5, T6)
          T = (T + t1) MOD 160
          CALL TransferEGA((T * 5) MOD 80, (T \ 16) * 35, T5, T6)
          T = (T + T2) MOD 160
          CALL TransferEGA((T * 5) MOD 80, (T \ 16) * 35, T5, T6)
          T = (T + T3) MOD 160
          CALL TransferEGA((T * 5) MOD 80, (T \ 16) * 35, T5, T6)
          T = (T + T4) MOD 160
          S = S + 1
          M = M + 1
        WEND
    WEND

END SUB

SUB Split

  SplitHorizontal 1, 0, 80, 349, 1, 0

END SUB

SUB Wipes (WipeType) STATIC

  R = 4
  Delay = 20

  SELECT CASE WipeType
    CASE 1
      Instant
    CASE 2
      Implode R, Delay
    CASE 3
      Fade R, Delay
    CASE 4
      Crossing R, Delay
    CASE 5
      LargeBoxes Delay
    CASE 6
      SmallBoxes 1, Delay
    CASE 7
      SmallBoxes 2, Delay
    CASE 8
      SmallBoxes 3, Delay
    CASE 9
      SmallBoxes 4, Delay
    CASE 10
      LineCrush 1, Delay
    CASE 11
      LineCrush 2, Delay
    CASE 12
      LineCrush 3, Delay
    CASE 13
      LineCrush 4, Delay
    CASE 14
      LineCrush 5, Delay
    CASE 15
      Explode R, Delay
    CASE 16
      SlideU
    CASE 17
      SlideD
    CASE 18
      SlideR
    CASE 19
      SlideL
    CASE 20
      SlideFan
    CASE 21
      Split
  END SELECT

END SUB

