DECLARE SUB printhelp ()
DECLARE SUB compuplan (P!, A!)
DECLARE FUNCTION compuchooseandpos (dosh!, P!)
DECLARE SUB compumove (P!, A!)
DECLARE SUB GETGRAPHS2 ()
DECLARE SUB START ()
DECLARE SUB FRAME (TOP!, bottom!, LEFT!, RIGHT!)
DECLARE SUB GFRAME (X!, Y!, x1!, y1!)
DECLARE SUB WHOSWON ()
DECLARE FUNCTION READOBJ$ (FILENAME$)
DECLARE SUB QUICKINFO (P!, N!, TURNS!, MOVES!)
DECLARE SUB PIC ()
DECLARE SUB readterain (FILENAME$)
DECLARE SUB posforces (P!)
DECLARE FUNCTION chooseunit (P!, money!)
DECLARE SUB INFO2 (P!, N!, MOVES)
DECLARE SUB combat1 (N!, P)
DECLARE SUB moveunit (P, MAXN)
DECLARE SUB INIT ()
DECLARE FUNCTION SHOW$ (P, N, ok, OK2)
COMMON SHARED col1, col2, TURNS, whichplayer
COMMON SHARED NUMBERTURNS, endseq$
COMMON SHARED armx, army, WHICOBJ, comp1, comp2, modif1, modif2
COMMON SHARED up, down, LEFT, RIGHT, upleft, upright, downright, downleft
COMMON SHARED drawuNit
'-----------------------------------------------------------------------------
'
' VANQUISH V1.0
' By the skillful Cedd and Simon (with entertainment supplied by Ian)
'$DYNAMIC
'-----------------------------------------------------------------------------
DIM SHARED MAXOBJ(2)
DIM SHARED armyuse(2), navyuse(2), aIRUSE(2)
DIM SHARED MAP(1596)
DIM SHARED PLAYER(2, 90, 6)
DIM SHARED LET$(10)
DIM SHARED move(10)
DIM SHARED maxrange(10)
DIM SHARED probhit(10)
DIM SHARED damage(10)
DIM SHARED hitp(10)
DIM SHARED MAXSUP(10)
DIM SHARED maxnum(10)
DIM SHARED cost(10)
DIM SHARED TE2(10)
DIM SHARED SUPPLY(10)
DIM SHARED INAIR(10)
DIM SHARED SHIP(10)
DIM SHARED TERAIN(60, 40)
DIM SHARED movepen(10)
DIM SHARED SHIPONLY(10)
DIM SHARED SUPLYPT(10)
DIM SHARED defbonus(10)
DIM SHARED LANDONLY(10)
DIM SHARED CO(2)
DIM SHARED samcap(10)
DIM SHARED FULL(10) AS STRING * 17
DIM SHARED TER$(10)
DIM SHARED TERFUL(10) AS STRING * 10
DIM SHARED sustain(10)
DIM SHARED TECOL(10)
DIM SHARED WHOSTHERE(60, 40)
DIM SHARED OBJECTIVEX(2, 10, 15)
DIM SHARED OBJECTIVEY(2, 10, 15)
DIM SHARED LANDX(2), LANDY(2), SEAX(2), SEAY(2), AIRX(2), AIRY(2)
DIM SHARED money(2)
DIM SHARED colors&(0 TO 15)
DIM SHARED piccy(345, 72) AS LONG
DIM SHARED units(90, 42) AS LONG
DIM SHARED graph(72) AS STRING
DIM SHARED unt(42) AS STRING
DIM SHARED GRAPHICS(60, 40)
'-----------------------------------------------------------------------------
RANDOMIZE TIMER
KEY 1, "h"
KEY 2, "a"
CO(1) = 1
CO(2) = 4
DEF SEG = 0
POKE &H417, &HF0
colors&(0) = 0
colors&(1) = 5140
colors&(2) = 1315860
colors&(3) = 2631720
colors&(4) = 4144959
colors&(5) = 5120
colors&(6) = 1320960
colors&(7) = 1326100
colors&(8) = 1326120
colors&(9) = 2621440
colors&(10) = 3937300
colors&(11) = 3942420
colors&(12) = 5160
colors&(13) = 5180
colors&(14) = 60
colors&(15) = 1326140
PRINT "Initialising Variables...."
INIT
SCREEN 12
WIDTH , 60
PALETTE
PALETTE USING colors&
GETGRAPHS2
START
IF comp1 = 0 THEN A = chooseunit(1, money(1))
IF comp2 = 0 THEN B = chooseunit(2, money(2))
IF comp1 = 0 THEN posforces 1
IF comp2 = 0 THEN posforces 2
DIM SHARED ORDERSX(90)
DIM SHARED ORDERSY(90)
DIM SHARED help(90)
DIM SHARED KIL(90)
IF comp1 = 1 THEN A = compuchooseandpos(money(1) * modif1, 1)
IF comp2 = 1 THEN B = compuchooseandpos(money(2) * modif2, 2)
SCREEN 12
WIDTH , 60
PALETTE
PALETTE USING colors&
INFO2 1, 1, 1
DO
TURNS = TURNS + 1
IF comp1 = 0 THEN
moveunit 1, A
ELSE
compuplan 1, A
compumove 1, A
END IF
IF comp2 = 0 THEN
moveunit 2, B
ELSE
compuplan 2, B
compumove 2, B
END IF
WHOSWON
LOOP
END

'PT: LOCATE 30, 10: PRINT WHOSTHERE(12, 3)
'RETURN

REM $STATIC
FUNCTION chooseunit (P, money)
CLS
GFRAME 1, 1, 80, 60
DIM army(16)
COLOR 4
GFRAME 31, 3, 48, 6
LOCATE 4, 32: PRINT "VANQUISHER V1.0"
GFRAME 2, 8, 79, 15
COLOR 11
LOCATE 9, 10: PRINT "Player "; P; " Please choose your forces. Funds available = "; money
LOCATE 11, 10: PRINT "Numeric pad cursors 2 & 8 moves cursor up and down."
LOCATE 13, 10: PRINT "Numeric pad + & - changes number in force. ENTER ends selection."
COLOR 4
GFRAME 2, 20, 79, 58
LOCATE 21, 10: PRINT "Name of Unit    Move range          Cost         No in unit "
LOCATE 22, 10: PRINT "                       Resupply time    Attack range     No in force"
curs = 1
min = 1
max = 10
IF navyuse(P) = 0 THEN max = 7
IF armyuse(P) = 0 THEN min = 4
IF aIRUSE(P) = 0 THEN air = 1
COLOR 4
curs = min
COLOR 3
FOR N = 1 TO 10
LOCATE 28 + (N * 2), 10: PRINT FULL(N)
LOCATE 28 + (N * 2), 29: PRINT move(N)
LOCATE 28 + (N * 2), 39: PRINT MAXSUP(N)
LOCATE 28 + (N * 2), 46: PRINT cost(N)
LOCATE 28 + (N * 2), 54: PRINT maxrange(N)
LOCATE 28 + (N * 2), 62: PRINT maxnum(N)
IF N = 2 OR N = 4 OR N = 6 OR N = 8 OR N = 10 THEN TEMP = 26 ELSE TEMP = 0
PUT (17 + TEMP, 215 + N * 16), units(0, ((P - 1) * 20) + N), PSET
NEXT N
COLOR 4
FOR N = min TO max
LOCATE 28 + (N * 2), 10: PRINT FULL(N)
LOCATE 28 + (N * 2), 29: PRINT move(N)
LOCATE 28 + (N * 2), 39: PRINT MAXSUP(N)
LOCATE 28 + (N * 2), 46: PRINT cost(N)
LOCATE 28 + (N * 2), 54: PRINT maxrange(N)
LOCATE 28 + (N * 2), 62: PRINT maxnum(N)
NEXT N
IF air THEN
COLOR 3
FOR N = 5 TO 7
LOCATE 28 + (N * 2), 10: PRINT FULL(N)
LOCATE 28 + (N * 2), 29: PRINT move(N)
LOCATE 28 + (N * 2), 39: PRINT MAXSUP(N)
LOCATE 28 + (N * 2), 46: PRINT cost(N)
LOCATE 28 + (N * 2), 54: PRINT maxrange(N)
LOCATE 28 + (N * 2), 62: PRINT maxnum(N)
NEXT N
END IF
COLOR 14
LOCATE 52, 60: PRINT "Total Cost = "; tot
DO
COLOR 4
FOR N = 1 TO 10
ok = 0
IF N < min OR N > max THEN ok = 1
IF air THEN
 IF N = 5 OR N = 6 OR N = 7 THEN ok = 1
END IF
IF ok = 1 THEN COLOR 3 ELSE COLOR 4
LOCATE 28 + (N * 2), 69: PRINT army(N); "   "
NEXT N
IF air THEN
COLOR 3
FOR N = 5 TO 7
LOCATE 28 + (N * 2), 69: PRINT army(N)
NEXT N
END IF
COLOR 14
LOCATE 28 + (curs * 2), 69: PRINT army(curs); "   "
A$ = INPUT$(1)
IF A$ = "8" THEN curs = curs - 1
IF A$ = "2" THEN curs = curs + 1
IF air THEN
IF curs = 5 THEN curs = 8
IF curs = 7 THEN curs = 4
END IF
IF curs = min - 1 THEN curs = max
IF curs = max + 1 THEN curs = min
IF air THEN
IF curs = 5 THEN curs = 8
IF curs = 7 THEN curs = 4
END IF

IF A$ = "+" THEN army(curs) = army(curs) + 1
IF A$ = "-" THEN army(curs) = army(curs) - 1
IF army(curs) < 0 THEN army(curs) = 0
IF A$ = CHR$(13) THEN EXIT DO
tot = 0
FOR N = min TO max
tot = tot + (army(N) * cost(N))
NEXT N
IF tot > money THEN army(curs) = army(curs) - 1
LOCATE 52, 60: PRINT "Total Cost = "; tot
LOOP


FOR N = 1 TO 10
FOR t = 1 TO army(N)
IF army(N) = 0 THEN EXIT FOR
count = count + 1
PLAYER(P, count, 3) = maxnum(N)
PLAYER(P, count, 4) = N
PLAYER(P, count, 5) = MAXSUP(N)
IF count > 89 THEN EXIT FUNCTION
NEXT t, N
chooseunit = count
END FUNCTION

SUB combat1 (P, N)
A$ = SHOW$(P, N, 0, 1)
X = 0
Y = 0
MAXX = maxrange(PLAYER(P, N, 4))
MAXY = maxrange(PLAYER(P, N, 4))
MINX = -maxrange(PLAYER(P, N, 4))
MINY = -maxrange(PLAYER(P, N, 4))
x1 = PLAYER(P, N, 1)
y1 = PLAYER(P, N, 2)
X2 = x1 - 5
Y2 = y1 - 5
IF X2 < 1 THEN X2 = 1
IF Y2 < 1 THEN Y2 = 1
IF Y2 > 31 THEN Y2 = 31
IF X2 > 51 THEN X2 = 51
'-------------------------POSITIONING SQUARE OF ATTACK------------------------
DO
  X3 = 160 + ((PLAYER(P, N, 1) - X2) * 48)
  y3 = (PLAYER(P, N, 2) - Y2) * 48
  IF SUPPLY(PLAYER(P, N, 4)) = 1 THEN
    PUT (X3 + (X * 48), y3 + (Y * 48)), units(0, 42), PSET
  ELSE
    PUT (X3 + (X * 48), y3 + (Y * 48)), units(0, 41), PSET
END IF
A$ = INPUT$(1)
  PUT (X3 + (X * 48), y3 + (Y * 48)), piccy(0, GRAPHICS(x1 + X, y1 + Y)), PSET
  IF WHOSTHERE(x1 + X, y1 + Y) <> 0 THEN
  i = INT(WHOSTHERE(x1 + X, y1 + Y) / 100)
  g = WHOSTHERE(x1 + X, y1 + Y) - (i * 100)
  PUT (10 + X3 + (X * 48), 10 + y3 + (Y * 48)), units(0, PLAYER(i, g, 4) + PLAYER(i, g, 6) + (i - 1) * 20), PSET
  END IF
  IF A$ = "5" THEN EXIT DO
  IF A$ = "1" THEN Y = Y + 1: X = X - 1
  IF A$ = "2" THEN Y = Y + 1
  IF A$ = "3" THEN Y = Y + 1: X = X + 1
  IF A$ = "4" THEN X = X - 1
  IF A$ = "6" THEN X = X + 1
  IF A$ = "7" THEN Y = Y - 1: X = X - 1
  IF A$ = "8" THEN Y = Y - 1
  IF A$ = "9" THEN Y = Y - 1: X = X + 1
  IF X > MAXX THEN X = MAXX
  IF X < MINX THEN X = MINX
  IF Y > MAXY THEN Y = MAXY
  IF Y < MINY THEN Y = MINY
  IF X + PLAYER(P, N, 1) > 60 THEN X = X - 1
  IF X + PLAYER(P, N, 1) < 1 THEN X = X + 1
  IF Y + PLAYER(P, N, 2) > 40 THEN Y = Y - 1
  IF Y + PLAYER(P, N, 2) < 1 THEN Y = Y + 1
  LOOP
'-----------------------------------------------------------------------------
'------------------------------DECIDING DAMAGE--------------------------------
  HP = 0
  IF WHOSTHERE(x1 + X, y1 + Y) <> 0 THEN
    TRUE = 1
    PN = INT(WHOSTHERE(x1 + X, y1 + Y) / 100)
    O = WHOSTHERE(x1 + X, y1 + Y) - (PN * 100)
    IF INAIR(PLAYER(P, N, 4)) = 1 AND INAIR(PLAYER(PN, O, 4)) = 0 AND samcap(PLAYER(P, N, 4)) = 0 THEN
        TRUE = 0
        LINE (200, 100)-(430, 380), 0, BF
        LINE (200, 100)-(430, 380), 4, B
        LINE (202, 102)-(428, 378), 3, B
        LINE (204, 104)-(426, 376), 2, B
        COLOR 4
        LOCATE 16, 35: PRINT "COMBAT NOTE"
        LOCATE 19, 27: PRINT "Your shot did no damage"
        LOCATE 21, 27: PRINT "because you are firing "
        LOCATE 23, 27: PRINT "from and aircraft that "
        LOCATE 25, 27: PRINT "cannot attack ground "
        LOCATE 27, 27: PRINT "targets. Your aircraft"
        LOCATE 29, 27: PRINT "can only attack other"
        LOCATE 31, 27: PRINT "airborne opponents."
        LOCATE 35, 27: PRINT "  Press any Key"
        A$ = INPUT$(1)
    END IF
    IF INAIR(PLAYER(P, N, 4)) = 0 AND INAIR(PLAYER(PN, O, 4)) = 1 AND samcap(PLAYER(P, N, 4)) = 0 THEN
    TRUE = 0
        LINE (200, 100)-(430, 380), 0, BF
        LINE (200, 100)-(430, 380), 4, B
        LINE (202, 102)-(428, 378), 3, B
        LINE (204, 104)-(426, 376), 2, B
        COLOR 4
        LOCATE 16, 35: PRINT "COMBAT NOTE"
        LOCATE 19, 27: PRINT "Your shot did no damage"
        LOCATE 21, 27: PRINT "because you are firing "
        LOCATE 23, 27: PRINT "at an airborne target"
        LOCATE 25, 27: PRINT "and your troops have"
        LOCATE 27, 27: PRINT "no S.A.M capability."
        LOCATE 29, 27: PRINT "To take out air targets"
        LOCATE 31, 27: PRINT "use other planes or "
        LOCATE 33, 27: PRINT "specialized infantry."
        LOCATE 37, 27: PRINT "   Press any Key"
        A$ = INPUT$(1)
     END IF
    IF TRUE THEN
    FOR t = 1 TO PLAYER(P, N, 3)
    IF RND < probhit(PLAYER(P, N, 4)) THEN HP = HP + damage(PLAYER(P, N, 4))
    NEXT t
    killed = HP / hitp(PLAYER(PN, O, 4)) / defbonus(TERAIN(PLAYER(PN, O, 1), PLAYER(PN, O, 2)))
    PLAYER(PN, O, 3) = PLAYER(PN, O, 3) - killed
    IF PLAYER(PN, O, 3) < 1 THEN
        WHOSTHERE(PLAYER(PN, O, 1), PLAYER(PN, O, 2)) = 0
        PUT (X3 + (X * 48), y3 + (Y * 48)), piccy(0, GRAPHICS(x1 + X, y1 + Y)), PSET
    END IF
    END IF
  END IF
'-----------------------------------------------------------------------------
'--------------------SUPPLYING VEHICULES--------------------------------------
 TRUE = 1
 IF SUPPLY(PLAYER(P, N, 4)) = 1 AND WHOSTHERE(x1 + X, y1 + Y) <> 0 THEN
    PN = INT(WHOSTHERE(x1 + X, y1 + Y) / 100)
    O = VAL(RIGHT$(STR$(WHOSTHERE(x1 + X, y1 + Y)), 2))
  IF INAIR(PLAYER(PN, O, 4)) = 1 AND INAIR(PLAYER(P, N, 4)) = 0 THEN
   TRUE = 0
        LINE (200, 100)-(430, 380), 0, BF
        LINE (200, 100)-(430, 380), 4, B
        LINE (202, 102)-(428, 378), 3, B
        LINE (204, 104)-(426, 376), 2, B
        COLOR 4
        LOCATE 16, 35: PRINT "COMBAT NOTE"
        LOCATE 19, 27: PRINT "You cannot refuel aircraft"
        LOCATE 21, 27: PRINT "from the ground. To refuel"
        LOCATE 23, 27: PRINT "aircraft special refueling"
        LOCATE 25, 27: PRINT "aircraft must be used.  "
        LOCATE 29, 27: PRINT "   Press any key"
        A$ = INPUT$(1)
        g$ = SHOW$(P, N, 0, 1)
  END IF
  IF INAIR(PLAYER(PN, O, 4)) = 0 AND INAIR(PLAYER(P, N, 4)) = 1 THEN
  TRUE = 0
        LINE (200, 100)-(430, 380), 0, BF
        LINE (200, 100)-(430, 380), 4, B
        LINE (202, 102)-(428, 378), 3, B
        LINE (204, 104)-(426, 376), 2, B
        COLOR 4
        LOCATE 16, 33: PRINT "MOVEMENT NOTE"
        LOCATE 19, 27: PRINT "You are attempting to"
        LOCATE 21, 27: PRINT "resupply a ground unit  "
        LOCATE 23, 27: PRINT "from a airborne unit."
        LOCATE 25, 27: PRINT "Only ground units can    "
        LOCATE 27, 27: PRINT "resupply other ground "
        LOCATE 29, 27: PRINT "units.            "
        LOCATE 31, 27: PRINT "   Press any key"
        A$ = INPUT$(1)
        g$ = SHOW$(P, N, 0, 1)
 END IF
  IF TRUE = 1 THEN
  PLAYER(P, N, 5) = PLAYER(P, N, 5) - MAXSUP(PLAYER(P, O, 4)) + PLAYER(PN, O, 5)
  PLAYER(PN, O, 5) = MAXSUP(PLAYER(PN, O, 4))
 END IF
 END IF
'-----------------------------------------------------------------------------
END SUB

FUNCTION compuchooseandpos (dosh, P)
IF P = 1 THEN max = 30: min = 1
IF P = 2 THEN max = 60: min = 31
FOR N = 1 TO 10
IF OBJECTIVEX(P, N, 1) < max AND OBJECTIVEX(P, N, 1) > min THEN count = count + 1
NEXT N
IF count > 0 THEN
FOR N = 1 TO count
IF armyuse(P) AND totcost < dosh THEN
C = C + 1
PLAYER(P, C, 1) = OBJECTIVEX(P, N, 1)
PLAYER(P, C, 2) = OBJECTIVEY(P, N, 1)
PLAYER(P, C, 3) = maxnum(3)
PLAYER(P, C, 4) = 3
PLAYER(P, C, 5) = MAXSUP(3)
totcost = totcost + cost(3)
ORDERSX(C) = OBJECTIVEX(P, N, 1)
ORDERSY(C) = OBJECTIVEY(P, N, 1)
END IF
IF aIRUSE(P) AND totcost < dosh THEN
C = C + 1
PLAYER(P, C, 1) = OBJECTIVEX(P, N, 2)
PLAYER(P, C, 2) = OBJECTIVEY(P, N, 2)
PLAYER(P, C, 3) = maxnum(6)
PLAYER(P, C, 4) = 6
PLAYER(P, C, 5) = MAXSUP(6)
totcost = totcost + cost(6)
ORDERSX(C) = OBJECTIVEX(P, N, 2)
ORDERSY(C) = OBJECTIVEY(P, N, 2)
END IF
NEXT N
END IF
dist = 50
FOR N = 1 TO 10
IF P = 1 THEN
IF OBJECTIVEX(P, N, 1) > 30 THEN
IF OBJECTIVEX(P, N, 1) - 30 < dist THEN dist = OBJECTIVEX(P, N, 1) - 30: CIT = N
END IF
ELSE
IF OBJECTIVEX(P, N, 1) < 30 THEN
IF 30 - OBJECTIVEX(P, N, 1) < dist THEN dist = 30 - OBJECTIVEX(P, N, 1): CIT = N
END IF
END IF
NEXT N
IF CIT = 0 THEN
        DO
        IF armyuse(P) AND totcost < dosh THEN
        C = C + 1
        PLAYER(P, C, 1) = OBJECTIVEX(P, (C MOD count) + 1, 1)
        PLAYER(P, C, 2) = OBJECTIVEY(P, (C MOD count) + 1, 1)
        PLAYER(P, C, 3) = maxnum(3)
        PLAYER(P, C, 4) = 3
        PLAYER(P, C, 5) = MAXSUP(3)
        totcost = totcost + cost(3)
        ORDERSX(C) = OBJECTIVEX(P, (C MOD count) + 1, 1)
        ORDERSY(C) = OBJECTIVEY(P, (C MOD count) + 1, 1)
        END IF
        IF aIRUSE(P) AND totcost < dosh THEN
        C = C + 1
        PLAYER(P, C, 1) = OBJECTIVEX(P, (C MOD count) + 1, 1)
        PLAYER(P, C, 2) = OBJECTIVEY(P, (C MOD count) + 1, 1)
        PLAYER(P, C, 3) = maxnum(6)
        PLAYER(P, C, 4) = 6
        PLAYER(P, C, 5) = MAXSUP(6)
        totcost = totcost + cost(6)
        ORDERSX(C) = OBJECTIVEX(P, (C MOD count) + 1, 1)
        ORDERSY(C) = OBJECTIVEY(P, (C MOD count) + 1, 1)
        END IF
        LOOP UNTIL totcost >= dosh
ELSE
        Y = OBJECTIVEY(P, CIT, 1)
        X = 30
        IF SHIPONLY(TERAIN(X, Y)) = 0 THEN
                LX = X: LY = Y: AX = X: AY = Y
        ELSE
                AX = X
                AY = Y
        END IF
        IF LX = 0 THEN
        FOR N = 1 TO 40
        IF SHIPONLY(TERAIN(N, 30)) = 0 THEN
         IF ABS(X - N) < dist THEN dist = ABS(X - N): LX = 30: LY = N
        END IF
        NEXT N
        END IF
        DO
        IF aIRUSE(P) = 0 AND totcost < dosh THEN
        C = C + 1
        DO
        IF LY + C - TEMP > 38 THEN
          IF P = 1 THEN LX = LX - 1 ELSE LX = LX + 1
          TEMP = TEMP + C
          TEMP2 = 0
        END IF
        IF SHIPONLY(TERAIN(LX, LY + C - TEMP + TEMP2)) = 1 THEN TEMP2 = TEMP2 + 1
        LOOP UNTIL SHIPONLY(TERAIN(LX, LY + C - TEMP + TEMP2)) = 1
        PLAYER(P, C, 1) = LX
        PLAYER(P, C, 2) = LY + C - TEMP
        PLAYER(P, C, 3) = maxnum(2)
        PLAYER(P, C, 4) = 2
        PLAYER(P, C, 5) = MAXSUP(2)
        totcost = totcost + cost(2)
        IF MAXOBJ(P) = 1 THEN
                ORDERSX(C) = OBJECTIVEX(P, 1, 1)
                ORDERSY(C) = OBJECTIVEY(P, 1, 1)
        END IF
        C = C + 1
        PLAYER(P, C, 1) = LX
        PLAYER(P, C, 2) = LY + C - TEMP
        PLAYER(P, C, 3) = maxnum(4)
        PLAYER(P, C, 4) = 4
        PLAYER(P, C, 5) = MAXSUP(4)
        KIL(C) = C - 1
        totcost = totcost + cost(4)
        END IF
        IF aIRUSE(P) AND totcost < dosh THEN
        IF AY + C - TEMP1 > 38 THEN
                IF P = 1 THEN AX = AX - 1: AX = AX + 1
                TEMP1 = TEMP1 + C
        END IF
        C = C + 1
        PLAYER(P, C, 1) = AX
        PLAYER(P, C, 2) = AY + C - TEMP1
        PLAYER(P, C, 3) = maxnum(5)
        PLAYER(P, C, 4) = 5
        PLAYER(P, C, 5) = MAXSUP(5)
        IF MAXOBJ(P) = 1 THEN
                ORDERSX(C) = OBJECTIVEX(P, 1, 1)
                ORDERSY(C) = OBJECTIVEY(P, 1, 1)
        END IF
        totcost = totcost + cost(5)
        C = C + 1
        PLAYER(P, C, 1) = AX - 1
        PLAYER(P, C, 2) = AY + C - TEMP1 + 1
        PLAYER(P, C, 3) = maxnum(7)
        PLAYER(P, C, 4) = 7
        PLAYER(P, C, 5) = MAXSUP(7)
        totcost = totcost + cost(7)
        KIL(C) = C - 1
        END IF
        LOOP UNTIL dosh <= totcost
END IF
N = 0
DO
N = N + 1
A = OBJECTIVEX(P, CIT, N)
IF A = 0 THEN EXIT DO
LOOP
armx = OBJECTIVEX(P, CIT, N / 2)
army = OBJECTIVEY(P, CIT, N / 2)
WHICOBJ = CIT
compuchooseandpos = C
END FUNCTION

SUB compumove (P, A)
FOR N = 1 TO A STEP 1
MOVES = 0
IF PLAYER(P, N, 5) < 2 THEN help(N) = 1
IF PLAYER(P, N, 3) >= 1 AND PLAYER(P, N, 5) > 0 THEN
DO
'----------Go To  objective and Kick anything that moves------------------
'----------------Check for eligible opponents in range------------------------
IF P = 1 THEN PN = 2 ELSE PN = 1
OBJX = armx
OBJY = army
IF ORDERSX(N) <> 0 THEN OBJX = ORDERSX(N): OBJY = ORDERSY(N)
IF KIL(N) <> 0 AND SUPPLY(PLAYER(P, N, 4)) = 0 THEN OBJX = PLAYER(PN, KIL(N), 1): OBJY = PLAYER(PN, KIL(N), 2)
IF KIL(N) <> 0 AND SUPPLY(PLAYER(P, N, 4)) = 1 THEN OBJX = PLAYER(P, KIL(N), 1): OBJY = PLAYER(P, KIL(N), 2)
MINX = PLAYER(P, N, 1) - maxrange(PLAYER(P, N, 4))
MAXX = PLAYER(P, N, 1) + maxrange(PLAYER(P, N, 4))
MINY = PLAYER(P, N, 2) - maxrange(PLAYER(P, N, 4))
MAXY = PLAYER(P, N, 2) + maxrange(PLAYER(P, N, 4))
IF MINX < 1 THEN MINX = 1
IF MAXX > 60 THEN MAXX = 60
IF MINY < 1 THEN MINY = 1
IF MAXY > 40 THEN MAXY = 40
IF SUPPLY(PLAYER(P, N, 4)) = 0 THEN
        FOR S = MINX TO MAXX
        FOR t = MINY TO MAXY
        IF WHOSTHERE(S, t) <> 0 THEN
                PN = INT(WHOSTHERE(S, t) / 100)
                IF PN <> P THEN
                        TRUE = 1
                        NK = WHOSTHERE(S, t) - (PN * 100)
                        IF INAIR(PLAYER(P, N, 4)) = 1 AND INAIR(PLAYER(PN, NK, 4)) = 0 AND samcap(PLAYER(P, N, 4)) = 0 THEN TRUE = 0
                        IF INAIR(PLAYER(P, N, 4)) = 0 AND INAIR(PLAYER(PN, NK, 4)) = 1 AND samcap(PLAYER(P, N, 4)) = 0 THEN TRUE = 0
                        IF TRUE THEN
                                FOR t = 1 TO PLAYER(P, N, 3)
                                IF RND < probhit(PLAYER(P, N, 4)) THEN HP = HP + damage(PLAYER(P, N, 4))
                                NEXT t
                                killed = HP / hitp(PLAYER(PN, NK, 4)) / defbonus(TERAIN(PLAYER(PN, NK, 1), PLAYER(PN, NK, 2)))
                                PLAYER(PN, NK, 3) = PLAYER(PN, NK, 3) - killed
                                IF PLAYER(PN, NK, 3) < 1 THEN WHOSTHERE(PLAYER(PN, NK, 1), PLAYER(PN, NK, 2)) = 0
                                EXIT DO
                        END IF
                END IF
        END IF
        NEXT t
        NEXT S
        ELSE
        FOR S = MINX TO MAXX
        FOR t = MINY TO MAXY
        IF WHOSTHERE(S, t) <> 0 THEN
                PN = INT(WHOSTHERE(S, t) / 100)
                IF PN = P THEN
                        TRUE = 1
                        O = WHOSTHERE(S, t) - (PN * 100)
                        IF INAIR(PLAYER(P, N, 4)) = 1 AND INAIR(PLAYER(PN, O, 4)) = 0 AND samcap(PLAYER(P, N, 4)) = 0 THEN TRUE = 0
                        IF INAIR(PLAYER(P, N, 4)) = 0 AND INAIR(PLAYER(PN, O, 4)) = 1 AND samcap(PLAYER(P, N, 4)) = 0 THEN TRUE = 0
                        IF SUPPLY(PLAYER(P, O, 4)) = 1 THEN TRUE = 0
                        IF KIL(N) <> 0 THEN IF O <> KIL(N) THEN TRUE = 0
                        IF TRUE AND PLAYER(P, N, 5) < MAXSUP(PLAYER(P, N, 4)) THEN
                                PLAYER(P, N, 5) = PLAYER(P, N, 5) - MAXSUP(PLAYER(P, O, 4)) + PLAYER(P, O, 5)
                                PLAYER(PN, O, 5) = MAXSUP(PLAYER(PN, O, 4))
                                EXIT DO
                        END IF
                END IF
        END IF
        NEXT t
        NEXT S
        END IF
'----------Still in Loop so haven't killed anything, Therefore move-----------
        'G$ = SHOW$(P, n, 0, 0)
        IF WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = N + (P * 100) THEN WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = 0
        IF PLAYER(P, N, 1) > OBJX THEN X = -1 ELSE X = 1
        IF PLAYER(P, N, 2) > OBJY THEN Y = -1 ELSE Y = 1
        IF PLAYER(P, N, 1) = OBJX THEN X = 0
        IF PLAYER(P, N, 2) = OBJY THEN Y = 0
        '--------------find somewhere to move---------------
        count = 0
        DO
        MOVED = 1
        IF PLAYER(P, N, 1) + X < 1 THEN MOVED = 0
        IF PLAYER(P, N, 1) + X > 60 THEN MOVED = 0
        IF PLAYER(P, N, 2) + Y < 1 THEN MOVED = 0
        IF PLAYER(P, N, 2) + Y > 40 THEN MOVED = 0
        IF MOVED = 1 THEN
        IF SHIPONLY(TERAIN(PLAYER(P, N, 1) + X, PLAYER(P, N, 2) + Y)) = 1 AND SHIP(PLAYER(P, N, 4)) = 0 AND INAIR(PLAYER(P, N, 4)) = 0 THEN MOVED = 0
        IF LANDONLY(TERAIN(PLAYER(P, N, 1) + X, PLAYER(P, N, 2) + Y)) = 1 AND SHIP(PLAYER(P, N, 4)) = 1 AND INAIR(PLAYER(P, N, 4)) = 0 THEN MOVED = 0
        WHO = INT(WHOSTHERE(PLAYER(P, N, 1) + X, PLAYER(P, N, 2) + Y) / 100)
        IF WHO <> 0 THEN MOVED = 0
        END IF
        IF X = -1 THEN PLAYER(P, N, 6) = 10
        IF X = 1 THEN PLAYER(P, N, 6) = 0
        IF MOVED = 0 THEN
                count = count + 1
                IF X = 1 AND Y = 1 THEN U = 0:  t = 1
                IF X = 1 AND Y = 0 THEN U = 1: t = 1
                IF X = 1 AND Y = -1 THEN U = 1: t = 0
                IF X = 0 AND Y = 1 THEN U = -1: t = 1
                IF X = 0 AND Y = -1 THEN U = 1: t = -1
                IF X = -1 AND Y = 1 THEN U = -1: t = 0
                IF X = -1 AND Y = 0 THEN U = -1: t = -1
                IF X = -1 AND Y = -1 THEN U = 0: t = -1
        X = U
        Y = t
        END IF
        LOOP UNTIL MOVED = 1 OR count = 8
        IF SUPLYPT(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 1 THEN PLAYER(P, N, 5) = MAXSUP(PLAYER(P, N, 4))
        IF MOVED = 1 THEN
        PLAYER(P, N, 1) = PLAYER(P, N, 1) + X
        PLAYER(P, N, 2) = PLAYER(P, N, 2) + Y
        END IF
        WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = N + (P * 100)
        'G$ = SHOW$(P, n, 0, 0)
        IF INAIR(PLAYER(P, N, 4)) THEN MOVES = MOVES + 1 ELSE MOVES = MOVES + movepen(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
        IF INAIR(PLAYER(P, N, 4)) = 0 THEN PLAYER(P, N, 5) = PLAYER(P, N, 5) - 1
LOOP UNTIL MOVES >= move(PLAYER(P, N, 4))
IF INAIR(PLAYER(P, N, 4)) = 1 THEN PLAYER(P, N, 5) = PLAYER(P, N, 5) - 1
IF INAIR(PLAYER(P, N, 4)) = 1 AND PLAYER(P, N, 5) = 0 THEN PLAYER(P, N, 3) = -1: WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = 0
END IF
NEXT N
END SUB

SUB compuplan (P, A)
'-----HAVE WE TAKEN TARGET CITY-----------------------------------------------
TRUE = 1
FOR N = 1 TO 10
IF OBJECTIVEX(P, WHICOBJ, N) = 0 OR OBJECTIVEX(P, WHICOBJ, N) = -1 THEN
        max = N
        EXIT FOR
ELSE
        IF INT(WHOSTHERE(OBJECTIVEX(P, WHICOBJ, N), OBJECTIVEY(P, WHICOBJ, N)) / 100) <> P AND WHOSTHERE(OBJECTIVEX(P, WHICOBJ, N), OBJECTIVEY(P, WHICOBJ, N)) <> 0 THEN TRUE = 0
END IF
NEXT N
IF TRUE = 1 THEN
'---------------Taken city, move on to next ----------------------------------
IF MAXOBJ(P) <> 1 THEN
FOR N = 1 TO A
IF ORDERSX(N) = 0 AND KIL(N) = 0 AND SUPPLY(PLAYER(P, N, 4)) = 0 THEN
        ORDERSX(N) = OBJECTIVEX(P, WHICOBJ, max / 2)
        ORDERSY(N) = OBJECTIVEY(P, WHICOBJ, max / 2)
        C = C + 1
END IF
IF C = 2 THEN EXIT FOR
NEXT N
WHICOBJ = ((WHICOBJ + 1) MOD MAXOBJ(P))
DO
J = J + 1
IF OBJECTIVEX(P, WHICOBJ, J) = 0 THEN EXIT DO
LOOP
armx = OBJECTIVEX(P, WHICOBJ, J / 2)
army = OBJECTIVEY(P, WHICOBJ, J / 2)
END IF
END IF
IF P = 1 THEN PN = 2 ELSE PN = 1
FOR N = 1 TO A
IF help(N) = 1 AND PLAYER(P, N, 3) < 1 THEN help(N) = 0
NEXT N
FOR N = 1 TO A
IF SUPPLY(PLAYER(P, N, 4)) = 0 THEN U = PN ELSE U = P
IF KIL(N) <> 0 AND PLAYER(U, KIL(N), 3) < 1 THEN
        IF SUPPLY(PLAYER(P, N, 4)) = 0 THEN
                KIL(N) = 0
        ELSE
                KIL(N) = 0
                FOR J = 1 TO A
                IF help(J) = 1 THEN KIL(N) = J
                NEXT J
        END IF
END IF
NEXT N
END SUB

SUB FRAME (TOP, bottom, LEFT, RIGHT)
horiz = RIGHT - LEFT - 1
    hine$ = STRING$(horiz, 196)
LOCATE TOP, LEFT: PRINT CHR$(218); hine$
LOCATE TOP, RIGHT: PRINT CHR$(191)
LOCATE bottom, LEFT: PRINT CHR$(192); hine$
LOCATE bottom, RIGHT: PRINT CHR$(217)

FOR vert = TOP + 1 TO bottom - 1
    LOCATE vert, LEFT: PRINT CHR$(179); SPACE$(horiz); CHR$(179)
    'LOCATE vert, right: PRINT CHR$(179)
NEXT vert

END SUB

SUB GETGRAPHS2
PRINT "Loading graphics files..."
FOR N = 1 TO 72
DEF SEG = VARSEG(piccy(0, N))
BLOAD graph(N), VARPTR(piccy(0, N))
NEXT N
FOR N = 1 TO 42
DEF SEG = VARSEG(units(0, N))
BLOAD unt(N), VARPTR(units(0, N))
NEXT N
END SUB

SUB GFRAME (X, Y, x1, y1)
TOP = (Y - 1) * 8
LFT = (X - 1) * 8
BOT = (y1 - 1) * 8
RGHT = (x1 - 1) * 8
LINE (LFT, TOP)-(RGHT, BOT), 4, B
LINE (LFT + 1, TOP + 1)-(RGHT - 1, BOT - 1), 4, B
LINE (LFT + 2, TOP + 2)-(RGHT - 2, BOT - 2), 3, B
LINE (LFT + 3, TOP + 3)-(RGHT - 3, BOT - 3), 3, B
LINE (LFT + 4, TOP + 4)-(RGHT - 4, BOT - 4), 2, B
LINE (LFT + 5, TOP + 5)-(RGHT - 5, BOT - 5), 2, B
LINE (LFT + 6, TOP + 6)-(RGHT - 6, BOT - 6), 0, BF
END SUB

SUB INFO2 (P, N, MOVES)
'COLOR 4
'frame 1, 58, 1, 20
LINE (1, 1)-(160, 480), 4, B
LINE (3, 3)-(158, 478), 3, B
LINE (5, 5)-(156, 476), 2, B
COLOR 12
LOCATE 2, 2
PRINT "  VANQUISHER V1.0"
COLOR 15
LOCATE 4, 2
PRINT STRING$(18, 196)
LOCATE 5, 2
PRINT "Forces: ";
COLOR 12: PRINT "Player "; P
COLOR 15
LOCATE 6, 2
PRINT STRING$(18, 196)
LOCATE 8, 2
PRINT "Type of unit:";
COLOR 12
LOCATE 9, 2
PRINT " "; FULL(PLAYER(P, N, 4))
COLOR 15
LOCATE 10, 2
PRINT "Range:";
LOCATE 11, 4
COLOR 12: PRINT maxrange(PLAYER(P, N, 4))
COLOR 15
LOCATE 12, 2
PRINT "Moves left:";
LOCATE 13, 4
COLOR 12: PRINT MOVES
COLOR 15
LOCATE 14, 2
PRINT "No. left in unit:"
COLOR 12
LOCATE 15, 4
PRINT INT(PLAYER(P, N, 3))
LOCATE 16, 2
COLOR 15
PRINT "Supply status:";
LOCATE 17, 4
COLOR 12: PRINT PLAYER(P, N, 5)
COLOR 15
LOCATE 18, 2
PRINT STRING$(18, 196)
LOCATE 19, 2
PRINT "Terrain ";
COLOR 12: PRINT TERFUL(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
COLOR 15
LOCATE 20, 2
PRINT STRING$(18, 196)
LOCATE 22, 2
PRINT "Defence bonus:";
LOCATE 23, 4
COLOR 12: PRINT defbonus(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 24, 2
COLOR 15
PRINT "Movement cost:";
LOCATE 25, 4
COLOR 12: PRINT movepen(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 26, 2
COLOR 15
PRINT "Supply point?";
LOCATE 27, 4
COLOR 12: IF SUPPLY(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 1 THEN PRINT " Yes" ELSE PRINT " No "
COLOR 15
LOCATE 28, 2
PRINT "Suitable for Navy?"
COLOR 12
LOCATE 29, 4
IF LANDONLY(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 30, 2
COLOR 15
PRINT "Suitable for Army?"
COLOR 12
LOCATE 31, 4
IF SHIPONLY(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
COLOR 15
LOCATE 32, 2
        PRINT "Turn:";
LOCATE 33, 4
COLOR 12: PRINT TURNS
COLOR 6
LOCATE 34, 2: PRINT STRING$(18, 196)
LOCATE 35, 2: PRINT "Instructions"
LOCATE 36, 2: PRINT STRING$(18, 196)
LOCATE 38, 2: PRINT "Cursor pad moves."
LOCATE 39, 2: PRINT "5 ends movement."
LOCATE 40, 2: PRINT "0 attacks ."
LOCATE 41, 2: PRINT "When attacking "
LOCATE 42, 2: PRINT "position line of"
LOCATE 43, 2: PRINT "attack with cursor"
LOCATE 44, 2: PRINT "pad."
LOCATE 46, 2: PRINT "    F1 = Help"
LOCATE 47, 2: PRINT "   F2 = Options"
PUT (20, 385), MAP, PSET
IF drawuNit THEN
FOR t = 1 TO 2
FOR S = 1 TO 90
IF PLAYER(t, S, 3) > 0 THEN LINE (20 + ((PLAYER(t, S, 1) - 1) * 2), (2 * (PLAYER(t, S, 2) - 1)) + 386)-STEP(2, 2), 0 + (t - 1) * 4, BF
NEXT S, t
END IF
END SUB

SUB INIT
drawuNit = 1
up = 72
RIGHT = 77
down = 80
LEFT = 75
upright = 73
upleft = 71
downleft = 79
downright = 81
'descriptions of terrain -----------------------------------------------------
TERFUL(7) = "Sea/Lake"
TECOL(7) = 9
TE2(7) = 1
TER$(7) = "S"
movepen(7) = 1
defbonus(7) = 1
SUPLYPT(7) = 0
LANDONLY(7) = 0
SHIPONLY(7) = 1
sustain(7) = 1
TERFUL(6) = "River"
TE2(6) = 9
TECOL(6) = 10
TER$(6) = "R"
movepen(6) = 2
defbonus(6) = 2
SUPLYPT(6) = 0
LANDONLY(6) = 1
SHIPONLY(6) = 0
sustain(6) = 1
TERFUL(10) = "Urbanised"
TE2(10) = 7
TECOL(10) = 3
TER$(10) = "U"
movepen(10) = .5
defbonus(10) = 3
SUPLYPT(10) = 1
LANDONLY(10) = 1
SHIPONLY(10) = 0
sustain(10) = 1
TERFUL(9) = "Desert"
TE2(9) = 14
TECOL(9) = 15
TER$(9) = "D"
movepen(9) = 1
defbonus(9) = 1
SUPLYPT(9) = 0
LANDONLY(9) = 1
SHIPONLY(9) = 0
sustain(9) = 0
TERFUL(3) = "Woodland"
TE2(3) = 2
TECOL(3) = 5
TER$(3) = "W"
movepen(3) = 2
defbonus(3) = 2
SUPLYPT(3) = 0
LANDONLY(3) = 1
SHIPONLY(3) = 0
sustain(3) = 1
TERFUL(1) = "Mountain"
TE2(1) = 13
TER$(1) = "M"
movepen(1) = 3
defbonus(1) = 3
SUPLYPT(1) = 0
LANDONLY(1) = 1
SHIPONLY(1) = 0
sustain(1) = 1
TECOL(1) = 2
TERFUL(4) = "Grassland"
TE2(4) = 10
TER$(4) = "G"
TECOL(4) = 6
movepen(4) = 1
defbonus(4) = 1
SUPLYPT(4) = 0
LANDONLY(4) = 1
SHIPONLY(4) = 0
sustain(4) = 1
TERFUL(2) = "Hills"
TE2(2) = 12
TER$(2) = "H"
TECOL(2) = 7
movepen(2) = 2
defbonus(2) = 2
SUPLYPT(2) = 0
LANDONLY(2) = 1
sustain(2) = 1
TERFUL(5) = "Wetland"
TE2(5) = 3
TER$(5) = "W"
TECOL(5) = 11
movepen(5) = 2
defbonus(5) = 1
SUPLYPT(5) = 0
LANDONLY(5) = 1
sustain(5) = 1
TERFUL(8) = "Tundra"
TE2(8) = 15
TER$(8) = "T"
TECOL(8) = 4
movepen(8) = 1
defbonus(8) = 1
SUPLYPT(8) = 0
LANDONLY(8) = 1
sustain(8) = 0

'descriptions of military units ----------------------------------------------
FOR N = 1 TO 90
PLAYER(1, N, 6) = 0
PLAYER(2, N, 6) = 10
NEXT N
maxnum(1) = 500
maxnum(2) = 50
maxnum(3) = 10
maxnum(4) = 1
maxnum(5) = 2
maxnum(6) = 4
maxnum(7) = 1
maxnum(8) = 4
maxnum(9) = 2
maxnum(10) = 1

cost(1) = 1
cost(2) = 3
cost(3) = 4
cost(4) = 2
cost(5) = 3
cost(6) = 2
cost(7) = 2
cost(8) = 6
cost(9) = 3
cost(10) = 2

SUPPLY(4) = 1
SUPPLY(7) = 1
SUPPLY(10) = 1

INAIR(5) = 1
INAIR(6) = 1
INAIR(7) = 1

SHIP(8) = 1
SHIP(9) = 1
SHIP(10) = 1

MAXSUP(1) = 8
MAXSUP(2) = 6
MAXSUP(3) = 4
MAXSUP(4) = 50
MAXSUP(5) = 6
MAXSUP(6) = 8
MAXSUP(7) = 50
MAXSUP(8) = 24
MAXSUP(9) = 16
MAXSUP(10) = 50

maxrange(1) = 1
maxrange(2) = 2
maxrange(3) = 4
maxrange(4) = 1
maxrange(5) = 2
maxrange(6) = 3
maxrange(7) = 1
maxrange(8) = 4
maxrange(9) = 3
maxrange(10) = 1

move(1) = 3
move(2) = 2
move(3) = 1
move(4) = 2
move(5) = 6
move(6) = 10
move(7) = 4
move(8) = 1
move(9) = 2
move(10) = 2

LET$(1) = "I"
LET$(2) = "R"
LET$(3) = "A"
LET$(4) = "S"
LET$(5) = "B"
LET$(6) = "F"
LET$(7) = "E"
LET$(8) = "C"
LET$(9) = "D"
LET$(10) = "X"

FULL(1) = "light Infantry"
FULL(2) = "heavy aRmour"
FULL(3) = "Artillery battery"
FULL(4) = "Support truck"
FULL(5) = "Bomber aircraft"
FULL(6) = "Fighter aircraft"
FULL(7) = "rEfueling plane"
FULL(8) = "missile Carrier"
FULL(9) = "Destroyer"
FULL(10) = "auXilliary ship"

probhit(1) = .75
probhit(2) = .5
probhit(3) = .25
probhit(4) = 0
probhit(5) = .75
probhit(6) = .5
probhit(7) = 0
probhit(8) = .25
probhit(9) = .5
probhit(10) = 0

damage(1) = 1
damage(2) = 30
damage(3) = 200
damage(4) = 0
damage(5) = 1500
damage(6) = 500
damage(7) = 0
damage(8) = 500
damage(9) = 750
damage(10) = 0

hitp(1) = 2
hitp(2) = 60
hitp(3) = 100
hitp(4) = 1
hitp(5) = 500
hitp(6) = 250
hitp(7) = 1
hitp(8) = 1500
hitp(9) = 1500
hitp(10) = 1000

samcap(1) = 1
samcap(2) = 1
samcap(3) = 0
samcap(4) = 0
samcap(5) = 1
samcap(6) = 0
samcap(7) = 0
samcap(8) = 1
samcap(9) = 1
samcap(10) = 1

graph(1) = "desert.BSV"
graph(2) = "hills.BSV"
graph(3) = "land.BSV"
graph(4) = "MOUNTM0.BSV"
graph(5) = "renddown.BSV"
graph(6) = "rendleft.BSV"
graph(7) = "rendrght.BSV"
graph(8) = "rendup.BSV"
graph(9) = "riverbl.BSV"
graph(10) = "riverbr.BSV"
graph(11) = "riverlft.BSV"
graph(12) = "rivertl.BSV"
graph(13) = "rivertr.BSV"
graph(14) = "riverup.BSV"
graph(15) = "rleftup.BSV"
graph(16) = "rlftdown.BSV"
graph(17) = "rlftdwn2.BSV"
graph(18) = "rlftup2.BSV"
graph(19) = "rstartup.BSV"
graph(20) = "rstrtdwn.BSV"
graph(21) = "rstrtlft.BSV"
graph(22) = "rstrtrht.BSV"
graph(23) = "rupleft.BSV"
graph(24) = "rupleft2.BSV"
graph(25) = "ruprght2.BSV"
graph(26) = "rupright.BSV"
graph(27) = "sea.BSV"
graph(28) = "seablanD.BSV"
graph(29) = "seabllnd.BSV"
graph(30) = "seabrlnd.BSV"
graph(31) = "seacblld.BSV"
graph(32) = "seacbrld.BSV"
graph(33) = "seactlld.BSV"
graph(34) = "seactrld.BSV"
graph(35) = "sealland.BSV"
graph(36) = "searland.BSV"
graph(37) = "seatland.BSV"
graph(38) = "seatllnd.BSV"
graph(39) = "seatrlnd.BSV"
graph(40) = "swamps.BSV"
graph(41) = "tundra.BSV"
graph(42) = "urban.BSV"
graph(43) = "woodS.BSV"
graph(44) = "seamid.BSV"
graph(45) = "mountm0.BSV"
graph(46) = "mountm1.BSV"
graph(47) = "mountm2.BSV"
graph(48) = "mountm3.BSV"
graph(49) = "mountm4.BSV"
graph(50) = "mountm5.BSV"
graph(51) = "mountm6.BSV"
graph(52) = "mountm7.BSV"
graph(53) = "mountm8.bsv"
graph(54) = "mountm9.bsv"
graph(55) = "mountm10.bsv"
graph(56) = "mountm11.bsv"
graph(57) = "mountm12.bsv"
graph(58) = "mountm13.bsv"
graph(59) = "hillh0.BSV"
graph(60) = "hillh1.BSV"
graph(61) = "hillh2.BSV"
graph(62) = "hillh3.BSV"
graph(63) = "hillh4.BSV"
graph(64) = "hillh5.BSV"
graph(65) = "hillh6.BSV"
graph(66) = "hillh7.BSV"
graph(67) = "hillh8.bsv"
graph(68) = "hillh9.bsv"
graph(69) = "hillh10.bsv"
graph(70) = "hillh11.bsv"
graph(71) = "hillh12.bsv"
graph(72) = "hillh13.bsv"
'-----------------------------------------------------------------------------
unt(1) = "1rli.BSV"
unt(2) = "1rha.BSV"
unt(3) = "1rab.BSV"
unt(4) = "1rls.BSV"
unt(5) = "1rBa.BSV"
unt(6) = "1rFa.BSV"
unt(7) = "1rra.BSV"
unt(8) = "1rmc.BSV"
unt(9) = "1rde.BSV"
unt(10) = "1ras.BSV"
unt(11) = "1lli.BSV"
unt(12) = "1lha.BSV"
unt(13) = "1lab.BSV"
unt(14) = "1lls.BSV"
unt(15) = "1LBa.BSV"
unt(16) = "1LFa.BSV"
unt(17) = "1Lra.BSV"
unt(18) = "1Lmc.BSV"
unt(19) = "1Lde.BSV"
unt(20) = "1Las.BSV"
unt(21) = "2rli.BSV"
unt(22) = "2rha.BSV"
unt(23) = "2rab.BSV"
unt(24) = "2rls.BSV"
unt(25) = "2rBa.BSV"
unt(26) = "2rFa.BSV"
unt(27) = "2rra.BSV"
unt(28) = "2rmc.BSV"
unt(29) = "2rde.BSV"
unt(30) = "2ras.BSV"
unt(31) = "2lli.BSV"
unt(32) = "2lha.BSV"
unt(33) = "2lab.BSV"
unt(34) = "2lls.BSV"
unt(35) = "2LBa.BSV"
unt(36) = "2LFa.BSV"
unt(37) = "2Lra.BSV"
unt(38) = "2Lmc.BSV"
unt(39) = "2Lde.BSV"
unt(40) = "2Las.BSV"
unt(41) = "BOOM.BSV"
unt(42) = "SUPPLY.BSV"
END SUB

SUB moveunit (P, MAXN)
DIM ORDER(MAXN)
FOR N = 1 TO MAXN
ORDER(N) = N
NEXT N
A$ = SHOW$(P, 1, 0, 1)
FOR N = 1 TO MAXN
IF PLAYER(P, ORDER(N), 3) >= 1 THEN
  A$ = SHOW$(P, ORDER(N), 0, 0)
  QUICKINFO P, ORDER(N), TURNS, move(PLAYER(P, ORDER(N), 4))
  MOVES = 0
  MOVED = 0
  IF WHOSTHERE(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2)) = (P * 100) + ORDER(N) THEN WHOSTHERE(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2)) = 0
  DO
  DO
  A$ = SHOW$(P, ORDER(N), 1, 0)
  IF A$ = "C" THEN A$ = SHOW$(P, ORDER(N), 0, 1)
  IF A$ = "W" AND MOVES = 0 THEN
ST:
        TEMP = ORDER(N)
        FOR h = N TO MAXN - 1
        ORDER(h) = ORDER(h + 1)
        NEXT h
        ORDER(MAXN) = TEMP
        IF PLAYER(P, TEMP, 3) >= 1 THEN WHOSTHERE(PLAYER(P, TEMP, 1), PLAYER(P, TEMP, 2)) = (P * 100) + TEMP
        IF PLAYER(P, ORDER(N), 3) < 1 THEN WHOSTHERE(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2)) = 0: GOTO ST
        QUICKINFO P, ORDER(N), TURNS, move(PLAYER(P, ORDER(N), 4))
  END IF
  WHOSTHERE(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2)) = 0
LOOP WHILE A$ = "C" OR A$ = "W"
   IF A$ = "0" AND PLAYER(P, ORDER(N), 5) > 0 THEN
     combat1 P, ORDER(N)
     IF INAIR(PLAYER(P, ORDER(N), 4)) = 0 THEN PLAYER(P, ORDER(N), 5) = PLAYER(P, ORDER(N), 5) - 1
     EXIT DO
  END IF
  WHOSTHERE(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2)) = 0
  IF A$ = "5" THEN EXIT DO
  IF PLAYER(P, ORDER(N), 5) < 1 THEN EXIT DO
  X = 0: Y = 0
  IF A$ = "1" THEN Y = 1: X = -1
  IF A$ = "2" THEN Y = 1: X = 0
  IF A$ = "3" THEN Y = 1:  X = 1
  IF A$ = "4" THEN X = -1: Y = 0
  IF A$ = "6" THEN X = 1: Y = 0
  IF A$ = "7" THEN Y = -1: X = -1
  IF A$ = "8" THEN Y = -1: X = 0
  IF A$ = "9" THEN Y = -1: X = 1
  IF X = -1 THEN PLAYER(P, ORDER(N), 6) = 10
  IF X = 1 THEN PLAYER(P, ORDER(N), 6) = 0
  MOVED = 1
  IF PLAYER(P, ORDER(N), 1) + X < 1 THEN MOVED = 0: X = 0: Y = 0
  IF PLAYER(P, ORDER(N), 1) + X > 60 THEN MOVED = 0: X = 0: Y = 0
  IF PLAYER(P, ORDER(N), 2) + Y < 1 THEN MOVED = 0: X = 0: Y = 0
  IF PLAYER(P, ORDER(N), 2) + Y > 40 THEN MOVED = 0: X = 0: Y = 0
  IF MOVED AND SHIPONLY(TERAIN(PLAYER(P, ORDER(N), 1) + X, PLAYER(P, ORDER(N), 2) + Y)) = 1 AND SHIP(PLAYER(P, ORDER(N), 4)) = 0 AND INAIR(PLAYER(P, ORDER(N), 4)) = 0 THEN
     MOVED = 0
        LINE (200, 100)-(430, 380), 0, BF
        LINE (200, 100)-(430, 380), 4, B
        LINE (202, 102)-(428, 378), 3, B
        LINE (204, 104)-(426, 376), 2, B
        COLOR 4
        LOCATE 16, 33: PRINT "MOVEMENT NOTE"
        LOCATE 19, 27: PRINT "You are attempting to"
        LOCATE 21, 27: PRINT "move onto a sea square"
        LOCATE 23, 27: PRINT "with a unit which is not"
        LOCATE 25, 27: PRINT "capable of sea movement."
        LOCATE 27, 27: PRINT "Only aircraft and ships"
        LOCATE 29, 27: PRINT "can move on water."
        LOCATE 31, 27: PRINT "   Press any key"
        A$ = INPUT$(1)
        g$ = SHOW$(P, ORDER(N), 0, 1)
     END IF
  IF MOVED AND LANDONLY(TERAIN(PLAYER(P, ORDER(N), 1) + X, PLAYER(P, ORDER(N), 2) + Y)) = 1 AND SHIP(PLAYER(P, ORDER(N), 4)) = 1 AND INAIR(PLAYER(P, ORDER(N), 4)) = 0 THEN
        MOVED = 0
        LINE (200, 100)-(430, 380), 0, BF
        LINE (200, 100)-(430, 380), 4, B
        LINE (202, 102)-(428, 378), 3, B
        LINE (204, 104)-(426, 376), 2, B
        COLOR 4
        LOCATE 16, 33: PRINT "MOVEMENT NOTE"
        LOCATE 19, 27: PRINT "You are attempting to"
        LOCATE 21, 27: PRINT "move onto a land square"
        LOCATE 23, 27: PRINT "with a unit which is not"
        LOCATE 25, 27: PRINT "capable of land movement."
        LOCATE 27, 27: PRINT "Only aircraft and land"
        LOCATE 29, 27: PRINT "units can move on water."
        LOCATE 31, 27: PRINT "   Press any key"
        A$ = INPUT$(1)
        g$ = SHOW$(P, ORDER(N), 0, 1)
  END IF
  WHO = INT(WHOSTHERE(PLAYER(P, ORDER(N), 1) + X, PLAYER(P, ORDER(N), 2) + Y) / 100)
  IF WHO <> 0 THEN
  MOVED = 0: X = 0: Y = 0
        LINE (200, 100)-(430, 380), 0, BF
        LINE (200, 100)-(430, 380), 4, B
        LINE (202, 102)-(428, 378), 3, B
        LINE (204, 104)-(426, 376), 2, B
        COLOR 4
        LOCATE 16, 33: PRINT "MOVEMENT NOTE"
        LOCATE 19, 27: PRINT "You are attempting to"
        LOCATE 21, 27: PRINT "move onto a square which"
        LOCATE 23, 27: PRINT "is occupied. You must"
        LOCATE 25, 27: PRINT "destroy enemy units before"
        LOCATE 27, 27: PRINT "you can move onto the  "
        LOCATE 29, 27: PRINT "same square.      "
        LOCATE 31, 27: PRINT "   Press any key"
        A$ = INPUT$(1)
        g$ = SHOW$(P, ORDER(N), 0, 1)
  END IF
  IF MOVED = 1 THEN
  PLAYER(P, ORDER(N), 1) = PLAYER(P, ORDER(N), 1) + X: PLAYER(P, ORDER(N), 2) = PLAYER(P, ORDER(N), 2) + Y
  IF INAIR(PLAYER(P, ORDER(N), 4)) = 0 THEN PLAYER(P, ORDER(N), 5) = PLAYER(P, ORDER(N), 5) - 1
  IF INAIR(PLAYER(P, ORDER(N), 4)) THEN MOVES = MOVES + 1 ELSE MOVES = MOVES + movepen(TERAIN(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2)))
  END IF
  IF move(PLAYER(P, ORDER(N), 4)) - MOVES > 0 THEN QUICKINFO P, ORDER(N), TURNS, move(PLAYER(P, ORDER(N), 4)) - MOVES
  IF MOVES >= move(PLAYER(P, ORDER(N), 4)) THEN WHOSTHERE(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2)) = (100 * P) + ORDER(N)
  LOOP UNTIL MOVES >= move(PLAYER(P, ORDER(N), 4))
  IF SUPLYPT(TERAIN(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2))) = 1 THEN PLAYER(P, ORDER(N), 5) = MAXSUP(PLAYER(P, ORDER(N), 4))
  WHOSTHERE(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2)) = (100 * P) + ORDER(N)
  IF PLAYER(P, ORDER(N), 5) < 1 AND sustain(TERAIN(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2))) = 0 THEN
        PLAYER(P, ORDER(N), 3) = 0
        LINE (200, 100)-(430, 380), 0, BF
        LINE (200, 100)-(430, 380), 4, B
        LINE (202, 102)-(428, 378), 3, B
        LINE (204, 104)-(426, 376), 2, B
        COLOR 4
        LOCATE 16, 33: PRINT "MOVEMENT NOTE"
        LOCATE 19, 27: PRINT "Your unit has died. It"
        LOCATE 21, 27: PRINT "was left with no supply"
        LOCATE 23, 27: PRINT "on a terrain square from"
        LOCATE 25, 27: PRINT "which no food can be found."
        LOCATE 27, 27: PRINT "It is best to keep supply"
        LOCATE 29, 27: PRINT "trucks close to all units."
        LOCATE 31, 27: PRINT "   Press any key"
        A$ = INPUT$(1)
        g$ = SHOW$(P, ORDER(N), 0, 1)
  END IF
  IF INAIR(PLAYER(P, ORDER(N), 4)) = 1 THEN PLAYER(P, ORDER(N), 5) = PLAYER(P, ORDER(N), 5) - 1
  IF PLAYER(P, ORDER(N), 5) < 1 AND INAIR(PLAYER(P, ORDER(N), 4)) = 1 THEN
        IF WHOSTHERE(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2)) = (100 * P) + ORDER(N) THEN WHOSTHERE(PLAYER(P, ORDER(N), 1), PLAYER(P, ORDER(N), 2)) = 0
        PLAYER(P, ORDER(N), 3) = 0
        LINE (200, 100)-(430, 380), 0, BF
        LINE (200, 100)-(430, 380), 4, B
        LINE (202, 102)-(428, 378), 3, B
        LINE (204, 104)-(426, 376), 2, B
        COLOR 4
        LOCATE 16, 33: PRINT "MOVEMENT NOTE"
        LOCATE 19, 27: PRINT "You unit has died. When"
        LOCATE 21, 27: PRINT "aircraft units are not"
        LOCATE 23, 27: PRINT "in a city square when   "
        LOCATE 25, 27: PRINT "they run out of fuel they"
        LOCATE 27, 27: PRINT "crash and die. Aircraft "
        LOCATE 29, 27: PRINT "need to be kept close to "
        LOCATE 31, 27: PRINT "cities or refueling"
        LOCATE 33, 27: PRINT "aircraft. "
        LOCATE 37, 27: PRINT "    Press any key"
        A$ = INPUT$(1)
        g$ = SHOW$(P, ORDER(N), 0, 1)
  END IF
 END IF
NEXT N
END SUB

SUB PIC
SCREEN 0
WIDTH , 43
FOR N = 1 TO 60
FOR S = 1 TO 40
COLOR TE2(TERAIN(N, S)), 0
LOCATE S, N + 20: PRINT TER$(TERAIN(N, S))
NEXT S, N
END SUB

SUB posforces (P)
SCREEN 9
COLOR 0, 1
CLS
IF P = 1 THEN min = 1: max = 30
IF P = 2 THEN min = 31: max = 60
PIC
COLOR 15, 0
FRAME 1, 40, 1, 20
FOR N = 1 TO 90
'X = min
'Y = 1
IF INAIR(PLAYER(P, N, 4)) = 1 THEN
 X = LANDX(P)
 Y = LANDY(P)
END IF
IF SHIP(PLAYER(P, N, 4)) = 1 THEN
  X = SEAX(P)
  Y = SEAY(P)
 ELSE
  X = LANDX(P)
  Y = LANDY(P)
END IF
LOCATE Y, X + 20: COLOR TE2(TERAIN(X, Y)): PRINT TER$(TERAIN(X, Y))
LOCATE Y, X + 20: COLOR 30, 0: PRINT LET$(PLAYER(P, N, 4))
IF PLAYER(P, N, 3) > 0 THEN
DO
 COLOR 12, 0
 LOCATE 2, 2
 PRINT " VANQUISHER v1.0"
 COLOR 15, 0
 LOCATE 4, 2
 PRINT STRING$(18, 196)
 LOCATE 5, 2
 PRINT "Forces: ";
 COLOR 12, 0: PRINT "Player "; P
 COLOR 15, 0
 LOCATE 6, 2
 PRINT STRING$(18, 196)
 LOCATE 8, 2
 PRINT "Type of unit:"
 COLOR 12, 0
 LOCATE 9, 2
 PRINT " "; FULL(PLAYER(P, N, 4))
 COLOR 15, 0
 LOCATE 11, 2
 PRINT "Attack Range:";
 COLOR 12, 0: PRINT maxrange(PLAYER(P, N, 4))
 COLOR 15, 0
 LOCATE 13, 2
 PRINT "Supply status:";
 COLOR 12, 0: PRINT PLAYER(P, N, 5)
 COLOR 15, 0
 LOCATE 15, 2
 PRINT "Movement range";
 COLOR 12, 0: PRINT move(PLAYER(P, N, 4))
 COLOR 15, 0
 LOCATE 17, 2
 PRINT STRING$(18, 196)
 LOCATE 18, 2
 PRINT "Terrain:";
 COLOR 12, 0: PRINT TERFUL(TERAIN(X, Y))
 COLOR 15, 0
 LOCATE 19, 2
 PRINT STRING$(18, 196)
 LOCATE 21, 2
 PRINT "Defence bonus";
 COLOR 12, 0: PRINT defbonus(TERAIN(X, Y)); " "
 COLOR 15, 0
 LOCATE 23, 2
 PRINT "Movement cost";
 COLOR 12, 0: PRINT movepen(TERAIN(X, Y)); " "
 COLOR 15, 0
 LOCATE 25, 2
 PRINT "Supply point?";
 COLOR 12, 0
 LOCATE 26, 2
 IF SUPLYPT(TERAIN(X, Y)) = 1 THEN PRINT " Yes" ELSE PRINT " No "
 COLOR 15, 0
 LOCATE 27, 2
 PRINT "Suitable for Navy?"
 COLOR 12, 0
 LOCATE 28, 2
 IF LANDONLY(TERAIN(X, Y)) = 0 THEN PRINT " Yes" ELSE PRINT " No "
 COLOR 15, 0
 LOCATE 29, 2
 PRINT "Suitable for Army?"
 COLOR 12, 0
 LOCATE 30, 2
 IF SHIPONLY(TERAIN(X, Y)) = 0 THEN PRINT " Yes" ELSE PRINT " No "
 PRINT
 PRINT
 COLOR 11, 0
 LOCATE 32, 2
 PRINT "Choose your troop"
 LOCATE 33, 2
 PRINT "positions player"; RTRIM$(STR$(P))
 LOCATE 34, 2
 PRINT "Use Numeric pad "
 LOCATE 35, 2
 PRINT "cursors to change"
 LOCATE 36, 2
 PRINT "position and 5 to"
 LOCATE 37, 2
 PRINT "fix location of"
 LOCATE 38, 2
 PRINT "each unit"
 A$ = INPUT$(1)
  LOCATE Y, X + 20: COLOR TE2(TERAIN(X, Y)), 0: PRINT TER$(TERAIN(X, Y))
  IF A$ = "5" THEN
  ok = 1
  IF WHOSTHERE(X, Y) <> 0 THEN ok = 0
  IF LANDONLY(TERAIN(X, Y)) = 1 AND SHIP(PLAYER(P, N, 4)) AND INAIR(PLAYER(P, N, 4)) = 0 THEN ok = 0
  IF SHIPONLY(TERAIN(X, Y)) = 1 AND SHIP(PLAYER(P, N, 4)) = 0 AND INAIR(PLAYER(P, N, 4)) = 0 THEN ok = 0
  IF ok THEN EXIT DO
  END IF
  IF A$ = "1" THEN DY = 1: DX = -1
  IF A$ = "2" THEN DY = 1: DX = 0
  IF A$ = "3" THEN DY = 1: DX = 1
  IF A$ = "4" THEN DX = -1: DY = 0
  IF A$ = "6" THEN DX = 1: DY = 0
  IF A$ = "7" THEN DY = -1: DX = -1
  IF A$ = "8" THEN DY = -1: DX = 0
  IF A$ = "9" THEN DY = -1: DX = 1
  IF X + DX > max THEN DX = 0
  IF X + DX < min THEN DX = 0
  IF Y + DY > 40 THEN DY = 0
  IF Y + DY < 1 THEN DY = 0
  IF WHOSTHERE(X, Y) <> 0 THEN
  IF WHOSTHERE(X, Y) > 200 THEN TR = WHOSTHERE(X, Y) - 200: TP = 2 ELSE TR = WHOSTHERE(X, Y) - 100: TP = 1
  LOCATE Y, X + 20
  COLOR 15, 0
  PRINT LET$(PLAYER(TP, TR, 4))
  END IF
  X = X + DX
  Y = Y + DY
  LOCATE Y, X + 20: COLOR 30, 0: PRINT LET$(PLAYER(P, N, 4))
  LOOP
  PLAYER(P, N, 1) = X
  PLAYER(P, N, 2) = Y
  LOCATE Y, X + 20: COLOR 15, 0: PRINT LET$(PLAYER(P, N, 4))
  WHOSTHERE(X, Y) = (P * 100) + N
END IF
NEXT N
END SUB

SUB printhelp
GFRAME 25, 10, 62, 50
COLOR 6
LOCATE 12, 26: PRINT "       Vanquisher Help       "
COLOR 14
LOCATE 14, 26: PRINT "Movement:"
COLOR 4
LOCATE 15, 26: PRINT "When a unit is flashing use "
LOCATE 16, 26: PRINT "the Numeric Pad with Num Lock"
LOCATE 17, 26: PRINT "on. Use 5 to end movement."
LOCATE 18, 26: PRINT "Press '0' to attack. If you "
LOCATE 19, 26: PRINT "are using a supply vehicle a "
LOCATE 20, 26: PRINT "supply picture will appear, "
LOCATE 21, 26: PRINT "else an explosion will."
LOCATE 22, 26: PRINT "Position this over the unit "
LOCATE 23, 26: PRINT "you wish to supply or attack"
LOCATE 24, 26: PRINT "in the same way as you move"
LOCATE 25, 26: PRINT "and use 5 to finalize."
LOCATE 26, 26: PRINT "To scroll the screen use the"
LOCATE 27, 26: PRINT "numeric pad with Num Lock off."
COLOR 14
LOCATE 29, 26: PRINT "Options:"
COLOR 4
LOCATE 30, 26: PRINT "To call up the options screen"
LOCATE 31, 26: PRINT "press F2. This gives some "
LOCATE 32, 26: PRINT "options which can help to"
LOCATE 33, 26: PRINT "speed up the game."
COLOR 14
LOCATE 35, 26: PRINT "Movement options:"
COLOR 4
LOCATE 36, 26: PRINT "While a unit is flashing there"
LOCATE 37, 26: PRINT "are a few options. "
LOCATE 39, 26: PRINT "Press C to centre. This redraws"
LOCATE 40, 26: PRINT "the screen around the unit so"
LOCATE 41, 26: PRINT "everything in range can be seen."
LOCATE 43, 26: PRINT "Press W to wait. This lets you "
LOCATE 44, 26: PRINT "move the rest of your units before"
LOCATE 45, 26: PRINT "this one. The unit stops flashing"
LOCATE 46, 26: PRINT "and you can move the next one."
A$ = INPUT$(1)
END SUB

SUB QUICKINFO (P, N, TURNS, MOVES)
COLOR 12
LOCATE 5, 17
PRINT P
LOCATE 9, 2
PRINT " "; FULL(PLAYER(P, N, 4))
LOCATE 11, 4
PRINT maxrange(PLAYER(P, N, 4))
 LOCATE 13, 4
PRINT MOVES; "  "
LOCATE 15, 4
PRINT INT(PLAYER(P, N, 3)); "  "
LOCATE 17, 4
PRINT PLAYER(P, N, 5); "  "
LOCATE 19, 10
PRINT TERFUL(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 23, 4
PRINT defbonus(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 25, 4
PRINT movepen(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 27, 4
IF SUPPLY(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 1 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 29, 4
IF LANDONLY(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 31, 4
IF SHIPONLY(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 33, 4
PRINT TURNS
IF drawuNit THEN
PUT (20, 385), MAP, PSET
LINE (20 + (2 * PLAYER(P, N, 1)) - 10, 385 + (2 * PLAYER(P, N, 2)) - 10)-STEP(20, 20), 0, B
FOR t = 1 TO 2
COLOR 0 + (t - 1) * 15
FOR S = 1 TO 30
IF PLAYER(t, S, 3) >= 1 THEN LINE (20 + ((PLAYER(t, S, 1) - 1) * 2), (2 * (PLAYER(t, S, 2) - 1)) + 386)-STEP(2, 2), , BF
NEXT S, t
END IF
END SUB

FUNCTION READOBJ$ (FILENAME$)
OPEN FILENAME$ FOR INPUT AS #1
INPUT #1, terfile$
readterain (terfile$)
INPUT #1, armyuse(1), aIRUSE(1), navyuse(1)
INPUT #1, armyuse(2), aIRUSE(2), navyuse(2)
FOR N = 1 TO 2
INPUT #1, LANDX(N), LANDY(N), SEAX(N), SEAY(N), AIRX(N), AIRY(N)
NEXT N
INPUT #1, money(1), money(2)
FOR N = 1 TO 2
DO
first = first + 1
DO
bit = bit + 1
INPUT #1, A$
OBJECTIVEX(N, first, bit) = VAL(A$)
IF ASC(A$) = 4 THEN
  OBJECTIVEX(N, first, bit) = 0
  EXIT DO
END IF
IF ASC(A$) = 2 THEN
 ext = 1
 OBJECTIVEX(N, first, bit) = -1
 EXIT DO
END IF
INPUT #1, OBJECTIVEY(N, first, bit)
LOOP
IF ext = 1 THEN ext = 0: EXIT DO
bit = 0
LOOP
MAXOBJ(N) = first
first = 0
bit = 0
NEXT N
INPUT #1, whichplayer, NUMBERTURNS
DIM TEMPPIC(345, 8) AS LONG
DO WHILE NOT EOF(1)
IF INKEY$ = CHR$(27) THEN EXIT DO
IF NOT EOF(1) THEN INPUT #1, A$
A$ = UCASE$(A$)
IF A$ = CHR$(27) THEN EXIT DO
IF A$ = "LINE" THEN
 INPUT #1, X, Y, x1, y1, PSETX
 LINE (X, Y)-(x1, y1), PSETX, BF
END IF
IF A$ = "CLS" THEN CLS
IF A$ = "CIRCLE" THEN
 INPUT #1, X, Y, PSETX
 CIRCLE (X, Y), PSETX
END IF
IF A$ = "UNITLOOP" THEN
 INPUT #1, SART, FINISH, GAP
 INPUT #1, X, Y, NUM
 x1 = 0: y1 = 0
 IF X = 700 THEN x1 = 1
 IF Y = 700 THEN y1 = 1
 IF NUM = 0 THEN NUM = N
 FOR N = SART TO FINISH STEP GAP
 IF x1 = 1 THEN X = N
 IF y1 = 1 THEN Y = N
 PUT (X, Y), units(0, NUM), PSET
 NEXT N
END IF
IF A$ = "TEXT" THEN
 INPUT #1, X, Y, y1, x1
 GFRAME X - 1, Y - 1, X + x1 + 1, Y + y1 + 3
 FOR N = 1 TO y1
 LINE INPUT #1, TEXT$
 LOCATE Y + N, X: PRINT TEXT$
 NEXT N
END IF
IF A$ = "MAPAREA" THEN
 INPUT #1, X, Y, x1, y1
 INPUT #1, PSETX, PSETY
 INPUT #1, PSETX2, PSETY2
 FOR N = X TO x1
 FOR t = Y TO y1
 PUT (PSETX + ((N - X) * 48), PSETY + ((t - Y) * 48)), piccy(0, GRAPHICS(N, t)), PSET
 NEXT t, N
 PUT (PSETX2, PSETY2), MAP, PSET
 LINE (PSETX2 + (X * 2), PSETY2 + (Y * 2))-(PSETX2 + (x1 * 2), PSETY2 + (y1 * 2)), 4, B
END IF
IF A$ = "COLOR" THEN
 INPUT #1, N
 COLOR N
END IF
IF A$ = "MAPGRAPH" THEN
 INPUT #1, X, Y, NUM
 PUT (X, Y), piccy(0, NUM), PSET
END IF
IF A$ = "UNITGRAPH" THEN '
 INPUT #1, X, Y, NUM
 PUT (X, Y), units(0, NUM), PSET
END IF
IF A$ = "PAUSE" THEN
 INPUT #1, NUM
 FOR N = 1 TO NUM
 NEXT N
END IF
IF A$ = "WAIT" THEN g$ = INPUT$(1): IF g$ = CHR$(27) THEN EXIT DO
IF A$ = "GET" THEN
 INPUT #1, NUM, FILENAME$
 DEFSEG = VARSEG(TEMPPIC(0, NUM))
 BLOAD FILENAME$, VARPTR(TEMPPIC(0, NUM))
 DEF SEG
END IF
IF A$ = "DISP" THEN
 INPUT #1, NUM, X, Y
 PUT (X, Y), TEMPPIC(0, NUM)
END IF
LOOP
READOBJ$ = terfile$
CLOSE
ERASE TEMPPIC
END FUNCTION

SUB readterain (FILENAME$)
'OPEN filename$ FOR INPUT AS #1
'filename$ = LEFT$(filename$, LEN(filename$) - 4)
'filename$ = filename$ + ".gph"
CLS
PRINT "Please wait, Loading Mission Data...."
OPEN FILENAME$ FOR INPUT AS #2
FOR N = 1 TO 40
FOR t = 1 TO 60
'INPUT #1, TERAIN(T, N)
INPUT #2, GRAPHICS(t, N)
SELECT CASE GRAPHICS(t, N)
 CASE 1
        TERAIN(t, N) = 9
 CASE 2
        TERAIN(t, N) = 2
 CASE 3
        TERAIN(t, N) = 4
 CASE 4
        TERAIN(t, N) = 1
 CASE 5 TO 18, 23 TO 26
        TERAIN(t, N) = 6
 CASE 19 TO 23, 27 TO 39, 44
        TERAIN(t, N) = 7
 CASE 40
        TERAIN(t, N) = 5
 CASE 41
        TERAIN(t, N) = 8
 CASE 42
        TERAIN(t, N) = 10
 CASE 43
        TERAIN(t, N) = 3
 CASE 44
        TERAIN(t, N) = 7
 CASE 45 TO 57
        TERAIN(t, N) = 1
 CASE 58 TO 72
        TERAIN(t, N) = 2
END SELECT
NEXT t, N
CLOSE #2
DIM BLACK&(16)
PALETTE
PALETTE USING BLACK&
FOR t = 1 TO 40
FOR S = 1 TO 60
LINE (20 + (S * 2), (t * 2) + 395)-STEP(2, 2), TECOL(TERAIN(S, t)), BF
NEXT S, t
GET (20, 395)-STEP(120, 80), MAP
CLS
PALETTE
PALETTE USING colors&
ERASE BLACK&
END SUB

FUNCTION SHOW$ (P, N, ok, OK2)
STATIC X, Y
IF OK2 = 0 AND PLAYER(P, N, 1) - X < 10 AND PLAYER(P, N, 1) - X > -1 AND PLAYER(P, N, 2) - Y < 10 AND PLAYER(P, N, 2) - Y > -1 AND X > 0 THEN GOTO NODRAW
X = PLAYER(P, N, 1)
Y = PLAYER(P, N, 2)
X = X - 5
Y = Y - 5
IF X < 1 THEN X = 1
IF Y < 1 THEN Y = 1
IF Y > 31 THEN Y = 31
IF X > 51 THEN X = 51
GOSUB REDRAW:
 
NODRAW:
FOR S = 1 TO 10
FOR D = 1 TO 10
S1 = S - 1
D1 = D - 1
XS1 = X + S1
YD1 = Y + D1
S2 = 160 + S1 * 48
D2 = 48 * D1
IF WHOSTHERE(XS1, YD1) > 0 THEN
R = INT(WHOSTHERE(XS1, YD1) / 100)
IF R = 1 THEN TF = 0
IF R = 2 THEN TF = 20
h = WHOSTHERE(XS1, YD1) - (R * 100)
PUT (S2 + 10, D2 + 10), units(0, TF + PLAYER(R, h, 4) + PLAYER(R, h, 6)), PSET
END IF
NEXT D, S

IF ok = 0 THEN PUT (160 + (PLAYER(P, N, 1) - X) * 48, (PLAYER(P, N, 2) - Y) * 48), piccy(0, GRAPHICS(PLAYER(P, N, 1), PLAYER(P, N, 2))), PSET: EXIT FUNCTION
'-----------------------------------------------------------------------------
DO
IF PLAYER(P, N, 1) - X >= 0 AND PLAYER(P, N, 1) - X < 10 AND PLAYER(P, N, 2) - Y < 10 AND PLAYER(P, N, 2) - Y >= 0 THEN
PUT (160 + (PLAYER(P, N, 1) - X) * 48, (PLAYER(P, N, 2) - Y) * 48), piccy(0, GRAPHICS(PLAYER(P, N, 1), PLAYER(P, N, 2))), PSET
END IF
FOR O = 1 TO 500
A$ = INKEY$
 IF A$ <> "" THEN EXIT FOR
NEXT O
A = 0
IF A$ <> "" THEN A = ASC(RIGHT$(A$, 1))
IF A$ <> "" AND A$ <> "h" AND A <> 72 AND A <> 71 AND A <> 80 AND A <> 81 AND A <> 75 AND A <> 73 AND A <> 77 AND A <> 79 AND A$ <> "a" THEN EXIT DO
IF A$ = "h" THEN printhelp: GOSUB REDRAW
TRUE = 0
IF A = up THEN Y = Y - 1: TRUE = 1
IF A = down THEN Y = Y + 1: TRUE = 1
IF A = RIGHT THEN X = X + 1: TRUE = 1
IF A = LEFT THEN X = X - 1: TRUE = 1
IF A = upright THEN Y = Y - 1: X = X + 1: TRUE = 1
IF A = upleft THEN Y = Y - 1: X = X - 1: TRUE = 1
IF A = downright THEN X = X + 1: Y = Y + 1: TRUE = 1
IF A = downleft THEN X = X - 1: Y = Y + 1: TRUE = 1
IF X < 1 THEN X = 1
IF Y < 1 THEN Y = 1
IF Y > 31 THEN Y = 31
IF X > 51 THEN X = 51
IF TRUE = 1 THEN
        GOSUB REDRAW
        PUT (20, 385), MAP, PSET
        LINE (20 + (X * 2), 385 + (Y * 2))-STEP(20, 20), 0, B
END IF
IF A$ = "a" THEN
        GFRAME 30, 30, 70, 40
        COLOR 6
        LOCATE 31, 32: PRINT "      Options       "
        COLOR 4
        IF drawuNit = 1 THEN g$ = "ON" ELSE g$ = "OFF"
        LOCATE 33, 32: PRINT "Draw Units on strategical Map = "; g$
        LOCATE 35, 32: PRINT "View Map Area"
        curs = 1
        DO
        COLOR 4
        LOCATE 33, 32: PRINT "Draw Units on strategical Map = "; g$
        LOCATE 35, 32: PRINT "View Map Area"
        COLOR 14
        IF curs = 1 THEN LOCATE 33, 32: PRINT "Draw Units on strategical Map = "; g$
        IF curs = 2 THEN LOCATE 35, 32: PRINT "View Map Area"
        A$ = INPUT$(1)
        IF A$ = CHR$(13) THEN EXIT DO ELSE IF curs = 1 THEN curs = 2 ELSE curs = 1
        LOOP
        IF curs = 1 THEN
        IF drawuNit = 1 THEN drawuNit = 0 ELSE drawuNit = 1
        PUT (20, 385), MAP, PSET
        END IF
        IF curs = 2 THEN
        COLOR 6
        LOCATE 31, 32: PRINT "POSITION BOX ON AREA"
        LOCATE 33, 32: PRINT "Use Numeric pad to position box     "
        LOCATE 35, 32: PRINT "Press 5 or Return to confirm        "
        DO
        PUT (20, 385), MAP, PSET
        LINE (20 + (X * 2), 385 + Y * 2)-STEP(20, 20), 0, B
        A$ = INPUT$(1)
        IF A$ = "1" THEN Y = Y + 1: X = X - 1
        IF A$ = "2" THEN Y = Y + 1
        IF A$ = "3" THEN Y = Y + 1:  X = X + 1
        IF A$ = "4" THEN X = X - 1
        IF A$ = "6" THEN X = X + 1
        IF A$ = "7" THEN Y = Y - 1: X = X - 1
        IF A$ = "8" THEN Y = Y - 1
        IF A$ = "9" THEN Y = Y - 1: X = X + 1
        IF A$ = "5" THEN EXIT DO
        IF A$ = CHR$(13) THEN EXIT DO
        IF X < 1 THEN X = 1
        IF X > 50 THEN X = 50
        IF Y < 1 THEN Y = 1
        IF Y > 30 THEN Y = 30
        LOOP
        PUT (20, 385), MAP, PSET
        END IF
        GOSUB REDRAW
END IF
IF PLAYER(P, N, 1) - X >= 0 AND PLAYER(P, N, 1) - X < 10 AND PLAYER(P, N, 2) - Y < 10 AND PLAYER(P, N, 2) - Y >= 0 THEN
PUT (170 + (PLAYER(P, N, 1) - X) * 48, 10 + ((PLAYER(P, N, 2) - Y) * 48)), units(0, PLAYER(P, N, 4) + PLAYER(P, N, 6) + (P - 1) * 20), PSET
END IF
FOR O = 1 TO 500
A$ = INKEY$
 IF A$ <> "" THEN EXIT FOR
NEXT O
IF A$ = "h" THEN printhelp: GOSUB REDRAW
A = 0
IF A$ <> "" THEN A = ASC(RIGHT$(A$, 1))
IF A$ <> "" AND A$ <> "h" AND A <> 72 AND A <> 71 AND A <> 80 AND A <> 81 AND A <> 75 AND A <> 73 AND A <> 77 AND A <> 79 AND A$ <> "a" THEN EXIT DO
TRUE = 0
IF A = up THEN Y = Y - 1: TRUE = 1
IF A = down THEN Y = Y + 1: TRUE = 1
IF A = RIGHT THEN X = X + 1: TRUE = 1
IF A = LEFT THEN X = X - 1: TRUE = 1
IF A = upright THEN Y = Y - 1: X = X + 1: TRUE = 1
IF A = upleft THEN Y = Y - 1: X = X - 1: TRUE = 1
IF A = downright THEN X = X + 1: Y = Y + 1: TRUE = 1
IF A = downleft THEN X = X - 1: Y = Y + 1: TRUE = 1
IF X < 1 THEN X = 1
IF Y < 1 THEN Y = 1
IF Y > 31 THEN Y = 31
IF X > 51 THEN X = 51
IF TRUE = 1 THEN
        GOSUB REDRAW
        PUT (20, 385), MAP, PSET
        LINE (20 + (X * 2), 385 + (Y * 2))-STEP(20, 20), 0, B
END IF
IF A$ = "a" THEN
        GFRAME 30, 30, 70, 40
        COLOR 6
        LOCATE 31, 32: PRINT "      Options       "
        COLOR 4
        IF drawuNit = 1 THEN g$ = "ON" ELSE g$ = "OFF"
        LOCATE 33, 32: PRINT "Draw Units on strategical Map = "; g$
        LOCATE 35, 32: PRINT "View Map Area"
        curs = 1
        DO
        COLOR 4
        LOCATE 33, 32: PRINT "Draw Units on strategical Map = "; g$
        LOCATE 35, 32: PRINT "View Map Area"
        COLOR 14
        IF curs = 1 THEN LOCATE 33, 32: PRINT "Draw Units on strategical Map = "; g$
        IF curs = 2 THEN LOCATE 35, 32: PRINT "View Map Area"
        A$ = INPUT$(1)
        IF A$ = CHR$(13) THEN EXIT DO ELSE IF curs = 1 THEN curs = 2 ELSE curs = 1
        LOOP
        IF curs = 1 THEN
        IF drawuNit = 1 THEN drawuNit = 0 ELSE drawuNit = 1
        PUT (20, 385), MAP, PSET
        END IF
        IF curs = 2 THEN
        COLOR 6
        LOCATE 31, 32: PRINT "POSITION BOX ON AREA"
        LOCATE 33, 32: PRINT "Use Numeric pad to position box    "
        LOCATE 35, 32: PRINT "Press 5 or Return to confirm       "
        DO
        PUT (20, 385), MAP, PSET
        LINE (20 + (X * 2), 385 + Y * 2)-STEP(20, 20), 0, B
        A$ = INPUT$(1)
        IF A$ = "1" THEN Y = Y + 1: X = X - 1
        IF A$ = "2" THEN Y = Y + 1
        IF A$ = "3" THEN Y = Y + 1:  X = X + 1
        IF A$ = "4" THEN X = X - 1
        IF A$ = "6" THEN X = X + 1
        IF A$ = "7" THEN Y = Y - 1: X = X - 1
        IF A$ = "8" THEN Y = Y - 1
        IF A$ = "9" THEN Y = Y - 1: X = X + 1
        IF A$ = "5" THEN EXIT DO
        IF A$ = CHR$(13) THEN EXIT DO
        IF X < 1 THEN X = 1
        IF X > 50 THEN X = 50
        IF Y < 1 THEN Y = 1
        IF Y > 30 THEN Y = 30
        LOOP
        PUT (20, 385), MAP, PSET
        END IF
        GOSUB REDRAW
END IF
LOOP
'-----------------------------------------------------------------------------
SHOW$ = A$
IF WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) <> 0 THEN
i = INT(WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) / 100)
g = WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) - (i * 100)
IF ABS(PLAYER(P, N, 1) - X) < 10 AND ABS(PLAYER(P, N, 2) - Y) < 10 THEN PUT (170 + (PLAYER(P, N, 1) - X) * 48, 10 + (PLAYER(P, N, 2) - Y) * 48), units(0, PLAYER(i, g, 4) + PLAYER(i, g, 6) + (i - 1) * 20), PSET
ELSE
IF ABS(PLAYER(P, N, 1) - X) < 10 AND ABS(PLAYER(P, N, 2) - Y) < 10 THEN PUT (160 + (PLAYER(P, N, 1) - X) * 48, (PLAYER(P, N, 2) - Y) * 48), piccy(0, GRAPHICS(PLAYER(P, N, 1), PLAYER(P, N, 2))), PSET
END IF
EXIT FUNCTION
REDRAW:
        FOR S = 1 TO 10
        FOR D = 1 TO 10
        S1 = S - 1
        D1 = D - 1
        XS1 = X + S1
        YD1 = Y + D1
        S2 = 160 + S1 * 48
        D2 = 48 * D1
        PUT (S2, D2), piccy(0, GRAPHICS(XS1, YD1)), PSET
        IF WHOSTHERE(XS1, YD1) > 0 THEN
        R = INT(WHOSTHERE(XS1, YD1) / 100)
        IF R = 1 THEN TF = 0
        IF R = 2 THEN TF = 20
        h = WHOSTHERE(XS1, YD1) - (R * 100)
        PUT (S2 + 10, D2 + 10), units(0, TF + PLAYER(R, h, 4) + PLAYER(R, h, 6)), PSET
        END IF
        NEXT D, S
RETURN
END FUNCTION

SUB START
SHELL "dir *.msn > tremp2.slf"
CLS
OPEN "tremp2.slf" FOR INPUT AS #1
FOR N = 1 TO 4
LINE INPUT #1, A$
NEXT N
DO WHILE NOT EOF(1)
IF NOT EOF(1) THEN INPUT #1, A$
A = A + 1
LOOP
A = A - 3
CLOSE
DIM TEMP$(A + 4)
OPEN "tremp2.slf" FOR INPUT AS #1
FOR N = 1 TO 5
LINE INPUT #1, A$
NEXT N
TEMP$(1) = "View Rulebook"
D = 0
DO WHILE NOT EOF(1)
D = D + 1
IF NOT EOF(1) THEN INPUT #1, TEMP$(D + 1)
LOOP
curs = 1
GFRAME 18, 7, 62, 13 + A
LOCATE 8, 20: PRINT "Select Mission .........."
DO
COLOR 3
FOR N = 1 TO A + 1
LOCATE N + 10, 20: PRINT TEMP$(N)
NEXT N
COLOR 14
LOCATE curs + 10, 20: PRINT TEMP$(curs)
A$ = INPUT$(1)
IF A$ = "2" THEN curs = curs + 1
IF A$ = "8" THEN curs = curs - 1
IF curs = 0 THEN curs = A + 1
IF curs = A + 2 THEN curs = 1
IF A$ = CHR$(13) THEN EXIT DO
LOOP
IF curs = 1 THEN
  B$ = READOBJ$("RULES.IRN")
  CLOSE
  START
   ELSE
  CLOSE
  f$ = LEFT$(TEMP$(curs), 8)
  t$ = RIGHT$(LEFT$(TEMP$(curs), 12), 3)
  FILE$ = f$ + "." + t$
  B$ = READOBJ$(FILE$)
END IF
GFRAME 5, 10, 35, 16
COLOR 6
LOCATE 11, 6: PRINT "      Select Option ...     "
curs = 1
DO
COLOR 4
LOCATE 13, 6: PRINT "1 Player Game"
LOCATE 14, 6: PRINT "2 PLayer Game"
COLOR 14
IF curs = 1 THEN LOCATE 13, 6: PRINT "1 Player Game"
IF curs = 2 THEN LOCATE 14, 6: PRINT "2 Player Game"
A$ = INPUT$(1)
IF A$ = CHR$(13) THEN EXIT DO ELSE IF curs = 1 THEN curs = 2 ELSE curs = 1
LOOP
IF curs = 2 THEN comp1 = 0: comp2 = 0: modif1 = 0: modif2 = 0
IF curs = 1 THEN
        GFRAME 20, 18, 50, 24
        COLOR 6
        LOCATE 19, 21: PRINT "      Select Option ...     "
        curs = 1
        DO
        COLOR 4
        LOCATE 21, 21: PRINT "Computer Plays Player 1"
        LOCATE 22, 21: PRINT "Computer PLays Player 2"
        COLOR 14
        IF curs = 1 THEN LOCATE 21, 21: PRINT "Computer Plays Player 1"
        IF curs = 2 THEN LOCATE 22, 21: PRINT "Computer Plays Player 2"
        A$ = INPUT$(1)
        IF A$ = CHR$(13) THEN EXIT DO ELSE IF curs = 1 THEN curs = 2 ELSE curs = 1
        LOOP
        IF curs = 1 THEN comp1 = 1
        IF curs = 2 THEN comp2 = 1
        DIM Ytemp$(5)
        Ytemp$(1) = "Incredibly Easy - My Cat could do it"
        Ytemp$(2) = "Easy - The Cat Might have problems"
        Ytemp$(3) = "Mediocre - The Cat  couldn't do it"
        Ytemp$(4) = "Hard - Adolf Hitler Level"
        Ytemp$(5) = "Damn Hard - Even I can't do this one"
        GFRAME 40, 30, 80, 40
        COLOR 6
        LOCATE 31, 41: PRINT "            Select Level              "
        curs = 1
        DO
        COLOR 4
        FOR N = 1 TO 5
        LOCATE N + 32, 41: PRINT Ytemp$(N)
        NEXT N
        COLOR 14
        LOCATE curs + 32, 41: PRINT Ytemp$(curs)
        A$ = INPUT$(1)
        IF A$ = CHR$(13) THEN EXIT DO
        IF A$ = "8" THEN curs = curs - 1
        IF A$ = "2" THEN curs = curs + 1
        IF curs = 0 THEN curs = 5
        IF curs = 6 THEN curs = 1
        LOOP
        IF comp1 = 1 THEN
                IF curs = 1 THEN modif1 = .8
                IF curs = 2 THEN modif1 = 1
                IF curs = 3 THEN modif1 = 1.2
                IF curs = 4 THEN modif1 = 1.4
                IF curs = 6 THEN modif1 = 1.6
        END IF
        IF comp2 = 1 THEN
                IF curs = 1 THEN modif2 = .8
                IF curs = 2 THEN modif2 = 1
                IF curs = 3 THEN modif2 = 1.2
                IF curs = 4 THEN modif2 = 1.4
                IF curs = 6 THEN modif2 = 1.6
        END IF
END IF
CLOSE
END SUB

SUB WHOSWON
IF TURNS > NUMBERTURNS THEN PRINT "Player "; whichplayer; " Has Won": SHELL endseq$: END
FOR N = 1 TO 2
TRUE = 0
IF N = 1 THEN opp = 2 ELSE opp = 1
DO
first = first + 1
DO
bit = bit + 1
IF OBJECTIVEX(N, first, bit) = 0 THEN EXIT DO
IF OBJECTIVEX(N, first, bit) = -1 THEN ext = 1: EXIT DO
whicplay = INT(WHOSTHERE(OBJECTIVEX(N, first, bit), OBJECTIVEY(N, first, bit)) / 100)
IF whicplay = N THEN TRUE = 1
IF whicplay = opp THEN TRUE = 0: ext = 1: EXIT DO
LOOP
IF ext = 1 THEN EXIT DO
bit = 0
ext = 0
LOOP
IF TRUE = 1 THEN PRINT "Player "; N; " Has won":  END
first = 0
bit = 0
NEXT N
FOR P = 1 TO 2
IF P = 1 THEN PN = 2 ELSE PN = 1
TRUE = 1
FOR N = 1 TO 90
IF PLAYER(P, N, 3) >= 1 THEN TRUE = 0
NEXT N
IF TRUE = 1 THEN PRINT "Player "; PN; " Has Won": END
NEXT P
END SUB

