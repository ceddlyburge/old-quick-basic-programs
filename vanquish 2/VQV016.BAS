DECLARE SUB GFRAME (X!, Y!, X1!, Y1!)
DECLARE SUB GETGRAPHS ()
DECLARE SUB WHOSWON ()
DECLARE FUNCTION readobj$ (filename$)
DECLARE SUB QUICKINFO (P!, N!, TURNS!, MOVES!)
DECLARE SUB PIC ()
DECLARE SUB readterain (filename$)
DECLARE SUB posforces (P!)
DECLARE SUB chooseunit (P!, MONEY!)
DECLARE SUB INFO2 (P!, N!, MOVES)
DECLARE SUB combat1 (N!, P)
DECLARE SUB MOVEUNIT (P)
DECLARE SUB INIT ()
DECLARE FUNCTION SHOW$ (P, N, OK, OK2)
COMMON SHARED col1, col2, TURNS, ARMYUSE, NAVYUSE, whichplayer
COMMON SHARED NUMBERTURNS, endseq$

'-----------------------------------------------------------------------------
'
' VANQUISH V1.0
' By the skillful Cedd and Simon (with entertainment supplied by Ian)
'$DYNAMIC
'-----------------------------------------------------------------------------
DIM SHARED MAP(2596)
DIM SHARED PLAYER(2, 90, 6)
DIM SHARED LET$(16)
DIM SHARED MOVE(16)
DIM SHARED MAXRANGE(16)
DIM SHARED probhit(16)
DIM SHARED damage(16)
DIM SHARED hitp(16)
DIM SHARED MAXSUP(16)
DIM SHARED MAXNUM(16)
DIM SHARED COST(16)
DIM SHARED supply(16)
DIM SHARED inair(16)
DIM SHARED SHIP(16)
DIM SHARED terain(60, 40)
DIM SHARED movepen(30)
DIM SHARED SHIPONLY(30)
DIM SHARED suplyPT(30)
DIM SHARED defbonus(30)
DIM SHARED LANDONLY(30)
DIM SHARED CO(2)
DIM SHARED samcap(16)
DIM SHARED FULL(16) AS STRING * 17
DIM SHARED TER$(30)
DIM SHARED TERFUL(30) AS STRING * 10
DIM SHARED sustain(30)
DIM SHARED TECOL(30)
DIM SHARED WHOSTHERE(60, 40)
DIM SHARED OBJECTIVEX(2, 10, 15)
DIM SHARED objectivey(2, 10, 15)
DIM SHARED LANDX(2), LANDY(2), SEAX(2), SEAY(2), AIRX(2), AIRY(2)
DIM SHARED MONEY(2)
DIM COLORS&(0 TO 15)
DIM SHARED PICCY(345, 52) AS LONG
DIM SHARED UNITS(345, 40) AS LONG
DIM SHARED graph(52) AS STRING
DIM SHARED unt(40) AS STRING
DIM SHARED GRAPHICS(60, 40)
'-----------------------------------------------------------------------------
'ON TIMER(1) GOSUB PT:
'TIMER ON
SCREEN 9
RANDOMIZE TIMER
CO(1) = 1
CO(2) = 4
DEF SEG = 0
POKE &H417, &HF0
B$ = readobj$("C:\VANQUISH\MISSION1.MSN")
readterain B$
COLORS&(0) = 0
COLORS&(1) = 5140
COLORS&(2) = 1315860
COLORS&(3) = 2631720
COLORS&(4) = 4144959
COLORS&(5) = 5120
COLORS&(6) = 1320960
COLORS&(7) = 1326100
COLORS&(8) = 1326120
COLORS&(9) = 2621440
COLORS&(10) = 3937300
COLORS&(11) = 3942420
COLORS&(12) = 5160
COLORS&(13) = 5180
COLORS&(14) = 60
COLORS&(15) = 1326140
INIT

SCREEN 12
WIDTH , 60
PALETTE
PALETTE USING COLORS&
GETGRAPHS
chooseunit 1, MONEY(1)
chooseunit 2, MONEY(2)
posforces 1
posforces 2
SCREEN 12
WIDTH , 60
PALETTE
PALETTE USING COLORS&

INFO2 1, 1, 1

DO
TURNS = TURNS + 1
MOVEUNIT (1)
MOVEUNIT (2)
WHOSWON
LOOP
END

'PT: LOCATE 30, 10: PRINT WHOSTHERE(12, 3)
'RETURN

REM $STATIC
SUB chooseunit (P, MONEY)
GFRAME 1, 1, 80, 60
DIM ARMY(16)
COLOR 4
GFRAME 31, 3, 48, 6
LOCATE 4, 32: PRINT "VANQUISHER V1.0"
GFRAME 2, 8, 79, 15
COLOR 11
LOCATE 9, 10: PRINT "Player "; P; " Please choose your forces. Funds available = "; MONEY
LOCATE 11, 10: PRINT "Numeric pad cursors 2 & 8 moves cursor up and down."
LOCATE 13, 10: PRINT "Numeric pad + & - changes number in force. ENTER ends selection."
COLOR 4
GFRAME 2, 20, 79, 58
LOCATE 21, 10: PRINT "Name of Unit    Move range          Cost         No in unit "
LOCATE 22, 10: PRINT "                       Resupply time    Attack range     No in force"
CURS = 1
MIN = 1
MAX = 10
IF NAVYUSE = 0 THEN MAX = 7
IF ARMYUSE = 0 THEN MIN = 4
COLOR 4
CURS = MIN
FOR N = MIN TO MAX
LOCATE 28 + (N * 2), 10: PRINT FULL(N)
LOCATE 28 + (N * 2), 29: PRINT MOVE(N)
LOCATE 28 + (N * 2), 39: PRINT MAXSUP(N)
LOCATE 28 + (N * 2), 46: PRINT COST(N)
LOCATE 28 + (N * 2), 54: PRINT MAXRANGE(N)
LOCATE 28 + (N * 2), 62: PRINT MAXNUM(N)
IF N = 2 OR N = 4 OR N = 6 OR N = 8 OR N = 10 THEN TEMP = 26 ELSE TEMP = 0
PUT (17 + TEMP, 215 + N * 16), UNITS(0, ((P - 1) * 20) + N), PSET
NEXT N
DO
COLOR 4
FOR N = MIN TO MAX
LOCATE 28 + (N * 2), 69: PRINT ARMY(N); "   "
NEXT N
COLOR 14
LOCATE 28 + (CURS * 2), 69: PRINT ARMY(CURS); "   "
A$ = INPUT$(1)
IF A$ = "8" THEN CURS = CURS - 1
IF A$ = "2" THEN CURS = CURS + 1
IF CURS = MIN - 1 THEN CURS = MAX
IF CURS = MAX + 1 THEN CURS = MIN
IF A$ = "+" THEN ARMY(CURS) = ARMY(CURS) + 1
IF A$ = "-" THEN ARMY(CURS) = ARMY(CURS) - 1
IF A$ = CHR$(13) THEN EXIT DO
TOT = 0
FOR N = MIN TO MAX
TOT = TOT + (ARMY(N) * COST(N))
NEXT N
IF TOT > MONEY THEN ARMY(CURS) = ARMY(CURS) - 1
LOOP



FOR N = MIN TO MAX
FOR t = 1 TO ARMY(N)
IF ARMY(N) = 0 THEN EXIT FOR
count = count + 1
PLAYER(P, count, 3) = MAXNUM(N)
PLAYER(P, count, 4) = N
PLAYER(P, count, 5) = MAXSUP(N)
IF count > 89 THEN EXIT SUB
NEXT t, N
END SUB


SUB combat1 (P, N)
X = 0
Y = 0
maxx = MAXRANGE(PLAYER(P, N, 4))
maxy = MAXRANGE(PLAYER(P, N, 4))
minx = -MAXRANGE(PLAYER(P, N, 4))
miny = -MAXRANGE(PLAYER(P, N, 4))
X1 = PLAYER(P, N, 1)
Y1 = PLAYER(P, N, 2)
X2 = X1 - 5
Y2 = Y1 - 5
IF X2 < 1 THEN X2 = 1
IF Y2 < 1 THEN Y2 = 1
IF Y2 > 31 THEN Y2 = 31
IF X2 > 51 THEN X2 = 51
'-------------------------POSITIONING SQUARE OF ATTACK------------------------
DO
  X3 = 160 + ((PLAYER(P, N, 1) - X2) * 48)
  Y3 = (PLAYER(P, N, 2) - Y2) * 48
  LINE (X3 + (X * 48), Y3 + (Y * 48))-STEP(47, 47), 13, BF
  A$ = INPUT$(1)
  PUT (X3 + (X * 48), Y3 + (Y * 48)), PICCY(0, GRAPHICS(X1 + X, Y1 + Y)), PSET
  IF WHOSTHERE(X1 + X, Y1 + Y) <> 0 THEN LINE (X3 + (X * 48), Y3 + (Y * 48))-STEP(47, 47), 0, BF
  IF A$ = "5" THEN EXIT DO
  IF A$ = "1" THEN Y = Y + 1: X = X - 1
  IF A$ = "2" THEN Y = Y + 1
  IF A$ = "3" THEN Y = Y + 1: X = X + 1
  IF A$ = "4" THEN X = X - 1
  IF A$ = "6" THEN X = X + 1
  IF A$ = "7" THEN Y = Y - 1: X = X - 1
  IF A$ = "8" THEN Y = Y - 1
  IF A$ = "9" THEN Y = Y - 1: X = X + 1
  IF X > maxx THEN X = maxx
  IF X < minx THEN X = minx
  IF Y > maxy THEN Y = maxy
  IF Y < miny THEN Y = miny
  IF X + PLAYER(P, N, 1) > 60 THEN X = X - 1
  IF X + PLAYER(P, N, 1) < 1 THEN X = X + 1
  IF Y + PLAYER(P, N, 2) > 40 THEN Y = Y - 1
  IF Y + PLAYER(P, N, 2) < 1 THEN Y = Y + 1
  'A$ = SHOW$(P, N, 0)
  LOOP
'-----------------------------------------------------------------------------
'------------------------------DECIDING DAMAGE--------------------------------
  HP = 0
  IF WHOSTHERE(X1 + X, Y1 + Y) <> 0 THEN
    true = 1
    pn = INT(WHOSTHERE(X1 + X, Y1 + Y) / 100)
    O = WHOSTHERE(X1 + X, Y1 + Y) - (pn * 100)
    IF inair(PLAYER(P, N, 4)) = 1 AND inair(PLAYER(pn, O, 4)) = 0 AND samcap(PLAYER(P, N, 4)) = 0 THEN
        true = 0
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
    IF inair(PLAYER(P, N, 4)) = 0 AND inair(PLAYER(pn, O, 4)) = 1 AND samcap(PLAYER(P, N, 4)) = 0 THEN
    true = 0
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
    IF true THEN
    FOR t = 1 TO PLAYER(P, N, 3)
    IF RND < probhit(PLAYER(P, N, 4)) THEN HP = HP + damage(PLAYER(P, N, 4))
    NEXT t
    killed = HP / hitp(PLAYER(pn, O, 4)) / defbonus(terain(PLAYER(pn, O, 1), PLAYER(pn, O, 2)))
    PLAYER(pn, O, 3) = PLAYER(pn, O, 3) - killed
    IF PLAYER(pn, O, 3) < 1 THEN WHOSTHERE(PLAYER(pn, O, 1), PLAYER(pn, O, 2)) = 0
    END IF
  END IF
'-----------------------------------------------------------------------------
'--------------------SUPPLYING VEHICULES--------------------------------------
 true = 1
 IF supply(PLAYER(P, N, 4)) = 1 AND WHOSTHERE(X1 + X, Y1 + Y) <> 0 THEN
    pn = INT(WHOSTHERE(X1 + X, Y1 + Y) / 100)
    O = VAL(RIGHT$(STR$(WHOSTHERE(X1 + X, Y1 + Y)), 2))
  IF inair(PLAYER(pn, O, 4)) = 1 AND inair(PLAYER(P, N, 4)) = 0 THEN
   true = 0
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
        G$ = SHOW$(P, N, 0, 1)
  END IF
  IF inair(PLAYER(pn, O, 4)) = 0 AND inair(PLAYER(P, N, 4)) = 1 THEN
  true = 0
        LINE (200, 100)-(430, 380), 0, BF
        LINE (200, 100)-(430, 380), 4, B
        LINE (202, 102)-(428, 378), 3, B
        LINE (204, 104)-(426, 376), 2, B
        COLOR 4
        LOCATE 16, 33: PRINT "MOVEMENT NOTE"
        LOCATE 19, 27: PRINT "You are attempting to"
        LOCATE 21, 27: PRINT "resupply a ground unit  "
        LOCATE 23, 27: PRINT "from a refueling aircraft."
        LOCATE 25, 27: PRINT "Only ground unit can    "
        LOCATE 27, 27: PRINT "resupply other ground "
        LOCATE 29, 27: PRINT "units.            "
        LOCATE 31, 27: PRINT "   Press any key"
        A$ = INPUT$(1)
        G$ = SHOW$(P, N, 0, 1)
 END IF
  IF true = 1 THEN
  PLAYER(pn, O, 5) = MAXSUP(PLAYER(pn, O, 4))
  PLAYER(P, N, 5) = PLAYER(P, N, 5) - MAXSUP(PLAYER(P, O, 4))
 END IF
 END IF
'-----------------------------------------------------------------------------
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

SUB GETGRAPHS
COLOR 15
LOCATE 1, 1: PRINT "Please wait, Loading Graphics Files...."
FOR N = 1 TO 44
OPEN graph(N) FOR INPUT AS #1
INPUT #1, xmax, xmin, ymax, ymin, length'input values before the array values
FOR P = 0 TO 345 'length is the number of files left in the file
INPUT #1, PICCY(P, N)
NEXT P
CLOSE #1
NEXT N
FOR N = 1 TO 40
OPEN unt(N) FOR INPUT AS #1
INPUT #1, X, Y, t, u, I
FOR P = 0 TO 345
INPUT #1, UNITS(P, N)
NEXT P
CLOSE #1
NEXT N
END SUB

SUB GFRAME (X, Y, X1, Y1)
TOP = (Y - 1) * 8
LEFT = (X - 1) * 8
BOT = (Y1 - 1) * 8
RIGHT = (X1 - 1) * 8
LINE (LEFT, TOP)-(RIGHT, BOT), 4, BF
LINE (LEFT + 2, TOP + 2)-(RIGHT - 2, BOT - 2), 3, BF
LINE (LEFT + 4, TOP + 4)-(RIGHT - 4, BOT - 4), 2, BF
LINE (LEFT + 6, TOP + 6)-(RIGHT - 6, BOT - 6), 0, BF
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
COLOR 12: PRINT MAXRANGE(PLAYER(P, N, 4))
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
LOCATE 19, 2
PRINT STRING$(18, 196)
LOCATE 20, 2
PRINT "Terrain ";
COLOR 12: PRINT TERFUL(terain(PLAYER(P, N, 1), PLAYER(P, N, 2)))
COLOR 15
LOCATE 21, 2
PRINT STRING$(18, 196)
LOCATE 24, 2
PRINT "Defence bonus:";
LOCATE 25, 4
COLOR 12: PRINT defbonus(terain(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 26, 2
COLOR 15
PRINT "Movement cost:";
LOCATE 27, 4
COLOR 12: PRINT movepen(terain(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 28, 2
COLOR 15
PRINT "Supply point?";
LOCATE 29, 4
COLOR 12: IF supply(terain(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 1 THEN PRINT " Yes" ELSE PRINT " No "
COLOR 15
LOCATE 30, 2
PRINT "Suitable for Navy?"
COLOR 12
LOCATE 31, 4
IF LANDONLY(terain(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 32, 2
COLOR 15
PRINT "Suitable for Army?"
COLOR 12
LOCATE 33, 4
IF SHIPONLY(terain(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
COLOR 15
LOCATE 34, 2
        PRINT "Turn:";
LOCATE 35, 4
COLOR 12: PRINT TURNS
COLOR 10
LOCATE 36, 2: PRINT STRING$(18, 196)
LOCATE 37, 2: PRINT "Instructions"
LOCATE 38, 2: PRINT STRING$(18, 196)
LOCATE 40, 2: PRINT "Cursor pad moves."
LOCATE 41, 2: PRINT "5 ends movement."
LOCATE 42, 2: PRINT "0 attacks enemy's"
LOCATE 43, 2: PRINT "or supplies "
LOCATE 44, 2: PRINT "friendlies. "
LOCATE 45, 2: PRINT "When attacking "
LOCATE 46, 2: PRINT "position line of"
LOCATE 47, 2: PRINT "attack with cursor"
LOCATE 48, 2: PRINT "pad."
PUT (20, 395), MAP, PSET
FOR t = 1 TO 2
FOR S = 1 TO 30
IF PLAYER(t, S, 3) > 0 THEN LINE (20 + ((PLAYER(t, S, 1) - 1) * 2), (2 * (PLAYER(t, S, 2))) + 394)-STEP(2, 2), 0 + (t - 1) * 4, BF
NEXT S, t
END SUB

SUB INIT
'descriptions of terrain -----------------------------------------------------
TERFUL(7) = "Sea/Lake"
TECOL(7) = 9
TER$(7) = "w"
movepen(7) = 1
defbonus(7) = 1.2
suplyPT(7) = 0
LANDONLY(7) = 0
SHIPONLY(7) = 1
sustain(7) = 1
TERFUL(6) = "River"
TECOL(6) = 10
TER$(6) = "r"
movepen(6) = 2
defbonus(6) = 2
suplyPT(6) = 0
LANDONLY(6) = 1
SHIPONLY(6) = 0
sustain(6) = 1
TERFUL(10) = "Urbanised"
TECOL(10) = 3
TER$(10) = "u"
movepen(10) = .5
defbonus(10) = 3
suplyPT(10) = 1
LANDONLY(10) = 1
SHIPONLY(10) = 0
sustain(10) = 1
TERFUL(9) = "Desert"
TECOL(9) = 15
TER$(9) = "d"
movepen(9) = 1
defbonus(9) = 1
suplyPT(9) = 0
LANDONLY(9) = 1
SHIPONLY(9) = 0
sustain(9) = 0
TERFUL(3) = "Woodland"
TECOL(3) = 5
TER$(3) = "w"
movepen(3) = 2
defbonus(3) = 2
suplyPT(3) = 0
LANDONLY(3) = 1
SHIPONLY(3) = 0
sustain(3) = 1
TERFUL(1) = "Mountain"
TER$(1) = "m"
movepen(1) = 3
defbonus(1) = 3
suplyPT(1) = 0
LANDONLY(1) = 1
SHIPONLY(1) = 0
sustain(1) = 1
TECOL(1) = 2
TERFUL(4) = "Grassland"
TER$(4) = "."
TECOL(4) = 6
movepen(4) = 1
defbonus(4) = 1
suplyPT(4) = 0
LANDONLY(4) = 1
SHIPONLY(4) = 0
sustain(4) = 1
TERFUL(2) = "Hills"
TER$(2) = "h"
TECOL(2) = 7
movepen(2) = 2
defbonus(2) = 2
suplyPT(2) = 0
LANDONLY(2) = 1
sustain(2) = 1
TERFUL(5) = "Wetland"
TER$(5) = "s"
TECOL(5) = 11
movepen(5) = 2
defbonus(5) = 1
suplyPT(5) = 0
LANDONLY(5) = 1
sustain(5) = 1
TERFUL(8) = "Tundra"
TER$(8) = "t"
TECOL(8) = 4
movepen(8) = 1
defbonus(8) = 1
suplyPT(8) = 0
LANDONLY(8) = 1
sustain(8) = 0

'descriptions of military units ----------------------------------------------
FOR N = 1 TO 90
PLAYER(1, N, 6) = 0
PLAYER(2, N, 6) = 10
NEXT N
MAXNUM(1) = 500
MAXNUM(2) = 50
MAXNUM(3) = 10
MAXNUM(4) = 1
MAXNUM(5) = 2
MAXNUM(6) = 4
MAXNUM(7) = 1
MAXNUM(8) = 4
MAXNUM(9) = 2
MAXNUM(10) = 1

COST(1) = 1
COST(2) = 3
COST(3) = 4
COST(4) = 2
COST(5) = 3
COST(6) = 2
COST(7) = 2
COST(8) = 6
COST(9) = 3
COST(10) = 2

supply(4) = 1
supply(7) = 1
supply(10) = 1

inair(5) = 1
inair(6) = 1
inair(7) = 1

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

MAXRANGE(1) = 1
MAXRANGE(2) = 2
MAXRANGE(3) = 4
MAXRANGE(4) = 1
MAXRANGE(5) = 2
MAXRANGE(6) = 3
MAXRANGE(7) = 1
MAXRANGE(8) = 4
MAXRANGE(9) = 3
MAXRANGE(10) = 1

MOVE(1) = 3
MOVE(2) = 2
MOVE(3) = 1
MOVE(4) = 2
MOVE(5) = 6
MOVE(6) = 10
MOVE(7) = 4
MOVE(8) = 1
MOVE(9) = 2
MOVE(10) = 2

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
samcap(2) = 0
samcap(3) = 0
samcap(4) = 0
samcap(5) = 1
samcap(6) = 0
samcap(7) = 0
samcap(8) = 1
samcap(9) = 1
samcap(10) = 1

graph(1) = "desert.pic"
graph(2) = "hills.pic"
graph(3) = "land.pic"
graph(4) = "mountain.pic"
graph(5) = "renddown.pic"
graph(6) = "rendleft.pic"
graph(7) = "rendrght.pic"
graph(8) = "rendup.pic"
graph(9) = "riverbl.pic"
graph(10) = "riverbr.pic"
graph(11) = "riverlft.pic"
graph(12) = "rivertl.pic"
graph(13) = "rivertr.pic"
graph(14) = "riverup.pic"
graph(15) = "rleftup.pic"
graph(16) = "rlftdown.pic"
graph(17) = "rlftdwn2.pic"
graph(18) = "rlftup2.pic"
graph(19) = "rstartup.pic"
graph(20) = "rstrtdwn.pic"
graph(21) = "rstrtlft.pic"
graph(22) = "rstrtrht.pic"
graph(23) = "rupleft.pic"
graph(24) = "rupleft2.pic"
graph(25) = "ruprght2.pic"
graph(26) = "rupright.pic"
graph(27) = "sea.pic"
graph(28) = "seablanD.pic"
graph(29) = "seabllnd.pic"
graph(30) = "seabrlnd.pic"
graph(31) = "seacblld.pic"
graph(32) = "seacbrld.pic"
graph(33) = "seactlld.pic"
graph(34) = "seactrld.pic"
graph(35) = "sealland.pic"
graph(36) = "searland.pic"
graph(37) = "seatland.pic"
graph(38) = "seatllnd.pic"
graph(39) = "seatrlnd.pic"
graph(40) = "swamps.pic"
graph(41) = "tundra.pic"
graph(42) = "urban.pic"
graph(43) = "woods.pic"
graph(44) = "seamid.pic"
graph(45) = "MOUNTBR.PIC"
graph(46) = "mounttr.pic"
graph(47) = "mounttl.pic"
graph(48) = "mountbl.pic"
graph(49) = "mountcbr.pic"
graph(50) = "mountcbl.pic"
graph(51) = "mountctl.pic"
graph(52) = "mountctr.pic"

unt(1) = "1rli.pic"
unt(2) = "1rha.pic"
unt(3) = "1rab.pic"
unt(4) = "1rls.pic"
unt(5) = "1rBa.pic"
unt(6) = "1rFa.pic"
unt(7) = "1rra.pic"
unt(8) = "1rmc.pic"
unt(9) = "1rde.pic"
unt(10) = "1ras.pic"
unt(11) = "1lli.pic"
unt(12) = "1lha.pic"
unt(13) = "1lab.pic"
unt(14) = "1lls.pic"
unt(15) = "1LBa.pic"
unt(16) = "1LFa.pic"
unt(17) = "1Lra.pic"
unt(18) = "1Lmc.pic"
unt(19) = "1Lde.pic"
unt(20) = "1Las.pic"
unt(21) = "2rli.pic"
unt(22) = "2rha.pic"
unt(23) = "2rab.pic"
unt(24) = "2rls.pic"
unt(25) = "2rBa.pic"
unt(26) = "2rFa.pic"
unt(27) = "2rra.pic"
unt(28) = "2rmc.pic"
unt(29) = "2rde.pic"
unt(30) = "2ras.pic"
unt(31) = "2lli.pic"
unt(32) = "2lha.pic"
unt(33) = "2lab.pic"
unt(34) = "2lls.pic"
unt(35) = "2LBa.pic"
unt(36) = "2LFa.pic"
unt(37) = "2Lra.pic"
unt(38) = "2Lmc.pic"
unt(39) = "2Lde.pic"
unt(40) = "2Las.pic"

END SUB

SUB MOVEUNIT (P)
FOR N = 1 TO 90
IF PLAYER(P, N, 3) > 0 THEN
  A$ = SHOW$(P, N, 0, 1)
  QUICKINFO P, N, TURNS, MOVE(PLAYER(P, N, 4))
  MOVES = 0
  MOVED = 0
  IF WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = (P * 100) + N THEN WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = 0
  DO
  A$ = SHOW$(P, N, 1, 0)
  IF A$ = "0" AND MOVES > 0 THEN
        LINE (200, 100)-(430, 380), 0, BF
        LINE (200, 100)-(430, 380), 4, B
        LINE (202, 102)-(428, 378), 3, B
        LINE (204, 104)-(426, 376), 2, B
        COLOR 4
        LOCATE 16, 35: PRINT "COMBAT NOTE"
        LOCATE 19, 27: PRINT "You cannot engage in"
        LOCATE 21, 27: PRINT "combat because you have"
        LOCATE 23, 27: PRINT "moved. It is not possible"
        LOCATE 25, 27: PRINT "move and fire in the same"
        LOCATE 27, 27: PRINT "turn."
        LOCATE 31, 27: PRINT "Press any key"
        A$ = INPUT$(1)
        G$ = SHOW$(P, N, 0, 1)
  END IF
  IF A$ = "0" AND MOVES = 0 AND PLAYER(P, N, 5) > 0 THEN
     combat1 P, N
     IF inair(PLAYER(P, N, 4)) = 0 THEN PLAYER(P, N, 5) = PLAYER(P, N, 5) - 1
     EXIT DO
  END IF
  IF A$ = "5" THEN EXIT DO
  IF PLAYER(P, N, 5) < 1 THEN EXIT DO
  X = 0: Y = 0
  IF A$ = "1" THEN Y = 1: X = -1
  IF A$ = "2" THEN Y = 1: X = 0
  IF A$ = "3" THEN Y = 1:  X = 1
  IF A$ = "4" THEN X = -1: Y = 0
  IF A$ = "6" THEN X = 1: Y = 0
  IF A$ = "7" THEN Y = -1: X = -1
  IF A$ = "8" THEN Y = -1: X = 0
  IF A$ = "9" THEN Y = -1: X = 1
  IF X = -1 THEN PLAYER(P, N, 6) = 10
  IF X = 1 THEN PLAYER(P, N, 6) = 0
  MOVED = 1
  IF PLAYER(P, N, 1) + X < 1 THEN MOVED = 0: X = 0: Y = 0
  IF PLAYER(P, N, 1) + X > 60 THEN MOVED = 0: X = 0: Y = 0
  IF PLAYER(P, N, 2) + Y < 1 THEN MOVED = 0: X = 0: Y = 0
  IF PLAYER(P, N, 2) + Y > 40 THEN MOVED = 0: X = 0: Y = 0
  IF MOVED AND SHIPONLY(terain(PLAYER(P, N, 1) + X, PLAYER(P, N, 2) + Y)) = 1 AND SHIP(PLAYER(P, N, 4)) = 0 AND inair(PLAYER(P, N, 4)) = 0 THEN
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
        G$ = SHOW$(P, N, 0, 1)
     END IF
  IF MOVED AND LANDONLY(terain(PLAYER(P, N, 1) + X, PLAYER(P, N, 2) + Y)) = 1 AND SHIP(PLAYER(P, N, 4)) = 1 AND inair(PLAYER(P, N, 4)) = 0 THEN
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
        G$ = SHOW$(P, N, 0, 1)
  END IF
  WHO = INT(WHOSTHERE(PLAYER(P, N, 1) + X, PLAYER(P, N, 2) + Y) / 100)
  IF WHO <> P AND WHO <> 0 THEN
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
        G$ = SHOW$(P, N, 0, 1)
  END IF
  IF MOVED = 1 THEN
  'WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = 0:
  PLAYER(P, N, 1) = PLAYER(P, N, 1) + X: PLAYER(P, N, 2) = PLAYER(P, N, 2) + Y
  IF inair(PLAYER(P, N, 4)) = 0 THEN PLAYER(P, N, 5) = PLAYER(P, N, 5) - 1
  IF inair(PLAYER(P, N, 4)) THEN MOVES = MOVES + 1 ELSE MOVES = MOVES + movepen(terain(PLAYER(P, N, 1), PLAYER(P, N, 2)))
  END IF
  'WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = (100 * P) + N
  IF suplyPT(terain(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 1 THEN PLAYER(P, N, 5) = MAXSUP(PLAYER(P, N, 4))
  IF MOVE(PLAYER(P, N, 4)) - MOVES > 0 THEN QUICKINFO P, N, TURNS, MOVE(PLAYER(P, N, 4)) - MOVES
  IF MOVES >= MOVE(PLAYER(P, N, 4)) THEN WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = (100 * P) + N
  LOOP UNTIL MOVES >= MOVE(PLAYER(P, N, 4))
  WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = (100 * P) + N
  IF PLAYER(P, N, 5) < 1 AND sustain(terain(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN
        PLAYER(P, N, 3) = 0
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
        G$ = SHOW$(P, N, 0, 1)
  END IF
  IF inair(PLAYER(P, N, 4)) = 1 THEN PLAYER(P, N, 5) = PLAYER(P, N, 5) - 1
  IF PLAYER(P, N, 5) < 1 AND inair(PLAYER(P, N, 4)) = 1 THEN
        PLAYER(P, N, 3) = 0
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
        G$ = SHOW$(P, N, 0, 1)
  END IF
 END IF
NEXT N
END SUB

SUB PIC
SCREEN 0
WIDTH , 43
FOR N = 1 TO 60
FOR S = 1 TO 40
COLOR 0, TECOL(terain(N, S))
LOCATE S, N + 20: PRINT TER$(terain(N, S))
NEXT S, N
END SUB

SUB posforces (P)
SCREEN 0
COLOR 0, 0
CLS
IF P = 1 THEN MIN = 1: MAX = 30
IF P = 2 THEN MIN = 31: MAX = 59
PIC
FOR N = 1 TO 90
'X = min
'Y = 1
IF inair(PLAYER(P, N, 4)) = 1 THEN
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
LOCATE Y, X + 20: COLOR 0, TECOL(terain(X, Y)): PRINT TER$(terain(X, Y))
LOCATE Y, X + 20: COLOR 30, 0: PRINT LET$(PLAYER(P, N, 4))
IF PLAYER(P, N, 3) > 0 THEN
DO
 COLOR 12, 0
 LOCATE 1, 1
 PRINT "  VANQUISHER v1.0"
 COLOR 15, 0
 LOCATE 3, 1
 PRINT STRING$(20, 196)
 PRINT "Forces: ";
 COLOR 12, 0: PRINT "Player "; P
 COLOR 15, 0
 PRINT STRING$(20, 196)
 PRINT
 PRINT "Type of unit:"
 COLOR 12, 0
 PRINT " "; FULL(PLAYER(P, N, 4))
 COLOR 15, 0
 PRINT
 PRINT "Range of attack:";
 COLOR 12, 0: PRINT MAXRANGE(PLAYER(P, N, 4))
 COLOR 15, 0
 PRINT
 PRINT "Supply  status:";
 COLOR 12, 0: PRINT PLAYER(P, N, 5)
 COLOR 15, 0
 PRINT
 PRINT "Movement range:";
 COLOR 12, 0: PRINT MOVE(PLAYER(P, N, 4))
 COLOR 15, 0
 PRINT
 PRINT STRING$(20, 196)
 PRINT "Terrain: ";
 COLOR 12, 0: PRINT TERFUL(terain(X, Y))
 COLOR 15, 0
 PRINT STRING$(20, 196)
 PRINT
 PRINT "Defence bonus:";
 COLOR 12, 0: PRINT defbonus(terain(X, Y)); " "
 COLOR 15, 0
 PRINT
 PRINT "Movement cost:";
 COLOR 12, 0: PRINT movepen(terain(X, Y)); " "
 COLOR 15, 0
 PRINT
 PRINT "Supply point?";
 COLOR 12, 0
 IF suplyPT(terain(X, Y)) = 1 THEN PRINT " Yes" ELSE PRINT " No "
 COLOR 15, 0
 PRINT
 PRINT "Suitable for Navy?"
 COLOR 12, 0
 IF LANDONLY(terain(X, Y)) = 0 THEN PRINT " Yes" ELSE PRINT " No "
 COLOR 15, 0
 PRINT
 PRINT "Suitable for Army?"
 COLOR 12, 0
 IF SHIPONLY(terain(X, Y)) = 0 THEN PRINT " Yes" ELSE PRINT " No "
 PRINT
 PRINT
 COLOR 11
 PRINT "Choose your troop"
 PRINT "positions player"; P
 PRINT
 PRINT "Use numeric pad "
 PRINT "cursors to change"
 PRINT "position and 5 to"
 PRINT "fix location of"
 PRINT "each unit"
 A$ = INPUT$(1)
  LOCATE Y, X + 20: COLOR 0, TECOL(terain(X, Y)): PRINT TER$(terain(X, Y))
  IF A$ = "5" THEN
  OK = 1
  IF WHOSTHERE(X, Y) <> 0 THEN OK = 0
  IF LANDONLY(terain(X, Y)) = 1 AND SHIP(PLAYER(P, N, 4)) AND inair(PLAYER(P, N, 4)) = 0 THEN OK = 0
  IF SHIPONLY(terain(X, Y)) = 1 AND SHIP(PLAYER(P, N, 4)) = 0 AND inair(PLAYER(P, N, 4)) = 0 THEN OK = 0
  IF OK THEN EXIT DO
  END IF
  IF A$ = "1" THEN DY = 1: DX = -1
  IF A$ = "2" THEN DY = 1: DX = 0
  IF A$ = "3" THEN DY = 1: DX = 1
  IF A$ = "4" THEN DX = -1: DY = 0
  IF A$ = "6" THEN DX = 1: DY = 0
  IF A$ = "7" THEN DY = -1: DX = -1
  IF A$ = "8" THEN DY = -1: DX = 0
  IF A$ = "9" THEN DY = -1: DX = 1
  IF X + DX > MAX THEN DX = 0
  IF X + DX < MIN THEN DX = 0
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
IF P = 2 THEN
SCREEN 12
FOR t = 1 TO 40
FOR S = 1 TO 60
LINE (20 + (S * 2), (t * 2) + 395)-STEP(2, 2), TECOL(terain(S, t)), BF
NEXT S, t
GET (20, 395)-STEP(120, 80), MAP
END IF
END SUB

SUB QUICKINFO (P, N, TURNS, MOVES)
COLOR 12
LOCATE 5, 17
PRINT P
LOCATE 9, 2
PRINT " "; FULL(PLAYER(P, N, 4))
LOCATE 11, 4
PRINT MAXRANGE(PLAYER(P, N, 4))
LOCATE 13, 4
PRINT MOVES
LOCATE 15, 4
PRINT INT(PLAYER(P, N, 3)); "  "
LOCATE 17, 4
PRINT PLAYER(P, N, 5); "  "
LOCATE 20, 10
PRINT TERFUL(terain(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 25, 4
PRINT defbonus(terain(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 27, 4
PRINT movepen(terain(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 29, 4
IF supply(terain(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 1 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 31, 4
IF LANDONLY(terain(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 33, 4
IF SHIPONLY(terain(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 35, 4
PRINT TURNS
PUT (20, 395), MAP, PSET
LINE (20 + (2 * PLAYER(P, N, 1)) - 10, 395 + (2 * PLAYER(P, N, 2)) - 10)-STEP(20, 20), 0, B
FOR t = 1 TO 2
COLOR 0 + (t - 1) * 15
FOR S = 1 TO 30
IF PLAYER(t, S, 3) > 0 THEN LINE (20 + ((PLAYER(t, S, 1) - 1) * 2), (2 * (PLAYER(t, S, 2) - 1)) + 394)-STEP(2, 2), , BF
NEXT S, t
END SUB

FUNCTION readobj$ (filename$)
OPEN filename$ FOR INPUT AS #1
'INPUT #1, INTRO$
'SHELL INTRO$
INPUT #1, TERFILE$
INPUT #1, ARMYUSE, NAVYUSE
FOR N = 1 TO 2
INPUT #1, LANDX(N), LANDY(N), SEAX(N), SEAY(N), AIRX(N), AIRY(N)
NEXT N
INPUT #1, MONEY(1), MONEY(2)
FOR N = 1 TO 2
DO
FIRST = FIRST + 1
DO
BIT = BIT + 1
INPUT #1, A$
OBJECTIVEX(N, FIRST, BIT) = VAL(A$)
IF ASC(A$) = 4 THEN
  OBJECTIVEX(N, FIRST, BIT) = 0
  EXIT DO
END IF
IF ASC(A$) = 2 THEN
 EXT = 1
 OBJECTIVEX(N, FIRST, BIT) = -1
 EXIT DO
END IF
INPUT #1, objectivey(N, FIRST, BIT)
LOOP
IF EXT = 1 THEN EXT = 0: EXIT DO
BIT = 0
LOOP
FIRST = 0
BIT = 0
NEXT N
INPUT #1, whichplayer, NUMBERTURNS
readobj$ = TERFILE$
CLOSE
END FUNCTION

SUB readterain (filename$)
'OPEN filename$ FOR INPUT AS #1
'filename$ = LEFT$(filename$, LEN(filename$) - 4)
'filename$ = filename$ + ".gph"
OPEN filename$ FOR INPUT AS #2
FOR N = 1 TO 40
FOR t = 1 TO 60
'INPUT #1, TERAIN(T, N)
INPUT #2, GRAPHICS(t, N)
SELECT CASE GRAPHICS(t, N)
 CASE 1
        terain(t, N) = 9
 CASE 2
        terain(t, N) = 2
 CASE 3
        terain(t, N) = 4
 CASE 4
        terain(t, N) = 1
 CASE 5 TO 18, 23 TO 26
        terain(t, N) = 6
 CASE 19 TO 23, 27 TO 39, 44
        terain(t, N) = 7
 CASE 40
        terain(t, N) = 5
 CASE 41
        terain(t, N) = 8
 CASE 42
        terain(t, N) = 10
 CASE 43
        terain(t, N) = 3
END SELECT

NEXT t, N
CLOSE
END SUB

FUNCTION SHOW$ (P, N, OK, OK2)
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

FOR S = 1 TO 10
FOR D = 1 TO 10
S1 = S - 1
D1 = D - 1
XS1 = X + S1
YD1 = Y + D1
S2 = 160 + S1 * 48
D2 = 48 * D1
PUT (S2, D2), PICCY(0, GRAPHICS(XS1, YD1)), PSET
IF WHOSTHERE(XS1, YD1) > 0 THEN
R = INT(WHOSTHERE(XS1, YD1) / 100)
IF R = 1 THEN TF = 0
IF R = 2 THEN TF = 20
H = WHOSTHERE(XS1, YD1) - (R * 100)
PUT (S2 + 10, D2 + 10), UNITS(0, TF + PLAYER(R, H, 4) + PLAYER(R, H, 6)), PSET
END IF
NEXT D, S

NODRAW:
IF OK = 0 THEN PUT (160 + (PLAYER(P, N, 1) - X) * 48, (PLAYER(P, N, 2) - Y) * 48), PICCY(0, GRAPHICS(PLAYER(P, N, 1), PLAYER(P, N, 2))), PSET: EXIT FUNCTION
DO
A$ = INKEY$
PUT (160 + (PLAYER(P, N, 1) - X) * 48, (PLAYER(P, N, 2) - Y) * 48), PICCY(0, GRAPHICS(PLAYER(P, N, 1), PLAYER(P, N, 2))), PSET
FOR O = 1 TO 500
A$ = INKEY$
IF A$ <> "" THEN EXIT FOR
NEXT O
IF A$ <> "" THEN EXIT DO
PUT (170 + (PLAYER(P, N, 1) - X) * 48, 10 + ((PLAYER(P, N, 2) - Y) * 48)), UNITS(0, PLAYER(P, N, 4) + PLAYER(P, N, 6) + (P - 1) * 20), PSET
FOR O = 1 TO 500
A$ = INKEY$
IF A$ <> "" THEN EXIT FOR
NEXT O
LOOP WHILE A$ = ""
SHOW$ = A$
IF WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) <> 0 THEN
I = INT(WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) / 100)
G = WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) - (I * 100)
PUT (170 + (PLAYER(P, N, 1) - X) * 48, 10 + (PLAYER(P, N, 2) - Y) * 48), UNITS(0, PLAYER(I, G, 4) + PLAYER(I, G, 6) + (I - 1) * 20), PSET
ELSE
PUT (160 + (PLAYER(P, N, 1) - X) * 48, (PLAYER(P, N, 2) - Y) * 48), PICCY(0, GRAPHICS(PLAYER(P, N, 1), PLAYER(P, N, 2))), PSET
END IF
END FUNCTION

SUB WHOSWON
IF TURNS > NUMBERTURNS THEN PRINT "Player "; whichplayer; " Has Won": SHELL endseq$: END
FOR N = 1 TO 2
true = 0
IF N = 1 THEN opp = 2 ELSE opp = 1
DO
FIRST = FIRST + 1
DO
BIT = BIT + 1
IF OBJECTIVEX(N, FIRST, BIT) = 0 THEN EXIT DO
IF OBJECTIVEX(N, FIRST, BIT) = -1 THEN EXT = 1: EXIT DO
whicplay = INT(WHOSTHERE(OBJECTIVEX(N, FIRST, BIT), objectivey(N, FIRST, BIT)) / 100)
IF whicplay = N THEN true = 1
IF whicplay = opp THEN true = 0: EXT = 1: EXIT DO
LOOP
IF EXT = 1 THEN EXIT DO
BIT = 0
EXT = 0
LOOP
IF true = 1 THEN PRINT "Player "; N; " Has won":  END
FIRST = 0
BIT = 0
NEXT N
FOR P = 1 TO 2
IF P = 1 THEN pn = 2 ELSE pn = 1
true = 1
FOR N = 1 TO 30
IF PLAYER(P, N, 3) > 0 THEN true = 0
NEXT N
IF true = 1 THEN PRINT "Player "; pn; " Has Won": END
NEXT P
END SUB

