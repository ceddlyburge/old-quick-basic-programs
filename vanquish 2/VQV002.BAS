DECLARE SUB QUICKINFO (P!, N!, TURNS!, MOVES!)
DECLARE SUB PIC ()
DECLARE SUB readterain (filename$)
DECLARE SUB posforces (P!)
DECLARE SUB chooseunit (P!, money!)
DECLARE SUB INFO2 (P!, N!, MOVES)
DECLARE SUB combat1 (N!, P)
DECLARE SUB MOVEUNIT (P)
DECLARE SUB init ()
DECLARE FUNCTION SHOW$ (P, N, OK)
'-----------------------------------------------------------------------------
'
' VANQUISH V1.0
' By the skillful Cedd and Simon (with entertainment supplied by Ian)
'
'-----------------------------------------------------------------------------
DIM SHARED MAP(2596)
DIM SHARED PLAYER(2, 30, 5)
DIM SHARED LET$(16)
DIM SHARED move(16)
DIM SHARED MAXRANGE(16)
DIM SHARED probhit(16)
DIM SHARED damage(16)
DIM SHARED hitp(16)
DIM SHARED maxsup(16)
DIM SHARED maxnum(16)
DIM SHARED cost(16)
DIM SHARED supply(16)
DIM SHARED INAIR(16)
DIM SHARED SHIP(16)
DIM SHARED TERAIN(60, 40)
DIM SHARED movepen(30)
DIM SHARED SHIPONLY(30)
DIM SHARED suplyPT(30)
DIM SHARED defbonus(30)
DIM SHARED LANDONLY(30)
DIM SHARED CO(2)
DIM SHARED samcap(16)
DIM SHARED full(16) AS STRING * 18
DIM SHARED TER$(30)
DIM SHARED TERFUL(30) AS STRING * 10
DIM SHARED sustain(30)
DIM SHARED TECOL(30)
DIM SHARED WHOSTHERE(60, 40)
COMMON SHARED col1, col2, TURNS
'-----------------------------------------------------------------------------
SCREEN 9
RANDOMIZE TIMER
CO(1) = 1
CO(2) = 4
init
DEF SEG = 0
POKE &H417, &HF0
readterain "c:\VANQUISH\scene2.ter"
chooseunit 1, 100
chooseunit 2, 100
posforces 1
posforces 2
SCREEN 12
WIDTH , 60
INFO2 1, 1, 1
DO
TURNS = TURNS + 1
MOVEUNIT (1)
MOVEUNIT (2)
LOOP
END

SUB chooseunit (P, money)
DIM army(16)
SCREEN 0
WIDTH , 43
COLOR 4, 0
LOCATE 1, 1: PRINT "                               VANQUISHER V1.0"
PRINT
PRINT STRING$(80, 196)
COLOR 15
LOCATE 5, 1: PRINT "Player "; P; " Please choose your forces. Funds available= "; money
COLOR 11
LOCATE 7, 1: PRINT "Numeric pad cursors 2 & 8 moves cursor up and down."
PRINT "Numeric pad + & - changes number in force. ENTER ends selection."
PRINT
COLOR 4, 0
PRINT STRING$(80, 196)
COLOR 15, 4
LOCATE 13, 1: PRINT "Name of Unit        Move range             Cost             No in unit             "
LOCATE 14, 1: PRINT "                              Resupply time      Attack range        No in force"
curs = 1
DO
COLOR 15, 0
FOR N = 1 TO 10
LOCATE 15 + (N * 2), 1: PRINT SPACE$(80)
LOCATE 15 + (N * 2), 1: PRINT full(N);
LOCATE , 25: PRINT move(N);
LOCATE , 35: PRINT maxsup(N);
LOCATE , 45: PRINT cost(N);
LOCATE , 55: PRINT MAXRANGE(N);
LOCATE , 65: PRINT maxnum(N);
LOCATE , 75: PRINT army(N); "   "
NEXT N
ct = 0
FOR N = 1 TO 10
ct = ct + army(N) * cost(N)
NEXT N
LOCATE 37, 64: COLOR 15, 4: PRINT "Total Cost "; ct
IF ct > money THEN BEEP: army(curs) = army(curs) - 1
COLOR 0, 7
LOCATE 15 + (curs * 2), 1: PRINT SPACE$(80)
LOCATE 15 + (curs * 2), 1: PRINT full(curs);
LOCATE , 25: PRINT move(curs);
LOCATE , 35: PRINT maxsup(curs);
LOCATE , 45: PRINT cost(curs);
LOCATE , 55: PRINT MAXRANGE(curs);
LOCATE , 65: PRINT maxnum(curs);
LOCATE , 75: PRINT army(curs)
A$ = INPUT$(1)
IF A$ = "8" THEN curs = curs - 1
IF A$ = "2" THEN curs = curs + 1
IF curs = 0 THEN curs = 10
IF curs = 11 THEN curs = 1
IF A$ = "+" THEN army(curs) = army(curs) + 1
IF A$ = "-" THEN army(curs) = army(curs) - 1
IF A$ = CHR$(13) THEN EXIT DO
IF army(curs) < 0 THEN army(curs) = 0
LOOP
FOR N = 1 TO 10
FOR T = 1 TO army(N)
IF army(N) = 0 THEN EXIT FOR
count = count + 1
PLAYER(P, count, 3) = maxnum(N)
PLAYER(P, count, 4) = N
PLAYER(P, count, 5) = maxsup(N)
IF count > 29 THEN EXIT SUB
NEXT T, N
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
IF Y2 > 30 THEN Y2 = 30
IF X2 > 50 THEN X2 = 50
DO
  x3 = 160 + ((PLAYER(P, N, 1) - X2) * 48)
  y3 = (PLAYER(P, N, 2) - Y2) * 48
  LINE (x3 + (X * 48), y3 + (Y * 48))-STEP(48, 48), 13, BF
  A$ = INPUT$(1)
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
  A$ = SHOW$(P, N, 0)
  LOOP
  found = 0
  FOR O = 1 TO 30
  IF X = PLAYER(pn, O, 1) AND Y = PLAYER(pn, O, 2) THEN found = 1: EXIT FOR
  NEXT O
  hp = 0
  IF found = 1 THEN
  FOR T = 1 TO PLAYER(P, N, 3)
  IF RND < probhit(PLAYER(P, N, 4)) THEN hp = hp + damage(PLAYER(P, N, 4))
  NEXT T
  killed = hp / hitp(PLAYER(pn, O, 4)) / defbonus(TERAIN(PLAYER(pn, O, 1), PLAYER(pn, O, 2)))
   PLAYER(pn, O, 3) = PLAYER(pn, O, 3) - killed
  END IF
 IF supply(PLAYER(P, N, 4)) = 1 THEN
  found = 0
  FOR O = 1 TO 30
  IF X = PLAYER(P, O, 1) AND Y = PLAYER(P, O, 2) THEN found = 1: EXIT FOR
  NEXT O
  PLAYER(P, O, 5) = maxsup(PLAYER(P, O, 4))
  PLAYER(P, N, 5) = PLAYER(P, N, 5) - maxsup(PLAYER(P, O, 4))
 END IF
END SUB

SUB INFO2 (P, N, MOVES)
LINE (0, 0)-(160, 480), 0, BF
COLOR 12
LOCATE 1, 1
PRINT "  VANQUISHER V1.0"
COLOR 15
LOCATE 3, 1
PRINT STRING$(20, 196)
PRINT "Forces: ";
COLOR 12: PRINT "Player "; P
COLOR 15
PRINT STRING$(20, 196)
PRINT
PRINT "Type of unit:"
COLOR 12
PRINT " "; full(PLAYER(P, N, 4))
COLOR 15
PRINT
PRINT "Range:";
COLOR 12: PRINT MAXRANGE(PLAYER(P, N, 4))
COLOR 15
PRINT
PRINT "Moves left:";
COLOR 12: PRINT MOVES
PRINT
COLOR 15
PRINT "No. left in unit:"
COLOR 12
PRINT INT(PLAYER(P, N, 3))
PRINT
COLOR 15
PRINT "Supply status:";
COLOR 12: PRINT PLAYER(P, N, 5)
COLOR 15
PRINT
PRINT STRING$(20, 196)
PRINT "Terrain: ";
COLOR 12: PRINT TERFUL(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
COLOR 15
PRINT STRING$(20, 196)
PRINT
PRINT "Defence bonus:";
COLOR 12: PRINT defbonus(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
PRINT
COLOR 15
PRINT "Movement cost:";
COLOR 12: PRINT movepen(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
PRINT
COLOR 15
PRINT "Supply point?";
COLOR 12: IF supply(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 1 THEN PRINT " Yes" ELSE PRINT " No "
COLOR 15
PRINT
PRINT "Suitable for Navy?"
COLOR 12
IF LANDONLY(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
PRINT
COLOR 15
PRINT "Suitable for Army?"
COLOR 12
IF SHIPONLY(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
COLOR 15
PRINT
PRINT "Turn:";
COLOR 12: PRINT TURNS
''FOR t = 1 TO 40
'FOR s = 1 TO 60
'LINE (s * 2, (t * 2) + 270)-STEP(2, 2), TECOL(terain(s, t)), BF
'NEXT s, t
PUT (20, 395), MAP, PSET
FOR T = 1 TO 2
FOR S = 1 TO 30
IF PLAYER(T, S, 3) > 0 THEN LINE (20 + ((PLAYER(T, S, 1) - 1) * 2), (2 * (PLAYER(T, S, 2))) + 395)-STEP(2, 2), 0 + (T - 1) * 15, BF
NEXT S, T
END SUB

SUB init
'descriptions of terrain -----------------------------------------------------
TERFUL(7) = "Sea/Lake"
TECOL(7) = 1
TER$(7) = "w"
movepen(7) = 1
defbonus(7) = 1.2
suplyPT(7) = 0
LANDONLY(7) = 0
SHIPONLY(7) = 1
sustain(7) = 1
TERFUL(6) = "River"
TECOL(6) = 9
TER$(6) = "r"
movepen(6) = 2
defbonus(6) = 2
suplyPT(6) = 0
LANDONLY(6) = 1
SHIPONLY(6) = 0
sustain(6) = 1
TERFUL(10) = "Urbanised"
TECOL(10) = 7
TER$(10) = "u"
movepen(10) = .5
defbonus(10) = 3
suplyPT(10) = 1
LANDONLY(10) = 1
SHIPONLY(10) = 0
sustain(10) = 1
TERFUL(9) = "Desert"
TECOL(9) = 14
TER$(9) = "d"
movepen(9) = 1
defbonus(9) = 1
suplyPT(9) = 0
LANDONLY(9) = 1
SHIPONLY(9) = 0
sustain(9) = 0
TERFUL(3) = "Woodland"
TECOL(3) = 2
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
TECOL(1) = 13
TERFUL(4) = "Grassland"
TER$(4) = "."
TECOL(4) = 10
movepen(4) = 1
defbonus(4) = 1
suplyPT(4) = 0
LANDONLY(4) = 1
SHIPONLY(4) = 0
sustain(4) = 1
TERFUL(2) = "Hills"
TER$(2) = "h"
TECOL(2) = 12
movepen(2) = 2
defbonus(2) = 2
suplyPT(2) = 0
LANDONLY(2) = 1
sustain(2) = 1
TERFUL(5) = "Wetland"
TER$(5) = "s"
TECOL(5) = 3
movepen(5) = 2
defbonus(5) = 1
suplyPT(5) = 0
LANDONLY(5) = 1
sustain(5) = 1
TERFUL(8) = "Tundra"
TER$(8) = "t"
TECOL(8) = 15
movepen(8) = 1
defbonus(8) = 1
suplyPT(8) = 0
LANDONLY(8) = 1
sustain(8) = 0

'descriptions of military units ----------------------------------------------
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

supply(4) = 1
supply(7) = 1
supply(10) = 1

INAIR(5) = 1
INAIR(6) = 1
INAIR(7) = 1

SHIP(8) = 1
SHIP(9) = 1
SHIP(10) = 1

maxsup(1) = 8
maxsup(2) = 6
maxsup(3) = 4
maxsup(4) = 50
maxsup(5) = 6
maxsup(6) = 8
maxsup(7) = 50
maxsup(8) = 24
maxsup(9) = 16
maxsup(10) = 50

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

full(1) = "light Infantry"
full(2) = "heavy aRmour"
full(3) = "Artillery battery"
full(4) = "logistical Support"
full(5) = "Bomber aircraft"
full(6) = "Fighter aircraft"
full(7) = "rEfueling aircraft"
full(8) = "missile Carrier"
full(9) = "Destroyer"
full(10) = "auXilliary ship"

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

END SUB

SUB MOVEUNIT (P)
FOR N = 1 TO 30
IF PLAYER(P, N, 3) > 0 THEN
  A$ = SHOW$(P, N, 0)
  QUICKINFO P, N, TURNS, move(PLAYER(P, N, 4))
  MOVES = 0
  DO
  A$ = SHOW$(P, N, 1)
  IF A$ = "0" AND MOVES = 0 AND PLAYER(P, N, 5) > 0 THEN
     combat1 P, N
     IF INAIR(PLAYER(P, N, 4)) = 0 THEN PLAYER(P, N, 5) = PLAYER(P, N, 5) - 1
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
  MOVED = 1
  IF PLAYER(P, N, 1) + X < 1 THEN MOVED = 0: X = 0: Y = 0
  IF PLAYER(P, N, 1) + X > 59 THEN MOVED = 0: X = 0: Y = 0
  IF PLAYER(P, N, 2) + Y < 1 THEN MOVED = 0: X = 0: Y = 0
  IF PLAYER(P, N, 2) + Y > 40 THEN MOVED = 0: X = 0: Y = 0
  IF MOVED AND SHIPONLY(TERAIN(PLAYER(P, N, 1) + X, PLAYER(P, N, 2) + Y)) = 1 AND SHIP(PLAYER(P, N, 4)) = 0 THEN MOVED = 0
  IF MOVED AND LANDONLY(TERAIN(PLAYER(P, N, 1) + X, PLAYER(P, N, 2) + Y)) = 1 AND SHIP(PLAYER(P, N, 4)) = 1 THEN MOVED = 0
  IF MOVED = 1 THEN
  WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = 0:
  PLAYER(P, N, 1) = PLAYER(P, N, 1) + X: PLAYER(P, N, 2) = PLAYER(P, N, 2) + Y
  IF INAIR(PLAYER(P, N, 4)) = 0 THEN PLAYER(P, N, 5) = PLAYER(P, N, 5) - 1
  MOVES = MOVES + movepen(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
  WHOSTHERE(PLAYER(P, N, 1), PLAYER(P, N, 2)) = 100 * P + N
  END IF
  IF suplyPT(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 1 THEN PLAYER(P, N, 5) = maxsup(PLAYER(P, N, 4))
  IF move(PLAYER(P, N, 4)) - MOVES > 0 THEN QUICKINFO P, N, TURNS, move(PLAYER(P, N, 4)) - MOVES
  LOOP UNTIL MOVES >= move(PLAYER(P, N, 4))
  IF PLAYER(P, N, 5) < 1 AND sustain(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PLAYER(P, N, 3) = 0
  IF INAIR(PLAYER(P, N, 4)) = 1 THEN PLAYER(P, N, 5) = PLAYER(P, N, 5) - 1
  IF PLAYER(P, N, 5) < 1 AND INAIR(PLAYER(P, N, 4)) = 1 THEN PLAYER(P, N, 3) = 0
 END IF
NEXT N
END SUB

SUB PIC
SCREEN 0
WIDTH , 43
FOR N = 1 TO 60
FOR S = 1 TO 40
COLOR 0, TECOL(TERAIN(N, S))
LOCATE S, N + 20: PRINT TER$(TERAIN(N, S))
NEXT S, N
END SUB

SUB posforces (P)
COLOR 0, 0
CLS
IF P = 1 THEN min = 1: max = 30
IF P = 2 THEN min = 31: max = 59
PIC
FOR N = 1 TO 30
X = min
Y = 1
LOCATE Y, X + 20: COLOR 0, TECOL(TERAIN(X, Y)): PRINT TER$(TERAIN(X, Y))
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
 PRINT " "; full(PLAYER(P, N, 4))
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
 COLOR 12, 0: PRINT move(PLAYER(P, N, 4))
 COLOR 15, 0
 PRINT
 PRINT STRING$(20, 196)
 PRINT "Terrain: ";
 COLOR 12, 0: PRINT TERFUL(TERAIN(X, Y))
 COLOR 15, 0
 PRINT STRING$(20, 196)
 PRINT
 PRINT "Defence bonus:";
 COLOR 12, 0: PRINT defbonus(TERAIN(X, Y)); " "
 COLOR 15, 0
 PRINT
 PRINT "Movement cost:";
 COLOR 12, 0: PRINT movepen(TERAIN(X, Y)); " "
 COLOR 15, 0
 PRINT
 PRINT "Supply point?";
 COLOR 12, 0
 IF suplyPT(TERAIN(X, Y)) = 1 THEN PRINT " Yes" ELSE PRINT " No "
 COLOR 15, 0
 PRINT
 PRINT "Suitable for Navy?"
 COLOR 12, 0
 IF LANDONLY(TERAIN(X, Y)) = 0 THEN PRINT " Yes" ELSE PRINT " No "
 COLOR 15, 0
 PRINT
 PRINT "Suitable for Army?"
 COLOR 12, 0
 IF SHIPONLY(TERAIN(X, Y)) = 0 THEN PRINT " Yes" ELSE PRINT " No "
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
  LOCATE Y, X + 20: COLOR 0, TECOL(TERAIN(X, Y)): PRINT TER$(TERAIN(X, Y))
  IF A$ = "5" THEN
  OK = 1
  IF WHOSTHERE(X, Y) <> 0 THEN OK = 0
  IF LANDONLY(TERAIN(X, Y)) = 1 AND SHIP(PLAYER(P, N, 4)) AND INAIR(PLAYER(P, N, 4)) = 0 THEN OK = 0
  IF SHIPONLY(TERAIN(X, Y)) = 1 AND SHIP(PLAYER(P, N, 4)) = 0 AND INAIR(PLAYER(P, N, 4)) = 0 THEN OK = 0
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
END IF
PLAYER(P, N, 1) = X
PLAYER(P, N, 2) = Y
LOCATE Y, X + 20: COLOR 15, 0: PRINT LET$(PLAYER(P, N, 4))
WHOSTHERE(X, Y) = P * 100 + N
NEXT N
IF P = 2 THEN
SCREEN 12
FOR T = 1 TO 40
FOR S = 1 TO 60
LINE (20 + (S * 2), (T * 2) + 395)-STEP(2, 2), TECOL(TERAIN(S, T)), BF
NEXT S, T
GET (20, 395)-STEP(120, 80), MAP
END IF
END SUB

SUB QUICKINFO (P, N, TURNS, MOVES)
COLOR 12
LOCATE 4, 16
PRINT P
LOCATE 8, 1
PRINT " "; full(PLAYER(P, N, 4))
LOCATE 10, 7
PRINT MAXRANGE(PLAYER(P, N, 4))
LOCATE 12, 12
PRINT MOVES
LOCATE 15, 1
PRINT INT(PLAYER(P, N, 3)); "  "
LOCATE 17, 15
PRINT PLAYER(P, N, 5); "  "
LOCATE 20, 10
PRINT TERFUL(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 23, 15
PRINT defbonus(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 25, 15
PRINT movepen(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2)))
LOCATE 27, 14
IF supply(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 1 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 30, 1
IF LANDONLY(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 33, 1
IF SHIPONLY(TERAIN(PLAYER(P, N, 1), PLAYER(P, N, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 35, 6
PRINT TURNS
LINE (10, 385)-(140, 480), 0, BF
PUT (20, 395), MAP, PSET
FOR T = 1 TO 2
FOR S = 1 TO 30
IF PLAYER(T, S, 3) > 0 THEN LINE (20 + ((PLAYER(T, S, 1) - 1) * 2), (2 * (PLAYER(T, S, 2))) + 395)-STEP(2, 2), 0 + (T - 1) * 15, BF
NEXT S, T
END SUB

SUB readterain (filename$)
OPEN filename$ FOR INPUT AS #1
FOR N = 1 TO 40
FOR T = 1 TO 60
INPUT #1, TERAIN(T, N)
NEXT T, N
END SUB

FUNCTION SHOW$ (P, N, OK)
X = PLAYER(P, N, 1)
Y = PLAYER(P, N, 2)
X = X - 5
Y = Y - 5
IF X < 1 THEN X = 1
IF Y < 1 THEN Y = 1
IF Y > 30 THEN Y = 30
IF X > 50 THEN X = 50
FOR S = 1 TO 10
FOR D = 1 TO 10
COLOR TECOL(TERAIN(X + S - 1, Y + D - 1))
IF WHOSTHERE(X + S - 1, Y + D - 1) <> 0 THEN COLOR 0
LINE (160 + (S - 1) * 48, 48 * (D - 1))-STEP(48, 48), , BF
NEXT D, S
COLOR 15
LINE (20 + X * 2, 395 + Y * 2)-STEP(20, 20), 0, B
X2 = (184 + (PLAYER(P, N, 1) - X) * 48) - MAXRANGE(PLAYER(P, N, 4)) * 48
Y2 = (24 + (PLAYER(P, N, 2) - Y) * 48) - MAXRANGE(PLAYER(P, N, 4)) * 48
IF X2 < 160 THEN X2 = 160
IF X2 > 640 THEN X2 = 640
IF Y2 < 0 THEN Y2 = 0
IF Y2 > 480 THEN Y2 = 480
LINE (X2, Y2)-STEP(MAXRANGE(PLAYER(P, N, 4)) * 96, MAXRANGE(PLAYER(P, N, 4)) * 96), , B, &HF0F0
IF OK = 0 THEN EXIT FUNCTION
DO
A$ = INKEY$
C = C + 1
C = C MOD 2
COLOR C * 15
LINE (160 + (PLAYER(P, N, 1) - X) * 48, (PLAYER(P, N, 2) - Y) * 48)-STEP(48, 48), , BF
FOR O = 1 TO 10000: NEXT O
LOOP WHILE A$ = ""
SHOW$ = A$
END FUNCTION

