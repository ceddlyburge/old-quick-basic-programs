DECLARE SUB WHOSWON ()
DECLARE FUNCTION readobj$ (FILENAME$)
DECLARE SUB QUICKINFO (p!, n!, TURNS!, MOVES!)
DECLARE SUB PIC ()
DECLARE SUB readterain (FILENAME$)
DECLARE SUB posforces (p!)
DECLARE SUB chooseunit (p!, money!)
DECLARE SUB INFO2 (p!, n!, MOVES)
DECLARE SUB combat1 (n!, p)
DECLARE SUB MOVEUNIT (p)
DECLARE SUB INIT ()
DECLARE FUNCTION SHOW$ (p, n, OK)
'-----------------------------------------------------------------------------
'
' VANQUISH V1.0
' By the skillful Cedd and Simon (with entertainment supplied by Ian)
'
'-----------------------------------------------------------------------------
DIM SHARED MAP(2596)
DIM SHARED player(2, 30, 5)
DIM SHARED LET$(16)
DIM SHARED move(16)
DIM SHARED MAXRANGE(16)
DIM SHARED probhit(16)
DIM SHARED damage(16)
DIM SHARED hitp(16)
DIM SHARED MAXSUP(16)
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
DIM SHARED SAMCAP(16)
DIM SHARED full(16) AS STRING * 18
DIM SHARED TER$(30)
DIM SHARED TERFUL(30) AS STRING * 10
DIM SHARED sustain(30)
DIM SHARED TECOL(30)
DIM SHARED whosthere(60, 40)
DIM SHARED OBJECTIVEX(2, 10, 15)
DIM SHARED objectivey(2, 10, 15)
DIM SHARED LANDX(2), LANDY(2), SEAX(2), SEAY(2), AIRX(2), AIRY(2)
DIM SHARED money(2)
COMMON SHARED col1, col2, TURNS, ARMYUSE, NAVYUSE, whichplayer
COMMON SHARED NUMBERTURNS, endseq$
'-----------------------------------------------------------------------------
'ON TIMER(1) GOSUB PT:
'TIMER ON
SCREEN 9
RANDOMIZE TIMER
CO(1) = 1
CO(2) = 4
DEF SEG = 0
POKE &H417, &HF0
B$ = readobj$("A:mission1.msn")
readterain B$
INIT
chooseunit 1, money(1)
chooseunit 2, money(2)
posforces 1
posforces 2
SCREEN 12
WIDTH , 60
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

SUB chooseunit (p, money)
DIM army(16)
SCREEN 0
WIDTH , 43
COLOR 4, 0
LOCATE 1, 1: PRINT "                               VANQUISHER V1.0"
PRINT
PRINT STRING$(80, 196)
COLOR 15
LOCATE 5, 1: PRINT "Player "; p; " Please choose your forces. Funds available = "; money
COLOR 11
LOCATE 7, 1: PRINT "Numeric pad cursors 2 & 8 moves cursor up and down."
PRINT "Numeric pad + & - changes number in force. ENTER ends selection."
PRINT
COLOR 4, 0
PRINT STRING$(80, 196)
COLOR 15, 4
LOCATE 13, 1: PRINT "Name of Unit        Move range             Cost             No in unit             "
LOCATE 14, 1: PRINT "                              Resupply time      Attack range        No in force"
CURS = 1
MIN = 1
MAX = 10
IF NAVYUSE = 0 THEN MAX = 7
IF ARMYUSE = 0 THEN MIN = 4
CURS = MIN
DO
COLOR 15, 0
FOR n = MIN TO MAX
LOCATE 15 + (n * 2), 1: PRINT SPACE$(80)
LOCATE 15 + (n * 2), 1: PRINT full(n);
LOCATE , 25: PRINT move(n);
LOCATE , 35: PRINT MAXSUP(n);
LOCATE , 45: PRINT cost(n);
LOCATE , 55: PRINT MAXRANGE(n);
LOCATE , 65: PRINT maxnum(n);
LOCATE , 75: PRINT army(n); "   "
NEXT n
ct = 0
FOR n = MIN TO MAX
ct = ct + army(n) * cost(n)
NEXT n
LOCATE 37, 64: COLOR 15, 4: PRINT "Total Cost "; ct
IF ct > money THEN BEEP: army(CURS) = army(CURS) - 1
COLOR 0, 7
LOCATE 15 + (CURS * 2), 1: PRINT SPACE$(80)
LOCATE 15 + (CURS * 2), 1: PRINT full(CURS);
LOCATE , 25: PRINT move(CURS);
LOCATE , 35: PRINT MAXSUP(CURS);
LOCATE , 45: PRINT cost(CURS);
LOCATE , 55: PRINT MAXRANGE(CURS);
LOCATE , 65: PRINT maxnum(CURS);
LOCATE , 75: PRINT army(CURS)
A$ = INPUT$(1)
IF A$ = "8" THEN CURS = CURS - 1
IF A$ = "2" THEN CURS = CURS + 1
IF CURS = MIN - 1 THEN CURS = MAX
IF CURS = MAX + 1 THEN CURS = MIN
IF A$ = "+" THEN army(CURS) = army(CURS) + 1
IF A$ = "-" THEN army(CURS) = army(CURS) - 1
IF A$ = CHR$(13) THEN EXIT DO
IF army(CURS) < 0 THEN army(CURS) = 0
LOOP
FOR n = MIN TO MAX
FOR T = 1 TO army(n)
IF army(n) = 0 THEN EXIT FOR
count = count + 1
player(p, count, 3) = maxnum(n)
player(p, count, 4) = n
player(p, count, 5) = MAXSUP(n)
IF count > 29 THEN EXIT SUB
NEXT T, n
END SUB

SUB combat1 (p, n)
X = 0
Y = 0
maxx = MAXRANGE(player(p, n, 4))
maxy = MAXRANGE(player(p, n, 4))
minx = -MAXRANGE(player(p, n, 4))
miny = -MAXRANGE(player(p, n, 4))
X1 = player(p, n, 1)
Y1 = player(p, n, 2)
X2 = X1 - 5
Y2 = Y1 - 5
IF X2 < 1 THEN X2 = 1
IF Y2 < 1 THEN Y2 = 1
IF Y2 > 31 THEN Y2 = 31
IF X2 > 51 THEN X2 = 51
'-------------------------POSITIONING SQUARE OF ATTACK------------------------
DO
  X3 = 160 + ((player(p, n, 1) - X2) * 48)
  Y3 = (player(p, n, 2) - Y2) * 48
  LINE (X3 + (X * 48), Y3 + (Y * 48))-STEP(48, 48), 13, BF
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
  IF X + player(p, n, 1) > 60 THEN X = X - 1
  IF X + player(p, n, 1) < 1 THEN X = X + 1
  IF Y + player(p, n, 2) > 40 THEN Y = Y - 1
  IF Y + player(p, n, 2) < 1 THEN Y = Y + 1
  A$ = SHOW$(p, n, 0)
  LOOP
'-----------------------------------------------------------------------------
'------------------------------DECIDING DAMAGE--------------------------------
  HP = 0
  IF whosthere(X1 + X, Y1 + Y) <> 0 THEN
    true = 1
    pn = INT(whosthere(X1 + X, Y1 + Y) / 100)
    O = whosthere(X1 + X, Y1 + Y) - (pn * 100)
    IF INAIR(player(p, n, 4)) = 1 AND INAIR(player(pn, O, 4)) = 0 AND SAMCAP(player(p, n, 4)) = 0 THEN true = 0
    IF INAIR(player(p, n, 4)) = 0 AND INAIR(player(pn, O, 4)) = 1 AND SAMCAP(player(p, n, 4)) = 0 THEN true = 0
    IF true THEN
    FOR T = 1 TO player(p, n, 3)
    IF RND < probhit(player(p, n, 4)) THEN HP = HP + damage(player(p, n, 4))
    NEXT T
    killed = HP / hitp(player(pn, O, 4)) / defbonus(TERAIN(player(pn, O, 1), player(pn, O, 2)))
    player(pn, O, 3) = player(pn, O, 3) - killed
    IF player(pn, O, 3) < 1 THEN whosthere(player(pn, O, 1), player(pn, O, 2)) = 0
    END IF
  END IF
'-----------------------------------------------------------------------------
'--------------------SUPPLYING VEHICULES--------------------------------------
 IF supply(player(p, n, 4)) = 1 AND whosthere(X1 + X, Y1 + Y) <> 0 THEN
    pn = INT(whosthere(X1 + X, Y1 + Y) / 100)
    O = VAL(RIGHT$(STR$(whosthere(X1 + X, Y1 + Y)), 2))
  player(pn, O, 5) = MAXSUP(player(pn, O, 4))
  player(p, n, 5) = player(p, n, 5) - MAXSUP(player(p, O, 4))
 END IF
'-----------------------------------------------------------------------------
END SUB

SUB INFO2 (p, n, MOVES)
LINE (0, 0)-(160, 480), 0, BF
COLOR 12
LOCATE 1, 1
PRINT "  VANQUISHER V1.0"
COLOR 15
LOCATE 3, 1
PRINT STRING$(20, 196)
PRINT "Forces: ";
COLOR 12: PRINT "Player "; p
COLOR 15
PRINT STRING$(20, 196)
PRINT
PRINT "Type of unit:"
COLOR 12
PRINT " "; full(player(p, n, 4))
COLOR 15
PRINT
PRINT "Range:";
COLOR 12: PRINT MAXRANGE(player(p, n, 4))
COLOR 15
PRINT
PRINT "Moves left:";
COLOR 12: PRINT MOVES
PRINT
COLOR 15
PRINT "No. left in unit:"
COLOR 12
PRINT INT(player(p, n, 3))
PRINT
COLOR 15
PRINT "Supply status:";
COLOR 12: PRINT player(p, n, 5)
COLOR 15
PRINT
PRINT STRING$(20, 196)
PRINT "Terrain: ";
COLOR 12: PRINT TERFUL(TERAIN(player(p, n, 1), player(p, n, 2)))
COLOR 15
PRINT STRING$(20, 196)
PRINT
PRINT "Defence bonus:";
COLOR 12: PRINT defbonus(TERAIN(player(p, n, 1), player(p, n, 2)))
PRINT
COLOR 15
PRINT "Movement cost:";
COLOR 12: PRINT movepen(TERAIN(player(p, n, 1), player(p, n, 2)))
PRINT
COLOR 15
PRINT "Supply point?";
COLOR 12: IF supply(TERAIN(player(p, n, 1), player(p, n, 2))) = 1 THEN PRINT " Yes" ELSE PRINT " No "
COLOR 15
PRINT
PRINT "Suitable for Navy?"
COLOR 12
IF LANDONLY(TERAIN(player(p, n, 1), player(p, n, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
PRINT
COLOR 15
PRINT "Suitable for Army?"
COLOR 12
IF SHIPONLY(TERAIN(player(p, n, 1), player(p, n, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
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
IF player(T, S, 3) > 0 THEN LINE (20 + ((player(T, S, 1) - 1) * 2), (2 * (player(T, S, 2))) + 395)-STEP(2, 2), 0 + (T - 1) * 15, BF
NEXT S, T
END SUB

SUB INIT
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

SUB MOVEUNIT (p)
FOR n = 1 TO 30
IF player(p, n, 3) > 0 THEN
  A$ = SHOW$(p, n, 0)
  QUICKINFO p, n, TURNS, move(player(p, n, 4))
  MOVES = 0
  DO
  A$ = SHOW$(p, n, 1)
  IF A$ = "0" AND MOVES = 0 AND player(p, n, 5) > 0 THEN
     combat1 p, n
     IF INAIR(player(p, n, 4)) = 0 THEN player(p, n, 5) = player(p, n, 5) - 1
     EXIT DO
  END IF
  IF A$ = "5" THEN EXIT DO
  IF player(p, n, 5) < 1 THEN EXIT DO
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
  IF player(p, n, 1) + X < 1 THEN MOVED = 0: X = 0: Y = 0
  IF player(p, n, 1) + X > 59 THEN MOVED = 0: X = 0: Y = 0
  IF player(p, n, 2) + Y < 1 THEN MOVED = 0: X = 0: Y = 0
  IF player(p, n, 2) + Y > 40 THEN MOVED = 0: X = 0: Y = 0
  IF MOVED AND SHIPONLY(TERAIN(player(p, n, 1) + X, player(p, n, 2) + Y)) = 1 AND SHIP(player(p, n, 4)) = 0 AND INAIR(player(p, n, 4)) = 0 THEN MOVED = 0
  IF MOVED AND LANDONLY(TERAIN(player(p, n, 1) + X, player(p, n, 2) + Y)) = 1 AND SHIP(player(p, n, 4)) = 1 AND INAIR(player(p, n, 4)) = 0 THEN MOVED = 0
  IF MOVED AND whosthere(player(p, n, 1) + X, player(p, n, 2) + Y) <> 0 THEN MOVED = 0: X = 0: Y = 0
  IF MOVED = 1 THEN
  whosthere(player(p, n, 1), player(p, n, 2)) = 0:
  player(p, n, 1) = player(p, n, 1) + X: player(p, n, 2) = player(p, n, 2) + Y
  IF INAIR(player(p, n, 4)) = 0 THEN player(p, n, 5) = player(p, n, 5) - 1
  IF INAIR(player(p, n, 4)) THEN MOVES = MOVES + 1 ELSE MOVES = MOVES + movepen(TERAIN(player(p, n, 1), player(p, n, 2)))
  whosthere(player(p, n, 1), player(p, n, 2)) = (100 * p) + n
  END IF
  IF suplyPT(TERAIN(player(p, n, 1), player(p, n, 2))) = 1 THEN player(p, n, 5) = MAXSUP(player(p, n, 4))
  IF move(player(p, n, 4)) - MOVES > 0 THEN QUICKINFO p, n, TURNS, move(player(p, n, 4)) - MOVES
  LOOP UNTIL MOVES >= move(player(p, n, 4))
  IF player(p, n, 5) < 1 AND sustain(TERAIN(player(p, n, 1), player(p, n, 2))) = 0 THEN player(p, n, 3) = 0
  IF INAIR(player(p, n, 4)) = 1 THEN player(p, n, 5) = player(p, n, 5) - 1
  IF player(p, n, 5) < 1 AND INAIR(player(p, n, 4)) = 1 THEN player(p, n, 3) = 0
 END IF
NEXT n
END SUB

SUB PIC
SCREEN 0
WIDTH , 43
FOR n = 1 TO 60
FOR S = 1 TO 40
COLOR 0, TECOL(TERAIN(n, S))
LOCATE S, n + 20: PRINT TER$(TERAIN(n, S))
NEXT S, n
END SUB

SUB posforces (p)
COLOR 0, 0
CLS
IF p = 1 THEN MIN = 1: MAX = 30
IF p = 2 THEN MIN = 31: MAX = 59
PIC
FOR n = 1 TO 30
'X = min
'Y = 1
IF INAIR(player(p, n, 4)) = 1 THEN
 X = LANDX(p)
 Y = LANDY(p)
END IF
IF SHIP(player(p, n, 4)) = 1 THEN
  X = SEAX(p)
  Y = SEAY(p)
 ELSE
  X = LANDX(p)
  Y = LANDY(p)
END IF
LOCATE Y, X + 20: COLOR 0, TECOL(TERAIN(X, Y)): PRINT TER$(TERAIN(X, Y))
LOCATE Y, X + 20: COLOR 30, 0: PRINT LET$(player(p, n, 4))
IF player(p, n, 3) > 0 THEN
DO
 COLOR 12, 0
 LOCATE 1, 1
 PRINT "  VANQUISHER v1.0"
 COLOR 15, 0
 LOCATE 3, 1
 PRINT STRING$(20, 196)
 PRINT "Forces: ";
 COLOR 12, 0: PRINT "Player "; p
 COLOR 15, 0
 PRINT STRING$(20, 196)
 PRINT
 PRINT "Type of unit:"
 COLOR 12, 0
 PRINT " "; full(player(p, n, 4))
 COLOR 15, 0
 PRINT
 PRINT "Range of attack:";
 COLOR 12, 0: PRINT MAXRANGE(player(p, n, 4))
 COLOR 15, 0
 PRINT
 PRINT "Supply  status:";
 COLOR 12, 0: PRINT player(p, n, 5)
 COLOR 15, 0
 PRINT
 PRINT "Movement range:";
 COLOR 12, 0: PRINT move(player(p, n, 4))
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
 PRINT "positions player"; p
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
  IF whosthere(X, Y) <> 0 THEN OK = 0
  IF LANDONLY(TERAIN(X, Y)) = 1 AND SHIP(player(p, n, 4)) AND INAIR(player(p, n, 4)) = 0 THEN OK = 0
  IF SHIPONLY(TERAIN(X, Y)) = 1 AND SHIP(player(p, n, 4)) = 0 AND INAIR(player(p, n, 4)) = 0 THEN OK = 0
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
  IF whosthere(X, Y) <> 0 THEN
  IF whosthere(X, Y) > 200 THEN TR = whosthere(X, Y) - 200: TP = 2 ELSE TR = whosthere(X, Y) - 100: TP = 1
  LOCATE Y, X + 20
  COLOR 15, 0
  PRINT LET$(player(TP, TR, 4))
  END IF
  X = X + DX
  Y = Y + DY
  LOCATE Y, X + 20: COLOR 30, 0: PRINT LET$(player(p, n, 4))
  LOOP
  player(p, n, 1) = X
  player(p, n, 2) = Y
  LOCATE Y, X + 20: COLOR 15, 0: PRINT LET$(player(p, n, 4))
  whosthere(X, Y) = (p * 100) + n
END IF
NEXT n
IF p = 2 THEN
SCREEN 12
FOR T = 1 TO 40
FOR S = 1 TO 60
LINE (20 + (S * 2), (T * 2) + 395)-STEP(2, 2), TECOL(TERAIN(S, T)), BF
NEXT S, T
GET (20, 395)-STEP(120, 80), MAP
END IF
END SUB

SUB QUICKINFO (p, n, TURNS, MOVES)
COLOR 12
LOCATE 4, 16
PRINT p
LOCATE 8, 1
PRINT " "; full(player(p, n, 4))
LOCATE 10, 7
PRINT MAXRANGE(player(p, n, 4))
LOCATE 12, 12
PRINT MOVES
LOCATE 15, 1
PRINT INT(player(p, n, 3)); "  "
LOCATE 17, 15
PRINT player(p, n, 5); "  "
LOCATE 20, 10
PRINT TERFUL(TERAIN(player(p, n, 1), player(p, n, 2)))
LOCATE 23, 15
PRINT defbonus(TERAIN(player(p, n, 1), player(p, n, 2)))
LOCATE 25, 15
PRINT movepen(TERAIN(player(p, n, 1), player(p, n, 2)))
LOCATE 27, 14
IF supply(TERAIN(player(p, n, 1), player(p, n, 2))) = 1 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 30, 1
IF LANDONLY(TERAIN(player(p, n, 1), player(p, n, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 33, 1
IF SHIPONLY(TERAIN(player(p, n, 1), player(p, n, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 35, 6
PRINT TURNS
LINE (10, 385)-(140, 480), 0, BF
PUT (20, 395), MAP, PSET
FOR T = 1 TO 2
FOR S = 1 TO 30
IF player(T, S, 3) > 0 THEN LINE (20 + ((player(T, S, 1) - 1) * 2), (2 * (player(T, S, 2))) + 395)-STEP(2, 2), 0 + (T - 1) * 15, BF
NEXT S, T
END SUB

FUNCTION readobj$ (FILENAME$)
OPEN FILENAME$ FOR INPUT AS #1
INPUT #1, INTRO$
SHELL INTRO$
INPUT #1, endseq$, TERFILE$
INPUT #1, ARMYUSE, NAVYUSE
FOR n = 1 TO 2
INPUT #1, LANDX(n), LANDY(n), SEAX(n), SEAY(n), AIRX(n), AIRY(n)
NEXT n
INPUT #1, money(1), money(2)
FOR n = 1 TO 2
DO
FIRST = FIRST + 1
DO
BIT = BIT + 1
INPUT #1, A$
OBJECTIVEX(n, FIRST, BIT) = VAL(A$)
IF ASC(A$) = 4 THEN
  OBJECTIVEX(n, FIRST, BIT) = 0
  EXIT DO
END IF
IF ASC(A$) = 2 THEN
 EXT = 1
 OBJECTIVEX(n, FIRST, BIT) = -1
 EXIT DO
END IF
INPUT #1, objectivey(n, FIRST, BIT)
LOOP
IF EXT = 1 THEN EXT = 0: EXIT DO
BIT = 0
LOOP
FIRST = 0
BIT = 0
NEXT n
INPUT #1, whichplayer, NUMBERTURNS
readobj$ = TERFILE$
CLOSE
END FUNCTION

SUB readterain (FILENAME$)
OPEN FILENAME$ FOR INPUT AS #1
FOR n = 1 TO 40
FOR T = 1 TO 60
INPUT #1, TERAIN(T, n)
NEXT T, n
CLOSE
END SUB

FUNCTION SHOW$ (p, n, OK)
X = player(p, n, 1)
Y = player(p, n, 2)
X = X - 5
Y = Y - 5
IF X < 1 THEN X = 1
IF Y < 1 THEN Y = 1
IF Y > 31 THEN Y = 31
IF X > 51 THEN X = 51
FOR S = 1 TO 10
FOR D = 1 TO 10
COLOR TECOL(TERAIN(X + S - 1, Y + D - 1))
IF whosthere(X + S - 1, Y + D - 1) <> 0 THEN COLOR 0
LINE (160 + (S - 1) * 48, 48 * (D - 1))-STEP(48, 48), , BF
NEXT D, S
COLOR 15
LINE (20 + X * 2, 395 + Y * 2)-STEP(20, 20), 0, B
X2 = (184 + (player(p, n, 1) - X) * 48) - MAXRANGE(player(p, n, 4)) * 48
Y2 = (24 + (player(p, n, 2) - Y) * 48) - MAXRANGE(player(p, n, 4)) * 48
X3 = X2 + (96 * MAXRANGE(player(p, n, 4)))
Y3 = Y2 + (96 * MAXRANGE(player(p, n, 4)))
IF X2 < 160 THEN X2 = 160
IF X2 > 640 THEN X2 = 640
IF Y2 < 0 THEN Y2 = 0
IF Y2 > 480 THEN Y2 = 480
LINE (X2, Y2)-(X3, Y3), , B, &HF0F0
IF OK = 0 THEN EXIT FUNCTION
DO
A$ = INKEY$
C = C + 1
C = C MOD 2
COLOR C * 15
LINE (160 + (player(p, n, 1) - X) * 48, (player(p, n, 2) - Y) * 48)-STEP(48, 48), , BF
FOR O = 1 TO 10000: NEXT O
LOOP WHILE A$ = ""
SHOW$ = A$
END FUNCTION

SUB WHOSWON
IF TURNS > NUMBERTURNS THEN PRINT "Player "; whichplayer; " Has Won": SHELL endseq$: END
FOR n = 1 TO 2
true = 0
IF n = 1 THEN opp = 2 ELSE opp = 1
DO
FIRST = FIRST + 1
DO
BIT = BIT + 1
IF OBJECTIVEX(n, FIRST, BIT) = 0 THEN EXIT DO
IF OBJECTIVEX(n, FIRST, BIT) = -1 THEN EXT = 1: EXIT DO
whicplay = INT(whosthere(OBJECTIVEX(n, FIRST, BIT), objectivey(n, FIRST, BIT)) / 100)
IF whicplay = n THEN true = 1
IF whicplay = opp THEN true = 0: EXT = 1: EXIT DO
LOOP
IF EXT = 1 THEN EXIT DO
BIT = 0
EXT = 0
LOOP
IF true = 1 THEN PRINT "Player "; n; " Has won": SHELL endseq$: END
FIRST = 0
BIT = 0
NEXT n
FOR p = 1 TO 2
IF p = 1 THEN pn = 2 ELSE pn = 1
true = 1
FOR n = 1 TO 30
IF player(p, n, 3) > 0 THEN true = 0
NEXT n
IF true = 1 THEN PRINT "Player "; pn; " Has Won": SHELL endseq$: END
NEXT p
END SUB

