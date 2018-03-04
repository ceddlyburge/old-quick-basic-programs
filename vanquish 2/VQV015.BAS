DECLARE SUB getgraphs ()
DECLARE SUB WHOSWON ()
DECLARE FUNCTION readobj$ (filename$)
DECLARE SUB QUICKINFO (p!, n!, TURNS!, moves!)
DECLARE SUB PIC ()
DECLARE SUB readterain (filename$)
DECLARE SUB posforces (p!)
DECLARE SUB chooseunit (p!, money!)
DECLARE SUB INFO2 (p!, n!, moves)
DECLARE SUB combat1 (n!, p)
DECLARE SUB MOVEUNIT (p)
DECLARE SUB INIT ()
DECLARE FUNCTION SHOW$ (p, n, OK, OK2)
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
DIM SHARED full(16) AS STRING * 17
DIM SHARED TER$(30)
DIM SHARED TERFUL(30) AS STRING * 10
DIM SHARED sustain(30)
DIM SHARED TECOL(30)
DIM SHARED WHOSTHERE(60, 40)
DIM SHARED OBJECTIVEX(2, 10, 15)
DIM SHARED objectivey(2, 10, 15)
DIM SHARED LANDX(2), LANDY(2), SEAX(2), SEAY(2), AIRX(2), AIRY(2)
DIM SHARED money(2)
DIM COLORS&(0 TO 15)
DIM SHARED PICCY(345, 44) AS LONG
DIM SHARED graph(45) AS STRING
DIM SHARED GRAPHICS(60, 40)
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
getgraphs
chooseunit 1, money(1)
chooseunit 2, money(2)
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
a$ = INPUT$(1)
IF a$ = "8" THEN CURS = CURS - 1
IF a$ = "2" THEN CURS = CURS + 1
IF CURS = MIN - 1 THEN CURS = MAX
IF CURS = MAX + 1 THEN CURS = MIN
IF a$ = "+" THEN army(CURS) = army(CURS) + 1
IF a$ = "-" THEN army(CURS) = army(CURS) - 1
IF a$ = CHR$(13) THEN EXIT DO
IF army(CURS) < 0 THEN army(CURS) = 0
LOOP
FOR n = MIN TO MAX
FOR t = 1 TO army(n)
IF army(n) = 0 THEN EXIT FOR
count = count + 1
player(p, count, 3) = maxnum(n)
player(p, count, 4) = n
player(p, count, 5) = MAXSUP(n)
IF count > 29 THEN EXIT SUB
NEXT t, n
END SUB

SUB combat1 (p, n)
X = 0
y = 0
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
  LINE (X3 + (X * 48), Y3 + (y * 48))-STEP(47, 47), 13, BF
  a$ = INPUT$(1)
  PUT (X3 + (X * 48), Y3 + (y * 48)), PICCY(0, GRAPHICS(X1 + X, Y1 + y)), PSET
  IF WHOSTHERE(X1 + X, Y1 + y) <> 0 THEN LINE (X3 + (X * 48), Y3 + (y * 48))-STEP(47, 47), 0, BF
  IF a$ = "5" THEN EXIT DO
  IF a$ = "1" THEN y = y + 1: X = X - 1
  IF a$ = "2" THEN y = y + 1
  IF a$ = "3" THEN y = y + 1: X = X + 1
  IF a$ = "4" THEN X = X - 1
  IF a$ = "6" THEN X = X + 1
  IF a$ = "7" THEN y = y - 1: X = X - 1
  IF a$ = "8" THEN y = y - 1
  IF a$ = "9" THEN y = y - 1: X = X + 1
  IF X > maxx THEN X = maxx
  IF X < minx THEN X = minx
  IF y > maxy THEN y = maxy
  IF y < miny THEN y = miny
  IF X + player(p, n, 1) > 60 THEN X = X - 1
  IF X + player(p, n, 1) < 1 THEN X = X + 1
  IF y + player(p, n, 2) > 40 THEN y = y - 1
  IF y + player(p, n, 2) < 1 THEN y = y + 1
  'A$ = SHOW$(P, N, 0)
  LOOP
'-----------------------------------------------------------------------------
'------------------------------DECIDING DAMAGE--------------------------------
  HP = 0
  IF WHOSTHERE(X1 + X, Y1 + y) <> 0 THEN
    true = 1
    pn = INT(WHOSTHERE(X1 + X, Y1 + y) / 100)
    o = WHOSTHERE(X1 + X, Y1 + y) - (pn * 100)
    IF inair(player(p, n, 4)) = 1 AND inair(player(pn, o, 4)) = 0 AND samcap(player(p, n, 4)) = 0 THEN
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
        a$ = INPUT$(1)
    END IF
    IF inair(player(p, n, 4)) = 0 AND inair(player(pn, o, 4)) = 1 AND samcap(player(p, n, 4)) = 0 THEN
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
        a$ = INPUT$(1)
     END IF
    IF true THEN
    FOR t = 1 TO player(p, n, 3)
    IF RND < probhit(player(p, n, 4)) THEN HP = HP + damage(player(p, n, 4))
    NEXT t
    killed = HP / hitp(player(pn, o, 4)) / defbonus(terain(player(pn, o, 1), player(pn, o, 2)))
    player(pn, o, 3) = player(pn, o, 3) - killed
    IF player(pn, o, 3) < 1 THEN WHOSTHERE(player(pn, o, 1), player(pn, o, 2)) = 0
    END IF
  END IF
'-----------------------------------------------------------------------------
'--------------------SUPPLYING VEHICULES--------------------------------------
 true = 1
 IF supply(player(p, n, 4)) = 1 AND WHOSTHERE(X1 + X, Y1 + y) <> 0 THEN
    pn = INT(WHOSTHERE(X1 + X, Y1 + y) / 100)
    o = VAL(RIGHT$(STR$(WHOSTHERE(X1 + X, Y1 + y)), 2))
  IF inair(player(pn, o, 4)) = 1 AND inair(player(p, n, 4)) = 0 THEN
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
        a$ = INPUT$(1)
        g$ = SHOW$(p, n, 0, 1)
  END IF
  IF inair(player(pn, o, 4)) = 0 AND inair(player(p, n, 4)) = 1 THEN
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
        a$ = INPUT$(1)
        g$ = SHOW$(p, n, 0, 1)
 END IF
  IF true = 1 THEN
  player(pn, o, 5) = MAXSUP(player(pn, o, 4))
  player(p, n, 5) = player(p, n, 5) - MAXSUP(player(p, o, 4))
 END IF
 END IF
'-----------------------------------------------------------------------------
END SUB

SUB frame (top, bottom, left, right)
horiz = right - left - 1
    hine$ = STRING$(horiz, 196)
LOCATE top, left: PRINT CHR$(218); hine$
LOCATE top, right: PRINT CHR$(191)
LOCATE bottom, left: PRINT CHR$(192); hine$
LOCATE bottom, right: PRINT CHR$(217)

FOR vert = top + 1 TO bottom - 1
    LOCATE vert, left: PRINT CHR$(179); SPACE$(horiz); CHR$(179)
    'LOCATE vert, right: PRINT CHR$(179)
NEXT vert

END SUB

SUB getgraphs
COLOR 15
LOCATE 1, 1: PRINT "Please wait, Loading Graphics Files...."
FOR n = 1 TO 44
OPEN graph(n) FOR INPUT AS #1
INPUT #1, xmax, xmin, ymax, ymin, length'input values before the array values
FOR p = 0 TO 345 'length is the number of files left in the file
INPUT #1, PICCY(p, n)
NEXT p
CLOSE #1
NEXT n
END SUB

SUB INFO2 (p, n, moves)
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
COLOR 12: PRINT "Player "; p
COLOR 15
LOCATE 6, 2
PRINT STRING$(18, 196)
LOCATE 8, 2
PRINT "Type of unit:";
COLOR 12
LOCATE 9, 2
PRINT " "; full(player(p, n, 4))
COLOR 15
LOCATE 10, 2
PRINT "Range:";
LOCATE 11, 4
COLOR 12: PRINT MAXRANGE(player(p, n, 4))
COLOR 15
LOCATE 12, 2
PRINT "Moves left:";
LOCATE 13, 4
COLOR 12: PRINT moves
COLOR 15
LOCATE 14, 2
PRINT "No. left in unit:"
COLOR 12
LOCATE 15, 4
PRINT INT(player(p, n, 3))
LOCATE 16, 2
COLOR 15
PRINT "Supply status:";
LOCATE 17, 4
COLOR 12: PRINT player(p, n, 5)
COLOR 15
LOCATE 19, 2
PRINT STRING$(18, 196)
LOCATE 20, 2
PRINT "Terrain ";
COLOR 12: PRINT TERFUL(terain(player(p, n, 1), player(p, n, 2)))
COLOR 15
LOCATE 21, 2
PRINT STRING$(18, 196)
LOCATE 24, 2
PRINT "Defence bonus:";
LOCATE 25, 4
COLOR 12: PRINT defbonus(terain(player(p, n, 1), player(p, n, 2)))
LOCATE 26, 2
COLOR 15
PRINT "Movement cost:";
LOCATE 27, 4
COLOR 12: PRINT movepen(terain(player(p, n, 1), player(p, n, 2)))
LOCATE 28, 2
COLOR 15
PRINT "Supply point?";
LOCATE 29, 4
COLOR 12: IF supply(terain(player(p, n, 1), player(p, n, 2))) = 1 THEN PRINT " Yes" ELSE PRINT " No "
COLOR 15
LOCATE 30, 2
PRINT "Suitable for Navy?"
COLOR 12
LOCATE 31, 4
IF LANDONLY(terain(player(p, n, 1), player(p, n, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 32, 2
COLOR 15
PRINT "Suitable for Army?"
COLOR 12
LOCATE 33, 4
IF SHIPONLY(terain(player(p, n, 1), player(p, n, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
COLOR 15
LOCATE 34, 2
        PRINT "Turn:";
LOCATE 35, 4
COLOR 12: PRINT TURNS
PUT (20, 355), MAP, PSET
FOR t = 1 TO 2
FOR S = 1 TO 30
IF player(t, S, 3) > 0 THEN LINE (20 + ((player(t, S, 1) - 1) * 2), (2 * (player(t, S, 2))) + 355)-STEP(2, 2), 0 + (t - 1) * 15, BF
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

samcap(1) = 1
samcap(2) = 0
samcap(3) = 1
samcap(4) = 0
samcap(5) = 1
samcap(6) = 0
samcap(7) = 0
samcap(8) = 0
samcap(9) = 1
samcap(10) = 0

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
END SUB

SUB MOVEUNIT (p)
FOR n = 1 TO 30
IF player(p, n, 3) > 0 THEN
  a$ = SHOW$(p, n, 0, 1)
  QUICKINFO p, n, TURNS, move(player(p, n, 4))
  moves = 0
  DO
  a$ = SHOW$(p, n, 1, 0)
  IF a$ = "0" AND moves > 0 THEN
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
        a$ = INPUT$(1)
        g$ = SHOW$(p, n, 0, 1)
  END IF
  IF a$ = "0" AND moves = 0 AND player(p, n, 5) > 0 THEN
     combat1 p, n
     IF inair(player(p, n, 4)) = 0 THEN player(p, n, 5) = player(p, n, 5) - 1
     EXIT DO
  END IF
  IF a$ = "5" THEN EXIT DO
  IF player(p, n, 5) < 1 THEN EXIT DO
  X = 0: y = 0
  IF a$ = "1" THEN y = 1: X = -1
  IF a$ = "2" THEN y = 1: X = 0
  IF a$ = "3" THEN y = 1:  X = 1
  IF a$ = "4" THEN X = -1: y = 0
  IF a$ = "6" THEN X = 1: y = 0
  IF a$ = "7" THEN y = -1: X = -1
  IF a$ = "8" THEN y = -1: X = 0
  IF a$ = "9" THEN y = -1: X = 1
  MOVED = 1
  IF player(p, n, 1) + X < 1 THEN MOVED = 0: X = 0: y = 0
  IF player(p, n, 1) + X > 60 THEN MOVED = 0: X = 0: y = 0
  IF player(p, n, 2) + y < 1 THEN MOVED = 0: X = 0: y = 0
  IF player(p, n, 2) + y > 40 THEN MOVED = 0: X = 0: y = 0
  IF MOVED AND SHIPONLY(terain(player(p, n, 1) + X, player(p, n, 2) + y)) = 1 AND SHIP(player(p, n, 4)) = 0 AND inair(player(p, n, 4)) = 0 THEN
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
        a$ = INPUT$(1)
        g$ = SHOW$(p, n, 0, 1)
     END IF
  IF MOVED AND LANDONLY(terain(player(p, n, 1) + X, player(p, n, 2) + y)) = 1 AND SHIP(player(p, n, 4)) = 1 AND inair(player(p, n, 4)) = 0 THEN
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
        a$ = INPUT$(1)
        g$ = SHOW$(p, n, 0, 1)
  END IF
  WHO = INT(WHOSTHERE(player(p, n, 1) + X, player(p, n, 2) + y) / 100)
  IF WHO <> p AND WHO <> 0 THEN
  MOVED = 0: X = 0: y = 0
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
        a$ = INPUT$(1)
        g$ = SHOW$(p, n, 0, 1)
  END IF
  IF MOVED = 1 THEN
  WHOSTHERE(player(p, n, 1), player(p, n, 2)) = 0:
  player(p, n, 1) = player(p, n, 1) + X: player(p, n, 2) = player(p, n, 2) + y
  IF inair(player(p, n, 4)) = 0 THEN player(p, n, 5) = player(p, n, 5) - 1
  IF inair(player(p, n, 4)) THEN moves = moves + 1 ELSE moves = moves + movepen(terain(player(p, n, 1), player(p, n, 2)))
  END IF
  WHOSTHERE(player(p, n, 1), player(p, n, 2)) = (100 * p) + n
  IF suplyPT(terain(player(p, n, 1), player(p, n, 2))) = 1 THEN player(p, n, 5) = MAXSUP(player(p, n, 4))
  IF move(player(p, n, 4)) - moves > 0 THEN QUICKINFO p, n, TURNS, move(player(p, n, 4)) - moves
  LOOP UNTIL moves >= move(player(p, n, 4))
  WHOSTHERE(player(p, n, 1), player(p, n, 2)) = (100 * p) + n
  IF player(p, n, 5) < 1 AND sustain(terain(player(p, n, 1), player(p, n, 2))) = 0 THEN
        player(p, n, 3) = 0
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
        a$ = INPUT$(1)
        g$ = SHOW$(p, n, 0, 1)
  END IF
  IF inair(player(p, n, 4)) = 1 THEN player(p, n, 5) = player(p, n, 5) - 1
  IF player(p, n, 5) < 1 AND inair(player(p, n, 4)) = 1 THEN
        player(p, n, 3) = 0
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
        LOCATE 31, 27: PRINT "cities or refueling aircraft."
        LOCATE 35, 27: PRINT "    Press any key"
        a$ = INPUT$(1)
        g$ = SHOW$(p, n, 0, 1)
  END IF
 END IF
NEXT n
END SUB

SUB PIC
SCREEN 0
WIDTH , 43
FOR n = 1 TO 60
FOR S = 1 TO 40
COLOR 0, TECOL(terain(n, S))
LOCATE S, n + 20: PRINT TER$(terain(n, S))
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
IF inair(player(p, n, 4)) = 1 THEN
 X = LANDX(p)
 y = LANDY(p)
END IF
IF SHIP(player(p, n, 4)) = 1 THEN
  X = SEAX(p)
  y = SEAY(p)
 ELSE
  X = LANDX(p)
  y = LANDY(p)
END IF
LOCATE y, X + 20: COLOR 0, TECOL(terain(X, y)): PRINT TER$(terain(X, y))
LOCATE y, X + 20: COLOR 30, 0: PRINT LET$(player(p, n, 4))
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
 COLOR 12, 0: PRINT TERFUL(terain(X, y))
 COLOR 15, 0
 PRINT STRING$(20, 196)
 PRINT
 PRINT "Defence bonus:";
 COLOR 12, 0: PRINT defbonus(terain(X, y)); " "
 COLOR 15, 0
 PRINT
 PRINT "Movement cost:";
 COLOR 12, 0: PRINT movepen(terain(X, y)); " "
 COLOR 15, 0
 PRINT
 PRINT "Supply point?";
 COLOR 12, 0
 IF suplyPT(terain(X, y)) = 1 THEN PRINT " Yes" ELSE PRINT " No "
 COLOR 15, 0
 PRINT
 PRINT "Suitable for Navy?"
 COLOR 12, 0
 IF LANDONLY(terain(X, y)) = 0 THEN PRINT " Yes" ELSE PRINT " No "
 COLOR 15, 0
 PRINT
 PRINT "Suitable for Army?"
 COLOR 12, 0
 IF SHIPONLY(terain(X, y)) = 0 THEN PRINT " Yes" ELSE PRINT " No "
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
 a$ = INPUT$(1)
  LOCATE y, X + 20: COLOR 0, TECOL(terain(X, y)): PRINT TER$(terain(X, y))
  IF a$ = "5" THEN
  OK = 1
  IF WHOSTHERE(X, y) <> 0 THEN OK = 0
  IF LANDONLY(terain(X, y)) = 1 AND SHIP(player(p, n, 4)) AND inair(player(p, n, 4)) = 0 THEN OK = 0
  IF SHIPONLY(terain(X, y)) = 1 AND SHIP(player(p, n, 4)) = 0 AND inair(player(p, n, 4)) = 0 THEN OK = 0
  IF OK THEN EXIT DO
  END IF
  IF a$ = "1" THEN DY = 1: DX = -1
  IF a$ = "2" THEN DY = 1: DX = 0
  IF a$ = "3" THEN DY = 1: DX = 1
  IF a$ = "4" THEN DX = -1: DY = 0
  IF a$ = "6" THEN DX = 1: DY = 0
  IF a$ = "7" THEN DY = -1: DX = -1
  IF a$ = "8" THEN DY = -1: DX = 0
  IF a$ = "9" THEN DY = -1: DX = 1
  IF X + DX > MAX THEN DX = 0
  IF X + DX < MIN THEN DX = 0
  IF y + DY > 40 THEN DY = 0
  IF y + DY < 1 THEN DY = 0
  IF WHOSTHERE(X, y) <> 0 THEN
  IF WHOSTHERE(X, y) > 200 THEN TR = WHOSTHERE(X, y) - 200: TP = 2 ELSE TR = WHOSTHERE(X, y) - 100: TP = 1
  LOCATE y, X + 20
  COLOR 15, 0
  PRINT LET$(player(TP, TR, 4))
  END IF
  X = X + DX
  y = y + DY
  LOCATE y, X + 20: COLOR 30, 0: PRINT LET$(player(p, n, 4))
  LOOP
  player(p, n, 1) = X
  player(p, n, 2) = y
  LOCATE y, X + 20: COLOR 15, 0: PRINT LET$(player(p, n, 4))
  WHOSTHERE(X, y) = (p * 100) + n
END IF
NEXT n
IF p = 2 THEN
SCREEN 12
FOR t = 1 TO 40
FOR S = 1 TO 60
LINE (20 + (S * 2), (t * 2) + 395)-STEP(2, 2), TECOL(terain(S, t)), BF
NEXT S, t
GET (20, 395)-STEP(120, 80), MAP
END IF
END SUB

SUB QUICKINFO (p, n, TURNS, moves)
COLOR 12
LOCATE 5, 17
PRINT p
LOCATE 9, 2
PRINT " "; full(player(p, n, 4))
LOCATE 11, 4
PRINT MAXRANGE(player(p, n, 4))
LOCATE 13, 4
PRINT moves
LOCATE 15, 4
PRINT INT(player(p, n, 3)); "  "
LOCATE 17, 4
PRINT player(p, n, 5); "  "
LOCATE 20, 10
PRINT TERFUL(terain(player(p, n, 1), player(p, n, 2)))
LOCATE 25, 4
PRINT defbonus(terain(player(p, n, 1), player(p, n, 2)))
LOCATE 27, 4
PRINT movepen(terain(player(p, n, 1), player(p, n, 2)))
LOCATE 29, 4
IF supply(terain(player(p, n, 1), player(p, n, 2))) = 1 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 31, 4
IF LANDONLY(terain(player(p, n, 1), player(p, n, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 33, 4
IF SHIPONLY(terain(player(p, n, 1), player(p, n, 2))) = 0 THEN PRINT " Yes" ELSE PRINT " No "
LOCATE 35, 4
PRINT TURNS
PUT (20, 355), MAP, PSET
LINE (20 + (2 * player(p, n, 1)) - 10, 355 + (2 * player(p, n, 2)) - 10)-STEP(20, 20), 0, B
FOR t = 1 TO 2
COLOR 0 + (t - 1) * 15
FOR S = 1 TO 30
IF player(t, S, 3) > 0 THEN LINE (20 + ((player(t, S, 1) - 1) * 2), (2 * (player(t, S, 2) - 1)) + 355)-STEP(2, 2), , BF
NEXT S, t
END SUB

FUNCTION readobj$ (filename$)
OPEN filename$ FOR INPUT AS #1
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
INPUT #1, a$
OBJECTIVEX(n, FIRST, BIT) = VAL(a$)
IF ASC(a$) = 4 THEN
  OBJECTIVEX(n, FIRST, BIT) = 0
  EXIT DO
END IF
IF ASC(a$) = 2 THEN
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

SUB readterain (filename$)
'OPEN filename$ FOR INPUT AS #1
'filename$ = LEFT$(filename$, LEN(filename$) - 4)
'filename$ = filename$ + ".gph"
OPEN filename$ FOR INPUT AS #2
FOR n = 1 TO 40
FOR t = 1 TO 60
'INPUT #1, TERAIN(T, N)
INPUT #2, GRAPHICS(t, n)
SELECT CASE GRAPHICS(t, n)
 CASE 1
        terain(t, n) = 9
 CASE 2
        terain(t, n) = 2
 CASE 3
        terain(t, n) = 4
 CASE 4
        terain(t, n) = 1
 CASE 5 TO 18, 23 TO 26
        terain(t, n) = 6
 CASE 19 TO 23, 27 TO 39, 44
        terain(t, n) = 7
 CASE 40
        terain(t, n) = 5
 CASE 41
        terain(t, n) = 8
 CASE 42
        terain(t, n) = 10
 CASE 43
        terain(t, n) = 3
END SELECT

NEXT t, n
CLOSE
END SUB

FUNCTION SHOW$ (p, n, OK, OK2)
STATIC X, y
IF OK2 = 0 AND player(p, n, 1) - X < 10 AND player(p, n, 1) - X > -1 AND player(p, n, 2) - y < 10 AND player(p, n, 2) - y > -1 AND X > 0 THEN GOTO NODRAW
X = player(p, n, 1)
y = player(p, n, 2)
X = X - 5
y = y - 5
IF X < 1 THEN X = 1
IF y < 1 THEN y = 1
IF y > 31 THEN y = 31
IF X > 51 THEN X = 51

FOR S = 1 TO 10
FOR D = 1 TO 10
S1 = S - 1
D1 = D - 1
XS1 = X + S1
YD1 = y + D1
S2 = 160 + S1 * 48
D2 = 48 * D1
PUT (S2, D2), PICCY(0, GRAPHICS(XS1, YD1)), PSET
IF WHOSTHERE(XS1, YD1) > 0 THEN LINE (S2, D2)-STEP(47, 47), WHOSTHERE(XS1, YS1) / 100, BF
NEXT D, S
NODRAW:
IF OK = 0 THEN PUT (160 + (player(p, n, 1) - X) * 48, (player(p, n, 2) - y) * 48), PICCY(0, GRAPHICS(player(p, n, 1), player(p, n, 2))), PSET: EXIT FUNCTION
DO
a$ = INKEY$
C = C + 1
C = C MOD 2
COLOR C * 15
LINE (160 + (player(p, n, 1) - X) * 48, (player(p, n, 2) - y) * 48)-STEP(47, 47), , BF
FOR o = 1 TO 1000: NEXT o
LOOP WHILE a$ = ""
SHOW$ = a$
PUT (160 + (player(p, n, 1) - X) * 48, (player(p, n, 2) - y) * 48), PICCY(0, GRAPHICS(player(p, n, 1), player(p, n, 2))), PSET
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
whicplay = INT(WHOSTHERE(OBJECTIVEX(n, FIRST, BIT), objectivey(n, FIRST, BIT)) / 100)
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

