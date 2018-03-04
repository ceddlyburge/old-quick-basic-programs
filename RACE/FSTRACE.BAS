DECLARE SUB finish ()
DECLARE SUB crash ()
DECLARE SUB frame (left!, right!, top!, bottom!)
DECLARE SUB getgraphs ()
DIM SHARED car(500, 15) AS LONG
DIM RACECOURSE(-1 TO 17, -1 TO 17)
getgraphs
maxlevel = 7
DIM names(maxlevel) AS STRING, scores(maxlevel)
OPEN "FRDAT" FOR INPUT AS #1
INPUT #1, best$, bestscore
FOR n = 1 TO maxlevel
INPUT #1, names(n), scores(n)
NEXT n
CLOSE
spd = 299
DO
startagain:
SCREEN 0
CLS
frame 1, 80, 1, 23
COLOR 15
LOCATE 2, 25: PRINT "Welcome to Fastrace Version 1"
LOCATE 5, 26: PRINT "        ---Keys---"
COLOR 7
LOCATE 7, 26: PRINT "   Accelerate-Left Shift"
LOCATE 8, 26: PRINT "         Left-ALT"
LOCATE 9, 26: PRINT "        Right-CTRL"
LOCATE 10, 26: PRINT "Brake-Space (This uses time)"
LOCATE 12, 26: PRINT "       ESC - Exit"
LOCATE 14, 20: PRINT "Manouver your car around  track until you"
LOCATE 16, 20: PRINT "find the blue exit block-go past this and"
LOCATE 18, 20: PRINT "go on to next level"
LOCATE 21, 30: PRINT "Best: "; best$; " "; INT(bestscore)
a$ = INPUT$(1)
IF ASC(a$) = 27 THEN EXIT DO
lives = 3
score = 0
FOR level = 1 TO maxlevel
ty = 0
doagain:
tim = 0
IF level = 1 THEN OPEN "level3.gam" FOR INPUT AS #1
IF level = 2 THEN OPEN "level2.gam" FOR INPUT AS #1
IF level = 3 THEN OPEN "level1.gam" FOR INPUT AS #1
IF level = 4 THEN OPEN "level5.gam" FOR INPUT AS #1
IF level = 5 THEN OPEN "level6.gam" FOR INPUT AS #1
IF level = 6 THEN OPEN "level7.gam" FOR INPUT AS #1
IF level = 7 THEN OPEN "level8.gam" FOR INPUT AS #1
INPUT #1, timalow
INPUT #1, x8, y8, angle1
SCREEN 7
FOR y = 0 TO 16
FOR x = 0 TO 16
INPUT #1, RACECOURSE(x, y)
IF RACECOURSE(x, y) = 1 THEN
        LINE (x * 10, y * 10)-STEP(5, 10), 15, BF
        LINE ((x * 10) + 5, y * 10)-STEP(5, 10), 0, BF
 END IF
IF RACECOURSE(x, y) = 2 THEN
        LINE (x * 10, y * 10)-STEP(5, 10), 0, BF
        LINE ((x * 10) + 5, y * 10)-STEP(5, 10), 15, BF
 END IF
IF RACECOURSE(x, y) = 3 THEN
        LINE (x * 10, (y * 10) + 5)-STEP(10, 5), 0, BF
        LINE (x * 10, y * 10)-STEP(10, 5), 15, BF
 END IF
IF RACECOURSE(x, y) = 4 THEN
        LINE (x * 10, (y * 10) + 5)-STEP(10, 5), 15, BF
        LINE (x * 10, y * 10)-STEP(10, 5), 0, BF
 END IF
IF RACECOURSE(x, y) = 5 THEN
        LINE (x * 10, y * 10)-STEP(10, 10), 15, BF
 END IF
IF RACECOURSE(x, y) = 6 THEN
        LINE (x * 10, y * 10)-STEP(10, 10), 0, BF
 END IF
 IF RACECOURSE(x, y) = 7 THEN
        LINE (x * 10, y * 10)-STEP(5, 10), 0, BF
        LINE ((x * 10) + 5, y * 10)-STEP(5, 10), 1, BF
 END IF
IF RACECOURSE(x, y) = 8 THEN
        LINE (x * 10, y * 10)-STEP(10, 10), 0, BF
        LINE (x * 10, y * 10)-STEP(5, 5), 15, BF
 END IF
IF RACECOURSE(x, y) = 9 THEN
        LINE (x * 10, y * 10)-STEP(10, 10), 0, BF
        LINE (x * 10, (y * 10) + 5)-STEP(5, 5), 15, BF
 END IF
IF RACECOURSE(x, y) = 10 THEN
        LINE (x * 10, y * 10)-STEP(10, 10), 0, BF
        LINE ((x * 10) + 5, y * 10)-STEP(5, 5), 15, BF
 END IF
IF RACECOURSE(x, y) = 11 THEN
        LINE (x * 10, y * 10)-STEP(10, 10), 0, BF
        LINE ((x * 10) + 5, (y * 10) + 5)-STEP(5, 5), 15, BF
END IF
IF RACECOURSE(x, y) = 12 THEN
        LINE (x * 10, y * 10)-STEP(10, 10), 15, BF
        LINE (x * 10, y * 10)-STEP(5, 5), 0, BF
        RACECOURSE(x, y) = 12
END IF
IF RACECOURSE(x, y) = 13 THEN
        LINE (x * 10, y * 10)-STEP(10, 10), 15, BF
        LINE (x * 10, (y * 10) + 5)-STEP(5, 5), 0, BF
        RACECOURSE(x, y) = 13
END IF
IF RACECOURSE(x, y) = 14 THEN
        LINE (x * 10, y * 10)-STEP(10, 10), 15, BF
        LINE ((x * 10) + 5, y * 10)-STEP(5, 5), 0, BF
        RACECOURSE(x, y) = 14
END IF
IF RACECOURSE(x, y) = 15 THEN
        LINE (x * 10, y * 10)-STEP(10, 10), 15, BF
        LINE ((x * 10) + 5, (y * 10) + 5)-STEP(5, 5), 0, BF
        RACECOURSE(x, y) = 15
END IF
NEXT x, y
LINE (x8 * 10 / 640, y8 * 10 / 350)-STEP(10, 10), 7, B
IF (level / 2) = INT(level / 2) THEN
        reps = 1
        LOCATE 1, 23: PRINT "Maze Course"
        LOCATE 6, 23: PRINT "Lives left "; lives
        LOCATE 12, 23: PRINT "Memorize course"
        LOCATE 14, 23: PRINT "for maximum speed"
        COLOR 15
        LOCATE 21, 23: PRINT "Max Speed "; spd
        DO
        COLOR 15
        LOCATE 21, 23: PRINT "Max Speed "; spd
        DO
        a$ = INKEY$
        LOOP UNTIL a$ <> ""
        a = ASC(RIGHT$(a$, 1))
        IF a = 72 THEN spd = spd + 1
        IF a = 80 THEN spd = spd - 1
        IF spd < 1 THEN spd = 1
        IF spd > 35 THEN spd = 35
        IF ASC(a$) = 27 THEN EXIT DO
        IF a$ <> "" AND a <> 72 AND a <> 80 THEN EXIT DO
        LOOP
ELSE
        reps = 5
        LOCATE 1, 25: PRINT "Speed Course"
        LOCATE 4, 23: PRINT "Time = Points"
        LOCATE 8, 23: PRINT "One life only"
        LOCATE 12, 23: PRINT "No consequence to"
        LOCATE 14, 23: PRINT "lives on other"
        LOCATE 16, 23: PRINT "levels."
        COLOR 15
        LOCATE 21, 23: PRINT "Max Speed "; spd
        DO
        COLOR 15
        LOCATE 21, 23: PRINT "Max Speed "; spd
        DO
        a$ = INKEY$
        LOOP UNTIL a$ <> ""
        a = ASC(RIGHT$(a$, 1))
        IF a = 72 THEN spd = spd + 1
        IF a = 80 THEN spd = spd - 1
        IF spd < 1 THEN spd = 1
        IF spd > 299 THEN spd = 299
        IF ASC(a$) = 27 THEN EXIT DO
        IF a$ <> "" AND a <> 72 AND a <> 80 THEN EXIT DO
LOOP
END IF
spd1 = spd * 350 / 640
angle = angle1
x = x8
y = y8
CLOSE
DEF SEG = 0
SCREEN 9
xvel = 0
yvel = 0
tim = 0
sndjob = 1000 / timalow
CLS
LOCATE 1, 1: PRINT "Time left "
LOCATE 1, 20: PRINT "Best Time "; names(level); scores(level)
LOCATE 1, 50: PRINT "Score "; INT(score); " High "; INT(bestscore)
DO
SOUND 50 + (tim * sndjob), .1
tim = tim + 1
IF tim >= timalow THEN PRINT "Out of time": die = 1: EXIT DO
x = x + xvel
y = y + yvel
IF y < 0 THEN EXIT DO
cnt = 0
IF ABS(xvel) > ABS(yvel) * 640 / 350 THEN ybnc = 1 ELSE ybnc = 0
DO
ok = 0
IF POINT(321 + xvel, 164 + yvel) <> 0 OR POINT(317 + xvel, 183 + yvel) <> 0 OR POINT(309 + xvel, 173 + yvel) <> 0 OR POINT(333 + xvel, 173 + yvel) <> 0 THEN
      cnt = cnt + 1
      x = x - xvel
      y = y - yvel
      IF ybnc = 1 THEN
      IF cnt = 1 THEN yvel = -yvel
      IF cnt = 2 THEN xvel = -xvel
      IF cnt = 3 THEN xvel = -xvel: yvel = -yvel
      END IF
      IF ybnc = 0 THEN
      IF cnt = 1 THEN xvel = -xvel
      IF cnt = 2 THEN yvel = -yvel
      IF cnt = 3 THEN xvel = -xvel: yvel = -yvel
      END IF
      IF cnt = 4 THEN LOCATE 1, 1: PRINT "Crash Too Serious": crash: GOTO startagain
      x = x + xvel
      y = y + yvel
      ok = 1
END IF
LOOP UNTIL ok = 0
IF cnt > 0 AND xvel > 2 THEN xvel = xvel / 2
IF cnt > 0 AND yvel > 2 THEN yvel = yvel / 2
IF cnt > 0 THEN tim = tim + 75
PUT (310, 165), car(0, angle), PSET
backin:
x1 = x / 640
y1 = y / 350
x1min = FIX(x1)
y1min = FIX(y1)
x1max = x1min + 1
y1max = y1min + 1
xpass = (x1min - x1) * 640
ypass = (y1min - y1) * 350
LOCATE 1, 12: PRINT timalow - tim
ON RACECOURSE(x1min, y1min) GOSUB 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150
xpass = 640 + xpass
ON RACECOURSE(x1max, y1min) GOSUB 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150
ypass = 350 + ypass
ON RACECOURSE(x1max, y1max) GOSUB 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150
xpass = xpass - 640
ON RACECOURSE(x1min, y1max) GOSUB 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150
a$ = INKEY$
IF a$ = " " THEN xvel = 0: yvel = 0: tim = tim + 50
a = PEEK(&H417)
IF a AND 8 THEN angle = angle - 1
IF a AND 4 THEN angle = angle + 1
IF angle = -1 THEN angle = 15
IF angle = 16 THEN angle = 0
angle1 = angle * .392699
IF a AND 2 THEN axl = 3 ELSE axl = 0
xvel = xvel + (axl * SIN(angle1))
yvel = yvel - (axl / 2 * COS(angle1))
IF xvel > spd THEN xvel = spd
IF xvel < -spd THEN xvel = -spd
IF yvel > spd1 THEN yvel = spd1
IF yvel < -spd1 THEN yvel = -spd1
'die = 1: EXIT DO
LOOP
IF die = 1 THEN
        die = 0: crash
        IF (level / 2) = INT(level / 2) THEN
                lives = lives - 1
                IF lives = 0 THEN
                        EXIT FOR
                ELSE
                        x = x8
                        y = y8
                        xvel = 0
                        yvel = 0
                        tim = 0
                        LOCATE 1, 30: PRINT "Press any Key ......"
                        a$ = INPUT$(1)
                        LOCATE 1, 30: PRINT "                    "
                        COLOR 15
                        LOCATE 1, 1: PRINT "Time left "
                        LOCATE 1, 20: PRINT "Best Time "; names(level); scores(level)
                        GOTO backin
                END IF
          ELSE
                ty = ty + 1
                IF ty < reps THEN GOTO doagain
         END IF
ELSE
score = score + (2 ^ ((level + 20 - reps) * (1 - ((tim) / timalow))))
IF tim < scores(level) THEN
frame 15, 65, 10, 12
LOCATE 11, 16: INPUT "Fastest Time "; names(level): scores(level) = tim
names(level) = LEFT$(names(level), 12)
OPEN "FRDAT" FOR OUTPUT AS #1
WRITE #1, best$, bestscore
FOR n = 1 TO maxlevel
WRITE #1, names(n), scores(n)
NEXT n
CLOSE
END IF
END IF
ty = ty + 1
IF ty < reps THEN GOTO doagain
COLOR 12
LOCATE 1, 30: PRINT "Press any Key ......"
a$ = INPUT$(1)
COLOR 7
NEXT level
IF score > bestscore THEN
frame 15, 65, 10, 12
LOCATE 11, 16: INPUT "Enter Name "; best$
bestscore = score
OPEN "FRDAT" FOR OUTPUT AS #1
WRITE #1, best$, bestscore
FOR n = 1 TO maxlevel
WRITE #1, names(n), scores(n)
NEXT n
CLOSE
END IF
LOOP
END

10
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350:  ELSE y5 = ypass: ymax = 350
IF xpass < 0 THEN x5 = 0:  ELSE x5 = xpass
xmax = 320 + xpass: IF xmax > 640 THEN xmax = 640
IF ymax < 13 THEN ymax = 13
IF xpass > -320 THEN LINE (x5, y5)-(xmax, ymax), 15, BF
yr = ypass + 175
IF yr > 13 AND yr < 350 THEN LINE (x5, yr)-(xmax, yr), 0
x5 = xmax
IF x5 < 0 THEN x5 = 0
xmax = xpass + 640
IF xmax > 640 THEN xmax = 640
IF xpass < 320 THEN LINE (x5, y5)-(xmax, ymax), 0, BF
RETURN

20
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350:  ELSE y5 = ypass: ymax = 350
IF xpass < 0 THEN x5 = 0:  ELSE x5 = xpass
xmax = 320 + xpass: IF xmax > 640 THEN xmax = 640
IF ymax < 13 THEN ymax = 13
IF xpass > -320 THEN LINE (x5, y5)-(xmax, ymax), 0, BF
x5 = xmax
IF x5 < 0 THEN x5 = 0
xmax = xpass + 640
IF xmax > 640 THEN xmax = 640
IF xpass < 320 THEN LINE (x5, y5)-(xmax, ymax), 15, BF
yr = ypass + 175
IF yr > 13 AND yr < 350 THEN LINE (x5, yr)-(xmax, yr), 0
RETURN

30
IF ypass < 13 THEN y5 = 13:  ELSE y5 = ypass
IF xpass < 0 THEN x5 = 0: xmax = xpass + 640:  ELSE x5 = xpass: xmax = 640
ymax = 175 + ypass: IF ymax > 350 THEN ymax = 350
IF ymax < 13 THEN ymax = 13
IF ypass > -175 THEN LINE (x5, y5)-(xmax, ymax), 15, BF
xr = xpass + 320
IF xr > 0 AND xr < 640 THEN LINE (xr, y5)-(xr, ymax), 0
y5 = ymax
IF y5 < 13 THEN y5 = 13
ymax = ypass + 350
IF ymax > 350 THEN ymax = 350
IF ymax < 13 THEN ymax = 13
IF ypass < 175 THEN LINE (x5, y5)-(xmax, ymax), 0, BF
RETURN

40
IF ypass < 13 THEN y5 = 13:  ELSE y5 = ypass
IF xpass < 0 THEN x5 = 0: xmax = xpass + 640:  ELSE x5 = xpass: xmax = 640
ymax = 175 + ypass: IF ymax > 350 THEN ymax = 350
IF ymax < 13 THEN ymax = 13
IF ypass > -175 THEN LINE (x5, y5)-(xmax, ymax), 0, BF
y5 = ymax
IF y5 < 13 THEN y5 = 13
ymax = ypass + 350
IF ymax < 13 THEN ymax = 13
IF ymax > 350 THEN ymax = 350
IF ypass < 175 THEN LINE (x5, y5)-(xmax, ymax), 15, BF
xr = xpass + 320
IF xr > 0 AND xr < 640 THEN LINE (xr, y5)-(xr, ymax), 0
RETURN

50
IF xpass < 0 THEN x5 = 0: xmax = xpass + 640 ELSE x5 = xpass: xmax = 640
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350 ELSE y5 = ypass: ymax = 350
IF ymax < 13 THEN ymax = 13
LINE (x5, y5)-(xmax, ymax), 15, BF
xr = xpass + 320
IF xr > 0 AND xr < 640 THEN LINE (xr, y5)-(xr, ymax), 0
yr = ypass + 175
IF yr > 13 AND yr < 350 THEN LINE (x5, yr)-(xmax, yr), 0
RETURN

60
IF xpass < 0 THEN x5 = 0: xmax = xpass + 640 ELSE x5 = xpass: xmax = 640
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350 ELSE y5 = ypass: ymax = 350
IF ymax < 13 THEN ymax = 13
LINE (x5, y5)-(xmax, ymax), 0, BF
RETURN

70
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350:  ELSE y5 = ypass: ymax = 350
IF xpass < 0 THEN x5 = 0:  ELSE x5 = xpass
IF ymax < 13 THEN ymax = 13
xmax = 320 + xpass: IF xmax > 640 THEN xmax = 640
IF xpass > -320 THEN LINE (x5, y5)-(xmax, ymax), 0, BF
x5 = xmax
IF x5 < 0 THEN x5 = 0
xmax = xpass + 640
IF xmax > 640 THEN xmax = 640
IF xpass < 320 THEN LINE (x5, y5)-(xmax, ymax), 1, BF
yr = ypass + 175
IF yr > 13 AND yr < 350 THEN LINE (x5, yr)-(xmax, yr), 0
RETURN

80
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350:  ELSE y5 = ypass: ymax = 350
IF xpass < 0 THEN x5 = 0:  ELSE x5 = xpass
IF ymax < 13 THEN ymax = 13
xmax = 320 + xpass: IF xmax > 640 THEN xmax = 640
yr = ypass + 175
IF yr < 13 THEN yr = 13
IF yr > 350 THEN yr = 350
IF xpass > -320 THEN LINE (x5, y5)-(xmax, yr), 15, BF: LINE (x5, yr)-(xmax, ymax), 0, BF
x5 = xmax
IF x5 < 0 THEN x5 = 0
xmax = xpass + 640
IF xmax > 640 THEN xmax = 640
IF xpass < 320 THEN LINE (x5, y5)-(xmax, ymax), 0, BF
RETURN

90
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350:  ELSE y5 = ypass: ymax = 350
IF xpass < 0 THEN x5 = 0:  ELSE x5 = xpass
IF ymax < 13 THEN ymax = 13
xmax = 320 + xpass: IF xmax > 640 THEN xmax = 640
yr = ypass + 175
IF yr < 13 THEN yr = 13
IF yr > 350 THEN yr = 350
IF xpass > -320 THEN LINE (x5, yr)-(xmax, ymax), 15, BF: LINE (x5, y5)-(xmax, yr), 0, BF
x5 = xmax
IF x5 < 0 THEN x5 = 0
xmax = xpass + 640
IF xmax > 640 THEN xmax = 640
IF xpass < 320 THEN LINE (x5, y5)-(xmax, ymax), 0, BF
RETURN

110
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350:  ELSE y5 = ypass: ymax = 350
IF xpass < 0 THEN x5 = 0:  ELSE x5 = xpass
IF ymax < 13 THEN ymax = 13
xmax = 320 + xpass: IF xmax > 640 THEN xmax = 640
IF xpass > -320 THEN LINE (x5, y5)-(xmax, ymax), 0, BF
x5 = xmax
IF x5 < 0 THEN x5 = 0
xmax = xpass + 640
IF xmax > 640 THEN xmax = 640
yr = ypass + 175
IF yr < 13 THEN yr = 13
IF yr > 350 THEN yr = 350
IF xpass < 320 THEN LINE (x5, yr)-(xmax, ymax), 15, BF: LINE (x5, y5)-(xmax, yr), 0, BF
RETURN

100
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350:  ELSE y5 = ypass: ymax = 350
IF xpass < 0 THEN x5 = 0:  ELSE x5 = xpass
IF ymax < 13 THEN ymax = 13
xmax = 320 + xpass: IF xmax > 640 THEN xmax = 640
IF xpass > -320 THEN LINE (x5, y5)-(xmax, ymax), 0, BF
x5 = xmax
IF x5 < 0 THEN x5 = 0
xmax = xpass + 640
IF xmax > 640 THEN xmax = 640
yr = ypass + 175
IF yr < 13 THEN yr = 13
IF yr > 350 THEN yr = 350
IF xpass < 320 THEN LINE (x5, y5)-(xmax, yr), 15, BF: LINE (x5, yr)-(xmax, ymax), 0, BF
RETURN

120
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350:  ELSE y5 = ypass: ymax = 350
IF xpass < 0 THEN x5 = 0:  ELSE x5 = xpass
IF ymax < 13 THEN ymax = 13
xmax = 320 + xpass: IF xmax > 640 THEN xmax = 640
yr = ypass + 175
IF yr < 13 THEN yr = 13
IF yr > 350 THEN yr = 350
IF xpass > -320 THEN
LINE (x5, y5)-(xmax, yr), 0, BF
IF ypass < 175 THEN LINE (x5, yr)-(xmax, ymax), 15, BF
END IF
x5 = xmax
IF x5 < 0 THEN x5 = 0
xmax = xpass + 640
IF xmax > 640 THEN xmax = 640
IF xpass < 320 THEN LINE (x5, y5)-(xmax, ymax), 15, BF
RETURN

130
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350:  ELSE y5 = ypass: ymax = 350
IF xpass < 0 THEN x5 = 0:  ELSE x5 = xpass
IF ymax < 13 THEN ymax = 13
xmax = 320 + xpass: IF xmax > 640 THEN xmax = 640
yr = ypass + 175
IF yr < 13 THEN yr = 13
IF yr > 350 THEN yr = 350
IF xpass > -320 THEN
LINE (x5, yr)-(xmax, ymax), 0, BF
IF ypass > -175 THEN LINE (x5, y5)-(xmax, yr), 15, BF
END IF
x5 = xmax
IF x5 < 0 THEN x5 = 0
xmax = xpass + 640
IF xmax > 640 THEN xmax = 640
IF xpass < 320 THEN LINE (x5, y5)-(xmax, ymax), 15, BF
RETURN

150
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350:  ELSE y5 = ypass: ymax = 350
IF xpass < 0 THEN x5 = 0:  ELSE x5 = xpass
IF ymax < 13 THEN ymax = 13
xmax = 320 + xpass: IF xmax > 640 THEN xmax = 640
IF xpass > -320 THEN LINE (x5, y5)-(xmax, ymax), 15, BF
x5 = xmax
IF x5 < 0 THEN x5 = 0
xmax = xpass + 640
IF xmax > 640 THEN xmax = 640
yr = ypass + 175
IF yr < 13 THEN yr = 13
IF yr > 350 THEN yr = 350
IF xpass < 320 THEN
LINE (x5, yr)-(xmax, ymax), 0, BF
IF ypass > -175 THEN LINE (x5, y5)-(xmax, yr), 15, BF
END IF
RETURN

140
IF ypass < 13 THEN y5 = 13: ymax = ypass + 350:  ELSE y5 = ypass: ymax = 350
IF xpass < 0 THEN x5 = 0:  ELSE x5 = xpass
IF ymax < 13 THEN ymax = 13
xmax = 320 + xpass: IF xmax > 640 THEN xmax = 640
IF xpass > -320 THEN LINE (x5, y5)-(xmax, ymax), 15, BF
x5 = xmax
IF x5 < 0 THEN x5 = 0
xmax = xpass + 640
IF xmax > 640 THEN xmax = 640
yr = ypass + 175
IF yr < 13 THEN yr = 13
IF yr > 350 THEN yr = 350
IF xpass < 320 THEN
LINE (x5, y5)-(xmax, yr), 0, BF
IF ypass < 175 THEN LINE (x5, yr)-(xmax, ymax), 15, BF
END IF
RETURN

SUB crash
FOR n = 1 TO 30
FOR u = 1 TO 3000: NEXT u
CIRCLE (320, 175), n, 12
NEXT n
FOR n = 1 TO 13
FOR u = 1 TO 4000
NEXT u
CIRCLE (320, 175), n * 2, 14
NEXT n
FOR n = 1 TO 7
FOR u = 1 TO 5000: NEXT u
CIRCLE (320, 175), n * 3, 15
NEXT n

END SUB

SUB finish
CLS
frame 1, 80, 1, 23
LOCATE 2, 20: PRINT "Well done You have Completed the game"
LOCATE 5, 30: PRINT "Time Taken = "; totim
LOCATE 15, 30: INPUT "Enter Name"; name$
END SUB

SUB frame (left, right, top, bottom)
horiz = right - left - 1
hine$ = STRING$(horiz, 196)
LOCATE top, left: PRINT CHR$(218); hine$
LOCATE top, right: PRINT CHR$(191)
LOCATE bottom, left: PRINT CHR$(192); hine$
LOCATE bottom, right: PRINT CHR$(217)

FOR vert = top + 1 TO bottom - 1
    LOCATE vert, left: PRINT CHR$(179); SPACE$(horiz); CHR$(179)
    LOCATE vert, right: PRINT CHR$(179)
NEXT vert


END SUB

SUB getgraphs
DEF SEG = VARSEG(car(0, 0))
BLOAD "carn.bs9", VARPTR(car(0, 0))
DEF SEG = VARSEG(car(0, 1))
BLOAD "carnne.bs9", VARPTR(car(0, 1))
DEF SEG = VARSEG(car(0, 2))
BLOAD "carne.bs9", VARPTR(car(0, 2))
DEF SEG = VARSEG(car(0, 3))
BLOAD "carnee.bs9", VARPTR(car(0, 3))
DEF SEG = VARSEG(car(0, 4))
BLOAD "care.bs9", VARPTR(car(0, 4))
DEF SEG = VARSEG(car(0, 5))
BLOAD "carsee.bs9", VARPTR(car(0, 5))
DEF SEG = VARSEG(car(0, 6))
BLOAD "carse.bs9", VARPTR(car(0, 6))
DEF SEG = VARSEG(car(0, 7))
BLOAD "carsse.bs9", VARPTR(car(0, 7))
DEF SEG = VARSEG(car(0, 8))
BLOAD "cars.bs9", VARPTR(car(0, 8))
DEF SEG = VARSEG(car(0, 9))
BLOAD "carssw.bs9", VARPTR(car(0, 9))
DEF SEG = VARSEG(car(0, 10))
BLOAD "carsw.bs9", VARPTR(car(0, 10))
DEF SEG = VARSEG(car(0, 11))
BLOAD "carsww.bs9", VARPTR(car(0, 11))
DEF SEG = VARSEG(car(0, 12))
BLOAD "carw.bs9", VARPTR(car(0, 12))
DEF SEG = VARSEG(car(0, 13))
BLOAD "carnww.bs9", VARPTR(car(0, 13))
DEF SEG = VARSEG(car(0, 14))
BLOAD "carnw.bs9", VARPTR(car(0, 14))
DEF SEG = VARSEG(car(0, 15))
BLOAD "carnnw.bs9", VARPTR(car(0, 15))
END SUB

