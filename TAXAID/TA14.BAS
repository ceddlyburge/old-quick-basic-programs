DECLARE SUB editmonth ()
DECLARE SUB savemonth ()
DECLARE SUB loadmonth ()
DECLARE FUNCTION Filter$ (Txt$, FilterString$)
DECLARE SUB Travel (new)
DECLARE FUNCTION inpts2$ (maxlen!, first$, whole$, yes)
DECLARE SUB shiftpalette ()
DECLARE SUB hline (left!, right!, top!)
DECLARE FUNCTION remspace$ (string1$)
DECLARE SUB save ()
DECLARE SUB viewdist ()
DECLARE SUB vline (top!, bottom!, left!)
DECLARE SUB viewday ()
DECLARE FUNCTION inpts$ (maxlen!)
DECLARE SUB drawcol ()
DECLARE SUB shade (x!, y!, col!, density!)
DECLARE SUB shadeb (x!, y!, col!, density!)
DECLARE SUB getpic (x!, y!)
DECLARE SUB checkline (linepos!)
DECLARE SUB browse ()
DECLARE SUB inputday (daynum!)
DECLARE SUB makepaths ()
DECLARE SUB highlight2 (C!)
DECLARE SUB highlight (curs!)
DECLARE SUB clearspace (t!, B!, l!, lenn!)
DECLARE SUB checkdata ()
DECLARE SUB wp ()
DECLARE SUB printplan ()
DECLARE SUB enterstans ()
DECLARE SUB setupdist ()
DECLARE SUB setupday (p!)
DECLARE SUB setupabrev ()
DECLARE SUB exittravel ()
DECLARE FUNCTION nonstandardday (n!, count, REDO5, monthday)
DECLARE SUB phonebills ()
DECLARE SUB travelbills ()
DECLARE FUNCTION Convertdate! ()
DECLARE FUNCTION dayofweek$ (datenumber!)
DECLARE FUNCTION daynumbers! (day$)
DECLARE SUB menubox (left%, right%, top%, bottom%, n%, p%)
DECLARE SUB choices ()
DECLARE SUB setup ()
DECLARE SUB exitprog ()
DECLARE SUB frame (left%, right%, top%, bottom%)
DECLARE SUB menuoptions ()
DECLARE SUB openingmenu ()
DECLARE SUB curschoice ()
DECLARE SUB phoneset ()
DECLARE SUB travelset ()
COMMON SHARED x8, totaltotal, totaloff, DATEs$, taxmileage, untaxmileage, monthday, day, month, year, days, printphone$, filename$, curs, curss, drive$, unitcost$, vatrate$, indate$, bitstring AS STRING * 3
'x8 = 45
'DIM SHARED palettearray(x8) AS LONG
DIM SHARED stanroute(100) AS STRING * 18, stantax(100) AS STRING * 1, stanmile(100) AS STRING * 5
DIM SHARED abrev(10) AS STRING * 13
DIM SHARED route(7, 30) AS STRING * 18, mileage(7, 30) AS STRING * 5, tax(7, 30) AS STRING * 1
DIM SHARED daysinmonth(12) AS INTEGER
DIM SHARED ENDOFFILE(8) AS INTEGER
DIM SHARED printday(35, 4) AS STRING * 80
DIM SHARED text(10) AS STRING * 18
DIM SHARED text2(11) AS STRING * 18
DIM SHARED route1(31, 30) AS STRING, mileage1(31, 30) AS STRING, tax1(31, 30) AS STRING * 1, title1(31) AS STRING, dtws(31) AS STRING
CLS
'ON ERROR GOTO notega
'SCREEN 13
'SCREEN 9
'ON ERROR GOTO 0
'IF okscreen = 0 THEN
'SCREEN 13
'filename$ = "mandel.pic"
'getpic 1, 1
'COLOR 15
'LOCATE 3, 30: PRINT "Taxaid"
'LOCATE 10, 30: PRINT "Written"
'LOCATE 11, 30: PRINT "By"
'LOCATE 13, 30: PRINT "Cedd"
'LOCATE 15, 30: PRINT "Burge"
'LOCATE 18, 30: PRINT "Press"
'LOCATE 19, 30: PRINT "Any"
'LOCATE 20, 30: PRINT "Key"
'FOR n = 0 TO x8
'palettearray(n) = n
'NEXT n
'DO
'shiftpalette
'LOOP UNTIL INKEY$ <> ""
'END IF
'IF okscreen = 1 THEN
'filename$ = "car4"
'getpic 120, 100
'drawcol
'END IF
'IF okscreen = 2 THEN LOCATE 10, 1: PRINT "Sorry, screen not high enough resoloution to support opening graphics"
'IF okscreen <> 0 THEN
'frame 26, 46, 3, 5
'COLOR 5: LOCATE 4, 28: PRINT "Welcome to Taxaid"
'COLOR 7
'LOCATE 18, 25: PRINT "         Written by Cedd Burge"
'LOCATE 22, 25: PRINT " Press any key ............."
'a$ = INPUT$(1)
'END IF
LOCATE 1, 1
makepaths
DATA 31,28,31,30,31,30,31,31,30,31,30,31
FOR n = 1 TO 12
READ daysinmonth(n)
NEXT n
DATA "S to setup","N for New Month","A to Edit Month","L for load Month","T to Save Month","P to Print","V To View  Days","D to view Distances","G to Save Setup","E to Exit"
FOR n = 1 TO 10
READ text(n)
NEXT n
1
DATA 19
DATA "Setup is to enter default options for"
DATA "the program. This is the first  thing"
DATA "that you should do and  makes the   "
DATA "rest of the program a lot easier.     "
DATA "The things you can do are Enter Abrev- "
DATA "iations, Enter standard routes and     "
DATA "Enter Standard Days. Abbreviations is  "
DATA "to enter a line of text to each F-key."
DATA "It is best to put your most regulary   "
DATA "visited places here. Standard Routes    "
DATA "is to enter a route along with mileage"
DATA "and whether it is taxed. Every time you"
DATA "enter that route the mileage will be"
DATA "automatic. Standard Days is to enter"
DATA "all the routes you would do in a normal"
DATA "day. Then when you come to work out    "
DATA "all your travel for the month you can "
DATA "opt to use a standard day instead of "
DATA "typing in all the individual routes."
2
DATA 4
DATA "Browsing is to look through  the month's
DATA "travelling and check to see that it is
DATA "all correct. If not you can modify it to
DATA "your needs.
3
DATA 11
DATA "Notes is to add Lines of text to the "
DATA "finished printout. If for example you"
DATA "wanted to include information that"
DATA "isn't  included on the  printout then"
DATA "this is the way to do it. When using"
DATA "this you enter a complete line of text"
DATA "and when you press return this is sent"
DATA "to the printer. You have to put the "
DATA "paper you want printing on next to the"
DATA "print Head as the text appears
DATA "directly behind the head.
4
DATA 19
DATA "Travel records all your travelling    "
DATA "for one month, (or less if it suits "
DATA "you), So it can be sent off for tax
DATA "reclaiming. You have to enter the date"
DATA "you wish to start and then you get asked"
DATA "whether you wish to take a standard
DATA "day and the first fifteen routes of
DATA "the standard day are displayed. If you"
DATA "wish to see the last fifteen routes
DATA "instead then press 'd'. Having seen the
DATA "routes you decide whether the standard
DATA "day is what you did on that day. If it
DATA "is press 'y' and the computer works
DATA "everything out. If not press 'n' and
DATA "you will have to enter everything"
DATA "yourself. To restart a day press 'd'
DATA "when asked to enter the route. Other
DATA "options are press space to skip a day
DATA "and press 'r' to go back a day.
5
DATA 8
DATA "Phone Billing is to work out your"
DATA "Phone bills, including hire charges
DATA "you should have already entered the
DATA "the rate of V.A.T and the unit cost
DATA "of the Phone bills. So all you have
DATA "to do is enter the amount of phone
DATA "units and the hire charges and the
DATA "computer works out all the V.A.T
6
DATA 5
DATA "Printing prints out the current
DATA "travel contents. You need to have
DATA "done the Travel Billing First
DATA "and this will be printed in a
DATA "format to send off for tax reclaim
7
DATA 4
DATA "View Standard days lets you look at all
DATA "your standard days and check that they
DATA "are correct before using them. You can
DATA "also edit using this option
8
DATA 5
DATA "View Distances lets you look at all the "
DATA "standard routes you have entered and
DATA "check that they are correct. You can
DATA "also edit the distances with this
DATA "option.
9
DATA 7
DATA "Save is to save the current contents
DATA "of all the file in the program. This
DATA "will happen automatically when you
DATA "exit the program, but if you want to
DATA "backup the files to another disk or
DATA "another location on the hard disk
DATA "this is the way to do it.
10
DATA 1
DATA "Exits Taxaid"



21
DATA 6
DATA "Setup Abbreviations lets you assign"
DATA "a 13 - letter place name to each F-
DATA "key. From then on every time you
DATA "press that F - key the string of
DATA "13 letters will be printed. This
DATA "saves on typing time.
22
DATA 12
DATA "Re-enter Distances is to enter a
DATA "list of routes and mileages. You
DATA "will have to state whether they are
DATA "taxed. Routes must be entered in
DATA "both directions for the computer
DATA "to search for them
DATA ""
DATA "Warning: Selecting this option
DATA "wipes out any standard routes
DATA "you have already entered, use
DATA "View Standard Distances to Add
DATA "to the list.
23
DATA 13
DATA "Sets up a standard day. It is best
DATA "to have setup abbreviations and
DATA "distances first to save time. You
DATA "will be asked to enter a route.
DATA "To exit press e, otherwise enter
DATA "a route . If it is not found on
DATA "the standard distances you will
DATA "have to enter the mileage and
DATA "whether it is taxed or not.
DATA "Try to avoid using long place
DATA "names, because there is a limited
DATA "space on the printout. It is best
DATA "to use abbreviations.
24
DATA 2
DATA "Sets up rate of V.A.T and price
DATA "of Phone Units.
25
DATA 1
DATA "Exits Setup"
text2(1) = "Setup Abbreviations"
text2(2) = "Re-enter Distances"
text2(3) = "Setup Monday"
text2(4) = "Setup Tuesday"
text2(5) = "Setup Wednesday"
text2(6) = "Setup Thursday"
text2(7) = "Setup Friday"
text2(8) = "Setup Saturday"
text2(9) = "Setup Sunday"
text2(10) = "Setup Phone Options"
text2(11) = "Exit"

enterstans
SCREEN 9
openingmenu
choices
END

nofile:  ' error routine for opening graphics
LOCATE 1, 1: PRINT filename$; " not found, Please input path (press return if you do not have it):>"
INPUT filename$
LOCATE 1, 1: PRINT SPACE$(180)
IF filename$ = "" THEN RESUME NEXT
IF okscreen = 1 THEN filename$ = filename$ + "\car4"
IF okscreen = 0 THEN filename$ = filename$ + "\Mandel.pic"
RESUME

noprint: ' error handling for printing
PRINT "PRINTING ERROR: No printer, printer not plugged in, or printer in wrong port"
a$ = INPUT$(1)
RESUME

notega: ' must be a cga or mono screen
okscreen = okscreen + 1
RESUME NEXT

nodrive:
IF ERR = 68 OR ERR = 76 THEN   ' NO SUCH DRIVENAME
LOCATE 21, 25: PRINT "                 "
LOCATE 21, 25: INPUT drive$
IF drive$ = "" THEN drive$ = "c:\taxaid"
RESUME
END IF
IF ERR = 5 THEN
 PRINT "Insert disk with COMMAND.COM in any drive"
 a$ = INPUT$(1)
 RESUME
 
END IF
generalerr: ON ERROR GOTO 0
diskfull:
PRINT "Disk full, make changes to make space , then when finished type exit"
SHELL
RESUME

SUB browse
DIM routes(35) AS STRING * 20
CLS
COLOR 7
frame 51, 75, 4, 15
LOCATE 5, 52: PRINT "   Browsing   "
LOCATE 7, 52: PRINT "Press space or Return"
LOCATE 8, 52: PRINT "to alter day"
LOCATE 9, 52: PRINT "Press e to end"
LOCATE 10, 52: PRINT "or right mouse button"
LOCATE 11, 52: PRINT "Move mouse right, or "
LOCATE 12, 52: PRINT "press 6 to increase day"
LOCATE 13, 52: PRINT "Press 4 or move mouse"
LOCATE 14, 52: PRINT "left to decrease day"
COLOR 11
frame 1, 80, 1, 23
dayofmonth = 1
COLOR 13
frame 2, 11, 2, 4
frame 2, 50, 5, 22
COLOR 11
DO
LOCATE 3, 3: PRINT "Day "; dayofmonth
ok = 1
FOR n = 1 TO 35
routes(n) = ""
NEXT n
FOR n = 1 TO 4
length = LEN(printday(dayofmonth, n))
propday$ = RIGHT$(printday(dayofmonth, n), length - 19)
propday$ = LEFT$(propday$, 50)
DO
cont = cont + 1
place = INSTR(propday$, ",")
IF place <> 0 THEN
routes(cont) = LEFT$(propday$, place)
propday$ = RIGHT$(propday$, LEN(propday$) - place)
END IF
IF place > 0 THEN ok = 0
LOOP UNTIL cont = 35 OR place = 0
NEXT n
FOR n = 1 TO 16
LOCATE n + 5, 4: PRINT routes(n)
LOCATE n + 5, 25: PRINT routes(n + 15)
NEXT n
IF ok = 1 THEN LOCATE 10, 5: PRINT "No Travelling today"
DO
a$ = INKEY$
LOOP WHILE a$ = ""
a = ASC(RIGHT$(a$, 1))
IF a$ = "6" THEN dayofmonth = dayofmonth + 1
IF a$ = "4" THEN dayofmonth = dayofmonth - 1
IF a = 77 THEN dayofmonth = dayofmonth + 1
    DO
    q$ = INKEY$
    IF q$ = "" THEN q$ = "QWERT"
    q = ASC(RIGHT$(q$, 1))
    IF q = 77 THEN dayofmonth = dayofmonth + 1
    LOOP UNTIL q <> 77
IF a = 75 THEN dayofmonth = dayofmonth - 1
    DO
    q$ = INKEY$
    IF q$ = "" THEN q$ = "QWERT"
    q = ASC(RIGHT$(q$, 1))
    IF q = 75 THEN dayofmonth = dayofmonth - 1
    LOOP UNTIL q <> 75
IF dayofmonth < 1 THEN dayofmonth = 1
IF dayofmonth > 31 THEN dayofmonth = 31
IF a$ = "e" OR a$ = "E" OR a$ = CHR$(27) THEN EXIT DO
IF a$ = " " OR a$ = CHR$(13) THEN inputday (dayofmonth)
cont = 0
LOOP
openingmenu
END SUB

SUB choices
IF curs = 0 THEN curs = 1
DO
DO
a$ = INKEY$
LOOP WHILE a$ = ""
a = ASC(RIGHT$(a$, 1))
a$ = UCASE$(a$)
IF a$ = "E" THEN CLS : exitprog: SYSTEM
IF a$ = "S" THEN setup
IF a$ = "N" THEN Travel 1
IF a$ = "T" THEN savemonth
IF a$ = "A" THEN Travel 0
IF a$ = "P" THEN printplan
IF a$ = "L" THEN loadmonth
IF a$ = "V" THEN viewday
IF a$ = "D" THEN viewdist
IF a$ = "G" THEN save
IF a = 72 THEN curs = curs - 1: IF curs = 0 THEN curs = 10
    DO
    p$ = INKEY$
    IF p$ = "" THEN p$ = "QWERT"
    p = ASC(RIGHT$(p$, 1))
    IF p = 72 THEN curs = curs - 1: IF curs = 0 THEN curs = 10
    LOOP UNTIL p <> 72
IF a = 80 THEN curs = curs + 1: IF curs = 11 THEN curs = 1
    DO
    q$ = INKEY$
    IF q$ = "" THEN q$ = "QWERT"
    q = ASC(RIGHT$(q$, 1))
    IF q = 80 THEN curs = curs + 1: IF curs = 11 THEN curs = 1
    LOOP UNTIL q <> 80
    IF a$ = "8" THEN curs = curs - 1: IF curs = 0 THEN curs = 10
IF a$ = "2" THEN curs = curs + 1: IF curs = 11 THEN curs = 1
IF a$ = CHR$(13) OR a$ = " " THEN
   IF curs = 1 THEN setup
   IF curs = 3 THEN Travel 0
   IF curs = 4 THEN loadmonth
   IF curs = 5 THEN savemonth
   IF curs = 6 THEN printplan
   IF curs = 2 THEN Travel 1
   IF curs = 10 THEN CLS : exitprog: SYSTEM
   IF curs = 7 THEN viewday
   IF curs = 8 THEN viewdist
   IF curs = 9 THEN save
END IF
highlight curs
LOOP
END SUB

SUB clearspace (t, B, l, lenn)
FOR n = t TO B
LOCATE n, l: PRINT SPACE$(lenn)
NEXT n
END SUB

FUNCTION Convertdate
Temp = 0
ok = 0
hyphen1 = INSTR(DATEs$, "-")
hyphen2 = INSTR(hyphen1 + 1, DATEs$, "-")
IF hyphen1 <> 0 AND hyphen2 <> 0 THEN
month = VAL(LEFT$(DATEs$, hyphen1 - 1))
day = VAL(MID$(DATEs$, hyphen1 + 1, hyphen2 - hyphen1 - 1))
year = VAL(RIGHT$(DATEs$, 4))
ok = month >= 1 AND month <= 12
ok = ok AND year >= 1980
IF year MOD 4 = 0 AND NOT year MOD 25 = 0 THEN
daysinmonth(2) = 29
ELSE daysinmonth(2) = 28
END IF
IF ok THEN ok = day >= 1 AND day <= daysinmonth(month)
END IF
IF ok THEN
FOR I = 1980 TO year - 1
 Temp = Temp + 365
 IF I MOD 4 = 0 AND NOT year MOD 25 = 0 THEN Temp = Temp + 1
NEXT I
FOR I = 1 TO month - 1
Temp = Temp + daysinmonth(I)
NEXT I
Temp = Temp + day - 1
END IF
Convertdate = Temp
END FUNCTION

FUNCTION daynumbers (day$)
IF day$ = "Mon" THEN daynumbers = 1
IF day$ = "Tue" THEN daynumbers = 2
IF day$ = "Wed" THEN daynumbers = 3
IF day$ = "Thu" THEN daynumbers = 4
IF day$ = "Fri" THEN daynumbers = 5
IF day$ = "Sat" THEN daynumbers = 6
IF day$ = "Sun" THEN daynumbers = 7
END FUNCTION

FUNCTION dayofweek$ (datenumber)
dayss$ = "MonTueWedThuFriSatSun"
dayy = datenumber MOD 7
startpos = dayy * 3 + 1
dayofweek$ = MID$(dayss$, startpos, 3)
END FUNCTION

SUB editmonth

END SUB

SUB enterstans
OPEN drive$ + "\phones.dat" FOR INPUT AS #1
IF NOT EOF(1) THEN INPUT #1, vatrate$, unitcost$
CLOSE #1
OPEN drive$ + "\key.dat" FOR INPUT AS #1 LEN = 15
FOR n = 1 TO 10
IF NOT EOF(1) THEN INPUT #1, abrev(n)
KEY n, RTRIM$(abrev(n))
NEXT n
KEY ON
CLOSE #1
OPEN drive$ + "\month.dat" FOR INPUT AS #1
FOR n = 1 TO 35
FOR j = 1 TO 4
IF NOT EOF(1) THEN INPUT #1, printday(n, j)
NEXT j, n
IF NOT EOF(1) THEN INPUT #1, totaltotal, totaloff
IF NOT EOF(1) THEN INPUT #1, printphone$
CLOSE #1
OPEN drive$ + "\filelen.dat" FOR INPUT AS #1
FOR n = 1 TO 8
IF NOT EOF(1) THEN INPUT #1, ENDOFFILE(n)
NEXT n
CLOSE #1
OPEN drive$ + "\standist.dat" FOR INPUT AS #1 LEN = 15
FOR n = 1 TO 100
IF NOT EOF(1) THEN INPUT #1, stanroute(n), stanmile(n), stantax(n)
NEXT n
CLOSE #1
CLOSE #1
        OPEN drive$ + "\travel.dat" FOR INPUT AS #1 LEN = 50
        FOR p = 1 TO 7
        count = 0
        DO WHILE NOT EOF(1) AND count < 30
        count = count + 1
        IF NOT EOF(1) THEN INPUT #1, route(p, count), mileage(p, count), tax(p, count)
        LOOP
        NEXT p
        CLOSE #1

END SUB

SUB exitprog
CLOSE
ON ERROR GOTO diskfull
OPEN drive$ + "\key.dat" FOR OUTPUT AS #1 LEN = 15
FOR n = 1 TO 10
WRITE #1, abrev(n)
NEXT n
CLOSE
OPEN drive$ + "\month.dat" FOR OUTPUT AS #1
FOR n = 1 TO 35
FOR j = 1 TO 4
WRITE #1, printday(n, j)
NEXT j, n
WRITE #1, totaltotal, totaloff
WRITE #1, printphone$
CLOSE
OPEN drive$ + "\filelen.dat" FOR OUTPUT AS #1
FOR n = 1 TO 8
 WRITE #1, ENDOFFILE(n)
NEXT n
CLOSE
OPEN drive$ + "\standist.dat" FOR OUTPUT AS #1 LEN = 15
FOR n = 1 TO 100
 WRITE #1, stanroute(n), stanmile(n), stantax(n)
NEXT n
CLOSE
OPEN drive$ + "\travel.dat" FOR OUTPUT AS #1 LEN = 50
        FOR p = 1 TO 7
        FOR count = 1 TO 30
        WRITE #1, route(p, count), mileage(p, count), tax(p, count)
        NEXT count
        NEXT p
CLOSE
OPEN drive$ + "\phones.dat" FOR OUTPUT AS #1
WRITE #1, vatrate$, unitcost$
CLOSE
END SUB

FUNCTION Filter$ (Txt$, FilterString$) STATIC
   Temp$ = ""
   TxtLength = LEN(Txt$)

   FOR I = 1 TO TxtLength     ' Isolate each character in
      C$ = MID$(Txt$, I, 1)   ' the string.

      ' If the character is in the filter string, save it:
      IF INSTR(FilterString$, C$) <> 0 THEN
         Temp$ = Temp$ + C$
      END IF
   NEXT I

   Filter$ = Temp$
END FUNCTION

SUB frame (left%, right%, top%, bottom%)
LOCATE top%, left%: PRINT CHR$(201)
LOCATE top%, right%: PRINT CHR$(187)
LOCATE bottom%, left%: PRINT CHR$(200)
LOCATE bottom%, right%: PRINT CHR$(188)

FOR vert% = top% + 1 TO bottom% - 1
    LOCATE vert%, left%: PRINT CHR$(186)
    LOCATE vert%, right%: PRINT CHR$(186)
NEXT vert%


horiz% = right% - left% - 1
    hine$ = STRING$(horiz%, 205)
    LOCATE top%, left% + 1: PRINT hine$
    LOCATE bottom%, left% + 1: PRINT hine$

END SUB

SUB highlight (curs)
COLOR 7
FOR n = 1 TO 10
LOCATE n + 4, 3: PRINT text(n)
NEXT n
COLOR 15, 3
LOCATE curs + 4, 3: PRINT text(curs)
COLOR 7, 1
SELECT CASE curs
   CASE 1
    RESTORE 1
   CASE 2
    RESTORE 2
   CASE 3
    RESTORE 3
   CASE 4
    RESTORE 4
   CASE 5
    RESTORE 5
   CASE 6
    RESTORE 6
   CASE 7
    RESTORE 7
   CASE 8
    RESTORE 8
   CASE 9
    RESTORE 9
   CASE 10
    RESTORE 10
END SELECT
READ lenth
FOR n = 1 TO lenth
READ a$
LOCATE n + 2, 31: PRINT SPACE$(40)
LOCATE n + 2, 31: PRINT a$
NEXT n
FOR p = n TO 19
LOCATE p + 2, 31: PRINT SPACE$(40)
NEXT p
END SUB

SUB highlight2 (C)
COLOR 7
FOR n = 1 TO 11
LOCATE n + 4, 3: PRINT text2(n)
NEXT n
COLOR 15, 3
LOCATE C + 4, 3: PRINT text2(C)
COLOR 7, 1
IF C = 3 OR C = 4 OR C = 5 OR C = 6 OR C = 7 OR C = 8 OR C = 9 THEN C = 3
IF C = 10 THEN C = 4
IF C = 11 THEN C = 5
SELECT CASE C
 CASE 1
     RESTORE 21
 CASE 2
     RESTORE 22
 CASE 3
     RESTORE 23
 CASE 4
     RESTORE 24
 CASE 5
     RESTORE 25
END SELECT
READ lenth
FOR n = 1 TO lenth
READ a$
LOCATE n + 2, 31: PRINT SPACE$(40)
LOCATE n + 2, 31: PRINT a$
NEXT n
FOR p = n TO 19
LOCATE p + 2, 31: PRINT SPACE$(40)
NEXT p
END SUB

SUB hline (left, right, top)
FOR n = left + 1 TO right - 1
LOCATE top, n: PRINT CHR$(205)
NEXT n
LOCATE top, left: PRINT CHR$(204)
LOCATE top, right: PRINT CHR$(185)
END SUB

FUNCTION inpts$ (maxlen)
x = POS(0) - 1
y = CSRLIN
PRINT SPACE$(maxlen)
LOCATE y, x + 1: PRINT "?"
DIM cedd(0 TO maxlen + 1) AS STRING
DO
n = n + 1
IF n > maxlen THEN n = n - 1
cedd(n) = INPUT$(1)
IF cedd(n) = CHR$(13) THEN n = n - 1: EXIT DO
'IF n > maxlen THEN n = n - 1
IF cedd(n) = CHR$(8) THEN
 n = n - 2
IF n < 0 THEN n = n + 1
LOCATE y, 1 + x + n: PRINT "   "
LOCATE y, 1 + x + n: PRINT "_"
END IF
LOCATE y, x + n
PRINT cedd(n)
LOCATE y, x + n + 1: PRINT "_"
LOOP
FOR p = 1 TO n
is$ = is$ + cedd(p)
NEXT p
inpts$ = is$
END FUNCTION

FUNCTION inpts2$ (maxlen, first$, whole$, yes)
STATIC dirn
IF yes = 1 THEN inpts2$ = STR$(dirn): EXIT FUNCTION
x = POS(0) - 1
y = CSRLIN
IF maxlen = 1 THEN
        IF firts$ = CHR$(27) THEN dirn = 227: inpts2$ = whole$
        IF LEN(first$) = 1 THEN
                inpts2$ = first$
                dirn = 77
        ELSE
                dirn = ASC(RIGHT$(first$, 1))
                inpts2$ = whole$
        END IF
        EXIT FUNCTION
END IF
PRINT SPACE$(maxlen)
DIM cedd(maxlen + 1) AS STRING
cedd(1) = first$
IF LEN(whole$) > 1 THEN
FOR n = 2 TO LEN(whole$)
cedd(n) = RIGHT$(LEFT$(whole$, n), 1)
NEXT n
END IF
n = 1
FOR t = 1 TO maxlen
LOCATE y, x + t: PRINT cedd(t)
NEXT t
IF cedd(1) = CHR$(13) THEN inpts2$ = "": EXIT FUNCTION
DO
n = n + 1
IF n > maxlen THEN n = n - 1
DO
LOCATE y, x + n, 1
cedd(n) = INKEY$
LOOP WHILE cedd(n) = ""
IF cedd(n) = CHR$(27) THEN dirn = 227: cedd(n) = "": n = n - 1: EXIT DO
IF ASC(RIGHT$(cedd(n), 1)) = 9 THEN cedd(n) = "": n = n - 1
IF LEN(cedd(n)) = 2 THEN
        dirn = ASC(RIGHT$(cedd(n), 1))
        cedd(n) = ""
        n = n - 1
        EXIT DO
END IF
IF cedd(n) = CHR$(13) THEN cedd(n) = "": n = n - 1: dirn = 77: EXIT DO
'IF n > maxlen THEN n = n - 1
IF cedd(n) = CHR$(8) THEN
cedd(n) = ""
IF n >= length THEN length = n - 2
n = n - 2
IF n < 0 THEN n = n + 1
LOCATE y, 1 + x + n: PRINT "  "
'LOCATE y, 1 + x + n: PRINT "_"
END IF
LOCATE y, x + n, 1
PRINT cedd(n)
'LOCATE y, x + n + 1: PRINT "_"
IF n >= length THEN length = n
LOOP
is$ = ""
FOR p = 1 TO maxlen
is$ = is$ + cedd(p)
NEXT p
inpts2$ = is$
LOCATE 1, 1, 0
END FUNCTION

SUB inputday (daynum)
' re configure printday 1 and 2 for the value of daynum
' frame in which to work is 2, 50,5,23
DIM test(5) AS STRING
printday(daynum, 3) = ""
printday(daynum, 4) = ""
printday(daynum, 2) = LEFT$(printday(daynum, 2), 19)
LOCATE 2, 15: PRINT "Press space or return to enter route"
COLOR 13
frame 2, 50, 5, 22
DIM routes(35) AS STRING * 18
DIM taxs(35) AS STRING * 1
DIM mile(35) AS STRING * 5
clearspace 6, 21, 3, 45
LOCATE 5, 3: PRINT "Route================Mile==Taxed?"
vline 5, 22, 23
vline 5, 22, 29
start = 1
local2 = 1
part = 1
COLOR 7, 1
DO
FOR n = start TO start + 15
LOCATE n - start + 6, 4, 0: PRINT routes(n)
LOCATE n - start + 6, 24, 0: PRINT mile(n)
LOCATE n - start + 6, 30, 0: PRINT taxs(n), n; " "
NEXT n
COLOR 15, 3
IF part = 1 THEN LOCATE local2 + 5, 4, 0: PRINT routes(local2 + start - 1)
IF part = 2 THEN LOCATE local2 + 5, 24, 0: PRINT mile(local2 + start - 1)
IF part = 3 THEN LOCATE local2 + 5, 30, 0: PRINT taxs(local2 + start - 1)
COLOR 7, 1
DO
a$ = INKEY$
LOOP WHILE a$ = ""
a = ASC(RIGHT$(a$, 1))
IF a <> 72 AND a <> 73 AND a <> 75 AND a <> 77 AND a <> 80 AND a <> 81 THEN
  COLOR 15, 3
  IF part = 1 THEN
  LOCATE 3, 15: PRINT "Enter Route  "
  LOCATE local2 + 5, 4: routes(local2 + start - 1) = inpts2(17, a$, routes(local2 + start - 1), 0)
     a = VAL(inpts2$(1, "", "", 1))
     ok = 0
     FOR n = 1 TO 100
       IF UCASE$(stanroute(n)) = UCASE$(routes(local2 + start - 1)) THEN
        mile(local2 + start - 1) = stanmile(n)
        taxs(local2 + start - 1) = stantax(n)
        ok = 1
        EXIT FOR
       END IF
      NEXT n
   END IF
pink = 1
test(1) = ""
test(2) = ""
test(3) = ""
test(4) = ""
FOR n = 1 TO 30
temproutes$ = routes(n)
test(pink) = test(pink) + RTRIM$(temproutes$)
test(pink) = RTRIM$(LTRIM$(test(pink)))
IF RTRIM$(TEMPROUTE$) <> "" THEN test(pink) = test(pink) + ","
temproutes2$ = remspace$(routes(n + 1))
IF LEN(test(pink)) + LEN(temproutes2$) > 50 THEN
IF pink = 4 THEN LOCATE 3, 44: PRINT "No More Room for routes": pink = pink + 1: EXIT FOR
IF pink < 4 THEN pink = pink + 1
END IF
NEXT n
IF pink = 4 THEN LOCATE 3, 44: COLOR 7, 1: PRINT SPACE$(25)
IF part = 2 THEN
      LOCATE 3, 15: PRINT "Enter Mileage"
      LOCATE local2 + 5, 24: mile(local2 + start - 1) = inpts2(4, a$, mile(local2 + start - 1), 0)
      a = VAL(inpts2$(1, "", "", 1))
      END IF
      IF part = 3 THEN
      LOCATE 3, 15: PRINT "Taxed?        "
      LOCATE local2 + 5, 30: PRINT "?"
      IF UCASE$(a$) = "N" OR UCASE$(a$) = "Y" OR a$ = " " THEN taxs(local2 + start - 1) = inpts2(1, a$, taxs(local2 + start - 1), 1)
      a = VAL(inpts2$(1, "", "", 1))
      END IF
 'part = part + 1: IF part = 4 THEN part = 1: local2 = local2 + 1: IF local2 = 17 THEN local2 = 16: start = start + 1: IF start = 16 THEN start = 15
END IF
a$ = LCASE$(a$)
IF a = 80 THEN
 local2 = local2 + 1
 IF local2 = 17 THEN
   local2 = 16
   start = start + 1
    IF start = 16 THEN start = 15
 END IF
END IF
IF a = 72 THEN
 local2 = local2 - 1
 IF local2 = 0 THEN
   local2 = 1
   start = start - 1
    IF start = 0 THEN start = 1
 END IF
END IF
IF a = 77 THEN part = part + 1: IF part = 4 THEN part = 1
IF a = 75 THEN part = part - 1: IF part = 0 THEN part = 3
IF a$ = CHR$(27) THEN EXIT DO
IF a = 227 THEN EXIT DO
COLOR 7, 1
LOCATE 3, 15: PRINT "                       "
LOOP UNTIL a$ = CHR$(27)
clearspace 6, 21, 3, 45
FOR n = 1 TO 30
IF LCASE$(taxs(n)) = "n" THEN untaxmile = untaxmile + VAL(mile(n))
IF LCASE$(taxs(n)) = "y" THEN taxmile = taxmile + VAL(mile(n))
NEXT n
'DO
'count2 = count2 + 1
'IF remspace$(routes(count2)) <> "" THEN main$ = main$ + remspace$(routes(count2)) + ","
'lenth = lenth + LEN(routes(count2))
'LOOP UNTIL count2 = 30 OR lenth + LEN(remspace$(routes(count2 + 1))) >= 50
'a = LEN(main$)
'FOR n = a TO 50
'main$ = main$ + " "
'NEXT n
length = LEN(printday(daynum, 1))
totmile = VAL(RIGHT$(printday(daynum, 1), length - 76))
totmiles = taxmile + untaxmile
totaltotal = totaltotal - totmile + totmiles
offmile = VAL(LEFT$(RIGHT$(printday(daynum, 1), length - 71), 4))
totaloff = totaloff - offmile + untaxmile
untaxmile$ = STR$(untaxmile)
FOR n = LEN(untaxmile$) TO 4
untaxmile$ = untaxmile$ + " "
NEXT n
'printday(daynum, 1) = LEFT$(printday(daynum, 1), 18) + " " + main$ + STR$(untaxmile) + STR$(totmiles)
lenth = 0
main$ = ""
dfs = 0
DO
count2 = count2 + 1
IF remspace$(routes(count2)) <> "" THEN main$ = main$ + remspace$(routes(count2)) + ","
lenth = lenth + LEN(remspace$(routes(count2))) + 1
IF lenth + LEN(remspace$(routes(count2 + 1))) > 50 OR count2 = 31 THEN
dfs = dfs + 1
FOR n = lenth TO 50
main$ = main$ + " "
NEXT n
lenth = 0
IF dfs = 4 THEN EXIT DO
printday(daynum, dfs) = LEFT$(printday(daynum, dfs), 19) + main$
IF dfs = 1 THEN printday(daynum, 1) = LEFT$(printday(daynum, 1), 70) + LEFT$(untaxmile$, 4) + STR$(totmiles)
main$ = ""
END IF
LOOP UNTIL count2 = 31
clearspace 6, 21, 3, 45
COLOR 7, 1
LOCATE 2, 15: PRINT "                                                    "
COLOR 13
clearspace 6, 21, 3, 45
frame 2, 50, 5, 22
COLOR 11, 1
END SUB

SUB loadmonth
COLOR 13, 0
frame 31, 64, 10, 16
COLOR 7, 0
LOCATE 11, 32: PRINT "Enter Filename and path of      "
LOCATE 12, 32: PRINT "month to be loaded.             "
LOCATE 13, 32: PRINT "                                "
LOCATE 14, 32: PRINT "                                "
LOCATE 15, 32: PRINT "                                "
LOCATE 15, 32: INPUT d$
OPEN d$ FOR INPUT AS #1
INPUT #1, indate$
FOR n = 1 TO 31
FOR x = 1 TO 30
INPUT #1, route1(n, x), mileage1(n, x), tax1(n, x)
NEXT x, n
FOR n = 1 TO 31
INPUT #1, dtws(n), title1(n)
NEXT n
CLOSE
COLOR , 1
END SUB

SUB makepaths
COLOR 13
frame 4, 37, 15, 22
COLOR 7
LOCATE 16, 5: PRINT "Enter Drive and Directory      "
LOCATE 17, 5: PRINT "that data files are stored     "
LOCATE 18, 5: PRINT "on. If the files do not exist  "
LOCATE 19, 5: PRINT "the program will create them   "
LOCATE 20, 5: PRINT "Press return for default       "
LOCATE 21, 5: INPUT drive$
IF drive$ = "" THEN drive$ = "c:\taxaid"
'ON ERROR GOTO nodrive
CLOSE
IF LEFT$(drive$, 2) = "c:" OR LEFT$(drive$, 2) = "C:" THEN SHELL "md " + drive$
OPEN drive$ + "\month.dat" FOR APPEND AS #1
CLOSE
OPEN drive$ + "\phones.dat" FOR APPEND AS #1
CLOSE
OPEN drive$ + "\standist.dat" FOR APPEND AS #1
CLOSE
OPEN drive$ + "\filelen.dat" FOR APPEND AS #1
CLOSE
OPEN drive$ + "\key.dat" FOR APPEND AS #1
CLOSE
OPEN drive$ + "\travel.dat" FOR APPEND AS #1
CLOSE
'ON ERROR GOTO 0
'EXIT SUB
'ON ERROR GOTO 0
END SUB

FUNCTION nonstandardday (n, count, REDO5, monthday)
DIM test(5) AS STRING
DIM help(20) AS STRING * 39
part = 1
help(3) = "You will first be asked to enter a   " ' would have used data statements but
help(4) = "Title of no more than 14 letters, this" ' they are illegal in subs and functions
help(5) = "is a short description of the day. The"
help(6) = "first 7 letters go on one line of the"
help(7) = "printout and the last 7 go on the next"
help(8) = "line."
help(9) = "From then on you will get asked to "
help(10) = "enter a route, and if this is not one"
help(11) = "of the standard routes you will also "
help(12) = "be asked to enter the mileage and "
help(13) = "whether it is taxed. "
FOR qa = 3 TO 20
LOCATE qa, 3: PRINT help(qa)
NEXT qa
taxmileage = 0
untaxmileage = 0
count2 = 0
DIM printroute(10) AS STRING, TEMPROUTE(32) AS STRING * 18, temptax(32) AS STRING * 1, tempmile(32) AS STRING * 5
title$ = "Service       "
IF REDO5 = 1 THEN
        FOR p = 1 TO 30            'assigning default values to the routes
        TEMPROUTE(p) = route(n, p)
        temptax(p) = tax(n, p)
        tempmile(p) = mileage(n, p)
        NEXT p
END IF
IF REDO5 = 0 THEN
        FOR p = 1 TO 30
        TEMPROUTE(p) = route1(monthday, p)
        temptax(p) = tax1(monthday, p)
        tempmile(p) = mileage1(monthday, p)
        NEXT p
        title$ = title1(monthday)
END IF
LOCATE 4, 44: PRINT dayofweek(n - 1)
'LOCATE 5, 44
'PRINT "Enter Title for Day, 14 Max"
'LOCATE 6, 44
'title$ = inpts(14)      ' this is done to keep the length of the date constant so
'pa = LEN(title$)   ' everything comes out in neat lines on the printer
'  FOR hjk = pa TO 14
'  title$ = title$ + " "
'  NEXT hjk
start = 1
local2 = 1
COLOR 13
hline 43, 79, 5
vline 5, 22, 63
vline 5, 22, 69
vline 5, 22, 73
COLOR 7, 1
DO
LOCATE 4, 44: PRINT "Title "; title$
IF start = 0 THEN iu = 1 ELSE iu = 0
FOR n = start + iu TO start + 15 + iu
LOCATE n - start + 6 - iu, 44: PRINT TEMPROUTE(n)
LOCATE n - start + 6 - iu, 64: PRINT tempmile(n)
LOCATE n - start + 6 - iu, 70: PRINT temptax(n)
LOCATE n - start + 6 - iu, 74: PRINT n
NEXT n
COLOR 15, 3
IF start = 0 THEN
LOCATE 4, 44: PRINT "Title "; title$
COLOR 7, 1
ELSE
IF part = 1 THEN LOCATE local2 + 5, 44: PRINT TEMPROUTE(local2 + start - 1)
IF part = 2 THEN LOCATE local2 + 5, 64: PRINT tempmile(local2 + start - 1)
IF part = 3 THEN LOCATE local2 + 5, 70: PRINT temptax(local2 + start - 1)
COLOR 7, 1
END IF
DO
a$ = INKEY$
LOOP WHILE a$ = ""
a = ASC(RIGHT$(a$, 1))
IF a$ = CHR$(27) THEN EXIT DO
IF start = 0 THEN part = 1
IF a <> 72 AND a <> 75 AND a <> 73 AND a <> 77 AND a <> 80 AND a <> 81 THEN
  COLOR 15, 3
  IF start <> 0 THEN
  IF part = 1 THEN
  LOCATE 3, 44: PRINT "Enter route                 "
  LOCATE local2 + 5, 44: TEMPROUTE(local2 + start - 1) = inpts2(17, a$, TEMPROUTE(local2 + start - 1), 0)
  a = VAL(inpts2$(1, "", "", 1))
     ok = 0
     FOR n = 1 TO 100
       IF UCASE$(stanroute(n)) = UCASE$(TEMPROUTE(local2 + start - 1)) THEN
        tempmile(local2 + start - 1) = stanmile(n)
        temptax(local2 + start - 1) = stantax(n)
        ok = 1
        EXIT FOR
       END IF
      NEXT n
 END IF
 END IF
 IF part = 2 THEN
      LOCATE 3, 44: PRINT "Enter Mileage               "
      LOCATE local2 + 5, 64:
      tempmile(local2 + start - 1) = Filter$(inpts2(4, a$, tempmile(local2 + start - 1), 0), "0123456789.")
      a = VAL(inpts2$(1, "", "", 1))
 END IF
 IF part = 3 THEN
      LOCATE 3, 44: PRINT "Taxed ?                     "
      temptax(local2 + start - 1) = inpts2$(1, a$, temptax(local2 + start - 1), 0)
      a = VAL(inpts2$(1, "", "", 1))
 END IF
COLOR 7, 1
LOCATE 3, 44: PRINT day$ + " The " + STR$(days) + " of " + STR$(month) + STR$(year); "      "
pink = 1
test(1) = ""
test(2) = ""
test(3) = ""
test(4) = ""
FOR n = 1 TO 30
temproutes$ = TEMPROUTE(n)
test(pink) = test(pink) + RTRIM$(temproutes$)
test(pink) = RTRIM$(LTRIM$(test(pink)))
IF RTRIM$(TEMPROUTE$) <> "" THEN test(pink) = test(pink) + ","
temproutes2$ = remspace$(TEMPROUTE(n + 1))
IF LEN(test(pink)) + LEN(temproutes2$) > 50 THEN
IF pink = 4 THEN LOCATE 3, 44: PRINT "No More Room for routes"
IF pink < 4 THEN pink = pink + 1
END IF
NEXT n
IF pink = 3 THEN LOCATE 3, 44: PRINT SPACE$(20)
'part = part + 1
IF part = 4 THEN
 part = 1
 local2 = local2 + 1
 IF local2 = 17 THEN
   local2 = 16
   start = start + 1
    IF start = 16 THEN start = 15
END IF
END IF
IF start = 0 THEN
 LOCATE 4, 50: title$ = inpts2(14, a$, title$, 0)
 a = VAL(inpts2$(1, "", "", 1))
 title$ = title$ + "               "
 title$ = LEFT$(title$, 14)
END IF
END IF
IF a = 227 THEN EXIT DO
IF a = 73 OR a = 81 THEN EXIT DO
a$ = LCASE$(a$)
IF a = 75 THEN part = part - 1: IF part = 0 THEN part = 3
IF a = 77 THEN part = part + 1: IF part = 4 THEN part = 1
IF a = 80 THEN
 IF start = 0 THEN start = start + 1
 local2 = local2 + 1
 IF local2 = 17 THEN
   local2 = 16
   start = start + 1
    IF start = 16 THEN start = 15
 END IF
END IF
IF a = 72 THEN
 local2 = local2 - 1
 IF local2 = 0 THEN
   local2 = 1
   start = start - 1
    IF start = -1 THEN start = 0
  END IF
END IF
IF a$ = CHR$(27) THEN EXIT DO
LOOP
FOR p = 1 TO 30
route1(monthday, p) = TEMPROUTE(p)
tax1(monthday, p) = temptax(p)
mileage1(monthday, p) = tempmile(p)
NEXT p
title1(monthday) = title$
clearspace 3, 21, 44, 35
ERASE help
COLOR 13
frame 43, 79, 2, 22
COLOR 7, 1
nonstandardday = a
END FUNCTION

SUB openingmenu
SCREEN 0, 0, 0, 0
COLOR 11, 1
CLS
frame 1, 80, 1, 23
COLOR 13, 1
frame 2, 21, 2, 15
LOCATE 4, 3: PRINT STRING$(18, 196)
COLOR 15
LOCATE 3, 3: PRINT " Taxaid"
COLOR , 3
LOCATE 5, 3: PRINT text(1)
COLOR 7, 1
FOR n = 2 TO 10
LOCATE n + 4, 3: PRINT text(n)
NEXT n
frame 30, 71, 2, 22
COLOR 7
RESTORE 1
READ lenth
FOR n = 1 TO lenth
READ a$
LOCATE n + 2, 31: PRINT a$
NEXT n
LOCATE 22, 5: PRINT "Press Return to select"
LOCATE , , 0
END SUB

SUB phonebills
CLS
COLOR 11
frame 1, 80, 1, 23
DIM hirecharges(50) AS STRING
CLOSE #1
COLOR 14
frame 40, 76, 5, 16
COLOR 13
LOCATE 6, 45: PRINT "   Phone Billing"
LOCATE 8, 41: PRINT "Enter Number of Units"
LOCATE 9, 41: INPUT unit
DO
count = count + 1
count2 = count2 + 1
LOCATE count + 9, 41
PRINT "Enter Hire charges, e to end"; count2
LOCATE count + 10, 41
hirecharges(count2) = inpts(20)
IF count > 4 THEN count = 0: clearspace 8, 15, 41, 35
LOOP UNTIL LCASE$(hirecharges(count2)) = "e"
unit = unit * VAL(unitcost$)
costsunit = unit + (unit / 100) * VAL(vatrate$)
FOR n = 1 TO count2 - 1
costhire = costhire + 100 * VAL(hirecharges(n))
NEXT n
costhire = costhire + (costhire / 100) * VAL(vatrate$)
costhire = INT(costhire)
costhire = costhire / 100
costsunit = INT(costsunit)
costsunit = costsunit / 100
clearspace 8, 15, 41, 35
LOCATE 8, 41: PRINT "Cost Of Units + V.A.T = "; costsunit
LOCATE 9, 41: PRINT "Cost Of Hire Charges = "; costhire
total = costhire + costsunit
LOCATE 10, 41: PRINT "Total = "; total
a$ = INPUT$(1)
printphone$ = STR$(costsunit) + "         " + STR$(costhire)
openingmenu
END SUB

SUB phoneset
        clearspace 2, 22, 30, 50
        COLOR 13
        frame 26, 68, 5, 22
        COLOR 12
        LOCATE 6, 35: PRINT "         Phone Setup"
        LOCATE 11, 29: PRINT "Enter The rate of V.A.T in Percent"
        LOCATE 12, 29: vatrate$ = inpts(8)
        LOCATE 14, 29: PRINT "Enter Unit cost of Phone bills in Pence"
        LOCATE 15, 29: unitcost$ = inpts(8)
        clearspace 2, 22, 25, 55
END SUB

SUB printplan
DIM printroute(4)   AS STRING
COLOR 13, 0
frame 31, 64, 10, 16
COLOR 7, 0
LOCATE 11, 32: PRINT "Enter number of days to be      "
LOCATE 12, 32: PRINT "printed. eg. only 28 for        "
LOCATE 13, 32: PRINT "February.                       "
LOCATE 14, 32: PRINT "                                "
LOCATE 15, 32: PRINT "                                "
LOCATE 15, 32: INPUT d6
COLOR , 1
totaltotal = 0
totaloff = 0
FOR monthday = 1 TO d6
untaxmileage = 0
taxmileage = 0
FOR n = 1 TO 30
IF LCASE$(tax1(monthday, n)) = "n" THEN untaxmileage = untaxmileage + VAL(mileage1(monthday, n))
IF LCASE$(tax1(monthday, n)) = "y" THEN taxmileage = taxmileage + VAL(mileage1(monthday, n))
NEXT n
gt = 1
FOR z = 1 TO 4
printroute(z) = ""
NEXT z
FOR pcas = 1 TO 30
IF remspace$(route1(monthday, pcas)) <> "" THEN printroute(gt) = printroute(gt) + remspace$(route1(monthday, pcas)) + ","
IF pcas < 30 THEN
        IF LEN(printroute(gt)) + LEN((remspace$(route1(monthday, pcas + 1)))) > 50 THEN
         gt = gt + 1
        END IF
END IF
NEXT pcas
FOR y2 = 1 TO 4
p = LEN(printroute(y2))
IF p < 50 THEN
   FOR ty = p TO 50
   printroute(y2) = printroute(y2) + " "
   NEXT ty
END IF
NEXT y2
totmile$ = STR$(taxmileage + untaxmileage)
totaltotal = totaltotal + taxmileage + untaxmileage
totaloff = totaloff + untaxmileage
untaxmileage$ = STR$(untaxmileage)
p = LEN(untaxmileage$)
FOR uup = p TO 4
untaxmileage$ = untaxmileage$ + " "
NEXT uup
printday(monthday, 1) = dtws(monthday) + " " + LEFT$(title1(monthday), 7) + " " + printroute(1) + LEFT$(untaxmileage$, 5) + LEFT$(totmile$, 5)
printday(monthday, 2) = "          " + RIGHT$(title1(monthday), 7) + " " + printroute(2)
printday(monthday, 3) = "                  " + printroute(3)
printday(monthday, 4) = "                  " + printroute(4)
NEXT monthday

'ON ERROR GOTO noprint
CLOSE #1
OPEN "lpt1:bin" FOR OUTPUT AS #1
'e$ = CHR$(27)
'LPRINT e$; "10"; CHR$(40);
'LPRINT e$; "B"
'LPRINT e$; "R"
LPRINT " Date Started "; LEFT$(printday(1, 1), 9)
LPRINT
'LPRINT e$; "h"; CHR$(0)
'LPRINT e$; CHR$(102); CHR$(49); CHR$(2)
PRINT #1, "Date     Title     Route for day                                  Official Total"
LPRINT
'LPRINT e$; "3"; CHR$(17)
FOR n = 1 TO 32
FOR p = 1 TO 4
IF remspace$(printday(n, p)) <> "" THEN PRINT #1, printday(n, p)
NEXT p
NEXT n
LPRINT
PRINT #1, "Total Oficial Mileage"; STR$(INT(totaloff * 10) / 10)
LPRINT
PRINT #1, "Total Mileage"; STR$(INT(totaltotal * 10) / 10)
LPRINT
LPRINT
LPRINT
LPRINT
CLOSE #1
ON ERROR GOTO 0
END SUB

FUNCTION remspace$ (string1$)
count = LEN(string1$)
FOR n = 1 TO count
IF MID$(string1$, count, 1) = CHR$(32) OR MID$(string1$, count, 1) = CHR$(0) THEN count = count - 1
NEXT n
string1$ = LEFT$(string1$, count)
remspace$ = string1$
END FUNCTION

SUB save
COLOR 13, 0
clearspace 16, 22, 24, 33
frame 24, 57, 16, 22
COLOR 7
LOCATE 17, 25: PRINT "Enter Drive and Directory      "
LOCATE 18, 25: PRINT "that data files are stored     "
LOCATE 19, 25: PRINT "on. If the files do not exist  "
LOCATE 20, 25: PRINT "the program will create them   "
LOCATE 21, 25: INPUT drive$
IF drive$ = "" THEN drive$ = "c:\taxaid"
ON ERROR GOTO nodrive
exitprog
openingmenu
END SUB

SUB savemonth
STATIC save1$
COLOR 13, 0
frame 31, 64, 10, 16
COLOR 7, 0
LOCATE 11, 32: PRINT "Enter Filename and path of      "
LOCATE 12, 32: PRINT "month to be saved.              "
LOCATE 13, 32: PRINT "Press return for last used      "
LOCATE 14, 32: PRINT "filename.                       "
LOCATE 15, 32: PRINT "                                "
LOCATE 15, 32: INPUT a$
IF a$ <> "" THEN save1$ = a$
OPEN save1$ FOR OUTPUT AS #1
WRITE #1, indate$
FOR n = 1 TO 31
FOR x = 1 TO 30
WRITE #1, route1(n, x), mileage1(n, x), tax1(n, x)
NEXT x, n
FOR n = 1 TO 31
WRITE #1, dtws(n), title1(n)
NEXT n
CLOSE
COLOR , 1
END SUB

SUB setup
IF curss = 0 THEN curss = 1
COLOR 11
frame 1, 80, 1, 23
COLOR 13
frame 2, 21, 2, 16
LOCATE 5, 3: COLOR 15, 3: PRINT text2(1)
COLOR 7, 1
FOR n = 2 TO 11
LOCATE 4 + n, 3: PRINT text2(n)
NEXT n
highlight2 (curss)
DO
DO
a$ = INKEY$
LOOP WHILE a$ = ""
a = ASC(RIGHT$(a$, 1))
IF a$ = "8" THEN curss = curss - 1: IF curss = 0 THEN curss = 11
IF a$ = "2" THEN curss = curss + 1: IF curss = 12 THEN curss = 1
IF a = 72 THEN curss = curss - 1: IF curss = 0 THEN curss = 11
    DO
    p$ = INKEY$
    IF p$ = "" THEN p$ = "QWERT"
    p = ASC(RIGHT$(p$, 1))
    IF p = 72 THEN curss = curss - 1: IF curss = 0 THEN curss = 11
    LOOP UNTIL p <> 72

IF a = 80 THEN curss = curss + 1: IF curss = 12 THEN curss = 1
    DO
    q$ = INKEY$
    IF q$ = "" THEN q$ = "QWERT"
    q = ASC(RIGHT$(q$, 1))
    IF q = 80 THEN curss = curss + 1: IF curss = 12 THEN curss = 1
    LOOP UNTIL q <> 80

IF a$ = " " OR a$ = CHR$(13) THEN
  IF curss = 1 THEN setupabrev
  IF curss = 2 THEN setupdist
  FOR n = 3 TO 9
  IF curss = n THEN setupday (n - 2)
  NEXT n
  IF curss = 10 THEN phoneset
  IF curss = 11 THEN EXIT DO
END IF
IF a$ = CHR$(27) THEN EXIT DO
highlight2 (curss)
COLOR 7
frame 30, 71, 2, 22
LOOP
openingmenu
END SUB

SUB setupabrev
         clearspace 2, 22, 30, 50
        COLOR 13
        frame 40, 75, 2, 22
        COLOR 7
        LOCATE 3, 50: PRINT "Travel Setup"
        LOCATE 5, 41: PRINT "Entering Abbreviations"
        LOCATE 6, 41: PRINT "For Function keys"
        LOCATE 7, 41: PRINT "12 letters max"
        FOR n = 1 TO 10
        LOCATE n + 7, 41: PRINT "Enter text for F"; n; "              "
        LOCATE n + 8, 41: abrev(n) = inpts(13)
        abrev(n) = LCASE$(RTRIM$(LTRIM$(abrev(n))))
        KEY n, RTRIM$(abrev(n))
        NEXT n
        clearspace 2, 22, 30, 50
END SUB

SUB setupday (p)
        DIM test(2) AS STRING
        cedd = 1
        clearspace 2, 22, 30, 50
         COLOR 13
        frame 40, 75, 2, 22
        LOCATE 3, 41: PRINT "Entering Default days   "
        day$ = "MonTueWedThuFriSatSun"
        LOCATE 5, 41: PRINT MID$(day$, p * 3 - 2, 3)
        count = 0
        I = 1
        COLOR 7
        DO
        count = count + 1
        LOCATE I + 5, 41: PRINT "Enter route E to exit"; count
        LOCATE I + 6, 41: route(p, count) = inpts(17)
         IF LCASE$(RTRIM$(LTRIM$(route(p, count)))) = "e" THEN EXIT DO
         test(cedd) = test(cedd) + route(p, count)
         test(cedd) = RTRIM$(LTRIM$(test(cedd))) + ","
         IF LEN(test(cedd)) > 50 THEN
          IF cedd < 4 THEN cedd = cedd + 1: test(cedd) = route(p, count)
          IF cedd = 4 THEN
           LOCATE , 41
           PRINT "No more room for routes"
           I = I + 1
          END IF
         END IF
         FOR n = 1 TO 100
           IF UCASE$(RTRIM$(stanroute(n))) = UCASE$(RTRIM$(route(p, count))) THEN
           mileage(p, count) = stanmile(n)
           tax(p, count) = stantax(n)
           I = I + 2
           ok = 0
           IF I > 12 THEN I = 0: clearspace 3, 21, 41, 34
           EXIT FOR
         END IF
         ok = 1
         NEXT n
        IF ok = 1 THEN
        LOCATE I + 7, 41: PRINT "Enter Mileage"; count
        DO
        LOCATE I + 8, 41:  mileage(p, count) = inpts(4)
        LOOP UNTIL VAL(mileage(p, count)) < 1000 AND VAL(mileage(p, count)) > 0
        DO
        LOCATE I + 9, 41: PRINT "Taxed Y/N ?"
        tax(p, count) = UCASE$(INPUT$(1))
        LOOP UNTIL (tax(p, count) = "Y" OR tax(p, count) = "N")
        I = I + 4
        IF I > 12 THEN I = 0: clearspace 3, 21, 41, 34
        END IF
        LOOP
        ENDOFFILE(p) = count - 1
        FOR z = count TO 30
        route(p, z) = "  ": mileage(p, z) = "  ": tax(p, z) = "  "
        NEXT z
        clearspace 2, 22, 24, 56
 END SUB

SUB setupdist
clearspace 2, 22, 30, 50
COLOR 13
frame 40, 75, 2, 22
COLOR 15
LOCATE 3, 45: PRINT "Entering Standard Distances"
COLOR 7
DO WHILE count2 < 99
count = count + 3
count2 = count2 + 1
LOCATE count + 3, 41
PRINT "Enter Route, E to Exit"
LOCATE count + 4, 41
stanroute(count2) = inpts(15)
IF UCASE$(RTRIM$(LTRIM$(stanroute(count2)))) = "E" THEN EXIT DO
LOCATE count + 5, 41
PRINT "Enter Mileage "; : stanmile(count2) = inpts(8)
LOCATE count + 6, 41
PRINT "Taxed Y\N ": stantax(count2) = INPUT$(1)
IF count > 14 THEN
   LOCATE 4, 41
   count = 0
   clearspace 5, 21, 41, 34
END IF
LOOP
ENDOFFILE(8) = count2 - 1
clearspace 2, 22, 23, 57
END SUB

SUB Travel (new)
CLS
COLOR 11
frame 1, 80, 1, 23
COLOR 13
frame 43, 79, 2, 22
frame 2, 42, 2, 22
COLOR 7
IF new = 1 THEN
 LOCATE 3, 44: PRINT "          Travel Billing"
 LOCATE 5, 44: PRINT "Enter date of day to start"
 LOCATE 6, 44: PRINT "SYNTAX : mm-dd-yyyy"
 DO
 LOCATE 7, 44: PRINT "              "
 LOCATE 7, 44: DATEs$ = inpts(11)
 DATEs$ = LEFT$(DATEs$, 10)
 days$ = MID$(DATEs$, 4, 2)
 days = VAL(days$)
 day = Convertdate
 year = VAL(RIGHT$(DATEs$, 4))
 month = VAL(LEFT$(DATEs$, 2))
 LOOP UNTIL day <> 0
 d1$ = days$
 d2 = days
 d3 = day
 d4 = year
 d5 = month

 FOR ghj = 1 TO 31 'initialise routes as standard
        title1(ghj) = "Service       "
        bitstring = STR$(days)
        dtws(ghj) = bitstring
        bitstring = STR$(month)
        dtws(ghj) = dtws(ghj) + bitstring + " " + RIGHT$(STR$(year), 2)
        day$ = dayofweek((day MOD 7) + 1)
        daynum = daynumbers(day$)
        FOR gty = 1 TO 30
                route1(ghj, gty) = route(daynum, gty)
                mileage1(ghj, gty) = mileage(daynum, gty)
                tax1(ghj, gty) = tax(daynum, gty)
        NEXT gty
        day = day + 1
        monthday = monthday + 1
        IF monthday > max5 THEN max5 = monthday
        days = days + 1
        IF days > daysinmonth(month) THEN
        days = 1
        month = month + 1
        IF month > 12 THEN
        year = year + 1
        month = 1
        END IF
        END IF
 NEXT ghj

  days$ = d1$ 'restore initial values of date etc.
  days = d2
  day = d3
  year = d4
  month = d5

END IF

IF new = 0 THEN
 DATEs$ = LEFT$(indate$, 10)
 days$ = MID$(DATEs$, 4, 2)
 days = VAL(days$)
 day = Convertdate
 year = VAL(RIGHT$(DATEs$, 4))
 month = VAL(LEFT$(DATEs$, 2))
END IF
indate$ = DATEs$
monthday = 1
max5 = 0
DO
day$ = dayofweek((day MOD 7) + 1)
daynum = daynumbers(day$)
clearspace 3, 21, 44, 35
LOCATE 3, 44: PRINT day$ + " The " + STR$(days) + " of " + STR$(month) + STR$(year)
IF max5 > monthday OR new = 0 THEN REDO5 = 0 ELSE REDO5 = 1
a = nonstandardday(daynum, 0, REDO5, monthday)
IF a = 227 THEN EXIT DO
IF a = 73 AND monthday < 31 THEN
 day = day + 1
 monthday = monthday + 1
 IF monthday > max5 THEN max5 = monthday
 days = days + 1
 IF days > daysinmonth(month) THEN
  days = 1
  month = month + 1
   IF month > 12 THEN
    year = year + 1
    month = 1
   END IF
 END IF
END IF
IF a = 81 AND monthday <> 1 THEN
 day = day - 1
 monthday = monthday - 1
 days = days - 1
 IF days = 0 THEN
  month = month - 1
  days = daysinmonth(month)
  IF month = 0 THEN
   month = 12
   days = daysinmonth(12)
  END IF
 END IF
END IF
IF a = 27 THEN EXIT DO
LOOP
CLS
openingmenu
END SUB

SUB viewday
DIM test(4) AS STRING
part = 1
curspos = 1
CLS
COLOR 11
frame 1, 80, 1, 23
dday = 1
COLOR 7
frame 2, 79, 3, 20
COLOR 13
LOCATE 4, 5: PRINT "Route              Mileage Taxed      Route               Mileage Taxed"
COLOR 7
vline 3, 20, 23
vline 3, 20, 31
vline 3, 20, 37
vline 3, 20, 42
vline 3, 20, 62
vline 3, 20, 70
LOCATE 22, 5: PRINT "Use "; CHR$(17); " "; CHR$(16); " "; CHR$(24); " "; CHR$(25); " PgUp "; "PgDn"
LOCATE 21, 5: PRINT "Only use lower case characters"
COLOR 13
LOCATE 2, 20: PRINT dayofweek$(dday - 1)
a = 72
DO
IF a$ = CHR$(27) THEN EXIT DO
IF a <> 72 AND a <> 75 AND a <> 77 AND a <> 81 AND a <> 80 AND a <> 73 THEN
 COLOR 15, 3
 IF curspos <= 15 THEN loct = 4 ELSE loct = -11
 IF curspos <= 15 THEN loct2 = 5 ELSE loct2 = 44
 IF part = 1 THEN
 LOCATE curspos + loct, loct2: route(dday, curspos) = inpts2(17, a$, route(dday, curspos), 0)
 a = VAL(inpts2$(2, "", "", 1))
 FOR n = 1 TO 100
 IF RTRIM$(route(dday, curspos)) = RTRIM$(stanroute(n)) THEN
   mileage(dday, curspos) = stanmile(n)
   tax(dday, curspos) = stantax(n)
   ok = 1
 END IF
 NEXT n
 END IF
 IF part = 2 THEN LOCATE curspos + loct, loct2 + 19: mileage(dday, curspos) = inpts2(4, a$, mileage(dday, curspos), 0)
 a = VAL(inpts2$(1, "", "", 1))
 IF part = 3 THEN
 tax(dday, curspos) = inpts2$(1, a$, "", 0)
 a = VAL(inpts2$(1, "", "", 1))
 IF a = 77 THEN part = 1: curspos = curspos + 1: IF curspos > 30 THEN curspos = curspos - 30
 a = 0
 COLOR 7, 1
 LOCATE curspos + loct, loct2 + 27: PRINT "  "
END IF
 ok = 0
 COLOR , 1
pink = 1
test(1) = ""
test(2) = ""
test(3) = ""
test(4) = ""
FOR n = 1 TO 29
temproutes$ = route(dday, n)
test(pink) = test(pink) + RTRIM$(temproutes$)
test(pink) = RTRIM$(LTRIM$(test(pink))) + ","
temproutes2$ = remspace$(route(dday, n + 1))
IF LEN(test(pink)) + LEN(temproutes2$) > 50 THEN
IF pink = 4 THEN LOCATE 2, 44: PRINT "No More Room for routes": pink = 5: EXIT FOR
IF pink < 4 THEN pink = pink + 1
END IF
NEXT n
IF pink < 5 THEN LOCATE 2, 44: PRINT SPACE$(24)
'part = part + 1: IF part = 4 THEN part = 1: curspos = curspos + 1: IF curspos = 31 THEN curspos = 1: dday = dday + 1: IF dday = 8 THEN dday = 1
END IF
IF a = 75 THEN
       part = part - 1
       IF part = 0 THEN
           part = 3
           curspos = curspos + 15
           IF curspos > 30 THEN curspos = curspos - 30
       END IF
END IF
IF a = 77 THEN
       part = part + 1
       IF part = 4 THEN
        part = 1
        curspos = curspos + 15
        IF curspos > 30 THEN curspos = curspos - 30
       END IF
END IF
IF a = 227 THEN EXIT DO
IF a = 73 THEN dday = dday - 1: IF dday = 0 THEN dday = 7
IF a = 81 THEN dday = dday + 1: IF dday = 8 THEN dday = 1
IF a = 80 THEN curspos = curspos + 1: IF curspos = 31 THEN curspos = 1
IF a = 72 THEN curspos = curspos - 1: IF curspos = 0 THEN curspos = 30
COLOR 13
LOCATE 2, 20: PRINT dayofweek$(dday - 1)
COLOR 7
FOR n = 1 TO 15
LOCATE n + 4, 5: PRINT route(dday, n)
LOCATE n + 4, 24: PRINT mileage(dday, n)
LOCATE n + 4, 32: PRINT tax(dday, n)
NEXT n
FOR p = 1 TO 15
LOCATE p + 4, 44: PRINT route(dday, p + 15)
LOCATE p + 4, 63: PRINT mileage(dday, p + 15)
LOCATE p + 4, 71: PRINT tax(dday, p + 15)
NEXT p
COLOR 15, 3
IF curspos <= 15 THEN
IF part = 1 THEN LOCATE curspos + 4, 5: PRINT route(dday, curspos)
IF part = 2 THEN LOCATE curspos + 4, 24: PRINT mileage(dday, curspos)
IF part = 3 THEN LOCATE curspos + 4, 32: PRINT tax(dday, curspos)
END IF
IF curspos > 15 THEN
IF part = 1 THEN LOCATE curspos - 11, 44: PRINT route(dday, curspos)
IF part = 2 THEN LOCATE curspos - 11, 63: PRINT mileage(dday, curspos)
IF part = 3 THEN LOCATE curspos - 11, 71: PRINT tax(dday, curspos)
END IF
COLOR 7, 1
DO
a$ = INKEY$
LOOP WHILE a$ = ""
a = ASC(RIGHT$(a$, 1))
a$ = LCASE$(a$)
LOOP
openingmenu
END SUB

SUB viewdist
part = 1
curspos = 1
CLS
COLOR 11
frame 1, 80, 1, 23
COLOR 7
bit = 1
frame 2, 79, 3, 20
COLOR 13
LOCATE 4, 5: PRINT "Route              Mileage Taxed      Route               Mileage Taxed"
COLOR 7
vline 3, 20, 23
vline 3, 20, 31
vline 3, 20, 37
vline 3, 20, 42
vline 3, 20, 62
vline 3, 20, 70
LOCATE 22, 5: PRINT "Use "; CHR$(17); " "; CHR$(16); " "; CHR$(24); " "; CHR$(25); " PgUp "; "PgDn"
LOCATE 21, 5: PRINT "Only use lower case characters"
DO
COLOR 7, 1
LOCATE 2, 10: PRINT "Page "; (bit + 1) / 2; "of 5"
FOR n = 1 TO 10
LOCATE n + 4, 5: PRINT stanroute(n + 20 * (bit - 1) / 2)
LOCATE n + 4, 24: PRINT stanmile(n + 20 * (bit - 1) / 2)
LOCATE n + 4, 32: PRINT stantax(n + 20 * (bit - 1) / 2)
LOCATE n + 4, 44: PRINT stanroute(n + 20 * (bit - 1) / 2 + 10)
LOCATE n + 4, 63: PRINT stanmile(n + 20 * (bit - 1) / 2 + 10)
LOCATE n + 4, 72: PRINT stantax(n + 20 * (bit - 1) / 2 + 10)
NEXT n
COLOR 15, 3
IF curspos <= 10 THEN
IF part = 1 THEN LOCATE curspos + 4, 5: PRINT stanroute(curspos + 20 * (bit - 1) / 2)
IF part = 2 THEN LOCATE curspos + 4, 24: PRINT stanmile(curspos + 20 * (bit - 1) / 2)
IF part = 3 THEN LOCATE curspos + 4, 32: PRINT stantax(curspos + 20 * (bit - 1) / 2)
END IF
IF curspos > 10 THEN
IF part = 1 THEN LOCATE curspos - 6, 44: PRINT stanroute(curspos + 20 * (bit - 1) / 2)
IF part = 2 THEN LOCATE curspos - 6, 63: PRINT stanmile(curspos + 20 * (bit - 1) / 2)
IF part = 3 THEN LOCATE curspos - 6, 72: PRINT stantax(curspos + 20 * (bit - 1) / 2)
END IF
DO
a$ = INKEY$
LOOP WHILE a$ = ""
a = ASC(RIGHT$(a$, 1))
a$ = LCASE$(a$)
IF a$ = CHR$(27) THEN EXIT DO
IF a <> 72 AND a <> 73 AND a <> 75 AND a <> 77 AND a <> 80 AND a <> 81 THEN
 IF curspos <= 10 THEN
 loct3 = 4
 loct2 = 5
 ELSE
 loct3 = -6
 loct2 = 44
 END IF
  IF part = 1 THEN
  LOCATE curspos + loct3, loct2: stanroute(curspos + 20 * (bit - 1) / 2) = inpts2$(17, a$, stanroute(curspos + 20 * (bit - 1) / 2), 0)
  a = VAL(inpts2$(1, "", "", 1))
  END IF
  IF part = 2 THEN
  LOCATE curspos + loct3, loct2 + 19: stanmile(curspos + 20 * (bit - 1) / 2) = inpts2$(4, a$, stanmile(curspos + 20 * (bit - 1) / 2), 0)
  a = VAL(inpts2$(1, "", "", 1))
  END IF
  IF part = 3 THEN
  stantax(curspos + 20 * (bit - 1) / 2) = inpts2$(1, a$, stantax(curspos + 20 * (bit - 1) / 2), 0)
  a = VAL(inpts2$(1, "", "", 1))
  IF a = 77 THEN a = 0: part = 1: curspos = curspos + 1: IF curspos > 20 THEN curspos = 1
  END IF
  COLOR 7, 1
  LOCATE curspos + loct3, loct2 + 27: PRINT "    "
  'part = part + 1: IF part = 4 THEN part = 1: curspos = curspos + 1: IF curspos = 21 THEN curspos = 1: bit = bit + 2: IF bit = 11 THEN bit = 1
  IF curspos + 20 * (bit - 1) / 2 > ENDOFFILE(8) THEN ENDOFFILE(8) = curspos + 20 * (bit - 1) / 2
END IF
IF a = 75 THEN
       part = part - 1
       IF part = 0 THEN
           part = 3
           curspos = curspos + 10
           IF curspos > 20 THEN curspos = curspos - 20
       END IF
END IF
IF a = 77 THEN
       part = part + 1
       IF part = 4 THEN
        part = 1
        curspos = curspos + 10
        IF curspos > 20 THEN curspos = curspos - 20
       END IF
END IF
IF a = 73 THEN bit = bit + 2: IF bit = 11 THEN bit = 1
IF a = 81 THEN bit = bit - 2: IF bit = -1 THEN bit = 9
IF a = 80 THEN curspos = curspos + 1: IF curspos > 20 THEN curspos = 1
IF a = 72 THEN curspos = curspos - 1: IF curspos < 1 THEN curspos = 20
IF a$ = CHR$(27) THEN EXIT DO
IF a = 227 THEN EXIT DO
LOOP
openingmenu
END SUB

SUB vline (top, bottom, left)
LOCATE top, left: PRINT CHR$(203)
LOCATE bottom, left: PRINT CHR$(202)
FOR n = top + 1 TO bottom - 1
LOCATE n, left: PRINT CHR$(186)
NEXT n
END SUB

SUB wp
ON ERROR GOTO noprint:
CLS
COLOR 11
frame 1, 80, 1, 23
OPEN "lpt1:bin" FOR OUTPUT AS #1
COLOR 13
frame 2, 79, 20, 22
COLOR 11
frame 25, 60, 4, 15
LOCATE 5, 35
COLOR 15
PRINT "Adding Notes"
LOCATE 7, 29
PRINT " Enter a line of text after"
LOCATE 8, 29
PRINT " The ? prompt. Anything you"
LOCATE 9, 29
PRINT " Write will be  printed  as"
LOCATE 10, 29
PRINT " You press return"
LOCATE 11, 29
PRINT " Type 'e' to Exit"
DO
LOCATE 21, 3
PRINT STRING$(76, 32)
LOCATE 21, 3
oneline$ = inpts(75)
IF UCASE$(oneline$) = "E" THEN EXIT DO
PRINT #1, "    " + oneline$
LOOP
CLOSE #1
FOR n = 1 TO 3
LOCATE n + 19, 2
PRINT STRING$(78, 32)
NEXT n
ON ERROR GOTO 0
openingmenu
END SUB

