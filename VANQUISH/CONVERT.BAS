DIM SHARED picu(345) AS LONG
'IF NOT INSTR(COMMAND$, ".") THEN END
SCREEN 12
OPEN COMMAND$ FOR INPUT AS #1
INPUT #1, a, b, c, d, e
FOR n = 0 TO 345
INPUT #1, picu(n)
NEXT n
PUT (10, 10), picu, PSET
temp$ = LEFT$(COMMAND$, LEN(COMMAND$) - 4)
temp$ = temp$ + ".bsv"
DEF SEG = VARSEG(picu(0))
BSAVE temp$, VARPTR(picu(0)), 1380





