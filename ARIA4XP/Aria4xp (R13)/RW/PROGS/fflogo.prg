 PRIVATE lcwasblink, ff_incolor, g_lmrow, g_lmcol, i, show_color, grafchar, tag_color
 lcwasblink = SET("BLINK")
 SET BLINK OFF
 ff_incolor = ISCOLOR()
 g_lmrow = MROW("")
 g_lmcol = MCOL("")
 logo_bkgnd = IIF(ff_incolor, "n/w, n/w, n/w, n/w, n/w, n/w, n/w, n/w, n/w, n/w ", "w/n, w/n, w/n, w/n, w/n, w/n, w/n, w/n, w/n, w/n")
 IF  .NOT. WEXIST("foxlogo")
    DEFINE WINDOW foxlogo FROM 2, 0 TO 20, 78 NOFLOAT NOCLOSE SHADOW COLOR (logo_bkgnd)
 ENDIF
 ACTIVATE WINDOW SAME foxlogo
 CLEAR
 FOR i = 1 TO 6
    grafchar = CHR(223)
    show_color = IIF(ff_incolor, "R/W*", "N/W")
    DO show_fire
    grafchar = CHR(30)
    show_color = IIF(ff_incolor, "R+/W", "W+/N")
    DO show_fire
    grafchar = CHR(223)
    show_color = IIF(ff_incolor, "W+/R*", "W/N")
    DO show_fire
    grafchar = CHR(220)
    show_color = IIF(ff_incolor, "R/W*", "N/W")
    DO show_fire
    grafchar = CHR(30)
    show_color = IIF(ff_incolor, "R+/W", "W+/N")
    DO show_fire
    grafchar = CHR(220)
    show_color = IIF(ff_incolor, "W+/R*", "W/N")
    DO show_fire
    IF ( .NOT. (BETWEEN(MROW(""), g_lmrow-3, g_lmrow+3) .AND. BETWEEN(MCOL(""), g_lmcol-3, g_lmcol+3))) .OR. INKEY()<>0
       EXIT
    ENDIF
 ENDFOR
 IF SET("CURSOR")="ON"
    HIDE WINDOW foxlogo
    SET CURSOR OFF
    ACTIVATE WINDOW NOSHOW foxlogo
    SET CURSOR ON
 ENDIF
 RELEASE WINDOW foxlogo
 SET BLINK &lcwasblink
 RETURN
ENDPROC
**
PROCEDURE SHOW_FIRE
 @ 13, 1 TO 13, 4 grafchar COLOR (show_color)
 @ 13, 12 TO 13, 17 grafchar COLOR (show_color)
 @ 13, 21 TO 13, 23 grafchar COLOR (show_color)
 @ 13, 29 TO 13, 31 grafchar COLOR (show_color)
 @ 13, 34 TO 13, 36 grafchar COLOR (show_color)
 @ 13, 43 TO 13, 46 grafchar COLOR (show_color)
 @ 13, 49 TO 13, 53 grafchar COLOR (show_color)
 @ 13, 62 TO 13, 67 grafchar COLOR (show_color)
 @ 12, 2 TO 12, 3 grafchar COLOR (show_color)
 @ 12, 11 TO 12, 12 grafchar COLOR (show_color)
 @ 12, 17 TO 12, 18 grafchar COLOR (show_color)
 @ 12, 28 TO 12, 29 grafchar COLOR (show_color)
 @ 12, 23 TO 12, 24 grafchar COLOR (show_color)
 @ 12, 34 TO 12, 35 grafchar COLOR (show_color)
 @ 12, 44 TO 12, 45 grafchar COLOR (show_color)
 @ 12, 50 TO 12, 51 grafchar COLOR (show_color)
 @ 12, 61 TO 12, 62 grafchar COLOR (show_color)
 @ 12, 67 TO 12, 68 grafchar COLOR (show_color)
 @ 11, 2 TO 11, 3 grafchar COLOR (show_color)
 @ 11, 34 TO 11, 35 grafchar COLOR (show_color)
 @ 11, 2 TO 11, 3 grafchar COLOR (show_color)
 @ 11, 10 TO 11, 11 grafchar COLOR (show_color)
 @ 11, 18 TO 11, 19 grafchar COLOR (show_color)
 @ 11, 27 TO 11, 28 grafchar COLOR (show_color)
 @ 11, 24 TO 11, 25 grafchar COLOR (show_color)
 @ 11, 34 TO 11, 35 grafchar COLOR (show_color)
 @ 11, 44 TO 11, 45 grafchar COLOR (show_color)
 @ 11, 50 TO 11, 51 grafchar COLOR (show_color)
 @ 11, 60 TO 11, 61 grafchar COLOR (show_color)
 @ 10, 25 TO 10, 27 grafchar COLOR (show_color)
 @ 10, 2 TO 10, 3 grafchar COLOR (show_color)
 @ 10, 10 TO 10, 11 grafchar COLOR (show_color)
 @ 10, 18 TO 10, 19 grafchar COLOR (show_color)
 @ 10, 34 TO 10, 35 grafchar COLOR (show_color)
 @ 10, 44 TO 10, 45 grafchar COLOR (show_color)
 @ 10, 50 TO 10, 51 grafchar COLOR (show_color)
 @ 10, 60 TO 10, 61 grafchar COLOR (show_color)
 @ 9, 60 TO 9, 61 grafchar COLOR (show_color)
 @ 9, 2 TO 9, 3 grafchar COLOR (show_color)
 @ 9, 10 TO 9, 11 grafchar COLOR (show_color)
 @ 9, 18 TO 9, 19 grafchar COLOR (show_color)
 @ 9, 24 TO 9, 25 grafchar COLOR (show_color)
 @ 9, 27 TO 9, 28 grafchar COLOR (show_color)
 @ 9, 34 TO 9, 35 grafchar COLOR (show_color)
 @ 9, 44 TO 9, 45 grafchar COLOR (show_color)
 @ 9, 50 TO 9, 51 grafchar COLOR (show_color)
 @ 9, 60 TO 9, 69 grafchar COLOR (show_color)
 @ 8, 2 TO 8, 3 grafchar COLOR (show_color)
 @ 8, 11 TO 8, 12 grafchar COLOR (show_color)
 @ 8, 17 TO 8, 18 grafchar COLOR (show_color)
 @ 8, 23 TO 8, 24 grafchar COLOR (show_color)
 @ 8, 28 TO 8, 29 grafchar COLOR (show_color)
 @ 8, 34 TO 8, 35 grafchar COLOR (show_color)
 @ 8, 44 TO 8, 45 grafchar COLOR (show_color)
 @ 8, 50 TO 8, 51 grafchar COLOR (show_color)
 @ 8, 57 TO 8, 58 grafchar COLOR (show_color)
 @ 8, 61 TO 8, 62 grafchar COLOR (show_color)
 @ 8, 67 TO 8, 68 grafchar COLOR (show_color)
 @ 7, 2 TO 7, 9 grafchar COLOR (show_color)
 @ 7, 12 TO 7, 17 grafchar COLOR (show_color)
 @ 7, 21 TO 7, 23 grafchar COLOR (show_color)
 @ 7, 29 TO 7, 31 grafchar COLOR (show_color)
 @ 7, 34 TO 7, 40 grafchar COLOR (show_color)
 @ 7, 43 TO 7, 45 grafchar COLOR (show_color)
 @ 7, 48 TO 7, 49 grafchar COLOR (show_color)
 @ 7, 52 TO 7, 57 grafchar COLOR (show_color)
 @ 7, 62 TO 7, 67 grafchar COLOR (show_color)
 @ 6, 2 TO 6, 3 grafchar COLOR (show_color)
 @ 6, 34 TO 6, 35 grafchar COLOR (show_color)
 @ 5, 2 TO 5, 3 grafchar COLOR (show_color)
 @ 5, 34 TO 5, 35 grafchar COLOR (show_color)
 @ 5, 44 TO 5, 45 "Ü" COLOR (show_color)
 @ 4, 2 TO 4, 3 grafchar COLOR (show_color)
 @ 4, 34 TO 4, 35 grafchar COLOR (show_color)
 @ 3, 2 TO 3, 3 grafchar COLOR (show_color)
 @ 3, 13 TO 3, 14 grafchar COLOR (show_color)
 @ 3, 34 TO 3, 36 grafchar COLOR (show_color)
 @ 3, 40 TO 3, 41 grafchar COLOR (show_color)
 @ 2, 1 TO 2, 14 grafchar COLOR (show_color)
 @ 2, 36 TO 2, 40 grafchar COLOR (show_color)
 @ 9, 73 TO 9, 73 grafchar COLOR (show_color)
 @ 6, 72 TO 6, 74 grafchar COLOR (show_color)
 @ 5, 72 TO 5, 74 grafchar COLOR (show_color)
 @ 4, 72 TO 4, 74 grafchar COLOR (show_color)
 @ 3, 72 TO 3, 74 grafchar COLOR (show_color)
 @ 1, 73 TO 1, 73 grafchar COLOR (show_color)
 @ 2, 72 TO 2, 74 grafchar COLOR (show_color)
 @ 8, 73 TO 8, 73 grafchar COLOR (show_color)
 @ 7, 73 TO 7, 73 grafchar COLOR (show_color)
 @ 13, 74 SAY "" COLOR (show_color)
 @ 13, 72 SAY "" COLOR (show_color)
 @ 13, 73 SAY "Û" COLOR (show_color)
 @ 10, 73 TO 10, 73 grafchar COLOR (show_color)
 @ 11, 73 TO 11, 73 grafchar COLOR (show_color)
 tag_color = IIF("R+"$show_color, "R+/W", "R/W")
 PRIVATE lcslogan, lccopyright, lnlenslog, lnlencopyr
 lcslogan = "Query Tool and Report Writer for FoxPro 2.6"
 lnlenslog = LEN(lcslogan)
 lccopyright = "Copyright (c) 1992-96 by Micromega Systems, Inc."
 lnlencopyr = LEN(lccopyright)
 @ 15, (76-lnlenslog)/2 SAY m.lcslogan COLOR (tag_color)
 @ 16, (76-lnlencopyr)/2 SAY m.lccopyright
 RETURN
ENDPROC
**
