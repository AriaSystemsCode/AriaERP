 PARAMETER lcwhattodo
 PRIVATE opendbfs
 DIMENSION opendbfs[8, 2]
 opendbfs[1, 1] = "SAMPLE\CARS"
 opendbfs[1, 2] = "CARS"
 opendbfs[2, 1] = "SAMPLE\EXPENSES"
 opendbfs[2, 2] = "EXPENSES"
 opendbfs[3, 1] = "SAMPLE\DEALERS"
 opendbfs[3, 2] = "PURCH_FROM"
 opendbfs[4, 1] = "SAMPLE\SALES"
 opendbfs[4, 2] = "SALES"
 opendbfs[5, 1] = "SAMPLE\DEALERS"
 opendbfs[5, 2] = "SOLD_TO"
 opendbfs[6, 1] = "SAMPLE\EXPTYPES"
 opendbfs[6, 2] = "EXPTYPES"
 opendbfs[7, 1] = "SAMPLE\CARMAKE"
 opendbfs[7, 2] = "CARMAKE"
 opendbfs[8, 1] = "SAMPLE\CARCOLOR"
 opendbfs[8, 2] = "CARCOLOR"
 IF m.lcwhattodo=='START'
    DO fnopenfiles
 ELSE
    DO fnclosefiles
 ENDIF
 RETURN
ENDPROC
**
FUNCTION fnOpenFiles
 PRIVATE tcdbf
 PRIVATE lconerror
 lconerror = ON("ERROR")
 ON ERROR *
 FOR tcdbf = 1 TO ALEN(opendbfs, 1)
    IF  .NOT. USED(opendbfs(tcdbf, 2))
       SELECT 0
       USE SHARED (opendbfs(tcdbf, 1)) AGAIN ALIAS (opendbfs(tcdbf, 2)) ORDER 1
    ENDIF
 ENDFOR
 ON ERROR &lconerror
 RETURN .T.
ENDFUNC
**
FUNCTION fnCloseFiles
 PRIVATE tcdbf
 FOR tcdbf = 1 TO ALEN(opendbfs, 1)
    IF USED(opendbfs(tcdbf, 2))
       USE IN (opendbfs(tcdbf, 2))
    ENDIF
 ENDFOR
 RETURN .T.
ENDFUNC
**
