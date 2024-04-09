*:******************************************************************************
*: Program file  : ICSTYWIP.PRG
*: Program desc. : PRINT ADJUSTMENTS REPORT
*: System        : Aria Apparel System (A27)
*: Module        : Inventory Control   (IC )
*: Developer     : ABDOU ELGENDI     - (ABD) 
*: Date          : 12/15/1999
*: NOTE          : FOR PEOPLE WHO DON'T HAVE MANUFACTURING AND STYLES IMPORTING.
*:******************************************************************************
*: Calls : 
*:    FUNCTIONS  : lfwRepWhen , lfItmPos , lfvInstMod
*:    --------------------------------------------------------------------------
*:    PROCEDURES : 
*:******************************************************************************
*: Passed Parameters : None
*:******************************************************************************
*: This Report Program is due to B802874 ...
*: WIP INVENTORY ADJUSTMENTS JOURNAL ONLY FOR SYSTEM THAT DO NOT HAVE IMPORTING.
*:******************************************************************************
*:Modifications  :
*:******************************************************************************
*
*
*-- Variable declarations [ Begin]

STORE .F. TO llCostPriv , XAVG_COST
STORE ''  TO lcStyHdr

*-- Variable declarations [ End ]

*-- Get the Cost informatiom For user privilege.
llCostPriv = gfUserPriv('IC','ICSTYLE','COSTING')

lcStyHdr   = ALLTRIM(gfItemMask("HI"))
*-- To hold the mutiple Warehouse facility status.
XAVG_COST  = IIF(gfGetMemVar('M_COST_METH',gcAct_Comp) = 'A',.T.,.F.)

FOR lnInd   = 1 TO ALEN(laOgFxFlt,1)
  IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'WIPADJ.DATE'
    IF EMPTY(CTOD(SUBSTR(laOgFxFlt[lnInd,6],1,10))) .OR. EMPTY(CTOD(SUBSTR(laOgFxFlt[lnInd,6],12,10)))
      *---Text : 'Report Date Cannot be empty..!!'
      = gfModalGen('TRM04074B00000','DIALOG','Report date')
      RETURN
    ELSE
      ldLDate = SUBSTR(laOgFxFlt[lnInd,6],1,10)
      ldHDate = SUBSTR(laOgFxFlt[lnInd,6],12,10)
      EXIT
    ENDIF
  ENDIF
ENDFOR

XTITLE   = 'FROM: '+(ldLDate)+' THRU:'+(ldHDate)
SELECT WIPADJ  
GO TOP
SET RELATION TO STYLE INTO STYLE  

SET FILTER TO
LOCATE ALL FOR &lcRpExp

IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
SET FILTER TO &lcRpExp
R_TITLE = 'WIP ADJUSTMENTS JOURNAL'
R_WIDTH = 'W'
XREPORT = 'ICSTYWIP'
WAIT WINDOW 'Printing ... <Space Bar> To Abort' NOWAIT
SET DEVICE TO PRINT

ROW      = 99
XSTYLE   = STYLE
STORE 0 To XSTOTPCS , XSTOTAMT , XGTOTPCS , XGTOTAMT , PAGENO
SCAN FOR &lcRpExp WHILE INKEY() # 32
  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH  XREPORT,XTITLE,R_WIDTH
    ROW = 5
    IF XAVG_COST
      @ ROW,01 SAY '&lcStyHdr  Reference                 Division   SZ1   SZ2   SZ3   SZ4   SZ5   SZ6   SZ7   SZ8  Totqty  Avg. Cost     Date'
    ELSE
      @ ROW,01 SAY '&lcStyHdr  Reference                 Division   SZ1   SZ2   SZ3   SZ4   SZ5   SZ6   SZ7   SZ8  Totqty  Tot. Cost     Date'
    ENDIF
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('*',132)
    ROW = ROW + 1
  ENDIF
  IF STYLE <> XSTYLE 
    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    @ ROW,001 SAY '*** SUBTOTAL ***'+XSTYLE
    @ ROW,104 SAY XSTOTPCS PICTURE '9999999'
    IF llCostPriv              && COSTING ACCESS.
      @ ROW,112 SAY XSTOTAMT PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    XSTYLE   = STYLE
    XSTOTPCS  = 0
    XSTOTAMT  = 0
  ENDIF

  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
  *STYLE           CLR REFERENCE                 DV   SZ1   SZ2   SZ3   SZ4   SZ5   SZ6   SZ7   SZ8  TOTQTY  TOT. COST  DATE
  *123456789012345 123 1234567890123456789012345 12 12345 12345 12345 12345 12345 12345 12345 12345  123456 1234567.99  99/99/99

  IF llRpPrtDet
    @ ROW,001  SAY STYLE
    @ ROW,021 SAY CREASON
    @ ROW,047 SAY STYLE.CDIVISION
    @ ROW,056 SAY ADJ1 PICTURE '@Z 99999'
    @ ROW,062 SAY ADJ2 PICTURE '@Z 99999'
    @ ROW,068 SAY ADJ3 PICTURE '@Z 99999'
    @ ROW,074 SAY ADJ4 PICTURE '@Z 99999'
    @ ROW,080 SAY ADJ5 PICTURE '@Z 99999'
    @ ROW,086 SAY ADJ6 PICTURE '@Z 99999'
    @ ROW,092 SAY ADJ7 PICTURE '@Z 99999'
    @ ROW,098 SAY ADJ8 PICTURE '@Z 99999'
    @ ROW,105 SAY TOTADJ PICTURE '@Z 999999'

    IF llCostPriv              && COSTING ACCESS.
      @ ROW,112 SAY TOTADJ * IIF(XAVG_COST , STYLE.AVE_COST, STYLE.TOTCOST) ;
                    PICTURE '9999999.99'
    ENDIF
    @ ROW,123 SAY DATE
    ROW = ROW + 1
  ENDIF

  *-- UPDATE THE SUBTOTALS
  XSTOTPCS  = XSTOTPCS + TOTADJ
  XSTOTAMT  = XSTOTAMT + (TOTADJ * IIF(XAVG_COST , STYLE.AVE_COST , STYLE.TOTCOST))
  XGTOTPCS  = XGTOTPCS + TOTADJ
  XGTOTAMT  = XGTOTAMT + (TOTADJ * IIF(XAVG_COST , STYLE.AVE_COST,STYLE.TOTCOST))
ENDSCAN

    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    @ ROW,001 SAY ' *** SUBTOTAL *** '+XSTYLE
    @ ROW,104 SAY XSTOTPCS PICTURE '9999999'
    IF llCostPriv              && COSTING ACCESS.
      @ ROW,112 SAY XSTOTAMT PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('.',132)

  IF EOF() .OR. INKEY() = 32
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    @ ROW,001 SAY '*** GRAND TOTAL ***'
    @ ROW,104 SAY XGTOTPCS PICTURE '9999999'

    IF llCostPriv              && COSTING ACCESS.
      @ ROW,112 SAY XGTOTAMT PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
  ENDIF

DO ENDREPORT 
*-- End OF Printing Report.
*!******************************************************************************
*-- Functions and Procedures :
*!**************************************************************************
*! Name        : lfwRepWhen()
*! Developer   : ABDOU ELGENDI - (ABD)
*! Date        : 06/23/1999
*! Purpose     : To Set When running the Option Grid.
*!**************************************************************************
*! Called from : "When" Option Grid.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen

*-- If You Save You Filter Before.

IF lnOGSeting = 1
  lnDatWIP = lfItmPos('WIPADJ.DATE')
  laOGFxFlt[lnDatWIP,6] = DTOC(gdSysDate)+"|"+DTOC(gdSysDate)
ENDIF

*-- End Of lfwRepWhen.
*!***************************************************************************
*! Name        : lfItmPos
*! Developer   : ABDOU ELGENDI - (ABD)
*! Date        : 12/15/1999
*! Purpose     : to get the position of the fixed filter in OG.
*!***************************************************************************
*! Called from : OG When Function 
*!***************************************************************************
*! Parameters  : None.
*!***************************************************************************
*! Returns     : None.
*!***************************************************************************
*! Example     : = lfItmPos()
*!***************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
*!******************************************************************************
*! Name        : lfvInstMod
*! Developer   : ABDOU ELGENDI - (ABD)
*! Date        : 12/15/1999
*! Purpose     : TO VALID RUNNING This Report FOR PEOPLE WHO DON'T HAVE 
*!             : MANUFACTURING AND STYLES IMPORTING ONLY.
*!******************************************************************************
*! Called from : OG When Function 
*!******************************************************************************
*! Parameters: None.
*!******************************************************************************
*! Returns   : None.
*!******************************************************************************
*! Example   : = lfvInstMod()
*!******************************************************************************
*
FUNCTION lfvInstMod
*-- llogtrmnat:- Variable Terminate The Option Grid.

*-- WIP INVENTORY ADJUSTMENTS JOURNAL ONLY FOR SYSTEM THAT DO NOT HAVE IMPORTING.
IF 'MF' $ gcCmpModules .AND. 'PO' $ gcCmpModules
  *---Text : - 'You have Manufacturing and Style Purchase Orders Modules'
  =gfModalGen('TRM42080B00000','DIALOG','You have Manufacturing and Style Purchase Orders Modules')
  llOgTrmnat = .T.
  CLEAR READ
  RETURN
ENDIF

*-- End of lfvInstMod.
*!******************************************************************************
