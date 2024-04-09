*:***************************************************************************
*: Program file  : PO2002
*: Program desc. : Create Production Plan for year 2002
*: For screen    : PO2002.SCX
*:        System : Aria Advantage Series.
*:        Module : Style Purchased Order (PO)
*:        Date   : 07/13/2003
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO POYEAR
*:***************************************************************************
*: Due to C200569
*:***************************************************************************
*

lcOkBmp  = gcBmpHome + "OK.BMP"
lcCanBmp = gcBmpHome + "CAN.BMP"
STORE '0%' TO lcD1ThPrc

*-- To get the Color position and length.
STORE 0 TO lnClrPos,lnClrLen
=lfGetColor()

IF !gfSetup()
  RETURN
ENDIF  

lcEscKey = ON('KEY','ESC')
ON KEY LABEL ESC CLEAR READ
DO (gcScrDir+gcWinAppl+'\PO2002.SPX')
ON KEY LABEL ESC &lcEscKey.

glQuitting = .T.  && Rise quit flag because it's modal screen.
*-- end of program code.

*:*************************************************************
*: Name      : lfThrmo
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/13/2003
*: Purpose   : Thermometers.
*:*************************************************************
FUNCTION lfThrmo
PARAMETERS lnCurrRc,lnMaxum

lcD1Thrm  = REPLICATE(CHR(219),CEILING((lnCurrRc*34)/lnMaxum))
lcD1ThPrc = ALLTRIM(STR((lnCurrRc/lnMaxum)*100,3))+'%'
SHOW GET lcD1Thrm
SHOW GET lcD1ThPrc
*-- end of lfThrmo.

*:*************************************************************
*: Name      : lfvOk
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/13/2003
*: Purpose   : Valid function of the OK button
*:*************************************************************
FUNCTION lfvOk

IF SEEK('2002','YEARINPO')
  SELECT YEARINPO
  DELETE REST WHILE STR(NYEAR,4,0) = '2002'
ENDIF

PRIVATE lnMax,lnCnt,lcColor,lnI,lcI,lcMonth,lnQty,lnJ,lcJ
STORE 0 TO lnCnt,lnMax

IF SEEK('2002','FORCAST')
  SELECT FORCAST
  COUNT REST WHILE STR(NYEAR,4)='2002' FOR NWEEK <13 TO lnMax
  
  =SEEK('2002','FORCAST')
  SCAN REST WHILE STR(NYEAR,4)='2002' FOR NWEEK <13
    lnCnt = lnCnt + 1
    =lfThrmo(lnCnt,lnMax)
    IF SEEK(STYLE,'STYLE') .AND. SEEK('S'+STYLE.SCALE,'SCALE')
      lcColor = SUBSTR(STYLE,lnClrPos,lnClrLen)
      SELECT YEARINPO
      FOR lnI = 1 TO SCALE.CNT
        lcI = STR(lnI,1)
        lnQty = EVALUATE('FORCAST.NFORQTY'+lcI)
        IF lnQty > 0
          =lfAddLine()
        ENDIF
      ENDFOR
    ENDIF
  ENDSCAN
ENDIF

IF lnMax > 0
  =gfModalGen("INM000000B00000","DIALOG",'','','Create production plan for year 2002 completed successfully.')
ELSE
  =gfModalGen("INM000000B00000","DIALOG",'','','There are no records for year 2002.')
ENDIF
CLEAR READ
*-- end of lfvOk.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 07/13/2003
*! Purpose   : Get the color information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetColor()
*!*************************************************************
*
FUNCTION lfGetColor

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnClrPos = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnClrLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR
*-- end of lfGetColor.

*!*************************************************************
*! Name      : lfAddLine
*! Developer : Ahmed Maher
*! Date      : 07/13/2003
*! Purpose   : Add line to YEARINPO file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfAddLine()
*!*************************************************************
*
FUNCTION lfAddLine

IF !SEEK('2002'+STYLE.CSTYMAJOR+lcColor+STYLE.SCALE+lcI)
  APPEND BLANK
  REPLACE NYEAR     WITH 2002,;
          STYLE     WITH STYLE.CSTYMAJOR,;
          COLOR     WITH lcColor,;
          CSIZECODE WITH STYLE.SCALE+lcI
ENDIF

lcMonth = STRTRAN(STR(FORCAST.NWEEK,2,0),' ','0')
REPLACE ('NSHPMNTH'+lcMonth) WITH EVALUATE('NSHPMNTH'+lcMonth) + lnQty

FOR lnJ = VAL(lcMonth) TO 12
  lcJ = STRTRAN(STR(lnJ,2,0),' ','0')
  REPLACE ('NCUMSHIP'+lcJ) WITH EVALUATE('NCUMSHIP'+lcJ) + lnQty
ENDFOR

REPLACE NTOTSHIP WITH NTOTSHIP + lnQty
*--end of lfAddLine.