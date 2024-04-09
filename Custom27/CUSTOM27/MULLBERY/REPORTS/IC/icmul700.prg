****************************************************************************
*: Program file      : ICMUL700.PRG  (C101623)
*: Program desc.     : INVENTORY WORKSHEET REPORT 
*: System            : Aria Apparel System (A27).
*: Module            : Inventory Control (IC)
*: Developer         : ABDOU ELGENDI - (ABD)
*: Date              : 08/23/1999
*:**************************************************************************
*: Calls : FUNCTIONS : lfStyWarH  , lfPriInven , lfVStyWar , lfvStyle  ,
*:                   : lfWoldVal  , lfMajGet   , lfvFabric , lfVldWare ,
*:                   : lfEvalSegs , lfCollTime , lfClearRep,
*:                   : ...............   
*:         PROCEDURE : lpRpt_Hdr
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*:Modifications      :
*:**************************************************************************
*

 *--  Variable Declaration.
 lcStyPic = gfItemMask('HI')
 XTITLE  = SPACE(30)
 STORE '' TO XFILTER , XSTATUS , XFABRIC , XMAKE
 lcInventry = "N"
 XRPT_CD = 'M'
 XREPORT = 'ICMUL700'
 R_TITLE = 'INVENTORY WORKSHEET'
 R_WIDTH = 'W'
 llQuit  = .F.
 lcStTime  = TIME()
 *-- END OF variable Declaration.

*--- If user change filter criteria then you must collect data again [Begin]

IF llOGFltCh

 *-- TTILE 
 XTITLE = lcRpOpTlt 
 *-- FABRIC
 XFABRIC = lcRpFabr
 *-- STYLE MAKE/BUY
  DO CASE
    CASE lcRpMake='M'
     XMAKE  =  'M'
    CASE lcRpMake='U'
     XMAKE  =  'U'
  ENDCASE 
 
 *-- llRpPtInv : Print Existing Inventory 
  IF !llRpPtInv
     lcInventry = "N" 
  ELSE
     lcInventry = "Y"
  ENDIF 

 *-- STATUS Active/cancel/Both
  DO CASE
    CASE lcRpTyp='A'
       XSTATUS=  'A'
    CASE lcRpTyp='X'
       XSTATUS  ='X'
    CASE lcRpTyp='H'
       XSTATUS  ='H'
  ENDCASE
 
 SELECT STYLE
 GO TOP
 XFILTER = lcRpExp
 IF !EMPTY(XFABRIC)
   XFILTER = XFILTER +'.AND. FABRIC=XFABRIC'
 ENDIF
 IF !EMPTY(XSTATUS)
   XFILTER = XFILTER +'.AND. STATUS=XSTATUS'
 ENDIF
 IF XMAKE $ 'MU'
   XFILTER = XFILTER + ".AND.MAKE=IIF(XMAKE='M',.T.,.F.)"
 ENDIF
 SELECT STYLE
 GO TOP
 LOCATE FOR &XFILTER
 WAIT WINDOW 'Selecting records for report ...' NOWAIT
 *-- lcWorkTmp
 USE IN IIF(USED(lcWorkTmp),lcWorkTmp,0)
 COPY REST TO &gcWorkDir.&lcWorkTmp FOR &XFILTER
 =gfOpenFile(gcWorkDir+lcWorkTmp,'','EX')
 SELECT (lcWorkTmp)
 GOTO TOP
 *-- To hold the mutiple Warehouse facility status.
 XAVG_COST  = IIF(gfGetMemVar('M_COST_METH',gcAct_Comp) = 'A',.T.,.F.)
ENDIF   &&-- END IF FOR THE FLAG llOGFltCh
SELECT (lcWorkTmp)
GOTO TOP
IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcEdTime = TIME()  && Time in which we finish collect data.
WAIT WINDOW 'Selected Records in ' + ALLTRIM(STR(lfCollTime(lcStTime,lcEdTime),6,2)) + ' Seconds...' TIMEOUT 1

IF llMultWH .AND. !EMPTY(lcRpWareCd)
  IF !lfVStyWar()
    RETURN
  ENDIF
ENDIF

SELECT (lcWorkTmp)
GOTO TOP
ROW        = 99
PAGENO     = 00
LINENO     = 00
MAX_LINES  = 58
LMARGIN    = 00

************************************
* M INVENTORY WORKSHEET
************************************
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
*STYLE        COLOR  COLOR DESC.....  STYLE DESCRIPTION...    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8 TOTQTY
*                                                             XXX    XXX    XXX    XXX    XXX    XXX    XXX    XXX
*123456789*12 123456 123456789012345  12345678901234567890 ______ ______ ______ ______ ______ ______ ______ ______ ______
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3

DO WHILE !EOF() .AND. INKEY() <> 32
  SELECT (lcWorkTmp)
  IF llMultWH .AND. !lfStyWarH()
    SKIP
    LOOP
  ENDIF
  WAIT WINDOW 'PRINTING..'+style+'...<Space Bar> to Abort' NOWAIT
  SET DEVICE TO PRINT
  IF ROW > 55
    PAGENO = PAGENO + 1
    DO lpRpt_Hdr WITH  XREPORT,XTITLE,R_WIDTH
    IF !EMPTY(XTITLE)
      ROW = 6
    ELSE
      ROW = 5
    ENDIF
    @ ROW,01 SAY '&lcStyPic COLOR DESC.....  STYLE DESCRIPTION...    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8 TOTQTY'
    ROW = ROW + 1
  ENDIF
  SELECT (lcWorkTmp)
  XSCALE = GETSCALE(SCALE,SPACE(2))
  @ ROW,60 SAY XSCALE
  ROW = ROW + 1
  =lfPriInven()
  @ ROW,01 SAY STYLE
  *- Print Color Description
  XCOLOR=SUBSTR(Style,lnMajorLen+2,lnColorLen)
  lcColor = gfCodDes(XCOLOR,'COLOR' )
  @ ROW,21 SAY ALLTRIM(SUBSTR(lcColor,1,15))
  SELECT (lcWorkTmp)
  @ ROW,38 SAY SUBSTR(DESC,1,20)
  XCOL = 59
  FOR X = 1 TO 9
    @ ROW,XCOL SAY '______'
    XCOL = XCOL + 7
  ENDFOR
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE('-',130)
  ROW = ROW + 1
  SKIP
ENDDO
DO ENDREPORT  && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREEN
RETURN
*--

*!*************************************************************
*! Name      : lfStyWarH
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/24/1999 
*! Purpose   : Chick if style assigend to specific ware house
*!*************************************************************
*! Calls     : .....
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Example   : = lfStyWarH()
*!*************************************************************
*
FUNCTION lfStyWarH

IF EMPTY(lcRpWareCd)
  RETURN .T.
ENDIF
RETURN SEEK(Style+lcRpWareCd,'StyDye')
*-- END OF lfStyWarH

*!*************************************************************
*! Name      : lfPriInven
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/24/1999 
*! Purpose   :  Print inventory if user want to.
*!*************************************************************
*! Calls     : .....
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Example   : = lfPriInven()
*!*************************************************************
*
FUNCTION lfPriInven

IF lcInventry = "Y"
  IF EMPTY(lcRpWareCd)
    @ ROW,059 SAY Stk1
    @ ROW,066 SAY Stk2
    @ ROW,073 SAY Stk3
    @ ROW,080 SAY Stk4
    @ ROW,087 SAY Stk5
    @ ROW,094 SAY Stk6
    @ ROW,101 SAY Stk7
    @ ROW,108 SAY Stk8
    @ ROW,114 SAY TotStk
  ELSE
    =SEEK(Style+lcRpWareCd,'StyDye')
    @ ROW,059 SAY StyDye.Stk1
    @ ROW,066 SAY StyDye.Stk2
    @ ROW,073 SAY StyDye.Stk3
    @ ROW,080 SAY StyDye.Stk4
    @ ROW,087 SAY StyDye.Stk5
    @ ROW,094 SAY StyDye.Stk6
    @ ROW,101 SAY StyDye.Stk7
    @ ROW,108 SAY StyDye.Stk8
    @ ROW,114 SAY StyDye.TotStk
  ENDIF
  ROW = ROW + 1
ENDIF
*-- END OF lfPriInven

*!*************************************************************
*! Name      : lfVStyWar
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/22/1999 
*! Purpose   : Validate if style + color assigned to warehouse
*!*************************************************************
*! Calls     : .....
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Example   : = lfVStyWar()
*!*************************************************************
*
FUNCTION lfVStyWar
PRIVATE lnAlias

lnAlias = SELECT()
SELECT STYLE
SCAN
 llQuit = SEEK(Style+lcRpWareCd,'StyDye')
 IF llQuit
   EXIT
 ENDIF
ENDSCAN
IF !llQuit
  *---Text : 'No style's assigend to warehouse '
  =gfModalGen('INM42041B00000','DIALOG',lcStyPic+'|'+'warehouse '+lcRpWareCd)
  SET DEVICE TO SCREEN
  RETURN .F.
ENDIF
SELECT(lnAlias)
*-- END OF lfVstyWar

*!*************************************************************
*! Name      : lpRpt_Hdr
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/22/1999 
*! Purpose   : PRINTS THE HEADER ON THE REPORTS (WIDE PAPER,NARROW PAPER)
*!*************************************************************
*! Calls     : .....
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Example   : = lpRpt_Hdr()
*!*************************************************************
*
PROCEDURE lpRpt_Hdr
PARAMETER XPROG,XRPTNAME,XTYPE

PRIVATE ALL LIKE X*
XRPTNAME = TRIM(XRPTNAME)
R_TITLE  = TRIM(R_TITLE)
= SEEK (lcRpWareCd,'Warehous')
lcWareDesc = ALLTRIM(Warehous.cdesc)
lcWCodDes = ALLTRIM(lcRpWareCd + SPACE(02) + lcWareDesc)
X1 = ((130 - (LEN(TRIM(gcCom_NAME))))/2)
X2 = ((130 - (LEN( R_TITLE  )))/2)
X3 = ((130 - (LEN( XRPTNAME )))/2)
X4 = ((130 - (LEN( lcWCodDes )))/2)
@ 01,000 SAY XPROG
@ 01,X1  SAY gcCom_NAME
@ 01,120 SAY gdSysDate
@ 01,129 SAY '~'   
@ 02,000 SAY TIME()
@ 02,X2  SAY R_TITLE 
@ 02,120 SAY 'PAGE#'
@ 02,126 SAY STR(PAGENO,4)
IF !EMPTY(XRPTNAME)
  @ 03,X3  SAY XRPTNAME
ENDIF
IF !EMPTY(lcRpWareCd)
  IF !EMPTY(XRPTNAME)
    @ 04,X4  SAY lcWCodDes
    @ 05,00 SAY REPLICATE('*',132)
  ELSE
    @ 03,X4  SAY lcWCodDes
    @ 04,00 SAY REPLICATE('*',132)
  ENDIF
ELSE
  @ 04,00 SAY REPLICATE('*',132)
ENDIF
RETURN
*-- END OF lpRpt_Hdr 

*!*************************************************************
*! Name        : lfvStyle
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 08/22/1999 
*! Purpose     : validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
*
FUNCTION lfvStyle

lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE
IF !EMPTY(&lcStyle.) 
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor  
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
    IF EMPTY(&lcStyle)
      &lcStyle = laOldVal
    ENDIF
  ENDIF
ENDIF
SET ORDER TO lcTag IN STYLE
*-- END OF lfvStyle

*!*************************************************************
*! Name      : lfWoldVal
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/22/1999 
*! Purpose   : To return the old value.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfWoldVal()
*!*************************************************************
FUNCTION lfWoldVal

laOldVal = EVALUATE(SYS(18))
*-- END OF lfWoldVal

*!*************************************************************
*! Name        : lfMajGet
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 08/23/1999 
*! Purpose     : To get the title and picture of style major segement 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Example     : = lfMajInfGet()
*!*************************************************************
*
FUNCTION lfMajGet

lcMajPic = "@! " + gfItemMask("PM")
lcMajTtl = gfItemMask("HM")
*-- END OF lfMajGet

*!*************************************************************
*! Name        : lfvFabric
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 08/23/1999 
*! Purpose     : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = lcRpFabr
*IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal, 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- END OF lfvFabric.

*!*************************************************************
*! Name        : lfVldWare
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 08/23/1999 
*! Purpose     : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Example     : = lfVldWare()
*!*************************************************************
*
FUNCTION lfVldWare

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = lcRpWareCd
*IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal, 'WareHous'))
   llObjRet = gfBrowWare (.T.)
   lcObjVal = IIF(EMPTY(llObjRet), laOldVal,llObjRet)
   &lcObjName = lcObjVal
ENDIF
*-- END OF lfVldWare

*!*************************************************************
*! Name        : lfEvalSegs
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 11/30/1998
*! Purpose     : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
*
FUNCTION lfEvalSegs

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's.'
*-- Compute Free/Color Items in Style code Structure. [End]
*-- Compute Variables titles. [begin]
lcStyMake  = lcStyMajor + ' (Make/Buy)'
*-- Compute Variables titles. [end]
RETURN ''
*-- End of lfEvalSegs.

*!**************************************************************************
*! Name      : lfCollTime
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/17/1999
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!**************************************************************************
*! Returns            : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
*
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- END OF lfCollTime.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfClearRep()
*!*************************************************************
*
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(lcWorkTmp),lcWorkTmp,0)
ERASE &gcWorkDir.&lcWorkTmp+'.DBF'
ERASE &gcWorkDir.&lcWorkTmp+'.CDX'
*-- END OF lfClearRep.
*!*************************************************************