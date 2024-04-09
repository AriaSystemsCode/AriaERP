*:***************************************************************************
*: Program file  : SOSTKAV
*: Program desc. : Stock Availiablity Report
*: For Report    : (....)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mohamed Atia Badran (MAB)
*: Date          : 01/04/1999
*:***************************************************************************
*: Calls : 
*:    Procedures : lpPrnCode,DISP_TITLES,ORD971,ORD972,SUB_ORDQTY,ACUM_OTS,
*:               : POST_NVE_VALUS,POST_BF,DISP_OTS_LINES,lpUpdOTS 
*:    Functions  : gfModalGen,gfItemMask,gfThermo,gfCodDes,FaBrow,gfRange.
*:               : lfwRepWhen,lfStyTemp,lfwOldVal,lfEvalSegs,lfSRVSty,lfStySum,
*:               : lfvFabric,lfClearRep,lfCollTime,lfGtYTD.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOSTKAV
*:***************************************************************************
*: This Report Program is due to E301112 ...
*:E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.
*:***************************************************************************
*:Modifications:
*:B606586,1 BWA 01/06/2003 Increase the printing size.
*:B120121,1 MMM 11/13/2003 Fix Bug of not Printing the OTS for each period IF the setting of 
*:                         OTS based on exact transaction date = 'Yes'.
*:B129693,1 EIH 10/02/2005 Fix bug that print wrong scale in case of extended size scale 
*:B129693,1 EIH 10/02/2005 And fix wrong QTY's in case of over recieve PO's
*:B129693,1 EIH 10/02/2005 And fix wrong date of first period in case of england.
*:B608691,1 TMI 09/14/2008 Create the laTempstru array manually , i.e field by field (T20080506.0002   )
 *:***************************************************************************

lcStTime = TIME()

*B120121,1 MMM 11/13/2003  Adding variable to hold the OTS based on exact transaction date Value. [Start]  
llOTSbasTr = gfGetMemVar('M_OTSBASTR',gcAct_Comp)
*B120121,1 MMM [End]



*B129693,1 EIH 10/02/2005 Declare laNetQTY to store QTY's to determine by its value if that line in posln is 
*B129693,1 EIH 10/02/2005 Over recieve or not [Begin]
DIMENSION laNetQTY[8]
*B129693,1 EIH 10/02/2005 [End]

llHaveClr = (lcFree_Clr = 'C')

lcMakeOp  = IIF(lcRpMake='M','','!')
lcRepExpr = [style = ''] +;
            IIF(lcRpMake='B','', [ AND ] + lcMakeOp+[Make]) +;
            IIF(lcRpStatus='B','',[ AND (Status="] + lcRpStatus + [")])

lcRpExp   = lcRepExpr + IIF( '.T.' $ lcRpExp , '' , [ AND ] + lcRpExp )
lcRpExp   = STRTRAN(lcRpExp,'STYLE.','')

*-- if user clear setting or change criteria.
*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.[Begin
*IF llClearFn OR !(lcRpExp == lcLastExpr) OR llChStyle
IF llClearFn OR llOGFltCh
*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.[End

  lcLastExpr = lcRpExp   && To later know that user change critria.
  STORE .F. TO llClearFn,llChStyle

  *-- if you have previous data clear workfile then recreate it. [begin]
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF  
    = lfStyTemp()  && Create temporary cursor.
  ENDIF
  *-- if you have previous data clear workfile then recreate it. [end]

  *-- Start collecting style data in new structure.
  SELECT STYLE
  SCAN FOR &lcRpExp
    SCATTER MEMVAR MEMO

    m.cMajor    = LEFT(STYLE,lnMajorLen)
    m.cNonMajor = RIGHT(STYLE,lnNonMajLn)
    m.Color     = IIF(llHaveClr,SUBSTR(STYLE,lnNonMajSt,lnColorLen),SPACE(6))

    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDSCAN
  *-- End   collecting style data in new structure.
  
ENDIF  

IF RECCOUNT(lcWorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

*--- Start Report printing.

*-- Setup relation Section [Begin]

*-- if company setuped to use Purshase order module.
IF 'PO' $ gcCmpModules
  SELECT POSLN
  *ahmed
  *SET RELATION TO ShipNo INTO SHPMTHDR, 'P' + Po INTO POSHDR
  SET RELATION TO ShipNo INTO SHPMTHDR, cStyType + Po INTO POSHDR
  *ahmed end
ENDIF

*-- if company setuped to use Manufacturing module.
IF 'MF' $ gcCmpModules
  SELECT CUTTKTL
  SET RELATION TO CUTTKT INTO CUTTKTH
ENDIF

*-- if company setuped to use Account Receivable module.
IF 'AR' $ gcCmpModules
  SELECT INVLINE
  SET RELATION TO Invoice INTO INVHDR
ENDIF

SELECT ORDLINE
SET RELATION TO cOrdType + Order INTO ORDHDR

*-- Setup relation Section [End  ]

SELECT (lcWorkFile)
GO TOP

XREPORT  = 'SOSTKAV'
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
DO lpPrnCode  && Do Print code.

*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.

WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
DO ENDREPORT  && Display or print the report .
WAIT CLEAR

SET DEVICE TO SCREEN
*--- End   Report printing.

*-- Clean relation Section [Begin]

*-- if company setuped to use Purshase order module.
IF 'PO' $ gcCmpModules
  SELECT POSLN
  SET RELATION TO
ENDIF

*-- if company setuped to use Manufacturing module.
IF 'MF' $ gcCmpModules
  SELECT CUTTKTL
  SET RELATION TO
ENDIF

*-- if company setuped to use Account receivable module.
IF 'AR' $ gcCmpModules
  SELECT INVLINE
  SET RELATION TO
ENDIF

SELECT ORDLINE
SET RELATION TO

*-- Clean relation Section [End  ]

*-- end of report code.

*-- Start of function and procedure section.
*-------------------------------------------

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : When function of OG
*!*************************************************************
*! Called from : OG read cycle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*
FUNCTION lfwRepWhen

IF TYPE('lcLastExpr') $ 'UL' OR EMPTY(lcLastExpr)

  R_WIDTH  = 'W'

  SET ORDER TO STYLE  IN STYLE
  SET ORDER TO FABRIC IN FABRIC

  DIMENSION laTempStru[1,4]
  laTempStru = ''

  *B608691,1   / TMI 09/14/2008 [Start] Create the laTempstru array manually , i.e field by field
  *SELECT STYLE
  *lnAllFlds = AFIELDS(laTempStru)
  PRIVATE lnI
  
  lnArrLen = 0
  
  lnArrLen = lnArrLen + 1
  DIMENSION laTempStru[lnArrLen, 4]
  laTempStru[lnArrLen,1] = 'STYLE'
  laTempStru[lnArrLen,2] = 'C'
  laTempStru[lnArrLen,3] = 19
  laTempStru[lnArrLen,4] = 0

  lnArrLen = lnArrLen + 1
  DIMENSION laTempStru[lnArrLen, 4]
  laTempStru[lnArrLen,1] = 'SCALE'
  laTempStru[lnArrLen,2] = 'C'
  laTempStru[lnArrLen,3] = 3
  laTempStru[lnArrLen,4] = 0

  lnArrLen = lnArrLen + 1
  DIMENSION laTempStru[lnArrLen, 4]
  laTempStru[lnArrLen,1] = 'DESC1'
  laTempStru[lnArrLen,2] = 'C'
  laTempStru[lnArrLen,3] = 60
  laTempStru[lnArrLen,4] = 0

  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)

    lnArrLen = lnArrLen + 1
    DIMENSION laTempStru[lnArrLen, 4]
    laTempStru[lnArrLen,1] = 'STK'+lcI
    laTempStru[lnArrLen,2] = 'N'
    laTempStru[lnArrLen,3] = 7
    laTempStru[lnArrLen,4] = 0
  
    lnArrLen = lnArrLen + 1
    DIMENSION laTempStru[lnArrLen, 4]
    laTempStru[lnArrLen,1] = 'ORD'+lcI
    laTempStru[lnArrLen,2] = 'N'
    laTempStru[lnArrLen,3] = 7
    laTempStru[lnArrLen,4] = 0
  
    lnArrLen = lnArrLen + 1
    DIMENSION laTempStru[lnArrLen, 4]
    laTempStru[lnArrLen,1] = 'SHP'+lcI
    laTempStru[lnArrLen,2] = 'N'
    laTempStru[lnArrLen,3] = 7
    laTempStru[lnArrLen,4] = 0
      
  ENDFOR
  
  lnAllFlds = ALEN(laTempStru,1)
  *B608691,1 / TMI 09/14/2008 [End  ] 

  DIMENSION laTempStru[lnAllFlds + IIF(lnNonMajLn>0,3,2), 4]
  laTempStru[lnAllFlds+1,1] = 'COLOR'
  laTempStru[lnAllFlds+1,2] = 'C'
  laTempStru[lnAllFlds+1,3] = 6
  laTempStru[lnAllFlds+1,4] = 0
  
  laTempStru[lnAllFlds+2,1] = 'cMajor'
  laTempStru[lnAllFlds+2,2] = 'C'
  laTempStru[lnAllFlds+2,3] = lnMajorLen
  laTempStru[lnAllFlds+2,4] = 0

  IF lnNonMajLn > 0
    laTempStru[lnAllFlds+3,1] = 'cNonMajor'
    laTempStru[lnAllFlds+3,2] = 'C'
    laTempStru[lnAllFlds+3,3] = lnNonMajLn
    laTempStru[lnAllFlds+3,4] = 0
  ENDIF  
  
  =lfStyTemp()  && Create temporary used in collecting data.

ENDIF
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfStyTemp
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Create temporary cursor.
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStyTemp()
*!*************************************************************
*
FUNCTION lfStyTemp

CREATE CURSOR (lcWorkFile) ;
   FROM ARRAY laTempStru

INDEX ON cMajor + IIF(lcFree_Clr = 'C', Color , cNonMajor) TAG (lcWorkFile)
*-- end of lfStyTemp.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
  lnMajSeg    = gfItemMask('SM')  && No. of major segments.
  *-- Compute Free/Color Items in Style code Structure. [Begin]
  DIMENSION laMajSegs[1,1]
  = gfItemMask(@laMajSegs)

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

  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
  *-- Compute Free/Color Items in Style code Structure. [End]

  *-- Compute Variables titles. [begin]
  lcStyStat = lcStyMajor + ' Status'
  lcStyMake = lcStyMajor + ' (Make/Buy)'
  *-- Compute Variables titles. [end]

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvFabric.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.  && Rise clear done flag.
*-- Close temp. opended files, if it used.
IF USED(lcWorkFile)
 USE IN (lcWorkFile)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lpPrnCode
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Report Print code.
*!*************************************************************
*! Calls     : 
*!             Procedures : DISP_TITLES,ORD971,ORD972,POST_NVE_VALUS,DISP_OTS_LINES,
*!             Functions  : lfGtYTD.
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lpPrnCode()
*!*************************************************************
PROCEDURE lpPrnCode
* -- Initialize the necessary global variables.
R_TITLE    = 'Stock Availability Report'
PAGENO     = 00
ROW        = 99

*B606586,1 BWA 01/06/2003 Increase the printing size.[START]
*XMAXROW    = 47
XMAXROW    = 58
*B606586,1 BWA.[END]

XTODAY     = gdSysDate
C_MONTH    = MONTH (XTODAY)
C_YEAR     = YEAR  (XTODAY)
XSTYLE     = SPACE (12)
XCOLOR     = SPACE (06)
lcFullSty  = SPACE(19)
llPrintSty = .F.    && TAK 05/25/94 Flag to print style and
                    && desc if style was changed in case of
                    && llRpZeroAv = .F.

STORE 0 TO lnTitlLen,lnSzStart,lnClrStart,lnDescSt

* --- Create and initialize :
* -- 1) An array to hold the 10 periods. Each period contains 2 dates ,
* --    the first one is the start of the period and the last one is the end
* --    of the period , so ..
* --    the array will be "XPERIOD_ARR [10 * 2]" (Remember that this report
* --    deals with only 5 month).
* -- 2) An array to hold the end day for each month....
* --    The report titles variables are XLINE_1 , XLINE_2, And....
* --    to display XLINE_2 with the start and end of each period, we
* --    had to have an array "XYEAR" to hold the end day for each month.
* --    This array will be released after finishing of forming the titles.
* -- 3) Two dimensional array "STY_INFO", to hold the OTS quantities
* --    for each [Color\Size,Period], it will be initialized each time
* --    the style changes to fit the size scale for the current style.
* --    The array has 12 cols., 10 of these 12 are for the periods,
* --    and col. no.[11] is to hold the total of the periods,
* --    and col. no.[12] is to hold the quantities shipped for this
* --    color/size.
* --    The array also has a dynamic number of rows according to ..
* --    A) The size scale which changes from one style to another.
* --    B) The Report format : if not by size the array rows will be
* --       increased by one to hold the totals for each color,
* --       otherwise no rows will be added.
* -- 4) One dimensional array "XTOTAL", to hold the total OTS quantities
* --    for one style\color (Accumulater).

* -- Create and Initialive the arrays .
DIMENSION XPERIOD_ARR [20] , XYEAR [12] , STY_INFO [1,12] , XTOTAL [12]
XPERIOD_ARR = {}
XYEAR       = 31
STORE 30 TO XYEAR(4),XYEAR(6),XYEAR(9),XYEAR(11)
XYEAR (2)   = IIF (MOD(YEAR(XTODAY),4)=0 , 29 , 28)
STY_INFO    = 00
XTOTAL      = 00

* -- Initialize the first period with the first day of today's month .

*B129693,1 EIH 10/02/2005 Fix wrong date of first period in case of england [Begin].
*XPERIOD_ARR(1) = CTOD(ALLTRIM(STR(C_MONTH))+'/01/'+ALLTRIM(STR(C_YEAR)))
*B129693,1 EIH 10/02/2005 [End].

*ahmed
*XPERIOD_ARR(2) = CTOD(ALLTRIM(STR(C_MONTH))+'/15/'+ALLTRIM(STR(C_YEAR)))
llEngDate = (ALLTRIM(gcContCode) = 'ENG' .OR. SET('DATE')='BRITISH' )
IF llEngDate 
  
  *B129693,1 EIH 10/02/2005 Fix wrong date of first period in case of england [Begin].
  XPERIOD_ARR(1) = CTOD('01/'+ALLTRIM(STR(C_MONTH))+'/'+ALLTRIM(STR(C_YEAR)))
  *B129693,1 EIH 10/02/2005 [End].
  
  XPERIOD_ARR(2) = CTOD('15/'+ALLTRIM(STR(C_MONTH))+'/'+ALLTRIM(STR(C_YEAR)))
ELSE
  
  *B129693,1 EIH 10/02/2005 Fix wrong date of first period in case of england [Begin].
  XPERIOD_ARR(1) = CTOD(ALLTRIM(STR(C_MONTH))+'/01/'+ALLTRIM(STR(C_YEAR)))  
  *B129693,1 EIH 10/02/2005 [End].
  
  XPERIOD_ARR(2) = CTOD(ALLTRIM(STR(C_MONTH))+'/15/'+ALLTRIM(STR(C_YEAR)))
ENDIF
*ahmed end

* -- Initialize the title variables.
lnTitlLen = 25 - ( LEN(lcStyMajor) + LEN(lcOtherTlt) )
lnSzStart = LEN(lcStyMajor) + 1
lnClrStart= lnSzStart + 6
lnDescSt  = lnClrStart + LEN(lcOtherTlt) + 1   

XLINE_1   = SPACE (11)
XLINE_2   = lcStyMajor + IIF (llRpSizes,'/Size  ',SPACE(7))+ ;
            lcOtherTlt + ' Desc.' + SPACE(lnTitlLen)


* -- Fill the "XPERIOD_ARR" array and form the report titles.
XLOOP   = 3
XPERIOD = 1
XMONTH  = 0
DO WHILE XPERIOD < 10
  * -- Fill the periods array.
  XPERIOD_ARR(XLOOP  ) = XPERIOD_ARR(XLOOP-1) + 1
  XPERIOD_ARR(XLOOP+1) = IIF ( MOD(XPERIOD,2) = 0 ,               ;
                               XPERIOD_ARR(XLOOP) + 14          , ;
                               GOMONTH (XPERIOD_ARR(XLOOP-2),1)-1 )
  * -- Form the titles.
  IF MOD (XPERIOD,2) <> 0
    XLINE_1 = XLINE_1 + PADC (UPPER(CMONTH(GOMONTH(XTODAY,XMONTH))) + ;
                        IIF (XMONTH=4,' FUTURE','') , 16 , ' ' )
    XLINE_2 = XLINE_2 + '01-15   16-'+;
                        ALLTRIM(STR(XYEAR(MONTH(GOMONTH(XTODAY,XMONTH)))))+;
                        SPACE(3)
    XMONTH  = XMONTH + 1
  ENDIF

  XLOOP   = XLOOP   + 2
  XPERIOD = XPERIOD + 1
ENDDO

* -- To complete forming the title variabls.
XLINE_1 = XLINE_1 + ' TOTAL  TOTAL'
XLINE_2 = XLINE_2 +  'AVAIL SOLD YTD'
RELEASE XYEAR
* --- End of Initializing the necessary variables and arrays. ----------

*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� !*
*!                    *****                   *****                    !*
*!                    *** --- THE MAIN LOOP --- ***                    !*
*!                    *****                   *****                    !*
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� !*

SELECT (lcWorkFile)

lnUpThermo = RECCOUNT(lcWorkFile)
lnThermoNo = 0


*B129693,1 EIH 10/02/2005 Fix bug that print wrong scale in case of extended size scale [Begin] .
STORE '' TO lcOldScl 
STORE ' ' TO  lcNewScl
*B129693,1 EIH 10/02/2005 [End] .


DO WHILE !EOF()
  
  IF ROW > XMAXROW - 2
  
    DO DISP_TITLES
    LOOP
  
  ELSE
  
    * -- Each time the style changes the following will be done :
    * -- 1) Update the current style variable "XSTYLE".
    * -- 2) Print the style title.
    * -- 3) Get the size scale for this style.
    * -- 4) Recreate "STY_INFO" to fit the current style\size scale.

    *B129693,1 EIH 10/02/2005 Fix bug that print wrong scale in case of extended size scale [Begin] .
    *IF cMajor <> XSTYLE
    IF cMajor <> XSTYLE OR lcOldScl <> lcNewScl
      lcOldScl = lcNewScl
	*B129693,1 EIH 10/02/2005 [End] .
	
      XSTYLE    = cMajor
      SET DEVICE TO SCREEN
      lnThermoNo = lnThermoNo + 1
      = gfThermo(lnUpThermo,lnThermoNo,'Calculate availability for ' + lcStyMajor + ': ',XSTYLE)
      SET DEVICE TO PRINT
      
      Y =1                           && TMI 12/01/94

      *-- Added by TAK 05/25/94
      IF !llRpZeroAv
        llPrintSty = .T.
        XSTYLE  = cMajor
        XDESC  = DESC1
      ELSE
        
        @ ROW,00 SAY XSTYLE
        @ ROW,lnDescSt SAY DESC1                            && TAK 02/28/94
        ROW = ROW + 1
      
      ENDIF
      *-- End TAK 05/25/94 ---

      SELECT SCALE
      XSIZE_CNT = IIF ( SEEK ('S'+&lcWorkFile..SCALE) , CNT , 1)
      
      *B129693,1 EIH 10/02/2005 Fix bug that print wrong scale in case of extended size scale [Begin] .
      lcNewScl = &lcWorkFile..SCALE
      *B129693,1 EIH 10/02/2005 [End] .
      
      XSCALES   = GETSCALE (&lcWorkFile..SCALE,SPACE(0))          && TMI 12/01/94
      DIMENSION STY_INFO [XSIZE_CNT+IIF(llRpSizes,0,1),12]
      STY_INFO  = 00
      SELECT (lcWorkFile)
    ENDIF

    * -- Each time the color changes the following will be done :
    * -- 1) Display the new color on the screen.
    * -- 2) Update the current color variable "XCOLOR".
    * -- 4) Reinitialize "STY_INFO".
    * -- 5) Put the stock for this color on the 1st half or on the 2nd half
    * --    of the first month as a starting number for all the size scale
    * --    "STY_INFO [XSIZE_NO,(1 or 2)]".
    * -- 6) Put the shipped quantities on the last col. of the array for all
    * --    the size scale "STY_INFO [XSIZE_NO,12]" .
    * -- 7) If the report format is not by size then add the 2 quantities
    * --    (*No.5,6) on the last row of the array to accumulate them.

    XCOLOR    = IIF(llHaveClr,COLOR,cNonMajor)
    lcFullSty = STYLE

    STY_INFO = 0.00

    *B500568,1 Call a function to compute the ordered & shiped
    *B500568,1 quantities only for orders and invoices within
    *B500568,1 the current fiscal year.   
    IF llRpCurPrd
      STORE 0 TO lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8
      STORE 0 TO lnShp1,lnShp2,lnShp3,lnShp4,lnShp5,lnShp6,lnShp7,lnShp8
      = lfGtYTD()
    ENDIF  

    FOR  XSIZE_NO = 1 TO XSIZE_CNT
      XSTK_ID = 'STK' + ALLTRIM(STR(XSIZE_NO))
      STY_INFO [XSIZE_NO,01] = &XSTK_ID
      *-- YMA 06/09/94 Added the ordered quantity to the last column.
      *B500568,1 If the user selected to print the sold year
      *B500568,1 to date for current year only, Accumulate
      *B500568,1 the order and ship quantities from the order 
      *B500568,1 and invoice files otherwise get the order and 
      *B500568,1 ship quantities from the style file.
      XSHP_ID = IIF(llRpCurPrd,'lnShp','SHP') + ALLTRIM(STR(XSIZE_NO))
      lcOrdId = IIF(llRpCurPrd,'lnOrd','ORD') + ALLTRIM(STR(XSIZE_NO))
      STY_INFO [XSIZE_NO,12] = &XSHP_ID + &lcOrdId
    ENDFOR

    SELECT (lcWorkFile)

    *-- According to "MAKE" field please call either "ORD971" or "ORD972".
    IF MAKE

      IF 'MF' $ gcCmpModules
        DO ORD972
      ENDIF
    
    ELSE  && Imported style
      
      IF 'PO' $ gcCmpModules
        DO ORD971
      ENDIF

    ENDIF

    * -- YMA 02/02/94 To post the negative values to the nearst
    * -- positive value.
    
    *B120121,1 MMM 11/13/2003 Fix Bug of not Printing the OTS for each period IF the setting of 
    *                         OTS based on exact transaction date = 'Yes' [Start]
    *DO POST_NVE_VALUS
    IF !llOTSbasTr
      DO POST_NVE_VALUS
    ELSE
      DO lpUpdOTS
    ENDIF 
    *B120121,1 MMM [End]
   
    * -- Then we will print the OTS lines .
    DO DISP_OTS_LINES
  ENDIF

  * -- Go to the next record .
  SELECT (lcWorkFile)
  SKIP
ENDDO

SET DEVICE TO SCREEN
= gfThermo(lnUpThermo,lnUpThermo,'Finish Printing Stock availability',' ')
*WAIT WINDOW 'Calculate availability for ' + lcStyMajor + ': ' + XSTYLE NOWAIT
SET DEVICE TO PRINT

* --- This part is to print the "GRAND TOTAL" --------------------------
* --- It will be done when the report is finished .
IF ROW > XMAXROW
  DO DISP_TITLES
ENDIF

ROW = ROW + 1
@ ROW,0 SAY REPLICATE ('*',132)
ROW = ROW + 1
@ ROW,5 SAY 'GRAND TOTAL'
XCOL = 36
FOR XLOOP = 1 TO 12
  XCOL = IIF ( XLOOP = 12 , XCOL+1 , XCOL )
  @ ROW,XCOL SAY XTOTAL [XLOOP] PICTURE '9999999'
  XCOL = XCOL + 8
ENDFOR
ROW = ROW + 1
@ ROW,0 SAY REPLICATE ('*',132)
* --- End of printing the "GRAND TOTAL" -------------------------------

*-- end of lpPrnCode. DO


*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! PROG : DISP_TITLES
*! DATE : JAN 24,94
*! AUTH : YASSER MOHAMMED ALY (YMA)
*! DESC : Print the report titles and initialize the necessary variables .
*! NOTE : Called from ORD970.PRG
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE DISP_TITLES

PAGENO  = PAGENO  + 1
DO RPT_HDR WITH 'SOSTKAV','',R_WIDTH
@ 5,26 SAY XLINE_1
@ 6,00 SAY XLINE_2
ROW = 7
* ------------------ END OF DISP_TITLES PROCEDURE ------------------ *



*********** BADRAN


*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! PROG : ORD971
*! DATE : JAN 24,1994
*! AUTH : YASSER MOHAMMED ALY (YMA)
*! DESC : Procedure to compute the OTS quantites for the imported
*!        styles (MAKE : NO)
*! NOTE : Called from ORD970.PRG
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE ORD971

* -- Remember that there is a relation between
* -- "POSLN" -> "POSHDR"  to check on the "COMPLETE" and the "AVAILABLE "
* -- dates in case of the transaction code 1,2,4,5
* -- "POSLN" -> "SHPMTHDR" to check on the "ETA" date in case of
* -- the transaction code 3

IF SEEK(lcFullSty,'POSLN')
  SELECT POSLN
  *ahmed
  *SCAN REST WHILE Style+cstytype+po+STR(lineno,6)+trancd = lcFullSty FOR POSHDR.STATUS = 'O'
  
  *B129693,1 EIH 10/02/2005 Scan posln for ctrancd in '12' to wrong calculate of damaged and canceled 
  *B129693,1 EIH 10/02/2005 And recieving on second quailty [Begin]
  *SCAN REST WHILE Style+cstytype+po+STR(lineno,6)+trancd = lcFullSty ;
            FOR POSHDR.STATUS $ 'HO' .AND. cstytype # 'C' .AND. TranCD # '6'
            
  SCAN REST WHILE Style+cstytype+po+STR(lineno,6)+trancd = lcFullSty ;
            FOR POSHDR.STATUS $ 'HO' .AND. cstytype # 'C' .AND. TranCD $ '12'          
  *B129693,1 EIH 10/02/2005 [End]
  
  *ahmed end
    XTRAN_DATE = IIF (EMPTY(POSHDR.AVAILABLE) ,;
                      POSHDR.COMPLETE         ,;
                      POSHDR.AVAILABLE )

    * -- To accumulate the quantity in process.
    *ahmed
    *DO ACUM_OTS WITH 'POSLN' , XTRAN_DATE , IIF(TRANCD='1',1,-1)
    lcTrSign = IIF(Trancd='1',1,-1) * IIF(cStyType $ 'ADPN',1,-1)
    DO ACUM_OTS WITH 'POSLN' , XTRAN_DATE , lcTrSign
    *ahmed end
    
    IF TRANCD ='3'
      DO ACUM_OTS WITH 'POSLN' , SHPMTHDR->ETA , 1
    ENDIF
  ENDSCAN
ENDIF
* -- To subtract the proper quantities in the "ORDLINE" file .
DO SUB_ORDQTY
* ------------------ END OF ORD971 PROCEDURE ----------------------- *


*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! PROG : ORD972
*! DATE : JAN 24,1994
*! AUTH : YASSER MOHAMMED ALY (YMA).
*! DESC : Procedure to compute the OTS quantites for the
*!        manufactured styles (MAKE : YES)
*! NOTE : Called from ORD970.PRG
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE ORD972

* -- Remember that there is a relation between
* -- "CUTTKTL" -> "CUTTKTH"  to check on the "COMPLETE" date .
IF SEEK(lcFullSty,'CUTTKTL')
  SELECT CUTTKTL
  
  *B129693,1 EIH 10/02/2005 Scan CUTTKTL for ctrancd in '12' to wrong calculate of damaged and canceled 
  *B129693,1 EIH 10/02/2005 And recieving on second quailty [Begin]
  *SCAN REST WHILE style+cuttkt+trancd = lcFullSty FOR CUTTKTH.STATUS $ 'OAH'
  SCAN REST WHILE style+cuttkt+trancd = lcFullSty FOR CUTTKTH.STATUS $ 'OAH' .AND. TranCD $ '12'
  *B129693,1 EIH 10/02/2005 [End]
  
    * -- To accumulate the quantity in process.
    DO ACUM_OTS WITH 'CUTTKTL' , CUTTKTH.COMPLETE , IIF (TRANCD='1',1,-1)
  ENDSCAN
ENDIF
* -- To subtract the proper quantities in the "ORDLINE" file .
DO SUB_ORDQTY
* ------------------ END OF ORD972 PROCEDURE ----------------------- *


*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! PROG : SUB_ORDQTY
*! DATE : JAN 24,1994
*! AUTH : YASSER MOHAMMED ALY (YMA)
*! DESC : Procedure to substract the order line quantities from the
*!        OTS quantities .
*! NOTE : Called from ORD971 AND ORD972
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Modifications :
*!      YMA 02/02/94
*!          1) Modified the comptation of the sales order to make it
*!             use the start ship date instead of the completion
*!             date.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE SUB_ORDQTY

* -- Remember that there is a relation between
* -- "ORDLINE" -> "ORDHDR"  to check on the status of the order .
IF SEEK(lcFullSty,'ORDLINE')
  SELECT ORDLINE
  SCAN REST WHILE Style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcFullSty ;
            FOR ORDHDR.STATUS $ 'OH'

    * -- To accumulate the quantity in process .
    *** YMA 02/02/94 Use the start ship date instead of the completion date.
    DO ACUM_OTS WITH 'ORDLINE' , START , -1
  ENDSCAN
ENDIF  
* ------------------ END OF SUB_ORDQTY PROCEDURE ------------------- *

*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! PROG : ACUM_OTS
*! DATE : JAN 24,1994
*! AUTH : YASSER MOHAMMED ALY (YMA)
*! DESC : Procedure to accumulate the OTS quantities .
*! NOTE : Called from ORD971 , ORD972 , SUB_ORDQTY
*! PARM : 1) XFILE    ------> To hold the file name in process .
*!        2) XTRANS_DATE ---> To hold the date in process .
*!        3) XTYPE    ------> To deside whether to add or subtract the
*!                            quantity in process to or from the balance .
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Modifications :
*!      YMA 06/14/94
*!          1) Added the facility to accumulate the transactions
*!             that have a date prior to the first date to the first
*!             period in the report.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE ACUM_OTS
PARAMETERS XFILE , XTRANS_DATE , XTYPE

SELECT (XFILE)


*B129693,1 EIH 10/02/2005 Fill laNetQTY array which store QTY's to determine by its value if that line in posln is 
*B129693,1 EIH 10/02/2005 Over recieve or not [Begin]

STORE 0 TO laNetQTY
IF XFILE = 'POSLN' OR XFILE = 'CUTTKTL'
  STORE 0 TO laNetQTY
  lnRecNo = RECNO(XFILE)
  IF TRANCD = '2' AND XFILE = 'POSLN'
    lcCurrKey = POSLN.STYLE+POSLN.CSTYTYPE+POSLN.PO+STR(POSLN.lineno,6)
    lcSession = POSLN.crsession
    =SEEK(POSLN.STYLE+POSLN.CSTYTYPE+POSLN.PO+STR(POSLN.lineno,6),'POSLN')
    SCAN REST WHILE style+cstytype+po+STR(lineno,6)+trancd =  lcCurrKey 
      IF TRANCD = '1'
        FOR XSIZE_NO = 1 TO XSIZE_CNT
          lcQtyNo = 'QTY' + ALLTRIM(STR(XSIZE_NO))
          laNetQTY[XSIZE_NO] = laNetQTY[XSIZE_NO] + &lcQtyNo
        ENDFOR  
      ELSE
        FOR XSIZE_NO = 1 TO XSIZE_CNT
          lcQtyNo = 'QTY' + ALLTRIM(STR(XSIZE_NO))
          laNetQTY[XSIZE_NO] = laNetQTY[XSIZE_NO] - &lcQtyNo
        ENDFOR
      ENDIF  
      IF (POSLN.crsession = lcSession )
        EXIT
      ENDIF
    ENDSCAN
  ELSE
    IF TRANCD = '2' AND XFILE = 'CUTTKTL'
      lcCurrKey = CUTTKTL.STYLE+CUTTKTL.cuttkt
      lcSession = CUTTKTL.crsession
      lnLineNo  = CUTTKTL.lineno
      =SEEK(lcCurrKey ,'CUTTKTL')
      SCAN REST WHILE style+cuttkt+trancd = lcCurrKey 
        IF CUTTKTL.lineno # lnLineNo 
          LOOP
        ENDIF
        IF TRANCD = '1'
          FOR XSIZE_NO = 1 TO XSIZE_CNT
            lcQtyNo = 'QTY' + ALLTRIM(STR(XSIZE_NO))
            laNetQTY[XSIZE_NO] = laNetQTY[XSIZE_NO] + &lcQtyNo
          ENDFOR  
        ELSE
          FOR XSIZE_NO = 1 TO XSIZE_CNT
            lcQtyNo = 'QTY' + ALLTRIM(STR(XSIZE_NO))
            laNetQTY[XSIZE_NO] = laNetQTY[XSIZE_NO] - &lcQtyNo
          ENDFOR
        ENDIF
        IF CUTTKTL.crsession = lcSession
          EXIT
        ENDIF      
      ENDSCAN
    ENDIF
  ENDIF   
  GOTO lnRecNo 
ENDIF

*B129693,1 EIH 10/02/2005 [End]



* -- If the transaction date between the (1st OR 2nd) half of the first
* -- month and the last half of the last month we will do the following :
* -- ......
IF BETWEEN(XTRANS_DATE,XPERIOD_ARR[1],XPERIOD_ARR[20])
  * -- Loop around the size scale to check it all.
  FOR XSIZE_NO = 1 TO XSIZE_CNT

    XPER = 1
    * -- Search for the right period to process .
    DO WHILE XPER < 20

      * -- If the transaction date is between the current period "XPER" ..
      * -- Get all the quantities in process , then add them to the
      * -- proper cells in the array [Size,Period] .
      * -- And add them to the last row of the array [Max Size+1,Period]
      * -- to acumulate them (Get the sum) if the report format is not by
      * -- size.
      IF BETWEEN (XTRANS_DATE,XPERIOD_ARR[XPER],XPERIOD_ARR[XPER+1])
        
        XQTY_ID = 'QTY' + ALLTRIM(STR(XSIZE_NO))
        
        *B129693,1 EIH 10/02/2005 Check if this line is over recieve or not if laNetQTY < 0 then this 
        *B129693,1 EIH 10/02/2005 Line is over revieve [Begin]
        *XQTY    =  &XQTY_ID * XTYPE     
        IF laNetQTY[XSIZE_NO] < 0 
          XQTY    =  (laNetQTY[XSIZE_NO] + &XQTY_ID) * XTYPE 
        ELSE
          XQTY    =  &XQTY_ID * XTYPE 
        ENDIF
        *B129693,1 EIH 10/02/2005 Line is over revieve [End]
               
        STY_INFO [XSIZE_NO,ROUND(XPER/2,0)] = ;
        STY_INFO [XSIZE_NO,ROUND(XPER/2,0)] + XQTY
        EXIT
      ENDIF
      XPER = XPER + 2
    ENDDO
  ENDFOR
ELSE
  * -- If the transaction date is after the last half of the last month
  * -- the following will be done :
  *B129693,1 EIH 10/02/2005 Be sure we compare to date variabls [Begin].
  *IF XTRANS_DATE > XPERIOD_ARR[20]
  IF {XTRANS_DATE} > {XPERIOD_ARR[20]}
  *B129693,1 EIH 10/02/2005 [End].

    * -- Add the quantities in process to the last period for all the
    * -- size scale STY_INFO [XSIZE_NO,10] .
    * -- And add them - for all the size scale - to the last row of the
    * -- array STY_INFO [XSIZE_CNT+1,10] to acumulate it if the report
    * -- format is not by size.
    FOR  XSIZE_NO = 1 TO XSIZE_CNT
      XQTY_ID = 'QTY' + ALLTRIM(STR(XSIZE_NO))
      
      *B129693,1 EIH 10/02/2005 Check if this line is over recieve or not if laNetQTY < 0 then this 
      *B129693,1 EIH 10/02/2005 Line is over revieve [Begin]
      *XQTY    =  &XQTY_ID * XTYPE
      IF laNetQTY[XSIZE_NO] < 0 
        XQTY    = (laNetQTY[XSIZE_NO] + &XQTY_ID) * XTYPE 
      ELSE
        XQTY    =  &XQTY_ID * XTYPE 
      ENDIF
      *B129693,1 EIH 10/02/2005 [End]
      
      STY_INFO [XSIZE_NO,10] = STY_INFO [XSIZE_NO,10] + XQTY
    ENDFOR
  ELSE
    *-- YMA 06/14/94 Added the facility to accumulate the transactions
    *-- that have a date prior to the first date to the first period
    *-- in the report.
    FOR  XSIZE_NO = 1 TO XSIZE_CNT
      XQTY_ID = 'QTY' + ALLTRIM(STR(XSIZE_NO))
      
      *B129693,1 EIH 10/02/2005 Check if this line is over recieve or not if laNetQTY < 0 then this 
      *B129693,1 EIH 10/02/2005 Line is over revieve [Begin]
      *XQTY    =  &XQTY_ID * XTYPE
      IF laNetQTY[XSIZE_NO] < 0 
        XQTY    = (laNetQTY[XSIZE_NO] + &XQTY_ID) * XTYPE 
      ELSE
        XQTY    =  &XQTY_ID * XTYPE 
      ENDIF
      *B129693,1 EIH 10/02/2005 [End]
      
            
      STY_INFO [XSIZE_NO,1] = STY_INFO [XSIZE_NO,1] + XQTY
    ENDFOR
  ENDIF
ENDIF
* ------------------ END OF ACUM_OTS PROCEDURE --------------------- *


*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! PROG : POST_NVE_VALUS
*! DATE : FEB 02,94
*! AUTH : YASSER MOHAMMED ALY (YMA).
*! DESC : Procedure to post the negative values in any period to the
*!        nearst positive value of any next period.
*!        - We do this by looping the two dimensional array "STY_INFO"
*!          in case of the report format is by size , otherwise we will
*!          loop just the last row of the same array (which holds the
*!          total OTS quantities for this color) ,to do the following :
*!          - If there is a negative value , we will make it 0 and subtract
*!            it from the nearst next period has a POSITIVE value.
*! NOTE : Called from ORD970.PRG
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Modifications :
*!      YMA 06/20/94
*!          1) Modified the posting techinque of the -ve numbers to
*!             allow the remainning -ve number after posting all the
*!             -ve numbers to be displaied in the last period of the
*!             report.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE POST_NVE_VALUS

XON_HAND = 0
FOR XSIZE_NO = 1 TO XSIZE_CNT
  FOR XPER = 1 TO 10
    IF STY_INFO [XSIZE_NO,XPER] < 0    && -VE NUMBER
      XON_HAND = STY_INFO [XSIZE_NO,XPER]
      STY_INFO [XSIZE_NO,XPER] = 0
      IF XPER = 1
        DO POST_BF WITH 2,10                     && GO_BACKWORD   ->
      ELSE
        DO POST_BF WITH XPER-1,1                 && GO_BACKWORD   <-
        IF (XON_HAND < 0) .AND. (XPER < 10)
          DO POST_BF WITH XPER+1,10              && GO_FOREWORD   ->
        ENDIF
      ENDIF

    ENDIF   && ---------------------->     -VE NUMBER
  ENDFOR
  *-- YMA 06/20/94 To put the remainning -ve quantity in the last period.
  IF XON_HAND < 0
    STY_INFO [XSIZE_NO,10] = XON_HAND
  ENDIF
  XON_HAND = 0
ENDFOR
* ------------------ END OF POST_NVE_VALUS PROCEDURE --------------- *


*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! PROG : POST_BF
*! DATE : FEB 02,94
*! AUTH : YASSER MOHAMMED ALY (YMA)
*! DESC : TO DO THE POSTING EITHER BACK WORD OR FOR WORD.
*! NOTE : Called from POST_NVE_VALUS
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE POST_BF
PARAMETERS INIT_PER , END_PER

X_LLOOP = IIF (INIT_PER > END_PER , 'INIT_PER TO END_PER  STEP -1', ;
                                    'INIT_PER TO END_PER'           )
FOR X_XPER = &X_LLOOP
  XON_HAND =  XON_HAND + STY_INFO [XSIZE_NO,X_XPER]
  IF XON_HAND <= 0
    STY_INFO [XSIZE_NO,X_XPER] = 0
  ELSE
    STY_INFO [XSIZE_NO,X_XPER] = XON_HAND
    XON_HAND = 0
    EXIT
  ENDIF
ENDFOR
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴



*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! PROG : DISP_OTS_LINES
*! DATE : JAN 24,94
*! AUTH : YASSER MOHAMMED ALY (YMA).
*! DESC : PRINT THE OTS QUANTITIES.
*! NOTE : CALLED FROM ORD970.PRG
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE DISP_OTS_LINES

*-- YMA 05/29/94 To Accumulate the OTS quantities.
IF !llRpSizes
  FOR XPER = 1 TO 12
    FOR XSIZE_NO = 1 TO XSIZE_CNT
      STY_INFO [XSIZE_CNT+1,XPER] = STY_INFO [XSIZE_CNT+1,XPER] + ;
                                    STY_INFO [XSIZE_NO,XPER]
    ENDFOR
  ENDFOR
ENDIF

* -- Loop to accumulate the the OTS lines and total to the 11th col.
* -- in the array .
FOR XSIZE_NO = 1 TO XSIZE_CNT+IIF (llRpSizes,0,1)
  *B120121,1 MMM 11/13/2003 Fix Bug of not Printing the OTS for each period IF the setting of 
  *                         OTS based on exact transaction date = 'Yes' [Start]
  *FOR XPER = 1 TO 10
  *  STY_INFO [XSIZE_NO,11] = STY_INFO [XSIZE_NO,11] + ;
                             STY_INFO [XSIZE_NO,XPER]
  *ENDFOR
  IF !llOTSbasTr
    FOR XPER = 1 TO 10
      STY_INFO [XSIZE_NO,11] = STY_INFO [XSIZE_NO,11] + ;
                               STY_INFO [XSIZE_NO,XPER]
    ENDFOR
  ELSE
    STY_INFO [XSIZE_NO,11] = STY_INFO[XSIZE_NO,10] 
  ENDIF   
  *B120121,1 MMM [End]        
ENDFOR

XFIRST_TIME = .T.    && Flag to avoid printing the color more than once.

* -- If the report format is by size scale the following will be done
* -- for each color ...
IF llRpSizes
  * -- loop around the Two dimensional array "STY_INFO" to print the
  * -- OTS line for each [color\size] .

  * -- This is the outer loop [sizes]
  FOR  XSIZE_NO = 1 TO XSIZE_CNT

    *-- Added by TAK 05/25/94
    IF !llRpZeroAv
     IF STY_INFO [XSIZE_NO,11] = 0 .AND. ;
        STY_INFO [XSIZE_NO,12] = 0
        LOOP
      ENDIF
    ENDIF

    IF llPrintSty
      *-- Print the style and the style description.
      @ ROW,00 SAY XSTYLE
      @ ROW,lnDescSt SAY XDESC
      llPrintSty =.F.
      ROW = ROW + 1
    ENDIF
    *--End TAK 05/25/94 ---

    * -- Print the size .
    XSIZE = SUBSTR (XSCALES,((XSIZE_NO-1)*5)+1,5)          && TMI 12/01/94

    @ ROW,lnSzStart SAY XSIZE 

    * -- If we have not print the color then please print it.
    IF XFIRST_TIME
      @ ROW,lnClrStart SAY XCOLOR
      lcClrDesc = PADR(gfCodDes(XCOLOR , 'COLOR'  ),lnTitlLen+3)
      @ ROW,lnDescSt SAY lcClrDesc
      XFIRST_TIME = .F.
    ENDIF

    * -- This is the inner loop [Periods].
    XCOL  = 36
    FOR XLOOP = 1 TO 12
      XCOL = IIF ( XLOOP = 12 , XCOL+1 , XCOL )
      @ ROW,XCOL SAY STY_INFO [XSIZE_NO,XLOOP] PICTURE '9999999'
      XCOL = XCOL + 8

      * -- To add this line to the "XTOTAL" array .
      XTOTAL [XLOOP] = XTOTAL [XLOOP] + STY_INFO [XSIZE_NO,XLOOP]
    ENDFOR
    ROW = ROW + 1
  ENDFOR

ELSE

  * -- If the report format is not by size the following will be done
  * -- for each color ....

  *-- Added by TAK 05/25/94
  IF !llRpZeroAv
   IF STY_INFO [XSIZE_CNT+1,11] = 0 .AND. ;
      STY_INFO [XSIZE_CNT+1,12] = 0
      RETURN
    ENDIF
  ENDIF

  IF llPrintSty
    *-- Print the style and the style description.
    @ ROW,00 SAY XSTYLE
    @ ROW,lnDescSt SAY XDESC
    llPrintSty =.F.
    ROW = ROW + 1
  ENDIF
  *--End TAK 05/25/94 ---

  * -- Print the color and the color description.
  @ ROW,lnClrStart SAY XCOLOR
  lcClrDesc = PADR(gfCodDes(XCOLOR , 'COLOR'  ),lnTitlLen+3)
  @ ROW,lnDescSt SAY lcClrDesc
  XCOL  = 36

  * -- Loop around the "STY_INFO" array to print a single line
  * -- ( Total sizes for one color ) .
  FOR XLOOP = 1 TO 12
    * -- To print the quantities for this color.
    XCOL = IIF ( XLOOP = 12 , XCOL+1 , XCOL )
    @ ROW,XCOL SAY STY_INFO [XSIZE_CNT+1,XLOOP] PICTURE '9999999'
    XCOL = XCOL + 8

    * -- To accumulate the quantities in "XTOTAL" array.
    XTOTAL [XLOOP] = XTOTAL [XLOOP] + STY_INFO [XSIZE_CNT+1,XLOOP]
  ENDFOR

  ROW = ROW + 1
ENDIF

* -------------------- END OF DISP_OTS_LINES PROCEDURE ---------------- *

* 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 
* PROG : lfGtYTD()
* DESC : Calculate ordered and shipped quantites.
*B500568,1 A new function to compute the field Total Sold year 
*B500568,1 to date for the current fiscal year only.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 
FUNCTION lfGtYTD
PRIVATE lnAlias,lcCentuary
lnAlias = SELECT()
lcCentuary = SET('CENTURY')
SET CENTURY ON
 
IF SEEK (lcFullSty,'ORDLINE')
  SELECT ORDLINE
  SUM REST  Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 ;
      WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcFullSty ;
      FOR   OrdHdr.Status $ 'OH' .AND. gcCurrYear = STR(YEAR(OrdHdr.Entered),4) ;
      TO    lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8
ENDIF

IF 'AR' $ gcCmpModules AND SEEK (lcFullSty,'INVLINE')
  SELECT INVLINE
  SUM REST  Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 ;
      WHILE style+invoice+STR(lineno,6) = lcFullSty ;
      FOR   INVHDR.STATUS <> 'V' .AND. gcCurrYear = STR(YEAR(INVDATE),4) ;
      TO    lnShp1,lnShp2,lnShp3,lnShp4,lnShp5,lnShp6,lnShp7,lnShp8
ENDIF

SET CENTURY &lcCentuary
SELECT (lnAlias)
RETURN (.T.)
* ----------------------- END OF lfGtYTD FUNCTION -------------------- *


!*************************************************************
*! Name      : lpUpdOTS      
*! Developer :Mostafa Mahmoud Rawash (MMM)
*! Date      : 11/13/2003
*! Purpose   : Update OTS qty. in each period.
*!*************************************************************
*! Calls     :         
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : 
*!*************************************************************
*B120121,1 MMM 11/13/2003  Fix Bug of not Printing the OTS for each period IF the setting of 
*                          OTS based on exact transaction date = 'Yes' 
PROCEDURE lpUpdOTS         

FOR XSIZE_NO = 1 TO XSIZE_CNT
  lnCurrent = STY_INFO [XSIZE_NO,1]  
  FOR I = 2 TO 10
    STY_INFO [XSIZE_NO,I] = STY_INFO [XSIZE_NO,I] + lnCurrent
    lnCurrent = STY_INFO[XSIZE_NO,I]
  ENDFOR  
ENDFOR

* -------------END OF lpUpdOTS Procedure ----------------- *

