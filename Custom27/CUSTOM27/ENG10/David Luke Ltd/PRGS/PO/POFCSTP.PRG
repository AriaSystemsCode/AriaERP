*!****************************************************************
*! Program file  : POFRCSTP.PRG
*! Program desc. : FORECAST Planner program
*! For screen    : POFRCSTP.SPX
*! System        : Aria Apparel System - Version 2.7.
*! Module        : Style Purchased Order (PO)
*! Developer     : NADER NABIL (NNA)
*! Date          : 06/22/2004
*!****************************************************************
*! Passed Parameters  : None.
*!****************************************************************
*!C#122961,1.
*!Related to     : C#200569
*!****************************************************************
*!Modifications  :
*!****************************************************************

EXTERNAL ARRAY laData , laDefProc , laKeyField
DECLARE laShowFig[12,8],laAllData[137,1],laWareType[2] , laDataPrc[137,1]
STORE ''  TO lcStyDesc, lcMjrPct, lcClrDesc, lcScFields , lcColStyle
STORE ''  TO lcPrvSclS, lcNxtSclS, lcPrvMnthS, lcNxtMnthS
STORE 0   TO lnYear , lnMonth , lnPurAdjPc , lnSalAdjPc , lnTSalFcst , lnTPurFcst
STORE 0   TO lnAvgCost , lnPriceA , lnSepMins , lnTBkAmt , lnTPosMnth , lnSetDeci
STORE 0   TO lnRowCnt  , lnPrStock
lnYear   = YEAR(gdSysDate)
lnMonth  = MONTH(gdSysDate)
STORE .F. TO llbrowse , llMScale
STORE '' TO  lcMonth, lcYear 
STORE 0  TO laShowFig , laAllData , laDataPrC
laWareType[1] = 'Single'
laWareType[2] = 'Multiple'
llMScale      = gfGetMemVar('M_USEEXSSC')
STORE 2   TO lnScaleCnt
STORE 3   TO lnXPos
STORE 3   TO lnYPos
STORE 7   TO lnZPos           && the Movement Step From Month to another inside the array
STORE ''  TO lcVendor,lcWareCode,lcPCurr,lcDCurr,lcIType1,lcIType2,lcIType3,lcIType4,lcIType5
lcTmpFrCst = gfTempName()
lcTmpIndex = gfTempName()
lcTmpStyle = gfTempName()
lcTmpStyl1 = gfTempName()

*-- Hold the unit of the foreign curr.
STORE  1  TO lnUnit1,lnUnit2,lnPRate,lnDRate

*-- Hold the number of term,ship,term
STORE  1  TO lnTerm,lnShip,lnType,puType

lnSetDeci = SET('DECIMALS')
SET DECIMALS TO 5
*--ToolBar external proc.
laDefProc[1]  = .F.              && Disable the control panel prev proc.(lpTopScr)
laDefProc[2]  = .F.              && Disable the control panel prev proc.(lpBtmScr)
laDefProc[3]  = .F.              && Disable the control panel prev proc.(lpNxtScr)
laDefProc[4]  = .F.              && Disable the control panel prev proc.(lpPrvScr)
laDefProc[10] = .F.              && Disable the control panel Close proc.(lpClsScr)
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)

DIMENSION laTerm[1], laCodInfo [2,10] ,laShip[1]
STORE "" TO laTerm, laCodInfo, laShip

STORE .F. TO llAlowNew
llNoShow = .F.            && Flag to make the screen call the PROCEDURE lpShow evry time it run

*-- To get the Color position and length.
STORE 0 TO lnClrPos,lnClrLen
lcMjrPct  = gfItemMask('PM')
lnMjrWid  = LEN(lcMjrPct)


lnSepMins = lfvGtSclSp()           && the length of Scale with Separator.
=lfGetColor()
DECLARE laKeyField [2,4]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'STYLE'
laKeyField[1,4] = 1

laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'STYLE'
laKeyField[2,4] = 2


llWareHous = ALLTRIM(gfGetMemVar('M_WareHouse'))= 'Y'
llEditExRt = gfGetMemVar('LLEDITEXRA')
llMulCurr  = gfGetMemVar('llMulCurr')
lcIType1   = gfGetMemVar('M_cIType1')
lcIType2   = gfGetMemVar('M_cIType2')
lcIType3   = gfGetMemVar('M_cIType3')
lcIType4   = gfGetMemVar('M_cIType4')
lcIType5   = gfGetMemVar('M_cIType5')

*-- To get the include pattern No setup.
llUpdPatrn = gfGetMemVar('M_ADDPATNO')


IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(lcBaseWind)
  lcYear  = STR(YEAR(gdSysDate),4,0)
  lcMonth = CMONTH(gdSysDate)
  FOR lnI = 2 TO 12
    FOR lnJ = 1 TO 8
      laShowFig[lnI,lnJ] = 0
    ENDFOR
  ENDFOR


 *-- Create temp cursor to view/edit/add records in the POTOBUY file.
 SELECT FORECAST
 =AFIELDS(laFileStru)

 CREATE CURSOR (lcTmpFrCst) FROM ARRAY laFileStru
 INDEX ON style+STR(nyear,4)+STR(nmonth,2) TAG (lcTmpFrCst) OF (lcTmpFrCst)
 INDEX ON STR(nyear,4)+STR(nmonth,2)+style TAG (lcTmpIndex) OF (lcTmpFrCst)
 SELECT (lcTmpFrCst)
 SET ORDER TO (lcTmpFrCst)
  
 *-- Create temp cursor to Browse styles for generate POs.
  CREATE CURSOR (lcTmpStyle) (CSELECT C(1),STYLE C(19),COLOR C(6),NTOTQTY N(10,0),;
                              NMONTH N(2,0),NYEAR N(4,0),CADD_USER C(10),DADD_DATE D)
  INDEX ON STYLE+COLOR+STR(NYEAR,4,0)+STR(NMONTH,2,0) TAG (lcTmpStyle) OF (lcTmpStyle)
  INDEX ON CSELECT+STR(NYEAR,4,0)+STR(NMONTH,2,0)+STYLE+COLOR TAG (lcTmpStyl1) OF (lcTmpStyle)
  SELECT (lcTmpStyle)
  SET ORDER TO TAG (lcTmpStyle)
  =lfGetSelSt()

  *-- Screen fields variable, to be used to build the laData array.
  SELECT YEARINPO
  lcScFields ='STYLE,COLOR'

  SCATTER FIELDS &lcScFields. TO laData BLANK

ENDIF 
*--Activate Options pad.
=lfActPad()

DO (gcScrDir+gcWinAppl+"\POFCSTP.SPX")

IF glQuitting
  USE IN (lcTmpFrCst)
  USE IN (lcTmpStyle)
ENDIF

*--Release Option pad also
RELEASE PAD _Option OF _MSYSMENU
*-- End OF Code.

*!*************************************************************
*! Name      : lfvStyle
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Valid function to validate style field.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : gfStyBrw()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*!C#122961,1
FUNCTION lfvStyle

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT STYLE
SET ORDER TO TAG CSTYLE

llbrowse = llbrowse OR '?' $ laData[1]
IF llbrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK(laData[1],'STYLE'))
  IF !llbrowse .AND. !EMPTY(laData[1]) .AND. !SEEK(laData[1],'STYLE')
    *-- give user message. to browse or reenter.
    IF gfModalGen("QRM00000B42014",.F.,.F.,.F.,'Style : '+laData[1]+'is not found in the data file' ) = 2
      laData[1] = ''
      _CUROBJ = OBJNUM(laData[1])
      RETURN
    ENDIF
  ENDIF
  llbrowse = .F.
  laData[1] = gfStyBrw('M',laData[1],"",.F.)
ENDIF
SELECT STYLE
SET ORDER TO TAG STYLE

SELECT (lnAlias)
*-- End OF lfvStyle.

*!*************************************************************
*! Name      : lfvColor
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Valid function to validate Color.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*!C#122961,1
FUNCTION lfvColor

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT STYLE
llbrowse = llbrowse OR '?' $ laData[2]

laData[2] = IIF(llbrowse,'?',laData[2])

IF EMPTY(laData[2])
  RETURN
ENDIF

IF llBrowse .OR. !MDOWN()
  llCanBrow = .T.
  IF !EMPTY(laData[1]) .AND. SEEK(PADR(laData[1],lnMjrWid),'STYLE')
    LOCATE REST WHILE STYLE = PADR(laData[1],lnMjrWid);
                  FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
    IF FOUND()
      llCanBrow = .F.
    ENDIF
  ELSE
    LOCATE
  ENDIF
  
  IF llCanBrow
    IF !EMPTY(laData[1])
      lcItemCd  = gfStyBrw('N',PADR(laData[1],lnMjrWid),laData[2],.F.)
      laData[2] = SUBSTR(lcItemCd ,lnClrPos-lnMjrWid-1,lnClrLen)
    ELSE
      lcItemCd = gfStyBrw('I','','',.F.)
      laData[1]  = SUBSTR(lcItemCd ,1,lnMjrWid)
      laData[2]  = SUBSTR(lcItemCd ,lnClrPos,lnClrLen)
    ENDIF
  ENDIF
  llBrowse = .F.
ENDIF

IF !EMPTY(laData[2])
  lcStyDesc  = Style.DESC1
  lcClrDesc  = gfCodDes(laData[2], 'COLOR     ')
  laData[1]  = PADR(laData[1],lnMjrWid)
  laScrMode   = .F.
  laScrMode[2]= .T.
  SHOW GETS
  SHOW GET laData[1] DISABLE
  SHOW GET laData[2] DISABLE
  SHOW GET ibStyle   DISABLE
  SHOW GET ibColor   DISABLE
ENDIF
=lfRefresh()
llBrowse = .F.
SELECT(lnAlias)
RETURN
*-- End OF lfvColor.

*:*************************************************************
*: Name      : lfOldValue
*: Developer : NADER NABIL (NNA)
*: Date      : 06/22/2004
*: Purpose   : Function to store old value of the current filed.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfoldvalue()
*:*************************************************************
*:C#122961,1
FUNCTION lfoldvalue

*-- To disable the Total filed
IF lnXPos + VAL(LEFT(RIGHT(SYS(18),2),1)) - 1 >= lnScaleCnt
  RETURN .F.
ENDIF
*-- To disable The Sales Adj. and The Purchase Adj. if we arn't in the First Page of Scale
IF lnXPos > 3 AND SYS(18) = 'LNSALADJPC' 
  RETURN .F.
ENDIF
IF lnXPos > 3 AND  SYS(18) = 'LNPURADJPC'
  RETURN .F.
ENDIF


lnOldValue = EVALUATE(SYS(18))
*-- End of lfoldvalue.


*!*************************************************************
*! Name      : lpShow
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Show function.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
*!C#122961,1
FUNCTION lpShow

DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]
    STORE ''         TO laData[1] , laData[2] , lcStyDesc , lcClrDesc
    STORE 'DISABLE'  TO lcPrvSclS, lcNxtSclS, lcPrvMnthS, lcNxtMnthS
    STORE SPACE(0)   TO laShowFig, laAllData , laDataPrC

    FOR lnI = 2 TO 12
      FOR lnJ = 1 TO 8
        laShowFig[lnI,lnJ] = 0
      ENDFOR
    ENDFOR
    SHOW GET ibStyle    ENABLE
    SHOW GET ibColor    ENABLE
    SHOW GET pbPrvsScl  &lcPrvSclS.
    SHOW GET pbNxtScl   &lcNxtSclS.
    SHOW GET pbPrvsMnth &lcPrvMnthS.
    SHOW GET pbNxtMnth  &lcNxtMnthS.
    FOR lnI = 1 TO 8
      SHOW GET laShowFig[3,lnI] DISABLE      && Sales Forecast Fields
      SHOW GET laShowFig[6,lnI] DISABLE      && Purchase Forecast Fields
    ENDFOR
    SHOW GET lnSalAdjPc         DISABLE      && Act. Sales/Forecast YTD% Field
    SHOW GET lnPurAdjPc         DISABLE      && Act. Purchase/Forecast YTD% Field

    =lfRefresh()
  
  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]
    lcStyDesc  = Style.DESC1
    lcClrDesc  = gfCodDes(laData[2], 'COLOR     ')
    
    *-- recollect the data again.
    =lfCollect()
    =lfShowFig()
    
    SHOW GET ibStyle    DISABLE
    SHOW GET ibColor    DISABLE
    SHOW GET lcStyDesc

  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]
    SHOW GET ibStyle    DISABLE
    SHOW GET ibColor    DISABLE
    SHOW GET pbPrvsScl  &lcPrvSclS.
    SHOW GET pbNxtScl   &lcNxtSclS.
    SHOW GET pbPrvsMnth &lcPrvMnthS.
    SHOW GET pbNxtMnth  &lcNxtMnthS.
    FOR lnI = 1 TO 8
      SHOW GET laShowFig[3,lnI] ENABLE    && Sales Forecast Fields
      SHOW GET laShowFig[6,lnI] ENABLE    && Purchase Forecast Fields  
    ENDFOR
    SHOW GET lnSalAdjPc       ENABLE      && Act. Sales/Forecast YTD% Field
    SHOW GET lnPurAdjPc       ENABLE      && Act. Purchase/Forecast YTD% Field

    laData[2] = SUBSTR(laData[1],lnClrPos,lnClrLen)
    laData[1] = PADR(laData[1],lnMjrWid)
  
  ***--- A D D   M O D E ---***
  CASE laScrMode[4]
    *-- recollect the data again.
    =lfCollect()
    =lfShowFig()
    
    SHOW GET ibStyle    DISABLE
    SHOW GET ibColor    DISABLE

ENDCASE
SHOW GET pbUsrFields DISABLE
SHOW GET pbNotePad   DISABLE
SHOW GET pbAudTrail  DISABLE
=lfChkLstRc()
*-- End OF lpShow.

*!*************************************************************
*! Name      : lfCollect
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to collect the data Temp Files.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfCollect ()
*!*************************************************************
*!C#122961,1
FUNCTION lfCollect

PRIVATE lnAlias,lnNewScale,lnI,lcI,lnJ,lnMonthRow,laOrder,laWip,lnSclCnt,lnRecNo
PRIVATE laTotOrder,laTotWip
lnAlias  = SELECT(0)
lnRecNo  = RECNO('STYLE')
lnRowCnt = (25 - lnMonth) * 7
lnXPos   = 3
lnYPos   = 3
*--Get the position for the last month(12) of current year
lnZPos   = lnRowCnt - 88       

SELECT (lcTmpFrCst)
ZAP
  
IF laScrMode[2]
  *-- Get the save data.
  SELECT FORECAST
  SET ORDER TO TAG FORECAST
  
  *--NNA (BEGIN) 11/11/2004 Add all Records related to the process style, that by seeking for 
  *--NNA         the (style - Scale) for all Months in the current and next year to give the 
  *--NNA         ability to add or change any values for the Sales and Purchasing Forecast.
  *IF SEEK(STYLE.STYLE+STR(lnYear,4)+STR(lnMonth,2))
  *  lcColStyle = SUBST(laData[1],1,lnMjrWid) + IIF(lnClrPos >(lnMjrWid+1) , '-' + laData[2] ,laData[2])
  *  SCAN REST WHILE Style+STR(nyear,4)+STR(nmonth,2) = lcColStyle FOR NYEAR = lnYear
  *    SCATTER MEMVAR
  *    INSERT INTO (lcTmpFrCst) FROM MEMVAR
  *  ENDSCAN
  *ENDIF

  lcColStyle = SUBST(laData[1],1,lnMjrWid) + IIF(lnClrPos >(lnMjrWid+1) , '-' + laData[2] ,laData[2])
  IF SEEK(lcColStyle)
    SCAN REST WHILE Style+STR(nyear,4)+STR(nmonth,2) = lcColStyle FOR NYEAR >= lnYear
      SCATTER MEMVAR
      INSERT INTO (lcTmpFrCst) FROM MEMVAR
    ENDSCAN
  ENDIF
  *--NNA (END)
  
  SET ORDER TO TAG FORECASTY
  = SEEK(STYLE.STYLE)
ENDIF

*-- Collect data for all sizes and all months
STORE 2 TO lnScaleCnt,lnSclCnt
=lfGetScale()
DECLARE laAllData[lnRowCnt+2,lnScaleCnt],laDataPrc[lnRowCnt+2,4]
STORE 0 TO laDataPrc,laAllData
FOR lnI = 1 TO lnRowCnt  && loop for add year and month to the first two columns in the array.
  laAllData[lnI+2,1] = lnYear + INT((lnMonth + INT((lnI-1)/7) - 1)/12)
  laAllData[lnI+2,2] = MOD((lnMonth + INT((lnI-1)/7) - 1),12) + 1
ENDFOR
SELECT STYLE
IF SEEK(PADR(laData[1],lnMjrWid))
  lnAvgCost = Ave_Cost
  lnPriceA  = PriceA
  SCAN REST WHILE ALLTRIM(CSTYMAJOR) == ALLTRIM(laData[1]);
              FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
  
    IF SEEK('S'+SCALE,'SCALE')
      lnRecNo1 = RECNO('STYLE')
      lnNewScale = lnSclCnt + 1
      lnSclCnt   = lnSclCnt + SCALE.CNT
      FOR lnJ = 3 TO lnRowCnt+2  && loop for all rows of months.
        lnMonthRow = MOD(lnJ-3,7) + 1 && get the row number relative to month
        FOR lnI = lnNewScale TO lnSclCnt
          lcI = STR(lnI - lnNewScale + 1,1)
          laAllData[1,lnI] = SCALE + lcI  && add the scale code to the first row in the array.
          laAllData[2,lnI] = EVALUATE('SCALE.SZ'+lcI)  && add the size desc. to the snd. row.
        
          DO CASE
            CASE lnMonthRow = 1  && Opening Stock.
                =lfGetStock(lnJ,lnI)

            CASE lnMonthRow = 2  && Sales ForeCast.
                =lfGetSFCst(lnJ,lnI)

            CASE lnMonthRow = 3  && Last Years Actual Sales.
                =lfGetSales(lnJ,lnI)
            
            CASE lnMonthRow = 4  && Act. Sales and Purchase / Forecast YTD% 
               =lfGetPrc(lnJ,lnI)

            CASE lnMonthRow = 5  && Purchase ForeCast.
               =lfGetPFCst(lnJ,lnI)

            CASE lnMonthRow = 6  && Purchase/Forecast YTD% 
                *--Included With lnMonthRow = 4
            
            CASE lnMonthRow = 7  && Closing Stock.
                =lfGetClose(lnJ,lnI)
                
          ENDCASE
        ENDFOR
      ENDFOR
      GO lnRecNo1 IN STYLE
    ENDIF
  ENDSCAN
ENDIF

*-- Add the total column.
laAllData[1,lnScaleCnt] = 'TOTAL'
laAllData[2,lnScaleCnt] = 'TOTAL'

lnMnthRow = 0
I = 0
  FOR lnI = 1 TO lnRowCnt  && loop for add totals to the last column in the array.
   *-- To get the array Row When the Month change
   IF laAllData[lnI+2,2] <> lnMnthRow
      lnMnthRow = laAllData[lnI+2,2]
      I         = lnI + 2
   ENDIF
   *-- to not accumulate the Act. Sales/Forecast YTD% Or Act. Purchase/Forecast YTD%
   IF !(((lnI + 2) = (I + 3)) OR ((lnI + 2) = (I + 5)))
        laAllData[lnI+2,lnScaleCnt] = 0
        FOR lnJ = 3 TO lnScaleCnt - 1
          laAllData[lnI+2,lnScaleCnt] = laAllData[lnI+2,lnScaleCnt] + laAllData[lnI+2,lnJ]
        ENDFOR
   ENDIF
ENDFOR

GO lnRecNo IN STYLE
SELECT (lnAlias)
*-- End OF lfCollect

*!*************************************************************
*! Name      : lpSavScr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Save/Update.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C#122961,1
PROCEDURE lpSavScr

PRIVATE lnAlias , lnRecNo , lnSalVal , lnPurVal
STORE 0 TO lnSalVal , lnPurVal
lnAlias = SELECT(0)
lnRecNo = RECNO('FORECAST')

SELECT (lcTmpFrCst)
SCAN
  SCATTER MEMVAR
  m.cAdd_User = gcUser_id
  m.cAdd_Time = TIME()
  m.dAdd_Date = gdSysDate
  IF SEEK(STR(NYEAR,4,0) + STR(NMONTH,2,0) + STYLE ,'FORECAST')
    SELECT FORECAST
    GATHER MEMVAR
  ELSE
    FOR A = 1 TO 8
      lcSiz = STR(A,1)
      lnSalVal = lnSalVal + NSALFRCST&lcSiz
      lnPurVal = lnPurVal + NPURFRCST&lcSiz      
    ENDFOR
    IF lnSalVal > 0 .OR. lnPurVal > 0
      INSERT INTO FORECAST FROM MEMVAR
    ENDIF
    STORE 0 TO lnSalVal , lnPurVal    
  ENDIF
ENDSCAN
IF BETWEEN(lnRecNo,1,RECCOUNT('FORECAST'))
  GOTO lnRecNo IN FORECAST
ENDIF
=lfGetSelSt()
SELECT (lnAlias)
*-- End Of lpSavScr.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
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
*C#122961,1
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
*-- End of lfGetColor.
*!*************************************************************
*! Name      : lfGetSelSt
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Get the style to generate POs
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetSelSt()
*!*************************************************************
*!C#122961,1
FUNCTION lfGetSelSt

SELECT (lcTmpStyle)
ZAP

SELECT POTOBUY
SET ORDER TO TAG POTOBUYL
SCAN FOR NMONTH # 13 .AND. NTOBUYQTY > 0 .AND.;
         ((NMONTH >= lnMonth .AND. NYEAR = lnYear) .OR. NYEAR = lnYear + 1)
  SCATTER FIELDS STYLE,COLOR,NTOBUYQTY,NYEAR,NMONTH,CADD_USER,DADD_DATE MEMVAR
  SELECT (lcTmpStyle)
  IF !SEEK(m.Style+m.Color+STR(m.nYear,4,0)+STR(m.nMonth,2,0))
    APPEND BLANK
    GATHER FIELDS STYLE,COLOR,NYEAR,NMONTH,CADD_USER,DADD_DATE MEMVAR
  ENDIF
  REPLACE NTOTQTY WITH NTOTQTY + m.nToBuyQty
ENDSCAN
SET ORDER TO TAG POTOBUY
llCanGenPo = (RECCOUNT(lcTmpStyle)>0)
*-- end of lfGetSelSt.

*!*************************************************************
*! Name      : lfGetStock
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to Get the Opening Stock.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lnRow,lnCol
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfGetStock (lnJ,lnI)
*!*************************************************************
*!C#122961,1
FUNCTION lfGetStock
PARAMETERS lnRow,lnCol
PRIVATE lnAlias , lnRecNo , lnCloseStk , lnSalFrcst , lnPurFrcst , lcColStyle
*-- lnCloseStk = nCloseStk Field from the Yearinpo file for this year,style and scale size
*-- lnSalFrcst = nSalFrcst Field from the ForeCast file for this year,style and all the Months
*--              before the displayed Monthes
*-- lnPurFrcst = nPurFrcst Field from the ForeCast file for this year,style and all the Months
*--              before the displayed Monthes

STORE 0 TO lnCloseStk , lnSalFrcst , lnPurFrcst
STORE 0 TO lcColStyle

lnAlias  = SELECT(0)
lnRecNo  = RECNO('YEARINPO')

IF laAllData[lnRow,1] = lnYear .AND. laAllData[lnRow,2] = lnMonth
  IF SEEK(STR(lnYear,4,0)+PADR(laData[1],19)+laData[2]+laAllData[1,lnCol],'YEARINPO')
    lnCloseStk = YEARINPO.NCLOSESTK
    IF SEEK(STR(lnyear,4) + STR(lnmonth-1,2) + STYLE.STYLE,'FORECAST')
      SELECT FORECAST
      LOCATE
      IF lnMonth > 1
        SET ORDER TO TAG FORECAST
        lcColStyle = SUBST(laData[1],1,lnMjrWid) + IIF(lnClrPos >(lnMjrWid+1) , '-' + laData[2] ,laData[2])
        lcColStyle = lcColStyle + IIF(lnSepMins = 3 , SUBST(LAALLDATA[1,LNCOL],1,3) +;
                     SPACE(1) , '-' +  SUBST(LAALLDATA[1,LNCOL],1,3))
        SCAN REST WHILE Style+STR(nyear,4)+STR(nmonth,2) = lcColStyle+STR(laAllData[lnRow,1],4)
          IF nMonth < lnMonth 
            lnSalFrcst = lnSalFrcst + nSalFrcst&LCI
            lnPurFrcst = lnPurFrcst + nPurFrcst&LCI
          ENDIF
        ENDSCAN
        SET ORDER TO TAG FORECASTY       
        laAllData[lnRow,lnCol] = (lnCloseStk - lnSalFrcst) + lnPurFrcst
      ELSE
        laAllData[lnRow,lnCol] = lnCloseStk
      ENDIF
    ELSE
      laAllData[lnRow,lnCol] = lnCloseStk
    ENDIF
  ELSE
    laAllData[lnRow,lnCol] = 0
  ENDIF
ELSE
  laAllData[lnRow,lnCol] = laAllData[lnRow-1,lnCol]
ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT('ORDLINE'))
  GOTO lnRecNo IN ('ORDLINE')
ENDIF
SELECT (lnAlias)

*-- End of lfGetStock.

*!*************************************************************
*! Name      : lfGetSales
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to Get the Last Years Actual sales For 
*!           : the Month Being Processed
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lnRow,lnCol
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfGetSales (lnJ,lnI)
*!*************************************************************
*!C#122961,1
FUNCTION lfGetSales
PARAMETERS lnRow,lnCol
lcField = 'YEARINPO.NBKAMT'
lcField = lcField + STRTRAN(STR(laAllData[lnRow,2],2,0),' ','0')

*-- Search about Book amount for the previous Year
IF SEEK(STR(laAllData[lnRow,1]-1,4,0)+PADR(laData[1],19)+laData[2]+laAllData[1,lnCol],'YEARINPO')
  laAllData[lnRow,lnCol] = EVALUATE(lcField)
ELSE
  laAllData[lnRow,lnCol] = 0
ENDIF
*-- End of lfGetSales.

*!*************************************************************
*! Name      : lfGetClose
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to Get the Closing Stock.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lnRow,lnCol
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfGetClose (lnJ,lnI)
*!*************************************************************
*!C#122961,1
FUNCTION lfGetClose
PARAMETERS lnRow,lnCol
*-- Closing Stock      = (Opening Stock - Sales Forecast) + Purchase Forecast
*-- I used ROUND() function because if the user used the Sales Adj. Prc sometimes it get decimal
*-- digits and this get incorrect total so i Upper the No. To Up if the Deci. >=5
laAllData[lnRow,lnCol] = (ROUND(laAllData[lnRow-6,lnCol],0)  - ;
                          ROUND(laAllData[lnRow-5,lnCol],0)) + ;
                          ROUND(laAllData[lnRow-2,lnCol],0) 
*-- End of lfGetClose.

*!*************************************************************
*! Name      : lfLstMnDay
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to Get the Last Day of the month.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lnForMonth,lnForYear
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfLstMnDay(1,2002)
*!*************************************************************
*!C#122961,1
FUNCTION lfLstMnDay
PARAMETERS lnForMonth,lnForYear

IF lnForMonth = 12
  lnForMonth = 1
  lnForYear  = lnForYear + 1
ELSE
  lnForMonth = lnForMonth + 1
ENDIF
ldLstMnDay = CTOD(STRTRAN(STR(lnForMonth,2,0),' ','0')+'/01/'+STR(lnForYear,4,0)) - 1
RETURN ldLstMnDay
*-- End of lfLstMnDay.

*!*************************************************************
*! Name      : lfGetSFCst
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to Get the Sales ForeCast.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lnRow = Row (4) in the Screen and the array
*!                     Row(1) for sizes & Row (2) for Months
*!                     Row(3) for Opning Stock
*!           : lnCol = Size No ( from 1 to 8)
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfGetSFCst()
*!*************************************************************
*!C#122961,1
FUNCTION lfGetSFCst
PARAMETERS lnRow,lnCol
PRIVATE lnAlias
lnAlias = SELECT(0)
*-- IF SEEK ( YEAR               + MONTH                     + STYLE)
IF SEEK(STR(laAllData[lnJ,1],4,0)+ STR(laAllData[lnJ,2],2,0) + STYLE.STYLE,'FORECAST')
  SELECT FORECAST
  laAllData[lnRow,lnCol] = nSalFrCst&LCI
ELSE
  laAllData[lnRow,lnCol] = 0
ENDIF
SELECT (lnAlias)
*-- End of lfGetSFCst.

*!*************************************************************
*! Name      : lfShowFig
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to Show the current sizes and month.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfShowFig()
*!*************************************************************
*!C#122961,1
FUNCTION lfShowFig

PRIVATE lnI,lnJ,lnRow,lnCol,lcAbleSts
lcAbleSts = IIF(laScrMode[2],'DIS','EN') + 'ABLE'

FOR lnI = 1 TO 8
  lnCol = lnXPos+lnI-1
  FOR lnJ = 1 TO 12
    DO CASE
      CASE lnJ = 1           && sizes
        lnRow = lnJ+1
      CASE BETWEEN(lnJ,2,10) && Month Data
        lnRow = lnYPos+lnJ-2

    ENDCASE
    IF lnJ < 9
      laShowFig[lnJ,lnI] = IIF(lnScaleCnt<lnCol,'',laAllData[lnRow,lnCol])
    ELSE
      IF lnJ = 9
        laShowFig[lnJ,lnI] = IIF(lnScaleCnt<lnCol,'',laShowFig[2,lnI] * lnAvgCost)
      ENDIF
      IF lnJ = 10
        laShowFig[lnJ,lnI] = IIF(lnScaleCnt<lnCol,'',laShowFig[3,lnI] * lnPriceA)
     ENDIF
      IF lnJ = 11
        laShowFig[lnJ,lnI] = IIF(lnScaleCnt<lnCol,'',laShowFig[6,lnI] * lnAvgCost)
      ENDIF
      IF lnJ = 12
        laShowFig[lnJ,lnI] =IIF(lnScaleCnt<lnCol,'',(laShowFig[9,lnI]-laShowFig[10,lnI])+laShowFig[11,lnI])
      ENDIF
    ENDIF
  ENDFOR
  SHOW GET laShowFig[3,lnI] &lcAbleSts.        && Sales Forecast Fields
  SHOW GET laShowFig[6,lnI] &lcAbleSts.        && Purchase Forecast Fields
  SHOW GET lnSalAdjPc       &lcAbleSts.        && Act. Sales/Forecast YTD% Field
  SHOW GET lnPurAdjPc       &lcAbleSts.        && Act. Purchase/Forecast YTD% Field
ENDFOR

STORE 'ENABLE' TO lcPrvSclS, lcNxtSclS, lcPrvMnthS, lcNxtMnthS
IF lnXPos = 3
  lcPrvSclS = 'DISABLE'
ENDIF

IF lnScaleCnt <= lnXPos+7
  lcNxtSclS = 'DISABLE'
ENDIF
IF lnYPos = 3
  lcPrvMnthS = 'DISABLE'
ENDIF

IF lnYPos + 8 > ALEN(laAllData,1)
  lcNxtMnthS = 'DISABLE'
ENDIF

SHOW GET pbPrvsScl  &lcPrvSclS.
SHOW GET pbNxtScl   &lcNxtSclS.
SHOW GET pbPrvsMnth &lcPrvMnthS.
SHOW GET pbNxtMnth  &lcNxtMnthS.

=lfGetMonth()
=lfRefresh()
*-- End of lfShowFig.

*!*************************************************************
*! Name      : lfChangPos
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Valid funtion of the pbprvsscl button
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lcPos,lnValue
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfChangPos()
*!*************************************************************
*!C#122961,1
FUNCTION lfChangPos
PARAMETERS lcPos,lnValue

PRIVATE lcVarName
lcVarName = 'ln' + lcPos + 'Pos'

&lcVarName. = EVALUATE(lcVarName) + lnValue
IF lcPos = 'Y'
  lnXPos = 3
  
  IF lnYPos = lnZPos + 6
    lnYPos = lnYPos + 7
    lnZPos = lnZPos + 84
  ENDIF
  
  IF lnYPos = lnZPos - 84
    lnZPos = lnYPos - 6
    lnYPos = lnYPos - 7
  ENDIF

*-- Update the Act. Sales/Forecast YTD% and the Act. Purchase/Forecast YTD% for the next month
*-- Only if the user Press the next Month Arrow
= lfUpdNPrc()
ENDIF
= lfShowFig()
*-- End of lfChangPos.

*!*************************************************************
*! Name      : lfGetScale
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to get the scale count of style.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfGetScale()
*!*************************************************************
*!C#122961,1
FUNCTION lfGetScale

SELECT STYLE
IF SEEK(PADR(laData[1],lnMjrWid))

  SCAN REST WHILE STYLE = PADR(laData[1],lnMjrWid);
              FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
    IF SEEK('S'+SCALE,'SCALE')
      lnScaleCnt = lnScaleCnt + SCALE.CNT
    ENDIF
  ENDSCAN
ENDIF
lnScaleCnt = lnScaleCnt + 1
*-- End of lfGetScale.

*!*************************************************************
*! Name      : lfGetMonth
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to Get the month name.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lnForMonth,lnForYear
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  = lfGetMonth()
*!*************************************************************
*!C#122961,1
FUNCTION lfGetMonth

PRIVATE lcDateSet,ldFrsMnDay

lcDateSet = SET('DATE')
SET DATE TO MDY
ldFrsMnDay = CTOD(STRTRAN(STR(laAllData[lnYPos,2],2,0),' ','0')+'/01/'+STR(laAllData[lnYPos,1],4,0))
lcMonth = CMONTH(ldFrsMnDay)
lcYear  = STR(laAllData[lnYPos,1],4,0)
SET DATE TO &lcDateSet.
*-- End of lfGetMonth.

*!*************************************************************
*! Name      : lfvDefWare
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Validation of the warehouse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefWare()
*!*************************************************************
*!C#122961,1
FUNCTION lfvDefWare

IF llBrowse OR !SEEK(lcWareCode,'WAREHOUS')
  lcWareCode = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
ENDIF
llBrowse = .F.
*-- End of lfvDefWare.

*!*************************************************************
*! Name      : lfvDefPCurr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Validation of the default price currency
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefPCurr()
*!*************************************************************
*!C#122961,1
FUNCTION lfvDefPCurr

*-- if the entered currency does not exist in the currency code file
IF llBrowse OR !SEEK(lcPCurr,"SycCurr")
  IF !gfcurrbrow(@lcPCurr)
    lcPCurr = gcBaseCurr
  ENDIF
ENDIF
llBrowse = .F.
*-- get the final exchange rate of the price currency
lnPRate = gfchkrate('lnUnit1',lcPCurr ,gdSysDate,llEditExRt,gcAct_Comp,.F.)
*-- if there is no valid rate
IF lnPRate <= 0
  IF gfModalGen('INM36004B36001','DIALOG','price') = 1
    lcPCurr = gcBaseCurr
    lnPRate = 1
    lnUnit1 = 1
  ELSE
    _CUROBJ = OBJNUM(lcPCurr)
  ENDIF
ENDIF
IF lcPCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnPRate DISABLE
ELSE
  SHOW GET lnPRate ENABLE
ENDIF
*-- End of lfvDefPCurr.

*!*************************************************************
*! Name      : lfGetStyle
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to get the correct style record.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGetStyle()
*!*************************************************************
*!C#122961,1
FUNCTION lfGetStyle

PRIVATE lnAlias
lnAlias = SELECT(0)
IF SEEK(PADR(lcStyle,lnMjrWid),'STYLE')
  SELECT STYLE
  LOCATE REST WHILE STYLE = PADR(lcStyle,lnMjrWid);
                FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = lcColor .AND.;
                    SCALE = PADR(POTOBUY.CSIZECODE,3)
ENDIF
SELECT (lnAlias)
*-- End of lfGetStyle.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Delete current style/color.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C#122961,1
PROCEDURE lpDelScr

PRIVATE lnAlias , lcColStyle , lcOrder
STORE '' TO lcColStyle , lcOrder
lnAlias = SELECT(0)

SELECT FORECAST
lcOrder = SET ('ORDER')            && Save the Forecast Order.
SET ORDER TO TAG FORECASTY
lcColStyle = SUBST(laData[1],1,lnMjrWid) + IIF(lnClrPos >(lnMjrWid+1) , '-' + laData[2] ,laData[2])

IF SEEK(STR(laAllData[lnYPos,1],4,0)+ STR(laAllData[lnYPos,2],2,0) + lcColStyle)
  SCAN REST WHILE STR(nyear,4)+STR(nmonth,2)+style = STR(laAllData[lnYPos,1],4,0) + ;
                                                     STR(laAllData[lnYPos,2],2,0) + ;
                                                     lcColStyle
    DELETE
  ENDSCAN
ENDIF
SET ORDER TO &lcOrder       && return to the Previous Order in File Forecast
SELECT (lnAlias)
STORE .F. TO laScrMode
STORE .T. TO laScrMode[1]
*-- End Of lpDelScr.

*!*************************************************************
*! Name      : gfCPBrows     C#122961,1
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Validation of the browse button in the pannel
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* The valid function for the control panel browse button
*!->
FUNCTION gfCPBrows

SELECT (lcBaseFile)
=gfBrows()
laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
laScrMode   = .F.
laScrMode[2]= .T.
SHOW GETS
*-- End of gfCpBrows.

*!*************************************************************
*! Name      : lpTopScr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Top record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C#122961,1
PROCEDURE lpTopScr

laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)

IF SEEK(laData[1],'STYLE')
  SELECT STYLE
  LOCATE REST WHILE STYLE = PADR(laData[1],lnMjrWid);
                FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
ENDIF

*-- End OF lpTopScr.

*!*************************************************************
*! Name      : lpBtmScr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Bottom record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C#122961,1
PROCEDURE lpBtmScr

laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
IF SEEK(laData[1],'STYLE')
  SELECT STYLE
  LOCATE REST WHILE STYLE = PADR(laData[1],lnMjrWid);
                FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
ENDIF
*-- End OF lpBtmScr.

*!*************************************************************
*! Name      : lpNxtScr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Next record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C#122961,1
PROCEDURE lpNxtScr

lcStyle   = laData[1]
lcColor   = laData[2]
laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
IF laData[1]+laData[2] = lcStyle+lcColor
  SELECT STYLE
  LOCATE REST FOR PADR(STYLE,lnMjrWid) # laData[1] .OR.;
                  SUBSTR(STYLE,lnClrPos,lnClrLen) # laData[2]
  laData[1] = PADR(STYLE.STYLE,lnMjrWid)
  laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
ENDIF
*-- End OF lpNxtScr

*!*************************************************************
*! Name      : lpPrvScr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Previus record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*!C#122961,1
PROCEDURE lpPrvScr

laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
IF SEEK(PADR(laData[1],lnMjrWid),'STYLE')
  SELECT STYLE
  LOCATE REST WHILE STYLE = PADR(laData[1],lnMjrWid);
                FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
ENDIF
*-- End OF lpPrvScr.

*!*************************************************************
*! Name      : lfChkLstRc
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Chick the last record in the sytle file.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*!C#122961,1
FUNCTION lfChkLstRc

lnRecNo = RECNO('STYLE')
GO BOTTOM IN STYLE
lcStyle = PADR(STYLE.STYLE,lnMjrWid)
lcColor = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
IF laData[1]+laData[2] = lcStyle+lcColor
  STORE 'DISABLE' TO laCtrStat[2],laCtrStat[3]
  SHOW GET pbBtm DISABLE
  SHOW GET pbNxt DISABLE
ENDIF
GO lnRecNo IN STYLE
*-- End of lfChkLstRc.

*!*************************************************************
*! Name      : lfActPad
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActPad()
*!*************************************************************
*!C#122961,1
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Style Inquiry' SKIP FOR !laScrMode[2]
ON SELECTION BAR 1 OF _OPTIONPOP DO lfStyInq
*-- End of lfActPad.

*!*************************************************************
*! Name      : lfBrowSty
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Browse Styles
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowSty()
*!*************************************************************
*!C#122961,1
FUNCTION lfBrowSty

SELECT (lcTmpStyle)
BROWSE FIELDS cMarker =IIF(RECNO()=lnDMarker,'>',' '):H='':W=.F.,;
              cSelect:H=' '   :R,      ;
              Style:H='Style' :R,      ;
              Color:H='Colour':R,      ;
              nYear:H='Year'  :R,      ;
              nMonth:H='Month':R,      ;
              nTotQty:H='To Buy':R,    ;
              cAdd_User:H='Add User':R,;
              dAdd_Date:H='Add Date':R ;
              WINDOW POSELST1          ;
              IN WINDOW POSELSTY       ;
              NOMENU                   ;
              NOAPPEND                 ;
              NODELETE                 ;
              NOWAIT                   ;
              SAVE                     ;
              NOCLEAR                  ;
              WHEN lfwStyBrs()         ;
              TITLE lcStyleTit
*-- End of lfBrowSty.

*!*************************************************************
*! Name      : lfwStyBrs
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Show Styles
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwStyBrs()
*!*************************************************************
*!C#122961,1
FUNCTION lfwStyBrs

lndMarker = RECNO(lcTmpStyle)
SHOW WINDOW (lcStyleTit) REFRESH SAME
=lfvSelect('')
*-- End of lfwStyBrs.

*!*************************************************************
*! Name      : lfReadAct
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : READ Activate function
*!*************************************************************
*! Calls     : lfClearKey.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
*!C#122961,1
FUNCTION lfReadAct

IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcStyleTit)
*-- End of lfReadAct.

*!*************************************************************
*! Name      : lfClearKey
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Clear Hot Keys
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfClearKey()
*!*************************************************************
*!C#122961,1
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER
*-- End of lfClearKey.

*!*************************************************************
*! Name      : lfDMainLot
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Deactivate main screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDMainLot()
*!*************************************************************
*!C#122961,1
FUNCTION lfDMainLot

IF WONTOP()= lcStyleTit
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTabKey  WITH 'POSELST2','pbSelect'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'POSELST2','pbCancel'
ENDIF
*--NNA
SET DECIMALS TO lnSetDeci
*RETURN .F.
*-- End of lfDMainLot

*!*************************************************************
*! Name      : lpTabKey
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTabKey WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
*!C#122961,1
PROCEDURE lpTabKey
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL TAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lpBackTab
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackTab WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
*!C#122961,1
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL BACKTAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lfvSelect
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Called from the selection buttons
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSelect()
*!*************************************************************
*!C#122961,1
FUNCTION lfvSelect
PARAMETER lcSelect

PRIVATE lnAlias,lnRecNo

lnAlias = SELECT(0)
SELECT (lcTmpStyle)
lnRecNo = RECNO()

DO CASE
  CASE lcSelect = 'S'
    REPLACE cSelect WITH IIF(cSelect="�",'',"�")
  CASE lcSelect = 'A'
    REPLACE ALL cSelect WITH "�"
  CASE lcSelect = 'N'
    REPLACE ALL cSelect WITH " "
  CASE lcSelect = 'V'
    REPLACE ALL cSelect WITH IIF(cSelect="�",'',"�")
ENDCASE

lcOrdTag = TAG()
SET ORDER TO TAG (lcTmpStyl1)
IF SEEK("�")
  SHOW GET pbSelNone ENABLE
  IF llOkDefalt
    lcOkStatus = 'ENABLE'
  ELSE
    lcOkStatus = 'DISABLE'
  ENDIF
ELSE
  SHOW GET pbSelNone DISABLE
  lcOkStatus = 'DISABLE'
ENDIF
IF SEEK(" ")
  SHOW GET pbSelAll ENABLE
ELSE
  SHOW GET pbSelAll DISABLE
ENDIF
SET ORDER TO TAG &lcOrdTag
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
SHOW GET pbSelect,1 PROMPT IIF(EMPTY(cSelect),"\<Select","Un\<Select")
SHOW GET pbOk      &lcOkStatus.
*-- refresh the selection browse
SELECT (lnAlias)
*--End of lfvSelect.

*!**************************************************************************
*! Name      : lfVariabls
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to save/restore variabls
*!**************************************************************************
*! Example   : =lfVariabls()
*!**************************************************************************
*!C#122961,1
FUNCTION lfVariabls
PARAMETERS llRestore

FOR lnI = 1 TO 11
  IF llRestore
    &laSaveVar[lnI,1]. = laSaveVar[lnI,2]
  ELSE
    laSaveVar[lnI,2] = EVALUATE(laSaveVar[lnI,1])
  ENDIF
ENDFOR
*-- End of lfVariabls.

*!**************************************************************************
*! Name      : lfStyInq
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to call the style screen.
*!**************************************************************************
*! Example   : =lfStyInq()
*!**************************************************************************
*!C#122961,1
FUNCTION lfStyInq

lcStyle = STYLE.STYLE
lcParameter = "'"+lcStyle+"',''"
DO gpDoProg WITH 'AWRICSTYLE',.F.,'IC',lcParameter
*-- End of lfStyInq.

*!*************************************************************
*! Name      : lfvSalFcst
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Valid function of the Sales Forecast fields.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lnCol ---> The size NO#
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvSalFcst(1)
*!*************************************************************
*!C#122961,1
FUNCTION lfvSalFcst
PARAMETERS lnCol

PRIVATE lnAlias ,lcColStyle
STORE 0 TO lnAlias

lnAlias = SELECT(0)
*-- Prevent User from input a Negative Digits

*--NNA (BEGIN) 11/11/2004 Don't continue with this validation if value < Zero or there is no
*--NNA         changing in values
*IF laShowFig[3,lnCol] < 0
IF laShowFig[3,lnCol] < 0 OR (laShowFig[3,lnCol] = laAllData[lnYPos+1,lnXPos+lnCol-1])
*--NNA (END)

  laShowFig[3,lnCol] = laAllData[lnYPos+1,lnXPos+lnCol-1]  
  RETURN
ENDIF
*-- Update Sales Forecast Value 
laShowfig[10,lnCol] = laShowFig[3,lnCol] * lnPriceA
laShowFig[12,lnCol] = (laShowFig[9,lnCol]-laShowFig[10,lnCol])+laShowFig[11,lnCol]
SHOW GET laShowfig[10,lnCol] , laShowfig[12,lnCol]

*-- Get Sales ForeCast line Total.
laAllData[lnYPos+1,lnScaleCnt]     = ROUND(laAllData[lnYPos+1,lnScaleCnt],0) + ;
                                     laShowFig[3,lnCol]                      - ;
                                     ROUND(laAllData[lnYPos+1,lnXPos+lnCol-1],0)
= lfUpdSPrc(lncol)
*-- Hold the Current Closing Stock To use it if there are any changes
lnPrStock = laAllData[lnYPos+6,lnXPos+lnCol-1]

*-- Get Sales ForeCast line Fields.
laAllData[lnYPos+1,lnXPos+lnCol-1] = laShowFig[3,lnCol]

IF lnScaleCnt <= lnXPos + 7
  laShowFig[3,lnScaleCnt-lnXPos+1] = laAllData[lnYPos+1,lnScaleCnt]
  SHOW GET laShowFig[3,lnScaleCnt-lnXPos+1]
ENDIF

*-- Get Closing stock line.
=lfGetClose(lnYPos+6,lnXPos+lnCol-1)
laAllData[lnYPos+6,lnScaleCnt] = ROUND(laAllData[lnYPos+6,lnScaleCnt],0)    + ; 
                                 ROUND(laAllData[lnYPos+6,lnXPos+lnCol-1],0)- ; 
                                 laShowFig[8,lnCol]                
*--NNA (BEGIN) 11/11/2004 Get the difference between the old closing stock and the new value 
*--NNA         to add it to the next months's closing and opening values.
lnPrStock = laAllData[lnYPos+6,lnXPos+lnCol-1] - lnPrStock
IF lnPrStock <> 0
*--NNA (END)

  =lfUpdClose(lnCol)

*--NNA (BEGIN) 11/11/2004
ENDIF
*--NNA (END)   
laShowFig[8,lnCol] = laAllData[lnYPos+6,lnXPos+lnCol-1]                                                                  

IF lnScaleCnt <= lnXPos + 7
  laShowFig[8,lnScaleCnt-lnXPos+1] = laAllData[lnYPos+6,lnScaleCnt]
  SHOW GET laShowFig[8,lnScaleCnt-lnXPos+1]
ENDIF

*-- Update the temp table for the current month.
SELECT (lcTmpFrCst)
lcColStyle = SUBST(laData[1],1,lnMjrWid) + IIF(lnClrPos >(lnMjrWid+1) , '-' + laData[2] ,laData[2])
lcColStyle = lcColStyle + IIF(lnSepMins = 3 , SUBST(LAALLDATA[1,LNXPOS+LNCOL-1],1,3) +;
                          SPACE(1) , '-' +  SUBST(LAALLDATA[1,LNXPOS+LNCOL-1],1,3))
IF !SEEK(lcColStyle + STR(laAllData[lnYPos+6,1],4,0) + STR(laAllData[lnYPos+6,2],2,0))
  APPEND BLANK
  REPLACE NYEAR     WITH laAllData[lnYPos+6,1],;
          NMONTH    WITH laAllData[lnYPos+6,2],;
          STYLE     WITH lcColStyle              
ENDIF
Lci = SUBST(LAALLDATA[1,LNXPOS+LNCOL-1],4,1)
REPLACE NSalFrcst&Lci   WITH laShowFig[3,lnCol]
SELECT (lnAlias)

IF lnCol = 8
  =lfChangPos('X',8)
  IF lnXPos < lnScaleCnt
    _CUROBJ = OBJNUM(laShowFig[3,1])
  ENDIF
ENDIF
=lfShowFig()
=lfRefresh()
*-- End of lfvSalFcst.

*!*************************************************************
*! Name      : lfvSalAdj
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to Get the Sales ForeCast.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfvSalAdj()
*!*************************************************************
*!C#122961,1
FUNCTION lfvSalAdj
IF lnSalAdjPc <> 0
  FOR I = 1 TO (lnScaleCnt-3)
    N = MOD(I - 1 , 8) + 1
      laShowFig[3,N] = ROUND((laShowFig[3,N] * (lnSalAdjPc+100))/100,0)
      SHOW GET laShowFig[3,N]
       *--To change the array with this changes and get the new total
      = lfvSalFcst(N) 			
  ENDFOR
  *-- Clear the Adjustment field and refresh the screen.
  STORE 0 TO lnSalAdjPc
  SHOW GET lnSalAdjPc
  =lfRefresh()
ENDIF

*-- End of lfvSalAdj.
*!*************************************************************
*! Name      : lfvPurFcst
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Valid function of the Purchase Forecast fields.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lnCol ---> The size NO#
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvPurFcst(1)
*!*************************************************************
*!C#122961,1
FUNCTION lfvPurFcst
PARAMETERS lnCol

PRIVATE lnAlias , lcColStyle
lnAlias = SELECT(0)

*-- Prevent User from input a Negative Digits
IF laShowFig[6,lnCol] < 0
  laShowFig[6,lnCol] = laAllData[lnYPos+4,lnXPos+lnCol-1]  
  RETURN
ENDIF
*-- Update Purchase Forecast Value 
laShowfig[11,lnCol] = laShowFig[6,lnCol] * lnAvgCost
laShowFig[12,lnCol] = (laShowFig[9,lnCol]-laShowFig[10,lnCol])+laShowFig[11,lnCol]
SHOW GET laShowfig[11,lnCol] , laShowfig[12,lnCol]

*-- Get Purchase ForeCast line Total.
laAllData[lnYPos+4,lnScaleCnt]     = ROUND(laAllData[lnYPos+4,lnScaleCnt],0) +;
                                     laShowFig[6,lnCol]             -;
                                     ROUND(laAllData[lnYPos+4,lnXPos+lnCol-1],0)
*-- Update the Act. Purchase/Forecast YTD%
= lfUpdPPrc(lncol)

*-- Hold the Current Closing Stock To use it if there are any changes
lnPrStock = laAllData[lnYPos+6,lnXPos+lnCol-1]


*-- Get Purchase ForeCast line Fields.
laAllData[lnYPos+4,lnXPos+lnCol-1] = laShowFig[6,lnCol]

IF lnScaleCnt <= lnXPos + 7
  laShowFig[6,lnScaleCnt-lnXPos+1] = laAllData[lnYPos+4,lnScaleCnt]
  SHOW GET laShowFig[6,lnScaleCnt-lnXPos+1]
ENDIF

*-- Get Closing stock line.
=lfGetClose(lnYPos+6,lnXPos+lnCol-1)
laAllData[lnYPos+6,lnScaleCnt] = ROUND(laAllData[lnYPos+6,lnScaleCnt],0)    + ; 
                                 ROUND(laAllData[lnYPos+6,lnXPos+lnCol-1],0)- ; 
                                 laShowFig[8,lnCol]                
*--NNA (BEGIN) 11/11/2004 Get the difference between the old closing stock and the new value 
*--NNA         to add it to the next months's closing and opening values.
lnPrStock = laAllData[lnYPos+6,lnXPos+lnCol-1] - lnPrStock
IF lnPrStock <> 0
*--NNA (END)

  =lfUpdClose(lnCol)

*--NNA (BEGIN) 11/11/2004
ENDIF
*--NNA (END)   

laShowFig[8,lnCol] = laAllData[lnYPos+6,lnXPos+lnCol-1]

IF lnScaleCnt <= lnXPos + 7
  laShowFig[8,lnScaleCnt-lnXPos+1] = laAllData[lnYPos+6,lnScaleCnt]
  SHOW GET laShowFig[8,lnScaleCnt-lnXPos+1]
ENDIF

*-- Update the temp table for the current month.
SELECT (lcTmpFrCst)
lcColStyle = SUBST(laData[1],1,lnMjrWid) + IIF(lnClrPos >(lnMjrWid+1) , '-' + laData[2] ,laData[2])
lcColStyle = lcColStyle + IIF(lnSepMins = 3 , SUBST(LAALLDATA[1,LNXPOS+LNCOL-1],1,3) +;
                          SPACE(1) , '-' +  SUBST(LAALLDATA[1,LNXPOS+LNCOL-1],1,3))

IF !SEEK(lcColStyle + STR(laAllData[lnYPos+6,1],4,0) + STR(laAllData[lnYPos+6,2],2,0))
  APPEND BLANK
  REPLACE NYEAR     WITH laAllData[lnYPos+6,1],;
          NMONTH    WITH laAllData[lnYPos+6,2],;
          STYLE     WITH lcColStyle              
ENDIF
Lci = SUBST(LAALLDATA[1,LNXPOS+LNCOL-1],4,1)
REPLACE NPurFrcst&Lci   WITH laShowFig[6,lnCol]
SELECT (lnAlias)

IF lnCol = 8
  =lfChangPos('X',8)
  IF lnXPos < lnScaleCnt
    _CUROBJ = OBJNUM(laShowFig[6,1])
  ENDIF
ENDIF
=lfShowFig()
=lfRefresh()
*-- End of lfvPurFcst.

*!*************************************************************
*! Name      : lfvPurAdj
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to Get the Sales ForeCast.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfvPurAdj()
*!*************************************************************
*!C#122961,1
FUNCTION lfvPurAdj
IF lnPurAdjPc <> 0
  FOR L = 1 TO (lnScaleCnt-3)
    N = MOD(L - 1 , 8) + 1
      laShowFig[6,N] = ROUND((laShowFig[6,N] * lnPurAdjPc)/100 + laShowFig[6,N],0)
      SHOW GET laShowFig[6,N]
      = lfvPurFcst(N)			&& To change the array with this cganges and get the new total
  ENDFOR
  *-- Clear the Adjustment field and refresh the screen
  STORE 0 TO lnPurAdjPc
  SHOW GET lnPurAdjPc
  =lfRefresh()
ENDIF

*-- End of lfvPurAdj.
*!*************************************************************
*! Name      : lfGetPFCst
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to Get the Purchase ForeCast.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lnRow = Row (4) in the Screen and the array
*!                     Row(1) for sizes & Row (2) for Months
*!                     Row(3) for Opning Stock
*!           : lnCol = Size No ( from 1 to 8)
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfGetPFCst()
*!*************************************************************
*!C#122961,1
FUNCTION lfGetPFCst
PARAMETERS lnRow,lnCol
PRIVATE lnAlias
lnAlias = SELECT(0)
*-- IF SEEK ( YEAR               + MONTH                     + STYLE)
IF SEEK(STR(laAllData[lnJ,1],4,0)+ STR(laAllData[lnJ,2],2,0) + STYLE.STYLE,'FORECAST')
  SELECT FORECAST
  laAllData[lnRow,lnCol] = nPurFrCst&LCI
ELSE
  laAllData[lnRow,lnCol] = 0
ENDIF
SELECT (lnAlias)
*-- End of lfGetPFCst.
*!*************************************************************
*! Name      : lfGetPrc
*! Developer : NADER NABIL (NNA)
*! Date      : 06/22/2004
*! Purpose   : Function to Get the Purchase ForeCast.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lnRow = Row (6) in the Screen and the array.
*!                     Row (1) for sizes 
*!                     Row (2) for Months
*!                     Row (3) for Opning Stock
*!                     Row (4) for Sales Forecast
*!                     Row (5) for Last Years Actual Sales
*!           : lnCol = Size No ( from 1 to 8)
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  = lfGetPrc()
*!*************************************************************
*!C#122961,1
FUNCTION lfGetPrc
PARAMETERS lnRow , lnCol
PRIVATE lnBkAmt    , lnPosMnth  , lnSalFrcst , lnPurFrcst 
PRIVATE lcColStyle , lcSiz      , lcScal     , lnAlias
PRIVATE lcOrder    , lnRecno

STORE 0 TO lnBkAmt  , lnPosMnth  , lnSalFrcst , lnPurFrcst , lnRecno
STORE '' TO lcOrder ,  lcColStyle

lnAlias = SELECT(0)
SELECT FORECAST
lcOrder = SET ('ORDER')            && Save the Forecast Order.
SET ORDER TO TAG FORECAST 
lnRecno = RECNO()
*-- Get Scale From the Array of laAllData (Like BN1,BN2)
lcScal  = SUBST(laAllData[1,lnCol],1,LEN(laAllData[1,lnCol])-1)

*-- Get the Size No. From Array of laAllData Array (i.e Size = 1 in Scale BN11 & = 2 in Scale BN12)
lcSiz   = SUBST(laAllData[1,lnCol],LEN(laAllData[1,lnCol]),1)

*-- Collect the Style Parts to Search with it in the forecast File
lcColStyle = SUBST(laData[1],1,lnMjrWid) + IIF(lnClrPos >(lnMjrWid+1) , '-' + laData[2] ,laData[2])
lcColStyle = lcColStyle + IIF(lnSepMins = 3 , lcScal + SPACE(1) , '-' + lcScal)

IF SEEK(lcColStyle+STR(laAllData[lnRow,1],4))
  SCAN REST WHILE style+STR(nyear,4)+STR(nmonth,2) = lcColStyle+STR(laAllData[lnRow,1],4)

    *-- Accumulate Sales And Purchase Forecast including the Month on Screen.
     IF Nmonth <= laAllData[lnRow,2]
      lnSalFrcst = lnSalFrcst + NSalFrcst&lcSiz
      lnPurFrcst = lnPurFrcst + NPurFrcst&lcSiz
    ENDIF
  ENDSCAN
ELSE
  lnSalFrcst = 0
  lnPurFrcst = 0
ENDIF
IF SEEK(STR(laAllData[lnRow,1],4,0)+PADR(laData[1],19)+laData[2]+laAllData[1,lnCol],'YEARINPO')
  *--Get the Cumulate NBKAMT(Sales) and NPosMnth(Purchases) including the Month on Screen
  FOR I = 1 TO laAllData[lnRow,2]
    lcSalFld  = 'YEARINPO.NBKAMT' + STRTRAN(STR(I,2,0),' ','0')
    lcPurFld  = 'YEARINPO.NPosMnth' + STRTRAN(STR(I,2,0),' ','0')      
    IF laAllData[lnRow,2] > 1
      lnBkAmt    = lnBkAmt    + EVALUATE(lcSalFld)
      lnPosMnth  = lnPosMnth  + EVALUATE(lcPurFld)
    ELSE
      lnBkAmt    = EVALUATE(lcSalFld)
      lnPosMnth  = EVALUATE(lcPurFld)    
      EXIT
    ENDIF
  ENDFOR
ELSE
  lnBkAmt   = 0  
  lnPosMnth = 0
ENDIF
*-- Get the Act. Sales/Forecst YTD%    = ((Actual Sales / Sales Forecast)/100)-100
*-- Get the Act. Purchase/Forecst YTD% = ((Actual Purchases / Purchase Forecast)/100)-100
laAllData[lnRow,lnCol]   = IIF((lnBkAmt*lnSalFrcst)<>0,(((lnBkAmt/lnSalFrcst)/100)-100),0)
laAllData[lnRow+2,lnCol] = IIF((lnPosMnth*lnPurFrcst)<>0,(((lnPosMnth/lnPurFrcst)/100)-100),0)

laDataPrc[lnRow,1]   = laDataPrc[lnRow,1] + lnBkAmt
laDataPrc[lnRow,2]   = laDataPrc[lnRow,2] + lnSalFrcst
laDataPrc[lnRow,3]   = laDataPrc[lnRow,4] + lnPosMnth
laDataPrc[lnRow,4]   = laDataPrc[lnRow,5] + lnPurFrcst
IF LNSCLCNT=LNSCALECNT-1
  lnBKPrc  = laDataPrc[lnRow,1]
  lnSalPrc = laDataPrc[lnRow,2]
  lnPosPrc = laDataPrc[lnRow,3]
  lnPurPrc = laDataPrc[lnRow,4]
  laAllData[lnRow,lnScaleCnt]   = IIF(lnBKPrc*lnSalPrc<>0,(((lnBKPrc/lnSalPrc)/100)-100),0)
  laAllData[lnRow+2,lnScaleCnt] = IIF(lnPosPrc*lnPurPrc<>0,(((lnPosPrc/lnPurPrc)/100)-100),0)
ENDIF
SELECT FORECAST
SET ORDER TO &lcOrder       && return to the Previous Order in File Forecast

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecno 
ENDIF
SELECT (lnAlias)

*-- End of lfGetPrc.

*!*************************************************************
*! Name      : lfvGtSclSp
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
*! Purpose   : Function returns the length of Scale with Separator.
*!*************************************************************
*! Returns   : len of Scale 3 or 4 if separator exist.
*!*************************************************************
*! Example   : = lfvGtSclSp()
*!*************************************************************
*!C#122961,1
FUNCTION lfvGtSclSp
PRIVATE lnAlias
lnAlias = SELECT(0)
IF llMScale
  llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
  SELECT ICISTRU
  LOCATE FOR cItemRecty='U' AND cISegType='S'
  IF FOUND()
    SKIP -1
    lnSepMins = IIF(!lSegEndMaj AND !EMPTY(cISegSepr),4,3)
  ELSE
    lnSepMins = 3
  ENDIF
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
ELSE
  lnSepMins = 0
ENDIF
SELECT(lnAlias)
RETURN (lnSepMins)

*--End of lfvGtSclSp.


*!*************************************************************
*! Name      : lpClsScr
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
*! Purpose   : Function to Validae the close button in the pannel
*!*************************************************************
*! Returns   : len of Scale 3 or 4 if separator exist.
*!*************************************************************
*! Example   : = lpClsScr()
*!*************************************************************
*!C#122961,1
FUNCTION lpClsScr
IF !EMPTY(lcBaseFile)
  SCATTER FIELDS &lcScFields MEMO TO laData
  laData[1]  = PADR(laData[1],lnMjrWid)
ENDIF  
IF laScrMode[1] .OR. laScrMode[2]
  SET DECIMALS TO lnSetDeci
ENDIF
*--End of lpClsScr.
*!*************************************************************
*! Name      : lfUpdSPrc
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
*! Purpose   : Function to update the Act. scales/Forecast YTD%
*!           : if the user changed the sales forecast Qty. 
*!*************************************************************
*! Returns   : len of Scale 3 or 4 if separator exist.
*!*************************************************************
*! Example   : = lfUpdSPrc()
*!*************************************************************
*!C#122961,1
FUNCTION lfUpdSPrc
PARAMETERS lnCol
PRIVATE lnPrBkQty ,lnPSalFcst, lnCSalFcst , lnPrvPrc , lnNewPrc , lnTSalFcst , lnMFrcst
STORE 0 TO lnPrBkQty ,lnPSalFcst, lnNewPrc , lnCSalFcst , lnTSalFcst , lnMFrcst

lnPrvPrc   = laAllData[lnYPos+3,lnXPos+lnCol-1]

lnAlias = SELECT(0)
SELECT FORECAST
lcOrder = SET ('ORDER')            && Save the Forecast Order.
SET ORDER TO TAG FORECAST 
lnRecno = RECNO()
*-- Get Scale From the Array of laAllData (Like BN1,BN2)
lcScal  = SUBST(IIF(lnXPos = 3 , laAllData[1,lnCol+2]    , ;
                IIF(lnXPos = 11, laAllData[1,lnCol + 10] , ;
                IIF(lnXPos = 19, laAllData[1,lnCol + 18] , ;
                IIF(lnXPos = 27, laAllData[1,lnCol + 26] , ;
                laAllData[1,lnCol + 34] )))),1,LEN(laAllData[1,lnCol+2])-1)

*-- Get the Size No. From Array of laAllData Array 
*-- (i.e Size = 1 in Scale BN11 & = 2 in Scale BN12)
lcSiz1 = IIF(lnXPos = 3 , laAllData[1,lnCol+2]    , ;
         IIF(lnXPos = 11, laAllData[1,lnCol + 10] , ;
         IIF(lnXPos = 19, laAllData[1,lnCol + 18] , ;
         IIF(lnXPos = 27, laAllData[1,lnCol + 26] , ;
                          laAllData[1,lnCol + 34] ))))
         
lcSiz  = SUBST(lcSiz1,LEN(lcSiz1),1)

*-- Collect the Style Parts to Search with it in the forecast File
lcColStyle = SUBST(laData[1],1,lnMjrWid) + IIF(lnClrPos >(lnMjrWid+1) , '-' + laData[2] ,laData[2])
lcColStyle = lcColStyle + IIF(lnSepMins = 3 , lcScal + SPACE(1) , '-' + lcScal)

IF SEEK(lcColStyle+STR(laAllData[lnYPos+3,1],4))
  SCAN REST WHILE style+STR(nyear,4)+STR(nmonth,2) = lcColStyle+STR(laAllData[lnYPos+3,1],4)

    *-- Accumulate Sales And Purchase Forecast including the Month on Screen.
    IF Nmonth <= laAllData[lnYPos+3,2]
      IF Nmonth  = laAllData[lnYPos+3,2]
        lnMFrcst = NSalFrcst&lcSiz
      ELSE
        lnTSalFcst = lnTSalFcst + NSalFrcst&lcSiz
      ENDIF
    ENDIF
  ENDSCAN
ELSE
  lnSalFrcst = 0
  lnPurFrcst = 0
ENDIF
lnCSalFcst = lnTSalFcst + laShowFig[3,lnCol] 

lnPrBkQty = lfGetBook(lnCol)
lnNewPrc  = IIF(lnPrBkQty * lnCSalFcst <>0,((( lnPrBkQty /lnCSalFcst )/100)-100),0)

*-- Get total Prc. of The Month
lnPTotBk   = laDataPrc[lnYPos+3,1]
lnCTotFcst = laDataPrc[lnYPos+3,2] + (laShowFig[3,lnCol] - laAllData[lnYPos+1,lnXPos+lnCol-1])
laAllData[lnYPos+3,lnScaleCnt] = IIF(lnPTotBk * lnCTotFcst <>0 ,;
                                    (((lnPTotBk / lnCTotFcst) / 100) -100 ) , 0 )

*-- update laDataPrc array with the New Sales Forecast Value
laDataPrc[lnYPos+3,2] = lnCTotFcst


STORE lnNewPrc TO laAllData[lnYPos+3,lnXPos+lnCol-1] , laShowFig[5,lnCol]
SHOW GET laShowFig[5,lnCol]

*-- Show the TotPrc on the Screen
IF lnScaleCnt <= lnXPos + 7
  laShowFig[3,lnScaleCnt-lnXPos+1] = laAllData[lnYPos+3,lnScaleCnt]
  SHOW GET laShowFig[3,lnScaleCnt-lnXPos+1]
ENDIF
  
SET ORDER TO &lcOrder       && return to the Previous Order in File Forecast
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecno 
ENDIF
SELECT (lnAlias)

*--End of lfUpdSPrc.
*!*************************************************************
*! Name      : lfUpdPPrc
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
*! Purpose   : Function to update the Act. Purchase/Forecast YTD%
*!           : if the user changed the Purchase forecast Qty. 
*!*************************************************************
*! Returns   : len of Scale 3 or 4 if separator exist.
*!*************************************************************
*! Example   : = lfUpdPPrc()
*!*************************************************************
*!C#122961,1
FUNCTION lfUpdPPrc
PARAMETERS lnCol
PRIVATE lnPrPoQty , lnCPurFcst , lnPrvPrc , lnNewPrc , lnTPurFcst , lnMPurch
STORE 0 TO lnPrPoQty , lnNewPrc , lnCPurFcst , lnTPurFcst , lnPrvPrc ,lnMPurch

lnPrvPrc   = laAllData[lnYPos + 5,lnXPos+lnCol-1]

lnAlias = SELECT(0)
SELECT FORECAST
lcOrder = SET ('ORDER')            && Save the Forecast Order.
SET ORDER TO TAG FORECAST 
lnRecno = RECNO()

*-- Get Scale From the Array of laAllData (Like BN1,BN2)
lcScal  = SUBST(IIF(lnXPos = 3 , laAllData[1,lnCol+2]    , ;
                IIF(lnXPos = 11, laAllData[1,lnCol + 10] , ;
                IIF(lnXPos = 19, laAllData[1,lnCol + 18] , ;
                IIF(lnXPos = 27, laAllData[1,lnCol + 26] , ;
                laAllData[1,lnCol + 34] )))),1,LEN(laAllData[1,lnCol+2])-1)

*-- Get the Size No. From Array of laAllData Array 
*-- (i.e Size = 1 in Scale BN11 & = 2 in Scale BN12)
lcSiz = IIF(lnXPos = 3 , laAllData[1,lnCol+2]    , ;
        IIF(lnXPos = 11, laAllData[1,lnCol + 10] , ;
        IIF(lnXPos = 19, laAllData[1,lnCol + 18] , ;
        IIF(lnXPos = 27, laAllData[1,lnCol + 26] , ;
                          laAllData[1,lnCol + 34] ))))
         
lcSiz = SUBST(lcSiz,LEN(lcSiz),1)


*-- Collect the Style Parts to Search with it in the forecast File
lcColStyle = SUBST(laData[1],1,lnMjrWid) + IIF(lnClrPos >(lnMjrWid+1) , '-' + laData[2] ,laData[2])
lcColStyle = lcColStyle + IIF(lnSepMins = 3 , lcScal + SPACE(1) , '-' + lcScal)

IF SEEK(lcColStyle+STR(laAllData[lnYPos+3,1],4))
  SCAN REST WHILE style+STR(nyear,4)+STR(nmonth,2) = lcColStyle+STR(laAllData[lnYPos+3,1],4)

    *-- Accumulate Sales And Purchase Forecast including the Month on Screen.
    IF Nmonth <= laAllData[lnYPos+3,2]
      IF Nmonth = laAllData[lnYPos+3,2]
        lnMPurch = NPurFrcst&lcSiz
      ELSE
        lnTPurFcst = lnTPurFcst + NPurFrcst&lcSiz
      ENDIF
    ENDIF
  ENDSCAN
ELSE
  lnPurFrcst = 0
ENDIF

lnCPurFcst = lnTPurFcst + laShowFig[6,lnCol]
lnPrPoQty  = lfGetPur(lnCol)
lnNewPrc   = IIF(lnPrPoQty * lnCPurFcst <>0,((( lnPrPoQty /lnCPurFcst )/100)-100),0)

*-- Get total Prc. of The Month
lnPTotPos  = laDataPrc[lnYPos+3,3]
lnCTPoFcst = laDataPrc[lnYPos+3,4] + (laShowFig[6,lnCol] - laAllData[lnYPos+4,lnXPos+lnCol-1])
laAllData[lnYPos+5,lnScaleCnt] = IIF(lnPTotPos * lnCTPoFcst <>0 ,;
                                    (((lnPTotPos / lnCTPoFcst) / 100) -100 ) , 0 )
*-- update laDataPrc array with the New Po forecast Value
laDataPrc[lnYPos+3,4] = lnCTPoFcst

STORE lnNewPrc TO laAllData[lnYPos+5,lnXPos+lnCol-1] , laShowFig[7,lnCol]
SHOW GET laShowFig[7,lnCol]

*-- Show the TotPrc on the Screen
IF lnScaleCnt <= lnXPos + 7
  laShowFig[7,lnScaleCnt-lnXPos+1] = laAllData[lnYPos+5,lnScaleCnt]
  SHOW GET laShowFig[7,lnScaleCnt-lnXPos+1]
ENDIF

SET ORDER TO &lcOrder       && return to the Previous Order in File Forecast
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecno 
ENDIF
SELECT (lnAlias)

*--End of lfUpdPPrc.

*!**************************************************************************
*! Name      : lfUpdClose
*! Developer : NADER NABIL (NNA)
*! Date      : 06/23/2004
*! Purpose   : Function to Update The close and the Openning Stock at the Array
*!           : laAllData with any Changes in the To Buy Fileds
*!**************************************************************************
*! Example   : = lfUpdClose()
*!**************************************************************************
*!C122961,1
FUNCTION lfUpdClose
PARAMETERS lnCol

*-- Update  Openning stock and Closing Stock lines for the next Months.
FOR N1 = lnYPos + 7 TO lnRowCnt STEP 7
  *--Opening stock for next Month = Closing Stock for the Processed Month

  *--NNA (BEGIN) 11/11/2004 Add the difference between the old and the new closing stock value
  *--NNA         to the next openning and closing stock values even by (+ or -)
  *laAllData[N1,lnXPos + lnCol -1] = laAllData[N1,lnXPos + lnCol -1]        + ;
                                   (laAllData[N1 - 1,lnXPos + lnCol -1 ]  - ;
                                   lnPrStock)              
  *laAllData[N1,lnScaleCnt]        = laAllData[N1,lnScaleCnt]               + ;
                                   (laAllData[N1 - 1,lnXPos + lnCol -1 ]  - ;
                                   lnPrStock)              
  *-- Closing Stock
  *laAllData[N1+6,lnXPos+lnCol-1]  = laAllData[N1 + 6,lnXPos + lnCol -1 ]   + ;
                                   (laAllData[N1 - 1,lnXPos + lnCol -1]   - ;
                                   laShowFig[8,lnCol])             
  *laAllData[N1+6,lnScaleCnt]      = laAllData[N1 + 6,lnScaleCnt]           + ;
                                   (laAllData[N1 - 1,lnXPos + lnCol -1]   - ;
                                   laShowFig[8,lnCol])             

  laAllData[N1,lnXPos + lnCol -1] = laAllData[N1,lnXPos + lnCol -1]      + lnPrStock
  laAllData[N1,lnScaleCnt]        = laAllData[N1,lnScaleCnt]             + lnPrStock
  
  *-- Closing Stock
  laAllData[N1+6,lnXPos+lnCol-1]  = laAllData[N1 + 6,lnXPos + lnCol -1 ] + lnPrStock
  laAllData[N1+6,lnScaleCnt]      = laAllData[N1 + 6,lnScaleCnt]         + lnPrStock
  *--NNA (END)

 ENDFOR

*--End of lfUpdClose

*!**************************************************************************
*! Name      : lfGetBook
*! Developer : NADER NABIL (NNA)
*! Date      : 06/23/2004
*! Purpose   : Function to get the Booked Qty. When a user make Any Changes in
*!           : the Forecast Qty.
*!**************************************************************************
*! Example   : = lfGetBook()
*!**************************************************************************
*!C122961,1
FUNCTION lfGetBook
PARAMETERS lnCol
PRIVATE lnPrBkAmt , lnAlias , lcSiz1
STORE 0 TO lnPrBkAmt , lnAlias
STORE '' TO lcSiz1
lnAlias = SELECT(0)
lcSiz1 = IIF(lnXPos = 3 , laAllData[1,lnCol + 2 ] , ;
         IIF(lnXPos = 11, laAllData[1,lnCol + 10] , ;
         IIF(lnXPos = 19, laAllData[1,lnCol + 18] , ;
         IIF(lnXPos = 27, laAllData[1,lnCol + 26] , ;
                          laAllData[1,lnCol + 34] ))))
IF SEEK(STR(laAllData[lnYPos+3,1],4,0)+PADR(laData[1],19)+laData[2]+lcSiz1,'YEARINPO')
  SELECT YEARINPO
  *--Get the Cumulate NBKAMT(Sales) and NPosMnth(Purchases) including the Month on Screen
  FOR X = 1 TO laAllData[lnYPos+3,2]
    lcSalFld  = 'YEARINPO.NBKAMT' + STRTRAN(STR(X,2,0),' ','0')
    IF laAllData[lnYPos+3,2] > 1
      lnPrBkAmt = lnPrBkAmt    + EVALUATE(lcSalFld)
    ELSE
      lnPrBkAmt = EVALUATE(lcSalFld)
      EXIT
    ENDIF
  ENDFOR
ELSE
  lnPrBkAmt = 0  
ENDIF
SELECT(lnAlias)
Return (lnPrBkAmt)

*--End of lfGetBook

*!**************************************************************************
*! Name      : lfGetPur
*! Developer : NADER NABIL (NNA)
*! Date      : 06/23/2004
*! Purpose   : Function to get the Purchased Qty. When a user make Any Changes in
*!           : the Purchase forecast Qty.
*!**************************************************************************
*! Example   : = lfGetPur()
*!**************************************************************************
*!C122961,1
FUNCTION lfGetPur

PARAMETERS lnCol
PRIVATE lnPosMnth , lnAlias , lcSiz1
STORE 0 TO lnPosMnth , lnAlias
STORE '' TO lcSiz1
lnAlias = SELECT(0)
lcSiz1  = IIF(lnXPos = 3 , laAllData[1,lnCol + 2 ] , ;
          IIF(lnXPos = 11, laAllData[1,lnCol + 10] , ;
          IIF(lnXPos = 19, laAllData[1,lnCol + 18] , ;
          IIF(lnXPos = 27, laAllData[1,lnCol + 26] , ;
                          laAllData[1,lnCol + 34] ))))
IF SEEK(STR(laAllData[lnYPos+5,1],4,0)+PADR(laData[1],19)+laData[2]+lcSiz1,'YEARINPO')
  SELECT YEARINPO
  *--Get the Cumulate NBKAMT(Sales) and NPosMnth(Purchases) including the Month on Screen
  FOR Z = 1 TO laAllData[lnYPos+5,2]
    lcPurFld  = 'YEARINPO.NPosMnth' + STRTRAN(STR(Z,2,0),' ','0')      
    IF laAllData[lnYPos+5,2] > 1
      lnPosMnth  = lnPosMnth  + EVALUATE(lcPurFld)
    ELSE
      lnPosMnth  = EVALUATE(lcPurFld)    
      EXIT
    ENDIF
  ENDFOR
ELSE
  lnPosMnth = 0
ENDIF
SELECT(lnAlias)
Return (lnPosMnth)

*--End of lfGetPur

*!**************************************************************************
*! Name      : lfUpdNPrc
*! Developer : NADER NABIL (NNA)
*! Date      : 06/23/2004
*! Purpose   : Function to update the Act. Sales/Forecast YTD% and 
*!           : Act. Purchase/Forecast YTD% For the next Month
*!**************************************************************************
*! Example   : = lfUpdNPrc()
*!**************************************************************************
*!C122961,1
FUNCTION lfUpdNPrc
PRIVATE lnBkAmt,lnPosMnth,lnSalFrcst,lnPurFrcst,lnTBkAmt,lnTPosMnth,lnTSalFcst,lnTPurFcst
PRIVATE lcColStyle , lcSiz      , lcScal     , lnAlias
PRIVATE lcOrder    , lnRecno
STORE 0 TO lnBkAmt,lnPosMnth,lnSalFrcst,lnPurFrcst,lnTBkAmt,lnTPosMnth,lnTSalFcst,lnTPurFcst , lnRecno , lnAlias
STORE '' TO lcOrder, lcColStyle , lcSiz      , lcScal
lnAlias = SELECT(0)
SELECT FORECAST
lcOrder = SET ('ORDER')            && Save the Forecast Order.
SET ORDER TO TAG FORECAST 
lnRecno = RECNO()

FOR T = 3 TO lnScaleCnt - 1
  *-- Get Scale From the Array of laAllData (Like BN1,BN2)
  lcScal  = SUBSTR(laAllData[1,T] ,1,LEN(laAllData[1,T])-1)

  *-- Get the Size No. From Array of laAllData Array 
  *-- (i.e Size = 1 in Scale BN11 & = 2 in Scale BN12)
  lcSiz = SUBST(laAllData[1,T],LEN(laAllData[1,T]),1)

  *-- Collect the Style Parts to Search with it in the forecast File
  lcColStyle = SUBST(laData[1],1,lnMjrWid) + IIF(lnClrPos >(lnMjrWid+1) , '-' + laData[2] ,laData[2])
  lcColStyle = lcColStyle + IIF(lnSepMins = 3 , lcScal + SPACE(1) , '-' + lcScal)

  *-- Get the New forecast Values for the Current Month or The Next Months From the Forecast file
  SELECT (lcTmpFrcst)
  IF SEEK(lcColStyle+STR(laAllData[lnYPos,1],4))
    SCAN REST WHILE style+STR(nyear,4)+STR(nmonth,2) = lcColStyle+STR(laAllData[lnYPos,1],4)

      *-- Accumulate Sales And Purchase Forecast including the Month on Screen.
      IF Nmonth <= laAllData[lnYPos,2]
        lnSalFrcst = lnSalFrcst + NSalFrcst&lcSiz
        lnTSalFcst = lnTSalFcst + NSalFrcst&lcSiz

        lnPurFrcst = lnPurFrcst + NPurFrcst&lcSiz
        lnTPurFcst = lnTPurFcst + NPurFrcst&lcSiz
      ENDIF
    ENDSCAN
  ELSE
    lnSalFrcst = 0
    lnPurFrcst = 0
    lnTPurFcst = 0
    lnTSalFcst = 0
  ENDIF

  *-- Get the Forecast Values For the Previous Months
  SELECT FORECAST
  IF SEEK(lcColStyle+STR(laAllData[lnYPos,1],4))
    SCAN REST WHILE style+STR(nyear,4)+STR(nmonth,2) = lcColStyle+STR(laAllData[lnYPos,1],4)

      *-- Accumulate Sales And Purchase Forecast Not including the Month on Screen.
      IF Nmonth < laAllData[3,2]
        lnSalFrcst = lnSalFrcst + NSalFrcst&lcSiz
        lnTSalFcst = lnTSalFcst + NSalFrcst&lcSiz

        lnPurFrcst = lnPurFrcst + NPurFrcst&lcSiz
        lnTPurFcst = lnTPurFcst + NPurFrcst&lcSiz
      ENDIF
    ENDSCAN
  ENDIF

  *-- Get the Book Amount and The Purchased Amount from the YEARINPO File
  IF SEEK(STR(laAllData[lnYPos,1],4,0)+PADR(laData[1],19)+laData[2]+laAllData[1,T],'YEARINPO')
    *--Get the Cumulate NBKAMT(Sales) and NPosMnth(Purchases) including the Month on Screen
    FOR H = 1 TO laAllData[lnYPos,2]
      lcSalFld  = 'YEARINPO.NBKAMT' + STRTRAN(STR(H,2,0),' ','0')
      lcPurFld  = 'YEARINPO.NPosMnth' + STRTRAN(STR(H,2,0),' ','0')      
      IF laAllData[lnYPos,2] > 1
        lnBkAmt    = lnBkAmt    + EVALUATE(lcSalFld)
        lnTBkAmt   = lnTBkAmt   + EVALUATE(lcSalFld)
        
        lnPosMnth  = lnPosMnth  + EVALUATE(lcPurFld)
        lnTPosMnth = lnTPosMnth + EVALUATE(lcPurFld)
      ELSE
        lnBkAmt    = EVALUATE(lcSalFld)
        lnPosMnth  = EVALUATE(lcPurFld)    
        lnTBkAmt   = EVALUATE(lcSalFld)
        lnTPosMnth = EVALUATE(lcPurFld)
        EXIT
      ENDIF
    ENDFOR
  ELSE
    lnBkAmt    = 0  
    lnPosMnth  = 0
    lnTBkAmt   = 0
    lnTPosMnth = 0

  ENDIF
  *-- Get the Act. Sales/Forecst YTD%    = ((Actual Sales / Sales Forecast)/100)-100
  *-- Get the Act. Purchase/Forecst YTD% = ((Actual Purchases / Purchase Forecast)/100)-100
  laAllData[lnYPos+3,T] = IIF((lnBkAmt*lnSalFrcst)<>0,(((lnBkAmt/lnSalFrcst)/100)-100),0)
  laAllData[lnYPos+5,T] = IIF((lnPosMnth*lnPurFrcst)<>0,(((lnPosMnth/lnPurFrcst)/100)-100),0)
  
  STORE 0 TO lnBkAmt , lnPosMnth  , lnSalFrcst , lnPurFrcst 
ENDFOR

laAllData[lnYPos+3,lnScaleCnt] = IIF((lnTBkAmt*lnTSalFcst)<>0,(((lnTBkAmt/lnTSalFcst)/100)-100),0)
laAllData[lnYPos+5,lnScaleCnt] = IIF((lnTPosMnth*lnTPurFcst)<>0,(((lnTPosMnth/lnTPurFcst)/100)-100),0)


SELECT FORECAST
SET ORDER TO &lcOrder       && return to the Previous Order in File Forecast
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecno 
ENDIF

SELECT (lnAlias)

*--End of lfUpdNPrc