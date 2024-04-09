*:****************************************************************
*: Program file  : POPRDPL.PRG
*: Program desc. : Production Plan program
*: For screen    : POPRDPL.SPX
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Style Purchased Order (PO)
*: Developer     : AHMED MAHER (AMH)
*: Date          : 07/14/2003
*: Tracking Job Number : C#200569,1
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200569,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:C037172,1 ASH 10/12/2003 Apply the logic of pattern while computing 
*:C037172,1                the sales order in the production plan screen.
*:B123056,1 NNA 06/07/2004 Fix Bug that if there are two styles have the same first characters
*:B123056,1 NNA            i.e[415 & 415E19] then report not showing correctly data
*:C122961,1 NNA 06/20/2004 Make Some Changes as Follow :
*:C122961,1 NNA            1 - Calculate the Opening Stock on the Fly to Show any changes in the to Buy Fields in the Screen.
*:C122961,1 NNA            2 - Add the Open Orders to Annual YTD Sales as one figure.
*:C122961,1 NNA            3 - Replace the Open Orders Row with a new Row headed '9999 Years Sales' to Display 
*:C122961,1 NNA                the Previous Year Sales from the Yearinpo file.
*:C122961,1 NNA            4 - For the Annual Purchase and Sales Forecast get its Valuse from ForeCast File
*:C122961,1 NNA            5 - Add the 'OPEN' Qty. on the Sales Order to The Opening Stock before calculating
*:C122961,1 NNA                any figures only if the Sales order Related or linked to a Style PO.
*:C126267,1 NNA 05/13/2005 Make A change by Instead of adding the Open Sales Order quantities to the Opening Stock figure - 
*:C126267,1 NNA            only add open quantities on the Style Purchase Order that is linked to the Sales Order 
*:B132554,1 TMI 06/28/2006 Remove the entry C126267
*:****************************************************************

EXTERNAL ARRAY laData , laDefProc , laKeyField
DECLARE laShowFig[18,8],laAllData[137,1],laWareType[2]

STORE ''  TO lcStyDesc, lcMjrPct, lcClrDesc, lcScFields
STORE ''  TO lcPrvSclS, lcNxtSclS, lcPrvMnthS, lcNxtMnthS

*C122961,1 NNA 06/20/2004 (Begin) Define New Variable that hold the open qty on the Sales order
*STORE 0   TO lnOldValue,lnYear,lnMonth
STORE  0   TO lnOldValue , lnYear , lnMonth , lnLnkOrdQt
*C122961,1 NNA (End)

lnYear   = YEAR(gdSysDate)
lnMonth  = MONTH(gdSysDate)
STORE .F. TO llbrowse,llCanGenPo

STORE SPACE(0) TO laShowFig, lcMonth, lcYear, laAllData

laWareType[1] = 'Single'
laWareType[2] = 'Multiple'
STORE 2   TO lnScaleCnt
STORE 3   TO lnXPos             && the Sizes  Position in the Array 
STORE 3   TO lnYPos				&& the Months Position in the Array 
STORE 12  TO lnZPos
STORE ''  TO lcVendor,lcWareCode,lcPCurr,lcDCurr,lcIType1,lcIType2,lcIType3,lcIType4,lcIType5
lcTmpPoBuy = gfTempName()
lcTmpIndex = gfTempName()
lcTmpPoHdr = gfTempName()
lcTmpPosLn = gfTempName()
lcTmpStyle = gfTempName()
lcTmpStyl1 = gfTempName()

*-- Hold the unit of the foreign curr.
STORE  1  TO lnUnit1,lnUnit2,lnPRate,lnDRate

*-- Hold the number of term,ship,term
STORE  1  TO lnTerm,lnShip,lnType,puType

*--ToolBar external proc.
laDefProc[1]  = .F.              && Disable the control panel prev proc.(lpTopScr)
laDefProc[2]  = .F.              && Disable the control panel prev proc.(lpBtmScr)
laDefProc[3]  = .F.              && Disable the control panel prev proc.(lpNxtScr)
laDefProc[4]  = .F.              && Disable the control panel prev proc.(lpPrvScr)

laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
DECLARE laKeyField [2,4]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'STYLE'
laKeyField[1,4] = 1

laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'STYLE'
laKeyField[2,4] = 2

DIMENSION laTerm[1], laCodInfo [2,10] ,laShip[1]
STORE "" TO laTerm, laCodInfo, laShip

STORE .F. TO llAlowNew
llNoShow = .F.            && Flag to make the screen call the PROCEDURE lpShow evry time it run

*-- To get the Color position and length.
STORE 0 TO lnClrPos,lnClrLen
lcMjrPct = gfItemMask('PM')
lnMjrWid = LEN(lcMjrPct)

=lfGetColor()

llWareHous = ALLTRIM(gfGetMemVar('M_WareHouse'))= 'Y'
llEditExRt = gfGetMemVar('LLEDITEXRA')
llMulCurr  = gfGetMemVar('llMulCurr')
lcIType1   = gfGetMemVar('M_cIType1')
lcIType2   = gfGetMemVar('M_cIType2')
lcIType3   = gfGetMemVar('M_cIType3')
lcIType4   = gfGetMemVar('M_cIType4')
lcIType5   = gfGetMemVar('M_cIType5')

*-- Initialze the arrays for terms,division, and season
laCodInfo[1,01] = "CTERMCODE"    && Field Name
laCodInfo[1,02] = "laTerm"       && Array Name
laCodInfo[1,03] = "lnTerm"       && Popup Name
laCodInfo[1,04] = ""             && Popup Status  ("D"->Default,"A"->All)
laCodInfo[1,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[1,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[1,07] = ""             && Alternative File (For default val.)
laCodInfo[1,08] = ""             && Use this index for the Alternative file.
laCodInfo[1,09] = ""             && Seek this expretion.
laCodInfo[1,10] = "CTERMCODE"        && Alternative Field Name

laCodInfo[2,01] = "SHIPVIA   "
laCodInfo[2,02] = "laShip"
laCodInfo[2,03] = "lnShip"
laCodInfo[2,04] = ""
laCodInfo[2,05] = .F.
laCodInfo[2,06] = .F.
laCodInfo[2,07] = ""
laCodInfo[2,08] = ""
laCodInfo[2,09] = ""
laCodInfo[2,10] = "SHIPVIA"

*C037172,1 ASH 10/12/2003 (Begin)
*-- To get the include pattern No setup.
llUpdPatrn = gfGetMemVar('M_ADDPATNO')
*C037172,1 ASH 10/12/2003 (End)

IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(lcBaseWind)
  lcYear  = STR(YEAR(gdSysDate),4,0)
  lcMonth = CMONTH(gdSysDate)
  FOR lnI = 2 TO 18
    FOR lnJ = 1 TO 8
      laShowFig[lnI,lnJ] = 0
    ENDFOR
  ENDFOR
  
  *-- Create temp cursor to view/edit/add records in the POTOBUY file.
  CREATE table (lcTmpPoBuy) (NYEAR N(4,0),NMONTH N(2,0),STYLE C(19),COLOR C(6),CSIZECODE C(4),;
                              NTOBUYQTY N(10,0))
  INDEX ON STYLE+COLOR+CSIZECODE+STR(NMONTH,2,0)+STR(NYEAR,4,0) TAG (lcTmpPoBuy) OF (lcTmpPoBuy)
  INDEX ON STYLE+COLOR+STR(NMONTH,2,0)+STR(NYEAR,4,0)+CSIZECODE TAG (lcTmpIndex) OF (lcTmpPoBuy)
  SELECT (lcTmpPoBuy)
  SET ORDER TO TAG (lcTmpPoBuy)
  
  *-- Create temp cursor to Browse styles for generate POs.
  CREATE CURSOR (lcTmpStyle) (CSELECT C(1),STYLE C(19),COLOR C(6),NTOTQTY N(10,0),;
                              NMONTH N(2,0),NYEAR N(4,0),CADD_USER C(10),DADD_DATE D)
  INDEX ON STYLE+COLOR+STR(NYEAR,4,0)+STR(NMONTH,2,0) TAG (lcTmpStyle) OF (lcTmpStyle)
  INDEX ON CSELECT+STR(NYEAR,4,0)+STR(NMONTH,2,0)+STYLE+COLOR TAG (lcTmpStyl1) OF (lcTmpStyle)
  SELECT (lcTmpStyle)
  SET ORDER TO TAG (lcTmpStyle)
  =lfGetSelSt()
  
  *-- Create temp cursor to Generate POs.
  SELECT POSHDR
  =AFIELDS(laFldStu)
  CREATE CURSOR (lcTmpPoHdr) FROM ARRAY laFldStu
  INDEX ON CSTYTYPE+PO TAG (lcTmpPoHdr) OF (lcTmpPoHdr)
  
  SELECT POSLN
  =AFIELDS(laFldStu)
  CREATE CURSOR (lcTmpPosLn) FROM ARRAY laFldStu
  INDEX ON CSTYTYPE+PO+STYLE TAG (lcTmpPosLn) OF (lcTmpPosLn)
  
  *-- Screen fields variable, to be used to build the laData array.
  SELECT POTOBUY
  lcScFields ='STYLE,COLOR'
  SCATTER FIELDS &lcScFields. TO laData BLANK
ENDIF

*--Activate Options pad.
=lfActPad()

DO (gcScrDir+gcWinAppl+"\POPRDPL.SPX")

IF glQuitting
  USE IN (lcTmpPoBuy)
  USE IN (lcTmpPoHdr)
  USE IN (lcTmpPosLn)
  USE IN (lcTmpStyle)
ENDIF

*--Release Option pad also
RELEASE PAD _Option OF _MSYSMENU
*-- End OF Code.

*:*************************************************************
*: Name      : lfOldValue
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/14/2003
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
*: Example   : =lf..()
*:*************************************************************
*:
FUNCTION lfoldvalue

IF lnXPos + VAL(LEFT(RIGHT(SYS(18),2),1)) - 1 >= lnScaleCnt
  RETURN .F.
ENDIF

lnOldValue = EVALUATE(SYS(18))
*-- End of lfoldvalue.

*:*************************************************************
*: Name      : lfvStyle
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/14/2003
*: Purpose   : Valid function to validate style field.
*:*************************************************************
*: Calls     : 
*:             Procedures : 
*:             Functions  : gfStyBrw()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lf..()
*:*************************************************************
*:
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

*:*************************************************************
*: Name      : lfvColor
*: Developer : Ahmed Maher - (AMH)
*: Date      : 07/14/2003
*: Purpose   : Valid function to validate Color.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lf..()
*:*************************************************************
*:
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
  laData[1] = PADR(laData[1],lnMjrWid)
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
*: Name      : lpShow
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/14/2003
*: Purpose   : Show function.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lpShow()
*:*************************************************************
*:
FUNCTION lpShow

DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]
    STORE ''         TO laData[1] , laData[2] , lcStyDesc , lcClrDesc
    STORE 'DISABLE'  TO lcPrvSclS, lcNxtSclS, lcPrvMnthS, lcNxtMnthS
    STORE SPACE(0)   TO laShowFig, laAllData
    STORE gcBaseCurr TO lcPCurr,lcDCurr
    FOR lnI = 2 TO 18
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
    SHOW GET pbGenPo    DISABLE
    FOR lnI = 1 TO 8
      SHOW GET laShowFig[9,lnI] DISABLE
    ENDFOR
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
    IF laAllData[ALEN(laAllData,1)-2,lnScaleCnt] + laAllData[ALEN(laAllData,1)-119,lnScaleCnt] > 0
      SHOW GET pbGenPo  ENABLE
    ELSE
      SHOW GET pbGenPo  DISABLE
    ENDIF
  
  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]
    SHOW GET ibStyle    DISABLE
    SHOW GET ibColor    DISABLE
    SHOW GET pbPrvsScl  &lcPrvSclS.
    SHOW GET pbNxtScl   &lcNxtSclS.
    SHOW GET pbPrvsMnth &lcPrvMnthS.
    SHOW GET pbNxtMnth  &lcNxtMnthS.
    SHOW GET pbGenPo    DISABLE
    FOR lnI = 1 TO 8
      SHOW GET laShowFig[9,lnI] ENABLE
    ENDFOR
    laData[2] = SUBSTR(laData[1],lnClrPos,lnClrLen)
    laData[1] = PADR(laData[1],lnMjrWid)
  
  ***--- A D D   M O D E ---***
  CASE laScrMode[4]
    *-- recollect the data again.
    =lfCollect()
    =lfShowFig()
    
    SHOW GET ibStyle    DISABLE
    SHOW GET ibColor    DISABLE
    SHOW GET pbGenPo    DISABLE

ENDCASE
SHOW GET pbUsrFields DISABLE
SHOW GET pbNotePad   DISABLE
SHOW GET pbAudTrail  DISABLE
=lfChkLstRc()
*-- End OF lpShow.

*:*************************************************************
*: Name      : lfCollect
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/14/2003
*: Purpose   : Function to collect the data Temp Files.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfCollect ()
*:*************************************************************
*:
FUNCTION lfCollect

PRIVATE lnAlias,lnRowCnt,lnNewScale,lnI,lcI,lnJ,lnMonthRow,laOrder,laWip,lnSclCnt,lnRecNo

*C122961,1 NNA 06/20/2004 (Begin) Define a new  Array that hold the Purchase Forecast Values
*PRIVATE laTotOrder,laTotWip,laForCast 
PRIVATE laTotOrder , laTotWip , laForCast , laPurFrCst
*C122961,1 NNA (End)

lnAlias  = SELECT(0)
lnRecNo  = RECNO('STYLE')
lnRowCnt = (27 - lnMonth) * 9
lnXPos   = 3
lnYPos   = 3
lnZPos   = lnRowCnt - 123

*C122961,1 NNA 06/20/2004 (Begin) Declare the laPurFrCst Array
*DECLARE laOrder[8],laWip[8],laTotOrder[8],laTotWip[8],laForCast[8]
DECLARE laOrder[8] ,laWip[8] ,laTotOrder[8] ,laTotWip[8] ,laForCast[8] ,laPurFrCst[8]
*C122961,1 NNA (End)

SELECT (lcTmpPoBuy)
ZAP
  
IF laScrMode[2]
  *-- Get the save data.
  SELECT POTOBUY
  SET ORDER TO TAG POTOBUYL
  IF SEEK(PADR(laData[1],19)+laData[2])
    SCAN REST WHILE STYLE+COLOR = PADR(laData[1],19)+laData[2];
                FOR (NYEAR = lnYear .AND. NMONTH >= lnMonth) .OR. (NYEAR = lnYear + 1)
      SCATTER MEMVAR
      INSERT INTO (lcTmpPoBuy) FROM MEMVAR
    ENDSCAN
  ENDIF
  SET ORDER TO TAG POTOBUY
  =SEEK(PADR(laData[1],19)+laData[2])
ENDIF

*-- Collect data for all sizes and all months
STORE 2 TO lnScaleCnt,lnSclCnt
=lfGetScale()
DECLARE laAllData[lnRowCnt+2,lnScaleCnt]
FOR lnI = 1 TO lnRowCnt  && loop for add year and month to the first two columns in the array.
  laAllData[lnI+2,1] = lnYear + INT((lnMonth + INT((lnI-1)/9) - 1)/13)
  laAllData[lnI+2,2] = MOD((lnMonth + INT((lnI-1)/9) - 1),13) + 1
ENDFOR

SELECT STYLE
IF SEEK(PADR(laData[1],lnMjrWid))
  SCAN REST WHILE STYLE = PADR(laData[1],lnMjrWid);
              FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
    IF SEEK('S'+SCALE,'SCALE')
      *ash1
      lnRecNo1 = RECNO('STYLE')
      lnNewScale = lnSclCnt + 1
      lnSclCnt   = lnSclCnt + SCALE.CNT

      *C122961,1 NNA 06/20/2004 (Begin) Store 0 to the laPurFrCst Array
      *STORE 0 TO laTotOrder,laTotWip,laForCast
      STORE 0 TO laTotOrder ,laTotWip ,laForCast ,laPurFrCst
      *C122961,1 NNA (End)      
      
      *ash1 add flag to zero the total order array.
      llFlag = .T.
      FOR lnJ = 3 TO lnRowCnt+2  && loop for all rows of months.
        lnMonthRow = MOD(lnJ-3,9) + 1 && get the row number relative to month
        STORE 0 TO laOrder,laWip
        *ash1 zero the total order array if the year changed
        IF llFlag AND lnYear < laAllData[lnJ,1]
          STORE 0 TO laTotOrder
          llFlag = .F.
        ENDIF
        
        IF lnMonthRow = 6 .AND. laAllData[lnJ,2] < 13

          =lfGetOrder()
        ENDIF
        
        IF lnMonthRow = 7 .AND. laAllData[lnJ,2] < 13
          =lfGetWip()
        ENDIF
        
        IF lnMonthRow = 1 .AND. laAllData[lnJ,2] = 13
          =lfGetSFCst()
        ENDIF
        
        FOR lnI = lnNewScale TO lnSclCnt
          lcI = STR(lnI - lnNewScale + 1,1)
          laAllData[1,lnI] = SCALE + lcI  && add the scale code to the first row in the array.
          laAllData[2,lnI] = EVALUATE('SCALE.SZ'+lcI)  && add the size desc. to the snd. row.
       
          DO CASE
            CASE lnMonthRow = 1  && Opening Stock/Sales Forecast.
              IF laAllData[lnJ,2] = 13
                laAllData[lnJ,lnI] = laForCast[lnI-lnNewScale+1]
              ELSE
                =lfGetStock(lnJ,lnI)
              ENDIF
            
            CASE lnMonthRow = 2  && Prior Yr Sales/Sales YTD.
              IF laAllData[lnJ,2] = 13
                =lfGetSales(lnJ,lnI,laAllData[lnJ,1],'TOTSHIP')
              ELSE
                =lfGetSales(lnJ,lnI,laAllData[lnJ,1]-1,'SHPMNTH',.T.)
              ENDIF
            
            CASE lnMonthRow = 3  && Tot Yr Prior Sales/Open Orders.
              IF laAllData[lnJ,2] = 13

                *C122961,1 NNA 06/20/2004 (Begin) get the previous year sales values instead of Open Orders
                *laAllData[lnJ,lnI] = laTotOrder[lnI-lnNewScale+1]
                = lfPrv1YSal(lnJ,lnI,laAllData[lnJ,1])
                *C122961,1 NNA (End)

              ELSE
                =lfGetSales(lnJ,lnI,laAllData[lnJ,1]-1,'CUMSHIP',.T.)
              ENDIF
            
            CASE lnMonthRow = 4  && Cur Yr Sales/Remain sfcast.
              IF laAllData[lnJ,2] = 13
                laAllData[lnJ,lnI] = laAllData[lnJ-3,lnI] - laAllData[lnJ-2,lnI]
              ELSE
                =lfGetSales(lnJ,lnI,laAllData[lnJ,1],'SHPMNTH',.T.)
              ENDIF
            
            CASE lnMonthRow = 5  && Tot Yr Cur Sales/Purchase f/cast.
              IF laAllData[lnJ,2] = 13

                *C122961,1 NNA 06/20/2004 (Begin) get Purchase f/cast From the Forecast File not the Style Plan
                *laAllData[lnJ,lnI] = EVALUATE('STYLE.PLAN'+lcI)
                laAllData[lnJ,lnI] = laPurFrCst[lnI-lnNewScale+1]               
                *C122961,1 NNA (End)                

              ELSE
                =lfGetSales(lnJ,lnI,laAllData[lnJ,1],'CUMSHIP',.T.)
              ENDIF
            
            CASE lnMonthRow = 6  && Open Orders/Purchase YTD.
              IF laAllData[lnJ,2] = 13
                =lfGetPurch(lnJ,lnI)
              ELSE
                laAllData[lnJ,lnI] = laOrder[lnI-lnNewScale+1]
              ENDIF
            
            CASE lnMonthRow = 7  && Open Wip/Planned to buy.
              IF laAllData[lnJ,2] = 13
                =lfGetPoBuy(lnJ,lnI)
              ELSE
                laAllData[lnJ,lnI] = laWip[lnI-lnNewScale+1]
              ENDIF
            
            CASE lnMonthRow = 8  && To Buy/Remain to buy.
              IF laAllData[lnJ,2] = 13
                laAllData[lnJ,lnI] = laAllData[lnJ-3,lnI] -;
                                     laAllData[lnJ-2,lnI] -;
                                     laAllData[lnJ-1,lnI]
              ELSE
                =lfGetPoBuy(lnJ,lnI)
              ENDIF
            
            CASE lnMonthRow = 9  && Closing Stock.
              IF laAllData[lnJ,2] = 13
                *-- nothing to do for the last row
              ELSE
                =lfGetClose(lnJ,lnI)
              ENDIF
            
          ENDCASE
        ENDFOR
      ENDFOR
      *ash1
      GO lnRecNo1 IN STYLE
    ENDIF
  ENDSCAN
ENDIF

*-- Add the total column.
laAllData[1,lnScaleCnt] = 'TOTAL'
laAllData[2,lnScaleCnt] = 'TOTAL'

FOR lnI = 1 TO lnRowCnt-1  && loop for add totals to the last column in the array.
  IF lnI # lnZPos + 6
    laAllData[lnI+2,lnScaleCnt] = 0
    FOR lnJ = 3 TO lnScaleCnt - 1
      laAllData[lnI+2,lnScaleCnt] = laAllData[lnI+2,lnScaleCnt] + laAllData[lnI+2,lnJ]
    ENDFOR
  ENDIF
ENDFOR

GO lnRecNo IN STYLE
SELECT (lnAlias)
*-- End OF lfCollect

*:*************************************************************
*: Name      : lpSavScr
*: Developer : AHMED MAHER
*: Date      : 07/14/2003
*: Purpose   : Save/Update.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
PROCEDURE lpSavScr

PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT POTOBUY
SET ORDER TO TAG POTOBUYL
SELECT (lcTmpPoBuy)
m.cAdd_User = gcUser_id
m.cAdd_Time = TIME()
m.dAdd_Date = gdSysDate
SCAN
  SCATTER MEMVAR
  IF SEEK(STYLE+COLOR+CSIZECODE+STR(NMONTH,2,0)+STR(NYEAR,4,0),'POTOBUY')
    REPLACE POTOBUY.NTOBUYQTY WITH m.nToBuyQty
  ELSE
    INSERT INTO POTOBUY FROM MEMVAR
  ENDIF
ENDSCAN
SELECT POTOBUY
SET ORDER TO TAG POTOBUY
=SEEK(PADR(laData[1],19)+laData[2])
=lfGetSelSt()
SELECT (lnAlias)
*-- End Of lpSavScr.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 07/10/2003
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

*:*************************************************************
*: Name      : lfGetStock
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/17/2003
*: Purpose   : Function to Get the Opening Stock.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lnRow,lnCol
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfGetStock (lnJ,lnI)
*:*************************************************************
*:
FUNCTION lfGetStock
PARAMETERS lnRow,lnCol

*C122961,1 NNA 06/20/2004 (Begin) Hold the current Alias
STORE 0 TO lnAlias
lnAlias  = SELECT(0)
*C122961,1 NNA (End)

IF laAllData[lnRow,1] = lnYear .AND. laAllData[lnRow,2] = lnMonth

*C122961,1 NNA 06/20/2004 (Begin) Check if there are orders Related to PO's to another styles
*C122961,1 NNA            that has the same Pattern as the Style Processed
*laAllData[lnRow,lnCol] = EVALUATE('STYLE.STK'+lcI)
laAllData[lnRow,lnCol] = EVALUATE('STYLE.STK'+lcI)+IIF(lfLnkToPo(lnRow),lnLnkOrdQt,0)
*C122961,1 NNA (End)

ELSE
  IF lnRow = lnZPos + 9
  
*C122961,1 NNA 06/20/2004 (Begin) Check if there are orders Related to PO's to another styles
*C122961,1 NNA            that has the same Pattern as the Style Processed
    *laAllData[lnRow,lnCol] = laAllData[lnZPos-1,lnCol]
  *ELSE
    *laAllData[lnRow,lnCol] = laAllData[lnRow-1,lnCol]
    laAllData[lnRow,lnCol] = laAllData[lnZPos-1,lnCol] + IIF(lfLnkToPo(lnZPos - 1),lnLnkOrdQt,0)
  ELSE
    laAllData[lnRow,lnCol] = laAllData[lnRow-1,lnCol] + IIF(lfLnkToPo(lnRow),lnLnkOrdQt,0)
  ENDIF
ENDIF

*C122961,1 NNA 06/20/2004 (Begin) Select the Previous file
SELECT(lnAlias)
*C122961,1 NNA (End)

*-- end of lfGetStock.

*:*************************************************************
*: Name      : lfGetSales
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/17/2003
*: Purpose   : Function to Get the Prior Yr/Tot Yr Prior/Cur Yr/Tot Yr Cur.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lnRow,lnCol,lnForYear,lcField,llForMonth
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfGetSales (lnJ,lnI,lnYear,.T.)
*:*************************************************************
*:
FUNCTION lfGetSales
PARAMETERS lnRow,lnCol,lnForYear,lcField,llForMonth

lcField = 'YEARINPO.N' + lcField
IF llForMonth
  lcField = lcField + STRTRAN(STR(laAllData[lnRow,2],2,0),' ','0')
ENDIF
IF SEEK(STR(lnForYear,4,0)+PADR(laData[1],19)+laData[2]+laAllData[1,lnCol],'YEARINPO')

  *C122961,1 NNA 06/20/2004 (Begin) Add the Open Orders to the Sales YTD
  IF laAllData[lnJ,2] = 13
    laAllData[lnRow,lnCol] = EVALUATE(lcField) + laTotOrder[VAL(LCI)]
  ELSE
  *C122961,1 NNA (End)  

    laAllData[lnRow,lnCol] = EVALUATE(lcField)
  ENDIF
ELSE
  laAllData[lnRow,lnCol] = 0
ENDIF
*-- end of lfGetSales.

*:*************************************************************
*: Name      : lfGetOrder
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/17/2003
*: Purpose   : Function to Get the Open Orders.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfGetOrder()
*:*************************************************************
*:
FUNCTION lfGetOrder

PRIVATE lnAlias,ldStrDate,ldEndDate,lcForExp,lcDateSet
lnAlias = SELECT(0)

*-- Set relation for order & WIP data.
SELECT ORDLINE
SET RELATION TO CORDTYPE+ORDER INTO ORDHDR
  
IF SEEK(STYLE.STYLE,'ORDLINE')
  
  lcDateSet = SET('DATE')
  SET DATE TO MDY
  ldEndDate = lfLstMnDay(laAllData[lnJ,2],laAllData[lnJ,1])
  IF laAllData[lnJ,1] = lnYear .AND. laAllData[lnJ,2] = lnMonth
    lcForExp = 'ORDHDR.START<= ldEndDate'
  ELSE
    ldStrDate = CTOD(STRTRAN(STR(laAllData[lnJ,2],2,0),' ','0') + '/01/' + STR(laAlldata[lnJ,1],4,0))
    lcForExp = 'BETWEEN(ORDHDR.START,ldStrDate,ldEndDate)'
  ENDIF
  SET DATE TO &lcDateSet.
  
  SELECT ORDLINE
    SUM REST WHILE STYLE = STYLE.STYLE FOR &lcForExp. .AND. ORDHDR.STATUS $ 'OHB';
        QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8 TO ARRAY laOrder

  *C037172,1 ASH 10/12/2003 (Begin) Add the order qty for the style that saved as pattern.
  IF llUpdPatrn
    SELECT STYLE  
    lcPattern  = Pattern
    lcStyle    = STYLE    
    lcScale    = Scale    
    lnStyRec   = RECNO()
    lcStyOrd   = ORDER()
    SET ORDER TO Stylepat 
    =SEEK(lcPattern)
    SCAN WHILE PATTERN = lcPattern FOR (Style # lcStyle .AND. SCALE = lcScale .AND. SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2])
      IF SEEK(Style,'OrdLine')
        SELECT ORDLINE
        SCAN WHILE STYLE = Style.Style FOR &lcForExp. .AND. ORDHDR.STATUS $ 'OHB' 
          FOR lnCnt = 1 to 8
            lcCnt = STR(lnCnt,1)
            laOrder[lnCnt] = laOrder[lnCnt] + Qty&lcCnt
          ENDFOR
        ENDSCAN
      ENDIF
    ENDSCAN
    GOTO lnStyRec 
    SET ORDER TO lcStyOrd 
  ENDIF
  *C037172,1 ASH 10/12/2003 (End)
    FOR lnK = 1 TO 8
      laTotOrder[lnK] = laTotOrder[lnK] + laOrder[lnK]
    ENDFOR

*B123056,1 NNA 06/07/2004 (Begin) If there is no order line record for the style that Proccess 
*B123056,1 NNA 0                  I search for the styles that Linked to  the same Pattern NO.
ELSE
  lcDateSet = SET('DATE')
  SET DATE TO MDY
  ldEndDate = lfLstMnDay(laAllData[lnJ,2],laAllData[lnJ,1])
  IF laAllData[lnJ,1] = lnYear .AND. laAllData[lnJ,2] = lnMonth
    lcForExp = 'ORDHDR.START<= ldEndDate'
  ELSE
    ldStrDate = CTOD(STRTRAN(STR(laAllData[lnJ,2],2,0),' ','0') + '/01/' + STR(laAlldata[lnJ,1],4,0))
    lcForExp = 'BETWEEN(ORDHDR.START,ldStrDate,ldEndDate)'
  ENDIF
  SET DATE TO &lcDateSet.
  SELECT ORDLINE
  SUM REST WHILE STYLE = STYLE.STYLE FOR &lcForExp. .AND. ORDHDR.STATUS $ 'OHB';
      QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8 TO ARRAY laOrder

  IF llUpdPatrn
    SELECT STYLE  
    lcPattern  = Pattern
    lcStyle    = STYLE    
    lcScale    = Scale    
    lnStyRec   = RECNO()
    lcStyOrd   = ORDER()
    SET ORDER TO Stylepat 
    =SEEK(lcPattern)
    SCAN WHILE PATTERN = lcPattern FOR (Style # lcStyle .AND. SCALE = lcScale .AND. SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2])
      IF SEEK(Style,'OrdLine')
        SELECT ORDLINE
        SCAN WHILE STYLE = Style.Style FOR &lcForExp. .AND. ORDHDR.STATUS $ 'OHB' 
          FOR lnCnt = 1 to 8
            lcCnt = STR(lnCnt,1)
            laOrder[lnCnt] = laOrder[lnCnt] + Qty&lcCnt
          ENDFOR
        ENDSCAN
      ENDIF
    ENDSCAN
    GOTO lnStyRec 
    SET ORDER TO lcStyOrd 
      FOR lnK = 1 TO 8
        laTotOrder[lnK] = laTotOrder[lnK] + laOrder[lnK]
      ENDFOR
  ENDIF
*B123056,1 NNA (End)

ENDIF
SELECT ORDLINE
SET RELATION TO
SELECT (lnAlias)
*-- end of lfGetOrder.

*:*************************************************************
*: Name      : lfGetWip
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/17/2003
*: Purpose   : Function to Get the Open WIP.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lnRow,lnCol
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfGetWIP (lnJ,lnI)
*:*************************************************************
*:
FUNCTION lfGetWip
PARAMETERS lnRow,lnCol

PRIVATE lnAlias,ldStrDate,ldEndDate,lcForExp,lcDateSet
lnAlias = SELECT(0)

*-- Set relation for order & WIP data.
SELECT POSLN
SET RELATION TO CSTYTYPE+PO INTO POSHDR
  
IF SEEK(STYLE.STYLE,'POSLN')
  
  lcDateSet = SET('DATE')
  SET DATE TO MDY
  ldEndDate = lfLstMnDay(laAllData[lnJ,2],laAllData[lnJ,1])
  IF laAllData[lnJ,1] = lnYear .AND. laAllData[lnJ,2] = lnMonth
    lcForExp = 'POSHDR.AVAILABLE <= ldEndDate'
  ELSE
    ldStrDate = CTOD(STRTRAN(STR(laAllData[lnJ,2],2,0),' ','0') + '/01/' + STR(laAllData[lnJ,1],4,0))
    lcForExp  = 'BETWEEN(POSHDR.AVAILABLE,ldStrDate,ldEndDate)'
  ENDIF
  SET DATE TO &lcDateSet.
  
  SELECT POSLN
  SUM REST WHILE STYLE = STYLE.STYLE FOR &lcForExp. .AND. POSHDR.STATUS $ 'OHA' .AND.;
      !(TRANCD$'36') QTY1*IIF(TRANCD='1',1,-1),QTY2*IIF(TRANCD='1',1,-1),;
                     QTY3*IIF(TRANCD='1',1,-1),QTY4*IIF(TRANCD='1',1,-1),;
                     QTY5*IIF(TRANCD='1',1,-1),QTY6*IIF(TRANCD='1',1,-1),;
                     QTY7*IIF(TRANCD='1',1,-1),QTY8*IIF(TRANCD='1',1,-1) TO ARRAY laWip
  
  FOR lnK = 1 TO 8
    laTotWip[lnK] = laTotWip[lnK] + laWip[lnK]
  ENDFOR
ENDIF
SELECT POSLN
SET RELATION TO
SELECT (lnAlias)
*-- end of lfGetWip.

*:*************************************************************
*: Name      : lfGetPoBuy
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/17/2003
*: Purpose   : Function to Get the To Buy.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lnRow,lnCol
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfGetPoBuy (lnJ,lnI)
*:*************************************************************
*:
FUNCTION lfGetPoBuy
PARAMETERS lnRow,lnCol

IF SEEK(PADR(laData[1],19)+laData[2]+laAllData[1,lnCol]+;
        STR(laAllData[lnRow,2],2,0)+STR(laAllData[lnRow,1],4,0),lcTmpPoBuy)
  laAllData[lnRow,lnCol] = EVALUATE(lcTmpPoBuy+'.NTOBUYQTY')
ELSE
  laAllData[lnRow,lnCol] = 0
ENDIF
*-- end of lfGetPoBuy.

*:*************************************************************
*: Name      : lfGetClose
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/17/2003
*: Purpose   : Function to Get the Closing Stock.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lnRow,lnCol
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfGetClose (lnJ,lnI)
*:*************************************************************
*:
FUNCTION lfGetClose
PARAMETERS lnRow,lnCol

laAllData[lnRow,lnCol] = laAllData[lnRow-8,lnCol] + laAllData[lnRow-2,lnCol] +;
                         laAllData[lnRow-1,lnCol] - laAllData[lnRow-3,lnCol]
*-- end of lfGetClose.

*:*************************************************************
*: Name      : lfLstMnDay
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/17/2003
*: Purpose   : Function to Get the Last Day of the month.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lnForMonth,lnForYear
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfLstMnDay(1,2002)
*:*************************************************************
*:
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
*-- end of lfLstMnDay.

*:*************************************************************
*: Name      : lfGetSFCst
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/20/2003
*: Purpose   : Function to Get the Sales Forecast.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  = lfGetSFCst()
*:*************************************************************
*:
FUNCTION lfGetSFCst

PRIVATE lnAlias
lnAlias = SELECT(0)

*C122961,1 NNA 06/20/2004 (Begin) get the Annual purchase Forecast and the Annual Sales Forecast 
*C122961,1 NNA                    from the Forecast Filemonth 13
*IF SEEK(STR(laAllData[lnJ,1],4,0)+'52'+STYLE.STYLE,'FORCAST')
*  SELECT FORCAST
*  SUM REST WHILE STR(NYEAR,4)+STR(NWEEK,2)+STYLE = STR(laAllData[lnJ,1],4,0)+'52'+STYLE.STYLE;
      NFORQTY1,NFORQTY2,NFORQTY3,NFORQTY4,NFORQTY5,NFORQTY6,NFORQTY7,NFORQTY8 TO ARRAY laForCast
*ENDIF
IF SEEK(STR(laAllData[lnJ,1],4,0)+'13'+STYLE.STYLE,'FORECAST')
  SELECT FORECAST
  SCAN REST WHILE STR(NYEAR,4)+STR(NMONTH,2)+STYLE = STR(laAllData[lnJ,1],4,0)+'13'+STYLE.STYLE 
    FOR I = 1 TO 8
      lcSiz = STR(I,1)
      laForCast [I] = laForCast [I] + NSALFRCST&lcSiz
      laPurFrCst[I] = laPurFrCst[I] + NPURFRCST&lcSiz      
    ENDFOR
  ENDSCAN
ENDIF
*C122961,1 NNA (End)

SELECT (lnAlias)
*-- End of lfGetSFCst.

*:*************************************************************
*: Name      : lfGetPurch
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/20/2003
*: Purpose   : Function to Get the Purchase YTD.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lnRow,lnCol
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfGetPurch(lnJ,lnI)
*:*************************************************************
*:
FUNCTION lfGetPurch
PARAMETERS lnRow,lnCol

IF SEEK(STR(laAllData[lnRow,1],4,0)+PADR(laData[1],19)+laData[2]+laAllData[1,lnCol],'YEARINPO')
  laAllData[lnRow,lnCol] = YEARINPO.NTOTPOS
ELSE
  laAllData[lnRow,lnCol] = 0
ENDIF
*-- end of lfGetPurch.

*:*************************************************************
*: Name      : lfShowFig
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/20/2003
*: Purpose   : Function to Show the current sizes and month.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  = lfShowFig()
*:*************************************************************
*:
FUNCTION lfShowFig

PRIVATE lnI,lnJ,lnRow,lnCol,lcAbleSts
lcAbleSts = IIF(laScrMode[2],'DIS','EN') + 'ABLE'

FOR lnI = 1 TO 8
  lnCol = lnXPos+lnI-1
  FOR lnJ = 1 TO 18
    DO CASE
      CASE lnJ = 1           && sizes
        lnRow = lnJ+1
      
      CASE BETWEEN(lnJ,2,10) && Month Data
        lnRow = lnYPos+lnJ-2
      
      CASE lnJ > 10          && Year Data
        lnRow = lnZPos+lnJ-11
      
    ENDCASE
    laShowFig[lnJ,lnI] = IIF(lnScaleCnt<lnCol,'',laAllData[lnRow,lnCol])
  ENDFOR
  SHOW GET laShowFig[9,lnI] &lcAbleSts.
ENDFOR

STORE 'ENABLE' TO lcPrvSclS, lcNxtSclS, lcPrvMnthS, lcNxtMnthS
IF lnXPos = 3
  lcPrvSclS = 'DISABLE'
ENDIF

IF lnScaleCnt < lnXPos+7
  lcNxtSclS = 'DISABLE'
ENDIF

IF lnYPos = 3
  lcPrvMnthS = 'DISABLE'
ENDIF

IF lnYPos + 18 > ALEN(laAllData,1)
  lcNxtMnthS = 'DISABLE'
ENDIF

SHOW GET pbPrvsScl  &lcPrvSclS.
SHOW GET pbNxtScl   &lcNxtSclS.
SHOW GET pbPrvsMnth &lcPrvMnthS.
SHOW GET pbNxtMnth  &lcNxtMnthS.

=lfGetMonth()
=lfRefresh()
*-- end of lfShowFig.

*:*************************************************************
*: Name      : lfChangPos
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/20/2003
*: Purpose   : Valid funtion of the pbprvsscl button
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lcPos,lnValue
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  = lfChangPos()
*:*************************************************************
*:
FUNCTION lfChangPos
PARAMETERS lcPos,lnValue

PRIVATE lcVarName
lcVarName = 'ln' + lcPos + 'Pos'

&lcVarName. = EVALUATE(lcVarName) + lnValue
IF lcPos = 'Y'
  lnXPos = 3
  
  IF lnYPos = lnZPos
    lnYPos = lnYPos + 9
    lnZPos = lnZPos + 117
  ENDIF
  
  IF lnYPos = lnZPos - 117
    lnZPos = lnYPos
    lnYPos = lnYPos - 9
  ENDIF
ENDIF

=lfShowFig()
*-- end of lfChangPos.

*:*************************************************************
*: Name      : lfGetScale
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/20/2003
*: Purpose   : Function to get the scale count of style.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  = lfGetScale()
*:*************************************************************
*:
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
*-- end of lfGetScale.

*:*************************************************************
*: Name      : lfGetMonth
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/20/2003
*: Purpose   : Function to Get the month name.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lnForMonth,lnForYear
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfGetMonth()
*:*************************************************************
*:
FUNCTION lfGetMonth

PRIVATE lcDateSet,ldFrsMnDay

lcDateSet = SET('DATE')
SET DATE TO MDY
ldFrsMnDay = CTOD(STRTRAN(STR(laAllData[lnYPos,2],2,0),' ','0')+'/01/'+STR(laAllData[lnYPos,1],4,0))
lcMonth = CMONTH(ldFrsMnDay)
lcYear  = STR(laAllData[lnYPos,1],4,0)
SET DATE TO &lcDateSet.
*-- end of lfGetMonth.

*:*************************************************************
*: Name      : lfvToBuy
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/21/2003
*: Purpose   : Valid function of the to buy fields.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lnCol
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : =lfvToBuy(1)
*:*************************************************************
*:
FUNCTION lfvToBuy
PARAMETERS lnCol

*C122961,1 NNA 06/20/2004 (Begin) Define New Variable
*PRIVATE lnAlias 
PRIVATE lnAlias , lnRowCnt
*-- get the lenght of the array that hold the months for two years ( the Current and the next)
lnRowCnt = (27 - lnMonth) * 9        
*C122961,1 NNA (End)

lnAlias = SELECT(0)

*-- Get To Buy line.
laAllData[lnYPos+7,lnXPos+lnCol-1] = laShowFig[9,lnCol]
laAllData[lnYPos+7,lnScaleCnt]     = laAllData[lnYPos+7,lnScaleCnt] +;
                                     laShowFig[9,lnCol]             -;
                                     lnOldValue
IF lnScaleCnt < lnXPos+7
  laShowFig[9,lnScaleCnt-lnXPos+1] = laAllData[lnYPos+7,lnScaleCnt]
  SHOW GET laShowFig[9,lnScaleCnt-lnXPos+1]
ENDIF

*-- Get Closing stock line.
=lfGetClose(lnYPos+8,lnXPos+lnCol-1)
laShowFig[10,lnCol] = laAllData[lnYPos+8,lnXPos+lnCol-1]
laAllData[lnYPos+8,lnScaleCnt] = laAllData[lnYPos+8,lnScaleCnt] +;
                                 laShowFig[9,lnCol]             -;
                                 lnOldValue
IF lnScaleCnt < lnXPos+7
  laShowFig[10,lnScaleCnt-lnXPos+1] = laAllData[lnYPos+8,lnScaleCnt]
  SHOW GET laShowFig[10,lnScaleCnt-lnXPos+1]
ENDIF
*C122961,1 NNA 06/20/2004 (Begin) Update the laAllData Array by the new Values that Entered in 
*C122961,1 NNA            the To Buy Fields 
= lfUpDate()
*C122961,1 NNA (End)
*-- Get Planning to buy line.
laAllData[lnZPos+6,lnXPos+lnCol-1] = laAllData[lnZPos+6,lnXPos+lnCol-1] +;
                                     laShowFig[9,lnCol]                 -;
                                     lnOldValue
laShowFig[17,lnCol] = laAllData[lnZPos+6,lnXPos+lnCol-1]
laAllData[lnZPos+6,lnScaleCnt]     = laAllData[lnZPos+6,lnScaleCnt]     +;
                                     laShowFig[9,lnCol]                 -;
                                     lnOldValue
IF lnScaleCnt < lnXPos+7
  laShowFig[17,lnScaleCnt-lnXPos+1] = laAllData[lnZPos+6,lnScaleCnt]
  SHOW GET laShowFig[17,lnScaleCnt-lnXPos+1]
ENDIF

*-- Get Remain to buy line.
laAllData[lnZPos+7,lnXPos+lnCol-1] = laAllData[lnZPos+4,lnXPos+lnCol-1] -;
                                     laAllData[lnZPos+5,lnXPos+lnCol-1] -;
                                     laAllData[lnZPos+6,lnXPos+lnCol-1]
laShowFig[18,lnCol] = laAllData[lnZPos+7,lnXPos+lnCol-1]
laAllData[lnZPos+7,lnScaleCnt]     = laAllData[lnZPos+4,lnScaleCnt]     -;
                                     laAllData[lnZPos+5,lnScaleCnt]     -;
                                     laAllData[lnZPos+6,lnScaleCnt]
IF lnScaleCnt < lnXPos+7
  laShowFig[18,lnScaleCnt-lnXPos+1] = laAllData[lnZPos+7,lnScaleCnt]
  SHOW GET laShowFig[18,lnScaleCnt-lnXPos+1]
ENDIF

*-- Update the temp table for the current month.
SELECT (lcTmpPoBuy)
IF !SEEK(PADR(laData[1],19)+laData[2]+laAllData[1,lnXPos+lnCol-1]+;
         STR(laAllData[lnYPos+7,2],2,0)+STR(laAllData[lnYPos+7,1],4,0))
  APPEND BLANK
  REPLACE NYEAR     WITH laAllData[lnYPos+7,1],;
          NMONTH    WITH laAllData[lnYPos+7,2],;
          STYLE     WITH PADR(laData[1],19),;
          COLOR     WITH laData[2],;
          CSIZECODE WITH laAllData[1,lnXPos+lnCol-1]
ENDIF
REPLACE NTOBUYQTY WITH laShowFig[9,lnCol]

*-- Update the temp table for the total month.
IF !SEEK(PADR(laData[1],19)+laData[2]+laAllData[1,lnXPos+lnCol-1]+'13'+STR(laAllData[lnYPos+7,1],4,0))
  APPEND BLANK
  REPLACE NYEAR     WITH laAllData[lnYPos+7,1],;
          NMONTH    WITH 13,;
          STYLE     WITH PADR(laData[1],19),;
          COLOR     WITH laData[2],;
          CSIZECODE WITH laAllData[1,lnXPos+lnCol-1]
ENDIF
REPLACE NTOBUYQTY WITH NTOBUYQTY + laShowFig[9,lnCol] - lnOldValue
SELECT (lnAlias)

IF lnCol = 8
  =lfChangPos('X',8)
  IF lnXPos < lnScaleCnt
    _CUROBJ = OBJNUM(laShowFig[9,1])
  ENDIF
ENDIF
=lfRefresh()
*-- end of lfvToBuy.

*!*************************************************************
*! Name      : lfvDefVen
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/22/2003
*! Purpose   : Validation of the vendor at the PO defaults screen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefVen()
*!*************************************************************
FUNCTION lfvDefVen

PRIVATE lnAlias
lnAlias = SELECT(0)

*-- If the enterd vendor does not ecxit in the apvendor file
IF llBrowse .OR. (!EMPTY(lcVendor) .AND. IIF(SEEK(lcVendor,'APVENDOR'),;
    !('S' $ APVENDOR.cVenSupTyp .OR. 'C' $ APVENDOR.cVenSupTyp),.T.))
  =gfApVnBrow(@lcVendor,.F.,'CS')
ENDIF
llBrowse = .F.

IF !EMPTY(lcVendor)
  lcVenName = ApVendor.cVenComp
  IF SEEK('N'+'CTERMCODE '+APVENDOR.CTERMCODE,'CODES')
    = gfwCodePop(@laCodInfo, "CTERMCODE", "L")
    lnTerm = ASUBSCRIPT(laTerm,ASCAN(laTerm,APVENDOR.CTERMCODE),1)
    SHOW GET lnTerm
  ENDIF
  *-- if the system id set to use multi currency
  IF llMulCurr
    lcPCurr = ApVendor.ccurrcode
    lcDcurr = gcBaseCurr
    lnPRate = gfchkrate('lnUnit1',lcPCurr ,gdSysDate,llEditExRt,gcAct_Comp,.F.)
    lnDRate = gfchkrate('lnUnit2',lcDcurr ,gdSysDate,llEditExRt,gcAct_Comp,.F.)
    
    *-- if there is no valid rate for the entered currency,give a message
    *-- (No valid rate .default to the base currency)
    IF lnPRate <= 0
      =gfModalGen('INM36004B36001','DIALOG','price') = 1
      lcPCurr = gcBaseCurr
      lnPRate = 1
    ENDIF
    IF lnDRate <= 0
      =gfModalGen('INM36004B36001','DIALOG','duty') = 1
      lcDCurr = gcBaseCurr
      lnDRate = 1
    ENDIF
  ENDIF
ENDIF
*-- Check if the user has the right to edit the rate
IF lcPCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnPRate DISABLE
ELSE
  SHOW GET lnPRate ENABLE
ENDIF
IF lcDCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnDRate DISABLE
ELSE
  SHOW GET lnDRate ENABLE
ENDIF
SHOW GET lcPCurr
SHOW GET lcDCurr
SHOW GETS
SELECT (lnAlias)
*-- end of lfvDefVen.

*!*************************************************************
*! Name      : lfvDefWare
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/22/2003
*! Purpose   : Validation of the warehouse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefWare()
*!*************************************************************
FUNCTION lfvDefWare

IF llBrowse OR !SEEK(lcWareCode,'WAREHOUS')
  lcWareCode = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
ENDIF
llBrowse = .F.
*-- end of lfvDefWare.

*!*************************************************************
*! Name      : lfvDefPCurr
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/22/2003
*! Purpose   : Validation of the default price currency
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefPCurr()
*!*************************************************************
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
*-- end of lfvDefPCurr.

*!*************************************************************
*! Name      : lfvDefDCurr
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/22/2003
*! Purpose   : Validation of the default duty currency
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefDCurr()
*!*************************************************************
FUNCTION lfvDefDCurr

IF llBrowse OR !SEEK(lcDCurr,"SycCurr")
  IF !gfcurrbrow(@lcDCurr)
    lcDCurr = gcBaseCurr
  ENDIF
ENDIF
llBrowse = .F.
lnDRate = gfchkrate('lnUnit2',lcDCurr ,gdSysDate,llEditExRt,gcAct_Comp,.F.)
lcSign2 = gfGetExSin("",ALLTRIM(lcDCurr))
IF lnPRate <= 0
  IF gfModalGen('INM36004B36001','DIALOG','duty') = 1
    lcDCurr = gcBaseCurr
    lnDRate = 1
    lnUnit2 = 1
  ELSE
    _CUROBJ = OBJNUM(lnDRate)
  ENDIF
ENDIF
IF lcDCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnDRate DISABLE
ELSE
  SHOW GET lnDRate ENABLE
ENDIF
*-- end of lfvDefDCurr.

*!*************************************************************
*! Name      : lfvOK
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/22/2003
*! Purpose   : Validation of <OK> of the default PO screen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOK()
*!*************************************************************
FUNCTION lfvOK

*-- if the vendor is left empty, give the user a message
*-- (vendor field cannot be left empty)
IF EMPTY(lcVendor)
  =gfModalGen('INM36002B36000','DIALOG','Vendor')
  _CUROBJ = OBJNUM(lcVendor)
  RETURN
ENDIF
=SEEK(lcVendor,'APVENDOR')
lcPhone = ApVendor.cphoneno
lcCont  = ApVendor.cVenCont

*-- If the location is left empty give the user a message
*-- (You have to enter a warehouse code)
IF llWareHous AND EMPTY(lcWareCode)
  =gfModalGen('INM38138B38018','DIALOG')
  RETURN
ENDIF

llOkDefalt = .T.
llOk = .T.
CLEAR READ
*-- end of lfvOk.

*!*************************************************************
*! Name      : lfShowDef
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/22/2003
*! Purpose   : Show function for the objects of the PO default screen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfShowDef()
*!*************************************************************
FUNCTION lfShowDef

IF lcPCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnPRate DISABLE
ELSE
  SHOW GET lnPRate ENABLE
ENDIF
IF lcDCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnDRate DISABLE
ELSE
  SHOW GET lnDRate ENABLE
ENDIF
*-- end of lfShowDef.

*!*************************************************************
*! Name      : lfGetStyle
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/22/2003
*! Purpose   : Function to get the correct style record.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGetStyle()
*!*************************************************************
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
*-- end of lfGetStyle.

*:*************************************************************
*: Name      : lpDelScr
*: Developer : AHMED MAHER
*: Date      : 07/14/2003
*: Purpose   : Delete current style/color.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
PROCEDURE lpDelScr

PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT POTOBUY
SET ORDER TO TAG POTOBUYL
IF SEEK(PADR(laData[1],19)+laData[2])
  SCAN REST WHILE STYLE+COLOR = PADR(laData[1],19)+laData[2]
    BLANK
    DELETE
  ENDSCAN
ENDIF
SET ORDER TO TAG POTOBUY
SELECT (lnAlias)
STORE .F. TO laScrMode
STORE .T. TO laScrMode[1]
*-- End Of lpDelScr.

*!*************************************************************
*! Name      : lfAdYrInPo
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/30/2003
*! Purpose   : Function to Update the YEARINPO file.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfAdYrInPo()
*!*************************************************************
FUNCTION lfAdYrInPo

=SEEK('S'+SCALE,'SCALE')
lcMonthNo = STRTRAN(STR(MONTH(EVALUATE(lcTmpPoHdr+'.COMPLETE')),2,0),' ','0')
SELECT YEARINPO
FOR lnI = 1 TO SCALE.CNT
  lcI = STR(lnI,1)
  IF !SEEK(STR(YEAR(EVALUATE(lcTmpPoHdr+'.COMPLETE')),4,0)+PADR(lcStyle,19)+lcColor+STYLE.SCALE+lcI)
    APPEND BLANK
    REPLACE NYEAR     WITH YEAR(EVALUATE(lcTmpPoHdr+'.COMPLETE')),;
            STYLE     WITH PADR(lcStyle,19),;
            COLOR     WITH lcColor,;
            CSIZECODE WITH STYLE.SCALE+lcI
  ENDIF
  REPLACE ('NPOSMNTH'+lcMonthNo) WITH EVALUATE('NPOSMNTH'+lcMonthNo) + EVALUATE('m.Qty'+lcI)
  REPLACE NTOTPOS WITH NTOTPOS + EVALUATE('m.Qty'+lcI)
ENDFOR
*-- end of lfAdYrInPo.

*!*************************************************************
*! Name      : gfCPBrows
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/03/2003
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
*:->
FUNCTION gfCPBrows

SELECT (lcBaseFile)
=gfBrows()
laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
laScrMode   = .F.
laScrMode[2]= .T.
SHOW GETS
*-- end of gfCpBrows.

*:*************************************************************
*: Name      : lpTopScr
*: Developer : AHMED MAHER (AMH)
*: Date      : 08/03/2003
*: Purpose   : Top record in Navigation pb.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
PROCEDURE lpTopScr

laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
*-- End OF lpTopScr.

*:*************************************************************
*: Name      : lpBtmScr
*: Developer : AHMED MAHER (AMH)
*: Date      : 08/03/2003
*: Purpose   : Bottom record in Navigation pb.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
PROCEDURE lpBtmScr

laData[1] = PADR(STYLE.STYLE,lnMjrWid)
laData[2] = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen)
IF SEEK(PADR(laData[1],lnMjrWid),'STYLE')
  SELECT STYLE
  LOCATE REST WHILE STYLE = PADR(laData[1],lnMjrWid);
                FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2]
ENDIF
*-- End OF lpBtmScr.

*:*************************************************************
*: Name      : lpNxtScr
*: Developer : AHMED MAHER (AMH)
*: Date      : 08/03/2003
*: Purpose   : Next record in Navigation pb.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
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

*:*************************************************************
*: Name      : lpPrvScr
*: Developer : AHMED MAHER (AMH)
*: Date      : 08/03/2003
*: Purpose   : Previus record in Navigation pb.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
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
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/03/2003
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
*-- end of lfChkLstRc.

*!*************************************************************
*! Name      : lfActPad
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/03/2003
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
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Generate PO'   SKIP FOR laScrMode[3] .OR. !llCanGenPo
DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Style Inquiry' SKIP FOR !laScrMode[2]
ON SELECTION BAR 1 OF _OPTIONPOP DO lfSelStyle
ON SELECTION BAR 2 OF _OPTIONPOP DO lfStyInq
*-- end of lfActPad.

*!*************************************************************
*! Name      : lfSelStyle
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/03/2003
*! Purpose   : Display the select style screen to generate PO
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfSelStyle()
*!*************************************************************
FUNCTION lfSelStyle

PRIVATE lnAlias,lcPUntSin,lcDUntSin,lcPExSign,lcDExSign,lnRecNo
STORE '/' TO lcPUntSin,lcDUntSin
lcPExSign = gfGetExSin(@lcPUntSin,lcPCurr)
lcDExSign = gfGetExSin(@lcDUntSin,lcDCurr)

=gfOpenFile(gcSysHome+'SYCCURR', gcSysHome+'CCURRCODE','SH')
=gfOpenFile(gcDataDir+'APVENDOR', gcDataDir+'Vencode','SH')
=gfOpenFile(gcDataDir+'WAREHOUS', gcDataDir+'WAREHOUS','SH')

=gfwCodePop(@laCodInfo, "CTERMCODE", "D")
=gfwCodePop(@laCodInfo, "SHIPVIA",   "D")

STORE ''  TO lcPhone,lcCont,lcVenName

=gfwCodePop(@laCodInfo, "CTERMCODE", "L")
=gfwCodePop(@laCodInfo, "SHIPVIA",   "L")

lnDMarker  = 1
lcStyleTit = 'Styles'
lcOk       = gcBmpHome + "OK.BMP"
lcCancel   = gcBmpHome + "CAN.BMP"
lcOkStatus = 'DISABLE'
llOkDefalt = .F.
=lfvSelect('N')
SELECT (lcTmpStyle)
LOCATE

*-- Call the Styles screen
DO (gcScrDir+gcWinAppl+"\POSELSTY.SPX")

=gfCloseFile('SYCCURR')
=gfCloseFile('APVENDOR')
=gfCloseFile('WAREHOUS')
SHOW GETS
*-- end of lfSelStyle.

*!*************************************************************
*! Name      : lfGetSelSt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/03/2003
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
*! Name      : lfBrowSty
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2003
*! Purpose   : Browse Styles
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowSty()
*!*************************************************************
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
*-- end of lfBrowSty.

*!*************************************************************
*! Name      : lfwStyBrs
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2003
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
FUNCTION lfwStyBrs

lndMarker = RECNO(lcTmpStyle)
SHOW WINDOW (lcStyleTit) REFRESH SAME
=lfvSelect('')
*-- end of lfwStyBrs.

*!*************************************************************
*! Name      : lfReadAct
*! Developer : AHMED MAHER
*! Date      : 08/04/2003
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
FUNCTION lfReadAct

IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcStyleTit)
*-- end of lfReadAct.

*!*************************************************************
*! Name      : lfClearKey
*! Developer : AHMED MAHER
*! Date      : 08/04/2003
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
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER
*-- end of lfClearKey.

*!*************************************************************
*! Name      : lfDMainLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2003
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
FUNCTION lfDMainLot

IF WONTOP()= lcStyleTit
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTabKey  WITH 'POSELST2','pbSelect'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'POSELST2','pbCancel'
ENDIF
RETURN .F.
*-- end of lfDMainLot

*!*************************************************************
*! Name      : lpTabKey
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2003
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
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2003
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
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2003
*! Purpose   : Called from the selection buttons
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSelect()
*!*************************************************************
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
*--end of lfvSelect.

*!*************************************************************
*! Name      : lfPODef
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2003
*! Purpose   : Display the default PO screen.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfPODef()
*!*************************************************************
FUNCTION lfPODef

DECLARE laSaveVar[11,2]
laSaveVar[01,1] = 'lcVendor'
laSaveVar[02,1] = 'lcWareCode'
laSaveVar[03,1] = 'lcPCurr'
laSaveVar[04,1] = 'lcDCurr'
laSaveVar[05,1] = 'lcVenName'
laSaveVar[06,1] = 'lnPRate'
laSaveVar[07,1] = 'lnDRate'
laSaveVar[08,1] = 'lnTerm'
laSaveVar[09,1] = 'lnShip'
laSaveVar[10,1] = 'lnType'
laSaveVar[11,1] = 'puType'

=lfVariabls()
llOk = .F.

*-- Call the P/O defaults screen
DO (gcScrDir+gcWinAppl+"\POGENPO.SPX")

IF !llOk
  =lfVariabls(.T.)
ENDIF
=lfvSelect('')
*-- end of lfPODef.

*!**************************************************************************
*! Name      : lfvGenPo
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/22/2003
*! Purpose   : Valid function of the generate PO button
*!**************************************************************************
*! Example   : =lfvGenPo()
*!**************************************************************************
*
FUNCTION lfvGenPo

lnAlias = SELECT(0)
lnRecNo = RECNO('STYLE')
=gfOpenFile(gcDataDir+'STYDYE', gcDataDir+'STYDYE','SH')

IF EMPTY(lcWareCode)
  GO TOP IN 'WAREHOUS'
  lcWareCode = WAREHOUS.cWareCode
ENDIF

STORE SPACE(06) TO lcFirstCT,lcLastCt
SELECT (lcTmpStyle)
SET ORDER TO TAG (lcTmpStyl1)
SET ORDER TO TAG POTOBUYL IN POTOBUY
IF SEEK("�")
  STORE SPACE(25) TO lcPoKey
  STORE .F.       TO llNewPo
  STORE SPACE(00) TO lcDivision,lcPurCode,lcStyGrade
  STORE {}        TO ldComplete
  STORE 0         TO lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5,;
                     lnTCost1, lnTCost2, lnTCost3, lnTCost4, lnTCost5
  SCAN REST WHILE CSELECT = "�"
    lcStyle   = STYLE
    lcColor   = COLOR
    lnPoYear  = NYEAR
    lnPoMonth = NMONTH
    
    llNewPo   = !(STR(lnPoYear,4,0)+STR(lnPoMonth,2,0)+lcStyle==lcPoKey)
    IF llNewPo .AND. !EMPTY(lcPoKey)
      =lfUpdPoFls()
    ENDIF
    
    lcPoKey   = STR(lnPoYear,4,0)+STR(lnPoMonth,2,0)+lcStyle
    IF SEEK(lcStyle+lcColor,'POTOBUY')
      =lfGenPoSty()
    ENDIF
  ENDSCAN
  
  IF !EMPTY(lcPoKey)
    =lfUpdPoFls()
  ENDIF
ENDIF
SELECT (lcTmpStyle)
SET ORDER TO TAG (lcTmpStyle)
=gfCloseFile('STYDYE')

SET ORDER TO TAG POTOBUY IN POTOBUY
=SEEK(PADR(laData[1],19)+laData[2])

*-- Give the numbers of the generated P/Os.
=gfModalGen('INM38134B38018','DIALOG','P/O'+'|'+ALLTRIM(lcFirstCT)+"|"+ALLTRIM(lcLastCt))

GO lnRecNo IN STYLE
SELECT (lnAlias)
CLEAR READ
=lfGetSelSt()
*-- end of lfvGenPo.

*!**************************************************************************
*! Name      : lfGenPoSty
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/22/2003
*! Purpose   : Function to the generate PO for style
*!**************************************************************************
*! Example   : =lfGenPoSty()
*!**************************************************************************
*
FUNCTION lfGenPoSty

IF llNewPo
  SELECT (lcTmpPoHdr)
  ZAP
  SELECT (lcTmpPosLn)
  ZAP
ENDIF

lcDateSet = SET('DATE')
SET DATE TO MDY

SELECT POTOBUY
SCAN REST WHILE STYLE+COLOR = lcStyle+lcColor;
            FOR NMONTH = lnPoMonth .AND. NYEAR = lnPoYear .AND. NTOBUYQTY > 0
  lcMonthPo = STR(NMONTH,2,0)+STR(NYEAR,4,0)
  IF NTOBUYQTY > 0
    IF llNewPo
      =SEEK(PADR(lcStyle,lnMjrWid),'STYLE')
      lcDivision = STYLE.CDIVISION
      lcPurCode  = STYLE.CPURCODE
      lcStyGrade = STYLE.CSTYGRADE
      ldComplete = lfLstMnDay(NMONTH,NYEAR)
      SELECT (lcTmpPoHdr)
      APPEND BLANK
      REPLACE cStyType   WITH 'P' ,;
              PO         WITH lcMonthPo,;
              Vendor     WITH lcVendor ,;
              STATUS     WITH 'H',;
              CDIVISION  WITH lcDivision,;
              cPurCode   WITH lcPurCode ,;
              ENTERED    WITH gdSysDate ,;
              COMPLETE   WITH ldComplete,;
              SHIPVIA    WITH laShip[lnShip,2]   ,;
              CTERMCODE  WITH laTerm[lnTerm,2]   ,;
              cWareCode  WITH lcWareCode ,;
              cPriceCur  WITH lcPCurr ,;
              nPriceRat  WITH lnPRate ,;
              nCurrUnit  WITH lnUnit1 ,;
              cDutyCur   WITH lcDCurr ,;
              nDutyRat   WITH lnDRate ,;
              nDCurUnit  WITH lnUnit2 ,;
              lMultiWare WITH (lnType=2)
      llNewPo = .F.
      STORE 0 TO lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5,;
                  lnTCost1, lnTCost2, lnTCost3, lnTCost4, lnTCost5
    ENDIF
    
    =lfGetStyle()
    SELECT (lcTmpPosLn)
    IF !SEEK('P'+EVALUATE(lcTmpPoHdr+'.PO')+STYLE.STYLE)
      SELECT (lcTmpPoHdr)
      REPLACE LASTLINE WITH LASTLINE + 1
      SELECT (lcTmpPosLn)
      APPEND BLANK
      REPLACE CSTYTYPE  WITH 'P'                             ,;
              PO        WITH EVALUATE(lcTmpPoHdr+'.PO')      ,;
              STYLE     WITH STYLE.STYLE                     ,;
              TRANCD    WITH '1'                             ,;
              cWareCode WITH lcWareCode                      ,;
              LINENO    WITH EVALUATE(lcTmpPoHdr+'.LASTLINE'),;
              Vendor    WITH lcVendor                        ,;
              cStyGrade WITH lcStyGrade
    ENDIF
    lcSize = RIGHT(POTOBUY.CSIZECODE,1)
    REPLACE ('QTY'+lcSize) WITH POTOBUY.NTOBUYQTY ,;
            TOTQTY         WITH TOTQTY + EVALUATE('QTY'+lcSize)
    FOR lnCount = 1 TO 5
      lcCount = STR(lnCount,1)
      DO CASE
        CASE lcIType&lcCount = 'P'
          lnCost&lcCount = IIF(lcPCurr=Style.cPriceCur,;
                               EVALUATE('QTY'+lcSize)*STYLE.nICost&lcCount,0)
          lnECost&lcCount= lnCost&lcCount &lcPExSign lnPRate &lcPUntSin lnUnit1
        CASE INLIST(lcIType&lcCount,'M','D')
          lnCost&lcCount = IIF(lcDCurr=Style.cDutyCur,;
                               EVALUATE('QTY'+lcSize)*STYLE.nICost&lcCount,0)
          lnECost&lcCount= lnCost&lcCount &lcDExSign lnDRate &lcDUntSin lnUnit2
        OTHERWISE
          lnCost&lcCount = EVALUATE('QTY'+lcSize)*STYLE.nICost&lcCount
          lnECost&lcCount= lnCost&lcCount
      ENDCASE
      lnTCost&lcCount  = lnTCost&lcCount  + lnECost&lcCount
      lnTFCost&lcCount = lnTFCost&lcCount + lnCost&lcCount
    ENDFOR
    REPLACE nCost1  WITH nCost1 + lnCost1  ,;
            nCost2  WITH nCost2 + lnCost2  ,;
            nCost3  WITH nCost3 + lnCost3  ,;
            nCost4  WITH nCost4 + lnCost4  ,;
            nCost5  WITH nCost5 + lnCost5  ,;
            nECost1 WITH nECost1+ lnECost1 ,;
            nECost2 WITH nECost2+ lnECost2 ,;
            nECost3 WITH nECost3+ lnECost3 ,;
            nECost4 WITH nECost4+ lnECost4 ,;
            nECost5 WITH nECost5+ lnECost5
    SELECT (lcTmpPoHdr)
    REPLACE NSTYORDER WITH NSTYORDER+EVALUATE(lcTmpPosLn+'.QTY'+lcSize),;
            NFCOST1   WITH lnTFCost1,;
            NFCOST2   WITH lnTFCost2,;
            NFCOST3   WITH lnTFCost3,;
            NFCOST4   WITH lnTFCost4,;
            NFCOST5   WITH lnTFCost5,;
            NICOST1   WITH lnTCost1 ,;
            NICOST2   WITH lnTCost2 ,;
            NICOST3   WITH lnTCost3 ,;
            NICOST4   WITH lnTCost4 ,;
            NICOST5   WITH lnTCost5
  ENDIF
ENDSCAN
SET DATE TO &lcDateSet.

SELECT POTOBUY
IF SEEK(lcStyle+lcColor)
  REPLACE REST WHILE STYLE+COLOR = PADR(lcStyle,19)+lcColor;
                 FOR NYEAR = lnPoYear .AND. NMONTH = lnPoMonth NTOBUYQTY WITH 0
ENDIF
*-- end of lfGenPoSty.

*!**************************************************************************
*! Name      : lfUpdPoFls
*! Developer : Ahmed Maher (AMH)
*! Date      : 08/10/2003
*! Purpose   : Function to Update the POSHDR and POSLN with the generated PO
*!**************************************************************************
*! Example   : =lfUpdPoFls()
*!**************************************************************************
*
FUNCTION lfUpdPoFls

PRIVATE lcPONumber, lcPO
SELECT (lcTmpPoHdr)
SCAN
  lcPO       = PO
  lcPONumber = gfSequence('PO','','',CDIVISION)
  lcFirstCT  = IIF(EMPTY(lcFirstCT),lcPONumber,lcFirstCT)
  lcLastCt   = lcPONumber
  SELECT (lcTmpPosLn)
  =SEEK('P'+lcPO)
  SCAN REST WHILE CSTYTYPE+PO = 'P'+lcPO
    IF !SEEK('P'+lcPONumber+STYLE+STR(LINENO,6)+TRANCD,'POSLN')
      SCATTER MEMVAR
      m.PO = lcPONumber
      =SEEK(m.STYLE,'STYLE')
      m.Scale    = STYLE.SCALE
      m.nCost1   = m.nCost1 / m.TotQty
      m.nCost2   = m.nCost2 / m.TotQty
      m.nCost3   = m.nCost3 / m.TotQty
      m.nCost4   = m.nCost4 / m.TotQty
      m.nCost5   = m.nCost5 / m.TotQty
      m.nECost1  = m.nECost1 / m.TotQty
      m.nECost2  = m.nECost2 / m.TotQty
      m.nECost3  = m.nECost3 / m.TotQty
      m.nECost4  = m.nECost4 / m.TotQty
      m.nECost5  = m.nECost5 / m.TotQty
      m.cStyType = 'P'
      IF !SEEK(m.STYLE+m.CWARECODE+SPACE(10),'STYDYE')
        WAIT "Assigning Style"+ALLTRIM(m.STYLE)+" to warehouse "+ALLTRIM(m.cWareCode) WINDOW NOWAIT
        DO gpAdStyWar WITH m.STYLE,SPACE(10),m.cWareCode
        =SEEK(m.STYLE+m.CWARECODE+SPACE(10),'STYDYE')
        WAIT CLEAR
      ENDIF
      SELECT STYDYE
      =RLOCK()
      REPLACE WIP1   WITH WIP1   + m.Qty1  ,;
              WIP2   WITH WIP2   + m.Qty2  ,;
              WIP3   WITH WIP3   + m.Qty3  ,;
              WIP4   WITH WIP4   + m.Qty4  ,;
              WIP5   WITH WIP5   + m.Qty5  ,;
              WIP6   WITH WIP6   + m.Qty6  ,;
              WIP7   WITH WIP7   + m.Qty7  ,;
              WIP8   WITH WIP8   + m.Qty8  ,;
              TotWIP WITH TotWIP + m.TotQty,;
              nWO1   WITH nWO1   + m.Qty1  ,;
              nWO2   WITH nWO2   + m.Qty2  ,;
              nWO3   WITH nWO3   + m.Qty3  ,;
              nWO4   WITH nWO4   + m.Qty4  ,;
              nWO5   WITH nWO5   + m.Qty5  ,;
              nWO6   WITH nWO6   + m.Qty6  ,;
              nWO7   WITH nWO7   + m.Qty7  ,;
              nWO8   WITH nWO8   + m.Qty8  ,;
              nTotWo WITH nTotWo + m.TotQty
      UNLOCK
      =gfTraceKey('STYDYE',STYLE+CWARECODE+DYELOT,'M')
      SELECT STYLE
      =RLOCK()
      REPLACE WIP1   WITH WIP1   + m.Qty1  ,;
              WIP2   WITH WIP2   + m.Qty2  ,;
              WIP3   WITH WIP3   + m.Qty3  ,;
              WIP4   WITH WIP4   + m.Qty4  ,;
              WIP5   WITH WIP5   + m.Qty5  ,;
              WIP6   WITH WIP6   + m.Qty6  ,;
              WIP7   WITH WIP7   + m.Qty7  ,;
              WIP8   WITH WIP8   + m.Qty8  ,;
              TotWIP WITH TotWIP + m.TotQty,;
              nWO1   WITH nWO1   + m.Qty1  ,;
              nWO2   WITH nWO2   + m.Qty2  ,;
              nWO3   WITH nWO3   + m.Qty3  ,;
              nWO4   WITH nWO4   + m.Qty4  ,;
              nWO5   WITH nWO5   + m.Qty5  ,;
              nWO6   WITH nWO6   + m.Qty6  ,;
              nWO7   WITH nWO7   + m.Qty7  ,;
              nWO8   WITH nWO8   + m.Qty8  ,;
              nTotWo WITH nTotWo + m.TotQty
      UNLOCK
      =gfTraceKey('STYLE',STYLE,'M')
      m.cAdd_User = gcUser_id
      m.cAdd_Time = TIME()
      m.dAdd_Date = gdSysDate
      INSERT INTO POSLN FROM MEMVAR
      =lfAdYrInPo()
    ENDIF
  ENDSCAN
  IF !SEEK('P'+lcPONumber,'POSHDR')
    SELECT (lcTmpPoHdr)
    SCATTER MEMVAR
    m.PO = lcPONumber
    m.cMultiLot= 'S'
    =SEEK(m.cWarecode,'WAREHOUS')
    m.cOutAddr1 = WareHous.cAddress1
    m.cOutAddr2 = WareHous.cAddress2
    m.cOutAddr3 = WareHous.cAddress3
    m.cOutAddr4 = WareHous.cAddress4
    m.cOutAddr5 = WareHous.cAddress5
    m.ShpName   = WareHous.cDesc
    m.Available = COMPLETE
    m.Open      = nStyOrder
    m.cStyType  = "P"
    m.Contact   = lcCont
    m.Phone     = lcPhone
    m.POTOTAL   = NICOST1+NICOST2+NICOST3+NICOST4+NICOST5
    m.cAdd_User = gcUser_id
    m.cAdd_Time = TIME()
    m.dAdd_Date = gdSysDate
    INSERT INTO POSHDR FROM MEMVAR
    =gfTraceKey('POSHDR','P'+lcPONumber,'A')
    =gfTraceKey('POSLN','P'+lcPONumber+POSLN.STYLE+STR(POSLN.LINENO,6)+POSLN.Trancd,'A')
  ENDIF
ENDSCAN
*-- end of lfUpdPoFls.

*!**************************************************************************
*! Name      : lfVariabls
*! Developer : Ahmed Maher (AMH)
*! Date      : 08/04/2003
*! Purpose   : Function to save/restore variabls
*!**************************************************************************
*! Example   : =lfVariabls()
*!**************************************************************************
*
FUNCTION lfVariabls
PARAMETERS llRestore

FOR lnI = 1 TO 11
  IF llRestore
    &laSaveVar[lnI,1]. = laSaveVar[lnI,2]
  ELSE
    laSaveVar[lnI,2] = EVALUATE(laSaveVar[lnI,1])
  ENDIF
ENDFOR
*-- end of lfVariabls.

*!**************************************************************************
*! Name      : lfStyInq
*! Developer : Ahmed Maher (AMH)
*! Date      : 08/05/2003
*! Purpose   : Function to call the style screen.
*!**************************************************************************
*! Example   : =lfStyInq()
*!**************************************************************************
*
FUNCTION lfStyInq

lcStyle = STYLE.STYLE
lcParameter = "'"+lcStyle+"',''"
DO gpDoProg WITH 'AWRICSTYLE',.F.,'IC',lcParameter
*-- end of lfStyInq.

*!**************************************************************************
*! Name      : lfPrv1YSal
*! Developer : NADER NABIL (NNA)
*! Date      : 06/23/2004
*! Purpose   : Function to get the Sales Value for The Previous Year
*!**************************************************************************
*! Example   : = lfPrv1YSal()
*!**************************************************************************
*!C122961,1
FUNCTION lfPrv1YSal

PARAMETERS lnRow,lnCol,lnForYear
PRIVATE lnAlias
lnAlias  = SELECT(0)

IF SEEK(STR(lnForYear-1,4,0)+PADR(laData[1],19)+laData[2]+laAllData[1,lnCol],'YEARINPO')
  laAllData[lnRow,lnCol] = YEARINPO.nTotShip
ELSE
  laAllData[lnRow,lnCol] = 0
ENDIF
SELECT (lnAlias)

*-- End of lfPrv1YSal.

*!**************************************************************************
*! Name      : lfLnkToPo
*! Developer : NADER NABIL (NNA)
*! Date      : 06/23/2004
*! Purpose   : Function to get the Open Qty. on the Sales orders to add it 
*!           : to the Opening Stock.
*!**************************************************************************
*! Example   : = lfLnkToPo()
*!**************************************************************************
*!C122961,1
FUNCTION lfLnkToPo
PARAMETERS lnRow , lnAcuOrdQt
PRIVATE lnAlias , lnRecNo

STORE 0 TO lnLnkOrdQt , lnAcuOrdQt
lnAlias  = SELECT(0)
lnRecNo  = RECNO('ORDLINE')
SELECT ORDLINE
SET RELATION TO CORDTYPE+ORDER INTO ORDHDR
IF llUpdPatrn
  SELECT STYLE  
  lcPattern  = Pattern
  lcStyle    = STYLE    
  lcScale    = Scale    
  lnStyRec   = RECNO()
  lcStyOrd   = ORDER()
  SET ORDER TO Stylepat 
  =SEEK(lcPattern)
  SCAN WHILE PATTERN = lcPattern FOR (Style # lcStyle .AND. SCALE = lcScale .AND. ;
                       SUBSTR(STYLE,lnClrPos,lnClrLen) = laData[2])
***** added by Tony 21/07/2006 ************
   lnLnkOrdQt = lnLnkOrdQt + style.stk&LCI
*******************************************
*** removed by Tony 21/07/2006 ************
*      IF SEEK(STYLE,'OrdLine')
*         SELECT ORDLINE
*         SCAN FOR ORDHDR.STATUS$'OHB' WHILE STYLE=STYLE.STYLE
*            IF (LAALLDATA(LNROW,2)=LNMONTH .AND. (YEAR(ORDHDR.START)<YEAR(GDSYSDATE) .OR. (YEAR(ORDHDR.START)=YEAR(GDSYSDATE) .AND. MONTH(ORDHDR.START)<=LNMONTH))) .OR. (YEAR(ORDHDR.START)=LNYEAR .AND. MONTH(ORDHDR.START)=LAALLDATA(LNROW,2))
*               IF SEEK('2'+ORDLINE.ORDER,'CUTPICK')
*                  lnLnkOrdQt = lnLnkOrdQt + ORDLINE.QTY&LCI
*               ENDIF
*            ENDIF
*         ENDSCAN
*      ENDIF
********************************************
      ENDSCAN
    ENDIF
  ENDSCAN
  *B132554,1  TMI [Start] no need for this line
  *lnLnkOrdQt = lnAcuOrdQt
  *B132554,1  TMI [End  ] 
ENDIF
SELECT ORDLINE
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo IN ('ORDLINE')
ENDIF
SET RELATION TO
SELECT STYLE
IF llUpdPatrn
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GOTO lnStyRec IN STYLE
  ENDIF
  SET ORDER TO lcStyOrd 
ENDIF  
SELECT (lnAlias)
*-- End of lfLnkToPo.

*!**************************************************************************
*! Name      : lfUpdate
*! Developer : NADER NABIL (NNA)
*! Date      : 06/23/2004
*! Purpose   : Function to Update the Array(laAllData) with any Changes in the To Buy Fileds
*!**************************************************************************
*! Example   : = lfUpdate()
*!**************************************************************************
*!C122961,1
FUNCTION lfUpdate

*-- Update  Opening stock and Closing Stock lines for the next Months.
FOR I = lnYPos + 9 TO lnRowCnt STEP 9    
  laAllData[I,lnScaleCnt]       = 0
  laAllData[I + 8 , lnScaleCnt] = 0
 
  *--Opening stock for next Month = Closing Stock for the Processed Month
  IF laAllData[I,2] = 13
    I = I + 9                     && Move to next Month
    IF I <= lnRowCnt
      laAllData[I,lnXpos+lnCol-1] = IIF(laAllData[(I - 9),2] = 13           , ;
                                        laAllData[(I - 10) ,lnXpos+lnCol-1] , ;
                                        laAllData[(I - 9) ,lnXpos+lnCol-1])

      FOR N = 3 TO lnScaleCnt - 1
        laAllData[I,lnScaleCnt] = laAllData[I,lnScaleCnt] + laAllData[I,N]
      ENDFOR
      laAllData[I + 8 , lnXpos+lnCol-1] = (laAllData[I,lnXpos+lnCol-1]      + ;
                                           laAllData[I + 6,lnXpos+lnCol-1]  + ;
                                           laAllData[I + 7,lnXpos+lnCol-1]) - ;  
                                           laAllData[I + 5,lnXpos+lnCol-1]

      FOR N = 3 TO lnScaleCnt - 1
        laAllData[I + 8 , lnScaleCnt] = laAllData[I + 8 , lnScaleCnt] + laAllData[I + 8 ,N]
      ENDFOR
    ENDIF
  ELSE
    laAllData[I,lnXpos+lnCol-1] = IIF(laAllData[(I - 1),2] = 13          , ;
                                      laAllData[(I - 2) ,lnXpos+lnCol-1] , ;
                                      laAllData[(I - 1) ,lnXpos+lnCol-1])

    FOR N = 3 TO lnScaleCnt - 1
      laAllData[I,lnScaleCnt] = laAllData[I,lnScaleCnt] + laAllData[I,N]
    ENDFOR
   laAllData[I + 8 , lnXpos+lnCol-1] = (laAllData[I,lnXpos+lnCol-1]      + ;
                                        laAllData[I + 6,lnXpos+lnCol-1]  + ;
                                        laAllData[I + 7,lnXpos+lnCol-1]) - ; 
                                        laAllData[I + 5,lnXpos+lnCol-1]
    FOR N = 3 TO lnScaleCnt - 1
      laAllData[I + 8 , lnScaleCnt] = laAllData[I + 8 , lnScaleCnt] + laAllData[I + 8 ,N]
    ENDFOR

  ENDIF
ENDFOR
*-- End of lfUpdate.