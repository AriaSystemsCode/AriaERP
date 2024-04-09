*:************************************************************************
*: Prg file : ICAND600.PRG (COPIED FROM AND600 FOR ANDREW)
*:  desc.   : A customized stock on hand report for Andrew Mark
*:                 (A copy from standard one with some changes)
*: Module   : Aria Apparel Series.
*: Date     : 02/01/99
*: Developer: Adel Mohammed El Gazzar (ADEL).
*  Refer To : (C101393)
*:************************************************************************
*: Calls : 
*:         Functions  : lfVldWare()
*:                    : gfTempName()
*:                    : lfWHSubTot() 
*:				      : lfPrnDyelt()		
*:                    : lfSubTotal()
*:                    : lfPrnLoc()
*:                    : lfPrnHdr()
*:                    : lfVldType()
*:                    : lfwRepWhen()
*:         Procedures : lpReport
*:************************************************************************

*--Add the make to the report filter.
IF !EMPTY(lcRpMakBuy) AND lcRpMakBuy <> 'B'
  lcRpExp = lcRpExp + ' AND STYLE.MAKE = IIF(lcRpMakBuy = "D",.T.,.F.)'
ENDIF
llMultiWH  = (gfGetMemVar('M_WareHouse') = 'Y')
IF !llMultiWH
  lcRpExp = lcRpExp + '.AND. (STYLE.TOTSTK<>0) '
ENDIF
llAvgCost  =  (gfGetMemVar('M_COST_METH') = 'A')
llWareLoc  =  (gfGetMemVar('M_WareLoc') = 'Y')
llImpCost  =  (gfGetMemVar('M_LImpCost'))
lcPdSETup  = ''
lcRpWareDt =IIF(lcRpSortBy = 'W','Y',lcRpWareDt)
llPrnWHDitl= .F.
*--Get the first style.
lcMajTitle = UPPER(lcMajTitle)
lnMajorLen = LEN(lcMajPic)
lcFrstSty  = ''
lcSndSty   = ''
lnStyPos   = ASCAN('laOGVrFlt','SUBSTR(STYLE.&lcMajTitle,1,lnMajorLen)')
*-- Get the row.
lnStyPos  = ASUBSCRIPT('laOGVrFlt',lnStyPos,1)
IF lnStyPos <> 0 AND !EMPTY(laOgVrFlt[lnStyPos,6])
  lnSndSPos = AT('|',laOGVrFlt[1,6])
  lcFrstSty = IIF(lnSndSPos <>1,SUBSTR(laOGVrFlt[lnStyPos,6],1,lnMajorLen),'')
  lcSndSty  = SUBSTR(laOGVrFlt[lnStyPos,6],lnSndSPos+1,lnMajorLen)
ENDIF
*-- Clear the relation if we change the sort from WareHouse to Style or Style/color and press
*-- Preview or Run button for the second,third...time.
IF USED('&lcStDyTemp') AND lcRpSortBy $ 'CS'
  SELECT (lcStDyTemp)
  SET RELATION TO
ENDIF
IF !USED('&lcStyTemp')
  WAIT WINDOW 'Collecting data.....' NOWAIT
  *--Copy to the temp file each time we call the grid not each time we press Preview or Run.
  SELECT STYLE
  GOTO TOP
  *--Copy Style file data to the temp file.
  COPY TO (gcWorkDir+lcStyTemp)
  USE (gcWorkDir+lcStyTemp) EXCLUSIVE IN 0
  SELECT (lcStyTemp)
  INDEX ON SUBSTR(STYLE,2,lnMajorLen-1) + SUBSTR(STYLE,1,1) + SUBSTR(STYLE,lnMajorLen+1)  TAG STYLE
  INDEX ON SUBSTR(STYLE,2) TAG STYCOL
  *--Refresh the index
  USE
  SELECT 0
  USE (gcWorkDir+lcStyTemp) EXCLUSIVE
  SET ORDER TO TAG STYLE
  WAIT CLEAR
ELSE
  SELECT (lcStyTemp)
  SET FILTER TO  
ENDIF  
llReturn = .F.
SELECT (lcStyTemp)
*-- Clear the relation to rebuild it.
SET RELATION TO
GO TOP
IF !EMPTY(lcFrstSty)
   =SEEK(lcFrstSty)
ENDIF
lcRpExp = STRTRAN(lcRpExp,'STYLE.','')
GO TOP
LOCATE REST FOR &lcRpExp
IF EOF() OR (!EMPTY(lcSndSty) AND SUBSTR(STYLE,1,lnMajorLen)>lcSndSty)
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
SET FILTER TO &lcRpExp
GO TOP
*-----------------------------------------------------------------
*-- [A] WORK IN PROCESS REPORT
SET TALK OFF
PageNo   = 00
lnMaxLine  = 58
DO lpReport
IF llReturn
  RETURN
ENDIF  
*--Set to maupulate the printing.
SET CONSOLE ON
DO ENDREPORT
SET DEVICE TO SCREEN

*! ***********************************************************
*! Name : lfVldWare.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 02/01/99
* Refer To : (C101393)
*! ***********************************************************
*! Synopsis : Validates the warehouse entery.
*! ***********************************************************
*! Passed :
*!        Parameters :
*!        lcPrmWrCod : The entered warehouse code, To be validated.
*!        lnY        : The Row in which the warehouse description will
*!                     be displaied.
*!        lnX        : The Col.in which the warehouse description will
*!                       be displaied.
*!        Files      : The WareHouse file has to be opened.
*! ***********************************************************

FUNCTION lfVldWare
PARAMETERS lcPrmWrCod,lnY,lnX

*-- Check if the passed warehouse code exists in the Warehouse file
*-- or not.
SELECT WareHous
IF !SEEK (&lcPrmWrCod)
  *-- If the passed warehouse code does not exist in the Warehouse
  *-- file then browse it, And give the user the ability to press
  *-- escape from the browse screen to select all the warehouses.
  &lcPrmWrCod = gfBrowWare (.T.)
ENDIF
IF !EMPTY(&lcPrmWrCod)
  *-- If the user did not press escape then display the
  *-- warehouse description.
  @ lnY,lnX SAY SUBSTR(cDesc,1,24)
ELSE
  *-- If the user pressed escape then clear the description aria.
  SET COLOR TO &qClrNrm
  @ lnY,lnX TO lnY,lnX+24 CLEAR
  SET COLOR TO &qClrSay
ENDIF
RETURN (.T.)


*!*************************************************************
*! Name : lfVldType.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 02/01/99
* Refer To : (C101393)
*! ************************************************************
*! Synopsis : Validates the transaction type entery for the
*!            inventory journal report (Option[L]).
*! ************************************************************
*! Called from :
*!         Procedures : Sty900 Opiton [L].
*! ************************************************************

FUNCTION lfVldType

*-- Initialize the returned value.
llRet = .T.
*-- We will accept the empty type, So we will validate only the non empty
*-- type.
IF !EMPTY(lcType)
  *-- Loop for the string length until find any unvalid character.
  lnCount = 1
  DO WHILE llRet .AND. lnCount <= LEN(ALLTRIM(lcType))
    llRet = IIF(SUBSTR(lcType,lnCount,1) $ lcVldType, .T., .F.)
    lnCount = lnCount + 1
  ENDDO
ENDIF
RETURN (llRet)

*********************************************************
*: Prg   : lpReport
*: Desc. : Print the report.
*: AUTH  : Adel Mohammed El Gazzar (ADEL)
*: DATE  : 02/01/99
*: Refer To : (C101393)
*********************************************************
PROCEDURE lpReport

llDyeLot   = (gfGetMemVar('M_DYELOT') = 'Y')
llMultiLoc = (gfGetMemVar('M_WareLoc') = 'Y')
R_WIDTH    = 'W'        && Wide report.
PageNo     = 0          && Page No.
ROW        = 99         && Current printing row.
lnMaxLine  = 51                         && Max Lines in the page.
R_TITLE    = [STYLE INVENTORY SUMMARY]  && Report title.
llSubOk    = .T.                        && Style subTotal is printed.
IF llMultiWH
  *-- Don't print styles that have totstk=0.
  IF lcRpSortBy $ 'CS' AND lcRpWareDt = 'N'
    SELECT (lcStyTemp)
    lcSFilter=FILTER()
    SET FILTER TO
    lcSFilter=lcSFilter+' AND TOTSTK<>0'
    SET FILTER TO &lcSFilter
  ENDIF
ENDIF
llDyelotDt = (lcRpPrtDye = 'Y')
IF lcRpSortBy $ 'CS'
  SELECT (lcStyTemp)
  IF lcRpSortBy = "S"
    SET ORDER TO TAG STYLE
  ELSE
    SET ORDER TO TAG STYCOL
  ENDIF
  IF llMultiLoc
    SET RELATION TO Style INTO WhsLoc ADDITIVE
  ENDIF
ELSE
  SELECT StyDye
  IF USED('&lcStDyTemp')
    SELECT (lcStDyTemp)
  ELSE  
    COPY TO (gcWorkDir+lcStDyTemp)
    USE (gcWorkDir+lcStDyTemp) IN 0
    SELECT (lcStDyTemp)
    INDEX ON CWARECODE+SUBSTR(STYLE,2,lnMajorLen-1)+SUBSTR(STYLE,1,1)+SUBSTR(STYLE,lnMajorLen+1) TAG STYDYEW
    IF llMultiLoc
      SET RELATION TO Style+SPACE(6)+cWareCode INTO WhsLoc ADDITIVE
    ENDIF
    SELECT (lcStyTemp)
    INDEX ON SUBSTR(STYLE,2,lnMajorLen-1) + SUBSTR(STYLE,1,1) + SUBSTR(STYLE,lnMajorLen+1)  TAG STYLE
    SELECT (lcStDyTemp)
    SET RELATION TO SUBSTR(STYLE,2,lnMajorLen-1)+SUBSTR(STYLE,1,1)+SUBSTR(STYLE,lnMajorLen+1) INTO (lcStyTemp) ADDITIVE
    IF !EMPTY(lcRpWrOnly)
      *-- Add (totstk<>0) to STYDYE filter.
      *--We remove this filter from sty910 in case of multiwarehouse.
      SET FILTER TO cWareCode = lcRpWrOnly .AND. TotStk<>0 .AND. !EOF('&lcStyTemp')
    ELSE
      *--Add (totstk<>0) to STYDYE filter.
      *--We remove this filter from sty910 in case of multiwarehouse.
      SET FILTER TO TotStk<>0 .AND. !EOF('&lcStyTemp')
    ENDIF
  ENDIF    
ENDIF
GO TOP
IF EOF()
  llReturn = .T.
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
*--Start the report printing.
SET DEVICE TO PRINT
*-- Incease the Scale size description 
lcScale   = GETSCALE(&lcStyTemp..SCALE,SPACE(2))
XPRT_DYE = .F.     && flag used when printing dyelot info. to not to repeat
                   && the STYLE/clr info when switching to new page ...
DIMENSION laWhSubTot[11]
STORE 0 TO laWhSubTot
STORE 0 TO TOT1,TOT2,TOT3,TOT4,TOT5,TOT6,TOT7,TOT8,TOT9,TOT10,TOT11
STORE 0 TO lnSub1,lnSub2,lnSub3,lnSub4,lnSub5,lnSub6,lnSub7,lnSub8,lnSub9,lnSub10,lnSub11           
STORE 0 TO lnOShp1,lnOShp2,lnOShp3,lnOShp4,lnOShp5,lnOShp6,lnOShp7,lnOShp8,lnOShp9,lnOShp10,lnOShp11
lcOldStyle = SUBSTR(STYLE,2,lnMajorLen-1)   &&lcOldStyle = Style
lcOldColor = SUBSTR(STYLE,lnMajorLen+1)
lcOldsCol  = SUBSTR(STYLE,2,lnMajorLen-1) + SUBSTR(STYLE,lnMajorLen+1)
lcOldWreCd = IIF(lcRpSortBy = 'W',&lcStDyTemp..cWareCode,'')
lcKey = IIF(lcRpSortBy $ "SW",SUBSTR(STYLE,1,lnMajorLen),SUBSTR(STYLE,lnMajorLen+1))
DO CASE
  CASE lcRpSortBy $ "SW"
    SUB_SAVE = "SUBSTR(STYLE,1,lnMajorLen)+SPACE(2)+&lcStyTemp..Desc+SPACE(2)+IIF(EOF(),' ',lcScale)"
  CASE lcRpSortBy = "C"
    lcColorDesc = gfCodDes(SUBSTR(STYLE,lnMajorLen+2) , 'COLOR')  
    SUB_SAVE = "SUBSTR(STYLE,lnMajorLen+2)+SPACE(2)+IIF(!EMPTY(lcColorDesc),SUBSTR(lcColorDesc,1,15),SPACE(15))+SPACE(13)+IIF(EOF(),' ',lcScale)"
  OTHERWISE
    SUB_SAVE ="SUBSTR(STYLE,1,lnMajorLen) + SPACE(24)+IIF(EOF(),' ',lcScale)"
ENDCASE
SCAN WHILE INKEY() <> 32 FOR IIF(lcRpSortBy = 'W',EMPTY(Dyelot),.T.) 
  WAIT WINDOW 'Report printing ... <Space Bar> to Abort' NOWAIT
  *--UPDATE THE STYLE AND COLOR
  lcOldsCol = SUBSTR(Style,2)
  lcKey     = IIF(lcRpSortBy $ "SW",SUBSTR(STYLE,1,lnMajorLen),SUBSTR(STYLE,lnMajorLen+1))
  *-- Don't print styles that don't have stock in both style and stydye files.
  IF (lcRpSortBy='S' OR lcRpSortBy ='C' ) AND lcRpWareDt='Y'
    SELECT StyDye
    =SEEK(&lcStyTemp..Style)
    LOCATE REST WHILE Style=&lcStyTemp..Style FOR TotStk<>0
    IF !FOUND()
      LOOP
    ENDIF
    SELECT (lcStyTemp)
  ENDIF
  IF ROW > lnMaxLine
    =lfPrnHdr()
  ENDIF
  IF lcRpPrtLoc = 'Y'
    IF SEEK(lcOldStyle+lcOldColor+IIF(llMultiWH,lcOldWreCd,''),'WhsLoc');
       .AND. lcRpWareDt = 'N' .AND.;
       (lcOldStyle+lcOldColor <> Style .OR. lcRpSortBy = 'W' .AND. lcOldWreCd <> cWareCode)
      =lfPrnLoc()
    ENDIF
    IF llDyelotDt .AND. &lcStyTemp..cDye_Flg = 'Y' .AND. !llPrnWHDitl .AND.;
       lcOldStyle+lcOldColor <> Style
      XPRT_DYE = .T.
      =lfPrnDyelt(lcOldStyle,lcOldColor)
      XPRT_DYE = .F.
    ENDIF
    lcOldColor = SUBSTR(STYLE,lnMajorLen+1)
  ENDIF
  IF (lcOldStyle <> SUBSTR(Style,2,lnMajorLen-1) .OR. (lcRpSortBy = 'W' .AND. lcOldWreCd <> cWareCode));
     AND lcRpSortBy <> "C"
    =lfSubTotal()
    lcOldStyle = SUBSTR(STYLE,2,lnMajorLen-1)  &&lcOldStyle = Style
    llSubOk    = .T.
  ENDIF
  IF lcRpSortBy = "C" AND lcOldsCol<> SUBSTR(Style,2)
    =lfSubTotal()
    lcOldsCol = SUBSTR(Style,2)
    llSubOk    = .T.
  ENDIF
  IF lcRpSortBy = 'W'
    IF lcOldWreCd <> cWareCode
      =lfWHSubTot()
      STORE 0 TO laWhSubTot
      lcOldColor = SUBSTR(STYLE,lnMajorLen+1)
      lcOldStyle = SUBSTR(STYLE,2,lnMajorLen-1)   &&lcOldStyle = Style
      lcOldWreCd = IIF(lcRpWareDt = 'Y',cWareCode,'')
    ENDIF
  ELSE
    IF lcRpWareDt = 'Y'
      llPrnWHDitl = .T.
      lcOldStyle = SUBSTR(STYLE,2,lnMajorLen-1)   &&lcOldStyle = Style
      lcOldColor = SUBSTR(STYLE,lnMajorLen+1)
    ENDIF
  ENDIF
  IF IIF(lcRpSortBy $ "WS",lcKey <> SUBSTR(STYLE,1,lnMajorLen),lcKey <>SUBSTR(STYLE,lnMajorLen+1))
    ROW = ROW + 1
    @ ROW,0 SAY '** '
    @ ROW,3 SAY EVAL(SUB_SAVE)
    lcKey = IIF(lcRpSortBy $ "WS",SUBSTR(STYLE,1,lnMajorLen),SUBSTR(STYLE,lnMajorLen+1))
  ENDIF
  IF !XPRT_DYE
    lcAlias = IIF(lcRpSortBy $ 'CS',lcStyTemp,lcStDyTemp)
    llSubOk = .F.
    ROW = ROW +1
    @ ROW,0  SAY IIF(lcRpSortBy = "C",SUBSTR(&lcAlias..STYLE,1,lnMajorLen),SUBSTR(&lcAlias..STYLE,lnMajorLen+2))
    lcColorDesc = gfCodDes(SUBSTR(STYLE,lnMajorLen+2) , 'COLOR')      
    lcStyle = SUBSTR(STYLE,1,lnMajorLen)
    @ ROW,12 SAY IIF(lcRpSortBy = "C",&lcStyTemp..Desc,SUBSTR(lcColorDesc,1,15))     && Color Description
    @ ROW,38 SAY &lcAlias..STK1+0   PICTURE '999999'
    @ ROW,45 SAY &lcAlias..STK2+0   PICTURE '999999'
    @ ROW,52 SAY &lcAlias..STK3+0   PICTURE '999999'
    @ ROW,59 SAY &lcAlias..STK4+0   PICTURE '999999'
    @ ROW,66 SAY &lcAlias..STK5+0   PICTURE '999999'
    @ ROW,73 SAY &lcAlias..STK6+0   PICTURE '999999'
    @ ROW,80 SAY &lcAlias..STK7+0   PICTURE '999999'
    @ ROW,87 SAY &lcAlias..STK8+0   PICTURE '999999'
    @ ROW,94 SAY &lcAlias..TOTSTK+0 PICTURE '999999'
    IF qCostPrv            && COSTING ACCCESS.
      *-- Round the value of average cost to 2 decimals because 
      *-- it is in the table with 7 decimals
      @ ROW,101 SAY IIF(llAvgCost,ROUND(&lcAlias..AVE_COST,2),&lcStyTemp..TOTCOST) PICTURE '9999.99'
      @ ROW,109 SAY ROUND(&lcAlias..TOTSTK*IIF(llAvgCost,&lcAlias..AVE_COST,&lcStyTemp..TOTCOST),2) PICTURE '99999999.99'
    ENDIF
    @ ROW,121   SAY ROUND(&lcAlias..TOTSTK*&lcStyTemp..PRICEA,2)  PICTURE '99999999.99'
  ENDIF
  IF (lcRpSortBy $ 'SC' .AND. lcRpWareDt = 'Y') .AND. SEEK(STYLE+IIF(!EMPTY(lcRpWrOnly),lcRpWrOnly,''),'STYDYE')
    SELECT STYDYE
    SCAN REST WHILE STYLE+CWARECODE=&lcStyTemp..STYLE+IIF(!EMPTY(lcRpWrOnly),lcRpWrOnly,'')
      ROW=ROW+1
      lcOldWreCd=cWareCode
      @ ROW,20 SAY 'Warehouse ' + cWareCode
      @ ROW,38 SAY STK1     PICTURE '999999'
      @ ROW,45 SAY STK2     PICTURE '999999'
      @ ROW,52 SAY STK3     PICTURE '999999'
      @ ROW,59 SAY STK4     PICTURE '999999'
      @ ROW,66 SAY STK5     PICTURE '999999'
      @ ROW,73 SAY STK6     PICTURE '999999'
      @ ROW,80 SAY STK7     PICTURE '999999'
      @ ROW,87 SAY STK8     PICTURE '999999'
      @ ROW,94 SAY TOTSTK   PICTURE '999999'
      IF qCostPrv            && COSTING ACCCESS.
        *-- Round the average cost to 2 decimals.
        @ ROW,101 SAY IIF(llAvgCost,ROUND(AVE_COST,2),&lcStyTemp..TOTCOST) PICTURE '9999.99'
        *-- Fix the bug of rounding the warehouse line.
        @ ROW,109 SAY ROUND(TOTSTK*IIF(llAvgCost,AVE_COST,&lcStyTemp..TOTCOST),2) ;
                      PICTURE '99999999.99'                      
      ENDIF
      @ ROW,121   SAY ROUND(TOTSTK*&lcStyTemp..PRICEA,0)  PICTURE '99999999.99'
      IF lcRpPrtLoc = 'Y' .AND. SEEK(lcOldStyle+lcOldColor+IIF(llMultiWH,lcOldWreCd,''),'WhsLoc')
        =lfPrnLoc()
      ENDIF
      IF llDyelotDt .AND. &lcStyTemp..cDye_Flg = 'Y'
        =lfPrnDyelt(SUBSTR(&lcStyTemp..STYLE,1,lnMajorLen),SUBSTR(&lcStyTemp..STYLE,lnMajorLen+1))
      ENDIF
    ENDSCAN
  ENDIF
  IF llDyelotDt .AND. &lcStyTemp..cDye_Flg = 'Y' .AND. !llPrnWHDitl .AND. lcRpPrtLoc <> 'Y'
    XPRT_DYE = .T.
    =lfPrnDyelt(SUBSTR(&lcStyTemp..STYLE,1,lnMajorLen),SUBSTR(&lcStyTemp..STYLE,lnMajorLen+1))
    XPRT_DYE = .F.
  ENDIF
  SELECT (lcStyTemp)    
  FOR I=1 TO 8
    Z=STR(I,1)
    IF &lcAlias..STK&Z<0
      lnOShp&Z  = lnOShp&Z  + &lcAlias..STK&Z
    ELSE
      lnSub&Z  = lnSub&Z  + &lcAlias..STK&Z
    ENDIF
  ENDFOR
  IF &lcAlias..TOTSTK < 0
    lnOShp9  = lnOShp9  + &lcAlias..TOTSTK
    *-- Fix the bug of rounding the warehouse line.
     lnOShp10 = lnOShp10 + ROUND(&lcAlias..TOTSTK*IIF(llAvgCost,&lcAlias..AVE_COST,&lcStyTemp..TOTCOST),2)
    lnOShp11 = lnOShp11 + ROUND(&lcAlias..TOTSTK*&lcStyTemp..PRICEA,0)
  ELSE
    lnSub9  = lnSub9  + &lcAlias..TOTSTK
    *-- (Begin) Fix the bug of rounding the warehouse line.
    lnSub10 = lnSub10 + ROUND(&lcAlias..TOTSTK*IIF(llAvgCost,&lcAlias..AVE_COST,&lcStyTemp..TOTCOST),2)
    lnSub11 = lnSub11 + ROUND(&lcAlias..TOTSTK*&lcStyTemp..PRICEA,0)
  ENDIF
  TOT1     = TOT1  + &lcAlias..STK1
  TOT2     = TOT2  + &lcAlias..STK2
  TOT3     = TOT3  + &lcAlias..STK3
  TOT4     = TOT4  + &lcAlias..STK4
  TOT5     = TOT5  + &lcAlias..STK5
  TOT6     = TOT6  + &lcAlias..STK6
  TOT7     = TOT7  + &lcAlias..STK7
  TOT8     = TOT8  + &lcAlias..STK8
  TOT9     = TOT9  + &lcAlias..TOTSTK
  *-- Fix the bug of rounding the warehouse line
  TOT10    = TOT10 + ROUND(&lcAlias..TOTSTK*IIF(llAvgCost,&lcAlias..AVE_COST,&lcStyTemp..TOTCOST),2)
  TOT11    = TOT11 + ROUND(&lcAlias..TOTSTK*&lcStyTemp..PRICEA,0)
ENDSCAN
IF lcRpPrtLoc = 'Y'
  *-- Added condition that lcRpWareDt = 'N'.
  IF SEEK(lcOldStyle+lcOldColor+IIF(llMultiWH,lcOldWreCd,''),'WhsLoc');
    .AND. lcOldStyle+lcOldColor <> Style+Color .AND. lcRpWareDt = 'N'
    =lfPrnLoc()
  ENDIF
  IF llDyelotDt .AND. &lcStyTemp..cDye_Flg = 'Y' .AND. !llPrnWHDitl .AND.;
     lcOldStyle+lcOldColor <> Style
    XPRT_DYE = .T.
    =lfPrnDyelt(lcOldStyle,lcOldColor)
    XPRT_DYE = .F.
  ENDIF
  lcOldColor = SUBSTR(STYLE,lnMajorLen+1)
ENDIF
IF (lcOldStyle <> SUBSTR(Style,2,lnMajorLen-1) .OR. (lcRpSortBy = 'W' .AND. lcOldWreCd <> cWareCode));
  AND lcRpSortBy <> "C"
  =lfSubTotal()
  lcOldStyle = SUBSTR(STYLE,2,lnMajorLen-1) &&lcOldStyle = Style
ENDIF
IF lcRpSortBy = "C" AND lcOldsCol<> SUBSTR(Style,2)
   =lfSubTotal()
  lcOldsCol = SUBSTR(Style,2)
  llSubOk    = .T.
ENDIF
IF lcRpSortBy = 'W' .AND. lcOldWreCd <> cWareCode
  =lfWHSubTot()
  lcOldColor =  SUBSTR(STYLE,lnMajorLen+1)
  lcOldStyle = SUBSTR(STYLE,2,lnMajorLen-1)  &&lcOldStyle = Style
  lcOldWreCd = IIF(lcRpWareDt = 'Y',cWareCode,'')
ENDIF
*--Print the report grand total.
ROW = ROW + 1
@ ROW,0  SAY '*** Total ***'
@ ROW,38 SAY TOT1 PICTURE '999999'
@ ROW,45 SAY TOT2 PICTURE '999999'
@ ROW,52 SAY TOT3 PICTURE '999999'
@ ROW,59 SAY TOT4 PICTURE '999999'
@ ROW,66 SAY TOT5 PICTURE '999999'
@ ROW,73 SAY TOT6 PICTURE '999999'
@ ROW,80 SAY TOT7 PICTURE '999999'
@ ROW,87 SAY TOT8 PICTURE '999999'
@ ROW,94 SAY TOT9 PICTURE '999999'
IF qCostPrv                && COSTING ACCESS.
  @ ROW,109  SAY TOT10  PICTURE '99999999.99'
ENDIF
@ ROW,121  SAY TOT11  PICTURE '99999999.99'
ROW=ROW+1
@ ROW,00 SAY REPLICATE('=',132)
@ ROW+1,0  SAY [] &&just to get last line from some printers

*:************************************************************************
*: Prg   : lfPrnHdr
*: Desc. : Print the header of the report.
*: AUTH: Adel Mohammed El Gazzar (ADEL)
*: DATE: 02/01/99
*: Refer To : (C101393)
*:*************************************************************
FUNCTION lfPrnHdr

PageNo = PageNo + 1
DO RPT_HDR WITH 'ICAND600',lcRpTitle,R_WIDTH
ROW = 5
@ ROW,0 SAY '                                                                                                        UNIT       VALUE       SALES'
ROW = ROW + 1
DO CASE
  CASE lcRpSortBy = "C"
     @ ROW,0 SAY UPPER('&lcMajTitle       &lcMajTitle DESCR.                 SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTAL    COST        COST       VALUE')
  CASE lcRpSortBy = "S"
     @ ROW,0 SAY UPPER('&lcNonMajTl     &lcNonMajTl DESCR.                 SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTAL    COST        COST       VALUE')
  OTHERWISE
     lcNonMajD = lcNonMajTl+'DESCR.'
     @ ROW,0 SAY UPPER('&lcNonMajTl &lcNonMajD &lcMajTitle DESCRIPTION    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTAL    COST        COST       VALUE')
ENDCASE
ROW = ROW + 1
@ ROW,0 SAY REPLICATE('-',132)
ROW = ROW + 1
*-- Incease the Scale size description 
lcScale = GETSCALE(&lcStyTemp..SCALE,SPACE(2))
IF llSubOk
  IF lcRpSortBy = 'W'
    @ ROW,0 SAY '**  Warehouse ' + cWareCode
    ROW = ROW + 1
  ENDIF
  @ ROW,0 SAY '** '
  @ ROW,3 SAY EVAL(SUB_SAVE)
  IF XPRT_DYE            && IF IN THE MIDDLE OF PRINTING DYELOTS
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',59)
    @ ROW,60 SAY '<< DYELOTS >>'
    @ ROW,73 SAY REPLICATE('-',59)
    ROW = ROW + 1
  ENDIF
ENDIF

*:************************************************************************
*: Prg   : lfSubTotal
*: Desc. : Print the subtotal.
*: AUTH  : Adel Mohammed El Gazzar (ADEL)
*: DATE  : 02/01/99
*: Refer To : (C101393)
*:*************************************************************
FUNCTION lfSubTotal

ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW + 1
@ROW,00  SAY '** SubTotal **'
@ROW,38  SAY lnSub1   PICTURE '999999'
@ROW,45  SAY lnSub2   PICTURE '999999'
@ROW,52  SAY lnSub3   PICTURE '999999'
@ROW,59  SAY lnSub4   PICTURE '999999'
@ROW,66  SAY lnSub5   PICTURE '999999'
@ROW,73  SAY lnSub6   PICTURE '999999'
@ROW,80  SAY lnSub7   PICTURE '999999'
@ROW,87  SAY lnSub8   PICTURE '999999'
@ROW,94  SAY lnSub9   PICTURE '999999'
IF qCostPrv                && COSTING ACCESS.
  @ROW,109 SAY lnSub10  PICTURE '99999999.99'
ENDIF
@ROW,121 SAY lnSub11  PICTURE '99999999.99'
IF lnOShp9 <> 0
  ROW = ROW + 1
  @ROW,00  SAY '** OverShip **'
  @ROW,38  SAY lnOShp1   PICTURE '999999'
  @ROW,45  SAY lnOShp2   PICTURE '999999'
  @ROW,52  SAY lnOShp3   PICTURE '999999'
  @ROW,59  SAY lnOShp4   PICTURE '999999'
  @ROW,66  SAY lnOShp5   PICTURE '999999'
  @ROW,73  SAY lnOShp6   PICTURE '999999'
  @ROW,80  SAY lnOShp7   PICTURE '999999'
  @ROW,87  SAY lnOShp8   PICTURE '999999'
  @ROW,94  SAY lnOShp9   PICTURE '999999'
  IF qCostPrv                && COSTING ACCESS.
    @ROW,109 SAY lnOShp10  PICTURE '99999999.99'
  ENDIF
  @ROW,121 SAY lnOShp11  PICTURE '99999999.99'
ENDIF
ROW = ROW + 1
laWhSubTot[1]  = laWhSubTot[1]  + lnSub1+lnOShp1
laWhSubTot[2]  = laWhSubTot[2]  + lnSub2+lnOShp2
laWhSubTot[3]  = laWhSubTot[3]  + lnSub3+lnOShp3
laWhSubTot[4]  = laWhSubTot[4]  + lnSub4+lnOShp4
laWhSubTot[5]  = laWhSubTot[5]  + lnSub5+lnOShp5
laWhSubTot[6]  = laWhSubTot[6]  + lnSub6+lnOShp6
laWhSubTot[7]  = laWhSubTot[7]  + lnSub7+lnOShp7
laWhSubTot[8]  = laWhSubTot[8]  + lnSub8+lnOShp8
laWhSubTot[9]  = laWhSubTot[9]  + lnSub9+lnOShp9
laWhSubTot[10] = laWhSubTot[10] + lnSub10+lnOShp10
laWhSubTot[11] = laWhSubTot[11] + lnSub11+lnOShp11
@ROW,00  SAY '**   Net    **'
@ROW,38  SAY lnSub1+lnOShp1  PICTURE '999999'
@ROW,45  SAY lnSub2+lnOShp2  PICTURE '999999'
@ROW,52  SAY lnSub3+lnOShp3  PICTURE '999999'
@ROW,59  SAY lnSub4+lnOShp4  PICTURE '999999'
@ROW,66  SAY lnSub5+lnOShp5  PICTURE '999999'
@ROW,73  SAY lnSub6+lnOShp6  PICTURE '999999'
@ROW,80  SAY lnSub7+lnOShp7  PICTURE '999999'
@ROW,87  SAY lnSub8+lnOShp8  PICTURE '999999'
@ROW,94  SAY lnSub9+lnOShp9  PICTURE '999999'
IF qCostPrv                && COSTING ACCESS.
  @ROW,109 SAY lnSub10+lnOShp10 PICTURE '99999999.99'
ENDIF
@ROW,121 SAY lnSub11+lnOShp11 PICTURE '99999999.99'
ROW=ROW+1
@ ROW,00 SAY REPLICATE('=',132)
*-- Incease the Scale size description 
lcScale = GETSCALE(&lcStyTemp..SCALE,SPACE(2))
STORE 0 TO lnSub1,lnSub2,lnSub3,lnSub4,lnSub5,lnSub6,lnSub7,lnSub8,lnSub9,lnSub10,lnSub11           
STORE 0 TO lnOShp1,lnOShp2,lnOShp3,lnOShp4,lnOShp5,lnOShp6,lnOShp7,lnOShp8,lnOShp9,lnOShp10,lnOShp11

*:************************************************************************
*: Prg   : lfPrnLoc
*: Desc. : Print the loacations for a cretin style.
*: AUTH  : Adel Mohammed El Gazzar (ADEL)
*: DATE  : 02/01/99
*: Refer To : (C101393)
*:************************************************************************
FUNCTION lfPrnLoc
PRIVATE lnOldWA,lnCol,lnCount

lnOldWA = SELECT()
ROW = ROW + 1
@ ROW,0 SAY '** Locations **'
SELECT WhsLoc
lnCol   = 0
lnCount = 3
SCAN REST WHILE Style+cWareCode+cLocation = lcOldStyle+IIF(llMultiWH,lcOldColor+lcOldWreCd,'')
  IF ROW > lnMaxLine
    =lfPrnHdr()
  ENDIF
  IF MOD(lnCount,3) = 0
    ROW     = ROW + 1
    lnCol   = 0
    lnCount = 1
  ELSE
    lnCol   = lnCol + 12
    lnCount = lnCount + 1
  ENDIF
  @ Row,lnCol SAY cLocation
ENDSCAN
ROW = ROW + 1
SELECT (lnOldWA)

*:************************************************************************
*: Prg   : lfPrnDyelt
*: Desc. : Print the deylots for the style color.
*: AUTH  : Adel Mohammed El Gazzar (ADEL)
*: DATE  : 02/01/99
*: Refer To : (C101393)
*:************************************************************************
FUNCTION lfPrnDyelt
PARAMETERS lcPStyle,lcPColor

PRIVATE lcAlias,lnOldWA, lnRecNo, llPrnDyHdr
lnOldWA = SELECT()
lnRecNo = RECNO()
llPrnDyHdr = .T.
lcAlias = IIF(USED('&lcStDyTemp'),lcStDyTemp,'STYDYE')
SELECT (lcAlias)
IF SEEK(IIF(llMultiWH .AND. lcRpSortBy = 'W',lcOldWreCd,'')+lcPStyle+lcPColor+IIF(llMultiWH .AND. lcRpSortBy = 'S',lcOldWreCd,''),lcAlias)
  SCAN REST WHILE &lcAlias..Style = lcPStyle+lcPColor FOR !EMPTY(Dyelot) .AND. IIF(llMultiWH,cWareCode = lcOldWreCd, .T. )
    IF llPrnDyHdr
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('-',59)+'<< DYELOTS >>'+REPLICATE('-',60)
      ROW=ROW+1
      llPrnDyHdr = .F.
    ENDIF
    IF ROW > lnMaxLine
      =lfPrnHdr()
    ENDIF
    @ ROW,27 SAY DYELOT
    @ ROW,38 SAY STK1    PICTURE '999999'
    @ ROW,45 SAY STK2    PICTURE '999999'
    @ ROW,52 SAY STK3    PICTURE '999999'
    @ ROW,59 SAY STK4    PICTURE '999999'
    @ ROW,66 SAY STK5    PICTURE '999999'
    @ ROW,73 SAY STK6    PICTURE '999999'
    @ ROW,80 SAY STK7    PICTURE '999999'
    @ ROW,87 SAY STK8    PICTURE '999999'
    @ ROW,94 SAY TOTSTK  PICTURE '999999'
    ROW = ROW + 1
  ENDSCAN
ENDIF
SELECT (lnOldWA)
GOTO lnRecNo

*:************************************************************************
*: Prg   : lfWHSubTot
*: Desc. : Print the warehouse subtotal.
*: AUTH  : Adel Mohammed El Gazzar (ADEL)
*: DATE  : 02/01/99
*: Refer To : (C101393)
*:************************************************************************
FUNCTION lfWHSubTot

ROW = ROW + 1
@ ROW,00  SAY '** Net Warehouse ' + lcOldWreCd + ' ** '
@ ROW,38  SAY laWhSubTot[1]  PICTURE '999999'
@ ROW,45  SAY laWhSubTot[2]  PICTURE '999999'
@ ROW,52  SAY laWhSubTot[3]  PICTURE '999999'
@ ROW,59  SAY laWhSubTot[4]  PICTURE '999999'
@ ROW,66  SAY laWhSubTot[5]  PICTURE '999999'
@ ROW,73  SAY laWhSubTot[6]  PICTURE '999999'
@ ROW,80  SAY laWhSubTot[7]  PICTURE '999999'
@ ROW,87  SAY laWhSubTot[8]  PICTURE '999999'
@ ROW,94  SAY laWhSubTot[9]  PICTURE '999999'
IF qCostPrv                && COSTING ACCESS.
  @ ROW,109 SAY laWhSubTot[10] PICTURE '99999999.99'
ENDIF
@ ROW,121 SAY laWhSubTot[11] PICTURE '99999999.99'
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',132)
IF !EMPTY(cWareCode)
  ROW = ROW + 1
  @ ROW,0 SAY '**  Warehouse ' + cWareCode
  lcScale   = GETSCALE(&lcStyTemp..SCALE,SPACE(2))
ENDIF

****************************************************************
*: Prg   : lfwRepWhen
*: Desc. : Validate the WHEN option grid function.
*: AUTH  : Adel Mohammed El Gazzar (ADEL)
*: DATE  : 02/01/99
*: Refer To : (C101393)
****************************************************************
FUNCTION lfwRepWhen

FUNCTION lfwWWRepWhen1
IF !USED('&lcStyTemp')
  *--Copy to the temp file each time we call the grid not each time we press Preview or Run.
  SELECT STYLE
  GOTO TOP
  *--Copy Style file data to the temp file.
  COPY TO (gcWorkDir+lcStyTemp)
  USE (gcWorkDir+lcStyTemp) IN 0
  SELECT (lcStyTemp)
  INDEX ON SUBSTR(STYLE,2,lnMajorLen-1) + SUBSTR(STYLE,1,1) + SUBSTR(STYLE,lnMajorLen+1)  TAG STYLE
  INDEX ON SUBSTR(STYLE,2) TAG STYCOL
ELSE
  SET FILTER TO  
ENDIF  

****************************************************************************
* FUNC: lfwOldVal
* DESC: To get the old value.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 01/26/98
* Refer To : (C101393)
****************************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))

****************************************************************************
* FUNC: lfvStyle
* DESC: To valid the style.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 01/27/98
* Refer To : (C101393)
****************************************************************************
FUNCTION lfvStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 
*-- Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*-- Varible to hold  the value of the current GET field
lcObjVal = EVALUATE(SYS(18)) 

*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF 
SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Adel Mohammed El Gazzar (ADEK)
*! Date      : 01/27/99
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
* Refer To : (C101393)
*!*************************************************************
FUNCTION lfEvalSegs

**Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure.
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

*--Fill the sort by arrays here
DIMENSION laSortArr[3,1]
DIMENSION laFilSort[3,1]
laSortArr[1,1] = (lcMajTitle)
laSortArr[2,1] = (lcMajTitle)+'/'+(lcNonMajTlt)
laSortArr[3,1] = 'Warehouse'
laFilSort[1,1] = 'S'
laFilSort[2,1] = 'C'
laFilSort[3,1] = 'W'

RETURN ''

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Adel Mohammed El Gazzar (ADEK)
*! Date      : 01/27/99
*! Purpose   : Validation function for validating Fabric Code
*!*************************************************************
*! Called from : Only this color [Option Grid]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SELECT Fabric
lcFabOrder = ORDER()
SET ORDER TO Fabric
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
SELECT Fabric
SET ORDER TO &lcFabOrder
SELECT(lnAlias)

****************************************************************************
* FUNC: lfvSortBy
* DESC: To clear read in case of sorting by style to get the cost.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/06/98
* Refer To : (C101393)
****************************************************************************
FUNCTION lfvSortBy

IF (lcRpSortBy = 'W') OR lcOldVal =3
  CLEAR READ
ENDIF  

****************************************************************************
* FUNC: lfvWareHouse
* DESC: To valid the warehouse code.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 02/01/99
* Refer To : (C101393)
****************************************************************************
FUNCTION lfvWareHouse

lcWareHo = VARREAD()
lcTag = ORDER('WAREHOUS')
SET ORDER TO WAREHOUS IN WAREHOUS
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHo.,'WAREHOUS') 
    &lcWareHo = WAREHOUS.cWareCode
  ELSE
    &lcWareHo = gfBroWWare(.T.)
  ENDIF
ELSE
  &lcWareHo = ''
ENDIF
SET ORDER TO WAREHOUS IN WAREHOUS
