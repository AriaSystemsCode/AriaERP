****************************************************************************
*: Program file      : ICSTYMTH.PRG (C#101814)
*: Program desc.     : Styles By Month Report
*: System            : Aria Apparel System (A27).
*: Module            : Inventory Control (IC)
*: Developer         : Khalid Mohi El-Din Mohamed KHM
*: Date              : 04/24/2000
*C200137,1 TAK 10/10/2000
*:**************************************************************************
*-- (1)
*--Get the lower and higher range of date
ldFromDate = IIF(EMPTY(ldRpFDt),CTOD('01/01/'+STR(YEAR(gdSysDate),4)),ldRpFDt)
ldToDate   = ldRpTDt

*-- If the user enters more than 12 periods then "To" date will be 
*-- 12 periods starting from the FROM date.
*IF ldRpTDt = {} OR (YEAR(ldRpTDt) > YEAR(ldFromDate) ;
*                     AND MONTH(ldFromDate) >= MONTH(ldRpTDt))
*  ldToDate = CTOD(PADL(ALLTRIM(STR(MONTH(ldFromDate))),2,'0')+'/'+;
*             PADL(ALLTRIM(STR(DAY(ldFromDate))),2,'0')+'/'+;
*             STR(YEAR(ldFromDate)+1,4))
*  ldToDate = GOMONTH(ldFromDate,11)
*ENDIF

ldToDate   = IIF(EMPTY(ldRpTDt),CTOD(PADL(MONTH(gdSYSDate),2,'0')+'/'+;
             PADL(DAY(gdSYSDate),2,'0')+'/'+STR(YEAR(gdSysDate),4)),ldRpTDt)

*-- If the user enters more than 12 periods then "To" date would be 
*-- the "From" date + 1 year
IF ldToDate - ldFromDate > 366
  ldToDate = CTOD(PADL(ALLTRIM(STR(MONTH(ldFromDate))),2,'0')+'/'+;
             PADL(ALLTRIM(STR(DAY(ldFromDate))),2,'0')+'/'+;
             STR(YEAR(ldFromDate)+1,4)) - 1
ENDIF


*-- To get the years of the 12 periods
*ldYearD = GOMONTH(ldFromDate,-1)
*FOR lnCountr = 1 TO 12
*  lcCountr = ALLTRIM(STR(lnCountr,2))
*  ldYearD  = GOMONTH(ldYearD,1)
*  lcYear&lcCountr = RIGHT(STR(YEAR(ldYearD),4),2)
*ENDFOR


*-- To hold the filter options
lcFilter = '.T.'

*-- (2)
*-- To get the selected style's Status if any.
lcStyStats = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.STATUS'),1)
IF lnPosition > 0
  lcStyStats = laOgFxFlt[lnPosition,6]
ENDIF  
*-- Style's Status
lcFilter = IIF(!EMPTY(lcStyStats),lcFilter+' AND Style.Status $ lcStyStats',;
   															       lcFilter )

*-- (3)
*-- Check if no Location was selected then select the data for all location
*-- otherwise, select the data according to the selected location
IF lcRpSortBy $ 'AR'
  laRpLTargt[1] = 'FP-BLD'
ENDIF

= IIF(EMPTY(laRpLTargt),ACOPY(laRpLSourc,laLocation),ACOPY(laRpLTargt,laLocation))
lcAllLoc = ''
FOR lnArryCnt2 = 1 TO ALEN(laLocation,1)
  lcAllLoc = lcAllLoc + laLocation[1,lnArryCnt2]+'|'
ENDFOR

*-- (4)
*-- Check if no transaction was selected then print all the transactions
*-- otherwise, print the selected transactions.
= IIF(EMPTY(laRpTarget),ACOPY(laRpSource,laTrans),ACOPY(laRpTarget,laTrans))

*--(5)
*-- To check if there is any selected Fabrics
llEOFFab = .T.
*-- To hold the file of the selected Fabrics if any.
lcFabFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.FABRIC'),1)
IF lnPosition > 0
  lcFabFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcFabFile) AND USED(lcFabFile)
    SELECT (lcFabFile)
    GOTO TOP
    llEOFFab = EOF()
  ENDIF  
ENDIF
*-- Primary Faric
lcFilter = IIF(!llEOFFab, lcFilter+' AND SEEK(Style.Fabric,lcFabFile)',;
                                                            lcFilter)

*-- (6)
*-- To get the selected style groups.
lcGroups = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYGROUP'),1)
IF lnPosition > 0
  lcGroups = laOGFxFlt[lnPosition,6] 
ENDIF
*-- Style's Group
lcFilter = IIF(!EMPTY(lcGroups),lcFilter+' AND Style.cStyGroup $ lcGroups',;
                                                                  lcFilter)

*-- (7)
*-- To get the selected seasons
lcSeasons = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.SEASON'),1)
IF lnPosition > 0
  lcSeasons = laOGFxFlt[lnPosition,6] 
ENDIF
*-- Style's Season
lcFilter = IIF(!EMPTY(lcSeasons), lcFilter+' AND Style.Season $ lcSeasons',;
                                                                   lcFilter)

*-- Print the Form according to the sort option.
IF lcRpSortBy = 'C'  && Style Component
  lcCTStatus = ''
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.STATUS'),1)
  IF lnPosition > 0
    lcCTStatus = laOgFxFlt[lnPosition,6]
  ENDIF  
ENDIF  

*-- To calculate the overall percentage
lnPercent = 0

*-- To hold the filtering condition in the selected transactions.
STORE '' TO lcOrdFilt,lcInvFilt,lcRetFilt,lcWipFilt,lcStckFilt
*-- To hold the dates for the stock and wip transactions.
STORE {} TO ldStkFDate,ldStkTDate,ldWipFDate,ldWipTDate

**C200137,1
*Read Used periods. ldFromDate,ldToDate
STORE "  " TO lcYear1,lcYear2,lcYear3,lcYear4,lcYear5,lcYear6,lcYear7,lcYear8,lcYear9,lcYear10,lcYear11,lcYear12
DIME laUsdPrd[12]
laUsdPrd = .F.
IF YEAR(ldFromDate) <> YEAR(ldToDate)
  FOR lnPI = MONTH(ldFromDate) TO 12
    lcPI=ALLT(STR(lnPI,2))
    laUsdPrd[lnPI] = .T.
    lcYear&lcPI = RIGHT(STR(YEAR(ldFromDate),4),2)
  ENDFOR
  FOR lnPI = 1 TO MONTH(ldToDate)
    lcPI=ALLT(STR(lnPI,2))
    laUsdPrd[lnPI] = .T.
    lcYear&lcPI = RIGHT(STR(YEAR(ldToDate),4),2)
  ENDFOR
ELSE
  FOR lnPI = MONTH(ldFromDate) TO MONTH(ldToDate)
    lcPI=ALLT(STR(lnPI,2))
    laUsdPrd[lnPI] = .T.
    lcYear&lcPI = RIGHT(STR(YEAR(ldFromDate),4),2)
  ENDFOR
ENDIF


*-- To collect the data and print the report
=lfFromA()


*!*************************************************************
*! Name      : lfFromA
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : To print the forms when sorting by Style Major & Style Group.
*!*************************************************************
*! Example     : = lfFromA()
*!*************************************************************
FUNCTION lfFromA

*-- Create a temporary cursor to hold the records that matched the
*-- selected criteria.
lcTmpCurs = gfTempName()
**C200137,1
CREATE TABLE(gcWorkDir+lcTmpCurs);
    (cGroup C(7),cGrpDesc C(30),cSeason C(6),cSize C(5),cType C(1),;
     cStyle C(12),cStyDesc C(20),cType2 C(1),cTransct C(10), cTranTyp C(1),;
     nMonth1 N(6), nMonth2 N(6), nMonth3 N(6), nMonth4 N(6), nMonth5 N(6),;
     nMonth6 N(6), nMonth7 N(6), nMonth8 N(6), nMonth9 N(6), nMonth10 N(6),;
     nMonth11 N(6), nMonth12 N(6),nTotal N(7), nPercent N(5,2),cSzOrd C(1))

*-- If show size breakdown = 'N'
IF lcRpSzBrkD = 'N'
  INDEX ON cGroup+cType+cStyle+cType2+cTranTyp TAG(lcTmpCurs)
ELSE
  INDEX ON cGroup+cType+cStyle+cTranTyp+cType2+cSzOrd  TAG(lcTmpCurs)
ENDIF

*-- To hold the total of all transactions for each group.
lcGrpTCur = gfTempName()
**C200137,1
CREATE TABLE(gcWorkDir+lcGrpTCur);
    (cGroup C(7),cGrpDesc C(30),cType C(1), cType2 C(1),cStyle C(12),;
     cTransct C(10), cTranTyp C(1),nMonth1 N(6), nMonth2 N(6), nMonth3 N(6),;
     nMonth4 N(6), nMonth5 N(6),nMonth6 N(6), nMonth7 N(6), nMonth8 N(6),;
     nMonth9 N(6), nMonth10 N(6),nMonth11 N(6), nMonth12 N(6),nTotal N(7),;
     nPercent N(5,2))
INDEX ON cGroup+cType+cStyle+cType2+cTranTyp TAG(lcGrpTCur)

*-- To check if there is any selected styles.
llEOFSty = .T.
*-- To hold the file of the selected style if any
lcStyFile = ''
lnPosition = IIF(lcRpSortBy = 'C',;
                  ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ALLTRIM(STYLE.CSTYMAJOR)'),1),;
                  ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1))
IF lnPosition > 0
  lcStyFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcStyFile) AND USED(lcStyFile)
    SELECT (lcStyFile)
    GOTO TOP
    llEOFSty = EOF()
  ENDIF  
ENDIF

*-- To check if there is any selected Accounts.
llEOFAcc = .T.
*-- To hold the file of the selected style if any
lcAccFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
IF lnPosition > 0
  lcAccFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcAccFile) AND USED(lcAccFile)
    SELECT (lcAccFile)
    GOTO TOP
    llEOFAcc = EOF()
  ENDIF  
ENDIF

**C200137,1
IF lcRpSortBy='R'
  =gfOpenFile(gcDataDir+'SALESREP','SALESREP','SH')
ENDIF

*-- Accounts
lcAccFltr = IIF(!llEOFAcc, 'SEEK(Account,lcAccFile)','.T.')

*-- To set the relations for each file.
=lfSetRelat()

*-- To set the SCAN expression.
=lfSetExpr()

*-- If the user selects styles from the Option Grid.
IF !llEOFSty
  =lfSelBySty()
ELSE
  =lfSelByAll()
ENDIF

SELECT (lcTmpCurs)
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF


IF lcRpSortBy $ 'AR'
  DELETE FOR cType2='B' OR nmonth1+nmonth2+nmonth3+nmonth4+nmonth5+nmonth6+nmonth7+nmonth8+nmonth9+nmonth10+nmonth11+nmonth12=0
  GO TOP
ENDIF


*--Calculate percentage.
SELECT (lcTmpCurs)
IF lcRpSortBy <> 'M'   && 'G' & 'F'
  SCAN FOR cType='A' AND cType2='A'
     WAIT WINDOW 'Calculating Percentages for Style :' + cStyle NOWAIT 
  
     IF SEEK(cGroup+'B'+SPACE(12)+'A'+cTranTyp,lcGrpTCur) AND &lcGrpTCur..nTotal<>0
         REPLACE nPercent WITH  ROUND((nTotal/&lcGrpTCur..nTotal)*100,2)
     ENDIF

  ENDSCAN

  USE IN (lcGrpTCur)
  SELECT (lcTmpCurs)
  APPEND FROM (gcWorkDir+lcGrpTCur)
ELSE
  IF lcRpSzBrkD = 'Y'
  
    SCAN FOR cType='A' AND cType2='A'
       WAIT WINDOW 'Calculating Percentages for Style/Size :' + cStyle+'/'+cSize NOWAIT 
       lnRecordNo = RECNO()  
       
       lnPrcSz = 0
       IF SEEK(cGroup+'A'+cStyle+cTranTyp+'B') AND nTotal<>0
           lnPrcSz = nTotal
       ENDIF

       IF BETWEEN(lnRecordNo,1,RECCOUNT())
         GOTO lnRecordNo
         IF lnPrcSz <> 0
           REPLACE nPercent WITH  ROUND((nTotal/lnPrcSz)*100,2)
         ENDIF  
       ENDIF
    ENDSCAN
  
  ENDIF
  USE IN (lcGrpTCur)
ENDIF
GOTO TOP

*TTTT
IF lcRpSortBy = 'M'  && If sort by style major then empty the group field.
  SELECT (lcTmpCurs)
  REPLACE ALL CGROUP WITH " "
  GO TOP
ENDIF

IF lcRpOExcel = 'N'

  = gfDispRe(lcRpStyFrm)

ELSE
  *-- To generate the excel file
  IF !EMPTY(lcRpExlFil)
    =lfGenExlFl()
  ENDIF  
ENDIF

IF USED(lcTmpCurs)
  SELECT (lcTmpCurs)
  USE
  ERASE(gcWorkDir+lcTmpCurs+'.DBF')
  ERASE(lcTmpCurs+'.CDX')
ENDIF
ERASE(gcWorkDir+lcGrpTCur+'.DBF')
ERASE(gcWorkDir+lcGrpTCur+'.CDX')

*!*************************************************************
*! Name      : lfUpdTmpAN
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04//2000 
*! Purpose   : The Update the cursor with the selected transactions.
*! Parameter : lcExpr  - > The scan expersion
*!             lcTran   ->  '1'-> Order '2'-> Invoice '3'-> Retuns
*!							'4'-> WIP   '5'-> Stock 
*!             lcTotQty -> OrdLine.TotQty,InvLine.TotQty,RetLine.TotQty
*!                         PosLine.TotQty,StyInvJl.TotQty
*!             lcTranName - > 'Orders','Invoiced','Returns','WIP','Stock'     
*!             ldTranDate - > 'Start','InvDate','CrDate'
*!*************************************************************
*! Example     : = lfUpdTmpAN()
*!*************************************************************
FUNCTION lfUpdTmpAN
PARAMETER lcExpr,lcTran,lcTotQty,lcTranName,ldChkDate

SCAN &lcExpr
  WAIT WINDOW lcTranName+': Style - Color ' + Style NOWAIT
  **C200137,1
  lcGrpDesc = ''
  lcGroup = lfGetGrp(.T.)
  
 
  lcStyle   = SUBSTR(Style,1,12)

  IF !SEEK(lcGroup+'A'+lcStyle+'A'+lcTran,lcTmpCurs)    
    INSERT INTO (lcTmpCurs);
    (cGroup,cGrpDesc,cSeason,cType,cStyle,cStyDesc,cType2,cTransct,cTranTyp);
    VALUES;
    (lcGroup,lcGrpDesc,Style.Season,'A',lcStyle,Style.Desc,'A',lcTranName,;
                                                                    lcTran)
  ENDIF

  lcMonth = ALLTRIM(STR(MONTH(&ldChkDate)))  

  *-- Total transactions for each group if any was selected in the O.G.
  =lfTotGrp(lcGroup,lcGrpDesc,lcTran,lcTranName)
  
  IF lcTran <> '4'
    REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
            &lcTmpCurs..nMonth&lcMonth + &lcTotQty
    m.nMonth&lcMonth = m.nMonth&lcMonth + &lcTotQty
  ELSE
    IF TranCd $ '16'
      REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
              &lcTmpCurs..nMonth&lcMonth  + &lcTotQty
      m.nMonth&lcMonth = m.nMonth&lcMonth + &lcTotQty
    ELSE
      IF IIF(lcRpSortBy = 'C',TranCd $ '234',TranCd $ '245')
        REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
                &lcTmpCurs..nMonth&lcMonth - &lcTotQty
        m.nMonth&lcMonth = m.nMonth&lcMonth - &lcTotQty
      ENDIF  
    ENDIF  
  ENDIF
  REPLACE &lcTmpCurs..nTotal WITH &lcTmpCurs..nMonth1+&lcTmpCurs..nMonth2+;
          &lcTmpCurs..nMonth3+&lcTmpCurs..nMonth4+&lcTmpCurs..nMonth5+;
          &lcTmpCurs..nMonth6+&lcTmpCurs..nMonth7+&lcTmpCurs..nMonth8+;
          &lcTmpCurs..nMonth9+&lcTmpCurs..nMonth10+&lcTmpCurs..nMonth11+;
          &lcTmpCurs..nMonth12
  m.nTotal = m.nMonth1+m.nMonth2+m.nMonth3+m.nMonth4+m.nMonth5+m.nMonth6+;
             m.nMonth7+m.nMonth8+m.nMonth9+m.nMonth10+m.nMonth11+m.nMonth12        
ENDSCAN
WAIT CLEAR

*!*************************************************************
*! Name      : lfRepQtyN
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : To calculate the totals for each style
*!*************************************************************
*! Example     : = lfRepQtyN()
*!*************************************************************
FUNCTION lfRepQtyN
PRIVATE lnAlias

lnAlias = SELECT()

SELECT (lcTmpCurs)

**C200137,1
lcSekGrop = lfGetGrp()


IF !SEEK(lcSekGrop+'A'+lcStyMaj+'B')
  INSERT INTO (lcTmpCurs);
              (cGroup,cType,cStyle,cType2);
       VALUES (lcSekGrop,'A',lcStyMaj,'B')
ENDIF


FOR lnMonthCnt = 1 TO 12
  lcMonthCnt = ALLTRIM(STR(lnMonthCnt))
  REPLACE &lcTmpCurs..nMonth&lcMonthCnt WITH &lcTmpCurs..nMonth&lcMonthCnt+;
          m.nMonth&lcMonthCnt
ENDFOR
REPLACE &lcTmpCurs..nTotal WITH &lcTmpCurs..nMonth1+&lcTmpCurs..nMonth2+;
        &lcTmpCurs..nMonth3+&lcTmpCurs..nMonth4+&lcTmpCurs..nMonth5+;
        &lcTmpCurs..nMonth6+&lcTmpCurs..nMonth7+&lcTmpCurs..nMonth8+;
        &lcTmpCurs..nMonth9+&lcTmpCurs..nMonth10+&lcTmpCurs..nMonth11+;
        &lcTmpCurs..nMonth12
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdTmpAY
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04//2000 
*! Purpose   : The Update the cursor with the selected transactions.
*! Parameter : lcExpr  - > The scan expersion
*!             lcTran   ->  '1'-> Order '2'-> Invoice '3'-> Retuns
*!							'4'-> WIP   '5'-> Stock 
*!             lcTotQty -> OrdLine.TotQty,InvLine.TotQty,RetLine.TotQty
*!                         PosLine.TotQty,StyInvJl.TotQty
*!             lcTranName - > 'Orders','Invoiced','Returns','WIP','Stock'     
*!             ldTranDate - > 'Start','InvDate','CrDate'
*!*************************************************************
*! Example     : = lfUpdTmpAY()
*!*************************************************************
FUNCTION lfUpdTmpAY
PARAMETER lcExpr,lcTran,lcTotQty,lcTranName,ldChkDate

SCAN &lcExpr
  WAIT WINDOW lcTranName+': Style - Color ' + Style NOWAIT

  **C200137,1
  lcGrpDesc = ''
  lcGroup = lfGetGrp(.T.)


  lcStyle   = SUBSTR(Style,1,12)
  =SEEK('S'+Style.Scale,'Scale')
  
  *-- Total transactions for each group if any was selected in the O.G.
  lcMonth = ALLTRIM(STR(MONTH(&ldChkDate)))  
  
  =lfTotGrp(lcGroup,lcGrpDesc,lcTran,lcTranName)

  FOR lnScalSz = 1 TO Scale.Cnt
    lcScalSz = STR(lnScalSz,1)    

    IF IIF(lcTran = '5',nStk&lcScalSz,Qty&lcScalSz) <> 0
      lcSize   = Scale.Sz&lcScalSz
               
      IF !SEEK(lcGroup+'A'+lcStyle+lcTran+'A'+lcScalSz,lcTmpCurs)
        INSERT INTO (lcTmpCurs);
        (cGroup,cGrpDesc,cSize,cType,cStyle,cStyDesc,cType2,cTransct,;
         cTranTyp,cSzOrd);
         VALUES;
        (lcGroup,lcGrpDesc,lcSize,'A',lcStyle,Style.Desc,'A',;
         lcTranName,lcTran,lcScalSz)
      ENDIF      
      
      IF lcTran <> '4'
        REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
                &lcTmpCurs..nMonth&lcMonth + IIF(lcTran = '5',nStk&lcScalSz,Qty&lcScalSz)
        m.nMonth&lcMonth = m.nMonth&lcMonth + IIF(lcTran = '5',nStk&lcScalSz,Qty&lcScalSz)
      ELSE
        IF TranCd $ '16'
          REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
                  &lcTmpCurs..nMonth&lcMonth  + Qty&lcScalSz
          m.nMonth&lcMonth = m.nMonth&lcMonth + Qty&lcScalSz
        ELSE
          IF IIF(lcRpSortBy = 'C',TranCd $ '234',TranCd $ '245')
            REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
                    &lcTmpCurs..nMonth&lcMonth - Qty&lcScalSz
            m.nMonth&lcMonth = m.nMonth&lcMonth - Qty&lcScalSz
          ENDIF
        ENDIF
      ENDIF
      REPLACE &lcTmpCurs..nTotal WITH &lcTmpCurs..nMonth1+&lcTmpCurs..nMonth2+;
              &lcTmpCurs..nMonth3+&lcTmpCurs..nMonth4+&lcTmpCurs..nMonth5+;
              &lcTmpCurs..nMonth6+&lcTmpCurs..nMonth7+&lcTmpCurs..nMonth8+;
              &lcTmpCurs..nMonth9+&lcTmpCurs..nMonth10+&lcTmpCurs..nMonth11+;
              &lcTmpCurs..nMonth12
      m.nTotal = m.nMonth1+m.nMonth2+m.nMonth3+m.nMonth4+m.nMonth5+m.nMonth6+;
                 m.nMonth7+m.nMonth8+m.nMonth9+m.nMonth10+m.nMonth11+m.nMonth12
    ENDIF
  ENDFOR  
ENDSCAN
WAIT CLEAR

*!*************************************************************
*! Name      : lfRepQtyY
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : To calculate the totals for each transaction
*!*************************************************************
*! Example     : = lfRepQtyY()
*!*************************************************************
FUNCTION lfRepQtyY
PARAMETERS lcTran,lcTranName
PRIVATE lnAlias

lnAlias = SELECT()

SELECT (lcTmpCurs)

**C200137,1
lcSekGrop = lfGetGrp()


IF !SEEK(lcSekGrop+'A'+lcStyMaj+lcTran+'B'+'9')

  INSERT INTO (lcTmpCurs);
              (cGroup,cType,cStyle,cType2,cTransct,cTranTyp,cSzOrd);
       VALUES (lcSekGrop,'A',lcStyMaj,'B',lcTranName,lcTran,'9')
ENDIF

FOR lnMonthCnt = 1 TO 12
  lcMonthCnt = ALLTRIM(STR(lnMonthCnt))
  REPLACE &lcTmpCurs..nMonth&lcMonthCnt WITH &lcTmpCurs..nMonth&lcMonthCnt+;
          m.nMonth&lcMonthCnt
ENDFOR
REPLACE &lcTmpCurs..nTotal WITH &lcTmpCurs..nMonth1+&lcTmpCurs..nMonth2+;
        &lcTmpCurs..nMonth3+&lcTmpCurs..nMonth4+&lcTmpCurs..nMonth5+;
        &lcTmpCurs..nMonth6+&lcTmpCurs..nMonth7+&lcTmpCurs..nMonth8+;
        &lcTmpCurs..nMonth9+&lcTmpCurs..nMonth10+&lcTmpCurs..nMonth11+;
        &lcTmpCurs..nMonth12
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfTotGrp
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : To calculate the totals for each group if any was selected
*!             in the option grid
*!*************************************************************
*! Parameters: lcGrp : Group
*!             lcGrpDsc: Group Descriptin
*!             lcTranNo: '1'-> Order '2'-> Invoice '3'-> Retuns
*!					     '4'-> WIP   '5'-> Stock 
*!             lcTranNm: 'Orders','Invoiced',Returns','WIP','Stock'
*!*************************************************************
*! Example     : = lfTotGrp()
*!*************************************************************
FUNCTION lfTotGrp
PARAMETERS lcGrp,lcGrpDsc,lcTranNo,lcTranNm

IF !SEEK(lcGrp+'B'+SPACE(12)+'A'+lcTranNo,lcGrpTCur)
  INSERT INTO (lcGrpTCur);
      (cGroup, cGrpDesc, cType,cStyle,cType2, cTransct, cTranTyp) VALUES;
      (lcGrp,lcGrpDsc,'B',SPACE(12),'A',lcTranNm,lcTranNo)
ENDIF

IF lcTran <> '4'
  REPLACE &lcGrpTCur..nMonth&lcMonth WITH;
          &lcGrpTCur..nMonth&lcMonth + &lcTotQty
ELSE
  IF TranCd $ '16'
    REPLACE &lcGrpTCur..nMonth&lcMonth WITH;
            &lcGrpTCur..nMonth&lcMonth +&lcTotQty
  ELSE
    IF IIF(lcRpSortBy = 'C',TranCd $ '234',TranCd $ '245')
      REPLACE &lcGrpTCur..nMonth&lcMonth WITH;
              &lcGrpTCur..nMonth&lcMonth - &lcTotQty
    ENDIF  
  ENDIF  
ENDIF
REPLACE &lcGrpTCur..nTotal WITH &lcGrpTCur..nMonth1+&lcGrpTCur..nMonth2+;
        &lcGrpTCur..nMonth3+&lcGrpTCur..nMonth4+&lcGrpTCur..nMonth5+;
        &lcGrpTCur..nMonth6+&lcGrpTCur..nMonth7+&lcGrpTCur..nMonth8+;
        &lcGrpTCur..nMonth9+&lcGrpTCur..nMonth10+&lcGrpTCur..nMonth11+;
        &lcGrpTCur..nMonth12

*!*************************************************************
*! Name      : lfGetPrcnt
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : To get the total of transaction for each style and 
*!             group. Used in the FRX.
*!*************************************************************
*! Example     : = lfGetPrcnt()
*!*************************************************************
FUNCTION lfGetPrcnt
PARAMETERS lcSGroup,lcType1,lcStyleCd,lcType2,lcTranCd
*PRIVATE lnRecordNo

**C200137,1
RETURN



lnRecordNo = RECNO()
IF lcRpSzBrkD = 'Y'
  DO CASE
    CASE lcType1 = 'A' AND lcType2 = 'A'    
      =SEEK(lcSGroup+'A'+lcStyleCd+lcTranCd+'B')           
    CASE (lcType1 = 'A' AND lcType2 = 'B')
      =SEEK(lcSGroup+'B'+SPACE(12)+lcTranCd+'A')
  ENDCASE
ELSE
*  DO CASE
*    CASE lcType1 = 'A' AND lcType2 = 'A'    
*      =SEEK(lcSGroup+'B'+SPACE(12)+'A'+lcTranCd)
*      IF EMPTY(lcGroups)
*        =SEEK(lcSGroup+'A'+lcStyleCd+'B')
*      ELSE
*        =SEEK(lcSGroup+'B'+SPACE(12)+'A'+lcTranCd)
*      ENDIF  
*  ENDCASE
ENDIF

lnPercent = nTotal

IF BETWEEN(lnRecordNo,1,RECCOUNT())
  GOTO lnRecordNo
ENDIF
RETURN ''

*!*************************************************************
*! Name      : lfWhenRep
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : The validation function of the Option Grid.
*!*************************************************************
*! Example     : = lfWhenRep()
*!*************************************************************
FUNCTION lfWhenRep
R_WIDTH = 'W'
IF lnOGSeting = 1
  DIMENSION laRpSource[IIF(lcRpSortBy$'AR',3,5),1],laRpTarget[1,1],laRpLSourc[1],laRpLTargt[1,1]
  STORE '' TO laRpSource,laRpTarget,laRpLSourc,laRpLTargt,;
              laRpCSourc,laRpCTargt

  *-- Filling the source array for all transactions.
  STORE 'Orders'   TO laRpSource[1]
  STORE 'Invoiced' TO laRpSource[2]
  STORE 'Returns'  TO laRpSource[3]
  IF ! ( lcRpSortBy $ 'AR')
    STORE 'WIP'      TO laRpSource[4]
    STORE 'Stock'    TO laRpSource[5]
  ENDIF
  *-- Filling the source array for all locations.
  SELECT WareHous
  SELECT DISTINCT cWareCode FROM WareHous INTO ARRAY laRpLSourc
ELSE
  IF lcRpOExcel = 'Y'
    SHOW GET pbOGPrevie DISABLED
  ELSE
    SHOW GET pbOGPrevie ENABLED
  ENDIF  
ENDIF


*!*************************************************************
*! Name      : lfvDate
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/25/2000 
*! Purpose   : The validation the date range
*!*************************************************************
*! Example     : = lfvDate()
*!*************************************************************
FUNCTION lfvDate
PRIVATE ldFrom,ldTo

STORE {} TO ldFrom,ldTo
IF !EMPTY(ldRpTDt)
  ldFrom = ldRpFDt
  ldTo   = ldRpTDt
ENDIF
lcTitle = 'Dates'
DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 


*!*************************************************************
*! Name      : lfvpbOk
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : The validation of OK button
*!*************************************************************
*! Example     : = lfvpbOk()
*!*************************************************************
FUNCTION lfvpbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  ldRpFDt = ldFrom
  ldRpTDt = ldTo
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : To validate the sort by option. OG
*!*************************************************************
*! Example     : = lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy

*=lfRepPltFr(lcRpStyFrm)
CLEAR READ

*!*************************************************************
*! Name      : lfvTrans
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : Validate the selection of transactions.
*!*************************************************************
*! Example     : = lfvTrans()
*!*************************************************************
FUNCTION lfvTrans

=gfMover(@laRpSource,@laRpTarget,'Transactions',.T.,'')

*!*************************************************************
*! Name      : lfvLoc
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : Validate the selection of Locations
*!*************************************************************
*! Example     : = lfvLoc()
*!*************************************************************
FUNCTION lfvLoc

= gfMover(@laRpLSourc,@laRpLTargt,'Inventory Location',.T.,'')

*!*************************************************************
*! Name      : lfvSizeYN
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : To specify the Printed from.
*!*************************************************************
*! Example     : = lfvSizeYN()
*!*************************************************************
FUNCTION lfvSizeYN

lcRpStyFrm = IIF(lcRpSzBrkD = 'N','ICSTYMTA','ICSTYMTB')

=lfRepPltFr(lcRpStyFrm)

CLEAR READ

*!*************************************************************
*! Name      : lfOutToExl
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : To validated the Excel OutPut file.
*!*************************************************************
*! Example     : = lfOutToExl()
*!*************************************************************
FUNCTION lfOutToExl
IF lcRpOExcel = 'Y'
  SHOW GET pbOGPrevie DISABLED
  lcRpExlFil =PUTFILE('Excel File:',lcRpExlFil, 'XLS')
ELSE
  SHOW GET pbOGPrevie ENABLED
ENDIF  

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : To set and rest the Fabric file.
*!*************************************************************
*! Example     : = lfSRVFab()
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/24/2000 
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Example     : = lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
IF RECCOUNT() != 0
   lnFabRec = RECNO('FABRIC')
   SELECT Fabric_X
   SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
   SELECT Fabric
   IF BETWEEN(lnFabRec,1,RECCOUNT())
      GO lnFabRec
   ENDIF
ENDIF
RETURN INT(lnTotcomp)

*!*************************************************************
*! Name      : lfSetRelat
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/25/2000 
*! Purpose   : To set the index and relations.
*!*************************************************************
*! Example     : = lfSetRelat()
*!*************************************************************
FUNCTION lfSetRelat

*-- Setting the order of the style to style major
SELECT Style
SET ORDER TO TAG cStyle

*-- Order Transaction
SELECT OrdHdr
SET ORDER TO TAG OrdHdr
SELECT OrdLine
SET RELATION TO
SET ORDER TO TAG OrdLineS
SET RELATION TO 'O'+Order INTO OrdHdr ADDITIVE

*-- Invoice Transaction
SELECT InvHdr
SET ORDER TO TAG InvHdr
SELECT InvLine      
SET ORDER TO TAG InvLineS
SET RELATION TO
SET RELATION TO Invoice INTO InvHdr ADDITIVE

*-- Return Transaction
SELECT RetHdr
SET ORDER TO RetHdr
SELECT RetLine
SET ORDER TO RetLineS
SET RELATION TO
SET RELATION TO CrMemo INTO RetHdr ADDITIVE

*-- C/T WIP Transaction
SELECT CutTktL
SET ORDER TO TAG CutTktLS
SET RELATION TO
SET RELATION TO CutTkt INTO CutTktH ADDITIVE

*-- Dye Orders (WIP Transaction)
SELECT PosLn
SET ORDER TO TAG PosLnS
SET RELATION TO
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE

*!*************************************************************
*! Name      : lfSetExpr
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/25/2000 
*! Purpose   : To set the SCAN expression for each transaction
*!*************************************************************
*! Example     : = lfSetExpr()
*!*************************************************************
FUNCTION lfSetExpr

lcOrdFilt = 'REST WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6)='+;
            'lcStyMaj'+;
            ' FOR BETWEEN(Start,ldFromDate,ldToDate)'+;
            ' AND cWareCode $ lcAllLoc .AND. &lcFilter .AND. &lcAccFltr'

lcInvFilt = 'REST WHILE Style+Invoice+STR(LineNo,6) = lcStyMaj'+;
            ' FOR BETWEEN(InvDate,ldFromDate,ldToDate)'+;
            ' AND InvHdr.Status = "C"'+;
            ' AND InvHdr.cWareCode $ lcAllLoc .AND. &lcFilter'+;
            ' .AND. &lcAccFltr'

lcRetFilt = 'REST WHILE Style+CrMemo= lcStyMaj'+;
            ' FOR BETWEEN(CrDate,ldFromDate,ldToDate)'+;
            ' AND RetHdr.cWareCode $ lcAllLoc'+;
            ' .AND. &lcFilter .AND. &lcAccFltr'

IF lcRpSortBy = 'C'

  lcWipFilt = 'REST WHILE Style+CutTkt+TranCd = lcStyMaj'+;
              ' FOR cWareCode $ lcAllLoc'+;
              IIF(!EMPTY(lcCTStatus),' AND CutTktH.Status $ lcCTStatus','')+;
              ' .AND. &lcFilter'
ELSE
ldWipFDate = CTOD(PADL(MONTH(gdSYSDate),2,'0')+'/01/'+STR(YEAR(gdSysDate),4))
ldWipTDate = gdSysDate

lcWipFilt = 'REST WHILE Style+cStyType+Po+STR(LineNo,6)+TranCd=lcStyMaj'+;
            ' FOR BETWEEN(PosHdr.Entered,ldWipFDate,ldWipTDate)'+;
            ' AND cStyType="D"'+;
            ' AND PosHdr.Status $ "OH"'+;
            ' AND cWareCode $ lcAllLoc'+;
            ' .AND. &lcFilter'
ENDIF

ldStkFDate = CTOD(PADL(MONTH(gdSYSDate),2,'0')+'/01/'+STR(YEAR(gdSysDate),4))
ldStkTDate = gdSysDate

lcStckFilt = 'REST WHILE Style+cWareCode+cSession+DTOS(DtrDate)+cTrCode+STR(LineNo,6)='+;
             ' lcStyMaj FOR BETWEEN(DtrDate,ldStkFDate,ldStkTDate) AND !EMPTY(DtrDate)'+;
             ' AND nTotStk <> 0 AND cWareCode $ lcAllLoc .AND. &lcFilter'

*!*************************************************************
*! Name      : lfSelBySty
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/25/2000 
*! Purpose   : To select the records for each transaction if the
*!              user selects styles from the option grid.
*!*************************************************************
*! Example     : = lfSelBySty()
*!*************************************************************
FUNCTION lfSelBySty

SELECT (lcStyFile)
SCAN
  =SEEK(cStyMajor,'Style')
  lcStyMaj = SUBSTR(&lcStyFile..cStyMajor,1,12)
  FOR lnTranCnt = 1 TO ALEN(laTrans,1)
    DO CASE
      CASE laTrans[1,lnTranCnt] = 'Orders'
        IF SEEK(lcStyMaj,'OrdLine')
          SELECT OrdLine
          STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                     m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                     m.nMonth11,m.nMonth12,m.nTotal
          *-- Calling the function according to Show Size breakDown 'Y' or 'N'
          =lfUpdTmpA&lcRpSzBrkD(lcOrdFilt,'1','OrdLine.TotQty','Ordered','OrdLine.Start')
            
          *-- Replace total quantities for each style.
          IF m.nTotal <> 0
            =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('1','Ordered'))
          ENDIF
        ENDIF
      CASE laTrans[1,lnTranCnt] = 'Invoiced'   
        IF SEEK(lcStyMaj,'InvLine')
          SELECT InvLine
          STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                     m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                     m.nMonth11,m.nMonth12,m.nTotal
            
          *-- Calling the function according to Show Size breakDown 'Y' or 'N'
          =lfUpdTmpA&lcRpSzBrkD(lcInvFilt,'2','InvLine.TotQty','Invoiced','InvLine.InvDate')
            
          *-- Replace total quantities for each style.
          IF m.nTotal <> 0
            =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('2','Invoiced'))
          ENDIF            
        ENDIF
      CASE laTrans[1,lnTranCnt] = 'Returns'
        IF SEEK(lcStyMaj,'RetLine')
          SELECT RetLine
          STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                     m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                     m.nMonth11,m.nMonth12,m.nTotal

          *-- Calling the function according to Show Size breakDown 'Y' or 'N'
          =lfUpdTmpA&lcRpSzBrkD(lcRetFilt,'3','RetLine.TotQty','Returned','RetLine.CrDate')

          *-- Replace total quantities for each style.
          IF m.nTotal <> 0
            =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('3','Returned'))
          ENDIF            
        ENDIF
      CASE laTrans[1,lnTranCnt] = 'WIP'
        IF lcRpSortBy = 'C'
          IF SEEK(lcStyMaj,'CutTktL')
            SELECT CutTktL
            STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                       m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                       m.nMonth11,m.nMonth12,m.nTotal
  
            *-- Calling the function according to Show Size breakDown 'Y' or 'N'
            =lfUpdTmpA&lcRpSzBrkD(lcWipFilt,'4','CutTktL.TotQty','WIP','CutTktH.Entered')
 
            *-- Replace total quantities for each style.
            IF m.nTotal <> 0
              =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('4','WIP'))
            ENDIF               
          ENDIF
        ELSE  
          IF SEEK(lcStyMaj,'PosLn')
            SELECT PosLn
            STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                       m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                       m.nMonth11,m.nMonth12,m.nTotal
  
            *-- Calling the function according to Show Size breakDown 'Y' or 'N'
            =lfUpdTmpA&lcRpSzBrkD(lcWipFilt,'4','PosLn.TotQty','WIP','PosHdr.Entered')
 
            *-- Replace total quantities for each style.
            IF m.nTotal <> 0
              =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('4','WIP'))
            ENDIF               
          ENDIF
        ENDIF  
      CASE laTrans[1,lnTranCnt] = 'Stock'
        IF SEEK(lcStyMaj,'StyInvJl')
          SELECT StyInvJl
          STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                     m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                     m.nMonth11,m.nMonth12,m.nTotal

          *-- Calling the function according to Show Size breakDown 'Y' or 'N'
          =lfUpdTmpA&lcRpSzBrkD(lcStckFilt,'5','StyInvJl.nTotStk','Stock','StyInvJl.DtrDate')

          *-- Replace total quantities for each style.
          =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('5','Stock'))
        ENDIF      
    ENDCASE
  ENDFOR
ENDSCAN

*!*************************************************************
*! Name      : lfSelByAll
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/25/2000 
*! Purpose   : If the user select by all
*!*************************************************************
*! Example     : = lfSelByAll()
*!*************************************************************
FUNCTION lfSelByAll
  
FOR lnTranCnt = 1 TO ALEN(laTrans,1)
  DO CASE
    CASE laTrans[1,lnTranCnt] = 'Orders'
      SELECT OrdLine
      GOTO TOP
      DO WHILE !EOF()          
        lcStyMaj = SUBSTR(Style,1,12)
        =SEEK(lcStyMaj,'Style')
        STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                   m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                   m.nMonth11,m.nMonth12,m.nTotal
          
        *-- Calling the function according to Show Size breakDown 'Y' or 'N'
        =lfUpdTmpA&lcRpSzBrkD(lcOrdFilt,'1','OrdLine.TotQty','Ordered','OrdLine.Start')

        *-- Replace total quantities for each style.
        IF m.nTotal <> 0  
          =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('1','Ordered'))
        ENDIF          
      ENDDO                
    CASE laTrans[1,lnTranCnt] = 'Invoiced'
      SELECT InvLine
      GOTO TOP
      DO WHILE !EOF()
        lcStyMaj = SUBSTR(Style,1,12)
        =SEEK(lcStyMaj,'Style')
        STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                   m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                   m.nMonth11,m.nMonth12,m.nTotal

        *-- Calling the function according to Show Size breakDown 'Y' or 'N'
        =lfUpdTmpA&lcRpSzBrkD(lcInvFilt,'2','InvLine.TotQty','Invoiced','InvLine.InvDate')

        *-- Replace total quantities for each style.
        IF m.nTotal <> 0
          =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('2','Invoiced'))
        ENDIF          
      ENDDO
    CASE laTrans[1,lnTranCnt] = 'Returns'
      SELECT RetLine
      GOTO TOP
        DO WHILE !EOF()
        lcStyMaj = SUBSTR(Style,1,12)
        =SEEK(lcStyMaj,'Style')
        STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                   m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                   m.nMonth11,m.nMonth12,m.nTotal
        SELECT RetLine

        *-- Calling the function according to Show Size breakDown 'Y' or 'N'
        =lfUpdTmpA&lcRpSzBrkD(lcRetFilt,'3','RetLine.TotQty','Returned','RetLine.CrDate')

        *-- Replace total quantities for each style.
        IF m.nTotal <> 0
          =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('3','Returned'))
        ENDIF
           
      ENDDO
     CASE laTrans[1,lnTranCnt] = 'WIP'
      IF lcRpSortBy = 'C'
        SELECT CutTktL
        GOTO TOP
        DO WHILE !EOF()
         lcStyMaj = SUBSTR(Style,1,12)
          =SEEK(lcStyMaj,'Style')
          STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                     m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                     m.nMonth11,m.nMonth12,m.nTotal
          SELECT CutTktL
          *-- Calling the function according to Show Size breakDown 'Y' or 'N'
          =lfUpdTmpA&lcRpSzBrkD(lcWipFilt,'4','CutTktL.TotQty','WIP','CutTktH.Entered')

          *-- Replace total quantities for each style.
          IF m.nTotal <> 0
            =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('4','WIP'))
          ENDIF            
        ENDDO
      ELSE
        SELECT PosLn
        GOTO TOP
        DO WHILE !EOF()
          lcStyMaj = SUBSTR(Style,1,12)
          =SEEK(lcStyMaj,'Style')
          STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                     m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                     m.nMonth11,m.nMonth12,m.nTotal
          SELECT PosLn
          *-- Calling the function according to Show Size breakDown 'Y' or 'N'
          =lfUpdTmpA&lcRpSzBrkD(lcWipFilt,'4','PosLn.TotQty','WIP','PosHdr.Entered')

          *-- Replace total quantities for each style.
          =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('4','WIP'))
        ENDDO
      ENDIF
    CASE laTrans[1,lnTranCnt] = 'Stock'
      SELECT StyInvJl
      GOTO TOP
      DO WHILE !EOF()
        lcStyMaj = SUBSTR(Style,1,12)
        =SEEK(lcStyMaj,'Style')
        STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                   m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                   m.nMonth11,m.nMonth12,m.nTotal
        SELECT StyInvJl
        *-- Calling the function according to Show Size breakDown 'Y' or 'N'
        =lfUpdTmpA&lcRpSzBrkD(lcStckFilt,'5','StyInvJl.nTotStk','Stock','StyInvJl.DtrDate')

        *-- Replace total quantities for each style.
        IF m.nTotal <> 0
          =IIF(lcRpSzBrkD='N',lfRepQtyN(),lfRepQtyY('5','Stock'))
        ENDIF
      ENDDO
  ENDCASE
ENDFOR

*!*************************************************************
*! Name      : lfGenExlFl
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/25/2000 
*! Purpose   : To generate a generic Excel file.
*!*************************************************************
*! Example     : = lfGenExlFl()
*!*************************************************************
FUNCTION lfGenExlFl

*-- The file that will be export to the Excel file
lcExclCur = gfTempName()

*-- The fileds of the temporary file
lcYear = RIGHT(STR(YEAR(gdSysDate),4),2)
**C200137,1
lcJan  = 'JAN_'+lcYear1
lcFeb  = 'FEB_'+lcYear2
lcMar  = 'MAR_'+lcYear3
lcApr  = 'APR_'+lcYear4
lcMay  = 'MAY_'+lcYear5 
lcJun  = 'JUN_'+lcYear6
lcJul  = 'JUL_'+lcYear7
lcAug  = 'AUG_'+lcYear8
lcSept = 'SEP_'+lcYear9
lcOct  = 'Oct_'+lcYear10
lcNov  = 'NOV_'+lcYear11
lcDec  = 'DEC_'+lcYear12

*-- If the show size break down is 'Y'
IF lcRpSzBrkD = 'Y'
  CREATE CURSOR (lcExclCur);
         (Style C(50), Transact C(15), Size C(5),&lcJan C(8),;
          &lcFeb C(8), &lcMar C(8), &lcApr C(8), &lcMay C(8),;
          &lcJun C(8), &lcJul C(8), &lcAug C(8), &lcSept C(8),;
          &lcOct C(8), &lcNov C(8), &lcDec C(8), nTotal C(9),;
          Overall C(6))
*-- If the show size break down is 'N'
ELSE
  CREATE CURSOR (lcExclCur);
         (Style C(50), Transact C(15), &lcJan C(8), &lcFeb C(8),&lcMar C(8),;
          &lcApr C(8), &lcMay C(8), &lcJun C(8), &lcJul C(8), &lcAug C(8),;
          &lcSept C(8), &lcOct C(8), &lcNov C(8), &lcDec C(8), nTotal C(9),;
          Overall C(6))
ENDIF

lcTmpFile = gfTempName()
*-- Use the same file in another alias
USE (gcWorkDir+lcTmpCurs) IN 0 AGAIN ALIAS (lcTmpFile)  ORDER (lcTmpCurs)


SELECT (lcTmpCurs)
GOTO TOP

lcStyle   = ' '
lcTranCod = ' '
DO WHILE !EOF()
  lcGroupCd = cGroup
  llGrpTot  = .T.
  INSERT INTO (lcExclCur) (Style) VALUES ('Style Group: '+ALLTRIM(&lcTmpCurs..cGrpDesc))
  
  SCAN REST WHILE cGroup = lcGroupCd
    WAIT WINDOW cTransct +': Style - Color ' + cStyle NOWAIT
    IF cStyle <> lcStyle 
      IF !EMPTY(cStyle)
        INSERT INTO (lcExclCur) (Style) ;
               VALUES (&lcTmpCurs..cStyle+' - '+ALLTRIM(&lcTmpCurs..cStyDesc))
      ENDIF         
      lcStyle = &lcTmpCurs..cStyle
      lcTranCod = ' '      
    ENDIF
    IF lcRpSzBrkD = 'Y'
      DO CASE
        CASE cType = 'A' AND cType2 = 'A'    
          =SEEK(cGroup+'A'+cStyle+cTranTyp+'B',lcTmpFile)
        INSERT INTO (lcExclCur);
          (Transact,Size,&lcJan,&lcFeb,&lcMar,&lcApr,&lcMay,&lcJun,&lcJul,;
           &lcAug,&lcSept,&lcOct,&lcNov,&lcDec,nTotal,Overall);
         VALUES;
           (IIF(&lcTmpCurs..cTransct<>lcTranCod,&lcTmpCurs..cTransct,''),;
            &lcTmpCurs..cSize,STR(&lcTmpCurs..nMonth1,8),;
            STR(&lcTmpCurs..nMonth2,8),STR(&lcTmpCurs..nMonth3,8),;
            STR(&lcTmpCurs..nMonth4,8),STR(&lcTmpCurs..nMonth5,8),;
            STR(&lcTmpCurs..nMonth6,8),STR(&lcTmpCurs..nMonth7,8),;
            STR(&lcTmpCurs..nMonth8,8),STR(&lcTmpCurs..nMonth9,8),;
            STR(&lcTmpCurs..nMonth10,8),STR(&lcTmpCurs..nMonth11,8),;
            STR(&lcTmpCurs..nMonth12,8),STR(&lcTmpCurs..nTotal,9),;
            STR(ROUND((&lcTmpCurs..nTotal/&lcTmpFile..nTotal)*100,2),5,2)+'%')
            lcTranCod = &lcTmpCurs..cTransct
        CASE (cType = 'A' AND cType2 = 'B')
          =SEEK(cGroup+'B'+SPACE(12)+cTranTyp+'A',lcTmpFile)
          INSERT INTO (lcExclCur);
          (Transact,Size,&lcJan,&lcFeb,&lcMar,&lcApr,&lcMay,&lcJun,&lcJul,;
           &lcAug,&lcSept,&lcOct,&lcNov,&lcDec,nTotal,Overall);
          VALUES;
            ('Total '+&lcTmpCurs..cTransct,&lcTmpCurs..cSize,STR(&lcTmpCurs..nMonth1,8),;
            STR(&lcTmpCurs..nMonth2,8),STR(&lcTmpCurs..nMonth3,8),;
            STR(&lcTmpCurs..nMonth4,8),STR(&lcTmpCurs..nMonth5,8),;
            STR(&lcTmpCurs..nMonth6,8),STR(&lcTmpCurs..nMonth7,8),;
            STR(&lcTmpCurs..nMonth8,8),STR(&lcTmpCurs..nMonth9,8),;
            STR(&lcTmpCurs..nMonth10,8),STR(&lcTmpCurs..nMonth11,8),;
            STR(&lcTmpCurs..nMonth12,8),STR(&lcTmpCurs..nTotal,9),;
            STR(ROUND((&lcTmpCurs..nTotal/&lcTmpFile..nTotal)*100,2),5,2)+'%')
        CASE (cType = 'B' AND cType2 = 'A')
          IF llGrpTot
            INSERT INTO (lcExclCur) (Style) ;
                   VALUES ('Grand Total: '+ALLTRIM(&lcTmpCurs..cGrpDesc)+' Style Group')
            llGrpTot = .F.
          ENDIF         
          INSERT INTO (lcExclCur);
            (Transact,Size,&lcJan,&lcFeb,&lcMar,&lcApr,&lcMay,&lcJun,&lcJul,;
             &lcAug,&lcSept,&lcOct,&lcNov,&lcDec,nTotal);
             VALUES;
            (&lcTmpCurs..cTransct,&lcTmpCurs..cSize,STR(&lcTmpCurs..nMonth1,8),;
             STR(&lcTmpCurs..nMonth2,8),STR(&lcTmpCurs..nMonth3,8),;
             STR(&lcTmpCurs..nMonth4,8),STR(&lcTmpCurs..nMonth5,8),;
             STR(&lcTmpCurs..nMonth6,8),STR(&lcTmpCurs..nMonth7,8),;
             STR(&lcTmpCurs..nMonth8,8),STR(&lcTmpCurs..nMonth9,8),;
             STR(&lcTmpCurs..nMonth10,8),STR(&lcTmpCurs..nMonth11,8),;
             STR(&lcTmpCurs..nMonth12,8),STR(&lcTmpCurs..nTotal,9))
      ENDCASE      
    ELSE
      DO CASE
        CASE cType = 'A' AND cType2 = 'A'    
          IF EMPTY(lcGroups)
            =SEEK(cGroup+'A'+cStyle+'B',lcTmpFile)
            INSERT INTO (lcExclCur);
               (Transact,&lcJan,&lcFeb,&lcMar,&lcApr,&lcMay,&lcJun,&lcJul,;
                &lcAug,&lcSept,&lcOct,&lcNov,&lcDec,nTotal,Overall);
            VALUES;
              (&lcTmpCurs..cTransct,;
               STR(&lcTmpCurs..nMonth1,8),STR(&lcTmpCurs..nMonth2,8),;
               STR(&lcTmpCurs..nMonth3,8),STR(&lcTmpCurs..nMonth4,8),;
               STR(&lcTmpCurs..nMonth5,8),STR(&lcTmpCurs..nMonth6,8),;
               STR(&lcTmpCurs..nMonth7,8),STR(&lcTmpCurs..nMonth8,8),;
               STR(&lcTmpCurs..nMonth9,8),STR(&lcTmpCurs..nMonth10,8),;
               STR(&lcTmpCurs..nMonth11,8),STR(&lcTmpCurs..nMonth12,8),;
               STR(&lcTmpCurs..nTotal,9),;
               STR(ROUND((&lcTmpCurs..nTotal/&lcTmpFile..nTotal)*100,2),5,2)+'%')

          ELSE
            =SEEK(cGroup+'B'+SPACE(12)+'A'+cTranTyp,lcTmpFile)
            INSERT INTO (lcExclCur);
               (Transact,&lcJan,&lcFeb,&lcMar,&lcApr,&lcMay,&lcJun,&lcJul,;
                &lcAug,&lcSept,&lcOct,&lcNov,&lcDec,nTotal,Overall);
            VALUES;
              (&lcTmpCurs..cTransct,;
               STR(&lcTmpCurs..nMonth1,8),STR(&lcTmpCurs..nMonth2,8),;
               STR(&lcTmpCurs..nMonth3,8),STR(&lcTmpCurs..nMonth4,8),;
               STR(&lcTmpCurs..nMonth5,8),STR(&lcTmpCurs..nMonth6,8),;
               STR(&lcTmpCurs..nMonth7,8),STR(&lcTmpCurs..nMonth8,8),;
               STR(&lcTmpCurs..nMonth9,8),STR(&lcTmpCurs..nMonth10,8),;
               STR(&lcTmpCurs..nMonth11,8),STR(&lcTmpCurs..nMonth12,8),;
               STR(&lcTmpCurs..nTotal,9),;
               STR(ROUND((&lcTmpCurs..nTotal/&lcTmpFile..nTotal)*100,2),5,2)+'%')

          ENDIF  
        CASE (cType = 'A' AND cType2 = 'B')
          * =SEEK(lcSGroup+'B'+SPACE(12)+cTranTyp+'A')
          INSERT INTO (lcExclCur);
             (Style,&lcJan,&lcFeb,&lcMar,&lcApr,&lcMay,&lcJun,&lcJul,;
              &lcAug,&lcSept,&lcOct,&lcNov,&lcDec,nTotal);
          VALUES;
            ('Total all transactions for '+&lcTmpCurs..cStyle,;
             STR(&lcTmpCurs..nMonth1,8),STR(&lcTmpCurs..nMonth2,8),;
             STR(&lcTmpCurs..nMonth3,8),STR(&lcTmpCurs..nMonth4,8),;
             STR(&lcTmpCurs..nMonth5,8),STR(&lcTmpCurs..nMonth6,8),;
             STR(&lcTmpCurs..nMonth7,8),STR(&lcTmpCurs..nMonth8,8),;
             STR(&lcTmpCurs..nMonth9,8),STR(&lcTmpCurs..nMonth10,8),;
             STR(&lcTmpCurs..nMonth11,8),STR(&lcTmpCurs..nMonth12,8),;
             STR(&lcTmpCurs..nTotal,9))
        CASE (cType = 'B' AND cType2 = 'A')
          IF llGrpTot
            INSERT INTO (lcExclCur) (Style) ;
                   VALUES ('Grand Total '+ALLTRIM(&lcTmpCurs..cGrpDesc)+' Style Group')
            llGrpTot = .F.
          ENDIF         
            INSERT INTO (lcExclCur);
               (Transact,&lcJan,&lcFeb,&lcMar,&lcApr,&lcMay,&lcJun,&lcJul,;
                &lcAug,&lcSept,&lcOct,&lcNov,&lcDec,nTotal);
            VALUES;
              (&lcTmpCurs..cTransct,;
               STR(&lcTmpCurs..nMonth1,8),STR(&lcTmpCurs..nMonth2,8),;
               STR(&lcTmpCurs..nMonth3,8),STR(&lcTmpCurs..nMonth4,8),;
               STR(&lcTmpCurs..nMonth5,8),STR(&lcTmpCurs..nMonth6,8),;
               STR(&lcTmpCurs..nMonth7,8),STR(&lcTmpCurs..nMonth8,8),;
               STR(&lcTmpCurs..nMonth9,8),STR(&lcTmpCurs..nMonth10,8),;
               STR(&lcTmpCurs..nMonth11,8),STR(&lcTmpCurs..nMonth12,8),;
               STR(&lcTmpCurs..nTotal,9))
      ENDCASE
    ENDIF
  ENDSCAN  
ENDDO
USE IN (lcTmpFile)
SELECT (lcExclCur)
GOTO TOP
EXPORT TO (lcRpExlFil) XLS   
=gfModalGen("TRM000000B00000","DIALOG",'','','Export to Excel has been completed successfully.')


*!*************************************************************
*! Name      : lfPrevRun
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/25/2000 
*! Purpose   : To do not display the printing selection device
*!*************************************************************
*! Example     : = lfPrevRun()
*!*************************************************************
FUNCTION lfPrevRun

PRIVATE llContinue
llContinue = .T.
llOGEscPrsd = .F.
lnOGHD=IIF(EMPTY(laOGHdFlt[1,1]),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(laOGFxFlt[1,1]),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(laOGVrFlt[1,1]),0,ALEN(laOGVrFlt,1))
lcOGSelect=SELECT()
=lfInitValue('laOGVrFlt','T')
=lfInitValue('laOGFXFlt','T')
  
lcOGHDFlt=gfGenFlt('laOGHDFlt' , llOGFilter , .T.)
lcOGFxFlt=gfGenFlt('laOGFxFlt' , llOGFilter , .T.)
lcOGVrFlt=gfGenFlt('laOGVrFlt' , llOGFilter , .T.)
  
lnOGHD=IIF(EMPTY(lcOGHDFlt),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(lcOGFxFlt),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(lcOGVrFlt),0,ALEN(laOGVrFlt,1))  
=lfInitValue('laOGVrFlt','C')
=lfInitValue('laOGFXFlt','C')

IF VARREAD() = "PBRUN"  
  lcRpExp=lcOGHDFlt+IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,' AND (','')+;
        lcOGFxFlt+IIF(lnOGVr>0,IIF(lnOgFx>0,' AND ',''),'')+lcOGVrFlt+;
        IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,')','')
  IF lcRpOExcel = 'N'
    llContinue = pSetup(.T.,llOGBatchOk)
    IF llContinue
      IF gcDevice = 'BATCH'
        SELECT SYUBSRVJ
        IF EMPTY(CREP_ID)
          lcRpRepID = lcOGRepID 
          SAVE  TO MEMO MUSRDVAR ALL LIKE L?RP*
          IF !EMPTY(laOGHDFLT[1,1])
            SAVE TO MEMO mRepHdFlt ALL LIKE laOGHDFLT
          ENDIF
          IF !EMPTY(laOGFxFLT[1,1])  
            =lfInitValue('laOGFxFlt','T')    
            SAVE TO MEMO mRepFxFlt ALL LIKE laOGFxFlt
            =lfInitValue('laOGFxFlt','C')      
          ENDIF  
          IF !EMPTY(laOGVrFLT[1,1])  
            =lfInitValue('laOGVrFlt','T')
            SAVE TO MEMO mRepVrFlt ALL LIKE laOGVrFlt  
            =lfInitValue('laOGVrFlt','C') 
          ENDIF  
          REPLACE CREP_ID WITH lcOGManRep,;
                  cstatus WITH 'O'      
        ENDIF
      ENDIF
    ENDIF  
  ENDIF  
ELSE
  IF llContinue
    lcOGTmpExp=IIF(!EMPTY(lcOGHDFlt),lcOGHDFlt+'|Ÿ',lcOGHDFlt)+IIF(!EMPTY(lcOGFXFlt) AND !EMPTY(lcOGVRFlt),lcOGFXFlt+'|Ÿ',lcOGFxFlt)+;
               lcOGVrFlt+IIF(!EMPTY(lcOGHDFlt),'Ÿ','')+IIF(!EMPTY(lcOGFXFlt)AND !EMPTY(lcOGVRFlt),'Ÿ','')
           
    IF EMPTY(lcOGFxFlt) AND EMPTY(lcOGVrFlt)
      lcRpExp=lcOgHdFlt
    ELSE
      lcRpExp=STRTRAN(STRTRAN(lcOGTmpExp,IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'|Ÿ','|Ÿ|Ÿ');
                ,' AND('),IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'Ÿ','ŸŸ'),')')
    ENDIF          

    lcOGDevice=gcDevice
    lcOGOutFile=gcOutFile
    gcDevice="SCREEN"    && set printer to temprory file name
    gcOutFile=gcWorkDir+gfTempName()+".TXT"      
    llOGHeader = glHeader
    glHeader = .T.
  ENDIF
ENDIF

IF llContinue
  lcRpExp=IIF(EMPTY(lcRpExp),'.T.',lcRpExp)
  lcogExc=SET('EXACT')        
  SET EXACT ON
  llOGFltCh = IIF(llOGFltCh,llOGFltCh,lcogfltr<>lcrpexp)        

  SET EXACT &lcOGExc
  IF llOGFltCh
    lcOGFltr=lcRpExp
  ENDIF
  SET CONSOLE OFF

  IF !EMPTY(lcOGFormV)
    IF !(lcOGLastForm==&lcOGFormV)
      
      lcOGFormArr='laRpObj'+SUBSTR(lcOGPlatForm,1,3)
      
      lcOGLastForm=&lcOGFormV
      llOGStyleCh=.T.
    ENDIF  

    IF llOGRefForm .OR. lcRepAvlM = 'B' .OR. llOGStyleCh
      =gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)
    ENDIF

  ENDIF  

  DO (gcRepHome+gcAct_Appl+'REPORT.APP') WITH lcOGPrgName, .T.

  SET CONSOLE ON

  IF !VARREAD() == "PBRUN")
    gcDevice=lcOGDevice
    gcOutFile=lcOGOutFile
    glHeader = llOGHeader
  ENDIF  

  llOGFltCh = .F.
ENDIF  
RETURN .F.
*-- End of lfPrevRun.










*!*************************************************************
*! Name      : lfGetGrp
*! Developer : TAK
*! Date      : 04/24/2000 
*! Purpose   : To get a coresponding sort group/ return value.
*!*************************************************************
FUNCTION lfGetGrp
PARA llGetDEsc

lcRetGrup = SPACE(7)

DO CASE
  CASE lcRpSortBy='A'
     lcAcctAls = IIF(laTrans[1,lnTranCnt]='Returns',"RETLINE",IIF(laTrans[1,lnTranCnt]='Orders',"ORDLINE","INVLINE"))
    lcRetGrup = &lcAcctAls..Account
  CASE lcRpSortBy='R'
    lcRetGrup = IIF(laTrans[1,lnTranCnt]='Returns',RETHDR.Salesrep1,IIF(laTrans[1,lnTranCnt]='Orders',ORDHDR.Rep1,INVHDR.REP1))
  CASE lcRpSortBy='F'
    lcRetGrup = STYLE.Fabric
  OTHE  && lcRpSortBy='G'    
    lcRetGrup = STYLE.cStyGroup
ENDCASE

IF llGetDEsc   && Desc.
  DO CASE
    CASE lcRpSortBy='A'
      lcGrpDesc = IIF(SEEK('M'+lcRetGrup,'CUSTOMER'),CUSTOMER.BTNAME,'')
    CASE lcRpSortBy='R'
      lcGrpDesc = IIF(SEEK(lcRetGrup,'SALESREP'),SALESREP.REPCODE+' - '+SALESREP.NAME,'')
    CASE lcRpSortBy='F'
      lcGrpDesc = IIF(SEEK(lcRetGrup,'FABRIC'),FABRIC.Desc,'')
    OTHE  && lcRpSortBy='G'    
      lcGrpDesc = gfCodDes(lcRetGrup,'CSTYGROUP ')
  ENDCASE
ENDIF
RETURN PADR(lcRetGrup,7)


FUNCTION lfRetNam

lcRetNam = ""
  DO CASE
    CASE lcRpSortBy='A'
      lcRetNam = 'Account'
    CASE lcRpSortBy='R'
      lcRetNam = 'Sales Rep'
    CASE lcRpSortBy='F'
      lcRetNam = 'Fabric'
    CASE lcRpSortBy='G'
      lcRetNam = 'Group'
    OTHE
      lcRetNam = 'Style'
  ENDCASE
RETURN lcRetNam