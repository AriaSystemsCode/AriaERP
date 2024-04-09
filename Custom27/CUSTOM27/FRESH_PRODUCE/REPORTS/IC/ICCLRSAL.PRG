****************************************************************************
*: Program file      : ICCLRSL.PRG (C#101813)
*: Program desc.     : COLOR SALES REPORT
*: System            : Aria Apparel System (A27).
*: Module            : Inventory Control (IC)
*: Developer         : Khalid Mohi El-Din Mohamed KHM
*: Date              : 04/16/2000
*C200137,1 TAK 10/10/2000
*:**************************************************************************

*--Get the lower and higher range of date
ldFromDate = IIF(EMPTY(ldRpFDt),CTOD('01/01/'+STR(YEAR(gdSysDate),4)),ldRpFDt)
ldToDate   = IIF(EMPTY(ldRpTDt),CTOD(PADL(MONTH(gdSYSDate),2,'0')+'/'+;
             PADL(DAY(gdSYSDate),2,'0')+'/'+STR(YEAR(gdSysDate),4)),ldRpTDt)

*-- If the user enters more than 12 periods then "To" date would be 
*-- the "From" date + 1 year
IF ldToDate - ldFromDate > 366
  ldToDate = CTOD(PADL(ALLTRIM(STR(MONTH(ldFromDate))),2,'0')+'/'+;
             PADL(ALLTRIM(STR(DAY(ldFromDate))),2,'0')+'/'+;
             STR(YEAR(ldFromDate)+1,4))
ENDIF

*-- Check if no transaction was selected then print all the transactions
*-- otherwise, print the selected transactions.
= IIF(EMPTY(laRpTarget),ACOPY(laRpSource,laTrans),ACOPY(laRpTarget,laTrans))

*-- Check if no Location was selected then select the data for all location
*-- otherwise, select the data according to the selected location
= IIF(EMPTY(laRpLTargt),ACOPY(laRpLSourc,laLocation),ACOPY(laRpLTargt,laLocation))
lcAllLoc = ''
FOR lnArryCnt2 = 1 TO ALEN(laLocation,1)
  lcAllLoc = lcAllLoc + PADR(laLocation[1,lnArryCnt2],6,' ')+'|'
ENDFOR

*-- To get the selected seasons
lcSeasons = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.SEASON'),1)
IF lnPosition > 0
  lcSeasons = laOGFxFlt[lnPosition,6] 
ENDIF


**C200137,1
=gfOpenFile(gcDataDir+"StyDye","StyDye","SH")


*-- To get the selected style groups.
lcGroups = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYGROUP'),1)
IF lnPosition > 0
  lcGroups = laOGFxFlt[lnPosition,6] 
ENDIF

*-- If the user selects Styles.
lcStyFile = ''
llStySltd = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStyFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcStyFile) AND USED(lcStyFile)
    SELECT (lcStyFile)
    GOTO TOP
    llStySltd = !EOF()
  ENDIF  
ENDIF


*-- To get the selected Colors
lcAllClr = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'RIGHT(Style.style,6)'),1)
IF lnPosition > 0
  lcAllClr = laOGFxFlt[lnPosition,6] 
ENDIF


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


*-- Print the Form according to the sort option.

DO CASE
  CASE lcRpSort $ 'SG'
    *-- To collect the data for form B sort by Style or Style Major
    =lfvFormA()
  CASE lcRpSort = 'C'
    *-- To collect the data for form B sort by Color
    =lfvFormB()
  CASE lcRpSort = 'A'
    =lfvFormC()
ENDCASE


*!*************************************************************
*! Name      : lfvFormA
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : The Collect the data for the selected transactions. 
*!             sort by Style Major or Style Group Form A
*!*************************************************************
*! Example     : = lfvFormA()
*!*************************************************************
FUNCTION lfvFormA

*-- Create a temporary cursor to hold the records that matched the
*-- selected criteria.
lcTmpCurs = gfTempName()
CREATE TABLE(gcWorkDir+lcTmpCurs);
    (cGroup C(6),cGrpDesc C(30),cType C(1), cStyle C(12),cStyDesc C(20),;
     cType2 C(1),cColor C(6), cClrDesc C(30),cTransct C(10), cTranTyp C(1),;
     nMonth1 N(8), nMonth2 N(8), nMonth3 N(8), nMonth4 N(8), nMonth5 N(8),;
     nMonth6 N(8), nMonth7 N(8), nMonth8 N(8), nMonth9 N(8), nMonth10 N(8),;
     nMonth11 N(8), nMonth12 N(8),nTotal N(9), nPercent N(5,2))
INDEX ON cGroup+cType+cStyle+cType2+cColor+cTranTyp TAG(lcTmpCurs)

*-- To hold the total colors for each group.
lcGrpTCur = gfTempName()
CREATE TABLE(gcWorkDir+lcGrpTCur);
    (cGroup C(6),cGrpDesc C(30),cType C(1), cStyle C(12),cStyDesc C(30),;
     cType2 C(1),cColor C(6), cClrDesc C(30),cTransct C(10), cTranTyp C(1),;
     nMonth1 N(8), nMonth2 N(8), nMonth3 N(8), nMonth4 N(8), nMonth5 N(8),;
     nMonth6 N(8), nMonth7 N(8), nMonth8 N(8), nMonth9 N(8), nMonth10 N(8),;
     nMonth11 N(8), nMonth12 N(8),nTotal N(9), nPercent N(5,2))
INDEX ON cGroup+cType+cStyle+cType2+cColor+cTranTyp TAG(lcGrpTCur)

*-- To hold the total of all transactions for each group.
lcTotCur = gfTempName()
CREATE TABLE(gcWorkDir+lcTotCur);
    (cGroup C(6),cGrpDesc C(30),cType C(1), cType2 C(1),cTransct C(10),;
     cTranTyp C(1),nMonth1 N(8), nMonth2 N(8), nMonth3 N(8), nMonth4 N(8),;
     nMonth5 N(8),nMonth6 N(8), nMonth7 N(8), nMonth8 N(8), nMonth9 N(8),;
     nMonth10 N(8),nMonth11 N(8), nMonth12 N(8),nTotal N(9), nPercent N(5,2))

**C200137,1
*-- To get the selected style groups.
*lcGroups = ''
*lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYGROUP'),1)
*IF lnPosition > 0
*  lcGroups = laOGFxFlt[lnPosition,6] 
*ENDIF


*-- If the user selects Styles.
*lcStyFile = ''
*lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
*IF lnPosition > 0
*  lcStyFile = laOGFxFlt[lnPosition,6]
*  IF !EMPTY(lcStyFile) AND USED(lcStyFile)
*    SELECT (lcStyFile)
*    GOTO TOP
*  ENDIF  
*ENDIF


*-- Setting the order of the style to style major
SELECT Style
SET ORDER TO TAG cStyle

lnPercent = 0
FOR lnTranCnt = 1 TO ALEN(laTrans,1)
  DO CASE
    CASE laTrans[1,lnTranCnt] = 'Orders'

      SELECT OrdHdr
      SET ORDER TO TAG OrdHdr
      SELECT OrdLine
      SET RELATION TO
      SET ORDER TO TAG OrdLineS
      SET RELATION TO 'O'+Order INTO OrdHdr ADDITIVE
      
      lcOrdFilt = 'REST WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6)='+;
                  'lcStyMaj FOR BETWEEN(Start,ldFromDate,ldToDate)'+;
                  ' AND OrdHdr.Status $ "OH" AND TotQty > 0'+;
                  ' AND cWareCode $ lcAllLoc'+;
                  IIF(!EMPTY(lcSeasons),' AND Style.Season $ lcSeasons','')+;
                  IIF(!EMPTY(lcGroups),' AND Style.cStyGroup $ lcGroups','')+;
                  IIF(!EMPTY(lcAllClr),' AND  RIGHT(Style,6) $ lcAllClr','')
                  
      IF !EMPTY(lcStyFile) AND USED(lcStyFile) AND !EOF(lcStyFile)

        SELECT (lcStyFile)
        SCAN
          =SEEK(cStyMajor,'Style')
          lcStyMaj = SUBSTR(&lcStyFile..cStyMajor,1,12)
          IF SEEK(SUBSTR(cStyMajor,1,12),'OrdLine')
            SELECT OrdLine
            STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                       m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                       m.nMonth11,m.nMonth12,m.nTotal
            =lfUpdTmpA(lcOrdFilt,'1','OrdLine.TotQty','Orders','OrdLine.Start')

            *-- Replace total quantities for each style.
            IF m.nTotal > 0
              =lfRepQty('1','Orders')
            ENDIF  
            
          ENDIF
        ENDSCAN        
        GOTO TOP
      ELSE
        SELECT OrdLine
        GOTO TOP
        DO WHILE !EOF()          
          lcStyMaj = SUBSTR(Style,1,12)
          =SEEK(lcStyMaj,'Style')
          STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                     m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                     m.nMonth11,m.nMonth12,m.nTotal
          =lfUpdTmpA(lcOrdFilt,'1','OrdLine.TotQty','Orders','OrdLine.Start')  
          *-- Replace total quantities for each style.
          IF m.nTotal > 0
            =lfRepQty('1','Orders')
          ENDIF          
        ENDDO                
      ENDIF
    
    CASE laTrans[1,lnTranCnt] = 'Invoiced'
      SELECT InvHdr
      SET ORDER TO TAG InvHdr
      SELECT InvLine      
      SET ORDER TO TAG InvLineS
      SET RELATION TO
      SET RELATION TO Invoice INTO InvHdr ADDITIVE

      lcInvFilt = 'REST WHILE Style+Invoice+STR(LineNo,6) = lcStyMaj'+;
                  ' FOR BETWEEN(InvDate,ldFromDate,ldToDate)'+;
                  ' AND InvHdr.Status = "C" AND TotQty > 0'+;
                  ' AND InvHdr.cWareCode $ lcAllLoc'+;
                  IIF(!EMPTY(lcSeasons),' AND Style.Season $ lcSeasons','')+;
                  IIF(!EMPTY(lcGroups),' AND Style.cStyGroup $ lcGroups','')+;
                  IIF(!EMPTY(lcAllClr),' AND  RIGHT(Style,6) $ lcAllClr','')


      IF !EMPTY(lcStyFile) AND USED(lcStyFile) AND !EOF(lcStyFile)
        SELECT (lcStyFile)
        GOTO TOP
        SCAN
          =SEEK(cStyMajor,'Style')
          lcStyMaj = SUBSTR(&lcStyFile..cStyMajor,1,12)
          IF SEEK(SUBSTR(cStyMajor,1,12),'InvLine')
            SELECT InvLine
            STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                       m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                       m.nMonth11,m.nMonth12,m.nTotal

            =lfUpdTmpA(lcInvFilt,'2','InvLine.TotQty','Invoiced','InvLine.InvDate')

            *-- Replace total quantities for each style.
            IF m.nTotal > 0
              =lfRepQty('2','Invoiced')
            ENDIF  
          ENDIF
        ENDSCAN
        GOTO TOP       
      ELSE
        SELECT InvLine
        GOTO TOP
        DO WHILE !EOF()
          lcStyMaj = SUBSTR(Style,1,12)
          =SEEK(lcStyMaj,'Style')
          STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                     m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                     m.nMonth11,m.nMonth12,m.nTotal
          =lfUpdTmpA(lcInvFilt,'2','InvLine.TotQty','Invoiced','InvLine.InvDate')
          *-- Replace total quantities for each style.
          IF m.nTotal > 0
            =lfRepQty('2','Invoiced')
          ENDIF
        ENDDO
      ENDIF

    CASE laTrans[1,lnTranCnt] = 'Returns'
      SELECT RetHdr
      SET ORDER TO RetHdr
      SELECT RetLine
      SET ORDER TO RetLineS
      SET RELATION TO
      SET RELATION TO CrMemo INTO RetHdr ADDITIVE
      lcRetFilt = 'REST WHILE Style+CrMemo= lcStyMaj'+;
                  ' FOR BETWEEN(CrDate,ldFromDate,ldToDate)'+;
                  ' AND TotQty > 0 AND RetHdr.cWareCode $ lcAllLoc'+;
                  IIF(!EMPTY(lcSeasons),' AND Style.Season $ lcSeasons','')+;
                  IIF(!EMPTY(lcGroups),' AND Style.cStyGroup $ lcGroups','')+;
                  IIF(!EMPTY(lcAllClr),' AND  RIGHT(Style,6) $ lcAllClr','')


      IF !EMPTY(lcStyFile) AND USED(lcStyFile) AND !EOF(lcStyFile)

        SELECT (lcStyFile)
        GOTO TOP
        SCAN
          =SEEK(cStyMajor,'Style')
          lcStyMaj = SUBSTR(&lcStyFile..cStyMajor,1,12)
          IF SEEK(SUBSTR(cStyMajor,1,12),'RetLine')
            SELECT RetLine
            STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                       m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                       m.nMonth11,m.nMonth12,m.nTotal

            =lfUpdTmpA(lcRetFilt,'3','RetLine.TotQty','Returns','RetLine.CrDate')

            *-- Replace total quantities for each style.
            IF m.nTotal > 0
              =lfRepQty('3','Returns')
            ENDIF  
          ENDIF
        ENDSCAN
        GOTO TOP       
      ELSE
        SELECT RetLine
        GOTO TOP
        DO WHILE !EOF()
          lcStyMaj = SUBSTR(Style,1,12)
          =SEEK(lcStyMaj,'Style')
          STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                     m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                     m.nMonth11,m.nMonth12,m.nTotal
          SELECT RetLine
          =lfUpdTmpA(lcRetFilt,'3','RetLine.TotQty','Returns','RetLine.CrDate')
          *-- Replace total quantities for each style.
          IF m.nTotal > 0
            =lfRepQty('3','Returns')
          ENDIF
        ENDDO
      ENDIF

    CASE laTrans[1,lnTranCnt] = 'WIP'
      SELECT PosLn
      SET ORDER TO TAG PosLnS
      SET RELATION TO
      SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE

      lcWipFilt = 'REST WHILE Style+cStyType+Po+STR(LineNo,6)+TranCd=lcStyMaj'+;
                  ' FOR cStyType="D" AND PosHdr.Status $ "OH"'+;
                  ' AND TotQty > 0 AND cWareCode $ lcAllLoc'+;
                  IIF(!EMPTY(lcSeasons),' AND Style.Season $ lcSeasons','')+;
                  IIF(!EMPTY(lcGroups),' AND Style.cStyGroup $ lcGroups','')+;
                  IIF(!EMPTY(lcAllClr),' AND  RIGHT(Style,6) $ lcAllClr','')


      IF !EMPTY(lcStyFile) AND USED(lcStyFile) AND !EOF(lcStyFile)
        SELECT (lcStyFile)
        GOTO TOP
        SCAN
          =SEEK(cStyMajor,'Style')
          lcStyMaj = SUBSTR(&lcStyFile..cStyMajor,1,12)
          IF SEEK(SUBSTR(cStyMajor,1,12),'PosLn')
            SELECT PosLn
            STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                       m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                       m.nMonth11,m.nMonth12,m.nTotal

            =lfUpdTmpA(lcWipFilt,'4','PosLn.TotQty','WIP','PosHdr.Entered')

            *-- Replace total quantities for each style.
            IF m.nTotal > 0
              =lfRepQty('4','WIP')
            ENDIF  
          ENDIF
        ENDSCAN
        GOTO TOP       
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
          =lfUpdTmpA(lcWipFilt,'4','PosLn.TotQty','WIP','PosHdr.Entered')
          *-- Replace total quantities for each style.
          IF m.nTotal > 0
            =lfRepQty('4','WIP')
          ENDIF
        ENDDO
      ENDIF

    CASE laTrans[1,lnTranCnt] = 'Stock'
      *lcStckFilt = 'REST WHILE Style+cWareCode+cSession+DTOS(DtrDate)+cTrCode+STR(LineNo,6)='+;
      *             ' lcStyMaj FOR !EMPTY(DtrDate) AND nTotStk <> 0 AND cWareCode $ lcAllLoc'+;
      *             IIF(!EMPTY(lcSeasons),' AND Style.Season $ lcSeasons','')+;
      *            IIF(!EMPTY(lcGroups),' AND Style.cStyGroup $ lcGroups','')+;
      *            IIF(!EMPTY(lcAllClr),' AND  RIGHT(Style,6) $ lcAllClr','')


      lcStckFilt = 'REST WHILE style+cwarecode+dyelot='+;
                   ' lcStyMaj FOR TotStk <> 0 AND cWareCode $ lcAllLoc'+;
                   IIF(!EMPTY(lcSeasons),' AND Style.Season $ lcSeasons','')+;
                  IIF(!EMPTY(lcGroups),' AND Style.cStyGroup $ lcGroups','')+;
                  IIF(!EMPTY(lcAllClr),' AND  RIGHT(Style,6) $ lcAllClr','')

      IF !EMPTY(lcStyFile) AND USED(lcStyFile) AND !EOF(lcStyFile)
        SELECT (lcStyFile)
        GOTO TOP
        SCAN
          lcStyMaj = SUBSTR(&lcStyFile..cStyMajor,1,12)
          =SEEK(lcStyMaj,'Style')          

          IF SEEK(SUBSTR(lcStyMaj,1,12),'STYDYE')
            SELECT StyDye
            STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                       m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                       m.nMonth11,m.nMonth12,m.nTotal
            =lfUpdTmpA(lcStckFilt,'5','StyDye.TotStk','Stock','gdSysDate')

            *-- Replace total quantities for each style.
            IF m.nTotal <> 0
              =lfRepQty('5','Stock')
            ENDIF  
          ENDIF

          *IF SEEK(SUBSTR(cStyMajor,1,12),'StyInvJl')
          *  SELECT StyInvJl
          *  STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
          *             m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
          *             m.nMonth11,m.nMonth12,m.nTotal
          *  =lfUpdTmpA(lcStckFilt,'5','StyInvJl.nTotStk','Stock','StyInvJl.DtrDate')
          *  *-- Replace total quantities for each style.
          *  IF m.nTotal <> 0
          *    =lfRepQty('5','Stock')
          *  ENDIF  
          *ENDIF

        ENDSCAN
        GOTO TOP       
      ELSE
        **C200137,1
        SELECT StyDye
        GOTO TOP
        DO WHILE !EOF()
          lcStyMaj = SUBSTR(Style,1,12)
          =SEEK(lcStyMaj,'Style')
          STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
                     m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
                     m.nMonth11,m.nMonth12,m.nTotal
          SELECT StyDye
          =lfUpdTmpA(lcStckFilt,'5','StyDye.TotStk','Stock','gdSysDate')
          *-- Replace total quantities for each style.
          IF m.nTotal <> 0
            =lfRepQty('5','Stock')
          ENDIF
        ENDDO

        *SELECT StyInvJl
        *GOTO TOP
        *DO WHILE !EOF()
        *  lcStyMaj = SUBSTR(Style,1,12)
        *  =SEEK(lcStyMaj,'Style')
        *  STORE 0 TO m.nMonth1,m.nMonth2,m.nMonth3,m.nMonth4,m.nMonth5,;
        *             m.nMonth6,m.nMonth7,m.nMonth8,m.nMonth9,m.nMonth10,;
        *             m.nMonth11,m.nMonth12,m.nTotal
        *  SELECT StyInvJl
        *  =lfUpdTmpA(lcStckFilt,'5','StyInvJl.nTotStk','Stock','StyInvJl.DtrDate')
        *  *-- Replace total quantities for each style.
        *  IF m.nTotal <> 0
        *    =lfRepQty('5','Stock')
        *  ENDIF
        *ENDDO
      ENDIF
  ENDCASE
ENDFOR


SELECT Style
SET ORDER TO TAG cStyle

*-- To calculate the total for each transaction in the group
IF lcRpSort = 'G'
  =lfColGTot()
ENDIF

USE IN (lcGrpTCur)
USE IN (lcTotCur)

SELECT (lcTmpCurs)

**C200137,1
IF lcRpSort = 'G'
  APPEND FROM (gcWorkDir+lcGrpTCur)
  APPEND FROM (gcWorkDir+lcTotCur)
ENDIF
GOTO TOP
DELETE ALL FOR nTotal = 0
GOTO TOP
IF lcRpSort = 'G'
  *--Calculate percentage for style totals,
  SCAN FOR CTYPE='A' AND CTYPE2='B'
    lnPercent = 0
    =lfGetPrcnt(cGroup,'A','','B',cTranTyp)
    REPLACE nPercent WITH ROUND((nTotal/lnPercent)*100,2)
  ENDSCAN
ELSE
  REPLACE ALL cGroup WITH ''
ENDIF

GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
=gfDispRe(lcRpFrmId)
SET DEVICE TO SCREEN
IF USED(lcTmpCurs)
  SELECT (lcTmpCurs)
  USE
  ERASE(gcWorkDir+lcTmpCurs+'.DBF')
  ERASE(lcTmpCurs+'.CDX')
ENDIF

ERASE(gcWorkDir+lcGrpTCur+'.DBF')
ERASE(lcGrpTCur+'.CDX')
ERASE(gcWorkDir+lcTotCur+'.DBF')

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
PRIVATE lnRecordNo

lnRecordNo = RECNO()

DO CASE
  CASE lcType1 = 'A' AND lcType2 = 'A'    
    =SEEK(lcSGroup+'A'+lcStyleCd+'B'+SPACE(6)+lcTranCd)
    lnPercent = nTotal
  CASE (lcType1 = 'A' AND lcType2 = 'B') OR (lcType1 = 'B' AND lcType2 = 'A')
    =SEEK(lcSGroup+'B'+SPACE(12)+'B'+SPACE(6)+lcTranCd)
    lnPercent = nTotal  
ENDCASE

IF BETWEEN(lnRecordNo,1,RECCOUNT())
  GOTO lnRecordNo
ENDIF
RETURN ''
*!*************************************************************
*! Name      : lfUpdTmpA
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : The Update the cursor with the selected transactions.
*! Parameter : lcExpr  - > The scan expersion
*!             lcTran   ->  '1'-> Order '2'-> Invoice '3'-> Retuns
*!							'4'-> WIP   '5'-> Stock 
*!             lcTotQty -> OrdLine.TotQty,InvLine.TotQty,RetLine.TotQty
*!                         PosLine.TotQty,StyInvJl.TotQty
*!             lcTranName - > 'Orders','Invoiced','Returns','WIP','Stock'     
*!             ldTranDate - > 'Start','InvDate','CrDate'
*!*************************************************************
*! Example     : = lfUpdTmpA()
*!*************************************************************
FUNCTION lfUpdTmpA
PARAMETER lcExpr,lcTran,lcTotQty,lcTranName,ldChkDate

SCAN &lcExpr
  WAIT WINDOW lcTranName+': Style - Color ' + Style NOWAIT
  lcColor   = RIGHT(Style,6)
  lcClrDesc = gfCodDes(lcColor,'COLOR     ')
  lcGroup   = Style.cStyGroup
  lcGrpDesc = gfCodDes(lcGroup,'CSTYGROUP ')
  lcStyle   = SUBSTR(Style,1,12)


  IF !SEEK(lcGroup+'A'+lcStyle+'A'+lcColor+lcTran,lcTmpCurs)    
    INSERT INTO (lcTmpCurs);
        (cGroup, cGrpDesc, cType,cStyle, cStyDesc, cType2, cColor, cClrDesc,;
         cTransct, cTranTyp) VALUES;
        (lcGroup,lcGrpDesc,'A',lcStyle,Style.Desc,'A',lcColor,lcClrDesc,lcTranName,;
         lcTran)
  ENDIF

  lcMonth = ALLTRIM(STR(MONTH(&ldChkDate)))
  
  *-- To calculate the totals of all transactions of all colors for each group.
  =lfGetColor(lcGroup,lcGrpDesc,lcColor,lcTran,lcTranName)

  **C200137,1
  IF lcTran = '5'
     REPLACE &lcTmpCurs..nTotal WITH  &lcTmpCurs..nTotal + &lcTotQty
     m.nTotal = m.nTotal + &lcTotQty
  ELSE
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
      IF TranCd $ '245'
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
  ENDIF
ENDSCAN
WAIT CLEAR

*!*************************************************************
*! Name      : lfRepQty
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : To calculate the totals for each style
*!*************************************************************
*! Example     : = lfRepQty()
*!*************************************************************
FUNCTION lfRepQty
PARAMETERS lcTran,lcTranName
PRIVATE lnAlias

lnAlias = SELECT()

SELECT (lcTmpCurs)

IF !SEEK(Style.cStyGroup+'A'+lcStyMaj+'B'+SPACE(6)+lcTran)
  INSERT INTO (lcTmpCurs);
              (cGroup,cType,cStyle,cType2,cColor,cTransct,cTranTyp);
       VALUES (Style.cStyGroup,'A',lcStyMaj,'B',SPACE(6),lcTranName,lcTran)
ENDIF

**C200137,1
IF lcTran = '5'
   REPLACE &lcTmpCurs..nTotal WITH  &lcTmpCurs..nTotal + m.nTotal
ELSE
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
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : To calculate the totals each color and transactions
*!             of each style group.
*!*************************************************************
*! Example     : = lfGetColor()
*!*************************************************************
FUNCTION lfGetColor
PARAMETERS lcGrp,lcGrpDsc,lcClr,lcTranNo,lcTranNm

IF !SEEK(lcGrp+'B'+SPACE(12)+'A'+lcClr+lcTranNo,lcGrpTCur)
  INSERT INTO (lcGrpTCur);
      (cGroup, cGrpDesc, cType,cStyle,cType2,cColor, cClrDesc,;
       cTransct, cTranTyp) VALUES;
      (lcGrp,lcGrpDsc,'B',SPACE(12),'A',lcClr,lcClrDesc,lcTranNm,;
       lcTranNo)
ENDIF

**C200137,1
IF lcTran = '5'
   REPLACE &lcGrpTCur..nTotal WITH  &lcGrpTCur..nTotal + &lcTotQty
ELSE
IF lcTran <> '4'
  REPLACE &lcGrpTCur..nMonth&lcMonth WITH;
          &lcGrpTCur..nMonth&lcMonth + &lcTotQty
ELSE
  IF TranCd $ '16'
    REPLACE &lcGrpTCur..nMonth&lcMonth WITH;
            &lcGrpTCur..nMonth&lcMonth +&lcTotQty
  ELSE
    IF TranCd $ '245'
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
ENDIF

*!*************************************************************
*! Name      : lfColGTot
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : To calculate the totals for each group
*!*************************************************************
*! Example     : = lfColGTot()
*!*************************************************************
FUNCTION lfColGTot

SELECT (lcGrpTCur)
GOTO TOP

DO WHILE !EOF()
  FOR lnSelTran = 1 TO 12
    lcSelTran = ALLTRIM(STR(lnSelTran))
    STORE 0 TO lnOrdQty&lcSelTran,lnInvQty&lcSelTran,lnRetQty&lcSelTran,;
               lnWipQty&lcSelTran,lnStkQty&lcSelTran
  ENDFOR
  STORE 0 TO lnOrdTot,lnInvTot,lnRetTot,lnWipTot,lnStkTot
  lcStyGrp   = cGroup
  lcStyGrpDs = cGrpDesc
  SCAN REST WHILE cGroup+cType+cStyle+cType2+cColor+cTranTyp = lcStyGrp
    WAIT WINDOW "Total for group: " + lcStyGrp NOWAIT
    DO CASE
      CASE cTranTyp = '1'
        FOR lnOrdCnt = 1 TO 12
          lcOrdCnt = ALLTRIM(STR(lnOrdCnt))
          lnOrdQty&lcOrdCnt = lnOrdQty&lcOrdCnt + nMonth&lcOrdCnt
        ENDFOR
        lnOrdTot = lnOrdQty1+lnOrdQty2+lnOrdQty3+lnOrdQty4+lnOrdQty5+lnOrdQty6+;
                   lnOrdQty7+lnOrdQty8+lnOrdQty9+lnOrdQty10+lnOrdQty11+lnOrdQty12         
      CASE cTranTyp = '2'
        FOR lnInvCnt = 1 TO 12
          lcInvCnt = ALLTRIM(STR(lnInvCnt))
          lnInvQty&lcInvCnt = lnInvQty&lcInvCnt + nMonth&lcInvCnt
        ENDFOR
        lnInvTot = lnInvQty1+lnInvQty2+lnInvQty3+lnInvQty4+lnInvQty5+lnInvQty6+;
                   lnInvQty7+lnInvQty8+lnInvQty9+lnInvQty10+lnInvQty11+lnInvQty12
      CASE cTranTyp = '3'
        FOR lnRetCnt = 1 TO 12
          lcRetCnt = ALLTRIM(STR(lnRetCnt))
          lnRetQty&lcRetCnt = lnRetQty&lcRetCnt + nMonth&lcRetCnt
        ENDFOR
        lnRetTot = lnRetQty1+lnRetQty2+lnRetQty3+lnRetQty4+lnRetQty5+lnRetQty6+;
                   lnRetQty7+lnRetQty8+lnRetQty9+lnRetQty10+lnRetQty11+lnRetQty12
      CASE cTranTyp = '4'
        FOR lnWipCnt = 1 TO 12
          lcWipCnt = ALLTRIM(STR(lnWipCnt))
          lnWipQty&lcWipCnt = lnWipQty&lcWipCnt + nMonth&lcWipCnt
        ENDFOR
        lnWipTot = lnWipQty1+lnWipQty2+lnWipQty3+lnWipQty4+lnWipQty5+lnWipQty6+;
                   lnWipQty7+lnWipQty8+lnWipQty9+lnWipQty10+lnWipQty11+lnWipQty12
      CASE cTranTyp = '5'
        FOR lnStkCnt = 1 TO 12
          lcStkCnt = ALLTRIM(STR(lnStkCnt))
          lnStkQty&lcStkCnt = lnStkQty&lcStkCnt + nMonth&lcStkCnt        
        ENDFOR
        *lnStkTot = lnStkQty1+lnStkQty2+lnStkQty3+lnStkQty4+lnStkQty5+lnStkQty6+;
                   lnStkQty7+lnStkQty8+lnStkQty9+lnStkQty10+lnStkQty11+lnStkQty12
**C200137,1
        lnStkTot = lnStkTot + nTotal
    ENDCASE    
  ENDSCAN
  WAIT CLEAR
  IF lnOrdTot > 0
    =lfUpdTot(lcStyGrp,'Orders','1',lnOrdQty1,lnOrdQty2,lnOrdQty3,;
              lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8,lnOrdQty9,;
              lnOrdQty10,lnOrdQty11,lnOrdQty12,lnOrdTot)
  ENDIF
  IF lnInvTot > 0
    =lfUpdTot(lcStyGrp,'Invoiced','2',lnInvQty1,lnInvQty2,lnInvQty3,;
              lnInvQty4,lnInvQty5,lnInvQty6,lnInvQty7,lnInvQty8,lnInvQty9,;
              lnInvQty10,lnInvQty11,lnInvQty12,lnInvTot)
  ENDIF
  IF lnRetTot > 0
    =lfUpdTot(lcStyGrp,'Returns','3',lnRetQty1,lnRetQty2,lnRetQty3,;
              lnRetQty4,lnRetQty5,lnRetQty6,lnRetQty7,lnRetQty8,lnRetQty9,;
              lnRetQty10,lnRetQty11,lnRetQty12,lnRetTot)
  ENDIF
  IF lnWipTot > 0
    =lfUpdTot(lcStyGrp,'WIP','4',lnWipQty1,lnWipQty2,lnWipQty3,;
              lnWipQty4,lnWipQty5,lnWipQty6,lnWipQty7,lnWipQty8,lnWipQty9,;
              lnWipQty10,lnWipQty11,lnWipQty12,lnWipTot)
  ENDIF
  IF lnStkTot <> 0
    =lfUpdTot(lcStyGrp,'Stock','5',lnStkQty1,lnStkQty2,lnStkQty3,;
              lnStkQty4,lnStkQty5,lnStkQty6,lnStkQty7,lnStkQty8,lnStkQty9,;
              lnStkQty10,lnStkQty11,lnStkQty12,lnStkTot)
  ENDIF
ENDDO

*!*************************************************************
*! Name      : lfUpdTot
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : To add a new record for the total of each transaction
*!*************************************************************
*! Example     : = lfUpdTot()
*!*************************************************************
FUNCTION lfUpdTot
PARAMETERS lcStyGrp,lcTranNam,lcTranTyp,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,;
           lnQty6,lnQty7,lnQty8,lnQty9,lnQty10,lnQty11,lnQty12,lnTotQty
PRIVATE lnAlias

lnAlias = SELECT()

SELECT (lcTotCur)
APPEND BLANK
REPLACE cGroup   WITH lcStyGrp  , cGrpDesc WITH lcStyGrpDs,;       
        cType    WITH 'B'       , cType2   WITH 'B'       ,;
        cTransct WITH lcTranNam , cTranTyp WITH lcTranTyp ,;
        nMonth1  WITH lnQty1    , nMonth2  WITH lnQty2    ,;
        nMonth3  WITH lnQty3    , nMonth4  WITH lnQty4    ,;
        nMonth5  WITH lnQty5    , nMonth6  WITH lnQty6    ,;
        nMonth7  WITH lnQty7    , nMonth8  WITH lnQty8    ,;
        nMonth9  WITH lnQty9    , nMonth10 WITH lnQty10   ,;
        nMonth11 WITH lnQty11   , nMonth12 WITH lnQty12   ,;
        nTotal   WITH lnTotQty
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvFormB
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : The Collect the data for the selected transactions. 
*!             sort by Color FormB
*!*************************************************************
*! Example     : = lfvFormB()
*!*************************************************************
FUNCTION lfvFormB

*-- Create a temporary cursor to hold the records that matched the
*-- selected criteria.
lcTmpCurs = gfTempName()
CREATE CURSOR (lcTmpCurs);
    (cColor C(6), cClrDesc C(30), cTranTyp C(1), nMonth1 N(8), nMonth2 N(8),;
     nMonth3 N(8), nMonth4 N(8), nMonth5 N(8), nMonth6 N(8), nMonth7 N(8),;
     nMonth8 N(8), nMonth9 N(8), nMonth10 N(8), nMonth11 N(8), nMonth12 N(8),;
     nTotal N(9), nPercent N(5,2),cTransct C(10))
INDEX ON cColor+cTranTyp TAG(lcTmpCurs) OF (lcTmpCurs)


*-- To hold the filter
lcFilter  = ' FOR .T.'

*-- Adding the season to the filter if any selected.
lcFilter = IIF(!EMPTY(lcSeasons),lcFilter+' AND Style.Season $ lcSeasons',;
																lcFilter)
**C200137,1
lcFilter = IIF(!EMPTY(lcGroups),lcFilter+' AND Style.cStyGroup $ lcGroups',;
																lcFilter)
																
*-- Adding the colors to the filter if any selected.
lcFilter = IIF(!EMPTY(lcAllClr),lcFilter+' AND RIGHT(Style,6) $ lcAllClr',;
 													             lcFilter)
lcStyOrd = ORDER('Style')
SELECT Style
SET ORDER TO Style

*-- To hold the total of all colors.
lnClrTot = 0

*-- Get the records of the selected transactions.
FOR lnTranCnt = 1 TO ALEN(laTrans,1)
  DO CASE
    CASE laTrans[1,lnTranCnt] = 'Orders'
      SELECT OrdHdr
      SET ORDER TO TAG OrdHdr
      SELECT OrdLine
      SET ORDER TO TAG OrdLineS
      SET RELATION TO
      SET RELATION To Style     INTO Style ADDITIVE
      SET RELATION TO 'O'+Order INTO OrdHdr ADDITIVE

      *-- Adding the location to the filter if any was selected
      lcOrdFilt = lcFilter + ' AND BETWEEN(Start,ldFromDate,ldToDate)'+;
                             ' AND TotQty > 0 AND cWareCode $ lcAllLoc '+;
                             ' AND OrdHdr.Status $ "OH"'

      =lfUpdTmpB(lcOrdFilt,'1','OrdLine.TotQty','Orders','OrdLine.Start')

    CASE laTrans[1,lnTranCnt] = 'Invoiced'
      SELECT InvHdr
      SET ORDER TO TAG InvHdr
      SELECT InvLine
      SET ORDER TO TAG InvLineS
      SET RELATION TO
      GOTO TOP      
      SET RELATION To Style INTO Style ADDITIVE
      SET RELATION TO Invoice INTO InvHdr ADDITIVE
      lcInvFilt = lcFilter + ' AND BETWEEN(InvDate,ldFromDate,ldToDate)'+;
                              ' AND InvHdr.cWareCode $ lcAllLoc'+;
                              ' AND TotQty > 0 AND InvHdr.Status="C"'
                              
      =lfUpdTmpB(lcInvFilt,'2','InvLine.TotQty','Invoiced','InvLine.InvDate')

    CASE laTrans[1,lnTranCnt] = 'Returns'
      SELECT RetHdr
      SET ORDER TO RetHdr
      SELECT RetLine
      SET ORDER TO RetLineS
      SET RELATION TO
      SET RELATION To Style  INTO Style ADDITIVE
      SET RELATION TO CrMemo INTO RetHdr ADDITIVE
      lcRetFilt = lcFilter + ' AND BETWEEN(CrDate,ldFromDate,ldToDate)'+;
                             ' AND TotQty > 0 AND RetHdr.cWareCode $ lcAllLoc'
      =lfUpdTmpB(lcRetFilt,'3','RetLine.TotQty','Returns','RetLine.CrDate')

    CASE laTrans[1,lnTranCnt] = 'WIP'
      SELECT PosLn
      SET ORDER TO TAG PosLnS
      SET RELATION TO
      SET RELATION To Style  INTO Style ADDITIVE
      SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
      lcWipFilt = lcFilter+ ' AND cWareCode $ lcAllLoc AND TotQty > 0'+;
                            ' AND PosHdr.Status $ "OH"'
      =lfUpdTmpB(lcWipFilt,'4','PosLn.TotQty','WIP','PosHdr.Entered')

    CASE laTrans[1,lnTranCnt] = 'Stock'
**C200137,1
      SELECT StyDye
      SET RELATION TO
      SET RELATION TO Style INTO Style ADDITIVE
      lcStckFilt = lcFilter+ ' AND TotStk <> 0 AND cWareCode $ lcAllLoc'
      =lfUpdTmpB(lcStckFilt,'5','StyDye.TotStk','Stock','gdSysDate')

      *SELECT StyInvJl
      *SET RELATION TO
      *SET RELATION TO Style INTO Style ADDITIVE
      *lcStckFilt = lcFilter+ ' AND !EMPTY(DtrDate) AND nTotStk <> 0 AND cWareCode $ lcAllLoc'
      *=lfUpdTmpB(lcStckFilt,'5','StyInvJl.nTotStk','Stock','StyInvJl.DtrDate')

  ENDCASE
ENDFOR
SELECT Style
SET ORDER TO TAG cStyle

SELECT (lcTmpCurs)
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
INSERT INTO(lcTmpCurs) (cColor,cTranTyp) VALUES('ZZZZZZ','9')
GOTO TOP
=gfDispRe(lcRpFrmId)
SET DEVICE TO SCREEN

USE IN (lcTmpCurs)
ERASE(lcTmpCurs)

*!*************************************************************
*! Name      : lfUpdTmpB
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : The Update the cursor with the selected transactions.
*! Parameter : lcExpr  - > The scan expersion
*!             lcTran   ->  '1'-> Order '2'-> Invoice '3'-> Retuns
*!							'4'-> WIP   '5'-> Stock 
*!             lcTotQty -> OrdLine.TotQty,InvLine.TotQty,RetLine.TotQty
*!                         PosLine.TotQty,StyInvJl.TotQty
*!             lcTranName - > 'Orders','Invoiced','Returns','WIP','Stock'     
*!             ldTranDate - > 'Start','InvDate','CrDate'
*!*************************************************************
*! Example     : = lfUpdTmpB()
*!*************************************************************
FUNCTION lfUpdTmpB
PARAMETER lcExpr,lcTran,lcTotQty,lcTranName,ldChkDate

**C200137,1
IF llStySltd
  lnLinFlAls = SELECT()
  SELECT (lcStyFile)
  SCAN
    lcMajStCd = PADR(cStyMajor,12)
    SELECT(lnLinFlAls)
    =SEEK(lcMajStCd)
    SCAN REST WHILE PADR(Style,12)=lcMajStCd  &lcExpr
      =lfUpdTpBU()
    ENDSCAN
  ENDSCAN
  SELECT(lnLinFlAls)
ELSE
  SCAN &lcExpr
    =lfUpdTpBU()
  ENDSCAN
ENDIF

WAIT CLEAR
*******************
FUNCTION lfUpdTpBU

  WAIT WINDOW lcTranName + ': Style - Color ' + Style NOWAIT
  lcColor  = RIGHT(Style,6)
  lcClrDsc = gfCodDes(lcColor,'COLOR     ')

  IF !SEEK(lcColor+lcTran,lcTmpCurs)
    INSERT INTO (lcTmpCurs);
    (cColor,cClrDesc,cTranTyp,cTransct) VALUES(lcColor,lcClrDsc,lcTran,lcTranName)
  ENDIF

  lcMonth = ALLTRIM(STR(MONTH(&ldChkDate)))

  **C200137,1
  IF lcTran = '5'
    REPLACE &lcTmpCurs..nTotal WITH &lcTmpCurs..nTotal + &lcTotQty
  ELSE
    IF lcTran <> '4'
      REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
              &lcTmpCurs..nMonth&lcMonth + &lcTotQty
      lnClrTot = lnClrTot + &lcTotQty
    ELSE
      IF TranCd $ '16'
        REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
                &lcTmpCurs..nMonth&lcMonth +&lcTotQty
        lnClrTot = lnClrTot + &lcTotQty
      ELSE
        IF TranCd $ '245'
          REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
                  &lcTmpCurs..nMonth&lcMonth - &lcTotQty
          lnClrTot = lnClrTot - &lcTotQty
        ENDIF  
      ENDIF  
    ENDIF
    REPLACE &lcTmpCurs..nTotal WITH &lcTmpCurs..nMonth1+&lcTmpCurs..nMonth2+;
            &lcTmpCurs..nMonth3+&lcTmpCurs..nMonth4+&lcTmpCurs..nMonth5+;
            &lcTmpCurs..nMonth6+&lcTmpCurs..nMonth7+&lcTmpCurs..nMonth8+;
            &lcTmpCurs..nMonth9+&lcTmpCurs..nMonth10+&lcTmpCurs..nMonth11+;
            &lcTmpCurs..nMonth12
  ENDIF


*!*************************************************************
*! Name      : lfvFormC
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : The Collect the data for the selected transactions. 
*!             sort by Account FormC
*!*************************************************************
*! Example     : = lfvFormC()
*!*************************************************************
FUNCTION lfvFormC

*-- Create a temporary cursor to hold the records that matched the
*-- selected criteria.
lcTmpCurs = gfTempName()
CREATE CURSOR (lcTmpCurs);
    (cAccount C(5), cStore C(8), cAccName C(30),cColor C(6), cClrDesc C(30),;
     cTranTyp C(1), nMonth1 N(8), nMonth2 N(8),nMonth3 N(8), nMonth4 N(8),;
     nMonth5 N(8), nMonth6 N(8), nMonth7 N(8),nMonth8 N(8), nMonth9 N(8),;
     nMonth10 N(8), nMonth11 N(8), nMonth12 N(8),nTotal N(9),;
     nPercent N(5,2),cTransct C(10))
INDEX ON cAccount+cStore+cColor+cTranTyp TAG(lcTmpCurs) OF (lcTmpCurs)

*-- If the user selects accounts.
lcAccFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
IF lnPosition > 0
  lcAccFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcAccFile) AND USED(lcAccFile)
    SELECT (lcAccFile)
    GOTO TOP
  ENDIF  
ENDIF


SELECT Style
SET ORDER TO Style

*-- Total for each account/store
lnAccTot = 0

*-- Get the records of the selected transactions.
FOR lnTranCnt = 1 TO ALEN(laTrans,1)
  
  DO CASE
    CASE laTrans[1,lnTranCnt] = 'Orders'
      SELECT OrdHdr
      SET ORDER TO TAG OrdAcct
      SELECT OrdLine
      SET ORDER TO TAG OrdLine      
      SET RELATION TO
      SET RELATION To Style INTO Style ADDITIVE
      SET RELATION TO IIF(EMPTY(Store),'M'+Account,'S'+Account+Store) INTO CUSTOMER ADDITIVE

      lcOrdFilt = 'REST WHILE cOrdType+Order+STR(LineNo,6) = "O"+OrdHdr.Order'+;
                  ' FOR TotQty > 0 AND cWareCode $ lcAllLoc'+;
                  IIF(!EMPTY(lcSeasons),' AND Style.Season $ lcSeasons','')+;
                  IIF(!EMPTY(lcGroups),' AND Style.cStyGroup $ lcGroups','')+;
                  IIF(!EMPTY(lcAllClr),' AND  RIGHT(Style,6) $ lcAllClr','')


      IF !EMPTY(lcAccFile) AND USED(lcAccFile) AND !EOF(lcAccFile)
        SELECT (lcAccFile)
        
        SCAN
          IF SEEK(&lcAccFile..Account,'OrdHdr')
            SELECT OrdHdr
            SCAN REST WHILE Account+cOrdType+Order = &lcAccFile..Account+'O';
                      FOR BETWEEN(Start,ldFromDate,ldToDate) AND Status $ 'OH'
              SELECT OrdLine
              SEEK 'O'+OrdHdr.Order
              =lfUpdTmpC(lcOrdFilt,'1','OrdLine.TotQty','Orders','OrdLine.Start')
            ENDSCAN
          ENDIF
        ENDSCAN
        GOTO TOP
      ELSE
        SELECT OrdHdr
        SCAN FOR !EMPTY(Account) AND BETWEEN(Start,ldFromDate,ldToDate) AND Status $ 'OH'
          SELECT OrdLine
          SEEK 'O'+OrdHdr.Order
          =lfUpdTmpC(lcOrdFilt,'1','OrdLine.TotQty','Orders','OrdLine.Start')
        ENDSCAN        
      ENDIF
            

    CASE laTrans[1,lnTranCnt] = 'Invoiced'
      SELECT InvHdr
      SET ORDER TO InvHdrA
      SELECT InvLine
      SET ORDER TO InvLine
      SET RELATION TO
      SET RELATION To Style INTO Style ADDITIVE
      SET RELATION TO IIF(EMPTY(Store),'M'+Account,'S'+Account+Store) INTO CUSTOMER ADDITIVE
      lcInvFilt = 'REST WHILE Invoice+STR(LineNo,6)= InvHdr.Invoice'+;
                  ' FOR TotQty > 0 AND InvHdr.cWareCode $ lcAllLoc'+;
                  IIF(!EMPTY(lcSeasons),' AND Style.Season $ lcSeasons','')+;
                  IIF(!EMPTY(lcGroups),' AND Style.cStyGroup $ lcGroups','')+;
                  IIF(!EMPTY(lcAllClr),' AND  RIGHT(Style,6) $ lcAllClr','')


      IF !EMPTY(lcAccFile) AND USED(lcAccFile) AND !EOF(lcAccFile)
        SELECT (lcAccFile)
        SCAN
          IF SEEK(&lcAccFile..Account,'InvHdr')
            SELECT InvHdr
            SCAN REST WHILE Account+Invoice = &lcAccFile..Account;
                      FOR BETWEEN(InvDate,ldFromDate,ldToDate) AND Status ='C'
              SELECT InvLine
              SEEK InvHdr.Invoice
              =lfUpdTmpC(lcInvFilt,'2','InvLine.TotQty','Invoiced','InvLine.InvDate')
            ENDSCAN
          ENDIF
        ENDSCAN
        GOTO TOP
      ELSE
        SELECT InvHdr
        SCAN FOR BETWEEN(InvDate,ldFromDate,ldToDate) AND Status = 'C'
          SELECT InvLine
          SEEK InvHdr.Invoice
          =lfUpdTmpC(lcInvFilt,'2','InvLine.TotQty','Invoiced','InvLine.InvDate')
        ENDSCAN        
      ENDIF

    CASE laTrans[1,lnTranCnt] = 'Returns'
      SELECT RetHdr
      SET ORDER TO TAG RetHdrA
      SELECT RetLine
      SET ORDER TO TAG RetLine
      SET RELATION TO
      SET RELATION To Style  INTO Style ADDITIVE
      lcRetFilt = ' REST WHILE CrMemo+Style+Cret_LinNo+Cret_TrnCd=RetHdr.CrMemo'+;
                  ' FOR TotQty > 0 AND RetHdr.cWareCode $ lcAllLoc'+;
                  IIF(!EMPTY(lcSeasons),' AND Style.Season $ lcSeasons','')+;
                  IIF(!EMPTY(lcGroups),' AND Style.cStyGroup $ lcGroups','')+;
                  IIF(!EMPTY(lcAllClr),' AND  RIGHT(Style,6) $ lcAllClr','')


      IF !EMPTY(lcAccFile) AND USED(lcAccFile) AND !EOF(lcAccFile)
        SELECT (lcAccFile)
        SCAN
          IF SEEK(&lcAccFile..Account,'RetHdr')
            SELECT RetHdr
            SCAN REST WHILE Account+CrMemo = &lcAccFile..Account;
                      FOR BETWEEN(CrDate,ldFromDate,ldToDate)
              SELECT RetLine
              SEEK RetHdr.CrMemo
              =lfUpdTmpC(lcRetFilt,'3','RetLine.TotQty','Returns','RetLine.CrDate')
            ENDSCAN
          ENDIF
        ENDSCAN
        GOTO TOP
      ELSE
        SELECT RetHdr
        SCAN FOR BETWEEN(CrDate,ldFromDate,ldToDate)
          SELECT RetLine
          SEEK RetHdr.CrMemo
          =lfUpdTmpC(lcRetFilt,'3','RetLine.TotQty','Returns','RetLine.CrDate')
        ENDSCAN        
      ENDIF
  ENDCASE
ENDFOR
SELECT Style
SET ORDER TO TAG cStyle

SELECT (lcTmpCurs)
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

=gfDispRe(lcRpFrmId)
SET DEVICE TO SCREEN
USE IN (lcTmpCurs)
ERASE(lcTmpCurs)

*!*************************************************************
*! Name      : lfUpdTmpC
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : The Update the cursor with the selected transactions.
*! Parameter : lcExpr  - > The scan expersion
*!             lcTran   ->  '1'-> Order '2'-> Invoice '3'-> Retuns
*!							'4'-> WIP   '5'-> Stock 
*!             lcTotQty -> OrdLine.TotQty,InvLine.TotQty,RetLine.TotQty
*!                         PosLine.TotQty,StyInvJl.TotQty
*!             lcTranName - > 'Orders','Invoiced','Returns','WIP','Stock'     
*!             ldTranDate - > 'Start','InvDate','CrDate'
*!*************************************************************
*! Example     : = lfUpdTmpC()
*!*************************************************************
FUNCTION lfUpdTmpC
PARAMETER lcExpr,lcTran,lcTotQty,lcTranName,ldChkDate

SCAN &lcExpr
  **C200137,1
  IF llStySltd AND ! SEEK(PADR(Style,12),lcStyFile)
    LOOP
  ENDIF

  WAIT WINDOW lcTranName + ': Style - Color ' + Style NOWAIT
  lcColor   = RIGHT(Style,6)
  lcClrDsc  = gfCodDes(lcColor,'COLOR     ')
  lcAccount = Account
  lcStore   = IIF(lcTran $'12',Store,RetHdr.Store)
  IF !SEEK(lcAccount+lcStore+lcColor+lcTran,lcTmpCurs)
    INSERT INTO (lcTmpCurs);
          (cAccount,cStore,cAccName,cColor,cClrDesc,cTranTyp,cTransct);
    VALUES(lcAccount,lcStore,Customer.StName,lcColor,lcClrDsc,lcTran,lcTranName)
  ENDIF

  lcMonth = ALLTRIM(STR(MONTH(&ldChkDate)))
  IF lcTran <> '4'
    REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
            &lcTmpCurs..nMonth&lcMonth + &lcTotQty
  ELSE
    IF TranCd $ '16'
      REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
              &lcTmpCurs..nMonth&lcMonth +&lcTotQty
    ELSE
      IF TranCd $ '245'
        REPLACE &lcTmpCurs..nMonth&lcMonth WITH;
                &lcTmpCurs..nMonth&lcMonth - &lcTotQty
      ENDIF  
    ENDIF  
  ENDIF
  REPLACE &lcTmpCurs..nTotal WITH &lcTmpCurs..nMonth1+&lcTmpCurs..nMonth2+;
          &lcTmpCurs..nMonth3+&lcTmpCurs..nMonth4+&lcTmpCurs..nMonth5+;
          &lcTmpCurs..nMonth6+&lcTmpCurs..nMonth7+&lcTmpCurs..nMonth8+;
          &lcTmpCurs..nMonth9+&lcTmpCurs..nMonth10+&lcTmpCurs..nMonth11+;
          &lcTmpCurs..nMonth12
ENDSCAN
WAIT CLEAR

*!*************************************************************
*! Name      : lfAccTot
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : To get totals for each account. USED IN FRX for Account
*!*************************************************************
*! Example     : = lfAccTot()
*!*************************************************************
FUNCTION lfAccTot
PARAMETERS lcAccCode,lcStoreCod

lnAccTot = 0
SELECT (lcTmpCurs)
lnRecNo = RECNO()

SUM REST nTotal WHILE cAccount+cStore+cColor+cTranTyp = lcAccCode+lcStoreCod;
                TO lnAccTot

IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpCurs))
  GOTO lnRecNo
ENDIF

RETURN ''

*!*************************************************************
*! Name      : lfWhenRep
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : The validation function of the Option Grid.
*!*************************************************************
*! Example     : = lfWhenRep()
*!*************************************************************
FUNCTION lfWhenRep
R_WIDTH = 'W'
IF lnOGSeting = 1
  DIMENSION laRpSource[IIF(lcRpSort<>'A',5,3),1],laRpTarget[1,1],;
            laRpLSourc[1],laRpLTargt[1,1]
  STORE '' TO laRpSource,laRpTarget,laRpLSourc,laRpLTargt

  *-- Filling the source array for all transactions.
  STORE 'Orders'   TO laRpSource[1]
  STORE 'Invoiced' TO laRpSource[2]
  STORE 'Returns'  TO laRpSource[3]
  IF lcRpSort <> 'A'
    STORE 'WIP'      TO laRpSource[4]
    STORE 'Stock'    TO laRpSource[5]
  ENDIF
  *-- Filling the source array for all locations.
  SELECT WareHous
  SELECT DISTINCT cWareCode FROM WareHous INTO ARRAY laRpLSourc
ENDIF
*!*************************************************************
*! Name      : lfvSort
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : The validation function Report format option
*!*************************************************************
*! Example     : = lfvSort()
*!*************************************************************
FUNCTION lfvSort
DO CASE
  CASE lcRpSort = 'S'
    lcRpFrmId = 'ICCLRSLA'
  CASE lcRpSort = 'G'
    lcRpFrmId = 'ICCLRSLD'
  CASE lcRpSort = 'C'
    lcRpFrmId = 'ICCLRSLB'
  CASE lcRpSort = 'A'
    lcRpFrmId = 'ICCLRSLC'
ENDCASE
=lfRepPltFr(lcRpFrmId)

CLEAR READ



*!*************************************************************
*! Name      : lfvDate
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
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
*! Name      : lfvTrans
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : Validate the selection of transactions.
*!*************************************************************
*! Example     : = lfvTrans()
*!*************************************************************
FUNCTION lfvTrans

=gfMover(@laRpSource,@laRpTarget,'Transactions',.T.,'')

*!*************************************************************
*! Name      : lfvLoc
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : Validate the selection of Locations
*!*************************************************************
*! Example     : = lfvLoc()
*!*************************************************************
FUNCTION lfvLoc

= gfMover(@laRpLSourc,@laRpLTargt,'Inventory Location',.T.,'')
