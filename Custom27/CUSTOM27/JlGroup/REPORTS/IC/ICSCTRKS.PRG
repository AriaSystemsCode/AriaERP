*:**************************************************************************************
*: Program file  : ICSCTRKS.PRG
*: Program desc. : Custom tracking report by season C#102234
*: For Report    : (ICSCTRKS.FRX) & (ICSCTRKD.FRX)
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Hend Ghanem (HBG)
*: Date          : 17/06/2001
*:**************************************************************************************
*: B804369,1 HBG 30/08/2001 Fix Some problems with the custom tracking by season report.    
*: B804446,1 HBG 03/10/2001 1- IF the flag Style.lSty_typ is .T. total blank stock order,
*:                             total blank stock received , and units over will be empty. 
*:                          2- The formula of Total Books will be :
*:                             Total books = Total customer bookings.
*:                          3- Remove the Bookings vs PO's column from the frx.
*: B804526,1 BWA 11/27/2001 Change the calculation of the Open PO'S vs open booking.
*: B804526,1                Fix the bug of the booking fields and the total book fields get wrong data.
*: B804540,1 BWA 12/05/2001 Add filter for the location and effects 3 fields as per specs.
*: B804540,1                Get the program to use the settings of the option grid.
*: B606917,1 BWA 01/28/2003 Select Po's only.
*:**************************************************************************************

IF EMPTY(lcDirectry)
  lcDirectry = gcDataDir
ENDIF

*B804540,1 BWA 12/05/2001 Add filter for the location and effects 3 fields as per specs.[START]
*-- Get the Variables its Initial value
STORE SPACE(0) TO lcRpLoc
=lflocation()
*B804540,1 BWA 12/05/2001.[END]

*B804540,1 BWA 12/05/2001 Get the prorgram to use the settings of the option grid.[START]
IF lnOGSeting = 1
  STORE 0 TO lnStyleRec
  llDataFond = .F.

  FOR lnLop = 1 To 10
    lcLop = ALLTRIM(STR(lnLop))

    GOTO TOP IN (lctempst&lcLop)
    IF EMPTY(lcRpColl&lcLop) OR (lcRpStyle&lcLop > 0 AND EOF(lctempst&lcLop)) OR (lcRpStyle&lcLop = 0 AND EOF(lctempst&lcLop))
      lnStyleRec = lnStyleRec + 1
    ENDIF
    
  ENDFOR

  IF lnStyleRec = 10
    *---Text : 'There is a criteria not selected in the option grid.'
    =gfModalGen('INM00000B40011','DIALOG','','','There is a criteria not selected in the option grid.')
    SET DEVICE TO SCREEN
    RETURN
  ENDIF

ELSE
  llDataFond = .T.
  lnDataFond = 0

  FOR lnLop = 1 To 10
    lcLop = ALLTRIM(STR(lnLop))

    GOTO TOP IN (lctempst&lcLop)
    IF !EMPTY(lcRpColl&lcLop) AND !EOF(lctempst&lcLop)
      lnDataFond = lnDataFond + 1
    ENDIF
  ENDFOR

  *--To check if there is data in the other filters or not.
  IF lnDataFond # 0
    llDataFond = .F.
  ENDIF

ENDIF
*B804540,1 BWA 12/05/2001.[END]

IF lcRpPrvVer = '1'

  *B804540,1 BWA 12/05/2001 Add filter for the location and effects 3 fields as per specs.[START]
  SELECT STYLE
  SET RELATION TO STYLE.STYLE INTO STYDYE ADDITIVE
  *B804540,1 BWA 12/05/2001.[END]

  SELECT INVLINE
  SET RELATION TO invoice INTO Invhdr ADDITIVE
  SELECT ORDLINE
  SET RELATION TO 'O'+ Order INTO Ordhdr ADDITIVE
  SELECT POSLN
  SET RELATION TO 'P'+ Po INTO Poshdr ADDITIVE

  DIME laMonth[12,2]
  laMonth[1,1] = 'JAN'
  laMonth[1,2] = 'J'
  laMonth[2,1] = 'FEB'
  laMonth[2,2] = 'F'
  laMonth[3,1] = 'MAR'
  laMonth[3,2] = 'M'
  laMonth[4,1] = 'APR' 
  laMonth[4,2] = 'A' 
  laMonth[5,1] = 'MAY'
  laMonth[5,2] = 'Y'
  laMonth[6,1] = 'JUN'
  laMonth[6,2] = 'N'
  laMonth[7,1] = 'JUL'
  laMonth[7,2] = 'L'
  laMonth[8,1] = 'AUG'
  laMonth[8,2] = 'U'
  laMonth[9,1] = 'SEP'
  laMonth[9,2] = 'S'
  laMonth[10,1] = 'OCT'
  laMonth[10,2] = 'O'
  laMonth[11,1] = 'NOV'
  laMonth[11,2] = 'V'
  laMonth[12,1] = 'DES'
  laMonth[12,2] = 'D'

  *-- Get the name of the file to collect data in 
  lcFileName = UPPER(LEFT(cMonth(DATE()),3))+STRTRAN(STR(DAY(DATE()),2),' ','0')

  IF USED("lcTempFile") AND lcOldDir = lcDirectry
    SELECT lcTempFile
    ZAP
  ELSE
    IF lcOldDir <> lcDirectry
      lcOldDir = lcDirectry
      IF USED("lcTempFile")
        USE IN lcTempFile
      ENDIF
    ENDIF  
    
    llError = .F.
    lcOnError = ON('ERROR')
    ON ERROR llError = .T.
  
    IF FILE(lcDirectry+lcFileName+'.DBF')
      USE lcDirectry+lcFileName+'.DBF'  EXCL IN 0
    ENDIF  
  
    IF llError
      =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,'This report is updated in another session, can not update it.')
      RETURN
    ENDIF
    ON ERROR &lcOnError
    
    *-- Create temp file
    =lfCreatTmp()  
  ENDIF
  *-- Collect data
  =lfCollect()
  
  *-- Begin Print The report
  SELECT lcTempFile
  SET ORDER TO TAG LcFileName
  LOCATE
ELSE
  lnI = EVAL(lcRpPrvVer)
  =gfopenfile(lcDirectry+laDBFS[lnI,1],'','SH',lcTempName,.T.)
  SELECT (lcTempName) 
  SET ORDER TO TAG LcFileName
  LOCATE  
ENDIF

IF lcRpRport = 'D'
  SET FILTER TO UPPER(ALLTRIM(TYPE)) = 'D'
ELSE
  SET FILTER TO UPPER(ALLTRIM(TYPE)) = 'S'
ENDIF
 
DO gfDispRe WITH gcRepHome + 'IC\' + EVALUATE('lcRpName') 

=lfFillDbfs()
lcRpPrvVer = '1'

CLEAR READ

IF FILE(gcDataDir+'PATH.MEM') 
  ERASE(gcDataDir+'PATH.MEM')
ENDIF

SAVE TO gcDataDir+'PATH.MEM' ALL LIKE lcDirectry

IF USED(lcTempName)
  USE IN (lcTempName)
ENDIF


IF laDBFS[1,1] = 'N\A'
  SELECT INVLINE
  SET RELATION TO 
  SELECT ORDLINE
  SET RELATION TO 
  SELECT POSLN
  SET RELATION TO 
ENDIF 

*!*************************************************************
*! Name      : lfCreatTmp
*: Developer : Hend Ghanem (HBG)
*: Date      : 17/06/2001
*! Purpose   : Create temp file
*!*************************************************************
FUNCTION lfCreatTmp

DIMENSION laTempFile[1,4]
laTempFile= ''

laTempFile[1,1] = 'Collec'
laTempFile[1,2] = 'C'
laTempFile[1,3] = 19
laTempFile[1,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'StyMaj'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 19
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Style'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 19
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Desc'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 35
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Note'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 20
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Category'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 10
laTempFile[lnI,4] = 0

*B804369,1 HBG 30/08/2001 Make the width of Class field 20 [Begin]
*lnI = ALEN(laTempFile,1) + 1
*DIMENSION laTempFile[lnI , 4]
*laTempFile[lnI,1] = 'Class'
*laTempFile[lnI,2] = 'C'
*laTempFile[lnI,3] = 10
*laTempFile[lnI,4] = 0
lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Class'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 20
laTempFile[lnI,4] = 0
*B804369,1 [End]

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Color'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 6
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'CustShip'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 4
laTempFile[lnI,4] = 2

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'OpenPOS'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 2

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'StkOnHd'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 6
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'StkOrd'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 6
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'StkRcv'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 6
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'UntOvr'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 6
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'OpenPO'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 2

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'StkOTS'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 6
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Month1'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Month2'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Month3'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Month4'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Month5'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Month6'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Month7'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Month8'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 0

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'CustBook1'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'CustBook2'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'CustBook3'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'CustBook4'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'CustBook5'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'CustBook6'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'CustBook7'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'CustBook8'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'Future'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'TotBook'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'TotShip'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'GrndTot'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 5
laTempFile[lnI,4] = 3

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'UntPric'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 4
laTempFile[lnI,4] = 2

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'TotSale'
laTempFile[lnI,2] = 'N'
laTempFile[lnI,3] = 8
laTempFile[lnI,4] = 2

lnI = ALEN(laTempFile,1) + 1
DIMENSION laTempFile[lnI , 4]
laTempFile[lnI,1] = 'TYPE'
laTempFile[lnI,2] = 'C'
laTempFile[lnI,3] = 1
laTempFile[lnI,4] = 0

CREATE DBF (lcDirectry+lcFileName) FROM ARRAY laTempFile

USE &lcDirectry&lcFileName EXCL ALIAS lcTempFile

INDEX ON TYPE+Collec+StyMaj+Color TAG lcFileName ADDITIVE
INDEX ON StyMaj TAG lcFileIndx ADDITIVE


*!*************************************************************
*! Name      : lfCollect
*: Developer : Hend Ghanem (HBG)
*: Date      : 17/06/2001
*! Purpose   : Collect Date
*!*************************************************************
FUNCTION lfCollect


SELECT lcTempFile
SET ORDER TO TAG lcFileIndx 

lnMonth = ASCAN(laMonth,lcRpStMnth)

IF lnMonth > 0
  lnMonth = ASUBSCRIPT(laMonth,lnMonth ,1)   
ENDIF

STORE 0 To lnFrmMonth , lnToMonth , lnNFMonth , lnNTMonth 

lnFrmMonth = lnMonth
lnToMonth  = lnMonth + 7

IF lnToMonth > 12
  lnNFMonth = 1
  lnNTMonth = lnToMonth - 12
  lnToMonth = 12
ENDIF

FOR lnK = 1 TO 10
  IF lnK < 10
    lcK = STR(lnK,1)
  ELSE
    lcK = STR(lnK,2)
  ENDIF
  IF !EMPTY(lcRpColl&lcK)
    STORE 0 TO lnCustShip , lnOpenPos , lnStkOrd , lnStkRcv , lnUntOvr , lnOpenPo ,;
               lnStkOts , lnTotBook , lnTotShip , lnGrndTot , lnUntPric , lnTotSale,;
               lnTFuture , lnStkOnhd
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      lnCustBok&lcI = 0
    ENDFOR

    *B804540,1 BWA 12/05/2001 Get the prorgram to use the settings of the option grid.[START]
    SELECT(lcTempSt&lcK)
    LOCATE
    IF llDataFond
      IF lnOGSeting > 1 AND EOF(lcTempSt&lcK)
        SELECT lcStting
        SET ORDER TO TAG lcSttng2
        =SEEK(laOGSeting[lnOGSeting,1] + LEFT(lcRpColl&lcK,2))
        SCAN REST WHILE SetingNam + StyFltNm = laOGSeting[lnOGSeting,1] + LEFT(lcRpColl&lcK,2)
          SELECT(lcTempSt&lcK)
          IF !SEEK(lcStting.STYLE)
            APPEND BLANK
            REPLACE STYLE WITH lcStting.STYLE
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    *B804540,1 BWA 12/05/2001.[END]

    lcCurrFile = lcTempSt&lcK
    SELECT (lcCurrFile)
    lcStyMaj = LEFT(STYLE,12)
    SCAN
      SELECT STYLE
      *B804369,1 HBG 30/08/2001 Deale with Style-Color not Style majore [Begin]
      *SET ORDER TO TAG Cstyle
      *=SEEK(&lcCurrFile..CSTYMAJOR)
      SET ORDER TO TAG Style
      =SEEK(&lcCurrFile..STYLE)
      *SET ORDER TO TAG Style
      *B804369,1 [End]

      lnStkOnH = 0            && Variable to collect Stock On hand
      lnTotWip = 0            && Variable to collect WIP

      *B804540,1 BWA 12/05/2001 Add filter for the location and effects 3 fields as per specs.[START]
      lnStkOnHL = 0
      lnTotWipL = 0 
      *B804540,1 BWA 12/05/2001.[END]

      *B804369,1 HBG 30/08/2001 Scan For the selected Style-Color [Begin]
      *SCAN REST WHILE CSTYMAJOR = &lcCurrFile..CSTYMAJOR
      SCAN REST WHILE STYLE = &lcCurrFile..STYLE 
      *B804369,1 [End]

        *B804540,1 BWA 12/05/2001 Add filter for the location and effects 3 fields as per specs.[START]
        lnStkOnH =  STYLE.TotStk
        lnTotWip =  STYLE.TotWip

        IF !EOF('STYDYE')
          lcAliasLoc = SELECT(0)
          SELECT STYDYE
          lcOrder = EVAL(KEY())
          =SEEK(STYLE.STYLE)
          SCAN REST WHILE STYLE + CWARECODE + DYELOT = STYLE.STYLE
            IF STYDYE.CWARECODE $ lcRpLoc
              lnStkOnHL = lnStkOnHL + STYDYE.TotStk
              lnTotWipL = lnTotWipL + STYDYE.TotWip
            ENDIF
          ENDSCAN
          =SEEK(lcOrder , 'STYDYE')
          SELECT(lcAliasLoc)
        ENDIF

        IF EMPTY(lcRpLoc)
          lnStkOnHL = STYLE.TotStk
          lnTotWipL = STYLE.TotWip
        ENDIF
        *B804540,1 BWA 12/05/2001.[END]

        
        lnTotOPO = 0         && Variable to collect Open PO
        lnTotPPO = 0         && Variable to collect budgete PO
        lnTotRPO = 0         && Variable to collect Received PO
        lnTotPPoC = 0        && Variable to collect budgete PO for Complete PO only
        lnTotRPoC = 0        && Variable to collect Received PO for Complete PO only 

        IF SEEK(STYLE.STYLE,'POSLN')
          SELECT POSLN
          SCAN REST WHILE STYLE = STYLE.Style
            *-- Get the budgete Qty
            IF POSLN.TranCD = '1'
              *-- Get the budgete Qty for Complete PO only
              IF POSHDR.Status $ 'C'
                lnTotPPoC = lnTotPPoC + POSLN.TotQty
              ENDIF
              *B606917,1 BWA 01/28/2003 don't Select type "A" in Po's.[START]
              *lnTotPPO = lnTotPPO + POSLN.TotQty
              IF !(POSLN.CSTYTYPE $ 'A')
                lnTotPPO = lnTotPPO + POSLN.TotQty
              ENDIF
              *B606917,1 BWA 01/28/2003.[END]
            ENDIF
            *-- Get the Received Qty
            IF POSLN.TranCD = '2'
              *-- Get the Received Qty for Complete PO only
              IF POSHDR.Status $ 'C'
                lnTotRPoC = lnTotRPoC + POSLN.TotQty
              ENDIF
              *B606917,1 BWA 01/28/2003 don't Select type "A" in Po's.[START]
              *lnTotRPO = lnTotRPO + POSLN.TotQty
              IF !(POSLN.CSTYTYPE $ 'A')
                lnTotRPO = lnTotRPO + POSLN.TotQty
              ENDIF
              *B606917,1 BWA 01/28/2003.[END]
            ENDIF
            *-- Get the Open Qty
            IF POSHDR.Status $ 'OHA' 
              IF POSLN.TranCD = '1'
                lnTotOPO = lnTotOPO + POSLN.TotQty
              ENDIF
              IF POSLN.TranCD = '2' OR POSLN.TranCD = '4' OR POSLN.TranCD = '5' 
                lnTotOPO = lnTotOPO - POSLN.TotQty
              ENDIF
            ENDIF
          ENDSCAN
        ENDIF

        lnTotShp = 0        && Variable to collect Shipped Qty
        IF SEEK(STYLE.Style,'INVLINE')
          SELECT INVLINE
          SCAN REST WHILE STYLE = STYLE.Style
            *-- Get the Shipped Qty
            IF INVHDR.Status <> 'V'
              lnTotShp = lnTotShp + INVLINE.TotQty
            ENDIF
            *-- Get the Shipped Qty after exclude Voided Qty
            IF INVHDR.Status = 'V'
              lnTotShp = lnTotShp - INVLINE.TotQty
            ENDIF
          ENDSCAN
        ENDIF
        
        lnTotOrd = 0       && Variable to collect Open SO Qty

        *B804540,1 BWA 12/05/2001 Add filter for the location and effects 3 fields as per specs.[START]
        lnTotOrdL = 0
        *B804540,1 BWA 12/05/2001.[END]

        *B804446,1 HBG 03/10/2001 Fix bug of variabel not found [Begin]
        *FOR lnI = 1 TO 8
          *lcI = STR(lnI,1)
          *lnTotCBok&lcI = 0
        FOR lnI = 1 TO 12
          lcI = STR(lnI,LEN(ALLTRIM(STR(lnI))))
          lnTotCBk&lcI = 0
          *B804446,1 [End]
        ENDFOR
        lnFuture = 0

        IF SEEK(STYLE.Style,'ORDLINE')
          SELECT ORDLINE
          lnI = 1
          lnOldMonth = MONTH(ORDHDR.Start)

          SCAN REST WHILE STYLE = STYLE.Style
            *-- Get the Open SO Qty

            IF ORDHDR.Status $ 'OH'
              lnTotOrd = lnTotOrd + ORDLINE.TotQty
            ENDIF

            *B804540,1 BWA 12/05/2001 Add filter for the location and effects 3 fields as per specs.[START]
            IF ORDHDR.Status $ 'OH' AND ORDLINE.CWARECODE $ lcRpLoc
              lnTotOrdL = lnTotOrdL + ORDLINE.TotQty
            ENDIF
            *B804540,1 BWA 12/05/2001.[END]

            *-- Get the Customer Booking
            
            lnMonth = MONTH(ORDHDR.Start)
            IF lnMonth < 10
              lcMonth = STR(lnMonth,1)
            ELSE
              lcMonth = STR(lnMonth,2)
            ENDIF
            
            lnYear = YEAR(ORDHDR.Start)
            
            IF lnMonth >= lnFrmMonth AND lnMonth <= lnToMonth  AND ORDHDR.Status $ 'OH' ;
               AND lnYear = YEAR(DATE())

              *B804526,1 BWA 11/27/2001 Fix the bug of the booking fields and the total book fields get wrong data.[START]
              *lnI = IIF(lnFrmMonth = lnMonth ,lnMonth ,lnMonth - lnFrmMonth + 1)
              lnI = IIF(lnFrmMonth = lnMonth , 1 ,lnMonth - lnFrmMonth + 1)
              *B804526,1 BWA 11/27/2001.[END]

              *B804446,1 HBG 03/10/2001 Fix bug of unrecognized command verp [Begin]
              *lcI = STR(lnI,1)
              *lnTotCBok&lcI = lnTotCBok&lcI+ ORDLINE.TotQty
              lcI = STR(lnI,LEN(ALLTRIM(STR(lnI))))
              lnTotCBk&lcI = lnTotCBk&lcI+ ORDLINE.TotQty
              *B804446,1 [End]
            ELSE 
              IF lnNFMonth <> 0 AND lnMonth >= lnNFMonth AND lnMonth <= lnNTMonth AND ;
                 ORDHDR.Status $ 'OH' AND lnYear = (YEAR(DATE()) + 1)

                lnI = IIF(lnNFMonth = lnMonth ,lnMonth ,lnMonth - lnNFMonth + 1)
                *B804446,1 HBG 03/10/2001 Fix bug of unrecognized command verp [Begin]
                *lcI = STR(lnI,1)            
                *lnTotCBok&lcI = lnTotCBok&lcI + ORDLINE.TotQty
                lcI = STR(lnI,LEN(ALLTRIM(STR(lnI))))            
                lnTotCBk&lcI = lnTotCBk&lcI + ORDLINE.TotQty
                *B804446,1 [End]
              ELSE

                *B804526,1 BWA 11/27/2001 Fix the bug of the booking fields and the total book fields get wrong data.[START]
                *lnFuture  = lnFuture + ORDLINE.TotQty
                IF ORDHDR.Status $ 'OH'
                  lnFuture  = lnFuture + ORDLINE.TotQty
                ENDIF
                *B804526,1 BWA 11/27/2001.[END]

              ENDIF
            ENDIF
          ENDSCAN
        ENDIF
        
        lnMonth = lnFrmMonth
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          m.Month&lcI= laMonth[lnMonth,1]
          lnMonth = lnMonth + 1 
          IF lnMonth > 12
            lnMonth  = lnNFMonth 
          ENDIF  
        ENDFOR              
  
        m.Collec   = ALLTRIM(lcRpColl&lcK)
        m.StyMaj   = Style.CSTYMAJOR
        m.Style    = Style.Style
        m.Color    = RIGHT(Style.Style,6)
        m.Desc     = Style.Desc
        m.Class    = Style.CSTYCLASS 
        m.Category = gfCodDes(STYLE.cstygroup,'CSTYGROUP')
 
        *-- (Total shipped qty excluding the voided invoices + total open qty of 
        *-- open and on hold sales orders) / (total open  qty of PO's with status open, 
        *-- on hold or actualized + total received qty from all the PO's) * 100
        IF lnTotOPO + lnTotRPO <> 0
          m.CustShip = (lnTotShp + lnTotOrd) / (lnTotOPO + lnTotRPO ) * 100
        ELSE
          m.CustShip = 0  
        ENDIF  
        lnCustShip = lnCustShip + m.CustShip 
        *-- (Total open PO's  + stock on hand) / (total open qty of open or on 
        *-- hold sales orders) * 100
        IF lnTotOrd <> 0

          *B804526,1 BWA 11/27/2001 Change the calculation of the Open PO'S vs open booking.[START]
          *m.OpenPos  = ((lnTotOPO + lnStkOnH) / lnTotOrd ) * 100
          m.OpenPos  = ((lnTotOPO + lnStkOnH) / lnTotOrd )
          *B804526,1 BWA 11/27/2001.[END]

        ELSE
          m.OpenPos  = 0
        ENDIF  
        lnOpenPos  = lnOpenPos  + m.OpenPos  
        *-- Stock on hand

        *B804540,1 BWA 12/05/2001 Add filter for the location and effects 3 fields as per specs.[START]
        *m.StkOnhd  = lnStkOnH
        m.StkOnhd  = lnStkOnHL
        *B804540,1 BWA 12/05/2001.[END]

        lnStkOnhd  = lnStkOnhd  + m.StkOnhd  
        
        *B804446,1 HBG 03/10/2001 IF the flag Style.lSty_typ is .T. total blank stock order,
        *B804446,1                total blank stock received , and units over will be empty [Begin]
        *-- Summation of the budgeted qty of all the PO's created for the style. 
        *m.StkOrd   = lnTotPPO 
        *lnStkOrd = lnStkOrd + m.StkOrd
        *-- Summation of the received qty from PO's 
        *m.StkRcv   = lnTotRPO 
        *lnStkRcv   = lnStkRcv + m.StkRcv
        *-- Summation of the qty received - qty budgeted for only the PO's with status 
        *-- as complete.
        *m.UntOvr   = lnTotRPoC - lnTotPPoC 
        *lnUntOvr   = lnUntOvr  + m.UntOvr    
        
        *-- Summation of the budgeted qty of all the PO's created for the style. 
        m.StkOrd   = IIF(Style.lSty_typ,0,lnTotPPO )
        lnStkOrd = lnStkOrd + m.StkOrd
        *-- Summation of the received qty from PO's 
        m.StkRcv   = IIF(Style.lSty_typ,0,lnTotRPO )
        lnStkRcv   = lnStkRcv + m.StkRcv
        *-- Summation of the qty received - qty budgeted for only the PO's with status 
        *-- as complete.
        m.UntOvr   = IIF(Style.lSty_typ,0,lnTotRPoC - lnTotPPoC )
        lnUntOvr   = lnUntOvr  + m.UntOvr    
        *B804446,1 [End]
        
        *-- Summation of the open quantity of the open/on hold/actualized PO's 
        m.OpenPo   = lnTotOPO 
        lnOpenPo   = lnOpenPo   + m.OpenPo

        *-- Stock on hand + WIP - open orders

        *B804540,1 BWA 12/05/2001 Add filter for the location and effects 3 fields as per specs.[START]
        *m.StkOts   = lnStkOnH + lnTotWip - lnTotOrd
        m.StkOts   = lnStkOnHL + lnTotWipL - lnTotOrdL
        *B804540,1 BWA 12/05/2001.[END]

        lnStkOts   = lnStkOts  + m.StkOts   

        *B804446,1 HBG 03/10/2001 the formula of Total Books will be :
        *B804446,1                Total books = Total customer bookings [Begin] 
        *-- Total open qty of on hold or open sales order
        *m.TotBook  = lnTotOrd
        *lnTotBook  = lnTotBook  + m.TotBook  

        m.TotBook = 0
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          *B804446,1 HBG 03/10/2001 Fix bug of unrecognized command verp [Begin]
          *m.CustBook&lcI = lnTotCBok&lcI
          *lnCustBok&lcI = lnCustBok&lcI + lnTotCBok&lcI
          m.CustBook&lcI = lnTotCBk&lcI
          lnCustBok&lcI = lnCustBok&lcI + lnTotCBk&lcI
          *B804446,1 [End]
          *-- Total open qty of on hold or open sales order
          m.TotBook  = m.TotBook  + m.CustBook&lcI
          lnTotBook  = lnTotBook  + m.TotBook 
        ENDFOR
        
        m.Future   = lnFuture
        lnTFuture  = lnTFuture  + lnFuture
        
        *-- Total open qty of on hold or open sales order
        m.TotBook  = m.TotBook + m.Future
        lnTotBook  = lnTotBook  + m.TotBook 
        *B804446,1 [End]

        *-- Total qty shipped after excluding the voided invoices
        m.TotShip  = lnTotShp 
        lnTotShip  = lnTotShip + m.TotShip  
        
        m.GrndTot  = m.TotBook + m.TotShip
        lnGrndTot  = lnGrndTot + m.GrndTot  
        *-- Style unit price (Level A)
        m.UntPric  = Style.Pricea
        lnUntPric  = lnUntPric  + m.UntPric  
        
        m.TotSale  = m.GrndTot * m.UntPric
        lnTotSale  = lnTotSale  + m.TotSale  
        
        m.TYPE = 'D'
       
        *B804446,1 HBG 03/10/2001 move this block of code up To allow calculating Total books  
        *B804446,1                which will be Total books = Total customer bookings [Begin] 
        *FOR lnI = 1 TO 8
        *  lcI = STR(lnI,1)
        *  m.CustBook&lcI = lnTotCBok&lcI
        *  lnCustBok&lcI = lnCustBok&lcI + lnTotCBok&lcI
        *ENDFOR
        
        *m.Future   = lnFuture
        *lnTFuture  = lnTFuture  + lnFuture
        *B804446,1 [End]
       
        IF lcStyMaj <> m.StyMaj AND SEEK(m.StyMaj,'lcTempFile')
          m.Note = 'Continuing Style'
          lcStyMaj = m.StyMaj   
        ELSE
          m.Note = ''  
        ENDIF   
        
        INSERT INTO lcTempFile FROM MEMVAR
        SELECT STYLE
      ENDSCAN
      SELECT (lcCurrFile)
    ENDSCAN
    SELECT lcTempFile
    APPEND BLANK
    REPLACE Collec   WITH ALLTRIM(lcRpColl&lcK),;
            TYPE     WITH 'S',;
            CustShip WITH lnCustShip,;
            OpenPos  WITH lnOpenPos ,;
            StkOnhd  WITH lnStkOnhd ,;
            StkOrd   WITH lnStkOrd  ,;
            StkRcv   WITH lnStkRcv  ,;
            UntOvr   WITH lnUntOvr  ,;
            OpenPo   WITH lnOpenPo  ,;
            StkOts   WITH lnStkOts  ,;
            TotBook  WITH lnTotBook ,;
            TotShip  WITH lnTotShip ,;
            GrndTot  WITH lnGrndTot ,;
            UntPric  WITH lnUntPric ,;
            TotSale  WITH lnTotSale ,;
            Future   WITH lnTFuture  
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      REPLACE CustBook&lcI WITH lnCustBok&lcI 
    ENDFOR
    
    lnMonth = lnFrmMonth
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      REPLACE Month&lcI WITH laMonth[lnMonth,1]
      lnMonth = lnMonth + 1 
      IF lnMonth > 12
        lnMonth  = lnNFMonth 
      ENDIF  
    ENDFOR              

  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfRepWhen
*: Developer : Hend Ghanem (HBG)
*: Date      : 17/06/2001
*! Purpose   : When Function for the report
*!*************************************************************
FUNCTION lfRepWhen

*-- Get the DBFs in this Directory.

=lfOGShowGet('lcRpPrvVer')

FOR lnI = 1 TO 10
  lcI = STR(lnI,LEN(ALLTRIM(STR(lnI))))
  IF !FILE(gcWorkDir + lcTempSt&lcI + '.DBF')
    *B804369,1 HBG 30/08/2001 Use Style-Color not Style majore to allow select individual 
    *B804369,1                colors rather than selecting the entire style[Begin]
    *CREATE DBF (gcWorkDir+lcTempSt&lcI) (CSTYMAJOR C(19))
    *INDEX ON CSTYMAJOR TAG (lcTempSt&lcI) OF (gcWorkDir+lcTempSt&lcI+'.CDX')
    CREATE DBF (gcWorkDir+lcTempSt&lcI) (STYLE C(19))
    INDEX ON STYLE TAG (lcTempSt&lcI) OF (gcWorkDir+lcTempSt&lcI+'.CDX')
    *B804369,1 [End]
  ENDIF  
ENDFOR

*B804540,1 BWA 12/05/2001 Get the prorgram to use the settings of the option grid.[START]
IF !FILE(gcWorkDir + 'lcStting' + '.DBF')
  CREATE DBF (gcWorkDir + 'lcStting') (STYLE C(19) , SetingNam C(40) , StyFltNm C(2))

  INDEX ON STYLE + SetingNam + StyFltNm TAG 'lcStting' OF (gcWorkDir + 'lcStting' + '.CDX')
  INDEX ON SetingNam + StyFltNm         TAG 'lcSttng2' OF (gcWorkDir + 'lcStting' + '.CDX')
ELSE
  IF !USED('lcStting')
    USE (gcWorkDir + 'lcStting') IN 0 ALIAS lcStting ORDER lcStting
  ENDIF
ENDIF
*B804540,1 BWA 12/05/2001.[END]

*!*************************************************************
*! Name      : lfFillDbfs
*: Developer : Hend Ghanem (HBG)
*: Date      : 17/06/2001
*! Purpose   : Function to fill the arrays of the DBFS Popup
*!*************************************************************
FUNCTION lfFillDbfs

llNoDBFs = .F.    

IF EMPTY(lcDirectry)
  lcDirectry = gcDataDir   
ENDIF  

IF EMPTY(lcDirectry)
  llNoDBFs = .T.    
ELSE
  llNoDBFs = .F.    
ENDIF
DIME laDBFS[1,1] , laSlDBFS[1,1]
laDBFS[1,1] = 'N\A'

IF !llNoDBFs
  DIME laDummy[1,1]
  STORE '' TO laDummy
  = ADIR(laDummy,lcDirectry+'*.DBF')

  FOR lnI = 1 TO ALEN(laDummy,1) 
    IF LEFT(laDummy[lnI,1],3) $ "JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DES" AND SUBSTR(laDummy[lnI,1],4,1) $ "0|1|2|3|4|5|6|7|8|9"
      lnJ = ALEN(laDBFS,1) + 1
      DIME laDBFS[ALEN(laDBFS,1) + 1,1]
      laDBFS[lnJ,1] = laDummy[lnI,1]
    ENDIF  
  ENDFOR

  DIME laSlDBFS[ALEN(laDBFS,1),1]

  FOR lnI = 1 TO ALEN(laDBFS,1)
    lcI = STR(lnI,LEN(ALLTRIM(STR(lnI))))
    laSlDBFS[lnI,1] = lcI
  ENDFOR
ENDIF

*!*************************************************************
*! Name      : lfvStyle
*: Developer : Hend Ghanem (HBG)
*: Date      : 17/06/2001
*! Purpose   : Brow the style inrange and modify the filter.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
PARAMETERS lcColl

PRIVATE lcAliasSty , lcAliasSet , lcAliasAro

lcAliasSty = ALIAS()
STORE SPACE(0) TO lcExpsty , lcStyColor
STORE 0 TO lnStyColor

DIMENSION laStySeg[1]
STORE SPACE(0) TO laStySeg

=gfItemMask(@laStySeg)

FOR lnCount = 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCount,1]='F'
    lnStPos = LEN(laStySeg[lnCount,3])
    EXIT
  ENDIF
ENDFOR

lcFile = lcTempSt&lcColl

SELECT (lcFile)

*B804369,1 HBG 30/08/2001 Allow select individual colors rather than selecting the entire style[Begin]
*lcBrowFlds = [CSTYMAJOR :H = 'Style'       :25 ,]    + ;
             [DESC      :H = 'Description' :25 ,]    + ;
             [SEASON    :H = 'Season'      :15 ,]    + ;
             [CDIVISION :H = 'Division'    :15 ,]    + ;
             [PRICEA    :H = 'Price'       :10 ]
lcBrowFlds = [STYLE     :H = 'Style'       :25 ,]    + ;
             [DESC      :H = 'Description' :25 ,]    + ;
             [SEASON    :H = 'Season'      :15 ,]    + ;
             [CDIVISION :H = 'Division'    :15 ,]    + ;
             [PRICEA    :H = 'Price'       :10 ]
*B804369,1 [End]             
*--THE TEMPORARY INDEX.
SELECT STYLE

*B804369,1 HBG 30/08/2001 Allow select individual colors rather than selecting the entire style[Begin]
*SET ORDER TO CSTYLE
SET ORDER TO STYLE
*B804369,1 [End]
LOCATE

*B804369,1 HBG 30/08/2001 Allow select individual colors rather than selecting the entire style[Begin]
*=gfrange(lcBrowFlds,lcTempSt&lcColl,"CSTYMAJOR ","","","","@! XXXXXXXXXXXX")

*B804540,1 BWA 12/05/2001 Get the prorgram to use the settings of the option grid.[START]
IF lnOGSeting > 1
  GOTO TOP IN (lctempst&lcColl)
  IF RECNO(lctempst&lcColl) > 0 AND EOF(lctempst&lcColl)
    lcAliasAro = SELECT(0)
    SELECT lcStting
    SET ORDER TO TAG lcSttng2
    =SEEK(laOGSeting[lnOGSeting,1] + LEFT(lcRpColl&lcColl,2))
    SCAN REST WHILE SetingNam + StyFltNm = laOGSeting[lnOGSeting,1] + LEFT(lcRpColl&lcColl,2)
      SELECT(lcTempSt&lcColl)
      IF !SEEK(lcStting.STYLE)
        APPEND BLANK
        REPLACE STYLE WITH lcStting.STYLE
      ENDIF
    ENDSCAN
    SELECT(lcAliasAro)
  ENDIF
ENDIF
*B804540,1 BWA 12/05/2001.[END]

=gfrange(lcBrowFlds,lcTempSt&lcColl,"STYLE","","","","@! XXXXXXXXXXXX-XXXXXX")
*B804369,1 [End]

DIMENSION laStylFle[1,7]
*B804369,1 HBG 30/08/2001 Allow select individual colors rather than selecting the entire style[Begin]
*laStylFle[1,1]= "STYLE.CSTYMAJOR"
laStylFle[1,1]= "STYLE.STYLE"
*B804369,1 [End]
laStylFle[1,2]= "F"
laStylFle[1,3]= "C"
laStylFle[1,4]= .T.
laStylFle[1,5]= "In List             "
laStylFle[1,6]= lcTempSt&lcColl
laStylFle[1,7]= "R"

DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]
=ACOPY(laBrFldFlt,laBrTmpFlt)

DIMENSION laBrFldFlt[2,ALEN(laBrTmpFlt,2)]
laBrFldFlt = ""
*B804369,1 HBG 30/08/2001 Allow select individual colors rather than selecting the entire style[Begin]
*laBrFldFlt[2,1] = "CSTYMAJOR"
laBrFldFlt[2,1] = "STYLE"
*B804369,1 [End]
laBrFldFlt[2,2] = lcBrowFlds
laBrFldFlt[2,3] = ""
laBrFldFlt[2,4] = ""
laBrFldFlt[2,5] = "laStylFle:1"

lcStyColor = gfGenFlt('laStylFle',.T.,.T.)

DIMENSION laBrFldFlt[ALEN(laBrTmpFlt,1),ALEN(laBrTmpFlt,2)]
=ACOPY(laBrTmpFlt,laBrFldFlt)

SELECT (lcAliasSty)

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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

llClearFn = .T.
*-- Close temp. opended files, if it used.

FOR lnI = 1 TO 10
  lcI = STR(lnI,LEN(ALLTRIM(STR(lnI))))

  IF USED(lcTempSt&lcI)
    *wab
    *USE IN (lcTempSt&lcI)
    USE IN EVAL('lcTempSt'+lcI)
    

    IF FILE(gcWorkDir+EVAL('lcTempSt'+lcI)+'.DBF') 
      ERASE(gcWorkDir+EVAL('lcTempSt'+lcI)+'.DBF')
    ENDIF

    IF FILE(gcWorkDir+EVAL('lcTempSt'+lcI)+'.CDX') 
      ERASE(gcWorkDir+EVAL('lcTempSt'+lcI)+'.CDX')
    ENDIF
  ENDIF
ENDFOR

*B804540,1 BWA 12/05/2001 Get the prorgram to use the settings of the option grid.[START]
IF USED('lcStting')
  SELECT lcStting
  IF gfopenfile( gcWorkDir + 'lcStting' ,'','EX')
    PACK
  ENDIF
  USE IN lcStting
ENDIF
*B804540,1 BWA 12/05/2001.[END]

*!*************************************************************
*! Name      : lfGetDir
*: Developer : Hend Ghanem (HBG)
*: Date      : 17/06/2001
*! Purpose   : Get the report directory
*!*************************************************************
FUNCTION lfGetDir

lcDirectry = GETDIR()

=lfFillDbfs()
lcRpPrvVer = '1'

CLEAR READ

*!*************************************************************
*! Name      : lfClrRed
*: Developer : Hend Ghanem (HBG)
*: Date      : 17/06/2001
*! Purpose   : Change report name
*!*************************************************************
FUNCTION lfClrRed

IF lcRpRport = 'D'
  lcRpName = 'ICSCTRKS'
ELSE
  lcRpName = 'ICSCTRKD'
ENDIF

*!*************************************************************
*! Name      : lfFillARAY
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 11/27/2001
*! Purpose   : Function to fill Terms,Division,Region,Ship via,
*!           : Special Instruction,Customer classification,Seasons
*!           : Style Group, Royalty,Purchasing group.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillARAY()
*!*************************************************************
FUNCTION lfFillARAY

DIMENSION laRpSLoc[1,1],laRpTLoc[1,1]

STORE '' TO laRpSLoc,laRpTLoc

SELECT WAREHOUS
SELECT DISTINCT CWARECODE FROM WAREHOUS INTO ARRAY laRpSLoc

*--End of lfFillARAY.
*!*************************************************************
*! Name      : lfvLoc
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 11/27/2001
*! Purpose   : Function to call the mover of the location.
*!*************************************************************
*! Calls     : 
*! Functions : gfMover()
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Example   : =lfvLoc()
*!*************************************************************
FUNCTION lfvLoc
= gfMover(@laRpSLoc,@laRpTLoc,'Locations',.T.,'')

*--End of lfvLoc.
*!*************************************************************
*! Name      : lfvCratVar
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 11/27/2001
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lflocation

*-- The Locations.
FOR lnInd = 1 TO ALEN(laRpTLoc)
  lcRpLoc = lcRpLoc + PADR(laRpTLoc[lnInd],6) + ' | '
ENDFOR
lcRpLoc = IIF(ALLTRIM(lcRpLoc) = '|','',lcRpLoc)

*-- End of lfvCrATVar.




*B804540,1 BWA 12/05/2001 Get the prorgram to use the settings of the option grid.[START]
*:************************************************************************
*: Program file  : LFOGSAVE.PRG
*: Program desc. : 
*: F	or screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
FUNCTION LFOGSAVE
STORE .F. TO llOGRepFlt,llOGSetDef,llOGAccByAll
STORE '' TO lcOGFltDes,lcOGRepFlt

lcOGSelect=SELECT()

IF !USED('SYURPFLT')
  SELECT 0
  USE &gcSysHome.SYURPFLT ORDER 1
  llOGRepFlt=.T.
ELSE
  SELECT SYURPFLT
ENDIF
lcOGRepFlt=IIF(!llOGSysData,lcOGFlt_ID,'')
IF !EMPTY(lcOGRepFlt)
  IF SEEK(ALLTRIM(lcOGManRep)+gcUser_ID+lcOGRepFlt)
    lcOGFltDes=cFltr_Des
    llOGSetDef=lDefault
  ELSE
    IF SEEK(ALLTRIM(lcOGManRep))
      LOCATE REST WHILE ALLTRIM(crep_id)+cuser_id+cfltr_id = ALLTRIM(lcOGManRep);
                  FOR   cfltr_id = lcOGRepFlt AND lAccByAll
      IF FOUND()
        lcOGFltDes=cFltr_Des
      ENDIF            
    ENDIF 
  ENDIF
ENDIF
llNewId = .F.
llAccAlUsr= IIF(!EMPTY(lcOGRepFlt),lfIsAcc(1),.F.)
llAccAllU = IIF(!EMPTY(lcOGRepFlt),IIF(lAccByAll OR llAccAlUsr,1,0),0)

DO syOGSave.spr

*B804540,1 BWA 12/05/2001 Get the prorgram to use the settings of the option grid.[START]
PRIVATE lcAliasSet
lcAliasSet = SELECT(0)
IF lnOGSeting > 1
  FOR lnLoop = 1 To 10
    llEntrToDl = .T.
    lcColl = ALLTRIM(STR(lnLoop))
    SELECT(lcTempSt&lcColl)
    LOCATE
    IF !EOF()
      SCAN
        IF llEntrToDl
          SELECT lcStting
          SET ORDER TO TAG lcSttng2
          DELETE FOR SetingNam + StyFltNm = laOGSeting[lnOGSeting,1] + LEFT(lcRpColl&lcColl,2)
          SET ORDER TO TAG lcStting
          llEntrToDl = .F.
        ENDIF

        IF !SEEK(EVAL(lcTempSt&lcColl+'.STYLE') + laOGSeting[lnOGSeting,1] + LEFT(lcRpColl&lcColl,2) , 'lcStting')
          SELECT lcStting
          APPEND BLANK
          REPLACE lcStting.SetingNam WITH laOGSeting[lnOGSeting,1]       ,;
                  lcStting.StyFltNm  WITH LEFT(lcRpColl&lcColl,2)        ,;
                  lcStting.STYLE     WITH EVAL(lcTempSt&lcColl+'.STYLE')
        ENDIF
      ENDSCAN
    ENDIF
  ENDFOR
ENDIF
SELECT(lcAliasSet)
*B804540,1 BWA 12/05/2001.[END]

SELECT(lcOGSelect)
IF _DOS
  SHOW GETS OFF
ELSE
  SHOW GETS ONLY
ENDIF  


FUNCTION lfvSave

IF (!EMPTY(lcOGRepFlt) AND !EMPTY(lcOGFltDes)) 

    llNewId = (cfltr_id <> lcOGRepFlt)
    llCanSeek=SEEK(ALLTRIM(lcOGManRep)+PADR(gcUser_ID,LEN(CUSER_ID))+PADR(lcOGRepFlt,4))
  
  IF llCanSeek
    IF gfModalGen("INM00254B00006",'ALERT') = 2
      _CUROBJ = OBJNUM(lcOGRepFlt)
      RETURN
    ENDIF
    IF llOGSetDef
      REPLACE ALL lDefault WITH .F.;
          FOR CREP_ID = lcOGManRep;
            AND CUSER_ID = gcUser_ID 
    ENDIF      
    =SEEK(ALLTRIM(lcOGManRep)+PADR(gcUser_ID,LEN(CUSER_ID))+PADR(lcOGRepFlt,4))    
  ELSE
    IF llOGSetDef
      REPLACE ALL lDefault WITH .F.;
          FOR CREP_ID = lcOGManRep;
            AND CUSER_ID = gcUser_ID 
    ENDIF    
    APPEND BLANK
  ENDIF
  REPLACE CREP_ID WITH lcOGManRep;
          cUser_ID WITH gcUser_ID;
          cFltr_ID WITH lcOGRepFlt;
          lDefault WITH llOGSetDef;
          lAccByAll WITH llOGAccByAll;
          cfltr_des WITH lcOGFltDes;
          lSelect   WITH llOGFilter;
          lSys_Data WITH .F.

   lcKey = EVAL(KEY())
   DO CASE
     CASE llAccAlUsr AND llAccAllU = 0
       *--Was Access by all and no longer : delete for all except current user.
       =lfIsAcc(3)
     CASE llAccAllU = 1
       *--Was Access by all and still, OR Wasn't Access by all and becomes: update all.
       =lfIsAcc(2)
   ENDCASE
   =SEEK(lcKey)
   lcOGSeting = RECNO()
   REPLACE lAccByAll WITH IIF(!llCanSeek AND llAccAlUsr AND !llNewId ,.F.,IIF(llAccAllU = 1,.T.,.F.)) 
    lcOGSeting = RECNO()           
    DIMENSION laOGUDefVr[1,2]        
    STORE '' TO laOGUDefVr
    =lfSaveUDefV('laOGUDefVr')           
    IF !EMPTY(laOGUDefVr[1,1])
      SAVE TO MEMO mUsrDVar ALL LIKE laOGUDefVr
    ENDIF
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
    CLEAR READ      
    =lfGetSetting() 
    lnOGSeting = ASCAN(lAOGSeting,lcOGSeting)    
    lnOGSeting = IIF(lnogSeting<>0,ASUBSCRIPT(laOGSeting,lnogSeting,1),1)
    =lfwChangeset()
  ENDIF


FUNCTION lfOGSaveShow
IF (!EMPTY(lcOGRepFlt) AND !EMPTY(lcOGFltDes)) 
  SHOW GET pbSave ENABLE
ELSE
  SHOW GET pbSave DISABLE
ENDIF

*:************************************************************************
*: Program file  : lfIsAcc.PRG
*: Program desc. : Is there any setting for the current report "Access fro all users"?
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: BASSEM RAFAAT ERNEST (BWA)
*:************************************************************************
FUNCTION lfIsAcc
PARAMETERS lnUpdate

PRIVATE lcKey
lcKey = EVAL(KEY())
DO CASE
  CASE lnUpdate = 1
    IF SEEK(ALLTRIM(lcOGManRep))
       LOCATE REST WHILE ALLTRIM(crep_id)+cuser_id+cfltr_id = ALLTRIM(lcOGManRep);
                   FOR   cfltr_id = lcOGRepFlt AND lAccByAll
    ENDIF 
    llFound = FOUND()
    =SEEK(lcKey)  
    RETURN llFound
  CASE lnUpdate = 2
    IF SEEK(ALLTRIM(lcOGManRep))
      SCAN WHILE ALLTRIM(crep_id)+cuser_id+cfltr_id = ALLTRIM(lcOGManRep);
                      FOR   cfltr_id = lcOGRepFlt
        lcOGSeting = RECNO()           
        DIMENSION laOGUDefVr[1,2]        
        STORE '' TO laOGUDefVr
        =lfSaveUDefV('laOGUDefVr')           
        IF !EMPTY(laOGUDefVr[1,1])
          SAVE TO MEMO mUsrDVar ALL LIKE laOGUDefVr
        ENDIF
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
        REPLACE lAccByAll WITH IIF(lAccByAll,IIF(llAccAllU = 1,.T.,.F.),lAccByAll)
      ENDSCAN  
    ENDIF
  CASE lnUpdate = 3
    IF SEEK(ALLTRIM(lcOGManRep))
      DELETE REST WHILE ALLTRIM(crep_id)+cuser_id+cfltr_id = ALLTRIM(lcOGManRep);
                      FOR   cfltr_id = lcOGRepFlt AND CUSER_ID <> gcUser_ID 
    ENDIF                  
ENDCASE
=SEEK(lcKey)

FUNCTION lfCheclAcc

IF (llAccAlUsr AND llAccAllU = 0) AND gfModalGen("QRM00381B00006","ALERT") = 2
  llAccAllU = 1
  SHOW GET llAccAllU
ENDIF

*!********************************************************************
*!
*!              Function: lfOGDel
*!
*!********************************************************************
FUNCTION lfOGDel
IF lcOGSeting<>0 &&'Default'
 IF !llOGSysData OR (llOGSysData AND !EMPTY(GETENV('DEVELOP')))
  ** WE MUST CONFIRM THE DELETION HERE
  lnOGOption=1
  IF lnOGOption = 1
    lcOGSelect=SELECT()
    SELECT SYURPFLT
    GO lcOGSeting

      *B804540,1 BWA 12/05/2001 Get the prorgram to use the settings of the option grid.[START]
      PRIVATE lcAliasDel
      lcAliasDel = SELECT(0)
      SELECT lcStting
      SET ORDER TO TAG lcSttng2
      DELETE FOR SetingNam + StyFltNm = laOGSeting[lnOGSeting,1]
      SELECT(lcAliasDel)
      *B804540,1 BWA 12/05/2001.[END]

      =ADEL(laOGSeting,lnOGSeting)
      DIMENSION laOGSeting[ALEN(laOGSeting,1)-1,2]
      DELETE
      =lfChangeGrid(ALLTRIM(lcOGManRep))      
   ENDIF   
  ENDIF  
ENDIF
*B804540,1 BWA 12/05/2001.[END]