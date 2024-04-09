************************************************************************
*: Program file  : ROB900.PRG (C# 101333)
*: Program desc. : Special open work in process report for ROB100.
*:                 Convert ROB900 from 2.6 to 2.7
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls :PROCEDURES :
*:          lpOpenFiles,lpGetMfgOp,lpChkFilt,lpSelSort,lpRob910,
*:          lpSubTot,lpPrtNotePad. 
*:        FUNCTIONS  : 
*:          lfvCuttKt,lfWOgrid,lfWOldVal,lfvVend,lfvStyle
*:          lfvFabric,lfFillSort,lfvSortBy,
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: B:602726,1 SSH 30/03/99 Collect data if and only if selection criteria changed
*: B:605653,1 AMH 03/12/2002 Printing the account code & completion date and cust po from
*: B:605653,1                user defined fields.
*:*************************************************************
lcStatExpr = ''
IF !EMPTY(lcRpStatus)
  lcStatExpr = IIF(lcRpStatus = 'L','CutTktH.Status $ [OHA]',;
                         'CutTktH.Status = lcRpStatus')
ENDIF
lcRpExp = IIF(EMPTY(lcStatExpr),lcRpExp,lcRpExp + [ AND ] + lcStatExpr)

QTOPIC='OPEN WORK IN PROCESS REPORT'
llmfgUsd = gfOpenFile(gcDataDir+'mfgoprhd',gcDataDir+'mfgoprhd','SH')
R_TITLE   = 'Open Work In Process report'
lcReport  = 'MFROB900'

lcFabNmbr = ''
FOR lnCheck = 1 TO 5
  lcVar  = "lcMType" + ALLTRIM(STR(lnCheck))
  IF EVAL(lcVar) = 'F'
    lcFabNmbr = ALLTRIM(STR(lnCheck))
    EXIT
  ENDIF
ENDFOR

lcTrmNmbr = ''
FOR lnCheck = 1 TO 5
  lcVar  = "lcMType" + ALLTRIM(STR(lnCheck))
  IF EVAL(lcVar) = 'T'
    lcTrmNmbr = ALLTRIM(STR(lnCheck))
    EXIT
  ENDIF
ENDFOR

*:C101333,1 SSH Initialize variables.
lcTITLE  = lcRpTit
lcFilter = lcRpExp
SELECT CUTTKTH
SET RELATION TO ALLTRIM(STYLE) INTO STYLE
SET FILTER TO
GO TOP
*:C101333,1 SSH  SELECT RECORDS FOR REPORT
STORE ' ' TO lcSortField,lcBreak,lcTotald
lcSortCode = '1'
*:C101333,1 SSH  Select the report sort.
DO lpSelSort
*:C101333,1 SSH  USED TO FLAG IF NEW PAGE ON EVERY BREAK
llPgBreak =.F.
IF ( lcSortCode='6' .OR. lcSortCode='7' )
  llPgBreak = llRpBreak
ENDIF
DO WHILE .T.
*:B#602726,1 SSH(BEGIN)30/03/99 Add the following if statement.
*--- Global variable to indicate if the selection criteria has been changed or not.
  IF llOgFltCh
    SELECT CUTTKTH
    = AFIELDS(laStrArr)
    SET FILTER TO
    *lcTmpFile = gfTempName()
    *COPY STRUC TO &gcWorkDir.&lcTmpFile
    *SSH  Add Fields for Contractors
    lnStrArr = ALEN(laStrArr,1)
    *SSH  Contractor 1
    DIMENSION laStrArr[lnStrArr+5,4]
    laStrArr[lnStrArr+1,1]='CONTR1'
    laStrArr[lnStrArr+1,2]='C'
    laStrArr[lnStrArr+1,3]='8'
    laStrArr[lnStrArr+1,4]='0'
    *SSH  Contractor 2
    laStrArr[lnStrArr+2,1]='CONTR2'
    laStrArr[lnStrArr+2,2]='C'
    laStrArr[lnStrArr+2,3]='8'
    laStrArr[lnStrArr+2,4]='0'
    *SSH  Contractor 3
    laStrArr[lnStrArr+3,1]='Account'
    laStrArr[lnStrArr+3,2]='C'
    laStrArr[lnStrArr+3,3]='5'
    laStrArr[lnStrArr+3,4]='0'
    *SSH  Contractor 4
    laStrArr[lnStrArr+4,1]='Cust_Po'
    laStrArr[lnStrArr+4,2]='C'
    laStrArr[lnStrArr+4,3]='10'
    laStrArr[lnStrArr+4,4]='0'
    *SSH  Contractor 5
    laStrArr[lnStrArr+5,1]='RAtE1'
    laStrArr[lnStrArr+5,2]='N'
    laStrArr[lnStrArr+5,3]='6'
    laStrArr[lnStrArr+5,4]='2'
    CREATE TABLE (gcWorkDir+lcRpTmp) FROM ARRAY laStrArr
    *SSH END CREATE TEMP FILE
    SELECT CUTTKTH
    SCAN FOR &lcFilter
      SCATTER MEMVAR
      SELECT mfgoprhd
      =SEEK('M'+m.CUTTKT)
      m.Rate1 = 0
      LOCATE REST WHILE cimtyp+ctktno+coprcode ='M'+CUTTKTH.CutTkt ;
                  FOR  cContCode = ALLTRIM(lcRpVendor) AND !lInHouse 
      IF !FOUND() AND !EMPTY(lcRpVendor)
        LOOP
      ENDIF
      lnIndex = 0
      SELECT ctktbom
      SET ORDER TO TAG Ctktyp
      IF SEEK('M'+m.CUTTKT+SPACE(19)+SPACE(06)+mfgoprhd.coprcode,'ctktbom')
        m.Rate1 = ctktbom.UntCost
      ENDIF
      SET ORDER TO TAG Ctktbom
      SELECT mfgoprhd
      SCAN REST WHILE cimtyp+ctktno+coprcode='M'+CUTTKTH.CutTkt ;
                AND lnIndex < 2 FOR   !lInHouse 
        lnIndex = lnIndex + 1
        STORE ALLTRIM(cContCode) TO ('m.Contr'+STR(lnIndex,1))
      ENDSCAN
      m.Account = IIF(SEEK('M'+m.CUTTKT,'cutpick'),IIF(SEEK('O'+cutpick.Order,'ORDHDR'),OrdHdr.Account,''),'')
      m.Cust_Po = OrdHdr.CustPo
      INSERT INTO (lcRpTmp) FROM MEMVAR
    ENDSCAN
    USE IN IIF(llmfgUsd,'mfgoprhd',0)
    SELECT &lcRpTmp
    GOTO TOP
    IF EOF()
      *:C101333,1 SSH  Text "No Record Selected."
      =gfModalGen('TRM38155B00000','DIALOG' )
      RETURN
    ENDIF
    SELECT CUTTKTH
    SET RELATION TO
    *:C101333,1 SSH  Create 2 temp file to collect all the cuttkt sum of the
    *:C101333,1 SSH  unit cost * unit Qtys.
    *:C101333,1 SSH  Hold the data for MFGCODE $ "FD"
    *lcCstTmp1 = gfTempName() 
    *:C101333,1 SSH  Hold the data for MFGCODE = "O"
    *lcCstTmp2 = gfTempName() 
    *:C101333,1 SSH  Copy the structure of CTKTBOM file to an array.
    SELECT CTKTBOM
    lcTag1 = IIF(!EMPTY(SYS(14,1,"CTKTBOM")) , SYS(14,1,"CTKTBOM") , 'CIMTYP+CUTTKT+TYP+IIF(!(CCATGTYP$"PMD"),ITEM,PADR(MFGCODE,12))+ICLR+DYELOT+STR(RECNO(),7)')
    =AFIELDS(laCtBomFld)
    lnBdDtFlds = ALEN(laCtBomFld,1)
    DIMENSION laCtBomFld[ALEN(laCtBomFld,1)+1,4]
    *:C101333,1 SSH  Add 1 row to the array to hold an extra field info to hold
    *:C101333,1 SSH  the multiple of unit cost * unit Qty.
    laCtBomFld[ALEN(laCtBomFld,1),1] = 'nUntCst'
    laCtBomFld[ALEN(laCtBomFld,1),2] = 'N'
    laCtBomFld[ALEN(laCtBomFld,1),3] = 12
    laCtBomFld[ALEN(laCtBomFld,1),4] = 2
    SET TALK WINDOW
    SET TALK ON
    WAIT 'Collecting data...' WINDOW NOWAIT     
    *:C101333,1 SSH  Create the temp file from the array that hold the new struc.
    CREATE TABLE (gcWorkDir+lcRpBom1) FROM ARRAY laCtBomFld
    CREATE TABLE (gcWorkDir+lcRpBom2) FROM ARRAY laCtBomFld
    *:C101333,1 SSH  Collect all the cuttkts from CTKTBOM file with the sum of
    *:C101333,1 SSH  the unit cost * unit qty.
    SELECT * , SUM(UntCost*UntQty) AS nUntCst ;
      FROM CTKTBOM ;
     GROUP BY CUTTKT ;
     WHERE ((CIMTYP+CUTTKT+IIF(!(CCATGTYP$"PMD"),ITEM,PADR(MFGCODE,12))+ICLR+DYELOT+STR(RECNO(),7) LIKE ;
             "M%%%%%%F%")  .OR. ;
           (CIMTYP+CUTTKT+IIF(!(CCATGTYP$"PMD"),ITEM,PADR(MFGCODE,12))+ICLR+DYELOT+STR(RECNO(),7) LIKE ;
             "M%%%%%%D%"))  .AND. (CCATGTYP$"PMD") ;
      INTO DBF (gcWorkDir+lcRpBom1)
    *:C101333,1 SSH  Create an index for the temp file that hold the CTKTBOM data.
    INDEX ON &lcTag1 TAG &lcRpBom1
    SET ORDER TO TAG &lcRpBom1
    SELECT * , SUM(UntCost*UntQty) AS nUntCst ;
      FROM CTKTBOM ;
     GROUP BY CUTTKT ;
     WHERE (CIMTYP+CUTTKT+IIF(!(CCATGTYP$"PMD"),ITEM,PADR(MFGCODE,12))+ICLR+DYELOT+STR(RECNO(),7) LIKE ;
             "M%%%%%%O%") .AND. CCATGTYP $ "PMD" ;
     INTO DBF (gcWorkDir+lcRpBom2)
    *:C101333,1 SSH  Create an index for the temp file that hold the CTKTBOM data.
    INDEX ON &lcTag1 TAG &lcRpBom2
    SET ORDER TO TAG &lcRpBom2
    WAIT CLEAR
    SET TALK OFF
    SET TALK NOWINDOW
    SELECT (lcRpTmp)
    *:C101333,1 SSH  SORT TO lcRpTmp INDEX
    IF lcSortField <> ' '
       INDEX ON &lcSortField TAG &lcRpTmp
       SET ORDER TO TAG &lcRpTmp
    ENDIF
  ENDIF
  *B:602726,1 SSH(END)
   STORE 0 TO lnTot1,lnTot2,lnTot3,lnTot31,lnTot4,lnTot5,lnTot6,;
              lnTotA1,lnTotA2,lnTotA3,lnTotA31,lnTotA4,lnTotA5,lnTotA6
   STORE 0 TO lnGTot1,lnGTot2,lnGTot3,lnGTot31,lnGTot4,lnGTot5,lnGTot6
   PAGENO = 0
   ROW    = 99
   XTIME  = TIME()
   SELECT &lcRpTmp
   *:C101333,1 SSH  initialize first subtotal.
   lcSub1 = ACCOUNT+DTOS(DEL_DATE)      
   lcSub2 = PATTERN+ACCOUNT             
   lcsub3 = CONTR2+DTOS(COMPLETE)
   lcsub4 = CONTR1+DTOS(DEL_DATE)
   SELECT &lcRpTmp
   GOTO TOP
   lcHBreak = SPACE(1)
   IF LEN(TRIM(lcBreak)) <>0
      lcHBreak = &lcBreak
   ENDIF
   lfBlnkLine = lcRpLine='Y'
   CLEAR TYPEAHEAD
   SET DEVICE TO PRINT
   SELE &lcRpTmp
   *:C101333,1 SSH  Printing part.
   DO lpRob910
   ROW=ROW+1
   @ ROW,00 SAY REPLICATE('*',216)
   ROW = ROW+1
   @ ROW,00 SAY '* GRAND TOTAL *'
   @ ROW,137 SAY lnGTot1 PICTURE '9999999'
   @ ROW,145 SAY lnGTot2 PICTURE '9999999'
   @ ROW,153 SAY lnGTot3 PICTURE '9999999'
   @ ROW,162 SAY lnGTot6 PICTURE '9999999'
   @ ROW,170 SAY lnGTot4 PICTURE '9999999'
   IF qCostPrv
     @ ROW,203 SAY lnGTot5  PICTURE '999999999.99'
   ENDIF
   ROW = ROW+1
   @ ROW,00 SAY REPLICATE('*',216)
   EXIT
ENDDO
*:C101333,1 SSH   END THE REPORT OR DISPLAY ON SCREEN
DO ENDREPORT
RETURN
*!*************************************************************
*! Name      : lpSelSort
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Procedure to select sort.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lpSelSort
*!*************************************************************
PROCEDURE lpSelSort

*:C101333,1 SSH  SELECT SORT SEQUENCE
lcSortCode = lcRpSort
DO CASE
  CASE '1' = lcRpSort
    lcSortField = 'ACCOUNT+DTOS(DEL_DATE)+CUTTKT'
    lcBreak     = 'ACCOUNT'
    lcTotald    = 'ACCOUNT + CUSTOMER DELIVERY DATE'
  CASE lcRpSort='2'
    lcSortField = 'CUTTKT'
  CASE lcRpSort='3'
    lcSortField = 'CONTR1+CUTTKT'
    lcBreak     = 'CONTR1'
    lcTotald    = 'FIRST CONTRACTOR + CUTTKT #'
  CASE lcRpSort='4'
    lcSortField = 'DTOS(DEL_DATE)+CUTTKT'
    lcBreak     = 'DTOS(DEL_DATE)'
    lcTotald    = 'CUSTOMER DELIVERY DATE'
  CASE lcRpSort='5'
    lcSortField = 'PATTERN+ACCOUNT+CUTTKT'
    lcBreak     = 'PATTERN'
    lcTotald    = 'PATTERN + CUSTOMER '
  CASE lcRpSort='6'
    lcSortField = 'CONTR1+DTOS(COMPLETE)+CUTTKT'
    lcBreak     = 'CONTR1'
    lcTotald    = 'FIRST CONTRACTOR + COMPLETION DATE'
  CASE lcRpSort='7'
    lcSortField = 'CONTR2+DTOS(COMPLETE)+CUTTKT'
    lcBreak     = 'CONTR2'
    lcTotald    = 'SECOND CONTRACTOR + COMPLETION DATE'
  CASE lcRpSort='8'
    lcSortField = 'CONTR2+CUTTKT'
    lcBreak     = 'CONTR2'
    lcTotald    = 'SECOND CONTRACTOR + CUTTKT #'
  CASE lcRpSort='9'
    lcSortField = 'CONTR1+STATUS+CUTTKT'
    lcBreak     = 'CONTR1+STATUS'
    lcTotald    = 'FIRST CONTRACTOR + STATUS'
  CASE lcRpSort='10'
    lcSortField = 'CONTR1+DTOS(DEL_DATE)+ACCOUNT+CUTTKT'
    lcBreak     = 'CONTR1'
    lcTotald    = 'FIRST CONTRACTOR + CUSTOMER DELIVERY DATE'
ENDCASE

*!*************************************************************
*! Name      : lpRob910
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Open work in process report print format.
*!*************************************************************
*! Calls     : RPT_HDR
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpRob910
*!*************************************************************
PROCEDURE lpRob910

SET DECIMALS TO 3 
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3....+....4....+
*ISS DATE ACCNT CTKT # CUST. PO#  DEL DATE STYLE         ESTQTY VENDOR WIDTH  TYPE   PATTERN #  ACT DATE CNTR1 CNTR2 INI.C/D  COMPDATE ST QTY QTY REC QTY DAM CAN QTY OPEN     WIP UNITCOST DTY/FRT    MISC  TOTAL COST.
*MM/DD/YY XXXXX XXXXXX XXXXXXXXXX MM/DD/YY XXXXXXXXXXXX 9999999 XXXXX  XXXXXX XXXXXX XXXXXXXXXX MM/DD/YY XXXXX XXXXX MM/DD/YY MM/DD/YY X  9999999 9999999 9999999  9999999 9999999 9999.999 999.999 999.999 999999999.99
DO WHILE .T.
   IF ROW >=51
      PAGENO = PAGENO + 1
      IF lcBreak <> ' '
        lcReport='ROB900'+'  '+'TOTAL :'+lcTotald
      ELSE
        lcReport='ROB900'
      ENDIF
      DO RPT_HDR WITH lcReport,lcTITLE,R_WIDTH
      @ 005,000 SAY 'ISS DATE ACCNT CTKT # CUST. PO#  DEL DATE STYLE               ESTQTY  WIDTH  TYPE   PATTERN #  ACT DATE             INI.C/D  COMPDATE ST QTY QTY REC QTY DAM CAN QTY OPEN     WIP UNITCOST DTY/FRT    MISC  TOTAL COST.'
      @ 006,000 SAY '                                                                      VENDOR                   CONTRCT1 CONTRCT2                                                                                                       '
      @ 007,000 SAY REPLICATE('=',216)
      ROW = 8
   ENDIF
   llSub1=IIF('1' = lcSortCode .AND.(ACCOUNT+DTOS(DEL_DATE)<>lcSub1),.T.,.F.)
   llSub2=IIF(lcSortCode='5'.AND.(PATTERN+ACCOUNT<>lcSub2),.T.,.F.)
   llSub3=IIF(lcSortCode='7'.AND.(CONTR2+DTOS(COMPLETE)<>lcSub3),.T.,.F.) 
   llsub4=IIF(lcSortCode='10'.AND.(CONTR1+DTOS(DEL_DATE)<>lcSub4),.T.,.F.)
   *:C101333,1 SSH  Print Subtotals.
   DO lpSubTot
   IF EOF()
      EXIT
   ENDIF
   IF ROW >=50
      ROW = 99
      LOOP
   ENDIF
   *:C101333,1 SSH  GET VENDOR AND WIDTH FROM FIRST FABRIC and COSTS ****
   SELE CTKTBOM
   lcVendor = SPACE(1)
   lcWidth  = SPACE(1)
   lcFAB    = SPACE(1)
   STORE 0 TO lnUntC1,lnUntC2
   lcCutTkt = &lcRpTmp..CUTTKT
   lnUntC1 = IIF(SEEK('M' + lcCutTkt , lcRpBom1) , &lcRpBom1..nUntCst , 0)
   lnUntC2 = IIF(SEEK('M' + lcCutTkt , lcRpBom2) , &lcRpBom2..nUntCst , 0)
   SELECT CTKTBOM
   IF SEEK('M'+&lcRpTmp..CUTTKT+lcFabNmbr)
     lcFAB = SUBSTR(ITEM,1,7)
   ELSE
     lcFAB = ""
   ENDIF
   SELE FABRIC
   IF SEEK(lcFAB) AND !EMPTY(lcFAB)
     lcVendor = VENDOR
     lcWidth  = WIDTH
   ELSE
     lcVendor = SPACE(1)
     lcWidth  = SPACE(1)
   ENDIF
   *:C101333,1 SSH  GET UNIT COST OF FABRIC+TRIM+MFG_OPERAION ALL BUT SEWING AND PRESSING
   *:C101333,1 SSH  WHICH ARE MFG_OPR2 & MFG_OPR3, OPEN WIP, QUANTITY OF STATUS
   SELE &lcRpTmp
   lnTOT_PCS = 0
   DO CASE
     CASE STATUS='A'
       lnTOT_PCS = PCS_ACT
     OTHERWISE
       lnTOT_PCS = PCS_BUD - PCS_CAN  
   ENDCASE
   lnOPN_WIP = PCS_OPN 
   lnActCst = IIF(!EMPTY(lcFabNmbr),nAct_Cost&lcFabNmbr,0) + IIF(!EMPTY(lcTrmNmbr),nAct_Cost&lcTrmNmbr,0)
   lnUNT_CST = IIF(lnTOT_PCS<>0,ROUND(lnActCst/lnTOT_PCS,3),0)   && FABRIC + TRIM
   lnUNT_CST = lnUNT_CST + RATE1                                  && FIRST MFG.OPER
   SELE &lcRpTmp
   @ ROW,000 SAY ENTERED
   
   *B605653,1 AMH Printing the account code from user defined field [Start]
   *@ ROW,009 SAY IIF(EMPTY(ACCOUNT),'STOCK',ACCOUNT)
   @ ROW,009 SAY CACCOUNT
   *B605653,1 AMH [End]
   
   @ ROW,015 SAY CUTTKT
   
   *B605653,1 AMH Printing the cust po and complition date from user defined fields [Start]
   *@ ROW,022 SAY CUST_PO
   *@ ROW,033 SAY DEL_DATE
   @ ROW,022 SAY SUBSTR(CCUSTPO,1,10)
   @ ROW,033 SAY DDEL_DATE
   *B605653,1 AMH [End]
   
   @ ROW,042 SAY STYLE
   @ ROW,055 SAY PCS_BUD PICTURE '9999999'
   @ ROW,070 SAY lcWidth
   @ ROW,077 SAY CtktTYPE
   @ ROW,084 SAY PATTERN
   @ ROW,095 SAY ACT_DATE
   @ ROW,116 SAY INI_COMP
   @ ROW,125 SAY COMPLETE
   @ ROW,134 SAY STATUS
   @ ROW,137 SAY lnTOT_PCS PICTURE '9999999'
   @ ROW,145 SAY PCS_REC PICTURE '9999999'
   @ ROW,153 SAY PCS_DAM PICTURE '9999999'
   IF STATUS='A' 
     @ ROW,162 SAY (PCS_CAN - PCS_CANOLD) PICTURE '9999999'
   ELSE 
     @ ROW,162 SAY PCS_CAN PICTURE '9999999'      
   ENDIF
   @ ROW,170 SAY lnOPN_WIP PICTURE '9999999'
   IF qCostPrv
     @ ROW,178 SAY lnUNT_CST PICTURE '9999.999'
     @ ROW,187 SAY lnUntC1 PICTURE '999.999'
     @ ROW,195 SAY lnUntC2 PICTURE '999.999'
     @ ROW,203 SAY (lnOPN_WIP * (lnUNT_CST+lnUntC1+lnUntC2)) PICTURE '999999999.99'
   ENDIF
   *:C101333,1 SSH  MARK THIS CUTTING TICKIT LINE IF IT'S COMPLETION DATE IS LARGER THAN 
   *:C101333,1 SSH  THE COMPLETION DATE OF THE CUSTOMER ORDER
   IF !EMPTY(DEL_DATE) .AND. COMPLETE > DEL_DATE
     @ ROW,216 SAY '**'
   ENDIF   
   ROW = ROW + 1
   @ ROW,070 SAY lcVendor
   @ ROW,095 SAY CONTR1
   @ ROW,104 SAY CONTR2
   IF SEEK('I'+&lcRpTmp..Cuttkt,'NOTEPAD')
     DO lpPrtNotePad
   ENDIF 
   SELECT &lcRpTmp   
   lcDelDate = DTOC(DEL_DATE)
   lcComDate = DTOC(COMPLETE)
   *:C101333,1 SSH  ADD THE SUBTOTALS AND GRAND TOTALS
   lnTot1  = lnTot1  + lnTOT_PCS
   lnTotA1 = lnTotA1 + lnTOT_PCS
   lnGTot1 = lnGTot1 + lnTOT_PCS
   lnTot2  = lnTot2  + PCS_REC
   lnTotA2 = lnTotA2 + PCS_REC
   lnGTot2 = lnGTot2 + PCS_REC
   lnTot3  = lnTot3  + PCS_DAM
   lnTotA3 = lnTotA3 + PCS_DAM
   lnGTot3 = lnGTot3 + PCS_DAM
   lnTot4  = lnTot4  + lnOPN_WIP
   lnTotA4 = lnTotA4 + lnOPN_WIP
   lnGTot4 = lnGTot4 + lnOPN_WIP
   lnTot5  = lnTot5  + (lnOPN_WIP * (lnUNT_CST+lnUntC1+lnUntC2))
   lnTotA5 = lnTotA5 + (lnOPN_WIP * (lnUNT_CST+lnUntC1+lnUntC2))
   lnGTot5 = lnGTot5 + (lnOPN_WIP * (lnUNT_CST+lnUntC1+lnUntC2))
   lnTot6  = lnTot6  + (PCS_CAN - PCS_CANOLD)
   lnTotA6 = lnTotA6 + (PCS_CAN - PCS_CANOLD)
   lnGTot6 = lnGTot6 + (PCS_CAN - PCS_CANOLD)
   ROW    = ROW    + IIF(lfBlnkLine,2,1)
   SKIP
ENDDO
SET DECIMALS TO 2                      

*!*************************************************************
*! Name      : lpSubTot
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Print subtotals.
*!*************************************************************
*! Calls     : RPT_HDR
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpSubTot
*!*************************************************************
PROCEDURE lpSubTot

IF llSub1 .OR. llSub2 .OR. llSub3 .OR. llSub4
  @ ROW,030 SAY '*** TOTALS FOR '
  DO CASE
    CASE llSub1 .OR. llSub4
      @ ROW,046 SAY lcDelDate
    CASE llSub2
      @ ROW,046 SAY SUBSTR(lcSub2,11,5)
      SELE CUSTOMER
      lcKEY='M'+SUBSTR(lcSub2,11,5)
      @ ROW,053 SAY IIF(SEEK(lcKEY),BTNAME+'   =======>',;
        IIF(EMPTY(SUBSTR(lcSub2,11,5)) ,'CUT TO STOCK   =======>',' '))
    CASE llSub3
      @ ROW,046 SAY lcComDate
  ENDCASE
  SELE &lcRpTmp
  @ ROW,137 SAY lnTotA1 PICTURE '9999999'
  @ ROW,145 SAY lnTotA2 PICTURE '9999999'
  @ ROW,153 SAY lnTotA3 PICTURE '9999999'
  @ ROW,162 SAY lnTotA6 PICTURE '9999999'
  @ ROW,170 SAY lnTotA4 PICTURE '9999999'
  IF qCostPrv
    @ ROW,203 SAY lnTotA5  PICTURE '999999999.99'
  ENDIF
  ROW=ROW+2
  lcSub1=ACCOUNT+DTOS(DEL_DATE)
  lcSub2=PATTERN+ACCOUNT
  lcSub3=CONTR2+DTOS(COMPLETE)
  lcSub4=CONTR1+DTOS(DEL_DATE)
  STORE 0 TO lnTotA1,lnTotA2,lnTotA3,lnTotA4,lnTotA5,lnTotA6
ENDIF
DO WHILE !EMPTY(lcBreak)
  IF &lcBreak = lcHBreak
     EXIT
  ENDIF
  @ ROW,00 SAY REPLICATE('=',216)
  ROW = ROW+ 1
  DO CASE
    CASE '1' = lcSortCode
      SELE CUSTOMER
      lcKEY = 'M'+SUBSTR(lcHBreak,1,5)
      IF SEEK(lcKEY)
        SELE &lcRpTmp
        @ ROW,001 SAY '**** SUB TOTAL **** '+SUBSTR(lcHBreak,1,5)
        @ ROW,027 SAY CUSTOMER->BTNAME
      ELSE
        SELE &lcRpTmp
        IF EMPTY(SUBSTR(lcHBreak,1,5))
          @ ROW,001 SAY '**** SUB TOTAL ****  CUT TO STOCK   '
        ELSE
          @ ROW,001 SAY '**** SUB TOTAL ****  '+lcHBreak
        ENDIF
      ENDIF
    CASE lcSortCode='3'
      @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(lcHBreak,1,8)+'  '+SUBSTR(lcHBreak,9,LEN(lcHBreak))
    CASE lcSortCode='4'
      @ ROW,001 SAY '**** SUB TOTAL ****  '+lcDelDate
    CASE lcSortCode='5'
      @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(lcHBreak,1,10)
    CASE lcSortCode='9'
      DO CASE
        CASE SUBSTR(lcHBreak,9,1)='A'
          @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(lcHBreak,1,8)+'  '+'ACTUAL'
        CASE SUBSTR(lcHBreak,9,1)='O'
          @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(lcHBreak,1,8)+'  '+'OPEN'
        CASE SUBSTR(lcHBreak,9,1)='H'
          @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(lcHBreak,1,8)+'  '+'HOLD'
      ENDCASE
    OTHE  &&PSORTCODE=4,6,7,8,10
      @ ROW,001 SAY '**** SUB TOTAL ****  '+lcHBreak
  ENDCASE
  *:C101333,1 SSH  PRINT SUBTOTALS
  @ ROW,137 SAY lnTot1 PICTURE '9999999'
  @ ROW,145 SAY lnTot2 PICTURE '9999999'
  @ ROW,153 SAY lnTot3 PICTURE '9999999'
  @ ROW,162 SAY lnTot6 PICTURE '9999999'
  @ ROW,170 SAY lnTot4 PICTURE '9999999'
  IF qCostPrv
    @ ROW,203 SAY lnTot5  PICTURE '999999999.99'
  ENDIF
  ROW = ROW+1
  @ ROW,00 SAY REPLICATE('=',216)
  ROW = ROW+1
  *:C101333,1 SSH  INITIALIZE SUBTOTALS
  STORE 0 TO lnTot1,lnTot2,lnTot3,lnTot4,lnTot5,lnTot6
  STORE 0 TO lnTotA1,lnTotA2,lnTotA3,lnTotA4,lnTotA5,lnTotA6
  lcHBreak = &lcBreak
  IF ( lcSortCode='6' .OR. lcSortCode='7' ) .AND. llPgBreak
    ROW=99
  ENDIF
  EXIT
ENDDO
*:C101333,1 SSH  END SUBTOTALS

*!*************************************************************
*! Name      : lpPrtNotePad
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Print the NotePad of the CutTkt that start with (*APPROVAL).
*!*************************************************************
*! Calls     : RPT_HDR
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpPrtNotePad
*!*************************************************************
PROCEDURE lpPrtNotePad

SELECT NOTEPAD
lnMemoWdth = SET('MEMOWIDTH')
SET MEMOWIDTH TO 75
lnMemolins = MEMLINES(mNotes)
IF lnMemolins = 0
  SET MEMOWIDTH TO (lnMemoWdth)
  RETURN
ENDIF
STORE .T. TO llFrst,llCount
STORE 0   TO lnMemoLnNo,lnOTxtLen
DO WHILE lnMemoLnNo <> lnMemolins
  lnMemoLnNo = lnMemoLnNo + 1
  lcMline    = MLINE(mNotes,lnMemoLnNo)
  IF OCCURS("*APPROVAL",lcMline ) = 0
    LOOP
  ENDIF
  IF llFrst
    @ ROW,02 SAY '* --- N O T E S --- *' 
    Row    = Row + 1
    llFrst = .F.
  ENDIF
  IF ROW >= 51
    PageNo   = PageNo + 1
    lcReport = 'ROB900' + IIF(lcBreak<>' ','  '+'TOTAL :'+lcTotald,'')
    *:C101333,1 SSH  Print the header.
    DO RPT_HDR WITH lcReport,lcTITLE,R_WIDTH
    @ 05,00 SAY 'ISS DATE ACCNT CTKT # CUST. PO#  DEL DATE STYLE               ESTQTY  WIDTH  TYPE   PATTERN #  ACT DATE             INI.C/D  COMPDATE ST QTY QTY REC QTY DAM CAN QTY OPEN     WIP UNITCOST DTY/FRT    MISC  TOTAL COST.'
    @ 06,00 SAY '                                                                      VENDOR                   CONTRCT1 CONTRCT2                                                                                                       '
    @ 07,00 SAY REPLICATE('=',216)
    @ 08,02 SAY '* -- CONTINUE NOTES OF CUTTKT '+&lcRpTmp..CUTTKT+' -- *'
    ROW = 09
    llCount = .T.
  ENDIF
  lcText = STRTRAN(lcMline,"*APPROVAL")
  IF llCount
    lnPCol    = 01
    lnOTxtLen = 00
    llCount = .F.
  ELSE
    IF ( lnPCol+ lnOldText + LEN(lcText) ) > 215
      lnPCol = 01
      Row    = Row + 1
    ELSE
      lnPCol = lnPCol + lnOldText + 1
    ENDIF
  ENDIF  
  @ Row,lnPCol+1 SAY lcText
  lnOldText = LEN(lcText)
ENDDO
SET MEMOWIDTH TO (lnMemoWdth)
RETURN

*!*************************************************************
*! Name      : lfvPikTkt
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Validation function for the Picking ticket field
*!*************************************************************
*! Calls     : CutBrow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPikTkt()
*!*************************************************************
FUNCTION lfvCuttKt
PRIVATE lcVar , lcObj

SET ORDER TO CUTTKTH IN CUTTKTH
*:C101333,1 SSH Varible to hold  the name of the memory variable used to create the current GET control
lcVar = SYS(18)      
*:C101333,1 SSH Varible to hold the current field value
lcObj = EVALUATE(SYS(18))      
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
*:C101333,1 SSH IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'CUTTKTH'))
  SELECT CUTTKTH
  = CutBrow(@lcObj,' ')
ENDIF    && End of IF
IF EMPTY(lcObj)
  &lcVar = laOldVal 
ELSE
  &lcVar = lcObj
ENDIF

*!*************************************************************
*! Name      : lfWOgrid
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : When option grid.
*!*************************************************************
*! Calls     : gfUserPriv()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfWOgrid()
*!*************************************************************
FUNCTION lfWOgrid

FOR lnCount = 1 TO 5
  lcCount  = STR(lnCount,1)
  lcMType&lcCount = gfGetMemVar('M_CM'+'TYPE'+lcCount)
ENDFOR 
R_WIDTH  = 'XW'       && XTRA WIDE REPORT
qCostPrv = gfUserPriv('IC','ICSTYLE','COSTING')

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Get old value.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfWOldVal()
*!*************************************************************
FUNCTION lfWOldVal

laOldVal = EVAL(SYS(18))

*!*************************************************************
*! Name      : lfvVend
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Function to valid vendor.
*!*************************************************************
*! Calls     : gfModalGen,gfApVnBrow
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvVend()
*!*************************************************************
FUNCTION lfvVend

PRIVATE lcVar, lcObj
*:C101333,1 SSH Varible to hold  the name of the memory variable used to create the current GET control
lcVar = SYS(18)
*:C101333,1 SSH Varible to hold the current field value
lcObj = ALLTRIM(EVALUATE(SYS(18)))
SELECT APVENDOR
SET ORDER TO TAG VenCode 
*:C101333,1 SSH IF Statment to check if we are going to Browse
 IF !EMPTY(lcObj) .AND. (!('?' $ lcObj) .AND. !SEEK(lcObj , 'APVENDOR'))
   =gfModalGen('TRM38152B00000','DIALOG' )
 ENDIF
 IF !EMPTY(lcObj) .AND. ('?' $ lcObj)
  =gfApVnBrow(@lcObj)
  IF !EMPTY(lcObj)
    &lcVar = lcObj
  ELSE
    &lcVar = laOldVal
  ENDIF
ENDIF
*:C101333,1 SSH End of lfvVend.

*!*************************************************************
*! Name      : lfvStyle
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Function to valid style.
*!*************************************************************
*! Calls     : gfStyBrw
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 
*:C101333,1 SSH Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*:C101333,1 SSH Varible to hold  the value of the current GET field
lcObjVal = EVALUATE(SYS(18))

*:C101333,1 SSH IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , laOldVal)
  lcObjVal = ALLTRIM(lcObjVal)
  &lcObjName = lcObjVal
ENDIF
SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)
*:C101333,1 SSH End of lfvStyle.

*!*************************************************************
*! Name      : lfvFabric
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Function to valid fabric.
*!*************************************************************
*! Calls     : FaBrow
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias   = SELECT(0)
*:C101333,1 SSH Varible to hold  the name of the memory variable used to create the current GET control
lcVar = SYS(18)
*:C101333,1 SSH Varible to hold the current field value
lcObj = EVALUATE(SYS(18))
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
llUseByMe = .F.
IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'FABRIC'))
    = FaBrow(@lcObj,'*')
ENDIF
IF EMPTY(lcObj) AND !EMPTY(laOldVal)
  &lcVar = laOldVal 
ELSE
  &lcVar = lcObj
ENDIF
SET ORDER TO FABRIC IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF  
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfFillSort
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Function to fill sort array.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfFillSort()
*!*************************************************************
FUNCTION lfFillSort

DIMENSION  laSortDesc[10,1]
DIMENSION  laSortVal[10,1]
laSortDesc[01] = 'Customer + Customer delivery Date'
laSortDesc[02] = 'Cutting Tiket#'
laSortDesc[03] = 'First Contractor + Cutting Tiket#'
laSortDesc[04] = 'Customer Delivery Date'
laSortDesc[05] = 'Pattern + Customer'
laSortDesc[06] = 'First Contractor + Completion Date'
laSortDesc[07] = 'Second Contractor + Completion Date'
laSortDesc[08] = 'Second Contractor + Cutting Tiket#'
laSortDesc[09] = 'First Contractor Group By Status'
laSortDesc[10] = 'First Contractor + Delivery Date'
laSortVal[01]  = '1'
laSortVal[02]  = '2'
laSortVal[03]  = '3'
laSortVal[04]  = '4'
laSortVal[05]  = '5'
laSortVal[06]  = '6'
laSortVal[07]  = '7'
laSortVal[08]  = '8'
laSortVal[09]  = '9'
laSortVal[10]  = '10'

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Function to valid sort array.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy

PRIVATE llClrRead
llClrRead = .F.
lcOldVal  = laSortVal[laOldVal]
IF lcRpSort $ '67'
  IF !(lcOldVal $ '67')
    llRpFrSort = .F.
    llClrRead  = .T.
  ENDIF  
ELSE && C101333,1 SSH [1,2,3,4,5,8,9,10]
  IF lcOldVal $ '67'
    llRpFrSort = .T.
    llClrRead  = .T.
    llRpBreak  = .F.
  ENDIF  
ENDIF
IF llClrRead
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfClearRep
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*B:602726,1 Add clear function.
*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(lcRpTmp),lcRpTmp,0)
*khalid
*ERASE &gcWorkDir.&lcRpTmp+'.DBF'
*ERASE &gcWorkDir.&lcRpTmp+'.CDX'
IF FILE(gcWorkDir+lcRpTmp+'.DBF')
  ERASE &gcWorkDir.&lcRpTmp+'.DBF'
ENDIF
IF FILE(gcWorkDir+lcRpTmp+'.CDX')
  ERASE &gcWorkDir.&lcRpTmp+'.CDX'
ENDIF
*khalid
  

USE IN IIF(USED(lcRpBom1),lcRpBom1,0)
*khalid
*ERASE &gcWorkDir.&lcRpBom1+'.DBF'
*ERASE &gcWorkDir.&lcRpBom1+'.CDX'
IF FILE(gcWorkDir+lcRpBom1+'.DBF')
  ERASE &gcWorkDir.&lcRpBom1+'.DBF'
ENDIF
IF FILE(gcWorkDir+lcRpBom1+'.CDX')
  ERASE &gcWorkDir.&lcRpBom1+'.DBF'
ENDIF  
USE IN IIF(USED(lcRpBom2),lcRpBom2,0)
*khalid
*ERASE &gcWorkDir.&lcRpBom2+'.DBF'
*ERASE &gcWorkDir.&lcRpBom2+'.CDX'
IF FILE(gcWorkDir+lcRpBom2+'.DBF')
  ERASE &gcWorkDir.&lcRpBom2+'.DBF'
ENDIF
IF FILE(gcWorkDir+lcRpBom2+'.CDX')
  ERASE &gcWorkDir.&lcRpBom2+'.CDX'  
ENDIF
*khalid  