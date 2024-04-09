*:***********************************************************************************************************
*: Program file  : MFTRGDT.PRG                              
*: Program desc. : Project management trigger date report.(N00341)
*:         Module: Aria Apparel Series.
*:      Developer: Hend Ghanem (HBG)
*:***********************************************************************************************************
*:B119218,1 ALB 08/24/2003 Fix Bugs in Picture and validation function in template id field
*:B130985,1 HBG 03/28/2006 Fix Bug of Scheduling report displays status ON TIME when project late
*************************************************************************************************************
R_TITLE   = 'PROJECT MANAGEMENT SCHEDULING REPORT'
lcForCond = '.T.'            && The line filter.
llPrint = .F.
IF EMPTY(lcRpTemplt) AND lcRpRptLy  = 'V' 
  =gfModalGen('TRM38233B00000','DIALOG')
  lcRpRptLy  = 'H'
  lcRpOprTyp = 'A'
  CLEAR READ
  RETURN
ENDIF

*** Array to hold all operations data to be printed for the current project.
DIMENSION laOprToPrn[1,7]

DO CASE
  CASE lcRpSrtBy $ 'OC'
    lcSortBy = 'PmPrjHd.cPrj_Id'
  CASE lcRpSrtBy = 'R'
    lcSortBy = 'PmPrjHd.dReq_Fnsh,PmPrjHd.cPrj_Id'
  CASE lcRpSrtBy $ 'A'
    lcSortBy = 'ORDHDR.Account,PmPrjHd.cPrj_Id'
  CASE lcRpSrtBy $ 'V'
    lcSortBy = 'POSHDR.VENDOR,PmPrjHd.cPrj_Id'
ENDCASE

IF lfBuldFltr() .AND. lfGetData()
  R_WIDTH = IIF(lcRpRptLy = 'V','XW','W')
  XREPORT = 'PRM910'
  PageNo  = 0
  lnRow   = 99
  lnMax   = 53
  SELECT (lcTmpFile)
  SET DEVICE TO PRINT
  =lfRepFrm&lcRpRptLy()
  IF llPrint
    DO ENDREPORT
  ELSE
    =gfModalGen('TRM00052B00000','DIALOG')  
  ENDIF  
ELSE
  =gfModalGen('TRM00052B00000','DIALOG')
ENDIF
SET DEVICE TO SCREEN

*:************************************************************************
*: Program file  : lfBuldFltr
*: Program desc. : Function to build the filter string according to the
*:                 grid information.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfBuldFltr

DO CASE 
  CASE lcRpPrjTp = 'C'
    lcRpExp = lcRpExp + '.AND. PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle = lcRpPrjTp+CUTTKTH.CUTTKT'
  CASE lcRpPrjTp $ 'OT'
    lcRpExp = lcRpExp + '.AND. PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle = lcRpPrjTp+ORDHDR.ORDER'
  CASE lcRpPrjTp $ 'PADNR'
    lcRpExp = lcRpExp + '.AND. PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle = lcRpPrjTp+POSHDR.PO'
ENDCASE

*B130985,1 HBG 03/28/2006 Fix Bug of Scheduling report displays status ON TIME when project late [Begin]
*lcRpExp = lcRpExp + IIF(llRpShLate,'.AND. PmPrjHd.dAct_Fnsh > PmPrjHd.dClc_Fnsh','')
lcRpExp  = lcRpExp + IIF(llRpShLate,'.AND. PmPrjHd.dAct_Fnsh > PmPrjHd.dEst_Fnsh','')
*B130985,1 HBG 03/28/2006 [End]
lcRpExp = lcRpExp + IIF(lcRpPrjStt <> 'A', '.AND. PmPrjHd.cPrj_Stts = lcRpPrjStt ','.AND. PmPrjHd.cPrj_Stts <> "X" ')

*:************************************************************************
*: Program file  : lfGetData
*: Program desc. : Function to collect data required for processing.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfGetData

IF lcRpPrjTp  <> 'C'
  DO CASE
    CASE lcRpPrjTp  $ 'OT'
      SELECT PmPrjHd.*,ORDHDR.Account FROM PmPrjHd,ORDHDR;
        WHERE &lcRpExp;
        ORDER BY &lcSortBy INTO DBF (gcWorkDir+lcTmpFile)
    CASE lcRpPrjTp  $ 'PADNR'
      SELECT PmPrjHd.*,POsHdr.VENDOR FROM PmPrjHd,POsHdr;
        WHERE &lcRpExp;
        ORDER BY &lcSortBy INTO DBF (gcWorkDir+lcTmpFile)
  ENDCASE
ELSE
  SELECT PmPrjHd.*,lfGetAccnt('1',PmPrjHd.cPrj_Id) AS Account FROM PmPrjHd,CutTktH;
    WHERE &lcRpExp;
    ORDER BY &lcSortBy INTO DBF (gcWorkDir+lcTmpFile)
ENDIF

RETURN (_TALLY <> 0)

*:************************************************************************
*: Program file  : lfGetAccnt
*: Program desc. : Function to return the account for the cuttkt.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfGetAccnt
PARAMETERS lcPType,lcPCutTkt

lcRetVal = 'STOCK'
SELECT CutPick
LOCATE FOR TranCd + cTktNo = lcPType+lcPCutTkt
IF FOUND()
  lcOrdNo = Order
  CONTINUE
  IF FOUND()
    lcRetVal = 'MULTI'
  ELSE
    IF SEEK('O'+lcOrdNo,'OrdHdr')
      lcRetVal = OrdHdr.Account
    ENDIF
  ENDIF
ENDIF
RETURN lcRetVal

*:************************************************************************
*: Program file  : lfvLines
*: Program desc. : Function to validate if the current record notify memo
*:                 has the current notify field entered by the user or to
*:                 validate the line (operation) status for the passed project.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfvLines
PARAMETERS lcSeekCode, lcRpNotfy, lcRpOprStt

lcPrvAlis = SELECT(0)
llReturn = .F.
IF SEEK(lcSeekCode,'PmPrjDt')
  IF !EMPTY(lcRpNotfy)
    DO CASE
      CASE lcRpOprStt = 'A'
        lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify'
      CASE lcRpOprStt = 'V'
        lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. lVoid'
      CASE lcRpOprStt = 'O'
        lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. !lVoid'
    ENDCASE
  ELSE
    lcForCond = IIF(lcRpOprStt = 'V','lVoid','!lVoid')
  ENDIF

  SELECT PmPrjDt
  LOCATE REST WHILE cPrj_Typ + cPrj_Id + cStyle = lcSeekCode;
            FOR &lcForCond
  llReturn = FOUND()

  SELECT (lcPrvAlis)
ENDIF
RETURN (llReturn)

*:************************************************************************
*: Program file  : lfvPrjID
*: Program desc. : Validation function for the project passed.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfvPrjID

PRIVATE lcPrjTitle, lcBrFields, lnCurTag, lnCurAlias, lnSoftSeek, lnOption

lcPrjID = EVALUATE(SYS(18))

lcPrj  = STRTRAN(lcPrjID ," ","")

IF EMPTY(lcPrj)
  lcPrjID  = ""
ENDIF

IF !EMPTY(lcOldValue) AND lcPrjID  = lcOldValue
  RETURN
ENDIF

lcFldLocNam = SYS(18)

lnCurAlias   = SELECT()
SELECT PMPRJHD
lnCurTag     = VAL(SYS(21))
SET ORDER TO TAG PMPRJHD
lcBrFields   = "cPrj_ID  : H='Project ID',"+;
               "cStyle   : H='Style',"+;
               "cPrj_SDsc: H='Project Description'"     
DO CASE
  CASE lcRpPrjTp  = 'C'
    lcPrjTitle = 'Cutting Tickets Projects'
  CASE lcRpPrjTp  = 'P'      && PO
    lcPrjTitle = 'Purchase Orders Projects'
  CASE lcRpPrjTp  = 'A'      && Adorment Order
    lcPrjTitle = 'Adorment Order Projects'   
  CASE lcRpPrjTp  = 'D'      && Dye Order 
    lcPrjTitle = 'Dye Order Projects'     
  CASE lcRpPrjTp  = 'N'      && Inter-Location PO
    lcPrjTitle = 'Inter-Location PO Projects'      
  CASE lcRpPrjTp  = 'R'      && Return PO
    lcPrjTitle = 'Return PO Projects'    
  CASE lcRpPrjTp  = 'O'      && SO
    lcPrjTitle = 'Sales Orders Projects'    
  CASE lcRpPrjTp  = 'T'      && EDI Order
    lcPrjTitle = 'EDI Order Projects'  
ENDCASE

llBrowse = IIF(TYPE('llBrowse') = 'U', .F., llBrowse)
DIMENSION latemp[1]
IF llBrowse .OR. '?' $ lcPrjID
  GO TOP
  lcPrjID  = IIF(ARIABROW([lcRpPrjTp],lcPrjTitle, ;
                  gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PMPRJHD.cPrj_ID','laTemp'),;
                  PMPRJHD.cPrj_ID, SPACE(6))         
ELSE
  IF !EMPTY(lcPrjID) 
    IF !SEEK(lcRpPrjTp + lcPrjID)  
      lnSoftSeek = RECNO(0)
      IF BETWEEN(lnSoftSeek, 1, RECCOUNT())
        GO lnSoftSeek
       ELSE
         GO TOP
       ENDIF
       lcPrjID  = IIF(ARIABROW([lcRpPrjTp],lcPrjTitle, ;
                       gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PMPRJHD.cPrj_ID','laTemp'),;
                       PMPRJHD.cPrj_ID, SPACE(6))         
    ENDIF  
  ENDIF
ENDIF     

&lcFldLocNam = lcPrjID

SET ORDER TO (lnCurTag)
SELECT (lnCurAlias)


*:************************************************************************
*: Program file  : lfvUserId
*: Program desc. : Browse function for avilable Users.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfvUserId

PRIVATE lcTitle, lcBrFields, lnCurAlias

lnCurAlias   = SELECT(0)
lcUser_Id = EVALUATE(SYS(18))
lcUser = STRTRAN(lcUser_Id," ","")
IF EMPTY(lcUser)
  lcUser_Id = ""
ENDIF
IF !EMPTY(lcOldValue) AND lcUser_Id = lcOldValue
  RETURN
ENDIF
lcFldLocNam = SYS(18)
SELECT SYUUSER
GO TOP
IF EOF()
  SELECT (lnCurAlias)
  lcUser_Id = PADR(ALLTRIM(lcUser_Id),10)
ELSE
  lcBrFields = "cUser_Id : H='User ID',cUsr_Name : H='User Name'"
  lcSetExact = SET('EXACT')
  SET EXACT ON
  lcTitle    = 'Users'
  DIMENSION latemp[1]
  IF !EMPTY(lcUser_Id) AND !SEEK(ALLTRIM(lcUser_Id))
    lcUser_Id = IIF(ARIABROW('',lcTitle,;
                     gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','SYUUSER.cUser_Id','latemp'),;
                     SYUUSER.cUser_Id, SPACE(10))
    &lcFldLocNam = lcUser_Id                   
  ENDIF
  SET EXACT &lcSetExact
ENDIF
SELECT (lnCurAlias)

*:************************************************************************
*: Program file  : lfvPrjTmp
*: Program desc. : Browse function for avilable templates.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfvPrjTmp

PRIVATE lcTitle, lcBrFields, lnCurAlias

lnCurAlias   = SELECT(0)

*B119218,1 ALB Fix Bugs in Picture and validation function in template id field [BEGIN]
*SELECT PmRprTm
IF !USED('PMPTHHD')
  =gfOpenFile(gcDataDir+'PMPTHHD',gcDataDir+'PMPTHHD','SH')
ENDIF
SELECT PMPTHHD
SET ORDER TO PMPTHHD
*B119218,1 ALB Fix Bugs in Picture and validation function in template id field [END]

GO TOP
DIMENSION latemp[1]
*B119218,1 ALB Fix Bugs in Picture and validation function in template id field [BEGIN]
*lcBrFields = "cTmp_Code : H='Template ID',cTmp_Dsc  : H='Description'"
lcBrFields = "cPath_ID : H='Template ID',cPath_Dsc  : H='Description'"
*B119218,1 ALB Fix Bugs in Picture and validation function in template id field [END]

lcTitle    = 'Templates'
IF !EMPTY(lcRpTemplt) AND !SEEK(ALLTRIM(lcRpTemplt))
  *B119218,1 ALB Fix Bugs in Picture and validation function in template id field [BEGIN]
  *lcRpTemplt = IIF(ARIABROW('',lcTitle,;
                   gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PmRprTm.cTmp_Code','latemp'),;
                   PmRprTm.cTmp_Code, SPACE(3))
  lcRpTemplt = IIF(ARIABROW('',lcTitle,;
                   gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PMPTHHD.cPath_ID','latemp'),;
                   PMPTHHD.cPath_ID, SPACE(3))
  *B119218,1 ALB Fix Bugs in Picture and validation function in template id field [END]
ENDIF
IF EMPTY(lcRpTemplt)
  IF lcRpRptLy  = 'V'
    =gfModalGen('TRM38233B00000','DIALOG')
    lcRpRptLy  = 'H'
    lcRpOprTyp   = 'A'
    CLEAR READ
  ENDIF
ELSE
  *** Array to hold all template operations.
  DIMENSION laTmpltOpr[1]
  lcTmp_Memo = PmRprTm.mTmp_Oprt
  =gfStr2Ar(lcTmp_Memo,@laTmpltOpr,'|')
ENDIF
SELECT (lnCurAlias)


*:************************************************************************
*: Program file  : lfRepFrmV
*: Program desc. : Print report format Vertical layout.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
*: Report is X Wide
*: Master data base is the temp file.
*: Report layout :
*:*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3....+....4
*: Order# Account  Style        Quantity Req. Start  Req. Fnsh  Status  
*: ±±±±±± ±±±±±±±± ±±±±±±±±±±±±  ±±±±±±± ±±±±±±±±    ±±±±±±±±   ±±±±±±  
*:                                                              
*:                    ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± 
*:                    ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± 
*:                    ±±±±±±±±±±±±±±±±± ±±±±±±±±±±±±±±±±± 
*: Estimated dates  : ±±±±±±±± ±±±±±±±± ±±±±±±±± ±±±±±±±±       
*: Calculated dates : ±±±±±±±± ±±±±±±±± ±±±±±±±± ±±±±±±±±         
*: Actual dates     : ±±±±±±±± ±±±±±±±± ±±±±±±±± ±±±±±±±±     
*:*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....
*:************************************************************************
FUNCTION lfRepFrmV

IF !EMPTY(lcRpNotfy)
  DO CASE
    CASE lcRpOprStt = 'A'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify'
    CASE lcRpOprStt = 'V'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. lVoid'
    CASE lcRpOprStt = 'O'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. !lVoid'
  ENDCASE
ELSE
  lcForCond = IIF(lcRpOprStt = 'V','lVoid','!lVoid')
ENDIF

SELECT (lcTmpFile)

SCAN FOR lfvLines(lcRpPrjTp+cPrj_Id+cStyle,lcRpNotfy,lcRpOprStt)
  llPrint = .T.
  IF lnRow + 6 >= lnMax
    =lfSyPrjHdr()
  ENDIF
  =lfHdrData()
  lcRpPrj_ID = cPrj_Id
  
  SELECT cOprt_Dsc,dEst_Strt,dEst_Fnsh,dClc_Strt,dClc_Fnsh,dAct_Strt,dAct_Fnsh,0 AS lnStart;
    FROM PmPrjDt ;
    WHERE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = lcRpPrjTp +lcRpPrj_ID;
          .AND. ASCAN(laTmpltOpr,PmPrjDt.cOprt_Ctg+PmPrjDt.cOprt_Id) <> 0  ;
          .AND. &lcForCond ;
          .AND. IIF(lcRpRprt = 'C',!lshw2cust,.T.) ;
       INTO ARRAY laOprToPrn
    
  IF _TALLY <> 0
    lnRow    = lnRow + 2
    lnStrCol = 20

    FOR lnY = 1 TO 2
      FOR lnI = 1 TO ALEN(laOprToPrn,1)
        lnMaxChr = lfMaxChr()
        @ lnRow,lnStrCol SAY ALLTRIM(SUBSTR(laOprToPrn[lnI,1],laOprToPrn[lnI,8]+1,lnMaxChr))
        laOprToPrn[lnI,8] = laOprToPrn[lnI,8] + lnMaxChr
        lnStrCol = lnStrCol + 18
      ENDFOR
      lnRow    = lnRow + 1
      lnStrCol = 20
    ENDFOR
    
    *** Say the Estimated dates.
    @lnRow,1 SAY UPPER('Estimated dates  :')
    FOR lnI = 1 TO ALEN(laOprToPrn,1)
      @ lnRow,lnStrCol SAY laOprToPrn[lnI,2]
      lnStrCol = lnStrCol + 9
      @ lnRow,lnStrCol SAY laOprToPrn[lnI,3]
      lnStrCol = lnStrCol + 9
    ENDFOR
    lnRow    = lnRow + 1
    lnStrCol = 20
    
    *** Say the Calculated dates.
    @lnRow,1 SAY UPPER('Calculated dates :')
    FOR lnI = 1 TO ALEN(laOprToPrn,1)
      @ lnRow,lnStrCol SAY laOprToPrn[lnI,4]
      lnStrCol = lnStrCol + 9
      @ lnRow,lnStrCol SAY laOprToPrn[lnI,5]
      lnStrCol = lnStrCol + 9
    ENDFOR
    lnRow    = lnRow + 1
    lnStrCol = 20
    
    *** Say the Actual dates.
    @lnRow,1 SAY UPPER('Actual dates     :')
    FOR lnI = 1 TO ALEN(laOprToPrn,1)
      @ lnRow,lnStrCol SAY laOprToPrn[lnI,6]
      lnStrCol = lnStrCol + 9
      @ lnRow,lnStrCol SAY laOprToPrn[lnI,7]
      lnStrCol = lnStrCol + 9
    ENDFOR
    lnRow    = lnRow + 1
  ENDIF
ENDSCAN

*:************************************************************************
*: Program file  : lfRepFrmH
*: Program desc. : Print report format Horizontal layout.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
*: Report is Wide
*: Master data base is the temp file.
*: Report layout :
*:*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+
*: Order# Account  Style        Quantity Req. Start Req. Finish
*:     Task                                    Est. Start. Est. Finish. Clc. Start. Clc. Finish. Act. Start. Act. Finish. Status
*: -----------------------------------------------------------------------------------------------------------------------------
*: 1±±±±± 1±±±±±±± 17±±±±±±±±±±  31±±±±± 39±±±±±±   50±±±±±±                                                              
*: 123 12345678901234567890
*:    ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±± ±±±±±±±±    ±±±±±±±±     ±±±±±±±±    ±±±±±±±±     ±±±±±±±±    ±±±±±±±±     ±±±±±±±
*:    1234567890123456789012345678901234567890 12/45/78    12/45/78     12/45/78    12/45/78     12/45/78    12/45/78     1234567
*:    4±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±± 45±±±±±±    57±±±±±±     70±±±±±±    82±±±±±±     95±±±±±±    107±±±±±     120±±±±
*:    ±±±±±±(Details Part for each proj)±±±±±± ±±±±±±±±    ±±±±±±±±     ±±±±±±±±    ±±±±±±±±     ±±±±±±±±    ±±±±±±±±     ±±±±±±±
*:                                            
*:                                             
*:************************************************************************
FUNCTION lfRepFrmH

IF !EMPTY(lcRpNotfy)
  DO CASE
    CASE lcRpOprStt = 'A'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify'
    CASE lcRpOprStt = 'V'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. lVoid'
    CASE lcRpOprStt = 'O'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. !lVoid'
  ENDCASE
ELSE
  lcForCond = IIF(lcRpOprStt = 'V','lVoid','!lVoid')
ENDIF

SCAN FOR lfvLines(lcRpPrjTp+cPrj_Id+cStyle,lcRpNotfy,lcRpOprStt)
  llPrint = .T.
  IF lnRow >= lnMax
    =lfSyPrjHdr()
  ENDIF
  =lfHdrData()
  
  lnRow     = lnRow + 1
  lcOldCatg = SPACE(3)
  IF SEEK(CPRJ_TYP+CPRJ_ID+CSTYLE,'PmPrjDt')
    SELECT PmPrjDt

    SCAN REST FOR CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID =;
                  &lcTmpFile..CPRJ_TYP+&lcTmpFile..CPRJ_ID+&lcTmpFile..CSTYLE ;
                  .AND. IIF(lcRpOprTyp <> 'A',ASCAN(laTmpltOpr,PmPrjDt.cOprt_Ctg+PmPrjDt.cOprt_Id) <> 0,.t.) .AND. &lcForCond 
      
      IF lcRpRprt = 'C' AND !lshw2cust                   
        LOOP
      ENDIF                  
      
      IF lnRow >= lnMax
        =lfSyPrjHdr()
        =lfHdrData()
        lnRow = lnRow + 1
      ENDIF
      
      IF lcOldCatg <> PmPrjDt.cOprt_Ctg
        @ lnRow,001 SAY cOprt_Ctg + ' ' +;
                        IIF(SEEK(cOprt_Ctg,'PmCtgHd'),PmCtgHd.cCtg_Dsc,'')
        lcOldCatg = cOprt_Ctg
        lnRow     = lnRow + 1
      ENDIF

      IF dAct_Fnsh > dEst_Fnsh
        @ lnRow,000 SAY '*'
      ENDIF
      
      @ lnRow,004 SAY cOprt_Dsc
      @ lnRow,045 SAY dEst_Strt
      @ lnRow,057 SAY dEst_Fnsh
      @ lnRow,070 SAY dClc_Strt
      @ lnRow,082 SAY dClc_Fnsh
      @ lnRow,095 SAY dAct_Strt
      @ lnRow,107 SAY dAct_Fnsh
      *B130985,1 HBG 03/28/2006 Fix Bug of Scheduling report displays status ON TIME when project late [Begin]
      *@ lnRow,120 SAY IIF(dAct_Fnsh > dEst_Fnsh, 'LATE', 'ON TIME')
      @ lnRow,120 SAY IIF(dAct_Fnsh > &lcTmpFile..dReq_Fnsh OR dClc_Fnsh > &lcTmpFile..dReq_Fnsh, 'LATE', 'ON TIME')
      *B130985,1 HBG 03/28/2006 Fix [End]
      lnRow = lnRow + 1
    ENDSCAN
  ENDIF
ENDSCAN

*:************************************************************************
*: Program file  : lfSyPrjHdr
*: Program desc. : Function to say project header.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*: Layout        :
*:*....+....1....+....2....+....3....+....4....+....5....+....6....+....7
*: Order# Account  Style        Quantity Req. Start Req. Finish 
*: C/T#   Account  Style        Quantity Req. Start Req. Finish 
*: P/O#   Vendor   Style        Quantity Req. Start Req. Finish   STATUS
*:    Operation                                Est. Start. Est. Finish. Clc. Start. Clc. Finish. Act. Start. Act. Finish. Status
*:*....+....1....+....2....+....3....+....4....+....5....+....6....+....7
*:************************************************************************
FUNCTION lfSyPrjHdr

PageNo  = PageNo + 1
*--- SSH
DO CASE
  CASE lcRpPrjTp  = 'C'
    lcTitle = 'CUTTING TICKETS'
  CASE lcRpPrjTp  = 'P'      && PO
    lcTitle = 'PURCHASE ORDERS'
  CASE lcRpPrjTp  = 'A'      && Adorment Order
    lcTitle = 'ADORMENT ORDERS'   
  CASE lcRpPrjTp  = 'D'      && Dye Order 
    lcTitle = 'DYE ORDERS'     
  CASE lcRpPrjTp  = 'N'      && Inter-Location PO
    lcTitle = 'INTER-LOCATION PO'      
  CASE lcRpPrjTp  = 'R'      && Return PO
    lcTitle = 'RETURN PO'    
  CASE lcRpPrjTp  = 'O'      && SO
    lcTitle = 'SALES ORDERS'    
  CASE lcRpPrjTp  = 'T'      && EDI Order
    lcTitle = 'EDI ORDERS'  
ENDCASE
*hbg
IF lcRpRptLy = 'V'
  DO lfPrHeader WITH PAGENO
ELSE
*hbg
  DO Rpt_Hdr WITH XREPORT,'PROJECT MANAGEMENT SCHEDULING REPORT FOR '+lcTitle,R_WIDTH
*hbg  
ENDIF
=lfPreData()
*hbg
lnRow = 5
@ lnRow,01 SAY UPPER(lcField)+'#'
@ lnRow,08 SAY UPPER(lcAccOrVen)
@ lnRow,17 SAY 'STYLE' 
@ lnRow,30 SAY 'QUANTITY REQ. START REQ. FINISH' 

IF lcRpRptLy  = 'V'
  @ lnRow,64 SAY 'STATUS'
ENDIF

IF lcRpRptLy  = 'H'
  lnRow = lnRow + 1 
  @ lnRow,04 SAY UPPER('Task                                     Est. Start. Est. Finish. Clc. Start. Clc. Finish. Act. Start. Act. Finish. Status')
  lnRow = lnRow + 1 
  @ lnRow,01 SAY REPLICATE('-',132)
ENDIF

*:************************************************************************
*: Program file  : lfHdrData
*: Program desc. : Function to say project header data.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*: Layout        :
*:*....+....1....+....2....+....3....+....4....+....5....+....6....+....7
*: Order# Account  Style        Quantity Req. Start Req. Finish 
*: C/T#   Account  Style        Quantity Req. Start Req. Finish 
*: P/O#   Vendor   Style        Quantity Req. Start Req. Finish   STATUS
*: 1±±±±± 8±±±±±±± 17±±±±±±±±±±  30±±±±± 39±±±±±±   50±±±±±±      64±±±±±
*:*....+....1....+....2....+....3....+....4....+....5....+....6....+....7
*:************************************************************************
FUNCTION lfHdrData

lnRow = lnRow + 1 
IF lcRpRptLy  = 'V' .AND. &lcTmpFile..dAct_Fnsh > &lcTmpFile..dEst_Fnsh
  @ lnRow,00 SAY '*'
ENDIF

@ lnRow,01 SAY &lcTmpFile..cPrj_Id
IF lcAccCode = 'account'
  @ lnRow,08 SAY &lcTmpFile..account
ELSE
  IF lcAccCode = 'Vendor'
    @ lnRow,08 SAY &lcTmpFile..Vendor
  ENDIF
ENDIF  
@ lnRow,17 SAY IIF(&lcTmpFile..cStyle = REPLICATE('*',12),'All Styles',&lcTmpFile..cStyle)
@ lnRow,31 SAY lfGetQty()            PICTURE '9999999'
@ lnRow,39 SAY &lcTmpFile..dReq_Strt
@ lnRow,50 SAY &lcTmpFile..dReq_Fnsh

IF lcRpRptLy  = 'V'
  *HBG
  *@ lnRow,64 SAY IIF(&lcTmpFile..dAct_Fnsh > &lcTmpFile..dEst_Fnsh, 'LATE', 'ON TIME')
  @ lnRow,64 SAY IIF(&lcTmpFile..dAct_Fnsh > &lcTmpFile..dReq_Fnsh OR &lcTmpFile..dClc_Fnsh > &lcTmpFile..dReq_Fnsh, 'LATE', 'ON TIME')
  *HBG
ENDIF

*!*************************************************************
*! Name      : lfGetQty
*! Developer : HEND GHANEM (HBG)
*! Date      : 12/27/2001
*! Purpose   :
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfGetQty

DO CASE
  CASE lcRpPrjTp  $ 'OT'
    DIMENSION laRetVal[1]
    laRetVal = 0
    lcKey    = &lcTmpFile..cPrj_Id+&lcTmpFile..cStyle
    *hbg
    *SELECT SUM(TotQty) FROM OrdLine ;
      WHERE OrdLine.Order+OrdLine.Style = lcKey ;
      INTO ARRAY laRetVal
    *amh
    *SELECT SUM(TotQty) FROM OrdLine ;
      WHERE OrdLine.Order+OrdLine.Style = lcKey ;
      INTO ARRAY laRetVal
    lcKey = PADR(ALLTRIM(lcKey),25,'?')
    SELECT SUM(TotQty) FROM OrdLine ;
      WHERE LIKE(lcKey,OrdLine.Order+OrdLine.Style) ;
      INTO ARRAY laRetVal
    *amh
    *hbg  
    
    RETURN laRetVal[1]
  CASE lcRpPrjTp  = 'C'
    =SEEK(cPrj_Id,'CutTktH')
    RETURN CutTktH.Pcs_Opn
  CASE lcRpPrjTp  $ 'PADNR'
    DIMENSION laRetVal1[1]
    DIMENSION laRetVal2[1]
    STORE 0 TO laRetVal1,laRetVal2
    *hbg
    *lcKey    = lcRpPrjTp+&lcTmpFile..cPrj_Id+&lcTmpFile..cStyle
    lcKey     = &lcTmpFile..cPrj_Id+&lcTmpFile..cStyle
    *hbg
  
    *--- SSH Change
    lcPrvAlis = SELECT(0)
    SELECT POSLN
    *hbg
    *SCAN FOR cstytype+po+style+STR(lineno,6)+trancd = lcKey .AND. TranCd $ '1245'
    *amh
    *SCAN FOR cstytype+po+style = lcKey .AND. TranCd $ '1245'
    lcKey = PADR(ALLTRIM(lcKey),25,'?')
    SCAN FOR LIKE(lcKey,po+style) .AND. TranCd $ '1245'
    *amh
    *hbg
      IF TranCd = '1'
        laRetVal1[1] = laRetVal1[1] + PosLn.TotQty
      ELSE
        IF TranCd $ '245'
          laRetVal2[1] = laRetVal2[1] + PosLn.TotQty
        ENDIF
      ENDIF
    ENDSCAN
    SELECT (lcPrvAlis)

*    SELECT SUM(TotQty) FROM PosLn ;
      WHERE PosLn.PO+PosLn.Style = lcKey .AND. TranCd = '1';
      INTO ARRAY laRetVal1
    
*    SELECT SUM(TotQty) FROM PosLn ;
      WHERE PosLn.PO+PosLn.Style = lcKey .AND. TranCd $ '245';
      INTO ARRAY laRetVal2

     RETURN laRetVal1[1] - laRetVal2[1]
     
ENDCASE

*:************************************************************************
*: Program file  : lfMaxChr
*: Program desc. : Return the max number of characters to be printed with
*:               : out cutting words from its midle.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfMaxChr
PRIVATE lnStrLn, lnMax, lnFrom, lnTo

lnStrLn = 40                      && String length saved in file.
lnMax   = 17                      && Max avilable space to print on line.
lnFrom  = laOprToPrn[lnI,8] + 1   && Start from this charater.
lnTo    = lnFrom + lnMax -1       && End at this charater.

DO WHILE !EMPTY(RIGHT(SUBSTR(laOprToPrn[lnI,1],lnFrom,lnTo),1)) .AND. lnTo <> 0
  lnTo = lnTo - 1
ENDDO

IF lnTo > lnStrLn
  lnTo = lnStrLn
ENDIF
RETURN IIF(lnTo = 0,lnMax, lnTo - lnFrom)



*:************************************************************************
*: Program file  : lfFillSrBy
*: Program desc. : Fill Sort by Popup array
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfFillSrBy

DIMENSION laSortDesc[IIF(gcAct_Appl = "MF" , 2 , 3) ,1]
DIMENSION laSortVal[IIF(gcAct_Appl = "MF" , 2 , 3),1]

DO CASE
  CASE gcAct_Appl = "MF"
    laSortDesc[1,1] = 'Cut ticket'
    laSortVal[1,1]  = 'C'
    laSortDesc[2,1] = 'Required Date'
    laSortVal[2,1]  = 'R'

    lcPrjTitle  = 'Only these C/T #'    
    lcRpSrtBy   = 'C'

  CASE gcAct_Appl = "SO"
    laSortDesc[1,1] = 'Order'
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = 'Account'
    laSortVal[2,1]  = 'A'
    laSortDesc[3,1] = 'Required Date'
    laSortVal[3,1]  = 'R'

    lcPrjTitle  = 'Only these Orders'    
    lcRpSrtBy   = 'O'

  CASE gcAct_Appl = "PO"
    laSortDesc[1,1] = 'Order'
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = 'Vendor'
    laSortVal[2,1]  = 'V'
    laSortDesc[3,1] = 'Required Date'
    laSortVal[3,1]  = 'R'

    lcRpSrtBy   = 'O'
    lcPrjTitle  = 'Only these P/O #' 
ENDCASE


*:************************************************************************
*: Program file  : lfvPrjTyp
*: Program desc. : Project type valid function
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfvPrjTyp

DO CASE
  CASE lcRpPrjTp $ 'OT'
    lcInFile   = 'OrdHdr'
    lcField    = 'Order'
    lcAccCode  = 'account'
    lcAccOrVen = 'account'

    DIMENSION laSortDesc[3,1] 
    DIMENSION laSortVal[3,1]

    laSortDesc[1,1] = 'Order'
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = 'Account'
    laSortVal[2,1]  = 'A'
    laSortDesc[3,1] = 'Required Date'
    laSortVal[3,1]  = 'R'

    lcPrjTitle  = 'Only these Orders'    
    lcRpSrtBy   = 'O'

  CASE lcRpPrjTp = 'C'
    lcInFile   = 'CutTktH'
    lcField    = 'CutTkt'
    lcAccCode  = 'account'
    lcAccOrVen = ''

    DIMENSION laSortDesc[2,1] 
    DIMENSION laSortVal[2,1]

    laSortDesc[1,1] = 'Cut ticket'
    laSortVal[1,1]  = 'C'
    laSortDesc[2,1] = 'Required Date'
    laSortVal[2,1]  = 'R'

    lcPrjTitle  = 'Only these C/T #'    
    lcRpSrtBy   = 'C'

  CASE lcRpPrjTp $ 'PADNR'
    lcInFile   = 'POsHdr'
    lcField    = 'PO'
    lcAccCode  = 'Vendor'
    lcAccOrVen = 'vendor'

    DIMENSION laSortDesc[3,1] 
    DIMENSION laSortVal[3,1]

    laSortDesc[1,1] = 'Order'
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = 'Vendor'
    laSortVal[2,1]  = 'V'
    laSortDesc[3,1] = 'Required Date'
    laSortVal[3,1]  = 'R'
    lcRpSrtBy   = 'O'
    lcPrjTitle  = 'Only these P/O #' 
    
ENDCASE

CLEAR READ

*:************************************************************************
*: Program file  : lfPreData
*: Program desc. : Project type valid function
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfPreData

DO CASE
  CASE lcRpPrjTp $ 'OT'
    lcInFile   = 'OrdHdr'
    lcField    = 'Order'
    lcAccCode  = 'account'
    lcAccOrVen = 'account'

    DIMENSION laSortDesc[3,1] 
    DIMENSION laSortVal[3,1]

    laSortDesc[1,1] = 'Order'
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = 'Account'
    laSortVal[2,1]  = 'A'
    laSortDesc[3,1] = 'Required Date'
    laSortVal[3,1]  = 'R'

    lcPrjTitle  = 'Only these Orders'    
    lcRpSrtBy   = 'O'

  CASE lcRpPrjTp = 'C'
    lcInFile   = 'CutTktH'
    lcField    = 'CutTkt'
    lcAccCode  = 'account'
    lcAccOrVen = ''

    DIMENSION laSortDesc[2,1] 
    DIMENSION laSortVal[2,1]

    laSortDesc[1,1] = 'Cut ticket'
    laSortVal[1,1]  = 'C'
    laSortDesc[2,1] = 'Required Date'
    laSortVal[2,1]  = 'R'

    lcPrjTitle  = 'Only these C/T #'    
    lcRpSrtBy   = 'C'

  CASE lcRpPrjTp $ 'PADNR'
    lcInFile   = 'POsHdr'
    lcField    = 'PO'
    lcAccCode  = 'Vendor'
    lcAccOrVen = 'vendor'

    DIMENSION laSortDesc[3,1] 
    DIMENSION laSortVal[3,1]

    laSortDesc[1,1] = 'Order'
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = 'Vendor'
    laSortVal[2,1]  = 'V'
    laSortDesc[3,1] = 'Required Date'
    laSortVal[3,1]  = 'R'
    lcRpSrtBy   = 'O'
    lcPrjTitle  = 'Only these P/O #' 
    
ENDCASE

*!*************************************************************
*! Name      : lfvStyMaj
*! Developer : Hend Ghanem
*! Date      : 31/12/2001
*! Purpose   : Validate style major in range entered in grid.
*!*************************************************************
*! Calls     : gfStyBrw()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvStyMaj()
*!*************************************************************
FUNCTION lfvStyMaj

lcMStyle = EVALUATE(SYS(18))

lcSty = STRTRAN(lcMStyle," ","")

IF EMPTY(lcSty)
  lcMStyle = ""
ENDIF

IF !EMPTY(lcOldValue) AND lcMStyle = lcOldValue
  RETURN
ENDIF

lcFldLocNam = SYS(18)

IF !EMPTY(lcMStyle) AND !SEEK(lcMStyle,'STYLE')
  lcMStyle = gfStyBrw('M',"","",.F.)
  &lcFldLocNam = lcMStyle
ENDIF

RETURN

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Hend Ghanem
*! Date      : 31/12/2001
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN


*!*************************************************************
*! Name      : lfvAccount
*! Developer : Hend Ghanem
*! Date      : 31/12/2001
*! Purpose   : Valid function for account
*!*************************************************************
FUNCTION  lfvAccount

lcAccount = EVALUATE(SYS(18))

lcAcont = STRTRAN(lcAccount ," ","")

IF EMPTY(lcAcont)
  lcAccount = ""
ENDIF

IF !EMPTY(lcOldValue) AND lcAccount = lcOldValue
  RETURN
ENDIF

lcFldLocNam = SYS(18)

IF !EMPTY(lcAccount) !SEEK('M'+lcAccount,Customer)
  DO CusBrowM WITH lcAccount 
  &lcFldLocNam = lcAccount
ENDIF

*!*************************************************************
*! Name      : lfvVendor
*! Developer : Hend Ghanem
*! Date      : 31/12/2001
*! Purpose   : Valid function for Vendor
*!*************************************************************
FUNCTION  lfvVendor

lcVendor = EVALUATE(SYS(18))

lcVend = STRTRAN(lcVendor," ","")

IF EMPTY(lcVend)
  lcVendor = ""
ENDIF

IF !EMPTY(lcOldValue) AND lcVendor = lcOldValue
  RETURN
ENDIF

lcFldLocNam = SYS(18)

IF !EMPTY(lcVendor) AND !SEEK(lcVendor ,'ApVendor')
  =gfApVnBrow(@lcVendor) 
  &lcFldLocNam = lcVendor
ENDIF

*!*************************************************************
*! Name      : lfvRpLay
*! Developer : Hend Ghanem
*! Date      : 31/12/2001
*! Purpose   : Valid function for Report layout
*!*************************************************************
FUNCTION lfvRpLay

IF lcRpRptLy = 'V'
  lcRpOprtyp = 'S'
ELSE
  lcRpOprtyp = 'A'  
EnDIF  
CLEAR READ

*!*************************************************************
*! Name      : lfvSelOpr
*! Developer : Hend Ghanem
*! Date      : 31/12/2001
*! Purpose   : Valid function for Selected operation
*!*************************************************************
FUNCTION lfvSelOpr

IF lcRpOprtyp = 'S'
  lcRpRptLy = 'V'
ELSE
  lcRpRptLy = 'H'
EnDIF  
CLEAR READ

*!*************************************************************
*! Name      : lfPrHeader
*! Developer : Wael Mohammed Anwar
*! Date      : 05/21/2000
*! Purpose   : Report header with 93 char 
*!*************************************************************
*! Called from : lpSCPrn
*!*************************************************************
*! Passed Parameters : HPAGENO
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     :  DO lfPrHeader WITH PAGENO
*!*************************************************************
*
PROCEDURE lfPrHeader
PARAMETERS HPAGENO  


*DO Rpt_Hdr WITH XREPORT,'PROJECT MANAGEMENT SCHEDULING REPORT FOR '+lcTitle,R_WIDTH

* MFTRGDT                                         Demo Company                                     05/20/00 ~
* 12:15:58                           PROJECT MANAGEMENT SCHEDULING REPORT                        PAGE#    1
*                          PROJECT MANAGEMENT SCHEDULING REPORT FOR PURCHASE ORDERS              
* ************************************************************************************************************                      

@ 01,00  SAY 'MFTRGDT'
@ 01,106 SAY gccom_name
@ 01,188 SAY gdsysdate
@ 02,00  SAY TIME()
@ 02,93  SAY 'PROJECT MANAGEMENT SCHEDULING REPORT'
@ 02,188 SAY 'PAGE#' 
@ 02,194 SAY HPAGENO PICTURE '9999'
@ 03,83  SAY 'PROJECT MANAGEMENT SCHEDULING REPORT FOR '+lcTitle
@ 04,00  SAY REPLICATE('*',200)
*-- end of lfPrHeader.


