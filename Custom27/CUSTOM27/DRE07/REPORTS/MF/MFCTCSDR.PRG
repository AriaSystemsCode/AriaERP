*:************************************************************************
*: Program file  : MFCTCSDR.Prg
*: Program desc. : CUTTING TICKET COST SHEET REPORT FOR Dream Apparel
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MF
*: Developer     : Ashraf Sherif Mohammed
*: Date          : 10/28/2003
*: REF           : *C119694,1
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetTit(), lfwOGWhen()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO MFCTCSDR
*:************************************************************************
*:Modifications:
*****************************************************************************
QTOPIC='CUTTING TICKET COST SHEET REP.'

* Get the Dyelot setting
XDYELOT_S = gfGetMemvar('M_DYELOT',gcAct_Comp)  ='Y'
STORE SPACE(0) TO PSEASON, PDIVISION, XPRM_VEND, XSCALE, XPRIM_FAB
STORE SPACE(30) TO lcLong1, lcLong2, lcLong3, lcLong4, lcLong5
*lcLong1                && Fabric long title type 1.
*lcLong2                && Trim   long title type 2.
*lcLong3                && MfgOp. long title type 3.
*lcLong4                && Style comp . long title type 3.
*lcLong5                && Misli. long title type 5.

* Get the cost elements titles
Dimension laCost[10,2]
laCost[1,1]  = 'M_CMTYPE1 '
laCost[2,1]  = 'M_CMTYPE2 '
laCost[3,1]  = 'M_CMTYPE3 '
laCost[4,1]  = 'M_CMTYPE4 '
laCost[5,1]  = 'M_CMTYPE5 '
laCost[6,1]  = 'M_CMSLBL1 '
laCost[7,1]  = 'M_CMSLBL2 '
laCost[8,1]  = 'M_CMSLBL3 '
laCost[9,1]  = 'M_CMSLBL4 '
laCost[10,1] = 'M_CMSLBL5 '
=gfGetMemvar(@laCost,gcAct_Comp)

* Get the setting  of STYLE MARKUP
M_STYMARK = gfGetMemvar('M_STYMARK',gcAct_Comp)

FOR lnI = 1 TO 5
  lcI = ALLTRIM(STR(lnI))
  lcLong&lcI = laCost[lnI+5,2]
ENDFOR

R_TITLE  = 'CUTTING TICKET COST SHEET REPORT'
XREPORT  = 'MFCTCSDR'
R_WIDTH  = 'W'

SELECT CUTTKTH
SET RELATION TO STYLE INTO STYLE
CTKTEMP  = GFTEMPNAME()
lcTmpOPH = GFTEMPNAME()

IF OPENED('&CTKTEMP')
  SELECT &CTKTEMP
  USE
  ERASE &GCWORKDIR.&CTKTEMP
ENDIF

IF EMPTY(lcRpExp)
  lcRpExp='CUTTKTH.PCS_BUD <> 0'
ELSE
  lcRpExp = '('+lcRpExp +')' + ' .AND. CUTTKTH.PCS_BUD <> 0'
ENDIF

*Add the filter of status
IF lcStatus <> "L"
  lcRpExp = '(' +lcRpExp+ ") .AND. (CUTTKTH.Status = '" + lcStatus +  "')"
ENDIF

SELECT CUTTKTH
*-- To get the selected C/T if any.
lcCutFile = ''
llUseCut = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CUTTKT'),1)
IF lnPosition > 0
  lcCutFile = laOGFxFlt[lnPosition,6]
  llUseCut = IIF(!EMPTY(lcCutFile) .AND. USED(lcCutFile) .AND. RECCOUNT(lcCutFile)>0,.T.,.F.)
ENDIF

=AFIELDS(laFileStru)
FOR lnCount=1 TO 3
  lcCount = STR(lnCount,1)
  lnPos   = ASCAN(laFileStru,'MFG_OPR'+lcCount,1)
  
  * If MFG_OPR1,MFG_OPR2,MFG_OPR3 fields removed from the file for any 
  * reason, create them in the temporary FILE
  IF lnPos = 0
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'MFG_OPR'+lcCount
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 6
    laFileStru[lnFileStru,4] = 0
  ELSE
    laFileStru[lnPos+2] = 6
  ENDIF
  
  lnPos   = ASCAN(laFileStru,'CONTR'+lcCount,1)
  IF lnPos = 0
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'CONTR'+lcCount
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 8
    laFileStru[lnFileStru,4] = 0
  ENDIF

ENDFOR

Create Table (gcWorkDir+CTKTEMP) FROM ARRAY laFileStru
INDEX ON CUTTKT TAG &CTKTEMP

CREATE CURSOR (lcTmpOPH) (CTKTNO C(6), cOperSeq C(2),cOprCode C(6),cContCode C(8) )
INDEX ON CTKTNO+cOperSeq+cOprCode TAG &lcTmpOPH OF &lcTmpOPH
SELECT CUTTKTH
IF llUseCut
  SELECT (lcCutFile)
  LOCATE
  SET RELATION TO CUTTKT INTO CUTTKTH ADDITIVE
ENDIF
SCAN FOR &lcRpExp
  SELECT CUTTKTH
  llVend  = .F.
  * Get first 3 operations in sequence applied to the CUTTKT, and get their 
  * contractors
  IF SEEK('M'+CUTTKTH.CUTTKT,'MFGOPRHD')
    SELECT (lcTmpOPH)
    ZAP  
    SELECT MFGOPRHD
    SCAN WHILE cTktNo = CUTTKTH.CUTTKT FOR !lInHouse
      SCATTER FIELDS CTKTNO,cOperSeq,cOprCode,cContCode MEMVAR
      INSERT INTO (lcTmpOPH) FROM MEMVAR
    ENDSCAN
    SELECT (lcTmpOPH)
    GO TOP
    IF !EOF()
      lnCount = 1
      DO WHILE lnCount <= 3 
        lcCOUNT = STR(lnCount,1)
        *-- If contractor of any operation match the required contractor set the
        *-- flag to add this cuttkt to the report data.
        llVend  = IIF(!llVend .AND. !EMPTY(lcRpVend),lcRpVend = cContCode,llVend)
        IF lnCount <= 3 
          m.CONTR&lcCount   = cContCode
          m.MFG_OPR&lcCount = COPRCODE
        ENDIF
        lnCount = lnCount+1
        SKIP
        IF EOF()
          EXIT
        ENDIF         
      ENDDO
    ELSE
      STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, ;
                        m.MFG_OPR2, m.MFG_OPR3
    ENDIF
  ELSE
    STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, ;
                      m.MFG_OPR2, m.MFG_OPR3
  ENDIF
  IF EMPTY(lcRpVend) .OR. llVend
    SELECT CUTTKTH
    SCATTER MEMVAR MEMO
    INSERT INTO (CTKTEMP) FROM MEMVAR
  ENDIF
ENDSCAN

IF llUseCut
  SELECT (lcCutFile)
  SET RELATION TO
ENDIF
SELECT (CTKTEMP)
*ash1
IF lcCostM = 'L' 
  SCAN
    SELECT BOMLINE
    lcOrder = ORDER()
    SET ORDER TO TAG Mfgopr 
    IF SEEK('M'+&CTKTEMP..CutTkt)
      LOCATE REST WHILE cimtyp+ctktno+coprcode+ctype+style+sclr+IIF(ccatgtyp$"MDP",PADR(mfgcode,12),item)+iclr='M'+&CTKTEMP..CutTkt FOR cType='2' AND EMPTY(cRSession) 
      IF !FOUND()
        SELECT (CTKTEMP)
        REPLACE COWNER WITH 'ARIADEL'
      ENDIF
    ELSE
      SELECT (CTKTEMP)
      REPLACE COWNER WITH 'ARIADEL'
    ENDIF
  ENDSCAN
  SELECT BOMLINE
  SET ORDER TO &lcOrder
  SELECT (CTKTEMP)
  DELETE ALL FOR COWNER =  'ARIADEL'
ENDIF
GO TOP
IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

*---------------------------------------------------------
* [3] SELECT REPORT FILE & INITIALIZE MEMORY
*---------------------------------------------------------
DO WHILE .T.
  STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XGTOT1,XGTOT2
  PAGENO = 0
  ROW    = 99
  XTIME  = TIME()
  SELECT &CTKTEMP
  GOTO TOP
  R_WIDTH = 'W'
  CLEAR TYPEAHEAD
  SET DEVICE TO PRINT
  *---------------------------------------------------------
  * [REPORT] LOOP
  *---------------------------------------------------------
  DO WHILE .T. 
    IF EOF()
      EXIT
    ENDIF

    IF ROW >=51
      PAGENO = PAGENO+1
      DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
      @ 05,00 SAY 'TICKET STYLE.............. ......DESC..... .....SEASON...... .....DIVISION.... ....................SIZE SCALE.........................'
      @ 06,00 SAY 'STATUS   ENTERED    CONTR1   .............NAME............. ..MFG. ..........OPERATION..........     RATE PATTERN    TYPE'
      @ 07,00 SAY 'ORDER    COMPLETE   CONTR2   .............NAME............. ..MFG. ..........OPERATION..........     RATE VENDOR     ACT.QTY'
      @ 08,00 SAY 'ACCOUNT             CONTR3   .............NAME............. ..MFG. ..........OPERATION..........     RATE '
      ROW = 09
    ENDIF

    IF ROW >=50
      ROW = 99
      LOOP
    ENDIF

    SELECT &CTKTEMP
    XSEASON   = SEASON
    XDIVISION = CDIVISION
    XVEN1 = SPACE(1)
    XVEN2 = SPACE(1)
    XVEN3 = SPACE(1)
    XOPER1= SPACE(1)
    XOPER2= SPACE(1)
    XOPER3= SPACE(1)
   
    IF !EMPTY(CONTR1)
      IF SEEK (&CTKTEMP..CONTR1,'APVENDOR')
        XVEN1 = APVENDOR.cVenComp
      ENDIF
      XOPER1 = gfCodDes(&CTKTEMP..MFG_OPR1, 'MFGCODE')
    ENDIF

    IF !EMPTY(CONTR2)
      IF SEEK (&CTKTEMP..CONTR2,'APVENDOR')
        XVEN2 = APVENDOR.cVenComp
      ENDIF
      XOPER2 = gfCodDes(&CTKTEMP..MFG_OPR2, 'MFGCODE')        
    ENDIF

    IF !EMPTY(CONTR3)
      IF SEEK (&CTKTEMP..CONTR3,'APVENDOR')
        XVEN3 = APVENDOR.cVenComp
      ENDIF
      XOPER3 = gfCodDes(&CTKTEMP..MFG_OPR3, 'MFGCODE')        
    ENDIF

    IF !EMPTY(XSEASON)
      PSEASON   = SUBSTR(gfCodDes(XSEASON, 'SEASON'),1,17)
    ENDIF
    IF !EMPTY(XDIVISION)
      PDIVISION = SUBSTR(gfCodDes(XDIVISION, 'CDIVISION'),1,17)
    ENDIF
    SELECT &CTKTEMP
    @ ROW,000 SAY REPLICATE('-',132)
    ROW = ROW+1
    IF SEEK(ALLTRIM(&CTKTEMP..STYLE) , 'STYLE')
      XSCALE    = GETSCALE(STYLE.SCALE,SPACE(1))
      XPRIM_FAB = STYLE.FABRIC      && PRIMARY FABRIC TO GET IT'S VENDOR FOR ROBYN
    ENDIF
    DO CASE
      CASE XPRICE_LVL ='B'
        XSELLPRICE = STYLE->PRICEB
      CASE XPRICE_LVL ='C'
        XSELLPRICE = STYLE->PRICEC
      OTHERWISE
        XSELLPRICE = STYLE->PRICEA
    ENDCASE
    IF 'MA' $ gccmpmodul
      SELECT FABRIC
      SEEK XPRIM_FAB
      XPRM_VEND = VENDOR
    ENDIF
    SELECT &CTKTEMP
 
    *** LINE 1 ***
    lcPieces=0
    DO CASE
      CASE Status='O'
        lcPieces = Pcs_Bud - Pcs_Can
      CASE Status='C'
        lcPieces = Pcs_Rec
      CASE Status='S'
        lcPieces = Pcs_Rec
      CASE Status='A'
        lcPieces = Pcs_Act
      CASE Status='H'
        lcPieces = Pcs_Opn
    ENDCASE          
    WAIT WINDOW 'Printing CT # '+CUTTKT  NOWAIT
    @ ROW,000 SAY CUTTKT
    @ ROW,007 SAY STYLE
    @ ROW,027 SAY LEFT(STYLE.DESC1,15)
    @ ROW,042 SAY PSEASON
    @ ROW,061 SAY PDIVISION
    @ ROW,079 SAY STYLE.SCALE
    @ ROW,083 SAY XSCALE
    ROW = ROW+1

    *** LINE 2 ***
    DO CASE
      CASE STATUS = 'O'
        @ ROW,00 SAY 'OPEN'
      CASE STATUS = 'C'
        @ ROW,00 SAY 'CMPLTE'
      CASE STATUS = 'X'
        @ ROW,00 SAY 'CANCEL'
      CASE STATUS = 'H'
        @ ROW,00 SAY 'HOLD'
      CASE STATUS = 'A'
        @ ROW,00 SAY 'ACTUAL'
      CASE STATUS = 'S'
        @ ROW,00 SAY 'CLOSE'
    ENDCASE
    @ ROW,009 SAY ENTERED
    @ ROW,020 SAY CONTR1
    @ ROW,029 SAY XVEN1
    @ ROW,060 SAY MFG_OPR1
    @ ROW,067 SAY XOPER1
    *B802186,1 AMM Print the corresponding to the field RATE in 2.7 and adjust
    *@ ROW,098 SAY PATTERN
    *@ ROW,109 SAY cTktType
    SET ORDER TO CTKTYP IN CTKTBOM
    IF SEEK('M'+CUTTKT+SPACE(25)+MFG_OPR1 ,'Ctktbom')
      @ ROW,098 SAY CTktBom.UntCost
    ENDIF
    @ ROW,106 SAY PATTERN
    @ ROW,117 SAY cTktType
    ROW = ROW+1

    *** LINE # 3
    IF SEEK('1'+CUTTKT,'CUTPICK')
      @ ROW,000 SAY CUTPICK.ORDER
    ENDIF
    @ ROW,009 SAY COMPLETE
    @ ROW,020 SAY CONTR2
    @ ROW,029 SAY XVEN2
    @ ROW,060 SAY MFG_OPR2
    @ ROW,067 SAY XOPER2
    SET ORDER TO CTKTYP IN CTKTBOM
    IF SEEK('M'+CUTTKT+SPACE(25)+MFG_OPR2 ,'Ctktbom')
      @ Row,098 SAY CTktBom.UntCost
    ENDIF
    @ ROW,106 SAY XPRM_VEND
    @ ROW,117 SAY lcPieces PICTURE '9999999'
    ROW = ROW+1

    *** LINE # 4
    IF !EOF('CUTPICK') .AND. 'SO' $ gccmpmodul .AND. ;
                             SEEK('O'+CUTPICK.ORDER,'ORDHDR')
      @ ROW,000 SAY ORDHDR.ACCOUNT
    ELSE
      @ ROW,000 SAY 'TO STOCK'
    ENDIF
    IF !EMPTY(DEL_DATE)
      @ ROW,009 SAY DEL_DATE
    ENDIF
    @ ROW,020 SAY CONTR3
    @ ROW,029 SAY XVEN3
    @ ROW,060 SAY MFG_OPR3
    @ ROW,067 SAY XOPER3
    SET ORDER TO CTKTYP IN CTKTBOM
    IF SEEK('M'+CUTTKT+SPACE(25)+MFG_OPR3 ,'Ctktbom')
      @ Row,098 SAY CTktBom.UntCost
    ENDIF

    ROW = ROW+1

    *** CHECK IF THIS CUTTKT# HAS A BOM ON FILE ****
    XCUTTKT=CUTTKT
    xAct_Qty = 0
    DO CASE
      CASE Status='O'
        xAct_Qty = Pcs_Bud - (Pcs_Can + Pcs_Rec + Pcs_Dam)
      CASE Status='C'
        xAct_Qty = Pcs_Rec
      CASE Status='S'
        xAct_Qty = Pcs_Rec
      CASE Status='A'
        xAct_Qty = Pcs_Act
      CASE Status='H'
        xAct_Qty = Pcs_Opn
    ENDCASE 
    SET ORDER TO CTKTBOM IN CTKTBOM
    IF SEEK('M'+XCUTTKT,'CTKTBOM')
      SELECT CTKTBOM
    ELSE
      SELECT &CTKTEMP
      SKIP
      LOOP
    ENDIF
    ******* CUTTKT BOM FOUND SO PRINT IT FOR THIS CUTTING TICKET *******
    IF  XDYELOT_S
      @ ROW,003 SAY REPLICATE('=',124)
      ROW=ROW+1
      *ASH1
      *@ ROW,004 SAY 'ITEM                COLOR  .......DESC......... DYELOT       UNT COST     YARDGE  EXTD. COST AVG.    YIELD  AVG.COST/PIECE'
      @ ROW,004 SAY 'ITEM                COLOR  .......DESC......... DYELOT        UNT COST     YARDGE  EXTD. COST AVG.    YIELD  AVG.COST/PIECE' 
    ELSE
      @ ROW,015 SAY REPLICATE('=',112)
      ROW=ROW+1
      @ ROW,016 SAY 'ITEM                COLOR ......DESC..........    UNT COST      YARDGE      EXTD. COST AVG.YIELD AVG.COST/PIECE'      
    ENDIF

    ROW=ROW+1
    SELECT CTKTBOM
    XPRV_TYP   = SPACE(1)
    OldTyp     = SPACE(1)
    lcOldCtg   = cCatgtyp

    DO WHILE .T.
      IF ROW >= 55
        PAGENO = PAGENO + 1
        DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
        IF XDYELOT_S
          @ 005,004 SAY 'ITEM                COLOR  .......DESC......... DYELOT       UNT COST     YARDGE  EXTD. COST AVG.   YIELD  AVG. COST/PIECE'
        ELSE
          @ 05,016 SAY 'ITEM                COLOR ......DESC.......... UNT COST       YARDGE      EXTD. COST AVG.YIELD AVG.COST/PIECE'
        ENDIF
        ROW=06
        XPRV_TYP=SPACE(1)
        OldTyp  =SPACE(1)
      ENDIF
      DO WHILE (!EOF()) .AND. (CUTTKT=XCUTTKT)
        IF IIF(lcOldCtg='M',OldTyp <> TYP,lcOldCtg <> cCatgTyp)
          XTOT3= IIF(XACT_QTY<>0,XTOT1/XACT_QTY,0)
          IF lcCostM <> 'L' 
            XTOT4= IIF(XACT_QTY<>0,XTOT2/XACT_QTY,0)
          ENDIF
          IF TYPE('lcNoToPrn') <> 'U'
            @ ROW,20 SAY ALLTRIM(EVAL('lcLong'+lcNoToPrn)) + ' TOTAL ==='+REPL('=',30-LEN(ALLTRIM(EVAL('lcLong'+lcNoToPrn)))) +'>'
            *ash1
            @ ROW,74 SAY XTOT1 PICTURE '99999999.999'
            @ ROW,88 SAY XTOT2 PICTURE '9999999999.999'
            IF lcOldCtg $ 'FTS'
              @ ROW,98 SAY XTOT3 PICTURE '99999.999'
            ENDIF
            @ ROW,114 SAY XTOT4 PICTURE '999999999.999'
          ENDIF
          lcOldCtg   = cCatgTyp       &&TYP            && BREAK FOR SUBTOTALS
          XGTOT1 = XGTOT1 + XTOT2
          XGTOT2 = XGTOT2 + XTOT4
          STORE 0 TO XTOT1,XTOT2,XTOT4,XTOT3
          ROW=ROW+1
        ENDIF
        IF IIF(lcOldCtg='M',OldTyp <> TYP,XPRV_TYP <> cCatgTyp)
          lcHdrToSay = '< ' +ALLTRIM(EVAL('lcLong'+Typ))+' >'
          @ ROW,IIF(XDYELOT_S,04,16) SAY PADC(lcHdrToSay,IIF(XDYELOT_S,122,111),'-')
          ROW=ROW+1
          XPRV_TYP  = cCatgTyp         &&TYP
          lcOldCtg  = cCatgTyp
          lcNoToPrn = TYP
          OldTyp    = TYP
        ENDIF
        *IF lcCostM  = 'A'
        IF lcCostM  $ 'AL'
          STORE 0 TO lnQty,lnUntCst,lnFound
          IF SEEK('M'+xCutTkt,'BomCost')
            SELECT BomCost
             SCAN REST WHILE cImTyp+cTktNo+Actualize+cBomType+Item+IClr+MfgCode+cOprCode+cLotNo+cISession+cRSession =;
                             'M'+xCutTkt ;
                  FOR CBOMTYPE+ITEM+ICLR+cDyelot+MFGCODE = CTKTBOM.TYP+CTKTBOM.ITEM+CTKTBOM.ICLR+CTKTBOM.Dyelot+CTKTBOM.MFGCODE
              lnUntCst = lnUntCst + BomCost.nUnitACst * BomCost.nTotQty
              lnQty    = lnQty    + BomCost.nTotQty
              lnFound  = lnFound + 1
            ENDSCAN
          ENDIF            
          IF lnFound = 0
            lnUntCst  = CTKTBOM.UntCost
          ELSE
            lnUntCst = IIF(lnQty <> 0 , lnUntCst / lnQty,0)
          ENDIF
          SELECT CTKTBOM
        ELSE
          lnUntCst  = UntCost
          lnQty     = Req_Qty
        ENDIF
        *ash1
        IF lcCostM = 'L' 
          xAvg_Yld = 0
          xAvg_Pcs = 0
          llAct    = .F.
          lcCType  = ' '
          lcBType  = ' '
          lnMfgCnt = 0
          lnUntCst = 0
          SELECT BOMLINE
          lcOrder = ORDER()
          SET ORDER TO TAG Mfgopr 
          IF SEEK('M'+CTKTBOM.CutTkt)
            SCAN WHILE cimtyp+ctktno = 'M'+CTKTBOM.CutTkt FOR (Item + IClr + MfgCode= CTKTBOM.ITEM+CTKTBOM.ICLR+CTKTBOM.MFGCODE)
              IF cType='2' AND EMPTY(cRSession) 
                  xAvg_Pcs = unitqty*unitcost
                  XTOT4 = XTOT4 + xAvg_Pcs
                ENDIF

              IF SEEK(ctktno,'CUTTKTH') AND CUTTKTH.Status = 'A' AND EMPTY(MfgCode)
                llAct = .T.    
                =lfCompAct()
                SELECT BOMLINE
              ELSE
              IF !EMPTY(MfgCode) AND EMPTY(CRSESSION) AND cType = '2'
                lnQty = BOMLINE.UnitQty
              ENDIF
              IF CTYPE<>lcCType OR CBOMTYP <> lcBType
                lnUntCst = 0
              ENDIF
              lnMfgCnt = IIF(CTYPE<>lcCType,1,IIF(EMPTY(CRSESSION),lnMfgCnt,lnMfgCnt + 1))
              lnUntCst = lnUntCst + IIF(EMPTY(CRSESSION),UNITCOST,0)
              lcCType  = CTYPE                
              lcBType  = CBOMTYP
              ENDIF
            ENDSCAN
            lnUntCst = IIF(lnMfgCnt<>0 AND !llAct,lnUntCst/lnMfgCnt,lnUntCst)            
          ENDIF
          SET ORDER TO &lcOrder
        ENDIF
        *ash1
        *xExt_Cst = ROUND(lnUntCst*lnQty,2)
        xExt_Cst = ROUND(lnUntCst,4)*ROUND(lnQty,4)
        *--intialise variables :-
		*--lnBudjet-----> budjet qty  when TranCd = '1'
		*--lnReciev-----> Recived qty when TranCd = '2' 
		*--lnDamje -----> Damaje qty  when TranCd = '3'
		*--lnCancel-----> Cancel Qty  when TranCd = '4'
        STORE 0 TO lnBudjet,lnReciev,lnDamje,lnCancel
        SELECT BOMLINE
        IF SEEK('M'+'1'+CTKTBOM.CutTkt)
          IF XPRV_TYP = 'M'
            *B605317,1 KHM 01/28/2002 (Begin) Adding the checking of the type
            lcExpr = [REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode= 'M'+'1'+CTKTBOM.CutTkt]+;
                     [ FOR mfgCode = CTKTBOM.mfgCode .AND. cType = '1' .AND. cBomTyp = cTktBom.Typ]
          ELSE
            lcExpr = [REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode = "M"+"1"+CTKTBOM.CutTkt]+;
                     [ FOR ITEM = CTKTBOM.ITEM AND IClr = CTKTBOM.IClr .AND. ]+;
                     [cType = '1' .AND. cCatgTyp = CTKTBOM.cCatgTyp .AND. ]+;
                     [cBomTyp = cTktBom.Typ]
          ENDIF
          SCAN &lcExpr
            SELECT CUTTKTL
            =SEEK(CTKTBOM.CutTkt+BOMLINE.STYLE)
            SCAN REST WHILE CutTkt+Style+Dyelot+TranCd = CTKTBOM.CutTkt+BOMLINE.STYLE
              lnBudjet = lnBudjet + IIF(TranCd = '1',TotQty,0)
              lnReciev = lnReciev + IIF(TranCd = '2',TotQty,0)
              lnDamje  = lnDamje  + IIF(TranCd = '3',TotQty,0)
              lnCancel = lnCancel + IIF(TranCd = '4',TotQty,0)
            ENDSCAN
          ENDSCAN
        ENDIF
        *--determin the devided qty depend on the cuttkt status
        lnPieces=0
        DO CASE
          CASE &cTktemp..Status='O'			&& the cuttkt status is 'OPEN'
            lnPieces = lnBudjet - ( lnCancel + lnReciev + lnDamje)
          CASE &cTktemp..Status $ 'CS'		&& the cuttkt status is 'Complete','CLOSE','ACTUALISE'
            lnPieces = lnReciev
         CASE &cTktemp..Status = 'A'			&& the cuttkt status is 'ON HOLD'
           lnPieces = lnBudjet 
        ENDCASE          
        lnPieces = IIF(lcCostM = 'A',lnPieces,lnBudjet)
        *ash1
        *xAvg_Pcs = IIF(lnPieces>0,ROUND(xExt_Cst/lnPieces,2),0)
        IF lcCostM <> 'L'
          xAvg_Pcs = IIF(lnPieces>0,ROUND(xExt_Cst/lnPieces,3),0)        
          xAvg_Yld = IIF(lnPieces<>0,lnQty/lnPieces,0)
        ENDIF
        SELECT CTKTBOM
        IF XDYELOT_S
          @ ROW,04 SAY ITEM
          @ ROW,24 SAY ICLR
          @ ROW,31 SAY DESC  
          @ ROW,52 SAY Dyelot
        ELSE
          @ ROW,16 SAY ITEM
          @ ROW,36 SAY ICLR
          @ ROW,43 SAY DESC
        ENDIF
        *ASH1
        *@ ROW,58  SAY lnUntCst PICTURE '9999999.999'
        @ ROW,58  SAY lnUntCst PICTURE '9999999.9999'
        @ ROW,74  SAY lnQty    PICTURE '9999999.999'
        *ASH1
        *@ ROW,85  SAY XEXT_CST PICTURE '9999999999999.99'
        @ ROW,85  SAY XEXT_CST PICTURE '99999999999.999'
        @ ROW,102 SAY XAVG_YLD PICTURE '99999.999'
        *ASH1
        *@ ROW,110 SAY XAVG_PCS PICTURE '9999999999999.99'
        @ ROW,111 SAY XAVG_PCS PICTURE '99999999999.9999'
        XTOT1 = XTOT1 + lnQty
        XTOT2 = XTOT2 + XEXT_CST
        ROW=ROW+1
        SKIP
        IF ROW >= 55
          EXIT
        ENDIF
      ENDDO
      IF EOF() .OR. CTKTBOM->CUTTKT <> XCUTTKT
        EXIT
      ENDIF

    ENDDO

    ***  SUBTOTAL FOR LAST TYPE ***
    XTOT3= IIF(XACT_QTY<>0,XTOT1/XACT_QTY,0)
    XTOT4= IIF(XACT_QTY<>0,XTOT2/XACT_QTY,0)
     
    IF TYPE('lcNoToPrn') <> 'U'
      @ ROW,20 SAY ALLTRIM(EVAL('lcLong'+lcNoToPrn)) + ' TOTAL ==='+REPL('=',30-LEN(ALLTRIM(EVAL('lcLong'+lcNoToPrn)))) +'>'
    ENDIF
    @ ROW,74 SAY XTOT1 PICTURE '9999999.9999'
    @ ROW,86 SAY XTOT2 PICTURE '99999999999.9999'
    IF lcOldCtg $ 'FTS'
      @ ROW,99 SAY XTOT3 PICTURE '99999.9999'
    ENDIF
    @ ROW,111 SAY XTOT4 PICTURE '99999999999.9999'
    XGTOT1 = XGTOT1 + XTOT2
    XGTOT2 = XGTOT2 + XTOT4
    ROW=ROW+1

    ***  GRAND TOTAL FOR THIS CUTTING TICKET ***
    @ ROW,61 SAY 'TOTAL COSTS  ===>'
    @ ROW,86  SAY XGTOT1 PICTURE '99999999999.9999'
    @ ROW,111 SAY XGTOT2 PICTURE '99999999999.9999'
    ROW=ROW+1
    XGROSS1 = (XSELLPRICE*XACT_QTY) - XGTOT1
    XGROSS2 = (XSELLPRICE - XGTOT2)
    IF M_STYMARK='T'
      XGROSSP1 = IIF(XGTOT1<>0,XGROSS1/XGTOT1,0)*100
      XGROSSP2 = IIF(XGTOT2<>0,XGROSS2/XGTOT2,0)*100
    ELSE
      XGROSSP1 = IIF((XSELLPRICE*XACT_QTY)<>0,XGROSS1/(XSELLPRICE*XACT_QTY),0)*100
      XGROSSP2 = IIF(XSELLPRICE<>0,XGROSS2/XSELLPRICE,0)*100
    ENDIF
    @ ROW,31  SAY 'SELLING PRICE :'
    @ ROW,47  SAY XSELLPRICE PICTURE '99999999.999'
    @ ROW,60  SAY 'GROSS PROFIT  ===>'
    @ ROW,85  SAY XGROSS1 PICTURE '999999999999.9999'
    @ ROW,111 SAY XGROSS2 PICTURE '99999999999.9999'
    ROW=ROW+1
    @ ROW,60  SAY 'GROSS PROFIT %===>'
    @ ROW,89  SAY XGROSSP1 PICTURE '9999999.9999'
    @ ROW,100  SAY '%'
    @ ROW,114 SAY XGROSSP2 PICTURE '9999999.9999'
    @ ROW,125 SAY '%'
    ROW=ROW+1

    STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XGTOT1,XGTOT2

    SELECT &CTKTEMP
    SKIP
    ROW=99      && PRINT ON NEXT PAGE UPON REQUEST FROM BRIDGET DONE
  ENDDO
  EXIT
ENDDO

DO ENDREPORT
SET DEVICE TO SCREEN
IF USED(CTKTEMP)
  USE IN (CTKTEMP)
  ERASE (GcWorkDir+CTKTEMP+'.DBF')
  ERASE (GcWorkDir+CTKTEMP+'.CDX')
ENDIF
IF USED(lcTmpOPH)
  USE IN (lcTmpOPH)
ENDIF


*!*************************************************************
*! Name      : lfGetTit
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : Get title to be displayed 
*!*************************************************************
*! Called from : The option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : lcTit
*!*************************************************************
*! Example     : = lfGetTit()
*!*************************************************************
FUNCTION lfGetTit

PRIVATE lcTit
lcTit   = ALLTRIM(gfGetMemvar('M_PRDLNLBL',gcAct_Comp))  
lcTit  = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit+'#')
RETURN lcTit


*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : When function of the option grid
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen



*!*************************************************************
*! Name      : lfvVend
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 04/05/99
*! Purpose   : Vaildate vendor
*! Reference : *B602739,1 AMM
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVend()
*!*************************************************************

FUNCTION lfvVend

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcRPVend) .AND. ;
   ('?' $ lcRPVend .OR. !SEEK(lcRPVend , 'APVENDOR'))
  lcOldVen = lcRPVend
  =gfApVnBrow(@lcRPVend)
  IF EMPTY(lcRPVend)
    lcRPVend = lcOldVen
  ENDIF
ENDIF


FUNCTION lfCompAct
lnRecQty    = 0
lnOpen      = 0
lnRecAmt    = 0
lnActAmt    = 0
lnLstRecYld = 0
lnLstRecCst = 0
IF SEEK(CTKTBOM.CutTkt+BOMLINE.Style,'CutTktL')
  SELECT CutTktL
  SUM REST IIF(TranCd='1',TotQty,-TotQty) TO lnClrOpen ;
      WHILE CutTkt+Style=CTKTBOM.CutTkt+BOMLINE.Style
  lnOpen = lnOpen + MAX(lnClrOpen,0)
ENDIF
SELECT BOMLINE
lnRec = RECNO()
IF SEEK(BOMLINE.cIMTyp+BOMLINE.cTktNo+BOMLINE.cBomTyp+BOMLINE.Item+BOMLINE.IClr+BOMLINE.MfgCode,'cTktBom')
  lnUsed = cTktBom.Used_Qty
ENDIF

SET ORDER TO TAG Bomline 
lcKey = cBomTyp+MfgCode+Item+IClr
lcExp = cIMTyp+'2'+cTktNo+STR(LineNo,6)+cBomTyp
IF SEEK(lcExp,'BomLine')
  SELECT BomLine
  SCAN REST WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp =lcExp ;
        FOR cBomTyp+MfgCode+Item+IClr = lcKey .AND. !EMPTY(cRSession)
    lnRecQty = lnRecQty + ItemQty
    lnRecAmt = lnRecAmt + ItemAmt
    lnLstRecYld = UnitQty
    lnLstRecCst = UnitCost
  ENDSCAN
ENDIF
GOTO lnRec
SET ORDER TO TAG Mfgopr 
lnAcQty=0
IF lnOpen = 0
  xAvg_Yld  = lnLstRecYld
  lnAcQty   = lnLstRecYld
  lnUntCst  = lnLstRecCst
ELSE
  xAvg_Yld = MAX(ROUND((lnUsed-lnRecQty)/lnOpen,3),0)
  lnAcQty  = MAX(ROUND((lnUsed-lnRecQty)/lnOpen,3),0)
  *--Get actual unit cost for this item component.
  SELECT BOMCOST
  lcOldOrd = ORDER()
  SET ORDER TO TAG BomCsTkt
  lcSekBCost = BomLine.cbomtyp+BomLine.cimtyp+BomLine.ctktno+BomLine.item+BomLine.iclr+BomLine.mfgcode
  lnActAmt   = 0
  IF SEEK(lcSekBCost,'BomCost')
    SUM REST nTotACst TO lnActAmt WHILE ;
    cbomtype+cimtyp+ctktno+item+iclr+mfgcode =lcSekBCost  
  ENDIF
  *IF lnqty <>0
  IF lnAcQty <>0
    *lnUntCst = MAX(ROUND(((lnActAmt-lnRecAmt)/lnOpen)/lnqty,7),0)
    lnUntCst = MAX(ROUND(((lnActAmt-lnRecAmt)/lnOpen)/lnAcQty ,7),0)
  ELSE
    lnUntCst = MAX(ROUND(((lnActAmt-lnRecAmt)/lnOpen),7),0)  
  ENDIF
ENDIF
SET ORDER TO lcOldOrd