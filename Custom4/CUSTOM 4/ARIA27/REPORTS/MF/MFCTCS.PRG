*:************************************************************************
*: Program file  : MFCTCS.Prg
*: Program desc. : CUTTING TICKET COST SHEET REPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MF
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 08/19/1998
*: REF           : *E300975,1
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetTit(), lfwOGWhen()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO MFCTDT
*:************************************************************************
*:Modifications:
*B602739,1 AMM 04/05/1999 1- Add a filter on Contractor
*B602739,1 AMM            2- Add a setting to print on Actual or estimated cost.
*B602739,1 AMM            3- Adjust the subtotal of Yardage and Est cost.
*B802186,1 AMM 04/22/1999 1- Print the style scale
*B802186,1 AMM            2- Print the Rate field.
*B802285,1 AMM 06/06/1999 Get the right element cost.
*B802487,1 WAB 08/01/1999 get The right avreage cost when printing with actual
*B802488,1 WAB 08/01/1999 Devide The Total Yardage with the # of pieces recived
*B802662,1 RAMY 10/04/1999 In case of using Dyelot and select to print cost sheet
*B802662,1 RAMY            with "Actual Cost", prints "Yardage" column for 
*B802662,1 RAMY            fabric/color/dyelot instead of printing on fabric/color level
*B802721,1 WAB  11/02/1999 The yardage required shoud divided by the qty only of the style
*B802721,1 WAB             not the total qty
*B802850,1 AMM 01/18/1999 Fix the bug of miscalculating the ave. yardege at fabric level
*B603348,1  ABD 02/07/2000 Increase The Picture Of The purchase price per item [UnTCost]
*B603348,1  To 9999999.999 & the Picture Of The TotCost [ ToTCost ] To 999999999999.99 
*B603348,1  Instead of the fields.
*B603713,10 HBG 11/22/2000 Increase The Picture Of The pricea , priceb , pricec
*B605317,1 KHM 01/28/2002 Fix the bug of not calculating the average yield
*B605317,1                properly.
*B605861,1 AMH 04/23/2002 Enhance the speed of the report.
*B606804,1 KHM 12/26/2002 Fix the problem of **** in the unit cost because of dividing by 0
*B607177,1 AMH 04/24/2003 Fix the bug of wrong Avg. Yield.
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
XREPORT  = 'MFCTCS'
R_WIDTH  = 'W'

SELE CUTTKTH
SET RELATION TO STYLE INTO STYLE
CTKTEMP  = GFTEMPNAME()
lcTmpOPH = GFTEMPNAME()

IF OPENED('&CTKTEMP')
  SELE &CTKTEMP
  USE
  ERASE &GCWORKDIR.&CTKTEMP
ENDIF

IF EMPTY(lcRpExp)
  *B605861,1 AMH Add alias [Start]
  *lcRpExp='PCS_BUD <> 0'
  lcRpExp='CUTTKTH.PCS_BUD <> 0'
  *B605861,1 AMH [End]
ELSE
  *B605861,1 AMH Add alias [Start]
 *lcRpExp = '('+lcRpExp +')' + ' .AND. PCS_BUD <> 0'
  lcRpExp = '('+lcRpExp +')' + ' .AND. CUTTKTH.PCS_BUD <> 0'
  *B605861,1 AMH [End]
ENDIF

*Add the filter of status
IF lcStatus <> "L"
   lcRpExp = '(' +lcRpExp+ ") .AND. (CUTTKTH.Status = '" + lcStatus +  "')"
ENDIF


SELE CUTTKTH

*B605861,1 AMH Make more optmization [Start]
*GOTO TOP
LOCATE

*LOCATE ALL FOR &lcRpExp

*IF EOF()
*  *Message 'There are no records to display...!'
*  =gfModalGen('TRM00052B00000','DIALOG')
*  RETURN
*ENDIF
*-- To get the selected C/T if any.
lcCutFile = ''
llUseCut = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CUTTKT'),1)
IF lnPosition > 0
  lcCutFile = laOGFxFlt[lnPosition,6]
  llUseCut = IIF(!EMPTY(lcCutFile) .AND. USED(lcCutFile) .AND. RECCOUNT(lcCutFile)>0,.T.,.F.)
ENDIF
*B605861,1 AMH [End]

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

*B802487,1 WAB - fix error ( invalid path ) 
*B802487,1 WAB - START
*INDEX ON CTKTNO+cOperSeq+cOprCode TAG &lcTmpOPH 

INDEX ON CTKTNO+cOperSeq+cOprCode TAG &lcTmpOPH OF &lcTmpOPH

*B802487,1 WAB - END

SELECT CUTTKTH

*B605861,1 AMH Make more optmization by scanning on the selected C/Ts [Start]
*SCAN FOR &lcRpExp
IF llUseCut
  SELECT (lcCutFile)
  LOCATE
  SET RELATION TO CUTTKT INTO CUTTKTH ADDITIVE
ENDIF
SCAN FOR &lcRpExp
  SELECT CUTTKTH
*B605861,1 AMH [End]

  *B602739,1 AMM comment, move this peice of code below.
  *SCATTER MEMVAR MEMO
  *INSERT INTO (CTKTEMP) FROM MEMVAR
  llVend  = .F.
  *B602739,1 AMM end
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
        *B602739,1 AMM Don't replace in the file, set memory variables instead.
        *REPLACE &CTKTEMP..CONTR&lcCount    WITH  cContCode,;
                &CTKTEMP..MFG_OPR&lcCount  WITH  COPRCODE
                *-- If contractor of any operation match the required contractor set the
        *-- flag to add this cuttkt to the report data.
        llVend  = IIF(!llVend .AND. !EMPTY(lcRpVend),lcRpVend = cContCode,llVend)
        IF lnCount <= 3 
          m.CONTR&lcCount   = cContCode
          m.MFG_OPR&lcCount = COPRCODE
        ENDIF
        *B602739,1 AMM end
          lnCount = lnCount+1
        SKIP
        IF EOF()
          EXIT
        ENDIF         
      ENDDO
    *B602739,1 AMM Initialize the cont. memory variables to add to the file
    ELSE
      STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, ;
                        m.MFG_OPR2, m.MFG_OPR3
    *B602739,1 AMM end
    ENDIF
  *B602739,1 AMM Initialize the cont. memory variables to add to the file
  ELSE
    STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, ;
                      m.MFG_OPR2, m.MFG_OPR3
  *B602739,1 AMM end
  ENDIF
  *B602739,1 AMM If there is vendor filter applicable, add to the temp. file.
  IF EMPTY(lcRpVend) .OR. llVend
    SELECT CUTTKTH
    SCATTER MEMVAR MEMO
    INSERT INTO (CTKTEMP) FROM MEMVAR
  ENDIF
  *B602739,1 AMM end
ENDSCAN
*B602739,1 AMM Check if there is data.

*B605861,1 AMH Remove relation [Start]
IF llUseCut
  SELECT (lcCutFile)
  SET RELATION TO
ENDIF
*B605861,1 AMH [End]

SELECT (CTKTEMP)
IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
*B602739,1 AMM end

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
      *B602739,1 AMM Adjust to print style description
      *@ 05,00 SAY 'TICKET STYLE....... .........SEASON................... ..........DIVISION......... .................SIZE SCALE......................'
      *B802186,1 AMM Adjust to fit adding the style scale
      *@ 05,00 SAY 'TICKET STYLE.............. .......DESC.... .....SEASON........ ......DIVISION..... .................SIZE SCALE......................'
      @ 05,00 SAY 'TICKET STYLE.............. ......DESC..... .....SEASON...... .....DIVISION.... ....................SIZE SCALE.........................'
      *B802186,1 AMM end
      *B602739,1 AMM end
      *B802186,1 AMM Add the Rate title
      *@ 06,00 SAY 'STATUS   ENTERED    CONTR1   .............NAME............. ..MFG. ..........OPERATION..........  PATTERN    TYPE'         
      *@ 07,00 SAY 'ORDER    COMPLETE   CONTR2   .............NAME............. ..MFG. ..........OPERATION..........  VENDOR     ACT.QTY'         
      *@ 08,00 SAY 'ACCOUNT             CONTR3   .............NAME............. ..MFG. ..........OPERATION.......... '
      @ 06,00 SAY 'STATUS   ENTERED    CONTR1   .............NAME............. ..MFG. ..........OPERATION..........     RATE PATTERN    TYPE'
      @ 07,00 SAY 'ORDER    COMPLETE   CONTR2   .............NAME............. ..MFG. ..........OPERATION..........     RATE VENDOR     ACT.QTY'
      @ 08,00 SAY 'ACCOUNT             CONTR3   .............NAME............. ..MFG. ..........OPERATION..........     RATE '
      *B802186,1 AMM end
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
      *B602739,1 AMM reduce season size to fit printing style description.
      *PSEASON   = SUBSTR(gfCodDes(XSEASON, 'SEASON'),1,27)
      *B802186,1 AMM Reduce the size to fit printing style scale
      *PSEASON   = SUBSTR(gfCodDes(XSEASON, 'SEASON'),1,19)
      PSEASON   = SUBSTR(gfCodDes(XSEASON, 'SEASON'),1,17)
      *B802186,1 AMM end
      *B602739,1 AMM end
    ENDIF
    IF !EMPTY(XDIVISION)
      *B602739,1 AMM Reduce division size to fit printing style description
      *PDIVISION = SUBSTR(gfCodDes(XDIVISION, 'CDIVISION'),1,27)
      *B802186,1 AMM Reduce size to fit printing style scale
      *PDIVISION = SUBSTR(gfCodDes(XDIVISION, 'CDIVISION'),1,19)
      PDIVISION = SUBSTR(gfCodDes(XDIVISION, 'CDIVISION'),1,17)
      *B802186,1 AMM end
      *B602739,1 AMM end
    ENDIF
    SELECT &CTKTEMP
    @ ROW,000 SAY REPLICATE('-',132)
    ROW = ROW+1
    IF SEEK(ALLTRIM(&CTKTEMP..STYLE) , 'STYLE')
      XSCALE    = GETSCALE(STYLE.SCALE,SPACE(1))
      XPRIM_FAB = STYLE.FABRIC      && PRIMARY FABRIC TO GET IT'S VENDOR FOR ROBYN
    ENDIF

    *B602739,1 AMM comment out, not used in 2.7
    *IF &CTKTEMP->SEL_PRICE=0
    *B602739,1 AMM end
    DO CASE
      CASE XPRICE_LVL ='B'
        XSELLPRICE = STYLE->PRICEB
      CASE XPRICE_LVL ='C'
        XSELLPRICE = STYLE->PRICEC
      OTHERWISE
        XSELLPRICE = STYLE->PRICEA
    ENDCASE
    *B602739,1 AMM comment out , field SEL_PRICE not used in 2.7
    *ELSE
      *XSELLPRICE=&CTKTEMP->SEL_PRICE
    *ENDIF
    *B602739,1 AMM end
    IF 'MA' $ gccmpmodul
      SELE FABRIC
      SEEK XPRIM_FAB
      XPRM_VEND = VENDOR
    ENDIF
    SELE &CTKTEMP
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
    *B802850,1 AMM Start, display a friendly window to user
    WAIT WINDOW 'Printing CT # '+CUTTKT  NOWAIT
    *B802850,1 AMM end
    @ ROW,000 SAY CUTTKT
    @ ROW,007 SAY STYLE
    *B602739,1 AMM Print style description 
    *@ ROW,027 SAY PSEASON
    *@ ROW,055 SAY PDIVISION
    @ ROW,027 SAY LEFT(STYLE.DESC1,15)
    *B802186,1 AMM Print the style scale and adjust.
    *@ ROW,042 SAY PSEASON
    *@ ROW,063 SAY PDIVISION
    @ ROW,042 SAY PSEASON
    @ ROW,061 SAY PDIVISION
    @ ROW,079 SAY STYLE.SCALE
    *B802186,1 AMM end
    *B602739,1 AMM end
    
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
      @ Row,098 SAY CTktBom.UntCost
    ENDIF
    @ ROW,106 SAY PATTERN
    @ ROW,117 SAY cTktType
    *B802186,1 AMM end
    ROW = ROW+1

    *** LINE # 3
    *B602739,1 AMM start
    *@ ROW,000 SAY ORDER
    IF SEEK('1'+CUTTKT,'CUTPICK')
      @ ROW,000 SAY CUTPICK.ORDER
    ENDIF
    *B602739,1 AMM end
    @ ROW,009 SAY COMPLETE
    @ ROW,020 SAY CONTR2
    @ ROW,029 SAY XVEN2
    @ ROW,060 SAY MFG_OPR2
    @ ROW,067 SAY XOPER2
    *B802186,1 AMM Print the corresponding to the field RATE in 2.7 and adjust.
    *@ ROW,098 SAY XPRM_VEND
    *@ ROW,109 SAY lcPieces PICTURE '9999999'
    SET ORDER TO CTKTYP IN CTKTBOM
    IF SEEK('M'+CUTTKT+SPACE(25)+MFG_OPR2 ,'Ctktbom')
      @ Row,098 SAY CTktBom.UntCost
    ENDIF
    @ ROW,106 SAY XPRM_VEND
    @ ROW,117 SAY lcPieces PICTURE '9999999'
    *B802186,1 AMM end
    ROW = ROW+1

    *** LINE # 4
    *B602739,1 AMM
    *IF !EMPTY(ACCOUNT)
      *@ ROW,000 SAY ACCOUNT
    IF !EOF('CUTPICK') .AND. 'SO' $ gccmpmodul .AND. ;
                             SEEK('O'+CUTPICK.ORDER,'ORDHDR')
      @ ROW,000 SAY ORDHDR.ACCOUNT
    *B602739,1 AMM end
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
    *B802186,1 AMM Print the corresponding to the field RATE in 2.7 .
    SET ORDER TO CTKTYP IN CTKTBOM
    IF SEEK('M'+CUTTKT+SPACE(25)+MFG_OPR3 ,'Ctktbom')
      @ Row,098 SAY CTktBom.UntCost
    ENDIF
    *B802186,1 AMM end

    ROW = ROW+1

    *** CHECK IF THIS CUTTKT# HAS A BOM ON FILE ****
    XCUTTKT=CUTTKT
    xAct_Qty = 0
    DO CASE
      CASE Status='O'
        *B605317,1 KHM 01/28/2002 (Begin) Deducting the received and damage also.
        *xAct_Qty = Pcs_Bud - Pcs_Can
        xAct_Qty = Pcs_Bud - (Pcs_Can + Pcs_Rec + Pcs_Dam)
        *B605317,1 KHM 01/28/2002 (EnD)
      CASE Status='C'
        xAct_Qty = Pcs_Rec
      CASE Status='S'
        xAct_Qty = Pcs_Rec
      CASE Status='A'
        xAct_Qty = Pcs_Act
      CASE Status='H'
        xAct_Qty = Pcs_Opn
    ENDCASE 
    *B802186,1 AMM set the proper index.
    SET ORDER TO CTKTBOM IN CTKTBOM
    *B802186,1 AMM end
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
      *B603348,1 ABD Remark The next Line & Move Unt Cost ,EXTD. COST to The Right . [ Begin ]
      *@ ROW,004 SAY 'ITEM                COLOR  .......DESC......... DYELOT     UNT COST     YARDGE  EXTD. COST   AVG. YIELD  AVG. COST/PIECE'      
       @ ROW,004 SAY 'ITEM                COLOR  .......DESC......... DYELOT       UNT COST     YARDGE  EXTD. COST AVG.    YIELD  AVG.COST/PIECE'
      *B603348,1 ABD [ End ]
    ELSE
      @ ROW,015 SAY REPLICATE('=',112)
      ROW=ROW+1
      *B603348,1 ABD Remark The next Line & Move EXTD. COST,AVG.YIELD,AVG.COST/PIECE to The Right. [ Begin ]
      *@ ROW,016 SAY 'ITEM                COLOR ......DESC.......... UNT COST       YARDGE  EXTD. COST  AVG. YIELD  AVG. COST/PIECE'
      @ ROW,016 SAY 'ITEM                COLOR ......DESC.......... UNT COST        YARDGE      EXTD. COST AVG.YIELD AVG.COST/PIECE'      
      *B603348,1 ABD [ End ]
    ENDIF

    ROW=ROW+1

    SELE CTKTBOM
    XPRV_TYP   = SPACE(1)
    OldTyp     = SPACE(1)
    lcOldCtg   = cCatgtyp

    DO WHILE .T.
      IF ROW >= 55
        PAGENO = PAGENO + 1
        DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
        IF XDYELOT_S
          *B603348,1 ABD Remark The next Line & Move Unt Cost ,EXTD. COST to The Right & . [ Begin ]
          *@ 05,004 SAY 'ITEM                COLOR  .......DESC......... DYELOT     UNT COST       YARDGE  EXTD. COST   AVG. YIELD  AVG. COST/PIECE'          
          @ 005,004 SAY 'ITEM                COLOR  .......DESC......... DYELOT       UNT COST     YARDGE  EXTD. COST AVG.   YIELD  AVG. COST/PIECE'
          *B603348,1 ABD [ End ]

        ELSE
          *B603348,1 ABD Remark The next Line & Move EXTD. COST,AVG.YIELD,AVG.COST/PIECE to The Right. [ Begin ]
          *@ 05,016 SAY 'ITEM                COLOR ......DESC.......... UNT COST       YARDGE  EXTD. COST  AVG. YIELD  AVG. COST/PIECE'
          @ 05,016 SAY 'ITEM                COLOR ......DESC.......... UNT COST       YARDGE       EXTD. COST AVG.YIELD AVG.COST/PIECE'
          *B603348,1 ABD [ End ]
        ENDIF
        ROW=06
        XPRV_TYP=SPACE(1)
        OldTyp  =SPACE(1)
      ENDIF
      DO WHILE (!EOF()) .AND. (CUTTKT=XCUTTKT)
        IF IIF(lcOldCtg='M',OldTyp <> TYP,lcOldCtg <> cCatgTyp)
          XTOT3= IIF(XACT_QTY<>0,XTOT1/XACT_QTY,0)
          XTOT4= IIF(XACT_QTY<>0,XTOT2/XACT_QTY,0)
          

          IF TYPE('lcNoToPrn') <> 'U'
            @ ROW,20 SAY ALLTRIM(EVAL('lcLong'+lcNoToPrn)) + ' TOTAL ==='+REPL('=',30-LEN(ALLTRIM(EVAL('lcLong'+lcNoToPrn)))) +'>'
            *B602739,1 AMM Print the subtotal

            *B603348,1 ABD Remark The next Lines & Change The Picture of The Variable & Place Of Print.[ Begin ]
            *@ ROW,72 SAY XTOT1 PICTURE '99999999.999'
            *@ ROW,85 SAY XTOT2 PICTURE '99999999.99'            
            @ ROW,73 SAY XTOT1 PICTURE '99999999.999'
            @ ROW,88 SAY XTOT2 PICTURE '9999999999.99'
            *B603348,1 ABD [ End ]
            
            *B602739,1 AMM end          
            IF lcOldCtg $ 'FTS'
              @ ROW,99 SAY XTOT3 PICTURE '99999.999'
            ENDIF
            @ ROW,115 SAY XTOT4 PICTURE '99999999.99'
          ENDIF
            
          lcOldCtg   = cCatgTyp       &&TYP            && BREAK FOR SUBTOTALS
          XGTOT1 = XGTOT1 + XTOT2
          XGTOT2 = XGTOT2 + XTOT4
          STORE 0 TO XTOT1,XTOT2,XTOT4,XTOT3
          ROW=ROW+1
        ENDIF
        IF IIF(lcOldCtg='M',OldTyp <> TYP,XPRV_TYP <> cCatgTyp)
          lcHdrToSay = '< ' +ALLTRIM(EVAL('lcLong'+Typ))+' >'
          @ ROW,IIF(XDYELOT_S,04,16) SAY PADC(lcHdrToSay,IIF(XDYELOT_S,122,110),'-')
          ROW=ROW+1
          XPRV_TYP  = cCatgTyp         &&TYP
          lcOldCtg  = cCatgTyp
          lcNoToPrn = TYP
          OldTyp    = TYP
        ENDIF
        *B602739,1 AMM if printing the actual cost, get it from the BOMCOST file
        *XEXT_CST = ROUND(UNTCOST*USED_QTY,2)
        *XAVG_PCS = IIF(PIECES>0,ROUND(XEXT_CST/PIECES,2),0)
        *XAVG_YLD = IIF(PIECES<>0,USED_QTY/PIECES,0)
        IF lcCostM  = 'A'
          *B802285,1 AMM Each element may have many records in BOMCOST 
          *IF SEEK('M'+xCutTkt+"Y"+TYP+ITEM+ICLR+MFGCODE,'BomCost')
            *lnUntCst = BomCost.nUnitACst
            *lnQty    = USED_QTY
          *ELSE
            *lnUntCst  = UntCost
            *lnQty     = 0
          *ENDIF
          STORE 0 TO lnQty,lnUntCst,lnFound
          IF SEEK('M'+xCutTkt,'BomCost')
            SELECT BomCost
            *B802662,1 RAMY Change this line to add the Dyelot to the FOR condition [start]
            *SCAN WHILE cImTyp+cTktNo='M'+xCutTkt ;
               FOR CBOMTYPE+ITEM+ICLR+MFGCODE = CTKTBOM.TYP+CTKTBOM.ITEM+CTKTBOM.ICLR+CTKTBOM.MFGCODE

             *-- khm
             *SCAN WHILE cImTyp+cTktNo='M'+xCutTkt ;
             FOR CBOMTYPE+ITEM+ICLR+cDyelot+MFGCODE = CTKTBOM.TYP+CTKTBOM.ITEM+CTKTBOM.ICLR+CTKTBOM.Dyelot+CTKTBOM.MFGCODE

             SCAN REST WHILE cImTyp+cTktNo+Actualize+cBomType+Item+IClr+MfgCode+cOprCode+cLotNo+cISession+cRSession =;
                             'M'+xCutTkt ;
                  FOR CBOMTYPE+ITEM+ICLR+cDyelot+MFGCODE = CTKTBOM.TYP+CTKTBOM.ITEM+CTKTBOM.ICLR+CTKTBOM.Dyelot+CTKTBOM.MFGCODE
             *-- khm
             
             *B802662,1 RAMY [end]
              *B802487,1 WAB calculate the total of units cost by (unit*TOTqty) 
              *B802487,1 WAB - START
              *lnUntCst = lnUntCst + BomCost.nUnitACst

              lnUntCst = lnUntCst + BomCost.nUnitACst * BomCost.nTotQty

              *B802487,1 WAB - END

              lnQty    = lnQty    + BomCost.nTotQty
              lnFound  = lnFound + 1
            ENDSCAN
          ENDIF            
          IF lnFound = 0
            lnUntCst  = CTKTBOM.UntCost
          ELSE

            *B802487,1 WAB - caluclate avrerage by ( total units cost/nqty )
            *B802487,1 WAB - START
            *lnUntCst = lnUntCst / lnFound

            *B606804,1 KHM 12/26/2002 (Begin) If lnQty is 0 then do not divide
            *lnUntCst = lnUntCst / lnQty
            lnUntCst = IIF(lnQty <> 0 , lnUntCst / lnQty,0)
            *B606804,1 KHM 12/26/2002 (End)

            *B802487,1 WAB - END

          ENDIF
          SELECT CTKTBOM
          *B802285,1 AMM end
        ELSE
          lnUntCst  = UntCost
          lnQty     = Req_Qty
        ENDIF

        xExt_Cst = ROUND(lnUntCst*lnQty,2)
        
        *B802488,1 WAB - devide the yardage by lcPieces
        *B802488,1 WAB - START
        *xAvg_Pcs = IIF(Pieces>0,ROUND(xExt_Cst/Pieces,2),0)
        *xAvg_Yld = IIF(PIECES<>0,lnQty/Pieces,0)

        *B802721,1 WAB - get the divided qty from the cuttkt line by select the 
        *B802721,1 WAB - bomline and seek the cutkt no and get the style that 
        *B802721,1 WAB - used the cutktBOM.fabric and select the cutkt line to 
        *B802721,1 WAB - get the divided qty
        *B802721,1 WAB - START
        *xAvg_Pcs = IIF(lcPieces>0,ROUND(xExt_Cst/lcPieces,2),0)
        *xAvg_Yld = IIF(lcPieces<>0,lnQty/lcPieces,0)
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
            *lcExpr = [REST FOR CtktNo = CTKTBOM.CutTkt AND mfgCode = CTKTBOM.mfgCode]
            *-- khm
            *lcExpr = [REST FOR CtktNo = CTKTBOM.CutTkt AND mfgCode = CTKTBOM.mfgCode .AND. ]+;
                     [cType = '1' .AND. cBomTyp = cTktBom.Typ]
            
            lcExpr = [REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode= 'M'+'1'+CTKTBOM.CutTkt]+;
                     [ FOR mfgCode = CTKTBOM.mfgCode .AND. cType = '1' .AND. cBomTyp = cTktBom.Typ]
            *-- khm
            
            *B605317,1 KHM 01/28/2002 (End)
          ELSE
            *B802850,1 AMM Add condition of cost type (fabric or trim)
            *lcExpr = [REST FOR CtktNo = CTKTBOM.CutTkt AND ITEM = CTKTBOM.ITEM AND IClr = CTKTBOM.IClr AND cType = '1']           

            *B605317,1 KHM 01/28/2002 (Begin) Adding the checking of the cost type.
            *lcExpr = [REST FOR CtktNo = CTKTBOM.CutTkt AND ITEM = CTKTBOM.ITEM AND IClr = CTKTBOM.IClr AND cType = '1' .AND. cCatgTyp = CTKTBOM.cCatgTyp]
            
            *-- khm
            *lcExpr = [REST FOR CtktNo = CTKTBOM.CutTkt .AND. ]+;
                     [ITEM = CTKTBOM.ITEM AND IClr = CTKTBOM.IClr .AND. ]+;
                     [cType = '1' .AND. cCatgTyp = CTKTBOM.cCatgTyp .AND. ]+;
                     [cBomTyp = cTktBom.Typ]

            lcExpr = [REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode = "M"+"1"+CTKTBOM.CutTkt]+;
                     [ FOR ITEM = CTKTBOM.ITEM AND IClr = CTKTBOM.IClr .AND. ]+;
                     [cType = '1' .AND. cCatgTyp = CTKTBOM.cCatgTyp .AND. ]+;
                     [cBomTyp = cTktBom.Typ]
            *-- khm
            *B605317,1 KHM 01/28/2002 (End)

            *B802850,1 AMM end
          ENDIF
          SCAN &lcExpr
            SELECT CUTTKTL
            *B802850,1 AMM Change to improve speed
            *SCAN FOR CUTTKT+STYLE = CTKTBOM.CutTkt+BOMLINE.STYLE
            =SEEK(CTKTBOM.CutTkt+BOMLINE.STYLE)
            
            *-- khm
            *SCAN WHILE CUTTKT+STYLE = CTKTBOM.CutTkt+BOMLINE.STYLE
            SCAN REST WHILE CutTkt+Style+Dyelot+TranCd = CTKTBOM.CutTkt+BOMLINE.STYLE
            *-- khm
            
            *B802850,1 AMM end
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
          *B802850,1 AMM Get the status from the cut header file
          *CASE Status='O'			&& the cuttkt status is 'OPEN'
            *lnPieces = lnBudjet -  lnCancel 
          CASE &cTktemp..Status='O'			&& the cuttkt status is 'OPEN'
            lnPieces = lnBudjet - ( lnCancel + lnReciev + lnDamje)
          *B802850,1 AMM end

          *B802850,1 AMM  Get the status from the cut header file
          *CASE Status $ 'CSA'		&& the cuttkt status is 'Complete','CLOSE','ACTUALISE'
          CASE &cTktemp..Status $ 'CS'		&& the cuttkt status is 'Complete','CLOSE','ACTUALISE'
          *B802850,1 AMM end
            lnPieces = lnReciev
         *B802850,1 AMM Get the status from the cut header file
         *CASE Status='H'			&& the cuttkt status is 'ON HOLD'
         CASE &cTktemp..Status = 'A'			&& the cuttkt status is 'ON HOLD'
         *B802850,1 AMM end
           lnPieces = lnBudjet 
        ENDCASE          
        
        *B607177,1 AMH Fix the bug of wrong Avg. Yield. [Start]
        lnPieces = IIF(lcCostM = 'A',lnPieces,lnBudjet)
        *B607177,1 AMH [End]
        
        xAvg_Pcs = IIF(lnPieces>0,ROUND(xExt_Cst/lnPieces,2),0)
        xAvg_Yld = IIF(lnPieces<>0,lnQty/lnPieces,0)
        SELECT CTKTBOM
        *B802721,1 WAB - END
        *B802488,1 WAB - END

        *B602739,1 AMM end

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
        *B602739,1 AMM Print cost and qty due to the actual or est. cost setting
        *@ ROW,64  SAY UNTCOST PICTURE '999.999'
        *@ ROW,73  SAY USED_QTY PICTURE '9999999.999'
        *B603348,1 ABD Remark The next Line & Change The Picture Instead of the UnCost field. [ Begin ]
        *@ ROW,64  SAY lnUntCst PICTURE '999.999'
        @ ROW,58  SAY lnUntCst PICTURE '9999999.999'
        *B603348,1 ABD [ End ] 
        @ ROW,74  SAY lnQty    PICTURE '9999999.999'
        *B602739,1 AMM end
        *B603348,1 ABD Remark The next Line & Change The Picture Instead of the field UntCost . [ Begin ]
        *@ ROW,86  SAY XEXT_CST PICTURE '9999999.99'
        *@ ROW,IIF(XDYELOT_S,85,84)  SAY XEXT_CST PICTURE '9999999999999.99'
        @ ROW,85  SAY XEXT_CST PICTURE '9999999999999.99'
        *B603348,1 ABD [ End ]
        
        @ ROW,100 SAY XAVG_YLD PICTURE '99999.999'
        *B603348,1 ABD Remark The next Line & Change The Picture Instead of the field UntCost . [ Begin ]        
        *@ ROW,116 SAY XAVG_PCS PICTURE '9999999.99'
        @ ROW,110 SAY XAVG_PCS PICTURE '9999999999999.99'
        *B603348,1 ABD [ End ]
        
        *B602739,1 AMM Print cost and qty due to the actual or est. cost setting
        *XTOT1 = XTOT1 + USED_QTY
        XTOT1 = XTOT1 + lnQty
        *B602739,1 AMM end
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
    *B603348,1 ABD Remark The next Line & Change The Picture of The Variable & Place Of Print. [ Begin ]
    *@ ROW,72 SAY XTOT1 PICTURE '99999999.999'
    *@ ROW,85 SAY XTOT2 PICTURE '99999999.99'
    @ ROW,73 SAY XTOT1 PICTURE '99999999.999'
    @ ROW,86 SAY XTOT2 PICTURE '999999999999.99'
    *B603348,1 ABD [ End ]
    
    IF lcOldCtg $ 'FTS'
 
      @ ROW,99 SAY XTOT3 PICTURE '999999.999'
     ENDIF
    *B603348,1 ABD Remark The next Line & Change The Picture of The Variable & Place Of Print. [ Begin ]
    *@ ROW,115 SAY XTOT4 PICTURE '99999999.99'
    @ ROW,111 SAY XTOT4 PICTURE '999999999999.99'
    *B603348,1 ABD [ End ]
    XGTOT1 = XGTOT1 + XTOT2
    XGTOT2 = XGTOT2 + XTOT4
    ROW=ROW+1

    ***  GRAND TOTAL FOR THIS CUTTING TICKET ***
    @ ROW,61 SAY 'TOTAL COSTS  ===>'
    
    *B603348,1 ABD Remark The next Line & Change The Picture of The Variable & Place Of Print [ Begin ]
    *@ ROW,85  SAY XGTOT1 PICTURE '99999999.99'
    *@ ROW,115 SAY XGTOT2 PICTURE '99999999.99'
    @ ROW,86  SAY XGTOT1 PICTURE '999999999999.99'
    @ ROW,111 SAY XGTOT2 PICTURE '999999999999.99'
    *B603348,1 ABD [ End ]
    
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
    *B603713,10 HBG 11/22/2000 Increase The Picture Of The selling price [Begin]
    *@ ROW,47  SAY XSELLPRICE PICTURE '9999.99'
    @ ROW,47  SAY XSELLPRICE PICTURE '999999999.99'
    *B603713,10 [End]
    @ ROW,60  SAY 'GROSS PROFIT  ===>'

    *B603348,1 ABD Remark The next Line & Change The Picture of The Variable & Place Of Print. [ Begin ]
    *@ ROW,85  SAY XGROSS1 PICTURE '99999999.99'
    *@ ROW,115 SAY XGROSS2 PICTURE '99999999.99'
    @ ROW,85  SAY XGROSS1 PICTURE '9999999999999.99'
    @ ROW,111 SAY XGROSS2 PICTURE '999999999999.99'
    *B603348,1 ABD [ End ]
    ROW=ROW+1

    @ ROW,60  SAY 'GROSS PROFIT %===>'
    *B603348,1 ABD Remark The next Line & Change The Place Of Printing. [ Begin ]
    *@ ROW,85  SAY XGROSSP1 PICTURE '99999999.99'
    *@ ROW,96  SAY '%'
    *@ ROW,115 SAY XGROSSP2 PICTURE '99999999.99'
    *@ ROW,126 SAY '%'
    @ ROW,89  SAY XGROSSP1 PICTURE '99999999.99'
    @ ROW,100  SAY '%'
    @ ROW,114 SAY XGROSSP2 PICTURE '99999999.99'
    @ ROW,125 SAY '%'
    *B603348,1 ABD [ End ]    
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

