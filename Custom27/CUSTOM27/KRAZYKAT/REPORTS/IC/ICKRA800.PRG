****************************************************************************
*: Program file      : ICKRA800.PRG        (C#101641)
*: Program desc.     : Cutting Card Report (for Krazy Kat)
*:                   : Report to list the PO Lines & Sales Order Lines .
*: System            : Aria Apparel System (A27)
*: Module            : ****Manufacturing       (MF) 
*:                   : [Change to IC module as per NY Sherif ] ( WAB on 12/12/2000)
*: Developer         : ABDOU ELGENDI       (ABD)
*: Date              : 09/14/1999
*:**************************************************************************
*: Calls : FUNCTIONS : lfCrtSetup , lfPrnRep   , lfGNum     , lfPrt      
*:                   : lfPrtNot   , lfCalcDate , lfPrnPrePk , lfPrnHdr   
*:                   : lfOpnToSel , lfShipped  , lfvFabric  , lfWoldVal  
*:                   : lfvStyle   , lfMajGet   , lfEvalSegs , lfVDir
*:                   : lfVAccpOk  , lfvDateRng , lfvCompDat , lfvpbOk
*:                   : lfClearRep
*:         PROCEDURE : lpPrtHdr
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Modifications     : ........
*B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*:**************************************************************************
*
*--  Variable Declaration.
 XTITLE   = SPACE(30)
 XRPT_CD  = 'M'
 R_TITLE  = 'CUSTOMIZED CUTTING CARD REPORT'
 R_WIDTH  = 'W'
 XRPT_CD  = 'D'
 lnArrInd = 0
 llNewPag = .T.
 lcIScale = SPACE(01)
 STORE '' TO XFILTER , XSTATUS , XFABRIC , XMAKE , lcParam
 llDatChng = .F.
*-- END OF variable Declaration.

IF FILE(lcMcompdir+'ITPOSHIP.DBF') .AND. FILE(lcMcompdir+'ITHBILLT.DBF')
  *--- Open  used  Files.
  =gfOpenFile(lcMcompdir+"ITPOSHIP",lcMcompdir+"Style","SH")
  =gfOpenFile(lcMcompdir+"ITHBILLT",lcMcompdir+"HBILLNUM","SH")
ELSE
  *-- TEXT :No files to be uploaded in the specified path.
  =gfModalGen('TRM42197B00000','DIALOG') 
  SET DEVICE TO SCREEN
  RETURN
ENDIF
*-- Get the complete date.
IF ldFComDate <> LCompDATE .OR. ldTComDate <> HCompDATE
  ldFComDate = LCompDATE
  ldTComDate = HCompDATE
  llDatChng = .T.
ENDIF
XTITLE1    = 'Completion dates between '+DTOC(ldFComDate)+' AND '+DTOC(ldTComDate)
*-- get the delivery date.
IF ldFDlvDate <> LDATE .OR. ldTDlvDate <> HDATE
  ldFDlvDate = LDATE
  ldTDlvDate = HDATE
  llDatChng = .T.    
ENDIF
  XTITLE2    = 'Delivery dates between '+ DTOC(ldFDlvDate)+ ' AND '+DTOC(ldTDlvDate) 
*--- If user change filter criteria then you must collect data again [Begin]
IF llOGFltCh .OR. llDatChng
*-- TTILE 
  XTITLE = lcRpOpTlt 
  *-- FABRIC
  XFABRIC = lcRpFabr
  *-- STYLE MAKE/BUY
  DO CASE
    CASE lcRpMake='M'
      XMAKE  =  'M'
    CASE lcRpMake='U'
      XMAKE  =  'U'
  ENDCASE 
  *-- STATUS Active/Cancel/Hold
  DO CASE
    CASE lcRpTyp='A'
      XSTATUS=  'A'
    CASE lcRpTyp='H'
      XSTATUS=  'H'
    CASE lcRpTyp='X'
       XSTATUS  ='X'
  ENDCASE
  SELECT STYLE
  GO TOP
  LOCATE FOR &lcRpExp
  IF !FOUND()
    =gfModalGen('TRM00052B00000','DIALOG') 
    SET DEVICE TO SCREEN
    RETURN
  ENDIF
  WAIT WINDOW 'Selecting records for report ...' NOWAIT
  XFILTER = lcRpExp
  IF !EMPTY(XFABRIC)
    XFILTER = XFILTER +'.AND. FABRIC=XFABRIC'
  ENDIF
  IF !EMPTY(XSTATUS)
    XFILTER = XFILTER +'.AND. STATUS=XSTATUS'
  ENDIF
  IF XMAKE $ 'MU'
    XFILTER = XFILTER + ".AND.MAKE=IIF(XMAKE='M',.T.,.F.)"
  ENDIF
  *-- Copy selected records from style file to a temporary file.
  WAIT WINDOW  'Validating selection criteria ...' NOWAIT
  SELECT STYLE
  DIMENSION laStyle[1,4]
  lnFldLen = AFIELDS(laStyle)
  FOR lnX = 1 TO lnFldLen
    IF laStyle[lnX,1]   = 'STYLE'
      laStyle[ lnX , 2] = 'C'
      laStyle[ lnX , 3] = lnMajorLen            
      laStyle[ lnX , 4] = 0
      EXIT
    ENDIF
  ENDFOR
  DIMENSION laStyle[lnFldLen + 1 , 4]
  laStyle[lnFldLen + 1 , 1] = 'COLOR'
  laStyle[lnFldLen + 1 , 2] = 'C'
  laStyle[lnFldLen + 1 , 3] = 6
  laStyle[lnFldLen + 1 , 4] = 0
  CREATE DBF (lcStyTmp) FROM ARRAY laStyle
  INDEX ON STYLE+COLOR TAG &lcStyTmp
  SELECT(lcStyTmp)
  ZAP
  SELECT STYLE
  GO TOP
  SCAN FOR &XFILTER
     SCATTER MEMVAR MEMO
     m.Style = SUBSTR(STYLE,1,lnMajorLen)
     m.Color = SUBSTR(STYLE,lnMajorLen+2,lnColorLen)
     INSERT INTO (lcStyTmp) FROM MEMVAR
  ENDSCAN
  DO CASE
    CASE EMPTY(ldFDlvDate) AND EMPTY(ldTDlvDate) 
      lcFilter = "!EMPTY(ITPOSHIP.STYLE)"
    CASE EMPTY(ldFDlvDate) 
      lcFilter = "ITHBILLT.dDelvDATE <= ldTDlvDate "
    CASE EMPTY(ldTDlvDate)
      lcFilter = "ITHBILLT.dDelvDATE >= ldFDlvDate "
    OTHERWISE
      lcFilter = "ITHBILLT.dDelvDATE BETWEEN ldFDlvDate AND ldTDlvDate"
  ENDCASE

  DO CASE
    CASE EMPTY(ldFComDate) AND EMPTY(ldTComDate) 
      lcFilter1 = "!EMPTY(SUBSTR(ORDLINE.STYLE,1,lnMajorLen))"
    CASE EMPTY(ldFComDate) 
      lcFilter1 = "ORDLINE.COMPLETE <= ldTComDate "
    CASE EMPTY(ldTComDate)
      lcFilter1 = "ORDLINE.COMPLETE >= ldFComDate "
    OTHERWISE
      lcFilter1 = "ORDLINE.COMPLETE BETWEEN ldFComDate AND ldTComDate"
  ENDCASE 

  WAIT WINDOW  'Selecting Records For Report ...' NOWAIT

  *-- Selecting PO lines 
  SELECT ITPOSHIP.PO,ITPOSHIP.STYLE,ITPOSHIP.COLOR,SUM(ITPOSHIP.TOTQTY) AS TOTQTY,;
         ITHBILLT.CVENDOR,ITHBILLT.DDELVDATE,POSHDR.AVAILABLE,;
         POSHDR.CONTACT, POSHDR.CFOB,POSHDR.SHIPVIA,;
         POSHDR.ENTERED , .F. AS PRINTED,&lcStyTmp..Scale;
  FROM   ITPOSHIP,&lcStyTmp ,POSHDR,ITHBILLT;
  WHERE  ITPOSHIP.STYLE + ITPOSHIP.COLOR =;
         &lcStyTmp..STYLE + SPACE(7) + &lcStyTmp..COLOR ;
    AND  &lcFilter ;
    AND  ITHBILLT.CSHIPNUM = ITPOSHIP.CSHIPNUM; 
    AND  POSHDR.PO = ITPOSHIP.PO;
    AND  POSHDR.STATUS <> "X" ;
   INTO  TABLE (gcWorkDir+lcPosLTmp);
  GROUP  BY ITPOSHIP.STYLE,ITPOSHIP.COLOR,ITPOSHIP.PO
  INDEX  ON Style+Color TAG &lcPosLTmp1
  INDEX  ON STYLE+PO+COLOR TAG &lcPosLTmp
  GOTO TOP
  
  *-- Selecting Sales Order Lines.
  SELECT ORDLINE.ORDER,ORDLINE.LINENO,SUBSTR(ORDLINE.STYLE,1,lnMajorLen) AS STYLE,SUBSTR(ORDLINE.STYLE,lnMajorLen+2,lnColorLen) AS COLOR,ORDLINE.ACCOUNT ,;
         ORDLINE.START, ORDLINE.COMPLETE,SUM(ORDLINE.TOTQTY) AS TOTQTY,;
         ORDHDR.CUSTPO, ORDHDR.NOTE1 ,.F. AS PRINTED,OrdLine.Scale ;
  FROM   ORDLINE , &lcStyTmp ,ORDHDR;
  WHERE  SUBSTR(ORDLINE.STYLE,1,lnMajorLen)+SUBSTR(ORDLINE.STYLE,lnMajorLen+2,lnColorLen)+DTOS(ORDLINE.COMPLETE)+ORDLINE.ORDER+;
         ORDLINE.STORE+STR(ORDLINE.LINENO,6) =;
         &lcStyTmp..STYLE + &lcStyTmp..COLOR ;
    AND  &lcFilter1;
    AND  ORDHDR.ORDER = ORDLINE.ORDER;
    AND  ORDHDR.STATUS <> "X" ;
   INTO  TABLE (gcWorkDir+lcOrdLTmp);
  GROUP  BY ORDLINE.STYLE,ORDLINE.ORDER
  GOTO TOP 
  INDEX  ON STYLE+DTOS(COMPLETE)+ORDER+COLOR TAG &lcOrdLTmp
ENDIF   &&-- END IF FOR THE FLAG llOGFltCh

SELECT (lcPosLTmp)
GOTO TOP
SELECT (lcOrdLTmp)
GOTO TOP

IF EOF(lcPosLTmp) .AND. EOF(lcOrdLTmp)
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SELECT DISTINCT &lcStyTmp..STYLE,&lcStyTmp..PrePak;
  FROM &lcStyTmp ;
  INTO TABLE (gcWorkDir+lcStyTmp1)

*-- REPORT LOOP

STORE 0  TO PAGENO,laTotQty,laSubTot,lnTotQty,laOTS,laTotOpQty

ROW     = 99
lcSty   = SPACE(12)
laColor = SPACE(6)
lnMaxRow= 59

SELECT(lcStyTmp1)
SET RELATION TO 
SET RELATION TO STYLE+SPACE(7) INTO &lcPosLTmp , STYLE INTO &lcOrdLTmp ADDITIVE
SELECT(lcOrdLTmp)
SET RELATION TO 
SET RELATION TO ORDER +STR(LINENO,6) INTO INVLINE ADDITIVE
 *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
SELECT(lcStyTmp1)
SCAN WHILE INKEY() <> 32
  WAIT WINDOW 'Printing ....<Space Bar> to Abort' NOWAIT
  SET DEVICE TO PRINT
  lcSty = &lcStyTmp1..STYLE
  llPrtNot = .F.
  lcHeader  = "VND P/O   VIA  BUYER        ORDER DT DLVRY DT SOLD FOR          SPCL NOTES"
  lcLine    = "--- ---   ---  -----        -------- -------- --------          ----------"
  =lfPrnRep(lcPosLTmp,"PO")
  llPrtNot = .F.   
  lnGNum = lfGNum(lcSty,lcOrdLTmp)
  =lfShipped()
  lcHeader = "SOLD TO              P.O. #     P.O. DT  DLVRY DT   NOTE"
  lcLine   = "-------------------- ---------- -------- --------   --------------------"
  =lfPrnRep(lcOrdLTmp,"SO")
ENDSCAN
DO ENDREPORT  && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREEN
RETURN

*!**************************************************************************
*! Name        : lfCrtSetup
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 09/14/1999
*! Purpose     : to call link company screen.
*!**************************************************************************
*! Called from : .....
*!**************************************************************************
*! Calls       : .....
*!**************************************************************************
*! Example     : = lfCrtSetup()
*!**************************************************************************
*
FUNCTION lfCrtSetup

IF !FILE(gcDataDir+'KRASETUP.MEM')
  DIMENSION laComp[1,2]
  STORE '' TO laComp , lcMcompdir , lcDir
  DO (gcSCrDir+"KRASETUP.SPX")  && Run The Linked Directry and Selected Company.
ENDIF

IF FILE(gcDataDir+'KRASETUP.MEM')
  RESTORE FROM (gcDataDir+'KRASETUP.MEM') ADDITIVE
ELSE
  llOgTrmnat = .T.
  CLEAR READ
ENDIF  
*-- End of lfCrtSetup.

*!**************************************************************************
*! Name      : lfPrnRep
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/15/1999 
*! Purpose   : Print  PO lines & sales order lines .
*!**************************************************************************
*! Calls :   : Functions  : lfGNum(),lfPrtNot(),lfPrt()
*!           : Procedures : lpPrtHdr 
*!**************************************************************************
*! Passed Parameters  : lcFile,lcTyp
*!**************************************************************************
*! Returns            : ........
*!**************************************************************************
*! Example            : =lfPrnRep(lcOrdLTmp,"SO")
*!**************************************************************************
*
FUNCTION lfPrnRep
PARAMETERS lcFile,lcTyp

IF EOF(lcFile)
  IF lcFile = "&lcPosLTmp"
    llNewPag = .F.
  ENDIF
  RETURN
ENDIF

lnGNum = lfGNum(lcSty,lcFile)
lnGroup = 0

lcStyleNam = SPACE(12)
lnRecNo = 1
FOR lnCount = 1 TO lnGNum
  lcOrder  = SPACE(6)
  llFirst  = .T.
  STORE 0 TO laTotQty,laTotOpQty,laSubTot,laOTS
  lnElement = (lnCount-1) * 7 + 1
  GO RECNO() IN &lcStyTmp1
  SELECT(lcFile)
  SCAN WHILE INKEY() <> 32 AND SUBSTR(STYLE,1,lnMajorLen) = &lcStyTmp1..STYLE FOR !PRINTED
    IF ROW >= lnMaxRow OR SUBSTR(STYLE,1,lnMajorLen) <> lcStyleNam
      PAGENO = PAGENO+1
      DO lpPrtHdr WITH 'ICKRA800',XTITLE,XTITLE1,XTITLE2,R_WIDTH
      ROW = IIF(EMPTY(XTITLE),5,6)
      lcStyleNam = SUBSTR(STYLE,1,lnMajorLen)
    ENDIF
    IF !llPrtNot
      ROW = ROW + 1
      @ ROW,0 SAY "STYLE -> " + SUBSTR(ALLTRIM(lcSty),1,10) +" - "
      @ ROW,22 SAY "Scale " + IIF(SEEK("S"+&lcFile..Scale,"SCALE"),"["+SCALE+"]:","")
      lnCol = 0
      FOR lnCounter = 1 TO 8
        lcCounter = STR(lnCounter,1)
        @ ROW,35 + lnCol SAY SCALE.Sz&lcCounter
        lnCol = lnCol + 7
      ENDFOR
      ROW = ROW + 1
      IF SEEK('P'+&lcFile..Scale+&lcStyTmp1..PrePak,'Scale')
        =lfPrnPrePk()
      ENDIF
      =lfPrtNot()
    ENDIF
    IF lnGroup <> lnCount
      Row = Row + 1
      =lfPrnHdr()
      @ ROW,0 SAY lcHeader
      =lfPrt("laColor",.T.,.F.)
      @ ROW,0 SAY lcLine
      =lfPrt("laColor",.F.,.T.)
      Row = Row + 1
      =lfPrnHdr()
      lnGroup = lnCount
      lcOrder = SPACE(6)
    ENDIF
    lnElmNum = ASCAN(laColor,COLOR,lnElement,7)
    IF lnElmNum <> 0
      lnElmNum1 = lnElmNum -lnElement + 1
      IF lcTyp = "PO" 
        IF lcOrder <> PO
          lcOrder = PO
          ROW =ROW +1  
          @ ROW,0  SAY SUBSTR(CVENDOR,1,3)
          @ ROW,4  SAY SUBSTR(PO,1,5)
          @ ROW,10 SAY SHIPVIA
          @ ROW,15 SAY SUBSTR(CONTACT,1,10)
          @ ROW,28 SAY ENTERED
          @ ROW,37 SAY dDelvDATE
          @ ROW,46 SAY Available
          @ ROW,64 SAY SUBSTR(FOB,1,13)
        ENDIF
        @ ROW,78+((lnElmNum1-1)* 8) SAY TOTQTY PICTURE '9999999'        
      ELSE
        IF llFirst
          llFirst = .F.
          ldComDat=lfCalcDate(COMPLETE)
        ENDIF
        IF COMPLETE > ldComDat
          ROW =ROW +1
          =lfPrt("laSubTot",.F.,.T.)
          ROW = ROW + 1
          @ ROW,9 SAY "Total UpTo " + DTOC(ldComDat) + " Delivery" 
          =lfPrt("laSubTot",.T.,.T.)
          ldComDat=lfCalcDate(COMPLETE)
          laSubTot = 0
          ROW = ROW +1
        ENDIF
        lnAlias1    = SELECT()
        lcTmpOrdr1 = ORDER
        lcTmpStyl1 = SUBSTR(STYLE,1,lnMajorLen)
        lcTmpColr1 = COLOR
        SELECT INVLINE
        lnRecNO2 = IIF(EOF(),0,RECNO())
        SUM TOTQTY TO lnInvQty FOR INVLINE.ORDER  +SUBSTR(INVLINE.STYLE,1,lnMajorLen)+SUBSTR(INVLINE.STYLE,lnMajorLen+2,lnColorLen) = ;
                       lcTmpOrdr1 + lcTmpStyl1 + lcTmpColr1
        IF lnRecNo2 <> 0
          GOTO lnRecNo2
        ENDIF
        SELECT(lnAlias1)
        lnTotQty = (lnInvQty + TotQty)
        IF lcOrder <> ORDER
          lcOrder = ORDER
          ROW =ROW +1  
          @ ROW,0 SAY IIF(SEEK("M"+ACCOUNT,"CUSTOMER"),SUBSTR(CUSTOMER.BTNAME,1,20),'')
          @ ROW,21 SAY SUBSTR(CUSTPO,1,10)
          @ ROW,32 SAY START
          @ ROW,41 SAY COMPLETE
          @ ROW,52 SAY SUBSTR(ALLTRIM(NOTE1),1,20)
        ENDIF
        @ ROW,78+((lnElmNum1-1)* 8) SAY TotQty PICTURE '9999999'
        laSubTot(lnElmNum) = laSubTot(lnElmNum) + TotQty
        lnRecNo = RECNO()
      ENDIF
      laTotQty(lnElmNum) = laTotQty(lnElmNum) + IIF(lcTyp = "SO",lnTotQty,TOTQTY)
      laTotOpQty(lnElmNum) = laTotOpQty(lnElmNum) +TOTQTY
     ELSE
      LOOP
    ENDIF
  ENDSCAN
  *-- Print group footer
  ROW = ROW + 1
  IF lcTyp = "SO"
    GO lnRecNo
    ldComDat=lfCalcDate(COMPLETE)
    =lfPrt("laSubTot",.F.,.T.)
    ROW = ROW + 1
    @ ROW,9 SAY "Total UpTo " + DTOC(ldComDat) + " Delivery" 
    =lfPrt("laSubTot",.T.,.T.)
    ROW = ROW + 1
  ENDIF
  =lfPrt(" ",.F.,.T.)
  ROW = ROW + 1
  @ ROW,9 SAY IIF(lcTyp = "PO","Orders Placed Total","Sales Orders total")
  =lfPrt("laTotQty",.T.,.T.)
  =lfOpnToSel()
  ROW = ROW + 2
ENDFOR
*-- END OF lfPrnRep.

*!**************************************************************************
*! Name      : lpPrtHdr
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/15/1999 
*! Purpose   : PRINTS THE HEADER ON THE REPORTS
*!**************************************************************************
*! Calls     : ........
*!**************************************************************************
*! Passed Parameters  : XPROG,XRPTNAME,XRPTNAME1,XTYPE
*!**************************************************************************
*! Returns   : ........
*!**************************************************************************
*! Example   : DO lpPrtHdr WITH 'KRA800',XTITLE,XTITLE1,R_WIDTH
*!**************************************************************************
PROCEDURE lpPrtHdr
PARAMETER XPROG,XRPTNAME,XRPTNAME1,XRPTNAME2,XTYPE

PRIVATE ALL LIKE X*
XRPTNAME = TRIM(XRPTNAME)
XRPTNAME1 = TRIM(XRPTNAME1)
R_TITLE  = TRIM(R_TITLE)
DO CASE
 CASE XTYPE = 'W'                            && WIDE REPORT
  X1 = ((130 - (LEN(TRIM(gcCom_NAME))))/2)
  X2 = ((130 - (LEN( R_TITLE  )))/2)
  X3 = ((130 - (LEN( XRPTNAME )))/2)
  X4 = ((130 - (LEN( XRPTNAME1 )))/2)
  X5 = ((130 - (LEN( XRPTNAME2 )))/2)
  @ 01,000 SAY XPROG
  @ 01,X1  SAY QCOMPANY
  @ 01,120 SAY DATE()
  @ 01,129 SAY '~'   
  @ 02,000 SAY TIME()
  @ 02,X2  SAY R_TITLE 
  @ 02,120 SAY 'PAGE#'
  @ 02,126 SAY STR(PAGENO,4)
  lnRow = 3
  IF !EMPTY(XRPTNAME)
    @ lnRow,X3  SAY XRPTNAME
    lnRow = lnRow+1
  ENDIF
  @ lnRow,X4  SAY XRPTNAME1
  lnRow = lnRow+1
  @ lnRow,X5 SAY XRPTNAME2
  lnRow = lnRow+1
  @ lnRow,00 SAY REPLICATE('*',132)
 CASE XTYPE = 'XW'
   X1 = ((203 - (LEN(TRIM(QCOMPANY))))/2)
   X2 = ((203 - (LEN( R_TITLE  )))/2)
   X3 = ((203 - (LEN( XRPTNAME )))/2)
   @ 01,000 SAY XPROG
   @ 01,X1  SAY gcCom_NAME
   @ 01,190 SAY gdSysDate
   @ 01,199 SAY '~'   &&TAK 06/05/94
   @ 02,000 SAY TIME()
   @ 02,X2  SAY R_TITLE 
   @ 02,190 SAY 'PAGE#'
   @ 02,196 SAY STR(PAGENO,4)
   @ 03,X3  SAY XRPTNAME
   @ 04,00  SAY REPLICATE('*',225)
 CASE XTYPE='N'
   X1 = ((80 - (LEN(TRIM(QCOMPANY))))/2)
   X2 = ((80 - (LEN( R_TITLE  )))/2)
   X3 = ((80 - (LEN( XRPTNAME )))/2)
   @ 01,000 SAY XPROG
   @ 01,X1  SAY gcCom_NAME
   @ 01,70  SAY gdSysDate
   @ 01,79  SAY '~'
   @ 02,000 SAY TIME()
   @ 02,X2  SAY R_TITLE
   @ 02,70  SAY 'PAGE#'
   @ 02,76  SAY STR(PAGENO,4)
   @ 03,X3  SAY XRPTNAME
   @ 04,00  SAY REPLICATE('*',80)
ENDCASE
RETURN
*-- END OF lpPrtHdr.

*!**************************************************************************
*! Name      : lfGNum
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/15/1999 
*! Purpose   : Calculate the number of color groups for eash style.
*!**************************************************************************
*! Called    : ......
*!**************************************************************************
*! Passed Parameters  : lcStyle
*!**************************************************************************
*! Returns   : lnGrpNum
*!**************************************************************************
*! Example   : lfGNum(&lcStyTmp..STYLE)
*!**************************************************************************
*
FUNCTION lfGNum
PARAMETERS lcStyle,lcFile

SELECT DISTINCT &lcStyTmp..STYLE , &lcStyTmp..COLOR;
FROM &lcFile,&lcStyTmp;
WHERE &lcStyTmp..STYLE + &lcStyTmp..COLOR = ;
      SUBSTR(&lcFile..STYLE,1,12) + &lcFile..COLOR ;
 AND  &lcStyTmp..STYLE = SUBSTR(LCSTYLE,1,12);
INTO  CURSOR (lcCountTmp)

lnCount = _TALLY
lnGrpNum = CEILING(lnCount/7)
IF lnCount > 0
  lnArr = lnGrpNum * 7
  DIMENSION laColor(lnArr)
  DIMENSION laTotQty(lnArr)
  DIMENSION laTotOpQty(lnArr)
  DIMENSION laSubTot(lnArr)
  DIMENSION laOTS(lnArr)
ENDIF
STORE 0 TO laTotQty,laTotOpQty,laSubTot
STORE SPACE(6) TO laColor
I=0
SCAN
  I=I+1
  STORE COLOR TO laColor(I)
ENDSCAN
lnArrInd = I
RETURN lnGrpNum
*-- END OF  lfGNum.

*!**************************************************************************
*! Name      : lfPrt
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/15/1999 
*! Purpose   : Print a group of color or total qty.
*!**************************************************************************
*! Calls     : ........
*!**************************************************************************
*! Passed Parameters  : lcArray,llPrint1,llPrint2
*!**************************************************************************
*! Returns   : ........
*!**************************************************************************
*! Example   : lfPrt("lcColor",.T.,.T.)
*!**************************************************************************
*
FUNCTION lfPrt
PARAMETERS lcArray,llPrint1,llPrint2

IF llPrint1
  lnCol =0
  FOR I = lnElement TO (lnElement+6)
     @ ROW,IIF(lcArray="laColor",79+lnCol,78+lnCol);
       SAY IIF(lcTyp="SO".AND.lcArray = "laTotQty",laTotOpQty(I),&lcArray(I));
               PICTURE IIF(lcArray = "laColor",'@Z','@Z 9999999')
  lnCol = lnCol + 8 
  ENDFOR
  ROW = ROW + 1
ENDIF
IF llPrint2
  lnCol =0
  FOR I = lnElement TO (lnElement+6)
    IF EMPTY(laColor(I))
      LOOP  
    ENDIF
    IF lcArray <> "laOTS"
       @ ROW , IIF(lcArray="laColor",79+lnCol,78+lnCol) SAY ;
               IIF(lcTyp = "SO" AND (lcArray="laTotQty" OR lcArray=" "),'======',"------")+;
               IIF(lcArray = "laColor","",;
               IIF(lcTyp="SO" AND (lcArray="laTotQty" OR lcArray=" "),"=","-"))
    ELSE
      @ Row,78+lnCol SAY "*******"      
    ENDIF        
    lnCol = lnCol + 8
  ENDFOR
ENDIF
*-- END OF lfPrt.

*!**************************************************************************
*! Name      : lfPrtNot
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/15/1999 
*! Purpose   : Print style notepad.
*!**************************************************************************
*! Calls     : ........
*!**************************************************************************
*! Returns   : ........
*!**************************************************************************
*! Example   : lfPrtNot()
*!**************************************************************************
*
FUNCTION lfPrtNot

lcAlias = Alias()
lnNotLine = 1
SELECT NOTEPAD
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 72
SEEK 'F' + lcSty
lnMemLins = MEMLINES(NOTEPAD.MNOTES)
IF TYPE + KEY = 'F' + lcSty
  DO WHILE lnNotLine <= lnMemLins
    =lfPrnHdr()
    IF !EMPTY(MLINE(MNOTES,lnNotLine))
      @ ROW,20 SAY MLINE(MNOTES,lnNotLine)
      ROW = ROW + 1
    ENDIF
    lnNotLine = lnNotLine + 1
  ENDDO
ENDIF
SET MEMOWIDTH TO lnOldMemW
llPrtNot = .T.
SELECT &lcAlias
*-- END OF lfPrtNot.

*!**************************************************************************
*! Name      : lfCalcDate
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/15/1999 
*! Purpose   :Return the date of middle or  the month .
*!**************************************************************************
*! Calls     : ........
*!**************************************************************************
*! Passed Parameters  : ldCdate
*!**************************************************************************
*! Returns   : ........
*!**************************************************************************
*! Example   : lfCalcDate(date())
*!**************************************************************************
*
FUNCTION lfCalcDate
PARAMETERS ldCdate

IF DAY(ldCdate) <= 15
  RETURN (ldCdate + 15 - DAY(ldCdate))
ENDIF 
lnMonth=IIF(MONTH(ldCdate)+1 = 13,'01',ALLTRIM(STR(MONTH(ldCdate)+1)))
ldDate = CTOD(lnMonth+'/'+'1'+'/'+SUBSTR(ALLTRIM(STR(YEAR(ldCdate)+ IIF(lnMonth = "01",1,0))),3,2))-1
RETURN ldDate
*-- END OF lfCalcDate.

*!**************************************************************************
*! Name      : lfPrnPrePk
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/15/1999 
*! Purpose   :To print the PrePacks of the style's scale
*!**************************************************************************
*! Calls     : ........
*!**************************************************************************
*! Returns   : ........
*!**************************************************************************
*! Example   : lfPrnPrePk()
*!**************************************************************************
*
FUNCTION lfPrnPrePk
PRIVATE lnAlias,lnCol,lcScale

lnAlias   = SELECT(0)
SELECT Scale
*-- To print the header when the row exceeds the Max row.
=lfPrnHdr()
lnCol     = 35
@ Row,22 SAY "Prepak" +"["+PrePak+"]:"
FOR lnCounter = 1 TO 8
  lcCounter = STR(lnCounter,1)
  IF Pp&lcCounter > 0
     @ Row,lnCol+2 SAY Pp&lcCounter
    lnCol = lnCol + 7
  ENDIF
ENDFOR
Row = Row + 1
SELECT(lnAlias)
*-- END OF lfPrnPrePk.

*!**************************************************************************
*! Name      : lfPrnHdr
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/15/1999 
*! Purpose   :To print the header when the row exceeds the max row.
*!**************************************************************************
*! Calls     : ........
*!**************************************************************************
*! Returns   : ........
*!**************************************************************************
*! Example   : =lfPrnHdr()
*!**************************************************************************
*
FUNCTION lfPrnHdr

IF Row >= lnMaxRow
  PAGENO = PAGENO+1
  DO lpPrtHdr WITH 'ICKRA800',XTITLE,XTITLE1,XTITLE2,R_WIDTH
  Row = IIF(EMPTY(XTITLE),5,6)
  Row = Row + 1
ENDIF
*-- END OF lfPrnHdr.

*!**************************************************************************
*! Name      : lfOpnToSel
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/15/1999 
*! Purpose   :To print the Open To Sell quantities.
*!**************************************************************************
*! Calls     : ........
*!**************************************************************************
*! Returns   : ........
*!**************************************************************************
*! Example   : =lfOpnToSel()
*!**************************************************************************
*
FUNCTION lfOpnToSel
PRIVATE lnAlias

lnAlias = SELECT(0)
IF lcTyp = "SO"
  lnPoQty = 0
  Row = Row + 1
  =lfPrnHdr()
  =lfPrt("laOTS",.F.,.T.)
  Row = Row + 1
  =lfPrnHdr()
  @ Row,9 SAY "Open To Sell" 
  SELECT (lcPosLTmp)
  lnPoRecNo = RECNO()
  SET ORDER TO &lcPosLTmp1
  FOR lnArrayElm = lnElement TO (lnElement+6)
    SELECT (lcPosLTmp)
    = SEEK (lcStyleNam+SPACE(7)+laColor[lnArrayElm])
    SUM REST TotQty WHILE Style+Color = lcStyleNam+SPACE(7)+laColor[lnArrayElm] TO lnPoQty
    laOTS[lnArrayElm] = lnPoQty - laTotQty[lnArrayElm]
  ENDFOR
  SET ORDER TO &lcPosLTmp
  IF BETWEEN(lnPoRecNo,1,RECCOUNT(lcPosLTmp))
     GO lnPoRecNo IN (lcPosLTmp)
   ENDIF  
  =lfPrt("laOTS",.T.,.T.)
ENDIF
SELECT (lnAlias)
*-- END OF lfOpnToSel.

*!**************************************************************************
*! Name      : lfShipped
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/15/1999 
*! Purpose   : To print the Invoice
*!**************************************************************************
*! Calls     : ........
*!**************************************************************************
*! Returns   : ........
*!**************************************************************************
*! Example   : lfShipped()
*!**************************************************************************
*
FUNCTION lfShipped
PRIVATE lnAlias, lcTmpInv, lcTmpOrd,lnRecNo1,lnMRecNo,lnCounter,lnTotQty1

lnAlias  = SELECT()
lnMRecNo = RECNO()
lnIRecNo = RECNO('InvLine')
lnDim    = 1
STORE .F. TO llMulti , llContin
DIMENSION laInvTot(lnDim,lnArrInd+6)
SELECT INVLINE
SELECT (lcOrdLTmp)

SELECT DISTINCT &lcOrdLTmp..ORDER,&lcOrdLTmp..COMPLETE;
FROM   &lcOrdLTmp;
WHERE  STYLE+DTOS(COMPLETE)+ORDER+COLOR = SUBSTR(lcSty,1,lnMajorLen);
INTO   CURSOR (lcInvTemp)

IF _TALLY = 0
  RETURN
ENDIF

SCAN
  lcTmpOrd  = ORDER
  ldComDate = COMPLETE
  SELECT INVLINE
  GOTO TOP
  IF SEEK(lcTmpOrd)
    llContin = .T.
    lcTmpInv  = INVLINE.Invoice
    lcIScale  = INVLINE.SCALE
    lcTmpAcc  = INVLINE.ACCOUNT
    ldInvDate = INVLINE.InvDate
    lnRecNo1 = RECNO()
    LOCATE REST FOR INVOICE <> lcTmpInv WHILE ORDER = lcTmpOrd
    llMulti = FOUND()
    GOTO lnRecNo1
    DIMENSION laInvTot(lnDim,lnArrInd+6)
    laInvTot(lnDim,lnArrInd+1) = IIF(SEEK("M"+lcTmpAcc,"CUSTOMER"),CUSTOMER.BTNAME,'')
    laInvTot(lnDim,lnArrInd+2) = ldComDate
    laInvTot(lnDim,lnArrInd+3) = lcTmpInv
    laInvTot(lnDim,lnArrInd+4) = ldInvDate
    laInvTot(lnDim,lnArrInd+5) = lcTmpOrd
    laInvTot(lnDim,lnArrInd+6) = llMulti
    FOR lnIndex = 1 TO lnArrInd
      SUM TotQty TO laInvTot(lnDim,lnIndex) FOR;
      Order = lcTmpOrd .AND.;
      SUBSTR(STYLE,1,lnMajorLen) + SUBSTR(STYLE,lnMajorLen+2,lnColorLen)= SUBSTR(lcSty,1,lnMajorLen)+;
      laColor(lnIndex)
    ENDFOR
    lnDim = lnDim + 1
  ENDIF
ENDSCAN
IF !llContin
  RETURN
ENDIF
IF llNewPag
  PAGENO = PAGENO+1
ENDIF  
DO lpPrtHdr WITH 'ICKRA800',XTITLE,XTITLE1,XTITLE2,R_WIDTH
ROW = IIF(EMPTY(XTITLE),5,6)
ROW = ROW + 1
@ ROW,0 SAY "STYLE -> " + SUBSTR(ALLTRIM(lcSty),1,10) +" - "
@ ROW,22 SAY "Scale " + IIF(SEEK("S"+lcIScale,"SCALE"),"["+lcIScale+"]:","")
lnCol = 0
FOR lnCounter = 1 TO 8
  lcCounter = STR(lnCounter,1)
  @ ROW,35 + lnCol SAY SCALE.Sz&lcCounter
  lnCol = lnCol + 7
ENDFOR
ROW = ROW + 1
IF SEEK('P'+lcIScale+&lcStyTmp1..PrePak,'Scale')
  =lfPrnPrePk()
ENDIF
=lfPrtNot()
lnMaxDim = lnDim - 1
DIMENSION laClrTot(lnArrInd)
STORE 0 TO laClrTot
lnDim  = 1
lnColr = 1
FOR lnDim = 1 TO lnArrInd
  FOR lnColr = 1  TO lnMaxDim
    laClrTot(lnDim) = laClrTot(lnDim) + laInvTot(lnColr,lnDim)
  ENDFOR
ENDFOR
ROW = ROW + 2
*---Start printing
lnTemp  =  7
lnStInd =  1
lnDim   =  1
llPrint = .T.
DO WHILE .T.
  IF llPrint
    @ ROW , 00 SAY "     FULL CUSTOMER NAME           INVOICE #    INVOICE DATE    ORDER #"  
    lnPos = 79
    FOR lnIndex = lnStInd  TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
      @ ROW , lnPos SAY laColor(lnIndex)
      lnPos = lnPos + 8
    ENDFOR
    ROW = ROW + 1    
    @ ROW , 00 SAY "------------------------------    ---------    ------------    -------"
    lnPos = 79
    FOR lnIndex = lnStInd TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
      @ ROW ,lnPos SAY "------"
      lnPos = lnPos + 8
    ENDFOR
  ENDIF
*---Start printing data
  ROW = ROW + 1
  @ ROW , 00 SAY laInvTot(lnDim,lnArrInd + 1)
  IF laInvTot(lnDim,lnArrInd + 6)
    @ ROW , 48 SAY laInvTot(lnDim,lnArrInd + 2)
  ELSE
    @ ROW , 35 SAY laInvTot(lnDim,lnArrInd + 3)
    @ ROW , 48 SAY laInvTot(lnDim,lnArrInd + 4)
  ENDIF
  @ ROW , 64 SAY laInvTot(lnDim,lnArrInd + 5)
*---End printing data
  lnPos = 79
  FOR lnIndex = lnStInd TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
    @ ROW ,lnPos+1 SAY IIF(laInvTot(lnDim,lnIndex) > 0 ,laInvTot(lnDim,lnIndex) ,'') PICTURE '99999'
    lnPos = lnPos + 8
  ENDFOR
  llPrint = .F.
  IF lnDim >= lnMaxDim
    ROW = ROW + 1
    lnPos = 79
    FOR lnIndex = lnStInd TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
      @ ROW ,lnPos SAY "------"
      lnPos = lnPos + 8
    ENDFOR
    ROW = ROW + 1
    @ ROW ,09 SAY "Invoices Total"
    lnPos = 79
    FOR lnIndex = lnStInd TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
      @ ROW ,lnPos+1 SAY IIF(laClrTot(lnIndex)>0,laClrTot(lnIndex),'') PICTURE '99999'
      lnPos = lnPos + 8
    ENDFOR
    ROW = ROW + 1
    lnPos = 79
    FOR lnIndex = lnStInd TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
      @ ROW ,lnPos SAY "------"
      lnPos = lnPos + 8
    ENDFOR
    ROW = ROW + 2
    lnDim   = 1
    lnStInd = lnTemp + 1
    lnTemp = lnTemp + 7
    llPrint = .T.
    ROW = ROW + 1
  ELSE
    lnDim   = lnDim + 1
  ENDIF
  IF lnStInd > lnArrInd
    EXIT
  ENDIF
  =lfPrnHdr()
ENDDO
RETURN
SELECT(lnAlias)
GOTO lnIRecNo IN InvLine
GOTO lnMRecNo
*-- END OF lfShipped.

*!*************************************************************
*! Name        : lfvFabric
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 09/14/1999
*! Purpose     : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Return      : ........
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = lcRpFabr
*--IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal, 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF   
*-- END OF lfvFabric.

*!*************************************************************
*! Name      : lfWoldVal
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/14/1999
*! Purpose   : To return the old value.
*!*************************************************************
*! Calls     : ........
*!*************************************************************
*! Returns   : ........
*!*************************************************************
*! Example   :  =lfWoldVal()
*!*************************************************************
FUNCTION lfWoldVal

laOldVal = EVALUATE(SYS(18))
*-- END OF lfWoldVal.

*!*************************************************************
*! Name        : lfvStyle
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 09/14/1999
*! Purpose     : validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ........
*!*************************************************************
*! Return      : ........
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
*
FUNCTION lfvStyle

lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE
IF !EMPTY(&lcStyle.) 
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor  
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
    IF EMPTY(&lcStyle)
      &lcStyle = laOldVal
    ENDIF
  ENDIF
ENDIF
SET ORDER TO lcTag IN STYLE
*-- END OF lfvStyle.

*!*************************************************************
*! Name        : lfMajGet
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 09/14/1999
*! Purpose     : To get the title and picture of style major segement 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ........
*!*************************************************************
*! Example     : = lfMajGet()
*!*************************************************************
*
FUNCTION lfMajGet

lcMajPic = "@! " + gfItemMask("PM")
lcMajTtl = gfItemMask("HM")
*-- END OF lfMajGet.

*!*************************************************************
*! Name        : lfEvalSegs
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 09/14/1999
*! Purpose     : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ........
*!*************************************************************
*! Return      : ........
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
*
FUNCTION lfEvalSegs

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
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
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl) 
ENDIF

*-- Compute Variables titles. [begin]
IF EMPTY (lcStyMajor)
  lcStyMake  = 'Style (Make/Buy)'
ELSE
  lcStyMake  = lcStyMajor + ' (Make/Buy)'
ENDIF 
RETURN ''
*-- End of lfEvalSegs.

*!*************************************************************
*! Name        : lfVDir
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 09/14/1999 
*! Purpose     : 
*!*************************************************************
*! Calls       : ........
*!*************************************************************
*! RETURN      : ........
*!*************************************************************
*! Example     : =lfVDir()
*!*************************************************************
*
FUNCTION lfVDir

lnCount = 0
lcDir = GETDIR()
lcDir = SUBSTR(lcDir,1,AT('\',lcDir,2))
SHOW GET lcDir
IF EMPTY(lcDir)
  RETURN
ENDIF
IF FILE(lcDir+'SYSFILES\SYCCOMP.DBF')
  USE IN (IIF(USED(lcAliasN),lcAliasN,0))
  *=gfOpenFile(lcDir+'SYSFILES\SYCCOMP',lcDir+'SYSFILES\Ccomp_id','SH',lcAliasN)
  =gfOpenFile(lcDir+'SYSFILES\SYCCOMP',lcDir+'SYSFILES\Ccomp_id','SH',@lcAliasN,.T.)
  SELECT (lcAliasN)
  COUNT TO lcCompNo FOR !EMPTY(CCOMP_id)
  DIMENSION laComp[lcCompNo,2]
  laComp = ''
  SCAN
    lnCount = lnCount + 1
    laComp[lnCount,1] = cComp_Id + '  ' + ALLTRIM(cCom_Name)
    laComp[lnCount,2] = ccom_ddir
  ENDSCAN  
  SHOW GET PbComp
ENDIF
IF lnCount > 0
  SHOW GET PbComp ENABLE
  SHOW GET PbOk   ENABLE
ENDIF
*-- END OF lfVDir.
*!*************************************************************
*! Name        : lfVAccpOk
*! Developer   : ABDOU ELGENDI - (ABD) 
*! Date        : 08/23/1999 
*! Purpose     : 
*!*************************************************************
*! Calls       : ........
*!*************************************************************
*! RETURN      : ........
*!*************************************************************
*! Example     : =lfVAccpOk()
*!*************************************************************
*
FUNCTION lfVAccpOk

IF !EMPTY(lcDir)
  lcMcompdir =  laComp[PbComp,2]
  lcMcompdir = ALLTRIM(lcMcompdir)
  IF FILE(lcMcompdir+'ITPOSHIP.DBF') .AND. FILE(lcMcompdir+'ITHBILLT.DBF')
     CLEAR READ
     SAVE ALL LIKE lcMcompdir TO (gcDataDir+'KRASETUP.MEM')
  ELSE
    *-- TEXT : Company files can not be Found
    =gfModalGen('TRM00113B00000','DIALOG','found') 
    SET DEVICE TO SCREEN
    laComp = ''
    SHOW GET pbComp
    SHOW GET PbComp DISABLE
    SHOW GET PbOk   DISABLE
    RETURN
  ENDIF
ENDIF
*-- END OF lfVAccpOk.

*!*************************************************************
*! Name      : lfvDateRng
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/23/1999 
*! Purpose   : Showes date range screen for delivery date
*!*************************************************************
*! Calls     : KRADATRG.spr
*!*************************************************************
*! Return    : ........
*!*************************************************************
*! Example   : = lfvDateRng()
*!*************************************************************
*
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo

ldFrom = LDATE
LDTO   = HDATE

lcTitle = 'Date range'
lcParam = 'D'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome+'KRADatRg.SPR') && Run The Date Range screen 
DO DateRng.Spx
*B603955,1 ABD - [End]

*-- END OF lfvDateRng.

*!*************************************************************
*! Name      : lfvCompDat
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/15/1999 
*! Purpose   : Showes date range screen for complete date
*!*************************************************************
*! Calls     : KRADATRG.spr
*!*************************************************************
*! Return    : ........
*!*************************************************************
*! Example   : = lfvCompDat()
*!*************************************************************
*
FUNCTION lfvCompDat

PRIVATE ldFrom,ldTo

ldFrom = LCompDATE
LDTO   = HCompDATE

lcTitle = 'Date range'
lcParam = 'C'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome+'KraDatRg.SPR')  && Run The Date Range screen 
DO DateRng.Spx
*B603955,1 ABD - [End]

*-- END OF lfvCompDat.

*!*************************************************************
*! Name      : lfvpbOk
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/23/1999 
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Calls     : ........
*!*************************************************************
*! Return    : ........
*!*************************************************************
*! Example   : =lfvpbOk()
*!*************************************************************
*
FUNCTION lfvpbOk

IF ldFrom > ldTo
  *-- Text : Date RANGE FROM must be less than or equal TO Date range to  
  =gfModalGen('TRM00272B00000','DIALOG','Date "'+"|"+'" must be less than or equal'+"|"+'Date " range to " ') 
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  IF lcParam = 'D'
    LDate = ldFrom
    HDate = ldTo
  ELSE
    LCompDATE = ldFrom
    HCompDATE = ldTo
  ENDIF  
  CLEAR READ
ENDIF

*-- END OF lfvpbOk.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : ........
*!*************************************************************
*! Returns   : ........
*!*************************************************************
*! Example   :  =lfClearRep()
*!*************************************************************
*
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(lcStyTmp),lcStyTmp,0)
ERASE &gcWorkDir.&lcStyTmp+'.DBF'
ERASE &gcWorkDir.&lcStyTmp+'.CDX'

USE IN IIF(USED(lcStyTmp1),lcStyTmp1,0)
ERASE &gcWorkDir.&lcStyTmp1+'.DBF'
ERASE &gcWorkDir.&lcStyTmp1+'.CDX'

USE IN IIF(USED(lcPosLTmp),lcPosLTmp,0)
ERASE &gcWorkDir.&lcPosLTmp+'.DBF'
ERASE &gcWorkDir.&lcPosLTmp+'.CDX'
ERASE &gcWorkDir.&lcPosLTmp1+'.CDX'


USE IN IIF(USED(lcOrdLTmp),lcOrdLTmp,0)
ERASE &gcWorkDir.&lcOrdLTmp+'.DBF'
ERASE &gcWorkDir.&lcOrdLTmp+'.CDX'

USE IN IIF(USED(lcCountTmp),lcCountTmp,0)
ERASE &gcWorkDir.&lcCountTmp+'.DBF'

*-- END OF lfClearRep.

*!*************************************************************
