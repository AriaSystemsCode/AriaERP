****************************************************************************
*: Program file      : POPAM200.PRG   (C#101630)
*: Program desc.     : STYLE PURCHASE ORDER DETAIL REPORT
*: System            : Aria Apparel System (A27).
*: Module            : Sales Order Allocation  (PO)
*: Developer         : ABDOU ELGENDI - (ABD)
*: Date              : 08/17/1999
*:**************************************************************************
*: Calls : FUNCTIONS : lfFillBin ,lfvBins   ,lfFiilBin  ,lfvBiins  ,lfClr
*:                   : lfvVendor ,lfvPO     ,lfUpdFltVar,lfMajGet  ,lfvStyle
*:                   : lfWoldVal ,lfEvalSegs,lfCollTime ,lfClearRep,lfUserPriv
*:                   : lfFltChng ,lfwRepWhen
*:                   : ...............  
*:         PROCEDURE : None.
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*:Modifications      :
*:**************************************************************************
*
 *--  Variable Declaration.
 lcStTime  = TIME()
 lnMajPict = SUBSTR(lcMajPic,4,19)
 lcStyPic = gfItemMask('HI')
 DIMENSION XDATA(1)
 STORE 00 TO lnNonMajSt
 STORE '' TO lcNonMajPi,lcNonMajTl,lcStyGrp,lcStyMajor
 XREPORT  = 'POPAM200'
 R_TITLE  = IIF(lcRpOut = 'D','STYLE PURCHASE ORDER DETAIL REPORT',;
                              'STYLE PURCHASE ORDER SUMMARY REPORT')
 NULLDATE =CTOD('  /  /    ')
 XVENDOR  = SPACE(8)
 LPO      = SPACE(6)
 HPO      = SPACE(6)
 XSTATUS  = SPACE(3)
 LENTERED = NULLDATE
 HENTERED = NULLDATE
 LCANCELD = NULLDATE
 HCANCELD = NULLDATE
 XDIV     = SPACE(2)
 XGRP     = SPACE(2)
 HSTYLE   = SPACE(7)
 LSTYLE   = SPACE(7)
 XCOLOR   = SPACE(3)
 XREP     = SPACE(4)
 XPRINT   = SPACE(1)
 XTITLE   = SPACE(40)
 XCONSOL  = SPACE(1)
 *-- END Variable Declaration.

*-- Function cheak if the filter change
=lfUpdFltVar()
*--- If user change filter criteria then you must collect data again [Begin]
IF llOGFltCh OR lfFltChng()
  DIME laRpCopyTg[1]
  DIME laRpCComTg[1]
  =ACOPY(laRpComTrg,laRpCComTg)  
  =ACOPY(laRpTarget,laRpCopyTg)
  =lfEvalSegs()
  DO WHILE .T.
    *--- Define the directry of company that collect the data from it.
    llClsSyuu = gfOpenFile(gcSysHome+'SYUUSRPR',gcSysHome+'CUSER_ID','SH') 
    IF llClsSyuu
      SELECT SYUUSRPR
      lcOldOdr = ORDER('SYUUSRPR')
     SET ORDER TO CUSER_ID IN SYUUSRPR
    ENDIF
    k = 0
    IF !EMPTY(laRpComTrg[1]) .AND. lcRpCons ='Y'
      FOR lnIndex = 1 TO ALEN(laRpComTrg)
        IF SEEK(PADL(laRpComTrg[lnIndex],2),'SYCCOMP')
          IF ASCAN(XDATA,ALLTRIM(SYCCOMP.Ccom_dDir))=0 AND ;
            lfUserPriv('PO','POPAM200','') 
            K=K+1
            DIME XDATA(K)
            XDATA(K) = ALLTRIM(SYCCOMP.Ccom_dDir)
          ENDIF
        ENDIF
      ENDFOR
    ELSE
      k = 1
      XDATA(1) = ALLTRIM(gcDataDir)
    ENDIF
    USE IN IIF(llClsSyuu,'SYUUSRPR',0)
    XFILTER = ' '
    IF !EMPTY(XVENDOR)
      XFILTER = XFILTER + '.AND. POSHDR.VENDOR=XVENDOR'
    ENDIF
    IF LPO<>' '.OR.HPO<>' '
      XFILTER = XFILTER +'.AND.PO>=LPO.AND.PO<=HPO'
    ENDIF
    IF DTOC(LENTERED)<>' '.OR.DTOC(HENTERED)<>' '
      XFILTER = XFILTER+'.AND.POSHDR.ENTERED>=LENTERED.AND.POSHDR.ENTERED<=HENTERED'
    ENDIF
    IF DTOC(LCANCELD)<>' '.OR.DTOC(HCANCELD)<>' '
      XFILTER = XFILTER+'.AND.POSHDR.COMPLETE>=LCANCELD.AND.POSHDR.COMPLETE<=HCANCELD'
    ENDIF
    IF !EMPTY(XSTATUS)
      XFILTER = XFILTER+'.AND.POSHDR.STATUS$XSTATUS'
    ENDIF
    IF !EMPTY(XDIV)
      XFILTER = XFILTER+'.AND.POSHDR.CDIVISION=XDIV'
    ENDIF                                    
    XFTRNCD = SPACE(0)
    IF LEN(TRIM(XREP)) <> 0
      IF 'O' $ XREP
        XFTRNCD=XFTRNCD+'1'
      ENDIF
      IF 'R' $ XREP
        XFTRNCD=XFTRNCD+'2'
      ENDIF
      IF 'S' $ XREP
        XFTRNCD=XFTRNCD+'3'
      ENDIF
      XFILTER = XFILTER + '.AND.TRANCD$XFTRNCD'
    ENDIF
    IF !EMPTY(LSTYLE)
      XFILTER = XFILTER + '.AND. STYLE.cStyMajor >= LSTYLE .AND. STYLE.cStyMajor <= HSTYLE '
    ENDIF
    IF !EMPTY(XGRP)
      XFILTER=XFILTER+'.AND.STYLE.cstygroup=XGRP'
    ENDIF
    IF LEN(XFILTER)>1
      XFILTER=SUBSTR(XFILTER,7)
    ELSE
      XFILTER="VAL(PO)<>0"
    ENDIF
   *-------------------------------------------
   * SELECT SORT SEQUENCE
   *-------------------------------------------
    BREAK  = ' '
    BREAKD = ' '
    DO CASE
      *- SORT BY ORDER#
      CASE lcRpSort = 'P'
        SORTFIELD = 'PO+TRANCD+STYLE+STR(RECNO(),7)'
        BREAK     = 'PO'
        BREAKD    = 'PO'
	  *- SORT BY COMPDATE
	  CASE lcRpSort = 'D'
        SORTFIELD = 'DTOS(COMPLETE)+PO+TRANCD+STYLE+STR(RECNO(),7)'
        BREAK     = 'DTOS(COMPLETE)'
        BREAKD    = 'DTOS(COMPLETE)'
      *- SORT BY VENDOR ACCOUNT
      CASE lcRpSort = 'V'
        SORTFIELD = 'VENDOR+PO+TRANCD+STYLE+STR(RECNO(),7)'
        BREAK     = 'VENDOR'
        BREAKD    = 'VENDOR'
      *- SORT BY PRIORITY
      CASE lcRpSort = 'S'
        SORTFIELD = 'STYLE+TRANCD+PO+STR(RECNO(),7)'
        BREAK     = 'STYLE'
        BREAKD    = 'STYLE'
      *- SORT BY COLOR
      CASE lcRpSort = 'O'
        SORTFIELD = 'STYLE+TRANCD+PO+STR(RECNO(),7)'
        BREAK     = 'STYLE'
        BREAKD    = 'SUBSTR(Style,lnMajorLen+2,lnColorLen)'
    ENDCASE
    EXIT
 ENDDO
 CREATE TABLE &gcWorkDir.&POLTEMP;
 (cStyType C(1),Status C(1),Po     C(6),TRANCD C(1),Lineno N(4) ,Vendor C(8),;
  Style   C(19),Scale  C(1),Prepak C(1),Ppqty  N(4),Price N(9.2),Ncost1 N(9),;
  qty1     N(6),qty2   N(6),qty3   N(6),qty4   N(6),qty5   N(6) ,qty6   N(6),;
  qty7     N(6),qty8   N(6),Totqty N(7),Date D     ,Shipno C(6) ,Amount n(6.2),;
  Notepad1 C(39),Complete D,Ppak_desc C(15)  )
  XPO=SPACE(6)
  *---------------------------------------------------------
  * [3] SELECT REPORT FILE & INITIALIZE MEMORY
  *---------------------------------------------------------
  J=0
  *--Close OPEN FILES
  FOR J=1 TO K
    USE IN 'STYLE'
    USE IN 'POSHDR'
    USE IN 'SCALE'
    USE IN 'NOTEPAD'
    USE IN 'POSLN'
    llStyle   = gfOpenFile(XDATA(J)+'STYLE',XDATA(J)+'STYLE','SH')
    llPoshdr  = gfOpenFile(XDATA(J)+'POSHDR',XDATA(J)+'POSHDR','SH')
    llScale   = gfOpenFile(XDATA(J)+'SCALE',XDATA(J)+'SCALE','SH')
    llNotepad = gfOpenFile(XDATA(J)+'NOTEPAD',XDATA(J)+'NOTEPAD','SH')
    llPosln   = gfOpenFile(XDATA(J)+'POSLN',XDATA(J)+'POSLN','SH')    
    SET FILTER TO
    SET RELATION TO
    SET RELATION TO cStyType+PO INTO POSHDR
    SET RELATION TO (STYLE) INTO STYLE ADDITIVE
    WAIT WINDOW 'Selecting records for report ...' TIMEOUT 1
    LOCATE ALL FOR &XFILTER
    SET TALK ON
    COPY REST TO &gcWorkDir.&WORKTEMP FOR &XFILTER
    SET TALK OFF
    SELECT &POLTEMP
    XREC = RECCOUNT()
    SELECT &POLTEMP
    APPEND FROM &gcWorkDir.&WORKTEMP
    IF XREC > 0
      GOTO XREC
      SKIP
    ENDIF
    SELECT (POLTEMP)
    GO TOP
    IF !EOF()
       SET RELATION TO
       SET RELATION TO cStyType+PO INTO POSHDR
       SET RELATION TO STYLE INTO STYLE ADDITIVE
       SET RELATION TO 'P' + SCALE + PREPAK INTO SCALE ADDITIVE
       SET RELATION TO 'P'+PO INTO NOTEPAD ADDITIVE
       IF XREC > 0
          GOTO XREC
          SKIP
       ELSE
          GOTO TOP
       ENDIF
      lnOldRec  = RecNo()
      REPLACE REST FOR (RECNO() > XREC) ;
              STATUS WITH POSHDR->STATUS,;
              COMPLETE WITH POSHDR->COMPLETE,;
              PPAK_DESC WITH STR(SCALE->PP1,1)+STR(SCALE->PP2,2);
		+STR(SCALE->PP3,2)+STR(SCALE->PP4,2)+STR(SCALE->PP5,2);
    	+STR(SCALE->PP6,2)+STR(SCALE->PP7,2)+STR(SCALE->PP8,2)
      GOTO lnOldRec
      DO WHILE (RECNO() > XREC)
        lcPo = PO
        lcNote=''
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)
        FOR I=1 TO lnMemLins
          lcNote=lcNote+MLINE(NOTEPAD.MNOTES,I)
        ENDFOR
        REPLACE REST WHILE PO = lcPo ;
                NOTEPAD1 WITH SUBSTR(lcNote,1,27)
       IF EOF()
         EXIT
       ENDIF
     ENDDO  
   ENDIF
  ENDFOR
  SELE &POLTEMP
  GOTO TOP
  *- SORT TO WORKFILE INDEX
  IF SORTFIELD<>' '
    Z = LTRIM(STR(RECCOUNT(),7))
    INDEX ON &SORTFIELD TO &gcWorkDir.&POLTEMP
    SET INDEX TO &gcWorkDir.&POLTEMP
  ENDIF
*-- END IF FOR THE FLAG llOGFltCh
ENDIF
SELE &POLTEMP
GOTO TOP
IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..*ORD910                                                                                                                 PAGE: 1234
*MM/DD/YY      
*             COMPLETE
*             RECEIVED
*PO#    VEND. E.T.A.   SHPMT# S STYLE   CLR  ....PREPAK.....   TOTQTY  $PRICE     AMOUNT ............. NOTEPAD.......................
*XXXXXX XXXXX MM/DD/YY XXXXXX X XXXXXXX XXX  X X X X X X X X  9999999  999.99  999999.99 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*
*--------------------------------------------------------- RECEIVED LINES ----------------------------------------------------------
*XXXXXX XXXXX MM/DD/YY XXXXXX X XXXXXXX XXX  X X X X X X X X  9999999  999.99  999999.99 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*
*-------------------------------------------------------- IN-TRANSIT LINES ---------------------------------------------------------
*XXXXXX XXXXX MM/DD/YY XXXXXX X XXXXXXX XXX  X X X X X X X X  9999999  999.99  999999.99 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*
*
* *SUB TOTALS*    XXXXXXXXXXXX
*  ORDERED................................   999999 999999 999999 999999 999999 999999 999999 999999  9999999      999.99  999999.99
*  RECEIVED...............................   999999 999999 999999 999999 999999 999999 999999 999999  9999999      999.99  999999.99
*  OPEN...................................   999999 999999 999999 999999 999999 999999 999999 999999  9999999      999.99  999999.99
*  IN-TRANSIT.............................   999999 999999 999999 999999 999999 999999 999999 999999  9999999      999.99  999999.99
*
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

   *------------------ BEGIN PRINTING THE REPORT  --------------------*
lcEdTime = TIME()  && Time in which we finish collect data.
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(POLTEMP))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcStTime,lcEdTime),6,2)) + ' Seconds...' TIMEOUT 1
SELE &POLTEMP
GOTO TOP
DO WHILE .T.
   XFIRST = .T.
   STORE 0 TO FINALQTY,FINALAMT,AMOUNT1
   PAGENO = 0
   ROW    = 99
   XTIME  = TIME()
   SELECT &POLTEMP
   GOTO TOP
   SELE &POLTEMP
   PTRANCD=SPACE(1)
   *---------------------------------------------------------
   * [REPORT] LOOP
   *---------------------------------------------------------
   DO WHILE INKEY() <> 32
      WAIT WINDOW  'Report printing - <SPACE BAR> to abort' NOWAIT   
      IF ROW >=53
         PAGENO = PAGENO+1
         DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
         @ 05,00 SAY '             COMPLETE'
         @ 06,00 SAY '             RECEIVED'
         @ 07,00 SAY 'PO#    VENDOR   E.T.A.   SHPMT# S &lcStyPic ....PREPAK.....    TOTQTY     $PRICE      AMOUNT ..........NOTEPAD............'
         @ 08,00 SAY REPLICATE('=',132)
         ROW = 09
      ENDIF
      *--------------------- END SUBTOTALS ----------------------------
      IF EOF()
         EXIT
      ENDIF
      IF ROW >=53
         ROW = 99
         LOOP
      ENDIF
      SELECT &POLTEMP
      IF EMPTY(TRANCD)
        SKIP
        LOOP
      ENDIF
      DO WHILE XPRINT='D'
        XPO=PO
        ROW=ROW+1
        IF (TRANCD='1') .AND. (TRANCD<>PTRANCD)
          PTRANCD=TRANCD
		  IF XFIRST = .F.
          	ROW=ROW+1
          	@ ROW,45 SAY "TOTAL:"
          	@ ROW,58 SAY FINALQTY
          	@ ROW,70 SAY "AMOUNT:"
          	@ ROW,78 SAY FINALAMT
          	STORE 0 TO FINALQTY,FINALAMT
            ROW=ROW+1
          	@ ROW,00 SAY REPLICATE('-',53)
          	@ ROW,55 SAY 'ORDERED LINES'
          	@ ROW,70 SAY REPLICATE('-',62)
            ROW=ROW+1
          ELSE
          	@ ROW,00 SAY REPLICATE('-',53)
          	@ ROW,55 SAY 'ORDERED LINES'
          	@ ROW,70 SAY REPLICATE('-',62)
            ROW=ROW+1
          ENDIF
        ENDIF
        IF (TRANCD='2') .AND. (TRANCD<>PTRANCD)
          PTRANCD=TRANCD
          IF XFIRST = .F.
          	ROW=ROW+1
          	@ ROW,45 SAY "TOTAL:"
          	@ ROW,58 SAY FINALQTY
          	@ ROW,70 SAY "AMOUNT:"
          	@ ROW,78 SAY FINALAMT
          	STORE 0 TO FINALQTY,FINALAMT
            ROW=ROW+1
          	@ ROW,000 SAY REPLICATE('-',53)
          	@ ROW,55 SAY 'RECEIVED LINES'
          	@ ROW,70 SAY REPLICATE('-',62)
            ROW=ROW+1
          ELSE
          	@ ROW,000 SAY REPLICATE('-',53)
          	@ ROW,55 SAY 'RECEIVED LINES'
          	@ ROW,70 SAY REPLICATE('-',62)
            ROW=ROW+1
          ENDIF
        ENDIF
        IF (TRANCD='3') .AND. (TRANCD<>PTRANCD)
          PTRANCD=TRANCD
		  IF XFIRST = .F.
			ROW=ROW+1
          	@ ROW,45 SAY "TOTAL:"
          	@ ROW,58 SAY FINALQTY
          	@ ROW,70 SAY "AMOUNT:"
			@ ROW,78 SAY FINALAMT
          	STORE 0 TO FINALQTY,FINALAMT
            ROW=ROW+1
          	@ ROW,000 SAY REPLICATE('-',53)
          	@ ROW,54 SAY 'IN-TRANSIT LINES'
          	@ ROW,71 SAY REPLICATE('-',61)
            ROW=ROW+1
          ELSE
          	@ ROW,000 SAY REPLICATE('-',53)
          	@ ROW,54 SAY 'IN-TRANSIT LINES'
          	@ ROW,71 SAY REPLICATE('-',61)
            ROW=ROW+1
		  ENDIF        
        ENDIF
        @ ROW,000 SAY PO
        @ ROW,007 SAY VENDOR
        IF (TRANCD='2') .OR. (TRANCD='3')
          @ ROW,016 SAY DATE  
          @ ROW,025 SAY SHIPNO
        ENDIF
        IF TRANCD='1'
          @ ROW,016 SAY COMPLETE
        ENDIF
        @ ROW,032 SAY STATUS
        @ ROW,034 SAY SUBSTR(STYLE,1,7)
        @ ROW,047 SAY SUBSTR(Style,lnMajorLen+2,3)
		IF PPAK_DESC <> '0 0 0 0 0 0 0 0'        
        	@ ROW,54 SAY PPAK_DESC  
        ELSE	
        	@ ROW,54 SAY '<**NO PREPAK**>'  
        ENDIF
        @ ROW,72 SAY TOTQTY
        @ ROW,81 SAY NCOST1    PICTURE '999999.99'
		AMOUNT1 = TOTQTY * NCOST1
		@ ROW,92 SAY AMOUNT1 PICTURE '9999999.99'
		@ ROW,104 SAY NOTEPAD1
        XFIRST = .F.
        EXIT
      ENDDO
      AMOUNT1  = TOTQTY*NCOST1
      FINALQTY = FINALQTY + TOTQTY		
      FINALAMT = FINALAMT + AMOUNT1
      DO CASE
       CASE TRANCD='1'
        XTRNCD=1
       CASE TRANCD='2'
        XTRNCD=2
       CASE TRANCD='3'
        XTRNCD=3
      ENDCASE
      SELECT &POLTEMP
      SKIP
   ENDDO 
   *------------------ END MAIN REPORT LOOP --------------------
   X = 2
   IF LEN(TRIM(BREAK)) =0
      X =1
   ENDIF
   ROW=ROW+2
   IF ROW>53
     PAGENO = PAGENO+1
     ROW = 09
   ENDIF     
  	@ ROW,45 SAY "TOTAL:"
  	@ ROW,58 SAY FINALQTY
   	@ ROW,70 SAY "AMOUNT:"
    @ ROW,78 SAY FINALAMT
   ROW = ROW+1
  EXIT
ENDDO
DO ENDREPORT  && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREEN
RETURN
*------------------ END OF PRINTING THE REPORT --------------------*
*!*************************************************************
*! Name      : lfFillBin
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : Fill The Source Array  With Default data.
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : = lfFillBin()
*!*************************************************************
*
FUNCTION lfFillBin

DIME laRpSource[3]
DIME laRpTarget[1,1]
STORE '' TO laRpTarget,laRpSource
laRpSource[1] = 'Ordered'
laRpSource[2] = 'Receipt'
laRpSource[3] = 'Shipment'
*- END OF lfFillBin

*!*************************************************************
*! Name      : lfvBins
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : Call the Global Function Mover To Fill The Target Array
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : = lfvBins()
*!*************************************************************
*
FUNCTION lfvBins

= gfMover(@laRpSource,@laRpTarget,'Reports',.T.,'')
*- END OF lfvBins

*!*************************************************************
*! Name      : lfFiilBin
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : Fill The Source Array  With Default data.
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : = lfFiilBin()
*!*************************************************************
*
FUNCTION lfFiilBin

DIME laRpComSrc[1,1]
DIME laRpComTrg[1,1]
STORE '' TO laRpComSrc , laRpComTrg
SELECT DISTINCT Ccomp_id+' - '+ALLTRIM(ccom_Name) from Syccomp Into Array laRpComSrc
*- END OF lfFiilBin

*!*************************************************************
*! Name      : lfvBiins
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : Call the Global Function Mover To Fill The Target Array
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : = lfvBiins()
*!*************************************************************
*
FUNCTION lfvBiins
= gfMover(@laRpComSrc,@laRpComTrg,'Company',.T.,'')
*- END OF lfvBiins

*!*************************************************************
*! Name      : lfClr
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : Clean The Screen Of Option Grid After Select Companys.
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : = lfClr()
*!*************************************************************
*
FUNCTION lfClr
CLEAR READ
*- END OF lfClr

*!*************************************************************
*! Name      : lfvVendor
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : Vaildate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
*
FUNCTION lfvVendor

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcRPVendor) .AND. ;
   ('?' $ lcRPVendor .OR. !SEEK(lcRPVendor, 'APVENDOR'))
  =gfApVnBrow(@lcRPVendor)
ENDIF
*-- END OF lfvVendor

*!*************************************************************
*! Name      : lfvPO
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : Vaildate The Po Number
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPO()
*!*************************************************************
*
FUNCTION lfvPO

lcRpPoType = 'P'
lcPOFld    = VARREAD()

lcPONo = &lcPOFld
SET ORDER TO POSLN IN POSLN
IF !EMPTY(lcPONo) .AND. ('?' $ lcPONo .OR. !SEEK(lcRpPoType + lcPONo , 'POSLN'))
  DO POSBrow WITH lcPONo,"",lcRpPoType
  IF !EMPTY(lcPONo)
    &lcPOFld = lcPONo
  ELSE
    &lcPOFld = laOldVal
  ENDIF
ENDIF
*-- END OF lfvPO

*!*************************************************************
*! Name      : lfUpdFltVar  (C#101630)
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/15/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUpdFltVar()
*!*************************************************************
FUNCTION lfUpdFltVar

XVendor   = lcRPVendor
XTITLE    = lcOpTitle
*---Report Type Summary/Detail
DO CASE
   CASE  lcRpOut= 'S'
     XPRINT  ='S'     
   CASE  lcRpOut= 'D'
     XPRINT  ='D'     
ENDCASE
*--- Po's Status
DO CASE
  CASE  lcRpStatus= 'O'
    XSTATUS = 'O'
  CASE  lcRpStatus= 'C'    
    XSTATUS = 'C'    
  CASE  lcRpStatus= 'X'
    XSTATUS = 'X'
  CASE  lcRpStatus= 'H'
    XSTATUS = 'H'
ENDCASE
IF !EMPTY (laRpTarget)
  IF ASCAN(laRpTarget,"Ordered") <> 0
    XREP = XREP + "O"
  ENDIF
  IF ASCAN(laRpTarget,"Receipt") <> 0
    XREP = XREP + "R"
  ENDIF
  IF ASCAN(laRpTarget,"Shipment") <> 0
    XREP = XREP + "S"
  ENDIF      
ENDIF

FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'POSLN.PO' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      LPO = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,6))
      HPO = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],8,6))
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'POSHDR.ENTERED' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      LENTERED = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      HENTERED = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'POSHDR.COMPLETE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      LCANCELD = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      HCANCELD = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'POSHDR.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      XDIV = SUBSTR(laOgFxFlt[lnInd,6],1,6)
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CSTYGROUP' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      XGRP = SUBSTR(laOgFxFlt[lnInd,6],1,6)
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.STYLE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      LSTYLE = SUBSTR(laOgFxFlt[lnInd,6],1,LEN(lnMajPict))
      HSTYLE = SUBSTR(laOgFxFlt[lnInd,6],LEN(lnMajPict)+2,LEN(lnMajPict))
  ENDCASE
ENDFOR
*-- END OF lfUpdFltVar

*!*************************************************************
*! Name      : lfMajGet
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : To get the title and picture of style major segement 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajInfGet()
*!*************************************************************
*
FUNCTION lfMajGet

lcMajPic = "@! " + gfItemMask("PM")
lcMajTtl = gfItemMask("HM")
*-- END OF lfMajGet

*!*************************************************************
*! Name      : lfvStyle
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
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
*-- END OF lfvStyle

*!*************************************************************
*! Name      : lfWoldVal
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : To return the old value.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfWoldVal()
*!*************************************************************
FUNCTION lfWoldVal

laOldVal = EVALUATE(SYS(18))
*-- END OF lfWoldVal

*!*************************************************************
*! Name      : lfEvalSegs (C#101630)
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

lnMajSeg   = gfItemMask('SM')  && No. of major segments.
lcMajPict  = gfItemMask("PM")
*-- Compute Free/Color Items in Style code Structure. [Begin]
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
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style code Structure. [End]
lcStyGrp = lcStyMajor + ' Group' 
RETURN ''
*-- END OF  lfEvalSegs

*!**************************************************************************
*! Name      : lfCollTime
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/17/1999
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!**************************************************************************
*! Returns            : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
*
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- END OF lfCollTime.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/15/1999 
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
*
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(WORKTEMP),WORKTEMP,0)
ERASE &gcWorkDir.&WORKTEMP+'.DBF'
ERASE &gcWorkDir.&WORKTEMP+'.CDX'

USE IN IIF(USED(POLTEMP),POLTEMP,0)
ERASE &gcWorkDir.&POLTEMP+'.DBF'
ERASE &gcWorkDir.&POLTEMP+'.CDX'
*-- END OF lfClearRep.

*!*************************************************************
*! Name      : lfUserPriv
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/19/1999 
*! Purpose   : Functio to chick user prev.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUserPriv()
*!*************************************************************
FUNCTION lfUserPriv
PARAMETERS lcModule,lcProcess,lcSubProc

IF EMPTY(lcModule) OR EMPTY(lcProcess) OR PARAMETERS()<2
  RETURN .F.
ENDIF
IF !glLog_Requ
  RETURN .T.
ENDIF
PRIVATE lcOldAlias,llRetrun,laUsrSubP
lcOldAlias = SELECT()
SELECT SYUUSRPR
llRetrun = SEEK(ALLTRIM(gcUser_ID)+UPPER(lcModule+SYCCOMP.cComp_Id+lcProcess))
IF !EMPTY(mSubProc) AND TYPE('lcSubProc')='C'
   DIMENSION laUsrSubP[1,2]
   =gfSubStr(mSubProc,@laUsrSubP,"|")
   llRetrun = ASCAN(laUsrSubP,UPPER(lcSubProc))>0
ENDIF
SELECT (lcOldAlias)
RETURN llRetrun
*-- END OF lfUserPrivN

*!*************************************************************
*! Name      : lfFltChng
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/19/1999 
*! Purpose   : Function to chick If the filter change.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFltChng()
*!*************************************************************
*
FUNCTION lfFltChng

*---laRpTarget
*---laRpCopyTg
IF ALEN(laRpTarget) <> ALEN(laRpCopyTg) ;
   OR ALEN(laRpComTrg) <> ALEN(laRpCComTg)
  RETURN .T.
ENDIF
FOR lnIndex = 1 TO ALEN(laRpTarget)
  IF ASCAN(laRpCopyTg,laRpTarget[lnIndex]) = 0
    RETURN .T.
  ENDIF
ENDFOR

*---laRpComTrg
*---laRpCComTg
FOR lnIndex = 1 TO ALEN(laRpComTrg)
  IF ASCAN(laRpCopyTg,laRpCComTg[lnIndex]) = 0
    RETURN .T.
  ENDIF
ENDFOR
RETURN .F.
*-- END OF lfFltChng

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 08/19/1999 
*! Purpose   : Option Grid When Function.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*
FUNCTION lfwRepWhen

R_WIDTH  = 'W'    && STANDARD REPORT IS 'WIDE'
*-- END OF lfwRepWhen.

*!*************************************************************