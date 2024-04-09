*:***************************************************************************
*: Program file  : POSCSCOL
*: Program desc. : Style Cost Sheet Report for Collage
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Order (PO )
*: Developer     : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:    Procedures : ENDREPORT
*:    Functions  : gfModalGen,gfCodDes,gfGetMemVar,gfUserPriv,gfItemMask,
*:               : lfwRepWhen,lfEvalSegs,lfNMajType,lfSRVSty,lfStySum,lfSRVFab,
*:               : lfFabSum,lfRecreate,lfClearRep,lfChanged,lfFillForm,
*:               : lfFormDefa 
*:    Procedure  : lpStyClr,lpStyOnly,lpSCPrn,lpCPrn
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO POSCSCOL
*:***************************************************************************
*: This Report Program is due to C200327,1 ...
*****************************************************************************
*

**-- define Variables --**
STORE SPACE(0) TO lnECost,lcCurCod
STORE 0 TO lnETotCost,lnLinTot,lnLenVal
STORE .F. TO llMulCurr

**-- open needed files --**
=gfOpenFile(gcSysHome+'SYCCURR','Ccurrcode','SH')

**-- chick if use multi currency --**
llMulCurr  = gfGetMemVar('llMulCurr')
llMScale = gfGetMemVar('M_USEEXSSC')
lnMajorLen = LEN(gfItemMask("PM"))

*-- if first time you (preview or run ) or user change critria.

IF llClearFn OR llOGFltCh
  lcRpExp = [STYLE = '' AND SEEK(CSTYMAJOR,'BOM') AND ] + lcRpExp
  llClearFn  = .F.  && do clear function flag
  llChStyClr = .F.  && change color/style or color selection flag
  llChFabric = .F.  && change fabric filter flag.
  llChStyle  = .F.  && change style filter flag.
  
  IF RECCOUNT(WORKFILE) > 0 
    USE IN (WORKFILE)
    = lfRecreate()
  ENDIF

  SELECT (WORKFILE)
  IF lcRpStyClr='C'
    INDEX ON cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + Item + IClr TAG &WORKFILE
    DO lpStyClr           && PRINT BY COLOR

  ELSE
    INDEX ON cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + Item + IClr TAG &WORKFILE
    DO lpStyOnly          && PRINT FOR ALL COLORS
  ENDIF           

ENDIF  && end if first time you (preview or run ) or user change critria.
  
IF RECCOUNT(WORKFILE) = 0
  =gfModalGen(.f.,.f.,.f.,.f.,'There are no records to display...! ')  
  RETURN
ENDIF

CLEAR TYPEAHEAD
SET DEVICE TO PRINT

SELECT (WORKFILE)
GO TOP

*-- call Print functions.
IF lcRpStyClr='C'
  DO lpSCPrn           && PRINT BY COLOR
ELSE
  DO lpCPrn            && PRINT FOR ALL COLORS
ENDIF

DO ENDREPORT
SET DEVICE TO SCREEN
*-- end of report code.

*----------- Procedures and Functions section -----------------
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lpStyClr
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/08/2002
*! Purpose   : Collect report data in style/color case
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpStyClr
*!*************************************************************
PROCEDURE lpStyClr
SELECT STYLE
*-- Scan all selected styles.
SCAN FOR &lcRpExp
  XSTYLE  = cStyMajor
  XSTYCLR = SUBSTR(STYLE.STYLE,lnMajorLen+1)
  SELECT BOM
  = SEEK(XSTYLE)

  *-- scan style in BOM file
  SCAN REST WHILE (cItmMajor = XSTYLE) FOR LIKE(STRTRAN(cItmMask,'*','?') , Style.Style)
    XUNTCOST=0
    DO CASE
      CASE cCatgTyp $ "PMD"
      
        SCATTER MEMVAR MEMO
        m.SClr       = XSTYCLR
        m.cSeekStyle = STYLE.STYLE
          
        INSERT INTO (WORKFILE) FROM MEMVAR

      OTHERWISE
        
        SCATTER MEMVAR MEMO
        IF cCatgTyp = "F" .OR. (cCatgTyp = "T" .AND. Trim_Invt)
          XKEY= PADR(ITEM,7) + IIF("*" $ ICLR , IIF(lcFree_Clr = 'C',SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),ICLR) , ICLR)
          IF SEEK(XKEY,'FABRIC')
            XFABDESC= FABRIC.DESC
          ELSE
            XUNTCOST=0
            XFABDESC=SPACE(0)
          ENDIF

            m.DESC     = IIF(ICLR = '******',XFABDESC,m.DESC)
        ENDIF
        m.cSeekStyle = STYLE.STYLE
        m.SClr     = XSTYCLR
        INSERT INTO (WORKFILE) FROM MEMVAR

    ENDCASE

  ENDSCAN  && end scan style in BOM file

ENDSCAN    && end Scan all selected styles.
*-- end of lpStyClr.

*!*************************************************************
*! Name      : lpStyOnly
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Collect report data in style only case
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpStyOnly
*!*************************************************************
PROCEDURE lpStyOnly
lcOldMajor = ''
SELECT STYLE
*-- Scan all selected styles.
SCAN FOR &lcRpExp

  SELECT BOM
  *-- scan style in BOM file
  SCAN FOR cItmMajor = STYLE.cStyMajor
    IF (lcOldMajor # STYLE.cStyMajor) OR ;
       (cCatgTyp $ 'FS' AND '******' $ cItmMask)
      SCATTER MEMVAR MEMO
      m.cSeekStyle = IIF('******' $ cItmMask , STYLE.STYLE , cItmMask)
      m.SClr       = SUBSTR(m.cSeekStyle,lnMajorLen+1) 
      IF EMPTY(m.ICLR) AND (cCatgTyp = 'S')
        m.IClr = SUBSTR(m.ITEM,lnNonMajSt,lnColorLen)
      ENDIF
      INSERT INTO (WORKFILE) FROM MEMVAR
    ENDIF
  ENDSCAN  && end scan style in BOM file

  lcOldMajor = STYLE.cStyMajor
  SELECT STYLE
  lcStyMaj = SUBSTR(STYLE,1,lnMajorLen)
  LOCATE FOR SUBSTR(STYLE,1,lnMajorLen) > lcStyMaj
ENDSCAN  && end Scan all selected styles.

*-- end of lpStyOnly.

*!*************************************************************
*! Name      : lpSCPrn
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Print report data in style/color case
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpSCPrn
*!*************************************************************
PROCEDURE lpSCPrn
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
* STYLE               ITEM   MFG               UOM      UNIT     UNIT       TOTAL
* COLOR  ITEM         COLOR  CODE DESCRIPTION  USE  QUANTITY     COST        COST
* XXXXXX XXXXXXXXXXXX XXXXXX  XX  XXXXXXXXXXXX XXX   999.999  999.999   999999.99
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
BREAK  = 'CITMMAJOR + SCLR'
PAGENO = 0
ROW    = 99
XTIME  = TIME()
XREPORT='MFCSREP'
R_TITLE ='Style Cost Sheet Report'
lnCstBySiz = 0
lnSCnt    = 8
lnBomSzs  = 8
lnUntCost = 0
lnTotCost = 0
STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5
XNEW_STY=.T.            && FLAG THAT TELLS IF NEW STYLE OR NOT
XPRV_TYP = TYP
HBREAK=SPACE(1)
lcSayStyle = cSeekStyle
IF LEN(TRIM(BREAK)) <>0
  HBREAK = &BREAK
ENDIF

*---------------------------------------------------------
* [REPORT] LOOP
*---------------------------------------------------------
DO WHILE .T.

  IF ROW >=55
    PAGENO = PAGENO+1
    
    IF llMulCurr
      DO lfPrHeader WITH PAGENO
    ELSE
      DO RPT_HDR WITH XREPORT,'',R_WIDTH
    ENDIF  
    
    IF llMulCurr
      @ 05,00 SAY '              ITEM   MFG                    UOM    UNIT        UNIT        TOTAL           TOTAL'
      @ 06,00 SAY ' ITEM         COLOR  CODE   DESCRIPTION     USE QUANTIT        COST         COST           EQUV.'
    ELSE
      @ 05,00 SAY '              ITEM   MFG                    UOM    UNIT        UNIT        TOTAL'
      @ 06,00 SAY ' ITEM         COLOR  CODE   DESCRIPTION     USE QUANTIT        COST         COST'
    ENDIF
    
    IF llMulCurr
      @ 07,00 SAY REPLICATE('=',96)
    ELSE
      @ 07,00 SAY REPLICATE('=',80)
    ENDIF  
    ROW=08
  ENDIF
  

  DO WHILE .NOT. EMPTY(BREAK)
    IF &BREAK = HBREAK
      EXIT
    ENDIF
  
    IF llMulCurr
      @ ROW,00 SAY REPLICATE('=',96)
    ELSE
      @ ROW,00 SAY REPLICATE('=',80)
    ENDIF  

    *- If user can access cost.
    IF qCostPrv   && COSTING ACCESS
      ROW = ROW+1
      @ ROW,025 SAY '**** TOTAL COST FOR  : ' + PADR(lcSayStyle,19)  
      XSTYCST=XTOT1+XTOT2+XTOT3+XTOT4+XTOT5
      
      IF llMulCurr
        @ ROW,084 SAY lnETotCost PICTURE '999999999.99'
      ELSE
        @ ROW,068 SAY lnCstBySiz PICTURE '999999999.99'
      ENDIF
      ROW=ROW+1
      @ ROW,000 SAY 'SELLING PRICE :'      
      @ ROW,14 SAY XPRICEA PICTURE '999999999.99'
      *-- Fix bug of price is get out from the report lyout
      @ ROW,026 SAY '** GROSS PROFIT FOR: ' + PADR(lcSayStyle,19)
      IF llMulCurr
		@ ROW,084 SAY XPRICEA - lnETotCost PICTURE '999999999.99'
		lnETotCost = 0 
		lnLinTot = 0 
      ELSE
        @ ROW,68 SAY XPRICEA - XSTYCST PICTURE '999999999.99'
      ENDIF
      
      ROW=ROW+1 
	  IF llMulCurr
	    @ ROW,00 SAY REPLICATE('=',96)
	  ELSE
	    @ ROW,00 SAY REPLICATE('=',80)
	  ENDIF  
    ENDIF

    ROW=ROW+1

    lnSCnt   = 8
    lnBomSzs = 8
    lnUntCost = 0
    lnTotCost = 0
    
    STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5

    XNEW_STY=.T.

    HBREAK = &BREAK
    lcSayStyle = cSeekStyle
    EXIT
  ENDDO

  IF EOF()
    EXIT
  ENDIF

  IF XNEW_STY
    XNEW_STY=.F.
    
    @ ROW,000 SAY PADR(lcStyTitle,19) + '->' + PADR(cSeekStyle,19) + '  '
    IF SEEK(cSeekStyle,'STYLE')
      XSTYDESC = STYLE.DESC1
      XPRICEA  = STYLE.PRICEA
    ELSE
      XSTYDESC=SPACE(0)
      XPRICEA =0
    ENDIF

    @ ROW,42 SAY PADR(XSTYDESC,38)
    ROW=ROW+1

    IF llMulCurr
      lnLenVal = 96
    ELSE
      lnLenVal = 80
    ENDIF
    
    DO CASE
      CASE Typ = "1"
        XHDR = '< '+lcSlbl1+' >'
         lnLen=INT(((lnLenVal-LEN(XHDR))/2))
         @ ROW,00 SAY REPLICATE('-',lnLen)
         @ ROW,lnLen+1 SAY XHDR
		 @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen)) 	 
      CASE Typ = "2"
        XHDR='< '+lcSlbl2+' >'
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
		@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
      CASE Typ = "3"
        XHDR='< '+lcSlbl3+' >'
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
		@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
      CASE Typ = "4"
        XHDR='< '+lcSlbl4+' >'
		lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
		@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
      CASE Typ = "5"
        XHDR='< '+lcSlbl5+' >'
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
		@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
      ENDCASE

    ROW=ROW+1
  ENDIF
  
  IF llMulCurr
    DO CASE
      CASE cCatgTyp = "P"
        lcCurCod = Style.CPriceCur
      CASE cCatgTyp $ "FTS"
        lcCurCod = gcBaseCurr
      OTHERWISE     
        lcCurCod = Style.CDutyCur      
    ENDCASE 
  ENDIF
  
  @ ROW,01 SAY IIF(CCATGTYP = 'F',PADR(ITEM,7),PADR(ITEM,lnMajorLen))
  @ ROW,14 SAY ICLR
  
  IF cCatgTyp = "M"
    @ ROW,21 SAY PADR(MFGCODE,6)
  ENDIF
  @ ROW,28 SAY SUBSTR(DESC,1,15)
  @ ROW,44 SAY UOM
  @ ROW,48 SAY nBomTOTQTY PICTURE '999.999'
  Z = Typ
  =lfCostBySz()

  *- if user can access cost
  IF qCostPrv         && COSTING ACCESS
    @ ROW,56 SAY lnUntCost PICTURE '9999999.999'
    @ ROW,68 SAY lnTotCost PICTURE '999999999.99'
    IF llMulCurr
      @ ROW,80 SAY IIF(SEEK(lcCurCod,'SYCCURR'),SYCCURR.cCurrSmbl,'')
   	  lnECost = lfAmntDisp(lnTotCost,gdSysDate,lcCurCod)
      @ ROW,84 SAY lnECost PICTURE '999999999.99'
      lnETotCost = lnETotCost + lnECost
      lnLinTot = lnLinTot + lnECost
    ENDIF
    
  ENDIF
  ROW=ROW+1
  Z = Typ

  SELE &WORKFILE
  SKIP
  IF XPRV_TYP<>TYP

    IF qCostPrv          && COSTING ACCESS

      DO CASE
        CASE XPRV_TYP='1'
          @ ROW,027 SAY ' '+lcSlbl1+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0 
          ELSE
            @ ROW,067 SAY XTOT1 PICTURE '9999999999.99'          
          ENDIF       
        CASE XPRV_TYP='2'
          @ ROW,027 SAY ' '+lcSlbl2+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0 
          ELSE
            @ ROW,067 SAY XTOT2 PICTURE '9999999999.99'
          ENDIF
        CASE XPRV_TYP='3'
          @ ROW,027 SAY ' '+lcSlbl3+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0 
          ELSE
            @ ROW,067 SAY XTOT3 PICTURE '9999999999.99'
          ENDIF
        CASE XPRV_TYP='4'
          @ ROW,027 SAY ' '+lcSlbl4+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0           
          ELSE  
            @ ROW,067 SAY XTOT4 PICTURE '9999999999.99'
          ENDIF
        CASE xPrv_Typ = "5"
          @ ROW,027 SAY ' '+lcSlbl5+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0          
          ELSE
            @ ROW,067 SAY XTOT5 PICTURE '9999999999.99' 
          ENDIF        
      ENDCASE
    ENDIF

    XPRV_TYP=TYP
    ROW=ROW+1
    IF &BREAK=HBREAK
      
      IF llMulCurr
        lnLenVal = 96
      ELSE
        lnLenVal = 80
      ENDIF
      
      DO CASE
        CASE Typ = "1"
          XHDR='< '+lcSlbl1+' >'
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        CASE Typ = "2"
          XHDR='< '+lcSlbl2+' >'
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        CASE Typ = "3"
          XHDR='< '+lcSlbl3+' >'
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        CASE Typ = "4"
          XHDR='< '+lcSlbl4+' >'
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        CASE Typ = "5"
          XHDR='< '+lcSlbl5+' >'
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))    
      ENDCASE  
      
      ROW=ROW+1
    ENDIF
  ENDIF
ENDDO
*-- end of lpSCPrn

*!*************************************************************
*! Name      : lpCPrn
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Print report data in style only case
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCPrn
*!*************************************************************
PROCEDURE lpCPrn
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
*   STYLE: 123456789012 12345678901234567890      STATUS: 123456
*
*   SEASON..: 12 1234567890   DIVISION: XX XXXXXXXXXX   GROUP : XX XXXXXXXXXX
*   PRICEA  : 9999.99         PRICEB  : 9999.99         PRICEC: 9999.99
*   SIZE (X): 111 222 333 444 555 666 777 888
*   PREPK(X): 111 222 333 444 555 666 777 888
*   COLORS  : 111111 XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX
*             XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                      ITEM  MFG                UOM     UNIT     UNIT       TOTAL
*      ITEM            COLOR CODE DESCRIPTION   USE QUANTITY     COST        COST
*      XXXXXXX  XXX   XX  XXXXXXXXXXXXXXXXXXXX  XXX  999.999  999.999   999999.99
*


*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE         ITEM  MFG                        UOM      UNIT     UNIT       TOTAL
*COLOR ITEM    COLOR CODE ....DESCRIPTION.....  USE  QUANTITY     COST        COST
* XXX  XXXXXXX  XXX   XX  XXXXXXXXXXXXXXXXXXXX  XXX   999.999  999.999   999999.99
*               XXX-XXX XXX-XXX XXX-XXX XXX-XXX XXX-XXX XXX-XXX XXX-XXX XXX-XXX
*

BREAK  = 'CITMMAJOR'
PAGENO = 0
ROW    = 99
XTIME  = TIME()
XREPORT='MFCSREP'
R_TITLE ='Style Cost Sheet Report'

lnCstBySiz = 0
lnSCnt     = 8
lnBomSzs   = 8
lnUntCost  = 0
lnTotCost  = 0

STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5

XNEW_STY=.T.            && FLAG THAT TELLS IF NEW STYLE OR NOT
XPRV_TYP = TYP

HBREAK=SPACE(1)
IF LEN(TRIM(BREAK)) <>0
   HBREAK = &BREAK
ENDIF

*---------------------------------------------------------
* [REPORT] LOOP
*---------------------------------------------------------
DO WHILE .T.

  IF ROW >=53
    PAGENO = PAGENO+1
    IF llMulCurr
      DO lfPrHeader WITH PAGENO
    ELSE
      DO RPT_HDR WITH XREPORT,'',R_WIDTH
    ENDIF  
    IF llMulCurr
      @ 05,00 SAY '                      ITEM  MFG              UOM    UNIT        UNIT       TOTAL           TOTAL'
      @ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION USE QUANTIT        COST        COST           EQUV.'  
    ELSE
      @ 05,00 SAY '                      ITEM  MFG              UOM    UNIT        UNIT       TOTAL'
      @ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION USE QUANTIT        COST        COST'
    ENDIF
    IF llMulCurr
      @ 07,00 SAY REPLICATE('=',96)
    ELSE
      @ 07,00 SAY REPLICATE('=',80)
    ENDIF  
    ROW=08
  ENDIF

  DO WHILE .NOT. EMPTY(BREAK)
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    IF llMulCurr
      @ ROW,00 SAY REPLICATE('=',96)
    ELSE
      @ ROW,00 SAY REPLICATE('=',80)
    ENDIF     
    ROW = ROW+1
    @ ROW,022 SAY '**** TOTAL COST FOR : '+SUBSTR(HBREAK,1,12)+'  '+SUBSTR(HBREAK,13,6)
    XSTYCST=XTOT1+XTOT2+XTOT3+XTOT4+XTOT5

    IF qCostPrv         && COSTING ACCESS
      IF llMulCurr
   		@ ROW,084 SAY lnETotCost PICTURE '999999999.99'
   		lnETotCost = 0 
		lnLinTot = 0 
      ELSE
        @ ROW,068 SAY lnCstBySiz PICTURE '999999999.99'
      ENDIF     
    ENDIF
    ROW=ROW+1
    IF llMulCurr
      @ ROW,00 SAY REPLICATE('=',96)
    ELSE
      @ ROW,00 SAY REPLICATE('=',80)
    ENDIF   
    ROW=ROW+1

    lnCstBySiz = 0
    
    lnSCnt   = 8
    lnBomSzs = 8
    lnUntCost = 0
    lnTotCost = 0
    
    STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5

    XNEW_STY=.T.

    HBREAK = &BREAK
    EXIT
  ENDDO

  IF EOF()
    EXIT
  ENDIF

  IF XNEW_STY
    XNEW_STY=.F.
    XSTYLE = ALLTRIM(&WORKFILE..cItmMajor)
    @ ROW,000 SAY ALLTRIM(lcStyMajor) + '->' + XSTYLE
    
    = SEEK (cSeekStyle,'STYLE')
    
    @ ROW,039 SAY PADR(STYLE.DESC1,41)
    ROW=ROW+2
    *------- NOW DISPLAY ALL STYLE INFO ----*
    XSEAS_DATA = IIF(STYLE.SEASON = 'Y     ','YEAR ROUND',gfCodDes(STYLE.SEASON , 'SEASON'))    && Get Season from codes file. 
    XDIVI_DATA = gfCodDes(STYLE.CDIVISION , 'CDIVISION')    && Get Division from codes file.  
    XGROU_DATA = gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP')    && Get Style group from codes file.  
    @ ROW,02 SAY 'SEASON.:'
    @ ROW,09 SAY PADR(XSEAS_DATA,17)
    @ ROW,27 SAY 'DIVISON:'
    @ ROW,35 SAY PADR(XDIVI_DATA,17)
    @ ROW,59 SAY 'GROUP.:'
    @ ROW,66 SAY SUBSTR(XGROU_DATA,1,14)

    ROW = ROW + 1
    
    @ ROW,02 SAY 'PRICEA :'
    @ ROW,14 SAY STYLE.PRICEA PICTURE '999999999.99'
    @ ROW,27 SAY 'PRICEB :'
    @ ROW,40 SAY STYLE.PRICEB PICTURE '999999999.99'
    @ ROW,59 SAY 'PRICEC :'
    @ ROW,71 SAY STYLE.PRICEC PICTURE '999999999.99'
    ROW = ROW + 1

    IF qCostPrv         && COSTING ACCESS
      @ ROW,02 SAY 'PROFITA:'
      @ ROW,10 SAY STYLE.PRICEA - STYLE.TOTCOST PICTURE '9999999999.99'
      @ ROW,27 SAY 'PROFITB:'
      @ ROW,36 SAY STYLE.PRICEB - STYLE.TOTCOST PICTURE '9999999999.99'
      @ ROW,59 SAY 'PROFITC:'
      @ ROW,67 SAY STYLE.PRICEC - STYLE.TOTCOST PICTURE '9999999999.99'
      ROW=ROW+1
    ENDIF
    @ ROW,02 SAY 'SIZE (' + STYLE.SCALE + ')'
    = SEEK('S' + STYLE.SCALE,'SCALE')    
    COL = 12
    FOR lnI = 1 TO 8 
      lcSc = STR(lnI,1)
      @ ROW,COL SAY PADL(ALLTRIM(SCALE.SZ&lcSc),5) PICTURE '@Z XXXXX' 
      COL = COL + 6
    ENDFOR
    @ ROW,57 SAY 'PATTERN:'
    @ ROW,67 SAY ALLTRIM(STYLE.PATTERN)
    ROW = ROW + 1
    @ ROW,02 SAY 'PREPK(' + STYLE.PREPAK + '):'
    IF !EMPTY(STYLE.PREPAK)
      = SEEK('P' + STYLE.SCALE + STYLE.PREPAK ,'SCALE')
      STORE 0 TO XPREPAK
      X   = 1
      COL = 14
      DO WHILE X<=8
        Y=STR(X,1)
        @ ROW,COL SAY SCALE.PP&Y PICTURE '@Z 999'
        X = X+1
        COL = COL + 6
      ENDDO
    ENDIF
    COL=12
    ROW=ROW+1
    IF ROW <= 53
      @ ROW,02 SAY 'COLORS  :'
    ENDIF
    
    DO WHILE ALLTRIM(STYLE.CSTYMAJOR) == XSTYLE
      IF EOF()
        EXIT
      ENDIF
      IF ROW > 53
        PAGENO = PAGENO+1
        DO RPT_HDR WITH XREPORT,'',R_WIDTH
        @ 05,00 SAY '                      ITEM  MFG              UOM    UNIT        UNIT       TOTAL'
        @ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION USE QUANTIT        COST        COST'
        @ 07,00 SAY REPLICATE('=',80)
        ROW=08
        @ ROW,04 SAY 'COLORS  :'
      ENDIF
      
      @ ROW,COL SAY IIF(lcFree_Clr = 'C',;
              SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'******')
      
      SKIP IN STYLE
      ROW = IIF( COL >= 60 , ROW + 1  , ROW )
      COL = IIF( COL <= 60 , COL + 7 , 14 )
      IF STYLE.STYLE<> XSTYLE .AND. COL = 14
        ROW = ROW - 1
      ENDIF
    ENDDO

    ROW=ROW+1
    
    IF llMulCurr
      lnLenVal = 96
    ELSE
      lnLenVal = 80
    ENDIF
    DO CASE
      CASE Typ = "1"
        XHDR='< '+lcSlbl1+' >'
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
      CASE Typ = "2"
        XHDR='< '+lcSlbl2+' >'
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
      CASE Typ = "3"
        XHDR='< '+lcSlbl3+' >'
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
      CASE Typ = "4"
        XHDR='< '+lcSlbl4+' >'
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
      CASE Typ = "5"
        XHDR='< '+lcSlbl5+' >'
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
    ENDCASE

    ROW=ROW+1
  ENDIF         && END OF NEW STYLE
  
  IF llMulCurr
    = SEEK(cSeekStyle,'STYLE')
    DO CASE
      CASE cCatgTyp = "P"
        lcCurCod = Style.CPriceCur
      CASE cCatgTyp $ "FTS"
        lcCurCod = gcBaseCurr
      OTHERWISE     
       lcCurCod = Style.CDutyCur      
    ENDCASE 
  ENDIF
  @ ROW,01 SAY IIF(CCATGTYP = 'F',PADR(ITEM,7),PADR(ITEM,lnMajorLen))
  IF SCLR = '******'
    @ ROW,17 SAY IIF(ICLR # '******',ICLR,;
                 IIF(lcFree_Clr = 'C',;
                 SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'******'))
                 
  ENDIF
  IF SUBSTR(cItmMask,lnNonMajSt,lnColorLen) = '******'
    @ ROW,17 SAY ICLR
  ENDIF
  
  IF cCatgTyp = "M"
    @ ROW,24 SAY PADR(MFGCODE,6)
  ENDIF
  
  @ ROW,33 SAY SUBSTR(DESC,1,11)
  @ ROW,45 SAY UOM
  @ ROW,49 SAY nBomTOTQTY PICTURE '999.999'
  Z = Typ
  =lfCostBySz()
  IF qCostPrv             && COSTING ACCESS
    @ ROW,57 SAY lnUntCost PICTURE '9999999.999'
    @ ROW,69 SAY lnTotCost PICTURE '99999999.99'
    IF llMulCurr
	  lnECost = lfAmntDisp(lnTotCost,gdSysDate,lcCurCod)
      @ ROW,80 SAY IIF(SEEK(lcCurCod,'SYCCURR'),SYCCURR.cCurrSmbl,'')
	  lnECost = lfAmntDisp(lnTotCost,gdSysDate,lcCurCod)
      @ ROW,84 SAY lnECost PICTURE '999999999.99'
      lnETotCost = lnETotCost + lnECost
      lnLinTot = lnLinTot + lnECost
    ENDIF
  ENDIF
  ROW=ROW+1

  Z = Typ
  
  *---- NOW DISPLAY THE FABRIC AND STYLE COLOR ASSIGNEMENTS ----*
  IF SUBSTR(cItmMask,lnNonMajSt,lnColorLen) <>'******' .AND. cCatgTyp $ "FS"
    COL = 15
    @ ROW,06 SAY 'COLORS  :'
    @ ROW,COL SAY SCLR
    @ ROW,COL+6 SAY '-'+ ICLR
    ROW=ROW+1
  ENDIF
  IF .NOT. EOF() .AND. (XPRV_TYP=TYP)
    SKIP IN (WORKFILE)
  ENDIF
  IF XPRV_TYP<>TYP

    IF qCostPrv           && COSTING ACCES
      DO CASE
        CASE XPRV_TYP='1'
          @ ROW,027 SAY ' '+lcSlbl1+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0 
          ELSE
            @ ROW,067 SAY XTOT1 PICTURE '9999999999.99'
          ENDIF
        CASE XPRV_TYP='2'
          @ ROW,027 SAY ' '+lcSlbl2+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0          
          ELSE
            @ ROW,067 SAY XTOT2 PICTURE '9999999999.99'
          ENDIF
        CASE XPRV_TYP='3'
          @ ROW,027 SAY ' '+lcSlbl3+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0                   
          ELSE
            @ ROW,067 SAY XTOT3 PICTURE '9999999999.99'
          ENDIF
        CASE XPRV_TYP='4'
          @ ROW,027 SAY ' '+lcSlbl4+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0                            
          ELSE
            @ ROW,067 SAY XTOT4 PICTURE '9999999999.99'
          ENDIF
        CASE xPrv_Typ = "5"
          @ ROW,027 SAY ' '+lcSlbl5+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0                   
          ELSE
            @ ROW,067 SAY XTOT5 PICTURE '9999999999.99'
          ENDIF
      ENDCASE
    ENDIF
    XPRV_TYP=TYP
    ROW=ROW+1
    IF &BREAK=HBREAK

      IF llMulCurr
        lnLenVal = 96
      ELSE
        lnLenVal = 80
      ENDIF
      DO CASE
        CASE Typ = "1"
          XHDR='< '+lcSlbl1+' >'
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        CASE Typ = "2"
          XHDR='< '+lcSlbl2+' >'
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        CASE Typ = "3"
          XHDR='< '+lcSlbl3+' >'
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        CASE Typ = "4"
          XHDR='< '+lcSlbl4+' >'
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        CASE Typ = "5"
          XHDR='< '+lcSlbl5+' >'
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
      ENDCASE

      ROW=ROW+1
    ENDIF
  ENDIF
ENDDO
*-- end of lpCPrn.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfAdrShift
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
IF EMPTY(lcSlbl1)

  IF lcFree_Clr # 'C'
    laOGObjCnt[1] = .F.    && Disble if there is no color segment.
    = lfOGShowGet('LCRPSTYCLR')  && Called to show object get.
  ENDIF

  XAVG_COST = (gfGetMemVar('M_COST_MET') = 'A')
  qCostPrv = gfUserPriv('IC','ICSTYLE','COSTING')

  SET ORDER TO STYLE IN STYLE        && To use it to validate STYLE   # in option grid.
  SET ORDER TO CODES IN CODES        && To use it to validate STYLE   # in option grid.
  SET ORDER TO SCALE IN SCALE        && To use it to validate STYLE   # in option grid.
  SET ORDER TO BOM IN BOM            && To use it to validate STYLE   # in option grid.

  IF 'MA' $ gcComp_Mdl
    SET ORDER TO FABRIC IN FABRIC      && To use it to validate STYLE   # in option grid.
  ENDIF  

  DIMENSION laTempStru[1,4]
  SELECT BOM
  = AFIELDS(laTempStru)
  DIMENSION laTempStru[ALEN(laTempStru,1) + 2, 4]

  *-- cSeekStyle     :  field save style to seek it in style file.
  laTempStru[ALEN(laTempStru,1) - 1 ,1] = 'cSeekStyle'
  laTempStru[ALEN(laTempStru,1) - 1 ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) - 1 ,3] = 19
  laTempStru[ALEN(laTempStru,1) - 1 ,4] = 0

  *-- SCLR     :  field save style color
  laTempStru[ALEN(laTempStru,1)  ,1] = 'SCLR'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)  ,3] = 19
  laTempStru[ALEN(laTempStru,1)  ,4] = 0
  *-- Create temporary file that holding order line data. [end]

  = lfRecreate()

  lcStyleTyp  = "I"
  FOR lnCount = 1 TO 5
    lcCount  = STR(lnCount,1)
    lcSlbl&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'SLBL'+lcCount)
  ENDFOR 

ENDIF  
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
  
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfNMajType
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Mask NonMajor segments .
*!*************************************************************
*! Called from : lfEvalSegs.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNMajType()
*!*************************************************************
FUNCTION lfNMajType
PARAMETERS lcNMajType,lnMajSegs

*-- Loop Around Non Major elements.
FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)

  IF laMajSegs[lnI,1] = lcNMajType

    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
    lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
                     lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

    lcNonMajTl = IIF(EMPTY(lcNonMajTl),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                     lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)
*-- end of lfNMajType. 

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
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
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR Fabric = Fabric.Fabric AND ;
             !MAKE AND lDetCost AND STATUS = 'A'
    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- !   No active imported styles use detail costing in fabric group XXX .
      *--                  <Ok>
      = gfModalGen("INM34122B36000","Dialog",ALLTRIM(Fabric.Fabric))
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfRecreate
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
FUNCTION lfRecreate
CREATE CURSOR (WORKFILE) ;
   FROM ARRAY laTempStru
*-- end of lfRecreate.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
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

IF USED(WORKFILE)
 USE IN (WORKFILE)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfChanged
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
FUNCTION lfChanged
llChStyClr = .T.
*-- END OF lfChanged.

*!*************************************************************
*! Name      : lfFillForm
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Fill report format arrays called from dummy variable in rep. gen.
*!*************************************************************
*! Called from : Dummy variable in Syrepuvr.dbf file
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillForm()
*!*************************************************************
FUNCTION lfFillForm
IF lcFree_Clr = 'C'
  DIMENSION laFormDesc[2,1] , laFormVal[2,1]
  laFormDesc[1,1] = lcStyMajor + ' / ' + lcNonMajTl
  laFormDesc[2,1] = lcStyMajor + ' only...'

  laFormVal[1,1]  = 'C'
  laFormVal[2,1]  = 'S'
ELSE
  DIMENSION laFormDesc[1,1] , laFormVal[1,1]
  laFormDesc[1,1] = lcStyMajor + ' only...'

  laFormVal[1,1]  = 'S'
ENDIF
*-- end of lfFillForm.

*!*************************************************************
*! Name      : lfFormDefa
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
FUNCTION lfFormDefa
RETURN IIF(lcFree_Clr = 'C','C','S')
*-- end of lfFormDefa.

*!*************************************************************
*! Name      : lfCostBySz
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Calculate cost for each cost element y size.
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCostBySz()
*!*************************************************************
*
FUNCTION lfCostBySz
PRIVATE lcCurAlias , lnStyRec , lnSclRec
 
lcCurAlias = ALIAS()
lnStyRec   = RECNO("STYLE")
lnSclRec   = RECNO("SCALE")

lnSCnt   = 8
lnBomSzs = 8

SELECT (WORKFILE)
IF !EMPTY(MSizes)
  IF SEEK(ALLTRIM(cItmMajor) , 'STYLE') 
    SELECT STYLE
    LOCATE REST FOR Style.cStyMajor = &WORKFILE..cItmMajor .AND. ;
                    LIKE(STRTRAN(&WORKFILE..cItmMask , "*" , "?") , STYLE.Style)
    IF FOUND()
      IF SEEK('S'+STYLE.Scale,'SCALE')
        lnSCnt=SCALE.Cnt
      ENDIF
    ENDIF
  ENDIF
  
  SELECT (WORKFILE)
  IF llMScale AND cCatgTyp='S' AND !EMPTY(MSzCrosref)
    lnBomSzs=0
    FOR lnMln=1 TO MEMLINES(MSzCrosref)
      IF STYLE.Scale=SUBSTR( MLINE(MSzCrosref,lnMln) ,1 , 3)
        lnBomSzs=lnBomSzs+1
      ENDIF
    ENDFOR
  ELSE
    lnBomSzs=lnSCnt
    FOR lnMln=1 TO MEMLINES(MSizes)
      IF OCCURS(STYLE.Scale,MLINE(MSizes,lnMln)) <> 0
        lnBomSzs=OCCURS(',',MLINE(MSizes,lnMln))+1
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDIF 

lnCstBySiz = lnCstBySiz + ROUND((lnBomSzs/lnSCnt)*TotCost,2)
XTOT&z     = XTOT&z + ROUND((lnBomSzs/lnSCnt)*TotCost,2)

lnUntCost = UntCost * (lnBomSzs/lnSCnt)
lnTotCost = ROUND(TotCost * (lnBomSzs/lnSCnt),2)
IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
  GOTO lnStyRec IN STYLE
ENDIF
IF lnSclRec > 0 .AND. lnSclRec <= RECCOUNT("SCALE")
  GOTO lnSclRec IN SCALE
ENDIF
SELECT (lcCurAlias)

*!********************************************************************
*! Name      : lfAmntDisp
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
*! Purpose   : Return the amount according to the display condition.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcCurrCode   && Currency Code
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
FUNCTION lfAmntDisp
PARAMETER lnAmount,ldExRateDt,lcCurrCode

PRIVATE lnAmount,ldExRateDt,lcExSin1,lcExSin2,lnSavAlias

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)

lnExRate   = 0
lnUnit     = 0
    
IF lcCurrCode = gcBaseCurr
  lnExRate = 1
  lnUnit   = 1
ELSE
  lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
ENDIF

lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)

lcExSin2   = ' '
lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)

SELECT (lnSavAlias)

RETURN lnAmount

*!*************************************************************
*! Name      : lfPrHeader
*! Developer : AMHMED MAHER (AMH)
*! Date      : 04/09/2002
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
PRIVATE lnCoNameCol

lnCoNameCol = 47 - (LEN(TRIM(gccom_name)) / 2)
@ 01,00 SAY 'MFCSREP'
@ 01,lnCoNameCol SAY gccom_name
@ 01,86 SAY gdsysdate
@ 02,00 SAY TIME()
@ 02,36 SAY 'Style Cost Sheet Report'
@ 02,86 SAY 'PAGE#' 
@ 02,92 SAY HPAGENO PICTURE '9999'
@ 04,00 SAY REPLICATE('*',96)
*-- end of lfPrHeader.