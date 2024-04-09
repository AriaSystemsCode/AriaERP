*:***************************************************************************
*: Program file  : SOSCSREP
*: Program desc. : Sales Order Cost Sheet Report
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO )
*: Developer     : Mohamed Shokry Mohamed (MHM)
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
*: Example : DO SOSCSREP
*:***************************************************************************
*: This Report Program is due to C037961,1 ...
*****************************************************************************
*: Modifications :
*: B608144,1 NNA 06/28/2007 Fix bug that Style/Colour Report is not picking up the 
*: B608144,1 NNA            Selling Price for the colour on the ORDLINE
*: B608278,1 TMI 09/23/2007 prevent printing over 0
*: B608386,1 NNA 12/23/2007 Fix bug that the report displays ****** in fields (i.e Selling Price) when printed by style only
***************************************************************************** 
**-- define Variables --**
STORE SPACE(0) TO lnECost,lcCurCod
STORE 0 TO lnETotCost,lnLinTot,lnLenVal
STORE .F. TO llMulCurr
lnTradDisc = 0
DECLARE laTrdDis[1,2]
laTrdDis[1,1] = 'NTERDISCR'
laTrdDis[1,2] = 'lnTradDisc'

**-- open needed files --**
=gfOpenFile(gcSysHome+'SYCCURR','Ccurrcode','SH')

**-- chick if use multi currency --**
llMulCurr  = gfGetMemVar('llMulCurr')

llMScale = gfGetMemVar('M_USEEXSSC')

lnMajorLen = LEN(gfItemMask("PM"))
*--OrdHder Relations

*-- if first time you (preview or run ) or user change critria.

IF llClearFn OR llOGFltCh
 
  lcRpExp = [SEEK(STYLE.CSTYMAJOR,'ORDBOM') AND ] + lcRpExp
  *--collect only active styles
  llClearFn  = .F.  && do clear function flag
  llChStyClr = .F.  && change color/style or color selection flag
  llChFabric = .F.  && change fabric filter flag.
  llChStyle  = .F.  && change style filter flag.
  
  SELECT ORDLINE
  SET RELATION TO style INTO Style ADDITIVE
  SET RELATION TO cordtype+ order INTO OrdHdr ADDITIVE
  
  IF RECCOUNT(WORKFILE) > 0 
    USE IN (WORKFILE)
    = lfRecreate()
  ENDIF

  SELECT (WORKFILE)
  IF lcRpStyClr='C'
    INDEX ON Order + cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + Item + IClr TAG &WORKFILE
  ELSE
    INDEX ON Order + cItmMajor + Typ + Item + IClr + cSeekStyle  + cItmMask + MfgCode  TAG &WORKFILE
  ENDIF           


  SELECT (WORKSTY)
  IF lcRpStyClr='C'
    INDEX ON Order + cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + Item + IClr TAG &WORKSTY
  ELSE
    INDEX ON Order + cItmMajor + Typ + Item + IClr + cSeekStyle +  cItmMask + MfgCode  TAG &WORKSTY
  ENDIF           

  SELECT (WORKTRIM)
  IF lcRpStyClr='C'
    INDEX ON Order + Item + IClr + cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode  TAG &WORKTRIM
  ELSE
    INDEX ON Order + Item + IClr + cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode  TAG &WORKTRIM
  ENDIF           

  =lfGtOrdDat()
  
ENDIF  && end if first time you (preview or run ) or user change critria.
  
IF RECCOUNT(WORKFILE) = 0
  =gfModalGen(.f.,.f.,.f.,.f.,'There are no records to display...! ')  
  RETURN
ENDIF

* INITIALIZE PRINTER
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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

XSTYLE = cStyMajor

XSTYCLR = SUBSTR(ORDLINE.STYLE,lnMajorLen+1)
SELECT ORDBOM
= SEEK(XSTYLE)

  *-- scan style in BOM file
  SCAN REST WHILE (Style = XSTYLE)  
      IF ORDER <> Ordhdr.Order  
        LOOP
      ENDIF
      
      XUNTCOST=0
      IF SUBSTR(cItmMask,lnMajorLen+2) <>'******' AND SUBSTR(cItmMask,lnMajorLen+1)<>XSTYCLR 
        LOOP
      ENDIF
      
      DO CASE
        CASE cCatgTyp $ "PMD"
      
          SCATTER MEMVAR MEMO
          m.SClr       = XSTYCLR
          m.cSeekStyle = STYLE.STYLE

          *B608144,1 NNA 06/28/2007 (Begin) Pick Up Ordline.Lineno
          m.Lineno = Ordline.Lineno
          *B608144,1 NNA (End)
          
          INSERT INTO (WORKFILE) FROM MEMVAR

        OTHERWISE
        
          SCATTER MEMVAR MEMO
          IF cCatgTyp = "F" .OR. (cCatgTyp = "T" .AND. Trim_Invt)
            XKEY= PADR(ITEM,7) + IIF("*" $ ICLR , IIF(lcFree_Clr = 'C',SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),ICLR) , ICLR)
                        
            IF SEEK(XKEY,'FABRIC')
              XFABDESC= FABRIC.DESC
            ELSE
              XFABDESC=SPACE(0)
            ENDIF

            m.DESC     = IIF(ICLR = '******',XFABDESC,m.DESC)
            m.TOTCOST  = ROUND(m.UNTCOST * m.nBomTOTQTY,3)
          ENDIF
          m.cSeekStyle = STYLE.STYLE
          m.SClr     = XSTYCLR

          *B608144,1 NNA 06/28/2007 (Begin) Pick Up Ordline.Lineno
          m.Lineno = Ordline.Lineno
          *B608144,1 NNA (End)

          INSERT INTO (WORKFILE) FROM MEMVAR
      ENDCASE
  ENDSCAN  && end scan style in BOM file

*-- end of lpStyClr.

*!*************************************************************
*! Name      : lpStyOnly
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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

*-- Scan all selected styles.
SELECT ORDBOM
  
*-- scan style in BOM file

SCAN FOR cItmMajor = STYLE.cStyMajor
    IF ORDER <> Ordhdr.Order  
      LOOP
    ENDIF
    *--To get only select orders from ORDBOM
    lcStyMaj = PADR(SUBSTR(OrdLine.STYLE,1,lnMajorLen),19,' ')
    IF SEEK(Ordhdr.Order +lcStyMaj+OrdLine.STYLE+Typ + cItmMask + MfgCode + Item + IClr ,WORKFILE)
      LOOP
    ENDIF

    IF (lcOldMajor # STYLE.cStyMajor) OR ;
       (cCatgTyp $ 'FS' AND '******' $ cItmMask)
      SCATTER MEMVAR MEMO
      m.cSeekStyle = IIF('******' $ cItmMask , STYLE.STYLE , cItmMask)

      m.SClr       = SUBSTR(m.cSeekStyle,lnMajorLen+1) 

      IF EMPTY(m.ICLR) AND (cCatgTyp = 'S')
        m.IClr = SUBSTR(m.ITEM,lnNonMajSt,lnColorLen)
      ENDIF

      *B608386,1 NNA 12/23/2007 (Begin) Pick Up Ordline.Lineno
        m.Lineno = Ordline.Lineno
      *B608386,1 NNA (End)

      INSERT INTO (WORKFILE) FROM MEMVAR
    ENDIF
ENDSCAN  && end scan style in BOM file

lcOldMajor = STYLE.cStyMajor
SELECT STYLE
lcStyMaj = SUBSTR(STYLE,1,lnMajorLen)
LOCATE FOR SUBSTR(STYLE,1,lnMajorLen) > lcStyMaj

*-- end of lpStyOnly.

*!*************************************************************
*! Name      : lpSCPrn
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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
XREPORT='SOSCSREP'
R_TITLE ='Sales Order Cost Sheet Report'

lnCstBySiz = 0
lnTotPQty = 0
lnSCnt    = 8
lnBomSzs  = 8
lnUntCost = 0
lnTotCost = 0

STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5
XNEW_STY=.T.            && FLAG THAT TELLS IF NEW STYLE OR NOT
lcOrder = ""
llOrder=.T.
llPrntQty = .F.
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
        @ ROW,084 SAY lnETotCost PICTURE '99999999.999'
        XSTYCST = lnETotCost
      ELSE
        @ ROW,068 SAY lnCstBySiz PICTURE '99999999.999'
        XSTYCST = lnCstBySiz
      ENDIF
      
      ROW=ROW+1
      lnETotCost = 0 
	  lnLinTot = 0 
      ROW=ROW+1
      @ ROW,026 SAY 'SELLING PRICE :'      
      @ ROW,084 SAY XPRICEA PICTURE '99999999.999'

      ROW=ROW+1
      @ ROW,026 SAY 'LESS SETTLEMENT DISCOUNT :'
      *--get Term Codes Percentage 
      =gfRltFld(ORDHDR.CTERMCODE , @laTrdDis , 'CTERMCODE')
      @ ROW,055 SAY lnTradDisc PICTURE '99.99%'
      lnCurTrad = ROUND((XPRICEA*lnTradDisc/100),3)
      @ ROW,084 SAY -1*(lnCurTrad) PICTURE '99999999.999'
      
      ROW=ROW+1
      @ ROW,026 SAY 'LESS COMMISSION :'
      *--get Term Codes Percentage 
      @ ROW,055 SAY OrdHdr.Comm1 PICTURE '99.99%'
      lnCurComm = ROUND((XPRICEA*OrdHdr.Comm1/100),3)
      @ ROW,084 SAY -1* lnCurComm PICTURE '99999999.999'

      ROW=ROW+1
      @ ROW,026 SAY 'NET SELLING PRICE :'      
      @ ROW,084 SAY XPRICEA- lnCurComm - lnCurTrad PICTURE '99999999.999'

      ROW=ROW+2
      @ ROW,026 SAY 'MARGIN £ :'      
      @ ROW,084 SAY XPRICEA- lnCurComm - lnCurTrad -XSTYCST PICTURE '99999999.999'
      
      ROW=ROW+1
      @ ROW,026 SAY 'MARGIN % :'      
      @ ROW,084 SAY ((XPRICEA- lnCurComm -lnCurTrad -XSTYCST)/(XPRICEA- lnCurComm - lnCurTrad ))*100 PICTURE '99999999.999'
      
      ROW=ROW+1 
	  IF llMulCurr
	    @ ROW,00 SAY REPLICATE('=',96)
	  ELSE
	    @ ROW,00 SAY REPLICATE('=',80)
	  ENDIF  
	    
    ENDIF

    ROW=ROW+1

    lnCstBySiz = 0
    
    lnSCnt   = 8
    lnBomSzs = 8
    lnUntCost = 0
    lnTotCost = 0
    
    
    STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5

    XNEW_STY =.T.
    llOrder  =.T.
    
    HBREAK = &BREAK
    lcSayStyle = cSeekStyle
    EXIT
  ENDDO

  IF EOF()
    EXIT
  ENDIF
  *-- Sales order Data
  IF lcOrder <> Order AND llOrder
    lcOrder = Order  
    llOrder = .F.
    llPrntQty = .F.
    XNEW_STY=.T.    
    =SEEK("O"+Order,'ORDHDR')
    =SEEK("M"+OrdHdR.Account,'CUSTOMER')
    =SEEK("O"+Order,'ORDCANLN')
    =SEEK("O"+Order,'OrdLine')

    SELECT ORDLINE
    SET ORDER TO TAG ORDLINES
    =SEEK(ALLTRIM(&WORKFILE..Style),'OrdLine')
    SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6)= ALLTRIM(&WORKFILE..Style)
      IF Order <> lcOrder
        LOOP
      ENDIF  
      IF Style <> &WORKFILE..cSeekStyle
        LOOP
      ENDIF
      =SEEK('S' + OrdLine.SCALE,'SCALE')    
      =SEEK("O"+Order+STR(LINENO,6),'ORDCANLN')
      lnTotPQty = lnTotPQty + (OrdLine.TotBook-OrdCanln.TOtqty)
    ENDSCAN
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINE
    SELECT (WORKFILE)

    @ ROW,000 SAY 'Sales Order' 
    @ ROW,013 SAY 'Customer'
    @ ROW,060 SAY 'Quantity'
    @ ROW,070 SAY 'Size Range'
   
    ROW=ROW+1
    
    @ ROW,000 SAY Order 
    @ ROW,013 SAY CUSTOMER.Account 
    @ ROW,020 SAY ALLTRIM(CUSTOMER.BTName)
    @ ROW,060 SAY lnTotPQty PICTURE '999999999'
    @ ROW,070 SAY Scale.cscl_desc
    ROW=ROW+1
  ENDIF


  IF XNEW_STY
    XNEW_STY=.F.
    
    IF llPrntQty 
      @ ROW,060 SAY 'Quantity'
      @ ROW,070 SAY 'Size Range'
      ROW=ROW+1
      =SEEK("O"+Order,'ORDCANLN')
      =SEEK("O"+Order,'OrdLine')
 
      SELECT ORDLINE
      SET ORDER TO TAG ORDLINES
      lnTotPQty = 0
      =SEEK(ALLTRIM(&WORKFILE..Style),'OrdLine')
      SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6)= ALLTRIM(&WORKFILE..Style)
        IF Order <> lcOrder
          LOOP
        ENDIF  
        IF Style <> &WORKFILE..cSeekStyle
          LOOP
        ENDIF
        =SEEK('S' + OrdLine.SCALE,'SCALE')    
        =SEEK("O"+Order+STR(LINENO,6),'ORDCANLN')
        lnTotPQty = lnTotPQty + (OrdLine.TotBook-OrdCanln.TOtqty)
      ENDSCAN
      SELECT ORDLINE
      SET ORDER TO TAG ORDLINE
      SELECT (WORKFILE)

      @ ROW,060 SAY lnTotPQty PICTURE '999999999'
      @ ROW,070 SAY Scale.cscl_desc
      ROW=ROW+1
    ENDIF
    llPrntQty = .T.
    @ ROW,000 SAY PADR(lcStyTitle,19) + '->' + PADR(cSeekStyle,19) + '  '
    IF SEEK(cSeekStyle,'STYLE')
      XSTYDESC = STYLE.DESC1
      *--get selling price from ordline
      =SEEK('O'+order,'ORDHDR')
    
      *B608144,1 NNA 06/28/2007 (Begin) Seek for Ordline.Lineno as additive
      *=SEEK('O'+order,'ORDLINE')
      =SEEK('O'+order+STR(lineno,6),'ORDLINE')    
      *B608144,1 NNA (End)
      
      lnPric = (ORDLINE.Price/OrdHdr.nExRate)
      XPRICEA  = lnPric - (OrdHdr.Disc * lnPric)/100
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
  

  @ ROW,21 SAY SUBSTR(DESC,1,17)
  
  @ ROW,44 SAY UOM
  @ ROW,48 SAY nBomTOTQTY PICTURE '999.999'

  Z = Typ
  =lfCostBySz()

  *- if user can access cost
  IF qCostPrv         && COSTING ACCESS
    @ ROW,56 SAY lnUntCost PICTURE '9999999.999'
    @ ROW,68 SAY lnTotCost PICTURE '99999999.999'
    IF llMulCurr
      @ ROW,80 SAY IIF(SEEK(lcCurCod,'SYCCURR'),SYCCURR.cCurrSmbl,'')
   	  lnECost = lfAmntDisp(lnTotCost,gdSysDate,lcCurCod)
      @ ROW,84 SAY lnECost PICTURE '99999999.999'
      lnETotCost = lnETotCost + lnECost
      lnLinTot = lnLinTot + lnECost
    ENDIF
    
  ENDIF
  ROW=ROW+1
  Z = Typ

  SELE &WORKFILE
  SKIP
  IF lcOrder <> Order  
    llOrder = .T.
    XNEW_STY= .T.
    LOOP
  ENDIF
  
  IF XPRV_TYP<>TYP

    IF qCostPrv          && COSTING ACCESS

      DO CASE
        
        CASE XPRV_TYP='1'
          @ ROW,027 SAY ' '+lcSlbl1+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '999999999.999'
            lnLinTot = 0 
          ELSE
            @ ROW,067 SAY XTOT1 PICTURE '999999999.999'          
          ENDIF       
        CASE XPRV_TYP='2'
          @ ROW,027 SAY ' '+lcSlbl2+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '999999999.999'
            lnLinTot = 0 
          ELSE
            @ ROW,067 SAY XTOT2 PICTURE '999999999.999'
          ENDIF
        CASE XPRV_TYP='3'
          @ ROW,027 SAY ' '+lcSlbl3+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '999999999.999'
            lnLinTot = 0 
          ELSE
            @ ROW,067 SAY XTOT3 PICTURE '999999999.999'
          ENDIF
        CASE XPRV_TYP='4'
          @ ROW,027 SAY ' '+lcSlbl4+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '999999999.999'
            lnLinTot = 0           
          ELSE  
            @ ROW,067 SAY XTOT4 PICTURE '999999999.999'
          ENDIF
        CASE xPrv_Typ = "5"
          @ ROW,027 SAY ' '+lcSlbl5+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '999999999.999'
            lnLinTot = 0          
          ELSE
            @ ROW,067 SAY XTOT5 PICTURE '999999999.999' 
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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

lnSCnt   = 8
lnBomSzs = 8
lnUntCost = 0
lnTotCost = 0

STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5

XNEW_STY=.T.            && FLAG THAT TELLS IF NEW STYLE OR NOT
lcOrder = ""
llOrder=.T.
llPrntQty = .F.
lnTotPQty=0
XPRV_TYP = TYP

HBREAK=SPACE(1)
IF LEN(TRIM(BREAK)) <>0
   HBREAK = &BREAK
ENDIF
SELECT (WORKFILE)
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
   		@ ROW,084 SAY lnETotCost PICTURE '99999999.999'
        XSTYCST = lnETotCost
   		lnETotCost = 0 
		lnLinTot = 0 
      ELSE
        @ ROW,068 SAY lnCstBySiz PICTURE '99999999.999'
        XSTYCST = lnCstBySiz
      ENDIF     
    ENDIF
      ROW=ROW+1
      @ ROW,026 SAY 'SELLING PRICE :'      
      @ ROW,084 SAY XPRICEA PICTURE '99999999.999'

      ROW=ROW+1
      @ ROW,026 SAY 'LESS SETTLEMENT DISCOUNT :'
      *--get Term Codes Percentage 
      =gfRltFld(ORDHDR.CTERMCODE , @laTrdDis , 'CTERMCODE')
      @ ROW,055 SAY lnTradDisc PICTURE '99.99'
      lnCurTrad = ROUND((XPRICEA*lnTradDisc/100),3)
      @ ROW,084 SAY -1*(lnCurTrad) PICTURE '99999999.999'
      
      ROW=ROW+1
      @ ROW,026 SAY 'LESS COMMISSION :'
      *--get Term Codes Percentage 
      @ ROW,055 SAY OrdHdr.Comm1 PICTURE '99.99'
      lnCurComm = ROUND(XPRICEA*OrdHdr.Comm1/100,3)
      @ ROW,084 SAY -1*(lnCurComm) PICTURE '99999999.999'

      ROW=ROW+1
      @ ROW,026 SAY 'NET SELLING PRICE :'      
      @ ROW,084 SAY XPRICEA- lnCurComm - lnCurTrad  PICTURE '99999999.999'

      ROW=ROW+2
      @ ROW,026 SAY 'MARGIN £ :'      
      @ ROW,084 SAY XPRICEA- lnCurComm - lnCurTrad -XSTYCST PICTURE '99999999.999'
      
      ROW=ROW+1
      @ ROW,026 SAY 'MARGIN % :'      
      @ ROW,084 SAY ((XPRICEA- lnCurComm - lnCurTrad -XSTYCST)/(XPRICEA- lnCurComm -lnCurTrad))*100 PICTURE '99999999.999'
    
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
    llOrder =.T.
    
    HBREAK = &BREAK
    EXIT
  ENDDO

  IF EOF()
    EXIT
  ENDIF

  *-- Sales order Data
  IF lcOrder <> Order AND llOrder
    lcOrder = Order  
    llOrder = .F.
    llPrntQty = .F.
    =SEEK("O"+Order,'ORDHDR')
    =SEEK("M"+OrdHdR.Account,'CUSTOMER')
    =SEEK('S' + OrdLine.SCALE,'SCALE')    

    SELECT ORDLINE
    SET ORDER TO TAG ORDLINES
    =SEEK(ALLTRIM(&WORKFILE..Style),'OrdLine')
    SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6)= SUBSTR(&WORKFILE..STYLE,1,lnMajorLen)
      IF Order <> lcOrder
        LOOP
      ENDIF  
      =SEEK("O"+Order+STR(LINENO,6),'ORDCANLN')
      lnTotPQty = lnTotPQty + (OrdLine.TotBook-OrdCanln.TOtqty)
    ENDSCAN
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINE
    SELECT (WORKFILE)
    =SEEK("O"+Order+STR(LINENO,6),'OrdLine')
    @ ROW,000 SAY 'Sales Order' 
    @ ROW,013 SAY 'Customer'
    @ ROW,060 SAY 'Quantity'
    @ ROW,070 SAY 'Size Range'
   
    ROW=ROW+1
    
    @ ROW,000 SAY Order 
    @ ROW,013 SAY ALLTRIM(CUSTOMER.Account )
    @ ROW,020 SAY ALLTRIM(CUSTOMER.BTName)
    @ ROW,060 SAY lnTotPQty PICTURE '999999999'
    @ ROW,070 SAY Scale.cscl_desc
    ROW=ROW+1
  ENDIF

  IF XNEW_STY
    XNEW_STY=.F.
    IF llPrntQty 
      lnTotPQty = 0
      SELECT ORDLINE
      SET ORDER TO TAG ORDLINES
      =SEEK(ALLTRIM(&WORKFILE..Style),'OrdLine')
      SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6)= ALLTRIM(&WORKFILE..Style)
        IF Order <> lcOrder
          LOOP
        ENDIF  
        =SEEK("O"+Order+STR(LINENO,6),'ORDCANLN')
        lnTotPQty = lnTotPQty + (OrdLine.TotBook-OrdCanln.TOtqty)
      ENDSCAN
      SELECT ORDLINE
      SET ORDER TO TAG ORDLINE
      SELECT(WORKFILE)
      =SEEK("O"+Order+STR(LINENO,6),'OrdLine')
      @ ROW,060 SAY 'Quantity'
      @ ROW,070 SAY 'Size Range'
      ROW=ROW+1
      @ ROW,060 SAY lnTotPQty PICTURE '999999999'
      @ ROW,070 SAY Scale.cscl_desc
      ROW=ROW+1
    ENDIF
    llPrntQty = .T.
    SELECT (WORKFILE)

    XSTYLE = ALLTRIM(&WORKFILE..cItmMajor)
    @ ROW,000 SAY ALLTRIM(lcStyMajor) + '->' + XSTYLE
    
    = SEEK (cSeekStyle,'STYLE')
    
    @ ROW,039 SAY PADR(STYLE.DESC1,41)
    =SEEK('O'+order,'ORDHDR')
   
    *B608144,1 NNA 06/28/2007 (Begin) Seek for Ordline.Lineno as additive
    *=SEEK('O'+order,'ORDLINE')
    =SEEK('O'+order+STR(lineno,6),'ORDLINE')    
    *B608144,1 NNA (End)
    
    lnPric   = (ORDLINE.Price/OrdHdr.nExRate)
    XPRICEA  = lnPric - (OrdHdr.Disc * lnPric)/100
    
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

    SELECT ORDLINE
    lnCurrRec = RECNO()
    =SEEK("O"+lcOrder,'OrdLine')
    lcCurrSty = ""
    SCAN REST WHILE cordtype+order+STR(lineno,6)= "O"+lcOrder
      IF !(ALLTRIM(SUBSTR(STYLE,1,lnMajorLen)) == ALLTRIM(XSTYLE))
        LOOP
      ENDIF
      IF lcCurrSty = STYLE
        LOOP
      ENDIF
      lcCurrSty = STYLE      
      
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
      
      ROW = IIF( COL >= 60 , ROW + 1  , ROW )
      COL = IIF( COL <= 60 , COL + 7 , 14 )
      
    ENDSCAN
    IF lnCurrRec > 0 .AND. lnCurrRec <= RECCOUNT()
      GOTO lnCurrRec
    ENDIF
    
    SELECT (WORKFILE)

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
  
  @ ROW,24 SAY SUBSTR(DESC,1,17)
  
  @ ROW,45 SAY UOM
  @ ROW,49 SAY nBomTOTQTY PICTURE '999.999'
 
  Z = Typ
  =lfCostBySz()

  IF qCostPrv             && COSTING ACCESS
    @ ROW,57 SAY lnUntCost PICTURE '9999999.999'
    @ ROW,69 SAY lnTotCost PICTURE '9999999.999'
    IF llMulCurr
	  lnECost = lfAmntDisp(lnTotCost,gdSysDate,lcCurCod)
      @ ROW,80 SAY IIF(SEEK(lcCurCod,'SYCCURR'),SYCCURR.cCurrSmbl,'')
	  lnECost = lfAmntDisp(lnTotCost,gdSysDate,lcCurCod)
      @ ROW,84 SAY lnECost PICTURE '99999999.999'
      lnETotCost = lnETotCost + lnECost
      lnLinTot = lnLinTot + lnECost
    ENDIF
  ENDIF
  ROW=ROW+1
  
  Z = Typ
  IF cCatgTyp $ "F"
    COL = 15
    @ ROW,06 SAY 'COLORS  :'
    @ ROW,COL SAY MLINE(mFabColor,1)
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
            @ ROW,083 SAY lnLinTot PICTURE '999999999.999'
            lnLinTot = 0 
          ELSE
            @ ROW,067 SAY XTOT1 PICTURE '999999999.999'
          ENDIF
        CASE XPRV_TYP='2'
          @ ROW,027 SAY ' '+lcSlbl2+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '999999999.999'
            lnLinTot = 0          
          ELSE
            @ ROW,067 SAY XTOT2 PICTURE '999999999.999'
          ENDIF
        CASE XPRV_TYP='3'
          @ ROW,027 SAY ' '+lcSlbl3+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '999999999.999'
            lnLinTot = 0                   
          ELSE
            @ ROW,067 SAY XTOT3 PICTURE '999999999.999'
          ENDIF
        CASE XPRV_TYP='4'
          @ ROW,027 SAY ' '+lcSlbl4+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '999999999.999'
            lnLinTot = 0                            
          ELSE
            @ ROW,067 SAY XTOT4 PICTURE '999999999.999'
          ENDIF
        CASE xPrv_Typ = "5"
          @ ROW,027 SAY ' '+lcSlbl5+' TOTAL :=>'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '999999999.999'
            lnLinTot = 0                   
          ELSE
            @ ROW,067 SAY XTOT5 PICTURE '999999999.999'
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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
  SET ORDER TO STYBOM IN ORDBOM      && To use it to validate STYLE   # in option grid.
  

  IF 'MA' $ gcComp_Mdl
    SET ORDER TO FABRIC IN FABRIC      && To use it to validate STYLE   # in option grid.
  ENDIF  

  DIMENSION laTempStru[1,4]
  SELECT ORDBOM
  = AFIELDS(laTempStru)
  DIMENSION laTempStru[ALEN(laTempStru,1) + 4, 4]

  *-- cSeekStyle     :  field save style to seek it in style file.
  laTempStru[ALEN(laTempStru,1)-3  ,1] = 'cSeekStyle'
  laTempStru[ALEN(laTempStru,1)-3  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)-3  ,3] = 19
  laTempStru[ALEN(laTempStru,1)-3  ,4] = 0

  laTempStru[ALEN(laTempStru,1)-2  ,1] = 'TotQty'
  laTempStru[ALEN(laTempStru,1)-2  ,2] = 'N'
  laTempStru[ALEN(laTempStru,1)-2  ,3] = 12
  laTempStru[ALEN(laTempStru,1)-2  ,4] = 3

  laTempStru[ALEN(laTempStru,1)-1,1] = 'TotValu'
  laTempStru[ALEN(laTempStru,1)-1,2] = 'N'
  laTempStru[ALEN(laTempStru,1)-1,3] = 12
  laTempStru[ALEN(laTempStru,1)-1,4] = 3

  laTempStru[ALEN(laTempStru,1),1] = 'MFabColor'
  laTempStru[ALEN(laTempStru,1),2] = 'M'
  laTempStru[ALEN(laTempStru,1),3] = 0
  laTempStru[ALEN(laTempStru,1),4] = 0

  *-- Create temporary file that holding order line data. [end]
  *mhm2004--change getting data for trim
  lnTempStru = ALEN(laTempStru,1)
  DIMENSION laTempStru[lnTempStru + 9, 4]

  *-- cSeekStyle     :  field save style to seek it in style file.
  laTempStru[lnTempStru +1  ,1] = 'Qty1'
  laTempStru[lnTempStru +1  ,2] = 'N'
  laTempStru[lnTempStru +1  ,3] = 6
  laTempStru[lnTempStru +1  ,4] = 0

  laTempStru[lnTempStru +2  ,1] = 'Qty2'
  laTempStru[lnTempStru +2  ,2] = 'N'
  laTempStru[lnTempStru +2  ,3] = 6
  laTempStru[lnTempStru +2  ,4] = 0

  laTempStru[lnTempStru +3  ,1] = 'Qty3'
  laTempStru[lnTempStru +3  ,2] = 'N'
  laTempStru[lnTempStru +3  ,3] = 6
  laTempStru[lnTempStru +3  ,4] = 0

  laTempStru[lnTempStru +4  ,1] = 'Qty4'
  laTempStru[lnTempStru +4  ,2] = 'N'
  laTempStru[lnTempStru +4  ,3] = 6
  laTempStru[lnTempStru +4  ,4] = 0

  laTempStru[lnTempStru +5  ,1] = 'Qty5'
  laTempStru[lnTempStru +5  ,2] = 'N'
  laTempStru[lnTempStru +5  ,3] = 6
  laTempStru[lnTempStru +5  ,4] = 0

  laTempStru[lnTempStru +6  ,1] = 'Qty6'
  laTempStru[lnTempStru +6  ,2] = 'N'
  laTempStru[lnTempStru +6  ,3] = 6
  laTempStru[lnTempStru +6  ,4] = 0

  laTempStru[lnTempStru +7  ,1] = 'Qty7'
  laTempStru[lnTempStru +7  ,2] = 'N'
  laTempStru[lnTempStru +7  ,3] = 6
  laTempStru[lnTempStru +7  ,4] = 0

  laTempStru[lnTempStru +8  ,1] = 'Qty8'
  laTempStru[lnTempStru +8  ,2] = 'N'
  laTempStru[lnTempStru +8  ,3] = 6
  laTempStru[lnTempStru +8  ,4] = 0

  laTempStru[lnTempStru +9  ,1] = 'TotOrdQty'
  laTempStru[lnTempStru +9  ,2] = 'N'
  laTempStru[lnTempStru +9  ,3] = 12
  laTempStru[lnTempStru +9  ,4] = 3

  = lfRecreate()

  lcStyleTyp  = "I"
  FOR lnCount = 1 TO 5
    lcCount  = STR(lnCount,1)
    lcSlbl&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'SLBL'+lcCount)
  ENDFOR 
ENDIF  
lnOrdPos = lfItmPos('ORDHDR.ORDER')
lnStyPos = lfItmPos('STYLE.CSTYMAJOR') 

*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
*! Purpose   : Create temp files
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
*! Example   : =lfRecreate()
*!*************************************************************
FUNCTION lfRecreate

CREATE CURSOR (WORKFILE) ;
   FROM ARRAY laTempStru
CREATE CURSOR (WORKSTY) ;
   FROM ARRAY laTempStru

CREATE CURSOR (WORKTRIM) ;
   FROM ARRAY laTempStru


*-- end of lfRecreate.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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

FUNCTION lfChanged
llChStyClr = .T.
*-- END OF lfChanged.

*!*************************************************************
*! Name      : lfFillForm
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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

*-- MAB Set default type for form format. 
*-- E300967 MAB 8/18/1998
FUNCTION lfFormDefa
RETURN IIF(lcFree_Clr = 'C','C','S')
*-- end of lfFormDefa.

*!*************************************************************
*! Name      : lfCostBySz
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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

lnCstBySiz = lnCstBySiz + ROUND((lnBomSzs/lnSCnt)*UNTCOST * nBomTotQty,3)
XTOT&z     = XTOT&z + ROUND((lnBomSzs/lnSCnt)*UNTCOST * nBomTotQty,3)

*--mhm2004
IF ccatgtyp = 'T' AND !EMPTY(MSizes)
  lnUntCost = UntCost 
  lnTotCost = UNTCOST * nBomTotQty 
ELSE
  lnUntCost = UntCost * (lnBomSzs/lnSCnt)
  lnTotCost = ROUND(UNTCOST * nBomTotQty * (lnBomSzs/lnSCnt),3)
ENDIF

IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
  GOTO lnStyRec IN STYLE
ENDIF
IF lnSclRec > 0 .AND. lnSclRec <= RECCOUNT("SCALE")
  GOTO lnSclRec IN SCALE
ENDIF

SELECT (lcCurAlias)

*!********************************************************************
*! Name      : lfAmntDisp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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
lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,3)

SELECT (lnSavAlias)

RETURN lnAmount
*!*************************************************************
*! Name      : lfPrHeader
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
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

* SOSCSREP                                Omar Ramadan                               05/20/00 ~
* 12:15:58                           Sales Order Cost Sheet Report                         PAGE#    1
*                                        
* *********************************************************************************************

lnCoNameCol = 47 - (LEN(TRIM(gccom_name)) / 2)
@ 01,00 SAY 'SOSCSREP'
@ 01,lnCoNameCol SAY gccom_name
@ 01,86 SAY gdsysdate
@ 02,00 SAY TIME()
@ 02,36 SAY 'Sales Order Cost Sheet Report'
@ 02,86 SAY 'PAGE#' 
@ 02,92 SAY HPAGENO PICTURE '9999'
@ 04,00 SAY REPLICATE('*',96)
*-- end of lfPrHeader.

*!*************************************************************
*! Name      : lfGtOrdDat
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
*! Purpose   : Get Data for orders
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     :  DO lfGtOrdDat
*!*************************************************************
*
PROCEDURE lfGtOrdDat

SET ORDER TO STYBOM IN ORDBOM      && To use it to validate STYLE   # in option grid.

SELECT ORDLINE
SCAN FOR &lcRpExp
    IF lcRpStyClr='C'
      IF !SEEK(OrdLine.Order +PADR(SUBSTR(OrdLine.STYLE,1,lnMajorLen),19,' ')+OrdLine.STYLE,WORKFILE)
        DO lpStyClr           && PRINT BY COLOR
      ENDIF
    ELSE
      IF !SEEK(OrdLine.Order +SUBSTR(OrdLine.STYLE,1,lnMajorLen),WORKFILE)
        DO lpStyOnly          && PRINT FOR ALL COLORS
      ENDIF  
    ENDIF           
ENDSCAN

IF lcRpStyClr<>'C'
  =lfCalcSty()
ENDIF

*--mhm2005
IF lcRpStyClr = 'C'

  SELECT (WORKFILE)

  lnContRec = 0
  lnDutyAmt = 0
  lnTotPQty = 0
  SCAN FOR (cCatgTyp $ "T")
    SCATT MEMVAR MEMO  
    =lfClCLRTrm()
  ENDSCAN

  SELECT (WORKTRIM)
  SCAN
    REPLACE TOTValu WITH TOTQTY * UntCost
  ENDSCAN

  SELECT (WORKTRIM)
  SCAN
    *B608278,1 TMI [Start] prevent printing over 0
    *REPLACE UntCost WITH ROUND(TOTValu/TotOrdQty,3)
    REPLACE UntCost WITH IIF(TotOrdQty>0,ROUND(TOTValu/TotOrdQty,3),0)
    *B608278,1 TMI [End  ] 
    REPLACE TotCost WITH UntCost * nBomTOTQTY
  ENDSCAN

  SELECT (WORKFILE)
  DELETE ALL FOR (cCatgTyp $ "T") 
  SELECT (WORKTRIM)
  SCAN
    SCATTER MEMVAR MEMO
    INSERT INTO (WORKFILE) FROM MEMVAR
  ENDSCAN
  SELECT (WORKTRIM)
  ZAP
ENDIF
*--mhm2005

SET ORDER TO ORDBOM IN ORDBOM      && To use it to validate STYLE   # in option grid.

*!**************************************************************************
*! Name      : lfSROrder
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
*! Purpose   : Go top in the ORDHDR IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSROrder()
*!**************************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    GO TOP
  CASE lcParm = 'R'
    *--get only selected styles from Order lines
    SELECT laOGFxFlt[lnOrdPos,6]
    LOCATE 
    IF !EOF()
      lcCurrOrd = ''
      SCAN
        SELECT ORDLINE
        SCAN FOR cordtype+order+STR(lineno,6) =ORDHDR.cOrdType+&laOGFxFlt[lnOrdPos,6]..Order
          lcXStyle = SUBSTR(STYLE,1,lnMajorLen)
          lcXStyle = PADR(lcXStyle,19,' ')
          IF AT(lcXStyle, lcCurrOrd)=0
            lcCurrOrd = lcXStyle+ ',' +lcCurrOrd
          ENDIF
        ENDSCAN
      ENDSCAN
      lcCurrOrd = LEFT(lcCurrOrd,LEN(lcCurrOrd)-1) 
    ENDIF
ENDCASE
*-- end of lfSROrder.


*!**************************************************************************
*! Name      : lfColor
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
*! Purpose   : Go top in the ORDHDR IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSROrder()
*!**************************************************************************
FUNCTION lfColor
PRIVATE lcItsName , lcItsVal , llObjRet

lcItsName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcItsVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

IF '?' $ lcItsVal .OR. !EMPTY(lcItsVal) 

    IF USED(laOGFxFlt[lnStyPos,6])
    SELECT laOGFxFlt[lnStyPos,6]
    LOCATE 
    
    IF !EOF()
      lcCurrSty = ''
      SCAN
        lcCurrSty = cStyMajor + ',' +lcCurrSty
      ENDSCAN
      lcCurrSty = LEFT(lcCurrSty,LEN(lcCurrSty)-1) 
      SELECT STYLE
      lcCodtion = "cStyMajor $ lcCurrSty"
    ELSE
      lcCodtion = ".T."
    ENDIF
  ELSE
      lcCodtion = ".T."  
  ENDIF  

  SELECT DISTINCT SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen) AS COLOR  FROM Style ;
         WHERE &lcCodtion;
         ORDER BY COLOR;
         INTO CURSOR (lcTmpColor)

  DIMENSION laTemp[1]
  laTemp = ''                           && Array to hold the Selected value

  lcBrFields = "Color             :R :H= 'Color'"
  lcFile_Ttl = 'Color'
  
  =gfBrows("",'COLOR','laTemp')
  lcItsVal= laTemp[1]
  lcItsVal = IIF(!EMPTY(lcItsVal) , lcItsVal , laOldVal)
  &lcItsName = lcItsVal
  SELECT STYLE
  SET FILTER TO 
  
ENDIF    && End of IF
*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.
*!*************************************************************
*! Name      : lfCalcSty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
*! Purpose   : Calculate Fabrics in Case of styes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
FUNCTION lfCalcSty
PRIVATE lcCurOrd,lcCurrSty,lnTotCst, lnAvgRat,lnAvgCost,lnContRec,;
        lnTotPQty,lnTotcQty,lnTotValu,lnTotQty,lnTotPQty 

*-- we add three variables to calculate the Qty
STORE 0 TO lnTotCst, lnAvgRat,lnAvgCost,lnContRec,lnTotPQty,lnTotcQty,;
           lnTotValu,lnTotQty,lnTotPQty 
SELECT (WORKFILE)
LOCATE

SCAN FOR (cCatgTyp $ "F")
  SCATT MEMVAR MEMO  
  IF SEEK(Order + cItmMajor + Typ + Item + IClr + cSeekStyle +  cItmMask + MfgCode ,WORKSTY)
    LOOP
  ENDIF
  IF SUBSTR(cItmMask,lnMajorLen+2) ='******' 
    =lfFabColor()
    REPLACE mFabcolor with m.mFabcolor
    LOOP
  ENDIF
  =SEEK("O"+Order,'OrdLine')
  =SEEK("O"+Order,'ORDCANLN')
  SELECT ORDLINE
  SET ORDER TO TAG ORDLINES
  =SEEK(ALLTRIM(&WORKFILE..Style),'OrdLine')
  lnTotQty = 0
  SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6)= SUBSTR(&WORKFILE..STYLE,1,lnMajorLen)
    IF Order <> &WORKFILE..Order
      LOOP
    ENDIF  
    lnTotcQty = lnTotcQty + (OrdLine.TotBook-OrdCanln.TOtqty)
    IF Style <> &WORKFILE..cSeekStyle
      LOOP
    ENDIF
    IF ALLTRIM(&WORKFILE..Style) = ALLTRIM(SUBSTR(Ordline.STYLE,1,lnMajorLen))
      lnTotQty = lnTotQty+ (OrdLine.TotBook-OrdCanln.TOtqty)
    ENDIF
    =SEEK("O"+Order+STR(LINENO,6),'ORDCANLN')
    lnTotPQty =  (OrdLine.TotBook-OrdCanln.TOtqty)
  ENDSCAN
  SELECT ORDLINE
  SET ORDER TO TAG ORDLINE
  SELECT (WORKFILE)
  IF  lnTotQty<> 0
    m.TotQty = lnTotQty
    m.TotValu = lnTotQty * &WORKFILE..UntCost * &WORKFILE..nBomTotQty
    m.TotOrdQty = lnTotcQty
    lnTotcQty = 0
    INSERT INTO (WORKSTY) FROM MEMVAR
  ENDIF
ENDSCAN

SELECT (WORKFILE)
LOCATE
lcCurOrd  = ''
lcCurrSty = ''
lcFabStr  = ''
SCAN FOR (cCatgTyp $ "F")
    IF lcCurOrd <> ORDER
      lcFabStr = ''
      lcCurOrd  = ORDER
    ENDIF
    IF EMPTY(lcFabStr)
      lcFabStr = lcFabStr + ALLTRIM(ITEM)
    ELSE
      IF AT(ALLTRIM(ITEM),lcFabStr) = 0
        lcFabStr = lcFabStr +'|'+ ALLTRIM(ITEM)
      ELSE
        LOOP
      ENDIF
    ENDIF
    lcCurrSty = cItmMajor 
    IF !SEEK(Order + cItmMajor + Typ + Item + IClr + cSeekStyle +  cItmMask + MfgCode ,WORKSTY)
      LOOP
    ENDIF
    SELECT (WORKSTY)
    lnContRec = 0
    lnAvgRat  = 0
    lnTotValu = 0
    lnTotCst  = 0
    lnUntcst  = 0
    lnCurrTot = 0
    SCAN REST WHILE Order + cItmMajor + Typ + Item + IClr + cSeekStyle +  cItmMask + MfgCode  = lcCurOrd+lcCurrSty 
      IF SUBSTR(cItmMask,lnMajorLen+2) ='******' 
        LOOP
      ENDIF
      IF Item <> &WORKFILE..Item
        LOOP
      ENDIF
      IF cadd_user = "*****"
        LOOP
      ENDIF
      IF (lnTotCst = 0) AND (lnUntcst = 0) 
        lnUntcst  = UNTCOST
      ELSE
        IF !(lnUntcst  = UNTCOST)
          lnUntcst = 0        
        ENDIF 
      ENDIF
      lnTotCst   = lnTotCst+TotQty
      lnCurrTot   = TotQty
      lnTotValu  = lnTotValu+TotValu
      lnContRec  = lnContRec + 1
      IF lnAvgRat  = 0
        *B608386,1 NNA 12/23/2007 (Begin) Avoid Numeric Overflow if we divide by Zero
        *lnAvgRat   = TotValu/(UNTCOST*TotQty)
        lnAvgRat   = IIF(TotValu=0,0,TotValu/(UNTCOST*TotQty))
        *B608386,1 NNA (End)
      ELSE
        IF lnAvgRat <> TotValu/(UNTCOST*TotQty)
           lnUntcst = 0        
        ENDIF
        lnAvgRat   = lnAvgRat + TotValu/(UNTCOST*TotQty)
      ENDIF
    ENDSCAN
    
    lnAvgRat  = lnAvgRat/lnContRec
    lnContRec = IIF(lnContRec=0,1,lnContRec)
    *--if diff. costs for the same fabric then calculat avarge
    IF lnUntcst = 0        
      lnAvgCost = (lnTotValu/lnTotCst)/IIF(lnAvgRat>0 ,lnAvgRat ,1)
    ELSE && Else get the same cost
      lnAvgCost = lnUntcst  
    ENDIF
    SELECT (WORKFILE)
    =SEEK(Order + cItmMajor + Typ + Item + IClr + cSeekStyle +  cItmMask + MfgCode ,WORKSTY)
    SELECT (WORKSTY)

    m.mfabcolor =  IClr
    
    REPLACE TotQty     WITH lnCurrTot,;
            nBomTOTQTY WITH lnAvgRat,;
            UntCost    WITH lnAvgCost,;
            TotCost    WITH lnAvgCost * lnAvgRat
    REPLACE cadd_user WITH "*****"
    
    IF !EOF()        
      SKIP     
    ELSE
      REPLACE mFabcolor with m.mFabcolor
    ENDIF   
    SCAN REST WHILE Order + cItmMajor + Typ + Item + IClr + cSeekStyle +  cItmMask + MfgCode  = lcCurOrd+lcCurrSty 
      IF Item <> &WORKFILE..Item
        LOOP
      ENDIF
      m.mfabcolor = m.mfabcolor +' - '+ IClr
      DELETE
    ENDSCAN          
    LOCATE FOR      lcCurOrd  = ORDER AND Item = &WORKFILE..Item 
    REPLACE mFabcolor with m.mFabcolor
ENDSCAN

SELECT (WORKFILE)
DELETE ALL FOR (cCatgTyp $ "F") AND SUBSTR(cItmMask,lnMajorLen+2) <>'******' 
SELECT (WORKSTY)
LOCATE
lcCurOrd  = ''
lcCurrSty = ''
lcCurrFab = ''

SELECT (WORKSTY)
SCAN
  SCATTER MEMVAR MEMO
  IF OCCURS('-',mfabcolor) = 0
    lnOldRat = m.nBomTotQty
    m.nBomTotQty = (m.nBomTotQty * m.TotQty)/ m.TotOrdQty
    m.UntCost  = (ROUND(m.TotValu,3)/m.TotOrdQty)/ROUND(m.nBomTotQty,3)
    
    m.TotCost  = m.UntCost * m.nBomTotQty 
  ENDIF
  INSERT INTO (WORKFILE) FROM MEMVAR
ENDSCAN
SELECT (WORKSTY)
ZAP


SELECT (WORKFILE)

lnContRec = 0
lnDutyAmt = 0
lnTotPQty = 0
SCAN FOR (cCatgTyp $ "T")
  SCATT MEMVAR MEMO  
  IF SEEK(Order + cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + Item + IClr,WORKSTY)
    LOOP
  ENDIF
  IF SUBSTR(cItmMask,lnMajorLen+2) ='******' 
    =lfFabColor()
    REPLACE mFabcolor with m.mFabcolor
  ENDIF
  
  =lfCalTrm()
ENDSCAN

SELECT (WORKTRIM)
SCAN
  REPLACE TOTValu WITH TOTQTY * UntCost
ENDSCAN

SELECT (WORKTRIM)
SCAN
  REPLACE UntCost WITH ROUND(TOTValu/lnTotPQty,3)
  REPLACE TotCost WITH UntCost * nBomTOTQTY
ENDSCAN

SELECT (WORKFILE)
DELETE ALL FOR (cCatgTyp $ "T") 
SELECT (WORKTRIM)
SCAN
  SCATTER MEMVAR MEMO
  INSERT INTO (WORKFILE) FROM MEMVAR
ENDSCAN
SELECT (WORKSTY)
ZAP
SELECT (WORKTRIM)
ZAP


SELECT (WORKFILE)

lnContRec = 0
lnDutyAmt = 0
lnTotPQty = 0
SCAN FOR (cCatgTyp $ "D")
  SCATT MEMVAR MEMO  
  IF SEEK(Order + cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + Item + IClr,WORKSTY)
    LOOP
  ENDIF
  IF SUBSTR(cItmMask,lnMajorLen+2) ='******' 
    =lfFabColor()
    REPLACE mFabcolor with m.mFabcolor
  ENDIF
  
  m.mfabcolor = IClr
  INSERT INTO (WORKTRIM) FROM MEMVAR
  lnTotPQty = 0

  lcOrder = &WORKFILE..Order  
  =SEEK("O"+Order,'ORDCANLN')
  =SEEK("O"+Order,'OrdLine')
  SELECT ORDLINE
  SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + lcOrder
    =SEEK("O"+Order+STR(LINENO,6),'ORDCANLN')
    lnTotPQty = lnTotPQty + (OrdLine.TotBook-OrdCanln.TOtqty)
    SELECT (WORKTRIM)
    IF SUBSTR(cItmMask,lnMajorLen+2) ='******' 
      IF SUBSTR(STYLE,1,lnMajorLen) <> SUBSTR(&WORKFILE..STYLE,1,lnMajorLen)
        LOOP
      ENDIF  
    ELSE
      IF Style <> &WORKFILE..cItmMajor
        LOOP
      ENDIF  
    ENDIF  
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
    
      IF EMPTY(&WORKTRIM..msizes) OR lcCount $ &WORKTRIM..msizes
        REPLACE &WORKTRIM..Qty&lcCount WITH &WORKTRIM..Qty&lcCount + (OrdLine.Book&lcCount-OrdCanln.qty&lcCount)
        REPLACE &WORKTRIM..TotQty WITH &WORKTRIM..TotQty +  (OrdLine.Book&lcCount-OrdCanln.qty&lcCount)
      ENDIF
    ENDFOR
  ENDSCAN

ENDSCAN

SELECT (WORKTRIM)
SCAN
  REPLACE TOTValu WITH TOTQTY * UntCost
ENDSCAN

SELECT (WORKTRIM)
SCAN
  REPLACE UntCost WITH ROUND(TOTValu/lnTotPQty,3)
  REPLACE TotCost WITH UntCost * nBomTOTQTY
ENDSCAN

SELECT (WORKFILE)
DELETE ALL FOR (cCatgTyp $ "D") 
SELECT (WORKTRIM)
lnkCount = 0
SCAN
  IF  lnkCount = 0
    SCATTER MEMVAR MEMO
    INSERT INTO (WORKFILE) FROM MEMVAR
  ELSE
    REPLACE &WORKFILE..TOTValu WITH &WORKFILE..TOTValu+TOTValu  
    REPLACE &WORKFILE..TOTQTY  WITH &WORKFILE..TOTQTY+TOTQTY
  ENDIF  
  lnkCount = lnkCount + 1 
ENDSCAN

SELECT (WORKFILE)
LOCATE FOR (cCatgTyp $ "D") 
REPLACE UntCost WITH ROUND(TOTValu/TOTQTY,3)
REPLACE TotCost WITH UntCost * nBomTOTQTY


SELECT (WORKSTY)
ZAP
SELECT (WORKTRIM)
ZAP

SELECT (WORKFILE)

lnContRec = 0
lnDutyAmt = 0
lnTotPQty = 0
SCAN FOR (cCatgTyp $ "M")
  SCATT MEMVAR MEMO  
  IF SEEK(Order + cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + Item + IClr,WORKSTY)
    LOOP
  ENDIF
  IF SUBSTR(cItmMask,lnMajorLen+2) ='******' 
    =lfFabColor()
    REPLACE mFabcolor with m.mFabcolor
  ENDIF
  
  m.mfabcolor = IClr
  INSERT INTO (WORKTRIM) FROM MEMVAR
  lnTotPQty = 0

  lcOrder = &WORKFILE..Order  
  =SEEK("O"+Order,'ORDCANLN')
  =SEEK("O"+Order,'OrdLine')
  SELECT ORDLINE
  SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + lcOrder
    =SEEK("O"+Order+STR(LINENO,6),'ORDCANLN')
    lnTotPQty = lnTotPQty + (OrdLine.TotBook-OrdCanln.TOtqty)
    SELECT (WORKTRIM)
    IF SUBSTR(cItmMask,lnMajorLen+2) ='******' 
      IF SUBSTR(STYLE,1,lnMajorLen) <> SUBSTR(&WORKFILE..STYLE,1,lnMajorLen)
        LOOP
      ENDIF  
    ELSE
      IF Style <> &WORKFILE..cItmMajor
        LOOP
      ENDIF  
    ENDIF  
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
    
      IF EMPTY(&WORKTRIM..msizes) OR lcCount $ &WORKTRIM..msizes
        REPLACE &WORKTRIM..Qty&lcCount WITH &WORKTRIM..Qty&lcCount + (OrdLine.Book&lcCount-OrdCanln.qty&lcCount)
        REPLACE &WORKTRIM..TotQty WITH &WORKTRIM..TotQty +  (OrdLine.Book&lcCount-OrdCanln.qty&lcCount)
      ENDIF
    ENDFOR
  ENDSCAN

ENDSCAN

SELECT (WORKTRIM)
SCAN
  REPLACE TOTValu WITH TOTQTY * UntCost
ENDSCAN

SELECT (WORKTRIM)
SCAN
  REPLACE UntCost WITH ROUND(TOTValu/lnTotPQty,3)
  REPLACE TotCost WITH UntCost * nBomTOTQTY
ENDSCAN

SELECT (WORKFILE)
DELETE ALL FOR (cCatgTyp $ "M") 
SELECT (WORKTRIM)
lnkCount = 0
SCAN
  IF  lnkCount = 0
    SCATTER MEMVAR MEMO
    INSERT INTO (WORKFILE) FROM MEMVAR
  ELSE
    REPLACE &WORKFILE..TOTValu WITH &WORKFILE..TOTValu+TOTValu  
    REPLACE &WORKFILE..TOTQTY  WITH &WORKFILE..TOTQTY+TOTQTY
  ENDIF  
  lnkCount = lnkCount + 1 
ENDSCAN

SELECT (WORKFILE)
LOCATE FOR (cCatgTyp $ "M") 
REPLACE UntCost WITH ROUND(TOTValu/TOTQTY,3)
REPLACE TotCost WITH UntCost * nBomTOTQTY


SELECT (WORKSTY)
ZAP
SELECT (WORKTRIM)
ZAP

*!*************************************************************
*! Name      : lfFabColor
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
*! Purpose   : Get Fabric Color 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfFabColor()
*!*************************************************************
*
FUNCTION lfFabColor
PRIVATE lcAlias
lcAlias = ALIAS()

lcOrder = &WORKFILE..Order  
=SEEK("O"+Order,'OrdLine')
SELECT ORDLINE
SET ORDER TO TAG ORDLINES
=SEEK(ALLTRIM(&WORKFILE..Style),'OrdLine')
SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6)= ALLTRIM(&WORKFILE..Style)
  IF Order <> lcOrder
    LOOP
  ENDIF  
  IF EMPTY(m.mfabcolor)
    m.mfabcolor =  SUBSTR(Style,lnNonMajSt,lnColorLen)
  ELSE
    m.mfabcolor = m.mfabcolor +' - '+ SUBSTR(Style,lnNonMajSt,lnColorLen)
  ENDIF
ENDSCAN
SELECT ORDLINE
SET ORDER TO TAG ORDLINE

SELECT (lcAlias)

*!*************************************************************
*! Name      : lfCalTrm
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
*! Purpose   : Calculate the trim on size level
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfCalTrm()
*!*************************************************************
*
FUNCTION lfCalTrm

IF !SEEK(&WORKFILE..Order +&WORKFILE..Item,WORKTRIM)
  m.mfabcolor = IClr
  INSERT INTO (WORKTRIM) FROM MEMVAR
ELSE
  m.mfabcolor = m.mfabcolor +' - '+ IClr
  REPLACE mFabcolor with m.mFabcolor
  IF (&WORKTRIM..msizes =&WORKFILE..msizes ) AND (&WORKTRIM..UntCost =&WORKFILE..UntCost )
    RETURN
  ENDIF
ENDIF
lnTotPQty = 0

lcOrder = &WORKFILE..Order  
=SEEK("O"+Order,'ORDCANLN')
=SEEK("O"+Order,'OrdLine')
SELECT ORDLINE
SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + lcOrder
  =SEEK("O"+Order+STR(LINENO,6),'ORDCANLN')
  lnTotPQty = lnTotPQty + (OrdLine.TotBook-OrdCanln.TOtqty)
  SELECT (WORKTRIM)
  IF SUBSTR(cItmMask,lnMajorLen+2) ='******' 
    IF SUBSTR(STYLE,1,lnMajorLen) <> SUBSTR(&WORKFILE..STYLE,1,lnMajorLen)
      LOOP
    ENDIF  
  ELSE
    IF Style <> &WORKFILE..cItmMajor
      LOOP
    ENDIF  
  ENDIF  
  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    
    IF EMPTY(&WORKTRIM..msizes) OR lcCount $ &WORKTRIM..msizes
      REPLACE &WORKTRIM..Qty&lcCount WITH &WORKTRIM..Qty&lcCount + (OrdLine.Book&lcCount-OrdCanln.qty&lcCount)
      REPLACE &WORKTRIM..TotQty WITH &WORKTRIM..TotQty +  (OrdLine.Book&lcCount-OrdCanln.qty&lcCount)
    ENDIF
  ENDFOR
ENDSCAN

*!*************************************************************
*! Name      : lfClCLRTrm
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/6/2004
*! Purpose   : Calculate the trim on size level in case of Colour
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfClCLRTrm()
*!*************************************************************
*
FUNCTION lfClCLRTrm


INSERT INTO (WORKTRIM) FROM MEMVAR

lnTotPQty = 0

lcOrder = &WORKFILE..Order  
=SEEK("O"+Order,'ORDCANLN')
=SEEK("O"+Order,'OrdLine')
SELECT ORDLINE
SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + lcOrder
  IF Style <> &WORKFILE..cSeekStyle 
    LOOP
  ENDIF
  =SEEK("O"+Order+STR(LINENO,6),'ORDCANLN')
  lnTotPQty = lnTotPQty + (OrdLine.TotBook-OrdCanln.TOtqty)
  SELECT (WORKTRIM)
  IF SUBSTR(cItmMask,lnMajorLen+2) ='******' 
    IF SUBSTR(STYLE,1,lnMajorLen) <> SUBSTR(&WORKFILE..STYLE,1,lnMajorLen)
      LOOP
    ENDIF  
  ELSE
    IF Style <> &WORKFILE..cItmMajor
      LOOP
    ENDIF  
  ENDIF  
  REPLACE &WORKTRIM..TotOrdQty WITH lnTotPQty
  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    
    IF EMPTY(&WORKTRIM..msizes) OR lcCount $ &WORKTRIM..msizes
      REPLACE &WORKTRIM..Qty&lcCount WITH &WORKTRIM..Qty&lcCount + (OrdLine.Book&lcCount-OrdCanln.qty&lcCount)
      REPLACE &WORKTRIM..TotQty WITH &WORKTRIM..TotQty +  (OrdLine.Book&lcCount-OrdCanln.qty&lcCount)
    ENDIF
  ENDFOR
ENDSCAN

