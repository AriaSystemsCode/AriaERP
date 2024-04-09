*:************************************************************************
*: Program file  : MAROBMAT.PRG
*: Program desc. : Custom Material Requirements Report for Rob10.
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: MATERIALS (MA).
*:      Developer: AHMED MAHER (AMH)
*:           Date: 11/26/2002
*:************************************************************************
*: Modifications :
*:************************************************************************

*-- Get the needed company setups.
DIMENSION laSetups[4,2]
laSetups[1,1] = 'M_WareHouse'
laSetups[2,1] = 'M_ApLink'
laSetups[3,1] = 'M_Div_Seq'
laSetups[4,1] = 'M_LIMpCost'
=gfGetMemVar(@laSetups,gcAct_Comp)

llMultiWH = (laSetups[1,2] = 'Y')
llApLink  = (laSetups[2,2] = 'Y')
llDivSeq  = (laSetups[3,2] = 'Y')
llDtlCost = laSetups[4,2]

lcVenFile  = 'ApVendor'
lcVenField = 'cVenComp'

FABTEMP = gfTempName()

*** CREATE TEMP FILE TO GATHER INFORMATION ABOUT MATERIALS FOR WHICH THERE
*** IS NEGATIVE REMAINING QUANTITY IF ANY. THIS TEMP FILE WILL CONTAIN :
*** ITEM CODE, ITEM COLOR, LEAD TIME, VENDOR, COMPLETION DATE, QUANTITY
*** NEEDED AND PRICE.

FABTEMP4 = gfTempName()
FABTEMP1 = gfTempName()     && INDEX FILE NAME
FABTEMP2 = gfTempName()     && INDEX FILE NAME
*-- NOTE: All these fields must be initialized by the calling program
*--
IF llDtlCost
  PoNoTmp =  gfTempName()
  CREATE CURSOR (PoNoTmp) (Item C(12),IClr C(6),cPONo C(6))
  INDEX ON Item+IClr+cPONo TAG (PoNoTmp) OF (gcWorkDir+PoNoTmp)
ENDIF
XTITLE = SPACE(30)
lcPAstNot = IIF(llRpPrntNt,'Y','N')
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.COMPLETE'),1)
IF lnPosition > 0
  xCOMPLETE = laOGFxFlt[lnPosition,6]
ENDIF
xPAST      = IIF(llRpPastCm,'Y','N')
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.PRIORITY'),1)
IF lnPosition > 0
  xPRIORITY = laOGFxFlt[lnPosition,6]
ENDIF
xPRJP      = lnRpPrIncr
xPRJB      = lcRpBasePj
*xPRJE      = IIF(llRpEditPj,'Y','N')
xPRJE      = 'N'
xREP       = lcRpCompn
xFMT       = IIF(lcRpRName='S','S','D')
xPRNT_ZERO = IIF(llRpZeroRq,'Y','N')
xNONINV    = IIF(llRpInvItm,'Y','N')
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.START'),1)
IF lnPosition > 0
  lnSepPos  = AT('|',laOGFxFlt[lnPosition,6])
  ldLoStart = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],1,lnSepPos-1))
  ldHiStart = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],lnSepPos+1))
ENDIF
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SUBSTR(Bom.item,1,7)'),1)
IF lnPosition > 0
  lnSepPos = AT('|',laOGFxFlt[lnPosition,6])
  lcFab1   = SUBSTR(laOGFxFlt[lnPosition,6],1,lnSepPos-1)
  lcFab2   = SUBSTR(laOGFxFlt[lnPosition,6],lnSepPos+1)
ENDIF
lcTClr = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'Bom.iclr'),1)
IF lnPosition > 0
  lcTClr = laOGFxFlt[lnPosition,6]
ENDIF
XLS=SPACE(0)
IF XFMT = 'D'
  XLS = IIF(lcRpRName='H','S','L')
ENDIF
XNEG_QTY = lcRpRmChk
lcMaterial = SPACE(7)

*--lcRpExp0  Expresion on Style file.
=ACOPY(laOGFxFlt,laStyFlt, 8,49)
DIME laStyFlt[6,7]
lcRpExp0 = gfGenFlt('laStyFlt',.T.)
lcRpExp0 = IIF(EMPTY(lcRpExp0),".T.",lcRpExp0) 
lcStylFile = ''
llStyRang = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.STYLE'),1)
IF lnPosition > 0
  lcStylFile = laOGFxFlt[lnPosition,6]
  llStyRang = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF
IF llStyRang
  lcRpExp0 = IIF(lcRpExp0='.T.','',lcRpExp0+' .AND. ') +;
             'SEEK(SUBSTR(STYLE.STYLE,1,12),lcStylFile)'
ENDIF
  
IF USED(FABTEMP)
  USE IN (FABTEMP)
ENDIF

DIMENSION laFlSruc[49,4]

laFlSruc[1,1] = "FABRIC"
laFlSruc[1,2] = "C"
laFlSruc[1,3] = 12
laFlSruc[1,4] = 0

laFlSruc[2,1] = "FCLR"
laFlSruc[2,2] = "C"
laFlSruc[2,3] = 6
laFlSruc[2,4] = 0

laFlSruc[3,1] = "DESC"
laFlSruc[3,2] = "C"
laFlSruc[3,3] = 20
laFlSruc[3,4] = 0

laFlSruc[4,1] = "STYLE"
laFlSruc[4,2] = "C"
laFlSruc[4,3] = 12
laFlSruc[4,4] = 0

laFlSruc[5,1] = "SCLR"
laFlSruc[5,2] = "C"
laFlSruc[5,3] = 6
laFlSruc[5,4] = 0

laFlSruc[6,1] = "TYP"
laFlSruc[6,2] = "C"
laFlSruc[6,3] = 1
laFlSruc[6,4] = 0

laFlSruc[7,1] = "CCATGTYP"
laFlSruc[7,2] = "C"
laFlSruc[7,3] = 1
laFlSruc[7,4] = 0

laFlSruc[8,1] = "YARDS"
laFlSruc[8,2] = "N"
laFlSruc[8,3] = 7
laFlSruc[8,4] = 3

laFlSruc[9,1] = "UOM"
laFlSruc[9,2] = "C"
laFlSruc[9,3] = 3
laFlSruc[9,4] = 0

laFlSruc[10,1] = "OREQ1"
laFlSruc[10,2] = "N"
laFlSruc[10,3] = 6
laFlSruc[10,4] = 0

laFlSruc[11,1] = "OREQ2"
laFlSruc[11,2] = "N"
laFlSruc[11,3] = 6
laFlSruc[11,4] = 0

laFlSruc[12,1] = "OREQ3"
laFlSruc[12,2] = "N"
laFlSruc[12,3] = 6
laFlSruc[12,4] = 0

laFlSruc[13,1] = "OREQ4"
laFlSruc[13,2] = "N"
laFlSruc[13,3] = 6
laFlSruc[13,4] = 0

laFlSruc[14,1] = "OREQ5"
laFlSruc[14,2] = "N"
laFlSruc[14,3] = 6
laFlSruc[14,4] = 0

laFlSruc[15,1] = "OREQ6"
laFlSruc[15,2] = "N"
laFlSruc[15,3] = 6
laFlSruc[15,4] = 0

laFlSruc[16,1] = "OREQ7"
laFlSruc[16,2] = "N"
laFlSruc[16,3] = 6
laFlSruc[16,4] = 0

laFlSruc[17,1] = "OREQ8"
laFlSruc[17,2] = "N"
laFlSruc[17,3] = 6
laFlSruc[17,4] = 0

laFlSruc[18,1] = "OREQTOT"
laFlSruc[18,2] = "N"
laFlSruc[18,3] = 7
laFlSruc[18,4] = 0

laFlSruc[19,1] = "OHREQ1"
laFlSruc[19,2] = "N"
laFlSruc[19,3] = 6
laFlSruc[19,4] = 0

laFlSruc[20,1] = "OHREQ2"
laFlSruc[20,2] = "N"
laFlSruc[20,3] = 6
laFlSruc[20,4] = 0

laFlSruc[21,1] = "OHREQ3"
laFlSruc[21,2] = "N"
laFlSruc[21,3] = 6
laFlSruc[21,4] = 0

laFlSruc[22,1] = "OHREQ4"
laFlSruc[22,2] = "N"
laFlSruc[22,3] = 6
laFlSruc[22,4] = 0

laFlSruc[23,1] = "OHREQ5"
laFlSruc[23,2] = "N"
laFlSruc[23,3] = 6
laFlSruc[23,4] = 0

laFlSruc[24,1] = "OHREQ6"
laFlSruc[24,2] = "N"
laFlSruc[24,3] = 6
laFlSruc[24,4] = 0

laFlSruc[25,1] = "OHREQ7"
laFlSruc[25,2] = "N"
laFlSruc[25,3] = 6
laFlSruc[25,4] = 0

laFlSruc[26,1] = "OHREQ8"
laFlSruc[26,2] = "N"
laFlSruc[26,3] = 6
laFlSruc[26,4] = 0

laFlSruc[27,1] = "OHREQTOT"
laFlSruc[27,2] = "N"
laFlSruc[27,3] = 7
laFlSruc[27,4] = 0

laFlSruc[28,1] = "PROJECT"
laFlSruc[28,2] = "N"
laFlSruc[28,3] = 7
laFlSruc[28,4] = 0

laFlSruc[29,1] = "NOOTC1"
laFlSruc[29,2] = "N"
laFlSruc[29,3] = 7
laFlSruc[29,4] = 0

laFlSruc[30,1] = "NOOTC2"
laFlSruc[30,2] = "N"
laFlSruc[30,3] = 7
laFlSruc[30,4] = 0

laFlSruc[31,1] = "NOOTC3"
laFlSruc[31,2] = "N"
laFlSruc[31,3] = 7
laFlSruc[31,4] = 0

laFlSruc[32,1] = "NOOTC4"
laFlSruc[32,2] = "N"
laFlSruc[32,3] = 7
laFlSruc[32,4] = 0

laFlSruc[33,1] = "NOOTC5"
laFlSruc[33,2] = "N"
laFlSruc[33,3] = 7
laFlSruc[33,4] = 0

laFlSruc[34,1] = "NOOTC6"
laFlSruc[34,2] = "N"
laFlSruc[34,3] = 7
laFlSruc[34,4] = 0

laFlSruc[35,1] = "NOOTC7"
laFlSruc[35,2] = "N"
laFlSruc[35,3] = 7
laFlSruc[35,4] = 0

laFlSruc[36,1] = "NOOTC8"
laFlSruc[36,2] = "N"
laFlSruc[36,3] = 7
laFlSruc[36,4] = 0

laFlSruc[37,1] = "NTOOTC"
laFlSruc[37,2] = "N"
laFlSruc[37,3] = 8
laFlSruc[37,4] = 0

laFlSruc[38,1] = "NOHOTC1"
laFlSruc[38,2] = "N"
laFlSruc[38,3] = 7
laFlSruc[38,4] = 0

laFlSruc[39,1] = "NOHOTC2"
laFlSruc[39,2] = "N"
laFlSruc[39,3] = 7
laFlSruc[39,4] = 0

laFlSruc[40,1] = "NOHOTC3"
laFlSruc[40,2] = "N"
laFlSruc[40,3] = 7
laFlSruc[40,4] = 0

laFlSruc[41,1] = "NOHOTC4"
laFlSruc[41,2] = "N"
laFlSruc[41,3] = 7
laFlSruc[41,4] = 0

laFlSruc[42,1] = "NOHOTC5"
laFlSruc[42,2] = "N"
laFlSruc[42,3] = 7
laFlSruc[42,4] = 0

laFlSruc[43,1] = "NOHOTC6"
laFlSruc[43,2] = "N"
laFlSruc[43,3] = 7
laFlSruc[43,4] = 0

laFlSruc[44,1] = "NOHOTC7"
laFlSruc[44,2] = "N"
laFlSruc[44,3] = 7
laFlSruc[44,4] = 0

laFlSruc[45,1] = "NOHOTC8"
laFlSruc[45,2] = "N"
laFlSruc[45,3] = 7
laFlSruc[45,4] = 0

laFlSruc[46,1] = "NTOHOTC"
laFlSruc[46,2] = "N"
laFlSruc[46,3] = 8
laFlSruc[46,4] = 0

laFlSruc[47,1] = "NTOTCPRJ"
laFlSruc[47,2] = "N"
laFlSruc[47,3] = 8
laFlSruc[47,4] = 0

laFlSruc[48,1] = "NYTOWIP"
laFlSruc[48,2] = "N"
laFlSruc[48,3] = 11
laFlSruc[48,4] = 3

laFlSruc[49,1] = "LMAKE"
laFlSruc[49,2] = "L"
laFlSruc[49,3] = 1
laFlSruc[49,4] = 0

CREATE CURSOR (FABTEMP) FROM ARRAY laFlSruc
INDEX ON cCatgTyp+FABRIC+FCLR+STYLE+SCLR TAG &FABTEMP
INDEX ON STYLE+SCLR+cCatgTyp+FABRIC+FCLR TAG &FABTEMP1
INDEX ON STYLE+SCLR TAG &FABTEMP2  UNIQUE

SET ORDER TO TAG &FABTEMP

IF !USED(FABTEMP4)
  CREATE DBF (gcWorkDir+FABTEMP4) ;
   ( Fabric C(7), Color C(6),Vendor C(8),nFabTotQty N(11,3),LeadTime N(3),;
     Complete D(8),UOMBuy C(3),cFabGrade C(1),nfabcost N(9,3),cPriceCur C(3),;
     nPriceRat N(9,4),nCurrUnit N(6),cDutyCur C(3),nDutyRat N(9,4),nDCurUnit N(6),;
     nItm_Frt N(9,3),nItem_Tax N(9,3), nItemQuota N(9,3), cWareCode C(6) )
  INDEX ON FABRIC+COLOR TAG &FABTEMP4
  USE IN (FABTEMP4)
  =gfOpenFile(gcWorkDir+FABTEMP4,'','EX')
  SET ORDER TO TAG &FABTEMP4
ENDIF

*-This part to cover case of imported
*-styles used, by create a temp file with all imported styles becouse we 
*-can not take them direct from style file becouse if we have a fabric 
*-used in two styles and there is a P/O for them and in the grid we edit only
*-one of these styles the filter on style file see only one but actualy
*-we have to take both styles since we can not get the issued qty for
*-each style but we get it for a P/O level.
*-This temp file prepared to build the main report temp file.

IF llDtlCost
  TmpSty = gfTempName()
  CREATE CURSOR (TmpSty) (Style C(12),Color C(6),Item C(12))
  INDEX ON Style+Color+Item TAG (TmpSty) OF (gcWorkDir+TmpSty)
ENDIF  

SELECT STYLE
lcSeprator = ''
SCAN FOR &lcRpExp0
  llMake = Make
  xStyle = SUBSTR(Style,1,12)
  xSclr  = SUBSTR(STYLE,14)
  lcSeprator = SUBSTR(STYLE,13,1)
  WAIT WINDOW 'Style/Color : '+xStyle+'/'+xSclr NOWAIT
  
  IF (!llDtlCost AND !llMake) OR !SEEK(xStyle,'BOM')
    LOOP
  ENDIF
  
  lnRecNo = RECNO("STYLE")
  SELECT BOM
  SCAN WHILE CITMMAJOR = xStyle FOR !(cCatgTyp $ 'MPD')
    =lfBildTmp()
    
    IF llDtlCost AND ! llMake
      lnSvBomRc=RECNO()
      lcTyp=Typ
      lcFab=Item
      SET ORDER TO TAG BOMITEM
      =SEEK(lcTyp+lcFab) 
      SCAN WHILE Typ+Item = lcTyp+lcFab
        lcBomStCl = SUBSTR(BOM.CITMMAJOR,1,12) + xSclr
        IF lcBomStCl <> xStyle + xSClr AND ;
           !SEEK(lcBomStCl+ lcFab,TmpSty) AND ;
           !SEEK(lcBomStCl,'STYLE') 
          INSERT INTO (TmpSty) (Style,Color,Item) VALUES;
                      (SUBSTR(BOM.CITMMAJOR,1,12),xSclr,lcFab)
        ENDIF
      ENDSCAN
      
      SELECT BOM
      SET ORDER TO TAG BOM
      GOTO lnSvBomRc
    ENDIF
    
  ENDSCAN
  GO lnRecNo IN STYLE
  
ENDSCAN

*--Loop to create an Imported styles (out of filter) records.
WAIT CLEAR
IF llDtlCost
  SELECT STYLE
  SET FILTER TO
  SELECT (TmpSty)
  SCAN
    xStyle=Style
    xSclr =Color 
    xFab  =Item
    IF !SEEK(xStyle+lcSeprator+xSclr,'STYLE') OR STYLE.Make
      LOOP
    ENDIF
    llMake=.F.
    =SEEK(xStyle,'BOM')
    SELECT BOM
    SCAN WHILE CITMMAJOR = xStyle FOR Item=xFab AND !(cCatgTyp $ 'MPD') 
      =lfBildTmp()
    ENDSCAN
  ENDSCAN
  USE IN (TmpSty)
ENDIF

SELECT (FABTEMP)
SET ORDER TO TAG &FABTEMP
LOCATE
IF EOF()
  *--No cost sheet information found for any of the selected
  *--styles or fabrics.
  *--                           [Ok]
  =gfModalGen('TRM36107B36000','DIALOG','')
  RETURN
ENDIF

********** NOW GO OFF THE FABTEMP AND CALCULATE THE OPEN AND OPEN+HOLD ORDER
SELECT ORDLINE
SET RELATION TO 'O'+ORDER INTO ORDHDR
SELECT CUTTKTL
SET RELATION TO CUTTKT INTO CUTTKTH
SELECT POSLN
SET RELATION TO 'P'+PO INTO POSHDR

XOFILTER = "ORDHDR.STATUS<>'X'"
IF DTOC(XCOMPLETE) <> ' '
  XOFILTER=XOFILTER + '.AND. ORDHDR.COMPLETE<=XCOMPLETE'
ENDIF
DO CASE
  CASE EMPTY(ldLoStart) .AND. !EMPTY(ldHiStart)
    XOFILTER = XOFILTER + '.AND. ORDHDR.START <= ldHiStart'
  CASE !EMPTY(ldLoStart) .AND. EMPTY(ldHiStart)
    XOFILTER = XOFILTER + '.AND. ORDHDR.START >= ldLoStart'
  CASE !EMPTY(ldLoStart) .AND. !EMPTY(ldHiStart)
    XOFILTER = XOFILTER + '.AND. BETWEEN (ORDHDR.START,ldLoStart,ldHiStart)'
ENDCASE
IF XPAST='Y' 
  XOFILTER=XOFILTER + '.AND. ORDHDR.COMPLETE>=gdSysDate'
ENDIF
IF !EMPTY(XPRIORITY)
  XOFILTER=XOFILTER + '.AND. ORDHDR.PRIORITY=XPRIORITY'
ENDIF

SELECT (FABTEMP)
SET ORDER TO TAG &FABTEMP1
LOCATE

DIMENSION XOORD(8),XOHORD(8),XOREQ(8),XOHREQ(8),XWIP(8),XWIP2(8)
DIMENSION xWIPo(8),xWIPh(8)
DIMENSION xWIPOTC(8),xOTCORQ(8),xOTCOHRQ(8)

****** NOW GO THROUGH ALL THE TEMP FILE *****
lcStyChKey = SPACE(18)

DO WHILE !EOF()
  llStyChged = ( Style+Sclr <> lcStyChKey )
  lcStyChKey = Style+Sclr
  XFABTREC = RECNO()
  
  IF llStyChged
    STORE 0 TO XOORD     && TOTALS FOR OPEN ORDERS
    STORE 0 TO XOHORD
    STORE 0 TO XOTOT     && TOTALS FOR OPEN + HOLD ORDERS
    STORE 0 TO XOHTOT
    
    SELECT ORDLINE
    SEEK &FABTEMP..STYLE+lcSeprator+&FABTEMP..SCLR
    SCAN FOR &XOFILTER WHILE STYLE = &FABTEMP..STYLE+lcSeprator+&FABTEMP..SCLR
      IF ORDHDR.STATUS = 'O'
        Z = 1
        DO WHILE Z <= 8
          X = STR(Z,1)
          XOORD(Z) = XOORD(Z) + ORDLINE.QTY&X
          XOTOT    = XOTOT    + XOORD(Z)
          Z = Z + 1
        ENDDO
      ENDIF
      IF ORDHDR.STATUS $ 'OH'
        Z=1
        DO WHILE Z<=8
          X=STR(Z,1)
          XOHORD(Z)=XOHORD(Z)+ORDLINE.QTY&X
          XOHTOT=XOHTOT+XOHORD(Z)
          Z=Z+1
        ENDDO
      ENDIF
    ENDSCAN
    ******** NOW COMPUTE THE OPEN TO CUT ********
    STORE 0 TO XOREQ        && OPEN REQUIREMETS
    STORE 0 TO XOHREQ       && OPEN + HOLD REQUIREMENTS
    STORE 0 TO XPREQTOT     && TOTALS FOR PLAN REQUIREMENTS
    STORE 0 TO XOREQTOT     && TOTALS FOR OPEN REQUIREMENTS
    STORE 0 TO XOHREQTOT    && TOTALS FOR OPEN + HOLD REQUIREMENTS
    STORE 0 TO XPROJECT     && PRJOECTED PIECES
    STORE 0 TO XSTK         && STOCK ON HAND AND WIP
    STORE 0 TO XPLAN        && PLAN
    STORE 0 TO XPIECES      && TEMPORARY VARIABLE USED TO HOLD TOT REQ PIECES
    
  ENDIF
  
  STORE 0 TO XWIP2,XWIP   && WORK IN PROSESS.
  STORE 0 TO XWIPOTC      && OTC WIP 'O' & 'A' C/T
  STORE 0 TO XWIPO,XWIPH  && OPEN & HOLD WIP
  STORE 0 TO XOTCORQ,XOTCOHRQ
  STORE 0 TO XOTCRQT,XOTCRQHT,XPROJOTC
  
  SELECT STYLE
  IF SEEK(&FABTEMP..STYLE+lcSeprator+&FABTEMP..SCLR)
    
    *-- COMPUTE WIP FROM C/T SO THAT WE DO NOT
    *-- INCLUDE C/T THAT HAVE STATUS 'H'.
    
    DO MAT935                 && IT CALCULATES THE WIP FROM C/T FILE.
    
    IF llStyChged
      SELECT STYLE
      Z=1
      DO WHILE Z <= 8
        X =STR(Z,1)
        XSTK    = IIF(STK&X > 0, STK&X, 0)
        
        *-- PLAN.
        XPLAN    = IIF(PLAN&X > 0, PLAN&X, 0)
        XPREQTOT = XPREQTOT + XPLAN
        
        *-- OPEN.
        xPieces  = MAX( (xOOrd(Z)-xStk-xWIP(Z)) , (xWIPO(Z)+xWIPH(Z)) )
        xOReq(Z) = xPieces
        XOREQTOT= XOREQTOT + XOREQ(Z)
        XPIECES  = XSTK + XWIPOTC(Z) - XOORD(Z)
        XOTCORQ(Z)= IIF(XPIECES > 0, 0, ABS(XPIECES))
        XOTCRQT= XOTCRQT + XOTCORQ(Z)
        
        *-- OPEN + HOLD
        xPieces   = MAX( (xOHOrd(Z)-xStk-xWIP(Z)) , (xWIPO(Z)+xWIPH(Z)) )
        xOHReq(Z) = xPieces
        XOHREQTOT = XOHREQTOT + XOHREQ(Z)
        XPIECES  = XSTK + XWIPOTC(Z) - XOHORD(Z)
        XOTCOHRQ(Z)= IIF(XPIECES > 0, 0, ABS(XPIECES))
        XOTCRQHT= XOTCRQHT + XOTCOHRQ(Z)
        Z=Z+1
      ENDDO
    ENDIF
  ENDIF
  
  IF llStyChged
    *-- NOW COMPUTE THE PROJECTION
    DO CASE
      CASE XPRJB='O'
      XPROJECT = XOREQTOT + ROUND(XOREQTOT * (XPRJP/100),0)
      XPROJOTC = XOTCRQT + ROUND(XOTCRQT * (XPRJP/100),0)
      CASE XPRJB='P'
        XPROJECT = XPREQTOT
        XPROJOTC = XPREQTOT
      CASE XPRJB='H'
        XPROJECT = XOHREQTOT + ROUND(XOHREQTOT * (XPRJP/100),0)
        XPROJOTC = XOTCRQHT + ROUND(XOTCRQHT * (XPRJP/100),0)
    ENDCASE
    
    *-- NOW REPLACE THE REQUIREMENT QTY IN FABTEMP FOR THIS STYLE+COLOR
    SELECT (FABTEMP)
    SET ORDER TO TAG &FABTEMP1
    
    XKEY=STYLE+SCLR
    =SEEK(XKEY)
    SCAN WHILE (STYLE+SCLR)=XKEY
      Z=1
      DO WHILE Z<=8
        X=STR(Z,1)
        REPL OREQ&X  WITH XOREQ(Z),;
             OHREQ&X WITH XOHREQ(Z)
        REPLACE nOOtc&X  WITH XOTCORQ(Z),;
                nOHOtc&X WITH XOTCOHRQ(Z)
        Z=Z+1
      ENDDO
      REPL OREQTOT WITH XOREQTOT,;
           OHREQTOT WITH XOHREQTOT,;
           PROJECT WITH XPROJECT
      
      SELECT (FABTEMP)
      REPLACE nTOOtc   WITH XOTCRQT ,;
              nTOHOtc  WITH XOTCRQHT,;
              nTOtcPrj WITH XPROJOTC    
    ENDSCAN
  ENDIF
  
  SELECT (FABTEMP)
  SET ORDER TO TAG &FABTEMP1
  GOTO XFABTREC
  SKIP
ENDDO

SELECT (FABTEMP)
GO TOP
*-- ONLY THE STYLE/COLORS THAT HAVE SOME REQUIREMENTS
*-- FILTER SET ON ORDER+HOLD TOTAL REQUIREMET
IF XPRNT_ZERO='N'
  SET FILTER TO (OHREQTOT<>0)
ENDIF

LOCATE
IF EOF()
  =gfModalGen('TRM36108B36000','DIALOG')
  RETURN
ELSE
  *-- IF EDIT PROJECTION IS REQUIRED
  IF XPRJE='Y'
    =lfEditProj()
  ENDIF
ENDIF

*-- START PRINTING THE REPORT HERE

****** RELEASE THE FILTER ON THE STYLE FILE BECAUSE STYLE COMPONENTS REPORT
****** NEEDS TO SEEK THE RECORD IN THE STYLE FILE TO GET THE DESC. AND STOCK
SELECT STYLE
SET FILTER TO
R_WIDTH = 'W'    && STANDARD REPORT IS 'WIDE'

DO WHILE .T.
  *-- IF THE TEMP FILE IS OPENED ZAP ALL RECORD TO BE FILLED AGAIN.
  IF USED(FABTEMP4)
    SELECT (FABTEMP4)
    ZAP
  ENDIF
  XREPORT = 'MAROBMAT'
  IF XREP$'FA'
    XREPORT = 'MAROBMATF'
    R_TITLE = 'MATERIAL REQUIREMENTS REPORT'
    SELECT (FABTEMP)
    SET ORDER TO TAG &FABTEMP
    IF SEEK('F')                && SEARCH FOR TYP=1
      DO MAT932
    ENDIF
  ENDIF
  IF XREP$'TA'
    XREPORT = 'MAROBMATT'
    R_TITLE = 'TRIM REQUIREMENTS REPORT'
    SELECT (FABTEMP)
    SET ORDER TO TAG &FABTEMP
    IF SEEK('T')                && SEARCH FOR TYP=1
      DO MAT933
    ENDIF
  ENDIF
  IF XREP$'SA'
    XREPORT = 'MAROBMATS'
    R_TITLE = 'STYLE COMPONENT REQUIREMENTS REPORT'
    SELECT (FABTEMP)
    SET ORDER TO TAG &FABTEMP
    IF SEEK('S')             && SEARCH FOR TYP=4
      DO MAT934
    ENDIF
  ENDIF
  WAIT CLEAR
  lcPrinter = SET('PRINTER',1)
  DO ENDREPORT
  SET DEVICE TO SCREEN
  *--Do you wish to reprint the report with edited projections.
  *--                        [ Yes / No ] 
  lnChoice=gfModalGen('TRM36109B36001','DIALOG')
  
  IF lnChoice = 1
    *--Edit Projection.
    =lfEditProj()


    *--Proceed with printing report ?
    *--         [ Yes / No ] 
    lnChoice=gfModalGen('TRM36110B36001','DIALOG')
    IF lnChoice = 1
      SET PRINTER TO (lcPrinter)
    ENDIF
  ENDIF

  IF lnChoice = 2
    *--Ceck if there is negative remainig in some materials.
    GO TOP IN (FABTEMP4)
    IF !EOF(FABTEMP4)
      *--Negative remaining quantity on some materials. Create P/Os ?
      *--                        [ Yes / No ] 
      lnChoice=gfModalGen('QRM36111B36001','DIALOG')
      IF lnChoice = 1
        wait window 'Generate P/Os...' NOWAIT
        IF lfGenMPo()
          RETURN
        ENDIF
      ENDIF
    ENDIF
    USE IN (FABTEMP4)
    ERASE (gcWorkDir+FABTEMP4+'.DBF')
    ERASE (gcWorkDir+FABTEMP4+'.CDX')
    EXIT
  ENDIF
ENDDO
RETURN
*--End....

*!******************************************************************
*!  Function : lfBildTmp
*! Purpose   : To build report temp file from cost sheet(BOM).
*!******************************************************************
FUNCTION lfBildTmp

SELECT BOM
DO CASE
  CASE cCatgTyp = 'F'
    IF XREP $ 'FA'          && FABRIC NOT ASKED FOR SO SKIP OVER
      IF ! EMPTY(lcFab1)
        IF !BETWEEN(ITEM,lcFab1,lcFab2)
          RETURN
        ENDIF
      ENDIF
    ELSE
      RETURN
    ENDIF
  CASE cCatgTyp = 'T'          && TRIM NOT ASKED FOR SO SKIP OVER
    IF (!XREP$'TA') .OR. ((XNONINV='N') .AND. (!TRIM_INVT))
      RETURN
    ENDIF
  
    IF XREP $ 'TA' AND !EMPTY(lcFab1) AND !BETWEEN(ITEM,lcFab1,lcFab2)
      RETURN
    ENDIF
  CASE cCatgTyp = 'S' .AND. !XREP$'SA'    && STYLE COMPONENT NOT ASKED FOR
    RETURN
ENDCASE

IF SUBSTR(CITMMASK,14) <> '******' .AND. (SUBSTR(CITMMASK,14) <> xSclr)
  RETURN
ENDIF

XICLR = IIF(IIF(cCatgTyp='S',SUBSTR(ITEM,14),ICLR) = '******', xSclr,;
            IIF(cCatgTyp='S',SUBSTR(ITEM,14),ICLR))
IF cCatgTyp $ 'FT'
  IF ( !EMPTY(lcTClr) .AND. ! (XICLR $ lcTClr) )
    RETURN
  ENDIF
ENDIF

*-- NOW UPDATE THE FABTEMP FILE
SELECT (FABTEMP)
IF SEEK(BOM.cCatgTyp+SUBSTR(BOM.Item,1,12)+xIClr+xStyle+xSclr)
  REPLACE YARDS WITH YARDS+BOM.NBOMTOTQTY
ELSE
  APPEND BLANK
  REPLACE TYP    WITH BOM.TYP,;
          FABRIC WITH BOM.ITEM,;
          FCLR   WITH XICLR,;
          DESC   WITH BOM.DESC,;
          STYLE  WITH BOM.CITMMAJOR,;
          SCLR   WITH xSclr,;
          YARDS  WITH BOM.NBOMTOTQTY,;
          UOM    WITH BOM.UOM,;
          cCatgTyp WITH BOM.cCatgTyp,;
          lMake   WITH llMake
ENDIF
SELECT BOM
RETURN
  
*!**************************************************************************
*! PROG: MAT932
*! DATE: 11/27/2002
*! DESC: PRINT FABRIC REQUIREMENTS
*!**************************************************************************
PROCEDURE MAT932

*SUMMURY
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*MAT930                 MATERIAL REQUIREMENTS REPORT     - 123456789012345678901234567890                           PAGE: 1234
*MM/DD/YY
*
* FABRIC  COLOR  CLR DESCRIPTION FABRIC DESCRIPTION     ONHAND  ONORDER    AVAIL                      PIECES REQUIRED YRD  REMAIN YRD
* XXXXXXX XXXXXX XXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXX 99999999 99999999 99999999        OPEN ORDERS: 9999999     99999999    99999999
*                                                                                OPEN + HOLD ORDERS: 9999999     99999999    99999999
*                                                                                         PROJECTED: 9999999     99999999    99999999
*-----------------------------------------------------------------------------------------------------------------------------------

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*DETAIL
*********************************************  DETAIL REPORT  LONG FORM *************************************************************
*
* FABRIC  COLOR  COLOR DESCRIPTION    FABRIC DESCRIPTION     ONHAND  ONORDER    AVAIL
* STYLE        DESCRIPTION
* XXXXXXX XXXXXX XXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXX 99999999 99999999 99999999
* XXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXX    YIELD : 999.999
* ........................... OPEN .............................  ..................... OPEN + HOLD ...................... PROJECTED
*          XXX   XXX   XXX   XXX   XXX   XXX   XXX   XXX   TOTAL  99999 99999 99999 99999 99999 99999 99999 99999  9999999   9999999
* XXXXXX 99999 99999 99999 99999 99999 99999 99999 99999 9999999  99999 99999 99999 99999 99999 99999 99999 99999  9999999   9999999
* XXXXXX 99999 99999 99999 99999 99999 99999 99999 99999 9999999  99999 99999 99999 99999 99999 99999 99999 99999  9999999   9999999
*                 TOTAL FABRIC YARDAGE REQUIRED ===>     99999999                                                     99999999  99999999
*
*                                                                                             PIECES    REQUIRED YRD   REMAINING YRD
*                                                                           OPEN ORDERS  ==>  9999999       99999999        99999999
*                                                                    OPEN + HOLD ORDERS  ==>  9999999       99999999        99999999
*                                                                             PROJECTED  ==>  9999999       99999999        99999999
*
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

*********************************************  DETAIL REPORT  SHORT FORM *************************************************************
*
* FABRIC   COLOR  COLOR DESCRIPTION    FABRIC DESCRIPTION         ONHAND                 ONORDER               AVAILABLE
* XXXXXXX  XXXXXX XXXXXXXXXXXXXXX      XXXXXXXXXXXXXXXXXXXX     99999999                99999999                99999999
* STYLE        COLOR  DESCRIPTION              YIELD   PCS.....OPEN......YRD   PCS...OPEN+HOLD...YRD   PCS...PROJECTED...YRD
* XXXXXXXXXXXX XXXXXX  XXXXXXXXXXXXXXXXXXXXX  999.999   9999999      99999999   9999999      99999999   9999999      99999999
*              XXXXXX  XXXXXXXXXXXXXXXXXXXXX  999.999   9999999      99999999   9999999      99999999   9999999      99999999
*              XXXXXX  XXXXXXXXXXXXXXXXXXXXX  999.999   9999999      99999999   9999999      99999999   9999999      99999999
* XXXXXXXXXXXX XXXXXX  XXXXXXXXXXXXXXXXXXXXX  999.999   9999999      99999999   9999999      99999999   9999999      99999999
*              XXXXXX  XXXXXXXXXXXXXXXXXXXXX  999.999   9999999      99999999   9999999      99999999   9999999      99999999
*
*                                                                                 PIECES   REQUIRED YRD   REMAINING YRD
*                                                              OPEN ORDERS  ==>  9999999       99999999        99999999
*                                                       OPEN + HOLD ORDERS  ==>  9999999       99999999        99999999
*                                                                PROJECTED  ==>  9999999       99999999        99999999
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

CLEAR TYPEAHEAD
SET DEVICE TO PRINT

STORE 0 TO XOPIECE,XOHPIECE,XPPIECE   &&Total material pieces.
STORE 0 TO XOYRD  ,XOHYRD  ,XPYRD     &&Total material yardage.
STORE 0 TO XOYSTY ,XOHYSTY ,XPYSTY    &&Total style material yardage.
STORE 0 TO lnCTUsdQy,lnPOUsdQy,lnPONetRq,lnCTNetRq
STORE 0 TO lnORemain,lnHRemain,lnPRemain
STORE 0 to lnOpnPOs,lnOpnCTs
lcOldSty=" "
llFrstPo=.T.

PAGENO  = 0
ROW     = 99
XTIME   = TIME()

BREAK  = 'cCatgTyp+FABRIC+FCLR'
XPRV_KEY= SPACE(0)         && USED TO TRIGGER WHEN FABRIC+COLOR IS TO BE WRITTEN
XPRV_TEMP=SPACE(0)
XP_STY_KEY=SPACE(0)        && USED TO TRIGGER WHEN STYLE+COLOR CHANGES
SELECT (FABTEMP)
SET ORDER TO TAG &FABTEMP

**** GET ONHAND,ONORDER,AVAIL ****
SELECT FABRIC
IF SEEK( SUBSTR(&FABTEMP..FABRIC,1,7)+&FABTEMP..FCLR )
  XONHAND=ONHAND
  lnMatRet = lfMatRetrn(Fabric,Color)
  XONORDER=ONORDER+lnMatRet
  XAVAIL = OnHand+OnOrder
  XFABDESC=DESC
  XCONV=CONV
  XFVENDOR=VENDOR
ELSE
  XONHAND=0
  XONORDER=0
  XAVAIL=0
  XFABDESC=SPACE(1)
  XCONV=1
  XFVENDOR=SPACE(1)
ENDIF

SELECT (FABTEMP)
lcUomUse   = IIF(EMPTY(UOM),'USE',UOM)

IF LEN(TRIM(BREAK)) <>0
  HBREAK = &BREAK
ENDIF

DO WHILE INKEY() <> 32
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  IF ROW >=53
     DO lpHeaders
  ENDIF
  
  DO WHILE LEN(TRIM(BREAK)) <>0
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    
    lnPOUsdQy = lnPONetRq
    lnCTUsdQy = lnCTNetRq
    lnORemain = XOYRD+(lnCTNetRq+lnPONetRq)
    lnHRemain = XOHYRD+(lnCTNetRq+lnPONetRq)
    lnPRemain = XPYRD+(lnCTNetRq+lnPONetRq)       
    IF XPRJB='P'
      lnPRemain = XPYRD
    ENDIF
    
    SELECT (FABTEMP)
    DO CASE
      CASE XFMT='D' .AND. XLS='L'      && DETAIL REPORT SO SKIP 2 LINES BEFORE PRINTING TOTALS
        ****** PRINT THE TOTAL YARDAGE FOR THE LAST STYLE ON THIS FABRIC/CLR ****
        ROW=ROW+1
        
        @ ROW,017 SAY 'TOTAL FABRIC '+lcUomUse+'     REQUIRED ===>'
        @ ROW,055 SAY CEILING(XOYSTY) PICTURE '99999999'
        @ ROW,113 SAY CEILING(XOHYSTY) PICTURE '99999999'
        @ ROW,123 SAY CEILING(XPYSTY) PICTURE '99999999'
        ROW = ROW + 1 
        DO lpPrnt_Note WITH .F.
        
        STORE 0 TO XOYSTY,XOHYSTY,XPYSTY
        **************************************************************************
        ROW=ROW+2
        
        @ ROW,82 SAY ' OTC      OTC TOTAL    NET REQUIRED'
        ROW=ROW+1
        lcIsuPrt='  FOR OPEN C/T'
        @ ROW,82 SAY 'PIECES   REQUIRED '+ lcUomUse+lcIsuPrt+' REMAINING '+lcUomUse
        ROW=ROW+1
        
      CASE XFMT='D' .AND. XLS='S'
        ROW = ROW+1
        DO lpPrnt_Note WITH .F.
        ROW=ROW+1
        @ ROW,81 SAY ' OTC      OTC TOTAL    NET REQUIRED'
        ROW=ROW+1
        lcIsuPrt='  FOR OPEN C/T '
        @ ROW,81 SAY 'PIECES   REQUIRED '+ lcUomUse+lcIsuPrt+' REMAINING '+lcUomUse
        ROW=ROW+1
        
    ENDCASE
    
    IF XLS='S'         && DETAIL SHORT FORM
      @ ROW,062 SAY 'OPEN ORDERS ==>'
      @ ROW,080 SAY XOPIECE PICTURE '9999999'
      @ ROW,094 SAY CEILING(XOYRD) PICTURE '99999999'
      @ ROW,106 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      @ ROW,123 SAY ROUND(xAvail-lnORemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,055 SAY 'OPEN + HOLD ORDERS ==>'
      @ ROW,080 SAY XOHPIECE PICTURE '9999999'
      @ ROW,094 SAY CEILING(XOHYRD) PICTURE '99999999'
      @ ROW,106 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      @ ROW,123 SAY ROUND(xAvail-lnHRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,064 SAY 'PROJECTED ==>'
      @ ROW,080 SAY XPPIECE PICTURE '9999999'
      @ ROW,094 SAY CEILING(XPYRD) PICTURE '99999999'
      IF XPRJB='P'
        @ ROW,106 SAY 0 PICTURE '999999999'       
      ELSE
        @ ROW,106 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      ENDIF
      @ ROW,123 SAY ROUND(xAvail-lnPRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW=ROW+1
    ENDIF
    
    IF XFMT='S'         && SUMMARY FORM
      ROW=ROW+1
      DO lpPrnt_Note WITH .F.
      @ ROW,82  SAY 'ORDERS [O]:'       
      @ ROW,94 SAY XOPIECE PICTURE '9999999'
      @ ROW,102 SAY CEILING(XOYRD) PICTURE '99999999'
      @ ROW,111 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      @ ROW,124 SAY ROUND(xAvail-lnORemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,80  SAY 'ORDERS [O+H]:'
      @ ROW,94  SAY XOHPIECE PICTURE '9999999'
      @ ROW,102 SAY CEILING(XOHYRD) PICTURE '99999999'
      @ ROW,111 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      @ ROW,124 SAY ROUND(xAvail-lnHRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,83  SAY 'PROJECTED:'
      @ ROW,94  SAY XPPIECE PICTURE '9999999'
      @ ROW,102 SAY CEILING(XPYRD) PICTURE '99999999'
      IF XPRJB='P'
        @ ROW,111 SAY 0 PICTURE '999999999'       
      ELSE
        @ ROW,111 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      ENDIF
      @ ROW,124 SAY ROUND(xAvail-lnPRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW=ROW+1
    ENDIF
    
    IF XLS='L'          && DETAIL LONG FORM
      @ ROW,63  SAY 'OPEN ORDERS ==>'
      @ ROW,81  SAY XOPIECE PICTURE '9999999'
      @ ROW,95  SAY CEILING(XOYRD) PICTURE '99999999'
      @ ROW,106 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      @ ROW,123 SAY ROUND(xAvail-lnORemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,56  SAY 'OPEN + HOLD ORDERS ==>'
      @ ROW,81  SAY XOHPIECE PICTURE '9999999'
      @ ROW,95 SAY CEILING(XOHYRD) PICTURE '99999999'
      @ ROW,106 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      @ ROW,123 SAY ROUND(xAvail-lnHRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,65  SAY 'PROJECTED ==>'
      @ ROW,81  SAY XPPIECE PICTURE '9999999'
      @ ROW,95 SAY CEILING(XPYRD) PICTURE '99999999'
      IF XPRJB='P'
        @ ROW,106 SAY 0 PICTURE '999999999'       
      ELSE
        @ ROW,106 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      ENDIF
      @ ROW,123 SAY ROUND(xAvail-lnPRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW=ROW+1
    ENDIF
    
    *** GATHER MATERIALS FOR WHICH THERE IS NEGATIVE REMAINING QUANTITY ***
    DO CASE
      CASE XNEG_QTY ='O' .AND. ROUND(XAVAIL-lnORemain,0) < 0
        xRequired = ABS(xAvail - lnORemain)
        DO MAT939
      CASE XNEG_QTY ='H' .AND. ROUND(XAVAIL-lnHRemain,0) < 0
        xRequired = ABS(xAvail - lnHRemain)
        DO MAT939
      CASE XNEG_QTY ='P' .AND. ROUND(XAVAIL-lnPRemain,0) < 0
        xRequired = ABS(xAvail - lnPRemain)
        DO MAT939
    ENDCASE
    SELECT (FABTEMP)
    
    XOPIECE =0
    XOHPIECE=0
    XPPIECE = 0
    XOYRD   =0
    XOHYRD  =0
    XPYRD   =0
    XOYSTY  =0         && OPEN YARDAGE FOR STYLE
    XOHYSTY =0         && OPEN HOLD YARDAGE FOR STYLE
    XPYSTY  =0         && PROJECTED YARDAGE FOR STYLE
    STORE 0 TO lnCTUsdQy,lnPOUsdQy,lnPONetRq,lnCTNetRq
    STORE 0 TO lnORemain,lnHRemain,lnPRemain
    STORE 0 to lnOpnPOs,lnOpnCTs
    lcOldSty=" "
    llFrstPo=.T.
    HBREAK=&BREAK
    EXIT
  ENDDO
  
  IF EOF() .OR. cCatgTyp<>'F'
    EXIT
  ENDIF
  
  IF ROW >=52
    ROW = 99
    LOOP
  ENDIF
  
  IF XPRV_KEY<>FABRIC+FCLR
    **** GET ONHAND,ONORDER,AVAIL ****
    SELECT FABRIC
    IF SEEK( SUBSTR(&FABTEMP..FABRIC,1,7)+&FABTEMP..FCLR )
      XONHAND=ONHAND
      lnMatRet = lfMatRetrn(Fabric,Color)
      XONORDER=ONORDER+lnMatRet
      XAVAIL   = OnHand+OnOrder
      XFABDESC=DESC
      XCONV=CONV        && SAVE CONVERT FACTOR FOR THIS FABRIC
      XFVENDOR=VENDOR   && SAVE VENDOR FOR THIS FABRIC
    ELSE
      XONHAND=0
      XONORDER=0
      XAVAIL=0
      XFABDESC=SPACE(1)
      XCONV=1
      XFVENDOR=SPACE(1)
    ENDIF
    
    SELECT (FABTEMP)
    ***** WRITE ******
    
    IF XLS='S' && DETAIL SHORT FORM
      @ ROW,01 SAY ALLTRIM(FABRIC)
      @ ROW,10 SAY FCLR
      @ ROW,17 SAY SUBSTR(gfCodDes(FCLR , 'COLOR'),1,15)
      @ ROW,38 SAY XFABDESC
      @ ROW,62 SAY XONHAND PICTURE '99999999'
      @ ROW,86 SAY XONORDER PICTURE '99999999'
      @ ROW,110 SAY lnMatRet PICTURE '99999999'
      @ ROW,124 SAY XAVAIL PICTURE '99999999'
    ENDIF
    
    IF XFMT = 'S'    &&SUMMARY FORM
      @ ROW,01 SAY ALLTRIM(FABRIC)
      @ ROW,09 SAY FCLR
      @ ROW,16 SAY SUBSTR(gfCodDes(FCLR , 'COLOR'),1,15)
      @ ROW,32 SAY XFABDESC
      @ ROW,53 SAY XONHAND PICTURE '99999999'
      @ ROW,62 SAY XONORDER PICTURE '99999999'
      @ ROW,71 SAY lnMatRet PICTURE '99999999'
      @ ROW,84 SAY XAVAIL   PICTURE '99999999'
    ENDIF
    
    IF XLS='L' && DETAIL LONG FORM
      @ ROW,01 SAY ALLTRIM(FABRIC)
      @ ROW,09 SAY FCLR
      @ ROW,16 SAY SUBSTR(gfCodDes(FCLR , 'COLOR'),1,15)
      @ ROW,37 SAY XFABDESC
      @ ROW,58 SAY XONHAND PICTURE '99999999'
      @ ROW,67 SAY XONORDER PICTURE '99999999'
      @ ROW,80 SAY lnMatRet PICTURE '99999999'
      @ ROW,89 SAY XAVAIL PICTURE '99999999'
    ENDIF
    
    XPRV_KEY=FABRIC+FCLR
    XP_STY_KEY=SPACE(0)       && INITIALIZE THE STYLE KEY FOR NEW FABRIC/COLOR
    IF XFMT='D' .AND. XLS='L'
      ROW=ROW+1
      @ ROW,00 SAY ' ........................... OPEN .............................  ..................... OPEN + HOLD ...................... PROJECTED'
    ENDIF
    IF XFMT='D' .AND. XLS='S'
      ROW=ROW+1
      @ ROW,00 SAY ' STYLE            COLOR  DESCRIPTION              YIELD  PCS.....OPEN......'+lcUomUse+'   PCS...OPEN+HOLD...'+lcUomUse+'   PCS...PROJECTED...'+lcUomUse
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('-',132)
      ROW=ROW+1
    ENDIF
    
  ENDIF
  
  IF XFMT='D' .AND. XLS='L' && DETAIL LONG REPORT THEN PRINT ALL STYLE+COLORS
    
    IF XP_STY_KEY<>STYLE          && NEW STYLE
      SELECT STYLE
      IF SEEK(&FABTEMP..STYLE)
        XSTYDESC=DESC
        XSCALE=SCALE
      ELSE
        XSTYDESC=SPACE(0)
        XSCALE=SPACE(0)
      ENDIF
      IF !EMPTY(XSCALE)
        XSCL_DESC=GETSCALE(XSCALE,SPACE(1))
      ELSE
        XSCL_DESC=SPACE(0)
      ENDIF
      
      IF !EMPTY(XP_STY_KEY)               && IF THIS IS NOT FIRST STYLE
                                               && THEN PRINT TOTAL OF LAST STYLE
        ROW=ROW+1
        @ ROW,017 SAY 'TOTAL FABRIC '+lcUomUse+'     REQUIRED ===>'
        @ ROW,055 SAY CEILING(XOYSTY) PICTURE '99999999'
        @ ROW,113 SAY CEILING(XOHYSTY) PICTURE '99999999'
        @ ROW,123 SAY CEILING(XPYSTY) PICTURE '99999999'
        STORE 0 TO XOYSTY,XOHYSTY,XPYSTY
      ENDIF
      
      XP_STY_KEY=STYLE
      
      SELECT (FABTEMP)
      lcUomUse   = IIF(EMPTY(UOM),'USE',UOM)
      
      ROW=ROW+2
      @ ROW,01 SAY STYLE
      @ ROW,15 SAY XSTYDESC
      @ ROW,40 SAY 'YIELD :'
      @ ROW,48 SAY YARDS PICTURE '999.999'
      @ ROW,56 SAY lcUomUse
      ROW=ROW+1
      @ ROW,01 SAY 'COLOR'
      @ ROW,08 SAY XSCL_DESC
      @ ROW,58 SAY 'TOTAL'
      @ ROW,65 SAY XSCL_DESC
      @ ROW,116 SAY 'TOTAL'
    ENDIF
    
    ROW=ROW+1
    @ ROW,01 SAY SCLR
    @ ROW,08 SAY nOOtc1 PICTURE '99999'
    @ ROW,14 SAY nOOtc2 PICTURE '99999'
    @ ROW,20 SAY nOOtc3 PICTURE '99999'
    @ ROW,26 SAY nOOtc4 PICTURE '99999'
    @ ROW,32 SAY nOOtc5 PICTURE '99999'
    @ ROW,38 SAY nOOtc6 PICTURE '99999'
    @ ROW,44 SAY nOOtc7 PICTURE '99999'
    @ ROW,50 SAY nOOtc8 PICTURE '99999'
    @ ROW,56 SAY nTOOtc PICTURE '9999999'
    @ ROW,065 SAY nOHOtc1 PICTURE '99999'
    @ ROW,071 SAY nOHOtc2 PICTURE '99999'
    @ ROW,077 SAY nOHOtc3 PICTURE '99999'
    @ ROW,083 SAY nOHOtc4 PICTURE '99999'
    @ ROW,089 SAY nOHOtc5 PICTURE '99999'
    @ ROW,095 SAY nOHOtc6 PICTURE '99999'
    @ ROW,101 SAY nOHOtc7 PICTURE '99999'
    @ ROW,107 SAY nOHOtc8 PICTURE '99999'
    @ ROW,114 SAY nTOHOtc PICTURE '9999999'
    @ ROW,124 SAY nTOtcPrj PICTURE '9999999'
  ENDIF
  
  IF XFMT='D' .AND. XLS='S' && DETAIL SHORT REPORT THEN PRINT ALL STYLE+COLORS
    IF XP_STY_KEY<>STYLE          && NEW STYLE
      XP_STY_KEY=STYLE
      SELECT STYLE
      IF SEEK(&FABTEMP..STYLE)
        XSTYDESC=DESC
      ELSE
        XSTYDESC=SPACE(1)
      ENDIF
      SELECT (FABTEMP)
      @ ROW,01 SAY STYLE
    ENDIF
    @ ROW,18 SAY SCLR
    @ ROW,25 SAY XSTYDESC
    @ ROW,48 SAY YARDS PICTURE '999.999'
    @ ROW,57 SAY nTOOtc PICTURE '9999999'
    @ ROW,70 SAY CEILING(nTOOtc*YARDS) PICTURE '99999999'
    @ ROW,81 SAY nTOHOtc PICTURE '9999999'
    @ ROW,94 SAY CEILING(nTOHOtc*YARDS) PICTURE '99999999'
    @ ROW,105 SAY nTOtcPrj PICTURE '9999999'
    @ ROW,118 SAY CEILING(nTOtcPrj*YARDS) PICTURE '99999999'
    ROW=ROW+1
  ENDIF
   
  SELECT (FABTEMP)
  ****** COMPUTE THE TOTAL PIECES,YRDAGE ******
  XOPIECE=XOPIECE+nTOOtc
  XOHPIECE=XOHPIECE+nTOHOtc
  XPPIECE=XPPIECE+nTOtcPrj
  XOYRD=XOYRD+(nTOOtc*YARDS)
  XOHYRD=XOHYRD+(nTOHOtc*YARDS)
  XPYRD=XPYRD+(nTOtcPrj*YARDS)
  XOYSTY=XOYSTY+(nTOOtc*YARDS)
  XOHYSTY=XOHYSTY+(nTOHOtc*YARDS)
  XPYSTY=XPYSTY+(nTOtcPrj*YARDS)
  IF !lMake
    lnOpnPOs=lnOpnPOs + nYTOWIP
    IF llFrstPo
      =lfGetIssu()
      llFrstPo=.F.
    ENDIF  
  ELSE
    lnOpnCTs = lnOpnCTs + nYTOWIP
    IF Style <> lcOldSty
      lcOldSty = Style
      =lfGetIssu()
    ENDIF
  ENDIF
  **********************************************
  SELECT (FABTEMP)
  lcMaterial = FABRIC
  SKIP
ENDDO
RETURN
*************
* END OF MAT932
*************

*!**************************************************************************
*! PROG: MAT933
*! DATE: 11/28/2002
*! DESC: PRINT TRIM REQUIREMENTS
*!**************************************************************************
PROCEDURE MAT933

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*MAT930                 TRIM   REQUIREMENTS REPORT     - 123456789012345678901234567890                           PAGE: 1234
*MM/DD/YY
*
* TRIM    COLOR  DESCRIPTION            ONHAND  ONORDER    AVAIL                              PIECES   REQUIRED UNITS REMAINING UNITS
* XXXXXXX XXXXXX XXXXXXXXXXXXXXXXXXXX 99999999 99999999 99999999               OPEN ORDERS:   9999999         99999999        99999999
*                                                                       OPEN + HOLD ORDERS:   9999999         99999999        99999999
*                                                                                PROJECTED:   9999999         99999999        99999999
*-----------------------------------------------------------------------------------------------------------------------------------
* XXXXXXX XXXXXX XXXXXXXXXXXXXXXXXXXX 99999999 99999999 99999999               OPEN ORDERS:   9999999         99999999
*                                                                       OPEN + HOLD ORDERS:   9999999         99999999
*                                                                                PROJECTED:   9999999         99999999
*
*********************************************  DETAIL REPORT  LONG ***********************************************************************
*
* TRIM    COLOR  DESCRIPTION            ONHAND  ONORDER    AVAIL
* XXXXXXX XXXXXX XXXXXXXXXXXXXXXXXXXX 99999999 99999999 99999999
*
* XXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXX    YIELD : 999.999
* ........................... OPEN .............................  ..................... OPEN + HOLD ...................... PROJECTED
*          XXX   XXX   XXX   XXX   XXX   XXX   XXX   XXX   TOTAL  99999 99999 99999 99999 99999 99999 99999 99999  9999999   9999999
* XXXXXX 99999 99999 99999 99999 99999 99999 99999 99999 9999999  99999 99999 99999 99999 99999 99999 99999 99999  9999999   9999999
* XXXXXX 99999 99999 99999 99999 99999 99999 99999 99999 9999999  99999 99999 99999 99999 99999 99999 99999 99999  9999999   9999999
*
*                                                                                             PIECES    REQUIRED UNITS REMAINING UNITS
*                                                                           OPEN ORDERS  ==>  9999999         99999999        99999999
*                                                                    OPEN + HOLD ORDERS  ==>  9999999         99999999        99999999
*                                                                             PROJECTED  ==>  9999999         99999999        99999999
*
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*********************************************  DETAIL REPORT  SHORT FORM *************************************************************
*
* TRIM     COLOR  DESCRIPTION                                    ONHAND                 ONORDER                   AVAIL
* XXXXXXX  XXXXXX XXXXXXXXXXXXXXXXXXXX                         99999999                99999999                99999999
* STYLE         COLOR     DESCRIPTION              YIELD  PCS.....OPEN......YRD   PCS...OPEN+HOLD...YRD   PCS...PROJECTED...YRD
* XXXXXXXXXXXX  XXXXXX    XXXXXXXXXXXXXXXXXXXXX  999.999  9999999      99999999   9999999      99999999   9999999      99999999
*               XXXXXX    XXXXXXXXXXXXXXXXXXXXX  999.999  9999999      99999999   9999999      99999999   9999999      99999999
*               XXXXXX    XXXXXXXXXXXXXXXXXXXXX  999.999  9999999      99999999   9999999      99999999   9999999      99999999
* XXXXXXXXXXXX  XXXXXX    XXXXXXXXXXXXXXXXXXXXX  999.999  9999999      99999999   9999999      99999999   9999999      99999999
*               XXXXXX    XXXXXXXXXXXXXXXXXXXXX  999.999  9999999      99999999   9999999      99999999   9999999      99999999
*                                                                              PIECES   REQUIRED UNITS  REMAINING UNITS
*                                                           OPEN ORDERS  ==>  9999999         99999999         99999999
*                                                    OPEN + HOLD ORDERS  ==>  9999999         99999999         99999999
*                                                             PROJECTED  ==>  9999999         99999999         99999999
*
*
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

CLEAR TYPEAHEAD
SET DEVICE TO PRINT

STORE 0 TO XOPIECE,XOHPIECE,XPPIECE   &&Total material pieces.
STORE 0 TO XOYRD  ,XOHYRD  ,XPYRD     &&Total material yardage.
STORE 0 TO lnCTUsdQy,lnPOUsdQy,lnPONetRq,lnCTNetRq
STORE 0 TO lnORemain,lnHRemain,lnPRemain,lnOpnCTs
lcOldSty=" "

PAGENO   = 0
ROW      = 99
XTIME    = TIME()

BREAK  = 'cCatgTyp+FABRIC+FCLR' && in this case fabric is actaually the trim
XPRV_KEY= SPACE(0)         && USED TO TRIGGER WHEN FABRIC+COLOR IS TO BE WRITTEN
XPRV_TEMP=SPACE(0)
XP_STY_KEY=SPACE(0)        && USED TO TRIGGER WHEN STYLE+COLOR CHANGES
SELECT (FABTEMP)
SET ORDER TO TAG &FABTEMP

**** GET ONHAND,ONORDER,AVAIL ****
SELECT FABRIC
IF SEEK(SUBSTR(&FABTEMP..FABRIC,1,7)+&FABTEMP..FCLR )
  XONHAND=ONHAND
  lnMatRet = lfMatRetrn(Fabric,Color)
  XONORDER=ONORDER+lnMatRet
  XAVAIL = OnHand+OnOrder
  XFABDESC=DESC
  XFABFOUND=.T.     && A FLAG TELLS IF THIS TRIM FOUND IN FABRIC FILE
  XCONV=CONV        && SAVE CONV. FACTOR FOR THIS FABRIC
  XFVENDOR=VENDOR   && SAVE VENDOR FOR THIS FABRIC
ELSE
  XFABFOUND=.F.          && IT'S A NON-INVENTORY ITEM
  XONHAND=0              && PROBABLY NON-INVENTORY ITEM
  XONORDER=0
  XAVAIL=0
  XFABDESC=SPACE(1)
ENDIF

SELECT (FABTEMP)
lcUomUse   = IIF(EMPTY(UOM),'USE',UOM)

IF LEN(TRIM(BREAK)) <>0
   HBREAK = &BREAK
ENDIF
**

DO WHILE INKEY() <> 32
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  IF ROW >=53
    DO lpTHeaders
  ENDIF
  
  DO WHILE LEN(TRIM(BREAK)) <>0
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    lnCTUsdQy = lnCTNetRq
    lnORemain = XOYRD+lnCTNetRq
    lnHRemain = XOHYRD+lnCTNetRq
    lnPRemain = XPYRD+lnCTNetRq
    IF XPRJB='P'
      lnPRemain = XPYRD
    ENDIF
    SELECT (FABTEMP)     
    DO CASE
      CASE XFMT='D' .AND. XLS='L'     && DETAIL REPORT SO SKIP 2 LINES BEFORE PRINTING TOTALS
        ROW = ROW + 1
        DO lpPrnt_Note WITH .T.
        ROW=ROW+2
        @ ROW,82 SAY ' OTC      OTC TOTAL    NET REQUIRED'
        ROW=ROW+1
        lcIsuPrt='  FOR OPEN C/T'
        @ ROW,82 SAY 'PIECES   REQUIRED '+ lcUomUse+lcIsuPrt+' REMAINING '+lcUomUse
        ROW=ROW+1
      CASE XFMT='D' .AND. XLS='S'
        ROW = ROW + 1
        DO lpPrnt_Note WITH .T.
        ROW=ROW+1
        @ ROW,81 SAY ' OTC     OTC TOTAL    NET REQUIRED'
        ROW=ROW+1
        lcIsuPrt='  FOR OPEN C/T '
        @ ROW,81 SAY 'PIECES  REQUIRED '+ lcUomUse+lcIsuPrt+' REMAINING '+lcUomUse
        ROW=ROW+1
    ENDCASE
     
    IF XLS='S'                    && DETAIL SHORT FORM
      @ ROW,62  SAY 'OPEN ORDERS ==>'
      @ ROW,80  SAY XOPIECE PICTURE '9999999'
      @ ROW,93  SAY CEILING(XOYRD) PICTURE '99999999'
      @ ROW,105 SAY -(ROUND(lnCTUsdQy,0)) PICTURE '999999999'
      @ ROW,122 SAY xAvail - lnORemain PICTURE '99999999'
      ROW=ROW+1
      @ ROW,55 SAY 'OPEN + HOLD ORDERS ==>'
      @ ROW,80 SAY XOHPIECE PICTURE '9999999'
      @ ROW,93 SAY CEILING(XOHYRD) PICTURE '99999999'
      @ ROW,105 SAY -(ROUND(lnCTUsdQy,0)) PICTURE '999999999'
      @ ROW,122 SAY xAvail - lnHRemain PICTURE '99999999'
      ROW=ROW+1
      @ ROW,64 SAY 'PROJECTED ==>'
      @ ROW,80 SAY XPPIECE PICTURE '9999999'
      @ ROW,93 SAY CEILING(XPYRD) PICTURE '99999999'
      IF XPRJB='P'
        @ ROW,105 SAY 0 PICTURE '999999999'       
      ELSE
        @ ROW,105 SAY -(ROUND(lnCTUsdQy,0)) PICTURE '999999999'
      ENDIF
      @ ROW,122 SAY xAvail - lnPRemain PICTURE '99999999'
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW=ROW+1
    ENDIF
    
    IF XFMT='S'         && SUMMARY FORM
      ROW = ROW + 1
      DO lpPrnt_Note WITH .T.
      @ ROW,82  SAY 'ORDERS [O]:'       
      @ ROW,94  SAY XOPIECE PICTURE '9999999'
      @ ROW,102 SAY CEILING(XOYRD) PICTURE '99999999'
      @ ROW,111 SAY -(ROUND(lnCTUsdQy,0)) PICTURE '999999999'       
      @ ROW,124 SAY ROUND(xAvail-lnORemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,80  SAY 'ORDERS [O+H]:'
      @ ROW,94  SAY XOHPIECE PICTURE '9999999'
      @ ROW,102 SAY CEILING(XOHYRD) PICTURE '99999999'
      @ ROW,111 SAY -(ROUND(lnCTUsdQy,0)) PICTURE '999999999'
      @ ROW,124 SAY ROUND(xAvail-lnHRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,83  SAY 'PROJECTED:'
      @ ROW,94  SAY XPPIECE PICTURE '9999999'
      @ ROW,102 SAY CEILING(XPYRD) PICTURE '99999999'
      IF XPRJB='P'
        @ ROW,111 SAY 0 PICTURE '999999999'       
      ELSE
        @ ROW,111 SAY -(ROUND(lnCTUsdQy,0)) PICTURE '999999999'
      ENDIF
      @ ROW,124 SAY ROUND(xAvail-lnPRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW=ROW+1
    ENDIF
    
    IF XLS='L'          && DETAIL LONG FORM OR SUMMARY FORM WHEN XLS=SPACE(0) AS INITILIAZED
      @ ROW,63  SAY 'OPEN ORDERS ==>'
      @ ROW,81  SAY XOPIECE PICTURE '9999999'
      @ ROW,95  SAY CEILING(XOYRD) PICTURE '99999999'
      @ ROW,106 SAY -(ROUND(lnCTUsdQy,0)) PICTURE '999999999'
      @ ROW,123 SAY ROUND(xAvail-lnORemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,56  SAY 'OPEN + HOLD ORDERS ==>'
      @ ROW,81  SAY XOHPIECE PICTURE '9999999'
      @ ROW,95  SAY CEILING(XOHYRD) PICTURE '99999999'
      @ ROW,106 SAY -(ROUND(lnCTUsdQy,0)) PICTURE '999999999'
      @ ROW,123 SAY ROUND(xAvail-lnHRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,65  SAY 'PROJECTED ==>'
      @ ROW,81  SAY XPPIECE PICTURE '9999999'
      @ ROW,95  SAY CEILING(XPYRD) PICTURE '99999999'
      IF XPRJB='P'
        @ ROW,106 SAY 0 PICTURE '999999999'       
      ELSE
        @ ROW,106 SAY -(ROUND(lnCTUsdQy,0)) PICTURE '999999999'
      ENDIF
      @ ROW,123 SAY ROUND(xAvail-lnPRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW=ROW+1
    ENDIF
    
    *** GATHER MATERIALS FOR WHICH THERE IS NEGATIVE REMAINING QUANTITY ***
    IF XFABFOUND
      DO CASE
        CASE XNEG_QTY ='O' .AND. ROUND(XAVAIL-lnORemain,0) < 0
          xRequired = ABS(xAvail - lnORemain)
          DO MAT939
        CASE XNEG_QTY ='H' .AND. ROUND(XAVAIL-lnHRemain,0) < 0
          xRequired = ABS(xAvail - lnHRemain)
          DO MAT939
        CASE XNEG_QTY ='P' .AND. ROUND(XAVAIL-lnPRemain,0) < 0
          xRequired = ABS(xAvail - lnPRemain)
          DO MAT939
      ENDCASE
      SELECT (FABTEMP)
    ENDIF
    
    STORE 0 TO XOPIECE,XOHPIECE,XPPIECE   &&Total material pieces.
    STORE 0 TO XOYRD  ,XOHYRD  ,XPYRD     &&Total material yardage.
    STORE 0 TO lnCTUsdQy,lnCTNetRq
    STORE 0 TO lnORemain,lnHRemain,lnPRemain,lnOpnCTs
    lcOldSty=" "
    HBREAK=&BREAK
    EXIT
  ENDDO
  
  IF EOF() .OR. cCatgTyp<>'T'
    EXIT
  ENDIF
  
  IF ROW >=53
    ROW = 99
    LOOP
  ENDIF
  
  IF XPRV_KEY<>FABRIC+FCLR
  
    **** GET ONHAND,ONORDER,AVAIL ****
    SELECT FABRIC
    IF SEEK(SUBSTR(&FABTEMP..FABRIC,1,7)+&FABTEMP..FCLR)
      XFABFOUND=.T.
      XCONV=CONV
      XFVENDOR=VENDOR
      XONHAND=ONHAND
      lnMatRet = lfMatRetrn(Fabric,Color)
      XONORDER=ONORDER+lnMatRet
      XAVAIL = OnHand+OnOrder
      XFABDESC=DESC
    ELSE
      XFABFOUND=.F.
      XONHAND=0
      XONORDER=0
      XAVAIL=0
      XFABDESC=SPACE(1)
    ENDIF
    
    SELECT (FABTEMP)
    lcUomUse   = IIF(EMPTY(UOM),'USE',UOM)
    
    ***** WRITE ******
    IF XFMT = 'S'           && DETAIL SHORT FORM
      @ ROW,01 SAY ALLTRIM(FABRIC)
      @ ROW,09 SAY FCLR
      @ ROW,16 SAY SUBSTR(gfCodDes(FCLR , 'COLOR'),1,15)
      @ ROW,32 SAY XFABDESC
      @ ROW,53 SAY XONHAND PICTURE '99999999'
      @ ROW,62 SAY XONORDER PICTURE '99999999'
      @ ROW,71 SAY XAVAIL PICTURE '99999999'
    ENDIF
    
    IF XLS='S'             && DETAIL SHORT FORM
      @ ROW,01 SAY ALLTRIM(FABRIC)
      @ ROW,10 SAY FCLR
      @ ROW,17 SAY SUBSTR(gfCodDes(FCLR , 'COLOR'),1,15)
      @ ROW,38 SAY XFABDESC
      @ ROW,62 SAY XONHAND PICTURE '99999999'
      @ ROW,86 SAY XONORDER PICTURE '99999999'
      @ ROW,110 SAY XAVAIL PICTURE '99999999'
    ENDIF
    
    IF XLS='L'            && DETAIL SHORT FORM
      @ ROW,01 SAY ALLTRIM(FABRIC)
      @ ROW,09 SAY FCLR
      @ ROW,16 SAY SUBSTR(gfCodDes(FCLR , 'COLOR'),1,15)
      @ ROW,37 SAY XFABDESC
      @ ROW,58 SAY XONHAND PICTURE '99999999'
      @ ROW,67 SAY XONORDER PICTURE '99999999'
      @ ROW,76 SAY XAVAIL PICTURE '99999999'
    ENDIF
    
    XPRV_KEY=FABRIC+FCLR
    XP_STY_KEY=SPACE(0)
    
    IF XFMT='D' .AND. XLS='L'
      ROW=ROW+1
      @ ROW,00 SAY ' ........................... OPEN .............................  ..................... OPEN + HOLD ...................... PROJECTED'
    ENDIF
    
    IF XFMT='D' .AND. XLS='S'
      ROW=ROW+1
      @ ROW,00 SAY ' STYLE            COLOR  DESCRIPTION              YIELD  PCS.....OPEN......'+lcUomUse+'   PCS...OPEN+HOLD...'+lcUomUse+'   PCS...PROJECTED...'+lcUomUse
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('-',132)
      ROW=ROW+1
    ENDIF
  ENDIF
  
  IF XFMT='D' .AND. XLS='L'          && DETAIL REPORT THEN PRINT ALL STYLE+cOLORS
    IF XP_STY_KEY<>STYLE          && NEW STYLE
      XP_STY_KEY=STYLE
      SELECT STYLE
      IF SEEK(&FABTEMP..STYLE)
        XSTYDESC=DESC
        XSCALE=SCALE
      ELSE
        XSTYDESC=SPACE(0)
        XSCALE=SPACE(0)
      ENDIF
      IF !EMPTY(XSCALE)
        XSCL_DESC=GETSCALE(XSCALE,SPACE(1))
      ELSE
        XSCL_DESC=SPACE(0)
      ENDIF
      SELECT (FABTEMP)
      lcUomUse   = IIF(EMPTY(UOM),'USE',UOM)
      ROW=ROW+2
      @ ROW,01 SAY STYLE
      @ ROW,16 SAY XSTYDESC
      @ ROW,41 SAY 'YIELD :'
      @ ROW,49 SAY YARDS PICTURE '999.999'
      @ ROW,57 SAY UOM
      ROW=ROW+1
      @ ROW,01 SAY 'COLOR'
      @ ROW,08 SAY XSCL_DESC
      @ ROW,58 SAY 'TOTAL'
      @ ROW,65 SAY XSCL_DESC
      @ ROW,116 SAY 'TOTAL'
    ENDIF
    ROW=ROW+1
    @ ROW,01 SAY SCLR
    @ ROW,08 SAY nOOtc1 PICTURE '99999'
    @ ROW,14 SAY nOOtc2 PICTURE '99999'
    @ ROW,20 SAY nOOtc3 PICTURE '99999'
    @ ROW,26 SAY nOOtc4 PICTURE '99999'
    @ ROW,32 SAY nOOtc5 PICTURE '99999'
    @ ROW,38 SAY nOOtc6 PICTURE '99999'
    @ ROW,44 SAY nOOtc7 PICTURE '99999'
    @ ROW,50 SAY nOOtc8 PICTURE '99999'
    @ ROW,56 SAY nTOOtc PICTURE '9999999'
    @ ROW,065 SAY nOHOtc1 PICTURE '99999'
    @ ROW,071 SAY nOHOtc2 PICTURE '99999'
    @ ROW,077 SAY nOHOtc3 PICTURE '99999'
    @ ROW,083 SAY nOHOtc4 PICTURE '99999'
    @ ROW,089 SAY nOHOtc5 PICTURE '99999'
    @ ROW,095 SAY nOHOtc6 PICTURE '99999'
    @ ROW,101 SAY nOHOtc7 PICTURE '99999'
    @ ROW,107 SAY nOHOtc8 PICTURE '99999'
    @ ROW,114 SAY nTOHOtc PICTURE '9999999'
    @ ROW,124 SAY nTOtcPrj PICTURE '9999999'
  ENDIF
  
  IF XFMT='D' .AND. XLS='S' && DETAIL SHORT REPORT THEN PRINT ALL STYLE+cOLORS
    IF XP_STY_KEY<>STYLE          && NEW STYLE
      XP_STY_KEY=STYLE
      SELECT STYLE
      IF SEEK(&FABTEMP..STYLE)
        XSTYDESC=DESC
      ELSE
        XSTYDESC=SPACE(0)
      ENDIF
      SELECT (FABTEMP)
      @ ROW,01 SAY STYLE
    ENDIF
    @ ROW,18 SAY SCLR
    @ ROW,25 SAY XSTYDESC
    @ ROW,48 SAY YARDS PICTURE '999.999'
    @ ROW,57 SAY nTOOtc PICTURE '9999999'
    @ ROW,70 SAY CEILING(nTOOtc*YARDS) PICTURE '99999999'
    @ ROW,81 SAY nTOHOtc PICTURE '9999999'
    @ ROW,94 SAY CEILING(nTOHOtc*YARDS) PICTURE '99999999'
    @ ROW,105 SAY nTOtcPrj PICTURE '9999999'
    @ ROW,118 SAY CEILING(nTOtcPrj*YARDS) PICTURE '99999999'
    ROW=ROW+1
  ENDIF
  SELECT (FABTEMP)
  ****** COMPUTE THE TOTAL PIECES,YRDAGE ******
  XOPIECE=XOPIECE+nTOOtc
  XOHPIECE=XOHPIECE+nTOHOtc
  XPPIECE=XPPIECE+nTOtcPrj
  XOYRD=XOYRD+(nTOOtc*YARDS)
  XOHYRD=XOHYRD+(nTOHOtc*YARDS)
  XPYRD=XPYRD+(nTOtcPrj*YARDS)
  lnOpnCTs = lnOpnCTs + nYTOWIP
  IF Style <> lcOldSty
    lcOldSty = Style
    STORE 0 TO lnCTUsdQy,lnCTNetRq
    =lfGetIssu()
  ENDIF
  **********************************************
  
  SELECT (FABTEMP)
  lcMaterial = FABRIC
  SKIP
ENDDO
RETURN

*!**************************************************************************
*! PROG: MAT934
*! DATE: 11/28/2002
*! DESC: PRINT STYLE REQUIREMENTS
*!**************************************************************************
PROCEDURE MAT934

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*MAT930                 STYLE  REQUIREMENTS REPORT     - 123456789012345678901234567890                           PAGE: 1234
*MM/DD/YY
*
* STYLE COMP   COLOR  DESCRIPTION            ONHAND  ONORDER    AVAIL                               PIECES   REQUIRED UNT   REMAINING UNT
* XXXXXXXXXXXX XXXXXX XXXXXXXXXXXXXXXXXXXX 99999999 99999999 99999999               OPEN ORDERS:   9999999       99999999        99999999
*                                                                            OPEN + HOLD ORDERS:   9999999       99999999        99999999
*                                                                                     PROJECTED:   9999999       99999999        99999999
*********************************************  DETAIL REPORT ******************************************************************
*
* FABRIC COLOR DESCRIPTION            ONHAND  ONORDER    AVAIL
* XXXXXXX XXX  XXXXXXXXXXXXXXXXXXXX 99999999 99999999 99999999
*
* XXXXXXXX XXXXXXXXXXXXXXXXXXXXX
* ......................... OPEN ............................     ..................... OPEN + HOLD ...................... PROJECTED
*       XXX   XXX   XXX   XXX   XXX   XXX   XXX   XXX   TOTAL     99999 99999 99999 99999 99999 99999 99999 99999  9999999   9999999
* XXX 99999 99999 99999 99999 99999 99999 99999 99999 9999999     99999 99999 99999 99999 99999 99999 99999 99999  9999999   9999999
* XXX 99999 99999 99999 99999 99999 99999 99999 99999 9999999     99999 99999 99999 99999 99999 99999 99999 99999  9999999   9999999
*
*                                                                                             PIECES    REQUIRED UNT   REMAINING UNT
*                                                                           OPEN ORDERS  ==>  9999999       99999999        99999999
*                                                                    OPEN + HOLD ORDERS  ==>  9999999       99999999        99999999
*                                                                             PROJECTED  ==>  9999999       99999999        99999999
*
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

CLEAR TYPEAHEAD
SET DEVICE TO PRINT

STORE 0 TO XOPIECE,XOHPIECE,XPPIECE   &&Total material pieces.
STORE 0 TO XOYRD  ,XOHYRD  ,XPYRD     &&Total material yardage.
STORE 0 TO lnCTUsdQy,lnPOUsdQy,lnPONetRq,lnCTNetRq
STORE 0 TO lnORemain,lnHRemain,lnPRemain
STORE 0 to lnOpnPOs,lnOpnCTs
lcOldSty=" "
llFrstPo=.T.
PAGENO   = 0
ROW      = 99
XTIME    = TIME()
BREAK  = 'cCatgTyp+FABRIC+FCLR' && actually in this case fabric is the style componenet
XPRV_KEY= SPACE(0)         && USED TO TRIGGER WHEN FABRIC+COLOR IS TO BE WRITTEN
XP_STY_KEY=SPACE(0)        && USED TO TRIGGER WHEN STYLE+COLOR CHANGES
SELECT (FABTEMP)
SET ORDER TO TAG &FABTEMP

**** GET ONHAND,ONORDER,AVAIL ****
SELECT STYLE
IF SEEK(&FABTEMP..FABRIC+&FABTEMP..FCLR)    && FABRIC IS ACTUALLY THE STYLE COMPNENT
  XONHAND  = TOTSTK
  XONORDER = TOTWIP
  XAVAIL   = TOTSTK+TOTWIP
ELSE
  XONHAND  = 0
  XONORDER = 0
  XAVAIL   = 0
ENDIF

SELECT (FABTEMP)
IF LEN(TRIM(BREAK)) <>0
  HBREAK = &BREAK
ENDIF

DO WHILE INKEY() <> 32
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  IF ROW >=53
    PAGENO = PAGENO+1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    SELECT (FABTEMP)
    IF XFMT='D'
      @ 05,00 SAY ' STYL COMP     COLOR  CLR DESCRIPTION STYL COMP DESCRIPT     ONHAND  ONORDER  P.RETURN  AVAIL   ' &&TAK 03/01/94
      @ 06,00 SAY ' STYLE             DESCRIPTION'  &&TAK03/01/94
      @ 07,00 SAY REPLICATE('=',132)
      ROW=8
    ELSE
      @ 05,000 SAY ' STYL COMP     COLOR  CLR DESCRIPTION STYL COMP DESCRIPT     ONHAND  ONORDER   AVAIL '  &&TAK 03/01/94
      @ 05,97 SAY ' PIECES REQ. UNT '+' NET REQ. REM. UNT'
      @ 06,000 SAY REPLICATE('=',132)
      ROW=7
    ENDIF
  ENDIF
  
  DO WHILE LEN(TRIM(BREAK)) <>0
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    lnPOUsdQy = lnPONetRq
    lnCTUsdQy = lnCTNetRq
    lnORemain = XOYRD+(lnCTNetRq+lnPONetRq)
    lnHRemain = XOHYRD+(lnCTNetRq+lnPONetRq)
    lnPRemain = XPYRD+(lnCTNetRq+lnPONetRq)
    IF XPRJB='P'
      lnPRemain = XPYRD
    ENDIF
    SELECT (FABTEMP)     
    IF XFMT='D'      && DETAIL REPORT SO SKIP 2 LINES BEFORE PRINTING TOTALS
      ROW=ROW+2
      @ ROW,83 SAY ' OTC      OTC TOTAL   NET REQUIRED'
      ROW=ROW+1
      lcIsuPrt=' FOR OPEN C/T'
      @ ROW,83 SAY 'PIECES   REQUIRED UNT'+lcIsuPrt+' REMAINING UNT'
      ROW=ROW+1
    ENDIF
    ***** IF SUMMARY THEN PRINT ON SAME LINE AS FABRIC ****
    IF XFMT='S'
      @ ROW,086 SAY 'ORDERS [O]:'
      @ ROW,97 SAY XOPIECE PICTURE '9999999'
      @ ROW,105 SAY CEILING(XOYRD) PICTURE '99999999'
      @ ROW,114 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      @ ROW,124 SAY ROUND(xAvail-lnORemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,084 SAY 'ORDERS [O+H]:'
      @ ROW,97 SAY XOHPIECE PICTURE '9999999'
      @ ROW,105 SAY CEILING(XOHYRD) PICTURE '99999999'
      @ ROW,114 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      @ ROW,124 SAY ROUND(xAvail-lnHRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,087 SAY 'PROJECTED:'
      @ ROW,97 SAY XPPIECE PICTURE '9999999'
      @ ROW,105 SAY CEILING(XPYRD) PICTURE '99999999'
      IF XPRJB='P'
        @ ROW,114 SAY 0 PICTURE '999999999'       
      ELSE
        @ ROW,114 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      ENDIF
      @ ROW,124 SAY ROUND(xAvail-lnPRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW=ROW+1
    ELSE
      @ ROW,65  SAY 'OPEN ORDERS ==>'
      @ ROW,82  SAY XOPIECE PICTURE '9999999'
      @ ROW,96 SAY CEILING(XOYRD) PICTURE '99999999'
      @ ROW,108 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      @ ROW,123 SAY ROUND(xAvail-lnORemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,58  SAY 'OPEN + HOLD ORDERS ==>'
      @ ROW,82  SAY XOHPIECE PICTURE '9999999'
      @ ROW,96 SAY CEILING(XOHYRD) PICTURE '99999999'
      @ ROW,108 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      @ ROW,123 SAY ROUND(xAvail-lnHRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,67  SAY 'PROJECTED ==>'
      @ ROW,82  SAY XPPIECE PICTURE '9999999'
      @ ROW,96 SAY CEILING(XPYRD) PICTURE '99999999'
      IF XPRJB='P'
        @ ROW,108 SAY 0 PICTURE '999999999'       
      ELSE
        @ ROW,108 SAY -(ROUND(lnCTUsdQy+lnPOUsdQy,0)) PICTURE '999999999'
      ENDIF
      @ ROW,123 SAY ROUND(xAvail-lnPRemain,0) PICTURE '99999999'
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW=ROW+1
    ENDIF
    XOPIECE  = 0
    XOHPIECE = 0
    XPPIECE  = 0
    XOYRD    = 0
    XOHYRD   = 0
    XPYRD    = 0
    STORE 0 TO lnCTUsdQy,lnPOUsdQy,lnPONetRq,lnCTNetRq
    STORE 0 TO lnORemain,lnHRemain,lnPRemain
    STORE 0 to lnOpnPOs,lnOpnCTs
    lcOldSty=" "
    llFrstPo=.T.
    HBREAK=&BREAK
    EXIT
  ENDDO
  
  IF EOF() .OR. cCatgTyp<>'S'
    EXIT
  ENDIF
  
  IF ROW >=53
    ROW = 99
    LOOP
  ENDIF
  
  IF XPRV_KEY<>FABRIC+FCLR
    **** GET ONHAND,ONORDER,AVAIL ****
    SELECT STYLE
    IF SEEK(&FABTEMP..FABRIC+&FABTEMP..FCLR)   && FABRIC IS ACTUALLY THE STYLE COMPNENT
      XONHAND  = TOTSTK
      XONORDER = TOTWIP
      XAVAIL   = TOTSTK+TOTWIP
    ELSE
      XONHAND  = 0
      XONORDER = 0
      XAVAIL   = 0
    ENDIF
    SELECT (FABTEMP)
    ***** WRITE ******
    @ ROW,01 SAY FABRIC          && actually it is the style component
    @ ROW,15 SAY FCLR
    @ ROW,22 SAY SUBSTR(gfCodDes(FCLR , 'COLOR'),1,15)
    @ ROW,38 SAY DESC
    @ ROW,59 SAY XONHAND PICTURE '99999999'
    @ ROW,68 SAY XONORDER PICTURE '99999999'
    @ ROW,77 SAY XAVAIL PICTURE '99999999'
    XPRV_KEY=FABRIC+FCLR
    XP_STY_KEY=SPACE(0)
    IF XFMT='D'
      ROW=ROW+1
      @ ROW,00 SAY ' ........................... OPEN .............................  ..................... OPEN + HOLD ...................... PROJECTED'
    ENDIF
  ENDIF
  
  IF XFMT='D'          && DETAIL REPORT THEN PRINT ALL STYLE+cOLORS
    IF XP_STY_KEY<>STYLE          && NEW STYLE
      XP_STY_KEY=STYLE
      SELECT STYLE
      IF SEEK(&FABTEMP..STYLE)
        XSTYDESC=DESC
        XSCALE=SCALE
      ELSE
        XSTYDESC=SPACE(0)
        XSCALE=SPACE(0)
      ENDIF
      IF !EMPTY(XSCALE)
        XSCL_DESC=GETSCALE(XSCALE,SPACE(1))
      ELSE
        XSCL_DESC=SPACE(0)
      ENDIF
      SELECT (FABTEMP)
      ROW=ROW+2
      @ ROW,01 SAY STYLE
      @ ROW,19 SAY XSTYDESC
      ROW=ROW+1
      @ ROW,01 SAY 'COLOR'
      @ ROW,08 SAY XSCL_DESC
      @ ROW,58 SAY 'TOTAL'
      @ ROW,65 SAY XSCL_DESC
      @ ROW,116 SAY 'TOTAL'
    ENDIF
    ROW=ROW+1
    @ ROW,01 SAY SCLR
    @ ROW,08 SAY nOOtc1 PICTURE '99999'
    @ ROW,14 SAY nOOtc2 PICTURE '99999'
    @ ROW,20 SAY nOOtc3 PICTURE '99999'
    @ ROW,26 SAY nOOtc4 PICTURE '99999'
    @ ROW,32 SAY nOOtc5 PICTURE '99999'
    @ ROW,38 SAY nOOtc6 PICTURE '99999'
    @ ROW,44 SAY nOOtc7 PICTURE '99999'
    @ ROW,50 SAY nOOtc8 PICTURE '99999'
    @ ROW,56 SAY nTOOtc PICTURE '9999999'
    @ ROW,065 SAY nOHOtc1 PICTURE '99999'
    @ ROW,071 SAY nOHOtc2 PICTURE '99999'
    @ ROW,077 SAY nOHOtc3 PICTURE '99999'
    @ ROW,083 SAY nOHOtc4 PICTURE '99999'
    @ ROW,089 SAY nOHOtc5 PICTURE '99999'
    @ ROW,095 SAY nOHOtc6 PICTURE '99999'
    @ ROW,101 SAY nOHOtc7 PICTURE '99999'
    @ ROW,107 SAY nOHOtc8 PICTURE '99999'
    @ ROW,114 SAY nTOHOtc PICTURE '9999999'
    @ ROW,124 SAY nTOtcPrj PICTURE '9999999'
  ENDIF
  SELECT (FABTEMP)
  ****** COMPUTE THE TOTAL PIECES,YRDAGE ******
  XOPIECE=XOPIECE+nTOOtc
  XOHPIECE=XOHPIECE+nTOHOtc
  XPPIECE=XPPIECE+nTOtcPrj
  XOYRD=XOYRD+(nTOOtc*YARDS)
  XOHYRD=XOHYRD+(nTOHOtc*YARDS)
  XPYRD=XPYRD+(nTOtcPrj*YARDS)
  IF !lMake
    lnOpnPOs=lnOpnPOs + nYTOWIP
    IF llFrstPo
      STORE 0 TO lnPOUsdQy,lnPONetRq
      =lfGetIssu()
      llFrstPo=.F.
    ENDIF  
  ELSE
    lnOpnCTs = lnOpnCTs + nYTOWIP
    IF Style <> lcOldSty
      lcOldSty = Style
      STORE 0 TO lnCTUsdQy,lnCTNetRq
      =lfGetIssu()
    ENDIF
  ENDIF
  **********************************************
  
  SELECT (FABTEMP)
  SKIP
ENDDO
RETURN
*************
* END OF MAT934
*************

*!*************************************************************
*! Name      : lfEditProj
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/27/2002
*! Purpose   : Edit Projection.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Call               : ARIABROW() , lfvEdtPrj()
*!*************************************************************
*! Example   : =lfEditProj()
*!*************************************************************
FUNCTION lfEditProj

lcBrFields = "Style    :R :H=lcStyHdr    :30,"+;
	         "SCLR     :R :H='Color'     :12,"+;
	         "lcDesc = STYLE.Desc1  :R :H='Description' :41,"+;
	         "OReqTot  :R :H='Open'      :12,"+;
	         "OHReqTot :R :H='Open+Hold' :12,"+;
	         "Project  :R :H='Projected' :12 "
SELECT STYLE
SET ORDER TO TAG STYLE
SELECT (FABTEMP)
SET ORDER TO TAG &FABTEMP2
SET RELA TO STYLE INTO STYLE
LOCATE
=ARIABROW('','Edit projection',gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2,'lfvEdtPrj',;
             'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;\<Edit;\<Ok' )
SELECT STYLE
SET RELATION TO
RETURN

*!*************************************************************
*! Name      : lfvEdtPrj
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/27/2002
*! Purpose   : Modify Projection, Valid of edit projection.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvEdtPrj()
*!*************************************************************
FUNCTION lfvEdtPrj

PRIVATE lnCurrRec, lcCurrSty
llOk = .F.
lnPrjn = Project
DO (gcRepHome+"MA\MAROBEDT.SPX")
IF llOk
  lnCurrRec = RECNO()
  lcCurrSty = STYLE+SCLR
  SET ORDER TO TAG &FABTEMP1
  SEEK lcCurrSty
  SCAN REST WHILE Style+SCLR+cCatgTyp+FABRIC+FClr = lcCurrSty
    REPLACE Project WITH lnPrjn
  ENDSCAN
  SET ORDER TO TAG &FABTEMP2
  IF BETWEEN(lnCurrRec,1,RECCOUNT())
    GOTO lnCurrRec
  ENDIF
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGenMPo
*! Developer : AHMED MAHER
*! Date      : 11/28/2002
*! Purpose   : Generate Material P/O.
*!  GENERATE PURCHASE ORDER(S) FOR MATERIAL(S) FOR WHICH THERE IS
*!                  NEGATIVE REMAINING QUANTITY IF ANY.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfGenMPo()
*!*************************************************************
FUNCTION lfGenMPo

SELECT APVENDOR
LOCATE
IF EOF()
 *--The vendor file is empty, you cannot create PO's .
  =gfModalGen('TRM36128B36000','DIALOG')
  RETURN .F.
ENDIF

*--Read the multi currency and multi warehouse setup.
*--Rights for edit exch. rates.
llEditExRt = gfGetMemVar('llEditExRa')
llMulCurr  = gfGetMemVar('llMulCurr')
llWareHous = (gfGetMemVar('M_WareHouse')='Y')

*---llGenMaPoN = .T.  -----> generated Po no
*---llGenMaPoN = .F.  -----> manuall Po no 
llGENMAPON = gfGetMemVar('M_GENMAPON')='N'

IF !llWareHous
  GO TOP IN WAREHOUS
ENDIF

SELECT (FABTEMP4)
SCAN
  lcFabKey = Fabric+Color
  lcFbCVen = Vendor
  WAIT WINDOW 'Collecting data for Material : ' + lcFabKey NOWAIT
  
  =SEEK(lcFabKey,'FABRIC')
  =RLOCK()
  REPLACE cFabGrade  WITH FABRIC.cFabGrade,;
          cWareCode  WITH IIF(!llWareHous,WAREHOUS.cWareCode,''),;
          cPriceCur  WITH IIF(EMPTY(FABRIC.cPriceCur),gcBaseCurr,FABRIC.cPriceCur),;
          cDutyCur   WITH IIF(EMPTY(FABRIC.cDutyCur) ,gcBaseCurr,FABRIC.cDutyCur),;
          nFabcost   WITH FABRIC.nFabcost,;
          nItm_Frt   WITH FABRIC.nItm_Frt,;
          nItem_Tax  WITH FABRIC.nItem_Tax,;
          nItemQuota WITH FABRIC.nItemQuota
  UNLOCK
  *--Search For Last Price This Material Was Purchased At From This Vendor,
  *--If it Was Never Purchased Before, Take the Price From Fabric file.
  SELECT POFLN  
  lcMPO=SPACE(6) 
  =SEEK(lcFabKey+'P')
  SCAN WHILE Fabric+Color+cMatType = lcFabKey+'P' ;
         FOR Trancd = '1' AND Vendor = ALLT(lcFbCVen)
    lcMPO  = POMat
    lnCost1 = nCost1
    lnCost2 = nCost2
    lnCost3 = nCost3
    lnCost4 = nCost4        
  ENDSCAN
  
  SELECT (FABTEMP4)
  IF !EMPTY(lcMPO)
    =SEEK('P'+lcMPO,'POFHDR')
    =RLOCK()
    REPLACE Vendor     WITH POFHDR.Vendor,;
            cPriceCur  WITH POFHDR.cPriceCur,;
            cDutyCur   WITH POFHDR.cDutyCur,;
            nFabcost   WITH lnCost1,;
            nItm_Frt   WITH lnCost2,;
            nItem_Tax  WITH lnCost3,;
            nItemQuota WITH lnCost4
    UNLOCK
  ELSE
    IF llMulCurr AND !EMPTY(lcFbCVen) AND ;
       SEEK(lcFbCVen,'APVENDOR') AND !EMPTY(APVENDOR.cCurrCode)
      =RLOCK()
      REPLACE cPriceCur WITH IIF(!EMPTY(APVENDOR.cCurrCode),APVENDOR.cCurrCode,cPriceCur)
      UNLOCK
    ENDIF
  ENDIF
  
  *--Update the Currency Rate and units.
  STORE 1 TO lnCurrUnt1,lnCurrUnt2
  STORE 1 TO lnPRate,lnDRate
  IF llMulCurr
    lnPRate = gfChkRate('lnCurrUnt1',cPriceCur,gdSysDate,llEditExRt,gcAct_comp,.F.)
    lnDRate = gfChkRate('lnCurrUnt2',cDutyCur ,gdSysDate,llEditExRt,gcAct_comp,.F.)
  ENDIF
  =RLOCK()
  REPLACE nPriceRat  WITH IIF(lnPRate=0,1,lnPRate),;
          nCurrUnit  WITH lnCurrUnt1,;
          nDutyRat   WITH IIF(lnDRate=0,1,lnDRate),;
          nDCurUnit  WITH lnCurrUnt2
  UNLOCK     

ENDSCAN

*--Initialize the currency method and symbol.
STORE '' TO lcPMethod,lcPUnMeth
STORE SET('CURRENCY',1) TO lcPBseSmbl
lcBrFields = "Fabric     :R :H='Fabric'      :10,"+;
             "Color      :R :H='Color'       :8 ,"+;
	         "Vendor     :R :H='Vendor'      :10,"+;
             "LeadTime   :R :H='LdTime'      :6 ,"+;
             "Complete   :R :H='Complete'    :10 ,"+;
             "nFabcost   :R :H='Price'       :8 ,"+;
             "lnBsPrc=lfGetEquv(cPriceCur,nFabcost,nPriceRat,nCurrUnit) :R :H=lcPBseSmbl+' Price'  :P='999999.99' :12,"+;
             "nFabTotQty :R :H='Qty. Needed' :12,"+;
             "UOMBUY     :R :H='UOMBuy'       :7 ,"+; 
             "cPriceCur  :R :H='PriceCur.'    :8,"+;
             "nPriceRat  :R :H='PriceRate'    :9,"+;
             "cDutyCur   :R :H='Duty Cur.'    :8,"+;
             "nDutyRat   :R :H='Duty Rate'    :9"

SELECT (FABTEMP4)
GO TOP
DO WHILE .T.
  lcCentry= SET('CENTURY')
  SET CENTURY ON
  =ARIABROW('','Negative Remaining Materials',gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2,'lfvEdtMat',;
               'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;\<Edit;\<Ok' )
  SET CENTURY  &lcCentry
  *--Check for any valid record (nothing selected).
  LOCATE FOR !EMPTY(Vendor) AND nFabTotQty <> 0
  IF !FOUND()
    *--No vendor,price or quantity needed has been defined for these materials.
    *--Do you wish to Assign?   <Assign><Cancel>
    IF gfModalGen('INM36129B36004','DIALOG') = 1
      LOOP
    ELSE
      RETURN .F.
    ENDIF
  ENDIF

  LOCATE FOR EMPTY(Vendor)
  IF FOUND() AND gfModalGen('QRM36122B36001','DIALOG') = 2
    *---Some materials has no vendor,therefor no purchase orders will be generated for this Materials! YES/NO
    LOOP
  ENDIF

  *--Generate Same P/O for this Key :
  *--KEY VENDOR+CFABGRADE+COMPLETE+CPRICECUR+CDUTYCUR+NPRICERAT+NDUTYRAT 

  *--Note that the materials with same vendor,quality,currency and completion date will be joinded in same P/O.
  *-- "<Generate> <Modify> <Cancel>"
  lnSeletn = gfModalGen('QRM36123B36009','DIALOG')
  DO CASE
    CASE lnSeletn = 1
      EXIT 
    CASE lnSeletn = 3
      RETURN .F.
  ENDCASE
ENDDO

*--Open temp P/O Heder.
lcPOTmpHD = gfTempName()
SELECT POFHDR
= AFIELDS(laTempStru)
IF !llGENMAPON 
  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
  *-- cTmpPoMat :  field used to hold the manual po no 
  laTempStru[ALEN(laTempStru,1),1] = 'cTmpPoMat'
  laTempStru[ALEN(laTempStru,1),2] = 'C'
  laTempStru[ALEN(laTempStru,1),3] = 6
  laTempStru[ALEN(laTempStru,1),4] = 0
ENDIF
CREATE DBF (gcWorkDir+lcPOTmpHD) FROM ARRAY laTempStru
= gfOpenFile(gcWorkDir+lcPOTmpHD,'','EX')
INDEX ON Vendor+cFabGrade+DTOC(Complete)+cPriceCur+cDutyCur+STR(nPriceRat,9,4)+STR(nDutyRat,9,4) TAG (lcPOTmpHD)

*--Open temp P/O Lines.
lcPOTmpLN = gfTempName()
SELECT POFLN
COPY STRU TO (gcWorkDir+lcPOTmpLN)
= gfOpenFile(gcWorkDir+lcPOTmpLN,'','EX')
INDEX ON POMAT+STR(LINENO,6) TAG (lcPOTmpLN)
= gfOpenFile(gcDataDir+'CODES','CODES','SH')

*--Read Default.
DECLARE laCodes[3,10],laTerms[1,2],laDivision[1,2],laShipVia[1,2]
STORE '' TO laCodes,laTerms,laDivision,laShipVia
STORE 1  TO lnTerms,lnDivision,lnShipVia
STORE '' TO laCodes[1,4],laCodes[2,4],laCodes[3,4]
STORE .F. TO laCodes[1,5],laCodes[1,6],laCodes[2,5],laCodes[2,6],laCodes[3,5],laCodes[3,6]
laCodes[1,1] = 'CTERMCODE'
laCodes[1,2] = 'laTerms'
laCodes[1,3] = 'lnTerms'
laCodes[1,10] = 'cTermCode'
= gfwCodePop( @laCodes, "CTERMCODE" ,'D' )  
laCodes[2,1] = 'SHIPVIA'
laCodes[2,2] = 'laShipVia'
laCodes[2,3] = 'lnShipVia'
laCodes[2,10] = 'SHIPVIA'
= gfwCodePop( @laCodes, "SHIPVIA" ,'D' )  
laCodes[3,1] = 'CDIVISION'
laCodes[3,2] = 'laDivision'
laCodes[3,3] = 'lnDivision'
laCodes[3,10] = 'cDivision'
= gfwCodePop( @laCodes, "CDIVISION" ,'D' )  

*--Read codes from defaults.
lcDEFDiv  = laDivision[lnDivision,2]
lcDEFTerms= laTerms[lnTerms,2]
lcDEFShpVa= laShipVia[lnShipVia,2]

SELECT (FABTEMP4)
SCAN FOR !EMPTY(Vendor) AND nFabTotQty<>0 
  lcPOKey = Vendor+cFabGrade+DTOC(Complete)+cPriceCur+cDutyCur+STR(nPriceRat,9,4)+STR(nDutyRat,9,4)
  =SEEK(cWareCode,'WAREHOUS')
  =SEEK(Vendor,'APVENDOR')
  SELECT (lcPOTmpHD)
  IF SEEK(lcPOKey)
    lnQtyToAcc =  &FABTEMP4..nFabTotQty
    REPLACE LASTLINE    WITH LASTLINE+1,;
            NFABORDER   WITH NFABORDER+ lnQtyToAcc,;
            NPO_OPEN    WITH NPO_OPEN + lnQtyToAcc,;
            NCOST1      WITH NCOST1   + (lnQtyToAcc * &FABTEMP4..nFabcost  ),;
            NCOST2      WITH NCOST2   + (lnQtyToAcc * &FABTEMP4..nItm_Frt  ),;
            NCOST3      WITH NCOST3   + (lnQtyToAcc * &FABTEMP4..nItem_Tax ),;
            NCOST4      WITH NCOST4   + (lnQtyToAcc * &FABTEMP4..nItemQuota),;
            NECOST1     WITH NECOST1  + (NFABORDER * lfGetEquv(&FABTEMP4..cPriceCur,&FABTEMP4..nFabcost,&FABTEMP4..nPriceRat,&FABTEMP4..nCurrUnit)),;
            NECOST2     WITH NECOST2  + (NFABORDER * lfGetEquv(&FABTEMP4..cDutyCur,&FABTEMP4..nItm_Frt,&FABTEMP4..nDutyRat,&FABTEMP4..nDCurUnit)),;
            NECOST3     WITH NECOST3  + (NFABORDER * lfGetEquv(&FABTEMP4..cDutyCur,&FABTEMP4..nItem_Tax,&FABTEMP4..nDutyRat,&FABTEMP4..nDCurUnit)),;
            NECOST4     WITH NECOST4  + (NFABORDER * lfGetEquv(&FABTEMP4..cDutyCur,&FABTEMP4..nItemQuota,&FABTEMP4..nDutyRat,&FABTEMP4..nDCurUnit)),;
            POTOTAL     WITH NECOST1+NECOST2+NECOST3+NECOST4
  ELSE
    lcMatPo = IIF(llGENMAPON,gfSequence('POMAT'),lfSequence())
    APPEND BLANK  
    REPLACE POMAT       WITH lcMatPo,;
            CMATTYPE    WITH 'P',;
            STATUS      WITH 'O',;  
            VENDOR      WITH &FABTEMP4..Vendor,;
            ENTERED     WITH gdSysDate,;
            COMPLETE    WITH &FABTEMP4..Complete ,;
            CPRICECUR   WITH &FABTEMP4..cPriceCur,;
            CDUTYCUR    WITH &FABTEMP4..cDutyCur ,;
            NPRICERAT   WITH &FABTEMP4..nPriceRat,;
            NDUTYRAT    WITH &FABTEMP4..nDutyRat ,;
            CFABGRADE   WITH &FABTEMP4..cFabGrade,;
            LASTLINE    WITH 1,;
            CONTACT     WITH APVENDOR.CVENCONT,;
            PHONE       WITH APVENDOR.CPHONENO,;
            CDIVISION   WITH IIF(EMPTY(APVENDOR.CDIVISION),lcDEFDiv,APVENDOR.CDIVISION),;
            CTERMCODE   WITH IIF(EMPTY(APVENDOR.CTERMCODE),lcDEFTerms,APVENDOR.CTERMCODE),;
            SHIPVIA     WITH lcDEFShpVa
    REPLACE NFABORDER   WITH &FABTEMP4..nFabTotQty,;
            NPO_OPEN    WITH NFABORDER ,;
            NCOST1      WITH NFABORDER * &FABTEMP4..nFabcost  ,;
            NCOST2      WITH NFABORDER * &FABTEMP4..nItm_Frt  ,;
            NCOST3      WITH NFABORDER * &FABTEMP4..nItem_Tax ,;
            NCOST4      WITH NFABORDER * &FABTEMP4..nItemQuota,;
            NECOST1     WITH NFABORDER * lfGetEquv(&FABTEMP4..cPriceCur,&FABTEMP4..nFabcost,&FABTEMP4..nPriceRat,&FABTEMP4..nCurrUnit),;
            NECOST2     WITH NFABORDER * lfGetEquv(&FABTEMP4..cDutyCur,&FABTEMP4..nItm_Frt,&FABTEMP4..nDutyRat,&FABTEMP4..nDCurUnit),;
            NECOST3     WITH NFABORDER * lfGetEquv(&FABTEMP4..cDutyCur,&FABTEMP4..nItem_Tax,&FABTEMP4..nDutyRat,&FABTEMP4..nDCurUnit),;
            NECOST4     WITH NFABORDER * lfGetEquv(&FABTEMP4..cDutyCur,&FABTEMP4..nItemQuota,&FABTEMP4..nDutyRat,&FABTEMP4..nDCurUnit),;
            POTOTAL     WITH NECOST1+NECOST2+NECOST3+NECOST4
    REPLACE CWARECODE   WITH &FABTEMP4..cWareCode,;
            COUTADDR1   WITH WAREHOUS.CADDRESS1,;
            COUTADDR2   WITH WAREHOUS.CADDRESS2,;
            COUTADDR3   WITH WAREHOUS.CADDRESS3,;
            COUTADDR4   WITH WAREHOUS.CADDRESS4,;
            COUTADDR5   WITH WAREHOUS.CADDRESS5,;
            LINK_CODE   WITH 'DEFDEF',;
            CADD_USER   WITH gcUser_id,;
            DADD_DATE   WITH gdSysDate,;
            CADD_TIME   WITH TIME()
  ENDIF

  *--Update Material P/O lines.
  =SEEK(&FABTEMP4..Fabric+&FABTEMP4..Color,'FABRIC')
  SELECT (lcPOTmpLN)
  APPEND BLANK
  REPLACE CMATTYPE    WITH 'P',;
          POMAT       WITH &lcPOTmpHD..POMAT,;
          TRANCD      WITH '1',;
          LINENO      WITH &lcPOTmpHD..LASTLINE,;
          VENDOR      WITH &lcPOTmpHD..VENDOR,;
          FABRIC      WITH &FABTEMP4..Fabric,;
          COLOR       WITH &FABTEMP4..Color,;
          PATTERN     WITH FABRIC.Pattern,;
          REFERENCE   WITH 'Refer To Material Requirements',;
          WIDTH       WITH FABRIC.Width,;
          NFABTOTQTY  WITH &FABTEMP4..nFabTotQty,;
          CWARECODE   WITH &lcPOTmpHD..cWareCode
  REPLACE NCOST1      WITH &FABTEMP4..nFabcost ,;
          NCOST2      WITH &FABTEMP4..nItm_Frt ,;
          NCOST3      WITH &FABTEMP4..nItem_Tax,;
          NCOST4      WITH &FABTEMP4..nItemQuota,;
          NECOST1     WITH lfGetEquv(&FABTEMP4..cPriceCur,&FABTEMP4..nFabcost,&FABTEMP4..nPriceRat,&FABTEMP4..nCurrUnit),;
          NECOST2     WITH lfGetEquv(&FABTEMP4..cDutyCur,&FABTEMP4..nItm_Frt,&FABTEMP4..nDutyRat,&FABTEMP4..nDCurUnit),;
          NECOST3     WITH lfGetEquv(&FABTEMP4..cDutyCur,&FABTEMP4..nItem_Tax,&FABTEMP4..nDutyRat,&FABTEMP4..nDCurUnit),;
          NECOST4     WITH lfGetEquv(&FABTEMP4..cDutyCur,&FABTEMP4..nItemQuota,&FABTEMP4..nDutyRat,&FABTEMP4..nDCurUnit),;
          CFABGRADE   WITH &FABTEMP4..cFabGrade,;
          CADD_USER   WITH gcUser_id,;
          DADD_DATE   WITH gdSysDate,;
          CADD_TIME   WITH TIME()
    
ENDSCAN

*--Edit warehous and some additional P/O information.
lcBrTtl   = 'Generated P/Os'
SELECT (lcPOTmpHD)
LOCATE
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrwTitl)
ON KEY LABEL ESCAPE     DO lpEsc
lcBsWind=gfTempName()
lcBrwCont=gfTempName()
lcPushB=gfTempName()
lcBmpOK= gcBmpHome + "OK.bmp"
lcMainTtl='Material Requirements'
lcBrwTitl='Generated P/Os'
DO(gcRepHome+gcAct_Appl+"\MaPOMR.spx")
ON KEY LABEL ALT+B
ON KEY LABEL ESCAPE

IF !llGENMAPON
  LOCATE
  SCAN
    SELECT (lcPOTmpLN)
    REPLACE ALL POMAT WITH &lcPOTmpHD..cTmpPoMat FOR POMAT = &lcPOTmpHD..POMAT
    SELECT (lcPOTmpHD)
    REPLACE POMAT WITH cTmpPoMat 
  ENDSCAN
ENDIF

*--Start update the master files.
SELECT (lcPOTmpLN)
SCAN 

  *-1) Update FABRIC and FABDYE file.-------------------------------
  =SEEK(Fabric+Color,'FABRIC')
  lnOnOrder = &lcPOTmpLN..nFabTOTQTY * FABRIC.CONV 
  SELECT FABRIC
  =RLOCK()
  REPLACE ONORDER WITH ONORDER + lnOnOrder
  UNLOCK
  SELECT (lcPOTmpHD)
  LOCATE FOR POMAT = &lcPOTmpLN..POMAT
  lcHdrLoc = cWareCode
  SELECT FABDYE
  IF !SEEK(&lcPOTmpLN..Fabric+&lcPOTmpLN..Color+lcHdrLoc,'FABDYE')
    APPEND BLANK
    REPLACE FABRIC     WITH &lcPOTmpLN..FABRIC,;
            COLOR      WITH &lcPOTmpLN..COLOR,;
            CWARECODE  WITH lcHdrLoc,;
            nFAve_Cost WITH FABRIC.nFAve_Cost,;
            nAveCstBuy WITH FABRIC.nAveCstBuy
  ENDIF
  =RLOCK()
  REPLACE ONORDER WITH ONORDER + lnOnOrder
  UNLOCK

  *-2) Update APVENDOR file.---------------------------------------
  SELECT APVENDOR  
  =SEEK(&lcPOTmpLN..Vendor)
  =RLOCK()
  REPLACE nVenOpnPO WITH nVenOpnPO + &lcPOTmpLN..nEcost1
  UNLOCK        

ENDSCAN

*-4) Update POFHDR file.--------------------------------------------
SELECT POFLN
APPEND FROM (gcWorkDir+lcPOTmpLN)

*-3) Update POFLN file.---------------------------------------------

SELECT POFHDR
APPEND FROM (gcWorkDir+lcPOTmpHD)

*--Would you like to preview/print the purchase orders just created?
*--<Preview>  <Print>  <None>.
lnSelDev = gfModalGen('QRM36125B36010','DIALOG')
IF lnSelDev <> 3
  IF lnSelDev = 1
    gcDevice = 'SCREEN'
  ELSE
    IF !pSetup(.T.,.F.)
      RETURN
    ENDIF  
  ENDIF

  *--Set the print po report tag.
  SET ORDER TO TAG POFLN IN POFLN
  
  SELECT (lcPOTmpHD)
  INDEX ON POMat TAG (lcPOTmpHD)

  *--Run Print Material P/O form program...
  lcSavRName  = lcRpRName
  lcRpRName   = 'MAMATPA'
  lcSavTmpFrm = lcOGTmpForm
  lcOGTmpForm = ' '
  =lfRepPltFr(lcRpRName)
      
  IF !USED('SYCCOMP')
    USE (gcSysHome+'SYCCOMP') ORDER TAG Ccomp_id IN 0 SHARED
  ENDIF

  DO (gcRepHome+gcAct_Appl+'\MAMATP') WITH lcPOTmpHD
  lcOGTmpForm = lcSavTmpFrm
  lcRpRName   = 'MAROBMAT'
  =lfRepPltFr(lcRpRName)
  lcRpRName   = lcSavRName

ENDIF

*--Remove the P/O temp files.
USE IN (lcPOTmpLN)
USE IN (lcPOTmpHD)

ERASE (gcWorkDir+lcPOTmpHD+'.DBF')
ERASE (gcWorkDir+lcPOTmpHD+'.CDX')
ERASE (gcWorkDir+lcPOTmpLN+'.DBF')
ERASE (gcWorkDir+lcPOTmpLN+'.CDX')

RETURN .T.
      
*!*************************************************************
*! Name      : lfvEdtMat
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/28/2002
*! Purpose   : Edit -ve Remaining line information.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvEdtMat()
*!*************************************************************
FUNCTION lfvEdtMat

*--Initialize screen variables.
llOk = .F.
SCATTER MEMVAR
STORE 1 TO lnCurrUnt1,lnCurrUnt2

lnSavRec = RECNO()
DO (gcRepHome+"MA\MAEDTFB.SPX")
IF llOk
  IF Vendor <> m.Vendor OR cPriceCur<>m.cPriceCur OR nPriceRat<>m.nPriceRat
    SELECT (FABTEMP4)   
    LOCATE FOR Vendor = m.Vendor AND (cPriceCur<>m.cPriceCur OR nPriceRat<>m.nPriceRat)
    IF FOUND()
      *--This vendor has other PO lines. Replace all with new currency/rate ?","Yes;No
      IF gfModalGen('QRM36126B36001','DIALOG') = 1
        REPLACE ALL cPriceCur WITH m.cPriceCur,;
                    cDutyCur  WITH m.cDutyCur,;
                    nPriceRat WITH m.nPriceRat,;
                    nDutyRat  WITH m.nDutyRat,;
                    nCurrUnit WITH lnCurrUnt1,;
                    nDCurUnit WITH lnCurrUnt2  FOR Vendor = m.Vendor
      ENDIF
    ENDIF
  ENDIF
  SELECT (FABTEMP4)   
  GOTO lnSavRec
  GATHER MEMVAR
  REPLACE nCurrUnit  WITH lnCurrUnt1,;
          nDCurUnit  WITH lnCurrUnt2
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGetEquv
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/28/2002
*! Purpose   : Get equivelent cost in Edit -ve Remaining screen.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfGetEquv()
*!*************************************************************
FUNCTION lfGetEquv
PARA lcPcrcy,lnFrnCost,lnCurRate,lnCurUnt

lcPMethod = gfGetExSin(@lcPUnMeth,lcPcrcy)
lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
RETURN lnEquCost   

*!*************************************************************
*! Name      : lfvVendor
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/28/2002
*! Purpose   : Validate vendor in Edit -ve Remaining screen.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvVendor()
*!*************************************************************
FUNCTION lfvVendor
PRIVATE lcBrFields

IF !SEEK(m.Vendor , 'APVENDOR')
  lcBrFields = "CVENDCODE :H='Vendor',"+;
               "CVENCOMP  :H='Name',"+;
               "cPhoneNo  :H='Phone',"+;
               "cVenOurAc :H='Our Account' "
  SELECT APVENDOR
  IF gfBrows('','cVendCode', 'laTempArr','Vendors')
    m.Vendor=APVENDOR.cVendCode
  ELSE
    m.Vendor=SPACE(8)
  ENDIF
  SHOW GET m.Vendor
  SELECT (FABTEMP4)   
ENDIF
=lfrefresh()
RETURN

*!*************************************************************
*! Name      : lfvEdtLoc
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/28/2002
*! Purpose   : Validate the location.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvEdtLoc()
*!*************************************************************
FUNCTION lfvEdtLoc

IF !SEEK(&lcPOTmpHD..cWareCode ,'WareHous' )
  SELECT (lcPOTmpHD)
  lcWarCd=gfBrowWare(.F.)
  REPLACE cWareCode WITH lcWarCd,;
          COUTADDR1 WITH WAREHOUS.CADDRESS1,;
          COUTADDR2 WITH WAREHOUS.CADDRESS2,;
          COUTADDR3 WITH WAREHOUS.CADDRESS3,;
          COUTADDR4 WITH WAREHOUS.CADDRESS4,;
          COUTADDR5 WITH WAREHOUS.CADDRESS5
ENDIF  
RETURN

*!*************************************************************
*! Name      : lfvCurrency
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/28/2002
*! Purpose   : Validate currency in Edit -ve Remaining screen.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvCurrency()
*!*************************************************************
FUNCTION lfvCurrency
PARA lcCurTyp

IF (lcCurTyp='P' AND cPriceCur = m.cPriceCur) OR ;
   (lcCurTyp='D' AND cDutyCur = m.cDutyCur)
  RETURN
ENDIF

lcCurrency = IIF(lcCurTyp='P',m.cPriceCur,m.cDutyCur)
=gfOpenFile(gcSysHome+'syccurr','Ccurrcode','SH')
=gfcurrbrow(@lcCurrency)
SELECT (FABTEMP4)   
IF lcCurTyp='P'
  m.cPriceCur= IIF(EMPTY(lcCurrency),gcBaseCurr,lcCurrency)
  IF m.cPriceCur <> cPriceCur
    m.nPriceRat=gfChkRate('lnCurrUnt1',m.cPriceCur,gdSysDate,llEditExRt,gcAct_comp,.F.)
    m.nPriceRat=IIF(m.nPriceRat=0 ,1,m.nPriceRat)
    lnCurrUnt1 =IIF(lnCurrUnt1 =0 ,1,lnCurrUnt1)
  ENDIF
  SHOW GET m.cPriceCur
  SHOW GET m.nPriceRat
ELSE
  m.cDutyCur= IIF(EMPTY(lcCurrency),gcBaseCurr,lcCurrency)
  IF m.cDutyCur <> cDutyCur
    m.nDutyRat = gfChkRate('lnCurrUnt2',m.cDutyCur ,gdSysDate,llEditExRt,gcAct_comp,.F.)
    m.nDutyRat = IIF(m.nDutyRat = 0 ,1,m.nDutyRat)
    lnCurrUnt2 = IIF(lnCurrUnt2 = 0 ,1,lnCurrUnt2)
  ENDIF
  SHOW GET m.cDutyCur
  SHOW GET m.nDutyRat
ENDIF
=lfrefresh()
RETURN

*!*************************************************************
*! Name      : lfvOKFab
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/28/2002
*! Purpose   : Valid Ok in Edit -ve Remaining screen.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvOKFab()
*!*************************************************************
FUNCTION lfvOKFab

IF m.nFabTotQty=0
  *--Total quantity is 0 !! This line will not be included in the P/O.
  = gfModalGen('INM36127B36000','DIALOG')
  *--Update Changes.
  GATHER MEMVAR
  CLEAR READ
  RETURN
ENDIF
llOk = .T.
CLEAR READ
RETURN

*!**************************************************************************
*! PROG : MAT939
*! DATE: 11/28/2002
*! DESC : PROGRAM GATHER MATERIALS FOR WHICH THERE IS NEGATIVE REMAINING
*!        QUANTITY
*! AUTH : AHMED MAHER (AMH)
*! NOTES: CALLED FROM MAT932, MAT933 AND MAT934 PROCEDURES
*!**************************************************************************
PROCEDURE MAT939

*** FILL THE TEMP FILE WITH ITEM CODE, COLOR, VENDOR, QUANTITY NEEDED,
*** LEADTIME AND COMPLETION DATE.
SELECT (FABTEMP4)
APPEND BLANK

*** THE VARIABLE XPRV_KEY CONTAINS THE CONTENTS OF FABRIC+COLOR OF THE
*** CURRENCT RECORD. GET THE FIRST 7 CHARACTERS TO REPRESENT THE FABRIC
*** AND THE NEXT 3 CHARACTERS TO REPRESENT THE COLOR.

lnFabcost = FABRIC.nFabcost
lcUOMBuy  = FABRIC.UOMBUY
REPLACE FABRIC     WITH SUBSTR(XPRV_KEY,01,7), ;
        COLOR      WITH SUBSTR(XPRV_KEY,13,6), ;
        VENDOR     WITH XFVENDOR, ;
        nFabTotQty WITH ROUND( (XREQUIRED/XCONV),2), ;
        LEADTIME   WITH FABRIC.LEADTIME, ;
        COMPLETE   WITH gdSysDate+LEADTIME,;
        UOMBuy     WITH lcUOMBuy,;
        nFabcost   WITH lnFabcost
RETURN

*:************************************************************************
*: Program file  : MAT935
*: Program desc. : Program to calculate the WIP actualize also calculate
*:                 Open and hold WIP , Issued qty and the net required.
*: Date  : 11/27/2002
*: Developer: AHMED MAHER (AMH)
*:************************************************************************
PROCEDURE MAT935

SELECT (FABTEMP)
lcItems   = Fabric
lcCItem   = FClr
xKey      = Style + lcSeprator + Sclr
lcTyp     = Typ
lcCatgTyp = cCatgTyp
lcWrkLnFl = IIF(lMake,'CUTTKTL','POSLN')
lcWrkHdFl = IIF(lMake,'CUTTKTH','POSHDR')
IF !SEEK(xKey,lcWrkLnFl)
  RETURN
ENDIF
SELECT (lcWrkLnFl)
llFrst = .T.
lcAvlStat = IIF(llStyChged,'AOH','O')

STORE 0 TO lnYTOwip,lnTotOwip
DO WHILE Style = xKey
  IF EOF()
    EXIT
  ENDIF  

  IF ! (&lcWrkHdFl..Status $ lcAvlStat )
    SKIP
    LOOP
  ENDIF

  IF llFrst
    xCut = IIF(&FABTEMP..lMake,Cuttkt,Po)
    llFrst = .F.
  ENDIF
  
  lcCStatus = &lcWrkHdFl..Status
  lcSLineNo = IIF(&FABTEMP..lMake,'     0',STR(&lcWrkLnFl..LineNo,6))

  DO CASE
    CASE TRANCD = '1'
      FOR X = 1 TO 8
        Z = STR(X,1)
        XWIP2(X) = XWIP2(X) + MAX( QTY&Z , 0 )
      ENDFOR
    CASE (STYLE.Make AND TRANCD $ '234' ) .OR. (!&FABTEMP..lMake AND TRANCD $ '245')
      FOR X = 1 TO 8
        Z = STR(X,1)
        XWIP2(X) = MAX( XWIP2(X) - ABS( QTY&Z  ) , 0 )
      ENDFOR
  ENDCASE

  SKIP
  IF xCut <> IIF(&FABTEMP..lMake,Cuttkt,Po)  .OR. Style <> xKey
    lnTotOwip = 0     &&Total open c/t qty.
    FOR X=1 TO 8
      DO CASE
        CASE lcCStatus = 'A'
          XWIP(X)=XWIP(X) + XWIP2(X)
          XWIPOTC(X) = XWIPOTC(X) + XWIP2(X)
        CASE lcCStatus = 'H'
          xWIPh(X)=xWIPh(X) + XWIP2(X)
        CASE lcCStatus = 'O'
          lnTotOwip = lnTotOwip + XWIP2(X)
          xWIPo(X)=xWIPo(X) + XWIP2(X)
          XWIPOTC(X) = XWIPOTC(X) + XWIP2(X)
      ENDCASE
    ENDFOR
    IF lcCStatus = 'O'
      SELECT BOMLINE 
      lcBomLnKey = IIF(&FABTEMP..lMake,'M','I')+'1'+xCut+lcSLineNo+lcTyp+xKey+SPACE(6)+;
                   IIF(lcCatgTyp='S',lcItems+lcSeprator,PADR(lcItems,19))+lcCItem
      IF SEEK(lcBomLnKey)
        lnYTOwip = lnYTOwip + (lnTotOwip * BOMLINE.UnitQty)
        SELECT (FABTEMP)
        REPLACE nYTOWIP WITH lnYTOwip
        IF !lMake AND !SEEK(lcItems+lcCItem+xCUT,PoNoTmp)
          SELECT (PoNoTmp)
          APPEND BLANK
          REPLACE cPONo WITH xCUT,;
                  Item  WITH lcItems,;
                  IClr  WITH lcCItem
        ENDIF
      ENDIF
      SELECT (lcWrkLnFl)
    ENDIF
    lnTotOwip=0
    xCUT=IIF(&FABTEMP..lMake,Cuttkt,Po)
    STORE 0 TO XWIP2
  ENDIF
  
ENDDO
SELECT (FABTEMP)
RETURN

*:************************************************************************
*: Program file  : lfGetIssu
*: Program desc. : Function to get the issued and net required.
*: Developer: AHMED MAHER (AMH)
*:************************************************************************
FUNCTION lfGetIssu

lcTyp     = Typ
lcStyle   = Style
lcItems   = Fabric
lcCItem   = FClr
lcCatgTyp = cCatgTyp
IF lMake
  SELECT CUTTKTH
  SET ORDER TO TAG CUTTKTHS
  SEEK lcStyle
  SCAN WHILE STYLE = lcStyle FOR Status='O'
    xCut=Cuttkt
    SELECT CTKTBOM
    STORE 0 TO lnIssue,lnNRequ
    IF SEEK('M'+xCut+lcTyp+IIF(lcCatgTyp='S',lcItems+lcSeprator,PADR(lcItems,19))+lcCItem)
      SUM REST Used_Qty,Req_Qty-Used_Qty TO lnIssue,lnNRequ ;
               WHILE cIMTyp+Cuttkt+typ+Item+IClr =;
               'M'+xCut+lcTyp+IIF(lcCatgTyp='S',lcItems+lcSeprator,PADR(lcItems,19))+lcCItem
    ENDIF
    SELECT CUTTKTH
    lnCTUsdQy = lnCTUsdQy+lnIssue
    lnCTNetRq = lnCTNetRq+lnNRequ
  ENDSCAN
  SELECT CUTTKTH
  SET ORDER TO TAG CUTTKTH
ELSE
  STORE 0 TO lnIssue,lnNRequ
  SELECT (PoNoTmp)
  SEEK lcItems+lcCItem
  SCAN WHILE Item+IClr = lcItems+lcCItem
    xCut=cPONo
    SELECT CTKTBOM
    STORE 0 TO lnIssue,lnNRequ
    IF SEEK('I'+xCut+lcTyp+IIF(lcCatgTyp='S',lcItems+lcSeprator,PADR(lcItems,19))+lcCItem)
      SUM REST Used_Qty,Req_Qty-Used_Qty TO lnIssue,lnNRequ ;
               WHILE cIMTyp+Cuttkt+typ+Item+IClr =;
               'I'+xCut+lcTyp+IIF(lcCatgTyp='S',lcItems+lcSeprator,PADR(lcItems,19))+lcCItem
    ENDIF
    lnPOUsdQy = lnPOUsdQy+lnIssue
    lnPONetRq = lnPONetRq+lnNRequ
  ENDSCAN
ENDIF

*!*************************************************************
*! Name      : lpPrnt_Note
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/28/2002
*! Purpose   : Print the notepad. 
*!*************************************************************
*! Calls     : 
*!             Procedures : lpHeaders
*!*************************************************************
*! Example            :  Do lpPrnt_Note
*!*************************************************************
PROCEDURE lpPrnt_Note
PARAMETER llFabTrim

STORE .T. TO llCount
XOLD_ALIAS = SELECT()
SELECT NOTEPAD
IF lcPAstNot = 'Y' .AND. SEEK ('G'+lcMaterial)
  lnMemoWdth = SET('MEMOWIDTH')
  SET MEMOWIDTH TO 75
  lnMemoLnNo = 0      
  lnMemolins = 0
  IF ROW > 53
    IF llFabTrim
      DO lpHeaders
    ELSE
      DO lpTHeaders
    ENDIF
  ENDIF
  lnMemolins = MEMLINES(mNotes)
  IF lnMemolins > 0
    ROW = ROW + 1
    llFirstLine=.T.
    DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
      IF ROW > 53
        IF llFabTrim
          DO lpHeaders      
        ELSE
          DO lpTHeaders      
        ENDIF
        llCount =.T.
     ENDIF
      lnMemoLnNo = lnMemoLnNo + 1
      lcMline    = MLINE(mNotes,lnMemoLnNo)  
      IF UPPER(SUBSTR(ALLTRIM(MLINE(mNotes,lnMemoLnNo)),1,9)) <> "*APPROVAL"
        LOOP
      ENDIF
      lcText = SUBSTR(ALLTRIM(MLINE(mNotes,lnMemoLnNo)),10,LEN(ALLTRIM(MLINE(mNotes,lnMemoLnNo))))
      IF llFirstLine
        @ ROW,00 SAY '**Notes  '
        ROW = ROW + 1
        @ ROW,00 SAY lcText
        llFirstLine = .F.
      ELSE 
        IF llCount
          lnPCol    = 01
          IF (lnPCol+ lnOldText + LEN(lcText)) > 131
            lnPCol    = 01
          ELSE
            lnPCol = lnPCol + lnOldText + 1
          ENDIF  
          llCount = .F.
        ELSE
          IF (lnPCol+ lnOldText + LEN(lcText)) > 131
            lnPCol = 01
            ROW = ROW + 1
          ELSE
            lnPCol = lnPCol + lnOldText + 1
          ENDIF
        ENDIF  
        @ Row,lnPCol SAY lcText        
      ENDIF  
      lnOldText = LEN(lcText)
    ENDDO
    ROW = ROW + 1
  ENDIF
  SET MEMOWIDTH TO (lnMemoWdth)
ENDIF
SELECT (XOLD_ALIAS)

*!*************************************************************
*! Name      : lpHeaders
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/28/2002
*! Purpose   : Print the header.
*!*************************************************************
*! Calls     : 
*!             Procedures : RPT_HDR
*!*************************************************************
*! Example            :  Do lpHeaders
*!*************************************************************
PROCEDURE lpHeaders

PAGENO = PAGENO + 1
DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
DO CASE
  CASE XFMT='D' .AND. XLS='L'       && DETAIL LONG FORM
    @ 05,00 SAY ' FABRIC  COLOR  COLOR DESCRIPTION    FABRIC DESCRIPTION     ONHAND  ONORDER    P.RETURN    AVAIL '
    @ 06,00 SAY ' STYLE         DESCRIPTION' 
    @ 07,00 SAY REPLICATE('=',132)
    ROW=8
    
  CASE XFMT='D' .AND. XLS='S'       && DETAIL SHORT FORM
    @ 05,00 SAY ' FABRIC   COLOR  COLOR DESCRIPTION    FABRIC DESCRIPTION        ONHAND                 ONORDER               P.RETURN      AVAILABLE'
    @ 06,00 SAY REPLICATE('=',132)
    ROW=7

  CASE XFMT='S'                      && SUMMARY FORM
    @ 05,00 SAY ' FABRIC  COLOR  CLR DESCRIPTION FABRIC DESCRIPTION     ONHAND  ONORDER    P.RETURN    AVAIL    PIECES REQ. USE ISSU. USE  REMAIN USE'
    @ 06,00 SAY REPLICATE('=',132)
    ROW=7
ENDCASE

*!*************************************************************
*! Name      : lpTHeaders
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/28/2002
*! Purpose   : Print the trim header.
*!*************************************************************
*! Calls     : 
*!             Procedures : RPT_HDR
*!*************************************************************
*! Example            :  Do lpTHeaders
*!*************************************************************
PROCEDURE lpTHeaders

PAGENO = PAGENO+1
DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
DO CASE
  CASE XFMT='D' .AND. XLS='L'
    @ 05,00 SAY ' TRIM    COLOR  COLOR DESCRIPTION    TRIM DESCRIPTION       ONHAND  ONORDER    AVAIL'   && WAM09/19/93
    @ 06,00 SAY ' STYLE          DESCRIPTION'   &&TAK03/01/94
    @ 07,00 SAY REPLICATE('=',132)
    ROW=8

  CASE XFMT='D' .AND. XLS='S'
    @ 05,00 SAY ' TRIM     COLOR  COLOR DESCRIPTION    TRIM DESCRIPTION          ONHAND                 ONORDER               AVAILABLE'  &&MODI WAM09/19/93
    @ 06,00 SAY REPLICATE('=',132)
    ROW=7

  CASE XFMT='S'
    @ 05,00 SAY ' TRIM    COLOR  CLR DESCRIPTION TRIM DESCRIPTION       ONHAND  ONORDER    AVAIL                PIECES REQ. USE ISSU. USE  REMAIN USE'
    @ 06,00 SAY REPLICATE('=',132)
    ROW=7
ENDCASE

*!*************************************************************
*! Name      : lfMatRetrn
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/27/2002
*! Purpose   : To calculate the returned qty for each fabric/color
*!*************************************************************
*! Returns            : lnRetQty : the opened qty.
*!*************************************************************
*! Example            :  lfMatRetrn()
*!*************************************************************
FUNCTION lfMatRetrn
PARAMETERS lcFab,lcColor

PRIVATE lnAlias,lnRetQty,lcOldOrder
lnAlias = SELECT(0)
lnRetQty = 0
SELECT POFLN
lcOldOrder = SET('ORDER')
SET ORDER TO TAG POFLNF
IF SEEK(lcFab+lcColor+'R')
  SCAN REST WHILE fabric+color+cmattype+pomat+trancd = lcFab+lcColor+'R'
    =SEEK('R'+POMAT,'POFHDR')
    IF POFHDR.Status $ "OH"
      lnRetQty = lnRetQty + IIF(TranCd = '1',NFABTOTQTY,-NFABTOTQTY)
    ENDIF
  ENDSCAN
ENDIF
SET ORDER TO &lcOldOrder.
SELECT (lnAlias)
RETURN (lnRetQty)

*!*************************************************************
*! Name      : lfvFabric
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/26/2002
*! Purpose   : Validate primary fabric and fabrics in range
*!             entered in grid.
*!*************************************************************
*! Calls     : FaBrow()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
lcFabric = EVALUATE(SYS(18))
IF !EMPTY(lcOldValue) AND lcFabric = lcOldValue
  RETURN
ENDIF
lcFldLocNam = SYS(18)
IF !EMPTY(lcFabric) AND !SEEK(lcFabric,'FABRIC')
  = FaBrow(@lcFabric,'*')
  &lcFldLocNam = IIF(!EMPTY(lcFabric),lcFabric,lcOldValue)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfOldValue
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/26/2002
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN

*!*************************************************************
*! Name      : lfStySum
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/26/2002
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
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
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
*! Name      : lfsrvSty
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/26/2002
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
FUNCTION lfSRSty
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
*! Name      : lfwRepWhen
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/26/2002
*! Purpose   : Optional Grid When Function.
*!*************************************************************
*! Calls     : ...........
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

lcOldValue = ' '
*--Get the style major picture.
lcStyPict = '@! '+gfItemMask('PM')
*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
=lfGetColor()

RETURN

*!*************************************************************
*! Name      : lfGetColor
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/26/2002
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
    EXIT
  ENDIF
ENDFOR
RETURN

*!**************************************************************************
*! Name      : lfSequence
*! Developer : AHMED MAHER
*! Date      : 12/01/2002
*! Purpose   : create a temp. sequence no for PO
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfSequence()
*!**************************************************************************
FUNCTION lfSequence

PRIVATE lnSequence
lnSequence = 0
SCAN
  lnSequence = MAX(lnSequence,VAL(POMAT))
ENDSCAN
=SEEK(SPACE(20)) 
RETURN PADL(ALLTRIM(STR(lnSequence+1)),6,"0")

*!**************************************************************************
*! Name      : lfvTmpPoMa
*! Developer : AHMED MAHER
*! Date      : 12/01/2002
*! Purpose   : validate MA po no enterd by user
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: llCheckAll = .T. if it called to chaeck all records in the file
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfSequence()
*!**************************************************************************
FUNCTION lfvTmpPoMa
PARAMETER llCheckAll

PRIVATE lnMessage
IF !llCheckAll
  lcTmpPoMat = EVALUATE(SYS(18))
  lcField    = SYS(18)
ELSE
  lcTmpPoMat = ''
  GO TOP
ENDIF
llRet=.T.
SCAN REST
  IF llCheckAll
    lcTmpPoMat = cTmpPoMat
  ENDIF
  lnRecNo = RECNO()
  COUNT TO lnRecord FOR cTmpPoMat = lcTmpPoMat
  Go Record lnRecNo
  IF lnRecord > 1  OR SEEK("P"+lcTmpPoMat,"POFHDR")
    lnMessage = IIF(llCheckAll,0,gfModalGen('TRM34023B34000','DIALOG',"Material Po. "))
    llRet=.F.
  ENDIF
  IF EMPTY(lcTmpPoMat)
    *-You cannot leave the Materila Po. number empty.
    lnMessage = IIF(llCheckAll,0,gfModalGen('TRM34021B34000','DIALOG',"Material Po. "))
    llRet=.F.
  ENDIF
  IF llRet AND LEN(ALLTRIM(lcTmpPoMat)) < 6
    *- "MA PO  number must be six digits.
    lnMessage = IIF(llCheckAll,0,gfModalGen('TRM34022B34000','DIALOG',"Material Po. "))
    llRet=.F.
  ENDIF
  IF !llCheckAll
    EXIT
  ENDIF
ENDSCAN
IF !llRet .AND. !llCheckAll
  lcTmpPoMat = ''
  REPLACE &lcField WITH lcTmpPoMat
  KEYBOARD CHR(15)
  llRet = .T.
ENDIF
RETURN llRet

*!*************************************************************
*! Name      : lfBrow
*! Developer : AHMED MAHER
*! Date      : 12/01/2002
*! Purpose   : Browse function for the Generated Po's Screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOkPO()
*!*************************************************************
FUNCTION lfBrow

lcFields = IIF(llGENMAPON,"pomat :R ","cTmpPoMat :P='@! XXXXXX' :V=lfvTmpPoMa()")+":H='P/O' ,Vendor :R :15,"+;
            "lcVenName=IIF(SEEK(Vendor,'APVENDOR'),APVENDOR.CVENCOMP,'') :H='Vendor name' : 40,"+;
            "cWareCode :P='@! XXXXXX' :H='Shipto Location' :V=lfvEdtLoc()"
         
BROWSE FIELDS &lcFields                     ;
         WINDOW (lcbrwcont)                 ;  
         IN WINDOW (lcBsWind)               ;
         LOCK 0                             ;
         NOMENU                             ;         
         NOAPPEND                           ;
         NOWAIT                             ;
         SAVE                               ;
         NOCLEAR                            ;
         NODELETE                           ;
         TITLE  (lcBrwTitl)  

*!*************************************************************
*! Name      : lfvOkPO
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/01/2002
*! Purpose   : Valid function for the ok button in the generated Po's screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOkPO()
*!*************************************************************
FUNCTION lfvOkPO

IF !llGENMAPON .AND. !lfvTmpPoMa(.T.)
  *-Material Po. Number have not been defined in some generates Pos.Please enter the Material Po.Number.
  = gfModalGen('INM36172B36000','DIALOG')
  RETURN
ENDIF

*--Check the warehous is not empty.
IF llWareHous
  LOCATE FOR EMPTY(cWareCode)
  IF FOUND()
    *-Some generated P/Os has not defined shipto location, Please enter the shipto location.
    = gfModalGen('INM36124B36000','DIALOG')
    RETURN   
  ELSE
    CLEAR READ
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfTrap
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/01/2002
*! Purpose   : Trap the Tab ,BackTab 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfTrap()
*!*************************************************************
FUNCTION lfTrap

IF WONTOP() = lcBrwTitl 
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpBackTab	  
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfClrTrap
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/01/2002
*! Purpose   : Clear the  Trap
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClrTrap()
*!*************************************************************
FUNCTION lfClrTrap

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lpTab
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/01/2002
*! Purpose   : the Tab Key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpTab()
*!*************************************************************
PROCEDURE lpTab

ON KEY LABEL TAB
IF WONTOP() = lcBrwTitl  
  ACTIVATE WINDOW (lcPushB)
  _CUROBJ = OBJNUM(pbOk)  
ENDIF

*!*************************************************************
*! Name      : lpBackTab
*! Developer : AHMED MAHER
*! Date      : 12/01/2002
*! Purpose   : The BackTab Key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpBackTab()
*!*************************************************************
PROCEDURE lpBackTab

ON KEY LABEL BACKTAB
IF WONTOP()=lcBrwTitl
  ACTIVATE WINDOW (lcPushB)
  _CUROBJ = OBJNUM(pbOk) 
ENDIF

*!*************************************************************
*! Name      : lpEsc
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/01/2002
*! Purpose   : The Escape Key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  .F.
*!*************************************************************
*! Example            :  =lpEsc()
*!*************************************************************
PROCEDURE lpEsc

RETURN .F.