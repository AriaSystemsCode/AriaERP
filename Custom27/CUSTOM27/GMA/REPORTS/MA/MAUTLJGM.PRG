*:************************************************************************
*: Program file  : MAUTLJGM.PRG 
*: Program desc. : Material Utilization Report For GMA
*: For Report    : (@x,y SAY)
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: MATERIALS (MA).
*:      Developer: Mohamed Shokry (MHM)
*:************************************************************************
*: Calls         : 
*:    Procedures : None
*:    Functions  : lfwRepWhen, lfGetColor, lfvFabBet, lfvFabRng, 
*:                 lfvpbOk, lfvSortBy, lfvSeq
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO MAUTLJGM
*:************************************************************************
*: This Report Program is due to 102585,1
*:************************************************************************
*: Modifications :
*:************************************************************************
*-- Option Grid Layout :
*-- Sort By (D)DAte (S)sequence    : D
*-- Activity Date / Sequence Range : .. to ..
*-- Item Type                      : ..
*-- Item Rnage                     : .. to ..
*-- Color                          : .. .. .. .. .. .. .. ..

*-- Note That using RANGE FUNCTION for selecting sequence in MATINVJL
*-- is not correct because starting period balance is calculated based  
*-- on selected fixed sequence (MATINVJL.CTRAN_SEQ) .
*-- We 'd used BETWEEN operator (similar to Item Range) and defined LCRPLSEQ, LCRPHSEQ 
*-- to hold the selected sequence .

*-- MAREPORT --> GFOPGRID --> MAUTLJL
*-- All needed dbfs. and indexes are initiated before calling MAUTLJL  
IF 'PO' $ gcCmpModules
  =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
ENDIF
IF 'MF' $ gcCmpModules
  =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
ENDIF
=gfOpenFile(gcDataDir+'mmfgordd',gcDataDir+'Mfgrec','SH')
IF !llDyelot  
  llRpPrtDy = .F.
ENDIF

llBegin = .F.

R_TITLE = 'MATERIAL UTILIZATION'   && report title
lcTempFile = gfTempName()    &&global function gives temporarily name for the workfile.

SELECT MATINVJL
SET RELATION TO CFABRIC+CCOLOR INTO FABRIC
SET RELATION TO CFABRIC+CCOLOR+CWARECODE INTO FABDYE

GO TOP
*-- if user selected Item Range, modify lcRpExp  
IF !EMPTY(lcRpFrmFab)       && meaning both lcRpFrmFab and lcRpToFab have values 
  lcRpExp = lcRpExp + ' AND BETWEEN(MATINVJL.CFABRIC,lcRpFrmFab,lcRpToFab)'   
ELSE   && else, lcRpFrmFab is empty and lcRpToFab has value
  IF !EMPTY(lcRpToFab)
    lcRpExp = lcRpExp + ' AND MATINVJL.CFABRIC <= lcRpToFab'       
  ENDIF
ENDIF  && end if user selected Item Range, modify lcRpExp  
*-- if user selected Item Type, modify lcRpExp  
IF !EMPTY(lcRpItmTyp)
  lcRpExp = lcRpExp + '.AND. FABRIC.ITEM_TYPE = lcRpItmTyp'
ENDIF  && end if user selected Item Type, modify lcRpExp  

COPY REST TO (gcWorkDir+lcTempFile) FOR &lcRpExp    && appends all needed records.
*-- reset the relation between MATIVJL and FABRIC
SET RELATION TO
=gfOpenFile(gcWorkDir+lcTempFile,' ','EX') &&global function opens DBF (with-optionally- its index) share or exculsive.
*-- if user selected Date Range, index lcTempFile 
IF lcrpSortby='D'
  INDEX ON cFabric+cColor+CWareCode+DTOS(dTranDate)+cTrn_Seq TAG (lcTempFile)
ELSE  && else user selected Sequence Range
  INDEX ON cFabric+cColor+CWareCode+cTrn_Seq TAG (lcTempFile)
ENDIF  && end if user selected Date Range
*-- if no records are inserted into lcTempFile
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')

  SET DEVICE TO SCREEN

  RETURN
ENDIF  && end if no records are inserted into lcTempFile
SET RELATION TO cTktNo INTO MMFGORDH ADDITIVE
IF 'PO' $ gcCmpModules
  SET RELATION TO 'P'+cTktNo INTO POSHDR ADDITIVE
ENDIF
IF 'MF' $ gcCmpModules
  SET RELATION TO cTktNo INTO CUTTKTH ADDITIVE
ENDIF

*-- XTOTCLR(1) : is an array element used to accumulate the PO-RECEIPTS . 
*-- in case of MATINVJL.cTranType='1' (PO-RECEIPT) 
*-- then we have XTOTCLR(1)= XTOTCLR(1)+nReceived .
*-- XTOTCLR(2) : is an array element used to accumulate the ADJUSTMENTS .
*-- in case of MATINVJL.cTranType='2' (Adjustment) 
*-- then we have XTOTCLR(2)=XTOTCLR(2)+IIF(nReceived>0,nReceived,IIF(nIssued > 0,-nIssued,nIssued)) 
*-- XTOTCLR(3) : is an array element used to accumulate the USAGE .
*-- in case of MATINVJL.cTranType='3' (Physical) 
*-- then we have XTOTCLR(3)=XTOTCLR(3)+nIssued
*-- XTOTFAB() : is similar to XTOTCLR() but these are totals by fabric
*-- and the other ones are totals by color.
DIMENSION XTOTCLR(3)
DIMENSION XTOTFAB(3)
STORE 0.00 TO XTOTCLR,XTOTFAB
ROW     = 99           && row no 
PAGENO  = 0            && page no
TIME    = TIME()     
XREPORT = 'MAUTLJGM'    && report name
XTITLE  = ' '          && report title

SET DEVICE TO PRINT
GO TOP                 && in lcTempFile
XFABRIC  = cFabric
XCOLOR   = cColor
XFTOTFLG =.F.
*-- laContr : stores the contractors' values
DIMENSION laContr[3]
*-- major print loop
DO WHILE INKEY() <> 32
  *-- print the header
  *-- if new page (row is initially 99 and for new page also) 
  IF ROW > 53
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    ROW=5
  ENDIF  && end if new page
  *-- XPRNT_ACT : is a flag saying that there is activity to print 
  *-- for this fabric/color 
  *-- totals by color
  IF (cFABRIC <> XFABRIC .OR. cCOLOR <> XCOLOR) .AND. XPRNT_ACT
    @ ROW,00 SAY REPLICATE('=',133)
    ROW = ROW + 1
    @ ROW,01 SAY XFABRIC+' '+ XCOLOR
    @ ROW,20 SAY 'SUB TOTAL'
    @ ROW,37 SAY 'PO-RECEIPTS :'
    
    @ ROW,52 SAY XTOTCLR(1) PICTURE '99999999.99'
    @ ROW,72 SAY 'ADJUSTMENTS :'
    
    @ ROW,87 SAY XTOTCLR(2) PICTURE '99999999.99'
    @ ROW,107 SAY 'USAGE : '
    
    @ ROW,116 SAY XTOTCLR(3) PICTURE '99999999.99'
    
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('=',133)
    XTOTCLR(1) = 0
    XTOTCLR(2) = 0
    XTOTCLR(3) = 0
  ENDIF  && end if TOTALS BY COLOR
  *-- totals by fabric
  IF cFABRIC <> XFABRIC .AND. XPRNT_ACT
    ROW = ROW + 1
    @ ROW,01 SAY XFABRIC
    @ ROW,20 SAY 'TOTAL'
    @ ROW,37 SAY 'PO-RECEIPTS :'
    @ ROW,51 SAY XTOTFAB(1) PICTURE '999999999.99'
    @ ROW,72 SAY 'ADJUSTMENTS :'

    @ ROW,86 SAY XTOTFAB(2) PICTURE '999999999.99'
    @ ROW,107 SAY 'USAGE : '

    @ ROW,115 SAY XTOTFAB(3) PICTURE '999999999.99'
    
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('=',133)
    XTOTFAB(1) = 0
    XTOTFAB(2) = 0
    XTOTFAB(3) = 0
  ENDIF  && end if TOTALS BY FABRIC
  *-- if eof(), exits print loop
  IF EOF()
    EXIT
  ENDIF  && end if eof()
  XFABRIC  = cFABRIC
  XCOLOR   = cCOLOR
  XBAL     = 0
  XNEWBAL  = 0
  XPRV_REC = 0
  XPRV_USG = 0
  *-- Calculate the starting inventory balance for this fabric/color .
  *-- If there's no starting activity date entered then start printing
  *-- the report with starting balance zero. Or if starting activity date 
  *-- is earlier than the first transaction date for this fabric/color.
  *-- if select by date 
  IF lcrpSortby='D'
    *-- entered date range is stored in lDate, hDate
    *-- if a date range is entered
    IF !EMPTY(lDate) .AND. dTranDate < lDate
      *-- scan lcTempFile for the same fabric and same color and transaction date less than the first entered date 
      SCAN WHILE cFABRIC+cCOLOR = XFABRIC+XCOLOR .AND. dTranDate < lDate
        *-- case transaction type  
        DO CASE
           CASE cTranType='1'   && po-receipt
             XBAL     = XBAL     + nReceived
             XPRV_REC = XPRV_REC + nReceived
           CASE cTranType='2'   && adjustment
             XBAL   = XBAL + (nReceived-nIssued)
           CASE cTranType='3'   && physical
             XBAL   = xbal+(nReceived-nIssued)
           CASE cTranType='4'   && issue/return fabric in cuttkt
             XBAL     = XBAL     -  IIF(nIssued <> 0 ,nIssued,-nReceived)
             XPRV_USG = XPRV_USG +  IIF(nIssued <> 0 ,nIssued,-nReceived)
        ENDCASE && end case transaction type  
      ENDSCAN  && end scan lcTempFile 
    ENDIF  && end if a date range is entered
  ELSE  && else select by sequence no (MATINVJL.CTRN_SEQ)
    *-- if user seleted range of sequence 
    IF !EMPTY(lcRplSeq) .AND. cTrn_Seq < lcRplSeq  
      *-- scan lcTempFile for the same fabric and same color and  transaction sequence no less than selected low sequence 
      SCAN WHILE (cFABRIC+cCOLOR = XFABRIC+XCOLOR) .AND. (cTrn_Seq < lcRplSeq)
        *-- case transaction type 
        DO CASE
           CASE cTranType='1'  && po-receipt
             XBAL     = XBAL     + nReceived
             XPRV_REC = XPRV_REC + nReceived
           CASE cTranType='2'  && adjustment
             XBAL   = XBAL +(nReceived-nIssued)
           CASE cTranType='3'  && physical
             XBAL    = xbal+(nReceived-nIssued)
           CASE cTranType='4'  && issue/return fabric in cuttkt
             XBAL     = XBAL     -  IIF(nIssued <> 0 ,nIssued,-nReceived)
             XPRV_USG = XPRV_USG +  IIF(nIssued <> 0 ,nIssued,-nReceived)
        ENDCASE  && end case transaction type 
      ENDSCAN  && end scan lcTempFile 
    ENDIF  && end if user seleted range of sequence 
  ENDIF  && end if select by date 
  XNEWBAL=XBAL                  && NEWBAL=BALANCE
  XPRNT_ACT=.T.                 && flag saying that there's activity to print
                                && for this fabric/color
  *-- if another fabric or another color
  IF cFABRIC+cCOLOR <> XFABRIC+XCOLOR
    XPRNT_ACT=.F.               && no more activity for this fabric/color
  ENDIF  && end if another fabric or another color
  *-- get some informations from the fabric file
  SELECT FABRIC
  SEEK XFABRIC+XCOLOR
  *-- if the fabric exists
  IF FOUND()
    XDESC     = DESC
    XPATTERN  = PATTERN
    XVENDOR   = VENDOR
    XLOC      = LOC
    XWIDTH    = WIDTH
  ELSE  && else fabric not exist
    XDESC     = ''
    XPATTERN  = ''
    XVENDOR   = ''
    XLOC      = ''
    XWIDTH    = ''
  ENDIF  && end if the fabric exists
  SELECT (lcTempFile)
  *-- print information about that fabric/color
  llBegin = .F.
  ROW=ROW+1
  @ ROW,01 SAY 'ITEM   :'
  @ ROW,10 SAY XFABRIC
  @ ROW,18 SAY 'COLOR :'
  @ ROW,26 SAY XCOLOR
  @ ROW,36 SAY 'DESC :'
  @ ROW,43 SAY XDESC
  @ ROW,70 SAY 'LOC:'
  @ ROW,76 SAY XLOC
  @ ROW,104 SAY 'PREVIOUS RECEIPTS :'
  @ ROW,124 SAY XPRV_REC PICTURE '99999999'
  ROW=ROW+1
  @ ROW,01 SAY 'VENDOR :'
  @ ROW,10 SAY XVENDOR
  @ ROW,33 SAY 'PATTERN :'
  @ ROW,43 SAY XPATTERN
  @ ROW,68 SAY 'WIDTH :'
  @ ROW,76 SAY XWIDTH
  @ ROW,104 SAY 'PREVIOUS    USAGE :'
  @ ROW,124 SAY XPRV_USG PICTURE '99999999'
  ROW = ROW+1
  @ ROW,95 SAY 'STARTING INVENTORY BALANCE :'
  
  @ ROW,124 SAY XBAL PICTURE '99999999'
  
  ROW=ROW+1
  *-- if no more activity to print 
  IF .NOT. XPRNT_ACT
    @ ROW,000 SAY REPLICATE('-',133)
    LOOP
  ENDIF  && end if no more activity to print 
  *-- print the transactions between activity 
  @ ROW,000 SAY REPLICATE('-',133)
  ROW=ROW+1
  *-- if select by date
  IF lcrpSortby='D'

    IF !llRpPrtDy
      @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 LOC.    REFERENCE              UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    ELSE
      @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 LOC.    REFERENCE   DYELOT     UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    ENDIF

  ELSE  && else select by sequence no (MATINVJL.CTRN_SEQ)

    IF !llRpPrtDy
      @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 LOC.    SEQ. NO REFERENCE              UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    ELSE
      @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 LOC.    SEQ. NO REFERENCE   DYELOT     UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    ENDIF

  ENDIF  && end if select by date 
  ROW=ROW+1
  @ ROW,00 SAY REPLICATE('-',133)
  ROW=ROW+1
  *-- scan lcTempFlie for the same fabric and the same color
  SCAN WHILE cFABRIC+cCOLOR= XFABRIC+XCOLOR
    
    *-- if selected date and the transaction date is exceeding the selected high date
    IF !EMPTY(hDate) .AND. (dTranDate > hDate)
       EXIT
    ENDIF  && end if selected date and the transaction date is exceeding the selected high date 
    XBAL     = XNEWBAL
    XTOTCOST = 0
    *-- case transaction type
    DO CASE
       CASE cTranType='1'        && po receipt      
         IF EMPTY(CiSession) && PO Received
           XNEWBAL    = XNEWBAL    + nReceived
           XTOTCLR(1) = XTOTCLR(1) + nReceived
           XTOTFAB(1) = XTOTFAB(1) + nReceived
           XTOTCOST   =  nReceived * nUnitCost
         ELSE && PO Return
           XNEWBAL    = XNEWBAL    + nIssued
           XTOTCLR(1) = XTOTCLR(1) + nIssued
           XTOTFAB(1) = XTOTFAB(1) + nIssued
           XTOTCOST   =  nIssued * nUnitCost
         ENDIF
       CASE cTranType='2'        && adjustment
         
         XNEWBAL=XNEWBAL         +nReceived-nIssued
         XTOTCLR(2)  = XTOTCLR(2)+nReceived-nIssued
         XTOTFAB(2) = XTOTFAB(2) +nReceived-nIssued
       CASE cTranType='3'        && physical
            
         XNEWBAL     =  XNEWBAL+nReceived-nIssued
         
       CASE cTranType='4'        && issue/return fabric in cuttkt
            
         XNEWBAL    = XNEWBAL       -  IIF(nIssued <> 0 ,nIssued,-nReceived)
         XTOTCLR(3) = XTOTCLR(3)    +  IIF(nIssued <> 0 ,nIssued,-nReceived)
         XTOTFAB(3) = XTOTFAB(3)    +  IIF(nIssued <> 0 ,nIssued,-nReceived)
         XTOTCOST   = IIF(nIssued <> 0 ,nIssued,-nReceived) *  nUnitCost
         
    ENDCASE  && end case transaction type
    SELECT (lcTempFile)
    *-- if page end, check for page breake
    IF ROW > 53
      PAGENO = PAGENO + 1
      DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
      llBegin = .T.
      ROW = 5
      IF lcrpSortby='D'
        IF !llRpPrtDy
          @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 LOC.    REFERENCE              UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
        ELSE
          @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 LOC.    REFERENCE   DYELOT     UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
        ENDIF
      ELSE
        IF !llRpPrtDy
          @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 LOC.    SEQ. NO REFERENCE              UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
        ELSE
          @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 LOC.    SEQ. NO REFERENCE   DYELOT     UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
        ENDIF
      ENDIF  
      ROW=ROW+1
      @ ROW,000 SAY REPLICATE('-',133)
      ROW=ROW+1
    ENDIF  && end if page end
    *-- case transaction type
    DO CASE
       CASE cTranType = '1'
         @ ROW,00 SAY dTranDate
         llMFO = (SEEK(crsession+cTran+cfabric+Ccolor+'2','mmfgordd'))
         @ ROW,09 SAY IIF(!llMFO,IIF(EMPTY(CiSession),'PO-RECEIPT','PO-RETURN'),'MF-RECEIPT')
         
         *-- get some informations from purchase order fabric line for the fabric/color  
         *-- if found record for that fabric/color in POFLN
         IF SEEK(cFabric+cColor+IIF(EMPTY(CiSession),'P','R')+cTran+'2','PofLn')
           SELECT PofLn   
           LOCATE FOR &lcTempFile..cTrn_Seq=cRsession;
                  WHILE &lcTempFile..cFabric+&lcTempFile..cColor = Fabric+Color

           @ ROW,20 SAY PofLn.POMAT
           @ ROW,27 SAY PofLn.VENDOR
           SELECT (lcTempFile)
           IF lcrpSortby = 'D'
             @ ROW,43 SAY cWareCode
           ELSE
             @ ROW,35 SAY cWareCode
           ENDIF  
           @ ROW,43 SAY IIF (lcrpSortby='S',cTrn_Seq,'')
           @ ROW,51 SAY SUBSTR(PofLn.REFERENCE,1,IIF(llRpPrtDy ,11,22))
           IF llRpPrtDy
             @ ROW,63 SAY PofLn.Dyelot
           ENDIF
           
         ELSE
           IF SEEK(crsession+cTran+cfabric+Ccolor+'2','mmfgordd')

             @ ROW,20 SAY mmfgordd.cMfgOrdNo
             IF lcrpSortby = 'D'
               @ ROW,43 SAY cWareCode
             ELSE
               @ ROW,35 SAY cWareCode
             ENDIF  

             @ ROW,51 SAY SUBSTR(mmfgordd.REFERENCE,1,IIF(llRpPrtDy ,11,22))
             IF llRpPrtDy
               @ ROW,63 SAY mmfgordd.Dyelot
             ENDIF
             
           ENDIF
         ENDIF  && end if found record for that fabric/color in POFLN
         IF !llBegin
           @ ROW,74 SAY nUnitCost PICTURE '9999.999'
           @ ROW,81 SAY XTOTCOST PICTURE '999999.999'
         ELSE
           @ ROW,76 SAY nUnitCost PICTURE '9999.999'
           @ ROW,83 SAY XTOTCOST PICTURE '999999.999'
         ENDIF  
         @ ROW,94 SAY XBAL PICTURE '99999999.999'
         @ ROW,107 SAY IIF(EMPTY(CiSession),nReceived,nIssued) PICTURE '999999999.999'
         @ ROW,121 SAY XNEWBAL PICTURE '99999999.999'
         *--B802245,1 HDM[End]
         
       CASE cTranType='2'
         @ ROW,00 SAY dTranDate
         @ ROW,09 SAY 'ADJUSTMENT'
         @ ROW,27 SAY SUBSTR(REFERENCE,1,15)
         IF lcrpSortby = 'D'
           @ ROW,43 SAY cWareCode
         ELSE
           @ ROW,35 SAY cWareCode
         ENDIF  
         @ ROW,43 SAY IIF (lcrpSortby='S',cTrn_Seq,'')
         
         IF llRpPrtDy
           @ ROW,63 SAY cDyelot
         ENDIF
         
         @ ROW,94 SAY XBAL PICTURE '99999999.999'
         @ ROW,108 SAY nReceived-nIssued PICTURE '99999999.999'
         @ ROW,121 SAY XNEWBAL PICTURE '99999999.999'
       CASE cTranType='3'
         @ ROW,0 SAY dTranDate
         @ ROW,09 SAY '*PHYSICAL*'
         @ ROW,20 SAY SUBSTR(REFERENCE,1,17)
         IF lcrpSortby = 'D'
           @ ROW,43 SAY cWareCode
         ELSE
           @ ROW,35 SAY cWareCode
         ENDIF  
         @ ROW,43 SAY IIF (lcrpSortby='S',cTrn_Seq,'')
         IF llRpPrtDy
           @ ROW,63 SAY cDyelot
         ENDIF
         
         @ ROW,94 SAY XBAL PICTURE '99999999.999'
         @ ROW,107 SAY nReceived-nIssued PICTURE '999999999.999'
         @ ROW,121 SAY XNEWBAL PICTURE '99999999.999'
       CASE cTranType='4'            && issue/return fabric for cuttkt/po 
         @ ROW,0 SAY dTranDate
         *-- the saving procedure of the material po cost sheet 
         *-- in MATINVJL is the same like cuttkt cost sheet.
         *-- one of the solutions is to distinguish between them
         *-- by adding CIMTYP Character (01) (as in MFGOPRHD).
         *-- thus , we 'll use that column to distinguish between
         *-- the two states (instead of checking EMPTY(cTktNo))
         @ ROW,09 SAY 'USAGE'
         @ ROW,20 SAY cTktNo
         *-- a maximum of three contractors are printed .
         
         lnCurAlias = SELECT(0)
         SELECT MFGOPRHD
         SET ORDER TO Tktoper
         STORE SPACE(08) TO laContr
         lnInd  = 1
         =SEEK(&lcTempFile..CIMTYP+&lcTempFile..cTktNo)
         SCAN WHILE CIMTYP+CTKTNO = &lcTempFile..CIMTYP+&lcTempFile..cTktNo FOR !lInHouse
           laContr[lnInd] = MFGOPRHD.CCONTCODE
           lnInd = lnInd+1 
           IF lnInd > 3     && to limit printing 3 contractors only
             EXIT
           ENDIF  
         ENDSCAN
         SELECT (lnCurAlias)
         @ ROW,27 SAY laContr[1]
         @ ROW,34 SAY laContr[2]
         *- Print  Location Insted of Vendor3
         IF lcrpSortby = 'D'
           @ ROW,43 SAY cWareCode
         ELSE
           @ ROW,35 SAY cWareCode
         ENDIF  
         @ ROW,43 SAY SUBSTR(IIF(lcrpSortby='D','',cTrn_Seq),1,7)
         
         IF INLIST(CIMTYP,'M','I','T')
           DO CASE
             CASE CIMTYP='M'
               lcFile='CUTTKTH'
             CASE CIMTYP='I'
              lcFile ='POSHDR'
             CASE CIMTYP='T'
              lcFile = 'MMFGORDH'
           ENDCASE
           *-- case status 
           @ ROW,51 SAY &lcFile..STATUS + "/"
         ENDIF  
         *-- if there's cuttkt, print cuttktkh.style 
         IF CIMTYP = 'M'
           @ ROW,53  SAY SUBSTR(CUTTKTH.STYLE,1,IIF(llRpPrtDy ,9,19))

           IF llRpPrtDy
             @ ROW,63 SAY cDyelot
           ENDIF
         ENDIF  && end if there's cuttkt
         IF !llBegin
           @ ROW,74 SAY nUnitCost PICTURE '9999.999'
           @ ROW,81 SAY XTOTCOST PICTURE '999999.999'
         ELSE
           @ ROW,76 SAY nUnitCost PICTURE '9999.999'
           @ ROW,83 SAY XTOTCOST PICTURE '999999.999'
         ENDIF  
         @ ROW,94 SAY XBAL PICTURE '99999999.999'
         @ ROW,107 SAY IIF(nReceived=0,-nIssued,nReceived) PICTURE '999999999.999'
         @ ROW,121 SAY XNEWBAL PICTURE '99999999.999'
    ENDCASE  && end case transaction type
    ROW=ROW+1
  ENDSCAN  && end scan lcTempFlie for the same fabric and the same color
  *-- IF FABRIC+COLOR=XFABRIC+XCOLOR THEN IT MEANS THAT WE REACHED THE ENDING
  *-- ACTIVITY DATE AND THERE ARE MORE TRANSACTIONS FOR THIS FAB/CLR. SO WE HAVE
  *-- TO SCAN UNITL FABRIC+COLOR<>XFABRIC+XCOLOR (i.e next fabric/color)
  SELECT (lcTempFile)
  IF CFABRIC+CCOLOR=XFABRIC+XCOLOR
     SCAN WHILE CFABRIC+CCOLOR=XFABRIC+XCOLOR
     ENDSCAN
  ENDIF
ENDDO  && end major print loop
DO ENDREPORT                   && end report 
SET DEVICE TO SCREEN           && return set device to screen 
RETURN                         && return to the caller which is gfopgrid (option grid program)
*-- end of report code
*--------------------- Functions' Section ---------------------
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
R_WIDTH  = 'W'       && Width of report either N',W or XW
=lfGetColor()

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
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

*!*************************************************************
*! Name      : lfvFabBet
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : Showes range screen for fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvFabBet()
*!*************************************************************
FUNCTION lfvFabBet
lcTitle = 'Item'
STORE lcRPFrmFab TO lcFromFab
STORE lcRPToFab  TO lcToFab
DO (gcRepHome + gcAct_Appl + '\FabRng.SPR')
lcRPFrmFab  = lcFromFab
lcRpToFab   = lcToFab
 
*!*************************************************************
*! Name      : lfvFabRng
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : To validate the fabric code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvFabRng()
*!*************************************************************
FUNCTION lfvFabRng
*-- this is the validation of from_item
lcRngVarNa = VARREAD()
lcRngVarVa = &lcRngVarNa.
IF !EMPTY(lcRngVarVa)
  SELECT FABRIC
  IF !SEEK(lcRngVarVa)
    DO FABROW WITH lcRngVarVa,'*'
    &lcRngVarNa = lcRngVarVa
  ENDIF
ENDIF
SHOW GET &lcRngVarNa

*!*************************************************************
*! Name      : lfvpbOk
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : Vailadet range screen's OK button 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvpbOk()
*!*************************************************************
FUNCTION lfvpbFabOk
*-- this is the validation of to_item
IF !EMPTY(lcFromFab) .OR. !EMPTY(lcToFab)
  IF EMPTY(lcToFab) 
    lcToFab = lcFromFab
  ENDIF
  IF lcFromFab > lcToFab
    WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
   _CUROBJ = OBJNUM(lcFromFab)
  ELSE
    lcRPFrmFab = lcFromFab
    lcRPToFab  = lcToFab
    CLEAR READ
  ENDIF
ELSE
  *-- CLEAR READ exits the active READ, 
  *-- and returns control to the previous READ level if the active READ 
  *-- is nested.
  CLEAR READ  
ENDIF

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : To validate the sort by option.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
CLEAR READ

*!*************************************************************
*! Name      : lfvSeq
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : Valid function of the Sequence field (MATINVJL.CTRN_SEQ).
*!*************************************************************
*! Called from : Option Grid [Sequence field option]
*!*************************************************************
*! Calls       : gfBrows().
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvSeq()
*!*************************************************************
FUNCTION lfvSeq

*******************************************************************
PRIVATE lcFrom,lcTo
STORE lcRplSeq  TO lcFrom,lcOldFrm
STORE lcRphSeq  TO lcTo,lcOldTo
STORE "@! X99999" TO lcPict
lcTitle  = 'Sequence Range'

DO ObjRng.Spx

*!*************************************************************
*! Name      : lfOldVal
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : Saving old value
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOldVal()
*!*************************************************************
FUNCTION lfOldVal
lcObjNam = VARREAD()
DO CASE
  CASE UPPER(lcObjNam) = "LCFROM"
    lcOldFrm = &lcObjNam
  CASE UPPER(lcObjNam) = "LCTO"
    lcOldTo = &lcObjNam
ENDCASE    
 
*!*************************************************************
*! Name      : lfvObj
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : Validate Account OR style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvObj()
*!*************************************************************
FUNCTION lfvObj
lcRngVarNa = VARREAD()
lcRngVarVa = &lcRngVarNa.
lnAlias = SELECT()
SELECT MATINVJL
IF !EMPTY(lcRngVarVa) .AND. ('?' $ lcRngVarVa .OR. !SEEK(lcRngVarVa))
  DIMENSION laTemp[1]
  laTemp = ''          && Array to hold the Selected value
  lcBrFields = "CTRN_SEQ :R :H= 'Seq.#' , CFABRIC :R :H= 'Fabric' , CCOLOR :R :H= 'Color' ,CWARECODE :R :H= 'Location' , CDYELOT :R :H= 'Dyelot' "
  lcFile_Ttl = "Activity Sequence"
  lcBrowCond = ""
  = gfBrows(lcBrowCond,'CTRN_SEQ','laTemp')
  IF !EMPTY(laTemp[1])
    &lcRngVarNa = ctrn_seq
  ELSE
    &lcRngVarNa = SPACE(06)
    RETURN .F.
  ENDIF
ENDIF
SHOW GET lcRngVarNa
*!*************************************************************
*! Name      : lfvpbObjOk
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : Validate range screen's ok button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbObjOk()
*!*************************************************************
FUNCTION lfvpbObjOk
*-- this is the validation of to_item
IF !EMPTY(lcFrom) .OR. !EMPTY(lcTo)
  IF EMPTY(lcTo) 
    lcTo = lcFrom
  ENDIF
  IF lcFrom > lcTo
    WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
   _CUROBJ = OBJNUM(lcFrom)
  ELSE
    lcRplSeq  = lcFrom
    lcRphSeq  = lcTo
    CLEAR READ
  ENDIF
ELSE
  CLEAR READ  
ENDIF

*!*************************************************************
*! Name      : lfvpbObjCa
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : Validate range screen's Cancel button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbObjCa()
*!*************************************************************
FUNCTION lfvpbObjCa
*-- this is the validation of to_item
STORE lcOldFrm TO lcFrom
STORE lcOldTo  TO lcTo
lcRplSeq  = lcFrom
lcRphSeq  = lcTo
CLEAR READ

*!*************************************************************
*! Name      : lfvDateRng
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo
ldFrom = LDATE
LDTO   = HDATE
lcTitle = 'Date range'
DO DateRng.Spx
*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbDateOk()
*!*************************************************************
FUNCTION lfvpbOk
IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  LDate = ldFrom
  HDate = ldTo
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfChkSysDy
*! Developer : Mohamed Shokry
*! Date      : 08/21/2002
*! Purpose   : check if MA used dyeolt ( yes / no
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChkSysDy()
*!*************************************************************
FUNCTION lfChkSysDy

RETURN (ALLTRIM(UPPER(gfGetMemVar('M_MATDYE'))) = 'Y')
