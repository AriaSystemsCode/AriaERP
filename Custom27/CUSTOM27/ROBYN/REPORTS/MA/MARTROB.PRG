*:***************************************************************************
*: Program File        : MARTROB.PRG
*: Program desc.       : Return PO Detail Report for ROB.
*: Date                : 01/29/2001
*: System              : ARIA ADVANTAGE SERIES
*: Module              : MATERIALS (MA)
*: Developer           : RANIA ABDEL RAZIK (RAE)			
*: Tracking Job Number : C102526
*:***************************************************************************
*: Calls               : 						
*:    Functions        : 
*:    Procedures       : 
*:***************************************************************************
*: Passed Parameters   : None
*:***************************************************************************
*: Example             : DO MARTROB
*:***************************************************************************

XREPORT = 'Return PO Detail Report for ROB'
R_WIDTH = 'W'                               && STANDARD REPORT IS 'WIDE'
R_TITLE = 'RETURN MATERIAL PO DETAIL REPORT'

lcDyelot = IIF(gfGetMemVar("M_DYELOT",gcAct_Comp)='Y', .T. , .F.)  && (M_DYELOT='Y')

*--  variable that is used to get the contents of the pofln.trancd
*--           and get its value to variable xrep
lnDatePos = ASCAN(laOGFxFlt,"POFLN.TRANCD")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  XREP  = laOGFxFlt[lnDatePos,6]
ELSE
  XREP = SPACE(0)
ENDIF

lcFilter = lcRpExp
lcFilter = STRTRAN(lcFilter,'APVENDOR.CVENDCODE','POFHDR.VENDOR')

* Case of ALL divisions
IF lcRpDiv <> 'A'
  lcFilter = lcFilter + " AND POFHDR.CDIVISION = '&lcRpDiv'"
ENDIF

* Select sort sequence
SORTFB    = ''
SORTFIELD = ''
BREAK     = ''
BREAKD    = ''

DO CASE
  * SUMMARY BY ORDER#
  CASE lcRpSort = "P"
    SORTFIELD = 'POMAT+TRANCD+Fabric+Color+STR(RECNO(),7)'
    BREAK     = 'POMAT'
    BREAKD    = 'POMAT'

  * SUMMARY BY VENDOR ACCOUNT
  CASE lcRpSort = "V"
    SORTFIELD = 'POFHDR.VENDOR+POMAT+TRANCD+Fabric+Color+STR(RECNO(),7)'
    BREAK     = 'POFHDR.VENDOR'
    BREAKD    = 'VENDOR'

  * SUMMARY BY ITEM
  CASE lcRpSort = "I"
    SORTFIELD = 'Fabric+TRANCD+Color+POMAT+STR(RECNO(),7)'
    BREAK     = 'Fabric'
    BREAKD    = 'Fabric'

  * SUMMARY BY COLOR
  CASE lcRpSort = "C"
    SORTFIELD = 'FABRIC+Color+TRANCD+POMAT+STR(RECNO(),7)'
    BREAK     = 'FABRIC+Color'
    BREAKD    = 'COLOR'    
ENDCASE

SELECT POFLN
SET RELATION TO Cmattype+POMAT INTO POFHDR
SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE

LOCATE ALL FOR cMatType = 'R' .AND. &lcFilter 
IF EOF() 
   *-- 'No Record Selected for the report..!'
   =gfModalGen('TRM00052B00000','DIALOG')
   SET DEVICE TO SCREEN
   RETURN
ENDIF
IF !USED(lcWorkFile)
  COPY REST TO (gcWorkDir+lcWorkFile) FOR cMatType = 'R' .AND. &lcFilter
ENDIF
SELECT POFLN
SET RELATION TO
=gfOpenFile(gcWorkDir+lcWorkFile,' ','EX')
SET RELATION TO Cmattype+POMAT INTO POFHDR
SET RELATION TO Fabric+Color INTO FABRIC ADDITIVE

IF SORTFIELD<>' '
  Z = LTRIM(STR(RECCOUNT(),7))
  WAIT WINDOW 'Sorting &Z records for return material purchase order detail report ...' NOWAIT
  INDEX ON &SORTFIELD TAG (lcWorkFile)
  SET ORDER TO TAG (lcWorkFile)
ENDIF

  DIMENSION XTOTAL(5,3),XTOTAL1(5,3)
  lcClrDesc = ''
  XTOTAL  = 0.00
  XTOTAL1 = 0.00
  PAGENO  = 0
  ROW     = 99
  XTIME   = TIME()
  SELECT (lcWorkFile)
  GOTO TOP

  IF LEN(TRIM(BREAK)) <>0
    HBREAK = &BREAK
  ENDIF
  SET DEVICE TO SCREEN
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  SET DEVICE TO PRINT
  SELECT (lcWorkFile)
  PTRANCD=SPACE(1)
  *---------------------------------------------------------
  * [REPORT] LOOP
  *---------------------------------------------------------
  DO WHILE INKEY() <>32
    IF ROW >=53
      PAGENO = PAGENO+1
      DO RPT_HDR WITH XREPORT,lcRpTitle,R_WIDTH
      @ 05,00 SAY '                COMPLETE'
      IF !lcDyelot
        @ 06,00 SAY 'R.PO#  VENDOR   RECEIVED S ITEM    COLOR  DESC.     REFERENCE  PATTERN   WIDTH       TOTQTY BUY    TOTQTY USE    $PRICE       AMOUNT'        
      ELSE
        @ 06,00 SAY 'R.PO#  VENDOR   RECEIVED S ITEM    COLOR  DESC.     DYELOT #   PATTERN   WIDTH       TOTQTY BUY    TOTQTY USE    $PRICE      AMOUNT'
      ENDIF
      @ 07,00 SAY REPLICATE('=',132)
      ROW = 08
    ENDIF
    DO WHILE LEN(TRIM(BREAK)) <>0
      IF &BREAK = HBREAK
        EXIT
      ENDIF
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW = ROW+1
      @ ROW,000 SAY '* SUB TOTAL * '
      @ ROW,015 SAY HBREAK

      IF BREAKD = 'COLOR'
        @ ROW,35 SAY lcClrDesc
      ENDIF
      ROW = ROW + 1

      IF '1' $ XREP .OR. EMPTY(XREP)
        @ ROW,000 SAY '  REQUESTED..............................'
        @ ROW,080 SAY XTOTAL(1,1)  PICTURE '99999999.999'
        @ ROW,096 SAY XTOTAL(1,2)  PICTURE '99999999.99'
        @ ROW,119 SAY XTOTAL(1,3)  PICTURE '9999999999.99'
        ROW = ROW + 1
      ENDIF

      IF '2' $ XREP .OR. EMPTY(XREP)
        @ ROW,000 SAY '  RETURNED...............................'
        @ ROW,080 SAY XTOTAL(2,1)  PICTURE '99999999.999'
        @ ROW,096 SAY XTOTAL(2,2)  PICTURE '99999999.99'
        @ ROW,119 SAY XTOTAL(2,3)  PICTURE '9999999999.99'
        ROW = ROW + 1
      ENDIF

      IF '3' $ XREP .OR. EMPTY(XREP)
        @ ROW,000 SAY '  OTHERS.................................'
        @ ROW,080 SAY XTOTAL(3,1)  PICTURE '99999999.999'
        @ ROW,096 SAY XTOTAL(3,2)  PICTURE '99999999.99'
        @ ROW,119 SAY XTOTAL(3,3)  PICTURE '9999999999.99'
        ROW = ROW + 1
      ENDIF

      IF '4' $ XREP .OR. EMPTY(XREP)
        @ ROW,000 SAY '  CANCELD................................'
        @ ROW,080 SAY XTOTAL(4,1)  PICTURE '99999999.999'
        @ ROW,096 SAY XTOTAL(4,2)  PICTURE '99999999.99'
        @ ROW,119 SAY XTOTAL(4,3)  PICTURE '9999999999.99'
        ROW = ROW + 1
      ENDIF

      IF '1|2' $ XREP .OR. EMPTY(XREP)
        @ ROW,000 SAY '  OPEN...................................'
        * Calculating the subtotals of the
        XTOTAL(5,1) = MAX (0, XTOTAL(1,1) - XTOTAL(2,1) - XTOTAL(3,1) - XTOTAL(4,1) )
        XTOTAL(5,2) = MAX (0, XTOTAL(1,2) - XTOTAL(2,2) - XTOTAL(3,2) - XTOTAL(4,2) )
        XTOTAL(5,3) = MAX (0, XTOTAL(1,3) - XTOTAL(2,3) - XTOTAL(3,3) - XTOTAL(4,3) )
        
        @ ROW,080 SAY XTOTAL(5,1)  PICTURE '99999999.999'
        @ ROW,096 SAY XTOTAL(5,2)  PICTURE '99999999.99'
        @ ROW,119 SAY XTOTAL(5,3)  PICTURE '9999999999.99'
        ROW = ROW +1
      ENDIF
      @ ROW,00 SAY REPLICATE('=',132)
      ROW = ROW +1
      X = 1
      DO WHILE X<=3
        XTOTAL1(1,X) = XTOTAL1(1,X) + XTOTAL(1,X)
        XTOTAL1(2,X) = XTOTAL1(2,X) + XTOTAL(2,X)
        XTOTAL1(3,X) = XTOTAL1(3,X) + XTOTAL(3,X)
        XTOTAL1(4,X) = XTOTAL1(4,X) + XTOTAL(4,X)
        XTOTAL1(5,X) = XTOTAL1(5,X) + XTOTAL(5,X)
        XTOTAL(1,X) = 0.00
        XTOTAL(2,X) = 0.00
        XTOTAL(3,X) = 0.00
        XTOTAL(4,X) = 0
        XTOTAL(5,X) = 0
        X = X+1
      ENDDO
      HBREAK = &BREAK
      PTRANCD = SPACE(1)
      EXIT
    ENDDO
    *--------------------- END SUBTOTALS ----------------------------
    IF EOF()
      EXIT
    ENDIF
    IF ROW >= 53
      ROW = 99
      LOOP
    ENDIF
    
    SELECT (lcWorkFile)
    ROW = ROW + 1
    IF (TRANCD ='1') .AND. (TRANCD<>PTRANCD)
      PTRANCD = TRANCD
      @ ROW,000 SAY '--------------------------------------------------------- Requested Lines ----------------------------------------------------------'
      ROW = ROW + 1
    ENDIF
    IF (TRANCD ='2') .AND. (TRANCD<>PTRANCD)
      PTRANCD=TRANCD
      @ ROW,000 SAY '--------------------------------------------------------- Returned Lines -----------------------------------------------------------'
      ROW =ROW + 1
    ENDIF
    IF (TRANCD ='3') .AND. (TRANCD<>PTRANCD)
      PTRANCD = TRANCD
      @ ROW,000 SAY '--------------------------------------------------------- Others Lines -------------------------------------------------------------'
      ROW = ROW + 1
    ENDIF
    IF (TRANCD ='4') .AND. (TRANCD<>PTRANCD)
      PTRANCD=TRANCD
      @ ROW,000 SAY '--------------------------------------------------------- Canceled Lines -----------------------------------------------------------'
      ROW = ROW + 1
    ENDIF

    @ ROW,000 SAY POMAT
    @ ROW,007 SAY POFHDR.VENDOR
    IF (TRANCD='2') .OR. (TRANCD='3') .OR. (TRANCD='4')
      @ ROW,016 SAY DATE
    ENDIF
    IF TRANCD='1'
      @ ROW,016 SAY POFHDR.COMPLETE
    ENDIF
    @ ROW,025 SAY POFHDR.STATUS
    @ ROW,027 SAY Fabric
    @ ROW,035 SAY Color
    @ ROW,042 SAY SUBSTR(Fabric.DESC,1,09)
    IF lcDyelot             && IF DYELOT SYSTEM
      @ ROW,052 SAY DYELOT
    ELSE
      @ ROW,052 SAY SUBSTR(REFERENCE,1,10)
    ENDIF
    @ ROW,063 SAY SUBSTR(PATTERN,1,9)
    @ ROW,073 SAY WIDTH
    @ ROW,080 SAY NFabTotQty PICTURE '9999999.999'
    @ ROW,092 SAY Fabric.UOMBUY
    @ ROW,095 SAY (NFabTotQty*Fabric.CONV)  PICTURE '9999999.99'
    @ ROW,106 SAY Fabric.UOMUSE
    @ ROW,108 SAY ncost1 PICTURE '9999999.99' 
    @ ROW,119 SAY (NFabTotQty*ncost1) PICTURE '9999999999.99'
    lcClrDesc = gfCodDes(&lcWorkFile..Color,PADR('COLOR',10))
    DO CASE
      CASE TRANCD= '1'
        XTRNCD = 1
      CASE TRANCD= '2'
        XTRNCD = 2
      CASE TRANCD= '3'
        XTRNCD = 3
      CASE TRANCD= '4'
        XTRNCD = 4  
    ENDCASE
    
    XTOTAL(XTRNCD,1) = XTOTAL(XTRNCD,1)+NFabTotQty
    XTOTAL(XTRNCD,2) = XTOTAL(XTRNCD,2)+(NFabTotQty*Fabric.CONV)
    XTOTAL(XTRNCD,3) = XTOTAL(XTRNCD,3)+(NFabTotQty*ncost1)
    SELECT (lcWorkFile)
    SKIP
  ENDDO 
  *------------------ END MAIN REPORT LOOP --------------------

  X = 2
  IF LEN(TRIM(BREAK)) = 0
    X = 1
  ENDIF
  ROW=ROW+2
  @ ROW,00 SAY REPLICATE('*',132)
  ROW = ROW+1
  @ ROW,000 SAY '* GRAND TOTAL *'
  ROW=ROW+1
  
  IF '1' $ XREP .OR. EMPTY(XREP)
    @ ROW,000 SAY '  REQUESTED..............................'
    @ ROW,079 SAY XTOTAL1(1,1)  PICTURE '999999999.999'
    @ ROW,094 SAY XTOTAL1(1,2)  PICTURE '999999999.99'
    @ ROW,117 SAY XTOTAL1(1,3)  PICTURE '999999999999.99'
    ROW = ROW +1
  ENDIF
  
  IF '2' $ XREP .OR. EMPTY(XREP)
    @ ROW,000 SAY '  RETURNED..............................'
    @ ROW,079 SAY XTOTAL1(2,1)  PICTURE '999999999.999'
    @ ROW,094 SAY XTOTAL1(2,2)  PICTURE '999999999.99'
    @ ROW,117 SAY XTOTAL1(2,3)  PICTURE '999999999999.99'
    ROW = ROW +1
  ENDIF

  IF '3' $ XREP .OR. EMPTY(XREP)
    @ ROW,000 SAY '  OTHERS................................'
    @ ROW,079 SAY XTOTAL1(3,1)  PICTURE '999999999.999'
    @ ROW,094 SAY XTOTAL1(3,2)  PICTURE '999999999.99'
    @ ROW,117 SAY XTOTAL1(3,3)  PICTURE '999999999999.99'
    ROW = ROW +1
  ENDIF

  IF '4' $ XREP .OR. EMPTY(XREP)
    @ ROW,000 SAY '  CANCELED.............................'
    @ ROW,079 SAY XTOTAL1(4,1)  PICTURE '999999999.999'
    @ ROW,094 SAY XTOTAL1(4,2)  PICTURE '999999999.99'
    @ ROW,117 SAY XTOTAL1(4,3)  PICTURE '999999999999.99'
    ROW = ROW +1
  ENDIF

  IF '1|2' $ XREP .OR. EMPTY(XREP)
    * Calculating the Grandtotals of the open qty and amount.
    *XTOTAL1(5,1) = XTOTAL1(1,1) - XTOTAL1(2,1)
    *XTOTAL1(5,2) = XTOTAL1(1,2) - XTOTAL1(2,2)
    *XTOTAL1(5,3) = XTOTAL1(1,3) - XTOTAL1(2,3)
    
    @ ROW,000 SAY '  OPEN...................................'
    @ ROW,079 SAY XTOTAL1(5,1)  PICTURE '999999999.999'
    @ ROW,094 SAY XTOTAL1(5,2)  PICTURE '999999999.99'
    @ ROW,117 SAY XTOTAL1(5,3)  PICTURE '999999999999.99'
    ROW = ROW+1
  ENDIF
  @ ROW,00 SAY REPLICATE('*',132)

  DO ENDREPORT     && End of Report or Display on Screen
  SET DEVICE TO SCREEN
  IF USED(lcWorkFile)
    USE IN (lcWorkFile)
  ENDIF
  IF FILE(gcWorkDir+lcWorkFile+'.DBF')
    ERASE (gcWorkDir+lcWorkFile+'.DBF')
  ENDIF
  IF FILE(gcWorkDir+lcWorkFile+'.CDX')  
    ERASE (gcWorkDir+lcWorkFile+'.CDX')
  ENDIF
  RETURN
****************************************************************************  
FUNCTION lfvVend

lcVenFld = VARREAD()
lcVendor = EVAL(lcVenFld)

SELECT APVENDOR
SET ORDER TO TAG VenCode 

IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF

&lcVenFld = lcVendor 

****************************************************************************  
FUNCTION lfsrvPo 

PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO Vencode IN APVENDOR
    SELECT POFHDR
    SET ORDER TO TAG POFHDR
    SET RELATION TO VENDOR INTO APVENDOR
    GO TOP IN POFHDR

  CASE lcParm = 'R'  && Reset code
    SELECT POFHDR
    SET RELATION TO 
ENDCASE
****************************************************************************  
FUNCTION lfSrvFab

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
    *llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
****************************************************************************
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
****************************************************************************
