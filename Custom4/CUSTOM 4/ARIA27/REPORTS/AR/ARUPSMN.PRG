*:************************************************************************
*: Program file  : ARUPSMN.PRG 
*: Program desc. : UPS MANIFEST
*:         System: ARIA APPAREL SYSTEM 2.7
*: Module        : ACCOUNT RECEIVABLE (AR)
*:      Developer: IHB
*  Date          : 21/02/1999
*:************************************************************************
*: Calls         : 
*:    Procedures : NONE
*:    Functions  : lfPrnSumry, lfPrtData, lfShipType, lfwRunGrid,
*:                 lfvDateRng, lfvpbDateOk, lfvInvNo, lfwOldVal, 
*:                 lfInvSet, lfvInv, lfEvalVars, lfAdrShift
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO ARUPSMN
*:************************************************************************
*: This Report Program is due to E301145,1
*:************************************************************************
*-- Option Grid Layout :
*-- Range of Invoice Dates       : from .. to ..
*-- Restart at Invoice #         : ..
*-- Sort by                      : <I>nvoice  <Z>one default is I
*-- Invoice Range                : range
*-- Approve Manifest             : <Y>es <N>o default is N

*-- XWEIGHT .. lnGrnWght : all are calculated values
*-- LDATE , HDATE        : selected low and high date.
*-- XSORTBY              : sort by criteria (either invoice or zone) 
*-- XCHOICE              : approve manifest (Y/N)
*-- laCompAdd            : company addresses
STORE 0 TO XWEIGHT, XFREIGHT, XINSUR, XCOD, XTOTCOD, XTOTDEC, XRECCNT
STORE 0 TO XSUBFREIGHT, XSUBINSUR, XSUBCOD, XSUBTTCOD, XSUBTTDEC
STORE 0 TO XGRNFREIGHT, XGRNINSUR, XGRNCOD, XGRNTTCOD, XGRNTTDEC
STORE 0 TO XCARTONS, XCOD_AMT, XDECL_VALUE,lnSubWght,lnGrnWght

XLDATE  = LDATE
XHDATE  = HDATE
XSORTBY = lcRpSortBy
XCHOICE = lcRpAprv      && Y/N
DECLARE laCompAdd[6,1] , laShipTo[5,1]
STORE SPACE(0) TO laCompAdd

*-- retrieving company addresses
IF !USED('SYCCOMP')
  = gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'Ccomp_id','SH')
ENDIF
SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName   = cCom_Name             && Variable to hold the Company Name
lcCompPhon   = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict   = gfPhoneTem()          && Variable to hold the Company Phone Format
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6] = TRANSFORM(lcCompPhon , lcPhonPict)
=lfAdrShift('laCompAdd')
*-- XADDR1,2,3 : are company addresses to be printed
XADDR1       = SUBSTR(laCompAdd[1],1,26)                && ADDR1
XADDR2       = SUBSTR(laCompAdd[2],1,26)                && XADDR2
XADDR3       = SUBSTR(laCompAdd[3],1,26)                && city+state+zip
*-- retrieving customer shipto addresses

*-- COD_FLAG & STATUS being used to save off INSURYN & CODYN values
*-- TOTALCHG being used to save off DECL_VALUE from UPSBOX.DBF
*-- reinitializing relation between invhdr and (syszones,codes,warehous,customer)
SELECT INVHDR
SET RELATION TO
SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
SET RELATION TO 'N'+PADR('SHIPVIA',10)+INVHDR.SHIPVIA+SPACE(30)+'CUPS' INTO CODES ADDITIVE
*-- if company is multiwarehouse
IF !llMultiWH
  SET RELATION TO  SUBSTR(CODES.CRLTD_VLU,1,7)+XUPSFROM+ ;
                SUBSTR(CUSTOMER.CADDRESS5,1,3) INTO SYSZONES ADDITIVE
ELSE  && else single warehouse
  SET RELATION TO INVHDR.cWareCode INTO WAREHOUS ADDITIVE
  SET RELATION TO  SUBSTR(CODES.CRLTD_VLU,1,7)+WareHous.Ups+ ;
                SUBSTR(CUSTOMER.CADDRESS5,1,3) INTO SYSZONES ADDITIVE
ENDIF  && endif company is multiwarehouse

*-- invwork is the file holding the records to be printed
*-- constructing invwork using &laogfxflt[1,6] which carries
*-- all the most recent selected invoices
SELECT (INVWORK)
*-- zap invwork if it is filled (another time before)
IF RECCOUNT() <> 0
  ZAP
ENDIF  
*-- scans (laOGFxFlt[1,6]) and appends all invoices' entries from upsbox
*-- lnInvPos    : invoice position
STORE 0  TO lnInvPos
lnInvPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVOICE'),1)
IF USED(laOGFxFlt[lnInvPos,6]) AND RECCOUNT(laOGFxFlt[lnInvPos,6]) <> 0
  SELECT (laOGFxFlt[lnInvPos,6])
  SCAN
    SELECT INVHDR
    SEEK &laOGFxFlt[1,6]..INVOICE  && to position on the correct syszones record
    XINVOICE = INVHDR.INVOICE
    SELECT UPSBOX       && it is already checked that the invoice has ups details created
    SEEK(XINVOICE)
    SCAN WHILE INVOICE=XINVOICE
      STORE 0 TO XINSUR, XCOD, XCOD_AMT, XDECL_VALUE
      XACCOUNT = ACCOUNT
      XSTORE   = STORE
      XWEIGHT  = WEIGHT
      XFREIGHT = FREIGHT
      XINSURYN = INSURYN
      XCODYN   = CODYN
      IF XINSURYN = 'Y'
        XINSUR = INSUR
        XDECL_VALUE = DECL_VALUE
      ENDIF
      IF XCODYN = 'Y'
        XCOD=COD
        XCOD_AMT=TOTALCOD
      ENDIF
      INSERT INTO (INVWORK) ;
      (INVOICE, ACCOUNT, STORE, WEIGHT, FREIGHT, INSUR, COD, UPSZONE, FLAG, COD_FLAG, STATUS, COD_AMT, TOTALCHG ) ;
      VALUES (XINVOICE, XACCOUNT, XSTORE, XWEIGHT, XFREIGHT, XINSUR, XCOD, SYSZONES.ZONE, 'Y', XCODYN, XINSURYN, XCOD_AMT, XDECL_VALUE )
    ENDSCAN
  ENDSCAN
ENDIF
SELECT (INVWORK)
GO TOP
*-- if no records are selected
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
*-- setting  relation between invwork and customer
SET RELATION TO
SET RELATION TO IIF( EMPTY(STORE) , 'M'+ACCOUNT , 'S'+ACCOUNT+STORE ) INTO CUSTOMER
*-- begin print routine
R_TITLE = 'United Parcel Shipping Manifest'
XTITLE  = 'FROM:'+DTOC(XLDATE)+' THRU:'+DTOC(XHDATE)
*-- lcUpsAct : ups account
STORE SPACE(0) TO  lcUpsAct
lcUpsAct = gfGetMemVar('XUPSACCT')
DO WHILE .T.
  ROW    = 99
  PAGENO = 0
  XSUBFLAG=.F.
  SET DEVICE TO PRINT
  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2...
  *S.J.B. international inc.                                                                             UPS Shipper # : XXXX
  *26 west 37th St.                                                                                      Pickup Rec. # : XXXX
  *New York NY 10018

  *Date Shipped : 04/22/91
  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2...
  *==========================================================================================================================
  *Invoice #  Consignee Name            Zone      Weight  Res    Package chgs           COD   O/S      Declared  GT  CT
  *           Address                                                                Amount               Value     
  *           City     State  Zip                                                                                         
  *201321     Macy's Northeast            2       999999           9999999.99    9999999.99    *     9999999.99
  *           189 west 35th st.                                                                                                                                                                
  *           New york       NY 10018                                                                                                                        
  *--------------------------------------------------------------------------------------------------------------------------
  *                                             ========          ===========   ===========         ===========
  *              SUB TOTAL ------>              99999999          99999999.99   99999999.99         99999999.99
  *--------------------------------------------------------------------------------------------------------------------------
  *                                            =========         ============  ============        ============
  *            GRAND TOTAL ------>             999999999         999999999.99  999999999.99        999999999.99
  *--------------------------------------------------------------------------------------------------------------------------
  *1) It will be pinted in numerical sequence by invoice #.
  *2) Subtotals will be done after every 100 entries. If report is run by zone,
  *   then subtotal after every zone, or after every 100 entries within the zone.

  SELECT (INVWORK)
  *-- if sortby criteria is Invoice or Zone
  IF XSORTBY='I'
    SET ORDER TO TAG (INVWORK)
  ELSE
    SET ORDER TO TAG (ZONEWORK)
  ENDIF
  SCAN WHILE INKEY() <> 32
    IF ROW>55 .AND. (!EOF())
      PAGENO = PAGENO + 1
      @ 05,01 SAY QCOMPANY
      @ 05,103 SAY 'UPS SHIPPER # : '+ lcUpsAct
      *-- print company addresses
      @ 06,01 SAY XADDR1
      @ 06,103 SAY 'PICKUP REC. # : '
      @ 07,01 SAY IIF( EMPTY(XADDR2) , XADDR3, XADDR2 )
      @ 08,01 SAY IIF( EMPTY(XADDR2) ,'',XADDR3)
      @ 10,01 SAY 'DATE SHIPPED :'
      @ 10,17 SAY DATE()
      @ 11,00 SAY REPLICATE('-',130)
      @ 12,01 SAY 'Invoice #  Consignee Name            Zone      Weight  Res    Package chgs           COD   O/S      Declared  GT  CT'
      @ 13,01 SAY '           Address                                                                Amount               Value'
      @ 14,01 SAY '           City   STATE  ZIP'
      @ 15,00 SAY REPLICATE('-',130)
      ROW = 16
    ENDIF
    XPREVZONE = UPSZone
    ROW=ROW+1
    @ ROW,01 SAY INVOICE
    @ ROW,12 SAY SUBSTR( CUSTOMER->STNAME,1, 20 )
    @ ROW,39 SAY UPSZone
    @ ROW,48 SAY WEIGHT    PICTURE '999999'
    @ ROW,65 SAY FREIGHT + INSUR + COD       PICTURE '9999999.99'
    *-- COD_FLAG <=> CODYN
    XTOTCOD=IIF(COD_FLAG='Y',(COD_AMT),0)
    @ ROW,79 SAY XTOTCOD PICTURE '9999999.99'
    *-- STATUS <=> INSURYN
    *-- TOTALCHG <=> DECL_VALUE
    XTOTDEC=IIF(STATUS='Y',(TOTALCHG),0)
    @ ROW,099 SAY XTOTDEC PICTURE '9999999.99'
    *-- calculate subtotals
    XSUBFREIGHT = XSUBFREIGHT + FREIGHT
    XSUBINSUR   = XSUBINSUR   + INSUR
    XSUBCOD     = XSUBCOD     + COD
    XSUBTTCOD   = XSUBTTCOD   + XTOTCOD
    XSUBTTDEC   = XSUBTTDEC   + XTOTDEC
    lnSubWght   = lnSubWght   + WEIGHT
    *-- calculate grand totals
    XGRNFREIGHT = XGRNFREIGHT + FREIGHT
    XGRNINSUR   = XGRNINSUR   + INSUR
    XGRNCOD     = XGRNCOD     + COD
    XGRNTTCOD   = XGRNTTCOD   + XTOTCOD
    XGRNTTDEC   = XGRNTTDEC   + XTOTDEC
    lnGrnWght   = lnGrnWght   + WEIGHT
    ROW = ROW + 1
    *-- customer shipto addresses
    =lfShipTo()   &&To get the shipto address from the customer file.
    @ ROW,12 SAY laShipTo[1]
    ROW = ROW + 1
    @ ROW,12 SAY IIF( EMPTY(laShipTo[2]) ,;
    TRIM( laShipTo[3])+' '+TRIM(laShipTo[4])+' '+;
    TRIM( laShipTo[5] ) , laShipTo[2] )
    IF !EMPTY( laShipTo[2] )
      ROW = ROW + 1
      @ ROW,12 SAY  TRIM( laShipTo[3] )+' '+TRIM(laShipTo[4])+' '+;
      TRIM( laShipTo[5] )
    ENDIF
    *-- XRECCNT : record count
    XRECCNT=XRECCNT+1
    *-- printing subtotals depending on selected sortby criteria
    DO CASE
      CASE XSORTBY='I' .AND. XRECCNT>99
        ROW=ROW+2
        @ ROW,46 SAY '========          ===========   ===========         ==========='
        ROW=ROW+1
        @ ROW,15 SAY 'SUB TOTAL ------>'
        @ ROW,46 SAY lnSubWght    PICTURE '99999999'
        @ ROW,64 SAY XSUBFREIGHT + XSUBINSUR + XSUBCOD   PICTURE '99999999.99'
        @ ROW,78 SAY XSUBTTCOD    PICTURE '99999999.99'
        @ ROW,98 SAY XSUBTTDEC    PICTURE '99999999.99'
        STORE 0 TO XSUBFREIGHT,XSUBINSUR,XSUBCOD,XSUBTTCOD,XSUBTTDEC,XRECCNT,lnSubWght
        XSUBFLAG=.T.
        ROW=ROW+1
      CASE XSORTBY='Z'
        SKIP
          IF UPSZone <> XPREVZONE .OR. XRECCNT>99
            ROW=ROW+2
            @ ROW,46 SAY '========          ===========   ===========         ==========='
            ROW=ROW+1
            @ ROW,15 SAY 'SUB TOTAL ------>'
            @ ROW,46 SAY lnSubWght    PICTURE '99999999'
            @ ROW,64 SAY XSUBFREIGHT + XSUBINSUR + XSUBCOD   PICTURE '99999999.99'
            @ ROW,78 SAY XSUBTTCOD    PICTURE '99999999.99'
            @ ROW,98 SAY XSUBTTDEC    PICTURE '99999999.99'
            STORE 0 TO XSUBFREIGHT,XSUBINSUR,XSUBCOD,XSUBTTCOD,XSUBTTDEC,XRECCNT,lnSubWght
            XSUBFLAG=.T.
            ROW=ROW+1
          ENDIF
        SKIP -1
    ENDCASE
    ROW = ROW + 1
  ENDSCAN
  *-- print subtotals if not already printed (only pertains to sortby invoice,
  *-- for sortby zone, previouse subtotal routine should have taken care of it.
  IF !XSUBFLAG  .OR. (XRECCNT<=99 .AND. XSORTBY='I')
    ROW=ROW+2
    @ ROW,46 SAY '========          ===========   ===========         ==========='
    ROW=ROW+1
    @ ROW,15 SAY 'SUB TOTAL ------>'
    @ ROW,46 SAY lnSubWght    PICTURE '99999999'
    @ ROW,64 SAY XSUBFREIGHT + XSUBINSUR + XSUBCOD   PICTURE '99999999.99'
    @ ROW,78 SAY XSUBTTCOD    PICTURE '99999999.99'
    @ ROW,98 SAY XSUBTTDEC    PICTURE '99999999.99'
    XSUBFLAG=.F.
  ENDIF
  *-- print grand totals
  ROW=ROW+1
  @ ROW,45 SAY '=========         ============  ============        ============'
  ROW=ROW+1
  @ ROW,13 SAY 'GRAND TOTAL ------>'
  @ ROW,45 SAY lnGrnWght    PICTURE '999999999'
  @ ROW,63 SAY XGRNFREIGHT + XGRNINSUR + XGRNCOD   PICTURE '999999999.99'
  @ ROW,77 SAY XGRNTTCOD    PICTURE '999999999.99'
  @ ROW,97 SAY XGRNTTDEC    PICTURE '999999999.99'
  *-- calling lfprnsumry to print summary part.
  =lfPrnSumry()
  *-- resetting subtotals and grandtotals
  STORE 0 TO XSUBFREIGHT, XSUBINSUR, XSUBCOD, XSUBTTCOD, XSUBTTDEC,lnSubWght
  STORE 0 TO XGRNFREIGHT, XGRNINSUR, XGRNCOD, XGRNTTCOD, XGRNTTDEC,lnGrnWght
  *-- if approve manifest = 'Y' , then update SHIPFLAG and  SHIPDATE
  *-- in INVHDR file
  *-- Approve manifest (Y/N)
  IF XCHOICE='Y'
    SELECT (laOGFxFlt[1,6])
    GO TOP
    *-- replace SHIPFLAG and SHIPDATE in INVHDR file
    SCAN
      XINVOICE=INVOICE
      WAIT WINDOW 'INVOICE #: '+TRIM(XINVOICE) NOWAIT
      SELECT INVHDR
      SEEK XINVOICE
      XR=RLOCK()
      REPLACE SHIPFLAG WITH .T.
      REPLACE SHIPDATE WITH DATE()
      UNLOCK
    ENDSCAN
    EXIT
  ENDIF
  IF XCHOICE='N'   && meaning don't approve manifest
    EXIT
  ENDIF
ENDDO
DO ENDREPORT                   && end report 
SET DEVICE TO SCREEN           && return set device to screen 
*-- end of report code...
*--------------------- Functions' And Procedures' Section -----
*--------------------------------------------------------------

*:************************************************************************
*: Program file  : lfPrnSumry
*: Program desc. : Function to print summary part. 
*:         System: Aria Apparel System
*:      Developer: IHB
*:           Date: 21/02/1999
*:************************************************************************
FUNCTION lfPrnSumry
*-- initialize some of the memory variables:
STORE 0 TO lnPiceComm,lnWegtComm,lnChrgComm, lnPiceAir,lnWegtAir,lnChrgAir ,;
           lnNoOfCod, lnCodChrg,lnPicCmTot,lnWegCmTot,lnChrCmTot,;
           lnPicAiTot,lnWegAiTot,lnChrAiTot,lnNoOfGrn,lnGrnTrChg
STORE SPACE(3) TO lcZone
STORE SPACE(7) TO lcShip
@ 03,000 SAY '                                               U.P.S SHIPPING MANIFEST SUMMARY'
@ 04,000 SAY '                                               ==============================='
@ 05,001 SAY QCOMPANY
@ 05,103 SAY 'UPS SHIPPER # : '+ lcUpsAct
@ 06,001 SAY XADDR1
@ 06,103 SAY 'PICKUP REC. # : '
@ 07,001 SAY IIF( EMPTY(XADDR2) , XADDR3 , XADDR2 )
@ 08,01 SAY IIF( EMPTY(XADDR2) ,'',XADDR3)
@ 10,01 SAY 'DATE SHIPPED :'
@ 10,17 SAY DATE()
@ 11,00 SAY '                                          COMMON CARRIER                                AIR'
@ 12,00 SAY 'TYPE SERVICE                     ZONE  PIECES   WEIGHT     CHARGES       ZONE    PIECS   WEIGHT     CHARGES'
@ 13,00 SAY '-----------------------------------------------------------------------------------------------------------'
IF XSORTBY = 'Z'
  SELECT (InvWork)
  INDEX ON INVOICE+STR(RECNO(),7) TAG (INVWORK)
ENDIF
*-- constructing invhzones using &laogfxflt[1,6] , upsbox and syszones
InvHZones = gfTempName()
InvHZoInd = gfTempName()
CREATE TABLE (gcWorkDir+InvHZones) (Invoice C(6) , Zone C(3), ShipType C(7))
INDEX ON INVOICE TAG (InvHZones)
=gfOpenFile(gcWorkDir+InvHZones,' ','EX') &&global function opens DBF (with-optionally- its index) share or exculsive.
INDEX ON ShipType+Zone+Invoice  TAG (InvHZoInd)
INDEX ON INVOICE TAG (InvHZones)
SELECT (laOGFxFlt[1,6])
*-- scans all selected invoices in (laogfxflt[1,6])
SCAN
  SELECT INVHDR
  SEEK &laOGFxFlt[1,6]..INVOICE
  IF !EOF('SYSZONES')
    INSERT INTO (InvHZones) (INVOICE, ZONE, SHIPTYPE) ;
           VALUES (&laOGFxFlt[1,6]..INVOICE, SYSZONES.ZONE, SYSZONES.SHIPTYPE)
  ENDIF
ENDSCAN
SELECT (InvHZones)
SET ORDER TO (InvHZoInd)
GO TOP
DELETE REST WHILE EMPTY(ShipType)
ROW = 13
llOut=.F.
GO TOP
*-- loop on (invhzones)
DO WHILE !EOF(InvHZones)
  IF SEEK(Invoice,INVWORK)
    IF lcShip <> ShipType 
      IF ALLTRIM(ShipType) <> 'USUPST'
        lcType = lfShipType(ShipType)
        ROW = ROW + 1
        @ ROW,00 SAY lcType
      ENDIF  
    ELSE
      ROW = ROW + 1
    ENDIF  
    lcShip = ShipType
    Do While lcShip = ShipType        
      =SEEK(Invoice,'InvHdr')
      lcZone = Zone
      Scan Rest While lcShip = ShipType
        IF lcZone <> Zone
          lcPreZone=lcZone
          lcZone = Zone
          IF ALLTRIM(ShipType) <> 'USUPST'
            lcZone=lcPreZone
            =lfPrtData()
            lcZone = Zone
          ENDIF  
          STORE 0 TO lnPiceComm,lnWegtComm,lnChrgComm, lnPiceAir,lnWegtAir,lnChrgAir,lnCodChrg
        ENDIF  
        IF SEEK(Invoice,'InvHdr')
          lnNoOfCod = lnNoOfCod+IIF(InvHdr.Cod_Flag='Y',1,0)
        ENDIF
        *-- calculate the charges from UpsBox.
        IF SEEK(Invoice,'UpsBox')
          lcInv=Invoice
          SELECT UpsBox
          SCAN REST WHILE Invoice=lcInv
            lnCodChrg=lnCodChrg+Freight+Insur+Cod
          ENDSCAN
          SELECT (InvHZones)
        ENDIF   
        DO CASE
          CASE ALLTRIM(ShipType) = 'USUPSG' .OR. ALLTRIM(ShipType) = 'USUPST' ;
                                            .OR. ALLTRIM(ShipType) = 'USUPS3' ;
                                            .OR. ALLTRIM(ShipType) = 'USUPS'
            lcCommCar   =  .T.
            lnPiceComm  = lnPiceComm + IIF(SEEK(&InvHZones->Invoice,'InvHdr'),InvHdr.Ship,0)
            lnWegtComm  = lnWegtComm + InvHdr.Weight
            lnChrgComm  =  IIF(ALLTRIM(ShipType) <> 'USUPST',lnCodChrg,0)
            lnNoOfGrn   = lnNoOfGrn  + IIF(ALLTRIM(ShipType) = 'USUPSG',InvHdr.Cartons,0)
            lnGrnTrChg  = IIF(ALLTRIM(ShipType) = 'USUPSG',lnCodChrg,0)
          CASE ALLTRIM(ShipType) = 'USUPS2' .OR. ALLTRIM(ShipType)= 'USUPSN' ;
                                            .OR. ALLTRIM(ShipType)='USUPSE'
            lcCommCar   =  .F.
            lnPiceAIR   = lnPiceAIR + IIF(SEEK(&InvHZones->Invoice,'InvHdr'),InvHdr.Ship,0)
            lnWegtAIR   = lnWegtAIR + InvHdr.Weight
            lnChrgAIR   =  lnCodChrg
        ENDCASE
        lcPreShip=ALLTRIM(ShipType)
      EndScan  
    EndDo
    IF lcPreShip <> 'USUPST'
      =lfPrtData()
      STORE 0 TO lnCodChrg
    ENDIF  
  ELSE
    SKIP
  ENDIF 
  STORE 0 TO lnPiceComm,lnWegtComm,lnChrgComm, lnPiceAir,lnWegtAir,lnChrgAir ,lnCodChrg 
ENDDO

ROW=ROW+2
*-- line1
@ ROW,00 SAY REPLICATE('-',130)
ROW=ROW+1
@ ROW,002 SAY 'Total'
@ ROW,037 SAY lnPicCmTot PICTURE '99999999'
@ ROW,046 SAY lnWegCmTot PICTURE '99999999'
@ ROW,055 SAY lnChrCmTot PICTURE '99999999.99'
@ ROW,078 SAY lnPicAiTot PICTURE '99999999'
@ ROW,087 SAY lnWegAiTot PICTURE '99999999'
@ ROW,096 SAY lnChrAiTot PICTURE '99999999.99'
ROW=ROW+1
@ ROW,00 SAY REPLICATE('-',130)
ROW=ROW+1
*-- line2
@ ROW,02 SAY 'NUMBER OF GROUNDTRAC PACKAGES :'
@ ROW,33 SAY ALLTRIM(STR(lnNoOfGrn))
@ ROW,61 SAY ' GROUNDTRAC CHARGES:'
@ ROW,83 SAY lnGrnTrChg  PICTURE '999999999.99'
ROW=ROW+1
*-- line3
@ ROW,02 SAY  "NUMBER OF COD'S               :"
@ ROW,33 SAY ALLTRIM(STR(lnNoOfCod))
@ ROW,61 SAY  ' COD CHARGES       :'
@ ROW,83 SAY XGRNTTCOD    PICTURE '999999999.99'
ROW=ROW+1
*-- line4
@ ROW,62 SAY  'TOTAL UPS REVENUE :'
@ ROW,83 SAY  lnChrCmTot+lnChrAiTot+lnGrnTrChg  PICTURE '999999999.99'
ROW=ROW+2
*-- line5
@ ROW,02 SAY  'RECEIVED BY:                    PICKUP DATE:            PICKUP TIME:           PKGS:       '
RETURN

*:************************************************************************
*: Program file  : lfPrtData
*: Program desc. : Function to print data lines. 
*:         System: Aria Apparel System
*:      Developer: IHB
*:           Date: 21/02/1999
*:************************************************************************
FUNCTION lfPrtData
IF lcCommCar
  *-- commen carrier
  @ ROW,33 SAY lcZone
  @ ROW,38 SAY lnPiceComm  PICTURE '9999999'
  @ ROW,47 SAY lnWegtComm  PICTURE '9999999'
  @ ROW,56 SAY lnChrgComm  PICTURE '9999999.99'
ELSE
  *-- air
  @ ROW,74 SAY lcZone     
  @ ROW,79 SAY lnPiceAir  PICTURE '@Z 9999999'
  @ ROW,88 SAY lnWegtAir  PICTURE '@Z 9999999'
  @ ROW,97 SAY lnChrgAir  PICTURE '@Z 9999999.99'
ENDIF
*-- acumulate regesters
lnPicCmTot=lnPicCmTot+lnPiceComm
lnWegCmTot=lnWegCmTot+lnWegtComm
lnChrCmTot=lnChrCmTot+lnChrgComm
lnPicAiTot=lnPicAiTot+lnPiceAir
lnWegAiTot=lnWegAiTot+lnWegtAir
lnChrAiTot=lnChrAiTot+lnChrgAir
ROW=ROW+1

*:************************************************************************
*: Program file  : lfShipType
*: Program desc. : Function to get the header of shiptype. 
*:         System: Aria Apparel System
*:      Developer: IHB
*:           Date: 21/02/1999
*:************************************************************************
FUNCTION lfShipType
PARAMETERS lcShipType
PRIVATE lnMaxLen, lcLookFor, lcLookIn, lnTypeNo, lcTypeDes, lcRetValue
IF lcShipType = "OTHER  "
  lcRetValue = "OTHERS"
ELSE
  lnMaxLen   = 31
  llIsCanada = (SUBSTR(lcShipType,1,2) = "CA")
  lcLookFor  = IIF(llIsCanada,PADR(lcShipType,7),SUBSTR(ALLTRIM(lcShipType),LEN(ALLTRIM(lcShipType)),1))
  lcLookIn   = IIF(llIsCanada,"CAXPR  CACOM  CAEXPR CAEXPD CASTAN CAECON ", "GT32NE")
  lnTypeNo   = ATC(lcLookFor, lcLookIn)
  lnTypeNo   = IIF(llIsCanada,6+IIF(lnTypeNo=1,1,(((lnTypeNo-1)/6)+1)),lnTypeNo)
  lcTypeDes  = PADR("USA UPS GROUND SERVICE"         , lnMaxLen) +;
               PADR("USA UPS GROUNDTRACK"            , lnMaxLen) +;
               PADR("USA UPS 3 DAY SELECT"           , lnMaxLen) +;
               PADR("USA UPS 2 DAY AIR"              , lnMaxLen) +;
               PADR("USA UPS NEXT DAY AIR"           , lnMaxLen) +;
               PADR("USA UPS NEXT DAY AIR EARLY A.M.", lnMaxLen) +;
               PADR("CANADA XPRESSPOST SERVICE"      , lnMaxLen) +;
               PADR("CANADA COMMERCIAL PARCEL"       , lnMaxLen) +;
               PADR("EXPRESS SERVICE WITHIN CANADA"  , lnMaxLen) +;
               PADR("EXPEDITED SERVICE WITHIN CANADA", lnMaxLen) +;
               PADR("STANDARD SERVICE WITHIN CANADA" , lnMaxLen) +;
               PADR("ECONOMY SERVICE WITHIN CANADA"  , lnMaxLen)
  lcRetValue = ALLTRIM(SUBSTR(lcTypeDes,(lnMaxLen*(lnTypeNo-1))+1,lnMaxLen))
ENDIF  
RETURN (lcRetValue)

*!*************************************************************
*! Name      : lfwRunGrid
*! Developer : IHB
*! Date      : 21/02/1999
*! Purpose   : valid function when run grid.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwRunGrid()
*!*************************************************************
FUNCTION lfwRunGrid
R_WIDTH = 'W'
=gfOpenFile(gcSysHome+'SYSZONES',gcSysHome+'FRTZONES','SH')
*-- Fill date field in OG. [begin]
LDATE  = gdSysDate
HDATE  = gdSysDate
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER
SET ORDER TO TAG CCODE_NO IN CODES
SET ORDER TO TAG Upsbox IN UPSBOX
SELECT INVHDR
SET RELATION TO
SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
SET RELATION TO 'N'+PADR('SHIPVIA',10)+INVHDR.SHIPVIA+SPACE(30)+'CUPS' INTO CODES ADDITIVE
*-- if company uses multiwarehouses
IF llMultiWH
  =gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
ENDIF  
SELECT INVHDR
IF !llMultiWH
  SET RELATION TO  SUBSTR(CODES.CRLTD_VLU,1,7)+XUPSFROM+ ;
                SUBSTR(CUSTOMER.CADDRESS5,1,3) INTO SYSZONES ADDITIVE
ELSE
  SET RELATION TO INVHDR.cWareCode INTO WAREHOUS ADDITIVE
  SET RELATION TO  SUBSTR(CODES.CRLTD_VLU,1,7)+WareHous.Ups+ ;
                SUBSTR(CUSTOMER.CADDRESS5,1,3) INTO SYSZONES ADDITIVE
ENDIF
INVWORK  = gfTempName()    &&global function gives temporarily name for the workfile.
ZONEWORK = gfTempName()
CREATE TABLE (gcWorkDir+INVWORK) (INVOICE C(6),ACCOUNT C(5),STORE C(8),SHIP N(7),WEIGHT N(6),FREIGHT N(10,2),INSUR N(10,2),;
        COD N(10,2),FLAG C(1),LABELS N(4),UPSZONE C(3),COD_FLAG C(1),STATUS C(1),COD_AMT N(10,2),TOTALCHG N(10,2))

=gfOpenFile(gcWorkDir+INVWORK,' ','EX') &&global function opens DBF (with-optionally- its index) share or exculsive.
*-- COD_FLAG & STATUS being used to save off INSURYN & CODYN values
*-- TOTALCHG Being used to save off DECL_VALUE from UPSBOX.DBF
*-- LABELA field is used to hold O/S and  AOD
INDEX ON INVOICE+STR(RECNO(),7) TAG (INVWORK)
INDEX ON UPSZONE+INVOICE+STR(RECNO(),7) TAG (ZONEWORK)
SET ORDER TO TAG (INVWORK)

*!*************************************************************
*! Name      : lfvDateRng
*! Developer : IHB
*! Date      : 21/02/1999
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
DO DateRng.SPR   && Run the advance payment screen 

*!*************************************************************
*! Name      : lfvpbok
*! Developer : IHB
*! Date      : 21/02/1999
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
*! Example     : = lfvpbok()
*!*************************************************************
FUNCTION lfvpbok
IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  LDate = ldFrom
  HDate = ldTo
  llClearRep = .T.
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfvInvNo
*! Developer : IHB
*! Date      : 21/02/1999
*! Purpose   : Validation function for the Invoice number field
*!*************************************************************
*! Called from : Invoice number field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvInvNo
PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag
lcObjName  = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal   = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER

*-- if the user want to browse or if the account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))
  lcBrFields = "Invoice :R :H= 'Invoice' , " +;
               "Printed = IIF(PrtFlag = 'P' , 'Yes' , 'No') :R :H= 'Printed' , " +;
               "InvDate :R :H= 'Date' , " +;
               "Account :R :H= 'Account' , " +;
               "Order   :R :H= 'Order' , " +;
               "CustPO  :R :H= 'Reference' , " +;
               "CUSTOMER.BTName :R :H= 'Bill to' , " +;
               "Rep1    :R :H= 'Sales Rep.' , " +;
               "Ship    :R :H= 'Pieces' , " +;
               "ShipAmt :R :H= 'Merchandise'"
  lcFile_Ttl = 'Receivable invoices' 
  SELECT INVHDR
  *-- the following relation already established
  *SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  
  DECLARE laRetVal[1]
  IF gfBrows('' , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE    && else no value is returned
    &lcObjName = laOldVal
  ENDIF    && end of if
  SET RELATION OFF INTO CUSTOMER
ENDIF    && end of if
*-- if the INVHDR file did not have an active index
IF EMPTY(lcInvHdTag)
  SET ORDER TO IN INVHDR
ELSE    && Else
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
ENDIF    && End of IF
*-- if the CUSTOMER file did not have an active index
IF EMPTY(lcCstmrTag)
  SET ORDER TO IN CUSTOMER
ELSE    && Else
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF    && End of IF
*-- llClearRep : is used to refresh the range selection according to
*-- slected invoice date range and restart at invoice number and the remaining
*-- (fixed filter) on INVHDR
llClearRep = .T.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : IHB
*! Date      : 21/02/1999
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
                                  && this is a global solution for any object type.
*!*************************************************************
*! Name      : lfInvSet
*! Developer : IHB
*! Date      : 21/02/1999
*! Purpose   : Set function for the invoice number option in case
*!             of In Range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfInvSet
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && set range
    lcInvHdTag = ORDER('INVHDR')
    lcCstmrTag = ORDER('CUSTOMER')
    SET ORDER TO TAG INVHDR IN INVHDR
    SET ORDER TO TAG CUSTOMER IN CUSTOMER
    SELECT INVHDR
    *--
    SET RELATION TO
    SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
    SET RELATION TO 'N'+PADR('SHIPVIA',10)+INVHDR.SHIPVIA+SPACE(30)+'CUPS' INTO CODES ADDITIVE
    *-- preparing to display zone of the invoice if selected range of invoices
    *-- only one time collecting this temp is better than collecting it per condition
    *-- Opening necessary files
    IF !llMultiWH
      SET RELATION TO  SUBSTR(CODES.CRLTD_VLU,1,7)+XUPSFROM+ ;
          SUBSTR(CUSTOMER.CADDRESS5,1,3) INTO SYSZONES ADDITIVE
    ELSE
      SET RELATION TO INVHDR.cWareCode INTO WAREHOUS ADDITIVE
      SET RELATION TO  SUBSTR(CODES.CRLTD_VLU,1,7)+WareHous.Ups+ ;
          SUBSTR(CUSTOMER.CADDRESS5,1,3) INTO SYSZONES ADDITIVE
    ENDIF
    
  CASE lcParm = 'R'  && reset range
    *-- If The INVHDR file did not have an active index
    IF EMPTY(lcInvHdTag)
      SET ORDER TO IN INVHDR
    ELSE    && Else
      SET ORDER TO TAG (lcInvHdTag) IN INVHDR
    ENDIF    && End of IF
    *-- If The CUSTOMER file did not have an active index
    IF EMPTY(lcCstmrTag)
      SET ORDER TO IN CUSTOMER
    ELSE    && Else
      SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
    ENDIF    && End of IF
    llClearRep = .F.
  CASE lcParm = 'V'  && valid per record in range
    XINVOICE = IIF(INVHDR.INVOICE >=lcRpInvoice .AND. BETWEEN(INVHDR.INVDATE,LDATE,HDATE) .AND. !INVHDR.SHIPFLAG .AND. INVHDR.STATUS <> 'V' .AND. SUBSTR(CODES.CRLTD_VLU,1,1)<>'C' , INVHDR.INVOICE , "")
    SELECT UPSBOX
    IF !SEEK(XINVOICE)
      WAIT WINDOW 'UPS Box Details for Invoice # ' +INVHDR.INVOICE+' Have Not Been Created. Can Not Select.'
      RETURN .F.  && prevent selection
    ENDIF
ENDCASE

*!*************************************************************
*! Name      : lfvInv
*! Developer : IHB
*! Date      : 21/02/1999
*! Purpose   : Validation function for the Invoice number field
*!*************************************************************
*! Called from : Invoice number field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvInv
PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')
lcInvZTag = ORDER(' &InvHZones')
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER
SET ORDER TO TAG (InvHZones) IN (InvHZones)
*-- If The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))
  lcBrFields = "Invoice :R :H= 'Invoice' , " +;
               "Printed = IIF(PrtFlag = 'P' , 'Yes' , 'No') :R :H= 'Printed' , " +;
               "InvDate :R :H= 'Date' , " +;
               "Account :R :H= 'Account' , " +;
               "Order   :R :H= 'Order' , " +;
               "CustPO  :R :H= 'Reference' , " +;
               "CUSTOMER.BTName :R :H= 'Bill to' , " +;
               "Rep1    :R :H= 'Sales Rep.' , " +;
               "Ship    :R :H= 'Pieces' , " +;
               "ShipAmt :R :H= 'Merchandise'"
  lcFile_Ttl = 'Receivable invoices' 
  SELECT INVHDR
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]
  IF gfBrows('' , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE    && Else
    &lcObjName = laOldVal
  ENDIF    && End of IF
  SET RELATION OFF INTO CUSTOMER
ENDIF    && End of IF
*-- If The INVHDR file did not have an active index
IF EMPTY(lcInvHdTag)
  SET ORDER TO IN INVHDR
ELSE    && Else
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
ENDIF    && End of IF
*-- If The CUSTOMER file did not have an active index
IF EMPTY(lcCstmrTag)
  SET ORDER TO IN CUSTOMER
ELSE    && Else
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfEvalVars
*! Developer : IHB
*! Date      : 21/02/1999
*! Purpose   : Assign/Fill Default values used in Option Grid .
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfEvalVars()
*!*************************************************************
FUNCTION lfEvalVars

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : IHB
*! Date      : 21/02/1999
*! Purpose   : get the total charge back for all the report
*!*************************************************************
*! Example     : =lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam
FOR lnCount = 1 TO 5
  *-- If The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- For Loop to loop the Address Array
FOR lnCount = 1 TO 5
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfShipTo
*! Developer : BASSEM RAFAAT 
*! Date      : 09/24/2000
*! Purpose   : To get the shipto address from the customer file.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfShipTo()
*!*************************************************************
FUNCTION lfShipTo
STORE SPACE(0) TO laShipTo
laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
=lfAdrShift('laShipTo')
