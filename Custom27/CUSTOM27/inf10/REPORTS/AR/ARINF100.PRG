*:***************************************************************************
*: Program file  : ARINF100.PRG
*: Program desc. : CUSTOMIZED SHIPPING LABEL PRINT FROM PACKING SLIP FOR INF10.
*: Date          : 08/26/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tester        : 
*: Tracking Job Number: C102648
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARINF100
*:***************************************************************************
*: Modifications :
***************************************************************************

*--The Color length
STORE 0 TO lnClrLnLb , lnClrPosLb
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnLb  = LEN(laItemSeg[lnCount,3])
    lnClrPosLb = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--The Style length
STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))

*--Section of Initializing the variables --*
lnUps  = gfGetMemVar('XUPSACCT',gcAct_Comp)
STORE SPACE(0) TO xAddr1, xAddr2, xAddr3
lcSize     = SPACE(16)
lnPcsBox   = 0
lcPrLine   = SPACE(0)

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'

*--Section to creat the InvHdr Temp File--*
=lfCreatTmp()
=lfcolctDat()

SELECT (PACKTEMP)
LOCATE

IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*-- TEMP FILE HAS BEEN SELECTED , PREPARE TO PRINT THE LABELS.
SELECT (PACKTEMP)

SET RELATION TO ORDER INTO ORDHDR
SET RELATION TO IIF(STORE = SPACE(8) , 'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER

lnMaxLine   = 10
lnNoOfLn    = 1
lcOldOrd    = SPACE(6)
lcShpVia    = ''
llUseUps    = .F.
llSkip      = .F.
llNextLabel = .F.

SELECT (PACKTEMP)
SCAN
  WAIT WINDOW 'Selecting Records For The Report ...' + Pack_No NOWAIT
  lnOfBox  = lfGetNo(&PackTemp..Pack_No , '>=' , 'No_Cart' )
  lnMinBox = lfGetNo(&PackTemp..Pack_No , '<=' , 'No_Cart' )

  SELECT (INVHTEMP)
  IF &PackTemp..Sp_Inst1 <> 'INVOICE'
    LOCATE FOR PikTkt = &PackTemp..Pack_No
    xInvoice = &INVHTEMP..Invoice
    lcPikTkt = &PackTemp..Pack_No
  ELSE
    xInvoice = &PackTemp..Pack_No
    lcPikTkt = SPACE(6)
  ENDIF
  =SEEK(xInvoice,'InvHdr')

  IF InvHdr.Consol <> 'Y'
    lcPikTkt = SPACE(6)
  ENDIF

  =gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
  lluseups = LEFT(ALLTRIM(lcUpsType),5) = "USUPS"

  xPack_No = &PackTemp..Pack_No
  xAccount = Customer.Account

  IF InvHdr.Consol = 'Y'
    SELECT CUSTOMER
    xAccount= Account
    lnSvRec = RECNO()

    SELECT CONSINVH
    =SEEK(xInvoice+&PackTemp..Store)
    xStore  = Store

    SELECT CUSTOMER
    =SEEK( 'S' + xAccount + xStore )
    xName  = STName
    XAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    XAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    XAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
    IF BETWEEN(lnSvRec,1,RECCOUNT('Customer'))
      GOTO lnSvRec IN CUSTOMER
    ENDIF
  ELSE
    xName  = Customer.STName
    XAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    XAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    XAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  ENDIF

  SELECT (INVHTEMP)
  lcCustPo = IIF(InvHdr.Consol = 'Y',ConsInvH.CustPo,InvHdr.CustPo)
  lcDeptNo = InvHdr.Dept
  xOrder   = InvHdr.Order
  lnWeight = 0
  lcStore  = &PackTemp..Store

  lnTmpRec = RECNO('Customer')
  lcDuns   = IIF(SEEK('M'+xAccount,'Customer'),SUBSTR(Customer.Duns,1,7),SPACE(7))
  IF BETWEEN(lnTmpRec,1,RECCOUNT('Customer'))
    GOTO lnTmpRec IN CUSTOMER
  ENDIF
  lcBL      = SUBSTR(&PackTemp..Note,1,6)
  Y         = ' '
  lcScale   = '#'
  lnNoOfSze = 0

  SELECT Pack_Lin
  IF &PackTemp..Sp_Inst1 <> 'INVOICE' .AND. SEEK(&PackTemp..Pack_No)
    lnRecNo = RECNO()
    COUNT REST TO lnNoInPack FOR Pack_No = &PackTemp..Pack_No .AND. ;
                             lnMinBox >= No_Cart .AND. lnMinBox <= No_Cart

    IF BETWEEN(lnRecNo,1,RECCOUNT('Pack_Lin'))
      GOTO lnRecNo IN Pack_Lin
    ENDIF
  ELSE
    LOCATE
  ENDIF

  DO WHILE lnMinBox <= lnOfBox
    SELECT (PackTemp)
    LABEL FORM (GCREPHOME+GCACT_APPL+'\ARINF100') NEXT 1 TO PRINT NOCONSOL

    SELECT Pack_Lin
    IF &PackTemp..Sp_Inst1 <> 'INVOICE' .AND. SEEK(&PackTemp..Pack_No)
      IF llNextLabel
        GO lnStrtFrm
      ENDIF
      llNextLabel = .F.

      SCAN WHILE lnMinBox <= lnOfBox FOR Pack_No = &PackTemp..Pack_No
        IF lnMinBox >= No_Cart .AND. lnMinBox <= No_Cart
          IF lnNoOfLn + lnNoOfSze < lnMaxLine
            lcStyle   = LEFT(Pack_Lin.Style,9) + SPACE(1) + SUBSTR(gfCodDes(SUBSTR(Pack_Lin.Style,lnClrPosLb,lnClrLnLb),'COLOR'),1,6)
            IF llRpnBySz
              lcSize    = ' ' + IIF(Qty1<>0,PADL(ALLTRIM(STR(Qty1,2)),2),SPACE(2)) + ' ' + IIF(Qty2<>0,PADL(ALLTRIM(STR(Qty2,2)),2),SPACE(2)) + ' ' + ;
                                IIF(Qty3<>0,PADL(ALLTRIM(STR(Qty3,2)),2),SPACE(2)) + ' ' + IIF(Qty4<>0,PADL(ALLTRIM(STR(Qty4,2)),2),SPACE(2)) + ' ' + ;
                                IIF(Qty5<>0,PADL(ALLTRIM(STR(Qty5,2)),2),SPACE(2)) + ' ' + IIF(Qty6<>0,PADL(ALLTRIM(STR(Qty6,2)),2),SPACE(2))
            ELSE
              lcSize    = SPACE(16)
            ENDIF
            lnPcsBox  = Pack_Lin.TotQty
            *--Print style scale.
            IF llRpnBySz
              =SEEK(Pack_Lin.Style,'Style')

              IF lcScale <> Style.Scale
                IF lnNoOfLn + lnNoOfSze <> lnMaxLine
                  lcScale  = Style.Scale
                  IF SEEK('S'+ Style.Scale,'SCALE')
                    lcPrLine = SPACE(17)+PADL(ALLTRIM(SCALE.Sz1),3,' ') +;
                                         PADL(ALLTRIM(SCALE.Sz2),3,' ') +;
                                         PADL(ALLTRIM(SCALE.Sz3),3,' ') +;
                                         PADL(ALLTRIM(SCALE.Sz4),3,' ') +;
                                         PADL(ALLTRIM(SCALE.Sz5),3,' ') +;
                                         PADL(ALLTRIM(SCALE.Sz6),3,' ')
                  ENDIF
                  SELECT (PackTemp)
                  LABEL FORM (GCREPHOME+GCACT_APPL+'\ARINF110') NEXT 1 TO PRINT NOCONSOL
                ENDIF
                lnNoOfSze = lnNoOfSze + 1
              ENDIF
            ENDIF
            IF lnNoOfLn + lnNoOfSze <= lnMaxLine .OR. lnNoInPack + lnNoOfSze = lnMaxLine
              lcPrLine = lcStyle + SPACE(1) + lcSize + SPACE(4) + IIF( lnPcsBox = 0, SPACE(4), STR( lnPcsBox, 4 ) )
            ELSE
              lcStyle  = SPACE(18)
              lcSize   = SPACE(20)
              lcPrLine = SPACE(40) + IIF( lnPcsBox = 0, SPACE(4), STR( lnPcsBox, 4 ) )
            ENDIF

            SELECT (PackTemp)
            LABEL FORM (GCREPHOME+GCACT_APPL+'\ARINF110') NEXT 1 TO PRINT NOCONSOL
            lnPcsBox  = 0
            lnNoOfLn  = lnNoOfLn + 1

            IF !EMPTY(lcStyle)
              IF lnNoOfLn + lnNoOfSze >= lnMaxLine
                STORE 1 TO lnNoOfLn, lnNoOfSze
                SELECT (PackTemp)
                LABEL FORM (GCREPHOME+GCACT_APPL+'\ARINF120') NEXT 1 TO PRINT NOCONSOL
                LABEL FORM (GCREPHOME+GCACT_APPL+'\ARINF100') NEXT 1 TO PRINT NOCONSOL
              ENDIF
              DO lpPrtSku
            ENDIF
          ELSE
            llNextLabel = .T.
            lnStrtFrm   = RECNO()
            EXIT
          ENDIF
        ENDIF
      ENDSCAN
    ELSE
      lnPcsBox = 0
    ENDIF

    SELECT (PackTemp)
    DO WHILE lnNoOfLn + lnNoOfSze <= lnMaxLine .AND. !llNextLabel
      lcStyle  = SPACE(12)
      lcSize   = SPACE(16)
      lcPrLine = SPACE(40) + IIF( lnPcsBox = 0, SPACE(4), STR( lnPcsBox, 4 ) )
      LABEL FORM (GCREPHOME+GCACT_APPL+'\ARINF110') NEXT 1 TO PRINT NOCONSOL
      lnNoOfLn = lnNoOfLn + 1
      lnPcsBox = 0
    ENDDO

    lcShpVia = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    IF llUseUps
      SELECT UpsBox
      LOCATE FOR Invoice = xInvoice
      SCAN REST WHILE Invoice = xInvoice
        IF Cartons = lnMinBox
          lnWeight = Weight
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
    SELECT (PackTemp)
    LABEL FORM (GCREPHOME+GCACT_APPL+'\ARINF120') NEXT 1 TO PRINT NOCONSOL
      IF !llNextLabel
        lnMinBox = lnMinBox + 1
      ENDIF
      lnNoOfLn = 1
  ENDDO
ENDSCAN

DO ENDREPORT
SET DEVICE TO SCREEN
gcDevice = 'SCREEN'
WAIT CLEAR
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/26/2002
*! Purpose   : Function to Collect the date.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat

SELECT (INVHTEMP)
LOCATE

IF !EOF()
  SCAN FOR CONSOL <> 'Y'
    IF !EMPTY(&INVHTEMP..PikTkt) .AND. SEEK(&INVHTEMP..PikTkt,'PACK_LIN')
      =SEEK(&INVHTEMP..PikTkt,'PACK_Hdr')
      SELECT PACK_Hdr
      XPACK_NO  = PACK_NO
      XACCOUNT  = ACCOUNT
      XORDER    = ORDER
      lnInvoice = &INVHTEMP..INVOICE
      XSTORE    = STORE
      XBILL     = Note
      XTOT_WGHT = TOT_WGHT
      XTOT_CART = TOT_CART
      XTOT_PCS  = TOT_PCS
      lcSp_Inst1= ''
    ELSE
      =SEEK(&INVHTEMP..Invoice,'INVHDR')
      XPACK_NO  = InvHdr.Invoice
      XACCOUNT  = InvHdr.ACCOUNT
      XORDER    = InvHdr.ORDER
      lnInvoice = &INVHTEMP..INVOICE
      XSTORE    = InvHdr.STORE
      XBILL     = InvHdr.Note2
      XTOT_WGHT = InvHdr.Weight
      XTOT_CART = InvHdr.Cartons
      XTOT_PCS  = InvHdr.Ship
      lcSp_Inst1= 'INVOICE'
    ENDIF

    SELE (PACKTEMP)
    APPEND BLANK
    REPLACE PACK_NO  WITH XPACK_NO   ,;
            ACCOUNT  WITH XACCOUNT   ,;
            STORE    WITH XSTORE     ,;
            ORDER    WITH XORDER     ,;
            Note     WITH XBILL      ,;
            TOT_WGHT WITH XTOT_WGHT  ,;
            TOT_CART WITH XTOT_CART  ,;
            TOT_PCS  WITH XTOT_PCS   ,;
            RECFLAG  WITH 'Y'        ,;
            Sp_Inst1 WITH lcSp_Inst1 ,;
            INVOICE  WITH lnInvoice

    SELECT (INVHTEMP)
  ENDSCAN

  SELECT (INVHTEMP)
  LOCATE
  SCAN FOR CONSOL = 'Y'
    SELECT CONSINVH
    =SEEK(&INVHTEMP..Invoice)
    SCAN WHILE INVOICE + STORE + ORDER = &INVHTEMP..Invoice

      IF SEEK(CONSINVH.PikTkt,'PACK_LIN')
        =SEEK(CONSINVH.PikTkt,'PACK_Hdr')
        SELECT PACK_HDR
        XPACK_NO  = PACK_NO
        XACCOUNT  = ACCOUNT
        XORDER    = ORDER
        lnInvoice = &INVHTEMP..INVOICE
        XSTORE    = STORE
        XBILL     = Note
        XTOT_WGHT = TOT_WGHT
        XTOT_CART = TOT_CART
        XTOT_PCS  = TOT_PCS
        lcSp_Inst1= ''
      ELSE
        =SEEK(&INVHTEMP..Invoice,'INVHDR')
        XPACK_NO   = InvHdr.Invoice
        XACCOUNT   = InvHdr.ACCOUNT
        XORDER     = InvHdr.ORDER
        lnInvoice  = &INVHTEMP..INVOICE
        XSTORE     = STORE
        XBILL      = InvHdr.Note2
        XTOT_WGHT  = InvHdr.Weight
        XTOT_CART  = InvHdr.Cartons
        XTOT_PCS   = InvHdr.Ship
        lcSp_Inst1 = 'INVOICE'
      ENDIF

      SELECT (PACKTEMP)
      APPEND BLANK
      REPLACE PACK_NO  WITH XPACK_NO   ,;
              ACCOUNT  WITH XACCOUNT   ,;
              STORE    WITH XSTORE     ,;
              ORDER    WITH XORDER     ,;
              Note     WITH XBILL      ,;
              TOT_WGHT WITH XTOT_WGHT  ,;
              TOT_CART WITH XTOT_CART  ,;
              TOT_PCS  WITH XTOT_PCS   ,;
              RECFLAG  WITH 'Y'        ,;
              Sp_Inst1 WITH lcSp_Inst1 ,;
              INVOICE  WITH lnInvoice

      SKIP IN (INVHTEMP)
    ENDSCAN
  ENDSCAN
ENDIF

SELECT (PACKTEMP)

*--End of lfcolctDat.
*!*************************************************************
*! Name      : lfGetNo
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/26/2002
*! Purpose   : Print the style/color Skus for a specific account.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetNo()
*!*************************************************************
FUNCTION lfGetNo
PARAMETERS lcPackNo, lcSign, lcFiled

PRIVATE lnAlias, lnRecNo, lnMax
lnAlias = SELECT()

SELECT Pack_Lin
lnMax = 0

IF SEEK (lcPackNo)
  lnRecNo = RECNO()
  lnMax   = No_Cart
ELSE
  IF SEEK(lcPackNo,'InvHdr') .AND. lcSign = '>='
    IF InvHdr.Consol = 'Y'
      =SEEK(lcPackNo+&PACKTEMP..Store,'ConsInvH')
      lnMax = ConsInvH.Cartons
    ELSE
      lnMax = InvHdr.Cartons
    ENDIF
  ENDIF
  lnRecNo = 1
ENDIF

SCAN WHILE lcPackNo = Pack_No
  lnMax = IIF ( No_Cart &lcSign lnMax , &lcFiled, lnMax )
ENDSCAN

IF BETWEEN(lnRecNo,1,RECCOUNT('Pack_Lin'))
  GOTO lnRecNo IN Pack_Lin
ENDIF
SELECT (lnAlias)
RETURN IIF(lnMax = 0 , 1 , lnMax)

*--EOF OF lfGetNo.
*!*************************************************************
*! Name      : lpPrtSku
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/26/2002
*! Purpose   : Print the style/color Skus for a specific account.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrtSku
*!*************************************************************
PROCEDURE lpPrtSku

IF !SEEK('S'+&PACKTEMP..Account+lcStyle,'Spck_Lin')
  RETURN
ENDIF

SELECT Spck_Lin
lcPackId = SUBSTR(PACK_ID,1,8)

IF EMPTY(Sku)
  lnI = 1
  =SEEK('S'+Style.Scale,'Scale')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF

  DIME laSku[8]
  laSku = SPACE(8)
  SCAN WHILE Type+Account+Style = 'S'+&PACKTEMP..Account+Style.Style .AND. lnI < 9;
       FOR SUBSTR(Spck_Lin.PACK_ID,1,8) = lcPackId

    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0 .AND. EMPTY(laSku(lnX))
        laSku(lnX)=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN

  lnI = 1
  LOCATE
  = SEEK('S'+&PACKTEMP..Account+Style.Style,'Spck_Lin')

  SCAN WHILE Type+Account+Style = 'S'+&PACKTEMP..Account+Style.Style .AND. lnI < 9;
           .AND. lnNoOfLn + lnNoOfSze <= lnMaxLine
    lcI = ALLTRIM(STR(lnI))

    IF MOD(lnI,2) <> 0
      lcPrLine = 'SKU N#' + SUBSTR(Pack_Id,1,lnDime1) + ' '+Scale.Sz&lcI+':'+laSku(lnI) + ' '
    ELSE
      lcPrLine = lcPrLine + Scale.Sz&lcI+':'+laSku(lnI) + ' '
    ENDIF

    IF EMPTY(laSku(lnI))
      SKIP -1
    ENDIF

    IF MOD(lnI,2) = 0 .OR. lnI = Scale.cnt
      SELECT (PackTemp)
      LABEL FORM (GCREPHOME+GCACT_APPL+'\ARINF110') NEXT 1 TO PRINT NOCONSOL
      lnNoOfLn = lnNoOfLn + 1
      lnPcsBox = 0
    ENDIF
    lnI = lnI + 1
  ENDSCAN
ELSE
  lcPrLine = 'CUSTOMER SKU # '+ Sku
  SELECT (PackTemp)
  LABEL FORM (GCREPHOME+GCACT_APPL+'\ARINF110') NEXT 1 TO PRINT NOCONSOL
  lnNoOfLn = lnNoOfLn + 1
  lnPcsBox = 0
ENDIF

RETURN

*--End of lpPrtSku.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/26/2002
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

PRIVATE llTempMast
llTempMast = .F.
CREATE CURSOR (InvhTemp) (INVOICE C(6) , ACCOUNT C(5) , PikTkt C(6) ,;
                          FLAG C(1)    , CONSOL C(1))
ZAP

*-- Select invoices for printing.
lcFltInv = lAOGFXFlt[ASUBSCRIPT(lAOGFXFlt, ASCAN(lAOGFXFlt,'INVHDR.INVOICE'),1),6]

IF USED(lcFltInv) AND RECCOUNT(lcFltInv) > 0
  SELECT (lcFltInv)
  SET RELATION TO INVOICE INTO INVHDR ADDITIVE
  llTempMast = .T.
ELSE
  SELECT INVHDR
  LOCATE
  llTempMast = .F.
ENDIF

SCAN
  INSERT INTO (InvhTemp) (INVOICE,ACCOUNT,PikTkt,FLAG,CONSOL) VALUES ;
    (IIF(llTempMast , EVAL(lcFltInv+'.INVOICE') , INVHDR.INVOICE),INVHDR.Account,INVHDR.PIKTKT,INVHDR.FLAG,INVHDR.CONSOL)
ENDSCAN
SET RELATION OFF INTO INVHDR

*--Section Add 'Invoice' Field to the Packtemp file.
SELECT PACK_HDR
LOCATE

DIMENSION laArray[1]
=AFIELDS(laArray)
DIMENSION laArray[ALEN(laArray,1)+1,4]
laArray[ALEN(laArray,1),1] = 'INVOICE'
laArray[ALEN(laArray,1),2] = 'C'
laArray[ALEN(laArray,1),3] = 6
laArray[ALEN(laArray,1),4] = 0

CREATE CURSOR (PACKTEMP) FROM ARRAY laArray
INDEX ON INVOICE + STORE + PACK_NO TAG (PACKTEMP) OF (gcWorkDir + PACKTEMP)

*--End of lfCreatTmp.