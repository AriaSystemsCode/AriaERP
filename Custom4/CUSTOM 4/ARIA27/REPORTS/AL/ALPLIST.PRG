*:************************************************************************
*: Program file  : ALPLIST.PRG
*: Program desc. : PRINT PACKING LIST LABELS
*: System        : ARIA27
*: Module        : ALLOCATION (AL)
*: Developer     : Sameh Saiid
*: Date          : 02/10/99
*: Reference     : *E301124,1
*:************************************************************************
*: Calls         : 
*:         Procedures : ....
*:         Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Modification       :
*:************************************************************************

STORE SPACE(0) TO xAddr1, xAddr2, xAddr3
lcSize     = SPACE(16)
lnPcsBox   = 0
lcPrLine   = SPACE(0)
DIMENSION laArray[1]

lnups = gfGetMemVar('XUPSACCT')
PACKTEMP = gfTempName()

SELECT PACK_HDR
=afields(laArray)
DIMENSION laArray[ALEN(laArray,1)+1,4]
laArray[ALEN(laArray,1),1] = 'INVOICE'
laArray[ALEN(laArray,1),2] = 'C'
laArray[ALEN(laArray,1),3] = 6
laArray[ALEN(laArray,1),4] = 0
CREATE CURSOR (PACKTEMP) FROM ARRAY laArray
INDEX ON INVOICE+STORE+PACK_NO TO &gcWorkDir.&PACKTEMP 

DO SIL310

SELECT (PACKTEMP)
GOTO TOP

IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

SET RELATION TO ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
                'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER

R_WIDTH   = 'N'
lnMaxLine = 10
lnNoOfLn  = 1
lcOldOrd  = SPACE( 6 )
lcShpVia  = ''
llUseUps  = .F.    &&TAK 06/18/95 Default is not useing UPS.

WAIT WINDOW 'LABELS PRINTING - <Space Bar> TO ABORT' NOWAIT

llNextLabel = .F.    && To Say that i will not print more than 1
                     && label for the carton.

lcUps = ''
DIMENSION laUPS[1,2]
laUPS[1,1] = "CUPS"
laUPS[1,2] = "lcUPS"

SELECT (PACKTEMP)
SCAN
  lnOfBox  = lfGetNo( &PackTemp..Pack_No , '>=' , 'To_Crt' )
  lnMinBox = lfGetNo( &PackTemp..Pack_No , '<=' , 'From_Crt' )

  IF &PackTemp..Sp_Inst1 <> 'INVOICE'
    xInvoice  = &PackTemp..Invoice
    lcPikTkt  = &PackTemp..Pack_No
  ELSE
    xInvoice = &PackTemp..Pack_No
    lcPikTkt = SPACE(6) 
  ENDIF
  =SEEK(xInvoice,'InvHdr')

  IF InvHdr.Consol <> 'Y'
    lcPikTkt = SPACE(6)
  ENDIF
  
  =gfRltFld(InvHdr.ShipVia, @laUPS, "SHIPVIA")
  llUseUps = SUBSTR(lcUPS,1,5) = 'USUPS'
  
  xPack_No  = &PackTemp..Pack_No
  xAccount= Customer.Account

  IF InvHdr.Consol = 'Y'
    SELECT ConsInvH
    =SEEK(xInvoice+&PackTemp..Store)
    xStore  = Store
    SELECT Customer
    =SEEK( 'S'+xAccount+xStore )
    xName     = STName
    XAddr1    = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    XAddr2    = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    XAddr3    = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  ELSE
    xName     = Customer.STName
    XAddr1    = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    XAddr2    = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    XAddr3    = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  ENDIF

  SELECT INvHdr
  lcCustPo  = IIF(InvHdr.Consol = 'Y',ConsInvH.CustPo,InvHdr.CustPo)
  lcDeptNo  = InvHdr.Dept
  ldDate    = OrdHdr.Complete
  xOrder    = InvHdr.Order
  lnWeight  = 0
  lcStore   = &PackTemp..Store
  lcDuns    = IIF(SEEK('M'+xAccount,'Customer'),SUBSTR(Customer.Duns,1,7),;
                  SPACE(7))         && Duns Filed form customer file.
  SELECT (PACKTEMP)
  lcBL      = SUBSTR(&PackTemp..Note,1,6)
  Y         = ' '    &&Used in scale  TAK
  lcScale   = '#'
  lnNoOfSze = 0
  SELECT Pack_Lin
  IF &PackTemp..Sp_Inst1 <> 'INVOICE' .AND. SEEK(&PackTemp..Pack_No)  && MFM 06/15/95.
    COUNT REST TO lnNoInPack FOR Pack_No = &PackTemp..Pack_No .AND. ;
                             lnMinBox >= From_Crt .AND. lnMinBox <= To_Crt
  ENDIF

  DO WHILE lnMinBox =< lnOfBox
    SELECT &PackTemp
    LABEL FORM (GCREPHOME+GCACT_APPL+'\ALPLIST1') NEXT 1 TO PRINT NOCONSOL
    SELECT Pack_Lin
    IF &PackTemp..Sp_Inst1 <> 'INVOICE' .AND. SEEK(&PackTemp..Pack_No)
      IF llNextLabel
        GO lnStrtFrm
      ENDIF
      llNextLabel = .F.
      SCAN WHILE lnMinBox <= lnOfBox FOR Pack_No = &PackTemp..Pack_No .AND. ;
                    lnMinBox >= From_Crt .AND. lnMinBox <= To_Crt
        IF lnNoOfLn + lnNoOfSze < lnMaxLine
          lcStyle   = Pack_Lin.Style
          IF lcRpSizes = 'Y'
            lcSize    = ' ' + PADL(ALLTRIM(STR(Qty1,4)),4) + ' ' + PADL(ALLTRIM(STR(Qty2,4)),4) + ' ' + ;
                              PADL(ALLTRIM(STR(Qty3,4)),4) + ' ' + PADL(ALLTRIM(STR(Qty4,4)),4) + ' ' + ;
                              PADL(ALLTRIM(STR(Qty5,4)),4) + ' ' + PADL(ALLTRIM(STR(Qty6,4)),4)
          ELSE
            lcSize    = SPACE(16)
          ENDIF
          lnPcsBox  = Pack_Lin.TotQty
          *-- Print style scale.
          IF lcRpSizes  = 'Y'
            =SEEK( Pack_Lin.Style,'Style')            
            IF lcScale <> Style.Scale
              IF lnNoOfLn + lnNoOfSze <> lnMaxLine
                lcScale  = Style.Scale
                 *lcPrLine = SPACE(15)+IIF(LEN(ALLTRIM(SZ1))>2,'',SPACE(2-LEN(ALLTRIM(SZ1))))+ALLTRIM(SZ1)+Y+;
                            IIF(LEN(ALLTRIM(SZ2))>2,'',SPACE(2-LEN(ALLTRIM(SZ2))))+ALLTRIM(SZ2)+Y+;
                            IIF(LEN(ALLTRIM(SZ3))>2,'',SPACE(2-LEN(ALLTRIM(SZ3))))+ALLTRIM(SZ3)+Y+;
                            IIF(LEN(ALLTRIM(SZ4))>2,'',SPACE(2-LEN(ALLTRIM(SZ4))))+ALLTRIM(SZ4)+Y+;
                            IIF(LEN(ALLTRIM(SZ5))>2,'',SPACE(2-LEN(ALLTRIM(SZ5))))+ALLTRIM(SZ5)+Y+;
                            IIF(LEN(ALLTRIM(SZ6))>2,'',SPACE(2-LEN(ALLTRIM(SZ6))))+ALLTRIM(SZ6)
                IF SEEK('S'+ Style.Scale,'SCALE')
                  lcPrLine = SPACE(21)+PADL(ALLTRIM(SCALE.SZ1),4)+Y+ PADL(ALLTRIM(SCALE.SZ2),4)+Y+;
                             PADL(ALLTRIM(SCALE.SZ3),4)+Y+ PADL(ALLTRIM(SCALE.SZ4),4)+Y+ ;
                             PADL(ALLTRIM(SCALE.SZ5),4)+Y+ PADL(ALLTRIM(SCALE.SZ6),4)+SPACE(4)+" TOTAL"
                ELSE
                  lcPrLine = SPACE(52)+" TOTAL"
                ENDIF
                LABEL FORM (GCREPHOME+GCACT_APPL+'\ALPLIST2') NEXT 1 TO PRINT NOCONSOL
              ENDIF
              lnNoOfSze = lnNoOfSze + 1
            ENDIF
          ENDIF
          IF lnNoOfLn + lnNoOfSze <= lnMaxLine .OR. lnNoInPack + lnNoOfSze = lnMaxLine
            lcPrLine = lcStyle + SPACE(1) + lcSize + SPACE(4) + IIF( lnPcsBox = 0, SPACE(6), STR( lnPcsBox, 6 ) )
          ELSE
            lcStyle  = SPACE(12)
            lcSize   = SPACE(20)
            lcPrLine = SPACE(40) + IIF( lnPcsBox = 0, SPACE(4), STR( lnPcsBox, 4 ) )
          ENDIF

          SELECT &PackTemp
          LABEL FORM (GCREPHOME+GCACT_APPL+'\ALPLIST2') NEXT 1 TO PRINT NOCONSOL
          lnPcsBox  = 0
          lnNoOfLn  = lnNoOfLn + 1

          IF !EMPTY(lcStyle)
            IF lnNoOfLn + lnNoOfSze >= lnMaxLine
              STORE 1 TO lnNoOfLn, lnNoOfSze
              SELECT &PackTemp
              LABEL FORM (GCREPHOME+GCACT_APPL+'\ALPLIST3') NEXT 1 TO PRINT NOCONSOL
	          LABEL FORM (GCREPHOME+GCACT_APPL+'\ALPLIST1') NEXT 1 TO PRINT NOCONSOL
            ENDIF
            DO lpPrtSku
          ENDIF
        ELSE
          llNextLabel = .T.
          lnStrtFrm   = RECNO()
          EXIT
        ENDIF
      ENDSCAN
    ELSE
      lnPcsBox  = 0
    ENDIF
    SELECT &PackTemp

    DO WHILE lnNoOfLn + lnNoOfSze <= lnMaxLine .AND. !llNextLabel
      lcStyle  = SPACE(12)
      lcSize   = SPACE(16)
      lcPrLine = SPACE(40) + IIF( lnPcsBox = 0, SPACE(4), STR( lnPcsBox, 4 ) )
      LABEL FORM (GCREPHOME+GCACT_APPL+'\ALPLIST2') NEXT 1 TO PRINT NOCONSOL
      lnNoOfLn = lnNoOfLn + 1
      lnPcsBox = 0
    ENDDO

    IF llUseUps
      SELECT UpsBox      
      LOCATE FOR Invoice=xInvoice
      SCAN REST WHILE Invoice = xInvoice     
        IF Cartons = lnMinBox        
          lnWeight = Weight
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
    SELECT &PackTemp
    LABEL FORM (GCREPHOME+GCACT_APPL+'\ALPLIST3') NEXT 1 TO PRINT NOCONSOL

    *-- Do not increment the box number if the label did not print all styles 
    *-- on 1 page.
    IF !llNextLabel
      lnMinBox = lnMinBox + 1
    ENDIF
    lnNoOfLn = 1
  ENDDO
ENDSCAN

DO ENDREPORT
SET DEVICE TO SCREEN
gcDevice = 'SCREEN'
**********************************************************************
* PROCE : SIL310
* DESC  : Collect data into the main temporary file
* AUTH  : Sameh Said
* DATE  : 02/10/99
* PARA  : NONE
*!********************************************************************
*! Modifications :
*!********************************************************************
PROCEDURE SIL310

SELECT INVHDR
SCAN FOR &lcRpExp
  IF CONSOL <> 'Y'
    IF !EMPTY(INVHDR.PikTkt) .AND. SEEK(INVHDR.PikTkt,'PACK_LIN')
      =SEEK(INVHDR.PikTkt,'PACK_Hdr')
      SELECT PACK_Hdr
      INSERT INTO (PACKTEMP) (PACK_NO, ACCOUNT, STORE, ORDER, Note,TOT_WGHT,;
                  TOT_CART, TOT_PCS, INVOICE, cpkChcode) ;
      VALUES (PACK_Hdr.PACK_NO,PACK_Hdr.ACCOUNT,PACK_Hdr.STORE,PACK_Hdr.ORDER,;
                   PACK_Hdr.Note,PACK_Hdr.TOT_WGHT,PACK_Hdr.TOT_CART,;
                             PACK_Hdr.TOT_PCS, InvHdr.INVOICE, 'P' )
    ELSE
      INSERT INTO (PACKTEMP) (PACK_NO, ACCOUNT, STORE, ORDER, Note,TOT_WGHT,;
                  TOT_CART, TOT_PCS, Sp_Inst1, INVOICE, cpkChcode) ;
      VALUES (InvHdr.Invoice,InvHdr.ACCOUNT,InvHdr.STORE,InvHdr.ORDER,;
                   InvHdr.Note2,InvHdr.Weight,InvHdr.Cartons,;
                             InvHdr.Ship,'INVOICE',InvHdr.INVOICE, 'I' )

    ENDIF
  ELSE     && i.e. CONSOL = 'Y'
    SELECT ConsInvH
    =SEEK(INVHDR.Invoice)
    SCAN WHILE INVHDR.Invoice = Invoice
      IF SEEK(ConsInvH.PikTkt,'PACK_LIN')
        =SEEK(ConsInvH.PikTkt,'PACK_Hdr')
        SELECT PACK_Hdr
        INSERT INTO (PACKTEMP) (PACK_NO, ACCOUNT, STORE, ORDER, Note,TOT_WGHT,;
                  TOT_CART, TOT_PCS,  INVOICE, cpkChcode) ;
        VALUES (PACK_Hdr.PACK_NO,PACK_Hdr.ACCOUNT,PACK_Hdr.STORE,PACK_Hdr.ORDER,;
                   PACK_Hdr.Note,PACK_Hdr.TOT_WGHT,PACK_Hdr.TOT_CART,;
                             PACK_Hdr.TOT_PCS, InvHdr.INVOICE, 'P' )
      ELSE
        INSERT INTO (PACKTEMP) (PACK_NO, ACCOUNT, STORE, ORDER, Note,TOT_WGHT,;
                  TOT_CART, TOT_PCS, Sp_Inst1, INVOICE, cpkChcode) ;
        VALUES (InvHdr.Invoice,InvHdr.ACCOUNT,ConsInvH.STORE,InvHdr.ORDER,;
                   InvHdr.Note2,InvHdr.Weight,InvHdr.Cartons,;
                             InvHdr.Ship, 'INVOICE',InvHdr.INVOICE, 'I' )
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
*SELECT &PACKTEMP

*!*************************************************************
*! Name : lfGetNo
*! Auth : Sameh Said
*! Date : 02/10/99
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Return the max number or the minmum of cartons.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Passed   : lcPackNo : The pack number.
*!*************************************************************
*!Modifications:
*!*************************************************************
FUNCTION lfGetNo
PARAMETERS lcPackNo, lcSign, lcFiled

PRIVATE lnAlias, lnRecNo, lnMax
lnAlias = SELECT()

SELECT Pack_Lin

lnMax = 0
IF &PackTemp..cpkChCode = "P" AND  SEEK (lcPackNo)
  lnRecNo = RECNO()
  lnMax   = To_Crt
ELSE                  && MFM 06/15/95.

  *--Added to get the maxmum number of cartons from the invhdr.
  IF SEEK(lcPackNo,'InvHdr') .AND. lcSign = '>='
    *-- Get the total invoice number from the consl inv hdr.
    IF InvHdr.Consol = 'Y'
      =SEEK(lcPackNo+&PACKTEMP..Store,'ConsInvH')
      lnMax = ConsInvH.Cartons
    ELSE
      lnMax = InvHdr.Cartons
    ENDIF

  ENDIF
  lnRecNo = 1         && MFM 06/15/95.
ENDIF

SCAN WHILE lcPackNo = Pack_No
  lnMax = IIF ( To_Crt &lcSign lnMax , &lcFiled, lnMax )
ENDSCAN

llGoRec = .T.
IF EOF()
  GO TOP
  IF EOF()
    llGoRec = .F.
  ENDIF
ENDIF

IF llGoRec
  GO lnRecNo
ENDIF

SELECT ( lnAlias )
RETURN IIF(lnMax=0,1,lnMax)  && MFM 06/15/95.

*!***************************************************************
*! Name : lpPrtSku.
*! Auth : Sameh Said
*! Date : 02/10/99
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the style/color Skus for a specific account.
*!***************************************************************
PROCEDURE lpPrtSku

IF ! SEEK('S'+&PACKTEMP..Account+lcStyle,'Spck_Lin')
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
  DIMENSION laSku[8]
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
  lnI1 = 1
  GO TOP
  = SEEK('S'+&PACKTEMP..Account+Style.Style,'Spck_Lin')

  SCAN WHILE Type+Account+Style+Color = 'S'+&PACKTEMP..Account+Style.Style+Style.Color .AND. lnI < 9;
            .AND. lnNoOfLn + lnNoOfSze <= lnMaxLine
    lcI = ALLTRIM(STR(lnI))
    llLastSku=.F.
    IF PACK_LIN.QTY&lcI<> 0 AND !EMPTY(laSku(lnI))
      IF MOD(lnI1,2) <> 0
        lcPrLine = 'SKU N#' + SUBSTR(Pack_Id,1,lnDime1) + ' '+Scale.Sz&lcI+':'+laSku(lnI) + ' '
      ELSE
        lcPrLine = lcPrLine + Scale.Sz&lcI+':'+laSku(lnI) + ' '
      ENDIF

      IF MOD(lnI1,2) = 0 .OR. lnI = Scale.cnt
        llLastSku = .T.
        SELECT &PackTemp
        LABEL FORM (GCREPHOME+GCACT_APPL+'\ALPLIST2') NEXT 1 TO PRINT NOCONSOL

        lnNoOfLn = lnNoOfLn + 1
        lnPcsBox = 0
      ENDIF
      lnI1=lnI1+1
    ENDIF    
     IF EMPTY(laSku(lnI))
        SKIP -1
      ENDIF
    lnI = lnI + 1    
  ENDSCAN
  IF MOD(lnI1,2) = 0 AND !llLastSku
    SELECT &PackTemp
    LABEL FORM (GCREPHOME+GCACT_APPL+'\ALPLIST2') NEXT 1 TO PRINT NOCONSOL
    lnNoOfLn = lnNoOfLn + 1
    lnPcsBox = 0
  ENDIF
ELSE
  lcPrLine = 'CUSTOMER SKU # '+ Sku  

  SELECT &PackTemp
  LABEL FORM (GCREPHOME+GCACT_APPL+'\ALPLIST2') NEXT 1 TO PRINT NOCONSOL
  lnNoOfLn = lnNoOfLn + 1
  lnPcsBox = 0
ENDIF

RETURN

*!*************************************************************
*! Name      : lfSRVLBL
*! Developer : Sameh Said
*! Date      : 02/10/99
*! Purpose   : control browsing (Order , Invoice or PikTkt) and validate 
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
*! Example   : =lfSRVLBL()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVLBL
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
        SELECT INVHDR
        SET ORDER TO TAG INVHDR
        SET ORDER TO CUSTOMER IN CUSTOMER
        SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) ;
                INTO CUSTOMER ADDITIVE

    
  CASE lcParm = 'R'  && Reset code
        SELECT INVHDR
        SET RELATION TO
  OTHERWISE      && Valid code
ENDCASE
