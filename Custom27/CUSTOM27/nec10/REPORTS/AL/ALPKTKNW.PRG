*:***************************************************************************
*: Program file  : ALPKTKNW.PRG
*: Program desc. : CUSTOMIZED SALES ORDER ALLOCATION PICKING TICKET FOR NECK WORKS.
*: Date          : 19/06/2002
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102638 , C102659
*:
*:***************************************************************************
*: Calls :
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKNW
*:***************************************************************************
*: Modifications :
***************************************************************************

*-- MAIN LOOP
llNoRec    = .F.         && Variable used to make the endreport function in the main program.
llScale    = .T.         && Flag to print the size scales at the first page only.
llEndPT    = .F.
llNewDoc   = .T.
lnNotLine  = 1
lcCustPO   = ''
lnPkTktNo  = 22
lcStrToPrn = SPACE(0)
DIMENSION laScaleSz[3]
STORE SPACE (0) TO laScaleSz
STORE 0 TO lnHdrRow , lnLinRow , lnBotRow , MaxRow , lnPageNo

*--Change the tag in the SPCK_LIN file.
SET ORDER TO TAG Spcklins IN SPCK_LIN

*--If EOF it will return to the main program
IF EOF(lcTmpOrdL)
  llNoRec = .T.
  *-- Message <No records to display.>
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT

SELECT (lcTmpOrdL)
SCAN
  WAIT WINDOW 'Printing PT# '+&lcTmpOrdL..PikTkt NOWAIT

  *-- GET ORDERHDR & LINE ITEMS
  IF llNewDoc
    SELECT (lcTmpOrdL)
    lnPageNo  = 0
    lcPikTkt  = PikTkt
    lcOrder   = Order
    ldDate    = PIKTKT.DATE
    lcStore   = PIKTKT.Store
    lcAccount = ORDHDR.Account
    lcCustPo  = LEFT(IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO),10)

    SELECT CUSTOMER
    lcBtName  = CUSTOMER.BTNAME
    lcBtAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
    lcBtAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
    lcBtAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')

    IF LEN(TRIM(lcBtAddr2)) = 0
      lcBtAddr2 = lcBtAddr3
      lcBtAddr3 = ''
    ENDIF

    *-- Get descriptions for Coded fields.
    XTERM_DATA = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')
    XSHIP_DATA = gfCodDes(IIF(ALLTRIM(ORDHDR.SHIPVIA) = "*" ,CUSTOMER.SHIPVIA,ORDHDR.SHIPVIA),'SHIPVIA')
    XDIVI_DATA = LEFT(gfCodDes(ORDHDR.CDIVISION,'CDIVISION'),9)

    STORE "" TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4
    lnPieces = 0
  ENDIF
  *-- END PICK TKT/ORDER SELECTION.

  *-- SHIP-TO ADDRESS FOR THIS STORE
  *- Initialize the alt address if ther is any.
  SELECT ORDHDR
  SEEK cordtype + LCORDER

  IF Alt_ShpTo
    lcStName  = OrdHdr.STName
    lcStAddr1 = OrdHdr.cAddress1
    lcStAddr2 = OrdHdr.cAddress2
    lcStAddr3 = TRIM(OrdHdr.cAddress3) + ' ' + TRIM(OrdHdr.cAddress4) + ' ' + OrdHdr.cAddress5

    IF LEN(TRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3
      lcStAddr3 = ''
    ENDIF
  ELSE
    SELECT Customer
    IF !EMPTY(CUSTOMER.Dist_Ctr)
      =SEEK("S" + CUSTOMER.Account + CUSTOMER.Dist_Ctr )
    ENDIF

    lcStName  = IIF( EMPTY(DBA) , STNAME , DBA)
    lcStAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
    lcStAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
    lcStAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )

    IF LEN(TRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3
      lcStAddr3 = ''
    ENDIF
  ENDIF
  SELECT ORDHDR

  *-- START PRINTING
  DO lpHdrLabls

  *-- LINE LOOP
  SELECT (lcTmpOrdL)
  llNewDoc = .T.
  lnTotQty = 0

  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6) = lcPikTkt + lcOrder + 'D'

    IF lnLinRow >= MaxRow
      DO lpOldFoter
      DO lpHdrLabls
    ENDIF

    SELECT STYLE
    lcStyDesc = IIF(SEEK(&lcTmpOrdL..Style),Desc,'')

    SELECT (lcTmpOrdL)
    @ lnLinRow,00 SAY LEFT(Style,12)
    @ lnLinRow,14 SAY SUBSTR(STYLE, 14 , 6)
    @ lnLinRow,22 SAY LEFT(Scale,1)
    @ lnLinRow,25 SAY Pik1   PICTURE '@Z 9999'
    @ lnLinRow,31 SAY Pik2   PICTURE '@Z 9999'
    @ lnLinRow,37 SAY Pik3   PICTURE '@Z 9999'
    @ lnLinRow,43 SAY Pik4   PICTURE '@Z 9999'
    @ lnLinRow,49 SAY Pik5   PICTURE '@Z 9999'
    @ lnLinRow,55 SAY Pik6   PICTURE '@Z 9999'
    @ lnLinRow,61 SAY Pik7 + Pik8   PICTURE '@Z 9999'
    @ lnLinRow,66 SAY TotPik PICTURE '99999'
    lnLinRow = lnLinRow + 1
    IF lnLinRow >= MaxRow
      DO lpOldFoter
      DO lpHdrLabls
    ENDIF

    @ lnLinRow,00 SAY lcStyDesc

    *--SKU PRINTING FOR ACCOUNTS THAT HAVE SKU'S ON FILE.
    IF llRpSkuBck
      DO lpPrtSku      && Procedure to print the Style/Color Sku no.
      IF !EMPTY(lcStrToPrn)
        lnLinRow = lnLinRow + 1
      ELSE
        lnLinRow = lnLinRow + 2
      ENDIF
    ELSE
      lnLinRow = lnLinRow + 2
    ENDIF

    IF lnLinRow >= MaxRow
      DO lpOldFoter
      DO lpHdrLabls
    ENDIF

    *--SKU PRINTING FOR ACCOUNTS THAT HAVE SKU'S ON FILE.
    IF llRpSkuBck
      IF !EMPTY(lcStrToPrn)
        @ lnLinRow,00 SAY lcStrToPrn
        lnLinRow = lnLinRow + 2
      ENDIF
    ENDIF

    lnPieces = lnPieces + TotPik
    SELECT (lcTmpOrdL)
  ENDSCAN
  *-- END PRINT LINE LOOP

  *-- To print the notepad.
  IF llNewDoc
    SELECT (lcTmpOrdL)
    IF llRpOrdNot
      SELECT NotePad
	  IF SEEK('B' + lcOrder)
        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 75
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)
        @ lnLinRow,00 SAY '* -- N O T E S -- *'
        lnLinRow = lnLinRow + 1

        DO WHILE lnNotLine <= lnMemLins
          IF lnLinRow >= MaxRow
            lnLinRow = lnLinRow + 2
            DO lpOldFoter
            DO lpHdrLabls
          ENDIF
          @ lnLinRow,00 SAY MLINE(MNOTES,lnNotLine)
          lnLinRow = lnLinRow + 1
          lnNotLine = lnNotLine + 1
        ENDDO
        SET MEMOWIDTH TO lnOldMemW
      ENDIF
    ENDIF
    lnNotLine = 1
    llEndPT = .T.
    DO lpNewFoter
  ELSE
    llEndPT = .F.
    DO lpOldFoter
    LOOP
  ENDIF

  llScale = .T.     && Re-initialize llScale to get the size scales for a new P/T
  SELECT (lcTmpOrdL)
ENDSCAN

WAIT CLEAR
SET DEVICE TO SCREEN
RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpHdrLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/06/2002
*! Purpose   : Print the Header labels.
*!*************************************************************
*! Called from : ALPKTKNW.PRG
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpHdrLabls()
*!*************************************************************
PROCEDURE lpHdrLabls

PRIVATE lcAlias
lcAlias = ALIAS()

IF lnPkTktNo = 22
  lnHdrRow  = 01                   && Start header row
  lnLinRow  = 23                   && Start lines printing row.
  lnBotRow  = 73                   && Start footer row.
  MaxRow    = lnBotRow - 03        && Max row number for lines printing.
  lnPkTktNo = 01
ELSE
  lnHdrRow  = lnBotRow + 6         && Start header row.
  lnLinRow  = lnHdrRow + 22        && Start lines printing row.
  lnBotRow  = lnLinRow + 50        && Start footer row.
  MaxRow    = lnBotRow - 03        && Max row number for lines printing.
  lnPkTktNo = lnPkTktNo  + 01
ENDIF

@ lnHdrRow,74 SAY IIF(!EMPTY(OrdHdr.Ship),'BACKORDER','')
lnHdrRow = lnHdrRow + 2

*-- Line 3
@ lnHdrRow,060 SAY XDIVI_DATA
@ lnHdrRow,073 SAY lcPikTkt
lnHdrRow = lnHdrRow + 2

*-- Line 5
@ lnHdrRow,058 SAY OrdHdr.Approval
lnHdrRow = lnHdrRow + 1

*-- Line 6
@ lnHdrRow,060 SAY lcOrder
@ lnHdrRow,073 SAY ldDate
lnHdrRow = lnHdrRow + 3

*-- Line 9
@ lnHdrRow,005 SAY lcBtName
@ lnHdrRow,048 SAY lcStName
lnHdrRow = lnHdrRow + 1

*-- Line 10
@ lnHdrRow,005 SAY lcBtAddr1
@ lnHdrRow,048 SAY lcStAddr1
lnHdrRow = lnHdrRow + 1

*-- Line 11
@ lnHdrRow,005 SAY lcBtAddr2
@ lnHdrRow,048 SAY lcStAddr2
lnHdrRow = lnHdrRow + 1

*-- Line 12
@ lnHdrRow,005 SAY lcBtAddr3
@ lnHdrRow,048 SAY lcStAddr3
lnHdrRow = lnHdrRow + 4

*-- Line 16
@ lnHdrRow,000 SAY lcAccount
@ lnHdrRow,008 SAY OrdHdr.REP1
@ lnHdrRow,012 SAY OrdHdr.REP2
@ lnHdrRow,017 SAY SUBSTR(DTOC(OrdHdr.Entered),1,5)
@ lnHdrRow,024 SAY SUBSTR(DTOC(OrdHdr.Start),1,5)
@ lnHdrRow,032 SAY DTOC(OrdHdr.Complete)
@ lnHdrRow,042 SAY SUBSTR(XTERM_DATA,1,09)
@ lnHdrRow,055 SAY lcStore
@ lnHdrRow,068 SAY IIF(OrdHdr.MultiPO , lcCustPO , LEFT(OrdHdr.CUSTPO,10))
@ lnHdrRow,078 SAY OrdHdr.DEPT
lnHdrRow = lnHdrRow + 3

IF llScale
  STORE SPACE (0) TO laScaleSz
  lnCount   = 1
  lcScalCnt = 0
  lcSelFile = SELECT(0)
  SELECT ORDLINE
  SEEK ORDHDR.CORDTYPE + lcORDER
  lcKey = CORDTYPE + ORDER + STR(LINENO,6)
  SCAN REST WHILE ORDER = lcORDER .AND. lcScalCnt <= 3
    IF ASCAN(laScaleSz,SUBSTR(ORDLINE.SCALE,1,2)) = 0 .AND. SEEK('S'+ORDLINE.SCALE,'SCALE')
      lcScalCnt = lcScalCnt +1
      DIMENSION laScaleSz[lcScalCnt]
      lcScalSiz = SUBSTR(ORDLINE.SCALE,1,2)         + '  '  + PADL(ALLTRIM(SCALE.SZ1),3,' ') + '   ' + ;
                     PADL(ALLTRIM(SCALE.SZ2),3,' ') + '   ' + PADL(ALLTRIM(SCALE.SZ3),3,' ') + '   ' + ;
                     PADL(ALLTRIM(SCALE.SZ4),3,' ') + '   ' + PADL(ALLTRIM(SCALE.SZ5),3,' ') + '   ' + ;
                     PADL(ALLTRIM(SCALE.SZ6),3,' ') + '   ' + PADL(ALLTRIM(SCALE.SZ7),3,' ')
      laScaleSz[lcScalCnt] = lcScalSiz
    ENDIF
  ENDSCAN

  =SEEK(lcKey)
  llScale =.F.
ENDIF

FOR lnRowCnt = 1 TO 3
  IF lnRowCnt <= ALEN(laScaleSz,1)
    @ lnHdrRow,22 SAY laScaleSz[lnRowCnt]
  ENDIF
  lnHdrRow = lnHdrRow + 1
ENDFOR
SELECT (lcAlias)

*--End of lpHdrLabls.
*!*************************************************************
*! Name      : lpOldFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/06/2002
*! Purpose   : Print the form footer.
*!*************************************************************
*! Called from : ALPKTKNW.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpOldFoter()
*!*************************************************************
PROCEDURE lpOldFoter

lnBotRow = lnBotRow + 1
@ lnBotRow,34 SAY '*** CONTINUED NEXT PAGE ***'
lnPageNo = lnPageNo + 1
@ lnBotRow,01 SAY 'PAGE '+ALLTRIM(STR(lnPageNo,3))
lnBotRow = lnBotRow + 4
@ lnBotRow,059 SAY SUBSTR(XSHIP_DATA,1,11)
lnBotRow = lnBotRow + 1

*--End of lpOldFoter.
*!*************************************************************
*! Name      : lpNewFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/06/2002
*! Purpose   : Print the form footer.
*!*************************************************************
*! Called from : ALPKTKNW.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpNewFoter()
*!*************************************************************
PROCEDURE lpNewFoter

@ lnBotRow - 3,000 SAY lcRpMsg1
@ lnBotRow - 2,000 SAY lcRpMsg2
@ lnBotRow - 1,000 SAY lcRpMsg3

lnBotRow = lnBotRow + 1
lnCol = 10
@ lnBotRow,075 SAY lnPieces PICTURE '999999'
lnPageNo = lnPageNo + 1
lnBotRow = lnBotRow + 1
@ lnBotRow,01 SAY 'PAGE '+ALLTRIM(STR(lnPageNo,3))
=SEEK('O' + LCORDER ,'ORDHDR')
IF SUBSTR(OrdHdr.Note1, 1, 1) <> '*'
  @ lnBotRow,lnCol SAY OrdHdr.Note1
  lnCol = 41
ENDIF
@ lnBotRow,lnCol SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1)<>'*', OrdHdr.Note2, '')
lnBotRow = lnBotRow + 3
@ lnBotRow,059 SAY SUBSTR(XSHIP_DATA,1,11)
lnBotRow = lnBotRow + 1

*--End of lpNewFoter.
*!*************************************************************
*! Name      : lfTotQty
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/06/2002
*! Purpose   : Get the rest of quantities of partial invoice.
*!*************************************************************
*! Called from : ALPKTKNW.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfTotQty()
*!*************************************************************
FUNCTION lfTotQty
PRIVATE lnAlias

lnAlias = SELECT()
lnTotQty = 0
IF SEEK (Order+STR(LineNo,6), 'InvLine')
  lcOrder  = Order
  lcLineNo = STR(LineNo,6)
  SELECT InvLine
  SCAN REST WHILE Order+STR(LineNo,6) = lcOrder+lcLineNo
    lnTotQty = TotQty
  ENDSCAN
ENDIF

SELECT (lnAlias)
RETURN lnTotQty

*-- End of lfTotQty.
*!*************************************************************
*! Name      : lpPrtSku
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/22/2001
*! Purpose   : Print the main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKNW.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : DO lpPrtSku
*!*************************************************************
PROCEDURE lpPrtSku
PRIVATE lcALias

lcALias = SELECT(0)
lcStrToPrn = ''

IF !SEEK('S'+lcAccount+&lcTmpOrdL..Style,'Spck_Lin')
  lcStrToPrn = ""
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  =SEEK('S'+Style.Scale,'Scale')
  =SEEK('M'+lcAccount,'Customer')
  lcSkuTmpl = IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S' + lcSkuTmpl , 'SkuTmpl')
    lnDime1 = SkuTmpl.Len1 + SkuTmpl.Len2 + SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF
  =SEEK('S' + lcAccount + &lcTmpOrdL..Style,'Spck_Lin')
  lcStrToPrn = SUBSTR(Pack_Id,1,lnDime1) + '  Customer SKU #'
ELSE
  lcStrToPrn = SUBSTR(Sku,1,8) + '  Customer SKU #'
ENDIF
SELECT(lcALias)

RETURN lcStrToPrn

*--End of lpPrtSku.