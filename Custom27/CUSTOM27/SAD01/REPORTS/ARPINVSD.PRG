*:***************************************************************************
*: Program file  : ARPINVSD.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR SADIMARA.
*: Date          : 05/24/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102322
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVSD
*:***************************************************************************
*: Modifications :
*: B604547,1 BWA 06/14/2001 Some modification dueto the customer.
*--Before running this Custom Program, must run 
*--B604500  B604004 E301621 C102212 C102323 B604547.
*: C102320,1 BWA 08/05/2001 Print the Pre-Billed invoices.
***************************************************************************

*--The Color , Style Length and Tax reference.
STORE 0 TO lnClrLnSd , lnClrPosSd
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrPosSd = laItemSeg[lnCount,4]
    lnClrLnSd  = LEN(laItemSeg[lnCount,3])
    EXIT
  ENDIF
ENDFOR

*B604547,1 BWA 06/14/2001 Some modification dueto the customer.[START]
IF !USED(lcWarHous)
  = gfOpenFile(gcDataDir+'WAREHOUS' ,gcDataDir+'WAREHOUS', 'SH', @lcWarHous, .T.)
ENDIF
*B604547,1 BWA 06/14/2001 [END]

*C102320,1 BWA 08/05/2001 Print the Pre-Billed invoices.[START]
IF lcInvKind = "S"
  SELECT INVHDR
  SET RELATION TO CWARECODE INTO (lcWarHous) ADDITIVE
ELSE
  *-- Section break the relatoins.
  SELECT INVHDR
  SET RELATION OFF INTO (lcTmpDbt)
  SET RELATION OFF INTO CUSTOMER
  SET RELATION OFF INTO Ordhdr
  SELECT (lcTmpDbt)
  SET RELATION TO

  SELECT InvLine
  SET RELATION OFF INTO STYLE
  SET RELATION OFF INTO SPCK_LIN

  SELECT STYLE
  SET RELATION OFF INTO SCALE
  *-- End Section break the relatoins.

  SELECT INVHDRS

  lcRpExp = STRTRAN(lcRpExp,"INVHDR","INVHDRS")
  lcRpExp = lcRpExp + IIF(EMPTY(lcInvoicSd) , "" , "  AND " + lcInvoicSd) + " AND EMPTY(INVHDRS.FLAG)"
  LOCATE FOR &lcRpExp

  IF !FOUND()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    llNoRec = .T.
    llarpinv = .F.
    SET DEVICE TO SCREEN
    RETURN
  ENDIF
  SET DEVICE TO PRINT

  SELECT INVLINES
  =AFIELDS(laFileStru)
  CREATE TABLE (gcWorkDir + lcInvLineS) FROM ARRAY laFileStru
  INDEX ON CSADINV + STR(LINENO,6) TAG lcInvLineS OF (gcWorkDir + lcInvLineS + '.CDX')

  SELECT INVHDRS
  SET RELATION TO INVHDRS.CSADINV INTO INVLINES ADDITIVE
  SCAN FOR &lcRpExp
    *-lcInvcee    >> Variable hold the invoice No.
    lcInvcee = INVHDRS.CSADINV
    SELECT INVLINES
    SCAN REST WHILE CSADINV + STR(LINENO,6) = lcInvcee AND INVHDRS.CONSOL = INVLINES.CONSOL AND INVLINES.TOTQTY > 0
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      SCATTER MEMVAR MEMO
      SELECT(lcInvLineS)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDSCAN
  ENDSCAN

  SELECT INVHDRS
  SET RELATION OFF INTO INVLINES

  *--Clos the invline and open the temp. file with the invline name.
  SELECT INVLINES
  CLOSE INDEX
  USE IN INVLINES
  USE IN (lcInvLineS)
  USE (gcWorkDir+lcInvLineS) IN 0 ALIAS INVLINES
  SELECT INVLINES
  SET ORDER TO 1

               *-- Section Create the new relatoins --*
  SELECT INVHDRS
  IF llPrntInst .OR. llRpInvNot
    SET RELATION TO '' INTO (lcTmpDbt)
    SELECT (lcTmpDbt)
    SET RELATION TO IIF(CFILE_NUM = '3', INVHDRS.CSADINV , '*') INTO ARINSTMD ADDITIVE
    SET RELATION TO IIF(CFILE_NUM = '1', INVHDRS.CSADINV , '*') INTO INVLINES ADDITIVE
  ELSE
    SET RELATION TO INVHDRS.CSADINV INTO INVLINES ADDITIVE
  ENDIF

  SELECT INVLINES
  LOCATE
  SET RELATION TO IIF(!EMPTY(INVLINES.ALTSTYLE) , INVLINES.ALTSTYLE , INVLINES.Style) INTO STYLE ADDITIVE
  SET RELATION TO "S" + INVLINES.Account + INVLINES.Style INTO SPCK_LIN ADDITIVE

  SELECT STYLE
  SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

  SELECT INVHDRS
  SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
  SET RELATION TO 'O' + INVHDRS.ORDER INTO OrdHdr ADDITIVE
  SET RELATION TO 'C' + INVHDRS.INVOICE INTO NOTEPAD ADDITIVE
  SET RELATION TO CWARECODE INTO (lcWarHous) ADDITIVE

  IF llPrntInst .OR. llRpInvNot
    SET SKIP TO (lcTmpDbt) , INVLINES , ARINSTMD
  ELSE
    SET SKIP TO INVLINES
  ENDIF

  SELECT INVHDRS
  LOCATE
  DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp

  IF gcDevice <> 'SCREEN'
    SELECT INVHDRS
    SCAN FOR &lcRpExp
      REPLACE INVHDRS.PRTFLAG WITH 'P'
    ENDSCAN
  ENDIF

  SELECT STYLE
  SET RELATION OFF INTO SCALE

  SELECT (lcTmpDbt)
  SET RELATION OFF INTO ARINSTMD

  SELECT INVHDRS
  SET RELATION OFF INTO (lcTmpDbt)
  SET RELATION OFF INTO CUSTOMER
  SET RELATION OFF INTO Ordhdr
  SET RELATION OFF INTO (lcWarHous)
  SET RELATION OFF INTO NOTEPAD

  SET DEVICE TO SCREEN
  llarpinv = .F.
  WAIT CLEAR

  SELECT INVLINES
  CLOSE INDEX
  USE IN INVLINES
  =gfOpenFile(gcDataDir + "INVLINES" , gcDataDir + "INVLINES" ,'SH')

  SELECT INVHDRS
  LOCATE

ENDIF
*C102320,1 BWA 08/05/2001 Print the Pre-Billed invoices.[END]

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfSolShp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/24/2001
*! Purpose   : Function to Get the Sold to & Ship to Address
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfSolShp
PARAMETER lcSolShp
PRIVATE lcAlias

*B604547,1 BWA 06/14/2001 Some modification dueto the customer.[START]
*DECLARE laSoldTo[5,1] , laShipTo[5,1]
DECLARE laSoldTo[5,1] , laShipTo[5,1] , laCmAdr[5,1]
*B604547,1 BWA 06/14/2001 [END]

laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
lcShpTName = ''

*B604547,1 BWA 06/14/2001 Some modification dueto the customer.[START]
laCmAdr = ''           && Array to hold the warehous address.

*-- Get the WareHouse addresses
laCmAdr[1] = gfGetAdr(lcWarHous , '' , '' , '' , 1)
laCmAdr[2] = gfGetAdr(lcWarHous , '' , '' , '' , 2)
laCmAdr[3] = gfGetAdr(lcWarHous , '' , '' , '' , 3)
laCmAdr[4] = gfGetAdr(lcWarHous , '' , '' , '' , 4)
laCmAdr[5] = gfGetAdr(lcWarHous , '' , '' , '' , 5)
=lfAdrShift('laCmAdr')
*B604547,1 BWA 06/14/2001 [END]

lcSolTName = CUSTOMER.BTName
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

IF OrdHdr.Alt_ShpTo
  lcShpTName  = OrdHdr.STName
  laShipTo[1] = OrdHdr.cAddress1
  laShipTo[2] = OrdHdr.cAddress2
  laShipTo[3] = OrdHdr.cAddress3
  laShipTo[4] = OrdHdr.cAddress4
  laShipTo[5] = OrdHdr.cAddress5
ELSE
  lnCUSRec = 0
  lcAlias = SELECT(0)
  SELECT CUSTOMER
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
  ENDIF

  *C102320,1 BWA 08/05/2001 Print the Pre-Billed invoices.[START]
  IF lcInvKind = "S"
    lcStoreNo   = IIF(EMPTY(INVHDR.STORE) .OR. INVHDR.STORE = "********" ,"",'Store # : '+INVHDR.STORE)
    lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
                  IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

    laShipTo[1] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
    laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
    laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
    laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
    laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  ELSE
    lcStoreNo   = IIF(EMPTY(INVHDRS.STORE).OR.INVHDRS.STORE = "********" , "" , 'Store # : '+INVHDRS.STORE)
    lcShpTName  = IIF(INVHDRS.STORE = "********" , "At Store Level " ,;
                  IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

    laShipTo[1] = IIF(INVHDRS.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
    laShipTo[2] = IIF(INVHDRS.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
    laShipTo[3] = IIF(INVHDRS.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
    laShipTo[4] = IIF(INVHDRS.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
    laShipTo[5] = IIF(INVHDRS.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  ENDIF
  *C102320,1 BWA 08/05/2001 Print the Pre-Billed invoices.[END]

  IF lnCUSRec <> 0
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
  SELECT(lcAlias)
ENDIF

=lfAdrShift('laShipTo')

*C102320,1 BWA 08/05/2001 Print the Pre-Billed invoices.[START]
IF lcInvKind = "S"
  lcShpVSd = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
  lcTermSd = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')
ELSE
  lcShpVSd = gfCodDes(INVHDRS.ShipVia , 'SHIPVIA')
  lcTermSd = gfCodDes(INVHDRS.cTermCode , 'CTERMCODE')
ENDIF
*C102320,1 BWA 08/05/2001 Print the Pre-Billed invoices.[END]

RETURN ''
*-- End of lfSolShp.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 27/03/2001
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 5
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

FOR lnCount = 1 TO 5
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*-- End of lfAdrShift.
*C102320,1 BWA 08/05/2001 Print the Pre-Billed invoices.[START]
*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/05/2001
*! Purpose   : Function to delete the invoice option and clear the read.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfClrRead
PARAMETER LCDUMMY

IF lcInvKind = "P"
  For lnLenAry =  1 TO ALEN(laOGVrFlt,1)
    IF ASUBSCRIPT(laOgVrFlt,ASCAN(laOgVrFlt,'INVHDR.INVOICE'),1) = lnLenAry
      DIMENSION laCopyAry[ALEN(laOGVrFlt,1),ALEN(laOGVrFlt,2)]
      =ACOPY(laOGVrFlt , laCopyAry)
      =ADEL(laOGVrFlt , lnLenAry)
      DIMENSION laOGVrFlt[ALEN(laOGVrFlt,1)-1,ALEN(laOGVrFlt,2)]
      EXIT
    ENDIF
  ENDFOR
ELSE
  DIMENSION laOGVrFlt[ALEN(laCopyAry,1),ALEN(laCopyAry,2)]
  =ACOPY(laCopyAry , laOGVrFlt)
ENDIF

CLEAR READ
LCDUMMY = .T.
RETURN LCDUMMY

*--End of lfClrRead.
*!*************************************************************
*! Name      : lfvInvNoSd
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/05/2001
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
FUNCTION lfvInvNoSd
PARAMETER LCDUMMY

PRIVATE lcAliasSty

lcAliasSty = ALIAS()
STORE SPACE(0) TO lcExpInv , lcInvoicSd
STORE 0 TO lnStyColor

lcBrowFlds = [cSadInv   :H = 'Invoice'     :10 ,]  +;
             [Printed =IIF(PrtFlag = 'P' , 'Yes' , 'No')  :H = 'Printed' : 10 ,]    + ;
             [InvDate   :H = 'Date'        :10 ,]  +;
             [Account   :H = 'Account'     :10 ,]  +;
             [CustPO    :H = 'Reference'   :15 ,]  +;
             [Order     :H = 'Order'       :10 ,]  +;
             [Rep1      :H = 'Sales Rep.'  :15 ,]  +;
             [Ship      :H = 'Pieces'      : 7 ,]  +;
             [ShipAmt   :H = 'Merchandise' :15  ]

SELECT INVHDRS
lcExpInv = 'FOR ' + "INVHDRS.STATUS <> 'V' AND EMPTY(INVHDRS.FLAG)"
=gfrange(lcBrowFlds,lcTempInv,"cSadInv",(lcExpInv),"","","@! XXXXXX")

DIMENSION laStylFle[1,7]
laStylFle[1,1]= "INVHDRS.CSADINV"
laStylFle[1,2]= "F"
laStylFle[1,3]= "C"
laStylFle[1,4]= .T.
laStylFle[1,5]= "In List             "
laStylFle[1,6]= lcTempInv
laStylFle[1,7]= "R"

DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]
=ACOPY(laBrFldFlt,laBrTmpFlt)

DIMENSION laBrFldFlt[2,ALEN(laBrTmpFlt,2)]
laBrFldFlt = ""
laBrFldFlt[2,1] = "INVHDRS"
laBrFldFlt[2,2] = lcBrowFlds
laBrFldFlt[2,3] = ""
laBrFldFlt[2,4] = ""
laBrFldFlt[2,5] = "laStylFle:1"

lcInvoicSd = gfGenFlt('laStylFle',.T.,.T.)
DIMENSION laBrFldFlt[ALEN(laBrTmpFlt,1),ALEN(laBrTmpFlt,2)]
=ACOPY(laBrTmpFlt,laBrFldFlt)

SELECT INVHDRS
LOCATE
SELECT (lcAliasSty)

LCDUMMY = .T.
RETURN LCDUMMY

*--End of lfvInvNoSd.
*!*************************************************************
*! Name      : lfFirstSd
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/05/2001
*! Purpose   : Function to initial the variabls in the first time.
*!*************************************************************
*! Called from : SYREPUVR.DBF
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfFirstSd()
*!*************************************************************
FUNCTION lfFirstSd
PARAMETER lcReturn

*-- Open the ORDLINE.DBF.
IF !USED('INVHDRS')
  llInvHdrs = gfOpenFile(gcDataDir + "INVHDRS" , gcDataDir + "INVHDRS" ,'SH')
ENDIF

IF !USED('INVLINES')
  llInvLins = gfOpenFile(gcDataDir + "INVLINES" , gcDataDir + "INVLINES" ,'SH')
ENDIF

CREATE CURSOR (lcTempInv) (cSadInv C(6))
ZAP
INDEX ON cSadInv TAG (lcTempInv) OF (gcWorkDir+lcTempInv+'.CDX')

RETURN ''
*-- End of lfFirstSd.
*!*************************************************************
*! Name      : lfSclSzSD
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/05/2001
*! Purpose   : To get the scale and the size.
*!*************************************************************
*! Called from : ARPINVSD.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSclSzSD()
*!*************************************************************
FUNCTION lfSclSzSD
PARAMETER lcReturn

PRIVATE lcAlias  , lcX , lnCountSc , lcKey , lnSclCnt , lcScaleSz
lcOldInv = INVHDRS.CSADINV
lcAlias  = ALIAS()
SELECT INVLINES
lcKey    = CSADINV + STR(LINENO,6)  
SEEK INVHDRS.CSADINV
lnSclCnt = 0

STORE SPACE(0) TO laScale
SCAN WHILE CSADINV = INVHDRS.CSADINV  .AND. lnSclCnt < lcScalCont + 1

  IF ASCAN(laScale,INVLINES.SCALE) = 0 .AND. SEEK('S'+INVLINES.SCALE,'SCALE')
    lnSclCnt = lnSclCnt + 1
    laScale[lnSclCnt,1] = SCALE.Scale
    For lnCountSc = 1 TO 8
      lcX = STR(lnCountSc,1)
      laScale[lnSclCnt,lnCountSc+1] = SCALE.SZ&lcX
    ENDFOR
  ENDIF
ENDSCAN
SEEK(lcKey)
SELECT (lcAlias)

RETURN ''
*-- End of lfSclSzSD.
*C102320,1 BWA 08/05/2001 Print the Pre-Billed invoices.[END]