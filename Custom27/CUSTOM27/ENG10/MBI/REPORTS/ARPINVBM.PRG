*:***************************************************************************
*: Program file  : ARPINVBM.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR BMI.
*: Date          : 27/03/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C200164
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVBM
*:***************************************************************************
*: Modifications :
*: C200191,1 BWA 05/09/2001 Changes in the invoice due to the customer.
*: B604584,1 BWA 06/24/2001 Fix the bug in the Line\Release Field.
***************************************************************************

*--The dimension for the sku numbers.
DECLARE laSkuBm[8]
STORE SPACE(0) TO lcOrdNo , lcSuppler

*--Function creat the Temp. file to delete any records if 
*--the user select preview many time and in the first time also.
=lfCratTmp()

*--The Color , Style Length and Tax reference.
STORE 0 TO lnClrLnBm , lnClrPosBm
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrPosBm = laItemSeg[lnCount,4]
    lnClrLnBm  = LEN(laItemSeg[lnCount,3])
    EXIT
  ENDIF
ENDFOR

*--The Style Length
STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))

lcTaxRefr = LEFT(gfGetMemVar('M_TAX_REFE',gcAct_Comp),15)
lnTax_Rat = gfGetMemVar('M_TAX_RATE',gcAct_Comp)

*C200191,1 BWA 05/09/2001 Changes in the invoice due to the customer.[START]
*=lfTempNot()
*C200191,1 [END]

              *--End The Color , Style Length and Tax reference--*
DIMENSION laScaleBm [1,8]
STORE SPACE(0) TO laScaleBm

*-- Check with the lcrpexp in the INVHDR.DBF if there is records for the critiria or not.
SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  *--Case there is no records,to not preview the message tow times.
  llarpinv = .F.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT

SELECT INVHDR
SET SKIP TO
SCAN FOR &lcASExp
  *--Case the user select the invoice to print it to change the flag.
  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF

  lcInvoice = INVHDR.invoice
  WAIT WINDOW 'Selecting Invoices for printing ...' + lcInvoice NOWAIT
  
  SELECT InvLine
  STORE SPACE(0) TO lcScalebm , lcStyleBm
  =SEEK(lcInvoice)

  lnScale = SCALE.CNT
  SCAN REST WHILE Invoice = lcInvoice
    lcInvLinNo = InvLine.LineNo

    STORE SPACE(0) TO lc2ndPrtbm , lcOrdNo
    IF lcScaleBm # InvLine.Scale
      FOR L = 1 TO SCALE.CNT
        lcSiz  = 'Sz' + ALLTRIM(STR(L))
        laScaleBm [1,L] = ALLTRIM(SCALE.&lcSiz)
      ENDFOR
      lcScaleBm = InvLine.Scale
    ENDIF

    SCATTER MEMVAR MEMO
    SELECT (lcTmpLnBm)
    =lfGetSkuBm()
    STORE .T. TO llFrstTmBm

    FOR N = 1 TO lnScale
      lcQty  = 'Qty' + ALLTRIM(STR(N))
      IF INVLINE.&lcQty > 0
        APPEND BLANK

        *C200191,1 BWA 05/09/2001 Changes in the invoice due to the customer.[START]
        *REPLACE &lcTmpLnBm..Invoice   WITH InvLine.Invoice        ,;
                &lcTmpLnBm..Order     WITH InvLine.Order          ,;
                &lcTmpLnBm..Account   WITH InvLine.Account        ,;
                &lcTmpLnBm..Store     WITH InvLine.Store          ,;
                &lcTmpLnBm..Style     WITH InvLine.Style          ,;
                &lcTmpLnBm..Scale     WITH InvLine.Scale          ,;
                &lcTmpLnBm..Price     WITH InvLine.Price          ,;
                &lcTmpLnBm..Desc1     WITH InvLine.Desc1          ,;
                &lcTmpLnBm..Qty1      WITH InvLine.&lcQty         ,;
                &lcTmpLnBm..SizeCode  WITH laScaleBm [1,N]        ,;
                &lcTmpLnBm..Styupc    WITH laSkuBm[N]             ,;
                &lcTmpLnBm..llprnDes  WITH IIF(llFrstTmBm , .T. , .F.) ,;
                &lcTmpLnBm..ContrctNo WITH IIF(llFrstTmBm , lfOrdNo() , IIF(!EMPTY(lcOrdNo) , lcOrdNo , SPACE(0))) ,;
                &lcTmpLnBm..llprnBlnk WITH IIF(lcStyleBm # LEFT(InvLine.STYLE,lnLenth) AND !EMPTY(lcStyleBm) AND llFrstTmBm , .T. , .F.) ,;
                &lcTmpLnBm..ColorDesc WITH SUBSTR(gfCodDes(SUBSTR(InvLine.STYLE,lnClrPosBm,lnClrLnBm),'COLOR'),1,15)

        REPLACE &lcTmpLnBm..Invoice   WITH InvLine.Invoice        ,;
                &lcTmpLnBm..Order     WITH InvLine.Order          ,;
                &lcTmpLnBm..Account   WITH InvLine.Account        ,;
                &lcTmpLnBm..Store     WITH InvLine.Store          ,;
                &lcTmpLnBm..Style     WITH InvLine.Style          ,;
                &lcTmpLnBm..Scale     WITH InvLine.Scale          ,;
                &lcTmpLnBm..Price     WITH InvLine.Price          ,;
                &lcTmpLnBm..Desc1     WITH lfStGrDscr()           ,;
                &lcTmpLnBm..Qty1      WITH InvLine.&lcQty         ,;
                &lcTmpLnBm..LineNo    WITH InvLine.LineNo         ,;
                &lcTmpLnBm..SizeCode  WITH ALLTRIM(SCALE.CDIM1) + laScaleBm [1,N] ,;
                &lcTmpLnBm..Styupc    WITH laSkuBm[N]             ,;
                &lcTmpLnBm..llprnDes  WITH IIF(llFrstTmBm , .T. , .F.) ,;
                &lcTmpLnBm..ContrctNo WITH IIF(llFrstTmBm , lfOrdNo() , IIF(!EMPTY(lcOrdNo) , lcOrdNo , SPACE(0))) ,;
                &lcTmpLnBm..llprnBlnk WITH IIF(lcStyleBm # LEFT(InvLine.STYLE,lnLenth) AND !EMPTY(lcStyleBm) AND llFrstTmBm , .T. , .F.) ,;
                &lcTmpLnBm..ColorDesc WITH LEFT(STYLE.DESC1,15)
        *C200191,1 [END]

        IF M.ACCOUNT = "AVON1"
          REPLACE &lcTmpLnBm..OrdSec  WITH '0' + ALLTRIM(STR(n)) ,;
                  &lcTmpLnBm..OrdInv  WITH IIF(llFrstTmBm , lfOrdInv() , lc2ndPrtbm)
        ENDIF
        llFrstTmBm = .F.
      ENDIF
    ENDFOR
    lcStyleBm = LEFT(&lcTmpLnBm..STYLE,lnLenth)
  ENDSCAN
  *--AS PER NADER
  REPLACE &lcTmpLnBm..llprncon WITH .T.
ENDSCAN

               *-- Section break the relatoins --*
SELECT InvHdr
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
     *-- End section break the relatoins --*
     
     *-- Section create the relatoins of the program --* 
SELECT (lcTmpLnBm)
IF !('IIF(.NOT.EMPTY(' $ SET('RELATION'))
  SET RELATION TO IIF(!EMPTY(&lcTmpLnBm..ALTSTYLE) , &lcTmpLnBm..ALTSTYLE ,&lcTmpLnBm..Style) INTO STYLE ADDITIVE
ENDIF
IF !('INVOICE' $ SET('RELATION'))
  SET RELATION TO INVOICE INTO INVHDR ADDI
ENDIF

IF !('"S"+SCALE' $ SET('RELATION'))
  SET RELATION TO "S"+SCALE INTO SCALE ADDI
ENDIF

SELECT INVHDR
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
     *-- End section create the relatoins of the program --* 

SELECT (lcTmpLnBm)
LOCATE

DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + "&lcTmpLnBm..Qty1 <> 0 "
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR

*--Break the relation of the temp. file to make a new one if the user select preview many times.
SELECT (lcTmpLnBm)
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO INVHDR
SET RELATION OFF INTO SCALE

*!*************************************************************
*! Name      : lfFirstTim
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 27/03/2001
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
*! Example     : = lfFirstTim()
*!*************************************************************
FUNCTION lfFirstTim
PARAMETER lcReturn

*-- Open the ORDLINE.DBF.
=gfOpenFile(gcDataDir + "ORDLINE" , "ORDLINE" , 'SH')

RETURN ''
*-- End of lfFirstTim.
*!*************************************************************
*! Name      : lfCratTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 27/03/2001
*! Purpose   : Function to Creat the temp. File.
*!*************************************************************
*! Called from : ARPINVBM.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCratTmp()
*!*************************************************************
FUNCTION lfCratTmp
PRIVATE ALIAS

ALIAS = SELECT(0)
*--Creat table for the trasactions.
SELECT InvLine
=AFIELDS(laFileStru)

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+9,4]

*-- Field for the Color Description.
laFileStru[lnFileStru+1,1] = 'ColorDesc'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 15
laFileStru[lnFileStru+1,4] = 0

*-- Field for the Size code from the scale file.
laFileStru[lnFileStru+2,1] = 'SizeCode'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 8
laFileStru[lnFileStru+2,4] = 0

*-- Field to check if print Style Descreption or not.
laFileStru[lnFileStru+3,1] = 'llprnDes'
laFileStru[lnFileStru+3,2] = 'L'
laFileStru[lnFileStru+3,3] = 1
laFileStru[lnFileStru+3,4] = 0

*-- Field to check if print blank line after style or not.
laFileStru[lnFileStru+4,1] = 'llprnBlnk'
laFileStru[lnFileStru+4,2] = 'L'
laFileStru[lnFileStru+4,3] = 1
laFileStru[lnFileStru+4,4] = 0

*-- Field for the first part for the order sequence.
laFileStru[lnFileStru+5,1] = 'OrdSec'
laFileStru[lnFileStru+5,2] = 'C'
laFileStru[lnFileStru+5,3] = 2
laFileStru[lnFileStru+5,4] = 0

*-- Field for the 2nd part for the Invoice sequence.
laFileStru[lnFileStru+6,1] = 'OrdInv'
laFileStru[lnFileStru+6,2] = 'C'
laFileStru[lnFileStru+6,3] = 6
laFileStru[lnFileStru+6,4] = 0

*-- Field for the Style upc.
laFileStru[lnFileStru+7,1] = 'Styupc'
laFileStru[lnFileStru+7,2] = 'C'
laFileStru[lnFileStru+7,3] = 11
laFileStru[lnFileStru+7,4] = 0

*-- Field for the contract no from the ordline file.
laFileStru[lnFileStru+8,1] = 'ContrctNo'
laFileStru[lnFileStru+8,2] = 'C'
laFileStru[lnFileStru+8,3] = 20
laFileStru[lnFileStru+8,4] = 0

*-- Field to check if we print text continue or not.
laFileStru[lnFileStru+9,1] = 'llprncon'
laFileStru[lnFileStru+9,2] = 'L'
laFileStru[lnFileStru+9,3] = 1
laFileStru[lnFileStru+9,4] = 0

CREATE TABLE (gcWorkDir+lcTmpLnBm) FROM ARRAY laFileStru
ZAP
INDEX ON STYLE + INVOICE TAG STYLE OF (gcWorkDir+lcTmpLnBm)
INDEX ON INVOICE + STR(LINENO,6) TAG (lcTmpLnBm) OF (gcWorkDir+lcTmpLnBm)

SELECT(ALIAS)
RETURN ''
*-- End of lfCratTmp.
*!*************************************************************
*! Name        : lfGetBmp
*! Developer   : BASSEM RAFAAT ERNEST (BWA)
*! Date        : 27/03/2001
*! Purpose     : Function to get bmp for the company.
*!*************************************************************
*! Called from : syrepuvr
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetBmp()
*!*************************************************************
FUNCTION lfGetBmp
PARAMETER lcDummy

IF !USED('SYFRMCDD')
  =gfOpenFile(gcSysHome+'SYFRMCDD',gcSysHome+'FORMCDDT','SH')
ELSE
  SELECT SYFRMCDD
ENDIF
=SEEK("ARPINV"+"BM")

RETURN ''
*-- End of lfGetBmp.
*!*************************************************************
*! Name      : lfSolShp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 27/03/2001
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

DECLARE laSoldTo[5,1] , laShipTo[5,1] 

laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address

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

  lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))
  
  laShipTo[1] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF lnCUSRec <> 0 
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
  SELECT(lcAlias)
ENDIF

=lfAdrShift('laShipTo')
lcTermBm = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')

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
*!*************************************************************
*! Name      : lfInvPad
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 27/03/2001
*! Purpose   : Function to return the first 2 line of the notepad.
*!*************************************************************
*! Called from : ARPINVBM.FRX
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfInvPad
PARAMETERS lcDumdy

lcOldInv = INVHDR.INVOICE
STORE SPACE(0) TO lcInvPad1 , lcInvPad2 , lcInvPad3
IF llRpInvNot
  IF SEEK('C' + INVHDR.Invoice , 'NOTEPAD') .AND. ;
         !EMPTY(ALLTRIM(NOTEPAD.mNotes))    .AND. ;
         LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' 

    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 35
    FOR lnContCd = 1 TO 3
      lcNum = ALLTRIM(STR(lncontcd))
      lcInvPad&lcNum = LEFT(MLINE(NOTEPAD.MNOTES,lnContCd),35)
    ENDFOR
    SET MEMOWIDTH TO lnOldMemW
  ENDIF
ENDIF

RETURN ""
*--End of lfInvPad.
*!*************************************************************
*! Name      : lfTempNot
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 27/03/2001
*! Purpose   : Function to return the first 2 lines of the notepad.
*!*************************************************************
*! Called from : ARPINVBM.FRX
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfTempNot
PARAMETERS lcDumdy

IF SEEK('T' + 'INVOICE' , 'NOTEPAD') .AND. ;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes))    .AND. ;
       LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' 

  lnOldMemW = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 80
  FOR lnContCd = 1 TO 3
    lcNum = ALLTRIM(STR(lncontcd))
    lcTmpNot&lcNum = LEFT(MLINE(NOTEPAD.MNOTES,lnContCd),35)
  ENDFOR
  SET MEMOWIDTH TO lnOldMemW
ENDIF

RETURN ""
*--End of lfTempNot.
*!*************************************************************
*! Name      : lfOrdInv
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 27/03/2001
*! Purpose   : Function to return the 2nd part for the invoice sequence.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfOrdInv
PRIVATE lcAlias

STORE 0 TO ln2ndPrtbm , lnLineno 
STORE SPACE(0) TO lc2ndPrtbm , lcOrderBm , lcKeyBm
lcAlias = SELECT(0)

*B604584,1 BWA 06/24/2001 Fix the bug in the Line\Release Field.[START]
*SELECT INVLINE
*lcOrderBm = ORDER()
*lcKeyBm   = EVAL(key())
*SET ORDER TO INVlINEO

*=SEEK(M.ORDER)
*SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE = M.ORDER FOR INVLINE.ACCOUNT == "AVON1"
*  IF INVLINE.STYLE = M.STYLE
*    ln2ndPrtbm = ln2ndPrtbm + 1

*  *C200191,1 BWA 05/09/2001 Changes in the invoice due to the customer.[START]
*  IF INVLINE.Invoice = lcInvoice
*    EXIT
*  ENDIF
*  *C200191,1 [END]

*  ENDIF
*ENDSCAN

*SET ORDER TO lcOrderBm
*=SEEK(lcKeyBm)
*lc2ndPrtbm = "00" + ALLTRIM(STR(ln2ndPrtbm))
*SELECT (lcAlias)

SELECT INVLINE
lcKeyBm   = EVAL(key())
LOCATE
SCAN WHILE INVOICE+STR(LINENO,6) = '' FOR INVLINE.ACCOUNT == "AVON1"
  IF INVLINE.STYLE = M.STYLE
    ln2ndPrtbm = ln2ndPrtbm + 1
  ENDIF
  IF VAL(INVLINE.INVOICE) = VAL(LCINVOICE)
    IF INVLINE.LINENO = lcInvLinNo
      EXIT
    ENDIF
  ENDIF
ENDSCAN

=SEEK(lcKeyBm)
lnCharBm = IIF(LEN(ALLTRIM(STR(ln2ndPrtbm))) < 3 , 3 , LEN(ALLTRIM(STR(ln2ndPrtbm))))
lc2ndPrtbm = PADL(ALLTRIM(STR(ln2ndPrtbm)),lnCharBm,'0')
SELECT (lcAlias)
*B604584,1 BWA 06/24/2001 [END]

RETURN lc2ndPrtbm
*--End of lfOrdInv.
*!*************************************************************
*! Name      : lfGetSkuBm
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 27/03/2001
*! Purpose   : Function to return style SKU.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetSkuBm
PRIVATE lnPrevAl
STORE '' TO laSkuBm
lnPrevAl = SELECT(0)

IF SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  SELECT Spck_Lin
  IF !EMPTY(PACK_ID)
    lnI = 1
    SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
      FOR lnX=1 TO 8
        Z=STR(lnX,1)
        IF QTY&Z > 0

          *C200191,1 BWA 05/09/2001 Changes in the invoice due to the customer.[START]
          *laSkuBm[lnX] = ALLTRIM(Pack_Id)
          laSkuBm[lnX] = ALLTRIM(SUBSTR(Pack_Id,5,11))
          *C200191,1 [END]

          EXIT
        ENDIF
      ENDFOR
      lnI = lnI + 1
    ENDSCAN
  ENDIF
ENDIF
SELECT (lnPrevAl)

RETURN .T.
*--End of lfGetSkuBm.
*!*************************************************************
*! Name      : lfSolShp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 27/03/2001
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
FUNCTION lfOrdNo
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT ORDLINE
IF SEEK("O" + INVLINE.ORDER + STR(INVLINE.LINENO,6) , 'ORDLINE' )
  lcOrdNo = LEFT(MLINE(ORDLINE.NOTE_MEM,1),20)
ENDIF
SELECT(lnAlias)

RETURN lcOrdNo
*--End of lfOrdNo.
*!**************************************************************************
*! Name      : lfClearRep
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 27/03/2001
*! Purpose   : Function that we call when Close the option grid.
*!**************************************************************************
*! Called from : The program
*!**************************************************************************
*! Calls     : None()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
FUNCTION lfClearRep

IF USED('ORDLINE')
  USE IN ORDLINE
ENDIF

IF USED('SYFRMCDD')
  USE IN SYFRMCDD
ENDIF

*--End of lfClearRep.
*!*************************************************************
*! Name      : lfSuppler
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 27/03/2001
*! Purpose   : Function to get the supplier.
*!*************************************************************
*! Called from : ARINVBM.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetcont()
*!*************************************************************
FUNCTION lfSuppler
PARAMETERS lcSuppler
PRIVATE lcAlias , lcRecNo

STORE SPACE(0) TO lcSuppler
lcAlias = SELECT(0)
SELECT CUSTOMER
lcRecNo = EVAL(KEY())

=SEEK('M' + INVHDR.ACCOUNT,'CUSTOMER')
lcSuppler = CUSTOMER.CCUSVEND

=SEEK(lcRecNo)
SELECT (lcAlias)

RETURN lcSuppler
*--End of lfdunsno.

*C200191,1 BWA 05/09/2001 Changes in the invoice due to the customer.[START]
*!*************************************************************
*! Name      : lfStGrDscr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 05/07/2001
*! Purpose   : Function to get the Style group descreption.
*!*************************************************************
*! Called from : ARINVBM.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfStGrDscr()
*!*************************************************************
FUNCTION lfStGrDscr
PRIVATE ALIAS

STORE SPACE(0) TO lcStyGrDs
ALIAS = SELECT(0)
SELECT STYLE
lcStyGrDs = gfCodDes(STYLE.CSTYGROUP,'CSTYGROUP')

SELECT(ALIAS)

RETURN lcStyGrDs
*--End of lfStGrDscr.
*C200191,1 [END]
*!*************************************************************
*! Name      : lfSpcinst
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 05/07/2001
*! Purpose   : Function to get the code wanted.
*!*************************************************************
*! Called from : ARINVBM.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSpcinst()
*!*************************************************************
FUNCTION lfSpcinst
PARAMETERS lcDummy
PRIVATE lcAlias , lcKey

lcAlias = SELECT(0)
SELECT CUSTOMER
lcKey = EVAL(KEY())

IF EMPTY(CUSTOMER.SPCINST)
  =SEEK('M' + Account , 'CUSTOMER')
  lcDummy = ALLTRIM(gfCodDes(CUSTOMER.SPCINST,'SPCINST'))
  =SEEK(lcKey)
ELSE
  lcDummy = ALLTRIM(gfCodDes(CUSTOMER.SPCINST,'SPCINST'))
ENDIF
SELECT(lcAlias)

RETURN lcDummy
*--End of lfSpcinst.