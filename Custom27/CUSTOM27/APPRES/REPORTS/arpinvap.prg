*:***************************************************************************
*: Program file  : ARPINVAP.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR APPAREL RESOURCES INC.
*: Date          : 23/09/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102405
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVAP
*:***************************************************************************
*: Modifications :
*: B804504,1 BWA 10/29/2001 Fix the bug of the data not aligned correctly.[Fix in FRX]
*: B605171,1 SSE 11/28/2001 Fix bug of printing the total merchandise and discount.
*: B605171,1                All modifications is in FRX.
*: B606631,1 RAE 11/12/2002 Fix the bug of not printing all the scales.
*****************************************************************************

           *--Section of Initializing the variables --*
DIMENSION laScaleAp[lcScalCont,12]
*B606631,1 RAE Fix the bug of not printing all the scales. [start]
*DIMENSION laScale[IIF(lcScalCont <= 4 , 5 , lcScalCont),12]
DIMENSION laScale[IIF(lcScalCont <= 4 , 5 , lcScalCont),13]
*B606631,1 RAE [end]
DIMENSION laSkuAp[12]
STORE SPACE(0) TO laScaleAp , laScale , lcScale , laSkuAp
lnCountAp = 0                 &&To check if the scale array is empty with the 5 scale or not.
LnLineNoAp = 0

                *--Section to get the style , color length and postion--*
IF llFrstTmAp
  STORE 0 TO lnClrLnAp , lnClrPosAp , lnStyLnAp , lnStyPosAp
  DECLARE laItemSeg[1]
  =gfItemMask(@laItemSeg)

  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    DO CASE
      CASE laItemSeg[lnCount,1] = 'C'
        lnClrLnAp  = LEN(laItemSeg[lnCount,3])
        lnClrPosAp = laItemSeg[lnCount,4]

      CASE laItemSeg[lnCount,1] = 'F'
        lnStyLnAp  = LEN(laItemSeg[lnCount,3])
        lnStyPosAp = laItemSeg[lnCount,4]
    ENDCASE
  ENDFOR
  llFrstTmAp = .F.
ENDIF
               *--Section to get the style , color length and postion--*

              *--Section to creat Temp. file and collect the data.
=lfCreatTmp()  && Create work cursor.
=lfCollData()  && Collect the data.

SELECT (lclinesAp)
SET RELATION TO
SELECT STYLE
SET RELATION TO

               *-- Section break the relatoins --*
SELECT InvHdr
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO INVLINE
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO Ordhdr
SELECT (lcTmpDbt)
SET RELATION TO

SELECT INVLINE
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT STYLE
SET RELATION OFF INTO SCALE
             *-- End Section break the relatoins --*
SELECT INVHDR
IF llPrntInst .OR. llRpInvNot
  SET RELATION TO '' INTO (lcTmpDbt)
  SELECT (lcTmpDbt)
  SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.INVOICE , '*') INTO &lclinesAp ADDITIVE
ELSE
  SET RELATION TO INVHDR.INVOICE INTO &lclinesAp ADDITIVE
ENDIF

SELECT (lclinesAp)
LOCATE

SET RELATION TO EVAL(lclinesAp+'.STYLE') INTO STYLE ADDITIVE
SET RELATION TO "S" + EVAL(lclinesAp+'.Account') + EVAL(lclinesAp+'.STYLE') INTO SPCK_LIN ADDITIVE

SELECT STYLE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

SELECT INVHDR
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
          'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + INVHDR.ORDER INTO ORDHDR ADDITIVE
SET RELATION TO 'C' + INVHDR.INVOICE INTO NOTEPAD ADDITIVE

IF llPrntInst .OR. llRpInvNot
  SET SKIP TO (lcTmpDbt) , (lclinesAp)
ELSE
  SET SKIP TO (lclinesAp)
ENDIF

SELECT INVHDR
DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 23/09/2001
*! Purpose   : Function to collect the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ARPINVAP.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************
FUNCTION lfCollData
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SET SKIP TO
lcScanExp = IIF(UPPER(lcRpExp) == ".T.", "", "FOR " + lcRpExp)
SET DEVICE TO PRINT
STORE SPACE(0) TO lcStyle , lcColor

SCAN &lcScanExp
  WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
  lcInvoice = INVHDR.invoice
  LnLineNoAp = 0

  =SEEK(lcInvoice)
  SELECT INVLINE
  SCAN REST WHILE INVOICE + STR(LINENO,6) = lcInvoice
    lcStyle = IIF(!EMPTY(INVLINE.ALTSTYLE) , SUBSTR(INVLINE.ALTSTYLE,lnStyPosAp,lnStyLnAp) , SUBSTR(INVLINE.STYLE,lnStyPosAp,lnStyLnAp))
    lcColor = IIF(!EMPTY(INVLINE.ALTCOLOR) , SUBSTR(INVLINE.ALTCOLOR,lnClrPosAp,lnClrLnAp) , SUBSTR(INVLINE.STYLE,lnClrPosAp,lnClrLnAp))

    IF !(LEFT(INVLINE.Scale,1) $ lcScale)
      lcScale = lcScale + LEFT(INVLINE.Scale,1)
      =lfScalPrnt()
    ENDIF

    SELECT (lclinesAp)
*    IF SEEK(lcInvoice + LEFT(INVLINE.Scale,1) + lcStyle , lclinesAp ) AND IIF( &lclinesAp..COLOR = lcColor , .T. , .F.)
    IF SEEK(lcInvoice + LEFT(INVLINE.Scale,1) + lcStyle + lcColor, lclinesAp )

      =lfGetQty()
      REPLACE &lclinesAp..Note_mem WITH CHR(13)+CHR(10) + INVLINE.Note_mem ADDITIVE
    ELSE
      LnLineNoAp = LnLineNoAp + 1
      APPEND BLANK
      REPLACE &lclinesAp..Invoice  WITH lcInvoice             ,;
              &lclinesAp..Account  WITH INVHDR.ACCOUNT        ,;
              &lclinesAp..Style    WITH lcStyle               ,;
              &lclinesAp..Color    WITH lcColor               ,;
              &lclinesAp..Scale    WITH LEFT(INVLINE.Scale,1) ,;
              &lclinesAp..Price    WITH INVLINE.Price         ,;
              &lclinesAp..Order    WITH INVLINE.Order         ,;
              &lclinesAp..Store    WITH INVHDR.STORE          ,;
              &lclinesAp..Desc     WITH LEFT(Style.Desc1,40)  ,;
              &lclinesAp..Note_mem WITH INVLINE.Note_mem      ,;
              &lclinesAp..LineNo   WITH LnLineNoAp            ,;
              &lclinesAp..StyClr   WITH lcStyle + "-" + lcColor

      =lfGetQty()
     ENDIF
   ENDSCAN
ENDSCAN

SELECT(lcAlias)

*--End of lfCollData.
*!*************************************************************
*! Name      : lfScalPrnt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 23/09/2001
*! Purpose   : Function to collect the scale data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : Report code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfScalPrnt()
*!*************************************************************
FUNCTION lfScalPrnt
PRIVATE lcAlias , lcKeyScl

lcAlias = ALIAS()
SELECT SCALE
lcKeyScl = EVAL(KEY())

IF SEEK( "S" + LEFT(INVLINE.Scale,1) , 'SCALE')
  *B606631,1 RAE Fix the bug of not printing all the scales. [start]
  *IF ASCAN(laScaleAp , LEFT(INVLINE.Scale,1)) = 0
  IF ASCAN(laScaleAp , LEFT(INVLINE.Scale,1)+ "##") = 0
  *B606631,1 RAE [end]
    lnCountAp = lnCountAp + 1
    laScaleAp[lnCountAp,1] = LEFT(INVLINE.Scale,1) + "##"
    SCAN REST WHILE TYPE + SCALE + PREPAK = "S" + LEFT(INVLINE.Scale,1)
      FOR lnLoopAp = 1 TO 8
        lcSzT  = 'SZ' + ALLTRIM(STR(lnLoopAp))
        IF !EMPTY(SCALE.&lcSzT)
          FOR lnEmpty = 1 To 12
            IF EMPTY(laScaleAp[lnCountAp,lnEmpty])
              laScaleAp[lnCountAp , lnEmpty] = SCALE.&lcSzT
              EXIT
            ENDIF
          ENDFOR
        ENDIF
        IF EMPTY(SCALE.&lcSzT)
          EXIT
        ENDIF
      ENDFOR
    ENDSCAN
  ENDIF
ENDIF

=SEEK(lcKeyScl,'SCALE')
SELECT(lcAlias)

*--End of lfScalPrnt.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 23/09/2001
*! Purpose   : Create Temp. file that hold the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : Report code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp
PRIVATE lcAlias

lcAlias = ALIAS()

CREATE CURSOR (lclinesAP) (Invoice C(6) , Account C(5) , Style C(lnStyLnAp) , Color C(lnClrLnAp)  ,;
                           Scale C(3)   , Qty1 N(6)    , Qty2 N(6)          , Qty3 N(6)  ,;
                           Qty4 N(6)    , Qty5 N(6)    , Qty6 N(6)          , Qty7 N(6)  ,;
                           Qty8 N(6)    , Qty9 N(6)    , Qty10 N(6)         , Qty11 N(6) ,;
                           Totqty N(7)  , Price N(9,2) , Order C(6)         , Store C(8) ,;
                           Desc C(40)   , Note_mem M   , LineNo N(6,0)      , StyClr C(lnClrLnAp+lnStyLnAp+1))

INDEX ON Invoice + LEFT(Scale,1) + Style + Color TAG Invoice of (gcWorkDir + lclinesAp)

SELECT(lcAlias)

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfGetQty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 23/09/2001
*! Purpose   : Fill the Quantity fields in the temp. file.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : Report code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfGetQty()
*!*************************************************************
FUNCTION lfGetQty
PRIVATE lcAlias

lcSeekScl = LEFT(INVLINE.Scale,1) + "##"
lnSclPos =  ASCAN(laScaleAp , lcSeekScl)
IF lnSclPos > 0
  lnItmPos = ASUBSCRIPT(laScaleAp , lnSclPos , 1)
ENDIF

*--Update the Quntity fields.
FOR lnQty = 1 TO SCALE.CNT
  lcSzI  = 'SZ' + ALLTRIM(STR(lnQty))
  lnInvQty = 'QTY' + ALLTRIM(STR(lnQty))
  IF INVLINE.&lnInvQty > 0
    FOR lnArry = 2 TO ALEN(laScaleAp,2)
      IF laScaleAp[lnItmPos , lnArry ] = SCALE.&lcSzI
        lnArry = lnArry - 1
        lnSzT  = 'QTY' + ALLTRIM(STR(lnArry))
        REPLACE &lclinesAp..&lnSzT WITH &lclinesAp..&lnSzT + INVLINE.&lnInvQty
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDFOR

*--Update the TotQty field.
  REPLACE &lclinesAp..TOTQTY WITH &lclinesAp..TOTQTY + INVLINE.TOTQTY

*--End of lfGetQty.
*!*************************************************************
*! Name      : lfSolShp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 23/09/2001
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
PRIVATE lcAlias , lcInvHdr

STORE '' TO lcShpVAp , lcTermAp , lcShpTName

lcInvHdr = SELECT(0)
SELECT INVHDR

DECLARE laSoldTo[5] , laShipTo[5]
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
laFactor = ''           && Array to hold the Factor information.

IF !EMPTY(INVHDR.CFACCODE)
  =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

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

  IF BETWEEN(lnCUSRec , 1 , RECCOUNT('CUSTOMER'))
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
  SELECT(lcAlias)
ENDIF

=lfAdrShift('laShipTo')
lcShpVAp = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
lcTermAp = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')

SELECT(lcInvHdr)
RETURN ''

*--End of lfSolShp.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 23/09/2001
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address.
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
*! Name      : lfSclSzeAp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 23/09/2001
*! Purpose   : To get the scale and the size.
*!*************************************************************
*! Called from : ARPINVAP.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSclSzeAp()
*!*************************************************************
FUNCTION lfSclSzeAp
PARAMETER lcReturn
PRIVATE lcAlias , lcKey , lnSclCnt , lcKeyScl , lnContAp

lcReturn = .T.
STORE SPACE(0) TO laScale
STORE 0 TO lnSclCnt , lnContAp

lcOldInv = INVHDR.INVOICE
lcAlias  = ALIAS()
SELECT INVLINE
lcKey = INVOICE + STR(LINENO,6)
=SEEK(INVHDR.INVOICE)

SCAN REST WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE  .AND. lnSclCnt < lcScalCont + 1
  SELECT SCALE
  lcKeyScl = EVAL(KEY())

  IF SEEK( "S" + LEFT(INVLINE.Scale,1) , 'SCALE')
    *B606631,1 RAE Fix the bug of not printing all the scales. [start]
    *IF ASCAN(laScale , LEFT(INVLINE.Scale,1)) = 0
    IF ASCAN(laScale , LEFT(INVLINE.Scale,1)+ "##") = 0
    *B606631,1 RAE [end]
      lnContAp = lnContAp + 1
      laScale[lnContAp,1] = LEFT(INVLINE.Scale,1)
      SCAN REST WHILE TYPE + SCALE + PREPAK = "S" + LEFT(INVLINE.Scale,1)
        FOR lnLoopAp = 1 TO 8
          lcSzT  = 'SZ' + ALLTRIM(STR(lnLoopAp))
          IF !EMPTY(SCALE.&lcSzT)
            FOR lnEmpty = 1 To 12
              IF EMPTY(laScale[lnContAp,lnEmpty])
                laScale[lnContAp , lnEmpty] = SCALE.&lcSzT
                EXIT
              ENDIF
            ENDFOR            
          ENDIF
          IF EMPTY(SCALE.&lcSzT)
            EXIT
          ENDIF
        ENDFOR
        *B606631,1 RAE Fix the bug of not printing all the scales. [start]
        laScale[lnContAp,13] = laScale[lnContAp,1] + "##"
        *B606631,1 RAE [end]
      ENDSCAN
    ENDIF
  ENDIF
  =SEEK(lcKeyScl,'SCALE')
ENDSCAN

=SEEK(lcKey,'INVLINE')
SELECT (lcAlias)
RETURN ''

*--End of lfSclSzeAp.
*!*************************************************************
*! Name      : lfPrtLNotes
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 23/09/2001
*! Purpose   : Function to Evaluate Line Notes Only To be Printed.
*!*************************************************************
*! Called from : ARPINVAP.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrtLNotes()
*!*************************************************************
FUNCTION lfPrtLNotes
PARAMETER lcReturn

lcReturn = .T.
DO CASE
  CASE llRpInvLNt .AND. !EMPTY(EVAL(lclinesAp+'.note_mem')) .AND. LEFT(ALLTRIM(STRTRAN(EVAL(lclinesAp+'.note_mem'),CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(EVAL(lclinesAp+'.note_mem'))

  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*--End of lfPrtLNotes.
*!*************************************************************
*! Name      : lpPrtSku
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 23/09/2001
*! Purpose   : Print the style/color Skus for a specific account.
*!*************************************************************
*! Called from : ARPINVAP.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lpPrtSku()
*!*************************************************************
FUNCTION lfPrtSku
PARAMETER lcReturn
PRIVATE lnPrevAl

lcReturn = .T.
STORE ' ' TO lcStrToPrn , laSkuAp

IF !SEEK('S' + EVAL(lclinesAp+'.Account') + EVAL(lclinesAp+'.StyClr'),'Spck_Lin')
  llPrtSku = .F.
  RETURN .F.
ENDIF

lnPrevAl = SELECT (0)
SELECT Spck_Lin
IF !EMPTY(PACK_ID)
  lnI = 1
  lcSkuTmpl = IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1 + SkuTmpl.Len2 + SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8
    lnDime2 = 8
  ENDIF

   SCAN WHILE Type + Account + Style = 'S' + EVAL(lclinesAp+'.Account') + EVAL(lclinesAp+'.StyClr') .AND. lnI < 12
    FOR lnX = 1 TO 8
      Z = STR(lnX,1)
      IF QTY&Z > 0
        laSkuAp[lnI] = SUBSTR(Pack_Id , lnDime1+1 , lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN
  lnI = 1
  =SEEK('S' + EVAL(lclinesAp+'.Account') + EVAL(lclinesAp+'.StyClr') , 'Spck_Lin')
  DO WHILE Type + Account + Style = 'S' + EVAL(lclinesAp+'.Account') + EVAL(lclinesAp+'.StyClr') .AND. lnI < 12
    lcStrToPrn = 'SKU # ' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = 'S' +  EVAL(lclinesAp+'.Account') + EVAL(lclinesAp+'.StyClr') .AND. !EOF()
      lcI = STR(lnI,1)
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
  ENDDO  
ENDIF
SELECT (lnPrevAl)
IF EMPTY(lcStrToPrn)
  STORE '' TO laSkuAp
  llPrtSku = .F.
  RETURN .F.
ELSE
  llPrtSku = .T.
  RETURN .T.
ENDIF

RETURN ""

*--End of lpPrtSku.