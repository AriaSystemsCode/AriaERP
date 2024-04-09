*:***************************************************************************
*: Program file  : ARPINVAR.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR APPAREL RESOURCES INC.
*: Date          : 12/17/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102785
*: N.P >> I used all the records in the Syrepuvr for the old invoice in this one.
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVAR
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*:B606849,1 BWA 01/08/2003 Fix the bug of Variable "LNITMPOS" not found.
*:B606849,2 RAE 01/08/2003 Print the color Descreption instead of the color code [IN the Frx]
*:C200463,1 RAE 01/13/2003 Add new Sort (Invoice / Account) in the Option Grid.
*:B606974,1 KHM 04/22/2003 Fix the bug of not marking the invoice as printed in 
*:B606974,1                case of sorting by account.
*:C121115,1 MHM 01/15/2004 customize the invoice to print the UPS tracking number
*:********************************************************************************************

*--Know the count of the Extended scale in the scale file.
lcScalCont = 0          &&  Scale Counter
lcScaleExp = ""         &&  Scale Expression
lcScalfl = SELECT(0)
SELECT SCALE
SCAN FOR Type + Scale + Prepak = 'S' AND lcScaleExp # LEFT(SCALE.SCALE,1)
  lcScalCont = lcScalCont + 1
  lcScaleExp = LEFT(SCALE.SCALE,1)
ENDSCAN
SELECT(lcScalfl)

           *--Section of Initializing the variables --*
DIMENSION laScaleAp[lcScalCont,12]
DIMENSION laScale[IIF(lcScalCont <= 4 , 5 , lcScalCont),12]
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
=lfFillSize()  && Fill the temp. file with scale and it's size.

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

*C121115,1 MHM 01/15/2004 Open UPS File[Start]
IF !USED('ARUPSSHP')
  =gfOpenFile(gcDataDir+'ARUPSSHP',gcDataDir+'ARUPSSHP','SH')
ENDIF
*C121115,1 MHM 01/15/2004 Open UPS File[End]

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

*B606974,1 KHM 04/22/2003 (Begin) Saving the order of invhdr file.
lcOldOrd = ORDER()
*B606974,1 KHM 04/22/2003 (End)

*C200463,1 RAE Add new Sort (Invoice / Account) in the Option Grid. [start]
IF lcRpSortBy = 'A'
  *-- If the sort is by Customer.
  SET ORDER TO TAG Invhdra
ENDIF
*C200463,1 RAE [end]

DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR
*B606974,1 KHM 04/22/2003 (Begin) Restoring the old order for invhdr file
SET ORDER TO &lcOldOrd IN InvHdr
*B606974,1 KHM 04/22/2003 (End)

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/17/2002
*! Purpose   : Function to collect the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ARPINVAR.PRG
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
              &lclinesAp..StyClr   WITH INVLINE.STYLE

      =lfGetQty()
     ENDIF
   ENDSCAN
ENDSCAN

SELECT(lcAlias)

*--End of lfCollData.
*!*************************************************************
*! Name      : lfFillSize
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/17/2002
*! Purpose   : Function to fill the temp. file with scale and it's size.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ARPINVAR.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfFillSize()
*!*************************************************************
FUNCTION lfFillSize

SELECT (lclinesAp)
SCAN
  lnPosScl = ASUBSCRIPT(laScaleAp,ASCAN(laScaleAp,LEFT(&lclinesAp..Scale,1) + "##"),1)
  IF lnPosScl > 0
    REPLACE &lclinesAp..Ssz1  WITH LEFT(laScaleAp[lnPosScl,1],1) ,;
            &lclinesAp..Ssz2  WITH laScaleAp[lnPosScl,2]         ,;
            &lclinesAp..Ssz3  WITH laScaleAp[lnPosScl,3]         ,;
            &lclinesAp..Ssz4  WITH laScaleAp[lnPosScl,4]         ,;
            &lclinesAp..Ssz5  WITH laScaleAp[lnPosScl,5]         ,;
            &lclinesAp..Ssz6  WITH laScaleAp[lnPosScl,6]         ,;
            &lclinesAp..Ssz7  WITH laScaleAp[lnPosScl,7]         ,;
            &lclinesAp..Ssz8  WITH laScaleAp[lnPosScl,8]         ,;
            &lclinesAp..Ssz9  WITH laScaleAp[lnPosScl,9]         ,;
            &lclinesAp..Ssz10 WITH laScaleAp[lnPosScl,10]        ,;
            &lclinesAp..Ssz11 WITH laScaleAp[lnPosScl,11]        ,;
            &lclinesAp..Ssz12 WITH laScaleAp[lnPosScl,12]
  ENDIF
ENDSCAN

*--End of lfFillSize
*!*************************************************************
*! Name      : lfScalPrnt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/17/2002
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
*B606849,1 BWA (START)
  *IF ASCAN(laScaleAp , LEFT(INVLINE.Scale,1)) = 0
  IF ASCAN(laScaleAp , LEFT(INVLINE.Scale,1) + "##" ) = 0
*B606849,1 (END)
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
*! Date      : 12/17/2002
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
                           Scale C(3)   , Qty1 N(6)    , Qty2 N(6)          , Qty3 N(6)   ,;
                           Qty4 N(6)    , Qty5 N(6)    , Qty6 N(6)          , Qty7 N(6)   ,;
                           Qty8 N(6)    , Qty9 N(6)    , Qty10 N(6)         , Qty11 N(6)  ,;
                           Ssz1 C(5)    , Ssz2 C(5)    , Ssz3 C(5)          , Ssz4 C(5)   ,;
                           Ssz5 C(5)    , Ssz6 C(5)    , Ssz7 C(5)          , Ssz8 C(5)   ,;
                           Ssz9 C(5)    , Ssz10 C(5)   , Ssz11 C(5)         , Ssz12 C(5)  ,;
                           Totqty N(7)  , Price N(9,2) , Order C(6)         , Store C(8)  ,;
                           Desc C(40)   , Note_mem M   , LineNo N(6,0)      , StyClr C(lnClrLnAp+lnStyLnAp+1))

INDEX ON Invoice + LEFT(Scale,1) + Style + Color TAG Invoice of (gcWorkDir + lclinesAp)

SELECT(lcAlias)

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfGetQty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/17/2002
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
*! Name      : lfSclSzeAp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/17/2002
*! Purpose   : To get the scale and the size.
*!*************************************************************
*! Called from : ARPINVAR.FRX
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
    IF ASCAN(laScale , LEFT(INVLINE.Scale,1)) = 0
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
*! Name      : lpPrtSku
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/17/2002
*! Purpose   : Print the style/color Skus for a specific account.
*!*************************************************************
*! Called from : ARPINVAR.FRX
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
*!*************************************************************
*! Name      : lfGetUPS
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/15/2004
*! Purpose   : Get UPS Traking No.
*!*************************************************************
*! Called from : ARPINVAR.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetUPS()
*!*************************************************************
*C121115,1 MHM 01/15/2004 Seek to correct UPS Tracking No
FUNCTION lfGetUPS
PARAMETER lcReturn

=SEEK(INVHDR.piktkt, 'ARUPSSHP')
lcReturn = .T.
