*:***************************************************************************
*: Program file  : ARPINVEC
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR EAST COAST
*! Date          : 01/18/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : WAB - WALID A. WAHAB
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVEC
*:***************************************************************************
*: This Report Program is due to C#101738 ...
*:***************************************************************************
*: Last modified :
*B603442,1 WAB 02/09/2000 fix the bug of alias not found after return to standred prog
*B803040,1 SSH 02/14/2000 Shift the page 7 lines up ,print the sku and alignment
*B803040,1 SSH 02/14/2000 the top right box.
*B803070,1 BWA 03/01/2000 Fix the bug of that the invoice not printing correctly in the Okidata 391 printer ,
*B803070,1                and resize the sku,size fields
*B803257,1 BWA 05/11/2000 1) Modify the notepad of the invocie header .
*B803257,1                2) Modify the Pack_id to be print before the Sku. in the invoice.
*:***************************************************************************

*B603442,1 WAB - let the variable lcInvoice private caue this variable is using 
*B603442,1 WAB - in standered prog for holding the temp file name.
*PRIVATE lnAlias
PRIVATE lnAlias,lcInvoice 
*B603442,1 WAB - END

lnAlias = SELECT()
*--call function to create temp files
*B803040,1 SSH 02/14/2000 Print the sku.
lcPack_Id = ''
*B803040,1 SSH (End)

*B803257,1 Print the Pack_id [START]
lcPack_Inv = ''
*B803257,1 [END]

=lfCreatTmp()
*--here we convert every line in invoice line to more than one line depends on how many
*-- size had enterd in invoice line. and accumulate by size
*-- and get the size headr for each size and olso the sku for each size

SELECT INVHDR
SET SKIP TO
SCAN FOR &lcRpExp
  SELECT INVLINE
  SCAN REST WHILE Invoice = INVHDR.Invoice
    *--call function to get SKU for each style
    =lfGetSku()
    SELECT (lcInvQtTmp)
    FOR lnCount = 1 TO 8 
      lcCount = alltrim(str(lnCount))
      IF invline.qty&lcCount <> 0
        lcStyle = IIF(!EMPTY(INVLINE.ALTSTYLE),INVLINE.ALTSTYLE,INVLINE.Style)
        IF !seek(INVLINE.INVOICE+SUBSTR(lcStyle,1,LEN(lcStyle)-3)+STR(INVLINE.PRICE)+SCALE.SZ&lcCount)
          APPEND BLANK
          REPLACE INVOICE WITH INVLINE.INVOICE ;
                  STYLE   WITH lcStyle         ;
                  DESC1   WITH INVLINE.DESC1   ;
                  PRICE   WITH INVLINE.PRICE   ;
                  cSize1  WITH SCALE.SZ&lcCount;
                  cSku1   WITH laSku[lnCount]
          *B803040,1 SSH 02/14/2000 Print the sku.
          
          *B803257,1 Print the Pack_id [START]
          *REPLACE PackId WITH lcPack_Id
          REPLACE PackId WITH lcPack_Id ;
                  cPackIdInv WITH lcPack_Inv
          *B803257,1 [END]
                    
          *B803040,1 SSH (End)
        ENDIF
        REPLACE qty1 WITH qty1 + invline.qty&lcCount
      ENDIF
    ENDFOR 
  ENDSCAN
  
   *B803257,1 Update the Temp. File with the invoice#.[START]
   SELECT (lcInvQtTmp)
   REPLACE CINVOICE WITH INVHDR.Invoice
   *B803257,1 [END]
   
ENDSCAN
*--colect every 7 size in one line grouped by style + price
SELECT (lcInvQtTmp)
lcInvoice = ' '
*B803257,1 Print the Pack_id and the notepad[START]
lcinvNote = ''
*B803257,1 [END]
lnLineNo = 0
SCAN
  lnlineNo = IIF(lcInvoice = Invoice,lnLineNo,1)
  lcInvoice  = Invoice
  lcStylColr = SUBSTR(STYLE,1,LEN(style)-3)
  lcPrice    = STR(PRICE)
  lnCount = 8
  lnTotQty = 0
  SCAN REST WHILE INVOICE+SUBSTR(STYLE,1,LEN(STYLE)-3)+STR(PRICE) = ;
                  lcInvoice+lcStylColr+lcPrice
                  
    *B803257,1 Print the Pack_id and the notepad[START]                  
    lcinvNote = IIF(EMPTY(lcinvNote),&lcInvQtTmp..CINVOICE,lcinvNote)
    *B803257,1 [END]
    
    SELECT (lcInvLnTmp)
    
    *B803070,1 Decreasing the variable lnCount to get 5 sizes not 7 sizes in the line of the invoice [START]
    *IF lnCount > 7
    IF lnCount > 5
    *B803070,1 BWA 03/01/2000 [END]
    
       lnCount = 1 
       APPEND BLANK
       REPLACE INVOICE WITH &lcInvQtTmp..INVOICE ;
               STYLE   WITH &lcInvQtTmp..STYLE   ;
               DESC1   WITH &lcInvQtTmp..DESC1   ;
               PRICE   WITH &lcInvQtTmp..PRICE   ;
               LineNo  WITH lnLineNo
    ENDIF
    lcCount = alltrim(str(lnCount))
    REPLACE Qty&lcCount   WITH &lcInvQtTmp..qty1   ;
            cSize&lcCount WITH &lcInvQtTmp..csize1 ;
            cSku&lcCount  WITH &lcInvQtTmp..cSku1
    *B803040,1 SSH 02/14/2000 Print the sku.
    
    *B803257,1 Print the Pack_id and the notepad[START]
    *REPLACE PackId WITH &lcInvQtTmp..PackId
    REPLACE PackId WITH &lcInvQtTmp..PackId ;
            cPackIdInv WITH &lcInvQtTmp..cPackIdInv
    *B803257,1 [END]
    
    *B803040,1 SSH (End)
    lnCount = lnCount + 1
    lnTotQty = lnTotQty + Qty&lcCount   
  ENDSCAN
  SKIP -1
  *-- put the total in first record in the groupe
  SELECT (lcInvLnTmp)
  
  *B803257,1 Print the Pack_id and the notepad[START]
  REPLACE CINVOICE   WITH lcinvNote
  lcinvNote = ""
  *B803257,1 [END]
  
  GO TOP
  =SEEK(lcInvoice+lcStylColr+lcPrice)
  REPLACE TOTQTY WITH lnTotQty
  lnLineNo = lnLineNo + 1
ENDSCAN  

*--put relation between the invhdr and the temp file
SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO (lcInvLnTmp) ADDITIVE
SET SKIP TO (lcInvLnTmp)

*B803257,1 BWA 05/11/2000 1) Modify the notepad of the invocie header .[START]
SET RELATION TO "C"+ INVHDR.invoice INTO NotePad ADDITIVE
*B803257,1 [END]


RETURN

*!**************************************************************************
*! Func. Name: lfCreatTmp
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/19/2000
*! Purpose   : create the temp. files 
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfCreatTmp()
*!**************************************************************************
FUNCTION lfCreatTmp
*-- check if the files created before
IF !llCreatFile
  SELECT INVLINE
  DIMENSION laFileStru[1,4]
    
  *B803040,1 SSH (End)
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  *B803040,1 SSH 02/14/2000 Print the sku.
  *DIMENSION laFileStru[lnFileStru+2,4]
  
  *B803257,1 ReDimension the array to Print the Pack_id and the notepad [START]
  *DIMENSION laFileStru[lnFileStru+3,4]
  DIMENSION laFileStru[lnFileStru+5,4]
  *B803257,1 [END]
  
  *B803040,1 SSH (End)
  laFileStru[lnFileStru+1,1] = 'cSize1'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 10
  laFileStru[lnFileStru+1,4] = 0
  
  *B803070,1 Change the width of the field from 10ch to 13ch [START]
  *laFileStru[lnFileStru+2,1] = 'cSku1'
  *laFileStru[lnFileStru+2,2] = 'C'
  *laFileStru[lnFileStru+2,3] = 10
  *laFileStru[lnFileStru+2,4] = 0

  laFileStru[lnFileStru+2,1] = 'cSku1'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 13
  laFileStru[lnFileStru+2,4] = 0
  *B803070,1 BWA 03/01/2000 [END]
  
  *B803040,1 SSH 02/14/2000 Print the sku.
  laFileStru[lnFileStru+3,1] = 'PackId'
  laFileStru[lnFileStru+3,2] = 'C'
  laFileStru[lnFileStru+3,3] = 10
  laFileStru[lnFileStru+3,4] = 0
  *B803040,1 SSH (End)
  
  *B803257,1 Modify new field to Print the Pack_id and the notepad.[START] 
  laFileStru[lnFileStru+4,1] = 'cPackIdInv'
  laFileStru[lnFileStru+4,2] = 'C'
  laFileStru[lnFileStru+4,3] = 16
  laFileStru[lnFileStru+4,4] = 0
  
  laFileStru[lnFileStru+5,1] = 'CInvoice'
  laFileStru[lnFileStru+5,2] = 'C'
  laFileStru[lnFileStru+5,3] = 6
  laFileStru[lnFileStru+5,4] = 0
  *B803257,1 [END]

  CREATE TABLE (gcWorkDir+lcInvQtTmp) FROM ARRAY laFileStru
  INDEX ON invoice+SUBSTR(STYLE,1,LEN(style)-3)+str(price)+cSize1 TAG (lcInvQtTmp)

  lnFileStru = ALEN(laFileStru,1)
  
  *B803070,1 Decreasing the fields of the size and the sku 2 fields [START]
  *DIMENSION laFileStru[lnFileStru+12,4]
  *FOR lnCount = 1 TO 6
  * laFileStru[lnFileStru+lnCount,1] = 'cSize'+STR(lnCount+1,1)
  * laFileStru[lnFileStru+lnCount,2] = 'C'
  * laFileStru[lnFileStru+lnCount,3] = 10
  * laFileStru[lnFileStru+lnCount,4] = 0
  *ENDFOR
  
  *FOR lnCount = 7 TO 12
   *laFileStru[lnFileStru+lnCount,1] = 'cSku'+STR(lnCount-5,1)  
   *laFileStru[lnFileStru+lnCount,2] = 'C'
   *laFileStru[lnFileStru+lnCount,3] = 10
   *laFileStru[lnFileStru+lnCount,4] = 0
  *ENDFOR  
  
  DIMENSION laFileStru[lnFileStru+8,4]
  FOR lnCount = 1 TO 4
   laFileStru[lnFileStru+lnCount,1] = 'cSize'+STR(lnCount+1,1)
   laFileStru[lnFileStru+lnCount,2] = 'C'
   laFileStru[lnFileStru+lnCount,3] = 10
   laFileStru[lnFileStru+lnCount,4] = 0
  ENDFOR
  
  FOR lnCount = 5 TO 8
   laFileStru[lnFileStru+lnCount,1] = 'cSku'+STR(lnCount-3,1)
   laFileStru[lnFileStru+lnCount,2] = 'C'
   laFileStru[lnFileStru+lnCount,3] = 13
   laFileStru[lnFileStru+lnCount,4] = 0
  ENDFOR  
  *B803070,1 BWA 03/01/2000 [END]

  CREATE TABLE (gcWorkDir+lcInvLnTmp) FROM ARRAY laFileStru
  INDEX ON invoice+SUBSTR(STYLE,1,LEN(style)-3)+str(price) TAG (lcInvLnTmp)
  llCreatFile = .T.
ELSE
  SELECT (lcInvQtTmp)
  ZAP
  SELECT (lcInvLnTmp)
  ZAP
ENDIF
*!**************************************************************************
*! Func. Name: lfGetSku
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/19/2000
*! Purpose   : get the style/color Skus for a specific account.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfGetSku()
*!**************************************************************************
FUNCTION lfGetSku 
PRIVATE lnPrevAl,lnSkuNo,lcSkuNo,lnArrElmnt,lcSize

STORE ' ' TO lcStrToPrn
lcStyle = IIF(!EMPTY(INVLINE.ALTSTYLE),INVLINE.ALTSTYLE,INVLINE.Style)

*B803257,1 Check for the Pack_id [START]
*IF !SEEK('S'+InvLine.Account+lcStyle,'Spck_Lin')
IF !SEEK('S'+InvLine.Account+lcStyle,'Spck_Lin') AND !SEEK('P'+InvLine.Account+lcStyle,'Spck_Lin')
*B803257,1 [END]

  *B803040,1 SSH 02/14/2000 Print the sku.
  FOR lnArrElmnt=1 TO 8
    lcSize=STR(lnArrElmnt,1)
    laSku[lnArrElmnt]=''
  ENDFOR
  lcPack_Id = ''
  *B803040,1 SSH (End)
  RETURN 
ENDIF

*B803257,1 Fill the variable with the Pack_id in the invline file to replace this value in the new field [START]
lcPack_Inv = InvLine.Pack_id
IF EMPTY(lcPack_Inv)        && If there is no Pack_id in the invline.
  IF SEEK('P'+InvLine.Account+lcStyle,'Spck_Lin')
    lcPack_Inv = Spck_Lin.Pack_Id
  ENDIF
ENDIF
*B803257,1 [END]

lnPrevAl = SELECT()
SELECT Spck_Lin
*B803257,1 Fill the variable with the Pack_id in the invline file to replace this value in the new field [START]
*IF !EMPTY(PACK_ID)
IF SEEK('S'+InvLine.Account+lcStyle,'Spck_Lin')
*B803257,1 (End)

  lnSkuNo = 1
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    *B803040,1 SSH 02/14/2000 Print the sku.
    *lnDime1  = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    llPakHdr = gfOpenFile(gcDataDir+'Spck_Hdr' ,'Sku_style','SH')
    SELECT Spck_Hdr
    
    IF SEEK('S'+InvLine.Account+SUBSTR(lcStyle,1,LEN(lcStyle)-4))
      *Ren
      *lcPack_Id = SUBSTR(Spck_Hdr.Pack_Id,1,4)
      lcPack_Id = Spck_Hdr.Pack_Id
      *Ren end
      lnDime1   = LEN(ALLTRIM(Spck_Hdr.Pack_Id))
    ELSE
      lnDime1   = 8
      lcPack_Id = ''
    ENDIF
    USE IN IIF(llPakHdr,'Spck_Hdr',0)
    SELECT Spck_Lin
    *B803040,1 SSH (End)
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF
  SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+lcStyle .AND. lnSkuNo < 9
    FOR lnArrElmnt=1 TO 8
      lcSize=STR(lnArrElmnt,1)
      IF QTY&lcSize > 0
        *Ren
        *laSku[lnArrElmnt]=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        laSku[lnArrElmnt]=ALLTRIM(Pack_Id)
        *Ren end
        EXIT
      ENDIF
    ENDFOR
    lnSkuNo = lnSkuNo + 1
  ENDSCAN
ENDIF
SELECT (lnPrevAl)
RETURN 
*!**************************************************************************
*! Func. Name: lfGetTotal
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/19/2000
*! Purpose   : print the total for each invoice during the running of the frx .
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfGetTotal()
*!**************************************************************************
FUNCTION lfGetTotal
PARAMETERS lcDumdy
lnTotAmnt = lnTotAm
RETURN
*!**************************************************************************
*! Func. Name: lfGetStyle
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/19/2000
*! Purpose   : GET STYLE CODE WITHOUT SCALE CODE
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfGetStyle()
*!**************************************************************************
FUNCTION lfGetStyle
PARAMETERS lcDumdy,lcStyle
lcDumdy = SUBSTR(lcStyle,1,LEN(lcStyle)-4)
IF RIGHT(lcDumdy,1) ='-'
  lcDmdy = SUBSTR(lcDumdy,1,LEN(lcDumdy)-1)
ENDIF
RETURN 

*!**************************************************************************
*! Func. Name: lfShSolAdr
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/19/2000
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Terms
*!**************************************************************************
*! Calls       : gfRltFld() , gfCodDes() , gfGetAdr() , lfShiftAdr()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfShSolAdr()
*!**************************************************************************
FUNCTION lfShSolAdr
PARAMETERS lcDumdy
PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec

IF !EMPTY(INVHDR.CFACCODE)
  =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfShiftAdr('laFactor')
ENDIF
llEndGroup = .F.
=gfRltFld(INVHDR.cDivision , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')
lcSolTName = CUSTOMER.BTName
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
=lfShiftAdr('laSoldTo')

IF ORDHDR.Alt_ShpTo
  lcShpTName  = ORDHDR.STName
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
ELSE    && Else

  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    *-- DC Code
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF
  lcShpTName  = IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
  IF lnCUSRec <> 0 
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
ENDIF    && End of IF
=lfShiftAdr('laShipTo')
RETURN 
*!**************************************************************************
*! Func. Name: lfShiftAdr
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/19/2000
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!**************************************************************************
*! Calls     : gfRltFld() , gfCodDes() , gfGetAdr() , lfShiftAdr()
*!**************************************************************************
*! Parameters: The Address Array name
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfShSolAdr()
*!**************************************************************************
FUNCTION lfShiftAdr
PARAMETERS lcArrayNam
*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*B803257,1 Modify the notepad of the invocie header .[START]
*!**************************************************************************
*! Func. Name : lfPrtInvNt
*! Developer  : BASSEM RAFAAT (BWA)
*! Date       : 05/11/2000
*! Purpose    : Function to print the invocie header.
*!**************************************************************************
*! Calls From : Option Grid.
*!**************************************************************************
*! Parameters : lcReturn
*!**************************************************************************
*! Returns    :  lcReturn
*!**************************************************************************
*! Example    :  =lfPrtInvNt()
*!**************************************************************************
FUNCTION lfPrtInvNt
PARAMETERS lcReturn

IF llRpInvNot .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
   FOR lnLoop = 1 TO MEMLINES(NOTEPAD.mNotes)
     IF MLINE(NOTEPAD.mNotes , lnLoop) = CHR(13)
        lcNotes    = ALLTRIM(NOTEPAD.mNotes)
     ENDIF
   ENDFOR
   lcNotesTtl = 'Invoice Notes'
   lcNotes    = ALLTRIM(NOTEPAD.mNotes)
ENDIF

RETURN !EMPTY(lcNotesTtl)
*B803257,1 [END]