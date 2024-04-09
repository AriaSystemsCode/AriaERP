*:***************************************************************************
*: Program file  : ARPINVBS.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR Berkley Shirts.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C101628)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" 
*! Date          : 05/22/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: C101791
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVBS
*:***************************************************************************
*B804093,1 NAD 03/29/2001 The Fax no is not formated and the customer phone is not formated  
*B604375,1 BWA 04/09/2001 Get the DunsNo from the company settings not from the customer.
*C200192,1 BWA 05/09/2001 Chaging the Remit To address.
*:***************************************************************************

           *--Section of Initializing the variables --*
XNOTE_LOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine  = 1        && For the notepad
llNoRec    = .F.      && Variable used to make the endreport function in the main program.
XTAX       = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC  = SUBSTR(gfGetMemVar('M_TAX_DESC',gcAct_Comp),1,10)
XTAX_RATE  = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH  = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr  = SUBSTR(gfGetMemVar('M_TAX_REFE',gcAct_Comp),1,18)
NEWDOC = .T.
MAXROW = 47

*--Section of getting the address from the main program --*
HLINE1  = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1]) TO HLINE2
STORE TRIM(laCompAdd[2]) TO HLINE3
STORE TRIM(laCompAdd[3]) TO HLINE4
IF EMPTY(HLINE3)
  STORE HLINE4 TO HLINE3
  STORE ''     TO HLINE4
ENDIF
STORE TRIM(LACOMPADD[5]) TO HLINE5      && for the phone

*--Section to get the FromZone --*
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  IF !USED(lcWarHous)
    = gfOpenFile(gcDataDir+'WAREHOUS' ,gcDataDir+'WAREHOUS', 'SH', @lcWarHous, .T.)
  ENDIF
  lcFromZone = IIF(SEEK(InvHdr.cWareCode, lcWarHous ),&lcWarHous..UPS,lcFromZone)
ENDIF
IF !USED(lcSysZone)
  = gfOpenFile(gcSysHome+'Syszones' ,gcSysHome+'Frtzones', 'SH', @lcSysZone, .T.)
ENDIF

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'

*--End Section to get the FromZone --*
SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT

SCAN FOR &lcASExp
  WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
  XINVOICE = INVOICE
  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * Initialize document totals.
  *-----------------------------------------------------------------
  IF NEWDOC
    STORE 0.00 TO XPIECES, XSUBTOTAL
    NEWDOC    = .F.
    XORDER    = ORDER
    XPHONE    = INVHDR.PHONE
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XORDER    = ORDER
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSEASON   = SEASON
    XDIVISION = CDIVISION

    *** GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
    SEEK IIF(INVHDR.STORE = SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+INVHDR.STORE)

    =lfSolSpAdr()        && function in the main program (ARPINV) to get the adrress.
    XBTNAME  = lcSolTName
    XBTADDR1 = laSoldTo[1]
    XBTADDR2 = laSoldTo[2]
    XBTADDR3 = TRIM(laSoldTo[3])
    IF LEN(TRIM(laSoldTo[2])) =0
       XBTADDR2 = laSoldTo[3]
       XBTADDR3 = ''
    ENDIF
    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    XSTADDR3 = TRIM(laShipTo[3])
    IF LEN(TRIM(laShipTo[2])) =0
      XSTADDR2 = laShipTo[3]
      XSTADDR3 = ''
    ENDIF

    *** GET THE DESCRIPTION ABOUT THE CODES
    SELECT CODES
    SET ORDER TO CODES IN CODES
    PTERMS     = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA   = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    PSPCINST   = gfCodDes(INVHDR.SPCINST,'SPCINST')
    PDIVISION  = gfCodDes(INVHDR.CDIVISION,'CDIVISION')
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')

    DO CASE
      CASE 'G' $ lcUpsType
        XZN = gfGetZone(lcUpsType,lcFromZone,SUBSTR(CUSTOMER.cAddress5,1,3))
        XZN = IIF(!EMPTY(XZN),'('+ XZN +')' ,XZN)
      CASE '2' $ lcUpsType
        XZN  = '(12)'
      CASE 'N' $ lcUpsType
        XZN  = '(22)'
      OTHERWISE
        XZN  = ''
    ENDCASE

    PSHIPVIA = IIF(TRIM(PSHIPVIA) = "N/A" , "" ,TRIM(PSHIPVIA)) +XZN
    *----------------------------------------------
    * [FACTOR] NAME & ADDRESS
    *----------------------------------------------
    IF .F.    
      IF llPrnFact
        XFNAME  = lcFacName
        XFADDR1 = laFactor[1]
        XFADDR2 = laFactor[2]
        XFADDR3 = laFactor[3]
        IF LEN(TRIM(laFactor[2])) =0  && If address#2 is empty fill it with address#3.
          XFADDR2 = laFactor[3]
          XFADDR3 = ''
        ENDIF
      ENDIF
    ENDIF
    CURLOOP = '1'
  ENDIF
  *--END NEWDOC
  
  *--START PRINT--*
  *---------------*
  =lfGetInHdr()

  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  SELECT INVLINE
  ROW    = 27
  DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
    SELECT INVLINE
    IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
      IF ROW >= MAXROW
        =lfGetcont()
        =lfGetInHdr()
        LOOP
      ENDIF
      EXIT
    ENDIF
    IF TOTQTY = 0
      SKIP
      LOOP
    ENDIF
    SELECT INVLINE
    @ ROW,1 SAY LEFT(STYLE,11)
    @ ROW,12 SAY SUBSTR(STYLE,14,6)    
    @ ROW,18 SAY STYLE.SCALE
    @ ROW,20 SAY QTY1   PICTURE '@Z 99999'
    @ ROW,25 SAY QTY2   PICTURE '@Z 99999'
    @ ROW,30 SAY QTY3   PICTURE '@Z 99999'
    @ ROW,35 SAY QTY4   PICTURE '@Z 99999'
    @ ROW,40 SAY QTY5   PICTURE '@Z 99999'
    @ ROW,45 SAY QTY6   PICTURE '@Z 99999'
    @ ROW,50 SAY QTY7   PICTURE '@Z 99999'
    @ ROW,55 SAY QTY8   PICTURE '@Z 99999'
    @ ROW,60 SAY TOTQTY PICTURE '999999'
    XLINETOT   = PRICE * TOTQTY
    XPIECES    = XPIECES + TOTQTY
    XSUBTOTAL  = XSUBTOTAL+XLINETOT
    @ ROW,66  SAY PRICE     PICTURE '9999.99'
    @ ROW,74  SAY XLINETOT  PICTURE '999999.99'
    ROW = ROW+1

    *-- Print the Sku --*
    =lfPrtSku()
    @ ROW,1 SAY SUBSTR(STYLE.Desc,1,17)
    ROW = ROW+1
    * GET NEXT LINE ITEM
    SELECT INVLINE
    SKIP
  ENDDO
  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')
  IF ENDPAGE = '1' .AND. !XNOTE_LOOP
    =lfGetcont()
    LOOP
  ENDIF
  *** Print invoice Notepad.
  IF llRpInvNot
    SELECT NOTEPAD
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75
    IF TYPE + KEY <> 'C' + XINVOICE
      SEEK 'C' + XINVOICE
    ENDIF
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)
    IF TYPE + KEY = 'C' + XINVOICE
      @ ROW,02 SAY '* -- N O T E S -- *'
      ROW = ROW + 1
      DO WHILE lnNotLine <= lnMemLins
        IF ROW >= MAXROW
          XNOTE_LOOP = .T.
        ELSE
          XNOTE_LOOP = .F.
          @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          ROW = ROW + 1
        ENDIF
        IF ROW >= MAXROW
          =lfGetcont()
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
      IF !XNOTE_LOOP
        @ ROW,02 SAY '* -- END OF NOTES -- *'
        lnNotLine = 1
        ROW = ROW + 1
      ELSE
        =lfGetcont()
        LOOP
      ENDIF
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF
  * B804093,1 NAD 03/29/2001 [START] 
  * @ 50,07 SAY INVHDR.PHONE SIZE 1,16
  @ 50,07 SAY TRANSFORM (INVHDR.PHONE,lcPhonPict) SIZE 1,16
  * B804093,1 NAD 03/29/2001 [END] 
  SELECT INVHDR
  @ 50,55 SAY 'MERCHANDISE'
  @ 50,74 SAY XSUBTOTAL PICTURE '9999999.99'  
  ROW=51
  @ 51,07 SAY INVHDR.NOTE1

  IF XTAX .AND. XTAX_METH = 'M'           && Tax Method = Merchandise
    IF !EMPTY(lcTaxRefr)
      @ Row,55 SAY lcTaxRefr
      Row = Row + 1
    ENDIF
    @ ROW ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+STR (INVHDR.TAX_RATE,5,2) + ' %'
    @ ROW ,74 SAY INVHDR.TAX_AMT   PICT '9999999.99'
    ROW = ROW + 1
  ENDIF

  IF ROW = 53
    @ 53,07 SAY INVHDR.NOTE2
    llNotePrnt = .T.
  ELSE
    llNotePrnt = .F.
  ENDIF
  
  WKAMT = FREIGHT + INSUR + COD
  IF WKAMT <> 0
    @ ROW,55 SAY 'TOTAL - FREIGHT'
    @ ROW,74 SAY WKAMT       PICTURE '9999999.99'  
    ROW=ROW+1
  ENDIF

  IF DISCOUNT<>0
    @ ROW,55 SAY 'DISCOUNT'
    @ ROW,73 SAY DISCOUNT    PICTURE  '99999999.99'   
  ENDIF

  IF !llNotePrnt
    @ 53,07 SAY INVHDR.NOTE2
  ENDIF

  IF XTAX .AND. XTAX_METH = 'A'             && Tax Method = All
    @ 54,55 SAY lcTaxRefr                  
    @ 55 ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+STR(INVHDR.TAX_RATE,5,2) + ' %'
    @ 55 ,74 SAY INVHDR.TAX_AMT PICT '9999999.99'
  ENDIF  
  @ 56,18 SAY INVHDR.CARTONS   PICTURE '@Z 99999'
  @ 56,62 SAY XPIECES           PICTURE '9999999'
  @ 56,74 SAY INVHDR.TOTALCHG  PICTURE '9999999.99'
  SELECT INVHDR
  IF EOF()
    NEWDOC = .F.
    SET DEVICE TO SCREEN
    RETURN
  ELSE
     NEWDOC = .T.
  ENDIF
ENDSCAN
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : gfGetZone
*! Developer : Bassem Rafaat (BWA)
*! Date      : 05/22/2000
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINVBS.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,lcSyszone),'',&lcSyszone..ZONE)
*--End Function gfGetZone

*!*************************************************************
*! Name      : lfPrtSku
*! Developer : Bassem Rafaat (BWA)
*! Date      : 05/22/2000
*! Purpose   : Print the style Skus for a specific account.
*!*************************************************************
*! Called from : ARPINVBS.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfPrtSku

IF !SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  RETURN
ENDIF

SELECT Spck_Lin
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
  laSku = SPACE(16)
  SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku(lnX)=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN

  lnI = 1
  = SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  DO WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    lcStrToPrn = 'SKU N#' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = ;
             'S'+InvLine.Account+InvLine.Style .AND. !EOF()
      lcI = STR(lnI,1)
      lcStrToPrn = lcStrToPrn + Scale.Sz&lcI+':'+laSku(lnI) + ' '
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9       && if lnI > 5 exit to print in the next line.
        EXIT
      ENDIF
    ENDDO
    
    @ ROW,1 SAY lcStrToPrn
    ROW = ROW + 1
  ENDDO  
ELSE
  @ ROW,1 SAY Sku
  @ ROW,16 SAY 'CUSTOMER SKU #'
  ROW=ROW+1
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 05/22/2000
*! Purpose   : PRINT THE HEADER OF THE INVOICE
*!*************************************************************
*! Called from : ARPINVBS.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetInHdr()
*!*************************************************************
FUNCTION lfGetInHdr
  
SELECT INVHDR
@ 01,60 SAY XINVOICE
@ 01,72 SAY INVDATE
   
IF llPrntComp
  @ 02,05 SAY HLINE1
ENDIF

* LINE 3
IF llPrntComp
  @ 03,05 SAY HLINE2
ENDIF
@ 03,72 SAY SUBSTR( PDIVISION, 1, 14 )  
  
* LINE 4
IF llPrntComp
  @ 04,05 SAY HLINE3
ENDIF

* LINE 5
IF llPrntComp
  IF EMPTY(HLINE5)
    @ 05,05 SAY HLINE4
    *B804093,1 NAD 03/29/2001 [START]
    * @ 05,22 SAY LEFT(SYCCOMP.Ccom_Fax,16)
    @ 05,22 SAY TRANSFORM(LEFT(SYCCOMP.Ccom_Fax,16) , lcPhonPict)
    *B804093,1 NAD 03/29/2001 [END]
  ELSE
    @ 05,05 SAY HLINE4
  ENDIF
ENDIF
@ 05,72 SAY ORDER   

* LINE 6
IF llPrntComp

  IF !EMPTY(HLINE5)
    *B804093,1 NAD 03/29/2001 [START]
    *@ 06,05 SAY XPHONE SIZE 1,16
    @ 06,05 SAY HLINE5
    *@ 06,22 SAY LEFT(SYCCOMP.Ccom_Fax,16)
    @ 06,22 SAY TRANSFORM(LEFT(SYCCOMP.Ccom_Fax,16) , lcPhonPict)
    *B804093,1 NAD 03/29/2001 [END]
    
    *B804093,1 BWA 04/01/2001 Fix the code to get the duns no.[START]
    *@ 7,05 SAY IIF(!EMPTY(CUSTOMER.DUNS),"Duns # : " + ALLTRIM(CUSTOMER.DUNS),"")

    *B604375,1 BWA 04/09/2001 Get the DunsNo from the company settings not from the customer.[START]
    *@ 7,05 SAY IIF(CUSTOMER.TYPE = "M" AND !EMPTY(CUSTOMER.DUNS) , "Duns # : " + ALLTRIM(CUSTOMER.DUNS) , lfdunsno())
    @ 7,05 SAY IIF(!EMPTY(lcDunsNo),"Duns # : " + lcDunsNo ,"")
    *B604375,1 [END]


    *B804093,1 [END]

  ELSE
    *B804093,1 BWA 04/01/2001 Fix the code to get the duns no.[START]
    *@ 6,05 SAY IIF(!EMPTY(CUSTOMER.DUNS),"Duns # : " + ALLTRIM(CUSTOMER.DUNS),"")

    *B604375,1 BWA 04/09/2001 Get the DunsNo from the company settings not from the customer.[START]
    *@ 6,05 SAY IIF(CUSTOMER.TYPE = "M" AND !EMPTY(CUSTOMER.DUNS) , "Duns # : " + ALLTRIM(CUSTOMER.DUNS) , lfdunsno())
    @ 6,05 SAY IIF(!EMPTY(lcDunsNo),"Duns # : " + lcDunsNo ,"")
    *B604375,1 [END]

    *B804093,1 [END]

  ENDIF  
ENDIF

@ 07,72 SAY PIKTKT        
@ 8,18 SAY IIF(llPrnFact AND !EMPTY(INVHDR.cFACCODE),"REMIT TO: CENTURY BUSINESS CREDIT CORP.","")

*C200192,1 BWA 05/09/2001 Chaging the Remit To address.[START]
*@ 9,18 SAY IIF(llPrnFact AND !EMPTY(INVHDR.cFACCODE),"          P.O. Box 11395","")
@ 9,18 SAY IIF(llPrnFact AND !EMPTY(INVHDR.cFACCODE),"         P.O. Box 360286","")
*C200192,1 [END]

@ 09,72 SAY APPROVAL

*C200192,1 BWA 05/09/2001 Chaging the Remit To address.[START]
*@ 10,18 SAY IIF(llPrnFact AND !EMPTY(INVHDR.cFACCODE),"          Church Street Station","")
*@ 11,18 SAY IIF(llPrnFact AND !EMPTY(INVHDR.cFACCODE),"          NEW YORK NY  10286-1395","")
@ 10,18 SAY IIF(llPrnFact AND !EMPTY(INVHDR.cFACCODE),"          PITTSBURGH, PA 15250-6286","")
*C200192,1 [END]

@ 13,09 SAY XBTNAME
@ 13,55 SAY XSTNAME
@ 14,09 SAY XBTADDR1
@ 14,55 SAY XSTADDR1
@ 15,09 SAY XBTADDR2
@ 15,55 SAY XSTADDR2
@ 16,09 SAY XBTADDR3
@ 16,55 SAY XSTADDR3
@ 21,02 SAY ACCOUNT
@ 21,13 SAY LEFT(CUSTPO,10)
@ 21,24 SAY STORE
@ 21,34 SAY DEPT                         
@ 21,42 SAY LEFT(PTERMS,14)
@ 21,64 SAY REP1
@ 21,67 SAY REP2
@ 21,71 SAY SUBSTR(PSHIPVIA,1,14)
ROW = 23

lcScalStr = SPACE(0)
lcInvNo   = INVHDR.INVOICE
DO lpGtScl WITH lcInvNo
SELECT Scale
FOR lnI = 1 TO LEN( lcScalStr )
  lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
  SEEK 'S' + lcSizeScal
  IF !EMPTY(lcSizeScal)
    @ ROW,19 SAY lcSizeScal+" "+ SUBSTR(SCALE.SZ1,1,5) + SUBSTR(SCALE.SZ2,1,5) +;
                                 SUBSTR(SCALE.SZ3,1,5) + SUBSTR(SCALE.SZ4,1,5) +;
                                 SUBSTR(SCALE.SZ5,1,5) + SUBSTR(SCALE.SZ6,1,5) +;
                                 SUBSTR(SCALE.SZ7,1,5) + SUBSTR(SCALE.SZ8,1,5)
    Row = Row + 1
  ENDIF
ENDFOR
Row = 27
           *--End of function lfGetInHdr --*
*!*************************************************************
*! Name      : lpGtScl
*! Developer : BASSEM RAFAAT 
*! Date      : 05/22/2000
*! Purpose   : Build a string holding the first uniqe 5 scales
*!           : from the invoice line scal.
*!*************************************************************
*! Called from : ARINVBS.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpGtScl()
*!*************************************************************
PROCEDURE lpGtScl
PARAMETERS lcInvNo
PRIVATE lcAlias, lnCount

lcAlias = ALIAS()
SELECT InvLine
lcExp   = Invoice + STR(LineNo,6)   && To hold the invocie before the scan.
lnCount = 1
SEEK lcInvNo
SCAN WHILE InvLine.Invoice = lcInvNo .AND. lnCount <= 2
  IF !( Scale $ lcScalStr )
    lcScalStr = lcScalStr + Scale
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN
= SEEK(lcExp ,"InvLine")
SELECT (lcAlias)

*-- EOF( lpGtScl )
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 05/22/2000
*! Purpose   : PRINT THE FOOTER OF THE INVOICE.
*!*************************************************************
*! Called from : ARINVBS.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetcont()
*!*************************************************************
FUNCTION lfGetcont

@ ROW+1,12 SAY 'C O N T I N U E D ...'
*B804093,1 NAD 03/29/2001 [START]
*@ 50,07 SAY INVHDR.PHONE SIZE 1,16
@ 50,07 SAY TRANSFORM (INVHDR.PHONE,lcPhonPict) SIZE 1,16
*B804093,1 NAD 03/29/2001 [END]
@ 50,55 SAY 'MERCHANDISE'
@ 50,75 SAY '******'
ROW=ROW+1
@ 51,07 SAY INVHDR.NOTE1
@ 53,07 SAY INVHDR.NOTE2
@ 57,18 SAY INVHDR.CARTONS PICTURE '@Z 99999'
@ 57,62 SAY '******'
@ 57,75 SAY '******'
           *--End of function lfGetcont --*
*B804093,1 BWA 04/01/2001 Function to get the duns no.[START]
*!*************************************************************
*! Name      : lfdunsno
*! Developer : BASSEM RAFAAT 
*! Date      : 04/01/2001
*! Purpose   : Function used to get the dunc no.
*!*************************************************************
*! Called from : ARINVBS.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetcont()
*!*************************************************************
FUNCTION lfdunsno
PRIVATE lcAlias , lcRecNo

STORE SPACE(0) TO lcDunsNo
lcAlias = SELECT(0)
SELECT CUSTOMER
lcRecNo = EVAL(KEY())

=SEEK('M'+XACCOUNT,'CUSTOMER')
lcDunsNo = CUSTOMER.DUNS
lcDunsNo = IIF(EMPTY(lcDunsNo) , "" , "Duns # : " + lcDunsNo)

=SEEK(lcRecNo)
SELECT (lcAlias)

RETURN lcDunsNo

*--End of lfdunsno.
*B804093,1 [END]