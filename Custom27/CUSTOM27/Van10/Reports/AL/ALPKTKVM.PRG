*!********************************************************************
*: Program file  : ALO820.PRG (REFERENCE TO C102151)
*: Program desc. : CUSTOMIZED PICKTECKET FORM FOR VAN MAR INC.
*: For screen    : None.
*:         System: ARIA APPAREL SERIES 27
*:         Module: FORM
*:      Developer: AbdELrahim Koura . KAM
*:          Date : 21/01/2001
*:********************************************************************
*: Modifications :
**********************************************************************
*: B605525,1 BWA 02/26/2002 Fix several bugs in the program.
*: B605525,1 BWA 02/26/2002 A variable in SYFRMCDD for printing the SKU.
*: B605821,1 BWA 04/21/2002 Fix the bug of the bill to address doesn't print the distribution center address.
**********************************************************************
*-- Initializing the global variables that are needed by the form

STORE SPACE(6) TO lcPikTkt,lcOrderNo
lcAccount  = SPACE(05)
lcStore    = SPACE(08)
lcTermDesc = SPACE(15)
lcSalesRep = SPACE(03)
lcShipVia  = SPACE(15)
lcCustPo   = SPACE(10)
lcDept     = SPACE(05)
lnPkTktNo  = 22

STORE {} TO ldentered, ldstart , ldcomplete
STORE SPACE(30) TO 	lcNote1,lcNote2
STORE SPACE(1) TO lcMainSku,lcBillTo
DECLARE laSkuClr[6,6]   && hold SKU color
DECLARE laAddress[6,3]  && hold all address

STORE "" TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,;
            lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5, laSkuClr

STORE 0 TO lnHdrRow,lnLinRow,lnBotRow,MaxRow, lnTotAmn,lnClrLen,lnClrPos
*--save old setting for some files,also set relation  for tmphdr file
SELECT (lcTmpOrdl)
SET RELATION OFF INTO (lcTmpOrdh)
SET RELATION OFF INTO PIKTKT
INDEX ON ORDER +PIKTKT TAG OrdPKT
lcOrdPik=ORDER('piktkt')
SET ORDER TO TAG ordpik IN piktkt
lcSpkline=ORDER('spck_lin')
SET ORDER TO TAG spcklins IN spck_lin
SELECT ordline
lcOrdLine=ORDER()
SET ORDER TO TAG ordlinst
*-- call function to get color position
=lfGetClrD()
*-- Loop To print all the selected PikTkt
SELECT (lcTmpOrdh)
GO TOP
IF EOF()
  llNoRec=.T.
  =gfDialog('I','No records have been selected for the report')
  RETURN
ENDIF

SET RELATION TO order+PikTkt INTO (lcTmpOrdl) ADDITIVE
SET RELATION TO order+PikTkt INTO PIKTKT ADDITIVE
SCAN WHILE INKEY() <> 32
 *-- To hold the total amount of the piktkt
 lnTotAmn = 0

 *B605525,1 BWA 02/26/2002 Empty the variables from the old data.[START]
 STORE "" TO lcStAddr1,lcStAddr2,lcStAddr3
 *B605525,1 BWA 02/26/2002.[END]

 *-- This function is used to collect the data that will be printed
 *-- in the header,body, and footer.
   =lfGetData()
   *-- This function is used to print the form's header
   SET DEVICE TO PRINT
   *--print header of the form
   =lfPrntHdr()
   *-- This function is used to print the body of the form
   =lfPrntLine()
ENDSCAN

*B605525,1 BWA 02/26/2002 To clear the wait window.[START]
WAIT CLEAR
*B605525,1 BWA 02/26/2002.[END]

*--back actual setting for some file(s) that changed during enter
*-- close the the relation that we used
SET ORDER TO TAG (lcOrdPik) IN piktkt
SET ORDER TO TAG (lcOrdLine) IN  OrdLine
SET ORDER TO TAG (lcSpkline) IN spck_lin
SELECT (lcTmpOrdh)
SET RELATION OFF INTO PIKTKT
SET DEVICE TO SCREEN
*!*************************************************************
*! Name      : lfGetData
*! Developer : AbdElrahim Koura-KAM
*! Date      : 14/01/2001
*! Purpose   : To collect the data that will be printed in the
*!             form header,body, and footer.
*!             return .t. if collect data,otherwise return .f.
*!*************************************************************
*! Example            :  =lfGetData()
*!*************************************************************
FUNCTION lfGetData
IF OrdHdr.Alt_ShpTo
  lcStName = OrdHdr.StName
  lcStAddr1 = ORDHDR.cAddress1
  lcStAddr2 = ORDHDR.cAddress2
  lcStAddr3 = ORDHDR.cAddress3
ELSE
  lcStName = IIF(EMPTY(Customer.Dba), Customer.StName, Customer.Dba)

  *B605821,1 BWA 04/21/2002 Fix the bug of the bill to address doesn't print the distribution address.[START]
  PRIVATE lnRcNoCust
  lnRcNoCust = RECNO('CUSTOMER')
  IF !EMPTY(CUSTOMER.DIST_CTR)
    =SEEK('S' + PIKTKT.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcStName = Customer.StName
  ENDIF
  *B605821,1 BWA 04/21/2002.[END]

  =gfGetAdr('CUSTOMER','','','',1,'')
  
  *B605821,1 BWA 04/21/2002 In case there is a dist. center get back to the main record.[START]
  IF BETWEEN(lnRcNoCust,1,RECCOUNT('CUSTOMER'))
    GOTO lnRcNoCust IN CUSTOMER
  ENDIF  
  *B605821,1 BWA 04/21/2002.[END]

  FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount   = STR(laAddress[lnCount,1],1)
      *lcStAddr&lcCount = lcStAddr&lcCount + IIF(EMPTY(lcStAddr&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])

      *B605525,1 BWA 02/26/2002 Use the alltrim() to print the Zip code.[START]
      *lcStAddr&lcCount = lcStAddr&lcCount + IIF(EMPTY(lcStAddr&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,30)

      lcStAddr&lcCount = lcStAddr&lcCount + IIF(EMPTY(lcStAddr&lcCount),'',',')+;
      SUBSTR(ALLTRIM(laAddress[lnCount,2]),1,30)
      *B605525,1 BWA 02/26/2002.[END]

  ENDFOR

  lcstaddr1=alltrim(lcstaddr1)
  lcstaddr1=padr(lcstaddr1,30)
  lcstaddr2=alltrim(lcstaddr2)
  lcstaddr2=padr(lcstaddr2,30)
  lcstaddr3=alltrim(lcstaddr3)
  lcstaddr3=padr(lcstaddr3,30)
ENDIF

IF EMPTY(lcStAddr2)
  lcStAddr2 = lcStAddr3
  lcStAddr3 = ""
ENDIF
*--Get the Bill To address except the country.
lcBillTo = Customer.BillTo
lcBtName = Customer.BtName
lcBtAddr1=gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
lcBtAddr2=gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
lcBtAddr3=gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
IF EMPTY(lcBtAddr2)
  lcBtAddr2 = lcBtAddr3
  lcBtAddr3 = ""
ENDIF
lcPikTkt    = PikTkt
lcOrderNo   = Order
lcAccount   = PIKTKT.Account
lcStore     = PIKTKT.Store
lcTermDesc  = SUBSTR(gfCodDes(OrdHdr.cTermCode,'CTERMCODE'),1,18)
lcSalesRep  = OrdHdr.Rep1
lcShipVia   = SUBSTR(gfCodDes(OrdHdr.ShipVia,'SHIPVIA'),1,18)
lcDept      = OrdHdr.Dept
lcCustPo    = CustPo
ldEntered   = ORDHDR.Entered
ldStart     = ORDHDR.Start
ldComplete  = ORDHDR.Complete
lcNote1     = ORDHDR.NOTE1
lcNote2     = ORDHDR.NOTE2
RETURN

*!*************************************************************
*! Name      : lfPrntHdr
*! Developer : AbdElrahim Koura-KAM
*! Date      : 14/01/2001
*! Purpose   : To Print the header of the form
*!*************************************************************
*! Example            :  =lfPrntHdr()
*!*************************************************************
FUNCTION lfPrntHdr
IF lnPkTktNo = 22
  lnHdrRow  = 10                   && Start header row
  lnLinRow  = 26                   && Start lines printing row.
  lnBotRow  = 49                   && Start footer row.
  MaxRow    = lnBotRow - 3         && Max row number for lines printing.
  lnPkTktNo = 1
ELSE
  lnHdrRow  = lnBotRow + 12        && Start header row.
  lnLinRow  = lnHdrRow + 16        && Start lines printing row.
  lnBotRow  = lnLinRow + 23        && Start footer row.
  MaxRow    = lnBotRow - 03        && Max row number for lines printing.
  lnPkTktNo = lnPkTktNo  + 01
ENDIF

IF !OrdHdr.MultiPo
  lcCustPo   = OrdHdr.CustPo
ENDIF
*-- First Header
@ lnHdrRow,83 SAY lcPikTkt
@ lnHdrRow,101 SAY alltrim(lcStName)
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,101 SAY lcStAddr1
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,101 SAY lcStAddr2
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,09 SAY lcBtName
@ lnHdrRow,46 SAY lcStName
@ lnHdrRow,101 SAY substr(lcStAddr3,1,30)
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,09 SAY lcBtAddr1
@ lnHdrRow,46 SAY lcStAddr1
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,09 SAY lcBtAddr2
@ lnHdrRow,46 SAY lcStAddr2
@ lnHdrRow,80 SAY lcOrderNo
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,09 SAY substr(lcBtAddr3,1,30)
@ lnHdrRow,46 SAY substr(lcStAddr3,1,30)
lnHdrRow = lnHdrRow + 3
@ lnHdrRow,43 SAY IIF(!EMPTY(lcNote2),"Tracking # " + lcNote2 ,"")
lnHdrRow = lnHdrRow + 3
*-- Second header
*B605821,1 ASH 04/21/2002 (Begin) Print the DC Store name in case of DC
*@ lnHdrRow,02  SAY LEFT(lcStore,7)
@ lnHdrRow,02  SAY LEFT(lcStName,7)
*B605821,1 ASH 04/21/2002 (End)
IF !EMPTY(lcStore)
  DO CASE
    CASE lcBillTo = 'M'
      @ lnHdrRow,10  SAY lcAccount
    CASE lcBillTo = 'S'
      *B605821,1 ASH 04/21/2002 (Begin) Print the DC Store name in case of DC
      *@ lnHdrRow,10  SAY SUBSTR(ALLTRIM(lcStore),1,7)
      @ lnHdrRow,10  SAY SUBSTR(ALLTRIM(lcStName),1,7)
      *B605821,1 ASH 04/21/2002 (End)
    CASE lcBillTo = 'A'
      @ lnHdrRow,10  SAY 'Altern'
  ENDCASE
ELSE
  @ lnHdrRow,10  SAY lcAccount
ENDIF
@ lnHdrRow,17  SAY lcTermDesc
@ lnHdrRow,38  SAY lcSalesRep
@ lnHdrRow,43  SAY lcShipVia
@ lnHdrRow,63  SAY lcCustPo
@ lnHdrRow,79  SAY lcDept
@ lnHdrRow,104 SAY lcCustPo
@ lnHdrRow,121  SAY lcDept

*!*************************************************************
*! Name      : lfPrntLine
*! Developer : AbdElrahim Koura-KAM
*! Date      : 14/01/2001
*! Purpose   : To Print the Body of the form
*!*************************************************************
*! Example            :  =lfPrntLine()
*!*************************************************************
FUNCTION lfPrntLine
PRIVATE lcOldSelect

lcOldSelect=SELECT()
SET DEVICE TO SCREEN

*B605525,1 BWA 02/26/2002 Modify the text of the wait window.[START]
*WAIT 'PRINT PICK TICKETS - <Space Bar> TO ABORT' WINDOW NOWAIT
WAIT WINDOW "Printing Pick Ticket : " + PIKTKT NOWAIT
*B605525,1 BWA 02/26/2002.[END]

SET DEVICE TO PRINT
SELECT (lcTmpOrdl)
SCAN REST WHILE ORDER+PIKTKT=lcOrderNo+lcPikTkt
  DO CASE
    CASE TOTPIK <= 0
      LOOP
    CASE lnLinRow >= MaxRow
      =lfFooter()
      =lfPrntHdr()
  ENDCASE
  =SEEK(Style,"Style")
  @ lnLinRow,02 SAY Style
  @ lnLinRow,20 SAY TotPik PICTURE "999999"
  @ lnLinRow,41 SAY SUBSTR(gfCodDes(STYLE.cStyGrOUP,'CSTYGROUP'),1,18)
  @ lnLinRow,74 SAY IIF(llRpStyPrc,Price,0) PICTURE "@Z 99999999.99"
  @ lnLinRow,85 SAY Price*TotPik PICTURE "99999999.99"
  lnTotAmn = lnTotAmn + (Price*TotPik)

  =lfPrnSidLb()
  lnLinRow = lnLinRow + 1

  *B605525,1 BWA 02/26/2002 Comment this lines because it is duplicated and leave empty line.[START]
  *=lfPrnSidLb()
  *lnLinRow = lnLinRow + 1
  *B605525,1 BWA 02/26/2002.[END]

  @ lnLinRow,41 SAY STYLE.Desc

  *B605525,1 BWA 02/26/2002 Print the 2nd line in the side label.[START]
  =lfPrnSidLb()
  *B605525,1 BWA 02/26/2002.[END]

  *-- To get the style desc., color desc., and qty per size.
  =SEEK ('S'+Scale,'Scale')
  =lfStyCrInf()
  *-- Function to get the style's skus
  IF llRpSkuBck
    *-- get sku info and print it
    =lfGetSku()
  ENDIF
  FOR lnElement = 1 TO ALEN(laSkuClr,2)
    IF !EMPTY(laSkuClr[lnElement,1]) OR !EMPTY(laSkuClr[lnElement,2])
      lnLinRow = lnLinRow + 1
      @ lnLinRow,02 SAY IIF(!EMPTY(laSkuClr[lnElement,1]),laSkuClr[lnElement,1],'')
      @ lnLinRow,41 SAY IIF(!EMPTY(laSkuClr[lnElement,2]),laSkuClr[lnElement,2],'')
      laSkuClr[lnElement,1] = ""
      laSkuClr[lnElement,2] = ""
      =lfPrnSidLb()
      IF lnLinRow >= MaxRow
        =lfFooter()
        =lfPrntHdr()
      ENDIF
    ENDIF
  ENDFOR
  lnLinRow = lnLinRow + 1
  =lfPrnSidLb()
  lnLinRow = lnLinRow + 1
ENDSCAN
IF lnLinRow >= MaxRow AND llRpOrdNot .AND. SEEK('B' + lcOrderNo,'NotePad')
  =lfFooter()
  =lfPrntHdr()
ENDIF
=lfPrnSidLb()
lnLinRow = lnLinRow + 1
@ lnLinRow,02 SAY ALLTRIM(lcNote1) + '  ' + ALLTRIM(lcNote2)
=lfPrnSidLb()
IF lnLinRow >= MaxRow AND llRpOrdNot .AND. SEEK('B' + lcOrderNo,'NotePad')
  =lfFooter()
  =lfPrntHdr()
ENDIF

*-- This function is used to print the order notepad
IF llRpOrdNot
  =lfPrnNotes()
ENDIF
DO lpFilEmpLn
SELECT(lcOldSelect)
RETURN

*!*************************************************************
*! Name      : lfStyCrInf
*! Developer : AbdElrahim Koura-KAM
*! Date      : 14/01/2001
*! Purpose   : To replace the style desc.,color desc, and qty
*!             per size in array
*!*************************************************************
*! Example            :  =lfStyCrInf()
*!*************************************************************
FUNCTION lfStyCrInf
laSkuClr[1,2] = gfCodDes(SUBSTR(Style,lnClrPos,lnClrLen),'COLOR')
lnCol    = 41
lnLineNo = 2
lcStrLine  = ""
FOR lnCounter = 1 TO Scale.Cnt
  IF EVAL("Pik"+STR(lnCounter,1)) <= 0
    LOOP
  ENDIF
  lcCounter = STR(lnCounter,1)
  lcQtySize = ALLTRIM(EVAL('Scale.Sz'+lcCounter))+'/'+ALLTRIM(STR(Pik&lcCounter,6))
  IF lnCol+LEN(lcQtySize) < 74
    lcStrLine = lcStrLine + lcQtySize +'  '
    laSkuClr[lnLineNo,2] = lcStrLine
    lnCol = lnCol + LEN(lcQtySize) + LEN(SPACE(2))
  ELSE
    lnCol     = 41
    lcStrLine = ""
    lnLineNo  = lnLineNo + 1
    lcStrLine = lcStrLine + lcQtySize +'  '
    laSkuClr[lnLineNo,2] = lcStrLine
    lnCol = lnCol + LEN(lcQtySize) + LEN(SPACE(1))
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfGetSku
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 14/01/2001
*! Purpose   : To get the Style's Sku either per size or not.
*!*************************************************************
*! Example            :  =lfGetSku()
*!*************************************************************
FUNCTION lfGetSku
PRIVATE lnAlias

lnAlias = SELECT(0)
IF !SEEK('S'+ordhdr.Account+Style,'Spck_Lin')
  RETURN
ENDIF
=SEEK('M'+ordhdr.Account,'Customer')
lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
  lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
  lnDime2 = SkuTmpl.Len4
ELSE
  lnDime1 = 8  &&Default
  lnDime2 = 8  &&Default
ENDIF

IF SEEK('S'+ordhdr.Account+Style,'Spck_Lin')
  SELECT Spck_Lin
  COUNT REST WHILE Type+Account+Style+Pack_Id = ;
                 'S'+ ordhdr.Account + &lcTmpOrdl..Style;
                 For TotQty <> 1 ;
                 TO lnNoOfSkus

  IF lnNoOfSkus = 1
    llRpSkuSiz = .F.
  ELSE
    llRpSkuSiz=.T.
  ENDIF

  SEEK 'S'+ordhdr.Account+&lcTmpOrdl..Style
  laSkuClr [1,1] = "SKU#: "+ IIF(UPPER(lcSkuTmpl) $ 'JCP*MER',;
                             LEFT(Pack_Id,SkuTmpl.Len1)+' '+;
                             SUBSTR(Pack_Id,SkuTmpl.Len1+1,SkuTmpl.Len2),;
                             LEFT(Pack_Id,SkuTmpl.Len1+SkuTmpl.Len2+;
                             SkuTmpl.Len3))
  IF llRpSkuBck AND llRpSkuSiz
    lnCol = 02
    lnSkuNo = 2
    lcStrLine = ""
    ***
    FOR lnCounter = 1 TO SCALE.Cnt
      SELECT Spck_Lin
      SEEK 'S'+ordhdr.Account+&lcTmpOrdl..Style
      SCAN REST WHILE Type+Account+Style = ;
         'S'+ordhdr.Account+&lcTmpOrdl..Style
        IF EVAL('Spck_Lin.Qty'+ALLTRIM(STR(lnCounter,1))) = 1
          lcCounter = STR(lnCounter,1)
          EXIT
        ENDIF
      ENDSCAN

      IF EVAL(lcTmpOrdl+'.Pik'+lcCounter) <> 0
        lcStrToPrn = ALLTRIM(EVAL('Scale.Sz'+lcCounter))+":"+ ALLTRIM(SUBSTR(Pack_Id,lnDime1+1,lnDime2))
        IF lnCol+LEN(lcStrToPrn) < 40
          lcStrLine = lcStrLine + lcStrToPrn + '  '
          laSkuClr[lnSkuNo,1] = lcStrLine
          lnCol = lnCol + LEN(lcStrToPrn) + LEN(SPACE(2))
        ELSE
          lnCol     = 02
          lcStrLine = ""
          lnSkuNo   = lnSkuNo + 1
          lcStrLine = lcStrLine + lcStrToPrn + '  '
          laSkuClr [lnSkuNo,1] = lcStrLine
          lnCol     = lnCol + LEN(lcStrToPrn) + LEN(SPACE(2))
        ENDIF
      ENDIF
    ENDFOR
  ENDIF
  IF llRpSkuBck AND !llRpSkuSiz
    lcMainSku = "SKU#: "+ IIF(UPPER(lcSkuTmpl) $ 'JCP*MER',;
                             LEFT(Pack_Id,SkuTmpl.Len1)+' '+;
                             SUBSTR(Pack_Id,SkuTmpl.Len1+1,SkuTmpl.Len2),;
                             LEFT(Pack_Id,SkuTmpl.Len1+SkuTmpl.Len2+;
                             SkuTmpl.Len3))
  ENDIF
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfPrnNotes
*! Developer : AbdElrahim Koura-KAM
*! Date      : 14/01/2001
*! Purpose   : To Print the Order notepad.
*!*************************************************************
*! Example            :  =lfPrnNotes()
*!*************************************************************
FUNCTION lfPrnNotes
PRIVATE lnAlias
lnAlias = SELECT(0)
lnNotLine = 1
IF SEEK('B' + lcOrderNo,'NotePad')
  lnLinRow = lnLinRow + 1
  IF lnLinRow >= MaxRow
    =lfFooter()
    =lfPrntHdr()
  ENDIF
  SELECT NotePad
  lnOldMemW = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  lnMemLins = MEMLINES(Mnotes)
  @ lnLinRow,02 SAY '* -- N O T E S -- *'
  =lfPrnSidLb()
  DO WHILE lnNotLine <= lnMemLins
    lnLinRow = lnLinRow + 1
     IF lnLinRow >= MaxRow
       =lfPrnSidLb()
       =lfFooter()
       =lfPrntHdr()
    ENDIF
    @ lnLinRow,02 SAY MLINE(Mnotes,lnNotLine)
    =lfPrnSidLb()
    lnNotLine = lnNotLine + 1
  ENDDO
  SET MEMOWIDTH TO lnOldMemW
ENDIF
SELECT(lnAlias)
*!*************************************************************
*! Name      : lpFilEmpLn
*! Developer : AbdElrahim Koura-KAM
*! Date      : 14/01/2001
*! Purpose   : Fill the empty pick tecket lines.
*!*************************************************************
*! Example            :  =lpFilEmpLn()
*!*************************************************************
PROCEDURE lpFilEmpLn
DO WHILE lnLinRow <= MaxRow
  lnLinRow = lnLinRow + 1
  =lfPrnSidLb()
ENDDO
@ lnBotRow,48  SAY ldEntered
@ lnBotRow,57 SAY ldStart
@ lnBotRow,66  SAY ldComplete
@ lnBotRow,84  SAY lnTotAmn PICTURE "999999999.99"
@ lnBotRow,104 SAY lcCustPo
@ lnBotRow,121 SAY lcDept
*!*************************************************************
*! Name      : lfPrnSidLb
*! Developer : AbdElrahim Koura-KAM
*! Date      : 14/01/2001
*! Purpose   : To Print the Side label.
*!*************************************************************
*! Example            :  =lfPrnSidLb()
*!*************************************************************
FUNCTION lfPrnSidLb

DO CASE
  CASE lnLinRow = lnHdrRow + 16
    @ lnLinRow,101 SAY lcStName
  CASE lnLinRow = lnHdrRow + 17
    @ lnLinRow,101 SAY lcStAddr1
  CASE lnLinRow = lnHdrRow + 18
    @ lnLinRow,101 SAY lcStAddr2
  CASE lnLinRow = lnHdrRow + 19
    @ lnLinRow,101 SAY substr(lcStAddr3,1,30)
ENDCASE

*!*************************************************************
*! Name      : lfFooter
*! Developer : AbdElrahim Koura-KAM
*! Date      : 14/01/200
*! Purpose   : To Print the footer of the form.
*!*************************************************************
*! Example            :  =lfFooter()
*!*************************************************************
FUNCTION lfFooter
@ lnBotRow,48 SAY ldEntered
@ lnBotRow,57 SAY ldStart
@ lnBotRow,66 SAY ldComplete
@ lnBotRow,84 SAY "*********"
@ lnBotRow,104 SAY lcCustPo
@ lnBotRow,121  SAY lcDept
*!*************************************************************
*! Name      : lfGetClrD
*! Developer : AbdElrahim Koura-KAM
*! Date      : 14/01/2001
*! Purpose   : To get color position
*!*************************************************************
*! Example   :  =lfGetClrD()
*!*************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*--Get Color Discreption