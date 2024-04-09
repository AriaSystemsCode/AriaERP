*:**************************************************************************
*: PROGRAM   : ALPKTKR.PRG  (For Andrew Marc)
*:           : Converted form 26 to 27.
*: DESC.     : PRINT INVOICE - 66 LINE PAGE, 8 1/2" x 11" (For Andrew Marc)
*: DATE      : 04/07/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Refer to  : (C101384)
*:**************************************************************************
*: Calls : 
*:         FUNCTION  : lfPriHdr()
*:                   : lfGetHData()
*:                   : lfGetWareh()
*:					 : lfPriScl()
*:                   : lfPriPik()
*:                   : lfPrnFot()
*:                   : lfPrnStrn()
*:                   : lfPrnNotPd()
*:                   : lfPrnLoc()
*:                   : lfClear()
*:                   : lfPrnLnNot()
*:         PROCEDURE : lpPrtSku()
*:**************************************************************************
*: Modifications:
*: B802540,1 ADEL 08/31/1999 Update PIKTKT.prtflag with 'P' when printing a piktkt.
*: B803008,1 AHM  01/26/1999 Supress the empty lines in copm. address and get the
*: B803008,1                 long description for the division
*: B803155,1 KHM 05/07/2000  Fix the bug of not printing the company name
*: B803155,1                 when the long division name is empty.
*: B124507,1 BWA 10/07/2004 Fix the bug of not printing Special Instructions.
*:**************************************************************************
*--Get the needed variables.

*B803008,1 AHM Declare array to be sent to gfRltFld function (Start)
DECLARE laDisRltFld[1,2]
laDisRltFld[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDisRltFld[1,2] = 'lcDivLName'
*B803008,1 AHM Declare array to be sent to gfRltFld function (End)

llMultiWH = (gfGetMemVar('M_WareHouse')  = 'Y')
llWLoc    = (gfGetMemVar('M_WareLoc')  = 'Y')
lcDunsNo  = gfGetMemVar('XDUNS')
*-- Opening nessecary file
=gfOpenFile(gcDataDir+'SalesRep',gcDataDir+'SalesRep','SH')
=gfOpenFile(gcDataDir+'InvHdr',gcDataDir+'InvHdr','SH')
=gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH')
=gfOpenFile(gcDataDir+'SkuTmpl',gcDataDir+'SkuTmpl','SH')
SET ORDER TO TAG spcklins IN SPCK_LIN
SET ORDER TO TAG Whslocst IN WHSLOC
*lcTmpFile = gfTempName()
*-- Get the style major and color.
*-Get the style major length
lnMajLen = LEN(gfItemMask('PM'))
*--Get the color
STORE 0 TO lnColorLen,lnNonMajSt
*--Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Get the Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lnNonMajSt = laMajSegs[lnI,4]
    lnColorLen = LEN(IIF(lnColorLen = 0 .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lnColorLen + laMajSegs[lnI-1,6] + laMajSegs[lnI,3]))
    EXIT
  ENDIF
ENDFOR 
*-- Variable declaration
STORE ' ' TO lcSkuSize,lcStrToPrn,lcStr,lcScale
STORE  0  TO lnLen,lnAmnt,lnCount,ROW ,lnMerch,lnGranTot,lnTotChg,lnShip
STORE  0  TO lnFrei,lnDisc,lnMerch,lnShip,lnPageNo
STORE 'Y' TO lcPrnStLoc
STORE .T. TO llPrint,llDuns,llFirst
STORE .F. TO llStart,llWarLoc,llPrnSkPck,llPrnSkSiz
STORE SPACE(03) TO lcWState,lcRep1
STORE SPACE(05) TO lcDept,lcAccount
STORE SPACE(06) TO lcPickTkt,lcWareCode,lcOrder,lcFactor
STORE SPACE(07) TO lcStore
STORE SPACE(10) TO lcApproval,lcCustPo
STORE SPACE(11) TO lcDuns 
STORE SPACE(15) TO lcShiVCode,lcTerVCode,lcWCity
STORE SPACE(19) TO lcStyle
STORE SPACE(25) TO lcSalesNam
STORE SPACE(30) TO lcStyColD,lcNotes1,lcNotes2,lcDivLName
STORE {} TO ldPikDate,ldOrdCancl,ldOrdStart
DIMENSION laAddress[1,1]
STORE '' TO laAddress
*- Ship to and Bill to Addresses
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6
*-- End Variable declaration
IF llWLoc 
  *--Print Warehouse Location ?
  llWarLoc = (gfModalGen('QRM44074B44009','DIALOG' ) = 1)
ENDIF
*--Print SKU/PACK ?
llPrnSkPck = (gfModalGen('QRM44075B44009','DIALOG' ) = 1) 
IF llPrnSkPck
  *-- Print SKU by size ?
  llPrnSkSiz = (gfModalGen('QRM44076B44009','DIALOG' ) = 1) 
ENDIF
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
*-- This file holds all the lines of the selected pick tickets.
SELECT (lcTmpOrdL)
*--- SSH
SET FILTER TO cGrupDetal = "D"
*--- SSH
GOTO TOP
DO WHILE !EOF() AND INKEY() <>32
  WAIT WINDOW 'PRINT PICK TICKETS - <Space Bar> TO ABORT' NOWAIT
  *--Clear variables.
  =lfClear()
  *-- Function to get piktkt HEADER information.  
  =lfGetHData()
  *--Print the piktkt header information. 
  =lfPriHdr()
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6)=lcPickTkt+lcOrder+'D'
    @ ROW,00 SAY Style
    lcClrDesc = SUBSTR(gfCodDes(SUBSTR(STYLE,lnMajLen+2) , 'COLOR'),1,15)
    lcClrDesc = IIF(!EMPTY(lcClrDesc),lcClrDesc,SPACE(10))
    lcStyColD = Style.Desc+SPACE(05)+ lcClrDesc
    @ ROW,20 SAY SUBSTR(lcStyColD,1,30)
    ROW = ROW + 1
    lcScale = Scale
    lcStyle = Style
    *--Print the scale.
    =lfPriScl()
    *--Print the picked qtys.
    =lfPriPik()
    ROW = ROW + 1
    *--Print the order lines notes.
    =lfPrnLnNot()
    *-- Prnit the sku.
    IF  llPrnSkPck
      DO lpPrtSku
    ENDIF
    lnCol1 = IIF(llStart,00,09)
    IF !llPrnSkSiz
      @ ROW,lnCol1 SAY SUBSTR(lcStrToPrn,1,75)
      lcCol=IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34)
      Row = Row + 1
    ELSE
      =lfPrnStrn() 
      lcSkuSize=lcStrToPrn+' '+lcSkuSize
      @ ROW,lnCol1 SAY SUBSTR(lcSkuSize,1,75)
      Row = Row + 1  
    ENDIF
    IF ROW >= 60
      =lfPriHdr()
    ENDIF
    IF llWLoc AND llWarLoc .AND. SEEK(STYLE+SPACE(6)+lcWareCode,'WHSLOC')
      @ ROW,00 SAY 'LOC.:'
      =lfPrnLoc()
    ENDIF
    Row = Row + 1
  ENDSCAN 
  @ ROW,58 SAY lnGranTot
  Row = Row + 1
  Row = Row + 3
  IF  SUBSTR(ALLTRIM(lcNotes1),1,1) <>'*';
    .AND. SUBSTR(ALLTRIM(lcNotes1),1,1) <>'I' .AND. llRpOrdNot;
    .AND. SUBSTR(ALLTRIM(lcNotes1),1,1) <>'i';
    .AND. UPPER(SUBSTR(ALLTRIM(lcNotes1),2,1)) <> SPACE(01)
    @ ROW , 00 SAY lcNotes1
    Row = Row + 1
  ENDIF
  IF  SUBSTR(ALLTRIM(lcNotes2),1,1) <>'*';
    .AND. SUBSTR(ALLTRIM(lcNotes2),1,1) <>'I' .AND. llRpOrdNot;
    .AND. UPPER(SUBSTR(ALLTRIM(lcNotes2),2,1)) <> SPACE(01);
    .AND. SUBSTR(ALLTRIM(lcNotes2),1,1) <>'i'
    @ ROW , 00 SAY lcNotes2
    Row = Row + 1  
  ENDIF
  IF llRpOrdNot
    =lfPrnNotPd()
  ENDIF
  =lfPrnFot()
  *B802540,1 (Begin) Update PIKTKT.prtflag with 'P' if a piktkt is printed.
  =lfEndGroup()
  *B802540,1 (End)
  *--Get the next piktkt
  SELECT (lcTmpOrdL)
  *--- SSH
  *SKIP
  *--- SSH
ENDDO
*--Flase the following flag  for ALPKTK program to endreport.
llNoRec = .T.
=ENDREPORT()
SET DEVICE TO SCREEN
RETURN

*:*************************************************************************
*: PROGRAM   : lfPriHdr (C101209)
*: DESC.     : PRINT INVOICE Header
*: MODULE    : Aria Apparel Series.
*: DATE      : 04/07/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfPriHdr

ROW = 0
lnPageNo = lnPageNo + 1
*B803155,1 KHM 05/07/2000 (Begin) Printing the company name if the long 
*B803155,1                division name was empty.
*@ ROW,00 SAY lcDivLName
@ ROW,00 SAY IIF(EMPTY(lcDivLName),gcCom_Name,lcDivLName)
*B803155,1 KHM 05/07/2000 (End)

@ ROW,55 SAY "PACKING LIST : "
@ ROW,70 SAY lcPickTkt
ROW = ROW + 1

*B803155,1 KHM 05/07/2000 (Begin) Print address laCompAdd[1] instead of laCompAdd[2]
*@ ROW,00 SAY laCompAdd[2]
@ ROW,00 SAY laCompAdd[1]
*B803155,1 KHM 05/07/2000 (End) 

@ ROW,55 SAY "DATE         : "
@ ROW,70 SAY ldPikDate
ROW = ROW + 1

*B803155,1 KHM 05/07/2000 (Begin) Print address laCompAdd[2] instead of address laCompAdd[3]
*@ ROW,00 SAY laCompAdd[3]
@ ROW,00 SAY laCompAdd[2]
*B803155,1 KHM 05/07/2000 (End)

ROW = ROW + 1
*B803155,1 KHM 05/07/2000 (Begin) Print address laCompAdd[3] instead of laCompAdd[4]
*@ ROW,00 SAY laCompAdd[4]
@ ROW,00 SAY laCompAdd[3]
*B803155,1 KHM 05/07/2000 (End)

ROW = ROW + 1
@ ROW,00 SAY "DUNS# "+lcDuns
ROW = ROW + 2
@ ROW,10 SAY "Bill To :"
@ ROW,45 SAY "Ship To :"
ROW = ROW + 1
@ ROW,10 SAY lcBtName
@ ROW,45 SAY lcStName
ROW = ROW + 1
@ ROW,10 SAY lcBtAdd1
@ ROW,45 SAY lcStAdd1
ROW = ROW + 1
@ ROW,10 SAY lcBtAdd2
@ ROW,45 SAY lcStAdd2
ROW = ROW + 1
@ ROW,10 SAY lcBtAdd3
@ ROW,45 SAY lcStAdd3
ROW = ROW + 2
@ ROW,00 SAY "Credit Approval #: "+lcApproval
@ ROW,33 SAY "Account : "+lcAccount
@ ROW,70 SAY "Page#: "+ALLTRIM(STR(lnPageNo))
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',78)
ROW = ROW + 1
@ ROW,00 SAY "P.O.    : " +lcCustPo
@ ROW,30 SAY "Dept.   : " +lcDept
@ ROW,58 SAY "F.O.B.  : " + SUBSTR(lcWCity +SPACE(02) +lcWState,1,10)
ROW = ROW + 1
@ ROW,00 SAY "Ship    : " 
@ ROW,10 SAY  ldOrdStart
@ ROW,30 SAY "Cancel  : " 
@ ROW,41 SAY ldOrdCancl
@ ROW,58 SAY "Via     : " + SUBSTR(lcShiVCode ,1,10)
ROW = ROW + 1
@ ROW,00 SAY "Order   : " + lcOrder
@ ROW,30 SAY "Ship To : " + lcStore
@ ROW,58 SAY "Terms   : " + SUBSTR(lcTerVCode ,1,10)
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',78)
ROW = ROW + 1
@ ROW,00 SAY "Product                                                      Tot Qty     Price  "
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',78)
ROW = ROW + 1

*:*************************************************************************
*: PROGRAM   : lfGetHData (C101209)
*: DESC.     : Load Variable
*: MODULE    : Aria Apparel Series.
*: DATE      : 04/07/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfGetHData
PRIVATE lcAlias,lnCount

lcAlias    = ALIAS()
lcPickTkt  = PikTkt
lcAccount  = PikTkt.Account
lcStore    = PikTkt.Store
lcOrder    = Order
lcWareCode = PikTkt.cWareCode
ldPikDate  = PikTkt.Date
lcCustPo   = IIF(EMPTY(PikTkt.CustPo),ORDHDR.CustPo,PikTkt.CustPo)
lcCusExp   = IIF(EMPTY(lcStore),"M","S") + lcAccount +IIF(EMPTY(lcStore),"",lcStore)
llDuns  = .T.
lcDuns  = SPACE(11)
IF UPPER(ORDHDR.cDivision) = 'L2    ' OR UPPER(ORDHDR.cDivision) = 'M2    ' OR UPPER(ORDHDR.cDivision) = 'M3    '
  lcDuns = '84-895-9474'
  llDuns = .F.
ENDIF   
IF UPPER(ORDHDR.cDivision) = 'L1    ' OR UPPER(ORDHDR.cDivision) = 'M1    ' OR UPPER(ORDHDR.cDivision) = 'M4    '
  lcDuns = '62-374-5114'
  llDuns = .F.
ENDIF   
IF INLIST(UPPER(ORDHDR.cDivision),'D1','D2','D3','D4','D5','D6','D7','D8')
  lcDuns ='04-702-2665'
  llDuns = .F.
ENDIF
IF llDuns
  lcDuns = lcDunsNo
ENDIF
lcApproval = OrdHdr.Approval
lcDept     = OrdHdr.Dept
lcNotes1   = OrdHdr.Note1
lcNotes2   = OrdHdr.Note2
ldOrdCancl = OrdHdr.Complete
ldOrdStart = OrdHdr.Start
SELECT CODES
SET ORDER TO TAG CODES
lcDivLName = gfCodDes(OrdHdr.cDivision,PADR('CDIVISION',10))
*B803008,1 AHM calling gfRltFld function (Start)
*lcDivLName = IIF(!EMPTY(lcDivLName),lcDivLName,SPACE(30))
=gfRltFld(OrdHdr.cDivision,@laDisRltFld,'CDIVISION')
*B803008,1 AHM calling gfRltFld function (End)
lcShiVCode = gfCodDes(OrdHdr.ShipVia ,PADR('SHIPVIA',10))
lcShiVCode = IIF(!EMPTY(lcShiVCode),lcShiVCode,SPACE(07))
lcTerVCode = gfCodDes(OrdHdr.cTermCode,PADR('CTERMCODE',10))
lcTerVCode = IIF(!EMPTY(lcTerVCode),lcTerVCode,SPACE(07))
*-- Function to get warehouse city and state
=lfGetWareh()
*--Get the proper record in the customer file.
=SEEK(lcCusExp,"Customer")
*-- Bill to Address
lcBtName  = CUSTOMER.BTNAME
=gfGetAdr('Customer','','','',1,'2')
*--Get the Bill To adddess except the country.
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAdd&lcCount = lcBtAdd&lcCount + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR
*-- Sold to Address  
IF OrdHdr.Alt_ShpTo
  lcStName = OrdHdr.STNAME
  lcStAdd1 = OrdHdr.cAddress1
  lcStAdd2 = OrdHdr.cAddress2
  lcStAdd3 = OrdHdr.cAddress3
  lcStAdd4 = OrdHdr.cAddress4
  lcStAdd5 = OrdHdr.cAddress5
ELSE
  *--Get the distribution center address if any.(lcCusAlias ALIAS exists in arpinv.prg)
  lcDist = IIF(!EMPTY(Customer.Dist_Ctr),Customer.Dist_Ctr,' ')
  IF !EMPTY(lcDist)
    =SEEK('S'+lcAccount+lcDist,'Customer')
  ENDIF
  lcStName = Customer.STNAME
  =gfGetAdr('Customer','','','',1,'')
  *--Get the Ship To adddess.
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR  	
ENDIF  
SELECT (lcAlias) 
 
*:*************************************************************
*: Name      : lfGetWareh
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: Date      : 04/07/1999
*: Purpose   : To get the warehouse information.
*:*************************************************************
*: Example   :  lfGetWareh()
*:*************************************************************
FUNCTION lfGetWareh

*-- To get the warehouse address.
IF llMultiWH AND SEEK(lcWareCode,'WareHous')
  lcWCity  = ALLTRIM(WareHous.cCity)
  lcWState = ALLTRIM(WareHous.cState)
ELSE
  GO TOP IN WAREHOUS
  lcWCity  = ALLTRIM(WareHous.cCity)
  lcWState = ALLTRIM(WareHous.cState)
ENDIF

*:*************************************************************************
*: PROGRAM   : lfPriScl (C101209)
*: DESC.     : Colect Lines
*: MODULE    : Aria Apparel Series.
*: DATE      : 04/07/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfPriScl
PRIVATE lnAlias,lcSubStr,lnIndex,lnPos1

lnAlias = SELECT()
lnIndex = 1
lnPos1  = 4
lnCount = Scale.CNT
FOR lnIndex = 1 TO Scale.CNT
  lcSubStr = ALLTRIM(STR(lnIndex))
  @ ROW,lnPos1 SAY PADL(ALLTRIM(Scale.Sz&lcSubStr),6,' ')
  lnPos1  = lnPos1 + 6
ENDFOR
ROW = ROW + 1
SELECT(lnAlias)

*:*************************************************************************
*: PROGRAM   : lfPriPik (C101209)
*: DESC.     : Colect Lines
*: MODULE    : Aria Apparel Series.
*: DATE      : 04/07/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfPriPik
PRIVATE lnAlias,lcSubStr,lnIndex,lnPos1

lnAlias = SELECT()
lnIndex = 1
lnPos1  = 4
FOR lnIndex = 1 TO lnCount
  lcSubStr = ALLTRIM(STR(lnIndex))
  @ ROW,lnPos1 SAY PADL(ALLTRIM(STR(Pik&lcSubStr)),6,' ')
  lnPos1  = lnPos1 + 6
ENDFOR
lnCount    = 0
@ ROW,62 SAY TotPik
lnGranTot  = lnGranTot + TotPik
IF llRpStyPrc
  @ ROW,70 SAY Price
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfPrnFot
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 04/07/1999
*! Purpose   : Printing the Order notepad
*!*************************************************************
*! Example            :  lfPrnFot()
*!*************************************************************
FUNCTION lfPrnFot

ROW = 59
IF SEEK('M'+lcAccount,'Customer')

  *B124507,1 BWA 10/07/2004 Select the right record in the ORDHDR.[START]
  PRIVATE lcAlasOrd , lcKey
  lcAlasOrd = SELECT(0)
  SELECT ORDHDR
  lckey = EVAL(KEY())
  =SEEK("O" + lcOrder , 'ORDHDR')
  *B124507,1 BWA 10/07/2004.[END]
  
  lcSpcInst = gfCodDes(OrdHdr.SPCINST , 'SPCINST')

  *B124507,1 BWA 10/07/2004 Return to the Original record.[START]
  =SEEK(lckey)
  SELECT (lcAlasOrd)
  *B124507,1 BWA 10/07/2004.[END]

  IF !EMPTY(lcSpcInst)
    @ ROW,00 SAY "Special Instructions : "
    @ ROW,24 SAY lcSpcInst
    ROW = ROW + 1
  ENDIF  
ENDIF
@ ROW,00 SAY "Picked By :"
@ ROW,36 SAY "Checked by:"
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',78)
ROW = ROW + 1
@ ROW,00 SAY ' Freight : '
@ ROW,36 SAY ' Weight : '
@ ROW,62 SAY ' Cartons : '
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',78)
  
*:*************************************************************************
*: PROGRAM     : lpPrtSku.
*: DESC.       : Print the ,main style/color Skus for a specific account.
*: DATE        : 22/04/98
*: Developer   : Ahmed Salah Shalaby
*: Called from : 
*:              Procedures : Alo820E.
*:*************************************************************
PROCEDURE lpPrtSku
PRIVATE lnAlias

lnAlias = SELECT()
IF !SEEK('S'+lcAccount+lcStyle,'Spck_Lin')
  lcStrToPrn = " "
  RETURN
ELSE
  SELECT Spck_Lin
  IF EMPTY(Sku)
    lnI = 1
    = SEEK('S'+lcScale,'Scale')
    = SEEK('M'+lcAccount,'Customer')
    lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
    IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
      lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
      lnDime2 = SkuTmpl.Len4
    ELSE
      *-- Default
      lnDime1 = 8
      lnDime2 = 8
    ENDIF
    = SEEK('S'+lcAccount+lcStyle,'Spck_Lin')
    llStart = .T.
    lcStrToPrn = 'SKU#:' + SUBSTR(Pack_Id,1,lnDime1)
    lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
    lnLength   = LEN(lcStrToPrn)+2
  ELSE
    @ ROW,00 SAY SUBSTR(Sku,1,8)
  ENDIF
ENDIF  
SELECT(lnAlias)

*:***************************************************************************
*: PROGRAM     : lfPrnStrn.
*: DESC.       : Print the ,main style/color Skus for a specific account.
*: DATE        : 04/07/1999
*: Developer   : Adel Mohhamed El Gazzar (ADEL)
*: Called from : 
*:              Procedures : Alo820E.
*:*************************************************************
FUNCTION lfPrnStrn

lcAlias=ALIAS()
lcSkuSize =' '
lcKey     =' '
IF SEEK('S'+lcAccount+lcStyle,'Spck_Lin')
  lcKey='S'+lcAccount+lcStyle
  lnSep=1
  Q=1
  W=STR(Q,1)
  X=1
  Z=STR(X,1)
  SELECT Spck_Lin
  SCAN REST WHILE 'S'+lcAccount+lcStyle = lcKey
    IF Qty&Z > 0
      lcSkuSize = lcSkuSize+'S'+W+':'+SUBSTR(Pack_Id,9,5)+' '
    ENDIF
    lnSep=lnSep+6
    X=X+1
    Z=STR(X,1) 
    Q=Q+1
    W=STR(Q,1)
    IF Z='9'
      EXIT
    ENDIF  
  ENDSCAN
  lcSkuSize=ALLTRIM(lcSkuSize)
ENDIF
SELECT (lcAlias)
RETURN

*!*************************************************************
*! Name      : lfPrnNotPd
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 04/07/1999
*! Purpose   : Printing the Order notepad
*!*************************************************************
*! Example            :  lfPrnNotPd()
*!*************************************************************
FUNCTION lfPrnNotPd
PRIVATE lnAlias, lnOldMemW, lnMemLins, lnNotLine

lnAlias   = SELECT()
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
llQuit    = .T.
SET MEMOWIDTH TO 66
SELECT NotePad
IF SEEK ('B' + lcOrder)
  lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  DO WHILE lnNotLine <= lnMemLins
    IF ROW >= 60
      =lfPriHdr()
    ENDIF
    IF !EMPTY(MLINE(MNOTES,lnNotLine)) .AND. ;
         (UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1)) <>'I';
         .OR. SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1) <> SPACE(01))
      IF  SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1) <> '*';
         .OR. SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1) <> SPACE(01)
        @ ROW,00 SAY IIF(UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1)) = 'P' .AND.;
                         UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1)) = SPACE(01) ,;
                         SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,LEN(ALLTRIM(MLINE(MNOTES,lnNotLine)))), ALLTRIM(MLINE(MNOTES,lnNotLine)))
        ROW       = ROW + 1
      ENDIF
    ENDIF
    lnNotLine = lnNotLine + 1
  ENDDO
ENDIF
SET MEMOWIDTH TO lnOldMemW
SELECT(lnAlias)

*:************************************************************************
*: Program file  : lfPrnLoc
*: Program desc. : Print style location within warehouse.
*: Module        : Aria Apparel Series.
*: Developer     : Adel Mohhamed El Gazzar (ADEL)
*:************************************************************************
FUNCTION lfPrnLoc
PRIVATE lnPos , lnNo

IF SEEK(lcSTYLE+SPACE(6)+lcWareCode,'WHSLOC')
  lnOldSel = SELECT()
  SELECT WHSLOC
  lnPos = 06
  lnNo  = 00
  SCAN REST WHILE style+color+cwarecode+clocation = lcSTYLE+SPACE(6)+lcWareCode
    @ ROW,lnPos SAY WHSLOC.CLOCATION
    lnNo  = lnNo + 1 
    lnPos = lnPos + 11
    IF lnNo = 6
      ROW = ROW + 1
      lnPos = 06
      lnNo  = 00
    ENDIF
  ENDSCAN
  SELECT (lnOldSel)
ENDIF

*:*************************************************************************
*: PROGRAM   : lfClear (C101209)
*: DESC.     : Clear Variable
*: MODULE    : Aria Apparel Series.
*: DATE      : 04/07/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfClear

STORE SPACE(03) TO lcWState,lcRep1
STORE SPACE(05) TO lcAccount,lcDept
STORE SPACE(06) TO lcInvoice,lcWareCode,lcPickTkt,lcOrder
STORE SPACE(07) TO lcStore
STORE SPACE(10) TO lcCustPo,lcApproval
STORE SPACE(11) TO lcDuns 
STORE SPACE(15) TO lcWCity ,lcShiVCode,lcTerVCode
STORE SPACE(25) TO lcSalesNam
STORE SPACE(30) TO lcDivLName,lcDivLName,lcNotes1,lcNotes2
*- Ship to and Bill to Addresses
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6
STORE  0  TO ROW,lnAmnt,lnMerch,lnFrei,lnDisc,lnMerch,lnGranTot,lnTotChg,lnPageNo

*!*************************************************************
*! Name      : lfPrnLnNot
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 04/07/1999
*! Purpose   : Printing the Order line notes.
*!*************************************************************
FUNCTION lfPrnLnNot
PRIVATE lnAlias, lnOldMemW, lnMemLins, lnNotLine,lnCurrRec,lnCurTag

lnAlias   = SELECT()
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
llQuit    = .T.
SET MEMOWIDTH TO 66
SELECT OrdLine
lnCurrRec = RECNO()
lnCurTag  = TAG()
SET ORDER TO TAG OrdLinSt
GOTO TOP
IF SEEK ('O'+lcOrder+lcStore+lcStyle,'OrdLine')
  lnMemLins = MEMLINES(OrdLine.NOTE_MEM)
  DO WHILE lnNotLine <= lnMemLins
    IF ROW >= 60
      =lfPriHdr()
    ENDIF
    IF !EMPTY(MLINE(NOTE_MEM,lnNotLine))
      @ ROW,00 SAY MLINE(NOTE_MEM,lnNotLine)
      ROW       = ROW + 1
    ENDIF
    lnNotLine = lnNotLine + 1
  ENDDO     
ENDIF
SET MEMOWIDTH TO lnOldMemW
SET ORDER TO lnCurTag
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Ahmed Amer (AHM)
*! Date      : 01/25/2000
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : Program code, OG When
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
*B803007,1 Suppress the empty lines in Comp. Address

FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
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
*-- end of lfAdrShift.
