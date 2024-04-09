*:***************************************************************************
*: Program file  : SOORCNNT
*: Program desc. : Customer Order confirmation Form based on Standard form B
*: For Report    : Order confirmation
*: System        : Aria 4XP
*: Module        : Sales Order (SO)
*: Developer     : Ahmed Salah shalaby(SSH)
*: Ticket NO     : T20070216.0012
*: Track  NO     : C200757
*: Date          : 05/MArch/2007
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  :..
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOORCNNT
*:***************************************************************************
*: Modifications:
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[T20070619.0025]
*: B608568,1 Mos 05/22/2008 fix bug of direct to store orders printing multiple times. 
*: C201019,1 AWD 06/22/2008 Calculate total cube of cartons according to inputs at Style screen> Packing tab:Master Pack.;
                            Calculate total weight of styles according to weight entered at Style screen> Packing tab: Master Pack  
*:***************************************************************************

*:***************************************************************************


*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
*IF llOGFltCh
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[END]
  llDonprnt=.F.
  lcRpExp=loogscroll.lcRpExp + IIF(lcRpEDIFlt='O',[ AND !ORDHDR.lEDIOrder],;
							IIF(lcRpEDIFlt='E',[ AND ORDHDR.lEDIOrder],""))
  *-- If No records selected.
  SELECT ORDHDR
  SET SKIP TO
  LOCATE FOR &lcRpExp
  IF !FOUND()
    llDonprnt=.T.
    *--No records to display.
    = gfModalGen('TRM00052B00000','DIALOG' )
    RETURN
  ELSE
    llNoRec = .F.
  ENDIF
  *IF FILE('&oAriaApplication.DataDir.SOORCNZ.MEM')
  IF FILE(oAriaApplication.DataDir+'SOORCNZ.MEM')
    RESTORE FROM (oAriaApplication.DataDir+'SOORCNZ.MEM') ADDITIVE
  ELSE
    lcDevice = SET('DEVICE')
    *--Would you like to print the report by Pack_Id ?
    lcMpack  = IIF(gfModalGen('TRM32063B32000','DIALOG' )=1,'Y','N')
  ENDIF
  llPack = (lcMpack = 'Y')
  SAVE ALL LIKE lcM* TO (oAriaApplication.DataDir+'SOORCNZ.MEM')
  RELEASE ALL LIKE lcM*
  *--Open needed files.
  =gfOpenFile(oAriaApplication.DataDir+'Spck_Lin',oAriaApplication.DataDir+'Spck_Lin','SH')
  =gfOpenFile(oAriaApplication.DataDir+'Spck_Hdr',oAriaApplication.DataDir+'Spck_Hdr','SH')
  =gfOpenFile(oAriaApplication.DataDir+'SkuTmpl',oAriaApplication.DataDir+'SkuTmpl','SH')
  *--Initilize variables
  DIMENSION laAddress[1]
  STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6,laAddress
  STORE '' TO lcOrder,lcAccount,lcMulti,lcStore,laAddress,lcTermData,lcShipVia,lcDivison
  STORE ' ' TO lcSpcInst,lcSeason,lcPrePack,XMER_DISC,lcScale,lcPrevScale
  STORE '' To lcDcStore
  llNrmPrt = .T.
  DECLARE laDivLName[1,2]
  laDivLName[1,1] = 'CDIVISION'
  laDivLName[1,2] = 'lcDivLName'
  *-- Get the style and color
  lcMajTitl  = ALLTRIM(gfItemMask('HM'))
  lcNonMajTl = ''
  =lfGetColor()
  lcRpTmpMain=loOgScroll.gfTempName()
  lcRpTmpLines=loOgScroll.gfTempName()
  =lfBuildTmp()
  SELECT (lcRpTmpMain)
  INDEX on order+store TAG (lcRpTmpMain)
  SELECT (lcRpTmpLines)
  *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
  IF lcRPSortby = 'S'
    INDEX ON Order+store+Style TAG (lcRpTmpLines)
  ELSE
  *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]
  INDEX ON Order+store+cline TAG (lcRpTmpLines)
  *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
  ENDIF 
  *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]
  STORE 1 TO lnNoteLn,lnSvNoteLn 
  lcMulTOrDT = loOgScroll.gfTempName()
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET DEVICE TO PRINT
  *------------------------------
  * SECTION: MAIN LOOP
  *------------------------------
  SELECT ORDHDR
  *--Refresh the relation.
  GO TOP
  SCAN REST WHILE INKEY()<> 32 FOR &lcRpExp
    WAIT WINDOW 'PRINTING - <Space Bar> TO ABORT' NOWAIT
    =lfGetOdHdr()
    SELECT OrdHdr
    =lfPrintHdr()
    SET ORDER TO TAG ORDLINE IN ORDLINE
    IF lcMulti = 'Y'
      IF !USED(lcMulTOrDT)
        =gfOpenFile(oAriaApplication.DataDir+lcMulTOrDT,'','EX')
      ELSE
        SELECT (lcMulTOrDT)
      ENDIF  
    ELSE
      SELECT ORDLINE
      SET ORDER TO TAG IIF(lcRpSortBy = 'L' , 'ORDLINE' , 'ORDLINST' ) IN ORDLINE
      LOCATE
      =SEEK(ORDHDR.cOrdType+lcOrder)
    ENDIF
    lnLWeight = 0
    lnCartons = 0
    *:C201019,1 AWD 06/22/2008 Calculate total cube of cartons[Start]
		lnTotCube  = 0 
   	*: C201019,1 AWD 06/22/2008 Calculate total cube of cartons[End]

    
    SCAN WHILE EVAL(KEY()) = ORDHDR.cOrdType+lcOrder FOR IIF(lcRpBook = 'N',TOTQTY > 0,TOTQTY > 0 OR TOTQTY = 0 )

      IF lcMulti # 'Y' OR &lcMulTOrDT..Store = lcStore  &&this is the reverse of the second case
      ELSE
        =lfGetStAd()
        =lfPrintHdr()
        
        *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
        lnLWeight = 0
        lnCartons = 0
        *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[END]
        *:C201019,1 AWD 06/22/2008 Calculate total cube of cartons[Start]
		    lnTotCube  = 0 
   			*: C201019,1 AWD 06/22/2008 Calculate total cube of cartons[End]

        
      ENDIF  && lcMulti # 'Y' OR &lcMulTOrDT..Store = lcStore  
      
      *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
      m.stname = lcStName 
      m.stadd1 = lcStAddr1 
      m.stadd2 = lcStAddr2 
      m.stadd3 = lcStAddr3 
      *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]
      
      =SEEK(IIF(lcMulti = 'Y' , &lcMulTOrDT..Style,Ordline.Style),'STYLE' )
      lcStyDesc  = STYLE.Desc
      IF lcMulti = 'Y'
        =SEEK(ORDHDR.cOrdType+Order+STR(LineNo,6),'OrdLine')
      ENDIF
      lcPackKey = 'P'+lcAccount+OrdLine.Pack_Id
      IF !SEEK('P'+lcAccount+OrdLine.Pack_Id+Style,'Spck_Lin')
        =SEEK('P'+'*****'+OrdLine.Pack_Id+Style,'Spck_Lin')
        lcPackKey = 'P'+'*****'+OrdLine.Pack_Id
      ENDIF
      IF !SEEK('P'+lcAccount+OrdLine.Pack_Id,'Spck_Hdr')
        =SEEK('P'+'*****'+OrdLine.Pack_Id,'Spck_Hdr')
      ENDIF
      IF OrdLine.Pack_Id <> lcPrePack .AND. llPack
        lcOldAlias = ALIAS()
        SELECT Spck_Lin
        SUM REST WHILE Type+Account+Pack_Id+Style = lcPackKey ;
                       Spck_Lin.Qty1,Spck_Lin.Qty2,Spck_Lin.Qty3,;
                       Spck_Lin.Qty4,Spck_Lin.Qty5,Spck_Lin.Qty6 TO ARRAY lnQty
        SELECT (lcOldAlias)
        lcPrePack = OrdLine.Pack_Id
        IF !EMPTY(OrdLine.Pack_Id)
          M.PACKID= 'PACK_ID : '+OrdLine.Pack_Id+'  '+'DESC.:'+Spck_Hdr.Desc+' ' +;
                       +STR(lnQty[1],3)+' '+ STR(lnQty[2],3)+' '+STR(lnQty[3],3)+' ' +;
                       STR(lnQty[4],3)+' '+STR(lnQty[5],3)+' '+STR(lnQty[6],3)
        ENDIF 
      ENDIF
      M.NoteMem=''
      IF llRpOrdLnt
        M.NoteMem=ALLTRIM(Note_Mem)
      ENDIF 
      M.NLINE= LineNo
      M.CLINE=RIGHT('000'+ ALLTRIM(STR(LineNo)),4)
      M.GROUP= Group
      M.STYLE= LEFT(Style,lnMajorLen)
      M.COLOR=RIGHT(STYLE,19-lnMajorLen-1 )
      M.DESC=lcStyDesc
      M.PRICE=Price
      M.PIECES= TotQty
      M.STORE=LCSTORE
      M.Amount= TotQty * Price
      M.TOTBOOK=TOTBOOK
      FOR lnI =1 TO 8
        lcI=ALLTRIM(STR(lnI))
        m.BOOK&lcI=BOOK&lcI
        m.QTY&lcI=QTY&lcI
      ENDFOR 
      =SEEK('S'+SCALE,'SCALE') 
      FOR lnI =1 TO 8
        lcI=ALLTRIM(STR(lnI))
        m.scale&lcI=scale.sz&lcI
      ENDFOR 
      M.SC_DESC=SCALE.SCALE
      *-- Print the order line notes.
      INSERT INTO (lcRpTmpLines)FROM MEMVAR
      *SSH
      *Update Cartons
      IF Style.Qty_Ctn <> 0
        lnCartons = lnCartons+(CEILING(TotQty/Style.Qty_Ctn))
      ELSE
        lnCartons = 0
  *: C201019,1 AWD 06/22/2008 Calculate total weight of styles according to weight entered at Style screen> Packing: master pack[Start]
        lnTotcube=0
  *: C201019,1 AWD 06/22/2008 Calculate total weight of styles according to weight entered at Style screen> Packing: master pack[End]
      ENDIF
     
     *: C201019,1 AWD 06/22/2008 Calculate total weight of styles according to weight entered at Style screen> Packing: master pack[Start]
      
 
*!*	      lnWeight  = Style.nstyweight
*!*	      
*!*	      lnLWeight = lnLWeight+(lnWeight*TotQty)
*!*	      
   *: C201019,1 AWD 06/22/2008 Calculate total weight of styles according to weight entered at Style screen> Packing: master pack[Start]

 
      SELECT (lcRpTmpLines)
      *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
      * REPLACE nCartons WITH lnCartons,;
              Qty_Cart WITH Style.Qty_Ctn
      
      REPLACE nCartons WITH IIF(Style.Qty_Ctn <> 0,(CEILING(M.PIECES/Style.Qty_Ctn)),0),;
              Qty_Cart WITH Style.Qty_Ctn
      *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]
      
       *: C201019,1 AWD 06/22/2008 Calculate total cube of cartons[Start]
     IF Style.nmspackhgt <> 0 AND Style.nmspacklen <> 0 AND Style.nmspackwdt <> 0
          lnTotCube =lnTotCube +(((Style.nmspackhgt  *  Style.nmspacklen  *  Style.nmspackwdt)/1728 )*nCartons)
          lnWeight  = Style.nmspackwgt
          lnLWeight = lnLWeight+(lnWeight*nCartons)
     ENDIF

  
*!*	      REPLACE cStrToPrn WITH lfPrtSku(OrdLine.Style,ORDHDR.Account),;
*!*	              TotCart   WITH TotCart+lnCartons,;
*!*	              TotWeight WITH TotWeight+(lnLWeight),;                

			REPLACE cStrToPrn WITH lfPrtSku(OrdLine.Style,ORDHDR.Account),;
              TotCart   WITH TotCart+lnCartons,;
              TotWeight WITH TotWeight+(lnLWeight),;                
              TotCube   WITH TotCube+(lnTotCube)
  *: C201019,1 AWD 06/22/2008 Calculate total cube of cartons[End]
  
  
      *Update Cartons
      *SSH
    
    
     ENDSCAN
  ENDSCAN
  SELECT (lcRpTmpMain)
  IF !RECCOUNT()>0
    llDonprnt=.T.
    *-- Message : There are no records to display...!
    *--                < Ok > 
      =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF 
  *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
  lcDunsNo = ""
  lcFax = ''
  lcPhon = ''
  lcStName  = ""
  lcStAddr1 = ""
  lcStAddr2 = ""
  lcStAddr3 = ""
  lcStAddr4 = ""
  lcStAddr5 = ""
  *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]
  
  =lfAdjustCRSettings()
  *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
  *!*	  IF USED(lcRpTmpMain)
  *!*	    USE IN (lcRpTmpMain)
  *!*	  ENDIF
  *!*	  IF USED(lcRpTmpLines)
  *!*	    USE IN (lcRpTmpLines)
  *!*	  ENDIF
  SELECT(lcRpTmpMain)
 
 *!B608568,1 Mos 05/22/2008 fix bug of direct to store orders printing multiple times.[start] 
  *SET RELATION TO order INTO (lcRpTmpLines)
   SET RELATION TO order+ STORE INTO (lcRpTmpLines)
 *!B608568,1 Mos 05/22/2008 fix bug of direct to store orders printing multiple times. [end]
 
  SET SKIP TO (lcRpTmpLines)
    *: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]
  =gfDispRe()
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
*!*	ELSE
*!*	  IF llDonprnt
*!*	    *-- Message : There are no records to display...!
*!*	    *--                < Ok > 
*!*	    =gfModalGen('TRM00052B40011','ALERT')
*!*	    RETURN
*!*	  ELSE

*!*	    =gfDispRe()
*!*	  ENDIF  
*!*	ENDIF  &&FILTER CHANGE
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]

*!*************************************************************
*! Name      : lfGetOdHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/24/99
*! Purpose   : TO get the information of the ORDER header.
*! Refer to  : (C101403)
*!*************************************************************
FUNCTION lfGetOdHdr
PRIVATE lcAlias , lcCurrKey
lcCurrKey = ""
lcAlias = ALIAS()
SELECT ORDHDR
lcOrder   = Order
lcAccount = Account
lcMulti   = Multi
IF lcMulti = 'Y'
  IF USED(lcMulTOrDT)
    SELECT (lcMulTOrDT)
    USE
  ENDIF
  SELECT ORDLINE
  =SEEK(ordhdr.cOrdtype+lcOrder)  
  COPY REST TO (oAriaApplication.WorkDir+lcMulTOrDT) WHILE cordtype+order+STR(lineno,6) = ordhdr.cOrdtype+lcOrder
  =gfOpenFile(oAriaApplication.WorkDir+lcMulTOrDT,'','EX')
  IF lcRpSortBy = 'S'
    IF llPack
      INDEX ON cOrdType+Order+Store+Style+STR(LineNo,6)+Pack_Id TAG (lcMulTOrDT)
    ELSE 
      INDEX ON cOrdType+Order+Store+Style+STR(LineNo,6) TAG (lcMulTOrDT)
    ENDIF
  ELSE
    INDEX ON CORDTYPE+ORDER+STORE+STR(LINENO,6) TAG (lcMulTOrDT)
  ENDIF
  GO TOP  
  lcStore = Store
  =SEEK('S'+lcAccount+lcStore,'CUSTOMER')  
ENDIF
lcStore = IIF(lcMulti <> 'Y',ORDHDR.STORE,lcStore)
IF lcMulti = 'Y'
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcDCStore = lcStore + ' DC:' + Customer.Store
  ELSE
    lcDcStore = lcStore 
  ENDIF
  IF !EMPTY(lcCurrKey)
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF
ELSE
  lcDcStore = lcStore 
ENDIF  

STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
lcBtName  = CUSTOMER.BTNAME
=gfGetAdr('Customer','','','',1,'2')
*--Get the Bill To adddess except the country.
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAddr&lcCount = lcBtAddr&lcCount + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
  ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
ENDFOR
*-- SHIP_TO ADDRESS FOR THIS STORE
STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
IF OrdHdr.Alt_ShpTo
  lcStName  = ALLTRIM(OrdHdr.STNAME)
  lcStAddr1 = ALLTRIM(OrdHdr.cAddress1)
  lcStAddr2 = ALLTRIM(OrdHdr.cAddress2)
  lcStAddr3 = ALLTRIM(OrdHdr.cAddress3)+','+ALLTRIM(OrdHdr.cAddress4)+','+ALLTRIM(OrdHdr.cAddress5)
ELSE
    IF !EMPTY(CUSTOMER.DIST_CTR)  
      lcCurrKey = 'S' + Customer.Account + Customer.Store
      =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    ENDIF
    lcStName  = ALLTRIM(IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA))
    lcStAddr1 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
    lcStAddr2 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
    lcStAddr3 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
    lcStAddr4 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
    lcStAddr5 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5))
    IF !EMPTY(lcCurrKey)
      = SEEK(lcCurrKey , 'CUSTOMER')
    ENDIF
ENDIF  
*-- GET DESCRIPTIONS FOR CODES FIELDS
SELECT CODES
*--Terms
lcTermData =SUBSTR(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),1,30)
*--ShipVia

IF lcMulti = 'Y'

*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
*!*	  IF ALLTRIM(ORDHDR.ShipVia) = '*'
*!*	    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
*!*	  ELSE
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]

    lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,30)
    
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
*  ENDIF  
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]
ELSE
  lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,30)
ENDIF
*--Division long name.
STORE '' TO lcDivLName
=gfRltFld(OrdHdr.cDivision,@laDivLName,'CDIVISION')
*--Special instruction
lcSpcInst = SUBSTR(gfCodDes(OrdHdr.SPCINST , 'SPCINST'),1,30)
*---Season
lcSeason = SUBSTR(gfCodDes(OrdHdr.SEASON , 'SEASON'),1,30)
*--Get the first line of the company address.

SELECT (lcAlias)

*!*************************************************************
*! Name      : lfPrintHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/24/99
*! Purpose   : TO print the information of the ORDER header.
*! Refer to  : (C101403)
*!*************************************************************
FUNCTION lfPrintHdr
PRIVATE lcAlias

lcAlias = ALIAS()

SELECT OrdHdr
M.ORDER=Order
M.ENTERED=ENTERED
M.STORE= lcStore

M.CUSTPO=IIF(MultiPO,&lcMulTOrDT..CUSTPO,CUSTPO) 
M.CNOTE=NOTE1+"    "+NOTE2

M.MESSAGE1=lcRpMsg1
M.MESSAGE2=lcRpMsg2
M.MESSAGE3=lcRpMsg3

M.ACCOUNT= Account
M.DCSTORE= lcDCStore 
M.DEPT= Dept 
M.BTNAME= lcBtName

M.BTADD1=lcBtAddr1
IF EMPTY(M.BTADD1)
  M.BTADD1=lcBtAddr2
  M.BTADD2=""
ELSE
  M.BTADD2=lcBtAddr2
ENDIF  

IF EMPTY(M.BTADD1)
  M.BTADD1=lcBtAddr3
  M.BTADD3=""
ELSE
  IF EMPTY(M.BTADD2)
   M.BTADD2=lcBtAddr3
   M.BTADD3=""
  ELSE
   M.BTADD3=lcBtAddr3
  ENDIF
ENDIF

M.STNAME=lcStName

M.STADD1=lcStAddr1
IF EMPTY(M.STADD1)
  M.STADD1=lcStAddr2
  M.STADD2=""
ELSE
  M.STADD2=lcStAddr2
ENDIF  

IF EMPTY(M.STADD1)
  M.STADD1=lcStAddr3
  M.STADD3=""
ELSE
  IF EMPTY(M.STADD2)
   M.STADD2=lcStAddr3
   M.STADD3=""
  ELSE
   M.STADD3=lcStAddr3
  ENDIF
ENDIF

M.START=START
M.COMPLETE= COMPLETE
M.CTERM =lcTermData  
M.CSHIP = SUBSTR(lcShipVia,1,15)
M.CSEASON= lcSeason
M.REP1=REP1
M.REP2=REP2
M.Disc=Disc
M.NOTE=''
IF !EMPTY(Note1) .AND. SUBSTR(Note1,1,1) <> '*' 
  M.NOTE=M.NOTE+  Note1
ENDIF       
IF !EMPTY(Note2) .AND. SUBSTR(Note2,1,1) <> '*' 
  M.NOTE=M.NOTE+  Note1
ENDIF  
m.mNotepad=''     
IF llRpOrdNot AND SEEK( 'B'+lcOrder,'NotePad')
  m.mNotepad=NotePad.MNOTES
ENDIF 

*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
m.MultiSt = lcMulti
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]

INSERT INTO (lcRpTmpMain)FROM MEMVAR

SELECT (lcAlias)

*!*************************************************************
*! Name      : lfGetStAd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/24/99
*! Purpose   : TO get the store address.
*! Refer to  : (C101403)
*!*************************************************************
FUNCTION lfGetStAd
PRIVATE lcAlias , lcCurrKey
lcCurrKey = ""

lcAlias = ALIAS()
lcStore   = Store
lcAccount = Account
=SEEK('S'+lcAccount+lcStore,'CUSTOMER')    
STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6

*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
*!*	IF ALLTRIM(ORDHDR.ShipVia) = '*'
*!*	  lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
*!*	ELSE
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]

  lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)

*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
*ENDIF 
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]

* Print distribution center address if found 
IF lcMulti = 'Y'
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcDCStore = lcStore + ' DC:' + Customer.Store
  ELSE
    lcDcStore = lcStore 
  ENDIF
  IF !EMPTY(lcCurrKey)
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF
ELSE
  lcDcStore = lcStore 
ENDIF  

lcBtName  = CUSTOMER.BTNAME
=gfGetAdr('Customer','','','',1,'2')
*--Get the Bill To adddess except the country.
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAddr&lcCount = lcBtAddr&lcCount + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
  ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
ENDFOR


IF !EMPTY(CUSTOMER.DIST_CTR)  
  lcCurrKey = 'S' + Customer.Account + Customer.Store
  =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
ENDIF

lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
lcStAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
lcStAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
lcStAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
lcStAddr4 = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
lcStAddr5 = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

IF !EMPTY(lcCurrKey)
  = SEEK(lcCurrKey , 'CUSTOMER')
ENDIF  

SELECT (lcAlias)


*!*************************************************************
*! Name      : lfGetColor
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! DATE      : 02/23/99
*! Purpose   : To get the length to of the color.
*! Refer To  : (C101403)
*!*************************************************************
FUNCTION lfGetColor

lcNonMajTl = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR



*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[2]
DIMENSION loOgScroll.laCRParams[15,2]

loOgScroll.lcOGLastForm ='SOORCNNT'
loOGScroll.cCROrientation='P'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcRpTmpMain+ ".DBF"
loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcRpTmpLines+ ".DBF"

  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'ORDER CONFIRMATION'

loOgScroll.laCRParams[2,1] = 'lcEdiOrder'
IF lcRpOrdTyp = 'T'
  loOgScroll.laCRParams[2,2]= 'Y'
ELSE
  loOgScroll.laCRParams[2,2]= 'N'
ENDIF 


loOgScroll.laCRParams[3,1] = 'lcBOOK'
IF lcRpBOOK = 'N'
  loOgScroll.laCRParams[3,2]= 'Y'
ELSE
  loOgScroll.laCRParams[3,2]= 'N'
ENDIF 

loOgScroll.laCRParams[4,1] = 'lCDEC'
IF lcRpDeciml = "Y"
   loOgScroll.laCRParams[4,2]= 'Y'
ELSE
  loOgScroll.laCRParams[4,2]= 'N'
ENDIF 
lcPhonPict = gfPhoneTem()  

loOgScroll.laCRParams[5,1] = 'lCADD2'
loOgScroll.laCRParams[6,1] = 'lCADD3'
loOgScroll.laCRParams[7,1] = 'lCADD4'
loOgScroll.laCRParams[10,1] = 'lCADD1'
loOgScroll.laCRParams[13,1] = 'lCADD5'


IF EMPTY(lcDivLName)
 loOgScroll.laCRParams[5,2] = laCompAdd[2]
 loOgScroll.laCRParams[6,2] = laCompAdd[3]
 loOgScroll.laCRParams[7,2] = laCompAdd[4]
 loOgScroll.laCRParams[10,2] = laCompAdd[1]
 loOgScroll.laCRParams[13,2] = ''
ELSE
 loOgScroll.laCRParams[5,2] = laCompAdd[1]
 loOgScroll.laCRParams[6,2] = laCompAdd[2]
 loOgScroll.laCRParams[7,2] = laCompAdd[3]
 loOgScroll.laCRParams[13,2] = laCompAdd[4]
 loOgScroll.laCRParams[10,2] = lcDivLName
ENDIF 



loOgScroll.laCRParams[8,1] = 'lCFAX'
loOgScroll.laCRParams[8,2] = TRANSFORM(SyCcomp.Ccom_Fax,'@R '+ lcPhonPict)

*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
lcFax = loOgScroll.laCRParams[8,2] 
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]



loOgScroll.laCRParams[9,1] = 'lCPHON'
loOgScroll.laCRParams[9,2] = TRANSFORM(SyCcomp.Ccom_phon,'@R '+ lcPhonPict)

*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
lcPhon = loOgScroll.laCRParams[9,2] 
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]


loOgScroll.laCRParams[11,1] = 'lCNOTLN'
IF llRpOrdLnt
   loOgScroll.laCRParams[11,2]= 'Y'
ELSE
  loOgScroll.laCRParams[11,2]= 'N'
ENDIF 

loOgScroll.laCRParams[12,1] = 'lCPACK'
IF LLPACK
   loOgScroll.laCRParams[12,2]= 'Y'
ELSE
  loOgScroll.laCRParams[12,2]= 'N'
ENDIF 

loOgScroll.laCRParams[14,1] = "lcDunsNo"
loOgScroll.laCRParams[14,2] = ALLTRIM(gfGetMemVar('XDUNS'))

*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
lcDunsNo = loOgScroll.laCRParams[14,2] 
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]

loOgScroll.laCRParams[15,1] = "lcRpSortBy"
loOgScroll.laCRParams[15,2] = lcRpSortBy

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn


*!*************************************************************
*! Name      : lfStr2Curs 
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs 
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString





*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[29,18]
STORE '' TO laTempStru
STORE 0 TO lnIndex

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ORDER'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STORE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 8
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CUSTPO'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ENTERED'
laTempStru[lnIndex,2] = 'D'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ACCOUNT'
laTempStru[lnIndex,2] = 'C'

*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
*laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,3] = 5
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]

laTempStru[lnIndex,4] = 0
*5
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'DCSTORE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'DEPT'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'BTNAME'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'BTADD1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'BTADD2'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0
*10
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'BTADD3'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STNAME' 
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STADD1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STADD2'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STADD3'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0
*15
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'START'
laTempStru[lnIndex,2] = 'D'
laTempStru[lnIndex,3] = 8
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'COMPLETE'
laTempStru[lnIndex,2] = 'D'
laTempStru[lnIndex,3] = 8
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CTERM'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CSHIP'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CSEASON'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0
*20
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'REP1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 3
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'REP2'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 3
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CNOTE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 125
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'Disc'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 2

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'mNotepad'
laTempStru[lnIndex,2] = 'M'
laTempStru[lnIndex,3] = 0
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'MESSAGE1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 125
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'MESSAGE2'

laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 125
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'MESSAGE3'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 125
laTempStru[lnIndex,4] = 0

*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'MultiSt'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 1
laTempStru[lnIndex,4] = 0
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]

*Create Temp*
=gfCrtTmp(lcRpTmpMain,@laTempstru,,"",.f.)

*MMT
*DIMENSION laTempStru[43,18]
*IMENSION laTempStru[47,18]
*MMT
*: C201019,1 AWD 06/22/2008 Calculate total cube of cartons[Start]
*DIMENSION laTempStru[47,18]
DIMENSION laTempStru[48,18]
*: C201019,1 AWD 06/22/2008 Calculate total cube of cartons[End]


STORE '' TO laTempStru
STORE 0 TO lnIndex
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ORDER'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'GROUP'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'COLOR'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STYLE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 19
laTempStru[lnIndex,4] = 0

FOR lnI=1 TO 8
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'QTY'+ALLTRIM(STR(lnI))
  laTempStru[lnIndex,2] = 'N'
  laTempStru[lnIndex,3] = 10
  laTempStru[lnIndex,4] = 2
ENDFOR 

FOR lnI=1 TO 8
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'BOOK'+ALLTRIM(STR(lnI))
  laTempStru[lnIndex,2] = 'N'
  laTempStru[lnIndex,3] = 10
  laTempStru[lnIndex,4] = 2
ENDFOR 

FOR lnI=1 TO 8
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'SCALE'+ALLTRIM(STR(lnI))
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 6
  laTempStru[lnIndex,4] = 0
ENDFOR 

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'TOTBOOK'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'NoteMem'
laTempStru[lnIndex,2] = 'M'

*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
*laTempStru[lnIndex,3] = 0
laTempStru[lnIndex,3] = 10
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'PRICE'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 7
laTempStru[lnIndex,4] = 2

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'PIECES'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'NLINE'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 4
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CLINE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 4
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'PACKID'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 244
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STORE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 8
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'DESC'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 50
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'SC_DESC'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 5
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'nCartons'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 5
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'Qty_Cart'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 5
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'cStrToPrn'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 65
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'TotCart'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'TotWeight'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2


*: C201019,1 AWD 06/22/2008 Calculate total cube of cartons[Start]
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'TotCube'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2
*: C201019,1 AWD 06/22/2008 Calculate total cube of cartons[End]


*MMT
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STNAME' 
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STADD1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STADD2'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STADD3'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0
*MMT

*Create Temp*


=gfCrtTmp(lcRpTmpLines,@laTempstru,,"",.f.)





*!*************************************************************
*! Name      : lfPrtSku
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/26/2001
*! Purpose   : Get the SKU number of the Account/Style/Color.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrtSku()
*!*************************************************************
FUNCTION lfPrtSku
LPARAMETERS lcStyKey,xAccount
PRIVATE lcStrToPrn,lnAlias
lnAlias=SELECT(0)
SELECT Spck_Lin
lcOrd=SET('ORDER')
SET ORDER TO TAG SPCKLINS
IF !SEEK('S'+xAccount+lcStyKey,'Spck_Lin')
  lcStrToPrn = " "
  SET ORDER TO &lcOrd
  SELECT (lnAlias)
  RETURN ""
ENDIF
= SEEK('M'+xAccount,'Customer')
lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
  lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
  lnDime2 = SkuTmpl.Len4
ELSE
  lnDime1 = 8  &&Default
  lnDime2 = 8  &&Default
ENDIF 
lcStrToPrn = 'SKU#: ' + SUBSTR(Spck_Lin.Pack_Id,1,lnDime1)
lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
SET ORDER TO &lcOrd
SELECT (lnAlias)
RETURN(lcStrToPrn)

*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[Start]
*************************************************************
*! Name      : lflastline
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/03/2007
*! Purpose   : function to reset last line var.
*!*************************************************************
FUNCTION lflastline
PARAMETERS llastline
llastline = .T.
RETURN .T.

*************************************************************
*! Name      : lfrlastline
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/03/2007
*! Purpose   : function to deteremine if last line or not
*!*************************************************************
FUNCTION lfrlastline
PARAMETERS llastline
llastline = .F.
RETURN .T.

*************************************************************
*! Name      : lfEndreset
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/03/2007
*! Purpose   : function to reset
*!*************************************************************
FUNCTION lfEndreset
PARAMETERS lendl
lendl = .F.
RETURN .T.

*************************************************************
*! Name      : lfEndLine
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/03/2007
*! Purpose   : function to deteremine if last record in group or not
*!*************************************************************
FUNCTION lfEndLine
PARAMETERS lendl

lnOldAlias = SELECT(0)
SELECT(lcRpTmpLines)
lcoldordStr = order+store
IF !EOF()
  SKIP 1
ENDIF 
IF lcoldordStr = order+store
  lendl = .F.
ELSE
  lendl = .T.  
ENDIF 
IF !BOF()
  SKIP -1 
ENDIF 
SELECT (lnOldAlias)
RETURN .T.

*************************************************************
*! Name      : lfRvar
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/03/2007
*! Purpose   : function to reset rep. variables
*!*************************************************************
FUNCTION lfRvar
PARAMETERS totamt,totpcs
totpcs = 0 
totamt = 0
RETURN .T.

*************************************************************
*! Name      : lfSumGrp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/03/2007
*! Purpose   : function to sum the total pcs
*!*************************************************************
FUNCTION lfSumGrp
PARAMETERS totamt,totpcs
lnOldAlias = SELECT(0)
SELECT(lcRpTmpLines)
lcoldordStr = order+store
lnRecOld = RECNO()
SUM pieces * price ,pieces  TO totamt,totpcs FOR order+store = lcoldordStr 
IF BETWEEN(lnRecOld,1,RECCOUNT())
  GO lnRecOld 
ENDIF
SELECT(lnOldAlias )  
RETURN .T.

*************************************************************
*! Name      : lfGetAddr
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/03/2007
*! Purpose   : function to get ship to address
*!*************************************************************
FUNCTION lfGetAddr
PRIVATE lcAlias , lcCurrKey

lcAlias = ALIAS()
lcStore   = &lcRpTmpLines..Store
lcAccount = &lcRpTmpMain..Account



=SEEK('S'+lcAccount+lcStore ,'CUSTOMER')

* Print distribution center address if found 
IF &lcRpTmpMain..MultiSt = 'Y'
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcDCStore = lcStore + ' DC:' + Customer.Store
  ELSE
    lcDcStore = lcStore 
  ENDIF
ELSE
  lcDcStore = lcStore 
ENDIF  


IF !EMPTY(CUSTOMER.DIST_CTR)  
  lcCurrKey = 'S' + Customer.Account + Customer.Store
  =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
ENDIF

lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
lcStAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
lcStAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
lcStAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
lcStAddr4 = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
lcStAddr5 = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

SELECT (lcAlias)

RETURN .T.
*: B608147,1 MMT 07/02/2007 fix bug of converting report to text instead of crystal[End]


