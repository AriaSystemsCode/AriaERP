*:***************************************************************************
*: Program file  : SOORCNHN
*: Program desc. : Customer Order confirmation Form HN
*: For Report    : Order confirmation
*: System        : Aria 4XP
*: Module        : Sales Order (SO)
*: Developer     : Mariam MAzhar(MMT)
*: Ticket NO     : T20080829.0007 
*: Track  NO     : C201056
*: Date          : 10/23/2008
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
*: B609126,1 WAM 01/12/2010 Get the proper customer record before getting the address [T20100111.0015]
*:***************************************************************************
PRIVATE lcMajTitle , lcNonMajTl
lcMajTitle = ALLTRIM(gfItemMask('HM'))      && get Major title
lcNonMajTl = ALLTRIM(gfItemMask('HN'))      && get Non Major title
LNTOTalORD = 0
LNTOTORD = 0
lnTOTalBok = 0
lnTOTBok = 0
LNTOTQTy = 0
LNTOTalQTy = 0
SELECT OrdHdr
SET SKIP TO
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  llNoRec = .T.
  RETURN
ENDIF


lcTempHdr = loogscroll.gfTempName()
SELECT OrdHdr
SET RELATION OFF INTO (lcTempOrd)

SELECT Ordhdr
DIMENSION laHdrStru[1,18]
AFIELDS(laHdrStru)
=gfCrtTmp(lcTempHdr ,@laHdrStru,"CORDTYPE+ORDER",lcTempHdr)


SELECT (lcTempOrd)
LOCATE
SCAN FOR !SEEK(CORDTYPE+ORDER,lcTempHdr)
  SELECT OrdHdr
  SEEK(&lcTempOrd..CORDTYPE + &lcTempOrd..Order)
  SCATTER MEMO MEMVAR 
  IF !SEEK(m.CORDTYPE+m.ORDER,lcTempHdr)
    INSERT INTO (lcTempHdr) FROM MEMVAR 
  ENDIF   
ENDSCAN 
SELECT (lcTempHdr) 

SET RELATION TO CORDTYPE + ORDER INTO (lcTempOrd)
SET SKIP TO (lcTempOrd)
LOCATE 
IF RECCOUNT(lcTempOrd) = 0
  RETURN .F.
ENDIF 
llLastLine = .F.
DO gfDispRe WITH EVAL('lcFormName') 
SELECT (lcTempHdr) 
SET RELATION TO 
*!***************************************************************************
*! Name      : lfGetOdHdr
*! Developer : Mariam Mazhar(MMT)
*! Date      : 10/23/2008
*! Purpose   : Function to get the information of the ORDER header.
*!***************************************************************************
*! Called from : None.
*!***************************************************************************
*! Calls       : None.
*!***************************************************************************
*! Passed Parameters : None.
*!***************************************************************************
*! Return            : None
*!***************************************************************************
*! Example           : = lfGetOdHdr()
*!***************************************************************************
*
FUNCTION lfGetOdHdr
PRIVATE lcAlias , lcCurrKey
lcAlias = ALIAS()
lcCurrKey = ""


lcOrder = &lcTempOrd..Order
lcMulti   = &lcTempHdr..Multi
lcAccount = &lcTempHdr..Account
SELECT (lcTempOrd)
lcStore = IIF(lcMulti <> 'Y',&lcTempHdr..STORE,&lcTempOrd..Store)
STORE '' TO lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6

*-- GET DESCRIPTIONS FOR CODED FIELDS
SELECT CODES
*--Terms
lcTermData =SUBSTR(gfCodDes(&lcTempHdr..CTERMCODE , 'CTERMCODE'),1,15)
*--ShipVia
lcShipVia = SUBSTR(gfCodDes(&lcTempHdr..SHIPVIA , 'SHIPVIA'),1,15)
*--Division long name.
STORE '' TO lcDivLName

lcCurAlias = ALIAS()
SELECT Codes
SET ORDER TO TAG Codes
IF SEEK('N'+&lcTempHdr..cDivision+'Y'+'CDIVISION')
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+&lcTempHdr..cDivision+'Y'+'CDIVISION'
    IF crltd_nam = 'DIVLNAME  '
      lcDivLName = crltd_vlu
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcCurAlias)
*=gfRltFld(OrdHdr.cDivision,@laDivLName,'CDIVISION')

*--Special instruction
lcSpcInst = SUBSTR(gfCodDes(&lcTempHdr..SPCINST , 'SPCINST'),1,15)
*---Season
lcSeason = SUBSTR(gfCodDes(&lcTempHdr..SEASON , 'SEASON'),1,10)
*--Get the first line of the company address.
lcTerms   = gfCodDes(&lcTempHdr..CTERMCODE,'CTERMCODE')

*SELECT ORDHDR
*GOTO RECNO()

IF lcMulti = "Y" 
  SELECT CUSTOMER
  =SEEK('S'+ &lcTempOrd..Account+ &lcTempOrd..Store)

  *-- Get ShipVia at store level
  IF ALLTRIM(&lcTempHdr..ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ENDIF  

  lcSolTName  = CUSTOMER.BTNAME
  laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
  
  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF &lcTempHdr..Alt_ShpTo
    lcShptName  = &lcTempHdr..STNAME
    laShipTo[1] = &lcTempHdr..cAddress1
    laShipTo[2] = &lcTempHdr..cAddress2
    laShipTo[3] = &lcTempHdr..cAddress3+','+&lcTempHdr..cAddress4+','+&lcTempHdr..cAddress5
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3 
    ENDIF
  ELSE
    * Print distribution center address if found 
    IF !EMPTY(CUSTOMER.DIST_CTR)  
      lcCurrKey = 'S' + Customer.Account + Customer.Store
      =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
      lcStore = lcStore + '  Dist. Center : ' + Customer.Store
    ENDIF

    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
    laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
    laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

    IF !EMPTY(lcCurrKey)
      = SEEK(lcCurrKey , 'CUSTOMER')
    ENDIF
  ENDIF 
ELSE
  *B609126,1 WAM 01/12/2010 Get the proper customer record
  IF EMPTY(&lcTempOrd..Store)
    =SEEK('M'+ &lcTempOrd..Account,'CUSTOMER')
  ELSE
    =SEEK('S'+ &lcTempOrd..Account+ &lcTempOrd..Store,'CUSTOMER')
  ENDIF
  *B609126,1 WAM 01/12/2010 (End)

  lcSolTName  = CUSTOMER.BTNAME
  SELECT CUSTOMER


  *B607326,1 KHM 06/02/2003 (Begin) Getting the right sold to information
  *= gfGetAdr('CUSTOMER','','','',@laSoldTo,'')
  laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')  
  *B607326,1 KHM 06/02/2003 (End)
  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF &lcTempHdr..Alt_ShpTo
    lcShptName  = &lcTempHdr..STNAME
    laShipTo[1] = &lcTempHdr..cAddress1
    laShipTo[2] = &lcTempHdr..cAddress2
    laShipTo[3] = &lcTempHdr..cAddress3+','+&lcTempHdr..cAddress4+','+&lcTempHdr..cAddress5
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3 
    ENDIF
  ELSE
    * Print distribution center address if found 
    IF !EMPTY(CUSTOMER.DIST_CTR)  
      lcCurrKey = 'S' + Customer.Account + Customer.Store
      =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
      lcStore = lcStore + '  Dist. Center : ' + Customer.Store
    ENDIF

    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
    laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
    laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

    IF !EMPTY(lcCurrKey)
      = SEEK(lcCurrKey , 'CUSTOMER')
    ENDIF
  ENDIF 

ENDIF
= lfAdrShift('laShipTo')  && Shift Sold To address if there is empty line.
laCompAdd1 = IIF(!EMPTY(lcDivLName),lcDivLName,laCompAdd[1])
laCompAdd1 = SPACE(ROUND((80-LEN( ALLTRIM(laCompAdd1) ))/2,0))+laCompAdd1
SELECT (lcAlias)

*-- End OF lfGetOdHdr.
*!***************************************************************************
*! Name      : lfReLstLn
*! Developer : Mariam Mazhar(MMT)
*! Date      : 10/23/2008
*! Purpose   : Function to Rest Report Variables
*!***************************************************************************
FUNCTION lfReLstLn
llLastLine = .F.
llEndGroup = .F.                        
LNTOTalQTy = 0
LNTOTalORD = 0
lnTOTalBok = 0
lnTOTBok = 0
RETURN .T.

*!***************************************************************************
*! Name      : lfCheckLn
*! Developer : Mariam Mazhar(MMT)
*! Date      : 10/23/2008
*! Purpose   : Function to Check last line in Order
*!***************************************************************************
FUNCTION lfCheckLn

lcAlias = ALIAS()
SELECT (lcTempOrd)
lcOrdStr = CORDTYPE + ORDER + STORE 
lcKey = KEY()
lcKeyVal = EVALUATE(lcKey)
lcLnNum = RECNO(lcTempOrd)
=SEEK(lcOrdStr)
SCAN REST WHILE &lcKey.  = lcOrdStr 
  IF EVALUATE(lcKey) = lcKeyVal  
    llLastLine = .T.
  ELSE
    llLastLine = .F.
  ENDIF 
ENDSCAN 
IF BETWEEN(lcLnNum ,1,RECCOUNT())
  GO RECORD lcLnNum 
ENDIF 
SELECT(lcAlias)
LNTOTalORD = LNTOTORD
LNTOTalQTy = LNTOTQTy
lnTOTalBok = lnTOTBok
RETURN .T.