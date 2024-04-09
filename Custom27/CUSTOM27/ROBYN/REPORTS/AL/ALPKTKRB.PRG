*:***************************************************************************
*: Program file  : ALPKTKRB.PRG
*: Program desc. : PRINT PICKING TICKETS SIDE SHIPPING LABELS
*:                 NO BULK RECAP AT THE END FORM 'Z'
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : ALLOCATION (AL)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Robyn Merdith
*:***************************************************************************
*C101362,1 KHM 11/8/98
*B605123,1 BWA 11/13/2001 Fix the bug of variable "BTADDR1" not found.
*:***************************************************************************

*-- If the memory file (AlPkTkRB) exist then restore from it otherwise
*-- display a message to enable the user to choose whether to print the
*-- Style location or not.
IF FILE(gcDataDir+'ALPKTKRB.MEM')
  RESTORE FROM &gcDataDir.ALPKTKRB.MEM ADDITIVE
ELSE
  STORE SPACE (1) TO lcMLoc
ENDIF

*-- Opening the SkuTmpl file in order to print the Sku according to
*-- the account's SKU code.
= gfOpenFile(gcDataDir+'SkuTmpl',gcDataDir+'SkuTmpl','SH')

*-- Initializing the necessary variables.
*-- 1) These variables are used to get the length of the style and color 
*--    color segments.
lcPrnComp  = IIF(llPrntComp,'Y','N')
lcNonMajTl = ''
lcNonMajPi = ''
STORE 0 TO lnFreeLen , lnColorLen , lnMajSeg,lnNonMajSt
lnMajLen = LEN(gfItemMask('PM'))
=lfGetColor()

*-- 2) global variables that are necessary for the Pick Ticket.
STORE SPACE(6)  TO lcPikTkt,lcOrder
STORE {}        TO ldDate
STORE SPACE(8)  TO lcStore
STORE 0         TO lnNoOfSkus,lnValue,lnPieces,lnNoOfSkus
STORE SPACE(5)  TO lcAccount 
STORE SPACE(10) TO lcCustPO
STORE ""        TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcTermData,;
                   lcViaData,lcDivision,lcStName,lcStAddr1,lcStAddr2,;
                   lcStAddr3,lcScale1,lcScale2
STORE .F.       TO llEndPT
STORE 1         TO lnNotLine
STORE lcRpMsg1  TO MSGL1
STORE lcRpMsg2  TO MSGL2
STORE lcRpMsg3  TO MSGL3
llPrntLoc  = lcMLoc = 'Y'
DECLARE laDivLName[1,2]
lcDivLName = ""

*-- A message to ask you if you want to print the SKU number by size or not
llPrnSku   = gfModalGen('QRM44054B00006','Dialog') = 1
llChoice   = llPrnSku

*-- If the memory file (AlPkTkRB) does not exist then which means that
*-- lcMLoc variable is empty then display a message to ask the user
*-- if he want to print the style's location or not.
IF EMPTY(lcMLoc)
  llButtNo_1  = gfModalGen('QRM44055B00006','Dialog') = 1
  lcMLoc    = IIF(llButtNo_1,"Y","N")
  llPrntLoc = llButtNo_1
ENDIF  

*-- Saving the varibale to the memory file and then release it from the memory.
SAVE ALL LIKE lcMLoc TO gcDataDir+"ALPKTKEB.MEM"
RELEASE lcMLoc

*-- Getting the company address from array (laCompAdd)
HLINE1 = lcCompName
HLine2 = TRIM(laCompAdd[1])
HLine3 = TRIM(laCompAdd[2])
HLine4 = TRIM(laCompAdd[3])

IF EMPTY(HLine3)
  HLine3 = HLine4
  HLine4 = ""
ENDIF

SET DEVICE TO PRINT

*-- Clearing the relation from the ordline file, saving the old order
*-- and setting a new index because its the right one to use in the form.
SELECT OrdLine
SET RELATION TO
lcOrdLTag = SET('ORDER')
SET ORDER TO TAG OrdLinST

*-- This file holds all the lines of the selected pick tickets.
SELECT (lcTmpOrdL)
GOTO TOP

DO WHILE !EOF()
  
  *-- This function is used to get all the information necessary to print
  *-- the header of the pick ticket form.
  =lfHdrData()
  
  *-- This procedure is used to print the headers of the pick ticket form.
  DO lpHrdLabls
  Row     = 24
  
  SELECT (lcTmpOrdL)
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6)=lcPikTkt+lcOrder+'D'
    WAIT WINDOW "Style/Color" + Style NOWAIT
    DO CASE
      CASE TotPik <= 0
        LOOP
      CASE Row >=47
        llEndPT = .F.
        DO lpFilEmpLn
        DO lpOldFoter
        DO lpHrdLabls
        ROW = 24
    ENDCASE
    XSTYDESC   = ALLTRIM(SUBSTR(Style.DESC,1,16))
    lcStyScale = ALLTRIM(Style.Scale)

    *-- To get the color and color description from the codes file.
    lcColor = SUBSTR(&lcTmpOrdL..Style,lnNonMajSt,lnColorLen)
    SELECT Codes    
    lcClrDesc = gfCodDes(lcColor , 'COLOR')
    
    *-- Start printing the details.
    SELECT (lcTmpOrdL)
    @ ROW,01 SAY ALLTRIM(SUBSTR(STYLE,1,lnMajLen))
    @ ROW,13 SAY lcColor
    @ ROW,20 SAY XSTYDESC
    @ ROW,37 SAY lcStyScale
    @ ROW,38 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,42 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,46 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,50 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,54 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,58 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,62 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,66 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,70 SAY TOTPIK PICTURE '9999'
    
    *-- If the user select to print the style price.
    IF llRpStyPrc
      @ ROW,74 SAY PRICE PICTURE '9999.99'
    ENDIF
    
    @ ROW,81 SAY TOTPIK PICTURE '9999'
    DO lpSidLabls
    ROW = ROW + 1

    lnLocCol   = 1     
    llSizLPrtd = .F.

    *-- To print the style's SKU numbers
    DO lpPrtSku
    
    *-- If the user choose to print the style's locations.
    IF llPrntLoc 
      =lfPrnLoc()
    ENDIF

    IF !llSizLPrtd
      DO lpSidLabls
      Row = Row + 1
    ENDIF  
     
    *-- Totals
    SELECT (lcTmpOrdL)
    lnValue  = lnValue + TOTPIK * PRICE
    lnPieces = lnPieces + TOTPIK
  ENDSCAN
   
  *-- IF the user selects to print the order notpade. 
  IF llRpOrdNot
    =lfPrnNotPd()
  ENDIF
  llEndPT = .T.
  DO lpFilEmpLn
  DO lpNewFoter

  SELECT (lcTmpOrdL)
  IF !EOF()
    SKIP
  ENDIF
ENDDO
WAIT CLEAR
SET DEVICE TO SCREEN
SELECT OrdLine
SET ORDER TO &lcOrdLTag
SELECT (lcTmpOrdL)
SET RELATION TO "O"+Order+STR(LineNo,6) INTO OrdLine

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfHdrData
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/09/1998
*! Purpose   : TO get the information of the pick ticket's header.
*!*************************************************************
*! Calls       : =lfHdrData()
*!*************************************************************
FUNCTION lfHdrData
PRIVATE lnAlias

lnAlias    = SELECT(0)
lcPikTkt   = PikTkt
lcOrder    = Order
ldDate     = PikDate
lcStore    = PikTkt.Store
lcAccount  = PikTkt.Account
lcCustPO   = PikTkt.CustPO
STORE 0 TO lnValue,lnNoOfSkus,lnPieces

=IIF(EMPTY(lcStore),SEEK('M'+lcAccount,'Customer'),SEEK('S'+lcAccount+lcStore,'Customer'))

*B605123,1 BWA 11/13/2001 Fix the bug of variable "BTADDR1" not found.[START]
*lcBtName  = CUSTOMER->BTNAME
*lcBtAddr1 = CUSTOMER->BTADDR1
*lcBtAddr2 = CUSTOMER->BTADDR2
*lcBtAddr3 = TRIM(CUSTOMER->BTCITY) + ' ' +TRIM(CUSTOMER->BTSTATE)+ ' '+;
                 CUSTOMER->BTZIP

lcBtName  = CUSTOMER.BTNAME
lcBtAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
lcBtAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
lcBtAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')

*B605123,1 BWA 11/13/2001.[END]

IF EMPTY(lcBtAddr2)
  lcBtAddr2 = lcBtAddr3
  lcBtAddr3 = ''
ENDIF

*-- GET DESCRIPTIONS FOR CODED FIELDS
SELECT CODES     
SET ORDER TO CODES IN CODES  

*Terms
lcTermData = gfCodDes(OrdHdr->CTERMCODE , 'CTERMCODE')

*ShipVia
lcViaData = gfCodDes(OrdHdr->SHIPVIA , 'SHIPVIA')

*Division
lcDivision  = gfCodDes(OrdHdr->CDIVISION , 'CDIVISION')
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
=gfRltFld(OrdHdr.cDivision , @laDivLName , 'CDIVISION')

HLINE1 = IIF(FOUND() .AND. (!EMPTY(lcDivLName)),lcDivLName,lcCompName)

STORE "" TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3

*-- GET THE SIZE SCALES
lcScale2   = SPACE(1)
SELECT (lcTmpOrdL)
lnRecNo = RECNO()
lcScale1 = SCALE
LOCATE REST FOR SCALE <> lcScale1
IF !EOF()
  lcScale2 = SCALE
ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF

*-- SHIP-TO ADDRESS FOR THIS STORE
IF OrdHdr.Alt_ShpTo
  lcStName  = OrdHdr.STNAME
  lcStAddr1 = OrdHdr.cAddress1
  lcStAddr2 = OrdHdr.cAddress2
  lcStAddr3 = TRIM(OrdHdr.cAddress3) + ' ' + TRIM(OrdHdr.cAddress4) + ' ' + OrdHdr.cAddress5
  IF EMPTY(lcStAddr2)
    lcStAddr2 = lcStAddr3
    lcStAddr3 = ''
  ENDIF
ELSE
  lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
  lcStAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
  lcStAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
  lcStAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )

  IF EMPTY(lcStAddr2)
    lcStAddr2 = lcStAddr3
    lcStAddr3 = ''
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpHrdLabls
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/09/1998
*! Purpose   : TO print the Header labels.
*!*************************************************************
*! Calls       : DO lpHrdLabls
*!*************************************************************
PROCEDURE lpHrdLabls

@ 03,001 SAY IIF(lcPrnComp='Y',HLINE1,'')
@ 03,094 SAY IIF(lcPrnComp='Y',HLINE1,'')
@ 04,001 SAY IIF(lcPrnComp='Y',HLINE2,'')
@ 04,094 SAY IIF(lcPrnComp='Y',HLINE2,'')
@ 05,001 SAY IIF(lcPrnComp='Y',HLINE3,'')
@ 05,094 SAY IIF(lcPrnComp='Y',HLINE3,'')
@ 06,001 SAY IIF(lcPrnComp='Y',HLINE4,'')
@ 06,058 SAY lcDivision
@ 06,073 SAY lcPikTkt
@ 06,094 SAY IIF(lcPrnComp='Y',HLINE4,'')
@ 07,094 SAY lcStName
@ 08,058 SAY ORDHDR->APPROVAL
@ 08,073 SAY ldDate
@ 08,094 SAY lcStAddr1
@ 09,094 SAY lcStAddr2
@ 10,094 SAY lcStAddr3  
@ 11,005 SAY lcBtName
@ 11,048 SAY lcStName
@ 12,005 SAY lcBtAddr1
@ 12,048 SAY lcStAddr1
@ 13,005 SAY lcBtAddr2
@ 13,048 SAY lcStAddr2
@ 13,088 SAY IIF(ORDHDR->MultiPO,lcCustPO,OrdHdr->CUSTPO)
@ 13,102 SAY lcOrder
@ 13,112 SAY ORDHDR->DEPT
@ 13,119 SAY lcStore
@ 14,005 SAY lcBtAddr3
@ 14,048 SAY lcStAddr3
@ 14,102 SAY lcPikTkt
@ 18,093 SAY IIF(lcPrnComp='Y',HLINE1,'')
@ 19,001 SAY lcAccount
@ 19,007 SAY lcOrder
@ 19,014 SAY ORDHDR->REP1
@ 19,018 SAY ORDHDR->REP2
@ 19,021 SAY ORDHDR->ENTERED
@ 19,029 SAY ORDHDR->START
@ 19,037 SAY ORDHDR->COMPLETE
@ 19,046 SAY SUBSTR(lcTermData,1,14)
@ 19,060 SAY lcStore
@ 19,069 SAY IIF(ORDHDR->MultiPO,lcCustPO,OrdHdr->CUSTPO)
@ 19,080 SAY ORDHDR->DEPT 
@ 19,093 SAY IIF(lcPrnComp='Y',HLINE2,'')
@ 20,093 SAY IIF(lcPrnComp='Y',HLINE3,'')
@ 21,37 SAY lcScale1
XSCALE = GETSCALE( lcScale1,SPACE(1))
@ 21,39 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ1),1,3),3,' ')
@ 21,43 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ2),1,3),3,' ')
@ 21,47 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ3),1,3),3,' ')
@ 21,51 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ4),1,3),3,' ')
@ 21,55 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ5),1,3),3,' ')
@ 21,59 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ6),1,3),3,' ')
@ 21,63 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ7),1,3),3,' ')
@ 21,67 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ8),1,3),3,' ')
@ 21,93 SAY IIF(lcPrnComp='Y',HLINE4,'')
IF !EMPTY( lcScale2 )
  @ 22,37 SAY lcScale2
  XSCALE = GETSCALE( lcScale2,SPACE(1))
  @ 22,39 SAY SUBSTR(SCALE->SZ1,1,3)
  @ 22,43 SAY SUBSTR(SCALE->SZ2,1,3)
  @ 22,47 SAY SUBSTR(SCALE->SZ3,1,3)
  @ 22,51 SAY SUBSTR(SCALE->SZ4,1,3)
  @ 22,55 SAY SUBSTR(SCALE->SZ5,1,3)
  @ 22,59 SAY SUBSTR(SCALE->SZ6,1,3)
  @ 22,63 SAY SUBSTR(SCALE->SZ7,1,3)
  @ 22,67 SAY SUBSTR(SCALE->SZ8,1,3)
ENDIF

*!*************************************************************
*! Name      : lpSidLabls
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/09/1998
*! Purpose   : TO print the Side labels.
*!*************************************************************
*! Calls       : DO lpSidLabls
*!*************************************************************
PROCEDURE lpSidLabls

DO CASE
  CASE ROW = 34 .OR. ROW = 50
    @ ROW,94 SAY IIF(lcPrnComp='Y',HLINE1,'')
  CASE ROW = 35 .OR. ROW = 51
    @ ROW,94 SAY IIF(lcPrnComp='Y',HLINE2,'')
  CASE ROW = 36 .OR. ROW = 52
     @ ROW,94 SAY IIF(lcPrnComp='Y',HLINE3,'')
  CASE ROW = 37 .OR. ROW = 53
    IF llEndPT .AND. Row = 53
      @ 53,000 SAY MSGL1                    
    ENDIF  
    @ ROW,94 SAY IIF(lcPrnComp='Y',HLINE4,'')
  CASE ROW = 24 .OR. ROW = 40
    @ ROW,94 SAY lcStName
  CASE ROW = 25 .OR. ROW = 41
    @ ROW,94 SAY lcStAddr1
  CASE ROW = 26 .OR. ROW = 42
    @ ROW,94 SAY lcStAddr2
  CASE ROW = 27 .OR. ROW = 43
    @ ROW,94 SAY lcStAddr3
  CASE ROW = 30 .OR. ROW = 47
    @ ROW,088 SAY IIF(ORDHDR->MultiPO,lcCustPO,OrdHdr->CUSTPO)
    @ ROW,102 SAY lcOrder
    @ ROW,112 SAY ORDHDR->DEPT
    @ ROW,119 SAY lcStore
  CASE ROW = 31 .OR. ROW = 49
    @ ROW,102 SAY lcPikTkt
ENDCASE

*!*************************************************************
*! Name      : lpFilEmpLn
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/09/1998
*! Purpose   : TO fill the empty pick ticit lines.
*!*************************************************************
*! Calls       : DO lpFilEmpLn
*!*************************************************************
PROCEDURE lpFilEmpLn

DO WHILE Row <= 53
  DO lpSidLabls
  Row = Row + 1
ENDDO

*!*************************************************************
*! Name      : lpOldFoter
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/09/1998
*! Purpose   : To print the form footer at in the middle of a spacific 
*!             pick ticket. 
*!*************************************************************
*! Calls       : DO lpOldFoter
*!*************************************************************
PROCEDURE lpOldFoter

@ 55,94 SAY lcStName
@ 56,94 SAY lcStAddr1
@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 57,94 SAY lcStAddr2
@ 58,94 SAY lcStAddr3
ROW = ROW + 1

*!*************************************************************
*! Name      : lpNewFoter
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/09/1998
*! Purpose   : To print the form footer at the end of a spacific 
*!             pick ticket. 
*!*************************************************************
*! Calls       : DO lpNewFoter
*!*************************************************************
PROCEDURE lpNewFoter

*-- Not to print the total amount of the piktkt if the 
*-- user does not want to print the style prices.
@ 54,000 SAY MSGL2
@ 55,000 SAY MSGL3
@ 56,094 SAY lcStName
@ 57,094 SAY lcStAddr1
@ 58,002 SAY PikTkt.CWARECODE

IF SUBSTR(OrdHdr->Note1, 1, 1) <> '*'
  @ 58,21 SAY OrdHdr->Note1
ENDIF

@ 58,094 SAY lcStAddr2
@ 59,021 SAY IIF(SUBSTR(OrdHdr->Note2, 1, 1)<>'*', OrdHdr->Note2, '')
@ 59,078 SAY lnPieces PICTURE '999999'
@ 59,094 SAY lcStAddr3
@ 63,060 SAY SUBSTR(lcViaData,1,11)
@ 63,076 SAY IIF(llRpStyPrc,lnValue,'') PICTURE '99999.99'

@ 64,088 SAY IIF(ORDHDR->MultiPO,lcCustPO,OrdHdr->CUSTPO)
@ 64,102 SAY lcOrder
@ 64,112 SAY ORDHDR->DEPT
@ 64,119 SAY lcStore
@ 65,102 SAY lcPikTkt

lnValue = 0.00
ROW = ROW + 1

*!*************************************************************
*! Name      : lfPrnLoc
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/09/1998
*! Purpose   : To Print style location within warehouse.
*!*************************************************************
*! Calls       : =lfPrnLoc()
*!*************************************************************
FUNCTION lfPrnLoc
PRIVATE lnAlias,lcOldOrd

lnAlias = SELECT(0)
SELECT WhsLoc
lcOldOrd = SET('ORDER')
SET ORDER TO TAG WhsLocSt
IF SEEK(&lcTmpOrdL..STYLE+SPACE(6)+PikTkt.cWareCode,'WHSLOC')
  @ ROW,lnLocCol SAY 'LOC.:'
  lnI = IIF (lnLocCol = 1, 1, 3)
  SCAN REST WHILE STYLE+COLOR+CWARECODE = &lcTmpOrdL..STYLE+SPACE(6)+PikTkt.cWareCode
    @ PROW(),PCOL() + 1 SAY CLOCATION
    lnI = lnI + 1
    llSizLPrtd = .F.    
    IF lnI = 6
      lnI = 1
      DO lpSidLabls
      llSizLPrtd = .T.    
      @ PROW()+1 ,00 SAY ''
      ROW = ROW + 1
    ENDIF
  ENDSCAN
ENDIF
SET ORDER TO &lcOldOrd IN WhsLoc
SELECT(lnAlias)

*!*************************************************************
*! Name      : lpPrtSku
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/09/1998
*! Purpose   : To print the style/color Skus for a specific account.
*!*************************************************************
*! Calls       : =lpPrtSku()
*!*************************************************************
PROCEDURE lpPrtSku
PRIVATE lnAlias,lcOldTag

lnAlias = SELECT(0)

SELECT Spck_Lin
lcOldTag = SET("ORDER")
SET ORDER TO TAG SpckLins
IF !SEEK('S'+lcAccount+&lcTmpOrdL..Style);
   AND !SEEK('P'+lcAccount+&lcTmpOrdL..Style)
  RETURN
ENDIF

lcType = Spck_Lin.Type 

=SEEK('S'+Style.Scale,'Scale')
lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
  lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
  lnDime2 = SkuTmpl.Len4
ELSE
  lnDime1 = 8  &&Default
  lnDime2 = 8  &&Default
ENDIF 

lcType = 'S'
IF SEEK(lcType + lcAccount + &lcTmpOrdL..Style,'Spck_Lin')
  SELECT Spck_Lin
  COUNT REST WHILE Type+Account+Style+Pack_Id = ;
                 'S'+ lcAccount+&lcTmpOrdL..Style;
                 For TotQty <> 1 ;
                 TO lnNoOfSkus
  IF lnNoOfSkus = 1
    llPrnSku = .F.
  ELSE
    llPrnSku  = llChoice
  ENDIF
  SEEK 'S'+ lcAccount+&lcTmpOrdL..Style
  IF llPrnSku
    lnSel = SELECT()
    SELECT Spck_Lin
    DIMENSION laSku(8,2)
    laSku   = CHR(255)
    lnCount = 0   
    lnQtyn  = 1
    SCAN REST WHILE Type+Account+Style = lcType+lcAccount+&lcTmpOrdL..Style
      lnCount = lnCount + 1
      FOR lnQtyn=1 TO 8
        IF EVAL('Spck_Lin.Qty'+ALLTRIM(STR(lnQtyn))) = 1
          laSku(lnCount, 1) = STR(lnQtyn)
          laSku(lnCount, 2) = IIF(EMPTY(Spck_Lin.Sku),'SKU N#:' +Spck_Lin.Pack_Id,'Pack # : '+Spck_Lin.Sku)
          EXIT
        ENDIF
      ENDFOR
    ENDSCAN
    =ASORT(laSku,1)
    llMainSku = .T.
    FOR lnQtyn = 1 TO lnCount
      IF EVAL(lcTmpOrdL+'.Pik'+  ALLTRIM(laSku(lnQtyn,1))) <> 0
        IF ROW >= 53    && Max Row
          DO lpOldFoter
          DO lpHrdLabls     && print the page header
          ROW = 23
        ENDIF
        IF llMainSku
          @ ROW,01 SAY IIF(UPPER(lcSkuTmpl) $ 'JCP*MER',;
          LEFT(laSku(lnQtyn,2),7+SkuTmpl.Len1)+' '+SUBSTR(laSku(lnQtyn,2),7+SkuTmpl.Len1+1,SkuTmpl.Len2),;
          LEFT(laSku(lnQtyn,2),7+SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3))
          IF UPPER(lcSkuTmpl) = 'SWR'
            DO lpSidLabls
            ROW = ROW + 1
          ENDIF  
          llMainSku = .F.
          lnCol = 20
        ENDIF
        lnLocCol = LEN(laSku(lnQtyn,2)) + 5
        lnStrWidth = LEN(ALLTRIM(EVAL('Scale.Sz'+ALLTRIM(laSku[lnQtyn,1])))+':')
        lnMaxWidth = lnStrWidth + lnDime2
        IF ROW >= 53
          DO lpOldFoter
          DO lpHrdLabls
          ROW = 23
        ENDIF
        IF  lnCol+lnMaxWidth < 85
          @ ROW,lnCol SAY ALLTRIM(EVAL('Scale.SZ'+ALLTRIM(laSku[lnQtyn,1])))+":"+;
          ALLTRIM(SUBSTR(laSku[lnQtyn,2],7+lnDime1+1,lnDime2))        
         lnCol = lnCol + lnStrWidth + LEN(ALLTRIM(SUBSTR(laSku[lnQtyn,2],7+lnDime1+1,lnDime2))) + 1          
        ELSE
          DO lpSidLabls
          ROW = ROW +  1
          IF ROW >= 53
             DO lpOldFoter
             DO lpHrdLabls
             ROW = 23
          ENDIF
          lnCol = 20
          @ ROW,lnCol SAY ALLTRIM(EVAL('Scale.SZ'+ALLTRIM(laSku[lnQtyn,1])))+":"+;
          ALLTRIM(SUBSTR(laSku[lnQtyn,2],7+lnDime1+1,7,lnDime2))
          lnCol = lnCol + lnStrWidth + LEN(ALLTRIM(SUBSTR(laSku[lnQtyn,2],7+lnDime1+1,lnDime2))) + 1
        ENDIF
      ENDIF
    ENDFOR
    IF llPrnSku
      DO lpSidLabls
      ROW = ROW + 1
    ENDIF  
    SELECT (lnSel)
  ELSE      && The user choosed not to print SKU's
    IF ROW >= 53    && Max Row
      DO lpOldFoter      
      DO lpHrdLabls     && print the page header
      ROW = 23
    ENDIF
    
    lcStrToPrn = IIF(EMPTY(Spck_Lin.Sku),'SKU N# : ' + SUBSTR(Spck_Lin.Pack_Id,1,lnDime1),'Pack # : '+SUBSTR(Spck_Lin.Sku,1,8))    
     @ ROW,01 SAY 'SKU N#:'+IIF(UPPER(lcSkuTmpl) $ 'JCP*MER',;
          LEFT(ALLTRIM(Spck_Lin.Pack_Id),SkuTmpl.Len1)+' '+SUBSTR(Spck_Lin.Pack_Id,SkuTmpl.Len1+1,SkuTmpl.Len2),;
          ALLTRIM(LEFT(Spck_Lin.Pack_Id,lnDime1)))
    lnLocCol = LEN(lcStrToPrn) + 5
    DO lpSidLabls
    ROW = ROW + 1
  ENDIF
ENDIF
SET ORDER TO &lcOldTag IN Spck_Lin
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfPrnNotPd
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/09/1998
*! Purpose   : TO print the order notepad.
*!*************************************************************
*! Calls       : =lfPrnNotPd()
*!*************************************************************
FUNCTION lfPrnNotPd
PRIVATE lnAlias

llEndPT   = .T.
lnNotLine = 1
lnAlias   = SELECT(0)
IF SEEK('B' + OrdHdr->Order,'NotePad')
  lnOldMemW = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  @ Row,00 SAY '* -- N O T E S -- *' 
  DO lpSidLabls
  Row = Row + 1  
  DO WHILE lnNotLine <= lnMemLins
    IF Row >= 53
      DO lpOldFoter
      DO lpHrdLabls
      Row = 23
    ENDIF
    @ ROW,00 SAY MLINE(NotePad.MNOTES,lnNotLine)
    DO lpSidLabls
    ROW = ROW + 1
    lnNotLine = lnNotLine + 1
  ENDDO
  SET MEMOWIDTH TO lnOldMemW
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/09/1998
*! Purpose   : To get the length to of the color.
*!*************************************************************
*! Calls       : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

lcNonMajTl = ''
lcNonMajPi = ''
lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
 *-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
