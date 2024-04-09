*:***************************************************************************
*: Prog. : OORCNMU.PRG (Converted from 26 o 27 for MULBERRY)
*: DESC  : PRINT ORDER CONF. - 84 LINE PAGE, 8 1/2" x 14" (For Mulberry)
*: Module: Aria Apparel Series.
*: DATE  : 09/02/1999
*: AUTH  : Adel Mohammed El Gazzar (ADEL)
*: NOTE  : This is an optional program made because the lines temp. file created
*:         in the masetr program SOORCN comes with one record per style\color.
*:         But this form needs one record per Size. So i made this program to face 
*:         this and other needed things.
*  Refer to  : (101616)
*:************************************************************************
*: Calls : 
*:         Functions  : gfItemMask()
*:                    : gfGetMemVar()
*:                    : gfTempName()
*:                    : gfCodDes()
*:************************************************************************
*C101919,1 ABD 07/02/2000 Adjust the picture of CUSTPO field
*B803421,1 ABD 07/24/2000 Fix problem that When printing more than one order, 
*B803421,1 ABD            The ship to address of the first one prints for the 
*B803421,1 ABD            Rest of the orders, Fix also in the frx.
*B803606,1 SHA 08/16/2000 Increased the space between the details lines
*B803676,1 SSE 09/20/2000 Fix Bug of Printing Billing Addresses instead of Shippin Addresses 
*B803676,1                in Order that exceeds one Page (all modifications in FRX only , Prg for Documentation only)
*C101998,1 MHM 10/08/2000 Add order header note1 and note2 in (all modifications in FRX only , Prg for Documentation only)
*C102416,1 AME 09/12/2001 increase the length of style to 12 ch in desc field.
*C102416,1 AME            Also there's some changes in .FRX
*C102481,1 MHM 11/26/2001 Add Option to print Order Confirmation by individual Stores for Multi-Store 
*B804546,1 RAE 12/04/2001 Fix the problem of Order Confirmation form in case of using an alternate address in the S/O for the ship to address.
*B605546,1 HBG 03/20/2002 Fix bug of File access denied
*:************************************************************************

STORE SPACE(78) TO lcNote
IF !FILE("SOORCNMU.MEM")
  *--The screen here won't appear properly if we press 'RUN' in the OG as
  *-- SET DEVIVE will be to 'PRINT' so i put this condition.
  lcSetDev = SET('DEVICE')
  SET DEVICE TO SCREEN
  DO gcRepHome+gcAct_Appl+'\SOORCNMU.spx'
  SET DEVICE TO &lcSetDev
  IF !EMPTY(lcNote)
    SAVE ALL LIKE LCNOTE*.* TO SOORCNMU
  ENDIF 
ELSE
  RESTORE FROM SOORCNMU.MEM ADDI
ENDIF
*-- Get the major and nonmajor titles and lengths.
lcNonMajPi = ''
lcNonMajTl = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
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
ENDFOR
*--Get the UPS for the first time. Next times it will be filled in the .FRX
lcUpsAct = gfGetMemVar('XUPSACCT')
*--Create a temp file to hold a record per size. This file will be used from .FRX
*-- to print records not the temp file (lcTempOrd)
lcOrdTmp = gfTempName()
*C102416,1 AME increase the field des by 5ch to hold 12 ch of style.  [START] 
*CREATE TABLE (gcWorkDir+lcOrdTmp) (ORDER C(6),STORE C(8),LINENO N(6),STYLE C(19),;
                             DESC C(13),QTY N(5),PRICE N(8,2),;
                             ACCOUNT C(5),AMOUNT N(8,2),WARECODE C(6),;
                             CUSTPO C(10),CLRDESC C(15))     

CREATE TABLE (gcWorkDir+lcOrdTmp) (ORDER C(6),STORE C(8),LINENO N(6),STYLE C(19),;
                             DESC C(18),QTY N(5),PRICE N(8,2),;
                             ACCOUNT C(5),AMOUNT N(8,2),WARECODE C(6),;
                             CUSTPO C(10),CLRDESC C(15))     

*C102416,1 AME [END]
SELECT (lcOrdTmp)
IF lcRpSortBy = 'S'
  INDEX ON ORDER + STORE + STYLE TAG (lcOrdTmp)
  SET ORDER TO TAG (lcOrdTmp)
ELSE
  INDEX ON ORDER + STORE + STR(LINENO,6) TAG (lcOrdTmp)
  SET ORDER TO TAG (lcOrdTmp)
ENDIF
SELECT (lcTempOrd)

*C102481,1 MHM 11/26/2001 call store function for browse storse[Start]
*SCAN 
STORE SPACE(0) TO lcAccStore
=LFStore()

*--Set relation off from ordhdr into customer to check for multi store
SELECT ORDHDR
SET RELATION OFF INTO CUSTOMER
SELECT (lcTempOrd)
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SCAN FOR &lcAccStore
*C102481,1 MHM  [End]

  WAIT WINDOW "Collect " + "Order-" +lcStyTitle+order+"-"+ Style + " - <Space Bar> TO ABORT " NOWAIT
  FOR lnSize = 1 to 8
    lcSize = STR(lnSize,1)
    IF QTY&lcSize > 0
      SCATTER MEMVAR 
      m.ClrDesc = SUBSTR(gfCodDes(SUBSTR(STYLE,lnNonMajSt,LEN(lcNonMajPi)),'COLOR'),1,15)

      *AMR
      *m.Desc   = ALLTRIM(SUBSTR(&lcTempOrd..STYLE,1,7))+;
                "-"+ALLTRIM(SUBSTR(&lcTempOrd..STYLE,lnNonMajSt,2))+;
      IIF(&lcTempOrd..Scale != "T"," "+SUBSTR(ALLTRIM(SCALE.SZ&lcSize),1,2),"")    
      *C102416,1 AME take 12 ch of style instead of 8ch. [Start] 
      *m.Desc   = ALLTRIM(SUBSTR(&lcTempOrd..STYLE,1,8))+;
                 "-"+ALLTRIM(SUBSTR(&lcTempOrd..STYLE,lnNonMajSt,2))+;
                 IIF(&lcTempOrd..Scale != "T"," "+SUBSTR(ALLTRIM(SCALE.SZ&lcSize),1,2),"")    
      m.Desc   = ALLTRIM(SUBSTR(&lcTempOrd..STYLE,1,12))+;
                 "-"+ALLTRIM(SUBSTR(&lcTempOrd..STYLE,lnNonMajSt,2))+;
                 IIF(&lcTempOrd..Scale != "T"," "+SUBSTR(ALLTRIM(SCALE.SZ&lcSize),1,2),"")    
      *C102416,1 AME [End]
      *AMR

      m.Qty    = &lcTempOrd..Qty&lcSize
      m.Amount = QTY*PRICE
      INSERT INTO (lcOrdTmp) FROM MEMVAR
    ENDIF
  ENDFOR
  IF INKEY() = 32
    EXIT
  ENDIF
ENDSCAN

*C102481,1 MHM 11/26/2001 call Retrive Relation of customer into ordhdr[Start]
SELECT (lcTempOrd)
DELE ALL FOR !&lcAccStore
LOCATE
SET RELATION OFF INTO CUSTOMER
SELECT ORDHDR
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
*C102481,1 MHM [End]
                    
SELECT (lcOrdTmp)
LOCATE 

*C102481,1 MHM 11/26/2001 Check for no record found[Start]
IF EOF()
  *--No records to display.
  llNoRec = .T.
  RETURN
ENDIF
*C102481,1 MHM [End]

SELECT ORDHDR
SET RELATION TO order INTO (lcOrdTmp) ADDITIVE 
SET RELATION TO 'B' + Order INTO NOTEPAD ADDITIVE 
SET SKIP TO [&lcOrdTmp]

*!*************************************************************
*! Name      : lfChgFormt
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/28/2000
*! Purpose   : Function to Change Format Custpo To 
*!           : Print as'XXXX-XXXXXXX-XXXX'
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfChgFormt()
*!*************************************************************
*C101919,1
FUNCTION lfChgFormt
PARAMETER lCustPoFrm

IF !EMPTY(ORDHDR.CUSTPO)
  lcCustpoNo = ALLTRIM(ORDHDR.CUSTPO)
ELSE
  lcCustpoNo = ALLTRIM(EVAL(lcOrdTmp+'.CUSTPO'))
ENDIF
*B803606,1 SHA(Begin)Changed the condition of formating the PO#
lCustPoFrm = lcCustpoNo

*IF !EMPTY(lcCustpoNo)
IF !EMPTY(lcCustpoNo) AND CUSTOMER.USR_DFND1 = "YES"
*B803606,1 SHA(End)
  lCustPoFrm = PADR(SUBSTR(lcCustpoNo,1,4),4) +'-'+PADR(SUBSTR(lcCustpoNo,5,7),7)+'-'+PADR(SUBSTR(lcCustpoNo,12,4),4)
ENDIF


RETURN lCustPoFrm
*- End Of lfChgFormt.

*!*************************************************************
*! Name      : lfgetSpAdr
*! Developer : Abdou Elgendi [ABD]
*! Date      : 07/24/2000
*! Purpose   : Function to Get the Sold to Address, Ship to Address,
*!           : the Description of the Ship Via, Season,
*!           : Special Instructions, and Terms.
*! Refere    : 803421  
*!*************************************************************
*! Called from : lfHeadVar Function
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : gfRltFld, gfCodDes, gfGetAdr, lfAdrShift.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*803421,1
FUNCTION lfgetSpAdr
Parameter llDummy

lnSavAlias = SELECT(0)

lcStore = &lcTempOrd..Store

= gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcShipVia = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
lcSeason  = gfCodDes(ORDHDR.Season  , 'SEASON'   )
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')

SELECT CUSTOMER
IF ORDHDR.MULTI = 'Y' 
  = SEEK('S' + &lcOrdTmp..Account + &lcOrdTmp..Store , "CUSTOMER")
ENDIF

lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

= lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

*-- IF alternate ship to address
IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
  lcShpTName = STName
  *B804546,1 RAE [BEGIN] 
  *laShpAdr[1] = cAddress1
  *laShpAdr[2] = cAddress2
  *laShpAdr[3] = cAddress3
  *laShpAdr[4] = cAddress4
  *laShpAdr[5] = cAddress5
  
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
  *B804546,1 RAE [END]
ELSE    && Else
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
  IF TYPE('lcCurrKey') = 'C'
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF
ENDIF    && End of IF
= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
SELECT (lnSavAlias)

*-- End of lfgetSpAdr.
*!*************************************************************
*! Name      : LFStore
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/26/2001
*! Purpose   : Brow the Account inrange and modify the filter.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = LFStore()
*!*************************************************************
*!*C102481,1 
FUNCTION LFStore

PRIVATE lcAliasStr

lcAliasStr = ALIAS()
STORE SPACE(0) TO lcExpStr

CREATE CURSOR (lcTempStr) (AccStore C(13) )
ZAP
INDEX ON AccStore TAG (lcTempStr) OF (gcWorkDir+lcTempStr+'.CDX')

lcBrowFlds = [ACCOUNT     :H = 'Acct #'       :10 ,]   + ;
             [STORE       :H = 'store' :10 ,]    + ;
             [BTNAME      :H = 'Name'      :15 ,]    + ;
             [PHONE1      :H = 'Phone'    :15 ,]    + ;
             [NETBAL      :H = 'Balance'    :15 ,]    + ;
             [CADDRESS6   :H = 'Country'       :10 ]


*--temp file for collecting data

SELECT CUSTOMER
lcOldOrd = SET('ORDER')
*B605546,1 HBG 03/20/2002 Change the name of the temp index to fix bug of File access denied[Begin]
*INDEX ON  ACCOUNT+STORE TAG lcMmajour OF (gcWorkDir + lcTmpIndx+ '.CDX') 
*SET ORDER TO TAG lcMmajour OF (gcWorkDir +lcTmpIndx  + '.CDX')
INDEX ON  ACCOUNT+STORE TAG lcMmajour OF (gcWorkDir + lcTempIndx+ '.CDX') 
SET ORDER TO TAG lcMmajour OF (gcWorkDir +lcTempIndx  + '.CDX')
*B605546,1 [End]

=lfCreatTmp()

SELECT (lcTmpFile)
LOCATE

=gfrange(lcBrowFlds,lcTempStr,"AccStore","","","","@! XXXXXXXXXXXXXX")
SELECT (lcTempStr)

DIMENSION laStylFle[1,7]
laStylFle[1,1]= "Account+Store"
laStylFle[1,2]= "F"
laStylFle[1,3]= "C"
laStylFle[1,4]= .T.
laStylFle[1,5]= "In List             "
laStylFle[1,6]= lcTempStr
laStylFle[1,7]= "R"

DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]
=ACOPY(laBrFldFlt,laBrTmpFlt)

DIMENSION laBrFldFlt[2,ALEN(laBrTmpFlt,2)]
laBrFldFlt = ""
laBrFldFlt[2,1] = "Account+Store"
laBrFldFlt[2,2] = lcBrowFlds
laBrFldFlt[2,3] = ""
laBrFldFlt[2,4] = ""
laBrFldFlt[2,5] = "laStylFle:1"

lcAccStore = gfGenFlt('laStylFle',.T.,.T.)
lcAccStore = STRTRAN(lcAccStore,'Account+Store','CUSTOMER.ACCOUNT+CUSTOMER.STORE')
lcAccStore =IIF(!EMPTY(lcAccStore),lcAccStore,".T.")
DIMENSION laBrFldFlt[ALEN(laBrTmpFlt,1),ALEN(laBrTmpFlt,2)]
=ACOPY(laBrTmpFlt,laBrFldFlt)
SELECT CUSTOMER
SET ORDER TO &lcOldOrd
SELECT (lcAliasStr)
*-- End Function LFSTORE --*

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/26/2001
*! Purpose   : Brow the Account Store inrange and modify the filter.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCreatTmp()
*!*************************************************************
*!*C102481,1 
FUNCTION lfCreatTmp

DIMENSION laTempStru[1,4]
laTempStru = ''

SELECT CUSTOMER
lnFildLen = AFIELDS(laTempStru)

*-- Add Code fields.
DIMENSION laTempStru[lnFildLen + 1, 4]
laTempStru[lnFildLen + 1, 1] = 'AccStore'
laTempStru[lnFildLen + 1, 2] = 'C'
laTempStru[lnFildLen + 1, 3] = 13
laTempStru[lnFildLen + 1, 4] = 0

CREATE CURSOR (lcTmpFile)   FROM ARRAY laTempStru

SELECT (lcTmpFile)
*-- Fix Cursor bug [Begin]
ZAP
*-- Fix Cursor bug [End  ]
INDEX ON AccStore TAG (lcTmpFile) OF (gcWorkDir+lcTmpFile+'.CDX')

SELECT (lcTempOrd)
SCAN 
  SCATTER MEMVAR MEMO
  IF !SEEK(&lcTempOrd..Account+&lcTempOrd..Store,lcTmpFile)
    =SEEK(m.Account+m.Store,'CUSTOMER')
    SELECT CUSTOMER
    SCATTER MEMVAR MEMO
    m.AccStore = CUSTOMER.Account+CUSTOMER.Store
    INSERT INTO (lcTmpFile) FROM MEMVAR
  ENDIF  
ENDSCAN
SELECT CUSTOMER