*:****************************************************************************
*: Program file        : SOORCNAS   && for customer Asian American DBA Eagle's Eye
*: Program desc.       : Order Acknowledgement
*: For Report          : SOORCNAS.FRX
*: System              : Aria Advantage Series VER. 2.7
*: Module              : Sales Oredr - (SO)
*: Developer           : MOHAMED SHOKRY (MHM)
*: Tracking Job Number : C101961
*: Date                : 09/12/2000
*:****************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : 
*:****************************************************************************
*: Called From         : System Menu (SO --> Output --> Order Confirmation)
*:****************************************************************************
*: Passed Parameters  : None
*:****************************************************************************
*: Example : DO SOORCNAS
*:****************************************************************************
*: Modification  :
*:****************************************************************************
*:B803791,1 MHM 11/07/2000  Fix some Bugs in Eagle's Eye order acknowldgement form 
*:****************************************************************************
*--Variable Section [start]
STORE '' TO lcStyNMj ,lcColorNm ,lcSalsNam ,lcTmpOrd ,lcLine
*:B803791,1 MHM  add variable to count # of pages [start]
*STORE 0  TO lnOrder
STORE 0  TO lnOrder , lnCount , lnSavOrd ,lcStore ,lcSavSor ,lnConter
*:B803791,1 MHM  add variable to count # of pages [end]
*--Variable Section [end]
*--if no record found return
SELECT ORDHDR
SET SKIP TO
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

lcTmpOrd = gfTempName()

*--Add this part to get last rec. in order no 
SELECT (lcTempOrd)
IF lcRpSortBy = 'S'
  lnOrder1 = 'CORDTYPE,ORDER,STORE,STYLE'
ELSE
  lnOrder1 = 'CORDTYPE,ORDER,STORE,LINENO'
ENDIF
*-- save relations
SET RELATION OFF INTO SCALE
*:B803791,1 MHM   [start]
SET RELATION OFF INTO STYLE
*:B803791,1 MHM   [end]
lnReltn = SET('RELATION')
*--get data in another table
*:B803791,1 MHM   [start]
* SELECT *,.F. AS llLstRec;
        FROM (lcTempOrd) ;
        INTO DBF (gcWorkDir+lcTmpOrd) ORDER BY &lnOrder1

 SELECT *,.F. AS llLstRec , 00000 AS pageNo ;
        FROM (lcTempOrd) ;
        INTO DBF (gcWorkDir+lcTmpOrd) ORDER BY &lnOrder1

*:B803791,1 MHM   [END]
SELECT (lcTmpOrd)
lnOrder = Order
*:B803791,1 MHM  move relation to get it with scan[start]
lcStore = Store
SET RELATION TO cOrdType+ Order INTO ORDHDR 
*:B803791,1 MHM  move relation to get it with scan[end]
SCAN
  IF (lnOrder = Order) .AND. (lcStore = Store)
    REPLACE &lcTmpOrd..llLstRec WITH .F. 
  ELSE
    *:B803791,1 MHM  [start]
    lnSavOrd = Order
    lcSavSor = Store
    *:B803791,1 MHM  [End]
    SKIP-1
    *:B803791,1 MHM  Function to get last rec [start]
    COUNT TO lnConter FOR (lnOrder = Order) .AND. (lcStore = Store)
    LOCATE FOR (Order = lnSavOrd) .AND. (store = lcSavSor)
    SKIP-1
    =lfGtLast()
    *SKIP
    REPLACE &lcTmpOrd..llLstRec WITH .T.
    REPLACE ALL &lcTmpOrd..pageNo WITH lnCount FOR (lnOrder = Order) .AND. (lcStore = Store) 
    LOCATE FOR (Order = lnSavOrd) .AND. (store = lcSavSor)
    lcStore = Store
    *:B803791,1 MHM  Function to get last rec [end]
    lnOrder = Order
  ENDIF  
ENDSCAN  
*-- to get last rec and update its value
*:B803791,1 MHM  Function to get last rec [start]
COUNT TO lnConter FOR (lnOrder = Order) .AND. (lcStore = Store)
*:B803791,1 MHM  Function to get last rec [end]

GO BOTT
*:B803791,1 MHM  Function to get last rec [start]
*REPLACE &lcTmpOrd..llLstRec WITH .T.
=lfGtLast()
REPLACE &lcTmpOrd..llLstRec WITH .T.
REPLACE ALL &lcTmpOrd..pageNo WITH lnCount FOR (lnOrder = Order) .AND. (lcStore = Store) 
GO TOP
*:B803791,1 MHM  Function to get last rec [end]


=gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
SELECT ORDHDR
SET RELATION TO rep1 INTO SALESREP ADDITIVE
SET RELATION OFF INTO (lcTempOrd)

SELECT (lcTmpOrd)
*:B803791,1 MHM  change relation [start]
*SET RELATION TO &lnReltn
*SET RELATION TO cordtype+ order INTO ORDHDR ADDITIVE
SET RELATION TO &lnReltn ADDITIVE
SET RELATION TO style INTO Style ADDITIVE
*:B803791,1 MHM  change relation [end]
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

SELECT (lcTmpOrd)

*!*************************************************************
*! Name      : lfStyle
*! Developer : Mohamed Shokry (MHM)
*! Date      : 09/12/2000
*! Purpose   : Function to get style major and nonmajor.
*!*************************************************************
*! Called from : SORDCONAS.FRX
*!*************************************************************
*! Calls       : gfItemMask()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStyle()
*!*************************************************************
FUNCTION lfStyle
PARAMETER lcDummy
PRIVATE lnClrLen,lnClrPos, lnAlias ,lcDummy
STORE 0 TO lnClrLen,lnClrPos

lnAlias = SELECT()
*--Get NonMajor 
SELECT (lcTmpOrd)
DECLARE laItemSeg[1]
STORE 0 TO lnClrLen,lnClrPos
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
lcStyNMj = SUBSTR(STYLE,lnClrPos,lnClrLen)
*--Get Color Discreption
SELECT CODES
=SEEK('N'+lcStyNMj+'N'+'COLOR     ')
lcColorNm = cdiscrep
*:B803791,1 MHM  set group[start]
llEndGroup = .F.    && Start of new Group.
*:B803791,1 MHM  set group[end]
SELECT(lnAlias)
RETURN ''

*--End lfStyle
*!*************************************************************
*! Name        : lfGetNots
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 09/19/2000
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : ( NotePad) .
*!*************************************************************
*! Called from : SORDCONAS.FRX [Variable lcDum in the report]
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetNots()
*!*************************************************************
FUNCTION lfGetNots
PARAMETER lcParam
lcParam = ''

IF ORDHDR.LastLine > 0
  *-- Note that the following Scheme
  *-- ....... llRoOrdNot ............. Order or Contract Notepad.
  IF llRpOrdNot AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
         !EMPTY(ALLTRIM(NOTEPAD.mNotes))
    =SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD')         
  ENDIF
ENDIF
lcNots    = ALLTRIM(NOTEPAD.MNOTES)

RETURN lcParam

*-- end of lfGetNotes.
*!*************************************************************
*! Name        : lfGetBmp
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 09/19/2000
*! Purpose     : Function to get bmp for the company
*!*************************************************************
*! Called from : syrepuvr
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetBmp()
*!*************************************************************
FUNCTION lfGetBmp
PARAMETER lcDummy

=gfOpenFile(gcSysHome+'SYFRMCDD',gcSysHome+'FORMCDDT','SH')
=SEEK("SOORCN"+"AS")
llLogo = .T.
RETURN ''
*-- end of lfGetBmp.
*!*************************************************************
*! Name        : lfGtLast
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 11/07/2000
*! Purpose     : Function to get number of pages per order.
*!             : Since every page takes 8 records. and it takes 34 lines of notes. 
*!             : So we need to know convert the lines per each notepad to records.
*!             : so we divied 34/8 = 4.25 record/page. 
*!             : In case of ORDHDR.NOTE1 OR ORDHDR.NOTE2 it takes 2.12 lines, so it takes
*!             : 1 record. So if there is Order has 5 lines in Order lines that means we will
*!             : print 5 records in first page. and we will print the Note1 and Note2 in 
*!             : record number 6, and the notepad will be printed in the 2 records remaining. 
*!*************************************************************
*! Called from : syrepuvr
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGtLast()
*!*************************************************************
*:B803791,1 MHM  
FUNCTION lfGtLast
STORE "" TO lcExp1,lcExp2

lcExp1 = LEFT(ALLTRIM(OrdHdr.NOTE1),1)<> '*' AND !EMPTY(OrdHdr.note1)
lcExp2 = LEFT(ALLTRIM(OrdHdr.NOTE2),1)<> '*' AND !EMPTY(OrdHdr.note2)
lcOrdEx = lnConter/8

IF llRpOrdNot .AND. SEEK('B' + ORDER , 'NOTEPAD')
  IF (lcExp1) .AND (lcExp2) AND llRpOrdLnt
    lnCount = CEILING(lcOrdEx + (MEMLINES(NOTEPAD.MNOTES)+3.2)/(8*4.50))
  ELSE
    IF (lcExp2) AND llRpOrdLnt
      lnCount = CEILING(lcOrdEx + ((MEMLINES(NOTEPAD.MNOTES)+1.6)/ (8*4.50)))
    ELSE
      lnCount = CEILING(lcOrdEx + (MEMLINES(NOTEPAD.MNOTES)/ (8*4.50)))
    ENDIF
   
    IF (lcExp1) AND llRpOrdLnt
      lnCount = CEILING(lcOrdEx + ((MEMLINES(NOTEPAD.MNOTES)+1.6)/ (8*4.50)))
    ELSE
      lnCount = CEILING(lcOrdEx + (MEMLINES(NOTEPAD.MNOTES)/ (8*4.50)))
    ENDIF
  ENDIF  
ELSE
  IF (lcExp1) .AND (lcExp2) AND llRpOrdLnt
    lnCount = CEILING ((lnConter+(3.2/4.5))/8)
  ELSE
    IF (lcExp1) AND llRpOrdLnt
      lnCount = CEILING ((lnConter+(1.6/4.5))/8)
    ELSE
      lnCount = CEILING(lcOrdEx) 
    ENDIF

    IF (lcExp2) AND llRpOrdLnt
      lnCount = CEILING ((lnConter+(1.6/4.5))/8)
    ELSE
      lnCount = CEILING(lcOrdEx) 
    ENDIF
  ENDIF  
ENDIF      

*--END lfGtLast
*!*************************************************************
*! Name      : lfHeader
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/12/2000
*! Purpose   : Function that fill header data 
*!*************************************************************
*! Called from : SORDCONAS.FRX
*!*************************************************************
*! Calls       : lfSlShpAdr()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfHeader()
*!*************************************************************
*:B803791,1
FUNCTION lfHeader
PARAMETER lcDummy
PRIVATE lnAlias ,lcDummy
lcAlias = ALIAS()   && Save Current alias.
llEndGroup = .F.    && Start of new Group.
= lfSlShpAdr()      && Call Function that fill header data [SoldTo and ShipTo]
SELECT (lcAlias)    && Restore before function alias.
RETURN ''
*-- end lfHeader
*!*************************************************************
*! Name      : lfSlShpAdr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/12/2000
*! Purpose   : Function to Get the Sold to Address, Ship to Address,
*!           : the Description of the Ship Via, Season,
*!           : Special Instructions, and Terms.
*!*************************************************************
*! Called from : lfHeader Function
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : gfRltFld, gfCodDes, gfGetAdr, lfAdrShift.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*:B803791,1 
FUNCTION lfSlShpAdr

lnSavAlias = SELECT(0)
lcStore = &lcTmpOrd..Store

= gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcShipVia = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
lcSeason  = gfCodDes(ORDHDR.Season  , 'SEASON'   )
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')

SELECT CUSTOMER
IF ORDHDR.MULTI = 'Y'
  = SEEK('S' + &lcTmpOrd..Account + &lcTmpOrd..Store , "CUSTOMER")
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
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else

  * Print distribution center address if found (1) Begin
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcStore = lcStore + '  Dist. Center : ' + Customer.Store
  ENDIF
  * Print distribution center address if found (1) End

  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

  *   Print distribution center address if found (2) Begin
  IF TYPE('lcCurrKey') = 'C'
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF
  *  Print distribution center address if found (2) End

ENDIF    && End of IF
= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
SELECT (lnSavAlias)
*-- end of lfSlShpAdr.
