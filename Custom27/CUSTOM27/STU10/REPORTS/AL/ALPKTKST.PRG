*:***************************************************************************
*: PROG : ALPKTKST.PRG                FOR : STUNT
*: DESC : Converted piktk from 26 to 27 FORMAT A
*: Modul: Aria Apparel Series.
*: DATE : 09/09/01
*: AUTH : Adel Mohammed El Gazzar (ADEL)
*: Refer to  : (C102289)
*:************************************************************************
*: Calls : 
*:         FUNCTION  : 
*:                   : 
*:                   : 
*:         PROCEDURE : 
***************************************************************************

SELECT (lcTmpOrdL)
GOTO TOP
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  llNoRec = .T.
  RETURN
ELSE
  llNoRec = .F.
ENDIF
  
*--Initialize needed variables.
STORE '' TO lcPikTkt,lcOrder,lcShipVia,lcAccount,lcTermData,lcWDesc,lcSpcInst
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6,lcSpcInst,lcSeason,lcStore
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6,lcCustPO,lcStrln1
lcNote1 = ''
lcNote2 = ''

STORE 0  TO lnColorLen,lnNonMajSt,lnRow,lnValue
STORE {} TO ldDate
DIMENSION laAddress[1,1]
STORE '' TO laAddress,HLINE2,HLINE3,HLINE4,HLINE5
*--Get the phone format.
lcPhnFrmt= gfPhoneTem()
lcComName = SycComp.cCom_Name
*-- Get the style major and color.
*-Get the style major length
lnMajLen = LEN(gfItemMask('PM'))
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
lnShift = -2
*--Restore the global flags.
llDyeLot   = (gfGetMemVar('M_DYELOT')  = 'Y')
lcFile  = 'ALPKTKST'
IF FILE( "&lcFile..MEM" )
  RESTORE ADDITIVE FROM &lcFile
ELSE
  IF gfModalGen('TRM00000B00006',.F.,.F.,.F.,"Do you wish to print the short color description?") = 1
    lcClrLDsc = 'Y'
  ELSE
    lcClrLDsc = 'N'
  ENDIF
  *- Save variable as default to be used later.
  SAVE ALL LIKE lcClrLDsc TO &lcFile
ENDIF
llWareHous  = (gfGetMemVar('M_WareHouse') = 'Y')

*--Open files
DO lpOpenFiles
*--Create or Open Summary temp file.
IF !USED('&lcSummary')
  lcSummary = &lcSummary
  SELECT ORDLINE
  COPY STRUCTURE TO (gcWorkDir+lcSummary)
  =gfOpenFile(gcWorkDir+lcSummary,'','EX')
  INDEX ON STYLE+STR(LINENO,6) TAG &lcSummary
ELSE
  SELECT (lcSummary)
  ZAP
ENDIF

    *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
 A='-------------------------------------------------------------------------------'
 B='  SHIPVIA       | SEASON |SPCL INSTRUCTIONS | STORE#   | DEPT# | PURCH ORDER  |'
B1='                |        |                  |          |       |              |'
 IF llDyeLot
 C='   G STYLE   CLR     DESCRIPTION              DYELOT #        PRICE      AMOUNT'
 ELSE
 C='   G STYLE   CLR     DESCRIPTION                              PRICE      AMOUNT'
 ENDIF
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
D='   _______ ___ ____________________ ___  ___  ___  ___  ___  ___  ___  ___ ____'
E='|  BILL OF LADING | # CARTONS | WEIGHT| PICKED BY | PACKED BY | SHIPPED VIA   |'

F='|                 |           |       |           |           |'
G='|  MERCHANDISE    |  FREIGHT  | INSUR | OTHER CHGS| TERMS     | COMMENTS:     |'
H='|  $              | $         | $     | $         |           |               |'
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

** DYELOT # IS PRINTED ONLY FOR SYSTEMS USING DYELOTS ***
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*   G STYLE   CLR     DESCRIPTION              DYELOT #         PRICE      AMOUNT
*12 X 1234567 123 12345678901234567890         XXXXXXXXXX     1234.99  123,456.99
*     SIZE: 12345 12345 12345 12345 12345 12345 12345 12345
*     ORDR: 12345 12345 12345 12345 12345 12345 12345 12345
*     ALLO: 12345 12345 12345 12345 12345 12345 12345 12345
*     PICK: 12345 12345 12345 12345 12345 12345 12345 12345

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
NEWDOC = .T.
XTIME = TIME()
*-------------
*-- MAIN LOOP
*-------------
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
*-- Get the data file.
*-- This file holds all the lines of the selected pick tickets.
SELECT (lcTmpOrdL)
GOTO TOP
*------------------------------
* SECTION: MAIN LOOP
*------------------------------
DO WHILE !EOF() AND INKEY() <>32
  WAIT WINDOW 'PRINT PICK TICKETS - <Space Bar> TO ABORT' NOWAIT
  *----------------------------
  * GET ORDERHDR & LINE ITEMS
  *----------------------------
  *-- Function to get piktkt HEADER information.
  =lfGetHData()
  *--Print the piktkt HEADER information.
  =lfPriHdr()
  *------------------
  * LINE LOOP
  *------------------
  lnTOTQTY = 0
  lnPieces = 0
  SELECT (lcTmpOrdL)
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6)=lcPikTkt+lcOrder+'D'
    DO CASE
      CASE TotPik <= 0
        LOOP
      CASE lnRow >=47
        =SEEK('O'+lcOrder,'ORDHDR')
        @ lnRow,30 SAY 'CONTINUED NEXT PAGE ...'
        @ 54+lnShift,01 SAY ORDHDR.NOTE1
        @ 54+lnShift,41 SAY ORDHDR.NOTE2
        @ 55+lnShift,00 SAY A
        @ 56+lnShift,00 SAY E
        @ 57+lnShift,00 SAY F
	    @ 57+lnShift,64 SAY SUBSTR(lcShipVia,1,10)
		@ 57+lnShift,78 SAY '|'
		@ 58+lnShift,00 SAY A
		@ 59+lnShift,00 SAY G
		@ 60+lnShift,00 SAY H
        @ 61+lnShift,05 SAY IIF(llRpStyPrc,'*******.**','')
        @ 61+lnShift,52 SAY SUBSTR(lcTermData,1,10)
        @ 62+lnShift,00 SAY A
        @ 63+lnShift,00 SAY lcRpMsg1
        @ 64+lnShift,00 SAY lcRpMsg2
        @ 65+lnShift,00 SAY lcRpMsg3
        *--Print the piktkt HEADER information.
        =lfPriHdr()
    ENDCASE
    @ lnRow,03 SAY GROUP
    @ lnRow,05 SAY SUBSTR(STYLE, 1, 7 )
    @ lnRow,13 SAY SUBSTR(STYLE, lnNonMajSt, 3 )
    @ lnRow,17 SAY STYLE.DESC
    IF llDyeLot
       @ lnRow,46 SAY DYELOT
    ENDIF
    IF llRpStyPrc
       @ lnRow,60 SAY PRICE         PICTURE '9999.99'
       @ lnRow,69 SAY PRICE*TOTPIK  PICTURE '999,999.99'
    ENDIF
    lcLongClr = SUBSTR(gfCodDes(SUBSTR(STYLE,lnMajLen+2) , 'COLOR'),1,15)
    IF lcClrLDsc = 'Y' .AND. !EMPTY(lcLongClr)
      lnRow = lnRow + 1
      @ lnRow,13 SAY lcLongClr
    ENDIF
    lnRow = lnRow + 1
    Y   = '   '
    lcScl = SPACE(0)
    IF SEEK('S'+Style.SCALE,'SCALE') 
      lcScl=Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')+Y+;
                          PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')+Y+;
                          PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')+Y+;
                          PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')   
    ENDIF
    @ lnRow,11 SAY lcScl
    lnRow = lnRow+2
    @ lnRow,05 SAY 'ORDR:'
    @ lnRow,12 SAY QTY1     PICTURE '99999'
    @ lnRow,18 SAY QTY2     PICTURE '99999'
    @ lnRow,24 SAY QTY3     PICTURE '99999'
    @ lnRow,30 SAY QTY4     PICTURE '99999'
    @ lnRow,36 SAY QTY5     PICTURE '99999'
    @ lnRow,42 SAY QTY6     PICTURE '99999'
    @ lnRow,48 SAY QTY7     PICTURE '99999'
    @ lnRow,54 SAY QTY8     PICTURE '99999'
    @ lnRow,61 SAY TOTQTY   PICTURE '999999'
    * CUM ORDER TOTALS
    lnValue   = lnValue + TOTPIK * PRICE
    PRTPIK=.F.
    PRTPIK = IIF(PIK1 # QTY1, .T., PRTPIK)
    PRTPIK = IIF(PIK2 # QTY2, .T., PRTPIK)
    PRTPIK = IIF(PIK3 # QTY3, .T., PRTPIK)
    PRTPIK = IIF(PIK4 # QTY4, .T., PRTPIK)
    PRTPIK = IIF(PIK5 # QTY5, .T., PRTPIK)
    PRTPIK = IIF(PIK6 # QTY6, .T., PRTPIK)
    PRTPIK = IIF(PIK7 # QTY7, .T., PRTPIK)
    PRTPIK = IIF(PIK8 # QTY8, .T., PRTPIK)
    IF PRTPIK
       lnRow = lnRow+1
       @ lnRow,05 SAY 'ALLO:'
       @ lnRow,12 SAY PIK1     PICTURE '99999'
       @ lnRow,18 SAY PIK2     PICTURE '99999'
       @ lnRow,24 SAY PIK3     PICTURE '99999'
       @ lnRow,30 SAY PIK4     PICTURE '99999'
       @ lnRow,36 SAY PIK5     PICTURE '99999'
       @ lnRow,42 SAY PIK6     PICTURE '99999'
       @ lnRow,48 SAY PIK7     PICTURE '99999'
       @ lnRow,54 SAY PIK8     PICTURE '99999'
       @ lnRow,61 SAY TOTPIK   PICTURE '999999'
    ENDIF
    lnPieces = lnPieces + TOTPIK
    lnRow =lnRow+1
    @ lnRow,05 SAY 'PICK:'
    @ lnRow,12 SAY '_____'
    @ lnRow,18 SAY '_____'
    @ lnRow,24 SAY '_____'
    @ lnRow,30 SAY '_____'
    @ lnRow,36 SAY '_____'
    @ lnRow,42 SAY '_____'
    @ lnRow,48 SAY '_____'
    @ lnRow,54 SAY '_____'
    @ lnRow,62 SAY '_____'
    lnRow=lnRow+2
    SELECT (lcSummary)
    =SEEK(&lcTmpOrdL..Style)
    IF EOF()
       APPEND BLANK
       REPLACE STYLE WITH &lcTmpOrdL..Style
    ENDIF
    REPLACE PIK1     WITH &lcTmpOrdL..PIK1 + PIK1,;
            PIK2     WITH &lcTmpOrdL..PIK2 + PIK2,;
            PIK3     WITH &lcTmpOrdL..PIK3 + PIK3,;
            PIK4     WITH &lcTmpOrdL..PIK4 + PIK4,;
            PIK5     WITH &lcTmpOrdL..PIK5 + PIK5,;
            PIK6     WITH &lcTmpOrdL..PIK6 + PIK6,;
            PIK7     WITH &lcTmpOrdL..PIK7 + PIK7,;
            PIK8     WITH &lcTmpOrdL..PIK8 + PIK8,;
            TOTPIK   WITH &lcTmpOrdL..TOTPIK + TOTPIK,;
            QTY1     WITH &lcTmpOrdL..QTY1 + QTY1,;
            QTY2     WITH &lcTmpOrdL..QTY2 + QTY2,;
            QTY3     WITH &lcTmpOrdL..QTY3 + QTY3,;
            QTY4     WITH &lcTmpOrdL..QTY4 + QTY4,;
            QTY5     WITH &lcTmpOrdL..QTY5 + QTY5,;
            QTY6     WITH &lcTmpOrdL..QTY6 + QTY6,;
            QTY7     WITH &lcTmpOrdL..QTY7 + QTY7,;
            QTY8     WITH &lcTmpOrdL..QTY8 + QTY8,;
            TOTQTY   WITH &lcTmpOrdL..TOTQTY + TOTQTY
    * GET NEXT LINE ITEM
    SELECT (lcTmpOrdL)
  ENDSCAN
  *------------------------ END PRINT LINE LOOP ----------------------
  IF lnPieces >0
    lnRow = lnRow + 1
    @ lnRow,10 SAY "TOTAL PIECES TO PICK ............"
    @ lnRow,61 SAY lnPieces      PICTURE '999999'
    lnPieces = 0
  ENDIF
  *------------------------
  * [NOTEPAD] COMMENT LINES
  *------------------------
  IF llRpOrdNot
    SELECT NOTEPAD
    IF SEEK('B'+lcOrder)
      =SEEK('O'+lcOrder,'ORDHDR')
      lnMemoWdth = SET('MEMOWIDTH')
      SET MEMOWIDTH TO 75
      lnMemoLnNo = 0      
      lnMemolins = 0
      lnRow = lnRow +1
      @ lnRow,02 SAY '*-- N O T E P A D --*'
      lnRow = lnRow +1
      lnMemolins = MEMLINES(mNotes)
      IF lnMemolins > 0
        DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
          IF lnRow > 51
            lnRow = lnRow + 1
            @ lnRow,2 SAY 'CONTINUED NEXT PAGE ...'
            @ 54+lnShift,01 SAY ORDHDR.NOTE1
            @ 54+lnShift,41 SAY ORDHDR.NOTE2
            @ 55+lnShift,00 SAY A
    		@ 56+lnShift,00 SAY E
	     	@ 57+lnShift,00 SAY F
		    @ 57+lnShift,64 SAY SUBSTR(lcShipVia,1,10)
		    @ 57+lnShift,78 SAY '|'
			@ 58+lnShift,00 SAY A
			@ 59+lnShift,00 SAY G
			@ 60+lnShift,00 SAY H
            @ 61+lnShift,05 SAY IIF(llRpStyPrc,'*******.**','')
            @ 61+lnShift,52 SAY SUBSTR(lcTermData,1,10)
            @ 62+lnShift,00 SAY A
            @ 63+lnShift,00 SAY lcRpMsg1
            @ 64+lnShift,00 SAY lcRpMsg2
            @ 65+lnShift,00 SAY lcRpMsg3
            *--Print the piktkt HEADER information.
            =lfPriHdr()
           ENDIF
           lnMemoLnNo = lnMemoLnNo + 1
           @ lnRow,02 SAY MLINE(NOTEPAD.mNotes,lnMemoLnNo)
           lnRow = lnRow + 1
         ENDDO
       ENDIF
       @ lnRow,02 SAY '*-- END OF NOTEPAD --*'
       lnMemoLnNo = 0      
       lnMemolins = 0
       SET MEMOWIDTH TO (lnMemoWdth)
     ENDIF
   ENDIF
   @ 54+lnShift,01 SAY lcNote1
   @ 54+lnShift,41 SAY lcNote2
   SELECT (lcTmpOrdL)
   @ 55+lnShift,00 SAY A
   @ 56+lnShift,00 SAY E
   @ 57+lnShift,00 SAY F
   @ 57+lnShift,64 SAY SUBSTR(lcShipVia,1,10)
   @ 57+lnShift,78 SAY '|'
   @ 58+lnShift,00 SAY A
   @ 59+lnShift,00 SAY G
   @ 60+lnShift,00 SAY H
   IF llRpStyPrc
     @ 61+lnShift,05 SAY lnValue     PICTURE '9999999.99'
     lnValue   = 0.00
   ENDIF
   @ 61+lnShift,52 SAY SUBSTR(lcTermData,1,10)
   @ 62+lnShift,00 SAY A
   @ 63+lnShift,00 SAY lcRpMsg1
   @ 64+lnShift,00 SAY lcRpMsg2
   @ 65+lnShift,00 SAY lcRpMsg3
   IF !EOF() AND cGrupDetal = 'H'
     SKIP
   ENDIF  
ENDDO
SELECT (lcSummary)
LOCATE
lnRow  = 99
PAGECT = 0
DO WHILE INKEY() <>32 .AND. !EOF()
  WAIT WINDOW 'PRINT BULK TICKETS - <Space Bar> TO ABORT' NOWAIT
   IF lnRow > 55
      PAGECT=PAGECT+1
      @ 01,00 SAY 'ALPKTKST'
      @ 01,30 SAY 'BULK PICKING TICKET'
      @ 01,65 SAY 'PAGE  '+STR(PAGECT,3)
      @ 02,00 SAY DATE()
      @ 02,65 SAY 'TIME  ' + XTIME
      @ 04,00 SAY '-------------------------------------------------------------------------------'
      @ 05,00 SAY '     STYLE   CLR     DESCRIPTION                                               '
      @ 06,00 SAY '-------------------------------------------------------------------------------'
      lnRow=8
   ENDIF
   @ lnRow,05 SAY SUBSTR(STYLE, 1, 7 )
   @ lnRow,13 SAY SUBSTR(STYLE, lnNonMajSt, 3 )
   @ lnRow,17 SAY IIF(SEEK(STYLE,'STYLE'),STYLE.DESC,'')
   lnRow = lnRow+1
   @ lnRow,05 SAY 'ORDR:'
   @ lnRow,12 SAY QTY1     PICTURE '99999'
   @ lnRow,18 SAY QTY2     PICTURE '99999'
   @ lnRow,24 SAY QTY3     PICTURE '99999'
   @ lnRow,30 SAY QTY4     PICTURE '99999'
   @ lnRow,36 SAY QTY5     PICTURE '99999'
   @ lnRow,42 SAY QTY6     PICTURE '99999'
   @ lnRow,48 SAY QTY7     PICTURE '99999'
   @ lnRow,54 SAY QTY8     PICTURE '99999'
   @ lnRow,61 SAY TOTQTY   PICTURE '999999'
   lnRow = lnRow+1
   @ lnRow,05 SAY 'ALLO:'
   @ lnRow,12 SAY PIK1     PICTURE '99999'
   @ lnRow,18 SAY PIK2     PICTURE '99999'
   @ lnRow,24 SAY PIK3     PICTURE '99999'
   @ lnRow,30 SAY PIK4     PICTURE '99999'
   @ lnRow,36 SAY PIK5     PICTURE '99999'
   @ lnRow,42 SAY PIK6     PICTURE '99999'
   @ lnRow,48 SAY PIK7     PICTURE '99999'
   @ lnRow,54 SAY PIK8     PICTURE '99999'
   @ lnRow,61 SAY TOTPIK   PICTURE '999999'
   lnRow = lnRow+1
   @ lnRow,00 SAY REPLICATE('-',79)
   lnRow=lnRow+2
   SKIP
ENDDO
SET DEVICE TO SCREEN
RETURN

*--------------------------------
*    END ALPKTKST.PRG
*--------------------------------


*!*************************************************************
*! Name      : lpOpenFiles
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/09/01
*! Purpose   : To Open files.
*!*************************************************************
PROCEDURE lpOpenFiles

=IIF(!OPENED('CODES'),gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH'),.T.)
=IIF(!OPENED('CUSTOMER'),gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH'),.T.)
=IIF(!OPENED('PIKTKT'),gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'PIKTKT','SH'),.T.)
=IIF(!OPENED('SCALE'),gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH'),.T.)
=IIF(llWareHous AND !USED('WareHous'),gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH'),.T.)

*!*************************************************************
*! Name      : lfGetHData
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/09/01
*! Purpose   : TO get the information of the pick ticket's header.
*!*************************************************************
FUNCTION lfGetHData
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcPikTkt  = PikTkt
lcOrder   = Order
ldDate    = PikDate
lcStore   = PikTkt.Store
lcAccount = PikTkt.Account
lcCustPO  = IIF(ORDHDR.MULTIPO,ORDLINE.CUSTPO,ORDHDR.CUSTPO)
*--Get the warehouse address
lcWDesc = SPACE(1)
IF llWareHous
  =SEEK(Piktkt.cWareCode,'WareHous')
  lcWDesc = 'WARHSE: '+ WareHous.cWareCode+ ' '+SUBSTR(WareHous.cDesc,1,18)
ELSE
  GO TOP IN WareHous
ENDIF
HLINE2  = WareHous.cAddress1
HLINE3  = WareHous.cAddress2
HLine4 = SUBSTR(WareHous.cAddress3,1,15) + ' ' + SUBSTR(WareHous.cAddress4,1,3)+ ' ' + SUBSTR(WareHous.cAddress5,1,10)
HLINE5  = WareHous.cPhone
IF EMPTY(HLINE3)
  HLINE3 = HLINE4
  HLINE4 = HLINE5
  HLINE5 = ''
ENDIF 
STORE 0 TO lnValue,lnPieces
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6,lcSpcInst,lcSeason
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6,lcCustPO,lcStrln1
*--Get the Bill To adddess.
*--Get the proper record in the customer file.
=IIF(EMPTY(lcStore),SEEK('M'+lcAccount,'Customer'),SEEK('S'+lcAccount+lcStore,'Customer'))
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
lcBtName  = CUSTOMER.BTNAME
=gfGetAdr('Customer','','','',1,'2')
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAdd&lcCount = lcBtAdd&lcCount + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
  ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
ENDFOR
IF EMPTY(lcBtAdd2)
  lcBtAdd2 = lcBtAdd3
  lcBtAdd3 = ''
ENDIF
*-- SHIP_TO ADDRESS FOR THIS STORE
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6
IF OrdHdr.Alt_ShpTo
  lcStName  = OrdHdr.STNAME
  =gfGetAdr('ORDHDR','','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  ENDFOR  
ELSE
  lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
  =gfGetAdr('CUSTOMER','','','',1,'')
  *--Get the Ship To adddess except the country.    
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  ENDFOR  
ENDIF
IF EMPTY(lcStAdd2)
  lcStAdd2 = lcStAdd3
  lcStAdd3 = ''
ENDIF
*-- GET DESCRIPTIONS FOR CODED FIELDS
SELECT CODES     
SET ORDER TO CODES IN CODES  
*--Terms
lcTermData = gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE')
*--ShipVia
lcShipVia = gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA')
*--Special instruction
lcSpcInst = gfCodDes(OrdHdr.SPCINST , 'SPCINST')
*---Season
lcSeason = gfCodDes(OrdHdr.SEASON , 'SEASON')


*:*************************************************************************
*: PROGRAM   : lfPriHdr
*: DESC.     : PRINT PIKTKT Header
*! Date      : 09/09/01
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfPriHdr
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT ORDHDR
* LINE 1
@ 00,01 SAY 'P I C K I N G   T I C K E T'
@ 00,35 SAY lcComName
@ 00,70 SAY DATE()
@ 01,01 SAY 'TICKET: '+lcPikTkt
@ 01,16 SAY ldDate
@ 01,35 SAY HLINE2
lcOrder=ORDER
@ 02,01 SAY 'ORDER : '+ORDER
@ 02,16 SAY SUBSTR(DTOC(START),1,5)
@ 02,24 SAY SUBSTR(DTOC(COMPLETE),1,5)
@ 02,35 SAY HLINE3
@ 02,70 SAY XTIME
@ 03,01 SAY IIF(!EMPTY(APPROVAL),'APPRVL: '+APPROVAL,'') 
@ 03,35 SAY HLINE4
IF llWareHous .AND. !EMPTY(lcWDesc)
  @ 04,01 SAY lcWDesc
ENDIF
@ 04,35 SAY HLINE5  PICTURE lcPhnFrmt SIZE 1,16
@ 06,04 SAY '.....SOLD TO .....'
@ 06,46 SAY '.....SHIP TO .....'
@ 07,04 SAY ACCOUNT
@ 07,10 SAY PHONE  PICTURE lcPhnFrmt SIZE 1,16
IF LEN(TRIM(lcStore))<>0 .AND. lcStore<>'*'
   @ 07,46 SAY 'STORE#: ' + lcStore
ENDIF
@ 08,04 SAY lcBtName
@ 08,46 SAY lcStName
@ 09,04 SAY lcBtadd1
@ 09,46 SAY lcStadd1
@ 10,04 SAY lcBtadd2
@ 10,46 SAY lcStadd2
@ 11,04 SAY lcBtadd3
@ 11,46 SAY lcStadd3
@ 13,00 SAY A
@ 14,00 SAY B
@ 15,00 SAY A
@ 16,01 SAY SUBSTR(lcShipVia,1,15)
@ 16,16 SAY '|'
@ 16,18 SAY SUBSTR(lcSeason,1,7) 
@ 16,25 SAY '|'
@ 16,26 SAY SUBSTR(lcSpcInst,1,15) 
@ 16,44 SAY '|'
@ 16,47 SAY &lcTmpOrdL..STORE
@ 16,55 SAY '|'
@ 16,58 SAY DEPT
@ 16,63 SAY '|'
@ 16,66 SAY SUBSTR(IIF(ORDHDR.MULTIPO,&lcTmpOrdL..CUSTPO,ORDHDR.CUSTPO),1,10)
@ 16,78 SAY '|'
@ 17,00 SAY A
@ 18,00 SAY C
@ 19,00 SAY A
lnRow=20
lcNote1 = ORDHDR.NOTE1
lcNote2 = ORDHDR.NOTE2

SELECT (lcAlias)