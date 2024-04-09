*:**************************************************************************
*: PROGRAM   : ALPKTKS.PRG  (For APPRES) Converted form 26 to 27.
*: DESC.     : PRINT INVOICE - 66 LINE PAGE, 8 1/2" x 11" (For Andrew Marc)
*: DATE      : 04/22/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Refer to  : (C101472)
*:**************************************************************************
*: Calls : 
*:         FUNCTION  : lfGetHData()
*:                   : lfPriHdr()
*:                   : gfItemMask()
*:                   : gfCodDes()
*:                   : gfGetAdr()
*:**************************************************************************
*:Modifications  :
*:B603513,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.
*:**************************************************************************
*--Initialize needed variables.
STORE '' TO lcPikTkt,lcOrder,lcShipVia,lcAccount,lcDivison,lcTermData
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6,lcSpcInst,lcSeason,lcStore
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6,lcCustPO,lcStrln1
STORE 0  TO lnColorLen,lnNonMajSt,lnRow
STORE {} TO ldDate
DIMENSION laAddress[1,1]
STORE '' TO laAddress
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
        @ 54,12 SAY '** CONTINUED NEXT PAGE **'
        =lfPriHdr()
    ENDCASE
    lcStyDesc = SUBSTR(STYLE.DESC,1,16)
    lcColDesc = SUBSTR(gfCodDes(SUBSTR(STYLE,lnNonMajSt,lnColorLen),'COLOR'),1,15)
    lcColDesc = IIF(!EMPTY(lcColDesc),lcColDesc,' ')
    @ lnRow,01 SAY ALLTRIM(STYLE)
    @ lnRow,20 SAY lcColDesc
    @ lnRow,31 SAY lcStyDesc
    @ lnRow,54 SAY SCALE
    @ lnRow,57 SAY PIK1 PICTURE '@Z 999'
    @ lnRow,61 SAY PIK2 PICTURE '@Z 999'
    @ lnRow,65 SAY PIK3 PICTURE '@Z 999'
    @ lnRow,69 SAY PIK4 PICTURE '@Z 999'
    @ lnRow,72 SAY PIK5 PICTURE '@Z 999'
    @ lnRow,76 SAY PIK6 PICTURE '@Z 999'
    @ lnRow,80 SAY PIK7 PICTURE '@Z 999'
    @ lnRow,83 SAY PIK8 PICTURE '@Z 999'
    @ lnRow,86 SAY TOTPIK  PICTURE '99999'
    IF TOTQTY >TOTPIK
      @ lnRow,92 SAY TOTQTY-TOTPIK  PICTURE '99999'
    ENDIF
    DO CASE
      CASE lnRow = 20 .OR. lnRow = 36
        @ lnRow,104 SAY lcStName
      CASE lnRow = 21 .OR. lnRow = 37
        @ lnRow,104 SAY lcStAdd1
      CASE lnRow = 22 .OR. lnRow = 38
        @ lnRow,104 SAY lcStAdd2
      CASE lnRow = 23 .OR. lnRow = 39
        @ lnRow,104 SAY lcStAdd3
      CASE lnRow = 26 .OR. lnRow = 43
        @ lnRow,99 SAY lcCustPO
        @ lnRow,113 SAY lcStore
        @ lnRow,122 SAY ORDHDR.DEPT
        @ lnRow,129 SAY lcOrder
      CASE lnRow = 28 .OR. lnRow = 45
        @ lnRow,113 SAY lcPikTkt 
     ENDCASE
     lnRow = lnRow + 1
     *-- CUM ORDER TOTALS
     lnTOTQTY = lnTOTQTY + TOTQTY
     lnPieces = lnPieces + TOTPIK
  ENDSCAN
  *------------------------ END PRINT LINE LOOP ----------------------
  DO WHILE lnRow < 48
    DO CASE
      CASE lnRow = 20 .OR. lnRow = 36
        @ lnRow,104 SAY lcStName
      CASE lnRow = 21 .OR. lnRow = 37
        @ lnRow,104 SAY lcStAdd1
      CASE lnRow = 22 .OR. lnRow = 38
        @ lnRow,104 SAY lcStAdd2
      CASE lnRow = 23 .OR. lnRow = 39
        @ lnRow,104 SAY lcStAdd3
      CASE lnRow = 26 .OR. lnRow = 43
        @ lnRow,099 SAY lcCustPO
        @ lnRow,113 SAY lcStore
        @ lnRow,122 SAY ORDHDR.DEPT
        @ lnRow,129 SAY lcOrder
      CASE lnRow = 28 .OR. lnRow = 45
        @ lnRow,113 SAY lcPikTkt
    ENDCASE
    lnRow = lnRow + 1
  ENDDO
  *--Print the totals
  @ 52,104 SAY lcStName
  @ 53,104 SAY lcStAdd1
  @ 54,20  SAY ORDHDR.NOTE1
  @ 54,104 SAY lcStAdd2
  @ 55,20  SAY ORDHDR.NOTE2
  @ 55,86  SAY lnPieces PICTURE '99999'
  IF lnTOTQTY > lnPieces
    @ 55,92 SAY lnTOTQTY-lnPieces PICTURE '99999'
  ENDIF
  @ 55,104 SAY lcStAdd3
  @ 59,99  SAY lcCustPO
  @ 59,113 SAY lcStore
  @ 59,122 SAY ORDHDR.DEPT
  @ 59,129 SAY lcOrder
  @ 61,113 SAY lcPikTkt
  
  *B603513,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.[START]
  *SELECT PIKTKT
  *=SEEK(lcPikTkt)
  *REPLACE PRTFLAG WITH 'P'
  *B603513,1 BWA 03/08/2000 [END]
  
  SELECT (lcTmpOrdL)
  SKIP
ENDDO
SET DEVICE TO SCREEN
RETURN
*--------------------------------
*    END ALPKTKS.PRG
*--------------------------------

*!*************************************************************
*! Name      : lfGetHData
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/21/99
*! Purpose   : TO get the information of the pick ticket's header.
*! Refer to  : (C101472)
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
*--Get the Bill To adddess.
*--Get the proper record in the customer file.
=IIF(EMPTY(lcStore),SEEK('M'+lcAccount,'Customer'),SEEK('S'+lcAccount+lcStore,'Customer'))
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
lcBtName  = CUSTOMER.BTNAME
=gfGetAdr('Customer','','','',1,'2')
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAdd&lcCount = lcBtAdd&lcCount + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR
*-- SHIP_TO ADDRESS FOR THIS STORE
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6
IF OrdHdr.Alt_ShpTo
  lcStName  = OrdHdr.STNAME
  =gfGetAdr('ORDHDR','','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR  
ELSE
  lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
  =gfGetAdr('CUSTOMER','','','',1,'')
  *--Get the Ship To adddess except the country.    
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR  
ENDIF
*-- GET DESCRIPTIONS FOR CODED FIELDS
SELECT CODES     
SET ORDER TO CODES IN CODES  
*--Terms
lcTermData = gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE')
*--ShipVia
lcShipVia = gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA')
*--Division desc.
lcDivison = SUBSTR(gfCodDes(OrdHdr.CDIVISION , 'CDIVISION'),1,13)


*:*************************************************************************
*: PROGRAM   : lfPriHdr
*: DESC.     : PRINT PIKTKT Header
*: DATE      : 04/21/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfPriHdr
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT ORDHDR
@ 02,057 SAY lcShipVia
@ 02,078 SAY lcPikTkt
@ 03,104 SAY lcStName
@ 04,057 SAY APPROVAL
@ 04,078 SAY ldDate
@ 04,104 SAY lcStAdd1
@ 05,104 SAY lcStAdd2
@ 06,104 SAY lcStAdd3
@ 08,16 SAY lcBtName
@ 08,59 SAY lcStName
@ 09,16 SAY lcBtAdd1
@ 09,59 SAY lcStAdd1
@ 10,16 SAY lcBtAdd2
@ 10,59 SAY lcStAdd2
@ 10,99 SAY lcCustPO
@ 10,113 SAY lcStore
@ 10,122 SAY DEPT
@ 10,129 SAY lcOrder
@ 11,16 SAY lcBtAdd3
@ 11,59 SAY lcStAdd3
@ 12,113 SAY lcPikTkt
@ 15,001 SAY lcAccount
@ 15,08 SAY lcOrder
@ 15,14 SAY REP1
@ 15,18 SAY REP2
@ 15,22 SAY ENTERED
@ 15,30 SAY START
@ 15,38 SAY lcDivison
@ 15,053 SAY lcTermData
@ 15,068 SAY lcStore  PICTURE '####'
@ 15,078 SAY lcCustPO  PICTURE '##########'
@ 15,089 SAY ORDHDR.DEPT
lcScale1 = SPACE(1)
lcScale2 = SPACE(1)
lcScale3 = SPACE(1)
SELECT ORDLINE
lnRecNo = RECNO()
SCAN WHILE cordtype+order+STR(lineno,6) = 'O'+lcOrder FOR PIKTKT = lcPikTkt
  IF EMPTY( lcScale1 )
    lcScale1 = SCALE
    LOOP
  ENDIF
  IF SCALE = lcScale1
    LOOP
  ENDIF
  IF EMPTY( lcScale2 )
    lcScale2 = SCALE
    LOOP
  ENDIF
  IF SCALE = lcScale2
    LOOP
  ENDIF
  lcScale3 = SCALE
ENDSCAN
GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,RECNO())
*-- PRINT 1 ST SIZE SCALE
@ 17,54 SAY lcScale1
SELE SCALE
SEEK 'S'+lcScale1
@ 17,57 SAY PADL(ALLTRIM(SCALE.SZ1),3,' ')+' '+;
            PADL(ALLTRIM(SCALE.SZ2),3,' ')+' '+;
            PADL(ALLTRIM(SCALE.SZ3),3,' ')+' '+;
            PADL(ALLTRIM(SCALE.SZ4),3,' ')+;
            PADL(ALLTRIM(SCALE.SZ5),3,' ')+' '+;
            PADL(ALLTRIM(SCALE.SZ6),3,' ')+' '+;
            PADL(ALLTRIM(SCALE.SZ7),3,' ')+' '+;                                                        
            PADL(ALLTRIM(SCALE.SZ8),3,' ')
*-- PRINT 2ND. SIZE SCALE
IF .NOT. EMPTY( lcScale2 )
  @ 18,54 SAY lcScale2  &&@ 18,43 SAY lcScale2
  SEEK 'S'+lcScale2
  @ 18,57 SAY PADL(ALLTRIM(SCALE.SZ1),3,' ')+' '+;
              PADL(ALLTRIM(SCALE.SZ2),3,' ')+' '+;
              PADL(ALLTRIM(SCALE.SZ3),3,' ')+' '+;
              PADL(ALLTRIM(SCALE.SZ4),3,' ')+;
              PADL(ALLTRIM(SCALE.SZ5),3,' ')+' '+;
              PADL(ALLTRIM(SCALE.SZ6),3,' ')+' '+;
              PADL(ALLTRIM(SCALE.SZ7),3,' ')+' '+;                                                        
              PADL(ALLTRIM(SCALE.SZ8),3,' ')
ENDIF
lnRow = 20
IF .NOT. EMPTY( lcScale3 )
  @ 19,54 SAY lcScale3  &&@ 19,43 SAY lcScale3
  SEEK 'S'+lcScale3
  @ 19,57 SAY PADL(ALLTRIM(SCALE.SZ1),3,' ')+' '+;
              PADL(ALLTRIM(SCALE.SZ2),3,' ')+' '+;
              PADL(ALLTRIM(SCALE.SZ3),3,' ')+' '+;
              PADL(ALLTRIM(SCALE.SZ4),3,' ')+;
              PADL(ALLTRIM(SCALE.SZ5),3,' ')+' '+;
              PADL(ALLTRIM(SCALE.SZ6),3,' ')+' '+;
              PADL(ALLTRIM(SCALE.SZ7),3,' ')+' '+;                                                        
              PADL(ALLTRIM(SCALE.SZ8),3,' ')
  @ lnRow,104 SAY lcStName
  lnRow = 21
ENDIF
IF lnRow = 20
  @ lnRow,104 SAY lcStName 
ENDIF
lnRow = 21
SELECT (lcAlias)