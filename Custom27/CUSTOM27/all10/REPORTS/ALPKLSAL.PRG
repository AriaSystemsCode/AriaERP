********************************************************************************
*! Porgram       : ALPKLSAL.PRG                                 (C102061)          
*! Desc          : Convert ALLEGRO INTIMATES Packing Slip Print Form AL.
*! Date          : 01/30/2001
*: System        : Aria 27 System 
*: Module        : Sales Order Allocation (AL)
*: Developer     : Mohamed Shokry (MHM)
*:******************************************************************************
*: Calls Procedures : 
*:******************************************************************************
*: Passed Parameters  : None
*:******************************************************************************
*: Modifications      :
*:******************************************************************************
*B804058,1 AAN 04/09/2001 Fix a bug that Box numbers are wrong always showing 0-0
*B804361,1 HBG 15/08/2001 Fix bug of repeating the cartons range.
*B605284,1 ABD 01/08/2002 Print the Ship to address not the bill to address.
********************************************************************************

*--lcPackTmp Temp file come from standerd packing list 

*:B804058,1 AAN Add a condition for special instruction to the OG filter[Begin]
IF !EMPTY(lcRpSInst)
  lcRpExp = lcRpExp + [ .AND. OrdHdr.SpcInst = ALLTRIM(lcRpSInst)]
ENDIF      
*:B804058,1 AAN Add a condition for special instruction to the OG filter[Begin]

*B804058,1 AAN Declare a variable to hold a line no [Begin].
PRIVATE lnLineNo
lnLineNo = 0
*B804058,1 AAN Declare a variable to hold a line no [End].

SELECT (lcPackTmp)

XREPORT = ''
R_WIDTH = 'N'
XTITLE  = ''
R_TITLE = '' 
IF (lcRpSelcBy <> "P")
  =gfModalGen('QRM00000B38018','F',' ',' ','This feature is not allowed, you have to select Packing List')   
  llNoRec = .T.
  RETURN
ELSE  
  llNoRec = .F.
ENDIF

WAIT WINDO 'PRINTING - <SPACE BAR> TO ABORT' NOWAIT
SET DEVICE TO PRINT

SELECT (lcPackTmp)
*:B804058,1 AAN Test if there are records in tmp. file match th econd. or not[Begin]
LOCATE FOR &lcRpExp
IF !FOUND()
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
*:B804058,1 AAN Test if there are records in tmp. file match th econd. or not[End]
LOCATE


*:B804058,1 AAN Add condition to the scan exp.[Begin]
*SCAN WHILE INKEY() <> 32
SCAN WHILE INKEY() <> 32 FOR &lcRpExp
*:B804058,1 AAN Add condition to the scan exp.[End]

  *:B804058,1 AAN Decrease the max. row[Begin]
  *MAXROW   = 52
  MAXROW   = 48
  *:B804058,1 AAN Decrease the max. row[End]  
  XPACK_NO = PACK_NO
  
  *B804361,1 HBG 15/08/2001 Use the temp PACk ine file to print from [Begin]
  *SELE PACK_LIN
  SELECT (lcPakLnTmp)
  *B804361,1 [End]
  
  SEEK &lcPackTmp->PACK_NO
  IF EOF()
    LOOP
  ENDIF
  SELECT (lcPackTmp)
  
  llNewPack=.T.     &&Flag to control the pages TAK10/16/94
  DO WHILE .T. 
    *B605284,1 ABD 01/08/2002 (Begin) Print the alternative ship to address.
    IF OrdHdr.Alt_ShpTo
      XNAME1 = OrdHdr.StName
      XADDR1 = OrdHdr.CADDRESS1
      XADDR2 = OrdHdr.CADDRESS2
      XCITY  = OrdHdr.CADDRESS3
      XSTATE = OrdHdr.CADDRESS4
      XZIP   = OrdHdr.CADDRESS5
    *B605284,1 ABD 01/08/2002 (End)
    ELSE
      IF EMPTY( STORE )
        SELECT CUSTOMER
        IF !SEEK ( 'M' + &lcPackTmp..ACCOUNT )
          XNAME1 = ' '
          XADDR1 = ' '
          XADDR2 = ' '
          XCITY  = ' '
          XSTATE = ' '
          XZIP   = ' ' 
        ELSE
          XNAME1 = BTNAME
          XADDR1 = CADDRESS1
          XADDR2 = CADDRESS2
          XCITY  = CADDRESS3
          XSTATE = CADDRESS4
          XZIP   = CADDRESS5
        ENDIF
      ELSE
        SELECT CUSTOMER
        IF !SEEK( 'S' + &lcPackTmp..ACCOUNT + &lcPackTmp..STORE )
          XNAME1 = ' '
          XADDR1 = ' '
          XADDR2 = ' '
          XCITY  = ' '
          XSTATE = ' '
          XZIP   = ' ' 
        ELSE
          XNAME1 = STNAME
          *B605284,1 ABD - Print the Ship to Address not the Bill to address. [Begin]
          *XADDR1 = CADDRESS12
          *XADDR2 = CADDRESS22
          *XCITY  = CADDRESS32
          *XSTATE = CADDRESS42
          *XZIP   = CADDRESS52
          XADDR1 = CADDRESS1
          XADDR2 = CADDRESS2
          XCITY  = CADDRESS3
          XSTATE = CADDRESS4
          XZIP   = CADDRESS5
          *B605284,1 ABD - [End]
        ENDIF 
      ENDIF
    ENDIF
    *B804058,1 AAN Change the position [Begin].
    *@ 07,76 SAY &lcPackTmp..PACK_NO
    @ 03,78 SAY &lcPackTmp..PACK_NO
    *B804058,1 AAN Change the position [End].
  
    *B804058,1 AAN Change the position [Begin].
    *@ 15,18 SAY XNAME1
    *@ 16,18 SAY XADDR1
    *@ 17,18 SAY XADDR2
    *@ 18,18 SAY LEFT(XCITY,17)
    *@ 18,36 SAY LEFT(XSTATE,3)
    *@ 18,41 SAY SUBSTR( XZIP, 1, 5 )
    @ 11,18 SAY XNAME1
    @ 12,18 SAY XADDR1
    @ 13,18 SAY XADDR2
    @ 14,18 SAY LEFT(XCITY,17)
    @ 14,36 SAY LEFT(XSTATE,3)
    @ 14,41 SAY SUBSTR( XZIP, 1, 5 )
    *B804058,1 AAN Change the position [End].

    SELECT ORDHDR
    SEEK "O" + &lcPackTmp..ORDER
    *B804058,1 AAN Change the position [Begin].
    *@ 23,01 SAY LEFT(CUSTPO,10)
    *@ 23,12 SAY LEFT(DEPT,6)
    *@ 23,19 SAY LEFT(&lcPackTmp..STORE,8)
    *@ 23,27 SAY IIF( !EMPTY( &lcPackTmp..STORE ), XNAME1, '' )
    *@ 23,60 SAY &lcPackTmp..ORDER
    *@ 23,73 SAY COMPLETE

    *B605284,1 ABD - Change the position to up one line. [Begin]
    *@ 20,01 SAY LEFT(CUSTPO,10)
    *@ 20,12 SAY LEFT(DEPT,6)
    *@ 20,19 SAY LEFT(&lcPackTmp..STORE,8)
    *@ 20,27 SAY IIF( !EMPTY( &lcPackTmp..STORE ), XNAME1, '' )
    *@ 20,60 SAY &lcPackTmp..ORDER
    *@ 20,73 SAY COMPLETE

    @ 19,01 SAY LEFT(CUSTPO,10)
    @ 19,12 SAY LEFT(DEPT,6)
    @ 19,19 SAY LEFT(&lcPackTmp..STORE,8)
    @ 19,27 SAY IIF( !EMPTY( &lcPackTmp..STORE ), XNAME1, '' )
    @ 19,60 SAY &lcPackTmp..ORDER
    @ 19,73 SAY COMPLETE
    *B605284,1 ABD - [End]
    
    *B804058,1 AAN Change the position [End].

    lcShipVia  = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
    *B804058,1 AAN Change the position [Begin].
    *@ 26,56 SAY LEFT(lcShipVia ,15 )
    
    *B605284,1 ABD - Change the position to up one line. [Begin]
    *@ 22,57 SAY LEFT(lcShipVia ,15 )
    @ 21,57 SAY LEFT(lcShipVia ,15 )
    *B605284,1 ABD - [End]
    
    *B804058,1 AAN Change the position [End].
    *** Print packing slip lines
    Y   = '  '    &&Used in scale TAK 10/02/94
    lcScale = '^' &&Used in scale TAK 10/02/94 
    *B804058,1 AAN Change the position [Begin].
    *ROW    = 31
    ROW    = 26
    *B804058,1 AAN Change the position [End].
 
    *B804361,1 HBG 15/08/2001 Use the temp PACk ine file to print from [Begin]
    *SELE PACK_LIN
    SELECT (lcPakLnTmp)
    *B804361,1 [END]
    IF llNewPack     
      SEEK XPACK_NO
    ENDIF
  
    SCAN WHILE PACK_NO = XPACK_NO 
      SELECT STYLE
      SEEK &lcPakLnTmp..Style
      IF Scale<>lcScale  
        lcScale=Scale
        SELECT SCALE 
        @ ROW,37 SAY lcScale
        *B804058,1 AAN Change the position [Begin].
        *@ ROW,39 SAY IIF(SEEK( 'S'+lcScale ),;
                         SZ1+Y+SZ2+Y+SZ3+Y+SZ4+Y+SZ5+Y+SZ6 ,Y )
        *@ ROW,39 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,5),5,' ')
        *@ ROW,46 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,5),5,' ')
        *@ ROW,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,5),5,' ')
        *@ ROW,58 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,5),5,' ')
        *@ ROW,64 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,5),5,' ')
        *@ ROW,70 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,5),5,' ')
        @ ROW,39 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,4),4,' ')
        @ ROW,45 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,4),4,' ')
        @ ROW,50 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,4),4,' ')
        @ ROW,55 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,4),4,' ')
        @ ROW,60 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,4),4,' ')
        @ ROW,65 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,4),4,' ')
  
        *B804058,1 AAN Change the position [End].
        ROW = ROW + 1     
      ENDIF
      *B804361,1 HBG 15/08/2001 Use the temp PACk ine file to print from [Begin]
      *SELECT PACK_LIN
      SELECT (lcPakLnTmp)
      *B804361,1 [END]
      *B804058,1 AAN Fix a bug that Box numbers are wrong always showing 0-0 [Begin].
      *@ ROW,01 SAY FROM_CRT  PICTURE '9999'
      
      *B804361,1 HBG 15/08/2001 Comment this block of code because we allready now use 
      *B804361,1                the temp PACk ine file to print from [Begin]
      *SELECT (lcPakLnTmp)
      *LOCATE FOR Pack_No==PACK_LIN.Pack_No AND Line_No>=PACK_LIN.Line_No
      *IF FOUND()
      *  IF Line_No>PACK_LIN.Line_No
      *    IF !BOF()
      *      Skip -1
      *    ENDIF
      *  ENDIF
      *ELSE
      *  CALCULATE MAX(Line_No) FOR Pack_No==PACK_LIN.Pack_No TO lnLineNo
      *  LOCATE FOR Pack_No==PACK_LIN.Pack_No AND Line_No=lnLineNo
      *ENDIF
      *SELECT PACK_LIN
      *B804361,1 [END]
       
      @ ROW,01 SAY &lcPakLnTmp..FROM_CRT  PICTURE '9999'
      *B804058,1 AAN Fix a bug that Box numbers are wrong always showing 0-0 [End].
      @ ROW,06 SAY '-'
      *B804058,1 AAN Fix a bug that Box numbers are wrong always showing 0-0 [Begin].
      *@ ROW,07 SAY TO_CRT    PICTURE '999'
      @ ROW,07 SAY &lcPakLnTmp..TO_CRT    PICTURE '999'
      *B804058,1 AAN Fix a bug that Box numbers are wrong always showing 0-0 [End].
      
      *B804361,1 HBG 15/08/2001 Get # of lines for same style-Color-Qty in same range of Cartons [Begin
      lnDenemort= &lcPakLnTmp..TO_CRT - &lcPakLnTmp..FROM_CRT  + 1
      *B804361,1 [Begin]
      
      *B804058,1 AAN Change the position [Begin].
      *@ ROW,11 SAY LEFT(STYLE,18)
      @ ROW,12 SAY LEFT(STYLE,18)
      *B804058,1 AAN Change the position [End].
      *B804058,1 AAN Change the position [Begin].
      *@ ROW,30 SAY TotQty  PICTURE '@ 99999'
      @ ROW,31 SAY TotQty  PICTURE '@ 99999'
      *B804058,1 AAN Change the position [Begin].

      *B804058,1 AAN Change the position [Begin].
      *@ ROW,39 SAY Qty1    PICTURE '999'
      *@ ROW,44 SAY Qty2    PICTURE '999'
      *@ ROW,49 SAY Qty3    PICTURE '999'
      *@ ROW,54 SAY Qty4    PICTURE '999'
      *@ ROW,59 SAY Qty5    PICTURE '999'
      *@ ROW,64 SAY Qty6    PICTURE '999'
      *@ ROW,40 SAY PADL(SUBSTR(ALLTRIM(STR(Qty1)),1,5),5,' ')
      *@ ROW,46 SAY PADL(SUBSTR(ALLTRIM(STR(Qty2)),1,5),5,' ')
      *@ ROW,52 SAY PADL(SUBSTR(ALLTRIM(STR(Qty3)),1,5),5,' ')
      *@ ROW,58 SAY PADL(SUBSTR(ALLTRIM(STR(Qty4)),1,5),5,' ')
      *@ ROW,64 SAY PADL(SUBSTR(ALLTRIM(STR(Qty5)),1,5),5,' ')
      *@ ROW,70 SAY PADL(SUBSTR(ALLTRIM(STR(Qty6)),1,5),5,' ')
      *B804361,1 HBG 15/08/2001 Divide the Qty by  # of lines for same style-Color-Qty
      *B804361,1                in same range of Cartons [Begin]
      *@ ROW,40 SAY PADL(SUBSTR(ALLTRIM(STR(Qty1)),1,4),4,' ')
      *@ ROW,45 SAY PADL(SUBSTR(ALLTRIM(STR(Qty2)),1,4),4,' ')
      *@ ROW,50 SAY PADL(SUBSTR(ALLTRIM(STR(Qty3)),1,4),4,' ')
      *@ ROW,55 SAY PADL(SUBSTR(ALLTRIM(STR(Qty4)),1,4),4,' ')
      *@ ROW,60 SAY PADL(SUBSTR(ALLTRIM(STR(Qty5)),1,4),4,' ')
      *@ ROW,65 SAY PADL(SUBSTR(ALLTRIM(STR(Qty6)),1,4),4,' ')
      @ ROW,40 SAY PADL(SUBSTR(ALLTRIM(STR(Qty1/lnDenemort)),1,4),4,' ') 
      @ ROW,45 SAY PADL(SUBSTR(ALLTRIM(STR(Qty2/lnDenemort)),1,4),4,' ') 
      @ ROW,50 SAY PADL(SUBSTR(ALLTRIM(STR(Qty3/lnDenemort)),1,4),4,' ') 
      @ ROW,55 SAY PADL(SUBSTR(ALLTRIM(STR(Qty4/lnDenemort)),1,4),4,' ') 
      @ ROW,60 SAY PADL(SUBSTR(ALLTRIM(STR(Qty5/lnDenemort)),1,4),4,' ') 
      @ ROW,65 SAY PADL(SUBSTR(ALLTRIM(STR(Qty6/lnDenemort)),1,4),4,' ') 
      *B804361,1 [END]
      *B804058,1 AAN Change the position [End].

      *B804058,1 AAN Change the position [Begin].
      *@ ROW,67 SAY IIF(OrdHdr->SpcInst = lcRpSInst,(TOTQTY * NO_CART) / 12 ,TOTQTY * NO_CART) PICTURE '@999.99'
      @ ROW,74 SAY IIF(llRpPrtDoz AND OrdHdr.SpcInst = lcRpSInst,(TOTQTY) / 12 ,TOTQTY) PICTURE '@ 999.99'
      *B804058,1 AAN Change the position [End].

      *B804058,1 AAN Change the Condition [Begin].
      *@ ROW,82 SAY IIF(OrdHdr->SpcInst = lcRpSInst, 'DOZ' , 'EAC' )
      @ ROW,82 SAY IIF( llRpPrtDoz AND OrdHdr.SpcInst = lcRpSInst, 'DOZ' , 'EAC' )      
      *B804058,1 AAN Change the Condition [End].
      
      IF SEEK( 'S' + &lcPackTmp..ACCOUNT + Style , 'Spck_Lin' )
        Row = Row + 1
        
        IF Spck_Lin.TotQty = 0
          @ Row,01 SAY 'SKU # :  ' + Spck_Lin.Pack_Id
        ELSE
          lnCol   = 01
          lnCount = 1
          
          SELECT Spck_Lin
 
          *B804361,1 HBG 15/08/2001 Use temp pack line insted of PACK_LIN [Begin]
          *SCAN REST WHILE 'S' + Account + Style  = 'S' + &lcPackTmp..ACCOUNT + PACK_LIN->Style  .AND.;
            lnCount <= 8
          SCAN REST WHILE 'S' + Account + Style  = 'S' + &lcPackTmp..ACCOUNT + &lcPakLnTmp..Style  .AND.;
            lnCount <= 8
          *B804361,1 [END]    
            IF lnCount = 5
              Row   = Row + 1
              lnCol = 01
            ENDIF
            lcCount = STR( lnCount, 1 )
            
            @ Row, lnCol SAY Scale.Sz&lcCount + ':' + Spck_Lin.Pack_Id
            lnCol   = lnCol   + 21
            lnCount = lnCount + 1
          ENDSCAN
        ENDIF
      ENDIF
      ROW = ROW +1
      IF ROW >= MAXROW
        *** IF ROW IS GREATER THAN THE MAXIMUM ROW, THE SYSTEM EXIT THE SCAN
        *** LOOP WITHOUT MOVING THE FILE POINTER TO THE NEXT RECORD. 
        *** SO THIS RECORD LINE WILL BE PRINTED AGAIN ON THE NEXT PAGE.
        *** SO IN THIS CASE MOVE THE FILE POINTER TO THE NEXT RECORD.
        SKIP           
        EXIT
      ENDIF
    ENDSCAN
 
    IF ROW >= MAXROW 
      llNewPack=.F.     
      *--B804058,1 AAN Change the position [Begin]
      *@ 60,20 SAY REPLICATE('-',78)
      *@ 61,40 SAY 'C O N T I N U E D ...'
      *@ 62,20 SAY REPLICATE('-',78)
      @ 60,01 SAY REPLICATE('-',84)
      @ 61,36 SAY 'C O N T I N U E D ...'
      @ 62,01 SAY REPLICATE('-',84)
      *--B804058,1 AAN Change the position [End]
      SELECT (lcPackTmp)
      LOOP
    ELSE
      llNewPack=.T. 
    ENDIF
  
    ***----- SUMMARY -----***
    SELECT (lcPackTmp)
    *B804058,1 AAN Change the position [Begin].
    *@ 55,01 SAY TOT_CART
    *@ 55,27 SAY TOT_PCS
    @ 52,01 SAY TOT_CART
    @ 52,27 SAY TOT_PCS
    *B804058,1 AAN Change the position [End].

    EXIT
  ENDDO
  IF llRpPrnNot  
    =lfPrnIns( 'B', OrdHdr.ORDER, '#', 3, 25, 60, 60 )
  ENDIF  
  SELECT (lcPackTmp) 
ENDSCAN
SELECT (lcPackTmp) 

SET DEVICE TO SCREEN
RETURN

*--end of ALPKLSAL form
*!*************************************************************
*! Name : lfPrnIns.
*! Auth : Mohamed Shokry Mohamed (MHM).
*! Date : 01/30/2001
*!*************************************************************
*! Synopsis : Print the special instructions.
*!*************************************************************
*! Modifications :
*!*************************************************************
FUNCTION lfPrnIns

PARAMETERS lcType, lcCode, lcSrchFor, lnNoOfLn, lnCol, lnRow, lnSize

PRIVATE lcType, lcCode, lcSrchFor, lnNoOfLn, lnCol, lnRow, lnSize ,lnAlias, lnCountr

lnSize   = IIF( lnSize > 75, 75, lnSize )
lnCountr = 1
lnAlias  = SELECT()

IF SEEK( lcType + lcCode, 'NotePad' )
  lnMline = 1
  SELECT NotePad
  DO WHILE Type + ALLTRIM(Key) = lcType + lcCode .AND. lnCountr <= lnNoOfLn
    
    IF LEFT(MLINE(MNOTES,lnMline),1) <> '*'
      @ lnRow, lnCol SAY MLINE(MNOTES,lnMline)
      lnRow    = lnRow    + 1
      lnCountr = lnCountr + 1
    ENDIF
    lnMline = lnMline + 1
  ENDDO

ENDIF
SELECT ( lnAlias )
RETURN .T.

*-- EOF( lfPrnIns ) 
