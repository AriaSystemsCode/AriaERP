*:***************************************************************************
*: Program file  : ALPKTKC.PRG
*: Program desc. : SALES ORDER ALLOCATION PICKING TICKET FORM "C".
*: Date          : 08/11/2002
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: B606361
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : lpPrtSku   , lpNewFoter , lpOldFoter , lpHrdLabls , lpSidLabls
*:               : lpFilEmpLn
*:    Functions  : lfContuLoc() , lfGetLoc() , lfPrnStrn()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKC
*:***************************************************************************
*: Modifications :
*: B606668,1 ASH 11/19/2002 1-If the form prints ALT ship address it shifted down.
*: B606668,1                2-Move the ship to and sold to address to the left.										
***************************************************************************

*--initialize the variables.
STORE SPACE(1) TO lcStrToPrn , lcSkuSize , lcStr
STORE 0.00 TO XVALUE , XTOTQTY
STORE .F. TO llNoRec                && Variable used to make the endreport function in the main program.
lnLength  = 07
lnLen     = 0
lcCustPo  = ""
llEndPT   = .F.
NEWDOC    = .T.
lnNotLine = 1

STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4

HLINE5 = lcXphone
IF LEN(HLINE3) = 0
  STORE HLINE4 TO HLINE3
  STORE HLINE5 TO HLINE4
  STORE ' '    TO HLINE5
ENDIF

*--If EOF it will return to the main program
IF EOF(lcTmpOrdL)
  llNoRec = .T.
  *-- Message <No records to display.>
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT

lcSpcOrd = ORDER("SPCK_LIN")
lcWhsOrd = ORDER("WHSLOC") 
SET ORDER TO TAG WHSLOCST IN WHSLOC
SET ORDER TO TAG SPCKLINS IN SPCK_LIN

*-- MAIN LOOP
SCAN
  SELECT (lcTmpOrdL)
  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
     XPIKTKT = PIKTKT
     XORDER  = ORDER
     XDATE   = PIKDATE
     lcDept  = ORDHDR.DEPT
     XSTORE  = PIKTKT.STORE

     SELECT ORDHDR
     XMULTI   = MULTI
     XACCOUNT = ACCOUNT
     STORE 0.00 TO XVALUE
     lcCustPo = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)

     XBTNAME  = CUSTOMER.BTNAME
     XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2') 
     XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
     XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
     
     IF LEN(TRIM(XBTADDR2)) =0
        XBTADDR2 = XBTADDR3
        XBTADDR3 = ''
     ENDIF

     *-- GET DESCRIPTIONS FOR CODED FIELDS
     SELECT CODES
     SET ORDER TO CODES IN CODES 
     XTERM_DATA = gfCodDes(ORDHDR.CTERMCODE , 'CTERMCODE')
     XSHIP_DATA = gfCodDes(ORDHDR.SHIPVIA , 'SHIPVIA')
     XDIVI_DATA = gfCodDes(ORDHDR.CDIVISION , 'CDIVISION')

     HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)

     STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
     XPIECES = 0

    *--GET THE SIZE SCALES.
    XSCALE2 = SPACE(1)
    SELECT (lcTmpOrdL)
    lcKey   = EVAL(KEY())   && Variable to hold the record number.
    XSCALE1 = SCALE
    LOCATE REST FOR ( SCALE <> XSCALE1 .AND. ORDER = XORDER  .AND. !EMPTY(SCALE) )
    IF !EOF()
      XSCALE2 = LEFT(SCALE,1)
    ENDIF
    =SEEK(lcKey)  && Back to original record number.

  ENDIF

  SELECT OrdHdr
  lcDistCntr = ""
  IF Alt_ShpTo
    lcShpTName  = trim(OrdHdr.STName)   
    laShipTo[1] = ALLTRIM(OrdHdr.cAddress1)
    laShipTo[2] = ALLTRIM(OrdHdr.cAddress2)
    laShipTo[3] = ALLTRIM(OrdHdr.cAddress3)
    laShipTo[4] = ALLTRIM(OrdHdr.cAddress4)
    laShipTo[5] = ALLTRIM(OrdHdr.cAddress5)

    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    *B606668,1 ASH 11/19/2002 (Begin) Only print 40 characters as there is no room to print the whole 3 addresses.
    XSTADDR3 = ALLTRIM(laShipTo[3]) + ' ' +ALLTRIM(laShipTo[4]) + ' ' + laShipTo[5]
    XSTADDR3 = SUBSTR(ALLTRIM(laShipTo[3]) + ' ' +ALLTRIM(laShipTo[4]) + ' ' + ALLTRIM(laShipTo[5]),1,40)
    *B606668,1 ASH 11/19/2002 (End)
    
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT CUSTOMER
    lnRecNo = RECNO()       && Variable to hold the record number.

    IF !EMPTY(Dist_Ctr)
      lcDistCntr = ALLTRIM(Dist_Ctr)
      =SEEK('S' + XACCOUNT + lcDistCntr)
    ENDIF

    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 ) 
    XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 ) 
    XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )

    IF !EMPTY(lcDistCntr)
      IF BETWEEN(lnRecNo , 1 , RECCOUNT('CUSTOMER'))
        GOTO lnRecNo IN CUSTOMER
      ENDIF
    ENDIF         

    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF

  SELECT ORDHDR
  *-- START PRINTING
  DO lpHrdLabls

  *-- LINE LOOP
  SELECT (lcTmpOrdL)
  NEWDOC  = .T.
  XTOTQTY = 0
  ROW     = 23
    
  DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT  .AND. LINENO <> 0 .AND. IIF(PIKTKT = "******",ORDLINE.CWareCode = lcWareh,.T.)
    WAIT WINDOW 'Printing Pick Ticket ...' + PIKTKT NOWAIT
    IF Row >= 53
      DO lpOldFoter
      DO lpHrdLabls
      Row = 23
    ENDIF

    *-- Modified to get the Style/Color location.     
    SELECT Style
    IF SEEK(&lcTmpOrdL..Style)
      XSTYDESC   = DESC
      lcStyLocat = Location
      lcScale    = Scale
    ELSE
      XSTYDESC   = ''
      lcStyLocat = ''
      lcScale    = ''
    ENDIF
    
    *-- To get the color description.
    lcClrDesc = gfCodDes(SUBSTR(Style.Style,lnClrPosM1,lnClrLnM1) ,'COLOR')

    SELECT (lcTmpOrdL)
    @ ROW,00 SAY STYLE
    @ ROW,29 SAY ALLTRIM(lcScale)
    @ ROW,30 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,34 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,38 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,42 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,46 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,50 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,54 SAY PIK7   PICTURE '@Z 999'
    @ ROW,57 SAY PIK8   PICTURE '@Z 999'
    @ ROW,60 SAY TOTPIK PICTURE '99999'
    
    IF llRpStyPrc
      @ ROW,66 SAY PRICE PICTURE '9999.99'
    ENDIF
    @ ROW,74 SAY TOTPIK PICTURE '9999'
    
    DO lpSidLabls
    ROW = ROW + 1

    IF !llRpSkuBck
      @ ROW,00 SAY 'STYLE DESC.:'+XStyDesc
      @ ROW,22 SAY IIF(llRpPrtClr,'CLR DESC.: ' + lcClrDesc, '')
      IF llRpStyLoc
        =lfGetLoc()
        IF !EMPTY(lcStr)
          @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
          DO lpSidLabls
          Row = Row + 1
          IF LEN(lcStr) > 18
            =lfContuLoc()
          ENDIF
        ELSE
          DO lpSidLabls
          Row = Row + 1  
        ENDIF
      ENDIF  
    ELSE
      IF SEEK('P' + ORDHDR.Account + &lcTmpOrdL..Style , 'SPCK_LIN')
        @ ROW,00 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
        @ ROW,25 SAY 'STYLE DESC.:'+XStyDesc
        @ ROW,59 SAY IIF(llRpPrtClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,10), '')
        DO lpSidLabls
        Row = Row + 1
      ELSE
        DO lpPrtSku      && Procedure to print the Style/Color Sku no.
        IF !llRpSkuSiz
          @ ROW,00 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
          @ ROW,IIF(LEN(lcStrToPrn)>=40,41,LEN(lcStrToPrn)+2) SAY 'STYLE DESC.:'+XStyDesc
          lcCol=IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34)
          @ ROW,lcCol SAY IIF(llRpPrtClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,76-lcCol), '')
          DO lpSidLabls
          Row = Row + 1
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,00 SAY 'BINS    :'+ SUBSTR(lcStr,1,85)
              DO lpSidLabls
              Row = Row + 1
              IF LEN(lcStr) > 85
                =lfContuLoc()
              ENDIF
            ELSE
              DO lpSidLabls
              Row = Row + 1  
            ENDIF
          ENDIF  
          DO lpSidLabls
          Row = Row + 1
        ELSE
          @ ROW,00 SAY 'STYLE DESC.:'+XStyDesc
          @ ROW,33 SAY IIF(llRpPrtClr,'CLR DESC.:' + lcClrDesc, '')
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
              DO lpSidLabls
              Row = Row + 1
              IF LEN(lcStr) > 18
                =lfContuLoc()
              ENDIF  
            ELSE
              DO lpSidLabls
              Row = Row + 1  
           ENDIF
          ELSE
            DO lpSidLabls
            Row = Row + 1  
          ENDIF  
          = lfPrnStrn() 
          lcSkuSize=lcStrToPrn+' '+lcSkuSize
          @ ROW,00 SAY SUBSTR(lcSkuSize,1,86)
          DO lpSidLabls
          Row = Row + 1  
        ENDIF
      ENDIF  
    ENDIF  

    *-- To print the color description, And the 
    *-- Style/Color location.
    DO lpSidLabls
    Row = Row + 1

    *--SUM ORDER TOTALS
    XTOTQTY = XTOTQTY+ TOTQTY
    XVALUE  = XVALUE + TOTPIK * PRICE
    XPIECES = XPIECES + TOTPIK

    SKIP                        
  ENDDO

  IF (XORDER # &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) ) OR (XORDER = &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) AND XPIKTKT # &lcTmpOrdL..PIKTKT )
    SKIP - 1  
  ENDIF
  *-- END PRINT LINE LOOP

  IF NewDoc
    IF llRpOrdNot
      SELECT NotePad
      IF SEEK('B' + OrdHdr.Order)
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
          
          @ ROW,00 SAY MLINE(MNOTES,lnNotLine)
          DO lpSidLabls
          ROW = ROW + 1
          lnNotLine = lnNotLine + 1
        ENDDO
    
        SET MEMOWIDTH TO lnOldMemW
      ENDIF
    ENDIF  
   
    lnNotLine = 1
    llEndPT = .T.
    DO lpFilEmpLn        
    DO lpNewFoter        
  ELSE
    llEndPT = .F.
    DO lpFilEmpLn   
    DO lpOldFoter   
    LOOP
  ENDIF

  SELECT (lcTmpOrdL)
  IF PIKTKT = "******" .AND. LINENO <> 0 .AND. !EOF()
    SKIP -1
  ENDIF
ENDSCAN
SET DEVICE TO SCREEN

SET ORDER TO &lcSpcOrd IN SPCK_LIN
SET ORDER TO &lcWhsOrd IN WHSLOC

SELECT (lcTmpOrdL)
WAIT CLEAR
RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpFilEmpLn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/11/2002
*! Purpose   : Fill the empty pick ticit lines.
*!*************************************************************
*! Called from : ALPKTKC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpFilEmpLn

DO WHILE Row <= 53
  DO lpSidLabls 
  Row = Row + 1
ENDDO

*--End of lpFilEmpLn.
*!*************************************************************
*! Name      : lpSidLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/11/2002
*! Purpose   : Print the Side labels.
*!*************************************************************
*! Called from : ALPKTKC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpSidLabls

DO CASE
*B606668,1 ASH 11/19/2002 (Begin) Move the ship to & company address 5 charcters to the left.
  CASE ROW = 34 .OR. ROW = 50
    @ ROW,90 SAY IIF(llPrntComp , HLINE1 , '')
  CASE ROW = 35 .OR. ROW = 51
    @ ROW,90 SAY IIF(llPrntComp , HLINE2 , '')
  CASE ROW = 36 .OR. ROW = 52
    @ ROW,90 SAY IIF(llPrntComp , HLINE3 , '')
  CASE ROW = 37 .OR. ROW = 53
    IF llEndPT .AND. Row = 53 .AND. NewDoc
      @ 53,00 SAY lcRpMsg1
    ENDIF
    @ ROW,90 SAY IIF(llPrntComp , HLINE4 , '')
  CASE ROW = 23 .OR. ROW = 40
    @ ROW,90 SAY XSTNAME
  CASE ROW = 24 .OR. ROW = 41
    @ ROW,90 SAY XSTADDR1
  CASE ROW = 25 .OR. ROW = 42
    @ ROW,90 SAY XSTADDR2
  CASE ROW = 26 .OR. ROW = 43
    @ ROW,90 SAY XSTADDR3
*B606668,1 ASH 11/19/2002 (End)
  CASE ROW = 29 .OR. ROW = 46
    @ ROW,086 SAY lcCustPO
    @ ROW,099  SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
    @ ROW,107 SAY lcDept
    @ ROW,117 SAY XORDER
    
  CASE ROW = 30 .OR. ROW = 47
    @ ROW,101 SAY XPIKTKT
    @ ROW,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
    
ENDCASE

*--End of lpSidLabls.
*!*************************************************************
*! Name      : lpHrdLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/11/2002
*! Purpose   : Print the Header labels.
*!*************************************************************
*! Called from : ALPKTKC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpHrdLabls()
*!*************************************************************
PROCEDURE lpHrdLabls
*B606668,1 ASH 11/19/2002 (Begin) Move the ship to & company address 5 charcters to the left.
*-- Line 0
@ 02,001 SAY IIF(llPrntComp , HLINE1 , '')
@ 02,090 SAY IIF(llPrntComp , HLINE1 , '')

*-- Line 1
@ 03,001 SAY IIF(llPrntComp , HLINE2 , '')
@ 03,090 SAY IIF(llPrntComp , HLINE2 , '')

*-- Line 2
@ 04,001 SAY IIF(llPrntComp , HLINE3 , '')
@ 04,090 SAY IIF(llPrntComp , HLINE3 , '')

*-- Line 3
@ 05,01 SAY IIF(llPrntComp , HLINE4 , '')
@ 05,60 SAY SUBSTR(ALLTRIM(XDIVI_DATA),1,11)
@ 05,73 SAY XPIKTKT
@ 05,90 SAY IIF(llPrntComp , HLINE4 , '')


*-- Line 5
@ 07,60 SAY ORDHDR.APPROVAL
@ 07,73 SAY XDATE
@ 07,90 SAY XSTNAME
  
*-- Line 6
@ 08,90 SAY XSTADDR1
  
*-- Line 7
@ 09,90 SAY XSTADDR2

*-- Line 8
@ 10,03 SAY XBTNAME
@ 10,46 SAY XSTNAME
@ 10,90 SAY XSTADDR3

*-- Line 9
@ 11,003 SAY XBTADDR1
@ 11,046 SAY XSTADDR1
   
*-- Line 10
@ 12,003 SAY XBTADDR2
@ 12,046 SAY XSTADDR2
  
*-- Line 11
@ 13,003 SAY XBTADDR3
@ 13,046 SAY XSTADDR3
@ 13,086 SAY lcCustPO
@ 13,099 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
@ 13,107 SAY lcDept
@ 13,117 SAY XORDER

*-- Line 13
@ 14,101 SAY XPIKTKT
@ 14,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10) 

*-- Line 15
@ 17,090 SAY IIF(llPrntComp , HLINE1 , '')

*-- Line 16
@ 18,001 SAY XACCOUNT
@ 18,012 SAY XORDER
@ 18,024 SAY ORDHDR.REP1
@ 18,028 SAY ORDHDR.REP2
@ 18,032 SAY SUBSTR(DTOC(ORDHDR.ENTERED),1,5)
@ 18,039 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
@ 18,045 SAY ORDHDR.COMPLETE
@ 18,054 SAY SUBSTR(XTERM_DATA,1,11)
@ 18,066 SAY XSTORE         PICTURE '####'
@ 18,070 SAY lcCustPO PICTURE '########'  
@ 18,078 SAY lcDept
@ 18,090 SAY IIF(llPrntComp , HLINE2 , '')

*-- Line 17
@ 19,090 SAY IIF(llPrntComp , HLINE3 , '')
@ 20,090 SAY IIF(llPrntComp , HLINE4 , '')
*B606668,1 ASH 11/19/2002 (End)
*-- Line 18 PRINT 1 ST SIZE SCALE
@ 21,29 SAY ALLTRIM(XSCALE1)
XSCALE = GETSCALE( XSCALE1,SPACE(1))
@ 21,31 SAY SUBSTR(ALLTRIM(SCALE.SZ1),1,3)
@ 21,35 SAY SUBSTR(ALLTRIM(SCALE.SZ2),1,3)
@ 21,39 SAY SUBSTR(ALLTRIM(SCALE.SZ3),1,3)
@ 21,43 SAY SUBSTR(ALLTRIM(SCALE.SZ4),1,3)
@ 21,47 SAY SUBSTR(ALLTRIM(SCALE.SZ5),1,3)
@ 21,51 SAY SUBSTR(ALLTRIM(SCALE.SZ6),1,3)
@ 21,55 SAY SUBSTR(ALLTRIM(SCALE.SZ7),1,3)
@ 21,59 SAY SUBSTR(ALLTRIM(SCALE.SZ8),1,3)


*-- Line 18 PRINT 2ND. SIZE SCALE
IF !EMPTY(XSCALE2)
 @ 22,29 SAY ALLTRIM(XSCALE2)
  XSCALE = GETSCALE( XSCALE2,SPACE(1))
  @ 22,31 SAY SUBSTR(ALLTRIM(SCALE.SZ1),1,3)
  @ 22,35 SAY SUBSTR(ALLTRIM(SCALE.SZ2),1,3)
  @ 22,39 SAY SUBSTR(ALLTRIM(SCALE.SZ3),1,3)
  @ 22,43 SAY SUBSTR(ALLTRIM(SCALE.SZ4),1,3)
  @ 22,47 SAY SUBSTR(ALLTRIM(SCALE.SZ5),1,3)
  @ 22,51 SAY SUBSTR(ALLTRIM(SCALE.SZ6),1,3)
  @ 22,55 SAY SUBSTR(ALLTRIM(SCALE.SZ7),1,3)
  @ 22,59 SAY SUBSTR(ALLTRIM(SCALE.SZ8),1,3)
  
ENDIF

*--End of lpHrdLabls.
*!*************************************************************
*! Name      : lpOldFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/11/2002
*! Purpose   : Print the form footer at in the middle of a spacific 
*!           : pick ticket. 
*!*************************************************************
*! Called from : ALPKTKC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpOldFoter
*B606668,1 ASH 11/19/2002 (Begin) Move the ship to & company address 5 charcters to the left.
@ 55,090 SAY XSTNAME
@ 56,090 SAY XSTADDR1
@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 58,090 SAY XSTADDR2
@ 59,090 SAY XSTADDR3
*B606668,1 ASH 11/19/2002 (End)
@ 62,086 SAY lcCustPO
@ 62,099 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
@ 62,107 SAY lcDept
@ 62,115 SAY XORDER
@ 63,101 SAY XPIKTKT
@ 63,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
ROW = ROW + 1

*--End of lpOldFoter.
*!*************************************************************
*! Name      : lpNewFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/11/2002
*! Purpose   : Print the form footer at the end of a spacific 
*!           : pick ticket.
*!*************************************************************
*! Called from : ALPKTKC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpNewFoter

@ 54,000 SAY lcRpMsg2
@ 55,000 SAY lcRpMsg3
*B606668,1 ASH 11/19/2002 (Begin) Move the ship to & company address 5 charcters to the left.
@ 56,090 SAY XSTNAME
@ 57,090 SAY XSTADDR1

=SEEK('O'+XORDER,'ORDHDR')
lnCol = 11
IF SUBSTR(OrdHdr.Note1, 1, 1) <> '*'
  @ 58,lnCol SAY OrdHdr.Note1
  lnCol = 42
ENDIF
@ 58,lnCol SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1)<>'*', SUBSTR(OrdHdr.Note2,1,25), '')
@ 58,075 SAY XPIECES PICTURE '999999' 
@ 58,090 SAY XSTADDR2
@ 59,090 SAY XSTADDR3
*B606668,1 ASH 11/19/2002 (End)
@ 62,060 SAY SUBSTR(XSHIP_DATA,1,11) 
@ 62,076 SAY IIF(llRpStyPrc , XVALUE , '') PICTURE '99999.99'
@ 62,086 SAY lcCustPO
@ 62,099 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
@ 62,107 SAY lcDept
@ 62,117 SAY XORDER
@ 63,101 SAY XPIKTKT
@ 63,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
XVALUE = 0.00
ROW = ROW + 1

*--End of lpNewFoter.
*!*************************************************************
*! Name      : lpPrtSku
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/11/2002
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKC.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpPrtSku
*!*************************************************************
PROCEDURE lpPrtSku

IF ! SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
  lcStrToPrn = " "
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  = SEEK('S'+Style.Scale,'Scale')
  = SEEK('M' + ORDHDR.Account , 'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1 + SkuTmpl.Len2 + SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 
  = SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
  lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnDime1)

  lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
  lnLength   = LEN(lcStrToPrn)+2
  lnLocCol = LEN(lcStrToPrn) + 5

ELSE
  @ ROW,01 SAY SUBSTR(Sku,1,8)
  lnLocCol = 35
  @ ROW,17 SAY 'CUSTOMER SKU #'
 
ENDIF
RETURN

*--End of lpPrtSku.
*!*************************************************************
*! Name      : lfPrnStrn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/11/2002
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfPrnStrn

lcAlias   = ALIAS()
lcSkuSize = ' '
lcKey     = ' '
IF SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style , 'Spck_Lin')
  lcKey='S' + ORDHDR.Account + &lcTmpOrdL..Style
  lnSep=1
  Q=1
  W=STR(Q,1)

  X=1
  Z=STR(X,1) 
  SELECT Spck_Lin
  SCAN REST WHILE 'S' + &lcTmpOrdL..Account + &lcTmpOrdL..Style = lcKey

    IF &LnTemp..Qty&Z > 0
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

*--End of lfPrnStrn. 
*!*************************************************************
*! Name      : lfGetLoc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/11/2002
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetLoc

PRIVATE lcAlias
lcAlias = ALIAS()
lcStr = " "
SELECT WhsLoc
IF SEEK(&lcTmpOrdL..Style)
  SCAN REST WHILE Style= &lcTmpOrdL..Style
    lcStr = lcStr + " " + ALLTRIM(cLocation)
  ENDSCAN
  lcStr = ALLTRIM(lcStr)
  lnLen = LEN(lcStr)
ENDIF
SELECT (lcAlias)

*--End of lfGetLoc.
*!*************************************************************
*! Name      : lfContuLoc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/11/2002
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfContuLoc

FOR I = 12 TO lnLen  
   @ ROW ,00 SAY SUBSTR(lcStr,I,86) 
   I=I+86
   DO lpSidLabls
   Row = Row + 1
ENDFOR

*--End of lfContuLoc.