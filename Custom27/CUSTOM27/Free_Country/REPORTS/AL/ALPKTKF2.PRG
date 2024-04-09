*:***************************************************************************
*: Program file  : ALPKTKF2.PRG
*: Program desc. : CUSTOMIZED PICK TICKET FOR FREE COUNTRY.
*: 				   The program is a conversion from 2.6 ..ALO820Z.PRG (REFERE TO C101302)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11"
*! Date          : 03/19/2001
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102247
*: 
*:***************************************************************************
*: Calls : lfContuLoc , lfGetLoc , lfPrnStrn
*:    Procedures : lpPrtSku , lpNewFoter , lpOldFoter , lpHrdLabls , lpSidLabls
*:               : lpFilEmpLn
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKFC
*:***************************************************************************
*: Modifications :
*: B605674,1 BWA 03/27/2002 Fix the bug of duplicating the color code.
*: B606869,1 BWA 01/15/2003 Fix the bug of printing wrong scale header.
*: B607408,1 KHM 10/07/2003 Fix the bug of printing the wrong custpo.
*:B039684 MMR 09/07/2005  Exclude the notes from the report.
***************************************************************************

*-- Get the answers for the color and location printing,
*-- either from the memory file - if found - or from the user.
IF FILE('&gcDataDir.ALPKTKFC.MEM')
  RESTORE FROM &gcDataDir.ALPKTKFC.MEM ADDITIVE
ELSE
  STORE SPACE (1) TO  lcMClr
ENDIF
llPrntClr = (lcMClr = 'Y')

IF EMPTY(lcMClr) 
  lcMClr    = IIF(gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you wish to print the color description?')=1,'Y','N')
  llPrntClr = (lcMClr = 'Y')
ENDIF
SAVE ALL LIKE lcM* TO &gcDataDir.ALPKTKFC.MEM
RELEASE ALL LIKE lcM*

           *--Section of getting the address from the main program --*
HLINE1  = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1]) TO HLINE2
STORE TRIM(laCompAdd[2]) TO HLINE3
STORE TRIM(laCompAdd[3]) TO HLINE4
IF EMPTY(HLINE3)
  STORE HLINE4 TO HLINE3
  STORE ''     TO HLINE4
ENDIF

*--Change the tag in this files to be used in the program.
SET ORDER TO TAG Spcklins IN SPCK_LIN
SET ORDER TO TAG Whslocst IN WHSLOC


*--THE COLOR LENGTH
STORE 0 TO lnClrLnFr , lnClrPosFr
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnFr  = LEN(laItemSeg[lnCount,3])
    lnClrPosFr = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--Inialize variables
*1) llRpStyLoc   && Print Style Location?
*2) llRpSkuBck   && Print Sku/Pack?
*3) llRpSkuSiz   && Print Sku by Size?
*4) llRpStyPrc   && Print Style Prices?

STORE ' ' To lcStrToPrn , lcSkuSize , lcSclStrn , lcStr
STORE 0   To lnLen , ROW , lnRec , lnSca
STORE .F. To llNoRec , llEndPT
STORE .T. To NEWDOC , llScale     && Flag to print the size scales at the first page only
lnNotLine = 1
lnLength  = 7

*-- This file holds all the lines of the selected pick tickets.
SELECT (lcTmpOrdL)
SET FILTER TO cGrupDetal = "D"
LOCATE
IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT

*--New Index we will use it in case getting the scale.
SELECT (lcTmpOrdL)
INDEX ON PikTkt + Order + cGrupDetal + SCALE + STR(LINENO,6) TAG 'SCLTEMP'
SET ORDER TO (lcTmpOrdL)
LOCATE

*-- MAIN LOOP
DO WHILE !EOF() AND INKEY() <>32
  WAIT WINDOW 'Printing Pick Ticket ...' + PIKTKT NOWAIT
  SELECT (lcTmpOrdL)
  IF NEWDOC
    lcPikTkt   = PIKTKT
    lcOrder    = ORDER
    lcPikDate  = PikDate
    lcStore    = PikTkt.Store
    lcAccount  = PIKTKT.ACCOUNT
    lcCustPo   = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)
    STORE 0 TO XVALUE , lnSca

    *** GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
    SEEK IIF(lcStore = SPACE(8),'M'+lcAccount,'S'+lcAccount+lcStore)
    XSKU = IIF(CUSTOMER.cskucode = 'Y',.T.,.F.)    && IF THIS ACCT HAS SKU ON FILE

    =lfSolSpAdr()        && function in the main program (ALPKTK) to get the adrress.
    XBTNAME  = lcSolTName

    *B605674,1 BWA 03/27/2002 Remove the phone from the address.[START]
    *XBTADDR1 = laSoldTo[1]
    *XBTADDR2 = laSoldTo[2]
    *XBTADDR3 = TRIM(laSoldTo[3])

    XBTADDR1 = IIF(ATC(' Phone# ',laSoldTo[1]) > 0 , ALLTRIM(LEFT(laSoldTo[1],ATC(' Phone# ',laSoldTo[1])-1 )) , ALLTRIM(laSoldTo[1]))
    XBTADDR2 = IIF(ATC(' Phone# ',laSoldTo[2]) > 0 , ALLTRIM(LEFT(laSoldTo[2],ATC(' Phone# ',laSoldTo[2])-1 )) , ALLTRIM(laSoldTo[2]))
    XBTADDR3 = IIF(ATC(' Phone# ',laSoldTo[3]) > 0 , ALLTRIM(LEFT(laSoldTo[3],ATC(' Phone# ',laSoldTo[3])-1 )) , ALLTRIM(laSoldTo[3]))
    *B605674,1 BWA 03/27/2002.[END]

    IF LEN(TRIM(laSoldTo[2])) =0
      XBTADDR2 = laSoldTo[3]
      XBTADDR3 = ''
    ENDIF

    XSTNAME  = lcShpTName

    *B605674,1 BWA 03/27/2002 Remove the phone from the address.[START]
    *XSTADDR1 = laShipTo[1]
    *XSTADDR2 = laShipTo[2]
    *XSTADDR3 = TRIM(laShipTo[3])

    XSTADDR1 = IIF(ATC(' Phone#',laShipTo[1]) > 0 , ALLTRIM(LEFT(laShipTo[1],ATC(' Phone#',laShipTo[1])-1 )) , ALLTRIM(laShipTo[1]))
    XSTADDR2 = IIF(ATC(' Phone#',laShipTo[2]) > 0 , ALLTRIM(LEFT(laShipTo[2],ATC(' Phone#',laShipTo[2])-1 )) , ALLTRIM(laShipTo[2]))
    XSTADDR3 = IIF(ATC(' Phone#',laShipTo[3]) > 0 , ALLTRIM(LEFT(laShipTo[3],ATC(' Phone#',laShipTo[3])-1 )) , ALLTRIM(laShipTo[3]))
    *B605674,1 BWA 03/27/2002.[END]

    IF LEN(TRIM(laShipTo[2])) =0
      XSTADDR2 = laShipTo[3]
      XSTADDR3 = ''
    ENDIF

    *-- GET DESCRIPTIONS FOR CODED FIELDS
    SELECT CODES
    SET ORDER TO CODES IN CODES

    lcTerms  = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA = gfCodDes(ORDHDR.SHIPVIA,'SHIPVIA')
    lcDvsion = LEFT(gfCodDes(ORDHDR.CDIVISION,'CDIVISION'),7)

    *--Get all scales for the current PK
    SELECT (lcTmpOrdL)
    lcKey = EVAL(KEY())
    SET ORDER TO TAG 'SCLTEMP'

    *B606869,1 BWA 01/15/2003 Seek for the right record.[START]
    lcPikKey = PIKTKT+ORDER+'D'
    *B606869,1 BWA 01/15/2003.[END]

    LOCATE
    lnRec = RECCOUNT()
    *B606869,1 BWA 01/15/2003 Get this line before the locate of the file 
    *B606869,1 BWA 01/15/2003 and Initial the array hold the scales.[START]
    *lcPikKey = PIKTKT+ORDER+'D'
    DIMENSION laScales[1]
    STORE SPACE(0) TO laScales
    =SEEK(lcPikKey)
    *B606869,1 BWA 01/15/2003.[END]

    I = 1
    FOR I = 1 TO lnRec
      X = STR(I,1)
      IF EOF() OR !FOUND()
        EXIT
      ENDIF
      lnSca =lnSca +1
      DIMENSION laScales[lnSca]
      laScales[lnSca] = SCALE
      LOCATE REST WHILE PikTkt + Order +cGrupDetal + SCALE+ STR(LINENO,6) = lcPikKey ;
              FOR   SCALE <> laScales[lnSca]
    ENDFOR       
    SET ORDER TO TAG (lcTmpOrdL)
    =SEEK(lcKey)
    llScale = .T.
    STORE 0 TO XVALUE,XPIECES
  ENDIF

  *-----------------
  * START PRINT
  *-----------------
  SELECT (lcTmpOrdL)
  DO lpHrdLabls
    
  *-- LINE LOOP
  SELECT (lcTmpOrdL)
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6)=lcPikTkt+lcOrder+'D'
    NEWDOC  = .T.
    XTOTQTY = 0

    DO CASE
      *B606869,1 BWA 01/15/2003 Fix the bug of not printing the last address.[START]
      CASE EOF()
        NEWDOC = .T.
        EXIT
      CASE TOTPIK <= 0
        LOOP
      CASE ROW >= 53
        NEWDOC = .F.
        EXIT

      *CASE EOF()
      *  NEWDOC = .T.
      *  EXIT
      *CASE TOTPIK <= 0
      *  SKIP                              
      *  LOOP
      *CASE ROW >= 55
      *  NEWDOC = .F.
      *  EXIT
      *CASE ROW >= 51
      *  DO WHILE ROW<=55
      *    DO lpSidLabls
      *    ROW = ROW + 1
      *  ENDDO
      *  NEWDOC = .F.
      *  EXIT
      *B606869,1 BWA 01/15/2003.[END]
    ENDCASE

    *-- Modified to get the Style/Color location.     
    IF SEEK(&lcTmpOrdL..Style,"Style")
      XSTYDESC   = STYLE.DESC
      lcStyLocat = STYLE.Location
      lcScale    = STYLE.Scale
    ELSE
      XSTYDESC   = ''
      lcStyLocat = ''
      lcScale    = ''
    ENDIF
    
    *-- To get the color description.
    lcClrDesc  = SUBSTR(gfCodDes(SUBSTR(&lcTmpOrdL..STYLE,lnClrPosFr,lnClrLnFr),'COLOR'),1,15)
    SELECT (lcTmpOrdL)
    @ ROW,1  SAY STYLE

    *B605674,1 BWA 03/27/2002 Remove the duplicate of the color.[START]
    *@ ROW,14 SAY SUBSTR(STYLE,lnClrPosFr,lnClrLnFr)
    *B605674,1 BWA 03/27/2002.[END]

    @ ROW,29 SAY ALLTRIM(lcScale)
    @ ROW,31 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,35 SAY PIK2   PICTURE '@Z 9999'

    *B605674,1 BWA 03/27/2002 Move the qty 1 chr left.[START]
    *@ ROW,40 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,39 SAY PIK3   PICTURE '@Z 9999'
    *B605674,1 BWA 03/27/2002.[END]

    @ ROW,44 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,48 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,53 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,58 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,62 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,68 SAY TOTPIK PICTURE '99999'
    @ ROW,75 SAY TOTPIK PICTURE '9999'
    DO lpSidLabls
    ROW = ROW + 1

    IF !llRpSkuBck
      @ ROW,1 SAY 'STYLE DESC.:'+XStyDesc
      @ ROW,33 SAY IIF(llPrntClr,'CLR DESC.: ' + lcClrDesc, '')
      IF llRpStyLoc
        =lfGetLoc()
        IF !EMPTY(lcStr)
          @ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,10)
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
      IF SEEK('P'+lcAccount+&lcTmpOrdL..Style,'SPCK_LIN')
        @ ROW,1 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
        @ ROW,25 SAY 'STYLE DESC.:'+XStyDesc
        @ ROW,59 SAY IIF(llPrntClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,10), '')
        DO lpSidLabls
        Row = Row + 1
      ELSE
        DO lpPrtSku      && Procedure to print the Style/Color Sku no.
        IF !llRpSkuSiz
          @ ROW,00 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
          @ ROW,IIF(LEN(lcStrToPrn)>=40,41,LEN(lcStrToPrn)+2) SAY 'STYLE DESC.:'+XStyDesc
          lcCol=IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34)
          @ ROW,lcCol SAY IIF(llPrntClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,76-lcCol), '')
          DO lpSidLabls
          Row = Row + 1
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,00 SAY 'LOCATION:'+ SUBSTR(lcStr,1,85)
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
          @ ROW,33 SAY IIF(llPrntClr,'CLR DESC.:' + lcClrDesc, '')
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,10)
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
          =lfPrnStrn() 
          lcSkuSize=lcStrToPrn+' '+lcSkuSize
          @ ROW,00 SAY SUBSTR(lcSkuSize,1,86)
          IF !EMPTY(lcSkuSize)
            DO lpSidLabls
            Row = Row + 1  
          ENDIF
        ENDIF
      ENDIF  
    ENDIF  

    *-- To print the color description, And the 
    *-- Style/Color location.
    DO lpSidLabls
    Row = Row + 1

    SELECT (lcTmpOrdL)
    XTOTQTY = XTOTQTY+ TOTQTY
    XVALUE  = XVALUE + TOTPIK * PRICE
    XPIECES = XPIECES + TOTPIK

  ENDSCAN
  *-- END PRINT LINE LOOP
    
  IF NewDoc
    SELECT (lcTmpOrdL)
    *B039684 MMR 09/07/2005  Exclude the notes from the report.[Start]
    *-- To print the notepad.
    *IF llRpOrdNot
    *  SELECT NotePad
    *  IF SEEK('B' + lcOrder)
    *    lnOldMemW = SET("MEMOWIDTH")
    *    SET MEMOWIDTH TO 75
    *    lnMemLins = MEMLINES(NOTEPAD.MNOTES)
    *    @ Row,00 SAY '* -- N O T E S -- *' 
    *    DO lpSidLabls
    *    Row = Row + 1
    *    DO WHILE lnNotLine <= lnMemLins
    *      IF Row >= 55
    *        DO lpOldFoter
    *        DO lpHrdLabls
    *      ENDIF
    *      @ ROW,00 SAY MLINE(MNOTES,lnNotLine)
    *      DO lpSidLabls
    *      ROW = ROW + 1
    *      lnNotLine = lnNotLine + 1
    *    ENDDO
    *    SET MEMOWIDTH TO lnOldMemW
*        SELECT (lcTmpOrdL)
*        IF !EOF()
*          SKIP
*        ENDIF  
     * ENDIF
    *ENDIF  
    *B039684,1 MMR [End]
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

  llScale = .T.
  SELECT (lcTmpOrdL)
ENDDO
SET DEVICE TO SCREEN
RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpFilEmpLn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/22/2001
*! Purpose   : Fill the empty pick ticit lines.
*!*************************************************************
*! Called from : ALPKTKFC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpFilEmpLn

*B606869,1 BWA 01/15/2003 Fix the bug of not printing the last address.[START]
*DO WHILE Row <= 50
*  DO lpSidLabls
*  Row = Row + 1
*ENDDO

DO WHILE Row <= 53
  DO lpSidLabls
  IF ROW < 53
    Row = Row + 1
  ELSE
    EXIT
  ENDIF
ENDDO
*B606869,1 BWA 01/15/2003.[END]

*--End of lpFilEmpLn.
*!*************************************************************
*! Name      : lpSidLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/22/2001
*! Purpose   : Print the Side labels.
*!*************************************************************
*! Called from : ALPKTKFC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpSidLabls

DO CASE
  CASE ROW = 34 .OR. ROW = 50
    @ ROW,93 SAY IIF(llPrntComp , HLINE1 , '' )
  CASE ROW = 35 .OR. ROW = 51
    @ ROW,93 SAY IIF(llPrntComp , HLINE2 , '' )
  CASE ROW = 36 .OR. ROW = 52
    @ ROW,93 SAY IIF(llPrntComp , HLINE3 , '' )
  CASE ROW = 37 .OR. ROW = 53
    IF llEndPT .AND. Row = 53 .AND. NewDoc
      @ 53,000 SAY lcRpMsg1
    ENDIF  
    @ ROW,93 SAY IIF(llPrntComp , HLINE4 , '' )
  CASE ROW = 23 .OR. ROW = 40
    @ ROW,93 SAY XSTNAME
  CASE ROW = 24 .OR. ROW = 41
    @ ROW,93 SAY XSTADDR1
  CASE ROW = 25 .OR. ROW = 42
    @ ROW,93 SAY XSTADDR2
  CASE ROW = 26 .OR. ROW = 43
    @ ROW,93 SAY XSTADDR3
  CASE ROW = 30 .OR. ROW = 46
    
    *B607408,1 KHM 10/07/2003 (Begin) Print lcCustPO in all cases.
    *@ ROW,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO) 
    @ ROW,086 SAY lcCustPO
    *B607408,1 KHM 10/07/2003 (End)
    
    @ ROW,100 SAY lcStore
    @ ROW,111 SAY ORDHDR.DEPT
    @ ROW,120 SAY lcOrder
  CASE ROW = 31 .OR. ROW = 48
    @ ROW,101 SAY lcPikTkt
    @ ROW,108 SAY 'SHIP VIA: ' + SUBSTR(PSHIPVIA,1,10)
ENDCASE

*--End of lpSidLabls.
*!*************************************************************
*! Name      : lpHrdLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/22/2001
*! Purpose   : Print the Header labels.
*!*************************************************************
*! Called from : ALPKTKFC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpHrdLabls

*-- Line 0
@ 02,01 SAY IIF(llPrntComp , HLINE1 , '' )
@ 02,95 SAY IIF(llPrntComp , HLINE1 , '' )
  
*-- Line 1
@ 03,001 SAY IIF(llPrntComp , HLINE2 , '' )
@ 03,095 SAY IIF(llPrntComp , HLINE2 , '' )
  
*-- Line 2
@ 04,001 SAY IIF(llPrntComp , HLINE3 , '' )
@ 04,095 SAY IIF(llPrntComp , HLINE3 , '' )
  
*-- Line 3
@ 05,001 SAY IIF(llPrntComp , HLINE4 , '' )
@ 05,61 SAY lcDvsion
@ 05,74 SAY lcPikTkt
@ 05,095 SAY IIF(llPrntComp , HLINE4 , '' )
  
*-- Line 4
@ 07,095 SAY XSTNAME
  
*-- Line 5
@ 08,61 SAY ORDHDR.APPROVAL
@ 08,74 SAY lcPikDate
@ 08,095 SAY XSTADDR1
  
*-- Line 6
@ 09,095 SAY XSTADDR2
  
*-- Line 7
@ 10,005 SAY XBTNAME
@ 10,048 SAY XSTNAME
@ 10,095 SAY XSTADDR3
  
*-- Line 8
@ 11,005 SAY XBTADDR1
@ 11,048 SAY XSTADDR1
  
*-- Line 9
@ 12,005 SAY XBTADDR2
@ 12,048 SAY XSTADDR2
  
*-- Line 10
@ 13,005 SAY XBTADDR3
@ 13,048 SAY XSTADDR3

*B607408,1 KHM 10/07/2003 (Begin) Print lcCustPO in all cases.
*@ 13,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)
@ 13,086 SAY lcCustPO
*B607408,1 KHM 10/07/2003 (End)

@ 13,100 SAY lcStore
@ 13,111 SAY ORDHDR.DEPT
@ 13,120 SAY lcOrder
  
*-- Line 11
@ 15,101 SAY lcPikTkt
@ 15,108 SAY 'SHIP VIA: ' + SUBSTR(PSHIPVIA,1,10) 
*-- Line 13
@ 17,093 SAY IIF(llPrntComp , HLINE1 ,'' )

*-- Line 15
@ 18,1 SAY LCACCOUNT
@ 18,7 SAY lcOrder
@ 18,13 SAY ORDHDR.REP1
@ 18,17 SAY ORDHDR.REP2
@ 18,21 SAY ORDHDR.ENTERED
@ 18,30 SAY ORDHDR.START
@ 18,38 SAY ORDHDR.COMPLETE
@ 18,46 SAY SUBSTR(lcTerms,1,14)
@ 18,61 SAY lcStore         PICTURE '########'

*B607408,1 KHM 10/07/2003 (Begin) Print lcCustPO in all cases.
*@ 18,70 SAY IIF(ORDHDR.MultiPO,LEFT(lcCustPO,13),LEFT(OrdHdr.CUSTPO,13))
@ 18,70 SAY LEFT(lcCustPO,13)
*B607408,1 KHM 10/07/2003 (End)

@ 18,80 SAY ORDHDR.DEPT 
@ 18,093 SAY IIF(llPrntComp , HLINE2 , '' )
@ 19,093 SAY IIF(llPrntComp , HLINE3 , '' )

ROW = 20
IF llScale
  I = 1
  ROW = 20
  lnSca = IIF(lnSca>4,4,lnSca)
  FOR I = 1 TO lnSca
    Z = STR(I,1)
    IF Row >= 55
      NEWDOC  = .F.
      llEndPT = .F.
      DO lpFilEmpLn
      DO lpOldFoter
      EXIT
    ENDIF 
    IF !EMPTY(laScales[I])
      @ ROW,29 SAY laScales[I]
      lcScale = GETSCALE(laScales[lnSca],SPACE(1))
      =SEEK('S'+laScales[I],'SCALE')
      @ ROW,31 SAY PADL(SCALE.SZ1,4,' ')
      @ ROW,35 SAY PADL(SCALE.SZ2,4,' ')
      @ ROW,40 SAY PADL(SCALE.SZ3,4,' ')
      @ ROW,44 SAY PADL(SCALE.SZ4,4,' ')
      @ ROW,49 SAY PADL(SCALE.SZ5,4,' ')
      @ ROW,53 SAY PADL(SCALE.SZ6,4,' ')
      @ ROW,58 SAY PADL(SCALE.SZ7,4,' ')
      @ ROW,62 SAY PADL(SCALE.SZ8,4,' ')
      IF I = 1
        @ ROW,93 SAY IIF(llPrntComp , HLINE4 , '' )            
      ENDIF
      DO lpSidLabls
      ROW = ROW+1
    ENDIF   
  ENDFOR  
  lcSclStrn=lcScale
  llScale =.F.
ENDIF 
lcSclStrn=lcScale
ROW = IIF(ROW < 24 , 24 , ROW)

*--End of lpHrdLabls.
*!*************************************************************
*! Name      : lpOldFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/22/2001
*! Purpose   : Print the form footer at in the middle of a spacific 
*!           : pick ticket. 
*!*************************************************************
*! Called from : ALPKTKFC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpOldFoter

@ 55,93 SAY XSTNAME
@ 56,93 SAY XSTADDR1
@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 57,93 SAY XSTADDR2
@ 58,93 SAY XSTADDR3

*B607408,1 KHM 10/07/2003 (Begin) Print lcCustPO in all cases.
*@ 63,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  
@ 63,086 SAY lcCustPO
*B607408,1 KHM 10/07/2003 (End)

@ 63,100 SAY lcStore                  
@ 63,111 SAY ORDHDR.DEPT            
@ 63,120 SAY lcOrder
@ 64,101 SAY lcPikTkt
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(PSHIPVIA,1,10)
ROW = ROW + 1

*--End of lpOldFoter.
*!*************************************************************
*! Name      : lpNewFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/22/2001
*! Purpose   : Print the form footer at the end of a spacific 
*!           : pick ticket. 
*!*************************************************************
*! Called from : ALPKTKFC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpNewFoter

*-- Not to print the total amount of the piktkt if the 
*-- user does not want to print the style prices.
@ 54,000 SAY lcRpMsg2
@ 55,000 SAY lcRpMsg3
@ 56,093 SAY XSTNAME
@ 57,093 SAY XSTADDR1

lnCol = 11
IF SUBSTR(OrdHdr.Note1, 1, 1) <> '*'
  @ 58,lnCol SAY OrdHdr.Note1
  lnCol = 42
ENDIF
@ 58,lnCol SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1)<>'*', SUBSTR(OrdHdr.Note2,1,25), '')
@ 58,076 SAY XPIECES PICTURE '999999' 
@ 58,093 SAY XSTADDR2
@ 59,093 SAY XSTADDR3
@ 62,060 SAY SUBSTR(PSHIPVIA,1,11) 
@ 62,076 SAY IIF(llRpStyPrc , XVALUE , '' ) PICTURE '99999.99'

*B607408,1 KHM 10/07/2003 (Begin) Print lcCustPO in all cases.
*@ 63,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  
@ 63,086 SAY lcCustPO
*B607408,1 KHM 10/07/2003 (End)

@ 63,100 SAY lcStore                  
@ 63,111 SAY ORDHDR.DEPT            
@ 63,120 SAY lcOrder
@ 64,101 SAY lcPikTkt
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(PSHIPVIA,1,10)
XVALUE = 0.00
ROW = ROW + 1

*--End of lpNewFoter.
*!*************************************************************
*! Name      : lpPrtSku
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/22/2001
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKFC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpPrtSku

IF ! SEEK('S'+lcAccount+&lcTmpOrdL..Style,'Spck_Lin')
  lcStrToPrn = " "
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  = SEEK('S'+Style.Scale,'Scale')
  = SEEK('M'+lcAccount,'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 
  = SEEK('S'+lcAccount+&lcTmpOrdL..Style,'Spck_Lin')
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
*! Date      : 03/22/2001
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKFC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfPrnStrn

lcAlias=ALIAS()
lcSkuSize =' '
lcKey     =' '
IF SEEK('S'+lcAccount+&lcTmpOrdL..Style,'Spck_Lin')
  lcKey='S'+lcAccount+&lcTmpOrdL..Style
  lnSep=1
  Q=1
  W=STR(Q,1)

  X=1
  Z=STR(X,1) 
  SELECT Spck_Lin
  SCAN REST WHILE 'S'+lcAccount+&lcTmpOrdL..Style = lcKey

    IF &lcTmpOrdL..Qty&Z > 0
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
*! Date      : 03/22/2001
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKFC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetLoc

lcAlias=ALIAS()
lcStr=" "
SELECT WhsLoc
IF SEEK(&lcTmpOrdL..Style)
  SCAN REST WHILE Style+Color = &lcTmpOrdL..Style
    lcStr = lcStr +" "+cLocation
  ENDSCAN
  lcStr=ALLTRIM(lcStr)
  lnLen=LEN(lcStr)
ENDIF
SELECT (lcAlias)

*--End of lfGetLoc.
*!*************************************************************
*! Name      : lfContuLoc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/22/2001
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKFC.PRG
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
*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/22/2001
*! Purpose   : Refresh the filters of the option grid.
*!*************************************************************
*! Called from : ALPKTKFC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfClrRead

PARAMETERS lcDumdy
lcDumdy = .T.
CLEAR READ

RETURN lcDumdy

*--End of lfClrRead.