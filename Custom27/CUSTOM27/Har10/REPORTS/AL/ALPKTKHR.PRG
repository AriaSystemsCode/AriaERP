*:******************************************************************************
*: PROGRAM FILE : ALPKTKHR.PRG  - (C102125)
*: DESCRIPTION  : PRINT PICK TICKET - 66 LINE PAGE,8 1/2" x 11" (For HARISON)
*:              : A copy from ALO820Z
*: Module       : Aria Apparel Series.
*: DATE         : 10/01/2000
*: FROM         : ALO800 - SETUP PRINT JOB
*: DEVELOPER    : HEND GHANEM - (HBG)
*:******************************************************************************
*: Calls :
*:         FUNCTION  : lfContuLoc()
*:                   : lfGetLoc()
*:                   : lfScndGrd()
*:                   : lfPrnStrn()
*:         PROCEDURE : lpPrtSku
*:					 : lpNewFoter
*:                   : lpOldFoter
*:                   : lpHrdLabls
*:                   : lpSidLabls
*:                   : lpFilEmpLn
*:******************************************************************************
*:B804128,1 HBG 05/07/2001 Fix bug of Printing Notes1 & Notes2 in wrong place
*:B804191,1 HBG 05/07/2001 Fix bug of not Printing list of scales in this Piktkt.
*:******************************************************************************

*--variables from the option grid
llNoRec     = .F.  
XPIKNAME    = .T.
PRTPRICE    = llRpStyPrc                   && variable hold the printing of he style price
PRTNPAD     = llRpOrdNot                   && variable hold the printing of notpad
MSGL1       = lcRpMsg1                     && variable hold the first line of the message
MSGL2       = lcRpMsg2                     && variable hold the second line of the message
MSGL3       = lcRpMsg3                     && variable hold the third line of the message
lcPrnStLoc  = llRpStyLoc                   && for the style location
lcPrnSkPck  = llRpSkuBck                   && for the sku\pack
lcPrnSkSiz  = llRpSkuSiz                   && for the sku\by size 

lnLength    = 07
STORE SPACE(1) TO lcStrToPrn, lcSkuSize, lcSclStrn, lcStr, HLINE1
lnLen       =  0
*ahmed
IF llPrntComp
*ahmed end
  HLine2      = TRIM(laCompAdd[1])           && variable hold the addressess of the company
  HLine3      = TRIM(laCompAdd[2])
  HLine4      = TRIM(laCompAdd[3])
  DIMENSION laDivLName[1,2]
  laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
  laDivLName[1,2] = 'HLINE1'
  IF LEN(TRIM(HLine3)) = 0
    HLine3 = HLine4
    HLine4 = SPACE(1)
  ENDIF
*ahmed
ELSE
  STORE SPACE(0) TO HLINE1,HLINE2,HLINE3,HLINE4
ENDIF
*ahmed end
lcCustPoVl  = ''


*--initialize Row globaly and llScale 
ROW = 0
llScale   = .T.         && Flag to print the size scales at the first page only
llEndPT   = .F.
NEWDOC    = .T.
lnNotLine =  1
llRet     = .F.         && Flag to loop if Row>=47 when printing size scales

CLEAR TYPEAHEAD
SET DEVICE TO PRINT

*--If eof it will return to the main program
IF EOF(lcTmpOrdL)
  llNoRec = .T.
  *-- Message <No records to display.>
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*--open this files again with another index
SELECT SPCK_LIN
SET ORDER TO SPCKLINS INTO SPCK_LIN ADDITIVE

SELECT WhsLoc
SET ORDER TO WhsLocSt INTO WhsLoc  ADDITIVE


*-- MAIN LOOP
DO WHILE !EOF(lcTmpOrdL)
  SELECT (lcTmpOrdL)
  lnCurRec = RECNO()
  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
     SELECT (lcTmpOrdL)
     XPIKTKT  = PIKTKT
     XORDER   = ORDER
     XDATE    = PIKTKT.DATE
     XSTORE   = PIKTKT.Store 
     
     SELECT ORDHDR
     *B804128,1 HBG 05/07/2001 Get the correct order in ORDHDR [Begin]
     =SEEK('O'+XORDER,'ORDHDR')
     *B804128,1 [End]
     
     XMULTI   = MULTI
     XACCOUNT = ACCOUNT
     STORE 0.00 TO XORDTOT, XVALUE , SVALUE
     
     lcCustPoVl = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)
     
     SELECT CUSTOMER
     XBTNAME  = CUSTOMER->BTNAME
     XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
     XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
     XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2') 

     IF LEN(TRIM(XBTADDR2)) =0
        XBTADDR2 = XBTADDR3
        XBTADDR3 = ''
     ENDIF

     *-- GET DESCRIPTIONS FOR CODED FIELDS
     XTERM_DATA  = gfCodDes(ORDHDR->CTERMCODE,'CTERMCODE')
     
     XSHIP_DATA  = gfCodDes(IIF(ALLTRIM(ORDHDR.SHIPVIA) = "*" ,;
                           CUSTOMER.SHIPVIA,ORDHDR.SHIPVIA),'SHIPVIA')
     
     XSPCI_DATA  = gfCodDes(ORDHDR->SPCINST,'SPCINST')
     XSEAS_DATA  = gfCodDes(ORDHDR->SEASON,'SEASON')
     XDIVI_DATA  = gfCodDes(ORDHDR->CDIVISION,'CDIVISION')
     
     *--global function get the related fields
     *ahmed
     IF llPrntComp
     *ahmed end
       =gfRltFld(PADR(ORDHDR->CDIVISION,6),@laDivLName,'CDIVISION')
       HLINE1   = IIF(EMPTY(HLINE1) , lcCompName , HLINE1)     
     *ahmed
     ENDIF
     *ahmed end 
     STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
     XPIECES = 0
  
  ENDIF

  *- Initialize the alt address if ther is any.
  SELECT OrdHdr
  SEEK cordtype+XORDER
  
  IF Alt_ShpTo

    lcShpTName  = OrdHdr.STName   
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3
    laShipTo[4] = OrdHdr.cAddress4
    laShipTo[5] = OrdHdr.cAddress5

    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]

    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT Customer

    IF !EMPTY(CUSTOMER.Dist_Ctr)
      =SEEK("S"+CUSTOMER.Account+CUSTOMER.Dist_Ctr)
    ENDIF

    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
    XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
    XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )

    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF
  SELECT ORDHDR
  
  *-- START PRINTING
  DO lpHrdLabls
  IF llRet
    LOOP
  ENDIF 
  
  *-- LINE LOOP
  SELECT (lcTmpOrdL)
  NEWDOC  = .T.
  XTOTQTY = 0
  
  SCAN WHILE piktkt = xpiktkt 
    DO CASE
      CASE EOF()
        NEWDOC = .T.
        EXIT
      CASE TOTPIK <= 0
        LOOP
      CASE ROW >= 55
        NEWDOC = .F.
        EXIT
    ENDCASE
    
    *-- Modified to get the Style/Color location.     
    SELECT Style
    IF SEEK(&lcTmpOrdL..Style) 
      XSTYDESC    = DESC
      XCOLORDESC  = gfCodDes(SUBSTR(&lcTmpOrdL..Style,14,19),'COLOR')
      lcStyLocat  = Location
      lcScale     = Scale
    ELSE
      XSTYDESC   = ''
      lcStyLocat = ''
      lcScale    = ''
    ENDIF
    *-- To get the color description.
    SELECT (lcTmpOrdL)
    
    @ ROW,1 SAY SUBSTR(STYLE,1,12)
    @ ROW,14 SAY SUBSTR(STYLE,14,19)
    @ ROW,20 SAY LEFT(lcScale,2)
    
    @ ROW,22 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,26 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,31 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,35 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,40 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,44 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,49 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,53 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,59 SAY TOTPIK PICTURE '99999'
    
    

    
    IF PRTPRICE = .T.
      @ ROW,66 SAY PRICE PICTURE '9999.99'
    ENDIF
    @ ROW,74 SAY TOTPIK PICTURE '99999'
    
    DO lpSidLabls
    ROW = ROW + 1
    =lfChkRow("B")
    IF lcPrnSkPck=.F.
      @ ROW,1 SAY 'STYLE DESC.:'
      @ ROW,12 SAY ALLTRIM(XStyDesc)
      @ ROW,33 SAY 'CLR DESC.:'
      @ ROW,43 SAY ALLTRIM(XCOLORDESC)  
      IF lcPrnStLoc=.T.
        =lfGetLoc()
        IF !EMPTY(lcStr)
          @ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,10)
          DO lpSidLabls
          Row = Row + 1
          =lfChkRow("B")
          
          IF LEN(lcStr) > 18
            =lfContuLoc()
          ENDIF  
        ELSE
          DO lpSidLabls
          Row = Row + 1
          =lfChkRow("B")
        ENDIF
      ENDIF
    ELSE
      IF SEEK('P'+ORDLINE.Account+ORDLINE.Style,'SPCK_LIN')
        @ ROW,1 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
        @ ROW,23 SAY 'STYLE DESC.:'
        @ ROW,39 SAY ALLTRIM(XStyDesc)
        @ ROW,58 SAY 'CLR DESC.:'
        @ ROW,68 SAY ALLTRIM(XCOLORDESC)  
        
        DO lpSidLabls
        Row = Row + 1
       =lfChkRow("B")
      ELSE
        DO lpPrtSku      && Procedure to print the Style/Color Sku no.
        
        IF lcPrnSkSiz=.F.
          @ ROW,00 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
          @ ROW,IIF(LEN(lcStrToPrn)>=40,41,LEN(lcStrToPrn)+2) SAY 'STYLE DESC.:'
          @ ROW,IIF(LEN(lcStrToPrn)>=40,53,LEN(lcStrToPrn)+14) SAY ALLTRIM(XStyDesc)
          @ ROW,IIF(LEN(lcStrToPrn)>=40,74,LEN(lcStrToPrn)+35)  SAY 'CLR DESC.:'
          @ ROW,IIF(LEN(lcStrToPrn)>=40,84,LEN(lcStrToPrn)+45)  SAY ALLTRIM(XCOLORDESC)
          
          DO lpSidLabls
          Row = Row + 1
          =lfChkRow("B")
          IF lcPrnStLoc=.T.
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,00 SAY 'LOCATION:'+ SUBSTR(lcStr,1,85)
              DO lpSidLabls
              Row = Row + 1
              =lfChkRow("B")
              IF LEN(lcStr) > 85
                =lfContuLoc()
              ENDIF  
            ELSE
              DO lpSidLabls
              Row = Row + 1  
		      =lfChkRow("B")
            ENDIF
          ENDIF  
          DO lpSidLabls
          Row = Row + 1
	     =lfChkRow("B")
        ELSE
          @ ROW,00 SAY 'STYLE DESC.:'
          @ ROW,12 SAY ALLTRIM(XStyDesc)
          @ ROW,33 SAY 'CLR DESC.:'
          @ ROW,43 SAY ALLTRIM(XCOLORDESC)  
          IF lcPrnStLoc=.T.
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,10)
              DO lpSidLabls
              Row = Row + 1
		      =lfChkRow("B")
              IF LEN(lcStr) > 18
                =lfContuLoc()
              ENDIF  
            ELSE
              DO lpSidLabls
              Row = Row + 1 
		      =lfChkRow("B")
           ENDIF
          ELSE
            DO lpSidLabls
            Row = Row + 1
		    =lfChkRow("B")
          ENDIF  
          =lfPrnStrn() 
          lcSkuSize=lcStrToPrn+' '+lcSkuSize
          @ ROW,00 SAY SUBSTR(lcSkuSize,1,86)
          IF !EMPTY(lcSkuSize)
            DO lpSidLabls
            Row = Row + 1
		    =lfChkRow("B")
          ENDIF
        ENDIF
      ENDIF  
    ENDIF  

    *-- To print the color description, And the 
    *-- Style/Color location.
    DO lpSidLabls
    Row = Row + 1
    =lfChkRow("B")
    IF lcPrnSkSiz=.T.
    ENDIF  
    SELECT (lcTmpOrdL)
    XTOTQTY = XTOTQTY+ TOTQTY
    XVALUE  = XVALUE + TOTPIK * PRICE
    XPIECES = XPIECES + TOTPIK

    SELECT (lcTmpOrdL)
  
  ENDSCAN
  
  *-- END PRINT LINE LOOP
  
  IF NewDoc
  *-- To print the notepad.
    SELECT (lcTmpOrdL)
    IF PrtNPad = .T.
      SELECT NotePad
	  IF SEEK('B' + XORDER)
        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 75
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)

        @ Row,00 SAY '* -- N O T E S -- *' 
        DO lpSidLabls
        Row = Row + 1
	    =lfChkRow("B")

        DO WHILE lnNotLine <= lnMemLins
          IF Row >= 55
            DO lpOldFoter
            DO lpHrdLabls
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

  SELECT PIKTKT
  SEEK &lcTmpOrdL..PIKTKT
  
 
  llScale = .T.     && Re-initialize llScale to get the size scales for a new P/T
  SELECT (lcTmpOrdL)
 
ENDDO
SET DEVICE TO SCREEN
*----- the functions of the program --*
*!*************************************************************
*! Name               : lpFilEmpLn.
*! Developer          : HEND GHANEM
*! Date               : 27/11/2000
*! Purpose            : Fill the empty pick ticit lines.
*!*************************************************************
*! Calls              : lpSidLabls
*!*************************************************************
*! Called from        : ALPKTKRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : =lpFilEmpLn()
*!*************************************************************
PROCEDURE lpFilEmpLn

DO WHILE Row <= 53
  DO lpSidLabls
  IF ROW < 53
    Row = Row + 1
  ELSE
    EXIT
  ENDIF  
ENDDO

*!*************************************************************
*! Name               : lpSidLabls.
*! Developer          : HEND GHANEM
*! Date               : 27/11/2000
*! Purpose            : Print the Side labels.
*!*************************************************************
*! Calls              : ...
*!*************************************************************
*! Called from        : ALPKTKRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : =lpSidLabls()
*!*************************************************************
PROCEDURE lpSidLabls
 
DO CASE
  CASE ROW = 34 .OR. ROW = 50
    @ ROW,93 SAY IIF(XPIKNAME=.T.,PADR(HLINE1,30),'')
  CASE ROW = 35 .OR. ROW = 51
    @ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE2,'')
  CASE ROW = 36 .OR. ROW = 52
     @ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE3,'')
  CASE ROW = 37 .OR. ROW = 53
    *IF llEndPT .AND. Row = 53 .AND. NewDoc
     * @ 53,000 SAY MSGL1                    
    *ENDIF  
    @ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE4,'')
  CASE ROW = 23 .OR. ROW = 40
    @ ROW,93 SAY XSTNAME
  CASE ROW = 24 .OR. ROW = 41
    @ ROW,93 SAY XSTADDR1
  CASE ROW = 25 .OR. ROW = 42
    @ ROW,93 SAY XSTADDR2
  CASE ROW = 26 .OR. ROW = 43
    @ ROW,93 SAY XSTADDR3
  CASE ROW = 30 .OR. ROW = 46
    @ ROW,086 SAY LEFT(lcCustPoVl,13)
    @ ROW,100 SAY XSTORE
    @ ROW,111 SAY ORDHDR->DEPT
    @ ROW,120 SAY XORDER
  CASE ROW = 31 .OR. ROW = 48
    @ ROW,101 SAY XPIKTKT
    @ ROW,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
ENDCASE

*!*************************************************************
*! Name               : lpHrdLabls.
*! Developer          : HEND GHANEM
*! Date               : 27/11/2000
*! Purpose            : Print the Header labels.
*!*************************************************************
*! Calls              : ...
*!*************************************************************
*! Called from        : ALPKTKRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : =lpHrdLabls()
*!*************************************************************
PROCEDURE lpHrdLabls

*-- Line 0
@ 02,001 SAY IIF(XPIKNAME=.T.,PADR(HLINE1,30),'')      
@ 02,095 SAY IIF(XPIKNAME=.T.,PADR(HLINE1,30),'')      
*-- Line 1
@ 03,001 SAY IIF(XPIKNAME=.T.,HLINE2,'')
@ 03,095 SAY IIF(XPIKNAME=.T.,HLINE2,'')
*-- Line 2
@ 04,001 SAY IIF(XPIKNAME=.T.,HLINE3,'')
@ 04,095 SAY IIF(XPIKNAME=.T.,HLINE3,'')
*-- Line 3
@ 05,001 SAY IIF(XPIKNAME=.T.,HLINE4,'')
@ 05,61 SAY LEFT(XDIVI_DATA,7)
@ 05,74 SAY XPIKTKT
@ 05,095 SAY IIF(XPIKNAME=.T.,HLINE4,'')
*-- Line 4
@ 07,095 SAY XSTNAME
*-- Line 5
@ 08,61 SAY ORDHDR.APPROVAL
@ 08,74 SAY XDATE

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
@ 13,086 SAY LEFT(lcCustPoVl,13)
@ 13,100 SAY XSTORE                                 
@ 13,111 SAY ORDHDR->DEPT                           
@ 13,120 SAY XORDER
*-- Line 11
@ 15,101 SAY XPIKTKT
@ 15,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10) 
*-- Line 13
@ 17,093 SAY IIF(XPIKNAME=.T.,PADR(HLINE1,30),'')
*-- Line 15
@ 18,1 SAY XACCOUNT
@ 18,7 SAY XORDER
@ 18,13 SAY ORDHDR->REP1
@ 18,17 SAY ORDHDR->REP2
@ 18,21 SAY ORDHDR->ENTERED
@ 18,30 SAY ORDHDR->START
@ 18,38 SAY ORDHDR->COMPLETE
@ 18,46 SAY SUBSTR(XTERM_DATA,1,14)
@ 18,61 SAY XSTORE
@ 18,69 SAY lcCustPoVl  PICTURE '##########'
@ 18,80 SAY ORDHDR->DEPT 
@ 18,093 SAY IIF(XPIKNAME=.T.,HLINE2,'')
@ 19,093 SAY IIF(XPIKNAME=.T.,HLINE3,'')
*-- Line 17 PRINT 1 ST SIZE SCALE

ROW = 20
IF llScale
  
  ROW = 20
  DIMENSION laScaleSz[5]
  STORE SPACE (0) TO laScaleSz
  lnCount   = 1
  lcScalCnt = 0
  lcSelFile = SELECT(0)
  SELECT ORDLINE
  SEEK ORDHDR.cOrdType+XORDER
  lcKey = cordtype+order+STR(lineno,6)
  SCAN WHILE XORDER = ORDER  .AND. lcScalCnt <= 5
    IF ASCAN(laScaleSz,SUBSTR(ORDLINE.SCALE,1,2)) = 0 .AND. SEEK('S'+ORDLINE.SCALE,'SCALE')    
      lcScalCnt = lcScalCnt +1
      DIMENSION laScaleSz[lcScalCnt]
                                                    
      lcScalSiz = SUBSTR(ordLine.SCALE,1,2)        +PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,4),4, SPACE(1))+;
                    PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,4),4, SPACE(1))+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,4),4, SPACE(1))+;
                    PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,4),4, SPACE(1))+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,4),4, SPACE(1))+;
                    PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,4),4, SPACE(1))+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,4),4, SPACE(1))+;
                    PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,4),4, SPACE(1))
      laScaleSz[lcScalCnt] = lcScalSiz
    ENDIF
  ENDSCAN

  SEEK (lcKey)
    FOR lnCount = 1 TO ALEN(laScaleSz,1)
      @ ROW,20 SAY laScaleSz[lnCount]
      IF lnCount = 1
        @ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE4,'')
      ENDIF
      *B804191,1 HBG 05/07/2001 Fix bug of not Printing list of scales in this Piktkt [Begin]
      ROW = ROW + 1 
      *B804191,1 HBG 05/07/2001 [End]
    ENDFOR
    DO lpSidLabls
    ROW = ROW + 1 
    llScale =.F.
  SELECT (lcSelFile)
ELSE
  @ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE4,'')
ENDIF 
ROW = IIF(ROW < 23,22,ROW)


*!*************************************************************
*! Name               : lpOldFoter.
*! Developer          : HEND GHANEM
*! Date               : 27/11/2000
*! Purpose            : Print the form footer at in the middle 
*!                    : of a spacific pick ticket. 
*!*************************************************************
*! Calls              : ...
*!*************************************************************
*! Called from        : ALPKTKRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : =lpOldFoter()
*!*************************************************************
PROCEDURE lpOldFoter

@ 55,93 SAY XSTNAME
@ 56,93 SAY XSTADDR1
@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 57,93 SAY XSTADDR2
@ 58,93 SAY XSTADDR3
@ 63,086 SAY LEFT(lcCustPoVl,13)
@ 63,100 SAY XSTORE                  
@ 63,111 SAY ORDHDR->DEPT            
@ 63,120 SAY XORDER
@ 64,101 SAY XPIKTKT
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
ROW = ROW + 1

*!*************************************************************
*! Name               : lpNewFoter.
*! Developer          : HEND GHANEM
*! Date               : 27/11/2000
*! Purpose            : Print the form footer at the end of a 
*!                    : spacific pick ticket. 
*!*************************************************************
*! Calls              : lfChkRow 
*!*************************************************************
*! Called from        : ALPKTKRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : =lpNewFoter()
*!*************************************************************
PROCEDURE lpNewFoter

=lfChkRow("A")
@ 54,000 SAY MSGL2
@ 55,000 SAY MSGL3
@ 56,093 SAY XSTNAME
@ 57,093 SAY XSTADDR1

lnCol = 11

*B804128,1 HBG 05/07/2001 Get the correct order in ORDHDR to Fix bug of 
*B804128,1                Printing Notes1 & Notes2 with wrong Piktkt[Begin]
=SEEK('O'+XORDER,'ORDHDR')
*B804128,1 [End]

IF SUBSTR(OrdHdr->Note1, 1, 1) <> '*'
  @ 58,lnCol SAY OrdHdr->Note1
  lnCol = 42
ENDIF

*B804128,1 HBG 05/07/2001 Fix Bug of Printing Notes Only 25 Char While it Should be 30 Char [Begin] 
*@ 58,lnCol SAY IIF(SUBSTR(OrdHdr->Note2, 1, 1)<>'*', SUBSTR(OrdHdr->Note2,1,25), '')
@ 58,lnCol SAY IIF(SUBSTR(OrdHdr->Note2, 1, 1)<>'*', SUBSTR(OrdHdr->Note2,1,30), '')
*B804128,1 [End]

@ 58,076 SAY XPIECES PICTURE '999999' 
@ 58,093 SAY XSTADDR2
@ 59,093 SAY XSTADDR3
@ 62,060 SAY SUBSTR(XSHIP_DATA,1,11)
@ 62,076 SAY IIF(PRTPRICE = .T.,XVALUE,'') PICTURE '999999.99'
@ 63,086 SAY LEFT(lcCustPoVl,13)
@ 63,100 SAY XSTORE                  
@ 63,111 SAY ORDHDR->DEPT            
@ 63,120 SAY XORDER
@ 64,101 SAY XPIKTKT
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
XVALUE = 0.00
ROW = ROW + 1

*!*************************************************************
*! Name               : lpPrtSku.
*! Developer          : HEND GHANEM
*! Date               : 27/11/2000
*! Purpose            : Print the ,main style/color Skus for a 
*!                    : specific account.
*!*************************************************************
*! Calls              : ... 
*!*************************************************************
*! Called from        : ALPKTKRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : =lpPrtSku()
*!*************************************************************
PROCEDURE lpPrtSku

IF !SEEK('S'+ORDLINE.Account+ORDLINE.Style,'Spck_Lin')
  lcStrToPrn = " "
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  = SEEK('S'+Style.Scale,'Scale')
  = SEEK('M'+ORDLINE.Account,'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 
  = SEEK('S'+ORDLINE.Account+ORDLINE.Style,'Spck_Lin')
  lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnDime1)
  lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
  lnLength   = LEN(lcStrToPrn)+2
  lnLocCol = LEN(lcStrToPrn) + 5

ELSE
  @ ROW,01 SAY SUBSTR(Sku,1,8)
  lnLocCol = 35
  @ ROW,17 SAY 'CUSTOMER SKU #'
ENDIF
*ahmed 
*SET DEVICE TO SCREEN
RETURN
*!*************************************************************
*! Name               : lfGetLoc.
*! Developer          : HEND GHANEM
*! Date               : 27/11/2000
*! Purpose            : Print the location.
*!*************************************************************
*! Calls              : ... 
*!*************************************************************
*! Called from        : ALPKTKRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : =lfGetLoc()
*!*************************************************************
FUNCTION lfGetLoc
lcAlias=ALIAS()
lcStr=" "        && Initialize the variable to prevent carrying over of locations.
SELECT WhsLoc
IF SEEK(&lcTmpOrdL..Style)
  SCAN REST WHILE Style = &lcTmpOrdL..Style
    lcStr = lcStr +" "+cLocation
  ENDSCAN
  lcStr=ALLTRIM(lcStr)
  lnLen=LEN(lcStr)
ENDIF
SELECT (lcAlias)
*!*************************************************************
*! Name               : lfContuLoc.
*! Developer          : HEND GHANEM
*! Date               : 27/11/2000
*! Purpose            : Print the location.
*!*************************************************************
*! Calls              : ... 
*!*************************************************************
*! Called from        : ALPKTKRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : =lfContuLoc()
*!*************************************************************
FUNCTION lfContuLoc
FOR I = 12 TO lnLen  
   @ ROW ,00 SAY SUBSTR(lcStr,I,86) 
   I=I+86
   DO lpSidLabls
   Row = Row + 1
  =lfChkRow("B")
ENDFOR
*!*************************************************************
*! Name               : lfChkRow.
*! Developer          : HEND GHANEM
*! Date               : 27/11/2000
*! Purpose            : To check the row position.
*!*************************************************************
*! Calls              : ... 
*!*************************************************************
*! Called from        : ALPKTKRG
*!*************************************************************
*! Passed Parameters  : lcType : To check if the row is 54 or 55
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : =lfChkRow()
*!*************************************************************
FUNCTION lfChkRow
PARAMETER lcType
IF Row  >= IIF(lcType = "A",54,55)
  DO lpOldFoter
  DO lpHrdLabls
ENDIF
*!*************************************************************
*! Name               : lfPrnStrn.
*! Developer          : HEND GHANEM
*! Date               : 27/11/2000
*! Purpose            : To print the acount/style/color skus.
*!*************************************************************
*! Calls              : ... 
*!*************************************************************
*! Called from        : ALPKTKRG
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : =lfPrnStrn()
*!*************************************************************
FUNCTION lfPrnStrn
PRIVATE lnAlias
lnAlias = SELECT(0)

lcSkuSize = ''

*-- To check the existence of Account/Style/Color SKUs 
  IF SEEK('S'+ORDLINE.Account+ORDLINE.Style,'Spck_Lin')
    lcKey = 'S'+ORDLINE.Account+ORDLINE.Style

  = SEEK('S'+Style.Scale,'Scale')

  *-- Declaring this array to hold the Skus in order to sort them
  DIMENSION laSkus[Scale.Cnt,2]
  laSkus = ''

  *-- Saving the customer pointer
  lnCustRec = RECNO('Customer')

  *-- Searching for the main account to get check the customer skutmpl
  = SEEK('M'+ordline.Account,'Customer')
  
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF BETWEEN(lnCustRec,1,RECCOUNT('Customer'))
    GOTO lnCustRec IN Customer
  ENDIF
  
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8
    lnDime2 = 8
  ENDIF 

  SELECT Spck_Lin
  SCAN REST WHILE 'S'+Account+Style = lcKey
  
    FOR lnCounter = 1 TO Scale.Cnt
      lcCounter = STR(lnCounter,1)
      IF Qty&lcCounter > 0
        laSkus[lnCounter,1] = STR(lnCounter,1)
        laSkus[lnCounter,2] = SUBSTR(Pack_Id,lnDime1+1,5)+' '
        EXIT
      ENDIF
    ENDFOR
  ENDSCAN
  
  =ASORT(laSkus,1)  
  
  FOR lnArrayCnt = 1 TO ALEN(laSkus,1)
    lcArrayCnt = ALLTRIM(STR(lnArrayCnt))
    IF &lcTmpOrdL..Qty&lcArrayCnt > 0
      lcSkuSize = lcSkuSize+'S'+lcArrayCnt+':'+ ALLTRIM(laSkus[lnArrayCnt,2])+ ' '
    ENDIF
  ENDFOR
  
  lcSkuSize=ALLTRIM(lcSkuSize)
ENDIF
SELECT (lnAlias)
RETURN
               
*!*************************************************************
*! Name      : lfClrRead
*! Developer : HEND GHANEM
*! Date      : 27/11/2000
*! Purpose   : Refresh the option grid
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfClrRead ()
*!*************************************************************
FUNCTION lfClrRead
PARAMETER lcParam

CLEAR READ
lcParam = .T.
RETURN lcParam
*-- End of lfClrRead.
