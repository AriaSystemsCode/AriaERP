*:**************************************************************************
*: Program file  : ALPKTKCH.PRG (C# 101545)
*: Program desc. : PRINT PICKING TICKETS SIDE SHIPPING LABELS
*:               : NO BULK RECAP AT THE END.
*:               : Convert ALO820Z.PRG from 2.6 to 2.7
*:           CUST: Christina sportswear LTD.
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY _ (SSH)
*!**************************************************************************
*: Calls :PROCEDURES : lpFilEmpLn,lpSidLabls,lpHrdLabls,lpOldFoter
*:                     lpNewFoter
*:        FUNCTIONS  : lfClrTmp
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*:Modifications:-
*:B#802435,1 SSH 20/07/99   Alignment.
*:B#802511,1 SSH 11/08/99   Fix Bug of print incorrect address.
*:B802915,1 ADEL 01/02/2000 1- Fix the bug of swicthing The Bill To Address with the Ship To Address.
*:B802915,1 ADEL            2- Move the PIKTKT# one row up. 
*:B802915,1 ADEL            3- Let the SKU br printed under conditions in OG.
*:B802915,1 ADEL            4- Move detalis one line up.
*:B603511,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.
*:**************************************************************************
*-- Get the answers for the color and location printing,
*-- either from the memory file - if found - or from the user.
IF FILE('&gcDataDir.ALPKTKCH.MEM')
  RESTORE FROM &gcDataDir.ALPKTKCH.MEM ADDITIVE
ELSE
  STORE 0 TO lnMClr
ENDIF
llPrntClr = (lnMClr = 1)
DIME laShipTo[5],laSoldTo[5]
IF EMPTY(lnMClr)
  lcDevice = SET('DEVICE')
  SET DEVICE TO SCREEN
  IF EMPTY(lnMClr)
    *--- 'Do you wish to print the color description ? [Y/N]','YN'
    lnMClr = gfModalGen('QRM44077B00006','DIALOG' )
    llPrntClr = (lnMClr = 1) 
  ENDIF  
  SET DEVICE TO &lcDevice
ENDIF
SAVE ALL LIKE lnM* TO &gcDataDir.ALPKTKCH.MEM
RELEASE ALL LIKE lnM*
lnMajPic = LEN(gfItemMask("PM"))
=gfOpenFile(gcDataDir+'SkuTmpl' ,'SkuTmpl','SH')
*-- Set the memory variables.
HLine1 = lcCompName
HLine2 = laCompAdd[1]
HLine3 = laCompAdd[2]
HLine4 = SUBSTR(ALLTRIM(laCompAdd[3]) + ' ' + ALLTRIM(laCompAdd[4]) + ' '+ ALLTRIM(laCompAdd[5]),1,30)
IF LEN(TRIM(HLine3)) = 0
  HLine3 = HLine4
  HLine4 = SPACE(1)
ENDIF
SELECT SPCK_LIN
SET ORDER TO SPCKLINS
SELECT SPCK_HDR
SET ORDER TO SPCK_HDR
lnLength   = 07
lcStrToPrn = ' '
lcSkuSize  = ' '
lcSclStrn  = ' '
lnLen      = 0
lcStr      = ' '
SELECT ORDLINE
SET ORDER TO ORDLINST
SELECT (lcTmpOrdL)
lcLnTemp  = gfTempName()
lcPikTemp = gfTempName()
SELECT(lcTmpOrdL)
SELECT * FROM PIKTKT ;
       WHERE SEEK(PIKTKT.PIKTKT,lcTmpOrdL) INTO DBF (gcWorkDir+lcPikTemp)
llNoRec = .T.
IF _TALLY = 0
  =gfModalGen('TRM42153B00000','DIALOG' )
  =lfClrTmp()
  SET DEVICE TO SCREEN
  RETURN
ELSE
  INDEX ON PIKTKT TAG &lcPikTemp
ENDIF

SELECT &lcPikTemp
*-- MAIN LOOP
llEndPT   = .F.
NEWDOC    = .T.
lnNotLine = 1
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
DO WHILE INKEY() <> 32
  SELECT &lcPikTemp
  IF EOF()
     EXIT
  ENDIF
  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
     SELECT &lcPikTemp
     XPIKTKT  = PIKTKT
     XORDER   = ORDER
     XDATE    = DATE
     XSTORE   = STORE
     SELECT ORDHDR
     =SEEK('O'+XORDER)
     XMULTI   = MULTI
     XACCOUNT = ACCOUNT
     STORE 0.00 TO XORDTOT, XVALUE , SVALUE
     SELECT ORDLINE
     =SEEK ('O'+xOrder+xStore)
     IF ORDHDR.MultiPO 
       lcCustPO = SUBSTR(CustPO,1,10)
     ENDIF 
     IF PikTkt <> xPikTkt
        LOCATE REST FOR   PikTkt = XPikTkt ;
                    WHILE cordtype+Order+Store = 'O'+ xOrder+xStore
     ENDIF
     IF cordtype+Order+Store <> 'O'+xOrder+xStore
        SELECT &lcPikTemp
        SKIP
        LOOP
     ENDIF
     *-- To copy the records which have the selected
     *-- PIKTKT from ORLINE file into a temp file (lcLnTemp) instead of
     *-- LOCATING in the ORDLINE file for each selected piktkt
     SET DEVICE TO SCREEN
     WAIT WINDOW  'PRINT PICK TICKETS - <Space Bar> TO ABORT ' NOWAIT 
     SET DEVICE TO PRINT     
     IF USED(lcLnTemp)
       SELECT &lcLnTemp
       USE
     ENDIF
     llNoRec = .F.
     SELECT OrdLine
     COPY REST TO &gcWorkDir.&lcLnTemp FOR PikTkt = xPikTkt ;
                               WHILE cordtype+Order+Store = 'O'+xOrder+xStore
     SET DEVICE TO SCREEN
     SELECT 0
     =gfOpenFile('&gcWorkDir.&lcLnTemp',' ','EX')
     INDEX ON ORDER+STR(LINENO,6) TAG &lcLnTemp
     SET DEVICE TO PRINT
     GO TOP
     SELECT CUSTOMER
     *:B#802511,1 SSH 11/08/99 Fix Bug of print incorrect address.
     = SEEK(IIF(EMPTY(XSTORE),'M'+xACCOUNT,'S'+xAccount+xStore))
     *:B#802511,1 SSH (END)
     *B802915,1 (Begin) Remark the following lines and get the Bill To address instead of ST A ddress.
     *laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
     *laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
     *laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
     *laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
     *laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
     laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1, '2')
     laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2, '2')
     laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3, '2')
     laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4, '2')
     laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5, '2')
     *B802915,1 (End)
     *--Get the Bill To adddess except the.
     XBTNAME  = CUSTOMER.BTNAME
     XBTADDR1 = laShipTo[1]
     XBTADDR2 = laShipTo[2]
     XBTADDR3 = SUBSTR(ALLTRIM(laShipTo[3])+" "+ALLTRIM(laShipTo[4])+" "+ALLTRIM(laShipTo[5]),1,30)
     IF LEN(TRIM(XBTADDR2)) = 0
        XBTADDR2 = XBTADDR3
        XBTADDR3 = ''
     ENDIF
     *-- GET DESCRIPTIONS FOR CODED FIELDS
     XTERM_DATA = SUBSTR(gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE '),1,10)
     XSHIP_DATA = SUBSTR(gfCodDes(ORDHDR.SHIPVIA,'SHIPVIA   '),1,10)
     XSPCI_DATA = gfCodDes(ORDHDR.SPCINST,'SPCINST   ')
     XSEAS_DATA = SUBSTR(gfCodDes(ORDHDR.SEASON,'SEASON    '),1,7)
     XDIVI_DATA = SUBSTR(gfCodDes(ORDHDR.CDIVISION ,'CDIVISION '),1,10)
     STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
     XPIECES = 0
     DECLARE laDivLName[1,2]
     lcDivLName = ' '
     laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
     laDivLName[1,2] = 'lcDivLName'
     = gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
     HLINE1 = IIF(EMPTY(lcDivLName),HLINE1,lcDivLName)
     *-- GET THE SIZE SCALES
     *-- Moved and enhanced to get the printed scales
     *-- once outside the loop not every time in the loop because it will 
     *-- be the same
     XSCALE2   = SPACE(1)
     SELE &lcLnTemp
     XSCALE1 = SCALE
     LOCATE REST FOR SCALE <> XSCALE1
     IF !EOF()
       XSCALE2 = SCALE
     ENDIF
     GO TOP
  ENDIF
  *-- END PICK TKT/ORDER SELECTION.
  
  *-- SHIP-TO ADDRESS FOR THIS STORE
  *- Initialize the alt address if ther is any.
  SELECT OrdHdr
  =SEEK('O'+xOrder)
  IF Alt_ShpTo
    XSTNAME  = OrdHdr.STNAME
    XSTADDR1 = OrdHdr.cAddress1
    XSTADDR2 = OrdHdr.cAddress2
    XSTADDR3 = SUBSTR(ALLTRIM(OrdHdr.cAddress3)+','+ALLTRIM(OrdHdr.cAddress4)+','+ALLTRIM(OrdHdr.cAddress5),1,30)
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT Customer
    *:B#802511,1 SSH 11/08/99 Fix Bug of print incorrect address.
    = SEEK(IIF(EMPTY(XSTORE),'M'+xACCOUNT,'S'+xAccount+xStore))
     *:B#802511,1 SSH(END)    
    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    *B802915,1 (Begin) Remark the following lines and get the Bill To address instead of ST A ddress.
    *laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
    *laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
    *laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
    *laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
    *laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
    laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
    laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
    laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
    laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )
    laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 )
    *B802915,1 (End)
    XSTADDR1 = laSoldTo[1]
    XSTADDR2 = laSoldTo[2]
    XSTADDR3 = SUBSTR(ALLTRIM(laSoldTo[3]) +' '+ALLTRIM(laSoldTo[4]) +' '+ALLTRIM(laSoldTo[5]),1,30)
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF
  SELECT ORDHDR
 
  *-- START PRINTING
  DO lpHrdLabls

  *-- LINE LOOP
  SELECT &lcLnTemp                          
  NEWDOC  = .T.
  XTOTQTY = 0
  *B802915,1 (Begin) Move a line up.
  *ROW     = 23
  ROW     = 22
  *B802915,1 (End)
  DO WHILE .T.                            
    DO CASE
      CASE EOF()
        NEWDOC = .T.
        EXIT
      CASE TOTPIK <= 0
        SKIP                              
        LOOP
      CASE ROW >= 47
        NEWDOC = .F.
        EXIT
    ENDCASE
    *-- Modified to get the Style/Color location.     
    SELECT Style
    IF SEEK(&lcLnTemp..Style)
      XSTYDESC   = DESC
      lcStyLocat = Location
      lcScale    = Scale
    ELSE
      XSTYDESC   = ''
      lcStyLocat = ''
      lcScale    = ''
    ENDIF
    *-- To get the color description.
    lcClrCod = ALLTRIM(SUBSTR(Style,lnMajPic+2))
    lcClrDesc = gfCodDes(lcClrCod,'COLOR     ')
    SELECT &lcLnTemp
    SELECT &lcLnTemp
    @ ROW,00 SAY STYLE
    *:B#802435,1 SSH 20/07/99 Alignment.
    *@ ROW,38 SAY lcScale
    @ ROW,37 SAY SUBSTR(lcScale,1,1)
   *@ ROW,39 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,38 SAY PIK1   PICTURE '@Z 9999'
   *@ ROW,43 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,42 SAY PIK2   PICTURE '@Z 9999'
   *@ ROW,47 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,46 SAY PIK3   PICTURE '@Z 9999'
   *@ ROW,51 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,50 SAY PIK4   PICTURE '@Z 9999'
   *@ ROW,55 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,54 SAY PIK5   PICTURE '@Z 9999'
   *@ ROW,59 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,58 SAY PIK6   PICTURE '@Z 9999'
   *@ ROW,62 SAY TOTPIK PICTURE '99999'
    @ ROW,61 SAY TOTPIK PICTURE '99999'
    IF llRpStyPrc
      @ ROW,67 SAY PRICE PICTURE '9999.99'
    ENDIF
    *@ ROW,73 SAY TOTPIK PICTURE '9999'  && Moved on chr. to the right. 
    @ ROW,75 SAY TOTPIK PICTURE '9999'  && Moved on chr. to the right. 
    *:B#802435,1 SSH(END)
    DO lpSidLabls
    ROW = ROW + 1
    IF !llRpSkuBck
      @ ROW,00 SAY 'STYLE DESC.: '+XStyDesc
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
      IF SEEK('P'+&lcLnTemp..Account+&lcLnTemp..Style,'SPCK_LIN')
        @ ROW,00 SAY 'PACK ID#: '+Spck_Lin.Pack_Id
        IF SEEK('P'+Spck_lin.Account+Spck_Lin.Pack_id,'SPCK_HDR')
          @ ROW,25 SAY 'PACK DESC.: '+Spck_Hdr.Desc
        ENDIF
        DO lpSidLabls
        Row = Row + 1
        @ ROW,00 SAY 'STYLE DESC.: '+XStyDesc
        @ ROW,33 SAY IIF(llPrntClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,10), '')
        DO lpSidLabls
        Row = Row + 1
      ELSE
        DO lpPrtSku      && Procedure to print the Style/Color Sku no.
        IF llRpSkuBck AND !llRpSkuSiz
          @ ROW,00 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
          @ ROW,IIF(LEN(lcStrToPrn)>=40,41,LEN(lcStrToPrn)+2) SAY 'STYLE DESC.:'+XStyDesc
          @ ROW,IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34) SAY IIF(llPrntClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,03), '')
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
          DO lpSidLabls
          Row = Row + 1  
        ENDIF
      ENDIF  
    ENDIF  
    DO lpSidLabls
    Row = Row + 1
    SELECT &lcLnTemp
    * SUM ORDER TOTALS
    XTOTQTY = XTOTQTY+ TOTQTY
    XVALUE  = XVALUE + TOTPIK * PRICE
    XPIECES = XPIECES + TOTPIK
    SELECT &lcLnTemp              
    SKIP                        
  ENDDO                         
  *-- END PRINT LINE LOOP
  IF NewDoc
    *-- To print the notepad.
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
            *B802915,1 (Begin) Move a line up.
            *Row = 23
            Row = 22
            *B802915,1 (End)
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
  *-- if it was printed once.
  *B603511,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.[START]
  *SELECT PIKTKT
  *=SEEK(&lcPikTemp..PIKTKT)
  *REPLACE PRTFLAG WITH 'P'
  *B603511,1 BWA 03/08/2000 [END]
  
  SELECT &lcPikTemp
  SKIP
ENDDO
=lfClrTmp()
IF llNoRec
  =gfModalGen('TRM42153B00000','DIALOG')
ENDIF
SET DEVICE TO SCRE

*!*************************************************************
*! Name      : lpFilEmpLn.
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 06/16/1999
*! Purpose   : Fill the empty pick ticit lines.
*!*************************************************************
*! Called from : ALPKTKCH.PRG
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
PROCEDURE lpFilEmpLn

DO WHILE Row <= 53
  DO lpSidLabls 
  Row = Row + 1
ENDDO

*!*************************************************************
*! Name      : lpSidLabls.
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 06/16/1999
*! Purpose   : Print the Side labels.
*!*************************************************************
*! Called from : ALPKTKCH.PRG
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
PROCEDURE lpSidLabls
DO CASE
  *B802915,1 (Begin) ADD -1 TO ALL CASES TO MOVE THE ROW UP WITHOUT REMARKING THEM.
  CASE ROW = 34-1
    @ ROW,93 SAY IIF(llPrntComp,SUBSTR(HLINE1,1,20),'')                 
    @ ROW,116 SAY SUBSTR(XSHIP_DATA,1,10)
  CASE ROW = 50-1  
    @ ROW,93 SAY IIF(llPrntComp,HLINE1,'')                 
  CASE ROW = 35-1
    @ ROW,93 SAY IIF(llPrntComp,HLINE2,'')
  CASE ROW = 51-1
    @ ROW,93 SAY IIF(llPrntComp,SUBSTR(HLINE2,1,20),'')
    @ ROW,116 SAY SUBSTR(XSHIP_DATA,1,10)
  CASE ROW = 36-1 .OR. ROW = 52-1     
     @ ROW,93 SAY IIF(llPrntComp,HLINE3,'')
  CASE ROW = 37-1 .OR. ROW = 53-1
    IF llEndPT .AND. Row = 53 .AND. NewDoc
      @ 53,000 SAY lcRpMsg1                    
    ENDIF  
    @ ROW,93 SAY IIF(llPrntComp,HLINE4,'')
  CASE ROW = 23-1 .OR. ROW = 40-1
    @ ROW,93 SAY XSTNAME
  CASE ROW = 24-1 .OR. ROW = 41-1
    @ ROW,93 SAY XSTADDR1
  CASE ROW = 25-1 .OR. ROW = 42-1
    @ ROW,93 SAY XSTADDR2
  CASE ROW = 26 .OR. ROW = 43
    @ ROW,93 SAY XSTADDR3
  CASE ROW = 30-1 .OR. ROW = 46-1 
    @ ROW,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,SUBSTR(OrdHdr.CUSTPO,1,10)) 
    @ ROW,100 SAY XSTORE
    @ ROW,109 SAY ORDHDR.DEPT
    @ ROW,115 SAY XORDER
  CASE ROW = 31-1 .OR. ROW = 48-1
    @ ROW,101 SAY XPIKTKT
  *B802915,1 (End)  
ENDCASE

*!*************************************************************
*! Name      : lpHrdLabls.
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 06/16/1999
*! Purpose   : Print the Header labels.
*!*************************************************************
*! Called from : ALPKTKCH.PRG
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
PROCEDURE lpHrdLabls

*-- Line 0
*:B#802435,1 SSH 20/07/99 Alignment [Decrease Row by 1].
*@ 01,116 SAY SUBSTR(XSHIP_DATA,1,10) 
 @ 00,116 SAY SUBSTR(XSHIP_DATA,1,10) 

*@ 02,001 SAY IIF(llPrntComp,HLINE1,'')
*@ 02,095 SAY IIF(llPrntComp,HLINE1,'')
@ 01,001 SAY IIF(llPrntComp,HLINE1,'')
@ 01,095 SAY IIF(llPrntComp,HLINE1,'')
  
*-- Line 1
*@ 03,001 SAY IIF(llPrntComp,HLINE2,'')
*@ 03,095 SAY IIF(llPrntComp,HLINE2,'')
@ 02,001 SAY IIF(llPrntComp,HLINE2,'')
@ 02,095 SAY IIF(llPrntComp,HLINE2,'')
  
*-- Line 2
*@ 04,001 SAY IIF(llPrntComp,HLINE3,'')
*@ 04,095 SAY IIF(llPrntComp,HLINE3,'')
@ 03,001 SAY IIF(llPrntComp,HLINE3,'')
@ 03,095 SAY IIF(llPrntComp,HLINE3,'')
  
*-- Line 3
*@ 05,001 SAY IIF(llPrntComp,HLINE4,'')
*@ 05,060 SAY XDIVI_DATA
@ 04,001 SAY IIF(llPrntComp,HLINE4,'')
@ 04,060 SAY XDIVI_DATA

*@ 06,076 SAY XPIKTKT
*@ 06,095 SAY IIF(llPrntComp,HLINE4,'')
*B802915,1 (Begin) Move the PIKTKT# one row up.
*@ 05,076 SAY XPIKTKT
@ 04,076 SAY XPIKTKT
*B802915,1 (End)
@ 05,095 SAY IIF(llPrntComp,HLINE4,'')
  
*-- Line 4
*@ 07,060 SAY ORDHDR.APPROVAL
*@ 07,076 SAY XDATE
*@ 07,095 SAY XSTNAME
@ 06,060 SAY ORDHDR.APPROVAL
@ 06,076 SAY XDATE
@ 06,095 SAY XSTNAME

*-- Line 5
*@ 08,095 SAY XSTADDR1
@ 07,095 SAY XSTADDR1
*-- Line 6
*@ 09,095 SAY XSTADDR2
@ 08,095 SAY XSTADDR2
*-- Line 7
*@ 10,005 SAY XBTNAME
*@ 10,048 SAY XSTNAME
*@ 10,095 SAY XSTADDR3
@ 9,005 SAY XBTNAME
@ 9,048 SAY XSTNAME
@ 9,095 SAY XSTADDR3
  
*-- Line 8
*@ 11,005 SAY XBTADDR1
*@ 11,048 SAY XSTADDR1
@ 10,005 SAY XBTADDR1
@ 10,048 SAY XSTADDR1

*-- Line 9
*@ 12,005 SAY XBTADDR2
*@ 12,048 SAY XSTADDR2
@ 11,005 SAY XBTADDR2
@ 11,048 SAY XSTADDR2
  
*-- Line 10
*@ 13,005 SAY XBTADDR3
*@ 13,048 SAY XSTADDR3
*@ 13,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,SUBSTR(OrdHdr.CUSTPO,1,10))
*@ 13,100 SAY XSTORE
*@ 13,109 SAY ORDHDR.DEPT
*@ 13,119 SAY XORDER
@ 12,005 SAY XBTADDR3
@ 12,048 SAY XSTADDR3
@ 12,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,SUBSTR(OrdHdr.CUSTPO,1,10))
@ 12,100 SAY XSTORE
@ 12,109 SAY ORDHDR.DEPT
@ 12,119 SAY XORDER

*-- Line 11
*@ 15,101 SAY XPIKTKT
@ 14,101 SAY XPIKTKT
*@ 17,065 SAY SUBSTR(XSTORE,1,4)         PICTURE '####'
*@ 17,069 SAY IIF(ORDHDR.MultiPO,SUBSTR(lcCustPO,1,7),SUBSTR(OrdHdr.CUSTPO,1,7))  PICTURE '########'
*@ 17,093 SAY IIF(llPrntComp,HLINE1,'')
@ 16,065 SAY SUBSTR(XSTORE,1,4)         PICTURE '####'
@ 16,069 SAY IIF(ORDHDR.MultiPO,SUBSTR(lcCustPO,1,7),SUBSTR(OrdHdr.CUSTPO,1,7))  PICTURE '########'
@ 16,093 SAY IIF(llPrntComp,HLINE1,'')

*-- Line 15
*@ 18,001 SAY XACCOUNT
*@ 18,012 SAY XORDER
*@ 18,023 SAY ORDHDR.REP1
*@ 18,027 SAY ORDHDR.REP2
*@ 18,031 SAY SUBSTR(DTOC(ORDHDR.ENTERED),1,5)
*@ 18,039 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
*@ 18,045 SAY ORDHDR.COMPLETE
*@ 18,054 SAY SUBSTR(XTERM_DATA,1,11)
*@ 18,065 SAY SUBSTR(XSTORE,5,8)         PICTURE '####'
*@ 18,070 SAY IIF(ORDHDR.MultiPO,SUBSTR(lcCustPO,8,10),SUBSTR(OrdHdr.CUSTPO,8,10))  PICTURE '########'
*@ 18,078 SAY ORDHDR.DEPT
*@ 18,093 SAY IIF(llPrntComp,SUBSTR(HLINE2,1,20),'')            
*@ 18,115 SAY SUBSTR(XSHIP_DATA,1,10) 
@ 17,001 SAY XACCOUNT
@ 17,012 SAY XORDER
@ 17,023 SAY ORDHDR.REP1
@ 17,027 SAY ORDHDR.REP2
@ 17,031 SAY SUBSTR(DTOC(ORDHDR.ENTERED),1,5)
@ 17,039 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
@ 17,045 SAY ORDHDR.COMPLETE
@ 17,054 SAY SUBSTR(XTERM_DATA,1,11)
@ 17,065 SAY SUBSTR(XSTORE,5,8)         PICTURE '####'
@ 17,070 SAY IIF(ORDHDR.MultiPO,SUBSTR(lcCustPO,8,10),SUBSTR(OrdHdr.CUSTPO,8,10))  PICTURE '########'
@ 17,078 SAY ORDHDR.DEPT
@ 17,093 SAY IIF(llPrntComp,SUBSTR(HLINE2,1,20),'')            
@ 17,115 SAY SUBSTR(XSHIP_DATA,1,10) 

*@ 19,093 SAY IIF(llPrntComp,HLINE3,'')            
@ 18,093 SAY IIF(llPrntComp,HLINE3,'')            
*-- Line 17 PRINT 1 ST SIZE SCALE
*@ 20,38 SAY XSCALE1
@ 19,38 SAY XSCALE1
XSCALE = GETSCALE( XSCALE1,SPACE(1))
*@ 20,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
*@ 20,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
*@ 20,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
*@ 20,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
*@ 20,56 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
*@ 20,60 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
@ 19,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
@ 19,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
@ 19,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
@ 19,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
@ 19,56 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
@ 19,60 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')

*-- Line 16
*@ 20,93 SAY IIF(llPrntComp,HLINE4,'')
@ 19,93 SAY IIF(llPrntComp,HLINE4,'')
*-- Line 18 PRINT 2ND. SIZE SCALE
IF !EMPTY( XSCALE2 )
  *@ 21,38 SAY XSCALE2
  @ 20,38 SAY XSCALE2
  XSCALE = GETSCALE( XSCALE2,SPACE(1))
*  @ 21,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
*  @ 21,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
*  @ 21,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
*  @ 21,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
*  @ 21,56 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
*  @ 21,60 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
  @ 20,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
  @ 20,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
  @ 20,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
  @ 20,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
  @ 20,56 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
  @ 20,60 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
ENDIF
lcSclStrn=XSCALE
*:B#802435,1 SSH(END)
*!*************************************************************
*! Name      : lpOldFoter.
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 06/16/1999
*! Purpose   : Print the form footer at in the middle of a spacific 
*!            pick ticket.
*!*************************************************************
*! Called from : ALPKTKCH.PRG
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
PROCEDURE lpOldFoter

@ 55,93 SAY XSTNAME
@ 56,93 SAY XSTADDR1
@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 57,93 SAY XSTADDR2
@ 58,93 SAY XSTADDR3
@ 63,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,SUBSTR(OrdHdr.CUSTPO,1,10))  
@ 63,100 SAY XSTORE                  
@ 63,109 SAY ORDHDR.DEPT            
@ 63,115 SAY XORDER
@ 64,101 SAY XPIKTKT
ROW = ROW + 1

*!*************************************************************
*! Name      : lpNewFoter.
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 06/16/1999
*! Purpose   : Print the form footer at the end of a spacific 
*!             pick ticket.
*!*************************************************************
*! Called from : ALPKTKCH.PRG
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
PROCEDURE lpNewFoter

*-- Not to print the total amount of the piktkt if the 
*-- user does not want to print the style prices.
@ 54,000 SAY lcRpMsg2          
@ 55,000 SAY lcRpMsg3          
@ 56,093 SAY XSTNAME
@ 57,093 SAY XSTADDR1
*-- note to prevent printing any one of them if it has an '*'
*-- in front of it.
lnCol = 11
IF SUBSTR(OrdHdr.Note1, 1, 1) <> '*'
  @ 58,lnCol SAY OrdHdr.Note1
  lnCol = 42
ENDIF
@ 58,lnCol SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1)<>'*', OrdHdr.Note2, '')

@ 58,075 SAY XPIECES PICTURE '999999' 
@ 58,093 SAY XSTADDR2
@ 59,093 SAY XSTADDR3
@ 62,060 SAY SUBSTR(XSHIP_DATA,1,11) 
@ 62,076 SAY IIF(llrpstyprc,XVALUE,'') PICTURE '99999.99'
@ 63,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,SUBSTR(OrdHdr.CUSTPO,1,10))  
@ 63,100 SAY XSTORE                  
@ 63,109 SAY ORDHDR.DEPT            
@ 63,115 SAY XORDER
@ 64,101 SAY XPIKTKT
XVALUE = 0.00
ROW = ROW + 1


*!*************************************************************
*! Name      : lpPrtSku.
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 06/16/1999
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKCH.PRG
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
PROCEDURE lpPrtSku

IF !SEEK('S'+&lcLnTemp..Account+&lcLnTemp..Style,'Spck_Lin')
  lcStrToPrn = " "
  RETURN
ENDIF
SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  = SEEK('S'+Style.Scale,'Scale')
  = SEEK('M'+&lcLnTemp..Account,'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  GO RECNO() IN &lcPikTemp
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 
  = SEEK('S'+&lcLnTemp..Account+&lcLnTemp..Style,'Spck_Lin')
  lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnDime1)
  lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
  lnLength   = LEN(lcStrToPrn)+2
  lnLocCol = LEN(lcStrToPrn) + 5

ELSE
  @ ROW,01 SAY SUBSTR(Sku,1,8)
  lnLocCol = 35
  @ ROW,17 SAY 'CUSTOMER SKU #'
  DO lpSidLabls
  ROW = ROW + 1
ENDIF
RETURN

*!*************************************************************
*! Name      : lfPrnStrn.
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 06/16/1999
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKCH.PRG
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfPrnStrn
lcAlias=ALIAS()
lcSkuSize =' '
lcKey     =' '
IF SEEK('S'+&lcLnTemp..Account+&lcLnTemp..Style,'Spck_Lin')
  lcKey='S'+&lcLnTemp..Account+&lcLnTemp..Style
  lnSep=1
  Q=1
  W=STR(Q,1)
  X=1
  Z=STR(X,1) 
  SELECT Spck_Lin
  SCAN REST WHILE 'S'+Account+Style = lcKey
    IF &lcLnTemp..Qty&Z > 0
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

*!*************************************************************
*! Name      : lfGetLoc.
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 06/16/1999
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKCH.PRG
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetLoc
lcAlias=ALIAS()
SELECT WhsLoc
IF SEEK(&lcLnTemp..Style)
  SCAN REST WHILE Style = &lcLnTemp..Style
    lcStr = lcStr +" "+cLocation
  ENDSCAN
  lcStr=ALLTRIM(lcStr)
  lnLen=LEN(lcStr)
ENDIF
SELECT (lcAlias)


*!*************************************************************
*! Name      : lfContuLoc.
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 06/16/1999
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKCH.PRG
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfContuLoc
FOR I = 12 TO lnLen  
   @ ROW ,00 SAY SUBSTR(lcStr,I,86) 
   I=I+86
   DO lpSidLabls
   Row = Row + 1
ENDFOR

*!*************************************************************
*! Name      : lfClrTmp
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 06/16/1999
*! Purpose   : Function to erase temp files.
*!*************************************************************
*! Called from : ALPKTKCH.PRG
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfClrTmp

IF FILE(lcPikTemp+'.DBF') AND USED(lcPikTemp)
  USE IN (lcPikTemp)
  ERASE &gcWorkDir.&lcPikTemp+'.DBF'
  ERASE &gcWorkDir.&lcPikTemp+'.CDX'
ENDIF
IF FILE(lcLnTemp+'.DBF') AND USED(lcLnTemp)
  USE IN (lcLnTemp)
  ERASE &gcWorkDir.&lcLnTemp+'.DBF'
  ERASE &gcWorkDir.&lcLnTemp+'.CDX'
ENDIF