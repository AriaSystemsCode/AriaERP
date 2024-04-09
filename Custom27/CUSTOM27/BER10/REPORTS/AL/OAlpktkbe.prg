*:***************************************************************************
*: Program file : ALPKTKBE.PRG (Converted from 26 to 27 for Berkley)
*: DESC         : PRINT PICK TICKET - 66 LINE PAGE, 8 1/2" x 11" (For Brekley)
*: Module       : Aria Apparel Series.
*: DATE         : 01/03/2001
*: Developer    : Adel Mohammed El Gazzar (ADEL) 
*: Refer to     : C(102139)
**:************************************************************************
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
*:************************************************************************

*======Inialize variables
*----(1) Needed OG Variables
*--llRpStyLoc   && Print Style Location?
*--llRpSkuBck   && Print Sku/Pack?
*--llRpSkuSiz   && Print Sku by Size?
*--llRpStyPrc   && Print Style Prices?
*----(2) Needed Local Variables.
*
*--llScale      && Flag to print the size scales at the first page only
lnMajLen = LEN(gfItemMask("PM"))
lnLength=07
STORE SPACE(1) TO lcStrToPrn,lcSkuSize,lcSclStrn,lcStr
STORE 0 TO lnLen,ROW,lnRec,lnSca
STORE .T. TO llScale
STORE .F. TO llEndPT,llRet
STORE 1 TO lnNotLine
DIMENSION laAddress[1,1],laScales[1]
STORE ' ' TO lcPikTkt,lcOrder,lcStore,lcAccount,lcCustPO,laAddress,lcDivison,lcTermData
STORE {}  TO ldDate
STORE ' ' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6,lcScale,laScales
STORE ' ' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
STORE 0 TO XVALUE,XPIECES
*----(3) Form setup Variables.
*--llPrntComp   && Print copmany name?

lcDivLName = ''
laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

*-- Get the answers for the color and location printing,
*-- either from the memory file - if found - or from the user.
IF FILE('&gcDataDir.ALPKTKBE.MEM')
  RESTORE FROM &gcDataDir.ALPKTKBE.MEM ADDITIVE
ELSE
  STORE SPACE (1) TO  lcMClr
ENDIF
llPrntClr = (lcMClr = 'Y')
IF EMPTY(lcMClr) 
  lcMClr    = IIF(gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you wish to print the color description?')=1,'Y','N')
  llPrntClr = (lcMClr = 'Y')
ENDIF
SAVE ALL LIKE lcM* TO &gcDataDir.ALPKTKBE.MEM
RELEASE ALL LIKE lcM*
*-- Get default warehous values.
SELECT WAREHOUS
LOCATE
HLine1 = SUBSTR(CDESC,1,30)
HLine2 = cAddress1
HLine3 = cAddress2
HLine4 = SUBSTR(cAddress3,1,15) + ' ' + SUBSTR(cAddress4,1,3)+ ' ' + SUBSTR(cAddress5,1,10)
IF LEN(TRIM(HLine3)) = 0
  HLine3 = HLine4
  HLine4 = SPACE(1)
ENDIF
SET ORDER TO TAG Whslocst in WhsLoc
SET ORDER TO TAG Spcklins in spck_lin

*-- MAIN LOOP
*-------------
*-- MAIN LOOP
*-------------
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
*-- Get the data file.
*-- This file holds all the lines of the selected pick tickets.
SELECT (lcTmpOrdL)
INDEX ON PikTkt + Order +cGrupDetal + SCALE+ STR(LINENO,6) TAG 'SCLTEMP'
SET ORDER TO (lcTmpOrdL)
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
  *--Print the Header lables.
  DO lpHrdLabls
    *------------------
  * LINE LOOP
  *------------------
  SELECT (lcTmpOrdL)
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6)=lcPikTkt+lcOrder+'D'
    DO CASE
      CASE TotPik <= 0
        LOOP
      CASE ROW >= 55
        DO lpHrdLabls
    ENDCASE
    IF SEEK(Style,'STYLE')
      XSTYDESC   = STYLE.DESC
      lcStyLocat = STYLE.Location
      lcScale    = STYLE.Scale
    ELSE
      XSTYDESC   = ''
      lcStyLocat = ''
      lcScale    = ''
    ENDIF
    lcColorDesc  = SUBSTR(gfCodDes(SUBSTR(STYLE,lnMajLen+2) , 'COLOR'),1,15)
    @ ROW,1 SAY STYLE
    @ ROW,20 SAY lcScale
    @ ROW,22 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,26 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,31 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,35 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,40 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,44 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,49 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,53 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,59 SAY TOTPIK PICTURE '99999'
    IF llRpStyPrc
      @ ROW,66 SAY PRICE PICTURE '9999.99'
    ENDIF
    @ ROW,75 SAY TOTPIK PICTURE '99999'
    DO lpSidLabls
    ROW = ROW + 1
    =lfChkRow("B")
    IF !llRpSkuBck
      @ ROW,1 SAY XStyDesc
      @ ROW,33 SAY IIF(llPrntClr,lcColorDesc, '')
      IF llRpStyLoc
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
      IF SEEK('P'+lcAccount+&lcTmpOrdL..Style,'SPCK_LIN')
        @ ROW,1 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
        @ ROW,25 SAY XStyDesc
        @ ROW,59 SAY IIF(llPrntClr,SUBSTR(lcColorDesc,1,10), '')
        DO lpSidLabls
        Row = Row + 1
        =lfChkRow("B")
      ELSE
        DO lpPrtSku      && Procedure to print the Style/Color Sku no.
        IF !llRpSkuSiz
          @ ROW,00 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
          @ ROW,IIF(LEN(lcStrToPrn)>=40,41,LEN(lcStrToPrn)+2) SAY XStyDesc
          lcCol=IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34)
          @ ROW,lcCol SAY IIF(llPrntClr,SUBSTR(lcColorDesc,1,76-lcCol), '')
          DO lpSidLabls
          Row = Row + 1
          =lfChkRow("B")
          IF llRpStyLoc
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
          @ ROW,00 SAY XStyDesc
          @ ROW,33 SAY IIF(llPrntClr,lcColorDesc, '')
          IF llRpStyLoc
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
    DO lpSidLabls
    Row = Row + 1
    =lfChkRow("B")
    SELECT (lcTmpOrdL)
    XVALUE  = XVALUE + TOTPIK * PRICE
    XPIECES = XPIECES + TOTPIK
    SELECT (lcTmpOrdL)
  ENDSCAN
  =lfPrintNot()
  lnNotLine = 1
  llEndPT = .T.
  DO lpSidLabls
  ROW = ROW + 1
  IF ROW >= 51
    DO lpOldFoter
    DO lpHrdLabls
  ENDIF
  DO lpFilEmpLn        
  DO lpNewFoter
  llScale = .T.
  SELECT PIKTKT
  =SEEK(lcPikTkt)
  REPLACE PRTFLAG WITH 'P'
  SELECT (lcTmpOrdL)
  IF !EOF()
    SKIP
  ENDIF  
ENDDO
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name : lpFilEmpLn.
*! Auth : Adel Mohammed El Gazzar (ADEL).
*! Date : 01/03/2001.
*!*************************************************************
*! Synopsis : Fill the empty pick ticit lines.
*!*************************************************************
PROCEDURE lpFilEmpLn

DO WHILE Row <= 50
  DO lpSidLabls
  Row = Row + 1
ENDDO


*!*************************************************************
*! Name : lpSidLabls.
*! Auth : Adel Mohammed El Gazzar (ADEL).
*! Date : 01/03/2001.
*!*************************************************************
*! Synopsis : Print the Side labels.
*!*************************************************************
PROCEDURE lpSidLabls

DO CASE
  CASE ROW = 34 .OR. ROW = 50
    @ ROW,93 SAY IIF(llPrntComp,HLINE1,'')
  CASE ROW = 35 .OR. ROW = 51
    @ ROW,93 SAY IIF(llPrntComp,HLINE2,'')
  CASE ROW = 36 .OR. ROW = 52
     @ ROW,93 SAY IIF(llPrntComp,HLINE3,'')
  CASE ROW = 37 .OR. ROW = 53
    IF llEndPT .AND. Row = 53
      @ 53,000 SAY ALLTRIM(lcRpMsg1)
    ENDIF  
    @ ROW,93 SAY IIF(llPrntComp,HLINE4,'')
  CASE ROW = 23 .OR. ROW = 40
    @ ROW,93 SAY lcStName
  CASE ROW = 24 .OR. ROW = 41
    @ ROW,93 SAY lcStAdd1
  CASE ROW = 25 .OR. ROW = 42
    @ ROW,93 SAY lcStAdd2
  CASE ROW = 26 .OR. ROW = 43
    @ ROW,93 SAY lcStAdd3
  CASE ROW = 30 .OR. ROW = 46
    @ ROW,086 SAY lcCustPO
    @ ROW,100 SAY lcStore
    @ ROW,111 SAY ORDHDR.DEPT
    @ ROW,120 SAY lcOrder
  CASE ROW = 31 .OR. ROW = 48
    @ ROW,101 SAY lcPikTkt
    @ ROW,108 SAY 'SHIP VIA: ' + SUBSTR(lcShipVia,1,10)
ENDCASE
*!*************************************************************
*! Name : lpHrdLabls.
*! Auth : Adel Mohammed El Gazzar (ADEL).
*! Date : 01/03/2001.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Print the Header labels.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Called from : 
*!         Procedures : Alo820E.
*!*************************************************************
PROCEDURE lpHrdLabls

*-- Line 0
@ 02,001 SAY IIF(llPrntComp,HLINE1,'')
@ 02,095 SAY IIF(llPrntComp,HLINE1,'')
  
*-- Line 1
@ 03,001 SAY IIF(llPrntComp,HLINE2,'')
@ 03,095 SAY IIF(llPrntComp,HLINE2,'')
  
*-- Line 2
@ 04,001 SAY IIF(llPrntComp,HLINE3,'')
@ 04,095 SAY IIF(llPrntComp,HLINE3,'')
  
*-- Line 3
@ 05,001 SAY IIF(llPrntComp,HLINE4,'')
@ 05,61 SAY lcDivison
@ 05,74 SAY lcPikTkt
@ 05,095 SAY IIF(llPrntComp,HLINE4,'')
  
*-- Line 4
@ 07,61 SAY ORDHDR.APPROVAL
@ 07,095 SAY lcStName                 
  
*-- Line 5
@ 08,74 SAY ldDate
@ 08,095 SAY lcStAdd1                 
  
*-- Line 6
@ 09,095 SAY lcStAdd2
  
*-- Line 7
@ 10,005 SAY lcBtName
@ 10,048 SAY lcStName
@ 10,095 SAY lcStAdd3
  
*-- Line 8
@ 11,005 SAY lcBtAdd1
@ 11,048 SAY lcStAdd1
  
*-- Line 9
@ 12,005 SAY lcBtAdd2
@ 12,048 SAY lcStAdd2
  
*-- Line 10
@ 13,005 SAY lcBtAdd3
@ 13,048 SAY lcStAdd3
@ 13,086 SAY lcCustPO
@ 13,100 SAY lcStore    
@ 13,111 SAY ORDHDR.DEPT
@ 13,120 SAY lcOrder
  
*-- Line 11
@ 15,101 SAY lcPikTkt
@ 15,108 SAY 'SHIP VIA: ' + SUBSTR(lcShipVia,1,10) 
*-- Line 13
@ 17,093 SAY IIF(llPrntComp,HLINE1,'')

*-- Line 15
@ 18,1 SAY lcAccount
@ 18,7 SAY lcOrder
@ 18,13 SAY ORDHDR.REP1
@ 18,17 SAY ORDHDR.REP2
@ 18,21 SAY ORDHDR.ENTERED
@ 18,30 SAY ORDHDR.START
@ 18,38 SAY ORDHDR.COMPLETE
@ 18,46 SAY SUBSTR(lcTermData,1,14)
@ 18,61 SAY lcStore    PICTURE '#######'
@ 18,69 SAY lcCustPO   PICTURE '###########'  

@ 18,80 SAY ORDHDR->DEPT 
@ 18,093 SAY IIF(llPrntComp,HLINE2,'')

@ 19,093 SAY IIF(llPrntComp,HLINE3,'')

IF !llScale
  @ 20,093 SAY IIF(llPrntComp,HLINE4,'')
ENDIF  
ROW = 20
IF llScale
  I = 1
  ROW = 20
  FOR I = 1 TO lnSca
    Z = STR(I,1)
     IF ROW >= 55
      llEndPT = .F.
      DO lpFilEmpLn   
      DO lpOldFoter
      DO lpHrdLabls
    ENDIF 
    IF !EMPTY(laScales[I])
      @ ROW,20 SAY laScales[I]
      lcScale = GETSCALE(laScales[lnSca],SPACE(1))
      =SEEK('S'+laScales[I],'SCALE')
      @ ROW,22 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,4),4,' ')
      @ ROW,26 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,4),4,' ')
      @ ROW,31 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,4),4,' ')
      @ ROW,35 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,4),4,' ')
      @ ROW,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,4),4,' ')
      @ ROW,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,4),4,' ')
      @ ROW,49 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,4),4,' ')
      @ ROW,53 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,4),4,' ')
      IF I = 1
        @ ROW,93 SAY IIF(llPrntComp,HLINE4,'')            
      ENDIF
      DO lpSidLabls
      ROW = ROW+1
    ENDIF   
  ENDFOR  
  lcSclStrn=lcScale
  llScale =.F.
  DO lpSidLabls
  ROW=ROW+1
ENDIF 
lcSclStrn=lcScale
ROW = IIF(ROW<23,23,ROW)


*!*************************************************************
*! Name : lpOldFoter.
*! Auth : Adel Mohammed El Gazzar (ADEL).
*! Date : 01/03/2001.
*!*************************************************************
*! Synopsis : Print the form footer at in the middle of a spacific 
*!            pick ticket. 
*!*************************************************************
PROCEDURE lpOldFoter

@ 55,93 SAY  lcStName
@ 56,93 SAY  lcStAdd1
@ 57,12 SAY  '*** CONTINUED NEXT PAGE ***'
@ 57,93 SAY  lcStAdd2
@ 58,93 SAY  lcStAdd3
@ 63,086 SAY lcCustPO
@ 63,100 SAY lcStore                  
@ 63,111 SAY ORDHDR->DEPT            
@ 63,120 SAY lcOrder
@ 64,101 SAY lcPikTkt
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(lcShipVia,1,10)
ROW = ROW + 1

*!*************************************************************
*! Name : lpNewFoter.
*! Auth : Adel Mohammed El Gazzar (ADEL).
*! Date : 01/03/2001.
*!*************************************************************
*! Synopsis : Print the form footer at the end of a spacific 
*!            pick ticket. 
*!*************************************************************
PROCEDURE lpNewFoter

*-- Not to print the total amount of the piktkt if the 
*-- user does not want to print the style prices.
=lfChkRow("A")
@ 51,93 SAY IIF(llPrntComp,HLINE2,'')
@ 52,93 SAY IIF(llPrntComp,HLINE3,'')
@ 53,93 SAY IIF(llPrntComp,HLINE4,'')
@ 54,000 SAY lcRpMsg2          
@ 55,000 SAY lcRpMsg3          
@ 56,093 SAY lcStName
@ 57,093 SAY lcStAdd1

lnCol = 11
IF SUBSTR(OrdHdr->Note1, 1, 1) <> '*'
  @ 58,lnCol SAY OrdHdr->Note1
  lnCol = 42
ENDIF
@ 58,lnCol SAY IIF(SUBSTR(OrdHdr->Note2, 1, 1)<>'*', SUBSTR(OrdHdr->Note2,1,25), '')

@ 58,076 SAY XPIECES PICTURE '999999' 
@ 58,093 SAY lcStAdd2
@ 59,093 SAY lcStAdd3
@ 62,060 SAY SUBSTR(lcShipVia,1,11) 
@ 62,076 SAY IIF(llRpStyPrc,XVALUE,'') PICTURE '999999.99'
@ 63,086 SAY IIF(ORDHDR->MultiPO,lcCustPO,OrdHdr->CUSTPO)  
@ 63,100 SAY lcStore                  
@ 63,111 SAY ORDHDR->DEPT            
@ 63,120 SAY lcOrder
@ 64,101 SAY lcPikTkt
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(lcShipVia,1,10)
XVALUE = 0.00
ROW = ROW + 1

*!***************************************************************
*! Name : lpPrtSku.
*! Auth : Tarek Mohamed Ismael
*! Date : 03/28/96.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

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

*!***************************************************************
*! Name : lfPrnStrn.
*! Auth : Tarek Mohamed Ismael
*! Date : 03/26/96.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

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
  SCAN REST WHILE type+account+style+pack_id = lcKey
    IF &lcTmpOrdL..Qty&Z > 0
       *lcSkuSize = lcSkuSize+'S'+W+':'+SUBSTR(Pack_Id,1,5)+' '
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


*!***************************************************************
*! Name : lfGetLoc.
*! Auth : Tarek Mohamed Ismael
*! Date : 03/26/96.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

FUNCTION lfGetLoc

lcAlias=ALIAS()
*--Initialize the variable to prevent carrying over of locations.
lcStr=" "
SELECT WhsLoc
*SET ORDER TO TAG Whslocst
IF SEEK(&lcTmpOrdL..Style)
  SCAN REST WHILE style+color+cwarecode+clocation= &lcTmpOrdL..Style
    lcStr = lcStr +" "+cLocation
  ENDSCAN
  lcStr=ALLTRIM(lcStr)
  lnLen=LEN(lcStr)
ENDIF
SELECT (lcAlias)

*!***************************************************************
*! Name      : lfContuLoc.
*! Developer : Adel Mohammed El Gazzar (ADEL) 
*! Date      : 02/14/1999
*!***************************************************************
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

FUNCTION lfContuLoc
FOR I = 12 TO lnLen  
   @ ROW ,00 SAY SUBSTR(lcStr,I,86) 
   I=I+86
   DO lpSidLabls
   Row = Row + 1
  =lfChkRow("B")
ENDFOR

*!*************************************************************
*! Name      : lfChkRow
*! Developer : Adel Mohammed El Gazzar (ADEL) 
*! Date      : 02/14/1999
*! Purpose   : To check the row position.
*!*************************************************************
*! Passed Parameters  : lcType : To check whether the row is 54 or 55
*!*************************************************************
*! Example            :  lfChkRow()
*!*************************************************************
*:B801938,1 KHM 02/14/99  Added
*!*************************************************************
FUNCTION lfChkRow
PARAMETER lcType
IF Row  >= IIF(lcType = "A",54,55)
  DO lpOldFoter
  DO lpHrdLabls
ENDIF

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
  lcBtAdd&lcCount = ALLTRIM(lcBtAdd&lcCount) + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
  ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
ENDFOR
*-- SHIP_TO ADDRESS FOR THIS STORE
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6
IF OrdHdr.Alt_ShpTo
  lcStName  = OrdHdr.STNAME
  =gfGetAdr('ORDHDR','','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = ALLTRIM(lcStAdd&lcCount) + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  ENDFOR  
ELSE
  IF !EMPTY(CUSTOMER.Dist_Ctr)
     =SEEK("S"+CUSTOMER.Account+CUSTOMER.Dist_Ctr)
  ENDIF
  lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
  =gfGetAdr('CUSTOMER','','','',1,'')
  *--Get the Ship To adddess except the country.    
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = ALLTRIM(lcStAdd&lcCount) + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
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
lcDivison = SUBSTR(gfCodDes(OrdHdr.CDIVISION , 'CDIVISION'),1,7)
*--Division long name.
STORE '' TO lcDivLName
=gfRltFld(OrdHdr.cDivision,@laDivLName,'CDIVISION')
HLINE1 = IIF(!EMPTY(lcDivLName),lcDivLName,HLINE1)

*--Get all scales for the current PK
SELECT (lcTmpOrdL)
lcKey = EVAL(KEY())
SET ORDER TO TAG 'SCLTEMP'
LOCATE
STORE 0 TO lnRec,lnSca
lnRec = RECCOUNT()
lcPikKey = PIKTKT+ORDER+'D'
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

FUNCTION lfPrintNot

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
llEndPT = .F.