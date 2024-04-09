*:******************************************************************************************
*:Program file : alpktkog.PRG
*:Program desc. : Pick Ticket Form for OGIO
*:System : Sales order allocation
*:Developer : Ahmed Abdel Naby (AAN)
*:Date : 17/8/2000
*:******************************************************************************************
*:Calls: 
*:		Functions: 
*:******************************************************************************************
*:Passed Parameters:
*:******************************************************************************************
*:Example : Do alpktkog
*:******************************************************************************************
*:Modifications :
*:C#102445,1 ABD 08/29/2001 Print 11 char from the the customer Po.
*:******************************************************************************************
*:

*-- Due to C101854,1
SELECT (lcTmpOrdL)
LOCATE
IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
ELSE
  *-- Call this function once per OG session.
  = (TYPE("lnMajSeg") = "C") AND lfEvalSegs()
  lcSpckOrd = ORDER("SPCK_LIN")
  SET ORDER TO SPCKLINS IN SPCK_LIN
  DO lpPrintRep
  SET ORDER TO &lcSpckOrd. IN SPCK_LIN	
ENDIF
*-- end of main report code.

*!*******************************************************************************************
*! Name : lpPrintRep
*! Developer : Ahmed Abdel Naby (AAN)
*! Date : 17/8/2000
*! Purpose : 
*!*******************************************************************************************
*! Calls :
*!*******************************************************************************************
*! Passed Parameters :
*!*******************************************************************************************
*! Return : 
*!*******************************************************************************************
*! Example : lpPrintRep
*!*******************************************************************************************

PROCEDURE lpPrintRep
*-- Variables declaration section [Starts
STORE 0.00 TO XORDTOT, XVALUE , SVALUE , XTOTQTY
llNoRec = .F.
HLINE2 = TRIM(laCompAdd[1])
HLINE3 = TRIM(laCompAdd[2])
HLINE4 = TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5]

IF LEN(HLINE3) = 0
  HLINE3=HLINE4
ENDIF
*-- Variables declaration section [Ends

lnLength=07
lcStrToPrn=' '
lcSkuSize =' '
lcSclStrn =' '
lnLen     = 0
lcStr     =' '

lcCustPoVl  = ''

*-- MAIN LOOP
llEndPT = .F.
NEWDOC  = .T.

lnNotLine = 1
SET DEVICE TO PRINT

lcStatuBar = SET("STATUS BAR")
lcMessIs  = SET("MESSAGE",1)

SET STATUS BAR ON
SET MESSAGE TO 'Printing - Press << Space Bar >> to abort'

DECLARE  laBtAddr[3,1] , laStAddr[3,1] , laCodDesc[5,3]
STORE "" TO laBtAddr , laStAddr
laCodDesc[1,2] = "CTERMCODE"
laCodDesc[2,2] = "SHIPVIA"
laCodDesc[3,2] = "SPCINST"
laCodDesc[4,2] = "SEASON"
laCodDesc[5,2] = "CDIVISION"

llSkipVe = .F.

SELECT (lcTmpOrdL)
LOCATE
SCAN REST WHILE INKEY()<> 32
  SELECT (lcTmpOrdL)
  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
	 WAIT WINDOW "Printing Pick Ticket : " + PIKTKT NOWAIT
     XPIKTKT  = PIKTKT
     XORDER   = ORDER
     XDATE    = PIKDATE
     XSTORE    = &lcTmpOrdL..STORE
     SELECT ORDHDR
     XMULTI   = MULTI
     XACCOUNT = ACCOUNT
     
     lcCustPoVl = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)

     STORE 0.00 TO XORDTOT, XVALUE , SVALUE
     *-- We added the STORE# to use the new created index (ORDLINST) which
     *-- consists of ORDER+STORE+STYLE+COLOR+STR(LINENO,6) in order to make
     *-- the search faster because one PIKTKT can't exceed one STORE. This
     *-- will work both form single or multi store orders. In single store
     *-- order, either the store will be empty or have something and all
     *-- the lines will have the same store. So this change will not improve
     *-- anything on the single store orders but it will on big multistore
     *-- orders. And in most cases the single store orders are not that big
     *-- anyway.

     SELECT ORDLINE
     SET ORDER TO ordlinst
     = SEEK("O" + XORDER+XSTORE)

     *-- To copy the records which have the selected
     *-- PIKTKT from ORLINE file into a temp file (LNTEMP) instead of
     *-- LOCATING in the ORDLINE file for each selected piktkt

     XBTNAME  = CUSTOMER.BTNAME

     = gfGetAdr('CUSTOMER' , '' , '' , '' , @laBtAddr , '2')
     IF LEN(TRIM(laBtAddr[2])) =0
        laBtAddr[2] = laBtAddr[3]
        laBtAddr[3] = ''
     ENDIF

     *-- GET DESCRIPTIONS FOR CODED FIELDS

     *TERMS
     laCodDesc[1,1] = ORDHDR.CTERMCODE
 
     *SHIPVIA
     laCodDesc[2,1] = ORDHDR.SHIPVIA

    *SPECIAL INST
    laCodDesc[3,1] = ORDHDR.SPCINST
     
    *SEASON
    laCodDesc[4,1] = ORDHDR.SEASON
     
    *DIVISION
    laCodDesc[5,1] = ORDHDR.CDIVISION

    =gfCodDes(@laCodDesc)	


    HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)      
    STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
    XPIECES = 0
    * GET DESCRIPTIONS FOR CODED FIELDS [end]
    *-- GET THE SIZE SCALES
    *-- Moved and enhanced to get the printed scales
    *-- once outside the loop not every time in the loop because it will 
    *-- be the same REM1
    XSCALE2   = SPACE(1)
    SELECT (lcTmpOrdL)
    XSCALE1 = SCALE
    lntmprec=RECNO()
    LOCATE REST FOR Scale<>xScale1
      IF Scale<>xScale1 
        IF !EOF()
          xScale2=Scale
        ENDIF
      ENDIF  
    GOTO lntmprec
  ENDIF
  *-- END PICK TKT/ORDER SELECTION.
  
  *-- SHIP-TO ADDRESS FOR THIS STORE
  *- Initialize the alt address if ther is any.
  SELECT OrdHdr
  
  IF Alt_ShpTo
  	XSTNAME  = OrdHdr.STName   
    XSTADDR1 = OrdHdr.cAddress1
    XSTADDR2 = OrdHdr.cAddress2
    XSTADDR3 = TRIM(OrdHdr.cAddress3) + ' ' + TRIM(OrdHdr.cAddress4) + ' ' + OrdHdr.cAddress5
    
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT Customer
    lnRecNo = RECNO()       && Variable to hold the record number.
    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    = gfGetAdr('CUSTOMER' , '' , '' , '' , @laStAddr)
    XSTADDR1 = laStAddr[1]
    XSTADDR2 = laStAddr[2]
    XSTADDR3 = laStAddr[3]

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
  ROW     = 22
  DO lpAplyLins	  

  IF NewDoc
    IF llSkipVe
      llSkipVe = .F.
      SELECT (lcTmpOrdL)
      SKIP -1
    ENDIF
    *-- To print the notepad.
    IF llRpOrdNot
      SELECT NotePad
      IF SEEK('B' + OrdHdr.Order)
        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 75
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)
        @ Row,00 SAY '* -- N O T E S -- *' 
        Row = Row + 1
        DO WHILE lnNotLine <= lnMemLins
          IF Row >= 53
            DO lpOldFoter
            DO lpHrdLabls
            Row = 22
          ENDIF
          @ ROW,00 SAY MLINE(MNOTES,lnNotLine)
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

ENDSCAN
WAIT CLEAR

SET MESSAGE TO lcMessIs
SET STATUS BAR &lcStatuBar

SET DEVICE TO SCREEN
RETURN
*-- END of report main code.

*!*************************************************************
*! Name : lpFilEmpLn.
*! Developer : Ahmed Abdel Naby (AAN)
*! Date : 17/8/2000
*!*************************************************************
*! Synopsis : Fill the empty pick tikit lines.
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!*************************************************************
PROCEDURE lpFilEmpLn
DO WHILE Row <= 53
  Row = Row + 1
ENDDO
*-- end of .

*!*************************************************************
*! Name : lpHrdLabls.
*! Auth : Ahmed Abdel Naby (AAN)
*! Date : 17/8/2000
*!*************************************************************
*! Synopsis : Print the Header labels.
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!*************************************************************
PROCEDURE lpHrdLabls
*-- Line 0
@ 00,001 SAY IIF(llPrntComp,HLINE1,'')          

IF llPrntComp
  *-- Line 1
  @ 01,001 SAY HLINE2

  *-- Line 2
  @ 02,001 SAY HLINE3            
  
  *-- Line 3
  @ 03,001 SAY HLINE4
ELSE
  =gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
  *-- Line 1
  @ 01,001 SAY ALLTRIM(lcDivLName)
ENDIF

@ 04,060 SAY SUBSTR(ALLTRIM(laCodDesc[5,3]),AT('-',laCodDesc[5,3])+1,11)
@ 04,073 SAY XPIKTKT

*-- Line 4
@ 06,060 SAY ORDHDR.APPROVAL

*-- Line 5
@ 07,073 SAY XDATE

*-- Line 7
@ 09,005 SAY XBTNAME
@ 09,048 SAY XSTNAME

*-- Line 8
@ 10,005 SAY laBtAddr[1]
@ 10,048 SAY XSTADDR1
  
*-- Line 9
@ 11,005 SAY laBtAddr[2]
@ 11,048 SAY XSTADDR2
  
*-- Line 10
@ 12,005 SAY laBtAddr[3]
@ 12,048 SAY XSTADDR3

*-- Line 15
@ 17,001 SAY XACCOUNT
@ 17,012 SAY XORDER
@ 17,022 SAY ORDHDR.REP1
@ 17,026 SAY ORDHDR.REP2
@ 17,031 SAY SUBSTR(DTOC(ORDHDR.ENTERED),1,5)
@ 17,037 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
@ 17,044 SAY ORDHDR.COMPLETE
@ 17,053 SAY SUBSTR(laCodDesc[1,3],AT('-',laCodDesc[1,3])+1,11)              
@ 17,065 SAY XSTORE         PICTURE '####'

*C#102445,1 ABD 08/29/2001 Print 11 char from the the customer Po. [Begin]
*@ 17,069 SAY lcCustPoVl  PICTURE '########' 
*@ 17,078 SAY ORDHDR.DEPT 
@ 17,069 SAY SUBSTR(lcCustPoVl,1,11)  PICTURE '###########' 
@ 17,082 SAY ORDHDR.DEPT 
*C102445,1 ABD [End]

*-- Line 17 PRINT 1 ST SIZE SCALE
@ 19,28 SAY XSCALE1

XSCALE = GETSCALE( XSCALE1,SPACE(1))
@ 19,30 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
@ 19,35 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
@ 19,39 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
@ 19,43 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
@ 19,47 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
@ 19,51 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
@ 19,55 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
@ 19,59 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')


*-- Line 18 PRINT 2ND. SIZE SCALE
IF .NOT. EMPTY( XSCALE2 )
  @ 20,28 SAY XSCALE2
  XSCALE = GETSCALE( XSCALE2,SPACE(1))
  @ 20,30 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
  @ 20,34 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
  @ 20,38 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
  @ 20,42 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
  @ 20,46 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
  @ 20,50 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
  @ 20,54 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
  @ 20,58 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
ENDIF
lcSclStrn = XSCALE
*!*************************************************************
*! Name : lpOldFoter.
*! Auth : Ahmed Abdel Naby (AAN)
*! Date : 17/08/2000
*!*************************************************************
*! Synopsis : Print the form footer at in the middle of a spacific 
*!            pick ticket. 
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!*************************************************************
PROCEDURE lpOldFoter
@ 56,12 SAY '*** CONTINUED NEXT PAGE ***'
ROW = ROW + 1

*!*************************************************************
*! Name : lpNewFoter.
*! Auth : Ahmed Abdel Naby (AAN)
*! Date : 17/08/2000
*!*************************************************************
*! Synopsis : Print the form footer at the end of a spacific 
*!            pick ticket. 
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!*************************************************************
PROCEDURE lpNewFoter
*-- Not to print the total amount of the piktkt if the 
*-- user does not want to print the style prices.
@ 52,000 SAY lcRpMsg1          
@ 53,000 SAY lcRpMsg2          
@ 54,000 SAY lcRpMsg3          


*-- note to prevent printing any one of them if it has an '*'
*-- in front of it.
lnCol = 11
IF SUBSTR(OrdHdr.Note1, 1, 1) <> '*'
  @ 57,lnCol SAY OrdHdr.Note1
  lnCol = 42
ENDIF
@ 57,lnCol SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1)<>'*', SUBSTR(OrdHdr.Note2,1,25), '')
@ 57,076 SAY XPIECES PICTURE '999999' 
@ 61,060 SAY SUBSTR(laCodDesc[2,3],1,11) 
@ 61,076 SAY IIF(llRpStyPrc,XVALUE,'') PICTURE '99999.99'
XVALUE = 0.00
ROW = ROW + 1
*!***************************************************************
*! Name : lpPrtSku.
*! Auth : Ahmed Abdel Naby (AAN)
*! Date : 17/08/2000
*!*************************************************************
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!***************************************************************

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
    nDime2 = SkuTmpl.Len4
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

*!***************************************************************
*! Name : lfPrnStrn.
*! Auth : Ahmed Abdel Naby (AAN)
*! Date : 17/08/2000
*!*************************************************************
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!***************************************************************

FUNCTION lfPrnStrn
lcAlias=ALIAS()
lcSkuSize =' '
lcKey     =' '

IF SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style , 'Spck_Lin')
  lcKey='S' + ORDHDR.Account + &lcTmpOrdL..Style
  lnSep=1
  Q=1
  W=STR(Q,1)
  X=1
  Z=STR(X,1) 
  SELECT Spck_Lin

  SCAN REST WHILE 'S' + ORDHDR.Account + &lcTmpOrdL..Style = lcKey

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
*-- end of .

*!***************************************************************
*! Name : lfGetLoc.
*! Auth : Ahmed Abdel Naby (AAN)
*! Date : 17/08/2000
*!*************************************************************
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!***************************************************************

FUNCTION lfGetLoc
lcAlias=ALIAS()
lcStr=" "
SELECT WhsLoc
IF SEEK(&lcTmpOrdL..Style)
  SCAN REST WHILE Style= &lcTmpOrdL..Style
    lcStr = lcStr +" "+cLocation
  ENDSCAN
  lcStr=ALLTRIM(lcStr)
  lnLen=LEN(lcStr)
ENDIF
SELECT (lcAlias)

*!***************************************************************
*! Name : lfContuLoc.
*! Auth : Ahmed Abdel Naby (AAN)
*! Date : 17/08/2000
*!*************************************************************
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!***************************************************************

FUNCTION lfContuLoc
FOR I = 12 TO lnLen  
  @ ROW ,00 SAY SUBSTR(lcStr,I,86) 
  I=I+86
  Row = Row + 1
ENDFOR

*!*************************************************************
*! Name : lfSpcWhen
*! Auth : Ahmed Abdel Naby (AAN)
*! Date : 17/08/2000
*!*************************************************************
*! Synopsis : 
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!*************************************************************

FUNCTION lfSpcWhen
PARAMETERS lcDummy

*-- Make intial setting
*-- SKU/PACK
llRpSkuBck = .T.
llRpSkuSiz =.T.
llRpStyLoc = .T.
CLEAR READ
lcSpWhen = "N"		&& Avoid doing special when function with each refresh.
RETURN lcDummy
*-- end of lfSpcWhen.

*!***************************************************************
*! Name : lfHaveUPC
*! Auth : Ahmed Abdel Naby (AAN)
*! Date : 17/08/2000
*!*************************************************************
*! Synopsis : 
*!*************************************************************
*! Called from : 
*!         Program : lfHaveUPC
*!***************************************************************
FUNCTION lfHaveUPC
PARAMETERS lcDummy
llUpcExist = (gfGetMemVar("M_UPC_USE") = "Y")
IF llUpcExist
  llUpcExist = lfSpcOpen("StyleUpc","StyleUpc")
  IF llUpcExist
    SELECT StyleUpc 
    LOCATE
    llUpcExist = !EOF()
  ENDIF  
ENDIF
*-- end of lfHaveUPC.

*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 17/08/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs
*PARAMETER lcReturn
STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF
*-- end of lfEvalSegs.

*!*******************************************************************************************
*! Name : lpAplyLins
*! Developer : Ahmed Abdel Naby (AAN)
*! Date : 17/8/2000
*! Purpose : Print PikTkt data
*!*******************************************************************************************
*! Calls :
*!*******************************************************************************************
*! Passed Parameters :
*!*******************************************************************************************
*! Return : 
*!*******************************************************************************************
*! Example : lpAplyLins
*!*******************************************************************************************

PROCEDURE lpAplyLins

lcOrdHKey = OrdHdr.cOrdType + OrdHdr.Order
*-- Scan lcTmpOrdL.
SCAN REST WHILE PikTkt + Order + cGrupDetal + STR(LineNo , 6) = ;
                XPIKTKT + XORDER FOR  LINENO <> 0
  
  IF SEEK(&lcTmpOrdL..Style,"Style")
    XSTYDESC   = Style.DESC
    lcStyLocat = Style.Location
    lcScale    = Style.Scale
  ELSE
    XSTYDESC   = ''
    lcStyLocat = ''
    lcScale    = ''
  ENDIF

  * To get the color desc.
  lcclrdesc=PADR(ALLTRIM(gfCodDes(SUBSTR(Style.Style,lnNonMajSt,lnColorLen),"COLOR")),30)
  @ ROW,00 SAY SUBSTR(Style,1,LnMajorlen)
  @ ROW,19 SAY SUBSTR(Style,lnNonMajSt,Lncolorlen)  
  @ ROW,28 SAY ALLTRIM(lcScale)
  @ ROW,29 SAY PIK1   PICTURE '@Z 9999'
  @ ROW,33 SAY PIK2   PICTURE '@Z 9999'
  @ ROW,37 SAY PIK3   PICTURE '@Z 9999'
  @ ROW,41 SAY PIK4   PICTURE '@Z 9999'
  @ ROW,45 SAY PIK5   PICTURE '@Z 9999'
  @ ROW,49 SAY PIK6   PICTURE '@Z 9999'
  @ ROW,53 SAY PIK7   PICTURE '@Z 9999'
  @ ROW,57 SAY PIK8   PICTURE '@Z 9999'
  @ ROW,61 SAY TOTPIK PICTURE '99999'
    
  IF llRpStyPrc
    @ ROW,66 SAY PRICE PICTURE '9999.99'
  ENDIF
  @ ROW,75 SAY TOTPIK PICTURE '9999'  && Moved on chr. to the right. 
  
  ROW = ROW + 1

  IF llRpSkuBck

    IF SEEK('P' + ORDHDR.Account + &lcTmpOrdL..Style , 'SPCK_LIN')
      @ ROW,00 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
      @ ROW,25 SAY 'STYLE DESC.:'+XStyDesc
      @ ROW,59 SAY IIF(llRpPrtClr,'CLR DESC.:'+SUBSTR(lcclrdesc,1,10),'') &&update
      Row = Row + 1
    ELSE
      DO lpPrtSku      && Procedure to print the Style/Color Sku no.
      IF !llRpSkuSiz
        @ ROW,00 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
        @ ROW,IIF(LEN(lcStrToPrn)>=40,41,LEN(lcStrToPrn)+2) SAY 'STYLE DESC.:'+XStyDesc
        lcCol=IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34)
        @ ROW,lccol SAY IIF(llRpPrtClr,'CLR DESC.:'+SUBSTR(lcclrdesc,1,76-lccol),'') &&update
        Row = Row + 1
        IF llRpStyLoc
          =lfGetLoc()
          IF !EMPTY(lcStr)
            @ ROW,00 SAY 'LOCATION:'+ SUBSTR(lcStr,1,85)
            Row = Row + 1
            IF LEN(lcStr) > 85
              =lfContuLoc()
            ENDIF
          ELSE
            Row = Row + 1  
          ENDIF
        ENDIF  
        Row = Row + 1
      ELSE
        @ ROW,00 SAY 'STYLE DESC.:'+XStyDesc
        @ ROW,33 SAY IIF(llRpPrtClr,'CLR DESC.:'+lcclrdesc,'') &&update
        IF llRpStyLoc
          =lfGetLoc()
          IF !EMPTY(lcStr)
            @ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,10)
            Row = Row + 1

            IF LEN(lcStr) > 18
              =lfContuLoc()
            ENDIF  
          ELSE
            Row = Row + 1  
         ENDIF
        ELSE
          Row = Row + 1  
        ENDIF  
        = lfPrnStrn() 
        lcSkuSize=lcStrToPrn+' '+lcSkuSize
        @ ROW,00 SAY SUBSTR(lcSkuSize,1,86)
        Row = Row + 1  
      ENDIF
    ENDIF  
   
  ELSE

    @ ROW,00 SAY 'STYLE DESC.:'+XStyDesc
    @ ROW,33 SAY IIF(llRpPrtClr,'CLR DESC.:'+lcclrdesc,'')
    IF llRpStyLoc
      =lfGetLoc()
      IF !EMPTY(lcStr)
        @ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,10)
        Row = Row + 1
        IF LEN(lcStr) > 18
          =lfContuLoc()
        ENDIF  
      ELSE
        Row = Row + 1  
      ENDIF
    ENDIF  
  ENDIF  

  IF llRpPrnUPC
    IF !llRpSkuBck 
      Row = Row + 1  
    ENDIF
    lnAlias= SELECT (0)
    lcStyKey = Style
    llFndUPC= SEEK(lcStyKey,'StyleUPC')
    IF llFndUPC
      SELECT StyleUPC
      lnCol = 00
      SCAN WHILE  lcStyKey = Style
        IF lnCol >= 75
          lnCol = 00
          ROW = ROW + 1
        ENDIF
        @ ROW,lnCol SAY cUPCNum1+cUPCNum2+cUPCNum3
        lnCol = lnCol + 15
      ENDSCAN     
    ENDIF
      SELECT(lnAlias)
      ROW = ROW + 1
  ENDIF
  *-- To print the color description, And the 
  *-- Style/Color location.
  Row = Row + 1
  SELECT (lcTmpOrdL)
  XTOTQTY = XTOTQTY+ TOTQTY
  XVALUE  = XVALUE + TOTPIK * PRICE
  XPIECES = XPIECES + TOTPIK

  DO CASE
    CASE EOF()
      NEWDOC=.T.
      EXIT
    CASE totpik<=0
      SKIP
      LOOP
    CASE ROW>=47
      NEWDOC=.F.
      EXIT
  ENDCASE    

ENDSCAN
llSkipVe = .T.

*-- END PRINT LINE LOOP
=SEEK(lcOrdHKey,"ORDHDR")
*-- end of lpAplyLins.        
