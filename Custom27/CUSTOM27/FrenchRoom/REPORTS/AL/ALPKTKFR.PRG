*:******************************************************************************************
*:Program file : alpktkfr.PRG
*:Program desc. : Pick Ticket Form for the French room
*:System : Sales order allocation
*:Developer : Ahmed Abdel Naby- (AAN) Due to C#101983
*:Date : 02/10/2000
*:******************************************************************************************
*:Calls: 
*:		Functions: 
*:******************************************************************************************
*:Passed Parameters:
*:******************************************************************************************
*:Example : Do alpktkog
*:******************************************************************************************
*: Modifications
*:B803968,1 AME 01/18/2001 fix the bug of not printing the most left charecter in the cust po.
*:B604639,1 BWA 11/20/2001 Fix the bug of Incorrect address.
*:B604639,1                Fix the bug of not printing the first line of the 2nd PIKTKT.
*:******************************************************************************************
*-- Due to C101854,1
SELECT (lcTmpOrdL)
LOCATE

*B604639,1 BWA 11/20/2001 Delete the line of the notepad in the Temp. file.[START]
SCAN FOR cGrupDetal = "H"
  DELETE
ENDSCAN
LOCATE
*B604639,1 BWA 11/20/2001.[END]

IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
ELSE
  = (TYPE("lnMajSeg") = "C") AND lfEvalSegs()
  DO lpPrintRep
ENDIF
*-- end of main report code.

*!*******************************************************************************************
*! Name : lpPrintRep
*! Developer : Ahmed Abdel Naby (AAN)
*! Date : 02/10/2000
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

*B604639,1 BWA 11/20/2001 Fix the bug of Incorrect address.[START]
llEndPkFr = .T.
*B604639,1 BWA 11/20/2001.[END]

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
     *XDATE    = PIKDATE
     XSTORE    = &lcTmpOrdL..STORE
     SELECT ORDHDR
     XMULTI   = MULTI
     XACCOUNT = ACCOUNT

     lcCustPoVl = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)
     *B803968,1 AME [Start]  take the most left chr into the substr.
     *lcaCustPoVl = IIF(ORDHDR.MultiPO,PADR(SUBSTR(&lcTmpOrdL..CustPO,2,10),9),PADR(SUBSTR(ORDHDR.CustPO,2,10),9))
     lcaCustPoVl = IIF(ORDHDR.MultiPO,LEFT(ALLTRIM(&lcTmpOrdL..CustPO),9),LEFT(ALLTRIM(ORDHDR.CustPO),9))
     *B803968,1 AME [END]
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

    *B604639,1 BWA 11/20/2001 Fix the bug of Incorrect address.[START]
    *XSTADDR3 = TRIM(OrdHdr.cAddress3) + ' ' + TRIM(OrdHdr.cAddress4) + ' ' + OrdHdr.cAddress5
    XSTADDR3 = TRIM(OrdHdr.cAddress3)
    *B604639,1 BWA 11/20/2001.[END]

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
  ROW     = 24

  DO lpAplyLins	  

  IF NewDoc
    IF llSkipVe
      llSkipVe = .F.
      SELECT (lcTmpOrdL)
      
      *B604639,1 BWA 11/20/2001 Fix the bug of Incorrect address.[START]
      *SKIP -1
      IF llEndPkFr
        llEndPkFr = .F.
      ELSE
        SKIP -1
      ENDIF
      *B604639,1 BWA 11/20/2001.[END]

    ENDIF
    lnNotLine = 1
    llEndPT = .T.
    DO lpNewFoter

    *B604639,1 BWA 11/20/2001 Fix the bug of not printing the first line of the 2nd PIKTKT.[START]
    IF PikTkt # XPIKTKT
      SKIP - 1 
    ENDIF
    *B604639,1 BWA 11/20/2001.[END]

  ELSE
    llEndPT = .F.
	@56,12 SAY '*** CONTINUED NEXT PAGE ***'
	*@62,61 SAY SUBSTR(laCodDesc[2,3],1,12) 
	@61,61 SAY SUBSTR(laCodDesc[2,3],1,12) 
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

@ 01,072 SAY XPIKTKT

@ 10,004 SAY XBTNAME
@ 10,048 SAY XSTNAME

@ 11,004 SAY laBtAddr[1]
@ 11,048 SAY XSTADDR1
  
@ 12,004 SAY laBtAddr[2]
@ 12,048 SAY XSTADDR2
  
@ 13,004 SAY laBtAddr[3]
@ 13,048 SAY XSTADDR3

@ 14,048 SAY lcXPhone

@ 16,028 SAY "Customer PO#"
@ 18,002 SAY PADR(XACCOUNT,5)
@ 18,008 SAY PADR(ORDHDR.ENTERED,9)
@ 18,018 SAY PADR(ORDHDR.START,9)
@ 18,028 SAY lcCustPoVl
@ 18,044 SAY PADR(XORDER,8)
@ 18,053 SAY ORDHDR.CDIVISION
@ 18,061 SAY lcaCustPoVl
@ 18,071 SAY ORDHDR.DEPT 
@ 18,077 SAY SUBSTR(XSTORE,1,5)

*-- Line 17 PRINT 1 ST SIZE SCALE
@ 21,31 SAY PADR(XSCALE1,2)

XSCALE = GETSCALE( XSCALE1,SPACE(1))
@ 21,33 SAY PADL(ALLT(SCALE.SZ1),IIF(SUBSTR(ALLT(SCALE.SZ1),3,1)$'ABCDE',2,3))
@ 21,37 SAY PADL(ALLT(SCALE.SZ2),IIF(SUBSTR(ALLT(SCALE.SZ2),3,1)$'ABCDE',2,3))
@ 21,41 SAY PADL(ALLT(SCALE.SZ3),IIF(SUBSTR(ALLT(SCALE.SZ3),3,1)$'ABCDE',2,3))
@ 21,44 SAY PADL(ALLT(SCALE.SZ4),IIF(SUBSTR(ALLT(SCALE.SZ4),3,1)$'ABCDE',2,3))
@ 21,48 SAY PADL(ALLT(SCALE.SZ5),IIF(SUBSTR(ALLT(SCALE.SZ5),3,1)$'ABCDE',2,3))
@ 21,52 SAY PADL(ALLT(SCALE.SZ6),IIF(SUBSTR(ALLT(SCALE.SZ6),3,1)$'ABCDE',2,3))
@ 21,56 SAY PADL(ALLT(SCALE.SZ7),IIF(SUBSTR(ALLT(SCALE.SZ7),3,1)$'ABCDE',2,3))
@ 21,59 SAY PADL(ALLT(SCALE.SZ8),IIF(SUBSTR(ALLT(SCALE.SZ8),3,1)$'ABCDE',2,3))


*-- Line 18 PRINT 2ND. SIZE SCALE
IF .NOT. EMPTY( XSCALE2 )
  @ 22,31 SAY PADR(XSCALE2,2)
  XSCALE = GETSCALE( XSCALE2,SPACE(1))
  @ 22,33 SAY PADL(ALLT(SCALE.SZ1),IIF(SUBSTR(ALLT(SCALE.SZ1),3,1)$'ABCDE',2,3))
  @ 22,37 SAY PADL(ALLT(SCALE.SZ2),IIF(SUBSTR(ALLT(SCALE.SZ2),3,1)$'ABCDE',2,3))
  @ 22,41 SAY PADL(ALLT(SCALE.SZ3),IIF(SUBSTR(ALLT(SCALE.SZ3),3,1)$'ABCDE',2,3))
  @ 22,44 SAY PADL(ALLT(SCALE.SZ4),IIF(SUBSTR(ALLT(SCALE.SZ4),3,1)$'ABCDE',2,3))
  @ 22,48 SAY PADL(ALLT(SCALE.SZ5),IIF(SUBSTR(ALLT(SCALE.SZ5),3,1)$'ABCDE',2,3))
  @ 22,52 SAY PADL(ALLT(SCALE.SZ6),IIF(SUBSTR(ALLT(SCALE.SZ6),3,1)$'ABCDE',2,3))
  @ 22,56 SAY PADL(ALLT(SCALE.SZ7),IIF(SUBSTR(ALLT(SCALE.SZ7),3,1)$'ABCDE',2,3))
  @ 22,59 SAY PADL(ALLT(SCALE.SZ8),IIF(SUBSTR(ALLT(SCALE.SZ8),3,1)$'ABCDE',2,3))
ENDIF
lcSclStrn = XSCALE

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
*-- note to prevent printing any one of them if it has an '*'
*-- in front of it.
@ 57,18 SAY IIF(SUBSTR(OrdHdr.Note1, 1, 1) <> '*',OrdHdr.Note1,'')
@ 58,18 SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1)<>'*', OrdHdr.Note2,'')
@ 58,078 SAY XPIECES PICTURE '999999' 
*@ 62,061 SAY SUBSTR(laCodDesc[2,3],1,12) 
@ 61,061 SAY SUBSTR(laCodDesc[2,3],1,12) 
*@ 62,076 SAY IIF(llRpStyPrc,XVALUE,'') PICTURE '99999.99'
@ 61,076 SAY IIF(llRpStyPrc,XVALUE,'') PICTURE '99999.99'
XVALUE = 0.00
ROW = ROW + 1


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
PARAMETER lcReturn
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

  @ ROW,02 SAY SUBSTR(Style,1,LnMajorlen)
  @ ROW,18 SAY SUBSTR(Style,lnNonMajSt,Lncolorlen)  
  @ ROW,31 SAY ALLTRIM(lcScale)
  @ ROW,33 SAY IIF(PIK1<>0,STR(PIK1,3),SPACE(3))
  @ ROW,37 SAY IIF(PIK2<>0,STR(PIK2,3),SPACE(3))
  @ ROW,41 SAY IIF(PIK3<>0,STR(PIK3,3),SPACE(3))
  @ ROW,44 SAY IIF(PIK4<>0,STR(PIK4,3),SPACE(3))
  @ ROW,48 SAY IIF(PIK5<>0,STR(PIK5,3),SPACE(3))
  @ ROW,52 SAY IIF(PIK6<>0,STR(PIK6,3),SPACE(3))
  @ ROW,56 SAY IIF(PIK7<>0,STR(PIK7,3),SPACE(3))
  @ ROW,59 SAY IIF(PIK8<>0,STR(PIK8,3),SPACE(3))
  @ ROW,63 SAY PADL(TOTPIK,5) PICTURE '@Z 99999'
    
  IF llRpStyPrc
    @ ROW,69 SAY PADL(PRICE,8) PICTURE '99999999'
  ENDIF
  @ ROW,82 SAY STR((TOTQTY - TOTPIK),3)
  
  ROW = ROW + 1

  *-- To print the color description, And the 
  *-- Style/Color location.
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
    *CASE ROW>=56
     CASE ROW>=47
      NEWDOC = .F.
      SKIP

      *B604639,1 BWA 11/20/2001 Fix the bug of Incorrect address.[START]
      IF PikTkt # XPIKTKT
        NEWDOC = .T.
        llEndPkFr = .T.
      ENDIF
      SKIP - 1 
      *B604639,1 BWA 11/20/2001.[END]

      EXIT
  ENDCASE    

ENDSCAN
llSkipVe = .T.

*-- END PRINT LINE LOOP
=SEEK(lcOrdHKey,"ORDHDR")
*-- end of lpAplyLins.
