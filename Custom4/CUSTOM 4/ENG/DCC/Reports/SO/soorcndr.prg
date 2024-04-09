*!***********************************************************************************
*! Name         : SOORCNDR.PRG
*! Developer    : Mostafa Eid(MOS)
*! Date         : 01/20/2009
*! Module       : SO (Sales Order)
*! Purpose      : Custom Order Confirmation form for Direct Corporate Clothing (DIR03)
*! Reference    : C201099 TICKET# - T20080806.0009 
*!***********************************************************************************
*! Called from  : Option Grid
*!***********************************************************************************
*! Calls        : ......
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return       : None
*!***********************************************************************************
*! Modifications:
*!B608953,1 MMT 07/30/2009 Fix bug of error while preview [T20080806.0009]
*!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001]
*!C201741,1 Derby 11/30/2015 SO - send out order confirmations via the R Builder [T20151008.0011]
*!***********************************************************************************

** Printing 
loogscroll.cCRPapersize = 'A4'
loogScroll.cCROrientation = 'P'

IF loogScroll.llOGFltCh
 =lfEvalSegs()
ENDIF 

PRIVATE lnTargtFld,lcOrdKey,lnMaxSize,lcRltColor,lcFabColor
STORE '' TO lcTargtFl,lcEmail,lcWebsite,lcPasPar,lcUsername,lcUserPhone 
STORE 1 TO lnTargtFld,lnSorceFld
lcRltColor = SPACE(0)
lcFabColor = SPACE(0)
lcPasPar   = ' '
lnMaxSize  = 16								&& Max. No. of sizes per one line
lcEmail    = gfGetMemVar('M_CEMAIL')	   	&& Var. to hold the company email
lcWebsite  = gfGetMemVar('M_CWEBSITE')	 	&& Var. to hold the company website

** Open Tables
*Mariam 01/22/2012 Fix error at DCC[T20120118.0002]
*!*	IF !USED('SYUUSER')
*!*	 =gfOpenTable('SYUUSER','CUSER_ID','SH')
IF !USED('SYUUSER_A')
 =gfOpenTable('SYUUSER','CUSER_ID','SH','SYUUSER_A')
*Mariam 01/22/2012 Fix error at DCC[T20120118.0002]
ENDIF

IF !USED('CONTACT')
  =gfOpenTable(oAriaApplication.DataDir+'CONTACT','CONTACT','SH')
ENDIF

=lfCrtTmp()

DECLARE laLngDesc[1,2]
laLngDesc[1,1] = 'CLRLNAME'
laLngDesc[1,2] = 'lcRltColor'

SELECT ORDHDR
SET RELATION TO ORDER INTO (lcEmplTmp) ADDITIVE
SET RELATION TO cordtype+ order INTO (lcTargtFl) ADDITIVE

SELECT (lcTargtFl)
*!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][Start]
*SET FILTER TO TotQty != 0
SET FILTER TO IIF(lcRpBook = 'Y',TotBOOK != 0,TotQty != 0)
*!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][End]
SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO style INTO style ADDITIV

lcOrdKey = ''
*--variable hold the complete date.[START]
lcCompDat = {}

lnStyleLen = lnMajorLen + lnColorLen +1
lcIndexKey = 'CORDTYPE + ORDER + STORE + SUBSTR(Style,1,lnStyleLen)'
*--if there is more than one order line with same style/colour/size/complete date/price
*- it should accumulate into one entry 

=lfAccmulte()
=lfColData()

SELECT (lcTempOrd)
SET RELATION TO
lcSkipExpr  = [&lcTargtFl]
SELECT ORDHDR
SET SKIP TO &lcSkipExpr
lcRpExp = lcRpExp + ' AND !EOF(lcTempOrd)'

DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
*!C201741,1 Derby 11/30/2015 SO - send out order confirmations via the R Builder [Start]
IF TYPE('lcXMLFileName')= 'C'
  loRequestObj = loAgent.GetRequest(lcRequestID, ClientID)
  MailTo=loRequestObj.CompleteNotification.To
  IF TYPE('MailTo') <> 'U' AND !IsNull(MailTo)
    SELECT (lcOrdPrtUp)
    LOCATE
    SCAN
      SELECT ORDHDR
      =SEEK(EVAL(lcOrdPrtUp +'.CORDTYPE')+EVAL(lcOrdPrtUp +'.ORDER'),'ORDHDR')
      *IF upper(alltrim(ORDHDR.Note2)) = upper(alltrim(MailTo))
      IF ALLTRIM(MailTo) = "<Notes2>" 
        REPLACE LORDML WITH .T.
      ENDIF
    ENDSCAN
    SELECT ORDHDR
    =gfTableUpdate()
  ENDIF
ENDIF 
*!C201741,1 Derby 11/30/2015 SO - send out order confirmations via the R Builder [End]

llSalsOrd = .F.

IF USED('CONTACT')
  gfCloseTable('CONTACT')
ENDIF
RETURN

*!***********************************************************************************
*! Name         : lfCrtTmp
*! Developer    : Mostafa Eid
*! Date         : 01/20/2009
*! Purpose      : Create Temp file
*!***********************************************************************************
*! Called from  : Soorcndr.prg
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return       : None
*!***********************************************************************************
*! Example      : = lfCrtTmp()
*!***********************************************************************************

FUNCTION lfCrtTmp

lcTargtFl = loogScroll.gftempname()
IF USED(lcTargtFl) AND RECCOUNT(lcTargtFl) > 0
 USE IN (lcTargtFl)
ENDIF

*-- Create File
IF !USED(lcTargtFl)

SELECT ORDLINE
=AFIELDS(laTmpStru)
lnTmpStru = ALEN(laTmpStru,1)

DIMENSION laTmpStru[lnTmpStru + 15,18]

lnI = 0
lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cSzesDesc'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 105
laTmpStru[lnTmpStru + lnI ,4] = 0

*-- Field hold the style group data.

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cSzesVal'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 105
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cSzesQty'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 105
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'nSzesTotal'
laTmpStru[lnTmpStru + lnI ,2] = 'N'
laTmpStru[lnTmpStru + lnI ,3] = 8
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'nSzToVl'
laTmpStru[lnTmpStru + lnI ,2] = 'N'
laTmpStru[lnTmpStru + lnI ,3] = 8
laTmpStru[lnTmpStru + lnI ,4] = 2

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cColorDes'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 50
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cPURDESC'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 30
laTmpStru[lnTmpStru + lnI ,4] = 0


lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cFABCOLOR'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 50
laTmpStru[lnTmpStru + lnI ,4] = 0


lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'lFIRSTREC'
laTmpStru[lnTmpStru + lnI ,2] = 'L'
laTmpStru[lnTmpStru + lnI ,3] = 1
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'StyGrop'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'COLOR'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 30
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'CDIM1'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'INDEXDIM'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 2
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'llPRNTSIZ'
laTmpStru[lnTmpStru + lnI ,2] = 'L'
laTmpStru[lnTmpStru + lnI ,3] = 1
laTmpStru[lnTmpStru + lnI ,4] = 0
**
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'cStock'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 16
    latmpstru[lntmpstru+lni, 4] = 0

**
FOR lnI = 1 TO ALEN(laTmpStru,1)-lnTmpStru
  STORE .F. TO laTmpStru[lnTmpStru+lnI,5],laTmpStru[lnTmpStru+lnI,6]
  STORE ''  TO laTmpStru[lnTmpStru+lnI,7],laTmpStru[lnTmpStru+lnI,8],laTmpStru[lnTmpStru+lnI,9],laTmpStru[lnTmpStru+lnI,10],laTmpStru[lnTmpStru+lnI,11],;
               laTmpStru[lnTmpStru+lnI,12],laTmpStru[lnTmpStru+lnI,13],laTmpStru[lnTmpStru+lnI,14],laTmpStru[lnTmpStru+lnI,15],laTmpStru[lnTmpStru+lnI,16]
  STORE 0  TO laTmpStru[lnTmpStru+lnI,17],laTmpStru[lnTmpStru+lnI,18]
ENDFOR  

=gfCrtTmp(lcTargtFl,@laTmpStru)
ENDIF 
SELECT (lcTargtFl)
IF lcRpSortBy = 'S'
  INDEX ON CORDTYPE + ORDER + STORE + EMPLOYEE + STYLE TAG (lcTargtFl)
  SET ORDER TO TAG (lcTargtFl)
ELSE
  INDEX ON CORDTYPE + ORDER + STORE + EMPLOYEE+ STR(LINENO,6) TAG (lcTargtFl)
  SET ORDER TO TAG (lcTargtFl)
ENDIF

IF USED(lcEmplTmp) AND RECCOUNT(lcEmplTmp) > 0
	 USE IN (lcEmplTmp)
	ENDIF
	*-- Create File
	
	IF !USED(lcEmplTmp)
	  
	  lnI = 1
	  DIMENSION laTempStru9[lnI,4]
	  laTempStru9[lnI,1] = 'ORDER'
	  laTempStru9[lnI,2] = 'C'
	  laTempStru9[lnI,3] = 6
	  laTempStru9[lnI,4] = 0
	  
	  lnI = ALEN(laTempStru9,1)+1
	  DIMENSION laTempStru9[lnI,4]
	  laTempStru9[lnI,1] = 'EMPLCODE '
	  laTempStru9[lnI,2] = 'C'
	  laTempStru9[lnI,3] = 12
	  laTempStru9[lnI,4] = 0
	    
	  
	  lnI = ALEN(laTempStru9,1)+1
	  DIMENSION laTempStru9[lnI,4]
	  laTempStru9[lnI,1] = 'EMPLNAME'
	  laTempStru9[lnI,2] = 'C'
	  laTempStru9[lnI,3] = 30
	  laTempStru9[lnI,4] = 0 
	  
	  lnI = ALEN(laTempStru9,1)+1
	  DIMENSION laTempStru9[lnI,4]
	  laTempStru9[lnI,1] = 'EMPLSITE'
	  laTempStru9[lnI,2] = 'C'
	  laTempStru9[lnI,3] = 10
	  laTempStru9[lnI,4] = 0 


	 =gfCrtTmp(lcEmplTmp,@laTempStru9)
	  SELECT (lcEmplTmp)
	  INDEX ON ORDER+EMPLCODE TAG (lcEmplTmp)
	ENDIF   

*--End of function lfCrtTmp.
*!***********************************************************************************
*! Name         : lfEvalSegs
*! Developer    : Mostafa Eid(MOS)
*! Date         : 01/20/2009
*! Purpose      : Get Color Length and Non major/free Length
*!***********************************************************************************
*! Called from  : Option Grid
*!***********************************************************************************
*! Calls        : ......
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return       : None
*!***********************************************************************************
*! Example      : = lfEvalSegs()
*!***********************************************************************************

FUNCTION lfEvalSegs
PARAMETERS lcReturn
*-- Compute Free/Color Items in Style Structure.
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcNonMajTl = ''
lcNonMajPi = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's.'
lcReturn = .T.
RETURN lcReturn
*-- end of lfEvalSegs.

*!***********************************************************************************
*! Name        : lfGtHedVar
*! Developer   : Mostafa Eid (NNA)
*! Date        : 01/20/2009
*! Purpose     : Function to fill the approparate data for report header.
*!***********************************************************************************
*! Calls       : Procedures : ....
*!               Functions  : lfSolSpAdr
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return      : Null
*!***********************************************************************************
*! Example     : = lfGtHedVar()
*!***********************************************************************************
FUNCTION lfGtHedVar
PARAMETER LCRETURN , LCPARM
lcAlias = ALIAS()    && Save Current alias.
llEndGroup = .F.     && Start of new Group.
= lfGetlSpAdr()      && Call Function that fill header data [SoldTo and ShipTo]
SELECT (lcAlias)     && Restore before function alias.
RETURN ''
*-- end of lfHeadVar.

*!***********************************************************************************
*! Name        : lfGetlSpAdr
*! Developer   : Mostafa Eid (MOS)
*! Date        : 01/20/2009
*! Purpose     : Function to Get the Sold to Address, Ship to Address,
*!             : the Description of the Ship Via, Season,
*!             : Special Instructions, and Terms.
*!***********************************************************************************
*! Called from : lfHeadVar Function
*!***********************************************************************************
*! Calls       : Procedures : ....
*!               Functions  : gfRltFld, gfCodDes, gfGetAdr, lfAdrShift.
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return      : ....
*!***********************************************************************************
FUNCTION lfGetlSpAdr

lnSavAlias = SELECT(0)
lcStore = &lcTargtFl..Store

*= gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
=lfGetCodes('CDIVISION',ORDHDR.cDivision,'DIVLNAME  ','lcDivLName')
lcShipVia = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
lcSeason  = gfCodDes(ORDHDR.Season  , 'SEASON'   )
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')
*Mariam 01/22/2012 Fix error at DCC
*!*	lcusername =  IIF(SEEK(ordhdr.cadd_user,'SYUUSER'),SYUUSER.CUSR_NAME,'')
*!*	lcUserPhone = SYUUSER.CUSR_PHON
lcusername =  IIF(SEEK(ordhdr.cadd_user,'SYUUSER_A'),SYUUSER_A.CUSR_NAME,'')
lcUserPhone = SYUUSER_A.CUSR_PHON
*Mariam 01/22/2012 Fix error at DCC
SELECT CUSTOMER
IF ORDHDR.MULTI = 'Y'
  = SEEK('S' + &lcTargtFl..Account + &lcTargtFl..Store , "CUSTOMER")
  IF ALLTRIM(ORDHDR.ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia,'SHIPVIA')
  ENDIF  
ENDIF
lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))
lcContactN = ORDHDR.CORDCONTAC
**Mariam 01/22/2012 Fix error at DCC[T20120118.0002]
*lcUserPhon = IIF(SEEK(gcUser_ID,'SYUUSER'),SYUUSER.CUSR_PHON,'')
lcUserPhon = IIF(SEEK(gcUser_ID,'SYUUSER_A'),SYUUSER_A.CUSR_PHON,'')
**Mariam 01/22/2012 Fix error at DCC[T20120118.0002]
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
= lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

*-- IF alternate ship to address
IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcStore = lcStore + '  Dist. Center : ' + Customer.Store
  ENDIF
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

  IF TYPE('lcCurrKey') = 'C'
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF

ENDIF    && End of IF
= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
SELECT (lnSavAlias)
*-- end of lfSolSpAdr.

*!***********************************************************************************
*:* Name       : lfAccmulte
*! Developer   : Mostafa Eid (Mos)
*! Date        : 01/15/2009
*:* Purpose    : When there is more than one order line with same 
*                 style/colour/size/complete date/price it should accumulate into one entry 
*!***********************************************************************************

FUNCTION lfAccmulte
PRIVATE lnSlct
lnSlct = SELECT()
SELECT (lcTempOrd)
lcNewTmp =loogScroll.gfTempName()
COPY STRUCTURE TO (oAriaApplication.DataDir+lcNewTmp)
SELECT 0
USE (oAriaApplication.DataDir+lcNewTmp) EXCL
INDEX ON CORDTYPE + ORDER + STORE + STYLE + DTOS(COMPLETE) + STR(PRICE,12,2) TAG &lcNewTmp 

SELECT (lcTempOrd)
LOCATE
SCAN
  SCATTER MEMVAR MEMO
  IF !SEEK(CORDTYPE+ORDER+STORE+STYLE+DTOS(COMPLETE)+STR(PRICE,12,2),lcNewTmp)
    INSERT INTO (oAriaApplication.DataDir+lcNewTmp) FROM MEMVAR
    *!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][Start]
    IF lcRpBook = 'Y'
       REPLACE QTY1 WITH M.BOOK1 ;
               QTY2 WITH M.BOOK2 ;
               QTY3 WITH M.BOOK3 ;
               QTY4 WITH M.BOOK4 ;
               QTY5 WITH M.BOOK5 ;
               QTY6 WITH M.BOOK6 ;
               QTY7 WITH M.BOOK7 ;
               QTY8 WITH M.BOOK8
    ENDIF        
    *!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][End]            
  ELSE
    SELECT (lcNewTmp)
    *!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][Start]
    IF lcRpBook = 'Y'
      REPLACE QTY1 WITH QTY1 + M.BOOK1 ;
            QTY2 WITH QTY2 + M.BOOK2 ;
            QTY3 WITH QTY3 + M.BOOK3 ;
            QTY4 WITH QTY4 + M.BOOK4 ;
            QTY5 WITH QTY5 + M.BOOK5 ;
            QTY6 WITH QTY6 + M.BOOK6 ;
            QTY7 WITH QTY7 + M.BOOK7 ;
            QTY8 WITH QTY8 + M.BOOK8
    ELSE
    *!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][End]
    REPLACE QTY1 WITH QTY1 + M.QTY1 ;
            QTY2 WITH QTY2 + M.QTY2 ;
            QTY3 WITH QTY3 + M.QTY3 ;
            QTY4 WITH QTY4 + M.QTY4 ;
            QTY5 WITH QTY5 + M.QTY5 ;
            QTY6 WITH QTY6 + M.QTY6 ;
            QTY7 WITH QTY7 + M.QTY7 ;
            QTY8 WITH QTY8 + M.QTY8
    *!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][Start]
    ENDIF         
    *!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][End]
  ENDIF
ENDSCAN

SELECT (lcTempOrd)
ZAP
USE IN &lcNewTmp
APPEND FROM (oAriaApplication.DataDir+lcNewTmp)
ERASE (oAriaApplication.DataDir+lcNewTmp+'.DBF')
ERASE (oAriaApplication.DataDir+lcNewTmp+'.CDX')
ERASE (oAriaApplication.DataDir+lcNewTmp+'.FPT')

SELECT (lnSlct)
*-- end of lfAccmulte.

*!***********************************************************************************
*! Name        : lfColData
*! Developer   : Mostafa Eid (MOS)
*! Date        : 01/21/2009
*! Purpose     : Collecting Data
*!***********************************************************************************
*! Called from : Soorcndr.Prg
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return      : ....
*!***********************************************************************************

FUNCTION lfColData

PRIVATE lcEmplCode,lcEmplName,lcEmplSite,lnOldAlias,lcColorDsc,lcDimDesc,lcStyle,lcClrHld ,lcColorMaj
STORE '' TO lcEmplCode,lcEmplName,lcEmplSite,lcColorDsc,lcDimDesc,lcStyle,lcClrHld
STORE 0 TO lnOldAlias
SELECT (lcTempOrd)
GO TOP
lcPurDesc = ''
SCAN
  IF !(SUBSTR(EVAL(lcTempOrd+'.Style'),lnNonMajSt,lnColorLen)==lcClrHld)
    lcClrHld = SUBSTR(EVAL(lcTempOrd+'.Style'),lnNonMajSt,lnColorLen)
    lcDimDesc = SCALE.CDIM1
  ENDIF
  
  IF !(STYLE.CSTYMAJOR ==lcStyle)
    lcStyle  = STYLE.CSTYMAJOR
    lcDimDesc = SCALE.CDIM1
  ENDIF
  
  IF lnTargtFld=1 
    lcDimDesc = SCALE.CDIM1
  ENDIF

  SCATTER MEMVAR MEMO
  
  =lfGetCodes('COLOR',SUBSTR(EVAL(lcTempOrd+'.Style'),lnNonMajSt,lnColorLen),'CLRLNAME','lcRltColor')
  *=gfRltFld(SUBSTR(EVAL(lcTempOrd+'.Style'),lnNonMajSt,lnColorLen) , @laLngDesc,'COLOR')
  lcColorDsc = gfCodDes(SUBSTR(EVAL(lcTempOrd+'.Style'),lnNonMajSt,lnColorLen), "COLOR")
 
  *--Get the Employee Code , Name and Site Ref.
  
IF !EMPTY(&lcTempOrd..EMPLOYEE)
  
  SELECT CONTACT   
  gfSetOrder('CONTACT')
  lcEmplCode = &lcTempOrd..EMPLOYEE
  IF gfSEEK('C'+ PADR(&lcTempOrd..ACCOUNT,8)+&lcTempOrd..STORE,'CONTACT')
     *!B608953,1 MMT 07/30/2009 Fix bug of error while preview [Start]
*!*	     LOCATE  REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT  = 'C'+ PADR(&lcTempOrd..ACCOUNT,8)+&lcTempOrd..STORE ;
*!*	   				   FOR CCONTCODE = &lcTempOrd..EMPLOYEE
     LOCATE  REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT  = 'C'+ PADR(&lcTempOrd..ACCOUNT,8)+&lcTempOrd..STORE ;
   				   FOR CCNTCTCODE= &lcTempOrd..EMPLOYEE
	 *!B608953,1 MMT 07/30/2009 Fix bug of error while preview [End]   				   
     IF  FOUND()   				   
       lcEmplName = CONTACT
       lcEmplSite = SITE_NO
     ELSE 
       STORE '' TO lcEmplName,lcEmplSite 
     ENDIF 
  
  ELSE
    STORE '' TO lcEmplName,lcEmplSite
  ENDIF

  lnOldAlias = SELECT(0)
  IF !SEEK(&lcTempOrd..ORDER+&lcTempOrd..EMPLOYEE,lcEmplTmp)
    SELECT(lcEmplTmp)
    APPEND BLANK
    REPLACE ORDER     WITH &lcTempOrd..ORDER		,;
            EMPLCODE  WITH lcEmplCode 				,;
            EMPLNAME  WITH lcEmplName 				,;
            EMPLSITE  WITH lcEmplSite 
  ENDIF
  SELECT(lnOldAlias)
ENDIF 

 
  *--If the complete date is different then add a new line.
  IF lcOrdKey # EVAL(lcIndexKey) OR lcCompDat # EVAL(lcTempOrd+'.COMPLETE')
    STORE 1 TO lnTargtFld
    SELECT (lcTargtFl)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cColorDes WITH lcRltColor 			,;
            cPURDESC  WITH lcPurDesc  			,;
            cFABCOLOR WITH lcFabColor 			,;
            lFIRSTREC WITH .T.        			,;
            llPRNTSIZ WITH .T.        			,;
            StyGrop   WITH STYLE.CSTYGROUP		,;
            COLOR     WITH lcColorDsc			,;
            CDIM1     WITH lcDimDesc	
            
    lnRecNo = RECNO()
    LOCATE										&& Refresh the relation
    GOTO lnRecNo
  ENDIF

  FOR lnCount = 1 TO SCALE.CNT
    *!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][Start]
    IF lcRpBook = 'Y'
      lcSzQty = '.BOOK' + ALLTRIM(STR(lnCOUNT))    
    ELSE
    *!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][End]
      lcSzQty = '.QTY' + ALLTRIM(STR(lnCOUNT))
    *!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][Start]
    ENDIF
    *!B611067,1 MMT 10/21/2015 Custom order confirmation form DR is not using Print Booked qty option[T20151015.0001][End]
    lcSzDes = '.SZ' + ALLTRIM(STR(lnCOUNT))
    IF lnTargtFld > lnMaxSize OR !(SCALE.CDIM1==lcDimDesc)
      IF lnTargtFld=1 OR !(SCALE.CDIM1==lcDimDesc)
        lcDimDesc = SCALE.CDIM1
      ENDIF
      STORE 1 TO lnTargtFld
      SELECT (lcTargtFl)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE cColorDes WITH lcRltColor 		,;
              cPURDESC  WITH lcPurDesc  		,;
              cFABCOLOR WITH lcFabColor 		,;
              lFIRSTREC WITH .F.        		,;
              llPRNTSIZ WITH .T.       			,;
              StyGrop   WITH STYLE.CSTYGROUP	,;
              COLOR     WITH lcColorDsc			,;
              CDIM1     WITH lcDimDesc

      lnRecNo = RECNO()
      LOCATE
      GOTO lnRecNo
    ENDIF
    lnSzQty = EVAL(lcTempOrd+lcSzQty)
    IF lnSzQty > 0
      SELECT (lcTargtFl)
      REPLACE cSzesQty   WITH RTRIM(cSzesQty)  + STR(lnSzQty,5)     ,;
              cSzesDesc  WITH RTRIM(cSzesDesc) + PADL(ALLTRIM(EVAL('SCALE'+lcSzDes)),5,' ')  ,;
              cSzesVal   WITH RTRIM(cSzesVal)  + STR(ROUND(EVAL(lcTempOrd+'.PRICE'),2),6,2),;
              nSzesTotal WITH nSzesTotal + lnSzQty,;
              nSzToVl    WITH nSzToVl    + (lnSzQty * EVAL(lcTempOrd+'.PRICE'))
**
          lcavail = '0'
          lcindex = STR(lncount, 1)
          IF lnszqty>&lctempord..pik&lcindex
             lcavail=IIF(STYLE.stk&lcindex-STYLE.ord&lcindex<0,'1','0') 
          ENDIF
          REPLACE cstock WITH RTRIM(cstock)+lcavail
**              
      lnTargtFld = lnTargtFld +1
    ENDIF
  ENDFOR
  SELECT (lcTempOrd)
  STORE '' TO lcFabColor,lcPurDesc
  lcOrdKey = EVAL(lcIndexKey)

  *--Fill the complete variable with it's value.
  lcCompDat = EVAL(lcTempOrd+'.COMPLETE')
ENDSCAN

*--End of Function lfColData.

*!*************************************************************
*! Name      : lfGetCodes
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : gfRltFld(), gfCodDes(), gfGetAdr(), lfShiftArr()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetCodes()
*!*************************************************************
FUNCTION lfGetCodes
PARAMETERS lcFld,lcValue,lcFldName,lcReturn
lcAlias = ALIAS()
SELECT Codes
SET ORDER TO TAG Codes
IF SEEK('N'+lcValue+'Y'+lcFld)
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+lcValue+'Y'+lcFld
    IF crltd_nam = lcFldName
      &lcReturn = crltd_vlu
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcAlias)