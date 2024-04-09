*:***************************************************************************
*: Program file  : ARPINVRU
*: Program desc. : Custom Invoice Form For Runner (RUN15)
*: For Report    : ....
*: System        : Aria 4XP
*: Module        : Account Receivable (AR)
*: Developer     : Mariam Mazhar  (MMT)
*: Date          : 06/22/2009
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:MOdifcations :
*:***************************************************************************
*: This Report Program is due to C201164(Aria4),C201163(Aria27)... [T20090617.0058]
*----------------------- Report Code Begin -----------------------------

lcRpPrSt = IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt)
SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  llarpinv = .F.
  RETURN .F.
ENDIF
lcTaxRefr = gfGetMemVar('M_TAX_REFE',oAriaApplication.ActiveCompanyID) 
lcPStRefer = gfGetMemVar('M_PST_ID  ',oAriaApplication.ActiveCompanyID) 


lcTmpInvhdr  = gfTempName()
SELECT INVHDR
DIMENSION laInvhdStr[1,18]
lnInvhdrlen = AFIELDS(laInvhdStr)
DIMENSION laInvhdStr[lnInvhdrlen +1,18]
laInvhdStr[ALEN(laInvhdStr,1),1] = 'CCUSTLANG'
laInvhdStr[ALEN(laInvhdStr,1),2] = 'C'
laInvhdStr[ALEN(laInvhdStr,1),3] = 10
laInvhdStr[ALEN(laInvhdStr,1),4] = 0
STORE ' ' TO  laInvhdStr[ALEN(laInvhdStr,1),7],laInvhdStr[ALEN(laInvhdStr,1),8],;
              laInvhdStr[ALEN(laInvhdStr,1),9],laInvhdStr[ALEN(laInvhdStr,1),10],;
              laInvhdStr[ALEN(laInvhdStr,1),11],laInvhdStr[ALEN(laInvhdStr,1),12],;
              laInvhdStr[ALEN(laInvhdStr,1),13],laInvhdStr[ALEN(laInvhdStr,1),14],;
              laInvhdStr[ALEN(laInvhdStr,1),15],laInvhdStr[ALEN(laInvhdStr,1),16]
      STORE 0 TO laInvhdStr[ALEN(laInvhdStr,1),17] ,laInvhdStr[ALEN(laInvhdStr,1),18]
=gfCrtTmp(lcTmpInvhdr  ,@laInvhdStr,"INVOICE",lcTmpInvhdr  ,.F.)


SELECT INVLINE
DIMENSION laInvLStr[1,18]
lnInvLlen = AFIELDS(laInvLStr)
DIMENSION laInvLStr[lnInvLlen + 9,18]
laInvLStr[lnInvLlen +1,1] = 'DESC2'
laInvLStr[lnInvLlen +1,2] = 'C'
laInvLStr[lnInvLlen +1,3] = 30
laInvLStr[lnInvLlen +1,4] = 0

lnInc = 2
FOR lnB = 1 TO 8
  laInvLStr[lnInvLlen +lnInc,1] = 'SIZ'+STR(lnB ,1)
  laInvLStr[lnInvLlen +lnInc,2] = 'C'
  laInvLStr[lnInvLlen +lnInc,3] = 30
  laInvLStr[lnInvLlen +lnInc,4] = 0
  lnInc = lnInc + 1
ENDFOR 

FOR lnA = 1 TO 9
  STORE ' ' TO  laInvLStr[lnInvLlen+LnA,7],laInvLStr[lnInvLlen+LnA,8],;
              	laInvLStr[lnInvLlen+LnA,9],laInvLStr[lnInvLlen+LnA,10],;
	            laInvLStr[lnInvLlen+LnA,11],laInvLStr[lnInvLlen+LnA,12],;
    	        laInvLStr[lnInvLlen+LnA,13],laInvLStr[lnInvLlen+LnA,14],;
        	    laInvLStr[lnInvLlen+LnA,15],laInvLStr[lnInvLlen+LnA,16]
  STORE 0 TO	laInvLStr[lnInvLlen+LnA,17] ,laInvLStr[lnInvLlen+LnA,18]
ENDFOR 
lcTmpInvLine = gfTempName()
=gfCrtTmp(lcTmpInvLine,@laInvLStr,"INVOICE+STR(LINENO,6)",lcTmpInvLine,.F.)

SELECT INVHDR
SET RELATION TO 
LOCATE 
SCAN FOR  &lcRpExp
  SCATTER MEMO MEMVAR 
  IF SEEK(IIF(EMPTY(Store) OR Store = "********",IIF (EMPTY(dist_ctr),'M' + Account,'S' + Account + dist_ctr),'S' + Account + Store),'Customer','Customer')
    m.CCUSTLANG = Customer.CCUSTLANG 
  ENDIF   
  **
  INSERT INTO (lcTmpInvhdr) FROM MEMVAR 
  lcIvoiceNum = invhdr.invoice
  SELECT Invline
  =SEEK(invhdr.invoice)
  SCAN REST WHILE INVOICE+STR(LINENO,6) = lcIvoiceNum 
    =SEEK(Invline.Style,'Style','Style')
    =SEEK('S'+Style.Scale,'SCALE','SCALE')
    FOR lnB = 1 TO 8
      lcB= STR(lnB,1)
      m.SIZ&lcB. = Scale.Sz&lcB.
    ENDFOR    
    m.Desc2 = Style.Desc2
    SCATTER MEMO MEMVAR
    m.Desc1 = Style.Desc 
    INSERT INTO (lcTmpInvLine) FROM MEMVAR  
  ENDSCAN 
ENDSCAN 

SELECT(lcTmpInvhdr)
SET RELATION TO &lcTmpInvhdr..INVOICE INTO Invhdr ADDITIVE
SET RELATION TO 'O' + &lcTmpInvhdr..order INTO Ordhdr ADDITIVE
SET RELATION TO &lcTmpInvhdr..INVOICE INTO (lcTmpInvLine) ADDITIVE  
SET SKIP TO (lcTmpInvLine)

IF !USED('Salesrep')
  =gfOpenTable('Salesrep','Salesrep')
ENDIF 
SELECT (lcTmpInvhdr) 
LOCATE 
DO gfDispRe WITH EVAL('lcFormName') 

lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)

llarpinv = .F.
SELECT (lcTmpInvhdr) 
SET RELATION TO 
*!*************************************************************
*! Name      : lfSolSpAdrRu
*! Developer : Mariam Mazhar  (MMT)
*! Date      :  06/22/2009
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Terms
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfSolSpAdrRu

PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec
lnInvHdRec = IIF(EOF(lcTmpInvhdr) , 0 , RECNO(lcTmpInvhdr))
lnInvLnRec = IIF(EOF(lcTmpInvLine) , 0 , RECNO(lcTmpInvLine))

SELECT (lcTmpInvLine)
COUNT TO lnLines WHILE &lcTmpInvLine..INVOICE = &lcTmpInvhdr..INVOICE


DECLARE laFactor[5,1]
STORE '' TO laFactor,lcFacName   


*-- Fill laFactor with factor address
IF !EMPTY(&lcTmpInvhdr..CFACCODE)
  =SEEK(&lcTmpInvhdr..CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

llEndGroup = .F.
lcShipVia = gfCodDes(&lcTmpInvhdr..ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(&lcTmpInvhdr..cTermCode , 'CTERMCODE')
=SEEK(IIF(EMPTY(&lcTmpInvhdr..Store) OR &lcTmpInvhdr..Store = "********",;
	  IIF (EMPTY(&lcTmpInvhdr..dist_ctr),'M' + &lcTmpInvhdr..Account,;
	  'S' + &lcTmpInvhdr..Account + &lcTmpInvhdr..dist_ctr),'S' + &lcTmpInvhdr..Account + &lcTmpInvhdr..Store),;
	  'Customer','Customer')
	  
lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')


SELECT (lcTmpInvhdr)
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GOTO RECNO()
ENDIF
SELECT CUSTOMER
DIMENSION laShipTo[6]
laShipTo[6] = "Tel:  "+ TRANSFORM(Customer.Phone1, '@R ' + lcPhonPict)
IF ORDHDR.Alt_ShpTo
  lcShpTName  = ORDHDR.STName
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
ELSE    && Else

  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr) AND !ORDHDR.lStrDirct
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF

  lcShpTName  = IIF(&lcTmpInvhdr..STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

  laShipTo[1] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF lnCUSRec <> 0
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
ENDIF    && End of IF

=lfAdrShift('laShipTo')

SELECT (lcTmpInvhdr)


IF lnInvLnRec = 0
  GO BOTTOM IN (lcTmpInvLine)
  IF !EOF(lcTmpInvLine)
    SKIP IN (lcTmpInvLine)
  ENDIF
ELSE
  GO lnInvLnRec IN (lcTmpInvLine)
ENDIF
SELECT (lcTmpInvhdr)
RETURN ''