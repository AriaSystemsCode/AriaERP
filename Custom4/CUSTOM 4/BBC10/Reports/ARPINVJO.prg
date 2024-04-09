*:***************************************************************************
*: Program file  : ARPINVJO
*: Program desc. : Custom Invoice Form Report
*: For Report    : ....
*: System        : Aria 4XP
*: Module        : Account Receivable (AR)
*: Developer     : Walid Hamed  (WLD)
*: Date          : 05/10/2007
*:***************************************************************************
*: Calls :
*:    Functions  : LFINVCRED,LFVJOMSG,LFINVNOTE,LFCOUNT,LFSOLSHP,LFADRSHIFT,LFGETDATE
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARPINVJO
*:***************************************************************************
*:MOdifcations :
*B608482,1 MMT 03/26/2008 Fix bug of Wrong prinitng of Invoice Total and skipping lines[T20071016.0010]
*B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [T20071016.0009]
*:***************************************************************************
*: This Report Program is due to C200786 ...
*----------------------- Report Code Begin -----------------------------
PRIVATE LCALIAS

LCALIAS = ALIAS()



*B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
lcRpPrSt = IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt)
lcTempHdr = loogscroll.gfTempName()
SELECT INVHDR
DIMENSION laInvhdStr[1,18]
AFIELDS(laInvhdStr)
=gfCrtTmp(lcTempHdr,@laInvhdStr,"INVOICE",lcTempHdr,.F.)

SELECT InvHdr
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
SELECT INVHDR
SET RELATION TO 
LOCATE 
SCAN FOR &lcASExp
  SCATTER MEMO MEMVAR 
  SELECT (lcTempHdr)   
  APPEND BLANK 
  GATHER MEMO MEMVAR 
ENDSCAN 
SELECT (lcTempHdr)  
IF llPrntInst .OR. llRpInvNot
  IF !llIsAparel
    SET RELATION TO '' INTO (lcTmpDbt)
    SELECT (lcTmpDbt)
    SET RELATION TO 
    SET RELATION TO IIF(CFILE_NUM = '3', &lcTempHdr..Invoice, '*') INTO ARINSTMD
    SET RELATION TO IIF(CFILE_NUM = '1', &lcTempHdr..Invoice, '*') INTO INVLINE ADDITIVE
  ELSE
    SELECT (lcTempHdr)  
    SET RELATION TO &lcTempHdr..INVOICE INTO INVLINE ADDITIVE  
  ENDIF 
ELSE
  SELECT (lcTempHdr) 
  SET RELATION TO &lcTempHdr..INVOICE INTO INVLINE ADDITIVE
ENDIF 

SELECT (lcTempHdr) 
SET RELATION TO IIF(EMPTY(Store) OR Store = "********",IIF (EMPTY(dist_ctr),'M' + Account,'S' + Account + dist_ctr),'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO Invoice INTO Invhdr ADDITIVE 

IF !llIsAparel
  SET SKIP TO INVLINE , SPCK_LIN
ENDIF

SELECT (lcTempHdr)
SET RELATION TO 'O' + &lcTempHdr..order INTO Ordhdr ADDITIVE

IF llPrntInst .OR. llRpInvNot
  IF !llIsAparel
    SET SKIP TO (lcTmpDbt) , INVLINE , ARINSTMD
  ENDIF
ELSE
  IF !llIsAparel
    SET SKIP TO INVLINE
  ENDIF
ENDIF

llarpinv  = .F.
*B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]



IF FILE(oAriaApplication.DataDir+LCTYPEJO+'.MEM')
  RESTORE FROM oAriaApplication.DataDir+LCTYPEJO+'.MEM' ADDITIVE
ENDIF
CREATE DBF (oAriaApplication.WorkDir+LCINVNOTE) ( INVNOTE M ( 10 ) )
IF  .NOT. USED(LCCREDIT)
  =gfOpenTable('Credit','Credit','SH',@LCCREDIT)
ENDIF
IF  .NOT. USED(LCORDLINE)
   =gfOpenTable('Ordline','Ordlinst','SH',@LCORDLINE)
ENDIF
IF  .NOT. USED('NEWORD')
  SELECT 0
    =gfOpenTable('ORDLINE','ORDLINE','SH','NEWORD')
ENDIF
SELECT INVLINE
SET RELATION TO 'O'+ORDER+STORE+STYLE+STR(LINENO,6) INTO &lcOrdLine ADDITIVE
_PADVANCE = 'LINEFEED'

*B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
SELECT (lcTempHdr) 
LOCATE 
DO gfDispRe WITH EVAL('lcFormName') &&, 'FOR ' + lcRpExp
lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
*B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]

SELECT (lcTempHdr) 
SET RELATION TO 

SELECT (LCALIAS)
  *!**************************************************************************
  *! Name      : LFINVCRED
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 05/10/2007
  *! Purpose   : Get Credit Amount
  *!**************************************************************************
FUNCTION  LFINVCRED
  PARAMETER LCDUMDY
  PRIVATE LCALIAS
  LCALIAS = ALIAS()
  LNMISC = 0
  SELECT (LCCREDIT)
  SCAN FOR ACCOUNT+TRAN+DTOS(TRANDATE)=''
    *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
    *IF LEFT(&lcCredit..REFERENCE,6) = InvHdr.ORDER
    IF LEFT(&lcCredit..REFERENCE,6) = &lcTempHdr..ORDER
    *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
      lnMisc = lnMisc + &lcCredit..Amount
    ENDIF
  ENDSCAN
  SELECT (LCALIAS)
  RETURN LCDUMDY
ENDFUNC
  *!**************************************************************************
  *! Name      : LFVJOMSG
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 05/10/2007
  *! Purpose   : Get Optional message
  *!**************************************************************************
FUNCTION  LFVJOMSG
  PARAMETER LCRETURN
  PRIVATE LAJOMSG
  DIMENSION LAJOMSG[ 4, 2]
  LAJOMSG[ 1, 1] = 'lcJOMes1'
  LAJOMSG[ 2, 1] = 'lcJOMes2'
  LAJOMSG[ 3, 1] = 'lcJOMes3'
  LAJOMSG[ 4, 1] = 'lcJOMes4'
  LAJOMSG[ 1, 2] = 60
  IF FILE(oAriaApplication.DataDir+LCTYPEJO+'.MEM')
    RESTORE FROM oAriaApplication.DataDir+LCTYPEJO+'.MEM' ADDITIVE
  ENDIF
  LCRETURN = GFOPTMSG('laJoMsg','Optional message')
  SET MEMOWIDTH TO 75
  SAVE TO oAriaApplication.DataDir+LCTYPEJO+'.MEM' ALL LIKE lcJoMes*
  RETURN LCRETURN

ENDFUNC
  *!**************************************************************************
  *! Name      : LFINVNOTE
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 05/10/2007
  *! Purpose   : Get Invoice Note
  *!**************************************************************************
FUNCTION  LFINVNOTE
  PARAMETER LCDUMDY
  PRIVATE LCALIAS, LNOLDMEMW
  LCALIAS = ALIAS()
  STORE '' TO LCINVNOTJ1, LCINVNOTJ2, LCINVNOTJ3, LCINVNOTJ4
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
  *!*	  SELECT INVHDR
  *!*	  IF gfSEEK('C'+INVHDR.INVOICE,'Notepad')
  SELECT (lcTempHdr)
  IF gfSEEK('C'+&lcTempHdr..INVOICE,'Notepad')
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
    LNOLDMEMW = SET('MEMOWIDTH')
    SET MEMOWIDTH TO 20
    LNMEMLINS = MEMLINES(NOTEPAD.MNOTES)
    
    *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
    *IF LCINVOICE#INVHDR.INVOICE
    IF LCINVOICE# &lcTempHdr..INVOICE
    *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
    
      LNCOUNTJO = 0
    ENDIF
    IF EVALUATE(LCTMPDBT+'.cfile_num')='2'
      SELECT (LCINVNOTE)
      APPEND BLANK
      FOR LNI = LNCOUNTJO TO LNMEMLINS
        REPLACE INVNOTE WITH ALLTRIM(MLINE(NOTEPAD.MNOTES,LNI))+' ' ADDITIVE
      ENDFOR
      LCINVNOTJ1 = ALLTRIM(EVALUATE(LCINVNOTE+'.InvNote'))
    ELSE
      LNCOUNTJO = LNCOUNTJO+1
      LCINVNOTJ1 = ALLTRIM(MLINE(NOTEPAD.MNOTES,LNCOUNTJO))
      LNCOUNTJO = LNCOUNTJO+1
      LCINVNOTJ2 = ALLTRIM(MLINE(NOTEPAD.MNOTES,LNCOUNTJO))
      LNCOUNTJO = LNCOUNTJO+1
    ENDIF
    IF  .NOT. EOF('SPCK_LIN')
      LCINVNOTJ3 = ALLTRIM(MLINE(NOTEPAD.MNOTES,LNCOUNTJO))
      LNCOUNTJO = LNCOUNTJO+1
      LCINVNOTJ4 = ALLTRIM(MLINE(NOTEPAD.MNOTES,LNCOUNTJO))
      LNCOUNTJO = LNCOUNTJO+1
    ENDIF
    
    *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
    *LCINVOICE = INVHDR.INVOICE
    LCINVOICE = &lcTempHdr..INVOICE
    *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
    
    SET MEMOWIDTH TO LNOLDMEMW
  ENDIF
  SELECT (LCALIAS)
  RETURN ALLTRIM(LCINVNOTJ1)+ALLTRIM(LCINVNOTJ2)+ALLTRIM(LCINVNOTJ3)+ALLTRIM(LCINVNOTJ4)
ENDFUNC
  *!**************************************************************************
  *! Name      : LFCOUNT
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 05/10/2007
  *! Purpose   : Get Invoice count
  *!**************************************************************************
FUNCTION  LFCOUNT
  PARAMETER LCDUMDY
  LNCOUNTJO = 0
  
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
  *IF LCINVOICE#INVHDR.INVOICE
  IF LCINVOICE # &lcTempHdr..INVOICE
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
  
    LCMDATE = SUBSTR(DTOS(EVALUATE(LCORDLINE+'.START')),5,8)+SUBSTR(DTOS(EVALUATE(LCORDLINE+'.COMPLETE')),5,8)
  ENDIF
  
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
  *LCINVOICE = INVHDR.INVOICE
  LCINVOICE = &lcTempHdr..INVOICE
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
  
  RETURN LCDUMDY
ENDFUNC
  *!**************************************************************************
  *! Name      : LFSOLSHP
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 05/10/2007
  *! Purpose   : Get Ship To address
  *!**************************************************************************
FUNCTION  LFSOLSHP
  PARAMETER LCSOLSHP
*B:608482,1 MMT 03/26/2008 Fix bug of Wrong prinitng of Invoice Total and skipping lines[Start]
*!*	  PRIVATE LNCUSRECNO
*!*	  DIMENSION LASOLDTO[ 5, 1], LASHIPTO[ 5, 1], LADIVLNAME[ 1, 2]
*!*	  LASOLDTO = ''
*!*	  LASHIPTO = ''
*!*	  LCDIVLNAME = ''
*!*	  LADIVLNAME[ 1, 1] = 'DIVLNAME'
*!*	  LADIVLNAME[ 1, 2] = 'lcDivLName'
*!*	  LCSOLTNAME = CUSTOMER.BTNAME
*!*	  LASOLDTO[ 1] = GFGETADR('CUSTOMER','','','',1,'2')
*!*	  LASOLDTO[ 2] = GFGETADR('CUSTOMER','','','',2,'2')
*!*	  LASOLDTO[ 3] = GFGETADR('CUSTOMER','','','',3,'2')
*!*	  LASOLDTO[ 4] = GFGETADR('CUSTOMER','','','',4,'2')
*!*	  LASOLDTO[ 5] = GFGETADR('CUSTOMER','','','',5,'2')
*!*	  = LFADRSHIFT('laSoldTo')
*!*	  IF ORDHDR.ALT_SHPTO
*!*	    LCSHPTNAME = ORDHDR.STNAME
*!*	    LASHIPTO[ 1] = ORDHDR.CADDRESS1
*!*	    LASHIPTO[ 2] = ORDHDR.CADDRESS2
*!*	    LASHIPTO[ 3] = ORDHDR.CADDRESS3
*!*	    LASHIPTO[ 4] = ORDHDR.CADDRESS4
*!*	    LASHIPTO[ 5] = ORDHDR.CADDRESS5
*!*	  ELSE
*!*	    LCSHPTNAME = IIF(EMPTY(CUSTOMER.DBA),CUSTOMER.STNAME,CUSTOMER.DBA)
*!*	    LASHIPTO[ 1] = GFGETADR('CUSTOMER','','','',1)
*!*	    LASHIPTO[ 2] = GFGETADR('CUSTOMER','','','',2)
*!*	    LASHIPTO[ 3] = GFGETADR('CUSTOMER','','','',3)
*!*	    LASHIPTO[ 4] = GFGETADR('CUSTOMER','','','',4)
*!*	    LASHIPTO[ 5] = GFGETADR('CUSTOMER','','','',5)
*!*	  ENDIF
*!*	  = LFADRSHIFT('laShipTo')
*!*	  LCTERMS = GFCODDES(INVHDR.CTERMCODE,'CTERMCODE')
*!*	  LCSHIPVIA = GFCODDES(INVHDR.SHIPVIA,'SHIPVIA')
*!*	  
*!*	  lcAlias = ALIAS()
*!*		SELECT Codes
*!*		SET ORDER TO TAG Codes
*!*		IF SEEK('N'+INVHDR.CDIVISION+'Y'+'CDIVISION')
*!*		  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+INVHDR.CDIVISION+'Y'+'CDIVISION'
*!*		    IF crltd_nam = 'DIVLNAME  '
*!*		      lcDivLName = crltd_vlu
*!*		    ENDIF
*!*		  ENDSCAN
*!*		ENDIF
*!*		SELECT (lcAlias)
*!*	*  = GFRLTFLD(INVHDR.CDIVISION,@LADIVLNAME,'CDIVISION')
*!*	   IF oAriaApplication.gcDevice = 'PRINTER'  AND INVHDR.PrtFlag <> 'P'
*!*	      REPLACE PrtFlag WITH 'P' IN INVHDR
*!*	   ENDif
  PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec
  
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
  *lnInvHdRec = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))
  lnInvHdRec = IIF(EOF(lcTempHdr) , 0 , RECNO(lcTempHdr))
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
  
  lnInvLnRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
  lnPakLnRec = IIF(EOF('SPCK_LIN') , 0 , RECNO('SPCK_LIN'))

  IF USED(lcTmpDbt)
    lnTmpDbt = IIF(EOF(lcTmpDbt) , 0 , RECNO(lcTmpDbt))
    lnARINSTMD = IIF(EOF('ARINSTMD') , 0 , RECNO('ARINSTMD'))
  ELSE
    lnTmpDbt   = 0
    lnARINSTMD = 0
  ENDIF
  lnLineRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
  
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
  *!*	  lnHrRc    = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))
  *!*	  COUNT TO lnLines WHILE INVLINE.INVOICE = INVHDR.INVOICE
  lnHrRc    = IIF(EOF(lcTempHdr) , 0 , RECNO(lcTempHdr))
  COUNT TO lnLines WHILE INVLINE.INVOICE = &lcTempHdr..INVOICE
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
  
  IF lnInvLnRec > 0
    GO (lnLineRec) IN INVLINE
  ENDIF
  IF lnHrRc > 0
  
    *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
    *GO (lnHrRc) IN INVHDR
    GO (lnHrRc) IN (lcTempHdr)
    *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
    
  ENDIF

  DECLARE laFactor[5,1]
  STORE '' TO laFactor,lcFacName   

  *-- Fill laFactor with factor address
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
*!*	  IF !EMPTY(INVHDR.CFACCODE)
*!*	    =SEEK(INVHDR.CFACCODE,'SYCFACT')
  IF !EMPTY(&lcTempHdr..CFACCODE)
    =SEEK(&lcTempHdr..CFACCODE,'SYCFACT')
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
  
      lcFacName   = SYCFACT.cfaccomp
      laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
      laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
      laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
      laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
      laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
      =lfAdrShift('laFactor')
  ENDIF

  llEndGroup = .F.
  
  

  *=gfRltFld(INVHDR.cDivision , @laDivLName , 'CDIVISION')
  lcAlias = ALIAS()
  SELECT Codes
  SET ORDER TO TAG Codes
  
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
*!*	  IF SEEK('N'+INVHDR.CDIVISION+'Y'+'CDIVISION')
*!*	    SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+INVHDR.CDIVISION+'Y'+'CDIVISION'
  IF SEEK('N'+&lcTempHdr..CDIVISION+'Y'+'CDIVISION')
    SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+&lcTempHdr..CDIVISION+'Y'+'CDIVISION'
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
  
      IF crltd_nam = 'DIVLNAME  '
        lcDivLName = crltd_vlu
      ENDIF
    ENDSCAN
  ENDIF
  SELECT (lcAlias)
*!*    IF oAriaApplication.gcDevice = 'PRINTER'  AND INVHDR.PrtFlag <> 'P'
*!*      REPLACE PrtFlag WITH 'P' IN INVHDR
*!*    ENDif


  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
*!*	  lcShipVia = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
*!*	  lcTerms = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')
  lcShipVia = gfCodDes(&lcTempHdr..ShipVia , 'SHIPVIA')
  lcTerms = gfCodDes(&lcTempHdr..cTermCode , 'CTERMCODE')
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]

  lcSolTName = CUSTOMER.BTName

  laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

  =lfAdrShift('laSoldTo')

  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
  *SELECT INVHDR
  SELECT (lcTempHdr)
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
  
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GOTO RECNO()
  ENDIF
  SELECT CUSTOMER

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

    *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
*!*	    lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
*!*	                  IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

*!*	    laShipTo[1] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
*!*	    laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
*!*	    laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
*!*	    laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
*!*	    laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))
    lcShpTName  = IIF(&lcTempHdr..STORE = "********" , "At Store Level " ,;
                  IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

    laShipTo[1] = IIF(&lcTempHdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
    laShipTo[2] = IIF(&lcTempHdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
    laShipTo[3] = IIF(&lcTempHdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
    laShipTo[4] = IIF(&lcTempHdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
    laShipTo[5] = IIF(&lcTempHdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))
    *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]

    IF lnCUSRec <> 0
      GOTO lnCUSRec IN CUSTOMER
    ENDIF
  ENDIF    && End of IF

  =lfAdrShift('laShipTo')

  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
  *  SELECT INVHDR
  SELECT (lcTempHdr)
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]

  IF lnTmpDbt <> 0
    GO lnTmpDbt IN (lcTmpDbt)
  ENDIF
  IF lnARINSTMD <> 0
    GO lnARINSTMD IN ARINSTMD
  ENDIF

  *-- Restore the old record pointer in INVLINE
  IF lnInvLnRec = 0
    GO BOTTOM IN INVLINE
    IF !EOF('INVLINE')
      SKIP IN INVLINE
    ENDIF
  ELSE
    GO lnInvLnRec IN INVLINE
  ENDIF

  *-- Restore the old record pointer in SPCK_LIN
  IF lnPakLnRec = 0
    GO BOTTOM IN SPCK_LIN
    IF !EOF('SPCK_LIN')
      SKIP IN SPCK_LIN
    ENDIF
  ELSE
    GO lnPakLnRec IN SPCK_LIN
  ENDIF
  *B:608482,1 MMT 03/26/2008 Fix bug of Wrong prinitng of Invoice Total and skipping lines[End]
  RETURN ''
ENDFUNC

  *!**************************************************************************
  *! Name      : LFADRSHIFT
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 05/10/2007
  *! Purpose   : Fill Array name
  *!**************************************************************************
FUNCTION LFADRSHIFT

  PARAMETER LCARRAYNAM
  FOR LNCOUNT = 1 TO 5
    IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND. EMPTY(&lcArrayNam.[lnCount])
      =ADEL(&lcArrayNam , lnCount)
      LNCOUNT = LNCOUNT-1
    ENDIF
  ENDFOR
  FOR LNCOUNT = 1 TO 5
    IF TYPE(LCARRAYNAM+'['+STR(LNCOUNT,1)+']')#'C'
      &lcArrayNam.[lnCount] = ''
    ENDIF
  ENDFOR
ENDFUNC
  *!**************************************************************************
  *! Name      : LFGETDATE
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 05/10/2007
  *! Purpose   : Get Complete date
  *!**************************************************************************
FUNCTION  LFGETDATE
  PARAMETER LDCOMP
  PRIVATE LDDATE
  STORE {} TO LDCOMP
  LCOLDDBF = ALIAS()
  SELECT INVLINE
  LCOLDREC = INVOICE+STR(LINENO,6)
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [Start]
  *SCAN WHILE INVOICE+STR(LINENO,6)=INVHDR.INVOICE
  SCAN WHILE INVOICE+STR(LINENO,6)=&lcTempHdr..INVOICE
  *B608681,1 MMT 09/04/2008 Fix bug of slow printing of invoice Form JO [End]
  
    IF gfSEEK('O'+ORDER+STR(LINENO,6),'NEWORD') .AND. NEWORD.COMPLETE>LDCOMP
      LDCOMP = NEWORD.COMPLETE
    ENDIF
  ENDSCAN
  =gfSEEK(LCOLDREC)
  SELECT &lcOldDbf
  RETURN LDCOMP
ENDFUNC
*====================================================================================================================  *
