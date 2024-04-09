*:************************************************************************
*: Program file  : ARCHBCR.Prg
*: Program desc. : CHARGEBACK & CREDIT ON ACCOUNT JOURNAL
*: System        : Aria4XP
*: Module        : AR
*: Developer     : AYMAN MAHMOUD AHMED(AYM)
*: Date          : 06/28/2006
*: REF           : N037679
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfwOGWhen(),
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARCHBCR
*:************************************************************************
*---------------------------------------------------------------------
*  3 = CHARGEBACK         DEBIT
*  5 = OPEN CREDIT        CREDIT
*---------------------------------------------------------------------
*:Modifications :
*:B608537,1 NNA 04/27/2008 [T20080415.0006]Fix bug that The charge back and credit on account 
*:B608537,1 NNA            report shifts the debit adjustment reason and description to the following line.
*:************************************************************************
IF llOgFltCh
llDonprnt=.F.
lcFilter=' .T. '

lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CREDIT.TRANDATE'),1)
LDATA = SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1)
HDATA = SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1)
DO CASE
    CASE EMPTY(LDATA ) .AND. !EMPTY(HDATA )
        lcFilter=lcFilter+" AND TRANDATE <=CTOD('"+HDATA + "')"   
    CASE !EMPTY(LDATA )
        Ldata    = SUBSTR(laOGFxFlt[lnDatePos,6],1,10)
        Hdata    = SUBSTR(laOGFxFlt[lnDatePos,6],12,21)
        lcFilter=lcFilter+" AND BETWEEN(TRANDATE,CTOD('"+LDATA +"'),CTOD('"+HDATA +"'))"  
  ENDCASE
           

* CURRENCY Filter
IF llMultCurr 
  lcCurFltr= lfCheckFilter(1, 'CREDIT.CCURRCODE')
  llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
  IF llCurFltr   
    SELECT (lcCurFltr)
    INDEX ON CCURRCODE TAG (lcCurFltr)
    IF EMPTY(lcFilter)
      lcFilter=" SEEK(CCURRCODE ,'"+lcCurFltr+"')"
    ELSE
      lcFilter=lcFilter+" AND SEEK(CCURRCODE ,'"+lcCurFltr+"')"
    ENDIF  
  ELSE
    IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
      USE IN (lcCurFltr)
    ENDIF
    lcCurFltr= ''
  ENDIF
ENDIF  


*-- Add a filter to get Charge back and Open credit only.
lcRpExp = lcFilter+" .AND. AMOUNT <> 0 .AND. INLIST(TRANTYPE, '3' , '6')"
* ACCOUNTFilter
  lcAccFltr= lfCheckFilter(1, 'CREDIT.ACCOUNT')
  llAccFltr   = !EMPTY(lcAccFltr) AND USED(lcAccFltr) AND RECCOUNT(lcAccFltr) > 0
  IF llAccFltr   
    SELECT (lcAccFltr)
    INDEX ON ACCOUNT TAG (lcAccFltr)
  ELSE
    IF TYPE("lcAccFltr") = "C" AND USED(lcAccFltr)
      USE IN (lcAccFltr)
    ENDIF
    lcCurFltr= ''
  ENDIF
 
lcWorkfile  = gfTempName()
=lfBuildTmp()


DO CASE 
  CASE lcRpCOB= 'B'
     IF llAccFltr   
       = lcScanfil('D',.T.)
       = lcScanfil('C',.T.)
     ELSE
       = lcScanfil('D',.F.)
       = lcScanfil('C',.F.)
     ENDIF
  CASE lcRpCOB $ 'C' &&charge back or both
 
    IF llAccFltr   
       = lcScanfil('D',.T.)
     ELSE
       = lcScanfil('D',.F.)
     ENDIF
  CASE lcRpCOB= 'O'  && open credit only
     IF llAccFltr   
       = lcScanfil('C',.T.)
     ELSE
       = lcScanfil('C',.F.)
     ENDIF
ENDCASE

SELECT (lcWorkfile )
IF !RECCOUNT()>0
  llDonprnt=.T.
  *-- Message : There are no records to display...!
  *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF  
GOTO TOP
lcWorkfile =lcWorkfile 
SELECT (lcWorkfile ) 

DO CASE
  CASE lcrpSRTCD = 'C'
    INDEX ON ACCOUNT+ccurrcode+TRAN TAG &lcWorkfile 
  CASE lcrpSRTCD = 'T'
    INDEX ON ccurrcode+TRAN  TAG &lcWorkfile 
  CASE lcrpSRTCD = 'D'
    INDEX ON DTOS(TRANDATE)+ccurrcode+TRAN  TAG &lcWorkfile 
ENDCASE

=lfAdjustCRSettings()
IF USED(lcWorkfile )
    USE IN (lcWorkfile )
ENDIF
=gfDispRe()
ELSE
  IF llDonprnt
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    =gfDispRe()
  ENDIF  
ENDIF  &&FILTER CHANGE


*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 11/12/1998
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************

FUNCTION lfwOGWhen
IF llMultCurr
  lnCurrPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CREDIT.CCURRCODE'),1)
  laOGFxFlt[lnCurrPos,6] = gcBaseCurr
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
ENDIF




*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[5,2]

loOgScroll.lcOGLastForm ='ARCHBCR'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkfile + ".DBF"
  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'CHARGE BACK & CREDIT ON ACCOUNT JOURNAL'

loOgScroll.laCRParams[2,1] = 'LAYOUT'
loOgScroll.laCRParams[2,2]= IIF(lcRpDS='S','Summary','Detail')

loOgScroll.laCRParams[3,1] = 'SORTBY'
loOgScroll.laCRParams[3,2]= IIF(LCRPSRTCD='C','Customer',IIF(LCRPSRTCD='T','Transaction','Date'))

loOgScroll.laCRParams[4,1] = 'DECIMAL'
loOgScroll.laCRParams[4,2]= IIF(llRpDec,'Y','N')

loOgScroll.laCRParams[5,1] = 'LCSORT'
loOgScroll.laCRParams[5,2]= IIF(LCRPSRTCD='C','A',IIF(LCRPSRTCD='T','T','D'))
*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[11,18] ,laTempCOM[1,18]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempCOM
lcExcStat = SET('EXACT')
SET EXACT ON
SELECT DEBIT
=OGAFIELDS(@laTempCOM)
laTempStru[1,1]  = 'TRAN'
laTempStru[2,1]  = 'ACCOUNT'
laTempStru[3,1]  = 'TRANDATE'
laTempStru[4,1]  = 'TRANCODE'
laTempStru[5,1]  = 'STORE '
laTempStru[6,1]  = 'REFERENCE'
laTempStru[7,1]  = 'AMOUNT'
laTempStru[8,1]  = 'CCURRCODE'
laTempStru[9,1]  = 'DESC'


*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 1 TO 9
  lnFldRow = ASCAN(laTempCOM,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCOM,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempCOM[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempCOM[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempCOM[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)


laTempStru[10,1] = 'CUSTOMER'
laTempStru[10,2] = 'C'
laTempStru[10,3] = 50
laTempStru[10,4] = 0

laTempStru[11,1] = 'TRANDESC'
laTempStru[11,2] = 'C'
laTempStru[11,3] = 30
laTempStru[11,4] = 0

=gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)
SET EXACT &lcExcStat  

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn


*************************************************************
*! Name      : FILTEMP
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 06/25/2006
*! Purpose   : FILL TEMP. FILE
*!*************************************************************
FUNCTION lcScanfil
LPARAMETERS CTYPE,llsek
IF CTYPE='C'
  WAIT WINDOW 'SELECTING OPEN CREDITS ...' NOWAIT
  IF llsek
    SELECT (lcAccFltr )  
    SCAN
      XACCOUNT =ACCOUNT
      SELECT CREDIT
      =SEEK(XACCOUNT)
      SCAN WHILE ACCOUNT=XACCOUNT FOR &lcRpExp
        SCATTER MEMVAR
        =FILTEMP('C')
      ENDSCAN
    ENDSCAN        
  ELSE
    SELECT CREDIT
    SCAN FOR &lcRpExp
      SCATTER MEMVAR
      =FILTEMP('C')
    ENDSCAN
  ENDIF
ELSE
  WAIT WINDOW 'SELECTING CHARGE BACKS ...' NOWAIT
  IF llsek
    SELECT (lcAccFltr )  
    SCAN
      XACCOUNT =ACCOUNT
      SELECT DEBIT
      =SEEK(XACCOUNT)
      SCAN WHILE ACCOUNT=XACCOUNT FOR &lcRpExp
        SCATTER MEMVAR
        =FILTEMP('D')
      ENDSCAN
    ENDSCAN 
  ELSE
    SELECT DEBIT
    SCAN FOR &lcRpExp
       SCATTER MEMVAR
       =FILTEMP('D')
     ENDSCAN
  ENDIF
ENDIF


*************************************************************
*! Name      : FILTEMP
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 06/25/2006
*! Purpose   : FILL TEMP. FILE
*!*************************************************************
FUNCTION FILTEMP
LPARAMETERS CTYPE
IF CTYPE='C'
  M.TRANCODE=M.CCREDITCOD
  *B608537,1 NNA 04/27/2008 (Begin) using m.trancode instead of &lcWorkfile..TRANCODE that not filled yet
  *M.TRANDESC=LEFT(IIF(EMPTY(TRANCODE),"",gfCodDes(&lcWorkfile..TRANCODE,'CCREDITCOD')),14)  
  M.TRANDESC=LEFT(IIF(EMPTY(TRANCODE),"",gfCodDes(m.TRANCODE,'CCREDITCOD')),14)  
  *B608537,1 NNA (END)
ELSE
  *B608537,1 NNA 04/27/2008 (Begin) using m.trancode instead of &lcWorkfile..TRANCODE that not filled yet
  *M.TRANDESC=LEFT(IIF(EMPTY(TRANCODE),"",gfCodDes(&lcWorkfile..TRANCODE,'TRANCODE')),14)  
  M.TRANDESC=LEFT(IIF(EMPTY(TRANCODE),"",gfCodDes(m.TRANCODE,'TRANCODE')),14)  
  *B608537,1 NNA (END)
ENDIF
M.CUSTOMER=IIF( SEEK('M'+M.ACCOUNT,'CUSTOMER'),LEFT(CUSTOMER.BTNAME,26),'')
M.DESC= SUBSTR(DESC,1,16)
M.REFERENCE=LEFT(REFERENCE,18)
INSERT INTO (lcWorkfile) FROM MEMVAR 


