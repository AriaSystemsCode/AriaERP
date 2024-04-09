*:************************************************************************
*: Program file  : ARSUMAG.Prg
*: Program desc. : SUMMARY AGING REPORT
*: System        : Aria 4XP 
*: Module        : AR
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*: Date          : 11/22/2006
*: TASK NO       : T20060908.0020
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARSUMAG
*: N000562
*:************************************************************************

IF llOgFltCh
	llDontPrn=.F.
	XAGINGTYPE  = gfGetMemvar('XAGINGTYPE',oAriaApplication.ActiveCompanyID )
	lcPhonPict  = gfPhoneTem()          && Variable to hold the Company Phone Format
	lcseek=' .T. '

	*Check Customer
	lcCurFltr= lfCheckFilter(1, 'CUSTOMER.ACCOUNT')
	llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
	IF llCurFltr   
	  SELECT (lcCurFltr)
	  INDEX ON ACCOUNT TAG (lcCurFltr)
	  lcseek=lcseek+" AND SEEK(CUSTOMER.ACCOUNT ,'"+lcCurFltr+"')"
	ELSE
	  IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
	    USE IN (lcCurFltr)
	  ENDIF
	  lcCurFltr= ''
	ENDIF

	* Check if there is a filter on term code
	lcCurName = lfCheckFilter(3, 'CUSTOMER.CTERMCODE')  
	lcTerm   = loOgScroll.gfTempName()
	llTerm  = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcTerm ,"CTerm")
	IF llTerm  
	  SELECT (lcTerm )
	  INDEX on CTerm TAG (lcTerm )
	  lcseek=lcseek+" AND SEEK(CTERMCODE,'"+lcTerm +"')"
	ENDIF

	DO CASE
	  CASE lnRpPrd = 1
	    lcseek= lcseek+ " .AND. " + IIF( XAGINGTYPE = 'D' ,;
	          " (AGE30 > 0 .OR. AGE60 > 0 .OR. AGE90 > 0 .OR. AGE120 > 0 )" ,;
	          " (TERAGE30 > 0 .OR. TERAGE60>0 .OR. TERAGE90>0 .OR. TERAGE120>0 )" )
	  CASE lnRpPrd = 2
	    lcseek= lcseek+ " .AND. " + IIF( XAGINGTYPE = 'D' ,;
	          " (AGE60 > 0 .OR. AGE90 > 0 .OR. AGE120 > 0 )",;
	          " (TERAGE60 > 0 .OR. TERAGE90 > 0 .OR. TERAGE120 > 0 )" )
	  CASE lnRpPrd = 3
	    lcseek= lcseek+ " .AND. " + IIF( XAGINGTYPE = 'D' ,;
	          " (AGE90 > 0 .OR. AGE120 > 0) ",;
	          " (TERAGE90 > 0 .OR. TERAGE120 > 0) ")
	  CASE lnRpPrd = 4
	    lcseek= lcseek+ " .AND. "+IIF( XAGINGTYPE = 'D' ,;
	          " AGE120 > 0 ", "  TERAGE120 > 0 ")
	ENDCASE


	lcseek= lcseek+ " .AND.  !EMPTY(NETBAL) "

	lcWorkfile =loOgScroll.gfTempName()

	=lfBuildTmp()
	 
	SELE CUSTOMER
	=GFSEEK('M')
	SCAN REST WHILE  TYPE='M' FOR &lcseek
		M.ACCOUNT= ACCOUNT
		M.ACC_NAME= SUBSTR(BTNAME,1,30)
		M.TELEPHONE= TRANSFORM(IIF( EMPTY(PHONE2), PHONE1 , PHONE2 ), '@R ' + lcPhonPict)
		M.NETBAL= NETBAL   
		M.CURRENT= IIF(XAGINGTYPE='D' , CURRENT , TERCURRENT ) 
		M.AGE30=IIF(XAGINGTYPE='D' , AGE30   , TERAGE30   )
		IF lnRpPrd >= 2 .OR. lnRpPrd = 0 
		   M.AGE60=IIF(XAGINGTYPE='D' , AGE60   , TERAGE60   ) 
		ENDIF
		IF lnRpPrd >= 3 .OR. lnRpPrd = 0
		   M.AGE90=IIF(XAGINGTYPE='D' , AGE90   , TERAGE90   ) 
		ENDIF
		IF INLIST(lnRpPrd,0,4)
		   M.AGE120= IIF(XAGINGTYPE='D' , AGE120  , TERAGE120  ) 
		ENDIF
		M.OPERCR= OPENCR
    INSERT INTO (lcWorkfile) FROM MEMVAR
	ENDSCAN

	SELECT (lcWorkfile )
	IF !RECCOUNT()>0
	  llDontPrn=.T.
	  *-- Message : There are no records to display...!
	  *--                < Ok > 
	    =gfModalGen('TRM00052B40011','ALERT')
	  RETURN
	ENDIF  
	INDEX ON ACCOUNT TAG (lcWorkfile )
	=lfAdjustCRSettings()
	IF USED(lcWorkfile )
	    USE IN (lcWorkfile )
	ENDIF

	=gfDispRe()
ELSE
  IF llDontPrn
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    =gfDispRe()
  ENDIF  
ENDIF  
*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[4,2]

loOgScroll.lcOGLastForm ='ARSUMAG'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkfile + ".DBF"

  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'SUMMARY AGING REPORT'

loOgScroll.laCRParams[2,1] = 'LCDEC'
loOgScroll.laCRParams[2,2]= IIF(LLRPDEC,'Y','N')

loOgScroll.laCRParams[3,1] = 'LCTOT'
loOgScroll.laCRParams[3,2]= IIF(LLRPTOTAL,'Y','N')

loOgScroll.laCRParams[4,1] = 'LCAGTYPE'
loOgScroll.laCRParams[4,2]= IIF(XAGINGTYPE ='D','Y','N')

*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[10,18] ,laTempCOM[1,18]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempCOM
lcExcStat = SET('EXACT')
SET EXACT ON

laTempStru[1,1] = 'ACCOUNT'
laTempStru[1,2] = 'C'
laTempStru[1,3] = 6
laTempStru[1,4] = 0

laTempStru[2,1] = 'ACC_NAME'
laTempStru[2,2] = 'C'
laTempStru[2,3] = 30
laTempStru[2,4] = 0

laTempStru[3,1] = 'NETBAL'
laTempStru[3,2] = 'N'
laTempStru[3,3] = 13
laTempStru[3,4] = 2


laTempStru[4,1] = 'CURRENT'
laTempStru[4,2] = 'N'
laTempStru[4,3] = 13
laTempStru[4,4] = 2

laTempStru[5,1] = 'AGE30'
laTempStru[5,2] = 'N'
laTempStru[5,3] = 13
laTempStru[5,4] = 2

laTempStru[6,1] = 'AGE60'
laTempStru[6,2] = 'N'
laTempStru[6,3] = 13
laTempStru[6,4] = 2

laTempStru[7,1] = 'AGE90'
laTempStru[7,2] = 'N'
laTempStru[7,3] = 13
laTempStru[7,4] = 2

laTempStru[8,1] = 'AGE120'
laTempStru[8,2] = 'N'
laTempStru[8,3] = 13
laTempStru[8,4] = 2

laTempStru[9,1] = 'OPERCR'
laTempStru[9,2] = 'N'
laTempStru[9,3] = 13
laTempStru[9,4] = 2

laTempStru[10,1] = 'TELEPHONE'
laTempStru[10,2] = 'C'
laTempStru[10,3] = 30
laTempStru[10,4] = 0


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


*!*************************************************************
*! Name      : lfStr2Curs 
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs 
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString

