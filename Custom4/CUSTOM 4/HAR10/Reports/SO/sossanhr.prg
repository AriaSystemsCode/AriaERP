*:************************************************************************
*: Program file  : sossanhr.PRG 
*: Program desc. : CUSTOMER ACTIVITY BY SEASON/STATE for HAR10
*:         System: ARIA 4XP
*: Module        : SALES ORDER (SO)
*:      Developer: Mariam Mazhar{MMT}
*  Date          : 10/18/2010
*: This Report Program is due to ...C201276
*:************************************************************************
*: Calls         : 
*:    Procedures : NONE
*:    Functions  : lfvStates, lfwOldVal, lfStitle, lfwRunGrid, 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Modifications:
*:************************************************************************


IF loogscroll.llOgFltCh
  llDonprnt=.F.

  XZERO       = lcRpNonAc
  lcPhonPict  = gfPhoneTem()                    && Variable to hold the Phone Format
  XC_FILTER = ' .T. '  && customer file filter
  XO_FILTER = ' .T. '  && order header file filter

  DECLARE laSeasons[6]
  STORE '' TO laSeasons
  STORE 0  TO lnSeaPos

  lnSeaPos = ASUBSCRIPT(loogscroll.laOGFxFlt,ASCAN(loogscroll.laOGFxFlt,'ORDHDR.SEASON'),1)
  =GFSUBSTR(loogscroll.laOGFxFlt[lnSeaPos,6],@laSeasons,'|')
  lnSeaLen = ALEN(laSeasons)
  *-- we have to take into acoount the 'all' representation of the season.
  IF lnSeaLen > 5
    WAIT WINDOW " Not Allowed to Select More Than 5 Seasons "
    RETURN
  ENDIF

  lnDatePos  = ASUBSCRIPT(loogscroll.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'ORDHDR.ENTERED'),1)
  LEDATE = SUBSTR(LOOGSCROLL.laOGFxFlt[lnDatePos,6],1,ATC('|',LOOGSCROLL.laOGFxFlt[lnDatePos,6])-1)
  HEDATE = SUBSTR(LOOGSCROLL.laOGFxFlt[lnDatePos,6],  ATC('|',LOOGSCROLL.laOGFxFlt[lnDatePos,6])+1)

  lnDatePos  = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'ORDHDR.COMPLETE'),1)
  LCDATE = SUBSTR(LOOGSCROLL.laOGFxFlt[lnDatePos,6],1,ATC('|',LOOGSCROLL.laOGFxFlt[lnDatePos,6])-1)
  HCDATE = SUBSTR(LOOGSCROLL.laOGFxFlt[lnDatePos,6],  ATC('|',LOOGSCROLL.laOGFxFlt[lnDatePos,6])+1)

  lcPERIODE   = IIF(EMPTY(LEDATE)and EMPTY(HEDATE),'','Entered Period: &LEDATE- &HEDATE')
  lcPERIODC   = IIF(EMPTY(LCDATE)and EMPTY(HCDATE),'','Complete Period: &LCDATE- &HCDATE')



*-- Rep Filter
lcRepFltr= lfCheckFilter(1, 'ORDHDR.REP1')
llRepFltr   = !EMPTY(lcRepFltr) AND USED(lcRepFltr) AND RECCOUNT(lcRepFltr) > 0
IF llRepFltr   
  SELECT (lcRepFltr)
  INDEX ON RepCode   TAG (lcRepFltr)
  XO_FILTER =XO_FILTER +" AND SEEK(REP1,'"+lcRepFltr+"')"
ELSE
  IF TYPE("lcRepFltr") = "C" AND USED(lcRepFltr)
    USE IN (lcRepFltr)
  ENDIF
  lcRepFltr= ''
ENDIF

* Check if there is a filter on Style CDIVISION
lcCurName = lfCheckFilter(1, 'ORDHDR.CDIVISION')  
lcDiv   = loOgScroll.gfTempName()
llDiv   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcDiv ,"CDivision")
IF llDiv   
  SELECT (lcDiv)
  INDEX on CDivision TAG (lcDiv)
  XO_FILTER =XO_FILTER +" AND SEEK(CDIVISION,'"+lcDiv+"')"
ENDIF

* Check if there is a filter on Style SEASON
lcCurName = lfCheckFilter(1, 'ORDHDR.SEASON')   
lcSea  = loOgScroll.gfTempName()
llSea   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSea  ,"SEASON") 
IF llSea   
  SELECT (lcSea)
  INDEX on SEASON TAG (lcSea)
  XO_FILTER =XO_FILTER +" AND(SEEK(SEASON,'"+lcSea+"') OR SEASON='*')"
ENDIF

*ENTERED Date Filter
IF !EMPTY(LEDATE)
  XO_FILTER =XO_FILTER +" AND  BETWEEN(ENTERED,CTOD('"+LEDATE +"'),CTOD('"+HEDATE+"')) "
ELSE
  IF  !EMPTY(HEDATE)
    XO_FILTER =XO_FILTER +" AND  ENTERED<=CTOD('"+HEDATE+"') "
  ENDIF
ENDIF

*COMPLE Date Filter
IF !EMPTY(LCDATE)
  XO_FILTER =XO_FILTER +" AND  BETWEEN(COMPLETE,CTOD('"+LCDATE +"'),CTOD('"+HCDATE+"')) "
ELSE
  IF  !EMPTY(HCDATE)
    XO_FILTER =XO_FILTER +" AND  COMPLETE<=CTOD('"+HCDATE+"') "
  ENDIF
ENDIF
*-- setup the order filter
IF !EMPTY(lcRpEdiFlt)
  XO_FILTER = XO_FILTER + [ AND ]+ lcRpEdiFlt
ENDIF

* Check if there is a filter on REGION
lcCurName = lfCheckFilter(1, 'CUSTOMER.REGION')  
lcREGION    = loOgScroll.gfTempName()
llREGION    = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcREGION ,"REGION")
IF llREGION    
  SELECT (lcREGION )
  INDEX on REGION TAG (lcREGION )
  XC_FILTER =XC_FILTER +" AND SEEK(ALLTRIM(REGION) ,'"+lcREGION +"')"
ENDIF

* Check if there is a filter on STATE
lcCurName = lfCheckFilter(1, 'CUSTOMER.CADDRESS4')  
lcSTATE    = loOgScroll.gfTempName()
llSTATE    = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSTATE    ,"STATE")
IF llSTATE    
  SELECT (lcSTATE    )
  INDEX on STATE TAG (lcSTATE    )
  XC_FILTER =XC_FILTER +" AND !EMPTY(CADDRESS4) AND SEEK(ALLTRIM(CADDRESS4),'"+lcSTATE +"')"
ENDIF


STORE '' TO  SES1, SES2, SES3, SES4, SES5

FOR X = 1 TO ALEN(laSeasons)
 IF !EMPTY(laSeasons[X])
   Z=STR(X,1)
   SES&Z=gfCodDes(laSeasons[X]  , 'SEASON'   )
 ENDIF
ENDFOR  


LCWORKFILE =loOgScroll.gfTempName()
=lfBuildTmp()

SELECT CUSTOMER
=SEEK('M')
SCAN WHILE TYPE+ACCOUNT='M' FOR &XC_FILTER
  lcAcc=ACCOUNT
  WAIT WINDOW "Collecting Data for Customer " +lcAcc NOWAIT 
  M.ACCOUNT=ACCOUNT
  M.CUSTOMER=LEFT(BTNAME,25)  
  M.STATE=CADDRESS4
  SELECT ordhdr
  IF SEEK(lcAcc)
    STORE 0 TO  M.B_UNT_A, M.B_AMT_A 
    FOR I=1 TO 5
      Z=STR(I,1)
      STORE 0 TO M.B_UNT_&Z, M.B_AMT_&Z 
    ENDFOR
    llNZero=.F.
    lnUnts=0
    SCAN WHILE ACCOUNT=lcAcc FOR &XO_FILTER   .AND. ( STATUS <> 'X' .OR. BULK = 'N')
      llNZero=.T.
      IF SEASON = '*'
        M.B_UNT_A = M.B_UNT_A + BOOK
        M.B_AMT_A = M.B_AMT_A + BOOKAMT
        lnUnts=lnUnts+ M.B_UNT_A
      ENDIF
      FOR X = 1 TO ALEN(laSeasons)
        IF laSeasons(X) = SEASON
          Z=STR(X,1)
          M.B_UNT_&Z = M.B_UNT_&Z + BOOK
          M.B_AMT_&Z = M.B_AMT_&Z + BOOKAMT
          lnUnts=lnUnts+M.B_UNT_&Z
        ENDIF
      ENDFOR  
    ENDSCAN 
    IF XZERO = 'Y' OR lnUnts <>0
      M.CUSTOMER=LEFT(CUSTOMER.BTNAME,30)
      M.STATE=LEFT(ALLTRIM(CUSTOMER.CADDRESS4),3)
      M.TEL=TRANSFORM(CUSTOMER.PHONE1,'@R '+ lcPhonPict)
      m.City = SUBSTR(CUSTOMER.CADDRESS3,1,15)
      INSERT INTO (LCWORKFILE)  FROM MEMVAR
    ENDIF  
  ENDIF 
ENDSCAN

WAIT CLEAr
SELECT (lcWorkfile )
IF !RECCOUNT()>0
  llDonprnt=.T.
  *-- Message : There are no records to display...!
  *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF  

INDEX ON STATE+ACCOUNT TAG (lcWorkfile )

=lfAdjustCRSettings()

IF USED(lcWorkfile)
  USE IN (lcWorkfile)
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
*! Name      : lfwOldVal
*! Developer : Mariam Mazhar{MMT}
*! Date      : 10/18/2010
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfStitle
*! Developer : Mariam Mazhar{MMT}
*! Date      : 10/18/2010
*! Purpose   : To get the Title for the STATE ,ZIP
*!             according to its country
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStitle()
*!*************************************************************
FUNCTION lfStitle

IF !USED('SYCCOMP')
  = gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'Ccomp_id','SH')
ENDIF
SET ORDER TO Ccomp_id IN SYCCOMP   && To use it to get state title.
IF !USED('SYCINT')
  = gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'Ccontcode','SH')
ELSE
  SET ORDER TO Ccontcode IN SYCINT   && To use it to get state title.
ENDIF
= SEEK(oAriaApplication.ActiveCompanyID   ,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')
lcZipTitle = SYCINT.CPART5LAB
RETURN (SYCINT.CPART4LAB)
*-- end of lfStitle.

*!**************************************************************************
*! Name      : lfwRunGrid
*! Developer : Mariam Mazhar{MMT}
*! Date      : 10/18/2010
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
FUNCTION lfwRunGrid



*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Mariam Mazhar{MMT}
*! Date      : 10/18/2010
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ oAriaApplication.CompanyInstalledModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
*-- end of lfvEdiOrd.



*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Mariam Mazhar{MMT}
*! Date      : 10/18/2010
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[9,2]

loOgScroll.lcOGLastForm ='SOSSANHR'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkfile + ".DBF"

loOgScroll.laCRParams[1,1] = 'SES1'
loOgScroll.laCRParams[1,2]= SES1

loOgScroll.laCRParams[2,1] = 'SES2'
loOgScroll.laCRParams[2,2]= SES2

loOgScroll.laCRParams[3,1] = 'SES3'
loOgScroll.laCRParams[3,2]= SES3

loOgScroll.laCRParams[4,1] = 'SES4'
loOgScroll.laCRParams[4,2]= SES4

loOgScroll.laCRParams[5,1] = 'SES5'
loOgScroll.laCRParams[5,2]= SES5
  
loOgScroll.laCRParams[6,1] = 'ReportName'
loOgScroll.laCRParams[6,2]= 'CUSTOMER ACTIVITY BY SEASON/STATE'


loOgScroll.laCRParams[7,1] = 'lcPERIODE'
loOgScroll.laCRParams[7,2] = lcPERIODE   

loOgScroll.laCRParams[8,1] = 'lcPERIODC'
loOgScroll.laCRParams[8,2] = lcPERIODC  

loOgScroll.laCRParams[9,1] = 'llRpCity'
loOgScroll.laCRParams[9,2] = IIF(llRpCity,1,0)


*************************************************************
*! Name      : lfBuildTmp
*! Developer : Mariam Mazhar{MMT}
*! Date      : 10/18/2010
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[17,18] 
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru
lcExcStat = SET('EXACT')
SET EXACT ON

***  FOR BOOK
lnIndex=1
laTempStru[lnIndex,1] = 'B_AMT_A'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 13
laTempStru[lnIndex,4] = 2
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'B_AMT_1'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 13
laTempStru[lnIndex,4] = 2
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'B_AMT_3'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 13
laTempStru[lnIndex,4] = 2
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'B_AMT_4'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 13
laTempStru[lnIndex,4] = 2
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'B_AMT_5'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 13
laTempStru[lnIndex,4] = 2
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'B_UNT_A'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'B_UNT_1'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'B_UNT_3'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'B_UNT_4'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'B_UNT_5'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2

**13
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STATE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 3
laTempStru[lnIndex,4] = 0

**14
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ACCOUNT'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 8
laTempStru[lnIndex,4] = 0
**15
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CUSTOMER'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0
**16
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'TEL'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0


lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'B_UNT_2'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2
**46

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'B_AMT_2'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 13
laTempStru[lnIndex,4] = 2

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CITY'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 15
laTempStru[lnIndex,4] = 0


=gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)
SET EXACT &lcExcStat 

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Mariam Mazhar{MMT}
*! Date      : 10/18/2010
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
*! Developer : Mariam Mazhar{MMT}
*! Date      : 10/18/2010
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

