*:***************************************************************************
*: Program file  		    : ARAGING.PRG
*: Program desc. 		    : A/R aging report
*: Module        		    : Account Rec. (AR)
*: Developer     		    : Heba Fathi (HFK)
*: Tracking Job Number 	: #037231
*: Date					        : 12/22/2003
*:***************************************************************************
*: Calls : 
*: Purpose :
*:  Global Functions 	: gfPhoneTem,gfGetAdr,gfDispRe,gfCodDes,gfBrows.
*:***************************************************************************
*: Passed Parameters 		: None
*!***************************************************************************
*! Modification:
*! B123663,1 SMM 07/13/2004 Change gfMover to lfOGMover.
*! B124823,1 HFK 10/13/2004 Comment Validation function for account browse becuase 
*! B124823,1 it is converted to a fixed in-range filter
*! B038983,1 HFK 01/27/2005 modify the G\L Account filter to be compatible with 
*! B038983,1 HFK 01/27/2005 the new In-List browse
*! B607796,1 WAM 10/08/2006 Open rethdr file if not opened from the right path
*! B132639,1 MMT 11/01/2006 Fix bug no record when Gl Accounts Selected
*! B608017,1 AYM 03/25/2007 Fix bug when select by sales rep missed data. T20070313.0015 
*! B608184,1 MMT 07/30/2007 Fix bug of Wrong data when Select 'Show past due only'[T20070313.0015]
*! B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
*! B609005,1 AHS 09/10/2009 Adding new field on the frx for the phone and leave the 1st field for the currency in case 
*!                          of multi currency companies [T20090826.0014]
*! B609545,1 MMT 03/06/2011 Error in AR aging report when user change report format[T20110225.0008]
*! B609596,1 MMT 05/30/2011 Chnage the filteration of Sales rep. to be on INVHDR and RETHDR Fields[T20110518.0007]
*! B609748,1 MMT 11/27/2011 A/R Aging Export to Excel needs to have the customer name[T20111108.0005]
*! B609922,1 MMT 05/17/2012 AR aging report does not  have subtotal for currecny[T20120409.0027]
*! B610065,1 HIA 08/29/2012 AR aging report, AR-CREDIT option problems[T20120814.0002]
*! E303331,1 HIA 12/20/2012 AR aging report, Print Notes for Detail, Summary and Short formates [T20121123.0003]
*! E304097,1 SAH 30/12/2018 modify this program as the Payment Terms code of Customer is exported to  AR aging report excel not the Invoice or Credit Memo related Payment term [T20181126.0003]
*!***************************************************************************
*-- lcStTime   : Variable to hold the start Time
*-- llCurrGrp  : Flag .T. if multi currency and print in foreign and not sort by currency.
*-- lcReplExpr : Replace key field expression instead of use index command on huge file
*-- lcAcctFilt : Variable to hold filter on account
*-- lcSorted   : Printed Sort By expression
*-- lcGroup    : variable hold Short Form groups.
*-- lcInnGrp   :
*-- lcCodeDesc :

#INCLUDE R:\Aria4xp\reports\ar\araging.H
loogScroll.cCROrientation = 'P'

lcStTime   = TIME()
llCurrGrp  = llMultCur AND (lcRpCurr = 'F') AND (lcRpAgSort !='C')
STORE '' TO lcAcctFilt , lcSorted , lcGroup , lcInnGrp , lcCodeDesc
lcReplExpr = lfGetReplc()  && Get Sort by value to update cTempKey with it.
llWorkFile   = .F.
lcSalsFile   = ''
lcFilterExp = loOGScroll.lcRpExp 

* B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[Start]
loOGScroll.llOGFltCh = .T.
* B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[End]

*-- if user change filter criteria then you must collect data again
IF loOGScroll.llOGFltCh

  *! B609596,1 MMT 05/30/2011 Chnage the filteration of Sales rep. to be on INVHDR and RETHDR Fields[Start]
  IF ('RM' $ oAriaApplication.CompanyInstalledModules) AND !USED('RETHDR')
	 =gfOpenTable('RETHDR','RETHDR')
  ENDIF 
  *! B609596,1 MMT 05/30/2011 Chnage the filteration of Sales rep. to be on INVHDR and RETHDR Fields[End]

  lcRpSlsExp   = [.T.]
  *-- Get the sales Rep Position.
  lnRpSlsPos   = lfItmPos("SALESREP.REPCODE")
  IF lnRpSlsPos > 0 .AND. loOGScroll.laOGFxFlt[lnRpSlsPos,7] = 'R'
      lcSalsFile = loOGScroll.laOGFxFlt[lnRpSlsPos,6]
    llWorkFile = !EMPTY(lcSalsFile) AND USED(lcSalsFile) AND RECCOUNT(lcSalsFile) > 0
    IF llWorkFile
      *! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug when select by sales rep missed data ..Begin
      *lcRpSlsExp = [ llWorkFile .AND. (SEEK(InvHdr.Rep1,lcSalsFile) .OR. SEEK(InvHdr.Rep2,lcSalsFile))]
      lcRpSlsExp = [ llWorkFile .AND. SEEK(CUSTOMER.SALESRep,lcSalsFile) ]
       *! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug when select by sales rep missed data ..END
    ENDIF

    *- Cut the lcRpExp
    IF AT('In List',loOGScroll.laOGFxFlt[lnRpSlsPos,5]) <> 0
      lnSlsPost = AT('INLIST(SALESREP.REPCODE',lcFilterExp)
    ENDIF

    IF lnSlsPost > 0
      lnPos1 = AT('AND',SUBSTR(lcFilterExp,lnSlsPost))
      IF lnPos1 > 0
      *! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug when select by sales rep missed data ..Begin
        *lcSlsPost = STRTRAN(lcFilterExp , SUBSTR(lcFilterExp , lnSlsPost , lnPos1-1) , " .T. ")
        lcFilterExp = STRTRAN(lcFilterExp , SUBSTR(lcFilterExp , lnSlsPost , lnPos1-1) , " .T. ")
      *! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug when select by sales rep missed data .. End
      ELSE
       *! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug when select by sales rep missed data ..Begin
       * lcSlsPost = STRTRAN(lcFilterExp , SUBSTR(lcFilterExp , lnSlsPost , LEN(lcFilterExp)) , ' .T. ')
        lcFilterExp = STRTRAN(lcFilterExp , SUBSTR(lcFilterExp , lnSlsPost , LEN(lcFilterExp)) , ' .T. ')
        *! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug when select by sales rep missed data ..End

      ENDIF
    ENDIF
  ENDIF

  *-- HFK, 02/14/2005, Modify Customer payment term code condition
  lcNewExp = ""
  if !EMPTY(lcFilterExp)
    lnOccur= occur(' AND',lcFilterExp)
    IF lnOccur > 0
      for lnCount= 1 to lnOccur+1
       lnStart = iif(lnCount=1,1,atc(' AND',lcFilterExp,lnCount-1)+5)
       lnEnd = iif(lnCount = lnOccur+1,LEN(lcFilterExp)+1,atc(' AND',lcFilterExp,lnCount))
       lnLength = lnend - lnstart
       lcScatteredExp = substr(lcFilterExp,lnStart,lnLength)
       if ATC('CUSTOMER.CTERMCODE',lcScatteredExp)> 0 .and. ATC('INLIST',lcScatteredExp)> 0 
         lcTermExp = substr(lcScatteredExp,atc(',',lcScatteredExp,1)+1,len(lcScatteredExp)-1)
         lcTermexp = strtran(lcTermexp,',',' Or CUSTOMER.CTERMCODE = ')
         lcTermexp = STRTRAN(lcTermexp,')','')
         lcTermexp = '( CUSTOMER.CTERMCODE = '+lcTermexp+')'
       else
         lcTermexp = lcScatteredExp
       endif 
      lcNewExp = iif(empty(lcNEwExp),lcTermexp,lcNewExp+' AND ' +lcTermexp)
      ENDFOR
    ELSE
      IF ATC('CUSTOMER.CTERMCODE',lcFilterExp)> 0 .and. ATC('INLIST',lcFilterExp)> 0 
      lcTermExp = SUBSTR(lcFilterExp,atc(',',lcFilterExp,1)+1,len(lcFilterExp)-1)
      lcTermexp = STRTRAN(lcTermexp,',',' Or CUSTOMER.CTERMCODE = ')
        lcTermexp = STRTRAN(lcTermexp,')','')
        lcTermexp = '( CUSTOMER.CTERMCODE = '+lcTermexp+')'
      ELSE
        lcTermexp = lcFilterExp
      ENDIF  
      lcNewExp = lcTermexp
    ENDIF 
  ENDIF  
  lcFilterExp = lcNewExp
  *! B609922,1 MMT 05/17/2012 AR aging report does not  have subtotal for currecny[Start]
  lnPosCur = ASCAN(loOgScroll.laOgFXFlt,"CUSTOMER.CCURRCODE")
  IF lnPosCur > 0 
    lnPosCur = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosCur,1)
    lcCurrencies= loOgScroll.laOgFxFlt[lnPosCur,6]
    IF !EMPTY(lcCurrencies)
      lnCurStPos = ATC('INLIST(CUSTOMER.CCURRCODE',lcFilterExp)
      IF  lnCurStPos > 0
        lcCurrExp = SUBSTR(lcFilterExp,lnCurStPos)
        lnEndExp  = ATC(')',lcCurrExp)
        lcCurExpr = SUBSTR(lcCurrExp,1,lnEndExp)
        lcFilterExp = STRTRAN(lcFilterExp ,lcCurExpr,' .T. ')
		llCurrSelect = .F.
		lcCurrencyCursor  = ''
        llCurrSelect = .T.
		lcCurrencyCursor  = loOgScroll.gfTempName()
		DIMENSION laTempacstru[1,4]
		laTempacstru[1,1]='CCURRCODE'
		laTempacstru[1,2]='C'
		laTempacstru[1,3]= 6
		laTempacstru[1,4]= 0
		=gfCrtTmp(lcCurrencyCursor,@laTempacstru,"CCURRCODE",lcCurrencyCursor,.T.)
		lnStart=1
		lnEnd=AT('|',lcCurrencies)
		DO WHILE lnEnd <> 0
		  SELECT(lcCurrencyCursor) 
		  APPEND BLANK 
		  REPLACE CCURRCODE WITH SUBSTR(lcCurrencies,lnStart,lnEnd-1)
		  lcCurrencies= STUFF(lcCurrencies,lnStart,lnEnd,"") 
		  lnEnd=AT('|',lcCurrencies)
		ENDDO 
		IF lnEnd = 0
		  SELECT(lcCurrencyCursor) 
		  APPEND BLANK 
	      REPLACE CCURRCODE WITH lcCurrencies
	    ENDIF 
	    lcFilterExp = lcFilterExp +" AND SEEK(CCURRCODE,'"+lcCurrencyCursor+"')"
      ENDIF
    ENDIF
  ENDIF
  *! B609922,1 MMT 05/17/2012 AR aging report does not  have subtotal for currecny[END]
  *-- HFK, 02/14/2005, Modify Customer payment term code condition
  *- B038983,1 HFK 01/27/2005 [Start]
  lcGlCondition = ""
  lcGlCondFile = ASCAN(loOGScroll.laOGFxFlt,'lcRpGlAcc')
  lcGlCondFile = ASUBSCRIPT(loOGScroll.laOGFxFlt,lcGlCondFile,1)

  *B132639,1 MMT 10/01/2006 Fix bug no record when Gl Accounts Selected[Start]
  *lcGlCondFile = loOGScroll.laOGFxFlt[lcGlCondFile,6]
  lcGlCondFileUse = loOGScroll.laOGFxFlt[lcGlCondFile,6]
  IF AT('In List',loOGScroll.laOGFxFlt[lcGlCondFile,5]) <> 0
     lnGLPost = AT('INLIST(lcRpGlAcc',lcFilterExp)
     IF lnGLPost = 0
       lnGLPost = AT('BETWEEN(lcRpGlAcc',lcFilterExp)
     ENDIF 
  ENDIF
  
  IF lnGLPost > 0
      lnPos1 = AT(')',SUBSTR(lcFilterExp,lnGLPost))
      IF lnPos1 > 0
         lcFilterExp = STRTRAN(lcFilterExp , SUBSTR(lcFilterExp , lnGLPost , lnPos1) , " Seek(ALLTRIM(cArGlAcc),'&lcGlCondFileUse')")
         lcFilterExp = STRTRAN(lcFilterExp , 'lcRpGlAcc' , "ALLTRIM(cArGlAcc)")
         
      ELSE
        lcFilterExp  = STRTRAN(lcFilterExp , SUBSTR(lcFilterExp , lnGLPost , LEN(lcFilterExp)) , " Seek(ALLTRIM(cArGlAcc),'&lcGlCondFileUse')")
        lcFilterExp = STRTRAN(lcFilterExp , 'lcRpGlAcc' , "ALLTRIM(cArGlAcc)")
      ENDIF
    ENDIF
  	
  IF !EMPTY(lcGlCondFileUse ) AND USED(lcGlCondFileUse )
    lcRpGlAcc = '.T.'
    lcGlCondition = "Seek(ALLTRIM(cArGlAcc),'&lcGlCondFileUse')"    
	*    lcGlCondition = "InList(ALLTRIM(cArGlAcc),"
	*!*	    SELECT (lcGlCondFile)
	*!*	    SCAN
	*!*	      lcGlCondition = lcGlCondition + "'" + SUBSTR(KeyExp,1,LEN(lcAcMask)) + "'"
	*!*	      lcGlCondition = lcGlCondition + ","
	*!*	    ENDSCAN
	*!*	    lcGlCondition = SUBSTR(lcGlCondition,1,LEN(lcGlCondition)-1)
	*!*	    lcGlCondition = lcGlCondition + ")"      
  ELSE
    lcRpGlAcc = '' 
  ENDIF 
  *B132639,1 MMT 10/01/2006 Fix bug no record when Gl Accounts Selected[End]
  
  *- B038983,1 HFK 01/27/2005 [Start]
  *-- if you have previous data clear work files then recreate it. 
  IF !USED(lcTempAge) OR (RECCOUNT(lcTempAge) > 0)
    =lfWorkFile()
  ENDIF

  *-- llChSortBy : Flag if .T. indicate that user change sort by, but here false it
  *                Avoid Replacing Key field with proper sort by values in if...endif block.
  *                i.e. do not enter reindex block because we do it with data collection.
  llChSortBy = .F.

  =lfCollect()               && Collect Aging data.

ENDIF  && if user change filter criteria then you must collect data again.

*-- Asking if no records (Display message) otherwise print report.
IF RECCOUNT(lcTempAge) = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF


*-- Filter Temporary Account File [Begin]

*! B608184,1 MMT 07/30/2007 Fix bug of Wrong data when Select 'Show past due only'[Start]
*-- if user want to show due past only.
IF llRPShowPa
  lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) + [nAges > 0]
ENDIF  && end if user want to show due past only
*! B608184,1 MMT 07/30/2007 Fix bug of Wrong data when Select 'Show past due only'[ENd]




*-- if user want balance debits only.
IF lcRpBalanc = 'D'
  lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) + [nNetBal >= lnRpBalanc]
ENDIF  && end if user want balance debits only.

*-- if user want balance credits only.
IF lcRpBalanc = 'C'
  lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) +  [nNetBal <= lnRpBalanc]
ENDIF  && end if user want balance credits only.

*-- if user print short form suppress zero lines because this form print totals.
IF lcRpReport = 'H'
  lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) + [nNetBal <> 0]
ENDIF  && end if user print short form suppress zero lines.

*-- if user want Balance greater than in case balance = both only.

IF lcRpBalanc = 'B' AND lnRpBalanc <> 0
  IF lnRpBalanc < 0
    lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) + [nNetBal < lnRpBalanc]
  ELSE
    lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) + [nNetBal > lnRpBalanc]
  ENDIF
ENDIF

*-- if you have totals filter.
IF !EMPTY(lcAcctFilt)
  SELECT (lcTempAcc)
  SET FILTER TO &lcAcctFilt
  GO TOP
  *-- Asking if no records (Display message) otherwise print report [Begin.]
  IF EOF()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
  ENDIF
  *-- Asking if no records (Display message) otherwise print report [End.]
ENDIF  && end if you have totals filter
*-- Filter Temporary Account File [End  ]

*=================================  Begin ===============================*
*==========================  Have data section ==========================*
*========================================================================*
SELECT (lcTempAge)
*-- Reindexing file if user change sort by [Begin.]
IF llChSortBy
  llChSortBy = .F.  && Avoid Replacing Key field with proper sort by values again.
  REPLACE ALL cTempKey WITH EVALUATE(lcReplExpr) && Replace key field with new values.
ENDIF
*-- Reindexing file if user change sort by [End.]

*-- if print short form.
IF lcRpReport = 'H'
  *-- Create Another alias form the same line descending order to print total line
  *-- from withen detail band of form, and this technique have the ability to print
  *-- both transaction lines and post dated cheques from one line in this band, avoiding
  *-- waste of papers.
  IF !USED(lcTempRev)
    DO lpAnothAls
  ENDIF

  *-- Set relation with reverse file.
  SELECT (lcTempAge)
  SET RELATION TO EVALUATE(lcReplExpr) INTO (lcTempRev)

  *-- lcTermData : Printed in short form if line figure transaction totals
  *-- lcPostText : Printed in short form if line figure Post dated totals
  *-- lcCurrSec  : Printed in short form currency section.

  *-- if multi currency company.
  IF llMultCur
    lcCurrSec = [cCurrCode + '-' + IIF(SEEK(cCurrCode,'SycCurr'),PADR(SycCurr.CCURRDESC,9),SPACE(9))]

  ELSE  && else single currency company.
*B609005,1 AHS 09/10/2009 Making the field of phone wider and remove transform [start]
    *lcCurrSec = [PADR(TRANSFORM(IIF(EMPTY(CUSTOMER.PHONE2), ]      +;
                [CUSTOMER.PHONE1,CUSTOMER.PHONE2),gfPhoneTem()),20)]
    lcCurrSec = [IIF(EMPTY(CUSTOMER.PHONE2),CUSTOMER.PHONE1,CUSTOMER.PHONE2)]
*B609005,1 AHS [end]   
  ENDIF && end if multi currency company.

  lcTermData = [IIF(RECNO() <> RECNO(lcTempRev) OR ]                      +;
               [(lnGrpAge=0 AND lnGrpCre=0),'',]                          +;
               [PADR(CUSTOMER.BTNAME,10) + ' ' + ]                        +;
               lcCurrSec + [ + ' ' +]                                     +;
               [Customer.SALESREP + ' ' + ]                               +;
               [PADR(gfCoddes(cTermCode,'CTERMCODE',.T.),25))]

  lcPostText = [IIF(RECNO() <> RECNO(lcTempRev) OR lnGrpPost=0 ,'',]      +;
               [PADR(CUSTOMER.BTNAME,10) + ]                              +;
               [',   Totals Post Dated Cheques = ' + ]                    +;
               [TRANSFORM(lnGrpPost,"99999999.99"))]

ELSE  && else print Detail or Summary forms.

  *-- Prepair to deal with Age Credits (Yes / No)
  llPrnCrdDt = llRpCrdDet AND llRPAgeCrd     && Print credit detail and age credits.

  *-- if user want to print new page per account
  IF llRpAccPag
    IF !USED(lcOGTmpForm)

      *-- To Fix structural CDX not found bug until making global fix [Begin]
      lcSvErrHan = ON('ERROR')
      ON ERROR
      *-- To Fix structural CDX not found bug until making global fix [END  ]
      *-hfk, 03/15/2004, to fix the bug when selecting new page per account[begin]
      *-- USE &oAriaApplication.WorkDir.&lcOGTmpForm..FRX IN 0 ORDER 0 EXCLUSIVE      
      lcPath=oAriaApplication.WorkDir
	    USE &lcPath.&lcOGTmpForm..FRX IN 0 ORDER 0 EXCLUSIVE
      *-hfk, 03/15/2004, to fix the bug when selecting new page per account[end]
      
      *-- To Fix structural CDX not found bug until making global fix [Begin]
      ON ERROR &lcSvErrHan
      *-- To Fix structural CDX not found bug until making global fix [End  ]

    ENDIF

    *-- Replace pagebreak with .T. to print new page per account group.
    SELECT (lcOGTmpForm)
    LOCATE FOR OBJTYPE = 9 AND OBJCODE = 3 AND 'ACCOUNT' $ UPPER(ALLTRIM(EXPR))
    REPLACE PAGEBREAK WITH .T.
    USE IN (lcOGTmpForm)  && Close file.
  ENDIF

ENDIF  && end if print short form.


*-- Set relation between Temporary file and customer files.
SELECT (lcTempAge)
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER ,;
                      ACCOUNT INTO (lcTempAcc) ADDITIVE


*-- Evaluate Aging headers, (Report aging titles). [Begin]
*-- If Age by date
IF lcAgeType = 'D'
  *--lcAgeHd30  = "Over" + ALLTRIM(STR(lnRPDay1))
  *--lcAgeHd60  = "Over" + ALLTRIM(STR(lnRPDay2))
  *--lcAgeHd90  = "Over" + ALLTRIM(STR(lnRPDay3))
  *--lcAgeHd120 = "Over" + ALLTRIM(STR(lnRPDay4))
  lcAgeHd30  = LANG_Araging_Over + ALLTRIM(STR(lnRPDay1))
  lcAgeHd60  = LANG_Araging_Over + ALLTRIM(STR(lnRPDay2))
  lcAgeHd90  = LANG_Araging_Over + ALLTRIM(STR(lnRPDay3))
  lcAgeHd120 = LANG_Araging_Over + ALLTRIM(STR(lnRPDay4))
  

ELSE  && Else Age by terms

  lcAgeHd30  = '1 - ' + ALLTRIM(STR(lnRPDay1))
  lcAgeHd60  = ALLTRIM(STR(lnRPDay1+1)) + ' - ' + ALLTRIM(STR(lnRPDay2))
  lcAgeHd90  = ALLTRIM(STR(lnRPDay2+1)) + ' - ' + ALLTRIM(STR(lnRPDay3))
  *-lcAgeHd120 = 'Over ' + ALLTRIM(STR(lnRPDay3+1))
  lcAgeHd120 = LANG_Araging_Over + ALLTRIM(STR(lnRPDay3+1))

ENDIF  && end If Age by date.
*-- Evaluate Aging headers, (Report aging titles). [End  ]

*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.*--
*--WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcTempAcc))) + ' Customer(s), ' + ALLTRIM(STR(RECCOUNT())) + ' Transaction(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
WAIT WINDOW LANG_Araging_Slctd  + ALLTRIM(STR(RECCOUNT(lcTempAcc))) + LANG_Araging_Custmrs   + ALLTRIM(STR(RECCOUNT())) + LANG_Araging_TranIn   + ALLTRIM(STR(lnInterval,6,2)) + LANG_Araging_Sec  NOWAIT
*========================================================================*
*==========================  Have data section ==========================*
*=================================   End  ===============================*


*-- Print Code

IF lcRpReport = 'H'            && In case the user select the short report.
  REPLACE ALL &lcTempAge..cTranType WITH "" FOR &lcTempAge..cTranType = 'H'
ENDIF

STORE SPACE(0) TO lcAcntPad
llRpInvNot = IIF(TYPE('llRpInvNot') = 'U', .F. , llRpInvNot)

IF llRpInvNot
  SELECT (lcTempAge)
  GO TOP
  SCAN
    IF llRPCusNot AND !EMPTY(lcAcntPad)
      IF lcAcntPad # &lcTempAge..ACCOUNT
        SKIP - 1
        REPLACE &lcTempAge..llFlgPad WITH .T.
        SKIP
      ENDIF
    ENDIF
    lcAcntPad = &lcTempAge..ACCOUNT
    IF &lcTempAge..TRANTYPE = "1" AND  SEEK('C'+ &lcTempAge..TRAN,'NOTEPAD') .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*'
      REPLACE &lcTempAge..mNotes WITH NOTEPAD.mNotes
    ENDIF
  ENDSCAN
  IF EOF(lcTempAge)
    SKIP - 1
    REPLACE &lcTempAge..llFlgPad WITH .T.
  ENDIF
ENDIF
GO TOP  && Refresh relation
cSetCen=SET("Century")
SET CENTURY OFF
IF EMPTY(lcAcctFilt)
  DO gfDispRe WITH EVALUATE('lcRpName')  && No totals filter.
ELSE
  DO gfDispRe WITH EVALUATE('lcRpName'), 'FOR !EOF(lcTempAcc)'  && Have totals filter.
ENDIF
SET CENTURY &cSetCen
WAIT CLEAR

*-- Release relation.
SET RELATION TO

*-- Close reverse file (if print short form only)
IF USED(lcTempRev)
  USE IN (lcTempRev)
ENDIF
*-- end of report code.

*========================================================================*
*=========================  Report Program Code =========================*
*=================================   End  ===============================*


*========================================================================*
*==========================  Functions Section ==========================*
*========================================================================*
*!*************************************************************
*! Name      : lpAnothAls
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Open temporary cursor in another alias with reverse sorting
*!           : which help us detect last line in each group with out using
*!           : .FRX group to gain suppress line if blank which work only in
*!           : .FRX detail band, also print Transaction line and post dated line
*!           : from the same .Frx detail line which save paper space.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
PROCEDURE lpAnothAls
PRIVATE lcFullPath , lcCurDir
lcFullPath = SET('FULLPATH')
SET FULLPATH ON
lcCurDir   = DBF()  && Get Cursor full path.
SET FULLPATH &lcFullPath

USE (lcCurDir) IN 0 AGAIN ALIAS (lcTempRev)  && Use the same file in another alias
SELECT (lcTempRev)
SET ORDER TO TAG (lcTempRev)  && Set Descending order.
*-- end of lpAnothAls.

*!*************************************************************
*! Name        : lfpostChq
*! Developer   : Sameh Saiid (SSE)
*! Date        : 02/22/1999
*! Purpose     : get total POST DATED CHECKS for each ACCOUNT
*! Called From : SHORT report (ARAGINH)----->DOS FRX
*!*************************************************************
*! Example     : lfPostChq()
*!*************************************************************
**no longer used 
FUNCTION lfPostChq

SELECT (lcPrtFile)
lcAccount  = ACCOUNT
SELECT POSTDCHQ
IF SEEK(&lcPrtFile..ACCOUNT)
  SUM amount REST WHILE account = lcAccount TO lnPDChcks
ELSE
  lnPDChcks = 0.00
ENDIF

RETURN lnPDChcks
*!*************************************************************
*! Name      : lfvReport
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the report form to enable
*!             and disable some other fields in the option grid
*!*************************************************************
*! Example     : = lfvReport()
*!*************************************************************
FUNCTION lfvReport
IF lcRpReport = 'H'
  lcRPName   = 'ARAGINH'
  = lfRepPltFr(lcRPName)

  *-- Do not print Page/Account
  *-- Do not print Credit detail
  *-- Do not print Customer Note Pad
  *! E303331,1 HIA 12/20/2012 AR aging report, Print Notes for Detail, Summary and Short formates [T20121123.0003][Begin]  
  *STORE .F. TO llRpAccPag , llRpCrdDet , llRpCusNot
  STORE .F. TO llRpAccPag , llRpCrdDet 
  *! E303331,1 HIA 12/20/2012 AR aging report, Print Notes for Detail, Summary and Short formates [T20121123.0003][End]
ELSE
  *-- if last run form was short Restore defaults.
  IF lcRpName = 'ARAGINH'
    lcRpName = 'ARAGING'
    = lfRepPltFr(lcRPName)
    *-- if detail form do default to print Credit detail, else do not print credit detail.
    STORE (lcRpReport $ 'DS') TO llRpCrdDet , llRpAgeCrd
  ENDIF
ENDIF

ClearRead()

*!*************************************************************
*! Name      : lfwGrid
*! Developer : HEBA FATHI (HFK)
*! Date      : 12/22/2003
*! Purpose   : Called from the when function of the option grid
*!*************************************************************
*! Example     : = lfwGrid()
*!*************************************************************
*!Create arrays hold temporary files structures, then create them.

FUNCTION lfwGrid
IF TYPE('lcBuild') = 'N'
  *-- lcBuild = 'OK'
  lcBuild = LANG_Araging_OK 
  *-- lcPrtFac = 'FACTORED AND NON FACTORED INVOICES'
  lcPrtFac = LANG_Araging_PrtFac 
  =lfFilTrnAr()  && Fill arrays then create files.
ENDIF

*-- Disable Age Credits if user does not want to include credit detail 
= !llRpCrdDet AND lfvIncDet()
*-- Amin
**-*-*-**-*-
lcAcMask   = SPACE(0)         && Account mask
lnAcLen    = 0                && Account Length
lcGlFld = SPACE(0)         && Chart of account browse fields.
lcAcntFld  = SPACE(0)         && Account field that will be validated.
lcAcntDesF = SPACE(0)         && Account Description.

llNoThing  = lfSetGLMsk()


*!*************************************************************
*! Name      : lfvInv
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the option grid to set the variable
*!             of factored invoices
*!*************************************************************
*! Example     : = lfvInv()
*!*************************************************************
FUNCTION lfvInv

lcRPSpeFac = IIF((lcRPFactor = 'F'),lcRPSpeFac,SPACE(05))
ClearRead()
*!*************************************************************
*! Name      : lfvBalance
*! Developer : HEBA FATHI (HFK)
*! Date      : 12/22/2003
*! Purpose   : Called from the option grid to set the variable
*!             of balance
*!*************************************************************
*! Example     : = lfvBalance()
*!*************************************************************
FUNCTION lfvBalance
ClearRead()
DO CASE
  CASE lcRPBalanc = 'C'
    *--lcHedBal  = 'Under'
	lcHedBal = LANG_Araging_Under 
	lnRpBalanc = ABS(lnRPBalanc) * -1
	
	CASE lcRPBalanc = 'D'
    *--lcHedBal  = 'Over'
    lcHedBal  = LANG_Araging_Over 
    lnRpBalanc = ABS(lnRPBalanc)
	    
  CASE lcRPBalanc = 'B'
    *-lcHedBal  = 'Balance Greater than'
	  lcHedBal  = LANG_Araging_BalGThan
    IF lnRpBalanc < 0
      lnRpBalanc = ABS(lnRPBalanc) * -1
    ELSE
      lnRpBalanc = ABS(lnRPBalanc)
    ENDIF
  OTHERWISE
    lcHedBal  = SPACE(10)
    lnRpBalanc = 0
ENDCASE



*!*************************************************************
*! Name      : lfvFactor
*! Developer : HEBA FATHI (HFK)
*! Date      : 12/22/2003
*! Purpose   : to validate a specific factor
*!*************************************************************
*! Example     : = lfvFactor()
*!*************************************************************
FUNCTION lfvFactor
PRIVATE lnAlias,lcObjName , lcObjVal , laRetVal, lcFile_Ttl
lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

=gfOpenFile(oAriaApplication.SysPath+'SycFact',oAriaApplication.SysPath+'Cfaccode','SH')  

IF '?' $lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal,'SycFact'))
  *--lcBrFields  = [cFacCode:H='Factor',cFacComp:H='Factor/Company Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone',cFacTitle :H='Title']
  lcBrFields  = [cFacCode:H=LANG_Araging_Fctr ,cFacComp:H=LANG_Araging_FacCom ,cFacCont:H=LANG_Araging_Contact ,cPhoneNo :P= gfPhoneTem() :H=LANG_Araging_Phone ,cFacTitle :H=LANG_Araging_Title ]
  *--  lcFile_Ttl= 'Factor'
  lcFile_Ttl= LANG_Araging_Fctr 
  SELECT SYCFACT
  LOCATE 
  DECLARE laRetVal[1]
  IF gfBrows('' ,'CFacCode', 'laRetVal',lcFile_Ttl)
    &lcObjName = laRetVal[1]
    lcRpSpeFac= EVALUATE(lcObjName)
  ELSE    && Else
    &lcObjName = lcObjVal
  ENDIF    && End of IF 
ENDIF  
*!*************************************************************
*! Name      : lfvSalRep
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the salesrep field from the option grid
*!*************************************************************
*! Example     : = lfvSalRep()
*!*************************************************************
FUNCTION lfvSalRep
PRIVATE lcObjNam , lcObjVal , llObjRet

SET ORDER TO TAG SALESREP IN SALESREP
lcObjNam = OGSYS18()                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'SALESREP')
  llBrowse = .T.
  xAccount = lcObjVal
  DO REPCHK WITH xAccount
  lcObjVal = xAccount
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal

*!*************************************************************
*! Name      : llPrtSub
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the report the check when to print the subtotals
*!*************************************************************
*! Example     : = llPrtSub()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION llPrtSub

IF EOF()
  llPrintSub = .T.
ENDIF
RETURN SPACE(01)  
*!*************************************************************
*! Name      : lfvDay1
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 1st bucket
*!*************************************************************
*! Example     : = lfvDay1()
*!*************************************************************
 
FUNCTION lfvDay1
IF lnRPDay1 <= 0
  =gfModalGen('TRM40132B40011','ALERT')
  *-lnRPDay1 = lnOldDays
  lnRPDay1 = loOgScroll.ActiveControl.oldValue
ENDIF
IF lnRPDay1 >= lnRPDay2
  =gfModalGen('TRM40133B40011','ALERT','2nd|1st')
  *-lnRPDay1 = lnOldDays
  lnRPDay1 = loOgScroll.ActiveControl.oldValue
ENDIF

*!*************************************************************
*! Name      : lfwDays
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the old value of days buckets in a variable
*!*************************************************************
*! Example     : = lfwDays()
*!*************************************************************
FUNCTION lfwDays
*-lnOldDays = EVALUATE(OGSYS18())
*-lnOldDays =  loOgScroll.ActiveControl.oldValue
*!*************************************************************
*! Name      : lfvDay2
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 2nd bucket
*!*************************************************************
*! Example     : = lfvDay2()
*!*************************************************************
FUNCTION lfvDay2

IF lnRPDay2 <= lnRPDay1
  =gfModalGen('TRM40133B40011','ALERT','2nd|1st')
  *-lnRPDay2 = lnOldDays
  lnRPDay2 = loOgScroll.ActiveControl.oldValue
ENDIF
IF lnRPDay2 >= lnRPDay3
  =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
  *--lnRPDay2 = lnOldDays
  lnRPDay2 = loOgScroll.ActiveControl.oldValue
ENDIF
*!*************************************************************
*! Name      : lfvDay3
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 3rd bucket
*!*************************************************************
*! Example     : = lfvDay3()
*!*************************************************************
FUNCTION lfvDay3

IF lnRPDay3 <= lnRPDay2
  =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
  *-lnRPDay3 = lnOldDays
  lnRPDay3 = loOgScroll.ActiveControl.oldValue
ENDIF
IF lnRPDay3 >= lnRPDay4
  =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
  *-lnRPDay3 = lnOldDays
  lnRPDay3 = loOgScroll.ActiveControl.oldValue
ENDIF
*!*************************************************************
*! Name      : lfvDay4
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 4th bucket
*!*************************************************************
*! Example     : = lfvDay4()
*!*************************************************************
FUNCTION lfvDay4

IF lnRPDay4 <= lnRPDay3
  =gfModalGen('TRM40133B40011','ALERT','4th|3rd')
  *-lnRPDay4 = lnOldDays
  lnRPDay4 = loOgScroll.ActiveControl.oldValue
ENDIF


*!*************************************************************
*! Name      : lfGetDesc
*! Developer : HEBA FATHI (HFK)
*! Date      : 12/22/2003
*! Purpose   : get the description of codes
*!*************************************************************
*! Example     : = lfGetDesc()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfGetDesc
PRIVATE lcCodeDesc

lcCodeDesc = SPACE(01)
DO CASE
  CASE lcGroup = 'cTermCode'
     lcCodeDesc = cTermCode + ' ' + PADR(gfCoddes(cTermCode,'CTERMCODE'),23)
  
  CASE lcGroup = 'CDIVISION'
    lcCodeDesc = CDIVISION + ' ' + PADR(gfCoddes(CDIVISION,'CDIVISION'),23)

  CASE lcGroup = 'cCurrCode'
    =SEEK(cCurrCode,'SYCCURR')
    lcCodeDesc = cCurrCode + ' ' + PADR(SYCCURR.CCURRDESC,23)

  *!*CASE lcGroup = 'Salesrep'      
  CASE lcGroup = LANG_Araging_SlsRp 		
    =SEEK(Salesrep,'Salesrep')
    lcCodeDesc = Salesrep.Name
  
  *!* CASE lcGroup = 'Region'  
  CASE lcGroup = LANG_Araging_Region   
    lcCodeDesc = REGION + ' ' + PADR(gfCoddes(REGION,'REGION'),23)

  CASE lcRpAgSort = 'U'
		lcCode = &lcTempAge..CCONT_CODE
		*!*			=SEEK(cCont_Code,'SYCINT')
		lcSelectCommand = [SELECT * FROM SYCINT WHERE cCont_Code = '] + lcCode + [']
		lnRemoteResult = loOGScroll.SQLExecute("SYCINT", lcSelectCommand,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"")
		IF lnRemoteResult >= 1 
	    lcCodeDesc = cCont_Code + ' ' + PADR(SYCINT.CCONT_DESC,23)
		ENDIF 
ENDCASE
RETURN lcCodeDesc
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the description of Curr.
*!*************************************************************
*! Example     : = lfvCurDisp()
*!*************************************************************
FUNCTION lfvCurDisp

=gfRepCur(.T., @lcRpCurr,@ldRpExDate ,lcRpTmpNam)

*!*************************************************************
*! Name      : lfGetRate
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the rate of Curr. in case of using multi curr.
*!*************************************************************
*! Example     : = lfGetRate()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfGetRate
PARAMETERS lcfCurr
PRIVATE lcfCurr,lnfRate

lnfRate = 0
DO CASE

  CASE lcRpCurr = 'D'
    lnfRate = gfchkrate('',lcfCurr,ldEXDate,.F.,oAriaApplication.ActiveCompanyID,.T.)

  CASE lcRpCurr = 'U'
    =SEEK(lcfCurr,lcRpTmpCur)
    lnfRate = &lcRpTmpCur..nExRate

ENDCASE
lnfRate = IIF(lnfRate = 0,1,lnfRate)
RETURN lnfRate

*!*************************************************************
*! Name      : lfRepName
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get salesrep name
*!*************************************************************
*! Example     : = lfRepName()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfRepName
PARAMETER lcRepCode
PRIVATE lcRepCode,lcRepName
lcRepName = IIF(SEEK(lcRepCode,'SALESREP'),SALESREP.NAME,SPACE(24))
RETURN lcRepName

*!*************************************************************
*! Name      : lfSumCre
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total credit for all the report
*!*************************************************************
*! Example     : = lfSumCre()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfSumCre
PARAMETER lcAccount,lcCurr
PRIVATE lnCredit,lnAlias,lcCurr

lnAlias = SELECT()
lnCredit = 0
IF SEEK(&lcPrtFile..ACCOUNT,'CREDIT')
  SELECT CREDIT
  SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT) FOR IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.)
    IF llMultCur AND lcRpCurr <> 'F'
      lcSign   = gfGetExSin("",ALLTRIM(CREDIT.CCURRCODE))
      lcSign   = IIF(EMPTY(lcSign),'*',lcSign)
      lnRate   = IIF(lcRpCurr = 'O',nExRate,lfGetRate(CREDIT.CCURRCODE))
      lnAmount = IIF(lcSign='/',AMOUNT/lnRate/nCurrUnit,AMOUNT*lnRate/nCurrUnit)
    ELSE
      lnAmount = AMOUNT
    ENDIF
    lnCredit   = lnCredit + lnAmount
  ENDSCAN
ENDIF
IF SEEK(&lcPrtFile..ACCOUNT,'ARHIST') AND llRPKeyOff
  SELECT ARHIST
  SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT);
    FOR TRANTYPE $ '0456' AND IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.);
    *-HFK,OLD,  AND IIF(!EMPTY(lcRpGlAcc),cArGlAcc = lcRpGlAcc,.T.)
    AND IIF(!EMPTY(lcRpGlAcc),&lcGlCondition,.T.)

    SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT);
      FOR (HISTDATE > ldRpEndDat) .AND. (TRANDATE <= ldRpEndDat);
      and TRANTYPE $ '0456' AND IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.);
      *-HFK,OLD,  AND IIF(!EMPTY(lcRpGlAcc),cArGlAcc = lcRpGlAcc,.T.)
      AND IIF(!EMPTY(lcRpGlAcc),&lcGlCondition,.T.)

      IF llMultCur AND lcRpCurr <> 'F'
        lcSign   = gfGetExSin("",ALLTRIM(ARHIST.CCURRCODE))
        lcSign   = IIF(EMPTY(lcSign),'*',lcSign)
        lnRate   = IIF(lcRpCurr = 'O',nExRate,lfGetRate(ARHIST.CCURRCODE))
        lnAmount = IIF(lcSign='/',AMOUNT/lnRate/nCurrUnit,AMOUNT*lnRate/nCurrUnit)
      ELSE
        lnAmount = AMOUNT
      ENDIF
      lnCredit   = lnCredit + lnAmount
    ENDSCAN
  ENDSCAN
ENDIF

SELECT(lnAlias)
RETURN lnCredit

*!*************************************************************
*! Name      : lfCharge
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total charge back for all the report
*!*************************************************************
*! Example     : = lfCharge()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfCharge
PARAMETER lcAccount,lcCurr
PRIVATE lnDebit,lnAlias,lnAR,lcCurr*

lnAlias = SELECT()
lnDebit = 0
IF SEEK(&lcPrtFile..ACCOUNT,'DEBIT')
  SELECT DEBIT
  SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT);
       FOR (IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.) AND TRANTYPE <> '3')
    IF llMultCur AND lcRpCurr <> 'F'
      lcSign   = gfGetExSin("",ALLTRIM(DEBIT.CCURRCODE))
      lcSign   = IIF(EMPTY(lcSign),'*',lcSign)
      lnRate   = IIF(lcRpCurr = 'O',nExRate,lfGetRate(DEBIT.CCURRCODE))
      lnAmount = IIF(lcSign='/',AMOUNT/lnRate/nCurrUnit,AMOUNT*lnRate/nCurrUnit)
    ELSE
      lnAmount = AMOUNT
    ENDIF
    lnDebit   = lnDebit + lnAmount
  ENDSCAN
ENDIF
IF SEEK(&lcPrtFile..ACCOUNT,'ARHIST') AND llRPKeyOff
  SELECT ARHIST
  SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT);
       FOR TRANTYPE $ '12' AND IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.);
       *-HFK,OLD,  AND IIF(!EMPTY(lcRpGlAcc),cArGlAcc = lcRpGlAcc,.T.)
      AND IIF(!EMPTY(lcRpGlAcc),&lcGlCondition,.T.)

    IF llMultCur AND lcRpCurr <> 'F'
      lcSign   = gfGetExSin("",ALLTRIM(ARHIST.CCURRCODE))
      lcSign   = IIF(EMPTY(lcSign),'*',lcSign)
      lnRate   = IIF(lcRpCurr = 'O',nExRate,lfGetRate(ARHIST.CCURRCODE))
      lnAmount = IIF(lcSign='/',AMOUNT/lnRate/nCurrUnit,AMOUNT*lnRate/nCurrUnit)
    ELSE
      lnAmount = AMOUNT
    ENDIF
    lnDebit   = lnDebit + lnAmount
  ENDSCAN
ENDIF
SELECT(lnAlias)
RETURN lnDebit
*!*************************************************************
*! Name      : lfSumCharge
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total charge back for all the report
*!*************************************************************
*! Example     : = lfSumCharge()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfSumCharge
PRIVATE lnDebit,lnAlias,lnRecNo

lnAlias = SELECT()
SELECT(lcPrtFile)
SET RELATION OFF INTO (lcTmpTran)
lnRecNo = RECNO()
SUM CHGBACK TO lnDebit
SELECT(lnAlias)
RETURN lnDebit
*!*************************************************************
*! Name      : lfCharge
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total charge back for all the report
*!*************************************************************
*! Example     : = lfCharge()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 5
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfGetAdd
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the address of the customer
*!*************************************************************
*! Example     : = lfGetAdd()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfGetAdd
laSoldTo[1] = gfGetAdr(lcPrtFile , '' , '' , '' , 1 , 1)
laSoldTo[2] = gfGetAdr(lcPrtFile , '' , '' , '' , 2 , 1)
laSoldTo[3] = gfGetAdr(lcPrtFile , '' , '' , '' , 3 , 1)
laSoldTo[4] = gfGetAdr(lcPrtFile , '' , '' , '' , 4 , 1)
laSoldTo[5] = gfGetAdr(lcPrtFile , '' , '' , '' , 5 , 1)
=lfAdrShift('laSoldTo')
RETURN ''


**!*************************************************************
*! Name      : lfGetNotes
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the notes of the customer
*!*************************************************************
*! Example     : = lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes

lcNotes = [IIF(llRPCusNot .AND.]+;
          [SEEK('A'+ ACCOUNT,'NOTEPAD') .AND.]+;
          [!EMPTY(NOTEPAD.mNotes),]+;
          [ALLTRIM(NOTEPAD.mNotes) + ] +;
          [IIF(!CHR(13) $ RIGHT(NOTEPAD.mNotes,2), CHR(13),'') , '')]
*REPLACE &lcPrtFile..LLOK_STAT WITH .T.
RETURN EVALUATE(lcNotes)
*!*************************************************************

*!*************************************************************
*! Name      : lfDefCurr
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/11/1998
*! Purpose   : Return Default currency value.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : currency default value.
*!*************************************************************
*! Example     : = lfDefCurr()
*!*************************************************************
*!
FUNCTION lfDefCurr

RETURN IIF(llMultCur,'F','O')
*-- end of lfDefCurr.

*!*************************************************************
*! Name      : lfFillSort
*! Developer : HEBA FATHI (HFK)
*! Date      : 12/22/2003
*! Purpose   : Fill sort by arrays.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfFillSort()
*!*************************************************************
FUNCTION lfFillSort
PRIVATE lnRows
lnRows = IIF(llMultCur,7,6)
DIMENSION laSortDesc[lnRows,1],laSortVal[lnRows,1]

*!*	laSortDesc[1,1] = 'Account'
*!*	laSortDesc[2,1] = 'Terms'
*!*	laSortDesc[3,1] = 'Salesrep'
*!*	laSortDesc[4,1] = 'Division'
*!*	laSortDesc[5,1] = 'Region'
*!*	laSortDesc[6,1] = 'Country'
laSortDesc[1,1] = LANG_Araging_Accnt 
laSortDesc[2,1] = LANG_Araging_Trms 
laSortDesc[3,1] = LANG_Araging_SlsRp 
laSortDesc[4,1] = LANG_Araging_Div 
laSortDesc[5,1] = LANG_Araging_Region 
laSortDesc[6,1] = LANG_Araging_Cntry 

laSortVal[1,1] = 'A'
laSortVal[2,1] = 'T'
laSortVal[3,1] = 'S'
laSortVal[4,1] = 'D'
laSortVal[5,1] = 'R'
laSortVal[6,1] = 'U'

*-- if company support multi currency add Currency to Sort By POPUP.
IF llMultCur
  =AINS(laSortDesc,5)
  =AINS(laSortVal,5)
  laSortDesc[5,1] = LANG_Araging_Curr
  laSortVal[5,1]  = 'C'
ENDIF  && end if company support multi currency add Currency to Sort By POPUP.
*-- end of lfFillSort.

*!*************************************************************
*! Name      : lfFilTrnAr
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Create temporary cursor structure.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfFilTrnAr()
*!*************************************************************
FUNCTION lfFilTrnAr
PRIVATE laTempTran , laTempCust , lcExcStat
lcExcStat = SET('EXACT')
SET EXACT ON

*-- laTempStru : Array hold structure of temporary transaction + Customer fields.
*-- laTempTran : Array hold structure of transaction fields.
*-- laTempCust : Array hold structure of Customer fields.
*! B609748,1 MMT 11/27/2011 A/R Aging Export to Excel needs to have the customer name[Start]
*DIMENSION laTempStru[40,18] , laTempTran[1,18] , laTempCust[1,18]
*DIMENSION laTempStru[52,18] , laTempTran[1,18] , laTempCust[1,18]
*! B609748,1 MMT 11/27/2011 A/R Aging Export to Excel needs to have the customer name[END]

*E304097,1 SAH 30/12/2018 modify this program as the Payment Terms code of Customer is exported to  AR aging report excel not the Invoice or Credit Memo related Payment term [start]
*DIMENSION laTempStru[52,18] , laTempTran[1,18] , laTempCust[1,18]
DIMENSION laTempStru[53,18] , laTempTran[1,18] , laTempCust[1,18]
*E304097,1 SAH 30/12/2018 modify this program as the Payment Terms code of Customer is exported to  AR aging report excel not the Invoice or Credit Memo related Payment term [end]

STORE '' TO laTempStru,laTempTran
PRIVATE lnFileCnt , lnFldRow

*-- Fields from Customer File.
SELECT CUSTOMER
= OGAFIELDS(@laTempCust)

laTempStru[1,1]  = 'ACCOUNT'
laTempStru[2,1]  = 'CFACCODE'
laTempStru[3,1]  = 'SALESREP'
laTempStru[4,1]  = 'CURRENT'
laTempStru[5,1]  = 'AGE30'
laTempStru[6,1]  = 'AGE60'
laTempStru[7,1]  = 'AGE90'
laTempStru[8,1]  = 'AGE120'
laTempStru[9,1]  = 'TOTAGE'
laTempStru[10,1] = 'OPENCR'
laTempStru[11,1] = 'CHGBACK'
laTempStru[12,1] = 'NETBAL'
laTempStru[13,1] = 'REGION'
laTempStru[14,1] = 'CTERMCODE'
laTempStru[15,1] = 'CDIVISION'
laTempStru[16,1] = 'CCURRCODE'
laTempStru[17,1] = 'CCONT_CODE'



*-- Loop to get other dimensions of Customer included fields (Like master file)
FOR lnFileCnt = 1 TO 17
  lnFldRow = ASCAN(laTempCust,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCust,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempCust[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempCust[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempCust[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of Customer included fields (Like master file)

*-- Fields from Debit , Credit , and History Files
SELECT ARHIST
=OGAFIELDS(@laTempTran)
laTempStru[18,1] = 'TRANTYPE'
laTempStru[19,1] = 'TRANCODE'
laTempStru[20,1] = 'TRAN'
laTempStru[21,1] = 'BATCH'
laTempStru[22,1] = 'CINSTALNO'
laTempStru[23,1] = 'TRANDATE'
laTempStru[24,1] = 'DPOSTDATE'
laTempStru[25,1] = 'CHGBK_DATE'
laTempStru[26,1] = 'DESC'
laTempStru[27,1] = 'REFERENCE'
laTempStru[28,1] = 'AMOUNT'
laTempStru[29,1] = 'DUEDATE'
laTempStru[30,1] = 'CADJACCT'
laTempStru[31,1] = 'CARGLACC'
laTempStru[32,1] = 'NCURRUNIT'
laTempStru[33,1] = 'NEXRATE'

*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 18 TO 33
  lnFldRow = ASCAN(laTempTran,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempTran,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempTran[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempTran[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempTran[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

*-- Add Fields from PostDchq File.
*-- cTranType field : Added to indicate that post dated if it's "P" else normal transaction.
laTempStru[34 ,1] = 'CTRANTYPE'
laTempStru[34 ,2] = 'C'
laTempStru[34 ,3] = 1
laTempStru[34 ,4] = 0

laTempStru[35 ,1] = 'CHEQUENO'
laTempStru[35 ,2] = 'C'
laTempStru[35 ,3] = 10
laTempStru[35 ,4] = 0

*-- Temporary Key Field.

laTempStru[36 ,1] = 'CTEMPKEY'
laTempStru[36 ,2] = 'C'
laTempStru[36 ,3]  = 50
laTempStru[36 ,4] = 0

*-- Field for the MNOTES.
laTempStru[37 ,1] = 'mNotes'
laTempStru[37 ,2] = 'M'
laTempStru[37 ,3] = 10
laTempStru[37 ,4] = 0

*-- Field for the MNOTES.
laTempStru[38 ,1] = 'llFlgPad'
laTempStru[38 ,2] = 'L'
laTempStru[38 ,3] = 1
laTempStru[38 ,4] = 0

laTempStru[39 ,1] = 'cCodeDescr'
laTempStru[39 ,2] = 'C'
laTempStru[39 ,3] = 30
laTempStru[39 ,4] = 0

laTempStru[40 ,1] = 'Ccitcstchk'
laTempStru[40 ,2] = 'C'
laTempStru[40 ,3] = 30
laTempStru[40 ,4] = 0

*! B609748,1 MMT 11/27/2011 A/R Aging Export to Excel needs to have the customer name[Start]
laTempStru[41,1] = 'BTNAME'
laTempStru[42,1] = 'CADDRESS1'
laTempStru[43,1] = 'CADDRESS2'
laTempStru[44,1] = 'CADDRESS3'
laTempStru[45,1] = 'CADDRESS4'
laTempStru[46,1] = 'CADDRESS5'
laTempStru[47,1] = 'CRLIMIT'
laTempStru[48,1] = 'FACTACCT'
laTempStru[49,1] = 'KEEPER'

*-- Loop to get other dimensions of Customer included fields (Like master file)
FOR lnFileCnt = 41 TO 49
  lnFldRow = ASCAN(laTempCust,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCust,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempCust[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempCust[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempCust[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of Customer included fields (Like master file)

laTempStru[50,1] = 'Status'
laTempStru[50 ,2] = 'C'
laTempStru[50 ,3] = 30
laTempStru[50 ,4] = 0
laTempStru[51,1] = 'PHONE1'
laTempStru[51 ,2] = 'C'
laTempStru[51 ,3] = 25
laTempStru[51 ,4] = 0
laTempStru[52,1] = 'PHONE2'
laTempStru[52 ,2] = 'C'
laTempStru[52 ,3] = 25
laTempStru[52 ,4] = 0
*! B609748,1 MMT 11/27/2011 A/R Aging Export to Excel needs to have the customer name[End]


*E304097,1 SAH 30/12/2018 modify this program as the Payment Terms code of Customer is exported to  AR aging report excel not the Invoice or Credit Memo related Payment term [start]
laTempStru[53,1] = 'TRAN_PAYMENT_TERM'
laTempStru[53 ,2] = 'C'
laTempStru[53 ,3] = 30
laTempStru[53 ,4] = 0
*E304097,1 SAH 30/12/2018 modify this program as the Payment Terms code of Customer is exported to  AR aging report excel not the Invoice or Credit Memo related Payment term [end]


SET EXACT &lcExcStat  && Restore exact setting.

=lfWorkFile()  && Create temporary files.
*-- end of lfFilTrnAr.
*!*************************************************************
*! Name      : lfWorkFile
*! Developer : HEBA FATHI (HFK)
*! Date      : 12/22/2003
*! Purpose   : Create work cursors.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWorkFile()
*!*************************************************************
FUNCTION lfWorkFile

*-- if company use multi currency.
IF llMultCur

  *-- Open Currency file.
  IF !USED('SYCCURR')
    =gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
  ENDIF
ELSE  && else company use single currency format.
  *-- Open Post dated cheques file.
  IF !USED('POSTDCHQ')
    USE (oAriaApplication.DataDir+'POSTDCHQ') ORDER TAG POSTDCHQ IN 0 SHARED
  ENDIF
ENDIF  && end if company use multi currency.

*! B609545,1 MMT 03/06/2011 Error in AR aging report when user change report format[Start]
*!*	IF !USED('SYCINT')
*!*	  =gfOpenFile(oAriaApplication.SysPath+'SYCINT',oAriaApplication.SysPath+'Ccontcode','SH')   
*!*	ENDIF

*!*	SET ORDER TO Ccontcode IN SYCINT
*! B609545,1 MMT 03/06/2011 Error in AR aging report when user change report format[End]
SET ORDER TO CUSTOMER IN CUSTOMER

*-- First Close all temporary files if it was opened before [Begin]
IF USED(lcTempAge)
  USE IN (lcTempAge)
ENDIF

IF USED(lcTempRev)
  USE IN (lcTempRev)
ENDIF

IF USED(lcTempAcc)
  USE IN (lcTempAcc)
ENDIF

*-- First Close all temporary files if it was opened before [End  ]

*--Heba..Create Totals file [Begin]
*-create Temp File Structure Array
DIMENSION laTempacstru[3,4]
laTempacstru[1,1]='Account'
laTempacstru[1,2]='C'
laTempacstru[1,3]=5
laTempacstru[1,4]=0

laTempacstru[2,1]='nAges'
laTempacstru[2,2]='N'
laTempacstru[2,3]=11
laTempacstru[2,4]=2

laTempacstru[3,1]='nNetBal'
laTempacstru[3,2]='N'
laTempacstru[3,3]=11
laTempacstru[3,4]=2
lcTempAcc  = loOGScroll.gfTempName()
gfCrtTmp(lcTempAcc,@laTempacstru,"ACCOUNT",lcTempAcc,.F.)
*--Heba [End]


*-- Create transaction lines file.

*!*	*-- First  index used in reverse sorting in short form to detect end of group .

*!*	*-- Second index used in printing according sort by .

*-  hfk , create cursor
lcTempAge = loOGScroll.gfTempName()
gfCrtTmp(lcTempAge,@laTempstru,,"",.T.)
*-gfCrtTmp(lcTempAge,@laTempstru,"cTempKey",lcTempAge,.T.)
SELECT (lcTempAge)
ZAP
INDEX ON cTempKey TAG (lcTempRev) DESCENDING
INDEX ON cTempKey TAG (lcTempAge)  

*-- end of lfWorkFile.
*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 04/05/99
*! Purpose   : change index flag to reindex temp cursor.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
llChSortBy = .T.  && Detect that user change sorting method.
*-- end of lfvSortBy.

*!*************************************************************
*! Name      : lfGetReplc
*! Developer : HEBA FATHI (HFK)
*! Date      : 12/22/2003
*! Purpose   : Get Replaced expression.(According to sort by options)
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : lcExpr ---> which means (Sort by expression)
*!*************************************************************
*! Example   : =lfGetReplc()
*!*************************************************************
*!
FUNCTION lfGetReplc
PRIVATE lcExpr
DO CASE
  *-- Sort by account
  CASE lcRpAgSort = 'A'
    *! B609922,1 MMT 05/17/2012 AR aging report does not  have subtotal for currecny[Start]
    *lcGroup  = []  && Used in short form only.
    lcGroup  =  [Account]  && Used in short form only.    
    lcCodeDesc = [IIF(SEEK('M'+ACCOUNT,'CUSTOMER','CUSTOMER'),Customer.BtName,SPACE(24)) + LANG_Araging_Total]    
    *! B609922,1 MMT 05/17/2012 AR aging report does not  have subtotal for currecny[END]
    lcInnGrp = [Account]
    *--lcExpr   = [ACCOUNT + cTranType]
    lcExpr   = [Account + cTranType]
    *-- lcSorted = 'Account'
    lcSorted = LANG_Araging_Accnt 


  *-- Sort by Terms
  CASE lcRpAgSort = 'T'
    lcGroup = [cTermCode]
    lcInnGrp = [cTermCode + Account]
		*!*	    lcCodeDesc = [gfCoddes(cTermCode,'CTERMCODE',.T.) + ' Totals ==>']
    lcCodeDesc = [gfCoddes(cTermCode,'CTERMCODE',.T.) + LANG_Araging_Total]
    lcExpr   = [cCodeDescr + Account + cTranType]
    *--lcSorted = 'Term Code'
    lcSorted = LANG_Araging_Trmcod 


  *-- Sort by division
  CASE lcRpAgSort = 'D'
    lcGroup  = [CDIVISION]
    lcInnGrp = [CDIVISION + Account]
		*!*	    lcCodeDesc = [gfCoddes(CDIVISION,'CDIVISION',.T.) + ' Totals ==>']
    *! B609922,1 MMT 05/17/2012 AR aging report does not  have subtotal for currecny[Start]
    *lcCodeDesc = [gfCoddes(cTermCode,'CTERMCODE',.T.) + LANG_Araging_Total]    
    lcCodeDesc = [gfCoddes(CDIVISION,'CDIVISION',.T.) + LANG_Araging_Total]
    *! B609922,1 MMT 05/17/2012 AR aging report does not  have subtotal for currecny[END]
    lcExpr   = [CDIVISION + Account + cTranType]
    *--lcSorted = 'Division'
    lcSorted = LANG_Araging_Div 

  *-- Sort by currency
  CASE lcRpAgSort = 'C'
    lcGroup  = [cCurrCode]
    lcInnGrp = [cCurrCode + Account]
		*!*	    lcCodeDesc = [cCurrCode + '-' + IIF(SEEK(cCurrCode,'SYCCURR'),SYCCURR.CCURRDESC,SPACE(30)) + ' Totals ==>']
    lcCodeDesc = [cCurrCode + '-' + IIF(SEEK(cCurrCode,'SYCCURR'),SYCCURR.CCURRDESC,SPACE(30)) + LANG_Araging_Total]
    lcExpr   = [CCURRCODE + Account + cTranType]
    *--lcSorted = 'Currency'
    lcSorted = LANG_Araging_Curr 

  *-- Sort by sales rep.
  CASE lcRpAgSort = 'S'
    lcGroup  = [Salesrep]
    lcInnGrp = [Salesrep + Account]
		*!*	    lcCodeDesc = [IIF(SEEK(Salesrep,'Salesrep'),Salesrep.Name,SPACE(24)) + ' Totals ==>']
    lcCodeDesc = [IIF(SEEK(Salesrep,'Salesrep'),Salesrep.Name,SPACE(24)) + LANG_Araging_Total]
    lcExpr   = [Salesrep + Account + cTranType]
    *--lcSorted = 'Sales Representative'
    lcSorted = LANG_Araging_SlsRep 


  *-- Sort by region
  CASE lcRpAgSort = 'R'
    lcGroup  = [Region]
    lcInnGrp = [Region + Account]
		*!*	    lcCodeDesc = [gfCoddes(REGION,'REGION',.T.) + ' Totals ==>']
    lcCodeDesc = [gfCoddes(REGION,'REGION',.T.) + LANG_Araging_Total]
    lcExpr   = [Region + Account + cTranType]
    *-- lcSorted = 'Region'
    lcSorted = LANG_Araging_Region 


  *-- Sort by country.
  CASE lcRpAgSort = 'U'
    lcGroup  = [cCont_Code]
    lcInnGrp = [cCont_Code + Account]
		lcContCode = &lcTempAge..CCONT_CODE
		lcSelectCommand = [SELECT * FROM SYCINT WHERE cCont_Code = '] + lcContCode + [']
		lnRemoteResult = loOGScroll.SQLExecute("SYCINT", lcSelectCommand,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"")
		IF lnRemoteResult >= 1 
			lcCodeDesc = [cCont_Code + '-' + SYCINT.CCONT_DESC + LANG_Araging_Total]
		ELSE 
			lcCodeDesc = [cCont_Code + '-' + SPACE(30) + LANG_Araging_Total]
		ENDIF 
		*!*			lcCodeDesc = [cCont_Code + '-' + IIF(SEEK(cCont_Code,'SYCINT'),SYCINT.CCONT_DESC,SPACE(30)) + ' Totals ==>']
    lcExpr   = [cCont_Code + Account + cTranType]
    *--lcSorted = 'Country'
    lcSorted = LANG_Araging_Cntry 
ENDCASE

*-- if multi currency and print in foreign and not sort by currency.
*-- include currency in your groups, and index filter.
IF llCurrGrp AND (LEFT(lcExpr,9) != 'CCURRCODE')
  lcExpr   = lcExpr   + [ + CCURRCODE]
  lcGroup  = lcGroup  + [ + CCURRCODE]
  lcInnGrp = lcInnGrp + [ + CCURRCODE]
ENDIF  && end if multi currency and print in foreign and not sort by currency.

RETURN lcExpr
*-- end of lfGetReplc

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfvEndDate
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Valid function for Ending period.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : .T.
*!*************************************************************
*! Example   : =lfvEndDate()
*!*************************************************************
*!
FUNCTION lfvEndDate

llRpKeyOff = (ldRpEndDat < oAriaApplication.SystemDate)  && Key off value (Yes or No)
=LFOGSHOWGET('LLRPKEYOFF')
RETURN .T.
*-- end of lfvEndDate.

*!*************************************************************
*! Name      : lfvIncDet
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Valid function for Include credit detail.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : .T.
*!*************************************************************
*! Example   : =lfvEndDate()
*!*************************************************************
*!
FUNCTION lfvIncDet
LNAGECRD = ASCAN(LAOGOBJTYPE,'LLRPAGECRD')
IF lnAgeCrd != 0
    LNAGECRD = ASUBSCRIPT(LAOGOBJTYPE,LNAGECRD,1)
    STORE LLRPCRDDET TO LAOGOBJCNT[LNAGECRD] , LLRPAGECRD
    = LFOGSHOWGET('LLRPAGECRD')
ENDIF
*-- end of lfvIncDet.

*!*************************************************************
*! Name      : lfCollect
*! Developer : HEBA FATHI (HFK)
*! Date      : 12/22/2003
*! Purpose   : Collect new data in two temporary cursor One is Transaction cursor
*!           : and another if for totals in base currency to handle due past only
*!           : and Over/Under Filters...
*!           : Note, Technique used here is to scan transcation files to get there transcations
*!           :       If achive customer filter in customer file.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lfCollect
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfCollect
*-- Define collect variables [Begin]
*-- lcFacFlt : Variable Hold Factor/Non-Factor/Both Filter.
*-- lcDebFlt : Variable Hold Debit main Filter.
*-- lcCreFlt : Variable Hold Credit main Filter.
*-- lcHstFlt : Variable Hold History main Filter.
 

PRIVATE lcDebFlt, lcCreFlt, lcHstFlt , lcFacFlt, lcGlAccFlt
STORE '' TO lcDebFlt, lcCreFlt, lcHstFlt, lcFacFlt

*E304097,1 SAH 30/12/2018 modify this program as the Payment Terms code of Customer is exported to  AR aging report excel not the Invoice or Credit Memo related Payment term [start]
STORE '' TO   m.TRAN_PAYMENT_TERM
*E304097,1 SAH 30/12/2018 modify this program as the Payment Terms code of Customer is exported to  AR aging report excel not the Invoice or Credit Memo related Payment term [end]

*-- Define collect variables [End  ]

*-- if print Factored only
IF lcRpFactor = 'F'

  lcFacFlt = IIF(EMPTY(lcRPSpeFac),[!EMPTY(cFacCode)],[cFacCode=lcRPSpeFac])

  lcDebFlt = [TRANDATE<=ldRpEndDat AND ]                          +;
             [IIF( TRANTYPE='3',CHGBK_DATE<=ldRpEndDat,.T.)]

  lcCreFlt = [TRANDATE<=ldRPEndDat AND ]                          +;
             [IIF( TRANTYPE='6',CREDT_DATE<=ldRPEndDat,.T.)]

ELSE && Factored (and/or) non Factored

  IF lcRpFactor='N'
    lcFacFlt = [EMPTY(cFacCode)]
  ENDIF

  lcDebFlt = [IIF( TRANTYPE='3',]                                +;
             [TRANDATE<=ldRpEndDat AND CHGBK_DATE<=ldRpEndDat ,] +;
             [TRANDATE<=ldRpEndDat )]

  lcCreFlt = [IIF( TRANTYPE='6',]                                +;
             [TRANDATE<=ldRPEndDat AND CREDT_DATE<=ldRPEndDat ,] +;
             [TRANDATE<=ldRPEndDat )]

ENDIF  && end if print Factored only.

*! B608184,1 MMT 07/30/2007 Fix bug of Wrong data when Select 'Show past due only'[Start]
*!*	IF llRPShowPa
*!*	  lcDebFlt  = lcDebFlt + [ AND DueDate < ldRpEndDat]
*!*	ENDIF  && end if user want to show due past only
*! B608184,1 MMT 07/30/2007 Fix bug of Wrong data when Select 'Show past due only'[ENd]


lcHstFlt = [HISTDATE > ldRPEndDat AND TRANDATE <= ldRPEndDat]  && History (Keyed) filter
 
IF !EMPTY(ldPostDate)
  lcDebFlt = lcDebFlt + IIF(EMPTY(lcDebFlt),'',[ AND ]) + [IIF(TranType='1',ldPostDate >= dPostDate,.T.)]
  lcCreFlt = lcCreFlt + IIF(EMPTY(lcCreFlt),'',[ AND ]) + [IIF(TranType='0',ldPostDate >= dPostDate,.T.)]
  lcHstFlt = lcHstFlt + IIF(EMPTY(lcHstFlt),'',[ AND ]) + [IIF(TranType $ '01',ldPostDate >= dPostDate,.T.)]
ENDIF
*-- Add Factor Filter to Main Files Filter.
IF !EMPTY(lcFacFlt)
  lcDebFlt = lcDebFlt + [ AND ] + lcFacFlt
  lcCreFlt = lcCreFlt + [ AND ] + lcFacFlt
  lcHstFlt = lcHstFlt + [ AND ] + lcFacFlt
ENDIF

*-- Add GlAccount Filter to Main Files Filter.
IF !EMPTY(STRTRAN(lcRpGlAcc,'-',''))
  *!*	  lcDebFlt = lcDebFlt + [ AND cArGlAcc = lcRpGlAcc]
  *!*	  lcCreFlt = lcCreFlt + [ AND cArGlAcc = lcRpGlAcc]
  *!*	  lcHstFlt = lcHstFlt + [ AND cArGlAcc = lcRpGlAcc]
  lcDebFlt = lcDebFlt + [ AND &lcGlCondition]
  lcCreFlt = lcCreFlt + [ AND &lcGlCondition]
  lcHstFlt = lcHstFlt + [ AND &lcGlCondition]
ENDIF

*-- Declare Customer file memory variables [Begin]
SELECT CUSTOMER
GO TOP
 
SCATTER MEMVAR MEMO BLANK  && Define aging variables
*-- Declare Customer file memory variables [End  ]



lcFilterExp = lcFilterExp + LANG_Araging_And  + " CUSTOMER.STATUS $ lcRpStatFr "
*--lcFilterExp = lcFilterExp + ' AND ' + " CUSTOMER.STATUS $ lcRpStatFr "

*-- insert post date cheques Transactions if not multi currency company.
IF !llMultCur
  =lfInsPstCh()
ENDIF

*-- Insert Debit Transactions ...
=lfInsDebit()

*-- Insert Credit Transactions ...
=lfInsCredt()

*-- If user want to include key off transactions.
IF llRpKeyOff
  *-- Insert keyed Transactions ...
  =lfInsHisto()
ENDIF
*-- end of lfCollect.

*!*************************************************************
*! Name      : lfInsPstCh
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert post dated cheques records to Transaction File.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsPstCh()
*!*************************************************************
*!
FUNCTION lfInsPstCh
*-- Set relation with customer file.
SELECT POSTDCHQ
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
*! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug whn select by sales rep missed data .. begin
GO top
*! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug whn select by sales rep missed data .. end

*-- Scan All post dated checks achieve customer filter.
SCAN FOR account+DTOS(paydate) = '' AND CUSTOMER.TYPE = 'M' AND &lcFilterExp AND ;
         Amount <> 0

  SCATTER MEMVAR
  m.TranDate  = POSTDCHQ.PAYDATE       && check pay date
  *-- m.Desc  = "PostDated Checks"     && Description
  m.Desc      = LANG_Araging_Pstdat      && Description
  m.TranType  = ' '                    && Empty transaction
  m.cTranType = 'P'  && This field Added to indicate that post dated if it's "P" else normal transaction.

  DO lpInsRecrd  && Insert New Record in Transaction Temporary File.

ENDSCAN  && end Scan All post dated checks achieve customer filter.

SET RELATION TO  && Rest relation.

*-- end of lfInsPstCh.

*!*************************************************************
*! Name      : lfInsDebit
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert Debit records to Transaction and total Files.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsDebit()
*!*************************************************************
*!
FUNCTION lfInsDebit
*-- Set relation with customer file.
SELECT DEBIT
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
SET RELATION TO Tran INTO Invhdr ADDITIVE
*! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug whn select by sales rep missed data .. begin
GO top
*! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug whn select by sales rep missed data .. end

*-- Scan Debit records achieve both customer and debit filters.

*! B609596,1 MMT 05/30/2011 Chnage the filteration of Sales rep. to be on INVHDR and RETHDR Fields[Start]
IF llWorkFile
  lcRpSlsExp = [llWorkFile .AND. IIF(TRANTYPE='1',SEEK(INVHDR.REP1, lcSalsFile),SEEK(CUSTOMER.SALESRep,lcSalsFile))]
ENDIF
*! B609596,1 MMT 05/30/2011 Chnage the filteration of Sales rep. to be on INVHDR and RETHDR Fields[End]


SCAN FOR account+tran+cinstalno+DTOS(trandate) = '' AND &lcDebFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcFilterExp AND Amount <> 0 .AND. &lcRpSlsExp

  *-- if user does not want to include charge back transaction and it is Charge back.
  IF (!llRPPrnCh .AND. TranType = '3' )
    LOOP
  ENDIF

  SCATTER MEMVAR
  DO lpDebDeal   && Calculate Ages and fill memory variables (Totage and NetBal)
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.

ENDSCAN  && end Scan Debit records achieve both customer and debit filters.

SET RELATION TO  && Rest relation.
*-- end of lfInsDebit.

*!*************************************************************
*! Name      : lfInsCredt
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert Credit records to Transaction and total Files.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsCredt()
*!*************************************************************
*!
FUNCTION lfInsCredt
*-- Set relation with customer file.


SELECT CREDIT
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
SET RELATION TO Tran INTO Invhdr ADDITIVE

*! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug whn select by sales rep missed data .. begin
GO top
*! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug whn select by sales rep missed data .. end
*-- Scan Credit records achieve both customer and Credit filters.

*! B609596,1 MMT 05/30/2011 Chnage the filteration of Sales rep. to be on INVHDR and RETHDR Fields[Start]
IF llWorkFile
  lcRpSlsExp = [llWorkFile .AND. IIF(TRANTYPE='0',gfSeek(CREDIT.TRAN,'RETHDR') AND SEEK(RETHDR.SALESREP1, lcSalsFile),SEEK(CUSTOMER.SALESRep,lcSalsFile))]
ENDIF
*! B609596,1 MMT 05/30/2011 Chnage the filteration of Sales rep. to be on INVHDR and RETHDR Fields[End]


SCAN FOR account+tran+DTOS(trandate) = '' AND &lcCreFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcFilterExp AND Amount <> 0 .AND. &lcRpSlsExp
         
  SCATTER MEMVAR
  DO lpCreDeal   && Calculate Ages and fill memory variables (OpenCr and NetBal)
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.
  
ENDSCAN  && end Scan Credit records achieve both customer and Credit filters.

SET RELATION TO  && Rest relation.
*-- end of lfInsCredt.

*!*************************************************************
*! Name      : lfInsHisto
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert History records to Transaction and total Files.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsHisto()
*!*************************************************************
*!
FUNCTION lfInsHisto
*-- Set relation with customer file.
SELECT ARHIST
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
SET RELATION TO Tran INTO Invhdr ADDITIVE
*! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug whn select by sales rep missed data .. begin
GO top
*! B608017,1 T20070313.0015 AYM 03/25/2007 Fix bug whn select by sales rep missed data .. end


*! B609596,1 MMT 05/30/2011 Chnage the filteration of Sales rep. to be on INVHDR and RETHDR Fields[Start]
IF llWorkFile
  lcRpSlsExp = [llWorkFile .AND. IIF( INLIST(TRANTYPE,'1','I'),SEEK(INVHDR.REP1, lcSalsFile),IIF(INLIST(TRANTYPE,'0','R'),gfSeek(ARHIST.TRAN,'RETHDR') AND SEEK(RETHDR.SALESREP1,lcSalsFile), SEEK(CUSTOMER.SALESRep,lcSalsFile)))]
ENDIF
*! B609596,1 MMT 05/30/2011 Chnage the filteration of Sales rep. to be on INVHDR and RETHDR Fields[END]


*-- Scan History records achieve both customer and History filters.
SCAN FOR account+tran+cinstalno = '' AND &lcHstFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcFilterExp AND Amount <> 0 .AND. &lcRpSlsExp


  *-- IF user does not want to include charge back transaction and it is Charge back (Type=3),
  *-- OR charge back date greater than ending period OR it is credit on account transaction
  *-- (Type=6) AND credit date greater than ending period, then
  *-- Cancel this transaction [i.e. LOOP it]


  IF (!llRPPrnCh .AND. TranType = '3' )            OR ;
     (TRANTYPE = '3' .AND. CHGBK_DATE>ldRPEndDat)  OR ;
     (TRANTYPE = '6' .AND. CREDT_DATE>ldRPEndDat)  OR ;
     !(TRANTYPE $ "0123456M")
     LOOP
  ENDIF

  SCATTER MEMVAR

  *====> Debit Transaction  <====*
  *-- if Invoice(Type=1), Debit Adjustment(Type=2) OR Charge back(Type=3) .


  IF TRANTYPE $ '123M'

    DO lpDebDeal    && Calculate Ages and fill memory variables (Totage and NetBal)
  ENDIF

  *====> Credit Transaction  <====*
  *-- if Return Invoice(Type=0), Payment(Type=4), Credit Adjustment(Type=5)
  *-- , OR Credit on account(Type=6).
  IF TRANTYPE $ '0456'
    DO lpCreDeal   && Calculate Ages and fill memory variables (OpenCr and NetBal)
  ENDIF

  IF lcRpReport = 'D'            && In case the user select the detail report.
    m.cTranType = 'H'  && This field Added to indicate history transactions.
  ENDIF

  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.

ENDSCAN  && end Scan History records achieve both customer and History filters.
SET RELATION TO  && Rest relation.
*-- end of lfInsHisto.

*!*************************************************************
*! Name      : lpDebDeal
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Age debits according to aging type (By Date OR By Terms) also fill memory variables.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpDebDeal
*!*************************************************************
*!
PROCEDURE lpDebDeal

*-- if Age By Date
IF lcAgeType = 'D'
  =lfUpdtAge('D',ldRpEndDat - TRANDATE)  && Update Date ages

ELSE  && else if Age by Terms

   ldDueDate = IIF(EMPTY(DUEDATE),TRANDATE+lnRPDay1,DUEDATE)
  =lfUpdtAge('D',ldRpEndDat - ldDueDate)  && Update Term ages
ENDIF  && end if Age By Date
  
*-- if charge back Charge back equal transaction amount
IF TRANTYPE= '3'

  *B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[Start]
  *m.ChgBack = Amount
  m.ChgBack = IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency) OR ;
              EMPTY(CCURRCODE) OR (Amount = 0),Amount,gfAmntDisp(Amount,lcRpCurr,ldRpExDate,lcRpTmpNam))
  *B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[End]
ENDIF

* B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[Start]
*STORE AMOUNT TO m.TotAge , m.NetBal  && Save total age and net balance per transaction
m.TotAge = IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency) OR EMPTY(CCURRCODE) OR ;
              (AMOUNT = 0),AMOUNT, gfAmntDisp(AMOUNT,lcRpCurr,ldRpExDate,lcRpTmpNam))
                 
m.NetBal = IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency) OR EMPTY(CCURRCODE) OR ;
              (AMOUNT = 0),AMOUNT, gfAmntDisp(AMOUNT,lcRpCurr,ldRpExDate,lcRpTmpNam))
           
* B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[End]

*-- if it is invoice and find it.
IF TRANTYPE = '1' AND SEEK(DEBIT.TRAN,'INVHDR')
  m.cDivision = INVHDR.CDIVISION  && invoice division.
  
  *E304097,1 SAH 30/12/2018 modify this program as the Payment Terms code of Customer is exported to  AR aging report excel not the Invoice or Credit Memo related Payment term [start]
  m.TRAN_PAYMENT_TERM= gfCoddes(INVHDR.cTermCode,'CTERMCODE')
  *E304097,1 SAH 30/12/2018 modify this program as the Payment Terms code of Customer is exported to  AR aging report excel not the Invoice or Credit Memo related Payment term [end]
ENDIF
*-- end of lpDebDeal.

*!*************************************************************
*! Name      : lpCreDeal
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Age Credits also fill memory variables.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpCreDeal
*!*************************************************************

PROCEDURE lpCreDeal

=lfUpdtAge('C',ldRpEndDat - TRANDATE) && Update Date ages

* B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[Start]
*STORE AMOUNT TO m.OpenCr , m.NetBal  && Save open credit and net balance per transaction

m.NetBal = IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency) OR EMPTY(CCURRCODE) OR ;
              (AMOUNT = 0),AMOUNT, gfAmntDisp(AMOUNT,lcRpCurr,ldRpExDate,lcRpTmpNam))
           
m.OpenCr = IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency) OR EMPTY(CCURRCODE) OR ;
              (AMOUNT = 0),AMOUNT, gfAmntDisp(AMOUNT,lcRpCurr,ldRpExDate,lcRpTmpNam))
           
* B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[End]

*-- if return invoice and return merchandise module installed and find this return.
*! B609596,1 MMT 05/30/2011 Chnage the filteration of Sales rep. to be on INVHDR and RETHDR Fields[Start]
*!*	IF ('RM' $ oAriaApplication.CompanyInstalledModules) AND !USED('RETHDR')
*!*	  *B607796,1 WAM 10/08/2006 Open rethdr file if not opened from the right path
*!*	  *=gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDR','SH')
*!*	  =gfOpenFile(oAriaApplication.DataDir+'RETHDR',oAriaApplication.DataDir+'RETHDR','SH')
*!*	  *B607796,1 WAM 10/08/2006 (End)
*!*	ENDIF 
*IF TRANTYPE = '0' AND ('RM' $ oAriaApplication.CompanyInstalledModules) AND SEEK(CREDIT.TRAN,'RETHDR')
IF TRANTYPE = '0' AND ('RM' $ oAriaApplication.CompanyInstalledModules) AND gfSEEK(CREDIT.TRAN,'RETHDR')
*! B609596,1 MMT 05/30/2011 Chnage the filteration of Sales rep. to be on INVHDR and RETHDR Fields[END]
  m.cDivision = RETHDR.CDIVISION  && Return division.
  
  *E304097,1 SAH 30/12/2018 modify this program as the Payment Terms code of Customer is exported to  AR aging report excel not the Invoice or Credit Memo related Payment term [start]
  m.TRAN_PAYMENT_TERM= gfCoddes(RETHDR.cTermCode,'CTERMCODE')
  *E304097,1 SAH 30/12/2018 modify this program as the Payment Terms code of Customer is exported to  AR aging report excel not the Invoice or Credit Memo related Payment term [end]
ENDIF
*-- end of lpCreDeal.

*!*************************************************************
*! Name      : lfUpdtAge
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Age Credits also fill memory variables.
*! Note      : I use Age fields (Current, Age30 , 60 , 90 , 120) to age both
*!           : by date or by terms to save disk usage and also simplify print
*!           : forms to print from one field instead of use command like
*!           : IIF(lcAgeType='D',Age30,TerAge30) , and we must all know that two situations
*!           : does not occur at the same time.
*!*************************************************************
*! Passed Parameters  : 1- Age Debits(D)/Credits(C)
*!                    : 2- Age Days
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfUpdtAge()
*!*************************************************************
*!
FUNCTION lfUpdtAge
PARAMETERS lcUpdtTyp , lnAgeDays

*-- Intializing variables that calculates data from transaction files. [begin]
STORE 0.00 TO m.Current , m.Age30 , m.Age60 , m.Age90 , m.Age120 ,;
             m.OpenCr,m.ChgBack , m.Totage , m.NetBal , m.Age00 
              
STORE '' TO m.Region , m.cDivision , m.SalesRep , m.cTermCode , m.cPastOnly
*-- Intializing variables that calculates data from transaction files. [End  ]

*-- if no parameter passed (i.e. want to intializing only) then return
*-- does occur in this program but it may be.
IF TYPE('lcUpdtTyp') $ 'UL'
  RETURN
ENDIF

*-- lcAges : Varaible Hold Field description (Age00, 30 , 60 , 90 , and 120)
*-- if Debit / History
IF lcUpdtTyp = 'D'

  *-- if Age By Date.
  IF lcAgeType = 'D'
    lcAges = 'm.Age'                                                    +;
      IIF(lnAgeDays >= lnRpDay4,'120',IIF(lnAgeDays >= lnRpDay3 ,'90'   ,;
      IIF(lnAgeDays >= lnRpDay2 ,'60' ,IIF(lnAgeDays >= lnRpDay1 ,'30','00'))))      

  ELSE  && else if Age By Terms.

    lcAges = 'm.Age'                                                      +;
      IIF(lnAgeDays >= lnRpDay3+1,'120',IIF(lnAgeDays >= lnRpDay2+1 ,'90' ,;
      IIF(lnAgeDays >= lnRpDay1+1 ,'60' ,IIF(lnAgeDays >= 1 ,'30','00'))))

  ENDIF  && end if Age By Date.

ELSE  && else if Credit / History

  lcAges = 'm.Age'                                                      +;
  IIF(lnAgeDays >= 120,'120',IIF(lnAgeDays >= 90 ,'90' ,;
  IIF(lnAgeDays >= 60 ,'60' ,IIF(lnAgeDays >= 30 ,'30','00'))))

ENDIF  && end if Debit / History

*! B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[Start]
*&lcAges = Amount       && Fill Age variable  
&lcAges  = IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency) OR EMPTY(CCURRCODE) OR ;
              (Amount = 0) ,Amount,gfAmntDisp(Amount,lcRpCurr,ldRpExDate,lcRpTmpNam))    
IF lcRpReport <> 'H'
  llPrnCrdDt = llRpCrdDet AND llRPAgeCrd        
  &lcAges = IIF((!llPrnCrdDt AND NetBal < 0) OR Amount = 0 ,0,TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR ;
                (CCURRCODE=oAriaApplication.BaseCurrency) OR EMPTY(CCURRCODE),Amount , ;
                 gfAmntDisp(Amount,lcRpCurr,ldRpExDate,lcRpTmpNam)), IIF(llRpDec,'99999999.99','99999999999')))
            
  &lcAges = VAL(&lcAges)
ENDIF 
*! B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[Start]

m.Current = IIF(m.Age00 = 0 , 0 , m.Age00)  && because there is no Age00 field (it's Current)

*-- end of lfUpdtAge.

*!*************************************************************
*! Name      : lpInsRecrd
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert both transaction and total records.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpInsRecrd
*!*************************************************************
*!
PROCEDURE lpInsRecrd
PRIVATE lnPastAges , lnNetBal
STORE 0 TO lnPastAges , lnNetBal

*-- Evaluate Customer file memory variables. [Begin]
m.Region     = Customer.Region
m.cTermCode  = Customer.cTermCode
m.cCont_Code = Customer.cCont_Code
m.SalesRep   = Customer.SalesRep

*! B609748,1 MMT 11/27/2011 A/R Aging Export to Excel needs to have the customer name[Start]
M.BTNAME = CUSTOMER.BTNAME 
M.PHONE1 = TRANSFORM(CUSTOMER.PHONE1,'@R '+ gfPhoneTem()) 
M.PHONE2 = TRANSFORM(CUSTOMER.PHONE2,'@R '+ gfPhoneTem()) 
M.caddress1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , 1) 
M.caddress2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , 1)
M.caddress3 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , 1))
M.caddress4 = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , 1)
M.caddress5 = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , 1)
M.CRLIMIT = CUSTOMER.CRLIMIT 
M.keeper=  Customer.keeper 
M.factacct = CUSTOMER.factacct 
M.STATUS = ALLTRIM(IIF(Customer.Status='A',LANG_Araging_Actv ,IIF(Customer.Status='P',LANG_Araging_Potential ,IIF(Customer.Status='H',LANG_Araging_Hold ,LANG_Araging_Cncl)))) 
*! B609748,1 MMT 11/27/2011 A/R Aging Export to Excel needs to have the customer name[End]

*-- Evaluate Customer file memory variables. [Begin]

m.cCodeDescr = gfCoddes(m.cTermCode,'CTERMCODE',.T.)

INSERT INTO (lcTempAge) FROM MEMVAR         && Insert Transaction Record
SELECT (lcTempAge)
REPLACE cTempKey WITH EVALUATE(lcReplExpr)  && Replace key field with approparate value.

*-- if Debit, Credit, or History record only (Not Post dated checks record).
IF EMPTY(cTranType)

  *-- if Multi currency and currency does not equal base currency.
  IF llMultCur AND (cCurrCode <> oAriaApplication.BaseCurrency )

    = lfBaseVal(@lnPastAges , @lnNetBal)  && Evaluate Past Ages and net balance in base currency.

  ELSE  && else if Single currency or currency equal base currency.

    lnPastAges = IIF(TotAge > 0 AND Current = 0,TotAge,0)  && Past age
    lnNetBal   = NetBal                                    && Net balance

  ENDIF  && end if Multi currency and currency does not equal base currency.

  *-- if Find this account in Temporary totals file.
  IF SEEK(Account,lcTempAcc)

    *-- Accomulate both past due ages and net balance in total file.
    REPLACE &lcTempAcc..nAges WITH   &lcTempAcc..nAges + lnPastAges ,;
            &lcTempAcc..nNetBal WITH &lcTempAcc..nNetBal + lnNetBal

  ELSE  && else if this account not found before.

    *-- Add new record in temporary total files.
    WAIT WINDOW LANG_Araging_Clct + Account NOWAIT
    m.Account = Account
    m.nAges   = lnPastAges
    m.nNetBal = lnNetBal
    INSERT INTO (lcTempAcc) FROM MEMVAR  && insert new record in account totals file.

  ENDIF  && end if Find this account in Temporary totals file.

ENDIF  && end if Debit, Credit, or History record.
*-- end of lpInsRecrd.

*!*************************************************************
*! Name      : lfBaseVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/17/98
*! Purpose   : Calculate Currency unit, rate, signs and Evaluate amount values
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfBaseVal()
*!*************************************************************
*
FUNCTION lfBaseVal
PARAMETERS lnBaseVal1 , lnBaseVal2
PRIVATE lcAExRSin , lcAUntSin , lnCurrRate , lnCurrUnit

lcAUntSin   = '*'        && Variable to hold unit sign.
lcAExRSin   = '*'        && Variable to hold exchange rate sign.

lcAExRSin  = gfGetExSin(@lcAUntSin, cCurrCode)  && Currency sign.


lnCurrRate  = nExRate          && Variable to hold current rate
lnCurrUnit  = nCurrUnit        && Variable to hold current unit


lnBaseVal1 = IIF(TotAge > 0 AND Current = 0,TotAge,0) &lcAExRSin lnCurrRate &lcAUntSin lnCurrUnit
lnBaseVal2 = NetBal &lcAExRSin lnCurrRate &lcAUntSin lnCurrUnit
*-- end of lfBaseVal.

*!*************************************************************
*! Name      : lfPrnCurDt
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 03/20/1999
*! Purpose   : Print under Current field in Detail form .FRX
*!           : and this is because Expression is too long and .FRX expression does not fit it.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfPrnCurDt()
*!*************************************************************
*!
FUNCTION lfPrnCurDt
PRIVATE lcPrintVal
lcPrintVal = ''
*-- if user want to age credits.
IF llRpAgeCrd

  *-- if (User Want to Print Credit details OR it's +Ve Transaction) AND
  *--    Current not equal zero.
  IF (llRpCrdDet OR NetBal >= 0) AND current <> 0

    IF llRpDec
      *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
      *lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ),Current,gfAmntDisp(Current,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999.99')
      lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ) OR EMPTY(CCURRCODE),Current,Current),'99999999.99')
      *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
    ELSE
      *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
      *lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ),Current,gfAmntDisp(Current,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999999')
      lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ) OR EMPTY(CCURRCODE),Current,Current),'99999999999')
      *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
    ENDIF
  ENDIF

ELSE  && User does not want to Age Credits

  *-- if it is credit transaction.
  IF OpenCr < 0

    *-- if Include credit detail.
    IF llRpCrdDet
      IF llRpDec
        *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
        *lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ),OpenCr,gfAmntDisp(OpenCr,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999.99')
        lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ) OR EMPTY(CCURRCODE),OpenCr,OpenCr),'99999999.99')
        *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
      ELSE
        *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
        *lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ),OpenCr,gfAmntDisp(OpenCr,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999999')
        lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ) OR EMPTY(CCURRCODE),OpenCr,OpenCr),'99999999999')
        *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
      ENDIF
    ENDIF
  ELSE  && else it is debit transaction, in current field.
    IF llRpDec
      *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
      *lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ),Current,gfAmntDisp(Current,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999.99')
      lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ) OR EMPTY(CCURRCODE),Current,Current),'99999999.99')
      *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
    ELSE
      *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
      *lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ),Current,gfAmntDisp(Current,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999999')
      lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=oAriaApplication.BaseCurrency ) OR EMPTY(CCURRCODE),Current,Current),'99999999999')
      *_*B608842,1 HES 04/07/2009 Fix bug of Aging report Export to Excel not converting currency values[T20090311.0003]
    ENDIF
  ENDIF        && end if it is credit transaction.
ENDIF          && end if user want to age credits.

*-- if Value of return value = 0. [Begin]
IF !EMPTY(lcPrintVal) AND VAL(lcPrintVal) = 0
  lcPrintVal = ''
ENDIF
*-- if Value of return value = 0. [End  ]

RETURN lcPrintVal
*-- end of lfPrnCurDt.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 03/20/1999
*! Purpose   : Close any opened files if user press OG <Close> Button
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*!
FUNCTION lfClearRep
*-- Rise llOGFltCh flag to recollect data next time preview or run because this fn called
*-- if user press <Reset> or Clear Read.
llOGFltCh = .T.

*-- Close post dated checks file.
IF USED('POSTDCHQ')
  USE IN POSTDCHQ
ENDIF

*-- Close Currency file.
IF USED('SYCCURR')
  USE IN SYCCURR
ENDIF

*-- Close Temporary Cursors.
IF USED(lcTempAge)
  USE IN (lcTempAge)
ENDIF

IF USED(lcTempRev)
  USE IN (lcTempRev)
ENDIF

IF USED(lcTempAcc)
  USE IN (lcTempAcc)
ENDIF
*-- end of lfClearRep.
***************************************************
*! Name      : lfFillARY
*! Developer : HEBA FATHI (HFK)
*! Date      : 12/22/2003
*! Purpose   : Function to fill Terms,Division,Region,Ship via,
*!           : Special Instruction,Customer classification.
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************
FUNCTION lfFillARY

DIMENSION laRpSource[3],laRpTarget[1,1]
STORE '' TO laRpSource,laRpTarget

*!*	laRpSource[1] = 'Active'
*!*	laRpSource[2] = 'Hold'
*!*	laRpSource[3] = 'Cancelled'
laRpSource[1] = LANG_Araging_Actv 
laRpSource[2] = LANG_Araging_Hold 
laRpSource[3] = LANG_Araging_Cncl 

*--End of lfFillARY.
*:*************************************************************
*: Name        : lfItmPos
*: Developer   : Abdou Elgendy. [ABD]
*: Date        : 05/26/2002
*: Purpose     : Evaluate fixed filter position within array.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfItmPos()
*:*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(loOGScroll.laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*:*************************************************************
*: Name        : lfSeTSRep
*: Developer   : Abdou Elgendy. [ABD]
*: Date        : 05/26/2002
*: Purpose     : Go top in Sales Rep file.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfSetSRep()
*:*************************************************************

FUNCTION lfSetSRep
PARAMETERS OpGrdParm
DO CASE
  
  CASE OpGrdParm = 'S'
    SELECT SalesRep
    SET ORDER TO TAG SalesRep
    GO TOP
  
  CASE OpGrdParm = 'R'
    SELECT SalesRep
    SET ORDER TO
ENDCASE
*-- End of lfSetSRep.
FUNCTION LFSETGL
PARAMETERS LCPARAM

DO CASE

  CASE LCPARAM  = 'S'
  CASE LCPARAM  = 'R'
  OTHERWISE 
  =lfvGLAccnt()

ENDCASE

*!******************************************************
*!
*!      Function : lfSetGLMsk
*!
*!******************************************************
*! E300697,1 YMA 07/13/97
*! This function is to load the GL account mask and width
*! to variables that will be used in all the screens that
*! display any GL account.
*!******************************************************

FUNCTION lfSetGLMsk
PRIVATE lnAlias

lnAlias    = SELECT(0)
llGlLink   = (ALLTRIM(gfGetMemVar('M_LINK_GL',oAriaApplication.ActiveCompanyID)) = 'Y')
lcLinkWith = (ALLTRIM(gfGetMemVar('M_GL_VERS',oAriaApplication.ActiveCompanyID)))
IF llGlLink
  IF lcLinkWith $ "AO"
    USE (oAriaApplication.SysPath + "SYCCOMP") IN 0 AGAIN ALIAS CompFile ORDER CCOMP_ID
    llNoThing  = SEEK(oAriaApplication.ActiveCompanyID, "CompFile")
    lcPthToUse = oAriaApplication.DataDir
    IF !EMPTY(CompFile.cCompPrnt)
      lcPrntComp = CompFile.cCompPrnt
      llNoThing  = SEEK(lcPrntComp, "CompFile")
      lcPthToUse = ALLTRIM(CompFile.cCom_DDir)
    ENDIF
    USE IN CompFile
    USE (lcPthToUse + "ACCOD") IN 0 AGAIN ALIAS CodeStr ORDER AccSegNo
    SELECT CodeStr
    GOTO TOP
    lcRep     = IIF(lcLinkWith = "A", "9", "X")
    lcAcMask  = "X" + SUBSTR(STRTRAN(ALLTRIM(cAcsMask),"#",lcRep),2)
    USE IN CodeStr
    IF lcLinkWith = "A" AND !USED('lcLinkChar')
      USE (lcPthToUse + "GLACCHAR") IN 0 ORDER ACCTCODE AGAIN ALIAS lcLinkChar
    ENDIF
    lnAcLen    = LEN(ALLTRIM(lcAcMask))
    *-lcGlFld    = "cAcctCode:"+ALLTRIM(STR(lnAcLen))+":H='Account Code',"+"cAccNlDes:65:H='Description'"
    lcGlFld    = "cAcctCode: 25 :H='Account Code',"+"cAccNlDes:65:H='Description'"
    lcAcntFld  = "cAcctCode"
    lcAcntDesF = "cAccNlDes"
  ELSE
    lcLinkComp = ALLTRIM(gfGetMemVar('M_GL_CO'  , oAriaApplication.ActiveCompanyID))
    lcSBTGLDir = ALLTRIM(gfGetMemVar('M_SYS_DIR', oAriaApplication.ActiveCompanyID))
    lcAcntChrt = lcSBTGLDir + "\GLDATA\GLACNT" + lcLinkComp + ".DBF"
    lcAcntStrc = lcSBTGLDir + "\GLDATA\GLSTRU" + lcLinkComp + ".DBF"
    lcAcMask   = SPACE(0)
    USE (lcAcntStrc) IN 0 AGAIN ALIAS AcntStrc ORDER SegID
    SELECT AcntStrc
    SCAN FOR SegLen > 0
      lcAcMask = lcAcMask + IIF(EMPTY(lcAcMask),"","-") + ALLTRIM(SegMask)
    ENDSCAN
    USE IN AcntStrc
    IF !USED("lcLinkChar")
      USE (lcAcntChrt) IN 0 AGAIN ALIAS lcLinkChar ORDER GlAcnt
    ENDIF
    lnAcLen    = LEN(ALLTRIM(lcAcMask))
    *-lcGlFld = "glAcnt:"+ALLTRIM(STR(lnAcLen))+":H='Account Code',"+"glDesc:53:H='Description'"
    lcGlFld = "glAcnt: 25 :H='Account Code',"+"glDesc:53:H='Description'"
    lcAcntFld  = "glAcnt"
    lcAcntDesF = "glDesc"
  ENDIF
ENDIF
SELECT (lnAlias)
*-*-*-*-*-
FUNCTION lfvTerm

STORE .F. TO llFlgMovr
STORE .T. TO llogfltch           && Rise this flag to recollect the data.
STORE SPACE(0) TO lcRpStatFr

*--= gfMover(@laRpSource,@laRpTarget,'Customer Status ',.T.,'')

* B123663,1 SMM Change gfMover to lfOGMover
*-- = gfMover(@laRpSource,@laRpTarget,LANG_Araging_CustStat ,.T.,'')
= lfOGMover(@laRpSource,@laRpTarget,LANG_Araging_CustStat ,.T.,'')
* B123663,1 SMM End

*--IF ASCAN(laRpTarget,"Active") > 0       && Case for the Active customer
IF ASCAN(laRpTarget,LANG_Araging_Actv ) > 0       && Case for the Active customer
  lcRpStatFr = "A"
  llFlgMovr = .T.
ENDIF

*--IF ASCAN(laRpTarget,"Hold") > 0         && Case for the Hold customer
IF ASCAN(laRpTarget,LANG_Araging_Hold ) > 0         && Case for the Hold customer
  lcRpStatFr = lcRpStatFr + "H"
  llFlgMovr = .T.
ENDIF

*--IF ASCAN(laRpTarget,"Cancelled") > 0    && Case for Cancelled the customer
IF ASCAN(laRpTarget,LANG_Araging_Cncl) > 0    && Case for Cancelled the customer
  lcRpStatFr = lcRpStatFr + "X"
  llFlgMovr = .T.
ENDIF

IF !llFlgMovr
  lcRpStatFr = "AHX"
ENDIF
*--End of lfvTerm.
*--B99999,1 MMT 02/16/2006 , add a function to refresh the status option[Start]
*!************************************************************
*! Name      : RefreshStatus
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/16/2006
*! Purpose   : function to refresh the status option
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
FUNCTION RefreshStatus
LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 
*--B99999,1 MMT 02/16/2006 , add a function to refresh the status option[End]

*! B609922,1 MMT 05/17/2012 AR aging report does not  have subtotal for currecny[Start]
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mariam Mazhar (MMT)
*! Date      : 05/17/2012
*! Purpose   : Fill Currency arrays
*!*************************************************************
FUNCTION lfEvalSegs
IF llMultCur
  DIMENSION laCurrVal[1,1]

  IF !USED('SYCCURR')
    =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1],laCurrSmbl[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrSmbl[lnI,1] = ALLTRIM(PADR(CCURRSMBL,3))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF
*! B609922,1 MMT 05/17/2012 AR aging report does not  have subtotal for currecny[END]