*:***************************************************************************
*: Program file  : ARAGINGM.PRG
*: Program desc. : A/R aging report for GMA
*: Module        : Account Rec. (AR)
*: Developer     : Heba Fathi (HFK)
*: Tracking      : C039269
*: Date          : 05/24/2005
*:***************************************************************************
*: Calls : 
*: Global Fn.    : gfPhoneTem,gfTempName,gfGetAdr,gfDispRe,gfCodDes,gfGetMemVar,gfBrows.
*:***************************************************************************
*: This Program is due to E300880 ...
*: Modifications:
*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [T20080107.0013]
*:***************************************************************************
*=================================  Begin ===============================*
*=========================  Report Program Code =========================*
*========================================================================*
*-- lcStTime   : Variable to hold the start Time
*-- llCurrGrp  : Flag .T. if multi currency and print in foreign and not sort by currency.
*-- lcReplExpr : Replace key field expression instead of use index command on huge file
*-- lcAcctFilt : Variable to hold filter on account
*-- lcSorted   : Printed Sort By expression
*-- lcGroup    : variable hold Short Form groups.
*-- lcInnGrp   :
*-- lcCodeDesc :
LCSTTIME = TIME()
LLCURRGRP = LLMULTCUR .AND. (LCRPCURR='F') .AND. (LCRPAGSORT<>'C')
STORE '' TO LCACCTFILT, LCSORTED, LCGROUP, LCINNGRP, LCCODEDESC
LCREPLEXPR = LFGETREPLC()
= GFDOTRIGER('ARAGING',PADR('CHECKREG', 10))
IF LLOGFLTCH
IF  .NOT. USED(LCTEMPAGE) .OR. (RECCOUNT(LCTEMPAGE)>0)
= LFWORKFILE()
ENDIF
LLCHSORTBY = .F.
= LFCOLLECT()
ENDIF
IF RECCOUNT(LCTEMPAGE)=0
= GFMODALGEN('TRM00052B00000','DIALOG')
RETURN
ENDIF
IF LLRPSHOWPA
LCACCTFILT = 'nAges > 0'
ENDIF
IF LCRPBALANC='D'
LCACCTFILT = IIF(EMPTY(LCACCTFILT), '', LCACCTFILT+' AND ')+'nNetBal >= lnRpBalanc'
ENDIF
IF LCRPBALANC='C'
LCACCTFILT = IIF(EMPTY(LCACCTFILT), '', LCACCTFILT+' AND ')+'nNetBal <= lnRpBalanc'
ENDIF
IF LCRPREPORT='H'
LCACCTFILT = IIF(EMPTY(LCACCTFILT), '', LCACCTFILT+' AND ')+'nNetBal <> 0'
ENDIF
IF LCRPBALANC='B' .AND. LNRPBALANC<>0
IF LNRPBALANC<0
LCACCTFILT = IIF(EMPTY(LCACCTFILT), '', LCACCTFILT+' AND ')+'nNetBal < lnRpBalanc'
ELSE
LCACCTFILT = IIF(EMPTY(LCACCTFILT), '', LCACCTFILT+' AND ')+'nNetBal > lnRpBalanc'
ENDIF
ENDIF
IF  .NOT. EMPTY(LCACCTFILT)
SELECT (LCTEMPACC)
SET FILTER TO &lcAcctFilt
GOTO TOP
IF EOF()
= GFMODALGEN('TRM00052B00000','DIALOG')
RETURN
ENDIF
ENDIF
SELECT (LCTEMPAGE)
IF LLCHSORTBY
LLCHSORTBY = .F.
REPLACE CTEMPKEY WITH EVALUATE(LCREPLEXPR) ALL
ENDIF
IF LCRPREPORT='H'
IF  .NOT. USED(LCTEMPREV)
DO LPANOTHALS
ENDIF
SELECT (LCTEMPAGE)
SET RELATION TO EVALUATE(LCREPLEXPR) INTO (LCTEMPREV)
IF LLMULTCUR
LCCURRSEC = "cCurrCode + '-' + IIF(SEEK(cCurrCode,'SycCurr'),PADR(SycCurr.CCURRDESC,9),SPACE(9))"
ELSE
LCCURRSEC = 'PADR(TRANSFORM(IIF(EMPTY(CUSTOMER.PHONE2), '+'CUSTOMER.PHONE1,CUSTOMER.PHONE2),gfPhoneTem()),13)'
ENDIF
LCTERMDATA = 'IIF(RECNO() <> RECNO(lcTempRev) OR '+"(lnGrpAge=0 AND lnGrpCre=0),'',"+"PADR(CUSTOMER.BTNAME,10) + ' ' + "+LCCURRSEC+" + ' ' +"+"Customer.SALESREP + ' ' + "+"PADR(gfCoddes(cTermCode,'CTERMCODE',.T.),25))"
LCPOSTTEXT = "IIF(RECNO() <> RECNO(lcTempRev) OR lnGrpPost=0 ,'',"+'PADR(CUSTOMER.BTNAME,10) + '+"',   Totals Post Dated Cheques = ' + "+'TRANSFORM(lnGrpPost,"99999999.99"))'
ELSE
LLPRNCRDDT = LLRPCRDDET .AND. LLRPAGECRD
IF LLRPACCPAG
IF  .NOT. USED(LCOGTMPFOR)
LCSVERRHAN = ON('ERROR')
ON ERROR
USE &gcWorkDir.&lcOGTmpForm..FRX IN 0 ORDER 0 EXCLUSIVE
ON ERROR &lcSvErrHan
ENDIF
SELECT (LCOGTMPFOR)
LOCATE FOR OBJTYPE=9 .AND. OBJCODE=3 .AND. 'ACCOUNT'$UPPER(ALLTRIM(EXPR))
REPLACE PAGEBREAK WITH .T.
USE IN (LCOGTMPFOR)
ENDIF
ENDIF
SELECT (LCTEMPAGE)
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER, ACCOUNT INTO (LCTEMPACC) ADDITIVE
IF LCAGETYPE='D'
LCAGEHD30 = 'Over '+ALLTRIM(STR(LNRPDAY1))
LCAGEHD60 = 'Over '+ALLTRIM(STR(LNRPDAY2))
LCAGEHD90 = 'Over '+ALLTRIM(STR(LNRPDAY3))
LCAGEHD120 = 'Over '+ALLTRIM(STR(LNRPDAY4))
ELSE
LCAGEHD30 = '1 - '+ALLTRIM(STR(LNRPDAY1))
LCAGEHD60 = ALLTRIM(STR(LNRPDAY1+1))+' - '+ALLTRIM(STR(LNRPDAY2))
LCAGEHD90 = ALLTRIM(STR(LNRPDAY2+1))+' - '+ALLTRIM(STR(LNRPDAY3))
LCAGEHD120 = 'Over '+ALLTRIM(STR(LNRPDAY3+1))
ENDIF
LCEDTIME = TIME()
LNINTERVAL = LFCOLLTIME(LCSTTIME,LCEDTIME)
WAIT WINDOW NOWAIT 'Selected '+ALLTRIM(STR(RECCOUNT(LCTEMPACC)))+' Customer(s), '+ALLTRIM(STR(RECCOUNT()))+' Transaction(s) in '+ALLTRIM(STR(LNINTERVAL, 6, 2))+' Seconds...'
IF LCRPREPORT='H'
REPLACE ALL &lcTempAge..cTranType WITH "" FOR &lcTempAge..cTranType = 'H'
ENDIF
STORE SPACE(0) TO LCACNTPAD
LLRPINVNOT = IIF(TYPE('llRpInvNot')='U', .F., LLRPINVNOT)
IF LLRPINVNOT
SELECT (LCTEMPAGE)
GOTO TOP
SCAN
IF LLRPCUSNOT .AND.  .NOT. EMPTY(LCACNTPAD)
IF lcAcntPad # &lcTempAge..ACCOUNT
SKIP -1
REPLACE &lcTempAge..llFlgPad WITH .T.
SKIP
ENDIF
ENDIF
lcAcntPad = &lcTempAge..ACCOUNT
IF &lcTempAge..TRANTYPE = "1" AND  SEEK('C'+ &lcTempAge..TRAN,'NOTEPAD') .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*'
REPLACE &lcTempAge..mNotes WITH NOTEPAD.mNotes
ENDIF
ENDSCAN
IF EOF(LCTEMPAGE)
SKIP -1
REPLACE &lcTempAge..llFlgPad WITH .T.
ENDIF
ENDIF
GOTO TOP


loogScroll.cCROrientation = 'P'
IF EMPTY(LCACCTFILT)
DO GFDISPRE WITH EVALUATE('lcRpName')
ELSE
DO GFDISPRE WITH EVALUATE('lcRpName'), 'FOR !EOF(lcTempAcc)'
ENDIF
WAIT CLEAR
SET RELATION TO
IF USED(LCTEMPREV)
USE IN (LCTEMPREV)
ENDIF
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
*
PROCEDURE LPANOTHALS
PRIVATE LCFULLPATH, LCCURDIR
LCFULLPATH = SET('FULLPATH')
SET FULLPATH ON
LCCURDIR = DBF()
SET FULLPATH &lcFullPath
USE IN 0 (LCCURDIR) AGAIN ALIAS (LCTEMPREV)
SELECT (LCTEMPREV)
SET ORDER TO (LCTEMPREV)
*!*************************************************************
*! Name        : lfpostChq
*! Developer   : Sameh Saiid (SSE)
*! Date        : 02/22/1999
*! Purpose     : get total POST DATED CHECKS for each ACCOUNT
*!*************************************************************
*!
FUNCTION LFPOSTCHQ
SELECT (LCPRTFILE)
LCACCOUNT = ACCOUNT
SELECT POSTDCHQ
IF SEEK(&lcPrtFile..ACCOUNT)
SUM REST WHILE ACCOUNT=LCACCOUNT AMOUNT TO LNPDCHCKS
ELSE
LNPDCHCKS = 0.00 
ENDIF
RETURN LNPDCHCKS
*
FUNCTION  LFVACCT
PRIVATE LCOBJNAM, LCOBJVAL, LLOBJRET
LCOBJNAM = SYS(18)
LCOBJVAL = EVALUATE(SYS(18))
IF  .NOT. EMPTY(LCOBJVAL) .AND.  .NOT. SEEK('M'+LCOBJVAL, 'CUSTOMER')
LLBROWSE = .T.
XACCOUNT = LCOBJVAL
DO CUSBROWM WITH XACCOUNT
LCOBJVAL = XACCOUNT
LLBROWSE = .F.
ENDIF
&lcObjNam = lcObjVal

*!*************************************************************
*! Name      : lfvReport
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the report form to enable 
*!             and disable some other fields in the option grid
*!*************************************************************
*!
FUNCTION LFVREPORT
IF LCRPREPORT='H'
LCRPNAME = 'ARAGINHM'
= LFREPPLTFR(LCRPNAME)
STORE .F. TO LLRPACCPAG, LLRPCRDDET, LLRPCUSNOT
ELSE
IF LCRPNAME='ARAGINHM'
LCRPNAME = 'ARAGINGM'
= LFREPPLTFR(LCRPNAME)
STORE (LCRPREPORT$'DS') TO LLRPCRDDET, LLRPAGECRD
ENDIF
ENDIF
CLEAR READ
*!*************************************************************
*! Name      : lfwGrid
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the when function of the option grid
*!*************************************************************
*!
FUNCTION  LFWGRID
IF TYPE('lcBuild')='N'
LCBUILD = 'OK'
LCPRTFAC = 'FACTORED AND NON FACTORED INVOICES'
= LFFILTRNAR()
ENDIF
=  .NOT. LLRPCRDDET .AND. LFVINCDET()
*!*************************************************************
*! Name      : lfvInv
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the option grid to set the variable
*!             of factored invoices
*!*************************************************************
*! Example     : = lfvInv()
*!*************************************************************
*!
FUNCTION LFVINV
LCRPSPEFAC = IIF((LCRPFACTOR='F'), LCRPSPEFAC, SPACE(05))
CLEAR READ
*!*************************************************************
*! Name      : lfvBalance
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the option grid to set the variable
*!             of balance
*!*************************************************************
*!
FUNCTION  LFVBALANCE
DO CASE
CASE LCRPBALANC='C'
LCHEDBAL = 'Under'
LNRPBALANC = ABS(LNRPBALANC)*-1
CASE LCRPBALANC='D'
LCHEDBAL = 'Over'
LNRPBALANC = ABS(LNRPBALANC)
CASE LCRPBALANC='B'
LCHEDBAL = 'Balance Greater than'
IF LNRPBALANC<0
LNRPBALANC = ABS(LNRPBALANC)*-1
ELSE
LNRPBALANC = ABS(LNRPBALANC)
ENDIF
OTHERWISE
LCHEDBAL = SPACE(10)
LNRPBALANC = 0
ENDCASE
CLEAR READ
*!*************************************************************
*! Name      : lfvFactor
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate a specific factor
*!*************************************************************
*!
FUNCTION  LFVFACTOR
PRIVATE LNALIAS
LNALIAS = SELECT()
SELECT CUSTOMER
IF  .NOT. EMPTY(LCRPSPEFAC) .AND.  .NOT. SEEK('F'+LCRPSPEFAC)
LLNOTHING = LFGETFAC(LCRPSPEFAC)
ENDIF
SELECT (LNALIAS)
*
FUNCTION  LFVSALREP
PRIVATE LCOBJNAM, LCOBJVAL, LLOBJRET
SET ORDER TO SALESREP IN SALESREP
LCOBJNAM = SYS(18)
LCOBJVAL = EVALUATE(SYS(18))
IF  .NOT. EMPTY(LCOBJVAL) .AND.  .NOT. SEEK(LCOBJVAL, 'SALESREP')
LLBROWSE = .T.
XACCOUNT = LCOBJVAL
DO REPCHK WITH XACCOUNT
LCOBJVAL = XACCOUNT
LLBROWSE = .F.
ENDIF
&lcObjNam = lcObjVal
*!*************************************************************
*! Name      : llPrtSub
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the report the check when to print the subtotals
*!*************************************************************
*! Example     : = llPrtSub()
*!*************************************************************
*!
FUNCTION LLPRTSUB
IF EOF()
LLPRINTSUB = .T.
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
*!
FUNCTION  LFVDAY1
IF LNRPDAY1<=0
= GFMODALGEN('TRM40132B40011','ALERT')
LNRPDAY1 = LNOLDDAYS
ENDIF
IF LNRPDAY1>=LNRPDAY2
= GFMODALGEN('TRM40133B40011','ALERT','2nd|1st')
LNRPDAY1 = LNOLDDAYS
ENDIF
*!*************************************************************
*! Name      : lfwDays
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the old value of days buckets in a variable
*!*************************************************************
*!
FUNCTION  LFWDAYS
LNOLDDAYS = EVALUATE(OGSYS18())
*!*************************************************************
*! Name      : lfvDay2
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 2nd bucket
*!*************************************************************
*!
FUNCTION  LFVDAY2
IF LNRPDAY2<=LNRPDAY1
= GFMODALGEN('TRM40133B40011','ALERT','2nd|1st')
LNRPDAY2 = LNOLDDAYS
ENDIF
IF LNRPDAY2>=LNRPDAY3
= GFMODALGEN('TRM40133B40011','ALERT','3rd|2nd')
LNRPDAY2 = LNOLDDAYS
ENDIF
*!*************************************************************
*! Name      : lfvDay3
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 3rd bucket
*!*************************************************************
*!
FUNCTION  LFVDAY3
IF LNRPDAY3<=LNRPDAY2
= GFMODALGEN('TRM40133B40011','ALERT','3rd|2nd')
LNRPDAY3 = LNOLDDAYS
ENDIF
IF LNRPDAY3>=LNRPDAY4
= GFMODALGEN('TRM40133B40011','ALERT','3rd|2nd')
LNRPDAY3 = LNOLDDAYS
ENDIF
*!*************************************************************
*! Name      : lfvDay4
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 4th bucket
*!*************************************************************
*!
FUNCTION  LFVDAY4
IF LNRPDAY4<=LNRPDAY3
= GFMODALGEN('TRM40133B40011','ALERT','4th|3rd')
LNRPDAY4 = LNOLDDAYS
ENDIF
*!*************************************************************
*! Name      : lfGetDesc
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the description of codes
*!*************************************************************
*!
FUNCTION LFGETDESC
PRIVATE LCCODEDESC
LCCODEDESC = SPACE(01)
DO CASE
CASE LCGROUP='cTermCode'
LCCODEDESC = CTERMCODE+' '+PADR(GFCODDES(CTERMCODE,'CTERMCODE'), 23)
CASE LCGROUP='CDIVISION'
LCCODEDESC = CDIVISION+' '+PADR(GFCODDES(CDIVISION,'CDIVISION'), 23)
CASE LCGROUP='cCurrCode'
= SEEK(CCURRCODE, 'SYCCURR')
LCCODEDESC = CCURRCODE+' '+PADR(SYCCURR.CCURRDESC, 23)
CASE LCGROUP='Salesrep'
= SEEK(SALESREP, 'Salesrep')
LCCODEDESC = SALESREP.NAME
CASE LCGROUP='Region'
LCCODEDESC = REGION+' '+PADR(GFCODDES(REGION,'REGION'), 23)
CASE LCRPAGSORT='U'
= SEEK(CCONT_CODE, 'SYCINT')
LCCODEDESC = CCONT_CODE+' '+PADR(SYCINT.CCONT_DESC, 23)
ENDCASE
RETURN LCCODEDESC
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the description of Curr.
*!*************************************************************
*!
FUNCTION LFVCURDISP
= GFREPCUR(.T.,@LCRPCURR,@LDRPEXDATE,LCRPTMPNAM)
*!*************************************************************
*! Name      : lfGetRate
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the rate of Curr. in case of using multi curr.
*!*************************************************************
*!
FUNCTION LFGETRATE
PARAMETER LCFCURR
PRIVATE LCFCURR, LNFRATE
LNFRATE = 0
DO CASE
CASE LCRPCURR='D'
LNFRATE = GFCHKRATE('',LCFCURR,LDEXDATE,.F.,GCACT_COMP,.T.)
CASE LCRPCURR='U'
= SEEK(LCFCURR, LCRPTMPCUR)
lnfRate = &lcRpTmpCur..nExRate
ENDCASE
LNFRATE = IIF(LNFRATE=0, 1, LNFRATE)
RETURN LNFRATE
*!*************************************************************
*! Name      : lfRepName
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get salesrep name
*!*************************************************************
*!
FUNCTION LFREPNAME
PARAMETER LCREPCODE
PRIVATE LCREPCODE, LCREPNAME
LCREPNAME = IIF(SEEK(LCREPCODE, 'SALESREP'), SALESREP.NAME, SPACE(24))
RETURN LCREPNAME
*!*************************************************************
*! Name      : lfSumCre
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total credit for all the report
*!*************************************************************
*!
FUNCTION LFSUMCRE
PARAMETER LCACCOUNT, LCCURR
PRIVATE LNCREDIT, LNALIAS, LCCURR
LNALIAS = SELECT()
LNCREDIT = 0
IF SEEK(&lcPrtFile..ACCOUNT,'CREDIT')
SELECT CREDIT
SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT) FOR IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.)
IF LLMULTCUR .AND. LCRPCURR<>'F'
LCSIGN = GFGETEXSIN('',ALLTRIM(CREDIT.CCURRCODE))
LCSIGN = IIF(EMPTY(LCSIGN), '*', LCSIGN)
LNRATE = IIF(LCRPCURR='O', NEXRATE, LFGETRATE(CREDIT.CCURRCODE))
LNAMOUNT = IIF(LCSIGN='/', AMOUNT/LNRATE/NCURRUNIT, AMOUNT*LNRATE/NCURRUNIT)
ELSE
LNAMOUNT = AMOUNT
ENDIF
LNCREDIT = LNCREDIT+LNAMOUNT
ENDSCAN
ENDIF
IF SEEK(&lcPrtFile..ACCOUNT,'ARHIST') AND llRPKeyOff
SELECT ARHIST
SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT) FOR (HISTDATE > ldRpEndDat) .AND. (TRANDATE <= ldRpEndDat) and TRANTYPE $ '0456' AND IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.) AND IIF(!EMPTY(lcRpGlAcc),cArGlAcc = lcRpGlAcc,.T.)
IF LLMULTCUR .AND. LCRPCURR<>'F'
LCSIGN = GFGETEXSIN('',ALLTRIM(ARHIST.CCURRCODE))
LCSIGN = IIF(EMPTY(LCSIGN), '*', LCSIGN)
LNRATE = IIF(LCRPCURR='O', NEXRATE, LFGETRATE(ARHIST.CCURRCODE))
LNAMOUNT = IIF(LCSIGN='/', AMOUNT/LNRATE/NCURRUNIT, AMOUNT*LNRATE/NCURRUNIT)
ELSE
LNAMOUNT = AMOUNT
ENDIF
LNCREDIT = LNCREDIT+LNAMOUNT
ENDSCAN
ENDIF
SELECT (LNALIAS)
RETURN LNCREDIT
*!*************************************************************
*! Name      : lfCharge
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total charge back for all the report
*!*************************************************************
*!
FUNCTION LFCHARGE
PARAMETER LCACCOUNT, LCCURR
PRIVATE LNDEBIT, LNALIAS, LNAR, LCCURR
LNALIAS = SELECT()
LNDEBIT = 0
IF SEEK(&lcPrtFile..ACCOUNT,'DEBIT')
SELECT DEBIT
SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT) FOR (IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.) AND TRANTYPE <> '3')
IF LLMULTCUR .AND. LCRPCURR<>'F'
LCSIGN = GFGETEXSIN('',ALLTRIM(DEBIT.CCURRCODE))
LCSIGN = IIF(EMPTY(LCSIGN), '*', LCSIGN)
LNRATE = IIF(LCRPCURR='O', NEXRATE, LFGETRATE(DEBIT.CCURRCODE))
LNAMOUNT = IIF(LCSIGN='/', AMOUNT/LNRATE/NCURRUNIT, AMOUNT*LNRATE/NCURRUNIT)
ELSE
LNAMOUNT = AMOUNT
ENDIF
LNDEBIT = LNDEBIT+LNAMOUNT
ENDSCAN
ENDIF
IF SEEK(&lcPrtFile..ACCOUNT,'ARHIST') AND llRPKeyOff
SELECT ARHIST
SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT) FOR TRANTYPE $ '12' AND IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.) AND IIF(!EMPTY(lcRpGlAcc),cArGlAcc = lcRpGlAcc,.T.)
IF LLMULTCUR .AND. LCRPCURR<>'F'
LCSIGN = GFGETEXSIN('',ALLTRIM(ARHIST.CCURRCODE))
LCSIGN = IIF(EMPTY(LCSIGN), '*', LCSIGN)
LNRATE = IIF(LCRPCURR='O', NEXRATE, LFGETRATE(ARHIST.CCURRCODE))
LNAMOUNT = IIF(LCSIGN='/', AMOUNT/LNRATE/NCURRUNIT, AMOUNT*LNRATE/NCURRUNIT)
ELSE
LNAMOUNT = AMOUNT
ENDIF
LNDEBIT = LNDEBIT+LNAMOUNT
ENDSCAN
ENDIF
SELECT (LNALIAS)
RETURN LNDEBIT
*!*************************************************************
*! Name      : lfSumCharge
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total charge back for all the report
*!*************************************************************
*!
FUNCTION LFSUMCHARG
PRIVATE LNDEBIT, LNALIAS, LNRECNO
LNALIAS = SELECT()
SELECT (LCPRTFILE)
SET RELATION OFF INTO (LCTMPTRAN)
LNRECNO = RECNO()
SUM CHGBACK TO LNDEBIT
SELECT (LNALIAS)
RETURN LNDEBIT
*!*************************************************************
*! Name      : lfCharge
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total charge back for all the report
*!*************************************************************
*!
FUNCTION  LFADRSHIFT
PARAMETER LCARRAYNAM
FOR LNCOUNT = 1 TO 5
IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND. EMPTY(&lcArrayNam.[lnCount])
=ADEL(&lcArrayNam , lnCount)
LNCOUNT = LNCOUNT-1
ENDIF
ENDFOR
FOR LNCOUNT = 1 TO 5
IF TYPE(LCARRAYNAM+'['+STR(LNCOUNT, 1)+']')<>'C'
&lcArrayNam.[lnCount] = ''
ENDIF
ENDFOR
*!*************************************************************
*! Name      : lfGetAdd
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the address of the customer
*!*************************************************************
*!
FUNCTION LFGETADD
LASOLDTO[ 1] = GFGETADR(LCPRTFILE,'','','',1,1)
LASOLDTO[ 2] = GFGETADR(LCPRTFILE,'','','',2,1)
LASOLDTO[ 3] = GFGETADR(LCPRTFILE,'','','',3,1)
LASOLDTO[ 4] = GFGETADR(LCPRTFILE,'','','',4,1)
LASOLDTO[ 5] = GFGETADR(LCPRTFILE,'','','',5,1)
= LFADRSHIFT('laSoldTo')
RETURN ''
*!*************************************************************
*! Name      : lfGetNotes
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the notes of the customer
*!*************************************************************
*!
FUNCTION LFGETNOTES
LCNOTES = 'IIF(llRPCusNot .AND.'+"SEEK('A'+ ACCOUNT,'NOTEPAD') .AND."+'!EMPTY(NOTEPAD.mNotes),'+'ALLTRIM(NOTEPAD.mNotes) + '+"IIF(!CHR(13) $ RIGHT(NOTEPAD.mNotes,2), CHR(13),'') , '')"
RETURN EVALUATE(LCNOTES)
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
*!
FUNCTION LFDEFCURR
RETURN IIF(LLMULTCUR, 'F', 'O')
*!*************************************************************
*! Name      : lfFillSort
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/11/1998
*! Purpose   : Fill sort by arrays.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*!
FUNCTION  LFFILLSORT
PRIVATE LNROWS
LNROWS = IIF(LLMULTCUR, 7, 6)
DIMENSION LASORTDESC[ LNROWS, 1], LASORTVAL[ LNROWS, 1]
LASORTDESC[ 1, 1] = 'Account'
LASORTDESC[ 2, 1] = 'Terms'
LASORTDESC[ 3, 1] = 'Salesrep'
LASORTDESC[ 4, 1] = 'Division'
LASORTDESC[ 5, 1] = 'Region'
LASORTDESC[ 6, 1] = 'Country'
LASORTVAL[ 1, 1] = 'A'
LASORTVAL[ 2, 1] = 'T'
LASORTVAL[ 3, 1] = 'S'
LASORTVAL[ 4, 1] = 'D'
LASORTVAL[ 5, 1] = 'R'
LASORTVAL[ 6, 1] = 'U'
IF LLMULTCUR
= AINS(LASORTDESC, 5)
= AINS(LASORTVAL, 5)
LASORTDESC[ 5, 1] = 'Currency'
LASORTVAL[ 5, 1] = 'C'
ENDIF
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
*!
FUNCTION  LFFILTRNAR
PRIVATE LATEMPTRAN, LATEMPCUST, LCEXCSTAT
LCEXCSTAT = SET('EXACT')
SET EXACT ON

*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [Start]
*DIMENSION LATEMPSTRU[ 39, 4], LATEMPTRAN[ 1, 4], LATEMPCUST[ 1, 4]
DIMENSION LATEMPSTRU[ 40, 4], LATEMPTRAN[ 1, 4], LATEMPCUST[ 1, 4]
*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [End]

STORE '' TO LATEMPSTRU, LATEMPTRAN
PRIVATE LNFILECNT, LNFLDROW
SELECT CUSTOMER
= AFIELDS(LATEMPCUST)
LATEMPSTRU[ 1, 1] = 'ACCOUNT'
LATEMPSTRU[ 2, 1] = 'CFACCODE'
LATEMPSTRU[ 3, 1] = 'SALESREP'
LATEMPSTRU[ 4, 1] = 'CURRENT'
LATEMPSTRU[ 5, 1] = 'AGE30'
LATEMPSTRU[ 6, 1] = 'AGE60'
LATEMPSTRU[ 7, 1] = 'AGE90'
LATEMPSTRU[ 8, 1] = 'AGE120'
LATEMPSTRU[ 9, 1] = 'TOTAGE'
LATEMPSTRU[ 10, 1] = 'OPENCR'
LATEMPSTRU[ 11, 1] = 'CHGBACK'
LATEMPSTRU[ 12, 1] = 'NETBAL'
LATEMPSTRU[ 13, 1] = 'REGION'
LATEMPSTRU[ 14, 1] = 'CTERMCODE'
LATEMPSTRU[ 15, 1] = 'CDIVISION'
LATEMPSTRU[ 16, 1] = 'CCURRCODE'
LATEMPSTRU[ 17, 1] = 'CCONT_CODE'
FOR LNFILECNT = 1 TO 17
LNFLDROW = ASCAN(LATEMPCUST, LATEMPSTRU(LNFILECNT,1))
IF LNFLDROW>0
LNFLDROW = ASUBSCRIPT(LATEMPCUST, LNFLDROW, 1)
LATEMPSTRU[ LNFILECNT, 2] = LATEMPCUST(LNFLDROW,2)
LATEMPSTRU[ LNFILECNT, 3] = LATEMPCUST(LNFLDROW,3)
LATEMPSTRU[ LNFILECNT, 4] = LATEMPCUST(LNFLDROW,4)
ENDIF
ENDFOR
SELECT ARHIST
= AFIELDS(LATEMPTRAN)
LATEMPSTRU[ 18, 1] = 'TRANTYPE'
LATEMPSTRU[ 19, 1] = 'TRANCODE'
LATEMPSTRU[ 20, 1] = 'TRAN'
LATEMPSTRU[ 21, 1] = 'BATCH'
LATEMPSTRU[ 22, 1] = 'CINSTALNO'
LATEMPSTRU[ 23, 1] = 'TRANDATE'
LATEMPSTRU[ 24, 1] = 'DPOSTDATE'
LATEMPSTRU[ 25, 1] = 'CHGBK_DATE'
LATEMPSTRU[ 26, 1] = 'DESC'
LATEMPSTRU[ 27, 1] = 'REFERENCE'
LATEMPSTRU[ 28, 1] = 'AMOUNT'
LATEMPSTRU[ 29, 1] = 'DUEDATE'
LATEMPSTRU[ 30, 1] = 'CADJACCT'
LATEMPSTRU[ 31, 1] = 'CARGLACC'
LATEMPSTRU[ 32, 1] = 'NCURRUNIT'
LATEMPSTRU[ 33, 1] = 'NEXRATE'
FOR LNFILECNT = 18 TO 33
LNFLDROW = ASCAN(LATEMPTRAN, LATEMPSTRU(LNFILECNT,1))
IF LNFLDROW>0
LNFLDROW = ASUBSCRIPT(LATEMPTRAN, LNFLDROW, 1)
LATEMPSTRU[ LNFILECNT, 2] = LATEMPTRAN(LNFLDROW,2)
LATEMPSTRU[ LNFILECNT, 3] = LATEMPTRAN(LNFLDROW,3)
LATEMPSTRU[ LNFILECNT, 4] = LATEMPTRAN(LNFLDROW,4)
ENDIF
ENDFOR
LATEMPSTRU[ 34, 1] = 'CTRANTYPE'
LATEMPSTRU[ 34, 2] = 'C'
LATEMPSTRU[ 34, 3] = 1
LATEMPSTRU[ 34, 4] = 0
LATEMPSTRU[ 35, 1] = 'CHEQUENO'
LATEMPSTRU[ 35, 2] = 'C'
LATEMPSTRU[ 35, 3] = 10
LATEMPSTRU[ 35, 4] = 0
LATEMPSTRU[ 36, 1] = 'CTEMPKEY'
LATEMPSTRU[ 36, 2] = 'C'
LATEMPSTRU[ 36, 3] = 15
LATEMPSTRU[ 36, 4] = 0
LATEMPSTRU[ 37, 1] = 'mNotes'
LATEMPSTRU[ 37, 2] = 'M'
LATEMPSTRU[ 37, 3] = 10
LATEMPSTRU[ 37, 4] = 0
LATEMPSTRU[ 38, 1] = 'llFlgPad'
LATEMPSTRU[ 38, 2] = 'L'
LATEMPSTRU[ 38, 3] = 1
LATEMPSTRU[ 38, 4] = 0
LATEMPSTRU[ 39, 1] = 'Ccitcstchk'
LATEMPSTRU[ 39, 2] = 'C'
LATEMPSTRU[ 39, 3] = 30
LATEMPSTRU[ 39, 4] = 0

*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [Start]
LATEMPSTRU[ 40, 1] = 'note_mem'
LATEMPSTRU[ 40, 2] = 'M'
LATEMPSTRU[ 40, 3] = 10
LATEMPSTRU[ 40, 4] = 0
*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [End]


SET EXACT &lcExcStat  
= LFWORKFILE()
*!*************************************************************
*! Name      : lfWorkFile
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/99
*! Purpose   : Create work cursors.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION  LFWORKFILE
IF LLMULTCUR
IF  .NOT. USED('SYCCURR')
= GFOPENFILE(GCSYSHOME+'SYCCURR',GCSYSHOME+'Ccurrcode','SH')
ENDIF
ELSE
IF  .NOT. USED('POSTDCHQ')
USE IN 0 SHARED (GCDATADIR+'POSTDCHQ') ORDER POSTDCHQ
ENDIF
ENDIF
SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO Ccontcode IN SYCINT
IF USED(LCTEMPAGE)
USE IN (LCTEMPAGE)
ENDIF
IF USED(LCTEMPREV)
USE IN (LCTEMPREV)
ENDIF
IF USED(LCTEMPACC)
USE IN (LCTEMPACC)
ENDIF
CREATE CURSOR (LCTEMPACC) (ACCOUNT C (5), NAGES N (11, 2), NNETBAL N (11, 2))
SELECT (LCTEMPACC)
ZAP
INDEX ON ACCOUNT TAG (LCTEMPACC) OF (GCWORKDIR+LCTEMPACC+'.CDX')
CREATE CURSOR (LCTEMPAGE) FROM ARRAY LATEMPSTRU
SELECT (LCTEMPAGE)
ZAP
INDEX ON CTEMPKEY TAG (LCTEMPREV) OF (GCWORKDIR+LCTEMPAGE+'.CDX') DESCENDING
INDEX ON CTEMPKEY TAG (LCTEMPAGE) OF (GCWORKDIR+LCTEMPAGE+'.CDX')
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
*!
FUNCTION  LFVSORTBY
LLCHSORTBY = .T.
*!*************************************************************
*! Name      : lfGetReplc
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Get Replaced expression.(According to sort by options)
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : lcExpr ---> which means (Sort by expression) 
*!*************************************************************
*!
FUNCTION LFGETREPLC
PRIVATE LCEXPR
DO CASE
CASE LCRPAGSORT='A'
LCGROUP = ''
LCINNGRP = 'Account'
LCEXPR = 'ACCOUNT + cTranType'
LCSORTED = 'Account'
CASE LCRPAGSORT='T'
LCGROUP = 'cTermCode'
LCINNGRP = 'cTermCode + Account'
LCCODEDESC = "gfCoddes(cTermCode,'CTERMCODE',.T.) + ' Totals ==>'"
LCEXPR = 'CTERMCODE + Account + cTranType'
LCSORTED = 'Term Code'
CASE LCRPAGSORT='D'
LCGROUP = 'CDIVISION'
LCINNGRP = 'CDIVISION + Account'
LCCODEDESC = "gfCoddes(CDIVISION,'CDIVISION',.T.) + ' Totals ==>'"
LCEXPR = 'CDIVISION + Account + cTranType'
LCSORTED = 'Division'
CASE LCRPAGSORT='C'
LCGROUP = 'cCurrCode'
LCINNGRP = 'cCurrCode + Account'
LCCODEDESC = "cCurrCode + '-' + IIF(SEEK(cCurrCode,'SYCCURR'),SYCCURR.CCURRDESC,SPACE(30)) + ' Totals ==>'"
LCEXPR = 'CCURRCODE + Account + cTranType'
LCSORTED = 'Currency'
CASE LCRPAGSORT='S'
LCGROUP = 'Salesrep'
LCINNGRP = 'Salesrep + Account'
LCCODEDESC = "IIF(SEEK(Salesrep,'Salesrep'),Salesrep.Name,SPACE(24)) + ' Totals ==>'"
LCEXPR = 'Salesrep + Account + cTranType'
LCSORTED = 'Sales Representative'
CASE LCRPAGSORT='R'
LCGROUP = 'Region'
LCINNGRP = 'Region + Account'
LCCODEDESC = "gfCoddes(REGION,'REGION',.T.) + ' Totals ==>'"
LCEXPR = 'Region + Account + cTranType'
LCSORTED = 'Region'
CASE LCRPAGSORT='U'
LCGROUP = 'cCont_Code'
LCINNGRP = 'cCont_Code + Account'
LCCODEDESC = "cCont_Code + '-' + IIF(SEEK(cCont_Code,'SYCINT'),SYCINT.CCONT_DESC,SPACE(30)) + ' Totals ==>'"
LCEXPR = 'cCont_Code + Account + cTranType'
LCSORTED = 'Country'
ENDCASE
IF LLCURRGRP .AND. (LEFT(LCEXPR, 9)<>'CCURRCODE')
LCEXPR = LCEXPR+' + CCURRCODE'
LCGROUP = LCGROUP+' + CCURRCODE'
LCINNGRP = LCINNGRP+' + CCURRCODE'
ENDIF
RETURN LCEXPR
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
*!
FUNCTION LFCOLLTIME
PARAMETER LCSTART, LCEND
LNSTHOUR = IIF(VAL(LEFT(LCSTART, 2))=0, VAL(LEFT(LCSTART, 2))+24, VAL(LEFT(LCSTART, 2)))
LNENDHOUR = IIF(VAL(LEFT(LCEND, 2))=0, VAL(LEFT(LCEND, 2))+24, VAL(LEFT(LCEND, 2)))
LNSTART = 3600*LNSTHOUR+60*VAL(SUBSTR(LCSTART, 4, 2))+VAL(RIGHT(LCSTART, 2))
LNEND = 3600*LNENDHOUR+60*VAL(SUBSTR(LCEND, 4, 2))+VAL(RIGHT(LCEND, 2))
RETURN (LNEND-LNSTART)
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
*!
FUNCTION LFVENDDATE
LLRPKEYOFF = (LDRPENDDAT<GDSYSDATE)
= LFOGSHOWGE('llRpKeyOff')
RETURN .T.
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
*!
FUNCTION  LFVINCDET
LNAGECRD = ASCAN(LAOGOBJTYP, 'llRpAgeCrd')
IF LNAGECRD<>0
LNAGECRD = ASUBSCRIPT(LAOGOBJTYP, LNAGECRD, 1)
STORE LLRPCRDDET TO LAOGOBJCNT[ LNAGECRD], LLRPAGECRD
= LFOGSHOWGE('llRpAgeCrd')
ENDIF
*!*************************************************************
*! Name      : lfCollect
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
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
*!
FUNCTION  LFCOLLECT
PRIVATE LCDEBFLT, LCCREFLT, LCHSTFLT, LCFACFLT, LCGLACCFLT
STORE '' TO LCDEBFLT, LCCREFLT, LCHSTFLT, LCFACFLT
IF LCRPFACTOR='F'
LCFACFLT = IIF(EMPTY(LCRPSPEFAC), '!EMPTY(cFacCode)', 'cFacCode=lcRPSpeFac')
LCDEBFLT = 'TRANDATE<=ldRpEndDat AND '+"IIF( TRANTYPE='3',CHGBK_DATE<=ldRpEndDat,.T.)"
LCCREFLT = 'TRANDATE<=ldRPEndDat AND '+"IIF( TRANTYPE='6',CREDT_DATE<=ldRPEndDat,.T.)"
ELSE
IF LCRPFACTOR='N'
LCFACFLT = 'EMPTY(cFacCode)'
ENDIF
LCDEBFLT = "IIF( TRANTYPE='3',"+'TRANDATE<=ldRpEndDat AND CHGBK_DATE<=ldRpEndDat ,'+'TRANDATE<=ldRpEndDat )'
LCCREFLT = "IIF( TRANTYPE='6',"+'TRANDATE<=ldRPEndDat AND CREDT_DATE<=ldRPEndDat ,'+'TRANDATE<=ldRPEndDat )'
ENDIF
IF  .NOT. EMPTY(LDPOSTDATE)
LCDEBFLT = LCDEBFLT+IIF(EMPTY(LCDEBFLT), '', ' AND ')+"IIF(TranType='1',IIF(SEEK(Tran,'InvHdr'),ldPostDate >= InvHdr.dPostDate,.F.),.T.)"
LCCREFLT = LCCREFLT+IIF(EMPTY(LCCREFLT), '', ' AND ')+"IIF(TranType='0',IIF(SEEK(Tran,'RetHdr'),ldPostDate >= RetHdr.dPostDate,.F.),.T.)"
ENDIF
LCHSTFLT = 'HISTDATE > ldRPEndDat AND TRANDATE <= ldRPEndDat'
IF  .NOT. EMPTY(LCFACFLT)
LCDEBFLT = LCDEBFLT+' AND '+LCFACFLT
LCCREFLT = LCCREFLT+' AND '+LCFACFLT
LCHSTFLT = LCHSTFLT+' AND '+LCFACFLT
ENDIF
IF  .NOT. EMPTY(STRTRAN(LCRPGLACC, '-', ''))
LCDEBFLT = LCDEBFLT+' AND cArGlAcc = lcRpGlAcc'
LCCREFLT = LCCREFLT+' AND cArGlAcc = lcRpGlAcc'
LCHSTFLT = LCHSTFLT+' AND cArGlAcc = lcRpGlAcc'
ENDIF
SELECT CUSTOMER
GOTO TOP
SCATTER BLANK MEMO MEMVAR

*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [Start]
STORE '' TO m.note_mem
*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [End]

LCRPEXP = LCRPEXP+' AND '+' CUSTOMER.STATUS $ lcRpStatFr '
= GFDOTRIGER('ARAGING',PADR('COMPLEXP', 10))
IF  .NOT. LLMULTCUR
= LFINSPSTCH()
ENDIF
= LFINSDEBIT()
= LFINSCREDT()
IF LLRPKEYOFF
= LFINSHISTO()
ENDIF
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
*!
FUNCTION  LFINSPSTCH
SELECT POSTDCHQ
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
SCAN FOR account+DTOS(paydate) = '' AND CUSTOMER.TYPE = 'M' AND &lcRpExp AND  Amount <> 0
SCATTER MEMVAR
M.TRANDATE = POSTDCHQ.PAYDATE
M.DESC = 'PostDated Checks'
M.TRANTYPE = ' '
M.CTRANTYPE = 'P'
DO LPINSRECRD
ENDSCAN
SET RELATION TO
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
*!
FUNCTION  LFINSDEBIT
SELECT DEBIT
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
SCAN FOR account+tran+cinstalno+DTOS(trandate) = '' AND &lcDebFlt AND  CUSTOMER.TYPE = 'M' AND &lcRpExp AND Amount <> 0
IF ( .NOT. LLRPPRNCH .AND. TRANTYPE='3')
LOOP
ENDIF
SCATTER MEMVAR
DO LPDEBDEAL
DO LPINSRECRD
ENDSCAN
SET RELATION TO
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
*!
FUNCTION  LFINSCREDT
SELECT CREDIT
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
SCAN FOR account+tran+DTOS(trandate) = '' AND &lcCreFlt AND  CUSTOMER.TYPE = 'M' AND &lcRpExp AND Amount <> 0
SCATTER MEMVAR
DO LPCREDEAL
DO LPINSRECRD
ENDSCAN
SET RELATION TO
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
*!
FUNCTION  LFINSHISTO
SELECT ARHIST
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
SCAN FOR account+tran+cinstalno = '' AND &lcHstFlt AND  CUSTOMER.TYPE = 'M' AND &lcRpExp AND Amount <> 0
IF ( .NOT. LLRPPRNCH .AND. TRANTYPE='3') .OR. (TRANTYPE='3' .AND. CHGBK_DATE>LDRPENDDAT) .OR. (TRANTYPE='6' .AND. CREDT_DATE>LDRPENDDAT) .OR.  .NOT. (TRANTYPE$'0123456')
LOOP
ENDIF
SCATTER MEMVAR
IF TRANTYPE$'123'
DO LPDEBDEAL
ENDIF
IF TRANTYPE$'0456'
DO LPCREDEAL
ENDIF
IF LCRPREPORT='D'
M.CTRANTYPE = 'H'
ENDIF

*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [Start]
m.note_mem = Arhist.note_mem
*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [end]

DO LPINSRECRD
ENDSCAN
SET RELATION TO
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
*!
PROCEDURE LPDEBDEAL

IF LCAGETYPE='D'
= LFUPDTAGE('D',LDRPENDDAT-TRANDATE)
ELSE
LDDUEDATE = IIF(EMPTY(DUEDATE), TRANDATE+LNRPDAY1, DUEDATE)
= LFUPDTAGE('D',LDRPENDDAT-LDDUEDATE)
ENDIF
IF TRANTYPE='3'
M.CHGBACK = AMOUNT
ENDIF

*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [Start]
m.note_mem = DEBIT.note_mem
*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [End]


STORE AMOUNT TO M.TOTAGE, M.NETBAL
IF TRANTYPE='1' .AND. SEEK(DEBIT.TRAN, 'INVHDR')
M.CDIVISION = INVHDR.CDIVISION
ENDIF
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
*!
PROCEDURE LPCREDEAL
= LFUPDTAGE('C',GDSYSDATE-TRANDATE)
STORE AMOUNT TO M.OPENCR, M.NETBAL

*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [Start]
IF ('RM' $ oAriaApplication.CompanyInstalledModules) AND !USED('RETHDR')
  =gfOpenTable(oAriaApplication.DataDir+'RETHDR',oAriaApplication.DataDir+'RETHDR','SH')
ENDIF 
*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [End]

IF TRANTYPE='0' .AND. ('RM'$oAriaApplication.CompanyInstalledModules) .AND. gfSEEK(CREDIT.TRAN, 'RETHDR')
  M.CDIVISION = RETHDR.CDIVISION
ENDIF

*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [Start]
m.note_mem = Credit.note_mem
*: C200967,1 MMT 03/18/2008 Print Debit/Credit Notes [End]

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
*!
FUNCTION  LFUPDTAGE
PARAMETER LCUPDTTYP, LNAGEDAYS
STORE 0.00  TO M.CURRENT, M.AGE30, M.AGE60, M.AGE90, M.AGE120, M.OPENCR, M.CHGBACK, M.TOTAGE, M.NETBAL, M.AGE00
STORE '' TO M.REGION, M.CDIVISION, M.SALESREP, M.CTERMCODE, M.CPASTONLY,m.note_mem

IF TYPE('lcUpdtTyp')$'UL'
RETURN
ENDIF
IF LCUPDTTYP='D'
IF LCAGETYPE='D'
LCAGES = 'm.Age'+IIF(LNAGEDAYS>=LNRPDAY4, '120', IIF(LNAGEDAYS>=LNRPDAY3, '90', IIF(LNAGEDAYS>=LNRPDAY2, '60', IIF(LNAGEDAYS>=LNRPDAY1, '30', '00'))))
ELSE
LCAGES = 'm.Age'+IIF(LNAGEDAYS>=LNRPDAY3+1, '120', IIF(LNAGEDAYS>=LNRPDAY2+1, '90', IIF(LNAGEDAYS>=LNRPDAY1+1, '60', IIF(LNAGEDAYS>=1, '30', '00'))))
ENDIF
ELSE
LCAGES = 'm.Age'+IIF(LNAGEDAYS>=120, '120', IIF(LNAGEDAYS>=90, '90', IIF(LNAGEDAYS>=60, '60', IIF(LNAGEDAYS>=30, '30', '00'))))
ENDIF
&lcAges   = Amount       
M.CURRENT = IIF(M.AGE00=0, 0, M.AGE00)
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
*!
PROCEDURE LPINSRECRD
PRIVATE LNPASTAGES, LNNETBAL
STORE 0 TO LNPASTAGES, LNNETBAL
M.REGION = CUSTOMER.REGION
M.CTERMCODE = CUSTOMER.CTERMCODE
M.CCONT_CODE = CUSTOMER.CCONT_CODE
M.SALESREP = CUSTOMER.SALESREP
INSERT INTO (LCTEMPAGE) FROM MEMVAR
SELECT (LCTEMPAGE)
REPLACE CTEMPKEY WITH EVALUATE(LCREPLEXPR)
IF EMPTY(CTRANTYPE)
IF LLMULTCUR .AND. (CCURRCODE<>GCBASECURR)
= LFBASEVAL(@LNPASTAGES,@LNNETBAL)
ELSE
LNPASTAGES = IIF(TOTAGE>0 .AND. CURRENT=0, TOTAGE, 0)
LNNETBAL = NETBAL
ENDIF
IF SEEK(ACCOUNT, LCTEMPACC)
REPLACE &lcTempAcc..nAges WITH   &lcTempAcc..nAges + lnPastAges , &lcTempAcc..nNetBal WITH &lcTempAcc..nNetBal + lnNetBal
ELSE
WAIT WINDOW NOWAIT 'Collect data for Customer '+ACCOUNT
M.ACCOUNT = ACCOUNT
M.NAGES = LNPASTAGES
M.NNETBAL = LNNETBAL
INSERT INTO (LCTEMPACC) FROM MEMVAR
ENDIF
ENDIF

*!*************************************************************
*! Name      : lfBaseVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/17/98
*! Purpose   : Calculate Currency unit, rate, signs and Evaluate amount values
*!*************************************************************
*! Passed Parameters  : 1- First  number (Due Ages Amount)
*! By Reference       : 2- Second number (Net Balance Amount)
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION  LFBASEVAL
PARAMETER LNBASEVAL1, LNBASEVAL2
PRIVATE LCAEXRSIN, LCAUNTSIN, LNCURRRATE, LNCURRUNIT
LCAUNTSIN = '*'
LCAEXRSIN = '*'
LCAEXRSIN = GFGETEXSIN(@LCAUNTSIN,CCURRCODE)
LNCURRRATE = NEXRATE
LNCURRUNIT = NCURRUNIT
lnBaseVal1 = IIF(TotAge > 0 AND Current = 0,TotAge,0) &lcAExRSin lnCurrRate &lcAUntSin lnCurrUnit
lnBaseVal2 = NetBal &lcAExRSin lnCurrRate &lcAUntSin lnCurrUnit
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
*!
FUNCTION LFPRNCURDT
PRIVATE LCPRINTVAL
LCPRINTVAL = ''
IF LLRPAGECRD
IF (LLRPCRDDET .OR. NETBAL>=0) .AND. CURRENT<>0
IF LLRPDEC
LCPRINTVAL = TRANSFORM(IIF( .NOT. LLMULTCUR .OR. (LCRPCURR='F') .OR. (CCURRCODE=GCBASECURR), CURRENT, GFAMNTDISP(CURRENT,LCRPCURR,LDRPEXDATE,LCRPTMPNAM)), '99999999.99')
ELSE
LCPRINTVAL = TRANSFORM(IIF( .NOT. LLMULTCUR .OR. (LCRPCURR='F') .OR. (CCURRCODE=GCBASECURR), CURRENT, GFAMNTDISP(CURRENT,LCRPCURR,LDRPEXDATE,LCRPTMPNAM)), '99999999999')
ENDIF
ENDIF
ELSE
IF OPENCR<0
IF LLRPCRDDET
IF LLRPDEC
LCPRINTVAL = TRANSFORM(IIF( .NOT. LLMULTCUR .OR. (LCRPCURR='F') .OR. (CCURRCODE=GCBASECURR), OPENCR, GFAMNTDISP(OPENCR,LCRPCURR,LDRPEXDATE,LCRPTMPNAM)), '99999999.99')
ELSE
LCPRINTVAL = TRANSFORM(IIF( .NOT. LLMULTCUR .OR. (LCRPCURR='F') .OR. (CCURRCODE=GCBASECURR), OPENCR, GFAMNTDISP(OPENCR,LCRPCURR,LDRPEXDATE,LCRPTMPNAM)), '99999999999')
ENDIF
ENDIF
ELSE
IF LLRPDEC
LCPRINTVAL = TRANSFORM(IIF( .NOT. LLMULTCUR .OR. (LCRPCURR='F') .OR. (CCURRCODE=GCBASECURR), CURRENT, GFAMNTDISP(CURRENT,LCRPCURR,LDRPEXDATE,LCRPTMPNAM)), '99999999.99')
ELSE
LCPRINTVAL = TRANSFORM(IIF( .NOT. LLMULTCUR .OR. (LCRPCURR='F') .OR. (CCURRCODE=GCBASECURR), CURRENT, GFAMNTDISP(CURRENT,LCRPCURR,LDRPEXDATE,LCRPTMPNAM)), '99999999999')
ENDIF
ENDIF
ENDIF
IF  .NOT. EMPTY(LCPRINTVAL) .AND. VAL(LCPRINTVAL)=0
LCPRINTVAL = ''
ENDIF
RETURN LCPRINTVAL
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
*!
FUNCTION  LFCLEARREP
LLOGFLTCH = .T.
IF USED('POSTDCHQ')
USE IN POSTDCHQ
ENDIF
IF USED('SYCCURR')
USE IN SYCCURR
ENDIF
IF USED(LCTEMPAGE)
USE IN (LCTEMPAGE)
ENDIF
IF USED(LCTEMPREV)
USE IN (LCTEMPREV)
ENDIF
IF USED(LCTEMPACC)
USE IN (LCTEMPACC)
ENDIF
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
*!
FUNCTION  LFVTERM
STORE .F. TO LLFLGMOVR
STORE .T. TO LLOGFLTCH
STORE SPACE(0) TO LCRPSTATFR
= GFMOVER(@LARPSOURCE,@LARPTARGET,'Customer Status ',.T.,'')
IF ASCAN(LARPTARGET, 'Active')>0
LCRPSTATFR = 'A'
LLFLGMOVR = .T.
ENDIF
IF ASCAN(LARPTARGET, 'Hold')>0
LCRPSTATFR = LCRPSTATFR+'H'
LLFLGMOVR = .T.
ENDIF
IF ASCAN(LARPTARGET, 'Cancelled')>0
LCRPSTATFR = LCRPSTATFR+'X'
LLFLGMOVR = .T.
ENDIF
IF  .NOT. LLFLGMOVR
LCRPSTATFR = 'AHX'
ENDIF
*!*************************************************************
*! Name      : lfFillARY
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to fill Terms,Division,Region,Ship via,
*!           : Special Instruction,Customer classification.
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************
*!
FUNCTION  LFFILLARY
DIMENSION LARPSOURCE[ 3], LARPTARGET[ 1, 1]
STORE '' TO LARPSOURCE, LARPTARGET
LARPSOURCE[ 1] = 'Active'
LARPSOURCE[ 2] = 'Hold'
LARPSOURCE[ 3] = 'Cancelled'
*!************************************************************
*! Name      : RefreshStatus
*: Developer : Heba Fathi (HFK)
*: Date      : 08/17/2004
*! Purpose   : function to Set the index for the SQL files
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

