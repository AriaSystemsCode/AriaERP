*:*******************************************************************************
*: Name      : APAGEVIT
*: Developer : Albert Raif  (ALB)
*: Date      : 08/29/2002
*: Purpose   : 
*:*******************************************************************************
*: Calls     : NONE
*:*******************************************************************************
*: Returns            : 
*:*******************************************************************************
*: Example            : =lfvMakExch()
*:*******************************************************************************
*: C102560

lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),""," AND ") + [IIF(cInvStat='V',;
                    IIF(SEEK(cInvNo + cVendCode,'APDISTDS') AND ;
                    DTOS(ApDistDs.dApdTrDat) > DTOS(ldRpCurDat),.T.,.F.),.T.)]

SELECT APINVHDR
SET ORDER TO CURVENINV

SET ORDER TO TAG VENDINV
SET ORDER TO TAG VENCODE IN APVENDOR
SET RELATION TO cvendcode INTO APVENDOR ADDITIVE

llShowGT = IIF(lcRpCurr="F",IIF(EMPTY(laogfxflt[1,6]),.F.,.T.),.T.)
lcExSin1 = ' '
lcExSin2 = ' '

IF !llRpConPay
  SET ORDER TO TAG INVVEND IN APDIST
  SET RELATION TO CINVNO+CVENDCODE INTO APDIST ADDITIVE
  SELECT APDIST
  SET ORDER TO TAG TYPMETHDOC IN APPAYMNT

  SET RELATION TO "P"+APDIST.CAPDTRTYP+PADR(ALLTRIM(APDIST.CAPDREF),8," ")+;
                   APDIST.CBNKCODE+APDIST.CCHKACCT INTO APPAYMNT ADDITIVE                   
  lnTotPaymnt = 0  
  llCanPrint  = .F.
  
  TmpApInvHd = gfTempName()
  *-- This function is used to get all the AP invoices according to the
  *-- selected criteria and insert it in a temporary file to be used
  *-- in the print process.
  =lfColData()
  RETURN
ENDIF
SELECT APINVHDR

*If user wants to display all invoice regardless of the currency code, change 
* the index to be Vendor Code  + Currency + invoice No.
=lfCreatNdx()
DO gfDispRe WITH EVAL('lcRpForm'),'FOR '+lcRpExp+ ' AND' + IIF(EMPTY(lcRpRefnce),".T.",' LIKE("'+STRTRAN(lcRpRefnce,' ','?')+'",APINVHDR.cinvref)')
SET RELATION TO  
SELECT APDIST
SET RELATION TO

*!**************************************************************************
*!
*!      Function: lfRepShow
*!
*!**************************************************************************
*
FUNCTION lfRepShow

laOGFxFlt[1,6]= gcBaseCurr
laOGObjCnt[12] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")

*!**************************************************************************
*!
*!      Function: lfvCurDat
*!
*!**************************************************************************
* 
FUNCTION lfvCurDat

lcRpVar=SYS(18)
IF EMPTY(ldRpCurDat)
  *** You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'report date')
  &lcRpVar=lcOldVal
  SHOW GET (lcRpVar)
  _CUROBJ=_CUROBJ
ELSE
 IF VARREAD()="LDRPCURDAT" AND ldRpCurDat < gdSysDate
   llRpConPay = .F.
   =lfOgShowGet("llRpConPay")   
   =lfvConPaymnt() 
 ENDIF
  IF !llInvDate
    ldRpC1F=ldRpCurDat - lnRpPrd1
    ldRpC1T=ldRpCurDat 
    ldRpC2F=ldRpCurDat - lnRpPrd2
    ldRpC2T=ldRpC1F - 1
    ldRpC3F=ldRpCurDat - lnRpPrd3
    ldRpC3T=ldRpC2F - 1
    ldRpC4F=ldRpCurDat - lnRpPrd4
    ldRpC4T=ldRpC3F - 1 
  ELSE
    ldRpC4F=ldRpCurDat 
    ldRpC4T=ldRpCurDat - lnRpPrd4
    ldRpC3F=ldRpC4T - 1
    ldRpC3T=ldRpCurDat - lnRpPrd3
    ldRpC2F=ldRpC3T - 1
    ldRpC2T=ldRpCurDat - lnRpPrd2
    ldRpC1F=ldRpC2T - 1
    ldRpC1T=ldRpCurDat - lnRpPrd1

  ENDIF
ENDIF 

*!**************************************************************************
*!
*!      Function: lfSumALL
*!
*!**************************************************************************
*
FUNCTION lfSumALL
PARAMETERS lcVend,lcInv

IF apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj  = 0
  llCanPrint  = .T.
  lnTotPaymnt = 0
  RETURN 0
ENDIF

lnTotPaymnt = 0
llCanPrint  = .F.
lnRPAlias=SELECT()
SELECT APDIST
IF gfGetMemVar('LLMULCURR')
  SEEK lcInv+lcVend
  SCAN REST WHILE CINVNO+CVENDCODE = lcInv+lcVend;
        FOR ((capdtrtyp $ "MNHP" AND capdactID $ "CSJ") .OR. ( capdtrtyp = "A" AND nApdAmnt < 0));
        AND dAPDtrdat > ldRpCurDat;
        AND cApdStat  <> 'V'
     
     DO CASE
       CASE APDIST.CAPDACTID = 'C'
         lcExSin2 = ' '
         IF APINVHDR.CCURRCODE = APPAYMNT.CCURRCODE
           lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT,2)
         ELSE
           lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.CCURRCODE,APPAYMNT.CCURRCODE)
           lcExSin1 = IIF(lcExSin1 = '*' , '/' , '*')
           lcExSin2 = IIF(lcExSin2 = '*' , '/' , '*')
           lnUnit   = APPAYMNT.NCURRUNIT
           lcInvCurr = APINVHDR.CCURRCODE
           lcPayCurr = APPAYMNT.CCURRCODE
           ldExRateDt = APPAYMNT.DPAYDATE
           lnExRate = gfChkRate('lnUnit',lcInvCurr,ldExRateDt,.F.,.F.,lcPayCurr)
           lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
         ENDIF
       CASE APDIST.CAPDACTID $ 'S'
         lnTotPaymnt = lnTotPaymnt - APDIST.nApdAmnt
       CASE APDIST.CAPDACTID $ 'J' .AND. APDIST.nApdLinNo <> 1
         lnTotPaymnt = lnTotPaymnt - APDIST.nApdAmnt
       CASE APDIST.cApdTrTyp = "A" 
         lnTotPaymnt = lnTotPaymnt + APDIST.nApdAmnt
     ENDCASE     
  ENDSCAN
ELSE
  SUM REST nAPdAmnt WHILE CINVNO+CVENDCODE = lcInv+lcVend;
      FOR ((capdtrtyp $ "MNHP" AND capdactID = "A") .OR. ( capdtrtyp = "A" AND nApdAmnt < 0));
      AND dAPDtrdat > ldRpCurDat AND cApdStat  <> 'V' TO lnTotPaymnt 
ENDIF
IF apinvhdr.nInvAmnt > 0 AND lnTotPaymnt <> apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj
  SELECT APDIST
  lcOldTag = SET("ORDER")
  SET ORDER TO PAYMNTS
  SEEK "A"+REPL(" ",FSIZE("CBNKCODE"))+REPL(" ",FSIZE("CCHKACCT"))+lcInv
  SUM REST nAPdAmnt WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF="A"+REPL(" ",FSIZE("CBNKCODE"))+;
                          REPL(" ",FSIZE("CCHKACCT"))+lcInv ;
                      FOR cVendCode = lcVend ;
                      AND dAPDtrdat > ldRpCurDat ;
                      AND cApdStat  <> 'V';
                      AND nApdAmnt > 0 TO lnTotDebit
  lnTotPaymnt = lnTotPaymnt + lnTotDebit
  SET ORDER TO &lcOldTag
ENDIF

SELECT (lnRpAlias)
GO RECNO()
llCanPrint  = (apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk - apinvhdr.ninvadj+lnTotPaymnt <> 0)
RETURN IIF(llCanPrint,lnTotPaymnt,0)

*!**************************************************************************
*!
*!      Function: lfSumPaymnt
*!
*!**************************************************************************
*
FUNCTION lfSumPaymnt
PARAMETERS lcVend,lcInv
ldComDate={}
lnTotPaymnt = 0
llCanPrint  = .F.
IF llInvDate
  DO CASE
    CASE BETWEEN(APINVHDR.DINVDATE,ldRpC1T,ldRpC1F)
      ldComDate=ldRpC1F
    CASE BETWEEN(APINVHDR.DINVDATE,ldRpC2T,ldRpC2F)
      ldComDate=ldRpC2F
    CASE BETWEEN(APINVHDR.DINVDATE,ldRpC3T,ldRpC3F)
      ldComDate=ldRpC3F
    CASE BETWEEN(APINVHDR.DINVDATE,ldRpC4T,ldRpC4F)
      ldComDate=ldRpC4F
  ENDCASE 
ELSE
  DO CASE
    CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC1F,ldRpC1T)
      ldComDate=ldRpC1T
    CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC2F,ldRpC2T)
      ldComDate=ldRpC2T
    CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC3F,ldRpC3T)
      ldComDate=ldRpC3T
    CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC4F,ldRpC4T)
      ldComDate=ldRpC4T
  ENDCASE 
ENDIF
lnRPAlias=SELECT()
SELECT APDIST
SUM REST nAPdAmnt WHILE CINVNO+CVENDCODE = lcInv+lcVend;
    FOR capdtrtyp $ "MNHP" AND capdactID = "A";
    AND dAPDtrdat > ldComDate TO lnTotPaymnt 
SELECT (lnRpAlias)
GO RECNO()
llCanPrint  = (apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk - apinvhdr.ninvadj+lnTotPaymnt <> 0)
RETURN ''

*!**************************************************************************
*!
*!      Function: lfvConPaymnt
*!
*!**************************************************************************
*
FUNCTION lfvConPaymnt

IF !llRpConPay
  lcRpForm = IIF(lcRpCType='S',"APAGEDS","APAGEDD")+IIF(llInvDate,"V","")
  laOGHDFlt[2,1] = "APINVHDR."+IIF(llInvDate,"DINVDATE","DINVDUDAT")
  laOGHDFlt[2,3] = "D"
  laOGHDFlt[2,5] = "Between"
  laOGHDFlt[2,6] = ""
  laOGHDFlt[2,7] = "V"
ELSE 
  lcRpForm = IIF(lcRpCType='S',"APAGECS","APAGECD")+IIF(llInvDate,"V","")
  laOGHDFlt[2,1] = "ABS(apinvhdr.ninvamnt)"
  laOGHDFlt[2,3] = "N"
  laOGHDFlt[2,5] = "Greater Than"  
  laOGHDFlt[2,6] = "ABS(apinvhdr.ninvpaid) + ABS(apinvhdr.ninvadj) + ABS(apinvhdr.ninvdistk)"  
  laOGHDFlt[2,7] = "E"
ENDIF
*!**************************************************************************
*!
*!      Function: lfvRepForm
*!
*!**************************************************************************
*
FUNCTION lfvRepForm

DO CASE
  CASE llRpConPay AND llInvDate
    lcRpForm = IIF(lcRpCType='S',"APAGECSV","APAGECDV")
  CASE !llRpConPay AND llInvDate
    lcRpForm = IIF(lcRpCType='S',"APAGEDSV","APAGEDDV")
  CASE llRpConPay AND !llInvDate
    lcRpForm = IIF(lcRpCType='S',"APAGECS","APAGECD")
  CASE !llRpConPay AND !llInvDate
    lcRpForm = IIF(lcRpCType='S',"APAGEDS","APAGEDD")
ENDCASE
*!**************************************************************************
*!
*!      Function: lfvPrd
*!
*!**************************************************************************
* 
FUNCTION lfvPrd

IF !llInvDate
  DO CASE
    CASE lnRpPrd1 <= 0 
      ** MESSAGE : " ð should be greater than ð."
      **           "           ® Ok ¯           "
      =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 1st period|zero')

    CASE lnRpPrd1 >= lnRpPrd2
      ** MESSAGE : " ð should be greater than ð."
      **           "           ® Ok ¯           "
      =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 2nd period|no. of days for the 1st period')

    CASE lnRpPrd2 >= lnRpPrd3
      ** MESSAGE : " ð should be greater than ð."
      **           "           ® Ok ¯           "
      =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 3rd period|no. of days for the 2nd period')

    CASE lnRpPrd3 >= lnRpPrd4
      ** MESSAGE : " ð should be greater than ð."
      **           "           ® Ok ¯           "
      =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 4th period|no. of days for the 3rd period')


    OTHERWISE
      =lfvCurDat()
      RETURN .T. 
  ENDCASE 
ELSE

  DO CASE
    CASE lnRpPrd4 <= 0 
      ** MESSAGE : " ð should be greater than ð."
      **           "           ® Ok ¯           "
      =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 1st period|zero')

    CASE lnRpPrd4 >= lnRpPrd3
      ** MESSAGE : " ð should be greater than ð."
      **           "           ® Ok ¯           "
      =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 2nd period|no. of days for the 1st period')

    CASE lnRpPrd3 >= lnRpPrd2
      ** MESSAGE : " ð should be greater than ð."
      **           "           ® Ok ¯           "
      =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 3rd period|no. of days for the 2nd period')

    CASE lnRpPrd2 >= lnRpPrd1
      ** MESSAGE : " ð should be greater than ð."
      **           "           ® Ok ¯           "
      =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 4th period|no. of days for the 3rd period')


    OTHERWISE
      =lfvCurDat()
      RETURN .T. 
  ENDCASE 

ENDIF
*B600490,1  Malak Hanna  {End}
lcRpVar=SYS(18)
&lcRpVar=lcOldVal
SHOW GET (lcRpVar)
_CUROBJ = _CUROBJ

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
*
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

IF lcRpCurr = 'F' .AND. EMPTY(laOGFxFlt[1,6])
  laOGFxFlt[1,6]= gcBaseCurr
ENDIF
SHOW GET laOGFxFlt[1,6]

*!*************************************************************
*! Name      : lfwCurCode
*! Developer : Mohamed Hassan
*! Date      : 12/17/1995
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************

FUNCTION lfwCurCode

lcOldCurr = laOGFxFlt[1,6]

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Mohamed Hassan
*! Date      : 12/17/1995
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfvCurCode

IF EMPTY(laOGFxFlt[1,6])  
  llAllCurr  = .T.
  RETURN
ENDIF
llAllCurr  = .F.

IF !SEEK(laOGFxFlt[1,6],'SYCCURR') .OR. ATC("?",laOGFxFlt[1,6]) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    laOGFxFlt[1,6] = lcOldCurr
  ELSE
    laOGFxFlt[1,6] = laTemp[1]
  ENDIF
ENDIF

SHOW GET laOGFxFlt[1,6]

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
*E301301,1 AKA 
FUNCTION lfClearRep
IF FILE(gcWorkDir +lcApInvHdr+ '.CDX') 
  SELECT APINVHDR
  CLOSE INDEX
  ERASE (gcWorkDir +lcApInvHdr+ '.CDX')
ENDIF


*!**************************************************************************
*!
*!      Function: lfCreatNdx
*!
*!**************************************************************************
*
FUNCTION lfCreatNdx
SELECT APINVHDR
IF EMPTY(laOGFxFlt[1,6]) .AND. lcRpCurr <> 'F' 
  IF TYPE("lcApInvHdr") <> 'U'
    IF !FILE(gcWorkDir +lcApInvHdr+ '.CDX') 
      lcApInvHdr = gfTempName()        
      INDEX ON cvendcode+ccurrcode+cinvno  TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')    
    ELSE
      SET ORDER TO TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')    
    ENDIF
  ELSE    && ELASE OF  "If index name is already defined "
    lcApInvHdr = gfTempName()          
    INDEX ON cvendcode+ccurrcode+cinvno  TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')    
  ENDIF  && ENDIF OF  "If index name is already defined "
ELSE  
  SET ORDER TO CURVENINV
ENDIF
  

*!**************************************************************************
*! Name      : lfvApAcCod
*! Developer : Sameh Saiid Ezzat
*! Date      : 02/22/2000
*! Purpose   : This function is to validate the accounts from 
*!             the chart of account of the active company or the
*!             chart of account of another company.
*!**************************************************************************
*! Called from : The Option Grid
*!**************************************************************************
*! Example   :  =lfvApAcCod()
*!**************************************************************************
*
FUNCTION lfvApAcCod
PRIVATE lcSavAlias,lcFieldCont

lcSavAlias  = ALIAS()  && Variable to save the selected alias.
lcFieldCont = EVALUATE(SYS(18))   && Assign the content of the field to the variable.

*** Variable hold an empty account to compair with. ***
lcEmptyAcs = REPLICATE('0',lnApsAcLen)

*Prevent executing the browse if the account code is empty.
IF !EMPTY(STRTRAN(lcFieldCont,"-",""))

  IF llApGlLink .AND. lcFieldCont <> lcEmptyAcs

    SELECT('lcLinkChar')
    SET ORDER TO TAG ACCTCODE

    IF !SEEK(lcFieldCont) .OR. ATC('?',lcFieldCont) > 0
      DIMENSION laTemp[2]
      laTemp = ''
      lcSavBrFld=lcBrfields
      lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description'"

      lcFile_Ttl="Chart of accounts"
        
      =gfbrows(' ','CACCTCODE,CACCNLDES','laTemp')

      lcFile_Ttl=lcSavTitle
      lcBrfields=lcSavBrFld

      IF !EMPTY(laTemp[1])
        lcFieldCont = ALLTRIM(laTemp[1])
      ELSE
        lcFieldCont = REPLICATE('0',lnApsAcLen)  
      ENDIF
    ENDIF  
 
    IF !EMPTY(lcSavAlias)
      SELECT(lcSavAlias)
    ENDIF  
    lcVarName = SYS(18)
    
    *Blank the account code if the value equal 0.
    &lcVarName= IIF(VAL(lcFieldCont) = 0 , "" , lcFieldCont)

  ENDIF
ENDIF

*Blank the account code if the value equal 0 and there is no GL link.
IF VAL(lcFieldCont) = 0 .AND. llApGlLink 
  lcVarName  = SYS(18)
  &lcVarName = ""
ENDIF

IF !EMPTY(lcSavAlias)
  SELECT(lcSavAlias)
ENDIF  
*-- End of lfvApAcCod.


*!**************************************************************************
*! Name      : lfColData
*! Developer : Khalid Mohi El-Din
*! Date      : 07/07/2000
*! Purpose   : Is used to get all the AP invoices according to the
*!             selected criteria and insert it in a temporary file to 
*!             be used in the print process.
*!**************************************************************************
*! Example   :  =lfColData()
*!**************************************************************************
*!
*!**************************************************************************
FUNCTION lfColData

*-- Creating the temporary file by copying the structure of APINVHDR and add
*-- two new fields nTotPay to hold the total payable invoices, and lCanPrint
*-- to determine whether to print the objects in the FRX or not.
SELECT APINVHDR
=AFIELD(laApInvHdr)
lnFileStru = ALEN(laApInvHdr,1)
DIMENSION laApInvHdr[lnFileStru+2,4]
laApInvHdr[lnFileStru+1,1] = 'nTotPay'
laApInvHdr[lnFileStru+1,2] = 'N'
laApInvHdr[lnFileStru+1,3] = 14
laApInvHdr[lnFileStru+1,4] = 2
laApInvHdr[lnFileStru+2,1] = 'lCanPrint'
laApInvHdr[lnFileStru+2,2] = 'L'
laApInvHdr[lnFileStru+2,3] = 0
laApInvHdr[lnFileStru+2,4] = 0

CREATE TABLE (gcWorkDir+TmpApInvHd) FROM ARRAY laApInvHdr
=gfOpenFile(gcWorkDir+TmpApInvHd,' ','EX')

*-- Setting the index 
IF EMPTY(laOGFxFlt[1,6]) .AND. lcRpCurr <> 'F' 
  INDEX ON cVendCode+cCurrCode+cInvNo  TAG (TmpApInvHd) OF (TmpApInvHd)
ELSE
  INDEX ON cCurrCode+cVendCode+cInvNo  TAG (TmpApInvHd) OF (TmpApInvHd)
ENDIF

*-- The FOR condition of the SCAN command in the APINVHDR. Where it holds
*-- the selected criteria.
lcForExp = 'FOR '+lcRpExp+ '.AND.' + IIF(EMPTY(lcRpRefnce),".T.",' LIKE("'+STRTRAN(lcRpRefnce,' ','?')+'",APINVHDR.cinvref)')

SELECT APINVHDR
SCAN &lcForExp
  WAIT WINDOW 'Payable invoice # : '+ cInvNo NOWAIT
     
  *-- Calculating the Total payable.
  lnAmtToPay = nInvAmnt-nInvPaid-nInvDistk-nInvAdj

  *-- Calling lfSumAll() to get the open amount of the invoice within
  *-- the date that was entered in the Option Grid.
  lnAmtToPay = lnAmtToPay + lfSumall(cVendCode,cInvNo)
  
  *-- Check if there is an open amount within the selected date then add
  *-- a new record to the temporary file.
  IF lnAmtToPay <> 0
     SCATTER MEMVAR MEMO
     INSERT INTO &TmpApInvHd FROM MEMVAR
     REPLACE &TmpApInvHd..nTotPay WITH lnAmtToPay,;
             &TmpApInvHd..lCanPrint WITH llCanPrint
      
  ENDIF 
ENDSCAN
WAIT CLEAR
  
*-- Check if the temporary file was empty then display a notification
*-- message that no record was selected.
SELECT &TmpApInvHd
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SET RELATION TO cvendcode        INTO APVENDOR ADDITIVE
SET RELATION TO CINVNO+CVENDCODE INTO APDIST ADDITIVE

DO gfDispRe WITH EVAL('lcRpForm')
SET RELATION TO  
SELECT APDIST
SET RELATION TO

USE IN (TmpApInvHd)

*-- Delete the temporary file with its related index.
ERASE(gcWorkDir+TmpApInvHd+'.DBF')
ERASE(gcWorkDir+TmpApInvHd+'.CDX')  
RETURN
*:*******************************************************************************
*: Name      : lfvMakExch
*: Developer : Albert Raif  (ALB)
*: Date      : 08/29/2002
*: Purpose   : Return the FRX for AP ageing report and Exchange the period value
*:*******************************************************************************
*: Calls     : NONE
*:*******************************************************************************
*: Returns            : 
*:*******************************************************************************
*: Example            : =lfvMakExch()
*:*******************************************************************************
*: C102560
FUNCTION lfvMakExch
DO CASE
  CASE llRpConPay AND llInvDate
    lcRpForm = IIF(lcRpCType='S',"APAGECSV","APAGECDV")
  CASE !llRpConPay AND llInvDate
    lcRpForm = IIF(lcRpCType='S',"APAGEDSV","APAGEDDV")
  CASE llRpConPay AND !llInvDate
    lcRpForm = IIF(lcRpCType='S',"APAGECS","APAGECD")
  CASE !llRpConPay AND !llInvDate
    lcRpForm = IIF(lcRpCType='S',"APAGEDS","APAGEDD")
ENDCASE
FOR lnCount = 1 TO 2
  lcCount = ALLTRIM(STR(lnCount))
  lnOldVal = lnRpPrd&lcCount
  lcExVal = ALLTRIM(STR(4-(lnCount-1)))
  lnRpPrd&lcCount = lnRpPrd&lcExVal
  lnRpPrd&lcExVal = lnOldVal
  SHOW GET lnRpPrd&lcCount
  SHOW GET lnRpPrd&lcExVal
ENDFOR
=lfvCurDat()