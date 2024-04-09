*B500790,1 MAN 06/21/95 Changing the numeric fields picture in the FRX
*B500790,1              to print the commas   
*E300301,1 MAN 10/25/95 Adding the session # in the variable filter
*E300296,1 M.H 12/18/95 Add the currency to the AP reports.
*B600904,1 M.H 01/02/96 When you enter the option grid you have the invoicce 
*B600904,1              status option = 'OPEN' but if you click on preview 
*B600904,1              or print the report will display all the invoices
*B600904,1              so I fixed this bug by calling the lfvInvoice in
*B600904,1              the report show function.
*B601013,1 REN 04/02/96 Correct the spelling of 'Curruncy' to 'Currency'
*B601013,1              in APPYINVD.FRX
*B800859,1 M.H 11/26/96 Change the date range to work for both the invoice
*B800859,1              header and the distribution.
*B601483,1 HS  12/10/96 Bug is not found in this version
*E300663,1 HS  04/24/97 Make some changes for we have change the file
*E300663,1              SYCSEQUN name to SEQUENCE and make it a data file
*E300663,1              [Change its dir. from SYSFILES to DBFS]
*B602072,1 RENEE 08/20/98 Fix the problem of sometimes getting into an 
*B602072,1 				  infinite loop when filtering on a session number
*B802483,1 AKA 07/28/99 Fix bug in AP invoice detail didn't show paid invoice if you choose
*B802483,1              "Paid" or "All".
*B603131,1 Walid Abou El-Magd 08/30/99 Add currency code field as a fixed filter
*B603131,1              and print the grand total.
*B603432,1 AKA 02/10/00 When you enter the option grid you have the invoicce 
*B603432,1              status option = 'OPEN' but if you click on preview 
*B603432,1              or print the report will display all the invoices
*B603432,1              so I fixed this bug by calling the lfvInvoice in
*B603432,1              the report show function. Beacuse the invoice status is 
*B603432,1              defined as varibale not as a filter expression, so I will
*B603432,1              call a function lfvInvoice() before calling the report just to 
*B603432,1              build the report expression. 
*B603522,1 SSE 03/19/2000 Convert all objects in FRX to TRANSPARENT
*B604005,1 MHM 11/02/00  Fix bug of Numeric overflow in AP reports (modified in reports only)
*B604005,1 MHM            we modify the follwoing fields in FRX  in (open balance changed)
*B604005,1 MHM             in all its coulmn and modify invoice amount in details band
*T20061011.0006 TMI 11/05/2006 if a vendor is selected then colloct the related lines from APINVHDR into a temp file with the alias APINVHDR to enhance speed
************************************************************************

*T20061011.0006 TMI [Start] collect selected vendors from apinvhdr file in a temp file with the samem alias
STORE '' TO lcOpnTmp, lcSvOrd, lcApInvHdr
llOpnTmp = lfGetInvH()
*T20061011.0006 TMI [End  ] 

*SELECT APDIST
*SET ORDER TO TAG VENCODE IN APVENDOR
*SET ORDER TO TAG VENDINV IN APINVHDR
*SET RELATION TO apdist.cvendcode + apdist.cinvno INTO APINVHDR ADDITIVE

*SET RELATION TO apdist.cvendcode INTO APVENDOR ADDITIVE
SELECT APINVHDR

*B603131,1 Use a new tmp index on the master file to support the currency groupping [Start]
*SET ORDER TO TAG VENDINV
=lfCreatNdx()
*B603131,1 Use this index to support the currency groupping [End..]

SET ORDER TO TAG VENCODE IN APVENDOR
SET ORDER TO TAG INVVEND IN APDIST
SET RELATION TO APINVHDR.CINVNO + APINVHDR.CVENDCODE INTO APDIST ADDITIVE
SET RELATION TO APINVHDR.CVENDCODE INTO APVENDOR ADDITIVE
SET SKIP TO APDIST
*E300301,1 MAN Adding the session # in the variable filter if the session # 
*E300301,1     is entered
*DO gfDispRe WITH EVAL('lcRpForm'),'FOR '+lcRpExp+ ' AND' + IIF(EMPTY(lcRpRefnce),".T.",' LIKE("'+STRTRAN(lcRpRefnce,' ','?')+'",APINVHDR.cinvref)')

*B800859,1 M.H Begin.
IF ATC([apdist.cfspprdid +'-'+ apdist.cfisfyear],lcRpExp) > 0
  llFound = .F.
  FOR lnCount = 1 TO ALEN(laOGVrFlt,1)
    IF laOGvrFlt[lnCount,1] = [apdist.cfspprdid +'-'+ apdist.cfisfyear]
     llFound = .T.
     EXIT
    ENDIF
  ENDFOR
  IF llFound
    DIMENSION laTmpArray[1,ALEN(laOGvrFlt,2)]
    laTmpArray[1,1] = [apinvhdr.cfspprdid +'-'+ apinvhdr.cfisfyear]
    FOR lnColumn = 2 TO ALEN(laOGvrFlt,2)
      laTmpArray[1,lnColumn] = laOGvrFlt[lnCount,lnColumn]
    ENDFOR 
    lcExpresion = GFGENFLT('laTmpArray',.T.)
    IF !EMPTY(lcExpresion)
      lcRpExp = lcRpExp + ' AND ' + lcExpresion
    ENDIF  
  ENDIF
ENDIF
*B800859,1 M.H End.

*B603432,1 AKA 02/10/00 [Start]
=lfvInvoice()
*B603432,1 AKA 02/10/00 [End]

*B802483,1 AKA (Start)
IF !EMPTY(lcStatExp)
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp) , '' , ' AND ') + ' ( ' + lcStatExp  + ')'
ENDIF
*B802483,1 AKA (End)

*B603131,1 [Start]
*-- llAllCurr : This variable is defined in syrepuvr to control the apperance
*--             of grand total objects.
*llAllCurr=IIF(lcRpCurr='F' AND EMPTY(laOGFxFlt[1,6]),.F.,.T.)
*B603131,1 [End..]

DO gfDispRe WITH EVAL('lcRpForm'),'FOR '+lcRpExp+ ' AND' + IIF(EMPTY(lcRpRefnce),".T.",' LIKE("'+STRTRAN(lcRpRefnce,' ','?')+'",APINVHDR.cinvref)')+;
                 IIF(EMPTY(lcRpSess),'',' .AND. lfFltSess()')

SET RELATION TO

*T20061011.0006 TMI [Start] if temp file with the selected vendor is created , then open the real APINVHDR file and remove the temp one
IF llOpnTmp
  SELECT APINVHDR
  USE (gcDataDir+'APINVHDR') ORDER &lcSvOrd   
  IF FILE(gcWorkDir +lcApInvHdr+ '.CDX') 
    ERASE (gcWorkDir +lcApInvHdr+ '.CDX')
  ENDIF
ENDIF
*T20061011.0006 TMI [End  ] 

*!**************************************************************************
*!
*!      Function: lfRepShow
*!
*!**************************************************************************
* 
FUNCTION lfRepShow
*E300296,1 M.H 12/18/95 Add the currency to the AP reports.
laOGObjCnt[6] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")
*E300296,1 M.H 12/18/95 End.
*B600904,1 M.H 01/02/96 Begin.
*B600904,1 M.H 01/02/96 When you enter the option grid you have the invoicce 
*B600904,1              status option = 'OPEN' but if you click on preview 
*B600904,1              or print the report will display all the invoices
*B600904,1              so I fixed this bug by calling the lfvInvoice in
*B600904,1              the report show function.
=lfvInvoice()
*B600904,1              End.

*!**************************************************************************
*!
*!      Function: lfvInvoice
*!
*!**************************************************************************
* 
FUNCTION lfvInvoice

*B802483,1 AKA (Start)
lcStatExp  = ""
*B802483,1 AKA (End)

DO CASE
  CASE lcRpStatus = 'O'
    SHOW GET llRpIncInv ENABLE

    *B802483,1 AKA (Start)    
    lcStatExp = " ABS(APINVHDR.NINVAMNT) > ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK) "
    IF !llRpIncInv
      lcStatExp = lcStatExp  + " .AND. "  + " APINVHDR.CINVSTAT  <> 'V' "
    ENDIF
    
    *DIMENSION laOGHDFlt[IIF(!llRpIncInv,3,2),7]
    *laOGHDFlt[2,1] = [ABS(APINVHDR.NINVAMNT)]
    *laOGHDFlt[2,2] = 'F'    
    *laOGHDFlt[2,3] = 'N'    
    *laOGHDFlt[2,4] = .T.    
    *laOGHDFlt[2,5] = 'Greater Than'    
    *laOGHDFlt[2,6] = [ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK)]    
    *laOGHDFlt[2,7] = 'E'
    *IF !llRpIncInv
    *  laOGHDFlt[3,1] = [APINVHDR.CINVSTAT]
    *  laOGHDFlt[3,2] = 'F'    
    *  laOGHDFlt[3,3] = 'C'    
    *  laOGHDFlt[3,4] = .F.    
    *  laOGHDFlt[3,5] = 'Like'    
    *  laOGHDFlt[3,6] = 'V'    
    *  laOGHDFlt[3,7] = 'V'        
    *ENDIF
    *B802483,1 AKA (End)
    
  CASE lcRpStatus = 'P'
    llRpIncInv=.F.
    SHOW GET llRpIncInv DISABLE

    *B802483,1 AKA (Start)    
    lcStatExp = " ABS(APINVHDR.NINVAMNT)  = ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK) "    
    
    *DIMENSION laOGHDFlt[2,7]
    *laOGHDFlt[2,1] = [APINVHDR.NINVAMNT]
    *laOGHDFlt[2,2] = 'F'    
    *laOGHDFlt[2,3] = 'N'    
    *laOGHDFlt[2,4] = .T.    
    *laOGHDFlt[2,5] = 'Like'    
    *laOGHDFlt[2,6] = [APINVHDR.NINVPAID+APINVHDR.NINVADJ+APINVHDR.NINVDISTK]        
    *laOGHDFlt[2,7] = 'E'
    *B802483,1 AKA (End)
    
  CASE lcRpStatus = 'A'  
    SHOW GET llRpIncInv ENABLE  

    *B802483,1 AKA (Start)
    IF !llRpIncInv
      lcStatExp = " APINVHDR.CINVSTAT  <> 'V' "
    ENDIF
    
    *DIMENSION laOGHDFlt[IIF(!llRpIncInv,2,2),7]
    *IF !llRpIncInv
    *  laOGHDFlt[2,1] = [APINVHDR.CINVSTAT]
    *  laOGHDFlt[2,2] = 'F'    
    *  laOGHDFlt[2,3] = 'C'    
    *  laOGHDFlt[2,4] = .F.    
    *  laOGHDFlt[2,5] = 'Like'    
    *  laOGHDFlt[2,6] = 'V'    
    *  laOGHDFlt[2,7] = 'V'        
    *ENDIF    
    *B802483,1 AKA (End)
    
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvSess
*!
*!**************************************************************************
*E300301,1 MAN to Validate the entered session #
FUNCTION lfvSess
PRIVATE ALL LIKE lcOldAl*

IF EMPTY(lcRpSess)
  RETURN
ENDIF
*MAN
PRIVATE llSeqisOpen
llSeqisOpen = USED("SEQUENCE")

*E300663,1 Change this line for the changes we have made to SYCSEQUN [Begin]
*IF !USED('SYCSEQUN')
IF !USED('SEQUENCE')
*E300663,1 Change this line for the changes we have made to SYCSEQUN [End]

  lcOldAl = ALIAS()
  SELECT 0

  *E300663,1 Change this line for the changes we have made to SYCSEQUN [Begin]
  *USE &gcSysHome.SYCSEQUN ORDER CSEQ_TYPE 
  USE &gcDataDir.SEQUENCE ORDER CSEQ_TYPE 
  *E300663,1 Change this line for the changes we have made to SYCSEQUN [End]

  IF EMPTY(lcOldAl)
    SELECT 0
  ELSE
    SELECT (lcOldAl)
  ENDIF  
ELSE

  *E300663,1 Change this line for the changes we have made to SYCSEQUN [Begin]
  *SET ORDER TO TAG CSEQ_TYPE IN SYCSEQUN
  SET ORDER TO TAG CSEQ_TYPE IN SEQUENCE
  *E300663,1 Change this line for the changes we have made to SYCSEQUN [End]

ENDIF  

lcRpSess= PADL(ALLTRIM(lcRpSess),8,'0')
  
*E300663,1 Change this lines for the changes we have made to SYCSEQUN [Begin]
*IF SEEK('APSESS'+gcAct_Comp,'SYCSEQUN')
*  IF !BETWEEN(VAL(lcRpSess),1,SYCSEQUN.nSeq_No)
*    =gfModalGen("TRM04158B00000","DIALOG",ALLTRIM(STR(SYCSEQUN.nSeq_No)))

*HAYTHAR Change this line for we have changed the index tag (CSEQ_TYPE)
*expression [Begin]
IF SEEK('CAPSESSNO ','SEQUENCE')
*HAYTHAR Change this line for we have changed the index tag [End]
  
  IF !BETWEEN(VAL(lcRpSess),1,SEQUENCE.nSeq_No)
    =gfModalGen("TRM04158B00000","DIALOG",ALLTRIM(STR(SEQUENCE.nSeq_No)))
*E300663,1 Change this lines for the changes we have made to SYCSEQUN [End]
    *MAN
    IF !llSeqisOpen
      USE IN SEQUENCE
    ENDIF  
    lcRpSess = ''
    SHOW GET lcRpSess
    RETURN 
  ENDIF
ENDIF  
*MAN
IF !llSeqisOpen
  USE IN SEQUENCE
ENDIF  

*!**************************************************************************
*!
*!      Function: lfFltSess
*!
*!**************************************************************************
*E300301,1 MAN To check if the one of distribution lines for the current
*E300301,1     invoice has the entered session number
FUNCTION lfFltSess

lcOldFl = ALIAS()
SELECT APDIST
lnDisRec = RECNO()
LOCATE REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = APINVHDR.CINVNO + APINVHDR.CVENDCODE ;
            FOR   cApSessNo = lcRpSess
*B602072,1 Restore the record number regardless of an ap session number
*B602072,1 being found or not, because it was distorting the APINVHDR\APIDST
*B602072,1 relation
*IF FOUND()
IF BETWEEN(lnDisRec, 1, RECCOUNT()) 
*B602072,1  end
  GO lnDisRec
ENDIF        
IF EMPTY(lcOldFl)
  SELECT 0
ELSE  
  SELECT (lcOldFl)      
ENDIF
RETURN FOUND("APDIST")

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
*E300296,1 M.H 12/18/95 Add the currency to the AP reports.
*
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfwCurCode
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
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
*B603131,1 New when function to save the old currency code.

FUNCTION lfwCurCode

lcOldCurr = laOGFxFlt[1,6]

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
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
*B603131,1 New valid function to validate the currency code.

FUNCTION lfvCurCode
lnAlias=SELECT(0)

IF EMPTY(laOGFxFlt[1,6]) 
  RETURN
ENDIF

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

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfRepShow
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B603131,1
*
FUNCTION lfRepShow

laOGFxFlt[1,6]= gcBaseCurr
=lfOGShowGet("lnRepCurr")


*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
*B603131,1 Walid Abou El-Magd
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
*B603131,1 Walid Abou El-Magd
FUNCTION lfCreatNdx
SELECT APINVHDR
* If index name is already defined 
IF TYPE("lcApInvHdr") <> 'U'
  * If index file is already creadted 
  IF !FILE(gcWorkDir +lcApInvHdr+ '.CDX') 
    lcApInvHdr = gfTempName()
    INDEX ON cvendcode+ccurrcode+cinvno TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')
  ELSE
    SET ORDER TO TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')    
  ENDIF
ELSE    && ELASE OF  "If index name is already defined "
    lcApInvHdr = gfTempName()          
    INDEX ON cvendcode+ccurrcode+cinvno  TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')    
ENDIF


*:**************************************************************************
*:* Name        : lfGetInvH
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/05/2006
*:* Purpose     : if a vendor is selected then colloct the related lines from APINVHDR into a temp file with the alias APINVHDR to enhance speed
*:***************************************************************************
*T20061011.0006
FUNCTION lfGetInvH
PRIVATE lnVnPos, lnCurrPos, llRet, laFlds, lcVndr, lcSetToOrd, lcCurr
lcSvOrd  = ORDER('APINVHDR')
lcOpnTmp = gfTempName()

llRet = .F.
lnCurrPos = ASCAN(laOgFxFlt,'APINVHDR.CCURRCODE')
lnVnPos   = ASCAN(laOgVrFlt,'APDIST.CVENDCODE')

lcCurr = IIF( lnCurrPos>0 AND !EMPTY(laOgFxFlt[lnCurrPos,6]) , laOgFxFlt[lnCurrPos,6] , '' )

IF lnVnPos>0 .AND. !EMPTY(laOgVrFlt[lnVnPos,6])
  
  SELECT APINVHDR
  SET ORDER TO VENDINV
  
  COPY STRUCTURE TO (gcWorkDir+lcOpnTmp) WITH CDX
  USE (gcWorkDir+lcOpnTmp) IN 0
  
  lcCurrCrit = IIF( !EMPTY(lcCurr) , 'FOR CCURRCODE = "' + lcCurr + '"' , '' )
  
  lcVend = laOgVrFlt[lnVnPos,6] + '|'
  DO WHILE LEN(lcVend)>0
    lcVndr = SUBSTR(lcVend,1,8)
    lcVend = STRTRAN(lcVend,lcVndr+'|','')
    =SEEK(lcVndr,'APINVHDR')
    SCAN REST WHILE CVENDCODE+CINVNO = lcVndr &lcCurrCrit
      SCATTER MEMVAR
      WAIT WINDOW NOWAIT M.CVENDCODE 
      INSERT INTO (gcWorkDir+lcOpnTmp) FROM MEMVAR
    ENDSCAN  
  ENDDO  
  
  USE IN APINVHDR
  SELECT &lcOpnTmp
  USE (gcWorkDir+lcOpnTmp) ALIAS APINVHDR ORDER &lcSvOrd
  llRet = .T.
ENDIF

*- if no vendor is selected but a currency is there
IF !llRet AND !EMPTY(lcCurr)
  SELECT APINVHDR
  SET ORDER TO CURVENINV
  
  WAIT WINDOW NOWAIT lcCurr
  =SEEK(lcCurr,'APINVHDR')
  COPY REST WHILE CCURRCODE+CVENDCODE+CINVNO = lcCurr WITH CDX TO (gcWorkDir+lcOpnTmp)
  
  SELECT APINVHDR
  USE (gcWorkDir+lcOpnTmp) ALIAS APINVHDR ORDER &lcSvOrd
  llRet = .T.
ENDIF

WAIT CLEAR

RETURN llRet
*-- end of lfGetInvH.
