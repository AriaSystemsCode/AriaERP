*:***************************************************************************
*: Program file  : APCHKPRV
*: Program desc. : Print Check Report
*: System        : Aria Advantage Series.Aria4XP
*: Module        : AP
*: Developer     : Mariam Mazhar{MMT}
*: Date          : 10/19/2009
*: Reference     : N000635[Task:T20090907.0009 Project:AP Conversion]
*:***************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : gfDispRe,gfModalGen,gfTempName
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO APCHKPRV
*!***************************************************************************
*! Modification:
*!N000635,2 MMT 01/04/2010 Convert AP Print Check report to Aria4 [T20091122.0011]
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [T20091122.0011]
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [T20091122.0011]
*N000635,5 TMI 07/19/2011 Enable the report to work in A4xp 
*N000635,6 TMI 07/29/2011 run under the shared folder only , this needs to be fixed later
*:***************************************************************************
IF !llTestChk
  IF !lfReValidate()    && Check if the data validated or not
    RETURN .F.
  ENDIF
ENDIF 

*N000635,6 TMI 07/29/2011 [Start] run under shared folder only
*IF oAriaApplication.MULTIINST 
IF .F. 
*N000635,6 TMI 07/29/2011 [End  ] 
  *N000635,5 TMI 07/19/2011 [Start] run the report from the reports folder 
  *SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\ap\apchkprv.FXP ADDITIVE
  *DO X:\ARIA4XP\SRVRPTS\ap\apchkprv.FXP  
  SET PROCEDURE TO (oAriaApplication.ClientReportHome+'AP\apchkprv.FXP') ADDITIVE
  DO (oAriaApplication.ClientReportHome+'AP\apchkprv.FXP')
  *N000635,5 TMI 07/19/2011 [End  ] 
ELSE
  *N000635,5 TMI 07/19/2011 [Start] run the report from the reports folder 
  *lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
  *DO lcSrvRpt+"ap\apchkprv.FXP" WITH .F.,.F.
  DO (oAriaApplication.ReportHome+"ap\apchkprv.FXP") WITH .F.,.F.
  *N000635,5 TMI 07/19/2011 [End  ] 
  
ENDIF   
RETURN   


*!*************************************************************
*! Name      : lfIncNxtChk
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Update check number field
*!************************************************************************
FUNCTION lfIncNxtChk
lcNxtChkNo = PADL(INT(VAL(lcNxtChkNo)) + 1,8,'0')
RETURN ''

*!*************************************************************
*! Name      : lfAllgTest
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Alignment test validation
*!************************************************************************
*
FUNCTION lfAllgTest

llEndGrp  = .F.
lnTotal   = 0

lnNoOfFields = lfCreateCur()
SELECT (lcRpTargt)
APPEND BLANK
FOR lnCount = 1 TO lnNoOfFields
  lcFieldName = FIELD(lnCount)
  DO CASE

    CASE TYPE(lcFieldName) = 'C'
      REPLACE &lcFieldName WITH REPLICATE('X',FSIZE(lcFieldName))

    CASE TYPE(lcFieldName) = 'N'
      REPLACE &lcFieldName WITH VAL(REPLICATE('9',MIN(FSIZE(lcFieldName),6)))

    CASE TYPE(lcFieldName) = 'L'
      REPLACE &lcFieldName WITH .T.

    CASE TYPE(lcFieldName) = 'D'
      REPLACE &lcFieldName WITH DATE()

  ENDCASE
ENDFOR

llPrintChk = .F.
llTestChk  = .T.
oAriaApplication.gcDevice = "PRINTER"
loOgScroll.ll2Printer = .T.
loogScroll.ReportPrint()
=loOGScroll.RefreshScroll()
llTestChk = .F.
USE IN (lcRpTargt)

*!*************************************************************
*! Name      : lfwGrid  
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : When Function
*!************************************************************************
*
FUNCTION lfwGrid  


IF (TYPE('lcSession') = 'U' OR (TYPE('lcSession') = 'C' AND EMPTY(lcSession)))
  lcSession = gfSequence('CAPSESSNO')
ENDIF 

PRIVATE lcTitle
lcTitle = PROPER(ALLTRIM(WTITLE('GWDGRID')))
IF !('Session : ' $ loogScroll.Parent.Caption)
  loogScroll.Parent.Caption =  '(' + ALLTRIM(oAriaApplication.ActiveCompanyID) + ') ' + ;
                 IIF(EMPTY(oAriaApplication.ActiveCompanyName), ;
                     '', ;
                     '- ' + ALLTRIM(oAriaApplication.ActiveCompanyName) + ' - ') +" " + ALLTRIM(SYDREPRT.cRep_Name) + " " +'  Session : ' + lcSession
ENDIF
lcOGWinTitl = loogScroll.Parent.Caption 

IF !USED('APSETUP')
  =gfOpenTable('APSETUP','APSETUP','SH')
  SELECT 'APSETUP'
  gfSeek('')
ENDIF 

IF APSETUP.CAPSGLLINK = 'Y'
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
  IF USED('SYCCOMP')
    =gfCloseTable('SYCCOMP')
  ENDIF 
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
  IF !USED('SYCCOMP')
    =gfOpenTable('SYCCOMP','CCOMP_ID','SH')   
    SELECT 'SYCCOMP'
    =GFSEEK('')
  ENDIF   
  SELECT 'SYCCOMP'
  =SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP','CCOMP_ID')
  lcDataDir = IIF(EMPTY(SYCCOMP.cCompPrnt)              , ;
              ALLTRIM(SYCCOMP.cCom_dDir), ;
              ALLTRIM(LOOKUP(SYCCOMP.CCOM_DDIR,APSETUP.cApSgLCom,SYCCOMP.CCOMP_ID,"CCOMP_ID")))
  lcDataDir = oAriaApplication.GetDataDir(lcDataDir)
  IF FILE(lcDataDir+'GLACCHAR.DBF')
    SELECT 0  
    =gfOpenTable(ADDBS(lcDataDir)+'GLACCHAR','ACCTCODE','SH','lcLinkChar')
    llOpenAcHar = .T.
    SELECT('lcLinkChar')
    gfSetOrder('ACCTCODE')
  ENDIF
ENDIF  

DIMENSION laOpenFile [10,3]
laOpenFile = .F.

laOpenFile [1,1] = 'APVENDOR'
IF !USED('APVENDOR')
  =gfOpenTable('APVENDOR','VENCODE')
  laOpenFile [1,2] = .T.
ENDIF

SELECT APVENDOR
laOpenFile [1,3] = ORDER()
=gfSetOrder('VENCODE')

laOpenFile [2,1] = 'APVENHST'
IF !USED('APVENHST')
  =gfOpenTable('APVENHST','VENDYEAR')
  *USE &gcDataDir.APVENHST IN 0
  laOpenFile [2,2] = .T.
ENDIF

SELECT APVENHST
laOpenFile [2,3] = ORDER()
=gfSetOrder('VENDYEAR')

laOpenFile [3,1] = 'APBANKS'
IF !USED('APBANKS')
  =gfOpenTable('APBANKS','BANKCODE')
  laOpenFile [3,2] = .T.
ENDIF

SELECT APBANKS
laOpenFile [3,3] = ORDER()
=gfSetOrder('BANKCODE')
=gfSEEK(lcRpBnkCod)

*****************************

laOpenFile [4,1] = 'APCHECKS'
IF !USED('APCHECKS')
  =gfOpenTable('APCHECKS','BANKCHECK')
  laOpenFile [4,2] = .T.
ENDIF

SELECT APCHECKS
laOpenFile [4,3] = ORDER()
=gfSetOrder('BANKCHECK')
=gfSEEK(lcRpBnkCod+lcRpChkAct)

*****************************

laOpenFile [5,1] = 'APINVHDR'
IF !USED('APINVHDR')
  =gfOpenTable('APINVHDR','INVVEND')
  laOpenFile [5,2] = .T.
ENDIF

SELECT APINVHDR
laOpenFile [5,3] = ORDER()
=gfSetOrder('INVVEND')


*****************************

laOpenFile [6,1] = 'APPAYMNT'
IF !USED('APPAYMNT')
  =gfOpenTable('APPAYMNT','TYPMETHDOC')
  laOpenFile [6,2] = .T.
ENDIF

SELECT APPAYMNT
laOpenFile [6,3] = ORDER()
gfSetOrder('TYPMETHDOC')
*****************************

laOpenFile [7,1] = 'APDIST'
IF !USED('APDIST')
  =gfOpenTable('APDIST','PAYMNTS')
  laOpenFile [7,2] = .T.
ENDIF

SELECT APDIST
laOpenFile [7,3] = ORDER()
=gfSetOrder('PAYMNTS')


*****************************

laOpenFile [8,1] = 'SYCFACT'
IF !USED('SYCFACT')
  =gfOpenTable('SYCFACT','CFACCODE')
  laOpenFile [8,2] = .T.
ENDIF

SELECT SYCFACT
laOpenFile [8,3] = ORDER()
=gfSetOrder('CFACCODE')


*****************************

laOpenFile [9,1] = 'SYCINT'
IF !USED('SYCINT')
  =gfOpenTable('SYCINT','CCONTCODE')
  laOpenFile [9,2] = .T.
ENDIF

SELECT SYCINT
laOpenFile [9,3] = ORDER()
gfSetOrder('CCONTCODE')

*****************************

laOpenFile [10,1] = 'APDIV'
IF !USED('APDIV')
  =gfOpenTable('APDIV','DIVISION')
  laOpenFile [10,2] = .T.
ENDIF

SELECT APDIV
laOpenFile [10,3] = ORDER()
=gfSetOrder('DIVISION')



IF !USED('CODES')
  =gfOpenTable('CODES','CODES')
ENDIF
lcPrintMod = lcRpChkMod
lcOldMode = lcRpChkMod
STORE '' TO lcRpChkFrm,lcRpChkTo

IF loogScroll.lcOGPrgName <>  'APCHKPR'+lcPrintMod
   lcOldMode = 'X'
   lfvPrintMode(.T.)
ENDIF 
IF loogScroll.lnOGSeting = 1
  lcOldMode = 'X'
  lcPrintMod = "V"
  loogScroll.lcOGPrgName = 'APCHKPR'+'V'
  lfvPrintMode(.T.)
ENDIF  
loogScroll.lcOGPrgName = 'APCHKPR'+'V'
loogScroll.lcOGManRep= 'APCHKPR'+'V'

*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [START]
*lcRepForm = 'APCHKPWL'
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [END]
DIME laFrxFiles[1,4]
STORE '' TO laFrxFiles
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start]
*=ADIR(laFrxFiles,oAriaApplication.ReportHome+'*.FRX')
=ADIR(laFrxFiles,IIF(oAriaApplication.MULTIINST,oAriaApplication.clientreporthome,oAriaApplication.ReportHome)+'*.FRX')
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End]
IF !EMPTY(laFrxFiles[1,1])
  IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__' + oAriaApplication.ActiveCompanyID)+'.FRX')>0 
    lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + oAriaApplication.ActiveCompanyID)
  ELSE  
    IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
      lcRepForm = '__' + SUBSTR(lcRepForm , 3)
    ELSE 
      **!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start]
      *lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')   
      lcSrvRpt = STRTRAN(UPPER(IIF(oAriaApplication.MULTIINST,oAriaApplication.clientreporthome,oAriaApplication.ReportHome)),'REPORTS','SRVRPTS')   
      **!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End]
      DIME laFrxFiles[1,4]
      STORE '' TO laFrxFiles
      =ADIR(laFrxFiles,lcSrvRpt+'*.FRX')
      IF !EMPTY(laFrxFiles[1,1])
        IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__' + oAriaApplication.ActiveCompanyID)+'.FRX')>0 
          lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + oAriaApplication.ActiveCompanyID)
        ELSE
          IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
            lcRepForm = '__' + SUBSTR(lcRepForm , 3)
          ENDIF   
        ENDIF 
      ENDIF
    ENDIF
  ENDIF
ENDIF  
loogScroll.CheckPlatForm()

IF ASCAN(LAOGOBJTYPE,'LCRPGLACCT') # 0
  LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCRPGLACCT'),1)
  LAOGOBJCNT[LNPOS] = IIF(lcPrintMod = 'V',.F.,.T.)
  = LFOGSHOWGET('LCRPGLACCT')
ENDIF

IF lcPrintMod <> 'R'  
  lcNxtChkNo = IIF(gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),PADL(APCHECKS.nChkNxtPn,8,'0'),'00000001')
ENDIF


IF TYPE('llCallFromRequestHandler') = 'L' .AND. llCallFromRequestHandler
  IF ASCAN(LAOGOBJTYPE,'LCPRINTMOD') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCPRINTMOD'),1)
    LAOGOBJCNT[LNPOS] = .F.
    = LFOGSHOWGET('LCPRINTMOD')
  ENDIF
  
  IF ASCAN(LAOGOBJTYPE,'LNALTEST') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LNALTEST'),1)
    LAOGOBJCNT[LNPOS] = .F.
    = LFOGSHOWGET('LNALTEST')
  ENDIF
ENDIF 


*!*************************************************************
*! Name      : lfvGrid  
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : validation Function
*!*************************************************************************
FUNCTION lfvGrid    
FOR lnCounter = 1 TO ALEN(laOpenFile,1)
  IF laOpenFile[lnCounter,2] 
    IF USED(ALIAS(laOpenFile[lnCounter,1]))
      =gfCloseTable(laOpenFile[lnCounter,1])
    ENDIF  
  ELSE
    SELECT (laOpenFile[lnCounter,1])
    =gfSetOrder(laOpenFile[lnCounter,3])
  ENDIF
ENDFOR
        
*!*************************************************************
*! Name      : lfvBnkCode   
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate bank code
*!*************************************************************************
FUNCTION lfvBnkCode   
IF EMPTY(lcRpBnkCod)
  IF lcPrintMod = 'R' 
    STORE '' TO lcRpChkFrm,lcRpChkTo
  ENDIF
  lcRpChkAct = ''
  lcNxtChkNo = ''
  lcRpGlAcct = ''
  RETURN 
ENDIF


lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lcOldVal = lcCurVar.oldvalue


SELECT APBANKS
=gfSeek('')
IF '?' $ lcRpBnkCod .OR. !SEEK(lcRpBnkCod)
  DECLARE laRpRetFld(1)
  lcBrFields    = 'CBnkCode:H="Code",CBNKLNDES:H="Description"'
  laRpRetFld[1] = ''
  =gfBrows([],'CBnkCode',"laRpRetFld",'Bank Codes ',.F.)
  IF EMPTY(laRpRetFld[1])  
    lcRpBnkCod = lcOldVal
  ELSE
    lcRpBnkCod = laRpRetFld[1]
  ENDIF
ENDIF

IF lcRpBnkCod <> lcOldVal .AND. !EMPTY(lcRpBnkCod)  && in case of press cancel and Empty(lcOldVal)
  =gfSEEK(lcRpBnkCod,'APCHECKS')
  lcRpChkAct = APCHECKS.CCHKACCT
  IF gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS')
    lcNxtChkNo = PADL(APCHECKS.NChkNxtPn,8,'0')
    lcRpGlAcct = APCHECKS.CCHKGLACC
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
  ELSE
    *** The bank has no checking accounts setup.
    =gfModalGen("INM04023B00000","DIALOG")
    lcRpBnkCod = lcOldVal
  ENDIF
ENDIF
        
*!*************************************************************
*! Name      : lfvChkAcct            
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate Checking account
*!*************************************************************************
*  
FUNCTION lfvChkAcct            

IF EMPTY(lcRpBnkCod)
  *** You have to enter the ð.
  
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *=gfModalGen("TRM04066B00000","DIALOG",'bank code')
   =gfModalGen("TRM04066B00000","DIALOG",LANG_bank_code)
  * N000682 ,1 Thabet Handle globalization issues [END]
  
  lcRpChkAct = ''
  IF lcPrintMod = 'R' 
    STORE '' TO lcRpChkFrm,lcRpChkTo
  ENDIF
  RETURN
ENDIF
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lcOldVal = lcCurVar.oldvalue
SELECT APCHECKS
=gfSeek('')
*** Search for the current checking account code
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [START]
*IF '?' $ lcRpChkAct .OR.!SEEK(ALLTRIM(lcRpBnkCod+lcRpChkAct))
IF '?' $ lcRpChkAct .OR.!SEEK(PADR(lcRpBnkCod,8)+PADR(lcRpChkAct,12))
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [END]
  DECLARE laRpRetFld(1)
  lcBrFields    = 'CBnkCode:H="Bank Code",CChkAcct:H="Checking account"'
  laRpRetFld[1] = ''
  =gfBrows([lcRpBnkCod],'CChkAcct',"laRpRetFld",'Bank & Check Accounts ',.F.)
  IF EMPTY(laRpRetFld[1])  
    lcRpChkAct = lcOldVal
  ELSE
    lcRpChkAct = laRpRetFld[1]
  ENDIF
ENDIF

IF lcRpChkAct <> lcOldVal
  IF EMPTY(lcRpChkAct)  && in case of press cancel and Empty(lcOldVal)
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
    lcNxtChkNo = ''
    lcRpGlAcct = ''
  ELSE
    lcNxtChkNo = PADL(APCHECKS.NChkNxtPn,8,'0')
    lcRpGlAcct = APCHECKS.CCHKGLACC
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfValidAcct    
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate GL account
*!*************************************************************************
*  
*
FUNCTION lfValidAcct    
PARAMETERS lcSendData

RETURN = IIF(APSETUP.CAPSGLLINK='Y', ;
         IIF(SEEK(ALLTRIM(lcSendData),'lcLinkChar'),.T.,.F.),.T.)

*!*************************************************************
*! Name      : lfvNxtChk   
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate Next Check Number
*!*************************************************************************
*
FUNCTION lfvNxtChk   

IF EMPTY(lcRpBnkCod)
  *** You have to enter the ð.

* N000682 ,1 Thabet Handle globalization issues [Start]
*  =gfModalGen("TRM04066B00000","DIALOG",'bank code')
    =gfModalGen("TRM04066B00000","DIALOG",LANG_bank_code)
    * N000682 ,1 Thabet Handle globalization issues [END]
  lcNxtChkNo = ''
  RETURN
ENDIF

IF EMPTY(lcRpChkAct)
  *** You have to enter the ð.
 * N000682 ,1 Thabet Handle globalization issues [Start]
*  =gfModalGen("TRM04066B00000","DIALOG",'checking account')
  =gfModalGen("TRM04066B00000","DIALOG",LANG_checking_account)
  * N000682 ,1 Thabet Handle globalization issues [END]
  lcNxtChkNo = ''
  RETURN
ENDIF

IF TYPE('llCallFromRequestHandler') = 'L' .AND. llCallFromRequestHandler AND EMPTY(lcNxtChkNo)
  RETURN   
ENDIF 

lcNxtChkNo = PADL(ALLTRIM(lcNxtChkNo),8,'0')
IF gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS')
  IF INT(VAL(lcNxtChkNo)) < APCHECKS.NChkNxtPn
    *** Next printed check number cannot be less than ð.
    =gfModalGen("INM04056B00000","DIALOG",ALLTRIM(STR(APCHECKS.NChkNxtPn)))
    lcNxtChkNo = PADL(APCHECKS.NChkNxtPn,8,'0')
  ENDIF
ELSE
  *** ð account not valid.

* N000682 ,1 Thabet Handle globalization issues [Start]
*  =gfModalGen("INM04077B00000","DIALOG",'Checking')
     =gfModalGen("INM04077B00000","DIALOG",LANG_Checking)
  * N000682 ,1 Thabet Handle globalization issues [END]

  lcRpChkAct = ''
  lcNxtChkNo = ''
  lcRpGlAcct = ''
ENDIF


*!*************************************************************
*! Name      : lfvChkDate   
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate  Check date
*!*************************************************************************
FUNCTION lfvChkDate   


lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
IF !lfVDTMSG(oAriaApplication.PrntCompanyID,@lcFisPrd,@lcFisYear,ldChkDat)
  ldChkDat = lcCurVar.objects[5].objects[1].oldvalue
ENDIF

*!*************************************************************
*! Name      : lfReValidate  
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Before printing this function revalid the needed informations
*!*************************************************************************
* 
FUNCTION lfReValidate  


DO CASE
  CASE lcPrintMod = 'R' 
    llSelectCheck = .F.
    lnPosCheck = ASCAN(loOgScroll.laOgFXFlt,"lcRePrnChk")
    IF lnPosCheck  > 0 
      lnPosCheck  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosCheck  ,1)
      lcCursorChk= loOgScroll.laOgFxFlt[lnPosCheck,6]
      IF !EMPTY(lcCursorChk) AND USED(lcCursorChk)
         SELECT(lcCursorChk) 
         LOCATE
        IF !EOF()
          llSelectCheck = .T.
        ENDIF 
      ENDIF 
    ENDIF 
  
    IF !llSelectCheck 
    *** ð cannot be empty.
* N000682 ,1 Thabet Handle globalization issues [Start]
*      =gfModalGen("INM04074B00000","DIALOG",'Reprint check number')
      =gfModalGen("INM04074B00000","DIALOG",LANG_Reprint_check_number)
      * N000682 ,1 Thabet Handle globalization issues [END]
      RETURN .F.
  ENDIF 
  
  CASE EMPTY(lcRpBnkCod)            
    *** ð cannot be empty   
    
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfModalGen("INM04074B00000","DIALOG",'Bank code')
     =gfModalGen("INM04074B00000","DIALOG",LANG_bank_code)
    * N000682 ,1 Thabet Handle globalization issues [END]
    

    RETURN .F.
      
  CASE !gfSEEK(lcRpBnkCod,'APBANKS')   
    *** ð not found.

* N000682 ,1 Thabet Handle globalization issues [Start]
*    =gfModalGen("INM04002B00000","DIALOG",'Bank code')
    =gfModalGen("INM04002B00000","DIALOG",LANG_bank_code)
* N000682 ,1 Thabet Handle globalization issues [END]

    RETURN .F.

  CASE EMPTY(lcRpChkAct)                   
    *** ð cannot be empty.

* N000682 ,1 Thabet Handle globalization issues [Start]
*    =gfModalGen("INM04074B00000","DIALOG",'Checking account')
     =gfModalGen("INM04074B00000","DIALOG",LANG_checking_account)
* N000682 ,1 Thabet Handle globalization issues [END]

    RETURN .F.

  CASE !gfSEEK(ALLTRIM(lcRpBnkCod+lcRpChkAct),'APCHECKS')   
    *** ð account not valid.

* N000682 ,1 Thabet Handle globalization issues [Start]
*    =gfModalGen("INM04077B00000","DIALOG",'Checking')
        =gfModalGen("INM04077B00000","DIALOG",LANG_Checking)
* N000682 ,1 Thabet Handle globalization issues [END]

    RETURN .F.

  CASE INT(VAL(lcNxtChkNo)) < APCHECKS.NChkNxtPn  AND !(TYPE('llCallFromRequestHandler') = 'L' .AND. llCallFromRequestHandler)  
    *** Next printed check number cannot be less than ð.
    =gfModalGen("INM04056B00000","DIALOG",ALLTRIM(STR(APCHECKS.NChkNxtPn)))
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. !lfVlDate(oAriaApplication.PrntCompanyID ,@lcFisPrd,@lcFisYear,ldChkDat)
    *** Check date is not valid.
    =gfModalGen("INM04052B00000","DIALOG")
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. EMPTY(STRTRAN(STRTRAN(lcRpGlAcct,'-'),'0')) 
    *** ð cannot be empty..
    =gfModalGen("INM04074B00000","DIALOG",LANG_GL_Account)
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. ! lfValidAcct(lcRpGlAcct)  
    *** ð not found.
    =gfModalGen("INM04002B00000","DIALOG","G/L account|"+ALLTRIM(lcRpGlAcct))
    RETURN .F.
ENDCASE

IF lcPrintMod = 'A'   

  llSelectVendor = .F.
  lnPosVend = ASCAN(loOgScroll.laOgFXFlt,"APINVHDR.CVENDCODE")
  IF lnPosVend> 0 
    lnPosVend= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosVend,1)
    lcCursorVend= loOgScroll.laOgFxFlt[lnPosVend,6]
    IF !EMPTY(lcCursorVend) AND USED(lcCursorVend)
      SELECT(lcCursorVend)
      LOCATE
      IF !EOF()
        llSelectVendor= .T.
      ENDIF 
    ENDIF 
  ENDIF 


  IF !llSelectVendor
    *** ð cannot be empty.
   
   * N000682 ,1 Thabet Handle globalization issues [Start]
*    =gfModalGen("INM04074B00000","DIALOG",'Vendor code')
        =gfModalGen("INM04074B00000","DIALOG",LANG_Vendor_code)
    * N000682 ,1 Thabet Handle globalization issues [END]
    RETURN .F.
  ELSE
    SELECT (lcCursorVend)
    SCAN 
      IF gfSEEK(ALLTRIM(&lcCursorVend..KeyExp),'APVENDOR')
        IF APVENDOR.CVENPRIOR = '0'
          *** Vendor ð has payment priority zero.  This vendor is on hold.
          =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(&lcCursorVend..KeyExp))
          RETURN .F.
        ENDIF
      ELSE
        *** ð not found.

* N000682 ,1 Thabet Handle globalization issues [Start]
*        =gfModalGen("INM04002B00000","DIALOG",'Vendor code')
        =gfModalGen("INM04002B00000","DIALOG",LANG_Vendor_code)
* N000682 ,1 Thabet Handle globalization issues [END]

        RETURN .F.
      ENDIF
    ENDSCAN   
  ENDIF  
ENDIF

IF lcPrintMod = 'V'   
  lcRpVenCod = laOGFxFlt[1,6]
  
  llSelectVendor = .F.
  lnPosVend = ASCAN(loOgScroll.laOgFXFlt,"APINVHDR.CVENDCODE")
  IF lnPosVend> 0 
    lnPosVend= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosVend,1)
    lcCursorVend= loOgScroll.laOgFxFlt[lnPosVend,6]
    IF !EMPTY(lcCursorVend) AND USED(lcCursorVend)
      SELECT(lcCursorVend)
      LOCATE
      IF !EOF()
        llSelectVendor= .T.
      ENDIF 
    ENDIF 
  ENDIF 
  
  
  IF llSelectVendor
    SELECT(lcCursorVend)
    SCAN 
      =gfSEEK(ALLTRIM(&lcCursorVend..KeyExp),'APVENDOR')
      IF APVENDOR.CVENPRIOR = '0'
        *** Vendor ð has payment priority zero.  This vendor is on hold.
        =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(&lcCursorVend..KeyExp))
        RETURN .F.
      ENDIF
    ENDSCAN    
  ENDIF  
ENDIF  

*!*************************************************************
*! Name      : lfvRemit
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate Remit to Address
*!*************************************************************************
*
FUNCTION lfvRemit
PARAMETERS loFormSet
lcInvRemit  = loFormSet.ariaForm1.cboInvRemit.Value
lcFactor  = loFormSet.AriaForm1.kbFacCode.KeyTextBox.value  
lcFactor   = IIF(EMPTY(lcFactor), APVENDOR.cFacCode, lcFactor)
loFormSet.AriaForm1.kbFacCode.KeyTextBox.value  = lcFactor   

lcInvRemit = gfRemit(lcInvRemit, .T. , lcRpVenCod, lcFactor,;
                @lcRemitStat, 'lcRem1', 'lcRem2', 'lcRem3', 'lcRem4', 'lcRem5', 'lcRem6',;
                2, 40, 'laRemitTo', @lcRemitTo, lnRemitLen)
lcFactor   = IIF(lcInvRemit = 'F', lcFactor, SPACE(6))
lcFactStat = IIF(lcInvRemit = 'F', 'ENABLE', 'DISABLE')
loFormSet.AriaForm1.kbFacCode.ENABLED = (lcFactStat='ENABLE')
loFormSet.AriaForm1.kbFacCode.KeyTextBox.value  = lcFactor  
loFormSet.ariaForm1.txtOutComp.Value = lcRem1       
loFormSet.ariaForm1.txtOutAddr1.Value = lcRem2       
loFormSet.ariaForm1.txtOutAddr2.Value = lcRem3       
loFormSet.ariaForm1.txtOutAddr3.Value = lcRem4       
loFormSet.ariaForm1.txtOutAddr4.Value = lcRem5       
loFormSet.ariaForm1.txtOutAddr5.Value = lcRem6

*!*************************************************************
*! Name      : lfvFactor
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Valid function for get field lcFactor
*!*************************************************************************
* 

FUNCTION lfvFactor
PARAMETERS loFormSet
lcFactor  = loFormSet.AriaForm1.kbFacCode.KeyTextBox.value  
llBrowse  = loFormSet.AriaForm1.kbFacCode.Selectedfrombrowse
lcOldVal = loFormSet.AriaForm1.kbFacCode.KeyTextBox.Oldvalue  
IF llBrowse .OR. !EMPTY(lcFactor) 
  IF lfGetFac(lcOldVal, llBrowse)
    lcRem1      = SYCFACT.cFacComp
    lcRem2      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,1)
    lcRem3      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,2)
    lcRem4      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,3)
    lcRem5      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,4)     
    lcRem6      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,5)
  ENDIF  
ELSE
  STORE SPACE(40) TO lcRem1, lcRem2, lcRem3, lcRem4, lcRem5, lcRem6
ENDIF   
llBrowse = .F.
loFormSet.ariaForm1.txtOutComp.Value = lcRem1       
loFormSet.ariaForm1.txtOutAddr1.Value = lcRem2       
loFormSet.ariaForm1.txtOutAddr2.Value = lcRem3       
loFormSet.ariaForm1.txtOutAddr3.Value = lcRem4       
loFormSet.ariaForm1.txtOutAddr4.Value = lcRem5       
loFormSet.ariaForm1.txtOutAddr5.Value = lcRem6
*!*************************************************************
*! Name      : lfCreateCur   
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Create cursor
*!*************************************************************************
* 
FUNCTION lfCreateCur   

DIMENSION laFieldStr(1,18)
SELECT APINVHDR
lnFieldNo = AFIELD(laFieldStr)

DIMENSION laFieldStr(lnFieldNo+3,18)
laFieldStr[lnFieldNo+1,1] = 'CADDRESS'
laFieldStr[lnFieldNo+1,2] = 'C'
laFieldStr[lnFieldNo+1,3] = 240
laFieldStr[lnFieldNo+1,4] = 0

laFieldStr[lnFieldNo+2,1] = 'nNoOfInv'
laFieldStr[lnFieldNo+2,2] = 'N'
laFieldStr[lnFieldNo+2,3] = 6
laFieldStr[lnFieldNo+2,4] = 0

laFieldStr[lnFieldNo+3,1] = 'cPageNo'
laFieldStr[lnFieldNo+3,2] = 'C'
laFieldStr[lnFieldNo+3,3] = 4
laFieldStr[lnFieldNo+3,4] = 0

FOR lnCount = 1 TO 3
  STORE '' TO laFieldStr[lnFieldNo+lnCount,7],laFieldStr[lnFieldNo+lnCount,8],laFieldStr[lnFieldNo+lnCount,9],;
              laFieldStr[lnFieldNo+lnCount,10],laFieldStr[lnFieldNo+lnCount,11],laFieldStr[lnFieldNo+lnCount,12],;
              laFieldStr[lnFieldNo+lnCount,13],laFieldStr[lnFieldNo+lnCount,14],laFieldStr[lnFieldNo+lnCount,15],;
              laFieldStr[lnFieldNo+lnCount,16]
  STORE 0 TO  laFieldStr[lnFieldNo+lnCount,17],laFieldStr[lnFieldNo+lnCount,18]
ENDFOR 
=gfCrtTmp(lcRpTargt ,@laFieldStr)
RETURN lnFieldNo + 3


*!*************************************************************
*! Name      : lfClearRep   
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Clear report
*!*******************************************************************
*
FUNCTION lfClearRep   

IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
  ERASE(oAriaApplication.WorkDir+lcRpTargt+'DBF')
  ERASE(oAriaApplication.WorkDir+lcRpTargt+'FPT')
ENDIF

glEscPrs = .F.

*!*************************************************************
*! Name      : lfBeforeRev 
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Check  that user entered all required info.
*!*******************************************************************
* 
FUNCTION lfBeforeRev 
IF !llTestChk
  IF lcPrintMod = 'A'
    RETURN lfReValidate() .AND. lfAdvance()
  ELSE
    RETURN lfReValidate() 
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfAdvance
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Advance payment function
*!*******************************************************************
* 
* 
FUNCTION lfAdvance

llSelectVendor = .F.
lnPosVend = ASCAN(loOgScroll.laOgFXFlt,"APINVHDR.CVENDCODE")
IF lnPosVend> 0 
  lnPosVend= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosVend,1)
  lcCursorVend= loOgScroll.laOgFxFlt[lnPosVend,6]
  IF !EMPTY(lcCursorVend) AND USED(lcCursorVend)
    SELECT(lcCursorVend)
    LOCATE
    IF !EOF()
      llSelectVendor= .T.
    ENDIF 
  ENDIF 
ENDIF 

DIMENSION laFlStru[16,4]
laFlStru[1,1] = 'cVendCode'
laFlStru[1,2] = 'C'
laFlStru[1,3] = 8
laFlStru[1,4] = 0
laFlStru[2,1] = 'cDebMemN'
laFlStru[2,2] = 'C'
laFlStru[2,3] = 12
laFlStru[2,4] = 0
laFlStru[3,1] = 'CDIVISION'
laFlStru[3,2] = 'C'
laFlStru[3,3] = 6
laFlStru[3,4] = 0
laFlStru[4,1] = 'Reference'
laFlStru[4,2] = 'C'
laFlStru[4,3] = 16
laFlStru[4,4] = 0
laFlStru[5,1] = 'CFactor'
laFlStru[5,2] = 'C'
laFlStru[5,3] = 6
laFlStru[5,4] = 0
laFlStru[6,1] = 'Rem1'
laFlStru[6,2] = 'C'
laFlStru[6,3] = 40
laFlStru[6,4] = 0
laFlStru[7,1] = 'Rem2'
laFlStru[7,2] = 'C'
laFlStru[7,3] = 40
laFlStru[7,4] = 0
laFlStru[8,1] = 'Rem3'
laFlStru[8,2] = 'C'
laFlStru[8,3] = 40
laFlStru[8,4] = 0
laFlStru[9,1] = 'Rem4'
laFlStru[9,2] = 'C'
laFlStru[9,3] = 40
laFlStru[9,4] = 0
laFlStru[10,1] = 'Rem5'
laFlStru[10,2] = 'C'
laFlStru[10,3] = 40
laFlStru[10,4] = 0
laFlStru[11,1] = 'Rem6'
laFlStru[11,2] = 'C'
laFlStru[11,3] = 40
laFlStru[11,4] = 0
laFlStru[12,1] = 'nPaymnt'
laFlStru[12,2] = 'N'
laFlStru[12,3] = 12  
laFlStru[12,4] = 2
laFlStru[13,1] = 'n1099Amnt'
laFlStru[13,2] = 'N'
laFlStru[13,3] = 15  
laFlStru[13,4] = 2
laFlStru[14,1] = 'cInvRemit '
laFlStru[14,2] = 'C'
laFlStru[14,3] = 1  
laFlStru[14,4] = 0
laFlStru[15,1] = 'cApAcct'
laFlStru[15,2] = 'C'
laFlStru[15,3] = 24 
laFlStru[15,4] = 0
laFlStru[16,1] = 'lOkAdvPay'
laFlStru[16,2] = 'L'
laFlStru[16,3] = 1 
laFlStru[16,4] = 0
=gfCrtTmp(lcDebtCurs,@laFlStru,'cVendCode',lcDebtCurs)  

SELECT(lcCursorVend)
LOCATE 
SCAN 
  lnRecN = RECNO()
  lcRpVenCod = ALLTRIM(&lcCursorVend..KeyExp)
  lcDebMemN  = SPACE(12)        && Debit memo number
  lcAdvDiv   = " "              && var. to holds active division code
  lcRef      = SPACE(16)        && Reference
  lcFactor   = SPACE(6)
  lnPaymnt   = 0                && Payment amount
  ln1099Amnt = 0                && 1099 amount
  lcRem1     = SPACE(40)        && Address 1
  lcRem2     = SPACE(40)        && Address 2
  lcRem3     = SPACE(40)        && Address 3
  lcRem4     = SPACE(40)        && Address 4
  lcRem5     = SPACE(40)        && Address 5
  lcRem6     = SPACE(40)        && Address 6
  lnRemit    = 1
  llOkAdvPay = .F.
  llBrowse   = .F.              && Variable to hold left mouse clicked or not. 
  =gfSEEK(ALLTRIM(&lcCursorVend..KeyExp),'APVENDOR')
  lc1099Stat = IIF(EMPTY(APVENDOR.cVen1099T),'DISABLE','ENABLE')

  DIMENSION  laRemitTo[3,2]

  *** Prepare Remit to array from SYDFIELD and get its maximum width
  lcFactStat   = 'DISABLE'
  lcRemitStat  = 'DISABLE'

  lnRemitLen   = gfGetVld('cInvRemit',@laRemitTo)
  lcRemitTo    = laRemitTo[1,1]   
  lcInvRemit   = laRemitTo[1,2]   
  lcRem1       = APVENDOR.cVenComp

  lcRem2       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,1)
  lcRem3       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,2)
  lcRem4       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,3)
  lcRem5       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,4)
  lcRem6       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,5)

  lcAdvDiv    = APVENDOR.cDivision
  IF gfSEEK('N'+APVENDOR.cDivision + 'N' + 'CDIVISION')
    lcAdvDiv    = APVENDOR.cDivision
  ENDIF

  IF !EMPTY(APVENDOR.cApAcct)
    lcApAcct   = APVENDOR.cApAcct
  ELSE
    IF !EMPTY(lcAdvDiv)  
      =gfSEEK(lcAdvDiv,'APDIV')
      IF EMPTY(APDIV.cApAcct)
        lcApAcct = APSETUP.CAPACCT    
      ELSE
        lcApAcct = APDIV.cApAcct
      ENDIF  
    Else
      lcApAcct   = APSETUP.CAPACCT       
    ENDIF
  ENDIF    
  DO FORM (oAriaApplication.ReportHome  + oAriaApplication.ActiveModuleID+ '\APCKADVP.SCX')   && Run the advance payment screen 
  IF llOkAdvPay
    SELECT (lcDebtCurs)
    APPEND BLANK 
    REPLACE cVendCode WITH lcRpVenCod ,;
            cDebMemN  WITH lcDebMemN  ,;
            CDIVISION WITH lcAdvDiv    ,;
            CFactor   WITH lcFactor   ,;
            Reference WITH lcRef      ,;
            Rem1      WITH lcRem1     ,;
            Rem2      WITH lcRem2     ,;
            Rem3      WITH lcRem3     ,;
            Rem4      WITH lcRem4     ,;
            Rem5      WITH lcRem5     ,;
            Rem6      WITH lcRem6     ,;
            lOkAdvPay WITH llOkAdvPay ,;
            cApAcct   WITH lcApAcct   ,;
            nPaymnt   WITH lnPaymnt   ,; 
            n1099Amnt with ln1099Amnt ,;
            cInvRemit WITH lcInvRemit    
  ENDIF     
  SELECT(lcCursorVend)  
  IF BETWEEN(lnRecN,1,RECCOUNT())
    GO RECORD lnRecN 
  ENDIF     
ENDSCAN 
RETURN llOkAdvPay
*!*************************************************************
*! Name      : lfvPrintMode
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate Print Mode
*!*******************************************************************
* 
FUNCTION lfvPrintMode
PARAMETERS llFromWhen
lcSessValue = lcSession
lcApDistOld = lcApDist  
lcDebtCursOld = lcDebtCurs               
IF lcPrintMod <> lcOldMode
  lcOldMode = lcPrintMod
  lcRpChkMod = lcPrintMod

  STORE '' TO loogScroll.aRepVrFlt, loogScroll.laOGFxFlt,loogScroll.laOGVRFlt,loogScroll.laOGHDFlt
  loogScroll.lcOGPrgName = 'APCHKPR'+lcPrintMod
  lnRemoteRslt = loogScroll.SQLExecute("SYDREPRT","Select * from SYDREPRT where cVer <> 'A27' and cRep_ID='"+PADR(ALLTRIM(loogScroll.lcOGPrgName),loogScroll.lnRepIDLen)+"'",;
                                    '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                    3,"",loogScroll.Parent.DataSessionID)
  IF (lnRemoteRslt < 1) OR EOF("SYDREPRT")
    RETURN .F.
  ENDIF 

  SELECT SYDREPRT
  loogScroll.lnRepIDLen = LEN(cRep_ID)
  WITH loogScroll
    STORE PADR(ALLTRIM(.lcOGPrgName),.lnRepIDLen) TO .lcOGPrgName, .lcOGRepID, .lcOGManRep
    IF (.lcOGPrgName != .lcOGRepID)  
      USE IN SYDREPRT    
    lnRemoteRslt = This.SQLExecute("SYDREPRT","Select * from SYDREPRT where cVer <> 'A27' and cRep_ID='"+PADR(ALLTRIM(.lcOGPrgName),.lnRepIDLen)+"'",;
                                       '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                        3,"",loogScroll.Parent.DataSessionID)    
      IF (lnRemoteRslt < 1) OR EOF("SYDREPRT")
      .lcOGPrgName =  .lcOGRepID
        lnRemoteRslt = This.SQLExecute("SYDREPRT","Select * from SYDREPRT where cVer <> 'A27 AND 'cRep_ID='"+PADR(ALLTRIM(This.lcOGPrgName),This.lnRepIDLen)+"'",;
                                    '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                    3,"",loogScroll.Parent.DataSessionID)
      ENDIF 
    ENDIF 
  ENDWITH   
  SELECT SYDREPRT
  LOCATE 
  loogScroll.gcAct_Appl = cApp_ID
  IF !llFromWhen
    lcSession = lcSessValue  
    loogScroll.InitRepHeader()
  ENDIF   
  lcSession = lcSessValue  
  loOGScroll.GetRepVar()
  lcSession = lcSessValue  
  WITH loOGScroll.OuterContainer.InnerContainer
    DO WHILE .ControlCount > 0 AND !('PRINTMOD' $ UPPER(.Controls(.ControlCount).Name))
     .RemoveObject(.Controls(.ControlCount).Name)
    ENDDO 
  ENDWITH
  lcSession = lcSessValue  
  loOGScroll.defineobjects (.T.)
  lcSession = lcSessValue  
  =loOGScroll.RefreshScroll()
  loogScroll.lcOGPrgName = 'APCHKPR'+'V'
  loogScroll.lcOGManRep= 'APCHKPR'+'V'
ENDIF 
lcApDist   = lcApDistOld  
lcDebtCurs = lcDebtCursOld  
IF lcPrintMod = 'R'
  IF !USED('APDISTA')
    =gfOpenTable('APDIST','CHECKS','SH','APDISTA')
  ENDIF  
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start]
*!*	  =gfSqlRun("Select * from apdist where "+;
*!*	        " capdtrtyp = 'P' and cApdActID = 'A' AND cApdStat <> 'V'",'APDISTA',.T.,lcApDist)
  =gfSqlRun("Select cstubchk,dApdTrDat,cApdRef,cVendCode,cInvNo,capdtrtyp,cbnkcode,cchkacct   from apdist where "+;
        " capdtrtyp = 'P' and cApdActID = 'A' AND cApdStat <> 'V'",'APDISTA',.T.,lcApDist)

*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End]  
  SELECT (lcApDist)
  
  CURSORSETPROP("Buffering" ,3)
  INDEX on capdtrtyp+ cbnkcode+cchkacct+ cstubchk TAG (lcApDist) 
ENDIF 
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [START]
*lcRepForm = 'APCHKPWL'
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [END]
DIME laFrxFiles[1,4]
STORE '' TO laFrxFiles
=ADIR(laFrxFiles,oAriaApplication.ReportHome+'*.FRX')
IF !EMPTY(laFrxFiles[1,1])
  IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__' + oAriaApplication.ActiveCompanyID)+'.FRX')>0 
    lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + oAriaApplication.ActiveCompanyID)
  ELSE  
    IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
      lcRepForm = '__' + SUBSTR(lcRepForm , 3)
    ELSE 
      lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')   
      DIME laFrxFiles[1,4]
      STORE '' TO laFrxFiles
      =ADIR(laFrxFiles,lcSrvRpt+'*.FRX')
      IF !EMPTY(laFrxFiles[1,1])
        IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__' + oAriaApplication.ActiveCompanyID)+'.FRX')>0 
          lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + oAriaApplication.ActiveCompanyID)
        ELSE
          IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
            lcRepForm = '__' + SUBSTR(lcRepForm , 3)
          ENDIF   
        ENDIF 
      ENDIF  
    ENDIF
  ENDIF
ENDIF 
loogScroll.CheckPlatForm()

 
DECLARE laRpSource[5],laRpTarget[1]
STORE 'Printed Checks'    TO laRpSource[1] 
STORE ''               TO laRpTarget[1]
STORE 'Manual Checks'     TO laRpSource[2] &&,laRpTarget[2]
STORE 'Non Check Payments'TO laRpSource[3]
STORE 'Credit Card'     TO laRpSource[4]
STORE 'Cash Payments'     TO laRpSource[5]
lcRpStatus = "'P','M','N','C','H'"
IF ASCAN(LAOGOBJTYPE,'LCRPGLACCT') # 0
  LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCRPGLACCT'),1)
  LAOGOBJCNT[LNPOS] = IIF(lcPrintMod = 'V',.F.,.T.)
  = LFOGSHOWGET('LCRPGLACCT')
ENDIF

IF lcPrintMod <> 'R'  
  lcNxtChkNo = IIF(gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),PADL(APCHECKS.nChkNxtPn,8,'0'),'00000001')
ENDIF


IF TYPE('llCallFromRequestHandler') = 'L' .AND. llCallFromRequestHandler
  IF ASCAN(LAOGOBJTYPE,'LCPRINTMOD') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCPRINTMOD'),1)
    LAOGOBJCNT[LNPOS] = .F.
    = LFOGSHOWGET('LCPRINTMOD')
  ENDIF
  
  IF ASCAN(LAOGOBJTYPE,'LNALTEST') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LNALTEST'),1)
    LAOGOBJCNT[LNPOS] = .F.
    = LFOGSHOWGET('LNALTEST')
  ENDIF
ENDIF 


*!*************************************************************
*! Name      : lfDefNxtChk
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose   : Function to Get next check number
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option Grid (Default value of variable "lcNxtChkNo")
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Next check number
*!*************************************************************
*! Example            :  =lfDefNxtChk()
*!*************************************************************
FUNCTION lfDefNxtChk

=gfOpenTable('APCHECKS','Bankcheck','SH')
RETURN IIF(gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'), ;
           PADL(APCHECKS.NChkNxtPn,8,'0'),'00000001')

*!*************************************************************
*! Name      : lfDefGlAcc
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose   : Function to Get Check GL account
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option Grid (Default value of variable "lcRpGlAcct")
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Check GL account
*!*************************************************************
*! Example            :  =lfDefGlAcc()
*!*************************************************************
FUNCTION lfDefGlAcc
=gfOpenTable('APCHECKS','Bankcheck','SH')
RETURN IIF(gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),APCHECKS.CCHKGLACC,' ')


FUNCTION lfSetAproe


  *!*************************************************************
  *! Name      : lfVDtMsg
  *! Developer : Mariam Mazhar (MMT)
  *! Date      : 10/19/2009
  *! Purpose   : Validation function for date by giving messages.
  *!*************************************************************
  *! Parameters: The company that you want to validate from its periods.
  *!           : Logical parameter to accept the date in a locked period.
  *!*************************************************************
  *! Returns   : True or False
  *!*************************************************************
FUNCTION lfVDtMsg
  PARAMETERS lcCompany, lcCrPrd, lcCrYer, ldDateTVal, llLockPer


  PRIVATE llLockStat, lcCompYer, lcCompPrd
  PRIVATE lnYear, lcYerPer, lcMessage, lcMessgSel
  PRIVATE lcCurYear, lcCurPrd, llVaildDate

  lcCompYer  = ' '   && Variable to hlod the current year of the company.
  lcCompPrd  = ' '   && Variable to hlod the current period of the company.

  lcCurYear  = ' '
  lcCurPrd   = ' '

  llVaildDate= .F.   && Variable to indicate if the date is valid or not

  lcCrYer    = IIF(TYPE('lcCrYer') <> 'C',' ',lcCrYer)

  lcCrPrd    = IIF(TYPE('lcCrPrd') <> 'C',' ',lcCrPrd)

  llLockPer  = IIF(llLockPer,llLockPer,.F.)

  lnYear     = 0     && Variable to hold the year No.

  llLockStat = .F.   && Variable to hold the lock stat of the period.

  ldDateTVal = IIF(TYPE('ldDateTVal')='O',ldDateTVal.Text1.VALUE,ldDateTVal)

  IF lfVlDate(lcCompany,lcCrPrd,lcCrYer,ldDateTVal)  && Call the date validation function.

    lcCrPrd  = lcCurPrd
    lcCrYer  = lcCurYear
    lcYerPer = lcCurPrd+'-'+lcCurYear


    lnYear   = (INT(VAL(lcCurYear))-INT(VAL(lcCompYer))) + 3


    lnYear   = IIF(lnYear <= 0,1,lnYear)

    IF lnYear = 3
      lnYear  = lnYear + SIGN(VAL(lcCrPrd) - VAL(oAriaApplication.CurrentPeriod))
    ENDIF  && End Period Validation

    DO CASE
      CASE llLockStat


        =gfModalGen("TRM00134B00000","DIALOG",lcYerPer)
        llVaildDate = .F.

      CASE lnYear > 0

        lcMessage  = 'history prior   current  future  '

        lcMessgSel = ALLTRIM(SUBSTR(lcMessage,(lnYear*8)-7,8))


        IF lnYear <> 3
          =gfModalGen("TRM00136B00000","DIALOG",lcMessgSel+"|"+lcYerPer)
        ENDIF

        IF lnYear = 1   && If the year = 1 we are not going to accept.
          llVaildDate = .F.
        ELSE
          llVaildDate = .T.
        ENDIF
    ENDCASE
  ELSE

    =gfModalGen("TRM00133B00000","DIALOG")

    llVaildDate = .F.
  ENDIF

  RETURN llVaildDate

  *--end of lfVDtMsg
   *!*************************************************************
  *! Name      : lfVlDate
  *! Developer : Mariam Mazhar
  *! Date      : 10/19/2009
  *! Purpose   : To Validate dates from the periods file.
  *!*************************************************************
  *! Parameters: The company that you want to validate from its periods.
  *!           : Logical parameter to accept the date in a locked period.
  *!*************************************************************
  *! Returns   : True or False
  *!*************************************************************
FUNCTION lfVlDate
  PARAMETERS lcCompany, lcCrPrd, lcCrYer, ldDateTVal, llLockPer
  LOCAL ap1

  PRIVATE llOpenPrd, lcSavOrdPr, llValidDate, lcExactStat

  IF TYPE('lcCurYear') <> 'C' .AND. TYPE('lcCurPrd') <> 'C'
    PRIVATE lcCurYear, lcCurPrd, lLockStat

    lcCrYer    = IIF(TYPE('lcCrYer') <> 'C',' ',lcCrYer)

    lcCrPrd    = IIF(TYPE('lcCrPrd') <> 'C',' ',lcCrPrd)
  ENDIF

  llLockPer  = IIF(llLockPer,llLockPer,.F.)

  llValidDate= .F.      && Variable to return with from the function.

  lcCurYear  = ' '      && Variable to hold the current year.
  lcCurPrd   = ' '      && Varibale to hold the current period.
  lcExactStat= ' '      && Variable to hold the SET EXACT status.
  lLockStat  = .F.      && Variable to hold the lock stat of the record.

  lcSavSelct = ALIAS()  && Variable to save the currently selected file.

  llOpenPrd  = .F.      && Variable to indicate that the there is a file;
    OPEN BY the FUNCTION.

  ap1 = CREATEOBJECT("ap")

  lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)

  lcCompYer  = oAriaApplication.CurrentYear
  lcCompPrd  = oAriaApplication.CurrentPeriod



  lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)


  lcCompYer  = oAriaApplication.CurrentYear
  lcCompPrd  = oAriaApplication.CurrentPeriod

  IF !USED('FSPRD')   && Check if the period file is open or not.
    llOpenPrd = .T.      && Indicates that the file is open by the function.
    SELECT 0             && Select an empty work area.
    lcdatadir = ap1.lcDataDir
    
    =gfOpenTable('fsprd','COMFYRPRDI','SH')
  ELSE
    SELECT FSPRD      && The file is open so we are going to select
    lcSavOrdPr = ORDER() && Save the file order
    =gfSetOrder('COMFYRPRDI')
  ENDIF

  IF TYPE('ldDateTVal') <> 'D'
    ldDate = IIF(TYPE('_screen.ActiveForm.ActiveControl.Value')='D',_SCREEN.ACTIVEFORM.ACTIVECONTROL.VALUE,{})
  ELSE
    ldDate = ldDateTVal
  ENDIF

  lcExactStat = SET('EXACT')
  SET EXACT OFF
  GO TOP

  SET EXACT &lcExactStat


  LOCATE REST FOR BETWEEN(ldDate,dfsppbgdt,dfsppendt)
  IF FOUND() .AND. BETWEEN(ldDate,ap1.ldPyBgDate,ap1.ldNyEnDate)

    llLockStat = lFspLocks  && Assign the variable with the period lock stat.
    lcCurYear  = cFisFYear  && Assign the variable with fiscal year of the period.
    lcCurPrd   = cFspprdid  && Assign the variable with the period no.
    lcCrYer    = cFisFYear  && Assign the variable with fiscal year of the period.
    lcCrPrd    = cFspprdid  && Assign the variable with the period no.
    llLockPer  = lFspLocks  && Assign the variable with the period lock stat.
    llValidDate= .T.        && Assign the variable with .T.

    IF USED('FSPRD') .AND. llOpenPrd
      llOpenPrd = .F.
      =gfCloseTable('FSPRD')
    ELSE
      SELECT FSPRD
      =gfSetOrder(lcSavOrdPr)
    ENDIF

    IF !EMPTY(lcSavSelct)
      SELECT(lcSavSelct)
    ENDIF
  ELSE
    IF USED('FSPRD') .AND. llOpenPrd
      llOpenPrd = .F.
      =gfCloseTable('FSPRD')
    ELSE
      SELECT FSPRD
      =gfSetOrder(lcSavOrdPr)
    ENDIF

    IF !EMPTY(lcSavSelct)
      SELECT(lcSavSelct)
    ENDIF
  ENDIF

  RETURN llValidDate

  *--end of lfVlDate
*!*************************************************************
  *! Name      : lfGetGLPIC
  *! Developer : Mariam MAzhar
  *! Date      : 10/19/2009
  *! Purpose   : Get GL format
  *!*************************************************************  
FUNCTION lfGetGLPIC
  IF !USED('APSETUP')
    =gfOpenTable('APSETUP','APSETUP','SH')
    SELECT 'APSETUP'
    gfSeek('')
  ENDIF 

  IF !USED('ACCOD')
  =gfOpenTable('ACCOD','ACCSEGNO','SH')
  SELECT 'ACCOD'
  gfSeek('')
ENDIF 
lcApsAcMas = ACCOD.cAcsMask
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))
RETURN lcApsAcMas 
*!*************************************************************
  *! Name      : GetRpField
  *! Developer : Mariam MAzhar
  *! Date      : 10/19/2009
  *! Purpose   : Get Selected Fields
  *!*************************************************************
*
FUNCTION GetRpField
PRIVATE lcOGSelected,lcOGFiles,lnOGCount
lcOGSelected=SELECT()
SELECT SYDREPRT 
LOCATE FOR cRep_id= lcOGPrgName
IF FOUND()
  IF !EMPTY(mRepField)
    RESTORE FROM MEMO mRepField ADDITIVE 
    lcOGFiles=''
    FOR lnOGCount=1 TO ALEN(laselfield,1)
      lcOGFiles=lcOGFiles+IIF(lnOGCount>1,',','')+;
      laselfield[lnOGCount,1]
      IF TYPE('laselfield[lnOGCount,2]')='C'
        lcOGFiles=lcOGFiles+IIF(!EMPTY(laselfield[lnOGCount,2]),;
               ' AS "'+ALLTRIM(laselfield[lnOGCount,2])+'"','')
      ENDIF         
    ENDFOR
  ENDIF
ELSE
  lcOGFiles=''
ENDIF
SELECT(lcOGSelected)
RETURN lcOGFiles
*!*************************************************************
*! Name      : lfGetFac
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose   : Global Function used by the validation function of the Factor Control.
*!*************************************************************
*! Parameters: lcOldVal, llBrowse
*!*************************************************************
*! Returns   : llFactFound
*!*************************************************************
FUNCTION lfGetFac
  PARAMETERS lcOldVal, llBrowse
  PRIVATE lcPrFactor, loFactObj, lcCurAlias, lnSavFacTg, lnClosRec,;
    llFactFound, llOpenFact, lcFile_Ttl, lcBrFields

  loFactObj    = IIF(TYPE('_Screen.ActiveForm.ActiveControl.value')='C', ;
    _SCREEN.ACTIVEFORM.ACTIVECONTROL, ;
    _SCREEN.ACTIVEFORM.ACTIVECONTROL.PARENT.KeyTextBox)
  lcPrFactor     = ALLTRIM(loFactObj.VALUE)
  llFactFound  = .F.
  IF llBrowse .OR. !EMPTY(lcPrFactor)
    lcCurAlias = ALIAS()
    IF !USED('SYCFACT')  && Check if the factors file is open or not.
      llOpenFact  = .T.     && Indicates that the file is open by the function.
      SELECT 0
      gfOpenTable(oAriaApplication.SysPath+'SYCFACT','CFACCODE','SH')
    ELSE
      llOpenFact = .F.
      SELECT SYCFACT
      lnSavFacTg = ORDER()
      =gfSetOrder('cFacCode')
    ENDIF
    lcPrFactor   = PADR(ALLTRIM(lcPrFactor), FSIZE('cFacCode','SYCFACT'))
    loFactObj.VALUE = lcPrFactor
    IF llBrowse .OR. !gfSEEK(lcPrFactor,'SYCFACT')
      DIMENSION laTemp[1]
      laTemp = ''
      lcFile_Ttl = 'Factors'
      lcBrFields = ' '
      lnClosRec = RECNO(0)
      =GFGETBRF(@lcFile_Ttl, @lcBrFields, 'SYCFACT',;  &&lower
      "cFacCode,cFaccomp,cPhoneNo,cFaxNo,cFacTitle,cFacCont")
      IF BETWEEN(lnClosRec,1,RECCOUNT())
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF

      =gfBrows(.F.,'cFacCode','laTemp')
      IF !EMPTY(laTemp[1])
        loFactObj.VALUE = laTemp[1]
        llFactFound = .T.
      ELSE
        loFactObj.VALUE = loFactObj.OldValue
        llFactFound = .F.
      ENDIF
    ELSE
      llFactFound = .T.
    ENDIF

    IF USED('SYCFACT')
      IF llOpenFact
        =gfCloseTable('SYCFACT')
      ELSE
        SELECT SYCFACT
        =gfSetOrder(lnSavFacTg)
      ENDIF
    ENDIF
    SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
  ENDIF
  RETURN llFactFound

  *!*************************************************************
  *! Name      : gfRemit
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 7/29/2004
  *! Purpose   : Global Function.
  *!*************************************************************
  *! Parameters: Many
  *!*************************************************************
  *! Returns   : lcRmtToCode
  *!*************************************************************
FUNCTION gfRemit
  PARAMETERS lcRmtToCode, llActPopup, lcVendCode, lcFactCode, lcRemitStat,;
    lcRemitCmp, lcRemitAd1, lcRemitAd2, lcRemitAd3,lcRemitAd4,lcRemitAd5, lnRow, lnCol,;
    lcArrName, lcDispStr, lnPopWdth
  *** lcRmtToCode : normally ('V', 'F', 'O')
  *** llActPopup  : .T. if the popup is to be activated
  *** lcVendCode  : vendor code
  *** lcFactCode  : factor code
  *** lcRemitStat : remit objects display status
  *** lcRemitCmp  : remit company object name
  *** lcRemitAd1  : remit address1 object name
  *** lcRemitAd2  : remit address2 object name
  *** lcRemitAd3  : remit address(3+4+5) object name
  *** lnRow       : DOS popup top left row
  *** lnCol       : DOS popup top left column
  *** lcArrName   : remit to array name
  *** lcDispStr   : display string of the popup
  *** lnPopWdth   : dos popup width
  *** example call from APPYINV.PRG
  *** laData[5]    = lfRemit(laData[5], !llJustShow, laData[1], laData[36],;
  ***              @lcRemitStat, 'laData[6]', 'laData[7]', 'laData[8]', 'laData[9]',;
  ***              6, 39, 'laRemitTo', @lcRemitTo, lnRemitLen)
  PRIVATE lcRemitFil, lcRemitTag, lcKeyCode
  lcRmtToCode = UPPER(ALLTRIM(lcRmtToCode))


  lcKeyCode = IIF(lcRmtToCode = 'V', lcVendCode, lcFactCode)
  STORE .F. TO llOpVen, llOpFact

  IF lcRmtToCode = 'V'
    lcRemitFil = 'APVENDOR_A'
    lcRemitTag = 'VENCODE'
    llOpVen    = gfOpenTable('APVENDOR','VENCODE','SH','APVENDOR_A')  &&lower
  ELSE
    lcRemitFil = 'SYCFACT_A'
    lcRemitTag = 'CFACCODE'
    llOpFact   = gfOpenTable(oAriaApplication.SysPath+'SYCFACT','CFACCODE','SH','SYCFACT_A')  &&lower
  ENDIF

  DO CASE
    CASE lcRmtToCode $ 'VF'
      
      IF gfSEEK(lcKeyCode, lcRemitFil)
        &lcRemitCmp  = PADR(IIF(lcRmtToCode = 'V',;
        APVENDOR_A.cVenComp, SYCFACT_A.cFacComp), 40)
        &lcRemitAd1 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,1)
        &lcRemitAd2 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,2)
        &lcRemitAd3 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,3)
        &lcRemitAd4 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,4)
        &lcRemitAd5 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,5)
      ELSE
        STORE SPACE(40) TO &lcRemitCmp, &lcRemitAd1,;
          (lcRemitAd2), (lcRemitAd3),;
          (lcRemitAd4), (lcRemitAd5)
      ENDIF
      lcRemitStat= 'DISABLE'
    OTHERWISE
      lcRemitStat= IIF(loFormSet.ActiveMode="V",'DISABLE','ENABLE')
  ENDCASE
  IF USED('APVENDOR_A') .AND. llOpVen
    =gfCloseTable('APVENDOR_A')
  ENDIF
  IF USED('SYCFACT_A') .AND. llOpFact
    =gfCloseTable('SYCFACT_A')
  ENDIF
  RETURN lcRmtToCode
  *--end of gfRemit
*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
*!*************************************************************
  *! Name      : lfApAcs
  *! Developer : Mariam MAzhar
  *! Date      : 10/19/2009
  *! Purpose   : Validate GL Account
  *!*************************************************************
FUNCTION lfApAcs
PARAMETERS lcAcsDes,llBrowse, llValidAcs

llApGlLink = .F.
SELECT APSETUP
=gfSeek('')
*** This line is very important to refresh the station buffer with new 
*** information from the disk
GO TOP
IF !EOF()
  GO 1
  llApGlLink = (CAPSGLLINK = 'Y')
ENDIF  

** If the parameter is undefined.
lcAcsDes    = IIF(TYPE('lcAcsDes') $ 'UL','',lcAcsDes)
lcSavAlias  = ALIAS()  && Variable to save the selected alias.
llBrowse    = IIF(TYPE('llBrowse') $ 'U',.F.,llBrowse)
llValidAcs  = .T.

lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lcOldVal = lcCurVar.oldvalue
lnApsAcLen = 0
lcPic = lfGetGLPIC()

lcFieldCont = lcCurVar  
*** Variable hold an empty account to compair with. ***
lcEmptyAcs = REPLICATE('0',lnApsAcLen)

IF llApGlLink .AND. (llBrowse .OR. lcFieldCont <> lcEmptyAcs)

  SELECT('lcLinkChar')
  gfSetOrder('ACCTCODE')
  =gfSeek('')
  LOCATE 
  IF llBrowse .OR. !gfSEEK(lcFieldCont) .OR. ATC('?',lcFieldCont) > 0
    llBrowse = .F. 
    DIMENSION laTemp[2]
    laTemp = ''
    lcSavBrFld=lcBrfields
    lcSavTitle=lcFile_Ttl

    lcBrfields="CACCTCODE :H= 'Account Code',;
                CACCNLDES :H= 'Account Description'"

    lcFile_Ttl="Chart of accounts"
        
    =gfbrows(' ','CACCTCODE,CACCNLDES','laTemp',lcFile_Ttl)

    lcFile_Ttl=lcSavTitle
    lcBrfields=lcSavBrFld

    IF !EMPTY(laTemp[1])
      lcFieldCont = ALLTRIM(laTemp[1])
      lcAcsDes    = ALLTRIM(laTemp[2])
    ELSE
      lcFieldCont = REPLICATE('0',lnApsAcLen)  
      lcAcsDes    = ' '
      llValidAcs  = .F.
    ENDIF
  ELSE
    lcAcsDes      = ALLTRIM(CACCNLDES)
  ENDIF  

  IF !EOF() AND cSegActiv = "I"
    =gfModalGen("TRM04202B00000","DIALOG",ALLTRIM(lcFieldCont))
    lcFieldCont = REPLICATE('0',lnApsAcLen)  
    lcAcsDes    = ' '
    llValidAcs  = .F.
  ENDIF

  IF !EMPTY(lcSavAlias)
    SELECT(lcSavAlias)
  ENDIF  
  LCRPGLACCT  = lcFieldCont
ENDIF

IF !EMPTY(lcSavAlias)
  SELECT(lcSavAlias)
ENDIF  

RETURN llValidAcs

*!*************************************************************
*! Name      : lfvPayMeth
*! Developer : Mariam MAzhar
*! Date      : 10/19/2009
*! Purpose   : Validate Payment Method
*!*************************************************************
FUNCTION lfvPayMeth
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpVenPay
= lfOGMover(@laRpSource,@laRpTarget,'Select Customer Status',.T.,'')  && call mover function.
lcRpVenPay = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  lcRpVenPay= lcRpVenPay+ IIF(laRpTarget[lnI] = 'Printed Checks',"'P'",;
                            IIF(laRpTarget[lnI] = 'Manual Checks',",'M'",;
                            IIF(laRpTarget[lnI] = 'Non Check Payments',",'N'",;
                            IIF(laRpTarget[lnI] = 'Credit Card',",'C'",IIF(laRpTarget[lnI] = 'Cash Payments' ,",'H'",'')))))

ENDFOR  && end Loop to make Status expression.
lcRpVenPay= IIF(EMPTY(lcRpVenPay),lcRpVenPay,ALLTRIM(lcRpVenPay))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpVenPay) 
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpVenPay)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- End of lfvOStatus.
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Return the selected Payment Method in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
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

*!*************************************************************
*! Name      : lfGetBnkCode
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : get bank code
*!*************************************************************
FUNCTION lfGetBnkCode

IF !USED('APSETUP')
  =gfOpenTable('APSETUP','APSETUP','SH')
  SELECT 'APSETUP'
  gfSeek('')
ENDIF 

RETURN APSETUP.cBnkCode 

*!*************************************************************
*! Name      : lfGetChkAcc
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Return the bank check account
*!*************************************************************
FUNCTION lfGetChkAcc

IF !USED('APSETUP')
  =gfOpenTable('APSETUP','APSETUP','SH')
  SELECT 'APSETUP'
  gfSeek('')
ENDIF 

RETURN APSETUP.cChkAcct 
*!*************************************************************
*! Name      : LFSRVCODE 
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Check Vendor Priority
*!*************************************************************
FUNCTION LFSRVCODE 
PARAMETERS lcParams

IF CVENPRIOR = '0'
  *** Vendor ð has payment priority zero.  This vendor is on hold.
  =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(CVENDCODE))
  RETURN .F.
ENDIF 
*!**************************************************************************
*!
*!      Function lfvOkAdvPay
*!
*!**************************************************************************
* 
FUNCTION lfvOkAdvPay      
PARAMETERS loFormSet

IF EMPTY(lcDebMemN)
  *** You have to enter the ð.
 
 * N000682 ,1 Thabet Handle globalization issues [Start]
*  =gfModalGen("TRM04066B00000","DIALOG",'debit memo number')
    =gfModalGen("TRM04066B00000","DIALOG",LANG_debit_memo_number)
  * N000682 ,1 Thabet Handle globalization issues [END]
  RETURN .F.
ELSE
  SELECT APINVHDR
  gfSetOrder('VENDINV') 
  IF gfSEEK(lcRpVenCod+lcDebMemN,'APINVHDR')
    *** ð exists for the vendor ð.
    =gfModalGen("TRM04024B00000","DIALOG",'Invoice '+ALLTRIM(lcDebMemN)+ ;
                '|'+ALLTRIM(lcRpVenCod))
    RETURN .F.
  ENDIF  
  =gfSetOrder('INVVEND')
ENDIF  

IF lnPaymnt <= 0
  *** The amount to be applied should be greater than zero.
  =gfModalGen("TRM04029B00000","DIALOG")
  RETURN .F.
ELSE
  IF !BETWEEN(ln1099Amnt,0,lnPaymnt)
    *** The 1099 amount must be between ð and ð.
    =gfModalGen("TRM04017B00000","DIALOG","0|"+STR(lnPaymnt))
    RETURN .F.
  ENDIF
ENDIF

IF EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')) 
  *** 'ð cannot be empty.' 

* N000682 ,1 Thabet Handle globalization issues [Start]
*  =gfModalGen("INM04074B00000","DIALOG",'AP account')
  =gfModalGen("INM04074B00000","DIALOG",LANG_AP_account)
* N000682 ,1 Thabet Handle globalization issues [END]

  RETURN .F.
ENDIF  

IF ! lfValidAcct(lcApAcct)  
  *** ð not found.
  =gfModalGen("INM04002B00000","DIALOG",'A/P account|'+ALLTRIM(lcApAcct))
  RETURN .F.
ENDIF  

*** If remitting to factor, and there is no factor code, 
*** do not proceed and present the following message
IF lcInvRemit = 'F' .AND. EMPTY(lcFactor)
  *** Message : "   You have to enter the ð.  "
  ***                 <  OK  >
  =gfModalGen("TRM04066B00000","DIALOG",lcTFactor)
  RETURN .F.
ENDIF
llOkAdvPay = .T.
