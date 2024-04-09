*:*********************************************************************************
*: Program file  : ARUPSGM.PRG
*: Program desc. : UPS Package Traking 
*: For screen    : Menu
*:        System : Aria Advantage Series.
*:        Module : Accounts Receivable (AR).
*:     Developer : HEND GHANEM (HBG)
*:     Entry     : C#038257
*:*********************************************************************************
*: Passed Parameters  : None
*:*********************************************************************************
*: Modifications :
*:*********************************************************************************
#INCLUDE R:\ARIA4XP\PRGS\AR\ARUPSGM.H

DO FORM oAriaApplication.cLIENTScreenHome+oAriaApplication.ActiveModuleID+ "\ARUPSGM.SCX"


*!*************************************************************
*! Name      : lfInit
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Initialize event.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvInv()
*!*************************************************************
FUNCTION lfInit
PARAMETERS loFormSet

DIMENSION loFormSet.laOperLine[1,6] 
STORE '' TO loFormSet.laOperLine 

llMulCurr = .F.
*--Get needed setups
DECLARE laSetups[2,2]
laSetups[1,1] = 'M_WareHouse'
laSetups[2,1] = 'llMulCurr'
=gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)

loFormSet.lcWareH   = laSetups[1,2]
loFormSet.llMulCurr = laSetups[2,2]

loFormSet.ariaform1.cmdPrint.Enabled = .F.
*B999999,1 MMT Fix bug of generating an error when press enter twice in Invoice # field [Begin]
loFormSet.ariaform1.lsOperLins.Enabled = .F.
*B999999,1 [End]

*!*************************************************************
*! Name      : lfvInv
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Valid Invoice Field.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvInv()
*!*************************************************************
FUNCTION lfvInv
PARAMETERS loFormSet

lcInvoice = loFormSet.ariaForm1.keyInvoice.keytextbox.Value
ldInvDate = {}
lcInvCust = ''
llBrowse  = loFormSet.ariaForm1.keyInvoice.selectedfrombrowse  OR ALLTRIM(lcInvoice)='?'
lcInvoice = IIF(ALLTRIM(lcInvoice)='?',"",lcInvoice)

lnReturn  = 1
SELECT INVHDR
DIMENSION laSelected[3]
IF (!EMPTY(lcInvoice) AND !SEEK(ALLTRIM(lcInvoice),'INVHDR'))  OR llBrowse
  lcBrFields  = [INVOICE :H=']+LANG_UPSPACK_LblInvoice+[',INVDATE :H=']+LANG_UPSPACK_LblDate+[',ACCOUNT :H=']+LANG_UPSPACK_LblAccount+[',]+;
                [STORE :H=']+LANG_UPSPACK_LblStore+[',ORDER :H=']+LANG_UPSPACK_LblOrder+[',CUSTOMER.STNAME :H=']+LANG_UPSPACK_LblName+[':20 ,]+;
                [SHIP :H=']+LANG_UPSPACK_LblPieces+[',TOTALCHG :H=']+LANG_UPSPACK_LblAmount+[',]+;
                IIF(loFormSet.lcWareH ='Y',[cWareCode :H=']+LANG_UPSPACK_LblWareHouse+[',],[])+;
                IIF(loFormSet.llMulCurr,[cCurrCode :H=']+LANG_UPSPACK_LblCurrency+[',NexRate :H=']+LANG_UPSPACK_LblCurrRate+[',],[])+;
                [REP1 :H=']+LANG_UPSPACK_LblRep1+[', COMM1 :H=']=LANG_UPSPACK_LblComm+[',REP2 :H=']+LANG_UPSPACK_LblRep2+[', COMM2 :H=']+LANG_UPSPACK_LblComm+[',]+;
                IIF(gfIsEdtble('CTERMCODE'),[cTermCode :H=']+LANG_UPSPACK_LblTerms+['],;
                [lcDesc= gfCodDes(cTermCode,'CTERMCODE') :H=']+LANG_UPSPACK_LblTerms+['])

  IF AriaBrow('',LANG_UPSPACK_BrowHdr,;
               gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',;
               "Invoice,InvDate,ShipVia","laSelected")
    lcInvoice = laSelected[1]                              
    ldInvDate = laSelected[2]               
    lcShipVia = laSelected[3]               

    IF lfChkInv()  
      IF SEEK(IIF(EMPTY(INVHDR.STORE),'M'+INVHDR.Account,;
                'S'+INVHDR.Account+INVHDR.STORE),'CUSTOMER')
        lcInvCust = ALLTRIM(CUSTOMER.StName)
      ENDIF
      lnReturn = 1
    ELSE
      =gfModalGen('INM00000B00000','ALERT',.F.,.F.,LANG_UPSPACK_MsgNtUPS)
      STORE '' TO lcInvoice,lcInvCust,lcShipVia  

      *B999999,1 MMT Fix bug of generating an error when press enter twice in Invoice # field [Begin]
      loFormSet.ariaform1.lsOperLins.Enabled = .F.
      loFormSet.ariaForm1.lsOperLins.RowSource  = ''
      *B999999,1 [End]
      ldInvDate = {}
      lnReturn = 0
    ENDIF  
  ELSE
    STORE '' TO lcInvoice,lcInvCust,lcShipVia  
    ldInvDate = {}
    lnReturn = 0
  ENDIF 
ELSE
  IF !EMPTY(lcInvoice) AND SEEK(ALLTRIM(lcInvoice),'INVHDR') AND SEEK(IIF(EMPTY(INVHDR.STORE),'M'+INVHDR.Account,;
            'S'+INVHDR.Account+INVHDR.STORE),'CUSTOMER')
    lcShipVia = INVHDR.ShipVia        
    IF lfChkInv()              
      ldInvDate = INVHDR.InvDate  
      lcInvCust = ALLTRIM(CUSTOMER.StName)
      lnReturn = 1
    ELSE
      *--B99999,1 MMT 05/08/2005,Fix bug of error appears when write inv#[Start]
      =gfModalGen('INM00000B00000','ALERT',.F.,.F.,LANG_UPSPACK_MsgNtUPS)
      loFormSet.ariaForm1.lsOperLins.RowSource  = ''
      loFormSet.ariaform1.lsOperLins.Enabled = .F.
      *--      =gfModalGen('INM00000B00000','ALERT',.F.,.F.,LANG_UPSPACK_MsgNotUPSType)
      *--B99999,1 MMT 05/08/2005,Fix bug of error appears when write inv#[END]
      STORE '' TO lcInvoice,lcInvCust,lcShipVia  
      ldInvDate = {}
      lnReturn = 0
    ENDIF      
  ENDIF
ENDIF    

IF !EMPTY(lcInvoice) AND SEEK(ALLTRIM(lcInvoice),'SHDAIMPR')
  SELECT SHDAIMPR
  
  
  
  lnI = 0
  SCAN REST WHILE cPkPkGrf2 = ALLTRIM(lcInvoice)
    lnI = lnI + 1
    DIME loFormSet.laOperLine[lnI,6]
    
    IF !loFormSet.ariaForm1.lsOperLins.RowSource  = 'loFormSet.laOperLine'
       loFormSet.ariaForm1.lsOperLins.RowSource  = 'loFormSet.laOperLine'
        *B999999,1 MMT Fix bug of generating an error when press enter twice in Invoice # field [Begin]
        loFormSet.ariaform1.lsOperLins.Enabled = .T.
        *B999999,1 [End]
    ENDIF
    
    loFormSet.laOperLine[lnI,1] = SHDAIMPR.csiboknum 
    loFormSet.laOperLine[lnI,2] = SHDAIMPR.csipagnum 
    loFormSet.laOperLine[lnI,3] = CTOD(SUBSTR(SHDAIMPR.csipkupdt,5,2)+'/'+SUBSTR(SHDAIMPR.csipkupdt,7,2)+'/'+ SUBSTR(SHDAIMPR.csipkupdt,1,4))
    loFormSet.laOperLine[lnI,4] = SHDAIMPR.npkgweght 
    loFormSet.laOperLine[lnI,5] = SUBSTR(SHDAIMPR.cpktrknum,1,18)
    loFormSet.laOperLine[lnI,6] = IIF(SHDAIMPR.cpkgisvod = 'Y',LANG_UPSPACK_Voided,LANG_UPSPACK_Active)
    loFormSet.ariaForm1.lsOperLins.Requery
  ENDSCAN  
  
 * loFormSet.ariaForm1.lsOperLins.RowSource  = ''
  
  loFormSet.ariaForm1.keyInvoice.keytextbox.enabled = .F.
  loFormSet.ariaform1.cmdPrint.Enabled = .T.
  loFormSet.ariaForm1.lsOperLins.listindex = 1

  loFormSet.ariaForm1.cmdClose.Caption = LANG_UPSPACK_ButtCancel
ELSE
  ldInvDate   = {}
  lcInvCust   = ''
  DIMENSION laOperLine[1,6] 
  STORE '' TO laOperLine 
  loFormSet.ariaForm1.lsOperLins.Requery 
  
  loFormSet.ariaForm1.lsOperLins.ListIndex = 1
  
  loFormSet.ariaForm1.keyInvoice.keytextbox.enabled = .T.
  loFormSet.ariaform1.cmdPrint.Enabled = .F.
  *B999999,1 MMT Fix bug of generating an error when press enter twice in Invoice # field [Begin]
  loFormSet.ariaForm1.lsOperLins.enabled = .F.
  *B999999,1 [End]
  loFormSet.ariaForm1.cmdClose.Caption = LANG_UPSPACK_ButtClose
ENDIF    
loFormSet.ariaForm1.keyInvoice.keytextbox.Value = lcInvoice
loFormSet.ariaForm1.txtCustName.Value = lcInvCust   
loFormSet.ariaForm1.dtInvDate.value   = ldInvDate   
loFormSet.ariaForm1.keyInvoice.selectedfrombrowse = .F.
RETURN lnReturn 

*!*************************************************************
*! Name      : lfChkInv
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Collect Invoices
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfChkInv()
*!*************************************************************
FUNCTION lfChkInv

IF SEEK('N'+lcShipVia+'Y'+'SHIPVIA','CODES')
  SELECT CODES
  lnI = 0
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+INVHDR.ShipVia+'Y'+'SHIPVIA'
    lnI = lnI + 1
    DIMENSION laShpRlt[lnI,2]
    laShpRlt[lnI,1] = CODES.crltd_nam
    laShpRlt[lnI,2] = ALLTRIM(CODES.cRltd_vlu)
  ENDSCAN
  lnFound = ASCAN(laShpRlt,'CUPS')
  IF lnFound > 0
    lnFound  = ASUBSCRIPT(laShpRlt,lnFound,1)
    RETURN (SUBSTR(laShpRlt[lnFound,2],1,5) = 'USUPS')
  ENDIF  
ENDIF
*!*************************************************************
*! Name      : lfvClose
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Collect Invoices
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfvClose
PARAMETERS loFormSet

lcInvoice = loFormSet.ariaForm1.keyInvoice.keytextbox.Value
IF !EMPTY(lcInvoice)
  loFormSet.ariaForm1.lsOperLins.RowSource  = ''
  loFormSet.ariaForm1.keyInvoice.keytextbox.Value = ''
  loFormSet.ariaForm1.txtCustName.Value = ""
  loFormSet.ariaForm1.dtInvDate.value   = {}
  DIMENSION loFormSet.laOperLine[1,6] 
  STORE '' TO loFormSet.laOperLine 
  loFormSet.ariaForm1.cmdClose.Caption = LANG_UPSPACK_ButtClose
  loFormSet.ariaForm1.lsOperLins.Requery  
  loFormSet.ariaForm1.lsOperLins.Refresh
  loFormSet.ariaForm1.keyInvoice.keytextbox.enabled = .T.
  loFormSet.ariaform1.cmdPrint.Enabled = .F.
  *B999999,1 MMT Fix bug of generating an error when press enter twice in Invoice # field [Begin]
  loFormSet.ariaform1.lsOperLins.Enabled = .T.
  *B999999,1 [End]
ELSE
  loFormSet.release()
ENDIF

*!*************************************************************
*! Name      : lfvModify
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Valid Modify Button
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvModify()
*!*************************************************************
FUNCTION lfvModify
PARAMETERS loFormSet

PRIVATE lcPrintAll , lcAlias , lnLoop , laCompAdd , llOpnSyCmp ,;
        lcCustPo , lcStore
       
lcAlias = ALIAS()
lcPrintAll = gfTempName()
lcInvoice  = loFormSet.ariaForm1.keyInvoice.keytextbox.Value
ldInvDate  = loFormSet.ariaForm1.dtInvDate.value
lcInvCust  = loFormSet.ariaForm1.txtCustName.Value
DECLARE laCompAdd[6]
laCompAdd = ''                   && Array to hold the Company  address

lcSqlStatment   = [SELECT * FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
loSqlConnection = CREATEOBJECT('remotedataaccess')

lnConnectionHandlar = loSqlConnection.sqlrun(lcSqlStatment,"SYCCOMP","SYCCOMP",oAriaApplication.SystemConnectionString,3,;
                                      'SAVE')
IF lnConnectionHandlar = 1
  =CURSORSETPROP("Buffering",3,"SYCCOMP")
  *-- To initialize the indecis that will be created for each file
  lcUserTable    = gfTempName()
  INDEX ON cComp_Id TAG cComp_Id OF (oAriaApplication.WorkDir+lcUserTable+".CDX")
  loSqlConnection = NULL
ELSE
  =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  loSqlConnection = NULL
ENDIF

IF lnConnectionHandlar >= 1
  SELECT SycComp
  =SEEK(oAriaApplication.ActiveCompanyID)
  lcCompName = cCom_Name             && Company Name.
  lcCompPhon = cCom_Phon             && Company Phone.
  lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.
  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  laCompAdd[6] = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  USE IN SycComp
ENDIF

IF SEEK(ALLTRIM(lcInvoice),'INVHDR')
  lcCustPo = InvHdr.CustPo
  lcStore  = InvHdr.Store
ENDIF

CREATE CURSOR (lcPrintAll) (csiboknum C(7), csipagnum C(6), dsipkupdt D(10), ;
                            npkgweght N(6), cpktrknum C(18), cpkgisvod C(6))
APPEND FROM ARRAY loFormSet.laOperLine FOR cpkgisvod = LANG_UPSPACK_Active
SELECT (lcPrintAll)
LOCATE
lcOGPlatForm = 'WINDOWS'
lnChoose = gfModalGen('INM00000B40017','DIALOG','','',LANG_UPSPACK_MsgPrint) 
DO CASE
  CASE lnChoose = 1
    oAriaApplication.gcDevice = "PRINTER"
*    IF pSetup(.T.)
    lcReportHome = oAriaApplication.ReportHome
    lcRprtNam    = oAriaApplication.ActiveModuleID + '\ARUPSGM.FRX'
    TRY  
      REPORT FORM "&lcReportHome.&lcRprtNam" TO PRINTER PROMPT NOCONSOLE NOEJECT 
    CATCH
 *     REPORT FORM "&lcReportHome.&lcRprtNam" TO PRINTER PROMPT NOCONSOLE NOEJECT 
    ENDTRY 
*    ENDIF  
  CASE lnChoose = 2
    oAriaApplication.gcDevice = "SCREEN"
    DO gfDispRep WITH oAriaApplication.ActiveModuleID + '\ARUPSGM.FRX'
ENDCASE

USE IN (lcPrintAll)
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF


*!**************************************************************************
*! Name      : lfAdrShift
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/02/2003
*! Purpose   : Shift the add. if there is any empty one
*!**************************************************************************
*! Example   : = lfAdrShift()
*!**************************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
  *--IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End of lfAdrShift.
