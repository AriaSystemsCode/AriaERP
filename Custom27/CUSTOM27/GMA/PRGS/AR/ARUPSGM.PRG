*:************************************************************
*: Program file  : ARUPSGM.PRG
*: Program desc. : UPS Package Traking 
*: For screen    : Menu
*:        System : Aria Advantage Series.
*:        Module : Accounts Receivable (AR).
*:     Developer : HEND GHANEM (HBG)
*:     Entry     : C#200352 
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Modifications :
*:************************************************************
*C200558,1 SSE 05/15/2003 Some modifications in UPS Package Tracking screen.
*C200580,1 SSE 07/02/2003 Custom UPS Tracking Report.

STORE "" TO lcInvoice,lcInvCust
ldInvDate = {}
llMulCurr = .F.
lcWinCh0 = gfTempName()
lcWinCh1 = gfTempName()
lcWinCh2 = gfTempName()
lcTmpInv = gfTempName()

lcUPSWeb  = gfGetMemVar('M_UPSWEB',gcAct_Comp)
lcWareH   = gfGetMemVar('M_WareHouse',gcAct_Comp)
llMulCurr = gfGetMemVar('llMulCurr',gcAct_Comp)   && Use Multi Currency       
lcPrompt = '\<Close'

=lfCrtFiles()


*-- Declare the globle variables.
DEFINE POPUP puOprts MOVER SCROLL MARGIN MARK ''
lsOperLins  = 1
DIME laOperLine[1,6]
lcWindTitl = "UPS Package Tracking : Form"

PUSH KEY
lcOnEsc = ON('KEY','ESCAPE')
ON KEY LABEL ESCAPE DO gfEscap
DO (gcScrDir+gcWinAppl+"\ARUPSGM.SPX")
ON KEY LABEL ESCAPE &lcOnEsc
POP KEY

RELEASE POPUP puOprts

IF glQuitting
  IF USED(lcTmpInv)
    USE IN (lcTmpInv)
    ERASE (gcWorkDir+lcTmpInv+'.DBF')
    ERASE (gcWorkDir+lcTmpInv+'.CDX')
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfUnTrapKeys
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Releases Key traps necessary for browse.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfUnTrapKeys()
*!*************************************************************
FUNCTION lfUnTrapKeys

ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    
ON KEY LABEL ESC DO lpDetEsc
RETURN

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfoldvalue()
*!*************************************************************
FUNCTION lfoldvalue

lcOldValue = EVALUATE(SYS(18))
RETURN

*!*************************************************************
*! Name      : lpDetEsc
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Trap Esc for lines entry.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpDetEsc
*!*************************************************************
PROCEDURE lpDetEsc
_CUROBJ = OBJNUM(pbCls)
KEYBOARD '{ENTER}'
RETURN

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
SELECT INVHDR
DIMENSION laSelected[3]

IF !EMPTY(lcInvoice) AND !SEEK(ALLTRIM(lcInvoice),'INVHDR')
  lcBrFields  = "INVOICE :H='Invoice',INVDATE :H='Date',ACCOUNT :H='Account',"+;
                "STORE :H='Store',ORDER :H='Order',CUSTOMER.STNAME :H='Name':20 ,"+;
                "SHIP :H='Pieces',TOTALCHG :H='Amount',"+;
                IIF(lcWareH ='Y',"cWareCode :H='Warehouse',",'')+;
                IIF(llMulCurr,"cCurrCode :H='Currency',NexRate :H='Curr.Rate',",'')+;
                "REP1 :H='Rep1', COMM1 :H='Comm',REP2 :H='Rep2', COMM2 :H='Comm',"+;
                IIF(gfIsEdtble('CTERMCODE'),"cTermCode :H='Terms'",;
                "lcDesc= gfCodDes(cTermCode,'CTERMCODE') :H='Terms'")

  IF AriaBrow('',"Invoices",;
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
    ELSE
      =gfModalGen('INM00000B00000','ALERT',.F.,.F.,'Ship Via for the selected invoice is not UPS type.')
      STORE '' TO lcInvoice,lcInvCust,lcShipVia  
      ldInvDate = {}
      _CUROBJ = OBJNUM(lcInvoice)
    ENDIF  
  ELSE
    STORE '' TO lcInvoice,lcInvCust,lcShipVia  
    ldInvDate = {}
    _CUROBJ = OBJNUM(lcInvoice)
  ENDIF 
ELSE
  IF !EMPTY(lcInvoice) AND SEEK(ALLTRIM(lcInvoice),'INVHDR') AND SEEK(IIF(EMPTY(INVHDR.STORE),'M'+INVHDR.Account,;
            'S'+INVHDR.Account+INVHDR.STORE),'CUSTOMER')
    lcShipVia = INVHDR.ShipVia        
    IF lfChkInv()              
      ldInvDate = INVHDR.InvDate  
      lcInvCust = ALLTRIM(CUSTOMER.StName)
    ELSE
      =gfModalGen('INM00000B00000','ALERT',.F.,.F.,'Ship Via for the selected invoice is not UPS type.')
      STORE '' TO lcInvoice,lcInvCust,lcShipVia  
      ldInvDate = {}
      _CUROBJ = OBJNUM(lcInvoice)
    ENDIF      
  ENDIF
ENDIF    

IF !EMPTY(lcInvoice) AND SEEK(ALLTRIM(lcInvoice),'SHDAIMPR')
  SELECT SHDAIMPR
  lnI = 0
  SCAN REST WHILE cPkPkGrf2 = ALLTRIM(lcInvoice)
    lnI = lnI + 1
    DIME laOperLine[lnI,6]
    laOperLine[lnI,1] = SHDAIMPR.csiboknum 
    laOperLine[lnI,2] = SHDAIMPR.csipagnum 

    *C200558,1 Display the date in screen as a date format instead of free format. [Begin]
    *laOperLine[lnI,3] = SHDAIMPR.csipkupdt 
    laOperLine[lnI,3] = CTOD(SUBSTR(SHDAIMPR.csipkupdt,5,2)+'/'+SUBSTR(SHDAIMPR.csipkupdt,7,2)+'/'+ SUBSTR(SHDAIMPR.csipkupdt,1,4))
    *C200558,1 Display the date in screen as a date format instead of free format. [End]

    laOperLine[lnI,4] = SHDAIMPR.npkgweght 
    laOperLine[lnI,5] = SUBSTR(SHDAIMPR.cpktrknum,1,18)
    laOperLine[lnI,6] = IIF(SHDAIMPR.cpkgisvod = 'Y','Voided','Active')
    =lfFillPop()
  ENDSCAN  
  lsOperLins = CNTBAR('puOprts')
  SHOW GET lcInvoice DISABLE  
  SHOW GET pbModify ENABLE
  lcPrompt = '\<Cancel'
ELSE
  ldInvDate   = ''
  lcInvCust   = ''
  RELEASE BAR ALL OF puOprts
  DIMENSION laOperLine[1,6] 
  STORE '' TO laOperLine 
  lsOperLins  = 1
  SHOW GET lcInvoice ENABLE
  SHOW GET pbModify DISABLE  
  lcPrompt = '\<Close'
ENDIF    

SHOW GET pbCls,1 PROMPT lcPrompt
SHOW GET ldInvDate 
SHOW GET lcInvCust 
SHOW GET lsOperLins
=lfRefresh()

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

*C200580,1 Print/Preview the report. [Begin]
*RUN &lcUPSWeb WWW.UPS.COM

PRIVATE lcPrintAll , lcAlias , lnLoop , laCompAdd , llOpnSyCmp ,;
        lcCustPo , lcStore
       
lcAlias = ALIAS()
lcPrintAll = gfTempName()

DECLARE laCompAdd[6]
laCompAdd = ''                   && Array to hold the Company  address

llOpnSyCmp = gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_ID','SH')

SELECT SycComp
SEEK gcAct_Comp
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
IF llOpnSyCmp
  USE IN SycComp
ENDIF

IF SEEK(ALLTRIM(lcInvoice),'INVHDR')
  lcCustPo = InvHdr.CustPo
  lcStore  = InvHdr.Store
ENDIF

CREATE CURSOR (lcPrintAll) (csiboknum C(7), csipagnum C(6), dsipkupdt D(10), ;
                            npkgweght N(6), cpktrknum C(18), cpkgisvod C(6))
APPEND FROM ARRAY laOperLine FOR cpkgisvod = "Active"
SELECT (lcPrintAll)
LOCATE
lcOGPlatForm = 'WINDOWS'
IF gfModalGen('INM00000B00040','DIALOG','','','Do You Want To') = 1
  gcDevice = "PRINTER"
  IF pSetup(.F.)
    DO gfDispRep WITH gcWinAppl + '\ARUPSGM.FRX'
  ENDIF  
ELSE
  gcDevice = "SCREEN"
  DO gfDispRep WITH gcWinAppl + '\ARUPSGM.FRX'
ENDIF

USE IN (lcPrintAll)
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
*C200580,1 Print/Preview the report. [End]

*!*************************************************************
*! Name      : lfCrtFiles
*! Developer : Timour A. K.
*! Date      : 07/22/97
*! Purpose   : Create temp files.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfCrtFiles()
*!*************************************************************
FUNCTION lfCrtFiles

=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'SHDAIMPR',gcDataDir+'SHDAIMPR','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')


*!*************************************************************
*! Name      : lfFillPop
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Fills popup bars from array
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfFillPop()
*!*************************************************************
FUNCTION lfFillPop
FOR lnCount = 1 to ALEN(laOperLine,1)
  DEFINE BAR lnCount OF puOprts ;
         PROMPT PADR(laOperLine[lnCount,1],10) + ' ' + PADR(laOperLine[lnCount,2],10) + ' ' +;
                PADR(laOperLine[lnCount,3],12) + ' ' + PADR(laOperLine[lnCount,4],8) + ' ' +;
                PADR(laOperLine[lnCount,5],18) + ' ' + PADR(laOperLine[lnCount,6],7)
ENDFOR

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

IF !EMPTY(lcInvoice)
  ldInvDate   = ''
  lcInvCust   = ''
  lcInvoice   = ''
  RELEASE BAR ALL OF puOprts
  DIMENSION laOperLine[1,6] 
  STORE '' TO laOperLine 
  lsOperLins  = 1
  lcPrompt = '\<Close'
  SHOW GET lcInvoice ENABLE
  SHOW GET pbCls,1 PROMPT lcPrompt 
  SHOW GET pbModify DISABLE  
  SHOW GET ldInvDate 
  SHOW GET lcInvCust 
  SHOW GET lsOperLins
  =lfRefresh()
ELSE
  CLEAR READ
ENDIF

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
  
*!**************************************************************************
*! Name      : lfAdrShift
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/02/2003
*! Purpose   : Shift the add. if there is any empty one
*!**************************************************************************
*! Example   : = lfAdrShift()
*!**************************************************************************
*C200580,1
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End of lfAdrShift.
