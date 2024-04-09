****************************************************************************
*: Program file      : POPRSPL.PRG (C#101771)
*: Program desc.     : PROPOSAL \ LICENSE AGREEMENT PRINT FORMS.
*: System            : Aria Apparel System (A27).
*: Module            : Sales Order (SO)
*: Developer         : Khalid Mohi El-Din Mohamed KHM
*: Date              : 03/30/2000
*:**************************************************************************
*: Modifications:
*:B803268,1 KHM 05/10/2000 Fix some bugs in the reports
*:**************************************************************************
*-- No permission to proceed the program if no page was selected.
IF EMPTY(laRpTarget)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'You should select at least one page to print.')
  RETURN
ENDIF
loOGScroll.cCROrientation = 'P'
*-- The account code is a mandatory field. it should not be empty
IF EMPTY(lcRpAcct)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Account code is mandatory. '+;
                                           'It should not be empty.')
  RETURN
ENDIF

*-- If the selected account+store is not found in the customer file.
IF !EMPTY(lcRpStore) AND !SEEK('S'+lcRpAcct+lcRpStore,'Customer')
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'The selected Account/Store is '+;
                                           'not found in the customer file.')
  RETURN
ENDIF

*-- The order # is mandatory. It should not left empty.
IF EMPTY(lcRpOrder)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Order# is mandatory. '+;
                                           'It should not be empty.')
  RETURN
ENDIF

*-- Check the validation of the contact if its not empty
IF !EMPTY(lcRpContct) AND !SEEK('C'+PADR(lcRpAcct,8,' ')+;   
   IIF(EMPTY(lcRpStore),SPACE(8),lcRpStore)+lcRpContct,'Contact')
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Invalid contact.')
  RETURN   
ENDIF

*-- Open the necessary files.
=gfOpenFile (gcDataDir+'objects','objectid ')
=gfOpenFile (gcDataDir+'objlink','Objlnkty')
=SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')

*-- Initializing the necessary variables
DECLARE laShipTo[5,1],laCompAdd[7,1]
STORE '' TO lcShpTName,laShipTo,laCompAdd


*-- To print the selected pages according to the selected of From Type.
=lfPrnPages(lcRpFrmTyp)

*!*************************************************************
*! Name      : lfPrnPages
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To get the selected pages
*!*************************************************************
*! Parameter : lcFormType = 'I' -> Initial System Proposal.
*!                          'P' -> Proposal for Additional Services.  
*!                          'L' -> License Agreement 
*!*************************************************************
*! Example   : = lfPrnPages()
*!*************************************************************
FUNCTION lfPrnPages
PARAMETER lcFormType

lcOgPlatForm = 'WINDOW'
lcTmpCursr   = gfTempName()

*-- If the Cover Letter page was selected to be printed.
*-- Its out of the DO CASE statement because its found in all Form Types.
IF ASCAN(laRpTarget,'Cover Letter') > 0
  lcCovrLetr = gfTempName()
  CREATE CURSOR (lcCovrLetr) (Salutation C(20), cLetter M)

  *-- To get the customer address.  
  =IIF(EMPTY(lcRpStore),SEEK('M'+lcRpAcct,'Customer'),SEEK('S'+lcRpAcct+lcRpStore,'Customer'))
  =lfCustAddr()
  
  *-- To get the salutation
  =SEEK('C'+PADR(lcRpAcct,8,' ')+IIF(EMPTY(lcRpStore),SPACE(8),lcRpStore)+;
                                                     lcRpContct,'Contact')
  *-- To get the body of the letter
  =SEEK(lcRpLetter,'Letters')
  INSERT INTO (lcCovrLetr) (Salutation, cLetter) VALUES (Contact.cContSalut,Letters.mLetrBody)  
  SELECT (lcCovrLetr)
  GOTO TOP

  *B803268,1 KHM 05/09/2000 (Begin) Changing the way of using gfDispRep by
  *B803268,1                using a variable instead of a fixed form name.
  *DO gfDispRe WITH 'SOCVRLTR'
  lcRpFrmId = 'SOCVRLTR'
  *-- Amin
  *=gfCrtFrm(lcRpFrmId,lcOGFormArr,llOGRefForm)
  =gfCrtFrm(lcRpFrmId,'',llOGRefForm)
  *-- Amin
  
  =lfRepPltFr(lcRpFrmId)
  SELECT (lcCovrLetr)
  GOTO TOP
  
  =gfDispRe(lcRpFrmId)
  *B803268,1 KHM 05/09/2000 (End)
  
ENDIF

DO CASE  
  CASE lcFormType = 'I'   &&-- Initial System Proposal 
    
    *-- Cover Sheet Page
    IF ASCAN(laRpTarget,'Cover Sheet') > 0
      *-- To get the customer name.
      =SEEK('M'+lcRpAcct,'Customer')
      
      *-- Creating this cursor with a dummy field in order to select it
      *-- when calling the report.      
      CREATE CURSOR (lcTmpCursr) (cDummy C(1))
      INSERT INTO (lcTmpCursr) (cDummy) VALUES (' ')
      SELECT (lcTmpCursr)
      GOTO TOP  
    
      *B803268,1 KHM 05/09/2000 (Begin) Changing the way of using gfDispRep by
      *B803268,1                using a variable instead of a fixed form name.
      *DO gfDispRe WITH 'SOCVRSHT'
      lcRpFrmId = 'SOCVRSHT'
      
      *-- Amin
		  *=gfCrtFrm(lcRpFrmId,lcOGFormArr,llOGRefForm)
		  =gfCrtFrm(lcRpFrmId,'',llOGRefForm)
		  *-- Amin

      
      
      
      =lfRepPltFr(lcRpFrmId)            
      SELECT (lcTmpCursr)
      GOTO TOP
	  
      =gfDispRe(lcRpFrmId)
      *B803268,1 KHM 05/09/2000 (End)
      
    ENDIF

    *-- Proposal Body page
    =lfBody('SOPBODY')

    *-- Implementation Sheet
    IF ASCAN(laRpTarget,'Implementation Sheet') > 0
      *-- Creating this cursor with a dummy field in order to select it
      *-- when calling the report.      
      IF !USED(lcTmpCursr)
        CREATE CURSOR (lcTmpCursr) (cDummy C(1))
        INSERT INTO (lcTmpCursr) (cDummy) VALUES (' ')
      ENDIF      
      *-- To get the customer name
      =SEEK('M'+lcRpAcct,'Customer')
      lcShpTName  = Customer.StName
   
      STORE 0 TO lnHours,lnUsers

      *-- To calculate number of hours for training.
      SELECT OrdLine
      SEEK 'O'+lcRpOrder
      SUM REST TotQty WHILE cOrdType+Order+STR(LineNo,6) = 'O'+lcRpOrder;
                      FOR Style = 'ONSITE-SUP' TO lnHours
      lcHours = ALLTRIM(STR(lnHours))

      *-- To calculate number of users.
      DO CASE
        CASE Customer.PriceLvl = 'A'
          SELECT OrdLine
          SEEK 'O'+lcRpOrder
          SUM REST TotQty WHILE cOrdType+Order+STR(LineNo,6) = 'O'+lcRpOrder;
                          FOR Style = 'ADD-USER' TO lnUsers
          lnUsers   = lnUsers + 5                
          lcUsersNo = ALLTRIM(STR(lnUsers))
        CASE Customer.PriceLvl = 'B'
          lcUsersNo = '3'
        CASE Customer.PriceLvl = 'C'
          lcUsersNo = '1'
      ENDCASE  
      SELECT (lcTmpCursr)
      GOTO TOP
      
      *B803268,1 KHM 05/09/2000 (Begin) Changing the way of using gfDispRep by
      *B803268,1                using a variable instead of a fixed form name.
      *DO gfDispRe WITH 'SOIMPLEM'
      lcRpFrmId = 'SOIMPLEM'      
      *-- Amin
      *=gfCrtFrm(lcRpFrmId,lcOGFormArr,llOGRefForm)
      =gfCrtFrm(lcRpFrmId,'',llOGRefForm)
      *-- Amin      
      =lfRepPltFr(lcRpFrmId)
      SELECT (lcTmpCursr)
      GOTO TOP  
	  
      =gfDispRe(lcRpFrmId)
      *B803268,1 KHM 05/09/2000 (End)
      
    ENDIF

    *-- 'Exhibit A'
    IF ASCAN(laRpTarget,'Exhibit A') > 0 
      lcExhbtA = gfTempName()
      
      CREATE CURSOR (lcExhbtA) (cStyle C(19), cStyDesc C(20), cStyMemo M)      

      *B803268,1 KHM 05/09/2000 (Begin) Removing the index from the cursor
      *B803268,1                in order to print the lines the way they
      *B803268,1                appear in the OrdLine. 
      *INDEX ON cStyle TAG(lcExhbtA) OF (lcExhbtA)
      *B803268,1 KHM 05/09/2000 (End)

      SELECT OrdLine
      SEEK 'O'+lcRpOrder
      SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+lcRpOrder        
        
        *B803268,1 KHM 05/09/2000 (Begin) Check if the style does not
        *B803268,1                have a notepad then do not add it.
        IF !SEEK('F'+Style,'NotePad')
          LOOP
        ENDIF
        *B803268,1 KHM 05/09/2000 (End)
        
        *B803268,1 KHM 05/09/2000 (Begin) Commenting the SEEK command in.
        *B803268,1                the lcExhbtA and the IF SEEK command in 
        *B803268,1                the notepad because we already check for 
        *B803268,1                it above.
        *IF !SEEK(Style,lcExhbtA)        
          =SEEK(Style,'Style')
          INSERT INTO (lcExhbtA) (cStyle,cStyDesc) VALUES (OrdLine.Style,;
                                                           Style.Desc)         
         
          *IF SEEK('F'+Style,'NotePad')
            =lfUpdNotes()
          *ENDIF
        *ENDIF
        *B803268,1 KHM 05/09/2000 (End)
      ENDSCAN  
      
      SELECT (lcExhbtA)
      GOTO TOP  

      *B803268,1 KHM 05/09/2000 (Begin) Changing the way of using gfDispRep by
      *B803268,1                using a variable instead of a fixed form name.
      *DO gfDispRe WITH 'SOEXHBTA'
      lcRpFrmId = 'SOEXHBTA'
      *-- Amin
      *=gfCrtFrm(lcRpFrmId,lcOGFormArr,llOGRefForm)
      =gfCrtFrm(lcRpFrmId,'',llOGRefForm)
      *-- Amin
      =lfRepPltFr(lcRpFrmId)
      SELECT (lcExhbtA)
      GOTO TOP
	  
      =gfDispRe(lcRpFrmId)
      *B803268,1 KHM 05/09/2000 (End)
      
    ENDIF

    *-- 'Exhibit B'
    IF ASCAN(laRpTarget,'Exhibit B') > 0
      *-- Creating this cursor with a dummy field in order to select it
      *-- when calling the report.      
      IF !USED(lcTmpCursr)
        CREATE CURSOR (lcTmpCursr) (cDummy C(1))
        INSERT INTO (lcTmpCursr) (cDummy) VALUES (' ')
      ENDIF      
      SELECT (lcTmpCursr)
      GOTO TOP  
      
      *B803268,1 KHM 05/09/2000 (Begin) Changing the way of using gfDispRep by
      *B803268,1                using a variable instead of a fixed form name.
      *DO gfDispRe WITH 'SOEXHBTB'
      lcRpFrmId = 'SOEXHBTB'
      *-- Amin
      *=gfCrtFrm(lcRpFrmId,lcOGFormArr,llOGRefForm)
      =gfCrtFrm(lcRpFrmId,'',llOGRefForm)
      *-- Amin
      =lfRepPltFr(lcRpFrmId)
      SELECT (lcTmpCursr)
      GOTO TOP  
	  
      =gfDispRe(lcRpFrmId)
      *B803268,1 KHM 05/09/2000 (End)
      
    ENDIF

    *-- 'Exhibit C'
    IF ASCAN(laRpTarget,'Exhibit C') > 0
      *-- Creating this cursor with a dummy field in order to select it
      *-- when calling the report.      
      IF !USED(lcTmpCursr)
        CREATE CURSOR (lcTmpCursr) (cDummy C(1))
        INSERT INTO (lcTmpCursr) (cDummy) VALUES (' ')
      ENDIF      
      SELECT (lcTmpCursr)
      GOTO TOP  
      
      *B803268,1 KHM 05/09/2000 (Begin) Changing the way of using gfDispRep by
      *B803268,1                using a variable instead of a fixed form name.
      *DO gfDispRe WITH 'SOEXHBTC'
      lcRpFrmId = 'SOEXHBTC'
			*-- Amin
      *=gfCrtFrm(lcRpFrmId,lcOGFormArr,llOGRefForm)
      =gfCrtFrm(lcRpFrmId,'',llOGRefForm)
			*-- Amin      
      =lfRepPltFr(lcRpFrmId)
      SELECT (lcTmpCursr)
      GOTO TOP  
	  
      =gfDispRe(lcRpFrmId)
      *B803268,1 KHM 05/09/2000 (End)
      
    ENDIF
  CASE lcFormType = 'P'  && Additional Services Form
    *-- To get the company address
    =lfCompAdd()

    *-- Proposal Body page
    =lfBody('SOASBODY')

  CASE lcFormType = 'L'
    *-- To get the company address
    =lfCompAdd()
    
    *-- Proposal Body page
    =lfBody('SOLABODY')    
ENDCASE

*!*************************************************************
*! Name      : lfCustAddr
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To get the customer address.
*!*************************************************************
*! Example     : = lfCustAddr()
*!*************************************************************
FUNCTION lfCustAddr

lcShpTName  = Customer.StName
laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
=lfAdrShift('laShipTo')
RETURN ''

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 6
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfUpdNotes
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To get the order line notes
*!*************************************************************
*! Example     : = lfUpdNotes()
*!*************************************************************
FUNCTION lfUpdNotes
PRIVATE lnAlias

lnAlias = SELECT(0)

SELECT NotePad
lnNotLine = 1
lnMemLins = MEMLINES(NOTEPAD.MNOTES)

DO WHILE lnNotLine <= lnMemLins
  IF  lnNotLine > 3 AND EMPTY(MLINE(MNOTES,lnNotLine))
    EXIT
  ENDIF
  IF !EMPTY(MLINE(MNOTES,lnNotLine))
    REPLACE &lcExhbtA..cStyMemo WITH &lcExhbtA..cStyMemo + MLINE(MNOTES,lnNotLine)+CHR(13)
  ENDIF  
  lnNotLine = lnNotLine + 1
ENDDO
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfCompAdd
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To get the Company address.
*!*************************************************************
*! Example     : = lfCompAdd()
*!*************************************************************
FUNCTION lfCompAdd
*--038561 , Convert to Aria 4
lcCompInfo = loOgScroll.gfTempName()
lcSqlCommand="SELECT SYCCOMP.CCOM_NAME,SYCCOMP.CCOM_PHON,cCom_Fax,caddress1,caddress2,caddress3,caddress4,caddress5,caddress6,ccont_code "
lcSqlCommand=lcSqlCommand+"  FROM SYCCOMP WHERE CCOMP_ID ='"+OARIAAPPLICATION.ACTIVECOMPANYID+"'  "
LOCAL lnResult
lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlCommand,"",lcCompInfo,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
IF lnResult = 1
  SELECT &lcCompInfo
*  lcCompName = cCom_Name
  lcCompPhon = cCom_Phon              && Variable to hold the Company Phone
  lcPhonPict  = gfPhoneTem()          && Variable to hold the Company Phone Format
  lcCompFax = cCom_Fax               && Variable to hold the Company Fax
  * Get the company addresses
  laCompAdd[1]    = gfGetAdr(lcCompInfo , '' , '' , '' , 1)
  laCompAdd[2]    = gfGetAdr(lcCompInfo , '' , '' , '' , 2)
  laCompAdd[3]    = gfGetAdr(lcCompInfo , '' , '' , '' , 3)
  laCompAdd[4]    = gfGetAdr(lcCompInfo , '' , '' , '' , 4)
  laCompAdd[5]    = gfGetAdr(lcCompInfo , '' , '' , '' , 5)
  laCompAdd[6]    = 'Phone : '+TRANSFORM(lcCompPhon ,'@R '+lcPhonPict)
  laCompAdd[7]    = 'Fax : '+TRANSFORM(lcCompFax,'@R '+lcPhonPict)
ENDIF 

*--038561 , Convert to Aria 4

*!*	=gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
*!*	SEEK gcAct_Comp
*!*	lcCompPhon  = cCom_Phon             && Variable to hold the Company Phone
*!*	lcPhonPict  = gfPhoneTem()          && Variable to hold the Company Phone Format
*!*	lcCompFax   = cCom_Fax
*!*	laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
*!*	laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
*!*	laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
*!*	laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
*!*	laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
*!*	laCompAdd[6]    = 'Phone : '+TRANSFORM(lcCompPhon , lcPhonPict)
*!*	laCompAdd[7]    = 'Fax : '+TRANSFORM(lcCompFax, lcPhonPict)

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : The validation function of the Option Grid.
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

IF lnOGSeting = 1
  DIMENSION laRpSource[7],laRpTarget[7,1]
  STORE '' TO laRpSource,laRpTarget

  *-- Filling the source array and the target array.
  STORE 'Cover Letter'         TO laRpSource[1],laRpTarget[1,1]
  STORE 'Cover Sheet'          TO laRpSource[2],laRpTarget[2,1]
  STORE 'Proposal Body'        TO laRpSource[3],laRpTarget[3,1]
  STORE 'Implementation Sheet' TO laRpSource[4],laRpTarget[4,1]
  STORE 'Exhibit A'            TO laRpSource[5],laRpTarget[5,1]
  STORE 'Exhibit B'            TO laRpSource[6],laRpTarget[6,1]
  STORE 'Exhibit C'            TO laRpSource[7],laRpTarget[7,1]
ENDIF  
*-- Disable the Print license Agreement Amendment Statement
*--038561,Convert To ARIA 4
*lnPosition = ASCAN(laOGObjType,'lcRpLicAAS')
lnPosition = ASCAN(laOGObjType,'LCRPLICAAS')
*--038561,Convert To ARIA 4
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGObjType,lnPosition,1)
  laOGObjCnt[lnPosition] = .F.
  *--038561,Convert To ARIA 4
  *=lfOGShowGet('lcRpLicAAS')
  =lfOGShowGet('LCRPLICAAS')
  *--038561,Convert To ARIA 4
ENDIF
*--038561,Convert To ARIA 4
*=lfOGShowGet('lcRpLetter')
=lfOGShowGet('LCRPLETTER')
*--038561,Convert To ARIA 4
*-- End of lfwRepWhen()

*!*************************************************************
*! Name      : lfvFrmType
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To validate the form type
*!*************************************************************
*! Example     : = lfvFrmType()
*!*************************************************************
FUNCTION lfvFrmType

*-- Initially enable the General Letter Code option.
*--038561,convert to aria 4
*lnPosition = ASCAN(laOGObjType,'lcRpLetter')
lnPosition = ASCAN(laOGObjType,'LCRPLETTER')
*--038561,convert to aria 4
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGObjType,lnPosition,1)
  laOGObjCnt[lnPosition] = .T.
*--038561,convert to aria 4
  =lfOGShowGet('LCRPLETTER')
 * =lfOGShowGet('lcRpLetter')
*--038561,convert to aria 4
ENDIF

DO CASE
  *-- Initial System Proposal
  CASE lcRpFrmTyp = 'I'
    DIMENSION laRpSource[7],laRpTarget[7,1]
    STORE '' TO laRpSource,laRpTarget
    STORE 'Cover Letter'         TO laRpSource[1],laRpTarget[1,1]
    STORE 'Cover Sheet'          TO laRpSource[2],laRpTarget[2,1]
    STORE 'Proposal Body'        TO laRpSource[3],laRpTarget[3,1]
    STORE 'Implementation Sheet' TO laRpSource[4],laRpTarget[4,1]
    STORE 'Exhibit A'            TO laRpSource[5],laRpTarget[5,1]
    STORE 'Exhibit B'            TO laRpSource[6],laRpTarget[6,1]
    STORE 'Exhibit C'            TO laRpSource[7],laRpTarget[7,1]
    
    *-- Initially enable the Print Travel/Lodging Expenses Line.
	*--038561,convert to aria 4    
    lnVarPos = ASCAN(laOGObjType,'LCRPLODEXP')
     *   lnVarPos = ASCAN(laOGObjType,'lcRpLodExp')
    *--038561,convert to aria 4
    IF lnVarPos > 0
      lnVarPos = ASUBSCRIPT(laOGObjType,lnVarPos,1)
      laOGObjCnt[lnVarPos] = .T.
      *--038561,convert to aria 4
 *     =lfOGShowGet('lcRpLodExp')
      =lfOGShowGet('LCRPLODEXP')
      *--038561,convert to aria 4
    ENDIF

    *-- Initially disable the Print License Agreement Amendment Statement.
    *--038561,convert to aria 4
    lnPosition = ASCAN(laOGObjType,'LCRPLICAAS')
    *lnPosition = ASCAN(laOGObjType,'lcRpLicAAS')
    *--038561,convert to aria 4
    IF lnPosition > 0
      lnPosition = ASUBSCRIPT(laOGObjType,lnPosition,1)
      laOGObjCnt[lnPosition] = .F.
      *--038561,convert to aria 4
      *=lfOGShowGet('lcRpLicAAS')
      =lfOGShowGet('LCRPLICAAS')
      *--038561,convert to aria 4
    ENDIF

  *-- Proposal for Additional Services 
  CASE lcRpFrmTyp = 'P'
    DIMENSION laRpSource[2],laRpTarget[2,1]
    STORE '' TO laRpSource,laRpTarget
    STORE 'Cover Letter'         TO laRpSource[1],laRpTarget[1,1]
    STORE 'Proposal Body'        TO laRpSource[2],laRpTarget[2,1]

    *-- Initially enable the Print Travel/Lodging Expenses Line.
    *--038561,convert to aria 4
    *lnPosition = ASCAN(laOGObjType,'lcRpLodExp')
    lnPosition = ASCAN(laOGObjType,'LCRPLODEXP')
    *--038561,convert to aria 4
    IF lnPosition > 0
      lnPosition = ASUBSCRIPT(laOGObjType,lnPosition,1)
      laOGObjCnt[lnPosition] = .F.
      *--038561,convert to aria 4
      *=lfOGShowGet('lcRpLodExp')
      =lfOGShowGet('LCRPLODEXP')
      *--038561,convert to aria 4
    ENDIF

    *-- Initially enable the Print License Agreement Amendment Statement.
    *--038561,convert to aria 4
    lnPosition = ASCAN(laOGObjType,'LCRPLICAAS')
    *lnPosition = ASCAN(laOGObjType,'lcRpLicAAS')
    *--038561,convert to aria 4
    IF lnPosition > 0
      lnPosition = ASUBSCRIPT(laOGObjType,lnPosition,1)
      laOGObjCnt[lnPosition] = .T.
      *--038561,convert to aria 4
      * =lfOGShowGet('lcRpLicAAS')
      =lfOGShowGet('LCRPLICAAS')
      *--038561,convert to aria 4
    ENDIF

      
  *-- License Agreement
  CASE lcRpFrmTyp = 'L'
    DIMENSION laRpSource[2],laRpTarget[2,1]
    STORE '' TO laRpSource,laRpTarget
    STORE 'Cover Letter'           TO laRpSource[1],laRpTarget[1,1]
    STORE 'License Agreement Body' TO laRpSource[2],laRpTarget[2,1]    
        
    *-- Initially disable the Print Travel/Lodging Expenses Line.
    *--038561,convert to aria 4
    *lnVarPos = ASCAN(laOGObjType,'lcRpLodExp')
    lnVarPos = ASCAN(laOGObjType,'LCRPLODEXP')
    *--038561,convert to aria 4
    IF lnVarPos > 0
      lnVarPos = ASUBSCRIPT(laOGObjType,lnVarPos,1)
      laOGObjCnt[lnVarPos] = .T.
      *--038561,convert to aria 4
      *=lfOGShowGet('lcRpLodExp')
      =lfOGShowGet('LCRPLODEXP')
      *--038561,convert to aria 4
    ENDIF
    
    *-- Initially disable the Print License Agreement Amendment Statement.
    *--038561,convert to aria 4
    lnPosition = ASCAN(laOGObjType,'LCRPLICAAS')
    *lnPosition = ASCAN(laOGObjType,'lcRpLicAAS')
    *--038561,convert to aria 4
    IF lnPosition > 0
      lnPosition = ASUBSCRIPT(laOGObjType,lnPosition,1)
      laOGObjCnt[lnPosition] = .F.
      *--038561,convert to aria 4
      =lfOGShowGet('LCRPLICAAS')
      *=lfOGShowGet('lcRpLicAAS')
      *--038561,convert to aria 4
    ENDIF

ENDCASE
*-- End of lfvFrmType()

*!*************************************************************
*! Name      : lfvPages
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To validate the pages to print.
*!*************************************************************
*! Example     : = lfvPages()
*!*************************************************************
FUNCTION lfvPages
PRIVATE lnPosition
= gfMover(@laRpSource,@laRpTarget,'Pages to Print',.T.,'')

*-- To enable the General Letter Code option if the Cover Letter was 
*-- chose in the print pages option.
*--038561,convert to aria 4
lnPosition = ASCAN(laOGObjType,'LCRPLETTER')
*lnPosition = ASCAN(laOGObjType,'lcRpLetter')
*--038561,convert to aria 4
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGObjType,lnPosition,1)
  laOGObjCnt[lnPosition] = ASCAN(laRpTarget,'Cover Letter') > 0
  *--038561,convert to aria 4
  =lfOGShowGet('LCRPLETTER')
  *=lfOGShowGet('lcRpLetter')
  *--038561,convert to aria 4
ENDIF

*-- If form type is Initial System Proposal or Proposal for Additional Services
*-- then check if the Proposal Body was chose in the print pages option
*-- then enable the Print Travel/Lodging Expenses Line option
IF lcRpFrmTyp $ 'IP'
  *--038561,convert to aria 4
  lnPosition = ASCAN(laOGObjType,'LCRPLODEXP')
  *lnPosition = ASCAN(laOGObjType,'lcRpLodExp')
  *--038561,convert to aria 4
  IF lnPosition > 0
    lnPosition = ASUBSCRIPT(laOGObjType,lnPosition,1)
    laOGObjCnt[lnPosition] = ASCAN(laRpTarget,'Proposal Body') > 0
    *--038561,convert to aria 4
    *=lfOGShowGet('lcRpLodExp')
    =lfOGShowGet('LCRPLODEXP')
    *--038561,convert to aria 4
  ENDIF
  
  *-- If Proposal for Additional Services, then check if the Proposal Body
  *-- was chose in the print pages option then enable the 
  *-- Print License Agreement Amendment Statement.
  IF lcRpFrmTyp = 'P'
    *--038561,convert to aria 4
    *lnPosition = ASCAN(laOGObjType,'lcRpLicAAS')
    lnPosition = ASCAN(laOGObjType,'LCRPLICAAS')
    *--038561,convert to aria 4
    IF lnPosition > 0
      lnPosition = ASUBSCRIPT(laOGObjType,lnPosition,1)
      laOGObjCnt[lnPosition] = ASCAN(laRpTarget,'Proposal Body') > 0
      *--038561,convert to aria 4
      *=lfOGShowGet('lcRpLicAAS')
      =lfOGShowGet('LCRPLICAAS')
      *--038561,convert to aria 4
    ENDIF  
  ENDIF
ENDIF
*-- End of lfvPages()

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To validate the account code.
*!*************************************************************
*! Example     : = lfvAccount()
*!*************************************************************
Function lfvAccount
PRIVATE lcObjNam,lcAccNo




*-- Amin
*IF !EMPTY(lcAccNo) AND !SEEK('M' + lcAccNo , 'Customer'))
*lcObjNam = SYS(18)
*lcAccNo   = EVALUATE(SYS(18))
lcObjNam = ogSys18(.T.)
lcAccNo   = EVALUATE(ogSys18(.T.))
IF !EMPTY(lcAccNo) AND !SEEK('M' + lcAccNo , 'Customer')
*-- Amin
    =CusBrowM(@lcAccNo , '' , 'M')
ENDIF
&lcObjNam = lcAccNo
*-- End of lfvAccount()

*!*************************************************************
*! Name      : lfvStore
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To validate the store code.
*!*************************************************************
*! Example     : = lfvStore()
*!*************************************************************
FUNCTION lfvStore
PRIVATE lcObjNam,lcObjVal,xStore

lcObjNam = ogSys18(.T.)
lcObjVal = EVALUATE(ogSys18(.T.))
xStore   = lcObjVal
IF !EMPTY(lcObjVal) AND !SEEK('S'+lcRpAcct+lcObjVal,'Customer')
  lcObjVal= IIF(CUSBROWS(lcRpAcct,.T.),xStore,SPACE(8))
ENDIF
&lcObjNam = lcObjVal
*-- End of lfvStore()

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To validate the Order#.
*!*************************************************************
*! Example     : = lfvOrder()
*!*************************************************************
FUNCTION lfvOrder
PRIVATE lcObjNam,lcObjVal,lcOrder

lcObjNam = ogSys18(.T.)
lcObjVal = EVALUATE(ogSys18(.T.))

SET ORDER TO Ordacct IN ORDHDR
lcOrder  = lcObjVal
IF !EMPTY(lcObjVal) AND !SEEK(lcRpAcct+'O'+lcObjVal,'OrdHdr')
  =lfOrdBrow(@lcOrder,lcRpAcct)
ENDIF

*-- If the selected order is not a bid order then display all the bid orders
IF SEEK(lcRpAcct+'O'+lcObjVal,'OrdHdr') AND OrdHdr.Status <> 'B'
  =lfOrdBrow(@lcOrder,lcRpAcct)
ENDIF
lcObjVal  = lcOrder
&lcObjNam = lcObjVal
*-- End of lfvOrder()

*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To browse the Bid orders.
*!*************************************************************
*! Example     : = lfOrdBrow()
*!*************************************************************
FUNCTION lfOrdBrow
PARAMETERS lcOrder,lcAccount
PRIVATE laBrowArr

DECLARE laBrowArr[1]
laBrowArr = ''

SELECT ORDHDR
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER

lcBrFields = [Order:H="Order#",]+;
             [status:1:H="S",]+;
             [ACCOUNT:H="Acct",]+;
             [store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
             [Customer.stname:30:H="Name",]+;
             [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
             [Open:H="Open.Qty.",]+;
             [OpenAmt:H="Open.Amt.",]+;
             [Ship:H="Ship.Qty.",]+;
             [Shipamt:H="Ship.Amt.",]+;
             [start:H="Start",]+;
             [Complete:H="Complete",]+;
             [lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",]+;
             [lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
             [Note1:6:H="Notes"]
lcOrder = IIF(ARIABROW("lcRpAcct+'O'"+" FOR Status = 'B'",;
                  "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))
SET RELATION OFF INTO CUSTOMER
*-- End of lfOrdBrow()

*!*************************************************************
*! Name      : lfGLCodes
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To get the general purpose codes from the letter file
*!*************************************************************
*! Example     : = lfGLCodes()
*!*************************************************************
FUNCTION lfGLCodes
DIMENSION laRpGLCode[1,1]
laRpGLCode = ' '
*-- To fill the General purpose letter codes
SELECT cLetterId FROM Letters ;
  WHERE cLeterType = 'G' AND cLeterTo = 'C' INTO ARRAY laRpGLCode
IF _TALLY = 0
  laRpGLCode[1]= 'N/A'
ENDIF
*-- End of lfGLCodes

*!*************************************************************
*! Name      : lfvContact
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To validate the contact code.
*!*************************************************************
*! Example     : = lfvContact()
*!*************************************************************
FUNCTION lfvContact
PRIVATE lcObjNam,lcObjVal,lcContact

lcObjNam  = ogSys18(.T.)
lcObjVal  = EVALUATE(ogSys18(.T.))
lcContact = lcObjVal

IF !EMPTY(lcObjVal) AND !SEEK('C'+PADR(lcRpAcct,8,' ')+;
                        IIF(EMPTY(lcRpStore),SPACE(8),lcRpStore)+lcContact,'Contact')
 SELECT Contact

  lcBrFields = [Contact:H="Contact",]+;
               [Phone:H="Phone",]+;
               [Fax:H="Fax",]+;
               [cContttl:H="Contact Title"]
  lcContact = IIF(ARIABROW("'C'+PADR(lcRpAcct,8,' ')+IIF(EMPTY(lcRpStore),SPACE(8),lcRpStore)",;
                  "Contacts",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Contact','laBrowArr'),;
                    Contact.Contact,SPACE(6))

ENDIF
lcObjVal = lcContact
&lcObjNam = lcObjVal

*!*************************************************************
*! Name      : lfGetOrdLn
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To get the order lines.
*!*************************************************************
*! Example     : = lfGetOrdLn()
*!*************************************************************
FUNCTION lfGetOrdLn
PRIVATE lnSubTax,lnSubNTax

STORE 0 TO lnSubTax,lnSubNTax

SELECT OrdLine
SEEK 'O'+lcRpOrder
SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+lcRpOrder

  =SEEK(Style,'Style')
  INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                           nAmount,nPercent,cMemo);
                 VALUES ('O',IIF(Style.lTaxable,'A','G'),OrdLine.Style,;
                         IIF(EMPTY(OrdLine.Desc1),Style.Desc1,OrdLine.Desc1),;
                         OrdLine.LineNo,OrdLine.TotQty,OrdLine.Price*OrdLine.TotQty,0,'')
  IF !EMPTY(OrdLine.Note_Mem)
    INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                             nAmount,nPercent,cMemo);
                     VALUES ('O',IIF(Style.lTaxable,'A','G'),OrdLine.Style,;
                             '',OrdLine.LineNo,0,0,0,OrdLine.Note_Mem)
  
  ENDIF
  *-- This logical variable will be used in the grid to determine whether to
  *-- print a text or not
  IF !lPrintTxt
    lPrintTxt = (Style = 'CUST-PRG')
  ENDIF

  *-- To get the numbers of free hours.
  IF Style = 'ONSITE-SUP'
    lnHours = lnHours+TotQty
  ENDIF  
ENDSCAN
*-- To be printed in the report
lnHours = IIF(lnHours<30,lnHours,30)

SELECT (TmpOrdLin)

*-- SubTotal for taxable styles
IF SEEK('O'+'A')
  SUM REST nAmount WHILE cGroup+cCode+STR(nLineNo,6) = 'O'+'A';
      TO lnSubTax
  *B803268,1 KHM 05/09/2000 (Begin) Commented out and moved to be under the 
  *B803268,1                condition of discount > 0
  *-- SubTotal for taxable style
  *INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                           nAmount,nPercent,cMemo);
                   VALUES ('O','B','ZZZZZZZZZZZZZZZZZZ1',;
                           '',999999,0,lnSubTax,0,'')
  *-- Discount
  SET ORDER TO ORDHDR IN ORDHDR
  =SEEK('O'+lcRpOrder,'OrdHdr')

  IF OrdHdr.Disc > 0
    *B803268,1 KHM 05/09/2000 (Begin) Adding a subtotal record if there
    *B803268,1                is a discount.  
    INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                           nAmount,nPercent,cMemo);
                   VALUES ('O','B','ZZZZZZZZZZZZZZZZZZ1',;
                           '',999999,0,lnSubTax,0,'')
    *B803268,1 KHM 05/09/2000 (End)
    
    *B803268,1 KHM 05/09/2000 (Begin) Getting the ROUND instead of INT
    *lnDiscAmnt = INT(lnSubTax*(OrdHdr.Disc/100)*-1)
    lnDiscAmnt = ROUND((lnSubTax*(OrdHdr.Disc/100)*-1),2)
    *B803268,1 KHM 05/09/2000 (End)
    
    INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                             nAmount,nPercent,cMemo);
                 VALUES ('O','C','ZZZZZZZZZZZZZZZZZZ2',;
                         '',999999,0,lnDiscAmnt,0,'')    
    *-- Subtotal after discount 
    *B803268,1 KHM 05/09/2000 (Begin) Getting the ROUND instead of INT
    *lnSubTax = INT(lnSubTax + lnDiscAmnt)
    lnSubTax = ROUND((lnSubTax + lnDiscAmnt),2)
    *B803268,1 KHM 05/09/2000 (End)
    
    *B803268,1 KHM 05/09/2000 (Begin) Adding a record for subtotal after 
    *B803268,1                discount if there is a non-taxable style or 
    *B803268,1                Customer.nTaxRate > 0
    IF SEEK ('O'+'G') OR Customer.nTaxRate > 0
    *B803268,1 KHM 05/09/2000 (End)
      INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                             nAmount,nPercent,cMemo);
                     VALUES ('O','D','ZZZZZZZZZZZZZZZZZZ3',;
                             '',999999,0,lnSubTax,0,'')
    ENDIF
  ENDIF
  *-- To calculate Sales Tax.
  IF Customer.nTaxRate > 0
    *B803268,1 KHM 05/09/2000 (Begin) Getting the ROUND of 2 decimal points
    *lnTaxAmnt = ROUND(lnSubTax * (Customer.nTaxRate/100),0)
    lnTaxAmnt = ROUND(lnSubTax * (Customer.nTaxRate/100),2)
    *B803268,1 KHM 05/09/2000 (End)
    
    INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                             nAmount,nPercent,cMemo);
                     VALUES ('O','E','ZZZZZZZZZZZZZZZZZZ4',;
                             '',999999,0,lnTaxAmnt,0,'')
    *B803268,1 KHM 05/09/2000 (Begin) Getting the ROUND of 2 decimal points
    *lnSubTax = ROUND(lnSubTax + lnTaxAmnt,0)
    lnSubTax = ROUND(lnSubTax + lnTaxAmnt,2)
    *B803268,1 KHM 05/09/2000 (End)

    *B803268,1 KHM 05/09/2000 (Begin) Adding a record for subtotal of sales
    *B803268,1                tax if there is a non taxable styles.
    IF SEEK ('O'+'G')
    *B803268,1 KHM 05/09/2000 (End)
    *-- SubTotal after adding the sales tax.
      INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                               nAmount,nPercent,cMemo);
                       VALUES ('O','F','ZZZZZZZZZZZZZZZZZZ5',;
                               '',999999,0,lnSubTax,0,'')
    ENDIF
  ENDIF
ENDIF

*-- SubTotal for Taxable + Non-Taxable styles.
IF SEEK ('O'+'G')
  SUM REST nAmount WHILE cGroup+cCode+STR(nLineNo,6) = 'O'+'G' TO lnSubNTax
  INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                           nAmount,nPercent,cMemo);
                   VALUES ('O','H',OrdLine.Style,;
                           '',999999,0,lnSubTax+lnSubNTax,0,'')
*B803268,1 KHM 05/09/2000 (Begin) Adding a record for grand total if
*B803268,1                there is no taxable styles.
ELSE
  INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                           nAmount,nPercent,cMemo);
                   VALUES ('O','H',OrdLine.Style,;
                           '',999999,0,lnSubTax,0,'')
*B803268,1 KHM 05/09/2000 (End)
ENDIF

*-- To get the payment schedule
IF SEEK('PY'+lcRpOrder,'BomVar')
  lnNo = 1
  SELECT BomVar
  SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6) = 'PY'+lcRpOrder
   INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                            nAmount,nPercent,cMemo);
                   VALUES ('P','I','ZZZZZZZZZZZZZZZZZZ'+ALLTRIM(STR(lnNo)),;
                           ALLTRIM(BomVar.mSizes),999999,0,BomVar.TotCost,BomVar.nPercent,'')
    lnNo = lnNo + 1
  ENDSCAN
ENDIF

*-- To get the Order note pad.
IF SEEK('B'+lcRpOrder,'NotePad')
   INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                            nAmount,nPercent,cMemo);
                   VALUES ('S','J','ZZZZZZZZZZZZZZZZZZZ',;
                           '',999999,0,0,0,NotePad.mNotes)
ELSE
  *-- To print the footer
 INSERT INTO (TmpOrdLin) (cGroup,cCode,cStyle,cStyDesc,nLineNo,nTotQty,;
                         nAmount,nPercent,cMemo);
                VALUES ('S','K','',;
                        '',999999,0,0,0,'')

ENDIF

                        
*!*************************************************************
*! Name      : lfDefLett
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To get the default letter
*!*************************************************************
*! Parameter : lcReport : SOPBody,SOASBody,SOLABody
*!*************************************************************
*! Example     : = lfDefLett()
*!*************************************************************
FUNCTION lfDefLett
*-- To get the default general purpose letter
lcDefLett=' '
SELECT Letters
lcOldOrd = ORDER('LETTERS')
SET ORDER TO Leterdefa
IF SEEK ('D'+'C'+'G')
  lcDefLett = Letters.cLetterId
ELSE
  lcDefLett='N/A'
ENDIF
SET ORDER TO &lcOldOrd
=ASCAN(laRpGLCode,lcDefLett)
RETURN lcDefLett

*!*************************************************************
*! Name      : lfBody
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/02/2000 
*! Purpose   : To print the body
*!*************************************************************
*! Parameter : lcReport : SOPBody,SOASBody,SOLABody
*!*************************************************************
*! Example     : = lfBody()
*!*************************************************************
FUNCTION lfBody
PARAMETER lcReport

IF IIF(lcFormType $'IP',ASCAN(laRpTarget,'Proposal Body') > 0,;
                        ASCAN(laRpTarget,'License Agreement Body') > 0)
  *-- To check the level of the price for the selected customer.
  *-- and to get the sales tax.
  =SEEK('M'+lcRpAcct,'Customer')
  *B803268,1 KHM 05/09/2000 (Begin) Adding the follwoing variable in order
  *B803268,1                to be printed in the License of Agreement Form.
  lcShipAdd1 = Customer.cAddress1
  lcShipAdd2 = Customer.cAddress2
  lcShipAdd3 = ALLTRIM(Customer.cAddress3)+', '+ALLTRIM(Customer.cAddress4)+;
               ', '+ALLTRIM(Customer.cAddress5)
  *B803268,1 KHM 05/09/2000 (End)             
  *-- Get the customer address
  =lfCustAddr()
  *-- To get the description of the state.
  lcStateDsc = IIF(Customer.nTaxRate>0,;
               ALLTRIM(gfCodDes(ALLTRIM(Customer.cAddress4),'STATE')),'')
  lPrintTxt  = .F.
  lcStyOrd   = ORDER('Style')
  lnHours    = 0
  SET ORDER TO STYLE IN STYLE
    
  TmpOrdLin  = gfTempName()
  CREATE CURSOR (TmpOrdLin) (cGroup C(1), cCode C(1), cStyle C(19),;
                             cStyDesc C(60), nLineNo N(6),nTotQty N(7),;
                             nAmount N(9,2),nPercent N(5,2),cMemo M)
  INDEX ON cGroup+cCode+STR(nLineNo,6) TAG(TmpOrdLin) OF (TmpOrdLin)
  *-- Get the order lines and prepare them in order to print the order.
  =lfGetOrdLn()
            
  SELECT (TmpOrdLin)      
  GOTO TOP

  *B803268,1 KHM 05/09/2000 (Begin) Changing the way of using gfDispRep by
  *B803268,1                using a variable instead of a fixed form name.
  *DO gfDispRe WITH lcReport
  lcRpFrmId = lcReport
   *-- Amin
  =gfCrtFrm(lcRpFrmId,'',llOGRefForm)
  *-- Amin
  =lfRepPltFr(lcRpFrmId)
  SELECT (TmpOrdLin)      
  GOTO TOP
  
  =gfDispRe(lcRpFrmId)  
  *B803268,1 KHM 05/09/2000 (End)
  
  SET ORDER TO &lcStyOrd IN STYLE
  IF lcFormType = 'L'
    *-- To print the terms and conditions
    lcTremsCur = gfTempName()
    CREATE CURSOR (lcTremsCur) (cGroup C(1))
    INSERT INTO (lcTremsCur) (cGroup) VALUES ('S')
    SELECT (lcTremsCur)
    GOTO TOP

    *B803268,1 KHM 05/09/2000 (Begin) Changing the way of using gfDispRep by
    *B803268,1                using a variable instead of a fixed form name.
    *DO gfDispRe WITH 'SOTERMS'
    lcRpFrmId = 'SOTERMS'
    *-- AMin
    *=gfCrtFrm(lcRpFrmId,lcOGFormArr,llOGRefForm)
    =gfCrtFrm(lcRpFrmId,'',llOGRefForm)
    *-- AMin    
    =lfRepPltFr(lcRpFrmId)
    SELECT (lcTremsCur)
    GOTO TOP
	
    =gfDispRe(lcRpFrmId)
    *B803268,1 KHM 05/09/2000 (End)
    
  ENDIF
ENDIF
