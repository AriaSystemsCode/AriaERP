*:************************************************************************
*: Program file  : ArCust.PRG
*: Program desc. : Customer Add,Change Inquire
*:
*:         System: Aria Apparel System
*:      Developer: WAM - Wael Aly Mohamed
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Modifications      :
*E300872,8 WAM 05/04/98 Call Trace Key function.
*E300878,1 WAM 05/07/98 Default new accounts to use consolidated invoices
*E300834,1 WAM 08/11/98 Add new price level 'At Quantity'
*E300941,1 WAM 08/16/98 Add Installment# in the Debit Finantial Browse
*B801805,1 Reham On 11/15/98
*B801805,1 Disable the notes button in the add mode.
*B602184,1 Reham On 11/15/98
*B602184,1 Fix the trappinp in the sales rep. 1 in case there is no reps.
*B602184,1 in the sales reps. file.
*B602179,1 AMM 11/25/98 Erase temporary files when quitting the program
*B602166,1 Reham On 11/30/98
*B602166,1 Add the title & Salutaion fields in the Browse contacts folder.
*B801858,1 WAM 12/17/98 Check necessary codes before run the program.
*B801928,1 WAM 02/03/99 Default store Back Order setting to customer's.
*B801952,1 WAM 02/11/99 define variable used by notepad as privare
*B801949,1 WAM 02/11/99 Restore customer old gl link code when browse failed
*B602550,1 WAM 02/18/99 Remove force address fields entry
*B801874,1 WAM 02/20/99 Fix browse first salesrep
*E301077,49 WAM 03/01/99 Inhance openning files to speed up transaction
*E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*E301176,1                as it's now controlled globally
*B602697,1 Reham On 03/25/99
*B602697,1 Add the contact field into the pending browse.
*E301196,1 ARH 04/08/99 Defaulted the consolidated setting to YES
*E301196,1              if the country is England
*C101427,1 Walid 04/21/99 Make an indicator in financial screen to indicates
*          if the customer has post dated checks or not and brows them if any
*          - Make the Debit folder is the default one 
*          - Remove Post Dated Checks from option menu 
*B602754,1 Reham On 05/11/1999
*B602754,1 Fix the error if press Ctrl+N.
*B802172,1 Reham On 05/16/1999
*B802172,1 1- Add when function for the state field in the screen.
*B802172,1 2- Execute the lfvstate function only if press enter.
*B602902,1 Hesham 05/20/1999
*B602902,1 when running user task list while we are in the pending browse window
*B602902,1 the program perform a general protection error
*B802216,1 WAM 05/28/99 Return to customer record after inquire transaction
*C200076,1 SSE 06/06/1999 Added defaults for 3 new User Fields that appear in Custom screen. 
*E301245,1 SSE 06/09/1999 Added a new field nHgWtrMark and calcualte its value
*                         from NETBAL field , if less than NETBAL update with 
*                         NETBAL else leave it with existing value
*E301245,1                Also default commissions of Rep1,Rep2 from SalesRep File 
*                         default commission field AND ipon creating new customer when a second Rep
*                         is entered, both commission will be reduced by Split Commission percentage
*E301246,1 SSE 06/07/1999 Added Summary Information Bar in OPTION PAD to call Summary Information Screen
*E301270,1 Reham On 06/20/1999
*E301270,1 Add the contact name in the history browse.
*E301249,1 IHB 06/22/1999 Make priority field C(3) in CUSTOMER 
*E301249,1 				  and ORDHDR files
*B603023,1 HDM 06/29/1999 Fix : customer screen leaving 'ALT+B' trapped
*B603023,1 				  after we exit the screen
*B802363,1 HDM 06/29/1999 In returns browse for main account we should include all its stores
*B603018,1 BWA 07/06/1999 Let the user access to see the cost from customer history or no.
*B802554,1 ALB 09/01/1999 Fixed bug : When entering Rep1 & comm1 ,if Rep2 's left
*B802554,1                empty , comm1's value is restored from file  
*B603189,1 BWA 10/05/1999 Fix the bug of the Amount field which is partially visible and the user has 
*B603189,1                to scroll to be able to view the amount. I decreased the size of 
*B603189,1                inst#, and reference to be able to see the whole field without having to scroll.
*B603253,1 RAMY 11/15/1999 Default the value of the priority field to the 
*B603253,1                 value entered in the setups
*B802873,1 ABD 12/12/1999 1- Fix Bug in the locate feature When browsing orders of the customer.
*B802873,1                2- Fix Bug in the locate feature When browsing Returns of the customer.
*E301119,1 MAB 10/30/1999 Add salutation screen to menu options.
*B802390,1 NAD 04/19/2000 Fix tab function in customer finisntial screen
*C101764,1 NAD 04/26/2000 Add the Email Address to the contact file 
*E301429,1 NAD 06/19/2000 Increase the amount field in the financial screen.
*B803388,1 AME 07/01/2000 In History folder & in Pindidng folder view all records.
*B803387,1 BWA 07/04/2000 Fix the bug of file in use when the user run the tasklist screen with 
*B803387,1                the customer/history folder screen.
*B803528,1 ABD 07/26/2000 Add filed NETBAL to browse filed in sydfiles to filter on the 
*B803528,1 ABD            Customer for only the customers that have a positive balance.
*B602885,1 ABD 07/27/2000 Fix bug that give message error "Operator/Operand type mismatch".
*B802622,1 ABD 07/31/2000 Display the adjustment reason description in history , Credit and Debit batch
*B802388,1 ABD 07/31/2000 Fix Bug in the history folder The cursor should stay on the selected batch.
*B803578,1 SSE 08/22/2000 Fix bug of having meaningful fields in the Customer Filter option in 
*B803578,1                Record Bar menu , all modifications is in SydField 
*B603871,1 AMH 09/05/2000 Fix Bug of 'Arcushst Alias Not Found' in Customer history screen
*E301465,1 Reham On 09/06/2000 Add new field cCustPass in the customer file.
*E500368,1 AMH 09/06/2000 Set the default Status of new customer to be Potential
*B603904,1 MHM 10/02/2000 Fix bug of browsing worng stores in case  of multi stores.
*B603593,1 MHM 10/02/2000 Fix bug of The main account of billing is editable in customer screen.
*B803864,1 MHM 12/20/2000 Fix bug windows is not defined (for both history and pending screens)
*E301557,1 AHM 02/07/2001 Send the customer information through EDI, Organizational Relationship
*E301557,1                Transaction
*E301581,1 AME 03/19/2001 Add new field "CstoreGLN " to Customer file. This done in Spceial folder in customer screen
*B604088,1 SSE 04/30/2001 Adjust Magnifying glass to be exactly like the Account browse.
*B604489,1 SSE 06/25/2001 Fix bug of variable 'cSelect not found' when deleting Customer.
*B604505,1 SSE 06/27/2001 Fix bug of not updating the contact file but inserting a new record every time.
*B604505,1                This bug happens only when editing the Contact ID field.
*B604594,1 SSE 06/27/2001 Fix bug of increasing User defined fields to be 30 Char instead of 20 Char.
*B604520,1 SSE 06/28/2001 Fix bug of removing and adding the same dept it is not saved.
*E301569,1 SSE 06/28/2001 Enh. in Dept. screen it should be defaulted to the main account code.
*B804095,1 SSE 06/28/2001 Fix bug when entering Rep code in Dept. screen it should display the Commission from Customer's Rep screen. 
*B605573,1 TMI 03/21/2002 Move the amount field to be visible within arcusfn.scx without scrolling.
*C102589,1 TMI 03/26/2002 Add a button to Contact screen to add/remove Roles of a contact
*B#605895 ,1 SSH [Start] Assigne "M" to the customer type (ladata[1]) incase of add from 
*B#605895 ,1 SSH         phone object.
*B#606027 ,1 SSH [Start] Fix bug of variable "lcContRole"  not found.
*C102565,1 TMI 05/05/2002 Enable the client to know if any user did any changes on the customer file
*B605625,1 RAE 05/09/2002 Display the Total credit amount - Gross Amount - in customer screen
*                         for Returns Browse instead of Net Amount.
*C200339,1 TMI 05/08/2002 Define Sales reps for each division for a givin customer.
*B606017,1 TMI 06/17/2002 Fix the bug that A lost bmp file when open dept screen
*B606178,1 TMI 07/08/2002 Open History file with descending order on completed data 
*B605825,1 TMI 07/25/2002 Optimze opening browse screen for shipped styles for Customer and Store
*B606444,1 TMI 10/30/2002 Get the style description from InvLine file not from style file
*E302089,1 ABD 01/20/2003 The default customer status should be based on setup.
*B607391,1 KHM 07/06/2003 Get the reason description instead of reason code.
*B607403,1 TMI 07/16/2003 Fix the bug that deleteing a store which is a DC for another store
*B605086,1 NNA 12/28/2003 Change the message which appear when changing the Alternate ship via.
*E037853,1 HBG 02/16/2004 Change the width of Key field in EDITRANS to 40 char
*B123164,1 NNA 07/12/2004 Fix bug that when we change the class for the main account it change 
*B123164,1 NNA            only for the main account and leave the stores with the old Class code
*C200597,1 TMI 08/19/2004 Call a customized tracekey function for Bong Wha
*B038431,1 NNA 09/30/2004 Fix bug that when you open the customer Screen you'll find that the menu
*B038431,1 NNA            icons are Disable but when you Open the Style Screen on the Customer Screen
*B038431,1 NNA            then close the Style screen to return the customer Screen you'll find that
*B038431,1 NNA            the icons are enable even you didn't select a Customer
*C123847,1 TMI 01/31/2005  Add a trigger for DIR03 to set the field CUSTOMER.LCMPCSTPO to Yes as default
*B127178,1 NNA 05/05/2005 Fix bug that orders for a cancelled account can be updated
*B127522,1 EIH 06/02/2005 Fix bug that when browse customer by name get valid customer selected for
*B127522,1 EIH            This name , account and store in case of multi store account
*C123851,1 TMI 06/07/2005 Update altship data files 
*B128585,1 ASH 06/17/2005 Add the comments fields to the post dated checks browse.
*B039299,1 TMI 06/21/2005 We should not Cancel / Delete the Customer or its stores if it has an open/hold material sales order
*C128481,1 NNA 09/12/2005 Add a new trigger for Pan21 to make Lcharge Filed as True with the new Customers
*B130045,1 NNA 11/20/2005 Fix bug that credit Avaliable not updated by cash receipt,credit adj.,
*B130045,1 NNA            debit Adj. or Return merchandize.
*B130389,1 EIH 01/07/2006 Fix bug that when the user does not have costing information privilege he can login to the history 
*B130389,1 EIH 			  Screen but we delete the columns "COGS,PROFIT,NET PROFIT" from the screen.
********************************************************************************************** 
PARAMETERS lcCustCode,lcStoreCode
EXTERNAL ARRAY laData,laKeyField

DECLARE lafolders[8,2],laAddress[6,3],lafoldwinds[8,2],laKeyField[3,4],;
        laSetups[5,2],laReBrowse[4],laDRltFld[2,2],laBrowArr[1]
DECLARE laCodes[7,10],laClass[1,2],laTerms[1,2],laDivision[1,2],laDeptTerms[1,2],;
        laRegion[1,2],laShipVia[1,2],laAShipVia[1,2],laSpcInst[1,2],laContries[1,2],;
        laGraphFlt[1,7],laFromBrow[6]
*C102589,1 TMI [Start]
DECLARE laRole[1],laTarget[1]      && Array to hold Roles from codes file, and Selected roles
*C102589,1 TMI [End  ]
STORE '' TO laCodes,laClass,laTerms,laDivision,laRegion,laShipVia,laAShipVia,;
            laSpcInst,laContries,lcCustLink,lcCustSales,laBrowArr,lcExclFld,laDeptTerms
            
STORE '' TO lcContFile,lcProFile,lcCustDept,lcProVal
STORE '' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcWinCh41,lcWinCh42,;
            lcWinCh5,lcWinCh51,lcWinCh52,lcWinCh6,lcWinCh61,lcWinCh62,lcWinCh7,;
            lcWinCh71,lcWinCh72,lcWinCh8,laGraphFlt
            
STORE '' TO lcField1,lcField2,lcField3,lcField4,lcField5,lcField6,lcField7,;
            lcField8,lcField9,lcField10,lcDelRec,lcSelDel,lcOldValue,lcAddHed1,lcAddHed2,;
            lcGrfFields,lcTempGrf

*B127178,1 NNA 05/05/2005 (BEGIN) Initialize a new variable to hold the current value for the status popup
STORE 0 TO lnOldValue
*B127178,1 NNA (END)
            
*B603018,1 BWA 07/06/1999 Let the user access to see the cost from customer history or no.            
llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
*B603018,1 BWA [END]

*B603253,1 RAMY 11/15/1999 Get the default value [start]
lcDefaPrior = gfGetMemVar('M_DEFAPRIO')
*B603253,1 RAMY [end]


STORE 0 TO lnGraphTyp,llByRow

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'CUSTOMER'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] = .F.
laKeyField[2,3] = 'CUSTOMER'
laKeyField[2,4] = 2
laKeyField[3,1] = 'laData[3]'
laKeyField[3,2] = .T.
laKeyField[3,3] = 'CUSTOMER'
laKeyField[3,4] = 3

lcExclFld = 'laData[1]'
STORE .F. TO laDefProc[1],laDefProc[2],laDefProc[3],laDefProc[4],;
             llIsEngland,llIsCanada,llMulCurr,llAddAcc,llGlLInk,llSycCurr,llFinancial

*E302089,1 ABD - The default customer status should be based on setup. [Begin]
*STORE 1   TO lnActFolder,lnMarker,lnPMarker,lnNMarker,lnHMarker,lnDMarker,;
             lnStatus,lnBackOrd
STORE 1   TO lnActFolder,lnMarker,lnPMarker,lnNMarker,lnHMarker,lnDMarker,;
             lnBackOrd
lcDefaStat = gfGetMemVar('M_CUSTSTAT')
lnStatus  = AT(lcDefaStat,'PAHX')
*E302089,1 ABD - [End]


*E301196,1 ARH Defaulted the consolidated setting to YES if the country is England
*E300878,1 Default new accounts to use consolidated invoices
*STORE 2  TO lnConsol
*STORE 1   TO lnPricelvl,lnInsur,lnCtaxrule,lnBillto,lnMstatm,lnConsol
STORE 1   TO lnPricelvl,lnInsur,lnCtaxrule,lnBillto,lnMstatm
lnConsol = IIF(gcContCode = 'ENG',1,2)
*E300878,1 (End)
*301196,1 ARH (End)

*B803388,1 AME [Start]
*STORE ''  TO  lcFolder,lcSession,lcScFields,lcBrowseTl,lcHistFile,lcBaseFild,;
              lcBaseTit,lcHistPrd
STORE ''  TO  lcFolder,lcSession,lcScFields,lcBrowseTl,lcHistFile,lcBaseFild,;
              lcBaseTit,lcHistPrd,lcHistIndx,lcTmpTag
*B803388,1 AME [End]


STORE .F. TO laSetups,llNoShow,llBrowse
laDefProc[7]  = .F.     && Void procedure(lpDelScr)
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
laDefProc[10] = .F.     && close procedure(lpClsScr)
STORE 1 TO lnClass,lnTerms,lnDivision,lnRegion,lnShipVia,lnSpcInst,;
           lnAShipVia,lnDeptTerms
STORE 0 TO lnLastMode

*B#606027 ,1 SSH [Start] Fix bug of variable "lcContRole"  not found.
lcContRole  = ""
*B#606027 ,1 SSH [End] Fix bug of variable "lcContRole"  not found.
*C200339,1 TMI [START]
lnDiv      = 0  				&& Hold the selected division
lcDivSlsRp = '' 		&& Hold the temp alias of DIVSLSRP
*C200339,1 TMI [END  ]

IF !gfSetup()
  RETURN
ENDIF
*E301077,49 Inhance openning files to speed up transaction
*STORE .F. TO llCustDept,llContact,llProfile,llPrfCode,llSysChdul,llOrdHdr,llarHist,;
             llOrdLine,llInvHdr,llInvLine,llNotePad,llDebit,llCredit,llConsInv,llStyle
*E301077,49 (END)
lcContTitl = 'Contacts'           && Contact browse title.
lcProfTitl = 'Profiles'           && ProFile browse title.
lcPendTitl = 'Pending'            && Pending browse title.
lcHisTTitl = 'History'            && History browse title.
lcBrowDept = 'Departments'
lcBrDelTtl = 'Account stores'

laFromBrow[1] = lcContTitl 
laFromBrow[2] = lcProfTitl
laFromBrow[3] = lcPendTitl 
laFromBrow[4] = lcHisTTitl 
laFromBrow[5] = lcBrowDept 
laFromBrow[6] = lcBrDelTtl 

lafolders[1,1] = 'General'
lafolders[1,2] = 1
lafolders[2,1] = 'Billing'
lafolders[2,2] = 2
lafolders[3,1] = 'Fields'
lafolders[3,2] = 3
lafolders[4,1] = 'Contacts'
lafolders[4,2] = 4
lafolders[5,1] = 'Profile'
lafolders[5,2] = 5
lafolders[6,1] = 'Pending'
lafolders[6,2] = 6
lafolders[7,1] = 'History'
lafolders[7,2] = 7
lafolders[8,1] = 'Special'
lafolders[8,2] = 8
lnnofld = 8
lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder
lnfoldercend = 102.00
lnfolderrend = 2.000
IF !WEXIST(gcBaseWind)
  *C200339,1 TMI [START]
  lnDiv      = 1  				&& Hold the selected division
  lcDivSlsRp = gfTempName() 		&& Hold the temp alias of DIVSLSRP
  *C200339,1 TMI [START]
  *B801858,1 Check necessary codes before run the program.
  SET ORDER TO TAG Ccode_no IN CODES
  IF !SEEK('N'+'CLASS','CODES')     OR ;
     !SEEK('N'+'CTERMCODE','CODES') OR ;
     !SEEK('N'+'REGION','CODES')    OR ;
     !SEEK('N'+'SHIPVIA','CODES')   OR ;
     !SEEK('N'+'SPCINST','CODES')
    *B801858,1 Message : 40150
    *B801858,1 One or more codes have not been setup. Please setup and try again.
    *B801858,1 Button : 00000
    *B801858,1 Ok
    =gfModalGen('TRM40150B00000','ALERT')
    RETURN
  ENDIF
  *B801858,1 (End)
  *C102589,1 TMI [START] Fill Roles array
  SELECT cCode_No+' - '+cDiscrep FROM Codes ;
     WHERE cDefCode+cRltField+cFld_Name = 'NNCROLE     ' ;
     ORDER BY cCode_No INTO ARRAY laRole
  *C102589,1 TMI [END  ] Fill Roles array
  
  SELECT cDiscrep,cCode_No FROM Codes WHERE cDefCode+cRltField+cFld_Name=;
         'N'+'N'+'SHIPVIA'INTO ARRAY laAShipVia
  DIMENSION laAShipVia[_TALLY+1,2]
  =AINS(laAShipVia,1)
  laAShipVia[1,1]= 'N/A'
  laAShipVia[1,2]= SPACE(6)

  lnGraphTyp = 9
  llByRow = .F.
  lcTempGrf = gfTempName()
  =SEEK(gcContCode,'SycInt')
  lcBrFields = "Account :H='Acct#', BtName :H='Name':R,"+;
               "cAddress3 :H='"+SycInt.cPart3Lab+"':R :P='"+REPLICATE('X',SycInt.nPart3Len)+"',"+;
               "cAddress4 :H='"+SycInt.cPart4Lab+"':R :P='"+REPLICATE('X',SycInt.nPart4Len)+"',"+;
               "cAddress5 :H='"+SycInt.cPart5Lab+"':R :P='"+REPLICATE('X',SycInt.nPart5Len)+"',"+;
               "Phone1 :P= GFPHONETEM() :H='Phone #...',Buyer :H='Buyer',salesrep :H='Rep',NetBal:11:H='Balance'"
  lcBaseFild = lcBrFields
  lcBaseTit  = lcFile_Ttl

  lnLastMode = 0
  IF !FILE(gcDataDir+'ARCUST.MEM')
    FOR lnCount = 1 TO 10
      lcCount = ALLTRIM(STR(lnCount,2))
      lcField&lcCount = 'USER FIELD' + lcCount
    ENDFOR
    SAVE TO (gcDataDir+'ARCUST.MEM') ALL LIKE lcField*
  ENDIF
  RESTORE FROM (gcDataDir+'ARCUST.MEM') ADDITIVE

  *E301581,1 AME [Start] Add the store Gln Field to the fields list
  *lcScFields = 'TYPE,ACCOUNT,STORE,STNAME,PHONE1,STATUS,CADDRESS1,CADDRESS2,'+;
               'CADDRESS3,CADDRESS4,CADDRESS5,CADDRESS6,NTAXRATE,CTAXRULE,'+;
               'DBA,BUYER,KEEPER,PHONE2,SALESREP,COMM,REP2,COMM2,CCONT_CODE,DUNSRTG,'+;
               'CRLIMIT,DISC,DUNS,EXPIRES,UPSZONE,CFACCODE,FACTACCT,FAX,CLASS,'+;
               'cTermCode,cDivision,PRIORITY,PRICELVL,CCURRCODE,REGION,SHIPVIA,'+;
               'SPCINST,NOTE,BTNAME,CADDRESS12,CADDRESS22,CADDRESS32,'+;
               'CADDRESS42,CADDRESS52,CADDRESS62,BILLTO,USR_DFND1,USR_DFND2,'+;
               'USR_DFND3,USR_DFND4,USR_DFND5,USR_DFND6,USR_DFND7,USR_DFND8,'+;
               'USR_DFND9,USR_DFND10,LINK_CODE,CALTSHPVIA,NBRKWEIGHT,CVATNO,'+;
               'UPS_INCR,LVATEXEM,CONSOL,CINSUR,PRNT_STATM,CCONT_COD2,CrAvail,'+;
               'AGEDATE,cSlsGlLink,DIST_CTR,CBACKORD,CCUSVEND,SKUTMPL'
  
  lcScFields = 'TYPE,ACCOUNT,STORE,STNAME,PHONE1,STATUS,CADDRESS1,CADDRESS2,'+;
               'CADDRESS3,CADDRESS4,CADDRESS5,CADDRESS6,NTAXRATE,CTAXRULE,'+;
               'DBA,BUYER,KEEPER,PHONE2,SALESREP,COMM,REP2,COMM2,CCONT_CODE,DUNSRTG,'+;
               'CRLIMIT,DISC,DUNS,EXPIRES,UPSZONE,CFACCODE,FACTACCT,FAX,CLASS,'+;
               'cTermCode,cDivision,PRIORITY,PRICELVL,CCURRCODE,REGION,SHIPVIA,'+;
               'SPCINST,NOTE,BTNAME,CADDRESS12,CADDRESS22,CADDRESS32,'+;
               'CADDRESS42,CADDRESS52,CADDRESS62,BILLTO,USR_DFND1,USR_DFND2,'+;
               'USR_DFND3,USR_DFND4,USR_DFND5,USR_DFND6,USR_DFND7,USR_DFND8,'+;
               'USR_DFND9,USR_DFND10,LINK_CODE,CALTSHPVIA,NBRKWEIGHT,CVATNO,'+;
               'UPS_INCR,LVATEXEM,CONSOL,CINSUR,PRNT_STATM,CCONT_COD2,CrAvail,'+;
               'AGEDATE,cSlsGlLink,DIST_CTR,CBACKORD,CCUSVEND,SKUTMPL,CSTOREGLN'
  *E301581,1 AME [End]
  SELECT Customer
  SCATTER FIELDS &lcScFields TO laData BLANK

  lcSession     = gfsequence('CSESSION')
  llIsEngland   = UPPER(ALLTRIM(gcContCode))='ENG'
  llIsCanada    = UPPER(ALLTRIM(gcContCode))='CANADA'  
  llMulCurr     = gfGetMemVar('llMulCurr',gcAct_Comp)
  laSetups[1,1] = 'M_LINK_GL'
  laSetups[2,1] = 'M_TAX'
  laSetups[3,1] = 'M_DIV_LINK'
  laSetups[4,1] = 'XAGINGTYPE'
  laSetups[5,1] = 'M_PACK'
  =gfGetMemVar(@laSetups,gcAct_Comp)
  lcHistFile  = gfTempName()
  *B803388,1 AME [Start]
  lcHistIndx  = gfTempName()
  lcTmpTag    = gfTempName()
  *B803388,1 AME [End]
  *C102589,1 TMI [Start]
  lcContRole  = gfTempName()   && edited/new roles
  *C102589,1 TMI [End  ]
  lcHistPrd   = gfTempName()
  lcDelRec    = gfTempName()
  lcSelDel    = gfTempName()
  lcContFile  = gfTempName()
  lcProFile   = gfTempName()
  lcProVal    = gfTempName()
  lcCustDept  = gfTempName()
  lcWinCh0    = gfTempName()
  lcWinCh41   = gfTempName()
  lcWinCh42   = gfTempName()
  lcWinCh51   = gfTempName()
  lcWinCh52   = gfTempName()
  lcWinCh61   = gfTempName()  
  lcWinCh62   = gfTempName()
  lcWinCh71   = gfTempName()  
  lcWinCh72   = gfTempName()
  lcfolder    = gfTempName()      && Folder Window Name
  lcfoldprnt  = gcBaseWind        && window parent name for the folder
  lnactfolder = 1                 && active folder
  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    lcWinCh&lcCount = gfTempName()
    lafoldwinds[lnCount,1] = lafolders[lnCount,1]
    lafoldwinds[lnCount,2] = EVAL('lcWinCh'+STR(lnCount,1))
  ENDFOR
  laCodes[1,1] = 'CLASS'
  laCodes[1,2] = 'laClass'
  laCodes[1,3] = 'lnClass'
  laCodes[1,4] = ''
  laCodes[1,5] = .F.
  laCodes[1,6] = .F.
  laCodes[1,7] = 'CUSTOMER'
  laCodes[1,8] = 'CUSTOMER'
  laCodes[1,9] = 'laData[1]+laData[2]+laData[3]'
  laCodes[1,10] = 'CLASS'

  laCodes[2,1] = 'CTERMCODE'
  laCodes[2,2] = 'laTerms'
  laCodes[2,3] = 'lnTerms'
  laCodes[2,4] = ''
  laCodes[2,5] = .F.
  laCodes[2,6] = .F.
  laCodes[2,7] = 'CUSTOMER'
  laCodes[2,8] = 'CUSTOMER'
  laCodes[2,9] = 'laData[1]+laData[2]+laData[3]'
  laCodes[2,10] = 'cTermCode'

  laCodes[3,1] = 'CDIVISION'
  laCodes[3,2] = 'laDivision'
  laCodes[3,3] = 'lnDivision'
  laCodes[3,4] = ''
  laCodes[3,5] = .F.
  laCodes[3,6] = .T.
  laCodes[3,7] = 'CUSTOMER'
  laCodes[3,8] = 'CUSTOMER'
  laCodes[3,9] = 'laData[1]+laData[2]+laData[3]'
  laCodes[3,10] = 'cDivision'

  laDRltFld[1,1] = 'LINK_CODE'
  laDRltFld[1,2] = 'lcCustLink'
  laDRltFld[2,1] = 'CSLSGLLINK'
  laDRltFld[2,2] = 'lcCustSales'

  laCodes[4,1] = 'REGION'
  laCodes[4,2] = 'laRegion'
  laCodes[4,3] = 'lnRegion'
  laCodes[4,4] = ''
  laCodes[4,5] = .F.
  laCodes[4,6] = .F.
  laCodes[4,7] = 'CUSTOMER'
  laCodes[4,8] = 'CUSTOMER'
  laCodes[4,9] = 'laData[1]+laData[2]+laData[3]'
  laCodes[4,10] = 'REGION'

  laCodes[5,1] = 'SHIPVIA'
  laCodes[5,2] = 'laShipVia'
  laCodes[5,3] = 'lnShipVia'
  laCodes[5,4] = ''
  laCodes[5,5] = .F.
  laCodes[5,6] = .F.
  laCodes[5,7] = 'CUSTOMER'
  laCodes[5,8] = 'CUSTOMER'
  laCodes[5,9] = 'laData[1]+laData[2]+laData[3]'
  laCodes[5,10] = 'SHIPVIA'

  laCodes[6,1] = 'SPCINST'
  laCodes[6,2] = 'laSpcInst'
  laCodes[6,3] = 'lnSpcInst'
  laCodes[6,4] = ''
  laCodes[6,5] = .F.
  laCodes[6,6] = .F.
  laCodes[6,7] = 'CUSTOMER'
  laCodes[6,8] = 'CUSTOMER'
  laCodes[6,9] = 'laData[1]+laData[2]+laData[3]'
  laCodes[6,10] = 'SPCINST'

  laCodes[7,1] = 'CPRO_CODE'
  laCodes[7,2] = 'laProfile'
  laCodes[7,3] = 'lnProfile'
  laCodes[7,4] = ''
  laCodes[7,5] = .F.
  laCodes[7,6] = .F.
  laCodes[7,9] = "'C'+PADR(laData[2],8)+laData[3]+m.CPRO_CODE"
  laCodes[7,10] = 'CPRO_CODE'

  SELECT cPart1Lab,cCont_Code FROM SycInt INTO ARRAY laContries
  
  DO CASE 
    CASE TYPE('lcCustCode')='C' .AND. TYPE('lcStoreCode')='C' .AND. ;
       SEEK('S'+PADR(lcCustCode,5)+PADR(lcStoreCode,8),'CUSTOMER')
      SELECT CUSTOMER
      STORE .F. TO laScrMode
      STORE .T. TO laScrMode[2]
      SCATTER FIELDS &lcScFields TO laData
    CASE TYPE('lcCustCode')='C' .AND. SEEK('M'+PADR(lcCustCode,5),'CUSTOMER')
      SELECT CUSTOMER
      STORE .F. TO laScrMode
      STORE .T. TO laScrMode[2]
      SCATTER FIELDS &lcScFields TO laData
  ENDCASE
  
  CREATE TABLE (gcWorkDir+lcHistPrd) ;
  (Period C(3),Order N(15,2), Sales N(15,2), Discount N(15,2),Return N(15,2),;
   COGS N(15,2))

ENDIF
*E301077,49 Inhance openning files to speed up transaction
*llSycCurr=llMulCurr .AND. gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
*llSkuTemp=laSetups[5,2]='Y' .AND. gfOpenFile(gcDataDir+'SKUTMPL',gcDataDir+'SKUTMPL','SH')
*E301077,49 (END)
llGlLInk =laSetups[1,2]='Y' .AND. gfOpenFile(gcDataDir+'Gl_Link',gcDataDir+'Gl_Link1','SH')

*B603023,1 Push keys before key trapping
PUSH KEY
*B603023,1 end

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

lcKey     = gcBmpHome + "ExtKey.BMP"
lcOk      = gcBmpHome + "OK.BMP"
lcNew     = gcBmpHome + "new.bmp"
lcRemove  = gcBmpHome + "rem.bmp"
lcModify  = gcBmpHome + "MODIFY.bmp"
lcSchedule= gcBmpHome + "Schedule.bmp"
lcGraphBmp= gcBmpHome + "graph8.bmp"

*E301176,1 HDM 03/22/1999 [Start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally

*DECLARE laPanelObj[3,3]
DECLARE laPanelObj[2,3]

STORE '' TO laPanelObj

*laPanelObj[1,1] = 'pbNotes'
*laPanelObj[1,2] = gcBmpHome+'NOTES2.BMP'
*laPanelObj[1,3] = [VALID lfvCusNote() MESSAGE 'Customer Notes']

laPanelObj[1,1] = 'pbObjLink'
laPanelObj[1,2] = gcBmpHome+"Relate.bmp"
laPanelObj[1,3] = [VALID lfvObjLink() MESSAGE 'Object Link']

laPanelObj[2,1] = 'pbCusFin'
laPanelObj[2,2] = gcBmpHome+"Finance.bmp"
laPanelObj[2,3] = [VALID lfCallInv(lfFinancial(llMulCurr,laSetups(4,2),laData(2),laData(4),laData(3))) MESSAGE 'Customer Financial']
*lnPanelObj = 3
lnPanelObj = 2
*E301176,1 HDM 03/22/1999 [End]
IF 'SO' $ gcCmpModules
  lnPanelObj = lnPanelObj + 1
  DIMENSION laPanelObj[lnPanelObj,3]
  laPanelObj[lnPanelObj,1] = 'pbCusOrd'
  laPanelObj[lnPanelObj,2] = gcBmpHome+"orders.bmp"
  laPanelObj[lnPanelObj,3] = [VALID lfOrders() MESSAGE 'Customer Orders']
ENDIF
IF 'AR' $ gcCmpModules
  lnPanelObj = lnPanelObj + 2
  DIMENSION laPanelObj[lnPanelObj,3]
  laPanelObj[lnPanelObj-1,1] = 'pbCusInv'
  laPanelObj[lnPanelObj-1,2] = gcBmpHome+"Document.bmp"
  laPanelObj[lnPanelObj-1,3] = [VALID lfInvoices() MESSAGE 'Customer Invoices']
  laPanelObj[lnPanelObj,1] = 'pbCusSty'
  laPanelObj[lnPanelObj,2] = gcBmpHome+"Style.bmp"
  laPanelObj[lnPanelObj,3] = [VALID lfStyles() MESSAGE 'Shipped Styles']
ENDIF
IF 'RM' $ gcCmpModules
  lnPanelObj = lnPanelObj + 2
  DIMENSION laPanelObj[lnPanelObj,3]
  laPanelObj[lnPanelObj-1,1] = 'pbCusRa'
  laPanelObj[lnPanelObj-1,2] = gcBmpHome+"Document.bmp"
  laPanelObj[lnPanelObj-1,3] = [VALID lfCusRa() MESSAGE 'Customer Return Authorizations']
  laPanelObj[lnPanelObj,1] = 'pbCusCr'
  laPanelObj[lnPanelObj,2] = gcBmpHome+"Document.bmp"
  laPanelObj[lnPanelObj,3] = [VALID lfCrMemos() MESSAGE 'Customer Credit Memos']
ENDIF


*B602754,1 Reham On 05/11/1999   *** Begin ***
*B602754,1 Save the current menu.
*-- Clear all the trapped keys.
*B603023,1 Move 'Push key' up before key trapping 
*PUSH KEY
*B603023,1 end
*B602902,1 Hesham (start)
*PUSH MENU _MSYSMENU
*B602902,1 Hesham (End)

*B602754,1 Get the bar# of the next bar in the record pad.
lnBarNo = lfGetBar("GFCPNEXT")
ON SELECTION BAR lnBarNo OF P03PU03 DO gfCPNext
*B602754,1 Get the bar# of the Top bar in the record pad.
lnBarNo = lfGetBar("GFCPTOP")
ON SELECTION BAR lnBarNo OF P03PU03 DO gfCPTop
*B602754,1 Get the bar# of the Bottom bar in the record pad.
lnBarNo = lfGetBar("GFCPBttm")
ON SELECTION BAR lnBarNo OF P03PU03 DO gfCPBttm
*B602754,1 Get the bar# of the Previous bar in the record pad.
lnBarNo = lfGetBar("GFCPPrvis")
ON SELECTION BAR lnBarNo OF P03PU03 DO gfCPPrvis
*B602754,1 Reham On 05/11/1999   *** End   ***

SELECT CUSTOMER
DO (gcScrDir+"ARCUST.SPX")
=lfClearKey()
*B803387,1 BWA 07/04/2000 Fix the bug of file in use when the user run the tasklist screen with 
*B803387,1                the customer/history folder screen.[START]
IF USED(lcHistFile)
  *B803388,1 AME[Start]
  lnali = SELECT()
  SELECT (lcHistFile)
  CLOSE INDEXES
  ERASE (GcWorDir)+lcHistIndx..CDX
  SELECT (lnali)
  *B803388,1 AME[End]
  USE IN (lcHistFile)
ENDIF
*B803387,1 [END]


*B602754,1 Reham On 05/11/1999   *** Begin ***
*B602754,1 Restore the original menu.
*B602902,1 Hesham (start)
*POP MENU _MSYSMENU
*B602902,1 Hesham (End)
POP KEY
*B602754,1 Reham On 05/11/1999   *** End   ***
*B602902,1 Hesham (start)
  RELEASE WINDOW (lcContTitl)
  RELEASE WINDOW (lcProfTitl)  
  RELEASE WINDOW (lcPendTitl)
  RELEASE WINDOW (lcHistTitl)  
  RELEASE WINDOW (lcBrowDept)
  RELEASE WINDOW (lcBrDelTtl)
*B602902,1 Hesham (End)
IF glQuitting
*B602902,1 Hesham (start)
*  RELEASE WINDOW (lcContTitl)
*  RELEASE WINDOW (lcProfTitl)  
*  RELEASE WINDOW (lcPendTitl)
*  RELEASE WINDOW (lcHistTitl)  
*  RELEASE WINDOW (lcBrowDept)
*  RELEASE WINDOW (lcBrDelTtl)
*B602902,1 Hesham (End)
  *B602179,1 AMM Erase temporary files when quitting the program
  IF USED(lcHistPrd)
    USE IN (lcHistPrd)
  ENDIF
  ERASE (gcWorkDir+lcHistPrd+".DBF")
  ERASE (gcWorkDir+lcHistPrd+".CDX")

  IF USED(lcDelRec)
    USE IN (lcDelRec)
  ENDIF
  ERASE (gcWorkDir+lcDelRec+".DBF")
  ERASE (gcWorkDir+lcDelRec+".CDX")
  *B602179,1 AMM end
ENDIF
*E301077,49 Inhance openning files to speed up transaction
*IF llGlLInk
*  USE IN Gl_Link
*ENDIF
*IF llSycCurr
*  USE IN SycCurr
*ENDIF
*IF llCustDept
*  USE IN CUSTDEPT
*ENDIF
*IF llContact
*  USE IN CONTACT
*ENDIF
*IF llProfile
* USE IN ProFile
*ENDIF  
*IF llPrfCode
*  USE IN arPrFCod
*ENDIF
*IF llSysChdul
*  USE IN syschdul
*ENDIF
*IF llOrdHdr
*  USE IN 'ORDHDR'
*ENDIF
*IF llOrdLine
*  USE IN 'ORDLINE'
*ENDIF
*IF llInvHdr
*  USE IN 'INVHDR'
*ENDIF
*IF llInvLine
*  USE IN 'INVLINE'
*ENDIF
*IF llConsInv
*  USE IN 'CONSINVH'
*ENDIF
*IF llStyle
*  USE IN 'STYLE'
*ENDIF
*IF llCredit
*  USE IN 'CREDIT'
*ENDIF
*IF llDebit
*  USE IN 'DEBIT'
*ENDIF
*IF llarHist
*  USE IN 'ARHIST'
*ENDIF
*IF llSycInt 
*  USE IN 'SycInt'
*ENDIF
*IF USED(lcHistFile)
*  USE IN (lcHistFile)
*ENDIF
*E301077,49 (End)
RELEASE PAD _INQUIRY OF _MSYSMENU

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Activate function of INV700
*!*************************************************************
*! Calls     : lfClearKey.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfDARCUST
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Deactivate Customer screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfDARCUST()
*!*************************************************************
FUNCTION lfDARCUST
SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[1]
IF WONTOP()=lcContTitl .OR. WONTOP()=lcProfTitl .OR. WONTOP()=lcPendTitl .OR. ;
   WONTOP()=lcHistTitl
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  DO CASE 
    CASE WONTOP()=lcContTitl
      ON KEY LABEL TAB     DO lpTab WITH lcWinCh42,'pbCNew'
      ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,'ibFolder[4]'
    CASE WONTOP()=lcProfTitl
      ON KEY LABEL TAB     DO lpTab WITH 'gwcContrl1','pbTop'
      ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,'ibFolder[5]'
    CASE WONTOP()=lcPendTitl
      ON KEY LABEL TAB     DO lpTab WITH lcWinCh62,'pbNNew'
      ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,'ibFolder[6]'
    CASE WONTOP()=lcHistTitl
      ON KEY LABEL TAB     DO lpTab WITH lcWinCh72,'pbHRemove'
      ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,'ibFolder[7]'
  ENDCASE
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Change folders
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfActFolder()
*!*************************************************************
FUNCTION lfActFolder

lcStatus = IIF(laScrMode[3] .OR. laScrMode[4],'ENABLE','DISABLE')
SHOW GETS WINDOW (lcwinch42) DISABLE ONLY
SHOW GETS WINDOW (lcwinch52) DISABLE ONLY
SHOW GETS WINDOW (lcwinch62) DISABLE ONLY
SHOW GETS WINDOW (lcwinch72) DISABLE ONLY
DO CASE
  CASE lnActFolder = 1
    IF laData[1] = 'M'
      SHOW GETS WINDOW (lcwinch1) &lcStatus ONLY
    ELSE
      SHOW GETS WINDOW (lcwinch1) DISABLE ONLY
      IF laScrMode[3] .OR. laScrMode[4]
        IF laSetups[2,2]='Y'
          SHOW GET laData[13] ENABLE
        ENDIF
        IF laSetups[2,2]='Y' .AND. llIsCanada
          SHOW GET lnCtaxrule ENABLE
        ENDIF
        SHOW GET laData[19] ENABLE
        SHOW GET laData[20] ENABLE
        SHOW GET laData[21] ENABLE
        SHOW GET laData[22] ENABLE
        SHOW GET laData[42] ENABLE
        SHOW GET lnShipVia  ENABLE
        SHOW GET ibRep1     ENABLE
        SHOW GET ibRep2     ENABLE
      ENDIF  
    ENDIF
    SHOW GET ibBgDumi1 ENABLE
  CASE lnActFolder = 2
    IF laData[1] = 'M'
      SHOW GETS WINDOW (lcwinch2)  DISABLE ONLY
    ELSE
      SHOW GETS WINDOW (lcwinch2) &lcStatus ONLY
      IF laScrMode[2]
        SHOW GET pbcopy ENABLE
      ELSE
        SHOW GET pbcopy DISABLE
      ENDIF
      IF (laScrMode[3] .OR. laScrMode[4]) .AND. lnBillto <> 3
        SHOW GET pbFromSt DISABLE
        *B603593,1 MHM 10/02/2000 [start] 
        SHOW GET laData[43] DISABLE
        SHOW GET laData[44] DISABLE
        SHOW GET laData[45] DISABLE
        SHOW GET laData[46] DISABLE
        SHOW GET laData[47] DISABLE
        SHOW GET laData[48] DISABLE
        SHOW GET laData[49] DISABLE
        SHOW GET ibAddress2 DISABLE
        SHOW GET lcAddHed2  DISABLE
        *B603593,1 MHM 10/02/2000 [End]
      ENDIF
    ENDIF
    SHOW GET ibBgDumi2 ENABLE
  CASE lnActFolder = 3
   SHOW GETS WINDOW (lcwinch3) &lcStatus ONLY
  CASE lnActFolder = 4
    IF laReBrowse[1]
      =gfOpenFile(gcDataDir+'CONTACT',gcDataDir+'CONTACT','SH')
      IF (laScrMode[3] .OR. laScrMode[4]) .AND. !USED(lcContFile)
        SELECT * FROM CONTACT WHERE cContType+cCont_Id+Store+Contact = ;
        'C'+PADR(laData[2],8)+laData[3] INTO DBF (gcWorkDir+lcContFile)
        INDEX ON cContType+cCont_Id+Store+Contact TAG (lcContFile)
        GO TOP IN (lcContFile)
        *C102589,1 TMI [START] Create temp lcContRole file.
        IF !USED('CONTROLE')
          =gfOpenFile(gcDataDir+'CONTROLE',gcDataDir+'CONTROLE','SH')
        ENDIF
        =lfCrTmpRol()
        *C102589,1 TMI [END  ] Create temp lcContRole file.       
      ENDIF
    ENDIF  
    =lfBrowCont('C')
    SHOW GET ibContact ENABLE
    lnRecNo = RECNO()
    IF SEEK('C'+PADR(laData[2],8)+laData[3])
      SHOW GET pbContact ENABLE
      IF laScrMode[3] .OR. laScrMode[4]
        SHOW GET pbCRemove ENABLE
      ENDIF
    ENDIF
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GOTO lnRecNo
    ENDIF
    IF laScrMode[3] .OR. laScrMode[4]
      SHOW GET pbCNew ENABLE
    ENDIF
  CASE lnActFolder = 5
    IF laReBrowse[2]
      =gfOpenFile(gcDataDir+'arPrFCod',gcDataDir+'ProFile','SH')
      IF (laScrMode[3] .OR. laScrMode[4]) .AND. !USED(lcProVal)
        SELECT * FROM arPrFCod INTO DBF (gcWorkDir+lcProVal)
        INDEX ON cpro_code+cpro_value TAG (lcProVal)
      ENDIF
      =gfOpenFile(gcDataDir+'ProFile',gcDataDir+'ProFile','SH')
      IF (laScrMode[3] .OR. laScrMode[4]) .AND. !USED(lcProFile)
        SELECT * FROM Profile ;
        WHERE cContType+cCont_Id+Store+cPro_Code = ;
        'C'+PADR(laData[2],8)+laData[3] INTO DBF (gcWorkDir+lcProFile)
        INDEX ON cContType+cCont_Id+Store+cPro_Code+cPro_Value TAG (lcProFile)
      ENDIF
    ENDIF  
    =lfBrowCont('R')
    SHOW GET ibProfile ENABLE
    IF !laScrMode[1] .AND. !EMPTY(cPro_Code)
      SHOW GET pbPModify ENABLE
    ENDIF
    IF (laScrMode[3] .OR. laScrMode[4]) .AND. !EMPTY(cPro_Code)
      SHOW GET pbPRemove ENABLE
    ENDIF
    IF laScrMode[3] .OR. laScrMode[4]
      SHOW GET pbPNew ENABLE
    ENDIF
  CASE lnActFolder = 6
    IF laReBrowse[3]
      =gfOpenFile(gcSysHome+'syschdul',gcSysHome+'SchDate','SH')
    ENDIF
    =lfBrowCont('P')
    SHOW GET ibPending ENABLE
    IF !laScrMode[1] .AND. !laScrMode[4]
      IF llAddRec
        SHOW GET pbNNew ENABLE
      ENDIF
      IF !EMPTY(cSeqNumber)
        SHOW GET pbNModify ENABLE
      ENDIF
      IF !EMPTY(cSeqNumber) .AND. llDeleRec
        SHOW GET pbNRemove ENABLE
      ENDIF
    ENDIF
  CASE lnActFolder = 7
    IF laReBrowse[4]
      =gfOpenFile(gcSysHome+'syschdul',gcSysHome+'SchDate','SH')
      IF !USED(lcHistFile)
        USE gcSysHome+'syschdul' AGAIN IN 0 ALIAS (lcHistFile)
      ENDIF
    ENDIF
    =lfBrowCont('H')
    SHOW GET ibHistory ENABLE
    IF !laScrMode[1] .AND. !EMPTY(cSeqNumber)
      SHOW GET pbHModify ENABLE
    ENDIF
    IF !laScrMode[1] .AND. !EMPTY(cSeqNumber) .AND. llDeleRec
      SHOW GET pbHRemove ENABLE
    ENDIF
  CASE lnActFolder = 8
    IF laData[1]= 'S' .AND. (laScrMode[3] .OR. laScrMode[4])
      SHOW GETS WINDOW (lcwinch8) DISABLE ONLY
      SHOW GET lnAShipVia ENABLE
      SHOW GET laData[63] ENABLE
      SHOW GET laData[66] ENABLE
      SHOW GET laData[64] ENABLE
      SHOW GET ibDistCtr  ENABLE
      SHOW GET laData[74] enable
      *E301581,1 AME [start]  
      SHOW GET laData[78] ENABLE
      *E301581,1 AME [End]
    ELSE
      SHOW GETS WINDOW (lcwinch8) &lcStatus ONLY
      SHOW GET ibDistCtr  DISABLE
      SHOW GET laData[74] DISABLE
      *E301581,1 AME [start]      
      SHOW GET laData[78] DISABLE
      *E301581,1 AME [End]
    ENDIF
    SHOW GET ibBgDumi7 ENABLE
ENDCASE
RETURN

*!*************************************************************
*! Name      : lpShow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Show function
*!*************************************************************
*! Calls     : lfReStart,gfwCodePop,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpShow()
*!*************************************************************
FUNCTION lpShow

SHOW GET ibStore   DISABLE
SHOW GET laData[3] DISABLE
lcBrFields = lcBaseFild
lcFile_Ttl = lcBaseTit
lnLastFold = lnActFolder
STORE .T. TO laReBrowse
lnActFolder= IIF(laScrMode[1] .OR. laScrMode[4],1,lnActFolder)
=lfChngFolder(lnActFolder)

*B038431,1 NNA 09/30/2004 (Begin) disable the Menu Icons if the Customer filed is Empty
IF EMPTY(laData[2])
  SHOW GET pbNotes   DISABLE
  SHOW GET pbObjLink DISABLE
  SHOW GET pbCusInv  DISABLE
  SHOW GET pbCusRa   DISABLE
  SHOW GET pbCusCr   DISABLE
  SHOW GET pbCusOrd  DISABLE
  SHOW GET pbCusFin  DISABLE
  SHOW GET pbCusSty  DISABLE
ENDIF
*B038431,1 NNA (End)

DO CASE
  CASE laScrMode[1] .AND. lnLastMode <> 1
    lnLastMode = 1
    =gfwCodePop(@laCodes,'CLASS','N')
    =gfwCodePop(@laCodes,'CTERMCODE','N')
    =gfwCodePop(@laCodes,'CDIVISION','N')
    =gfwCodePop(@laCodes,'REGION','N')
    =gfwCodePop(@laCodes,'SHIPVIA','N')
    =gfwCodePop(@laCodes,'SPCINST','N')
    SHOW GET ibName    ENABLE
    SHOW GET laData[4] ENABLE
    SHOW GET ibPhone   ENABLE
    SHOW GET laData[5] ENABLE
    STORE .F. TO llAddAcc
    SHOW GET pbNotes   DISABLE
    SHOW GET pbObjLink DISABLE
    SHOW GET pbCusInv  DISABLE
    SHOW GET pbCusRa   DISABLE
    SHOW GET pbCusCr   DISABLE
    SHOW GET pbCusOrd  DISABLE
    SHOW GET pbCusFin  DISABLE
    SHOW GET pbCusSty  DISABLE

    *E301196,1 ARH Defaulted the consolidated setting to YES if the country is England
    *E300878,1 Default new accounts to use consolidated invoices
    *STORE 2 TO lnConsol
    *STORE 1 TO lnPricelvl,lnCtaxrule,lnStatus,lnInsur,lnMstatm,lnBackOrd,lnConsol
    
    *E302089,1 ABD - The default customer status should be based on setup. [Begin]
    *STORE 1 TO lnPricelvl,lnCtaxrule,lnStatus,lnInsur,lnMstatm,lnBackOrd
    STORE 1 TO lnPricelvl,lnCtaxrule,lnInsur,lnMstatm,lnBackOrd
    lnStatus  = AT(lcDefaStat,'PAHX')
    *E302089,1 ABD - [End]
    
    lnConsol = IIF(gcContCode = 'ENG',1,2)
    *E300878,1 (End)
    *E301196,1 ARH (End)
    *C200339,1 TMI [START] Close the file (lcDivSlsRp)
    IF ASCAN(laEvntTrig , PADR('SALESREP',10)) <> 0
      IF TYPE('lcDivSlsRp')='C' AND USED(lcDivSlsRp)
        USE IN (lcDivSlsRp)
      ENDIF
    ENDIF     
    *C200339,1 TMI [END  ] Close the file (lcDivSlsRp)

  CASE laScrMode[2]
  
    SELECT CUSTOMER
    lnLastMode = 2
    SET KEY TO 'M'
    =SEEK('M'+laData[2])
    SKIP
    IF EOF()
      SHOW GET pbNxt DISABLE
      SHOW GET pbBtm DISABLE
    ENDIF
    SELECT Customer
    =SEEK('M'+laData[2])
    SKIP -1
    IF BOF()
      SHOW GET pbTop  DISABLE
      SHOW GET pbPrvs DISABLE
    ENDIF
    SELECT Customer
    SET KEY TO
    =SEEK('M'+laData[2])
    IF laData[1]='S'
      laData[24] = Customer.DUNSRTG
      laData[25] = Customer.CRLIMIT
      laData[26] = Customer.DISC
      laData[27] = Customer.DUNS
      laData[28] = Customer.EXPIRES
      laData[30] = Customer.cFacCode
      laData[31] = Customer.FACTACCT
      laData[33] = Customer.CLASS
      laData[34] = Customer.cTermCode
      laData[35] = Customer.cDivision
      laData[36] = Customer.PRIORITY
      laData[37] = Customer.PRICELVL
      laData[38] = Customer.CCURRCODE
      laData[39] = Customer.REGION
      laData[41] = Customer.SPCINST
      laData[61] = Customer.LINK_CODE
      laData[65] = Customer.UPS_INCR
      laData[67] = Customer.CONSOL
      laData[68] = Customer.CINSUR
      laData[69] = Customer.PRNT_STATM
      laData[73] = Customer.cSlsGlLink
      laData[76] = Customer.CCUSVEND
      laData[77] = Customer.SkuTmpl
      *B801928,1 Default store Back Order setting to customer's.
      laData[75] = Customer.CBACKORD
    ENDIF
    lcType  = laData[1]
    lcStore = laData[3]
    laData[1] = 'M'
    laData[3] = SPACE(8)
    =gfwCodePop(@laCodes,'CLASS','T')
    =gfwCodePop(@laCodes,'CTERMCODE','T')
    =gfwCodePop(@laCodes,'CDIVISION','T')
    =gfwCodePop(@laCodes,'REGION','T')
    =gfwCodePop(@laCodes,'SPCINST','T')
    laData[1] = lcType
    laData[3] = lcStore
    =SEEK(laData[1]+laData[2]+laData[3],'Customer')
    *E500368,1 AMH Set the default Status of new customer to be Potential [Start]
    *lnStatus  = AT(laData[6],'AHPX')
    lnStatus  = AT(laData[6],'PAHX')
    *E500368,1 AMH Set the default Status of new customer to be Potential [End  ]

    *E300834,1 Add new price level 'At Quantity'
    *lnPricelvl = IIF(laData[37]='A',1,IIF(laData[37]='B',2,3))
    lnPricelvl = IIF(laData[37]='A',1,IIF(laData[37]='B',2,IIF(laData[37]='C',3,4)))
    *E300834,1 (End)
        
    lnCtaxrule = IIF(laData[14]=' 2',2,1)
    lnConsol   = IIF(laData[67]='Y',1,2)
    lnInsur    = IIF(laData[68]='Y',1,2)
    lnMstatm   = IIF(laData[69]='Y',1,2)
    lnBillto   = IIF(laData[50]='M',1,IIF(laData[50]='S',2,3))
    lnBackOrd  = AT(laData[75],' ANI')
    IF laData[1] = 'S'
      SHOW GET pbcopy  ENABLE
    ENDIF
    SHOW GET pbNotes   ENABLE
    SHOW GET pbObjLink ENABLE
    SHOW GET pbCusInv  ENABLE
    SHOW GET pbCusRa   ENABLE
    SHOW GET pbCusCr   ENABLE
    SHOW GET pbCusOrd  ENABLE
    SHOW GET pbCusFin  ENABLE
    SHOW GET pbCusSty  ENABLE
    =gfwCodePop(@laCodes,'SHIPVIA','T')
    lnAShipVia = (ASCAN(laAShipVia,laData[62])+1)/2
    =SEEK(laData[23],'SycInt')
    lcAddHed1  = SycInt.cPart1Lab
    =SEEK(laData[70],'SycInt')
    lcAddHed2  = SycInt.cPart1Lab
    *C200339,1 TMI [START] Close the file (lcDivSlsRp)
    IF ASCAN(laEvntTrig , PADR('SALESREP',10)) <> 0 
      IF TYPE('lcDivSlsRp')='C' AND USED(lcDivSlsRp)
        USE IN (lcDivSlsRp)
      ENDIF
    ENDIF     
    *C200339,1 TMI [END  ] Close the file (lcDivSlsRp)

  CASE laScrMode[3] .AND. lnLastMode <> 3
    SELECT Customer
    =SEEK('M'+laData[2])
    IF laData[1]='S'
      laData[24] = Customer.DUNSRTG
      laData[25] = Customer.CRLIMIT
      laData[26] = Customer.DISC
      laData[27] = Customer.DUNS
      laData[28] = Customer.EXPIRES
      laData[30] = Customer.cFacCode
      laData[31] = Customer.FACTACCT
      laData[33] = Customer.CLASS
      laData[34] = Customer.cTermCode
      laData[35] = Customer.cDivision
      laData[36] = Customer.PRIORITY
      laData[37] = Customer.PRICELVL
      laData[38] = Customer.CCURRCODE
      laData[39] = Customer.REGION
      laData[41] = Customer.SPCINST
      laData[61] = Customer.LINK_CODE
      laData[65] = Customer.UPS_INCR
      laData[67] = Customer.CONSOL
      laData[68] = Customer.CINSUR
      laData[69] = Customer.PRNT_STATM
      laData[73] = Customer.cSlsGlLink
      laData[77] = Customer.SkuTmpl
    ENDIF
    =SEEK(laData[1]+laData[2]+laData[3],'Customer')
    lnLastMode = 3
    SHOW GET laData[4] ENABLE
    SHOW GET laData[5] ENABLE
    SHOW GET pbcopy    DISABLE
    SHOW GET pbNotes   ENABLE
    SHOW GET pbObjLink DISABLE
    SHOW GET pbCusInv  DISABLE
    SHOW GET pbCusRa   DISABLE
    SHOW GET pbCusCr   DISABLE
    SHOW GET pbCusOrd  DISABLE
    SHOW GET pbCusFin  DISABLE
    SHOW GET pbCusSty  DISABLE
    SHOW GET pbcopy    DISABLE
    IF lnBillto <> 3
      SHOW GET pbFromSt DISABLE
      *B603593,1 MHM 10/02/2000 [Start]
      SHOW GET laData[43] DISABLE
      SHOW GET laData[44] DISABLE
      SHOW GET laData[45] DISABLE
      SHOW GET laData[46] DISABLE
      SHOW GET laData[47] DISABLE
      SHOW GET laData[48] DISABLE
      SHOW GET laData[49] DISABLE
      SHOW GET ibAddress2 DISABLE
      SHOW GET lcAddHed2  DISABLE
      *B603593,1 MHM 10/02/2000 [End]
    ENDIF
    _CUROBJ = OBJNUM(laData[4])
  CASE laScrMode[4] .AND.   lnLastMode <>4
    lnLastMode = 4
    SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[1]
    laData[38] = gcBaseCurr
    SHOW GET laData[4] ENABLE
    SHOW GET laData[5] ENABLE
    *B801805,1 Reham On 11/15/98   *** Begin ***
    *B801805,1 Disable the notes button in the add mode.
    *SHOW GET pbNotes   ENABLE
    SHOW GET pbNotes   DISABLE
    *B801805,1 Reham On 11/15/98   *** End   ***
    SHOW GET pbObjLink DISABLE
    SHOW GET pbCusInv  DISABLE
    SHOW GET pbCusRa   DISABLE
    SHOW GET pbCusCr   DISABLE
    SHOW GET pbCusOrd  DISABLE
    SHOW GET pbCusFin  DISABLE
    SHOW GET pbCusSty  DISABLE
    
    IF laData[1]='M'
      =gfwCodePop(@laCodes,'CLASS','D')
      =gfwCodePop(@laCodes,'CTERMCODE','D')
      =gfwCodePop(@laCodes,'CDIVISION','N')
      =gfwCodePop(@laCodes,'REGION','D')
      =gfwCodePop(@laCodes,'SHIPVIA','D')
      =gfwCodePop(@laCodes,'SPCINST','D')
      laData[33] = SUBSTR(laClass[lnClass,2],1,6)
      laData[34] = SUBSTR(laTerms[lnTerms,2],1,6)
      laData[35] = SUBSTR(laDivision[lnDivision,2],1,6)
      laData[39] = SUBSTR(laRegion[lnRegion,2],1,6)
      laData[40] = SUBSTR(laShipVia[lnShipVia,2],1,6)
      laData[41] = SUBSTR(laSpcInst[lnSpcInst,2],1,6)
      laData[23] = gcContCode
      laData[70] = gcContCode
      
      *E301249,1 Make priority field C(3) [start]
      *laData[36] = '5'
      *B603253,1 RAMY Change this line to replace the array element with 
      *B603253,1 RAMY the value got from the setups [start]
      *laData[36] = '  5'
      laData[36] = lcDefaPrior
      *B603253,1 RAMY [end]
      *E301249,1 [end]
      
      laData[14] = ' 1'
      laData[61] = 'DEFDEF'
      laData[73] = 'DEF'
      laData[77] = 'DEF'
      *E500368,1 AMH Set the default Status of new customer to be Potential [Start]
      *STORE 'A' TO laData[6],laData[37]
      STORE 'A' TO laData[37]

      *E302089,1 ABD - The default customer status should be based on setup. [Begin]
      *STORE 'P' TO laData[6]
      STORE lcDefaStat TO laData[6]
      *E302089,1 ABD - [End]
      
      *E500368,1 AMH Set the default Status of new customer to be Potential [End  ]

      *E301196,1 ARH Defaulted the consolidated setting to YES if the country is England
      *E300878,1 Default new accounts to use consolidated invoices
      *STORE 'N' TO laData[67]
      *STORE 'Y' TO laData[67]
      laData[67] = IIF(gcContCode = 'ENG','Y','N')
      *E300878,1 (End)
      *E301196,1 ARH (End)
      
      STORE 'Y' TO laData[68],laData[69]      
      STORE 'M' TO laData[50]
    ELSE
      SHOW GET pbcopy   DISABLE
      SHOW GET pbFromSt DISABLE
      *B603593,1 MHM 10/02/2000 [start]
      SHOW GET laData[43] DISABLE
      SHOW GET laData[44] DISABLE
      SHOW GET laData[45] DISABLE
      SHOW GET laData[46] DISABLE
      SHOW GET laData[47] DISABLE
      SHOW GET laData[48] DISABLE
      SHOW GET laData[49] DISABLE
      SHOW GET ibAddress2 DISABLE
      SHOW GET lcAddHed2  DISABLE
      *B603593,1 MHM 10/02/2000 [End]
      lnBillto = 1
      lcStore = laData[3]
      =SEEK('M'+laData[2],'Customer')
      SELECT CUSTOMER
      laData[1] = 'M'
      laData[3] = SPACE(8)
      
      =gfwCodePop(@laCodes,'CLASS','T')
      =gfwCodePop(@laCodes,'CTERMCODE','T')
      =gfwCodePop(@laCodes,'CDIVISION','T')
      =gfwCodePop(@laCodes,'REGION','T')
      =gfwCodePop(@laCodes,'SHIPVIA','T')
      =gfwCodePop(@laCodes,'SPCINST','T')
        
      laData[1]  = 'S'
      laData[3]  = lcStore
      laData[4]  = Customer.STNAME
      laData[24] = Customer.DUNSRTG
      laData[25] = Customer.CRLIMIT
      laData[26] = Customer.DISC
      laData[27] = Customer.DUNS
      laData[28] = Customer.EXPIRES
      laData[30] = Customer.cFacCode
      laData[31] = Customer.FACTACCT
      laData[33] = Customer.CLASS
      laData[34] = Customer.cTermCode
      laData[35] = Customer.cDivision
      laData[36] = Customer.PRIORITY
      laData[37] = Customer.PRICELVL
      laData[38] = Customer.CCURRCODE
      laData[39] = Customer.REGION
      laData[40] = Customer.SHIPVIA
      laData[41] = Customer.SPCINST
      laData[61] = Customer.LINK_CODE
      laData[65] = Customer.UPS_INCR
      laData[67] = Customer.CONSOL
      laData[68] = Customer.CINSUR
      laData[69] = Customer.PRNT_STATM
      laData[73] = Customer.cSlsGlLink
      laData[77] = Customer.SkuTmpl
      laData[19] = Customer.SALESREP
      laData[20] = Customer.COMM
      laData[21] = Customer.REP2
      laData[22] = Customer.COMM2
      laData[23] = Customer.CCONT_CODE
      laData[43] = Customer.BTNAME
      laData[44] = Customer.CADDRESS12
      laData[45] = Customer.CADDRESS22
      laData[46] = Customer.CADDRESS32
      laData[47] = Customer.CADDRESS42
      laData[48] = Customer.CADDRESS52
      laData[49] = Customer.CADDRESS62
      laData[50] = 'M'
      *E500368,1 AMH Set the default Status of new customer to be Potential [Start]
      *laData[6]  = 'A'
      
      *E302089,1 ABD - The default customer status should be based on setup. [Begin]
      *laData[6]  = 'P'
      STORE lcDefaStat TO laData[6]
      *E302089,1 ABD - [End]
      
      *E500368,1 AMH Set the default Status of new customer to be Potential [End  ]
      laData[70] = Customer.CCONT_COD2
      laData[13] = Customer.NTAXRATE
      laData[14] = Customer.CTAXRULE
      laData[76] = Customer.CCUSVEND
      *B801928,1 Default store Back Order setting to customer's.
      laData[75] = Customer.CBACKORD
      lnBackOrd  = AT(laData[75],' ANI')

      _CUROBJ = OBJNUM(laData[4])
    ENDIF
    *C123847,1  TMI [Start] Add a trigger for DIR03 to set the field CUSTOMER.LCMPCSTPO to Yes as default
    IF ASCAN(laEvntTrig , PADR('CMPCSTPO',10)) <> 0
      =gfDoTriger('ARCUST',PADR('CMPCSTPO',10))
    ENDIF     
    *C123847,1  TMI [End  ]     
   
    STORE SPACE(6) TO laData[62]
    STORE 1 TO lnAShipVia

    *E300834,1 Add new price level 'At Quantity'
    *lnPricelvl = IIF(laData[37]='A',1,IIF(laData[37]='B',2,3))
    lnPricelvl = IIF(laData[37]='A',1,IIF(laData[37]='B',2,IIF(laData[37]='C',3,4)))
    *E300834,1 (End)
        
    lnCtaxrule = IIF(laData[14]=' 2',2,1)
    lnConsol   = IIF(laData[67]='Y',1,2)
    lnInsur    = IIF(laData[68]='Y',1,2)
    lnMstatm   = IIF(laData[69]='Y',1,2)
    lnBillto   = IIF(laData[50]='M',1,IIF(laData[50]='S',2,3))
    =SEEK(laData[23],'SycInt')
    STORE SycInt.cPart1Lab  TO lcAddHed1,lcAddHed2
    STORE SycInt.cCont_Desc TO laData[12],laData[49]
    
  	*C200076,1 if company does not have User fields then don't show it [Begin.]
	IF !EMPTY(laUsrFields[1,1])
      =gfDoTriger('ARCUST','INI_VAR')
    ENDIF
    *C200076,1 if company does not have User fields then don't show it [End.]


ENDCASE  

*C123851,1  TMI [Start] empty the temp altship file when the screen mode is changed
IF ASCAN(laEvntTrig,'ZPALTSHP')<>0
  =gfDoTriger('ARCUST','ZPALTSHP')
ENDIF
*C123851,1  TMI [End  ] 

*!*************************************************************
*! Name      : lfActPad
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : lpvInquiry
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActPad()
*!*************************************************************
FUNCTION lfActPad

DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[1]
ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP
DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR  1 OF _INQURYPOP PROMPT '\<Notes'      SKIP FOR laScrMode[4] .AND. laData[1]='M'
DEFINE BAR  2 OF _INQURYPOP PROMPT '\<Invoices'   SKIP FOR laScrMode[1] .OR. laScrMode[4] OR !('AR' $ gcCmpModules)
DEFINE BAR  3 OF _INQURYPOP PROMPT '\<Return Authorizations'   SKIP FOR laScrMode[1] .OR. laScrMode[4] OR !('RM' $ gcCmpModules)
DEFINE BAR  4 OF _INQURYPOP PROMPT 'R\<eturns'    SKIP FOR laScrMode[1] .OR. laScrMode[4] OR !('RM' $ gcCmpModules)
DEFINE BAR  5 OF _INQURYPOP PROMPT '\<Orders'     SKIP FOR laScrMode[1] .OR. laScrMode[4] OR !('SO' $ gcCmpModules)
DEFINE BAR  6 OF _INQURYPOP PROMPT '\<Financial'  SKIP FOR laScrMode[1] .OR. laScrMode[4]
DEFINE BAR  7 OF _INQURYPOP PROMPT '\<Styles'     SKIP FOR laScrMode[1] .OR. laScrMode[4] OR !('AR' $ gcCmpModules)

*C101427,1 Remove '\<Post Dated Checks' bar [BEGIN]
*DEFINE BAR  8 OF _INQURYPOP PROMPT '\<Post Dated Checks' SKIP FOR laScrMode[1] .OR. laScrMode[4]
*DEFINE BAR  9 OF _INQURYPOP PROMPT '\<Departments'
*DEFINE BAR 10 OF _INQURYPOP PROMPT '\<History'   SKIP FOR laScrMode[1] .OR. laScrMode[4]
*DEFINE BAR 11 OF _INQURYPOP PROMPT '\<Locations' SKIP FOR !laScrMode[2]
*DEFINE BAR 12 OF _INQURYPOP PROMPT '\<Graph'     SKIP FOR laScrMode[1] .OR. laScrMode[4]
*DEFINE BAR 13 OF _INQURYPOP PROMPT 'Ob\<ject Link'  SKIP FOR laScrMode[1] .OR. laScrMode[4]
DEFINE BAR  8 OF _INQURYPOP PROMPT '\<Departments'

*B603018,1 BWA 07/06/1999 Let the user access to see the cost from customer history or no.
*DEFINE BAR  9 OF _INQURYPOP PROMPT '\<History'   SKIP FOR laScrMode[1] .OR. laScrMode[4]  

*B130389,1 EIH 01/07/2006 Fix bug that when the user does not have costing information privilege he can login to the history 
*B130389,1 EIH 			  Screen but we delete the columns "COGS,PROFIT,NET PROFIT" from the screen [Begin]
*DEFINE BAR  9 OF _INQURYPOP PROMPT '\<History'   SKIP FOR laScrMode[1] .OR. laScrMode[4]  .OR. !LLCOSTPRV
DEFINE BAR  9 OF _INQURYPOP PROMPT '\<History'   SKIP FOR laScrMode[1] .OR. laScrMode[4]  
*B130389,1 EIH [End]


*B603018,1 BWA [END]

DEFINE BAR 10 OF _INQURYPOP PROMPT '\<Locations' SKIP FOR !laScrMode[2]
DEFINE BAR 11 OF _INQURYPOP PROMPT '\<Graph'     SKIP FOR laScrMode[1] .OR. laScrMode[4]
DEFINE BAR 12 OF _INQURYPOP PROMPT 'Ob\<ject Link'  SKIP FOR laScrMode[1] .OR. laScrMode[4]
*C101427,1 Remove '\<Post Dated Checks' bar [END..] 

*E301246,1 Add 'S\<ummary Information' bar [Begin.]
DEFINE BAR 13 OF _INQURYPOP PROMPT 'S\<ummary Information' SKIP FOR laScrMode[1] .OR. laScrMode[4]
*E301246,1 Add 'S\<ummary Information' bar [End.]

*E301119,1 MAB 10/30/1999 Add salutation screen to menu options. [Begin]
DEFINE BAR 14 OF _INQURYPOP PROMPT 'S\<alutations' SKIP FOR laScrMode[4]
*E301119,1 MAB 10/30/1999 Add salutation screen to menu options. [Begin]
*C200339,1 TMI [START] Define Option to open SODIVSRP SCREEN
IF ASCAN(laEvntTrig , PADR('SALESREP',10)) <> 0
  DEFINE BAR 15 OF _INQURYPOP PROMPT 'Enter Sales Rep by Di\<vision' SKIP FOR laScrMode[1] OR EMPTY(laData[35])
ENDIF     
*C200339,1 TMI [END  ] Define Option to open SODIVSRP SCREEN

*C123851,1  TMI [Start] Add an option for DIR03 titled "Add Alternate Shipping Addresses"
IF ASCAN(laEvntTrig , PADR('ADOPDIR',10)) <> 0
  =gfDoTriger('ARCUST',PADR('ADOPDIR',10))
ENDIF     
*C123851,1  TMI [End  ] 
 

ON SELECTION POPUP _INQURYPOP DO lpvInquiry
IF llFinancial
  =lfCallInv(lfFinancial(llMulCurr,laSetups(4,2),laData(2),laData(4),laData(3)))
  llFinancial=.F.
ENDIF



*!*************************************************************
*! Name      : lpvInquiry
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpvInquiry()
*!*************************************************************
FUNCTION lpvInquiry

DO CASE
  CASE BAR() = 1      && Notes
    =lfvCusNote()
  CASE BAR() = 2      && Invoice
    =lfInvoices()
  CASE BAR() = 3      && R\A
    =lfCusRa()
  CASE BAR() = 4      && Returns
    =lfCrMemos()
  CASE BAR() = 5      && Orders
    =lfOrders()
  CASE BAR() = 6      && Financial
    =lfCallInv(lfFinancial(llMulCurr,laSetups[4,2],laData[2],laData[4],laData[3]))
  CASE BAR() = 7      && Style
    =lfStyles()

  *C101427,1 Remove '\<Post Dated Checks' functionality because [BEGIN] 
  *          it's now in fininicial screen.        
  *CASE BAR() = 8      && Post Dated Checks
  *  =lfvPosted()
  *CASE BAR() = 9     && Departments
  *  =lfDepartments()
  *CASE BAR() = 10     && History
  *  =lfvCusHst()
  *CASE BAR() = 11     && Locations
  *  =lfvLocations()
  *CASE BAR() = 12     && History
  *  =lfvCusHst('G')
  *CASE BAR() = 13     && Object Link
  *  =lfvObjLink()
  
  CASE BAR() = 8     && Departments
    =lfDepartments()
  CASE BAR() = 9     && History
    =lfvCusHst()
  CASE BAR() = 10     && Locations
    =lfvLocations()
  CASE BAR() = 11     && History
    =lfvCusHst('G')
  CASE BAR() = 12     && Object Link
    =lfvObjLink()
  *C101427,1 Remove '\<Post Dated Checks' functionality because [End  ] 

*E301246,1 Add 'S\<ummary Information' functionality [Begin.]
  CASE BAR() = 13
    =lfvSumInf() 
*E301246,1 Add 'S\<ummary Information' functionality [End.]

  *E301119,1 MAB 10/30/1999 Add salutation screen to menu options. [Begin]
  CASE BAR() = 14
    DO (gcAppHome+"ARSALUT.FXP") WITH "CUSTOMER"
  *E301119,1 MAB 10/30/1999 Add salutation screen to menu options. [Begin]

  *C200339,1 TMI [START] Open the screen SODIVSRP 
  CASE BAR() = 15  
  IF ASCAN(laEvntTrig , PADR('SALESREP',10)) <> 0
    =gfDoTriger('ARCUST',PADR('SALESREP',10))
  ENDIF     
  *C200339,1 TMI [END  ] Open the screen SODIVSRP 

ENDCASE

*!*************************************************************
*! Name      : lfvLocations
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse customer locations
*!*************************************************************
*! Calls     : CUSBROWS
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvLocations()
*!*************************************************************
FUNCTION lfvLocations

PRIVATE xStore
xStore = SPACE(8)
IF CUSBROWS(laData[2],.T.)
  laData[3] = xStore
  SHOW GET laData[3] ENABLE
  _CUROBJ = OBJNUM(laData[3])
  KEYBOARD "{ENTER}"
  RETURN
ENDIF  

*!*************************************************************
*! Name      : lfvState
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate USA/CANADA State
*!*************************************************************
*! Calls     : AriaBrow
*!*************************************************************
*! Passed Parameters  : Main Account State / Store State
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvState()
*!*************************************************************
FUNCTION lfvState
PARAMETERS lcState
PRIVATE lnAlias,lcKey

*B802172,1 Reham On 05/16/1999   *** Begin ***
*B802172,1 Execute the lfvstate function only if press enter.
*B602885,1 ABD Valid only when state is not empty. [Begin]
IF !EMPTY(ALLTRIM(EVAL(lcState)))
  *B602885,1 ABD   [End]
  IF LASTKEY() = 13
    *B802172,1 Reham On 05/16/1999   *** End   ***
    IF (EMPTY(EVAL(SYS(18))) .OR. EVAL(SYS(18)) <> lcOldValue) .AND. ;
       (SYS(18)='LADATA(10)' .AND. INLIST(UPPER(ALLTRIM(laData[23])),'USA','CANADA')) .OR. ;
       (SYS(18)='LADATA(47)' .AND. INLIST(UPPER(ALLTRIM(laData[70])),'USA','CANADA'))
      lnAlias = SELECT()
      SELECT CODES
      SET ORDER TO TAG Idrltfname
      =SEEK('N'+'N'+'STATE')
      LOCATE REST WHILE cDefCode+cRltField+cFld_Name = 'N'+'N'+'STATE' ;
      FOR SUBSTR(cCode_No,1,3) = SUBSTR(&lcState,1,3)
      IF !FOUND()
        SELECT CODES
        =SEEK('N'+'N'+'STATE')
        lcBrowTitl = PROP(LOOKUP(SYCINT.CPARt4LAB,laData[23],sycint.ccont_code,'CCONTCODE'))
        lcBrFields = "cCode_No  :R :H=lcBrowTitl, "+;
                     "cDiscrep  :R :H='Name'"
        lcKey = 'N'+'N'+'STATE'
        &lcState = IIF(AriaBrow("lcKey",lcBrowTitl,gnBrHSRow1,.F., gnBrHSRow2, .F., '',;
                       '','cCode_No','laBrowArr'),Codes.cCode_No,lcOldValue)
      ENDIF
      IF laSetups[2,2]='Y'
        =SEEK('N'+'Y'+'STATE')
        LOCATE REST WHILE cDefCode+cRltField+cFld_Name = 'N'+'Y'+'STATE' ;
        FOR SUBSTR(cCode_No,1,3) = SUBSTR(&lcState,1,3) .AND. cRltd_Nam='NTAXRATE'
        laData[13] = VAL(codes.cRltd_Vlu)
        SHOW GET laData[13]
      ENDIF
      IF laSetups[2,2]='Y' .AND. llIsCanada
        =SEEK('N'+'Y'+'STATE')
        LOCATE REST WHILE cDefCode+cRltField+cFld_Name = 'N'+'Y'+'STATE' ;
        FOR SUBSTR(cCode_No,1,3) = SUBSTR(&lcState,1,3) .AND. cRltd_Nam='CTAXRULE'
        laData[14] = codes.cRltd_Vlu
        lnCtaxrule = INT(VAL(ALLTRIM(laData[14])))
        SHOW GET lnCtaxrule
      ENDIF
      SELECT (lnAlias)
    ENDIF
  *B802172,1 Reham On 05/16/1999   *** Begin ***
  ENDIF
  *B602885,1 ABD End for if statment. [Begin]
ENDIF
*B602885,1 ABD [End]
*B802172,1 Reham On 05/16/1999   *** End   ***
RETURN

*!*************************************************************
*! Name      : lfvPosted
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse customer posted dated checks
*!*************************************************************
*! Calls     : AriaBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfvPosted()
*!*************************************************************
*Modification C101427,1
*
FUNCTION lfvPosted
PRIVATE lnAlias,lcBrFields,lcFile_Ttl,lcFile

*C101427,1 We need to open PostDChq file in lffinancial which [BEGIN]
*          call this function
*E301077,49 Inhance openning files to speed up transaction
*=gfOpenFile(gcDataDir+'PostDChq','PostDChq','SH')
*E301077,49 (End)
*C101427,1 We need to open PostDChq fine in lffinancial which [END  ]

lnAlias = SELECT()
SELECT PostDChq

*C101427,1 laData[2] not defined in this function lcAccount is. [BEGIN]
*          change \<Ok to \<Close since we can't select any thing, 
*          and also change laData[2] to lcAccount in AriaBrow calling.
*IF SEEK(laData[2])
IF SEEK(lcAccount,'PostDChq')


  lcBrowTitl = "Posted dated checks"
  *B128585,1 ASH 06/17/2005 (Begin) Add the comments fields to the post dated checks browse.
  *lcBrFields = "account  :R :H='Account', "+;
               "paydate  :R :H='Pay date', "+;
               "amount   :R :H='Amount' ,  "+;
               "Chequeno :R :H='Check #'"
  
  lcBrFields = "account  :R :H='Account', "+;
               "paydate  :R :H='Pay date', "+;
               "amount   :R :H='Amount' ,  "+;
               "Chequeno :R :H='Check #',  "+;
               "cComment1 :R :H='Comments1',"+;
               "cComment2 :R :H='Comments2'"
  *B128585,1 ASH 06/17/2005 (End)

  *=AriaBrow("laData[2]",lcBrowTitl,gnBrHSRow1,.F., gnBrHSRow2, .F., '',;
   'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok','Account','laBrowArr')

  =AriaBrow("lcAccount",lcBrowTitl,gnBrHSRow1,.F., gnBrHSRow2, .F., '',;
   'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Close','Account','laBrowArr')

*C101427,1 laData[2] not defined in this function lcAccount is [END  ]

*C101427,1 If No Posted dated checks for this account we can't call [BEGIN] 
*                 this function (the push button will be disable)   
*ELSE
  *E300455,1 Message : 40121
  *E300455,1 No Posted dated checks for this account. 
  *E300455,1 Button : 00000
  *E300455,1 Ok
*  =gfModalGen('TRM40121B00000','ALERT')
*C101427,1 If No Posted dated checks for this account we can't call [End  ] 
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfEditLabl
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Edit labels
*!*************************************************************
*! Calls     : ARCUSTF.SPX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfEditLabl()
*!*************************************************************
FUNCTION lfEditLabl
PRIVATE lcOldLabel
STORE '' TO lcOldLabel
DO (gcScrDir+"ARCUSTF.SPX")
SAVE TO (gcDataDir+'ARCUST.MEM') ALL LIKE lcField*
=lfRefresh(lcwinch3)

*!*************************************************************
*! Name      : lfwLabel
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Store label old value
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwLabel()
*!*************************************************************
FUNCTION lfwLabel
lcOldLabel = EVAL(SYS(18))

*!*************************************************************
*! Name      : lfvLabel
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate label value
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvLabel()
*!*************************************************************
FUNCTION lfvLabel
IF EMPTY(EVAL(SYS(18)))
  lcVar  = SYS(18)
  &lcVar = lcOldLabel
ENDIF

*!*************************************************************
*! Name      : lfBrowCont
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse customer Contacts,Profiles,Pending and History
*!*************************************************************
*! Calls     : lfShowBrow
*!*************************************************************
*! Passed Parameters  : lcType 'C' : Contact
*!                             'R' : Profile 
*!                             'P' : Pending
*!                             'H' : History
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrowCont()
*!*************************************************************
FUNCTION lfBrowCont
PARAMETERS lcType
*B803864,1 MHM 12/20/2000 initialize the array to refrish screen[start]
STORE .T. TO laReBrowse
*B803864,1 MHM 12/20/2000 [end]
IF lcType='C'
  SELECT IIF(laScrMode[1] .OR. laScrMode[2],'Contact',lcContFile)
  lnMarker = RECNO()
  IF laReBrowse[1]
    RELEASE WINDOW (lcContTitl)
    *B602166,1 Reham On 11/30/98   *** Begin ***
    *B602166,1 Add the title & salutaion fields to the contact browse.
    *BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,;
           Contact :H='Contact'  :R,;
           Phone   :P= gfPhoneTem() :H='Phone' :R,;
           Fax     :P= gfPhoneTem() :H='Fax' :R;
           WINDOW (lcwinch41)  ;
           IN WINDOW (lcwinch4);
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           WHEN lfShowBrow() ;
	       KEY 'C'+PADR(laData[2],8)+laData[3] ;
           TITLE lcContTitl
   *C101764,1 (Start) Comment this lines and rewrite them adding the Email_add Field 
   * BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,;
           Contact    :H='Contact'  :22:R,;
           cContTtl   :H='Title' :22:R,;
           Phone      :P= gfPhoneTem() :H='Phone' :R,;
           Fax        :P= gfPhoneTem() :H='Fax' :R,;
           cContSalut :H='Salutation' :R;
           WINDOW (lcwinch41)  ;
           IN WINDOW (lcwinch4);
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           WHEN lfShowBrow() ;
	       KEY 'C'+PADR(laData[2],8)+laData[3] ;
           TITLE lcContTitl
           
     BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,;
           Contact    :H='Contact'  :22:R,;
           cContTtl   :H='Title' :22:R,;
           Phone      :P= gfPhoneTem() :H='Phone' :R,;
           Fax        :P= gfPhoneTem() :H='Fax' :R,;
           cContSalut :H='Salutation' :R,;
           cEmail_Add :H='Email':R;
           WINDOW (lcwinch41)  ;
           IN WINDOW (lcwinch4);
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           WHEN lfShowBrow() ;
	       KEY 'C'+PADR(laData[2],8)+laData[3] ;
           TITLE lcContTitl      
    *C101764,1 (End)
    *B602166,1 Reham On 11/30/98   *** End   ***
    STORE .F. TO laReBrowse[1]
  ENDIF
ENDIF
IF lcType='R'
  SELECT IIF(laScrMode[1] .OR. laScrMode[2],'ProFile',lcProFile)
  lnPMarker = RECNO()
  IF laReBrowse[2]
    RELEASE WINDOW (lcProfTitl)
    =SEEK('C'+PADR(laData[2],8)+laData[3])
    BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnPMarker,'>',' '):H=' ':R:1:W=.F.,;
           lcDesc= gfCodDes(cPro_Code,'CPRO_CODE') :H='Profile' :R,;
           cPro_Value :R:H='Reference' ,;
           dPro_Date :H='Profile Date' :R;
           WINDOW (lcwinch51)  ;
           IN WINDOW (lcwinch5);
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           WHEN lfShowBrow() ;
    	   KEY 'C'+PADR(laData[2],8)+laData[3] ;
           TITLE lcProfTitl
    STORE .F. TO laReBrowse[2]
  ENDIF
ENDIF
IF lcType='P'
  SELECT syschdul
  SET ORDER TO TAG SchDate ASCENDING
  *B803388,1 AME [Start]
  GO TOP
  *B803388,1 AME [End]
  lnNMarker = RECNO()
  IF laReBrowse[3]
    RELEASE WINDOW (lcPendTitl)
    *B602697,1 Reham On 03/25/99   *** Begin ***
    *B602697,1 Add the contact field into the pending browse.
    *BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnNMarker,'>',' '):H=' ':R:1:W=.F.,;
           lcDesc=IIF(CTRANTYPE='C','Call',IIF(CTRANTYPE='A','Appointment','ToDo')) :11:R:H='Activity',;
           dTranDate :R :H='Date' ,;
           cTranTime :R :H='Time' ,;
           cUser_Id  :R :H='User' ,;
           lcReson = gfCodDes(CTRANRESON,'CTRANRESON') :R :H='Reason' :30 ,;
           CSUBJECT :R :H='Subject' ;
           WINDOW (lcwinch61)  ;
           IN WINDOW (lcwinch6);
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           WHEN lfShowBrow() ;
	       KEY 'C'+PADR(laData[2],8)+laData[3]+'N' ;
           TITLE lcPendTitl
    BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnNMarker,'>',' '):H=' ':R:1:W=.F.,;
           lcDesc=IIF(CTRANTYPE='C','Call',IIF(CTRANTYPE='A','Appointment','ToDo')) :11:R:H='Activity',;
           dTranDate :R :H='Date' ,;
           cTranTime :R :H='Time' ,;
           cUser_Id  :R :H='User' ,;
           Contact :R :H='Contact':20,;
           lcReson = gfCodDes(CTRANRESON,'CTRANRESON') :R :H='Reason' :20 ,;
           CSUBJECT :R :H='Subject':20;
           WINDOW (lcwinch61)  ;
           IN WINDOW (lcwinch6);
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           WHEN lfShowBrow() ;
	       KEY 'C'+PADR(laData[2],8)+laData[3]+'N' ;
           TITLE lcPendTitl
    *B602697,1 Reham On 03/25/99   *** End   ***
    STORE .F. TO laReBrowse[3]
  ENDIF
ENDIF
IF lcType='H'
  SELECT (lcHistFile)
  *B803388,1 AME [Start]
  *SET ORDER TO TAG SchDate DESCENDING   && replace tran date with complete date.

  *B803864,1 MHM 12/20/2000 set order to index in case of found [start]
  *IF !FILE(gcWorkDir+lcHistIndx+".CDX")  
  *  INDEX ON CCONTTYPE+CCONT_ID+STORE+CCOMPLETED+DTOS(DCMPLTDATE)+CTRANTIME+CSEQNUMBER  TAG  lcabdou OF (gcWorkDir)+lcHistIndx  DESCENDING
  *ENDIF
  IF !FILE(gcWorkDir+lcHistIndx+".CDX")  
    INDEX ON CCONTTYPE+CCONT_ID+STORE+CCOMPLETED+DTOS(DCMPLTDATE)+CTRANTIME+CSEQNUMBER  TAG  lcabdou OF (gcWorkDir)+lcHistIndx  DESCENDING
  ELSE 
    *B606178,1 TMI [Start] Assure index is open with tag 'LCABDOU' in history file    
    *SET ORDER TO 2
    IF TAG() # 'LCABDOU'
      SET INDEX TO (gcWorkDir+lcHistIndx+".CDX") &&tmi
      SET ORDER TO TAG  "lcabdou" OF (gcWorkDir+lcHistIndx+".CDX") 
    ENDIF
    *B606178,1 TMI [End  ] Assure index is open with tag 'LCABDOU' in history file 
  ENDIF
  *B803864,1 MHM 12/20/2000 [End]
  
  GO BOTTOM
  *B803388,1 AME [End]
  lnHMarker = RECNO()
  IF laReBrowse[4]
    RELEASE WINDOW (lcHistTitl)
    *E301270,1 Reham On 06/20/1999   *** Begin ***
    *E301270,1 Add the contact name in the history browse.
    *BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnHMarker,'>',' '):H=' ':R:1:W=.F.,;
           dCmpltDate :R:H='Date' ,;
           lcDesc=IIF(CTRANTYPE='C','Call',IIF(CTRANTYPE='A','Appointment','ToDo')) :R:H='Activity',;
           CCMPLTUSER :R :H='User',;
           lcReson = gfCodDes(CRESULTCD,'CRESULTCD') :R :H='Result' :30 ,;
           CSUBJECT :R :H='Subject' ;
           WINDOW (lcwinch71)  ;
           IN WINDOW (lcwinch7);
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           WHEN lfShowBrow() ;
	       KEY 'C'+PADR(laData[2],8)+laData[3]+'Y' ;
           TITLE lcHistTitl
    BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnHMarker,'>',' '):H=' ':R:1:W=.F.,;
           dCmpltDate :R:H='Date' ,;
           lcDesc=IIF(CTRANTYPE='C','Call',IIF(CTRANTYPE='A','Appointment','ToDo')) :R:H='Activity',;
           CCMPLTUSER :R :H='User',;
           Contact :R :H='Contact':20,;
           lcReson = gfCodDes(CRESULTCD,'CRESULTCD') :R :H='Result' :30 ,;
           CSUBJECT :R :H='Subject' ;
           WINDOW (lcwinch71)  ;
           IN WINDOW (lcwinch7);
           NOMENU            ;         
           NOAPPEND          ;
           NODELETE          ;         
           NOWAIT            ;
           SAVE              ;
           NOCLEAR           ;
           WHEN lfShowBrow() ;
	       KEY 'C'+PADR(laData[2],8)+laData[3]+'Y' ;
           TITLE lcHistTitl
    *E301270,1 Reham On 06/20/1999   *** End   ***
    STORE .F. TO laReBrowse[4]
  ENDIF
ENDIF
=lfShowBrow()

*!*************************************************************
*! Name      : lfShowBrow
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Refresh customer Contacts,Profiles,Pending and History Browse 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowBrow()
*!*************************************************************
FUNCTION lfShowBrow
DO CASE
  CASE lnActFolder = 4
    lnMarker = RECNO(IIF(laScrMode[1] .OR. laScrMode[2],'Contact',lcContFile))
    SHOW WINDOW (lcContTitl) REFRESH SAME
  CASE lnActFolder = 5
    lnPMarker = RECNO(IIF(laScrMode[1] .OR. laScrMode[2],'ProFile',lcProFile))
    SHOW WINDOW (lcProfTitl) REFRESH SAME
  CASE lnActFolder = 6
    lnNMarker = RECNO('syschdul')
    SHOW WINDOW (lcPendTitl) REFRESH SAME
  CASE lnActFolder = 7
    lnHMarker = RECNO(lcHistFile)
    SHOW WINDOW (lcHistTitl) REFRESH SAME
ENDCASE

*!*************************************************************
*! Name      : lfvAccount
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate customer code
*!*************************************************************
*! Calls     : gfModalGen,CUSBROWM,gfSeekRec
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAccount()
*!*************************************************************
FUNCTION lfvAccount
PRIVATE xAccount

IF !EMPTY(laData[2]) .AND. LEN(ALLTRIM(laData[2])) < 5 .AND. !('?' $ laData[2])
  *E300455,1 Message : 40057
  *E300455,1 Account code should not be less than 5 characters.
  *E300455,1 Button : 00000
  *E300455,1 Browse Reenter
  IF gfModalGen('TRM40057B40007','ALERT') = 2
    _CUROBJ  = OBJNUM(ladata[2])
    RETURN
  ENDIF
  llBrowse = .T.    
ENDIF
IF llBrowse .OR. '?' $ laData[2]
  xAccount = laData[2]
  DO CUSBROWM WITH xAccount
  laData[2] = xAccount
ENDIF
llBrowse = .F.
IF EMPTY(laData[2])
  SHOW GET ibStore   DISABLE
  SHOW GET laData[3] DISABLE
  RETURN
ENDIF
IF LEN(ALLTRIM(laData[2])) < 5
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
IF !SEEK('M'+laData[2],'Customer')
  IF llAddAcc
    STORE .F. TO laScrMode
    STORE .T. TO laScrMode[4]
    SHOW GETS
  ELSE
    DECLARE laKeyField[2,4]
    laData[1]='M'
    laKeyField[1,1] = 'laData[1]'
    laKeyField[1,2] = .F.
    laKeyField[1,3] = 'CUSTOMER'
    laKeyField[1,4] = 1
    laKeyField[2,1] = 'laData[2]'
    laKeyField[2,2] = .T.
    laKeyField[2,3] = 'CUSTOMER'
    laKeyField[2,4] = 2
    =gfSeekRec()
    IF laScrMode[1]
      _CUROBJ = OBJNUM(laData[2])
    ENDIF
  ENDIF
ELSE
  IF llAddAcc
    *E300455,1 Message : 40042
    *E300408,1 Account xxxxx already exists.
    *E300455,1 Button : 40006
    *E300455,1 View Reenter
    IF gfModalGen('QRM40042B40006','ALERT',ALLTRIM(laData[2])) = 2
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
  ENDIF
  SHOW GET ibStore   ENABLE
  SHOW GET laData[3] ENABLE
ENDIF
llAddAcc = .F.
*C128481,1 NNA 09/12/2005 (Begin) New Trigger for Pan21 (Tony) to make The Default value for the Lcharge Filed as True
IF ASCAN(laEvntTrig , PADR('USRFELDS',10)) <> 0
  =gfDoTriger('ARCUST',PADR('USRFELDS',10))
ENDIF
*C128481,1 NNA (End)

*!*************************************************************
*! Name      : lfvStore
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate customer store code
*!*************************************************************
*! Calls     : CUSBROWS,gfSeekRec
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStore()
*!*************************************************************
FUNCTION lfvStore
PRIVATE xStore,lcOldAcc
IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
IF llBrowse .OR. '?' $ laData[3]
  xStore   = laData[3]
  IF !CUSBROWS(laData[2],.T.)
    STORE SPACE(8) TO xStore
  ENDIF
  laData[3] = xStore
ENDIF  
llBrowse = .F.

DECLARE laKeyField[3,4]
laData[1]=IIF(EMPTY(laData[3]),'M','S')
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'CUSTOMER'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] = .F.
laKeyField[2,3] = 'CUSTOMER'
laKeyField[2,4] = 2
laKeyField[3,1] = 'laData[3]'
laKeyField[3,2] = .T.
laKeyField[3,3] = 'CUSTOMER'
laKeyField[3,4] = 3
lcOldAcc = laData[2]
SELECT CUSTOMER
=gfSeekRec()
IF laScrMode[1]
  laData[2] = lcOldAcc
  SHOW GET laData[2]
  _CUROBJ = OBJNUM(laData[3])
ENDIF

*!*************************************************************
*! Name      : lfvCusName
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate customer name
*!*************************************************************
*! Calls     : CUSBROWM,gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCusName()
*!*************************************************************
FUNCTION lfvCusName
PRIVATE lcAccount

IF (laScrMode[3] .OR. laScrMode[4]) .AND. laData[1] = 'M'
  laData[43] = laData[4]
  IF lnActFolder=2
     SHOW GET laData[43] DISABLE
  ENDIF
  RETURN
ENDIF
IF !laScrMode[1] .OR. (!llBrowse .AND. EMPTY(laData[4]))
  RETURN
ENDIF
IF llBrowse .OR. '?' $ laData[4]
  lcAccount = laData[4]
  IF !CUSBROWM(@lcAccount,'','MSN')
    STORE SPACE(30) TO laData[4]
  ELSE
    laData[2] = lcAccount
    
    *B127522,1 EIH 06/02/2005 Set laData[3] to selected store to prevent search by account, name
    *B127522,1 EIH            And empty store [Begin]
    laData[3] = Customer.store
    *B127522,1 EIH   [End]
    
    laData[4] = Customer.StName
    laData[5] = Customer.Phone1
    SHOW GET laData[2]
    SHOW GET laData[4]
    SHOW GET laData[5]
    SHOW GET ibStore   ENABLE
    SHOW GET laData[3] ENABLE
    _CUROBJ = OBJNUM(laData[3])
  ENDIF
  llBrowse = .F.
  RETURN
ENDIF
SET ORDER TO TAG 'Customnm' IN Customer

*B127522,1 EIH 06/02/2005 Another bug if customer type is 'S' we must seek by type = 'S'
*IF SEEK('M'+UPPER(laData[4]),'Customer')
IF SEEK('M'+UPPER(laData[4]),'Customer')  OR SEEK('S'+UPPER(laData[4]),'Customer')
*B127522,1 EIH [End]

  *E300455,1 Message : 40041
  *E300455,1 Name xxxxx already exists for one or more accounts.
  *E300455,1 Button : 40005
  *E300455,1 Browse Add Reenter
  lnChoice = gfModalGen('QRM40041B40005','ALERT','Name '+ALLTRIM(laData[4]))
ELSE
  *E300455,1 Message : 40040
  *E300455,1 Name xxxxx is not found in the data file.
  *E300455,1 Button : 40005
  *E300455,1 Browse Add Reenter
  lnChoice = gfModalGen('QRM40040B40005','ALERT','Name '+ALLTRIM(laData[4]))
ENDIF
DO CASE
  CASE lnChoice = 1
    lcAccount = laData[4]
    =SEEK('M'+UPPER(ALLTRIM(laData[4])),'Customer')
    
    *B127522,1 EIH 06/02/2005 Another bug if customer type is 'S' we must seek by type = 'S'
    IF !FOUND() 
      =SEEK('S'+UPPER(ALLTRIM(laData[4])),'Customer')
    ENDIF
    *B127522,1 EIH [End]
    
    IF !CUSBROWM(@lcAccount,'','MSN')
      STORE SPACE(30) TO laData[4]
    ELSE
      laData[2] = lcAccount
      
      *B127522,1 EIH 06/02/2005 Set laData[3] to selected store to prevent search by account, name
      *B127522,1 EIH            And empty store [Begin]
      laData[3] = Customer.store
      *B127522,1 EIH   [End]
    
      laData[4] = Customer.StName
      laData[5] = Customer.Phone1
      SHOW GET laData[2]
      SHOW GET laData[4]
      SHOW GET laData[5]
      SHOW GET ibStore   ENABLE
      SHOW GET laData[3] ENABLE
      _CUROBJ = OBJNUM(laData[3])
    ENDIF
  CASE lnChoice = 2
    STORE .T. TO llAddAcc
    _CUROBJ = OBJNUM(laData[2])
  CASE lnChoice = 3
    _CUROBJ = _CUROBJ
ENDCASE
SET ORDER TO TAG 'Customer' IN Customer

*!*************************************************************
*! Name      : lfvCusPhone
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate customer phone
*!*************************************************************
*! Calls     : CUSBROWM,gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCusPhone()
*!*************************************************************
FUNCTION lfvCusPhone
PRIVATE lcAccount

IF !laScrMode[1] .OR. (!llBrowse .AND. EMPTY(laData[5]))
  RETURN
ENDIF
IF llBrowse .OR. '?' $ laData[5]
  lcAccount = laData[5]
  IF !CUSBROWM(@lcAccount,'','MSP')
    STORE SPACE(16) TO laData[5]
  ELSE
    laData[2] = lcAccount
    laData[4] = Customer.StName
    laData[5] = Customer.Phone1
    SHOW GET laData[2]
    SHOW GET laData[4]
    SHOW GET laData[5]
    SHOW GET ibStore   ENABLE
    SHOW GET laData[3] ENABLE
    _CUROBJ = OBJNUM(laData[3])
  ENDIF
  llBrowse = .F.
  RETURN
ENDIF
SET ORDER TO TAG 'Customph' IN Customer
IF SEEK('M'+UPPER(laData[5]),'Customer')
  *E300455,1 Message : 40041
  *E300455,1 Phone xxxxx already exists for one or more accounts.
  *E300455,1 Button : 40005
  *E300455,1 Browse Add Reenter
  lnChoice = gfModalGen('QRM40041B40005','ALERT','Phone '+ALLTRIM(laData[5]))
ELSE
  *E300455,1 Message : 40040
  *E300455,1 Phone xxxxx is not found in the data file.
  *E300455,1 Button : 40005
  *E300455,1 Browse Add Reenter
  lnChoice = gfModalGen('QRM40040B40005','ALERT','Phone '+ALLTRIM(laData[5]))
ENDIF
DO CASE
  CASE lnChoice = 1
    lcAccount = laData[5]
    =SEEK('M'+UPPER(ALLTRIM(laData[5])),'Customer')
    IF !CUSBROWM(@lcAccount,'','MSP')
      STORE SPACE(16) TO laData[5]
    ELSE
      laData[2] = lcAccount
      laData[4] = Customer.StName
      laData[5] = Customer.Phone1
      SHOW GET laData[2]
      SHOW GET laData[4]
      SHOW GET laData[5]
      SHOW GET ibStore   ENABLE
      SHOW GET laData[3] ENABLE
      _CUROBJ = OBJNUM(laData[3])
    ENDIF
  CASE lnChoice = 2
    STORE .T. TO llAddAcc
    *--- B#605895 ,1 SSH [Start] Assigne "M" to the customer type (ladata[1]) incase of add from 
    *--- B#605895 ,1 SSH         phone object.
    laData[1] = IIF(EMPTY(laData[3]) .AND. ((TYPE("lcStore")="C" .AND. EMPTY(lcStore)) .OR. (TYPE("lcStore")<>"C")),'M','S')
    *--- B#605895 ,1 SSH [End]
    _CUROBJ = OBJNUM(laData[2])
  CASE lnChoice = 3
    _CUROBJ = _CUROBJ
ENDCASE
SET ORDER TO TAG 'Customer' IN Customer

*!*************************************************************
*! Name      : lfInvoices
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Inquire Customer Invoices
*!*************************************************************
*! Calls     : gfOpenFile,AriaBrow,gpDoProg,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfInvoices()
*!*************************************************************
FUNCTION lfInvoices
PRIVATE lcFile_Ttl,lcBrfields

*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDRA','SH')
=gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH')
=gfOpenFile(gcDataDir+'CONSINVH',gcDataDir+'CONSINVH','SH')
*E301077,49 (End)

SELECT INVHDR
SET RELATION TO IIF(EMPTY(STORE),'M'+Account,'S'+Account+STORE) INTO CUSTOMER
=SEEK(laData[2])
IF !EMPTY(laData[3])
  LOCATE REST WHILE ACCOUNT = laData[2] FOR ;
  STORE=laData[3] OR (CONSOL='Y' .AND. SEEK(INVHDR.INVOICE+laData[3],'CONSINVH'))
ENDIF
IF FOUND('INVHDR')
  lcFile_Ttl = "Invoices"
  lcBrfields = "INVOICE :H='Invoice' ,STATUS :H='S',INVDATE :H='Date',"+;
               "STORE :H='Store',ORDER :H='Order',CUSTOMER.STNAME :H='Name':17 ,"+;
               "TOTALCHG :H='Amount',PIKTKT :H='Piktkt',CUSTPO :H='Cust P/O',"+;
               "REP1 :H='Rep1', COMM1 :H='Comm',REP2 :H='Rep2', COMM2 :H='Comm',"+;
               IIF(gfIsEdtble('CTERMCODE'),"cTermCode :H='Terms'",;
               "lcDesc= gfCodDes(cTermCode,'CTERMCODE') :H='Terms'")
 IF AriaBrow("laData[2] FOR IIF(EMPTY(laData[3]),.T.,;
           ((CONSOL='Y' AND SEEK(INVHDR.INVOICE+laData[3],'CONSINVH'));
           OR STORE=laData[3]))",lcFile_Ttl,gnBrFSRow1, ;
           gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Invoice','laBrowArr')
    lcInvoice = "'"+INVHDR.INVOICE+"'"
    DO gpDoProg WITH 'AWRARDINV',.F.,'AR',lcInvoice
  ENDIF
ELSE
  *E300455,1 Message : 40082
  *E300455,1 No invoices found for account/Store: xxxxx/xxxxxxx.
  *E300455,1 Button : 00000
  *E300455,1 Ok
  IF laData[1]='M'
    =gfModalGen('TRM40082B00000','ALERT','invoices'+'|'+'account: '+laData[2])
  ELSE
    =gfModalGen('TRM40082B00000','ALERT','invoices'+'|'+'account/store: '+laData[2]+'/'+laData[3])
  ENDIF  
ENDIF
*E301077,49 Inhance openning files to speed up transaction
=gfCloseFile('INVHDR')
=gfCloseFile('INVLINE')
=gfCloseFile('CONSINVH')
*E301077,49 (End)
SELECT CUSTOMER
*B802216,1 Return to customer record after inquire transaction
=SEEK(laData[1]+laData[2]+laData[3])
*B802216,1 (End)
*!*************************************************************
*! Name      : lfOrders
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Inquire Customer Orders
*!*************************************************************
*! Calls     : gfOpenFile,ORDBROWA,gpDoProg
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfOrders()
*!*************************************************************
FUNCTION lfOrders
PRIVATE xStore,lcParameter

llBrowse = .T.

*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
*E301077,49 (End)

xStore = IIF(EMPTY(laData[3]),'',laData[3])
*B802873,1 ABD 12/12/1999 Pass parameter 'O' to browse orders. [ Begin ] 
*IF ORDBROWA(laData[2],.T.,'A')
IF ORDBROWA(laData[2],.T.,'O')
  *B802873,1 ABD [ End ]
  llBrowse = .F.
  lcParameter = "'"+ORDHDR.CORDTYPE+"','"+ORDHDR.ORDER+"'"
  DO gpDoProg WITH 'AWRSOORD',.F.,'SO',lcParameter
ENDIF
llBrowse = .F.
*E301077,49 Inhance openning files to speed up transaction
=gfCloseFile('ORDHDR')
=gfCloseFile('ORDLINE')
*E301077,49 (End)
SELECT CUSTOMER
*B802216,1 Return to customer record after inquire transaction
=SEEK(laData[1]+laData[2]+laData[3])
*B802216,1 (End)
*!*************************************************************
*! Name      : lfvCusNote
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Customer Notepad
*!*************************************************************
*! Calls     : NOTEPAD
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvCusNote()
*!*************************************************************
FUNCTION lfvCusNote
*B801952,1 define variable 'lckey' as privare
PRIVATE lcKey
=NOTEPAD('A',laData[2])

*!*************************************************************
*! Name      : lfvObjLink
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Object Link
*!*************************************************************
*! Calls     : GetObj
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvObjLink()
*!*************************************************************
FUNCTION lfvObjLink
PRIVATE lnAlais
lnAlais = SELECT()
DO GetObj WITH 'A',laData[2]
SELECT (lnAlais)

*!*************************************************************
*! Name      : lfvRep1
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate first salesrep
*!*************************************************************
*! Calls     : REPCHK,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRep1()
*!*************************************************************
FUNCTION lfvRep1
PRIVATE lcSales

*B801874,1 Commented out
*B602184,1 Reham On 11/15/98  *** Begin ***
*B602184,1 Execute the function only if typed something in the object.
*IF !EMPTY(laData[19])
*B602184,1 Reham On 11/15/98  *** End   ***
*B801874,1 May leave salesrep empty
*  IF llBrowse .OR. !SEEK(laData[19],'SalesRep')
IF llBrowse .OR. (!EMPTY(laData[19]) AND !SEEK(laData[19],'SalesRep'))
*B801874,1 (End)

  lcSales = IIF(llBrowse,SPACE(3),laData[19])
  GO TOP IN 'SalesRep'
  =REPCHK(@lcSales)
  laData[19] = IIF(EMPTY(lcSales),lcOldValue,lcSales)

ENDIF

*E301245,1 since there's updating for Both Commissions , SHOW GET must be for Both Commissions [Begin.]
IF EMPTY(laData[19])                 && If Rep1 empty
  laData[20] = 0                     && Rep1 comm must be also empty (zero)
  IF !EMPTY(laData[21])              && and if Rep2 not empty 
    IF laScrMode[4]                  && ask if in ADD Mode
      =SEEK(laData[21],'SALESREP')   && get pointer at rep2 in SALESREP file
      laData[22] = SALESREP.COMM     && and restore default comm .
    ELSE                             && ask if in EDIT Mode
      =SEEK(laData[21],'SALESREP')   && get pointer at rep2 in SALESREP file 
      IF laData[22] = SALESREP.COMM-(SALESREP.COMM*SALESREP.COMM_RATE/100)
        laData[22] = SALESREP.COMM        
      ENDIF          
    ENDIF  
  ENDIF
  
ELSE                                 && Else Rep1 not empty
  IF EMPTY(laData[21])               && ask if Rep2 empty
    IF lcOldValue <> laData[19]      && If Rep1 is no changed
      laData[20] = SALESREP.COMM     && Rep2 empty then Rep1 comm must be restored to default comm from SALESREP File
      laData[22] = 0
    ENDIF
  ELSE                               && Else Rep2 not empty and also Rep1 not empty
    IF laScrMode[4]                  && ask if in ADD Mode     
      IF lcOldValue <> laData[19]    && If Rep1 is no changed
        *-- in this case we calculate Comm1 for Rep1 with split comm and also the same to Rep2
        laData[20] = SALESREP.COMM-(SALESREP.COMM*SALESREP.COMM_RATE/100)   
        =SEEK(laData[21],'SALESREP')
        laData[22] = SALESREP.COMM-(SALESREP.COMM*SALESREP.COMM_RATE/100)
      ENDIF  
    ELSE                             && EDIT Mode
      IF lcOldValue <> laData[19]    && If Rep1 is no changed
        laData[20] = SALESREP.COMM-(SALESREP.COMM*SALESREP.COMM_RATE/100)
        =SEEK(laData[21],'SALESREP')
        IF laData[22] = SALESREP.COMM
          laData[22] = SALESREP.COMM-(SALESREP.COMM*SALESREP.COMM_RATE/100)        
        ENDIF    
      ENDIF  
    ENDIF  
  ENDIF
ENDIF

SHOW GET laData[20]
SHOW GET laData[22]  
*E301245,1 since there's updating for Both Commissions , SHOW GET must be for Both Commissions [End.]
  
=lfRefresh(lcWinCh1)
llBrowse = .F.

*B801874,1 Commented out

*  *B602184,1 Reham On 11/15/98  *** Begin ***
*  *B602184,1 Fix the trappinp in the sales rep. 1 in case there is no reps.
*  *B602184,1 in the sales reps. file.
*  _CUROBJ = _CUROBJ + 1
**ENDIF
**B602184,1 Reham On 11/15/98  *** End   ***
*B801874,1 (End)

*!*************************************************************
*! Name      : lfvRep2
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Second salesrep
*!*************************************************************
*! Calls     : REPCHK,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRep2()
*!*************************************************************
FUNCTION lfvRep2
PRIVATE lcSales

IF llBrowse .OR. (!EMPTY(laData[21]) AND !SEEK(laData[21],'SalesRep'))
  lcSales = IIF(llBrowse,SPACE(3),laData[21])
  GO TOP IN 'SalesRep'
  =REPCHK(@lcSales)
  laData[21] = IIF(EMPTY(lcSales),lcOldValue,lcSales)

ENDIF  

*E301245,1 since there's updating for Both Commissions , SHOW GET must be for Both Commissions [Begin.]
IF EMPTY(laData[21])                 && If Rep2 empty
  laData[22] = 0                     && Rep2 comm must be also empty (zero)
  IF !EMPTY(laData[19])              && and if Rep1 not empty 
    IF laScrMode[4]                  && ask if in ADD Mode
      =SEEK(laData[19],'SALESREP')   && get pointer at rep1 in SALESREP file
      *B802554,1 ALB(Start) 09/01/1999 If enterning Rep1 only for first time,leaving Rep2 empty
      *B802554,1                get comm1 from file . 
      IF laData[20] = 0 
        laData[20] = SALESREP.COMM     && and restore default comm .
      ENDIF
      *B802554,1 ALB(End)      
    ELSE                             && ask if in EDIT Mode
      =SEEK(laData[19],'SALESREP')   && get pointer at rep1 in SALESREP file 
      IF laData[20] = SALESREP.COMM-(SALESREP.COMM*SALESREP.COMM_RATE/100)
        laData[20] = SALESREP.COMM        
      ENDIF          
    ENDIF  
  ENDIF
  
ELSE                                 && Else Rep1 not empty
  IF EMPTY(laData[19])               && ask if Rep2 empty
    IF lcOldValue <> laData[21]      && If Rep1 is no changed
      laData[22] = SALESREP.COMM     && Rep2 empty then Rep1 comm must be restored to default comm from SALESREP File
      laData[20] = 0
    ENDIF  
  ELSE                               && Else Rep2 not empty and also Rep1 not empty
    IF laScrMode[4]                  && ask if in ADD Mode     
      IF lcOldValue <> laData[21]    && If Rep2 is no changed
        *-- in this case we calculate Comm1 for Rep1 with split comm and also the same to Rep2
        laData[22] = SALESREP.COMM-(SALESREP.COMM*SALESREP.COMM_RATE/100)   
        =SEEK(laData[19],'SALESREP')
        laData[20] = SALESREP.COMM-(SALESREP.COMM*SALESREP.COMM_RATE/100)
      ENDIF  
    ELSE                             && EDIT Mode
      IF lcOldValue <> laData[21]    && If Rep2 is no changed 
        laData[22] = SALESREP.COMM-(SALESREP.COMM*SALESREP.COMM_RATE/100)   
        =SEEK(laData[19],'SALESREP')
        IF SALESREP.COMM = laData[20]
          laData[20] = SALESREP.COMM-(SALESREP.COMM*SALESREP.COMM_RATE/100)        
        ENDIF    
      ENDIF  
    ENDIF  
  ENDIF
ENDIF

SHOW GET laData[20]
SHOW GET laData[22]  
*E301245,1 since there's updating for Both Commissions , SHOW GET must be for Both Commissions [End.]

=lfRefresh(lcWinCh1)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvFactor
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate factor
*!*************************************************************
*! Calls     : FacChk
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFactor()
*!*************************************************************
FUNCTION lfvFactor
PRIVATE lcFactor
IF llBrowse .OR. (!EMPTY(laData[30]) .AND. !SEEK(laData[30],'SycFact'))

  lcBrFields  = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  laData[30]= IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,;
                  gnBrFSRow2, gnBrFSCol2,'','','cFacCode','laBrowArr'),;
                  SycFact.cFacCode,SPACE(6))
ENDIF
=lfRefresh(lcWinCh1)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvSkuTemp
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Sku/Pack Template
*!*************************************************************
*! Calls     : AriaBrow
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSkuTemp()
*!*************************************************************
FUNCTION lfvSkuTemp

IF llBrowse OR !EMPTY(laData[77])
  lnAlias = SELECT()
  *E301077,49 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'SKUTMPL',gcDataDir+'SKUTMPL','SH')
  *E301077,49 (End)

  IF llBrowse OR (!SEEK('P'+laData[77],'SKUTMPL') AND !SEEK('S'+laData[77],'SKUTMPL'))
    SELECT cSkuCode,Desc FROM SKUTMPL GROUP BY cSkuCode ORDER BY cSkuCode INTO CURSOR SKUPACK
    lcBrowTitl = 'SKU/Packs Template Codes'
    lcBrFields = "cSkuCode :R :H='Code',Desc :R :H='Description'"
    laData[77] = IIF(AriaBrow("",lcBrowTitl,gnBrHSRow1,.F., gnBrHSRow2, .F., '',;
                   '','cSkuCode','laBrowArr'),cSkuCode,lcOldValue)
    USE IN SKUPACK
  ENDIF  
  *E301077,49 Inhance openning files to speed up transaction
  =gfCloseFile('SKUTMPL')
  *E301077,49 (End)
  SELECT (lnAlias)
ENDIF
llBrowse = .F.

*!*************************************************************
*! Name      : lfvLinkCode
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer link code
*!*************************************************************
*! Calls     : gfGLBrowse
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvLinkCode()
*!*************************************************************
FUNCTION lfvLinkCode
PRIVATE lcLinkCode

IF llBrowse .OR. !SEEK('01'+laData[61],'Gl_Link')
  lcLinkCode = laData[61]
  *B801949,1 Restore customer old gl link code when browse failed
  *=gfGLBrowse('01',@lcLinkCode)
  *laData[61] = lcLinkCode
  laData[61] = IIF(gfGLBrowse('01',@lcLinkCode),lcLinkCode,lcOldValue)
  *B801949,1 (End)
ENDIF
=lfRefresh(lcwinch8)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvSlsLink
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer Sales link code
*!*************************************************************
*! Calls     : gfGLBrowse
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSlsLink()
*!*************************************************************
FUNCTION lfvSlsLink
PRIVATE lcLinkCode
IF llBrowse .OR. !SEEK('02'+laData[73],'Gl_Link')
  lcLinkCode = laData[73]
  *B801949,1 Restore customer old gl link code when browse failed
  *=gfGLBrowse('02',@lcLinkCode,'',1)
  *laData[73] = lcLinkCode
  laData[73] = IIF(gfGLBrowse('02',@lcLinkCode,'',1),lcLinkCode,lcOldValue)
  *B801949,1 Restore customer old gl link code when browse failed
ENDIF
=lfRefresh(lcwinch8)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvClass
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer Class
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvClass()
*!*************************************************************
FUNCTION lfvClass
laData[33] = SUBSTR(laClass[lnClass,2],1,6)

*!*************************************************************
*! Name      : lfvTerms
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer Terms
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTerms()
*!*************************************************************
FUNCTION lfvTerms
laData[34] = SUBSTR(laTerms[lnTerms,2],1,6)

*!*************************************************************
*! Name      : lfvDivision
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer Division
*!*************************************************************
*! Calls     : gfRltFld,gfModalGen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDivision()
*!*************************************************************
FUNCTION lfvDivision
laData[35] = SUBSTR(laDivision[lnDivision,2],1,6)
IF laSetups[3,2]='Y'
  =gfRltFld(laData[35],@laDRltFld,'CDIVISION')
  IF !EMPTY(lcCustLink) .AND. laData[61] <> PADR(lcCustLink,6)
    *E300455,1 Message : 40047
    *E300455,1 Division link code does not match entered link code. Overwrite?
    *E300455,1 Button : 40000
    *E300455,1 Yes No
    IF gfModalGen('QRM40047B40000','ALERT') = 1
      laData[61] = PADR(lcCustLink,6)
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvRegion
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer Region
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRegion()
*!*************************************************************
FUNCTION lfvRegion
laData[39] = SUBSTR(laRegion[lnRegion,2],1,6)

*!*************************************************************
*! Name      : lfvShipVia
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer ShipVia
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipVia()
*!*************************************************************
FUNCTION lfvShipVia
IF SUBSTR(laShipVia[lnShipVia,2],1,6) = ladata[62]
  *E300455,1 Message : 40056
  *E300455,1 Alternate ship via must be different from the original ship via.
  *E300455,1 Button : 00000
  *E300455,1 Ok
  *B605086,1 NNA 12/28/2003 Change the message which appear when changing the Alternate ship via.
  ** ALTERNATIVE SHIP VIA MUST BE DIFFERENT FROM THE ORIGINAL SHIP VIA
  =gfModalGen('TRM40056B00000','ALERT')
  lnShipVia = (ASCAN(laShipVia,laData[40])+1)/2
  SHOW GET lnShipVia
ENDIF
laData[40] = SUBSTR(laShipVia[lnShipVia,2],1,6)

*!*************************************************************
*! Name      : lfvAltShipVia
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer Alternate ShipVia
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAltShipVia()
*!*************************************************************
FUNCTION lfvAltShipVia

laData[62] = SUBSTR(laAShipVia[lnAShipVia,2],1,6)
IF !EMPTY(ladata[62]) .AND. ladata[62] = laData[40]
  *E300455,1 Message : 40056
  *E300455,1 Alternate ship via must be different from the original ship via.
  *E300455,1 Button : 00000
  *E300455,1 Ok
  =gfModalGen('TRM40056B00000','ALERT')
  STORE 1 TO lnAShipVia
  laData[62] = SPACE(6)
  SHOW GET lnAShipVia
ENDIF

*!*************************************************************
*! Name      : lfvSpcInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer Special Instruction
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSpcInst()
*!*************************************************************
FUNCTION lfvSpcInst
laData[41] = SUBSTR(laSpcInst[lnSpcInst,2],1,6)

*!*************************************************************
*! Name      : lfvAccCurr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer Currency
*!*************************************************************
*! Calls     : gfCurrBrow
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAccCurr()
*!*************************************************************
FUNCTION lfvAccCurr
PRIVATE lcCurrency

IF llBrowse .OR. laData[38] <> lcOLdValue
  *E301077,49 Inhance openning files to speed up transaction
  =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
  *E301077,49 (End)

  IF llBrowse .OR. !SEEK(laData[38],'SycCurr')
    lcCurrency = laData[38]
    IF !gfCurrBrow(@lcCurrency)
      lcCurrency = lcOLdValue
    ENDIF
    laData[38] = lcCurrency
  ENDIF
  *E301077,49 Inhance openning files to speed up transaction
  =gfCloseFile('SycCurr')
  *E301077,49 (End)
ENDIF    
llBrowse = .F.

*!*************************************************************
*! Name      : lfvProfile
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Add/Modify Customer Profile
*!*************************************************************
*! Calls     : arCusPrf.SPX
*!*************************************************************
*! Passed Parameters  :  llNewPrf  .T. New Profile
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvProfile()
*!*************************************************************
FUNCTION lfvProfile
PARAMETERS llNewPrf
PRIVATE laPrfCode,laProfile,lnProfile,lnPrfRecNo
DECLARE laPrfCode[1,10],laProfile[1,2]

laCodes[7,7] = IIF(laScrMode[2],'PROFILE',lcProFile)
laCodes[7,8] = IIF(laScrMode[2],'PROFILE',lcProFile)
STORE '' TO laProfile
STORE 0  TO lnProfile
laProfile[1,1] = 'Select a Profile'
laProfile[1,2] = ''
laCodes[7,4]   = "N"
lnPrfRecNo = 1
SELECT IIF(laScrMode[2],'Profile',lcProFile)
IF llNewPrf
  SCATTER MEMVAR FIELDS cPro_Code,cPro_Value,dPro_Date BLANK
ELSE
  SCATTER MEMVAR FIELDS cPro_Code,cPro_Value,dPro_Date
  lnPrfRecNo = RECNO()
ENDIF
lnProfile = 1
lcPrfStat  = IIF(llNewPrf,'ENABLE','DISABLE')
lcFildStat = IIF(llNewPrf .OR. laScrMode[2],'DISABLE','ENABLE')
DO (gcScrDir+"arCusPrf.SPX")
IF !laScrMode[2] .AND. !EMPTY(&lcProFile..cPro_Code)
  SELECT (lcProFile)
  SHOW GET pbPModify ENABLE
  SHOW GET pbPRemove ENABLE
ENDIF
SHOW WINDOW (lcProfTitl) REFRESH SAME

*!*************************************************************
*! Name      : lfvRemPrf
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Remove Customer Profile
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemPrf()
*!*************************************************************
FUNCTION lfvRemPrf

*E300455,1 Message : 40054
*E300408,1 Are you sure you want to delete this Profile?
*E300455,1 Button : 40000
*E300455,1 Yes No
IF gfModalGen('QRM40054B40000','ALERT','Profile') = 1
  SELECT (lcProFile)
  DELETE
  IF !SEEK('C'+PADR(laData[2],8)+laData[3])
    SHOW GET pbPModify DISABLE
    SHOW GET pbPRemove DISABLE
  ENDIF
  SHOW WINDOW (lcProfTitl) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvPrfId
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer Profile Code
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPrfId()
*!*************************************************************
FUNCTION lfvPrfId

m.cPro_Code = SUBSTR(laProfile[lnProfile,2],1,6)
m.dPro_Date = gdSysDate
SHOW GET lnProfile    DISABLE
SHOW GET pbBrowPrf    ENABLE
SHOW GET m.cPro_Value ENABLE
SHOW GET m.dPro_Date  ENABLE
SHOW GET pbOkPrf      ENABLE
_CUROBJ = OBJNUM(m.cPro_Value)

*!*************************************************************
*! Name      : lfvSelValue
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Activate Customer Profile Values screen
*!*************************************************************
*! Calls     : arPrfVal.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSelValue()
*!*************************************************************
FUNCTION lfvSelValue
SELECT (lcProVal)
SET KEY TO m.cPro_Code
lcValRemSt=IIF(SEEK(m.cPro_Code),'ENABLE','DISABLE')
llNewPrfVal =.F.
DO (gcScrDir+"arPrfVal.SPX")
SELECT (lcProFile)
_CUROBJ = OBJNUM(m.cPro_Value)

*!*************************************************************
*! Name      : lfvNewVal
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Add new Profile value
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNewVal()
*!*************************************************************
FUNCTION lfvNewVal

llNewPrfVal =.T.
STORE SPACE(30) TO lcPrfValue
SHOW GET lcPrfValue ENABLE
_CUROBJ = OBJNUM(lcPrfValue)

*!*************************************************************
*! Name      : lfvRemVal
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Remove Profile value
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemVal()
*!*************************************************************
FUNCTION lfvRemVal
*E300455,1 Message : 40054
*E300408,1 Are you sure you want to delete this Reference?
*E300455,1 Button : 40000
*E300455,1 Yes No
IF gfModalGen('QRM40054B40000','ALERT','Reference') = 1
  SELECT (lcProVal)
  =SEEK(m.cPro_Code+lsPrfValue)
  DELETE
  lcValRemSt=IIF(SEEK(m.cPro_Code),'ENABLE','DISABLE')
  lcPrfValue = cPro_Value
  SHOW GET pbRemVal   &lcValRemSt
  SHOW GET lcPrfValue &lcValRemSt
  SHOW GET pbOkVal    &lcValRemSt
  SHOW GET lsPrfValue &lcValRemSt
ENDIF

*!*************************************************************
*! Name      : lfwPrfValue
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Store old profile value
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwPrfValue()
*!*************************************************************
FUNCTION lfwPrfValue
lcPrfValue = lsPrfValue
SHOW GET lcPrfValue

*!*************************************************************
*! Name      : lfvPrfValue
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Profile value
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPrfValue()
*!*************************************************************
FUNCTION lfvPrfValue
IF llNewPrfVal .AND. !EMPTY(lcPrfValue) .AND. !SEEK(m.cPro_Code+lcPrfValue,lcProVal)
  INSERT INTO (lcProVal) (cPro_Code,cPro_Value) VALUES (m.cPro_Code,lcPrfValue)
  lsPrfValue = lcPrfValue
ENDIF
IF !llNewPrfVal .AND. !EMPTY(lcPrfValue) .AND. ;
   !SEEK(m.cPro_Code+lcPrfValue,lcProVal) .AND. SEEK(m.cPro_Code+lsPrfValue,lcProVal)
  SELECT (lcProVal)
  REPLACE cPro_Value WITH lcPrfValue
  lsPrfValue = lcPrfValue
ENDIF
lcValRemSt=IIF(SEEK(m.cPro_Code),'ENABLE','DISABLE')
lcPrfValue = IIF(EMPTY(lcPrfValue),lsPrfValue,lcPrfValue)
=SEEK(m.cPro_Code+lcPrfValue)
SHOW GET pbRemVal   &lcValRemSt
SHOW GET lcPrfValue &lcValRemSt
SHOW GET pbOkVal    &lcValRemSt
SHOW GET lsPrfValue &lcValRemSt
llNewPrfVal =.F.

*!*************************************************************
*! Name      : lfvOkPrf
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Update Customer Profile
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOkPrf()
*!*************************************************************
FUNCTION lfvOkPrf
SELECT (lcProFile)
IF !SEEK('C'+PADR(laData[2],8)+laData[3]+m.cPro_Code+m.cpro_value,lcProFile)
  INSERT INTO (lcProFile) (cContType,cCont_Id,Store,cPro_Code,cpro_value) ;
  VALUES ('C',laData[2],laData[3],m.cPro_Code,m.cpro_value)
ENDIF
REPLACE dPro_Date WITH m.dPro_Date

*!*************************************************************
*! Name      : lfvContact
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Add/Modify Customer Contact
*!*************************************************************
*! Calls     : arcuscon.SPX
*!*************************************************************
*! Passed Parameters  :  llNewCont .T. New Contact
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvContact()
*!*************************************************************
FUNCTION lfvContact
PARAMETERS llNewCont

SELECT IIF(laScrMode[2],'Contact',lcContFile)
IF llNewCont
  *C101764,1 (Start) Add the Email_add Field 
  *SCATTER MEMVAR FIELDS Contact,cContTtl,Phone,Fax,cContSalut,mNote MEMO BLANK
  SCATTER MEMVAR FIELDS Contact,cContTtl,Phone,Fax,cContSalut,mNote,cEmail_Add MEMO BLANK 
  *C101764,1 (End)  
ELSE
  *C101764,1 (Start)  Add the Email_add Field 
  *SCATTER MEMVAR FIELDS Contact,cContTtl,Phone,Fax,cContSalut,mNote MEMO
  SCATTER MEMVAR FIELDS Contact,cContTtl,Phone,Fax,cContSalut,mNote,cEmail_Add MEMO
  *C101764,1 (End) 
ENDIF
lcContStat = IIF(laScrMode[2],'DISABLE','ENABLE')
lcFildStat = IIF(EMPTY(m.Contact) .OR. laScrMode[2],'DISABLE','ENABLE')
lcZoomStat = IIF(EMPTY(m.Contact) ,'DISABLE','ENABLE')
lcNoModify = IIF(laScrMode[2]," NOMODIFY","")
*C102589,1 TMI [START] 
lcRoleStat = IIF(EMPTY(laRole[1]) OR EMPTY(m.Contact) ,'DISABLE','ENABLE')     
m.OldCntID = SPACE(30)     && Define Old contact id
*C102589,1 TMI [END  ] 
*C102589,4 TMI [START] Call lfvRole function from lfvContact to fill the laTarget Array.
llRole = .F.  && variable to fix the bug that when Modify contact and click role btn the screen disappear
=lfvRole(.T.)
*C102589,4 TMI [END  ] Call lfvRole function from lfvContact to fill the laTarget Array.
DO (gcScrDir+"arcuscon.SPX")
IF !laScrMode[2] .AND. !EMPTY(Contact)
  SHOW GET pbContact ENABLE
  SHOW GET pbCRemove ENABLE
ENDIF
SHOW WINDOW (lcContTitl) REFRESH SAME

*!*************************************************************
*! Name      : lfvRemCon
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Remove Customer Contact
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemCon()
*!*************************************************************
FUNCTION lfvRemCon

*E300455,1 Message : 40054
*E300408,1 Are you sure you want to delete this contact?
*E300455,1 Button : 40000
*E300455,1 Yes No
IF gfModalGen('QRM40054B40000','ALERT','Contact') = 1
  SELECT (lcContFile)
  *C102589,1 TMI [START] Delete related records in lcContRole
  SELECT (lcContRole)
  *C102589,4 TMI [START]
  GO TOP
  *C102589,4 TMI [END  ]
  REPLACE CMARK WITH 'D' FOR CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE = ;
                        'C'+PADR(laData[2],8)+laData[3]+&lcContFile..CONTACT
  SELECT (lcContFile)           
  *C102589,1 TMI [END  ] Delete related records in lcContRole
  DELETE
  IF !SEEK('C'+PADR(laData[2],8)+laData[3])
    SHOW GET pbCRemove DISABLE
    SHOW GET pbContact DISABLE
  ENDIF
  SHOW WINDOW (lcContTitl) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvContId
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Customer Contact
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvContId()
*!*************************************************************
FUNCTION lfvContId
PRIVATE lcOldContact
IF EMPTY(m.Contact)
  RETURN
ENDIF
lcOldContact = &lcContFile..Contact
*C102589,1 TMI [Start] Save old contact id.
m.OldCntID = &lcContFile..Contact
*C102589,1 TMI [End  ] Save old contact id.
IF SEEK('C'+PADR(laData[2],8)+laData[3]+m.Contact,lcContFile)
  *C101764,1 (Start) Add The Email_add Field 
  *SCATTER MEMVAR FIELDS Contact,cContTtl,Phone,Fax,cContSalut,mNote MEMO
  SCATTER MEMVAR FIELDS Contact,cContTtl,Phone,Fax,cContSalut,mNote,cEmail_Add MEMO 
  *C101764,1 (End)
ELSE
  =SEEK('C'+PADR(laData[2],8)+laData[3]+lcOldContact,lcContFile)
ENDIF
SHOW GET m.cContTtl   ENABLE
SHOW GET m.Phone      ENABLE
SHOW GET m.Fax        ENABLE
SHOW GET m.cContSalut ENABLE
SHOW GET m.mNote      ENABLE
SHOW GET pbZoom       ENABLE
SHOW GET pbOkCont     ENABLE
*C101764,1 (Start) Add the Email_add Field 
SHOW GET m.cEmail_Add ENABLE
*C101764,1 (End)
*C102589,1 TMI [START] 
IF !EMPTY(laRole[1])
  SHOW GET pbRole     ENABLE  && Enable Role button
ENDIF
IF !llNewCont AND m.OldCntID # m.Contact
  *C102589,4 TMI [START] Prevent Clicking Role button when used modified contact id
  IF BETWEEN(MCOL(),47,57) AND BETWEEN(MROW(),2,4)
    llRole = .T.
  ELSE
  *C102589,4 TMI [END  ] Prevent Clicking Role button when it is disabled
    SHOW GET pbRole     DISABLE  && DISABLE Role button
  *C102589,4 TMI [START]
  ENDIF
  *C102589,4 TMI [END  ]
ENDIF
*C102589,1 TMI [END  ]

*!*************************************************************
*! Name      : lfvOkCont
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Update Customer Contact
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOkCont()
*!*************************************************************
FUNCTION lfvOkCont
*C102589,4 TMI [Start] 
PRIVATE lcAlias
lcAlias = ALIAS()
*C102589,4 TMI [End  ] 

*C102589,1 TMI [Start] Ask to change Contact ID in the Role file.
IF !llNewCont AND m.OldCntID # m.Contact AND !EMPTY(laTarget[1])
  *-- Message 32104 : You have changed the contact ID. Would you like to delete 
  *--           the roles assigned to the old ID or Keep them ?
  *-- Button  32012 : Delete ; Keep ; Cancel
  lnResp = gfModalGen('TRM32104B32012','ALERT')                             
  SELECT (lcContRole)
  =SEEK('C'+PADR(laData[2],8)+laData[3]+m.OldCntID)
  DO CASE
    CASE lnResp = 1   && Delete 
      REPLACE CMARK WITH 'D'  REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE = ;
                                       'C'+PADR(laData[2],8)+laData[3]+m.OldCntID
    
    CASE lnResp = 2   && Update
      PRIVATE laRls
      DIMENSION laRls[1]      
      FOR lnI = 1 TO ALEN(laTarget,1)
        IF !SEEK('C'+PADR(laData[2],8)+laData[3]+m.OldCntID+laTarget[lnI],lcContRole)
          INSERT INTO (lcContRole) VALUES ;
                ('C',PADR(laData[2],8),laData[3],m.OldCntID,laTarget[lnI],'D')
        ELSE
          REPLACE CMARK WITH 'D'
        ENDIF
        INSERT INTO (lcContRole) VALUES ;
              ('C',PADR(laData[2],8),laData[3],m.CONTACT,laTarget[lnI],' ')        
      ENDFOR
            
    CASE lnResp = 3
      *C102589,4 TMI [Start] 
      SELECT (lcAlias)
      *C102589,4 TMI [End  ] 
      RETURN .F.  
  ENDCASE 
ENDIF
*C102589,1 TMI [End  ] Ask to change Contact ID in the Role file.
*C102589,4 TMI [Start] 
SELECT (lcAlias)
*C102589,4 TMI [End  ] 

IF llNewCont .AND. !SEEK('C'+PADR(laData[2],8)+laData[3]+m.Contact,lcContFile)
  INSERT INTO (lcContFile) (cContType,cCont_Id,Store) ;
  VALUES ('C',laData[2],laData[3])
ENDIF
IF !laScrMode[2]
  GATHER MEMVAR MEMO
ENDIF
CLEAR READ

*!*************************************************************
*! Name      : lfCusRa
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Inquire Customer R/As
*!*************************************************************
*! Calls     : RABROW,gpDoProg
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCusRa()
*!*************************************************************
FUNCTION lfCusRa
PRIVATE lcRaNo,xAccount,xStore

lcRaNo = SPACE(6)
xAccount = laData[2]
xStore   = laData[3]
=RABROW(@lcRaNo,.T.)
IF !EMPTY(lcRaNo)
  lcRaNo = "'"+lcRaNo+"'"
  DO gpDoProg WITH 'AWRRMRTATH',.F.,'RM',lcRaNo
ENDIF
SELECT CUSTOMER
*B802216,1 Return to customer record after inquire transaction
=SEEK(laData[1]+laData[2]+laData[3])
*B802216,1 (End)
*!*************************************************************
*! Name      : lfCrMemos
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Inquire Customer Credit Memos
*!*************************************************************
*! Calls     : gfCrdtBrow,gpDoProg
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCrMemos()
*!*************************************************************
FUNCTION lfCrMemos
PRIVATE lcCrMemo
lcCrMemo=gfCrdtBrow(SPACE(6),.F.,laData[2],laData[3])
IF !EMPTY(lcCrMemo)
  lcCrMemo= "'"+lcCrMemo+"'"
  DO gpDoProg WITH 'AWRRMCRMEM',.F.,'RM',lcCrMemo
ENDIF
SELECT CUSTOMER
*B802216,1 Return to customer record after inquire transaction
=SEEK(laData[1]+laData[2]+laData[3])
*B802216,1 (End)
*!*************************************************************
*! Name      : lfStyles
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Inquire Customer Shipped Styles
*!*************************************************************
*! Calls     : gfOpenFile,gfModalGen,AriaBrow,gpDoProg
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfStyles()
*!*************************************************************
FUNCTION lfStyles
PRIVATE lcStyHdr,lcBrFields,lcFile_Ttl

lcStyHdr = gfItemMask("HI")
*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDRA','SH')
*E301077,49 (End)
SELECT InvHdr
=SEEK(laData[2])
LOCATE REST WHILE Account = laData[2] ;
       FOR  Status <> 'V' .AND. IIF(EMPTY(laData[3]),.T.,Store=laData[3])
IF !FOUND()
  *E300455,1 Message : 40082
  *E300455,1 No invoices found for account/Store: xxxxx/xxxxxxx.
  *E300455,1 Button : 00000
  *E300455,1 Ok
  IF laData[1]='M'
    =gfModalGen('TRM40082B00000','ALERT','shipment details'+'|'+'account: '+laData[2])
  ELSE
    =gfModalGen('TRM40082B00000','ALERT','shipment details'+'|'+'account/store: '+laData[2]+'/'+laData[3])
  ENDIF  
ELSE
  *E301077,49 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH')
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
  *E301077,49 (End)

  lcBrowTitl = 'Styles Shipped'
  *B606444,1 TMI [Start] Get the style description from InvLine file not from style file
  *lcBrFields = "InvHdr.invoice :R :H='Inv #':6,"+;
               "InvLine.order  :R :H='Order':6,"+;
               "InvLine.invdate:R :H='Inv Date':10,"+;
               "InvLine.lineno :R :H='Line':6,"+;
               "InvLine.style  :R :H=lcStyHdr,"+;
               "lcStyle=LOOKUP(STYLE.DESC,INVLINE.STYLE,STYLE.STYLE,'STYLE'):R :H='Description.':20,"+;
               "InvLine.totqty :R :H='Totqty':7,"+;
               "InvLine.price  :R :H='Price':9"
  lcBrFields = "InvHdr.invoice :R :H='Inv #':7,"+;
               "InvLine.order  :R :H='Order':7,"+;
               "InvLine.invdate:R :H='Inv Date':10,"+;
               "InvLine.lineno :R :H='Line':6,"+;
               "InvLine.style  :R :H=lcStyHdr,"+;
               "InvLine.Desc1  :R :H='Description.':27,"+;
               "InvLine.totqty :R :H='Totqty':7,"+;
               "InvLine.price  :R :H='Price':9"
  *B606444,1 TMI [End  ] Get the style description from InvLine file not from style file
  SELECT INVLINE
  SET RELATION TO STYLE INTO STYLE
  SET ORDER TO TAG InvLine
  SELECT INVHDR
  SET RELATION TO Invoice INTO InvLine
  SET SKIP TO INVLINE
  *B605825,1 TMI [Start] Optimze opening browse screen for shipped styles for Customer and Store
  *IF AriaBrow("laData[2] FOR STATUS<>'V' .AND. IIF(EMPTY(laData[3]),.T.,;
              STORE=laData[3])",lcBrowTitl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2,gnBrHSCol2,'','','Invoice','laBrowArr')
  SET ORDER TO TAG INVHDR
  IF AriaBrow("FOR ACCOUNT+INVOICE = laData[2] AND STATUS<>'V' .AND. IIF(EMPTY(laData[3]),.T.,;
              STORE=laData[3])",lcBrowTitl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2,gnBrHSCol2,'','','Invoice','laBrowArr')
    SET ORDER TO TAG INVHDRA
    *B605825,1 TMI [End  ] Optimze opening browse screen for shipped styles for Customer and Store
    lcStyClr = "'"+INVLINE.STYLE+"',''"
    DO gpDoProg WITH 'AWRICSTYLE',.F.,'IC',lcStyClr
  ENDIF
  *E301077,49 Inhance openning files to speed up transaction
  =gfCloseFile('INVLINE')
  =gfCloseFile('STYLE')
  *E301077,49 (End)
ENDIF
*E301077,49 Inhance openning files to speed up transaction
=gfCloseFile('INVHDR')
*E301077,49 (End)
SELECT CUSTOMER

*!*************************************************************************
*! Name      : gfCrdtBrow
*! Developer : Reham Alallamy
*! Date      : 30/03/97
*! Purpose   : Func. to browse all the available credit memos.
*!*************************************************************************
*! Calls     :
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION gfCrdtBrow
PARAMETER lcCrMemo,llRetAlias,lcAccount,lcStore
PRIVATE lcBrFields,lnCurAlias,laData

*B802873,1 Define Variable Private [ Begin ]
PRIVATE lcCrMOrdr 
*B802873,1 [ End ]

lcAccount = IIF(TYPE('lcAccount')='C',lcAccount,'')
lcStore   = IIF(TYPE('lcStore')='C',lcStore,'')
DECLARE laData[1]  && array to get values from browse
STORE '' TO laData
llWasSel = .T.
llBrowse = IIF(TYPE('llBrowse') = 'U' , .T. , llBrowse) && variable to determine forcing browse or not
*B605625,1 RAE Change Amount to TotCredit in Returns Browse
*lcBrFields = [CrMemo:8,Account:7:H="Acct#",Store:12,CrDate:8,RaNo:8,]+;
             [Status:1:H="S",Reference:15:H="Ref.",Pieces:7,Amount:10,]+;
             [cWareCode:9:H="WareHouse",Invoice:8,Order:8,Reason:6,]+;
             [cDivision:8,Salesrep1:9,Salesrep2:9]

*B607391,1 KHM 07/06/2003 (Begin) Get the reason description instead of reason code.
*lcBrFields = [CrMemo:8,Account:7:H="Acct#",Store:12,CrDate:8,RaNo:8,]+;
             [Status:1:H="S",Reference:15:H="Ref.",Pieces:7,TotCredit:10:H="Amount",]+;
             [cWareCode:9:H="WareHouse",Invoice:8,Order:8,Reason:6,]+;
             [cDivision:8,Salesrep1:9,Salesrep2:9]

lcBrFields = [CrMemo:8,Account:7:H="Acct#",Store:12,CrDate:8,RaNo:8,]+;
             [Status:1:H="S",Reference:15:H="Ref.",Pieces:7,TotCredit:10:H="Amount",]+;
             [cWareCode:9:H="WareHouse",Invoice:8,Order:8,lcReason=gfCodDes(Reason,'REASON   '):30:H="Reason",]+;
             [cDivision:8,Salesrep1:9,Salesrep2:9]
*B607391,1 KHM 07/06/2003 (End)

*B605625,1 RAE
lnCurAlias = SELECT()
llOpenCrM = .F.
IF !USED("RETHDR")
  SELECT 0
  USE (gcDataDir+'RETHDR') IN 0 ORDER TAG RETHDRA
  llOpenCrM = .T.
ELSE
  SELECT RETHDR
  lcCrMOrder = TAG()
  SET ORDER TO TAG RETHDRA
ENDIF
=SEEK(lcAccount)

*-- HDM B802363,1 [Start] if inq. main account locate for all its stores
*LOCATE REST WHILE Account+CrMemo = lcAccount FOR Store = lcStore
LOCATE REST WHILE Account + CrMemo = lcAccount FOR Store = IIF(EMPTY(lcStore) , ALLTRIM(lcStore) , PADR(lcStore,8))
*-- HDM B802363,1 [End]

IF !FOUND()
  *E300455,1 Message : 40058
  *E300455,1 No Credit Memos found
  *E300455,1 Button : 00000
  *E300455,1 Ok
  =gfModalGen('TRM40058B00000','ALERT',IIF(EMPTY(lcAccount),'',;
  'for account: '+lcAccount)+IIF(EMPTY(lcStore),'',' store: '+lcStore))
  lcCrMemo = SPACE(6)
ELSE
  IF llBrowse .OR. !SEEK(lcAccount+lcCrMemo)
    lnSoftSeek = RECNO(0)
    IF lnSoftSeek > 0 .AND. lnSoftSeek <= RECCOUNT("RETHDR")
      GO lnSoftSeek
    ELSE
      GO TOP
    ENDIF
    
    *-- HDM B802363,1 [Start] include store code when browsin returns
    *llWasSel = ARIABROW("lcAccount","Credit Memos",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",'','CrMemo',"laData")
    
    *B802873,1 Select RetHdr & SET ORDER TO TAG Rethdr Before Call ARIABROW . [ Begin ]
    SELECT RETHDR
    lcCrMOrdr = TAG()
    SET ORDER TO TAG RETHDR
    llWasSel = ARIABROW("FOR ACCOUNT=lcAccount AND STORE=IIF(EMPTY(lcStore) , ALLTRIM(lcStore) , PADR(lcStore,8))","Credit Memos",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",'','CrMemo',"laData")
    SET ORDER TO TAG (lcCrMOrdr) IN RETHDR
    *B802873,1   [ End ]
    
    *-- HDM B802363,1 [End]
    
    lcCrMemo = IIF(llWasSel , laData[1] , SPACE(6))
  ENDIF
ENDIF
IF llOpenCrM
  USE IN RETHDR
ELSE
  SELECT RETHDR
  IF !EMPTY(lcCrMOrder)
    SET ORDER TO TAG (lcCrMOrder)
  ELSE
    SET ORDER TO
  ENDIF
ENDIF
IF llRetAlias
  SELECT (lnCurAlias)
ENDIF
RETURN (lcCrMemo)

*!*************************************************************
*! Name      : lfDepartments
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Add,Modify and remove Customer departments
*!*************************************************************
*! Calls     : gfOpenFile,ARCDEPT.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDepartments()
*!*************************************************************
FUNCTION lfDepartments

*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'CUSTDEPT',gcDataDir+'CUSTDEPT','SH')
*E301077,49 (End)

IF (laScrMode[3] .OR. laScrMode[4]) .AND. !USED(lcCustDept)
  SELECT * FROM CUSTDEPT WHERE ACCOUNT=laData[2] INTO DBF (gcWorkDir+lcCustDept)
  INDEX ON Account+Dept TAG (lcCustDept)
ENDIF
SELECT IIF(laScrMode[2],'CUSTDEPT',lcCustDept)
SCATTER MEMVAR
laCodes[2,2] = 'laDeptTerms'
laCodes[2,3] = 'lnDeptTerms'
laCodes[2,6] = .T.
laCodes[2,7] = IIF(laScrMode[2],'CUSTDEPT',lcCustDept)
laCodes[2,8] = IIF(laScrMode[2],'CUSTDEPT',lcCustDept)
laCodes[2,9] = 'laData[2]+m.Dept'
*B606017,1 TMI [Start] Fix the bug that A lost bmp file when open dept screen
lcKey     = gcBmpHome + "ExtKey.BMP"
*B606017,1 TMI [End  ] 
DO (gcScrDir+"ARCDEPT.SPX")
*E301077,49 Inhance openning files to speed up transaction
=gfCloseFile('CUSTDEPT')
*E301077,49 (End)

laCodes[2,2] = 'laTerms'
laCodes[2,3] = 'lnTerms'
laCodes[2,6] = .F.
laCodes[2,7] = 'CUSTOMER'
laCodes[2,8] = 'CUSTOMER'
laCodes[2,9] = 'laData[1]+laData[2]+laData[3]'

*!*************************************************************
*! Name      : lfDeptBrow
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse customer departments
*!*************************************************************
*! Calls     : gfCodDes,lfwBrow
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDeptBrow()
*!*************************************************************
FUNCTION lfDeptBrow

SELECT IIF(laScrMode[2],'CustDept',lcCustDept)
lnDMarker = RECNO()
RELEASE WINDOW (lcBrowDept)
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,;
       Dept      :H='Dept.' :R,;
       cDeptDesc :H='Name'  :R,;
       Rep1      :H='Rep1'  :R,;
       Comm1     :H='Comm1' :P= '99.99' :R,;
       Rep2      :H='Rep2'  :R,;
       Comm2     :H='Comm2' :P= '99.99' :R,;
       lcTerms =gfCodDes(cTermCode,'CTERMCODE') :H='Terms' :R;
       WINDOW ARCDEPT0   ;
       IN WINDOW ARCDEPT ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfwBrow()    ;
       KEY laData[2] ;
       TITLE lcBrowDept

*!*************************************************************
*! Name      : lfDDeptBrow
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Deactivate function for customer departments browse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDDeptBrow()
*!*************************************************************
FUNCTION lfDDeptBrow
IF WONTOP()=lcBrowDept
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTab WITH 'ARCDEPT1','m.Dept'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'ARCDEPT1','pbClsDept'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfwBrow
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Refresh customer departments screen
*!*************************************************************
*! Calls     : gfwCodePop,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwBrow()
*!*************************************************************
FUNCTION lfwBrow
lnDMarker = IIF(laScrMode[2],RECNO('CustDept'),RECNO(lcCustDept))
SHOW WINDOW (lcBrowDept) REFRESH SAME
SCATTER MEMVAR
=gfwCodePop(@laCodes,'CTERMCODE','T')
SHOW GETS WINDOW 'ARCDEPT1' ONLY
=lfRefresh('ARCDEPT1')

*!*************************************************************
*! Name      : lfvNewDept
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Add new customer department
*!*************************************************************
*! Calls     : gfwCodePop,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNewDept()
*!*************************************************************
FUNCTION lfvNewDept
SCATTER MEMVAR BLANK
SHOW GETS WINDOW 'ARCDEPT1' DISABLE ONLY
  
*E301569,1 Default Terms Code in Dept. screen to main account term code. [Begin]
*=gfwCodePop(@laCodes,'CTERMCODE','D')
=ACOPY(laTerms,laDeptTerms)
SHOW GET lnDeptTerms DISABLE
*E301569,1 Default Terms Code in Dept. screen to main account term code. [End]

=lfRefresh('ARCDEPT1')
SHOW GET m.Dept ENABLE

*!*************************************************************
*! Name      : lfvRemDept
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Remove customer department
*!*************************************************************
*! Calls     : gfModalGen,gfwCodePop,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemDept()
*!*************************************************************
FUNCTION lfvRemDept

*E300455,1 Message : 40054
*E300408,1 Are you sure you want to delete this department?
*E300455,1 Button : 40000
*E300455,1 Yes No
IF gfModalGen('QRM40054B40000','ALERT','department') = 1
  SELECT (lcCustDept)
  DELETE
  IF !SEEK(laData[2])
    SCATTER MEMVAR BLANK
    =gfwCodePop(@laCodes,'CTERMCODE','N')
    SHOW GETS WINDOW 'ARCDEPT1' DISABLE ONLY
    SHOW GET pbNewDept  ENABLE
    SHOW GET pbClsDept  ENABLE
    SHOW GET ibBrowDept ENABLE
  ELSE
    SCATTER MEMVAR
    lnDeptTerms = ASCAN(laDeptTerms,m.cTermCode)/2
    SHOW GETS WINDOW 'ARCDEPT1' ONLY
  ENDIF
  =lfRefresh('ARCDEPT1')
  SHOW WINDOW (lcBrowDept) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvDepartment
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Update customer department
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDepartment()
*!*************************************************************
FUNCTION lfvDepartment

SELECT (lcCustDept)
SHOW GET m.Dept DISABLE
IF EMPTY(m.Dept)
  IF SEEK(laData[2])
    m.Dept = Dept
  ELSE
    SHOW GET pbNewDept  ENABLE
    SHOW GET pbClsDept  ENABLE
    SHOW GET ibBrowDept ENABLE
    RETURN
  ENDIF
ENDIF
IF !SEEK(laData[2]+m.Dept)
  INSERT INTO (lcCustDept) (Account,Dept) VALUES (laData[2],m.Dept)
  
  *E301569,1 Default Terms Code in Dept. screen to main account term code. [Begin]
  =ACOPY(laTerms,laDeptTerms)
  *E301569,1 Default Terms Code in Dept. screen to main account term code. [End]  
ELSE
  SCATTER MEMVAR
  
  *E301569,1 Fill the array from the Dept record [Begin]
  =gfwCodePop(@laCodes,'CTERMCODE','T')  
  *E301569,1 Fill the array from the Dept record [End]
ENDIF
SHOW WINDOW (lcBrowDept) REFRESH SAME
SHOW GETS WINDOW 'ARCDEPT1' ENABLE ONLY
=lfRefresh('ARCDEPT1')
_CUROBJ = OBJNUM(m.cDeptDesc)

*!*************************************************************
*! Name      : lfvDeptRep
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate department sales reps
*!*************************************************************
*! Calls     : REPCHK,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDeptRep()
*!*************************************************************
FUNCTION lfvDeptRep
PRIVATE lcRep,lcSales

lcRep = 'm.'+SYS(18)
IF llBrowse .OR. (!EMPTY(&lcRep) .AND. !SEEK(&lcRep,'SalesRep'))
  &lcRep = IIF(llBrowse,SPACE(3),&lcRep)
  lcSales = &lcRep
  DO REPCHK WITH lcSales
  &lcRep = lcSales
ENDIF
SELECT (lcCustDept)
REPLACE Rep1 WITH m.Rep1 ,;
        Rep2 WITH m.Rep2

*B804095,1 Replace commissions for selected rep from Customer rep commission. [Begin]
PRIVATE lnDeptComm , lcDeptComm
IF RIGHT(lcRep,1) = "1"
  lcDeptComm = "Comm1"
  lnDeptComm = IIF(&lcRep == lcOldValue,m.Comm1,IIF(&lcRep=laData[19],laData[20],IIF(&lcRep=laData[21],laData[22],IIF(SEEK(&lcRep,'SalesRep'),SalesRep.Comm,0))))
  m.Comm1 = lnDeptComm
  
  SHOW GET m.Comm1 ENABLE
ELSE
  lcDeptComm = "Comm2"
  lnDeptComm = IIF(&lcRep == lcOldValue,m.Comm2,IIF(&lcRep=laData[19],laData[20],IIF(&lcRep=laData[21],laData[22],IIF(SEEK(&lcRep,'SalesRep'),SalesRep.Comm,0))))
  m.Comm2 = lnDeptComm

  SHOW GET m.Comm2 ENABLE
ENDIF

REPLACE &lcDeptComm WITH lnDeptComm
*B804095,1 Replace commissions for selected rep from Customer rep commission. [End]

=lfRefresh('ARCDEPT1')
llBrowse = .F.

*!*************************************************************
*! Name      : lfvDeptInfo
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Update department information
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDeptInfo()
*!*************************************************************
FUNCTION lfvDeptInfo
m.cTermCode = SUBSTR(laDeptTerms[lnDeptTerms,2],1,6)
SELECT (lcCustDept)
REPLACE Comm1     WITH m.Comm1 ,;
        Comm2     WITH m.Comm2 ,;
        cDeptDesc WITH m.cDeptDesc ,;
        cTermCode WITH m.cTermCode
SHOW WINDOW (lcBrowDept) REFRESH SAME

*!*************************************************************
*! Name      : lfFinancial
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Customer finanical
*!*************************************************************
*! Calls     : gfOpenFile,gfTempName,lfvAging,ARCUSFN.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfFinancial()
*!*************************************************************
FUNCTION lfFinancial
PARAMETERS llMulCurr,lcAgeType,lcAccount,lcAccName,lcStore

*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'CREDIT',gcDataDir+'CREDIT','SH')
=gfOpenFile(gcDataDir+'DEBIT',gcDataDir+'DEBIT','SH')
=gfOpenFile(gcDataDir+'ARHIST',gcDataDir+'ARHIST','SH')
*E301077,49 (End)

*C101427,1 Open PostDChq here instead of lfvPosted() to be availble [BEGIN]
*                  for lpShowPDC() 
=gfOpenFile(gcDataDir+'PostDChq','PostDChq','SH')
*C101427,1 Open PostDChq here instead of lfvPosted() to be availble [End  ]


*C101427,1 [BEGIN] Save lcbasefile which will be overwriten.
lcBaseFOld=lcBaseFile
*C101427,1 [End  ] Save lcbasefile which will be overwriten.

USE (gcDataDir+'ARHIST') AGAIN ALIAS ARHIST1 IN 0 ORDER TAG ARHISTHT
lcCurrWind = gcBaseWind
=gfStatic()
PRIVATE ALL EXCEPT G*
DECLARE laScrMode[5]

*C101427,1 thes arrays are defined in customer screen from which [BEGIN] 
*          we used to call Post Dated Checks screen now we need to
*          define them here in financial screen .
DIMENSION laField_H[1,1] , laField_N[1,1] , laBrowArr[1] , laFiltrExp[1,7] , laFilterExp[1,7]
STORE ' ' TO laField_H , laField_N ,  laBrowArr , laFiltrExp , laFilterExp
*C101427,1 thes arrays are defined in customer screen from which [End  ] 

STORE .T. TO laScrMode[2]
lcCusFin1 = gfTempName()
lcCusFin2 = gfTempName()
lcCusFin3 = gfTempName()
lcCusFin4 = gfTempName()
lcCusFin5 = gfTempName()
lcCusFin6 = gfTempName()
lcCusFin7 = gfTempName()
lcCusFin8 = gfTempName()
lcHistory = gfTempName()
lcCredit  = gfTempName()
lcDebit   = gfTempName()
lcfolder  = gfTempName()      && Folder Window Name

lcBrowseTl = ''
lcAccHist  = 'Customer History'
lcAccDebit = 'Debits'
lcAccCrd   = 'Credits'
llHistDet  = .F.
DECLARE lafolders[3,2],lafoldwinds[3,2]
STORE 'History' TO lafolders[1,1],lafoldwinds[1,1]
STORE 'Debits'  TO lafolders[2,1],lafoldwinds[2,1]
STORE 'Credit'  TO lafolders[3,1],lafoldwinds[3,1]
lafolders[1,2] = 1
lafolders[2,2] = 2
lafolders[3,2] = 3
lnnofld = 3
lcwfoldchng = '=lfCustChAct()'  && function to control shows after change the folder
lnfoldercend = 102.00
lnfolderrend = 2.000

lcfoldprnt  = 'ARCUSFN'         && window parent name for the folder

*C101427,1 Make the Debit folder is the default one  [BEGIN]
*lnactfolder = 1                 && active folder
lnactfolder = 2                 && active folder
*C101427,1 Make the Debit folder is the default one  [END  ]

lafoldwinds[1,2] = lcCusFin4
lafoldwinds[2,2] = lcCusFin5
lafoldwinds[3,2] = lcCusFin6

STORE 0 TO lnCur,lnAge1,lnAge2,lnAge3,lnAge4,lnAging,lnOpnCr,lnNet,;
           lnChgBack,lnNet_Cb
cbCurrency = .F.
puCurrency = 1
IF llMulCurr
  lcBaseCurr = '\<Display in '+gcBaseCurr
  *E301077,49 Inhance openning files to speed up transaction
  =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
  *E301077,49 (End)
  SELECT SYCCURR.CCURRCODE FROM (gcSysHome+'SYCCURR');
  ORDER BY SYCCURR.CCURRCODE INTO ARRAY laCurrency
  IF _TALLY > 0
    lnArrayLen = ALEN(laCurrency,1)
  ELSE
    lnArrayLen = 0
  ENDIF  
  DIMENSION laCurrency[lnArrayLen+1]
  laCurrency[lnArrayLen+1] = 'ALL'
  puCurrency = lnArrayLen+1
  cbCurrency  = .T.
ENDIF
=lfvAging('ALL',.T.)
lnHistory = 0
*B802388,1 ABD define the variable that will use when save last recored. [Begin]
lnLastRec = 1
*B802388,1 ABD [End]

*B802622,1 ABD Add filed transaction type to table to browse in the screen. [Begin]
*CREATE CURSOR (lcHistory) ;
*  (history C(6),histdate D,totdb N(11,2),totcr N(11,2),openamt N(11,2),;
*   cCurrCode C(5),nExRate N(9,4),nCurrUnit N(4))
CREATE CURSOR (lcHistory) ;
  (history C(6),histdate D,TranType C(1) , totdb N(11,2),totcr N(11,2),openamt N(11,2),;
   cCurrCode C(5),nExRate N(9,4),nCurrUnit N(4))
*B802622,1 ABD [End]
   
lnCredit = 0

*B802622,1 ABD Add filed transaction type to table to browse in the screen. [Begin]
*CREATE CURSOR (lcCredit) ;
*(tran C(6),batch C(6),trandate D,desc C(15),reference C(20),store C(8),amount N(11,2))
CREATE CURSOR (lcCredit) ;
(tran C(6),batch C(6),TranType C(1),trandate D,desc C(15),reference C(20),store C(8),amount N(11,2))
*B802622,1 ABD [End]

lnDebit = 0
*E300941,1 Add Installment# in the Debit Finantial Browse
CREATE CURSOR (lcDebit) ;
(tran C(6),batch C(6),trandate D,duedate D,desc C(15),reference C(20),;
 store C(8),amount N(11,2),TranType C(1),cInstalNo C(3))
lnOldCurr = 0
lcInvoice=''

*C101427,1 [BEGIN] give initial values to these variables [declare them ]
lcPostStat="DISABLE" &&-- Variable that control the apperance of <Post Dated Cheks> 
lcBaseFile=lcBaseFOld  && Restore original lcBaseFile
*C101427,1 [END  ] give initial values to these variables [declare them ]

*B130045,1 NNA 11/20/2005 (Begin) Update the Credit Available in the customer File
=lfUpdCrAv()
*B130045,1 NNA (End)

DO (gcScrDir+"ARCUSFN.SPX")

USE IN (lcHistory)
USE IN (lcCredit)
USE IN (lcDebit)
USE IN ARHIST1
SELECT CUSTOMER
RETURN(lcInvoice)

FUNCTION lfCallInv
PARAMETERS lcInvoice
IF !EMPTY(lcInvoice)
  DO gpDoProg WITH 'AWRARDINV',.F.,'AR',lcInvoice
  llFinancial = .T.
  lcCurrWind = gcBaseWind
  =gfStatic()
ELSE
  RELEASE WINDOW 'ARCUSFN'
ENDIF

*!*************************************************************
*! Name      : lfvAging
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Show customer financial 
*!*************************************************************
*! Calls     : gfGetExSin
*!*************************************************************
*! Passed Parameters  :  lcCurrency : Customer
*!                       llBaseCurr : Show in Base currency
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAging()
*!*************************************************************
FUNCTION lfvAging
PARAMETERS lcCurrency, llBaseCurr

lcUnitSign = '/' 
SELECT CUSTOMER
IF UPPER(lcCurrency) = 'ALL'
  lnCur     = IIF(lcAgeType='D',Current,TerCurrent)
  lnAge1    = IIF(lcAgeType='D',Age30,TerAge30)
  lnAge2    = IIF(lcAgeType='D',Age60,TerAge60)
  lnAge3    = IIF(lcAgeType='D',Age90,TerAge90)
  lnAge4    = IIF(lcAgeType='D',Age120,TerAge120)
  lnOpnCr   = OPENCR
  lnChgBack = CHGBACK
  lnNet     = NETBAL
  lnNet_Cb  = NETBAL-CHGBACK
  lnAging   = lnCur+lnAge1+lnAge2+lnAge3+lnAge4
ELSE
  STORE 0.00 TO lnCur,lnAge1,lnAge2,lnAge3,lnAge4,lnOpnCr,lnChgBack
  *Age debits
  SELECT DEBIT
  IF SEEK(lcAccount)
    SCAN REST WHILE Account = lcAccount FOR cCurrCode = lcCurrency
      lcRateSign = IIF(llMulCurr .AND. llBaseCurr,;
                       gfGetExSin(@lcUnitSign, cCurrCode),'/')
      lnExRate   = IIF(llMulCurr .AND. llBaseCurr,nExRate,1)
      lnCurrUnit = IIF(llMulCurr .AND. llBaseCurr,nCurrUnit,1)
      IF lcAgeType = 'T'
        * This computes the age of a transaction based on the tran
        * due date. The due date is computed based on the data in the code
        * file for terms.
        lnDays = Customer.AgeDate - IIF(EMPTY(DueDate),TranDate+30,DueDate)
        DO CASE
          CASE lnDays >= 91
            lnAge4 = lnAge4 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
          CASE lnDays >= 61
            lnAge3 = lnAge3 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
          CASE lnDays >= 31
            lnAge2 = lnAge2 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
          CASE lnDays >= 1
            lnAge1 = lnAge1 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
          OTHERWISE
            lnCur = lnCur + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
        ENDCASE
      ELSE
        * This computes the age of a transaction based on the tran date
        lnDays = Customer.AgeDate - Debit.TranDate
        DO CASE
          CASE lnDays >= 120
            lnAge4 = lnAge4 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
          CASE lnDays >= 90
            lnAge3 = lnAge3 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
          CASE lnDays >= 60
            lnAge2 = lnAge2 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
          CASE lnDays >= 30
            lnAge1 = lnAge1 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
          OTHERWISE
            lnCur = lnCur + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
        ENDCASE
      ENDIF    
      IF TranType = '3'
        lnChgBack = lnChgBack + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      ENDIF
    ENDSCAN
  ENDIF
  lnAging = lnCur+lnAge1+lnAge2+lnAge3+lnAge4
  *Aging credits
  IF SEEK(lcAccount,'CREDIT')
    SELECT CREDIT
    SCAN REST WHILE Account = lcAccount FOR cCurrCode = lcCurrency
      lcRateSign = IIF(llMulCurr .AND. llBaseCurr,;
                       gfGetExSin(@lcUnitSign, cCurrCode),'/')
      lnExRate   = IIF(llMulCurr .AND. llBaseCurr,nExRate,1)
      lnCurrUnit = IIF(llMulCurr .AND. llBaseCurr,nCurrUnit,1)
      lnOpnCr = lnOpnCr + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
    ENDSCAN
  ENDIF
  lnNet    = lnAging + lnOpnCr
  lnNet_Cb = lnNet   - lnChgBack
ENDIF

*!*************************************************************
*! Name      : lfCustChAct
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Customer financial folders
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCustChAct()
*!*************************************************************
FUNCTION lfCustChAct

DO CASE
  CASE lnActFolder = 1
    SHOW GET pbHistDet ENABLE
    IF llHistDet
      SHOW GET pbHistDet,1 ENABLE PROMPT '\<History'
    ELSE
      SHOW GET pbHistDet,1 ENABLE PROMPT '\<Details'
    ENDIF    
  CASE lnActFolder = 2
    SHOW GET pbHistDet,1 ENABLE PROMPT '\<Details'
  CASE lnActFolder = 3
    SHOW GET pbHistDet,1 DISABLE PROMPT '\<Details'
ENDCASE

*!*************************************************************
*! Name      : lfDCusFn
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Customer financial screen deactivate function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDCusFn()
*!*************************************************************
FUNCTION lfDCusFn
IF WONTOP()=lcAccHist .OR. WONTOP()=lcAccCrd .OR. WONTOP()=lcAccDebit
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  *B802390,1 Fix tab function in customer finisntial screen
  *ON KEY LABEL TAB     DO lpTab WITH 'ARCUSFN0','pbClsCusFn'
  ON KEY LABEL TAB     DO lpTab WITH 'ARCUSFN0','pbAge'
  *B802390,1 (End)
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'ARCUSFN0','pbClsCusFn'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfFinBrow
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse Customer financials (Debits, Credit and History)
*!*************************************************************
*! Calls     : gfGetExSin,lfShowFin
*!*************************************************************
*! Passed Parameters  :  lcCurrency : Customer
*!                       llBaseCurr : Show in Base currency
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfFinBrow()
*!*************************************************************
FUNCTION lfFinBrow
PARAMETERS lcCurrency,llBaseCurr

*History
SELECT (lcHistory)
ZAP   
SELECT ARHIST 
SET ORDER TO TAG ARHIST IN ARHIST
=SEEK(lcAccount)
IF llMulCurr .AND. lcCurrency <> 'ALL'
  LOCATE REST WHILE ACCOUNT = lcAccount FOR cCurrCode = lcCurrency
ENDIF
IF FOUND()
  lcUnitSign = '/'
  SELECT ARHIST
  SCAN REST WHILE ACCOUNT = lcAccount ;
       FOR IIF(!llMulCurr .OR. lcCurrency = 'ALL',.T.,cCurrCode = lcCurrency)
    lcRateSign = IIF(llMulCurr .AND. llBaseCurr,;
                     gfGetExSin(@lcUnitSign, ARHIST.cCurrCode),'/')
    lnExRate  = IIF(llMulCurr .AND. llBaseCurr,ARHIST.nExRate,1)
    lnCurrUnit = IIF(llMulCurr .AND. llBaseCurr,ARHIST.nCurrUnit,1)
    
    *B802622,1 ABD Add filed transaction type to table to browse in the screen. [Begin]
    *INSERT INTO (lcHistory) ;
    *            (history,histdate,totdb,totcr,openamt,cCurrCode,nExRate,nCurrUnit);
    *  VALUES    (ARHIST.history,ARHIST.histdate,;
    *            ARHIST.totdb &lcRateSign lnExRate &lcUnitSign lnCurrUnit ,;
    *            ARHIST.totcr &lcRateSign lnExRate &lcUnitSign lnCurrUnit ,;
    *            ARHIST.openamt &lcRateSign lnExRate &lcUnitSign lnCurrUnit,;
    *            ARHIST.cCurrCode,ARHIST.nExRate,ARHIST.nCurrUnit)
                
    INSERT INTO (lcHistory) ;
                (history,histdate,TranType,totdb,totcr,openamt,cCurrCode,nExRate,nCurrUnit);
      VALUES    (ARHIST.history,ARHIST.histdate,ARHIST.TranType,;
                ARHIST.totdb &lcRateSign lnExRate &lcUnitSign lnCurrUnit ,;
                ARHIST.totcr &lcRateSign lnExRate &lcUnitSign lnCurrUnit ,;
                ARHIST.openamt &lcRateSign lnExRate &lcUnitSign lnCurrUnit,;
                ARHIST.cCurrCode,ARHIST.nExRate,ARHIST.nCurrUnit)
    *B802622,1 [End]
  ENDSCAN
ENDIF

*Credit
SELECT (lcCredit)
ZAP
SELECT CREDIT
=SEEK(lcAccount)
IF llMulCurr .AND. lcCurrency <> 'ALL'
  LOCATE REST WHILE ACCOUNT = lcAccount FOR cCurrCode = lcCurrency
ENDIF
IF FOUND()
  lcUnitSign = '/'
  SCAN REST WHILE ACCOUNT = lcAccount ;
       FOR IIF(!llMulCurr .OR. lcCurrency = 'ALL',.T.,cCurrCode = lcCurrency)
    lcRateSign = IIF(llMulCurr .AND. llBaseCurr,;
                     gfGetExSin(@lcUnitSign, CREDIT.cCurrCode),'/')
    lnExRate  = IIF(llMulCurr .AND. llBaseCurr,CREDIT.nExRate,1)
    lnCurrUnit = IIF(llMulCurr .AND. llBaseCurr,CREDIT.nCurrUnit,1)
    *B802622,1 ABD Add filed transaction type to table to browse in the screen. [Begin]
    *INSERT INTO (lcCredit) ;
    *            (tran,batch,trandate,desc,reference,store,amount);
    *  VALUES    (CREDIT.tran,CREDIT.batch,CREDIT.trandate,CREDIT.desc,;
    *            CREDIT.reference,CREDIT.store,;
    *            CREDIT.amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit)
    
    INSERT INTO (lcCredit) ;
                (tran,batch,Trantype,trandate,desc,reference,store,amount);
      VALUES    (CREDIT.tran,CREDIT.batch,CREDIT.Trantype,CREDIT.trandate,CREDIT.desc,;
                CREDIT.reference,CREDIT.store,;
                CREDIT.amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit)
    *B802622,1 ABD [End]
  ENDSCAN
ENDIF

**DEBIT
SELECT (lcDebit)
ZAP
SELECT DEBIT
=SEEK(lcAccount)
LOCATE REST WHILE ACCOUNT = lcAccount ;
FOR IIF(llMulCurr .AND. lcCurrency <> 'ALL',cCurrCode=lcCurrency,.T.) .AND. ;
    IIF(!EMPTY(lcStore),Store = lcStore,.T.)
IF FOUND()
  lcUnitSign = '/'
  SCAN REST WHILE ACCOUNT = lcAccount ;
  FOR IIF(!llMulCurr .OR. lcCurrency = 'ALL',.T.,cCurrCode = lcCurrency) .AND. ;
      IIF(!EMPTY(lcStore),Store = lcStore,.T.)
    lcRateSign = IIF(llMulCurr .AND. llBaseCurr,;
                     gfGetExSin(@lcUnitSign, DEBIT.cCurrCode),'/')
    lnExRate  = IIF(llMulCurr .AND. llBaseCurr,DEBIT.nExRate,1)
    lnCurrUnit = IIF(llMulCurr .AND. llBaseCurr,DEBIT.nCurrUnit,1)
    *E300941,1 Add Installment# in the Debit Finantial Browse
    INSERT INTO (lcDebit) ;
                (tran,batch,trandate,duedate,desc,reference,store,amount,TranType,cInstalNo);
      VALUES    (DEBIT.tran,DEBIT.batch,DEBIT.trandate,DEBIT.duedate,;
                 DEBIT.desc,DEBIT.reference,DEBIT.store,;
                 DEBIT.amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit,DEBIT.TranType,DEBIT.cInstalNo)
  ENDSCAN
ENDIF

SELECT (lcHistory)
GO TOP
lnHistory = RECNO()
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnHistory,'>',' '):H=' ':R:1:W=.F.,;
       History :H='History'      :R,;
       HistDate:H='Key-off Date' :R,;
       TotDb   :H='Total Debits' :R,;
       TotCr   :H='Total Credits':R,;
       OpenAmt :H='Open Amount'  :R;
       WINDOW (lcCusFin1);
       IN WINDOW (lcCusFin4);
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       WHEN lfCusHist() ;
       TITLE lcAccHist
SELECT (lcDebit)
GO TOP
lnDebit = RECNO()
*E300941,1 Add Installment# in the Debit Finantial Browse
*B603189,1 BWA 10/05/1999 Fix the bug of the Amount field [START]
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnDebit,'>',' '):H=' ':R:1:W=.F.,;
       Tran      :H='Debit'       :R,;
       cInstalNo :H='Inst.#'      :R,;
       Batch     :H='Batch'       :R,;
       TranDate  :H='Date'        :R,;
       DueDate   :H='Date Due'    :R,;
       Desc      :H='Description' :R,;
       Reference :H='Reference'   :R,;
       Store     :H='Store'       :R,;
       Amount    :H='Amount' :P='9999999.99' :R;
       WINDOW (lcCusFin2);
       IN WINDOW (lcCusFin5);
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       WHEN lfCusHist() ;
       TITLE lcAccDebit
*E301429,1 (Start) Increase the amount field in the debit browse in financial screen.
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnDebit,'>',' '):H=' ':R:1:W=.F.,;
       Tran      :H='Debit'       :R   ,;
       cInstalNo :H='Inst.#'      :R   ,;
       Batch     :H='Batch'       :R   ,;
       TranDate  :H='Date'        :R   ,;
       DueDate   :H='Date Due'    :R   ,;
       Desc      :H='Description' :R   ,;
       Reference :H='Reference'   :R:15,;
       Store     :H='Store'       :R   ,;
       Amount    :H='Amount' :P='9999999.99' :R;
       WINDOW (lcCusFin2);
       IN WINDOW (lcCusFin5);
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       WHEN lfCusHist() ;
       TITLE lcAccDebit
       
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnDebit,'>',' '):H=' ':R:1:W=.F.,;
       Tran      :H='Debit'       :R   ,;
       cInstalNo :H='Inst.#'      :R   ,;
       Batch     :H='Batch'       :R   ,;
       TranDate  :H='Date'        :R   ,;
       DueDate   :H='Date Due'    :R   ,;
       Desc      :H='Description' :R   ,;
       Reference :H='Reference'   :R:15,;
       Store     :H='Store'       :R   ,;
       Amount    :H='Amount' :P='99999999999.99' :R;
       WINDOW (lcCusFin2);
       IN WINDOW (lcCusFin5);
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       WHEN lfCusHist() ;
       TITLE lcAccDebit
*B802622,1 ABD Add filed transaction type to table to browse in the screen. [Begin]
*B605573,1 TMI [START] Move the amount field to be visible within arcusfn.scx without scrolling.
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnDebit,'>',' '):H=' ':R:1:W=.F.,;
       Tran      :H='Debit'       :R                        ,;
       TranType  =lfvDesType():H='Adjustment Reason'        ,;              
       cInstalNo :H='Inst.#'      :R                        ,;
       Batch     :H='Batch'       :R                        ,;
       TranDate  :H='Date'        :R                        ,;
       DueDate   :H='Date Due'    :R                        ,;
       Desc      :H='Description' :R                        ,;
       Reference :H='Reference'   :R:15                     ,;
       Store     :H='Store'       :R                        ,;
       Amount    :H='Amount' :P='99999999999.99' :R          ;
       WINDOW (lcCusFin2)                                    ;
       IN WINDOW (lcCusFin5)                                 ;
       NOMENU                                                ;
       NOAPPEND                                              ;
       NODELETE                                              ;
       NOWAIT                                                ;
       SAVE                                                  ;
       NOCLEAR                                               ;
       WHEN lfCusHist()                                      ;
       TITLE lcAccDebit
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnDebit,'>',' '):H=' ':R:1:W=.F.,;
       Tran      :H='Debit'       :R                        ,;
       TranType  =lfvDesType():H='Adjustment Reason'        ,;              
       cInstalNo :H='Inst.#'      :R                        ,;
       Batch     :H='Batch'       :R                        ,;
       TranDate  :H='Date'        :R                        ,;
       DueDate   :H='Date Due'    :R                        ,;
       Desc      :H='Description' :R                        ,;
       Amount    :H='Amount' :P='99999999999.99' :R         ,;
       Store     :H='Store'       :R                        ,;
       Reference :H='Reference'   :R:15                      ;
       WINDOW (lcCusFin2)                                    ;
       IN WINDOW (lcCusFin5)                                 ;
       NOMENU                                                ;
       NOAPPEND                                              ;
       NODELETE                                              ;
       NOWAIT                                                ;
       SAVE                                                  ;
       NOCLEAR                                               ;
       WHEN lfCusHist()                                      ;
       TITLE lcAccDebit
*B605573,1 TMI [END  ] Move the amount field to be visible within arcusfn.scx without scrolling.
*B802622,1 ABD [End]
*E301429,1 (End)  
*B603189,1 BWA 10/05/1999 [END]

SELECT (lcCredit)
GO TOP
lnCredit = RECNO()
*E301429,1 (Start) Increase the amount field in the Credit browse in financial screen.
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnCredit,'>',' '):H=' ':R:1:W=.F.,;
       Tran   :H='Credit'  :R,;
       Batch  :H='Batch'   :R,;
       TranDate :H='Date'  :R,;
       Desc    :H='Description' :R,;
       Reference :H='Reference' :R,;
       Store  :H='Chk/Str#.' :R,;
       Amount :H='Amount' :P='99999999.99' :R;
       WINDOW (lcCusFin3);
       IN WINDOW (lcCusFin6);
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       WHEN lfCusHist() ;
       TITLE lcAccCrd

*- Add trantype to browse.
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnCredit,'>',' '):H=' ':R:1:W=.F.,;
       Tran   :H='Credit'  :R,;
       Batch  :H='Batch'   :R,;
       TranDate :H='Date'  :R,;
       Desc    :H='Description' :R,;
       Reference :H='Reference' :R,;
       Store  :H='Chk/Str#.' :R,;
       Amount :H='Amount' :P='99999999999.99' :R;
       WINDOW (lcCusFin3);
       IN WINDOW (lcCusFin6);
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       WHEN lfCusHist() ;
       TITLE lcAccCrd

*B605573,1 TMI [START] Move the amount field to be visible within arcusfn.scx without scrolling.
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnCredit,'>',' '):H=' ':R:1:W=.F.,;
       Tran      :H='Credit'  :R                             ,;
       Batch     :H='Batch'   :R                             ,;
       TranType  =lfvDesType():H='Adjustment Reason'         ,;
       TranDate  :H='Date'    :R                             ,;
       Desc      :H='Description':R                          ,;
       Reference :H='Reference'  :R                          ,;
       Store     :H='Chk/Str#.'  :R                          ,;
       Amount    :H='Amount'     :P='99999999999.99' :R       ;
       WINDOW (lcCusFin3);
       IN WINDOW (lcCusFin6);
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       WHEN lfCusHist() ;
       TITLE lcAccCrd
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnCredit,'>',' '):H=' ':R:1:W=.F.,;
       Tran      :H='Credit'  :R                             ,;
       Batch     :H='Batch'   :R                             ,;
       TranType  =lfvDesType():H='Adjustment Reason'         ,;
       TranDate  :H='Date'    :R                             ,;
       Desc      :H='Description':R                          ,;
       Store     :H='Chk/Str#.'  :R                          ,;
       Amount    :H='Amount'     :P='99999999999.99' :R      ,;
       Reference :H='Reference'  :R                           ;
       WINDOW (lcCusFin3);
       IN WINDOW (lcCusFin6);
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       WHEN lfCusHist() ;
       TITLE lcAccCrd
*B605573,1 TMI [END  ] Move the amount field to be visible within arcusfn.scx without scrolling.

*E301429,1 (End)  
*!*************************************************************
*! Name      : lfCusHist
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Refresh Customer history
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCusHist()
*!*************************************************************
FUNCTION lfCusHist
DO CASE
  CASE lnActFolder = 1
    lnHistory = IIF(llHistDet,RECNO('ARHIST1'),RECNO(lcHistory))
    SHOW WINDOW (lcAccHist) REFRESH SAME
  CASE lnActFolder = 2
    lnDebit = RECNO(lcDebit)
    SHOW WINDOW (lcAccDebit) REFRESH SAME
  CASE lnActFolder = 3
    lnCredit = RECNO(lcCredit)
    SHOW WINDOW (lcAccCrd) REFRESH SAME
ENDCASE

*!*************************************************************
*! Name      : lfwCurrency
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Store old currency
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwCurrency()
*!*************************************************************
FUNCTION lfwCurrency
lnOldCurr = puCurrency

*!*************************************************************
*! Name      : lfvCurrency
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Show customer financial in a specefic currency
*!*************************************************************
*! Calls     : lfvAging,lfFinBrow,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCurrency()
*!*************************************************************
FUNCTION lfvCurrency
IF puCurrency <> lnOldCurr
  IF laCurrency[puCurrency] = 'ALL' .OR. laCurrency[puCurrency] = gcBaseCurr
    cbCurrency = .T.
    SHOW GET cbCurrency DISABLE
  ELSE
    SHOW GET cbCurrency ENABLE
  ENDIF
  =lfvAging(laCurrency[puCurrency],cbCurrency)
  =lfFinBrow(laCurrency[puCurrency],cbCurrency)
  =lfRefresh('ARCUSFN0')
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'ARDINV0', 'pbClose'
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackTab WITH 'ARDINV0', 'pbClose'
*!*************************************************************
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lfvBillAddr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate store billing address
*!*************************************************************
*! Calls     : gfModalGen,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBillAddr()
*!*************************************************************
FUNCTION lfvBillAddr
DO CASE
  CASE lnBillto = 1
    laData[50] = "M"
    =SEEK('M'+laData[2],'Customer')
    laData[43] = Customer.BtName
    laData[70] = Customer.ccont_cod2
    laData[44] = Customer.caddress12
    laData[45] = Customer.caddress22
    laData[46] = Customer.caddress32
    laData[47] = Customer.caddress42
    laData[48] = Customer.caddress52
    laData[49] = Customer.caddress62
    SHOW GET pbFromSt DISABLE
    *B603593,1 MHM 10/02/2000 [Start]
    SHOW GET laData[43] DISABLE
    SHOW GET laData[44] DISABLE
    SHOW GET laData[45] DISABLE
    SHOW GET laData[46] DISABLE
    SHOW GET laData[47] DISABLE
    SHOW GET laData[48] DISABLE
    SHOW GET laData[49] DISABLE
    SHOW GET ibAddress2 DISABLE
    SHOW GET lcAddHed2  DISABLE
    *B603593,1 MHM 10/02/2000 [End]
    =SEEK(laData[1]+laData[2]+laData[3],'Customer')
    SHOW GETS WINDOW (lcWinCh2) ONLY
    =lfRefresh(lcWinCh2)
  CASE lnBillto = 2
    IF EMPTY(laData[7]) .OR. EMPTY(laData[9]) .OR. EMPTY(laData[10]) .OR. ;
       EMPTY(laData[11]) .OR. EMPTY(laData[12])
      *E300455,1 Message : 40050
      *E300408,1 One or more address fields is empty. Please add all the address fields.
      *E300455,1 Button : 00000
      *E300455,1 Ok
      =gfModalGen('TRM40050B00000','ALERT')
      lnBillto = lcOldValue
      SHOW GET lnBillto
      RETURN
    ENDIF
    laData[50] = "S"
    laData[43] = laData[4]
    laData[70] = laData[23]
    laData[44] = laData[7]
    laData[45] = laData[8]
    laData[46] = laData[9]
    laData[47] = laData[10]
    laData[48] = laData[11]
    laData[49] = laData[12]
    SHOW GET pbFromSt DISABLE
    *B603593,1 MHM 10/02/2000 [Start]
    SHOW GET laData[43] DISABLE
    SHOW GET laData[44] DISABLE
    SHOW GET laData[45] DISABLE
    SHOW GET laData[46] DISABLE
    SHOW GET laData[47] DISABLE
    SHOW GET laData[48] DISABLE
    SHOW GET laData[49] DISABLE
    SHOW GET ibAddress2 DISABLE
    SHOW GET lcAddHed2  DISABLE
    *B603593,1 MHM 10/02/2000 [End]
    SHOW GETS WINDOW (lcWinCh2) ONLY
    =lfRefresh(lcWinCh2)
  CASE lnBillto = 3
    laData[50] = "A"
    SHOW GET pbFromSt ENABLE
    *B603593,1 MHM 10/02/2000 [Start]
    SHOW GET laData[43] ENABLE
    SHOW GET laData[44] ENABLE
    SHOW GET laData[45] ENABLE
    SHOW GET laData[46] ENABLE
    SHOW GET laData[47] ENABLE
    SHOW GET laData[48] ENABLE
    SHOW GET laData[49] ENABLE
    SHOW GET ibAddress2 ENABLE
    SHOW GET lcAddHed2  ENABLE
    *B603593,1 MHM 10/02/2000 [End]
ENDCASE

*!*************************************************************
*! Name      : lfvCopyTO
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Copy store information to another store
*!*************************************************************
*! Calls     : ARCOPYST.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCopyTO()
*!*************************************************************
FUNCTION lfvCopyTO
PRIVATE lcCopyStore

SELECT CUSTOMER
STORE SPACE(8) TO lcCopyStore
DO (gcScrDir+"ARCOPYST.SPX")
=SEEK(laData[1]+laData[2]+laData[3],'Customer')

*!*************************************************************
*! Name      : lfvCopyStore
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Copy to store
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCopyStore()
*!*************************************************************
FUNCTION lfvCopyStore
IF EMPTY(lcCopyStore) .OR. ('?' $ lcCopyStore ) .OR. LASTKEY()<> 13
  RETURN
ENDIF
IF lcCopyStore = laData[3]
  *E300455,1 Message : 40043
  *E300455,1 You cannot copy store data onto itself.
  *E300455,1 Button : 00000
  *E300455,1 Ok
  =gfModalGen('TRM40043B00000','ALERT')
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*E300455,1 Message : 40044
*E300455,1 Store XXXXXXXX not found. Do you want to add a new one with the 
*E300455,1 current store data?
*E300455,1 Button : 40000
*E300455,1 Yes No
IF !SEEK('S'+laData[2]+lcCopyStore,'Customer') 
  IF gfModalGen('QRM40044B40000','ALERT',lcCopyStore)= 1
    APPEND BLANK
    =RLOCK()
    GATHER FROM laData FIELDS &lcScFields   
    REPLACE Store WITH lcCopyStore
    *B123164,1 NNA 07/12/2004 (Begin) empty the class field for the account stores because it's not
    *B123164,1 NNA            correct to have data because it assigned auto. to the main Account
    *-- When we copy a store information to a new store it save the Main account Class to the 
    *-- New Store and this isn't correct so I Empty the Class for the copied Store
    REPLACE CLASS WITH ''     
    *B123164,1 NNA (End)

    UNLOCK
  ENDIF
ELSE
  *E300455,1 Message : 40045
  *E300455,1 Store XXXXXXXX already exists. Do you want to overwrite it with 
  *E300455,1 the current store data?
  *E300455,1 Button : 40000
  *E300455,1 Yes No
  IF gfModalGen('QRM40045B40000','ALERT',lcCopyStore)= 1
    =RLOCK()
    GATHER FROM laData FIELDS &lcScFields   
    REPLACE Store WITH lcCopyStore

    *B123164,1 NNA 07/12/2004 (Begin) empty the class field for the account stores because it's not
    *B123164,1 NNA            correct to have data because it assigned auto. the main Account
    *-- When we copy a store information to another exist store it save the Main account Class to  
    *-- that Store and this isn't correct so I empty the Class for the Copied Store
    REPLACE CLASS WITH ''     
    *B123164,1 NNA (End)

    UNLOCK
  ENDIF
ENDIF
STORE SPACE(8) TO lcCopyStore
_CUROBJ = _CUROBJ

*!*************************************************************
*! Name      : lfvCopyFrom
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Copy store billing address from another store
*!*************************************************************
*! Calls     : CUSBROWS,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCopyFrom()
*!*************************************************************
FUNCTION lfvCopyFrom
PRIVATE xStore
xStore = SPACE(8)
IF CUSBROWS(laData[2],.T.)
  laData[43] = Customer.BtName
  laData[70] = Customer.ccont_cod2
  laData[44] = Customer.caddress12
  laData[45] = Customer.caddress22
  laData[46] = Customer.caddress32
  laData[47] = Customer.caddress42
  laData[48] = Customer.caddress52
  laData[49] = Customer.caddress62
  SHOW GETS WINDOW (lcWinCh2) ONLY
  =lfRefresh(lcWinCh2)
  _CUROBJ = OBJNUM(laData[43])
ENDIF

*!*************************************************************
*! Name      : lpSavScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Save new Account/Store
*!*************************************************************
*! Calls     : gfModalGen,lfClsTmpFiles
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
PRIVATE lcDelStat

IF EMPTY(laData[4])
  *E300455,1 Message : 40051
  *E300408,1 Name field should not be left empty. Please add the name.
  *E300455,1 Button : 00000
  *E300455,1 Ok
  =gfModalGen('TRM40051B00000','ALERT')
  _CUROBJ  = OBJNUM(laData[4])
  llCSave = .F.
  RETURN
ENDIF
*B602550,1 Remove force address fields entry
*STORE ' ' TO lcAddrObj
*DO CASE
*  CASE EMPTY(laData[7])
*    lcAddrObj  = "laData[7]"
*  CASE EMPTY(laData[9])
*    lcAddrObj  = "laData[9]"
*  CASE EMPTY(laData[10])
*    lcAddrObj  = "laData[10]"
*  CASE EMPTY(laData[11])
*    lcAddrObj  = "laData[11]"
*  CASE EMPTY(laData[12])
*    lcAddrObj  = "laData[12]"
*ENDCASE
*IF !EMPTY(lcAddrObj)
*  *E300455,1 Message : 40050
*  *E300408,1 One or more address fields is empty. Please add all the address fields.
*  *E300455,1 Button : 00000
*  *E300455,1 Ok
*  =gfModalGen('TRM40050B00000','ALERT')
*  _CUROBJ  = OBJNUM(&lcAddrObj)
*  llCSave = .F.
*  RETURN
*ENDIF
*IF EMPTY(laData[19])
*  *E300455,1 Message : 40052
*  *E300408,1 Must enter a salesrep code.
*  *E300455,1 Button : 00000
*  *E300455,1 Ok
*  =gfModalGen('TRM40052B00000','ALERT')
*  IF lnActFolder = 1
*    _CUROBJ  = OBJNUM(laData[19])
*  ENDIF
*  llCSave = .F.
*  RETURN
*ENDIF
*B602550,1 (End)

IF llMulCurr AND EMPTY(laData[38])
  *E300455,1 Message : 40053
  *E300408,1 You have to enter the currency code.
  *E300455,1 Button : 00000
  *E300455,1 Ok
  =gfModalGen('TRM40053B00000','ALERT')
  _CUROBJ=OBJNUM(laData[38])
  llCSave = .F.
  RETURN
ENDIF

SELECT CUSTOMER
IF laData[1] = "M"
  laData[43] = laData[4]
  laData[44] = laData[7]
  laData[45] = laData[8]
  laData[46] = laData[9]
  laData[47] = laData[10]
  laData[48] = laData[11]
  laData[49] = laData[12]
  laData[70] = laData[23]
  IF laScrMode[3] .AND. ;
     (Customer.StName    <> laData[4]  .OR. Customer.cAddress1 <> laData[7]  .OR. ;
      Customer.cAddress2 <> laData[8]  .OR. Customer.cAddress3 <> laData[9]  .OR. ;
      Customer.cAddress4 <> laData[10] .OR. Customer.cAddress5 <> laData[11] .OR. ;
      Customer.cAddress6 <> laData[12] .OR. Customer.ccont_cod2 <> laData[23]) .AND. ;
      SEEK('S'+laData[2])
    *E300455,1 Message : 40046
    *E300455,1 One or more address data was changed. Do you want to update
    *E300455,1 the billing address for account stores if they are billing to
    *E300455,1 master address?
    *E300455,1 Button : 40000
    *E300455,1 Yes No
    IF gfModalGen('QRM40046B40000','ALERT') = 1
      SCAN REST WHILE TYPE+ACCOUNT+STORE = "S"+laData[2] FOR BillTo = "M"
        REPLACE  Btname     WITH laData[4]  ,;
                 caddress12 WITH laData[7]  ,;
                 caddress22 WITH laData[8]  ,;
                 caddress32 WITH laData[9]  ,;
                 caddress42 WITH laData[10] ,;
                 caddress52 WITH laData[11] ,;
                 caddress62 WITH laData[12] ,;
                 ccont_cod2 WITH laData[23]
        *E300872,8 Call Trace Key function.
        =gfTraceKey('Customer','S'+laData[2]+Store,'M')
        *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
        IF ASCAN(laEvntTrig , PADR('CUSTOMER_M',10)) <> 0
          =gfDoTriger('ARCUST',PADR('CUSTOMER_M',10))
        ENDIF
	    *C200597,1  TMI [End  ] 
      ENDSCAN
    ENDIF
  ENDIF
ENDIF  
lcDelStat = SET('DELETE')
SET DELETE OFF
IF USED(lcCustDept)
  *E301077,49 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'CUSTDEPT',gcDataDir+'CUSTDEPT','SH')
  *E301077,49 (End)
  SELECT (lcCustDept)
  SCAN
    =SEEK(Account+Dept,'CustDept')
    DO CASE
      CASE FOUND('CustDept') .AND. DELETED(lcCustDept)
        SELECT CUSTDEPT
        *E300872,8 Call Trace Key function.
        =gfTraceKey('CUSTDEPT',Account+Dept,'D')
        DELETE
      
      *B604520,1 Check if record is not deleted in CustDept file. [Begin]
      *CASE FOUND('CustDept') .AND. !DELETED(lcCustDept)
      *  SCATTER MEMVAR
      *  SELECT CUSTDEPT
      *  GATHER MEMVAR
      *  *E300872,8 Call Trace Key function.
      *  =gfTraceKey('CUSTDEPT',m.Account+m.Dept,'M')
      CASE FOUND('CustDept') .AND. !DELETED(lcCustDept) AND !DELETED('CustDept')
        SCATTER MEMVAR
        SELECT CUSTDEPT
        GATHER MEMVAR
        =gfTraceKey('CUSTDEPT',m.Account+m.Dept,'M')

      CASE FOUND('CustDept') .AND. !DELETED(lcCustDept) AND DELETED('CustDept')
        SCATTER MEMVAR
        SELECT CUSTDEPT
        GATHER MEMVAR
        =RLOCK()
        RECALL
        UNLOCK
      *B604520,1 Check if record is not deleted in CustDept file. [Begin]

      CASE !FOUND('CustDept') .AND. !DELETED(lcCustDept)
        SCATTER MEMVAR
        INSERT INTO CUSTDEPT FROM MEMVAR
        *E300872,8 Call Trace Key function.
        =gfTraceKey('CUSTDEPT',m.Account+m.Dept,'A')
    ENDCASE
  ENDSCAN
  *E301077,49 Inhance openning files to speed up transaction
  =gfCloseFile('CUSTDEPT')
  *E301077,49 (End)
ENDIF

*B604505,1 Delete all records in master Contact file before updating. [Begin]
*IF USED(lcContFile)
*  SELECT (lcContFile)
*  SCAN
*    =SEEK(cContType+cCont_Id+Store+Contact,'CONTACT')
*    DO CASE
*      CASE FOUND('CONTACT') AND DELETED(lcContFile)
*        SELECT CONTACT
*        *E300872,8 Call Trace Key function.
*        =gfTraceKey('CONTACT',cContType+cCont_Id+Store+Contact,'D')
*        DELETE
*      CASE FOUND('CONTACT') AND !DELETED(lcContFile)
*        SCATTER MEMVAR MEMO
*        SELECT CONTACT
*        GATHER MEMVAR MEMO
*        *E300872,8 Call Trace Key function.
*        =gfTraceKey('CONTACT',cContType+cCont_Id+Store+Contact,'M')
*      CASE !FOUND('CONTACT') AND !DELETED(lcContFile)
*        SCATTER MEMVAR MEMO
*        INSERT INTO CONTACT FROM MEMVAR
*        *E300872,8 Call Trace Key function.
*        =gfTraceKey('CONTACT',m.cContType+m.cCont_Id+m.Store+m.Contact,'A')
*    ENDCASE
*  ENDSCAN
*ENDIF

IF USED(lcContFile)
  GO TOP IN (lcContFile)
  *--- SSH : Fix the seek excpression to take stores into consedirations.
  *IF SEEK(&lcContFile..cContType+&lcContFile..cCont_Id,'CONTACT')
  IF SEEK(&lcContFile..cContType+&lcContFile..cCont_Id+laData[3],'CONTACT')
  *--- SSH : END
    SELECT Contact
    DELETE REST WHILE gfTraceKey('CONTACT',cContType+cCont_Id+Store+Contact,'D') ;
                  AND cContType+cCont_Id+Store +Contact = "C" + &lcContFile..cCont_Id+laData[3]
  ENDIF
 
  SELECT (lcContFile)
  *--- SSH : Change the FOR Excepretion.
  *SCAN  FOR cContType+cCont_Id+Store + Contact = "C" + laData[2] + laData[3]
  SCAN  FOR cContType+cCont_Id+Store + Contact = "C" + PADR(laData[2],8) + laData[3]
  *--- SSH : END
    SCATTER MEMVAR MEMO 
    *C102589,1 TMI [Start] Use deleted records.
    *SELECT CONTACT
    *APPEND BLANK    
    *GATHER MEMVAR MEMO
    IF !DELETED(lcContFile)
      SELECT CONTACT
      LOCATE FOR DELETED()
      IF FOUND()
        RECALL
        BLANK
        GATHER MEMVAR MEMO
      ELSE
        INSERT INTO CONTACT FROM MEMVAR
      ENDIF
    ELSE  && Delete all related records in ContRole file
      SELECT CONTROLE   && Delete related records form Contact File
      =SEEK(m.CCONTTYPE+m.CCONT_ID+m.STORE+m.CONTACT,'CONTROLE')
      DELETE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE = m.CCONTTYPE+m.CCONT_ID+m.STORE+m.CONTACT
    ENDIF
    GO TOP IN CONTACT
    *C102589,1 TMI [End  ] Use deleted records , and delete related records in contact file 
    =gfTraceKey('CONTACT',m.cContType+m.cCont_Id+m.Store+m.Contact,'A')
  ENDSCAN
ENDIF
*B604505,1 Delete all records in master Contact file before updating. [End]

*C102589,1 TMI [Start] Update CONTROLE file.
SET DELETED ON 
IF USED(lcContRole)
  SELECT (lcContRole)
  GO TOP
  SCAN
    IF !SEEK('C'+CCONT_ID+STORE+CONTACT+CROLE,'CONTROLE') 
      *C102589,4 TMI [START] Check that cMark is empty
      IF EMPTY(CMARK)   
      *C102589,4 TMI [END  ] Check that cMark is empty
        SCATTER MEMVAR
        INSERT INTO CONTROLE FROM MEMVAR         && New 
        =gfAdd_Info('CONTROLE')
      *C102589,4 TMI [START] 
      ENDIF
      *C102589,4 TMI [END  ] 
    ELSE
      IF CMARK = 'D'                             && Delete
        SELECT CONTROLE
        DELETE
      ENDIF      
    ENDIF
  ENDSCAN
ENDIF
SET DELETED OFF
*C102589,1 TMI [End  ] Update CONTROLE file.
*C102589,4 TMI [START] Close files To refresh data
IF USED("CONTROLE")
  USE IN CONTROLE
ENDIF
IF USED(lcContRole)
  USE IN (lcContRole)
ENDIF
*C102589,4 TMI [END  ] Close files To refresh data

IF USED(lcProFile)
  SELECT (lcProFile)
  SCAN
    =SEEK(cContType+cCont_Id+Store+cPro_Code+cPro_Value,'Profile')
    DO CASE
      CASE DELETED(lcProFile) .AND. FOUND('Profile')
        SELECT Profile
        *E300872,8 Call Trace Key function.
        =gfTraceKey('Profile',cContType+cCont_Id+Store+cPro_Code+cPro_Value,'D')
        DELETE
      CASE !DELETED(lcProFile) .AND. FOUND('Profile')
        SCATTER MEMVAR
        SELECT Profile
        GATHER MEMVAR
        *E300872,8 Call Trace Key function.
        =gfTraceKey('Profile',cContType+cCont_Id+Store+cPro_Code+cPro_Value,'M')
      CASE !DELETED(lcProFile) .AND. !FOUND('Profile')
        SCATTER MEMVAR MEMO
        INSERT INTO Profile FROM MEMVAR
        *E300872,8 Call Trace Key function.
        =gfTraceKey('Profile',m.cContType+m.cCont_Id+m.Store+m.cPro_Code+m.cPro_Value,'M')
    ENDCASE
  ENDSCAN
ENDIF
IF USED(lcProVal)
  SELECT (lcProVal)
  SCAN
    =SEEK(cPro_Code+cPro_Value,'arPrFCod')
    DO CASE
      CASE DELETED(lcProVal) .AND. FOUND('arPrFCod')
        SELECT arPrFCod
        *E300872,8 Call Trace Key function.
        =gfTraceKey('arPrFCod',cPro_Code+cPro_Value,'D')
        DELETE
      CASE !DELETED(lcProVal) .AND. FOUND('arPrFCod')
        SCATTER MEMVAR MEMO
        SELECT arPrFCod
        GATHER MEMVAR MEMO
        *E300872,8 Call Trace Key function.
        =gfTraceKey('arPrFCod',m.cPro_Code+m.cPro_Value,'M')
      CASE !DELETED(lcProVal) .AND. !FOUND('arPrFCod')
        SCATTER MEMVAR MEMO
        INSERT INTO arPrFCod FROM MEMVAR
        *E300872,8 Call Trace Key function.
        =gfTraceKey('arPrFCod',m.cPro_Code+m.cPro_Value,'A')
    ENDCASE
  ENDSCAN
ENDIF
SET DELETE &lcDelStat
=lfClsTmpFiles()
*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'ARCUSHST',gcDataDir+'ACTHST','SH')
*E301077,49 (End)

SELECT Customer
IF !SEEK(laData[1]+laData[2]+laData[3],'Customer')
  APPEND BLANK
ENDIF
IF laData[1]='M' .AND. !SEEK(laData[2],'arCusHst')
  INSERT INTO arCusHst (Account,cFisFYear) VALUES (laData[2],STR(VAL(gcCurrYear)-1,4))
  INSERT INTO arCusHst (Account,cFisFYear) VALUES (laData[2],gcCurrYear)
  INSERT INTO arCusHst (Account,cFisFYear) VALUES (laData[2],STR(VAL(gcCurrYear)+1,4))
  *E300872,8 Call Trace Key function.
  =gfTraceKey('arCusHst',laData[2],'A')
ENDIF
=RLOCK()
IF laData[1]='S'
  STORE '' TO laData[30],laData[31],laData[33],laData[34],laData[35],laData[36],;
              laData[37],laData[38],laData[39],laData[41],laData[61],laData[27],;
              laData[67],laData[68],laData[69],laData[73],laData[24],laData[76]
  *B801928,1 Default store Back Order setting to customer's.
  STORE '' TO laData[75]

  STORE 0  TO laData[25],laData[26],laData[65]
  STORE {} TO laData[28]
ENDIF
GATHER FROM laData FIELDS &lcScFields
UNLOCK

*------------------------------------------------------------
*E301557,1 Saving a record for the current saved customer in the EDITrans Table (Start)
IF 'NC' $ gcComp_Mdl AND laData[1]='M'
  PRIVATE lcCodesTag

  = gfOpenFile(gcDataDir+'EDIAcPrt',gcDataDir+'Accfact','SH')
  = gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PartTrans','SH')
  = gfOpenFile(gcDataDir+'EDITrans',gcDataDir+'Typekey','SH')

  lcCodesTag = ORDER('Codes')
  SET ORDER TO cCode_No IN Codes
  SELECT EDIAcPrt
  SCAN
    IF lInterComp AND SEEK(EDIAcPrt.cPartCode+'816','EDIPD')
      IF SEEK('N'+'CSITEID   '+PADR(EDIAcPrt.cSiteID,6)+SPACE(30)+'LRECCUST','Codes') AND ;
         Codes.cRltd_Vlu = 'T'
        SELECT EDITRANS
        *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin] 
        *IF !SEEK('816'+PADR('M'+laData[2],20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
        IF !SEEK('816'+PADR('M'+laData[2],40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
        *E037853,1 [End]
          INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,lInterComp) ;
          VALUES                 ('816','M'+laData[2],EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
        ENDIF  
        REPLACE cStatus WITH 'N'
        =gfAdd_Info('EDITRANS')
      ENDIF
    ENDIF
  ENDSCAN
  SET ORDER TO (lcCodesTag) IN Codes
ENDIF
*E301557,1 (End)
*------------------------------------------------------------
*C102565,1 TMI [START] Update the fields detecting last user updated in the customer file
IF laScrMode[3] AND ASCAN(laEvntTrig , PADR('AUDITTRL',10)) <> 0
  =gfDoTriger('ARCUST',PADR('AUDITTRL',10))
ENDIF     
*C102565,1 TMI [END  ] Update the fields detecting last user updated in the customer file

*C200339,1 TMI [START] Save added Data to the file DivSlsRp for customer ENG07
IF ASCAN(laEvntTrig , PADR('UPDATE',10)) <> 0
  =gfDoTriger('ARCUST',PADR('UPDATE',10))
ENDIF     
*C200339,1 TMI [END  ] Save added Data to the file DivSlsRp for customer ENG07

*E301077,49 Inhance openning files to speed up transaction
=gfCloseFile('ARCUSHST')
*E301077,49 (End)
*E300872,8 Call Trace Key function.
=gfTraceKey('Customer',laData[1]+laData[2]+laData[3],IIF(laScrMode[4],'A','M'))
*C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
IF ASCAN(laEvntTrig , PADR('CUSTOMER_M',10)) <> 0
  IF laScrMode[4]
    =gfDoTriger('ARCUST',PADR('CUSTOMER2A',10))
  ELSE
    =gfDoTriger('ARCUST',PADR('CUSTOMER2M',10))
  ENDIF
ENDIF
*C200597,1  TMI [End  ] 

*C123851,1  TMI [Start] Save added "alternate shipping addresses"
IF ASCAN(laEvntTrig,'SVALTSHP')<>0
  =gfDoTriger('ARCUST','SVALTSHP')
ENDIF
*C123851,1  TMI [End  ] 

RETURN


*!**************************************************************************
*! Name      : lpDelScr
*! Developer : Wael Ali Mohamed
*! Date      : 06/30/97
*! Purpose   : Delete customer or store.
*!**************************************************************************
*! Calls     : gfOpenFile,gfModalGen,ARDELST.SPX
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lpDelScr
*!**************************************************************************
FUNCTION lpDelScr
PRIVATE lnDeleted,llDelMain

llDelMain = .F.
lnDeleted = 0
IF laData[1] = 'M'
  lnChoice = 1
  IF SEEK('S'+laData[2],'Customer')
    *E300455,1 Message : 40065
    *E300455,1 Do you wish to delete 
    *E300455,1 Button : 40010
    *E300455,1 Customer  Select Stores  Cancel
    lnChoice = gfModalGen('QRM40065B40010','ALERT')
  ENDIF
  =SEEK('M'+laData[2],'Customer')
  DO CASE 
    CASE lnChoice = 1
      SELECT TYPE,ACCOUNT,STORE,STNAME,cAddress3,cAddress4,cAddress5,'»' AS 'CSELECT';
      FROM CUSTOMER WHERE TYPE+Account+Store = 'S'+laData[2] ;
      INTO DBF (gcWorkDir+lcDelRec)
    CASE lnChoice = 2
      SELECT TYPE,ACCOUNT,STORE,STNAME,cAddress3,cAddress4,cAddress5,SPACE(1) AS 'CSELECT';
      FROM CUSTOMER WHERE TYPE+Account = 'S'+laData[2] ;
      INTO DBF (gcWorkDir+lcDelRec)
    CASE lnChoice = 3
      RETURN
  ENDCASE
  INDEX ON cSelect+Account+Store TAG (lcSelDel)
  INDEX ON Account+Store TAG (lcDelRec) ADDITIVE
  IF lnChoice = 1
    llDelMain = .T.
  ELSE
    DO (gcScrDir+"ARDELST.SPX")
  ENDIF
ELSE
  SELECT TYPE,ACCOUNT,STORE,STNAME,cAddress3,cAddress4,cAddress5,"»" AS 'CSELECT';
  FROM CUSTOMER WHERE TYPE+Account+Store = 'S'+laData[2]+laData[3] ;
  INTO DBF (gcWorkDir+lcDelRec)
  INDEX ON cSelect+Account+Store TAG (lcSelDel)
  INDEX ON Account+Store TAG (lcDelRec) ADDITIVE
ENDIF

*E300455,1 CHECKING THE ORDER HEADER FILE
WAIT WINDOW 'Checking the ORDER file...' NOWAIT

*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDACCT','SH')
=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'Ordlinst','SH')
*E301077,49 (End)

SELECT (lcDelRec)
SET ORDER TO

SELECT ORDHDR
IF llDelMain .AND. SEEK(laData[2])
  LOCATE REST WHILE ACCOUNT= laData[2] FOR INLIST(STATUS,'O','H')
  IF FOUND()
    *E300455,1 Message : 40066
    *E300455,1 Open or hold orders have been found for account xxxxx. Cannot delete
    *E300455,1 Button : 00000
    *E300455,1 Ok
    =gfModalGen('TRM40066B00000','ALERT','Open or hold orders'+'|'+'account '+laData[2])
    llDelMain = .F.
  ENDIF
ENDIF
SELECT ORDHDR
=SEEK(laData[2])
SCAN REST WHILE ACCOUNT = laData[2] FOR INLIST(STATUS,'O','H')
  WAIT WINDOW 'Order # : '+ORDER NOWAIT
  SELECT(lcDelRec)
  SCAN FOR CSELECT = "»"
    IF SEEK(OrdHdr.cOrdType+OrdHdr.Order+Store,'ORDLINE')
      *E300455,1 Message : 40066
      *E300455,1 Open or hold orders have been found for store xxxxx. Cannot delete
      *E300455,1 Button : 00000
      *E300455,1 Ok
      =gfModalGen('TRM40066B00000','ALERT',IIF(OrdHdr.cOrdType='O','Open or hold orders','Open or hold order contracts')+'|'+'store '+store)
      REPLACE CSELECT WITH SPACE(1)
    ENDIF
  ENDSCAN
ENDSCAN

*B039299,1  TMI [Start] Prevent deleting an account or one of its stores if it has a material sales order
IF !USED('MASOHDR')
  =gfOpenFile(gcDataDir+'MASOHDR',gcDataDir+'ORDACCT','SH')
ENDIF
IF !USED('MASOLIN')
  =gfOpenFile(gcDataDir+'MASOLIN',gcDataDir+'MASOLINST','SH')
ENDIF
SET ORDER TO ORDACCT   IN MASOHDR
SET ORDER TO MASOLINST IN MASOLIN
SELECT MASOHDR
IF llDelMain .AND. SEEK(laData[2])
  LOCATE REST WHILE ACCOUNT= laData[2] FOR INLIST(STATUS,'O','H')
  IF FOUND()
    *B039299,1 Message : 40066
    *B039299,1 Open or hold orders have been found for account xxxxx. Cannot delete
    *B039299,1 Button : 00000
    *B039299,1 Ok
    =gfModalGen('TRM40066B00000','ALERT','Open or hold Material Sales Orders'+'|'+'account '+laData[2])
    llDelMain = .F.
  ENDIF
ENDIF
SELECT MASOHDR
=SEEK(laData[2])
SCAN REST WHILE ACCOUNT = laData[2] FOR INLIST(STATUS,'O','H')
  WAIT WINDOW 'Material Sales Order # : '+CMORDER NOWAIT
  SELECT(lcDelRec)
  SCAN FOR CSELECT = "»"
    IF SEEK(MASOHdr.cOrdType+MASOHdr.CMOrder+&lcDelRec..Store,'MASOLIN')
      *B039299,1 Message : 40066
      *B039299,1 Open or hold orders have been found for store xxxxx. Cannot delete
      *B039299,1 Button : 00000
      *B039299,1 Ok
      =gfModalGen('TRM40066B00000','ALERT',IIF(OrdHdr.cOrdType='O','Open or hold Material Sales Orders','Open or hold Material order contracts')+'|'+'store '+store)
      REPLACE CSELECT WITH SPACE(1)
    ENDIF
  ENDSCAN
ENDSCAN
=gfCloseFile('MASOHDR') 
=gfCloseFile('MASOLIN') 
*B039299,1  TMI [End  ] 

*E300455,1 CHECKING DEBIT FILE
WAIT WINDOW 'Checking the DEBIT file...' NOWAIT
*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'DEBIT',gcDataDir+'DEBIT','SH')
*E301077,49 (End) 

SELECT DEBIT
IF llDelMain .AND. SEEK(laData[2])
  *E300455,1 Message : 40066
  *E300455,1 Debits have been found for account xxxxx. Cannot delete
  *E300455,1 Button : 00000
  *E300455,1 Ok
  =gfModalGen('TRM40066B00000','ALERT','Debits'+'|'+'account '+laData[2])
  llDelMain = .F.
ENDIF
SELECT(lcDelRec)
SCAN FOR CSELECT = "»"
  SELECT DEBIT
  =SEEK(laData[2])
  LOCATE REST WHILE ACCOUNT=laData[2] FOR STORE = &lcDelRec..Store
  IF FOUND()
    *E300455,1 Message : 40066
    *E300455,1 Debits have been found for store xxxxx. Cannot delete
    *E300455,1 Button : 00000
    *E300455,1 Ok
    =gfModalGen('TRM40066B00000','ALERT','Debits'+'|'+'store '+&lcDelRec..Store)

    *B604489,1 Return to lcDelRec temp file. [Begin] 
    SELECT(lcDelRec)
    *B604489,1 Return to lcDelRec temp file. [End] 
        
    REPLACE CSELECT WITH SPACE(1)
  ENDIF
ENDSCAN

*E300455,1 CHECKING CREDIT FILE
WAIT WINDOW 'Checking the Credit file...' NOWAIT
*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'CREDIT',gcDataDir+'CREDIT','SH')
*E301077,49 (End)
SELECT CREDIT 
IF llDelMain .AND. SEEK(laData[2])
  *E300455,1 Message : 40066
  *E300455,1 Credits have been found for account xxxxx. Cannot delete
  *E300455,1 Button : 00000
  *E300455,1 Ok
  =gfModalGen('TRM40066B00000','ALERT','Credits'+'|'+'account '+laData[2])
  llDelMain = .F.
ENDIF
SELECT(lcDelRec)
SCAN FOR CSELECT = "»"
  SELECT CREDIT
  =SEEK(laData[2])
  LOCATE REST WHILE ACCOUNT=laData[2] FOR STORE = &lcDelRec..Store
  IF FOUND()
    *E300455,1 Message : 40066
    *E300455,1 Credits have been found for store xxxxx. Cannot delete
    *E300455,1 Button : 00000
    *E300455,1 Ok
    =gfModalGen('TRM40066B00000','ALERT','Credits'+'|'+'store '+&lcDelRec..Store)
    REPLACE CSELECT WITH SPACE(1)
  ENDIF
ENDSCAN
WAIT CLEAR

*B607403,1  TMI [Start] Check that if a store is a DC for another store
*- Open files in new aliases to check a store is a DC
PRIVATE lcTmpCst,lcTmpDlRec
lcTmpCst   = gfTempName()
lcTmpDlRec = gfTempName()
=gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH',@lcTmpCst,.T.)
=gfOpenFile(gcWorkDir+lcDelRec,lcSelDel,'SH',@lcTmpDlRec,.T.)

*-- if this store is a DC for another store , deny deletion.
SELECT(lcDelRec)
SCAN FOR CSELECT = "»"
  IF lfChkDC(&lcDelRec..STORE) 
    =gfModalGen('INM40186B00000','DIALOG',ALLTRIM(&lcDelRec..STORE))
    REPLACE &lcDelRec..CSELECT WITH SPACE(1)
  ENDIF
ENDSCAN  

*- Close files opened to check a store is a DC
IF USED(lcTmpDlRec)
  USE IN &lcTmpDlRec
ENDI
IF USED(lcTmpCst)
  USE IN &lcTmpCst
ENDIF  
*B607403,1  TMI [End  ]

SELECT(lcDelRec)
COUNT FOR !EMPTY(CSELECT) TO lnDelCount
lnDelCount = lnDelCount + IIF(llDelMain,1,0)

IF (lnDelCount = 0 AND gfModalGen('TRM40067B00000','ALERT')=1) OR ;
    gfModalGen('QRM40068B40003','ALERT',ALLTRIM(STR(lnDelCount))) = 2
  *E300455,1 Message : 40067
  *E300455,1 No customer records qualified for deletion.
  *E300455,1 Button : 00000
  *E300455,1 Ok

  *E300455,1 Message : 40068
  *E300455,1 Deleting n customer records. Last Chance
  *E300455,1 Button : 40003
  *E300455,1 Proceed  Cancel

  *E301077,49 Inhance openning files to speed up transaction
  =gfCloseFile('ORDHDR')
  =gfCloseFile('ORDLINE')
  =gfCloseFile('DEBIT')
  =gfCloseFile('CREDIT')
  *E301077,49 (End)

  SELECT CUSTOMER
  RETURN
ENDIF

*E300455,1 Message : 40069
*E300455,1 Would you like to print a report of deleted customers ?
*E300455,1 Button : 40000
*E300455,1 Yes No
*IF gfModalGen('QRM40069B40000','ALERT') = 1
*ENDIF

*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'CONTACT',gcDataDir+'CONTACT','SH')
=gfOpenFile(gcDataDir+'ProFile',gcDataDir+'ProFile','SH')
=gfOpenFile(gcSysHome+'syschdul',gcSysHome+'SchDate','SH')
=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDRA','SH')
=gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH')
=gfOpenFile(gcDataDir+'ARHIST',gcDataDir+'ARHIST','SH')
=gfOpenFile(gcDataDir+'ARCUSHST',gcDataDir+'ACTHST','SH')
*E301077,49 (End)
*C102589,1 TMI [START] OPEN CONTRLE FILE 
=gfOpenFile(gcDataDir+'CONTROLE',gcDataDir+'CONTROLE','SH')
*C102589,1 TMI [END  ] OPEN CONTRLE FILE 

SELECT ORDLINE
SET ORDER TO
SELECT INVLINE
SET ORDER TO 
SELECT(lcDelRec)
SCAN FOR !EMPTY(CSELECT)
  *E300455,1 DELETE CUSTOMER HISTORY
  SELECT ARHIST
  IF SEEK(laData[2])
    WAIT WINDOW 'Deleting customer history for Store : '+&lcDelRec..STORE+'.' NOWAIT
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE Account+History = laData[2] ;
           FOR STORE = &lcDelRec..STORE AND ;
           gfTraceKey('ARHIST',ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO,'D')
  ENDIF

  *E300455,1 DELETE ORDERS HEADERS FOR THAT ACCOUNT
  SELECT ORDHDR
  IF SEEK(laData[2])
    WAIT WINDOW 'Deleting orders for Store : '+&lcDelRec..STORE+'.' NOWAIT
    SCAN REST WHILE ACCOUNT = laData[2] FOR STORE=&lcDelRec..STORE
      SELECT ORDLINE
      *E300872,8 Call Trace Key function.
      DELETE ALL FOR cOrdType+ORDER = OrdHdr.cOrdType+OrdHdr.Order AND ;
      gfTraceKey('ORDLINE',CORDTYPE+ORDER+STR(LINENO,6),'D')
    ENDSCAN
    SELECT ORDHDR
    =SEEK(laData[2])
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE ACCOUNT = laData[2] FOR STORE=&lcDelRec..STORE AND ;
    gfTraceKey('ORDHDR',CORDTYPE+ORDER,'D')
  ENDIF

  *E300455,1 DELETE INVOICES
  SELECT INVHDR
  IF SEEK(laData[2])
    WAIT WINDOW 'Deleting invoices for Store : '+&lcDelRec..STORE+'.' NOWAIT
    SCAN REST WHILE ACCOUNT=laData[2] FOR STORE=&lcDelRec..STORE
      SELECT INVLINE
      *E300872,8 Call Trace Key function.
      DELETE ALL FOR INVOICE = InvHdr.Invoice AND ;
      gfTraceKey('INVLINE',INVOICE+STR(LINENO,6),'D')
    ENDSCAN
    SELECT INVHDR
    =SEEK(laData[2])
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE ACCOUNT = laData[2] FOR STORE = &lcDelRec..STORE AND ;
    gfTraceKey('INVHDR',INVOICE,'D')
  ENDIF

  *E300455,1 Delete Contacts
  SELECT CONTACT
  IF SEEK('C'+PADR(laData[2],8)+&lcDelRec..STORE)
    *E300872,8 Call Trace Key function.
    *C102589,1 TMI [START] Delete related roles from CONTROLE FILE
    *DELETE REST WHILE cContType+cCont_Id+Store+Contact = ;
    *                 'C'+PADR(laData[2],8)+&lcDelRec..STORE AND ;
    *gfTraceKey('CONTACT',CCONTTYPE+CCONT_ID+STORE+CONTACT,'D')
    SCAN REST WHILE cContType+cCont_Id+Store+Contact = 'C'+PADR(laData[2],8)+&lcDelRec..STORE 
      SELECT CONTROLE
      =SEEK('C'+PADR(laData[2],8)+&lcDelRec..STORE+CONTACT.CONTACT,'CONTROLE')
      DELETE REST WHILE cContType+cCont_Id+Store+Contact+CROLE = ;
               'C'+PADR(laData[2],8)+&lcDelRec..STORE+CONTACT.CONTACT
      SELECT CONTACT
      DELETE
    ENDSCAN
    *C102589,1 TMI [END  ] Delete related roles from CONTROLE FILE
  ENDIF

  *E300455,1 Delete Profile
  SELECT PROFILE
  IF SEEK('C'+PADR(laData[2],8)+&lcDelRec..STORE)
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE cContType+cCont_Id+Store+cPro_Code= ;
                     'C'+PADR(laData[2],8)+&lcDelRec..STORE AND ;
    gfTraceKey('PROFILE',CCONTTYPE+CCONT_ID+STORE+CPRO_CODE+CPRO_VALUE,'D')
  ENDIF

  *E300455,1 Delete Pending/History
  SELECT syschdul
  IF SEEK('C'+PADR(laData[2],8)+&lcDelRec..STORE)
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE cContType+cCont_Id+Store+Contact = ;
                     'C'+PADR(laData[2],8)+&lcDelRec..STORE AND ;
    gfTraceKey('syschdul',CSEQNUMBER,'D')
  ENDIF
  
  *E300455,1 Delete customer stores
  WAIT WINDOW 'Deleting Store : '+&lcDelRec..STORE+'.' NOWAIT
  SELECT CUSTOMER
  IF SEEK('S'+laData[2]+&lcDelRec..STORE)
    *E300872,8 Call Trace Key function.
    =gfTraceKey('CUSTOMER','S'+laData[2]+&lcDelRec..STORE,'D')
    *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
    IF ASCAN(laEvntTrig , PADR('CUSTOMER_D',10)) <> 0
      =gfDoTriger('ARCUST',PADR('CUSTOMER_D',10))
    ENDIF
    *C200597,1  TMI [End  ] 
    DELETE
  ENDIF
ENDSCAN
*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'CUSTDEPT',gcDataDir+'CUSTDEPT','SH')
=gfOpenFile(gcDataDir+'NOTEPAD',gcDataDir+'NOTEPAD','SH')
*E301077,49 (End)
IF llDelMain
  *E300455,1 DELETE CUSTOMER HISTORY
  SELECT ARHIST
  IF SEEK(laData[2])
    WAIT WINDOW 'Deleting history for Account: '+laData[2]+'.' NOWAIT
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE account+history = laData[2] AND ;
    gfTraceKey('ARHIST',ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO,'D')
  ENDIF

  *E300455,1 DELETE ORDERS HEADERS FOR THAT ACCOUNT
  SELECT ORDHDR
  IF SEEK(laData[2])
    WAIT WINDOW 'Deleting orders for Account : '+laData[2]+'.' NOWAIT
    SCAN REST WHILE ACCOUNT = laData[2]
      SELECT ORDLINE
      *E300872,8 Call Trace Key function.
      DELETE ALL FOR cOrdType+ORDER = OrdHdr.cOrdType+OrdHdr.Order AND ;
      gfTraceKey('ORDLINE',CORDTYPE+ORDER+STR(LINENO,6),'D')
    ENDSCAN
    SELECT ORDHDR
    =SEEK(laData[2])
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE ACCOUNT=laData[2] AND gfTraceKey('ORDHDR',CORDTYPE+ORDER,'D')
  ENDIF
  *E300455,1 DELETE INVOICES
  SELECT INVHDR
  IF SEEK(laData[2])
    WAIT WINDOW 'Deleting invoices for Account : '+laData[2]+'.' NOWAIT
    SCAN REST WHILE ACCOUNT=laData[2]
      SELECT INVLINE
      *E300872,8 Call Trace Key function.
      DELETE ALL FOR INVOICE = InvHdr.Invoice AND ;
      gfTraceKey('INVLINE',INVOICE+STR(LINENO,6),'D')
    ENDSCAN
    SELECT INVHDR
    =SEEK(laData[2])
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE ACCOUNT=laData[2] AND gfTraceKey('INVHDR',INVOICE,'D')
  ENDIF
  *E300455,1 Delete Contacts
  SELECT CONTACT
  IF SEEK('C'+PADR(laData[2],8)+SPACE(8))
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE cContType+cCont_Id+Store+Contact = ;
                     'C'+PADR(laData[2],8)+SPACE(8) AND ;
          gfTraceKey('CONTACT',CCONTTYPE+CCONT_ID+STORE+CONTACT,'D')
  ENDIF

  *E300455,1 Delete Profile
  SELECT PROFILE
  IF SEEK('C'+PADR(laData[2],8)+SPACE(8))
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE cContType+cCont_Id+Store+cPro_Code= ;
                     'C'+PADR(laData[2],8)+SPACE(8) AND ;
    gfTraceKey('PROFILE',CCONTTYPE+CCONT_ID+STORE+CPRO_CODE+CPRO_VALUE,'D')
  ENDIF

  *E300455,1 Delete Pending/History
  SELECT syschdul
  IF SEEK('C'+PADR(laData[2],8)+SPACE(8))
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE cContType+cCont_Id+Store+Contact = ;
                     'C'+PADR(laData[2],8)+SPACE(8) AND ;
           gfTraceKey('syschdul',CSEQNUMBER,'D')
  ENDIF
  
  *E300455,1 DELETE Customer Departments
  WAIT WINDOW 'Deleting customer departments.' NOWAIT
  SELECT CUSTDEPT
  IF SEEK(laData[2])
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE ACCOUNT=laData[2] AND gfTraceKey('CUSTDEPT',ACCOUNT+DEPT,'D')
  ENDIF
  
  *E300455,1 Delete Customer History
  SELECT ARCUSHST
  IF SEEK(laData[2])
    *E300872,8 Call Trace Key function.
    DELETE REST WHILE ACCOUNT=laData[2] AND gfTraceKey('ARCUSHST',ACCOUNT+CFISFYEAR,'D')
  ENDIF

  *E300455,1 DELETE THE CUSTOMER
  WAIT WINDOW 'Deleting the Account : '+laData[2]+'.' NOWAIT
  SELECT CUSTOMER
  IF SEEK('M'+laData[2])
    WAIT WINDOW 'Deleting the notepad for Account : '+laData[2] NOWAIT
    SELECT NOTEPAD
    IF SEEK('A'+laDAta[2])
      *E300872,8 Call Trace Key function.
      DELETE REST WHILE TYPE+KEY='A'+laDAta[2] AND ;
      gfTraceKey('NOTEPAD','A'+laDAta[2],'D')
    ENDIF
    SELECT CUSTOMER 
    *E300872,8 Call Trace Key function.
    =gfTraceKey('CUSTOMER','M'+laData[2],'D')
    *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
    IF ASCAN(laEvntTrig , PADR('CUSTOMER2D',10)) <> 0
      =gfDoTriger('ARCUST',PADR('CUSTOMER2D',10))
    ENDIF
    *C200597,1  TMI [End  ] 
    DELETE
  ENDIF
ENDIF
*E301077,49 Inhance openning files to speed up transaction
=gfCloseFile('ORDHDR')
=gfCloseFile('ORDLINE')
=gfCloseFile('INVHDR')
=gfCloseFile('INVLINE')
=gfCloseFile('DEBIT')
=gfCloseFile('CREDIT')
=gfCloseFile('ARHIST')
=gfCloseFile('CONTACT')
=gfCloseFile('ProFile')
=gfCloseFile('syschdul')
=gfCloseFile('CUSTDEPT')
=gfCloseFile('NOTEPAD')
=gfCloseFile('ARCUSHST')
*E301077,49 (End)

WAIT CLEAR
SELECT CUSTOMER
STORE .F. TO laScrMode
STORE .T. TO laScrMode[1]

*!**************************************************************************
*! Name      : lfDelStore
*! Developer : Wael Ali Mohamed
*! Date      : 06/30/97
*! Purpose   : Browse customer stores to delete
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfDelStore()
*!**************************************************************************
FUNCTION lfDelStore
SELECT (lcDelRec)
lnDeleted = RECNO()
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnDeleted,'>',' '):H=' ':R:1:W=.F.,;
       CSELECT :H=' ' :R,;
       Store   :H='Store' :R,;
       StName  :H='Name'  :R,;
       cAddress3 :H=SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),;
       cAddress4 :H=SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),;
       cAddress5 :H=SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len);
       WINDOW ARDELST0   ;
       IN WINDOW ARDELST ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfwDelSt()   ;
       TITLE lcBrDelTtl

*!**************************************************************************
*! Name      : lfwDelSt
*! Developer : Wael Ali Mohamed
*! Date      : 06/30/97
*! Purpose   : Show function for customer deleted stores browse
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfwDelSt()
*!**************************************************************************
FUNCTION lfwDelSt
lnDeleted = RECNO(lcDelRec)
SHOW WINDOW (lcBrDelTtl) REFRESH SAME
IF CSELECT = SPACE(1)
  SHOW GET pbSelect,1 PROMPT '\<Select'
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select'
ENDIF

*!*************************************************************
*! Name      : lfAdjButt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Adjust buttons for order template screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfAdjButt()
*!*************************************************************
FUNCTION lfAdjButt
PARAMETERS lcType
PRIVATE lcKey,lcDataSel,lcDataUSel

SELECT (lcDelRec)
lcKey = Account+Store
DO CASE
  CASE lcType = 'S'
    REPLACE CSELECT WITH IIF(CSELECT=SPACE(1),"»",SPACE(1))
    SKIP
    IF EOF()
      SKIP -1
    ENDIF
    lcKey = Account+Store
  CASE lcType = 'A'
    REPLACE ALL CSELECT WITH "»"
  CASE lcType = 'N'
    REPLACE ALL CSELECT WITH SPACE(1)
  CASE lcType = 'I'
    REPLACE ALL CSELECT WITH IIF(CSELECT=SPACE(1),"»",SPACE(1))
ENDCASE  
SET ORDER TO TAG (lcSelDel)
lcDataSel   = IIF(SEEK("»"),'ENABLE','DISABLE')
lcDataUSel  = IIF(SEEK(' '),'ENABLE','DISABLE')
SET ORDER TO TAG (lcDelRec)
=SEEK(lcKey)
SHOW GET pbSelAll  &lcDataUSel
SHOW GET pbSelNone &lcDataSel
IF CSELECT = SPACE(1)
  SHOW GET pbSelect,1 PROMPT '\<Select'
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select'
ENDIF

*!*************************************************************
*! Name      : gfCPBrows
*! Developer : Wael Aly Mohaed
*! Date      : 07/01/1996
*! Purpose   : Validation of the browse button in the pannel
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfCPBrows
*B604088,1 Change browse of Magnifying glass to be exactly like Account Browse [Begin]
SELECT (lcBaseFile)
=gfBrows(['M'])
*DO CUSBROWM WITH '?'
*B604088,1 Change browse of Magnifying glass to be exactly like Account Browse [End]

*!*************************************************************
*! Name      : gfCPTop
*! Developer : Wael Aly Mohaed
*! Date      : 07/01/1996
*! Purpose   : Validation of the top button in the pannel
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION  gfCPTop

ACTIVATE WINDOW GWCCONTRL1 TOP
SELECT Customer
SET KEY TO 'M'
=SEEK('M'+laData[2])
GO TOP
SCATTER FIELDS &lcScFields MEMO TO laData
SHOW GETS
SELECT Customer
SET KEY TO

*!*************************************************************
*! Name      : gfCPBttm
*! Developer : Wael Aly Mohaed
*! Date      : 07/01/1996
*! Purpose   : Validation of the bottom button in the pannel
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfCPBttm

ACTIVATE WINDOW GWCCONTRL1 TOP
SELECT Customer
SET KEY TO 'M'
=SEEK('M'+laData[2])
GO BOTTOM
SCATTER FIELDS &lcScFields MEMO TO laData
SHOW GETS
SELECT Customer
SET KEY TO

*!*************************************************************
*! Name      : gfCPNext
*! Developer : Wael Aly Mohaed
*! Date      : 07/01/1996
*! Purpose   : Validation of next button in the pannel
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfCPNext

ACTIVATE WINDOW GWCCONTRL1 TOP
SELECT Customer
SET KEY TO 'M'
=SEEK('M'+laData[2])
SKIP 
SCATTER FIELDS &lcScFields MEMO TO laData
SHOW GETS
SELECT Customer
SET KEY TO

*!*************************************************************
*! Name      : gfCPPrvis
*! Developer : Wael Aly Mohaed
*! Date      : 07/01/1996
*! Purpose   : Validation of the prvios button in the pannel
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION gfCPPrvis

ACTIVATE WINDOW GWCCONTRL1 TOP
SELECT Customer
SET KEY TO 'M'
=SEEK('M'+laData[2])
SKIP -1
SCATTER FIELDS &lcScFields MEMO TO laData
SHOW GETS
SELECT Customer
SET KEY TO

*!*************************************************************
*! Name      : lfClearKey
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Clear key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+Z
ON KEY LABEL ALT+A
ON KEY LABEL ALT+C

*!*************************************************************
*! Name      : gfSeekRec
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Get selected record
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =gfSeekRec()
*!*************************************************************
FUNCTION gfSeekRec

*E300689,1 Check if some key fields to be excluded from the dialog message
lcExclFld = IIF(TYPE('lcExclFld')='C',lcExclFld,'')
*E300689,1 Initialize dialog message title and value
STORE '' TO lcMsgValue,lcMessage
*E300689,1 (End)
lcExacStat=SET("EXACT")
lcFld_Ttl = ''
llUnique  = .F.
lcCurrObj = SYS(18)
glDobrow  = IIF(TYPE('llBrowse')<>"U",llBrowse,.F.)
SELECT (lcBaseFile)
FOR lnCount = 1 TO ALEN(laKeyField,1)
  IF SUBSTR(laKeyField[lnCount,1],8, ATC(']',laKeyField[lnCount,1]) -8);
    = SUBSTR(SYS(18),8, ATC(')',SYS(18))-8)
    IF laKeyField[lnCount,2]
      llUnique = .T.
    ENDIF
    lcSeekTag = laKeyField[lnCount,3]
    SET ORDER TO TAG (lcSeekTag)
    lcSeekExp = laKeyField[lnCount,1]

    *E300689,1 Prepare Message
    lcMsgValue= IIF(laKeyField[lnCount,1] $ lcExclFld,'',laKeyField[lnCount,1])
    *E300689,1 (End)
        
    lnCount1  = laKeyField[lnCount,4]

    lcFld_Nam = gfSubStr(SYS(14,VAL(SYS(21))),lnCount1,'+') 
    lnFld_pos = ASCAN(laField_N,lcFld_Nam) 

    IF lnFld_pos > 0 
      lcFld_Ttl=PROPER(laField_H[lnFld_pos])+" :" 
      *E300689,1 Prepare Message title
      lcMessage=IIF(laKeyField[lnCount,1] $ lcExclFld,'',PROPER(laField_H[lnFld_pos])+" :")
      *E300689,1 (End)
        
    ELSE
      lcFld_Ttl=lcFld_Nam +" :"
      *E300689,1 Prepare Message title
      lcMessage=lcFld_Nam +" :"
      *E300689,1 (End)
      
    ENDIF    

    DO WHILE lnCount1 > 1
      lnCount1 = lnCount1 -1

      FOR lnCount = 1 TO ALEN(laKeyField,1) 
        IF laKeyField[lnCount,3] = lcSeekTag .AND.;
          laKeyField[lnCount,4] = lnCount1
          lcSeekExp = laKeyField[lnCount,1] + "+" + lcSeekExp  

          *E300689,1 Prepare Message
          lcMsgValue= IIF(laKeyField[lnCount,1] $ lcExclFld,'',laKeyField[lnCount,1]+ "+") + lcMsgValue
          *E300689,1 (End)
          
          lcFld_Nam = gfSubStr(SYS(14,VAL(SYS(21))),lnCount1,'+') 
          lnFld_pos = ASCAN(laField_N,lcFld_Nam) 

          IF lnFld_pos > 0 
            lcFld_Ttl=PROPER(laField_H[lnFld_pos])+"\"+lcFld_Ttl
            *E300689,1 Prepare Message Tilte
            lcMessage=IIF(laKeyField[lnCount,1] $ lcExclFld,'',PROPER(laField_H[lnFld_pos])+"\")+lcMessage
            *E300689,1 (End)
          ELSE
            lcFld_Ttl=lcFld_Nam +"\"+lcFld_Ttl
            *E300689,1 Prepare Message Tilte
            lcMessage=lcFld_Nam +"\"+lcMessage
            *E300689,1 (End)
          ENDIF    

          EXIT && FOR loop
        ENDIF
      ENDFOR
    ENDDO
    EXIT && FOR loop
  ENDIF
ENDFOR
IF llUnique
 SET EXACT ON 
 IF SEEK(&lcSeekExp)
    laScrMode   = .F.
    laScrMode[2]= .T.
    SCATTER FIELDS &lcScFields MEMO TO laData
  ELSE
    
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    IF TYPE(lcSeekExp) ='C' 
      IF ALLTRIM(&lcSeekExp) ='?' .OR. glDobrow
        lnOption = 1
      ELSE
        *E300647,1 Hesham El-Sheltawi (Start)
        *E300647,1 Add new button to the control panel
        *E300647,1 if the program is using the new button then
        *E300647,1 make the message Browse Reenter
        *E300647,1 else make it Browse Add Reenter
        *IF glAutoAdd
        *  lnOption = 2
        *ELSE
        *  lnOption = gfModalGen('QRM00001B00001','Dialog',lcFld_Ttl+" "+ALLTRIM(&lcSeekExp))
        *ENDIF  
        IF TYPE('LLALOWNEW')='U' OR llAlowNew = .F.
          IF glAutoAdd
            lnOption = 2
          ELSE
            *E300689,1 Exclude unneeded fields from the message
            *lnOption = gfModalGen('QRM00001B00001','Dialog',lcFld_Ttl+" "+ALLTRIM(&lcSeekExp))
            lnOption = gfModalGen('QRM00001B00001','Dialog',lcMessage+" "+ALLTRIM(&lcMsgValue))
            *E300689,1 (End)
          ENDIF  
        ELSE
          *E300689,1 Exclude unneeded fields from the message
          *lnOption = gfModalGen('QRM00001B00014','Dialog',lcFld_Ttl+" "+ALLTRIM(&lcSeekExp))
          lnOption = gfModalGen('QRM00001B00014','Dialog',lcMessage+" "+ALLTRIM(&lcMsgValue))
          *E300689,1 (End)
          
          lnOption = lnOption+IIF(lnOption=2,1,0)
        ENDIF  
        *E300647,1 Hesham El-Sheltawi (End)        
      ENDIF  
    ELSE
      *E300647,1 Hesham El-Sheltawi (Start)
      *E300647,1 Add new button to the control panel
      *E300647,1 if the program is using the new button then
      *E300647,1 make the message Browse Reenter
      *E300647,1 else make it Browse Add Reenter
      *IF glAutoAdd
      *  lnOption = 2
      *ELSE
      *  lcFieldCon = ALLTRIM(STR(&lcSeekExp))
      *  lnOption   = gfModalGen('QRM00001B00001','Dialog',lcFld_Ttl+" "+lcFieldCon) 
      *ENDIF  
      IF TYPE('LLALOWNEW')='U' OR llAlowNew = .F.
        IF glAutoAdd
          lnOption = 2
        ELSE
          lcFieldCon = ALLTRIM(STR(&lcSeekExp))
          *E300689,1 Exclude unneeded fields from the message
          *lnOption   = gfModalGen('QRM00001B00001','Dialog',lcFld_Ttl+" "+lcFieldCon) 
          lnOption   = gfModalGen('QRM00001B00001','Dialog',lcMessage+" "+ALLTRIM(STR(&lcMsgValue))) 
          *E300689,1 (End)
        ENDIF  
      ELSE  
        lcFieldCon = ALLTRIM(STR(&lcSeekExp))
        *E300689,1 Exclude unneeded fields from the message
        *lnOption = gfModalGen('QRM00001B00014','Dialog',lcFld_Ttl+" "+lcFieldCon)
        lnOption = gfModalGen('QRM00001B00014','Dialog',lcMessage+" "+ALLTRIM(STR(&lcMsgValue)))
        *E300689,1 (End)
                
        lnOption = lnOption+IIF(lnOption=2,1,0)
      ENDIF  
      *E300647,1 Hesham El-Sheltawi (End)      
    ENDIF
    DO CASE
      CASE lnOption = 1
        SET EXACT &lcExacStat && wael
        DO WHILE .T.
          IF OCCURS('+',lcSeekExp) = ALEN(laKeyField,1)-2
            SET EXACT &lcExacStat
            IF SEEK(&lcSeekExp) && wael
             =gfBrows(lcSeekExp)
            ELSE
              *E300420,1 Message : 00052
              *E300420,1 There is no records to display
              *E300420,1 Button : 00000 
              *E300420,1 Ok
              =gfModalGen('TRM00052B00000','ALERT')
            ENDIF  
            RETURN
          ENDIF
          lcSeekExp = IIF(OCCURS('+',lcSeekExp)<> ALEN(laKeyField,1)-2,;
                          LEFT(lcSeekExp,RAT('+',lcSeekExp)-1),;
                          lcSeekExp)
          IF SEEK(ALLTRIM(&lcSeekExp))
            EXIT
          ENDIF
        ENDDO
        SET EXACT &lcExacStat
        *B603904,1 MHM 10/02/2000 [star] if it is multi store "S" 
        *=gfBrows(lcSeekExp)
        IF laData[1] = "S"
          xStore   = laData[3]
          IF !CUSBROWS(laData[2],.T.)
            STORE SPACE(8) TO xStore
          ENDIF
          laData[2] = lcOldAcc
          laScrMode   = .F.
          laScrMode[2]= .T.
          SELECT CUSTOMER  
          SCATTER FIELDS &lcScFields MEMO TO laData
          laData[3] = xStore
        ELSE  
          =gfBrows(lcSeekExp)
          RETURN
        ENDIF
        *B603904,1 MHM 10/02/2000 [end]
        *RETURN
      CASE lnOption = 2
          IF !llAddRec
          =gfModalGen("TRM00088B00000","ALERT",'Adding') 
        ELSE
          lcCrFiler = SET('FILTER')
          IF EMPTY(lcCrFiler)
            laScrMode = .F.
            laScrMode[4]=.T.
          ELSE
            SET FILTER TO 
            IF SEEK(&lcSeekExp)
              =gfModalGen("TRM00096B00000","ALERT") 
            ELSE
              laScrMode = .F.
              laScrMode[4]=.T.
            ENDIF
            SET FILTER TO &lcCrFiler   
          ENDIF  
        ENDIF 
      CASE lnOption = 3
        SCATTER FIELDS &lcScFields MEMO TO laData  BLANK
        FOR lnkey = 1 TO ALEN(laKeyField,1)
          SHOW GET &laKeyField[lnkey,1]
        ENDFOR
        _CUROBJ = OBJNUM(&laKeyField[1,1])
        SET EXACT &lcExacStat
        RETURN
    ENDCASE
  ENDIF
  SET EXACT &lcExacStat
  SHOW GETS
ELSE
  DO WHILE !SEEK(&lcSeekExp)
    IF OCCURS('+',lcSeekExp) = 0
      *E300689,1 Exclude unneeded fields from the message
      *lnOption = gfModalGen('INM00001B00000','Dialog',lcFld_Ttl+" "+&lcSeekExp) 
      lnOption = gfModalGen('INM00001B00000','Dialog',lcMessage+" "+&lcMsgValue)
      *E300689,1 (End)
            
      _CUROBJ  = OBJNUM(&lcCurrObj)
      SET EXACT &lcExacStat      
      RETURN
    ENDIF
    lcSeekExp = LEFT(lcSeekExp,RAT('+',lcSeekExp)-1)
    IF SEEK(ALLTRIM(&lcSeekExp))
       EXIT
    ENDIF
  ENDDO
  SET EXACT &lcExacStat  
  =gfBrows(lcSeekExp)
  RETURN
ENDIF
SET EXACT &lcExacStat  

*!*************************************************************
*! Name      : lfvCusHst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Show customer history
*!*************************************************************
*! Calls     : gfOpenFile,ARCUSHST.SPR
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCusHst()
*!*************************************************************
FUNCTION lfvCusHst
PARAMETERS lcToDisp
PRIVATE laCusHst,laHeader,laHistYear,lnCol,lnHistYear

*E301077,49 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'FISHD',gcDataDir+'COMPFYEAR','SH')
=gfOpenFile(gcDataDir+'ARCUSHST',gcDataDir+'ACTHST','SH')
*E301077,49 (End)

lcToDisp = IIF(TYPE('lcToDisp')#'C','H',lcToDisp)
IF !SEEK(laData[2],'arCusHst')
  INSERT INTO arCusHst (Account,cFisFYear) VALUES (laData[2],STR(VAL(gcCurrYear)-1,4))
  INSERT INTO arCusHst (Account,cFisFYear) VALUES (laData[2],gcCurrYear)
  INSERT INTO arCusHst (Account,cFisFYear) VALUES (laData[2],STR(VAL(gcCurrYear)+1,4))
ENDIF

DECLARE laCusHist[15,14],laHeader[14]
STORE '' TO laCusHist
STORE 1  TO lnCol
laHeader[1]  = 'Order '+ SET('CURR',1)
laHeader[2]  = 'Sales '+ SET('CURR',1)
laHeader[3]  = 'COGS '+ SET('CURR',1)
laHeader[4]  = 'Discount '+ SET('CURR',1)
laHeader[5]  = 'Returns '+ SET('CURR',1)
laHeader[6]  = 'Profit '+ SET('CURR',1)
laHeader[7]  = 'Allowance '+ SET('CURR',1)
laHeader[8]  = 'Net Profit '+ SET('CURR',1)
laHeader[9]  = 'Order Quantity'
laHeader[10] = 'Sales Quantity'
laHeader[11] = 'Returns Quantity'
laHeader[12] = 'Payments '+ SET('CURR',1)
laHeader[13] = 'Debit Adjust. '+ SET('CURR',1)
laHeader[14] = 'Credit Adjust. '+ SET('CURR',1)
SELECT cFisFYear FROM arCusHst WHERE Account+cFisFYear=laData[2] INTO ARRAY laHistYear
IF _TALLY > 0
  =lfvHistYear(gcCurrYear)
  lnHistYear = ASCAN(laHistYear,gcCurrYear)
  lnHistYear = MAX(lnHistYear,1)
  lnCol = 1
  lcFisYear = 'Current'
  IF lcToDisp = 'H'
    
    *B130389,1 EIH 01/07/2006 Check if the user not have costing information privilege we delete the columns "COGS,PROFIT,
    *B130389,1 EIH            NET PROFIT" from the screen [Begin]
    IF !LLCOSTPRV
      =GfADel(@laHeader,8,1)
      =GfADel(@laHeader,6,1)
      =GfADel(@laHeader,3,1)
    ENDIF  
    *B130389,1 EIH [End]
    
    DO (gcScrDir+"ARCUSHST.SPR")
  ELSE
    =lfvGraph() 
  ENDIF  
ENDIF
*E301077,49 Inhance openning files to speed up transaction
=gfCloseFile('FISHD')
=gfCloseFile('ARCUSHST')
*E301077,49 (End)

*!*************************************************************
*! Name      : lfvHistNext
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Get next field in the customer history screen
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvHistNext()
*!*************************************************************
FUNCTION lfvHistNext
lnCOl = lnCol + 1
SHOW GET pbPrevHist ENABLE

*B130389,1 EIH 01/07/2006 Check if the user haven't costing privilege we change the col's count displaying of the screen [Begin]
*IF lnCol = 10
lnFirstCol  = IIF(!LLCOSTPRV, 7,10)
IF lnCol = lnFirstCol 
*B130389,1 EIH [End]

  SHOW GET pbNextHist DISABLE
ENDIF  
=lfRefresh('ARCUSHST')

*!*************************************************************
*! Name      : lfvHistPrev
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Get previous  field in the customer history screen
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvHistPrev()
*!*************************************************************
FUNCTION lfvHistPrev
lnCOl = lnCol - 1
SHOW GET pbNextHist ENABLE
IF lnCol = 1 
  SHOW GET pbPrevHist DISABLE
ENDIF  
=lfRefresh('ARCUSHST')

*!*************************************************************
*! Name      : lfvHistYear
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Get fiscal year in the customer history screen
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvHistYear()
*!*************************************************************
FUNCTION lfvHistYear
PARAMETERS lcYear
PRIVATE lnCount,lcCount

*B130389,1 EIH 01/07/2006 Reset laCusHist variable [Begin].
DECLARE laCusHist[15,14]
STORE '' TO laCusHist
*B130389,1 EIH [End].

SELECT ARCUSHST
=SEEK(laData[2]+lcYear)
FOR lnCount = 1 TO 13
  lcCount = PADL(lnCount,2,'0')
  laCusHist[lnCount,1]  = STR(ROUND(nOrdAmt&lcCount,0),12)
  laCusHist[lnCount,2]  = STR(ROUND(nSlsAmt&lcCount,0),12)
  laCusHist[lnCount,3]  = STR(ROUND(nCOGSAmt&lcCount,0),12)
  laCusHist[lnCount,4]  = STR(ROUND(nDisAmt&lcCount,0),12)
  laCusHist[lnCount,5]  = STR(ROUND(nRetAmt&lcCount,0),12)
  laCusHist[lnCount,6]  = STR(ROUND(nSlsAmt&lcCount-nCOGSAmt&lcCount-nDisAmt&lcCount-nRetAmt&lcCount,0),12)
  laCusHist[lnCount,7]  = STR(ROUND(nAllow&lcCount,0),12)
  laCusHist[lnCount,8]  = STR(ROUND(nSlsAmt&lcCount-nCOGSAmt&lcCount-nDisAmt&lcCount-nRetAmt&lcCount-nAllow&lcCount,0),12)  
  laCusHist[lnCount,9]  = STR(ROUND(nOrdQty&lcCount,0),11)
  laCusHist[lnCount,10] = STR(ROUND(nSlsQty&lcCount,0),11)
  laCusHist[lnCount,11] = STR(ROUND(nRetQty&lcCount,0),11)
  laCusHist[lnCount,12] = STR(ROUND(nPayment&lcCount,0),12)
  laCusHist[lnCount,13] = STR(ROUND(nDrAdj&lcCount,0),12)
  laCusHist[lnCount,14] = STR(ROUND(nCrAdj&lcCount,0),12)
ENDFOR
laCusHist[14,1]  = STR(ROUND(nOrdAmt,0),12)
laCusHist[14,2]  = STR(ROUND(nSlsAmt,0),12)
laCusHist[14,3]  = STR(ROUND(nCOGSAmt,0),12)
laCusHist[14,4]  = STR(ROUND(nDisAmt,0),12)
laCusHist[14,5]  = STR(ROUND(nRetAmt,0),12)
laCusHist[14,6]  = STR(ROUND(nSlsAmt-nCOGSAmt-nDisAmt-nRetAmt,0),12)
laCusHist[14,7]  = STR(ROUND(nAllow,0),12)
laCusHist[14,8]  = STR(ROUND(nSlsAmt-nCOGSAmt-nDisAmt-nRetAmt-nAllow,0),12)
laCusHist[14,9]  = STR(ROUND(nOrdQty,0),11)
laCusHist[14,10] = STR(ROUND(nSlsQty,0),11)
laCusHist[14,11] = STR(ROUND(nRetQty,0),11)
laCusHist[14,12] = STR(ROUND(nPayment,0),12)
laCusHist[14,13] = STR(ROUND(nDrAdj,0),12)
laCusHist[14,14] = STR(ROUND(nCrAdj,0),12)
=SEEK(lcYear,'FISHD')
laCusHist[15,1]  = STR(ROUND(nOrdAmt/VAL(FISHD.cFisNoPrd),0),12)
laCusHist[15,2]  = STR(ROUND(nSlsAmt/VAL(FISHD.cFisNoPrd),0),12)
laCusHist[15,3]  = STR(ROUND(nCOGSAmt/VAL(FISHD.cFisNoPrd),0),12)
laCusHist[15,4]  = STR(ROUND(nDisAmt/VAL(FISHD.cFisNoPrd),0),12)
laCusHist[15,5]  = STR(ROUND(nRetAmt/VAL(FISHD.cFisNoPrd),0),12)
laCusHist[15,6]  = STR(ROUND((nSlsAmt-nCOGSAmt-nDisAmt-nRetAmt)/VAL(FISHD.cFisNoPrd),0),12)
laCusHist[15,7]  = STR(ROUND(nAllow/VAL(FISHD.cFisNoPrd),0),12)
laCusHist[15,8]  = STR(ROUND((nSlsAmt-nCOGSAmt-nDisAmt-nRetAmt-nAllow)/VAL(FISHD.cFisNoPrd),0),12)
laCusHist[15,9]  = STR(ROUND(nOrdQty/VAL(FISHD.cFisNoPrd),0),11)
laCusHist[15,10] = STR(ROUND(nSlsQty/VAL(FISHD.cFisNoPrd),0),11)
laCusHist[15,11] = STR(ROUND(nRetQty/VAL(FISHD.cFisNoPrd),0),11)
laCusHist[15,12] = STR(ROUND(nPayment/VAL(FISHD.cFisNoPrd),0),12)
laCusHist[15,13] = STR(ROUND(nDrAdj/VAL(FISHD.cFisNoPrd),0),12)
laCusHist[15,14] = STR(ROUND(nCrAdj/VAL(FISHD.cFisNoPrd),0),12)
lcFisYear = IIF(FISHD.cFisYStat='H','History',IIF(FISHD.cFisYStat='P','Previous',IIF(FISHD.cFisYStat='C','Current','Next')))

*B130389,1 EIH 01/07/2006 Check if the user not have costing information privilege we delete the columns "COGS,PROFIT,
*B130389,1 EIH            NET PROFIT" from the screen [Begin]
IF !LLCOSTPRV
  =GfADel(@laCusHist,8,2)
  =GfADel(@laCusHist,6,2)
  =GfADel(@laCusHist,3,2)
ENDIF  
*B130389,1 EIH [End]


*!*************************************************************
*! Name      : lfClsTmpFiles
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Close temporary files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClsTmpFiles()
*!*************************************************************
FUNCTION lfClsTmpFiles

IF USED(lcCustDept)
  USE IN (lcCustDept)
ENDIF  
IF USED(lcContFile)
  USE IN (lcContFile)
ENDIF
IF USED(lcProFile)
  USE IN (lcProFile)
ENDIF  
IF USED(lcProVal)
  USE IN (lcProVal)
ENDIF
*E301077,49 Inhance openning files to speed up transaction
=IIF(!laReBrowse[1],gfCloseFile('CONTACT'),.T.)
=IIF(!laReBrowse[2],gfCloseFile('arPrFCod') AND gfCloseFile('ProFile'),.T.)
=IIF(!laReBrowse[3] OR !laReBrowse[4],gfCloseFile('syschdul'),.T.)
*B803388,1 AME[Start]
IF !laReBrowse[4]
  lnali = SELECT()
  SELECT (lcHistFile)
  CLOSE INDEXES
  ERASE (GcWorDir)+lcHistIndx..CDX
  SELECT (lnali)
ENDIF
*B803388,1 AME[End]
=IIF(!laReBrowse[4],gfCloseFile(lcHistFile),.T.)
*E301077,49 (End)
*C102589,1 TMI [START] ZAP lcCONTROLE,lcCONTFILE FILES
IF USED(lcContFile)
  *C102589,4 TMI [START]
  *SELECT (lcContFile)
  *ZAP
  USE IN (lcContFile)
  *C102589,4 TMI [END  ]
ENDIF
IF USED(lcContRole)
  *C102589,4 TMI [START]
  *SELECT (lcContRole)  
  *ZAP
  USE IN (lcContRole)  
  *C102589,4 TMI [END  ]
ENDIF  
*C102589,1 TMI [END  ] ZAP CONTROLE FILE
*!*************************************************************
*! Name      : lpClsScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Cancel new Invoice
*!*************************************************************
*! Calls     : lfClsTmpFiles
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpClsScr()
*!*************************************************************
FUNCTION lpClsScr

=lfClsTmpFiles()
SELECT CUSTOMER
IF laScrMode[4]
  SCATTER FIELDS &lcScFields TO laData BLANK
ELSE
  =SEEK(laData[1]+laData[2]+laData[3])
  SCATTER FIELDS &lcScFields TO laData
ENDIF

*!*************************************************************
*! Name      : lfvCusAge
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Age customer
*!*************************************************************
*! Calls     : gfGetExSin,lfvAging,lfFinBrow
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCusAge()
*!*************************************************************
FUNCTION lfvCusAge
PRIVATE lnCur,lnAge30,lnAge60,lnAge90,lnAge120,lnOpnCr,lnChgBack,lcUnitSign,;
        lnTCur,lnTAge30,lnTAge60,lnTAge90,lnTAge120
lcUnitSign = '/' 
STORE 0.00 TO lnCur,lnAge30,lnAge60,lnAge90,lnAge120,lnOpnCr,lnChgBack,;
              lnTCur,lnTAge30,lnTAge60,lnTAge90,lnTAge120
*Age debits
SELECT DEBIT
IF SEEK(lcAccount)
  SCAN REST WHILE Account = lcAccount
    lcRateSign = IIF(llMulCurr,gfGetExSin(@lcUnitSign, cCurrCode),'/')
    lnExRate   = IIF(llMulCurr,nExRate,1)
    lnCurrUnit = IIF(llMulCurr,nCurrUnit,1)

    * This computes the age of a transaction based on the tran
    * due date. The due date is computed based on the data in the code
    * file for terms.
    lnDays = gdSysDate - IIF(EMPTY(DueDate),TranDate+30,DueDate)
    DO CASE
      CASE lnDays >= 91
        lnTAge120 = lnTAge120 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 61
        lnTAge90 = lnTAge90 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 31
        lnTAge60 = lnTAge60 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 1
        lnTAge30 = lnTAge30 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      OTHERWISE
        lnTCur = lnTCur + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
    ENDCASE

    * This computes the age of a transaction based on the tran date
    lnDays = gdSysDate - Debit.TranDate
    DO CASE
      CASE lnDays >= 120
        lnAge120 = lnAge120 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 90
        lnAge90 = lnAge90 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 60
        lnAge60 = lnAge60 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      CASE lnDays >= 30
        lnAge30 = lnAge30 + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
      OTHERWISE
        lnCur = lnCur + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
    ENDCASE

    IF TranType = '3'
      lnChgBack = lnChgBack + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
    ENDIF
  ENDSCAN
ENDIF
lnAging = lnCur+lnAge1+lnAge2+lnAge3+lnAge4
*Aging credits
IF SEEK(lcAccount,'CREDIT')
  SELECT CREDIT
  SCAN REST WHILE Account = lcAccount
    lcRateSign = IIF(llMulCurr,gfGetExSin(@lcUnitSign, cCurrCode),'/')
    lnExRate   = IIF(llMulCurr,nExRate,1)
    lnCurrUnit = IIF(llMulCurr,nCurrUnit,1)
    lnOpnCr = lnOpnCr + Amount &lcRateSign lnExRate &lcUnitSign lnCurrUnit
  ENDSCAN
ENDIF
SELECT CUSTOMER
=SEEK('M'+lcAccount)
=RLOCK()
REPLACE AgeDate  WITH gdSysDate ,;
        CURRENT  WITH lnCur     ,;
        AGE30    WITH lnAge30   ,;
        AGE60    WITH lnAge60   ,;
        AGE90    WITH lnAge90   ,;
        AGE120   WITH lnAge120  ,;
        TOTAGE   WITH lnCur+lnAge30+lnAge60+lnAge90+lnAge120 ,;
        OPENCR   WITH lnOpnCr   ,;
        CHGBACK  WITH lnChgBack ,;
        NETBAL   WITH TOTAGE + OPENCR ,;
        TERCURRENT    WITH lnTCur   ,;
        TERAGE30      WITH lnTAge30 ,;
        TERAGE60      WITH lnTAge60 ,;
        TERAGE90      WITH lnTAge90 ,;
        TERAGE120     WITH lnTAge120
UNLOCK

*E301245 function to update nHgWtrMark Field with NETBAL field [Begin.]
=lfHgWUpdat() 
*E301245 function to update nHgWtrMark Field with NETBAL field [End.]

IF !EMPTY(lcStore)
  =SEEK('S'+lcAccount+lcStore)
ENDIF
=IIF(llMulCurr,lfvAging(laCurrency[puCurrency],cbCurrency),;
               lfvAging(gcBaseCurr,.T.))
=IIF(llMulCurr,lfFinBrow(laCurrency[puCurrency],cbCurrency),;
               lfFinBrow(gcBaseCurr,.T.))
=lfRefresh('ARCUSFN0')

*!*************************************************************
*! Name      : lfvSchedule
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Add/Modify Customer Schedule
*!*************************************************************
*! Calls     : TASKTYP
*!*************************************************************
*! Parameters: lcType   'A'  Add
*!                      'M'  Modify
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSchedule()
*!*************************************************************
FUNCTION lfvSchedule
PARAMETERS lcType
PRIVATE lcWinAppl,laCodes

lcType = IIF(lcType='M' .AND. !llEditRec,'V',lcType)
lcWinAppl = gcWinAppl
gcWinAppl = 'SM'
DO (gcAppHome+'SM\TASKTYP') WITH lcType,;
IIF(lcType='A','',Syschdul.cSeqNumber),1,'C',laData[2],laData[3],;
EVAL('gcUser_Id'),EVAL('gdSysDate')
gcWinAppl = lcWinAppl
SELECT SySchdul
SET ORDER TO TAG SchDate ASCENDING
SHOW WINDOW (lcPendTitl) REFRESH SAME

*!*************************************************************
*! Name      : lfvRemSch
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Remove Customer Schedule
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvRemSch()
*!*************************************************************
FUNCTION lfvRemSch
*E300455,1 Message : 40062
*E300455,1 Are you sure you want to delete the current open activity from pending?
*E300455,1 Button : 40000
*E300455,1 Yes No
IF gfModalGen('QRM40062B40000','ALERT','open|pending') = 1
  SELECT syschdul
  DELETE
  IF !SEEK('C'+PADR(laData[2],8)+laData[3]+'N')
    SHOW GET pbNModify DISABLE
    SHOW GET pbNRemove DISABLE
  ENDIF
  SHOW WINDOW (lcPendTitl) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvSchHist
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Modify Customer History
*!*************************************************************
*! Calls     : TASKCOM
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSchHist()
*!*************************************************************
FUNCTION lfvSchHist

lcWinAppl = gcWinAppl
gcWinAppl = 'SM'
DO (gcAppHome+'SM\TASKCOM') WITH IIF(llEditRec,'M','V'),&lcHistFile..cSeqNumber,0,.F.
gcWinAppl = lcWinAppl

*!*************************************************************
*! Name      : lfvRemHst
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Remove Customer History
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvRemHst()
*!*************************************************************
FUNCTION lfvRemHst
*E300455,1 Message : 40062
*E300455,1 Are you sure you want to delete the current completed activity from history?
*E300455,1 Button : 40000
*E300455,1 Yes No
IF gfModalGen('QRM40062B40000','ALERT','completed|history') = 1
  SELECT (lcHistFile)
  DELETE
  IF !SEEK('C'+PADR(laData[2],8)+laData[3]+'Y')
    SHOW GET pbHModify DISABLE
    SHOW GET pbHRemove DISABLE
  ENDIF
  SHOW WINDOW (lcHistTitl) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvDistCntr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate store distribution center
*!*************************************************************
*! Calls     : CUSBROWS
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvDistCntr()
*!*************************************************************
FUNCTION lfvDistCntr
PRIVATE xStore
IF llBrowse .OR. (!EMPTY(laData[74]) .AND. !SEEK('S'+laData[2]+laData[74],'Customer'))
  xStore   = laData[74]
  IF !CUSBROWS(laData[2],.T.)
    STORE SPACE(8) TO xStore
  ENDIF
  laData[74] = xStore
ENDIF  
=SEEK(laData[1]+laData[2]+laData[3],'Customer')
llBrowse = .F.

*!*************************************************************
*! Name      : lfvGraph
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show customer history graph
*!*************************************************************
*! Calls     : SYGRAPH
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvGraph()
*!*************************************************************
FUNCTION lfvGraph

*E301077,49 Inhance openning files to speed up transaction
*B603871,1 AMH Comment the next line , ARCUSHST is opened from lfvCusHst [Start]
*=gfOpenFile(gcDataDir+'ARCUSHST',gcDataDir+'ACTHST','SH')
*B603871,1 AMH Comment the next line , ARCUSHST is opened from lfvCusHst [End  ]
*E301077,49 (End)

SELECT (lcHistPrd)
ZAP

=SEEK(laData[2]+laHistYear[lnHistYear],'arCusHst')
FOR lnCount = 1 TO 13
  lcCount = PADL(lnCount,2,'0')
  INSERT INTO (lcHistPrd) (Period,Order,Sales,Discount,Return,COGS);
  VALUES ('P'+lcCount,arCusHst.nOrdAmt&lcCount,arCusHst.nSlsAmt&lcCount,;
  arCusHst.nDisAmt&lcCount,arCusHst.nRetAmt&lcCount,arCusHst.nCOGSAmt&lcCount)
ENDFOR
SELECT (lcHistPrd)
GO TOP


*B130389,1 EIH 01/07/2006 Check if the user not have costing information privilege we delete the columns "COGS"
*B130389,1 EIH            From the Graph [Begin]
*=SYGRAPH(@lnGraphTyp,@lcGrfFields,@llByRow,'CUSGRAPH','Period,Order','Period,Order,Sales,Discount,Return,COGS')
IF LLCOSTPRV
  =SYGRAPH(@lnGraphTyp,@lcGrfFields,@llByRow,'CUSGRAPH','Period,Order','Period,Order,Sales,Discount,Return,COGS')
ELSE
  =SYGRAPH(@lnGraphTyp,@lcGrfFields,@llByRow,'CUSGRAPH','Period,Order','Period,Order,Sales,Discount,Return')  
ENDIF
*B130389,1 EIH [End]

*E301077,49 Inhance openning files to speed up transaction
*B603871,1 AMH Comment the next line , ARCUSHST is opened from lfvCusHst [Start]
*=gfCloseFile('ARCUSHST')
*B603871,1 AMH Comment the next line , ARCUSHST is opened from lfvCusHst [End  ]
*E301077,49 (End)

*!*************************************************************
*! Name      : lfvHistDet
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse history details
*!*************************************************************
*! Calls     : lfCusHist
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvHistDet()
*!*************************************************************
FUNCTION lfvHistDet
PRIVATE lcHistNo,lcBrowTtl,lcBrFields
DO CASE
  CASE lnActFolder = 1
    lcHistNo = &lcHistory..History
    RELEASE WINDOW (lcAccHist)
    IF llHistDet
      lcAccHist  = 'Customer History'
      llHistDet = .F.
      SHOW GET pbHistDet,1 PROMPT '\<Details'
      SELECT (lcHistory)
      *B802388,1 ABD Cheak if browse for first time , go to top. [Begin]
      IF lnLastRec = 1
        GO TOP
      ELSE  
        GOTO lnLastRec
      ENDIF
      *B802388,1 ABD [End]
      
      lnHistory = RECNO()
      *B802388,1 ABD Add Function to save last Selected recored .[Begin]      
      *BROWSE FIELDS ;
             cMarker =IIF(RECNO()=lnHistory,'>',' '):H=' ':R:1:W=.F.,;
             History :H='History'      :R,;
             HistDate:H='Key-off Date' :R,;
             TotDb   :H='Total Debits' :R,;
             TotCr   :H='Total Credits':R,;
             OpenAmt :H='Open Amount'  :R;
             WINDOW (lcCusFin1);
             IN WINDOW (lcCusFin4);
             NOMENU           ;         
             NOAPPEND         ;
             NODELETE         ;         
             NOWAIT           ;
             SAVE             ;
             NOCLEAR          ;
             WHEN lfCusHist() ;
             TITLE lcAccHist
      
      BROWSE FIELDS ;
             cMarker =IIF(RECNO()=lnHistory,'>',' '):H=' ':R:1:W=lfSavRec(),;
             History :H='History'      :R,;
             HistDate:H='Key-off Date' :R,;
             TotDb   :H='Total Debits' :R,;
             TotCr   :H='Total Credits':R,;
             OpenAmt :H='Open Amount'  :R;
             WINDOW (lcCusFin1);
             IN WINDOW (lcCusFin4);
             NOMENU           ;         
             NOAPPEND         ;
             NODELETE         ;         
             NOWAIT           ;
             SAVE             ;
             NOCLEAR          ;
             WHEN lfCusHist() ;
             TITLE lcAccHist
      *B802388,1 ABD [End]
    ELSE
      lcAccHist  = 'Customer History# '+lcHistNo+' Details'
      llHistDet = .T.
      SHOW GET pbHistDet,1 PROMPT '\<History'
      SELECT ARHIST1
      =SEEK(lcAccount+lcHistNo)
      lnHistory = RECNO()
      
      *B802622,1 ABD Add filed transaction type to table to browse in the screen. [Begin]
      *BROWSE FIELDS ;
             cMarker =IIF(RECNO()=lnHistory,'>',' '):H=' ':R:1:W=.F.,;
             tran      :R :H='Tran.#',;
             trandate  :R :H='TranDate',;
             batch     :R :H='Batch#',;
             desc      :R :H='Description',;
    	     reference :R :27 :H='Reference',;               
             Store     :R :H='Chk/Str#.',;
             amount    :R :H='Amount';
             WINDOW (lcCusFin1);
             IN WINDOW (lcCusFin4);
             NOMENU           ;         
             NOAPPEND         ;
             NODELETE         ;         
             NOWAIT           ;
             SAVE             ;
             NOCLEAR          ;
             KEY lcAccount+lcHistNo ;
             WHEN lfCusHist() ;
             TITLE lcAccHist
      *B802622,1 ABD Add filed transaction type to table to browse in the screen.[Begin]
      BROWSE FIELDS ;
             cMarker =IIF(RECNO()=lnHistory,'>',' '):H=' ':R:1:W=.F.,;
             tran      :R :H='Tran.#'                               ,;
             trandate  :R :H='TranDate'                             ,;
             batch     :R :H='Batch#'                               ,;
             TranType  =lfvDesType() :H='Adjustment Reason'         ,;
             desc      :R :H='Description'                          ,;
             Store     :R :H='Chk/Str#.'                            ,;
             amount    :R :H='Amount'                               ,;
             reference :R :27 :H='Reference'                         ;
             WINDOW (lcCusFin1)                                      ;
             IN WINDOW (lcCusFin4)                                   ;
             NOMENU                                                  ;
             NOAPPEND                                                ;
             NODELETE                                                ;
             NOWAIT                                                  ;
             SAVE                                                    ;
             NOCLEAR                                                 ;
             KEY lcAccount+lcHistNo                                  ;
             WHEN lfCusHist()                                        ;
             TITLE lcAccHist
      *B802622,1 ABD [End]
    ENDIF
  CASE lnActFolder = 2 .AND. &lcDebit..TRANTYPE='1'
    lcInvoice = "'"+&lcDebit..TRAN+"'"
    CLEAR READ
ENDCASE

*!*************************************************************
*! Name      : lpShowPDC
*! Developer : Walid abou el-magd
*! Date      : 04/21/99
*! Purpose   : control the appearance of button <Post Dated Check> and the indicator
*!*************************************************************
*! Called from     : On refersh of ARCUSFN screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpShowPDC()
*!*************************************************************
*C101427,1
FUNCTION lpShowPDC
IF SEEK(lcAccount,'PostDChq') &&-- Red indicator [controled in ARCUSFN screen]
  lcPostStat="ENABLE"
ELSE
  lcPostStat="DISABLE" &&-- Gray indicator [controled in ARCUSFN screen]
ENDIF    
SHOW GET PbPDC &lcPostStat
*-- end of lpShowPDC.

*!*************************************************************
*! Name      : lfGetBar
*! Developer : Reham Al-Allamy
*! Date      : 05/10/1999
*! Purpose   : Function to return the bar no of the current bar.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcFunction
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lfGetBar
*!*************************************************************
*B602754,1 Reham On 05/11/1999
*B602754,1 Seek with the sent parameter for its bar in the menu file.
*!*************************************************************
*
FUNCTION lfGetBar
PARAMETERS lcFunction
PRIVATE lnRetVal , lcFunction

*-- Open the menu file to get the bar no. of the save bar.
USE (gcSysHome+"SYCMenu") IN 0 ORDER Pross_ID AGAIN ALIAS MenuFile

lnRetVal = IIF(SEEK(UPPER(lcFunction), "MenuFile"), VAL(MenuFile.cBar_Pos), 51)

USE IN MenuFile

RETURN (lnRetVal)

*!**************************************************************************
*! Name      : lfvSumInf
*! Developer : SSE
*! Date      : 07/06/1999
*! Purpose   : opens Summary information screen 
*!**************************************************************************
*! Calls     : lfRefresh
*!**************************************************************************
*! Passed Parameters  :  None
*!**************************************************************************
*! Returns            :  None
*!**************************************************************************
*! Example            :  =lfvSumInf()
*!**************************************************************************
*E301246,1
FUNCTION lfvSumInf
PRIVATE lnRecSav

lnRecSav    = RECNO('CUSTOMER')  && to save the current record in customer file 

=SEEK('M'+laData[2],'CUSTOMER')    && get the Main Account
lnHgwtrmrk = CUSTOMER.nHgWtrMark
lnPst12Avg = CUSTOMER.nPast12Avg 

DO (gcScrDir+'ARSUMINF.SPX')     && display the Summary Information Screens 

GOTO lnRecSav IN CUSTOMER        && restore the old record no.

*--End of lfvSumInf.


*!**************************************************************************
*! Name      : lfHgWUpdat
*! Developer : Sameh (SSE)
*! Date      : 06/09/1999
*! Purpose   : Update nHgWtrMark field (Customer) with NETBAL, if NETBAL is greater
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfHgWUpdat()
*!*************************************************************************
*E301245,1
FUNCTION lfHgWUpdat
=RLOCK()
REPLACE nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
UNLOCK
*-- End of lfHgWUpdat.

*!**************************************************************************
*! Name      : lfvDesType
*! Developer : Abdou ElGendi (ABD)
*! Date      : 07/31/2000
*! Purpose   : Print the description Of the Adjustment Reason.
*!*************************************************************************
*! Calls     : None.
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfvDesType()
*!*************************************************************************
*B802622,1 ABD 

FUNCTION lfvDesType
Do Case
  CASE TranType = '0'
    Return 'Returns'
  CASE TranType = '1'
    Return 'Direct Invoice'
  CASE TranType = '2'
    Return 'Debit Adjustment'
  CASE TranType = '3'
    Return 'Debit On Account'
  CASE TranType = '4'
    Return 'Payment'
  CASE TranType = '5'
    Return 'Credit Adjustment'
  CASE TranType = '6'
    Return 'Credit On Account'
  CASE TranType = '7'
    Return 'Credit Adjustment'
  CASE TranType = '8'
    Return 'Charge Back'
  CASE TranType = '9'
    Return 'Credit On Account'
ENDCASE

*- End Of lfvDesType.
*!**************************************************************************
*! Name      : lfSavRec
*! Developer : Abdou ElGendi (ABD)
*! Date      : 07/31/2000
*! Purpose   : Save Last Recored
*!*************************************************************************
*! Calls     : None.
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfSavRec()
*!*************************************************************************
*B802388,1 
Function lfSavRec
lnLastRec = RecNo()

*- End Of lfSavRec.
*!*************************************************************
*! Name      : lfvStorGLN
*! Developer : Ahmed Mohamed ElAnwar (AME)
*! Date      : 03/19/2001
*! Purpose   : Validate store GLN.
*!*************************************************************
*! Calls     : None.
*!*************************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStorGLN()
*!*************************************************************
*E301581,1
FUNCTION lfvStorGLN
IF laData[78] # lcOldValue
  lcOldAlias = SELECT(0)
  SELECT CUSTOMER
  lnRecNo = RECNO()
  lcOldTag = ORDER()
  SET ORDER TO StoreGLN
  IF SEEK('S'+laData[2]+laData[78])
    * Message : Custom
    * This Store GLN has been entered to this account before.
    * Button : 00000
    * Ok
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"GLN " + ALLTRIM(laData[78]) + " already assigned to store " + STORE )
    laData[78] = lcOldValue
  ENDIF
  SET ORDER TO lcOldTag
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNo
  ENDIF
  SELECT (lcOldAlias)
ENDIF
*- End Of lfvStorGLN.

*:**************************************************************************
*:* Name        : lfvRole
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/26/2002
*:* Purpose     : Valid function to add/remove roles to a contact
*:***************************************************************************
*:* Called from : Contact screen
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvRole()
*:* Refer to    : C102589,1
*:***************************************************************************
FUNCTION lfvRole
*C102589,4 TMI [START] Call this function from lfvContact to fill the laTarget Array
PARAMETERS llFromCont
*--if user try to click role btn when he modified the Contact , then prevent him
IF llRole
  SHOW GET pbRole     DISABLE  && DISABLE Role button
  llRole = .F.
  RETURN
ENDIF
*C102589,4 TMI [END  ] Call this function from lfvContact to fill the laTarget Array
PRIVATE lcAlias,lnI,lnTrgt,lnRolPos,llOk

lcAlias = SELECT(0)
*C102589,4 TMI [START] Call this function from lfvContact to fill the laTarget Array
IF llFromCont
*C102589,4 TMI [END  ] Call this function from lfvContact to fill the laTarget Array
  *--Open CONTROLE file
  IF !USED('CONTROLE')
    =gfOpenFile(gcDataDir+'CONTROLE',gcDataDir+'CONTROLE','SH')
  ENDIF
  =lfCrTmpRol()  && Create the temp file lcContRole 
  
  *--Clear selected roles
  IF ASCAN(laRole,'\') > 0
    FOR lnI = 1 TO ALEN(laRole,1)
      IF LEFT(laRole[lnI],1) = '\'
        laRole[lnI] = SUBSTR(laRole[lnI],2)
      ENDIF
    ENDFOR
  ENDIF
  
  *--Fill the temp file
  IF !laScrMode[2]
    SELECT CONTROLE
    =SEEK('C'+PADR(laData[2],8)+laData[3]+m.CONTACT,'CONTROLE')
    SCAN REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE = 'C'+PADR(laData[2],8)+laData[3]+m.CONTACT    
      IF !SEEK('C'+PADR(laData[2],8)+laData[3]+m.CONTACT+CONTROLE.CROLE,lcContRole)
        INSERT INTO (lcContRole) (CCONTTYPE,CCONT_ID,STORE,CONTACT,CROLE) VALUES ;
                       ('C',PADR(laData[2],8),laData[3],m.CONTACT,CONTROLE.CROLE)
      ENDIF
    ENDSCAN
  ENDIF
  
  *--Empty Target array
  DIMENSION laTarget[1]
  laTarget[1] = SPACE(0)
  
  *--Then ReFill it.
  lnTrgt = 0
  SELECT IIF(laScrMode[2],'CONTROLE',lcContRole)
  =SEEK('C'+PADR(laData[2],8)+laData[3]+m.CONTACT)
  SCAN REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE = ;
                    'C'+PADR(laData[2],8)+laData[3]+m.CONTACT FOR IIF(laScrMode[2],.T.,CMARK # 'D')
    lnRolPos = ASCAN(laRole,CROLE+' - ')
    IF lnRolPos > 0
      lnTrgt = lnTrgt + 1
      DIMENSION laTarget[lnTrgt]
      laTarget[lnTrgt] = laRole[lnRolPos]
      laRole[lnRolPos] = '\'+laRole[lnRolPos]
    ENDIF    
  ENDSCAN
  
*C102589,4 TMI [START] end of above If statement
  SELECT (lcAlias)
  RETURN
ENDIF
*C102589,4 TMI [END  ] end of above If statement
*-- Call mover function.
=gfMover(@laRole,@laTarget,'Select Roles', !laScrMode[2] ,'')
IF laScrMode[2]
  SELECT (lcAlias)
  RETURN .F.
ENDIF
SHOW GET m.Contact DISABLE

*--Mark all records of this contact as deleted
SELECT (lcContRole)
=SEEK('C'+PADR(laData[2],8)+laData[3]+m.CONTACT,lcContRole))
REPLACE CMARK WITH 'D' REST ;
WHILE  CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE = 'C'+PADR(laData[2],8)+laData[3]+m.CONTACT

*--Update lcContRole from the laTarget Array
IF IIF(ALEN(laTarget,1)=1,!EMPTY(laTarget[1]),.T.)
  FOR lnI = 1 TO ALEN(laTarget,1)
    IF SEEK('C'+PADR(laData[2],8)+laData[3]+m.CONTACT+PADR(laTarget[lnI],6),lcContRole)
      REPLACE CMARK WITH ' '
    ELSE
      INSERT INTO (lcContRole) VALUES ;
      ('C',PADR(laData[2],8),laData[3],m.CONTACT,PADR(laTarget[lnI],6),' ')
    ENDIF
  ENDFOR
ENDIF
SELECT (lcAlias)
*-- end of lfvRole.

*:**************************************************************************
*:* Name        : lfCrTmpRol
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/31/02
*:* Purpose     : Create temp file for Roles
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrTmpRol()
*:***************************************************************************
FUNCTION lfCrTmpRol
IF !USED(lcContRole)
  CREATE TABLE (gcWorkDir+lcContRole) ;
  (CCONTTYPE C(1),CCONT_ID C(8),STORE C(8),CONTACT C(30),CROLE C(6),CMARK C(1))
  *-- CMARK :  "D" ==> Delete ; " " ==> New
  INDEX ON CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE TAG (lcContRole)
ENDIF
*-- end of lfCrTmpRol.

*:**************************************************************************
*:* Name        : lfChkDC                                         *B607403,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/16/2003
*:* Purpose     : if this store is a DC for another store , deny deleting
*:***************************************************************************
*:* Called from : lpDelScr
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : .T. if this store is a DC
*:***************************************************************************
*:* Example     :  = lfChkDC()
*:***************************************************************************
FUNCTION lfChkDC
PARAMETERS lcDCStore
PRIVATE lcSlct,llDC
lcSlct = SELECT()
llDC = .F.

*-- for case laData[1] = 'S'  && Deleting a store
*-  we need only to check if this store is a DC for any other store

*-- case laData[1] = 'M'  &&Deleting a main account
*-      llDelMain is .T.    && All  stores  , all will be deleted , no checks needed here
*-      llDelMain is .F.    && Some stores  , if current store is a DC for a store not in the deleted list , then deny deletion

DO CASE
  CASE laData[1] = 'S'
    SELECT &lcTmpCst
    =SEEK('S'+laData[2],lcTmpCst)
    LOCATE REST WHILE TYPE+ACCOUNT+STORE = 'S'+laData[2] ;
                FOR DIST_CTR = lcDCStore
    IF FOUND()
      llDC = .T.
    ENDIF              

  CASE laData[1] = 'M'
    IF !llDelMain      
      SELECT &lcTmpCst
      =SEEK('S'+laData[2],lcTmpCst)
      SCAN REST WHILE TYPE+ACCOUNT+STORE = 'S'+laData[2] ;
                FOR DIST_CTR = lcDCStore
        IF SEEK(' '+laData[2]+&lcTmpCst..STORE,lcTmpDlRec)
          llDC = .T.
        ENDIF
      ENDSCAN
    ENDIF
ENDCASE

SELECT (lcSlct)
RETURN llDC
*-- end of lfChkDC.
*:*************************************************************
*: Name      : lfOldValue
*: Developer : NADER NABIL (NNA)
*: Date      : 05/05/2005
*: Purpose   : Function to store old value of the current filed.
*:*************************************************************
*: Calls     : Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfoldvalue()
*:*************************************************************
*:B127178,1
FUNCTION lfoldvalue
lnOldValue = EVALUATE(SYS(18))

*-- End of Function lfoldvalue.
*:*************************************************************
*: Name      : lfvCancel
*: Developer : NADER NABIL (NNA)
*: Date      : 05/05/2005
*: Purpose   : Validation function to validate the customer cancellation
*:*************************************************************
*: Calls     : Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvCancel()
*:*************************************************************
*:B127178,1
FUNCTION lfvCancel
*B039299,1  TMI [Start] Allow the customer to change status to Hold,Potential, Active
IF lnStatus<>4   && Current statuses : 'PAHX'
  RETURN .T.
ENDIF 

PRIVATE lcAccStors,lnSlct
lcAccStors = gfTempName()
lnSlct = SELECT()

SELECT STORE,DIST_CTR FROM CUSTOMER ;
  WHERE !EMPTY(STORE) .AND. STATUS <> 'X' ;
  INTO TABLE (gcWorkDir+lcAccStors)
INDEX ON STORE TAG &lcAccStors
*B039299,1  TMI [End  ] 

IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDACCT','SH')
ENDIF
SELECT ORDHDR
IF laData[3] = SPACE(8)
  IF SEEK(laData[2])
    LOCATE REST WHILE ACCOUNT= laData[2] FOR INLIST(STATUS,'O','H')
    IF FOUND()
      *B127178,1 Message : 40197  
      *B127178,1 Open or hold orders have been found for account xxxxx. Cannot cancel
      *B127178,1 Button : 00000
      *B127178,1 Ok
      =gfModalGen('TRM40197B00000','ALERT','Open or hold orders'+'|'+'account '+laData[2])
      =gfCloseFile('ORDHDR')
      *B039299,1  TMI [Start] Close temp stores file
      =lfClsTmSto()
      *B039299,1  TMI [End  ] 
      RETURN .F.
      *B039299,1  TMI [Start]       
    ELSE
      =SEEK(laData[2])
      LOCATE REST WHILE ACCOUNT= laData[2] FOR INLIST(STATUS,'O','H') .AND. SEEK(STORE,lcAccStors)
      IF FOUND()
        *-  Open or hold orders have been found for account xxxxx. Cannot cancel
        =gfModalGen('TRM40197B00000','ALERT','Open or hold orders'+'|'+'account '+laData[2]+' and Store '+STORE )
        =gfCloseFile('ORDHDR')
        =lfClsTmSto()
        RETURN .F.
      ENDIF
    *B039299,1  TMI [End  ] 
    ENDIF
  ENDIF
  *B039299,1  TMI [Start] if the account has open / hold material sales order , do not cancel
  IF !USED('MASOHDR')
    =gfOpenFile(gcDataDir+'MASOHDR',gcDataDir+'ORDACCT','SH')
  ENDIF
  SELECT MASOHDR   
  IF SEEK(laData[2])
    LOCATE REST WHILE ACCOUNT= laData[2] FOR INLIST(STATUS,'O','H')
    IF FOUND()
      *- Open or hold orders have been found for account xxxxx. Cannot cancel
      =gfModalGen('TRM40197B00000','ALERT','Open or hold Material Sales Orders'+'|'+'account '+laData[2])
      =gfCloseFile('MASOHDR')
      =lfClsTmSto()
      RETURN .F.
    ELSE
      =SEEK(laData[2])
      LOCATE REST WHILE ACCOUNT= laData[2] FOR INLIST(STATUS,'O','H') .AND. SEEK(STORE,lcAccStors)
      IF FOUND()
        *- Open or hold orders have been found for account xxxxx. Cannot cancel
        =gfModalGen('TRM40197B00000','ALERT','Open or hold Material Sales Orders'+'|'+'account '+laData[2]+' and Store '+STORE)
        =gfCloseFile('MASOHDR')
        =lfClsTmSto()
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
  *B039299,1  TMI [End  ] 
ELSE
  IF SEEK(laData[2])
    LOCATE REST WHILE ACCOUNT= laData[2] FOR INLIST(STATUS,'O','H') AND STORE = laData[3]
    IF FOUND()
      *B127178,1 Message : 40198
      *B127178,1 Open or hold orders have been found for account xxxxx and Store xxxxx. Cannot cancel
      *B127178,1 Button : 00000
      *B127178,1 Ok
      =gfModalGen('TRM40198B00000','ALERT','Open or hold orders'+'|'+'account '+laData[2] + ' and Store '+laData[3])
      *B039299,1  TMI [Start] close temp created file for stores
      =gfCloseFile('ORDHDR')
      *B039299,1  TMI [End  ] 
      =lfClsTmSto()
      RETURN .F.
    ENDIF
  ENDIF
  *B039299,1  TMI [Start] 
  IF !USED('MASOHDR')
    =gfOpenFile(gcDataDir+'MASOHDR',gcDataDir+'ORDACCT','SH')
  ENDIF
  SELECT MASOHDR   
  IF SEEK(laData[2])
    LOCATE REST WHILE ACCOUNT= laData[2] FOR INLIST(STATUS,'O','H') AND STORE = laData[3]
    IF FOUND()
      *- ð have been found for ð; cannot cancel.
      =gfModalGen('TRM40198B00000','ALERT','Open or hold Material Sales Orders'+'|'+'account '+laData[2]+ ' and Store '+laData[3])
      =gfCloseFile('MASOHDR')
      =lfClsTmSto()
      RETURN .F.
    ENDIF
  ENDIF
  *B039299,1  TMI [End  ] 
ENDIF
=gfCloseFile('ORDHDR')

*-  TMI [Start] CHECKING DEBIT FILE
=gfOpenFile(gcDataDir+'DEBIT',gcDataDir+'DEBIT','SH')
=gfOpenFile(gcDataDir+'CREDIT',gcDataDir+'CREDIT','SH')
IF laData[3] = SPACE(8)

  SELECT DEBIT
  IF SEEK(laData[2])
    *- ð have been found for ð; cannot cancel.
    =gfModalGen('TRM40198B00000','ALERT','Debits'+'|'+'account '+laData[2])
    =lfClsTmSto()
    RETURN .F.
  ENDIF

  SELECT CREDIT
  IF SEEK(laData[2])
    *- ð have been found for ð; cannot cancel.
    =gfModalGen('TRM40198B00000','ALERT','Credits'+'|'+'account '+laData[2])
    =lfClsTmSto()
    RETURN .F.
  ENDIF

ELSE
  
  SELECT DEBIT
  =SEEK(laData[2])
  LOCATE REST WHILE ACCOUNT=laData[2] FOR STORE = laData[3] .AND. SEEK(STORE,lcAccStors)
  IF FOUND()
    *- ð have been found for ð; cannot cancel.
    =gfModalGen('TRM40198B00000','ALERT','Debits'+'|'+'store '+DEBIT.Store)
    =lfClsTmSto()
    RETURN .F.
  ENDIF
  
  SELECT CREDIT
  =SEEK(laData[2])
  LOCATE REST WHILE ACCOUNT=laData[2] FOR STORE = laData[3] .AND. SEEK(STORE,lcAccStors)
  IF FOUND()
    *- ð have been found for ð; cannot cancel.
    =gfModalGen('TRM40198B00000','ALERT','Credits'+'|'+'store '+Store)
    =lfClsTmSto()
    RETURN .F.
  ENDIF
  
  *- Check that if a store is a DC for another store
  *-- if this store is a DC for another store , deny deletion.
  SELECT &lcAccStors
  LOCATE FOR laData[3] = DIST_CTR .AND. laData[3] <> STORE
  IF FOUND()
    =gfModalGen('INM40186B00000','DIALOG',ALLTRIM(&lcAccStors..STORE))
    =lfClsTmSto()
    RETURN .F.
  ENDIF
  
ENDIF
=gfCloseFile('DEBIT')
=gfCloseFile('CREDIT')

=lfClsTmSto()
RETURN .T.
*--End of function lfvCancel.

*:**************************************************************************
*:* Name        : lfClsTmSto
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/26/2005
*:* Purpose     : Close created temp file for stores
*:***************************************************************************
*:* Called from : lfvCancel
*:***************************************************************************
FUNCTION lfClsTmSto

&& Colse and erase the temp file created in the function lfvCancel
IF USED(lcAccStors)
  USE IN &lcAccStors
ENDIF
ERASE (gcWorkDir+lcAccStors+'.DBF')
ERASE (gcWorkDir+lcAccStors+'.CDX')

&& Select the old alias befor entring the function lfvCancel
SELECT (lnSlct) 

*-- end of lfClsTmSto.
*!***************************************************************************
*!* Name        : lfUpdCrAv
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 20/11/2005
*!* Purpose     : Recalculate the credit Avalibale field in the customer file
*!***************************************************************************
*!* Called from : ARCUST.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfUpdCrAv()
*!***************************************************************************
*B130045,1
FUNCTION lfUpdCrAv
PRIVATE lnOldFile,lcOldOrder,lnCrAvail,lcExRSin,llOpened
STORE 0 TO lnCrAvail,lnOldFile
STORE '' TO lcExRSin,lcOldOrder
STORE .F. TO llOpened
lnOldFile = SELECT(0)
IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDACCT','SH')
  llOpened = .T.
ELSE
  lcOldOrder=ORDER('ORDHDR')
  SET ORDER TO TAG ORDACCT IN ORDHDR
ENDIF
lcUntSin = ' '
lnCrAvail = Customer.CrLimit - customer.NetBal
SELECT ORDHDR
=SEEK(CUSTOMER.ACCOUNT+'O')
SCAN REST WHILE Ordhdr.Account+OrdHdr.cOrdType = CUSTOMER.ACCOUNT+'O' FOR INLIST(ORDHDR.Status,'O','H')
  lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)
  lnCrAvail = lnCrAvail - ApprAmt &lcExRSin nExRate &lcUntSin nCurrUnit
ENDSCAN
SELECT Customer
REPLACE CrAvail WITH lnCrAvail
SELECT(lnOldFile)
*-- End of Function lfUpdCrAv.