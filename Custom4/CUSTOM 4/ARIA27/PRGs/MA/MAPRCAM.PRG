*!*************************************************************
*! Program File : MAPRCAM.PRG
*! Program Desc : Program to Add, Inquire and Modify materials P/O, OR
*!													 materials RETURN ,OR	 
*!													 materials CONTRACTS	
*! For Screen   : MAPRCAM.SPR
*!       System : Aria Apparel Series(A27) 
*!       Module : MATERIAL Module 
*! Developer    : Samah Wilson Kirollos (SWK)
*!*************************************************************
*! Calls        :
*!        Procedures :lpShow,lpOptions,lpDelScr
*!        
*!        Functions  : lfActFolder(),lfFillFold(),lfvData1()
*!                     lfvIData1()  ,lfInit()    ,lfPOLines()
*!                     lfBrowLine() ,lfwLine()   ,lfvobjlink()
*!                     lfvNotes()   ,lfvData2()  ,lfvIData2()
*!                     lfvData3()   ,lfvIData3() ,lfwData22()
*!                     lfwData2()   ,lfvData11() ,lfvData12()
*!                     lfvData13()  ,lfvData9()  ,lfvData10()
*!                     lfvData23()  ,lfvData25() ,lfwData23()
*!                     lfwData25()  ,lfwData3()  ,lfwData32()
*!                     lfvNewPL()   ,lfvFabric() ,lfvColor()
*!                     lfvKFabric() ,lfvKColor()
*!*************************************************************
*: Passed Parameters  : lcScrType 
*:                      'P' : FOR MATERIAL PO
*:                      'R' : FOR MATERIAL RETURN
*:                      'C' : FOR MATERIAL CONTRACT
*!                      
*!                      lcPOCall : if called for a specific P/O as inquire screen  
*!*************************************************************
*! Example :
*!        DO MAPRCAM WITH 'C' ( For Contracts )
*!*************************************************************
*!E300798,1 SWK 08/09/1998 Add the close option in case of A/P 
*!E300798,1                link for both P/O and Return
*!B602197,1 HDM 11/16/1998 Adding Field in FABDYE File (onorder) to be updated by
*!B602197,1 HDM 11/16/1998 the ordered Quantity according to The Warehouse
*!B602179,1 AMM 11/25/98 Erase temporary files when quitting the program
*!B801822,1 HDM 12/01/1998 Force to get the fabric.conv
*!B602331,1 HDM 12/17/1998 We Need To assign the warehouse For Both Returns and PO
*!E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*!E301176,1 HDM 03/22/1999 as it's now controlled globally
*!B602753,1 HDM 04/07/1999 Stop Calling NotePad Program In lpSavScr as the global save
*!B602753,1 HDM 04/07/1999 Will Call it
*!B602400,1 HDM 04/29/1999 Fixing bug of not assigning material to a certain warehouse in edit mode
*!E301268,1 HDM 06/21/1999 filter vendor browse by sup. type
*!E301295,1 AHM 07/18/1999 Add receiving date from PoFLin file to rec. browse
*!B602979,1 HDM 08/04/1999 Release Menue pad Browse & Browse1
*!B603259,1 HDM 11/02/1999 Fixing bug of lfRefresh not found case
*                          we are Cling cancelled or Closed Po from 
*                          another screen
*:B603507, 1 BWA 29/03/2000 Fix the bug of E.price doesn't correctly shows.
*B603504,1 ADEL 12/04/2000 Change the transaction type sent by GLDIST procedure for
*B603504,1                 closing matreial PO from 'MO' to 'MC'.
*B803242,1 ADEL 21/05/2000 When recieving a PO partially and then cancel it the status should 
*B803242,1                 be 'Complete' not 'Cancelled'. Also fill the Date field in the 
*B803242,1                 created record for cancelled qty and leave the rsession field empty 
*B803242,1                 as it should be filled only when recieving.
*B603654,1 ADEL 21/05/2000 Fix the bug of 'Numeric overflow' when closing the po.
*B803334,1 ADEL 08/06/2000 Change 'Ship to:' to 'Ship from:' on the Return Material PO screen.
*E500349,1 WAB 08/20/2000 ability to enter manual material PO #  
*B603834,1 WAB 08/28/2000 Fix the bug of wrong link code send  to gl dist 
*B803016,1 ADEL 09/03/2000 Fix the bug of caculate 'Amount' field in the 'Reciving lines' browse
*B803016,1                 based on the default PO's cost not the received cost.
*B803243,1 ABD 08/08/2000 Fix bug that when select the option menu and click 
*B803243,1 ABD            On show receiveing lines and click on summary folder
*B803243,1 ABD            And click next the detail screen display within the summary folder.
*B602618,1 RAMY 13/02/2001 Fix the bug of : 1- Displaying wrong open qty when closing the PO
*                                           2- Update the FABDYE file with the canceled qty
*C200149,1 ABD 02/20/2001 Add new triger For ENG10
*C200200,1 MHM 06/19/2001 ADD new triger For ENG10
*B604302,1 MHM 07/08/2001 FIx the bug of there is no softseek while browsing MAterial
*B604660,1 MHM 07/25/2001 Fix the bug of not Updating WareHouse in Pofln 
*B604678,1 MHM 08/20/2001 Fix the bug ship to Location can changed if partially recived 
*B604786,1 MHM 08/23/2001 Fix the bug of The (FABDYE) file  does not updated correctly when you remove lines.
*B804444,1 ADEL 10/03/01   Add the ability to the users to print MPO if they want.
*B804449,1 MHM 10/04/2001 Fix the bug of if you canceled a PO and try to uncancel it , 
*B804449,1                 you will get an error Variable Conv not found
*E500442,1 ABD 10/21/2001 Enhance the speed of openining the screen.
*B605033,1 ABD 10/23/2001 Editing terms on material po does not save.
*B605044,1 ABD 10/30/2001 Fix problem that the program didn't print the return Po and Contract.
*E500442,5 ABD 11/04/2001 Fix some problem in the sceen while we speed up the screen.
*B602633,1 ADEL 11/25/2001 Fix the bug of not be able to enter GL code while not linked to AP
*B602633,1                 and trying to edit PO actual cost.
*B605173,1 WAB 12/05/2001 Fix the bug of emty status popup.
*C102525,1 RAE 01/22/2002 view the PO returns issued against the selected PO.
*B605724,1 ABD 03/21/2002 Fix problem in the WIP link code when creating new material Po's.
*B605812,1 ABD 06/10/2002 Fix Bug in the 'Edit Cost Per Line ' Option is not enable , & the 
*B605812,1 ABD            Summary folder not update while I Edit Cost Per Line.
*B605935,1 AMH 06/10/2002 Fix the bug of printing MPO without the PO notepad if any.
*B606260,1 ADEL 07/15/02 Fix the bug of "Alias 'xxxxxx' not found" when accepting to print the
*B606260,1               just entered POs for Alena Custom Material PO form. 
*B606327,1 ADEL 07/31/02 When printring POs after saving them, rename the field hoding thier
*B606327,1               numbers to work with all forms (especially for Alena)
*E301937,1 ADEL 07/31/02 If the "Material WIP link code" setup is empty, use the same WIP
*E301937,1               Style link code, otherwise get its value entered in setup
*B606229,1 ADEL 07/31/02 Fix the bug of "VariAble 'lcDET_TTL' not found" when calling the SM\codes
*B606229,1               While the Matreial PO screens is Open. 
*B604856,1 AMH 08/22/2002 Fix the bug of wronge updating in fabdye when cancel PO.
*C200439,1 ABD 12/12/2002 New Fields add to Material PO Detail Screen for Custom Dar10.
*B606710,1 KHM 01/05/2003 Fix the bug of not displaying the lines when move forward or backward
*B606710,1                after you cancel a PO
*C102780,1 AMH 01/09/2003 Made some modification to the material PO screen (by adding supplier
*C102780,1                cod, color and delivery date).
*B607085,1 ABD 03/30/2003 Fix problem that the deliv. date not updated in add mode or edit mode.
*B607159,1 AMH 04/10/2003 Fix bug of negative on order qty.
*C102819,1 AMH 04/10/2003 Add MRP field to the PO screen.
*B607276,1 ALB 08/12/2003 Fix bug changing vendor contact name and telephone when change vendor in edit mode
*B119869,1 ABD 10/09/2003 Display the close date into Closing of Material PO's and allow the 
*B119869,1 ABD            User to overwrite as in the Style PO Closing.
*B122673,1 NNA 05/15/2004 Fix bug that the price field Rounding up i.e (0.065 save as 0.07)
*B122673,1 NNA            that By changing the Vendmatl.Price field name to be (nFabCost) by add
*B122673,1 NNA            new record in sydflfld.dbf with (Vendmatl & nFabCost) and Replace 
*B122673,1 NNA            all (Price) with (nFabCost) in this code and the all related prg.
*B038431,1 MHM 08/23/2004 Fix the bug Return Material PO >>edit old PO >> type ? In any field to open browser
*B038431,1 MHM            >>press Escape from the browser>>old value in the field disappear
*B124255,1 NNA 11/03/2004 Fix bug that In the Material PO screen, if you used the print icon
*B124255,1 NNA            to print the PO, and then click on the Details Tab of the PO it will 
*B124255,1 NNA            give you 'Alias Not found' that for tha Fabric File
*B125577,1 NNA 12/29/2004 fix bug When an Material PO has been partially recieved and then a user
*B125577,1 NNA            edits the PO by removing the remaining open qty. the MPO is saved with 
*B125577,1 NNA            an Open status instead of being marked as Completed. 
*!***************************************************************************************************************
PARAMETERS lcScrType,lcPOCall

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE ladata[55],laKeyField[2,4],laFoldWinds[3,2],laAddress[6,3],laDefProc[10],;
        laVariables[6] 

lcScrType = IIF(TYPE('lcScrType')='C',lcScrType,'P')
lcPOCall  = IIF(TYPE('lcPOCall  ')='C',lcPOCall,SPACE(01))
STORE '' TO lcFolder ,laData,laKeyField,laFoldWinds,laAddress,lcPOHdr,lcQuality
STORE '' TO lcStatus ,lcOldVEn,ldOldEnt, ldOldComp,lcDetTmp,;
            lcOldPCurr, lcOldDCurr,lcFrnSmbl1,lcFrnSmbl2,lcFrnSign1,lcFrnSign2,lcKeyType,;
            lcFabric,lcOldFab,lcColor,lcOldClr,lcModal,lcIDesc,lcClrDesc,;
            lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcWinC31,lcWinC32,lcWinC33,lcDelMesag,lcCurDesc,;
            lcPOLine,lcPOTran,lcWinC34,laVariables,cMarker,lcContLin,lcPrgtype,;
            lcMenProm,lcGl_link,lcLinkDesc,lcGlPeriod,lcGLFYear,lcUOB

*C102780,1 AMH Add variableS for vendor item, vendor color and delivry date [Start]
STORE '' TO lcVenFab,lcOldVFab,lcVenColr,lcOldVColr
STORE {} TO ldDelvDate
*C102780,1 AMH [End]

*C102525,1 RAE [BEGIN] Declare lcFLt to hold the filter Exp. that used when showing receiving lines.
STORE '' TO lcFlt , lcTmpHdr , llTmpHdr
*C102525,1 RAE [END]

*B804444,1 (Begin) Initialize needed variables.
lcReportID = " "

*B605044,1 ABD - We define this variable here to use when we print the Material 
*B605044,1 ABD - Purchase Order report to fix the problem that the program didn't print 
*B605044,1 ABD - the contract and return PO [Begin]
lcTypPrint = ''

*B605724,1 ABD - Define llbrowse as a global variable. [Begin]
llBrowse = .F.
*B605724,1 ABD - [End]

*B605044,1 ABD - [End]

*B607085,1 ABD - [Begin]
ldOldValue  = {}
*B607085,1 ABD - [End]

DIMENSION  laNewMpos[1]
laNewMpos  = ""

*B605935,1 AMH Define variable to hold cursor name of new MPOs [Start]
lcNewMPos = gfTempName()
*B606327,1 (Begin) Rename the field.
*CREATE CURSOR (lcNewMPos) (MATPO C(6))
*INDEX ON MATPO TAG (lcNewMPos) OF (lcNewMPos)
CREATE CURSOR (lcNewMPos) (POMAT C(6))
INDEX ON POMAT TAG (lcNewMPos) OF (lcNewMPos)
*B606327,1 (End)
APPEND BLANK
*B605935,1 AMH [End]

*B804444,1 (End) 
DECLARE laFromBrow[2]
*B606229,1 (Begin) Define a new bar for ALT+B.
*ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDet_Ttl)
DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcDet_Ttl)
*B606229,1 (End)

*B802047 AHM (Start)
llShowRec = .F.
lcWinC30 = ''
lcWinC35 = ''
lnRecRec = 0
*B802047 AHM (End)

STORE 0 TO  lcOldRate,lnCost1,lnOldCost1,lnCost2,lnOldCost2,lnCost3,lnOldCost3,;
            lnCost4,lnOldCost4,lnQty,lnOldQty,lnTotQty,lnBarNo,lnLineNo,lnLastLine,lnNewRec,;
            lnLastFold,lnStatus
STORE 1 TO  lnUnit1,lnUnit2,lnQuaFab,lnQuaFab,lnDiv,lnTerms,lnShip
STORE '' TO lcVEnAdd1,lcVEnAdd2,lcVEnAdd3,lcVEnAdd4,lcVEnAdd5,lcVEnAdd6,;
            lcVenName,lcOldWare,lcWareDEsc,lcActTmp,lcOldLInk,lcFiles
STORE .F. TO llContCls

*-- B602400,1 HDM[Start] Define variable as flag to prevent asking user to
*-- B602400,1 HDM assign the material more than one time,
STORE .T. TO llAssignWH , llAsk
STORE '' TO lcOldLocat
*-- B602400,1 HDM[End]

*-- Varaible that showes if the vendor file is opened by the program (Start)
*-- or it is already opened
*-- AAMER 10/26/98
STORE .F. TO llOpnVend
*-- Varaible that showes if the vendor file is opened by the program (End)

laData[9]  = gdSysDate
laData[10] = gdSysDate+90 

llContinue  = .F.
llSavAct  = .F.
lnSessNo  = gnProgCopy
lnNomOfFo = 3
llAlowNew = .T.   && Flag to enable the new button in the control pannel.
llEdCost  = .F.
lcQuality = '1'

ibData11 = 1
ibData12 = 1
ibData13 = 1

lcOldEscTrap  = ON('Key','Esc')
laDefProc[7]  = .F.
laDefProc[9]  = .F.
laDefProc[10] = .F.


*--E301077,64 do this part after gfSetUp (Start)
*lcSession  = gfsequence('CSESSION')
*llMFIstall = (OCCURS('MF',gcCmpModules)<>0)
*llPOIstall = (OCCURS('PO',gcCmpModules)<>0)
*lcMtCstMth = gfGetMemVar('M_MatCstMth')  
*llMultiWH  = ALLTRIM(gfGetMemVar('M_WareHouse'))= 'Y'
*llDyelot   = ALLTRIM(gfGetMemVar('M_DYELOT'))   = 'Y' 
*llTrkRolls = ALLTRIM(gfGetMemVar('M_TrkRolls'))= 'Y'
*llWareLoc  = ALLTRIM(gfGetMemVar('M_WARELOC'))= 'Y'
**MAN
**llCostPrv  = gfUserPriv('MA','STY100')
*llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
*llMulCurr  = gfGetMemVar('llMulCurr')  
*llEditExRt = gfGetMemVar('LLEDITEXRA')
*llCalled   = !EMPTY(lcPOCall)
*llGlLink   = ALLTRIM(gfGetMemVar('M_LINK_GL'))  = 'Y'
*llApIstall = (OCCURS('AP',gcCmpModules)<>0)
*--E301077,64 do this part after gfSetUp (End)

llNEW    = .F.
lNewLine = .F.

DIMENSION laStatus[6]
laStatus[1]  = 'Open'
laStatus[2]  = 'Bid'
laStatus[3]  = 'Cancelled'
*B605173 WAB (Start) the status is 'Completed'
*laStatus[4]  = 'Complete'
laStatus[4]  = 'Completed'
*B605173 WAB (End) 
laStatus[5]  = 'Hold'
laStatus[6]  = 'Closed'

DIMENSION laTranCodes[3,1]
laTranCodes[1,1] = 'Received'
laTranCodes[2,1] = 'Damaged'
laTranCodes[3,1] = 'Cancelled'

DIMENSION laQuality[3,2]
laQuality[1,1] = 'First Quality'
laQuality[1,2] = '1'
laQuality[2,1] = 'Second Quality'
laQuality[2,2] = '2'
laQuality[3,1] = 'Damaged'
laQuality[3,2] = '3'

lcScrMode = 'S' 

DIMENSION laShip[1], laTerms[1], laDiv[1], laCodInfo [3,10]
STORE "" TO laShip,laTerms, laDiv, laCodInfo

*--MHM2000
llFlag = .T.
STORE '' TO lclDetTmp  
*--MHM2000
IF !gfSetup()
  RETURN
ENDIF  

*E500442,1 ABD - Define new variables to open POFLN with anther name and
*E500442,1 ABD - also open POFhdr with anther name. [Begin]
*-- We will use this temp file to open POFLN with anther name.
STORE '' TO lcContHdr  , lcContLin
STORE .F. TO llContHdr , llContLin
lcContHdr = gfTempName()
lcContLin = gfTempName()
llContHdr = gfOpenFile(gcDataDir+'POFHDR',gcDataDir+'POFHDR','SH',@lcContHdr,.T.)
llContLin = gfOpenFile(gcDataDir+'POFLN',gcDataDir+'POFLNF','SH',@lcContLin,.T.)
SET RELATION TO cMattype+PoMat INTO (lcContHdr)
*E500442,1 ABD - [End]

*C102525,1 RAE [BEGIN] Open POFHDR with new name
STORE '' TO lcFlt , lcTmpHdr , llTmpHdr
lcTmpHdr = gfTempName()
IF ASCAN(laEvntTrig , PADR('POHALIAS',10)) <> 0
  =gfDoTriger('MAPRCAM',PADR('POHALIAS',10))
ENDIF

*--E301077,64 (Start)
*E500349,1 - WAB (START) - add element to the array that hold the company setup
*DIMENSION laSetUp[8,2]
*E301937,1 (Begin) Add a new element for the new setup.
DIMENSION laSetUp[10,2]
*E301937,1 (End)
*E500349,1 - WAB (END) 

laSetUp[1,1] = 'M_MatCstMth'
laSetUp[2,1] = 'M_WareHouse'
laSetUp[3,1] = 'M_DYELOT'
laSetUp[4,1] = 'M_TrkRolls'
laSetUp[5,1] = 'M_WARELOC'
laSetUp[6,1] = 'llMulCurr'
laSetUp[7,1] = 'LLEDITEXRA'
laSetUp[8,1] = 'M_LINK_GL'

*E500349,1 - WAB (START) - add the element 'M_GENMAPON' ( Enter MA Po manualy ) to the array 
laSetUp[9,1] = 'M_GENMAPON'
*E500349,1 - WAB (END) 


*E301937,1 (Begin) Add a new element for the new setup.
laSetUp[10,1] = 'M_WIPLINK'
*E301937,1 (End)
= gfGetMemVar(@laSetUp)

lcMtCstMth = laSetUp[1,2]
llMultiWH  = ALLTRIM(laSetUp[2,2])= 'Y'
llDyelot   = ALLTRIM(laSetUp[3,2])   = 'Y' 
llTrkRolls = ALLTRIM(laSetUp[4,2])= 'Y'
llWareLoc  = ALLTRIM(laSetUp[5,2])= 'Y'
llMulCurr  = laSetUp[6,2]
llEditExRt = laSetUp[7,2]
llGlLink   = ALLTRIM(laSetUp[8,2])  = 'Y'

llCalled   = !EMPTY(lcPOCall)
lcSession  = gfsequence('CSESSION')
llMFIstall = (OCCURS('MF',gcCmpModules)<>0)
llPOIstall = (OCCURS('PO',gcCmpModules)<>0)
llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
llApIstall = (OCCURS('AP',gcCmpModules)<>0)
*--E301077,64 (End)


*E500349,1 - WAB (START) - save the company setup ( Enter Ma Po No Manual ) into llGenMApon 
*---llGenMaPoN = .T.  -----> generated Po no
*---llGenMaPoN = .F.  -----> manuall Po no 

llGenMaPoN = ALLTRIM(laSetUp[9,2])  = 'N'
*E500349,1 - WAB (END) 

*B801825 AAMER 11/18/98 (Start)
*-- Checking the existence of at least one warehous (Default) in warehous file
PRIVATE lnWareRec,llWareFond
lnWareRec  = 0
llWareFond = .F.
SELECT WAREHOUS
lnWareRec = RECNO()
GO TOP
IF llMultiWH
  SCAN FOR lMatInv
    llWareFond = .T.
    EXIT
  ENDSCAN
ELSE
  IF EOF()
    llWareFond = .F.
  ELSE 
    llWareFond = .T.
  ENDIF
ENDIF
IF RECCOUNT() >= lnWareRec
  GO lnWareRec
ENDIF

IF !llWareFond
  *-- Locations file is Empty . Cannot proceed.
  *-- <Ok>
  *--Locations file is Empty, Cannot Proceed.
  =gfModalGen('QRM42080B00000','DIALOG','Locations file is Empty')
  glQuitting = .T.
  RETURN
ENDIF  
*B801825 AAMER 11/18/98 (End)

lcAProgm=IIF(lcKeyType  ='R','RETPO',IIF(lcKeyType  ='C','CONTR','MATPO'))
DO CASE
  CASE lcScrType = UPPER('P')
    lcPrgtype     = 'P/O #'
    lcDialog   = 'order'
    lcKeyType  = 'P'
    lcFile_ttl = 'Material Purchase Orders'
  CASE lcScrType = UPPER('R')
    lcPrgtype = 'Return #'
    lcDialog   = 'return'
    lcKeyType  = 'R'
    lcFile_ttl = 'Material Purchase Orders Returns'
  CASE lcScrType = UPPER('C')
    lcPrgtype = 'Contract #'
    lcDialog   = 'contract'    
    lcKeyType  = 'C'
    lcFile_ttl = 'Material Contracts'
  OTHERWISE
    RETURN  
ENDCASE
lcUnsmsPrg   = PADR('MAT_PO'+lcKeyType,10)
lcwfoldchng  = '=lfActFolder()'   && function to control shows after change the folder
lcfoldpush   = 'pbFolders'        && push button name for the next folder
lnfolderCend = 103.00
lnfolderRend = 2.00


laKeyField[1,1] = 'lcKeyType'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'POFHDR'
laKeyField[1,4] = 1

laKeyField[2,1] = 'laData[1]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'POFHDR'
laKeyField[2,4] = 2

*C102780,1 AMH Add variable to get the use vendor referance setup [Start]
llVenRef   = (gfGetMemVar('M_VENREF  ')='Y')
llDlivDate = (gfGetMemVar('M_DLIVDATE')='Y')
IF llVenRef
  = gfOpenFile(gcDataDir+'VENDMATH','VENDMATH','SH')
  = gfOpenFile(gcDataDir+'VENDMATL' , 'MATCOL','SH')
ENDIF
*C102780,1 AMH [End]

*C102819,1 AMH Add MRP field to the po screen [Start]
llMrp = (gfGetMemVar('M_MRP     ')='Y')
*C102819,1 AMH [End]

*SWK
*DIMENSION ladata[55]

*C102780,1 AMH Add the revision number to the screen fields [Start]
*DIMENSION ladata[56]

*C102819,1 AMH Add MRP field to the po screen [Start]
*DIMENSION ladata[57]
DIMENSION ladata[58]
*----------------------------------------------------------------
*E300798,1 SWK 08/09/1998 Add the link code to the screnn fields

*lcScFields   = 'POMAT, Vendor, cWareCode,cOutAddr1,cOutAddr2,cOutAddr3,'+;
               'cOutAddr4,cOutAddr5,'+;
               'Entered,Complete,cDivision,cTermCode,ShipVia,'+;
               'Insurance ,cFob,  QuotaCat, '+;
               'Contact , Phone,Origin ,CLCNO , LCExpire,'+;
               'crequis, cPriceCur , nPriceRat, cDutyCur,'+;
               'nDutyRat , nEcost1, nEcost2, nEcost3, nEcost4,'+;
               'nELanCost1, nELanCost2, nELanCost3, nELanCost4,'+;
               'nEActCost1, nEActCost2, nEActCost3, nEActCost4,'+;
               'nCost1,nCost2,nCost3,nCost4,'+;
               'nLan_Cost1 ,nLan_Cost2 ,nLan_Cost3,nLan_Cost4,'+;
               'nAct_Cost1, nAct_Cost2, nAct_Cost3, nAct_Cost4,nfaborder,nfbreceive,'+;
               'nfabdamage,nfabcancel,npo_open,Link_Code'
*lcScFields   = 'POMAT, Vendor, cWareCode,cOutAddr1,cOutAddr2,cOutAddr3,'+;
               'cOutAddr4,cOutAddr5,'+;
               'Entered,Complete,cDivision,cTermCode,ShipVia,'+;
               'Insurance ,cFob,  QuotaCat, '+;
               'Contact , Phone,Origin ,CLCNO , LCExpire,'+;
               'crequis, cPriceCur , nPriceRat, cDutyCur,'+;
               'nDutyRat , nEcost1, nEcost2, nEcost3, nEcost4,'+;
               'nELanCost1, nELanCost2, nELanCost3, nELanCost4,'+;
               'nEActCost1, nEActCost2, nEActCost3, nEActCost4,'+;
               'nCost1,nCost2,nCost3,nCost4,'+;
               'nLan_Cost1 ,nLan_Cost2 ,nLan_Cost3,nLan_Cost4,'+;
               'nAct_Cost1, nAct_Cost2, nAct_Cost3, nAct_Cost4,nfaborder,nfbreceive,'+;
               'nfabdamage,nfabcancel,npo_open,Link_Code,cRevNo'
lcScFields   = 'POMAT, Vendor, cWareCode,cOutAddr1,cOutAddr2,cOutAddr3,'+;
               'cOutAddr4,cOutAddr5,'+;
               'Entered,Complete,cDivision,cTermCode,ShipVia,'+;
               'Insurance ,cFob,  QuotaCat, '+;
               'Contact , Phone,Origin ,CLCNO , LCExpire,'+;
               'crequis, cPriceCur , nPriceRat, cDutyCur,'+;
               'nDutyRat , nEcost1, nEcost2, nEcost3, nEcost4,'+;
               'nELanCost1, nELanCost2, nELanCost3, nELanCost4,'+;
               'nEActCost1, nEActCost2, nEActCost3, nEActCost4,'+;
               'nCost1,nCost2,nCost3,nCost4,'+;
               'nLan_Cost1 ,nLan_Cost2 ,nLan_Cost3,nLan_Cost4,'+;
               'nAct_Cost1, nAct_Cost2, nAct_Cost3, nAct_Cost4,nfaborder,nfbreceive,'+;
               'nfabdamage,nfabcancel,npo_open,Link_Code,cRevNo,cMrp'
*C102819,1 AMH [End]

*C102780,1 AMH [End]

*lcScFields   = 'POMAT, Vendor, cWareCode,cOutAddr1,cOutAddr2,cOutAddr3,'+;
               'cOutAddr4,cOutAddr5,'+;
               'Entered,Complete,cDivision,cTermCode,ShipVia,'+;
               'Insurance ,cFob,  QuotaCat, '+;
               'Contact , Phone,Origin ,CLCNO , LCExpire,'+;
               'crequis, cPriceCur , nPriceRat, cDutyCur,'+;
               'nDutyRat , nEcost1, nEcost2, nEcost3, nEcost4,'+;
               'nELanCost1, nELanCost2, nELanCost3, nELanCost4,'+;
               'nEActCost1, nEActCost2, nEActCost3, nEActCost4,'+;
               'nCost1,nCost2,nCost3,nCost4,'+;
               'nLan_Cost1 ,nLan_Cost2 ,nLan_Cost3,nLan_Cost4,'+;
               'nAct_Cost1, nAct_Cost2, nAct_Cost3, nAct_Cost4,nfaborder,nfbreceive,'+;
               'nfabdamage,nfabcancel,npo_open'
*E300798,1 SWK(End)
*----------------------------------------------------------------
*-- Define the variable of the system
lcCancel  = gcBmpHome + "trash.bmp"
lcUncan   = gcBmpHome + "untrash.bmp"
lcPromp   = lcCancel
lcDet_Ttl = "P/O Lines"

laFromBrow[1]=lcDet_Ttl 
lcTrn_Ttl = "Transaction"
lcRec_Ttl = "Receiving lines"

laFromBrow[2]=lcRec_Ttl

lcMark    = '>'
lcUnMark   = SPACE(1)
lcfoldprnt = gcBaseWind         && window parent name for the folder
llNoShow = .F.
= lfFillFold()
qWareHouse = WAREHOUS.CWARECODE
*----------------------------------------------------------------
IF !WEXIST(gcBaseWind)
  *B804444,1 (Begin) Initialize needed variables.
  laNewMpos  = ""
  *B804444,1 (End) 
  lcDelMesag  = 'cancel'
  lcMenProm  = PROPER(lcDelMesag)
  DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .T.
  lcStatus    = IIF(lcKeyType = 'C','Hold','Open')
  lnStatus    = IIF(lcKeyType = 'C',5,1)
  ldOldEnt    = GDSYSDATE
  ldOldComp   = GDSYSDATE + 90
  ldComplet   = GDSYSDATE + 90
  lcFrnSign1  = '*'
  lcFrnSign2  = '*'
  lcPOHdr      = gfTempName()
  lcWinCh1     = gfTempName()
  lcWinCh2     = gfTempName()
  lcWinCh3     = gfTempName()
  lcWinC31     = gfTempName()
  lcWinC32     = gfTempName()
  lcWinC33     = gfTempName()
  lcWinC34     = gfTempName()

  *B802047 AHM (Start)
  lcWinC30     = gfTempName()
  lcWinC35     = gfTempName()
  *B802047 AHM (End)

  lcWinCh4     = gfTempName()
  lcfolder     = gfTempName()
  llNoShow = .F.
  laVariables[1]  = 'lcScrMode'
  laVariables[2]  = 'laData[4]'
  laVariables[3]  = 'laData[5]'  
  laVariables[4]  = 'laData[6]'
  laVariables[5]  = 'laData[7]'
  laVariables[6]  = 'laData[8]'
  *laVariables[7]  = 'lcScrMode'
  *laVariables[8]  = 'lcScrMode'
  *laVariables[9]  = 'lcScrMode'
  *laVariables[10] = 'lcScrMode'
  *laVariables[11] = 'lcScrMode'
  *laVariables[12] = 'lcScrMode'
      
  laCodInfo[1,01] = "CTERMCODE"    && Field Name
  laCodInfo[1,02] = "laTerms"      && Array Name
  laCodInfo[1,03] = "lnTerms"      && Popup Name
  laCodInfo[1,04] = ""             && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[1,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[1,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[1,07] = "POFHDR"       && Alternative File (For default val.)
  laCodInfo[1,08] = "POFHDR"                 && Use this index for the Alternative file.
  laCodInfo[1,09] = "lcKeyType+laData[1]"    && Seek this expretion.
  laCodInfo[1,10] = "cTermCode"                  && Alternative Field Name

  laCodInfo[2,01] = "CDIVISION"
  laCodInfo[2,02] = "laDiv"
  laCodInfo[2,03] = "lnDiv"
  laCodInfo[2,04] = ""
  laCodInfo[2,05] = .F.
  laCodInfo[2,06] = .F.
  laCodInfo[2,07] = "POFHDR" 
  laCodInfo[2,08] = "POFHDR"
  laCodInfo[2,09] = "lcKeyType+laData[1]" 
  laCodInfo[2,10] = "cDivision"

  laCodInfo[3,01] = "SHIPVIA"      && Field Name
  laCodInfo[3,02] = "laShip"       && Array Name
  laCodInfo[3,03] = "lnShip"       && Popup Name
  laCodInfo[3,04] = ""             && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[3,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[3,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[3,07] = "POFHDR"       && Alternative File (For default val.)
  laCodInfo[3,08] = "POFHDR"       && Use this index for the Alternative file.
  laCodInfo[3,09] = "lcKeyType+laData[1]"    && Seek this expretion.
  laCodInfo[3,10] = "SHIPVIA"      && Alternative Field Name


  = gfwCodePop(@laCodInfo, "CTERMCODE", "N")
  = gfwCodePop(@laCodInfo, "CDIVISION", "N")
  = gfwCodePop(@laCodInfo, "SHIPVIA" , "N")
  
  
*  SELECT POFHDR
*  SELECT *,00 AS nSteps FROM POFHDR WHERE .F. INTO DBF (gcWorkDir+lcPOHdr)
*  SELECT (lcPOHdr)
*  INDEX ON cType+POMAT TAG lcPOHdr
*  APPEND BLANK 

  SELECT POFHDR
  *E500442,5 ABD - set filter on the full index to speed up 
  *E500442,5 ABD - the screen while you open the screen. [Begin]
  *SET FILTER TO cMatType = lcKeyType  
  SET FILTER TO cMatType+PoMat = lcKeyType  
  *E500442,5 ABD - [End]
  
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laData[9]  = gdSysDate
  laData[10] = gdSysDate+90 
  laData[1]   = IIF(llCalled,lcPoCall,laData[1])     
  DIMENSION laFoldWinds[lnNomOfFo,2]
  lcDetTmp   = gfTempName()
  lcPOLine   = gfTempName()
  lcPOTran   = gfTempName()

  *E500442,5 ABD - Move this part up after gfsetup. [Begin]
  *E500442,1 ABD - Define new variables to open POFLN with anther name and
  *E500442,1 ABD - also open POFhdr with anther name. [Begin]
  *-- We will use this temp file to open POFLN with anther name.
  *STORE '' TO lcContHdr  , lcContLin
  *STORE .F. TO llContHdr , llContLin
  *lcContHdr = gfTempName()
  *lcContLin = gfTempName()
  *llContHdr = gfOpenFile(gcDataDir+'POFHDR',gcDataDir+'POFHDR','SH',@lcContHdr,.T.)
  *llContLin = gfOpenFile(gcDataDir+'POFLN',gcDataDir+'POFLNF','SH',@lcContLin,.T.)
  *SET RELATION TO cMattype+PoMat INTO (lcContHdr)
  *E500442,1 ABD - [End]
  *E500442,5 ABD - [End]
    
  lcActTmp   = gfTempName()
  llEdCost   = .F.
  lcDelProm  = lcCancel

  *C200200,1  MHM 06/19/2001 [Start]
  IF ASCAN(laEvntTrig , PADR('CRTMPFLS',10)) <> 0
    STORE '' TO lclDetTmp  
    *--flag for collecting data
    =gfDoTriger('MAPRCAM',PADR('CRTMPFLS',10))
  ENDIF     
  *C200200,1  MHM 06/19/2001 [End]
*----------------------------------------------------------------
  lnactfolder = 1
  lnlastfold  = lnactfolder
*----------------------------------------------------------------

*-- These files will be added to SydObject.mPrgFiles (Program files) (Start)
*-- to be opened by gfsetup function 
*-- AAMER 11/18/98


*=gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
*=gfOpenFile(gcSysHome+'SyuEror',gcSysHome+'cSession','SH')
*=gfOpenFile(gcDataDir+'Codes','cCode_no','SH')
*=gfOpenFile(gcDataDir+'Fabric',gcDataDir+'Fabric','SH')
*=gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')

*-- These files will be added to SydObject.mPrgFiles (Program files) (End)

IF llGlLink
  =gfOpenFile(gcDataDir+'GL_LINK',gcDataDir+'GL_LINK1','SH')
  =gfOpenFile(gcDataDir+'GLDIST' ,'','SH')
ENDIF
IF llApIstall
  =gfOpenFile(gcDataDir+'APVINVDT',gcDataDir+'ORGVINV','SH')
  =gfOpenFile(gcDataDir+'APINVTKT',gcDataDir+'TICKET','SH')
ENDIF
*----------------------------------------------------------------
*  SELECT POFLN
*  = AFields(laFileStru)
*  = AFields(laContStru)  
*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'cMarker'
*  laFileStru[lnNewFld,2] = 'C'
*  laFileStru[lnNewFld,3] = 1
*  laFileStru[lnNewFld,4] = 0
*  
*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'lNew'
*  laFileStru[lnNewFld,2] = 'L'
*  laFileStru[lnNewFld,3] = 1
*  laFileStru[lnNewFld,4] = 0
*  
*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'nRecno'
*  laFileStru[lnNewFld,2] = 'N'
*  laFileStru[lnNewFld,3] = 10
*  laFileStru[lnNewFld,4] = 0
*
*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'cStatus'
*  laFileStru[lnNewFld,2] = 'C'
*  laFileStru[lnNewFld,3] = 1
*  laFileStru[lnNewFld,4] = 0

*  CREATE table (gcWorkDir+ lcDetTmp) FROM ARRAY laFileStru
*  INDEX ON cType+POMAT+FABRIC+COLOR TAG (lcDetTmp) OF (gcWorkDir+lcDetTmp)

*  CREATE table (gcWorkDir+ lcActTmp) FROM ARRAY laFileStru

  
*  =gfOpenFile (gcWorkDir+lcDetTmp,gcWorkDir+lcDetTmp,"EX")
*  IF lcKeyType $ 'PC' 
*    CREATE table (gcWorkDir+lcConTLin) FROM ARRAY laContStru
*    INDEX ON FABRIC+COLOR TAG (lcConTLin) OF (gcWorkDir+lcConTLin)
*    =gfOpenFile (gcWorkDir+lcConTLin,gcWorkDir+lcConTLin,"EX")
*  ENDIF
  
  IF !USED('ApVendor')
    =gfOpenFile(gcDataDir+'ApVendor',gcDataDir+'VenCode','SH')
    llOpnVend = .T.
  ELSE
    SELECT ApVendor
    lnOldTag = INT(VAL(SYS(21)))
    SET ORDER TO TAG VenCode
    lnRecVend = RECNO("ApVendor")
  ENDIF
  IF llCalled
    =lfcrtUncmp()
    laScrMode[2] = .T.
    laScrMode[1] = .F.
    = lfInit(.T.)
  ENDIF  
  lcFiles = 'lcPOHdr,'+lcPOHdr+','+lcPOHdr+';'+'lcDetTmp,'+lcDetTmp+','+lcDetTmp+';'
ENDIF
*----------------------------------------------------------------
*E300798,1 SWK 08/09/1998 Add a close button for the tool bar 
*E300798,1 in case of A/P link and P/O or Return
*DECLARE laPanelObj[2,3]

* E301176,1 HDM 03/22/1999[Start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally
*IF llApIstall AND lcScrType $ 'PR'
*  DECLARE laPanelObj[2,3]
*ELSE
*  DECLARE laPanelObj[1,3]
*ENDIF

IF llApIstall AND lcScrType $ 'PR'
  DECLARE laPanelObj[1,3]
ENDIF


STORE '' TO laPanelObj
*laPanelObj[1,1] = 'pbInvNote'
*laPanelObj[1,2] = gcBmpHome+'NOTES2.BMP'
*laPanelObj[1,3] = [VALID lfvNotes()]

IF llApIstall AND lcScrType $ 'PR'
  laPanelObj[1,1] = 'pbClose'
  laPanelObj[1,2] = gcBmpHome+"CLS_Shet.bmp"
  laPanelObj[1,3] = [VALID lfvClose()]
ENDIF
*!E301176,1 HDM 03/22/1999[End]

*E300798,1(End)

*E500442,1 ABD - Don't call this function again to speed up the screen .[Begin]
*IF lcKeyType = 'P' OR lcKeyType = 'C'
*  = lfContLin()
*ENDIF
*E500442,1 ABD - [End]

SELECT FABRIC
SET ORDER TO TAG FABRIC

SELECT LC
SET ORDER TO TAG LC

SELECT (lcBaseFile)
PUSH KEY
=lfActPad()
DEFINE PAD _BROWSE OF _MSYSMENU PROMPT "" KEY CTRL+B

*B802047 AHM (Start)
DEFINE PAD _BROWSE1 OF _MSYSMENU PROMPT "" KEY CTRL+R
ON SELECTION PAD _BROWSE1 OF _MSYSMENU DO lpActRec
*B802047 AHM (End)

*B802047 AHM (Start)
*ON SELECTION PAD _BROWSE OF _MSYSMENU DO lpTab
ON SELECTION PAD _BROWSE OF _MSYSMENU ACTIVATE WINDOW(lcDet_Ttl)
*B802047 AHM (End)

DO (gcScrDir+gcWinAppl+'\MAPRCAM.SPX')

POP KEY
SET FILT TO
RELEASE PAD _Option OF _MSYSMENU
RELEASE PAD _WipOptn OF _MSYSMENU
RELEASE WINDOWS (lcDet_Ttl)

*-- HDM B602979,1 [Start] Release Menue pad Browse & Browse1
RELEASE PAD _BROWSE  OF _MSYSMENU
RELEASE PAD _BROWSE1 OF _MSYSMENU
*-- HDM B602979,1 [End]
*khalid
RELEASE BAR 100 OF P01PU01


*B802047 AHM (Start)
IF llShowRec
  RELEASE WINDOW (lcRec_Ttl)
ENDIF  
*B802047 AHM (Start)
IF glQuitting
  *-- IF it is opened by the program, close it  (AAMER)
  IF llOpnVend
    USE IN APVENDOR
  ENDIF

  USE IN (lcActTmp)
  ERASE (gcWorkDir+lcActTmp+'.DBF')

  USE IN (lcPOHdr)
  ERASE (gcWorkDir+lcPOHdr+'.DBF')
  *B602179,1 AMM Erase temporary files when quitting the program
  ERASE (gcWorkDir+lcPOHdr+'.CDX')
  *B602179,1 AMM  end
  *B804444,1 (Begin) If the user is in the select mode after saving MPO.
  IF !EMPTY(laNewMpos)
    lcPoNo = IIF(ALEN(laNewMpos,1)>1,ALLTRIM(STR(ALEN(laNewMpos,1),6)),'')
    *--If the user entered at least 1 MPO, ask him.
    * Do you want to print the (1-2..etc) purchase orders you have just entered?
    *     Yes    No
    *B605044,1 ABD - Change the message when we print the report , the message will 
    *B605044,1 ABD - Depended on the screen type. [Begin]
    *IF gfModalGen('INM36179B36001','DIALOG',IIF(ALEN(laNewMpos,1)>1,lcPoNo+ ' purchase orders','purchase order')) = 1
    DO CASE
      CASE lcScrType = 'P'
        lcMessage = IIF(ALEN(laNewMpos,1)>1,lcPoNo+ ' purchase orders','purchase order')
        lcTypPrint = 'P'
      CASE lcScrType = 'R'
        lcMessage = IIF(ALEN(laNewMpos,1)>1,lcPoNo+ ' return purchase orders','return purchase order')
        lcTypPrint = 'R'
      CASE lcScrType = 'C'
        lcMessage = IIF(ALEN(laNewMpos,1)>1,lcPoNo+ ' contracts','contract')
        lcTypPrint = 'C'
    ENDCASE
    IF gfModalGen('INM36179B36001','DIALOG',lcMessage) = 1
      *B605044,1 ABD - [End]
      SELECT POFHDR

      *B605935,1 AMH Comment the set ket which make set skip command in the report program 
      *B605935,1     does not work & insert the laNewMPos elements into cursor lcNewMPos [Start]
      *IF ALEN(laNewMpos,1)>1
      *  SET KEY TO RANGE laNewMpos[1], laNewMpos[ALEN(laNewMpos,1)]
      *ELSE
      *  SET KEY TO laNewMpos[1]
      *ENDIF
      SELECT (lcNewMPos)
      ZAP
      FOR lnI = 1 TO ALEN(laNewMPos,1)
        *B606327,1 (Begin) Rename the field.
        *INSERT INTO (lcNewMPos) (MATPO) VALUES (SUBSTR(laNewMPos[lnI],2))
        INSERT INTO (lcNewMPos) (POMAT) VALUES (SUBSTR(laNewMPos[lnI],2))
        *B606327,1 (End)
      ENDFOR
      *B605935,1 AMH [End]
      
      *B605044,1 ABD - Send enter to the buffer to print the report, this 2 
      *B605044,1 ABD - line is too important don't comment any line bettween this entry 605044. [Begin]
      KEYBOARD  CHR(13)
      WAIT WINDOW ''
      *B605044,1 ABD -  [End]
      =gfCPPrint()
    ENDIF
  ENDIF
 *B804444,1 (End) 
  *B606260,1 (Begin) Don't close the files unless it exists.
  IF FILE(gcWorkDir+lcDetTmp+'.DBF') AND USED(lcDetTmp)
  *B606260,1 (End) 
    USE IN (lcDetTmp)
    ERASE (gcWorkDir+lcDetTmp+'.DBF')
    ERASE (gcWorkDir+lcDetTmp+'.CDX') 
    ERASE (gcWorkDir+lcDetTmp+'.FPT') 
  *B606260,1 (Begin) Don't close the files unless it exists.
  ENDIF
  *B606260,1 (End) 
  
  *B602110 (Start)
  *-- AAMER 10/26/98 

  *E500442,1 ABD - Close open files that we open before. [Begin]
  *-- we should check the existence of "lcContLin" file not "lcPOLine" file
  *IF FILE(gcWorkDir+lcPOLine+'.DBF')
  *IF FILE(gcWorkDir+lcContLin+'.DBF')
  *B602110 (End)
  *  USE IN (lcContLin)
  *  ERASE (gcWorkDir+lcContLin+'.DBF')
  *  ERASE (gcWorkDir+lcContLin+'.CDX')
  *  ERASE (gcWorkDir+lcContLin+'.FPT')
  *ENDIF
  *-- Close POfln & Pofhdr that opend with anther name.
  IF llContLin
    SELECT (lcContLin)
    SET RELATION TO 
    = gfCloseFile(lcContLin)
  ENDIF
  IF llContHdr
    = gfCloseFile(lcContHdr)
  ENDIF
  *E500442,1 ABD - [End]
  
  IF FILE(gcWorkDir+lcPOLine+'.DBF')
    USE IN (lcPOLine)
    ERASE (gcWorkDir+lcPOLine+'.DBF')
    ERASE (gcWorkDir+lcPOLine+'.CDX')
    ERASE (gcWorkDir+lcPOLine+'.FPT')
  ENDIF
  *B602179,1 AMM Erase temporary files when quitting the program
  IF USED(lcPOTran)
    USE IN lcPOTran
    ERASE (gcWorkDir+lcPOTran+'.DBF')
    ERASE (gcWorkDir+lcPOTran+'.CDX')
    ERASE (gcWorkDir+lcPOTran+'.FPT')
  ENDIF
  *B602179,1 AMM end
  
ENDIF

*!*************************************************************
*! Name        : lfActFolder
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Purpose     : Called when the activ folder is changed
*!               to disable the others folders and show the
*!               gets of the active folder with cases
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls       :
*!*************************************************************
FUNCTION lfActFolder

DO CASE
  CASE lnActFolder = 1
    *B802047 AHM (Start)
    SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
    IF WEXIST(lcRec_Ttl)
      RELEASE WINDOW (lcRec_Ttl)
    ENDIF
    SHOW GETS WINDOW (lcWinC30) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC35) DISABLE ONLY
    
    *B802047 AHM (End)
    SHOW GETS WINDOW (lcWinC31) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC33) DISABLE ONLY    
    SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
    *B803243,1 ABD Activate the screen to be visable to hide it in the spr.[Begin]
    SHOW WINDOW (lcWinC32) SAME
    *B803243,1 ABD [End]
    SHOW WINDOW (lcWinCh2) TOP
    IF laScrMode[2] OR laScrMode[1]
      SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
    ELSE
      SHOW GETS WINDOW (lcWinCh2) ENABLE  ONLY
      IF laScrMode[3]
        SHOW GET laData[23] DISABLE
        SHOW GET laData[25] DISABLE
      ENDIF
      IF PofHdr.nfbreceive+PofHdr.nfabdamage+PofHdr.nfabcancel= 0 
        SHOW GET laData[2] ENABLE  
        SHOW GET ibData2   ENABLE  
        IF !llEditExRt OR laData[23] = gcbasecurr
          SHOW GET laData[24] DISABLE
        ENDIF  
        IF !llEditExRt OR laData[25] = gcbasecurr
          SHOW GET laData[26] DISABLE
        ENDIF  
      ELSE
        *B604678,1 MHM 208/20/2001 enable vendor to work like Style po[Start]
        *SHOW GET laData[2]  DISABLE  
        *SHOW GET ibData2    DISABLE  
        SHOW GET laData[2]  ENABLE
        SHOW GET ibData2    ENABLE
        *B604678,1 MHM 208/20/2001 [End]
        SHOW GET laData[24] DISABLE  
        SHOW GET laData[26] DISABLE  
      ENDIF  
      IF POFHDR.STATUS = 'B' OR POFHDR.STATUS = 'H' OR laScrMode[4]
        SHOW GET lnStatus  ENABLE
        SHOW GET laData[2] ENABLE  
        SHOW GET ibData2   ENABLE  
      ELSE
        SHOW GET lnStatus DISABLE
      ENDIF
    ENDIF  
    
  CASE lnActFolder = 2
    SHOW WINDOW (lcWinCh3) TOP
    SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
    *B802047 AHM (Start)
    SHOW GETS WINDOW (lcWinC31) ENABLE  ONLY
    *B802047 AHM (End)
    IF laScrMode[2] OR laScrMode[1]
      SHOW GETS WINDOW (lcWinC31) DISABLE ONLY
      *B802047 AHM (Start)
      SHOW GETS WINDOW (lcWinC35) DISABLE ONLY
      SHOW GETS WINDOW (lcWinC32) DISABLE ONLY
      SHOW GETS WINDOW (lcWinC33) DISABLE ONLY
      *B802047 AHM (Start)
      SHOW GET ibFromRec ENABLE
      *B802047 AHM (End)
    ELSE
      SHOW GETS WINDOW (lcWinC31) ENABLE  ONLY
      SHOW GETS WINDOW (lcWinC32) ENABLE  ONLY
      SHOW GETS WINDOW (lcWinC33) ENABLE  ONLY
      GOTO TOP IN (lcDetTmp)
      IF EOF(lcDetTmp)
        SHOW GET pbRemove DISABLE
      ELSE
        SHOW GET pbRemove ENABLE
      ENDIF
    ENDIF
    IF laScrMode[3] OR laScrMode[4]
      _CUROBJ = OBJNUM(pbNew)
    ENDIF
    =lfBrowLine()
    =lfwLine()
    SHOW GET ibFromBrow ENABLE

    *B802047 AHM (Start)
    SHOW GETS WINDOW (lcWinC30) ENABLE ONLY
    IF llShowRec
      = lfRecBrow()
    ENDIF
    *B802047 AHM (End)

  CASE lnActFolder = 3
    SHOW WINDOW (lcWinCh4) TOP
    *B803243,1 ABD Activate the screen to be visable to hide it in the spr.[Begin]
    SHOW WINDOW (lcWinC32) SAME
    *B803243,1 ABD [End]
    SHOW GETS WINDOW (lcWinC32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC33) DISABLE ONLY    
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY    
    *B802047 AHM (Start)
    IF WEXIST(lcRec_Ttl)
      RELEASE WINDOW (lcRec_Ttl)
    ENDIF
    SHOW GETS WINDOW (lcWinC30) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC35) DISABLE ONLY
    *B802047 AHM (End)
    *B605812,1 ABD - Refresh the Summary folder in case edit the cost per line. [Begin]
    =lfRefresh()
    *B605812,1 ABD - [End]
ENDCASE

*!*************************************************************
*! Name        : lfFillFold
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : to initialize the folders name and windows
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfFillFold

laFoldWinds[1,1] = "Header"
laFoldWinds[1,2] = "lcWinCh2"

laFoldWinds[2,1] = "Detail"
laFoldWinds[2,2] = "lcWinCh3"

laFoldWinds[3,1] = "Summary"
laFoldWinds[3,2] = "lcWinCh4"

*!*************************************************************
*! Name        : lpShow
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : show gets enable and disable due to the screen mode
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]
*    SET ORDER TO TAG TRANS IN UNCMSESS
*    lnReprocess = SET('REPROCESS')
*    IF lnSessNo = 1 .AND. SEEK('O'+PADR('MAT_PO',10)+gcUser_id,'UNCMSESS')
*      SET REPROCESS TO 1
*      SELECT UNCMSESS
*      LOCATE REST WHILE Status+cUTranType+gcUser_id=;
*                       'O'+PADR('MAT_PO',10)+gcUser_id FOR RLOCK() AND ;
*                       cProgram = lcDialog
*      SET REPROCESS TO lnReprocess
*      IF FOUND()
*        IF SEEK(UNCMSESS.cSession,'SyuEror')
*          *-The system detected an incomplete XXX session due to program error :  . Please make sure that you resolve the problem before proceeding.
*          IF gfModalGen('QRM00257B00012','ALERT','Purchase order|'+ALLTRIM(SyuEror.Er_Msg)) = 2
*          	glQuitting = .T. 
*            CLEAR READ
*            RETURN
*          ENDIF
*        ELSE
*          *-The system detected an incomplete XXX session. 
*          =gfModalGen('INM00258B00000','ALERT', 'Purchase order')
*        ENDIF
*        *- The system will now attempt to resume processing on the uncompleted session.
*        =gfModalGen('INM00259B00000','ALERT')
*        IF lfGetTmpFile()
*          SELECT (lcPOHdr)
*          SCATTER FIELDS &lcScFields TO laData
*          llContinue  =.T.
*          laScrMode =.F.
*          laScrMode[2]=(lcScrMode='V')
*          laScrMode[3]=(lcScrMode='E')
*          laScrMode[4]=(lcScrMode='A')
*          SELECT POFHDR
*          SEEK lcKeyType+laData[1]
*          lcSession = UNCMSESS.cSession
*          llModVend=.F.
*          =SEEK(laData[2],'ApVendor')
*          SELECT (lcDetTmp)
*          =lfBrowLine()
*          IF !EMPTY(UnCmSess.cCurrObj)
*            DO CASE
*              CASE ALLTRIM(UnCmSess.cCurrObj) = 'pbDlt'     
*                DO lpDelScr
*              CASE ALLTRIM(UnCmSess.cCurrObj) = 'pbSav'     
*                DO lpSavScr
*            ENDCASE
*          ELSE
*            SHOW GETS  &&&&&pbSav ENABLE
*            SELECT (lcDetTmp)
*            =lfBrowline()    
*          ENDIF
*          RETURN
*        ENDIF
*      ENDIF
*    ENDIF
*    SELECT(lcDetTmp)
*    ZAP
    IF gfUnCompSession(lcUnsmsPrg,lnSessNo,lcDialog)
    *IF gfUnCompSession('MAT_PO')
      SELECT (lcPOHdr)
      SCATTER FIELDS &lcScFields TO laData
      llContinue  =.T.
      laScrMode =.F.
      laScrMode[2]=(lcScrMode='V')
      laScrMode[3]=(lcScrMode='E')
      laScrMode[4]=(lcScrMode='A')
      SELECT POFHDR
      SEEK lcKeyType+laData[1]
      lcSession = UNCMSESS.cSession
      llModVend=.F.
      =SEEK(laData[2],'ApVendor')
      SELECT (lcDetTmp)
      =lfBrowLine()
      IF !EMPTY(UnCmSess.cCurrObj)
        DO CASE
          CASE ALLTRIM(UnCmSess.cCurrObj) = 'pbDlt'     
            DO lpDelScr
          CASE ALLTRIM(UnCmSess.cCurrObj) = 'pbSav'     
            DO lpSavScr
        ENDCASE
      ELSE
        SHOW GETS  &&&&&pbSav ENABLE
        SELECT (lcDetTmp)
        =lfBrowline()    
      ENDIF
      RETURN
    ELSE    
      =lfcrtUncmp()
      =lfBrowLine()
    ENDIF

    = gfwCodePop(@laCodInfo, "CDIVISION","N")
    = gfwCodePop(@laCodInfo, "CTERMCODE","N")
    = gfwCodePop(@laCodInfo, "SHIPVIA","N")
    lnQuaFab = 1
    lcQuality = '1'
    lcStatus  = IIF(lcKeyType = 'C','Hold','Open')
    lnStatus  = IIF(lcKeyType = 'C',5,1)
    llSavAct  = .F.
    SHOW GET ibFolder[3] DISABLE  
    SHOW GET ibFolder[2] ENABLE      
    SHOW GET laData[2] ENABLE
    SHOW GET ibData2   ENABLE    
    SELECT(lcBaseFile)
    lnlastfold  = lnactfolder
    lnactfolder = 1
    =lfChngFolder(lnActFolder)
    lcVEnAdd1   = SPACE(30)
    lcVEnAdd2   = SPACE(30)
    lcVEnAdd3   = SPACE(30)
    lcVEnAdd4 = SPACE(30)
    lcVEnAdd5 = SPACE(30)
    lcVenName = SPACE(30)
    lcWareDEsc= SPACE(30)
    lcFabric  = SPACE(07) 
    lcColor   = SPACE(06) 
    
    *C102780,1 AMH Update the vendor item, vendor color and delivry date [Start]
    IF llVenRef
      lcVenFab  = SPACE(10)
      lcVenColr = SPACE(10)
    ENDIF
    IF llDlivDate
      ldDelvDate = {  /  /    }
    ENDIF
    *C102780,1 AMH [End]
    
    lcPattern = SPACE(10) 
    lcWidth   = SPACE(06)
    lcClrDesc = SPACE(15)
    lcIDesc   = SPACE(20)
    lnCost1   = 0 
    lnCost2   = 0 
    lnCost3   = 0 
    lnCost4   = 0       
    lnQty     = 0
    lnTotQty  = 0
    lnLineNo  = 0
    llEdCost    = .F.
    laData[3]  = IIF(llMultiWH,SPACE(06),qWareHouse)
    SET MARK OF BAR 2 OF _OPTIONPOP TO llEdCost
    laData[9]  = GDSYSDATE
    laData[10] = GDSYSDATE+90 
    laData[23] = gcbasecurr
    laData[25] = gcbasecurr
    lcFrnSign1 = gfGetExSin("",ALLTRIM(laData[23]))
    lcFrnSign2 = gfGetExSin("",ALLTRIM(laData[25]))  
    lcFrnSmbl1 = '*'
    lcFrnSmbl2 = '*'
    laData[24] = 1
    laData[26] = 1
    SHOW GET pbDlt,1 PROMPT lcPromp
    SHOW GET pbDlt     DISABLE
    SHOW GET pbInvNote DISABLE
    SHOW GET pbNew     DISABLE    
    SHOW GET lcFabric  DISABLE
    SHOW GET lcColor   DISABLE    
    
    *C102780,1 AMH Show the vendor item, vendor color and delivry date [Start]
    IF llVenRef
      SHOW GET lcVenFab DISABLE
      SHOW GET lcVenColr DISABLE
    ENDIF
    IF llDlivDate
      SHOW GET ldDelvDate DISABLE
    ENDIF
    *C102780,1 AMH [End]
    
    SHOW GET lcPattern DISABLE
    SHOW GET lcWidth   DISABLE    
    SHOW GET lnCost1   DISABLE
    SHOW GET lnCost2   DISABLE    
    SHOW GET lnCost3   DISABLE
    SHOW GET lnCost4   DISABLE    
    IF llApIstall AND lcScrType $ 'PR'
      SHOW GET pbClose   DISABLE
    ENDIF
    
    *E500442,1 ABD - Don't call this function again to speed up the screen .[Begin]
    *IF lcKeyType = 'P' OR lcKeyType = 'C'
    *  =lfContLin()
    *ENDIF
    *E500442,1 ABD - [End]
    
  CASE laScrMode[4]
    lcScrMode = 'A'
    llcUpdate = .T.
    IF llContinue
      SELECT (lcDetTmp)
      =lfBrowLine()    
      laCodInfo[1,07] = lcPOhdr 
      laCodInfo[1,08] = lcPOhdr
      laCodInfo[1,09] = "lcScrType+laData[1]"
      laCodInfo[2,07] = lcPOhdr
      laCodInfo[2,08] = lcPOhdr                
      laCodInfo[2,09] = "lcScrType+laData[1]"
      laCodInfo[3,07] = lcPOhdr
      laCodInfo[3,08] = lcPOhdr                 
      laCodInfo[3,09] = "lcScrType+laData[1]"
      =gfwCodePop(@laCodInfo, "CDIVISION","T")
      =gfwCodePop(@laCodInfo, "CTERMCODE","T")
      =gfwCodePop(@laCodInfo, "SHIPVIA","T")
      laData[11] = laDiv[lnDiv,2]
      laData[12] = laTerms[lnTerms,2]
      laData[13] = laShip[lnShip,2]
    ELSE
      SELECT (lcDetTmp)
      DELETE ALL FOR !EMPTY(POMAT)
      GOTO TOP
      IF EOF()
        lnQuaFab  = 1
        lcQuality = '1'
      ENDIF
      SELECT(lcBaseFile)
      *--Create uncmsess record.
      =lfCrUnSess('') 
      laData[3]  = IIF(!llMultiWH,qWareHouse,laData[3])
      laData[11] = IIF(EMPTY(laData[11]),SPACE(06),laData[11])
      laData[12] = IIF(EMPTY(laData[12]),SPACE(06),laData[12])
      laData[13] = IIF(EMPTY(laData[13]),SPACE(06),laData[13])
      lcVEnAdd1  = IIF(EMPTY(lcVEnAdd1),SPACE(30),lcVEnAdd1)
      lcVEnAdd2  = IIF(EMPTY(lcVEnAdd2),SPACE(30),lcVEnAdd2)
      lcVEnAdd3  = IIF(EMPTY(lcVEnAdd3),SPACE(30),lcVEnAdd3)
      lcVEnAdd4  = IIF(EMPTY(lcVEnAdd4),SPACE(30),lcVEnAdd4)
      lcVEnAdd5  = IIF(EMPTY(lcVEnAdd5),SPACE(30),lcVEnAdd5)                        
      IF EMPTY(laData[11])
        =gfwCodePop(@laCodInfo, "CDIVISION","D")
        laData[11] = laDiv[lnDiv,2]
      ENDIF
      IF EMPTY(laData[12])
        =gfwCodePop(@laCodInfo, "CTERMCODE","D")
        laData[12] = laTerms[lnTerms,2]
      ENDIF
      IF EMPTY(laData[13])  
        =gfwCodePop(@laCodInfo, "SHIPVIA","D")
        laData[13] = laShip[lnShip,2]
      ENDIF  
      laData[9]  = IIF(EMPTY(laData[9]),gdSysDate,laData[9])
      laData[10] = IIF(EMPTY(laData[10]),gdSysDate+90,laData[10])      
      laData[23] = IIF(EMPTY(laData[23]),gcbasecurr,laData[23])
      laData[25] = IIF(EMPTY(laData[25]),gcbasecurr,laData[25])
      laData[24] = IIF(laData[24] = 0,1,laData[24])
      laData[26] = IIF(laData[26] = 0,1,laData[26])
      lcVEnAdd1  = IIF(EMPTY(laData[2]),SPACE(30),lcVEnAdd1)
      lcVEnAdd2  = IIF(EMPTY(laData[2]),SPACE(30),lcVEnAdd2)
      lcVEnAdd3  = IIF(EMPTY(laData[2]),SPACE(30),lcVEnAdd3)
      lcVEnAdd4  = IIF(EMPTY(laData[2]),SPACE(30),lcVEnAdd4)
      lcVEnAdd5  = IIF(EMPTY(laData[2]),SPACE(30),lcVEnAdd5)
      lcVenName  = IIF(EMPTY(laData[2]),SPACE(30),lcVenName)
      *E301937,1 (Begin) Get the link code from the setup firstly.
      *laData[56] = 'DEFDEF'
      laData[56] = IIF(EMPTY(laSetUp[10,2]),'DEFDEF',laSetUp[10,2])
      *E301937,1 (End)
      IF !llMultiWH
        lcWareDesc  = WAREHOUS.cDesc
        laData[4]=gfGetAdr('WareHous','','','',1,'')
        laData[4]=laData[4]+IIF(LEN(laData[4])<30,SPACE(30-LEN(laData[4])),'')
        FOR lnCounter = 1 TO 5
          L=ALLTRIM(STR(lnCounter))
          lcTempVr=''
          FOR lnCount = 1 TO ALEN(laAddress,1)
            IF laAddress[lnCount,1] = lnCounter
              lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
              lcTempVr = lcTempVr+IIF(EMPTY(lcTempVr) .OR. RIGHT(lcTempVr,1) = ',' ,'',', ') + lcAddPart
            ENDIF
          ENDFOR
         laData[3+lnCounter]=lcTempVr+IIF(LEN(lcTempVr)<30,SPACE(30-LEN(lcTempVr)),'')
       ENDFOR
     ENDIF  
     lcFabric  = SPACE(07) 
     lcColor   = SPACE(06) 
     
     *C102780,1 AMH Update the vendor item, vendor color and delivry date [Start]
     IF llVenRef
       lcVenFab  = SPACE(10)
       lcVenColr = SPACE(10)
     ENDIF
     IF llDlivDate
       ldDelvDate = {  /  /    }
     ENDIF
     *C102780,1 AMH [End]
     
     lcPattern = SPACE(10) 
     lcWidth   = SPACE(06)
     lcClrDesc = SPACE(15)
     lcIDesc   = SPACE(20)
     lnCost1   = 0 
     lnCost2   = 0 
     lnCost3   = 0 
     lnCost4   = 0       
     lnQty     = 0
     lnTotQty  = 0
     llEdCost  = .F.
     lcStatus  = IIF(lcKeyType = 'C','Hold','Open')
     lnStatus  = IIF(lcKeyType = 'C',5,1)
     SET MARK OF BAR 2 OF _OPTIONPOP TO llEdCost
     lcFrnSign1 = gfGetExSin("",ALLTRIM(laData[23]))
     lcFrnSign2 = gfGetExSin("",ALLTRIM(laData[25]))  
   ENDIF
   IF llApIstall AND lcScrType $ 'PR'
    SHOW GET pbClose   DISABLE
   ENDIF  
    SHOW GET laData[2]   ENABLE
    SHOW GET ibData2     ENABLE    
    SHOW GET ibFromBrow  ENABLE
    _CUROBJ = OBJNUM(laData[2])
    
    *B602154 Enabling summary folder in add mode (Start)
    *-- AAMER 11/29/98
    *SHOW GET ibFolder[3] DISABLE  
    SHOW GET ibFolder[3] ENABLE
    *B602154 Enabling summary folder in add mode (End)
    
    IF llEditExRt
      SHOW GET laData[24] ENABLE
      SHOW GET laData[26] ENABLE        
    ELSE
      SHOW GET laData[24] DISABLE
      SHOW GET laData[26] DISABLE
    ENDIF  
    IF llMultiWH
      SHOW GET laData[3] ENABLE
      SHOW GET ibData3   ENABLE   
    ELSE
      SHOW GET laData[3] DISABLE
      SHOW GET ibData3 DISABLE
    ENDIF  
    GOTO TOP IN (lcDetTmp) 
    IF !EOF(lcDetTmp)
      SHOW GET pbRemove ENABLE
    ELSE
      SHOW GET pbRemove DISABLE
    ENDIF     
    lcPromp = lcCancel
    SHOW GET pbDlt,1 PROMPT lcPromp
    SHOW GET pbDlt     DISABLE
    SHOW GET pbInvNote DISABLE
    =lfBrowLine()
    lnLastFold  = lnActFolder
    lnActFolder = 1
    =lfChngFolder(lnActFolder)
  CASE laScrMode[2]
    lcScrMode = 'V'
    =lfInit(.T.)
    IF lnActFolder = 2
      =lfBrowLine()
      =lfwLine()
    ENDIF  
    SELECT (lcPOHdr)
    GATHER FROM laData FIELDS &lcScFields MEMO    
    SELECT(lcBaseFile)
    
    laCodInfo[2,07] = "POFHDR" 
    laCodInfo[2,08] = "POFHDR"
    laCodInfo[2,09] = "lcKeyType+laData[1]" 
    laCodInfo[2,10] = "cDivision"
    
    *B605033,1  ABD 10/23/2001 Editing terms on material po does not save. [Begin]
    *= gfwCodePop(@laCodInfo, "CTERMCODE","T")
    *= gfwCodePop(@laCodInfo, "CDIVISION","T")
    *= gfwCodePop(@laCodInfo, "SHIPVIA","T")
    *-- Get the Terms Code.
    =gfwCodePop(@laCodInfo,'CTERMCODE','L')
    lnTerms = ASCAN('laTerms',Pofhdr.ctermcode)
    lnTerms = IIF(lnTerms = 0,1,ASUBSCRIPT(laTerms,lnTerms,1))

    *-- Get the Division Code.
    =gfwCodePop(@laCodInfo,'CDIVISION','L')
    lnDiv = ASCAN('laDiv',Pofhdr. Cdivision)
    lnDiv= IIF(lnDiv = 0,1,ASUBSCRIPT(laDiv,lnDiv,1))

    *-- Get the Ship Via Code.
    =gfwCodePop(@laCodInfo,'SHIPVIA','L')
    lnShip= ASCAN('laShip',Pofhdr.shipvia)
    lnShip= IIF(lnShip= 0,1,ASUBSCRIPT(laShip,lnShip,1))
    *B605033,1  ABD 10/23/2001 Editing terms on material po does not save. [End]
    
    *E500442,5 ABD - Assign the correct status to the variable. [Begin]
    lnStatus = ASCAN(laStatus,lcStatus)
    *E500442,5 ABD - [End]

    SHOW GET ibFolder[3]  ENABLE    
    SHOW GET ibFolder[2]  ENABLE        
    IF llApIstall AND lcScrType $ 'PR' AND lcStatus <> 'Bid'
      SHOW GET pbClose ENABLE
    ENDIF  
    lcPromp   = lcCancel
    lcDelMesag = 'cancel'
    lcMenProm  = PROPER(lcDelMesag)
    DEFINE BAR 9  OF P03PU03 PROMPT '\<Edit' SKIP FOR .F.
    DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .F.
    IF lcStatus  = 'Cancelled'
      lcPromp    = lcUnCan
      lcDelMesag = 'uncancel'
      lcMenProm  = PROPER(lcDelMesag)
      DEFINE BAR 9  OF P03PU03 PROMPT '\<Edit'    SKIP FOR .T.
      DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .F.
      SHOW GET pbEdt   DISABLE    
      SHOW GET pbClose DISABLE    
    ENDIF
    SHOW GET pbDlt,1 PROMPT lcPromp
    SHOW GET pbDlt ENABLE    
    SHOW GET pbInvNote ENABLE
    IF lcStatus  = 'Closed'
      lcDelMesag = 'Cancel'
      lcMenProm  = PROPER(lcDelMesag)
      DEFINE BAR 9  OF P03PU03 PROMPT '\<Edit'    SKIP FOR .T.
      DEFINE BAR 10 OF P03PU03 PROMPT (lcMenProm) SKIP FOR .T.
      SHOW GET pbEdt   DISABLE    
      SHOW GET pbDlt   DISABLE    
      SHOW GET pbClose DISABLE    
    ENDIF

    *B605935,1 AMH Save the current MPO to print it only from gfCPPrint function [Start]
    *B606327,1 (Begin) Rename the field.
    *REPLACE (lcNewMPos+'.MATPO') WITH laData[1]
    REPLACE (lcNewMPos+'.POMAT') WITH laData[1]
    *B606327,1 (End)
    *B605935,1 AMH [End]
  CASE laScrMode[3]
    llCUpdate  = .T.
    lcScrMode = 'E'
    *SHOW GET ibFolder[3]  DISABLE
    DO CASE
      CASE POFHDR.STATUS = 'O'
        IF lnactfolder = 3 
          lnlastfold  = lnactfolder
          lnactfolder = 1
          =lfchngfolder(lnactfolder)
        ENDIF
        IF PofHdr.nfbreceive+PofHdr.nfabdamage+PofHdr.nfabcancel= 0
          SHOW GET laData[2] ENABLE  
          SHOW GET ibData2   ENABLE  
          IF llEditExRt AND ladata[23] <> gcBaseCurr
            SHOW GET laData[24] ENABLE
          ELSE
            SHOW GET laData[24] DISABLE
          ENDIF 
          IF llEditExRt AND ladata[25] <> gcBaseCurr
            SHOW GET laData[26] ENABLE        
          ELSE  
            SHOW GET laData[26] DISABLE
          ENDIF  
        ELSE
          *B604678,1 MHM 208/20/2001 enable vendor to work like Style po[Start]
          *SHOW GET laData[2]  DISABLE  
          *SHOW GET ibData2    DISABLE  
          SHOW GET laData[2]  ENABLE        
          SHOW GET ibData2    ENABLE        
          *B604678,1 MHM 208/20/2001 [End]
          
          SHOW GET laData[24] DISABLE  
          SHOW GET laData[26] DISABLE  
        ENDIF  
        SHOW GET lnStatus DISABLE
      CASE POFHDR.STATUS = 'X'
        =gfModalGen('INM36000B36000','DIALOG',lcPrgtype+SPACE(01)+laData[1])
        laScrMode[2] = .T.
        laScrMode[3] = .F.
        REPLACE POFHDR.lLok_Stat WITH .F.
        llContinue = .T.
        SHOW GETS
      CASE POFHDR.STATUS = 'C'
        lnlastfold  = lnactfolder
        lnactfolder = 1
        SHOW GET ibFolder[2] DISABLE  
        SHOW GET laData[2]   DISABLE  
        SHOW GET ibData2     DISABLE
        SHOW GET lnStatus    DISABLE
    ENDCASE
    IF llApIstall AND lcScrType $ 'PR'
      SHOW GET pbClose DISABLE
    ENDIF  
    IF !llContinue 
      *--Create uncmsess record.
      =lfCrUnSess('') 
    ENDIF
    GOTO TOP IN (lcDetTmp) 
    IF !EOF(lcDetTmp)
      SHOW GET pbRemove ENABLE
    ELSE
      SHOW GET pbRemove DISABLE
    ENDIF
    IF llMultiWH
      SHOW GET laData[3] ENABLE
      SHOW GET ibData3   ENABLE
    ELSE
      SHOW GET laData[3] DISABLE
      SHOW GET ibData3   DISABLE
    ENDIF  
    IF llEdCost
      SHOW GET lnCost2    ENABLE        
      SHOW GET lnCost3    ENABLE        
      SHOW GET lnCost4    ENABLE        
    ELSE
      SHOW GET lnCost2    DISABLE        
      SHOW GET lnCost3    DISABLE        
      SHOW GET lnCost4    DISABLE        
    ENDIF  
    SHOW GET laData[23] DISABLE
    SHOW GET laData[25] DISABLE    
    SHOW GET pbInvNote ENABLE    
    =lfchngfolder(lnactfolder)
ENDCASE
=lfActFolder()
*!*************************************************************
*! Name        : lfvData1
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Validate the P/O field and if 
*!               found swithch the screen mode to view mode
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData1

*E500442,5 ABD - Define New Variables. [Begin]
PRIVATE lcOldFiltr
lcOldFiltr = ''
*E500442,5 ABD - [End]

lnAlias = SELECT()
llMdown = MDOWN()
IF llMdown
  RETURN
ENDIF  
SELECT POFHDR
IF !EMPTY(laData[1]) AND !laScrMode[4]
  *E500442,5 ABD - Remove the filter before the seek to speed 
  *E500442,5 ABD - Up the screen while we browse . [Begin]
  lcOldFiltr = SET('FILTER')
  SET FILTER TO
  *E500442,5 ABD - [End]
  IF !SEEK(lcKeyType+laData[1],"POFHDR")
    *E500442,5 ABD - Return the old Filter. [Begin]
    SET FILTER TO &lcOldFiltr
    *E500442,5 ABD - [End]
    lcPOBrow  = laData[1]
    lcVenBrow = laData[2]
    =POFBROW (lcKeyType,@lcVenBrow ,@lcPOBrow)
    laData[1] = lcPOBrow  
  *E500442,5 ABD - Return the old Filter. [Begin]
  ELSE
    SET FILTER TO &lcOldFiltr
  *E500442,5 ABD - [End]
  ENDIF


  
  IF !EMPTY(laData[1])
    IF POFHDR.Status = 'X'
      lcStatus = 'Cancelled'    
    ENDIF 
    laScrMode[1] = .F.
    laScrMode[2] = .T.
    SHOW GETS
  ENDIF
ENDIF  

*!*************************************************************
*! Name        : lfvIData1
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Validate the P/O field from the key field 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvIData1
laData[1]     = PADR("?",6)
llNothing = lfvData1()

*!*************************************************************
*! Name        : lfInit
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Initialize the ladata array from the POFHDR file 
*!               if the screen mode is view mode
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfInit
PARAMETER llFromfile

lnAlias = SELECT()
IF llFromfile
  DIMENSION laStatus[6]
  laStatus[1]  = 'Open'
  laStatus[2]  = 'Bid'
  laStatus[3]  = 'Cancelled'
  *B605173 WAB (Start) the status is 'Completed'
  *laStatus[4]  = 'Complete'
  laStatus[4]  = 'Completed'
  *B605173 WAB (End) 
  
  *E500442,5 ABD - Update the array with the correct status. [Begin]
  *laStatus[5]  = 'Hold'
  *laStatus[5]  = 'Closed'
  laStatus[5]  = 'Hold'
  laStatus[6]  = 'Closed'
  *E500442,5 ABD - [End]
  
  =SEEK(lcKeyType+laData[1],"POFHDR")
  laData[2] = POFHDR.Vendor
  =SEEK(laData[2],"APVENDOR")
  lcVenName = APVENDOR.cVenComp
  lcVEnAdd1 =gfGetAdr('ApVendor','','','',1,'')
  lcVEnAdd1 =lcVEnAdd1+IIF(LEN(lcVEnAdd1)<30,SPACE(30-LEN(lcVEnAdd1)),'')
  FOR lnCounter = 2 TO 5
    L=ALLTRIM(STR(lnCounter))
    lcTempVr=''
    FOR lnCount = 1 TO ALEN(laAddress,1)
      IF laAddress[lnCount,1] = lnCounter
        lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
        lcTempVr  = lcTempVr+IIF(EMPTY(lcTempVr) .OR. RIGHT(lcTempVr,1) = ',' ,'',', ') + lcAddPart
      ENDIF
    ENDFOR
    lcVEnAdd&L=lcTempVr+IIF(LEN(lcTempVr)<30,SPACE(30-LEN(lcTempVr)),'')
  ENDFOR
  laData[3] = POFHDR.cWareCode
  lcWareDEsc = IIF(SEEK(laData[3],"WAREHOUS"),WAREHOUS.cDesc,SPACE(10))
  laData[4]   = PofHdr.cOutAddr1
  laData[5]   = PofHdr.cOutAddr2
  laData[6]   = PofHdr.cOutAddr3
  laData[7]   = PofHdr.cOutAddr4
  laData[8]   = PofHdr.cOutAddr5
  laData[9]   = PofHdr.entered
  laData[10]  = PofHdr.complete
  laData[11]  = PofHdr.cDivision
  laData[12]  = PofHdr.cTermCode
  laData[13]  = PofHdr.ShipVia
  laData[14]  = POFHDR.Insurance
  laData[15]  = POFHDR.cFob
  laData[16]  = POFHDR.QuotaCat
  laData[17]  = POFHDR.Contact
  laData[18]  = POFHDR.Phone
  laData[19]  = POFHDR.Origin
  laData[20]  = POFHDR.CLCNO
  laData[21]  = POFHDR.lcExpire
  laData[22]  = POFHDR.cRequis
  laData[23]  = POFHDR.cpricecur
  lcFrnSign1  = gfGetExSin("",ALLTRIM(laData[23]))
  =SEEK(laData[23],'syccurr')
  lcFrnSmbl1  = syccurr.ccurrsmbl
  lnUnit1     = syccurr.nCurrUnit
  laData[25]  = POFHDR.cdutycur
  lcFrnSign2  = gfGetExSin("",ALLTRIM(laData[25]))  
  =SEEK(laData[25],'syccurr')  
  lcFrnSmbl2  = syccurr.ccurrsmbl
  lnUnit2     = syccurr.nCurrUnit
  laData[24]   = POFHDR.npricerat
  laData[26]   = POFHDR.ndutyrat
  ladata[39]   = POFHDR.nCost1
  ladata[40]   = POFHDR.nCost2
  ladata[41]   = POFHDR.nCost3
  ladata[42]   = POFHDR.nCost4      
  ladata[43]   = POFHDR.nLan_cost1
  ladata[44]   = POFHDR.nLan_cost2
  ladata[45]   = POFHDR.nLan_cost3
  ladata[46]   = POFHDR.nLan_cost4
  ladata[47]   = POFHDR.nAct_cost1
  ladata[48]   = POFHDR.nAct_cost2
  ladata[49]   = POFHDR.nAct_cost3
  ladata[50]   = POFHDR.nAct_cost4
  ladata[27]  = POFHDR.nEcost1
  ladata[28]  = POFHDR.nEcost2
  ladata[29]  = POFHDR.nEcost3
  ladata[30]  = POFHDR.nEcost4   
  ladata[31]  = POFHDR.nelancost1
  ladata[32]  = POFHDR.nelancost2
  ladata[33]  = POFHDR.nelancost3
  ladata[34]  = POFHDR.nelancost4
  ladata[35]  = POFHDR.neactcost1      
  ladata[36]  = POFHDR.neactcost2     
  ladata[37]  = POFHDR.neactcost3      
  ladata[38]  = POFHDR.neactcost4
  ladata[51]  = POFHDR.nFabOrder
  ladata[52]  = POFHDR.nFbReceive
  ladata[53]  = POFHDR.nFabDamage
  ladata[54]  = POFHDR.nFabCancel
  ladata[55]  = POFHDR.nPo_Open
  ladata[56]  = POFHDR.LINK_CODE
  lnLastLine  = POFHDR.LastLine
  lcQuality   = POFHDR.cFabGrade
  lnQuaFab    = ASCAN(laQuality,lcQuality)/2
  DO CASE
    CASE POFHDR.Status = 'O'
      lnStatus = 1
      lcStatus = 'Open'
    CASE POFHDR.Status = 'B'
      lnStatus = 2
      lcStatus = 'Bid'
    CASE POFHDR.Status = 'X'
      lnStatus = 3
      lcStatus = 'Cancelled'
      *B603259,1 HDM [Start] No Need to call lfRefresh here
      *                      as 'lfinit()' may called before 
      *                      the screen starting
      *=lfRefresh('lcWinCh1')
      *B603259,1 HDM [End]
    CASE POFHDR.Status = 'C'        
      lnStatus = 4    
      lcStatus = 'Completed'    
    CASE POFHDR.Status = 'H'
      lnStatus = 5    
      lcStatus = 'Hold'    
    CASE POFHDR.Status = 'L'
      lnStatus = 6    
      lcStatus = 'Closed'    
      *B603259,1 HDM [Start] No Need to call lfRefresh here
      *                      as 'lfinit()' may called before 
      *                      the screen starting
      *=lfRefresh('lcWinCh1')
      *B603259,1 HDM [End]
  ENDCASE
  
  *C102780,1 AMH Get the revision number [Start]
  laData[57] = POFHDR.CREVNO
  *C102780,1 AMH [End]
  
  *C102819,1 AMH Get the MRP number [Start]
  IF llMrp
    laData[58] = POFHDR.CMRP
  ENDIF
  *C102819,1 AMH [End]
  
  =lfPOLines()
  SELECT (lcPOHdr)
  GATHER FROM laData FIELDS &lcScFields MEMO    
ELSE
  laData[1]   = SPACE(06)
  laData[2]   = SPACE(08)
  lcVenName   = SPACE(30)
  laData[3]   = IIF(llMultiWH, SPACE(06), qWareHouse )
  lcWareDEsc  = SPACE(30)
  lcVEnAdd1   = SPACE(30)
  lcVEnAdd2   = SPACE(30)
  lcVEnAdd3   = SPACE(30)
  lcVEnAdd4   = SPACE(30)
  lcVEnAdd5   = SPACE(30)
  lcVEnAdd6   = SPACE(30)
  laData[4]   = SPACE(30)
  laData[5]   = SPACE(30)
  laData[6]   = SPACE(30)
  laData[7]   = SPACE(30)
  laData[8]   = SPACE(30)
  lcShiAdd6   = SPACE(30)

  laData[9]   = GDSYSDATE
  laData[10]  = GDSYSDATE + 90
  laData[14]  = SPACE(18)
  laData[15]  = SPACE(18)
  laData[16]  = SPACE(18)
  laData[17]  = SPACE(20)
  laData[18]  = SPACE(16)
  laData[19]  = SPACE(18)
  laData[20]  = SPACE(18)
  laData[21]  = GDSYSDATE
  laData[22]  = SPACE(30)
  laData[23]  = gcbasecurr
  laData[25]  = gcbasecurr
  lcFrnSmbl1  = SPACE(05) 
  lcFrnSmbl2  = SPACE(05)
  laData[24]  = 1
  laData[26]  = 1
  lcFabric    = SPACE(07)
  lcColor     = SPACE(06)
  
  *C102780,1 AMH Update the vendor item, vendor color and delivry date [Start]
  IF llVenRef
    lcVenFab  = SPACE(10)
    lcVenColr = SPACE(10)
  ENDIF
  IF llDlivDate
    ldDelvDate = {  /  /    }
  ENDIF
  *C102780,1 AMH [End]
  
  lcIDesc     = SPACE(20) 
  lcClrDesc   = SPACE(15) 
  lcPattern   = SPACE(10)
  lcWidth     = SPACE(06)

  lnCost1 = 0
  lnCost2 = 0
  lnCost3 = 0
  lnCost4 = 0
  lnQty   = 0
  lnTotQty= 0

  laData[26] = 0
  ladata[28] = 0
  ladata[29] = 0
  ladata[30] = 0

  ladata[31] = 0
  ladata[32] = 0
  ladata[33] = 0
  ladata[34] = 0

  ladata[35] = 0
  ladata[36] = 0
  ladata[37] = 0
  ladata[38] = 0

  ladata[39] = 0
  ladata[40] = 0
  ladata[41] = 0
  ladata[42] = 0

  ladata[43] = 0
  ladata[44] = 0
  ladata[45] = 0
  ladata[46] = 0

  ladata[47] = 0
  ladata[48] = 0
  ladata[49] = 0
  ladata[50] = 0

  ladata[51] = 0
  ladata[52] = 0
  ladata[53] = 0
  ladata[54] = 0
  ladata[55] = 0
ENDIF
*=lfRefresh()
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfPOLines
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : get all lines from the POFLN for the selected P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfPOLines
*B606710,1 KHM 01/05/2003 (Begin) Declare a varibale to hold the old order of POFLN
PRIVATE lcOldLnOrd
*B606710,1 KHM 01/05/2003 (End)

*E500442,1 ABD 10/21/2001 Enhance the speed of openining the screen. [Begin]

IF USED(lcDetTmp) .AND. !EOF()
  SELECT (lcDetTmp)
  DELETE ALL
ELSE
 SELECT POFLN
  = AFields(laFileStru)
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cMarker'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'lNew'
  laFileStru[lnNewFld,2] = 'L'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0
 
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'nRecno'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 10
  laFileStru[lnNewFld,4] = 0
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cStatus'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0

  *C102525,1 RAE [BEGIN] Create a new field in the temp. file.
  IF ASCAN(laEvntTrig , PADR('CRTEMPFILE',10)) <> 0
    =gfDoTriger('MARTMAT',PADR('CRTEMPFILE',10))
  ENDIF
  *C102525,1 RAE [END]
  
  CREATE table (gcWorkDir+ lcDetTmp) FROM ARRAY laFileStru
  INDEX ON cmattype+POMAT+FABRIC+COLOR TAG (lcDetTmp) OF (gcWorkDir+lcDetTmp)
ENDIF  

SELECT POFLN

*B606710,1 KHM 01/05/2003 (Begin) Saving the old order and set the order to POFLN
lcOldLnOrd = ORDER()
SET ORDER TO POFLN
*B606710,1 KHM 01/05/2003 (End)

IF SEEK(lcKeyType+laData[1])
  SCAN REST WHILE cmattype+pomat+fabric+color+trancd = lcKeyType+laData[1] For TRANCD = "1"
    SCATTER MEMVAR MEMO
    m.cMarker = ' '
    m.lNew    = .F.
    m.cStatus = 'S'
    INSERT INTO (lcDetTmp) FROM MEMVAR 
  ENDSCAN
ENDIF

*B606710,1 KHM 01/05/2003 (Begin) Restoring the old order
SELECT POFLN
SET ORDER TO &lcOldLnOrd
*B606710,1 KHM 01/05/2003 (End)

*SELECT * ,  " " AS cMarker,.F. AS lNew,'S' AS cStatus;
*  FROM POFLN ;
*  WHERE cMatType+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7) = lcKeyType+laData[1];
*    AND TRANCD = "1";
*INTO DBF (gcWorkDir+lcDetTmp)

*SELECT(lcDetTmp)
*INDEX ON cMatType+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7) TAG lcDetTmp
*E500442,1 ABD 10/21/2001 Enhance the speed of openining the screen. [End]

*!*************************************************************
*! Name        : lfBrowLine
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : browse lines for the selected P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfBrowLine

lnAlias = SELECT()
SELECT (lcDetTmp)
GOTO TOP
lcBrowFlds = [cMarker :R:H='',]+;
             [Fabric  :R:H='Item' :7,]+;
             [Color   :R:H='Color':6,]+;
             [Pattern :R:10:H='Pattern',]+;
             [Width   :R:H='Width',]+;
             [nCost1  :R:13:H='Price':P='999999999.999',]+;
             IIF(laData[23]<>gcBaseCurr,[nECost1 :R:13:H='E.Price':6:P='999999999.999',],[])+;
             [nFabTotQty  :R:13:H='Quantity':P='9999999.999',]+;
             [Amount  =nCost1*nFabTotQty:20:R:H='Amount':P='999999999999999.999']
IF llCostPrv   
  IF laData[25] <> gcBaseCurr
    lcCostFlds = [,nCost2 :R:13:H='Freight':P='999999999.999',]+;
                 [nECost2 :R:13:H='E.Freight':6:P='999999999.999',]+;
                 [nCost3  :R:13:H='Tax':P='999999999.999',]+;
                 [nECost3 :R:13:H='E.Tax':6:P='999999999.999',]+;
                 [nCost4  :R:13:H='Quota':P='999999999.999',]+;
                 [nECost4 :R:13:H='E.Quota':6:P='999999999.999']
  ELSE
    lcCostFlds = [,nCost2 :R:13:H='Freight':P='999999999.999',]+;
                 [nCost3  :R:13:H='Tax':P='999999999.999',]+;
                 [nCost4  :R:13:H='Quota':P='999999999.999']
  ENDIF
ELSE  
  lcCostFlds = ''
ENDIF
lcBrowFlds = lcBrowFlds + lcCostFlds 
BROWSE FIELDS &lcBrowFlds;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwLine() ;       
       VALID :F lfvBrows() ;
       TITLE lcDet_Ttl    ;
       WINDOW (lcWinC31) IN WINDOW (lcWinCh3)
=lfRefresh('lcWinC33')
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfwLine
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : refresh the fields when changing the record in the browse
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwLine
glFromBrow = .T.
lnAlias = SELECT()
SELECT (lcDetTmp)
IF !EOF() AND !BOF()
  lnNewRec = RECNO()
  REPLACE ALL cMarker WITH lcUnMark
  GO lnNewRec
  REPLACE cMarker WITH lcMark
  lcFabric    = FABRIC
  lcOldFab    = FABRIC
  lcColor     = COLOR
  lcOldClr    = COLOR
  
  *C102780,1 AMH Update the vendor item, vendor color and delivry date [Start]
  IF llVenRef
    lcVenFab  = CVENFAB
    lcVenColr = CVENCOLR
  ENDIF
  IF llDlivDate
    ldDelvDate  = DDELIVDATE
  ENDIF
  *C102780,1 AMH [End]
  
  *B124255,1 NNA 11/03/2004 (Begin) Check if Fabric File opened or not to Reopen it
  lnOldAlis = SELECT(0)   && Hold the current Alias
  IF !USED('FABRIC')
    =gfOpenFile(gcDataDir+'Fabric',gcDataDir+'Fabric','SH')
  ENDIF
  SELECT(lnOldAlis)       && Select the Previous Alias
  *B124255,1 NNA (End)
  
  =SEEK(lcFabric+lcColor,'FABRIC')
  lcIDesc     = FABRIC.DESC
  lcUOB       = Fabric.UomBuy
  lcClrDesc   = gfCodDes(lcColor, 'COLOR')
  lcPattern   = PATTERN
  lcWidth     = WIDTH
  lnCost1     = nCost1
  lnCost2     = nCost2
  lnCost3     = nCost3
  lnCost4     = nCost4      
  lnQty       = nFabTotQty
  lnTotQty    = nCost1*nFabTotQty
  lnLineNo    = LineNo

  *B802047 AHM (Start)
  IF llShowRec
    PRIVATE lnCurAlias 
    lnCurAlias = SELECT(0)
    SELECT POFLN
    LOCATE FOR cMatType+POMat+Fabric+Color+TranCd = ;
               'P'+ laData[1] + &lcDetTmp..Fabric + ;
               &lcDetTmp..Color AND TranCd <> '1'
    SELECT(lnCurAlias)
    =lfwRecWBro()
  ELSE
  *B802047 AHM (End)

    IF laScrMode[3] OR laScrMode[4]
      SHOW GET pbRemove ENABLE
    ENDIF  
  *B802047 AHM (Start)
  ENDIF
  *B802047 AHM (End)
ENDIF  

*B802047 AHM (Start)
IF !llShowRec
*B802047 AHM (End)
  IF laScrMode[2] OR laScrMode[1] OR EOF(lcDetTmp)
    SHOW GET lcFabric  DISABLE
    SHOW GET lcColor   DISABLE
    SHOW GET ibFabric  DISABLE
    SHOW GET ibColor   DISABLE
    
    *C102780,1 AMH Show the vendor item, vendor color and delivry date [Start]
    IF llVenRef
      SHOW GET lcVenFab DISABLE
      SHOW GET ibVenFab DISABLE
      SHOW GET lcVenColr DISABLE
      SHOW GET ibVenColr DISABLE
    ENDIF
    IF llDlivDate
      SHOW GET ldDelvDate DISABLE
    ENDIF
    *C102780,1 AMH [End]
    
    SHOW GET lcPattern DISABLE
    SHOW GET lcWidth   DISABLE
    SHOW GET lnCost1   DISABLE
    SHOW GET lnCost3   DISABLE
    SHOW GET lnCost2   DISABLE
    SHOW GET lnCost4   DISABLE
    SHOW GET lnQty     DISABLE
  ELSE
    SHOW GET lcFabric  ENABLE
    SHOW GET lcColor   ENABLE
    SHOW GET ibFabric  ENABLE
    SHOW GET ibColor   ENABLE
    
    *C102780,1 AMH Show the vendor item, vendor color and delivry date [Start]
    IF llVenRef
      SHOW GET lcVenFab ENABLE
      SHOW GET ibVenFab ENABLE
      SHOW GET lcVenColr ENABLE
      SHOW GET ibVenColr ENABLE
    ENDIF
    IF llDlivDate
      SHOW GET ldDelvDate ENABLE
    ENDIF
    *C102780,1 AMH [End]
    
    SHOW GET lcPattern ENABLE
    SHOW GET lcWidth   ENABLE
    SHOW GET lnQty     ENABLE
    
    *E500442,1 ABD - Call function to valid the fabric - Color & Price. [Begin]
    *IF lcKeyType $ 'PC' AND SEEK(lcFabric+lcColor,lcContLin) AND &lcContLin..CPRICECUR = laData[23]
      IF lcKeyType $ 'PC' .AND. lfvline('CPRICECUR')
      *E500442,1 ABD - [End]
      SHOW GET lnCost1 DISABLE
    ELSE
      SHOW GET lnCost1 ENABLE
    ENDIF
    
    *E500442,1 ABD - Call function to valid the fabric - Color & Duty curr. [Begin]
    *IF !llEdCost OR (lcKeyType $ 'PC' AND SEEK(lcFabric+lcColor,lcContLin) AND &lcContLin..CDUTYCUR = laData[25])
    IF !llEdCost OR (lcKeyType $ 'PC' .AND. lfvline('CDUTYCUR'))
    *E500442,1 ABD - [End]
    
      SHOW GET lnCost3   DISABLE
      SHOW GET lnCost2   DISABLE
      SHOW GET lnCost4   DISABLE
    ELSE
      SHOW GET lnCost3   ENABLE
      SHOW GET lnCost2   ENABLE
      SHOW GET lnCost4   ENABLE
    ENDIF  
  *B802047 AHM (Start)
  ENDIF
  *B802047 AHM (End)
ENDIF  
=lfRefre3()
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfvobjlink
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : get the objects linked the selected P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvobjlink

lnalais = SELECT()
DO getobj WITH 'P',laData[1]
SELECT (lnalais)
RETURN

*!*************************************************************
*! Name        : lfvNotes
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : get the notes for the selected P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvNotes

=NotePad("M",lcKeyType+laData[1])

*!*************************************************************
*! Name        : lfvData2
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the vendor field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData2

IF MDOWN()
  RETURN
ENDIF

IF laScrMode[4]
 IF !SEEK(laData[2],"ApVendor") AND LASTKEY() <> 9 AND LASTKEY() <> 15
   lcVendor = laData[2]
   *-- E301268,1 HDM [Start] filter vendor browse by sup. type
   *=gfApVnBrow (@lcVendor)
   =gfApVnBrow (@lcVendor,.F.,'M')
   *-- E301268,1 HDM [End]
   laData[2] = lcVendor
 ENDIF  
 IF EMPTY(laData[2]) 
   RETURN
 ELSE
   =SEEK(laData[2],"ApVendor")
   laData[17] = ApVendor.cvencont
   laData[18] = ApVendor.cphoneno
   IF llMulCurr AND laData[2] <> lcOldVen
     laData[23] = ApVendor.ccurrcode
     laData[24] = gfchkrate('lnUnit1',laData[23],laData[9],llEditExRt,gcAct_Comp,.F.) 
     IF laData[24] <= 0
       IF gfModalGen('INM36004B36001','DIALOG','price') = 1
         laData[23] = gcbasecurr
         laData[24] = 1
         lnUnit1    = 1 
       ELSE
         _CUROBJ = OBJNUM(laData[2])  
         RETURN
       ENDIF 
     ENDIF
     IF laData[23] = gcbasecurr
       SHOW GET laData[24] DISABLE
     ELSE
       SHOW GET laData[24] ENABLE
     ENDIF
     IF laData[25] = gcbasecurr
       SHOW GET laData[26] DISABLE
     ENDIF
   ENDIF  
   SHOW GET laData[23]
   SHOW GET laData[17]
   SHOW GET laData[18]
  ENDIF 
ELSE 
  *B604678,1 MHM 208/20/2001 Check if there is any transaction type 2 4 5 
  *B604678,1                 then dont let user change vendor[Start]
  IF laScrMode[3] AND SEEK(lcScrType+laData[1],'POfLN')
    SELECT POfLN 
    LOCATE REST WHILE cMatType+PoMat = lcScrType+laData[1] FOR TranCd $ '245'
    IF FOUND()
      *-Cannot change the vendor since the recieving was done.
      =gfModalGen('TRM34025B34000','DIALOG','vendor'+'|'+'recieving')        
      SELECT POfHDR
      laData[2]=lcOldVen
      RETURN
    ENDIF
  ENDIF
  *B604678,1 MHM 208/20/2001 Check if there is any transaction type 2 4 5 
  *B604678,1                 then dont let user change vendor[End]

  IF LASTKEY() <> 9 AND LASTKEY() <> 15 AND !SEEK(laData[2],'ApVendor') 
    lcVend = laData[2]

   *-- E301268,1 HDM [Start] filter vendor browse by sup. type
   *=gfApVnBrow (@lcVend)
    =gfApVnBrow (@lcVend,.F.,'M')
   *-- E301268,1 HDM [End]
    laData[2] = lcVend
    IF EMPTY(laData[2])  
      laData[2] = lcOldVen

    *B607276,1 ALB Fix bug changing vendor contact name and telephone when change vendor in edit mode [Begin]
    ELSE
      =SEEK(laData[2],"ApVendor")
      laData[17] = ApVendor.cvencont
      laData[18] = ApVendor.cphoneno
      IF llMulCurr AND laData[2] <> lcOldVen
        laData[23] = ApVendor.ccurrcode
        laData[24] = gfchkrate('lnUnit1',laData[23],laData[9],llEditExRt,gcAct_Comp,.F.) 
        IF laData[24] <= 0
          IF gfModalGen('INM36004B36001','DIALOG','price') = 1
            laData[23] = gcbasecurr
            laData[24] = 1
            lnUnit1    = 1 
          ELSE
            _CUROBJ = OBJNUM(laData[2])  
            RETURN
          ENDIF 
        ENDIF
        IF laData[23] = gcbasecurr
          SHOW GET laData[24] DISABLE
        ELSE
          SHOW GET laData[24] ENABLE
        ENDIF
        IF laData[25] = gcbasecurr
          SHOW GET laData[26] DISABLE
        ENDIF
      ENDIF  
      SHOW GET laData[23]
      SHOW GET laData[17]
      SHOW GET laData[18]
    ENDIF
    *B607276,1 ALB Fix bug changing vendor contact name and telephone when change vendor in edit mode [end]
  ENDIF  
  IF EMPTY(laData[1]) AND LASTKEY() <> 9 AND LASTKEY() <> 15
    lcPObrow  = laData[1]
    lcVenbrow = laData[2]
    =POFBROW(lcKeyType,@lcVenbrow,@lcPObrow)
    laData[1] = lcPObrow
  ENDIF
  IF !EMPTY(laData[1]) AND laScrMode[1]
    laScrMode[1]  = .F.
    laScrMode[2]  = .T.
    SHOW GETS 
  ELSE
    IF laScrMode[1] AND LASTKEY() <> 9 AND LASTKEY() <> 15
      laScrMode[2]  = .F.
      laData[2]     =  SPACE(08)
      _CUROBJ       = OBJNUM(laData[1])
      RETURN
    ENDIF  
  ENDIF
ENDIF  

*B603507, 1 Fix the bug of E.price doesn't correctly shows.[START]
lcFrnSign1 = gfGetExSin("",ALLTRIM(laData[23]))
*B603507, 1 [END]

lcVenName = LOOKUP(ApVendor.cvencomp ,laData[2] ,ApVendor.cvendcode)
lcVEnAdd1=gfGetAdr('ApVendor','','','',1,'')
lcVEnAdd1=lcVEnAdd1+IIF(LEN(lcVEnAdd1)<30,SPACE(30-LEN(lcVEnAdd1)),'')
FOR lnCounter = 1 TO 5
  L=ALLTRIM(STR(lnCounter))
  lcTempVr=''
    FOR lnCount = 1 TO ALEN(laAddress,1)
      IF laAddress[lnCount,1] = lnCounter
        lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
        lcTempVr = lcTempVr+IIF(EMPTY(lcTempVr) .OR. RIGHT(lcTempVr,1) = ',' ,'',', ') + lcAddPart
      ENDIF
    ENDFOR
    lcVEnAdd&L=lcTempVr+IIF(LEN(lcTempVr)<30,SPACE(30-LEN(lcTempVr)),'')
ENDFOR

*-- Case of new P/O get the default term and division of the chosen vendor

*B607276,1 ALB Fix bug changing vendor contact name and telephone when change vendor in edit mode [Begin]
*IF laScrMode[4]
IF laScrMode[4] OR laScrMode[3]
*B607276,1 ALB Fix bug changing vendor contact name and telephone when change vendor in edit mode [end]
  laCodInfo[1,07] = "APVENDOR" 
  laCodInfo[1,08] = "VENCODE"  
  laCodInfo[1,09] = "laData[2]"
  laCodInfo[1,10] = "cTermCode"

  laCodInfo[2,07] = "APVENDOR" 
  laCodInfo[2,08] = "VENCODE"
  laCodInfo[2,09] = "laData[2]" 
  laCodInfo[2,10] = "cDivision"
ELSE
  laCodInfo[1,07] = "POFHDR" 
  laCodInfo[1,08] = "POFHDR"  
  laCodInfo[1,09] = "lcKeyType+laData[1]"
  laCodInfo[1,10] = "cTermCode"

  laCodInfo[2,07] = "POFHDR" 
  laCodInfo[2,08] = "POFHDR"
  laCodInfo[2,09] = "lcKeyType+laData[1]"
  laCodInfo[2,10] = "cDivision"
ENDIF  

=gfwCodePop(@laCodInfo, "CDIVISION", "T")
=gfwCodePop(@laCodInfo, "CTERMCODE", "T")
laData[11] = laDiv[lnDiv,2]
laData[12] = laTerms[lnTerms,2]

SHOW GET laData[2]
*--HDM Go to Location Field[start]
_CUROBJ = OBJNUM(laData[3])
*--HDM Go to Location Field[end]
SELECT (lcPOHdr)
=RLOCK()
GATHER FROM laData FIELDS &lcScFields MEMO    
UNLOCK
SELECT(lcBaseFile)

*--we needn't this in edit mode  AAMER (Start)
*--and change the seek according to removing comp_id from codes
*IF (laScrMode[4] AND !SEEK(gcAct_Comp+'CDIVISION '+APVENDOR.CDIVISION,'CODES')) OR laScrMode[3]
IF laScrMode[4] AND !SEEK('N'+'CDIVISION '+APVENDOR.CDIVISION,'CODES')
*--we needn't this in edit mode  AAMER (End)
  =gfwCodePop(@laCodInfo, "CDIVISION", "D")
  laData[11] = laDiv[lnDiv,2]
ENDIF  
*--we needn't this in edit mode  AAMER (Start)
*--and change the seek according to removing comp_id from codes
*IF (laScrMode[4] AND !SEEK(gcAct_Comp+'CTERMCODE '+APVENDOR.CTERMCODE,'CODES')) OR laScrMode[3]
IF laScrMode[4] AND !SEEK('N'+'CTERMCODE '+APVENDOR.CTERMCODE,'CODES')
*--we needn't this in edit mode  AAMER (End)
  = gfwCodePop(@laCodInfo, "CTERMCODE ", "D")
  laData[12] = laTerms[lnTerms,2]
ENDIF  

IF !EMPTY(laData[20]) AND laData[2] <> lcOldVen
  =gfModalGen('INM36120B36000','DIALOG')
  laData[20] = SPACE(06)
  laData[21] = {}
  SHOW GET laData[20]
  SHOW GET laData[21]
ENDIF
=lfRefresh()
*!*************************************************************
*! Name        : lfvIData2
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the vendor field from the key field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvIData2

laData[2] = PADR('?',8)
=lfvData2()

*!*************************************************************
*! Name        : lfvData3
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the store field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData3

*B604678,1 MHM 208/20/2001 Check if there is any transaction type 2 4 5 
*B604678,1                 then dont let user change location [Start]
IF laScrMode[3] AND SEEK(lcScrType+laData[1],'POfLN')
  SELECT POfLN 
  LOCATE REST WHILE cMatType+PoMat = lcScrType+laData[1] FOR TranCd $ '245'
  IF FOUND()
    *-Cannot change the location since the recieving was done.
    =gfModalGen('TRM34025B34000','DIALOG','location'+'|'+IIF(TranCd='6','Issue','recieving'))
    SELECT POfHDR
    laData[3]=lcOldWare
    RETURN
  ENDIF
ENDIF
*B604678,1 MHM 208/20/2001 Check if there is any transaction type 2 4 5 
*B604678,1                 then dont let user change vendor[End]

IF !SEEK( laData[3],  'WareHous' )
 laData[3] = gfBrowWare( .T. )
 IF EMPTY(laData[3])
   IF EMPTY(lcOldWare)
     =gfModalGen('INM36001B36000','DIALOG')
     _CUROBJ = OBJNUM(laData[3])
     RETURN
   ELSE
     laData[3] = lcOldWare
   ENDIF  
 ENDIF
ENDIF
IF !EMPTY(laData[3]) AND laData[3] <> lcOldWare
  *--HDM[START]
  SELECT (lcDetTmp)
  GO TOP
  SCAN
    IF !lfFabWar(&lcDetTmp..FABRIC , &lcDetTmp..COLOR , laData[3], .F.)
      laData[3] = lcOldWare
      SHOW GET laData[3]
      llAsk = .T.
      RETURN
    ELSE
      lcOldLocat = lcOldWare
    ENDIF
  ENDSCAN
  *--HDM[End]


  =SEEK(laData[3],"WAREHOUS")
  lcWareDesc  = WAREHOUS.cDesc
  laData[4]=gfGetAdr('WareHous','','','',1,'')
  laData[4]=laData[4]+IIF(LEN(laData[4])<30,SPACE(30-LEN(laData[4])),'')
  FOR lnCounter = 1 TO 5
    L=ALLTRIM(STR(lnCounter))
    lcTempVr=''
      FOR lnCount = 1 TO ALEN(laAddress,1)
        IF laAddress[lnCount,1] = lnCounter
          lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
          lcTempVr = lcTempVr+IIF(EMPTY(lcTempVr) .OR. RIGHT(lcTempVr,1) = ',' ,'',', ') + lcAddPart
        ENDIF
      ENDFOR
      laData[3+lnCounter]=lcTempVr+IIF(LEN(lcTempVr)<30,SPACE(30-LEN(lcTempVr)),'')
  ENDFOR
ENDIF
FOR I = 3 TO 8
 SHOW GET laData[I]
ENDFOR 
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
SELECT(lcBaseFile)
=lfRefresh()
*!*************************************************************
*! Name        : lfvIData3
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the store field from the key field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvIData3

laData[3] = PADR('?',6)
=lfvData3()
*!*************************************************************
*! Name        : lfwData22
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : get the old vendor in a variable from the key field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwData22

=MDOWN()
lcOldVen = laData[2]

*!*************************************************************
*! Name        : lfwData2
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : get the old vendor in a variable 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwData2

lcOldVen = laData[2]
*!*************************************************************
*! Name        : lfvData11
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the division field 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData11
PRIVATE lnAlias

lnAlias = SELECT()
laData[11] = laDiv[lnDiv,2]
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfvData12
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the terms field 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData12
PRIVATE lnAlias

lnAlias = SELECT()
laData[12] = laTerms[lnTerms,2]
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
SELECT(lnAlias)
*!*************************************************************
*! Name        : lfvData13
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the ship via field 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData13
PRIVATE lnAlias

lnAlias = SELECT()
laData[13] = laShip[lnShip,2]
SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO    
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfvData9
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the entered date  field 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData9

IF EMPTY(laData[9])
  =gfModalGen('INM36005B36000','DIALOG','entered')
  _CUROBJ = OBJNUM(laData[9])
  laData[9] = ldOldEnt
  RETURN
ENDIF  

IF laData[9] > laData[10]
  =gfModalGen('INM36003B36000','DIALOG')
  _CUROBJ = OBJNUM(laData[9])
  laData[9] = ldOldEnt
ELSE
  lcOldRate = laData[24]
  laData[24] = gfchkrate('lnUnit1',laData[23],laData[9],llEditExRt,gcAct_Comp,.T.) 
  IF laData[24] <= 0 
      IF laScrMode[3] 
        IF laData[9] <> ldOldEnt
          =gfDialog ("I","No valid rate for this date.")
          _CUROBJ   = OBJNUM(laData[9])  
          laData[9] = ldOldEnt
          laData[24] = lcOldRate
          RETURN
        ENDIF  
      ELSE
        IF gfModalGen('INM36004B36001','DIALOG','price') = 1
          laData[23] = gcbasecurr
          laData[24] = 1
         ELSE
          _CUROBJ   = OBJNUM(laData[9])  
          laData[9] = ldOldEnt
          RETURN
         ENDIF 
      ENDIF   
   ENDIF
   lcOldRate = laData[26]
   laData[26] = gfchkrate('lnUnit2',laData[25],laData[9],llEditExRt,gcAct_Comp,.F.) 
    IF laData[26] <= 0
      IF laScrMode[3] 
        IF laData[9] <> ldOldEnt
          =gfDialog ("I","No valid rate for this date.")
          _CUROBJ   = OBJNUM(laData[9])  
          laData[9] = ldOldEnt
          laData[26] = lcOldRate
          RETURN
        ENDIF  
      ELSE  
        IF gfModalGen('INM36004B36001','DIALOG','duty') = 1
          laData[25] = gcbasecurr
          laData[26] = 1
        ELSE
          _CUROBJ   = OBJNUM(laData[9])  
          laData[9] = ldOldEnt
          RETURN
       ENDIF 
    ENDIF   
  ENDIF
ENDIF  

*C102780,1 AMH Recalculate the delivry date when change the entered date [Start]
IF llDlivDate .AND. laData[9]<>ldOldEnt
  =lfReCalDlv()
ENDIF
*C102780,1 AMH [End]

SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
IF laData[23] = gcbasecurr
  SHOW GET laData[24] DISABLE
ENDIF
IF laData[25] = gcbasecurr
  SHOW GET laData[26] DISABLE
ENDIF

*!*************************************************************
*! Name        : lfvData10
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the complete date field 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData10

IF EMPTY(laData[10])
  =gfModalGen('INM36005B36000','DIALOG','complete')
  _CUROBJ = OBJNUM(laData[10])
  laData[10] = ldOldComp
  RETURN
ENDIF  

IF laData[9] > laData[10]
  =gfModalGen('INM36003B36000','DIALOG')
  _CUROBJ = OBJNUM(laData[10])
  laData[10] = ldOldComp
ENDIF  

*C200439,1 ABD - Add new trigger for custom Dar10.  [Begin]
IF lcKeytype = 'P' .AND. ASCAN(laEvntTrig , PADR('REPLCOMP',10)) <> 0
  =gfDoTriger('MAPRCAM',PADR('REPLCOMP',10))
ENDIF     
*C200439,1 ABD - [End]

*!*************************************************************
*! Name        : lfvData23
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the price currency field 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData23

IF !SEEK(laData[23],"SycCurr")
    lcCurr = laData[23]
	IF !gfcurrbrow(@lcCurr)
	  laData[23]= lcOldPCurr
	ELSE  
	  laData[23]= lcCurr 
	ENDIF
ENDIF
lcOldRate = laData[24]
laData[24] = gfchkrate('lnUnit1',laData[23],laData[9],llEditExRt,gcAct_Comp,.T.) 
IF laData[24] <= 0
  IF gfModalGen('INM36004B36001','DIALOG','price') = 1 
    laData[23] = gcbasecurr
    laData[24] = 1
   ELSE
    _CUROBJ   = OBJNUM(laData[23])  
    laData[23]= lcOldPCurr
    RETURN
   ENDIF 
ENDIF
IF !EMPTY(laData[20]) AND laData[23] <> lcOldPCurr
  =gfModalGen('INM36120B36000','DIALOG')
  laData[20] = SPACE(06)
  laData[21] = {}
  SHOW GET laData[20]
  SHOW GET laData[21]
ENDIF

lcFrnSmbl1 = syccurr.ccurrsmbl
lcFrnSign1 = gfGetExSin("",ALLTRIM(laData[23]))
SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
IF laData[23] = gcbasecurr
  SHOW GET laData[24] DISABLE
ELSE  
  SHOW GET laData[24] ENABLE
ENDIF
IF laData[25] = gcbasecurr
  SHOW GET laData[26] DISABLE
ELSE  
  SHOW GET laData[26] ENABLE
ENDIF
IF laData[24] <> lcOldRate
  SELECT(lcDetTmp)
  SCAN
    =RLOCK()
    REPLACE nECost1 WITH IIF(lcFrnSign1='*',ROUND(nCost1*laData[24]/lnUnit1,3),ROUND(nCost1/laData[24]/lnUnit1,3))
    UNLOCK
  ENDSCAN
ENDIF
=RLOCK()
REPLACE &lcPOHdr.. cPriceCur WITH laData[23],;
        &lcPOHdr.. nPriceRat WITH laData[24]
UNLOCK        
*!*************************************************************
*! Name        : lfvData25
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the duty currency field 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData25

IF !SEEK(laData[25],"SycCurr")
    lcCurr = laData[25]
	IF !gfcurrbrow(@lcCurr)
	  laData[25]= lcOldDCurr
	ELSE  
	  laData[25]= lcCurr 
	ENDIF
ENDIF
lcOldRate = laData[26]
laData[26] = gfchkrate('lnUnit2',laData[25],laData[9],llEditExRt,gcAct_Comp,.T.)
IF laData[26] <= 0
  IF gfModalGen('INM36004B36001','DIALOG','Duty') = 1 
    laData[25] = gcbasecurr
    laData[26] = 1
   ELSE
    _CUROBJ   = OBJNUM(laData[25])
    laData[25]= lcOldDCurr
    RETURN
   ENDIF 
ENDIF
IF !EMPTY(laData[20]) AND laData[25] <> lcOldDCurr
  =gfModalGen('INM36120B36000','DIALOG')
  laData[20] = SPACE(06)
  laData[21] = {}
  SHOW GET laData[20]
  SHOW GET laData[21]
ENDIF

lcFrnSmbl2 = syccurr.ccurrsmbl
lcFrnSign2 = gfGetExSin("",ALLTRIM(laData[25]))
SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
IF laData[25] = gcbasecurr
  SHOW GET laData[26] DISABLE
ELSE  
  SHOW GET laData[26] ENABLE
ENDIF
IF laData[26] <> lcOldRate
  SELECT(lcDetTmp)
  SCAN
    =RLOCK()
    REPLACE nECost2 WITH IIF(lcFrnSign2='*',ROUND(nCost2*laData[26]/lnUnit2,3),ROUND(nCost2/laData[26]/lnUnit2,3)),;
            nECost3 WITH IIF(lcFrnSign2='*',ROUND(nCost3*laData[26]/lnUnit2,3),ROUND(nCost3/laData[26]/lnUnit2,3)),;
            nECost4 WITH IIF(lcFrnSign2='*',ROUND(nCost4*laData[26]/lnUnit2,3),ROUND(nCost4/laData[26]/lnUnit2,3))
    UNLOCK        
  ENDSCAN
ENDIF
=RLOCK()
REPLACE &lcPOHdr.. cDutyCur WITH laData[25],;
        &lcPOHdr.. nDutyRat WITH laData[26]
UNLOCK        
*!*************************************************************
*! Name        : lfwData23
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : get the old value of the price currency in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwData23

lcOldPCurr = laData[23]

*!*************************************************************
*! Name        : lfwData23
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : get the old value of the duty currency in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwData25

lcOldDCurr = laData[25]

*!*************************************************************
*! Name        : lfwData3
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : get the old value of the store in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwData3

lcOldWare = laData[3]
*!*************************************************************
*! Name        : lfwData32
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : get the old value of the store in a variable from the key field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwData32

=MDOWN()
lcOldWare = laData[3]

*!*************************************************************
*! Name        : lpDelScr
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : cancel or uncancel P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
PROCEDURE lpDelScr

SELECT (lcPOHdr)
GATHER FROM laData FIELDS &lcScFields MEMO

IF !llContinue
  =lfCrUnSess('pbDlt')
ELSE
  SELECT(lcPOHDR)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK  
ENDIF
DO CASE
  CASE lcStatus  = 'Open' AND lcPromp = lcCancel AND llApIstall;
                          AND SEEK('L'+laData[1],'APINVTKT') 
    =gfModalGen('INM36114B36000','DIALOG',lcPrgtype+SPACE(01)+laData[1])
    =SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession,'UNCMSESS')
    SELECT UNCMSESS
    =RLOCK()
    REPLACE Status WITH 'C'
    UNLOCK
    SELECT (lcBaseFile)
    =lfRefresh()
    RETURN

  CASE lcStatus  = 'Cancelled' AND lcPromp = lcUnCan AND laData[54] > 0
      llNothing  = IIF(lcKeyType <> 'C',lfCanFab(.F.),.T.)
      IF lcKeyType <> 'C'
        =lfVenCan(.F.)
      ENDIF  
      lnCanQty   = lfCanLine(.F.)
      ladata[55] = ladata[55] +lnCanQty
      ladata[54] = ladata[54] - lnCanQty      
      lnStatus   = 1
      lcStatus   = 'Open'
      lcPromp    = lcCancel
      REPLACE POFHDR.STATUS     WITH 'O',;
              POFHDR.npo_open   WITH laData[55],;
              POFHDR.nfabcancel WITH laData[54]
  CASE lcStatus  = 'Cancelled' AND lcPromp = lcUnCan AND laData[54] = 0              
      lnCanQty   = lfCanLine(.F.) 
      ladata[55] = 0
      ladata[54] = 0
      lnStatus   = IIF(lcKeyType = 'C',5,2)
      lcStatus   = IIF(lcKeyType = 'C','Hold','Bid')
      lcPromp    = lcCancel
      REPLACE POFHDR.STATUS     WITH IIF(lcKeyType = 'C','H','B'),;
              POFHDR.npo_open   WITH laData[55],;
              POFHDR.nfabcancel WITH laData[54]
              
  CASE lcStatus = 'Complete' AND lcPromp = lcCancel
    =gfModalGen('INM36007B36000','DIALOG',lcPrgtype+SPACE(01)+laData[1])
    =SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession,'UNCMSESS')
    SELECT UNCMSESS
    =RLOCK()
    REPLACE Status WITH 'C'
    UNLOCK
    SELECT (lcBaseFile)
    =lfRefresh()
    RETURN
    
  CASE lcStatus = 'Open' AND lcPromp = lcCancel AND ;
                   PofHdr.nfbreceive > 0
      ladata[54]  = ladata[54]  + ladata[55]
      ladata[55]  = 0
      lcStatus = 'Complete' 
      lnStatus = 4
      lcPromp   = lcCancel
      =lfVenCan(.T.)
      *B803242,1 (Begin) The status should be 'Complete' not 'Cancelled'
      *REPLACE POFHDR.STATUS     WITH 'X',;
              POFHDR.npo_open   WITH laData[55],;
              POFHDR.nfabcancel WITH laData[54]
      REPLACE POFHDR.STATUS     WITH 'C',;
              POFHDR.npo_open   WITH laData[55],;
              POFHDR.nfabcancel WITH laData[54]
      *B803242,1 (End)
      =lfCanFab(.T.)        
      =lfCanLine(.T.)        
      SELECT POFLN
      APPEND FROM (gcWorkDir+lcDetTmp) for trancd = '4' AND lCancel
  OTHERWISE   
  
      *C200149,1  ABD 02/20/2001 Add new triger. [Begin]
      IF ASCAN(laEvntTrig , PADR('DLEALOMA',10)) <> 0
       lcParam = 'A'
       llReturn  =gfDoTriger('MAPRCAM',PADR('DLEALOMA',10))
       IF !llReturn
         RETURN 
       ENDIF
      ENDIF  
      *C200149,1  ABD [End]

      ladata[54]  = ladata[54]  + ladata[55]
      ladata[55]  = 0
      lcStatus    = 'Cancelled' 
      lnStatus    = 3
      lcPromp     = lcUnCan
      IF lcKeyType <> 'C' 
        =lfVenCan(.T.)
      ENDIF
        REPLACE POFHDR.STATUS     WITH 'X',;
                POFHDR.npo_open   WITH laData[55],;
                POFHDR.nfabcancel WITH laData[54]
        llNoting = IIF(lcKeyType <> 'C',lfCanFab(.T.),.T.)
        =lfCanLine(.T.)        
        SELECT POFLN
        APPEND FROM (gcWorkDir+lcDetTmp) for trancd = '4' AND lCancel
ENDCASE
=SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession,'UNCMSESS')
SELECT UNCMSESS
=RLOCK()
REPLACE Status WITH 'C'
UNLOCK
SELECT (lcBaseFile)
=lfRefresh()
*!*************************************************************
*! Name        : lfvNewPL
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : when adding line for the P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvNewPL

*--HDM B602197,1 11/17/1998 [START]
*--Check If The Warehouse field is empty
*--Return to it and don't add new line
IF EMPTY(laData[3])
     =gfModalGen('INM36001B36000','DIALOG')
     _CUROBJ = OBJNUM(laData[3])
     RETURN
ENDIF
*--HDM B602197,1 11/17/1998 [END]

*C102780,1 AMH Ckeck if the vendor field is empty in case of use vender referance [Start]
IF llVenRef .AND. EMPTY(laData[2])
  =gfModalGen('INM36195B36000','DIALOG')
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF
*C102780,1 AMH [End]

lcFabric  = SPACE(07)
lcColor   = SPACE(06)

*C102780,1 AMH Update the vendor item, vendor color and delivry date [Start]
IF llVenRef
  lcVenFab = SPACE(10)
  lcVenColr = SPACE(10)
ENDIF
IF llDlivDate
  ldDelvDate = {  /  /    }
ENDIF
*C102780,1 AMH [End]

lcIDesc   = SPACE(20) 
lcClrDesc = SPACE(15) 
lcPattern = SPACE(10) 
lcWidth   = SPACE(06) 
lnCost1   = 0
lnCost2   = 0
lnCost3   = 0
lnCost4   = 0
lnQty     = 0
lnTotQty  = 0

SHOW GET lcFabric  ENABLE
SHOW GET lcColor   ENABLE
SHOW GET ibFabric  ENABLE
SHOW GET ibColor   ENABLE

*C102780,1 AMH Show the vendor item, vendor color and delivry date [Start]
IF llVenRef
  SHOW GET lcVenFab ENABLE
  SHOW GET ibVenFab ENABLE
  SHOW GET lcVenColr ENABLE
  SHOW GET ibVenColr ENABLE
ENDIF
IF llDlivDate
  SHOW GET ldDelvDate ENABLE
ENDIF
*C102780,1 AMH [End]

SHOW GET lcPattern DISABLE
SHOW GET lcWidth   DISABLE
SHOW GET lnCost1   DISABLE
SHOW GET lnCost2   DISABLE
SHOW GET lnCost3   DISABLE
SHOW GET lnCost4   DISABLE
SHOW GET lnQty     DISABLE

lnQty    = 0
lnCost1  = 0
lnCost2  = 0
lnCost3  = 0
lnCost4  = 0
lNewLine = .T.
=lfRefresh()
_CUROBJ = OBJNUM(lcFabric)

*!*************************************************************
*! Name        : lfvFabric
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the fabric field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT()
SELECT Fabric
SET FILTER TO CFABGRADE = lcQuality
IF !EMPTY(lcFabric) AND !SEEK(lcFabric)
    *--MHM 604302,1
    llBrowse = .F.
    *--MHM 604302,1
    DO FaBrow WITH lcFabric,'*'
    *B038431,1 MHM 08/23/2004 Restore old value [Start]
    IF EMPTY(lcFabric)
      lcFabric = lcOldFab
    ENDIF
    *B038431,1 MHM  [End]
ENDIF
SET FILTER TO

*C102780,1 AMH Get the vendor referance [Start]
IF llVenRef .AND. lcFabric <> lcOldFab
  SELECT VENDMATL
  lcOrder = SET('ORDER')
  SET ORDER TO TAG MATCOL
  IF SEEK(laData[2]+lcFabric)
    lcVenFab = CVENFAB
  ELSE
    lcVenFab = SPACE(10)
  ENDIF
  SHOW GET lcVenFab
  SET ORDER TO &lcOrder.
ENDIF
*C102780,1 AMH [End]

SHOW GET lcFabric ENABLE
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfvColor
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the color field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvColor
PRIVATE lnAlias

IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT()
SELECT Fabric
SET ORDER TO TAG FABRIC
IF !EMPTY(lcColor) AND !SEEK(lcFabric+lcColor)
  lcColor  = IIF(EMPTY(lcFabric),PADR(lcColor,6),CHR(240))
  lcOldF   = lcFabric
  SELECT FABRIC
  SET FILTER TO CFABGRADE = lcQuality
  DO FaBrow WITH lcFabric,lcColor

  *B038431,1 MHM 08/23/2004 Restore old value [Start]
  IF EMPTY(lcColor)
    lcColor= lcOldClr
    lcFabric = lcOldFab
  ENDIF
  *B038431,1 MHM [End]

  SET FILTER TO
  IF EMPTY(lcColor)
    lcFabric = lcOldF
    _CUROBJ = OBJNUM(lcColor)
    SELECT(lnAlias)
    RETURN
  ENDIF
ENDIF

IF !EMPTY(lcFabric) AND !EMPTY(lcColor) AND ;
   ((lcFabric <> lcOldFab) OR (lcColor <> lcOldClr))
  IF Fabric.Make .AND. gfModalGen('INM36009B36000','DIALOG')=1
    IF lNewLine
      lNewLine = .F.
    ENDIF     
    IF !EOF(lcDetTmp)
      _CUROBJ = OBJNUM(lcFabric)
    ELSE
      lcFabric = SPACE(07)
      lcColor  = SPACE(06)
      
      *C102780,1 AMH Update the vendor referance [Start]
      IF llVenRef
        lcVenFab  = SPACE(10)
        lcVenColr = SPACE(10)
      ENDIF
      *C102780,1 AMH [End]
      
    ENDIF

    =lfwLine()
  	SELECT(lnAlias)
    RETURN
  ENDIF
  IF lcFabric <> lcOldFab OR lcColor <> lcOldClr
    IF SEEK(lcKeyType+laData[1]+lcOldFab+lcOldClr+'2','POFLN') .AND. gfModalGen('INM36010B36000','DIALOG','Shipment') = 1
      lcFabric = lcOldFab 
      lcColor  = lcOldClr
      
      *C102780,1 AMH Update the vendor referance [Start]
      IF llVenRef
        lcVenFab  = lcOldVFab
        lcVenColr = lcOldVColr
      ENDIF
      *C102780,1 AMH [End]
      
      =lfwLine()
      _CUROBJ = OBJNUM(lcFabric)
  	  SELECT(lnAlias)
      RETURN
    ENDIF
    IF SEEK(lcKeyType+laData[1]+lcOldFab+lcOldClr+'3','POFLN') .AND. gfModalGen('INM36010B36000','DIALOG','Damaged') = 1
      lcFabric = lcOldFab 
      lcColor  = lcOldClr
      
      *C102780,1 AMH Update the vendor referance [Start]
      IF llVenRef
        lcVenFab  = lcOldVFab
        lcVenColr = lcOldVColr
      ENDIF
      *C102780,1 AMH [End]
      
      =lfwLine()
      _CUROBJ = OBJNUM(lcFabric)
  	  SELECT(lnAlias)
      RETURN
    ENDIF
    IF SEEK(lcKeyType+laData[1]+lcOldFab+lcOldClr+'4','POFLN') .AND. gfModalGen('INM36011B36000','DIALOG','Cancelled ') = 1
      lcFabric = lcOldFab 
      lcColor  = lcOldClr
      
      *C102780,1 AMH Update the vendor referance [Start]
      IF llVenRef
        lcVenFab  = lcOldVFab
        lcVenColr = lcOldVColr
      ENDIF
      *C102780,1 AMH [End]
      
      =lfwLine()
      _CUROBJ = OBJNUM(lcFabric)
  	  SELECT(lnAlias)
      RETURN
    ENDIF
  ENDIF
  SELECT(lcDetTmp)
  IF SEEK(lcKeyType+laData[1]+lcFabric+lcColor)
    =gfModalGen('INM36012B36000','DIALOG',lcDialog)
    lcFabric = SPACE(07) 
    lccOLOR  = SPACE(06) 
    
    *C102780,1 AMH Update the vendor referance [Start]
    IF llVenRef
      lcVenFab  = SPACE(10)
      lcVenColr = SPACE(10)
    ENDIF
    *C102780,1 AMH [End]
    
    lNewLine = .F.
    =lfwLine()
    _CUROBJ = OBJNUM(lcFabric)
    SELECT(lnAlias)
    RETURN
  ENDIF
  *--B602197,1 HDM Adding warehouse if not found [start]
  IF !SEEK(lcFabric+lcColor+ladata[3],'FABDYE')
    IF !lfFabInWar(lcFabric , lcColor , laData[3])
      lcFabric = SPACE(07) 
      lccOLOR  = SPACE(06) 
      
      *C102780,1 AMH Update the vendor referance [Start]
      IF llVenRef
        lcVenFab  = SPACE(10)
        lcVenColr = SPACE(10)
      ENDIF
      *C102780,1 AMH [End]
      
      lNewLine = .F.
      =lfwLine()
      _CUROBJ = OBJNUM(lcFabric)
      SELECT(lnAlias)
      RETURN
    ENDIF
  ENDIF
  *--HDM B602197,1 HDM Adding warehouse if not found [end]

  *C102780,1 AMH Get the vendor referance & Delievery Date [Start]
  IF llVenRef
    SELECT VENDMATL
    lcOrder = SET('ORDER')
    SET ORDER TO TAG MATCOL
    IF SEEK(laData[2]+lcFabric+lcColor)
      lcVenFab  = CVENFAB
      lcVenColr = CVENCOLR
    ELSE
      lcVenColr = SPACE(10)
    ENDIF
    SET ORDER TO &lcOrder.
    SELECT(lcDetTmp)
  ENDIF
  IF llDlivDate
    IF lNewLine
      IF llVenRef .AND. !EOF('VENDMATL') .AND. !EMPTY(VENDMATL.LEADTIME)
        ldDelvDate = laData[9] + VENDMATL.LEADTIME
      ELSE
        IF !EMPTY(FABRIC.LEADTIME)
          ldDelvDate = laData[9] + FABRIC.LEADTIME
        ELSE
          ldDelvDate = laData[10]
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  *C102780,1 AMH [End]
  
  IF lNewLine
    APPEND BLANK
    SHOW GET lcFabric  ENABLE
    SHOW GET lcColor   ENABLE
    
    *C102780,1 AMH Update the vendor referance [Start]
    IF llVenRef
      SHOW GET lcVenFab
      SHOW GET lcVenColr
    ENDIF
    *C102780,1 AMH [End]
    
    SHOW GET lcPattern ENABLE
    SHOW GET lcWidth   ENABLE
    SHOW GET lnCost1   ENABLE
    SHOW GET lnQty     ENABLE
    IF llEdCost
      SHOW GET lnCost2 ENABLE
      SHOW GET lnCost3 ENABLE
      SHOW GET lnCost4 ENABLE
    ELSE
      SHOW GET lnCost2 DISABLE
      SHOW GET lnCost3 DISABLE
      SHOW GET lnCost4 DISABLE
    ENDIF
    lnLastLine = lnLastLine + 1 
    lnNewRec = RECNO()
  ELSE  
    GOTO lnNewRec 
  ENDIF
  =RLOCK()
  REPLACE FABRIC  WITH lcFabric   ,;
          COLOR   WITH lcColor    ,;
          POMAT   WITH laData[1]  ,;
          LineNo  WITH lnLastLine ,;
          TRANCD  WITH '1'        ,;
          LNEW    WITH lNewLine   ,;
          cMatType WITH lcKeyType  ,; 
          cStatus WITH IIF(cStatus ='A',cStatus ,IIF(EMPTY(cStatus),'A','M'))
  
  *C102780,1 AMH Save the vendor referance and delievery date [Start]
  IF llVenRef
    REPLACE CVENFAB  WITH lcVenFab ,;
            CVENCOLR WITH lcVenColr
  ENDIF
  IF llDlivDate
    REPLACE DDELIVDATE WITH ldDelvDate
  ENDIF
  *-- save the vendor and warehouse fields.
  REPLACE VENDOR    WITH laData[2],;
          CWARECODE WITH laData[3]
  *C102780,1 AMH [End]
  
  UNLOCK
  =lfwLine()
  SHOW GET pbRemove ENABLE
  =SEEK(lcFabric+lcColor,'FABRIC')        
  lcPattern = IIF(lNewLine ,FABRIC.PATTERN,&lcDetTmp..PATTERN)
  lcWidth   = IIF(lNewLine ,FABRIC.WIDTH,&lcDetTmp..WIDTH)
  lcIDesc   = FABRIC.DESC
  lcClrDesc = gfCodDes(lcColor, 'COLOR')

  *E500442,1 ABD - get the price from the pofhdr that used with anther name. [Begin]
  *IF lcKeyType = 'P' AND lNewLine AND SEEK(lcFabric+lcColor,lcContLin)
  *IF &lcContLin..cPriceCur = laData[23]
  *lnCost1 = &lcContLin..nCost1
  IF lcKeyType = 'P' AND lNewLine .AND. lfvline()
    IF &lcContHdr..cPriceCur = laData[23]
     lnCost1 = &lcContlin..nCost1
     *E500442,1 ABD - [End]
    ENDIF
    
    *E500442,1 ABD - get the Duty Curr from the pofhdr that used with anther name. [Begin]
    *IF &lcContLin..cDutyCur = laData[25]
    IF &lcContHdr..cDutyCur = laData[25]
      *E500442,1 ABD - [End]
      lnCost2 = &lcContLin..nCost2
      lnCost3 = &lcContLin..nCost3
      lnCost4 = &lcContLin..nCost4
    ENDIF
  ELSE
    
    *C102780,1 AMH Get the price/quantity for vendor referance [Start]
    *lnCost1 = IIF(lNewLine,IIF(laData[23] = FABRIC.cPriceCur,FABRIC.nFabCost,0),&lcDetTmp..nCost1)
    IF lNewLine
      IF llVenRef .AND. !EOF('VENDMATL')

        *B122673,1 NNA 05/15/2004 (Begin) Replace the Price Field with the new name (NFABCOST)
        *lnCost1   = VENDMATL.PRICE
        lnCost1   = VENDMATL.NFABCOST
        *B122673,1 NNA (End)

        lnQty     = VENDMATL.NFABTOTQTY
        lnOldQty  = 0
        =lfvQty()
      ELSE
        IF laData[23] = FABRIC.cPriceCur
          lnCost1 = FABRIC.nFabCost
        ELSE
          lnCost1 = 0
        ENDIF
      ENDIF
    ELSE
      lnCost1 = &lcDetTmp..nCost1
    ENDIF
    *C102780,1 AMH [End]
    
    lnCost2 = IIF(lNewLine,IIF(laData[25] = FABRIC.cDutyCur,FABRIC.nItm_frt,0),&lcDetTmp..nCost2)
    lnCost3 = IIF(lNewLine,IIF(laData[25] = FABRIC.cDutyCur,FABRIC.nItem_tax,0),&lcDetTmp..nCost3)
    lnCost4 = IIF(lNewLine,IIF(laData[25] = FABRIC.cDutyCur,FABRIC.nItemquota,0),&lcDetTmp..nCost4)
  ENDIF
  =RLOCK()
  REPLACE nCost1   WITH lnCost1,;
          nCost2   WITH lnCost2,;
          nCost3   WITH lnCost3,;
          nCost4   WITH lnCost4,;
          neCost1  WITH IIF(lcFrnSign1='*',ROUND(lnCost1*laData[24]/lnUnit1,3),ROUND(lnCost1/laData[24]/lnUnit1,3)),;
          neCost2  WITH IIF(lcFrnSign2='*',ROUND(lnCost2*laData[26]/lnUnit2,3),ROUND(lnCost2/laData[26]/lnUnit2,3)),;
          neCost3  WITH IIF(lcFrnSign2='*',ROUND(lnCost3*laData[26]/lnUnit2,3),ROUND(lnCost3/laData[26]/lnUnit2,3)),;
          neCost4  WITH IIF(lcFrnSign2='*',ROUND(lnCost4*laData[26]/lnUnit2,3),ROUND(lnCost4/laData[26]/lnUnit2,3)),;
          PATTERN  WITH lcPattern,;
          WIDTH    WITH lcWidth    
  UNLOCK        
ELSE
  
  *C102780,1 AMH Consider case of vendor referance [Start]
  *IF LASTKEY() <> 15 AND EMPTY(lcFabric) AND EMPTY(lcColor)
  IF LASTKEY() <> 15 AND EMPTY(lcFabric) AND EMPTY(lcColor) AND !llVenRef
  *C102780,1 AMH [End]
  
    =gfModalGen('INM36013B36000','DIALOG')
    _CUROBJ = IIF(EMPTY(lcFabric),OBJNUM(lcFabric),OBJNUM(lcColor))    
    RETURN
  ELSE  
    RETURN
  ENDIF  
ENDIF
SHOW GET lcFabric ENABLE
SHOW GET lcColor  ENABLE
lNewLine = .F.
=lfRefresh()
SHOW GETS WINDOW (lcWinC32) ENABLE ONLY

*E500442,1 ABD - Call function to valid the fabric - Color & Price. [Begin]
*IF lcKeytype = 'P' AND SEEK(lcFabric+lcColor,lcContLin) AND &lcContLin..CPRICECUR = laData[23]
IF lcKeytype = 'P' .AND. lfvline('CPRICECUR')
  *E500442,1 ABD - [End]
  SHOW GET lnCost1 DISABLE
ENDIF

*C200439,1 ABD - Add new trigger for custom Dar10.  [Begin]
IF lcKeytype = 'P' .AND. ASCAN(laEvntTrig , PADR('UPDLINED',10)) <> 0
  =gfDoTriger('MAPRCAM',PADR('UPDLINED',10))
ENDIF     
*C200439,1 ABD - [End]


*E500442,1 ABD - Call function to valid the fabric - Color & Duty Curr. [Begin]
*IF !llEdCost OR(lcKeytype = 'P' AND SEEK(lcFabric+lcColor,lcContLin) AND &lcContLin..CDUTYCUR = laData[25])
IF !llEdCost OR(lcKeytype = 'P' .AND. lfvline('CDUTYCUR'))
  *E500442,1 ABD - [End]
  SHOW GET lnCost2 DISABLE
  SHOW GET lnCost3 DISABLE
  SHOW GET lnCost4 DISABLE
ENDIF
SHOW WINDOW (lcDet_Ttl) REFRESH SAME
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfvKFabric
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the fabric field from the key field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvKFabric

lcFabric = PADR('?',7)
=lfvFabric()
_CUROBJ = OBJNUM(lcColor)

*!*************************************************************
*! Name        : lfvKColor
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the color field from the key field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvKColor

lcColor = PADR('?',6)
=lfvColor()

*!*************************************************************
*! Name        : lfvData26
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the duty rate field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData26

lnAlias = SELECT()
IF laData[26] <= 0
  =gfModalGen('INM36014B36000','DIALOG','Rate')
  laData[26] = lcOldRate
  _CUROBJ = OBJNUM(laData[26])
  RETURN
ENDIF
IF laData[26] <> lcOldRate
  SELECT(lcDetTmp)
  SCAN
    =RLOCK()
    REPLACE nECost2 WITH IIF(lcFrnSign2='*',ROUND(nCost2*laData[26]/lnUnit2,3),ROUND(nCost2/laData[26]/lnUnit2,3)),;
            nECost3 WITH IIF(lcFrnSign2='*',ROUND(nCost3*laData[26]/lnUnit2,3),ROUND(nCost3/laData[26]/lnUnit2,3)),;
            nECost4 WITH IIF(lcFrnSign2='*',ROUND(nCost4*laData[26]/lnUnit2,3),ROUND(nCost4/laData[26]/lnUnit2,3))
    UNLOCK 
  ENDSCAN
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfwData26
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old duty rate in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwData26

lcOldRate = laData[26] 
*!*************************************************************
*! Name        : lfwData10
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old complete date in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwData10

ldOldComp = laData[10]

*!*************************************************************
*! Name        : lfwData9
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old entered date in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwData9

ldOldEnt = laData[9]

*!*************************************************************
*! Name        : lfvData24
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the price rate field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvData24

lnAlias = SELECT()
IF laData[24] <= 0
  =gfModalGen('INM36014B36000','DIALOG','Rate')
  laData[24] = lcOldRate
  _CUROBJ = OBJNUM(laData[24])
ENDIF
IF laData[24] <> lcOldRate
  SELECT(lcDetTmp)
  SCAN
    =RLOCK()
    REPLACE nECost1 WITH IIF(lcFrnSign1='*',ROUND(nCost1*laData[24]/lnUnit1,3),ROUND(nCost1/laData[24]/lnUnit1,3))
    UNLOCK
  ENDSCAN
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfwData24
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old price rate value in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwData24

lcOldRate = laData[24] 

*!*************************************************************
*! Name        : lpSavScr
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : save the modification or a new record 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
PROCEDURE lpSavScr

lnAlias = SELECT()
IF laScrMode[4] AND laData[51] = 0
  =gfModalGen('INM36101B36000','DIALOG',IIF(lcKeyType = 'P','PO',IIF(lcKeyType = 'R','Return PO','Contract')))
  lnlastfold = lnactfolder
  lnactfolder  = 2
  =lfchngfolder(lnactfolder)
  llCSave = .F.
  RETURN
ENDIF
IF EMPTY(laData[2])
  =gfModalGen('INM36002B36000','DIALOG','Vendor')
  lnlastfold = lnactfolder
  lnactfolder  = 1
  =lfchngfolder(lnactfolder)
  _CUROBJ = OBJNUM(laData[2])
  llCSave = .F.
  RETURN
ENDIF
IF llMultiWH AND EMPTY(laData[3])
  =gfModalGen('INM36002B36000','DIALOG','Warehouse')
  lnlastfold = lnactfolder
  lnactfolder  = 1
  =lfchngfolder(lnactfolder)
  _CUROBJ = OBJNUM(laData[3])
  llCSave = .F.
  RETURN
ENDIF
IF EMPTY(laData[11])
  laData[11] = laDiv[lnDiv,2]
ENDIF  
IF EMPTY(laData[12])
  laData[12] = laTerms[lnTerms,2]
ENDIF
IF EMPTY(laData[13])
  laData[13] = laShip[lnShip,2]
ENDIF

SELECT (lcDetTmp)
SCAN
  IF nFabTotQty = 0
    IF gfModalGen('INM36102B36007','DIALOG') = 2
      lnlastfold = lnactfolder
      lnactfolder  = 2
      =lfchngfolder(lnactfolder)
      llCSave = .F.
      RETURN
    ENDIF
    EXIT
  ENDIF
ENDSCAN
DELETE ALL FOR nFabTotQty = 0 AND cStatus <> 'S'
GOTO TOP
IF EOF()
  =gfModalGen('INM36015B36000','DIALOG')
  lnlastfold = lnactfolder
  lnactfolder  = 2
  =lfchngfolder(lnactfolder)
  _CUROBJ = OBJNUM(pbNew)
  llCSave = .F.
  RETURN
ENDIF

IF llSavAct AND lcMtCstMth $ 'AS'
    IF !CHECKPRD(gdSysDate,'lcGLFYear','lcGLPeriod','PO',.T.)
      IF gfModalGen('INM36103B36008','DIALOG') = 2
        llCSave = .F.
        RETURN
      ELSE
        llSavAct = .F.
      ENDIF
  ENDIF 
ENDIF

=SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession,'UNCMSESS')
SELECT UNCMSESS
REPLACE cCurrObj WITH 'pbSav'

IF laScrMode[4]
  *E500349,1 - WAB (START) - call the lfMaPoNum() in case of manual po no 
  *laData[1]=gfSEQUENCE('POMAT')
  
  *E500442,5 ABD - Remove the filter from the POFHDR but 
  *E500442,5 ABD - Before that we will save the old filter to return the file
  *E500442,5 ABD - to the old filter after get the new PO number. [Begin]
  PRIVATE lnOldAls , lcOldFltr
  lnOldAls = SELECT (0)
  SELECT POFHDR
  lcOldFltr = FILTER()
  SET FILTER TO
  SELECT (lnOldAls)
  *E500442,5 ABD - [End]
  
  laData[1] = IIF(llGenMaPoN,gfSequence('POMAT'),lfMaPoNum())

  *E500442,5 ABD - return the POFHR to the old filter. [Begin]
  lnOldAls = SELECT (0)
  SELECT POFHDR
  SET FILTE TO &lcOldFltr
  SELECT (lnOldAls)
  *E500442,5 ABD - [End]
  
  *E500349,1 - WAB (END) 
ENDIF

IF lcKeyType <> 'C' AND lnStatus <> 2 
  =lfUpdFab()
  =lfUpdVend()
ENDIF

*C102780,1 AMH Save the revisions Data [Start]
=lfUpdRev()
*C102780,1 AMH [End]

=lfUpdLines()
=lfUpdHdr()

*--B602400,1 HDM [START] Check warehouse record
SELECT (lcDetTmp)
GO TOP
SCAN
  =lfFabWar(&lcDetTmp..FABRIC , &lcDetTmp..COLOR , laData[3], .T.)
ENDSCAN
llAsk = .T.
*--B602400,1 HDM[END]

*C200200,1  MHM 06/19/2001 [Start]
IF ASCAN(laEvntTrig , PADR('SAVSCR',10)) <> 0
  =gfDoTriger('MAPRCAM',PADR('SAVSCR',10))
ENDIF     
*C200200,1  MHM 06/19/2001 [End]

IF llSavAct
  DO CASE
    CASE lcMtCstMth $ 'AS'
      =lfActCst('1')
    CASE lcMtCstMth $ 'FLI' 
      =lfActCst('2')
  ENDCASE    
ENDIF
=SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession,'UNCMSESS')
SELECT UNCMSESS
REPLACE Status WITH 'C'
IF laScrMode[4]
  SHOW GET laData[1]
  lcMsgTxt=IIF(lckeyType='C','Contract',IIF(lckeyType='R','Return purchase order','Purchase order'))
  =gfModalGen('INM36085B36000','DIALOG',lcMsgTxt+'|'+laData[1])
ENDIF

*--B602753,1 HDM 04/07/1999[Start] Stop Calling NotePad Program In lpSavScr as the global save
*--B602753,1 HDM 04/07/1999        Will Call it
*IF laScrMode[4] AND gfModalGen('INM00271B00006','DIALOG') = 1
*  =NotePad("M",lcKeyType+laData[1])
*ENDIF
*--B602753,1 HDM 04/07/1999[End]

laScrMode[2] = .T.
laScrMode[1] = .F.
SHOW GET ibFolder[3] ENABLE
lnlastfold  = lnactfolder
lnactfolder = 1
=lfchngfolder(lnactfolder)
SELECT(lcBaseFile)

*!*************************************************************
*! Name        : lfActPad
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : activate a new pad with the option
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Append lines from P/O'   SKIP FOR (lcKeyType <> 'R') OR (lnactfolder <> 2) OR EMPTY(laData[2]) OR laScrMode[1] OR laScrMode[2]

*B605812,1 ABD - show the edit cost per line in the detail folder only. [Begin]
*DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Edit cost per line'      SKIP FOR (lnActfolder = 2) AND (laScrMode[3] OR laScrMode[1] OR laScrMode[4])
DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Edit cost per line'      SKIP FOR !((lnActfolder = 2) AND (laScrMode[3] OR laScrMode[1] OR laScrMode[4]))
*B605812,1 ABD - [End]

DEFINE BAR 3 OF _OPTIONPOP PROMPT 'Edit cost for header'    SKIP FOR (lnactfolder=3) OR !laScrMode[4] OR RECCOUNT(lcDetTmp) = 0
DEFINE BAR 4 OF _OPTIONPOP PROMPT 'Edit actual cost'        SKIP FOR (laData[52] = 0 AND laData[53] = 0) OR llApIstall OR !laScrMode[3] OR LNACTFOLDER <> 3

*B802047 AHM (Start)
DEFINE BAR 5 OF _OPTIONPOP PROMPT 'Show receiving lines'    SKIP FOR !(lcKeyType='P' AND lnActFolder=2)
*B802047 AHM (End)

*C102780,1 AMH Add new bar to the PO Revision screen [Start]
DEFINE BAR 6 OF _OPTIONPOP PROMPT 'PO Revision'             SKIP FOR lcKeyType<>'P' .OR. laScrMode[1]
*C102780,1 AMH [End]

*C200200,1  MHM 06/19/2001 [Start]
IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
  =gfDoTriger('MAPRCAM',PADR('ADDOPTN',10))
ENDIF     
*C200200,1  MHM 06/19/2001 [End]

*C200439,1 ABD - Add new trigger for custom Dar10.  [Begin]
IF lcKeytype = 'P' .AND. ASCAN(laEvntTrig , PADR('LINEDATE',10)) <> 0
  =gfDoTriger('MAPRCAM',PADR('LINEDATE',10))
ENDIF     
*C200439,1 ABD - [End]


ON SELECTION POPUP _OPTIONPOP DO lpOptions WITH BAR()
ON KEY LABEL ALT+P ACTIVATE POPUP _OPTIONPOP
*!*************************************************************
*! Name        : lpOptions
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : define the actions done when selecting one of the option bars
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
PROCEDURE lpOptions
PARAMETERS lnBar

DO CASE
  CASE lnBar = 1
    DO lpPoSele
  CASE lnBar = 2
    llEdCost = !llEdCost
    SET MARK OF BAR 2 OF _OPTIONPOP TO llEdCost
    *B605812,1 ABD - show the related field if edit cost per line in the detail folder. [Begin]
    IF !llEdCost
      SHOW GET lnCost3   DISABLE
      SHOW GET lnCost2   DISABLE
      SHOW GET lnCost4   DISABLE
    ELSE
      IF !EOF(lcDetTmp)
        SHOW GET lnCost3   ENABLE
        SHOW GET lnCost2   ENABLE
        SHOW GET lnCost4   ENABLE
      ENDIF  
    ENDIF  
    *B605812,1 ABD - [End]
    
  CASE lnBar = 3
    lcOld39 = laData[39]
    lcOld40 = laData[40]
    lcOld41 = laData[41]
    lcOld42 = laData[42]
    PUSH KEY
    ON KEY
    DO (gcScrDir+gcWinAppl+'\POMCOST.SPX')
    POP KEY
    SHOW GET lnCost3 
    SHOW GET lnCost2 
    SHOW GET lnCost4 
  CASE lnBar = 4
    *B602633,1 (Begin) Remark as it always clear the selected GL code desc.
    *lcLinkDesc = SPACE(30)
    lcGl_link = POFHDR.Link_Code
    *B605724,1 ABD - remark the next line and seek with WIP code [05] not [04]. [Begin]
    *=SEEK("04"+lcGl_link, "GL_Link")
    =SEEK("05"+lcGl_link, "GL_Link")
    *B605724,1 ABD [End]
    
    lcLinkDesc = GL_Link.LinkDesc
    *B602633,1 (End)
    
    lcOld35 = laData[35]
    lcOld36 = laData[36]
    lcOld37 = laData[37]
    lcOld38 = laData[38]
    lcOld47 = laData[47]
    lcOld48 = laData[48]
    lcOld49 = laData[49]
    lcOld50 = laData[50]
    DO (gcScrDir+gcWinAppl+'\MAPOACT.SPX')

  *B802047 AHM (Start)
  CASE lnBar = 5
    = lfReceived()
  *B802047 AHM (Start)

  *C102780,1 AMH Run the PO Revision screen [Start]
  CASE lnBar = 6
    DO (gcScrDir+gcWinAppl+'\MAPOREV.SPX')
  *C102780,1 AMH [End]

ENDCASE    

*!*************************************************************
*! Name        : lfvQty
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the quantity field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvQty
PRIVATE lnAlias

*E500442,5 ABD - [Begin]
IF MDOWN()
  RETURN
ENDIF  
*E500442,5 ABD - [End]

lnAlias = SELECT()
IF lnQty <= 0         &&&&&&&&&&&&AND !SEEK(lcKeyType+laData[1]+lcFabric+lcColor+'1','POFLN')
  =gfModalGen('INM36014B36000','DIALOG','Quantity')
  lnQty   = lnOldQty
  _CUROBJ = OBJNUM(lnQty)
ELSE
  lnRQty =lfQtyRec(lcFabric,lcColor)
  IF lnQty < lnRQty 
    =gfModalGen('INM36016B36000','DIALOG')
    lnQty   = lnOldQty
    _CUROBJ = OBJNUM(lnQty)
  ELSE
    lnTotQty = lnQty * lnCost1
    =SEEK(lcKeyType+laData[1]+lcFabric+lcColor,(lcDetTmp))
    REPLACE &lcDetTmp..nFabTOTQTY WITH lnQty
    =lfRefresh()
    SHOW WINDOW (lcDet_Ttl) REFRESH SAME
    IF !llEdCost AND (LASTKEY() <> 9 AND LASTKEY() <> 15)
      _CUROBJ = OBJNUM(pbNew)
    ENDIF
  ENDIF  
ENDIF

ladata[39] = ladata[39]+lnCost1*(lnQty-lnOldQty)
ladata[27] = IIF(lcFrnSign1='*',ROUND(ladata[39]*laData[24]/lnUnit1,3),ROUND(ladata[39]/laData[24]/lnUnit1,3))

ladata[40] = ladata[40]+lnCost2*(lnQty-lnOldQty)
ladata[28] = IIF(lcFrnSign2='*',ROUND(ladata[40]*laData[26]/lnUnit2,3),ROUND(ladata[40]/laData[26]/lnUnit2,3))

ladata[41] = ladata[41]   + lnCost3*(lnQty-lnOldQty)
ladata[29] = IIF(lcFrnSign2='*',ROUND(ladata[41]*laData[26]/lnUnit2,3),ROUND(ladata[41]/laData[26]/lnUnit2,3))

ladata[42] = ladata[42]   + lnCost4*(lnQty-lnOldQty)
ladata[30] = IIF(lcFrnSign2='*',ROUND(ladata[42]*laData[26]/lnUnit2,3),ROUND(ladata[42]/laData[26]/lnUnit2,3))

ladata[51] = ladata[51] + lnQty  - lnOldQty
ladata[55] = ladata[55] + lnQty  - lnOldQty

SELECT (lcPOHdr)
=RLOCK()
GATHER FROM laData FIELDS &lcScFields MEMO    
UNLOCK
=lfRefresh()
SELECT(lnAlias)
*!*************************************************************
*! Name        : lfwQty
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old quantity  in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwQty
lnOldQty = lnQty

*!*************************************************************
*! Name        : lfvCost1
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the Price field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvCost1

IF lnCost1 < 0 
  =gfModalGen('INM36014B36000','DIALOG','Price')
  lnCost1  = lnOldCost1
ELSE  
  lnTotQty = lnQty * lnCost1
  =SEEK(lcKeyType+laData[1]+lcFabric+lcColor,(lcDetTmp))
  REPLACE &lcDetTmp..nCost1  WITH lnCost1
  REPLACE &lcDetTmp..nECost1 WITH IIF(lcFrnSign1='*',ROUND(&lcDetTmp..nCost1*laData[24]/lnUnit1,3),ROUND(&lcDetTmp..nCost1/laData[24]/lnUnit1,3))
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
ENDIF
ladata[39] = ladata[39]   + (lnCost1 - lnOldCost1)*&lcDetTmp..nFabTotQty
ladata[27] = IIF(lcFrnSign1='*',ROUND(ladata[39]*laData[24]/lnUnit1,3),ROUND(ladata[39]/laData[24]/lnUnit1,3))
*!*************************************************************
*! Name        : lfvCost2
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the Freight field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvCost2

IF lnCost2 < 0 
  =gfModalGen('INM36014B36000','DIALOG','Freight')
  lnCost2  = lnOldCost2
ELSE
  =SEEK(lcKeyType+laData[1]+lcFabric+lcColor,(lcDetTmp))
  REPLACE &lcDetTmp..nCost2 WITH lnCost2 ,;
          &lcDetTmp..nECost2 WITH IIF(lcFrnSign2='*',ROUND(&lcDetTmp..nCost2*laData[26]/lnUnit2,3),ROUND(&lcDetTmp..nCost2/laData[26]/lnUnit2,3))
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
ENDIF
ladata[40] = ladata[40]   + (lnCost2 - lnOldCost2)*&lcDetTmp..nFabTotQty
ladata[28] = IIF(lcFrnSign2='*',ROUND(ladata[40]*laData[26]/lnUnit2,3),ROUND(ladata[40]/laData[26]/lnUnit2,3))
*!*************************************************************
*! Name        : lfvCost3
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the quota field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvCost3

IF lnCost3 < 0 
  =gfModalGen('INM36014B36000','DIALOG','Tax')
  lnCost3  = lnOldCost3
ELSE
  =SEEK(lcKeyType+laData[1]+lcFabric+lcColor,(lcDetTmp))
  REPLACE &lcDetTmp..nCost3 WITH lnCost3 ,;
          &lcDetTmp..nECost3 WITH IIF(lcFrnSign2='*',ROUND(&lcDetTmp..nCost3*laData[26]/lnUnit2,3),ROUND(nCost3/laData[26]/lnUnit2,3))
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
ENDIF
ladata[41] = ladata[41]   + (lnCost3 - lnOldCost3)*&lcDetTmp..nFabTotQty
ladata[29] = IIF(lcFrnSign2='*',ROUND(ladata[41]*laData[26]/lnUnit2,3),ROUND(ladata[41]/laData[26]/lnUnit2,3))
*!*************************************************************
*! Name        : lfvCost4
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the tax field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvCost4

IF lnCost4 < 0 
  =gfModalGen('INM36014B36000','DIALOG','Quota')
  lnCost4  = lnOldCost4
ELSE
  =SEEK(lcKeyType+laData[1]+lcFabric+lcColor,(lcDetTmp))
  REPLACE &lcDetTmp..nCost4 WITH lnCost4 ,;
          &lcDetTmp..nECost4 WITH IIF(lcFrnSign2='*',ROUND(&lcDetTmp..nCost4*laData[26]/lnUnit2,3),ROUND(nCost4/laData[26]/lnUnit2,3))
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
  IF (LASTKEY() <> 9 AND LASTKEY() <> 15)
    _CUROBJ = OBJNUM(pbNew)
  ENDIF
ENDIF
ladata[42] = ladata[42]   + (lnCost4 - lnOldCost4)*&lcDetTmp..nFabTotQty
ladata[30] = IIF(lcFrnSign2='*',ROUND(ladata[42]*laData[26]/lnUnit2,3),ROUND(ladata[42]/laData[26]/lnUnit2,3))
*!*************************************************************
*! Name        : lfwCost1
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old price in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwCost1

lnOldCost1 = lnCost1
*!*************************************************************
*! Name        : lfwCost2
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old freight in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwCost2

lnOldCost2 = lnCost2
*!*************************************************************
*! Name        : lfwCost3
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old quota in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwCost3

lnOldCost3 = lnCost3
*!*************************************************************
*! Name        : lfwCost4
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old tax in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwCost4

lnOldCost4 = lnCost4
*!*************************************************************
*! Name        : lfvPattern
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the pattern field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvPattern

=SEEK(lcKeyType+laData[1]+lcFabric+lcColor,(lcDetTmp))
REPLACE &lcDetTmp..Pattern WITH lcPattern
SHOW WINDOW (lcDet_Ttl) REFRESH SAME
*!*************************************************************
*! Name        : lfvWidth
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the width field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvWidth

=SEEK(lcKeyType+laData[1]+lcFabric+lcColor,(lcDetTmp))
REPLACE &lcDetTmp..Width WITH lcWidth
SHOW WINDOW (lcDet_Ttl) REFRESH SAME
*!*************************************************************
*! Name        : lfUpdLines
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : to update POFLN file 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfUpdLines

lnAlias = SELECT()
IF laScrmode[3]
  lcDelete = SET ('DELETE') 
  SET DELETE OFF
  SELECT(lcDetTmp)
  SCAN FOR DELETED() AND cMatType = lcKeyType AND POMAT = laData[1]
    IF SEEK(lcKeyType+laData[1]+Fabric+Color+'1','POFLN') 
      SELECT POFLN
      
      *B607159,1 AMH Scan the current record in the lcDetTmp file only [Start]
      *SCAN WHILE cMatType+POMat+Fabric+Color+Trancd = lcKeyType+laData[1]+Fabric+Color+'1'
      SCAN WHILE cMatType+POMat+Fabric+Color+Trancd =;
                 lcKeyType+laData[1]+EVALUATE(lcDetTmp+'.FABRIC+'+lcDetTmp+'.COLOR')+'1'
      *B607159,1 AMH [End]
      
        IF !DELETED()
          =SEEK(FABRIC+COLOR,'FABRIC')
          SELECT FABRIC
          =RLOCK()
          REPLACE FABRIC.ONORDER WITH IIF(lcKeyType = 'P',FABRIC.ONORDER-POFLN.nFabTotQty*CONV,FABRIC.ONORDER + POFLN.nFabTotQty*CONV)
          UNLOCK 

          *B604786,1 MHM 08/23/2001 updated FABDYE[Start].
          =SEEK(FABRIC+COLOR+POFLN.cWareCode,'FABDYE')
          SELECT FABDYE
          =RLOCK()
          REPLACE FABDYE.ONORDER WITH IIF(lcKeyType = 'P',FABDYE.ONORDER-POFLN.nFabTotQty*FABRIC.CONV,FABDYE.ONORDER + POFLN.nFabTotQty*FABRIC.CONV)
          UNLOCK 
          *B604786,1 MHM [End].
          
       ENDIF
     ENDSCAN   
      SELECT(lcDetTmp)
    ENDIF
  ENDSCAN
  SET DELETE &lcDelete
  SELECT POFLN
  *E500442,5 ABD - Try to go to first recored we will delete and delete 
  *E500442,5 ABD -  rest, this command will not take alomg time. [Begin]
  *DELETE ALL FOR cMatType = lcKeyType AND POMAT = laData[1] AND TRANCD = "1"
  IF SEEK(lcKeyType+laData[1]) 
    DELETE REST WHILE cMatType+POMat+Fabric+Color+Trancd = lcKeyType+laData[1] FOR TRANCD = "1"
  ENDIF
  *E500442,5 ABD - [End]
ENDIF
SELECT(lcDetTmp)
REPLACE ALL POMAT      WITH laData[1]
REPLACE ALL VENDOR     WITH laData[2]
REPLACE ALL cMatType   WITH lcKeyType
REPLACE ALL cWareCode  WITH laData[3]
REPLACE ALL cFabGrade  WITH lcQuality
SELECT POFLN
APPEND FROM (gcWorkDir+lcDetTmp)
SELECT(lnAlias)
*!*************************************************************
*! Name        : lfUpdHdr
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : to update POFHDR file 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfUpdHdr

SELECT POFHDR
IF laScrMode[4]
  APPEND BLANK
ELSE
  SEEK(lcKeyType+laData[1])  
ENDIF

SELECT(lcDetTmp)
ladata[39] = 0
ladata[40] = 0
ladata[41] = 0
ladata[42] = 0
ladata[27] = 0
ladata[28] = 0
ladata[29] = 0
ladata[30] = 0
SCAN 
  ladata[39] = ladata[39] + (nCost1*nFabTotQty)
  ladata[40] = ladata[40] + (nCost2*nFabTotQty)
  ladata[41] = ladata[41] + (nCost3*nFabTotQty)
  ladata[42] = ladata[42] + (nCost4*nFabTotQty)     

  ladata[27] = ladata[27] + (nECost1*nFabTotQty)
  ladata[28] = ladata[28] + (nECost2*nFabTotQty)
  ladata[29] = ladata[29] + (nECost3*nFabTotQty)
  ladata[30] = ladata[30] + (nECost4*nFabTotQty)     
ENDSCAN
SELECT POFHDR
=RLOCK()
REPLACE POMAT     WITH laData[1] ,;
        cMatType  WITH lcKeyType ,;  
        VENDOR    WITH laData[2],;
        cDivision WITH laData[11],;
        ENTERED   WITH laData[9],;
        COMPLETE  WITH laData[10],;
        ORIGIN    WITH laData[19];
        cFabGrade WITH lcQuality
        
REPLACE cFob      WITH laData[15],;
        cTermCode WITH laData[12],;
        SHIPVIA   WITH laData[13],;
        QUOTACAT  WITH laData[16],;
        CLCNO     WITH laData[20],;
        link_Code WITH laData[56]
 
REPLACE LCEXPIRE  WITH laData[21],;
        CONTACT   WITH laData[17],;
        PHONE     WITH laData[18],;
        INSURANCE WITH laData[14],;
        CPRICECUR WITH laData[23],;
        CDUTYCUR  WITH laData[25],;
        NPRICERAT WITH laData[24],;
        NDUTYRAT  WITH laData[26]
        
REPLACE cOutAddr1  WITH laData[4],;
        cOutAddr2  WITH laData[5],;
        cOutAddr3  WITH laData[6],;        
        cOutAddr4  WITH laData[7],;        
        cOutAddr5  WITH laData[8],;                
        cWarecode  WITH laData[3],;
        cRequis    WITH laData[22],;
        LastLine   WITH lnLastLine,;
        nfaborder  WITH ladata[51],;
        npo_open   WITH IIF(lnStatus = 2 OR lnStatus = 5,0,ladata[55]),;
        nfabcancel WITH ladata[54] ,;
        nCost1     WITH ladata[39] ,;
        nCost2     WITH ladata[40] ,;
        nCost3     WITH ladata[41] ,;
        nCost4     WITH ladata[42] ,;
        nECost1    WITH ladata[27] ,;
        nECost2    WITH ladata[28] ,;
        nECost3    WITH ladata[29] ,;
        nECost4    WITH ladata[30] ,;
        POTotal    WITH nEcost1+nEcost2+nEcost3+nEcost4,;
        nAct_Cost1 WITH ladata[47] ,;
        nAct_Cost2 WITH ladata[48] ,;
        nAct_Cost3 WITH ladata[49] ,;
        nAct_Cost4 WITH ladata[50] ,;
        nEActCost1 WITH ladata[35] ,;
        nEActCost2 WITH ladata[36] ,;
        nEActCost3 WITH ladata[37] ,;
        nEActCost4 WITH ladata[38]                        

*B125577,1 NNA 12/29/2004 (Begin) if user changed the PO qty. to equal with the Received qty. or 
*B125577,1 NNA            there were over received and there is no open Qty then I'll check quantities 
*B125577,1 NNA            then I'll Change the Status of the PO
*--if the PO ordered qty.<= the Received + Cancelled + others (Second+damaged) and open = 0 then this PO completed
IF ladata[51]<=ladata[52]+ladata[53]+ladata[54] AND laData[55]=0
  lnStatus = 4
  lcStatus = 'Completed'
ENDIF
SHOW GETS 
*B125577,1 NNA (End)

DO CASE
  CASE lcStatus = 'Open'
    REPLACE STATUS WITH 'O'
  CASE lcStatus = 'Bid'
    REPLACE STATUS WITH 'B'
  CASE lcStatus = 'Hold'
    REPLACE STATUS WITH 'H'
  CASE lcStatus = 'Cancelled'
    REPLACE STATUS WITH 'X'
  CASE lcStatus = 'Completed'
    REPLACE STATUS WITH 'C'
ENDCASE

*C102780,1 AMH Save the revision number [Start]
REPLACE CREVNO WITH laData[57]
*C102780,1 AMH [End]

*C102819,1 AMH Save the MRP number [Start]
IF llMrp
  REPLACE CMRP WITH laData[58]
ENDIF
*C102819,1 AMH [End]

UNLOCK

*B804444,1 (Begin) Initialize needed variables.
IF laScrMode[4]
  IF ALEN(laNewMpos,1) = 1 AND EMPTY(laNewMpos)
    
    *B605044,1 ABD - Add the correct type to the array that we 
    *B605044,1 ABD - will print the PO that we just create.[Begin]
    *laNewMpos[1] = 'P'+laData[1]
    laNewMpos[1] = lcScrType+laData[1]
    *B605044,1 ABD - [End]
    
  ELSE
    lnNewOne = ALEN(laNewMpos,1) +1
    DIMENSION  laNewMpos[lnNewOne]
    
    *B605044,1 ABD - Add the correct type to the array that we 
    *B605044,1 ABD - will print the PO that we just create.[Begin]
    *laNewMpos[lnNewOne] = 'P'+laData[1]
    laNewMpos[lnNewOne] = lcScrType+laData[1]
    *B605044,1 ABD - [End]
    
  ENDIF
ENDIF  
*B804444,1 (End) 


*!*************************************************************
*! Name        : lfvRemLine
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : to remove line from the P/O lines
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvRemLine

lnAlias = SELECT()
SELECT(lcDetTmp)
IF gfModalGen('INM36017B36001','DIALOG') = 1

  *C200149,1  ABD 02/20/2001 Add new triger. [Begin]
  IF ASCAN(laEvntTrig , PADR('DLEALOMA',10)) <> 0
    lcParam = 'L'
    llReturn  =gfDoTriger('MAPRCAM',PADR('DLEALOMA',10))
    IF !llReturn
      RETURN 
    ENDIF
  ENDIF  
  *C200149,1  ABD [End]

  IF SEEK(lcKeyType+laData[1]+lcFabric+lcColor+'2','POFLN') .AND. gfModalGen('INM36010B36000','DIALOG','Shipment') = 1
    RETURN
  ENDIF
  IF SEEK(lcKeyType+laData[1]+lcOldFab+lcOldClr+'3','POFLN') .AND. gfModalGen('INM36010B36000','DIALOG','Damaged') = 1
    RETURN
  ENDIF
  IF SEEK(lcKeyType+laData[1]+lcOldFab+lcOldClr+'4','POFLN') .AND. gfModalGen('INM36011B36000','DIALOG','Cancelled ') = 1
    RETURN
  ENDIF
  ladata[51] = ladata[51] - nFabTotQty
  ladata[55] = ladata[55] - nFabTotQty
  
  laData[39] = laData[39] - (&lcDetTmp..nCost1*&lcDetTmp..nFabTotQty)
  laData[40] = laData[40] - (&lcDetTmp..nCost2*&lcDetTmp..nFabTotQty)
  laData[41] = laData[41] - (&lcDetTmp..nCost3*&lcDetTmp..nFabTotQty)
  laData[42] = laData[42] - (&lcDetTmp..nCost4*&lcDetTmp..nFabTotQty)

  ladata[27] = IIF(lcFrnSign1='*',ROUND(ladata[39]*laData[24]/lnUnit1,3),ROUND(ladata[39]/laData[24]/lnUnit1,3))
  ladata[28] = IIF(lcFrnSign2='*',ROUND(ladata[40]*laData[26]/lnUnit2,3),ROUND(ladata[40]/laData[26]/lnUnit2,3))
  ladata[29] = IIF(lcFrnSign2='*',ROUND(ladata[41]*laData[26]/lnUnit2,3),ROUND(ladata[41]/laData[26]/lnUnit2,3))
  ladata[30] = IIF(lcFrnSign2='*',ROUND(ladata[42]*laData[26]/lnUnit2,3),ROUND(ladata[42]/laData[26]/lnUnit2,3))

  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)
  DELETE
ENDIF
SHOW WINDOW (lcDet_Ttl) REFRESH SAME
GOTO TOP
IF EOF(lcDetTmp)
  SHOW GET pbRemove DISABLE
  lcFabric  = SPACE(07) 
  lcColor   = SPACE(06) 
  
  *C102780,1 AMH Update the vendor item, vendor color and delivry date [Start]
  IF llVenRef
    lcVenFab = SPACE(10)
    lcVenColr = SPACE(10)
  ENDIF
  IF llDlivDate
    ldDelvDate = {  /  /    }
  ENDIF
  *C102780,1 AMH [End]
  
  lcPattern = SPACE(10) 
  lcWidth   = SPACE(06)
  lcClrDesc = SPACE(15)
  lcIDesc   = SPACE(20)
  lnCost1   = 0 
  lnCost2   = 0 
  lnCost3   = 0 
  lnCost4   = 0       
  lnQty     = 0
  lnTotQty  = 0
  lnLineNo  = 0
ENDIF
=lfwLine()
SELECT(lnAlias)
*!*************************************************************
*! Name        : lfwFabric
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old fabric field in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwFabric

lcOldFab = lcFabric
*!*************************************************************
*! Name        : lfwColor
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old color fieldin a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwColor

lcOldClr = lcColor
*!*************************************************************
*! Name        : POFBROW
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : browse the P/OS due to the selected vendor
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION POFBROW
PARAMETER lcPOType,lcVendor,lcPo
*E500442,5 ABD - Fix problem ladata.prg not found when open the browse
*E500442,5 ABD - and try to click on the menue. [Begin]
*PRIVATE lcBrFields,lcAlias,laData,llRetValue,lcPOOrder
PRIVATE lcBrFields,lcAlias,llRetValue,lcPOOrder
*E500442,5 ABD - [End]

lcAlias = ALIAS()
SELECT POFHDR
lcOldFilt = FILTER()
SET FILT TO
DO CASE
  CASE lcPOType= 'P'
    lcTitle  = 'Material Purchase Orders'
  CASE lcPOType= 'C'
    lcTitle  = 'Material Contracts'
  CASE lcPOType= 'R'
    lcTitle  = 'Material Purchase Order Returns'
ENDCASE
lcPOOrder = ORDER('POFHDR')
lcVendor  = IIF(TYPE('lcVendor')='C' .AND. !EMPTY(lcVendor),lcVendor,'')
lcPo      = IIF(TYPE('lcPo')='C',lcPo,SPACE(6))
SET ORDER TO TAG IIF(EMPTY(lcVendor),'POFHDR','POFHDRV') IN POFHDR
IF !SEEK(lcVendor+lcPOType,'POFHDR')
  =gfDialog ("I","No "+lcTitle+' found'+IIF(EMPTY(lcVendor),'',' for vendor '+lcVendor)+'.')
  lcVendor = SPACE(8)
  lcPo     = SPACE(6)
  SELECT POFHDR
  SET FILTER TO &lcOldFilt 
  SET ORDER TO TAG lcPoOrder IN POFHDR
  IF !EMPTY(lcAlias)
     SELECT (lcAlias)
  ENDIF  
  RETURN(.F.)
ENDIF
DECLARE laBrow[2]
laBrow=' '

lcBrFields = "POMAT   :R :H=lcPrgtype:8,"+;
             "Status  :R :H='S':2,"+;	         	           	         
             "Vendor  :R :H='Vendor':10,"+;
	         "ApVendor.cVenComp :R :H='Name':18,"+;
	         "Complete:R :H='Complete':8,"+;
  	         "NFABORDER  :R :H='Tot.Qty.':7,"+;	         
             "POTotal :R :H='Amount':10,"+;
             "NFBRECEIVE :R :H='Receive':7,"+;
             "NPO_OPEN   :R :H='Open':7"
llOpnApVen = gfOpenFile(gcDataDir+'ApVendor',gcDataDir+'VenCode','SH')
CLEAR TYPEAHEAD
SELECT POFHDR
SET RELATION TO Vendor INTO ApVendor
=SEEK(lcVendor+lcPOType)
llRetValue = gfBrows('lcVendor+lcPOType',"PoMat,Vendor","laBrow",lcTitle)
IF llRetValue
  lcPo     = laBrow[1]
  lcVendor = laBrow[2]
ELSE
  lcPo     = SPACE(6)
  lcVendor = SPACE(8) 
ENDIF  
IF llOpnApVen
  USE IN ApVendor
ENDIF
SET FILTER TO &lcOldFilt 
SET ORDER TO TAG lcPoOrder IN POFHDR
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF  
RETURN llRetValue
*!*************************************************************
*! Name        : lfvBrows
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : valid function for the lines browse
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvBrows
IF WONTOP() # (lcDet_Ttl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*!*************************************************************
*! Name        : lfUpdVend
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : update for the vendor file
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfUpdVend

lnAlias = SELECT()
SELECT APVENDOR  
IF laScrMode[4] OR POFHDR.STATUS = 'B' 
  =SEEK(laData[2])
  =RLOCK()
  REPLACE nVenOpnPO WITH nVenOpnPO + IIF(lcKeytype = 'P',ladata[27],-ladata[27])
  UNLOCK        
ELSE
  =SEEK(laData[2])
  =RLOCK()
  REPLACE nVenOpnPO WITH nVenOpnPO + IIF(lcKeytype = 'P',ladata[27]-POFHDR.nEcost1 ,POFHDR.nEcost1 - ladata[27])
  UNLOCK        
  IF laData[2] <> POFHDR.VENDOR
    IF SEEK(POFHDR.VENDOR)
      =RLOCK()
      REPLACE nVenOpnPO WITH nVenOpnPO - POFHDR.nEcost1
      UNLOCK
    ENDIF  
  ENDIF  
ENDIF
SELECT(lnAlias)
*!*************************************************************
*! Name        : lfContLin
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : select all the contracts line matching the criteria
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*E500442,1 ABD - we will not Call this function again. [Begin]
*FUNCTION lfContLin

*lnAlias = SELECT()
*SELECT POFHDR
*lcFilter = FILTER()
*SET FILT TO 
*SELECT DISTINCT POFHDR.POMAT,POFHDR.CPRICECUR,POFHDR.CDUTYCUR,;
*                POFHDR.ENTERED ,POFHDR.COMPLETE,POFHDR.VENDOR,;
*                POFLN.FABRIC,POFLN.COLOR,POFLN.nCost1,;
*                POFLN.nCost2,POFLN.nCost3,POFLN.nCost4;
*  FROM POFHDR,POFLN ;
*  WHERE POFHDR.cMatType  = 'C' ;
*    AND POFHDR.STATUS = 'O' ;
*    AND POFLN.POMAT   = POFHDR.POMAT;
* INTO DBF (gcWorkDir+lcContLin)
*INDEX ON FABRIC+COLOR TAG (lcContLin) OF (gcWorkDir+lcContLin)
*SET FILTER TO &lcContLin..VENDOR   =  laData[2]   AND ;
*              &lcContLin..ENTERED  <= laData[9]   AND ;
*              &lcContLin..COMPLETE => laData[10]  AND ;
*              (&lcContLin..CPRICECUR = laData[23] OR  ;
*               &lcContLin..CDUTYCUR  = laData[25])
*SELECT POFHDR
*SET FILT TO &lcFilter 
*SELECT(lnAlias)
*E500442,1 ABD - we will not Call function again. [End]
*!*************************************************************
*! Name        : lfwStatus
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : redimenssion the lastatus array
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwStatus

DO CASE
  CASE POFHDR.STATUS = 'B' OR (laScrMode[4] AND lcKeyType <> 'C')
    DIMENSION laStatus[2]
    laStatus[1]  = 'Open'
    laStatus[2]  = 'Bid'
    SHOW GET lnStatus ENABLE
  CASE POFHDR.STATUS = 'H' OR (laScrMode[4] AND lcKeyType = 'C')
    DIMENSION laStatus[2]
    laStatus[1]  = 'Open'
    laStatus[2]  = 'Hold'
    lnStatus = ASCAN(laStatus,lcStatus)
    SHOW GET lnStatus ENABLE
ENDCASE
*!*************************************************************
*! Name        : lfvStatus
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the status field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvStatus

lcStatus = laStatus[lnStatus]
IF lcKeyType = 'C' AND lcStatus = 'Open'

  *E500442,5 ABD - No need this function we will use new function to
  *E500442,5 ABD - Valid the lines for this fabric color. [Begin]
  *IF lfvCont()
  IF lfvLine()
    *E500442,5 ABD - [End]
  
    =gfModalGen('TRM36021B00000','ALERT')
    lnStatus = 2
    lcStatus = laStatus[lnStatus]
    RETURN
  ENDIF  
ENDIF
laData[55] = IIF((POFHDR.Status = 'B' OR POFHDR.Status = 'H') AND lnStatus = 1,laData[51],laData[55])
*!*************************************************************
*! Name        : lfUpdFab
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : update the fabric file
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*! Modifications: 
*!               B602197,1 HDM  Also Update Fabdye File[onorder field]
*!               B801822,1 Force to get the fabric.conv
*!*************************************************************
FUNCTION lfUpdFab

lnAlias = SELECT()
SELECT(lcDetTmp)
SET RELATION TO cMatType+pomat+fabric+color+trancd INTO Pofln ADDITIVE
SCAN 
  IF SEEK(&lcDetTmp..Fabric+&lcDetTmp..Color,'FABRIC')
    SELECT FABRIC
    =RLOCK()
    REPLACE FABRIC.ONORDER WITH IIF(lcKeyType = 'P',;
            IIF(POFHDR.STATUS = 'B',FABRIC.ONORDER + (&lcDetTmp..nFabTOTQTY *CONV),FABRIC.ONORDER + ((&lcDetTmp..nFabTOTQTY - POFLN.nFabTotQty)*CONV)),;
            IIF(POFHDR.STATUS = 'B',FABRIC.ONORDER - (&lcDetTmp..nFabTOTQTY *CONV),FABRIC.ONORDER + ((POFLN.nFabTotQty - &lcDetTmp..nFabTOTQTY )*CONV)))
    UNLOCK
    SELECT(lcDetTmp)
  ENDIF
  *--B602197,1 HDM Also upadate fabdye file[START]
  SELECT FABDYE
  SET ORDER TO TAG FABDYE
  IF SEEK(&lcDetTmp..Fabric+&lcDetTmp..Color+ladata[3],'FABDYE')
  *-- B801822,1 Force to get the fabric.conv[start]
  *  REPLACE FABDYE.ONORDER WITH IIF(lcKeyType = 'P',;
            IIF(POFHDR.STATUS = 'B',FABDYE.ONORDER + (&lcDetTmp..nFabTOTQTY * CONV),FABDYE.ONORDER + ((&lcDetTmp..nFabTOTQTY - POFLN.nFabTotQty)*FABRIC.CONV)),;
            IIF(POFHDR.STATUS = 'B',FABDYE.ONORDER - (&lcDetTmp..nFabTOTQTY * CONV),FABDYE.ONORDER + ((POFLN.nFabTotQty - &lcDetTmp..nFabTOTQTY )*FABRIC.CONV)))

     REPLACE FABDYE.ONORDER WITH IIF(lcKeyType = 'P',;
            IIF(POFHDR.STATUS = 'B',FABDYE.ONORDER + (&lcDetTmp..nFabTOTQTY * FABRIC.CONV),FABDYE.ONORDER + ((&lcDetTmp..nFabTOTQTY - POFLN.nFabTotQty)*FABRIC.CONV)),;
            IIF(POFHDR.STATUS = 'B',FABDYE.ONORDER - (&lcDetTmp..nFabTOTQTY * FABRIC.CONV),FABDYE.ONORDER + ((POFLN.nFabTotQty - &lcDetTmp..nFabTOTQTY )*FABRIC.CONV)))
  *-- B801822,1 Force to get the fabric.conv[end]

  ENDIF
  UNLOCK
  SELECT(lcDetTmp)
*--B602197,1 HDM Also upadate fabdye file[END]
ENDSCAN
SET RELATION TO 
SELECT(lnAlias)
*!*************************************************************
*! Name        : lfCanLine
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : update the POFLN file in case of cancel or uncancel
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfCanLine
PARAMETER llCancel

lnAlias = SELECT()
SELECT POFLN
IF llCancel
  SET FILTER TO cMatType = lcKeyType AND POMAT = laData[1] 
  SET ORDER TO TAG POFLNF
  GOTO TOP
  DO WHILE !EOF()
    lcFabLn  = POFLN.FABRIC
    lcClrLn  = POFLN.COLOR
    lnTotQty = 0
    lnCost   = 0
    SCAN WHILE FABRIC = lcFabLn AND COLOR =lcClrLn
      DO CASE
        CASE Trancd = '1'
          lnTotQty = lnTotQty + nFabTotQty
          lnCost   = nECost1
        CASE Trancd = '2' OR Trancd = '3' OR Trancd = '4'
          lnTotQty = lnTotQty - nFabTotQty
      ENDCASE
    ENDSCAN
    IF lnTotQty > 0
      SELECT (lcDetTmp)
      APPEND BLANK
      *B604660,1 MHM 07/25/2001 Update WareHouse in Pofln [Star]
      *REPLACE Fabric  WITH lcFabLn,;
              COLOR   WITH lcClrLn,;
              nFabTotQty  WITH lnTotQty,;
              TRANCD  WITH '4',;
              cMatType   WITH lcKeyType,;
              POMAT   WITH laData[1],;
              VENDOR  WITH laData[2],;
              nEcost1 WITH lnCost   ,;
              lCancel WITH .T.
      REPLACE Fabric     WITH lcFabLn,;
              COLOR      WITH lcClrLn,;
              nFabTotQty WITH lnTotQty,;
              TRANCD     WITH '4',;
              cMatType   WITH lcKeyType,;
              POMAT      WITH laData[1],;
              VENDOR     WITH laData[2],;
              nEcost1    WITH lnCost   ,;
              cWareCode  WITH laData[3],;
              lCancel    WITH .T.
      *B604660,1 MHM [End]
              *B803242,1 (Begin) Update date field.
              REPLACE Date WITH gdSysDate
              *B803242,1 (End)

      SELECT POFLN        
    ENDIF
  ENDDO  
SET FILTER TO
ELSE
  SET FILTER TO cMatType = lcKeyType AND POMAT = laData[1] AND TRANCD = '4' AND lCancel
  lnTotQty  = 0
  SCAN
    lnTotQty  = lnTotQty + nFabTotQty
  ENDSCAN     
  SET FILTER TO
  DELETE FOR cMatType = lcKeyType AND POMAT = laData[1] AND TRANCD = '4' AND lCancel
ENDIF  
SELECT(lnAlias)
RETURN lnTotQty
*!*************************************************************
*! Name        : lfCanLine
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : update the FABRIC file in case of cancel or uncancel
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfCanFab
PARAMETER llCancel


lnAlias = SELECT()

*E500442,5 ABD Fix bug Variable 'lcFabdye' not found. [Begin]

*B604856,1 AMH Commented out. Retern it back down in the loop [Start]
*lcWare   = POFLN.cWareCode
*lcFabDye = POFLN.Dyelot
*B604856,1 AMH [End]

*E500442,5 ABD [End]

SELECT POFLN
IF llCancel
  SET FILTER TO cMatType = lcKeyType AND POMAT = laData[1]
  GOTO TOP
  DO WHILE !EOF()
    lcFabLn  = POFLN.FABRIC
    lcClrLn  = POFLN.COLOR
    
    *E500442,5 ABD Move up. [Begin]
    *--B602618,1 RAMY [START]
    
    *B604856,1 AMH Release the comment by E500442,5 [Start]
    *lcWare   = POFLN.cWareCode
    *lcFabDye = POFLN.Dyelot
    lcWare   = POFLN.cWareCode
    lcFabDye = POFLN.Dyelot
    *B604856,1 AMH [End]
    
    *--B602618,1 RAMY [END]
    *E500442,5 ABD [End]
    
    lnTotQty = 0
    
    *B604856,1 AMH Calculate the canceled quantity for fabric/color/warehouse/dyelot [Start]
    *SCAN WHILE FABRIC = lcFabLn AND COLOR = lcClrLn
    SCAN WHILE FABRIC = lcFabLn AND COLOR = lcClrLn AND CWARECODE = lcWare AND DYELOT = lcFabDye
    *B604856,1 AMH [End]
    
      IF TRANCD = '1'
        lnTotQty = lnTotQty + nFabTOTQTY
      ELSE
        lnTotQty = lnTotQty - nFabTOTQTY
      ENDIF
    ENDSCAN
    IF SEEK(lcFabLn+lcClrLn,'FABRIC')
      SELECT FABRIC
      REPLACE FABRIC.ONORDER WITH FABRIC.ONORDER +IIF(lcKeyType = 'P',- (lnTotQty*CONV),(lnTotQty*CONV))
      SELECT POFLN
      *--B602618,1 RAMY [START]
      IF SEEK(lcFabLn+lcClrLn+IIF(!EMPTY(lcWare) , lcWare , ;
              POFHDR.cWareCode) + SPACE(10),'FABDYE')
        SELECT FABDYE
        REPLACE ONORDER WITH ONORDER +IIF(lcKeyType = 'P' , - (lnTotQty * FABRIC.CONV) , (lnTotQty * FABRIC.CONV))
        SELECT POFLN
      ENDIF
      
      *B604856,1 AMH Check if there is a dyelot [Start]
      *IF llDyelot
      IF llDyelot .AND. !EMPTY(lcFabDye)
      *B604856,1 AMH [End]
      
        IF SEEK(lcFabLn+lcClrLn+IIF(!EMPTY(lcWare) , lcWare , ;
                POFHDR.cWareCode) + lcFabDye ,'FABDYE')
          SELECT FABDYE
          REPLACE FABDYE.ONORDER WITH FABDYE.ONORDER +IIF(lcKeyType = 'P' , - (lnTotQty * FABRIC.CONV) , (lnTotQty * FABRIC.CONV))
          SELECT POFLN
        ENDIF
      ENDIF
      *--B602618,1 RAMY [END]
    ENDIF
  ENDDO  
ELSE
  SET FILTER TO cMatType = lcKeyType AND POMAT = laData[1] AND TRANCD = '4' AND lCancel
  SCAN
    IF SEEK(POFLN.FABRIC+POFLN.COLOR,'FABRIC')
      SELECT FABRIC 
      REPLACE FABRIC.ONORDER WITH FABRIC.ONORDER + IIF(lcKeyType = 'P',(POFLN.nFabTotQty*CONV),-(POFLN.nFabTotQty*CONV))
      SELECT POFLN
      *--B602618,1 RAMY [START]
      IF SEEK(POFLN.FABRIC + POFLN.COLOR + IIF(!EMPTY(POFLN.cWareCode) , POFLN.cWareCode , ;
              POFHDR.cWareCode) + SPACE(10),'FABDYE')
        SELECT FABDYE

        *B804449,1  MHM 10/04/2001 Get CONV from Fabric file Not from FABDYE [Start]
        *REPLACE ONORDER WITH ONORDER +IIF(lcKeyType = 'P' , (POFLN.nFabTotQty*CONV) , - (POFLN.nFabTotQty*CONV))
        REPLACE ONORDER WITH ONORDER +IIF(lcKeyType = 'P' , (POFLN.nFabTotQty*FABRIC.CONV) , - (POFLN.nFabTotQty*FABRIC.CONV))
        *B804449,1  MHM 10/04/2001 [End]

        SELECT POFLN
      ENDIF
      
      *B604856,1 AMH Fix bug of varible lcFabDye not found [Start]
      *IF llDyelot AND !EMPTY(lcFabDye)
      IF llDyelot AND !EMPTY(POFLN.DYELOT)
      *B604856,1 AMH [End]
      
      IF SEEK(POFLN.FABRIC + POFLN.COLOR + IIF(!EMPTY(POFLN.cWareCode) , POFLN.cWareCode , ;
              POFHDR.cWareCode) + POFLN.Dyelot ,'FABDYE')
          SELECT FABDYE

          *B804449,1  MHM 10/04/2001 Get CONV from Fabric file Not from FABDYE In case of Dyelot [Start]
          *REPLACE ONORDER WITH ONORDER +IIF(lcKeyType = 'P', (POFLN.nFabTotQty*CONV) , - (POFLN.nFabTotQty*CONV))
          REPLACE ONORDER WITH ONORDER +IIF(lcKeyType = 'P', (POFLN.nFabTotQty*FABRIC.CONV) , - (POFLN.nFabTotQty*FABRIC.CONV))
          *B804449,1  MHM 10/04/2001 [End]

          SELECT POFLN
        ENDIF
      ENDIF
      *--B602618,1 RAMY [START]
    ENDIF      
  ENDSCAN
ENDIF  
SET FILTER TO
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfQtyRec
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : calculate the quantity recieved for a spacific line
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfQtyRec
PARAMETER lcFab , lcClr

lnAlias = SELECT()
SELECT POFLN

*E500442,1 ABD - change the filter to be seek and scan rest while. [Begin]
*SET FILTER TO cMatType = lcKeyType AND POMAT = laData[1] AND TRANCD <> '1' AND ;
*              FABRIC = lcFab  AND COLOR = lcClr
*              
*SCAN
*  lnRecQty = lnRecQty +nFabTotQty
*ENDSCAN              
*SET FILTER TO 
lnRecQty = 0
IF SEEK(lcKeyType + laData[1] + lcFab + lcClr)
  SCAN REST WHILE cMattype + Pomat + Fabric + Color + Trancd = ;
       lcKeyType + laData[1] + lcFab + lcClr                   ;
       FOR TRANCD <> '1'
    lnRecQty = lnRecQty +nFabTotQty
  ENDSCAN              
ENDIF

*E500442,1 ABD - [End]
SELECT(lnAlias)
RETURN lnRecQty 
*!*************************************************************
*! Name        : lfBrowDet
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : browse the transactios for P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfBrowDet

lcBrowFlds = [cMarker   :H='',]+;
             [Fabric  :7:H='Item':R,]+;
             [Color   :6:H='Color':R,]+;
             IIF(llMultiWH,[cWareCode:6:H='Ware.':R,],[])+;
             [cTranCode=IIF(TranCD $ '234',laTranCodes(VAL(TranCD)-1),''):H='Type':R:10,]+;
             [date:8:H='Date':R,]+;
             IIF(llDyelot, [Dyelot:10:H='Dyelot':R,], [Reference:10:H='Reference':R,])+;
             [TOTQTY:11:H='Quantity':P='9999999.999':R] 
             
SELECT(lcPoTran)
BROWSE FIELDS &lcBrowFlds;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwBrowse();
       TITLE lcTrn_Ttl    ;
       WINDOW (lcWinC34) IN WINDOW (lcWinCh3)
=lfRefresh()     
*!*************************************************************
*! Name        : lpPoSele
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : select a P/O to copy lines from in case of return
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
PROCEDURE lpPoSele
PRIVATE laBrow,lcBrTitle,lcBrFields,lcFilter

lnAlias = SELECT()
lcPOBrow  = laData[1]
lcVenBrow = laData[2]
SELECT POFHDR
*--AAMER (Start)
PRIVATE lnCurRec
lnCurRec = RECNO()
*--AAMER (End)
lcFilter = FILTER()
SET FILTER TO

LOCATE FOR cMatType+Vendor+cFabGrade='P'+lcVenBrow+STR(lnQuaFab,1)
IF !FOUND()
  =gfDialog ("I","No records to display.")
  RETURN
ENDIF
DECLARE laBrow[1]
laBrow =' '
lcBrTitle  = 'Purchase Orders'
lcBrFields = "POMAT   :R :H='PO #':8,"+;
             "Status  :R :H='S':2,"+;	         	           	         
             "Vendor  :R :H='Vendor':10,"+;
	         "Complete:R :H='Complete':8,"+;
  	         "NFABORDER  :R :H='Tot.Qty.':7,"+;	         
             "POTotal :R :H='Amount':10,"+;
             "NFBRECEIVE :R :H='Receive':7,"+;
             "NPO_OPEN   :R :H='Open':7"
llRetValue = gfBrows("FOR cMatType ='P' AND Vendor=lcVenBrow AND cFabGrade=STR(lnQuaFab,1)","POMAT","laBrow",lcBrTitle)
SET FILTER TO &lcFilter

IF llRetValue
  lcPOBrow = laBrow[1]
ELSE
  lcPOBrow = SPACE(06)
ENDIF  
IF !EMPTY(lcPOBrow)
  =lfSelPoLin()
  SELECT(lcPOLine)
  GOTO TOP
  IF EOF()
    =gfModalGen('INM36018B36000','DIALOG',lcPOBrow )
    SELECT (lnAlias)
    RETURN
  ELSE
    IF gfModalGen('INM36019B36001','DIALOG',lcPOBrow) = 2
      PUSH KEY
      ON KEY
      DO (gcScrDir+gcWinAppl+'\POSELE.SPX')
      POP KEY
    ELSE
      REPLACE ALL cStatus WITH 'J'
      =lfApplin()
    ENDIF  
  ENDIF  
ENDIF
*-- AAMER (Start)
IF lnCurRec <= RECCOUNT('POFHdr')
  GOTO lnCurRec IN POFHdr
ENDIF  
*-- AAMER (End)

SELECT (lnAlias)
*!*************************************************************
*! Name        : lfSelPoLin
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : get all the recieved lines for the selected P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfSelPoLin

lnAlias = SELECT()
SELECT *,'' AS cMarker,'' AS cStatus FROM POFLN;
  WHERE cMatType  = 'P' AND ;
        POMAT  = lcPOBrow AND ;
        (Trancd = '2' OR Trancd = '3') ;
 INTO DBF (gcWorkDir+lcPOLine)
SELECT(lcPOLine)
INDEX ON cMatType+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7) TAG lcPOLine
SELECT (lnAlias)
*!*************************************************************
*! Name        : lfBrPOLin
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : browse all the recieved lines for the selected P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfBrPOLin

lnAlias = SELECT()
lcBrowFlds = [cMarker :H=' ' :R :1 :W=.F.,]+;
             [cStatus :H='',]+;
             [Fabric  :7:H='Item':R,]+;
             [Color   :6:H='Color':R,]+;
             IIF(llMultiWH,[cWareCode:6:H='Ware.':R,],[])+;
             [date:8:H='Date':R,]+;
             IIF(llDyelot, [Dyelot:10:H='Dyelot':R,], [Reference:10:H='Reference':R,])+;
             [nFabTotQty:11:H='Quantity':P='9999999.999':R] 
SELECT(lcPOLine)
GOTO TOP
lnMarker = RECNO()
BROWSE FIELDS &lcBrowFlds;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwPOl();
       TITLE lcRec_Ttl    ;
       WINDOW POSELE1 IN WINDOW POSELE
ACTIVATE WINDOW (lcRec_Ttl)
SELECT (lnAlias)
*!*************************************************************
*! Name        : lfvSelec
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : select one or all recieved lines
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvSelec
PARAMETER llAll ,lnRecno

lnAlias = SELECT()
SELECT (lcPOLine)
lnRecno = RECNO()
IF llAll
  REPLACE ALL cStatus WITH "»"
ELSE
  REPLACE cStatus WITH "»"
ENDIF
SHOW WINDOW (lcRec_Ttl) REFRESH SAME
SHOW GET pbUSele  ENABLE 
SHOW GET pbUSeleA ENABLE 
SHOW GET pbSele   DISABLE 
SHOW GET pbSeleA  DISABLE 
GOTO TOP
SCAN
  IF EMPTY(cStatus)
    SHOW GET pbSele  ENABLE   
    SHOW GET pbSeleA ENABLE       
    EXIT
  ENDIF
ENDSCAN
GOTO lnRecno
=lfwPOl()
SELECT (lnAlias)
*!*************************************************************
*! Name        : lfwPOl
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : when for the recievung lines browse
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfwPOl
PRIVATE lnNewRec 

lnAlias = SELECT()
SELECT (lcPOLine)
lnNewRec = RECNO()
REPLACE ALL cMarker WITH lcUnMark
GO IIF(lnNewRec < RECCOUNT(lcPOLine),lnNewRec ,RECCOUNT(lcPOLine))
REPLACE cMarker WITH lcMark
SHOW WINDOW (lcRec_Ttl) REFRESH
SELECT (lnAlias)
*!*************************************************************
*! Name        : lfvUSelec
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : unselect line or all recieving lines 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvUSelec
PARAMETER llAll

lnAlias = SELECT()
SELECT (lcPOLine)
IF llAll
  REPLACE ALL cStatus WITH SPACE(01)
ELSE
  REPLACE cStatus WITH SPACE(01)
ENDIF
SHOW WINDOW (lcRec_Ttl) REFRESH
SHOW GET pbSele  ENABLE 
SHOW GET pbSeleA ENABLE 
SHOW GET pbUSele  DISABLE 
SHOW GET pbUSeleA DISABLE 
SCAN
  IF !EMPTY(cStatus)
    SHOW GET pbUSele  ENABLE 
    SHOW GET pbUSeleA ENABLE 
    EXIT
  ENDIF
ENDSCAN
=lfwPOl()
SELECT (lnAlias)
*!*************************************************************
*! Name        : lfApplin
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : append line for the P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfApplin
PRIVATE lnAlias 

lnAlias = SELECT()
SELECT(lcPOLine)
REPLACE ALL cMatType WITH 'R'
SCAN FOR !EMPTY(cStatus)
  REPLACE cStatus WITH IIF(!EMPTY(cStatus),'A','')
  IF SEEK('R'+laData[1]+&lcPOLine..FABRIC+&lcPOLine..COLOR,(lcDetTmp))
    IF gfModalGen('INM36020B36001','DIALOG',&lcPOLine..FABRIC+ '|'+&lcPOLine..COLOR) = 1
      SELECT(lcDetTmp) 
      REPLACE &lcDetTmp..nFabTOTQTY  WITH &lcDetTmp..nFabTOTQTY + &lcPOLine..nFabTOTQTY 
    ENDIF
    laData[51] = laData[51] + &lcPOLine..nFabTOTQTY 
    laData[55] = laData[55] + &lcPOLine..nFabTOTQTY 

    ladata[39] = ladata[39] + (&lcPOLine..nCost1*&lcPOLine..nFabTotQty)
    ladata[40] = ladata[40] + (&lcPOLine..nCost2*&lcPOLine..nFabTotQty)
    ladata[41] = ladata[41] + (&lcPOLine..nCost3*&lcPOLine..nFabTotQty)
    ladata[42] = ladata[42] + (&lcPOLine..nCost4*&lcPOLine..nFabTotQty) 
  ELSE
    SELECT(lcDetTmp) 
    APPEND BLANK
    lnLastLine = lnLastLine + 1
    REPLACE FABRIC  WITH &lcPOLine..FABRIC,;
            COLOR   WITH &lcPOLine..COLOR,;
            POMAT   WITH laData[1]       ,;
            LineNo  WITH lnLastLine      ,;
            TRANCD  WITH '1'        ,;
            LNEW    WITH lNewLine   ,;
            cMatType   WITH lcKeyType  ,; 
            nFabTotQty  WITH &lcPOLine..nFabTotQty,;
            cStatus WITH 'A'       
            
    REPLACE nCost1   WITH &lcPOLine..nLan_Cost1,;
            nCost2   WITH &lcPOLine..nLan_Cost2,;
            nCost3   WITH &lcPOLine..nLan_Cost3,;
            nCost4   WITH &lcPOLine..nLan_Cost4,;
            neCost1  WITH IIF(lcFrnSign1='*',ROUND(nCost1*laData[24]/lnUnit1,3),ROUND(nCost1/laData[24]/lnUnit1,3)),;
            neCost2  WITH IIF(lcFrnSign2='*',ROUND(nCost2*laData[26]/lnUnit2,3),ROUND(nCost2/laData[26]/lnUnit2,3)),;
            neCost3  WITH IIF(lcFrnSign2='*',ROUND(nCost3*laData[26]/lnUnit2,3),ROUND(nCost3/laData[26]/lnUnit2,3)),;
            neCost4  WITH IIF(lcFrnSign2='*',ROUND(nCost4*laData[26]/lnUnit2,3),ROUND(nCost4/laData[26]/lnUnit2,3)),;            
            PATTERN  WITH &lcPOLine..Pattern,;
            WIDTH    WITH &lcPOLine..Width    
    laData[51] = laData[51] + &lcPOLine..nFabTotQty
    laData[55] = laData[55] + &lcPOLine..nFabTotQty    

    ladata[39] = ladata[39] + (&lcPOLine..nCost1*&lcPOLine..nFabTotQty)
    ladata[40] = ladata[40] + (&lcPOLine..nCost2*&lcPOLine..nFabTotQty)
    ladata[41] = ladata[41] + (&lcPOLine..nCost3*&lcPOLine..nFabTotQty)
    ladata[42] = ladata[42] + (&lcPOLine..nCost4*&lcPOLine..nFabTotQty) 
  ENDIF
  
  *C102525,1 RAE [BEGIN] Append the PO # in POFLN
  IF ASCAN(laEvntTrig , PADR('APPFROMPO',10)) <> 0
    =gfDoTriger('MARTMAT',PADR('APPFROMPO',10))
  ENDIF
  *C102525,1 RAE [END]

ENDSCAN
SELECT(lcDetTmp)
=lfwLine()
SHOW WINDOW (lcDet_Ttl) REFRESH
=lfRefresh()
SELECT(lnAlias)
*!*************************************************************
*! Name        : lfEdtCost
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : to generate total cost for the header
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfEdtCost
PRIVATE lnAlias , lnTotLin, lnLnCst2 ,lnLnCst3, lnLnCst4

lnAlias = SELECT()
SELECT(lcDetTmp)
lnTotLin = 0
SCAN
  lnTotLin = lnTotLin + nFabTotQty
ENDSCAN
lnLnCst2 = IIF(lnTotLin >0,laData[40]/lnTotLin,0)
lnLnCst3 = IIF(lnTotLin >0,laData[41]/lnTotLin,0)
lnLnCst4 = IIF(lnTotLin >0,laData[42]/lnTotLin,0)
SCAN 
  REPLACE nCost2  WITH lnLnCst2 ,;
          nCost3  WITH lnLnCst3 , ;
          nCost4  WITH lnLnCst4 , ;  
          nECost2 WITH IIF(lcFrnSign2='*',ROUND(nCost2*laData[26]/lnUnit2,3),ROUND(nCost2/laData[26]/lnUnit2,3)),;
          nECost3 WITH IIF(lcFrnSign2='*',ROUND(nCost3*laData[26]/lnUnit2,3),ROUND(nCost3/laData[26]/lnUnit2,3)),;
          nECost4 WITH IIF(lcFrnSign2='*',ROUND(nCost4*laData[26]/lnUnit2,3),ROUND(nCost4/laData[26]/lnUnit2,3))
ENDSCAN
lnCost2 = lnLnCst2 
lnCost3 = lnLnCst3
lnCost4 = lnLnCst4 

SELECT(lnAlias)
*!*************************************************************
*! Name        : lfPoTran
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : select transaction lines for the P/O
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfPoTran

lnAlias = SELECT()
SELECT *,'' AS cMarker,'' AS cStatus FROM POFLN;
  WHERE cMatType  = lcKeyType AND ;
        POMAT  = laData[1] AND ;
        Trancd <> '1';
 INTO DBF (gcWorkDir+lcPOTran)
SELECT(lcPOTran)  
INDEX ON cMatType+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7) TAG lcPOTran
SELECT (lnAlias)
*!*************************************************************
*! Name        : lfVenCan
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : update the VENDOR file in case of cancel and uncancel
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfVenCan
PARAMETER llCancel
PRIVATE lnAlias,lcFilter,lnCost,lnVenAmt

lnAlias  = SELECT()
lnVenAmt = 0
lnCost   = 0

SELECT POFLN
lcFilter = FILTER()
IF llCancel
  SET FILTER TO cMatType = lcKeyType AND POMAT = laData[1]
  SCAN
    DO CASE 
      CASE TRANCD = '1'
        lnVenAmt = lnVenAmt + nFabTotQty*nEcost1
        lnCost   = nEcost1
      OTHERWISE
        lnVenAmt = lnVenAmt - nFabTotQty*lnCost
    ENDCASE  
  ENDSCAN
 
ELSE
  SET FILTER TO cMatType = lcKeyType AND POMAT = laData[1] AND TRANCD = '4' AND lCancel
  SCAN
    lnVenAmt = lnVenAmt + nFabTotQty*nEcost1 
  ENDSCAN
ENDIF  
SET FILTER TO &lcFilter
SELECT APVENDOR 
IF SEEK(laData[2])
  DO CASE 
    CASE lcKeyType = 'P'
      REPLACE APVENDOR.nVenOpnPO WITH IIF(llCancel,APVENDOR.nVenOpnPO - lnVenAmt,APVENDOR.nVenOpnPO + lnVenAmt)
    *B604856,1 AMH 08/22/2002 (Begin) Change the type to handle the Return
    *CASE lcKeyType = 'P'
    CASE lcKeyType = 'R'
    *B604856,1 AMH 08/22/2002 (End)
    
      REPLACE APVENDOR.nVenOpnPO WITH IIF(llCancel,APVENDOR.nVenOpnPO + lnVenAmt,APVENDOR.nVenOpnPO - lnVenAmt)
  ENDCASE  
ENDIF
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfGetVar
*! Developer : Haytham El_Sheltawi
*! Date      : 05/14/1997
*! Purpose   : Function to restore the varibles that was saved in the field
*!             UNCMSESS.mComent
*!*************************************************************
*! Called from : lfFndUnCSe()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetVar
PRIVATE lcVarName , lcVarType , lcVarVal , lcVarStr

lcVarStr = UNCMSESS.mComent      && Varible to hold the field UNCMSESS.mComent
FOR lnCount = 1 TO OCCURS('~',lcVarStr)
  lcVarName = SUBSTR(lcVarStr , 1 , AT('|',lcVarStr) - 1)      && The varible name
  lcVarType = SUBSTR(lcVarStr , AT('|',lcVarStr) + 1 , 1)      && The varible Type
  lcVarVal = SUBSTR(lcVarStr , AT('|' , lcVarStr , 2) + 1 ,;
             AT('~' , lcVarStr) - AT('|' , lcVarStr , 2) -1)          && The varible value
  &lcVarName = IIF(lcVarType='N',VAL(lcVarVal) ,;
               IIF(lcVarType='D',CTOD(lcVarVal) ,;
               IIF(lcVarType='L',(lcVarVal='Y') ,lcVarVal)))
  lcVarStr = SUBSTR(lcVarStr,AT('~',lcVarStr)+1)
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfVarStr
*! Developer : Haytham El_Sheltawi
*! Date      : 05/14/1997
*! Purpose   : Function to create a string to be saved in the field
*!             UNCMSESS.mComent
*!*************************************************************
*! Called from : lfvScope()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Array name of a array that hold the varibles name
*!*************************************************************
*! Return      : The created string
*!*************************************************************
*
FUNCTION lfVarStr
PARAMETERS lcArrayNam
PRIVATE lnVarNum , lcReturnSt , lcVarNam

lcReturnSt = ''      && Varible to hold the created string
FOR lnVarNum = 1 TO ALEN(lcArrayNam)
  lcVarNam   = lcArrayNam[lnVarNum]       && The varible name
  lcReturnSt = lcReturnSt + lcVarNam + '|' + TYPE(lcVarNam) + '|' +;
               IIF(TYPE(lcVarNam) = 'N' , ALLTRIM(STR(EVALUATE(lcVarNam))) ,;
               IIF(TYPE(lcVarNam) = 'D' , ALLTRIM(DTOC(EVALUATE(lcVarNam))) ,;
               IIF(TYPE(lcVarNam) = 'L' , IIF(EVALUATE(lcVarNam),'Y','N'), &lcVarNam))) + '~'
ENDFOR    && End of FOR Loop
RETURN lcReturnSt

*!*************************************************************
*! Name      : lpClsScr
*! Purpose   : Close screen P/O.
*!*************************************************************
PROCEDURE lpClsScr

IF SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession,'UNCMSESS')
    SELECT unCmSess
    REPLACE STATUS WITH 'I'
    llContinue = .F.
ENDIF
SELECT (lcBaseFile)
IF laScrMode[3] 
  =lfInit(.T.)
ENDIF
*!*************************************************************
*! Name        : lfCrUnSess
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Add record in UnCmSess file
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfCrUnSess
PARA lcPushBt
lnAlias = SELECT()
SELECT UNCMSESS
IF !SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession)
  IF !SEEK('I'+lcUnsmsPrg+gcUser_id) 
    INSERT INTO UNCMSESS ;
       (Status,cUTranType,cUserId,cSession,cProgram,cCurrScr,dTranDate,cTranTime,mTmpFiles,mComent,ccurrobj) VALUES ;
       ('O',lcUnsmsPrg,gcUser_id,lcSession,lcDialog,'',gdSysDate,TIME(),'lcPOHdr,'+lcPOHdr+','+lcPOHdr+';'+'lcDetTmp,'+lcDetTmp+','+lcDetTmp+';',;
       lfVarStr(@laVariables),'  ')
  ELSE
    REPLACE Status    WITH 'O',;
            cSession  WITH lcSession,;
            dTranDate WITH gdSysDate,;
            cTranTime WITH TIME(),;
            mTmpFiles WITH 'lcPOHdr,'+lcPOHdr+','+lcPOHdr+';'+'lcDetTmp,'+lcDetTmp+','+lcDetTmp+';',;
            mComent   WITH lfVarStr(@laVariables),;
            ccurrobj  WITH lcPushBt
  ENDIF
  =RLOCK('UNCMSESS')
ENDIF
SELECT(lnAlias)
RETURN
* --------------------------------------------------------------- 
*E500442,5 ABD - Comment this Function . [Begin]
*- We didn't need this function again.
*FUNCTION lfvCont
*PRIVATE lnAlias,llFound

*lnAlias = SELECT()
*llFound = .F.
*SELECT(lcDetTmp)
*SCAN 
*  IF SEEK(&lcDetTmp..Fabric+&lcDetTmp..Color,(lcContLin))
*    llFound = .T.
*    EXIT
*  ENDIF
*ENDSCAN
*SELECT(lnAlias)
*RETURN llFound
*E500442,5 ABD - [End]
* --------------------------------------------------------------- 
FUNCTION lfTrapKeys

*B802047 AHM (Start)
*IF WONTOP() = lcDet_Ttl
IF WONTOP() = lcDet_Ttl OR WONTOP() = lcRec_Ttl
*B802047 AHM (End)
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lpTab
  ON KEY LABEL BACKTAB    DO lpShiftTab
  ON KEY LABEL ESC        DO gfCPClose
ENDIF  
* --------------------------------------------------------------- 
PROCEDURE lpShiftTab

*B802047 AHM (Start)
ON KEY LABEL BACKTAB
DO CASE
  CASE WONTOP() = lcDet_Ttl
    ACTIVATE WINDOW (lcFolder)
    _CUROBJ = OBJNUM(ibFolder[2])
  CASE WONTOP() = lcRec_Ttl
    ACTIVATE WINDOW (lcWinC30)
    _CUROBJ = OBJNUM(ibFromBrow)
ENDCASE

if .f.
DO CASE
  CASE !WONTOP(lcDet_Ttl) AND lnActFolder = 2
    ACTIVATE WINDOW(lcDet_Ttl)
  CASE WONTOP(lcDet_Ttl)
    IF laScrMode[2]
      ACTIVATE WINDOW (lcFolder)
     _CUROBJ = OBJNUM(ibFolder(3))
    ELSE
      ACTIVATE WINDOW (lcWinC32)
      IF llEdCost
       _CUROBJ = OBJNUM(lnCost4)
      ELSE
       _CUROBJ = OBJNUM(lnQty)
      ENDIF 
    ENDIF
ENDCASE    
endif
* --------------------------------------------------------------- 
PROCEDURE lpTab

*B802047 AHM (Start)
ON KEY LABEL TAB
DO CASE
  CASE WONTOP() = lcDet_Ttl
    ACTIVATE WINDOW (lcWinC33)
    IF llShowRec AND (laScrMode[1] OR laScrMode[2])
      _CUROBJ = OBJNUM(ibFromRec)
    ELSE
      _CUROBJ = OBJNUM(pbNew)
    ENDIF
  CASE WONTOP() = lcRec_Ttl
    ACTIVATE WINDOW gwcContrl1
ENDCASE

*B802047 AHM (Start)
*DO CASE
*  CASE lnActFolder = 2 AND !WONTOP(lcDet_Ttl) 
*    ACTIVATE WINDOW(lcDet_Ttl)
*  CASE WONTOP(lcDet_Ttl)
*    ACTIVATE WINDOW gwcContrl1
*  OTHERWISE
*    _CUROBJ =  _CUROBJ + 1
*ENDCASE    
*B802047 AHM (End)

* --------------------------------------------------------------- 
FUNCTION lfClearTrap

IF glFromBrow
  =gfStopBrow()
ENDIF
*B802704 (AHM) Start
*IF WONTOP() <> lcDet_Ttl 
IF WONTOP() <> lcDet_Ttl OR WONTOP() <> lcRec_Ttl
*B802704 (AHM) End
   ON KEY LABEL ESC &lcOldEscTrap
   ON KEY LABEL TAB
   ON KEY LABEL BACKTAB
ENDIF

* --------------------------------------------------------------- 
FUNCTION lfwQuality
PRIVATE lcDetRec,lnAlias,llDisable

llDisable = .T.
lcDetRec  = 0
lnAlias = SELECT()

SELECT(lcDetTmp)
IF !EOF()
  lcDetRec = RECNO()
ENDIF
GOTO TOP
IF !EOF() AND lcDetRec > 0
  llDisable = .F.
  GOTO lcDetRec  
ENDIF
SELECT(lnAlias)
RETURN llDisable
* --------------------------------------------------------------- 
FUNCTION lfvQuality

lcQuality = laQuality[lnQuaFab,2]
* --------------------------------------------------------------- 
FUNCTION lfEdtAct
PRIVATE lnAlias,lnTotLin,lnLnCst1,lnLnCst2 ,lnLnCst3, lnLnCst4,;
                         lnELnCst1,lnELnCst2 ,lnELnCst3, lnELnCst4  

IF llGlLink AND EMPTY(lcGl_link)
  =gfModalGen('INM36002B36000','DIALOG','Gl_link code')
  _CUROBJ = OBJNUM(lcGl_link)
  RETURN
ENDIF
lnAlias = SELECT()
SELECT *,RECNO() AS LineNo FROM POFLN ;
  WHERE cMatType+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7) = lcKeyType+laData[1];
    AND TRANCD <> "1" AND TRANCD <> "4";
 INTO DBF (gcWorkDir+lcActTmp)
SELECT(lcActTmp)
lnTotLin = 0
SCAN
  lnTotLin = lnTotLin + nFabTotQty
ENDSCAN

lnELnCst1 = IIF(lnTotLin >0,laData[35]/lnTotLin,0)
lnELnCst2 = IIF(lnTotLin >0,laData[36]/lnTotLin,0)
lnELnCst3 = IIF(lnTotLin >0,laData[37]/lnTotLin,0)
lnELnCst4 = IIF(lnTotLin >0,laData[38]/lnTotLin,0)

IF (laData[23] = gcBaseCurr AND LADATA[25] = gcBaseCurr) OR !llMulCurr
  laData[47] = laData[35]
  laData[48] = laData[36]
  laData[49] = laData[37]
  laData[50] = laData[38]      
  lnLnCst1 = lnELnCst1
  lnLnCst2 = lnELnCst2
  lnLnCst3 = lnELnCst3
  lnLnCst4 = lnELnCst4
ELSE
  lnLnCst1 = IIF(lnTotLin >0,laData[47]/lnTotLin,0)
  lnLnCst2 = IIF(lnTotLin >0,laData[48]/lnTotLin,0)
  lnLnCst3 = IIF(lnTotLin >0,laData[49]/lnTotLin,0)
  lnLnCst4 = IIF(lnTotLin >0,laData[50]/lnTotLin,0)
ENDIF  

SCAN 
  REPLACE nAct_Cost1 WITH lnLnCst1  ,;
          nAct_Cost2 WITH lnLnCst2  ,;
          nAct_Cost3 WITH lnLnCst3  ,;
          nAct_Cost4 WITH lnLnCst4  ,;  
          nEActCost1 WITH lnELnCst1 ,;
          nEActCost2 WITH lnELnCst2 ,;
          nEActCost3 WITH lnELnCst3 ,;
          nEActCost4 WITH lnELnCst4 
ENDSCAN
llSavAct  = .T.
=lfRefresh()
SELECT(lnAlias)
CLEAR READ
* --------------------------------------------------------------- 
FUNCTION lfvCanAct

laData[35] = lcOld35
laData[36] = lcOld36
laData[37] = lcOld37
laData[38] = lcOld38
laData[47] = lcOld47 
laData[48] = lcOld48 
laData[49] = lcOld49 
laData[50] = lcOld50 
CLEAR READ
* --------------------------------------------------------------- 
FUNCTION lfvCanCost

laData[39] = lcOld39 
laData[40] = lcOld40 
laData[41] = lcOld41 
laData[42] = lcOld42
CLEAR READ
* --------------------------------------------------------------- 
*B605724,1 ABD - Close this function , the program call the wrong one. [Begin]
*FUNCTION lfvLink
FUNCTION lfSvLink
*B605724,1 ABD - [End]

PRIVATE lnAlias

lnAlias = SELECT()
IF !EMPTY(lcGl_link) AND !SEEK("04"+lcGl_link, "GL_Link")
  IF !gfGLBrowse('04',@lcGl_link ,@lcLinkDesc) 
    lcGl_link = SPACE(06)
  ENDIF
  SHOW GET lcGl_link
ENDIF
IF !EMPTY (lcGl_link)
    =SEEK("04"+lcGl_link, "GL_Link")
    lcLinkDesc = GL_Link.LinkDesc
    =lfRefrAct()
    *B602633,1 (Begin) Save it.
    laData[56] = lcGl_link
    *B602633,1 (End)
ENDIF
SELECT(lnAlias)
* --------------------------------------------------------------- 
FUNCTION lfvkLink

lcGl_link = PADR("?",6) 
llNothing  = lfvLink()
_CUROBJ = OBJNUM(lcGl_link)
* --------------------------------------------------------------- 
FUNCTION lfActCst
PARAMETER lcCost
PRIVATE lnAlias,lnActAmnt,lnGlAmount,lnFinAmnt,lcRecSess,lcKey,;
        lncost ,lncostbuy,lnissued,lcactcostfld

lnAlias = SELECT()
IF lcCost = '1'
  
  SELECT(lcActTmp)
  *B605724,1 ABD - Remark the next line , We will not add any Gl Entry in 
  *B605724,1 ABD - Case we Modify or update thye actual cost , and also remove the limk 
  *B605724,1 ABD - Code from the actual cost screen , so we will not add
  *B605724,1 ABD - Any sequence number to the squence file for the GL.  [Begin]
  *lcRecSess = gfsequence('GLSESSION')
  *B605724,1 ABD - [End]
  
  SCAN
    *B605724,1 ABD - Remark the next line , We will not add any Gl Entry in 
    *B605724,1 ABD - case we Modify or update thye actual cost , and also remove the limk 
    *B605724,1 ABD - code from the actual cost screen.  [Begin]
    *  lcKey = IIF(EMPTY(cFabric1),Fabric+Color,cFabric1+cColor1)
    *  lnActAmnt = &lcActTmp..NEACTCOST1+&lcActTmp..NEACTCOST2+&lcActTmp..NEACTCOST3+&lcActTmp..NEACTCOST4
    *  =SEEK(lcKeyType+laData[1]+lcKey+Trancd+STR(LineNo_b,7),'POFLN')
    *  lnGlAmount = POFLN.NEACTCOST1+POFLN.NEACTCOST2+POFLN.NEACTCOST3+POFLN.NEACTCOST4
    *  lnGlAmount = IIF(lnGlAmount <> 0,lnGlAmount,POFLN.NELANCOST1+POFLN.NELANCOST2+POFLN.NELANCOST3+POFLN.NELANCOST4)
    *  IF lnGlAmount <> lnActAmnt
    *    lnFinAmnt = lnActAmnt - lnGlAmount
    *    ldDate = gdSysDate
    *    IF llGlLink
    *      DO GlDist WITH lcGl_link, '021', lnFinAmnt, 'MO',laData[1], ldDate,;
    *                     lcGlFYear, lcGlPeriod, 'GLDIST'
    *      REPLACE GLDIST.GLSESSION WITH lcRecSess
    *      DO GlDist WITH lcGl_link, '013', -lnFinAmnt, 'MO',laData[1], ldDate,;
    *                     lcGlFYear, lcGlPeriod, 'GLDIST'
    *      REPLACE GLDIST.GLSESSION WITH lcRecSess
    *    ENDIF  
    *  ENDIF
    *B605724,1 ABD - [End]
  
    SELECT POFLN
    REPLACE nAct_Cost1 WITH &lcActTmp..nAct_Cost1,;
            nAct_Cost2 WITH &lcActTmp..nAct_Cost2,;
            nAct_Cost3 WITH &lcActTmp..nAct_Cost3,;
            nAct_Cost4 WITH &lcActTmp..nAct_Cost4,;  
            nEActCost1 WITH &lcActTmp..nEActCost1,;
            nEActCost2 WITH &lcActTmp..nEActCost2,;
            nEActCost3 WITH &lcActTmp..nEActCost3,;
            nEActCost4 WITH &lcActTmp..nEActCost4 
  ENDSCAN  
ELSE
  =gfOpenFile(gcDataDir+'MATINVJL',gcDataDir+'MATINVJL','SH')
  =gfOpenFile(gcDataDir+'BOMCOST',gcDataDir+'BOMCOST','SH')  
  IF llMFIstall
    =gfOpenFile(gcDataDir+'CTKTBOM',gcDataDir+'CTKTBOM','SH')  
    =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')  
    =gfOpenFile(gcDataDir+'MMFGORDH',gcDataDir+'MMFGORDH','SH')  
  ENDIF  
  
  IF llPOIstall
    =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')  
  ENDIF
  
  SELECT(lcActTmp)
  SET RELATION TO IIF(EMPTY(CFABRIC1),FABRIC+COLOR,CFABRIC1+CCOLOR1) INTO Fabric ADDITIVE
  SET RELATION TO IIF(EMPTY(cfabric1),fabric+color,cfabric1+ccolor1)+cwarecode+dyelot+crsession INTO Matinvjl ADDITIVE  
  IF llMultiWH
    SET RELATION TO IIF(EMPTY(CFABRIC1),FABRIC+COLOR,CFABRIC1+CCOLOR1)+CWARECODE INTO FABDYE ADDITIVE
  ENDIF  
  SELECT MATINVJL
  SET RELATION TO PADR(CFABRIC,19,' ')+CCOLOR+CWARECODE+CDYELOT+CRSESSION+CISESSION INTO BOMCOST ADDITIVE
  SELECT BOMCOST
  IF llMFIstall
    SET RELATION TO CIMTYP+CTKTNO INTO CTKTBOM  ADDITIVE
    SET RELATION TO CTKTNO        INTO CUTTKTH  ADDITIVE
    SET RELATION TO CTKTNO        INTO MMFGORDH ADDITIVE
  ENDIF  
  IF llPOIstall
    SET RELATION TO CTKTNO INTO POSHDR  ADDITIVE
  ENDIF
  SELECT(lcActTmp)  
  SCAN
    lncost    = (nAct_Cost1 + nAct_Cost2 + nAct_Cost3 + nAct_Cost4) / Fabric.conv 
    lncostbuy = (nAct_Cost1 + nAct_Cost2 + nAct_Cost3 + nAct_Cost4)
    lnissued  = 0
    lcKey = IIF(EMPTY(cFabric1),Fabric+Color,cFabric1+cColor1)
    =SEEK(lcKeyType+laData[1]+lcKey+Trancd+STR(LineNo_b,7),'POFLN')        
    lnGlAmount = POFLN.NEACTCOST1+POFLN.NEACTCOST2+POFLN.NEACTCOST3+POFLN.NEACTCOST4
    SELECT MatInvJl
    SCAN REST WHILE cFabric + cColor + cWareCode + cDyelot + cRsession = ;
                    &lcActTmp..Fabric + &lcActTmp..Color +;
                    &lcActTmp..cWareCode + &lcActTmp..Dyelot + &lcActTmp..cRsession
      IF !EMPTY(cIsession)
        REPLACE MatInvJl.nRecCost   WITH IIF(lnGlAmount = 0,MatInvJl.nUnitCost,MatInvJl.nRecCost) ,;
                MatInvJl.nUnitCost  WITH lnCost,;
                MatInvJl.nUntcstBuy WITH lnCostBuy
                
         IF MatInvJl.cTranType = '4' .AND. !EOF('BOMCOST')
           REPLACE BOMCOST.nUnitaCst WITH ROUND(lncost,3) ,;
                   BOMCOST.nTotaCst  WITH BOMCOST.nTotaCst + ROUND(lncost * MatInvJl.nIssued,3),;
                   BOMCOST.capInvNo  WITH ladata[1]
           IF llMFIstall
             DO CASE
               CASE ctktbom.cimtyp = 'M'
                 SELECT CUTTKTH
                 lcactcostfld = 'NACT_COST'+ALLTRIM(BOMCOST.cbomtype)
                 REPLACE &lcactcostfld WITH EVALUATE(lcactcostfld) - BOMCOST.nTotCst + BOMCOST.nTotaCst
               CASE ctktbom.cimtyp = 'T'
                 SELECT MMFGORDH
                 lcactcostfld = 'NACT_COST'+ALLTRIM(BOMCOST.cbomtype)
                 REPLACE &lcactcostfld WITH EVALUATE(lcactcostfld) - BOMCOST.nTotCst + BOMCOST.nTotaCst
               OTHERWISE AND llMPOIstall
                 SELECT POSHDR
                 lcactcostfld = 'NACT_COST'+ALLTRIM(BOMCOST.cbomtype)
                 REPLACE &lcactcostfld WITH EVALUATE(lcactcostfld) - BOMCOST.nTotCst + BOMCOST.nTotaCst
             ENDCASE
           ENDIF  
         ENDIF
         SELECT MatInvJl
         lnissued = lnissued + MatInvJl.nIssued
      ENDIF
    ENDSCAN
    SELECT(lcActTmp)
    IF !EOF()
      GO RECNO()
    ENDIF
    IF lnIssued < MatInvJl.nReceived
      lnunissued = MatInvJl.nReceived - lnissued
      lcKey = IIF(EMPTY(cFabric1),Fabric+Color,cFabric1+cColor1)
      =SEEK(lcKeyType+laData[1]+lcKey+Trancd+STR(LineNo_b,7),'POFLN')        
      lnGlAmount = POFLN.NEACTCOST1+POFLN.NEACTCOST2+POFLN.NEACTCOST3+POFLN.NEACTCOST4
      REPLACE MatInvJl.nRecCost   WITH IIF(lnGlAmount = 0,MatInvJl.nUnitCost,MatInvJl.nRecCost) ,;
              MatInvJl.nUnitCost  WITH lncost,;
              MatInvJl.nUntCstBuy WITH lncostbuy
      IF nAct_cost1 <> pofln.nAct_cost1 .OR. ;
         nAct_cost2 <> pofln.nAct_cost2 .OR. ;
         nAct_cost3 <> pofln.nAct_cost3 .OR. ;
         nAct_cost4 <> pofln.nAct_cost4
         lnoldisscst = pofln.nAct_cost1 + pofln.nAct_cost2 + ;
         pofln.nAct_cost3 + pofln.nAct_cost4
        IF Fabric.onhand > 0
          lnAvCstBuy = (FABRIC.navecstbuy*FABRIC.ONHAND+;
                        lnUnIssued*(lnCostBuy-lnOldIssCst))/FABRIC.ONHAND
          REPLACE Fabric.navecstbuy WITH ROUND(lnavcstbuy,3) ,;
                  Fabric.nFAve_Cost WITH ROUND(lnavcstbuy/Fabric.conv,3)
        ENDIF
        IF llMultiWH AND Fabdye.onhand > 0
          lnAvCstBuy = (FABDYE.navecstbuy*FABDYE.ONHAND+;
                       lnUnIssued*(lnCostBuy-lnOldIssCst))/FABDYE.ONHAND
          REPLACE Fabdye.nAveCstBuy WITH ROUND(lnavcstbuy,3) ,;
                  Fabdye.nFAve_Cost WITH ROUND(lnavcstbuy/Fabric.conv,3)
        ENDIF
        
     ENDIF
   ENDIF
   SELECT POFLN
   REPLACE nAct_Cost1 WITH &lcActTmp..nAct_Cost1,;
           nAct_Cost2 WITH &lcActTmp..nAct_Cost2,;
           nAct_Cost3 WITH &lcActTmp..nAct_Cost3,;
           nAct_Cost4 WITH &lcActTmp..nAct_Cost4,;
           nEActCost1 WITH &lcActTmp..nEActCost1,;
           nEActCost2 WITH &lcActTmp..nEActCost2,;
           nEActCost3 WITH &lcActTmp..nEActCost3,;
           nEActCost4 WITH &lcActTmp..nEActCost4
 ENDSCAN
 SET RELATION TO
 SELECT BOMCOST
 SET RELATION TO
ENDIF 
SELECT(lnAlias)
* --------------------------------------------------------------- 

FUNCTION lfcrtUncmp

SELECT POFHDR
= AFields(laFileStru)
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'nSteps'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 2
laFileStru[lnNewFld,4] = 0
=gfCrtTmp(lcPOHdr,@laFileStru,[cMatType+POMAT],lcPOHdr)
SELECT (lcPOHdr)
APPEND BLANK 
=RLOCK()
REPLACE cMatType WITH lcScrType
UNLOCK
*----------------------------------------------------------------
  SELECT POFLN
  = AFields(laFileStru)
  = AFields(laContStru)  
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cMarker'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0
  
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'lNew'
  laFileStru[lnNewFld,2] = 'L'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0
  
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'nRecno'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 10
  laFileStru[lnNewFld,4] = 0

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cStatus'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0

  =gfCrtTmp(lcDetTmp,@laFileStru,[cMatType+POMAT+FABRIC+COLOR],lcDetTmp)

  =gfCrtTmp(lcActTmp,@laFileStru)

  *E500442,1 ABD - we will not Call this function again. [Begin]
  *IF lcKeyType $ 'PC' 
  *  =gfCrtTmp(lcConTLin,@laContStru,[FABRIC+COLOR],lcConTLin)  
  *ENDIF
  *E500442,1 ABD - we will not Call function again. [Begin]
********************************************************
*B602633,1 (Begin) There are 2 functions with the same name, and this is the wrong one, remark
*FUNCTION lfvLink
*FUNCTION lfvLink
*B605724,1 ABD - call this function , this function is the correct one. [Begin]
FUNCTION lfvLink
*B605724,1 ABD - [End]
*B602633,1 (End)
PRIVATE lnAlias

lnAlias = SELECT()
*B605724,1 ABD - remark the next line to browse from the browse button. [Begin]
*IF !EMPTY(laData[56]) AND !SEEK("05"+laData[56], "GL_Link")
*lcLink = laData[56]
IF llBrowse .OR. (!EMPTY(laData[56]) AND !SEEK("05"+laData[56], "GL_Link"))
IF llBrowse
  lcLink = '?'
ELSE
  lcLink = laData[56]
ENDIF
llBrowse = .F.
*B605724,1 ABD - [End]

  IF !gfGLBrowse('05',@lcLink ,@lcLinkDesc) 
    laData[56] = lcOldLink
  ELSE
    laData[56] = lcLink 
  ENDIF
ENDIF
IF EMPTY(laData[56])
  laData[56] = 'DEFDEF'
ENDIF
SHOW GET laData[56]
SELECT(lnAlias)
********************************************************
*B602633,1 (Begin) There are 2 functions with the same name, and this is the wrong one, remark
*FUNCTION lfvkLink
FUNCTION lfSvkLink
*B602633,1 (End)

laData[56] = PADR("?",6) 
llNothing  = lfvLink()
_CUROBJ = OBJNUM(laData[56])
********************************************************
FUNCTION lfwLink

lcOldLink = laData[56]
********************************************************
FUNCTION lfvClose

IF !CHECKPRD(gdSysDate,'lcGLFYear','lcGLPeriod','PO',.T.)
  =gfModalGen('INM36115B36000','DIALOG')
  RETURN
ENDIF
*B119869,1 ABD - Define the closing date before call the close screen. [Begin]
ldDate = gdSysdate
*B119869,1 ABD - [End]

DO (gcScrDir+gcWinAppl+'\MAPRC6.SPX')


SHOW GETS
********************************************************
FUNCTION lfvOkCls

IF laData[55] > 0
  *--B602618,1 RAMY [START]
  *IF gfModalGen('INM36113B36001','DIALOG',ALLTRIM(STR(laDATA[55]))+'|'+'P/O') = 2
  IF gfModalGen('INM36113B36001','DIALOG',ALLTRIM(STR(laDATA[55],11,3))+'|'+'P/O') = 2
  *--B602618,1 RAMY [END]
    CLEAR READ
    RETURN
  ENDIF
ENDIF
=lfCalcCost()
=lfvCancel()
llContCls = .T.
CLEAR READ
RETURN
********************************************************
FUNCTION lfvCanCls

llContCls = .F.
CLEAR READ
RETURN
********************************************************
FUNCTION lfvCancel

ladata[54]  = ladata[54]  + ladata[55]
ladata[55]  = 0
lcStatus = 'Close' 
lnStatus = 6
=lfVenCan(.T.)
REPLACE POFHDR.STATUS     WITH 'L',;
        POFHDR.npo_open   WITH laData[55],;
        POFHDR.nfabcancel WITH laData[54]
=lfCanFab(.T.)        
=lfCanLine(.T.)        
SELECT POFLN
APPEND FROM (gcWorkDir+lcDetTmp) for trancd = '4' AND lCancel
SELECT (lcBaseFile)
*!*************************************************************
*! Name        : lfCalcCost
*! Developer   : Samah Wilson Kirollos
*! Date        : 08/17/98
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : To calculate the actual cost
*!               
*!               
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfCalcCost
PRIVATE lnAlias,lnInvPrice,lnInvTax,lnInvFreight,lnInvQuota,;
        lnTotLin1,lnTotLin2,lnTotLin3,lnTotLin4,lcRecSess

STORE 0 TO lnInvPrice,lnInvTax,lnInvFreight,lnInvQuota,lnTotLin1,;
           lnTotLin2,lnTotLin3,lnTotLin4           
lnAlias = SELECT()

SELECT APINVTKT
IF SEEK('L'+laData[1])
  SCAN WHILE CIMTYP+CTKTNO+STR(LINENO,6)+CRSESSION = 'L'+laData[1];
       AND EMPTY(CRSESSION) AND NAPAPLAMNT <> 0
       IF SEEK(cVendCode+capInvNo+capVilNo,'APVINVDT')
         DO CASE
           CASE APVINVDT.CBOMTYPE = '1'
             lnInvPrice = lnInvPrice + NAPAPLAMNT
           CASE APVINVDT.CBOMTYPE = '2'
             lnInvFreight = lnInvFreight + NAPAPLAMNT
           CASE APVINVDT.CBOMTYPE = '3'
             lnInvTax = lnInvTax + NAPAPLAMNT
           CASE APVINVDT.CBOMTYPE = '4'
             lnInvQuota = lnInvQuota + NAPAPLAMNT
         ENDCASE
       ENDIF
  ENDSCAN
  SELECT POFLN
  SEEK(lcScrType+laData[1])
  SCAN WHILE CMATTYPE+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7) = ;
             lcScrType+laData[1] FOR TRANCD <> '4'
    lnTotLin1 = lnTotLin1 + nFabTotQty*nCost1
    lnTotLin2 = lnTotLin2 + nFabTotQty*nCost2
    lnTotLin3 = lnTotLin3 + nFabTotQty*nCost3
    lnTotLin4 = lnTotLin4 + nFabTotQty*nCost4
  ENDSCAN
  *B603654,1 (Begin) Take into consideration not to divide by 0.
  *lnCost1 = lnInvPrice/lnTotLin1
  *lnCost2 = lnInvFreight/lnTotLin2
  *lnCost3 = lnInvTax/lnTotLin3
  *lnCost4 = lnInvQuota/lnTotLin4
  lnCost1 = IIF(lnTotLin1=0,0,lnInvPrice/lnTotLin1)
  lnCost2 = IIF(lnTotLin2=0,0,lnInvFreight/lnTotLin2)
  lnCost3 = IIF(lnTotLin3=0,0,lnInvTax/lnTotLin3)
  lnCost4 = IIF(lnTotLin4=0,0,lnInvQuota/lnTotLin4)
  *B603654,1 (End)
  SEEK(lcScrType+laData[1])
  SCAN WHILE CMATTYPE+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7) = ;
             lcScrType+laData[1] FOR TRANCD <> '4'
    REPLACE NACT_COST1 WITH NACT_COST1+lnCost1,;
            NACT_COST2 WITH NACT_COST2+lnCost2,;
            NACT_COST3 WITH NACT_COST3+lnCost3,; 
            NACT_COST4 WITH NACT_COST4+lnCost4
  ENDSCAN
ENDIF
lnLanded = laData[31]+laData[32]+laData[33]+laData[34]
lnActual = laData[35]+laData[36]+laData[37]+laData[38]

IF lnLanded <> lnActual AND llGlLink
  
  *B119869,1 ABD - [Begin]
  IF TYPE('ldDate') = 'U'
    ldDate = gdSysDate
  ELSE
    = CHECKPRD(ldDate,'lcGLFYear','lcGLPeriod','PO',.T.)  
  ENDIF
  *B119869,1 ABD - [End]
  lcRecSess = gfsequence('GLSESSION')
  *B603504,1 (Begin) Change the transaction type sent by GLDIST procedure for closing matreial PO from 'MO' to 'MC'.
  *DO GlDist WITH laData[29],'013',(lnLanded - lnActual),'MO',;
                 laData[1],ldDate,lcGlFYear, lcGlPeriod, 'GLDIST'
  *REPLACE GLDIST.GLSESSION WITH lcRecSess
  *DO GlDist WITH laData[29],'021',(lnActual-lnLanded),'MO',;
                 laData[1],ldDate,lcGlFYear, lcGlPeriod, 'GLDIST'
  *REPLACE GLDIST.GLSESSION WITH lcRecSess
  *B603834,1 WAB (START)- the link code filed is laData[56] Not laData[29]
  *DO GlDist WITH laData[29],'013',(lnLanded - lnActual),'MC',;
                 laData[1],ldDate,lcGlFYear, lcGlPeriod, 'GLDIST'
  *REPLACE GLDIST.GLSESSION WITH lcRecSess
  *DO GlDist WITH laData[29],'021',(lnActual-lnLanded),'MC',;
                 laData[1],ldDate,lcGlFYear, lcGlPeriod, 'GLDIST'
  *REPLACE GLDIST.GLSESSION WITH lcRecSess
  DO GlDist WITH laData[56],'013',(lnLanded - lnActual),'MC',;
                 laData[1],ldDate,lcGlFYear, lcGlPeriod, 'GLDIST'
  REPLACE GLDIST.GLSESSION WITH lcRecSess
  DO GlDist WITH laData[56],'021',(lnActual-lnLanded),'MC',;
                 laData[1],ldDate,lcGlFYear, lcGlPeriod, 'GLDIST'
  REPLACE GLDIST.GLSESSION WITH lcRecSess
  *B603834,1 WAB (END) 
  *B603504,1 (End)
ENDIF
SELECT(lnAlias)
********************************************************
FUNCTION lfvLC
PRIVATE lcbrfields,lnAlias

DECLARE laLC[2]
laLC =' '
lnAlias = SELECT()
IF !EMPTY(laData[20]) AND EMPTY(laData[2])
  =gfModalGen('INM36002B36000','DIALOG','Vendor')
  laData[20] = SPACE(06) 
  RETURN
ELSE
  IF !EMPTY(laData[20]) AND (!SEEK('M'+laData[20],'LC') OR LC.VENDOR <> laData[2])
    *-- Set browse fields for browsing vendors.
    lcbrfields = "VENDOR :9:H='Vendor' , CLCNO  :30:H='LC #',"+;
                 "Expire :17:H='Expire Date'"
    
    *-- Assign browse window name for vendors.
    lcFile_ttl = "LC "
    SELECT LC
    *-- Browse the available LC'S vendor.
    LOCATE FOR CLCTYPE = 'M' AND VENDOR = laData[2]
    IF !FOUND()
      =gfModalGen('INM36117B36000','DIALOG',laData[2])
      laData[20]  = SPACE(06)
      laData[21]  = {}
      RETURN
    ELSE
      IF gfBrows('FOR CLCTYPE ="M" AND VENDOR = laData[2]' ,'cLCNo,Expire', 'laLC',lcFile_ttl)
        laData[20]  = LC.cLCNo
        laData[21]  = LC.Expire
      ELSE
        laData[20]  = SPACE(06)
        laData[21]  = {}
      ENDIF
    ENDIF  
  ENDIF
ENDIF
IF !EMPTY(laData[20]) AND SEEK('M'+laData[20],'LC') AND EMPTY(LC.CCURRCODE) AND llMulCurr
  =gfModalGen('INM36118B36000','DIALOG')
  laData[20]  = SPACE(06)
  laData[21]  = {}
  RETURN
ENDIF
IF !EMPTY(laData[20]) AND llMulCurr AND;
         ((laData[23] <> LC.CCURRCODE) OR (laData[25] <> LC.CCURRCODE))
  =gfModalGen('INM36119B36000','DIALOG',LC.CCURRCODE)
  laData[20]  = SPACE(06)
  laData[21]  = {}
  RETURN
ENDIF   
IF !EMPTY(laData[20])
  laData[21]  = LC.Expire
ENDIF      
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfFabInWar
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : check if the fabric/color is assigned to the warehouse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfFabInWar
PARAMETER lcfFabric,lcfColor,lcfWare

IF !SEEK(lcfFabric+lcfColor+lcfWare,'FabDye')
*--HDM B602331,1 We Need This Check For Both return and PO[start]
*  IF lcKeyType = 'R'
*    lnChoice=gfModalGen('TRM36049B36000','ALERT',ALLTRIM(lcfFabric)+'/'+ALLTRIM(lcfColor)+'|'+ALLTRIM(lcfWare))
*    RETURN .F.                  
*  ELSE
    *-- AMER (Start)
    *lnChoice=gfModalGen('TRM36049B36001','ALERT',ALLTRIM(lcfFabric)+'/'+ALLTRIM(lcfColor)+'|'+ALLTRIM(lcfWare)) 
    IF llMultiWH
      lnChoice=gfModalGen('TRM36049B36001','ALERT',ALLTRIM(lcfFabric)+'/'+ALLTRIM(lcfColor)+'|'+ALLTRIM(lcfWare)) 
    ELSE
      lnChoice = 1
    ENDIF
    *-- AMER (End)
*  ENDIF                    
*--HDM B602331,1 We Need This Check For Both return and PO[end]
  IF lnChoice = 1
    DO gpAdFabWar WITH lcfFabric, lcfColor,SPACE(10), lcfWare
  ELSE
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.

************************************************

FUNCTION lfReceived

IF !llShowRec
  llShowRec = .T.
ELSE
  llShowRec = .F.
  SET MARK OF BAR 5 OF _OPTIONPOP TO .F.
ENDIF

IF llShowRec
  IF laScrMode[2]
    SHOW GETS WINDOW (lcWinC32) DISABLE ONLY
  ENDIF
  HIDE WINDOW (lcWinC32) SAME
  =lfRecBrow()
ELSE
  RELEASE WINDOW (lcRec_Ttl)
  SHOW WINDOW (lcWinC32) SAME
ENDIF

********************************************

FUNCTION lfRecBrow

PRIVATE lnCurAlias,lcBrowFlds 
SET MARK OF BAR 5 OF _OPTIONPOP TO .T.

lnCurAlias = SELECT(0)

SELECT POFLN
*E301295 (Start)
*lcBrowFlds = [cMarker=IIF(RECNO('POFLN')=lnRecRec,'>',''):H='':1,cRsession:R:H='Session',]+;
             [Tran=IIF(cFabGrade='1',IIF(TranCd='4','First quality (Canceled)','First quality'),IIF(cFabGrade='2','Second quality',IIF(cFabGrade='3','Damaged',''))):R:H='Receiving type':25,]+;
             [Item=IIF(TranCd $ '24',Fabric,cFabric1) :R:H='Item' :7,]+;
             [Clr =IIF(TranCd $ '24',Color ,cColor1 ) :R:H='Color':6,]+;
             IIF(llDyelot,[Dyelot:R:H='Dyelot':10,],'')+;
             IIF(llMultiWH,[cWareCode:R:H='Location':11,],'')+;
             [nFabTotQty:R:13:H='Quantity':P='9999999.999',]+;
             [Amount=nCost1*nFabTotQty:18:R:H='Amount':P='9999999999999.999']
             
*B803016,1 (Begin) Calculate Amount on the Landed cost not the default cost.
*lcBrowFlds = [cMarker=IIF(RECNO('POFLN')=lnRecRec,'>',''):H='':1,cRsession:R:H='Session',]+;
             [Tran=IIF(cFabGrade='1',IIF(TranCd='4','First quality (Canceled)','First quality'),IIF(cFabGrade='2','Second quality',IIF(cFabGrade='3','Damaged',''))):R:H='Receiving type':18,]+;
             [Date :R:H='Date' :10,] +;
             [Item=IIF(TranCd $ '24',Fabric,cFabric1) :R:H='Item' :7,]+;
             [Clr =IIF(TranCd $ '24',Color ,cColor1 ) :R:H='Color':6,]+;
             IIF(llDyelot,[Dyelot:R:H='Dyelot':10,],'')+;
             IIF(llMultiWH,[cWareCode:R:H='Location':11,],'')+;
             [nFabTotQty:R:13:H='Quantity':P='9999999.999',]+;
             [Amount=nCost1*nFabTotQty:16:R:H='Amount':P='9999999999999.999']
             
lcBrowFlds = [cMarker=IIF(RECNO('POFLN')=lnRecRec,'>',''):H='':1,cRsession:R:H='Session',]+;
             [Tran=IIF(cMatType='R','Returned',IIF(cFabGrade='1',IIF(TranCd='4','First quality (Canceled)','First quality'),IIF(cFabGrade='2','Second quality',IIF(cFabGrade='3','Damaged','')))):R:H='Receiving type':18,]+;
             [Date :R:H='Date' :10,] +;
             [Item=IIF(TranCd $ '24',Fabric,cFabric1) :R:H='Item' :7,]+;
             [Clr =IIF(TranCd $ '24',Color ,cColor1 ) :R:H='Color':6,]+;
             IIF(llDyelot,[Dyelot:R:H='Dyelot':10,],'')+;
             IIF(llMultiWH,[cWareCode:R:H='Location':11,],'')+;
             [nFabTotQty:R:13:H='Quantity':P='9999999.999',]+;
             [Amount=nLan_Cost1*nFabTotQty:16:R:H='Amount':P='9999999999999.999']
*B803016,1 (End)             
*E301295 (End)
=lfTrapKeys()

*C102525,1 RAE [BEGIN]
*BROWSE FIELDS &lcBrowFlds ;
       FOR (cMatType+POMat+Fabric+Color+TranCd = ;
           'P'+ laData[1] + &lcDetTmp..Fabric + &lcDetTmp..Color ;
           AND TranCd <> '1');
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE;
       TITLE lcRec_Ttl;
       VALID :F lfvRecWBro();
       WHEN  lfwRecWBro();
       WINDOW (lcWinC35) IN WINDOW (lcWinCh3)

lcFlt = " cMatType+POMat+Fabric+Color+TranCd = ;
          'P'+ laData[1] + &lcDetTmp..Fabric + &lcDetTmp..Color ;
          AND TranCd <> '1' "

IF ASCAN(laEvntTrig , PADR('ADDRETLNE',10)) <> 0
  =gfDoTriger('MAPRCAM',PADR('ADDRETLNE',10))
ENDIF

BROWSE FIELDS &lcBrowFlds ;
       FOR &lcFlt ;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE;
       TITLE lcRec_Ttl;
       VALID :F lfvRecWBro();
       WHEN  lfwRecWBro();
       WINDOW (lcWinC35) IN WINDOW (lcWinCh3)
*C102525,1 RAE [END]

=lfwRecWBro()

SELECT (lnCurAlias)

********************************************

FUNCTION lfwRecWBro

lnRecRec = RECNO('POFLN')
glFromBrow = .T.
IF WEXIST(lcRec_Ttl)
  SHOW WINDOW (lcRec_Ttl) REFRESH SAME
ENDIF  

********************************************

FUNCTION lfvRecWBro

*IF WONTOP() # (lcRec_Ttl)
  glFromBrow = .T.
  = gfStopBrow()
*ENDIF

********************************************

PROCEDURE lpActRec

IF llShowRec
  ACTIVATE WINDOW(lcRec_Ttl)
ELSE
  ACTIVATE WINDOW(lcWinC32)
  _CUROBJ = OBJNUM(lcFabric)
  
ENDIF



*!*************************************************************
*! Name      : lfFabWar
*! Developer : Hossam El Etreby
*! Date      : 04/29/1999
*! Purpose   : check if the fabric/color is assigned to the warehouse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Due to B602400
*!*************************************************************
FUNCTION lfFabWar
PARAMETER lcfFabric,lcfColor,lcfWare , llUpdate
PRIVATE lnTotQty , lnOnOrder , lnRec

STORE 0  TO lnTotQty , lnOnOrder , lnRec

IF !SEEK(lcfFabric+lcfColor+lcfWare,'FabDye')
  IF llMultiWH
    IF llAsk
      lnChoice = gfModalGen('QRM36135B36001','DIALOG',ALLTRIM(lcfWare)) 
      llAsk = .F.
      IF lnChoice = 1
        llAssignWH = .T.
      ELSE
        llAssignWH = .F.
      ENDIF
    ENDIF
  ENDIF
  IF llAssignWH AND llUpdate
    
    DO gpAdFabWar WITH lcfFabric, lcfColor,SPACE(10), lcfWare
    SELECT POFLN
    
    SCAN FOR cmattype+pomat+fabric+color+trancd = lcScrType + laData[1] + lcfFabric + lcfColor + '1'
      lnTotQty = lnTotQty + POFLN.NFABTOTQTY
    ENDSCAN

    SCAN FOR cmattype+pomat+fabric+color+trancd = lcScrType + laData[1] + lcfFabric + lcfColor + '2'
      lnRec = lnRec + POFLN.NFABTOTQTY
    ENDSCAN
    
    lnOnOrder = lnTotQty - lnRec

    SELECT FABDYE
    = SEEK(lcfFabric+lcfColor + lcOldLocat , 'FabDye')
    REPLACE ONORDER WITH ONORDER - lnOnOrder
    FLUSH
    = SEEK(lcfFabric+lcfColor+laData[3],'FabDye')
    REPLACE ONORDER WITH ONORDER + lnOnOrder
    FLUSH
    
  ENDIF
ENDIF

RETURN (llAssignWH)



*!************************************************************************
*! Name      : lfMaPoNum
*! Developer : WAB - Walid A Wahab
*! Date      : 08/08/2000
*! Purpose   : Edit the MA Po number manualy  if the system was setup to do so.
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfMaPoNum()
*!*************************************************************
*E500349,1  
*!*************************************************************
FUNCTION lfMaPoNum
PRIVATE lcPONum,lcTitle
DO CASE
  CASE lcScrType= 'P'
    lcTitle  = 'Material PO.'
  CASE lcScrType= 'C'
    lcTitle  = 'Material Contracts'
  CASE lcScrType= 'R'
    lcTitle  = 'Material PO Returns'
ENDCASE
lcPONum   = SPACE(6)
lcSctTtle = 'Number'
lcScrText = 'Please enter the '+lcTitle
lcHdEsc1= ON('KEY','ESC')
ON KEY LABEL ESC 
DO (gcScrDir+"POGNum.SPX")
ON KEY LABEL ESC &lcHdEsc1
RETURN (lcPONum)

*!************************************************************************
*! Name      : lfMaPoNum
*! Developer : WAB - Walid A Wahab
*! Date      : 08/08/2000
*! Purpose   : Validate MA P/O no in entered manualy.
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvPoNum()
*!*************************************************************
*E500349,1  
*!*************************************************************
FUNCTION lfvPoNum

llRet=.T.

*----lcTitle ('Material Purchase Orders'/'Material Contracts'/'Material Purchase Order Returns')
IF EMPTY(lcPONum)
  *-You cannot leave the lcTitle number empty.
  =gfModalGen('TRM34021B34000','DIALOG',lcTitle)
  llRet=.F.
ENDIF
IF llRet AND LEN(ALLTRIM(lcPONum)) < 6
  *-lcPOType+" number must be six digits.
  =gfModalGen('TRM34022B34000','DIALOG',lcTitle)
  llRet=.F.
ENDIF
IF llRet AND SEEK(lcKeyType+lcPONum,"POFHDR")
  *-This "+lcPOType+" number is already exist.
  =gfModalGen('TRM34023B34000','DIALOG',lcTitle)
  llRet=.F.
ENDIF
RETURN llRet

*-- End Of lfvPonum.
*!*************************************************************
*! Name      : lfvLine
*! Developer : Abdou Elgendy. (ABD)
*! Date      : 10/22/2001
*! Purpose   : Validate the price for selected 
*!           : Vendor and faric - color.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : = lfvLine()
*!*************************************************************
*E500442,1 ABD - [Begin]
FUNCTION lfvLine
PARAMETER lcParam
PRIVATE lnAlias , llReturn , lcExpr
STORE '' TO lnAlias , lcExpr
STORE .F. TO llReturn

*-- Check for the Parameter.
IF TYPE('lcParam') $ 'UL'
  lcParam  = ''
ENDIF

DO CASE
  CASE lcParam = 'CPRICECUR'
    lcExpr = '&lcContHdr..CPRICECUR = laData[23]'
  CASE lcParam = 'CDUTYCUR'
  lcExpr = '&lcContHdr..CDUTYCUR  = laData[25]'
  OTHERWISE
  lcExpr = '.T.'
ENDCASE

*-- lcContHdr  , lcContLin
lnAlias = SELECT (0)
SELECT (lcContLin)

*-- Scan for specific Condation.
IF SEEK (lcFabric+lcColor+'C')
  SCAN REST WHILE Fabric+Color+cMattype+Pomat+Trancd = lcFabric+lcColor+'C'
    *-- Check on lines.
    IF Vendor = laData[2] .AND. &lcContHdr..ENTERED  <= laData[9]  ;
      .AND. &lcContHdr..STATUS = 'O' ;
      .AND. &lcContHdr..COMPLETE => laData[10] .AND. &lcExpr
      *-- Return true.
      llReturn = .T.
      EXIT
    ENDIF
  ENDSCAN
ENDIF

SELECT (lnAlias)

RETURN llReturn
*-- End Of lfvLine.
*E500442,1 ABD - [End]
*!*************************************************************

*!*************************************************************
*! Name        : lfwVenFab
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old vendor fabric field in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfwVenFab

lcOldVFab = lcVenFab

*!*************************************************************
*! Name        : lfvVenFab
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the vendor fabric field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfvVenFab

IF !EMPTY(lcVenFab) .AND. lcQuality <> '1'
  =gfModalGen('INM36196B36000','DIALOG')
  lcVenFab = lcOldVFab
  RETURN
ENDIF

PRIVATE lnAlias
IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT(0)
SELECT VENDMATL
lcOrder = SET('ORDER')
SET ORDER TO TAG VENMAT1
IF !EMPTY(lcVenFab) .AND. lcVenFab<>lcOldVFab
  IF SEEK(laData[2]+lcVenFab)
    lcFabric = FABRIC
  ELSE
    llBrowse = .F.
    DIMENSION laTempData[2]
    STORE '' TO laTempData
    
    lcFile_Ttl = 'Supplier Items'
    
    lcBrFields = "VENDOR  :H='Vendor'       ,"+;
                 "CVENFAB :H='Supplier Item',"+;
                 "FABRIC  :H='Fabric'"
    
    =AriaBrow([laData(2)],lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CVENFAB,FABRIC','laTempData')
    lcVenFab = IIF(EMPTY(laTempData[1]),SPACE(10),laTempData[1])
    lcFabric = IIF(EMPTY(laTempData[2]),lcFabric,laTempData[2])
  ENDIF
ENDIF
SET ORDER TO &lcOrder.
SHOW GET lcFabric
SHOW GET lcVenFab
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfwVenColr
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : take the old vendor color field in a variable
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfwVenColr

lcOldVColr = lcVenColr

*!*************************************************************
*! Name        : lfvVenColr
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the vendor color field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfvVenColr

IF !EMPTY(lcVenColr) .AND. lcQuality <> '1'
  =gfModalGen('INM36196B36000','DIALOG')
  lcVenColr = lcOldVColr
  RETURN
ENDIF

PRIVATE lnAlias
IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT(0)
SELECT VENDMATL
lcOrder = SET('ORDER')
SET ORDER TO TAG VENMAT
IF !EMPTY(lcVenColr) .AND. lcVenColr<>lcOldVColr
  IF SEEK(laData[2]+lcVenFab+lcVenColr)
    lcFabric = FABRIC
    lcColor  = COLOR
  ELSE
    llBrowse = .F.
    DIMENSION laTempData[4]
    STORE '' TO laTempData
    
    lcFile_Ttl = 'Supplier Items/Colors'
    
    lcBrFields = "VENDOR   :H='Vendor'        ,"+;
                 "CVENFAB  :H='Supplier Item ',"+;
                 "CVENCOLR :H='Supplier Color',"+;
                 "FABRIC   :H='Fabric'        ,"+;
                 "COLOR    :H='Color'"
    
    =AriaBrow([laData(2)+IIF(EMPTY(lcVenFab),'',lcVenFab)],lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,;
              gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CVENFAB,CVENCOLR,FABRIC,COLOR','laTempData')
    lcVenFab  = IIF(EMPTY(laTempData[1]),SPACE(10),laTempData[1])
    lcVenColr = IIF(EMPTY(laTempData[2]),SPACE(10),laTempData[2])
    lcFabric  = IIF(EMPTY(laTempData[3]),lcFabric,laTempData[3])
    lcColor   = IIF(EMPTY(laTempData[4]),lcColor,laTempData[4])
  ENDIF
ENDIF
SET ORDER TO &lcOrder.
SHOW GET lcFabric
SHOW GET lcColor
SHOW GET lcVenFab
SHOW GET lcColor
SELECT (lnAlias)
=lfvColor()

*!*************************************************************
*! Name        : lfvKVenFab
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the vendor fabric field from the key field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvKVenFab

lcVenFab = PADR('?',10)
=lfvVenFab()
_CUROBJ = OBJNUM(lcVenColr)

*!*************************************************************
*! Name        : lfvKVenClr
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the vendor color field from the key field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
FUNCTION lfvKVenClr

lcVenColr = PADR('?',10)
=lfvVenColr()

*!*************************************************************
*! Name        : lfReCalDlv
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Recalculate the delivry date when change the entered date
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfReCalDlv

PRIVATE lnAlias,lcOrder
lnAlias = SELECT(0)
IF llVenRef
  SELECT VENDMATL
  lcOrder = SET('ORDER')
  SET ORDER TO TAG VENMAT
ENDIF
SELECT (lcDetTmp)
LOCATE
IF !EOF() .AND. gfModalGen('INM36197B36001','DIALOG')=1
  SCAN
    IF llVenRef .AND. !EMPTY(CVENFAB) .AND. !EMPTY(CVENCOLR) .AND.;
      SEEK(laData[2]+CVENFAB+CVENCOLR,'VENDMATL') .AND. !EMPTY(VENDMATL.LEADTIME)
      REPLACE DDELIVDATE WITH laData[9] + VENDMATL.LEADTIME
    ELSE
      IF SEEK(FABRIC+COLOR,'FABRIC') .AND. !EMPTY(FABRIC.LEADTIME)
        REPLACE DDELIVDATE WITH laData[9] + FABRIC.LEADTIME
      ELSE
        REPLACE DDELIVDATE WITH laData[10]
      ENDIF
    ENDIF
  ENDSCAN
ENDIF
IF llVenRef
  SET ORDER TO &lcOrder. IN VENDMATL
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfUpdRev
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Update the revision data
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfUpdRev

PRIVATE llHasRev,lnAlias,llOpenFld,lnI,lcOldAlias,lcNewAlias
STORE .F. TO llHasRev,llOpenFld
lnAlias = SELECT(0)

DECLARE laSkipFld[18]
laSkipFld[01] = 'CADD_USER'
laSkipFld[02] = 'DADD_DATE'
laSkipFld[03] = 'CLOK_USER'
laSkipFld[04] = 'DLOK_DATE'
laSkipFld[05] = 'CADD_TIME'
laSkipFld[06] = 'CLOK_TIME'
laSkipFld[07] = 'LLOK_STAT'
laSkipFld[08] = 'STATUS'
laSkipFld[09] = 'LASTLINE'
laSkipFld[10] = 'POTOTAL'
laSkipFld[11] = 'CFABGRADE'
laSkipFld[12] = 'CREVNO'
laSkipFld[13] = 'CEDIT_USER'
laSkipFld[14] = 'DEDIT_DATE'
laSkipFld[15] = 'CEDIT_TIME'
laSkipFld[16] = 'CADD_VER'
laSkipFld[17] = 'CEDT_VER'
laSkipFld[18] = 'LINENO'

IF !USED('SYDFIELD')
  =gfOpenFile(gcSysHome+'SYDFIELD','CFLD_NAME','SH')
  llOpenFld = .T.
ENDIF

IF laScrMode[4]
  laData[57] = laData[1]
ELSE
  SELECT(lcDetTmp)
  lcOldAlias = 'POFLN'
  lcNewAlias = lcDetTmp
  SCAN
    IF SEEK(lcKeyType+laData[1]+Fabric+Color+'1','POFLN')  && Modifay record
      =lfComFile('C')
    ELSE                                                   && New     record
      IF !llHasRev
        llHasRev = .T.
        =lfAddRevHd()
      ENDIF
      =lfAddRevLn('A','NFABTOTQTY')
    ENDIF
  ENDSCAN
  IF SEEK(lcKeyType+laData[1],'POFLN')
    SELECT POFLN
    SCAN REST WHILE CMATTYPE+POMAT+FABRIC+COLOR+TRANCD = lcKeyType+laData[1]
      IF !SEEK(lcKeyType+laData[1]+Fabric+Color,lcDetTmp)  && Delete  record
        IF !llHasRev
          llHasRev = .T.
          =lfAddRevHd()
        ENDIF
        =lfAddRevLn('D','NFABTOTQTY')
      ENDIF
    ENDSCAN
  ENDIF
  DECLARE laSkipFld[31]
  laSkipFld[19] = 'NCOST1'
  laSkipFld[20] = 'NCOST2'
  laSkipFld[21] = 'NCOST3'
  laSkipFld[22] = 'NCOST4'
  laSkipFld[23] = 'NFABORDER'
  laSkipFld[24] = 'NFBRECEIVE'
  laSkipFld[25] = 'NFABDAMAGE'
  laSkipFld[26] = 'NFABCANCEL'
  laSkipFld[27] = 'NPO_OPEN'
  laSkipFld[28] = 'NECOST1'
  laSkipFld[29] = 'NECOST2'
  laSkipFld[30] = 'NECOST3'
  laSkipFld[31] = 'NECOST4'
  IF TYPE('laUsrFields[1,1]') = 'C' .AND. !EMPTY(laUsrFields[1,1])
    FOR lnI = 1 TO ALEN(laUsrFields,1)
      lnSkipCnt = ALEN(laSkipFld) + 1
      DECLARE laSkipFld[lnSkipCnt]
      laSkipFld[lnSkipCnt] = ALLTRIM(laUsrFields[lnI,1])
    ENDFOR
  ENDIF
  lcOldAlias = 'POFHDR'
  lcNewAlias = lcPOHdr
  SELECT (lcPOHdr)
  GATHER FROM laData FIELDS &lcScFields MEMO
  =lfComFile('H')
ENDIF
IF llOpenFld
  USE IN SYDFIELD
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfAddRevHd
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Add record to the revision header file
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfAddRevHd

PRIVATE lnRevNo,lcRevNo,lnAlias
lnRevNo = VAL(SUBSTR(laData[57],8))+1
lcRevNo = STRTRAN(STR(lnRevNo,2,0),' ','0')
laData[57] = laData[1]+'-'+lcRevNo
lnAlias = SELECT(0)
SELECT REVHDR
APPEND BLANK
REPLACE CREVNO   WITH laData[57],;
        DREVDATE WITH gdSysDate ,;
        CUSER_ID WITH gcUser_Id
=gfAdd_Info('REVHDR')
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfAddRevLn
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Add record to the revision line file
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfAddRevLn
PARAMETERS lcRevType,lcFieldNam,llUsrField,lnFldPos

PRIVATE lcRevType,lcFieldNam,lnAlias,lcOldVal,lcNewVal
lnAlias = SELECT(0)
SELECT REVLN
APPEND BLANK
=SEEK(lcFieldNam,'SYDFIELD')
REPLACE CREVNO    WITH laData[57],;
        CREVTYPE  WITH lcRevType ,;
        FABRIC    WITH IIF(lcRevType='H','',lfUpdRevFl('FABRIC'))  ,;
        COLOR     WITH IIF(lcRevType='H','',lfUpdRevFl('COLOR'))   ,;
        CVENFAB   WITH IIF(lcRevType='H','',lfUpdRevFl('CVENFAB')) ,;
        CVENCOLR  WITH IIF(lcRevType='H','',lfUpdRevFl('CVENCOLR')),;
        CFLD_HEAD WITH SYDFIELD.CFLD_HEAD
lcOldVal = lfUpdRevFl(lcFieldNam)
lcNewVal = IIF(lcRevType$'AD','',IIF(llUsrField,laUsrFields[lnFldPos,6],;
               EVALUATE(lcNewAlias+'.'+lcFieldNam)))
DO CASE
  CASE SYDFIELD.CDATA_TYP = 'C'
    REPLACE COLDVAL   WITH lcOldVal,;
            CNEWVAL   WITH lcNewVal
  CASE SYDFIELD.CDATA_TYP = 'N'
    REPLACE COLDVAL   WITH STR(lcOldVal,30,3),;
            CNEWVAL   WITH IIF(lcRevType$'AD','',STR(lcNewVal,30,3))
  CASE SYDFIELD.CDATA_TYP = 'D'
    REPLACE COLDVAL   WITH DTOC(lcOldVal),;
            CNEWVAL   WITH IIF(lcRevType$'AD','',DTOC(lcNewVal))
  CASE SYDFIELD.CDATA_TYP = 'L'
    REPLACE COLDVAL   WITH IIF(lcOldVal,'Yes','No'),;
            CNEWVAL   WITH IIF(lcRevType$'AD','',IIF(lcNewVal,'Yes','No'))
ENDCASE
=gfAdd_Info('REVLN')
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfUpdRevFl
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Update filed in the revision line file
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfUpdRevFl
PARAMETERS lcFldName

lcRet = EVALUATE(EVALUATE('lc'+IIF(lcRevType='A','New','Old')+'Alias')+'.'+lcFldName)

RETURN lcRet

*!*************************************************************
*! Name        : lfComFile
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/09/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Comper between fields of two files
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfComFile
PARAMETERS lcRevType

PRIVATE lcRevType,lnAlias
lnAlias = SELECT(0)
SELECT (lcOldAlias)
=AFIELDS(laFields)
FOR lnI = 1 TO ALEN(laFields,1)
  IF ASCAN(laSkipFld,laFields[lnI,1]) <> 0
    LOOP
  ENDIF
  IF TYPE(lcNewAlias+'.'+laFields[lnI,1]) <> laFields[lnI,2]
    LOOP
  ENDIF
  IF EVALUATE(lcNewAlias+'.'+laFields[lnI,1]) <> EVALUATE(lcOldAlias+'.'+laFields[lnI,1])
    IF !llHasRev
      llHasRev = .T.
      =lfAddRevHd()
    ENDIF
    =lfAddRevLn(lcRevType,laFields[lnI,1])
  ENDIF
ENDFOR
IF lcRevType = 'H' .AND. TYPE('laUsrFields[1,1]') = 'C' .AND. !EMPTY(laUsrFields[1,1])
  FOR lnI = 1 TO ALEN(laUsrFields,1)
    IF laUsrFields[lnI,6] <> EVALUATE(lcOldAlias+'.'+laUsrFields[lnI,1])
      IF !llHasRev
        llHasRev = .T.
        =lfAddRevHd()
      ENDIF
      =lfAddRevLn(lcRevType,laUsrFields[lnI,1],.T.,lnI)
    ENDIF
  ENDFOR
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfvPreview
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/12/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Preview the revision report
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfvPreview

=lfRevRep('S')

*!*************************************************************
*! Name        : lfvPrint
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/12/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Print the revision report
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfvPrint

=lfRevRep('P')

*!*************************************************************
*! Name        : lfRevRep
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/12/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Preview/Print the revision report
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfRevRep
PARAMETERS lcAct

PRIVATE lcTempRep,lnAlias
lcTempRep = gfTempName()
lnAlias = SELECT(0)
SELECT REVLN
=AFIELDS(laFields)
CREATE CURSOR (lcTempRep) FROM ARRAY laFields
INDEX ON CREVNO+CREVTYPE TAG (lcTempRep) OF (lcTempRep)

IF SEEK(laData[1],'REVHDR')
  SELECT REVHDR
  SCAN REST WHILE CREVNO = laData[1]
    IF SEEK(CREVNO,'REVLN')
      SELECT REVLN
      SCAN REST WHILE CREVNO+CREVTYPE = REVHDR.CREVNO
        SCATTER MEMVAR
        INSERT INTO (lcTempRep) FROM MEMVAR
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF

SELECT (lcTempRep)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  USE IN (lcTempRep)
  SELECT (lnAlias)
  RETURN
ENDIF

SET RELATION TO CREVNO INTO REVHDR
llOpenUser = .F.
IF !USED('SYUUSER')
  =gfOpenFile(gcSysHome+'SYUUSER','CUSER_ID','SH')
  llOpenUser = .T.
ENDIF
SELECT REVHDR
SET RELATION TO CUSER_ID INTO SYUUSER
SELECT (lcTempRep)
gcDevice = IIF(lcAct = 'P','PRINTER','SCREEN')
lcOgPlatForm = 'WINDOWS'
lcTime = TIME()          && Variable to hold the Time
DEFINE WINDOW WINPRINT FROM 1,1 TO 2,2
ACTIVATE WINDOW WINPRINT
lcCons = SET('CONS')
SET CONS OFF
DO (gcRepHome +  'MAREPORT.APP') WITH 'MAREVREP', .T.
SET CONS &lcCons
RELEASE WINDOW WINPRINT
USE IN (lcTempRep)
IF llOpenUser
  USE IN SYUUSER
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name        : lfRevType
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/12/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : Print the revision type
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102780,1 AMH
FUNCTION lfRevType
PARAMETERS lcRevType

DO CASE
  CASE lcRevType $ 'CH'
    RETURN 'Changed'
  CASE lcRevType = 'A'
    RETURN 'Add'
  CASE lcRevType = 'D'
    RETURN 'Removed'
ENDCASE

*!*************************************************************
*! Name      : lfvDlivDat
*! Developer : Abdou Elgendy. (ABD)
*! Date      : 03/30/2003
*! Purpose   : Validate the Dliv date.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : = lfvDlivDat()
*!*************************************************************
*B607085,1 ABD - [Begin]
FUNCTION lfvDlivDat
PRIVATE lnAlias

lnAlias = SELECT(0)

IF EMPTY(ldDelvDate)
  *-- Message text   : ğ field cannot be empty. 
  *-- Message Number : 36002
  *-- Button text    : OK
  *-- Button Number  : 00000
  =gfModalGen('QRM36002B00000','DIALOG','Delivery date ')
  ldDelvDate = ldOldValue
ELSE
  IF BETWEEN(ldDelvDate,laData[10],laData[10])
    IF llDlivDate
      SELECT(lcDetTmp)
      REPLACE DDELIVDATE WITH ldDelvDate
    ENDIF
  ELSE
    *-- Message text   : ğ should be between ğ & ğ
    *-- Message Number : 00351 
    *-- Button text    : OK
    *-- Button Number  : 00000
    =gfModalGen('QRM00351B00000','DIALOG','Delivery date ' +'|'+ 'Entered date' + '|' + 'Complete date.')
    ldDelvDate = ldOldValue
  ENDIF
ENDIF

SELECT(lnAlias)

*-- End OF lfvDlivDat
*B607085,1 ABD - [Begin]
*!*************************************************************

*!*************************************************************
*! Name        : lfvIData58
*! Developer   : AHMED MAHER (AMH)
*! Date        : 04/10/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the MRP field from the key field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102819,1 AMH
FUNCTION lfvIData58

laData[58] = PADR('?',6)
=lfvData58()

*!*************************************************************
*! Name        : lfvData58
*! Developer   : AHMED MAHER (AMH)
*! Date        : 04/10/2003
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : validate the MRP field
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Calls       : 
*!*************************************************************
*C102819,1 AMH
FUNCTION lfvData58

PRIVATE lnAlias
IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT(0)
SELECT MRPHDR
IF !EMPTY(laData[58]) .AND. !SEEK(laData[58])
  llBrowse = .F.
  DIMENSION laTempData[1]
  STORE '' TO laTempData
  
  lcFile_Ttl = 'Supplier Items'
  
  lcBrFields = "CMRP :H='MRP #'"
  
  =AriaBrow([],lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CMRP','laTempData')
  laData[58] = IIF(EMPTY(laTempData[1]),SPACE(6),laTempData[1])
ENDIF
SELECT (lnAlias)


