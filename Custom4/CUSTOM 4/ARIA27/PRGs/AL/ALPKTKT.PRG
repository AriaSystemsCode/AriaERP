***************************************************************
*: Program file  : ALPKTKT.PRG
*: Program desc. : Picking Ticket
*: Module        : Allocation
*:         System: Aria Apparel System
*:      Developer: IHAB 
*:*************************************************************
*: Calls 		 : ALPKTKT.SPR
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : Pick Ticket No. ==> lcpPikTkt
*:*************************************************************
*: Modifications      :
*:
*B602546,1 IHB 02/21/1999 if the selected pick ticket already has packing 
*B602546,1 IHB            list, then the user can not release this pick 
*B602546,1 IHB            ticket.
*IHB 04/08/1999 Add parameter to allow calling for piktkt screen
*E301273,1 Reham On 06/22/1999
*E301273,1 Add status Pulled in the Picking Tickets Status..
*C200089,1 Reham On 07/22/1999  *** Begin ***
*C200089,1 1- When Picking Ticket status changed to "Pulled" if piktkt is picked completely,
*C200089,1    Call process Id to execute special function.
*C200089,1 2- When Picking Ticket status changed to "Pulled" if piktkt is picked partialy,
*C200089,1    Call process Id to execute special function.
*C200089,1 3- When Picking Ticket Status change from "Pulled" to "Open", Call process
*C200089,1    Id to execute special function.
*B603084,1 RAMY On 08/30/1999
*B603084,1 When the customer is not active should not Proceed dealing with
*B603084,1 him.
*B603242,1 MAN 10/27/99 Optomize In : Screen Opening (Select Mode), 
*B603242,1 MAN          Selcting Pktkt (View Mode), and Edit Mode.
*B603286,1 RAMY 11/17/1999 Fix the bug of 'File does not exist ' when 
*B603286,1 useing the user defined fields button
*B603299,1 KHM 11/24/99 Fix the bug of not enabling the user to change
*B603299,1              the qty if its a one size scale in the screen.
*B603318,1 TAK 12/05/99 Added a custom process to not allow to release
*B603318,1              of modify an adornment pick ticket.
*B802757,1 BWA 12/05/1999 Fix the bug of not display the ticket date in the P/T screen.
*B802727,1 BWA 12/07/1999 Fix the bug of showing the original ship to address even if
*                         the user put alternate ship to address in the sales order screen.
*B803304,1 MAB 05/28/2000 Update ordhdr with Book and Open Quantities.
*B603506,1 ABD 06/12/2000 Increase amount sizes in Browse fields to fit currencies.
*B603506,1 ABD            [BookAmt,ShipAmt,CancelAmt,Openamt]
*B803402,1 HBG 08/07/2000 When zeroing out the picked quantities of one or more pick
*B803402,1                ticket lines, such lines should be removed from the pick ticket.
*B603575,1 HBG 03/09/2000 Fix bug of variable "lcBrTtl' not found.
*B803633,1 HBG 03/09/2000 Fix bug of not Displaying the size scale for styles if 
*B803633,1                PIKTKT completed or canceled  
*C102283,1 ABD 06/05/2001 Add the status of "K" for "Packed" to the pick ticket screen. 
*B804372,1 HBG 30/08/2001 Fix bug of The allocated qty doesn't get updated properly. 
*C200330,1 HBG 05/08/2002 Add trigger to GMA to Add column to detail browse to display 
*C200330,1                Pack Information.
*B605953,1 ASH 05/28/2002 Fix the bug of displaying wrong ship to address.
*B606714,1 ALB 12/03/2002 Fix the multisession screen
*C102782,1 ABD 12/31/2002 Add Trigger for CAM05.
*B606999,1 ASH 02/26/2003 Fix bug 'End of file encountered'.
*B607240,1 ABD 06/18/2003 Fix bug of variable "lcBrTtl' not found.
*B122346,1 BWA 04/01/2004 Fix the bug of showing Store data neither from distribution data.
*C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*B122561,1 NNA 04/29/2004 Fix bug that the all Cancelled Piktkt Have Status 'Pulled' In the Browse
*B123702,1 TMI 09/14/2004 Fix a problem that if ALPKTKT screen is open it prevents any other screen to be activated 
*B124651,1 NNA 11/02/2004 fix bug that if you changed the style.Desc in the SO this Change not appear
*B124651,1 NNA            in the piktkt Screen So I Changed in the Screen to get the desc. from the ordline
*B124651,1 NNA            file or the lc_Tmpordl File instead of Style File 
*B127347,1 TMI 05/03/2005 remove piktkt refrence from the POCRTNMF file

*E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt.
*B131018,1 OMS 07/02/2006 not allow to force allcation where forcing is No And Costing Method Is FIFO OR LIFO 
*B039660,1 NNA 02/14/2006 Enhancement in Bin Location system(CP#037816)
*B608067,1 NNA 04/30/2007 Fix bug that if you opened the Sales order screen(SO) then opened the Pick ticket Screen (AL) and
*B608067,1 NNA            you selected a piktkt then navigated to the SO screen then returned back you'll get an error
*B608067,1 NNA            that 'Desc1 not found'
*:*****************************************************************************
*IHB Add parameter [start]
PARAMETERS lcpPikTkt
*-IF the program is running stand alone or to view passed style.
llExCalled = (PARAMETERS()=1) OR TYPE('lcpPikTkt')='C'
*IHB [end]
*-- Variable declaration
*-- lc_TmpOrdL temp name for order lines temp file , in case order state 
*-- is 'Open' or 'on Hold' and edit mode , editing applied on lc_TmpOrdL
*-- lcScFields is the screen fields (aria3.prg make scatter and gather
*-- on this char.)
*-- lcWinChX is the child window 


STORE '' TO lc_TmpOrdL, lcScFields, lcWinCh1, lcWinCh2, ; 
		    lcWinCh3, lcWinCh30, lcWinCh31 , lcWinCh32, lcNdxExp, lcForCond

STORE '' TO lcOrderBr,lcFilter        && macro substitutes for br. lines
STORE 0  TO lnMarker                  && lnmarker points to the rec. in br. reg.
                                      && which is - currently - in the edit reg.

*-- This variable is set (.T.) from the browsing button validations
*-- It indicates that the user wishes to browse..
STORE .F. TO llBrowse

*-- to allow undo msg.
llCUpDate  = .T.

*-- ini for altstydesc
STORE '' TO lcStyle, lcStyDesc, lcStyHdr, lcStyMsk, lcStyleWid
STORE SPACE(60) TO lcAltStyDsc

*B608067,1 NNA 04/30/2007 (Begin) Initiallize a variable with name (Desc1) and width (60) as the ordline.desc1 filed
DESC1=SPACE(60)
*B608067,1 NNA (End)

*-- laScrMode is the screen mode array (laScrMode[1] = .T. means Select mode,
*-- laScrMode[2] means View ,laScrMode[3] means Edit,laScrMode[4] means Add)
DECLARE laStatus[6], laKeyField[2,4], laScrMode[5]

*E301273,1 Reham On 06/22/1999   *** Begin ***
*E301273,1 Define the status array before gfSetup to save the array elements.
DECLARE laPkTktStatus[3,2]
*E301273,1 Reham On 06/22/1999   *** End   ***

*-- lafoldwinds array that hold the count of used folders and it's titles 
*-- lcfolder    variable that hold the temp name of the folder window
*-- lcfoldprnt  variable that hold the name of the parent window of the folder
*               window which is the base screen 
*-- lnLastFold  variable that hold the last activated folder befor changing
*               the folder
*-- lnActFolder variable that hold the number of activated folder
DIMENSION lafoldwinds[2,2]   && we have two folders : header and detail
STORE SPACE(0) TO lcfolder,lcfoldprnt
STORE 0 TO lnLastFold
STORE 0 TO lnActFolder

*-- All codes in the piktkt are say fields cuz modifying them is N/A.
STORE ' ' TO lcTerms, lcShipVia, lcSpcInst, lcSeason, lcDivision

*-- lcStyHdr var. label  for style
*-- lnStyLngth Var. hold the length of the style.  
*-- furthermore , style stru. is variable (e.g.: code+color , 
*-- code+scale , code+color+scale ..) 
STORE ''  TO lcStyHdr
STORE 0   TO lnStyLngth, lnStyleWid

*-- lcSizeX ini. label for sizes of style (they are stored in scale.dbf)
lcSize1   = 'Size1'
lcSize2   = 'Size2'
lcSize3   = 'Size3'
lcSize4   = 'Size4'
lcSize5   = 'Size5'
lcSize6   = 'Size6'
lcSize7   = 'Size7'
lcSize8   = 'Size8'

*-- laPik    : Array to hold the allo. qtys. for validation.
*-- laQty    : Array to hold the Ord. qtys. for validation.
*-- lnTotPik : Total picked qty.
*-- lnTotQty : Total ordered qty.
*-- laOldPik : Array to hold the old allo. qtys.
*--            if modified to substitute in style/stydye.
DECLARE laPik[8] , laQty[8], laOldPik[8] 
laPik    = 0
laQty    = 0
laOldPik = 0

STORE 0 TO lnTotPik, lnTotQty 

*-- laOldVal : Old value
STORE '' TO laOldVal

*-- lcPickStat in the msg. cond. of dyelot !!!
STORE 'DISABLE' TO lcPickStat

*-- For bill to & ship to flds. 
DECLARE laAddress[6,3]
STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5

*-- Prtflag indicates that the piktkt is printed 
STORE ' ' TO lcPrtflag 

*-- For special save and del. proc. 
*-- (The del. uses aria3's own msg. handler , if needed another then write gfcpdelete local) 
DECLARE   laDefProc[10,1] 
laDefProc      = .T.
*laDefProc[7]  = .F.     && Cancel piktkt procedure(lpDelScr)
laDefProc[9]   = .F.     && Save procedure(lpSavScr)
*laDefProc[10]  = .F.     && close procedure(lpClsScr)

*-- lcWinKey   variable that hold temp name to be used for defining the
*              key fields window
STORE SPACE(0) TO lcWinKey

*-- initial value for qtys., piks., style, altstyle, dyelots (for multi session)
STORE 0 TO m.Qty1, m.Qty2, m.Qty3, m.Qty4, m.Qty5, m.Qty6, m.Qty7, m.Qty8, m.TotQty 
STORE 0 TO m.Pik1, m.Pik2, m.Pik3, m.Pik4, m.Pik5, m.Pik6, m.Pik7, m.Pik8, m.TotPik
STORE '' TO m.Style, m.AltStyle, m.Dyelot

*B602546,1 IHB check whether the pick ticket has a packing list or not.
*B602546,1 IHB llSekPak -> Flag to hold if any of the selected pick ticket has a packing list
STORE .F. TO llSekPak
*B602546,1 IHB end

*B603575,1 HBG 03/09/2000 Decleare "lcBrTtl' before "gfSetup" to be saved[Begin]
*-- lcBrTtl is the title of the browse window
lcBrTtl   = 'Picking Ticket Lines'
*B603575,1 HBG 03/09/2000 [End]

*B606714,1 ALB Fix the multisession screen [begin]
lcPkTktStatus = ''  &&Declare befgore gfsetup to restore the value
*B606714,1 ALB Fix the multisession screen [end]

*-- Call gfSetup : open files, and define the screen variables (e.g. lcWindTitl)
IF !gfSetup()     
  RETURN
ENDIF


*E130074,1 EIH 01/02/2006 Check if the field name 'CSNDPKT' exist in the PIKTKT file. [Begin]
lnAlias = SELECT(0) 
STORE .F. TO llSndPkFld
SELECT PIKTKT
FOR I = 1 TO FCOUNT('PIKTKT')
  IF UPPER(FIELD(I))='CSNDPKT'
    llSndPkFld = .T.
	EXIT
  ENDIF 
ENDFOR
SELECT (lnAlias)
*E130074,1 EIH 01/02/2006 [End]


*C200330,1 HBG 08/05/2002 Trigger to GMA [Begin]
llGMAPack = ASCAN(laEvntTrig,PADR('ADDBAR',10)) <> 0
IF llGMAPack
  STORE llGMAPack TO llFirst,llByLine
ENDIF  
*C200330,1 [END]

*C102284,1 ABD  Update the Piktkt file with status 'K' Packed. [Begin]
llPakStats = .F.
IF ASCAN(laEvntTrig , PADR('LLPAKST',10)) <> 0
  llPakStats = .T.
ENDIF
*C102284,1 ABD  [End]

*B606714,1 ALB Fix the multisession screen [begin]
*-- laData is a standard array holds the screen fields . 
*DECLARE laData[8]
*STORE '' TO laData
*B606714,1 ALB Fix the multisession screen [end]

*B603575,1 HBG 03/09/2000  comment this variable , and decleare it before "gfSetup" [Begin]
*-- lcBrTtl is the title of the browse window
*lcBrTtl   = 'Picking Ticket Lines'
*B603575,1 HBG 03/09/2000 [End]

*-- lcKeyBmp stores the location/file of the bmp.
lcKeyBmp  = gcBMPHome + 'EXTKEY.BMP'

*-- Remove button 
lcRemove  = gcBmpHome + "rem1.bmp"

*-- laPkTktStatus array of pick ticket status .
*-- this array allow in edit mode cycling only bet. Open and on Hold 
*-- and in the same time ASCAN is made in the obj. validation to update
*-- ladata[2] val.
*-- laStatus represents various options for lcPkTktStatus
*-- lcPkTktStatus is the screen object of the pick ticket
*E301273,1 Reham On 06/22/1999   *** Begin ***
*E301273,1 Increase the status array with Pulled in the Picking Tickets Status..
*DECLARE laPkTktStatus[2,2]

*C102283,1 ABD Add the status of "K" for "Packed" to the pick ticket screen. [Begin]
IF llPakStats
  DECLARE laPkTktStatus[4,2]
ELSE
  DECLARE laPkTktStatus[3,2]
ENDIF
*C102283,1 ABD [End]

*E301273,1 Reham On 06/22/1999   *** End   ***
laPkTktStatus[1,1]  = 'Open'
laPkTktStatus[1,2]  = 'O'
laPkTktStatus[2,1]  = 'On Hold'
laPkTktStatus[2,2]  = 'H'
*E301273,1 Reham On 06/22/1999   *** Begin ***
*E301273,1 Add the status data for the new added row.
laPkTktStatus[3,1]  = 'Pulled'
laPkTktStatus[3,2]  = 'P'

*C102283,1 ABD Add the status of "K" for "Packed" to the pick ticket screen. [Begin]
IF llPakStats
  laPkTktStatus[4,1]  = 'Packed'
  laPkTktStatus[4,2]  = 'K'
ENDIF  
*C102283,1 ABD [End]

*E301273,1 Add new row for the Pulled Picking Tickets.
*DECLARE laStatus[5]

*C102283,1 ABD Add the status of "K" for "Packed" to the pick ticket screen. [Begin]
IF llPakStats
  DECLARE laStatus[7]
ELSE  
  DECLARE laStatus[6]
ENDIF
*C102283,1 ABD [End]

*E301273,1 Reham On 06/22/1999   *** End   ***
laStatus[1] = ' '
laStatus[2] = 'Open'     
laStatus[3] = 'On Hold'
laStatus[4] = 'Complete'
laStatus[5] = 'Canceled'
*E301273,1 Reham On 06/22/1999   *** Begin ***
*E301273,1 Add new element for the Pulled Picking Tickets.
laStatus[6] = 'Pulled'
*E301273,1 Reham On 06/22/1999   *** End   ***

*B606714,1 ALB Fix the multisession screen [begin]
*STORE '' TO lcPkTktStatus
*B606714,1 ALB Fix the multisession screen [end]

lcBrFields = ''

*C102283,1 ABD Add the status of "K" for "Packed" to the pick ticket screen. [Begin]
IF llPakStats
  laStatus[7] = 'Packed'
ENDIF  
*C102283,1 ABD [End]

*-- ldEntered, ldStart, ldComplete are read from ORDHDR not the base file.
*-- As a rule : if col.(s) exist in base file and another file , its better 
*-- to collect it from base file .
STORE {} TO ldEntered, ldStart, ldComplete

*-- lcDept, lcNote1, lcNote2 is read from ORDHDR not the base file.
*-- puWareHous is read from the base file.
*-- Warehouse (which changed to location) is stored as code (cwarecode)
*-- in the ordhdr/ordline/pikline and the desc. (which we need) is 
*-- stored in warehous dbf , we represent these in two dim. array 
*-- (which is lawarehouse) two cols. and no. of rows = no of warehouses
*-- the goal is to make ascan in this array using the code and extract
*-- the corresponding desc.
*-- lcNote1, lcNote2 are sudjested flds. to fill the header
STORE ' '  TO lcDept, lcNote1, lcNote2

*-- warehouse is changed to be o/p say field 
*STORE 0  TO puWareHous
STORE SPACE(35)  TO puWareHous

STORE '' TO lcDefWare 
STORE '' TO lcOldVal 

*-- llMultWare is .T. if the company is set to use multiple locations
llMultWare = ALLTRIM(gfGetMemVar('M_WareHouse')) = 'Y'

*-- llUseDyes identify whether co. uses Dyelots or not (restore company settings)
*-- furthermore , not only per company but also per style 
llUseDyes  = ALLTRIM(gfGetMemVar('M_DYELOT'))    = 'Y'

*-- laWareHous array for vals. of warehouse code.
DECLARE laWareHous[1,2]

*-- lnFolderCEnd is the end col. of the folder
*-- lnFolderREnd is the end row of the folder
*-- lnNoFld is the no. of folders
*-- lcwfoldchng stores the name of a function to control shows after change the folder 
lnFolderCEnd = 102.00
lnFolderREnd = 2.00
lnNoFld      = 3
lcwfoldchng  = '=lfActFolder()' 

*--
lafoldwinds[1,1] = 'Header'
lafoldwinds[1,2] = lcWinCh2
lafoldwinds[2,1] = 'Details'
lafoldwinds[2,2] = lcWinCh3
lcfoldprnt       = gcBaseWind            && Window parent name for the folder
*B606714,1 ALB Fix the multisession screen [begin]
*STORE 1 TO lnActFolder,lnLastFold        && Active folder
*B606714,1 ALB Fix the multisession screen [end]

***************************************************************************
*******   Adding options to to the toolbar                   **************
*DECLARE laPanelObj[2,3]
*laPanelObj[1,1] = 'pbOrdNote'
*laPanelObj[1,2] = gcBmpHome+'NOTES2.BMP'
*laPanelObj[1,3] = [VALID lfvOrdNote() MESSAGE 'Order Notes']
*laPanelObj[2,1] = 'pbObjLink'
*laPanelObj[2,2] = gcBmpHome+"Relate.bmp"
*laPanelObj[2,3] = [VALID lfvObjLink() MESSAGE 'Object Link']
***************************************************************************
***************************************************************************

*-- gcDataDir : ARIA27\DBFS\.. dir.
*-- gcWorkDir : ARIA27\WORK.. (temp files)
*-- gcSysHome : ARIA27\SYSFILES (system files dir)
*-- gcScrDir  : ARIA27\SCREENS\ dir
*-- gcWin_Appl : Window application ID

*-- If the session is opened for the first time, i.e. the base window
*-- (gcBaseWind) has never been defined before
IF !WEXIST(gcBaseWind)
  *-- llAnother  : when it is rised means that user come from another screen
  llAnother = .F.     && First time you open this session.
  
  *-- Temp name for child windows
  lcWinCh1         = gfTempName()
  lcWinCh2	       = gfTempName()
  lcWinCh3         = gfTempName()
  lcWinCh30        = gfTempName()
  lcWinCh31        = gfTempName()
  lcWinCh32        = gfTempName()
  lcfolder         = gfTempName()          && Folder Window Name
  lc_TmpOrdL       = gfTempName()          &&temporary file for order lines
  lcWinKey         = gfTempName()
  lcStyHdr         = gfItemMask("HI")      && label for style
  lcStyMsk         = gfItemMask("PI")
  lnStyLngth       = LEN(lcStyHdr)         && Var. hold the length of the style.  
  lnStyleWid       = LEN(lcStyHdr)  
  lafoldwinds[1,1] = 'Header'
  lafoldwinds[1,2] = lcWinCh2
  lafoldwinds[2,1] = 'Details'
  lafoldwinds[2,2] = lcWinCh3
  lcfoldprnt       = gcBaseWind            && Window parent name for the folder
  STORE 1 TO lnActFolder,lnLastFold        && Active folder
  
  lcScFields    = 'PikTkt, Status, Order, Account, Date, Store, CustPo, Prtflag' 
  SELECT PIKTKT
  SCATTER FIELDS &lcScFields TO laData BLANK 

  *-- Create temporaroy ORDLINE file.
  *-- It is recommended to build the temp. from an array to allow chg. stru.
  *-- we have AFIELDS() fn. places table stru. inf. into an array.
  *-- this array [x,y] where x = no. of table fields , y = 4 (fld. nam.,type,
  *-- wid. and dec. respectivly) . ret. the no. of flds. in the table.
  *-  Here x = 104 .
  SELECT ORDLINE
  
  =AFIELDS(laFileStru)           
  lnFileStru = ALEN(laFileStru,1)
  *B804372,1 HBG 30/08/2001 Add a rows for new fields for old PikQty [Begin]
  *DIMENSION laFileStru[lnFileStru + 2, 4]
  DIMENSION laFileStru[lnFileStru + 18, 4]
  *B804372,1 [End]
  *-- Add a field for modification/deletion status
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'cStatus'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0
  *-- Add a field for record number
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'nRecNo'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 10
  laFileStru[lnFileStru ,4] = 0
  *B804372,1 HBG 30/08/2001Add a fields for old PikQty [Begin]
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'OldPik1'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'OldPik2'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'OldPik3'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'OldPik4'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'OldPik5'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'OldPik6'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'OldPik7'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'OldPik8'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
   lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Ordln1'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Ordln2'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Ordln3'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Ordln4'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Ordln5'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Ordln6'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Ordln7'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Ordln8'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  *B804372,1 [End]
  CREATE CURSOR (lc_TmpOrdL) FROM ARRAY laFileStru
   
    
  *-- (lc_TmpOrdL) act like ordline but in edit mode 
  SELECT (lc_TmpOrdL) 
  *C200330,1 HBG 08/05/2002 IFtrigger to GMA to Add Index to the temp file to sort it by pack or line # [Begin] 
  IF llGMAPack
    =gfDoTriger('ALPKTKT',PADR('ADDINDX',10))
  ENDIF  
  *C200330,1 [End]
  *INDEX ON piktkt+order+STR(lineno,6) TAG PikLine       && chk this index , to be removed
  *INDEX ON cOrdType + ORDER  + STR(LineNo,6) TAG OrdLine
  *SET ORDER TO TAG PikLine
  
  *-- lc_tmpordl is dealt like ordline exactly in set relation.
  SET RELATION TO 'S'+SCALE INTO SCALE
  SET RELATION TO Style INTO Style ADDITIVE  
  *-- cWareCode of the temp (which is equivalent to ordline)
  *-- this is if changes allocated qtys. ' warehouse (in allocation)
  *-- affects stydye's warehouse in terms of lines' warehouse =.t.   
  SET RELATION TO Style + cWareCode INTO StyDye ADDITIVE  
    
  *-- (lc_TmpOrdL) act like ordline but in edit mode 
  *INDEX ON piktkt+order+STR(lineno,6) TAG PikLine
  *INDEX ON cOrdType + ORDER  + STR(LineNo,6) TAG OrdLine
  *SET ORDER TO TAG PikLine
  *-- For ini. vals. for qty(s) and orderqty(s) .
  SELECT ORDLINE  
  SCATTER MEMVAR MEMO BLANK
  
  *-- warehouse became o/p say field
  *-- In co. setup we idetify that either multi warehouse or single
  *-- warehouse . this is defined per co. per module , means
  *-- IC has setting and material has another setting
  *-- Also use dyelots or don't use is idetified per co.
  *-- these affect obj. disp. and saving proc.  
  SELECT WAREHOUS
  SET ORDER TO                        && to enable rushmore optomization
  lcDefWare  = WAREHOUS.cWareCode 
  *-- Browse style warehouses only
  LOCATE FOR lStyInv
  IF FOUND() 
    lcDefWare  = WAREHOUS.cWareCode
    *IF llMultWare
      SELECT IIF(EMPTY(cDesc),PADR(cWareCode,35),cDesc) AS cDescrip, cWareCode ;
      FROM WAREHOUS         ;
      WHERE lStyInv         ;
      INTO ARRAY laWareHous ;
      ORDER BY cDescrip
    *ENDIF  
  ENDIF
  **-- Default warehouse is blank (puWareHous = 0)     
  *puWareHous = 0   
  *SET ORDER TO TAG WAREHOUS
  puWareHous = SPACE(35)
  
ELSE
  *--Retrieve old session
  llAnother = .T.       && user is coming from another session.
  *B606714,1 ALB Fix the multisession screen [begin]
  *Rerelation the temp file with the scale and warehouse
  SELECT (lc_TmpOrdL) 
  *-- lc_tmpordl is dealt like ordline exactly in set relation.
  SET RELATION TO 'S'+SCALE INTO SCALE
  SET RELATION TO Style INTO Style ADDITIVE  
  *-- cWareCode of the temp (which is equivalent to ordline)
  *-- this is if changes allocated qtys. ' warehouse (in allocation)
  *-- affects stydye's warehouse in terms of lines' warehouse =.t.   
  SET RELATION TO Style + cWareCode INTO StyDye ADDITIVE  
  lnActFolder = lnLastFold
  *B606714,1 ALB Fix the multisession screen [end]
ENDIF  

*-- To retreive different (line) scales ('labels) 
*-- and stk. and alo.  vals. corresponding to line in edit reg.
SELECT ORDLINE
SET RELATION TO 'S'+SCALE INTO SCALE
SET RELATION TO Style INTO Style ADDITIVE  
SET RELATION TO Style + cWareCode INTO StyDye ADDITIVE  
SELECT ORDHDR
SET ORDER TO TAG Ordhdr            && cordtype+order



*-- Select the screen's base file
SELECT PIKTKT
SET ORDER TO PikTkt                && piktkt

lcFilter = 'PikTkt <> "******"'    && piktkt with no number, initial filter
SET FILTER TO &lcFilter

*-- To retreive some order info. -not exist in base file- from ORDHDR  
SET RELATION TO 'O'+order INTO ORDHDR  

*-- For customer inf.
SET RELATION TO 'M'+Account INTO CUSTOMER ADDITIVE   && to preserve other set rels. in the current wrk area

*B803633,1 HBG 03/09/2000 Set relations between PIKLINE and SCALE,Style[Begin]
SELECT PIKLINE
SET RELATION TO 'S'+SCALE INTO SCALE
SET RELATION TO Style INTO Style ADDITIVE  
*B803633,1 HBG 03/09/2000 [End]


IF WEXIST(lcBrTtl)
  HIDE WINDOW (lcBrTtl) SAME
  RELEASE WINDOW (lcBrTtl)
ENDIF

**-- Chg. def. msg. accompanied del. butt. (only one word).
**-- If needed completely different msg. append it to syddlobj
**-- and call gfModalGen()
**lcdelmesag= .F.
**lcDlgMessg  = 'release piktkt '
**lcDelMesag = 'cancel'
**lcMenProm  = PROPER(lcDelMesag) + " Picking Ticket"

**-- Before running the screen, we must do the folowing :
**-- 1. Save the current systrem menu (if exist).
**-- 2. Create the "Option" menu pad (if needed).
**-- 3. Set the needed filters, indexs on the used files.
**-- Run the screen and then restore all the previously stored values.

*-- Temp.,  after will chk. multi session ..

*B603506,1 ABD Increase amount sizes in Browse fields to fit currencies.
*B603506,1 ABD [BookAmt,ShipAmt,CancelAmt,Openamt] [Begin]

*lcOrderBr    = [cMarker= IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+; 
			   [STYLE= Style :H=lcStyHdr:R,]+;
			   [GROUP:H='G':P='!':1:R,]+; 
			   IIF(llUseDyes,;
			   [DYELOT= Dyelot :H='Dyelot':10:R,],[])+;
			   [nAvail=lfGetTotStk():H='Avail.':P='999999':R,]+;
			   [TOTQTY= TotQty]+;
               [:H='Open':P='999999' :R,]+;
		       [OpnAmt= TotQty*Price]+;
			   [:H='Opn Amnt.':P='99999999.99' :R,]+;
			   [TOTPIK=TotPik]+;
		 	   [:H='Picked':P='999999' :R,]+;
		 	   [PikAmt=TotPik*Price]+;
		       [:H='Pik Amnt.':P='99999999.99':R]

*C200330,1 HBG 08/05/2002 IF trigger to GMA Add column to detail browse to display Pack Information[Begin]
IF llGMAPack
  lcOrderBr = ""
  =gfDoTriger('ALPKTKT',PADR('BROWFLDS',10)) 
ELSE
*C200330,1 [End]
  lcOrderBr    = [cMarker= IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+; 
	             [STYLE= Style :H=lcStyHdr:R,]+;
                 [GROUP:H='G':P='!':1:R,]+; 
			     IIF(llUseDyes,;
			     [DYELOT= Dyelot :H='Dyelot':10:R,],[])+;
			     [nAvail=lfGetTotStk():H='Avail.':P='999999':R,]+;
			     [TOTQTY= TotQty]+;
			     [:H='Open':P='999999' :R,]+;
  			     [OpnAmt= TotQty*Price]+;
                 [:H='Opn Amnt.':P='9999999999.99' :R,]+;
			     [TOTPIK=TotPik]+;
			     [:H='Picked':P='999999' :R,]+;
                 [PikAmt=TotPik*Price]+;
                 [:H='Pik Amnt.':P='9999999999.99':R]
*C200330,1 HBG 08/05/2002 End if trigger to GMA [Begin]
ENDIF
*C200330,1 [END]

*B603506,1 ABD [End]

*B607240,1 ABD - Fix bug of variable "lcBrTtl' not found, that we should save
*B607240,1 ABD - the on key label when enter the screen, and return it after
*B607240,1 ABD - we exit from the screen. [Begin]
lchdAt_b = ON('KEY','Alt+B')
*B607240,1 ABD - [End]

*-- Trap Alt+B so that it activates the browse screen
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)

*IHB Add parameter [start]
*--Called from external program.
IF llExCalled
  SELECT PIKTKT
  lcPikSeek = lcpPikTkt
  =SEEK(lcPikSeek)
******************
  SCATTER FIELDS &lcScFields TO laData 
  lcPkTktStatus = lfAdjStatus(laData[2])
  *SHOW GET  lcPkTktStatus DISABLE
  *IF lcPkTktStatus = 'Canceled' OR  lcPkTktStatus = 'Complete' OR llSekPak
  *  SHOW GET pbEdt  DISABLE    
  *  SHOW GET pbDlt  DISABLE    
  *ELSE
  *  SHOW GET pbEdt  ENABLE
  *  SHOW GET pbDlt  ENABLE
  *ENDIF
  lcWareCode     = IIF(llMultWare, PIKTKT.cWareCode, lcDefWare)
  lnElem       = ASCAN(laWareHous, lcWareCode)
  IF lnElem <> 0
    puWareHous     = laWareHous[ASUBSCRIPT(laWareHous, lnElem,1),1]
  ELSE
    puWareHous     = SPACE(35)
  ENDIF

  SELECT ORDHDR
  ldEntered   = Entered
  ldStart     = Start
  ldComplete  = Complete
  lcDept      = Dept
  lcNote1     = Note1
  lcNote2     = Note2
  *SHOW GET ldEntered   DISABLE
  *SHOW GET ldStart     DISABLE
  *SHOW GET ldComplete  DISABLE        
  *SHOW GET lcNote1     DISABLE    
  *SHOW GET lcNote2     DISABLE    
  STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
           lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
           lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
           lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5

  *B122346,1 BWA 04/01/2004 Fix the billing address.[START]
  PRIVATE lcAliasB , lnCUSRecB
  lnCUSRecB = 0
  lcAliasB = ALIAS()
  SELECT CUSTOMER
  IF !EMPTY(ALLTRIM(laData[6]))
    lnCUSRecB = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+laData[4]+laData[6],'CUSTOMER') 
  ENDIF
  *B122346,1 BWA 04/01/2004.[END]

  lcBillName  = Customer.BtName
  =gfGetAdr('Customer','','','',1,'2')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount   = STR(laAddress[lnCount,1],1)
    lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR

  *B122346,1 BWA 04/01/2004 Return to the original record.[START]
  IF lnCUSRecB <> 0 
    GOTO lnCUSRecB IN CUSTOMER
  ENDIF
  SELECT(lcAliasB)
  *B122346,1 BWA 04/01/2004.[End]

  *B802727,1 BWA 12/07/1999 Fix the bug of showing the original ship to address [START]
  IF ORDHDR.Alt_ShpTo

    lcShipName  = ORDHDR.STName  
    lcShipAdd1 = ORDHDR.cAddress1
    lcShipAdd2 = ORDHDR.cAddress2
    lcShipAdd3 = ORDHDR.cAddress3
    lcShipAdd4 = ORDHDR.cAddress4
    lcShipAdd5 = ORDHDR.cAddress5
  ELSE
    *B605953,1 ASH 05/28/2002 (Begin) Fix the bug of displaying wrong ship to address.
    
	*B122346,1 BWA 04/01/2004 Declares the variables.[START]
    PRIVATE lcAlias , lnCUSRec
    lnCUSRec = 0
    lcAlias = ALIAS()
    *B122346,1 BWA 04/01/2004.[END]
    
    IF !EMPTY(laData[6])
      =SEEK('S'+laData[4]+laData[6],'CUSTOMER') 

      *B122346,1 BWA 04/01/2004 Fix the bug of showing Store data neither from Distripution data.[START]
      lcAlias = ALIAS()
      SELECT CUSTOMER
      IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
        lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
        =SEEK('S'+laData[4]+CUSTOMER.Dist_ctr)
      ENDIF
      *B122346,1 BWA 04/01/2004[End]

    ENDIF
    *B605953,1 ASH 05/28/2002 (End)
   
    lcShipName  =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
    =gfGetAdr('CUSTOMER','','','',1,'')

    *B122346,1 BWA 04/01/2004 Return to the original recno in the customer file.[START]
    IF lnCUSRec <> 0 
      GOTO lnCUSRec IN CUSTOMER
	ENDIF
      SELECT(lcAlias)
    *B122346,1 BWA 04/01/2004 [End]

    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount   = STR(laAddress[lnCount,1],1)
      lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR
  ENDIF
  *B802727,1 BWA 12/07/1999 [END]
  
  *SHOW GET lcShipName DISABLE
  *SHOW GET lcShipAdd1 DISABLE
  *SHOW GET lcShipAdd2 DISABLE
  *SHOW GET lcShipAdd3 DISABLE
  *SHOW GET lcShipAdd4 DISABLE
  *SHOW GET lcShipAdd5 DISABLE
  *IF lnActFolder = 1
    *SHOW GET laData[5]  DISABLE        
  *ENDIF
  SELECT ORDHDR
  lcTerms    = gfCodDes(cTermCode,'CTERMCODE')
  lcShipVia  = gfCodDes(ShipVia,'SHIPVIA')
  lcSpcInst  = gfCodDes(SpcInst,'SPCINST')
  lcSeason   = gfCodDes(Season,'SEASON')
  lcDivision = gfCodDes(cDivision,'CDIVISION')                
*****************
  *SCATTER FIELDS &lcScFields MEMO TO laData
  laScrMode    = .F.
  laScrMode[2] = .T.
  *=lpShow()
  
ENDIF
*IHB [end]

*C102782,1 ABD - Add Trigger for CAM05. [Begin]
IF ASCAN(laEvntTrig,PADR('CAMADBAR',10)) <> 0
  *--Activate Options pad.
  llReturn = gfDoTriger('ALPKTKT',PADR('CAMADBAR',10))
ENDIF
*C102782,1 ABD - [End]

DO (gcScrDir + gcWinAppl + '\ALPKTKT.SPR')

*C102782,1 ABD - Add Trigger for CAM05. [Begin]
IF ASCAN(laEvntTrig,PADR('CAMRLBAR',10)) <> 0
  *--Activate Options pad.
  = gfDoTriger('ALPKTKT',PADR('CAMRLBAR',10))
ENDIF
*C102782,1 ABD - [End]

*B607240,1 ABD - Fix bug of variable "lcBrTtl' not found, that we should save
*B607240,1 ABD - the on key label when enter the screen, and return it after
*B607240,1 ABD - we exit from the screen. [Begin]
ON KEY LABEL Alt+B &lchdAt_b
*B607240,1 ABD - [End]


*-- Release the browse window
RELEASE WINDOWS (lcBrTtl)

*C200330,1 HBG 08/05/2002 IF trigger to GMA Release the option menu[Begin] 
IF llGMAPack
  =gfDoTriger('ALPKTKT',PADR('RLESOPTN',10)) 
ENDIF
*C200330,1 [End]
  
*-- Reset filters and relations , for usage(s) of dbfs. elsewhere .
SELECT PIKTKT
SET FILTER TO
SET RELATION TO
SELECT ORDHDR
SET RELATION TO
SELECT ORDLINE
SET RELATION TO
SELECT Style 
SET RELATION TO

*-- Close and erase temporary files if quitting
IF glQuitting
  IF USED(lc_TmpOrdL) 
    USE IN (lc_TmpOrdL)
  ENDIF
  ERASE (gcWorkDir + lc_TmpOrdL + '.DBF')
  ERASE (gcWorkDir + lc_TmpOrdL + '.FPT')
  ERASE (gcWorkDir + lc_TmpOrdL + '.CDX')  
ENDIF  
  
*-- Release defined popups and pads (if exist)

*-- Functions and procedures
*!*************************************************************
*! Name      : lpShow
*! Developer : IHAB
*! Date      : 01/01/1996
*! Purpose   : Assigns enable/disable vals. depend on laScrMode
*!*************************************************************
*! Calls     : ARIABROW,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpShow()
*!*************************************************************
PROCEDURE lpShow

*-- if the user is coming from another session.
IF llAnother
  DO lpActFold
  llAnother = .F.
  RETURN
ENDIF  

DO CASE
  CASE laScrMode[1]   && Select 
    *-- Base file ini. and adjustment(s).
    
    SELECT PIKTKT
    *-- reset piktkt filter
    SET FILTER TO &lcFilter
    
    SCATTER FIELDS &lcScFields TO laData BLANK 
    lcPkTktStatus = ' '
    SHOW GET  lcPkTktStatus DISABLE

    *-- No recs. and no marker 
    STORE 1   TO lnMarker  
    
    *-- warehouse became o/p say field
    **-- Adjust warehouses
    *STORE '' TO lcOldVal 
    *STORE lcDefWare TO lcWareCode
    *IF llMultWare
    *  lnElem     = ASCAN(laWareHous, lcDefWare)
    *  STORE IIF(lnElem <> 0, ASUBSCRIPT(laWareHous, lnElem, 1), puWareHous);
    *     TO puWareHous, puPickWare
    *ENDIF
    *SHOW GET  puWareHous DISABLE
    STORE SPACE(35) TO puWareHous
    
    STORE {} TO ldEntered, ldStart, ldComplete    
    SHOW GET  ldEntered   DISABLE
    SHOW GET  ldStart     DISABLE
    SHOW GET  ldComplete  DISABLE        
    
    *--
    STORE ' '  TO lcDept, lcNote1, lcNote2
    
    *-- warehouse became o/p say field
    *STORE 0 TO puWareHous
    *
    
    *-- For bill to & ship to flds. 
    STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
                 lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
                 lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
                 lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5
    *
 
    *-- Ini. base/temp. work alias.
    SELECT (lc_TmpOrdL)
    *ZAP
    SCATTER MEMVAR MEMO BLANK
    
    SELECT ORDLINE
    *lnCurTag = VAL(SYS(21))
    *SET ORDER TO TAG PikLine         && to be removed
    
    =lfBrowse() 
    
    *SELECT ORDLINE
    *SET ORDER TO TAG (lnCurTag)
    
    *-- Ini. vals. for codes (which are -here- say fields) 
    STORE ' ' TO lcTerms, lcShipVia, lcSpcInst, lcSeason, lcDivision

    *-- editing header information is not allowed 
    SHOW GET ldEntered   DISABLE
    SHOW GET ldStart     DISABLE
    SHOW GET ldComplete  DISABLE        
*    SHOW GET lcDept      DISABLE    
    SHOW GET lcShipName DISABLE
    SHOW GET lcShipAdd1 DISABLE
    SHOW GET lcShipAdd2 DISABLE
    SHOW GET lcShipAdd3 DISABLE
    SHOW GET lcShipAdd4 DISABLE
    SHOW GET lcShipAdd5 DISABLE
*    SHOW GET laData[5]  DISABLE        
*    SHOW GET laData[7]  DISABLE              
    *
    
    *B602546,1 IHB start, Initialize in select mode
    STORE .F. TO llSekPak
    *B602546,1 IHB end

    *-- To be in the validate folder        
    *-- N.B. lpshow ideally doesn't care about folders ,
    *-- so , calling lfactfolder is determining the folder to be activated. 
    IF lnActFolder = 1
    
    ENDIF
        
  CASE laScrMode[2]   && View
    *C200330,1 HBG 08/05/2002 IF trigger to GMA Set the flag to handle the option menu[Begin] 
    IF llGMAPack
      =gfDoTriger('ALPKTKT',PADR('SETFLAG',10)) 
    ENDIF
    *C200330,1 [End]
    *-- Refreshing necessary objs. (tkt. status chg.,need reassign ladata[2])
    SELECT PIKTKT
    SCATTER FIELDS &lcScFields TO laData 
    lcPkTktStatus = lfAdjStatus(laData[2])
    SHOW GET  lcPkTktStatus DISABLE
    
    *B602546,1 IHB Check if this pick ticket has a packing list or not
    llSekPak = lfSekPak()
    *B602546,1 IHB end
    
    *-- If PikTktstatus is cancelled or complete or piktkt has packing list 
    *-- edit is not allowed.
    *B602546,1 IHB if this pick ticket has a packing list, disable the 
    *B602546,1 IHB Delete and edit buttons
    *IF lcPkTktStatus = 'Canceled' OR  lcPkTktStatus = 'Complete'
    IF lcPkTktStatus = 'Canceled' OR  lcPkTktStatus = 'Complete' OR llSekPak
    *B602546,1 IHB end
      SHOW GET pbEdt  DISABLE    
      SHOW GET pbDlt  DISABLE    
    ELSE
      SHOW GET pbEdt  ENABLE
      SHOW GET pbDlt  ENABLE
    ENDIF
    
    *--
    lcWareCode     = IIF(llMultWare, PIKTKT.cWareCode, lcDefWare)

    *IF llMultWare
      lnElem       = ASCAN(laWareHous, lcWareCode)
      *puWareHous   = IIF(lnElem <> 0, ASUBSCRIPT(laWareHous, lnElem, 1), 0)
    *ENDIF
    
    *SHOW GET  puWareHous DISABLE
    *puWareHous   = IIF(lnElem <> 0, ASUBSCRIPT(laWareHous, lnElem, 1), 0)
    IF lnElem <> 0
      puWareHous     = laWareHous[ASUBSCRIPT(laWareHous, lnElem,1),1]
    ELSE
      puWareHous     = SPACE(35)
    ENDIF  
    *
    
    *-- Retreive some info from ORDHDR
    SELECT ORDHDR
    ldEntered   = Entered
    ldStart     = Start
    ldComplete  = Complete
    lcDept      = Dept
    lcNote1     = Note1
    lcNote2     = Note2
    SHOW GET ldEntered   DISABLE
    SHOW GET ldStart     DISABLE
    SHOW GET ldComplete  DISABLE        
*    SHOW GET lcDept      DISABLE    
    SHOW GET lcNote1     DISABLE    
    SHOW GET lcNote2     DISABLE    

    *-- Cust. billing & shipping addr.
    STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5
    
    *-- gfGetAdr get cust. addr.
    *-- parameters ; lcAlias, lcTag, lcKeyCode, lcAdrCode,lnLineNo,lcAddGrp,lcCurrCode
    *-- lcAlias   : source file name 
    *-- lcTag     : source file tag that is to be used in seeking
    *-- lckeycode : search key code (of the source file) (optional)
    *-- lcAdrCode : address code (optional)
    *-- lnLineNo  : The Address line number to return

   *B122346,1 BWA 04/01/2004 Fix the billing address.[START]
   PRIVATE lcAliasB , lnCUSRecB
   lnCUSRecB = 0
   lcAliasB = ALIAS()
   SELECT CUSTOMER
   IF !EMPTY(ALLTRIM(laData[6]))
     lnCUSRecB = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
     =SEEK('S'+laData[4]+laData[6],'CUSTOMER') 
   ENDIF
   *B122346,1 BWA 04/01/2004 [END]
    
   lcBillName  = Customer.BtName
   =gfGetAdr('Customer','','','',1,'2')
   FOR lnCount = 1 TO ALEN(laAddress,1)
     lcCount   = STR(laAddress[lnCount,1],1)
     lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
     SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
   ENDFOR
    
   *B122346,1 BWA 04/01/2004 Return to the original record.[START]
   IF lnCUSRecB <> 0 
     GOTO lnCUSRecB IN CUSTOMER
   ENDIF
   SELECT(lcAliasB)
   *B122346,1 BWA 04/01/2004 [End]

    *B802727,1 BWA 12/07/1999 Fix the bug of showing the original ship to address [START]
    IF ORDHDR.Alt_ShpTo

      lcShipName  = ORDHDR.STName  
      lcShipAdd1 = ORDHDR.cAddress1
      lcShipAdd2 = ORDHDR.cAddress2
      lcShipAdd3 = ORDHDR.cAddress3
      lcShipAdd4 = ORDHDR.cAddress4
      lcShipAdd5 = ORDHDR.cAddress5
    ELSE
      *B605953,1 ASH 05/28/2002 (Begin) Fix the bug of displaying wrong ship to address.

      *B122346,1 BWA 04/01/2004 Declares the variables.[START]    
      PRIVATE lcAlias , lnCUSRec
      lnCUSRec = 0
      lcAlias = ALIAS()
      *B122346,1 BWA 04/01/2004 [END]
      
      IF !EMPTY(laData[6])
        =SEEK('S'+laData[4]+laData[6],'CUSTOMER') 
        
        *B122346,1 BWA 04/01/2004 Fix the bug of showing Store data neither from Distripution data.[START]
        lcAlias = ALIAS()
        SELECT CUSTOMER
        IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
          lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
          =SEEK('S'+laData[4]+CUSTOMER.Dist_ctr)
        ENDIF
        *B122346,1 BWA 04/01/2004 [End].
      
      ENDIF
      *B605953,1 ASH 05/28/2002 (End)
   
      lcShipName  =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
      =gfGetAdr('CUSTOMER','','','',1,'')

	  *B122346,1 BWA 04/01/2004 Return to the original recno in the customer file.[START]
      IF lnCUSRec <> 0 
        GOTO lnCUSRec IN CUSTOMER
	  ENDIF
      SELECT(lcAlias)
      *B122346,1 BWA 04/01/2004 [End].
	
      FOR lnCount = 1 TO ALEN(laAddress,1)
        lcCount   = STR(laAddress[lnCount,1],1)
        lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
        SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
      ENDFOR
    ENDIF
    *B802727,1 BWA 12/07/1999 [END]
    
    SHOW GET lcShipName DISABLE
    SHOW GET lcShipAdd1 DISABLE
    SHOW GET lcShipAdd2 DISABLE
    SHOW GET lcShipAdd3 DISABLE
    SHOW GET lcShipAdd4 DISABLE
    SHOW GET lcShipAdd5 DISABLE

    *-- collect tmp. (if not collected)
    *IF RECCOUNT(lc_TmpOrdL) = 0
    *   SELECT (lc_TmpOrdL)
    *   APPEND FROM gcDataDir+"OrdLine"
    *   SET RELATION TO 'S'+SCALE INTO SCALE
    *ENDIF  
    
    SELECT ORDLINE 
    
    *SET ORDER TO TAG PikLine          && to be removed
    
    =lfBrowse() 

    *-- ladata[5] is the piktkt entered date ( for now )
    IF lnActFolder = 1
      SHOW GET laData[5]  DISABLE        
*      SHOW GET laData[7]  DISABLE              
    ENDIF
    
    SELECT ORDHDR
    
    *-- Retrieve codes' descr.
    lcTerms    = gfCodDes(cTermCode,'CTERMCODE')
    lcShipVia  = gfCodDes(ShipVia,'SHIPVIA')
    lcSpcInst  = gfCodDes(SpcInst,'SPCINST')
    lcSeason   = gfCodDes(Season,'SEASON')
    lcDivision = gfCodDes(cDivision,'CDIVISION')                
    
    IF lnActFolder = 1
    ENDIF

   *
   *-- Restore key settings
   ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)
   *

  
  CASE laScrMode[3]    && Edit
  
    *-- to allow undo msg.
    llCUpDate  = .T.
    *--
    SHOW GET pbRemove  ENABLE
    
    *-- disabling the capability of altering the warehouse and the store.
    *-- warehouse became o/p say field
    *SHOW GET  puWareHous DISABLE
    
    SHOW GET  laData[5] DISABLE     && tkt date
*    SHOW GET  laData[7] DISABLE     && po no 

    *-- collect tmp. 
    *-- Only needed recs. are to be collected 
    *-- then composing ini. vals. for the extra two cols. nrecno,cstatus.
    *-- indexes and relations are already set.
    SELECT (lc_TmpOrdL)
    ZAP
    SELECT ORDLINE
    lnCurTag = VAL(SYS(21))
    SET ORDER TO

    *B603242,1 MAN Use Rushmore to Optimize the speed
    *SCAN FOR PikTkt=laData[1] AND Order=ALLTRIM(laData[3])          
    SCAN FOR cordtype+order+STR(lineno,6)="O"+laData[3] AND PikTkt=laData[1]
      SCATTER MEMVAR MEMO        && if memo fields exist use memo 
      m.nRecNo  = RECNO()
      m.cStatus = 'S'
      INSERT INTO (lc_TmpOrdL) FROM MEMVAR
      *B804372,1 HBG 30/08/2001 Update fields of old PikQty with old allo. qtys. to be used
      *B804372,1                if PikQty modified to update style/stydye. [Begin]
      REPLACE &lc_TmpOrdL..OldPik1 WITH m.Pik1,;
              &lc_TmpOrdL..OldPik2 WITH m.Pik2,;
              &lc_TmpOrdL..OldPik3 WITH m.Pik3,;
              &lc_TmpOrdL..OldPik4 WITH m.Pik4,;
              &lc_TmpOrdL..OldPik5 WITH m.Pik5,;
              &lc_TmpOrdL..OldPik6 WITH m.Pik6,;
              &lc_TmpOrdL..OldPik7 WITH m.Pik7,;
              &lc_TmpOrdL..OldPik8 WITH m.Pik8
      REPLACE &lc_TmpOrdL..Ordln1 WITH m.Qty1,;
              &lc_TmpOrdL..Ordln2 WITH m.Qty2,;
              &lc_TmpOrdL..Ordln3 WITH m.Qty3,;
              &lc_TmpOrdL..Ordln4 WITH m.Qty4,;
              &lc_TmpOrdL..Ordln5 WITH m.Qty5,;
              &lc_TmpOrdL..Ordln6 WITH m.Qty6,;
              &lc_TmpOrdL..Ordln7 WITH m.Qty7,;
              &lc_TmpOrdL..Ordln8 WITH m.Qty8
      *B804372,1 [End]        
    ENDSCAN
    *B603242,1 MAN Set the record pointer to 1st record
    GO TOP IN (lc_TmpOrdL)
    IF BETWEEN(lnMarker, 1, RECCOUNT())
      GO lnMarker  
    ENDIF  
    
    SELECT ORDLINE
    SET ORDER TO (lnCurTag)    

    =lfBrowse() 
    
    *-- editing header information is not allowed 
    SHOW GET ldEntered   DISABLE
    SHOW GET ldStart     DISABLE
    SHOW GET ldComplete  DISABLE        
*    SHOW GET lcDept      DISABLE    
    SHOW GET lcShipName DISABLE
    SHOW GET lcShipAdd1 DISABLE
    SHOW GET lcShipAdd2 DISABLE
    SHOW GET lcShipAdd3 DISABLE
    SHOW GET lcShipAdd4 DISABLE
    SHOW GET lcShipAdd5 DISABLE
*    SHOW GET laData[5]  DISABLE        
*    SHOW GET laData[7]  DISABLE              
    *
  CASE laScrMode[4]    && Add
ENDCASE  
=lfActFolder()

*!*************************************************************
*! Name      : lfAdjStatus
*! Developer : RENEE
*! Date      : 26/10/1998
*! Purpose   : Fix PkTktStatus
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfAdjStatus(lcStatus)
*!*************************************************************
FUNCTION lfAdjStatus
PARAMETER lcStatus
*E301273,1 Reham On 06/22/1999   *** Begin ***
*E301273,1 Add new chr: "P" for status "Pulled".
*RETURN laStatus[AT(lcStatus, ' OHCX')]

*C102283,1 ABD Add the status of "K" for "Packed" to the pick ticket screen. [Begin]
IF llPakStats
  RETURN laStatus[AT(lcStatus, ' OHCXPK')]
ELSE
  RETURN laStatus[AT(lcStatus, ' OHCXP')]
ENDIF
*C102283,1 ABD [End]

*E301273,1 Reham On 06/22/1999   *** End   ***

*!*************************************************************
*! Name      : lfvData_1
*! Developer : IHAB
*! Date      : 12/10/1998
*! Purpose   : Validate pik ticket number
*!*************************************************************
*! Calls     : gfBrows()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvData_1()
*!*************************************************************
FUNCTION lfvData_1
*PRIVATE lcBrFields, lcFileTtl  && To allow differnt brfields.,brfile

SELECT PIKTKT
*-- adjust filter due to entered account or order or none

GO TOP

IF EMPTY(laData[1]) OR AT('?',ALLTRIM(laData[1]),1) > 0   
  
  *  SELECT ORDHDR                                        

    *BADRAN 02/24/2000
    *C102283,1 ABD Add the status of "K" for "Packed" to the pick ticket Browse. [Begin]
    IF llPakStats
      *B122561,1 NNA 04/29/2004 (Begin) Add the status of 'X' for 'Canceled' the pick ticket Browse
      *lcBrFields = [PIKTKT :R :H= 'Piktkt#' , ACCOUNT :R :H= 'Account', STORE :R :H= 'Store' ,] +;
                   [ORDER :R :H= 'Order' , DATE :R :H= 'Date', ] +;
                   [CWARECODE :R :H= 'Location' , CUSTPO :R :H= 'Customer PO' , STATUS = IIF(STATUS = 'O' , 'Open' , IIF(STATUS = 'H' ,'On Hold',IIF(STATUS = 'C','Complete', IIF(STATUS = 'K','Packed','Pulled')))) :R :H= 'Status  ']
      lcBrFields = [PIKTKT :R :H= 'Piktkt#' , ACCOUNT :R :H= 'Account', STORE :R :H= 'Store' ,] +;
                   [ORDER :R :H= 'Order' , DATE :R :H= 'Date', ] +;
                   [CWARECODE :R :H= 'Location' , CUSTPO :R :H= 'Customer PO' , ;
                   STATUS = IIF(STATUS = 'O' , 'Open' , IIF(STATUS = 'H' ,'On Hold',;
                            IIF(STATUS = 'C','Complete',IIF(STATUS = 'K','Packed',;
                            IIF(STATUS = 'X','Canceled','Pulled'))))) :R :H= 'Status  ']
      *B122561,1 NNA (End)
    ELSE
      *B122561,1 NNA 04/29/2004 (Begin) Add the status of 'X' for 'Canceled' the pick ticket Browse
      *lcBrFields = [PIKTKT :R :H= 'Piktkt#' , ACCOUNT :R :H= 'Account', STORE :R :H= 'Store' ,] +;
                   [ORDER :R :H= 'Order' , DATE :R :H= 'Date', ] +;
                   [CWARECODE :R :H= 'Location' , CUSTPO :R :H= 'Customer PO' , STATUS = IIF(STATUS = 'O' , 'Open' , IIF(STATUS = 'H' ,'On Hold',IIF(STATUS = 'C','Complete', 'Pulled'))) :R :H= 'Status  ']
      lcBrFields = [PIKTKT :R :H= 'Piktkt#' , ACCOUNT :R :H= 'Account', STORE :R :H= 'Store' ,] +;
                   [ORDER :R :H= 'Order' , DATE :R :H= 'Date', ] +;
                   [CWARECODE :R :H= 'Location' , CUSTPO :R :H= 'Customer PO' ,;
                   STATUS = IIF(STATUS = 'O' , 'Open' , IIF(STATUS = 'H' ,'On Hold',;
                            IIF(STATUS = 'C','Complete',IIF(STATUS = 'X' , 'Canceled' ,'Pulled')))) :R :H= 'Status  ']
      *B122561,1 NNA (End)
    ENDIF
    *C102283,1 ABD [End]


  *  lcFileTtl  = 'Order lines'
  *  DECLARE laTmpArray[2]
  *  =gfBrows(.f., 'order,account', laTmpArray)
  *  SCATTERED TO 'laData[]

  IF ISDIGIT(LEFT(ALLTRIM(laData[1]), 1))
    laData[1] =  PADL(ALLTRIM(laData[1]), 6, '0')
  ENDIF  
  IF !llBrowse 
    IF EMPTY(laData[1])    
      STORE '' TO laData[1]
      RETURN         && Either user choosed pb or is bypassing PikTkt#
    ELSE
      * Either '?' alone or part of the key with '?' 
      IF AT('?',ALLTRIM(laData[1]),1) > 0 
        STORE '' TO laData[1]    
        llBrowse   = .T.
      
        *lcSeekExp = SUBSTR(ALLTRIM(laData[1]),AT('?',ALLTRIM(laData[1]),1)+1,LEN(laData[1]))
        *STORE '' TO lcSeekExp
      ENDIF

  * To move pointer to specific rec.   after
  *    IF !SEEK(lcSeekExp)
  *      STORE '' TO laData[1]    
  *      llBrowse   = .T.
  *      lnSoftSeek = RECNO(0)
  *      IF BETWEEN(lnSoftSeek, 1, RECCOUNT())
  *        GO lnSoftSeek
  *      ENDIF
  *    ENDIF  

    ENDIF  
  ENDIF
  IF llBrowse          && Calling browser according to empty(ladata[1]) or '?'
    =gfBrows([laData(1)])  
  ENDIF  
  llBrowse = .F.

ELSE
  *-- Either user entered valid entry or invalid entry  
  IF !SEEK(laData[1])
    WAIT WINDOW 'Invalid Pik Ticket No.'
    laScrMode    = .F.
    laScrMode[1] = .T.
    SHOW GETS
    =lpShow()
  ELSE  
    SCATTER FIELDS &lcScFields MEMO TO laData
    laScrMode    = .F.
    laScrMode[2] = .T.
    SHOW GETS
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfvData_2
*! Developer : IHAB
*! Date      : 27/10/1998
*! Purpose   : Validate pik ticket status
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvData_2()
*!*************************************************************
FUNCTION lfvData_2                      && Piktkt status 

*E301273,1 Reham On 06/22/1999   *** Begin ***
*E301273,1 Replace with one of the following status: Open, On Hold, Pulled.
*laData[2] = IIF(ASCAN(laPkTktStatus,lcPkTktStatus)=2,'O','H')
laData[2] = IIF(ASCAN(laPkTktStatus,lcPkTktStatus) > 0, laPkTktStatus[ASUBSCRIPT(laPkTktStatus,ASCAN(laPkTktStatus,lcPkTktStatus),1),2] ,'H')
*E301273,1 Reham On 06/22/1999   *** End   ***

*laDdata[2] = ASCAN()..  use(laPkTkt....)

*!*************************************************************
*! Name      : lfvData_3
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key laData[3] . && Order no
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfGetData,OrdBrowA,lfvNewPack
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_3()
*!*************************************************************
FUNCTION lfvData_3

PRIVATE lnCurAlias,lcTag,lcOrder
lnCurAlias = SELECT(0)
lcTag = ORDER('OrdHdr')
SET ORDER TO OrdHdr IN OrdHdr

IF !( (EMPTY(laData[3]) OR LASTKEY() <> 13) AND !llBrowse)
  IF (!EMPTY(laData[3]) .AND. !SEEK('O'+laData[3],'OrdHdr') ) OR llBrowse
    lcOrder = laData[3]
    =lfOrdBrow(@lcOrder,laData[4])
    laData[3] = lcOrder 
    llBrowse = .F.
  ENDIF
ENDIF

*-- filter base file, assign account if not eof() is reached.
IF !EMPTY(laData[3])
   *-- 
   SELECT PIKTKT
   SET FILTER TO ORDER=laData[3]
   GO TOP
   IF EOF()
      *-- 'No piktkts found for order laData[3]
      *-- <OK>
      =gfModalGen("INM44028B00000","Dialog",laData[3])
      *-- return to select mode/ini. vars.
      STORE SPACE(06) TO laData[3]
      SHOW GET laData[3]
      SET FILTER TO &lcFilter      && restore initial filter of base file
      RETURN
   ENDIF

   laData[4] = ORDHDR.Account

   SHOW GET laData[4]
   =lfRefresh(lcWinCh1)
  _CUROBJ = OBJNUM(laData[1])
ENDIF
*
SET ORDER TO lcTag IN OrdHdr
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : browse all pack list or validated by the account
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : AriaBrow,gfModalGen
*!*************************************************************
*! Passed Parameters  : lcOrder,lcAccount
*!*************************************************************
*! Returns            : llReturn
*!*************************************************************
*! Example   : = lfOrdBrow()
*!*************************************************************
FUNCTION lfOrdBrow

PARAMETERS lcOrder,lcAccount
*-- lcFields   variable that hold the name of browsed fields
*-- laBrow     array that hold the returned values from AriaBrow function
*-- lnCurAlias variable that hold the current alias
*-- lcCurTag   variable that hold the currend tag name
*-- llReturn   variable which is returned by this function 
*-- lcTag      variable that hold the name of the tag which is desired to switch 
*              file order to it
PRIVATE lcFields,laBrowArr,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields
DIMENSION laBrowArr[1]
STORE SPACE(0) TO lcFields,laBrowArr,lcBrFields

lnCurAlias = SELECT(0)
SELECT ORDHDR
GO TOP
lcTag = ORDER('OrdHdr')
lcBrFields = [Order:H="Order#",status:1:H="S",lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
             [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
             [ACCOUNT:H="Acct",store=IIF(MULTI='Y','*Multi*',STORE):H="Store",Customer.stname]+;
             [:15:H="Name",Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",Ship:H="Ship.Qty.",Shipamt:H="Ship.Amt.",]+;
             [start:H="Start",Complete:H="Complete",]+;
             [Note1:6:H="Notes"]
DO CASE
  CASE !EMPTY(lcAccount)
    SET ORDER TO TAG 'OrdAcct' IN OrdHdr
    lcOrder = IIF(ARIABROW("lcAccount+'O'",;
                  "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))

  CASE EMPTY(lcAccount)
    SET ORDER TO TAG 'OrdHdr' IN OrdHdr
    lcOrder = IIF(ARIABROW("'O'","Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))
ENDCASE
SET ORDER TO lcTag IN OrdHdr
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvData_4
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key laData[4] . && Account 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : CUSBROWM,lfRefresh
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_4()
*!*************************************************************
FUNCTION lfvData_4
*-- only validate in case of empty(piktkt) and empty(order)
IF EMPTY(laData[1]) AND EMPTY(laData[1])

  laData[4] = PADR(ALLTRIM(laData[4]), FSIZE('Account', 'PIKTKT'))
  XACCOUNT = laData[4]
  IF llBrowse OR (!EMPTY(laData[4]) AND !SEEK('M'+XACCOUNT,'Customer'))
    llBrowse = .F.
    DO CUSBROWM WITH XACCOUNT
    laData[4] = XACCOUNT
  ENDIF  
  IF EMPTY(laData[4])
    STORE SPACE(6) TO laData[3]
    SHOW GET laData[3]
    =lfRefresh(lcWinCh1)
    _CUROBJ = OBJNUM(laData[1])
  ELSE  
    *--
    SELECT PIKTKT
    SET FILTER TO ACCOUNT=laData[4]
    GO TOP
    IF EOF()
      *-- 'No piktkts found for account laData[4]
      *-- <OK>
      =gfModalGen("INM44043B00000","Dialog",laData[4])
      *-- return to select mode/ini. vars.
      STORE SPACE(05) TO laData[4]
      SHOW GET laData[4]
      SET FILTER TO &lcFilter
      _CUROBJ = OBJNUM(laData[1])
      RETURN
    ENDIF
    *
    *=lfRefresh(lcWinKey)
    =lfRefresh(lcWinCh1)
    _CUROBJ = OBJNUM(laData[1])
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfBrowse
*! Developer : IHAB
*! Date      : 12/10/1998
*! Purpose   : Browse Detail Block
*!*************************************************************
*! Calls     : gfBrows()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfBrowse()
*!*************************************************************
FUNCTION lfBrowse     && In the screen setup code of any/parent screen 
PRIVATE lnCurAlias, lnCurTag
lnCurAlias = SELECT(0)

IF WEXIST(lcBrTtl)
  RELEASE WINDOW (lcBrTtl)
ENDIF

*-- In view mode browse from  PikLine ,OrdLine   .
*-- In edit mode (only edit is enabled for status 'O' and 'H') browse from  Tmp 

*-- If the ticket is completed or canceled, browse from PikLine file
IF laData[2] $ 'CX' 
  SELECT PikLine 
  *B603242,1 MAN Use Rushomre "PikTkt Tag"
  *lcForCond = laData[1] + laData[3]
  lcForCond = " FOR piktkt+order+STR(lineno,6)=laData[1] + laData[3]"
  
ELSE
  IF laScrMode[3] 			&& Edit mode
    SELECT (lc_TmpOrdL)    
 
    *SCATTER FIELDS LIKE PIK* TO laPik
    *SCATTER FIELDS LIKE QTY* TO laQty
    *lnTotPik = TotPik
    *lnTotQty = TotQty
    *B603242,1 MAN No need for a condition while browse from the temp file
    lcForCond = ""
    
  ELSE
    SELECT OrdLine
    *B603242,1 MAN Use Rushomre by getting the order number
    lcForCond = " FOR cordtype+order+STR(lineno,6)='O"
    lcForCond = lcForCond + LOOKUP(Piktkt.order,laData[1],Piktkt.piktkt,"Piktkt") + "'"
    lcForCond = lcForCond + " AND piktkt= laData[1]"
  ENDIF  
  *B603242,1 MAN Comment this line, The for condition variable has been set
  *lcForCond = laData[1] + laData[3]
ENDIF

*-- Adjust marker
*IF !BETWEEN(lnMarker, 1, RECCOUNT())
*  lnMarker = RECNO()
*ENDIF

*-- SYS(21) returns the active tag number, SYS(14, ..) returns the index
*-- expression of a tag number. Use the index expression with the FOR condition
*-- for optimization.
lnCurTag = VAL(SYS(21))

*lcNdxExp = SYS(14, lnCurTag)

SET ORDER TO 0
  
*SET RELATION TO ...
*ACTIVATE WINDOW (lcWinCh3)

*-- chk. piktkt and order instead of count (to disable
*-- remove button and to release piktkt) 
*-- Instead of set exact use &lcNdxExp and &lcForCond
*B603242,1 MAN Use the lcForeCond Varable
*BROWSE FIELDS               ;
       &lcOrderBr           ;
       WINDOW  (lcWinCh31)  ;
       IN WINDOW (lcWinCh3) ;
       NOMENU               ;         
       NOAPPEND             ;
       NOEDIT               ;
       NODELETE             ;
       SAVE                 ;
       NOWAIT               ;
       NOCLEAR              ;   
       WHEN lfwPikBrow()    ;     
       VALID  :F lfvBrowse();
       FOR   PIKTKT+ORDER = laData[1] + laData[3] ;
       TITLE (lcBrTtl)                  
BROWSE FIELDS               ;
       &lcOrderBr           ;
       WINDOW  (lcWinCh31)  ;
       IN WINDOW (lcWinCh3) ;
       NOMENU               ;         
       NOAPPEND             ;
       NOEDIT               ;
       NODELETE             ;
       SAVE                 ;
       NOWAIT               ;
       NOCLEAR              ;   
       WHEN lfwPikBrow()    ;     
       VALID  :F lfvBrowse();
       &lcForCond. ;
       TITLE (lcBrTtl)                  


  
*FOR &lcNdxExp = laData[1] + laData[3] ;
*PikTkt=ALLTRIM(laData[1]) AND Order=ALLTRIM(laData[3]);          
*-- 
*=lfwPikBrow()
*IF SEEK(laData[1]+laData[3]) 
*GO TOP

IF laScrMode[3] 			
*  IF BETWEEN(lnMarker, 1, RECCOUNT())
*    GO lnMarker  
*  ENDIF  
  SCATTER FIELDS LIKE PIK* TO laPik
  SCATTER FIELDS LIKE QTY* TO laQty
  lnTotPik = TotPik
  lnTotQty = TotQty
ENDIF

IF PikTkt=ALLTRIM(laData[1]) AND Order=ALLTRIM(laData[3])
  SCATTER MEMVAR MEMO
ELSE
  SCATTER MEMVAR MEMO BLANK
ENDIF

lnMarker   = IIF(EOF(), 1, RECNO())

IF lnActFolder = 2 
  SHOW WINDOW (lcWinCh3)    REFRESH  
  SHOW WINDOW (lcWinCh32)    REFRESH  
  SHOW GET lcStyHdr DISABLE
  SHOW GET m.Style DISABLE  
ENDIF

*
*SHOW WINDOW (lcBrTtl) REFRESH SAME
*

=lfRefresh(lcWinCh32)
SHOW GETS WINDOW (lcWinCh32) ONLY

*-- Reset environment
SET ORDER TO (lnCurTag)
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvBrowse
*! Developer : IHAB
*! Date      : 10/21/1998
*! Purpose   : Accompaning browser with line details  
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBrowse()
*!*************************************************************
FUNCTION lfvBrowse
IF WONTOP() = lcBrTtl
  =gfStopBrow() 
ENDIF

*!*************************************************************
*! Name      : lfwPikBrow
*! Developer : IHAB
*! Date      : 10/21/1998
*! Purpose   : Accompaning browser with line details  
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwPikBrow()
*!*************************************************************
FUNCTION lfwPikBrow
PRIVATE lnCurAlias 
lnCurAlias = SELECT(0)

*-- If it is the EOF(), blank memory variables will be created
*SELECT IIF(laScrMode[2] 

*IF !EMPTY(laData[1])
IF PikTkt=ALLTRIM(laData[1]) AND Order=ALLTRIM(laData[3])
  SCATTER MEMVAR MEMO
ELSE
  SCATTER MEMVAR MEMO BLANK
ENDIF  

IF laScrMode[3] 			
  SCATTER FIELDS LIKE PIK* TO laPik
  SCATTER FIELDS LIKE QTY* TO laQty
  SCATTER FIELDS LIKE PIK* TO laOldPik    && store the old qty. to subtract it when saving in style/stydye
  lnTotPik = TotPik
  lnTotQty = TotQty
ENDIF  

*
lcAltStyDsc  = IIF(EMPTY(m.AltStyle), SPACE(60),;
                  LOOKUP(STYLE.Desc1, m.AltStyle, STYLE.Style, 'STYLE'))
*

lnMarker = RECNO()  

*
*IF BETWEEN(lnMarker, 1, RECCOUNT())
*  GO lnMarker 
*ENDIF  
*

SHOW WINDOW (lcBrTtl) REFRESH SAME
SHOW GETS WINDOW (lcWinCh32) ONLY
*C200330,1 HBG 04/15/2002 If GMA Trigger and the line is from Pack, and
*C200330,1                this pack is Range Disable the Qty fields [Begin]
IF llGmaPack
  =gfDoTriger('ALPKTKT',PADR('SHWGETS',10)) 
ENDIF
*C200330,1 [End]

=lfRefresh(lcWinCh32)
*
SHOW WINDOW (lcWinCh32) REFRESH SAME
SHOW WINDOW (lcBrTtl) REFRESH
*
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfGetTotStk
*! Developer : IHAB
*! Date      : 11/11/1998
*! Purpose   : Calculates total stock per order line
*!*************************************************************
*! Called From : BROWSE command
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetTotStk()
*!*************************************************************
FUNCTION lfGetTotStk
RETURN     MAX(STYDYE.Stk1-STYDYE.Alo1,0)+;
           MAX(STYDYE.Stk2-STYDYE.Alo2,0)+;
           MAX(STYDYE.Stk3-STYDYE.Alo3,0)+;
           MAX(STYDYE.Stk4-STYDYE.Alo4,0)+;
           MAX(STYDYE.Stk5-STYDYE.Alo5,0)+;
           MAX(STYDYE.Stk6-STYDYE.Alo6,0)+;
           MAX(STYDYE.Stk7-STYDYE.Alo7,0)+;
           MAX(STYDYE.Stk8-STYDYE.Alo8,0)

*!*************************************************************
*! Name      : lpActFold
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/02/98
*! Purpose   : Disable deactive folder objects(used in multi session).
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpActFold
*!*************************************************************
PROCEDURE lpActFold
*DO CASE
*  CASE lnActFolder = 1
*    SHOW GETS WINDOW (lcWinCh3)  DISABLE ONLY
*    SHOW GETS WINDOW (lcWinCh31)  DISABLE ONLY    
*    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
*  CASE lnActFolder = 2
*    SHOW GETS WINDOW (lcWinCh2)   DISABLE ONLY
*    IF laScrMode[3] =.T. 
*      SHOW GETS WINDOW (lcWinCh32)   ENABLE ONLY  
*      SHOW GET m.Style    DISABLE
*      SHOW GET m.dyelot   DISABLE
*      *- If deleted all lines 
*      SELECT (lc_TmpOrdL)
*      IF piktkt <> laData[1] .AND.  order<> laData[3]
*        SHOW GET pbRemove  DISABLE
*      ENDIF
*    ENDIF  
*    *-- Trap Alt+B so that it activates the browse screen
*    ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)
*ENDCASE
**-- end of lpActFold.

*!*************************************************************
*! Name      : lfActFolder
*! Developer : IHAB
*! Date      : 10/14/98
*! Purpose   : Activate folder when Change.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfActFolder()
*!*************************************************************
FUNCTION lfActFolder
STORE "DISABLE" TO lcInstStat,lcShipStat

*IF !llUnComp AND ( ((ASCAN(laScrMode,.T.)<>lnPrvMode) AND INLIST(lnActFolder,2,3) AND !laScrMode[1]) OR ;
*   (!(laData[1]==lcPrvPack)) AND INLIST(lnActFolder,2,3) AND !laScrMode[1]) )
*  = lfvSelOrdL()
*ENDIF

lnLastFold = lnActFolder

DO CASE
  CASE lnActFolder = 1
    SHOW GETS WINDOW (lcWinCh3)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh31)  DISABLE ONLY    
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY

    *= lfRefresh(lcWinHdr)
    
  CASE lnActFolder = 2
    SHOW GETS WINDOW (lcWinCh2)   DISABLE ONLY
    IF laScrMode[3] =.T. 
      SHOW GETS WINDOW (lcWinCh32)   ENABLE ONLY  
      SHOW GET m.Style    DISABLE
      SHOW GET m.dyelot   DISABLE
      *- If deleted all lines 
      SELECT (lc_TmpOrdL)
      IF piktkt <> laData[1] .AND.  order<> laData[3]
        SHOW GET pbRemove  DISABLE
      ENDIF
    ENDIF  
    
    *
    
    *-- Trap Alt+B so that it activates the browse screen
    ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)
  *  SHOW GET ibBrowTrn ENABLE
  *  SHOW GET ibBrowTrn ENABLE

    *
    
ENDCASE

*!*************************************************************
*! Name      : FUNCTION lfvSizeQty
*! Developer : IHAB
*! Date      : 10/21/1998
*! Purpose   : Validate Style size quantity
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: lnSize   : Size 
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSizeQty()
*!*************************************************************
FUNCTION lfvSizeQty
PARAMETERS lcSize,llStores

*!*************************************************************
*! Name      : lfvAloQty
*! Developer : IHAB
*! Date      : 11/19/1998
*! Purpose   : Validation for allocated qtys.
*!
*! Calls       : gfModalGen() , lfRefresh()
*!*************************************************************
*! Passed Parameters : [number representing scale size]
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvAloQty

PARAMETERS lnParm
PRIVATE lcParm , lnCurAlias
*B803402,1 HBG 08/07/2000 If the picked quantities of this pick ticke line are zeros
*B803402,1                this line should be removed from the pick ticket. [Begin]
IF m.Pik1 + m.Pik2 + m.Pik3 + m.Pik4 +;
   m.Pik5 + m.Pik6 + m.Pik7 + m.Pik8 = 0
   =lfvRemove()
	RETURN
ENDIF		
*B803402,1 [End]

lnCurAlias = SELECT(0)
SELECT (lc_TmpOrdL)    

lcObjNum   = RIGHT(SYS(18),1)


*E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt. [Begin]
IF 'AS' $ gcCmpModules AND llSndPkFld
  IF PIKTKT.csndpkt = 'Y' AND m.Pik&lcObjNum <> laPik[lnParm]
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' can not edit')
    m.Pik&lcObjNum  = laPik[lnParm]
    _CUROBJ = OBJNUM(m.Pik&lcObjNum)
    RETURN
  ENDIF
ENDIF
*E130074,1 EIH 01/02/2006 [End]



*-- laPik[lnParm], laQty[lnParm] hold the val. before chg.
IF m.Pik&lcObjNum < 0
  m.Pik&lcObjNum  = laPik[lnParm]
ENDIF
**-- store the old qty. to subtract it when saving in style/stydye
*laOldPik[lnParm] = m.Pik&lcObjNum
*-- If alo. > Ord.
IF m.Pik&lcObjNum > m.Qty&lcObjNum

  *B131018 oms Start
  *-- Message : "Quantity allocated is greater than ordered. Increase "
  *--           "ordered quantity?                                    "
  *--           "                < Yes >           < No >             "
  *IF gfModalGen('TRM44002B00006','DIALOG') = 1
  *  m.TotQty = m.TotQty - m.Qty&lcObjNum   + m.Pik&lcObjNum  
  *  m.Qty&lcObjNum  = m.Pik&lcObjNum  
  *ELSE    
  *  m.Pik&lcObjNum  = laPik[lnParm]
  *ENDIF

  IF gfGetMemVar('M_FORCEALO')='N' AND gfGetMemVar('M_Cost_Meth') $ 'FL'  AND m.Pik&lcObjNum > max(StyDye.Stk&lcObjNum - StyDye.alo&lcObjNum,0)
    =gfModalGen('TRM44000B00000', 'DIALOG','The allocated quantity|the available quantity'+"|"+" ")
    m.Pik&lcObjNum  = laPik[lnParm]
    RETURN
  ELSE  
    IF gfModalGen('TRM44002B00006','DIALOG') = 1
      m.TotQty = m.TotQty - m.Qty&lcObjNum   + m.Pik&lcObjNum  
      m.Qty&lcObjNum  = m.Pik&lcObjNum  
    ELSE    
      m.Pik&lcObjNum  = laPik[lnParm]
    ENDIF
  ENDIF
  *B131018 oms End
ENDIF    
*-- Reassign Totpik
IF m.Pik&lcObjNum <> laPik[lnParm]
  m.TotPik = m.TotPik - laPik[lnParm] + m.Pik&lcObjNum  
  =lfRefresh()
  laPik[lnParm]  = m.Pik&lcObjNum  
  m.cStatus = 'M'
*ELSE
*  m.cStatus = 'S'
ENDIF
*B606999,1 ASH 02/26/2003 (Begin) Fix bug 'End of file encountered'.
*GATHER MEMVAR MEMO
IF !EOF()
  GATHER MEMVAR MEMO
ENDIF
*B606999,1 ASH 02/26/2003 (End)
SHOW WINDOW (lcBrTtl) REFRESH SAME
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvShipping
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid alternative shipping address information
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcAddressNo : Alternative address line #
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipping()
*!*************************************************************
FUNCTION lfvShipping
PARAMETERS lcAddressNo

*!*************************************************************
*! Name      : lfvRemove
*! Developer : IHAB
*! Date      : 10/29/1998
*! Purpose   : Remove Picking Ticket line
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvMRemove()
*!*************************************************************
FUNCTION lfvRemove
PRIVATE lnCurAlias


*E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt. [Begin]
IF 'AS' $ gcCmpModules AND llSndPkFld
  IF PIKTKT.csndpkt = 'Y'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' we can not remove')
    RETURN
  ENDIF
ENDIF
*E130074,1 EIH 01/02/2006 [End]


*C200330,1 HBG 08/05/2002 If trigger of GMA remove the all pack when removing line in it[Begin]
IF llGMAPack 
  =gfDoTriger('ALPKTKT',PADR('REMVPCK',10))
ELSE
*C200330,1 [End]
  *-- Confirm deletion from the user. If he confirms, proceed
  IF gfModalGen('QRM44021B00006','DIALOG')=1
    lnCurAlias = SELECT(0)
  
    SELECT (lc_TmpOrdL)
    *-- Replace cSTatus with 'D'
    REPLACE cStatus WITH 'D'
    DELETE
  
    IF !EOF()
       SKIP-1
    ENDIF
  
    =lfwPikBrow()            && avoid browse if no need
  
    SELECT (lc_TmpOrdL)
    IF EOF()
      SHOW GET pbRemove  DISABLE
      SHOW GET m.Pik1    DISABLE
      SHOW GET m.Pik2    DISABLE  
      SHOW GET m.Pik3    DISABLE  
      SHOW GET m.Pik4    DISABLE  
      SHOW GET m.Pik5    DISABLE  
      SHOW GET m.Pik6    DISABLE  
      SHOW GET m.Pik7    DISABLE  
      SHOW GET m.Pik8    DISABLE  
      SHOW GET m.TotPik  DISABLE  
    ENDIF
    SELECT (lnCurAlias)
  ENDIF
*C200330,1 HBG 08/05/2002 End If trigger of GMA[Begin]
ENDIF
*C200330,1 [End]
*:----------------------------------------------------------------
*: Name       : gfCPDelete
*: Developer  : IHAB
*: Date       : 11/02/1998
*: Purpose    : Global fn. ctrl. panel "Delete", will make "release piktkt".
*:              This function shoud be overwritten in the program
*:              because there is some checks that should be performed
*:              prior to the confirming messge displayed in the 
*:              standard global function.
*:              
*:              This fn. may be called directly by chooosing del. or 
*:              can be called due to deleting all rec. in (lc_tmpordl)  
*:              (Its better to select from the ordline )
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None
*:----------------------------------------------------------------
*: Example    : = gfCPDelete()
*:----------------------------------------------------------------
FUNCTION gfCPDelete
PRIVATE lnCurAlias


*B603318,1 Start.
IF ASCAN(laEvntTrig , PADR('NOCHAGMSG',10)) <> 0 AND PIKTKT.cPickType = 'A'
  llRelPik = .T.
  *--Only Blank pick ticket can be released, the Adornment pickets should be released throw the Release Pick Tickets screen.
  =gfDoTriger('ALPKTKT',PADR('NOCHAGMSG',10))
  RETURN
ENDIF
*B603318,1 End.


*-- Message : "Are you sure you want to release the selected picking ticket "
*--           "                   < Yes >           < No >                  "
*IF gfModalGen("TRM44005B00006","DIALOG") = 1   && 1='Yes', 2='No'
IF gfModalGen("QRM44023B00006" , "DIALOG") = 1

  lnCurAlias = SELECT(0)
  
  SELECT PIKTKT
  
  *E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt. [Begin]
  IF 'AS' $ gcCmpModules AND llSndPkFld
    IF PIKTKT.csndpkt = 'Y'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' can not release ')
      SELECT(lnCurAlias)
      RETURN
    ENDIF
  ENDIF
  *E130074,1 EIH 01/02/2006 [End]



  IF gfObj_Lock(.T.)
     *-- two reasons make us delete from ordline not (lc_tmpordl)  
     *-- one : we may modify the alo. qty. in (lc_tmpordl) for a line
     *-- then release this line . so we must release from the original in ordline
     *-- two : we may release the picktkt in view mode (whereas the (lc_tmpordl)
     *-- is not collected. so we must release from the original in ordline   . 
     *-- set relation is accomplished already (only one time)
     *-- cWareCode of (lc_TmpOrdL) (which is equivalent to ordline)
     *-- this is if changes allocated qtys. ' warehouse (in allocation)
     *-- affects stydye's warehouse in terms of lines' warehouse =.t.   
     SELECT ORDLINE
     lnCurTag = VAL(SYS(21))
     SET ORDER TO TAG OrdLine     && cOrdType + ORDER  + STR(LineNo,6) 
     *-- Scan records of the picking ticket in ORDLINE (no need for go top)
     *-- for now the sequence of the piktkt is not correct.
     *-- so for now we'll add the order to the equity cond.
     SCAN FOR PikTkt = laData[1]  AND Order=ALLTRIM(laData[3])
       *-- saving in pikline
       SCATTER MEMVAR MEMO  
       INSERT INTO PIKLINE FROM MEMVAR 
       =gfAdd_Info('PIKLINE')       && For audit info. 
       
       *-- when affecting/saving in ordline, style, stydye we must chk. the 
       *-- existence of altstyle ..
       *-- if we have altstyle in the ordline . in this case if we release
       *-- this line , then we must do the following :
       *-- assume the altsyle(original) is x and the (new/replacement) style is y
       *-- 1: in the ordline , move altstyle (orignal style) to be the style
       *--    and  clear the altsyle fld.
       *-- 2: in the style (and stydye) move ordered qty from y to x
       *--    and clear pik. qty. for y .
       *-- before changing anything in style and stydye we must chk. 
       *-- whether the scale of the style (y) fit/less than/greater than
       *-- the altstyle(x) . if equal or less then move directly .
       *-- if greater then move all the correspondent scales only and ignore 
       *-- the rest and adjust ordhdr new ordered qtys.
       *C037816,1 MHM 04/06/2004 Custom Release Bin Location For David Luke[Start]
       IF ASCAN(laEvntTrig,PADR("ALRELORD",10)) <> 0
         = gfDoTriger("ALPKTKT",PADR("ALRELORD",10))
       ENDIF 
       *C037816,1 MHM [End]
       SELECT ORDLINE
       IF !EMPTY(m.AltStyle)       && adjust scale and move ordered qtys.
          =lpSwchSty(m.Style, m.AltStyle, m.cWareCode, ORDHDR.cWareCode) 
       ENDIF
       REPLACE Pik1    WITH 0   ,;
               Pik2    WITH 0   ,;
               Pik3    WITH 0   ,;
               Pik4    WITH 0   ,;
               Pik5    WITH 0   ,;
               Pik6    WITH 0   ,;
               Pik7    WITH 0   ,;
               Pik8    WITH 0   ,;
               TotPik  WITH 0   ,;
               PikTkt  WITH ' ' ,;
               Picked  WITH .F. ,;
               PikDate WITH {}
       =gfAdd_Info()
       *-- saving in style (only one rec. carrying the alo. qtys.)
       SELECT STYLE
       =RLOCK()
       REPLACE Alo1 WITH MAX(Alo1 - m.Pik1, 0),;
               Alo2 WITH MAX(Alo2 - m.Pik2, 0),;
               Alo3 WITH MAX(Alo3 - m.Pik3, 0),;
               Alo4 WITH MAX(Alo4 - m.Pik4, 0),;
               Alo5 WITH MAX(Alo5 - m.Pik5, 0),;
               Alo6 WITH MAX(Alo6 - m.Pik6, 0),;
               Alo7 WITH MAX(Alo7 - m.Pik7, 0),;
               Alo8 WITH MAX(Alo8 - m.Pik8, 0),;
               TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
       =gfAdd_Info()      && For audit info. 
       UNLOCK
       *-- saving in stydye (for every rec. in ordline we have - generally -
       *-- two recs. representing it : one for the entire style+cwarecode
       *-- and the other for style+cwarecode+dyelot (if no dyelot then only one))
       SELECT STYDYE      && Repl. for all corresponding recs. 
       IF SEEK(m.Style + m.cWareCode) 
         =RLOCK()
         REPLACE Alo1 WITH MAX( Alo1 - m.Pik1, 0 ),;
                 Alo2 WITH MAX( Alo2 - m.Pik2, 0 ),;
                 Alo3 WITH MAX( Alo3 - m.Pik3, 0 ),;
                 Alo4 WITH MAX( Alo4 - m.Pik4, 0 ),;
                 Alo5 WITH MAX( Alo5 - m.Pik5, 0 ),;
                 Alo6 WITH MAX( Alo6 - m.Pik6, 0 ),;
                 Alo7 WITH MAX( Alo7 - m.Pik7, 0 ),;
                 Alo8 WITH MAX( Alo8 - m.Pik8, 0 ),;
                 TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
         =gfAdd_Info()
         UNLOCK
       ENDIF 
       IF llUseDyes .AND. STYLE.cDye_Flg = 'Y'
         IF SEEK(m.Style + m.cWareCode + m.Dyelot) 
           =RLOCK()
           REPLACE Alo1 WITH MAX( Alo1 - m.Pik1, 0 ),;
                   Alo2 WITH MAX( Alo2 - m.Pik2, 0 ),;
                   Alo3 WITH MAX( Alo3 - m.Pik3, 0 ),;
                   Alo4 WITH MAX( Alo4 - m.Pik4, 0 ),;
                   Alo5 WITH MAX( Alo5 - m.Pik5, 0 ),;
                   Alo6 WITH MAX( Alo6 - m.Pik6, 0 ),;
                   Alo7 WITH MAX( Alo7 - m.Pik7, 0 ),;
                   Alo8 WITH MAX( Alo8 - m.Pik8, 0 ),;
                   TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
           =gfAdd_Info()
           UNLOCK
         ENDIF 
       ENDIF
       
       *-- restoring original style (altstyle) to be the style
       *-- in ordline and clearing the altstyle fld.
       SELECT ORDLINE
       IF !EMPTY(m.AltStyle)
          REPLACE Style        WITH AltStyle  ,;               
                  AltStyle     WITH SPACE(19) ,; 
                  Scale        WITH IIF(SEEK(m.Style, 'STYLE'), SCALE.Scale, ' ') 
       ENDIF  

      
     ENDSCAN  

     *-- saving in piktkt
     SELECT PIKTKT
     REPLACE Status WITH "X"
     =gfAdd_Info()
     SELECT ORDLINE
     SET ORDER TO (lnCurTag)    
     SELECT PIKTKT
     =gfObj_Lock(.F.)
     *B127347,1  TMI [Start] remove piktkt refrence from the POCRTNMF file
     IF ASCAN(laEvntTrig,'REMPKTK')<>0
       =gfDoTriger('ALPKTKT','REMPKTK')
     ENDIF
     *B127347,1  TMI [End] 
     *-- either release while being in edit (i.e. remove all lines),
     *-- or release while in view mode .
     *-- if in case 1 return to view mode .
     *-- if in case 2 return to select mode . (!!!)
     IF laScrMode[3]
       *SCATTER FIELDS &lcScFields TO laData 
       laScrMode    = .F.
       laScrMode[2] = .T.  
       =lpShow()
     ELSE
       *SCATTER FIELDS &lcScFields TO laData BLANK
       laScrMode    = .F.
       laScrMode[1] = .T.  
       SHOW GETS
     ENDIF
   ENDIF
   SELECT(lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lpSwchSty    
*! Developer : IHAB
*! Date      : 11/17/1998
*! Purpose   : Switch ordered quantities between a style + warehouse
*!             and another style + warehouse
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: lcFrmSty  : From style  (new/replacement style)
*!             lcToSty   : To Style    (original style)
*!             lcFrmWare : From warehouse (warehouse of replacement)
*!             lcToWare  : To warehouse   (warehouse of original)
*!*************************************************************
*! Returns   :  .T. if switching is successful
*!*************************************************************
*! Example   :  =lpSwchSty(m.Style, m.cWareCode, ;
*!                         m.AltStyle, m.cWareCode)
*!*************************************************************
PROCEDURE lpSwchSty
PARAMETERS lcFrmStyle, lcToStyle, lcFrmWare, lcToWare 
PRIVATE lcSize, lnSize, lnFrmCnt, lnToCnt, lnCurAlias, lnCanc, lnCancAmt
lnCurAlias = SELECT(0)
*-- Get the scale of the target style
IF SEEK(lcToStyle, 'STYLE') .AND. SEEK('S'+STYLE.Scale, 'SCALE')
  lnToCnt   = SCALE.Cnt
  m.Scale   = SCALE.Scale
  =SEEK(lcFrmStyle, 'STYLE')   
  lnFrmCnt   = SCALE.Cnt                && count of current scale 
                                        && to be altered to the original
                                        
  *-- If the scale is greater, cancel any extra ordered amounts
  STORE 0 TO lnCanc, lnCancAmt
  IF lnFrmCnt > lnToCnt
  
    *-- Cancel any extra ordered amounts in ORDLINE
    
    
    *-- N.B. we must drop all ordered qtys. (not only the extra)
    *-- from style and stydye for the style which is to be changed
    
    
    
    SELECT ORDLINE
    FOR lnSize = lnToCnt + 1 TO lnFrmCnt
      lcSize    = STR(lnSize, 1)
      lnQty     = ORDLINE.Qty&lcSize
      lnCanc    = lnCanc    + lnQty
      lnCancAmt = lnCancAmt + lnQty * ORDLINE.Price
      
      *Renee
      m.Qty&lcSize  = 0
      *Renee end
      
      REPLACE     Qty&lcSize  WITH 0,;
			      TotQty      WITH TotQty      - lnQty,;
				  Book&lcSize WITH Book&lcSize - lnQty,;
			      TotBook     WITH TotBook     - lnQty
      
      *-- Cancel any extra ordered amounts in STYDYE
      SELECT STYDYE
      REPLACE Ord&lcSize WITH MAX(Ord&lcSize - lnQty, 0),;
              TotOrd     WITH TotOrd - Ord&lcSize 
	  
	  *????   Shall we update STYDYE for dyelots too?
      *-- we have the stydye consists of each style qtys. per 
      *-- warehouse and if the co. uses dyelots and the style uses 
      *-- dyelots then we'll have multi records per style representing
      *-- (per one warehouse) each dyelot qtys. plus a record representing
      *-- the total qtys. of this style of these dyelots of this warehouse.
      *-- there's a rule states that : if a style in the order uses dyelots 
      *-- then we CAN NOT change/alternate this style .
      *-- so the updating in stydye for alternative style
      *-- always applied on a style that does not use dyelots .
      *-- meaning only one record in stydye. 
      
      *-- Cancel any extra ordered amounts in STYLE
      SELECT STYLE
      REPLACE Ord&lcSize WITH MAX(Ord&lcSize - lnQty, 0),;
              TotOrd     WITH TotOrd - Ord&lcSize
    ENDFOR
  ENDIF  
  
  *-- if the scales are not matched (meaning scale count 
  *-- of style (from style) is less than or greater than  
  *-- the scale count of altstyle (to style) ,
  *-- don't we need to rescatter ordline before moving qtys. ????
  
  =lpMoveOrd(lcFrmStyle, lcToStyle, lcFrmWare, lcToWare)

  *-- Update the ordered quantities in ARCUSHST (customer history)
  *-- style and its ordered quantities
  IF lnCanc <> 0
    *-- Cancel any extra ordered amounts in ORDHDR
    SELECT ORDHDR               && chk. the correct rec. 
    REPLACE Open      WITH Open    - lnCanc ,;
            OpenAmt   WITH OpenAmt - lnCancAmt ,;
            Book      WITH Book    - lnCanc,;
            BookAmt   WITH BookAmt - lnCancAmt

    SELECT ARCUSHST
    IF SEEK(m.Account + lcGlYear)
      REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod ;
                        			  -ORDLINE.TotQty + m.TotQty,;
              nOrdQty            WITH nOrdQty            ;
						 			  -ORDLINE.TotQty + m.TotQty,;
              nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod -;
				                      (ORDLINE.TotQty - m.TotQty) * ORDLINE.Price,;
              nOrdAmt            WITH nOrdAmt            -;
								      (ORDLINE.TotQty - m.TotQty) * ORDLINE.Price
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpMoveOrd
*! Developer : IHAB
*! Date      : 11/11/1998
*! Purpose   : Move ordered qtys. from a style + warehouse
*!             and another style + warehouse
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: lcFrmSty  : From style
*!             lcToSty   : To Style
*!             lcFrmWare : From warehouse
*!             lcToWare  : To warehouse
*!*************************************************************
*! Returns   :  .T. if switching is successful
*!*************************************************************
*! Example   :  =lpMoveOrd(m.Style, m.cWareCode, ;
*!                         m.AltStyle, m.cWareCode)
*!*************************************************************
PROCEDURE lpMoveOrd
PARAMETERS lcFrmSty, lcToSty, lcFrmWare, lcToWare, lcFrmDye, lcToDye 
PRIVATE lnCurAlias, llRetVal, llSavCont

lnCurAlias = SELECT(0)
llRetVal   =.F.

SELECT STYDYE

*-- If using dyelots, attempt to lock
*-- altstyle is not allowed for co. which use dyelots
IF llUseDyes .AND. STYLE.cDye_flg = 'Y'
  llSavCont = SEEK(lcToSty + lcToWare  + lcToDye) .AND. gfObj_Lock(.T.);
             .AND. SEEK(lcFrmSty + lcFrmWare + lcFrmDye)  .AND. gfObj_Lock(.T.)
ELSE
  llSavCont = .T.   && in case of moving ordered qtys. for altstyle.
                    && meaning only one rec. in stydye to be updated per style.  
ENDIF  
IF llSavCont 
  IF SEEK(lcToSty + lcToWare  + SPACE(10)) .AND. gfObj_Lock(.T.);
    .AND. SEEK(lcFrmSty + lcFrmWare + SPACE(10))  .AND. gfObj_Lock(.T.)
 
    *-- Get the year/period of ORDHDR entered date so as to update
    *-- style history.
    IF CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod ','',.T.)
      SELECT ICSTYHST
      IF SEEK(lcToSty + lcGlYear) .AND. gfObj_Lock(.T.);
         .AND. SEEK(lcFrmSty + lcGlYear) .AND. gfObj_Lock(.T.)
  
        SELECT STYLE
        IF SEEK(lcToSty) .AND. gfObj_Lock(.T.);
          .AND. SEEK(lcFrmSty) .AND. gfObj_Lock(.T.)
          
          *-- it is already deducted for exess scales(!!!).   
          REPLACE Ord1   WITH MAX(Ord1 - m.Qty1, 0),;      
                  Ord2   WITH MAX(Ord2 - m.Qty2, 0),;
                  Ord3   WITH MAX(Ord3 - m.Qty3, 0),;
                  Ord4   WITH MAX(Ord4 - m.Qty4, 0),;
                  Ord5   WITH MAX(Ord5 - m.Qty5, 0),;
                  Ord6   WITH MAX(Ord6 - m.Qty6, 0),;
                  Ord7   WITH MAX(Ord7 - m.Qty7, 0),;
                  Ord8   WITH MAX(Ord8 - m.Qty8, 0),;
                  TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8
              
          SELECT STYDYE
          REPLACE Ord1   WITH MAX(Ord1 - m.Qty1, 0),;
                  Ord2   WITH MAX(Ord2 - m.Qty2, 0),;
                  Ord3   WITH MAX(Ord3 - m.Qty3, 0),;
                  Ord4   WITH MAX(Ord4 - m.Qty4, 0),;
                  Ord5   WITH MAX(Ord5 - m.Qty5, 0),;
                  Ord6   WITH MAX(Ord6 - m.Qty6, 0),;
                  Ord7   WITH MAX(Ord7 - m.Qty7, 0),;
                  Ord8   WITH MAX(Ord8 - m.Qty8, 0),;
                  TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8
                      
          SELECT ICSTYHST
          REPLACE nOrdQty&lcGlPeriod WITH MAX(nOrdQty&lcGlPeriod - m.TotQty, 0),;
                  nOrdQty            WITH MAX(nOrdQty            - m.TotQty, 0),;
                  nOrdAmt&lcGlPeriod WITH MAX(nOrdAmt&lcGlPeriod - m.TotQty * ORDLINE.Price, 0) ,;
                  nOrdAmt            WITH nOrdAmt                - MAX(nOrdAmt&lcGlPeriod - m.TotQty * ORDLINE.Price, 0)

          *-- incrementing ord. qtys. must be with correct vals(!!!). 
          *-- rescatter ordline might be here 
          
          
          SELECT STYLE
          IF SEEK(lcToSty)
            REPLACE Ord1   WITH Ord1 + m.Qty1,;       
                    Ord2   WITH Ord2 + m.Qty2,;
                    Ord3   WITH Ord3 + m.Qty3,;
                    Ord4   WITH Ord4 + m.Qty4,;
                    Ord5   WITH Ord5 + m.Qty5,;
                    Ord6   WITH Ord6 + m.Qty6,;
                    Ord7   WITH Ord7 + m.Qty7,;
                    Ord8   WITH Ord8 + m.Qty8,;
                    TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8
    
          ENDIF
          SELECT STYDYE
          IF SEEK(lcToSty + lcToWare  + SPACE(10)) 
            REPLACE Ord1   WITH Ord1 + m.Qty1,;
                    Ord2   WITH Ord2 + m.Qty2,;
                    Ord3   WITH Ord3 + m.Qty3,;
                    Ord4   WITH Ord4 + m.Qty4,;
                    Ord5   WITH Ord5 + m.Qty5,;
                    Ord6   WITH Ord6 + m.Qty6,;
                    Ord7   WITH Ord7 + m.Qty7,;
                    Ord8   WITH Ord8 + m.Qty8,;
                    TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8
     
          ENDIF
          SELECT ICSTYHST
          IF SEEK(lcToSty + lcGlYear)
            REPLACE nOrdQty&lcGlPeriod WITH MAX(nOrdQty&lcGlPeriod - m.TotQty, 0),;
                   nOrdQty            WITH MAX(nOrdQty            - m.TotQty, 0),;
                   nOrdAmt&lcGlPeriod WITH MAX(nOrdAmt&lcGlPeriod - m.TotQty * ORDLINE.Price, 0) ,;
                   nOrdAmt            WITH nOrdAmt                - MAX(nOrdAmt&lcGlPeriod - m.TotQty * ORDLINE.Price, 0)
          ENDIF
  
          llRetVal   =.T.
        ENDIF &&ENDIF SEEK(..,'STYLE')
        SELECT STYLE
        =SEEK(lcFrmSty) .AND. gfObj_Lock(.F.) 
        =SEEK(lcToSty) .AND. gfObj_Lock(.F.)
      ENDIF  &&ENDIF SEEK(..,'ICSTYHST')
      SELECT ICSTYHST    
      =SEEK(lcFrmSty + lcGlYear) .AND. gfObj_Lock(.F.);
      =SEEK(lcToSty) .AND. gfObj_Lock(.F.)
    ENDIF  &&ENDIF CHECKPRD
    SELECT STYDYE
    *-- Unlock all records
    =SEEK(lcFrmSty + lcFrmWare + SPACE(10))  .AND. gfObj_Lock(.F.)
    =SEEK(lcToSty + lcToWare  + SPACE(10)) .AND. gfObj_Lock(.F.)
  ENDIF
ELSE
  =SEEK(lcToSty + lcToWare  + lcToDye) .AND. gfObj_Lock(.F.)
  =SEEK(lcFrmSty + lcFrmWare + lcFrmDye) .AND. gfObj_Lock(.F.))
ENDIF  

SELECT (lnCurAlias)
RETURN llRetVal

*:----------------------------------------------------------------
*: Name       : lpSavScr
*: Developer  : IHAB
*: Date       : 11/03/1998
*: Purpose    : Local save function.
*:----------------------------------------------------------------
*: Calls      : None.
*:       Called by : gfCPSave
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None
*:----------------------------------------------------------------
*: Example    : = lpSavScr()
*:----------------------------------------------------------------
PROCEDURE lpSavScr
PRIVATE lnAlias


lnAlias    = SELECT(0)
SELECT (lc_TmpOrdL)

IF piktkt <> laData[1] .AND.  order<> laData[3]       && Means all lines are deleted  
  =gfCPDelete()          && release piktkt either in edit (with remove all lines) 
  *=lpDelScr()           && or from view mode directly .   
ELSE 
  *-- relations are already set above
  SELECT ORDLINE
  lnCurTag = VAL(SYS(21))
  SET ORDER TO TAG OrdLine     && cOrdType + ORDER  + STR(LineNo,6) 
  
  SELECT (lc_TmpOrdL)
  lcSetDel = SET('DELETED')
  SET DELETED OFF
  
   
  *B803304,1 Update ordhdr with Book and Open Quantities [Begin]
  *B803304,1 Define Variables hold differences in ordhdr [Begin] 
  PRIVATE lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif , llHaveDiff , lnOrgBook , lnOrgQty
  STORE 0 TO lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif, lnOrgBook , lnOrgQty
  llHaveDiff = .F.  && .T. if differences encounterd
  *B803304,1 Define Variables hold differences in ordhdr [End  ] 

  *C200089,1 Reham On 07/28/1999  *** Begin ***
  *C200089,1 Variable to know if this piktkt is picked partially or completely.
  llPartial = .F.
  *C200089,1 Reham On 07/28/1999  *** End   ***
  SCAN
   
    *B803304,1 Detect old Book and Open Quantities [Begin] 
    IF SEEK(&lc_TmpOrdL..cOrdType + &lc_TmpOrdL..ORDER  + STR(&lc_TmpOrdL..LineNo,6),"ORDLINE")
      lnOrgBook = OrdLine.ToTBook
      lnOrgQty  = OrdLine.TotQty
    ENDIF  
    *B803304,1 Detect old Book and Open Quantities [End  ] 

    *C200089,1 Reham On 07/28/1999  *** Begin ***
    *C200089,1 Check if the current line in the piktkt is picked partially or completely.
    IF !llPartial .AND. (&lc_TmpOrdL..cStatus <> 'D' .AND. OrdLine.TotPik > &lc_TmpOrdL..Pik1+&lc_TmpOrdL..Pik2+&lc_TmpOrdL..Pik3+&lc_TmpOrdL..Pik4+&lc_TmpOrdL..Pik5+&lc_TmpOrdL..Pik6+&lc_TmpOrdL..Pik7+&lc_TmpOrdL..Pik8)
      *C200089,1 Variable to know if this piktkt is picked partially or completely.
      llPartial = .T.
    ENDIF
    *C200089,1 Reham On 07/28/1999  *** End   ***
    
    *-- chk. if the rec. status is 'D' , 'M' or 'S'    
    DO CASE
      CASE cStatus = 'D'
        SCATTER MEMVAR
        *-- saving in ORDLINE
        *C037816,1 MHM 04/06/2004 Custom Release Bin Location For David Luke[Start]
        IF ASCAN(laEvntTrig,PADR("ALRELORD",10)) <> 0
          = gfDoTriger("ALPKTKT",PADR("ALRELORD",10))
        ENDIF 
        *C037816,1 MHM [End]
        SELECT ORDLINE
        *--
        IF !EMPTY(m.AltStyle)       && adjust scale and move ordered qtys.
           =lpSwchSty(m.Style, m.AltStyle, m.cWareCode, ORDHDR.cWareCode) 
        ENDIF
        *--
        IF SEEK(m.cOrdType + m.Order  + STR(m.LineNo,6)) AND DELETED() = .F.
          =RLOCK()
          REPLACE Pik1    WITH 0   ,;
                  Pik2    WITH 0   ,;
                  Pik3    WITH 0   ,;
                  Pik4    WITH 0   ,;
                  Pik5    WITH 0   ,;
                  Pik6    WITH 0   ,;
                  Pik7    WITH 0   ,;
                  Pik8    WITH 0   ,;
                  PikTkt  WITH ' ' ,;
                  TotPik  WITH 0   ,;
                  Picked  WITH .F. ,;
                  PikDate WITH {}
          =gfAdd_Info()
          UNLOCK
        ENDIF
        *-- saving in style
        SELECT STYLE
        =RLOCK()
        REPLACE Alo1 WITH MAX(Alo1 - m.Pik1, 0),;
                Alo2 WITH MAX(Alo2 - m.Pik2, 0),;
                Alo3 WITH MAX(Alo3 - m.Pik3, 0),;
                Alo4 WITH MAX(Alo4 - m.Pik4, 0),;
                Alo5 WITH MAX(Alo5 - m.Pik5, 0),;
                Alo6 WITH MAX(Alo6 - m.Pik6, 0),;
                Alo7 WITH MAX(Alo7 - m.Pik7, 0),;
                Alo8 WITH MAX(Alo8 - m.Pik8, 0),;
                TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
        =gfAdd_Info()      && For audit info. 
        UNLOCK
        
        *-- saving in stydye (for every rec. in ordline we have - generally -
        *-- two recs. representing it : one for the entire style+cwarecode
        *-- and the other for style+cwarecode+dyelot (if no dyelot then only one))
        SELECT STYDYE      && Repl. for all corresponding recs. 
        IF SEEK(m.Style + m.cWareCode) 
          =RLOCK()
          REPLACE Alo1 WITH MAX( Alo1 - m.Pik1, 0 ),;
                  Alo2 WITH MAX( Alo2 - m.Pik2, 0 ),;
                  Alo3 WITH MAX( Alo3 - m.Pik3, 0 ),;
                  Alo4 WITH MAX( Alo4 - m.Pik4, 0 ),;
                  Alo5 WITH MAX( Alo5 - m.Pik5, 0 ),;
                  Alo6 WITH MAX( Alo6 - m.Pik6, 0 ),;
                  Alo7 WITH MAX( Alo7 - m.Pik7, 0 ),;
                  Alo8 WITH MAX( Alo8 - m.Pik8, 0 ),;
                  TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
          =gfAdd_Info()
          UNLOCK
        ENDIF 
        IF STYLE.cDye_Flg = 'Y'
          IF SEEK(m.Style + m.cWareCode + m.Dyelot) 
            =RLOCK()
            REPLACE Alo1 WITH MAX( Alo1 - m.Pik1, 0 ),;
                    Alo2 WITH MAX( Alo2 - m.Pik2, 0 ),;
                    Alo3 WITH MAX( Alo3 - m.Pik3, 0 ),;
                    Alo4 WITH MAX( Alo4 - m.Pik4, 0 ),;
                    Alo5 WITH MAX( Alo5 - m.Pik5, 0 ),;
                    Alo6 WITH MAX( Alo6 - m.Pik6, 0 ),;
                    Alo7 WITH MAX( Alo7 - m.Pik7, 0 ),;
                    Alo8 WITH MAX( Alo8 - m.Pik8, 0 ),;
                    TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
            =gfAdd_Info()
            UNLOCK
          ENDIF 
        ENDIF
        *-- restoring original style (altstyle) to be the style
        *-- in ordline and clearing the altstyle fld.
        SELECT ORDLINE
        IF !EMPTY(m.AltStyle)
           REPLACE Style        WITH AltStyle  ,;               
                   AltStyle     WITH SPACE(19) ,; 
                   Scale        WITH IIF(SEEK(m.Style, 'STYLE'), SCALE.Scale, ' ') 
 *                  lcAltStyDsc  WITH ''
        ENDIF  
        *--
      CASE cStatus = 'M' 
        *--
        SCATTER MEMVAR
        *-- saving in ORDLINE
        *-- we have to encounter also if pik > ord , so modify ord. qty. too
        SELECT ORDLINE
        IF SEEK(m.cOrdType + m.Order  + STR(m.LineNo,6)) AND DELETED() = .F.
          =RLOCK()
          REPLACE Pik1    WITH m.Pik1 ,;
                  Pik2    WITH m.Pik2 ,;
                  Pik3    WITH m.Pik3 ,;
                  Pik4    WITH m.Pik4 ,;
                  Pik5    WITH m.Pik5 ,;
                  Pik6    WITH m.Pik6 ,;
                  Pik7    WITH m.Pik7 ,;
                  Pik8    WITH m.Pik8 ,;
                  TotPik  WITH m.Pik1+m.Pik2+m.Pik3+m.Pik4+m.Pik5+m.Pik6+m.Pik7+m.Pik8
          REPLACE Book1    WITH IIF(Qty1 < m.Pik1,Book1-Qty1+m.Pik1,Book1),;
                  Book2    WITH IIF(Qty2 < m.Pik2,Book2-Qty2+m.Pik2,Book2),;
                  Book3    WITH IIF(Qty3 < m.Pik3,Book3-Qty3+m.Pik3,Book3),;
                  Book4    WITH IIF(Qty4 < m.Pik4,Book4-Qty4+m.Pik4,Book4),;
                  Book5    WITH IIF(Qty5 < m.Pik5,Book5-Qty5+m.Pik5,Book5),;
                  Book6    WITH IIF(Qty6 < m.Pik6,Book6-Qty6+m.Pik6,Book6),;
                  Book7    WITH IIF(Qty7 < m.Pik7,Book7-Qty7+m.Pik7,Book7),;
                  Book8    WITH IIF(Qty8 < m.Pik8,Book8-Qty8+m.Pik8,Book8)
		  REPLACE TotBook  WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8
          REPLACE Qty1     WITH IIF(Qty1 < m.Pik1,m.Pik1,Qty1),;
                  Qty2     WITH IIF(Qty2 < m.Pik2,m.Pik2,Qty2),;
                  Qty3     WITH IIF(Qty3 < m.Pik3,m.Pik3,Qty3),;
                  Qty4     WITH IIF(Qty4 < m.Pik4,m.Pik4,Qty4),;
                  Qty5     WITH IIF(Qty5 < m.Pik5,m.Pik5,Qty5),;
                  Qty6     WITH IIF(Qty6 < m.Pik6,m.Pik6,Qty6),;
                  Qty7     WITH IIF(Qty7 < m.Pik7,m.Pik7,Qty7),;
                  Qty8     WITH IIF(Qty8 < m.Pik8,m.Pik8,Qty8)
		  REPLACE TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
          =gfAdd_Info()
          UNLOCK
        ENDIF
        *-- saving in style
        *-- we have to encounter the old value because it may be less
        *-- or greater than the new.
        
        SELECT STYLE
        =RLOCK()
        *B804372,1 HBG 30/08/2001 Update fields of old PikQty insted of the array [Begin]
        *REPLACE Alo1 WITH MAX(Alo1 - laOldPik[1] + m.Pik1, 0),;
                Alo2 WITH MAX(Alo2 - laOldPik[2] + m.Pik2, 0),;
                Alo3 WITH MAX(Alo3 - laOldPik[3] + m.Pik3, 0),;
                Alo4 WITH MAX(Alo4 - laOldPik[4] + m.Pik4, 0),;
                Alo5 WITH MAX(Alo5 - laOldPik[5] + m.Pik5, 0),;
                Alo6 WITH MAX(Alo6 - laOldPik[6] + m.Pik6, 0),;
                Alo7 WITH MAX(Alo7 - laOldPik[7] + m.Pik7, 0),;
                Alo8 WITH MAX(Alo8 - laOldPik[8] + m.Pik8, 0),;
                TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
        REPLACE Alo1 WITH MAX(Alo1 - m.OldPik1 + m.Pik1, 0),;
                Alo2 WITH MAX(Alo2 - m.OldPik2 + m.Pik2, 0),;
                Alo3 WITH MAX(Alo3 - m.OldPik3 + m.Pik3, 0),;
                Alo4 WITH MAX(Alo4 - m.OldPik4 + m.Pik4, 0),;
                Alo5 WITH MAX(Alo5 - m.OldPik5 + m.Pik5, 0),;
                Alo6 WITH MAX(Alo6 - m.OldPik6 + m.Pik6, 0),;
                Alo7 WITH MAX(Alo7 - m.OldPik7 + m.Pik7, 0),;
                Alo8 WITH MAX(Alo8 - m.OldPik8 + m.Pik8, 0),;
                TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
        REPLACE Ord1 WITH IIF(m.Pik1 > m.Ordln1,MAX(Ord1 - m.Ordln1 + m.Pik1, 0),Ord1),;
                Ord2 WITH IIF(m.Pik2 > m.Ordln2,MAX(Ord2 - m.Ordln2 + m.Pik2, 0),Ord2),;
                Ord3 WITH IIF(m.Pik3 > m.Ordln3,MAX(Ord3 - m.Ordln3 + m.Pik3, 0),Ord3),;
                Ord4 WITH IIF(m.Pik4 > m.Ordln4,MAX(Ord4 - m.Ordln4 + m.Pik4, 0),Ord4),;
                Ord5 WITH IIF(m.Pik5 > m.Ordln5,MAX(Ord5 - m.Ordln5 + m.Pik5, 0),Ord5),;
                Ord6 WITH IIF(m.Pik6 > m.Ordln6,MAX(Ord6 - m.Ordln6 + m.Pik6, 0),Ord6),;
                Ord7 WITH IIF(m.Pik7 > m.Ordln7,MAX(Ord7 - m.Ordln7 + m.Pik7, 0),Ord7),;
                Ord8 WITH IIF(m.Pik8 > m.Ordln8,MAX(Ord8 - m.Ordln8 + m.Pik8, 0),Ord8),;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8        
        *B804372,1 [End]
        =gfAdd_Info()      && For audit info. 
        UNLOCK
        *C037816,1 MHM 04/06/2004 Custom Save Bin Location For David Luke[Start]
        *B039660,1 NNA 02/14/2006 (Begin) save changes in the pick ticket screen as in the allocation by order
        IF ASCAN(laEvntTrig,PADR("ALSAVORD",10)) <> 0
          SET DELETED ON
          = gfDoTriger("ALPKTKT",PADR("ALSAVORD",10))
          SET DELETED OFF
        ENDIF 
        *IF ASCAN(laEvntTrig,PADR("ALSAVPKT",10)) <> 0
        *  SET DELETED ON
        *  = gfDoTriger("ALPKTKT",PADR("ALSAVPKT",10))
        *  SET DELETED OFF
        *ENDIF 
        *B039660,1 NNA (End)
        *C037816,1 MHM [End]
        
        *-- saving in stydye (for every rec. in ordline we have - generally -
        *-- two recs. representing it : one for the entire style+cwarecode
        *-- and the other for style+cwarecode+dyelot (if no dyelot then only one))
        SELECT STYDYE      && Repl. for all corresponding recs. 
        IF SEEK(m.Style + m.cWareCode) 
          =RLOCK()
          *B804372,1 HBG 30/08/2001 Update fields of old PikQty insted of the array [Begin]
          *REPLACE Alo1 WITH MAX(Alo1 - laOldPik[1] + m.Pik1, 0),;
                  Alo2 WITH MAX(Alo2 - laOldPik[2] + m.Pik2, 0),;
                  Alo3 WITH MAX(Alo3 - laOldPik[3] + m.Pik3, 0),;
                  Alo4 WITH MAX(Alo4 - laOldPik[4] + m.Pik4, 0),;
                  Alo5 WITH MAX(Alo5 - laOldPik[5] + m.Pik5, 0),;
                  Alo6 WITH MAX(Alo6 - laOldPik[6] + m.Pik6, 0),;
                  Alo7 WITH MAX(Alo7 - laOldPik[7] + m.Pik7, 0),;
                  Alo8 WITH MAX(Alo8 - laOldPik[8] + m.Pik8, 0),;
                  TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
          REPLACE Alo1 WITH MAX(Alo1 - m.OldPik1 + m.Pik1, 0),;
                  Alo2 WITH MAX(Alo2 - m.OldPik2 + m.Pik2, 0),;
                  Alo3 WITH MAX(Alo3 - m.OldPik3 + m.Pik3, 0),;
                  Alo4 WITH MAX(Alo4 - m.OldPik4 + m.Pik4, 0),;
                  Alo5 WITH MAX(Alo5 - m.OldPik5 + m.Pik5, 0),;
                  Alo6 WITH MAX(Alo6 - m.OldPik6 + m.Pik6, 0),;
                  Alo7 WITH MAX(Alo7 - m.OldPik7 + m.Pik7, 0),;
                  Alo8 WITH MAX(Alo8 - m.OldPik8 + m.Pik8, 0),;
                  TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
          REPLACE Ord1 WITH IIF(m.Pik1 > Ord1,MAX(Ord1 - m.OldPik1 + m.Pik1, 0),Ord1),;
                  Ord2 WITH IIF(m.Pik2 > Ord2,MAX(Ord2 - m.OldPik2 + m.Pik2, 0),Ord2),;
                  Ord3 WITH IIF(m.Pik3 > Ord3,MAX(Ord3 - m.OldPik3 + m.Pik3, 0),Ord3),;
                  Ord4 WITH IIF(m.Pik4 > Ord4,MAX(Ord4 - m.OldPik4 + m.Pik4, 0),Ord4),;
                  Ord5 WITH IIF(m.Pik5 > Ord5,MAX(Ord5 - m.OldPik5 + m.Pik5, 0),Ord5),;
                  Ord6 WITH IIF(m.Pik6 > Ord6,MAX(Ord6 - m.OldPik6 + m.Pik6, 0),Ord6),;
                  Ord7 WITH IIF(m.Pik7 > Ord7,MAX(Ord7 - m.OldPik7 + m.Pik7, 0),Ord7),;
                  Ord8 WITH IIF(m.Pik8 > Ord8,MAX(Ord8 - m.OldPik8 + m.Pik8, 0),Ord8),;
                  TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8                     
          *B804372,1 [End]
          =gfAdd_Info()
          UNLOCK
        ENDIF 
        IF STYLE.cDye_Flg = 'Y'
          IF SEEK(m.Style + m.cWareCode + m.Dyelot) 
            =RLOCK()
            *B804372,1 HBG 30/08/2001 Update fields of old PikQty insted of the array [Begin]
            *REPLACE Alo1 WITH MAX(Alo1 - laOldPik[1] + m.Pik1, 0),;
                    Alo2 WITH MAX(Alo2 - laOldPik[2] + m.Pik2, 0),;
                    Alo3 WITH MAX(Alo3 - laOldPik[3] + m.Pik3, 0),;
                    Alo4 WITH MAX(Alo4 - laOldPik[4] + m.Pik4, 0),;
                    Alo5 WITH MAX(Alo5 - laOldPik[5] + m.Pik5, 0),;
                    Alo6 WITH MAX(Alo6 - laOldPik[6] + m.Pik6, 0),;
                    Alo7 WITH MAX(Alo7 - laOldPik[7] + m.Pik7, 0),;
                    Alo8 WITH MAX(Alo8 - laOldPik[8] + m.Pik8, 0),;
                    TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
            REPLACE Alo1 WITH MAX(Alo1 - m.OldPik1 + m.Pik1, 0),;
                    Alo2 WITH MAX(Alo2 - m.OldPik2 + m.Pik2, 0),;
                    Alo3 WITH MAX(Alo3 - m.OldPik3 + m.Pik3, 0),;
                    Alo4 WITH MAX(Alo4 - m.OldPik4 + m.Pik4, 0),;
                    Alo5 WITH MAX(Alo5 - m.OldPik5 + m.Pik5, 0),;
                    Alo6 WITH MAX(Alo6 - m.OldPik6 + m.Pik6, 0),;
                    Alo7 WITH MAX(Alo7 - m.OldPik7 + m.Pik7, 0),;
                    Alo8 WITH MAX(Alo8 - m.OldPik8 + m.Pik8, 0),;
                    TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
            *B804372,1 [End]
            =gfAdd_Info()
            UNLOCK
          ENDIF 
        ENDIF
      *--    
    ENDCASE


    *B803304,1 Calculate differences per line and accomulate it [Begin] 
    IF SEEK(&lc_TmpOrdL..cOrdType + &lc_TmpOrdL..ORDER  + STR(&lc_TmpOrdL..LineNo,6),"ORDLINE")
      lnBookDiff = lnBookDiff + (OrdLine.TotBook - lnOrgBook)
      lnBkAmtDif = lnBkAmtDif + ((OrdLine.TotBook - lnOrgBook) * OrdLine.Price)

      lnQtyDiff  = lnQtyDiff  + (OrdLine.TotQty - lnOrgQty)
      lnQyAmtDif = lnQyAmtDif + ((OrdLine.TotQty - lnOrgQty) * OrdLine.Price)      
    ENDIF  
    *B803304,1 Calculate differences per line and accomulate it [End  ] 

 
  ENDSCAN 
 
  *B803304,1 Detect if there are difference and then update ordhdr [Begin] 
  *-- MAB Note : Check for quantity is not sufficient because we may have the same quantity
  *--            but the user increase one line and decrease another, and these two lines have 
  *--            different prices. 
  llHaveDiff = (lnBookDiff != 0) OR (lnQtyDiff  != 0) OR ;
               (lnBkAmtDif != 0) OR (lnQyAmtDif != 0)

  IF llHaveDiff
    SELECT OrdHdr
    =RLOCK()
    REPLACE BOOK    WITH BOOK    + lnBookDiff ,;
            BOOKAMT WITH BOOKAMT + lnBkAmtDif ,;
            OPEN    WITH OPEN    + lnQtyDiff  ,;
            OPENAMT WITH OPENAMT + lnQyAmtDif
    UNLOCK        
    =gfAdd_Info()

  ENDIF
  *B803304,1 Detect if there are difference and then update ordhdr [End  ] 

 
   
  SET DELETED &lcSetDel

  *-- saving in piktkt
  SELECT PIKTKT
  
  *C200089,1 Reham On 07/22/1999  *** Begin ***
  *C200089,1 When Order header status changed to "Open", Call process Id to
  *C200089,1 execute special function.
  IF PikTkt.Status <> laData[2] .AND. laData[2] = "P"
    *C200089,1 If pick paritally.
    IF llPartial
      IF ASCAN(laEvntTrig , PADR('PUL_PAR',10)) <> 0
        llReturn = gfDoTriger('ALPKTKT',PADR('PUL_PAR',10))
      ENDIF
    ELSE
    *C200089,1 If pick completely.
      IF ASCAN(laEvntTrig , PADR('PUL_COM',10)) <> 0
        llReturn = gfDoTriger('ALPKTKT',PADR('PUL_COM',10))
      ENDIF
    ENDIF
  ENDIF
  *C200089,1 Reham On 07/22/1999  *** End   ***

  *C200089,1 Reham On 07/22/1999  *** Begin ***
  *C200089,1 When Order header status changed to "Open", Call process Id to
  *C200089,1 execute special function.
  IF PikTkt.Status = "P" .AND. laData[2] = "O" .AND. ASCAN(laEvntTrig , PADR('PUL_OPN',10)) <> 0
    llReturn = gfDoTriger('ALPKTKT',PADR('PUL_OPN',10))
  ENDIF
  *C200089,1 Reham On 07/22/1999  *** End   ***
  
  *E301273,1 Reham On 06/22/1999   *** Begin ***
  *E301273,1 Replace with on the following status : Open, On Hold, Pulled.
  *REPLACE Status WITH IIF(lcPkTktStatus='Open','O','H')
  REPLACE Status WITH IIF(ASCAN(laPkTktStatus,lcPkTktStatus) > 0, laPkTktStatus[ASUBSCRIPT(laPkTktStatus,ASCAN(laPkTktStatus,lcPkTktStatus),1),2] ,'H')
  *E301273,1 Reham On 06/22/1999   *** End   ***
  =gfAdd_Info()

  SELECT ORDLINE
  SET ORDER TO (lnCurTag)    

  SELECT PIKTKT
  SCATTER FIELDS &lcScFields TO laData 

  laScrMode    = .F.
  laScrMode[2] = .T.  
  =lpShow()
ENDIF

SELECT(lnAlias)

**!*************************************************************
**! Name      : lpClsScr
**! Developer : IAHB
**! Date      : 11/04/1998
**! Purpose   : Undo
**!*************************************************************
**! Calls     : None
**!*************************************************************
**! Passed Parameters  :  None
**!*************************************************************
**! Returns            :  None
**!*************************************************************
**! Example            :  =lpClsScr()
**!*************************************************************
*FUNCTION lpClsScr
*llRecall = .T.
**lpClsScr && Undo if laDefProc[10] = .F.

*!*************************************************************
*! Name      : lfwOldVar
*! Developer : IHAB
*! Date      : 11/08/1998
*! Purpose   : Valid function for push button < Next > (pbNext).
*!             Displays the next record on the file.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfwOldVar()
*!*************************************************************
FUNCTION lfwOldVar
*-- SYS(18) Returns the name in upper-case of the memory variable, 
*-- array element or field used to create the current @ ... GET control.
lcOldVal = EVALUATE(SYS(18))  

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : IHAB
*! Date      : 11/19/1998
*! Purpose   : When function to store the contents of the current 
*!             object which might be changed.  
*!
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*!*************************************************************
*! Name      : lfvWareCode
*! Developer : IHAB
*! Date      : 11/08/1998
*! Purpose   : Valid function for get field 'cWareCode' (Warehouse)
*!             Validates an entered warehouse or browses for one.
*!             Checks if Fabric + Color is assigned to the warhouse
*!             If not, adds a record in FABDYE
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvWareHouse()
*!*************************************************************
FUNCTION lfvWareCode
IF puWareHous <>lcOldVal
  *-- If a picking ticket exists for the current line, 
  *-- use its pick from warehouse, 
  *-- Else, use the default warehouse for picking.  
  
  *-- Use the selected warehouse for any line that is not picked
  lcWareCode = laWareHous[puWareHous, 2]
  
  *lnCurAlias = SELECT(0)
  *SELECT ORDLINE
  *lnRecNo = RECNO()
  *IF SEEK(laData[9] + laData[1])
  *  lcScanExp = IIF(!EMPTY(lcBrowExp),;
  *     		      [WHILE ] + lcNdxExp + [ = laData(9) + laData(1) + ] +;
  *     		      IIF(EMPTY(lcStore), [STORE ], [lcStore]) + ;
  *     		      IIF(EMPTY(lcStyle), [], [+ lcStyle]);
  *     		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ AND Group = lcGroup],[]),;
  *     		      [REST WHILE cOrdType + Order = laData(9) + laData(1)];
  *     		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ WHILE Group = lcGroup],[]))
  *  SCAN REST &lcScanExp ;
  *       FOR !lfLinIsPck()
  *    =lfAddLine(lcWareCode)
  *  ENDSCAN
  *ENDIF  
  *IF BETWEEN(lnRecNo, 1, RECCOUNT())
  *  GO lnRecNo
  *ELSE
  *  GO TOP
  *ENDIF
  *IF !EOF(lc_TmpOrdL)
  *  SELECT (lc_TmpOrdL)
  *ENDIF  
  *SCATTER MEMVAR MEMO  
  *IF !lfLinIsPck()
  *  puPickWare = puWareHous
  *  SHOW GET puPickWare
  *ENDIF
  *SELECT (lnCurAlias)
  *SHOW WINDOW (lcOrdBrTtl) REFRESH SAME
  *=lfRefresh(lcAlOrdAl3)
ENDIF

*-- using key traps to handle tabs.
*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Activate function of INV700
*!*************************************************************
*! Calls     : gfClearKey.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
*-- Restore key settings
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)
ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*C200330,1 HBG 08/05/2002 IF trigger to GMA to Add option menu to sort it by pack or line # [Begin] 
IF llGMAPack 
  =gfDoTriger('ALPKTKT',PADR('ADDBAR',10)) 
ENDIF
*C200330,1 [End]

*- Clear browse as you are coming out of a browse window
IF glFromBrow
  =gfStopBrow()
ENDIF  
SELECT PIKTKT

*!*************************************************************
*! Name      : lfReadDAct
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Deactivate function of screen Ord100
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfReadDAct()
*!*************************************************************
FUNCTION lfReadDAct
ON KEY LABEL ALT+B
IF WONTOP() = (lcBrTtl)
  *-- Set browsing variable
  glFromBrow  = .T.
  *-- Trap keys
  ON KEY LABEL TAB         DO lpTab WITH lcWinCh32, OBJNUM(pbRemove)
  IF laScrMode[2] .OR. laScrMode[3]
    ON KEY LABEL BACKTAB   DO lpTab WITH 'gwcContrl1', OBJNUM(pbCls)
  ELSE
    ON KEY LABEL BACKTAB   DO lpTab WITH lcWinCh1,  OBJNUM(laData[2])
  ENDIF  
ENDIF
*B123702,1  TMI [Start] 
*RETURN .F.
*B123702,1  TMI [End  ] 

*!*************************************************************
*! Name      : lpTab
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'ORD100P', 'pbApply'
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lnObjNum
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum

*****************************************************************
*********  Add    an options menu       *************************
*

*-- glReadWhen .AND. lfActPad() is written in 'on screen entry (when)'
*-- in the base screen layout code .

**!*************************************************************
**! Name      : lfActPad
**! Developer : IHAB
**! Date      : 11/23/1998
**! Purpose   : Bulid a new menu pad [Options]
**!*************************************************************
**! Calls     : lpvInquiry
**!*************************************************************
**! Passed Parameters  : None
**!*************************************************************
**! Returns            :  None
**!*************************************************************
**! Example            :  =lfActPad()
**!*************************************************************
*FUNCTION lfActPad
**-- Options menu pad definition
*DEFINE PAD _OPTION OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P,SPACE(1)
*ON PAD _OPTION OF _MSYSMENU ACTIVATE POPUP _OPTIONPOP
*
**-- Options popup definition
*DEFINE POPUP _OPTIONPOP MARGIN SHADOW
*DEFINE BAR 1  OF _OPTIONPOP PROMPT 'Customer Inquire' SKIP FOR laScrMode[1]
*DEFINE BAR 2  OF _OPTIONPOP PROMPT 'Style Inquire' SKIP FOR laScrMode[1]
*DEFINE BAR 3  OF _OPTIONPOP PROMPT 'Sort by Line#' SKIP FOR laScrMode[1] 
**.OR. !EMPTY(lcBrowExp)
*DEFINE BAR 4  OF _OPTIONPOP PROMPT 'Sort by Store' SKIP FOR laScrMode[1]
*DEFINE BAR 5  OF _OPTIONPOP PROMPT 'Scope on...'   SKIP FOR .T. &&laScrMode[1]
*DEFINE BAR 6  OF _OPTIONPOP PROMPT 'Zoom Order Lines'       
*SET MARK OF BAR 3 OF _OPTIONPOP TO !llOrdLStr
*SET MARK OF BAR 4 OF _OPTIONPOP TO llOrdLStr
*ON SELECTION POPUP _OPTIONPOP DO lfvOptions WITH BAR()

**!*************************************************************
**! Name      : lfvOptions
**! Developer : IHAB
**! Date      : 11/23/1998
**! Purpose   : Bulid a new menu pad [Options]
**!*************************************************************
**! Calls     : lpvInquiry,lfvSortBy,lfvQtyPSize,lfvDetails,lfvLNote,;
**!             lfvCopyQty,lfvPasteQty,lfvBrowZoom
**!*************************************************************
**! Passed Parameters  : None
**!*************************************************************
**! Returns            :  None
**!*************************************************************
**! Example            :  =lpvInquiry()
**!*************************************************************
*FUNCTION lfvOptions
*PARAMETERS lnBarNo
*PRIVATE lnCurAlias

*lnCurALias = SELECT()
*DO CASE
*  CASE lnBarNo = 1      && Customer Inquire
*    lcParameter = [']+laData[2]+[']+IIF(EMPTY(m.Store),[],[,']+m.Store+['])
*    DO gpDoProg WITH 'AWRARCUST',.F.,'AR',lcParameter
*  
*  CASE lnBarNo = 2      && Style Inquire
*  	lcParameter = "'"+m.Style+"'"
*    DO gpDoProg WITH 'AWRICSTYLE',.F.,'IC',lcParameter
*  
*  CASE lnBarNo = 3
*    SET MARK OF BAR 3 OF _OPTIONPOP TO .T.  
*    SET MARK OF BAR 4 OF _OPTIONPOP TO .F.
*    SELECT ORDLINE
*    SET ORDER TO TAG ORDLINE 
*    llOrdLStr  = .F.
*    lcNdxExp   = SYS(14,VAL(SYS(21)))
*    SHOW WINDOW (lcBrTtl) REFRESH SAME
*    
*  CASE lnBarNo = 4
*    SET MARK OF BAR 3 OF _OPTIONPOP TO .F.  
*    SET MARK OF BAR 4 OF _OPTIONPOP TO .T.
*    SELECT ORDLINE
*    SET ORDER TO TAG ORDLINST 
*    llOrdLStr  = .T.
*    lcNdxExp   = SYS(14,VAL(SYS(21)))
*    SHOW WINDOW (lcBrTtl) REFRESH SAME
*    
*  *-- Scope option
*  CASE lnBarNo = 5
*    SET MARK OF BAR 6 OF _OPTIONPOP TO !llZoomed
*    =lfvScope()
*
*  *-- Zoom option
*  CASE lnBarNo = 6
*    SET MARK OF BAR 6 OF _OPTIONPOP TO !llZoomed
*    =lfvZoom()
*ENDCASE
*SELECT (lnCurALias)
*
**!*************************************************************
**! Name      : lfvScope
**! Developer : IHAB
**! Date      : 11/23/1998
**! Purpose   : Adjust stydye file
**!*************************************************************
**! Calls     : lfRefresh()
**!*************************************************************
**! Passed Parameters  :  None
**!*************************************************************
**! Returns            :  None
**!*************************************************************
**! Example            :  =lfShowLines()
**!*************************************************************
*FUNCTION lfvScope
*PRIVATE lcOldStyle, lcOldStore, lcOldGroup, lcOldScope, llScoped
*llScoped = .T.
*
**-- Store old variables:
*lcOldStyle = lcStyle
*lcOldStore = lcStore
*lcOldGroup = lcGroup
*lcOldScope = lcBrowExp
*
*lcStyDesc  = IIF(EMPTY(lcStyle), '', LOOKUP(STYLE.Desc1, lcStyle, STYLE.Style, 'STYLE'))
*lcStorDesc = IIF(EMPTY(lcStore), '',;
*				 LOOKUP(CUSTOMER.StName, 'S'+laData[2] + lcStore,;
*				 CUSTOMER.Type, 'CUSTOMER')) 
*PUSH KEY
*ON KEY
*DO (gcScrDir + gcWinAppl + '\ALORDALS.SPR')
*POP KEY
*
**-- If the user selects to scope, set order of ORDLINE file to store,
**-- or style.
*IF llScoped 
*  *-- If the scope is changed, 
*  IF lcOldStyle <> lcStyle .OR. lcOldStore <> lcStore .OR. ;
*  	 lcOldGroup <> lcGroup
*  	IF !EMPTY(lcStore + lcStyle + lcGroup) 
*      IF !llOrdLStr
*        SET MARK OF BAR 3 OF _OPTIONPOP TO .F.  
*        SET MARK OF BAR 4 OF _OPTIONPOP TO .T.
*        SELECT ORDLINE
*        SET ORDER TO TAG ORDLINST 
*        llOrdLStr  = .T.
*        lcNdxExp   = SYS(14,VAL(SYS(21)))
*      ENDIF  
* 	  lcBrowExp  =  IIF(cbMultiStore,;
*         			   IIF(!EMPTY(lcStore), lcStore + IIF(!EMPTY(lcStyle), lcStyle, ''),;
*  				       IIF(!EMPTY(lcStyle), '.AND. Style = lcStyle', '')),;
*    				IIF(!EMPTY(lcStyle), ORDHDR.Store + lcStyle, ''))    				
*    ELSE
*      lcBrowExp  =  ''
*    ENDIF
*    SHOW WINDOW (lcBrTtl) REFRESH SAME
*  ENDIF  
*ELSE
*  *-- ReStore old variables:
*  lcStyle   = lcOldStyle
*  lcStore   = lcOldStore 
*  lcGroup   = lcOldGroup
*  lcBrowExp = lcOldScope 
*ENDIF

**!*************************************************************
**! Name      : lfvZoom
**! Developer : IHAB
**! Date      : 11/23/1998
**! Purpose   : Zoom function
**!*************************************************************
**! Calls     : None
**!*************************************************************
**! Passed Parameters  :  None
**!*************************************************************
**! Returns            :  None
**!*************************************************************
**! Example            :  =lfvZoom()
**!*************************************************************
*FUNCTION lfvZoom
*IF llZoomed
*  llZoomed = .F.
*  *-- Change the size of the browse window
*  ZOOM WINDOW (lcAlOrdal1) NORM;
*  		 SIZE WROWS(lcAlOrdal1) - lnSizOffst , WCOLS(lcAlOrdal1)  	  		 
*  ZOOM WINDOW (lcOrdBrTtl) NORM;
*  		 SIZE WROWS(lcAlOrdAl1) + 1.3,  WCOLS(lcAlOrdal1) + 3.3  	  		 
*  =lfRefresh(gcBaseWind)
*  *-- Move buttons window up   
*  
*  *--  NEED REVISION
*  MOVE WINDOW (lcAlOrdAl2) BY - (lnSizOffst + .9) - 0.083, 0
*  SHOW WINDOW (lcAlOrdAl3)  
*  *--  NEED REVISION
*  
*  =lfwOrdBrow()
*ELSE
*  llZoomed = .T.
*  
*  *--  NEED REVISION
*  *-- Hide details window
*  SHOW GETS WINDOW (lcAlOrdAl3) DISABLE ONLY
*  HIDE WINDOW (lcAlOrdAl3)
*  *-- Move buttons window down
*  MOVE WINDOW (lcAlOrdAl2) BY lnSizOffst + .9, 0
*  *-- Change the size of the browse window
*  ZOOM WINDOW (lcAlOrdal1) NORM;
*  		 SIZE WROWS(lcAlOrdal1) + lnSizOffst , WCOLS(lcAlOrdal1)  	  
*  ZOOM WINDOW (lcOrdBrTtl) NORM;
* 		 SIZE WROWS(lcAlOrdAl1) + 1.3, WCOLS(lcAlOrdal1) + 3.3  	  		 
*
*  *--  NEED REVISION
*ENDIF  
*****************************************************************

*!*************************************************************
*! Name      : lfSekPak
*! Developer : Ihab Abd Elmawgood (IHB)
*! Date      : 02/21/1999
*! Purpose   : Function to check if the Pick ticket has a packing list
*!*************************************************************
*! Called from : lpShow() (in view mode)
*!*************************************************************
*! Calls       : gfOpenFile()
*!*************************************************************
*! Reference   : *B602546,1 IHB
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
FUNCTION lfSekPak
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
=gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'PACK_HDR','SH')
SELECT (lnCurAlias)
*-- if The Pick ticket has a packing list
IF SEEK(PIKTKT.PIKTKT,"PACK_HDR")
  RETURN .T.
ELSE
  RETURN .F.
ENDIF

*!*************************************************************
*! Name            : gfCPEdit
*! Developer       : Ramy Mabrouk (RAMY)
*! Date            : 08/30/1999
*! Purpose         : Function to overwrite the valid function
*!                   of the Tool Bar Edit button to check if the
*!                   Customer is active or not, if the customer
*!                   is active we are going to call valid function
*!                   of the Tool Bar Edit button from AAS_WIN.EXE.
*! Tracking Job No : B603084,1
*!*************************************************************
*! Called from : Edit button (Control Panel in Picking Ticket Screen)
*!*************************************************************
*! Calls       : gfModalGen()
*!               gfCPEdit() 'From AAS_WIN.EXE'
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION gfCPEdit

* If the customer status in non-active
IF CUSTOMER.Status <> 'A'
  *-- Message : 46036
  *--             "This is a non-active account."
  *-- Button  : 00000
  *--                        <  Ok  >
  =gfModalGen('TRM46036B00000','ALERT')
ELSE  && ELSE (If the customer status is active)

  *B603318,1 Start.
  IF ASCAN(laEvntTrig , PADR('NOCHAGMSG',10)) <> 0 AND PIKTKT.cPickType = 'A'
    llRelPik = .F.
    *--Only Blank pick ticket can be modified, the Adornment pickets should be modified or pulled throw the Issue Adornment order process.
    =gfDoTriger('ALPKTKT',PADR('NOCHAGMSG',10))
    RETURN
  ENDIF
  *B603318,1 End.

  *-- call the valid function of the Tool Bar Edit button from AAS_WIN.EXE
  DO gfCPEdit IN AAS_WIN.EXE
ENDIF    && End of IF CUSTOMER.Status <> 'A'


*!*************************************************************
*! Name            : lfwRepWhen
*! Developer       : TAK
*! Date            : 11/09/2000
*!*************************************************************
FUNCTION lfwRepWhen
*--The only reason for this function is to avoid error in user
*--defined screen (optional Grid), due to conflict with P/T form.
RETURN



*! This function is only for GMA.
*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : HEnd Ghanem (HBG)
*! Date      : 12/10/2001
*! Purpose   : Function To Size discreption
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfGetGmSz()
*!*************************************************************
*!200330
FUNCTION lfGetGmSz
PARAMETER lcPackSize

IF !EMPTY(lcPackSize) AND SEEK('S'+LEFT(lcPackSize,1),'SCALE')
  lcSiz = RIGHT(lcPackSize,1)
  lcSize = SCALE.Sz&lcSiz
ELSE
  lcSize = '*****'    
ENDIF  

RETURN lcSize 
