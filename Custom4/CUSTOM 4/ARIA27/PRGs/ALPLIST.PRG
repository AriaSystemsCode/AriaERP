************************************************************
*: Program file  : ALPLIST
*: Program desc. : Create Packing List for Order or PikTkt
*: For screen    : ALPLIST
*:        System : Aria Advantage Series.
*:        Module : Allocation (AL).
*:     Developer : Ahmed Amer (AHM)
*:************************************************************
*: Calls :
*:         Procedures : lpShow,lpSavscr
*:         Functions  : gfSetup,gfOpenFile,lfvData_1,lfvData_2,
*:                    : lfvData_3,lfvData_4,lfvData_5,lfvData_13,lfvData_14
*:                    : lfvShpVia,lfTrap,lfClrTrap
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Example : DO ALPLIST
*:************************************************************
*B802029,1  HS  03/31/1999 Use the EDIACPRT and EDIPH files instead
*B802029,1                 of the file EDIACC because we have replaced
*B802029,1                 the file EDIACC with the files
*B802029,1                 EDIACPRT, EDIPH and EDIPD.
*B802516,1  WAM 08/05/1999 Create sequence# based on division
*B802541,1  AHM 08/19/1999 Fix the bug of wrong update open field in OrdHdr
*B802541,1                 file in case of multi store order
*B603121,1  AHM 09/22/1999 Fix the bug of wrong update open field in OrdLine
*B603121,1                 file in case of multi store order and pack qty.
*B603121,1                 exceeds ord. qty.
*B802726,1  AHM 10/31/1999 fix the bug of displaying order qty instead of
*B802726,1                 picked qty if the user select pick ticket
*B802796,1  WAB 11/18/1999 Fix The bug Of Not showning Neither the  packing list lines
*B802796,1                 nor cartons details Generate packing list from an pick ticket
*B802796,1                 and generate invoice from this pick Ticket
*B603379,1 WAM 12/29/1999  Increased the width of the line no field used in
*B603379,1                 the packing list lines to be 6 instead of 3 digits
*B603379,1                 not to have an error when creating big packing lists
*B802953,1 AHM 01/14/2000 Fix the bug of subscript out of bound.
*C101735,1 ARD 01/19/2000  Add the Ability to Print Carton Labels.
*B803183,1 ABD 02/05/2000 Run packing list program from SO
*B803183,1 ABD            For Customer Mul10 without AL modules.
*E301373,1 NAD 02/14/2000  Speed up the process of packing.Add a new bar in the options
*E301373,1                 pad to filter the order or pick ticket lines to some styles or colors
*B603496,1 ARD 03/05/2000  To print carton labels without EDI installed.
*B603501,1 ARD 03/08/2000  correction the values of asn_ship file
*B603515,1 ARD 03/13/2000  Fix Updating carton information
*B602797,1 ARD 04/04/2000  change the style and the stydye files in case of the user change
*B602797,1                 in the packed Qty. and need the change to effect in these files.
*B603479,1 ARD 04/18/2000  Suppres print packed cartons from SYCCONFG.
*B803213,1 WAM 04/19/2000  Take into consideration styles with exteded size scale while printning labels
*B803213,4 ARD 04/19/2000  FIX TOTQTY IN ASN_SHP FILE
*B803177,1 HBG 06/28/2000  1 - Add new setting in AL module setup: "Assign P. list to BOL" with
*B803177,1                     valid enteries:  A - Automatic.    I - Inquire.    N - Never.
*B803177,1                 2 - generate BOL acording to the setting of the BOL setup,and include
*B803177,1                     shipvia in the validation of it. (Done in AL.Prg)
*B803177,1                 3 - Validate the BOL with selection of Shipvia (Popup).
*B803177,1                 4 - Make changes in the information printed in Carton label
*B803177,1                       a - Shipvia should be of the Packing list not the order shipvia.
*B803261,1 HBG 06/27/2000        b - The manifacturing ID has to be lead by zero.
*B803261,1 HBG 08/09/2000  Fix bug of copying the packing slip from one to another don't copy the
*B803261,1                 cty_typ and Direct to.
*B603777,1 HBG 07/26/2000  If you run two sessions from manual packing list screen, and then
*B603777,1                 switch from one to another an error message prompt you
*B603777,1                 Variable lcTmAsnShp not found.
*B603807,1 HBG 08/10/2000  1 - Don't allow creating packing list on completed pick ticket
*B603807,1                 2 - If packing list is shipped , we shouldn.t allow deleting or
*B603807,1                     modifying it.
*B603807,1                 3 - Copying the Packing slip from one to another Should copy the
*B603807,1                 Cty_type and Direct to
*B603957,1 MAB 10/10/2000 Update ordhdr with Book and Open Quantities.
*B803777,1 HBG 11/14/2000 In detail folder , when sellect all the QTY/CTN
*B803777,1                field doesn't get updated correctly.
*B803778,1 HBG 11/16/2000 Fix bug of negative numbers in P.Qty
*B604186,1 HBG 02/07/2001 The program was transfered to root directory.
*B804003,1 HBG 11/02/2001 Check if the generated Pack_No found before or
*B804003,1                not , to prevent repeating the same No.
*C102281,1 AAN 11/04/2001 Add a parameter in "SO Allocation" module to specify the order of
*C102281,1                the lines in the packing list.
*C102239,1 ADEl 04/05/2001 Custom : Update ALPAKINF file for Bell.
*C102284,1 ABD 06/01/2001 Add the status of "K" for "Packed" to the Manual Packing List screen.
*C102275,1 HS  05/06/2001 Add the capability to print detailed UCC128 Label.
*E301646,1 HBG 07/25/2001 Add button "Select Style" to the screen according to the new
*E301646,1                Setup "Pack Per Size".
*C200218,1 BWA 30/08/2001 Modify the custom invoice for sademara.
*B804310,1 HBG 06/09/2001 Packing list with wrong cartons on the header when deleting a carton.
*B804384,1 HBG 13/09/2001 Modify the packing slip program to accept the 0 PLT.
*B604201,1 Hassan 10/04/2001 Total Ctn No Does not Changed W.R.T Ctn.s Edit
*C102478,1 ADEL 11/12/01 Generate P/K from P. List for Vital.
*B804458,1 ADEL 11/18/01 Fix the bug of not deleting whole style\color (whether it has
*B804458,1               one size or more).
*B804238,1 ASH 11/19/2001 Fix the bug of "\SCREENS\\ALOUTPRT.SPX "
*B605477,1 ASH 02/17/2002 Select Style button is not working.
*B605559,1 HBG 18/02/2002 Fix the following bugs:
*B605559,1                     - Deleting a packing slip completly does not update npck1 .... npck8 in Ordline.
*B605559,1                     - Removing a carton does not Renumber the remaining cartons.
*B605559,1                     - Removing Style/Color from a carton comes back after saving.
*B605592,1 HBG 24/02/2002 Fix bug :
*B605592,1                     - Creating 2 boxes while the packed carton is only one
*B605592,1                     - Apply button not working in case of select all then unselect one line
*B605583,1 HBG 24/02/2002 Fix bug :
*B605583,1                     - Alias is already in use
*B605583,1                     - Style filter option not working
*B605583,1                     - IF change range of cartons after selecting the line, Qty/Crtn doesn't change
*B604145,1 HBG 03/06/2002 Disable B.O.L field if generate B.O.L Automatic is 'Yes'
*B605661,1 HBG 03/06/2002 Fix bug of Can not enter weight for Single line orders
*B804306,1 HBG 04/03/2002 Save field of To Store or consalidatore in BOL_HDR.
*B606276,1 TMI 08/08/2002 Fix the bug that nPack qtys is saved in order line with negative quantities.
*B605412,1 TMI 09/22/2002 Check if current account is Edi Account
*C102789,1 TMI 02/02/2003 Add the option of having the creating the cartons
*C102789,1                type and add that type in the pack header and line
*B606977,1 ASH 02/20/2003 Fix bug of 'lcWareCode' not found in case of opening multi sessions.
*B606983,1 AMH 02/26/2003 Fix bug of wronge open quantity.
*B607157,1 HBG 04/13/2003 Fix bug of Problem in copying packing list from existing one.
*E302144,1 AMH 04/15/2003 Change the custom C102789 to be standard.
*B607210,1 KHM 05/21/2003 Fix the bug of showing the wrong palter # in the detail folder.
*B606895,1 ABD 06/18/2003 Fix bug of can't print he UCC 128 labels from
*B606895,1 ABD            Manual packing slip for Non edi account.
*B607290,1 KHM 07/28/2003 Fix the bug of record is used by another user.
*B607445,1 TMI 07/28/2003 Fix the bug that if order is invoiced that has a pack with no PikTkt
*B607445,1                then the detail browse show nothing
*B119166,1 ASH 09/02/2003 Don't renumber the cartons if the user delete a carton in the edit mode.
*E037191,1 SSE 10/12/2003 Add MUCB variable to printing Label function.
*E120116,1 TMI 10/14/2003 Allow to edit Carrier carton ID
*C122115,1 NFZ 05/18/2004 Get the cartons details from text file.
*B123229,1 NNA 08/01/2004 Fix bug that two different users can't make a packing list for
*B123229,1 NNA            the same order but with different Stores
*E302210,1 WLD 09/09/2004 Using visual label report
*C123616,1 TMI 10/13/2004 Add a new option to generate cartons for Nik Nak
*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch)
*C125077,1 AEH 06/02/2005 Add Carton Number to File
*B129128,1 EIH 09/01/2005 Cancel uncomp. session for england customer's .
*E039957,1 MHM 11/14/2005 Get the Carton # from the new file EDICRTSQ instead of calculating it
*B130950,1 EIH 30/01/2006 Do not allow user to pack more than existing inventory if costing method is FIFO or LIFO .
*E040123,1 HBG 03/14/2006 Get the UCC # structure from the setup
*B039660,1 NNA 04/16/2006 New trigger to prevent user from changing the Qty from packing list screen and force him to 
*B039660,1 NNA            Change it in the Pick tickets screen,all that in case using Bin Location
*B132318,1 NNA 06/18/2006 fix bug that Packing lists can be edited after invoicing
*E302357,1 TMI 02/01/2007 make some changes to allow to print visual labels with no bol# sent
*:*************************************************************************
*-- laData      array that hold the data of the key screens and header folder
*-- laScrMode   array that hold the screen mode
*-- laKeyField  array that hold the key of the screen
*-- laCodeInfo  array to be used in gfwCodePop function
*-- laShpVia    array that hold all shipvia codes
*-- lafoldwinds array that hold the count of used folder and it's titles
DIMENSION laData[1],laScrMode[4],laKeyField[3,4],laCodInfo[1,10],laShpVia[1],;
	lafoldwinds[3,2]
STORE SPACE(0) TO laData,laKeyField,laCodInfo,laShpVia
*-- lcCusPo    variable that hold customer purchase order No
*-- lcDept     variable that hold the depart of order
*-- lcShVDesc  variable that hold shipvia description
*-- lcPkChCode variable that hold Pack Char. Code
*-- lcPkDsCode variable that hold Pack Desc. Code
*-- lcStoreDsc variable that hold store description
*-- lcStyPic   variable that hold the picture of the style
*-- lcStyTtl   variable that hold the style title
*-- lcDistCtr  variable that hold the distripution center of the customer
*-- lcBol      variable that hold the bill of lading number
*-- lcPackNo   variable that hold the target pack no which used in case
*              coping information from another pack
*-- lcOrderNo  variable that hold the order number which used in case
*              coping information from another pack
*-- lcStore    variable that hold the store which used in case
*              coping information from another pack
*-- lcPckLin   variable that hold the temp name of dbf file which used for
*              holding styles/sizes information for order lines
*-- lcCtnHdr   variable that hold the temp name of dbf file which used for
*              holding carton information (No,Total Weight,Total Quantity)
*-- lcCtnDtl   variable that hold the temp name of dbf file which used for
*              holding the lines of each carton
*-- lcScaFile  variable that hold the temp name of dbf file which used for
*              holding the 8 lines for sizes
*-- lcCartonSz variable that hold the temp name of dbf file which used for
*              holding the 8 lines for sizes
*-- lcWinKey   variable that hold temp name to be used for defining the
*              key fields window
*-- lcWinHdr   variable that hold temp name to be used for defining the
*              haeder folder window
*-- lcWinDtl   variable that hold temp name to be used for defining the
*              detail folder window
*-- lcWinDtl1  variable that hold temp name to be used for defining the
*              the window which used to browse the order styles/sizes in
*              detail folder
*-- lcWinDtl2  variable that hold temp name to be used for defining the
*              the selection buttons window which in detail folder
*-- lcWinCtn   variable that hold temp name to be used for defining the
*              carton information folder window
*-- lcWinCtn1  variable that hold temp name to be used for defining the
*              the window which used to browse the carton header information
*              in carton information folder
*-- lcWinCtn2  variable that hold temp name to be used for defining the
*              the window which used to browse the carton lines information
*              in carton information folder
*-- lcWinCtn3  variable that hold temp name to be used for defining the
*              the edit region for carton header and lines windows in
*              carton information folder
*-- lcfolder   variable that hold the temp name of the folder window
*-- lcfoldprnt variable that hold the name of the parent window of the folder
*              window which is the base screen
*-- lcOldVal   variable that hold the original value befor update the value
*              of any get field used lfoldval finction in when snippet
*-- lcCtnSty   variable that hold style of carton lines edit rigion
*-- lcStySz    variable that hold size  of carton lines edit rigion
*-- lcLsVPck   variable that hold the current pack in view mode in order to
*--            avoid perfom lfvSelOrdl(),lfCtnInfo() functions if the pack
*--            has not been changed(the user has not used navigation by tool bar)
*-- lcPrvPack  Variable that hold the last pack no while navigating

*-- c101735,1
*-- lnNonMajSt
*-- lnMajLen
*-- lcFree_Clr
*-- lnColorLen
lclcl = ''

*B130950,1 EIH 30/01/2006 Do not allow user to pack more than existing inventory if costing method is FIFO or LIFO [Begin].
lcCostMeth = gfGetMemVar('M_COST_MET') 
*here we must open the stydye file to get the stock
IF lcCostMeth $ 'FL' 
  =gfOpenFile(gcDataDir+"StyDye",gcDataDir+'StyDye','SH')
ENDIF
*B130950,1 EIH 30/01/2006 [End]

*E040123,1 HBG 03/14/2006 Check for UCC # structure setup [Begin]
llIncPLNum = .F.
lnNumOfDig = 5
*E040123,1 HBG 03/14/2006 [End]

*B803183,1 ABD Cheak if AL OR AS Moduls is Install. [Begin]
llModInst = .F.
llModInst = ('AL' $ gcCmpModules  .OR. 'AS' $ gcCmpModules)
IF !llModInst
	*B803183,1 Message No.    : 42083
	*B803183,1 Message Text   : XX module is not installed. Cannot proceed.
	*B803183,1 Button No.     :  00000
	*B803183,1 Message Button : OK
	=gfModalGen('TRM42083B00000','DIALOG','Advanced Shipment and Sales Order Allocation')
	CLEAR READ
	RETURN
ENDIF
*B803183,1 ABD [End]


*E301373,1 NAD 02/14/2000 (Start) Defining Variables
DIMENSION laStySrc[1], laStyTrgt[1],laOldSty[1]
DIMENSION laClrSrc[1], laClrTrgt[1],laOldClr[1]
STORE SPACE (0) TO laStySrc,laStyTrgt,laOldSty,laClrSrc, laClrTrgt,laOldClr
lcMask=gfItemMask("PM")
*E301373,1 NAD 02/14/2000 (End)

*B603501,1
llntfound = .F.
*B603501,1

*Defining new variables [START]
*STORE SPACE(0) TO lcCusPo,lcDept,lcShVDesc,lcPkChCode,lcPkDsCode,lcStoreDsc,;
lcStyPic,lcStyTtl,lcDistCtr,lcBol,lcPackNo,lcOrderNo,;
lcStore,lcPckLin,lcCtnHdr,lcCtnDtl,lcWinKey,;
lcWinHdr,lcWinDtl,lcWinDtl1,lcWinDtl2,;
lcWinCtn,lcWinCtn1,lcWinCtn2,lcWinCtn3,lcfolder,lcfoldprnt,;
lcOldVal,lcCtnSty,lcStySz,lcLsVPck,lcScaFile,lcCartonSz,;
lcPrvPack
*B803177,1 HBG Add new variable to hold the title of the screen [Begin]
*STORE SPACE(0) TO lcCusPo,lcDept,lcShVDesc,lcPkChCode,lcPkDsCode,lcStoreDsc,;
lcStyPic,lcStyTtl,lcDistCtr,lcBol,lcPackNo,lcOrderNo,;
lcStore,lcPckLin,lcCtnHdr,lcCtnDtl,lcWinKey,;
lcWinHdr,lcWinDtl,lcWinDtl1,lcWinDtl2,;
lcWinCtn,lcWinCtn1,lcWinCtn2,lcWinCtn3,lcfolder,lcfoldprnt,;
lcOldVal,lcCtnSty,lcStySz,lcLsVPck,lcScaFile,lcCartonSz,;
lcPrvPack, lcFree_Clr , lcTmpDetFl

*B603777,1 HBG 07/26/2000 declare the variable "lcTmAsnShp " before gfSetup() to be saved [Begin]
*STORE SPACE(0) TO lcCusPo,lcDept,lcShVDesc,lcPkChCode,lcPkDsCode,lcStoreDsc,;
lcStyPic,lcStyTtl,lcDistCtr,lcBol,lcPackNo,lcOrderNo,;
lcStore,lcPckLin,lcCtnHdr,lcCtnDtl,lcWinKey,;
lcWinHdr,lcWinDtl,lcWinDtl1,lcWinDtl2,;
lcWinCtn,lcWinCtn1,lcWinCtn2,lcWinCtn3,lcfolder,lcfoldprnt,;
lcOldVal,lcCtnSty,lcStySz,lcLsVPck,lcScaFile,lcCartonSz,;
lcPrvPack, lcFree_Clr , lcTmpDetFl , lcBollsttl
*B606977,1 ASH 02/20/2003 (Begin) Declare the variable "lcWareCode" before gfSetup() to be saved.
STORE SPACE(0) TO lcCusPo,lcDept,lcShVDesc,lcPkChCode,lcPkDsCode,lcStoreDsc,;
	lcStyPic,lcStyTtl,lcDistCtr,lcBol,lcPackNo,lcOrderNo,;
	lcStore,lcPckLin,lcCtnHdr,lcCtnDtl,lcWinKey,;
	lcWinHdr,lcWinDtl,lcWinDtl1,lcWinDtl2,;
	lcWinCtn,lcWinCtn1,lcWinCtn2,lcWinCtn3,lcfolder,lcfoldprnt,;
	lcOldVal,lcCtnSty,lcStySz,lcLsVPck,lcScaFile,lcCartonSz,;
	lcPrvPack, lcFree_Clr , lcTmpDetFl , lcBollsttl ,lcTmAsnShp ,lcTmpPck ,lcTmpIdx, lcWareCode
*B603777,1 [End]

*E302210,1 WLD 09/09/2004 Using visual label report [Begin]
STORE SPACE(0) TO lcPrnAsnShp
*E302210,1 WLD 09/09/2004 Using visual label report [End  ]

*B606977,1 ASH 02/20/2003 (End)
*B803177,1 HBG Add new variable to hold the title of the screen End ]

STORE 0  TO lnDumDtlR, lnDumCtnR, lnNonMajSt, lnMajLen , lnColorLen
*Defining new variables  [END  ]




*-- lcSel      variable that hold label of select buuton in second folder
*              the label to be (select OR unselect)
lcSel = "Select"
*-- lcDtl2Stat variable that hold the enabling status of lcWinDtl2 window
*-- lcStoStat  variable that hold the enabling status of store field (laData[5])
*-- lcCtnStat  variable that hold the enabling status of carton type field (lnCtnTyp)
*-- lcPikStat  variable that hold the enabling status of piktkt field (laData[2])
*-- lcShipStat variable that hold the enabling status of shipvia field (lnShpVia)
*-- lcInstStat variable that hold the enabling status of special inst. fields (laData[11],laData[12])
*-- lcCartNoSt variable that hold the enabling status of cart_no field in edit rigion
*              in window lcWinCtn3 (lnCartNo)
*-- lcTotWghSt variable that hold the enabling status of tot_wgh field in edit rigion
*              in window lcWinCtn3 (lnTotWgh)
*-- lcPalNoSt  variable that hold the enabling status of Pal_No field in edit rigion
*              in window lcWinCtn3 (lnPalNo)
*-- lcCtnPalSt variable that hold the enabling status of Pal_No field in edit rigion
*              in window lcWinDtl2(lnCtnPal)
*-- lcCtnStySt variable that hold the enabling status of style field in edit rigion
*              in window lcWinCtn3 (lcCtnSty)
*-- lcStySzSt  variable that hold the enabling status of size field in edit rigion
*              in window lcWinCtn3 (lcStySz)
*-- lcStyQtySt variable that hold the enabling status of quantity field in edit rigion
*              in window lcWinCtn3 (lnStyQty)
*-- lcStyWghSt variable that hold the enabling status of weight field in edit rigion
*              in window lcWinCtn3 (lnStyWgh)
*-- lcAddHdrSt variable that hold the enabling status of header new button in edit rigion
*              in window lcWinCtn3
*-- lcRemHdrSt variable that hold the enabling status of header rem button in edit rigion
*              in window lcWinCtn3
*-- lcAddDtlSt variable that hold the enabling status of detail new button in edit rigion
*              in window lcWinCtn3
*-- lcRemDtlSt variable that hold the enabling status of detail rem  button in edit rigion
*              in window lcWinCtn3
STORE "DISABLE" TO lcDtl2Stat,lcStoStat,lcCtnStat,lcPikStat,lcShipStat,;
	lcInstStat,lcCartNoSt,lcTotWghSt,lcPalNoSt,lcCtnPalSt,lcCtnStySt,;
	lcStySzSt,lcStyQtySt,lcStyWghSt,lcAddHdrSt,lcRemHdrSt,;
	lcAddDtlSt,lcRemDtlSt
*E120116,1 [START] Define variables
STORE SPACE(25) TO lcCarrCtnID   && to store Carrier Carton ID
STORE "DISABLE" TO lcCrCtnIDSt   && and a screen field Status variable
*E120116,1 [END  ]

*-- lcPackStat variable that hold the enabling status of pack no field (laData[1])
*-- lcOrdStat  variable that hold the enabling status of order no  field (laData[2])
*-- lcAccStat  variable that hold the enabling status of account field (laData[4])
STORE "ENABLE" TO lcPackStat,lcOrdStat,lcAccStat

*-- lnLastFold  variable that hold the last activated folder befor changing
*               the folder
*-- lnShpVia    variable that hold the shipvia
*-- lnRelCho    variable that showes if release the remain order lines is desired (1=release)
*-- lnTotWgh    variable that hold total weight of the carton
*-- lnPalNo     variable that hold palette No of the carton
*-- lnCtnPal    variable that hold palette No of the carton
*-- lnStyQty    variable that hold style/size quantity in carton lines temp files
*-- lnStyWgh    variable that hold style/size weight in carton lines temp file
*-- lnCartNo    variable that hold carton no of the edit rigion in lcWinCtn3 window
*-- lnHdBrRec   variable that hold current record no in carton header temp file
*-- lnDtBrRec   variable that hold current record no in temp file lcPckLin
*-- lnCtHdBrRe  variable that hold current record no in temp file lcCtnHdr
*-- lnCtDtBrRe  variable that hold current record no in temp file lcCtnDtl
*-- lnMaxPal    variable that hold max palette No
*-- lnMinPal    variable that hold min palette No
*-- lnLastNo    variable that hold last lineno for pack line records
*-- lnMaxCtn    variable that hold max carton No
*-- lnLastCtn   variable that hold last carton # that has styles (not empty carton)
*-- lnPackWgh   variable that hold last pack header weight
*-- lnPackQty   variable that hold last pack header quantity
*-- lnPackCtn   variable that hold last pack header cartons count
*-- lnPrvMode   variable that hold the the screen mode when lfvSelOrdL was executed
*--             only if the mode has been changed
*-- lnOrdMdPr   variable that hold the period that order is allowed to be
*--             modified through it
STORE 0 TO lnLastFold,lnShpVia,lnStyleWid,lnRelCho,lnTotWgh,lnPalNo,lnCtnPal,lnStyQty,;
	lnStyWgh,lnCartNo,lnHdBrRec,lnDtBrRec,lnCtHdBrRe,lnCtDtBrRe,lnFrom,;
	lnTo,lnMaxPal,lnMinPal,lnLastNo,lnMaxCtn,lnLastCtn,;
	lnPackWgh,lnPackQty,lnPackCtn,lnPrvMode,lnBrCtnQty,lnBrUntWgh,lnOrdMdPr
*-- lnActFolder variable that hold the number of activated folder
*-- lnCtnTyp    variable that showes carton type (standard or pikpcak)
*-- lnDrctTo    variable that showes if the paking is directed to store or consolidator
*E301373,1 NAD (Start) Change the Default Values to a Pick & Pack carton and Ship to Store
*STORE 1 TO lnCtnTyp,lnDrctTo,lnActFolder
STORE 2 TO lnCtnTyp
STORE 1 TO lnDrctTo,lnActFolder
*E301373,1 NAD 02/14/2000 (End)

*-- llBrowse   variable that used to show if any browse button is clicked
*-- llEdiSys   variable that showes if there is EDI module or not
*-- llAsnSys   variable that showes if there is ASN module or not
*-- llAloModul variable that showes if there is ALO module or not
*-- llEdiAcc   variable that shoes if the account is EID account
*-- llPCDStat  variable that is used to make Pack Char. Code & Pack Desc. Code
*              visible or not
*-- llPalStat  variable that is used to make palette No. visible or not
*-- llClrSel   variable that showes if it is desired to clear selected lines
*              in lcPckLin file after apply
*-- llShwOpn   variable that indicates to show all or only open quantities
*-- llEdiopn   variable that showes if this program opened EdiAcc File or it
*              is opened by another program
*-- llB_H_Opn  variable that showes if this program opened BOL_Hdr File or it
*              is opened by another program
*-- llB_D_Opn  variable that showes if this program opened BOL_Lin File or it
*              is opened by another program
*-- llDyelot   variable that showes if system uses Dyelot or not
*-- llMulWare  variable that showes if system uses multi warehouse or not
*-- llNew      variable that showes if we got in new function
*-- llPckCopy  variable that showes if the user want to copy from another pack
*-- llNoShow   Variable that prevent or enable the program go into    ;
lpshow once the PROGRAM is executed;
(.F. GO IN lpshow,.T. don't)
*-- llSelOrdLi variable that showes if the order lines has been selected to
*              use it in lfactfolder function in case view mode
*-- llUnComp   variable That showes if the sysytem didect uncomp. session or not
*-- llCupdate  varibale that showes if any editing has been happened
*-- llAnyUpd   varibale that showes if any editing has been happened,but in local
*--            scope to limit executing lfvSelOrdl() for the same pack only
*--            if any modification has happened
*-- llAnyRec   variable that showes if temp file (lcPckLin) hold any record or it
*--            is empty
*-- llCanPrnLb   Hold The State Of The User Rights If He can Print Or Not.
*-- llScrPrnLb   Hold The State of the option menue print label after each carton
*-- lcSndPort    Hold the name of the printing port.


*-- C101735,1  New variables
llScrPrnLb = .F.
lcSndPort = "COM2"
*-- C101735,1  New variables

*E301646,1 HBG 07/25/2001 Add flag (llPakPrSiz) to hold the value of the setup (M_PAKPRSIZ) [Begin]
*STORE .F. TO llBrowse,llEdiSys,llAsnSys,llAloModul,llEdiAcc,llPCDStat,llPalStat,llClrSel,;
llShwOpn,llEdiopn,llB_H_Opn,llB_D_Opn,llDyelot,llMulWare,llNew,llPckCopy,llNoShow,;
llSelOrdLi,llUnComp,llCupdate,llAnyUpd,llAnyRec, llPrint
STORE .F. TO llBrowse,llEdiSys,llAsnSys,llAloModul,llEdiAcc,llPCDStat,llPalStat,llClrSel,;
	llShwOpn,llEdiopn,llB_H_Opn,llB_D_Opn,llDyelot,llMulWare,llNew,llPckCopy,llNoShow,;
	llSelOrdLi,llUnComp,llCupdate,llAnyUpd,llAnyRec, llPrint,llPakPrSiz
*E301646,1 [End]

STORE .F. TO llQtyArr,llWghArr

DIMENSION laMajSegs[1,1]


llFirstQty = .F.
llFirstWgh = .F.

llAlowNew  = .T.             && Flag to allow adding new Pack.

laDefproc[07] = .F.                && This is to Enable local delete
laDefproc[09] = .F.                && This is to prevent the global save
laDefProc[10] = .F.                && This is to prevent the global close

lcDtlBrTtl = 'Open Quantities'     && This is to hold the title of the browse screen
lcCtHdBrTt = 'Cartons'             && This is to hold the title of the browse screen
lcCtDtBrTt = 'Contents of carton'  && This is to hold the title of the browse screen

lnSessNo  = gnProgCopy             && This is to hold the no of the opened session from global variable
lcSession = SPACE(1)               && This is to hold the session id for the uncompleted session
lcUserID  = PADR(gcUser_id, 10)    && This is to hold the user id

*--E301077 (Start)
*IF llAsnSys
*  llB_H_Opn = gfOpenFile(gcDataDir+'Bol_Hdr',gcDataDir+'Bol_Hdr','SH')
*  llB_D_Opn = gfOpenFile(gcDataDir+'Bol_Lin',gcDataDir+'Bol_Lin','SH')
*ENDIF
*--E301077 (End)

laKeyField[1,1] = "laData[1]"
laKeyField[1,2] =.F.
laKeyField[1,3] = "Pack_Hdr"
laKeyField[1,4] = 1

laKeyField[2,1] = "laData[2]"
laKeyField[2,2] = .F.
laKeyField[2,3] = "Pack_Hdr"
laKeyField[2,4] = 2

laKeyField[3,1] = "laData[3]"
laKeyField[3,2] = .T.
laKeyField[3,3] = "Pack_Hdr"
laKeyField[3,4] = 3

*--This part is to be used in case detecting an uncomplete program session.

lcScrMode  = ""                 && This is to hold the screen mode to be used in the uncomplete session
lnUnCmSeRc = 0                  && This is to hold the uncomplete session record no in the "uncmsess" file
llCupdate  = .F.                && This is .T. if the updating for any field is happened

*-- Variables that are needed to be saved and restored when detecting an
*-- uncomplete program session.

DIMENSION laVars[24]
laVars[01] = "lcScrMode"
laVars[02] = "lnUnCmSeRc"
laVars[03] = "lnActFolder"
laVars[04] = "llCupdate"
laVars[05] = "lnCtnTyp"
laVars[06] = "lnDrctTo"
laVars[07] = "lnShpVia"
laVars[08] = "llPCDStat"
laVars[09] = "llPalStat"
laVars[10] = "lnRelCho"
laVars[11] = "llNew"
laVars[12] = "lnMaxPal"
laVars[13] = "lnMinPal"
laVars[14] = "lnLastNo"
laVars[15] = "lnMaxCtn"
laVars[16] = "lnLastCtn"
laVars[17] = "lnFrom"
laVars[18] = "lnTo"
laVars[19] = "lnPackWgh"
laVars[20] = "lnPackQty"
laVars[21] = "lnPackCtn"
laVars[22] = "lcLsVPck"
laVars[23] = "llAnyUpd"
laVars[24] = "llAnyRec"

*B602797,1 creat a memory file to hold the state of the menu option llUpdtPkTk [Begin]
llUpdtPkTk = .F.
IF FILE (gcDataDir+'UpdtPkTk.MEM')
	RESTORE FROM (gcDataDir+'UpdtPkTk.MEM') ADDITIVE
ENDIF
*B602797,1 creat a memory file to hold the state of the menu option llUpdtPkTk [End  ]

*-- This is to add laData elements to laVars array---------------
FOR lnK = 1 TO 16
	DIMENSION laVars[ALEN(laVars)+1]
	laVars[ALEN(laVars)] = "laData[" + ALLTRIM(STR(lnK,2)) + "]"
ENDFOR

IF !gfSetup()
	RETURN
ENDIF

*-- This is to check if the system has EDI module , and open EdiAcc File if is true

* B603496,1 change EDI to AS
*llEdiSys   = ('EB' $ gcCmpModules)
*B803177,1 HBG 06/28/2000 Check for  AS
llEdiSys = ('AS' $ gcCmpModules)
IF llEdiSys
	*B802029,1 Change this line to open the EDIACPRT and EDIPH files instead of
	*          the file EDIACC because we have replaced the file EDIACC with
	*          the files EDIACPRT, EDIPH and EDIPD [Begin]
	*llEdiopn = gfOpenFile(gcDataDir+'EdiAcc',gcDataDir+'EdiAcc','SH')
	=gfOpenFile(gcDataDir + 'EDIACPRT' , gcDataDir + 'ACCFACT' , 'SH')
	=gfOpenFile(gcDataDir + 'EDIPH' , gcDataDir + 'PARTNER' , 'SH')
	*B802029,1 Change this line to open the EDIACPRT file instead of [End]

	*E302210,1 WLD 09/09/2004 Using visual label report
	=gfOpenFile(gcDataDir+'Bol_Hdr',gcDataDir+'Bol_Hdr','SH')
	=gfOpenFile(gcDataDir+'Bol_Lin',gcDataDir+'Bol_Lin','SH')
	*E302210,1 WLD (End)
    *E039957,1  MHM 11/14/2005 Open the new file EDICRTSQ [Begin]
    =gfOpenFile(gcDataDir + 'EDICRTSQ' , gcDataDir + 'EDICRTSQ' , 'SH')
    *E039957,1  MHM 11/14/2005 [End]  
	
ENDIF

*B603515,1 Open Warehouse file
=gfOpenFile(gcDataDir+'WareHous','WareHous','SH')
*B603515,1 (End)

*B603501,1 Open Spck_Lin to get SKUs to be printed on the labels
=gfOpenFile(gcDataDir + 'SPCK_LIN' , gcDataDir + 'Spcklins' , 'SH')
=gfOpenFile(gcDataDir + 'CUSTDEPT' , gcDataDir + 'CUSTDEPT' , 'SH')

DECLARE laDRltFld[1,2]
laDRltFld[1,1] = 'DIVLNAME  '
laDRltFld[1,2] = 'lcDivLName'
lcDivLName = ''
*B603501,1 (End)

*B603515,1 Get Carrier SCAC Code
DECLARE laVRltFld[1,2]
laVRltFld[1,1] = 'CARRIERCOD'
laVRltFld[1,2] = 'lcCarCod'
lcCarCod = ''
*B603515,1 (End)

*-- C101735,1 redimension the array [Begin]

*DIMENSION laSetup[2,2]
*BADRAN 04/24/2000
* Check if user can force allocation
*DIMENSION laSetup[4,2]
*B803177,1 HBG 07/11/2000 Add row to the array laSetups for the setup of BOL [Begin]
*DIMENSION laSetup[5,2]
*E301646,1 HBG 07/25/2001 Add row to the array laSetups for the setup of Pack Per Size [Begin]
*DIMENSION laSetup[6,2]
*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
*DIMENSION laSetup[7,2]
DIMENSION laSetup[8,2]
*E040123,1 HBG 03/13/2006 [End]
*E301646,1 [End]

*B803177,1 HBG 07/11/2000 Add row to the array laSetups for the setup of BOL [End  ]
*BADRAN 04/24/2000

*-- C101735,1 redimension the array [End  ]

laSetUp[1,1] = 'M_DYELOT'
laSetUp[2,1] = 'M_CANAFTER'

*-- C101735,1 Get Status For The M_PRNLABEL [Begin]
laSetUp[3,1] = 'M_WareHouse'
laSetUp[4,1] = 'XMANUFID'
*-- C101735,1 Get Status For The M_PRNLABEL [End  ]

*BADRAN 04/24/2000
* Check if user can force allocation
laSetUp[5,1] = 'M_FORCEALO'
*BADRAN 04/24/2000
*B803177,1 HBG 07/11/2000 Add Setup (M_BOLSTUTS) to the array laSetups [Begin]
laSetUp[6,1] = 'M_BOLSTUTS'
*B803177,1 HBG 07/11/2000 Add Setup (M_BOLSTUTS) to the array laSetups [Begin]

*E301646,1 HBG 07/25/2001 Add Setup (M_PAKPRSIZ) to the array laSetups [Begin]
laSetUp[7,1] = 'M_PAKPRSIZ'
*E301646,1  [End]

*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
laSetup[8,1] = 'M_UCCBSDON'
*E040123,1 HBG 03/13/2006 [End]

=gfGetMemVar(@laSetUp,gcAct_Comp)

*BADRAN 04/24/2000
* Check if user can force allocation
llAlwForce = lfSuppForc()
*

llDyelot   = ALLTRIM(laSetUp[1,2]) = 'Y'
lnOrdMdPr  = laSetUp[2,2]

*-- C101735,1 Get Status For The M_PRNLABEL And llCanPrnLb [Begin]
llMulWare  = ALLTRIM(laSetUp[3,2]) = 'Y'
lcManufId  = ALLTRIM(laSetUp[4,2])
llCanPrnLb = gfUserPriv('AL','ALPLIST','PRNPACKING')
*-- C101735,1 Get Status For The M_PRNLABEL And llCanPrnLb [End  ]

*B803177,1 HBG 07/11/2000 Add flag to hold the valu of the setup (M_BOLSTUTS) [Begin]
llBolnever = (ALLTRIM(laSetup[6,2]) = "N")
*B803177,1 HBG 07/11/2000 Add flag to hold the valu of the setup (M_BOLSTUTS) [End  ]
*B604145,1 HBG 03/06/2002 Get the setting of generate B.O.L Automatic [begin]
llBolAutom = (ALLTRIM(laSetup[6,2]) = "A")
*B604145,1 [End]

*E301646,1 HBG 07/25/2001 Add flag to hold the value of the setup (M_PAKPRSIZ) [Begin]
llPakPrSiz = laSetup[7,2]
*E301646,1 [End  ]

*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
llDispMsg  = .F.
=gfOpenFile(gcDataDir+"SETUPS",gcDataDir+'MODVAR','SH')
=gfOpenFile(gcSysHome+"SYCCONFG",gcSysHome+'MODVAR','SH')
IF SEEK('ALM_UCCBSDON','SYCCONFG') AND EVAL(ALLTRIM(SYCCONFG.mData_def)) = 0
  PUSH KEY
  lcTitle  = 'Select UCC # Structure'
  DO (gcScrDir + '\ALUCCSTR.SPR')
  POP KEY
  SELECT SYCCONFG
  REPLACE mVentries WITH '5 Digits of PL # + 4 Digits for Carton #|6 Digits of PL # + 3 Digits for Carton #|9 Digits for Carton #~5|6|9',;
          mData_def WITH '5'
  IF SEEK('ALM_UCCBSDON','SETUPS') 
    SELECT SETUPS
    REPLACE mVentries WITH SYCCONFG.mVentries ,;
            mData_def WITH ALLTRIM(STR(lnNumOfDig))
  ELSE
    SELECT SYCCONFG           
    SCATTER MEMVAR MEMO
    SELECT SETUPS
    APPEND BLANK
    m.mData_def = ALLTRIM(STR(lnNumOfDig))
    GATHER MEMVAR MEMO
  ENDIF
ELSE
  lnNumOfDig = EVAL(laSetup[8,2])
ENDIF
llIncPLNum = (lnNumOfDig = 5 OR lnNumOfDig = 6)
*E040123,1 HBG 03/14/2006 [End]

llAsnSys   = ('AS' $ gcCmpModules)
llAloModul = ('AL' $ gcCmpModules)

lcProgID = PADR("PACKLIST",10)

lnFolderCEnd = 102.40
lnFolderREnd = 1.99
lnNoFld      = 3
lcwfoldchng  = '=lfActFolder()'  && function to control shows after change the folder

*B803213,1 Take into consideration styles with exteded size scale while printning labels
llExtSizSc = gfGetMemVar('M_USEEXSSC',gcAct_Comp)
lcSizeSep  = ''
STORE 0 TO lnSizePos,lnSizeLen
IF llExtSizSc
	DECLARE laStySeg[1,1]
	STORE "" TO laStySeg
	=gfItemMask(@laStySeg)
	FOR lnCnt = 1 TO ALEN(laStySeg,1)
		IF laStySeg[lnCnt , 1] = "S"
			lcSizeSep  = ALLTRIM(laStySeg[lnCnt-1 , 6])
			lnSizePos  = laStySeg[lnCnt , 4] - IIF(!EMPTY(lcSizeSep) , 1 , 0)
			lnSizeLen  = LEN(laStySeg[lnCnt , 3]) + IIF(!EMPTY(lcSizeSep) , 1 , 0)
		ENDIF
	ENDFOR
ENDIF
*B803213,1 (End)

IF !WEXIST(gcBaseWind)
	lcWinKey   = gfTempName()
	lcWinHdr   = gfTempName()
	lcWinDtl   = gfTempName()
	lcWinDtl1  = gfTempName()
	lcWinDtl2  = gfTempName()
	lcWinCtn   = gfTempName()
	lcWinCtn1  = gfTempName()
	lcWinCtn2  = gfTempName()
	lcWinCtn3  = gfTempName()

	*C101735,1 Defining temp. tables  [Begin]
	lcAsnLabel = gfTempName()
	lcTmAsnShp = gfTempName()
	lcTmpDetFl = gftempName()
	*C101735,1 Defining temp. tables  [End  ]

	*E302210,1 WLD 09/09/2004 Using visual label report [Begin]
	lcPrnAsnShp = gfTempName()
	*E302210,1 WLD 09/09/2004 Using visual label report [End  ]

	lcStyPic   = gfItemMask("PI")
	lcStyTtl   = gfItemMask("HI")
	lnStyleWid = LEN(lcStyPic)

	lcScFields = "Pack_No, Order, Piktkt, Account, Store, Note, ShipVia,;
		Tot_Wght, Tot_Cart, Tot_Pcs, Sp_Inst1, Sp_Inst2, LStandCtn,;
		CToStorCn, CPkChCode, CPkDsCode "

	*E302144,1 AMH Change the custom C102789 to be standard [Start]
	*C102789,1 TMI [Start] Add new field "CCRTNVLTYP" for KrazyCat to the field list in the variable "lcScFields"
	*IF ASCAN(laEvntTrig , PADR('ADDTPFLD',10)) <> 0
	*  =gfDoTriger('ALPLIST',PADR('ADDTPFLD',10))
	*ENDIF
	*C102789,1 TMI [End  ]
	lcScFields = lcScFields + ',CCRTNVLTYP'
	*E302144,1 AMH [End]
	*E120116,1 TMI [START] add the field LCARRCTNID to the screen header fields
	lcScFields = lcScFields + ',LCARRCTNID'
	*E120116,1 TMI [END  ]

	SELECT Pack_Hdr
	SCATTER FIELDS &lcScFields TO laData BLANK
	lcWareCode = ''
	lcBol      = ''
	lcOldValue = ""

	lcPckLin   = gfTempName()
	lcCtnHdr   = gfTempName()
	lcCtnDtl   = gfTempName()
	lcScaFile  = gfTempName()
	lcCartonSz = gfTempName()

	*-- C102281 AAN Add variable to hold new alias for temp. (lcPckLin) [Begin].
	lcTmpPck  = gfTempName()
	lcTmpIdx  = gfTempName()
	*-- C102281 AAN Add variable to hold new alias for temp. (lcPckLin) [End].

	CREATE TABLE (gcWorkDir + lcScaFile) (SzCode C(1))
	INDEX ON SzCode TAG (lcScaFile)
	FOR lnI = 1 TO 8
		INSERT INTO (lcScaFile) (SzCode) VALUES (STR(lnI,1))
	ENDFOR
	USE (gcWorkDir+lcScaFile) IN 0 AGAIN ALIAS (lcCartonSz) ORDER (lcScaFile)

	lafoldwinds[1,1] = 'Header'
	lafoldwinds[1,2] = lcWinHdr
	lafoldwinds[2,1] = 'Details'
	lafoldwinds[2,2] = lcWinDtl
	lafoldwinds[3,1] = 'Carton Information'
	lafoldwinds[3,2] = lcWinCtn
	lcfolder    = gfTempName()        && Folder Window Name
	lcfoldprnt  = gcBaseWind          && window parent name for the folder
	STORE 1 TO lnActFolder,lnLastFold && active folder

	*-- Get a session number, to be updated in the uncomplete session
	*-- number file for each session.
	lcSession  = gfsequence('CSESSION')

	*c101735,1 calling function that caluculat style parametars [Begin]
	=lfEvalSegs()
	*c101735,1 calling function that caluculat style parametars [End  ]

ENDIF

*-- C101735,1 [Begin] Open fiels for data collecting if EDI and company
*--                   installed to print packed cartons.

*-- B603496,1 check for llcanprnlb only [Begin]
IF llCanPrnLb
	*IF llEdiSys AND llCanPrnLb
	*-- B603496,1 check for llcanprnlb only [End  ]

	=gfOpenFile(gcDataDir+'Asn_Ship','Asn_Ship','SH')
	*B603777,1 HBG 07/26/2000 IF THE TABLE IS NOT OPENED BEFORE OPEN IT ,THIS IS ONLY IF
	*B603777,1                THIS FIRST TIME RUN THE SCREEN [BEGIN]
	IF !USED(lcTmAsnShp)
		COPY STRUCTURE TO (gcWorkDir+lcTmAsnShp)
		=gfOpenFile(gcWorkDir+lcTmAsnShp,'','EX')
		INDEX ON STR(CART_NO,6) TAG (lcTmAsnShp)
	ENDIF
	*B603777,1 [END]
	=gfOpenFile(gcSysHome+'SYCASNLB','ASNlbl','SH')

	*E302210,1 WLD 09/09/2004 Using visual label report [Begin]
	=gfOpenFile(gcSysHome+'SYCASNHD','VerPrt','SH')
	=gfOpenFile(gcSysHome+'SYCASNDT','CVer','SH')
	*E302210,1 WLD 09/09/2004 Using visual label report [End  ]

	*C102275,1 HS 05/06/2001 Add some new variables and open the Style UPC file if the
	*             UP Module is installed [Begin]
	*-- Flag to know if the detailed label exist or not


	*E302210,1 WLD 09/09/2004 Using visual label report [Begin]
	*llDetLabel = SEEK("XX1" + "H" , "SYCASNLB") .AND. SEEK("XX1" + "L" , "SYCASNLB")
	SELECT SYCASNHD
	LOCATE FOR SYCASNHD.lDetLabel = .T.
	IF FOUND()
		llDetLabel = .T.
	ENDIF
	*E302210,1 WLD 09/09/2004 Using visual label report [End  ]

	*IF llDetLabel
	*-- Flag to know if UPC module is installed or not
	llUPCInst  = ('UP' $ gcCmpModules)
	*-- Flag to know if the user want to print detailed shipping label for all cartons or not
	lcDetLbAll = ""

	IF llUPCInst
		=gfOpenFile(gcDataDir + 'STYLEUPC' , gcDataDir + 'STYLEUPC' , 'SH')
	ENDIF
	*ENDIF
	*C102275,1 HS 05/06/2001 Add some new variables and open... [End]

ENDIF
*-- C101735,1 [End  ] Open fiels for data collecting if EDI and company

laCodInfo[1,01] = "SHIPVIA"                 && Field Name
laCodInfo[1,02] = "laShpVia"                && Array Name
laCodInfo[1,03] = "lnShpVia"                && Popup Name
laCodInfo[1,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
laCodInfo[1,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[1,06] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[1,07] = "Pack_Hdr"                && Alternative File (For default val.)
laCodInfo[1,08] = "Pack_Hdr"                && Use this index for the Alternative file.
laCodInfo[1,09] = "laData[1]"               && Seek this expretion.
laCodInfo[1,10] = "ShipVia"                 && Alternative Field Name
= gfwCodePop(@laCodInfo, "SHIPVIA", "N")

*-- Before running the screen, please do the folowing...
*-- 1. Save the current system menu.
*-- 2. Switch the "Delete" bar to be "Cancel/Uncancel"
*-- 3. Create the "Option" menu pad.
*-- 4. Set the needed filters, indexs on the used files.
*-- Run the screen and then restore all the previously stored values.
= lfActPad()

SELECT Pack_Lin
SET ORDER TO PackStyle

SELECT OrdHdr
SET FILTER TO cOrdType = "O"
SET ORDER TO OrdHdr

SELECT PikTkt
SET ORDER TO TAG 'OrdPik' IN PikTkt
SELECT Pack_Hdr
SET ORDER TO Pack_Hdr
SET RELATION TO 'O'+ORDER INTO OrdHdr

SET ORDER TO TAG STYLE IN STYLE

SELECT OrdLine
SET FILTER TO cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = "O"
SET ORDER TO OrdLinSt
IF EMPTY(SET("RELATION"))
	SET RELATION TO 'S'+Scale INTO Scale ADDITIVE
	SET RELATION TO STYLE INTO STYLE ADDITIVE
ENDIF

SELECT Pack_Lin
IF EMPTY(SET("RELATION"))
	SET RELATION TO 'O'+laData[2]+laData[5]+STYLE INTO OrdLine ADDITIVE
ENDIF

*B802796,1 WAB - Open Pikline File and amke a relation with orderlines
*B802796,1 WAB - START
*ashraf
=gfOpenFile(gcDataDir+"Pikline",gcDataDir+'Pikline','SH')
SELECT PikLine
IF EMPTY(SET("RELATION"))
	SET RELATION TO 'O'+Pikline.order+Pikline.Store+Pikline.Style+STR(Pikline.lineno,6) INTO ORDLINE ADDITIVE
ENDIF
SELECT PikTkt
IF EMPTY(SET("RELATION"))
	SET RELATION TO Piktkt.piktkt INTO Pikline ADDITIVE
ENDIF
*B802796,1 WAB - END

*--E301077,68 (Start)
*SELECT StyDye
*SET ORDER TO StyDye
*--E301077,68 (End)

*B604186,1 HBG 02/07/2001 The program was transfered to root. [Begin]
*DO (gcScrDir + gcWinAppl + '\ALPLIST.SPR')      && calling the screen Mainvlo
DO (gcScrDir + '\ALPLIST.SPR')      && calling the screen Mainvlo
*B604186,1 [End  ]

SELECT Pack_Hdr
SET RELATION TO
SELECT OrdLine
SET RELATION TO
SELECT Pack_Lin
SET RELATION TO

=lfRELPad()
RELEASE WINDOW (lcDtlBrTtl)
RELEASE WINDOW (lcCtHdBrTt)
RELEASE WINDOW (lcCtDtBrTt)
= lfDeActPad()

*-- If we realy quitting the screen
IF glQuitting
	SELECT PikTkt
	SET RELATION TO
	SELECT OrdLine
	SET RELATION TO
	SELECT Pack_Lin
	SET RELATION TO

	*B802029,1 Remove these lines because there is no need for it any more, the
	*          system will close the file when the screen is closed because
	*          it was opened with (gfOpenFile) [Begin]
	*IF llEdiopn
	*  *--E301077 (Start)
	*  *USE IN EdiAcc
	*  = gfCloseFile("EdiAcc")
	*  *--E301077 (End)
	*ENDIF
	*B802029,1 Remove these lines because there is no need for it any more [End]

	*--E301077 (Start)
	*IF llB_H_Opn
	*  USE IN Bol_Hdr
	*ENDIF
	*IF llB_D_Opn
	*  USE IN Bol_Lin
	*ENDIF
	*--E301077 (End)

	IF USED(lcPckLin)
		USE IN (lcPckLin)
		*--C102281 AAN Remove the alias [Begin].
		IF USED(lcTmpPck)
			USE IN (lcTmpPck)
		ENDIF
		*--C102281 AAN Remove the alias [End].
	ENDIF
	ERASE (gcWorkDir+lcPckLin+".DBF")
	ERASE (gcWorkDir+lcPckLin+".CDX")
	IF USED(lcCtnHdr)
		USE IN (lcCtnHdr)
	ENDIF
	ERASE (gcWorkDir+lcCtnHdr+".DBF")
	ERASE (gcWorkDir+lcCtnHdr+".CDX")
	IF USED(lcCtnDtl)
		USE IN (lcCtnDtl)
	ENDIF
	ERASE (gcWorkDir+lcCtnDtl+".DBF")
	ERASE (gcWorkDir+lcCtnDtl+".CDX")

	IF USED(lcCartonSz)
		USE IN (lcCartonSz)
	ENDIF
	IF USED(lcScaFile)
		USE IN (lcScaFile)
	ENDIF
	ERASE (gcWorkDir+lcScaFile+".DBF")
	ERASE (gcWorkDir+lcScaFile+".CDX")

	*B602797,1 Save user setting to memory file [Begin]
	SAVE TO (gcDataDir+'UpdtPkTk.MEM') ALL LIKE llUpdtPkTk
	*B602797,1 Save user setting to memory file [End  ]

ENDIF

*!*************************************************************
*! Name      : lpShow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Handling the screen mode
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfWHBrow,lfwBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************

PROCEDURE lpShow
PRIVATE lnCurAlias,lcFilToUse

lnCurAlias = SELECT(0)

DO CASE
	*-- Select mode
CASE laScrMode[1]
	lcScrMode  = "S"
	IF lfChkUnCmS()
		= lfChngFolder(lnActFolder)
		RETURN
	ENDIF

	STORE SPACE(0) TO lcCusPo,lcDept,lcShVDesc,lcPkChCode,lcPkDsCode,;
		lcStoreDsc,lcDistCtr,lcBol,lcPackNo,lcOrderNo,lcStore,;
		lcOldVal,lcCtnSty,lcStySz,lcLsVPck,lcPrvPack
	lcSel = "Select"
	STORE "DISABLE" TO lcStoStat,lcCtnStat,lcPikStat,lcShipStat,lcInstStat,;
		lcCartNoSt,lcTotWghSt,lcPalNoSt,lcCtnPalSt,lcCtnStySt,lcStySzSt,;
		lcStyQtySt,lcStyWghSt,lcAddHdrSt,lcRemHdrSt,lcAddDtlSt,;
		lcRemDtlSt
	*E120116,1  TMI [Start] Disable Carrier Carton ID field in select mode
	STORE 'DISABLE' TO lcCrCtnIDSt
	*E120116,1  TMI [End  ]
	STORE "ENABLE" TO lcPackStat,lcOrdStat,lcAccStat
	STORE 0 TO lnLastFold,lnShpVia,lnRelCho,lnTotWgh,lnPalNo,lnCtnPal,lnStyQty,lnStyWgh,;
		lnCartNo,lnHdBrRec,lnDtBrRec,lnCtHdBrRe,lnCtDtBrRe,lnFrom,lnTo,;
		lnMaxPal,lnMinPal,lnLastNo,lnMaxCtn,lnLastCtn,;
		lnPackWgh,lnPackQty,lnPackCtn,lnPrvMode,lnBrCtnQty,lnBrUntWgh
	*E301373,1 NAD 02/14/2000 (Start) Change the default values
	*STORE 1 TO lnCtnTyp,lnDrctTo
	STORE 2 TO lnCtnTyp
	STORE 1 TO lnDrctTo
	*E301373,1 NAD  (End)
	STORE .F. TO llBrowse,llPCDStat,llPalStat,llClrSel,llShwOpn,llNew,;
		llPckCopy,llSelOrdLi,llUnComp,llCUpdate,llAnyUpd,llAnyRec,;
		llQtyArr,llWghArr
	SET MARK OF BAR 1 OF _OPTPOP TO .F.
	SET MARK OF BAR 2 OF _OPTPOP TO .F.

	*B602797,1 mark the option menu [Begin]
	SET MARK OF BAR IIF(llCanPrnLb,6,4) OF _OPTPOP TO llUpdtPkTk
	*B602797,1 mark the option menu [End  ]

	*--- varaiables that used for uncomplete session-------------
	*-- lnUnCmSeRc This is to hold the uncomplete session record
	*   no in the "uncmsess" file
	lnUnCmSeRc = 0
	*------------------------------------------------------------

	= gfwCodePop(@laCodInfo , "SHIPVIA" , "N")
	SELECT Pack_Hdr
	SCATTER FIELDS &lcScFields TO laData BLANK
	lcWareCode = ''
	lcBol      = ''

	SELECT(lcPckLin)
	SET FILTER TO
	SELECT (lcCtnHdr)
	SELECT (lcCtnDtl)
	SET FILTER TO

	_CUROBJ = OBJNUM(laData[1])

	*E301373,1 NAD (Start) Reset the filter
	*B605583,1 HBG 24/02/2002 Reset the filter of temp pack line file[Begin]
	*=lfReset()
	DIMENSION laStyTrgt(1)
	DIMENSION laClrTrgt(1)
	STORE SPACE(0) TO laStyTrgt,laClrTrgt
	SELECT (lcPckLin)
	SET FILTER TO
	*B605583,1 [End]
	*E301373,1 NAD (End)

	*-- View mode
CASE laScrMode[2]
	lcScrMode  = "V"
	SELECT Pack_Hdr
	SCATTER FIELDS &lcScFields TO laData

    *B132318,1 NNA 06/18/2006 (Begin) if the packing list generated on piktkt then I'll check the piktkt status,if it compleated
    *B132318,1 NNA            then I'll prevent user from editing the packing list . but if it generated on the order directly
    *B132318,1 NNA            then I'll check the order status.
    IF !EMPTY(LADATA[3])
      IF SEEK(IIF(ORDER('PIKTKT')='ORDPIK',LADATA[2]+LADATA[3],LADATA[3]),'PIKTKT')
        IF PIKTKT.STATUS='C'
          *-- To disable the edit bar in the record pad in the system menu.
          laCtrStat[7] = 'DISABLE'
          *-- Disable the edit button on the control pannel all the time.
          SHOW GET pbEdt DISABLE
        ENDIF
      ENDIF
    ELSE
      IF ORDER('ORDHDR')='ORDHDR' AND SEEK('O' + LADATA[2] ,'ORDHDR') AND ORDHDR.STATUS='C'
        *-- To disable the edit bar in the record pad in the system menu.
        laCtrStat[7] = 'DISABLE'
        *-- Disable the edit button on the control pannel all the time.
        SHOW GET pbEdt DISABLE
      ENDIF
    ENDIF
    *B132318,1 NNA (End)

	lnPackWgh = laData[08]
	lnPackCtn = laData[09]
	lnPackQty = laData[10]

	lcWareCode = Pack_Hdr.cWareCode
	lcBol      = Pack_Hdr.Bill_Ladg
	STORE nLastCart TO lnMaxCtn
	STORE nLastCart+1 TO lnFrom,lnTo

	lcCusPo    = IIF(OrdHdr.MultiPo,IIF(SEEK('O'+ladata[2]+laData[5],'OrdLine'),OrdLine.CustPo,SPACE(10)),OrdHdr.CustPo)

	*B802029,1 Change these lines to use the EDIACPRT and EDIPH files
	*          instead of the file EDIACC because we have replaced the file
	*          EDIACC with the files EDIACPRT, EDIPH and EDIPD [Begin]
	*llEdiAcc   = llEdiSys .AND. SEEK(laData[4],'EdiAcc')
	*STORE llEdiSys .AND. llEdiAcc .AND. EdiAcc.lPkChrDes  TO llPCDStat
	*STORE llEdiSys .AND. llEdiAcc .AND. EdiAcc.lnShipnPlt TO llPalStat
	llEdiAcc   = llEdiSys .AND. SEEK('A' + laData[4] , 'EDIACPRT') .AND. ;
		SEEK(EDIACPRT.cPartCode , 'EDIPH')

	STORE llEdiSys .AND. llEdiAcc .AND. EDIACPRT.lPkChrDes  TO llPCDStat
	STORE llEdiSys .AND. llEdiAcc .AND. EDIPH.lPltShp TO llPalStat
	*B802029,1 Change these lines to use the EDIACPRT and EDIPH files [End]
	*E120116,1  TMI [Start] disable Carrier Carton ID field in view mode
	STORE 'DISABLE' TO lcCrCtnIDSt
	*E120116,1  TMI [End  ]

	*E302210,1 WLD 09/09/2004 Using visual label report
	IF llAsnSys
		=SEEK(Pack_Hdr.Bill_Ladg,'Bol_hdr')
	ENDIF
	*E302210,1 WLD (End)

	*-- This part to enforce change to the first folder while the navigation
	IF lnActFolder <> 1 AND VARREAD() $ "PBTOP,PBPRVS,PBNXT,PBBTM"
		lnLastFold = lnActFolder
		lnActFolder = 1
		llNoThing   = lfChngFolder(lnActFolder)
	ENDIF
	*E301373,1 NAD 02/14/2000 (Start) Set the value to Standered,Store instead of N/A
	*lnCtnTyp = IIF(Pack_Hdr.LStandCtn,2,3)
	lnCtnTyp = IIF(Pack_Hdr.LStandCtn,1,2)
	*lnDrctTo = IIF(Pack_Hdr.CToStorCn='S',2,IIF(Pack_Hdr.CToStorCn='C',3,1))
	lnDrctTo = IIF(Pack_Hdr.CToStorCn='C',2,1)
	*E301373,1 NAD (End)

	lnShpVia = laData[7]
	laCodInfo[1,01] = "SHIPVIA"                 && Field Name
	laCodInfo[1,02] = "laShpVia"                && Array Name
	laCodInfo[1,03] = "lnShpVia"                && Popup Name
	laCodInfo[1,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
	laCodInfo[1,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
	laCodInfo[1,06] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
	laCodInfo[1,07] = "Pack_Hdr"                && Alternative File (For default val.)
	laCodInfo[1,08] = "Pack_Hdr"                && Use this index for the Alternative file.
	laCodInfo[1,09] = "laData[1]"               && Seek this expretion.
	laCodInfo[1,10] = "ShipVia"                 && Alternative Field Name
	= gfwCodePop(@laCodInfo, "SHIPVIA", "T")
	STORE "DISABLE" TO lcStoStat,lcCtnStat,lcPikStat,lcShipStat,lcInstStat,;
		lcPackStat,lcOrdStat,lcAccStat
	IF !llCupdate
		DO lpClsScr
	ENDIF

	*E301373,1 NAD (Start) Reset the filter
	*B605583,1 HBG 24/02/2002 Reset the filter of temp pack line file[Begin]
	*=lfReset()
	DIMENSION laStyTrgt(1)
	DIMENSION laClrTrgt(1)
	STORE SPACE(0) TO laStyTrgt,laClrTrgt
	SELECT (lcPckLin)
	SET FILTER TO
	*B605583,1 [End]
	*E301373,1 NAD (End)
	*-- Edit mode
CASE laScrMode[3]

	*B603807,1 HBG 08/09/2000 Can't modify if this packing list is shiped [Begin]
	IF Pack_Hdr.Status = 'C'
		PRIVATE lcMessage
		*-- This packing list is shipped. Can't be Deleted
		*-- OK
		lcMessage = "Can't be modified."
		= gfModalGen("INM44102B00000","Dialog",lcMessage)
		STORE .F. TO laScrMode
		laScrMode[2] = .T.
		SELECT Pack_Hdr
		= gfObj_Lock(.F.)
		SHOW GETS
		llGoOn = lfChkOrdLok()
		RETURN
	ENDIF
	*B603807,1 [End]

	*E120116,1  TMI [Start] Enable Carrier Carton ID field based on the check box in folder1 (ladatap18])
	STORE IIF(laData[18],'ENABLE','DISABLE') TO lcCrCtnIDSt
	*E120116,1  TMI [End  ]

	lcScrMode = "E"
	*-- check if any pack is being created now on the same order in any
	*-- other session
	llGoOn = lfChkOrdLok()
	IF llGoOn
		IF !llUnComp
			llNoThing = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)
		ENDIF
	ELSE
		laDcrMode = .F.
		laScrMode[2] = .T.
		SHOW GETS
	ENDIF

	*-- Add mode
CASE laScrMode[4]
	lcScrMode  = "A"
	IF llUnComp AND llNew
		*-- This part is to fill ShipVia list to refresh lnShpVia field
		*-- in case detecting uncomp. session in Add Mode and the lnSpVia
		*-- had been changed befor illegal termination
		lnOrjShp = lnShpVia
		IF lnShpVia <> 1
			lnShpVia = 1
			= gfwCodePop(@laCodInfo, "SHIPVIA", "L")
		ENDIF
		lnShpVia = lnOrjShp
		SHOW GET lnShpVia
	ENDIF

	IF !llNew
		STORE "ENABLE" TO lcPikStat,lcOrdStat,lcAccStat
		STORE "DISABLE" TO lcPackStat

		STORE 0 TO lnShpVia,lnPackQty,lnPackWgh,lnPackCtn
		*E301373,1 NAD 02/14/2000 (Start) Change the Dehfault values
		*STORE 1 TO lnCtnTpy,lnDrctTo
		STORE 2 TO lnCtnTpy
		STORE 1 TO lnDrctTo
		*E301373,1 NAD  (End)
		= gfwCodePop(@laCodInfo, "SHIPVIA", "N")

		_CUROBJ = OBJNUM(laData[2])
	ENDIF

ENDCASE

= lfActFolder()

SHOW GET pbPackNo  &lcPackStat
SHOW GET laData[1] &lcPackStat

SHOW GET pbOrder   &lcOrdStat
SHOW GET laData[2] &lcOrdStat

SHOW GET pbPickTkt &lcPikStat
SHOW GET laData[3] &lcPikStat

SHOW GET pbAcc     &lcAccStat
SHOW GET laData[4] &lcAccStat
SHOW GET laData[6] &lcInstStat
SHOW GET lnShpVia  &lcShipStat

SHOW GET laData[11] &lcInstStat
SHOW GET laData[12] &lcInstStat

*c101735,1
*B604145,1 HBG 03/06/2002 Disable B.O.L field if generate B.O.L Automatic is 'Yes' [begin]
IF llBolAutom
	SHOW GET lcBol DISABLE
ELSE
	*B604145,1 [End]
	SHOW GET lcBol &lcInstStat
	*B604145,1 HBG 03/06/2002 End if generate B.O.L Automatic is 'Yes' [begin]
ENDIF
*B604145,1 [End]

SELECT (lnCurAlias)
*--

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key laData[1] .
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_1()
*!*************************************************************

FUNCTION lfvData_1

PRIVATE lnCurAlias,lcPCkHdrTag

lcPCkHdrTag = ORDER('Pack_Hdr')

SET ORDER TO Pack_Hdr IN Pack_Hdr

IF llBrowse OR (!EMPTY(laData[1]) AND !SEEK(laData[1],'Pack_Hdr')))
	llBrowse  = .F.
	laData[1] = IIF(lfPakNoBrw(),Pack_Hdr.Pack_No,SPACE(6))
ENDIF

IF !EMPTY(laData[1])
	laData[4] = Pack_Hdr.Account
	laScrMode    = .F.
	laScrMode[2] = .T.
	SHOW GETS
ENDIF


SET ORDER TO TAG lcPCkHdrTag IN Pack_Hdr

*!*************************************************************
*! Name      : lfPakNoBrw
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : browse all pack list or validated by the account
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : AriaBrow,gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : llReturn
*!*************************************************************
*! Example   : = lfPakNoBrw()
*!*************************************************************

FUNCTION lfPakNoBrw

*-- lcFields   variable that hold the name of browsed fields
*-- laBrow     array that hold the returned values from AriaBrow function
*-- lnCurAlias variable that hold the current alias
*-- lcCurTag   variable that hold the currend tag name
*-- llReturn   variable which is returned by this function
*-- lcTag      variable that hold the name of the tag which is desired to switch
*              file order to it

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "Pack_No,Order,Account,Store,Note,Tot_Wght"
lcBrFields  = [Pack_No  :H='Pack #',]+;
	[Order    :H='Order#',]+;
	[Account  :H='Account',]+;
	[Store    :H='Store #',]+;
	[Bill_Ladg:H='Bill of Lading',]+;
	[Tot_Wght :H='Tot. Weight']
lcFile_Ttl  = 'Packing Slips'
SELECT Pack_Hdr
lcCurTag = ORDER('Pack_Hdr')

*-- Here, we use AccPack Tag if we need to validate for particular account or any
*   account by using Alltrim(laData[4]), other wise we will use orderpck tag
lcTag = IIF((EMPTY(laData[2]) AND !EMPTY(laData[4]) OR llPckCopy),"AccPack","OrderPck")
SET ORDER TO &lcTag
IF !SEEK(IIF(ORDER('Pack_Hdr')="ACCPACK",laData[4],ALLTRIM(laData[2])),'Pack_Hdr')
	*-- There are no records to browse.
	*-- OK
	= gfModalGen("INM44032B00000","Dialog")
ELSE
	llReturn = AriaBrow(IIF(ORDER('Pack_Hdr')="ACCPACK","laData[4]","ALLTRIM(laData[2])"),lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'Pack_Hdr',.F.)
ENDIF

SET ORDER TO lcCurTag IN Pack_Hdr
SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfvData_2
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key laData[2] .
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfGetData,OrdBrowA,lfvNewPack
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_2()
*!*************************************************************

FUNCTION lfvData_2

PRIVATE lnCurAlias,lcTag,lcOrder
lnCurAlias = SELECT(0)

lcTag = ORDER('OrdHdr')
SET ORDER TO OrdHdr IN OrdHdr

IF !( (EMPTY(laData[2]) OR LASTKEY() <> 13) AND !llBrowse)
	IF (!EMPTY(laData[2]) .AND. !SEEK('O'+laData[2],'OrdHdr') ) OR llBrowse
		lcOrder = laData[2]
		=lfOrdBrow(@lcOrder,laData[4])
		laData[2] = lcOrder
		llBrowse = .F.
	ENDIF
ENDIF
IF !EMPTY(laData[2]) AND SEEK('O'+laData[2],'OrdHdr')
	DO CASE
	CASE OrdHdr.Status = 'C'
		*-- This order is completed, cannot pack.
		*-- OK
		= gfModalGen("INM44050B00000","Dialog","completed")
		laData[2] = SPACE(6)
		SHOW GET laData[2]

	CASE OrdHdr.Status = 'X'
		*-- This order is canceled, cannot pack.
		*-- OK
		= gfModalGen("INM44050B00000","Dialog","canceled")
		laData[2] = SPACE(6)
		SHOW GET laData[2]

	OTHERWISE
		*B603515,1 CORRECTING DATA COLLECTION [Begin]
		=lfGetData()
		*B603515,1 CORRECTING DATA COLLECTION [End]

		*B803778,1 HBG 11/16/2000 Set order of PIKTKT To ORDPIK  to check if there is
		*B803778,1                a Pick Ticket for this order or not [Begin]
		SET ORDER TO ORDPIK IN PIKTKT
		*B803778,1 [End]

		*-- llHaSPik variable that indicates if this order has pikTkt or not
		IF SEEK(laData[2],'PikTkt')
			lnAlias = SELECT(0)
			SELECT PikTkt

			*C101735,1 Filter on Opened, Pulled or completed Piktkts [Begin]
			*LOCATE REST WHILE Order = laData[2] FOR !(Status=='X')
			*B603807,1 HBG 08/10/2000 Don't allow creating packing list on completed pick ticket [Begin]
			*LOCATE REST WHILE Order = laData[2] FOR Status$'POC'
			LOCATE REST WHILE ORDER = laData[2] FOR STATUS$'PO'
			*B603807,1 [End]
			*C101735,1 Filter on Opened, Pulled or completed Piktkts [End  ]

			IF FOUND()
				llHasPik = .T.
			ELSE
				llHasPik = .F.
			ENDIF
			SELECT (lnAlias)
		ELSE
			llHasPik = .F.
		ENDIF
		DO CASE
			*-- this order has piktkt and it is multi store
			*-- piktkt could be selected to create Pack list for piktkt or
			*-- store could be selected to create Pack list for order
		CASE llHasPik AND laScrMode[4] AND OrdHdr.Multi = 'Y'
			lcStoStat = "ENABLE"
			_CUROBJ = OBJNUM(laData[3])

			*-- this order has piktkt and it is single store
			*-- piktkt could be selected to create Pack list for piktkt or
			*-- if piktkt is left empty that creates pack list for order
		CASE llHasPik AND laScrMode[4] AND OrdHdr.Multi = 'N'
			lcStoStat = "DISABLE"
			_CUROBJ = OBJNUM(laData[3])

			*-- this order has no piktkt and it is multi store
			*-- this pack list is for order and a store should be selected
		CASE !llHasPik AND laScrMode[4] AND OrdHdr.Multi = 'Y'
			lcStoStat = "ENABLE"
			_CUROBJ = OBJNUM(laData[5])

			*-- this order has no piktkt and it is single store
			*-- so the processing goes in creating pack list for order with
			*-- selecting order or piktkt
		CASE !llHasPik AND laScrMode[4] AND OrdHdr.Multi = 'N'
			lcStoStat = "DISABLE"
			= lfvNewPack()

		ENDCASE

		SHOW GET laData[5] &lcStoStat
		SHOW GET pbStore   &lcStoStat

	ENDCASE
ENDIF

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
*! Name      : lfvData_3
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key laData[3] .
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfPikTktBrow,gfModalGen,lfGetData,lfvNewPack
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_3()
*!*************************************************************

FUNCTION lfvData_3

*-- lcPikTag variable that hold piktkt file tag name
*-- lnCurAlias variable that hold the current alias
PRIVATE lcPikTag,lnCurAlias
*-- this condition is to prevent excute this function in case user
*-- enter piktkt no and click the browse button
*-- because in this case this function is excuted twice, first for validate
*-- the field ,second because of clicking the browse button

*B802953,1 (Start)
*llMdown = MDOWN()
IF MDOWN()
	RETURN
ENDIF
*B802953,1 (End)


IF llBrowse OR "?" $ laData[3]
	llBrowse  = .T.
	laData[3] = ''
ENDIF


IF !( LASTKEY() <> 13 AND !llBrowse)
	*IF !(EMPTY(laData[3]) AND llMdown)
	lcPikTag = ORDER("PikTkt")
	SET ORDER TO TAG IIF(EMPTY(laData[2]),'PikTkt','OrdPik') IN PikTkt



	*B603807,1 HBG 08/16/2000 User can not enter a completed pick ticket [Begin]
	*IF llBrowse OR !SEEK(ALLTRIM(laData[2])+laData[3],'PikTkt')
	IF llBrowse OR !SEEK(ALLTRIM(laData[2])+laData[3],'PikTkt') OR ;
			(SEEK(ALLTRIM(laData[2])+laData[3],'PikTkt') AND PikTkt.Status = 'C')
		*B603807,1 [End]
		llBrowse = .F.
		DO CASE
			*-- this means browse will be validated by account
		CASE EMPTY(laData[2]) AND !EMPTY(laData[4])
			lnCurAlias = SELECT(0)
			SELECT PikTkt

			*C101735,1 Comment out the if block because we are already in it. [Begin]
			*IF !EMPTY(laData[4])
			*C101735,1 Filter on Opened, Pulled or completed Piktkts [Begin]
			*LOCATE FOR Account = laData[4] AND PikTkt.Status <> 'X'
			*B603807,1 HBG 08/10/2000 Don't allow creating packing list on completed pick ticket [Begin]
			*LOCATE FOR Account = laData[4] AND PikTkt.Status $ "POC"
			LOCATE FOR Account = laData[4] AND PikTkt.Status $ "PO"
			*B603807,1 [End]
			*C101735,1 Filter on Opened, Pulled or completed Piktkts [End  ]
			*ENDIF
			*C101735,1 Comment out the if block because we are already in it. [End  ]

			IF FOUND()
				laData[3] = IIF(lfPikTktBrow(),PikTkt.PikTkt,SPACE(6))
			ELSE
				*-- 'No piktkts found for account laData[4]
				*-- <OK>
				llNoThing = gfModalGen("INM44043B00000","Dialog",laData[4])
				STORE SPACE(6) TO laData[3]
			ENDIF
			SELECT (lnCurAlias)
			SHOW GET laData[3]
		OTHERWISE
			IF EMPTY(laData[2])
				laData[3] = IIF(lfPikTktBrow(),PikTkt.PikTkt,SPACE(6))

			ELSE

				IF SEEK(ALLTRIM(laData[2]),'PikTkt')
					SELECT PikTkt

					*C101735,1 Filter on Opened, Pulled or completed Piktkts [Begin]
					*LOCATE REST WHILE Order = laData[2] FOR !(Status=='X')
					*B603807,1 HBG 08/10/2000 Don't allow creating packing list on completed pick ticket [Begin]
					*LOCATE REST WHILE Order = laData[2] FOR Status $ "POC"
					LOCATE REST WHILE ORDER = laData[2] FOR STATUS $ "PO"
					*B603807,1  [End]
					*C101735,1 Filter on Opened, Pulled or completed Piktkts [End  ]

					IF FOUND()
						laData[3] = IIF(lfPikTktBrow(),PikTkt.PikTkt,SPACE(6))
					ENDIF
				ELSE
					*-- 'No piktkts found for order laData[2]
					*-- <OK>
					llNoThing = gfModalGen("INM44028B00000","Dialog",laData[2])
					STORE SPACE(6) TO laData[3]
				ENDIF
			ENDIF
			SHOW GET laData[3]
		ENDCASE

	ENDIF

	IF !EMPTY(laData[3])
		IF SEEK(laData[3],'Pack_Hdr')
			laScrMode = .F.
			laScrMode[2] = .T.
			SHOW GETS
		ELSE
			laData[4] = PikTkt.Account
			laData[2] = PikTkt.Order

			IF PikTkt.Status = 'H'
				*-- This picking ticket is on hold.
				*-- OK
				= gfModalGen("INM44045B00000","Dialog")
			ENDIF

			= lfGetData()
			SHOW GET laData[1]
		ENDIF
	ENDIF

	*-- laData[3] could be empty while laData[2] is empty, but while ladata[3]
	*-- is validated and not empty ladata[2] could not be empty
	IF (!EMPTY(laData[2]) OR !EMPTY(laData[3])) AND laScrMode[4]
		=SEEK('O'+laData[2],'OrdHdr')
		DO CASE
		CASE OrdHdr.Multi = 'Y' AND !EMPTY(laData[3])
			laData[5] = PikTkt.Store
			lcStoStat = "DISABLE"

			= lfvNewPack()
		CASE OrdHdr.Multi = 'Y' AND EMPTY(laData[3])
			lcStoStat = "ENABLE"
			_CUROBJ = OBJNUM(laData[5])
		CASE OrdHdr.Multi = 'N'
			=lfvNewPack()
		ENDCASE
	ENDIF

	SHOW GET pbStore   &lcStoStat
	SHOW GET laData[5] &lcStoStat

	SET ORDER TO lcPikTag IN PikTkt
ENDIF

*!*************************************************************
*! Name      : lfPikTktBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : browse all piktkt or validated by the account or order
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : AriaBrow,gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : llReturn
*!*************************************************************
*! Example   : = lfPikTktBrow()
*!*************************************************************

FUNCTION lfPikTktBrow

*-- lcFields   variable that hold the name of browsed fields
*-- laBrow     array that hold the returned values from AriaBrow function
*-- lnCurAlias variable that hold the current alias
*-- lcCurTag   variable that hold the currend tag name
*-- llReturn   value that is returned by this function
*-- lcTag      variable that hold the name of the tag which is desired to switch
*              file order to it

PRIVATE lcBrFields,lcFile_Ttl,laBrow,lcFields,lnCurAlias,lcCurTag,llReturn,;
	lcBrFields
DIMENSION laBrow[7]
STORE SPACE(0) TO laBrow

lnCurAlias = SELECT(0)

lcFields    = "PikTkt,Order,Store,OrdHdr.Complete,Date,Account,Customer.StName"
lcBrFields  = [PikTkt:H='PikTkt',]+;
	[Order :H='Order#',]+;
	[Store :H='Store' ,]+;
	[OrdHdr.Complete:H='Complete',]+;
	[Date:H='PikDate',]+;
	[Account:H='Account',]+;
	[Customer.StName:H='Account Name']
lcFile_Ttl = 'Pick Tickets'

SELECT OrdHdr
lcCurTag = ORDER('OrdHdr')
SET ORDER TO OrdAcct
lcKey = IIF(EMPTY(laData[4]),'',laData[4])
SET KEY TO lcKey

SELECT PikTkt
SET RELATION TO Account+'O'+ORDER INTO OrdHdr ADDITIVE
IF EMPTY(laData[2]) AND !EMPTY(laData[4])
	*C101735,1 Filter on Opened, Pulled or completed Piktkts [Begin]
	*llReturn = AriaBrow("'' FOR PikTkt.Account=laData[4] AND !EOF('OrdHdr') AND PikTkt.Status='O' OR PikTkt.Status = 'H'",;
	lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
	gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'PikTkt',.F.)
*B603807,1 HBG 08/10/2000 Don't allow creating packing list on completed pick ticket [Begin]
*llReturn = AriaBrow("'' FOR PikTkt.Account=laData[4] AND !EOF('OrdHdr') AND PikTkt.Status$'POC'",;
lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'PikTkt',.F.)
llReturn = AriaBrow("'' FOR PikTkt.Account=laData[4] AND !EOF('OrdHdr') AND PikTkt.Status$'PO'",;
	lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
	gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'PikTkt',.F.)
*B603807,1  [End]
*C101735,1 Filter on Opened, Pulled or completed Piktkts [End  ]
ELSE
*C101735,1 Filter on Opened, Pulled or completed Piktkts [Begin]
*llReturn = AriaBrow("FOR Order = ALLTRIM(laData[2]) AND Status = 'O'",;
lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'PikTkt',.F.)
*B603807,1 HBG 08/10/2000 Don't allow creating packing list on completed pick ticket [Begin]
*llReturn = AriaBrow("FOR Order = ALLTRIM(laData[2]) AND Status $ 'POC'",;
lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'PikTkt',.F.)
llReturn = AriaBrow("FOR Order = ALLTRIM(laData[2]) AND Status $ 'PO'",;
	lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
	gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'PikTkt',.F.)
*B603807,1  [End]
*C101735,1 Filter on Opened, Pulled or completed Piktkts [End  ]
ENDIF

SELECT PikTkt
SET RELATION TO
SET ORDER TO lcCurTag IN OrdHdr
SELECT OrdHdr
SET KEY TO ''
SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfvNewPack
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : create new pack list according keys fields validation
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfPackCopy,fvSelOrdL,lfWinHdrSt,lfUpdVars,
*!                          lfWinCtnSt
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNewPack()
*!*************************************************************

FUNCTION lfvNewPack
PRIVATE lcTag,lnCurAlias

STORE .T. TO llCupdate,llAnyUpd,llNew

*-- check if any pack is being created now on the same order in any
*-- other session
llGoOn = lfChkOrdLok()

IF llGoOn
	*B603515,1 Get shipvia in case of shipvia at store level
	IF ALLTRIM(OrdHdr.ShipVia)='*' AND SEEK('S'+laData[4]+laData[5],'Customer')
		laData[7]  = Customer.ShipVia
		laCodInfo[1,07] = "customer"                && Alternative File (For default val.)
		laCodInfo[1,08] = "customer"                && Use this index for the Alternative file.
		laCodInfo[1,09] = "'S'+laData[4]+laData[5]"         && Seek this expretion.
		laCodInfo[1,10] = "ShipVia"               && Alternative Field Name
		= gfwCodePop(@laCodInfo, "SHIPVIA", "T")
	ENDIF
	*B603515,1 (End)

	*C101735,1 Genrate new BOL [Begin]
	=lfNewBOL()
	*C101735,1 Genrate new BOL [End  ]

	IF lnActFolder = 3
		= lfWinCtnSt()
	ENDIF

	lcTag = ORDER('Pack_Hdr')

	SET ORDER TO AccPack IN Pack_Hdr
	IF SEEK(laData[4],'Pack_Hdr')
		*-- "Do you wish to copy from another pack list"
		*-- <YES>, <NO>
		IF gfModalGen("INM44044B00006","Dialog") = 1
			= lfPackCopy()
		ENDIF
	ENDIF
	SET ORDER TO lcTag IN Pack_Hdr
	= lfvSelOrdL()
	= lfWinHdrSt()
	llNoThing = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)
	llNoThing = lfUpdVars()
ELSE
	laDcrMode = .F.
	laScrMode[1] = .T.
	SHOW GETS
ENDIF

*!*************************************************************
*! Name      : lfWinHdrSt
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : adjust enable status for header folder fields
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWinHdrSt()
*!*************************************************************

FUNCTION lfWinHdrSt

IF laScrMode[3] OR llNew
	STORE "DISABLE" TO lcPackStat,lcOrdStat,lcPikStat,lcAccStat,lcStoStat
	IF lnActFolder = 1
		STORE "ENABLE" TO lcCtnStat,lcInstStat,lcShipStat
	ELSE
		STORE "DISABLE" TO lcCtnStat,lcInstStat,lcShipStat
	ENDIF
ENDIF

SHOW GET pbPackNo   &lcPackStat
SHOW GET laData[01] &lcPackStat

SHOW GET pbOrder    &lcOrdStat
SHOW GET laData[02] &lcOrdStat

SHOW GET pbPickTkt  &lcPikStat
SHOW GET laData[03] &lcPikStat

SHOW GET pbAcc      &lcAccStat
SHOW GET laData[04] &lcAccStat

SHOW GET laData[05] &lcStoStat
SHOW GET pbStore    &lcStoStat

SHOW GET laData[06] &lcInstStat

SHOW GET lnShpVia   &lcShipStat

SHOW GET laData[11] &lcInstStat
SHOW GET laData[12] &lcInstStat


SHOW GET lnCtnTyp
SHOW GET lnDrctTo
*c101735,1 [Begin]
*B604145,1 HBG 03/06/2002 Disable B.O.L field if generate B.O.L Automatic is 'Yes' [begin]
IF llBolAutom
	SHOW GET lcBol DISABLE
ELSE
	*B604145,1 [End]
	SHOW GET lcBol &lcInstStat
	*B604145,1 HBG 03/06/2002 End if generate B.O.L Automatic is 'Yes' [begin]
ENDIF
*B604145,1 [End]

*c101735,1 [End  ]



*!*************************************************************
*! Name      : lfvData_4
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key laData[4] .
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

laData[4] = PADR(ALLTRIM(laData[4]), FSIZE('Account', 'Pack_Hdr'))
XACCOUNT = laData[4]
IF llBrowse OR (!EMPTY(laData[4]) AND !SEEK('M'+XACCOUNT,'Customer'))
	llBrowse = .F.
	DO CUSBROWM WITH XACCOUNT
	laData[4] = XACCOUNT
ENDIF

IF EMPTY(laData[4])
	STORE SPACE(6) TO laData[2]
	SHOW GET laData[2]
ELSE
	= lfRefresh(lcWinKey)
ENDIF

*!*************************************************************
*! Name      : lfvData_5
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key laData[5] .
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : CUSBROWM,lfRefresh,lfvNewPack,gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_5()
*!*************************************************************

FUNCTION lfvData_5

PRIVATE xStore
xStore = laData[5]

IF llBrowse .OR. (!EMPTY(laData[5]) AND !SEEK('S'+laData[4]+laData[5],'Customer'))
	llBrowse  = .F.
	laData[5] = IIF(!CusBrowS(laData[4]),SPACE(8),xStore)
ENDIF
SELECT OrdLine
SET ORDER TO OrdLinSt
IF !EMPTY(laData[5]) AND !SEEK('O'+laData[2]+laData[5],'OrdLine')
	*-- No ordered quantity for Store laData[5]
	*-- <OK>
	llNoThing = gfModalGen("INM44031B00000","Dialog",laData[5])
	laData[5] = SPACE(8)
ELSE
	IF EMPTY(laData[2])
		IF OrdHdr.MultiPo AND SEEK('O'+laData[2]+laData[5],'OrdLine')
			lcCusPo=OrdLine.CustPo
		ELSE
			lcCusPo=OrdHdr.CustPo
		ENDIF
		lcStoreDsc = IIF(!EMPTY(laData[5]) AND SEEK("M"+laData[4],'Customer'),Customer.stName," ")
		=lfRefresh(lcWinKey)
		=lfRefresh(lcWinHdr)
	ENDIF
	IF !EMPTY(laData[5])
		= lfvNewPack()
	ENDIF

ENDIF

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To save the old value befor modifying it
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfOldValue()
*!*************************************************************

FUNCTION lfOldValue

lcOldVal = EVAL(VARREAD())

*!*************************************************************
*! Name      : lfGetData
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To restore the order or piktkt informtion form
*!             OrdHdr file or PikTkt file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfGetData()
*!*************************************************************

FUNCTION lfGetData

llNoThing = SEEK('O'+laData[2],'OrdHdr')

laData[4] = IIF(!EMPTY(laData[2]),OrdHdr.Account,SPACE(6))

*B802029,1 Change these lines to use the EDIACPRT and EDIPH files
*          instead of the file EDIACC because we have replaced the file
*          EDIACC with the files EDIACPRT, EDIPH and EDIPD [Begin]
*llEdiAcc   = llEdiSys .AND. SEEK(laData[4],'EdiAcc')
*
*STORE llEdiSys .AND. llEdiAcc .AND. EdiAcc.lPkChrDes  TO llPCDStat
*STORE llEdiSys .AND. llEdiAcc .AND. EdiAcc.lnShipnPlt TO llPalStat
llEdiAcc   = llEdiSys .AND. SEEK('A' + laData[4] , 'EDIACPRT') .AND. ;
	SEEK(EDIACPRT.cPartCode , 'EDIPH')

STORE llEdiSys .AND. llEdiAcc .AND. EDIACPRT.lPkChrDes  TO llPCDStat
STORE llEdiSys .AND. llEdiAcc .AND. EDIPH.lPltShp  TO llPalStat
*B802029,1 Change these lines to use the EDIACPRT and EDIPH files [End]

laData[5]  = IIF(EMPTY(laData[2]),SPACE(8),IIF(!EMPTY(laData[3]),PikTkt.Store,;
	IIF(OrdHdr.Multi='Y',SPACE(8),OrdHdr.Store)))
lcWareCode = IIF(EMPTY(laData[2]),'',IIF(!EMPTY(laData[3]),PikTkt.cWareCode,OrdHdr.cWareCode))

lcCusPo    = IIF(OrdHdr.MultiPo,IIF(SEEK('O'+ladata[2]+laData[5],'OrdLine'),OrdLine.CustPo,SPACE(10)),OrdHdr.CustPo)
lcDept     = IIF(EMPTY(laData[2]),'',OrdHdr.Dept)

laData[7]  = IIF(EMPTY(laData[2]),SPACE(6),OrdHdr.ShipVia)
laCodInfo[1,07] = "OrdHdr"                && Alternative File (For default val.)
laCodInfo[1,08] = "OrdHdr"                && Use this index for the Alternative file.
laCodInfo[1,09] = "'O'+laData[2]"         && Seek this expretion.
laCodInfo[1,10] = "ShipVia"               && Alternative Field Name
= gfwCodePop(@laCodInfo, "SHIPVIA", "T")
SHOW GET lnShpVia

IF llEdiAcc
	lcCtnStat  = IIF(laScrMode[1] OR laScrMode[2],"DISABLE","ENABLE")

	*B802029,1 Change these lines to use the EDIACPRT and EDIPH files
	*          instead of the file EDIACC because we have replaced the file
	*          EDIACC with the files EDIACPRT, EDIPH and EDIPD [Begin]
	*laData[15] = IIF(EMPTY(laData[2]),SPACE(5),IIF(llPCDStat,EdiAcc.cPckChCode,SPACE(5)))
	*laData[16] = IIF(EMPTY(laData[2]),SPACE(7),IIF(llPCDStat,EdiAcc.cPckDsCode,SPACE(7)))
	laData[15] = IIF(EMPTY(laData[2]) , SPACE(5) ,;
		IIF(llPCDStat , EDIACPRT.cPckChCode , SPACE(5)))

	laData[16] = IIF(EMPTY(laData[2]) , SPACE(7) ,;
		IIF(llPCDStat , EDIACPRT.cPckDsCode , SPACE(7)))
	*B802029,1 Change these lines to use the EDIACPRT and EDIPH files [End]

	lcEdiObSt  = IIF(llPCDStat AND (laScrMode[3] OR llNew) AND lnActFolder = 1,"ENABLE","DISABLE")
	SHOW GET laData[15] &lcEdiObSt
	SHOW GET laData[16] &lcEdiObSt
	lcPalNoSt  = IIF(llPalStat AND (laScrMode[3] OR llNew) AND lnActFolder = 3,"ENABLE","DISABLE")
	SHOW GET lnPalNo &lcPalNoSt
ELSE
	*E301373,1 NAD 02/14/2000 (Start)  Set the value to Standered,Store instead of N/A
	*lnCtnTyp   = 3
	lnCtnTyp   = 2
	*E301373,1 NAD  (End)

	laData[13] = .F.                 && means carton type is standard
	lnDrctTo   = 1
	laData[14] = 'S'                 && means pack is directed to store
	lcCtnStat = "DISABLE"
ENDIF

SHOW GET lnCtnTyp &lcCtnStat
SHOW GET lnDrctTo &lcCtnStat

= lfRefresh(lcWinKey)
= lfRefresh(lcWinHdr)
= lfRefresh(lcWinDtl2)
= lfRefresh(lcWinCtn3)
SHOW GETS WINDOW (lcWinKey)  ONLY
SHOW GETS WINDOW (lcWinHdr)  ONLY
SHOW GETS WINDOW (lcWinDtl2) ONLY
SHOW GETS WINDOW (lcWinCtn3) ONLY

*!*************************************************************
*! Name      : lfvSelOrdL
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To collect order lines
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvSelOrdL()
*!*************************************************************

FUNCTION lfvSelOrdL

*-- lnAlias    variable that hold the current alias
*-- lnI        counter to be used for the 8 quantities or piked fields
*-- lcSize     variable that hold the size desc. from scale file
*-- lnQty      variable that hold the value in quantities fields
*-- lcStyle    variable that hold style
*-- lcStyDesc  variable that hold style desc.
*-- lnRemPQty  variable to be used in case style multi ordline is found
*--            that hold the remained quantity from style qty as long as
*--            style order line is packed

PRIVATE lnAlias,lnI,lnJ,lcSize,lnQty,lcStyle,lcStyDesc,lnContinue,;
	lnCtnQty,lnCtnWgh,lnStyOrdLin,llStyFound,lnRemPQty

*-- This variable indecates if the style is found more than once in order
llStyFound = .F.

*B607445,1  TMI [Start] Variable llCmplt is if order is completed with no piktkt
llCmplt = .F.
*B607445,1  TMI [End  ]

STORE 0 TO lnContinue,lnCtnQty,lnCtnWgh,lnI,lnJ,lnStyOrdLin,lnRemPQty

IF laScrMode[4] AND llNew
	WAIT WINDOW 'Reading order lines. Please stand by....' NOWAIT
ENDIF

llAnyUpd = .F.

lnPrvMode = ASCAN(laScrMode,.T.)
lcPrvPack = laData[1]

lnAlias = ALIAS()

*-- Creating the temp. files
llNothing = lfCrtUnCmp()

SET ORDER TO Ordlinst IN OrdLine
llNoThing = SEEK('O'+laData[2]+laData[5],'OrdLine')

STORE SPACE(0) TO lcStyle,lcStyDesc
lcExp = IIF(!EMPTY(laData[3]),"WHILE cOrdType+Order+Store='O'+laData[2]+laData[5] FOR PikTkt=laData[3] AND Picked",;
	"WHILE cOrdType+Order+Store='O'+laData[2]+laData[5]")
*B802796,1 WAB - check if this pick ticket is complete and collect data after that
*B802796,1 WAB - from pikline not from ordline
*B802796,1 WAB - START
*SELECT OrdLine
*--- llFrmPikLn --> .T. ----> colect data from pikline , .F. --> from ordline
llFrmPikLn = .F.
IF !EMPTY(laData[3])
	SELECT PIKTKT
	SET ORDER TO PIKTKT
	IF SEEK(laData[3]) AND STATUS = 'C'
		llFrmPikLn = .T.
		lcExp = "WHILE PikTkt+Order+Store=laData[3]+laData[2]+laData[5] FOR Picked "
	ENDIF
	*B607445,1  TMI [Start] if no piktkt and order is completed
ELSE
	=SEEK('O'+laData[2],'ORDHDR')
	llCmplt = ( ORDHDR.STATUS = 'C' ) .AND. EMPTY(laData[3])
	IF llCmplt
		lcSvOrd = ORDER('PACK_HDR')
		SET ORDER TO ORDERPCK IN PACK_HDR
		SELECT PACK_LIN
		lcSvRel = SET('RELATION')
		SET RELATION TO
	ENDIF
	*B607445,1  TMI [End  ]
ENDIF

*-- move the pointer to frist record
IF llFrmPikLn
	SELECT "PikLine"
	=SEEK(laData[3]+laData[2])
ELSE
	SELECT "OrdLine"
	=SEEK('O'+laData[2]+laData[5])
ENDIF
*B802796,1 WAB - END

SCAN REST &lcExp
	SELECT (lcPckLin)
	APPEND BLANK
	*B802796,1 WAB - get data from pikline in case of pik ticket is complete
	*B802796,1 WAB - START
	*REPLACE Style     WITH OrdLine.Style,;
	Scale      WITH Scale.Scale,;
	SzCnt      WITH Scale.Cnt,;
	StyWgh     WITH Style.nStyWeight,;
	OrgStyWgh  WITH Style.nStyWeight,;
	nOrdLineNo WITH OrdLine.LineNo,;
	LPicked    WITH OrdLine.Picked,;
	cSize1     WITH Scale.Sz1,;
	cSize2     WITH Scale.Sz2,;
	cSize3     WITH Scale.Sz3,;
	cSize4     WITH Scale.Sz4,;
	cSize5     WITH Scale.Sz5,;
	cSize6     WITH Scale.Sz6,;
	cSize7     WITH Scale.Sz7,;
	cSize8     WITH Scale.Sz8,;
	AvlQty1    WITH IIF(EMPTY(laData[3]),OrdLine.Qty1,OrdLine.Pik1),;
	AvlQty2    WITH IIF(EMPTY(laData[3]),OrdLine.Qty2,OrdLine.Pik2),;
	AvlQty3    WITH IIF(EMPTY(laData[3]),OrdLine.Qty3,OrdLine.Pik3),;
	AvlQty4    WITH IIF(EMPTY(laData[3]),OrdLine.Qty4,OrdLine.Pik4),;
	AvlQty5    WITH IIF(EMPTY(laData[3]),OrdLine.Qty5,OrdLine.Pik5),;
	AvlQty6    WITH IIF(EMPTY(laData[3]),OrdLine.Qty6,OrdLine.Pik6),;
	AvlQty7    WITH IIF(EMPTY(laData[3]),OrdLine.Qty7,OrdLine.Pik7),;
	AvlQty8    WITH IIF(EMPTY(laData[3]),OrdLine.Qty8,OrdLine.Pik8),;
	OrdQty1    WITH OrdLine.Qty1,;
	OrdQty2    WITH OrdLine.Qty2),;
	OrdQty3    WITH OrdLine.Qty3),;
	OrdQty4    WITH OrdLine.Qty4),;
	OrdQty5    WITH OrdLine.Qty5),;
	OrdQty6    WITH OrdLine.Qty6),;
	OrdQty7    WITH OrdLine.Qty7),;
	OrdQty8    WITH OrdLine.Qty8)
*B607445,1  TMI [Start] if order is completed then get the avlqty from pack_lin file
*REPLACE Style      WITH IIF(llFrmPikLn,PikLine.Style,OrdLine.Style),;
Scale      WITH Scale.Scale,;
SzCnt      WITH Scale.Cnt,;
StyWgh     WITH Style.nStyWeight,;
OrgStyWgh  WITH Style.nStyWeight,;
nOrdLineNo WITH IIF(llFrmPikLn,PikLine.LineNo,OrdLine.LineNo),;
LPicked    WITH IIF(llFrmPikLn,PikLine.Picked,OrdLine.Picked),;
cSize1     WITH Scale.Sz1,;
cSize2     WITH Scale.Sz2,;
cSize3     WITH Scale.Sz3,;
cSize4     WITH Scale.Sz4,;
cSize5     WITH Scale.Sz5,;
cSize6     WITH Scale.Sz6,;
cSize7     WITH Scale.Sz7,;
cSize8     WITH Scale.Sz8,;
AvlQty1    WITH IIF(EMPTY(laData[3]),OrdLine.Qty1,;
IIF(llFrmPikLn,PikLine.Pik1,OrdLine.Pik1)),;
AvlQty2    WITH IIF(EMPTY(laData[3]),OrdLine.Qty2,;
IIF(llFrmPikLn,PikLine.Pik2,OrdLine.Pik2)),;
AvlQty3    WITH IIF(EMPTY(laData[3]),OrdLine.Qty3,;
IIF(llFrmPikLn,PikLine.Pik3,OrdLine.Pik3)),;
AvlQty4    WITH IIF(EMPTY(laData[3]),OrdLine.Qty4,;
IIF(llFrmPikLn,PikLine.Pik4,OrdLine.Pik4)),;
AvlQty5    WITH IIF(EMPTY(laData[3]),OrdLine.Qty5,;
IIF(llFrmPikLn,PikLine.Pik5,OrdLine.Pik5)),;
AvlQty6    WITH IIF(EMPTY(laData[3]),OrdLine.Qty6,;
IIF(llFrmPikLn,PikLine.Pik6,OrdLine.Pik6)),;
AvlQty7    WITH IIF(EMPTY(laData[3]),OrdLine.Qty7,;
IIF(llFrmPikLn,PikLine.Pik7,OrdLine.Pik7)),;
AvlQty8    WITH IIF(EMPTY(laData[3]),OrdLine.Qty8,;
IIF(llFrmPikLn,PikLine.Pik8,OrdLine.Pik8)),;
OrdQty1    WITH IIF(llFrmPikLn,PikLine.Qty1,OrdLine.Qty1),;
OrdQty2    WITH IIF(llFrmPikLn,PikLine.Qty2,OrdLine.Qty2),;
OrdQty3    WITH IIF(llFrmPikLn,PikLine.Qty3,OrdLine.Qty3),;
OrdQty4    WITH IIF(llFrmPikLn,PikLine.Qty4,OrdLine.Qty4),;
OrdQty5    WITH IIF(llFrmPikLn,PikLine.Qty5,OrdLine.Qty5),;
OrdQty6    WITH IIF(llFrmPikLn,PikLine.Qty6,OrdLine.Qty6),;
OrdQty7    WITH IIF(llFrmPikLn,PikLine.Qty7,OrdLine.Qty7),;
OrdQty8    WITH IIF(llFrmPikLn,PikLine.Qty8,OrdLine.Qty8)

REPLACE STYLE      WITH IIF(llFrmPikLn,PikLine.Style,OrdLine.Style),;
	Scale      WITH Scale.Scale,;
	SzCnt      WITH Scale.Cnt,;
	StyWgh     WITH Style.nStyWeight,;
	OrgStyWgh  WITH Style.nStyWeight,;
	nOrdLineNo WITH IIF(llFrmPikLn,PikLine.LineNo,OrdLine.LineNo),;
	LPicked    WITH IIF(llFrmPikLn,PikLine.Picked,OrdLine.Picked),;
	cSize1     WITH Scale.Sz1,;
	cSize2     WITH Scale.Sz2,;
	cSize3     WITH Scale.Sz3,;
	cSize4     WITH Scale.Sz4,;
	cSize5     WITH Scale.Sz5,;
	cSize6     WITH Scale.Sz6,;
	cSize7     WITH Scale.Sz7,;
	cSize8     WITH Scale.Sz8

PRIVATE M.PkQty1,M.PkQty2,M.PkQty3,M.PkQty4,M.PkQty5,M.PkQty6,M.PkQty7,M.PkQty8,lcSvAls
STORE 0 TO M.PkQty1,M.PkQty2,M.PkQty3,M.PkQty4,M.PkQty5,M.PkQty6,M.PkQty7,M.PkQty8
IF llCmplt
	lcSvAls = SELECT()
	=SEEK(ORDHDR.ORDER+ORDHDR.STORE,'PACK_HDR')
	=SEEK(PACK_HDR.PACK_NO,'PACK_LIN')
	SELECT PACK_LIN
	SCAN REST WHILE PACK_NO+STR(NO_CART,4)+STYLE = PACK_HDR.PACK_NO FOR PACK_LIN.NORDLINENO = ORDLINE.LINENO
		m.PkQty1 = m.PkQty1 + PACK_LIN.QTY1
		m.PkQty2 = m.PkQty2 + PACK_LIN.QTY2
		m.PkQty3 = m.PkQty3 + PACK_LIN.QTY3
		m.PkQty4 = m.PkQty4 + PACK_LIN.QTY4
		m.PkQty5 = m.PkQty5 + PACK_LIN.QTY5
		m.PkQty6 = m.PkQty6 + PACK_LIN.QTY6
		m.PkQty7 = m.PkQty7 + PACK_LIN.QTY7
		m.PkQty8 = m.PkQty8 + PACK_LIN.QTY8
	ENDSCAN
	SELECT (lcSvAls)
ENDIF
REPLACE AvlQty1    WITH IIF(EMPTY(laData[3]),IIF(llCmplt,m.PkQty1,OrdLine.Qty1),;
	IIF(llFrmPikLn,PikLine.Pik1,OrdLine.Pik1)),;
	AvlQty2    WITH IIF(EMPTY(laData[3]),IIF(llCmplt,m.PkQty2,OrdLine.Qty2),;
	IIF(llFrmPikLn,PikLine.Pik2,OrdLine.Pik2)),;
	AvlQty3    WITH IIF(EMPTY(laData[3]),IIF(llCmplt,m.PkQty3,OrdLine.Qty3),;
	IIF(llFrmPikLn,PikLine.Pik3,OrdLine.Pik3)),;
	AvlQty4    WITH IIF(EMPTY(laData[3]),IIF(llCmplt,m.PkQty4,OrdLine.Qty4),;
	IIF(llFrmPikLn,PikLine.Pik4,OrdLine.Pik4)),;
	AvlQty5    WITH IIF(EMPTY(laData[3]),IIF(llCmplt,m.PkQty5,OrdLine.Qty5),;
	IIF(llFrmPikLn,PikLine.Pik5,OrdLine.Pik5)),;
	AvlQty6    WITH IIF(EMPTY(laData[3]),IIF(llCmplt,m.PkQty6,OrdLine.Qty6),;
	IIF(llFrmPikLn,PikLine.Pik6,OrdLine.Pik6)),;
	AvlQty7    WITH IIF(EMPTY(laData[3]),IIF(llCmplt,m.PkQty7,OrdLine.Qty7),;
	IIF(llFrmPikLn,PikLine.Pik7,OrdLine.Pik7)),;
	AvlQty8    WITH IIF(EMPTY(laData[3]),IIF(llCmplt,m.PkQty8,OrdLine.Qty8),;
	IIF(llFrmPikLn,PikLine.Pik8,OrdLine.Pik8))


*B130950,1 EIH 30/01/2006 Do not allow user to pack more than existing inventory if costing method is FIFO or LIFO [Begin].
IF lcCostMeth $ 'FL' 
  =SEEK(ORDLINE.STYLE+ORDLINE.CWARECODE+ORDLINE.DYELOT,'STYDYE')
  IF (AvlQty1 > STYDYE.STK1 OR AvlQty2 > STYDYE.STK2 OR AvlQty3 > STYDYE.STK3 OR AvlQty4 > STYDYE.STK4 OR ;
     AvlQty5 > STYDYE.STK5 OR AvlQty6 > STYDYE.STK6 OR AvlQty7 > STYDYE.STK7 OR AvlQty8 > STYDYE.STK8 ) AND ( LASCRMODE[3] OR LASCRMODE[4] )
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'There are some items have not enough stock')
  ENDIF	   
  REPLACE  STK1    WITH  STYDYE.STK1 ;
   	       STK2    WITH  STYDYE.STK2 ;
	  	   STK3    WITH  STYDYE.STK3 ;
		   STK4    WITH  STYDYE.STK4 ;
		   STK5    WITH  STYDYE.STK5 ;
		   STK6    WITH  STYDYE.STK6 ;
		   STK7    WITH  STYDYE.STK7 ;
		   STK8    WITH  STYDYE.STK8
  
  REPLACE  AvlQty1    WITH  MIN(STK1,AvlQty1);
   	       AvlQty2    WITH  MIN(STK2,AvlQty2) ;
	  	   AvlQty3    WITH  MIN(STK3,AvlQty3) ;
		   AvlQty4    WITH  MIN(STK4,AvlQty4) ;
		   AvlQty5    WITH  MIN(STK5,AvlQty5) ;
		   AvlQty6    WITH  MIN(STK6,AvlQty6) ;
		   AvlQty7    WITH  MIN(STK7,AvlQty7) ;
		   AvlQty8    WITH  MIN(STK8,AvlQty8)
ENDIF

*B130950,1 EIH [End].

REPLACE OrdQty1    WITH IIF(llFrmPikLn,PikLine.Qty1,OrdLine.Qty1),;
	OrdQty2    WITH IIF(llFrmPikLn,PikLine.Qty2,OrdLine.Qty2),;
	OrdQty3    WITH IIF(llFrmPikLn,PikLine.Qty3,OrdLine.Qty3),;
	OrdQty4    WITH IIF(llFrmPikLn,PikLine.Qty4,OrdLine.Qty4),;
	OrdQty5    WITH IIF(llFrmPikLn,PikLine.Qty5,OrdLine.Qty5),;
	OrdQty6    WITH IIF(llFrmPikLn,PikLine.Qty6,OrdLine.Qty6),;
	OrdQty7    WITH IIF(llFrmPikLn,PikLine.Qty7,OrdLine.Qty7),;
	OrdQty8    WITH IIF(llFrmPikLn,PikLine.Qty8,OrdLine.Qty8)
*B607445,1  TMI [End  ]
*B802796,1 WAB - END
*B802726 (Start)
*REPLACE OQty1      WITH MAX(0,OrdLine.Qty1-Ordline.nPck1),;
OQty2      WITH MAX(0,OrdLine.Qty2-Ordline.nPck2),;
OQty3      WITH MAX(0,OrdLine.Qty3-Ordline.nPck3),;
OQty4      WITH MAX(0,OrdLine.Qty4-Ordline.nPck4),;
OQty5      WITH MAX(0,OrdLine.Qty5-Ordline.nPck5),;
OQty6      WITH MAX(0,OrdLine.Qty6-Ordline.nPck6),;
OQty7      WITH MAX(0,OrdLine.Qty7-Ordline.nPck7),;
OQty8      WITH MAX(0,OrdLine.Qty8-Ordline.nPck8),;
PQty1      WITH OrdLine.nPck1,;
PQty2      WITH OrdLine.nPck2,;
PQty3      WITH OrdLine.nPck3,;
PQty4      WITH OrdLine.nPck4,;
PQty5      WITH OrdLine.nPck5,;
PQty6      WITH OrdLine.nPck6,;
PQty7      WITH OrdLine.nPck7,;
PQty8      WITH OrdLine.nPck8
REPLACE OQty1  WITH MAX(0,AvlQty1-Ordline.nPck1),;
	OQty2  WITH MAX(0,AvlQty2-Ordline.nPck2),;
	OQty3  WITH MAX(0,AvlQty3-Ordline.nPck3),;
	OQty4  WITH MAX(0,AvlQty4-Ordline.nPck4),;
	OQty5  WITH MAX(0,AvlQty5-Ordline.nPck5),;
	OQty6  WITH MAX(0,AvlQty6-Ordline.nPck6),;
	OQty7  WITH MAX(0,AvlQty7-Ordline.nPck7),;
	OQty8  WITH MAX(0,AvlQty8-Ordline.nPck8),;
	PQty1  WITH OrdLine.nPck1,;
	PQty2  WITH OrdLine.nPck2,;
	PQty3  WITH OrdLine.nPck3,;
	PQty4  WITH OrdLine.nPck4,;
	PQty5  WITH OrdLine.nPck5,;
	PQty6  WITH OrdLine.nPck6,;
	PQty7  WITH OrdLine.nPck7,;
	PQty8  WITH OrdLine.nPck8
*B802726 (End)
REPLACE PWgh1      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty1,;
	PWgh2      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty2,;
	PWgh3      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty3,;
	PWgh4      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty4,;
	PWgh5      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty5,;
	PWgh6      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty6,;
	PWgh7      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty7,;
	PWgh8      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty8
REPLACE OrgPQty1   WITH OrdLine.nPck1,;
	OrgPQty2   WITH OrdLine.nPck2,;
	OrgPQty3   WITH OrdLine.nPck3,;
	OrgPQty4   WITH OrdLine.nPck4,;
	OrgPQty5   WITH OrdLine.nPck5,;
	OrgPQty6   WITH OrdLine.nPck6,;
	OrgPQty7   WITH OrdLine.nPck7,;
	OrgPQty8   WITH OrdLine.nPck8,;
	OrgPwgh    WITH OrdLine.nPwght
*B603121 (Start)
*B802796,1 WAB - get data from pikline in case of pik ticket is complete
*B802796,1 WAB - START
*REPLACE OrgOrd1    WITH OrdLine.Qty1,;
OrgOrd2    WITH OrdLine.Qty2,;
OrgOrd3    WITH OrdLine.Qty3,;
OrgOrd4    WITH OrdLine.Qty4,;
OrgOrd5    WITH OrdLine.Qty5,;
OrgOrd6    WITH OrdLine.Qty6,;
OrgOrd7    WITH OrdLine.Qty7,;
OrgOrd8    WITH OrdLine.Qty8
REPLACE OrgOrd1    WITH IIF(llFrmPikLn,PikLine.Qty1,OrdLine.Qty1),;
	OrgOrd2    WITH IIF(llFrmPikLn,PikLine.Qty2,OrdLine.Qty2),;
	OrgOrd3    WITH IIF(llFrmPikLn,PikLine.Qty3,OrdLine.Qty3),;
	OrgOrd4    WITH IIF(llFrmPikLn,PikLine.Qty4,OrdLine.Qty4),;
	OrgOrd5    WITH IIF(llFrmPikLn,PikLine.Qty5,OrdLine.Qty5),;
	OrgOrd6    WITH IIF(llFrmPikLn,PikLine.Qty6,OrdLine.Qty6),;
	OrgOrd7    WITH IIF(llFrmPikLn,PikLine.Qty7,OrdLine.Qty7),;
	OrgOrd8    WITH IIF(llFrmPikLn,PikLine.Qty8,OrdLine.Qty8)
*B802796,1 WAB - END
*B603121 (End)

llAnyRec = .T.
ENDSCAN

*B607445,1  TMI [Start] restore old order and relation for pack_hdr/lin files
IF llCmplt
	SET ORDER TO &lcSvOrd IN PACK_HDR
	SELECT PACK_LIN
	SET RELATION TO &lcSvRel
ENDIF
*B607445,1  TMI [End  ]

SELECT Pack_Lin
lcTag = ORDER()
SET ORDER TO PackStyle
IF !EMPTY(laData[1]) OR !EMPTY(lcPackNo)
	IF SEEK(IIF(EMPTY(laData[1]),lcPackNo,laData[1]),'Pack_Lin')
		SCAN REST WHILE pack_no = IIF(EMPTY(laData[1]),lcPackNo,laData[1])
			llStyFound = .F.
			lnRemPQty1 = Pack_Lin.Qty1
			lnRemPQty2 = Pack_Lin.Qty2
			lnRemPQty3 = Pack_Lin.Qty3
			lnRemPQty4 = Pack_Lin.Qty4
			lnRemPQty5 = Pack_Lin.Qty5
			lnRemPQty6 = Pack_Lin.Qty6
			lnRemPQty7 = Pack_Lin.Qty7
			lnRemPQty8 = Pack_Lin.Qty8

			*-- This is to avoid checking that the style has more than 1 line
			*-- in the order in case we are restoring pack data (in view mode)
			*-- which means that this check will be only we are copying from
			*-- another pack
			lcSearExp = IIF(EMPTY(lcPackNo),;
				"'O'+laData[2]+laData[5]+Pack_Lin.Style+;
				STR(Pack_Lin.nOrdLineNo,6)",;
				"'O'+laData[2]+laData[5]+Pack_Lin.Style")
			IF SEEK(&lcSearExp.,'Ordline')
				SELECT OrdLine
				lnPackLin = 0
				SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
						&lcSearExp.
					lnStyOrdLin = OrdLine.LineNo
					IF !EMPTY(lcPackNo)
						llStyFound = .F.

						LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+laData[2]+laData[5]+Pack_Lin.Style ;
							FOR LINENO > lnStyOrdLin
						IF FOUND('OrdLine')
							llStyFound = .T.
						ENDIF
					ENDIF

					llNoThing = SEEK('O'+laData[2]+laData[5]+Pack_Lin.Style+STR(lnStyOrdLin,6),'Ordline')
					llNoThing = SEEK(Pack_Lin.Style+STR(lnStyOrdLin,6),lcPckLin)
					*B802796,1 WAB - in case of pik ticket is complete the field picked in ordline is false
					*B802796,1 WAB - START
					*IF (EMPTY(laData[3]) AND !OrdLine.Picked) OR (!EMPTY(laData[3]) AND OrdLine.Picked)
					IF (EMPTY(laData[3]) AND !OrdLine.Picked) OR (!EMPTY(laData[3]) AND IIF(llFrmPikLn,!OrdLine.Picked,OrdLine.Picked))
						*B802796,1 WAB - END
						SELECT (lcCtnDtl)
						lnPackLin = IIF(EMPTY(lcPackNo),Pack_Lin.Line_No,lnPackLin+1)
						APPEND BLANK
						REPLACE STYLE      WITH Pack_Lin.Style,;
							SzCnt      WITH &lcPckLin..SzCnt,;
							cStatus    WITH IIF(EMPTY(lcPackNo),"S","A"),;
							nOrdLineNo WITH lnStyOrdLin,;
							PackLineNo WITH lnPackLin,;
							Cart_No    WITH Pack_Lin.No_Cart,;
							Qty1       WITH IIF(!EMPTY(lcPackNo) AND !EMPTY(Pack_Lin.Qty1),lfChkStyQty('1'),Pack_Lin.Qty1),;
							Qty2       WITH IIF(!EMPTY(lcPackNo) AND !EMPTY(Pack_Lin.Qty2),lfChkStyQty('2'),Pack_Lin.Qty2),;
							Qty3       WITH IIF(!EMPTY(lcPackNo) AND !EMPTY(Pack_Lin.Qty3),lfChkStyQty('3'),Pack_Lin.Qty3),;
							Qty4       WITH IIF(!EMPTY(lcPackNo) AND !EMPTY(Pack_Lin.Qty4),lfChkStyQty('4'),Pack_Lin.Qty4),;
							Qty5       WITH IIF(!EMPTY(lcPackNo) AND !EMPTY(Pack_Lin.Qty5),lfChkStyQty('5'),Pack_Lin.Qty5),;
							Qty6       WITH IIF(!EMPTY(lcPackNo) AND !EMPTY(Pack_Lin.Qty6),lfChkStyQty('6'),Pack_Lin.Qty6),;
							Qty7       WITH IIF(!EMPTY(lcPackNo) AND !EMPTY(Pack_Lin.Qty7),lfChkStyQty('7'),Pack_Lin.Qty7),;
							Qty8       WITH IIF(!EMPTY(lcPackNo) AND !EMPTY(Pack_Lin.Qty8),lfChkStyQty('8'),Pack_Lin.Qty8)

						REPLACE Size1      WITH IIF(Qty1>0,&lcPckLin..cSize1,Size1),;
							Size2      WITH IIF(Qty2>0,&lcPckLin..cSize2,Size2),;
							Size3      WITH IIF(Qty3>0,&lcPckLin..cSize3,Size3),;
							Size4      WITH IIF(Qty4>0,&lcPckLin..cSize4,Size4),;
							Size5      WITH IIF(Qty5>0,&lcPckLin..cSize5,Size5),;
							Size6      WITH IIF(Qty6>0,&lcPckLin..cSize6,Size6),;
							Size7      WITH IIF(Qty7>0,&lcPckLin..cSize7,Size7),;
							Size8      WITH IIF(Qty8>0,&lcPckLin..cSize8,Size8)
						REPLACE Br1        WITH !EMPTY(Qty1),;
							Br2        WITH !EMPTY(Qty2),;
							Br3        WITH !EMPTY(Qty3),;
							Br4        WITH !EMPTY(Qty4),;
							Br5        WITH !EMPTY(Qty5),;
							Br6        WITH !EMPTY(Qty6),;
							Br7        WITH !EMPTY(Qty7),;
							Br8        WITH !EMPTY(Qty8),;
							Weight1    WITH Qty1*(Pack_Lin.Weight/Pack_Lin.TotQty),;
							Weight2    WITH Qty2*(Pack_Lin.Weight/Pack_Lin.TotQty),;
							Weight3    WITH Qty3*(Pack_Lin.Weight/Pack_Lin.TotQty),;
							Weight4    WITH Qty4*(Pack_Lin.Weight/Pack_Lin.TotQty),;
							Weight5    WITH Qty5*(Pack_Lin.Weight/Pack_Lin.TotQty),;
							Weight6    WITH Qty6*(Pack_Lin.Weight/Pack_Lin.TotQty),;
							Weight7    WITH Qty7*(Pack_Lin.Weight/Pack_Lin.TotQty),;
							Weight8    WITH Qty8*(Pack_Lin.Weight/Pack_Lin.TotQty),;
							OrgWgh     WITH &lcPckLin..OrgStyWgh

						IF SEEK(STR(Pack_Lin.No_Cart,4),lcCtnHdr)
							SELECT (lcCtnHdr)
							REPLACE TotPcs WITH TotPcs + ;
								&lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
								&lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
								&lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
								&lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
								TotWgh WITH TotWgh + ;
								&lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
								&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
								&lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
								&lcCtnDtl..Weight7+&lcCtnDtl..Weight8
							IF !EMPTY(lcPackNo)
								lnPackQty = lnPackQty + &lcCtnDtl..Qty1 + &lcCtnDtl..Qty2 +;
									&lcCtnDtl..Qty3 + &lcCtnDtl..Qty4 +;
									&lcCtnDtl..Qty5 + &lcCtnDtl..Qty6 +;
									&lcCtnDtl..Qty7 + &lcCtnDtl..Qty8
								lnPackWgh = lnPackWgh + &lcCtnDtl..Weight1 + &lcCtnDtl..Weight2 +;
									&lcCtnDtl..Weight3 + &lcCtnDtl..Weight4 +;
									&lcCtnDtl..Weight5 + &lcCtnDtl..Weight6 +;
									&lcCtnDtl..Weight7 + &lcCtnDtl..Weight8
							ENDIF
						ELSE
							IF (&lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
									&lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
									&lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
									&lcCtnDtl..Qty7+&lcCtnDtl..Qty8) > 0

								lnMaxCtn = MAX(lnMaxCtn,Pack_Lin.No_Cart)

								INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,TotPcs,TotWgh,EMPTY);
									VALUES (Pack_Lin.No_Cart,Pack_Lin.NPltNo,;
									&lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
									&lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
									&lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
									&lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
									&lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
									&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
									&lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
									&lcCtnDtl..Weight7+&lcCtnDtl..Weight8,'N')

								*E120116,1  TMI [Start] Update Carrier Carton ID field
								REPLACE &lcCtnHdr..cCarrCtnID WITH PACK_LIN.cCarrCtnID
								*E120116,1  TMI [End  ]

								IF !EMPTY(lcPackNo)
									lnPackCtn = lnPackCtn + 1
									STORE lnPackCtn + 1 TO lnFrom,lnTo

									lnPackQty = lnPackQty + &lcCtnDtl..Qty1 + &lcCtnDtl..Qty2 +;
										&lcCtnDtl..Qty3 + &lcCtnDtl..Qty4 +;
										&lcCtnDtl..Qty5 + &lcCtnDtl..Qty6 +;
										&lcCtnDtl..Qty7 + &lcCtnDtl..Qty8
									lnPackWgh = lnPackWgh + &lcCtnDtl..Weight1 + &lcCtnDtl..Weight2 +;
										&lcCtnDtl..Weight3 + &lcCtnDtl..Weight4 +;
										&lcCtnDtl..Weight5 + &lcCtnDtl..Weight6 +;
										&lcCtnDtl..Weight7 + &lcCtnDtl..Weight8
								ENDIF
							ENDIF
						ENDIF
					ENDIF

					*E302144,1 AMH Change the custom C102789 to be standard [Start]
					*C102789,4 ASH 02/15/2003 (Begin) Retrieve Carton type from pack_lin file.
					*IF ASCAN(laEvntTrig , PADR('GETVOL',10)) <> 0
					*  =gfDoTriger('ALPLIST',PADR('GETVOL',10))
					*ENDIF
					*C102789,4 ASH 02/15/2003 (End)
					REPLACE &lcPckLin..cCrtnVlTyp WITH Pack_Lin.cCrtnVlTyp
					REPLACE &lcCtnHdr..cCrtnVlTyp WITH Pack_Lin.cCrtnVlTyp
					*E302144,1 AMH [End]

				ENDSCAN
			ENDIF
		ENDSCAN
	ENDIF
ELSE
	STORE 1 TO lnFrom,lnTo
ENDIF

SET ORDER TO lcTag IN Pack_Lin

= lfRefresh(lcWinHdr)

SELECT (lcPckLin)
GO TOP
=RLOCK(lcPckLin)
UNLOCK IN (lcPckLin)
SET RELATION TO '' INTO (lcScaFile) ADDITIVE
SET SKIP TO (lcScaFile)

IF !WEXIST(lcDtlBrTtl) AND lnActFolder = 2
	= lfDtlBrow()
ELSE
	IF lnActFolder = 2
		= lfwDtlBrow()
	ENDIF
ENDIF

SELECT(lcCtnHdr)
= RLOCK(lcCtnHdr)
UNLOCK IN (lcCtnHdr)
GO TOP

SELECT(lcCtnDtl)
UNLOCK IN (lcCtnDtl)

IF !WEXIST(lcCtHdBrTt) AND lnActFolder = 3
	= lfCtnHdrBr()
ELSE
	IF lnActFolder = 3
		= lfwCtnHdrBr()
	ENDIF
ENDIF

IF !WEXIST(lcCtDtBrTt) AND lnActFolder = 3
	= lfCtnDtlBr()
ELSE
	IF lnActFolder = 3
		= lfwCtnDtlBr()
	ENDIF
ENDIF

IF laScrMode[4] AND llNew
	WAIT CLEAR
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfChkStyQty
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To compare between order qty and packed qty
*!             in copying from another pack
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfChkStyQty()
*!*************************************************************

FUNCTION lfChkStyQty

PARAMETERS lcSizeCode

PRIVATE lcSizeCode,lnReturn,lnCurAlias,lnChoice

lnCurAlias = SELECT(0)

lnChoice = 0
lnReturn = 0

IF (EMPTY(laData[3]) AND !OrdLine.Picked) OR ;
		(!EMPTY(laData[3]) AND OrdLine.Picked AND EVAL("OrdLine.Pik"+lcSizeCode)>0)

	lcQtyFld = 'Pack_Lin.Qty'+ lcSizeCode
	lcOrdQtyFld = lcPckLin + '.OrdQty'  + lcSizeCode
	lcPQtyFld   = lcPckLin + '.PQty'    + lcSizeCode
	lcOQtyFld   = lcPckLin + '.OQty'    + lcSizeCode
	lcCtnQtyFld = lcPckLin + '.CtnQty'  + lcSizeCode
	lcSizeFld   = lcPckLin + '.cSize'   + lcSizeCode
	lcPWghFld   = lcPckLin + '.PWgh'    + lcSizeCode
	lcRemVar    = "lnRemPQty" + lcSizeCode

	IF !llStyFound AND (&lcOrdQtyFld < &lcPQtyFld + &lcRemVar)
		*-- Packed quantity for Style/Size x/x exceeds ordered quantity.;
		DO you want TO MODIFY the orderd quantity?
	*-- <Yes>,<No>
	lnChoice = gfModalGen("QRM44034B00006","Dialog",Pack_Lin.Style + '/' + EVAL(lcSizeFld))
ENDIF

lnReturn = IIF(llStyFound,&lcOQtyFld,IIF(lnChoice<>2,&lcRemVar,&lcOQtyFld))

SELECT (lcPckLin)
*B607157,1 HBG 04/13/2003 Fix bug of Problem in copying packing list from existing one.
*B607157,1                "O.Qty" and "P.Qty" column doesn't reflect the correct qty [Begin]
*REPLACE &lcPQtyFld   WITH IIF(llStyFound,&lcOQtyFld,IIF(lnChoice<>2,&lcRemVar,&lcOrdQtyFld)),;
*        &lcOQtyFld   WITH IIF(&lcOrdQtyFld-&lcPQtyFld<0,0,&lcOrdQtyFld-&lcPQtyFld),;
*        &lcPWghFld   WITH (Pack_lin.Weight/Pack_Lin.TotQty)* &lcPQtyFld,;
*        &lcOrdQtyFld WITH IIF(llStyFound,&lcOrdQtyFld,IIF(lnChoice<>1,&lcOrdQtyFld,&lcRemVar))
REPLACE &lcPQtyFld   WITH &lcPQtyFld + IIF(llStyFound,&lcOQtyFld,IIF(lnChoice<>2,&lcRemVar,&lcOrdQtyFld)),;
	&lcOQtyFld   WITH IIF(&lcOrdQtyFld-&lcPQtyFld<0,0,&lcOrdQtyFld-&lcPQtyFld),;
	&lcPWghFld   WITH (Pack_lin.Weight/Pack_Lin.TotQty)* &lcPQtyFld,;
	&lcOrdQtyFld WITH IIF(llStyFound,&lcOrdQtyFld,IIF(lnChoice<>1,&lcOrdQtyFld,&lcRemVar))
*B607157,1 [End]


&lcRemVar = &lcRemVar - lnReturn

ENDIF

SELECT(lnCurAlias)

RETURN lnReturn

*!*************************************************************
*! Name      : lfvData_13
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat lnCtnTyp field .
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfUpdVars
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_13()
*!*************************************************************

FUNCTION lfvData_13
*E301373,1 NAD 02/14/2000 (Start)  Set the value to Standered,Store instead of N/A
*laData[13] = IIF(lnCtnTyp=2,.T.,.F.)
laData[13] = (lnCtnTyp=1)
*E301373,1 NAD  (End)
llNoThing = lfUpdVars()

*!*************************************************************
*! Name      : lfvData_14
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat lnDrctTo field .
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfUpdVars
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_14()
*!*************************************************************

FUNCTION lfvData_14

*B804306,1 HBG 04/03/2002 Update field of To Store or consalidatore in BOL_HDR. [Begin]
*laData[14] = IIF(lnDrctTo=3,"C","S")
laData[14] = IIF(lnDrctTo=2,"C","S")
IF SEEK(lcBol,'BOL_HDR')
	lnPrvAlis = SELECT(0)
	SELECT BOL_HDR
	REPLACE BOL_HDR.cToStorCn WITH laData[14]
	SELECT (lnPrvAlis)
ENDIF
*B804306,1 [End]
llNoThing = lfUpdVars()

*!*************************************************************
*! Name      : lfvShpVia
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat lnShpVia field .
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfUpdVars
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvShpVia()
*!*************************************************************

FUNCTION lfvShpVia

*B803177,1 HBG 06/27/2000 Check if shipvia was changed or not [Begin]
PRIVATE llGetShipV
llGetShipV = (laData[7] <> laShpVia[lnShpVia,2])
*B803177,1 HBG 06/27/2000 Check if shipvia was changed or not [End  ]
laData[7] = laShpVia[lnShpVia,2]

*B803177,1 HBG 06/27/2000 Get the BOL# with the changes of each ShipVia
=llGetShipV AND lfNewBOL()
SHOW GET lcBOL
*B803177,1 HBG 06/27/2000  Get the BOL# with the changes of each ShipVia

llNoThing = lfUpdVars()

*!*************************************************************
*! Name      : lfvCtnSty
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat lcCtnSty field .
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfStyBrow,lfChkSty
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtnSty()
*!*************************************************************

FUNCTION lfvCtnSty

PRIVATE lnCurAlias,llNoThing,lnOrdLine,lnWgh,lcSzCode,lnK,lcFld,;
	lcPQtyFld,lcPWghFld,lcOQtyFld,llAlowAdd
STORE SPACE(0) TO lcSzCode
STORE 0 TO lnOrdLine,lnWgh,lnK
llNoThing = .F.
llAlowAdd = .F.

lnCurAlias = SELECT(0)

*-- using lcpcklin file with another alias refers to there is a browse
*-- from this file and browsing from the same file on other window terminates
*-- the first one

USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS PackLines ORDER (lcPckLin)

USE (gcWorkDir+lcScaFile) IN 0 AGAIN ALIAS StyQty ORDER (lcScaFile)

SELECT PackLines
SET RELATION TO '' INTO StyQty
SET SKIP TO StyQty

IF llBrowse OR (LASTKEY() = 13 AND !EMPTY(lcCtnSty) AND !SEEK(lcCtnSty,'PackLines'))
	llBrowse = .T.
ELSE
	IF LASTKEY() = 13 AND !EMPTY(lcCtnSty)
		lcCtnSty  = PackLines.Style
	ENDIF
ENDIF

IF llBrowse
	llBrowse  = .F.
	llNoThing = lfStyBrow()
	lcCtnSty  = IIF(llNoThing,PackLines.Style,SPACE(19))
	lcStySz   = IIF(llNoThing,EVAL('PackLines.cSize'+StyQty.SzCode),SPACE(5))
	lnStyQty  = IIF(llNoThing,EVAL('PackLines.OQty'+StyQty.SzCode),0)
	lnOrdLine = IIF(llNoThing,PackLines.nOrdLineNo,0)
	lnWgh     = IIF(llNoThing,EVAL('PackLines.OQty'+StyQty.SzCode)*PackLines.StyWgh,0)

	lnSzOrd   = VAL(StyQty.SzCode)
	lcSizeFld = 'Size'+StyQty.SzCode
	lcQtyFld  = 'Qty'+StyQty.SzCode
	lcWghFld  = 'Weight'+StyQty.SzCode
	lcBrFld   = 'Br'+StyQty.SzCode
ENDIF

IF !EMPTY(lcCtnSty)
	SELECT (lcCtnDtl)
	IF EMPTY(lcStySz)
		REPLACE STYLE WITH lcCtnSty
	ELSE
		*-- if the style with order record No. is not found in the carton
		IF !SEEK(STR(&lcCtnHdr..Cart_No,4)+lcCtnSty+STR(lnOrdLine,6),lcCtnDtl)
			llAlowAdd = .T.
			IF SEEK(lcCtnSty+STR(lnOrdLine,6),'PackLines') AND;
					PackLines.LPicked AND EMPTY(laData[3])
				*-- This order line has been picked, can not be selected.
				*-- OK
				= gfModalGen("INM44041B00000","Dialog")
				lcCtnSty = SPACE(19)
				lcStySz  = SPACE(5)
				SHOW GET lcCtnSty
				SHOW GET lcStySz
			ELSE
				IF SEEK(STR(&lcCtnHdr..Cart_No,4)+SPACE(19),lcCtnDtl)
					REPLACE STYLE      WITH lcCtnSty ,;
						Br1        WITH .F.            ,;
						&lcSizeFld WITH lcStySz ,;
						&lcQtyFld  WITH lnStyQty,;
						&lcWghFld  WITH lnWgh,;
						nOrdLineNo WITH lnOrdLine,;
						&lcBrFld   WITH .T.,;
						SzCnt      WITH PackLines.SzCnt,;
						OrgWgh     WITH PackLines.StyWgh,;
						cStatus    WITH "A"
					SKIP lnSzOrd - 1 IN (lcCtnDtl)

					lnPackWgh = lnPackWgh + EVAL(lcCtnDtl+'.'+lcWghFld)
					lnPackQty = lnPackQty + EVAL(lcCtnDtl+'.'+lcQtyFld)
					SELECT(lcCtnHdr)
					*B804310,1 HBG 06/09/2001 Update 'Empty' field in 'lcCtnHdr' because it is used to
					*B804310,1                get the right number of cartons in this packing list [Begin]
					*REPLACE TotPcs WITH TotPcs + EVAL(lcCtnDtl+'.'+lcQtyFld),;
					TotWgh WITH TotWgh + EVAL(lcCtnDtl+'.'+lcWghFld)
				REPLACE TotPcs WITH TotPcs + EVAL(lcCtnDtl+'.'+lcQtyFld),;
					TotWgh WITH TotWgh + EVAL(lcCtnDtl+'.'+lcWghFld),;
					EMPTY  WITH IIF(TotPcs>0,'N','Y')
				*B804310,1 [End]
				= RLOCK(lcCtnHdr)
				UNLOCK IN (lcCtnHdr)
				SELECT(lcCtnDtl)
			ENDIF
		ENDIF
	ELSE

		*-- if the style with order record No. is found in the carton
		*-- we should determine if the user has select or entered
		*-- an existing size or size hasnot added to the carton yet.

		*-- Determining if size has been added (Start)
		*-- this by looping for the 8 size fields in carton detail file
		*-- and comparing the content of each size field with the selected
		*-- or entered size which is represented by the variable "lcStySz"
		lnK = 0
		FOR lnK = 1 TO 8
			*-- this means that the size has been added before
			IF lcStySz = EVAL('Size'+STR(lnK,1))
				EXIT
			ENDIF
		ENDFOR
		*-- Determining if size has been added (End)


		*-- IF lnK>8 means that the size hasnot been added yet
		*-- and we can added now
		IF lnK>8
			lcFld = 'Br'+STR(lnSzOrd,1)
			REPLACE &lcSizeFld WITH lcStySz,;
				&lcQtyFld  WITH lnStyQty,;
				&lcWghFld  WITH lnWgh,;
				nOrdLineNo WITH lnOrdLine,;
				&lcFld     WITH .T.,;
				OrgWgh     WITH PackLines.StyWgh,;
				cStatus    WITH "A"
			SKIP lnSzOrd - 1 IN (lcCtnDtl)
			lnPackWgh = lnPackWgh + EVAL(lcCtnDtl+'.'+lcWghFld)
			lnPackQty = lnPackQty + EVAL(lcCtnDtl+'.'+lcQtyFld)

			SELECT(lcCtnHdr)
			REPLACE TotPcs WITH TotPcs + EVAL(lcCtnDtl+'.'+lcQtyFld),;
				TotWgh WITH TotWgh + EVAL(lcCtnDtl+'.'+lcWghFld)
			= RLOCK(lcCtnHdr)
			UNLOCK IN (lcCtnHdr)
			SELECT(lcCtnDtl)
		ELSE
			SKIP lnK - 1 IN (lcCtnDtl)
		ENDIF
		lnCtnRec = RECNO(lcCtnDtl)
		lnSzRec  = RECNO(lcCartonSz)
		IF SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl) AND Br1 AND EMPTY(Size1)
			REPLACE STYLE WITH SPACE(19),;
				Br1   WITH .F.
			GOTO lnCtnRec IN (lcCtnDtl)
			SKIP lnSzRec - 1 IN (lcCtnDtl)
		ENDIF
	ENDIF

	IF SEEK(lcCtnSty+STR(lnOrdLine,6),lcPckLin)
		SKIP lnSzOrd - 1 IN (lcPckLin)

		*-- IF lnK>8 means that the size hasnot been added yet
		*-- and we added to carton detail file, so we can update
		*-- pack lines file
		IF lnK>8 OR llAlowAdd
			SELECT(lcPckLin)
			lcPQtyFld = 'PQty'+STR(lnSzOrd,1)
			lcPWghFld = 'PWgh'+STR(lnSzOrd,1)
			lcOQtyFld = 'OQty'+STR(lnSzOrd,1)
			REPLACE &lcPQtyFld WITH &lcPQtyFld+lnStyQty,;
				&lcPWghFld WITH &lcPWghFld+lnWgh   ,;
				&lcOQtyFld WITH EVAL('OrdQty'+STR(lnSzOrd,1)) - EVAL('PQty'+STR(lnSzOrd,1))
			= RLOCK(lcPckLin)
			UNLOCK IN (lcPckLin)
		ENDIF
	ENDIF
ENDIF
ENDIF

= RLOCK(lcCtnDtl)
UNLOCK IN (lcCtnDtl)

=lfwCtnDtlBr()

USE IN PackLines
USE IN StyQty

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfStyBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse order styles/sizes.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : AriaBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfStyBrow()
*!*************************************************************

FUNCTION lfStyBrow

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "Style,cSize,nOrdLineNo,OQty,StyWgh"
lcBrFields  = [StyCode=Style:H='Style',]+;
	[SzCode =EVAL('cSize'+StyQty.SzCode):H='Size',]+;
	[SzWgh  =StyWgh     :H='Wgh\Unit',]+;
	[Open   =EVAL('OQty'+StyQty.SzCode):H='Open Quantity']
lcFile_Ttl  = 'Style Open Quantities'

*aannew[begin]
*lcForExp = "FOR IIF(llShwOpn,EVAL('OQty'+StyQty.SzCode)>0 AND EVAL('AvlQty'+StyQty.SzCode)>0 AND StyQty.SzCode <= STR(PackLines.SzCnt,1),EVAL('AvlQty'+StyQty.SzCode)>0 AND StyQty.SzCode <= STR(PackLines.SzCnt,1))"
lcForExp = "FOR IIF(llShwOpn,EVAL('OQty'+StyQty.SzCode)>0 AND EVAL('AvlQty'+StyQty.SzCode)>0 AND StyQty.SzCode <= STR(PackLines.SzCnt,1),EVAL('AvlQty'+StyQty.SzCode)>0 AND StyQty.SzCode <= STR(PackLines.SzCnt,1))"
*aannew[end]

SELECT PackLines

DECLARE laTemp[1]

llReturn  = gfBrows(lcForExp,'Style','laTemp')

SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfvCtnSize
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat lcCtnSty field .
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfSizeBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtnSize()
*!*************************************************************

FUNCTION lfvCtnSize
PRIVATE lnCurAlias,llNoThing,lnOrdLine,lcSzCode,llSzUpdate,;
	lcPQtyFld,lcPWghFld,lcOQtyFld,lnWgh

llSzUpdate = .F.

lnCurAlias = SELECT(0)

lnWgh     = 0
lnOrdLine = 0
lnSzOrd   = 0
lcSizeFld = ''
lcQtyFld  = ''
lcWghFld  = ''
lcBrFld   = ''

*-- using lcpcklin file with another alias refers to there is a browse
*-- from this file and browsing from the same file on other window terminates
*-- the first one

IF !USED('PackLines')
	USE (gcWorkDir+lcPckLin) IN 0 AGAIN ALIAS PackLines ORDER (lcPckLin)
ENDIF

IF !USED('StyQty')
	USE (gcWorkDir+lcScaFile) IN 0 AGAIN ALIAS StyQty ORDER (lcScaFile)
ENDIF

SELECT PackLines
SET RELATION TO '' INTO StyQty
SET SKIP TO StyQty

SELECT(lcCtnDtl)

IF llBrowse OR ;
		(LASTKEY() = 13 AND !EMPTY(lcStySz) AND SEEK(lcCtnSty,'PackLines') AND ;
		!INLIST(lcStySz,PackLines.cSize1,PackLines.cSize2,;
		PackLines.cSize3,PackLines.cSize4,;
		PackLines.cSize5,PackLines.cSize6,;
		PackLines.cSize7,PackLines.cSize8)) OR;
		(LASTKEY() = 13 AND !EMPTY(lcStySz) AND EMPTY(lcCtnSty)) OR lfChkSty()
	llBrowse = .T.
ENDIF

IF llBrowse
	llBrowse  = .F.
	llNoThing = lfSizeBrow()
	lcCtnSty  = IIF(llNoThing,PackLines.Style,SPACE(19))
	lcStySz   = IIF(llNoThing,EVAL('PackLines.cSize'+StyQty.SzCode),SPACE(5))
	lnStyQty  = IIF(llNoThing,EVAL('PackLines.OQty'+StyQty.SzCode),0)
	lnOrdLine = IIF(llNoThing,PackLines.nOrdLineNo,0)
	lnWgh     = IIF(llNoThing,EVAL('PackLines.OQty'+StyQty.SzCode)*PackLines.StyWgh,0)

	lnSzOrd   = VAL(StyQty.SzCode)
	lcSizeFld = 'Size'+StyQty.SzCode
	lcQtyFld  = 'Qty'+StyQty.SzCode
	lcWghFld  = 'Weight'+StyQty.SzCode
	lcBrFld   = 'Br'+StyQty.SzCode

	= lfChkSty()
ENDIF

IF llSzUpdate
	IF SEEK(lcCtnSty+STR(lnOrdLine,6),'PackLines') AND;
			&lcPckLin..LPicked AND EMPTY(laData[3])
		*-- This order line has been picked, can not be selected.
		*-- OK
		= gfModalGen("INM44041B00000","Dialog")
		lcCtnSty = SPACE(19)
		lcStySz  = SPACE(5)
		SHOW GET lcCtnSty
		SHOW GET lcStySz
	ELSE
		REPLACE STYLE      WITH lcCtnSty,;
			&lcSizeFld WITH lcStySz,;
			&lcQtyFld  WITH lnStyQty,;
			&lcWghFld  WITH lnWgh,;
			nOrdLineNo WITH lnOrdLine,;
			&lcBrFld   WITH .T.,;
			SzCnt      WITH PackLines.SzCnt,;
			OrgWgh     WITH PackLines.StyWgh,;
			cStatus    WITH "A"

		lnPackWgh = lnPackWgh + EVAL(lcCtnDtl+'.'+lcWghFld)
		lnPackQty = lnPackQty + EVAL(lcCtnDtl+'.'+lcQtyFld)
		SELECT(lcCtnHdr)
		REPLACE TotPcs WITH TotPcs + EVAL(lcCtnDtl+'.'+lcQtyFld),;
			TotWgh WITH TotWgh + EVAL(lcCtnDtl+'.'+lcWghFld)
		= RLOCK(lcCtnHdr)
		UNLOCK IN (lcCtnHdr)
		SELECT(lcCtnDtl)

		IF SEEK(lcCtnSty+STR(lnOrdLine,6),lcPckLin)
			SKIP lnSzOrd - 1 IN (lcPckLin)
			SELECT(lcPckLin)
			lcPQtyFld = 'PQty'+STR(lnSzOrd,1)
			lcPWghFld = 'PWgh'+STR(lnSzOrd,1)
			lcOQtyFld = 'OQty'+STR(lnSzOrd,1)
			REPLACE &lcPQtyFld WITH &lcPQtyFld+lnStyQty,;
				&lcPWghFld WITH &lcPWghFld+lnWgh,;
				&lcOQtyFld WITH EVAL('OrdQty'+STR(lnSzOrd,1)) - EVAL('PQty'+STR(lnSzOrd,1))
			= RLOCK(lcPckLin)
			UNLOCK IN (lcPckLin)
		ENDIF

		SELECT(lcCtnDtl)

	ENDIF
ENDIF

= lfwCtnDtlBr()

USE IN PackLines

USE IN StyQty

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfChkSty
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Check the existence of the style/size.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfChkSty()
*!*************************************************************

FUNCTION lfChkSty

IF !EMPTY(lcStySz)
	IF !EMPTY(&lcCtnDtl..Style)
		SELECT(lcCtnDtl)
		LOCATE REST FOR STR(&lcCtnDtl..Cart_No,4)+&lcCtnDtl..Style== ;
			STR(&lcCtnHdr..Cart_No,4)+lcCtnSty AND ;
			&lcCtnDtl..nOrdLineNo<>0
		IF FOUND()
			lcSizes = ";"+&lcCtnDtl..Size1+";"+&lcCtnDtl..Size2+ ;
				";"+&lcCtnDtl..Size3+";"+&lcCtnDtl..Size4+ ;
				";"+&lcCtnDtl..Size5+";"+&lcCtnDtl..Size6+ ;
				";"+&lcCtnDtl..Size7+";"+&lcCtnDtl..Size8

			lnPos   = AT(lcStySz,lcSizes)
			lcSizes = SUBSTR(lcSizes,1,lnPos-1)
			lnSzOrd = OCCURS(';',lcSizes)

			IF lnPos <> 0
				SKIP 8 IN PackLines
				IF PackLines.Style == lcCtnSty
					llBrowse = .T.
				ELSE
					=SEEK(STR(&lcCtnHdr..Cart_No,4)+lcCtnSty,lcCtnDtl)
					REPLACE Br1 WITH .F.
					LOCATE REST FOR STR(&lcCtnDtl..Cart_No,4)+&lcCtnDtl..Style== ;
						STR(&lcCtnHdr..Cart_No,4)+lcCtnSty AND ;
						&lcCtnDtl..nOrdLineNo<>0
					IF FOUND()
						SKIP lnSzOrd-1 IN (lcCtnDtl)
					ENDIF
				ENDIF
				SKIP -8 IN PackLines
			ELSE
				IF lnOrdLine = 0
					lcSizes = ";"+PackLines.cSize1+";"+PackLines.cSize2+ ;
						";"+PackLines.cSize3+";"+PackLines.cSize4+ ;
						";"+PackLines.cSize5+";"+PackLines.cSize6+ ;
						";"+PackLines.cSize7+";"+PackLines.cSize8

					lnOrdLine = PackLines.nOrdLineNo

					lnPos   = AT(lcStySz,lcSizes)
					lcSizes = SUBSTR(lcSizes,1,lnPos-1)
					lnSzOrd = OCCURS(';',lcSizes)

					lcSizeFld = 'Size'  +STR(lnSzOrd,1)
					lcQtyFld  = 'Qty'   +STR(lnSzOrd,1)
					lcWghFld  = 'Weight'+STR(lnSzOrd,1)
					lcBrFld   = 'Br'    +STR(lnSzOrd,1)
				ENDIF

				=SEEK(STR(&lcCtnHdr..Cart_No,4)+lcCtnSty,lcCtnDtl)
				REPLACE Br1 WITH .F.
				LOCATE REST FOR STR(&lcCtnDtl..Cart_No,4)+&lcCtnDtl..Style== ;
					STR(&lcCtnHdr..Cart_No,4)+lcCtnSty AND ;
					&lcCtnDtl..nOrdLineNo<>0
				IF FOUND()
					SKIP lnSzOrd-1 IN (lcCtnDtl)
				ENDIF

				llSzUpdate = .T.

			ENDIF
		ELSE

			lcSizes = ";"+PackLines.cSize1+";"+PackLines.cSize2+ ;
				";"+PackLines.cSize3+";"+PackLines.cSize4+ ;
				";"+PackLines.cSize5+";"+PackLines.cSize6+ ;
				";"+PackLines.cSize7+";"+PackLines.cSize8

			lnOrdLine = PackLines.nOrdLineNo

			lnPos   = AT(lcStySz,lcSizes)
			lcSizes = SUBSTR(lcSizes,1,lnPos-1)
			lnSzOrd = OCCURS(';',lcSizes)

			lcSizeFld = 'Size'  +STR(lnSzOrd,1)
			lcQtyFld  = 'Qty'   +STR(lnSzOrd,1)
			lcWghFld  = 'Weight'+STR(lnSzOrd,1)
			lcBrFld   = 'Br'    +STR(lnSzOrd,1)

			=SEEK(STR(&lcCtnHdr..Cart_No,4)+lcCtnSty,lcCtnDtl)
			REPLACE Br1 WITH .F.
			SKIP lnSzOrd-1 IN (lcCtnDtl)

			llSzUpdate = .T.

		ENDIF
	ELSE
		IF !SEEK(STR(&lcCtnHdr..Cart_No,4)+lcCtnSty+STR(lnOrdLine,6),lcCtnDtl)
			=SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl)
			REPLACE Br1 WITH .F.
			SKIP lnSzOrd-1 IN (lcCtnDtl)
			llSzUpdate = .T.
		ELSE
			lcSizes = ";"+&lcCtnDtl..Size1+";"+&lcCtnDtl..Size2+ ;
				";"+&lcCtnDtl..Size3+";"+&lcCtnDtl..Size4+ ;
				";"+&lcCtnDtl..Size5+";"+&lcCtnDtl..Size6+ ;
				";"+&lcCtnDtl..Size7+";"+&lcCtnDtl..Size8

			lnPos   = AT(lcStySz,lcSizes)
			lcSizes = SUBSTR(lcSizes,1,lnPos-1)
			lnSzOrd = IIF(OCCURS(';',lcSizes)=0,lnSzOrd,OCCURS(';',lcSizes))

			=SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl)
			REPLACE Br1 WITH .F.
			=SEEK(STR(&lcCtnHdr..Cart_No,4)+lcCtnSty+STR(lnOrdLine,6),lcCtnDtl)
			SKIP lnSzOrd-1 IN (lcCtnDtl)

			IF lnPos = 0
				llSzUpdate = .T.
			ENDIF
		ENDIF
	ENDIF
ENDIF
RETURN llBrowse

*!*************************************************************
*! Name      : lfSizeBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse order styles/sizes.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : AriaBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfSizeBrow()
*!*************************************************************

FUNCTION lfSizeBrow

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "Style,cSize,nOrdLineNo,OQty,StyWgh"
lcBrFields  = [StyCode=Style:H='Style',]+;
	[SzCode =EVAL('cSize'+StyQty.SzCode):H='Size',]+;
	[SzWgh  =StyWgh     :H='Wgh\Unit',]+;
	[Open   =EVAL('OQty'+StyQty.SzCode):H='Open Quantity']
lcFile_Ttl  = 'Style Open Quantities'

IF EMPTY(lcCtnSty)
	lcForExp = "FOR IIF(llShwOpn,EVAL('OQty'+StyQty.SzCode)>0 AND EVAL('AvlQty'+StyQty.SzCode)>0 AND StyQty.SzCode <= STR(PackLines.SzCnt,1),EVAL('AvlQty'+StyQty.SzCode)>0 AND StyQty.SzCode <= STR(PackLines.SzCnt,1))"
ELSE
	lcForExp = "FOR IIF(llShwOpn,Style=lcCtnSty AND EVAL('OQty'+StyQty.SzCode)>0 AND EVAL('AvlQty'+StyQty.SzCode)>0 AND StyQty.SzCode <= STR(PackLines.SzCnt,1),Style=lcCtnSty AND EVAL('AvlQty'+StyQty.SzCode)>0 AND StyQty.SzCode <= STR(PackLines.SzCnt,1))"
ENDIF

SELECT PackLines

DECLARE laTemp[1]

llReturn  = gfBrows(lcForExp,'Style','laTemp')


SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfvStyCtnQty
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate Style quantity in carton
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStyCtnQty()
*!*************************************************************

FUNCTION lfvStyCtnQty

PRIVATE lnCurAlias,lnUnitWgh,lnContinue,lcFld

STORE 0 TO lnUnitWgh,lnContinue

*B039660,1 NNA 04/16/2006 (Begin) Check if using bin location or not to prevent user form changing the Pack Qty.
IF ASCAN(laEvntTrig , PADR('CHNGQTY',10)) <> 0 AND !gfDoTriger('ALPLIST',PADR('CHNGQTY',10)) 
  RETURN 
ENDIF  
*B039660,1 NNA (End)

IF LASTKEY() = 13 AND !(lnStyQty == lcOldVal)
	lnCurAlias = SELECT(0)

	lnUnitWgh = IIF(EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode)>0,;
		EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode)/;
		EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode),0)

	IF SEEK(&lcCtnDtl..Style+STR(&lcCtnDtl..nOrdLineNo,6),lcPckLin)
	    *B130950,1 EIH 30/01/2006 Do not allow user to pack more than existing inventory if costing method is FIFO or LIFO [Begin].
    	IF lcCostMeth $ 'FL'
    	  lnAlias = SELECT(0)
    	  select (lcPckLin)
	      IF lnStyQty > EVAL('STK'+&lcScaFile..SzCode)
        	=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'There is not enough stock')
    	    lnStyQty = EVAL('STK'+&lcScaFile..SzCode)
    	    SHOW GET lnStyQty
	      ENDIF
	      SELECT(lnAlias)
    	ENDIF
	    *B130950,1 EIH [End].
	       
		SKIP VAL(&lcCartonSz..SzCode) - 1 IN (lcPckLin)
		IF EVAL(lcPcklin+'.OrdQty'+&lcScaFile..SzCode) < ;
				EVAL(lcPckLin+'.PQty'+&lcScaFile..SzCode) - ;
				EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode) + lnStyQty
			*-- Packed quantity for Style/Size x/x exceeds ordered quantity.;
			DO you want TO MODIFY the orderd quantity?
		*-- <Yes>,<No>
		lnContinue = gfModalGen("QRM44034B00006","Dialog",&lcPckLin..Style + '/' + EVAL(lcPckLin+'.cSize'+&lcScaFile..SzCode))

		*B603121 (Start)
		*IF lnContinue = 1
		*  SELECT (lcPckLin)
		*  lcFld = 'OrdQty' + &lcScaFile..SzCode
		*  REPLACE &lcFld WITH EVAL(lcPckLin+'.PQty'+&lcScaFile..SzCode) - ;
		*                      EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode) + lnStyQty
		*ENDIF
	ENDIF

	IF lnContinue <> 2

		SELECT(lcPckLin)
		lcOQtyFld = 'OQty' + &lcScaFile..SzCode
		lcPQtyFld = 'PQty' + &lcScaFile..SzCode
		lcPWghFld = 'PWgh' + &lcScaFile..SzCode

		*B603121 (Start)
		lcFld = 'OrdQty' + &lcScaFile..SzCode
		PRIVATE lcOrgOrd
		lcOrgOrd = 'OrgOrd' + &lcScaFile..SzCode
		REPLACE &lcFld WITH MAX(EVAL(lcPckLin+'.PQty'+&lcScaFile..SzCode) - ;
			EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode) + ;
			lnStyQty ,;
			&lcOrgOrd)
		*B603121 (End)

		*B606983,1 AMH Fix bug of wronge open quantity [Start]
		*REPLACE &lcOQtyFld WITH MAX(EVAL('OQty'+&lcScaFile..SzCode) + EVAL('PQty'+&lcScaFile..SzCode) - lnStyQty,0),;
		&lcPQtyFld WITH EVAL('PQty'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Qty'   +&lcCartonSz..SzCode) + lnStyQty,;
		&lcPWghFld WITH EVAL('PWgh'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode) + lnStyQty*lnUnitWgh
	REPLACE &lcOQtyFld WITH MAX(EVAL('OQty'+&lcScaFile..SzCode) + EVAL(lcCtnDtl+'.Qty'   +&lcCartonSz..SzCode) - lnStyQty,0),;
		&lcPQtyFld WITH EVAL('PQty'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Qty'   +&lcCartonSz..SzCode) + lnStyQty,;
		&lcPWghFld WITH EVAL('PWgh'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode) + lnStyQty*lnUnitWgh
	*B606983,1 AMH [End]

	= RLOCK(lcPckLin)
	UNLOCK IN (lcPckLin)
ENDIF
ENDIF

IF SEEK(STR(&lcCtnDtl..Cart_No,4),lcCtnHdr) AND lnContinue <> 2
	lnPackWgh = lnPackWgh - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode) + lnStyQty*lnUnitWgh
	lnPackQty = lnPackQty - EVAL(lcCtnDtl+'.Qty'   +&lcCartonSz..SzCode) + lnStyQty

	*B604201 Hassan 10/04/2001 Total Ctn No Does not Changed W.R.T Ctn.s Edit  [Begin]
	SELECT(lcCtnHdr)
	*    REPLACE TotPcs WITH TotPcs - EVAL(lcCtnDtl+'.Qty'   +&lcCartonSz..SzCode) + lnStyQty,;
	TotWgh WITH TotWgh - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode) + lnStyQty*lnUnitWgh
REPLACE TotPcs WITH TotPcs - EVAL(lcCtnDtl+'.Qty'   +&lcCartonSz..SzCode) + lnStyQty,;
	EMPTY  WITH IIF(TotPcs > 0 , 'N' , 'Y'),;
	TotWgh WITH TotWgh - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode) + lnStyQty*lnUnitWgh


*B604201  Hassan 10/04/2001 Total Ctn No Does not Changed W.R.T Ctn.s Edit [End]
= RLOCK(lcCtnHdr)
UNLOCK IN (lcCtnHdr)
ENDIF

IF lnContinue <> 2
	SELECT(lcCtnDtl)
	lcQtyFld = 'Qty'   +&lcCartonSz..SzCode
	lcWghFld = 'Weight'+&lcCartonSz..SzCode
	REPLACE &lcQtyFld  WITH lnStyQty,;
		&lcWghFld  WITH EVAL('Qty'+&lcCartonSz..SzCode)*lnUnitWgh,;
		cStatus WITH 'M'
	= RLOCK(lcCtnDtl)
	UNLOCK IN (lcCtnDtl)
ENDIF

= lfwCtnDtlBr()

SELECT(lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lfvCtDtWgh
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate Style weight in carton
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtDtWgh()
*!*************************************************************

FUNCTION lfvCtDtWgh

PRIVATE lnCurAlias,lnCart,lcStyle,lnOrdLin,lnUnitWgh
STORE 0 TO lnCart,lnOrdLin,lnUnitWgh
STORE SPACE(0) TO lcStyle

IF LASTKEY() = 13 AND !(lnStyWgh == lcOldVal)
	lnCurAlias = SELECT(0)

	lnUnitWgh = lnStyWgh/EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode)

	lnCart    = &lcCtnDtl..Cart_No
	lcStyle   = &lcCtnDtl..Style
	lnOrdLin  = &lcCtnDtl..nOrdLineNo

	lnStyRec = RECNO(lcPckLin)
	lnSzeRec = RECNO(lcScaFile)

	IF SEEK(lcStyle,lcPckLin)
		SELECT(lcPckLin)
		SET RELATION TO
		SET RELATION TO STR(&lcCtnHdr..Cart_No,4)+STYLE+STR(nOrdLineNo,6) INTO (lcCtnDtl)
		REPLACE REST FOR STYLE+STR(nOrdLineNo,6) = lcStyle+STR(lnOrdLin,6) ;
			PWgh1 WITH PWgh1 - &lcCtnDtl..Weight1 + &lcCtnDtl..Qty1*lnUnitWgh,;
			PWgh2 WITH PWgh2 - &lcCtnDtl..Weight2 + &lcCtnDtl..Qty2*lnUnitWgh,;
			PWgh3 WITH PWgh3 - &lcCtnDtl..Weight3 + &lcCtnDtl..Qty3*lnUnitWgh,;
			PWgh4 WITH PWgh4 - &lcCtnDtl..Weight4 + &lcCtnDtl..Qty4*lnUnitWgh,;
			PWgh5 WITH PWgh5 - &lcCtnDtl..Weight5 + &lcCtnDtl..Qty5*lnUnitWgh,;
			PWgh6 WITH PWgh6 - &lcCtnDtl..Weight6 + &lcCtnDtl..Qty6*lnUnitWgh,;
			PWgh7 WITH PWgh7 - &lcCtnDtl..Weight7 + &lcCtnDtl..Qty7*lnUnitWgh,;
			PWgh8 WITH PWgh8 - &lcCtnDtl..Weight8 + &lcCtnDtl..Qty8*lnUnitWgh
		SET RELATION TO
		SET RELATION TO '' INTO (lcScaFile)
		SET SKIP TO (lcScaFile)
		IF lnStyRec <= RECCOUNT(lcPckLin)
			GOTO lnStyRec
			SKIP lnSzeRec-1
		ENDIF
		= RLOCK(lcPckLin)
		UNLOCK IN (lcPckLin)
	ENDIF

	IF SEEK(STR(lnCart,4)+lcStyle,lcCtnDtl)
		lnStyRec = RECNO(lcCtnDtl)
		lnSzeRec = RECNO(lcCartonSz)
		lcWghFld = 'Weight'+&lcCartonSz..SzCode
		SELECT(lcCtnDtl)
		REPLACE REST FOR STR(Cart_No,4)+STYLE = STR(lnCart,4)+lcStyle ;
			Weight1 WITH Qty1*lnUnitWgh,;
			Weight2 WITH Qty2*lnUnitWgh,;
			Weight3 WITH Qty3*lnUnitWgh,;
			Weight4 WITH Qty4*lnUnitWgh,;
			Weight5 WITH Qty5*lnUnitWgh,;
			Weight6 WITH Qty6*lnUnitWgh,;
			Weight7 WITH Qty7*lnUnitWgh,;
			Weight8 WITH Qty8*lnUnitWgh,;
			cStatus WITH 'M'
		IF lnStyRec <= RECCOUNT(lcCtnDtl)
			GOTO lnStyRec
			SKIP lnSzeRec-1
		ENDIF
		= RLOCK(lcCtnDtl)
		UNLOCK IN (lcCtnDtl)
		IF SEEK(STR(&lcCtnDtl..Cart_No,4),lcCtnHdr)
			SELECT SUM(Weight1+Weight2+Weight3+Weight4+Weight5+Weight6+Weight7+Weight8);
				FROM (lcCtnDtl) INTO ARRAY laCtnWgh;
				WHERE Cart_No = EVAL(lcCtnHdr+'.Cart_No')

			lnPackWgh = lnPackWgh - &lcCtnHdr..TotWgh + laCtnWgh

			SELECT(lcCtnHdr)
			REPLACE TotWgh WITH laCtnWgh
			= RLOCK(lcCtnHdr)
			UNLOCK IN (lcCtnHdr)
		ENDIF
	ENDIF

	= lfwCtnHdrBr()

	SELECT(lnCurAlias)

ENDIF

*!*************************************************************
*! Name      : lfvCtHdWgh
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate Carton total weight
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtHdWgh()
*!*************************************************************

FUNCTION lfvCtHdWgh

PRIVATE lnCurAlias,lnCart,lcStyle,lnOrdLin,lnUnitWgh,lnChoice
STORE 0 TO lnCart,lnOrdLin,lnUnitWgh,lnChoice
STORE SPACE(0) TO lcStyle

lnCurAlias = SELECT(0)

IF LASTKEY() = 13 AND !(lnTotWgh == lcOldVal)
	*-- This is to enforce equal contribution if the total weight was zero
	IF lcOldVal = 0
		lnChoice = 2
	ELSE
		SELECT(lcCtnDtl)
		lnStyRec = RECNO(lcCtnDtl)
		lnSzeRec = RECNO(lcCartonSz)
		SCAN
			IF (Br1 AND Weight1=0) OR (Br2 AND Weight2=0) OR ;
					(Br3 AND Weight3=0) OR (Br4 AND Weight4=0) OR ;
					(Br5 AND Weight5=0) OR (Br6 AND Weight6=0) OR ;
					(Br7 AND Weight7=0) OR (Br8 AND Weight8=0)
				*-- One or more sizes has zeros weight.
				*-- Contributing the total carton weight may
				*-- either ignore the zero weight sizes,
				*-- or contribute equal weight to all sizes
				*-- <Ignore zero sizes>,<Equal contibution>,<Cancel>
				lnChoice = gfModalGen("INM44049B44007","Dialog")
				EXIT
			ENDIF
		ENDSCAN
		IF lnStyRec <= RECCOUNT(lcCtnDtl)
			GOTO lnStyRec
			SKIP lnSzeRec-1
		ENDIF
	ENDIF
	IF !(lnChoice=3)
		SELECT(lcPckLin)
		SET RELATION TO
		SET RELATION TO STR(&lcCtnHdr..Cart_No,4)+STYLE+STR(nOrdLineNo,6) INTO (lcCtnDtl)
		lnStyRec = RECNO(lcPckLin)
		lnSzeRec = RECNO(lcScaFile)
		REPLACE ALL FOR STYLE+STR(nOrdLineNo,6)=&lcCtnDtl..Style+STR(nOrdLineNo,6);
			PWgh1 WITH IIF(&lcCtnDtl..Br1,lfPckLinUp('1'),PWgh1),;
			PWgh2 WITH IIF(&lcCtnDtl..Br2,lfPckLinUp('2'),PWgh2),;
			PWgh3 WITH IIF(&lcCtnDtl..Br3,lfPckLinUp('3'),PWgh3),;
			PWgh4 WITH IIF(&lcCtnDtl..Br4,lfPckLinUp('4'),PWgh4),;
			PWgh5 WITH IIF(&lcCtnDtl..Br5,lfPckLinUp('5'),PWgh5),;
			PWgh6 WITH IIF(&lcCtnDtl..Br6,lfPckLinUp('6'),PWgh6),;
			PWgh7 WITH IIF(&lcCtnDtl..Br7,lfPckLinUp('7'),PWgh7),;
			PWgh8 WITH IIF(&lcCtnDtl..Br8,lfPckLinUp('8'),PWgh8)
		SET RELATION TO
		SET RELATION TO '' INTO (lcScaFile)
		SET SKIP TO (lcScaFile)
		IF lnStyRec <= RECCOUNT(lcPckLin)
			GOTO lnStyRec
			SKIP lnSzeRec-1
		ENDIF
		= RLOCK(lcPckLin)
		UNLOCK IN (lcPckLin)

		SELECT(lcCtnDtl)
		lnStyRec = RECNO(lcCtnDtl)
		lnSzeRec = RECNO(lcCartonSz)
		SET RELATION TO
		= SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl)
		REPLACE REST FOR STR(Cart_No,4)+STYLE+STR(nOrdLineNo,6) = STR(&lcCtnHdr..Cart_No,4) ;
			Weight1 WITH IIF(Br1,lfCtnDtlUp('1'),Weight1),;
			Weight2 WITH IIF(Br2,lfCtnDtlUp('2'),Weight2),;
			Weight3 WITH IIF(Br3,lfCtnDtlUp('3'),Weight3),;
			Weight4 WITH IIF(Br4,lfCtnDtlUp('4'),Weight4),;
			Weight5 WITH IIF(Br5,lfCtnDtlUp('5'),Weight5),;
			Weight6 WITH IIF(Br6,lfCtnDtlUp('6'),Weight6),;
			Weight7 WITH IIF(Br7,lfCtnDtlUp('7'),Weight7),;
			Weight8 WITH IIF(Br8,lfCtnDtlUp('8'),Weight8),;
			cStatus WITH 'M'
		SET RELATION TO '' INTO (lcCartonSz)
		SET SKIP TO (lcCartonSz)
		IF lnStyRec <= RECCOUNT(lcCtnDtl)
			GOTO lnStyRec
			SKIP lnSzeRec-1
		ENDIF
		= RLOCK(lcCtnDtl)
		UNLOCK IN (lcCtnDtl)
	ENDIF

	lnPackWgh = lnPackWgh - &lcCtnHdr..TotWgh + lnTotWgh

	SELECT (lcCtnHdr)
	REPLACE TotWgh WITH lnTotWgh

	= lfwCtnHdrBr()

ELSE
	lnTotWgh = lcOldVal
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPckLinUp
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Update the weight in file lcPckLin according to
*!             update the total carton weight field
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPckLinUp()
*!*************************************************************

FUNCTION lfPckLinUp
PARAMETERS lcSize,lnReturn
PRIVATE lcSize

lcPWghFld = 'PWgh'+lcSize
lcWghFld  = lcCtnDtl+'.Weight'+lcSize
lcQtyFld  = lcCtnDtl+'.Qty'+lcSize

lnReturn = (&lcPWghFld-&lcWghFld+IIF(lnChoice=2,;
	(&lcQtyFld/&lcCtnHdr..TotPcs)*lnTotWgh,;
	(lnTotWgh/lcOldVal)*&lcWghFld))

RETURN lnReturn

*!*************************************************************
*! Name      : lfCtnDtlUp
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Update the weight in lcCtnDtl file according to
*!             update the total carton weight field
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCtnDtlUp()
*!*************************************************************

FUNCTION lfCtnDtlUp
PARAMETERS lcSize
PRIVATE lcSize,lnReturn

lcWghFld  = 'Weight'+lcSize
lcQtyFld  = 'Qty'+lcSize

lnReturn = (IIF(lnChoice=2,(&lcQtyFld/&lcCtnHdr..TotPcs)*lnTotWgh,(lnTotWgh/lcOldVal)*&lcWghFld))

RETURN lnReturn

*!*************************************************************
*! Name      : lfvCtHdNew
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate pbHdrNew button
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtHdNew()
*!*************************************************************

FUNCTION lfvCtHdNew
PRIVATE lnCurAlias

STORE .T. TO llCupdate,llAnyUpd

lnCurAlias = SELECT(0)

SELECT (lcCtnHdr)
APPEND BLANK

lnPackCtn = lnPackCtn + 1
lnMaxCtn  = lnMaxCtn  + 1

*-- Ahm (START)
*lnFrom = lnFrom + 1
*lnTo   = lnTo   + 1
lnFrom = lnMaxCtn + 1
lnTo   = lnMaxCtn + 1
*-- Ahm (END)

REPLACE Cart_No WITH lnMaxCtn,;
	EMPTY   WITH 'Y'
= RLOCK(lcCtnHdr)
UNLOCK IN (lcCtnHdr)

lcAddDtlSt = "ENABLE"
SHOW GET pbDtlNew  &lcAddDtlSt

= lfwCtnHdrBr()

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCtHdRem
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate pbHdrRem button
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtHdRem()
*!*************************************************************

FUNCTION lfvCtHdRem

PRIVATE lnAlias,lcFltrExp,lnStyRec,lnSzeRec

STORE 0 TO lnStyRec,lnSzeRec

STORE .T. TO llCupdate,llAnyUpd

lnAlias = SELECT(0)

*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>
IF gfModalGen("INM44040B00006","Dialog") = 1

	SELECT(lcPckLin)
	lcFltrExp = SET("FILTER")
	SET FILTER TO STYLE = ''
	SET RELATION TO

	SELECT(lcCtnDtl)
	SET RELATION TO
	IF SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl)
		lnStyRec = RECNO(lcPckLin)
		lnSzeRec = RECNO(lcScaFile)

		SCAN REST FOR STR(Cart_No,4)+STYLE+STR(nOrdLineNo,6) = STR(&lcCtnHdr..Cart_No,4)
			SELECT(lcPckLin)
			IF !EMPTY(&lcCtnDtl..Style) AND SEEK(&lcCtnDtl..Style+STR(&lcCtnDtl..nOrdLineNo,6),lcPckLin)
				REPLACE PQty1   WITH IIF(&lcCtnDtl..Br1,MAX(PQty1 - &lcCtnDtl..Qty1,0),PQty1),;
					PQty2   WITH IIF(&lcCtnDtl..Br2,MAX(PQty2 - &lcCtnDtl..Qty2,0),PQty2),;
					PQty3   WITH IIF(&lcCtnDtl..Br3,MAX(PQty3 - &lcCtnDtl..Qty3,0),PQty3),;
					PQty4   WITH IIF(&lcCtnDtl..Br4,MAX(PQty4 - &lcCtnDtl..Qty4,0),PQty4),;
					PQty5   WITH IIF(&lcCtnDtl..Br5,MAX(PQty5 - &lcCtnDtl..Qty5,0),PQty5),;
					PQty6   WITH IIF(&lcCtnDtl..Br6,MAX(PQty6 - &lcCtnDtl..Qty6,0),PQty6),;
					PQty7   WITH IIF(&lcCtnDtl..Br7,MAX(PQty7 - &lcCtnDtl..Qty7,0),PQty7),;
					PQty8   WITH IIF(&lcCtnDtl..Br8,MAX(PQty8 - &lcCtnDtl..Qty8,0),PQty8)
				*B603121,1 (Start)
				REPLACE OrdQty1 WITH IIF(&lcCtnDtl..Br1,OrgOrd1,OrdQty1),;
					OrdQty2 WITH IIF(&lcCtnDtl..Br2,OrgOrd2,OrdQty2),;
					OrdQty3 WITH IIF(&lcCtnDtl..Br3,OrgOrd3,OrdQty3),;
					OrdQty4 WITH IIF(&lcCtnDtl..Br4,OrgOrd4,OrdQty4),;
					OrdQty5 WITH IIF(&lcCtnDtl..Br5,OrgOrd5,OrdQty5),;
					OrdQty6 WITH IIF(&lcCtnDtl..Br6,OrgOrd6,OrdQty6),;
					OrdQty7 WITH IIF(&lcCtnDtl..Br7,OrgOrd7,OrdQty7),;
					OrdQty8 WITH IIF(&lcCtnDtl..Br8,OrgOrd8,OrdQty8)
				*B603121,1 (End)
				REPLACE PWgh1   WITH IIF(&lcCtnDtl..Br1,MAX(PWgh1 - &lcCtnDtl..Weight1,0),PWgh1),;
					PWgh2   WITH IIF(&lcCtnDtl..Br2,MAX(PWgh2 - &lcCtnDtl..Weight2,0),PWgh2),;
					PWgh3   WITH IIF(&lcCtnDtl..Br3,MAX(PWgh3 - &lcCtnDtl..Weight3,0),PWgh3),;
					PWgh4   WITH IIF(&lcCtnDtl..Br4,MAX(PWgh4 - &lcCtnDtl..Weight4,0),PWgh4),;
					PWgh5   WITH IIF(&lcCtnDtl..Br5,MAX(PWgh5 - &lcCtnDtl..Weight5,0),PWgh5),;
					PWgh6   WITH IIF(&lcCtnDtl..Br6,MAX(PWgh6 - &lcCtnDtl..Weight6,0),PWgh6),;
					PWgh7   WITH IIF(&lcCtnDtl..Br7,MAX(PWgh7 - &lcCtnDtl..Weight7,0),PWgh7),;
					PWgh8   WITH IIF(&lcCtnDtl..Br8,MAX(PWgh8 - &lcCtnDtl..Weight8,0),PWgh8)

				REPLACE OQty1   WITH IIF(&lcCtnDtl..Br1,OQty1 + &lcCtnDtl..Qty1,OQty1),;
					OQty2   WITH IIF(&lcCtnDtl..Br2,OQty2 + &lcCtnDtl..Qty2,OQty2),;
					OQty3   WITH IIF(&lcCtnDtl..Br3,OQty3 + &lcCtnDtl..Qty3,OQty3),;
					OQty4   WITH IIF(&lcCtnDtl..Br4,OQty4 + &lcCtnDtl..Qty4,OQty4),;
					OQty5   WITH IIF(&lcCtnDtl..Br5,OQty5 + &lcCtnDtl..Qty5,OQty5),;
					OQty6   WITH IIF(&lcCtnDtl..Br6,OQty6 + &lcCtnDtl..Qty6,OQty6),;
					OQty7   WITH IIF(&lcCtnDtl..Br7,OQty7 + &lcCtnDtl..Qty7,OQty7),;
					OQty8   WITH IIF(&lcCtnDtl..Br8,OQty8 + &lcCtnDtl..Qty8,OQty8)

				REPLACE CtnQty1 WITH IIF(&lcCtnDtl..Br1,IIF(cSelect1='»',OQty1,0),CtnQty1),;
					CtnQty2 WITH IIF(&lcCtnDtl..Br2,IIF(cSelect2='»',OQty2,0),CtnQty2),;
					CtnQty3 WITH IIF(&lcCtnDtl..Br3,IIF(cSelect3='»',OQty3,0),CtnQty3),;
					CtnQty4 WITH IIF(&lcCtnDtl..Br4,IIF(cSelect4='»',OQty4,0),CtnQty4),;
					CtnQty5 WITH IIF(&lcCtnDtl..Br5,IIF(cSelect5='»',OQty5,0),CtnQty5),;
					CtnQty6 WITH IIF(&lcCtnDtl..Br6,IIF(cSelect6='»',OQty6,0),CtnQty6),;
					CtnQty7 WITH IIF(&lcCtnDtl..Br7,IIF(cSelect7='»',OQty7,0),CtnQty7),;
					CtnQty8 WITH IIF(&lcCtnDtl..Br8,IIF(cSelect8='»',OQty8,0),CtnQty8)
			ENDIF

			*-- Ahm (START)
			*-- replacing this with subtract the weight and the quantity of
			*-- the carton from pack weight and quantity

			*lcWghFld = lcCtnDtl+'.Weight'+&lcCartonSz..SzCode
			*lcQtyFld = lcCtnDtl+'.Qty'+&lcCartonSz..SzCode
			*lnPackWgh = MAX(lnPackWgh - &lcWghFld,0)
			*lnPackQty = MAX(lnPackQty - &lcQtyFld,0)
			*-- Ahm (END)

			SELECT (lcCtnDtl)
			REPLACE cStatus WITH 'D'
			DELETE

		ENDSCAN


	ENDIF
	SELECT (lcPckLin)
	SET FILTER TO &lcFltrExp
	= RLOCK(lcCtnDtl)
	UNLOCK IN (lcCtnDtl)

	SET RELATION TO '' INTO (lcScaFile)
	SET SKIP TO (lcScaFile)
	IF lnStyRec <> 0 AND lnStyRec <= RECCOUNT(lcPckLin)
		GOTO lnStyRec
		SKIP lnSzeRec-1
	ENDIF

	SELECT (lcCtnDtl)
	SET RELATION TO '' INTO (lcCartonSz)
	SET SKIP TO (lcCartonSz)

	SELECT(lcCtnHdr)
	*-- This means that the carton has the max carton No.
	*-- which means it is the last record in the file
	IF &lcCtnHdr..Cart_No = lnMaxCtn
		SKIP -1
		lnMaxCtn = &lcCtnHdr..Cart_No
		IF !BOF()
			SKIP 1
			*IF EOF()
			*  lnMaxCtn = 0
			*  SKIP -1
			*ENDIF
		ENDIF
	ENDIF

	lnPackCtn = lnPackCtn - 1



	*-- Ahm (START)
	*-- This is instead of subtracting each deleteing line in carton detail
	*-- from Pack weight and quantity
	lnPackWgh = MAX(lnPackWgh - &lcCtnHdr..TotWgh,0)
	lnPackQty = MAX(lnPackQty - &lcCtnHdr..TotPcs,0)
	*-- Ahm (END)

	DELETE
	GO TOP
	= RLOCK(lcCtnHdr)
	UNLOCK IN (lcCtnHdr)	

	lnMaxCtn = MAX(lnMaxCtn - 1,0)
	
	lnFrom   = lnMaxCtn + 1
	lnTo     = lnMaxCtn + 1

	llNoThing = lfwCtnHdrBr()
	
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvCtDtNew
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate pbDtlNew button
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtDtNew()
*!*************************************************************

FUNCTION lfvCtDtNew
PRIVATE lnCurAlias,lcBrFld,lcNewFld,lnK

STORE .T. TO llCupdate,llAnyUpd
lnCurAlias = SELECT(0)

SELECT(lcCtnDtl)

IF !SEEK(STR(&lcCtnHdr..Cart_NO,4)+SPACE(19),lcCtnDtl)
	APPEND BLANK
	REPLACE Cart_No WITH &lcCtnHdr..Cart_NO,;
		SzCnt   WITH 1,;
		Br1     WITH .T.

	= RLOCK(lcCtnDtl)
	UNLOCK IN (lcCtnDtl)
ELSE
	REPLACE Br1     WITH .T.
ENDIF
= lfwCtnDtlBr()

SHOW GET pbDtlRem ENABLE

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCtDtRem
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate pbDtlRem button
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtDtRem()
*!*************************************************************

FUNCTION lfvCtDtRem

PRIVATE lnAlias,lcFltrExp,lcSzFld

STORE .T. TO llCupdate,llAnyUpd
lnAlias = SELECT(0)
*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>
IF gfModalGen("INM44040B00006","Dialog") = 1

	SELECT(lcPckLin)
	lcFltrExp = SET("FILTER")
	SET FILTER TO STYLE = ''
	lnSzeRec = RECNO(lcCartonSz)

	IF !EMPTY(&lcCtnDtl..Style) AND SEEK(&lcCtnDtl..Style+STR(&lcCtnDtl..nOrdLineNo,6),lcPckLin)
		SELECT(lcPckLin)
		SKIP lnSzeRec-1
		lcPQtyFld   = 'PQty'   + &lcScaFile..SzCode
		lcPWghFld   = 'PWgh'   + &lcScaFile..SzCode
		lcOQtyFld   = 'OQty'   + &lcScaFile..SzCode
		lcCtnQtyFld = 'CtnQty' + &lcScaFile..SzCode
		lcSelectFld = 'cSelect'+ &lcScaFile..SzCode

		*B603121,1 (Start)
		lcOrdQtyFld = 'OrdQty'+ &lcScaFile..SzCode
		lcOrgOrdFld = 'OrgOrd'+ &lcScaFile..SzCode
		*REPLACE &lcPQtyFld   WITH MAX(EVAL('PQty'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode),0) ,;
		&lcPWghFld   WITH MAX(EVAL('PWgh'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode),0),;
		&lcOQtyFld   WITH EVAL('OQty'+&lcScaFile..SzCode) + EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode),;
		&lcCtnQtyFld WITH IIF(EVAL('cSelect'+&lcScaFile..SzCode)='»',EVAL('OQty'+&lcScaFile..SzCode),0)

	REPLACE &lcPQtyFld   WITH MAX(EVAL('PQty'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode),0) ,;
		&lcPWghFld   WITH MAX(EVAL('PWgh'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode),0),;
		&lcOQtyFld   WITH EVAL('OQty'+&lcScaFile..SzCode) + EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode),;
		&lcCtnQtyFld WITH IIF(EVAL('cSelect'+&lcScaFile..SzCode)='»',EVAL('OQty'+&lcScaFile..SzCode),0),;
		&lcOrdQtyFld WITH &lcOrgOrdFld
	*B603121,1 (End)



	= RLOCK(lcPckLin)
	UNLOCK IN (lcPckLin)
ENDIF
SET FILTER TO &lcFltrExp

lnPackWgh = MAX(lnPackWgh - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode),0)
lnPackQty = MAX(lnPackQty - EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode),0)

SELECT (lcCtnHdr)
REPLACE TotPcs WITH MAX(TotPcs - EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode),0),;
	TotWgh WITH MAX(TotWgh - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode),0),;
	EMPTY  WITH IIF(TotPcs>0,'N','Y')
= RLOCK(lcCtnHdr)
UNLOCK IN (lcCtnHdr)
lcFld    = 'Br'  + &lcCartonSz..Szcode
lcSzFld  = 'Size'+ &lcCartonSz..Szcode
lcQtyFld = 'Qty' + &lcCartonSz..Szcode
SELECT (lcCtnDtl)
REPLACE &lcFld     WITH .F. ,;
	&lcSzFld   WITH ''  ,;
	&lcQtyFld  WITH 0
IF Br1 OR Br2 OR Br3 OR Br4 OR Br5 OR Br6 OR Br7 OR Br8
	REPLACE cStatus WITH 'M'
ELSE
	REPLACE cStatus WITH 'D'
	DELETE

ENDIF
*B804458,1 (Begin) Remark the following line and change the variable name from the one in
*B804458,1         the loop as it always - when al BRs are .f. - with 9
*lnM = 0
*FOR lnM = 1 TO 8
lnMn = 0
FOR lnM = 1 TO 8
	lnMn = lnMn +1
	*B804458,1 (End)
	IF EVAL('Br'+STR(lnM,1))
		EXIT
	ENDIF
ENDFOR
*B804458,1 (Begin) Remark the following line and change the variable name from the one in
*IF lnM <= 8
IF lnMn <= 8
	*B804458,1 (End)
	GO TOP IN (lcCartonSz)
	SKIP lnM-1 IN (lcCtnDtl)
ELSE
	SELECT (lcCtnDtl)
	REPLACE Cart_No WITH 0,;
		STYLE   WITH ''
ENDIF
= RLOCK(lcCtnDtl)
UNLOCK IN (lcCtnDtl)
llNoThing = lfwCtnDtlBr()
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
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
*! Example   : = lfvCtDtRem()
*!*************************************************************

FUNCTION lfActFolder

STORE IIF((laScrMode[3] OR llNew) AND lnActFolder = 1,"ENABLE","DISABLE") TO lcInstStat,lcShipStat

IF !llUnComp AND ( ((ASCAN(laScrMode,.T.)<>lnPrvMode) AND INLIST(lnActFolder,2,3) AND !laScrMode[1]) OR ;
		(!(laData[1]==lcPrvPack)) AND INLIST(lnActFolder,2,3) AND !laScrMode[1]) )
	= lfvSelOrdL()
ENDIF

lnLastFold = lnActFolder
llNoThing = lfUpdVars()

= lfWinHdrSt()

DO CASE
CASE lnActFolder = 1
	IF laScrMode[3] OR laScrMode[4]
		lcWinHdrSt = "ENABLE"
	ELSE
		lcWinHdrSt = "DISABLE"
	ENDIF

	SHOW GETS WINDOW (lcWinDtl1)  DISABLE ONLY
	SHOW GETS WINDOW (lcWinDtl2)  DISABLE ONLY
	SHOW GETS WINDOW (lcWinCtn1)  DISABLE ONLY
	SHOW GETS WINDOW (lcWinCtn2)  DISABLE ONLY
	SHOW GETS WINDOW (lcWinCtn3)  DISABLE ONLY

	*E120116,1  TMI [Start] Update status of Assign Carrier Carton ID field
	PRIVATE lcAsgnCrIDSt
	lcAsgnCrIDSt = IIF(laScrMode[3] .OR. laScrMode[4],'ENABLE','DISABLE')
	SHOW GET laData[18] &lcAsgnCrIDSt
	*E120116,1  TMI [End  ]
	= lfRefresh(lcWinHdr)

CASE lnActFolder = 2
	SHOW GETS WINDOW (lcWinHdr)   DISABLE ONLY
	SHOW GETS WINDOW (lcWinCtn1)  DISABLE ONLY
	SHOW GETS WINDOW (lcWinCtn2)  DISABLE ONLY
	SHOW GETS WINDOW (lcWinCtn3)  DISABLE ONLY

	= lfSelStatus()

	IF !WEXIST(lcDtlBrTtl)
		= lfDtlBrow()
	ELSE
		= lfwDtlBrow()
	ENDIF

CASE lnActFolder = 3
	SHOW GETS WINDOW (lcWinHdr)   DISABLE ONLY
	SHOW GETS WINDOW (lcWinDtl1)  DISABLE ONLY
	SHOW GETS WINDOW (lcWinDtl2)  DISABLE ONLY
	= lfWinCtnSt()
	ACTIVATE WINDOW (lcWinCtn3)
	SHOW GET ibCtnHdr ENABLE
	SHOW GET ibCtnDtl ENABLE
	IF !WEXIST(lcCtHdBrTt)
		= lfCtnHdrBr()
	ELSE
		= lfwCtnHdrBr()
	ENDIF
	IF !WEXIST(lcCtDtBrTt)
		= lfCtnDtlBr()
	ELSE
		= lfwCtnDtlBr()
	ENDIF

ENDCASE

*!*************************************************************
*! Name      : lfTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO Assign functions to some keys to not affect the browse
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfBrTab,lfBrBack
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrap()
*!*************************************************************

FUNCTION lfTrap

IF !INLIST(WONTOP(),gcBaseWind,lcDtlBrTtl,lcCtHdBrTt,lcCtDtBrTt)
	=lfRelPad()
ENDIF

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF INLIST(WONTOP(),lcDtlBrTtl,lcCtHdBrTt,lcCtDtBrTt)
	glFromBrow = .T.
	ON KEY LABEL TAB     DO lfBrTab
	ON KEY LABEL BACKTAB DO lfBrBack
ELSE
	glFromBrow = .F.
ENDIF

*!*************************************************************
*! Name      : lfClrTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Clearing the previous trapping
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfClrTrap()
*!*************************************************************

FUNCTION lfClrTrap

=lfActPad()

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore
*-- the previous on key label
IF glFromBrow
	=gfStopBrow()
ENDIF

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lfBrTab
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the Tab Key
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrTab()
*!*************************************************************

FUNCTION lfBrTab

ON KEY LABEL TAB
DO CASE
CASE lnActFolder = 2 AND WONTOP() = lcDtlBrTtl
	ACTIVATE WINDOW (lcWinDtl2)
	_CUROBJ = OBJNUM(lnFrom)
CASE lnActFolder = 3 AND WONTOP() = lcCtHdBrTt
	ACTIVATE WINDOW (lcWinCtn3)
	_CUROBJ = OBJNUM(pbHdrNew)
CASE lnActFolder = 3 AND WONTOP() = lcCtDtBrTt
	ACTIVATE WINDOW (lcWinCtn3)
	_CUROBJ = OBJNUM(pbDtlNew)
ENDCASE

*!*************************************************************
*! Name      : lfBrBack
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrBack()
*!*************************************************************

FUNCTION lfBrBack

ON KEY LABEL BACKTAB
DO CASE
CASE lnActFolder = 2 AND WONTOP() = lcDtlBrTtl
	ACTIVATE WINDOW (lcFolder)
	_CUROBJ = OBJNUM(ibFolder[2])
CASE lnActFolder = 3 AND WONTOP() = lcCtHdBrTt
	ACTIVATE WINDOW (lcFolder)
	_CUROBJ = OBJNUM(ibFolder[3])
CASE lnActFolder = 3 AND WONTOP() = lcCtDtBrTt
	ACTIVATE WINDOW (lcWinCtn3)
	IF lcRemHdrSt = "ENABLE"
		_CUROBJ = OBJNUM(pbHdrRem)
	ELSE
		IF lcAddHdrSt = "ENABLE"
			_CUROBJ = OBJNUM(pbHdrNew)
		ELSE
			_CUROBJ = OBJNUM(ibCtnHdr)
		ENDIF
	ENDIF
ENDCASE

*!*************************************************************
*! Name      : lfDtlBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse lcPckLin file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfwBrow,lfvBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDtlBrow()
*!*************************************************************

FUNCTION lfDtlBrow

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

*C#102281,1 AAN [Begin] Check if the user chooce order by what and open a new alias by specific order.
*SELECT (lcPckLin)

*B605583,1 HBG 24/02/2002 Check if file is open to fix bug of Alias is already in use [Begin]
IF !USED(lcTmpPck)
	*B605583,1 [End]
	IF gfGetMemVar('M_ORDSTUTS',gcAct_Comp) = 'L'
		USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcTmpIdx)
	ELSE
		USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPckLin)
	ENDIF
	*B605583,1 HBG 24/02/2002 End Check if file is open to fix bug of Alias is already in use [Begin]
ENDIF
*B605583,1 [End]

SELECT (lcTmpPck)
*C#102281,1 AAN [End].
GO TOP
IF EMPTY(SET('RELATION'))
	SET RELATION TO '' INTO (lcScaFile) ADDITIVE
	SET SKIP TO (lcScaFile)
ENDIF

lnDtBrRec = RECNO()
lnDumDtlR = RECNO(lcScaFile)

*--C102281 AAN Change a browse stat. with a new alias [Begin].
*BROWSE FIELDS cMarker =IIF(RECNO()=lnDtBrRec AND RECNO(lcScaFile) = lnDumDtlR,'>',' '):H= ' ':R:1:W=.F.,;
Selector=EVAL('cSelect'+&lcScaFile..SzCode) :H = ' ':R:1:W=.F.,;
StyCode =STYLE:H = lcStyTtl  :25:R,;
SIZE    =EVAL('cSize' +&lcScaFile..SzCode):H = 'Size'  :7 :R,;
OpenQty =EVAL('OQty'  +&lcScaFile..SzCode):H = 'O.Qty.':6 :R,;
CartQty =EVAL('CtnQty'+&lcScaFile..SzCode):H = 'Qty.\Ctn':10:R,;
Weight  =StyWgh:H = 'Wgh.\Unt':10:R,;
PackQty =EVAL('PQty'+&lcScaFile..SzCode):H = 'P.Qty.':6 :R,;
PackWgh =EVAL('PWgh'+&lcScaFile..SzCode):H = 'P.Wgh.':6 :R ;
FOR IIF(llShwOpn,EVAL('OQty'+&lcScaFile..SzCode)>0 AND EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1),;
EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1));
SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
TITLE(lcDtlBrTtl) WHEN lfwDtlBrow() ;
VALID :F lfvDtlBrow()    ;
WINDOW (lcWinDtl1) IN WINDOW (lcWinDtl)

*aannew
*BROWSE FIELDS cMarker =IIF(RECNO()=lnDtBrRec AND RECNO(lcScaFile) = lnDumDtlR,'>',' '):H= ' ':R:1:W=.F.,;
Selector=EVAL('cSelect'+&lcScaFile..SzCode) :H = ' ':R:1:W=.F.,;
StyCode =STYLE:H = lcStyTtl  :25:R,;
SIZE    =EVAL('cSize' +&lcScaFile..SzCode):H = 'Size'  :7 :R,;
OpenQty =EVAL('OQty'  +&lcScaFile..SzCode):H = 'O.Qty.':6 :R,;
CartQty =EVAL('CtnQty'+&lcScaFile..SzCode):H = 'Qty.\Ctn':10:R,;
Weight  =StyWgh:H = 'Wgh.\Unt':10:R,;
PackQty =EVAL('PQty'+&lcScaFile..SzCode):H = 'P.Qty.':6 :R,;
PackWgh =EVAL('PWgh'+&lcScaFile..SzCode):H = 'P.Wgh.':6 :R ;
FOR IIF(llShwOpn,EVAL('OQty'+&lcScaFile..SzCode)>0 AND EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcTmpPck..SzCnt,1),;
EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcTmpPck..SzCnt,1));
SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
TITLE(lcDtlBrTtl) WHEN lfwDtlBrow() ;
VALID :F lfvDtlBrow()    ;
WINDOW (lcWinDtl1) IN WINDOW (lcWinDtl)

BROWSE FIELDS cMarker =IIF(RECNO()=lnDtBrRec AND RECNO(lcScaFile) = lnDumDtlR,'>',' '):H= ' ':R:1:W=.F.,;
	Selector=EVAL('cSelect'+&lcScaFile..SzCode) :H = ' ':R:1:W=.F.,;
	StyCode =STYLE:H = lcStyTtl  :25:R,;
	SIZE    =EVAL('cSize' +&lcScaFile..SzCode):H = 'Size'  :7 :R,;
	OpenQty =EVAL('OQty'  +&lcScaFile..SzCode):H = 'O.Qty.':6 :R,;
	CartQty =EVAL('CtnQty'+&lcScaFile..SzCode):H = 'Qty.\Ctn':10:R,;
	Weight  =StyWgh:H = 'Wgh.\Unt':10:R,;
	PackQty =EVAL('PQty'+&lcScaFile..SzCode):H = 'P.Qty.':6 :R,;
	PackWgh =EVAL('PWgh'+&lcScaFile..SzCode):H = 'P.Wgh.':6 :R ;
	FOR IIF(llShwOpn,EVAL('OQty'+&lcScaFile..SzCode)>0 AND EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcTmpPck..SzCnt,1),;
	EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcTmpPck..SzCnt,1));
	SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
	TITLE(lcDtlBrTtl) WHEN lfwDtlBrow() ;
	VALID :F lfvDtlBrow()    ;
	WINDOW (lcWinDtl1) IN WINDOW (lcWinDtl)
*--C102281 AAN Change a browse stat. with a new alias [End].
*aannewend
= lfwDtlBrow()

*!*************************************************************
*! Name      : lfwDtlBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : adjust the label of pbsel button
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwDtlBrow()
*!*************************************************************

FUNCTION lfwDtlBrow
PRIVATE lnAlias

lnDumDtlR = RECNO(lcScaFile)

lcSelStt = IIF((laScrMode[3] OR llNew) AND lcDtl2Stat = 'ENABLE','ENABLE','DISABLE')
lnAlias = SELECT(0)
*--C102281 AAN Check if the new alias is used or not [Begin].
*SELECT (lcPckLin)

IF !USED(lcTmpPck)

	*B605583,1 HBG 24/02/2002 Check if file is open to fix bug of Alias is already in use [Begin]
	IF !USED(lcTmpPck)
		*B605583,1 [End]
		IF gfGetMemVar('M_ORDSTUTS',gcAct_Comp) = 'L'
			USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcTmpIdx)
		ELSE
			USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPckLin)
		ENDIF
		*B605583,1 HBG 24/02/2002 End Check if file is open to fix bug of Alias is already in use [Begin]
	ENDIF
	*B605583,1 [End]
ENDIF

SELECT (lcTmpPck)
*--C102281 AAN Check if the new alias is used or not [End].
lnDtBrRec = RECNO()
IF lnActFolder = 2
	*--C102281 AAN Change the following statments with a new alias [Begin].
	*lnBrCtnQty = EVAL(lcPckLin+'.CtnQty' + &lcScaFile..SzCode)
	*lnBrUntWgh = EVAL(lcPckLin+'.StyWgh')
	lnBrCtnQty = EVALUATE(lcTmpPck+'.CtnQty' + &lcScaFile..SzCode)
	lnBrUntWgh = EVALUATE(lcTmpPck+'.StyWgh')
	*--C102281 AAN Change the following statments with a new alias [End].

	SHOW WINDOW (lcDtlBrTtl) REFRESH SAME
ENDIF

lcSel  = IIF(EMPTY(EVAL('cSelect'+&lcScaFile..SzCode)),"Select","UnSelect")
SHOW GET pbSel,1 PROMPT lcSel &lcSelStt
= lfSelStatus()
IF llQtyArr OR llWghArr
	CLEAR TYPEAHEAD
	KEYBOARD "{RIGHTARROW}{LEFTARROW}" CLEAR PLAIN
	ACTIVATE WINDOW (lcWinDtl2)
	DO CASE
	CASE llQtyArr
		_CUROBJ = OBJNUM(lnBrCtnQty)
	CASE llWghArr
		_CUROBJ = OBJNUM(lnBrUntWgh)
	ENDCASE
	STORE .F. TO llQtyArr,llWghArr
ENDIF

*B804384,1 HBG 13/09/2001 Get the pallet No , and refresh the field of it in the Detail folder[Begin]
*B607210,1 KHM 05/21/2003 (Begin) Commenting the following line because it shows wrong plate #
*lnCtnPal  = &lcCtnHdr..Pal_No
*B607210,1 KHM 05/21/2003 (End)
SHOW GET lnCtnPal
*B804384,1 [End]

SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvDtlBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() function
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDtlBrow()
*!*************************************************************

FUNCTION lfvDtlBrow

IF WONTOP() # (lcDtlBrTtl)
	=gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfvCtnQty
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate quantity per carton
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtnQty()
*!*************************************************************

FUNCTION lfvCtnQty

PRIVATE lnCurAlias

*B039660,1 NNA 04/16/2006 (Begin) Check if using bin location or not to prevent user form changing the Pack Qty.
IF ASCAN(laEvntTrig , PADR('CHNGQTY',10)) <> 0 AND !gfDoTriger('ALPLIST',PADR('CHNGQTY',10)) 
  RETURN 
ENDIF  
*B039660,1 NNA (End)

lnCurAlias = SELECT(0)
lcFldNam = 'CtnQty'+&lcScaFile..SzCode

*B605661,1 HBG 03/06/2002 Save the size # to back to it again after seeking in Temp Pack_Lin
*B605661,1                to fix bug of Can not enter weight for Single line orders [Begin]
lcSize = &lcScaFile..SzCode
*B605661,1 [End]

*B607157,1 HBG 04/13/2003 Fix bug of changing Qty in Qty/Crt field doesn't update the temp file [Begin]
*IF !EMPTY(EVAL(lcPckLin+'.cSelect'+&lcScaFile..SzCode)) AND lnBrCtnQty > 0
IF !EMPTY(EVAL(lcTmpPck+'.cSelect'+&lcScaFile..SzCode)) AND lnBrCtnQty > 0
	*B607157,1 [End]
	SELECT (lcPckLin)
	=SEEK(EVAL(lcTmpPck+'.STYLE') + STR(EVAL(lcTmpPck+'.nOrdLineNo'),6) )
	
	*B130950,1 EIH 30/01/2006 Do not allow user to pack more than existing inventory if costing method is FIFO or LIFO [Begin].
    IF lcCostMeth $ 'FL'
      IF lnBrCtnQty > EVAL('STK'+&lcScaFile..SzCode)
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'There is not enough stock')
        lnBrCtnQty = EVAL('STK'+&lcScaFile..SzCode)
      ENDIF
    ENDIF
    *B130950,1 EIH [End].
    
	REPLACE &lcFldNam WITH lnBrCtnQty
	= RLOCK(lcPckLin)
	UNLOCK IN (lcPckLin)
	SHOW WINDOW (lcDtlBrTtl) REFRESH SAME
ELSE
	lnBrCtnQty = lcOldVal
ENDIF
*B605661,1 HBG 03/06/2002 back to the correct Size it again after seeking in Temp Pack_Lin
*B605661,1                to fix bug of Can not enter weight for Single line orders [Begin]
=SEEK(lcSize,lcScaFile)
*B605661,1 [End]

lcSkip = IIF(LASTKEY() = 5,"{UPARROW}",IIF(LASTKEY() = 24,"{DNARROW}",""))
lcKeyb = ""
SELECT(lnCurAlias)
CLEAR TYPEAHEAD
llQtyArr = .F.
IF !EMPTY(lcSkip)
	llQtyArr = .T.
	lcKeyb = ["{ALT+B}+]+lcSkip+'"'
	KEYBOARD &lcKeyB PLAIN CLEAR
ENDIF

*!*************************************************************
*! Name      : lfvStyWgh
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate unit weight per carton
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStyWgh()
*!*************************************************************

FUNCTION lfvStyWgh

PRIVATE lnCurAlias,lnCurWgh,lcSelSty

lnCurAlias = SELECT(0)

*B605661,1 HBG 03/06/2002 Save the size # to back to it again after seeking in Temp Pack_Lin
*B605661,1                to fix bug of Can not enter weight for Single line orders [Begin]
lcSize = &lcScaFile..SzCode
*B605661,1 [End]

IF !EMPTY(EVAL(lcPckLin+'.cSelect'+&lcScaFile..SzCode)) AND lnBrUntWgh > 0
	SELECT (lcPckLin)
	=SEEK(EVAL(lcTmpPck+'.STYLE') + STR(EVAL(lcTmpPck+'.nOrdLineNo'),6)  )
	REPLACE StyWgh WITH lnBrUntWgh
	= RLOCK(lcPckLin)
	UNLOCK IN (lcPckLin)
	SHOW WINDOW (lcDtlBrTtl) REFRESH SAME
ELSE
	lnBrUntWgh = lcOldVal
ENDIF
*B605661,1 HBG 03/06/2002 pack to the correct size again after seeking in Temp Pack_Lin
*B605661,1                to fix bug of Can not enter weight for Single line orders [Begin]
=SEEK(lcSize,lcScaFile)
*B605661,1 [End]
lcSkip = IIF(LASTKEY() = 5,"{UPARROW}",IIF(LASTKEY() = 24,"{DNARROW}",""))
lcKeyb = ""
SELECT(lnCurAlias)
CLEAR TYPEAHEAD
llWghArr = .F.
IF !EMPTY(lcSkip)
	llWghArr = .T.
	lcKeyb = ["{ALT+B}+]+lcSkip+'"'
	KEYBOARD &lcKeyB PLAIN CLEAR
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCtnPal
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate palette No.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtnPal()
*!*************************************************************

FUNCTION lfvCtnPal

*B804384,1 HBG 13/09/2001 Accept the 0 PALLET No. [Begin]
*IF lnCtnPal <= 0
*-- palette no has to be greater than 0.
*-- OK
*  = gfModalGen("INM44046B00000","Dialog")
*  lnCtnPal = lcOldval
SHOW GET lnCtnPal
*ENDIF
*B804384,1 [End]

*!*************************************************************
*! Name      : lfvPalNo
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate palette No for carton header edit rigion.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPalNo()
*!*************************************************************

FUNCTION lfvPalNo

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

IF lnPalNo < lnMinPal OR (lnPalNo - IIF(lnMaxPal=0,lnPalNo,lnMaxPal)) > 1
	*-- palette no has to be sequential.
	*-- OK
	= gfModalGen("INM44048B00000","Dialog")
	lnPalNo = lcOldVal
	SHOW GET lnPalNo
ELSE
	SELECT(lcCtnHdr)
	REPLACE Pal_No WITH lnPalNo
	= RLOCK(lcCtnHdr)
	UNLOCK IN (lcCtnHdr)
	lnMaxPal = MAX(lnMaxPal,&lcCtnHdr..Pal_No)
	lnMinPal = MIN(IIF(lnMinPal=0,&lcCtnHdr..Pal_No,lnMinPal),&lcCtnHdr..Pal_No)
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvApply
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate pbApply button
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfwDtlBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvApply()
*!*************************************************************

FUNCTION lfvApply

PRIVATE lnCurAlias,lnCurRec,lnJ,laCart,lnContinue,lcTag
DIMENSION laCart[1]
STORE 0 TO laCart,lnContinue
lnCurAlias = SELECT(0)
SELECT (lcPckLin)
SET RELATION TO
lnCurRec = RECNO()
lnSizeRec = VAL(&lcScaFile..SzCode)

IF lnFrom <= 0 OR lnTo <= 0 OR lnFrom > lnTo
	*-- You have to enter correct carton range.
	*-- <OK>
	= gfModalGen("INM44033B00000","Dialog")
	_CUROBJ = OBJNUM(lnFrom)
ELSE
	IF llPalStat
		SELECT COUNT(Cart_No) FROM (lcCtnHdr) INTO ARRAY laCart;
			WHERE Pal_No <> lnCtnPal;
			AND   BETWEEN (Cart_No,lnFrom,lnTo)
	ENDIF
	IF laCart[1] > 0
		*-- some cartons in carton range related to another palette.
		*-- OK
		= gfModalGen("INM44047B00000","Dialog")
	ELSE
		= lfUpdate()
		*-- This part is to avoid entering a new palette number less
		*-- than the first palette number.
		*-- It also will avoid having gaps between palette numbers.
		*-- In order to check if there is a gap between two palette numbers
		*-- we subtract the last pallet number (lnMaxPal) from the currently
		*-- entered pallet number, the difference should not be greater than 1.
		*-- this rule is valid, except for the first pallet number to be
		*-- entered, because this rule will prevent the user from start
		*-- counting the pallets using any number greater than 1, that's why
		*-- we should not use that rule when this is the first pallet number
		*-- to be entered (in other words when lnMaxPal = 0).

		IF llPalStat AND lnCtnPal < lnMinPal OR (lnCtnPal - IIF(lnMaxPal=0,lnCtnPal,lnMaxPal)) > 1
			*-- palette No. has to be sequential.
			*-- OK
			= gfModalGen("INM44048B00000","Dialog")
			lnCtnPal = lnMaxPal + 1
			SHOW GET lnCtnPal
		ELSE
			lcTag = ORDER(lcPckLin)
			SET ORDER TO Selected IN (lcPckLin)
			IF SEEK('Y',lcPckLin)
				SCAN REST WHILE Selected>0
					*-- Here, is comparison between packed quantity and ordered quantity
					*-- in ordline file and give choice to modify the ordered qauntity if
					*-- packed exceeded the ordered
					lnI  = 0
					FOR lnI = 1 TO &lcPckLin..SzCnt
						lcSelFld    = lcPckLin + '.cSelect' + STR(lnI,1)
						lcCtnQtyFld = lcPckLin + '.CtnQty'  + STR(lnI,1)
						lcOrdQtyFld = lcPckLin + '.OrdQty'  + STR(lnI,1)
						lcPQtyFld   = lcPckLin + '.PQty'    + STR(lnI,1)
						lcCtnQtyFld = lcPckLin + '.CtnQty'  + STR(lnI,1)
						lcSizeFld   = lcPckLin + '.cSize'   + STR(lnI,1)
						IF !EMPTY(&lcSelFld) AND !EMPTY(&lcCtnQtyFld) AND ;
								&lcOrdQtyFld < &lcPQtyFld + (&lcCtnQtyFld*(lnTo-lnFrom+1))
							*-- Packed quantity for Style/Size x/x exceeds ordered quantity.;
							DO you want TO MODIFY the orderd quantity?
						*-- <Yes>,<No>
						lnContinue = gfModalGen("QRM44034B00006","Dialog",&lcPckLin..Style + '/' + EVAL(lcSizeFld))
						EXIT
					ENDIF
				ENDFOR
			ENDSCAN
		ENDIF
		IF lnContinue <> 2
			SET ORDER TO Selected
			IF SEEK('Y',lcPckLin)
				SCAN REST WHILE Selected>0
					SELECT(lcPckLin)
					*-- This for updating order qty with the new order qty if the user
					*-- select to update order qty with exceeds qty.
					IF lnContinue = 1
						SELECT (lcPckLin)
						*B603121 (Start)
						*REPLACE OrdQty1 WITH IIF(EMPTY(cSelect1) AND EMPTY(CtnQty1),OrdQty1,(CtnQty1*(lnTo-lnFrom+1))),;
						OrdQty2 WITH IIF(EMPTY(cSelect2) AND EMPTY(CtnQty2),OrdQty2,(CtnQty2*(lnTo-lnFrom+1))),;
						OrdQty3 WITH IIF(EMPTY(cSelect3) AND EMPTY(CtnQty3),OrdQty3,(CtnQty3*(lnTo-lnFrom+1))),;
						OrdQty4 WITH IIF(EMPTY(cSelect4) AND EMPTY(CtnQty4),OrdQty4,(CtnQty4*(lnTo-lnFrom+1))),;
						OrdQty5 WITH IIF(EMPTY(cSelect5) AND EMPTY(CtnQty5),OrdQty5,(CtnQty5*(lnTo-lnFrom+1))),;
						OrdQty6 WITH IIF(EMPTY(cSelect6) AND EMPTY(CtnQty6),OrdQty6,(CtnQty6*(lnTo-lnFrom+1))),;
						OrdQty7 WITH IIF(EMPTY(cSelect7) AND EMPTY(CtnQty7),OrdQty7,(CtnQty7*(lnTo-lnFrom+1))),;
						OrdQty8 WITH IIF(EMPTY(cSelect8) AND EMPTY(CtnQty8),OrdQty8,(CtnQty8*(lnTo-lnFrom+1)))
					REPLACE OrdQty1 WITH IIF(EMPTY(cSelect1) OR (!EMPTY(cSelect1) AND EMPTY(CtnQty1)),OrdQty1,MAX(OrdQty1,OrgPQty1+(CtnQty1*(lnTo-lnFrom+1)))),;
						OrdQty2 WITH IIF(EMPTY(cSelect2) OR (!EMPTY(cSelect2) AND EMPTY(CtnQty2)),OrdQty2,MAX(OrdQty2,OrgPQty2+(CtnQty2*(lnTo-lnFrom+1)))),;
						OrdQty3 WITH IIF(EMPTY(cSelect3) OR (!EMPTY(cSelect3) AND EMPTY(CtnQty3)),OrdQty3,MAX(OrdQty3,OrgPQty3+(CtnQty3*(lnTo-lnFrom+1)))),;
						OrdQty4 WITH IIF(EMPTY(cSelect4) OR (!EMPTY(cSelect4) AND EMPTY(CtnQty4)),OrdQty4,MAX(OrdQty4,OrgPQty4+(CtnQty4*(lnTo-lnFrom+1)))),;
						OrdQty5 WITH IIF(EMPTY(cSelect5) OR (!EMPTY(cSelect5) AND EMPTY(CtnQty5)),OrdQty5,MAX(OrdQty5,OrgPQty5+(CtnQty5*(lnTo-lnFrom+1)))),;
						OrdQty6 WITH IIF(EMPTY(cSelect6) OR (!EMPTY(cSelect6) AND EMPTY(CtnQty6)),OrdQty6,MAX(OrdQty6,OrgPQty6+(CtnQty6*(lnTo-lnFrom+1)))),;
						OrdQty7 WITH IIF(EMPTY(cSelect7) OR (!EMPTY(cSelect7) AND EMPTY(CtnQty7)),OrdQty7,MAX(OrdQty7,OrgPQty7+(CtnQty7*(lnTo-lnFrom+1)))),;
						OrdQty8 WITH IIF(EMPTY(cSelect8) OR (!EMPTY(cSelect8) AND EMPTY(CtnQty8)),OrdQty8,MAX(OrdQty8,OrgPQty8+(CtnQty8*(lnTo-lnFrom+1))))
					*B603121 (End)
				ENDIF

				REPLACE OQty1 WITH IIF(EMPTY(cSelect1) AND EMPTY(CtnQty1),&lcPckLin..OQty1,MAX(0,OQty1-(&lcPckLin..CtnQty1*(lnTo-lnFrom+1)))),;
					OQty2 WITH IIF(EMPTY(cSelect2) AND EMPTY(CtnQty2),&lcPckLin..OQty2,MAX(0,OQty2-(&lcPckLin..CtnQty2*(lnTo-lnFrom+1)))),;
					OQty3 WITH IIF(EMPTY(cSelect3) AND EMPTY(CtnQty3),&lcPckLin..OQty3,MAX(0,OQty3-(&lcPckLin..CtnQty3*(lnTo-lnFrom+1)))),;
					OQty4 WITH IIF(EMPTY(cSelect4) AND EMPTY(CtnQty4),&lcPckLin..OQty4,MAX(0,OQty4-(&lcPckLin..CtnQty4*(lnTo-lnFrom+1)))),;
					OQty5 WITH IIF(EMPTY(cSelect5) AND EMPTY(CtnQty5),&lcPckLin..OQty5,MAX(0,OQty5-(&lcPckLin..CtnQty5*(lnTo-lnFrom+1)))),;
					OQty6 WITH IIF(EMPTY(cSelect6) AND EMPTY(CtnQty6),&lcPckLin..OQty6,MAX(0,OQty6-(&lcPckLin..CtnQty6*(lnTo-lnFrom+1)))),;
					OQty7 WITH IIF(EMPTY(cSelect7) AND EMPTY(CtnQty7),&lcPckLin..OQty7,MAX(0,OQty7-(&lcPckLin..CtnQty7*(lnTo-lnFrom+1)))),;
					OQty8 WITH IIF(EMPTY(cSelect8) AND EMPTY(CtnQty8),&lcPckLin..OQty8,MAX(0,OQty8-(&lcPckLin..CtnQty8*(lnTo-lnFrom+1))))

				REPLACE PQty1 WITH IIF(EMPTY(cSelect1) AND EMPTY(CtnQty1),&lcPckLin..PQty1,PQty1+(&lcPckLin..CtnQty1*(lnTo-lnFrom+1))),;
					PQty2 WITH IIF(EMPTY(cSelect2) AND EMPTY(CtnQty2),&lcPckLin..PQty2,PQty2+(&lcPckLin..CtnQty2*(lnTo-lnFrom+1))),;
					PQty3 WITH IIF(EMPTY(cSelect3) AND EMPTY(CtnQty3),&lcPckLin..PQty3,PQty3+(&lcPckLin..CtnQty3*(lnTo-lnFrom+1))),;
					PQty4 WITH IIF(EMPTY(cSelect4) AND EMPTY(CtnQty4),&lcPckLin..PQty4,PQty4+(&lcPckLin..CtnQty4*(lnTo-lnFrom+1))),;
					PQty5 WITH IIF(EMPTY(cSelect5) AND EMPTY(CtnQty5),&lcPckLin..PQty5,PQty5+(&lcPckLin..CtnQty5*(lnTo-lnFrom+1))),;
					PQty6 WITH IIF(EMPTY(cSelect6) AND EMPTY(CtnQty6),&lcPckLin..PQty6,PQty6+(&lcPckLin..CtnQty6*(lnTo-lnFrom+1))),;
					PQty7 WITH IIF(EMPTY(cSelect7) AND EMPTY(CtnQty7),&lcPckLin..PQty7,PQty7+(&lcPckLin..CtnQty7*(lnTo-lnFrom+1))),;
					PQty8 WITH IIF(EMPTY(cSelect8) AND EMPTY(CtnQty8),&lcPckLin..PQty8,PQty8+(&lcPckLin..CtnQty8*(lnTo-lnFrom+1)))

				REPLACE PWgh1 WITH IIF(EMPTY(cSelect1) AND EMPTY(CtnQty1),&lcPckLin..PWgh1,PWgh1 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty1*(lnTo-lnFrom+1))),;
					PWgh2 WITH IIF(EMPTY(cSelect2) AND EMPTY(CtnQty2),&lcPckLin..PWgh2,PWgh2 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty2*(lnTo-lnFrom+1))),;
					PWgh3 WITH IIF(EMPTY(cSelect3) AND EMPTY(CtnQty3),&lcPckLin..PWgh3,PWgh3 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty3*(lnTo-lnFrom+1))),;
					PWgh4 WITH IIF(EMPTY(cSelect4) AND EMPTY(CtnQty4),&lcPckLin..PWgh4,PWgh4 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty4*(lnTo-lnFrom+1)))
				REPLACE PWgh5 WITH IIF(EMPTY(cSelect5) AND EMPTY(CtnQty5),&lcPckLin..PWgh5,PWgh5 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty5*(lnTo-lnFrom+1))),;
					PWgh6 WITH IIF(EMPTY(cSelect6) AND EMPTY(CtnQty6),&lcPckLin..PWgh6,PWgh6 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty6*(lnTo-lnFrom+1))),;
					PWgh7 WITH IIF(EMPTY(cSelect7) AND EMPTY(CtnQty7),&lcPckLin..PWgh7,PWgh7 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty7*(lnTo-lnFrom+1))),;
					PWgh8 WITH IIF(EMPTY(cSelect8) AND EMPTY(CtnQty8),&lcPckLin..PWgh8,PWgh8 + &lcPckLin..StyWgh*(&lcPckLin..CtnQty8*(lnTo-lnFrom+1)))

				STORE 0 TO lnQuantities,lnWeights
				FOR lnJ = 0 TO (lnTo-lnFrom)
					lnQuantities = &lcPckLin..CtnQty1+&lcPckLin..CtnQty2+;
						&lcPckLin..CtnQty3+&lcPckLin..CtnQty4+;
						&lcPckLin..CtnQty5+&lcPckLin..CtnQty6+;
						&lcPckLin..CtnQty7+&lcPckLin..CtnQty8

					lnWeights   = &lcPckLin..StyWgh*&lcPckLin..CtnQty1+;
						&lcPckLin..StyWgh*&lcPckLin..CtnQty2+;
						&lcPckLin..StyWgh*&lcPckLin..CtnQty3+;
						&lcPckLin..StyWgh*&lcPckLin..CtnQty4+;
						&lcPckLin..StyWgh*&lcPckLin..CtnQty5+;
						&lcPckLin..StyWgh*&lcPckLin..CtnQty6+;
						&lcPckLin..StyWgh*&lcPckLin..CtnQty7+;
						&lcPckLin..StyWgh*&lcPckLin..CtnQty8
					IF lnQuantities > 0
						IF SEEK(STR(lnFrom+lnJ,4),lcCtnHdr)
							SELECT (lcCtnHdr)
							REPLACE TotPcs WITH TotPcs + lnQuantities,;
								TotWgh WITH TotWgh + lnWeights

							lnPackQty = lnPackQty + lnQuantities
							lnPackWgh = lnPackWgh + lnWeights

							SELECT(lcPckLin)
						ELSE
							INSERT INTO (lcCtnHdr)(Cart_No   ,Pal_No  ,TotPcs      ,TotWgh   ,EMPTY);
								VALUES (lnFrom+lnJ,lnCtnPal,lnQuantities,lnWeights,'N'  )
							lnMaxCtn  = MAX(lnMaxCtn,lnFrom+lnJ)
							lnPackWgh = lnPackWgh + &lcCtnHdr..TotWgh
							lnPackCtn = lnPackCtn + 1
							lnPackQty = lnPackQty + &lcCtnHdr..TotPcs
						ENDIF
						lnMaxPal  = MAX(lnMaxPal,&lcCtnHdr..Pal_No)
						lnMinPal  = MIN(IIF(lnMinPal=0,&lcCtnHdr..Pal_No,lnMinPal),&lcCtnHdr..Pal_No)
					ENDIF

					SELECT (lcCtnDtl)
					IF !SEEK(STR(lnFrom+lnJ,4)+&lcPckLin..Style+STR(&lcPckLin..nOrdLineNo,6),lcCtnDtl)
						APPEND BLANK
					ENDIF
					REPLACE STYLE      WITH &lcPckLin..Style,;
						OrgWgh     WITH &lcPckLin..OrgStyWgh,;
						nOrdLineNo WITH &lcPckLin..nOrdLineNo,;
						SzCnt      WITH &lcPckLin..SzCnt,;
						cStatus    WITH "A",;
						Cart_No    WITH lnFrom+lnJ
					REPLACE Size1      WITH IIF(!EMPTY(&lcPckLin..cSelect1) AND &lcPckLin..CtnQty1>0,&lcPckLin..cSize1,Size1),;
						Size2      WITH IIF(!EMPTY(&lcPckLin..cSelect2) AND &lcPckLin..CtnQty2>0,&lcPckLin..cSize2,Size2),;
						Size3      WITH IIF(!EMPTY(&lcPckLin..cSelect3) AND &lcPckLin..CtnQty3>0,&lcPckLin..cSize3,Size3),;
						Size4      WITH IIF(!EMPTY(&lcPckLin..cSelect4) AND &lcPckLin..CtnQty4>0,&lcPckLin..cSize4,Size4),;
						Size5      WITH IIF(!EMPTY(&lcPckLin..cSelect5) AND &lcPckLin..CtnQty5>0,&lcPckLin..cSize5,Size5),;
						Size6      WITH IIF(!EMPTY(&lcPckLin..cSelect6) AND &lcPckLin..CtnQty6>0,&lcPckLin..cSize6,Size6),;
						Size7      WITH IIF(!EMPTY(&lcPckLin..cSelect7) AND &lcPckLin..CtnQty7>0,&lcPckLin..cSize7,Size7),;
						Size8      WITH IIF(!EMPTY(&lcPckLin..cSelect8) AND &lcPckLin..CtnQty8>0,&lcPckLin..cSize8,Size8)
					REPLACE Br1        WITH !EMPTY(Size1),;
						Br2        WITH !EMPTY(Size2),;
						Br3        WITH !EMPTY(Size3),;
						Br4        WITH !EMPTY(Size4),;
						Br5        WITH !EMPTY(Size5),;
						Br6        WITH !EMPTY(Size6),;
						Br7        WITH !EMPTY(Size7),;
						Br8        WITH !EMPTY(Size8)
					REPLACE Qty1       WITH IIF(EMPTY(&lcPckLin..cSelect1) AND EMPTY(&lcPckLin..CtnQty1),Qty1,Qty1+&lcPckLin..CtnQty1),;
						Qty2       WITH IIF(EMPTY(&lcPckLin..cSelect2) AND EMPTY(&lcPckLin..CtnQty2),Qty2,Qty2+&lcPckLin..CtnQty2),;
						Qty3       WITH IIF(EMPTY(&lcPckLin..cSelect3) AND EMPTY(&lcPckLin..CtnQty3),Qty3,Qty3+&lcPckLin..CtnQty3),;
						Qty4       WITH IIF(EMPTY(&lcPckLin..cSelect4) AND EMPTY(&lcPckLin..CtnQty4),Qty4,Qty4+&lcPckLin..CtnQty4)
					REPLACE Qty5       WITH IIF(EMPTY(&lcPckLin..cSelect5) AND EMPTY(&lcPckLin..CtnQty5),Qty5,Qty5+&lcPckLin..CtnQty5),;
						Qty6       WITH IIF(EMPTY(&lcPckLin..cSelect6) AND EMPTY(&lcPckLin..CtnQty6),Qty6,Qty6+&lcPckLin..CtnQty6),;
						Qty7       WITH IIF(EMPTY(&lcPckLin..cSelect7) AND EMPTY(&lcPckLin..CtnQty7),Qty7,Qty7+&lcPckLin..CtnQty7),;
						Qty8       WITH IIF(EMPTY(&lcPckLin..cSelect8) AND EMPTY(&lcPckLin..CtnQty8),Qty8,Qty8+&lcPckLin..CtnQty8)
					REPLACE Weight1    WITH IIF(EMPTY(&lcPckLin..cSelect1) AND EMPTY(&lcPckLin..CtnQty1),Weight1,Weight1+&lcPckLin..StyWgh*(&lcPckLin..CtnQty1)),;
						Weight2    WITH IIF(EMPTY(&lcPckLin..cSelect2) AND EMPTY(&lcPckLin..CtnQty2),Weight2,Weight2+&lcPckLin..StyWgh*(&lcPckLin..CtnQty2)),;
						Weight3    WITH IIF(EMPTY(&lcPckLin..cSelect3) AND EMPTY(&lcPckLin..CtnQty3),Weight3,Weight3+&lcPckLin..StyWgh*(&lcPckLin..CtnQty3)),;
						Weight4    WITH IIF(EMPTY(&lcPckLin..cSelect4) AND EMPTY(&lcPckLin..CtnQty4),Weight4,Weight4+&lcPckLin..StyWgh*(&lcPckLin..CtnQty4))
					REPLACE Weight5    WITH IIF(EMPTY(&lcPckLin..cSelect5) AND EMPTY(&lcPckLin..CtnQty5),Weight5,Weight5+&lcPckLin..StyWgh*(&lcPckLin..CtnQty5)),;
						Weight6    WITH IIF(EMPTY(&lcPckLin..cSelect6) AND EMPTY(&lcPckLin..CtnQty6),Weight6,Weight6+&lcPckLin..StyWgh*(&lcPckLin..CtnQty6)),;
						Weight7    WITH IIF(EMPTY(&lcPckLin..cSelect7) AND EMPTY(&lcPckLin..CtnQty7),Weight7,Weight7+&lcPckLin..StyWgh*(&lcPckLin..CtnQty7)),;
						Weight8    WITH IIF(EMPTY(&lcPckLin..cSelect8) AND EMPTY(&lcPckLin..CtnQty8),Weight8,Weight8+&lcPckLin..StyWgh*(&lcPckLin..CtnQty8))
					REPLACE cStatus    WITH IIF(&lcPckLin..Selected>0,'M',cStatus)
					SELECT(lcPckLin)
				ENDFOR

				REPLACE CtnQty1  WITH IIF(!llClrSel AND !EMPTY(cSelect1),&lcPckLin..OQty1,0),;
					CtnQty2  WITH IIF(!llClrSel AND !EMPTY(cSelect2),&lcPckLin..OQty2,0),;
					CtnQty3  WITH IIF(!llClrSel AND !EMPTY(cSelect3),&lcPckLin..OQty3,0),;
					CtnQty4  WITH IIF(!llClrSel AND !EMPTY(cSelect4),&lcPckLin..OQty4,0),;
					CtnQty5  WITH IIF(!llClrSel AND !EMPTY(cSelect5),&lcPckLin..OQty5,0),;
					CtnQty6  WITH IIF(!llClrSel AND !EMPTY(cSelect6),&lcPckLin..OQty6,0),;
					CtnQty7  WITH IIF(!llClrSel AND !EMPTY(cSelect7),&lcPckLin..OQty7,0),;
					CtnQty8  WITH IIF(!llClrSel AND !EMPTY(cSelect8),&lcPckLin..OQty8,0)
				REPLACE cSelect1 WITH IIF(!llClrSel AND !EMPTY(cSelect1),'»',''),;
					cSelect2 WITH IIF(!llClrSel AND !EMPTY(cSelect2),'»',''),;
					cSelect3 WITH IIF(!llClrSel AND !EMPTY(cSelect3),'»',''),;
					cSelect4 WITH IIF(!llClrSel AND !EMPTY(cSelect4),'»',''),;
					cSelect5 WITH IIF(!llClrSel AND !EMPTY(cSelect5),'»',''),;
					cSelect6 WITH IIF(!llClrSel AND !EMPTY(cSelect6),'»',''),;
					cSelect7 WITH IIF(!llClrSel AND !EMPTY(cSelect7),'»',''),;
					cSelect8 WITH IIF(!llClrSel AND !EMPTY(cSelect8),'»','')

				*E302144,1 AMH Change the custom C102789 to be standard [Start]
				*C102789,4 ASH 02/15/2003 (Begin) Save Carton type in lcCtnHdr file from Apply button.
				*IF ASCAN(laEvntTrig , PADR('VOLAPP',10)) <> 0
				*  =gfDoTriger('ALPLIST',PADR('VOLAPP',10))
				*ENDIF
				*C102789,4 ASH 02/15/2003 (End)
				IF SEEK(STR(lnPackCtn,4),lcCtnHdr)
					REPLACE &lcCtnHdr..cCrtnVlTyp WITH &lcPckLin..cCrtnVlTyp
				ENDIF
				*E302144,1 AMH [End]

			ENDSCAN
		ENDIF
		lnOldFrm = lnFrom
		lnOldTo  = lnTo
		STORE lnPackCtn + 1 TO lnFrom,lnTo

		SHOW GET lnFrom
		SHOW GET lnTo

		IF llPalStat
			STORE lnMaxPal + 1 TO lnCtnPal
		ENDIF
	ENDIF
	SET ORDER TO lcTag IN (lcPckLin)
ENDIF
ENDIF
ENDIF
SELECT(lcPckLin)
=RLOCK(lcPckLin)
UNLOCK IN (lcPckLin)
SET RELATION TO '' INTO (lcScaFile) ADDITIVE
SET SKIP TO (lcScaFile)

IF lnCurRec <= RECCOUNT()
	GOTO lnCurRec
	SKIP lnSizeRec-1
ENDIF

SELECT (lcCtnHdr)
GOTO TOP
= RLOCK(lcCtnHdr)
UNLOCK IN (lcCtnHdr)

SELECT (lcCtnDtl)
GOTO TOP
= RLOCK(lcCtnDtl)
UNLOCK IN (lcCtnDtl)

*-- c101735,1 Print labels for all selected cartons [Begin]
IF llScrPrnLb
	PRIVATE lnDetRec , lnCrtRec , lnCarRef , lcCurrAlis

	*-- Save current setting
	lcCurrAlis = SELECT (0)
	lnDetRec   = RECNO(lcCtnDtl)
	lnCrtRec   = RECNO(lcCartonSz)
	*E302357,1 TMI [Start] save current filter on the lcCtnDtl file
	lcDetFlt = FILTER()
	SET FILTER TO 
	GO TOP
	*E302357,1 TMI [End  ] 

	lnCarRef = 0
	FOR lnCarRef = lnOldFrm TO lnOldTo
		=SEEK(STR(lnCarRef,4),lcCtnDtl) AND lfvLblInfo(lnCarRef,llScrPrnLb)
	ENDFOR
    
    *E302357,1 TMI [Start] restore current filter on lcCtnDtl temp file
    SELECT &lcCtnDtl
    SET FILTER TO &lcDetFlt
    GO TOP 
    *E302357,1 TMI [End  ] 
	*-- Restore Setting.
	IF BETWEEN(lnDetRec,1,RECCOUNT(lcCtnDtl))
		GO lnDetRec IN (lcCtnDtl)
	ENDIF

	IF BETWEEN(lnCrtRec,1,RECCOUNT(lcCartonSz))
		GO lnCrtRec IN (lcCartonSz)
	ENDIF

	SELECT (lcCurrAlis)

ENDIF
*-- c101735,1 Print labels for all selected cartons [End  ]

= lfwDtlBrow()
= lfRefresh(lcWinHdr)
SELECT (lnCurAlias)
*-- END OF lfvApply

*!*************************************************************
*! Name      : lfvSel
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate selection buttons pbsel,pbselAll,
*!             pbSelNo,pbInvert buttons
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfwDtlBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSel()
*!*************************************************************

FUNCTION lfvSel

PRIVATE lnCurRec,lcUnSelSty,lcSelFld,lcCtnQtyFld,lnSizeRec
lnCurAlias = SELECT(0)

*-- C102281 AAN Change the alias [Begin].
*SELECT (lcPckLin)
SELECT (lcTmpPck)
*-- C102281 [End].
lnCurRec = RECNO()
lnSizeRec = VAL(&lcScaFile..SzCode)
lcAllStat = "DISABLE"
lcNonStat = "DISABLE"
llcUpate = .T.

lcSelFld    = 'cSelect'+&lcScaFile..SzCode
lcCtnQtyFld = 'CtnQty'+&lcScaFile..SzCode
lcOQtyFld   = 'OQty'+&lcScaFile..SzCode

SET RELATION TO
= lfUpdate()

DO CASE
CASE _CUROBJ = OBJNUM(pbSel)

	IF EMPTY(&lcSelFld) AND LPicked AND EMPTY(laData[3])
		*-- This order line has been picked, can not be selected.
		*-- OK
		= gfModalGen("INM44041B00000","Dialog")
	ENDIF
	*B605592,1 HBG 24/02/2002 Fix bug of Apply button not working in case of select all then unselect one line[Begin]
	*REPLACE &lcSelFld    WITH IIF(&lcSelFld='»','',IIF(LPicked AND EMPTY(laData[3]),'','»'))         ,;
	&lcCtnQtyFld WITH IIF(&lcSelFld='»',IIF((lnTo-lnFrom+1)>0,INT(&lcOQtyFld/(lnTo-lnFrom+1)),0),0),;
	StyWgh       WITH IIF(&lcSelFld='',OrgStyWgh,StyWgh),;
	Selected     WITH IIF(&lcSelFld='»',Selected+1,MAX(Selected-1,0))
REPLACE &lcSelFld    WITH IIF(&lcSelFld='»','',IIF(LPicked AND EMPTY(laData[3]),'','»'))         ,;
	&lcCtnQtyFld WITH IIF(&lcSelFld='»',IIF((lnTo-lnFrom+1)>0,INT(&lcOQtyFld/(lnTo-lnFrom+1)),0),0),;
	StyWgh       WITH IIF(&lcSelFld='',OrgStyWgh,StyWgh)
REPLACE Selected WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
	cSelect3='»' OR cSelect4='»' OR ;
	cSelect5='»' OR cSelect6='»' OR ;
	cSelect7='»' OR cSelect8='»',   ;
	1,0)
*B605592,1 [End]
*-- This is to restore the original Style weight,Which means update
*-- field(StyWgh) with (OrgStyWgh) for the same style rest records and
*-- in case deselect the record
*-- C102281 AAN Change the alias [Begin].
*lcUnSelSty  = &lcPckLin..Style
lcUnSelSty  = &lcTmpPck..Style
*-- C102281 AAN [End].
*-- C102281 AAN Change the alias [Begin].
*IF EMPTY(&lcSelFld) AND SEEK(lcUnSelSty,lcPckLin)

IF EMPTY(&lcSelFld) AND SEEK(lcUnSelSty,lcTmpPck)
	*-- C102281 AAN [End].
	*-- C102281 AAN Change the alias [Begin].
	*SELECT(lcPckLin)
	*lnCurRec = RECNO(lcPckLin)
	SELECT(lcTmpPck)
	lnCurRec = RECNO(lcTmpPck)
	*-- C102281 AAN [End].
	REPLACE REST FOR STYLE+STR(nOrdLineNo,6) = lcUnSelSty;
		StyWgh WITH OrgStyWgh

ENDIF


CASE _CUROBJ = OBJNUM(pbSelAll)
REPLACE ALL cSelect1 WITH IIF(IIF(llShwOpn,AvlQty1>0 AND OQty1>0,AvlQty1>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect2 WITH IIF(IIF(llShwOpn,AvlQty2>0 AND OQty2>0,AvlQty2>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect3 WITH IIF(IIF(llShwOpn,AvlQty3>0 AND OQty3>0,AvlQty3>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect4 WITH IIF(IIF(llShwOpn,AvlQty4>0 AND OQty4>0,AvlQty4>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect5 WITH IIF(IIF(llShwOpn,AvlQty5>0 AND OQty5>0,AvlQty5>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect6 WITH IIF(IIF(llShwOpn,AvlQty6>0 AND OQty6>0,AvlQty6>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect7 WITH IIF(IIF(llShwOpn,AvlQty7>0 AND OQty7>0,AvlQty7>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect8 WITH IIF(IIF(llShwOpn,AvlQty8>0 AND OQty8>0,AvlQty8>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),'')
*B803777,1 HBG 11/14/2000 Calculate QTY/CTN field according to the carton range [Begin]
*REPLACE ALL CtnQty1  WITH IIF(cSelect1='»',OQty1,CtnQty1),;
CtnQty2  WITH IIF(cSelect2='»',OQty2,CtnQty2),;
CtnQty3  WITH IIF(cSelect3='»',OQty3,CtnQty3),;
CtnQty4  WITH IIF(cSelect4='»',OQty4,CtnQty4),;
CtnQty5  WITH IIF(cSelect5='»',OQty5,CtnQty5),;
CtnQty6  WITH IIF(cSelect6='»',OQty6,CtnQty6),;
CtnQty7  WITH IIF(cSelect7='»',OQty7,CtnQty7),;
CtnQty8  WITH IIF(cSelect8='»',OQty8,CtnQty8)
REPLACE ALL CtnQty1  WITH IIF(cSelect1='»',IIF((lnTo-lnFrom+1)>0,INT(OQty1/(lnTo-lnFrom+1)),0),0),;
	CtnQty2  WITH IIF(cSelect2='»',IIF((lnTo-lnFrom+1)>0,INT(OQty2/(lnTo-lnFrom+1)),0),0),;
	CtnQty3  WITH IIF(cSelect3='»',IIF((lnTo-lnFrom+1)>0,INT(OQty3/(lnTo-lnFrom+1)),0),0),;
	CtnQty4  WITH IIF(cSelect4='»',IIF((lnTo-lnFrom+1)>0,INT(OQty4/(lnTo-lnFrom+1)),0),0),;
	CtnQty5  WITH IIF(cSelect5='»',IIF((lnTo-lnFrom+1)>0,INT(OQty5/(lnTo-lnFrom+1)),0),0),;
	CtnQty6  WITH IIF(cSelect6='»',IIF((lnTo-lnFrom+1)>0,INT(OQty6/(lnTo-lnFrom+1)),0),0),;
	CtnQty7  WITH IIF(cSelect7='»',IIF((lnTo-lnFrom+1)>0,INT(OQty7/(lnTo-lnFrom+1)),0),0),;
	CtnQty8  WITH IIF(cSelect8='»',IIF((lnTo-lnFrom+1)>0,INT(OQty8/(lnTo-lnFrom+1)),0),0)
*B803777,1 [End]

REPLACE ALL Selected WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
	cSelect3='»' OR cSelect4='»' OR ;
	cSelect5='»' OR cSelect6='»' OR ;
	cSelect7='»' OR cSelect8='»',   ;
	1,0)

CASE _CUROBJ = OBJNUM(pbSelNo)
REPLACE ALL cSelect1 WITH '',;
	cSelect2 WITH '',;
	cSelect3 WITH '',;
	cSelect4 WITH '',;
	cSelect5 WITH '',;
	cSelect6 WITH '',;
	cSelect7 WITH '',;
	cSelect8 WITH '',;
	CtnQty1  WITH 0 ,;
	CtnQty2  WITH 0 ,;
	CtnQty3  WITH 0 ,;
	CtnQty4  WITH 0 ,;
	CtnQty5  WITH 0 ,;
	CtnQty6  WITH 0 ,;
	CtnQty7  WITH 0 ,;
	CtnQty8  WITH 0 ,;
	StyWgh   WITH OrgStyWgh,;
	Selected WITH 0

CASE _CUROBJ = OBJNUM(pbInvert)
REPLACE ALL cSelect1 WITH IIF(IIF(llShwOpn,AvlQty1>0 AND OQty1>0,AvlQty1>0),IIF(cSelect1='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect1),;
	cSelect2 WITH IIF(IIF(llShwOpn,AvlQty2>0 AND OQty2>0,AvlQty2>0),IIF(cSelect2='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect2),;
	cSelect3 WITH IIF(IIF(llShwOpn,AvlQty3>0 AND OQty3>0,AvlQty3>0),IIF(cSelect3='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect3),;
	cSelect4 WITH IIF(IIF(llShwOpn,AvlQty4>0 AND OQty4>0,AvlQty4>0),IIF(cSelect4='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect4),;
	cSelect5 WITH IIF(IIF(llShwOpn,AvlQty5>0 AND OQty5>0,AvlQty5>0),IIF(cSelect5='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect5),;
	cSelect6 WITH IIF(IIF(llShwOpn,AvlQty6>0 AND OQty6>0,AvlQty6>0),IIF(cSelect6='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect6),;
	cSelect7 WITH IIF(IIF(llShwOpn,AvlQty7>0 AND OQty7>0,AvlQty7>0),IIF(cSelect7='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect7),;
	cSelect8 WITH IIF(IIF(llShwOpn,AvlQty8>0 AND OQty8>0,AvlQty8>0),IIF(cSelect8='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect8)

REPLACE ALL CtnQty1  WITH IIF(cSelect1='»',IIF((lnTo-lnFrom+1)>0,INT(OQty1/(lnTo-lnFrom+1)),0),0),;
	CtnQty2  WITH IIF(cSelect2='»',IIF((lnTo-lnFrom+1)>0,INT(OQty2/(lnTo-lnFrom+1)),0),0),;
	CtnQty3  WITH IIF(cSelect3='»',IIF((lnTo-lnFrom+1)>0,INT(OQty3/(lnTo-lnFrom+1)),0),0),;
	CtnQty4  WITH IIF(cSelect4='»',IIF((lnTo-lnFrom+1)>0,INT(OQty4/(lnTo-lnFrom+1)),0),0),;
	CtnQty5  WITH IIF(cSelect5='»',IIF((lnTo-lnFrom+1)>0,INT(OQty5/(lnTo-lnFrom+1)),0),0),;
	CtnQty6  WITH IIF(cSelect6='»',IIF((lnTo-lnFrom+1)>0,INT(OQty6/(lnTo-lnFrom+1)),0),0),;
	CtnQty7  WITH IIF(cSelect7='»',IIF((lnTo-lnFrom+1)>0,INT(OQty7/(lnTo-lnFrom+1)),0),0),;
	CtnQty8  WITH IIF(cSelect8='»',IIF((lnTo-lnFrom+1)>0,INT(OQty8/(lnTo-lnFrom+1)),0),0)

REPLACE ALL StyWgh   WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
	cSelect3='»' OR cSelect4='»' OR ;
	cSelect5='»' OR cSelect6='»' OR ;
	cSelect7='»' OR cSelect8='»',   ;
	StyWgh,OrgStyWgh)

REPLACE ALL Selected WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
	cSelect3='»' OR cSelect4='»' OR ;
	cSelect5='»' OR cSelect6='»' OR ;
	cSelect7='»' OR cSelect8='»',   ;
	1,0)

*E301646,1 HBG 07/25/2001 Select Only this style in case of Select Style [Begin]
CASE _CUROBJ = OBJNUM(pbSelSty)

REPLACE cSelect1 WITH IIF(IIF(llShwOpn,AvlQty1>0 AND OQty1>0,AvlQty1>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect2 WITH IIF(IIF(llShwOpn,AvlQty2>0 AND OQty2>0,AvlQty2>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect3 WITH IIF(IIF(llShwOpn,AvlQty3>0 AND OQty3>0,AvlQty3>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect4 WITH IIF(IIF(llShwOpn,AvlQty4>0 AND OQty4>0,AvlQty4>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect5 WITH IIF(IIF(llShwOpn,AvlQty5>0 AND OQty5>0,AvlQty5>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect6 WITH IIF(IIF(llShwOpn,AvlQty6>0 AND OQty6>0,AvlQty6>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect7 WITH IIF(IIF(llShwOpn,AvlQty7>0 AND OQty7>0,AvlQty7>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),''),;
	cSelect8 WITH IIF(IIF(llShwOpn,AvlQty8>0 AND OQty8>0,AvlQty8>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),'')


REPLACE CtnQty1  WITH IIF(cSelect1='»',IIF((lnTo-lnFrom+1)>0,INT(OQty1/(lnTo-lnFrom+1)),0),0),;
	CtnQty2  WITH IIF(cSelect2='»',IIF((lnTo-lnFrom+1)>0,INT(OQty2/(lnTo-lnFrom+1)),0),0),;
	CtnQty3  WITH IIF(cSelect3='»',IIF((lnTo-lnFrom+1)>0,INT(OQty3/(lnTo-lnFrom+1)),0),0),;
	CtnQty4  WITH IIF(cSelect4='»',IIF((lnTo-lnFrom+1)>0,INT(OQty4/(lnTo-lnFrom+1)),0),0),;
	CtnQty5  WITH IIF(cSelect5='»',IIF((lnTo-lnFrom+1)>0,INT(OQty5/(lnTo-lnFrom+1)),0),0),;
	CtnQty6  WITH IIF(cSelect6='»',IIF((lnTo-lnFrom+1)>0,INT(OQty6/(lnTo-lnFrom+1)),0),0),;
	CtnQty7  WITH IIF(cSelect7='»',IIF((lnTo-lnFrom+1)>0,INT(OQty7/(lnTo-lnFrom+1)),0),0),;
	CtnQty8  WITH IIF(cSelect8='»',IIF((lnTo-lnFrom+1)>0,INT(OQty8/(lnTo-lnFrom+1)),0),0)

REPLACE Selected WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
	cSelect3='»' OR cSelect4='»' OR ;
	cSelect5='»' OR cSelect6='»' OR ;
	cSelect7='»' OR cSelect8='»',   ;
	1,0)
*E301646,1 [End]

ENDCASE

*-- C102281 AAN Change the alias [Begin].
*= RLOCK(lcPckLin)
*UNLOCk IN (lcPckLin)
= RLOCK(lcTmpPck)
UNLOCK IN (lcTmpPck)
*-- C102281 AAN [End].

SET RELATION TO '' INTO (lcScaFile) ADDITIVE
SET SKIP TO (lcScaFile)

IF lnCurRec <= RECCOUNT()
	GOTO lnCurRec
	SKIP lnSizeRec-1
ENDIF

*-- C102281 AAN Change the alias [Begin].
*= RLOCK(lcPckLin)
*UNLOCk IN (lcPckLin)
= RLOCK(lcTmpPck)
UNLOCK IN (lcTmpPck)
*-- C102281 AAN Change the alias [Begin].

= lfwDtlBrow()

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCtnRng
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate lnFrom and lnTo fields which validate carton range
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfwDtlBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtnRng()
*!*************************************************************

FUNCTION lfvCtnRng
PRIVATE lnCurAlias,lcCurTag,lnCurRec,lcOQtyFld,lnSizeRec

lnCurAlias = SELECT(0)

*B605583,1 HBG 24/02/2002 Select the file which browse from[Begin]
*SELECT(lcPckLin)
SELECT(lcTmpPck)
*B605583,1 [End]

IF lnFrom<=0 OR lnTo<=0 OR lnFrom > lnTo
	*-- You have to enter correct carton range.
	*-- <OK>
	= gfModalGen("INM44033B00000","Dialog")
	_CUROBJ = _CUROBJ
	lcObjName = VARREAD()
	&lcOBjName = lcOldVal
	SHOW GET &lcOBjName
ELSE
	IF (lcOldVal # EVAL(VARREAD()) AND lnTo-lnFrom+1>=0)
		SET RELATION TO
		*B605583,1 HBG 24/02/2002 Work on the file which browse from[Begin]
		*lcCurTag = ORDER(lcPckLin)
		*SET ORDER TO Selected IN (lcPckLin)
		*lnCurRec = RECNO(lcPckLin)
		lcCurTag = ORDER(lcTmpPck)
		SET ORDER TO Selected IN (lcTmpPck)
		lnCurRec = RECNO(lcTmpPck)
		*B605583,1 [End]

		lnSizeRec = VAL(&lcScaFile..SzCode)
		*B605583,1 HBG 24/02/2002 Seek in the file which browse from[Begin]
		*IF SEEK ('Y',lcPckLin)
		IF SEEK ('Y',lcTmpPck)
			*B605583,1 [End]
			REPLACE REST CtnQty1 WITH IIF(cSelect1='»',INT(OQty1/(lnTo-lnFrom+1)),0),;
				CtnQty2 WITH IIF(cSelect2='»',INT(OQty2/(lnTo-lnFrom+1)),0),;
				CtnQty3 WITH IIF(cSelect3='»',INT(OQty3/(lnTo-lnFrom+1)),0),;
				CtnQty4 WITH IIF(cSelect4='»',INT(OQty4/(lnTo-lnFrom+1)),0),;
				CtnQty5 WITH IIF(cSelect5='»',INT(OQty5/(lnTo-lnFrom+1)),0),;
				CtnQty6 WITH IIF(cSelect6='»',INT(OQty6/(lnTo-lnFrom+1)),0),;
				CtnQty7 WITH IIF(cSelect7='»',INT(OQty7/(lnTo-lnFrom+1)),0),;
				CtnQty8 WITH IIF(cSelect8='»',INT(OQty8/(lnTo-lnFrom+1)),0) ;
				WHILE Selected>0
		ENDIF

		*B605583,1 HBG 24/02/2002 Work on the file which browse from[Begin]
		*SET ORDER TO lcCurTag In (lcPckLin)
		SET ORDER TO lcCurTag IN (lcTmpPck)
		*B605583,1 [End]



		SET RELATION TO '' INTO (lcScaFile) ADDITIVE
		SET SKIP TO (lcScaFile)
		*B605583,1 HBG 24/02/2002 Work on the file which browse from[Begin]
		*IF lnCurRec<=RECCOUNT(lcPckLin)
		IF lnCurRec<=RECCOUNT(lcTmpPck)
			*B605583,1 [End]
			GOTO lnCurRec
			SKIP lnSizeRec-1
		ENDIF
	ENDIF
ENDIF

= RLOCK(lcPckLin)
UNLOCK IN (lcPckLin)

= lfwDtlBrow()

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lpClrSel
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate BAR 1 of PAD _OPTIONS OF _MSYSMENU
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   :  DO lpClrSel
*!*************************************************************

PROCEDURE lpClrSel

*-- this means if it is not clear selection upon apply invers this to be
*-- cleared upon apply and vice vresa is coorect

IF !llClrSel
	llClrSel = .T.
	SET MARK OF BAR 1 OF _OPTPOP TO .T.
ELSE
	llClrSel = .F.
	SET MARK OF BAR 1 OF _OPTPOP TO .F.
ENDIF

*!*************************************************************
*! Name      : lpShwOpn
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate BAR 2 of PAD _OPTIONS OF _MSYSMENU
*!*************************************************************
*! Calls     :
*!             Procedures : ...
*!             Functions  : lfSelStatus,lfDtlBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   :  DO lpShwOpn
*!*************************************************************

PROCEDURE lpShwOpn

PRIVATE lnCurAlias,lcTag

lnCurAlias = SELECT(0)

*-- this to show only opened quantity or all quantity according to
*-- variable llShwOpn

SELECT (lcPckLin)
IF !llShwOpn
	llShwOpn = .T.
	SET MARK OF BAR 2 OF _OPTPOP TO .T.
ELSE
	llShwOpn = .F.
	SET MARK OF BAR 2 OF _OPTPOP TO .F.
ENDIF

SELECT (lcPckLin)
lcTag = ORDER(lcPckLin)
SET ORDER TO Opened IN (lcPckLin)

IF SEEK('Y',lcPckLin)
	llAnyRec = .T.
ELSE
	llAnyRec = .F.
ENDIF
SET ORDER TO lcTag IN (lcPckLin)

= lfDtlBrow()

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfSelStatus
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : adjust the status of selection buttons
*!*************************************************************
*! Calls     :
*!             Procedures : ...
*!             Functions  : lfSelStatus,lfDtlBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfSelStatus()
*!*************************************************************

FUNCTION lfSelStatus
PRIVATE lnCurAlias,lnCurRec

lnCurAlias = SELECT(0)

SELECT (lcPckLin)
lnCurRec = RECNO()
lnSizeRec = VAL(&lcScaFile..SzCode)
IF !llAnyRec
	lcDtl2Stat = "DISABLE"
ELSE
	lcDtl2Stat = IIF((laScrMode[3] OR llNew) AND lnActFolder = 2,"ENABLE","DISABLE")
ENDIF

SHOW GETS WINDOW (lcWinDtl2) &lcDtl2Stat ONLY
*E301646,1 HBG 07/25/2001 Enable the Button Select Style if setting Pack Oer Size = 'NO'  [Begin]
IF llPakPrSiz
	SHOW GET pbSelSty DISABLE
ELSE
	SHOW GET pbSelSty &lcDtl2Stat
ENDIF
*E301646,1 [End]

IF lnActFolder = 2
	SHOW GET ibPckLin ENABLE
ENDIF




SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To make local save.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpSavscr
*!*************************************************************

PROCEDURE lpSavscr

*-- lnBookQty,lnBookAmt these variables are to update book,bookamt fields in
*-- ordhdr file
PRIVATE llReturn,lnCurAlias,lcPckHdTag,lnCount,lnBookQty,lnBookAmt,;
	lnOpenQty,lnOpenAmt

*--E301077,68 (Start)
PRIVATE llStyDyOpn

*ARD INITIAT llStyDyOpn by default value
llStyDyOpn = .F.

*--E301077,68 (End)

STORE 0 TO lnBookQty,lnBookAmt,lnRelCho,lnOpenQty,lnOpenAmt
lnCount = 0
lnCurAlias = SELECT(0)

llNoThing = lfUpdVars()
llNoThing = lfUpdUnCmS("Open", VARREAD() )

lcDelStat = SET("DELETED")

*B603957,1 Define Variables hold differences in ordhdr [Begin]
*B802541 (Start)
*PRIVATE lnOldOQty,lnOldOAmt
*STORE 0 TO lnOldOQty,lnOldOAmt
*B802541 (End)
PRIVATE lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif , lnOrgBook , lnOrgQty
STORE 0 TO lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif, lnOrgBook , lnOrgQty
*B603957,1 Define Variables hold differences in ordhdr [End  ]

*E301373,1 NAD (Start) Reset the filter before saving
SELECT (lcPcklin)
SET FILTER TO
*E301373,1 NAD (End)

SELECT (lcCtnDtl)
lcCtDtRel = SET("RELATION")
SET RELATION TO
SET FILTER TO
GOTO TOP
IF EOF() OR BOF() OR DELETED()
	*--No Packing list lines were applied. Cannot proceed!
	*-- <OK>
	llNoThing = gfModalGen("INM44035B00000","Dialog")
	llReturn = .F.
ELSE

	*badran 04/24/2000
	*---------------------------------------------------
	*-------- Check for availability of picking --------
	*-- IF user want to update pick quantity and does not have access to force allocation

	IF llUpdtPkTk
		*-- Open StyDye file before checking in it.
		=gfOpenFile(gcDataDir+"StyDye",gcDataDir+'StyDye','SH')
		*-- if no Sufficient quantity.
		IF lfNoSuffic()
			llCSave = .F.  && do not apply any saving code.
			RETURN
		ENDIF
	ENDIF
	*badran 04/24/2000


	*============================================================
	*This part for updating allocated quantities in style,stydye files
	*and picked quantities in ordline file if the created pack list is
	*made by piktkt
	IF !EMPTY(laData[3])
		SELECT(lcPckLin)
		lcTag = ORDER(lcPckLin)
		SET ORDER TO NoPacked
		IF SEEK('Y',lcPckLin)
			*-- "All unselected lines from the picking ticket will be released."
			*-- <Release> <Resume packing> <Save as is>
			IF lnRelCho = 0      && uncompelete session
				lnRelCho = gfModalGen("QRM44039B44006","Dialog")
			ENDIF
		ENDIF
		SET ORDER TO lcTag IN (lcPckLin)
		*C102239,1 (Begin) Custom : Update ALPAKINF file for Bel05.
		IF ASCAN(laEvntTrig , PADR('UPDPACK',10)) <> 0
			=gfDoTriger('ALPLIST',PADR('UPDPACK',10))
		ENDIF
		*C102239,1 (End)
	ENDIF
	IF lnRelCho <> 2
		SELECT(lcPckLin)
		SCAN
			llNoThing  = SEEK('O'+laData[2]+laData[5]+&lcPckLin..Style+STR(nOrdLineNo,6),'OrdLine')
			IF llNoThing AND &lcPckLin..nStep = 0      && uncompelete session
				SELECT OrdLine
				*B605559,1 HBG 18/02/2002 Update Order line Qty with the Packed Qty from the temp pack line file[Begin]
				*REPLACE nPck1  WITH nPck1 - &lcPckLin..OrgPQty1 + &lcPckLin..PQty1,;
				nPck2  WITH nPck2 - &lcPckLin..OrgPQty2 + &lcPckLin..PQty2,;
				nPck3  WITH nPck3 - &lcPckLin..OrgPQty3 + &lcPckLin..PQty3,;
				nPck4  WITH nPck4 - &lcPckLin..OrgPQty4 + &lcPckLin..PQty4,;
				nPck5  WITH nPck5 - &lcPckLin..OrgPQty5 + &lcPckLin..PQty5,;
				nPck6  WITH nPck6 - &lcPckLin..OrgPQty6 + &lcPckLin..PQty6,;
				nPck7  WITH nPck7 - &lcPckLin..OrgPQty7 + &lcPckLin..PQty7,;
				nPck8  WITH nPck8 - &lcPckLin..OrgPQty8 + &lcPckLin..PQty8,;
				nPWght WITH nPWght- &lcPckLin..OrgpWgh  + ;
				&lcPckLin..PWgh1 + &lcPckLin..PWgh2 +;
				&lcPckLin..PWgh3 + &lcPckLin..PWgh4 +;
				&lcPckLin..PWgh5 + &lcPckLin..PWgh6 +;
				&lcPckLin..PWgh7 + &lcPckLin..PWgh8
			REPLACE nPck1  WITH &lcPckLin..PQty1,;
				nPck2  WITH &lcPckLin..PQty2,;
				nPck3  WITH &lcPckLin..PQty3,;
				nPck4  WITH &lcPckLin..PQty4,;
				nPck5  WITH &lcPckLin..PQty5,;
				nPck6  WITH &lcPckLin..PQty6,;
				nPck7  WITH &lcPckLin..PQty7,;
				nPck8  WITH &lcPckLin..PQty8,;
				nPWght WITH &lcPckLin..PWgh1 + &lcPckLin..PWgh2 +;
				&lcPckLin..PWgh3 + &lcPckLin..PWgh4 +;
				&lcPckLin..PWgh5 + &lcPckLin..PWgh6 +;
				&lcPckLin..PWgh7 + &lcPckLin..PWgh8
			*B605559,1 [End]
			*B602797,1 ubdate picked quantities fields in ordline file [Begin]

			IF llUpdtPkTk
				SELECT (lcPckLin)
				REPLACE nDiff1 WITH PQty1- OrdLine.Pik1 ,;
					nDiff2 WITH PQty2- OrdLine.Pik2 ,;
					nDiff3 WITH PQty3- OrdLine.Pik3 ,;
					nDiff4 WITH PQty4- OrdLine.Pik4 ,;
					nDiff5 WITH PQty5- OrdLine.Pik5 ,;
					nDiff6 WITH PQty6- OrdLine.Pik6 ,;
					nDiff7 WITH PQty7- OrdLine.Pik7 ,;
					nDiff8 WITH PQty8- OrdLine.Pik8
				SELECT OrdLine
				REPLACE Pik1  WITH nPck1,;
					Pik2  WITH nPck2,;
					Pik3  WITH nPck3,;
					Pik4  WITH nPck4,;
					Pik5  WITH nPck5,;
					Pik6  WITH nPck6,;
					Pik7  WITH nPck7,;
					Pik8  WITH nPck8,;
					TotPik  WITH Pik1 + Pik2 + Pik3 + Pik4 +Pik5 + Pik6 + Pik7 + Pik8
			ENDIF
			*B602797,1 [End]

			*-- If we are packing from piktkt
			IF !EMPTY(laData[3])
				*-- update pik fields in ordline file only
				*-- If user option release or (save as is and the line is selected)
				*-- which means if it is unselected line and "save as is" left the
				*-- pik quantities as it is.
				IF lnRelCho = 1 OR ;
						(lnRelCho = 3  AND ;
						!( EMPTY(&lcPckLin..cSelect1) OR EMPTY(&lcPckLin..cSelect2) OR ;
						EMPTY(&lcPckLin..cSelect3) OR EMPTY(&lcPckLin..cSelect4) OR ;
						EMPTY(&lcPckLin..cSelect5) OR EMPTY(&lcPckLin..cSelect6) OR ;
						EMPTY(&lcPckLin..cSelect7) OR EMPTY(&lcPckLin..cSelect8) ) )
					REPLACE Pik1   WITH &lcPckLin..PQty1,;
						Pik2   WITH &lcPckLin..PQty2,;
						Pik3   WITH &lcPckLin..PQty3,;
						Pik4   WITH &lcPckLin..PQty4,;
						Pik5   WITH &lcPckLin..PQty5,;
						Pik6   WITH &lcPckLin..PQty6,;
						Pik7   WITH &lcPckLin..PQty7,;
						Pik8   WITH &lcPckLin..PQty8,;
						TotPik WITH &lcPckLin..PQty1+&lcPckLin..PQty2+;
						&lcPckLin..PQty3+&lcPckLin..PQty4+;
						&lcPckLin..PQty5+&lcPckLin..PQty6+;
						&lcPckLin..PQty7+&lcPckLin..PQty8 ;
						PikTkt  WITH IIF(TotPik=0,'',PikTkt),;
						PikDate WITH IIF(TotPik=0,{},PikDate),;
						Picked  WITH IIF(TotPik=0,.F.,Picked)
				ENDIF
			ENDIF
			*C102239,1 (Begin) Custom : Update ALPAKINF file for Bel05.
			IF ASCAN(laEvntTrig , PADR('UPDTOT',10)) <> 0
				=gfDoTriger('ALPLIST',PADR('UPDTOT',10))
			ENDIF
			*C102239,1 (End)
			SELECT (lcPckLin)
			REPLACE &lcPckLin..nStep WITH 1
		ENDIF

		*--  updating the style file.
		IF &lcPckLin..nStep = 1      && uncompelete session
			SELECT STYLE
			*B602797,1 Put the modification in allocated style under the option [Begin]
			*B602797,1 Comment out the following if condition and enable it
			*B602797,1 if user want to update pick quanties only.
			*IF !EMPTY(laData[3]) AND SEEK(&lcPckLin..Style,'Style')

			IF llUpdtPkTk AND !EMPTY(laData[3]) AND SEEK(&lcPckLin..Style,'Style')
				*B602797,1 Put the modification in allocated style under the option [End  ]
				*REPLACE Alo1   WITH Alo1 - OrdLine.Pik1 + &lcPckLin..PQty1,;
				Alo2   WITH Alo2 - OrdLine.Pik2 + &lcPckLin..PQty2,;
				Alo3   WITH Alo3 - OrdLine.Pik3 + &lcPckLin..PQty3,;
				Alo4   WITH Alo4 - OrdLine.Pik4 + &lcPckLin..PQty4,;
				Alo5   WITH Alo5 - OrdLine.Pik5 + &lcPckLin..PQty5,;
				Alo6   WITH Alo6 - OrdLine.Pik6 + &lcPckLin..PQty6,;
				Alo7   WITH Alo7 - OrdLine.Pik7 + &lcPckLin..PQty7,;
				Alo8   WITH Alo8 - OrdLine.Pik8 + &lcPckLin..PQty8,;
				TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
			REPLACE Alo1   WITH Alo1 + &lcPckLin..nDiff1,;
				Alo2   WITH Alo2 + &lcPckLin..nDiff2,;
				Alo3   WITH Alo3 + &lcPckLin..nDiff3,;
				Alo4   WITH Alo4 + &lcPckLin..nDiff4,;
				Alo5   WITH Alo5 + &lcPckLin..nDiff5,;
				Alo6   WITH Alo6 + &lcPckLin..nDiff6,;
				Alo7   WITH Alo7 + &lcPckLin..nDiff7,;
				Alo8   WITH Alo8 + &lcPckLin..nDiff8,;
				TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8

		ENDIF

		*-- updaing ord fields in style file in all cases not only piktkt
		REPLACE Ord1   WITH IIF(&lcPckLin..PQty1>OrdLine.Qty1,Ord1+(&lcPckLin..PQty1-OrdLine.Qty1),Ord1),;
			Ord2   WITH IIF(&lcPckLin..PQty2>OrdLine.Qty2,Ord2+(&lcPckLin..PQty2-OrdLine.Qty2),Ord2),;
			Ord3   WITH IIF(&lcPckLin..PQty3>OrdLine.Qty3,Ord3+(&lcPckLin..PQty3-OrdLine.Qty3),Ord3),;
			Ord4   WITH IIF(&lcPckLin..PQty4>OrdLine.Qty4,Ord4+(&lcPckLin..PQty4-OrdLine.Qty4),Ord4),;
			Ord5   WITH IIF(&lcPckLin..PQty5>OrdLine.Qty5,Ord5+(&lcPckLin..PQty5-OrdLine.Qty5),Ord5),;
			Ord6   WITH IIF(&lcPckLin..PQty6>OrdLine.Qty6,Ord6+(&lcPckLin..PQty6-OrdLine.Qty6),Ord6),;
			Ord7   WITH IIF(&lcPckLin..PQty7>OrdLine.Qty7,Ord7+(&lcPckLin..PQty7-OrdLine.Qty7),Ord7),;
			Ord8   WITH IIF(&lcPckLin..PQty8>OrdLine.Qty8,Ord8+(&lcPckLin..PQty8-OrdLine.Qty8),Ord8),;
			TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
		SELECT (lcPckLin)
		REPLACE &lcPckLin..nStep WITH 2
	ENDIF

	*--E301077,68 (Start)
	llStyDyOpn = gfOpenFile(gcDataDir+"StyDye",gcDataDir+'StyDye','SH')

	*--  updating the stydye file.
	IF &lcPckLin..nStep = 2      && uncompelete session
		IF !EMPTY(laData[3])
			IF SEEK (OrdLine.Style+lcWareCode,'StyDye')
				SELECT StyDye
				*B602797,1 The following update must be done if user want to update pick only [Begin]
				IF llUpdtPkTk
					*REPLACE Alo1 WITH Alo1 - OrdLine.Pik1 + &lcPckLin..PQty1,;
					Alo2 WITH Alo2 - OrdLine.Pik2 + &lcPckLin..PQty2,;
					Alo3 WITH Alo3 - OrdLine.Pik3 + &lcPckLin..PQty3,;
					Alo4 WITH Alo4 - OrdLine.Pik4 + &lcPckLin..PQty4,;
					Alo5 WITH Alo5 - OrdLine.Pik5 + &lcPckLin..PQty5,;
					Alo6 WITH Alo6 - OrdLine.Pik6 + &lcPckLin..PQty6,;
					Alo7 WITH Alo7 - OrdLine.Pik7 + &lcPckLin..PQty7,;
					Alo8 WITH Alo8 - OrdLine.Pik8 + &lcPckLin..PQty8

				REPLACE Alo1 WITH Alo1 + &lcPckLin..nDiff1,;
					Alo2 WITH Alo2 + &lcPckLin..nDiff2,;
					Alo3 WITH Alo3 + &lcPckLin..nDiff3,;
					Alo4 WITH Alo4 + &lcPckLin..nDiff4,;
					Alo5 WITH Alo5 + &lcPckLin..nDiff5,;
					Alo6 WITH Alo6 + &lcPckLin..nDiff6,;
					Alo7 WITH Alo7 + &lcPckLin..nDiff7,;
					Alo8 WITH Alo8 + &lcPckLin..nDiff8

				REPLACE StyDye.TotAlo WITH StyDye.Alo1+StyDye.Alo2+StyDye.Alo3+;
					StyDye.Alo4+StyDye.Alo5+StyDye.Alo6+;
					StyDye.Alo7+StyDye.Alo8

			ENDIF

			*-- Update ord quantities in stydye file.
			*B602797,1 updaing ord fields in style file in all cases not only piktkt [Begin]
			REPLACE Ord1 WITH Ord1 + MAX(&lcPckLin..PQty1-OrdLine.Qty1,0),;
				Ord2 WITH Ord2 + MAX(&lcPckLin..PQty2-OrdLine.Qty2,0),;
				Ord3 WITH Ord3 + MAX(&lcPckLin..PQty3-OrdLine.Qty3,0),;
				Ord4 WITH Ord4 + MAX(&lcPckLin..PQty4-OrdLine.Qty4,0),;
				Ord5 WITH Ord5 + MAX(&lcPckLin..PQty5-OrdLine.Qty5,0),;
				Ord6 WITH Ord6 + MAX(&lcPckLin..PQty6-OrdLine.Qty6,0),;
				Ord7 WITH Ord7 + MAX(&lcPckLin..PQty7-OrdLine.Qty7,0),;
				Ord8 WITH Ord8 + MAX(&lcPckLin..PQty8-OrdLine.Qty8,0),;
				TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
			*B602797,1 updaing ord fields in style file in all cases not only piktkt [End  ]

		ENDIF
	ENDIF
	SELECT (lcPckLin)
	REPLACE &lcPckLin..nStep WITH 3
ENDIF

IF !EMPTY(laData[3])
	IF &lcPckLin..nStep = 3      && uncompelete session
		IF llDyelot AND  Style.cDye_Flg = 'Y' AND ;
				SEEK(OrdLine.Style+lcWareCode+OrdLine.Dyelot,'StyDye')
			SELECT StyDye
			*B602797,1 The following update must be done if user want to update pick only [Begin]
			IF llUpdtPkTk
				*REPLACE Alo1 WITH Alo1 - OrdLine.Pik1 + &lcPckLin..PQty1,;
				Alo2 WITH Alo2 - OrdLine.Pik2 + &lcPckLin..PQty2,;
				Alo3 WITH Alo3 - OrdLine.Pik3 + &lcPckLin..PQty3,;
				Alo4 WITH Alo4 - OrdLine.Pik4 + &lcPckLin..PQty4,;
				Alo5 WITH Alo5 - OrdLine.Pik5 + &lcPckLin..PQty5,;
				Alo6 WITH Alo6 - OrdLine.Pik6 + &lcPckLin..PQty6,;
				Alo7 WITH Alo7 - OrdLine.Pik7 + &lcPckLin..PQty7,;
				Alo8 WITH Alo8 - OrdLine.Pik8 + &lcPckLin..PQty8
			REPLACE Alo1 WITH Alo1 + &lcPckLin..nDiff1,;
				Alo2 WITH Alo2 + &lcPckLin..nDiff2,;
				Alo3 WITH Alo3 + &lcPckLin..nDiff3,;
				Alo4 WITH Alo4 + &lcPckLin..nDiff4,;
				Alo5 WITH Alo5 + &lcPckLin..nDiff5,;
				Alo6 WITH Alo6 + &lcPckLin..nDiff6,;
				Alo7 WITH Alo7 + &lcPckLin..nDiff7,;
				Alo8 WITH Alo8 + &lcPckLin..nDiff8

			REPLACE StyDye.TotAlo WITH StyDye.Alo1+StyDye.Alo2+StyDye.Alo3+;
				StyDye.Alo4+StyDye.Alo5+StyDye.Alo6+;
				StyDye.Alo7+StyDye.Alo8

		ENDIF
		*B602797,1 updaing ord fields in style file in all cases not only piktkt [Begin]
		REPLACE Ord1 WITH Ord1 + MAX(&lcPckLin..PQty1-OrdLine.Qty1,0),;
			Ord2 WITH Ord2 + MAX(&lcPckLin..PQty2-OrdLine.Qty2,0),;
			Ord3 WITH Ord3 + MAX(&lcPckLin..PQty3-OrdLine.Qty3,0),;
			Ord4 WITH Ord4 + MAX(&lcPckLin..PQty4-OrdLine.Qty4,0),;
			Ord5 WITH Ord5 + MAX(&lcPckLin..PQty5-OrdLine.Qty5,0),;
			Ord6 WITH Ord6 + MAX(&lcPckLin..PQty6-OrdLine.Qty6,0),;
			Ord7 WITH Ord7 + MAX(&lcPckLin..PQty7-OrdLine.Qty7,0),;
			Ord8 WITH Ord8 + MAX(&lcPckLin..PQty8-OrdLine.Qty8,0),;
			TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
		*B602797,1 updaing ord fields in style file in all cases not only piktkt [End  ]
	ENDIF
	SELECT (lcPckLin)
	REPLACE &lcPckLin..nStep WITH 4
ENDIF
ENDIF
ENDSCAN
ENDIF
*--E301077,68 (Start)
IF llStyDyOpn
	= gfCloseFile("StyDye")
ENDIF
*--E301077,68 (End)

*===============end updating style,stydye files===================

*===============this part for updating pack_hdr,bol files=========
IF lnRelCho <> 2

	*-- This is to get the count of only cartons that have contents
	*-- and the last carton no in the pack also that has contents
	*-- Start
	SELECT (lcCtnHdr)
	lcCtHdInd = ORDER(lcCtnHdr)
	SET ORDER TO EMPTY IN (lcCtnHdr)
	IF SEEK ('Y',lcCtnHdr)
		lnRecNo = RECNO()
		SKIP -1
		lnLastCtn = &lcCtnHdr..Cart_No
		GO lnRecNo
		SCAN REST FOR EMPTY = 'Y'
			lnPackCtn = lnPackCtn - 1
		ENDSCAN
	ELSE
		GO BOTTOM
		lnLastCtn = &lcCtnHdr..Cart_No
	ENDIF
	SET ORDER TO lcCtHdInd IN (lcCtnHdr)
	*-- End getting last carton and carton count

	*B603515,1 Move open bol_hdr and bol_lin
	=gfOpenFile(gcDataDir+'Bol_Hdr',gcDataDir+'Bol_Hdr','SH')
	=gfOpenFile(gcDataDir+'Bol_Lin',gcDataDir+'Bol_Lin','SH')
	*B603515,1 (End)

	SELECT Pack_Hdr
	IF !SEEK(laData[1],'Pack_Hdr')
		*-- this part to Handle pack_no
		IF EMPTY(laData[3])
			*B802516,1 Create sequence# based on division
			*laData[1] = gfSequence('PIKTKT')
			=SEEK('O'+laData[2],'ORDHDR')
			laData[1] = gfSequence('PIKTKT', '', '', ORDHDR.cDivision)
			*B802516,1 (End)
			*B804003,1 HBG 11/02/2001 Check if the generated Pack_No found before or
			*B804003,1                not , to prevent repeating the same No. [Begin]
			DO WHILE SEEK(laData[1],'PACK_HDR')
				laData[1] = gfSequence('PIKTKT', '', '', ORDHDR.cDivision)
			ENDDO
			INSERT INTO PACK_HDR (PACK_NO) VALUES (laData[1])
			*B804003,1 [End]
			*-- Packing slip saved with # laData[1]
			*-- <OK>
			llNoThing = gfModalGen("INM44036B00000","Dialog",laData[1])
		ELSE
			laData[1] = laData[3]
			*B804003,1 HBG 11/02/200 Save PACK_NO as soon as it generated [Begin]
			INSERT INTO PACK_HDR (PACK_NO) VALUES (laData[1])
			*B804003,1 [End]
		ENDIF
		*=============This part for BOL=================
		*-- This part will be executed only if the system has ASN Module

		*B803177,1 HBG 07/24/2000  Comment If statment because This blok is not used any more [Begin]
		*IF llEdiSys

		*--E301077 (Start)
		*llB_H_Opn = gfOpenFile(gcDataDir+'Bol_Hdr',gcDataDir+'Bol_Hdr','SH')
		*llB_D_Opn = gfOpenFile(gcDataDir+'Bol_Lin',gcDataDir+'Bol_Lin','SH')

		*B603515,1 Commented out
		**C101735,1 Open BOL files if not opened only [Begin]
		*llB_H_Opn = !llB_H_Opn AND gfOpenFile(gcDataDir+'Bol_Hdr',gcDataDir+'Bol_Hdr','SH')
		*llB_D_Opn = !llB_D_Opn AND gfOpenFile(gcDataDir+'Bol_Lin',gcDataDir+'Bol_Lin','SH')
		**C101735,1 Open BOL files if not opened only [End  ]
		*--E301077 (End)

		*C101735,1 Get BOL only if not found [Begin]
		*IF EMPTY(lcBol)
		* *C101735,1 Get BOL only if not found [End  ]
		*   =SEEK(IIF(EMPTY(laData[5]),'M'+laData[4],'S'+laData[4]+laData[5]),'Customer')
		*  lcDistCtr  = Customer.Dist_Ctr
		*   SELECT BOL_HDR
		*   LOCATE FOR Account+Store+W_CODE=laData[4]+lcDistCtr+lcWareCode AND Status <> 'C'
		*   IF !FOUND()
		*     lcBol = gfSequence('BOL_NO')
		*     INSERT INTO BOL_HDR (BOL_NO,ACCOUNT  ,STORE    ,STANCARTON             ,BOLDATE     ,W_CODE );
		*                  VALUES (lcBol ,laData[4],lcDistCtr,IIF(laData[13],'Y','N'),gdSysDate,lcWareCode)
		*   ELSE
		*    lcBol = BOL_HDR.BOL_NO
		*   ENDIF
		* *C101735,1 Get BOL only if not found [Begin]
		* ENDIF
		*C101735,1 Get BOL only if not found [End  ]
		*B603515,1 (End)

		*B603515,1 Add BOL Lines
		*IF !SEEK(lcBol+laData[2]+laData[1],'BOL_LIN')

		*B803177,1 HBG 07/24/2000 This lines are moved out of the if condition of
		*B803177,1                "IF !SEEK(laData[1],'Pack_Hdr')" to fix the bug of saving
		*B803177,1                in bol_lin file only in the first time dealing with this
		*B803177,1                pack list [Begin]
		*IF !EMPTY(lcBol) AND !SEEK(lcBol+laData[2]+laData[1],'BOL_LIN')
		*B603515,1 (End)

		*  INSERT INTO BOL_LIN (BOL_NO,ORDER    ,PACK_NO  ) ;
		*               VALUES (lcBol ,laData[2],laData[1])
		*ENDIF
		*B803177,1 HBG [End]

		*C101735,1 Comment out the following block and gfCleanUp do the function [Begin]
		*--E301077 (Start)
		*IF llB_H_Opn
		*  = gfCloseFile('BOL_Hdr')
		*ENDIF
		*IF llB_D_Opn
		*  = gfCloseFile('BOL_Lin')
		*ENDIF
		*--E301077 (End)
		*C101735,1 Comment out the following block and gfCleanUp do the function [End  ]

		*ENDIF
		*B803177,1 HBG 07/24/2000  Comment If statment because This blok is not used any more [End  ]
		*===============end of BOL=========================
		SELECT Pack_Hdr
		*B804003,1 HBG 11/02/2001 comment it because we insert a new line
		*B804003,1                as soon as the Pack_no. generated [Begin]
		*APPEND BLANK
		*B804003,1 [End]
	ENDIF
	lnLastNo = Pack_Hdr.nLastLNo

	laData[08] = lnPackWgh
	laData[09] = lnPackCtn
	laData[10] = lnPackQty
	*B603807,1 HBG 08/09/2000 Get the value of "lstandctn" And "ctostorcn" fields [Begin]
	laData[13] = IIF(lnCtnTyp = 1,.T.,.F.)
	laData[14] = IIF(lnDrctTo = 1,'S','C')
	*B603807,1 [End]
	*B603515,1 Update BOL Header totals
	IF llEdiSys
		*B803177,1 HBG This lines were moved here to fix the bug of saving
		*B803177,1     in bol_lin file only in the first time dealing with this
		*B803177,1     pack list[Begin]
		IF !EMPTY(lcBol) AND !SEEK(lcBol+laData[2]+laData[1],'BOL_LIN')
			*B603515,1 (End)
			INSERT INTO BOL_LIN (BOL_NO,ORDER    ,PACK_NO  ) ;
				VALUES (lcBol ,laData[2],laData[1])
		ENDIF
		*B803177,1 HBG [End]
		*-- IF user change BOL then apply delete BOL code
		PRIVATE llTranBol

		llTranBol = .F.
		llTranBol =  (lcBol <> Pack_Hdr.Bill_Ladg) AND lfDelBol()

		*B803177,1 HBG 07/24/2000 Get the current BOL in BOL_HDR file [Begin]
		IF SEEK(lcBol,"BOL_HDR")
			*B803177,1 [End]
			SELECT BOL_HDR
			*B803177,1 HBG 07/24/2000 IF change Bol_no for this pack list add the new
			*B803177,1                quantites to the quantites of the new BOL[Begin]
			*WAIT WIND ALLTRIM(STR(laData[09])) + "laData[09]"
			IF llTranBol
				REPLACE TOT_WGHT WITH TOT_WGHT + laData[08] ,;
					TOT_CART WITH TOT_CART + laData[09] ,;
					TOT_PCS  WITH TOT_PCS  + laData[10]
			ELSE   && Update the quantites of the BOL
				REPLACE TOT_WGHT WITH TOT_WGHT- PACK_HDR.Tot_Wght + laData[08] ,;
					TOT_CART WITH TOT_CART- PACK_HDR.Tot_Cart + laData[09] ,;
					TOT_PCS  WITH TOT_PCS- PACK_HDR.Tot_Pcs   + laData[10]
			ENDIF
			*B803177,1 HBG 07/24/2000 End of IF change Bol_no for this pack list add the new
			*B803177,1                quantites to the quantites of the new BOL

			*B803177,1 HBG 07/24/2000  end of getting the current BOL in BOL_HDR file [Begin]
		ENDIF
		*B803177,1 [End]
		SELECT Pack_Hdr
	ENDIF
	*B603515,1 (End)

	GATHER FROM laData FIELDS &lcScFields

	REPLACE cWareCode WITH lcWareCode,;
		Bill_Ladg WITH lcBol,;
		cAdd_user WITH gcUser_Id,;
		dAdd_date WITH gdSysDate,;
		cAdd_time WITH gfGettime()

	*C102478,1 (Begin) Generate P/K.
	IF ASCAN(laEvntTrig , PADR('UPDPICK',10)) <> 0
		=gfDoTriger('ALPLIST',PADR('UPDPICK',10))
	ENDIF
	*C102478,1 (End)

ENDIF
*=================end of updating pack_hdr,BOL Files================

*=================This part for saving pack_lin file================
IF lnRelCho <> 2

    *E039957,1  MHM 11/14/2005 Update the new file EDICRTSQ , abd thenew field Ucc9 in EDIACPRT [Begin]
    *E039957,2  EIH 02/13/2006 Check if EDI installed or not [Begin] 
    IF llEdiSys 
    *E039957,2  EIH [End] 
      *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
      IF !llIncPLNum
      *E040123,1 HBG 03/13/2006 [End]
        lcSetDele = SET('DELETE')
        SET DELETE ON
        SET ORDER TO PCKCRTSQ IN EDICRTSQ
        IF SEEK(laData[1],'EDICRTSQ')
          SELECT EDICRTSQ      
          *B125541,1  TMI [Start] delete only cartons those are deleted from lcCtnHdr file, coded after updating pack_lin carton #s
          *DELETE FOR pack_no+STR(cart_no,6) = laData[1]
          *B125541,1  TMI [End  ] 
        ENDIF  
        SET DELETE &lcSetDele 
      *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
      ENDIF 
      *E040123,1 HBG 03/13/2006 [End]
    *E039957,2  EIH 02/13/2006 Check if EDI installed or not [Begin] 
    ENDIF
    *E039957,2  EIH [End] 
    *E039957,1  MHM 11/14/2005 [End]

	SET DELETED OFF
	SELECT(lcCtnDtl)
	SET FILTER TO
	SET ORDER TO 0
	= SEEK('O'+laData[2],'OrdHdr')

	SCAN FOR cStatus <> 'S'
		IF &lcCtnDtl..nStep = 0   && for uncomplete session
			SELECT Pack_Lin
			IF SEEK(laData[1]+STR(&lcCtnDtl..Cart_No,4)+&lcCtnDtl..Style,'Pack_Lin')
				IF DELETED('Pack_Lin')
					*B605592,1 HBG 24/02/2002 Get the correct line in Pack_line file[Begin]
					*LOCATE REST FOR pack_no+STR(no_cart,4)+style = ;
					laData[1]+STR(&lcCtnDtl..Cart_No,4)+;
					&lcCtnDtl..Style
				LOCATE REST FOR pack_no+STR(no_cart,4)+STYLE = ;
					laData[1]+STR(&lcCtnDtl..Cart_No,4)+;
					&lcCtnDtl..Style AND !DELETED()
				*B605592,1 [End]
			ENDIF

			*B603379,1 WAM 12/29/1999 (Start) Increased the width of the line no
			*B603379,1                field used in the packing list lines to be
			*B603379,1                6 instead of 3 digits not to have an error
			*B603379,1                when creating big packing lists
			*LOCATE REST FOR Pack_No+STR(Line_No,3)+Style+cPackColor = ;
			laData[1]+STR(&lcCtnDtl..PackLineNo,3)+;
			&lcCtnDtl..Style

		LOCATE REST FOR Pack_No+STR(Line_No,6)+STYLE+cPackColor = ;
			laData[1]+STR(&lcCtnDtl..PackLineNo,6)+&lcCtnDtl..Style
		*B603379,1 WAM 12/29/1999 (End)
	ENDIF
    
	IF !FOUND('Pack_Lin') AND !DELETED(lcCtnDtl)
		lnLastNo = lnLastNo + 1
		APPEND BLANK
		REPLACE Line_No WITH lnLastNo
		SELECT (lcCtnDtl)
		REPLACE PackLineNo WITH lnLastNo
	ENDIF
	*-- This seek for getting Pal_No
	llNothing = SEEK(STR(&lcCtnDtl..Cart_No,4),lcCtnHdr)
	SELECT Pack_Lin

	REPLACE Pack_No    WITH laData[1],;
		No_Cart    WITH &lcCtnDtl..Cart_No,;
		NPltNo     WITH &lcCtnHdr..Pal_No,;
		STYLE      WITH &lcCtnDtl..Style,;
		nOrdLineNo WITH &lcCtnDtl..nOrdLineNo,;
		Qty1       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty1,0),;
		Qty2       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty2,0),;
		Qty3       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty3,0),;
		Qty4       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty4,0),;
		Qty5       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty5,0),;
		Qty6       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty6,0),;
		Qty7       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty7,0),;
		Qty8       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty8,0),;
		TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8

	REPLACE Weight     WITH IIF(!DELETED(lcCtnDtl),;
		&lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
		&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
		&lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
		&lcCtnDtl..Weight7+&lcCtnDtl..Weight8 ;
		,MAX(Weight-&lcCtnDtl..Weight1;
		-&lcCtnDtl..Weight2;
		-&lcCtnDtl..Weight3;
		-&lcCtnDtl..Weight4;
		-&lcCtnDtl..Weight5;
		-&lcCtnDtl..Weight6;
		-&lcCtnDtl..Weight7;
		-&lcCtnDtl..Weight8;
		+(&lcCtnDtl..Qty1*&lcCtnDtl..OrgWgh);
		+(&lcCtnDtl..Qty2*&lcCtnDtl..OrgWgh);
		+(&lcCtnDtl..Qty3*&lcCtnDtl..OrgWgh);
		+(&lcCtnDtl..Qty4*&lcCtnDtl..OrgWgh);
		+(&lcCtnDtl..Qty5*&lcCtnDtl..OrgWgh);
		+(&lcCtnDtl..Qty6*&lcCtnDtl..OrgWgh);
		+(&lcCtnDtl..Qty7*&lcCtnDtl..OrgWgh);
		+(&lcCtnDtl..Qty8*&lcCtnDtl..OrgWgh),0))

	REPLACE cAdd_user WITH gcUser_Id,;
		dAdd_date WITH gdSysDate,;
		cAdd_time WITH gfGettime()

	IF Pack_Lin.TotQty = 0
		DELETE
	ENDIF
	SELECT (lcCtnDtl)
	REPLACE &lcCtnDtl..nStep WITH 1
ENDIF

*=================update ordline file ==========================
IF lnRelCho <> 2
	SELECT OrdLine
	*-- These 2 seek are to adjust the pointer in both files (lcpcklin,'OrdLine')
	=SEEK(&lcCtnDtl..Style+STR(&lcCtnDtl..nOrdLineNo,6),lcPckLin)
	=SEEK('O'+laData[2]+laData[5]+&lcCtnDtl..Style+STR(&lcCtnDtl..nOrdLineNo,6),'OrdLine')
	*B603957,1 Detect old Book and Open Quantities [Begin]
	lnOrgBook = ToTBook
	lnOrgQty  = TotQty
	*B603957,1 Detect old Book and Open Quantities [End  ]

	REPLACE Book1   WITH Book1+MAX(&lcPckLin..OrdQty1-OrdLine.Qty1,0),;
		Book2   WITH Book2+MAX(&lcPckLin..OrdQty2-OrdLine.Qty2,0),;
		Book3   WITH Book3+MAX(&lcPckLin..OrdQty3-OrdLine.Qty3,0),;
		Book4   WITH Book4+MAX(&lcPckLin..OrdQty4-OrdLine.Qty4,0),;
		Book5   WITH Book5+MAX(&lcPckLin..OrdQty5-OrdLine.Qty5,0),;
		Book6   WITH Book6+MAX(&lcPckLin..OrdQty6-OrdLine.Qty6,0),;
		Book7   WITH Book7+MAX(&lcPckLin..OrdQty7-OrdLine.Qty7,0),;
		Book8   WITH Book8+MAX(&lcPckLin..OrdQty8-OrdLine.Qty8,0),;
		ToTBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8

	lnBookQty = lnBookQty + ToTBook
	*B802541 (Start)
	lnBookAmt = lnBookAmt + (TotBook * Price)
	*lnOldOQty = lnOldOQty + TotQty
	*lnOldOAmt = lnOldOAmt + (TotQty * Price)
	*B802541 (End)

	REPLACE Qty1    WITH &lcPckLin..OrdQty1,;
		Qty2    WITH &lcPckLin..OrdQty2,;
		Qty3    WITH &lcPckLin..OrdQty3,;
		Qty4    WITH &lcPckLin..OrdQty4,;
		Qty5    WITH &lcPckLin..OrdQty5,;
		Qty6    WITH &lcPckLin..OrdQty6,;
		Qty7    WITH &lcPckLin..OrdQty7,;
		Qty8    WITH &lcPckLin..OrdQty8,;
		TotQty  WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8


	*B603957,1 Calculate differences per line and accomulate it [Begin]
	lnBookDiff = lnBookDiff + (TotBook - lnOrgBook)
	lnBkAmtDif = lnBkAmtDif + ((TotBook - lnOrgBook) * Price)
	lnQtyDiff  = lnQtyDiff  + (TotQty - lnOrgQty)
	lnQyAmtDif = lnQyAmtDif + ((TotQty - lnOrgQty) * Price)
	*B603957,1 Calculate differences per line and accomulate it [End  ]
ENDIF
*=================end updating ordline file======================

ENDSCAN

*B605559,1 HBG 18/02/2002 Renumber the cartons in PACK_LIN[Begin]
*B119166,1 ASH 09/02/2003 [Begin] Don't renumber the cartons if the user delete a carton in the edit mode.
IF laScrMode[4]
	*B119166,1 ASH 09/02/2003 [End]
	SELECT Pack_Lin
	lcSetDel = SET('DELETE')
	SET DELETE ON
	=SEEK(laData[1])
	*B605592,1 HBG 24/02/2002 Fix bug of creating 2 boxes while the packed carton is only one[Begin]
	*lnI = 1
	lnI = 0
	llFirst  = .T.
	lnLastCrt = Pack_Lin.No_Cart
	llSameCrt = .F.
	*B605592,1 [End]
	SCAN REST WHILE Pack_No+STR(No_Cart,4)+STYLE = laData[1]
		*B605592,1 HBG 24/02/2002 Fix bug of creating 2 boxes while the packed carton is only one[Begin]
		*REPLACE Pack_Lin.No_Cart WITH lnI
		*lnI = lnI + 1
		IF !llFirst
			llSameCrt = (Pack_Lin.No_Cart = lnLastCrt)
			IF !llSameCrt
				lnLastCrt = Pack_Lin.No_Cart
			ENDIF
		ENDIF
		IF llFirst OR !llSameCrt
			lnI = lnI + 1
		ENDIF
		REPLACE Pack_Lin.No_Cart WITH lnI
		llFirst  = .F.
		*B605592,1 [End]
	ENDSCAN
    *E039957,1  MHM 11/14/2005 Update the new file EDICRTSQ , and the new field Ucc9 in EDIACPRT [Begin]
    *E039957,2  EIH 02/13/2006 Check if EDI installed or not [Begin] 
    IF llEdiSys
    *E039957,2  EIH [End] 
      *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
      IF !llIncPLNum
      *E040123,1 HBG 03/13/2006 [End]
        IF SEEK(laData[1]+STR(lnI+1,6),'EDICRTSQ')
          SELECT EDICRTSQ
          DELETE REST WHILE pack_no+STR(cart_no,6) = laData[1]
        ENDIF
        SELECT Pack_Lin
        =SEEK(laData[1])
        SCAN REST WHILE Pack_No+STR(No_Cart,4)+Style = laData[1]
          SET ORDER TO PCKCRTSQ IN EDICRTSQ
          IF !SEEK(laData[1]+STR(Pack_Lin.No_Cart,6),'EDICRTSQ')
            *-- If this customer is a partner , get the last Ucc # from EDIACPRT file,
            IF SEEK('A'+laData[4],'EDIACPRT')
              lnUcc9  = IIF(EMPTY(EDIACPRT.Ucc9),0,EVAL(EDIACPRT.Ucc9)) + 1
              SET ORDER TO EDICRTSQ IN EDICRTSQ
              llFound = SEEK(laData[4]+PADL(lnUcc9,9,'0'),'EDICRTSQ')
              DO WHILE llFound 
                lnUcc9  = lnUcc9  + 1
                llFound = SEEK(laData[4]+PADL(lnUcc9,9,'0'),'EDICRTSQ')
              ENDDO
              INSERT INTO EDICRTSQ (Pack_No,Account,cart_no,Ucc9);
                           VALUE (laData[1],laData[4],Pack_Lin.No_Cart,PADL(lnUcc9,9,'0'))
              REPLACE EDIACPRT.Ucc9 WITH PADL(lnUcc9,9,'0')
            ELSE    && Get the last Ucc # from EDICRTSQ file.
              SELECT MAX(EDICRTSQ .ucc9) FROM EDICRTSQ WHERE EDICRTSQ.account = laData[4] INTO CURSOR lcMaxUcc
              SELECT lcMaxUcc
              LOCATE
              IF EOF()
                lcUcc9 = '000000001'
              ELSE
                lcUcc9 = PADL(EVAL(lcMaxUcc.MAX_UCC9)+1,9,'0')
              ENDIF  
              INSERT INTO EDICRTSQ (Pack_No,Account,cart_no,Ucc9);
                             VALUE (laData[1],laData[4],Pack_Lin.No_Cart,lcUcc9)
            ENDIF  
          ENDIF  
        ENDSCAN
      *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
      ENDIF
      *E040123,1 HBG 03/13/2006 [End]
    *E039957,2  EIH 02/13/2006 Check if EDI installed or not [Begin] 
    ENDIF
    *E039957,2  EIH [End] 
    *E039957,1  MHM 11/14/2005 [End]
	
	lnLastCtn = lnI
	SELECT (lcCtnDtl)

    *E039957,1  MHM 11/14/2005 Update the new file EDICRTSQ , and the new field Ucc9 in EDIACPRT [Begin]
    SET ORDER TO (lcCtnDtl) IN (lcCtnDtl)
    *E039957,1  MHM 11/14/2005 [END]

	GO TOP
	*B605592,1 HBG 24/02/2002 Fix bug of creating 2 boxes while the packed carton is only one[Begin]
	*lnI = 1
	lnI = 0
	llFirst  = .T.
	lnLastCrt = &lcCtnDtl..Cart_No
	llSameCrt = .F.
	*B605592,1 [End]
	SCAN
		*B605592,1 HBG 24/02/2002 Fix bug of creating 2 boxes while the packed carton is only one[Begin]
		*REPLACE &lcCtnDtl..Cart_No WITH lnI
		*lnI = lnI + 1
		IF !llFirst
			llSameCrt = (&lcCtnDtl..Cart_No = lnLastCrt)
			IF !llSameCrt
				lnLastCrt = &lcCtnDtl..Cart_No
			ENDIF
		ENDIF
		IF llFirst OR !llSameCrt
			lnI = lnI + 1
		ENDIF
        *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
        IF !llIncPLNum
        *E040123,1 HBG 03/13/2006 [End]
          *E039957,1  MHM 11/14/2005  [Start] Update the field &lcCtnHdr..Cart_No
          IF SEEK(STR(&lcCtnDtl..Cart_No,4) , lcCtnHdr )
            SELECT &lcCtnHdr
            REPLACE Cart_No WITH lnI
            SELECT &lcCtnDtl
          ENDIF
          *E039957,1  MHM 11/14/2005 TMI [End  ]  
        *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
        ENDIF
        *E040123,1 HBG 03/13/2006 [End]
		
	  	REPLACE &lcCtnDtl..Cart_No WITH lnI
		llFirst  = .F.
		*B605592,1 [End]
	ENDSCAN
	SET DELETE &lcSetDel
	*B119166,1 ASH 09/02/2003 [Begin] Don't renumber the cartons if the user delete a carton in the edit mode.
ENDIF
*B119166,1 ASH 09/02/2003 [End]
*B605559,1 [End]
*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
IF !llIncPLNum
*E040123,1 HBG 03/13/2006 [End]
  *E039957,1  MHM 11/14/2005 Update the new file EDICRTSQ , and the new field Ucc9 in EDIACPRT [Begin]
  IF laScrMode[3]
	lcSetDel = SET('DELETE')
	SET DELETE ON

	lnLastCtn = lnI
	SELECT (lcCtnDtl)

    SET ORDER TO (lcCtnDtl) IN (lcCtnDtl)

	GO TOP

	lnI = 0
	llFirst  = .T.
	lnLastCrt = &lcCtnDtl..Cart_No
	llSameCrt = .F.
	SCAN
		IF !llFirst
			llSameCrt = (&lcCtnDtl..Cart_No = lnLastCrt)
			IF !llSameCrt
				lnLastCrt = &lcCtnDtl..Cart_No
			ENDIF
		ENDIF
		IF llFirst OR !llSameCrt
			lnI = lnI + 1
		ENDIF
        IF SEEK(STR(&lcCtnDtl..Cart_No,4) , lcCtnHdr )
          SELECT &lcCtnHdr
          REPLACE Cart_No WITH lnI
          SELECT &lcCtnDtl
        ENDIF
		
		REPLACE &lcCtnDtl..Cart_No WITH lnI
		llFirst  = .F.
	ENDSCAN

    IF SEEK(laData[1]+STR(lnI,6),'EDICRTSQ')
      SELECT EDICRTSQ
      *DELETE REST WHILE pack_no+STR(cart_no,6) = laData[1]
      DELETE FOR pack_no+STR(cart_no,6) = laData[1]
      FLUSH
    ENDIF
    
    SELECT Pack_Lin
    =SEEK(laData[1])
    SCAN REST WHILE Pack_No+STR(No_Cart,4)+Style = laData[1]
      SET ORDER TO PCKCRTSQ IN EDICRTSQ
      IF !SEEK(laData[1]+STR(Pack_Lin.No_Cart,6),'EDICRTSQ')
        *-- If this customer is a partner , get the last Ucc # from EDIACPRT file,
        IF SEEK('A'+laData[4],'EDIACPRT')
          lnUcc9  = IIF(EMPTY(EDIACPRT.Ucc9),0,EVAL(EDIACPRT.Ucc9)) + 1
          SET ORDER TO EDICRTSQ IN EDICRTSQ
          llFound = SEEK(laData[4]+PADL(lnUcc9,9,'0'),'EDICRTSQ')
          DO WHILE llFound 
            lnUcc9  = lnUcc9  + 1
            llFound = SEEK(laData[4]+PADL(lnUcc9,9,'0'),'EDICRTSQ')
          ENDDO
          INSERT INTO EDICRTSQ (Pack_No,Account,cart_no,Ucc9);
                         VALUE (laData[1],laData[4],Pack_Lin.No_Cart,PADL(lnUcc9,9,'0'))
          REPLACE EDIACPRT.Ucc9 WITH PADL(lnUcc9,9,'0')
        ELSE    && Get the last Ucc # from EDICRTSQ file.
          SELECT MAX(EDICRTSQ .ucc9) FROM EDICRTSQ WHERE EDICRTSQ.account = laData[4] INTO CURSOR lcMaxUcc
          SELECT lcMaxUcc
          LOCATE
          IF EOF()
            lcUcc9 = '000000001'
          ELSE
            lcUcc9 = PADL(EVAL(lcMaxUcc.MAX_UCC9)+1,9,'0')
          ENDIF  
          INSERT INTO EDICRTSQ (Pack_No,Account,cart_no,Ucc9);
                         VALUE (laData[1],laData[4],Pack_Lin.No_Cart,lcUcc9)
        ENDIF  
      ENDIF  
    ENDSCAN
	SET DELETE &lcSetDel
  ENDIF
  *E039957,1  MHM 11/14/2005 [End]
*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
ENDIF
*E040123,1 HBG 03/13/2006 [End]

SELECT Pack_Hdr
REPLACE nLastLno  WITH MAX(nLastLno,lnLastNo) ,;
	nLastCart WITH lnLastCtn
ENDIF

*E120116,1  TMI [Start] Update Pack_lin file with the Carrier Carton ID field from the lcCtnHdr temp file
PRIVATE lcDelSt
lcDelSt = SET('DELETED')
SET DELETED ON
SELECT (lcCtnHdr)
GO TOP
SCAN
	IF SEEK(laData[1]+STR(&lcCtnHdr..CART_NO,4),'PACK_LIN')
		SELECT PACK_LIN
		REPLACE cCarrCtnID WITH &lcCtnHdr..cCarrCtnID REST ;
			WHILE PACK_NO+STR(NO_CART,4)+STYLE = laData[1]+STR(&lcCtnHdr..CART_NO,4)
	ENDIF
ENDSCAN
SET DELETED &lcDelStat
*E120116,1  TMI [End  ]

*C200218,1 BWA 30/08/2001 Modify the custom invoice for sademara.[START]
IF ASCAN(laEvntTrig , PADR('RECALC',10)) <> 0
	lcParam = "MANUAL"
	=gfDoTriger('ALPLIST',PADR('RECALC',10))
ENDIF
*C200218,1 BWA 30/08/2001.[END]

*=================end saving pack_lin file======================
*C125077,1 AEH 06/02/2005 Add Carton Number to File [Start]
IF ASCAN(laEvntTrig , PADR('ADDCRTNO',10)) <> 0
	=gfDoTriger('ALPLIST',PADR('ADDCRTNO',10))
ENDIF
*C125077,1 AEH 06/02/2005 Add Carton Number to File [END]

*=================update book,open fields in ordhdr file=========
IF lnRelCho <> 2
	*B603957,1 Detect if there are difference and then update ordhdr [Begin]
	*-- MAB Note : Check for quantity is not sufficient because we may have the same quantity
	*--            but the user increase one line and decrease another, and these two lines have
	*--            different prices.
	*IF SEEK('O'+Pack_Hdr.Order,'OrdHdr')
	IF !(lnBookDiff=0 AND lnQtyDiff=0 AND lnBkAmtDif=0 AND lnQyAmtDif=0) AND ;
			SEEK('O'+Pack_Hdr.Order,'OrdHdr')
		*B603957,1
		SELECT OrdHdr
		*B802541 (Start)
		*REPLACE   BOOK     WITH lnBookQty                 ;
		BOOKAMT  WITH lnBookQty * OrdLine.Price ;
		OPEN     WITH lnBookQty                 ;
		OPENAMT  WITH lnBookQty * OrdLine.Price

	*B603957,1 Update OrdHdr Table [Begin]
	*REPLACE   BOOK     WITH (Book    - lnOldOQty + lnBookQty) ;
	BOOKAMT  WITH (BookAmt - lnOldOAmt + lnBookAmt) ;
	OPEN     WITH (OPEN    - lnOldOQty + lnBookQty) ;
	OPENAMT  WITH (OpenAmt - lnOldOAmt + lnBookAmt)

=RLOCK()
REPLACE BOOK    WITH BOOK    + lnBookDiff ,;
	BOOKAMT WITH BOOKAMT + lnBkAmtDif ,;
	OPEN    WITH OPEN    + lnQtyDiff  ,;
	OPENAMT WITH OPENAMT + lnQyAmtDif
UNLOCK
=gfAdd_Info()
ENDIF
ENDIF
*=================end update book,open fields in ordhdr file=====
*-- C101735,1 SAVE CARTON INFO. TO BE PRINTED [Begin]

*C102284,1 ABD  Update the Piktkt file with status 'K' Packed. [Begin]
*================= Update status fields in Piktkt file to be 'K' As 'Pack' =========*
IF ASCAN(laEvntTrig , PADR('LLPAKST',10)) <> 0
	IF lnRelCho <> 2 .AND. PikTkt.Status # 'K'
		lnOldAlias = SELECT(0)
		SELECT PikTkt
		REPLACE STATUS WITH 'K'
		SELECT (lnOldAlias)
	ENDIF
ENDIF
*C102284,1 ABD  [End]

*B603496,1 print carton label without EDI [Begin]

=llCanPrnLb AND lfSavCartn()
*=llEdiSys AND llCanPrnLb AND lfSavCartn()
*B603496,1 print carton label without EDI [End  ]

*-- C101735,1 SAVE CARTON INFO. TO BE PRINTED [End  ]

ENDIF
SET DELETED &lcDelStat

*E302144,1 AMH Change the custom C102789 to be standard [Start]
*C102789,4 ASH 02/15/2003 (Begin) Save Carton type in pack_lin file.
*IF ASCAN(laEvntTrig , PADR('SAVLINE',10)) <> 0
*  =gfDoTriger('ALPLIST',PADR('SAVLINE',10))
*ENDIF
*C102789,4 ASH 02/15/2003 (End)
SELECT (lcCtnHdr)
SCAN
	IF SEEK(laData[1]+STR(Cart_No,4),'Pack_Lin')
		SELECT Pack_Lin
		SCAN WHILE pack_no+STR(no_cart,4)+STYLE = laData[1]+STR(&lcCtnHdr..Cart_No,4)
			REPLACE cCrtnVlTyp WITH IIF(EMPTY(&lcCtnHdr..cCrtnVlTyp),PACK_HDR.cCrtnVlTyp,&lcCtnHdr..cCrtnVlTyp)
		ENDSCAN
	ENDIF
ENDSCAN
*E302144,1 AMH [End]

IF lnRelCho <> 2
	llReturn = .T.
ELSE
	llReturn = .F.
ENDIF

SELECT(lcCtnDtl)
SET RELATION TO &lcCtDtRel.

llNoThing = lfUpdUnCmS("Comp", SPACE(0))

SELECT OrdHdr
lcTag = ORDER()
SET ORDER TO TAG OrdHdr
IF SEEK('O'+laData[2],'OrdHdr')
	= gfObj_Lock(.F.)
ENDIF
SET ORDER TO TAG lcTag

SELECT(lnCurAlias)
llCSave = llReturn
*-- end of lpSavscr.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To make local delete.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpDelScr
*!*************************************************************

PROCEDURE lpDelScr

PRIVATE lnCurAlias,lcHdrTag,lcLinTag,llShiped

*B603807,1 HBG 08/09/2000 Can't Delete if this packing list is shiped [Begin]
IF SEEK(laData[1],'Pack_Hdr')
	IF Pack_Hdr.Status = 'C'
		PRIVATE lcMessage
		*-- This packing list is shipped. Can't be Deleted
		*-- OK
		lcMessage = "Can't be Deleted."
		= gfModalGen("INM44102B00000","Dialog",lcMessage)
		*-- Return to "SELECT" mode
		laScrMode        = .F.
		laScrMode[2]     = .T.
		RETURN
	ENDIF
ENDIF
*B603807,1 [End]

lnCurAlias = SELECT(0)

IF !(laData==lcPrvPack)
	= lfvSelOrdL()
ENDIF

SELECT (lcCtnDtl)
SET FILTER TO
SET RELATION TO
SCAN
	IF SEEK('O'+laData[2]+laData[5]+&lcCtnDtl..Style+STR(&lcCtnDtl..nOrdLineNo,6),'OrdLine')
		SELECT OrdLine
		*B605559,1 HBG 18/02/2002 Update packed qty with correct qty after deleteing the pack[Begin]
		*REPLACE nPck1  WITH nPck1  - &lcCtnDtl..Qty1,;
		nPck2  WITH nPck2  - &lcCtnDtl..Qty2,;
		nPck3  WITH nPck3  - &lcCtnDtl..Qty3,;
		nPck4  WITH nPck4  - &lcCtnDtl..Qty4,;
		nPck5  WITH nPck5  - &lcCtnDtl..Qty5,;
		nPck6  WITH nPck6  - &lcCtnDtl..Qty6,;
		nPck7  WITH nPck7  - &lcCtnDtl..Qty7,;
		nPck8  WITH nPck8  - &lcCtnDtl..Qty8,;
		nPwght WITH nPwght - (&lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
		&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
		&lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
		&lcCtnDtl..Weight7+&lcCtnDtl..Weight8)
	*B606276,1 TMI [Start] Fix the bug that nPack qtys is saved in order line with negative quantities.
	*REPLACE nPck1  WITH IIF(&lcCtnDtl..Qty1 <> 0,nPck1  - &lcCtnDtl..Qty1,0),;
	nPck2  WITH IIF(&lcCtnDtl..Qty1 <> 0,nPck2  - &lcCtnDtl..Qty2,0),;
	nPck3  WITH IIF(&lcCtnDtl..Qty1 <> 0,nPck3  - &lcCtnDtl..Qty3,0),;
	nPck4  WITH IIF(&lcCtnDtl..Qty1 <> 0,nPck4  - &lcCtnDtl..Qty4,0),;
	nPck5  WITH IIF(&lcCtnDtl..Qty1 <> 0,nPck5  - &lcCtnDtl..Qty5,0),;
	nPck6  WITH IIF(&lcCtnDtl..Qty1 <> 0,nPck6  - &lcCtnDtl..Qty6,0),;
	nPck7  WITH IIF(&lcCtnDtl..Qty1 <> 0,nPck7  - &lcCtnDtl..Qty7,0),;
	nPck8  WITH IIF(&lcCtnDtl..Qty1 <> 0,nPck8  - &lcCtnDtl..Qty8,0),;
	nPwght WITH IIF((&lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
	&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
	&lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
	&lcCtnDtl..Weight7+&lcCtnDtl..Weight8) <> 0,;
	nPwght - (&lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
	&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
	&lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
	&lcCtnDtl..Weight7+&lcCtnDtl..Weight8),0)
REPLACE nPck1  WITH IIF(&lcCtnDtl..Qty1 <> 0,MAX(nPck1  - &lcCtnDtl..Qty1,0),0),;
	nPck2  WITH IIF(&lcCtnDtl..Qty1 <> 0,MAX(nPck2  - &lcCtnDtl..Qty2,0),0),;
	nPck3  WITH IIF(&lcCtnDtl..Qty1 <> 0,MAX(nPck3  - &lcCtnDtl..Qty3,0),0),;
	nPck4  WITH IIF(&lcCtnDtl..Qty1 <> 0,MAX(nPck4  - &lcCtnDtl..Qty4,0),0),;
	nPck5  WITH IIF(&lcCtnDtl..Qty1 <> 0,MAX(nPck5  - &lcCtnDtl..Qty5,0),0),;
	nPck6  WITH IIF(&lcCtnDtl..Qty1 <> 0,MAX(nPck6  - &lcCtnDtl..Qty6,0),0),;
	nPck7  WITH IIF(&lcCtnDtl..Qty1 <> 0,MAX(nPck7  - &lcCtnDtl..Qty7,0),0),;
	nPck8  WITH IIF(&lcCtnDtl..Qty1 <> 0,MAX(nPck8  - &lcCtnDtl..Qty8,0),0),;
	nPwght WITH IIF((&lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
	&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
	&lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
	&lcCtnDtl..Weight7+&lcCtnDtl..Weight8) <> 0,;
	nPwght - (&lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
	&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
	&lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
	&lcCtnDtl..Weight7+&lcCtnDtl..Weight8),0)
*B606276,1 TMI [End]
*B605559,1 [End]
ENDIF
ENDSCAN
SET RELATION TO '' INTO (lcCartonSz)
SET SKIP TO (lcCartonSz)

*B603515,1 Update BOL Header totals
IF llEdiSys
	=gfOpenFile(gcDataDir+'Bol_Hdr',gcDataDir+'Bol_Hdr','SH')
	=gfOpenFile(gcDataDir+'Bol_Lin',gcDataDir+'Bol_Lin','SH')
	=lfDelBol()
ENDIF
*B603515,1 (eND)

lcHdrTag = ORDER('Pack_Hdr')
IF SEEK(laData[1],'Pack_Hdr')
	SELECT Pack_Hdr
	BLANK
	DELETE
ENDIF
SET ORDER TO TAG (lcHdrTag) IN Pack_hdr

lcLinTag = ORDER('Pack_Lin')
IF SEEK(laData[1],'Pack_Lin')
	SELECT Pack_Lin

	*B803778,1 HBG 11/16/2000 Scan "For" to delete all records for this Packing list [Begin]
	*SCAN REST WHILE Pack_No+STR(No_Cart,4)+Style = laData[1]
	SCAN FOR Pack_No+STR(No_Cart,4)+STYLE = laData[1]
		*B803778,1 [End]
		BLANK
		DELETE
	ENDSCAN
ENDIF

*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
IF !llIncPLNum
*E040123,1 HBG 03/13/2006 [End]
  *E039957,1  MHM 11/14/2005 Update the new file EDICRTSQ , abd thenew field Ucc9 in EDIACPRT [Begin]
  SET ORDER TO PCKCRTSQ IN EDICRTSQ
  lcSetDele = SET('DELETE')
  SET DELETE ON
  IF SEEK(laData[1],'EDICRTSQ')
    SELECT EDICRTSQ
    DELETE FOR pack_no+STR(cart_no,6) = laData[1]
  ENDIF  
  SET DELETE &lcSetDele 
  *E039957,1  MHM 11/14/2005 [End]
*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
ENDIF
*E040123,1 HBG 03/13/2006 [End]


SET ORDER TO TAG (lcLinTag) IN Pack_Lin

=lfCrtUnCmp()

*-- Return to "SELECT" mode
laScrMode        = .F.
laScrMode[1]     = .T.

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPackCopy
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : use if the user wishes to copy data for new pack from
*!             an existing one.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPackCopy()
*!*************************************************************

FUNCTION lfPackCopy

llBrowse = .F.
lcWName    = "ICPCopy"
lcWSPTitl  = "Copy From another Pack Slip"

PUSH KEY
*B604186,1 HBG 02/07/2001 The program was transfered to root. [Begin]
*DO (gcScrDir + gcWinAppl + '\ALPLSTCP.SPR')
DO (gcScrDir + '\ALPLSTCP.SPR')
*B604186,1 [End]
POP KEY

*!*************************************************************
*! Name      : lfvPackNo
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate the selected pack no to copy data from it
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPackNo()
*!*************************************************************

FUNCTION lfvPackNo

lnCurAlias = SELECT(0)
lcCurTag   = ORDER('Pack_Hdr')

SELECT Pack_Hdr
SET ORDER TO Pack_Hdr
IF llBrowse OR !SEEK(lcPackNo ,'Pack_Hdr')
	llPckCopy = .T.
	llBrowse  = .F.
	lcPackNo  = IIF(lfPakNoBrw(),Pack_Hdr.Pack_No,SPACE(6))
	lcOrderNo = IIF(!EMPTY(lcPackNo),Pack_Hdr.Order,SPACE(6))
	lcStore   = IIF(!EMPTY(lcPackNo),Pack_Hdr.Store,SPACE(6))
	llPckCopy = .F.
ENDIF

IF !EMPTY(lcPackNo)
	lcOrderNo = Pack_Hdr.Order
	lcStore   = Pack_Hdr.Store
	*B603807,1 HBG 08/09/2000 Get the value of "lstandctn" And "ctostorcn" fields
	*B603807,1                from the packing list we copy from[Begin]
	lnCtnTyp = IIF(Pack_Hdr.LStandCtn,1,2)
	lnDrctTo = IIF(Pack_Hdr.CToStorCn='C',2,1)
	*-- Fixing bug of getting lnFrom & lnTo intioaly by zero when we copy from packing list[Begin]
	lnFrom = 1
	lnTo   = 1
	*-- Fixing bug of getting lnFrom & lnTo intioaly by zero when we copy from packing list[End  ]
	*B603807,1 [End]
ENDIF



SET ORDER TO lcCurTag IN Pack_Hdr
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPackScrAct
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the keys in screen ICPACKSCR
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfPackScrAct()
*!*************************************************************

FUNCTION lfPackScrAct

*-- This function called in activate snippet for screen packScr
*-- which use to copy data from another pack id

ON KEY LABEL ESCAPE DO lfPackScrEsc

*!*************************************************************
*! Name      : lfPackScrEsc
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the Esacpe Key in screen IcpackScr
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfPackScrEsc()
*!*************************************************************

FUNCTION lfPackScrEsc

ON KEY LABEL ESCAPE

_CUROBJ = OBJNUM(pbCancel)
KEYBOARD "{ENTER}" CLEAR PLAIN

*!*************************************************************
*! Name      : lfCtnHdrBr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse lcCtnHdr file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCtnHdrBr()
*!*************************************************************

FUNCTION lfCtnHdrBr

PRIVATE lnCurAlias,lcFields

lnCurAlias = SELECT(0)

IF llEdiSys
	lcFields = "cMarker =IIF(RECNO()=lnCtHdBrRe,'>',' ')  :H=' ':R:1:W=.F.,"+;
		"Cart_No    :H = 'Cart.#'       :6 :R,"+;
		"Pal_No     :H = 'Palette'      :6 :R,"+;
		"TotPcs     :H = 'Tot.Pcs'      :8 :R,"+;
		"TotWgh     :H = 'Tot.Wgh'      :8 :R"
ELSE
	lcFields = "cMarker =IIF(RECNO()=lnCtHdBrRe,'>',' ')  :H=' ':R:1:W=.F.,"+;
		"Cart_No    :H = 'Cart.#'       :6 :R,"+;
		"TotPcs     :H = 'Tot. Pieces'  :11:R,"+;
		"TotWgh     :H = 'Tot. Weight'  :11:R"
ENDIF
*E120116,1 TMI [START] Show field 'Carrier Carton ID' in browse
lcFields = lcFields + ",cCarrCtnID     :H = 'Carrier Carton ID'  :30:R"
*E120116,1 TMI [END  ]

SELECT (lcCtnHdr)
GO TOP
lnCtHdBrRe = RECNO()
BROWSE FIELDS &lcFields ;
	SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
	TITLE(lcCtHdBrTt) WHEN lfwCtnHdrBr() VALID :F lfvCtnHdrBr()    ;
	WINDOW (lcWinCtn1) IN WINDOW (lcWinCtn)

=lfwCtnHdrBr()
SELECT (lnCurAlias )

*!*************************************************************
*! Name      : lfwCtnHdrBr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : refresh the edit rigion for carton header according to
*!             lcCtnHdr file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwCtnHdrBr()
*!*************************************************************

FUNCTION lfwCtnHdrBr
PRIVATE lnAlias

lnCtHdBrRe = RECNO(lcCtnHdr)

IF lnActFolder = 3
	SHOW WINDOW (lcCtHdBrTt) REFRESH
ENDIF

lnAlias = SELECT(0)
SELECT (lcCtnHdr)
lnCtHdBrRe = RECNO()
lnTotCart  = RECCOUNT()
lnCartNo   = &lcCtnHdr..Cart_No
lnTotWgh   = &lcCtnHdr..TotWgh
lnPalNo    = &lcCtnHdr..Pal_No
*E120116,1 TMI [START] Populate the field cCarrCtnID while navigation on grid
lcCarrCtnID = &lcCtnHdr..cCarrCtnID
*E120116,1 TMI [END  ]

IF (laScrMode[3] OR llNew) AND lnActFolder = 3
	IF EOF(lcCtnHdr) OR BOF(lcCtnHdr) OR DELETED(lcCtnHdr)
		STORE "DISABLE" TO lcRemHdrSt,lcCartNoSt,lcTotWghSt,lcPalNoSt,lcAddDtlSt
	ELSE
		STORE "ENABLE" TO lcRemHdrSt
		IF !EMPTY(&lcCtnHdr..Cart_No)
			lcCartNoSt = "DISABLE"
		ELSE
			lcCartNoSt = "ENABLE"
		ENDIF
		lcTotWghSt = "ENABLE"
		lcPalNoSt  = IIF(llPalStat,"ENABLE","DISABLE")
	ENDIF
ELSE
	STORE "DISABLE" TO lcRemHdrSt,lcCartNoSt,lcTotWghSt,lcPalNoSt
ENDIF

SHOW GET pbDtlNew &lcAddDtlSt
SHOW GET pbHdrRem &lcRemHdrSt
SHOW GET lnCartNo &lcCartNoSt
SHOW GET lnTotWgh &lcTotWghSt
SHOW GET lnPalNo  &lcPalNoSt
*E120116,1  TMI [Start] Update lcCarrCtnID status
SHOW GET lcCarrCtnID &lcCrCtnIDSt
*E120116,1  TMI [End  ]

SELECT (lcCtnDtl)
SET RELATION TO
SET FILTER TO STR(Cart_No,4)+STYLE+STR(nOrdLineNo,6) = STR(&lcCtnHdr..Cart_No,4)

SELECT (lcCtnDtl)
SET RELATION TO '' INTO (lcCartonSz) ADDITIVE
SET SKIP TO (lcCartonSz)
LOCATE FOR EVAL(lcCtnDtl+'.Br'+&lcCartonSz..SzCode)

IF WEXIST(lcCtDtBrTt) AND lnActFolder = 3
	= lfwCtnDtlBr()
ENDIF

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvCtnHdrBr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() function
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtnHdrBr()
*!*************************************************************

FUNCTION lfvCtnHdrBr

IF WONTOP() # (lcCtHdBrTt)
	=gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfCtnDtlBr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse lcCtnDtl file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfwBrow,lfvBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCtnDtlBr()
*!*************************************************************

FUNCTION lfCtnDtlBr

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

SELECT (lcCtnDtl)
GO TOP
IF EMPTY(SET('RELATION'))
	SET RELATION TO '' INTO (lcCartonSz) ADDITIVE
	SET SKIP TO (lcCartonSz)
ENDIF

lnCtDtBrRe = RECNO()
lnDumCtnR  = RECNO(lcCartonSz)

BROWSE FIELDS cMarker =IIF(RECNO()=lnCtDtBrRe AND RECNO(lcCartonSz)=lnDumCtnR,'>',' '):H=' ':R:1:W=.F.,;
	StyCode=STYLE:H= lcStyTtl     :25:R,;
	SIZE   =EVAL('Size' +&lcCartonSz..SzCode) :H = 'Size':7 :R,;
	SzQty  =EVAL('Qty'+&lcCartonSz..SzCode)   :H = 'Qty.':6 :R,;
	SzWgh  =EVAL('Weight'+&lcCartonSz..SzCode):H = 'Wgh.':6 :R ;
	FOR EVAL(lcCtnDtl+'.Br'+&lcCartonSz..SzCode) AND &lcCartonSz..SzCode <= STR(&lcCtnDtl..SzCnt,1);
	SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
	TITLE(lcCtDtBrTt) WHEN lfwCtnDtlBr() VALID :F lfvCtnDtlBr();
	WINDOW (lcWinCtn2) IN WINDOW (lcWinCtn)

=lfwCtnDtlBr()
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfwCtnDtlBr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : refresh the edit rigion for carton details according to
*!             lcCtnDtl file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwCtnDtlBr()
*!*************************************************************

FUNCTION lfwCtnDtlBr
PRIVATE lnAlias

lnUnitWgh = lnStyWgh/EVAL(lcCtnDtl+'.Qty'+&lcScaFile..SzCode)

lnAlias = SELECT(0)
SELECT (lcCtnDtl)
lnCtDtBrRe = RECNO()
lnDumCtnR  = RECNO(lcCartonSz)

lcCtnSty = &lcCtnDtl..Style
lcStySz  = EVAL(lcCtnDtl+'.Size'+&lcCartonSz..SzCode)
lnStyQty = EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode)
lnStyWgh = EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode)

IF (laScrMode[3] OR llNew) AND lnActFolder = 3
	IF &lcCtnDtl..Cart_No = 0 AND EMPTY(&lcCtnDtl..Style+EVAL(lcCtnDtl+'.Size'+&lcCartonSz..SzCode))
		STORE "DISABLE" TO lcRemDtlSt
		STORE "DISABLE" TO lcCtnStySt,lcStySzSt,lcStyQtySt,lcStyWghSt
	ELSE
		STORE "ENABLE" TO lcRemDtlSt
		IF EMPTY(&lcCtnDtl..Style) OR EMPTY(EVAL(lcCtnDtl+'.Size'+&lcCartonSz..SzCode))
			STORE "ENABLE" TO lcCtnStySt,lcStySzSt,lcStyQtySt,lcStyWghSt
		ELSE
			STORE "DISABLE" TO lcCtnStySt,lcStySzSt
			STORE "ENABLE"  TO lcStyQtySt,lcStyWghSt
		ENDIF
	ENDIF
ELSE
	STORE "DISABLE" TO lcRemDtlSt
	STORE "DISABLE" TO lcCtnStySt,lcStySzSt,lcStyQtySt,lcStyWghSt
ENDIF

IF lnActFolder = 3
	SHOW WINDOW (lcCtDtBrTt) REFRESH SAME
	SHOW GETS WINDOW (lcWinCtn3) ONLY
ENDIF

SHOW GET pbDtlRem  &lcRemDtlSt
SHOW GET pbStyBrow &lcCtnStySt
SHOW GET lcCtnSty  &lcCtnStySt
SHOW GET pbSzBrow  &lcStySzSt
SHOW GET lcStySz   &lcStySzSt
SHOW GET lnStyQty  &lcStyQtySt
SHOW GET lnStyWgh  &lcStyWghSt

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvCtnDtlBr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() function
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtnDtlBr()
*!*************************************************************

FUNCTION lfvCtnDtlBr

IF WONTOP() # (lcCtDtBrTt)
	=gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfActPad
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To create the "Option" menu pad.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfActPad()
*!*************************************************************

FUNCTION lfActPad

DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
DEFINE BAR 101 OF P01PU01 PROMPT "" KEY ALT+C
DEFINE BAR 102 OF P01PU01 PROMPT "" KEY ALT+D
ON SELECTION BAR 100 OF  P01PU01 ACTIVATE WINDOW (lcDtlBrTtl)
ON SELECTION BAR 101 OF  P01PU01 ACTIVATE WINDOW (lcCtHdBrTt)
ON SELECTION BAR 102 OF  P01PU01 ACTIVATE WINDOW (lcCtDtBrTt)

*-- Define the "Options" menu pad.
DEFINE PAD   _OPTIONS OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , SPACE(1)

*-- Define the "Options" and the "Transactions" popups.
DEFINE POPUP _OPTPOP MARGIN SHADOW

*-- Define the "Options" bars in the "Options" menu pad.
*-- Define the "Transactions" bars in the "Transactions" menu pad.
DEFINE BAR 1 OF _OPTPOP PROMPT "Clear Selection Upon Apply   " SKIP FOR !(!laScrMode[1] AND lnActFolder=2) KEY ALT+C
DEFINE BAR 2 OF _OPTPOP PROMPT "Show Open Quantities Only    " SKIP FOR !(!laScrMode[1] AND lnActFolder=2) KEY ALT+O

*-- C101735,1 Add Menue Bar In Option Menue Under Condition [Begin]
IF llCanPrnLb
	DEFINE BAR 3 OF _OPTPOP PROMPT "\-"
	DEFINE BAR 4 OF _OPTPOP PROMPT "Print Labels Setup" SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder!=2))
	*E301373,1 NAD 02/14/2000 (Start) Define the menu bar
	DEFINE BAR 5 OF _OPTPOP PROMPT "Style Filter" SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder!=2) OR EOF(lcPckLin))

	*B602797,1 Add Menue Bar In Option Menue [Begin]
	DEFINE BAR 6 OF _OPTPOP PROMPT "Update Pick Ticket Qty. Upon Saving" SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder=1))
	SET MARK OF BAR 6 OF _OPTPOP TO llUpdtPkTk
	*B602797,1 Add Menue Bar in option menue [End  ]

ELSE
	DEFINE BAR 3 OF _OPTPOP PROMPT "Style Filter" SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder!=2) OR EOF(lcPckLin))
	*E301373,1 NAD  (End)

	*B602797,1 Add Menue Bar In Option Menue [Begin]
	DEFINE BAR 4 OF _OPTPOP PROMPT "Update Pick Ticket Qty. Upon Saving" SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder=1))
	SET MARK OF BAR 4 OF _OPTPOP TO llUpdtPkTk
	*B602797,1 Add Menue Bar in option menue [End  ]

ENDIF

*-- C101735,1 Add Menue Bar In Option Menue Under Condition [End  ]

*E302144,1 AMH Change the custom C102789 to be standard [Start]
*C102789,1 TMI [Start] Add pad option for KrazyCat to update the Carton Dimension type field
*IF ASCAN(laEvntTrig , PADR('ALCRTNPD',10)) <> 0
*  =gfDoTriger('ALPLIST',PADR('ALCRTNPD',10))
*ENDIF
*C102789,1 TMI [End  ]
PRIVATE lnBarNo
lnBarNo = CNTBAR('_OPTPOP') + 1
DEFINE BAR lnBarNo   OF _OPTPOP PROMPT "\-" SKIP FOR .T.

DEFINE BAR lnBarNo+1 OF _OPTPOP PROMPT '\<Update Carton Dimensions type field'  ;
	SKIP FOR (laScrMode[1] .OR. laScrMode[2])
ON SELECTION BAR lnBarNo+1 OF _OPTPOP DO lfCRTNTYPE
*E302144,1 AMH [End]
*C122115,1 NFZ 05/18/2004 (Begin) Trigger to add option in the option menu
IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0
	=gfDoTriger('ALPLIST',PADR('ADDOPTN',10))
ENDIF
*C122115,1 NFZ 05/18/2004 (End)

*C123616,1  TMI [Start] Add a new option to generate cartons for Nik Nak
IF ASCAN(laEvntTrig , PADR('GENCRTPD',10)) <> 0
	=gfDoTriger('ALPLIST',PADR('GENCRTPD',10))
ENDIF
*C123616,1  TMI [End  ]

*-- Define the action to be done when activating the pad and the popup.
ON PAD _OPTIONS OF _MSYSMENU ACTIVATE POPUP _OPTPOP
ON SELECTION BAR 1 OF _OPTPOP DO lpClrSel
ON SELECTION BAR 2 OF _OPTPOP DO lpShwOpn

*--C101735,1 Define the action to be done when activating the pad and the popup. [Begin]
IF llCanPrnLb AND (laScrMode[3] OR laScrMode[4]) AND (lnActFolder = 2)
	ON SELECTION BAR 4 OF _OPTPOP DO lpPortScr
ENDIF

*E301373,1 NAD 02/14/2000 (Start) On selection of the menu bar
IF llCanPrnLb
	ON SELECTION BAR 5 OF _OPTPOP DO lpFltrScr
ELSE
	ON SELECTION BAR 3 OF _OPTPOP DO lpFltrScr
ENDIF
*E301373,1 NAD  (End)
*--C101735,1 Define the action to be done when activating the pad and the popup. [End  ]
*B602797,1 Define the action to be done when activating the pad and the popup. [Begin]
IF (laScrMode[3] OR laScrMode[4]) AND (lnActFolder <> 1)
	ON SELECTION BAR IIF(llCanPrnLb,6,4) OF _OPTPOP DO lpUpdtPkTk
ENDIF
*B602797,1 Define the action to be done when activating the pad and the popup. [End  ]
*-- end of lfActPad.


*!*************************************************************
*! Name      : lfRELPad
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To release the "Option" menu pad.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRELPad()
*!*************************************************************

FUNCTION lfRELPad

RELE BAR 100 OF P01PU01
RELE BAR 101 OF P01PU01
RELE BAR 102 OF P01PU01
*-- Define the "Options" menu pad.
RELE PAD   _OPTIONS OF _MSYSMENU

*!*************************************************************
*! Name      : lfDeActPad
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To release the "Options" menu pad.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDeActPad()
*!*************************************************************

FUNCTION lfDeActPad

RELEASE PAD _OPTIONS OF _MSYSMENU
RELEASE POPUP _CLRSEL
RELEASE POPUP _SHWOPN

*!*************************************************************
*! Name      : lfWinCtnSt
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To adjust add buttons for carton header and
*!             carton detail staus
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWinCtnSt()
*!*************************************************************

FUNCTION lfWinCtnSt
PRIVATE lnCtnHdrRe,lnCtnDtlRe

IF lnActFolder = 3 AND (laScrMode[3] OR llNew)
	STORE "ENABLE" TO lcAddHdrSt

	*-- C101735,1 Put the print button under EDI condition [Begin]
	*IF llEdiSys			&& B603496,1 remove the EDI condition
	STORE "ENABLE" TO lcPrnLbl
	*ENDIF
	*-- C101735,1 Put the print button under EDI condition [End  ]

	*-- C101735,1 Hold the state of the print label button (pbPrnLbl)
	IF EMPTY(&lcCtnHdr..Cart_No)
		STORE "DISABLE" TO lcAddDtlSt

		*-- C101735,1 Put the print button under EDI condition [Begin]
		*IF llEdiSys 		&& B603496,1 remove the EDI condition
		STORE "DISABLE" TO lcPrnLbl
		*ENDIF
		*-- C101735,1 Put the print button under EDI condition [End  ]

	ELSE
		STORE "ENABLE" TO lcAddDtlSt

		*-- C101735,1 Put the print button under EDI condition [Begin]
		*IF llEdiSys 		&& B603496,1 remove the EDI condition
		STORE "ENABLE" TO lcPrnLbl
		*ENDIF
		*--C101735,1 Put the print button under EDI condition [End  ]

	ENDIF
ELSE
	STORE "DISABLE" TO lcAddHdrSt,lcAddDtlSt
	*--C101735,1 Put the print button under EDI condition [Begin]
	*IF llEdiSys     &&  B603496,1 remove the EDI condition
	STORE "DISABLE" TO lcPrnLbl
	*ENDIF
	*--C101735,1 Put the print button under EDI condition [End  ]

ENDIF
SHOW GET pbHdrNew  &lcAddHdrSt
SHOW GET pbDtlNew  &lcAddDtlSt
*IF llEdiSys  		&& B603496,1 remove the EDI condition
SHOW GET pbPrnLbl  &lcPrnLbl
*ENDIF

*!*************************************************************
*! Name      : lfChkUnCmS
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Check if there is uncomplete session
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfChkUnCmS()
*!*************************************************************

FUNCTION lfChkUnCmS
PRIVATE lnAlias, llGoOut, lnReprocess



*B129128,1 EIH 09/01/2005 Cancel uncomp. session for england customer's [Begin].

IF UPPER(ALLTRIM(gcContCode))='ENG'
  = lfCrtUnCmp()
  RETURN .F.
ENDIF

*B129128,1 EIH 09/01/2005 [End].

llGoOut     = .F.

IF gfUnCompSession(lcProgID,lnSessNo,"Manual Packing List")
	llGoOut   = .T.
	STORE .F. TO laScrMode
	laScrMode[ATC(lcScrMode,"SVEA")] = .T.
	lcSession = UnCmSess.cSession
	lcCurObj  = ALLTRIM(UnCmSess.cCurrObj)
	_CUROBJ   = OBJNUM(&lcCurObj)
	llUnComp = .T.
	SHOW GETS
	IF VARREAD() = 'PBSAV'
		DO lpSavscr
	ENDIF
ELSE
	= lfCrtUnCmp()
ENDIF



RETURN llGoOut

*!*************************************************************
*! Name      : lfCrtUnCmp
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Calling functions that Create Temp. files in case
*!             un detectimg uncomp. Session
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfCrtUnCmp()
*!*************************************************************

FUNCTION lfCrtUnCmp

= lfCrSelFiles()
= lfCrCtnFiles()

*!*************************************************************
*! Name      : lfCrSelFiles
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Create the files that are used in Function lfvSelOrd
*!             (lcPckLin)
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfCrSelFiles()
*!*************************************************************

FUNCTION lfCrSelFiles

PRIVATE lnCurAlias,lnI
lnCurAlias = SELECT(0)

lnI = 1

DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect8'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Style'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Scale'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SzCnt'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize8'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'StyWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgStyWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'LPicked'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStep'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 2
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nOrdLineNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Selected'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

*B603121,1 (Start)
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
*B603121,1 (End)


*B602797,1 DEFINE NEW FIELD [BEGIN]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
**B602797,1 DEFINE NEW FIELD [END  ]


*B130950,1 EIH 30/01/2006 Do not allow user to pack more than existing inventory if costing method is FIFO or LIFO [Begin].

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STK1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STK2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STK3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STK4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STK5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STK6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STK7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STK8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*B130950,1 EIH [End].



*E302144,1 AMH Change the custom C102789 to be standard [Start]
*C102789,4 ASH 02/15/2003 (Begin) Add cCrtnVlTyp in lcPckLin.
*IF ASCAN(laEvntTrig , PADR('ADDFLD',10)) <> 0
*  =gfDoTriger('ALPLIST',PADR('ADDFLD',10))
*ENDIF
*C102789,4 ASH 02/15/2003 (End)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'cCrtnVlTyp'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0
*E302144,1 AMH [End]

*--C102281 AAN Reindex an array of index [Begin].
*DIMENSION laIndx[4,2]

DIMENSION laIndx[5,2]
*--C102281 AAN Reindex an array of index [End].

laIndx[1,1] = "Style+STR(nOrdLineNo,6)"
laIndx[1,2] = lcPckLin

laIndx[2,1] = "IIF(Selected>0,'Y','N')"
laIndx[2,2] = 'Selected'

laIndx[3,1] = "IIF(OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+OQty8>0,'Y','N')"
laIndx[3,2] = 'Opened'

laIndx[4,1] = "IIF(PQty1+PQty2+PQty3+PQty4+PQty5+PQty6+PQty7+PQty8=0,'Y','N')"
laIndx[4,2] = 'NoPacked'

*--C102281 AAN Add new index [Begin].
laIndx[5,1] = "STR(nOrdLineNo,6)+Style"
laIndx[5,2] = lcTmpIdx
*--C102281 AAN Add new index [End].

=gfCrtTmp(lcPckLin,@laFileStru,@laIndx)
SET ORDER TO lcPckLin IN (lcPckLin)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfCrCtnFiles
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Create the files that are used in Function lfvSelOrd
*!             (lcOrdLin,lcPckLin)
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfCrCtnFiles()
*!*************************************************************

FUNCTION lfCrCtnFiles

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
DIMENSION laFileStru[5,4]

laFileStru[01,1] = 'Cart_No'
laFileStru[01,2] = 'N'
laFileStru[01,3] = 4
laFileStru[01,4] = 0

laFileStru[02,1] = 'Pal_No'
laFileStru[02,2] = 'N'
laFileStru[02,3] = 4
laFileStru[02,4] = 0

laFileStru[03,1] = 'TotPcs'
laFileStru[03,2] = 'N'
laFileStru[03,3] = 7
laFileStru[03,4] = 0

laFileStru[04,1] = 'TotWgh'
laFileStru[04,2] = 'N'
laFileStru[04,3] = 9
laFileStru[04,4] = 2

laFileStru[05,1] = 'Empty'
laFileStru[05,2] = 'C'
laFileStru[05,3] = 1
laFileStru[05,4] = 0

*E302144,1 AMH Change the custom C102789 to be standard [Start]
*C102789,4 ASH 02/15/2003 (Begin) Add cCrtnVlTyp in lcCtnHdr.
*IF ASCAN(laEvntTrig , PADR('ADDFLD',10)) <> 0
*  =gfDoTriger('ALPLIST',PADR('ADDFLD',10))
*ENDIF
*C102789,4 ASH 02/15/2003 (End)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'cCrtnVlTyp'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0
*E302144,1 AMH [End]

*E120116,1 TMI [START] Add the field CCARRCTNID ( Carton Carrier ID ) to carton hdr file
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'CCARRCTNID'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 25
laFileStru[lnFileStru+1,4] = 0
*E120116,1 TMI [END  ] Add the field CCARRCTNID to carton hdr file

DIMENSION laIndx[2,2]
laIndx[1,1] = "STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[1,2] = lcCtnHdr

laIndx[2,1] = "Empty+STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[2,2] = "EMPTY"

=gfCrtTmp(lcCtnHdr,@laFileStru,@laIndx)
SET ORDER TO lcCtnHdr IN (lcCtnHdr)


lnI = 1

DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Cart_No'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Style'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nOrdLineNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size8'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStep'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PackLineNo'
laFileStru[lnI,2] = 'N'

*B603379,1 WAM 12/29/1999 (Start) Increased the width of the line no field used in
*B603379,1                the packing list lines to be 6 instead of 3 digits
*B603379,1                not to have an error when creating big packing lists
*laFileStru[lnI,3] = 3
laFileStru[lnI,3] = 6
*B603379,1 WAM 12/29/1999 (End)

laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SzCnt'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cStatus'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br1'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br2'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br3'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br4'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br5'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br6'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br7'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br8'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

DIMENSION laIndx[2,2]

laIndx[1,1] = "STR(Cart_No,4)+Style+STR(nOrdLineNo,6)"
laIndx[1,2] = lcCtnDtl

laIndx[2,1] = "cStatus"
laIndx[2,2] = "Status"

=gfCrtTmp(lcCtnDtl,@laFileStru,@laIndx)
SET ORDER TO lcCtnDtl IN (lcCtnDtl)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfAdUnCmSR
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Adding record in uncomplete session file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfAdUnCmSR()
*!*************************************************************

FUNCTION lfAdUnCmSR
RETURN
PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT UnCmSess

*B607290,1 KHM 07/28/2003 (Begin) Changing the seek expression
*IF !SEEK('I')
IF !SEEK('I'+PADR(lcProgID,10)+PADR(lcUserID,10)) AND RLOCK()
	*B607290,1 KHM 07/28/2003 (End)

	APPEND BLANK
ENDIF
lnUnCmSeRc = RECNO()
BLANK
REPLACE STATUS     WITH 'O'      ,;
	cUTranType WITH lcProgID ,;
	cUserId    WITH lcUserID ,;
	cSession   WITH lcSession,;
	cProgram   WITH lcProgID ,;
	cCurrScr   WITH lcProgID ,;
	cCurrObj   WITH VARREAD(),;
	dTranDate  WITH gdSysDate,;
	cTranTime  WITH TIME()
llNoThing = lfUpdVars()
llNoThing  = RLOCK()

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdVars
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Update the variable string
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfUpdVars()
*!*************************************************************

FUNCTION lfUpdVars

lcFiles = "lcPckLin," + lcPckLin + "," + ORDER(lcPckLin) + ";" + ;
	"lcCtnHdr," + lcCtnHdr + "," + ORDER(lcCtnHdr) + ";" + ;
	"lcCtnDtl," + lcCtnDtl + "," + ORDER(lcCtnDtl) + ";"
llNoThing = IIF(lnUnCmSeRc=0, .T., gfSavSess(lcProgID, lcFiles, @laVars, lcSession))

*!*************************************************************
*! Name      : lfUpdUnCmS
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Update the current object and the status of the session
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfUpdUnCmS()
*!*************************************************************

FUNCTION lfUpdUnCmS
PARAMETERS lcStatus, lcCurObj
PRIVATE lnAlias

IF lnUnCmSeRc <> 0
	lnAlias  = SELECT(0)
	lcStatus = UPPER(LEFT(lcStatus,1))
	SELECT UnCmSess
	GOTO lnUnCmSeRc
	*ashraf
	IF RLOCK('UnCmSess')
		REPLACE cCurrObj WITH lcCurObj ,;
			STATUS   WITH lcStatus
	ENDIF
	IF STATUS $ "IC"
		UNLOCK
	ENDIF
	SELECT(lnAlias)
ENDIF

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   :
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lpClsScr()
*!*************************************************************

PROCEDURE lpClsScr
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

SELECT OrdHdr
lcTag = ORDER()
SET ORDER TO TAG OrdHdr

IF SEEK('O'+laData[2],'OrdHdr')
	= gfObj_Lock(.F.)
ENDIF

*B123229,1 NNA 08/01/2004 (Begin) Change the Lock state for the order multi store in the
*B123229,1 NNA                    OrdLine File to be Available to any other users
lnOldAlis = SELECT(0)
IF !EMPTY(laData[5]) .AND. SEEK('O'+laData[2]+laData[5],'OrdLine')
	SELECT ORDLINE
	SCAN REST WHILE CordType+ORDER+STORE = 'O'+laData[2]+laData[5]
		= gfObj_Lock(.F.)
	ENDSCAN
ENDIF
SELECT (lnOldAlis)
*B123229,1 NNA (End)

SET ORDER TO TAG lcTag
SELECT(lnCurAlias)

llNoThing = lfUpdUnCmS("Initia", SPACE(0))

*!*************************************************************
*! Name      : lfUpdate
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   :
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfUpdate
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfUpdate()
*!*************************************************************

FUNCTION lfUpdate

= gfUpdate()
IF UPDATED()
	llAnyUpd = .T.
ENDIF

*!*************************************************************
*! Name      : lfChkOrdLok
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : check using this order in creating pack and
*!           : Lock ordhdr record if it is not
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfUpdate
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfChkOrdLok()
*!*************************************************************

FUNCTION lfChkOrdLok
PRIVATE llGoOn,lnCurAlias

*B603807,1 HBG 08/10/2000 Fix bug of variable not found [Begin]
llGoOn = .T.
*B603807,1 [End]

lnCurAlias = SELECT(0)

IF SEEK('O'+laData[2],'OrdHdr')

	*B123229,1 NNA 08/01/2004 (Begin) if this order is multi store ,I lock the Records for the
	*B123229,1 NNA             Current Processed Store to be unavailable for another users
	IF OrdHdr.Multi = 'Y'
		IF !EMPTY(laData[5]) .AND. SEEK('O'+laData[2]+laData[5],'OrdLine')
			SELECT ORDLINE
			*-- I Run the gfObj_Lock function only for one time to change the lock state and display
			*-- the locking massage to the user for one time and then change the lock state for the
			*-- the Remain order lines
			llGoOn = gfObj_Lock(.T.)
			SCAN REST WHILE CordType+ORDER+STORE = 'O'+laData[2]+laData[5]
				REPLACE Llok_Stat WITH .T.
			ENDSCAN
		ELSE
			SELECT OrdHdr
			llGoOn = gfObj_Lock(.T.)
		ENDIF
	ELSE
		*B123229,1 NNA (End)

		SELECT OrdHdr
		llGoOn = gfObj_Lock(.T.)

		*B123229,1 NNA (Begin) end of if Statement
	ENDIF
	*B123229,1 NNA (End)
ENDIF

SELECT (lnCurAlias)

RETURN llGoOn

*!*************************************************************
*! Name      : lfNewBOL
*! Developer : Ahmed Reda (ARD)
*! Date      : 01/20/2000
*! Purpose   : Check for the BOL in add mode and if not found generat a rundoum one
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfNewBOL()
*!*************************************************************
*!C101735,1
*!
FUNCTION lfNewBOL
IF !llEdiSys
	RETURN
ENDIF

*B803177,1 HBG 06/27/2000 Get or Create BOL for this customer,Dist. Center,store,and shipvia[Begin]
PRIVATE laFields
*-- array to hold defulted fields for BOL
DIMENSION laFields[3,2]
laFields[1,1] = "cgronhang"
laFields[1,2] = "N"
laFields[2,1] = "ctranmthd"
laFields[2,2] = "M"
laFields[3,1] = "packtype"
laFields[3,2] = "CTN25"
*B804306,1 HBG 04/03/2002 Pass new parameter to lfGetBol to save field of To Store or
*B804306,1                consalidatore in BOL_HDR.[Begin]
*lcBol = lfGetBOL("",laData[4],laData[5],lcWareCode,laData[7],IIF(lnCtnTyp=1,'Y','N'),"laFields")
lcBol = lfGetBOL("",laData[4],laData[5],lcWareCode,laData[7],IIF(lnCtnTyp=1,'Y','N'),"laFields",.F.,.F.,IIF(lnDrctTo=2,"C","S"))
*B804306,1 [End]
*B803177,1 HBG 06/27/2000 Get or Create BOL for this customer,Dist. Center,store,and shipvia[End  ]

*B803177,1 HBG 06/27/2000 comment out the following block [Begin]
*                         because the previuse way to create BOL was changed
*PRIVATE lnCurAlias
*lnCurAlias = SELECT(0)
*llB_H_Opn = !llB_H_Opn AND gfOpenFile(gcDataDir+'Bol_Hdr',gcDataDir+'Bol_Hdr','SH')
*llB_D_Opn = !llB_D_Opn AND gfOpenFile(gcDataDir+'Bol_Lin',gcDataDir+'Bol_Lin','SH')

*=SEEK(IIF(EMPTY(laData[5]),'M'+laData[4],'S'+laData[4]+laData[5]),'Customer')
*SELECT BOL_HDR
*lcDistCtr  = IIF(EMPTY(Customer.Dist_Ctr),Customer.Store,Customer.Dist_Ctr)
*LOCATE FOR Account+Store+W_Code=laData[4]+lcDistCtr+lcWareCode AND Status <> 'C'

*IF FOUND()
*  lcBol = BOL_HDR.BOL_NO
*ELSE
*  *-- "Do You Wish To Generat A New BOL"
*  *-- <YES>, <NO>
*  IF gfModalGen("INM44095B00006","Dialog") = 1

*   lcBol = gfSequence('BOL_NO')
*    *B603515,1 Add new BOL
*    *INSERT INTO BOL_HDR (BOL_NO,ACCOUNT,STORE,W_CODE) VALUES (lcBol,laData[4],lcDistCtr,lcWareCode)
*    =gfRltFld(laData[7],@laVRltFld,'SHIPVIA   ')
*    INSERT INTO BOL_HDR ;
*    (BOL_NO,ACCOUNT,STORE,W_CODE,Carrier,StanCarton,CarrierCod,ShipVia,BolDate,cBolTime) VALUES ;
*    (lcBol,laData[4],lcDistCtr,lcWareCode,gfCodDes(laData[7],'SHIPVIA'),IIF(lnCtnTyp=1,'Y','N'),;
*     lcCarCod,laData[7],gdSysDate,Time())
*    *B603515,1 (End)
*  ENDIF
*ENDIF

*SELECT(lnCurAlias)
*B803177,1 HBG 06/27/2000 comment out the following block [End  ]
*-- End Of Function lfNewBOL

*!*************************************************************
*! Name      : lfvLblInfo
*! Developer : Ahmed Reda (ARD)
*! Date      : 01/20/2000
*! Purpose   : Print carton labels
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Parameter : lnCarton : May be one of the following
*!           :         1- Carton No.
*!           :         2- Array or string of cartons
*!           :         3- Null for all cartons
*!*************************************************************
*! Example   : =lfvLblInfo(lnCarton)
*!*************************************************************
*!C101735,1
*!
FUNCTION lfvLblInfo
PARAMETERS lnCarton, llPrint
PRIVATE lnCarton , lcUccVer , laAcShipTo , laDcShipTo , laWareAddr ,;
	lcStyle,lcColor,lcSize, llPrint

DIMENSION laAcShipTo[5] , laDcShipTo[5] , laWareAddr[5], laLblInfo[1, 6]

STORE "" TO laAcShipTo, laDcShipTo, laWareAddr, laLblInfo, lcStyle , lcColor , lcSize, lcCrtnSku, lcCrtnUpc
=lfGetStySz()

*E302357,1 TMI [Start] open BOL_HDR if not opened before
IF !USED('BOL_HDR')
  =gfOpenFile(gcDataDir+'BOL_HDR','BOL_HDR','SH')
ENDIF
*E302357,1 TMI [End  ] 

m.bol_no   = lcBol
m.cPRO_NO  = Bol_hdr.cPRO_NO
m.Cart_No  = lnCarton

*B603515,1 Open Warehouse file at the beginging of the program
*-- Get Warehouse information [Begin]
*=gfOpenFile(gcDataDir+'WareHous','WareHous','SH')
*=SEEK(lcWareCode)

*B603515,1 Get warehouse Address
*=gfGetAdr('WAREHOUS','','','',@laWareAddr,'')
*m.VND_NAME = lcWareCode
*m.VND_ADDR1  = laWareAddr[1]
*m.VND_ADDR2  = laWareAddr[2]
*m.VND_CITY   = laWareAddr[3]
*m.VND_STATE  = laWareAddr[4]
*m.VND_ZIP    = laWareAddr[5]

=SEEK(lcWareCode,'Warehous')
m.VND_NAME  = Warehous.cDesc
m.VND_ADDR1 = Warehous.cAddress1
m.VND_ADDR2 = Warehous.cAddress2
m.VND_CITY  = Warehous.cAddress3
m.VND_STATE = Warehous.cAddress4
m.VND_ZIP   = Warehous.cAddress5

*B603515,1 (End)

*-- Get Customer information [Begin]

*B603515,1 Get Store Address
*SELECT CUSTOMER
*=SEEK(IIF(EMPTY(lcStore),'M'+laData[4],'S'+laData[4]+lcStore),'Customer')
*lcDistCtr = Customer.Dist_Ctr
*m.cStStore  = laData[5]
*m.cStName   = Customer.StName
*lcStName  = Customer.StName
*=gfGetAdr('CUSTOMER','','','',@laAcShipTo,'')
*m.cStAddr1  = laAcShipTo[1]
*m.cStAddr2  = laAcShipTo[2]
*m.cStCity   = laAcShipTo[3]
*m.cStState  = laAcShipTo[4]
*m.cStZip    = laAcShipTo[5]

SELECT CUSTOMER
=SEEK(IIF(EMPTY(laData[5]),'M'+laData[4],'S'+laData[4]+laData[5]),'Customer')
lcDistCtr = Customer.Dist_Ctr

STORE laData[5]          TO m.cStStore,m.STORE
STORE Customer.StName    TO m.cStName ,m.SHP_NAME
STORE Customer.cAddress1 TO m.cStAddr1,m.SHP_ADDR1
STORE Customer.cAddress2 TO m.cStAddr2,m.SHP_ADDR2
STORE Customer.cAddress3 TO m.cStCity ,m.SHP_CITY
STORE Customer.cAddress4 TO m.cStState,m.SHP_STATE
STORE Customer.cAddress5 TO m.cStZip  ,m.SHP_ZIP
*B603515,1 (End)

*-- Get Customer information [End  ]

*-- Get Distribution Center information [Begin]

*B603515,1 Get DC Information
*=SEEK('S'+laData[4]+lcDistCtr,'Customer')
*lcDCName   = Customer.StName
*=gfGetAdr('CUSTOMER','','','',@laDcShipTo,'')
*m.STORE     = IIF(EMPTY(lcDistCtr),lcStore  ,lcDistCtr)
**--MAN
**m.SHP_NAME  = IIF(EMPTY(lcDistCtr),Customer.StName ,Customer.DCName)
*m.SHP_NAME  = lcDCName
*m.SHP_ADDR1 = IIF(EMPTY(lcDistCtr),laAcShipTo[1], laDcShipTo[1])
*m.SHP_ADDR2 = IIF(EMPTY(lcDistCtr),laAcShipTo[2], laDcShipTo[2])
*m.SHP_CITY  = IIF(EMPTY(lcDistCtr),laAcShipTo[3], laDcShipTo[3])
*m.SHP_STATE = IIF(EMPTY(lcDistCtr),laAcShipTo[4], laDcShipTo[4])
*m.SHP_ZIP   = IIF(EMPTY(lcDistCtr),laAcShipTo[5], laDcShipTo[5])

=SEEK('M'+laData[4],'Customer')
m.Int_Vend = Customer.cCusVend
IF SEEK('S'+laData[4]+lcDistCtr,'Customer')
	m.STORE     = lcDistCtr
	m.SHP_NAME  = Customer.STName
	m.SHP_ADDR1 = Customer.cAddress1
	m.SHP_ADDR2 = Customer.cAddress2
	m.SHP_CITY  = Customer.cAddress3
	m.SHP_STATE = Customer.cAddress4
	m.SHP_ZIP   = Customer.cAddress5
ENDIF
*B603515,1 (End)

*-- Get Distribution Center information [End  ]
*B803177,1 HBG 06/28/2000 comment it to Get the ShipVia which printed in the carton
*                         label from Packing list. not the order shipvia [Begin]
*m.ShipVia = Customer.ShipVia
m.ShipVia  = laShpVia[lnShpVia,2]
*B803177,1 HBG 06/28/2000  comment it to Get the ShipVia which printed in the carton
*                         label from Packing list. not the order shipvia [End  ]

lcwrname  = lcwarecode

*-- Get Carton information [Begin]
SELECT ORDHDR
SET ORDER TO ORDHDR
=SEEK('O'+laData[2])
lcOrStore   = OrdHdr.Store

*-- B603496,1 remove the EDI condition from printing carton labels [Begin]
lcUccVer = ''
*B605412,1 TMI [Start] Ask if account is Edi Account ( llEdiAcc = .T. )
*IF llEdiSys
IF llEdiSys AND llEdiAcc
	*B605412,1 TMI [End  ] Ask if account is Edi Account ( llEdiAcc = .T. )
	lcUccVer = IIF(lnDrctTo=1,EdiPH.cASNLbl1,EdiPH.cASNLbl2)
	m.lPlt   = EDIPH.lPltShp
ENDIF
*-- B603496,1 remove the EDI condition from printing carton labels [End  ]

lcUccVer    = IIF(EMPTY(lcUccVer),'XXX',lcUccVer)
m.CUSTPO    = OrdHdr.CustPo
m.DEPT      = OrdHdr.Dept
m.Note1     = OrdHdr.Note1
m.Note2     = OrdHdr.Note2
m.Int_Vend  = IIF(EMPTY(ORDHDR.INT_VEND),m.Int_Vend,ORDHDR.INT_VEND)
m.Cancelled = OrdHdr.Complete
m.EVENT_COD = OrdHdr.Event_Cod

*B803261,1 HBG 06/27/2000 The manifacturing ID has to be lead by zero and UCC9 label
*B803261,1                 has to take the right 5 digits of packing list no.[Begin]
*m.MANUF_ID  = lcManufId
*m.UCC9      = RIGHT(PADL(ALLTRIM(lcPackNo),6,'0'),5)+PADL(lnCarton,4,'0')
*m.UCC_CHECK = lfCheckNo('000'+lcManufId+m.Ucc9)

m.MANUF_ID  = PADL(ALLTRIM(lcManufId),7,'0')
*E039957,1  MHM 11/14/2005 Get the Carton # form the new file EDICRTSQ instead of calculating it [Begin]
*m.UCC9     = RIGHT(PADL(ALLTRIM(laData[1]),6,'0'),5)+PADL(lnCarton,4,'0')

*E039957,2  EIH 02/13/2006 Check if EDI installed or not [Begin] 
IF llEdiSys
*E039957,2  EIH [End] 
  *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
  IF llIncPLNum
    m.UCC9  = RIGHT(PADL(ALLTRIM(laData[1]),6,'0'),lnNumOfDig)+PADL(lnCarton,IIF(lnNumOfDig = 5,4,3),'0')
  ELSE 
  *E040123,1 HBG 03/13/2006 [End]

    SET ORDER TO PCKCRTSQ IN EDICRTSQ
    IF SEEK(laData[1]+STR(&lcCtnDtl..PackLineNo ,6),'EDICRTSQ')
      m.Ucc9 = PADL(EDICRTSQ.Ucc9,9,'0')
    ELSE
      m.Ucc9 = '000000001'
    ENDIF
  *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
  ENDIF
  *E040123,1 HBG 03/13/2006 [End]
*E039957,1  MHM 11/14/2005 [End]

  m.UCC_CHECK = lfCheckNo('000'+m.MANUF_ID+m.Ucc9)
*E039957,2  EIH 02/13/2006 Check if EDI installed or not [Begin] 
ENDIF
*E039957,2  EIH [End] 
*B803261,1 [End  ]


m.ASN_VER   = lcUccVer

*B603515,1 Order Ship via
*B803177,1 HBG 06/28/2000 comment it to Get the ShipVia which printed in the carton
*                         label from Packing list. not the order shipvia [Begin]
*m.ShipVia   = IIF(ALLTRIM(OrdHdr.ShipVia)='*',m.ShipVia,OrdHdr.ShipVia)
*B803177,1 HBG 06/28/2000 comment it to Get the ShipVia which printed in the carton
*                         label from Packing list. not the order shipvia [End  ]
*B603515,1 (End)

IF (lcFree_Clr!="C") OR EMPTY(lcColor) OR ("MIXED" $ lcColor)
	DIMENSION laCodDesc[1,3]
	laCodDesc = ""
	laCodDesc[1,1] = m.ShipVia
	laCodDesc[1,2] = "SHIPVIA"
	m.cClrDesc = lcColor
ELSE
	DIMENSION laCodDesc[2,3]
	laCodDesc = ""
	laCodDesc[1,1] = m.ShipVia
	laCodDesc[2,1] = SUBSTR(&lcCtnDtl..Style,lnNonMajSt,lnColorLen)
	laCodDesc[1,2] = "SHIPVIA"
	laCodDesc[2,2] = "COLOR"
	=gfCodDes(@laCodDesc)
	m.cClrDesc = laCodDesc[2,1]
ENDIF

=gfCodDes(@laCodDesc)
m.PACK_NO   = IIF(EMPTY(ladata[1]),laData[3],ladata[1])
m.Carrier   = laCodDesc[1,3]
m.Bol_No    = lcBol
m.Style     = lcStyle
*-- Get Carton information [End  ]

*B803213,4 FIX TOTQTY IN THE aSN_SHIP FILE [BEGIN]
=SEEK(STR(m.Cart_No,4),lcctnhdr)
*B803213,4 FIX TOTQTY IN THE aSN_SHIP FILE [END  ]

m.TotQty    = &lcCtnHdr..TotPcs
m.cSizeDesc = lcSize
m.cUPC = lcCrtnUpc
m.Pack_Id = lcCrtnSku

*-- Get No. of Cartons.
lcDetOrder = ORDER(lcCtnHdr)
USE (gcWorkDir+lcCtnHdr) ORDER (lcDetOrder) IN 0 AGAIN ALIAS (lcTmpDetFl)
GO BOTTOM IN (lcTmpDetFl)
m.Cartons = &lcTmpDetFl..Cart_No
USE IN (lcTmpDetFl)

SELECT (lcTmAsnShp)
IF SEEK(STR(m.CART_NO,6),lcTmAsnShp)
	GATHER MEMVAR MEMO
ELSE
	INSERT INTO (lcTmAsnShp) FROM MEMVAR
ENDIF
=gfAdd_Info(lcTmAsnShp)
IF TYPE("laLblInfo[1,4]") = "C" .AND. !EMPTY(laLblInfo[1,4])
	SAVE TO MEMO MLBLINFO ALL LIKE laLblInfo
ENDIF
IF llPrint
	*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [Begin]
	SELECT SycAsnHd
	GO TOP
	LOCATE FOR cver = lcUccVer AND cType = 'Y'
	IF FOUND()
		*E302210,1 WLD 09/09/2004 Using visual label report [Begin]
		*=lfPrintLbl(lcUccVer)
		*IF SEEK(lcUccVer,'SycAsnHd') AND SycAsnHd.cType = 'Y'
		SELECT (lcTmAsnShp)
		*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [End  ]
		=lfVsulLbl(lcUccVer,ALLTRIM(STR(m.Cart_No)))
	ELSE
		*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [Begin]
		LOCATE FOR cver = lcUccVer AND cType = 'N'
		IF FOUND()
			SELECT (lcTmAsnShp)
			*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [End  ]
			=lfPrintLbl(lcUccVer)
			*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [Begin]
		ENDIF
		*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [End  ]
	ENDIF
	*E302210,1 WLD 09/09/2004 Using visual label report [End]

	IF llntFound
		RETURN
	ENDIF
ENDIF
SELECT (lcPckLin)
*-- End Of lfvLblInfo

*!*************************************************************
*! Name      : lfPrintLbl
*! Developer : Ahmed Reda (ARD)
*! Date      : 01/20/2000
*! Purpose   : Print carton labels
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :
*!*************************************************************
*!C101735,1
*!
FUNCTION lfPrintLbl
PARAMETERS lcVersion
PRIVATE lnCurAlias, lnHandle, lcOutFile, lcString, lcData
lnCurAlias = SELECT(0)
WAIT 'Generating Shipping Label...Please stand by....' WINDOW NOWAIT
lcString  = SPACE(40)

SELECT SYCASNLB
IF !SEEK(lcVersion+'H') .AND. gfModalGen('INM44068B00000','DIALOG' )=1
	RETURN
	llntfound = .T.
ENDIF
lcOutFile = gcWorkDir+lcAsnLabel+".TXT"
lnHandle  = FCREATE(lcOutFile,0)
=FSEEK(lnHandle,0,2)

SCAN WHILE 	cVer+cEdiType = lcVersion+'H'
	STORE DATA TO lcData
	lcString = &lcData
	=FPUTS(lnHandle,lcString)
ENDSCAN

lcString = SPACE(3)
=FPUTS(lnHandle,lcString)

*B603501,1 Load Size description,quantity and SKU
SELECT (lcTmAsnShp)
SCATTER MEMVAR

*E037191,1 SSE 10/12/2003 Add MUCB no variable. [Begin]
MUCB = PADL(ALLTRIM(lfManufID(m.Bol_No)) , 7 , '0') + PADL(ALLTRIM(m.Bol_No) , 9 , '0')
MUCB = MUCB + lfCheckDgt(MUCB,'E')
*E037191,1 SSE 10/12/2003 Add MUCB no variable. [End]

=gfRltFld(ORDHDR.cDivision,@laDRltFld,'CDIVISION ')
m.DivLName = lcDivLName
=SEEK(OrdHdr.Account+OrdHdr.Dept,'CUSTDEPT')
m.DeptDesc = CustDept.cDeptDesc
=SEEK(ALLTRIM(STYLE),'Style')
m.StyDesc = Style.Desc

*B803213,1 Get carton information from carton details temp file
*SELECT pack_lin
*=SEEK(laData[1]+STR(&lcTmAsnShp..cart_no,4))
*SUM REST Weight TO m.Weight WHILE pack_no+STR(no_cart,4)+style = laData[1]+STR(&lcTmAsnShp..cart_no,4)
=SEEK(STR(&lcTmAsnShp..cart_no,4),lcCtnHdr)
m.Weight = &lcCtnHdr..TotWgh
*B803213,1 (End)

SELECT (lcTmAsnShp)
m.Date     = gdSysDate
m.Order    = laData[2]
m.Account  = laData[4]
m.Pattern = Style.Pattern
*B03113,1 (End)

STORE '' TO m.SizeDesc1,m.SizeDesc2,m.SizeDesc3,m.SizeDesc4,m.SizeDesc5,m.SizeDesc6,m.SizeDesc7,m.SizeDesc8
STORE '' TO m.SizeSku1,m.Sizesku2,m.Sizesku3,m.Sizesku4,m.Sizesku5,m.Sizesku6,m.Sizesku7,m.Sizesku8
STORE 0  TO m.SizeQty1,m.SizeQty2,m.SizeQty3,m.SizeQty4,m.SizeQty5,m.SizeQty6,m.SizeQty7,m.SizeQty8


*B803213,1 Remove temp carton details relation
SELECT(lcCtnDtl)
lnStyRec = RECNO(lcCtnDtl)
lnSzeRec = RECNO(lcCartonSz)
SET RELATION TO
*B803213,1 Take into consideration styles with exteded size scale while printning labels
*=SEEK('S'+Style.Scale,'Scale')
*IF SEEK(laData[1]+STR(cart_no,4)+ALLTRIM(Style),'pack_lin')
*  FOR lnCount = 1 TO 8
*    lcCount = STR(lnCount,1)
*    IF Pack_Lin.Qty&lcCount <> 0
*      m.SizeDesc&lcCount = ALLTRIM(Scale.Sz&lcCount)
*      m.SizeQty&lcCount  = Pack_Lin.Qty&lcCount
*      SELECT SPCK_LIN
*      =SEEK('S'+laData[4]+pack_lin.style)
*      LOCATE REST WHILE type+account+style+pack_id = 'S'+laData[4]+pack_lin.style FOR QTY&lcCount = 1
*      IF FOUND()
*        m.SizeSku&lcCount = ALLTRIM(Spck_LIn.Pack_Id)
*      ENDIF
*    ENDIF
*  ENDFOR
*ENDIF

IF llExtSizSc
	SELECT (lcCtnDtl)
	IF SEEK(STR(&lcTmAsnShp..cart_no,4)+ALLTRIM(&lcTmAsnShp..Style))
		lnSizeCount = 0
		lcCurrSty = SUBSTR(STYLE,1,lnSizePos-IIF(EMPTY(lcSizeSep),1,2))
		SCAN REST WHILE STR(Cart_No,4)+STYLE+STR(nOrdLineNo,6) = STR(&lcTmAsnShp..cart_no,4)+lcCurrSty ;
				AND lnSizeCount < 8
			=SEEK('S'+SUBSTR(STYLE,lnSizePos,lnSizeLen),'Scale')
			FOR lnCount = 1 TO Scale.Cnt
				lcCount = STR(lnCount,1)
				IF &lcCtnDtl..Qty&lcCount <> 0
					lnSizeCount = lnSizeCount + 1
					lcSizeCount = STR(lnSizeCount,1)
					m.SizeDesc&lcSizeCount = ALLTRIM(Scale.Sz&lcCount)
					m.SizeQty&lcSizeCount  = &lcCtnDtl..Qty&lcCount
					SELECT SPCK_LIN
					=SEEK('S'+laData[4]+&lcCtnDtl..style)
					LOCATE REST WHILE TYPE+account+STYLE+pack_id = 'S'+laData[4]+&lcCtnDtl..style FOR QTY&lcCount = 1
					IF FOUND()
						m.SizeSku&lcSizeCount = ALLTRIM(Spck_Lin.Pack_Id)
					ENDIF
				ENDIF
			ENDFOR
		ENDSCAN
	ENDIF
ELSE
	SELECT (lcTmAsnShp)
	=SEEK('S'+Style.Scale,'Scale')
	IF SEEK(STR(Cart_no,4)+ALLTRIM(STYLE),lcCtnDtl)
		FOR lnCount = 1 TO Scale.Cnt
			lcCount = STR(lnCount,1)
			IF &lcCtnDtl..Qty&lcCount <> 0
				m.SizeDesc&lcCount = ALLTRIM(Scale.Sz&lcCount)
				m.SizeQty&lcCount  = &lcCtnDtl..Qty&lcCount
				SELECT SPCK_LIN
				=SEEK('S'+laData[4]+&lcCtnDtl..style)
				LOCATE REST WHILE TYPE+account+STYLE+pack_id = 'S'+laData[4]+&lcCtnDtl..style FOR QTY&lcCount = 1
				IF FOUND()
					m.SizeSku&lcCount = ALLTRIM(Spck_Lin.Pack_Id)
				ENDIF
			ENDIF
		ENDFOR
	ENDIF
ENDIF
SELECT (lcTmAsnShp)
*B803213,1 (End)

*B603501,1 (End)

TMP_ADDR = ALLTRIM(VND_CITY) + ",  " +ALLTRIM(VND_STATE) + "   " + ALLTRIM(VND_ZIP)
SELECT SYCASNLB
=SEEK(lcVersion+'L')
SCAN WHILE cVer+cEdiType= lcVersion+'L'
	STORE DATA TO lcData
	lcString = &lcData
	=FPUTS(lnHandle,lcString)
ENDSCAN
lcString = SPACE(3)
=FPUTS(lnHandle,lcString)

*B606895,1 ABD - Check if the customer is edi customer. [Begin]
IF llEdiAcc
	*-- Check for the detail Version for current account.
	IF SEEK('A' + laData[4] , 'EDIACPRT') .AND. ;
			SEEK(EDIACPRT.cPartCode , 'EDIPH') .AND. ;
			Ediph.lDtlbl
		llDetLabel = .T.
		lcDetailVr = eDiph.cDtlbl
	ELSE
		llDetLabel = .F.
	ENDIF
ELSE
	*- Function to get the number of XX? into array.
	STORE '' TO lcDetailVr , lcDetLbAll
	DIMENSION laVersn[1]
	lnVerCount = lfGetVerXX ()
	llDetLabel = (lnVerCount >= 1)
ENDIF
*B606895,1 ABD - [End]

*C102275,1 HS 05/06/2001 Print the detailed label, if needed [Begin]
IF llDetLabel
	PRIVATE lnChoice , llPrintLbl , lnMajorLen , lnClrLen , lnClrPos , llUseColor ,;
		lcUPCStyle , lcGenColor , lcGenSty

	IF EMPTY(lcDetLbAll)
		lnChoice = gfModalGen('TRM44105B40016' , 'DIALOG' , ALLTRIM(STR(&lcTmAsnShp..Cart_No , 4)))
		DO CASE
		CASE lnChoice = 1
			llPrintLbl = .T.
		CASE lnChoice = 2
			llPrintLbl = .T.
			lcDetLbAll = "Y"
		CASE lnChoice = 3
			llPrintLbl = .F.
		CASE lnChoice = 4
			llPrintLbl = .F.
			lcDetLbAll = "N"
		ENDCASE
	ELSE
		llPrintLbl = (lcDetLbAll = "Y")
	ENDIF

	IF llPrintLbl

		*B606895,1 ABD - Check to print the Detail version type in case account is not EDI account. [Begin]
		IF !llEdiAcc .AND. lnVerCount >= 1
			*-- the customer should print one from the current version.
			= lfSeleVer ()
		ENDIF
		*B606895,1 ABD - [End]

		STORE '' TO MDSTYLE1 , MDSTYLE2 , MDSTYLE3 , MDSTYLE4 , MDSTYLE5 ,;
			MDSTYLE6 , MDSTYLE7 , MDSTYLE8 , MDSTYLE9 , MDSTYLE10,;
			MDSTYLE11, MDSTYLE12, MDSTYLE13, MDSTYLE14, MDSTYLE15,;
			MDSTYLE16, MDSTYLE17, MDSTYLE18, MDSTYLE19, MDSTYLE20

		STORE '' TO MDSTYMAJ1 , MDSTYMAJ2 , MDSTYMAJ3 , MDSTYMAJ4 , MDSTYMAJ5 ,;
			MDSTYMAJ6 , MDSTYMAJ7 , MDSTYMAJ8 , MDSTYMAJ9 , MDSTYMAJ10,;
			MDSTYMAJ11, MDSTYMAJ12, MDSTYMAJ13, MDSTYMAJ14, MDSTYMAJ15,;
			MDSTYMAJ16, MDSTYMAJ17, MDSTYMAJ18, MDSTYMAJ19, MDSTYMAJ20

		STORE '' TO MDCOLOR1 , MDCOLOR2 , MDCOLOR3 , MDCOLOR4 , MDCOLOR5 ,;
			MDCOLOR6 , MDCOLOR7 , MDCOLOR8 , MDCOLOR9 , MDCOLOR10,;
			MDCOLOR11, MDCOLOR12, MDCOLOR13, MDCOLOR14, MDCOLOR15,;
			MDCOLOR16, MDCOLOR17, MDCOLOR18, MDCOLOR19, MDCOLOR20

		STORE '' TO MDSKU1 , MDSKU2 , MDSKU3 , MDSKU4 , MDSKU5 ,;
			MDSKU6 , MDSKU7 , MDSKU8 , MDSKU9 , MDSKU10,;
			MDSKU11, MDSKU12, MDSKU13, MDSKU14, MDSKU15,;
			MDSKU16, MDSKU17, MDSKU18, MDSKU19, MDSKU20

		STORE '' TO MDSTYUPC1 , MDSTYUPC2 , MDSTYUPC3 , MDSTYUPC4 , MDSTYUPC5 ,;
			MDSTYUPC6 , MDSTYUPC7 , MDSTYUPC8 , MDSTYUPC9 , MDSTYUPC10,;
			MDSTYUPC11, MDSTYUPC12, MDSTYUPC13, MDSTYUPC14, MDSTYUPC15,;
			MDSTYUPC16, MDSTYUPC17, MDSTYUPC18, MDSTYUPC19, MDSTYUPC20

		STORE '' TO MDSIZDES1 , MDSIZDES2 , MDSIZDES3 , MDSIZDES4 , MDSIZDES5 ,;
			MDSIZDES6 , MDSIZDES7 , MDSIZDES8 , MDSIZDES9 , MDSIZDES10,;
			MDSIZDES11, MDSIZDES12, MDSIZDES13, MDSIZDES14, MDSIZDES15,;
			MDSIZDES16, MDSIZDES17, MDSIZDES18, MDSIZDES19, MDSIZDES20

		STORE 0  TO MDQTY1 , MDQTY2 , MDQTY3 , MDQTY4 , MDQTY5 ,;
			MDQTY6 , MDQTY7 , MDQTY8 , MDQTY9 , MDQTY10,;
			MDQTY11, MDQTY12, MDQTY13, MDQTY14, MDQTY15,;
			MDQTY16, MDQTY17, MDQTY18, MDQTY19, MDQTY20

		*-- Get major length
		lnMajorLen = LEN(lcMask)

		*-- Get color start position and length
		STORE 0   TO lnClrLen , lnClrPos
		STORE .F. TO llUseColor
		DECLARE laItemSeg[1]
		=gfItemMask(@laItemSeg)
		FOR lnCount = 1 TO ALEN(laItemSeg , 1)
			IF laItemSeg[lnCount,1] = 'C'
				llUseColor = .T.
				lnClrLen   = LEN(laItemSeg[lnCount,3])
				lnClrPos   = laItemSeg[lnCount,4]
				EXIT
			ENDIF
		ENDFOR

		lcGenColor = PADR(gfGetMemVar("MCLRASSCOD") , 6)
		SELECT (lcCtnDtl)
		IF SEEK(STR(&lcTmAsnShp..Cart_No , 4))
			lnSizeCount = 0
			SCAN REST WHILE STR(Cart_No , 4) = STR(&lcTmAsnShp..Cart_No , 4) .AND. lnSizeCount < 20
				=SEEK(&lcCtnDtl..Style , 'STYLE')
				=SEEK('S' + Style.Scale , 'Scale')

				IF llUPCInst
					lcUPCStyle = &lcCtnDtl..Style
					*C102275,1 HS  Call a trigger to get the UPC style
					IF ASCAN(laEvntTrig , PADR('GETUPCST',10)) <> 0
						=gfDoTriger('ALPLIST',PADR('GETUPCST',10))
					ENDIF
					IF llUseColor
						lcGenSty = SUBSTR(lcUPCStyle , 1 , lnClrPos - 1) + lcGenColor +;
							SUBSTR(lcUPCStyle , lnClrPos + lnCLrLen)
					ENDIF
				ENDIF

				FOR lnCount = 1 TO Scale.Cnt
					lcCount = STR(lnCount , 1)
					IF lnSizeCount < 20 .AND. &lcCtnDtl..Qty&lcCount <> 0
						lnSizeCount = lnSizeCount + 1
						lcSizeCount = ALLTRIM(STR(lnSizeCount,2))

						MDStyle&lcSizeCount  = &lcCtnDtl..Style
						MDStyMaj&lcSizeCount = LEFT(&lcCtnDtl..Style , lnMajorLen)
						MDSizDes&lcSizeCount = ALLTRIM(Scale.Sz&lcCount)
						MDQty&lcSizeCount    = &lcCtnDtl..Qty&lcCount

						IF llUseColor
							MDColor&lcSizeCount = SUBSTR(&lcCtnDtl..Style , lnClrPos , lnClrLen)
						ENDIF

						IF llUPCInst
							IF SEEK(lcUPCStyle + lcCount , "StyleUPC")
								MDStyUPC&lcSizeCount = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
							ELSE
								IF llUseColor .AND. SEEK(lcGenSty + lcCount , "StyleUPC")
									MDStyUPC&lcSizeCount = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 +;
										StyleUPC.cUPCNum3
								ENDIF
							ENDIF
						ENDIF

						SELECT SPCK_LIN
						=SEEK('S' + laData[4] + &lcCtnDtl..Style)
						LOCATE REST;
							WHILE TYPE + Account + STYLE + Pack_ID = 'S' + laData[4] + &lcCtnDtl..Style;
							FOR QTY&lcCount = 1

						IF FOUND()
							MDSKU&lcSizeCount = ALLTRIM(Spck_Lin.Pack_Id)
						ENDIF
					ENDIF
				ENDFOR
			ENDSCAN
		ENDIF

		SELECT SYCASNLB
		*B606895,1 ABD - collect data for the selected Version.  [Begin]
		*=SEEK("XX1" + 'H')
		*SCAN WHILE cVer + cEdiType = "XX1" + 'H'
		=SEEK(lcDetailVr + 'H')
		SCAN WHILE cVer + cEdiType = lcDetailVr + 'H'
			*B606895,1 ABD - [End]
			STORE DATA TO lcData
			lcString = &lcData
			=FPUTS(lnHandle,lcString)
		ENDSCAN
		lcString = SPACE(3)
		=FPUTS(lnHandle,lcString)

		*B606895,1 ABD - collect data for the selected Version.  [Begin]
		*=SEEK("XX1" + 'L')
		*SCAN WHILE cVer + cEdiType = "XX1" + 'L'
		=SEEK(lcDetailVr  + 'L')
		SCAN WHILE cVer + cEdiType = lcDetailVr + 'L'
			*B606895,1 ABD - [End]
			STORE DATA TO lcData
			lcString = &lcData
			=FPUTS(lnHandle,lcString)
		ENDSCAN
	ENDIF
ENDIF
*C102275,1 HS 05/06/2001 Print the detailed label, if needed [End]

=FCLOSE(lnHandle)
WAIT 'Sending Shipping Labels to Printer...Please stand by....' WINDOW NOWAIT
*! MODE COM1:24,N,8,1,P >>null
*! MODE LPT1:=COM1: >>null
*! PRINT /D:LPT1 /S:1 >>null
lcCommand = "TYPE " + lcOutFile + " > " + lcSndPort
!/N0 &lcCommand

WAIT CLEAR

*B803213,1 Restore temp carton details relation
SELECT (lcCtnDtl)
SET RELATION TO '' INTO (lcCartonSz)
SET SKIP TO (lcCartonSz)
IF lnStyRec <= RECCOUNT(lcCtnDtl)
	GOTO lnStyRec
	SKIP lnSzeRec-1
ENDIF
= RLOCK(lcCtnDtl)
UNLOCK IN (lcCtnDtl)
*B803213,1 (End)

SELECT(lnCurAlias)
RETURN
*-- End Of Function lfPrintLbl

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Ahmed Reda (ARD)
*! Date      : 01/20/2000
*! Purpose   : Print carton labels
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfEvalSegs()
*!*************************************************************
*!C101735,1
*!
FUNCTION lfEvalSegs

lcNonMajPi = ""

lnMajSeg = gfItemMask('SM')  && No. of major segments.
lnMajLen = LEN(gfItemMask('PM'))

*-- Compute Free/Color Items in Style code Structure. [Begin]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
	IF laMajSegs[lnI,1] $ 'CF'
		lcFree_Clr = laMajSegs[lnI,1]
		lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
		lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
			laMajSegs[lnI,3],;
			lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
	ENDIF
	*-- If you Find Color Type or Find Free Type and current type not Free.
	IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
		EXIT
	ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

lnColorLen = LEN(lcNonMajPi)

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfCheckNo
*! Developer : Ahmed Reda (ARD)
*! Date      : 02/24/1999
*! Purpose   : Algorithm to Compute the modulus-10 Check digit for the
*!             UCC 128 code
*!*************************************************************
*! Parameters: lcUccNo : UCC number without check digit
*!*************************************************************
*! Returns   : UCC Check digit
*!*************************************************************
*! Example   :  =lfCheckNo('1234567890123456789')
*!*************************************************************
*!C101735,1
*!
FUNCTION lfCheckNo
PARAMETER lcUccNo
PRIVATE lnChkDigit,lnSumOdd,lnSumEven,lnCount

STORE 0 TO lnSumOdd,lnSumEven,lnChkDigit
FOR lnCount = 1 TO 9
	lnSumOdd  = lnSumOdd + VAL(SUBSTR(lcUccNo,lnCount*2-1,1))
	lnSumEven = lnSumEven+ VAL(SUBSTR(lcUccNo,lnCount*2,1))
ENDFOR
lnSumOdd   = lnSumOdd + VAL(SUBSTR(lcUccNo,19,1))
lnChkDigit = MOD(lnSumOdd*3+lnSumEven,10)
RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))
*-- end of lfCheckNo.

*!*************************************************************
*! Name      : lfGetStySz
*! Developer : Ahmed Rea (ARD)
*! Date      : 01/20/2000
*! Purpose   : Clculatre the style size.
*!*************************************************************
*! Example     : = lfGetStySz()
*!*************************************************************
*!C101735,1
*!
FUNCTION lfGetStySz
PRIVATE lcCurrAlis , lnRecPoint , lnDetPoint

*-- Save current setting
lcCurrAlis = SELECT(0)

SELECT (lcCtnDtl)
lnRecPoint = RECNO()
lnDetPoint = RECNO(lcCartonSz)

*E302210,1 WLD 09/09/2004 Using visual label report
lcSetSkip = SET('SKIP')
SET SKIP TO
*E302210,1 WLD (End)

*B603501,1 CHANGE THE SEARCH TO LOOK FOR THE CARTONS [BEGIN]
*LOCATE FOR EVAL(lcCtnDtl+'.Br'+&lcCartonSz..SzCode) AND &lcCartonSz..SzCode <= STR(&lcCtnDtl..SzCnt,1)
SEEK STR(lncarton,4)
*B603501,1 CHANGE THE SEARCH TO LOOK FOR THE CARTONS [END]

lcStyle  = SUBSTR(STYLE,1,lnMajLen)
lcUpdSty = STYLE
IF lcFree_Clr="C"
	lcColor = SUBSTR(STYLE,lnNonMajSt,lnColorLen)
ELSE
	lcColor = SPACE(6)
ENDIF
lnLblInfo = 0
DIMENSION laLblInfo[1, 6]
lcCrtnSku = ""
lcCrtnUpc = ""
lcSize = ''
*SCAN FOR EVAL(lcCtnDtl+'.Br'+&lcCartonSz..SzCode) AND &lcCartonSz..SzCode <= STR(&lcCtnDtl..SzCnt,1)
SCAN REST WHILE STR(cart_no,4) = STR(lncarton,4)
	*B603501,1 CHANGE THE SEARCH TO LOOK FOR THE CARTONS [END]

	lcStyle  = IIF(lcStyle<>SUBSTR(STYLE,1,lnMajLen),SPACE(lnMajLen),lcStyle)
	lcUpdSty = IIF(lcUpdSty<>STYLE,SPACE(19),lcUpdSty)
	IF lcFree_Clr="C"
		lcColor = IIF(EMPTY(lcStyle),SPACE(lnColorLen),;
			IIF(lcColor<>SUBSTR(STYLE,lnNonMajSt,lnColorLen),'MIXED',lcColor))
	ENDIF
	*E302210,1 WLD 09/09/2004 Using visual label report
	FOR lnSzNo1 = 1 TO 8
		lcSzNo = STR(lnSzNo1,1)
		IF Br&lcSzNO
			lcSKU = SPACE(16)
			IF SEEK('S'+laData[4] + &lcCtnDtl..Style, 'Spck_Lin')
				SELECT SPCK_LIN
				LOCATE REST ;
					WHILE TYPE + Account + STYLE = 'S' + laData[4] + &lcCtnDtl..Style FOR Qty&lcSzNo = 1
				IF FOUND()
					lcSKU = PADR(Pack_Id, 16)
				ELSE
					=SEEK('S' + laData[4] + &lcCtnDtl..Style, 'Spck_Lin')
					LOCATE REST;
						WHILE TYPE + Account + STYLE = 'S' + laData[4] + &lcCtnDtl..Style FOR TotQty = 0
					IF FOUND()
						lcSKU = PADR(Pack_Id, 16)
					ENDIF
				ENDIF
			ENDIF
			lcUpc = SPACE(13)
			IF llUPCInst AND SEEK(&lcCtnDtl..Style+lcSzNo,'StyleUpc')
				lcUpc = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
			ENDIF
			SELECT (lcCtnDtl)
			lnLblInfo = lnLblInfo + 1
			DIMENSION laLblInfo[lnLblInfo, 6]
			laLblInfo[lnLblInfo,1] = lcSKU
			laLblInfo[lnLblInfo,2] = Qty&lcSzNo
			laLblInfo[lnLblInfo,3] = Size&lcSzNo
			laLblInfo[lnLblInfo,4] = STYLE
			laLblInfo[lnLblInfo,5] = lcUpc
			laLblInfo[lnLblInfo,6] = lnSzNo1

			lcSize    = IIF(Size&lcSzNo <> lcSize   ,'MIXED',Size&lcSzNo)
			lcCrtnSku = IIF(lcSKU <> lcCrtnSku ,SPACE(16),lcSKU)
			lcCrtnUpc = IIF(lcUpc <> lcCrtnUpc,SPACE(13),lcUpc)
		ENDIF
	ENDFOR
	*E302210,1 WLD 09/09/2004 Using visual label report
ENDSCAN
lcSize = IIF(EMPTY(lcStyle),SPACE(5), lcSize)

*E302210,1 WLD 09/09/2004 Using visual label report
IF !EMPTY(lcSetSkip)
	SET SKIP TO (lcSetSkip)
ENDIF
*E302210,1 WLD (End)

*-- Restore Setting.
IF BETWEEN(lnRecPoint,1,RECCOUNT())
	GO lnRecPoint
ENDIF

IF BETWEEN(lnDetPoint,1,RECCOUNT(lcCartonSz))
	GO lnDetPoint IN (lcCartonSz)
ENDIF

SELECT (lcCurrAlis)
*-- end of lfGetStySz.


*!*************************************************************
*! Name      : lfSavCartn
*! Developer : Ahmed Reda (ARD)
*! Date      : 01/20/2000
*! Purpose   : Saving Cartons  in ASN_SHIP
*!*************************************************************
*! Example   : = lfSavCartn()
*!*************************************************************
*!C101735,1
*!
FUNCTION lfSavCartn
PRIVATE llchoice, lnCartons, lnCarRef , lnActAlias , lcDetOrder , llPrinSel
lnActAlias = SELECT(0)
*E039957,1  MHM 11/14/2005 Fix bug of of not deleting old cartons from ASN_SHIP file [Begin]
lcSetDele = SET('DELETE')
*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
IF !llIncPLNum
*E040123,1 HBG 03/13/2006 [End]
  SET DELETE ON
  SELECT (lcTmAsnShp)
  ZAP
  SELECT Asn_Ship
  DELETE FOR bol_no+pack_no+STR(cart_no,6)+asn_ver = lcBol+laData[1]
*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
ENDIF
*E040123,1 HBG 03/13/2006 [End]
*E039957,1  MHM 11/14/2005 [End]


STORE SPACE (0) TO laSource,laTarget
lnCartons =0
GO BOTTOM IN (lcCtnDtl)
lnCartons = &lcCtnDtl..Cart_No
IF lnCartons <> 0
	DIMENSION laSource[lnCartons] , laTarget[1]
ENDIF
DIMENSION laTarget[1]

lnCarRef = 0
lcDetOrder = ORDER(lcCtnDtl)
SET ORDER TO (lcCtnDtl) IN (lcCtnDtl)
*-- Saving code.
FOR lnCarRef = 1 TO lnCartons
	=SEEK(STR(lnCarRef,4),lcCtnDtl) AND lfvLblInfo(lnCarRef)
	laSource[lnCarRef] =  "Carton # " + PADL(ALLTRIM(STR(&lcCtnDtl..CART_NO)),4)
	IF SEEK(STR(lnCarRef,6),lcTmAsnShp)
		SELECT (lcTmAsnShp)
		SCATTER MEMVAR MEMO
		IF SEEK(&lcTmAsnShp..BOL_NO+&lcTmAsnShp..PACK_NO+STR(&lcTmAsnShp..CART_NO,6),"Asn_Ship")
			SELECT Asn_Ship
			GATHER MEMVAR MEMO
		ELSE
			INSERT INTO Asn_Ship FROM MEMVAR
		ENDIF
	ENDIF
ENDFOR

*E039957,1  MHM 11/14/2005 Fix bug of of not deleting old cartons from ASN_SHIP file [Begin]
SET DELETE &lcSetDele
*E039957,1  MHM 11/14/2005 [End]

llchoice = .T.
*E302210,1 WLD 09/09/2004 Using visual label report [Begin]
*IF !llScrPrnLb
IF llScrPrnLb
	*E302210,1 WLD 09/09/2004 Using visual label report [End]

	DO WHILE llchoice
		lnCarRef = 0
		lnChoice = gfModalGen("QRM44097B44011","Dialog")
		DO CASE

			*-- Case of YES
		CASE lnChoice=1
			*E302210,1 WLD 09/09/2004 Using visual label report [Begin]
			IF SEEK(ASN_VER,'SycAsnHd') AND SycAsnHd.cType = 'Y'
				=lfVsulLbl(ASN_VER,'')
			ELSE
				*E302210,1 WLD 09/09/2004 Using visual label report [End]

				FOR lnCarRef = 1 TO lnCartons
					IF SEEK(STR(lnCarRef,6),lcTmAsnShp)
						SELECT (lcTmAsnShp)
						=lfPrintLbl(ASN_VER)
					ENDIF
				ENDFOR
			ENDIF
			*E302210,1 WLD 09/09/2004 Using visual label report [Begin]
			llchoice = .F.
			*E302210,1 WLD 09/09/2004 Using visual label report [End]


			*-- Case of Selective
		CASE lnChoice=2

			DIMENSION laTarBef[ALEN(laTarget,1)]
			=ACOPY(laTarget,laTarBef)
			= gfMover(@laSource,@laTarget,'Select Carton(s).',.T.,'')
			IF ALEN(laTarBef,1) = ALEN(laTarget,1)
				llPrinSel = .F.
				IF !EMPTY(laTarget[1])
					FOR lnJ = 1 TO ALEN(laTarget,1)
						IF ASCAN(laTarBef,laTarget[lnJ]) = 0)
							llPrinSel = .T.
							EXIT
						ENDIF
					ENDFOR
				ENDIF
			ELSE
				llPrinSel = !EMPTY(laTarget[1])
			ENDIF
			IF llPrinSel
				*E302210,1 WLD 09/09/2004 Using visual label report [Begin]
				IF SEEK(ASN_VER,'SycAsnHd') AND SycAsnHd.cType = 'Y'
					lcCrtnRng = ''
					FOR lnCarRef = 1 TO ALEN(laTarget,1)
						lcCrtnRng = lcCrtnRng + ALLTRIM(RIGHT(laTarget[lnCarRef],4)) + ','
					ENDFOR
					=lfVsulLbl(ASN_VER,LEFT(lcCrtnRng,LEN(lcCrtnRng)-1))
				ELSE
					*E302210,1 WLD 09/09/2004 Using visual label report [End]
					FOR lnCarRef = 1 TO ALEN(laTarget,1)
						lnToPrnCrt = VAL(RIGHT(laTarget[lnCarRef],4))
						IF SEEK(STR(lnToPrnCrt,6),lcTmAsnShp)
							SELECT (lcTmAsnShp)
							=lfPrintLbl(ASN_VER)
						ENDIF
					ENDFOR
				ENDIF
			ENDIF

			*-- Case of proceed
		CASE lnChoice=3
			llchoice = .F.

		ENDCASE
	ENDDO

ENDIF
SELECT (lnActAlias)
*-- end of lfSavCartn.

*!*************************************************************
*! Name      : lpPortScr
*! Developer : Ahmed Reda (ARD)
*! Date      : 01/20/2000
*! Purpose   : Handle the printing  labels screen setup
*!*************************************************************
*! Example   : = lpPortScr
*!*************************************************************
*!C101735,1
*!
PROCEDURE lpPortScr
PRIVATE lcOldPort , llOldPrn , llCancel
lcOldPort = lcSndPort
llOldPrn  = llScrPrnLb
llCancel  = .F.
*B604186,1 HBG 02/07/2001 The program was transfered to root. [Begin]
*DO (gcScrDir + gcWinAppl + '\ALOUTPRT.SPX')
*B804238,1 ASH 11/19/2001 (Begin) Fix the bug of "\SCREENS\\ALOUTPRT.SPX "
IF RIGHT(gcScrDir,1)='\'
	DO (gcScrDir + 'ALOUTPRT.SPX')
ELSE
	*B804238,1 ASH 11/19/2001 (End)
	DO (gcScrDir + '\ALOUTPRT.SPX')
ENDIF
*B604186,1 [End]
IF llCancel
	lcSndPort  = lcOldPort
	llScrPrnLb = llOldPrn
ENDIF
*-- end of lpPortScr.

*!*************************************************************
*! Name      : lfvBol
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 02/01/2000
*! Purpose   : Validation function B.O.L field
*!*************************************************************
*! Called from : Screen
*!*************************************************************
*! Calls       : lfvBol
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvBol

*B803177,1 HBG 07/05/2000 Display selected Bol in B.O.L field in the screen [Begin]
IF !MDOWN() AND !EMPTY(lcBol)
	IF llEdiSys AND !llBolnever
		*B804306,1 HBG 04/03/2002 Pass new parameter to lfGetBol to save field of To Store or
		*B804306,1                consalidatore in BOL_HDR.[Begin]
		*lcBol = lfGetBOL("",laData[4],laData[5],lcWareCode,laData[7],IIF(lnCtnTyp=1,'Y','N'),"laFields")
		lcBol = lfGetBOL("",laData[4],laData[5],lcWareCode,laData[7],IIF(lnCtnTyp=1,'Y','N'),"laFields",.F.,.F.,IIF(lnDrctTo=2,"C","S"))
		*B804306,1 [End]
	ENDIF
	lcBol = IIF(EMPTY(lcBol),lcOldValue,lcBol)
ENDIF

*PRIVATE lcVar , lcObj , laTemp,lcBrowFields, llWoNotFnd
*IF llEdiSys
*  lcVar = SYS(18)
*  lcObj = EVALUATE(SYS(18))
*  lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
*  lcPrevAl = SELECT(0)
*  STORE '' TO lcBrowCond

*  llB_H_Opn = gfOpenFile(gcDataDir+'Bol_Hdr',gcDataDir+'Bol_Hdr','SH')
*  SELECT BOL_HDR
*  SET ORDER TO BOL_HDR

*  llWoNotFnd = ('?' $ lcObj) OR !(SEEK(lcObj))
*  DIMENSION laTemp[1]
*-- Setting the fields for browsing [Begin]
*  IF !EMPTY(lcObj) AND llWoNotFnd
*   lcBrFields ="BOL_NO    :R :H='BOL #'        ,"+;
*                "Account   :R :H='Account'      ,"+;
*                "Store     :R :H='Store'        ,"+;
*                "Tot_Wght  :R :H='Tot. Wght'    ,"+;
*                "Tot_Cart  :R :H='Tot. Cartons' ,"+;
*                "Tot_Pcs   :R :H='Tot. Pcs.'    ,"+;
*                "W_Code    :R :H='Were house'"

*    =gfBrows(lcBrowCond,'BOL_no','laTemp')
*    lcObj = IIF(EMPTY(laTemp[1]),"",laTemp[1])
*  ENDIF
*-- Setting the fields for browsing [End  ]

*-- IF The user selected a record

*  SET ORDER TO BOL_HDR
*  =SEEK(lcObj,'bol_hdr')

*  lcObj=IIF(Account+Store+W_Code=laData[4]+lcDistCtr+lcWareCode, lcObj, lcOldVal)
*  &lcVar = lcObj
*  SELECT (lcPrevAl)
*ENDIF
*B803177,1 HBG 07/05/2000 Display selected Bol in B.O.L field in the screen [End  ]

*-- End Of lfvBol

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : Ahmed M. Reda.(ARD)
*! Date      : 02/01/2000
*! Purpose   : Validation function for cut teckit#.
*!*************************************************************
*! Called from : Cut Teckit # [Option Grid]
*!*************************************************************
*! Calls       : lfvBol
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfWOldVal
lcOldValue = EVALUATE(SYS(18))
*-- end of lfWOldVal


*!*************************************************************
*! Name      : lfvStyFltr
*! Developer : Nader Anis (NAD)
*! Date      : 02/09/2000
*! Purpose   : Valid Function for the filter push buttoms
*! Refer To  : E301373
*!*************************************************************
*! Example   : = lfvStyFltr()
*!*************************************************************
FUNCTION lfvStyFltr
PARAMETERS lcType
DO CASE
CASE lcType = 'M'
	SELECT DISTINCT SUBSTR(STYLE,1,LEN(lcMask)) FROM (lcPckLin) INTO ARRAY laStySrc
	=gfMover(@laStySrc,@laStyTrgt,lcStyFltr,.T.,'')
CASE lcType = 'N'
	SELECT DISTINCT SUBSTR(STYLE,LEN(lcMask)+2,LEN(STYLE)) FROM &lcPckLin INTO ARRAY laClrSrc
	=gfMover(@laClrSrc,@laClrTrgt,lcStMjFltr,.T.,'')
ENDCASE
*--END FUNCTION  lfvStyFltr

*!*************************************************************
*! Name      : lpFltrScr
*! Developer : Nader Anis (NAD)
*! Date      : 02/09/2000
*! Purpose   : to load the filter screen
*! Called by : the menu bar Style Filter
*! Refer To  : E301373
*!*************************************************************
*! Example   : = lpFltrScr()
*!*************************************************************
PROCEDURE lpFltrScr

lcStyFltr = 'Only these '+ALLTRIM(gfitemmask("HN"))
lcStMjFltr= 'Only these '+ALLTRIM(gfitemmask("HM"))
DIMENSION laOldSty[ALEN(laStyTrgt)]
*DIMENSION laOldClr[ALEN(laClrSrc)]
DIMENSION laOldClr[ALEN(laClrTrgt)]
=ACOPY(laStyTrgt,laOldSty)
=ACOPY(laClrTrgt,laOldClr)
*B604186,1 HBG 02/07/2001 The program was transfered to root. [Begin]
*DO (gcScrDir + gcWinAppl + '\ALSTYFLTR.SPR')
DO (gcScrDir + '\ALSTYFLTR.SPR')
*B604186,1 [End]
GO TOP IN (LCSCAFILE)

*--END PROCEUDRE lpFltrScr
*!****************************************************************************
*! Name      : lfvpbOk
*! Developer : Nader Anis (NAD)
*! Date      : 02/09/2000
*! Purpose   : Valid function for the ok push buttom in the filter screen
*! Refer To  : E301373
*!*****************************************************************************
*! Example   : = lfvpbOk()
*!*****************************************************************************
FUNCTION lfvpbOk

lcOAlias=ALIAS()
*B605583,1 HBG 24/02/2002 Select the file which browse from[Begin]
*SELECT (lcPckLin)
SELECT (lcTmpPck)
*B605583,1 [End]

DO CASE
CASE !EMPTY(laStyTrgt(1)) AND !EMPTY(laClrTrgt(1))
	SET FILTER TO ASCAN(laStyTrgt,ALLTRIM(SUBSTR(STYLE,1,LEN(lcMask)))) > 0 AND ;
		ASCAN(laClrTrgt,ALLTRIM(SUBSTR(STYLE,LEN(lcMask)+2))) > 0
	LOCATE FOR IIF(llShwOpn,EVAL('OQty'+&lcScaFile..SzCode)>0 AND EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1),;
		EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1))
	IF EOF()
		* --No Records To Browse
		=gfModalGen('TRM44032B00000','DIALOG' )
		=lfvpbRst()
		RETURN
	ENDIF
CASE EMPTY(laStyTrgt[1]) AND EMPTY(laClrTrgt[1])
	SET FILTER TO
CASE EMPTY(laStyTrgt[1]) AND !EMPTY(laClrTrgt[1])
	SET FILTER TO ASCAN(laClrTrgt,ALLTRIM(SUBSTR(STYLE,LEN(lcMask)+2))) > 0
	LOCATE FOR  IIF(llShwOpn,EVAL('OQty'+&lcScaFile..SzCode)>0 AND EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1),;
		EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1))
CASE !EMPTY(laStyTrgt[1]) AND EMPTY(laClrTrgt[1])
	SET FILTER TO ASCAN(laStyTrgt,ALLTRIM(SUBSTR(STYLE,1,LEN(lcMask)))) > 0
	LOCATE FOR IIF(llShwOpn,EVAL('OQty'+&lcScaFile..SzCode)>0 AND EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1),;
		EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1))
ENDCASE
GO TOP IN (LCSCAFILE)
SHOW WINDOW (lcDtlBrTtl) REFRESH SAME
SELECT (lcOAlias)
*--END FUNCTION  lfvpbOk

*!*************************************************************
*! Name      : lfReset
*! Developer : Nader Anis (NAD)
*! Date      : 02/09/2000
*! Purpose   : Valid function for the Reset push buttom in the filter screen
*! Refer To  : E301373
*!*************************************************************
*! Example   : = lfReset()
*!*************************************************************
FUNCTION lfReset

DIMENSION laStyTrgt(1)
DIMENSION laClrTrgt(1)
STORE SPACE(0) TO laStyTrgt,laClrTrgt
lcOAlias=ALIAS()
*B605583,1 HBG 24/02/2002 Select the file which browse from[Begin]
*SELECT (lcPckLin)
SELECT (lcTmpPck)
*B605583,1 [End]
SET FILTER TO
SELECT (lcOAlias)
*--END FUNCTION  lfReset

*!*************************************************************
*! Name      : lfvpbRst
*! Developer : Nader Anis (NAD)
*! Date      : 02/09/2000
*! Purpose   : Valid function for the Reset push buttom in the filter screen
*! Refer To  : E301373
*!*************************************************************
*! Example   : = lfvpbRst()
*!*************************************************************
FUNCTION lfvpbRst

=lfReset()
SHOW WINDOW (lcDtlBrTtl) REFRESH SAME

*--END FUNCTION lfvpbRst

*!******************************************************************************
*! Name      : lfvpbCncl
*! Developer : Nader Anis (NAD)
*! Date      : 02/09/2000
*! Purpose   : Valid function for the cancel push buttom in the filter screen
*! Refer To  : E301373
*!*******************************************************************************
*! Example   : = lfvpbCncl()
*!*******************************************************************************
FUNCTION lfvpbCncl

DIMENSION laStyTrgt[ALEN(laOldSty)]
DIMENSION laClrTrgt[ALEN(laOldClr)]
=ACOPY(laOldSty,laStyTrgt)
=ACOPY(laOldClr,laClrTrgt)
*--END FUNCTION  lfvpbCncl

*!******************************************************************************
*! Name      : lpUpdtPkTk
*! Developer : Ahmed Reda (ARD)
*! Date      : 02/25/2000
*! Purpose   : Valid function for the update piktkt menu bar.
*! Refer To  : B602797
*!*******************************************************************************
*! Example   : = lfUpdtPkTk()
*!*******************************************************************************
*!B602797,1
PROCEDURE lpUpdtPkTk
llUpdtPkTk = !llUpdtPkTk
SET MARK OF BAR IIF(llCanPrnLb,6,4) OF _OPTPOP TO llUpdtPkTk
*-- End of lfUpdtPkTk.


*!*************************************************************
*! Name      : lfSuppForc
*! Developer : Mohamed Badran (MAB)
*! Date      : 04/24/2000
*! Purpose   : User can Force allocation. (Y/N)
*!*************************************************************
*!
*BADRAN 04/24/2000
FUNCTION lfSuppForc
PRIVATE llAlwForce
llAlwForce = .T.
IF laSetUp[5,2] <> "Y"
	*-- No Force allocation done.
	IF laSetUp[5,2] = "N"
		llAlwForce = .F.  && User can not

	ELSE  && User Prev.
		*-- Call user defined process.
		llAlwForce = gfUserPriv('AL','ALAUTAL','FORCING')

	ENDIF
ENDIF
RETURN llAlwForce
*-- end of lfSuppForc.


*!*************************************************************
*! Name      : lfNoSuffic
*! Developer : Mohamed Badran (MAB)
*! Date      : 04/24/2000
*! Purpose   : Check Stydye Availability
*!*************************************************************
*!
FUNCTION lfNoSuffic
PRIVATE lcMessage , lnI , lcI , lnChoice , llExitLoop, llRet2Main
STORE .F. TO llExitLoop, llRet2Main
SELECT(lcPckLin)
SCAN
	llNoThing = SEEK('O'+laData[2]+laData[5]+&lcPckLin..Style+STR(nOrdLineNo,6),'OrdLine')
	IF llNoThing
		lnI = 0
		FOR lnI = 1 TO 8
			lcI = STR(lnI,1)
			IF (PQty&lcI > OrdLine.Pik&lcI) AND ;
					SEEK(OrdLine.Style+OrdLine.cWareCode+OrdLine.Dyelot,'StyDye') AND ;
					PQty&lcI > (StyDye.Stk&lcI - StyDye.Alo&lcI)
				lcMessage = "Order : " + laData[2] + ", Style : " + ordline.style +;
					", does not have available quantity."
				IF llAlwForce
					lcMessage = lcMessage + "Do you want to force allocation?"
					*GFMODALGEN
					* <Yes> <Yes to All> <No>
					*lnChoice = 1 or 2 or 3
					lnChoice  =gfModalGen("INM00000B44002","Dialog","","",lcMessage)
					DO CASE
					CASE lnChoice = 1
						*DO NOTHING

					CASE lnChoice = 2
						*EXIT LOOP AND CONTINUE
						llExitLoop = .T.

					CASE lnChoice = 3
						*EXIT LOOP AND RETURN
						STORE .T. TO llExitLoop , llRet2Main
					ENDCASE

				ELSE
					lcMessage = lcMessage + "Save without update pick Quantity?"
					*GFMODALGEN
					* <Yes> <No>
					*lnChoice=1 or 2
					lnChoice =gfModalGen("INM00000B44009","Dialog","","",lcMessage)
					IF lnChoice=1
						llUpdtPkTk = .F.
						*EXIT LOOP
						llExitLoop = .T.

					ELSE
						*EXIT LOOP AND RETURN
						STORE .T. TO llExitLoop , llRet2Main

					ENDIF

				ENDIF

				*-- Exit for loop
				IF llExitLoop
					EXIT
				ENDIF

			ENDIF

		ENDFOR

		*-- exit scan loop
		IF llExitLoop
			EXIT
		ENDIF

	ENDIF
ENDSCAN
RETURN llRet2Main
*-- end of lfNoSuffic.


*!*************************************************************
*! Name      : lfDelBol
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/20/2000
*! Purpose   :
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfDelBol
*IF SEEK(lcBol,'BOL_HDR') AND SEEK(PACK_HDR.Bill_Ladg+PACK_HDR.Order+PACK_HDR.Pack_No,'BOL_LIN')
IF SEEK(PACK_HDR.Bill_Ladg,'BOL_HDR') AND ;
		SEEK(PACK_HDR.Bill_Ladg+PACK_HDR.Order+PACK_HDR.Pack_No,'BOL_LIN')
	SELECT BOL_LIN
	BLANK
	DELETE
	SELECT BOL_HDR
	REPLACE TOT_WGHT WITH TOT_WGHT - PACK_HDR.Tot_Wght ,;
		TOT_CART WITH TOT_CART - PACK_HDR.Tot_Cart ,;
		TOT_PCS  WITH TOT_PCS  - PACK_HDR.Tot_Pcs

ENDIF


FUNCTION gfCrtTmp
PARAMETERS lcFile,lcFileStruc,lcTagExp,lcTag
PRIVATE lcFileType,lcOnError,llError,laFileStruc,lcFileName,lnWorkArea,lcTagType,;
	lnCount
lnWorkArea = SELECT()
lcFileName = IIF(TYPE('lcFile')='C',lcFile,gfTempName())
lcOnError = ON('ERROR')
llError = .F.
ON ERROR llError = .T.
lcFileType = 'A'
lcFileType = IIF(TYPE("lcFileStruc[1]")#"U",'A',;
	IIF(LEFT(ALLT(lcFileStruc),1)='(','S','F'))
ON ERROR &lcOnError
DO CASE
CASE lcFileType = 'F'
	SELECT (lcFileStruc)
	=AFIELDS(laFileStruc)
	lcFileType = 'A'
CASE lcFileType= 'A'
	=ACOPY(lcFileStruc,laFileStruc)
ENDCASE
lcTagType = 'A'
lcTagType = IIF(TYPE("lcTagExp[1]")#"U",'A','S')

IF lcFileType = 'A'
	CREATE TABLE (gcWorkDir+lcFileName) FROM ARRAY laFileStruc
ELSE
	CREATE TABLE (gcWorkDir+lcFileName) &lcFileStruc
ENDIF
SELECT (lcFileName)
IF lcTagType = 'A'
	FOR lnCount = 1 TO ALEN(lcTagExp,1)
		INDEX ON &lcTagExp[lnCount,1] TAG &lcTagExp[lnCount,2]
	ENDFOR
ELSE
	IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') <> 'C'
		lcTag = FIELD(1)
	ENDIF
	IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') = 'C'
		INDEX ON &lcTagExp TAG &lcTag
	ENDIF
ENDIF
USE
USE (gcWorkDir+lcFileName) &&EXCL
IF lcTagType = 'A'
	SET ORDER TO TAG &lcTagExp[1,2]
ELSE
	IF TYPE('lcTagExp') = 'C' AND TYPE('lcTag') = 'C'
		SET ORDER TO TAG (lcTag)
	ENDIF
ENDIF
SELECT (lnWorkArea)
RETURN lcFileName

*:**************************************************************************
*:* Name        : lfCRTNTYPE
*:* Developer   : AMH - AHMED MAHER
*:* Date        : 04/15/2003
*:* Purpose     : Open the screen that updates the carton dimension field in Pack_lin file
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCRTNTYPE()
*:***************************************************************************
*E302144,1
FUNCTION lfCRTNTYPE

PRIVATE laCodes,lnCrtntype,laDfCod,lcSlctdCod
lnCrtntype = 1
DIMENSION laCodes[1,10],laCrtntype[1,2],laDfCod[1]
laCodes[1,1] = 'CCRTNVLTYP'
laCodes[1,2] = 'laCrtntype'
laCodes[1,3] = 'lnCrtntype'
laCodes[1,4] = ''
laCodes[1,5] = .F.
laCodes[1,6] = .F.
laCodes[1,10] = 'CCRTNVLTYP'

laCrtntype = ''
=gfwCodePop(@laCodes,'CCRTNVLTYP','L')

*--Get the "Default code value" position in the array "laCrtntype"
SELECT CCODE_NO FROM CODES WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'DNCCRTNVLTYP' INTO ARRAY laDfCod
IF !EMPTY(laDfCod[1])
	DO CASE
	CASE lnActFolder = 1
		lcSlctdCod = IIF(!EMPTY(laData[17]),laData[17],laDfCod[1])
	CASE lnActFolder = 2
		lcSlctdCod = IIF(!EMPTY(&lcTmpPck..CCRTNVLTYP),&lcTmpPck..CCRTNVLTYP,laDfCod[1])
	CASE lnActFolder = 3
		lcSlctdCod = IIF(!EMPTY(&lcCtnHdr..CCRTNVLTYP),&lcCtnHdr..CCRTNVLTYP,laDfCod[1])
	ENDCASE
	lnCrtntype = ASUBSCRIPT( laCrtntype , ASCAN(laCrtntype,lcSlctdCod) , 1 )
	DO (gcScrDir+gcWinAppl+'\ALCRTNTY.SPX')
ELSE
	=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Carton Dimensions type'+;
		' codes has not been set up. Please, set up and try again.')
	RETURN
ENDIF
*-- end of lfCRTNTYPE.

*:**************************************************************************
*:* Name        : lfvCrtnOk
*:* Developer   : AMH - AHMED MAHER
*:* Date        : 04/15/2003
*:* Purpose     : Ok button for ALCRTNTY.SCX SCREEN
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvCrtnOk()
*:***************************************************************************
*C102789,1
FUNCTION lfvCrtnOk

PRIVATE lnPos, lcCrtNum
DO CASE
CASE lnActFolder = 1
	laData[17] = laCrtntype[lnCrtntype,2]
CASE lnActFolder = 2
	IF SEEK(&lcTmpPck..STYLE,lcPckLin)
		PRIVATE lcAlias
		lcAlias = ALIAS()
		SELECT (lcPckLin)
		REPLACE cCrtnVlTyp WITH laCrtntype[lnCrtntype,2]
		SELECT (lcCtnDtl)
		SCAN
			IF STYLE = &lcPckLin..Style
				IF SEEK(STR(Cart_No,4),lcCtnHdr)
					REPLACE &lcCtnHdr..cCrtnVlTyp WITH laCrtntype[lnCrtntype,2]
				ENDIF
			ENDIF
		ENDSCAN
		SELECT (lcAlias)
	ENDIF
CASE lnActFolder = 3
	PRIVATE lcAlias
	lcAlias = ALIAS()
	SELECT (lcCtnHdr)
	IF !EOF(lcCtnHdr)
		REPLACE cCrtnVlTyp WITH laCrtntype[lnCrtntype,2]
	ENDIF
	SELECT (lcAlias)
ENDCASE
CLEAR READ
*-- end of lfvCrtnOk.

*!*************************************************************
*! Name      : lfGetVerXX
*! Developer : Abdou Elgendy (ABD)
*! Date      : 06/18/2003
*! Purpose   : Function to get the number of XX? into array.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfGetVerXX()
*!*************************************************************
*!B606895,1 ABD [Begin]
*!
FUNCTION lfGetVerXX
PRIVATE lnPRvAlias , lnReturnVr
lnReturnVr = 0
lnPRvAlias = SELECT(0)

SELECT SYCASNLB
SELECT DIST cVer FROM sycasnlb ;
	WHERE LEFT(Cver,2) = 'XX' .AND. Cver # 'XXX';
	INTO ARRAY laVersn

lnReturnVr = ALEN(laVersn,1)
*-- if the customer have one version it should print it direct.
IF lnReturnVr >= 1
	lcDetailVr = laVersn[1]
ENDIF

SELECT(lnPRvAlias)

RETURN lnReturnVr

*-- End Of lfGetVerXX
*!*************************************************************
*! Name      : lfSeleVer
*! Developer : Abdou Elgendy (ABD)
*! Date      : 06/18/2003
*! Purpose   : Function to get the Version types
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfSeleVer()
*!*************************************************************
*!B606895,1 ABD [Begin]
*!
FUNCTION lfSeleVer
PRIVATE lnPRvAlias

lnPRvAlias = SELECT(0)

lcHdEsc1= ON('KEY','ESC')
ON KEY LABEL ESC

DO (gcScrDir+"ALDtVer.SPX")

ON KEY LABEL ESC &lcHdEsc1

SELECT(lnPRvAlias)

*-- End OF lfSeleVer
*!B606895,1 ABD [End]
*!*************************************************************

*!*************************************************************
*! Name      : lfManufID
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/12/2003
*! Purpose   : validate pbDtlNew button
*!*************************************************************
*! Example   : = lfManufID(Bill of lading no)
*!*************************************************************
*E037191,1 This function is copied and modified from EDI
FUNCTION lfManufID
PARAMETERS lcBolNo
PRIVATE lcManuf_Id, laRltdFld, MUCCLEVEL

lcManuf_Id = ALLTRIM(laSetUp[4,2])
MUCCLEVEL  = gfGetMemVar('M_UCCDIV',gcAct_Comp)

*-- Maintain UCC manufaturer ID at division level
IF MUCCLEVEL = 'N'
	DECLARE laRltdFld[1,2]
	STORE '' TO laRltdFld,LCUPCMAN
	laRltdFld[1,1] = "CUPCMAN"
	laRltdFld[1,2] = 'LCUPCMAN'
	=gfRltFld(ORDHDR.cDivision,@laRltdFld,'CDIVISION ')
	lcManuf_Id = IIF(EMPTY(LCUPCMAN),lcManuf_Id,LCUPCMAN)
ENDIF
RETURN ALLTRIM(lcManuf_Id)
*-- End of lfManufID.

*:**************************************************************************
*:* Name        : lfvCrCtnId
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/14/2003
*:* Purpose     : Valid function to Carrier Carton Id field
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvCrCtnId()
*:***************************************************************************
FUNCTION lfvCrCtnId
lcCrCtnIDSt = IIF(laData[18],'ENABLE','DISABLE')

*-- end of lfvCrCtnId.

*:**************************************************************************
*:* Name        : lfvCarCtnID
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/14/2003
*:* Purpose     :
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvCarCtnID()
*:***************************************************************************
FUNCTION lfvCarCtnID

SELECT (lcCtnHdr)
REPLACE cCarrCtnID WITH ALLTRIM(lcCarrCtnID)
=lfwCtnHdrBr()

*-- end of lfvCarCtnID.

*:**************************************************************************
*:* Name        : lfCheckDgt
*:* Developer   : SSE - Sameh
*:* Date        : 10/14/2003
*:* Purpose     :
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCheckDgt()
*:***************************************************************************
FUNCTION lfCheckDgt
PARAMETER lcUccNo, lcType
PRIVATE lnChkDigit ,lnSumOdd  ,lnSumEven ,lnCount
STORE 0 TO lnChkDigit ,lnSumOdd  ,lnSumEven ,lnTop

lnTop = LEN(lcUccNo)
FOR lnCount = 1 TO lnTop STEP 2
	lnSumOdd  = lnSumOdd  + VAL(SUBSTR(lcUccNo,lnCount     , 1))
	lnSumEven = lnSumEven + VAL(SUBSTR(lcUccNo,lnCount + 1 , 1))
ENDFOR
IF lcType = 'O'
	lnChkDigit = MOD(lnSumOdd*3 + lnSumEven , 10)
ELSE
	lnChkDigit = MOD(lnSumOdd + lnSumEven*3 , 10)
ENDIF
RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))

*:**************************************************************************
*:* Name        : lfVsulLbl
*:* Developer   : WLD - Waleed Hamed
*:* Date        : 09/12/2004
*:* Purpose     : Calling Crystal Visual Report Label E302210
*:***************************************************************************
*:* Called from : lfSavCartn  and lfvLblInfo
*:***************************************************************************
*:* Parameters : lcVersion && Label Version
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfVsulLbl()
*:***************************************************************************
FUNCTION lfVsulLbl
PARAMETERS lcVersion, lcCartons

lnEdiRecNo = 0
IF llEdiAcc
	*-- Check for the detail Version for current account.
	IF SEEK('A' + laData[4] , 'EDIACPRT') .AND. ;
			SEEK(EDIACPRT.cPartCode , 'EDIPH') .AND. Ediph.lDtlbl
		llDetLabel = .T.
		lcDetailVr = eDiph.cDtlbl
	ELSE
		llDetLabel = .F.
	ENDIF
ELSE
	*- Function to get the number of XX? into array.
	STORE '' TO lcDetailVr , lcDetLbAll
	DIMENSION laVersn[1]
	lnVerCount = lfGetVerXX ()
	llDetLabel = (lnVerCount >= 1)
ENDIF
IF !llEdiAcc .AND. lnVerCount >= 1
	*-- the customer should print one from the current version.
	= lfSeleVer ()
ENDIF
*C102275,1 HS 05/06/2001 Print the detailed label, if needed [Begin]
llPrintLbl = .F.
IF llDetLabel
	IF EMPTY(lcDetLbAll)
		lnChoice = gfModalGen('TRM44105B40016' , 'DIALOG' , ALLTRIM(STR(m.Cart_No , 4)))
		DO CASE
		CASE lnChoice = 1
			llPrintLbl = .T.
		CASE lnChoice = 2
			llPrintLbl = .T.
			lcDetLbAll = "Y"
		CASE lnChoice = 3
			llPrintLbl = .F.
		CASE lnChoice = 4
			llPrintLbl = .F.
			lcDetLbAll = "N"
		ENDCASE
	ELSE
		llPrintLbl = (lcDetLbAll = "Y")
	ENDIF
ENDIF

SELECT (lcTmAsnShp)
lnEdiRecNo = RECNO()
COPY TO (gcWorkDir+lcPrnAsnShp)
SELECT 0
USE (gcWorkDir+lcPrnAsnShp) EXCLUSIVE
INDEX ON bol_no+pack_no+STR(cart_no,6)+asn_ver TAG (lcPrnAsnShp)
USE IN (lcPrnAsnShp)
SELECT (lcTmAsnShp)
GOTO lnEdiRecNo
tcEDIAct   = laData[4] && m.Account
tcEDIShp   = lcPrnAsnShp
tcEDICmp   = gcAct_Comp
tcEDIPrtNm = lcSndPort
tcEDIBolNo = Bol_No
tcEDIPckNo = Pack_No
tnEDICrtNo = lcCartons 
tcEDIVer   = lcVersion
tlEDIDetLb = llPrintLbl
tcEDIDetVr = IIF(llPrintLbl,lcDetailVr,'')

*E302357,1 TMI [Start] if a Bol# is provided the proceed normally,
STORE '' TO lcTmpBolH,lcTmpBolL,lcTmPckH,lcTmPckL
IF !lfTmpSvScr(lcCartons)
  RETURN
ENDIF  
*E302357,1 TMI [End  ] 

*E302357,1 TMI [Start] send more variables
*SAVE TO (gcWorkDir+lcPrnAsnShp+'.MEM') ALL LIKE T?EDI*
SAVE TO (gcWorkDir+lcPrnAsnShp+'.MEM') ALL LIKE * EXCEPT g*
*E302357,1 TMI [End  ] 

lcCommLine = (gcWorkDir+lcPrnAsnShp+'.MEM')
lcLib=SYS(2004)+"foxtools.fll"
*Wait Clear
IF FILE(lcLib)
	SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
	SW_HIDE = 0
	lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
	*E302357,1 TMI [Start] run the edi3 exe if it is installed
	IF FILE(gcAppHome+'PRNLABL3.EXE')
      =EVALUATE("CALLFN("+STR(lnFnWinExec)+",gcAppHome+[PRNLABL3.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
    ELSE
    *E302357,1 TMI [END  ]
    
	  =EVALUATE("CALLFN("+STR(lnFnWinExec)+",gcAppHome+[PRNLABEL.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
	
	*E302357,1 TMI [Start] Close the added if statement
	ENDIF
	*E302357,1 TMI [End  ] 
	
	RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
	WAIT "LIBRARY NOT FOUND" WINDOW
	RETURN .F.
ENDIF
WAIT CLEAR
*-- end of  lfVsulLbl.


*:**************************************************************************
*:* Name        : lfvUccStrc
*:* Developer   : Hend Ghanem (HBG)
*:* Date        : 03/14/2006
*:* Purpose     : Valid function to Ok bnuton in UCC # structure screen
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvUccStrc()
*:***************************************************************************
*E040123
FUNCTION lfvUccStrc

DO CASE
  CASE lnChoice = 1
    lnNumOfDig = 5
  CASE lnChoice = 2
    lnNumOfDig = 6
  CASE lnChoice = 3
    lnNumOfDig = 9
ENDCASE

CLEAR READ


*:**************************************************************************
*:* Name        : lfTmpSvScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/04/2007
*:* Purpose     : to simulate the saving process to the actual files to be done to a temp files
*-                then open these temp files when calling the "SendUccLabels" since that class it is based on already saved data
*-                but aria27 is designed to print cartons in edit mode only , i.e based on a temp data
*:***************************************************************************
*E302357,1 TMI 
PROCEDURE lfTmpSvScr
PARAMETERS lcCartons

*- check if edi3 is not installed , then do not continue
IF !FILE(gcAppHome+'PRNLABL3.EXE')
  RETURN
ENDIF

*- lnRecNo1 : hold the current record # of lcCtnHdr temp file
*- lnRecNo2 : hold the current record # of lcCtnDtl temp file

PRIVATE lnRecno1,lnCurAlias
lnCurAlias = SELECT(0) 

*- Add extra commas to check the existence of a carton # in a range of cartons
lcCartFlt = IIF(EMPTY(lcCartons),'',","+lcCartons+",")

*- Check that there is a real peices to be pinted
SELECT &lcCtnHdr
lnRecno1 = RECNO(lcCtnHdr)
lcFiltr1 = FILTER()
SET FILTER TO 
GO TOP

SET FILTER TO TOTPCS>0 AND IIF(EMPTY(lcCartons),.T.,ALLTRIM(STR(Cart_no))$lcCartFlt)
GO TOP
IF EOF()
  SET FILTER TO &lcFiltr1
  IF BETWEEN(lnRecno1,1,RECCOUNT(lcCtnHdr))
    GOTO (lnRecno1)
  ENDIF
  SELECT (lnCurAlias)
  WAIT WINDOW NOWAIT 'No data to print .. '
  RETURN .F.
ENDIF  

*- Create temp variable names 
lcTmpBolH = gfTempName()
lcTmpBolL = gfTempName()
lcTmPckH  = gfTempName()
lcTmPckL  = gfTempName()

*- create a temp file that stores pack_hdr data while editing
SELECT PACK_HDR
=AFIELDS(laStru)
CREATE TABLE (gcWorkdir+lcTmPckH) FROM ARRAY laStru 
INDEX ON PACK_NO TAG PACK_HDR
APPEND BLANK

laData[13] = IIF(lnCtnTyp = 1,.T.,.F.)
laData[14] = IIF(lnDrctTo = 1,'S','C')

GATHER FIELDS &lcScFields FROM laData

SELECT &lcCtnHdr  
COUNT TO m.TOT_CART
CALCULATE SUM(TOTPCS),SUM(TOTWGH) TO m.TOT_PCS,m.TOT_WGHT
*- Update other Pack_hdr fields from the temp file lcCtnHdr

tcEDIBolNo = IIF(EMPTY(lcBol),'______',lcBol)

SELECT &lcTmPckH
REPLACE Pack_No    WITH IIF(EMPTY(laData[1]),laData[3],laData[1]) ;
        BILL_LADG  WITH  tcEDIBolNo ;
        TOT_CART   WITH  m.TOT_CART ;
        TOT_WGHT   WITH  m.TOT_WGHT ;
        TOT_PCS    WITH  m.TOT_PCS  ;
        cWareCode WITH lcWareCode
M.PACK_NO = PACK_NO

SELECT PACK_LIN
COPY STRU TO (gcWorkdir+lcTmPckL) WITH CDX
USE (gcWorkdir+lcTmPckL) IN 0 ORDER TAG PACKSTYLE

*:***************************************************************************
lcDelStat = SET("DELETED")

SELECT (lcCtnDtl)
lcReltn2  = SET('RELATION')
lcSkip2   = SET('SKIP')
lnRecno2  = RECNO(lcCtnDtl)
lcFiltr2  = FILTER()
lcOrder2  = ORDER()
SET SKIP TO
SET RELATION TO
SET FILTER TO
SET ORDER TO 0
GOTO TOP

*=================This part for saving pack_lin file================
lnLine = lnLastNo

SET DELETED OFF
SELECT(lcCtnDtl)
= SEEK('O'+laData[2],'OrdHdr')
SCAN
  SELECT &lcTmPckL
  IF SEEK(laData[1]+STR(&lcCtnDtl..Cart_No,4)+&lcCtnDtl..Style,lcTmPckL)
    IF DELETED(lcTmPckL)
      LOCATE REST FOR pack_no+STR(no_cart,4)+STYLE = ;
        laData[1]+STR(&lcCtnDtl..Cart_No,4)+;
        &lcCtnDtl..Style AND !DELETED()
    ENDIF
    LOCATE REST FOR Pack_No+STR(Line_No,6)+STYLE+cPackColor = ;
       laData[1]+STR(&lcCtnDtl..PackLineNo,6)+&lcCtnDtl..Style
  ENDIF
    
  IF !FOUND(lcTmPckL) AND !DELETED(lcCtnDtl)
    lnLine = lnLine + 1
    APPEND BLANK
    REPLACE Line_No WITH lnLine
  ENDIF
  
  *-- This seek for getting Pal_No
  =SEEK(STR(&lcCtnDtl..Cart_No,4),lcCtnHdr)
  
  SELECT &lcTmPckL
  REPLACE Pack_No    WITH laData[1],;
          No_Cart    WITH &lcCtnDtl..Cart_No,;
          NPltNo     WITH &lcCtnHdr..Pal_No,;
          STYLE      WITH &lcCtnDtl..Style,;
          nOrdLineNo WITH &lcCtnDtl..nOrdLineNo,;
          Qty1       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty1,0),;
          Qty2       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty2,0),;
          Qty3       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty3,0),;
          Qty4       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty4,0),;
          Qty5       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty5,0),;
          Qty6       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty6,0),;
          Qty7       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty7,0),;
          Qty8       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty8,0),;
          TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
   
  REPLACE Weight     WITH IIF(!DELETED(lcCtnDtl),;
           &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
           &lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
           &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
           &lcCtnDtl..Weight7+&lcCtnDtl..Weight8 ;
           ,MAX(Weight-&lcCtnDtl..Weight1;
           -&lcCtnDtl..Weight2;
           -&lcCtnDtl..Weight3;
           -&lcCtnDtl..Weight4;
           -&lcCtnDtl..Weight5;
           -&lcCtnDtl..Weight6;
           -&lcCtnDtl..Weight7;
           -&lcCtnDtl..Weight8;
           +(&lcCtnDtl..Qty1*&lcCtnDtl..OrgWgh);
           +(&lcCtnDtl..Qty2*&lcCtnDtl..OrgWgh);
           +(&lcCtnDtl..Qty3*&lcCtnDtl..OrgWgh);
           +(&lcCtnDtl..Qty4*&lcCtnDtl..OrgWgh);
           +(&lcCtnDtl..Qty5*&lcCtnDtl..OrgWgh);
           +(&lcCtnDtl..Qty6*&lcCtnDtl..OrgWgh);
           +(&lcCtnDtl..Qty7*&lcCtnDtl..OrgWgh);
           +(&lcCtnDtl..Qty8*&lcCtnDtl..OrgWgh),0))

  IF &lcTmPckL..TotQty = 0
    DELETE
  ENDIF

ENDSCAN

*- Renumbering cartons in Add mode only 
IF laScrMode[4]
  SELECT &lcTmPckL
  lcSetDel = SET('DELETE')
  SET DELETE ON
  =SEEK(M.PACK_NO)
  lnI = 0
  llFirst  = .T.
  lnLastCrt = &lcTmPckL..No_Cart
  llSameCrt = .F.
  SCAN REST WHILE Pack_No+STR(No_Cart,4)+STYLE = M.PACK_NO
    IF !llFirst
      llSameCrt = (&lcTmPckL..No_Cart = lnLastCrt)
      IF !llSameCrt
        lnLastCrt = &lcTmPckL..No_Cart
       ENDIF
    ENDIF
    IF llFirst OR !llSameCrt
      lnI = lnI + 1
    ENDIF
    REPLACE &lcTmPckL..No_Cart WITH lnI
    llFirst  = .F.
  ENDSCAN
ENDIF

*- Restore deleted state
SET DELETED &lcDelStat

*- Restore previous states
SELECT(lcCtnHdr)
SET FILTER TO &lcFiltr1
GO TOP
IF BETWEEN(lnRecno1,1,RECCOUN(lcCtnHdr))
  GOTO (lnRecno1)
ENDIF

SELECT(lcCtnDtl)
SET FILTER   TO &lcFiltr2
GO TOP                     && to cativate old filter
SET RELATION TO &lcReltn2
SET ORDER    TO &lcOrder2
SET SKIP     TO &lcSkip2  
IF BETWEEN(lnRecno2,1,RECCOUN(lcCtnDtl))
  GOTO (lnRecno2)
ELSE
  GO TOP  
ENDIF

*- Create the related bol_hdr , bol_lin temp files
=lfTmBOL("",laData[4],laData[5],lcWareCode,laData[7],IIF(lnCtnTyp=1,'Y','N'),"laFields",.T.,.F.,IIF(lnDrctTo=2,"C","S"))

USE IN &lcTmPckH
USE IN &lcTmPckL
USE IN &lcTmpBolH
USE IN &lcTmpBolL


SELECT(lnCurAlias)
*-- end of lpSavscr.


*:**************************************************************************
*:* Name        : lfTmBOL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/01/2007
*:* Purpose     : Get Temp Bol to print Visual labels using the SendUccLabels class
*:***************************************************************************
*! Parameters: lcAcc      : The account for this packing list
*!           : lcStore    : The Store for this packing list
*!           : lcLoc      : The WareHouse for this packing list
*!           : lcShpVia   : The ShipVia for this packing list
*!           : lcStnCartn : The defult value of the field "StanCarton" for the BOL_HDR
*!                       :* Previuos parameters used in the validation expression to get the
*!                       :* the BOL#.
*!           : lcFldArray : parameter hold the name of the array which hold defulted 
*!                        : values of the fields of BOL_HDR.
*!           : llGenerate : This parameter passed by .T. if want to force 
*!                        : the generation of BOL. When it passed by .T. , the 
*!                        : parameter "llAskMe" should be .F.
*!           : llAskMe    : This parameter is .T. if want to ask user about 
*!                        : the generation of BOL. When it passed by .T. , the 
*!                        : parameter "llGenerate" should be .F.
*!*************************************************************
*! Returns   : BOL #
*!*************************************************************
*! Example   : lfGetBOL()
*!*************************************************************
* This function is a copy of lfGetBOL from "AL.PRG" program file, it manipulate the case that the user needs to print the visual label while 
* there is no BOL# is selected
* And if there is no line for the pack in BOL_LIN 
* I will not check for the "AS" module , since I just add a line to a temp file and use it again in the program prnLabel.exe with the 
* aliases BOL_HDR, BOL_LIN
*!*************************************************************
*E302357,1 TMI 
FUNCTION lfTmBOL
PARAMETERS lcBollsttl , lcAcc, lcStore, lcLoc,lcShpVia , lcStnCartn , lcFldArray ,;
            llGenerate , lcBolVal , lcToStorCn

PRIVATE lcAcc, lcStore, lcLoc,lcShpVia,lcPack_No , lnCurAlias , lcStnCartn  ,;
        laBolFilds, llGenerate , llAskMe ,lvMemeVar  , lcBolStuts ,lcMessage;
        llForceGen , lcBolVal ,lcshpViaDs, lcBollsttl

lnCurAlias = SELECT(0)

IF TYPE("lcBolVal") $ "UL"
  lcBolVal = ""
ENDIF
*- IF parameter of To Store or consalidatore not passed set it to store as defult
IF TYPE("lcToStorCn") $ "UL"
  lcToStorCn = 'S'
ENDIF

llForceGen = llGenerate  && Variable to determan if force generation or not

*-- If Account and location is empty ,retern
IF EMPTY(lcAcc) OR EMPTY(lcLoc)
  RETURN ""
ENDIF 

=gfOpenFile(gcDataDir+'Bol_Hdr',gcDataDir+'Bol_Hdr','SH')
SELECT BOL_HDR
lcKey = KEY()
=AFIELDS(laTmpStru)  
CREATE TABLE (gcWorkDir+lcTmpBolH) FROM ARRAY laTmpStru 
INDEX ON &lcKey TAG BOL_HDR
    
IF !USED('BOL_LIN')
  =gfOpenFile(gcDataDir+'Bol_LIN',gcDataDir+'Bol_LIN','SH')
ENDIF
SELECT BOL_LIN
lcKey = KEY()
=AFIELDS(laTmpStru)
CREATE TABLE (gcWorkDir+lcTmpBolL) FROM ARRAY laTmpStru 
INDEX ON &lcKey TAG BOL_LIN

lcDistCtr  = lcStore
*-- Get the value of Distrepution center From Customer file
IF SEEK(IIF(EMPTY(lcStore),'M','S')+lcAcc+lcStore,'Customer')
  lcDistCtr  = IIF(EMPTY(Customer.Dist_Ctr),Customer.Store,Customer.Dist_Ctr)
ENDIF
  
*- Get ShipVia Description
lcshpViaDs = gfCodDes(lcShpVia,"ShipVia")
  
*-- If we go to Generate BOL because user want that or force generation
IF llGenerate
  *-- If the array that hold the initial value of BOL_HDR fields is empty , Intialize it
  IF (TYPE(lcFldArray) $ "UL") OR (TYPE(lcFldArray+"[1,1]") $ "UL") OR EMPTY(&lcFldArray[1,1])
    DIMENSION &lcFldArray[3,2]
    &lcFldArray[1,1] = "cgronhang"
    &lcFldArray[1,2] = "N"
    &lcFldArray[2,1] = "ctranmthd"
    &lcFldArray[2,2] = "M"
    &lcFldArray[3,1] = "packtype"
    &lcFldArray[3,2] = "CTN25"  
  ENDIF && If the array that hold the initial value of BOL_HDR fields is empty , Intialize it

  *-- Get the value of carreier code which is related field to Shipvia code
  DECLARE laVRltFld[1,2]
  laVRltFld[1,1] = 'CARRIERCOD'
  laVRltFld[1,2] = 'lcCarCod'
  lcCarCod = ''
  =gfRltFld(lcShpVia ,@laVRltFld,'SHIPVIA   ')
    
  *-- Insert record to the new BOL in BOL_HDR file.  
  m.BOL_NO     = tcEDIBolNo
  m.ACCOUNT    = lcAcc
  m.STORE      = lcDistCtr
  m.W_CODE     = lcLoc  
  m.ShipVia    = lcShpVia
  m.Carrier    = gfCodDes(m.ShipVia,'SHIPVIA')
  m.CarrierCod = lcCarCod
  m.StanCarton = lcStnCartn
  m.cToStorCn  = lcToStorCn

  *-- assign the defult values of BOL which holded in the passed array to memory var.
  FOR lnI = 1 TO ALEN(&lcFldArray,1)
    lvMemeVar  = "m." + &lcFldArray[lnI,1]
    &lvMemeVar = &lcFldArray[lnI,2]
  ENDFOR
  m.BolDate    = gdSysDate
  m.cBolTime   = Time()

  *- Change a default on Bill Of Lading Screen , according to Partner Setup
  IF SEEK('A'+m.ACCOUNT,'EDIACPRT','ACCFACT') AND SEEK(EDIACPRT.cPartCode,'EDIPH')
    m.cgronhang = IIF(EDIPH.lGrmHang,'Y','N')
  ENDIF

  INSERT INTO (gcWorkDir+lcTmpBolH) FROM MEMVAR
  
  M.ORDER   = laData[2]  
   
  INSERT INTO (gcWorkDir+lcTmpBolL) FROM MEMVAR  
  
ENDIF
    
SELECT(lnCurAlias)

*-- end of lfTmBOL.
