*:****************************************************************************************************************
*: Program file  : ALAUPLG.PRG
*: Program desc. : Custom Packing List Tamplate For GMA 
*: For screen    : ALAUPLG
*:        System : Aria Advantage Series.
*:        Module : Allocation (AL).
*:     Developer : TMI - TAREK MOHAMED IBRAHIM
*:****************************************************************************************************************
* Important notes by TMI :  06/09/2003
* 
*  1)  This program is base on "v:\custom27\gma\prgs\alplstg.prg" the manual packing list program for GMA
*      So you may face non used variables and non used functions .
*  2)  The uncomplete session code is commented and do not work now , this screen works as file screen 
*:****************************************************************************************************************
*: Modification:
*: C122070,1 HBG 04/04/2004 Ability to identify a range pack from other standard packs on the screen
*: B123477,1 TMI 08/04/2004 Fix some problems of the entry # C122070
*: C038675,1 HBG 10/26/2004 Add configuration field to the screen
*: B125144,1 NNA 01/09/2005 Fix bug that happen when switching between (Packing list template screen)
*: B125144,1 NNA            and (the Automatic packing list screen) (the Two screens for GMA), in this
*: B125144,1 NNA            case we get an error massage (Variable 'lcPakindxst' Not found)
*: B125190,1 NNA 01/16/2005 Fix bug that the Auto P/L doesn't update the Qty/Ctn that was updating 
*: B125190,1 NNA            in the custom Packing info. option in the style screen
*:****************************************************************************************************************
PARAMETERS lcPassOrd  
*--lcPassOrd : a parameters that holds order # when calling this screen from Automatic P/L screen

*-- laData      array that hold the data of the key screens and header folder
*-- laScrMode   array that hold the screen mode
*-- laKeyField  array that hold the key of the screen    
*-- laCodeInfo  array to be used in gfwCodePop function
*-- laShpVia    array that hold all shipvia codes
*-- lafoldwinds array that hold the count of used folder and it's titles


*DIMENSION laData[1],laScrMode[4],laKeyField[3,4],laCodInfo[1,10],laShpVia[1],;
          lafoldwinds[2,2]

DIMENSION laData[1],laScrMode[4],laCodInfo[1,10],laShpVia[1],;
          lafoldwinds[2,2]          
*STORE SPACE(0) TO laData,laKeyField,laCodInfo,laShpVia
STORE SPACE(0) TO laData,laCodInfo,laShpVia
PRIVATE  lcAlias , lcOrdr , lnRcNo , llPacked , llComp
STORE .F. TO  llPacked , llComp
lnRcNo = 0
STORE '' TO lcAlias , lcOrdr

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

*-- lnNonMajSt
*-- lnMajLen
*-- lcFree_Clr
*-- lnColorLen

*B125144,1 NNA 01/09/2005 (Begin) initiate lcPakIndxSt to use as Public
STORE '' TO lcPakIndxSt
*B125144,1 NNA (End)

*-- lnCrtnSeq  : When clicks apply button this variable is increased by 1 for each new carton
lcAppStat = 'DISABLE'  && Enable/disable status for Apply button
lnCrtnSeq = 0
lnCurrCtn = 0
lnLineRange=IIF(_WINDOWS,1.77,2)  && the line height for the dbl click
lnY = 0
lnx = 0
lcObjName = ''
llClick    =.F.                   && logical variable to control the dbl click selection
lnTimelimt = 0
lnCurR = 0
lnCount = 1  && Defined to by used by the array laScObj in the spr file 
lnOrdPk = 1  && Detail = 1 | summary = 2
llIsPck = .T.
llLockOnErr = .T.   && Lock order on error

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


*--(Start) Defining Variables
DIMENSION laStySrc[1], laStyTrgt[1],laOldSty[1]
DIMENSION laClrSrc[1], laClrTrgt[1],laOldClr[1]
STORE SPACE (0) TO laStySrc,laStyTrgt,laOldSty,laClrSrc, laClrTrgt,laOldClr
lcMask=gfItemMask("PM")

llntfound = .F.

*Defining new variables [START]
STORE SPACE(0) TO lcCusPo,lcDept,lcShVDesc,lcPkChCode,lcPkDsCode,lcStoreDsc,;
                  lcStyPic,lcStyTtl,lcDistCtr,lcBol,lcPackNo,lcOrderNo,;
                  lcStore,lcPckLin,lcCtnHdr,lcCtnDtl,lcWinKey,;
                  lcWinHdr,lcWinDtl,lcWinDtl1,lcWinDtl2,;
                  lcWinCtn,lcWinCtn1,lcWinCtn2,lcWinCtn3,lcfolder,lcfoldprnt,;
                  lcOldVal,lcCtnSty,lcStySz,lcLsVPck,lcScaFile,lcCartonSz,;
                  lcPrvPack, lcFree_Clr , lcTmpDetFl , lcBollsttl,;
                  lcTmpPck,lcSumPck

STORE 0  TO lnDumDtlR, lnDumCtnR, lnNonMajSt, lnMajLen , lnColorLen
*Defining new variables  [END  ]


*-- lcSel      variable that hold label of select buuton in second folder
*              the label to be (select OR unselect)

*--Last modification
lcSel = "This"

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
           lnStyWgh,lnCartNo,lnHdBrRec,lnDtBrRec,lnSmBrRec,lnCtHdBrRe,lnCtDtBrRe,lnFrom,;
           lnTo,lnMaxPal,lnMinPal,lnLastNo,lnMaxCtn,lnLastCtn,;
           lnPackWgh,lnPackQty,lnPackCtn,lnPrvMode,lnBrCtnQty,lnBrUntWgh,lnOrdMdPr
*-- lnActFolder variable that hold the number of activated folder
*-- lnCtnTyp    variable that showes carton type (standard or pikpcak)
*-- lnDrctTo    variable that showes if the paking is directed to store or consolidator
*--Change the Default Values to a Pick & Pack carton and Ship to Store
STORE 2 TO lnCtnTyp
STORE 1 TO lnDrctTo,lnActFolder

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
               lpshow once the program is executed;
               (.F. go in lpshow,.T. don't)
*-- llSelOrdLi variable that showes if the order lines has been selected to 
*              use it in lfactfolder function in case view mode
*-- llUnComp   variable That showes if the sysytem didect uncomp. session or not
*-- llCupdate  varibale that showes if any editing has been happened
*-- llAnyUpd   varibale that showes if any editing has been happened,but in local
*--            scope to limit executing lfvSelOrdl() for the same pack only
*--            if any modification has happened
*-- llAnyRec   variable that showes if temp file (lcPckLin) hold any record or it 
*--            is empty
*-- llScrPrnLb   Hold The State of the option menue print label after each carton
*-- lcSndPort    Hold the name of the printing port.
*-- llShoPckDt   Hold if .T. it Shows Pack details in the browse , else It shows pack summary 
*-- llShwOpn   : show only records with AvlQty > 0 , Show all records in browse , if .F. 

STORE .T. TO llShoPckDt

*-- C101735,1  New variables
llScrPrnLb = .F.
lcSndPort = "COM2"
*-- C101735,1  New variables

*C038675,1 HBG 10/26/2004 Add flag to check if use configuration [Begin]
*STORE .F. TO llBrowse,llEdiSys,llAsnSys,llAloModul,llEdiAcc,llPCDStat,llPalStat,llClrSel,;
             llShwOpn,llEdiopn,llB_H_Opn,llB_D_Opn,llDyelot,llMulWare,llNew,llPckCopy,llNoShow,;
             llSelOrdLi,llUnComp,llCupdate,llAnyUpd,llAnyRec, llPrint,llPakPrSiz
STORE .F. TO llBrowse,llEdiSys,llAsnSys,llAloModul,llEdiAcc,llPCDStat,llPalStat,llClrSel,;
             llShwOpn,llEdiopn,llB_H_Opn,llB_D_Opn,llDyelot,llMulWare,llNew,llPckCopy,llNoShow,;
             llSelOrdLi,llUnComp,llCupdate,llAnyUpd,llAnyRec, llPrint,llPakPrSiz,llUseConfg
*C038675,1 [End]       
             
STORE .F. TO llQtyArr,llWghArr

DIMENSION laMajSegs[1,1]
             
llFirstQty = .F.             
llFirstWgh = .F.

llAlowNew  = .F.             &&  Disable new button , the screen will work as transaction screen 
llAddLine = .T.

laDefproc[07] = .F.                && This is to Enable local delete
laDefproc[09] = .F.                && This is to prevent the global save
laDefProc[10] = .F.                && This is to prevent the global close

lcDtlBrTtl = 'Open Quantities'     && This is to hold the title of the browse screen
lcCtHdBrTt = 'Cartons'             && This is to hold the title of the browse screen
lcCtDtBrTt = 'Contents of carton'  && This is to hold the title of the browse screen

lnSessNo  = gnProgCopy             && This is to hold the no of the opened session from global variable
lcSession = SPACE(1)               && This is to hold the session id for the uncompleted session
lcUserID  = PADR(gcUser_id, 10)    && This is to hold the user id 

*-* laKeyField[1,1] = "laData[1]" 
*-* laKeyField[1,2] =.F.
*-* laKeyField[1,3] = "TMPL_HDR"
*-* laKeyField[1,4] = 1
*-* 
*-* laKeyField[2,1] = "laData[2]"
*-* laKeyField[2,2] = .F.
*-* laKeyField[2,3] = "TMPL_HDR"
*-* laKeyField[2,4] = 2
*-* 
*-* laKeyField[3,1] = "laData[3]"
*-* laKeyField[3,2] = .T.
*-* laKeyField[3,3] = "TMPL_HDR"
*-* laKeyField[3,4] = 3

*--This part is to be used in case detecting an uncomplete program session.

lcScrMode  = ""                 && This is to hold the screen mode to be used in the uncomplete session 
*-* lnUnCmSeRc = 0                  && This is to hold the uncomplete session record no in the "uncmsess" file
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

*-**--creat a memory file to hold the state of the menu option llUpdtPkTk 
*-- This is to add laData elements to laVars array---------------
FOR lnK = 1 TO 16
  DIMENSION laVars[ALEN(laVars)+1]
  laVars[ALEN(laVars)] = "laData[" + ALLTRIM(STR(lnK,2)) + "]"
ENDFOR
IF !gfSetup()
  RETURN
ENDIF  

lcBaseFile  = "TMPL_HDR"
lcFile_ttl  = 'Tamplates'
lcBrFields  = [Order    :H='Order#',]+;
              [Account  :H='Account',]+;
              [STATUS   :H='Status',]+;
              [TOT_WGHT :H='Tot. Weight',]+;
              [TOT_CART :H='Tot. Carton',]+;
              [TOT_PCS  :H='Tot. Pcs',]+;
              [CWARECODE:H='Warehous',]+;
              [NLASTLNO :H='Lastline']

*-- This is to check if the system has EDI module , and open EdiAcc File if is true
*--- SSH
ON KEY LABEL ALT+B DO lfCtrKey

llEdiSys   =  ('AS' $ gcCmpModules)
IF llEdiSys
  
  =gfOpenFile(gcDataDir + 'EDIACPRT' , gcDataDir + 'ACCFACT' , 'SH')
  =gfOpenFile(gcDataDir + 'EDIPH' , gcDataDir + 'PARTNER' , 'SH')
  
ENDIF

=gfOpenFile(gcDataDir+'WareHous','WareHous','SH')

=gfOpenFile(gcDataDir + 'SPCK_LIN' , gcDataDir + 'SPCK_LINVR' , 'SH')
=gfOpenFile(gcDataDir + 'CUSTDEPT' , gcDataDir + 'CUSTDEPT' , 'SH')

=gfOpenFile(gcDataDir + 'CSTPKINF' , gcDataDir + 'CSTPKINF' , 'SH')
=gfOpenFile(gcDataDir + 'TMPL_HDR' , gcDataDir + 'TMPL_HDR' , 'SH')
=gfOpenFile(gcDataDir + 'TMPL_LIN' , gcDataDir + 'TMPL_LIN' , 'SH')

DECLARE laDRltFld[1,2]
laDRltFld[1,1] = 'DIVLNAME  '
laDRltFld[1,2] = 'lcDivLName'
lcDivLName = ''
DECLARE laVRltFld[1,2]
laVRltFld[1,1] = 'CARRIERCOD'
laVRltFld[1,2] = 'lcCarCod'
lcCarCod = ''

*-- C101735,1 redimension the array [Begin]
*C038675,1 HBG 10/26/2004 Add use configuration setup  [Begin]
*DIMENSION laSetup[7,2]
DIMENSION laSetup[8,2]
*C038675,1 [End]

laSetUp[1,1] = 'M_DYELOT'
laSetUp[2,1] = 'M_CANAFTER'

laSetUp[3,1] = 'M_WareHouse'
laSetUp[4,1] = 'XMANUFID'

laSetUp[5,1] = 'M_FORCEALO'
laSetUp[6,1] = 'M_BOLSTUTS'

laSetUp[7,1] = 'M_PAKPRSIZ'

*C038675,1 HBG 10/26/2004 Add use configuration setup  [Begin]
laSetUp[8,1] = 'M_STYCNFG'
*C038675,1 [End]

=gfGetMemVar(@laSetUp,gcAct_Comp)

llAlwForce = lfSuppForc()

llDyelot   = ALLTRIM(laSetUp[1,2]) = 'Y'
lnOrdMdPr  = laSetUp[2,2]

llMulWare  = ALLTRIM(laSetUp[3,2]) = 'Y'
lcManufId  = ALLTRIM(laSetUp[4,2])

*Add flag to hold the valu of the setup (M_BOLSTUTS) [Begin]
llBolnever = (ALLTRIM(laSetup[6,2]) = "N")

*Get the setting of generate B.O.L Automatic [begin]
llBolAutom = (ALLTRIM(laSetup[6,2]) = "A")

*Add flag to hold the value of the setup (M_PAKPRSIZ) [Begin]
llPakPrSiz = laSetup[7,2]

*C038675,1 HBG 10/26/2004 Add flag to check for use configuration setup  [Begin]
llUseConfg = (ALLTRIM(laSetup[8,2]) = 'Y')
*C038675,1 [End]

llAsnSys   = ('AS' $ gcCmpModules)
llAloModul = ('AL' $ gcCmpModules)

lcProgID = PADR("PACKLIST",10)

lnFolderCEnd = 102.40
lnFolderREnd = 1.99
lnNoFld      = 3
lcwfoldchng  = '=lfActFolder()'  && function to control shows after change the folder

*-- Take into consideration styles with exteded size scale while printning labels
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

  lcAsnLabel = gfTempName()
  lcTmpDetFl = gftempName()
    
  lcStyPic   = gfItemMask("PI")
  lcStyTtl   = gfItemMask("HI")
  lnStyleWid = LEN(lcStyPic)  
  
  lcScFields = "Pack_No, Order, Piktkt, Account, Store, Note, ShipVia,;
                Tot_Wght, Tot_Cart, Tot_Pcs, Sp_Inst1, Sp_Inst2, LStandCtn,;
                CToStorCn, CPkChCode, CPkDsCode "
  SELECT TMPL_HDR
  SCATTER FIELDS &lcScFields TO laData BLANK
  lcWareCode = ''
  lcBol      = ''
  lcOldValue = ""

  lcPckLin   = gfTempName()  
  lcCtnHdr   = gfTempName()
  lcCtnDtl   = gfTempName()
  lcScaFile  = gfTempName()
  lcCartonSz = gfTempName()

  lcTmpPck  = gfTempName()

  lcSumPck  = gfTempName()
  lcPakIndxSt = gfTempName()
  CREATE TABLE (gcWorkDir + lcScaFile) (SzCode C(1))
  INDEX ON SzCode TAG (lcScaFile)
  FOR lnI = 1 TO 8
    INSERT INTO (lcScaFile) (SzCode) VALUES (STR(lnI,1))
  ENDFOR
  USE (gcWorkDir+lcScaFile) IN 0 AGAIN ALIAS (lcCartonSz) ORDER (lcScaFile)

  lafoldwinds[1,1] = 'Order Styles/Packs'
  lafoldwinds[1,2] = lcWinDtl
  lafoldwinds[2,1] = 'Carton Information'
  lafoldwinds[2,2] = lcWinCtn
  
  lcfolder    = gfTempName()        && Folder Window Name
  lcfoldprnt  = gcBaseWind          && window parent name for the folder
  STORE 1 TO lnActFolder,lnLastFold && active folder
  
  *-- Get a session number, to be updated in the uncomplete session
  *-- number file for each session.
  lcSession  = gfsequence('CSESSION')

  =lfEvalSegs()
  IF TYPE('lcPassOrd') = 'C' .AND. SEEK(PADR(lcPassOrd,6),'TMPL_HDR')
    SELECT TMPL_HDR
    STORE .F. TO laScrMode
    STORE .T. TO laScrMode[2]
    SCATTER FIELDS &lcScFields TO laData
  ENDIF

ENDIF


laCodInfo[1,01] = "SHIPVIA"                 && Field Name
laCodInfo[1,02] = "laShpVia"                && Array Name
laCodInfo[1,03] = "lnShpVia"                && Popup Name
laCodInfo[1,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
laCodInfo[1,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[1,06] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[1,07] = "TMPL_HDR"                && Alternative File (For default val.)
laCodInfo[1,08] = "TMPL_HDR"                && Use this index for the Alternative file.
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

*SELECT TMPL_LIN
*SET ORDER TO PackStyle

SELECT OrdHdr
SET FILTER TO cOrdType = "O"
SET ORDER TO OrdHdr

SELECT PikTkt
SET ORDER TO TAG 'OrdPik' IN PikTkt
SELECT TMPL_HDR
SET ORDER TO TMPL_HDR
SET RELATION TO 'O'+Order INTO OrdHdr

SET ORDER TO TAG Style IN Style
  
SELECT OrdLine
SET FILTER TO cOrdType+order+store+style+STR(lineno,6) = "O"
SET ORDER TO OrdLinSt
IF EMPTY(SET("RELATION"))
  SET RELATION TO 'S'+Scale INTO Scale ADDITIVE
  SET RELATION TO Style INTO Style ADDITIVE
ENDIF

=gfOpenFile(gcDataDir+"Pikline",gcDataDir+'PiklineO','SH')
SELECT PikTkt
SET RELATION TO Piktkt.piktkt INTO Pikline ADDITIVE

*--lnFoldSize : Holds folder size in the screen  
lnFoldSize = 51.25
DO (gcScrDir + '\ALAUPLG.SPR')

SELECT TMPL_HDR
SET RELATION TO
SELECT OrdLine
SET RELATION TO
SELECT TMPL_LIN
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
  SELECT TMPL_LIN
  SET RELATION TO
  
  IF USED(lcPckLin)
    USE IN (lcPckLin)
    IF USED(lcTmpPck)
      USE IN (lcTmpPck)
    ENDIF
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
 
  IF USED(lcSumPck)
    USE IN (lcSumPck)
  ENDIF
  ERASE (gcWorkDir+lcSumPck+".DBF")
  ERASE (gcWorkDir+lcSumPck+".CDX")
 
  IF USED(lcScaFile)
    USE IN (lcScaFile)
  ENDIF
  ERASE (gcWorkDir+lcScaFile+".DBF")
  ERASE (gcWorkDir+lcScaFile+".CDX")

ENDIF  

*!*************************************************************
*! Name      : lpShow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Handling the screen mode
*!*************************************************************

PROCEDURE lpShow
PRIVATE lnCurAlias,lcFilToUse,lcSelStyPk

lcSelStyPk = 'DISABLE'
lnCurAlias = SELECT(0)

SHOW GET pbcptask DISABLE
SHOW GET pbCpCalndr DISABLE
SHOW GET pbcpCalc DISABLE

 SHOW GET pbHdrNew DISABLE
 SHOW GET pbDtlNew DISABLE

DO CASE
  *-- Select mode
  CASE laScrMode[1]
    lcScrMode  = "S"
    = lfCrtUnCmp()
*-*     IF lfChkUnCmS()
*-*       = lfChngFolder(lnActFolder)
*-*       RETURN
*-*     ENDIF
    
    STORE SPACE(0) TO lcCusPo,lcDept,lcShVDesc,lcPkChCode,lcPkDsCode,;
                      lcStoreDsc,lcDistCtr,lcBol,lcPackNo,lcOrderNo,lcStore,;
                      lcOldVal,lcCtnSty,lcStySz,lcLsVPck,lcPrvPack

    STORE "DISABLE" TO lcStoStat,lcCtnStat,lcPikStat,lcShipStat,lcInstStat,;
                       lcCartNoSt,lcTotWghSt,lcPalNoSt,lcCtnPalSt,lcCtnStySt,lcStySzSt,;
                       lcStyQtySt,lcStyWghSt,lcAddHdrSt,lcRemHdrSt,lcAddDtlSt,;
                       lcRemDtlSt    
    STORE "ENABLE" TO lcPackStat,lcOrdStat,lcAccStat
    STORE 0 TO lnLastFold,lnShpVia,lnRelCho,lnTotWgh,lnPalNo,lnCtnPal,lnStyQty,lnStyWgh,;
               lnCartNo,lnHdBrRec,lnDtBrRec,lnSmBrRec,lnCtHdBrRe,lnCtDtBrRe,lnFrom,lnTo,;
               lnMaxPal,lnMinPal,lnLastNo,lnMaxCtn,lnLastCtn,;
               lnPackWgh,lnPackQty,lnPackCtn,lnPrvMode,lnBrCtnQty,lnBrUntWgh
    STORE 2 TO lnCtnTyp
    STORE 1 TO lnDrctTo
    STORE .F. TO llBrowse,llPCDStat,llPalStat,llClrSel,llShwOpn,llNew,;
                 llPckCopy,llSelOrdLi,llUnComp,llCUpdate,llAnyUpd,llAnyRec,;
                 llQtyArr,llWghArr
    *--- varaiables that used for uncomplete session-------------
    *-- lnUnCmSeRc This is to hold the uncomplete session record
    *   no in the "uncmsess" file 
*-*     lnUnCmSeRc = 0
    *------------------------------------------------------------

    = gfwCodePop(@laCodInfo , "SHIPVIA" , "N")
    SELECT TMPL_HDR
    SCATTER FIELDS &lcScFields TO laData BLANK
    lcWareCode = ''
    lcBol      = ''
    
    SELECT(lcPckLin)
    SET FILTER TO
    SELECT (lcCtnHdr)
    SELECT (lcCtnDtl)
    SET FILTER TO

    _CUROBJ = OBJNUM(laData[2])
    
    DIMENSION laStyTrgt(1)
    DIMENSION laClrTrgt(1)
    STORE SPACE(0) TO laStyTrgt,laClrTrgt
    SELECT (lcPckLin)
    SET FILTER TO
    
    lnCrtnSeq = 0
    lnCurrCtn = 0
    
    SHOW GET pbSelSty  DISABLE
    SHOW GET pbSelPack DISABLE

    SHOW GET pbBrws ENABLE
        
  *-- View mode  
  CASE laScrMode[2]
    
    lcScrMode  = "V"
    IF INLIST(lnPrvMode,3,4)
      *-- Release order lock if prev mode is add or edit
      =lfChkOrdLok(.F.) 
    ENDIF

    SELECT TMPL_HDR
    SCATTER FIELDS &lcScFields TO laData

    lnPackWgh = laData[08]
    lnPackCtn = laData[09]
    lnPackQty = laData[10]

    lcWareCode = TMPL_HDR.cWareCode
    lcBol      = TMPL_HDR.Bill_Ladg
    STORE nLastCart TO lnMaxCtn
    STORE nLastCart+1 TO lnFrom,lnTo
    
    lcCusPo    = IIF(OrdHdr.MultiPo,IIF(SEEK('O'+ladata[2]+laData[5],'OrdLine'),OrdLine.CustPo,SPACE(10)),OrdHdr.CustPo)

    llEdiAcc   = llEdiSys .AND. SEEK('A' + laData[4] , 'EDIACPRT') .AND.;
                 SEEK(EDIACPRT.cPartCode , 'EDIPH')
    
    STORE llEdiSys .AND. llEdiAcc .AND. EDIACPRT.lPkChrDes  TO llPCDStat
    STORE llEdiSys .AND. llEdiAcc .AND. EDIPH.lPltShp TO llPalStat
    
    
    *-- This part to enforce change to the first folder while the navigation
    IF lnActFolder <> 1 AND VARREAD() $ "PBTOP,PBPRVS,PBNXT,PBBTM"
      lnLastFold = lnActFolder
      lnActFolder = 1
      llNoThing   = lfChngFolder(lnActFolder)
    ENDIF
    *(Start) Set the value to Standered,Store instead of N/A
    lnCtnTyp = IIF(TMPL_HDR.LStandCtn,1,2)

    lnDrctTo = IIF(TMPL_HDR.CToStorCn='C',2,1)
    
    lnShpVia = laData[7]
    laCodInfo[1,01] = "SHIPVIA"                 && Field Name
    laCodInfo[1,02] = "laShpVia"                && Array Name
    laCodInfo[1,03] = "lnShpVia"                && Popup Name
    laCodInfo[1,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
    laCodInfo[1,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
    laCodInfo[1,06] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
    laCodInfo[1,07] = "TMPL_HDR"                && Alternative File (For default val.)
    laCodInfo[1,08] = "TMPL_HDR"                && Use this index for the Alternative file.
    laCodInfo[1,09] = "laData[1]"               && Seek this expretion.
    laCodInfo[1,10] = "ShipVia"                 && Alternative Field Name
    = gfwCodePop(@laCodInfo, "SHIPVIA", "T")
    STORE "DISABLE" TO lcStoStat,lcCtnStat,lcPikStat,lcShipStat,lcInstStat,;
                       lcPackStat,lcOrdStat,lcAccStat
    IF !llCupdate
      DO lpClsScr
    ENDIF

    DIMENSION laStyTrgt(1)
    DIMENSION laClrTrgt(1)
    STORE SPACE(0) TO laStyTrgt,laClrTrgt

    IF lcPrvPack # laData[2] .OR. lnPrvMode = 3
        dime laSq[1]
        laSq[1] = 0
        select max(ncarton) from tmpl_lin where order = laData[2] into array laSq
        lnCrtnSeq = laSq[1]
        lnCurrCtn = lnCrtnSeq
        lcPrvPack = laData[2]    
*      lnCrtnSeq = TMPL_HDR.TOT_CART
       =lfvSelOrdL()
    ENDIF
    SELECT (lcPckLin)
    SET FILTER TO
        
    *--Navigation buttons
    =lfvNavigate()
    
    SHOW GET pbSlct ENABLE
    SHOW GET pbEdt ENABLE
    SHOW GET pbBrws ENABLE
    SHOW GET pbDlt ENABLE
    
    **Prevent editing(or deleting)  when packs are generated
    lcPckOrd = ORDER('PACK_HDR')
    SET ORDER TO ORDERPCK IN PACK_HDR
    IF SEEK(laData[2],'PACK_HDR')
      SHOW GET pbEdt DISABLE
      SHOW GET pbDlt DISABLE
    ENDIF
    
    *-- if order is completed show a template ( if exists ) in readonly mode
    IF ORDHDR.STATUS = 'C'
      SHOW GET pbEdt DISABLE
    ENDIF
    
    
    SET ORDER TO &lcPckOrd IN PACK_HDR
    
    IF !EMPTY(lcPassOrd)
      SHOW GET pbSlct DISABLE  
      SHOW GET pbEdt DISABLE
      SHOW GET pbBrws DISABLE
      SHOW GET pbDlt DISABLE

      SHOW GET PBTOP  DISABLE
      SHOW GET PBPRVS DISABLE
      SHOW GET PBBTM  DISABLE
      SHOW GET PBNXT  DISABLE      
    ENDIF

  *-- Edit mode
  CASE laScrMode[3]

    IF TMPL_HDR.Status = 'C'
      PRIVATE lcMessage
      *-- This packing list is shipped. Can't be Deleted
      *-- OK
      lcMessage = "Can't be modified."
      = gfModalGen("INM44102B00000","Dialog",lcMessage)  
      STORE .F. TO laScrMode
      laScrMode[2] = .T.
      SELECT TMPL_HDR
      = gfObj_Lock(.F.) 
      SHOW GETS
      llGoOn = lfChkOrdLok(.F.)
      RETURN
    ENDIF
 
    lcScrMode = "E"
    *-- check if any pack is being created now on the same order in any
    *-- other session
    llGoOn = lfChkOrdLok(.T.)
    IF llGoOn
*-*      IF !llUnComp
*-*         llNoThing = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)    
*-*      ENDIF
    ELSE
      laDcrMode = .F.
      laScrMode[2] = .T.
      SHOW GETS
    ENDIF
    SHOW GET pbBrws DISABLE

    
  *-- Add mode
  CASE laScrMode[4]
    lcScrMode = "A"
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
      STORE 2 TO lnCtnTpy
      STORE 1 TO lnDrctTo
      = gfwCodePop(@laCodInfo, "SHIPVIA", "N")
      
      _CUROBJ = OBJNUM(laData[2])
    ENDIF
    
    SHOW GET pbSav  ENABLE
    SHOW GET pbSlct DISABLE
    SHOW GET pbBrws DISABLE
    STORE 'DISABLE' TO lcOrdStat,lcAccStat
      
    *-- Lock in Add mode
    IF !lfChkOrdLok(.T.)
      laScrMode = .F.
      laScrMode[1] = .T.
      SHOW GETS
    ENDIF

ENDCASE

IF laScrMode[3] .OR. laScrMode[4]
  SHOW GET pbCls,1  PROMPT "BMPS\Cancel.BMP"      
ENDIF
IF !llIsPck
  SHOW GET lnOrdPk DISABLE
ENDIF  

SHOW GET pbSelSty  &lcSelStyPk
SHOW GET pbSelPack &lcSelStyPk


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

*--Disable B.O.L field if generate B.O.L Automatic is 'Yes' [begin]
IF llBolAutom
  SHOW GET lcBol DISABLE
ELSE
  SHOW GET lcBol &lcInstStat
*--End if generate B.O.L Automatic is 'Yes' [begin]
ENDIF

lnPrvMode = ASCAN(laScrMode,.T.)

SELECT (lnCurAlias)
*--


*!*************************************************************
*! Name      : lfPakNoBrw
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : browse all pack list or validated by the account
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
SELECT TMPL_HDR
lcCurTag = Order('TMPL_HDR')

*-- Here, we use AccPack Tag if we need to validate for particular account or any
*   account by using Alltrim(laData[4]), other wise we will use orderpck tag
lcTag = IIF((EMPTY(laData[2]) AND !EMPTY(laData[4]) OR llPckCopy),"AccPack","OrderPck")
SET ORDER TO &lcTag
IF !SEEK(IIF(ORDER('TMPL_HDR')="ACCPACK",laData[4],ALLTRIM(laData[2])),'TMPL_HDR')
  *-- There are no records to browse.
  *-- OK
  = gfModalGen("INM44032B00000","Dialog")  
ELSE
  llReturn = AriaBrow(IIF(ORDER('TMPL_HDR')="ACCPACK","laData[4]","ALLTRIM(laData[2])"),lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'TMPL_HDR',.F.)
ENDIF  

SET ORDER TO lcCurTag IN TMPL_HDR
SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfvData_2
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Validat the screen key laData[2] .
*!*************************************************************

FUNCTION lfvData_2

PRIVATE lnCurAlias,lcOrdHdrTg,lcOrder
lnCurAlias = SELECT(0)

lcOrdHdrTg = ORDER('OrdHdr')
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
  lcAlias = SELECT(0)
  SELECT TMPL_HDR
  lcTmpKey = Order
  llPacked = SEEK(laData[2],'TMPL_HDR')
  =SEEK(lcTmpKey)

  *relocate pointer on the Ordhdr file due to the relation between tmpl_hdr and ordHdr
  =SEEK('O'+laData[2],'OrdHdr')
  SELECT (lcAlias)
  
  DO CASE

    *--- and the orde packed.[Begin]
     CASE OrdHdr.Status = 'C' AND !llPacked
       =gfModalGen("INM000000B00000","DIALOG",'','',;
       'This order is completed, cannot create a template for it.')  
      laData[2] = SPACE(6)
      SHOW GET laData[2]
      llPacked = .F.
    CASE OrdHdr.Status = 'X'
      *-- This order is canceled, cannot pack.
      *-- OK
      = gfModalGen("INM44050B00000","Dialog","canceled")  
      laData[2] = SPACE(6)
      SHOW GET laData[2]
    
    CASE OrdHdr.bulk = 'Y'
      *-- This order is bulk, cannot pack.
      *-- OK
      = gfModalGen("INM44050B00000","Dialog","bulk")  
      laData[2] = SPACE(6)
      SHOW GET laData[2]
    
    OTHERWISE
      llComp = (OrdHdr.Status = 'C' AND !llPacked)
      =lfGetData()      
      SET ORDER TO ORDPIK IN PIKTKT
      laScrMode = .F.
      *IF !SEEK(laData[2],'TMPL_HDR')
      IF !llPacked
        lnRspn = gfModalGen('INM00000B32000',.F.,.F.,.F.,'No tamplate has been created for order '+;
                                              laData[2]+', Do you want to create new?')
        IF lnRspn=1
          laScrMode[4] = .T.
          =lfvNewPack()
        ELSE
          laData[2] = '      '
          laData[4] = '     '
          laScrMode[1] = .T.
        ENDIF
      ELSE
        Dime laSq[1]
        laSq[1] = 0
        select max(ncarton) from tmpl_lin where order = laData[2] into array laSq
        lnCrtnSeq = laSq[1]
        lnCurrCtn = lnCrtnSeq
        laScrMode[2] = .T.
        =lfvNewPack()
      ENDIF
      DO lpShow
  ENDCASE
ENDIF

SET ORDER TO lcOrdHdrTg IN OrdHdr
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : browse all pack list or validated by the account
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
*! Name      : lfPikTktBrow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : browse all piktkt or validated by the account or order
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
lcCurTag = Order('OrdHdr')
SET ORDER TO OrdAcct
lcKey = IIF(EMPTY(laData[4]),'',laData[4])
SET KEY TO lcKey

SELECT PikTkt
SET RELATION TO Account+'O'+Order INTO OrdHdr ADDITIVE
IF EMPTY(laData[2]) AND !EMPTY(laData[4])
  llReturn = AriaBrow("'' FOR PikTkt.Account=laData[4] AND !EOF('OrdHdr') AND PikTkt.Status$'PO'",;
                      lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
                      gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'PikTkt',.F.)
ELSE
  llReturn = AriaBrow("FOR Order = ALLTRIM(laData[2]) AND Status $ 'PO'",;
                      lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
                       gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'PikTkt',.F.)
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : create new pack list according keys fields validation
*!*************************************************************

FUNCTION lfvNewPack
PRIVATE lcTag,lnCurAlias

STORE .T. TO llCupdate,llAnyUpd,llNew

llNew = laScrMode[4]
IF ALLTRIM(OrdHdr.ShipVia)='*' AND SEEK('S'+laData[4]+laData[5],'Customer')
  laData[7]  = Customer.ShipVia
  laCodInfo[1,07] = "customer"                && Alternative File (For default val.)
  laCodInfo[1,08] = "customer"                && Use this index for the Alternative file.
  laCodInfo[1,09] = "'S'+laData[4]+laData[5]"         && Seek this expretion.
  laCodInfo[1,10] = "ShipVia"               && Alternative Field Name
  = gfwCodePop(@laCodInfo, "SHIPVIA", "T")
ENDIF
IF lnActFolder = 2
  = lfWinCtnSt()
ENDIF

= lfvSelOrdL()
  
*-*   llNoThing = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)
llNoThing = lfUpdVars()

*!*************************************************************
*! Name      : lfWinHdrSt
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : adjust enable status for header folder fields
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
IF llBolAutom
  SHOW GET lcBol DISABLE
ELSE
  SHOW GET lcBol &lcInstStat
ENDIF


*!*************************************************************
*! Name      : lfvData_4
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Validat the screen key laData[4] .
*!*************************************************************

FUNCTION lfvData_4

laData[4] = PADR(ALLTRIM(laData[4]), FSIZE('Account', 'TMPL_HDR'))
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
*! Name      : lfOldValue
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : To save the old value befor modifying it
*!*************************************************************

FUNCTION lfOldValue

lcOldVal = EVAL(VARREAD())

*!*************************************************************
*! Name      : lfGetData
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : To restore the order or piktkt informtion form
*!             OrdHdr file or PikTkt file
*!*************************************************************

FUNCTION lfGetData

llNoThing = SEEK('O'+laData[2],'OrdHdr')

laData[4] = IIF(!EMPTY(laData[2]),OrdHdr.Account,SPACE(6))

llEdiAcc   = llEdiSys .AND. SEEK('A' + laData[4] , 'EDIACPRT') .AND.;
             SEEK(EDIACPRT.cPartCode , 'EDIPH')

STORE llEdiSys .AND. llEdiAcc .AND. EDIACPRT.lPkChrDes  TO llPCDStat
STORE llEdiSys .AND. llEdiAcc .AND. EDIPH.lPltShp  TO llPalStat

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
  
  laData[15] = IIF(EMPTY(laData[2]) , SPACE(5) ,;
                   IIF(llPCDStat , EDIACPRT.cPckChCode , SPACE(5)))
  
  laData[16] = IIF(EMPTY(laData[2]) , SPACE(7) ,;
                   IIF(llPCDStat , EDIACPRT.cPckDsCode , SPACE(7)))
  
  lcEdiObSt  = IIF(llPCDStat AND (laScrMode[3] OR llNew) AND lnActFolder = 1,"ENABLE","DISABLE")
  SHOW GET laData[15] &lcEdiObSt
  SHOW GET laData[16] &lcEdiObSt
ELSE
  lnCtnTyp   = 2

  laData[13] = .F.                 && means carton type is standard
  lnDrctTo   = 1
  laData[14] = 'S'                 && means pack is directed to store
  lcCtnStat = "DISABLE"
ENDIF

SHOW GET lnCtnTyp &lcCtnStat
SHOW GET lnDrctTo &lcCtnStat

= lfRefresh(lcWinKey)
*--= lfRefresh(lcWinHdr)
= lfRefresh(lcWinDtl2)
*--= lfRefresh(lcWinCtn3)
SHOW GETS WINDOW (lcWinKey)  ONLY
*--SHOW GETS WINDOW (lcWinHdr)  ONLY      
SHOW GETS WINDOW (lcWinDtl2) ONLY      
*--SHOW GETS WINDOW (lcWinCtn3) ONLY      

*!*************************************************************
*! Name      : lfvSelOrdL
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : To collect order lines
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
        lnCtnQty,lnCtnWgh,lnStyOrdLin,llStyFound,lnRemPQty,lcSz,lcSeek,llPack,lcSeekKey

*-- This variable indecates if the style is found more than once in order
llStyFound = .F.

STORE 0 TO lnContinue,lnCtnQty,lnCtnWgh,lnI,lnJ,lnStyOrdLin,lnRemPQty

IF laScrMode[4] AND llNew
  WAIT WINDOW 'Reading order lines. Please stand by....' NOWAIT
ENDIF

llAnyUpd = .F.

lcPrvPack = laData[2]

lnAlias = ALIAS()

*-- Creating the temp. files
llNothing = lfCrtUnCmp()

SET ORDER TO OrdLinst IN OrdLine
llNoThing = SEEK('O'+laData[2],'OrdLine')

STORE SPACE(0) TO lcStyle,lcStyDesc

SELECT "OrdLine"
=SEEK('O'+laData[2])
SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+laData[2]
  llPack = !EMPTY(PACK_ID)
  IF llPack
    SET ORDER TO 'CSTPKINFP' IN CSTPKINF
    lcSeekKey = 'P'+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION
  ELSE
    SET ORDER TO 'CSTPKINF' IN CSTPKINF
    lcSeekKey = 'S'+ACCOUNT+STYLE
  ENDIF  
  =SEEK(lcSeekKey,'CSTPKINF')
  *B125190,1 NNA 01/16/2005 (BEGIN) If the style not assigned in the Customer Pack Info. Screen
  *B125190,1 NNA             then we'll get the Qty/Crt from the style file
  llDefault = !SEEK(lcSeekKey,'CSTPKINF')
  *B125190,1 NNA (END)
  IF llPack
    *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
    *=SEEK('P'+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'SPCK_LIN')
    llSeek =SEEK('P'+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+DYELOT,'SPCK_LIN')
    IF !llSEEK
      llSeek =SEEK('P*****'+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+DYELOT,'SPCK_LIN')
    ENDIF
    *C038675,1 [End]
  ENDIF
  
  *--PikLine file contains the piked line if the order is invoiced
  llFrmPikLn = SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'PIKLINE') AND PIKLINE.PICKED
  *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
  *lcSeek = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE
  lcSeek = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+DYELOT
  *C038675,1 [End]
  llAdd = !SEEK(lcSeek,lcPckLin)
  SELECT (lcPckLin)
  FOR lnIScl = 1 TO SCALE.CNT    
    lcSz = STR(lnIScl,1)
    IF llAdd
      APPEND BLANK   
      REPLACE Style      WITH IIF(llFrmPikLn,PikLine.Style,OrdLine.Style),;             
              Scale      WITH Scale.Scale,;
              OrgStyWgh  WITH Style.nStyWeight,;
              PkStyQty   WITH SPCK_LIN.QTY&lcSz,;
              CtnQty     WITH IIF(llPack , CSTPKINF.NINPACK * SPCK_LIN.QTY&lcSz , CSTPKINF.NINPACK ),;
              StyWgh     WITH IIF(llPack , Style.nStyWeight , IIF(!EMPTY(CSTPKINF.NMPWEIGHT),CSTPKINF.NMPWEIGHT,Style.nStyWeight) ),; 
              LPicked    WITH IIF(llFrmPikLn,PikLine.Picked,OrdLine.Picked),;
              cSizeNo    WITH lcSz,;
              cSize      WITH Scale.Sz&lcSz  ,;
              PACK_ID    WITH IIF(llFrmPikLn , PikLine.PACK_ID    , OrdLine.PACK_ID   ),;
              cPkColor   WITH IIF(llFrmPikLn , PikLine.cPkColor   , OrdLine.cPkColor  ),;
              cPckSize   WITH IIF(llFrmPikLn , PikLine.cPckSize   , OrdLine.cPckSize  ),;
              cPkVersion WITH IIF(llFrmPikLn , PikLine.cPkVersion , OrdLine.cPkVersion)

      *C122070,1 HBG 04/04/2004 Update the new filed "lRange" with its value from ORdline file to know if this pack is rang or not[Begin]
      REPLACE lRange     WITH IIF(llFrmPikLn , PikLine.lRange     , OrdLine.lRange)
      *C122070,1 [End]
      *C038675,1 HBG 10/26/2004 Update Configuration field [Begin]
      REPLACE DYELOT WITH IIF(llFrmPikLn,PikLine.Dyelot,OrdLine.Dyelot) 
      *C038675,1 [End]

      *B125190,1 NNA 01/16/2005 (Begin) in case of this line have (lRange=.T.) then do the following
      *B125190,1 NNA            line to use field DISCTNQTY in Validating the Reange Pack
      REPLACE DISCTNQTY WITH IIF(lRange,CtnQty,0)	,;
              CtnQty 	WITH IIF(llDefault,STYLE.Qty_Ctn,CtnQty)
      *B125190,1 NNA (End)
      
      llIsPck = !EMPTY(PACK_ID)
    ELSE
      =SEEK(lcSeek+lcSz,lcPckLin)
    ENDIF
    
    REPLACE AvlQty  WITH AvlQty + IIF(llFrmPikLn,PikLine.Pik&lcSz,IIF(!EMPTY(ORDLINE.PIKTKT),;
                                                 OrdLine.Pik&lcSz,OrdLine.Qty&lcSz)),;
            OrdQty  WITH OrdQty + IIF(llFrmPikLn,PikLine.Qty&lcSz,OrdLine.Qty&lcSz),;
            OrgOrd  WITH OrgOrd + IIF(llFrmPikLn,PikLine.Qty&lcSz,OrdLine.Qty&lcSz),;
            OQty    WITH AvlQty  ,;
            OrgPQty WITH OrdLine.nPck&lcSz
*            OQty    WITH MAX(0,AvlQty-Ordline.nPck&lcSz),;
    llAnyRec = .T.              
  ENDFOR
ENDSCAN 

*B123477,4  TMI [Start] Show all lines again
*-* *B123477,1  TMI [Start] Remove lines from lcPckLin file for range packs except first style-size,
*-* *                       This is due the way that range packs are packed in cartons, that we care only 
*-* *                       with qty regardless of styles in the pack
*-* SELECT &lcPckLin
*-* SET FILTER TO LRANGE
*-* GO TOP
*-* DO WHILE !EOF()
*-*   lcRtnStySz = PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo     
*-*   lcRngPck   = PACK_ID+cPkColor+cPckSize+cPkVersion
*-*   *- This loop is just to skip first style 
*-*   SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcRtnStySz  && Reain this style size
*-*     REPLACE OQTY   WITH IIF(OQTY  =0,1,OQTY  );
*-*             AvlQty WITH IIF(AvlQty=0,1,AvlQty)
*-*   ENDSCAN
*-*   *- Delete rest lines for style other than first one
*-*   DELETE REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcRngPck
*-* ENDDO
*-* SET FILTER TO 
*-* GO TOP
*-* *B123477,1  TMI [End  ] 
*B123477,4  TMI [End  ] 

*--collect data for temp file in case of pack summery
=lfColSum()

SELECT (lcPckLin)
GO TOP
=RLOCK(lcPckLin) 
UNLOCk IN (lcPckLin)

IF !USED(lcTmpPck)
  USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPakIndxSt)
ENDIF

IF !WEXIST(lcDtlBrTtl) AND lnActFolder = 1
  =lfDtlBrow()
ELSE
  IF lnActFolder = 1
    =lfwDtlBrow()
  ENDIF
ENDIF

SELECT(lcCtnHdr)
=RLOCK(lcCtnHdr) 
UNLOCk IN (lcCtnHdr)
GO TOP

SELECT(lcCtnDtl)
UNLOCk IN (lcCtnDtl)

IF !WEXIST(lcCtHdBrTt) AND lnActFolder = 2
  = lfCtnHdrBr()
ELSE
  IF lnActFolder = 2
    = lfwCtnHdrBr()    
  ENDIF
ENDIF

IF !WEXIST(lcCtDtBrTt) AND lnActFolder = 2
  = lfCtnDtlBr()
ELSE
  IF lnActFolder = 2
    = lfwCtnDtlBr()
  ENDIF
ENDIF

IF laScrMode[4] AND llNew
  WAIT CLEAR
ENDIF  
IF laScrMode[2] 
  =lfGetTmpl()
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfChkStyQty
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : To compare between order qty and packed qty 
*!             in copying from another pack
*!*************************************************************

FUNCTION lfChkStyQty

PARAMETERS lcSizeCode

PRIVATE lcSizeCode,lnReturn,lnCurAlias,lnChoice

lnCurAlias = SELECT(0)

lnChoice = 0
lnReturn = 0

IF (EMPTY(laData[3]) AND !OrdLine.Picked) OR ;
   (!EMPTY(laData[3]) AND OrdLine.Picked AND EVAL("OrdLine.Pik"+lcSizeCode)>0)

  lcQtyFld = 'TMPL_LIN.Qty'+ lcSizeCode
  lcOrdQtyFld = lcPckLin + '.OrdQty'  + lcSizeCode
  lcPQtyFld   = lcPckLin + '.PQty'    + lcSizeCode
  lcOQtyFld   = lcPckLin + '.OQty'    + lcSizeCode
  lcCtnQtyFld = lcPckLin + '.CtnQty'  + lcSizeCode
  lcSizeFld   = lcPckLin + '.cSize'   + lcSizeCode
  lcPWghFld   = lcPckLin + '.PWgh'    + lcSizeCode
  lcRemVar    = "lnRemPQty" + lcSizeCode

  IF !llStyFound AND (&lcOrdQtyFld < &lcPQtyFld + &lcRemVar)
    *-- Packed quantity for Style/Size x/x exceeds ordered quantity.;
       Do you want to modify the orderd quantity?
    *-- <Yes>,<No>
    lnChoice = gfModalGen("QRM44034B00006","Dialog",TMPL_LIN.Style + '/' + EVAL(lcSizeFld))
  ENDIF  

  lnReturn = IIF(llStyFound,&lcOQtyFld,IIF(lnChoice<>2,&lcRemVar,&lcOQtyFld))

  SELECT (lcPckLin)
  REPLACE &lcPQtyFld   WITH IIF(llStyFound,&lcOQtyFld,IIF(lnChoice<>2,&lcRemVar,&lcOrdQtyFld)),;
          &lcOQtyFld   WITH IIF(&lcOrdQtyFld-&lcPQtyFld<0,0,&lcOrdQtyFld-&lcPQtyFld),;
          &lcPWghFld   WITH (TMPL_LIN.Weight/TMPL_LIN.TotQty)* &lcPQtyFld,;
          &lcOrdQtyFld WITH IIF(llStyFound,&lcOrdQtyFld,IIF(lnChoice<>1,&lcOrdQtyFld,&lcRemVar))

  &lcRemVar = &lcRemVar - lnReturn
  
ENDIF

SELECT(lnCurAlias)

RETURN lnReturn

*!*************************************************************
*! Name      : lfvData_13
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Validat lnCtnTyp field .
*!*************************************************************

FUNCTION lfvData_13
*--Set the value to Standered,Store instead of N/A
laData[13] = (lnCtnTyp=1)

llNoThing = lfUpdVars()

*!*************************************************************
*! Name      : lfvData_14
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Validat lnDrctTo field .
*!*************************************************************

FUNCTION lfvData_14

*--Update field of To Store or consalidatore in BOL_HDR. [Begin]
laData[14] = IIF(lnDrctTo=2,"C","S")
IF SEEK(lcBol,'BOL_HDR')
  lnPrvAlis = SELECT(0)
  SELECT BOL_HDR
  REPLACE BOL_HDR.cToStorCn WITH laData[14]
  SELECT (lnPrvAlis)
ENDIF

llNoThing = lfUpdVars()

*!*************************************************************
*! Name      : lfvShpVia
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Validat lnShpVia field .
*!*************************************************************

FUNCTION lfvShpVia

*--Check if shipvia was changed or not [Begin]
PRIVATE llGetShipV
llGetShipV = (laData[7] <> laShpVia[lnShpVia,2])

laData[7] = laShpVia[lnShpVia,2]

=llGetShipV()

SHOW GET lcBOL

llNoThing = lfUpdVars()

*!*************************************************************
*! Name      : lfvCtnSty
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Validat lcCtnSty field .
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

SELECT PackLines

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
  lcStySz   = IIF(llNoThing,PackLines.cSize,SPACE(5))
  lnStyQty  = IIF(llNoThing,PackLines.OQty,0)
  lnWgh     = IIF(llNoThing,PackLines.OQty*PackLines.StyWgh,0)
ENDIF

IF !EMPTY(lcCtnSty)
  SELECT (lcCtnDtl)
  IF EMPTY(lcStySz) 
    REPLACE Style WITH lcCtnSty
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
          REPLACE Style      WITH lcCtnSty ,;
                  Br1        WITH .F.      ,;
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
          REPLACE TotPcs WITH TotPcs + EVAL(lcCtnDtl+'.'+lcQtyFld),;
                  TotWgh WITH TotWgh + EVAL(lcCtnDtl+'.'+lcWghFld),;
                  Empty  WITH IIF(TotPcs>0,'N','Y')
          = RLOCK(lcCtnHdr) 
          UNLOCk IN (lcCtnHdr)
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
          UNLOCk IN (lcCtnHdr)
          SELECT(lcCtnDtl)
      ELSE
        SKIP lnK - 1 IN (lcCtnDtl)
      ENDIF
      lnCtnRec = RECNO(lcCtnDtl)
      lnSzRec  = RECNO(lcCartonSz)
      IF SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl) AND Br1 AND EMPTY(Size1)
         REPLACE Style WITH SPACE(19),;
                 Br1   WITH .F.
         GOTO lnCtnRec IN (lcCtnDtl)
         SKIP lnSzRec - 1 IN (lcCtnDtl)
      ENDIF
    ENDIF

    lcExpGma = "SEEK(&lcTmpPck..Pack_ID+&lcTmpPck..cPkColor+&lcTmpPck..cPckSize+&lcTmpPck..cPkVersion+lcCtnSty+STR(lnOrdLine,6),lcPckLin)"
    IF &lcExpGma

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
       UNLOCk IN (lcPckLin)
      ENDIF
    ENDIF
  ENDIF
ENDIF  

= RLOCK(lcCtnDtl)
UNLOCk IN (lcCtnDtl)

=lfwCtnDtlBr()

USE IN PackLines
USE IN StyQty  

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfStyBrow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Browse order styles/sizes.
*!*************************************************************

FUNCTION lfStyBrow

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "Style,cSize,nOrdLineNo,OQty,StyWgh"
lcBrFields  = [StyCode=Style:H='Style',]+;
              [cSize:H='Size',]+;
              [StyWgh     :H='Wgh\Unit',]+;
              [OQty:H='Open Quantity']
lcFile_Ttl  = 'Style Open Quantities'

lcForExp = "FOR EMPTY(NCARTON)" 
           
SELECT PackLines

DECLARE laTemp[1]

llReturn  = gfBrows(lcForExp,'Style','laTemp')                       

SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfChkSty
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Check the existence of the style/size. 
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Browse order styles/sizes.
*!*************************************************************

FUNCTION lfSizeBrow

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "Style,nOrdLineNo,OTotQty,StyWgh"
lcBrFields  = [StyCode=Style:H='Style',]+;
              [SzWgh  =StyWgh     :H='Wgh\Unit',]+;
              [Open   =OTotQty:H='Open Quantity']

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
*! Name      : lfvCtDtWgh
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : validate Style weight in carton
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
  lnStyRec = RECNO(lcPckLin)

  IF SEEK(STR(lnCart,4)+lcStyle,lcCtnDtl)
    lnStyRec = RECNO(lcCtnDtl)
    lnSzeRec = RECNO(lcCartonSz)
    lcWghFld = 'Weight'+&lcCartonSz..SzCode
    SELECT(lcCtnDtl)

    REPLACE REST FOR STR(Cart_No,4)+Style = STR(lnCart,4)+lcStyle ;
                     Weight1 WITH Qty1*lnUnitWgh,; 
                     Weight2 WITH Qty2*lnUnitWgh,;
                     Weight3 WITH Qty3*lnUnitWgh,;
                     Weight4 WITH Qty4*lnUnitWgh,;
                     Weight5 WITH Qty5*lnUnitWgh,;
                     Weight6 WITH Qty6*lnUnitWgh,;
                     Weight7 WITH Qty7*lnUnitWgh,;
                     Weight8 WITH Qty8*lnUnitWgh,;
                     cStatus WITH 'M',;
                     TotWeight WITH (Weight1+Weight2+Weight3+Weight4+Weight5+Weight6+Weight7+Weight8)
                     
                     
    IF lnStyRec <= RECCOUNT(lcCtnDtl) 
      GOTO lnStyRec
      SKIP lnSzeRec-1
    ENDIF
    = RLOCK(lcCtnDtl) 
    UNLOCk IN (lcCtnDtl)
    IF SEEK(STR(&lcCtnDtl..Cart_No,4),lcCtnHdr)
      SELECT SUM(Weight1+Weight2+Weight3+Weight4+Weight5+Weight6+Weight7+Weight8);
        FROM (lcCtnDtl) INTO ARRAY laCtnWgh;
        WHERE Cart_No = EVAL(lcCtnHdr+'.Cart_No')

      lnPackWgh = lnPackWgh - &lcCtnHdr..TotWgh + laCtnWgh

      SELECT(lcCtnHdr)
      REPLACE TotWgh WITH laCtnWgh
      = RLOCK(lcCtnHdr) 
      UNLOCk IN (lcCtnHdr)
    ENDIF
  ENDIF

  = lfwCtnHdrBr()

  SELECT(lnCurAlias)
  
ENDIF

*!*************************************************************
*! Name      : lfvCtHdWgh
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : validate Carton total weight 
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
    *HBG
    *SET RELATION TO STR(&lcCtnHdr..Cart_No,4)+Style+STR(nOrdLineNo,6) INTO (lcCtnDtl)
    SET RELATION TO STR(&lcCtnHdr..Cart_No,4)+Style+Dyelot+STR(nOrdLineNo,6) INTO (lcCtnDtl)
    *HBG
    lnStyRec = RECNO(lcPckLin)

    REPLACE ALL FOR Style+STR(nOrdLineNo,6)=&lcCtnDtl..Style+STR(nOrdLineNo,6);
                PWgh1 WITH IIF(&lcCtnDtl..Br1,lfPckLinUp('1'),PWgh1),;
                PWgh2 WITH IIF(&lcCtnDtl..Br2,lfPckLinUp('2'),PWgh2),;
                PWgh3 WITH IIF(&lcCtnDtl..Br3,lfPckLinUp('3'),PWgh3),;
                PWgh4 WITH IIF(&lcCtnDtl..Br4,lfPckLinUp('4'),PWgh4),;
                PWgh5 WITH IIF(&lcCtnDtl..Br5,lfPckLinUp('5'),PWgh5),;
                PWgh6 WITH IIF(&lcCtnDtl..Br6,lfPckLinUp('6'),PWgh6),;
                PWgh7 WITH IIF(&lcCtnDtl..Br7,lfPckLinUp('7'),PWgh7),;
                PWgh8 WITH IIF(&lcCtnDtl..Br8,lfPckLinUp('8'),PWgh8),;
                PTotWgh WITH (PWgh1+PWgh2+PWgh3+PWgh4+PWgh5+PWgh6+PWgh7+PWgh8)
    SET RELATION TO
    *SET RELATION TO '' INTO (lcScaFile)
    *SET SKIP TO (lcScaFile)
    IF lnStyRec <= RECCOUNT(lcPckLin) 
      GOTO lnStyRec
      SKIP lnSzeRec-1
    ENDIF
    = RLOCK(lcPckLin) 
    UNLOCk IN (lcPckLin)

    SELECT(lcCtnDtl)
    lnStyRec = RECNO(lcCtnDtl)
    lnSzeRec = RECNO(lcCartonSz)
    SET RELATION TO 
    = SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl)

    REPLACE REST FOR STR(Cart_No,4)+Style+STR(nOrdLineNo,6) = STR(&lcCtnHdr..Cart_No,4) ;
                 Weight1 WITH IIF(Br1,lfCtnDtlUp('1'),Weight1),;
                 Weight2 WITH IIF(Br2,lfCtnDtlUp('2'),Weight2),;
                 Weight3 WITH IIF(Br3,lfCtnDtlUp('3'),Weight3),;
                 Weight4 WITH IIF(Br4,lfCtnDtlUp('4'),Weight4),;
                 Weight5 WITH IIF(Br5,lfCtnDtlUp('5'),Weight5),;
                 Weight6 WITH IIF(Br6,lfCtnDtlUp('6'),Weight6),;
                 Weight7 WITH IIF(Br7,lfCtnDtlUp('7'),Weight7),;
                 Weight8 WITH IIF(Br8,lfCtnDtlUp('8'),Weight8),;
                 cStatus WITH 'M',;
                 TotWeight WITH (Weight1+Weight2+Weight3+Weight4+Weight5+Weight6+Weight7+Weight8)

    SET RELATION TO '' INTO (lcCartonSz)
    SET SKIP TO (lcCartonSz)
    

    IF lnStyRec <= RECCOUNT(lcCtnDtl) 
      GOTO lnStyRec
      SKIP lnSzeRec-1
    ENDIF
    = RLOCK(lcCtnDtl) 
    UNLOCk IN (lcCtnDtl)
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Update the weight in file lcPckLin according to
*!             update the total carton weight field
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Update the weight in lcCtnDtl file according to
*!             update the total carton weight field
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : validate pbHdrNew button
*!*************************************************************

FUNCTION lfvCtHdNew
PRIVATE lnCurAlias

STORE .T. TO llCupdate,llAnyUpd

lnCurAlias = SELECT(0)

SELECT (lcCtnHdr)
APPEND BLANK

lnPackCtn = lnPackCtn + 1   
lnMaxCtn  = lnMaxCtn  + 1

lnFrom = lnMaxCtn + 1
lnTo   = lnMaxCtn + 1

=lfNewCrt()
REPLACE Cart_No WITH lnCurrCtn,;
        Empty   WITH 'Y'
= RLOCK(lcCtnHdr) 
UNLOCk IN (lcCtnHdr)

lcAddDtlSt = "ENABLE"
SHOW GET pbDtlNew  &lcAddDtlSt

= lfwCtnHdrBr()

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCtHdRem
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : validate pbHdrRem button
*!*************************************************************

FUNCTION lfvCtHdRem

PRIVATE lnAlias,lcFltrExp,lnStyRec,lnSzeRec,lcPrvOrd,lcSeek

STORE 0 TO lnStyRec,lnSzeRec

STORE .T. TO llCupdate,llAnyUpd

lnAlias = SELECT(0)

lcPrvOrd = ORDER(lcPckLin)
SET ORDER TO &lcPakIndxSt IN (lcPckLin)   

*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>
IF gfModalGen("INM44040B00006","Dialog") = 1

  SELECT(lcPckLin)
  lcFltrExp = SET("FILTER")
  SET FILTER TO Style = ''
  SET RELATION TO

  SELECT(lcCtnDtl)
  SET RELATION TO
  IF SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl)
    lnStyRec = RECNO(lcPckLin)
    SCAN REST FOR STR(Cart_No,4)+Style = STR(&lcCtnHdr..Cart_No,4)
      lcSeek = PACK_ID+cPkColor+cPckSize+cPkVersion+Style
      
      SELECT(lcPckLin)   
      IF !EMPTY(&lcCtnDtl..Style) 
        FOR lnI = 1 TO 8
          SELECT(lcPckLin)   
          lcI = STR(lnI,1)
          IF &lcCtnDtl..Br&lcI
            IF SEEK(lcSeek+lcI,lcPckLin)     
              BLANK FIELDS NCARTON
              IF !EMPTY(&lcPckLin..PACK_ID)
                IF SEEK(PADR(&lcPckLin..PACK_ID,19)+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion,lcSumPck)
                  SELECT &lcSumPck
                  BLANK FIELDS NCARTON
                ENDIF              
              ENDIF
            ENDIF
          ENDIF
        ENDFOR    
      ENDIF

      SELECT (lcCtnDtl)
      REPLACE cStatus WITH 'D'
      DELETE
      
    ENDSCAN
    

  ENDIF
  SELECT (lcPckLin)  
  SET FILTER TO &lcFltrExp
  
  = RLOCK(lcCtnDtl) 
  UNLOCk IN (lcCtnDtl)

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
  =RLOCK(lcCtnHdr) 
  UNLOCk IN (lcCtnHdr)

  lnMaxCtn = MAX(lnMaxCtn - 1,0)
  lnFrom   = lnMaxCtn + 1
  lnTo     = lnMaxCtn + 1

  llNoThing = lfwCtnHdrBr()
  
  SHOW WINDOW (lcWinCtn1) REFRESH SAME
  SHOW WINDOW (lcWinCtn2) REFRESH SAME
  
ENDIF

SET ORDER TO &lcPrvOrd IN (lcPckLin)

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvCtDtNew
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : validate pbDtlNew button
*!*************************************************************

FUNCTION lfvCtDtNew
PRIVATE lnCurAlias,lcBrFld,lcNewFld,lnK

STORE .T. TO llCupdate,llAnyUpd
lnCurAlias = SELECT(0)

SELECT(lcCtnDtl)

IF !SEEK(STR(&lcCtnHdr..Cart_NO,4)+SPACE(48),lcCtnDtl)
  APPEND BLANK
  REPLACE Cart_No WITH &lcCtnHdr..Cart_NO,;
          SzCnt   WITH 1,;
          Br1     WITH .T.
     
  =RLOCK(lcCtnDtl) 
  UNLOCk IN (lcCtnDtl)
ELSE
  REPLACE Br1     WITH .T.
ENDIF
=lfwCtnDtlBr()

SHOW GET pbDtlRem ENABLE

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCtDtRem 
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : validate pbDtlRem button
*!*************************************************************

FUNCTION lfvCtDtRem

PRIVATE lnAlias,lcFltrExp,lcSzFld,lcSvOrd,lnResp,lcSeekPack,lnWgh,lnQty 

STORE .T. TO llCupdate,llAnyUpd
lnAlias = SELECT(0)

lcSvOrd = ORDER(lcPckLin)
SET ORDER TO lcPakIndxSt IN (lcPckLin) 

*==>|* TMI 06/11/2003 I started to do this peice of code , then I thought that it is better not to delever it unless
*==>|*                GMA asks for it , It works but needs some test
*==>|* *C200519,5  TMI [Start] Check if this is a pack, delete all the pack from the carton
*==>|* IF !EMPTY(&lcCtnDtl..PACK_ID)
*==>|*   lnResp = gfModalGen("INM00000B00006",.F.,.F.,.F.,;
*==>|*            'This is a pack, are you sure to delete all the pack from this carton?')
*==>|*   IF lnResp = 1
*==>|*     SELECT &lcCtnDtl 
*==>|*     lcSeekPack = STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION
*==>|*     IF SEEK(lcSeekPack,lcCtnDtl)
*==>|*       SELECT &lcCtnDtl 
*==>|*       SCAN REST WHILE STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION = lcSeekPack
*==>|*       *1
*==>|*         
*==>|*         lnWgh = Weight1+Weight2+Weight3+Weight4+Weight5+Weight6+Weight7+Weight8
*==>|*         lnQty = Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
*==>|*         lnPackWgh = MAX(lnPackWgh-lnWgh,0)
*==>|*         lnPackQty = MAX(lnPackQty-lnQty,0)
*==>|* 
*==>|*         SELECT (lcCtnHdr)
*==>|*         REPLACE TotPcs WITH MAX(TotPcs-lnQty,0),;
*==>|*                 TotWgh WITH MAX(TotWgh-lnWgh,0),;
*==>|*                 Empty  WITH IIF(TotPcs>0,'N','Y')
*==>|*       
*==>|*         SELECT (lcCtnDtl)
*==>|*         IF SEEK(PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,lcPckLin)
*==>|*           lcCtnSty = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE 
*==>|*           SELECT(lcPckLin)
*==>|*           SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcCtnSty          
*==>|*             BLANK FIELDS NCARTON,Selected,cSelect
*==>|*           ENDSCAN
*==>|*         ENDIF  
*==>|*         
*==>|*         =RLOCK(lcCtnHdr) 
*==>|*         UNLOCk IN (lcCtnHdr)
*==>|*           
*==>|*         SELECT (lcCtnDtl)
*==>|*         BLANK FIELDS Br1,Br2,Br3,Br4,Br5,Br6,Br7,Br8,;
*==>|*                      Size1,Size2,Size3,Size4,Size5,Size6,Size7,Size8,;
*==>|*                      Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,;
*==>|*                      Cart_No,Style
*==>|*         REPLACE cStatus WITH 'D'
*==>|*       
*==>|*       *2        
*==>|*       ENDSCAN
*==>|*   
*==>|*       SELECT &lcCtnDtl      
*==>|*       GO TOP
*==>|*       DELETE FOR cStatus = 'D'
*==>|*       
*==>|*       GO TOP
*==>|*       SET SKIP TO &lcSvSkip
*==>|*     
*==>|*       llNoThing = lfwCtnDtlBr()
*==>|*     ENDIF
*==>|*   ENDIF
*==>|*   SET ORDER TO lcSvOrd IN (lcPckLin)
*==>|*   SELECT (lnAlias) 
*==>|*   RETURN
*==>|* ENDIF
*==>|* *C200519,5  TMI [End  ] 


*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>
IF gfModalGen("INM44040B00006","Dialog") = 1
  
  lnPackWgh = MAX(lnPackWgh - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode),0)
  lnPackQty = MAX(lnPackQty - EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode),0)
  
  SELECT (lcCtnDtl)
  IF SEEK(PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+&lcCartonSz..SzCode,lcPckLin)
    SELECT(lcPckLin)
    BLANK FIELDS NCARTON,Selected
  ENDIF  
  
  SELECT (lcCtnHdr)
  REPLACE TotPcs WITH MAX(TotPcs - EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode),0),;
          TotWgh WITH MAX(TotWgh - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode),0),;
          Empty  WITH IIF(TotPcs>0,'N','Y')

  =RLOCK(lcCtnHdr) 
  UNLOCk IN (lcCtnHdr)

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
  lnMn = 0
  FOR lnM = 1 TO 8
    lnMn = lnMn +1
    IF EVAL('Br'+STR(lnM,1))
      EXIT
    ENDIF
  ENDFOR
  IF lnMn <= 8
    GO TOP IN (lcCartonSz)
    SKIP lnM-1 IN (lcCtnDtl)
  ELSE
    SELECT (lcCtnDtl)
    REPLACE Cart_No WITH 0 ,;
            Style   WITH ''
  ENDIF
  =RLOCK(lcCtnDtl) 
  UNLOCk IN (lcCtnDtl)
  llNoThing = lfwCtnDtlBr()
ENDIF

SET ORDER TO lcSvOrd IN (lcPckLin)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfActFolder
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Activate folder when Change.
*!*************************************************************

FUNCTION lfActFolder

STORE IIF((laScrMode[3] OR llNew) AND lnActFolder = 1,"ENABLE","DISABLE") TO lcInstStat,lcShipStat

lnLastFold = lnActFolder
llNoThing = lfUpdVars()

DO CASE
  CASE lnActFolder = 1
    SHOW GETS WINDOW (lcWinCtn1)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCtn2)  DISABLE ONLY
    = lfSelStatus()

    IF !WEXIST(lcDtlBrTtl)
      = lfDtlBrow()
    ELSE
      = lfwDtlBrow()
    ENDIF

    *--Update the user defined field variable LLCKONERR
    IF ASCAN(laUsrFields,'LLCKONERR')>0
      laUsrFields[ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'LLCKONERR'),1),6] = llLockOnErr
    ENDIF
    
  CASE lnActFolder = 2
    SHOW GETS WINDOW (lcWinDtl1)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinDtl2)  DISABLE ONLY    
    = lfWinCtnSt()
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : TO Assign functions to some keys to not affect the browse
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
  IF WONTOP() = lcDtlBrTtl
    ON KEY LABEL LEFTMOUSE DO lfSelCurr
  ENDIF
ELSE
  glFromBrow = .F.
ENDIF

*!*************************************************************
*! Name      : lfClrTrap
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Clearing the previous trapping
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
ON KEY LABEL LEFTMOUSE

*!*************************************************************
*! Name      : lfBrTab
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Trap the Tab Key
*!*************************************************************

FUNCTION lfBrTab

ON KEY LABEL TAB
DO CASE
  CASE lnActFolder = 1 AND WONTOP() = lcDtlBrTtl
    ACTIVATE WINDOW (lcWinDtl2)
    _CUROBJ = OBJNUM(llLockOnErr)
  CASE lnActFolder = 2 AND WONTOP() = lcCtHdBrTt
    ACTIVATE WINDOW (lcWinCtn3)
    _CUROBJ = OBJNUM(pbHdrNew)
  CASE lnActFolder = 2 AND WONTOP() = lcCtDtBrTt
    ACTIVATE WINDOW (lcWinCtn3)
    _CUROBJ = OBJNUM(pbDtlNew)
ENDCASE    

*!*************************************************************
*! Name      : lfBrBack
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Trap the BackTab Key
*!*************************************************************

FUNCTION lfBrBack

ON KEY LABEL BACKTAB
DO CASE
  CASE lnActFolder = 1 AND WONTOP() = lcDtlBrTtl
    ACTIVATE WINDOW (lcFolder)
    _CUROBJ = OBJNUM(ibFolder[2])
  CASE lnActFolder = 2 AND WONTOP() = lcCtHdBrTt
    ACTIVATE WINDOW (lcFolder)
    _CUROBJ = OBJNUM(ibFolder[3])
  CASE lnActFolder = 2 AND WONTOP() = lcCtDtBrTt
*--    ACTIVATE WINDOW (lcWinCtn3)
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Browse lcPckLin file 
*!*************************************************************

FUNCTION lfDtlBrow

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

IF !USED(lcTmpPck)
  USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPakIndxSt)
ENDIF

IF llShoPckDt
  SELECT (lcTmpPck)
  lnDtBrRec = RECNO()
  
  *C122070,1 HBG 04/04/2004 Modify the browse fileds in detail mode to handle case of range pack[Begin]
  *lcBrFlds = "cMarker =IIF(RECNO(lcTmpPck)=lnDtBrRec ,'>',' '):H= ' ':R:1:W=.F.,"+;
            "cSelect :H = ' ':R:1:W=.F.,"+;
            "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :30:R,"+;
            "StyCode =Style:H = lcStyTtl    :25:R,"+;
            "cSize   :H = 'Size'  :7 :R,"+;
            "OQty    :H = 'O.Qty.':6 :R,"+;
            "CtnQty  :H = 'Qty.\Ctn':10 :W=(laScrMode[3].OR.laScrMode[4]).AND.EMPTY(PACK_ID) .AND. EMPTY(nCarton) :V=lfvQtyCtn(),"+;
            "StyWgh  :H = 'Wgh.\Unt':10 :W=(laScrMode[3].OR.laScrMode[4]) .AND. lfEmpCtn() :V=lfvWeight(),"+;
            "nCarton :H=  'Carton#':8 :R"
  *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
  *lcBrFlds = "cMarker =IIF(RECNO(lcTmpPck)=lnDtBrRec ,'>',' '):H= ' ':R:1:W=.F.,"+;
             "cSelect :H = ' ':R:1:W=.F.,"+;
             "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :30:R,"+;
             "StyCode =Style:H = lcStyTtl    :25:R,"+;
             "cSize     :H = 'Size'  :7 :R,"+;
             "OQty      :H = 'O.Qty.':6 :R,"+;
             "DisCtnQty :H = 'Qty.\Ctn':10 :W=(laScrMode[3].OR.laScrMode[4]).AND. EMPTY(PACK_ID) .AND. EMPTY(nCarton) :V=lfvQtyCtn(),"+;
             "StyWgh    :H = 'Wgh.\Unt':10 :W=(laScrMode[3].OR.laScrMode[4]) .AND. lfEmpCtn() :V=lfvWeight(),"+;
             "nCarton =IIF(lRange AND laScrMode[4],nRngCart, nCarton):H=  'Carton#':8 :R"
  IF llUseConfg

    *B125190,1 NNA 01/16/2005 (BEGIN) if this a Range Pack then use DisCtnQty field 
    IF EVAL(lcTmpPck+'.LRange')
    *B125190,1 NNA (END)

      lcBrFlds = "cMarker =IIF(RECNO(lcTmpPck)=lnDtBrRec ,'>',' '):H= ' ':R:1:W=.F.,"+;
                 "cSelect :H = ' ':R:1:W=.F.,"+;
                 "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :36:R,"+;
                 "StyCode =Style:H = lcStyTtl    :25:R,"+;
                 "Dyelot    :H = 'Configuration' :15:R,"+;             
                 "cSize     :H = 'Size'  :7 :R,"+;
                 "OQty      :H = 'O.Qty.':6 :R,"+;
                 "DisCtnQty :H = 'Qty.\Ctn':10 :W=(laScrMode[3].OR.laScrMode[4]).AND. EMPTY(PACK_ID) .AND. EMPTY(nCarton) :V=lfvQtyCtn(),"+;
                 "StyWgh    :H = 'Wgh.\Unt':10 :W=(laScrMode[3].OR.laScrMode[4]) .AND. lfEmpCtn() :V=lfvWeight(),"+;
                 "nCarton =IIF(lRange AND laScrMode[4],nRngCart, nCarton):H=  'Carton#':8 :R"

    *B125190,1 NNA 01/16/2005 (BEGIN) if this not a Range Pack then use CtnQty field 
    ELSE
    lcBrFlds = "cMarker =IIF(RECNO(lcTmpPck)=lnDtBrRec ,'>',' '):H= ' ':R:1:W=.F.,"+;
               "cSelect :H = ' ':R:1:W=.F.,"+;
               "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :36:R,"+;
               "StyCode =Style:H = lcStyTtl    :25:R,"+;
               "Dyelot    :H = 'Configuration' :15:R,"+;             
               "cSize     :H = 'Size'  :7 :R,"+;
               "OQty      :H = 'O.Qty.':6 :R,"+;
               "CtnQty    :H = 'Qty.\Ctn(Pcs)':13 :W=(laScrMode[3].OR.laScrMode[4]).AND. EMPTY(PACK_ID) .AND. EMPTY(nCarton) :V=lfvQtyCtn(),"+;
               "StyWgh    :H = 'Wgh.\Unt':10 :W=(laScrMode[3].OR.laScrMode[4]) .AND. lfEmpCtn() :V=lfvWeight(),"+;
               "nCarton =IIF(lRange AND laScrMode[4],nRngCart, nCarton):H=  'Carton#':8 :R"
    ENDIF
    *B125190,1 NNA (END)

  ELSE
    *B125190,1 NNA 01/16/2005 (BEGIN) if this a Range Pack then use DisCtnQty field 
    IF EVAL(lcTmpPck+'.LRange')
    *B125190,1 NNA (END)

      lcBrFlds = "cMarker =IIF(RECNO(lcTmpPck)=lnDtBrRec ,'>',' '):H= ' ':R:1:W=.F.,"+;
                 "cSelect :H = ' ':R:1:W=.F.,"+;
                 "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :36:R,"+;
                 "StyCode =Style:H = lcStyTtl    :25:R,"+;
                 "cSize     :H = 'Size'  :7 :R,"+;
                 "OQty      :H = 'O.Qty.':6 :R,"+;
                 "DisCtnQty :H = 'Qty.\Ctn':10 :W=(laScrMode[3].OR.laScrMode[4]).AND. EMPTY(PACK_ID) .AND. EMPTY(nCarton) :V=lfvQtyCtn(),"+;
                 "StyWgh    :H = 'Wgh.\Unt':10 :W=(laScrMode[3].OR.laScrMode[4]) .AND. lfEmpCtn() :V=lfvWeight(),"+;
                 "nCarton =IIF(lRange AND laScrMode[4],nRngCart, nCarton):H=  'Carton#':8 :R"

    *B125190,1 NNA 01/16/2005 (BEGIN) if this not a Range Pack then use CtnQty field 
    ELSE
      lcBrFlds = "cMarker =IIF(RECNO(lcTmpPck)=lnDtBrRec ,'>',' '):H= ' ':R:1:W=.F.,"+;
                 "cSelect :H = ' ':R:1:W=.F.,"+;
                 "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :36:R,"+;
                 "StyCode =Style:H = lcStyTtl    :25:R,"+;
                 "cSize     :H = 'Size'  :7 :R,"+;
                 "OQty      :H = 'O.Qty.':6 :R,"+;
                 "CtnQty    :H = 'Qty.\Ctn(Pcs)':13 :W=(laScrMode[3].OR.laScrMode[4]).AND. EMPTY(PACK_ID) .AND. EMPTY(nCarton) :V=lfvQtyCtn(),"+;
                 "StyWgh    :H = 'Wgh.\Unt':10 :W=(laScrMode[3].OR.laScrMode[4]) .AND. lfEmpCtn() :V=lfvWeight(),"+;
                 "nCarton =IIF(lRange AND laScrMode[4],nRngCart, nCarton):H=  'Carton#':8 :R"
    ENDIF
    *B125190,1 NNA (END)

  ENDIF             
  *C038675,1 [End]           
  *C122070,1 [End]
  BROWSE FIELDS &lcBrFlds;
           FOR IIF(llShwOpn,AvlQty>0,.T.);
           SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
           TITLE(lcDtlBrTtl) WHEN lfwDtlBrow() ;
           VALID :F lfvDtlBrow()    ;
           WINDOW (lcWinDtl1) IN WINDOW (lcWinDtl)           

ELSE
  
  SELECT(lcSumPck)
  lnSmBrRec = RECNO()
*  lcBrFlds =  "cMarker =IIF(RECNO(lcSumPck)=lnSmBrRec ,'>',' '):H= ' ':R:1:W=.F.,"+;
              "Selector=EVAL('cSelect') :H = ' ':R:1:W=.F.,"+;
              "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :30:R,"+;
              "OTotQty   :H = 'O.Qty.':6 :R,"+;
              "PkCtnQty  :H = 'Qty.\Ctn(Pks)':13 :V=lfvQtyCtn() :W=(laScrMode[3].OR.laScrMode[4]).AND.llPack .AND. EMPTY(nCarton) .AND. lfOldValue(),"+;
              "CtnTotQty :H = 'Qty.\Ctn(Pcs)':13 :V=lfvQtyCtn() :W=(laScrMode[3].OR.laScrMode[4]).AND.!llPack .AND. EMPTY(nCarton) .AND. lfOldValue(),"+;
              "PTotWgh   :H = 'Wgh.\Unt':10 :V=lfvWeight() :W=!llPack .AND. EMPTY(nCarton) .AND. lfOldValue(),"+;
              "nCarton   :H=  'Carton#' :8  :R "
  *C122070,1 HBG 04/04/2004 Modify the browse fileds in summary mode to allow enabling and disabling fields in case of range pack[Begin]
  *lcBrFlds =  "cMarker =IIF(RECNO(lcSumPck)=lnSmBrRec ,'>',' '):H= ' ':R:1:W=.F.,"+;
              "Selector=EVAL('cSelect') :H = ' ':R:1:W=.F.,"+;
              "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :30:R,"+;
              "OTotQty   :H = 'O.Qty.':6 :R,"+;
              "PkCtnQty  :H = 'Qty.\Ctn(Pks)':13 :V=lfvQtyCtn() :W=(laScrMode[3].OR.laScrMode[4]).AND.llPack .AND. EMPTY(nCarton) .AND. lfOldValue(),"+;
              "CtnTotQty :H = 'Qty.\Ctn(Pcs)':13 :R,"+;
              "PTotWgh   :H = 'Wgh.\Unt':10 :V=lfvWeight() :W=(laScrMode[3].OR.laScrMode[4]).AND.!llPack .AND. EMPTY(nCarton) .AND. lfOldValue(),"+;
              "nCarton   :H=  'Carton#' :8  :R "
  *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
  *lcBrFlds =  "cMarker =IIF(RECNO(lcSumPck)=lnSmBrRec ,'>',' '):H= ' ':R:1:W=.F.,"+;
              "Selector=EVAL('cSelect') :H = ' ':R:1:W=.F.,"+;
              "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :30:R,"+;
              "cRange    :H = 'Range' :7 :R,"+;
              "OTotQty   :H = 'O.Qty.':6 :R,"+;
              "PkCtnQty  :H = 'Qty.\Ctn(Pks)':13 :V=lfvQtyCtn()  :W=(laScrMode[3].OR.laScrMode[4]).AND. llPack .AND. !lRange .AND. EMPTY(nCarton) .AND. lfOldValue(),"+;
              "CtnTotQty :H = 'Qty.\Ctn(Pcs)':13 :V=lfvRngQCtn() :W=(laScrMode[3].OR.laScrMode[4]).AND. llPack .AND. lRange .AND. EMPTY(nCarton),"+;
              "PTotWgh   :H = 'Wgh.\Unt':10 :V=lfvWeight() :W=(laScrMode[3].OR.laScrMode[4]).AND.!llPack .AND. EMPTY(nCarton) .AND. lfOldValue(),"+;
              "nCarton   :H=  'Carton#' :8  :R "
  IF llUseConfg
    lcBrFlds =  "cMarker =IIF(RECNO(lcSumPck)=lnSmBrRec ,'>',' '):H= ' ':R:1:W=.F.,"+;
                "Selector=EVAL('cSelect') :H = ' ':R:1:W=.F.,"+;
                "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :36:R,"+;
                "Dyelot    :H = 'Configuration' :15 :R,"+;
                "cRange    :H = 'Range' :7 :R,"+;
                "OTotQty   :H = 'O.Qty.':6 :R,"+;
                "PkCtnQty  :H = 'Qty.\Ctn(Pks)':13 :V=lfvQtyCtn()  :W=(laScrMode[3].OR.laScrMode[4]).AND. llPack .AND. !lRange .AND. EMPTY(nCarton) .AND. lfOldValue(),"+;
                "CtnTotQty :H = 'Qty.\Ctn(Pcs)':13 :V=lfvRngQCtn() :W=(laScrMode[3].OR.laScrMode[4]).AND. llPack .AND. lRange .AND. EMPTY(nCarton),"+;
                "PTotWgh   :H = 'Wgh.\Unt':10 :V=lfvWeight() :W=(laScrMode[3].OR.laScrMode[4]).AND.!llPack .AND. EMPTY(nCarton) .AND. lfOldValue(),"+;
                "nCarton   :H=  'Carton#' :8  :R "
  ELSE
    lcBrFlds =  "cMarker =IIF(RECNO(lcSumPck)=lnSmBrRec ,'>',' '):H= ' ':R:1:W=.F.,"+;
                "Selector=EVAL('cSelect') :H = ' ':R:1:W=.F.,"+;
                "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :36:R,"+;
                "cRange    :H = 'Range' :7 :R,"+;
                "OTotQty   :H = 'O.Qty.':6 :R,"+;
                "PkCtnQty  :H = 'Qty.\Ctn(Pks)':13 :V=lfvQtyCtn()  :W=(laScrMode[3].OR.laScrMode[4]).AND. llPack .AND. !lRange .AND. EMPTY(nCarton) .AND. lfOldValue(),"+;
                "CtnTotQty :H = 'Qty.\Ctn(Pcs)':13 :V=lfvRngQCtn() :W=(laScrMode[3].OR.laScrMode[4]).AND. llPack .AND. lRange .AND. EMPTY(nCarton),"+;
                "PTotWgh   :H = 'Wgh.\Unt':10 :V=lfvWeight() :W=(laScrMode[3].OR.laScrMode[4]).AND.!llPack .AND. EMPTY(nCarton) .AND. lfOldValue(),"+;
                "nCarton   :H=  'Carton#' :8  :R "
  ENDIF
  *C038675,1 [End]
  *C122070,1 [End]                        

  BROWSE FIELDS &lcBrFlds ;
           FOR IIF(llShwOpn,AvlQty>0,.T.);
           SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
           TITLE(lcDtlBrTtl) WHEN lfwDtlBrow() ;
           VALID :F lfvDtlBrow()    ;
           WINDOW (lcWinDtl1) IN WINDOW (lcWinDtl)
ENDIF
= lfwDtlBrow()

*:**************************************************************************
*:* Name        : lfEmpCtn
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : When function for style weight field
*:***************************************************************************
FUNCTION lfEmpCtn
PRIVATE lcSty,laCnt
DIMENSION laCnt[1]
laCnt[1] = 0
lcSty = STYLE
IF !EMPTY(nCarton)
  RETURN .F.
ELSE
  SELECT SUM(NCARTON) ;
  FROM &lcTmpPck ;
  WHERE EMPTY(PACK_ID) AND STYLE = lcSty ;
  INTO ARRAY laCnt
  RETURN (laCnt[1]=0)
ENDIF
*-- end of lfEmpCtn.

*!*************************************************************
*! Name      : lfwDtlBrow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : adjust the label of pbsel button
*!*************************************************************

FUNCTION lfwDtlBrow
PRIVATE lnAlias,lcSelStat

lcSelStt = IIF((laScrMode[3] OR llNew) AND lcDtl2Stat = 'ENABLE','ENABLE','DISABLE')
lnAlias = SELECT(0)

IF !USED(lcTmpPck)
  USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPakIndxSt)
ENDIF

lcTemFile = IIF(lnOrdPk=2,lcSumPck,lcTmpPck)
SELECT (lcTemFile)
llShoPckDt = (lnOrdPk=1)
IF llShoPckDt
  lnDtBrRec = RECNO(lcTemFile)
ELSE
  lnSmBrRec = RECNO(lcSumPck)
ENDIF
IF lnActFolder = 1
  lnBrCtnQty = EVALUATE(lcTmpPck+'.CtnQty')
  IF llShoPckDt
    lnBrUntWgh = EVALUATE(lcTmpPck+'.StyWgh')
  ENDIF  
  =lfLclRfrsh()
  SHOW WINDOW (lcDtlBrTtl) REFRESH SAME
ENDIF
SELECT (lcTemFile)
lcSel  = IIF(EMPTY(cSelect),"This","UnSel")

SHOW GET pbSel,1 PROMPT lcSel &lcSelStt
=lfSelStatus()
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

lnCtnPal  = &lcCtnHdr..Pal_No
SHOW GET lnCtnPal 

=lfSlStyPckBt()

SELECT(lnAlias)
 
*!*************************************************************
*! Name      : lfvDtlBrow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() function
*!*************************************************************

FUNCTION lfvDtlBrow

IF WONTOP() # (lcDtlBrTtl)
  =gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfvCtnQty
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Validate quantity per carton
*!*************************************************************

FUNCTION lfvCtnQty

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
lcFldNam = 'CtnTotQty'
lcFldNam = CtnQty

IF !EMPTY(EVAL(lcTmpPck+'.cSelect'+&lcScaFile..SzCode)) AND lnBrCtnQty > 0


  SELECT (lcPckLin)
  =SEEK(EVAL(lcTmpPck+'.Pack_ID') +EVAL(lcTmpPck+'.cPkColor')+EVAL(lcTmpPck+'.cPckSize')+EVAL(lcTmpPck+'.cPkVersion')+ STR(EVAL(lcTmpPck+'.nOrdLineNo'),6) + EVAL(lcTmpPck+'.STYLE') )

  REPLACE &lcFldNam WITH lnBrCtnQty
  = RLOCK(lcPckLin) 
  UNLOCk IN (lcPckLin)
  SHOW WINDOW (lcDtlBrTtl) REFRESH SAME                
ELSE
  lnBrCtnQty = lcOldVal  
ENDIF  

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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Validate unit weight per carton
*!*************************************************************

FUNCTION lfvStyWgh

PRIVATE lnCurAlias,lnCurWgh,lcSelSty

lnCurAlias = SELECT(0)
IF !EMPTY(EVAL(lcTmpPck+'.cSelect'+&lcScaFile..SzCode)) AND lnBrUntWgh > 0
  IF llShoPckDt

    SELECT (lcPckLin)
    =SEEK(EVAL(lcTmpPck+'.Pack_ID') +EVAL(lcTmpPck+'.cPkColor') +EVAL(lcTmpPck+'.cPckSize') +EVAL(lcTmpPck+'.cPkVersion') + STR(EVAL(lcTmpPck+'.nOrdLineNo'),6) + EVAL(lcTmpPck+'.STYLE') )
    REPLACE StyWgh WITH lnBrUntWgh
    IF &lcSumPck..cSelect = '»'
      SELECT (lcPckLin)
      =SEEK(LEFT(EVAL(lcSumPck+'.Pack_ID'),16) +EVAL(lcSumPck+'.cPkColor') +EVAL(lcSumPck+'.cPckSize') +EVAL(lcSumPck+'.cPkVersion'))
      SCAN REST WHILE (PACK_ID+cPkColor+cPckSize+cPkVersion+STR(nOrdLineNo,6)+Style = LEFT(EVAL(lcSumPck+'.Pack_ID'),16) +EVAL(lcSumPck+'.cPkColor') +EVAL(lcSumPck+'.cPckSize') +EVAL(lcSumPck+'.cPkVersion'))
        REPLACE StyWgh            WITH lnBrUntWgh 
      ENDSCAN
      SELECT (lcSumPck)
      REPLACE PTotWgh WITH lnBrUntWgh * OTotQty
      SELECT (lcPckLin)
    ENDIF  

  ELSE
    SELECT (lcPckLin)
    
    =SEEK(LEFT(EVAL(lcSumPck+'.Pack_ID'),16) +EVAL(lcSumPck+'.cPkColor') +EVAL(lcSumPck+'.cPckSize') +EVAL(lcSumPck+'.cPkVersion'))
    
    SCAN REST WHILE (PACK_ID+cPkColor+cPckSize+cPkVersion+STR(nOrdLineNo,6)+Style = LEFT(EVAL(lcSumPck+'.Pack_ID'),16) +EVAL(lcSumPck+'.cPkColor') +EVAL(lcSumPck+'.cPckSize') +EVAL(lcSumPck+'.cPkVersion'))
      REPLACE StyWgh            WITH lnBrUntWgh 
    ENDSCAN  
    SELECT (lcSumPck)
    REPLACE PTotWgh WITH lnBrUntWgh * OTotQty
    SELECT (lcPckLin)
  ENDIF  
  = RLOCK(lcPckLin) 
  UNLOCk IN (lcPckLin)
  SHOW WINDOW (lcDtlBrTtl) REFRESH SAME                
ELSE
  lnBrUntWgh = lcOldVal    
ENDIF  
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Validate palette No.
*!*************************************************************

FUNCTION lfvCtnPal

SHOW GET lnCtnPal

*!*************************************************************
*! Name      : lfvPalNo
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Validate palette No for carton header edit rigion.
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
  UNLOCk IN (lcCtnHdr)
  lnMaxPal = MAX(lnMaxPal,&lcCtnHdr..Pal_No)
  lnMinPal = Min(IIF(lnMinPal=0,&lcCtnHdr..Pal_No,lnMinPal),&lcCtnHdr..Pal_No)
ENDIF

SELECT(lnCurAlias)  

*!*************************************************************
*! Name      : lfvApply   
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : validate pbApply button
*!*************************************************************
FUNCTION lfvApply

PRIVATE lnCurAlias,lnCurRec,lnJ,laCart,lcTag,llFound,lnRecNom
DIMENSION laCart[1]
STORE 0 To laCart
lnCurAlias = SELECT(0)
SELECT (lcTmpPck)
lnRecNom = RECNO()

SELECT (lcSumPck)
lnRecNo = RECNO()

SELECT (lcPckLin)
SET RELATION TO
lnCurRec = RECNO()
*lnSizeRec = VAL(&lcScaFile..SzCode) 

=lfUpdate()

lcTag = ORDER(lcPckLin)
SET ORDER TO Selected IN (lcPckLin)

*--Check that no selected line with Qty/Ctn or Wgh/Unt has zero value
SELECT &lcPckLin
IF SEEK('Y',lcPckLin)
  SCAN FOR Selected>0 AND cSelect = '»'
    *C122070,1 HBG 04/04/2004 Check the new field "DisCtnQty" instead of "CTNQTY" in case of range pack[Begin]
    *IF CTNQTY = 0 OR STYWGH = 0

    *B125190,1 NNA 01/16/2005 (BEGIN) IF this not a range Pack then check for field CTNQTY=0 else
    *B125190,1 NNA            check filed DisCtnQty 
    *IF DisCtnQty = 0 OR STYWGH = 0
    lcCheckStr =IIF(lRange,'DisCtnQty = 0 OR STYWGH = 0','CTNQTY = 0 OR STYWGH = 0')
    IF &lcCheckStr
    *B125190,1 NNA (END)

    *C122070,1 [End]
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Style '+Style+' has '+IIF(CTNQTY=0,'Qty/Ctn','Wgh/Unt')+' equals zero.')
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Style '+Style+'-'+Dyelot+' has '+IIF(CTNQTY=0,'Qty/Ctn','Wgh/Unt')+' equals zero.')
      *C038675,1 [End]
      GOTO RECNO(lcPckLin) IN &lcTmpPck
      RETURN
    ENDIF
    *C122070,1 HBG 04/04/2004 No need for this check in case of range pack[Begin]
    IF !lRange
    *C122070,1 [End]
      IF MOD(OQTY,CTNQTY) # 0
        *-- Button : B44002
        *-- <Yes>;<Yes to All>;<No>
        lnRspn = gfModalGen('INM00000B44002',.F.,.F.,.F.,'The O.Qty. for '+STYLE+' is not divisible by Qty./Ctn. Continue?')
        IF lnRspn = 2
          EXIT
        ENDIF
        IF lnRspn = 3
          GOTO RECNO(lcPckLin) IN &lcTmpPck
          RETURN
        ENDIF
      ENDIF    
    *C122070,1 HBG 04/04/2004 End if case of range pack[Begin]
    ENDIF
    *C122070,1 [End]  
  ENDSCAN
ENDIF

SELECT &lcPckLin
SET ORDER To Selected

*--Increment Carton#
SELECT &lcPckLin
LOCATE FOR cSelect = '»'
llFound = FOUND()
IF llFound
  =lfNewCrt()
ELSE
  RETURN  
ENDIF
*B123477,4  TMI [Start] Define a test Array
DIMENSION laPckId[1]
laPckId[1] = ' '
lnPckLn = 0
*B123477,4  TMI [End  ] 

IF llFound
  SCAN FOR cSelect = '»'
    
    =SEEK(&lcPckLin..STYLE,'STYLE')
    =SEEK('S'+STYLE.SCALE,'SCALE')
    SELECT (lcSumPck)
    IF !EMPTY(&lcPckLin..PACK_ID+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion)
      =SEEK(PADR(&lcPckLin..PACK_ID,19)+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion)
    ELSE
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *=SEEK(PADR(&lcPckLin..Style,19)+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion)            
      =SEEK(PADR(&lcPckLin..Style,19)+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion+&lcPckLin..DYELOT)            
      *C038675,1 [End]
    ENDIF 
    IF &lcSumPck..cSelect = '»' 
      REPLACE &lcSumPck..PTotQty    WITH &lcSumPck..PTotQty + &lcPckLin..PQty 
      REPLACE cSelect WITH ' '
      REPLACE NCARTON WITH  lnCurrCtn 
    ENDIF          
    lnQuantities = &lcPckLin..CtnQty
    lnWeights    = &lcPckLin..StyWgh*&lcPckLin..CtnQty
    *B123477,4  TMI [Start] if a range pack is encountered first time , add qty , wght , otherwise , add zeros
    lcPckId = &lcPckLin..PACK_ID+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion+STR(lnCurrCtn)
    IF ASCAN(laPckId , lcPckId ) = 0
      lnPckLn = lnPckLn + 1
      DIMENSION laPckId[lnPckLn]
      laPckId[lnPckLn] = lcPckId
    ELSE
      IF &lcPckLin..LRANGE
        lnQuantities = 0
        lnWeights    = 0
      ENDIF
    ENDIF
    *B123477,4  TMI [End  ] 
    IF lnQuantities > 0
      IF SEEK(STR(lnCurrCtn,4),lcCtnHdr)
        SELECT (lcCtnHdr)
        REPLACE TotPcs WITH TotPcs + lnQuantities,;
                TotWgh WITH TotWgh + lnWeights
        lnPackQty = lnPackQty + lnQuantities
        lnPackWgh = lnPackWgh + lnWeights
        SELECT(lcPckLin)
      ELSE
        INSERT INTO (lcCtnHdr)(Cart_No   ,Pal_No  ,TotPcs      ,TotWgh   ,Empty);
                       VALUES (lnCurrCtn,lnCtnPal,lnQuantities,lnWeights,'N'  )
        lnPackWgh = lnPackWgh + &lcCtnHdr..TotWgh
        lnPackCtn = lnPackCtn + 1
        lnPackQty = lnPackQty + &lcCtnHdr..TotPcs
      ENDIF
      lnMaxPal  = MAX(lnMaxPal,&lcCtnHdr..Pal_No)    
      lnMinPal  = Min(IIF(lnMinPal=0,&lcCtnHdr..Pal_No,lnMinPal),&lcCtnHdr..Pal_No)
    ENDIF
    
    SELECT (lcPckLin)    
    *C122070,1 HBG 04/04/2004 Ignore lines with 0 Packed qty in case of range pack[Begin]
    IF CTNQTY <> 0
    *C122070,1 [End]
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *IF !SEEK(STR(lnCurrCtn,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,lcCtnDtl)  
      IF !SEEK(STR(lnCurrCtn,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style+Dyelot,lcCtnDtl)  
      *C038675,1 [End]
        =SEEK(&lcPckLin..Style,'STYLE')
        =SEEK('S'+STYLE.SCALE,'SCALE')
        SELECT &lcCtnDtl
        APPEND BLANK
        REPLACE PACK_ID     WITH &lcPckLin..PACK_ID     ,;
                CPKCOLOR    WITH &lcPckLin..CPKCOLOR    ,;
                CPCKSIZE    WITH &lcPckLin..CPCKSIZE    ,;
                CPKVERSION  WITH &lcPckLin..CPKVERSION  ,;
                Style       WITH &lcPckLin..Style       ,;
                OrgWgh      WITH &lcPckLin..OrgStyWgh   ,;
                SzCnt       WITH SCALE.CNT,;
                cStatus     WITH "A",;
                Cart_No     WITH lnCurrCtn
        *C038675,1 HBG 10/26/2004 Update Configuration field [Begin]
        REPLACE Dyelot WITH &lcPckLin..Dyelot
        *C038675,1 [End]
        *C122070,1 HBG 04/04/2004 calculate the weight in case of range pack[Begin]
        REPLACE lFlag WITH .T.
        IF &lcPckLin..lRange
          lcSz = &lcPckLin..cSizeNo
          REPLACE Weight&lcSz WITH &lcPckLin..StyWgh*&lcSumPck..CtnTotQty,;
                  TotWeight   WITH TotWeight + Weight&lcSz

        ENDIF 
        *C122070,1 [End]
      ENDIF
      SELECT &lcCtnDtl
      lcSz = &lcPckLin..cSizeNo
      *C122070,1 HBG 04/04/2004 update the weight in case of non range pack only[Begin]
      IF &lcPckLin..lRange
       REPLACE Size&lcSz   WITH IIF(!EMPTY(&lcPckLin..cSelect) AND &lcPckLin..CtnQty>0,&lcPckLin..cSize,Size&lcSz) ,;
                Br&lcSz     WITH !EMPTY(Size&lcSz),;
                Qty&lcSz    WITH &lcPckLin..CtnQty,;
                cStatus     WITH IIF(&lcPckLin..Selected>0,'M',cStatus)      
      ELSE
        REPLACE Size&lcSz   WITH IIF(!EMPTY(&lcPckLin..cSelect) AND &lcPckLin..CtnQty>0,&lcPckLin..cSize,Size&lcSz) ,;
                Br&lcSz     WITH !EMPTY(Size&lcSz),;
                Qty&lcSz    WITH &lcPckLin..CtnQty,;
                Weight&lcSz WITH &lcPckLin..StyWgh*&lcPckLin..CtnQty,;
                TotWeight   WITH TotWeight + Weight&lcSz ,;
                cStatus     WITH IIF(&lcPckLin..Selected>0,'M',cStatus)
      ENDIF         
    *-- End if Packed qty = 0 
    ELSE          
      IF &lcPckLin..lRange
        *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
        *IF !SEEK(STR(lnCurrCtn,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,lcCtnDtl)  
        IF !SEEK(STR(lnCurrCtn,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style+Dyelot,lcCtnDtl)  
        *C038675,1 [End]
          =SEEK(&lcPckLin..Style,'STYLE')
          =SEEK('S'+STYLE.SCALE,'SCALE')
          SELECT &lcCtnDtl
          lcSz = &lcPckLin..cSizeNo
          APPEND BLANK
          REPLACE PACK_ID     WITH &lcPckLin..PACK_ID     ,;
                 CPKCOLOR    WITH &lcPckLin..CPKCOLOR    ,;
                 CPCKSIZE    WITH &lcPckLin..CPCKSIZE    ,;
                 CPKVERSION  WITH &lcPckLin..CPKVERSION  ,;
                 Style       WITH &lcPckLin..Style       ,;
                 OrgWgh      WITH &lcPckLin..OrgStyWgh   ,;
                 SzCnt       WITH SCALE.CNT,;
                 cStatus     WITH "A",;
                 Cart_No     WITH lnCurrCtn,;
                 Weight&lcSz WITH &lcPckLin..StyWgh*&lcSumPck..CtnTotQty,;
                 TotWeight   WITH TotWeight + Weight&lcSz 
          *C038675,1 HBG 10/26/2004 Update Configuration field [Begin]
          REPLACE Dyelot WITH &lcPckLin..Dyelot
          *C038675,1 [End]       
        ENDIF
        SELECT &lcCtnDtl
        lcSz = &lcPckLin..cSizeNo
        REPLACE Size&lcSz   WITH IIF(!EMPTY(&lcPckLin..cSelect) AND &lcPckLin..CtnQty>0,&lcPckLin..cSize,Size&lcSz) ,;
                Br&lcSz     WITH !EMPTY(Size&lcSz),;
                cStatus     WITH IIF(&lcPckLin..Selected>0,'M',cStatus),;
                lFlag       WITH IIF(lFlag,.T.,.F.)
      ENDIF    
    ENDIF
    *C122070,1 [End]
    SELECT(lcPckLin)
    REPLACE cSelect WITH ' '
    REPLACE NCARTON WITH lnCurrCtn   
    *C122070,1 HBG 04/04/2004 Update the new field "NRNGCART" with the carton # in case of range pack[Begin]
    IF lRange
      REPLACE NRNGCART WITH lnCurrCtn   
    ENDIF  
    *C122070,1 [End] 
  ENDSCAN
ENDIF
SET ORDER TO lcTag IN (lcPckLin)

SELECT(lcPckLin)

=RLOCK(lcPckLin)
UNLOCK IN (lcPckLin)

llApply = .T.

SELECT (lcSumPck)
IF BETWEEN(lnRecNo,1,RECCOUNT(lcSumPck))
  GOTO lnRecNo
ENDIF

SELECT(lcPckLin)
IF lnCurRec <= RECCOUNT()
  GOTO lnCurRec
*  SKIP lnSizeRec-1  
ENDIF

SELECT (lcCtnHdr)
GOTO TOP
= RLOCK(lcCtnHdr) 
UNLOCk IN (lcCtnHdr)

SELECT (lcCtnDtl)
GOTO TOP
= RLOCK(lcCtnDtl) 
UNLOCk IN (lcCtnDtl)

*-- Refresh detail window browse
SHOW WINDOW (lcDtlBrTtl) REFRESH SAME

SELECT (lcTmpPck)
IF BETWEEN(lnRecNom ,1,RECCOUNT(lcTmpPck))
  GOTO lnRecNom 
ENDIF

SELECT (lnCurAlias)
*-- END OF lfvApply

*:**************************************************************************
*:* Name        : lfNewCrt
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Check if there is gaps between cartons and get current carton #
*:***************************************************************************
FUNCTION lfNewCrt
PRIVATE lnI,laCtns
DIME laCtns[1]
laCtns = 0

SELECT DIST Cart_No FROM &lcCtnHdr WHERE !DELETED() INTO ARRAY  laCtns
FOR lnI = 1 TO lnCrtnSeq
  IF ASCAN(laCtns,lnI)=0
    lnCurrCtn = lnI
    RETURN 
  ENDIF
ENDFOR

lnCrtnSeq = lnCrtnSeq + 1
lnCurrCtn = lnCrtnSeq


*-- end of lfNewCrt.
*!*************************************************************
*! Name      : lfvSel
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : validate selection buttons pbsel,pbselAll,
*!             ,pbInvert buttons
*!*************************************************************
              
FUNCTION lfvSel 
PARAMETERS lnBtn

PRIVATE lnRecno,lnRecno2,lcUnSelSty,lcSelFld,lcCtnQtyFld,lnSizeRec,lcStyMjr,lcSeek,;
        lcTmPckOrd
        
PRIVATE lcSvOrd
lcSvOrd = ORDER('ORDLINE')
lcTmPckOrd = ORDER(lcTmpPck)

SET ORDER TO ORDLINE IN ORDLINE
SET ORDER TO &lcPakIndxSt IN (lcTmpPck)    && "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo"   

lnCurAlias = SELECT(0)
IF ALIAS() = lcSumPck
  IF llPack
    =SEEK(PADR(PACK_ID,16)+cPkColor+cPckSize+cPkVersion,lcTmpPck)
  ELSE
    =SEEK(PADR('',29)+PACK_ID,lcTmpPck)
  ENDIF
ENDIF

SELECT (lcTmpPck)
lnRecno  = RECNO(lcTmpPck)
lnRecno2 = RECNO(lcSumPck)

*lnSizeRec = VAL(&lcTmpPck..cSizeNo)
lcSelFld    = &lcTmpPck..cSelect
lcCtnQtyFld = &lcTmpPck..CtnQty
lcOQtyFld   = &lcTmpPck..OQty

lcAllStat = "DISABLE"
lcNonStat = "DISABLE"

llcUpate = .T.
SET RELATION TO
=lfUpdate()

DO CASE
  CASE IIF(EMPTY(lnBtn), _CUROBJ = OBJNUM(pbSelAll) , lnBtn = 'ALL' )
    SELECT (lcSumPck)
    LOCATE
    SCAN  
      lcPack_Id = IIF(&lcSumPck..llPack , LEFT(&lcSumPck..Pack_Id,16),SPACE(16)) + ;
                  &lcSumPck..cpkcolor + &lcSumPck..cpcksize + &lcSumPck..cPkVersion +;
                  IIF(&lcSumPck..llPack,'',&lcSumPck..Pack_Id) 
      SET ORDER TO TAG (lcPakIndxSt) IN (lcTmpPck)
 
      SELECT (lcTmpPck)
      LOCATE
      =SEEK(lcPack_Id)
      SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcPack_Id ;
                FOR nCarton = 0
        REPLACE cSelect  WITH IIF(AvlQty>0 , '»' , ' ') ,;
                Selected WITH IIF(cSelect='»' , 1 , 0 )

        SELECT (lcSumPck)
        *-- Update as lcTmpPck
        REPLACE &lcSumPck..cSelect   WITH IIF(&lcTmpPck..cSelect = '»','»',' ')
      ENDSCAN  
    ENDSCAN

    
  CASE IIF(EMPTY(lnBtn), _CUROBJ = OBJNUM(pbSelNo) , lnBtn = 'NONE' )
    SELECT (lcTmpPck)
    REPLACE ALL cSelect WITH '',;
                Selected WITH 0

    SELECT(lcSumPck)
    REPLACE ALL  &lcSumPck..cSelect    WITH ' '

  CASE IIF(EMPTY(lnBtn), _CUROBJ = OBJNUM(pbSelSty) , lnBtn = 'STY' )
    IF !EMPTY(PACK_ID) 
    
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This style is included in a pack , can not be selected individually.')

    ELSE    
      
      lcStyMjr = SUBSTR(&lcTmpPck..STYLE,1,lnMajLen)
      SET ORDER TO TAG (lcPakIndxSt) IN (lcTmpPck)
      GO TOP      
      lcKey = PADR('',16+6+3+4)+lcStyMjr
      =SEEK(lcKey)
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *SCAN REST WHILE Pack_Id + cpkcolor + cpcksize + cPkVersion + Style  + cSizeNo = lcKey ;
                FOR nCarton = 0
      SCAN REST WHILE Pack_Id + cpkcolor + cpcksize + cPkVersion + Style  + Dyelot + cSizeNo = lcKey ;
                FOR nCarton = 0
      *C038675,1 [End]            
        REPLACE cSelect  WITH IIF(AvlQty>0,IIF(cSelect='»','','»'),''),;
                Selected WITH IIF(cSelect='»', 1,0)
        *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
        *IF SEEK(STYLE + cpkcolor + cpcksize + cPkVersion ,lcSumPck) 
        IF SEEK(STYLE + cpkcolor + cpcksize + cPkVersion + Dyelot ,lcSumPck) 
        *C038675,1 [End]
          SELECT &lcSumPck
          REPLACE &lcSumPck..cSelect WITH &lcTmpPck..cSelect
        ENDIF
      ENDSCAN
    ENDIF

  CASE IIF(EMPTY(lnBtn), _CUROBJ = OBJNUM(pbSelPack) , lnBtn = 'PACK' )

    lcPack_Id  = PADR(Pack_Id,16) + cpkcolor + cpcksize + cPkVersion 
    lcPack_id2 = PADR(Pack_Id,19) + cpkcolor + cpcksize + cPkVersion 
    IF !EMPTY(lcPack_Id)
    
      LOCATE 
      =SEEK(lcPack_Id)
      *C122070,1 HBG 04/04/2004 Select only packed lines to handle case of Range packs [Begin]
      *SCAN REST WHILE lcPack_Id = Pack_Id + cpkcolor + cpcksize + cPkVersion ;
                FOR   nCarton = 0
      SCAN REST WHILE lcPack_Id = Pack_Id + cpkcolor + cpcksize + cPkVersion ;
                FOR   nCarton = 0 AND !lIgnor
      *C122070,1 [End]                 
        REPLACE cSelect  WITH IIF(AvlQty>0,IIF(cSelect='»','','»'),''),;
                Selected WITH IIF(cSelect='»',1,0)
      ENDSCAN
      IF SEEK(lcPack_id2,lcSumPck)
        =SEEK(lcPack_id,lcTmpPck)
        SELECT (lcSumPck)
        REPLACE cSelect  WITH &lcTmpPck..cSelect
      ENDIF
    ELSE
      IF !EMPTY(&lcTmpPck..NCARTON)
        *--Message to tell user it is not belonge to pack
        =gfModalGen("INM00000B42001","Dialog",.F.,.F.,;
                    "This style did not belong to pack.")
      ENDIF                  
    
    ENDIF  
ENDCASE

SELECT(lcTmpPck)
= RLOCK(lcTmpPck)
UNLOCk IN (lcTmpPck)


IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpPck))
  GOTO (lnRecno) IN &lcTmpPck
ENDIF
IF BETWEEN(lnRecno2,1,RECCOUNT(lcSumPck))
  GOTO (lnRecno2) IN &lcSumPck
ENDIF

SET ORDER TO &lcSvOrd IN ORDLINE
SET ORDER TO &lcTmPckOrd IN (lcTmpPck)
=lfwDtlBrow()

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lpClrSel
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : validate BAR 1 of PAD _OPTIONS OF _MSYSMENU
*!*************************************************************

PROCEDURE lpClrSel

*-- this means if it is not clear selection upon apply invers this to be
*-- cleared upon apply and vice vresa is coorect

IF !llClrSel
  llClrSel = .T.
ELSE
  llClrSel = .F.
ENDIF

*!*************************************************************
*! Name      : lpShwOpn
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : validate BAR 2 of PAD _OPTIONS OF _MSYSMENU
*!*************************************************************

PROCEDURE lpShwOpn

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
*-- this to show only opened quantity or all quantity according to
*-- variable llShwOpn
llShwOpn = !llShwOpn
SET MARK OF BAR 1 OF _OPTPOP TO llShwOpn
=lfDtlBrow()

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfSelStatus
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : adjust the status of selection buttons
*!*************************************************************

FUNCTION lfSelStatus
PRIVATE lnCurAlias,lnCurRec

lnCurAlias = SELECT(0)

IF !USED(lcTmpPck)
  USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPakIndxSt)
ENDIF

lcTemFile = IIF(lnOrdPk=1,lcTmpPck,lcSumPck)
SELECT (lcTemFile)

lnCurRec = RECNO()
IF !llAnyRec
  lcDtl2Stat = "DISABLE"
ELSE
  lcDtl2Stat = IIF((laScrMode[3] OR llNew) AND lnActFolder = 1,"ENABLE","DISABLE")
ENDIF  

SHOW GETS WINDOW (lcWinDtl2) &lcDtl2Stat ONLY

IF lnActFolder = 1
  SHOW GET ibPckLin ENABLE
ENDIF  

lcSelStat = IIF( laScrMode[1] .OR. laScrMode[2] , 'DISABLE' , 'ENABLE' )
SHOW GET pbSel       &lcSelStat 
SHOW GET pbSelAll    &lcSelStat 
SHOW GET pbSelNo     &lcSelStat 
SHOW GET lnBrCtnQty  &lcSelStat 
SHOW GET lnBrUntWgh  &lcSelStat 
SHOW GET llLockOnErr &lcSelStat  
SHOW GET lnOrdPk     &lcSelStat 

SELECT &lcPckLin  && *--  Count number of selected lines if > 0 then enable the button
PRIVATE lnSvRec
lnSvRec = RECNO(lcPckLin)
GO TOP
lnCntSlct = 0
COUNT FOR cSelect = '»' TO lnCntSlct
lcAppStat = IIF(lnCntSlct>0,'ENABLE','DISABLE')
IF BETWEEN(lnSvRec,1,RECCOUNT(lcPckLin))
  GOTO (lnSvRec)
ENDIF

SHOW GET pbApply &lcAppStat

SELECT(lnCurAlias)

=lfSlStyPckBt()

*:**************************************************************************
*:* Name        : lfSlStyPckBt
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Select style/pack button status
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSlStyPckBt()
*:***************************************************************************
FUNCTION lfSlStyPckBt

PRIVATE lnAlias,lcStyStat,lcPckStat
lnAlias = SELECT()
STORE 'DISABLE' TO lcStyStat,lcPckStat
IF laScrMode[3] .OR. laScrMode[4]
  IF ALIAS() = lcTmpPck .OR. ALIAS() = lcPckLin
    SELECT &lcTmpPck
    lcStyStat = IIF(EMPTY(PACK_ID),'ENABLE','DISABLE')
    lcPckStat = IIF(!EMPTY(PACK_ID),'ENABLE','DISABLE')
  ELSE
    SELECT &lcSumPck
    lcStyStat = IIF(!llPack,'ENABLE','DISABLE')
    lcPckStat = IIF(llPack,'ENABLE','DISABLE')
  ENDIF
ENDIF
SHOW GET pbSelSty &lcStyStat
SHOW GET pbSelPack &lcPckStat 
SHOW GET lnOrdPk ENABLE

SELECT (lnAlias)
*-- end of lfSlStyPckBt.

*!*************************************************************
*! Name      : lpSavscr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : To make local save.
*!*************************************************************

PROCEDURE lpSavscr

*-- lnBookQty,lnBookAmt these variables are to update book,bookamt fields in 
*-- ordhdr file
PRIVATE llReturn,lnCurAlias,lcPckHdTag,lnCount,lnBookQty,lnBookAmt,;
        lnOpenQty,lnOpenAmt,llStyDyOpn,lnCurRecNo

llStyDyOpn = .F.

STORE 0 To lnBookQty,lnBookAmt,lnRelCho,lnOpenQty,lnOpenAmt
lnCount = 0
lnCurAlias = SELECT(0)

llNoThing = lfUpdVars()  
*-* llNoThing = lfUpdUnCmS("Open", VARREAD() )

lcDelStat = SET("DELETED")

PRIVATE lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif , lnOrgBook , lnOrgQty
STORE 0 TO lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif, lnOrgBook , lnOrgQty

SELECT (lcPcklin)
SET FILTER TO

IF !lfGtNtAply()
  llCSave = .F.
  RETURN .F.
ENDIF

SELECT (lcCtnDtl)
lcCtDtRel = SET("RELATION")
SET RELATION TO
SET FILTER TO
GOTO TOP

IF EOF() OR BOF() OR DELETED()
  *--No Packing list lines were applied. Cannot proceed!
  *-- <OK>
  llNoThing = gfModalGen("INM00000B00000","Dialog",.F.,.F.,'No lines applied, cannot proceed.')  
  llCSave = .F.
  RETURN .F.

ELSE
  
  *===============this part for updating TMPL_HDR file=========
  lnLastCtn = lnCurrCtn
  SELECT TMPL_HDR
  IF !SEEK(laData[2],'TMPL_HDR')
    INSERT INTO TMPL_HDR (ORDER) VALUES (laData[2])
  ENDIF  
  lnLastNo = TMPL_HDR.nLastLNo    

  *-- Get cartons information
  DIMENSION laCtnHdrSm[1,3]
  laCtnHdrSm = 0
  SELECT COUNT(CART_NO),SUM(TOTPCS),SUM(TOTWGH) ;
  FROM &lcCtnHdr ;
  INTO ARRAY laCtnHdrSm  
  
  laData[08] = laCtnHdrSm[1,3]
  laData[09] = laCtnHdrSm[1,1]
  laData[10] = laCtnHdrSm[1,2]
  
  laData[13] = IIF(lnCtnTyp = 1,.T.,.F.)
  laData[14] = IIF(lnDrctTo = 1,'S','C')

  *--Update user field LLCKONERR
  IF ASCAN(laUsrFields,'LLCKONERR')>0
    laUsrFields[ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'LLCKONERR'),1),6] = llLockOnErr
  ENDIF

  SELECT TMPL_HDR
  GATHER FROM laData FIELDS &lcScFields
  REPLACE LLCKONERR WITH llLockOnErr
  REPLACE cWareCode WITH lcWareCode,;
          cAdd_user WITH gcUser_Id,;
          dAdd_date WITH gdSysDate,;
          cAdd_time WITH gfGettime()

ENDIF
  *=================end of updating TMPL_HDR File================
 
  *=================This part for saving TMPL_LIN file================
  
  *--Remove all lines relating to order laData[2] in TMPL_LIN file
  *- Note  : after a long time these deleted records may cause the file to be large , it is suggested to pack this file periodically
  IF SEEK(laData[2],'TMPL_LIN') 
    SELECT TMPL_LIN
    *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
    *DELETE REST WHILE ORDER+STR(NCARTON,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = laData[2]      
    DELETE REST WHILE ORDER+STR(NCARTON,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+DYELOT = laData[2]      
    *C038675,1 [End]
  ENDIF
  
  SELECT(lcCtnDtl)
  SET FILTER TO 
  SET ORDER TO 0
    
  set relation to 
    
  = SEEK('O'+laData[2],'OrdHdr')
  *C122070,1 HBG 04/04/2004 Set the order of ordline to check if this pack is range or not[Begin]
  SET ORDER TO Ordblkst IN ORDLINE
  *C122070,1 [End] 
  SCAN FOR cStatus <> 'S'
    IF &lcCtnDtl..nStep = 0   && for uncomplete session
      SELECT &lcCtnDtl
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *IF !SEEK(laData[2]+STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,'TMPL_LIN')              
      IF !SEEK(laData[2]+STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style+Dyelot,'TMPL_LIN')              
      *C038675,1 [End]
        SELECT TMPL_LIN
        APPEND BLANK 
        REPLACE ORDER      WITH laData[2],;
                NCARTON    WITH &lcCtnDtl..Cart_No,;
                PACK_ID    WITH &lcCtnDtl..PACK_ID    ,;
                CPKCOLOR   WITH &lcCtnDtl..CPKCOLOR   ,;
                CPCKSIZE   WITH &lcCtnDtl..CPCKSIZE   ,;
                CPKVERSION WITH &lcCtnDtl..CPKVERSION ,;
                Style      WITH &lcCtnDtl..Style
        *C038675,1 HBG 10/26/2004 Update Configuration field [Begin]
        REPLACE DYELOT WITH &lcCtnDtl..Dyelot
        *C038675,1 [End]        
        REPLACE cAdd_user  WITH gcUser_Id,;
                dAdd_date  WITH gdSysDate,;
                cAdd_time  WITH gfGettime()
      ENDIF    
      SELECT TMPL_LIN      
      REPLACE Qty1       WITH &lcCtnDtl..Qty1,;
              Qty2       WITH &lcCtnDtl..Qty2,;
              Qty3       WITH &lcCtnDtl..Qty3,;
              Qty4       WITH &lcCtnDtl..Qty4,;
              Qty5       WITH &lcCtnDtl..Qty5,;
              Qty6       WITH &lcCtnDtl..Qty6,;
              Qty7       WITH &lcCtnDtl..Qty7,;
              Qty8       WITH &lcCtnDtl..Qty8,;
              TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8      
      *C122070,1 HBG 04/04/2004 If this pack is range update the weight in diffrent way[Begin]
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *IF SEEK('O'+laData[2]+&lcCtnDtl..Style+&lcCtnDtl..PACK_ID+&lcCtnDtl..CPKCOLOR+;
              &lcCtnDtl..CPCKSIZE+&lcCtnDtl..CPKVERSION,'ORDLINE') AND ORDLINE.lRange 
      IF SEEK('O'+laData[2]+&lcCtnDtl..Style+&lcCtnDtl..PACK_ID+&lcCtnDtl..CPKCOLOR+;
              &lcCtnDtl..CPCKSIZE+&lcCtnDtl..CPKVERSION,'ORDLINE') 
        SELECT ORDLINE
        LOCATE REST WHILE cordtype+order+style+pack_id+cpkcolor+cpcksize+cpkversion+store =;
            'O'+laData[2]+&lcCtnDtl..Style+&lcCtnDtl..PACK_ID+&lcCtnDtl..CPKCOLOR+&lcCtnDtl..CPCKSIZE+&lcCtnDtl..CPKVERSION FOR ;
            DYELOT = &lcCtnDtl..Dyelot
        IF FOUND() AND ORDLINE.lRange    
          SELECT TMPL_LIN
      *C038675,1 [End]  
          *B123477,1  TMI [Start] Seek with the correct expression from the lcCtnDtl file
          *=SEEK(PADR(&lcPckLin..PACK_ID,19)+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion,lcSumPck)
          *REPLACE NPWGHT WITH &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+ ;
          *                    &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+&lcCtnDtl..Weight7+&lcCtnDtl..Weight8 ,;
          *        WEIGHT WITH NPWGHT/&lcSumPck..CtnTotQty      
          =SEEK(PADR(&lcCtnDtl..PACK_ID,19)+&lcCtnDtl..cPkColor+&lcCtnDtl..cPckSize+&lcCtnDtl..cPkVersion,lcSumPck)
          REPLACE NPWGHT WITH &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+ ;
                              &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+&lcCtnDtl..Weight7+&lcCtnDtl..Weight8 ,;
                  WEIGHT WITH NPWGHT/&lcSumPck..CtnTotQty      
          *B123477,1  TMI [End  ] 
        ELSE        
          *C122070,1 [End]
          SELECT TMPL_LIN
          IF TOTQTY>0
            REPLACE NPWGHT WITH &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+ ;
                                &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+&lcCtnDtl..Weight7+&lcCtnDtl..Weight8 ,;
                    WEIGHT WITH NPWGHT/TOTQTY
          ENDIF
        *C122070,1 HBG 04/04/2004 End If this pack is range[Begin]  
        *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
        ENDIF  
        *C038675,1 [End]
      ENDIF
      *C122070,1 [End]
      SELECT (lcCtnDtl)
      REPLACE &lcCtnDtl..nStep WITH 1
    ENDIF
  ENDSCAN

  SELECT TMPL_LIN
  lcSetDel = SET('DELETE')
  SET DELETE ON
  =SEEK(laData[2])
  lnI = 0
  llFirst  = .T.
  lnLastCrt = TMPL_LIN.No_Cart
  llSameCrt = .F.
  SCAN REST WHILE ORDER+STR(No_Cart,4)+Style = laData[2]
    IF !llFirst
      llSameCrt = (TMPL_LIN.No_Cart = lnLastCrt)
      IF !llSameCrt 
        lnLastCrt = TMPL_LIN.No_Cart
      ENDIF
    ENDIF
    IF llFirst OR !llSameCrt 
      lnI = lnI + 1
    ENDIF
    REPLACE TMPL_LIN.No_Cart WITH lnI
    llFirst  = .F.
  ENDSCAN

  lnLastCtn = lnI

  SELECT (lcCtnDtl)
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
    REPLACE &lcCtnDtl..Cart_No WITH lnI
    llFirst  = .F.
  ENDSCAN
  SET DELETE &lcSetDel
    
  SELECT TMPL_HDR
  REPLACE nLastLno  WITH MAX(nLastLno,lnLastNo) ,;
          nLastCart WITH lnLastCtn
  *=================end saving TMPL_LIN file======================  
  
  
  *------------- Update field STYLE.NSTYWEIGHT if it is zero --------------*
  SELECT &lcPckLin
  LOCATE 
  SCAN
    IF EMPTY(&lcPckLin..PACK_ID) .AND. SEEK(&lcPckLin..STYLE,'STYLE') .AND. STYLE.NSTYWEIGHT = 0
      SELECT STYLE
      REPLACE STYLE.NSTYWEIGHT WITH &lcPckLin..StyWgh 
    ENDIF    
  ENDSCAN  
  
SET DELETED &lcDelStat

IF lnRelCho <> 2
  llReturn = .T.
ELSE
  llReturn = .F.
ENDIF

SELECT(lcCtnDtl)
SET RELATION TO &lcCtDtRel.

*-* llNoThing = lfUpdUnCmS("Comp", SPACE(0))

*-- ORDHDR file locking
=lfOrdHdLck(.F.)

SELECT OrdLine
lcTag = ORDER()
lnCurRecNo = 0
SET ORDER TO TAG OrdLinSt
*IF SEEK('O'+laData[2]+laData[5],'OrdLine') 
IF SEEK('O'+laData[2],'OrdLine') 
  SELECT OrdLine
  lnCurRecNo = RECNO()
  SCAN REST WHILE cOrdType+Order+Store+Style+STR(lineno,6) = 'O'+laData[2]
    = gfObj_Lock(.F.)
  ENDSCAN
ENDIF
SELECT OrdLine
SET ORDER TO TAG lcTag
IF BETWEEN(lnCurRecNo,1,RECCOUNT('ORDLINE'))
  GOTO lnCurRecNo 
ENDIF  

SELECT(lnCurAlias)
llCSave = llReturn
*-- end of lpSavscr.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : To make local delete.
*!*************************************************************

PROCEDURE lpDelScr

PRIVATE lnCurAlias,lcHdrTag,lcLinTag,llShiped 

IF SEEK(laData[2],'TMPL_HDR')
  IF TMPL_HDR.Status = 'C'
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

lnCurAlias = SELECT(0)

IF !(laData[2]==lcPrvPack)
  lcPrvPack = laData[2]    
  = lfvSelOrdL()
ENDIF

lcHdrTag = ORDER('TMPL_HDR')
SET ORDER TO TMPL_HDR IN TMPL_HDR
IF SEEK(laData[2],'TMPL_HDR')
  SELECT TMPL_HDR
  BLANK
  DELETE 
ENDIF  
SET ORDER TO TAG (lcHdrTag) IN TMPL_HDR

lcLinTag = ORDER('TMPL_LIN')

IF SEEK(laData[2],'TMPL_LIN')
  SELECT TMPL_LIN
  SCAN FOR ORDER = laData[2]
    BLANK
    DELETE
  ENDSCAN
ENDIF  

SET ORDER TO TAG (lcLinTag) IN TMPL_LIN

=lfCrtUnCmp()

*-- Return to "SELECT" mode
laScrMode        = .F.
laScrMode[1]     = .T.

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPackCopy
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : use if the user wishes to copy data for new pack from 
*!             an existing one.
*!*************************************************************

FUNCTION lfPackCopy

llBrowse = .F.
lcWName    = "ICPCopy"
lcWSPTitl  = "Copy From another Pack Slip"

PUSH KEY                                
DO (gcScrDir + '\ALPLSTCP.SPR')    
POP KEY                                        

*!*************************************************************
*! Name      : lfvPackNo
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : validate the selected pack no to copy data from it
*!*************************************************************

FUNCTION lfvPackNo

lnCurAlias = SELECT(0)
lcCurTag   = ORDER('TMPL_HDR')

SELECT TMPL_HDR
SET ORDER TO TMPL_HDR
IF llBrowse OR !SEEK(lcPackNo ,'TMPL_HDR')
  llPckCopy = .T.
  llBrowse  = .F.
  lcPackNo  = IIF(lfPakNoBrw(),TMPL_HDR.Pack_No,SPACE(6))
  lcOrderNo = IIF(!EMPTY(lcPackNo),TMPL_HDR.Order,SPACE(6))
  lcStore   = IIF(!EMPTY(lcPackNo),TMPL_HDR.Store,SPACE(6))
  llPckCopy = .F.
ENDIF  

IF !EMPTY(lcPackNo)
  lcOrderNo = TMPL_HDR.Order
  lcStore   = TMPL_HDR.Store
  lnCtnTyp = IIF(TMPL_HDR.LStandCtn,1,2)
  lnDrctTo = IIF(TMPL_HDR.CToStorCn='C',2,1)
  *-- Fixing bug of getting lnFrom & lnTo intioaly by zero when we copy from packing list[Begin]
  lnFrom = 1
  lnTo   = 1
  *-- Fixing bug of getting lnFrom & lnTo intioaly by zero when we copy from packing list[End  ]
ENDIF

SET ORDER TO lcCurTag IN TMPL_HDR
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPackScrAct
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Trap the keys in screen ICPACKSCR
*!*************************************************************

FUNCTION lfPackScrAct

*-- This function called in activate snippet for screen packScr
*-- which use to copy data from another pack id

ON KEY LABEL ESCAPE DO lfPackScrEsc

*!*************************************************************
*! Name      : lfPackScrEsc
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Trap the Esacpe Key in screen IcpackScr
*!*************************************************************

FUNCTION lfPackScrEsc

ON KEY LABEL ESCAPE

_CUROBJ = OBJNUM(pbCancel)
KEYBOARD "{ENTER}" CLEAR PLAIN

*!*************************************************************
*! Name      : lfCtnHdrBr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Browse lcCtnHdr file 
*!*************************************************************

FUNCTION lfCtnHdrBr

PRIVATE lnCurAlias,lcFields

lnCurAlias = SELECT(0)

IF llEdiSys
  lcFields = "cMarker =IIF(RECNO()=lnCtHdBrRe,'>',' ')  :H=' ':R:1:W=.F.,"+;
             "Cart_No    :H = 'Cart.#'       :6 :R,"+;
             "TotPcs     :H = 'Tot.Pcs'      :8 :R,"+;
             "TotWgh     :H = 'Tot.Wgh'      :8 :R"
ELSE
  lcFields = "cMarker =IIF(RECNO()=lnCtHdBrRe,'>',' ')  :H=' ':R:1:W=.F.,"+;
             "Cart_No    :H = 'Cart.#'       :6 :R,"+;
             "TotPcs     :H = 'Tot. Pieces'  :11:R,"+;
             "TotWgh     :H = 'Tot. Weight'  :11:R"
ENDIF              

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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : refresh the edit rigion for carton header according to
*!             lcCtnHdr file
*!*************************************************************

FUNCTION lfwCtnHdrBr
PRIVATE lnAlias

lnCtHdBrRe = RECNO(lcCtnHdr)

IF lnActFolder = 2
  SHOW WINDOW (lcCtHdBrTt) REFRESH
ENDIF

lnAlias = SELECT(0)
SELECT (lcCtnHdr)
lnCtHdBrRe = RECNO()
lnTotCart  = RECCOUNT()
lnCartNo   = &lcCtnHdr..Cart_No
lnTotWgh   = &lcCtnHdr..TotWgh
lnPalNo    = &lcCtnHdr..Pal_No

IF (laScrMode[3] OR llNew) AND lnActFolder = 2
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
  ENDIF
ELSE
  STORE "DISABLE" TO lcRemHdrSt,lcCartNoSt,lcTotWghSt,lcPalNoSt
ENDIF

SHOW GET pbDtlNew &lcAddDtlSt
SHOW GET pbHdrRem &lcRemHdrSt
SHOW GET lnCartNo &lcCartNoSt
SHOW GET lnTotWgh &lcTotWghSt

 SHOW GET pbHdrNew DISABLE
 SHOW GET pbDtlNew DISABLE

SELECT (lcCtnDtl)
SET RELATION TO
*SET FILTER TO STR(Cart_No,4)+Style+STR(nOrdLineNo,6) = STR(&lcCtnHdr..Cart_No,4)
SET FILTER TO STR(Cart_No,4)+Style = STR(&lcCtnHdr..Cart_No,4)
GO TOP

SELECT (lcCtnDtl)
SET RELATION TO '' INTO (lcCartonSz) ADDITIVE
SET SKIP TO (lcCartonSz)


LOCATE FOR EVAL(lcCtnDtl+'.Br'+&lcCartonSz..SzCode)

IF lnActFolder = 2
  IF !WEXIST(lcCtDtBrTt) 
    =lfCtnDtlBr()
  ELSE
    = lfwCtnDtlBr()
  ENDIF  
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvCtnHdrBr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() function
*!*************************************************************

FUNCTION lfvCtnHdrBr

IF WONTOP() # (lcCtHdBrTt)
  =gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfCtnDtlBr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Browse lcCtnDtl file 
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
*C122070,1 HBG 04/04/2004 Display the first style only in case of range pack[Begin]
*BROWSE FIELDS cMarker =IIF(RECNO()=lnCtDtBrRe AND RECNO(lcCartonSz)=lnDumCtnR,'>',' '):H=' ':R:1:W=.F.,;
              StyCode=Style:H= lcStyTtl     :25:R,;
              Size   =EVAL('Size' +&lcCartonSz..SzCode) :H = 'Size':7 :R,;
              SzQty  =EVAL('Qty'+&lcCartonSz..SzCode)   :H = 'Qty.':6 :R,;                            
              SzWgh  =EVAL('Weight'+&lcCartonSz..SzCode):H = 'Wgh.':6 :R ;              
         FOR EVAL(lcCtnDtl+'.Br'+&lcCartonSz..SzCode) AND &lcCartonSz..SzCode <= STR(&lcCtnDtl..SzCnt,1);
         SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
         TITLE(lcCtDtBrTt) WHEN lfwCtnDtlBr() VALID :F lfvCtnDtlBr();
         WINDOW (lcWinCtn2) IN WINDOW (lcWinCtn)
*B123477,4  TMI [Start] allow to show all lines in carton details regardless of it is range or non range
*BROWSE FIELDS cMarker =IIF(RECNO()=lnCtDtBrRe AND RECNO(lcCartonSz)=lnDumCtnR,'>',' '):H=' ':R:1:W=.F.,;
*              StyCode=Style:H= lcStyTtl     :25:R,;
*              Size   =EVAL('Size' +&lcCartonSz..SzCode) :H = 'Size':7 :R,;
*              SzQty  =EVAL('Qty'+&lcCartonSz..SzCode)   :H = 'Qty.':6 :R,;                            
*              SzWgh  =EVAL('Weight'+&lcCartonSz..SzCode):H = 'Wgh.':6 :R ;              
*         FOR EVAL(lcCtnDtl+'.Br'+&lcCartonSz..SzCode) AND &lcCartonSz..SzCode <= STR(&lcCtnDtl..SzCnt,1) AND &lcCtnDtl..lFlag;
*         SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
*         TITLE(lcCtDtBrTt) WHEN lfwCtnDtlBr() VALID :F lfvCtnDtlBr();
*         WINDOW (lcWinCtn2) IN WINDOW (lcWinCtn)
*C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
*BROWSE FIELDS cMarker =IIF(RECNO()=lnCtDtBrRe AND RECNO(lcCartonSz)=lnDumCtnR,'>',' '):H=' ':R:1:W=.F.,;
              StyCode=Style:H= lcStyTtl     :25:R,;
              Size   =EVAL('Size' +&lcCartonSz..SzCode) :H = 'Size':7 :R,;
              SzQty  =EVAL('Qty'+&lcCartonSz..SzCode)   :H = 'Qty.':6 :R,;                            
              SzWgh  =EVAL('Weight'+&lcCartonSz..SzCode):H = 'Wgh.':6 :R ;              
         FOR EVAL(lcCtnDtl+'.Br'+&lcCartonSz..SzCode) AND &lcCartonSz..SzCode <= STR(&lcCtnDtl..SzCnt,1) ;
         SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
         TITLE(lcCtDtBrTt) WHEN lfwCtnDtlBr() VALID :F lfvCtnDtlBr();
         WINDOW (lcWinCtn2) IN WINDOW (lcWinCtn)
IF llUseConfg
  BROWSE FIELDS cMarker =IIF(RECNO()=lnCtDtBrRe AND RECNO(lcCartonSz)=lnDumCtnR,'>',' '):H=' ':R:1:W=.F.,;
                StyCode =Style:H= lcStyTtl     :25:R,;
                cConfig = Dyelot :H= 'Configuration'    :15:R,;
                Size   =EVAL('Size' +&lcCartonSz..SzCode) :H = 'Size':7 :R,;
                SzQty  =EVAL('Qty'+&lcCartonSz..SzCode)   :H = 'Qty.':6 :R,;                            
                SzWgh  =EVAL('Weight'+&lcCartonSz..SzCode):H = 'Wgh.':6 :R ;              
           FOR EVAL(lcCtnDtl+'.Br'+&lcCartonSz..SzCode) AND &lcCartonSz..SzCode <= STR(&lcCtnDtl..SzCnt,1) ;
           SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
           TITLE(lcCtDtBrTt) WHEN lfwCtnDtlBr() VALID :F lfvCtnDtlBr();
           WINDOW (lcWinCtn2) IN WINDOW (lcWinCtn)
  
ELSE
  BROWSE FIELDS cMarker =IIF(RECNO()=lnCtDtBrRe AND RECNO(lcCartonSz)=lnDumCtnR,'>',' '):H=' ':R:1:W=.F.,;
                StyCode=Style:H= lcStyTtl     :25:R,;
                Size   =EVAL('Size' +&lcCartonSz..SzCode) :H = 'Size':7 :R,;
                SzQty  =EVAL('Qty'+&lcCartonSz..SzCode)   :H = 'Qty.':6 :R,;                            
                SzWgh  =EVAL('Weight'+&lcCartonSz..SzCode):H = 'Wgh.':6 :R ;              
           FOR EVAL(lcCtnDtl+'.Br'+&lcCartonSz..SzCode) AND &lcCartonSz..SzCode <= STR(&lcCtnDtl..SzCnt,1) ;
           SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
           TITLE(lcCtDtBrTt) WHEN lfwCtnDtlBr() VALID :F lfvCtnDtlBr();
           WINDOW (lcWinCtn2) IN WINDOW (lcWinCtn)
ENDIF
*C038675,1 [End]         
*B123477,4  TMI [End  ] 
*C122070,1 [End]         
=lfwCtnDtlBr()
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfwCtnDtlBr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : refresh the edit rigion for carton details according to
*!             lcCtnDtl file
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

IF (laScrMode[3] OR llNew) AND lnActFolder = 2
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

IF lnActFolder = 2
  SHOW WINDOW (lcCtDtBrTt) REFRESH SAME
*--  SHOW GETS WINDOW (lcWinCtn3) ONLY
ENDIF  

SHOW GET pbDtlRem  &lcRemDtlSt
SHOW GET pbStyBrow &lcCtnStySt
SHOW GET lcCtnSty  &lcCtnStySt
SHOW GET pbSzBrow  &lcStySzSt
SHOW GET lcStySz   &lcStySzSt
SHOW GET lnStyQty  &lcStyQtySt
SHOW GET lnStyWgh  &lcStyWghSt

SHOW GET pbStyBrow DISABLE
SHOW GET lcCtnSty  DISABLE
SHOW GET pbSzBrow  DISABLE
SHOW GET lcStySz   DISABLE
SHOW GET lnStyQty  DISABLE
SHOW GET lnStyWgh  DISABLE
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvCtnDtlBr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() function
*!*************************************************************

FUNCTION lfvCtnDtlBr

IF WONTOP() # (lcCtDtBrTt)
  =gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfActPad
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : To create the "Option" menu pad.
*!*************************************************************

FUNCTION lfActPad

DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
ON KEY LABEL ALT+C lError = .T.

*-- Define the "Options" menu pad.
DEFINE PAD   _OPTIONS OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , SPACE(1)

*-- Define the "Options" and the "Transactions" popups.
DEFINE POPUP _OPTPOP MARGIN SHADOW

*-- Define the "Options" bars in the "Options" menu pad.
*-- Define the "Transactions" bars in the "Transactions" menu pad.
DEFINE BAR 1 OF _OPTPOP PROMPT "Show Open Quantities Only      " SKIP FOR (laScrMode[1] .OR. lnActFolder=2) KEY ALT+O
SET MARK OF BAR 1 OF _OPTPOP TO llShwOpn

ON PAD _OPTIONS OF _MSYSMENU ACTIVATE POPUP _OPTPOP
ON SELECTION BAR 1 OF _OPTPOP DO lpShwOpn


*-- end of lfActPad.
*!*************************************************************
*! Name      : lfRELPad
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : To release the "Option" menu pad.
*!*************************************************************

FUNCTION lfRELPad

RELE BAR 100 OF P01PU01
RELE BAR 101 OF P01PU01
RELE BAR 102 OF P01PU01
*-- Define the "Options" menu pad.
RELE PAD   _OPTIONS OF _MSYSMENU

*!*************************************************************
*! Name      : lfDeActPad
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : To release the "Options" menu pad.
*!*************************************************************

FUNCTION lfDeActPad

RELEASE PAD _OPTIONS OF _MSYSMENU 
RELEASE POPUP _CLRSEL
RELEASE POPUP _SHWOPN

*!*************************************************************
*! Name      : lfWinCtnSt
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : To adjust add buttons for carton header and 
*!             carton detail staus 
*!*************************************************************

FUNCTION lfWinCtnSt
PRIVATE lnCtnHdrRe,lnCtnDtlRe

IF lnActFolder = 2 AND (laScrMode[3] OR llNew)
  STORE "ENABLE" TO lcAddHdrSt
  STORE "ENABLE" TO lcPrnLbl

  IF EMPTY(&lcCtnHdr..Cart_No)
    STORE "DISABLE" TO lcAddDtlSt
    STORE "DISABLE" TO lcPrnLbl
  ELSE
    STORE "ENABLE" TO lcAddDtlSt
    STORE "ENABLE" TO lcPrnLbl
  ENDIF
ELSE
  STORE "DISABLE" TO lcAddHdrSt,lcAddDtlSt
  STORE "DISABLE" TO lcPrnLbl
ENDIF


*!*************************************************************
*! Name      : lfChkUnCmS
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Check if there is uncomplete session
*!*************************************************************

FUNCTION lfChkUnCmS
PRIVATE lnAlias, llGoOut, lnReprocess
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Calling functions that Create Temp. files in case
*!             un detectimg uncomp. Session
*!*************************************************************

FUNCTION lfCrtUnCmp

= lfCrSelFiles()
= lfCrCtnFiles()
= lfCrSelSum()

*!*************************************************************
*! Name      : lfCrSelFiles
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Create the files that are used in Function lfvSelOrd
*!             (lcPckLin)
*!*************************************************************

FUNCTION lfCrSelFiles

PRIVATE lnCurAlias,lnI
lnCurAlias = SELECT(0)

=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPCK_HDRVR','SH')

*--We modify way of selection to just select By Style-Color

lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect'
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
laFileStru[lnI,1] = 'OrgPQty'
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
laFileStru[lnI,1] = 'OrdQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- The carton No the style applied into
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nCarton'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSizeNo'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PkStyQty'
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
laFileStru[lnI,1] = 'PWgh'
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
laFileStru[lnI,1] = 'Selected'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_ID'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 16
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkVersion'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkColor'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPckSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

*C122070,1 HBG 04/04/2004 Add new filed to temp detail file to know if this pack is range or not [Begin]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lRange'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DisCtnQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nRngCart'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lIgnor'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0
*C122070,1 [End]

*C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Dyelot'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0
*C038675,1 [End]
*--Indexes
DIMENSION laIndx[5,2]

*C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
*laIndx[1,1] = "Style"
*laIndx[1,2] = lcPckLin
laIndx[1,1] = "Style+Dyelot"
laIndx[1,2] = lcPckLin
*C038675,1 [End]


laIndx[2,1] = "IIF(Selected>0,'Y','N')"
laIndx[2,2] = 'Selected'

laIndx[3,1] = "IIF(OQty>0,'Y','N')"
laIndx[3,2] = 'Opened'

laIndx[4,1] = "IIF(PQty=0,'Y','N')"
laIndx[4,2] = 'NoPacked'

*C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
*laIndx[5,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo"   
*laIndx[5,2] = lcPakIndxSt
laIndx[5,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+Dyelot+cSizeNo"   
laIndx[5,2] = lcPakIndxSt
*C038675,1 [End]

=gfCrtTmp(lcPckLin,@laFileStru,@laIndx)
SET ORDER TO lcPakIndxSt IN (lcPckLin)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfCrCtnFiles
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Create the files that are used in Function lfvSelOrd
*!             (lcOrdLin,lcPckLin)
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

DIMENSION laIndx[2,2]
laIndx[1,1] = "STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[1,2] = lcCtnHdr

laIndx[2,1] = "Empty+STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[2,2] = "EMPTY"

=gfCrtTmp(lcCtnHdr,@laFileStru,@laIndx)           
SET ORDER TO lcCtnHdr In (lcCtnHdr)

*:***************************************************************************
*:*******************  lcCtnDtl   *******************************************

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

*--
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0
*--

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

*-- TotWight
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotWeight'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 2
*-

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

*---(Start) Increased the width of the line no field used in 
*---the packing list lines to be 6 instead of 3 digits
*---not to have an error when creating big packing lists
laFileStru[lnI,3] = 6

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

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_ID'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 16
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkColor'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPckSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkVersion'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'NPACKNO'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*C122070,1 HBG 04/04/2004 Add flag to Display the first style only in case of range pack[Begin]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lFlag'
laFileStru[lnI,2] = 'l'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0
*C122070,1 [End]

*C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Dyelot'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0
*C038675,1 [End]

DIMENSION laIndx[2,2]
*C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
*laIndx[1,1] = "STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style" 
*laIndx[1,2] = lcCtnDtl
laIndx[1,1] = "STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style+Dyelot" 
laIndx[1,2] = lcCtnDtl
*C038675,1 [End]

laIndx[2,1] = "cStatus"
laIndx[2,2] = "Status"

=gfCrtTmp(lcCtnDtl,@laFileStru,@laIndx)           
SET ORDER TO lcCtnDtl In (lcCtnDtl)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfAdUnCmSR
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Adding record in uncomplete session file 
*!*************************************************************

FUNCTION lfAdUnCmSR
PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT UnCmSess
IF !SEEK('I')
  APPEND BLANK
ENDIF
lnUnCmSeRc = RECNO()
BLANK
REPLACE Status     WITH 'O'      ,;
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Update the variable string 
*!*************************************************************

FUNCTION lfUpdVars

lcFiles = "lcPckLin," + lcPckLin + "," + ORDER(lcPckLin) + ";" + ;
          "lcCtnHdr," + lcCtnHdr + "," + ORDER(lcCtnHdr) + ";" + ;
          "lcCtnDtl," + lcCtnDtl + "," + ORDER(lcCtnDtl) + ";"  
*-* llNoThing = IIF(lnUnCmSeRc=0, .T., gfSavSess(lcProgID, lcFiles, @laVars, lcSession))

*!*************************************************************
*! Name      : lfUpdUnCmS
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Update the current object and the status of the session
*!*************************************************************

FUNCTION lfUpdUnCmS
PARAMETERS lcStatus, lcCurObj
PRIVATE lnAlias

IF lnUnCmSeRc <> 0
  lnAlias  = SELECT(0)
  lcStatus = UPPER(LEFT(lcStatus,1))
  SELECT UnCmSess
  GOTO lnUnCmSeRc
  REPLACE cCurrObj WITH lcCurObj ,;
          Status   WITH lcStatus
  IF Status $ "IC"
    UNLOCK
  ENDIF
  SELECT(lnAlias)          
ENDIF

*!*************************************************************
*! Name      : lpClsScr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Cancel any editing in the current screen
*!*************************************************************

PROCEDURE lpClsScr
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

*-- ORDHDR file locking
=lfOrdHdLck(.F.)

SELECT OrdLine
lcTag = ORDER()
SET ORDER TO TAG OrdLinSt
IF SEEK('O'+laData[2],'OrdLine') 
  SELECT OrdLine
  lnCurRecNo = RECNO()
  SCAN REST WHILE cOrdType+Order+Store+Style+STR(lineno,6) = 'O'+laData[2]
    = gfObj_Lock(.F.)
  ENDSCAN  
  GOTO lnCurRecNo 
ENDIF
SELECT OrdLine
SET ORDER TO TAG lcTag
 
SELECT(lnCurAlias)

*-* llNoThing = lfUpdUnCmS("Initia", SPACE(0))

*!*************************************************************
*! Name      : lfUpdate
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : 
*!*************************************************************

FUNCTION lfUpdate

= gfUpdate()
IF UPDATED()
  llAnyUpd = .T.
ENDIF

*!*************************************************************
*! Name      : lfChkOrdLok
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : check using this order in creating pack and 
*!           : Lock ordhdr record if it is not
*!*************************************************************

FUNCTION lfChkOrdLok
PARAMETERS llLock
PRIVATE llGoOn,lnCurAlias,lnCurRecNo

llGoOn = .T.

*-- ORDHDR file locking
llGoOn = lfOrdHdLck( llLock )

IF llGoOn
  lcSvOrd = ORDER('ORDLINE')
  SET ORDER TO ORDLINST IN ORDLINE
  lnCurAlias = SELECT(0)
  lnCurRecNo = 0
  IF SEEK('O'+laData[2],'OrdLine')
    SELECT OrdLine
    lnCurRecNo = RECNO()
    SCAN REST WHILE cOrdType+Order+Store+Style+STR(lineno,6) = 'O'+laData[2]
      *lcPrvPack = IIF(ORDLINE.LLOK_STAT,' ',lcPrvPack)    && used to refresh data of the line needed to be edited
      llGoOn = gfObj_Lock(llLock)
      IF !llGoOn      
        EXIT
      ENDIF
    ENDSCAN  
  ENDIF
ENDIF

SELECT OrdLine
SET ORDER TO &lcSvOrd IN ORDLINE
IF BETWEEN(lnCurRecNo,1,RECCOUNT('ORDLINE'))
  GOTO lnCurRecNo 
ENDIF

SELECT (lnCurAlias)

RETURN llGoOn

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Print carton labels
*!*************************************************************

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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Algorithm to Compute the modulus-10 Check digit for the 
*!             UCC 128 code
*!*************************************************************

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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Clculatre the style size.
*!*************************************************************
*! Example     : = lfGetStySz()
*!*************************************************************
*!C101735,1
*!
FUNCTION lfGetStySz
PRIVATE lcCurrAlis , lnRecPoint , lnDetPoint

*-- Save current setting
lcCurrAlis = SELECT (0)

SELECT (lcCtnDtl)
lnRecPoint = RECNO()
lnDetPoint = RECNO(lcCartonSz)

SEEK STR(lncarton,4)

lcStyle  = SUBSTR(Style,1,lnMajLen)

IF lcFree_Clr="C"
  lcColor  = SUBSTR(Style,lnNonMajSt,lnColorLen)
ELSE
  lcColor  = SPACE(6)
ENDIF
  
lcUpdSty = Style

lcSize = ''
FOR lnSzNo = 1 TO 8
  lcSzNo = STR(lnSzNo,1)
  IF Br&lcSzNO
    lcSize = Size&lcSzNo
    EXIT 
  ENDIF  
ENDFOR

SCAN REST WHILE STR(cart_no,4) = STR(lncarton,4)

  lcStyle = IIF(lcStyle<>SUBSTR(Style,1,lnMajLen),SPACE(lnMajLen),lcStyle)
  IF lcFree_Clr="C"
    lcColor = IIF(EMPTY(lcStyle),SPACE(lnColorLen),;
              IIF(lcColor<>SUBSTR(Style,lnNonMajSt,lnColorLen),'MIXED',lcColor))
  ENDIF            

  lcSize1 = ''
  IF !EMPTY(lcStyle)
    FOR lnSzNo1 = 1 TO 8
      lcSzNo = STR(lnSzNo1,1)    
      IF Br&lcSzNO
        lcSize1 = Size&lcSzNo
        lcSize  = IIF(lcSize <>lcSize1,'MIXED',lcSize)
        IF lcSize = 'MIXED'
          EXIT 
        ENDIF  
      ENDIF
    ENDFOR
  ELSE
    lcSize = SPACE(5)  
  ENDIF  

ENDSCAN

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
*! Name      : lpPortScr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Handle the printing  labels screen setup
*!*************************************************************
PROCEDURE lpPortScr
PRIVATE lcOldPort , llOldPrn , llCancel
lcOldPort = lcSndPort
llOldPrn  = llScrPrnLb
llCancel  = .F.

IF RIGHT(gcScrDir,1)='\'
  DO (gcScrDir + 'ALOUTPRT.SPX')
ELSE

  DO (gcScrDir + '\ALOUTPRT.SPX')
ENDIF  

IF llCancel
  lcSndPort  = lcOldPort
  llScrPrnLb = llOldPrn
ENDIF
*-- end of lpPortScr.

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Valid Function for the filter push buttoms
*!*************************************************************
FUNCTION lfvStyFltr
PARAMETERS lcType
DO CASE 
  CASE lcType = 'M'
    SELECT DISTINCT SUBSTR(Style,1,LEN(lcMask)) FROM (lcPckLin) INTO ARRAY laStySrc
    =gfMover(@laStySrc,@laStyTrgt,lcStyFltr,.T.,'')
  CASE lcType = 'N'
    SELECT DISTINCT SUBSTR(Style,len(lcMask)+2,len(Style)) FROM &lcPckLin INTO ARRAY laClrSrc
    =gfMover(@laClrSrc,@laClrTrgt,lcStMjFltr,.T.,'')
ENDCASE
*--END FUNCTION  lfvStyFltr

*!*************************************************************
*! Name      : lpFltrScr 
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : to load the filter screen
*! Called by : the menu bar Style Filter
*:***************************************************************************
PROCEDURE lpFltrScr

lcStyFltr = 'Only these '+ALLTRIM(gfitemmask("HN"))
lcStMjFltr= 'Only these '+ALLTRIM(gfitemmask("HM"))
DIMENSION laOldSty[ALEN(laStyTrgt)]

DIMENSION laOldClr[ALEN(laClrTrgt)]
=ACOPY(laStyTrgt,laOldSty)
=ACOPY(laClrTrgt,laOldClr)
DO (gcScrDir + '\ALSTYFLTR.SPR')
GO TOP IN (LCSCAFILE)

*--END PROCEUDRE lpFltrScr
*!****************************************************************************
*! Name      : lfvpbOk
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Valid function for the ok push buttom in the filter screen
*!*****************************************************************************
FUNCTION lfvpbOk

lcOAlias=ALIAS()

SELECT (lcTmpPck)

DO CASE
  CASE !EMPTY(laStyTrgt(1)) AND !EMPTY(laClrTrgt(1))
    SET FILTER TO ASCAN(laStyTrgt,ALLTRIM(SUBSTR(Style,1,LEN(lcMask)))) > 0 AND ;
    ASCAN(laClrTrgt,ALLTRIM(SUBSTR(Style,LEN(lcMask)+2))) > 0
    LOCATE FOR IIF(llShwOpn,EVAL('OQty'+&lcScaFile..SzCode)>0 AND EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1),;
    EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1))
    IF EOF()
    * --No Records To Browse
      =gfModalGen('TRM44032B00000','DIALOG' )
      =lfvpbRst()
      RETURN
    ENDIf
  CASE EMPTY(laStyTrgt[1]) AND EMPTY(laClrTrgt[1]) 
    SET FILTER TO
  CASE EMPTY(laStyTrgt[1]) AND !EMPTY(laClrTrgt[1])
    SET FILTER TO ASCAN(laClrTrgt,ALLTRIM(SUBSTR(Style,LEN(lcMask)+2))) > 0
    LOCATE FOR  IIF(llShwOpn,EVAL('OQty'+&lcScaFile..SzCode)>0 AND EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1),;
    EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1))
  CASE !EMPTY(laStyTrgt[1]) AND EMPTY(laClrTrgt[1])  
    SET FILTER TO ASCAN(laStyTrgt,ALLTRIM(SUBSTR(Style,1,LEN(lcMask)))) > 0
    LOCATE FOR IIF(llShwOpn,EVAL('OQty'+&lcScaFile..SzCode)>0 AND EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1),;
    EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcPckLin..SzCnt,1))
ENDCASE  
GO TOP IN (LCSCAFILE)
SHOW WINDOW (lcDtlBrTtl) REFRESH SAME
SELECT (lcOAlias)
*--END FUNCTION  lfvpbOk

*!*************************************************************
*! Name      : lfReset
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Valid function for the Reset push buttom in the filter screen
*!*************************************************************
Function lfReset

DIMENSION laStyTrgt(1)
DIMENSION laClrTrgt(1)
STORE SPACE(0) TO laStyTrgt,laClrTrgt
lcOAlias=ALIAS()
SELECT (lcTmpPck)
SET FILTER TO
SELECT (lcOAlias)
*--END FUNCTION  lfReset

*!*************************************************************
*! Name      : lfvpbRst
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Valid function for the Reset push buttom in the filter screen
*!*************************************************************
Function lfvpbRst

=lfReset()
SHOW WINDOW (lcDtlBrTtl) REFRESH SAME

*--END FUNCTION lfvpbRst

*!******************************************************************************
*! Name      : lfvpbCncl
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Valid function for the cancel push buttom in the filter screen
*!*******************************************************************************
FUNCTION lfvpbCncl

DIMENSION laStyTrgt[ALEN(laOldSty)]
DIMENSION laClrTrgt[ALEN(laOldClr)]
=ACOPY(laOldSty,laStyTrgt)
=ACOPY(laOldClr,laClrTrgt)
*--END FUNCTION  lfvpbCncl

*!******************************************************************************
*! Name      : lpUpdtPkTk
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Valid function for the update piktkt menu bar.
*!*******************************************************************************
PROCEDURE lpUpdtPkTk
llUpdtPkTk = !llUpdtPkTk
*-- End of lfUpdtPkTk.


*!*************************************************************
*! Name      : lfSuppForc
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : User can Force allocation. (Y/N)
*!*************************************************************
*!
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
*! Name        : lfCrSelSum
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date         : 06/09/2003
*! Purpose   : Create the files that are used in Function lfvSelOrd
*!                  to get pack summery (lcSumPck)
*!*************************************************************

FUNCTION lfCrSelSum

PRIVATE lnCurAlias,lnI
lnCurAlias = SELECT(0)

*--We modify way of selection to just select By Style-Color
lnI = 1

DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 2

*-- Add field for available qty used in browse
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PkCtnQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_ID'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkVersion'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkColor'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPckSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'llPack'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lRange'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nCarton'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*C122070,1 HBG 04/04/2004 Add new filed to summary file to know if this pack is range or not [Begin]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cRange'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0
*C122070,1 [End]

*C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Dyelot'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0
*C038675,1 [End]

DIMENSION laIndx[1,2]
*C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
*laIndx[1,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion"
*laIndx[1,2] = lcSumPck
laIndx[1,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Dyelot"
laIndx[1,2] = lcSumPck
*C038675,1 [End]

=gfCrtTmp(lcSumPck,@laFileStru,@laIndx)
SET ORDER TO lcSumPck IN (lcSumPck)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

FUNCTION lfGetGmSz
PARAMETER lcPackSize
PRIVATE lcNombr

IF !EMPTY(lcPackSize)
  =SEEK('S'+LEFT(lcPackSize,1),'SCALE')
  lcNombr = RIGHT(lcPackSize,1)
  lcLocSize=EVAL('SCALE.SZ'+lcNombr)
ELSE
  lcLocSize ='*****'
ENDIF  


RETURN lcLocSize 

*!*************************************************************
*! Name      : lfColSum
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Example            : =lfColSum()
*!*************************************************************
*
FUNCTION lfColSum
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

SELECT (lcPckLin)
LOCATE
lcPack_Id  = ''
SCAN
 *  IF lcPack_Id = LEFT(Pack_Id,IIF(EMPTY(&lcPckLin..Pack_id),19,16)) + cpkcolor + cpcksize + cPkVersion 
  *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
  *IF lcPack_Id = IIF(EMPTY(&lcPckLin..Pack_id),STYLE,LEFT(Pack_Id,16)) + cpkcolor + cpcksize + cPkVersion 
  IF lcPack_Id = IIF(EMPTY(&lcPckLin..Pack_id),STYLE,LEFT(Pack_Id,16)) + cpkcolor + cpcksize + cPkVersion + IIF(EMPTY(&lcPckLin..Pack_id),Dyelot,"")
  *C038675,1 [End]
    SELECT(lcSumPck) 
    *B123477,4  TMI [Start] 
    IF !&lcPckLin..LRANGE .OR. (&lcPckLin..LRANGE .AND. &lcSumPck..CtnTotQty = 0)
      *B123477,4  TMI [End  ]  
      REPLACE OTotQty      WITH OTotQty   + &lcPckLin..OQty,;
              CtnTotQty    WITH CtnTotQty + IIF(&lcPckLin..OQty>0,&lcPckLin..CtnQty,0),;
              PkCtnQty     WITH IIF(!EMPTY(&lcPckLin..Pack_id),CtnTotQty,0) 
      *B123477,4  TMI [Start] 
    ENDIF
    *B123477,4  TMI [End  ] 
    *-- Update AvlQty field in Summary file
    REPLACE AvlQty WITH AvlQty + &lcPckLin..AvlQty

   IF !EMPTY(&lcPckLin..Pack_id)
     REPLACE PTotWgh WITH PTotWgh + &lcPckLin..StyWgh
   ENDIF
           
  ELSE
    SELECT(lcSumPck)  
    APPEND BLANK
    REPLACE OTotQty      WITH &lcPckLin..OQty,;
            CtnTotQty    WITH IIF(&lcPckLin..OQty>0,&lcPckLin..CtnQty,0),;
            PkCtnQty     WITH IIF(!EMPTY(&lcPckLin..Pack_id),CtnTotQty,0)    ,;
            PTotWgh      WITH &lcPckLin..StyWgh,;
            cPkVersion   WITH &lcPckLin..cPkVersion,; 
            cPkColor     WITH &lcPckLin..cPkColor,;
            cPckSize     WITH &lcPckLin..cPckSize
           
    *--  Update AvlQty field in Summary file
    REPLACE AvlQty WITH &lcPckLin..AvlQty
    
    IF EMPTY(&lcPckLin..Pack_id)
      REPLACE PACK_ID  WITH &lcPckLin..Style
      *C038675,1 HBG 10/26/2004 Update Configuration field [Begin]
      REPLACE Dyelot   WITH &lcPckLin..Dyelot
      *C038675,1 [End]       
    ELSE
      llGetPack  =SEEK('P'+laData[4]+PADR(&lcPckLin..Pack_Id,16)+&lcPckLin..cPkColor+;
                        &lcPckLin..cPckSize+&lcPckLin..cPkVersion,'SPCK_HDR')
      IF !llGetPack 
        = SEEK('P*****'+PADR(&lcPckLin..Pack_Id,16)+&lcPckLin..cPkColor+;
                        &lcPckLin..cPckSize+&lcPckLin..cPkVersion,'SPCK_HDR')
      ENDIF 
      *C122070,1 HBG 04/04/2004 Get the value of lRange from Ordline file not SPCK_HDR [Begin]
      *REPLACE PACK_ID      WITH &lcPckLin..PACK_ID,;
              llPack       WITH .T.,;
              lRange       WITH SPCK_HDR.lRange
      REPLACE PACK_ID      WITH &lcPckLin..PACK_ID,;
              llPack       WITH .T.,;
              lRange       WITH &lcPckLin..lRange,;
              cRange       WITH IIF(lRange,'Yes','No')
      *C122070,1 [End]                
    ENDIF       
    *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
    *lcPack_Id = LEFT(Pack_Id,IIF(EMPTY(&lcPckLin..Pack_id),19,16)) + cpkcolor + cpcksize + cPkVersion             
    lcPack_Id = LEFT(Pack_Id,IIF(EMPTY(&lcPckLin..Pack_id),19,16)) + cpkcolor + cpcksize + cPkVersion + IIF(EMPTY(&lcPckLin..Pack_id),Dyelot,"")
    *C038675,1 [End]
  ENDIF 
ENDSCAN
llApply = .F.
=lfUpdSumFl()
*
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfPoupBrow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Function To validate PopUp
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfPoupBrow()
*!*************************************************************
*
FUNCTION lfPoupBrow
PARAMETERS lnShPck
llShoPckDt = IIF(lnShPck=1,.T.,.F.)
=lfDtlBrow()

*!*************************************************************
*! Name      : lfvCtnSize
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Validat lcCtnSty field .
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
    REPLACE Style      WITH lcCtnSty,;
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
    UNLOCk IN (lcCtnHdr)
    SELECT(lcCtnDtl)

    lcExpGma2 = "SEEK(&lcTmpPck..Pack_ID+&lcTmpPck..cPkColor+&lcTmpPck..cPckSize+&lcTmpPck..cPkVersion+lcCtnSty+STR(lnOrdLine,6),lcPckLin)"
    IF &lcExpGma2    

      SKIP lnSzOrd - 1 IN (lcPckLin)
      SELECT(lcPckLin)
      lcPQtyFld = 'PQty'+STR(lnSzOrd,1)
      lcPWghFld = 'PWgh'+STR(lnSzOrd,1)
      lcOQtyFld = 'OQty'+STR(lnSzOrd,1)
      REPLACE &lcPQtyFld WITH &lcPQtyFld+lnStyQty,;
              &lcPWghFld WITH &lcPWghFld+lnWgh,;
              &lcOQtyFld WITH EVAL('OrdQty'+STR(lnSzOrd,1)) - EVAL('PQty'+STR(lnSzOrd,1))
     = RLOCK(lcPckLin)
     UNLOCk IN (lcPckLin)
    ENDIF

    SELECT(lcCtnDtl)
    
  ENDIF
ENDIF

= lfwCtnDtlBr()

USE IN PackLines

USE IN StyQty  

SELECT(lnCurAlias)


*!*************************************************************
*! Name      : lfCtrKey
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Function To validate Key
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfCtrKey()
*!*************************************************************
*
FUNCTION lfCtrKey
=lfDtlBrow()

*!*************************************************************
*! Name      : lfUpdSumFl
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Function To validate Key
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfUpdSumFl()
*!*************************************************************

FUNCTION lfUpdSumFl
PRIVATE lnPkWght

lcCurAlis = ALIAS()
SELECT(lcSumPck) 
lcExpr = IIF(llApply,"cSelect = '»'",".T.")
SCAN FOR &lcExpr 
  IF &lcSumPck..llPack AND !(&lcSumPck..lRange)
    llGetPack  =SEEK('P'+laData[4]+PADR(Pack_Id,16)+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')
    IF !llGetPack 
      = SEEK('P*****'+PADR(Pack_Id,16)+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')
    ENDIF 
    *B123477,3  TMI [Start] check that the pack exist
    IF !EOF('SPCK_HDR')
      *B123477,3  TMI [End  ] 
      =lfPackQty()                && Update nPckQty field if it 0
      lnOqty = &lcSumPck..OTotQty / SPCK_HDR.npckqty
      lnPqty = &lcSumPck..PTotQty / SPCK_HDR.npckqty
      lnPkWght = lfPackWght()
      *B123477,3  TMI [Start] PtotWgh is calculated in lfColSum, do not repeate calculating it
      *REPLACE OTotQty WITH lnOqty,;
      *        PTotQty WITH lnPqty,;
      *        PkCtnQty WITH PkCtnQty / SPCK_HDR.npckqty,;
      *        PTotWgh  WITH  lnPkWght
      REPLACE OTotQty WITH lnOqty,;
              PTotQty WITH lnPqty,;
              PkCtnQty WITH PkCtnQty / SPCK_HDR.npckqty
    ENDIF
    *B123477,3  TMI [End  ] 
  ENDIF          
ENDSCAN
*REPLACE ALL &lcSumPck..cSelect WITH IIF(!EMPTY(&lcSumPck..cSelect),'»','')
GO TOP  

SELECT (lcCurAlis)


*:**************************************************************************
*:* Name        : lfSelCurr
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Select the current record in lcPckLin if it is clicked by mouse
*:***************************************************************************
*:* Called from : ON KEY LABEL LEFTMOUSE
*:***************************************************************************
FUNCTION lfSelCurr
PRIVATE lcPack_Id,lcSty,laCnt

*-- No selection in View or Select mode
IF laScrMode[1] .OR. laScrMode[2]
  RETURN
ENDIF


CLEAR TYPEAHEAD
lnX =INT(MROW())  && get the mouse row
lnY =INT(MCOL())  && get the mouse column
* if the mouse click is inside the browse window 
IF  (MROW(lcWinDtl1)<>-1) AND (MCOL(lcWinDtl1)<>-1) AND (MROW(lcWinDtl1)>lnLineRange);
      AND BETWEEN(MCOL(lcWinDtl1),2,WCOL(lcWinDtl1)-IIF(_DOS OR _UNIX,2,1.77));
     AND BETWEEN(MROW(lcWinDtl1),2,WROWS(lcWinDtl1)-IIF(_DOS OR _UNIX,2,1.77))
  * it is the first click
  IF !llClick
    lnTimelimt = SECONDS()
    lnCurR     = lnX
    llClick    = .T.
  ELSE
  * if it was the second click then check if it is in the double click time
  * range or not if it is the select the record 
    IF SECONDS() < lnTimelimt + _DBLCLICK .AND. lnCurR  = INT(MROW());
       AND  lnX=lnCurR &&INT(ABS(MROW(lcBrowTtl)-lnCurR))=0       
      llClick    = .F.

      IF WONTOP() = lcDtlBrTtl .AND. !EOF(lcTmpPck) .AND. EMPTY(NCARTON)
        IF ALIAS()=lcTmpPck
          lcPack_Id = Pack_Id + cpkcolor + cpcksize + cPkVersion
          IF EMPTY(lcPack_Id)
            IF AvlQty>0
              REPLACE cSelect  WITH IIF(cSelect=' ','»',' ') ,;
                      Selected WITH IIF(!EMPTY(cSelect),1,0)
              lcSty = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE
              IF SEEK(STYLE + cpkcolor + cpcksize + cPkVersion , lcSumPck )           
                DIME laCnt[1]
                laCnt[1] = 0
                SELECT COUNT(*) FROM &lcTmpPck ;
                WHERE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = lcSty ; 
                  AND !EMPTY(cSelect) AND EMPTY(PACK_ID) AND !DELETED() ;
                INTO ARRAY laCnt
                REPLACE &lcSumPck..CSELECT WITH IIF(laCnt[1]>0,'»','')
              ENDIF
            ENDIF
          ELSE
            *--Message to tell user it is not belonge to pack
            =gfModalGen("INM00000B42001","Dialog",.F.,.F.,;
                     "It is not allowed to select/unselect individual styles belonging to a Pack ID.")
          ENDIF

        ELSE  && This is the case of lcSumPck alias
          IF AvlQty>0            
            =lfvSel(IIF(llPack,'PACK','STY'))
          ENDIF        

        ENDIF   
      ENDIF
    ENDIF
    IF lnCurR    = INT(MROW())
      lnTimelimt = SECONDS()
    ELSE
      llClick    = .F.
    ENDIF
  ENDIF
ELSE
  * if the click was outside the browse window then check if the user is selecting
  * any object from the control window
  llClick    = .F.
  PUSH KEY
  ON KEY
  lcObjName=lfGetObj(lnX,lnY)
  CLEAR TYPEAHEAD
  * if the use is selecting an object from the control window then make
  * this object the current object and press it to go to the validation function
  * of this object
  IF !EMPTY(lcObjName) AND !("DUMY"$UPPER(lcObjName)) .AND. LEFT(lcObjName,2)#'IB'
    _CUROBJ=OBJNUM(&lcObjName)
    CLEAR TYPEAHEAD
    IF _CUROBJ = OBJNUM(&lcObjName)  
      KEYBOARD "{ENTER}" PLAIN CLEAR
    ENDIF
  ELSE
    CLEAR TYPEAHEAD
  ENDIF
  POP KEY
ENDIF
*-- Update buttons after mouse select
=lfSelStatus()
*-- end of lfSelCurr.

*:**************************************************************************
*:* Name        : lfvQtyCtn
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Valid function for Carton Qty field (style or pack)
*:***************************************************************************
FUNCTION lfvQtyCtn
PRIVATE lcOrd
lcOrd = ORDER(lcTmpPck)

lcTmpAls = ALIAS()
DO CASE
  CASE ALIAS() = lcTmpPck

    *B125190,1 NNA 01/16/2005 (Begin) if this a Range Pack 
    IF EVAL(lcTmpPck+'.LRANGE')=.T.
    *B125190,1 NNA (End)

      *C122070,1 HBG 04/04/2004 Update CtnQty with value entered in the new field "DisCtnQty" [Begin]
      REPLACE CtnQty WITH DisCtnQty
      *C122070,1 [End]
      DIMENSION laCtnSum[1]
      laCtnSum[1] = 0
      IF !EMPTY(PACK_ID)
        lcPck = PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION
        SELECT SUM(CtnQty) FROM &lcTmpPck WHERE ;
        PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION = lcPck ;
        .AND. AvlQty>0 INTO ARRAY laCtnSum
        IF SEEK(PADR(PACK_ID,19)+cPkColor+cPckSize+cPkVersion,lcSumPck)
          REPLACE &lcSumPck..CtnTotQty WITH laCtnSum[1]
        ENDIF
      ELSE
        *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
        *lcPck = STYLE+CPKCOLOR+CPCKSIZE+CPKVERSION
        *SELECT SUM(CtnQty) ;
          FROM &lcTmpPck ;
          WHERE STYLE+CPKCOLOR+CPCKSIZE+CPKVERSION = lcPck ;
          .AND. EMPTY(PACK_ID) .AND. AvlQty>0;
          INTO ARRAY laCtnSum
        lcPck = STYLE+CPKCOLOR+CPCKSIZE+CPKVERSION+DYELOT
        SELECT SUM(CtnQty) ;
          FROM &lcTmpPck ;
          WHERE STYLE+CPKCOLOR+CPCKSIZE+CPKVERSION+Dyelot = lcPck ;
          .AND. EMPTY(PACK_ID) .AND. AvlQty>0;
          INTO ARRAY laCtnSum
        *C038675,1 [End]    
        IF SEEK(lcPck,lcSumPck)
          REPLACE &lcSumPck..CtnTotQty WITH laCtnSum[1]
        ENDIF
      ENDIF

    *B125190,1 NNA 01/16/2005 (Begin) if this a free style or a non Range Pack
    ELSE
      IF !EMPTY(PACK_ID)
        lcPck = PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION
        SELECT SUM(CtnQty) FROM &lcTmpPck WHERE ;
        PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION = lcPck ;
        .AND. AvlQty>0 INTO ARRAY laCtnSum
        IF SEEK(PADR(PACK_ID,19)+cPkColor+cPckSize+cPkVersion,lcSumPck)
          REPLACE &lcSumPck..CtnTotQty WITH laCtnSum[1]
        ENDIF
      ELSE
        lcPck = STYLE+CPKCOLOR+CPCKSIZE+CPKVERSION+DYELOT
        SELECT SUM(CtnQty) ;
          FROM &lcTmpPck ;
          WHERE STYLE+CPKCOLOR+CPCKSIZE+CPKVERSION+Dyelot = lcPck ;
          .AND. EMPTY(PACK_ID) .AND. AvlQty>0;
          INTO ARRAY laCtnSum
        IF SEEK(lcPck,lcSumPck)
          REPLACE &lcSumPck..CtnTotQty WITH laCtnSum[1]
        ENDIF
      ENDIF
   ENDIF
   *B125190,1 NNA (End)

    IF &lcTmpPck..CTNQTY = 0
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Qty./Ctn. can not be zero.')
    ELSE  
      IF MOD(&lcTmpPck..OQTY,&lcTmpPck..CTNQTY) # 0
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The O.Qty. is not divisible by Qty./Ctn.')
  *    KEYBOARD [{LEFTARROW}]
      ENDIF
    ENDIF
 
  CASE ALIAS() = lcSumPck
    IF llPack 
      lcKey = PADR(PACK_ID,16)+cPkColor+cPckSize+cPkVersion
      =SEEK(lcKey,lcTmpPck)
      SELECT &lcTmpPck  
      lnQtySm = 0
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcKey
      SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+Dyelot+cSizeNo = lcKey
      *C038675,1 [End]
        REPLACE CtnQty WITH PkStyQty * &lcSumPck..PkCtnQty
        *C122070,1 HBG 04/04/2004 Update the new field "DisCtnQty" with same value saved in "CtnQty"[Begin]
        REPLACE DisCtnQty WITH PkStyQty * &lcSumPck..PkCtnQty
        *C122070,1 [End]
        lnQtySm = lnQtySm + CtnQty
      ENDSCAN
      SELECT &lcSumPck
      REPLACE &lcSumPck..CtnTotQty WITH lnQtySm
    ENDIF
    SELECT &lcSumPck
    IF &lcSumPck..CTNTotQTY = 0
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Qty./Ctn. can not be zero.')
    ELSE  
      IF MOD(&lcSumPck..OTotQty,IIF(llPack,&lcSumPck..PkCtnQty,&lcSumPck..CtnTotQty)) # 0
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The O.Qty. is not divisible by Qty./Ctn.')
  *    KEYBOARD [{LEFTARROW}]
      ENDIF
    ENDIF

ENDCASE

*-- end of lfvQtyCtn.

*:**************************************************************************
*:* Name        : lfvWeight
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Valid function for weight field (style or pack)
*:***************************************************************************
*:* Called from : Browse for lcTmpPck and lcSumPck
*:***************************************************************************
FUNCTION lfvWeight
PRIVATE lnAlias,lcSeek
lnAlias = SELECT()
lcSeek = ''
*
DO CASE
  CASE ALIAS() = lcTmpPck
    IF !EMPTY(PACK_ID)
      DIMENSION laWghtSum[1]
      laWghtSum[1] = 0
      lcPck = PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION
      SELECT SUM(StyWgh) FROM &lcTmpPck WHERE ;
      PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION = lcPck INTO ARRAY laWghtSum
      IF SEEK(PADR(PACK_ID,19)+cPkColor+cPckSize+cPkVersion,lcSumPck)
        REPLACE &lcSumPck..PTotWgh WITH laWghtSum[1]
        *-- Mothing to Do
      ENDIF
      *IF SEEK(PADR(PACK_ID,19)+cPkColor+cPckSize+cPkVersion,lcSumPck)
        *-- Mothing to Do
      *ENDIF
    ELSE
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *IF SEEK(STYLE+cPkColor+cPckSize+cPkVersion,lcSumPck)
      IF SEEK(STYLE+cPkColor+cPckSize+cPkVersion+Dyelot,lcSumPck)
      *C038675,1 [End]
        REPLACE &lcSumPck..PTotWgh WITH &lcTmpPck..StyWgh
      ENDIF
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *lcSeek = PACK_ID+cPkColor+cPckSize+cPkVersion+STYLE
      lcSeek = PACK_ID+cPkColor+cPckSize+cPkVersion+STYLE+Dyelot
      *C038675,1 [End]
      lcSz = CSIZENO
      =SEEK(lcSeek,lcTmpPck)
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *SCAN REST WHILE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+CSIZENO = lcSeek
      SCAN REST WHILE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+Dyelot+CSIZENO = lcSeek
      *C038675,1 [End]
        REPLACE StyWgh WITH &lcSumPck..PTotWgh
      ENDSCAN
      =SEEK(lcSeek+lcSz,lcTmpPck)
    ENDIF
  
  CASE ALIAS() = lcSumPck
    IF llPack 
      REPLACE &lcSumPck..PTotWgh WITH lcOldVal
    ELSE
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *lcLoop = PADR('',16)+CPKCOLOR+CPCKSIZE+CPKVERSION+PACK_ID
      lcLoop = PADR('',16)+CPKCOLOR+CPCKSIZE+CPKVERSION+PADR(PACK_ID,19)+DYELOT
      *C038675,1 [End]
      SELECT (lcTmpPck)      
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *lcSv = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+CSIZENO
      lcSv = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+DYELOT+CSIZENO
      *C038675,1 [End]
      =SEEK(lcLoop,lcTmpPck)
      *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
      *SCAN REST WHILE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+CSIZENO = lcLoop
      SCAN REST WHILE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+DYELOT+CSIZENO = lcLoop
      *C038675,1 [End]
        REPLACE &lcTmpPck..StyWgh WITH &lcSumPck..PTotWgh
      ENDSCAN
      =SEEK(lcSv,lcTmpPck)
    ENDIF
ENDCASE

SELECT (lnAlias)
*-- end of lfvWeight.

*:***************************************************************************
*! Name      : LFGETOBJ
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : function to get the object name from its corrdinates
*!*************************************************************************
*: Passed parameters  : lnx  row corrdinate
*:                      lny  column corrdinate
*:*************************************************************************
*! Returns   :     object name that has the same corrdinate like the par.
*:*************************************************************************     
FUNCTION lfGetObj
PARAMETER lnX,lnY
lcObj=''
* loop through the screen object array and check for the first object that
* it's cordinates is the passed conrrdinate to the function
FOR lnCount=1 TO ALEN(laScObj,1)
  IF BETWEEN(lnX,laScObj[lnCount,1],laScObj[lnCount,3]) AND BETWEEN(lnY,laScObj[lnCount,2],laScObj[lnCount,4])
    lcObj=laScObj[lnCount,5]
    EXIT
  ENDIF
ENDFOR
RETURN lcObj


*:**************************************************************************
*:* Name        : lfGetTmpl
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Get current order tamplate information 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetTmpl()
*:***************************************************************************
FUNCTION lfGetTmpl
*--Loop throgh TMPL_LIN file , if a line is applied into a carton , then write carton # and 
*-- add a line to lcCtnHdr,lcCtnDtl files
PRIVATE lnAlias,lnUpdtWgh,lcSeek,lcSz,lcSvOrd
lnAlias = SELECT(0)
lnUpdtWgh = 0
lcSvOrd = ORDER('SPCK_HDR')
SET ORDER TO SPCK_HDRVR IN SPCK_HDR

SELECT TMPL_HDR
IF SEEK(laData[2])
  llLockOnErr = TMPL_HDR.LLCKONERR
ENDIF  

*B123477,4  TMI [Start] Define a test Array
DIMENSION laPckId[1]
laPckId[1] = ' '
lnPckLn = 0
llRng = .F.
*B123477,4  TMI [End  ] 

SELECT TMPL_LIN
=SEEK(laData[2])
SCAN REST WHILE ORDER+STR(NCARTON,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = laData[2]
  =SEEK(TMPL_LIN.Style,'STYLE')
  =SEEK('S'+STYLE.SCALE,'SCALE')
  =SEEK('P'+laData[4]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'SPCK_HDR') OR ;
   SEEK('P*****'+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'SPCK_HDR')
  =lfPackQty()                && Update nPckQty field if it 0
  *C123477,1  TMI [Start] Create an array laCtn used to allow to display only first line of range packs
  DIMENSION laCtn[TMPL_HDR.TOT_CART]
  *C123477,1  TMI [End  ] 
  FOR lnI = 1 TO 8
    lcSz = STR(lnI,1)
    SELECT TMPL_LIN

    *B125190,1 NNA 01/16/2005 (BEGIN) add field(Dyelot) to the seek EXPR. because it used in 
    *B125190,1 NNA            CP#038675
    *IF !EMPTY(QTY&lcSz) AND SEEK(PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+lcSz,lcPckLin)
    IF !EMPTY(QTY&lcSz) AND SEEK(PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+DYELOT+lcSz,lcPckLin)
    *B125190,1 NNA (END)

      SELECT &lcPckLin
      REPLACE &lcPckLin..NCARTON WITH TMPL_LIN.NCARTON,;
              &lcPckLin..CtnQty  WITH TMPL_LIN.Qty&lcSz,;
              &lcPckLin..StyWgh  WITH TMPL_LIN.WEIGHT
      *B123477,1  TMI [Start] Update field "DisCtnQty" used to deal with range pack case
      REPLACE &lcPckLin..DisCtnQty WITH TMPL_LIN.Qty&lcSz
      *B123477,1  TMI [End  ]  

      =SEEK(IIF(EMPTY(TMPL_LIN.PACK_ID),TMPL_LIN.STYLE,PADR(TMPL_LIN.PACK_ID,19))+TMPL_LIN.cPkColor+TMPL_LIN.cPckSize+TMPL_LIN.cPkVersion,lcSumPck)
      SELECT &lcSumPck
      IF EMPTY(&lcSumPck..NCARTON)     && if there is data befor , empty it to collect from tmpl_lin file
        REPLACE &lcSumPck..CtnTotQty WITH 0,;
                &lcSumPck..PTotWgh   WITH IIF(!llPack,0,&lcSumPck..PTotWgh)
      ENDIF
       
      *B123477,4  TMI [Start] Accomulate sum qty for non range packs only
      *REPLACE &lcSumPck..NCARTON   WITH TMPL_LIN.NCARTON,;  
      *        &lcSumPck..CtnTotQty WITH &lcSumPck..CtnTotQty + TMPL_LIN.Qty&lcSz,; 
      *        &lcSumPck..PTotWgh   WITH IIF(!llPack,TMPL_LIN.WEIGHT,&lcSumPck..PTotWgh),;
      *        &lcSumPck..PkCtnQty  WITH IIF(!llPack,0,&lcSumPck..CtnTotQty/SPCK_HDR.NPCKQTY)
      IF !&lcSumPck..LRange .OR. (&lcSumPck..LRange .AND. &lcSumPck..CtnTotQty = 0 )
        *B123477,4  TMI [End  ] 
        REPLACE &lcSumPck..NCARTON   WITH TMPL_LIN.NCARTON,;  
                &lcSumPck..CtnTotQty WITH &lcSumPck..CtnTotQty + TMPL_LIN.Qty&lcSz,; 
                &lcSumPck..PTotWgh   WITH IIF(!llPack,TMPL_LIN.WEIGHT,&lcSumPck..PTotWgh),;
                &lcSumPck..PkCtnQty  WITH IIF(!llPack,0,&lcSumPck..CtnTotQty/SPCK_HDR.NPCKQTY)                
        *B123477,4  TMI [End  ] 
      ENDIF
      *B123477,4  TMI [End  ] 
      
      *B123477,1  TMI [Start] if this is a range pack , let PkCtnQty be 0,since we pack range pack by pieces
      IF &lcSumPck..LRange
        REPLACE &lcSumPck..PkCtnQty WITH 0 ;
                &lcSumPck..PTotWgh  WITH TMPL_LIN.WEIGHT
      ENDIF      
      *B123477,1  TMI [End  ] 
      
      *--Ask for the carton header
      IF !SEEK(STR(TMPL_LIN.NCARTON,4),lcCtnHdr)
        INSERT INTO (lcCtnHdr)(Cart_No         ,TotPcs ,TotWgh   ,Empty);
                       VALUES (TMPL_LIN.NCARTON,0      ,0        ,'N')
      ENDIF
      
      *--Ask for the carton Details
      SELECT TMPL_LIN
      IF !SEEK(STR(nCarton,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,lcCtnDtl)
        SELECT &lcCtnDtl
        APPEND BLANK
        REPLACE PACK_ID     WITH &lcPckLin..PACK_ID     ,;
                CPKCOLOR    WITH &lcPckLin..CPKCOLOR    ,;
                CPCKSIZE    WITH &lcPckLin..CPCKSIZE    ,;
                CPKVERSION  WITH &lcPckLin..CPKVERSION  ,;
                Style       WITH &lcPckLin..Style       ,;
                OrgWgh      WITH &lcPckLin..OrgStyWgh   ,;
                SzCnt       WITH SCALE.CNT,;
                cStatus     WITH "A",;
                Cart_No     WITH &lcPckLin..NCARTON
        *C123477,1  TMI [Start] Update lFlag field for range and non range packs
        IF !laCtn[&lcPckLin..NCARTON]
          REPLACE lFlag WITH .T.
          IF &lcPckLin..lRange
            laCtn[&lcPckLin..NCARTON] = .T.  && Allow to show only the first line for range packs
          ENDIF
        ENDIF
        *C123477,1  TMI [End  ] 
      ENDIF
      
      SELECT &lcCtnDtl
      REPLACE Size&lcSz   WITH &lcPckLin..cSize,;
              Br&lcSz     WITH !EMPTY(Size&lcSz) ,;
              Qty&lcSz    WITH TMPL_LIN.Qty&lcSz,;
              TOTQTY      WITH TOTQTY + Qty&lcSz,;
              Weight&lcSz WITH TMPL_LIN.WEIGHT*TMPL_LIN.Qty&lcSz,;
              TotWeight   WITH TotWeight + Weight&lcSz ,;
              cStatus     WITH IIF(&lcPckLin..Selected>0,'M',cStatus)
      
      *B123477,4  TMI [Start] if a range pack is encountered first time , add qty , wght , otherwise , add zeros
      lnPkQty = TMPL_LIN.Qty&lcSz
      lnWght  = &lcCtnDtl..Weight&lcSz
      lcPckId = &lcPckLin..PACK_ID+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion+STR(TMPL_LIN.NCARTON)
      IF ASCAN(laPckId , lcPckId ) = 0
        lnPckLn = lnPckLn + 1
        DIMENSION laPckId[lnPckLn]
        laPckId[lnPckLn] = lcPckId
      ELSE
        IF &lcPckLin..LRANGE
          lnPkQty = 0
          lnWght  = 0
        ENDIF
      ENDIF
      *B123477,4  TMI [End  ] 

      *B123477,4  TMI [Start] Accumulate qty for packs , not range packs
      *REPLACE &lcCtnHdr..TotPcs WITH &lcCtnHdr..TotPcs + TMPL_LIN.Qty&lcSz ,;
      *        &lcCtnHdr..TotWgh WITH &lcCtnHdr..TotWgh + &lcCtnDtl..Weight&lcSz
      REPLACE &lcCtnHdr..TotPcs WITH &lcCtnHdr..TotPcs + lnPkQty ,;
              &lcCtnHdr..TotWgh WITH &lcCtnHdr..TotWgh + lnWght
      *B123477,4  TMI [End  ] 
    ENDIF
  ENDFOR
ENDSCAN

SET ORDER TO &lcSvOrd IN SPCK_HDR

SELECT (lcCtnDtl)
SET RELATION TO '' INTO (lcCartonSz)
SET SKIP TO (lcCartonSz)

*-- end of lfGetTmpl.

*:**************************************************************************
*:* Name        : lfGtNtAply
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Show if some styles are not applied in cartons
*:***************************************************************************
FUNCTION lfGtNtAply
PRIVATE lnAlias,lnRecno,lnCnt
lnAlias = SELECT()
lnRecno = RECNO()
lnCnt = 1
SELECT &lcPckLin

&& 1. If the order contains styles only, when we save the template, a msg always shows up: "Some styles are not applied.." even though we selected all the styles in cartons. (check on order# 999998). 
* The cause of this is that there is record of the order with avlqty = 0 and these are not displayed and when we execute the 
* command Locate for nCarton = 0 it returns that there is lines with nCarton not applied and it show the message that some 
* styles are not applied , 


LOCATE FOR nCarton=0 AND AvlQty>0
IF FOUND()
  lnCnt = gfModalGen('INM00000B00023',.F.,.F.,.F.,'Some styles are not applied to cartons.')
ENDIF

SELECT (lnAlias)
IF BETWEEN(lnRecno,1,RECCOUNT())
  GOTO lnRecno
ENDIF

RETURN (lnCnt = 1)  
*-- end of function.

*:**************************************************************************
*:* Name        : lfLclRfrsh
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Local refresh , to solve a strange problem the lcDtlBrow browse
*:***************************************************************************
FUNCTION lfLclRfrsh
PRIVATE lcWin
lcWin = gfTempName()
DEFINE WINDOW &lcWin FROM 1,1 TO 2,2
SHOW WINDOW &lcWin REFRESH SAME
RELEASE WINDOW &lcWin

*-- end of lfLclRfrsh.

*:**************************************************************************
*:* Name        : lfvNavigate
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*!* Purpose     : Function for navigation(top,bottom,next,priveus).
*:***************************************************************************
FUNCTION lfvNavigate
PRIVATE lnSvRc,lnTop,lnBot
lnAlias=SELECT()
SELECT TMPL_HDR
lnSvRc=IIF(!EOF(),RECNO(),0)
GO BOTTOM
lnBot = RECNO()
GO TOP
lnTop = RECNO()
GOTO IIF(lnSvRc=0,1,lnSvRc)

IF lnTop = lnBot
  SHOW GET PBTOP  DISABLE
  SHOW GET PBPRVS DISABLE
  SHOW GET PBBTM  DISABLE
  SHOW GET PBNXT  DISABLE
ELSE
  lcDisEnb=IIF(RECNO()=lnTop,'DISABLE','ENABLE')
  SHOW GET PBTOP  &lcDisEnb
  SHOW GET PBPRVS &lcDisEnb

  lcDisEnb=IIF(RECNO()=lnBot,'DISABLE','ENABLE')
  SHOW GET PBBTM &lcDisEnb
  SHOW GET PBNXT &lcDisEnb    
ENDIF  

SELECT(lnalias)

*:**************************************************************************
*:* Name        : lfPackWght                                      
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Get pack weight from SPCK_LIN file
*:***************************************************************************
FUNCTION lfPackWght
PRIVATE lnSlct,lnWght
lnSlct = SELECT()
lnWght = 0

SELECT SPCK_HDR
IF SEEK('P'+ACCOUNT+Pack_Id+cPkColor+cPckSize+cPkVersion,'SPCK_LIN')
  SELECT SPCK_LIN
  SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = ;
                  'P'+SPCK_HDR.ACCOUNT+SPCK_HDR.Pack_Id+SPCK_HDR.cPkColor+SPCK_HDR.cPckSize+SPCK_HDR.cPkVersion
    =SEEK(SPCK_LIN.STYLE,'STYLE')
    lnWght = lnWght + SPCK_LIN.TOTQTY * STYLE.NSTYWEIGHT
  ENDSCAN
ENDIF
SELECT (lnSlct)
RETURN lnWght
*-- end of lfPackWght.


*:**************************************************************************
*:* Name        : lfPackQty
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Get pack QTY from SPCK_LIN file and update SPCK_HDR.NPCKQTY if it is 0
*:***************************************************************************
FUNCTION lfPackQty
PRIVATE lnSlct,lnQty,lcSvOrd
lnSlct = SELECT()
lnQty = 0

SELECT SPCK_HDR
*B123477,1  TMI [Start] Get the # of pack pcs in all cases
*IF SPCK_HDR.NPCKQTY = 0
  *B123477,1  TMI [End  ] 
  lcSvOrd = ORDER('SPCK_LIN')
  SET ORDER TO SPCK_LINVR IN SPCK_LIN
  
  IF SEEK('P'+ACCOUNT+Pack_Id+cPkColor+cPckSize+cPkVersion,'SPCK_LIN')
    SELECT SPCK_LIN
    SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = ;
                    'P'+SPCK_HDR.ACCOUNT+SPCK_HDR.Pack_Id+SPCK_HDR.cPkColor+SPCK_HDR.cPckSize+SPCK_HDR.cPkVersion
      lnQty = lnQty + SPCK_LIN.TOTQTY
    ENDSCAN
  ENDIF
  SELECT SPCK_HDR
  REPLACE NPCKQTY WITH lnQty
  SET ORDER TO &lcSvOrd IN SPCK_LIN    
*B123477,1  TMI [Start] Get the # of pack pcs in all cases
*ENDIF  
*B123477,1  TMI [End  ] 

SELECT (lnSlct)

*-- end of lfPackQty.


*:**************************************************************************
*:* Name        : lfOrdHdLck
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Lock / un-lock ordhdr file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfOrdHdLck()
*:***************************************************************************
FUNCTION lfOrdHdLck
PARAMETERS llLock
PRIVATE lnAlias,lcSvOrd,llGoOn
llGoOn = .T.
lnAlias = SELECT()
lcSvOrd = ORDER('ORDHDR')
SET ORDER TO ORDHDR IN ORDHDR
SELECT ORDHDR
=SEEK('O'+laData[2],'ORDHDR')

=gfObj_Lock( llLock )

SET ORDER TO &lcSvOrd IN ORDHDR
SELECT (lnAlias)
RETURN llGoOn
*-- end of lfOrdHdLck.

*:**************************************************************************
*:* Name        : lfvRngQCtn
*:* Developer   : Hend Ghanem (HBG)
*:* Date        : 04/04/2004
*:* Purpose     : Valid function for Qty pr Crt field (Range pack)
*:***************************************************************************
*:C122070,1
FUNCTION lfvRngQCtn

IF &lcSumPck..CtnTotQty > &lcSumPck..OTotQty
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Qty./Ctn. cannot be grater than O/Qty.')
  REPLACE &lcSumPck..CtnTotQty WITH 0
  RETURN
ENDIF

lcKey = PADR(PACK_ID,16)+cPkColor+cPckSize+cPkVersion
=SEEK(lcKey,lcTmpPck)
SELECT (lcTmpPck)
*B123477,4  TMI [Start] Update CtnQty with the total qty , no meaning here for summing qty of each size and total them in ctntotqty , since this is a range pack
REPLACE CtnQty    WITH &lcSumPck..CtnTotQty ;
        DisCtnQty WITH CtnQty ;
        FOR OQTY>0 .AND. PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcKey
RETURN      && No need for the rest of the function
*B123477,4  TMI [End  ] 

*C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
*lcStyle = &lcTmpPck..Style
*REPLACE CtnQty WITH 0 FOR PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcKey+lcStyle
lcStyle = &lcTmpPck..Style+&lcTmpPck..Dyelot
REPLACE CtnQty WITH 0 FOR PACK_ID+cPkColor+cPckSize+cPkVersion+Style+Dyelot+cSizeNo = lcKey+lcStyle
*C038675,1 [End]


lnRngCtQty = &lcSumPck..CtnTotQty
DO WHILE (lnRngCtQty > 0)
  =SEEK(lcKey+lcStyle,lcTmpPck)
  SELECT (lcTmpPck)
  *C038675,1 HBG 10/26/2004 Add Configuration field [Begin]
  *SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcKey+lcStyle
  SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+Dyelot+cSizeNo = lcKey+lcStyle
  *C038675,1 [End]
    REPLACE CtnQty WITH CtnQty + MIN(OQty,lnRngCtQty)
    lnRngCtQty = MAX(lnRngCtQty - OQty,0)
    IF lnRngCtQty = 0 
      EXIT
    ENDIF
  ENDSCAN  
ENDDO

lnRngQty =  &lcSumPck..CtnTotQty

=SEEK(lcKey,lcTmpPck)
SELECT (lcTmpPck)
SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcKey
  *B123477,1  TMI [Start] Update DisCtnQty WITH CtnQty not OQty
  *REPLACE DisCtnQty WITH OQty
  REPLACE DisCtnQty WITH CtnQty
  *B123477,1  TMI [End  ] 
ENDSCAN

SELECT (lcSumPck)

*:********************************** End of program *****************************************
