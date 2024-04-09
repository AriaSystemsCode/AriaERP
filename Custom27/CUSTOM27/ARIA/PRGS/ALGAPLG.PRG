*:****************************************************************************************************************
*: Program file  : ALPLSTG
*: Program desc. : Create Custom Autoatic Packing List for Order or PikTkt For GMA
*: For screen    : ALPLSTG
*:        System : Aria Advantage Series.
*:        Module : Allocation (AL).
*:     Developer : TMI -TAREK MOHAMED IBRAHIM
*:****************************************************************************************************************
* Important notes BY TMI :
*  1)  This program is base on "v:\custom27\gma\prgs\alplstg.prg" the manual packing list program for GMA
*      So you may face non used variables and non used functions .
*  2)  The uncomplete session code is commented and do not work now , this screen works as file screen 
*:****************************************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************************************
*: Modification:
*: B607375,1 TMI 06/23/2003 Fix the bug that wrong missed style reported in the log file
*: C122070,1 HBG 04/04/2004 Ability to identify a range pack from other standard packs on the screen
*: B037982,1 HBG 4/18/2004 Get the Carton # form the new file EDICRTSQ instead of calculating it
*: B123477,1 TMI 08/04/2004 Fix some problems of the entry # C122070
*: B127213,1 MHM 08/07/2005 Fix bug of error when we close the Auto P/L screen          
*: C131958,1 HBG 05/02/2006 Get the UCC # structure from the setup
*:****************************************************************************************************************

*-- laData      array that hold the data of the key screens and header folder
*-- laScrMode   array that hold the screen mode
*-- laKeyField  array that hold the key of the screen 
*-- laCodeInfo  array to be used in gfwCodePop function
*-- laShpVia    array that hold all shipvia codes
*-- lafoldwinds array that hold the count of used folder and it's titles
DIMENSION laData[1],laScrMode[4],laKeyField[3,4],laCodInfo[1,10],laShpVia[1],;
          lafoldwinds[3,2]
STORE SPACE(0) TO laData,laKeyField,laCodInfo,laShpVia
PRIVATE llGma , lcAlias , lcOrdr , lnRcNo , llPacked , llComp
STORE .F. TO llGma , llPacked , llComp
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

*C131958,1 HBG 05/02/2006 Check for UCC # structure setup [Begin]
llIncPLNum = .F.
lnNumOfDig = 5
*C131958,1 HBG 05/02/2006 [End]

lnLineRange=IIF(_WINDOWS,1.77,2)  && the line height for the dbl click
llClick    =.F.                   && logical variable to control the dbl click selection
lnTimelimt = 0
lnCurR = 0
lnCount = 1  && Defined to by used by the array laScObj in the spr file 
lnY = 0
lnx = 0
lcObjName = ''

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
                  lcStore,lcPckLin,lcPack_Lin,lcCtnHdr,lcStores,lcCtnDtl,lcWinKey,;
                  lcWinHdr,lcWinHdr1,lcWinHdr2,lcWinDtl,lcWinDtl1,lcWinDtl2,;
                  lcWinCtn,lcWinCtn1,lcWinCtn2,lcWinCtn3,lcfolder,lcfoldprnt,;
                  lcOldVal,lcCtnSty,lcStySz,lcLsVPck,lcScaFile,lcCartonSz,;
                  lcPrvPack, lcFree_Clr , lcTmpDetFl , lcBollsttl ,lcTmAsnShp,;
                  lcTmpPck,lcSumPck,lcTmpIdx,lcPakIndxSt,lcPakIndxLn,;
                  lcWareCode,lcScFields,lcOldValue,lcTmpUpLn

                                

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
*-- lnRelCho    Variable that showes if release the remain order lines is desired (1=release)
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
           lnStyWgh,lnCartNo,lnHdBrRec,lnDtBrRec,lnStBrRec,lnCtHdBrRe,lnCtDtBrRe,lnFrom,;
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
*-- llCanPrnLb   Hold The State Of The User Rights If He can Print Or Not.
*-- llScrPrnLb   Hold The State of the option menue print label after each carton
*-- lcSndPort    Hold the name of the printing port.
*-- llShoPckSu   Hold if we choose 
*--lcVar

STORE .T. TO llShoPckSu 
STORE .F. TO llBrowse,llEdiSys,llAsnSys,llAloModul,llEdiAcc,llPCDStat,llPalStat,llClrSel,;
             llShwOpn,llEdiopn,llB_H_Opn,llB_D_Opn,llDyelot,llMulWare,llNew,llPckCopy,llNoShow,;
             llSelOrdLi,llUnComp,llCupdate,llAnyUpd,llAnyRec, llPrint,llPakPrSiz 
STORE .F. TO llQtyArr,llWghArr

DIMENSION laMajSegs[1,1]
             
llFirstQty = .F.             
llFirstWgh = .F.

llAlowNew  = .F.             &&  Disable new button , the screen will work as transaction screen 

laDefproc[07] = .F.                && This is to Enable local delete
laDefproc[09] = .F.                && This is to prevent the global save
laDefProc[10] = .F.                && This is to prevent the global close

lcStoBrTtl = 'Stores List'         && Stores list
lcDtlBrTtl = 'Open Quantities'     && This is to hold the title of the browse screen
lcCtHdBrTt = 'Cartons'             && This is to hold the title of the browse screen
lcCtDtBrTt = 'Contents of carton'  && This is to hold the title of the browse screen

lnSessNo  = gnProgCopy             && This is to hold the no of the opened session from global variable
lcSession = SPACE(1)               && This is to hold the session id for the uncompleted session
lcUserID  = PADR(gcUser_id, 10)    && This is to hold the user id 
 
*-*laKeyField[1,1] = "laData[1]"
*-*laKeyField[1,2] =.F.
*-*laKeyField[1,3] = "Pack_Hdr"
*-*laKeyField[1,4] = 1
*-*
*-*laKeyField[2,1] = "laData[2]"
*-*laKeyField[2,2] = .F.
*-*laKeyField[2,3] = "Pack_Hdr"
*-*laKeyField[2,4] = 2
*-*
*-*laKeyField[3,1] = "laData[3]"
*-*laKeyField[3,2] = .T.
*-*laKeyField[3,3] = "Pack_Hdr"
*-*laKeyField[3,4] = 3

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

*--creat a memory file to hold the state of the menu option llUpdtPkTk 
llUpdtPkTk = .F.
IF FILE (gcDataDir+'UpPkTk.MEM')
  RESTORE FROM (gcDataDir+'UpPkTk.MEM') ADDITIVE
ENDIF

*-- This is to add laData elements to laVars array---------------
FOR lnK = 1 TO 16
  DIMENSION laVars[ALEN(laVars)+1]
  laVars[ALEN(laVars)] = "laData[" + ALLTRIM(STR(lnK,2)) + "]"
ENDFOR

llGma = .T.

IF !gfSetup()
  RETURN
ENDIF  

LAUSRFIELD = ''

lcBaseFile  = "TMPL_HDR"
lcFile_ttl  = 'Tamplates'
lcBrFields  = [Order    :H='Order#',]+;
              [Account  :H='Account',]+;
              [TOT_WGHT :H='Tot. Weight',]+;
              [TOT_PCS  :H='Tot. Pcs',]+;
              [CWARECODE:H='Warehous']

*-- This is to check if the system has EDI module , and open EdiAcc File if is true
*--- SSH
ON KEY LABEL ALT+B DO lfCtrKey

llEdiSys   =  ('AS' $ gcCmpModules)

IF llEdiSys
  
  *B802029,1 Change this line to open the EDIACPRT and EDIPH files instead of
  *          the file EDIACC because we have replaced the file EDIACC with
  *          the files EDIACPRT, EDIPH and EDIPD [Begin]
  *llEdiopn = gfOpenFile(gcDataDir+'EdiAcc',gcDataDir+'EdiAcc','SH')
  =gfOpenFile(gcDataDir + 'EDIACPRT' , gcDataDir + 'ACCFACT' , 'SH')
  =gfOpenFile(gcDataDir + 'EDIPH' , gcDataDir + 'PARTNER' , 'SH')
  *B037982,1 HBG 4/18/2004 Open the new file EDICRTSQ [Begin]
  =gfOpenFile(gcDataDir + 'EDICRTSQ' , gcDataDir + 'EDICRTSQ' , 'SH')
  *B037982,1 [End]
  
ENDIF

=gfOpenFile(gcDataDir+'WareHous','WareHous','SH')
=gfOpenFile(gcDataDir + 'SPCK_LIN' , gcDataDir + 'Spcklins' , 'SH')
=gfOpenFile(gcDataDir + 'CUSTDEPT' , gcDataDir + 'CUSTDEPT' , 'SH')

DECLARE laDRltFld[1,2]
laDRltFld[1,1] = 'DIVLNAME  '
laDRltFld[1,2] = 'lcDivLName'
lcDivLName = ''

DECLARE laVRltFld[1,2]
laVRltFld[1,1] = 'CARRIERCOD'
laVRltFld[1,2] = 'lcCarCod'
lcCarCod = ''

*-- C101735,1 redimension the array [Begin]
*C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
*DIMENSION laSetup[7,2]
DIMENSION laSetup[8,2]
*C131958,1 HBG 05/02/2006 [End]


laSetUp[1,1] = 'M_DYELOT'
laSetUp[2,1] = 'M_CANAFTER'

laSetUp[3,1] = 'M_WareHouse'
laSetUp[4,1] = 'XMANUFID'

laSetUp[5,1] = 'M_FORCEALO'
laSetUp[6,1] = 'M_BOLSTUTS'

laSetUp[7,1] = 'M_PAKPRSIZ'

*C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
laSetup[8,1] = 'M_UCCBSDON'
*C131958,1 HBG 05/02/2006 [End]


=gfGetMemVar(@laSetUp,gcAct_Comp)

llAlwForce = lfSuppForc()

llDyelot   = ALLTRIM(laSetUp[1,2]) = 'Y'
lnOrdMdPr  = laSetUp[2,2]

*-- Get Status For The M_PRNLABEL And llCanPrnLb [Begin]
llMulWare  = ALLTRIM(laSetUp[3,2]) = 'Y'
lcManufId  = ALLTRIM(laSetUp[4,2])
llCanPrnLb = gfUserPriv('AL','ALPLIST','PRNPACKING')

*Add flag to hold the valu of the setup (M_BOLSTUTS) [Begin]
llBolnever = (ALLTRIM(laSetup[6,2]) = "N")

*Get the setting of generate B.O.L Automatic [begin]
llBolAutom = (ALLTRIM(laSetup[6,2]) = "A")

*Add flag to hold the value of the setup (M_PAKPRSIZ) [Begin]
llPakPrSiz = laSetup[7,2]


*C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
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
*C131958,1 HBG 05/02/2006 [End]

llAsnSys   = ('AS' $ gcCmpModules)
llAloModul = ('AL' $ gcCmpModules)

lcProgID = PADR("AUTO_PL",10)

lnFolderCEnd = 102.40
lnFolderREnd = 1.99
lnNoFld      = 3
lnFoldSize   = 51.25

lcwfoldchng  = '=lfActFolder()'  && function to control shows after change the folder

*Take into consideration styles with exteded size scale while printning labels
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
  lcWinHdr1  = gfTempName()
  lcWinHdr2  = gfTempName()
  lcWinDtl   = gfTempName()
  lcWinDtl1  = gfTempName()
  lcWinDtl2  = gfTempName()    
  lcWinCtn   = gfTempName()
  lcWinCtn1  = gfTempName()
  lcWinCtn2  = gfTempName()
  lcWinCtn3  = gfTempName()

  lcAsnLabel = gfTempName()
  lcTmAsnShp = gfTempName()
  lcTmpDetFl = gftempName()  
    
  lcStyPic   = gfItemMask("PI")
  lcStyTtl   = gfItemMask("HI")
  lnStyleWid = LEN(lcStyPic)  
  
  lcScFields = "Pack_No, Order, Piktkt, Account, Store, Note, ShipVia,;
                Tot_Wght, Tot_Cart, Tot_Pcs, Sp_Inst1, Sp_Inst2, LStandCtn,;
                CToStorCn, CPkChCode, CPkDsCode "
  SELECT Pack_Hdr
  SCATTER FIELDS &lcScFields TO laData BLANK
  lcWareCode = ''
  lcBol      = ''
  lcOldValue = ""
  lcPckLin   = gfTempName()  
  lcPack_Lin = gfTempName()  
  lcCtnHdr   = gfTempName()
  lcStores   = gfTempName()
  lcCtnDtl   = gfTempName()
  lcScaFile  = gfTempName()
  lcCartonSz = gfTempName()
  
  lcTmpPck  = gfTempName()
  lcTmpIdx  = gfTempName()

  *B037982,1 HBG Get the Carton # form the new file EDICRTSQ instead of calculating it
  lcTmpUpLn = gfTempName()
  *B037982,1 HBG (End)

  lcSumPck  = gfTempName()
  IF llGma
    lcPakIndxSt = gfTempName()
    lcPakIndxLn = gfTempName()
  ENDIF

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
  =lfEvalSegs()
ENDIF

IF llCanPrnLb
*-- B603496,1 check for llcanprnlb only [End  ]

 =gfOpenFile(gcDataDir+'Asn_Ship','Asn_Ship','SH')
  IF !USED(lcTmAsnShp)
    COPY STRUCTURE TO (gcWorkDir+lcTmAsnShp)
    =gfOpenFile(gcWorkDir+lcTmAsnShp,'','EX')
    INDEX ON STR(CART_NO,6) TAG (lcTmAsnShp)
  ENDIF 
  =gfOpenFile(gcSysHome+'SYCASNLB','ASNlbl','SH')    
  llDetLabel = SEEK("XX1" + "H" , "SYCASNLB") .AND. SEEK("XX1" + "L" , "SYCASNLB")
  
  IF llDetLabel
    *-- Flag to know if UPC module is installed or not
    llUPCInst  = ('UP' $ gcCmpModules)
    *-- Flag to know if the user want to print detailed shipping label for all cartons or not
    lcDetLbAll = ""
    
    IF llUPCInst
      =gfOpenFile(gcDataDir + 'STYLEUPC' , gcDataDir + 'STYLEUPC' , 'SH')
    ENDIF
  ENDIF
  
ENDIF

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

SELECT Pack_Lin
SET ORDER TO PackStyle

SELECT OrdHdr
SET FILTER TO cOrdType = "O"
SET ORDER TO OrdHdr

SELECT PikTkt
SET ORDER TO TAG 'OrdPik' IN PikTkt
SELECT Pack_Hdr
SET ORDER TO Pack_Hdr
SET RELATION TO 'O'+Order INTO OrdHdr

SET ORDER TO TAG Style IN Style
  
SELECT OrdLine
SET FILTER TO cOrdType+order+store+style+STR(lineno,6) = "O"
SET ORDER TO OrdLinSt
IF EMPTY(SET("RELATION"))
  SET RELATION TO 'S'+Scale INTO Scale ADDITIVE
  SET RELATION TO Style INTO Style ADDITIVE
ENDIF

SELECT Pack_Lin
IF EMPTY(SET("RELATION"))  
  SET RELATION TO 'O'+laData[2]+laData[5]+Style INTO OrdLine ADDITIVE
ENDIF

=gfOpenFile(gcDataDir+"Pikline",gcDataDir+'Pikline','SH')
SET RELATION TO 'O'+Pikline.order+Pikline.Store+Pikline.Style+STR(Pikline.lineno,6) INTO ORDLINE ADDITIVE
SELECT PikTkt
SET RELATION TO Piktkt.piktkt INTO Pikline ADDITIVE


DO (gcScrDir + '\ALGAPLG.SPR')      && calling the screen Mainvlo

ON KEY LABEL LEFTMOUSE

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
  
  IF USED(lcPckLin)
    USE IN (lcPckLin)
    IF USED(lcTmpPck)
      USE IN (lcTmpPck)
      *: B127213,1 MHM 08/07/2005 Fix bug of error when we close the Auto P/L screen [Start]         
      ERASE (gcWorkDir+lcTmpPck+".DBF")
      ERASE (gcWorkDir+lcTmpPck+".CDX")
      *: B127213,1 MHM 08/07/2005 [End]         
    ENDIF
  ENDIF
  
  
  ERASE (gcWorkDir+lcPckLin+".DBF")
  ERASE (gcWorkDir+lcPckLin+".CDX")


  IF USED(lcPack_Lin)  
    USE IN (lcPack_Lin)
  ENDIF
  ERASE (gcWorkDir+lcPack_Lin+".DBF")
  ERASE (gcWorkDir+lcPack_Lin+".CDX")

  **: B127213,1 MHM 08/07/2005 Fix bug of error when we close the Auto P/L screen [Start]         
  IF USED(lcStores)  
    USE IN (lcStores)
  ENDIF
  ERASE (gcWorkDir+lcStores+".DBF")
  ERASE (gcWorkDir+lcStores+".CDX")
  **: B127213,1 MHM 08/07/2005  [End]         


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
 
  **: B127213,1 MHM 08/07/2005 Fix bug of error when we close the Auto P/L screen [Start]         
  ERASE (gcWorkDir+lcCartonSz+".DBF")
  ERASE (gcWorkDir+lcCartonSz+".CDX")
  **: B127213,1 MHM 08/07/2005  [End]         

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
 
  SAVE TO (gcDataDir+'UpPkTk.MEM') ALL LIKE llUpdtPkTk
  
ENDIF  

*!*************************************************************
*! Name      : lpShow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
SHOW GET pbBrws DISABLE
SHOW GET pbSelAlSts ENABLE
SHOW GET pbSlNoSto ENABLE

SHOW GET pbcptask DISABLE
SHOW GET pbCpCalndr DISABLE
SHOW GET pbcpCalc DISABLE
SHOW GET pbDlt DISABLE


DO CASE
  *-- Select mode
  CASE laScrMode[1]
  
    lcScrMode  = "S"
*-*    IF lfChkUnCmS()
*-*      = lfChngFolder(lnActFolder)
*-*      RETURN
*-*    ENDIF
    
    *-- Creating the temp. files
    llNothing = lfCrtUnCmp()
    STORE SPACE(0) TO lcCusPo,lcDept,lcShVDesc,lcPkChCode,lcPkDsCode,;
                      lcStoreDsc,lcDistCtr,lcBol,lcPackNo,lcOrderNo,lcStore,;
                      lcOldVal,lcCtnSty,lcStySz,lcLsVPck,lcPrvPack
    lcSel = "This"

    STORE "DISABLE" TO lcStoStat,lcCtnStat,lcPikStat,lcShipStat,lcInstStat,;
                       lcCartNoSt,lcTotWghSt,lcPalNoSt,lcCtnPalSt,lcCtnStySt,lcStySzSt,;
                       lcStyQtySt,lcStyWghSt,lcAddHdrSt,lcRemHdrSt,lcAddDtlSt,;
                       lcRemDtlSt    
    STORE "ENABLE" TO lcPackStat,lcOrdStat,lcAccStat
    STORE 0 TO lnLastFold,lnShpVia,lnRelCho,lnTotWgh,lnPalNo,lnCtnPal,lnStyQty,lnStyWgh,;
               lnCartNo,lnHdBrRec,lnDtBrRec,lnCtHdBrRe,lnCtDtBrRe,lnFrom,lnTo,;
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

    _CUROBJ = OBJNUM(laData[2])
    
    DIMENSION laStyTrgt(1)
    DIMENSION laClrTrgt(1)
    STORE SPACE(0) TO laStyTrgt,laClrTrgt
    SELECT (lcPckLin)
    SET FILTER TO
    
    SHOW GET pbBrws ENABLE
    SET ORDER TO TAG ORDLINST IN ORDLINE
    IF SEEK('O'+laData[2],'OrdLine')
      SELECT OrdLine
      SCAN REST WHILE cOrdType+Order+Store+Style+STR(lineno,6) = 'O'+laData[2]
        llGoOn = gfObj_Lock(.F.)
      ENDSCAN
    ENDIF
    
  *-- View mode
  CASE laScrMode[2]
    
    lcScrMode  = "V"
    IF INLIST(lnPrvMode,3,4)
      *-- Release order lock if prev mode is add or edit
      =lfChkOrdLok(.F.) 
    ENDIF
    
    lnSvAls = SELECT()
    SELECT &lcStores
    REPLACE cSelect WITH ' ' ALL
    SELECT (lnSvAls)

    lnPackWgh = laData[08]
    lnPackCtn = laData[09]
    lnPackQty = laData[10]

    lcWareCode = Pack_Hdr.cWareCode 
    lcBol      = Pack_Hdr.Bill_Ladg
    
    lcCusPo    = IIF(OrdHdr.MultiPo,IIF(SEEK('O'+ladata[2]+laData[5],'OrdLine'),OrdLine.CustPo,SPACE(10)),OrdHdr.CustPo)

    *B802029,1 Change these lines to use the EDIACPRT and EDIPH files
    *          instead of the file EDIACC because we have replaced the file
    *          EDIACC with the files EDIACPRT, EDIPH and EDIPD [Begin]
    *llEdiAcc   = llEdiSys .AND. SEEK(laData[4],'EdiAcc')
    *STORE llEdiSys .AND. llEdiAcc .AND. EdiAcc.lPkChrDes  TO llPCDStat
    *STORE llEdiSys .AND. llEdiAcc .AND. EdiAcc.lnShipnPlt TO llPalStat

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
    lnCtnTyp = IIF(Pack_Hdr.LStandCtn,1,2)

    lnDrctTo = IIF(Pack_Hdr.CToStorCn='C',2,1)
    
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
    
    SHOW GET pbSlct ENABLE
    SHOW GET pbEdt ENABLE
    SHOW GET pbBrws ENABLE
    
    SHOW GETS WINDOW (lcWinHdr2) DISABLE ONLY

    DIMENSION laStyTrgt(1)
    DIMENSION laClrTrgt(1)
    STORE SPACE(0) TO laStyTrgt,laClrTrgt
    SELECT (lcPckLin)
    SET FILTER TO

    IF !(laData[2]==lcPrvPack) .OR. lnPrvMode = 3
      =lfvNewPack()
      lcPrvPack = laData[2]
    ENDIF
    
  *-- Edit mode
  CASE laScrMode[3]
   
    SHOW GET pbSav ENABLE
    SHOW GET pbCls,1  PROMPT "BMPS\Cancel.BMP"      

    lcScrMode = "E"
    *-- check if any pack is being created now on the same order in any
    *-- other session
    
    *-- Lock order in edit mode
    llGoOn = lfChkOrdLok(.T.)
    IF llGoOn
*-*      IF !llUnComp
*-*        llNoThing = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)    
*-*      ENDIF
    ELSE
      laScrMode = .F.
      laScrMode[2] = .T.
      SHOW GETS
    ENDIF
    
  *-- Add mode
  CASE laScrMode[4]
    lcScrMode  = "A"

*-*    IF llUnComp AND llNew
*-*      *-- This part is to fill ShipVia list to refresh lnShpVia field
*-*      *-- in case detecting uncomp. session in Add Mode and the lnSpVia
*-*      *-- had been changed befor illegal termination
*-*      lnOrjShp = lnShpVia
*-*      IF lnShpVia <> 1
*-*        lnShpVia = 1
*-*        = gfwCodePop(@laCodInfo, "SHIPVIA", "L")
*-*      ENDIF
*-*      lnShpVia = lnOrjShp
*-*      SHOW GET lnShpVia
*-*    ENDIF

    IF !llNew
      STORE "ENABLE" TO lcPikStat,lcOrdStat,lcAccStat
      STORE "DISABLE" TO lcPackStat

      STORE 0 TO lnShpVia,lnPackQty,lnPackWgh,lnPackCtn
      STORE 2 TO lnCtnTpy
      STORE 1 TO lnDrctTo
      = gfwCodePop(@laCodInfo, "SHIPVIA", "N")
      
      _CUROBJ = OBJNUM(laData[2])
    ENDIF

    SHOW GET pbSav ENABLE
    SHOW GET pbCls,1  PROMPT "BMPS\Cancel.BMP"      

    *-- Lock in Add mode
    IF !lfChkOrdLok(.T.)
      laScrMode = .F.
      laScrMode[1] = .T.
      SHOW GETS
    ENDIF
      
ENDCASE

*-- Save previous mode
lnPrvMode = ASCAN(laScrMode,.T.)

*--Navigation buttons
=lfvNavigate()

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

SELECT (lnCurAlias)
*--

*!*************************************************************
*! Name      : lfPakNoBrw
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
lcCurTag = Order('Pack_Hdr')

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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
  *--Check if the customer is GMA or not

    PRIVATE lcOrder
    lcOrder = OrdHdr.Order
    llGma = .T.
    lcAlias = SELECT(0)
    SELECT Pack_Hdr
    lcTmpKey = pack_no
    lcOrdr = SET('ORDER')
    SET ORDER TO TAG OrderPck
    llPacked = SEEK(OrdHdr.Order+OrdHdr.Store)
    SET ORDER TO &lcOrdr
    =SEEK(lcTmpKey)
    SELECT OrdHdr
    =SEEK('O'+lcOrder)

    SELECT (lcAlias)

  DO CASE

    CASE OrdHdr.Status = 'C' AND !llGma
      *-- This order is completed, cannot pack.
      *-- OK
      = gfModalGen("INM44050B00000","Dialog","completed")
      laData[2] = SPACE(6)
      SHOW GET laData[2]
      
    *---           and the orde packed.[Begin]
    CASE OrdHdr.Status = 'C' AND llGma AND llPacked
      =gfModalGen("INM000000B00000","DIALOG",'','',;
      'This order is completed and packed, cannot pack.')  
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
      *-- This order is canceled, cannot pack.
      *-- OK
      = gfModalGen("INM44050B00000","Dialog","bulk")  
      laData[2] = SPACE(6)
      SHOW GET laData[2]
    
    OTHERWISE
      
      IF !SEEK(laData[2],'TMPL_LIN')
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No tamplate saved for this order, can not proceed.')
        SET ORDER TO lcTag IN OrdHdr
        laData[2] = '     '
        _CUROBJ = OBJNUM(laData[2])
        SELECT (lnCurAlias)
        RETURN        
      ENDIF
            
      llComp = (OrdHdr.Status = 'C' AND llGma AND !llPacked)
      
      lcPrvPack = laData[2]
      
      =lfGetData()
      
      SET ORDER TO ORDPIK IN PIKTKT
      
      *-- llHaSPik variable that indicates if this order has pikTkt or not
      IF SEEK(laData[2],'PikTkt')
        lnAlias = SELECT(0)
        SELECT PikTkt

        LOCATE REST WHILE Order = laData[2] FOR Status$'PO'
        
        IF FOUND()
          llHasPik = .T.
        ELSE
          llHasPik = .F.
        ENDIF
        SELECT (lnAlias)
      ELSE
        llHasPik = .F.
      ENDIF

      =lfvNewPack() &&
      lcPckHdrOrd = order('pack_hdr')
      SET ORDER TO TAG ORDERPCK IN PACK_HDR
      laScrMode = .F.
      IF !SEEK(laData[2],'PACK_HDR')
        laScrMode[4] = .T.   && Add mode
      ELSE
        laScrMode[2] = .T.   && View mode
      ENDIF
      SET ORDER TO TAG &lcPckHdrOrd IN PACK_HDR      
      DO lpShow 
  ENDCASE
ENDIF

SET ORDER TO lcTag IN OrdHdr
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Name      : lfPikTktBrow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
IF !laScrMode[2]
*  llGoOn = lfChkOrdLok()
ENDIF  

IF ALLTRIM(OrdHdr.ShipVia)='*' AND SEEK('S'+laData[4]+laData[5],'Customer')
  laData[7]  = Customer.ShipVia
  laCodInfo[1,07] = "customer"                && Alternative File (For default val.)
  laCodInfo[1,08] = "customer"                && Use this index for the Alternative file.
  laCodInfo[1,09] = "'S'+laData[4]+laData[5]"         && Seek this expretion.
  laCodInfo[1,10] = "ShipVia"               && Alternative Field Name
  = gfwCodePop(@laCodInfo, "SHIPVIA", "T")
ENDIF

IF lnActFolder = 3
  = lfWinCtnSt()
ENDIF

lcTag = Order('Pack_Hdr')
 
*-- Creating the temp. files
llNothing = lfCrtUnCmp()
lcPckHdrOrd = ORDER('PACK_HDR')
SET ORDER TO TAG ORDERPCK IN PACK_HDR

*-- Get Order stores
=lfGtOrdSto()   

PRIVATE lcTmplOrd
lcTmplOrd = ORDER('TMPL_LIN')
SET ORDER TO TAG TMPL_LINS IN TMPL_LIN

SELECT &lcStores
GO TOP
SCAN    
  WAIT WINDOW NOWAIT 'Collecting data for store '+&lcStores..STORE + ' D.C '+ &lcStores..DIST_CTR
  laData[1] = &lcStores..PACK_NO
  laData[3] = &lcStores..PIKTKT
  laData[5] = &lcStores..STORE
  =lfvSelOrdL()
ENDSCAN

*-- Remove the wait window 
WAIT CLEAR  

SET ORDER TO TAG &lcPckHdrOrd IN PACK_HDR
SET ORDER TO &lcTmplOrd IN TMPL_LIN

GO TOP IN &lcStores
IF !WEXIST(lcStoBrTtl)
  =lfStrsBr()
ELSE
  =lfwStrsBr()
ENDIF
=lfRefresh(lcWinHdr)

SELECT (lcPckLin)
GO TOP
=RLOCK(lcPckLin) 
UNLOCk IN (lcPckLin)

IF !(lcScaFile $ SET('RELATION'))
  SET RELATION TO '' INTO (lcScaFile) ADDITIVE
  SET SKIP TO (lcScaFile)
ENDIF  

IF !USED(lcTmpPck)
  IF gfGetMemVar('M_ORDSTUTS',gcAct_Comp) = 'L'
    USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPakIndxLn)
  ELSE
    USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPakIndxSt)
  ENDIF
ENDIF

IF !WEXIST(lcDtlBrTtl) AND lnActFolder = 2
  = lfDtlBrow()
ELSE
  IF lnActFolder = 2
    = lfwDtlBrow()
  ENDIF
ENDIF

SELECT(lcCtnHdr)
= RLOCK(lcCtnHdr) 
UNLOCk IN (lcCtnHdr)
GO TOP

SELECT(lcCtnDtl)
UNLOCk IN (lcCtnDtl)

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

= lfWinHdrSt()
  
*!*************************************************************
*! Name      : lfWinHdrSt
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Name      : lfOldValue
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
  lcPalNoSt  = IIF(llPalStat AND (laScrMode[3] OR llNew) AND lnActFolder = 3,"ENABLE","DISABLE")
  SHOW GET lnPalNo &lcPalNoSt
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
= lfRefresh(lcWinHdr)
SHOW GETS WINDOW (lcWinKey)  ONLY
SHOW GETS WINDOW (lcWinHdr)  ONLY      

*!*************************************************************
*! Name      : lfvSelOrdL
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

STORE 0 TO lnContinue,lnCtnQty,lnCtnWgh,lnI,lnJ,lnStyOrdLin,lnRemPQty

SELECT &lcCtnHdr
SET FILTER TO STORE = &lcStores..STORE
GO TOP

SELECT &lcCtnDtl
SET FILTER TO STORE = &lcStores..STORE
GO TOP

llAnyUpd = .F.

*lnPrvMode = ASCAN(laScrMode,.T.)
lnAlias = ALIAS()

SET ORDER TO Ordlinst IN OrdLine
llNoThing = SEEK('O'+laData[2]+laData[5],'OrdLine')

STORE SPACE(0) TO lcStyle,lcStyDesc
lcExp = IIF(!EMPTY(laData[3]),"WHILE cOrdType+Order+Store='O'+laData[2]+laData[5] FOR PikTkt=laData[3] AND Picked",;
                              "WHILE cOrdType+Order+Store='O'+laData[2]+laData[5]")
*--- llFrmPikLn --> .T. ----> colect data from pikline , .F. --> from ordline

llFrmPikLn = .F.
IF !EMPTY(laData[3])
  SELECT PIKTKT
  SET ORDER TO PIKTKT
  IF SEEK(laData[3]) AND Status = 'C'
     llFrmPikLn = .T.
     lcExp = "WHILE PikTkt+Order+Store=laData[3]+laData[2]+laData[5] FOR Picked "
  ENDIF
ENDIF
*-- move the pointer to frist record 
IF llFrmPikLn
  SELECT "PikLine"
  =SEEK(laData[3]+laData[2])
ELSE
  SELECT "OrdLine"
  =SEEK('O'+laData[2]+laData[5])
ENDIF

SCAN REST &lcExp
  SELECT (lcPckLin)
  APPEND BLANK
  REPLACE Style      WITH IIF(llFrmPikLn,PikLine.Style,OrdLine.Style),;
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
          OrdQty8    WITH IIF(llFrmPikLn,PikLine.Qty8,OrdLine.Qty8),;
          STORE      WITH laData[5]          

  REPLACE AvlTotQty  WITH (AvlQty1+AvlQty2+AvlQty3+AvlQty4+AvlQty5+AvlQty6+AvlQty7+AvlQty8),;
          OrdTotQty  WITH (OrdQty1+OrdQty2+OrdQty3+OrdQty4+OrdQty5+OrdQty6+OrdQty7+OrdQty8)

  REPLACE OQty1  WITH MAX(0,AvlQty1-Ordline.nPck1),;
          OQty2  WITH MAX(0,AvlQty2-Ordline.nPck2),;
          OQty3  WITH MAX(0,AvlQty3-Ordline.nPck3),;
          OQty4  WITH MAX(0,AvlQty4-Ordline.nPck4),;
          OQty5  WITH MAX(0,AvlQty5-Ordline.nPck5),;
          OQty6  WITH MAX(0,AvlQty6-Ordline.nPck6),;
          OQty7  WITH MAX(0,AvlQty7-Ordline.nPck7),;
          OQty8  WITH MAX(0,AvlQty8-Ordline.nPck8),;
          OTotQty WITH MAX(0,OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+OQty8),;
          PQty1  WITH OrdLine.nPck1,;
          PQty2  WITH OrdLine.nPck2,;
          PQty3  WITH OrdLine.nPck3,;
          PQty4  WITH OrdLine.nPck4,;
          PQty5  WITH OrdLine.nPck5,;
          PQty6  WITH OrdLine.nPck6,;
          PQty7  WITH OrdLine.nPck7,;
          PQty8  WITH OrdLine.nPck8
  REPLACE PTotQty WITH (PQty1+PQty2+PQty3+PQty4+PQty5+PQty6+PQty7+PQty8),;
          OTotQty WITH (OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+oQty8)
  *C122070,1 HBG 04/04/2004 Update new open Qty fileds to handle Range Packs[Begin]
  REPLACE UpOqty1   WITH MAX(0,AvlQty1-Ordline.nPck1),;
          UpOqty2   WITH MAX(0,AvlQty2-Ordline.nPck2),;
          UpOqty3   WITH MAX(0,AvlQty3-Ordline.nPck3),;
          UpOqty4   WITH MAX(0,AvlQty4-Ordline.nPck4),;
          UpOqty5   WITH MAX(0,AvlQty5-Ordline.nPck5),;
          UpOqty6   WITH MAX(0,AvlQty6-Ordline.nPck6),;
          UpOqty7   WITH MAX(0,AvlQty7-Ordline.nPck7),;
          UpOqty8   WITH MAX(0,AvlQty8-Ordline.nPck8),;
          UOTotQty WITH MAX(0,UpOqty1+UpOqty2+UpOqty3+UpOqty4+UpOqty5+UpOqty6+UpOqty7+UpOqty8)
  *C122070,1 [End]          

  REPLACE PWgh1      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty1,;
          PWgh2      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty2,;
          PWgh3      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty3,;
          PWgh4      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty4,;
          PWgh5      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty5,;
          PWgh6      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty6,;
          PWgh7      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty7,;
          PWgh8      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty8

  REPLACE PTotWgh    WITH (PWgh1+PWgh2+PWgh3+PWgh4+PWgh5+PWgh6+PWgh7+PWgh8)
          
  REPLACE PACK_ID    WITH IIF(llFrmPikLn , PikLine.PACK_ID    , OrdLine.PACK_ID),;
          cPkColor   WITH IIF(llFrmPikLn , PikLine.cPkColor   , OrdLine.cPkColor),;
          cPckSize   WITH IIF(llFrmPikLn , PikLine.cPckSize   , OrdLine.cPckSize),;
          cPkVersion WITH IIF(llFrmPikLn , PikLine.cPkVersion    , OrdLine.cPkVersion)

  *C122070,1 HBG 04/04/2004 Update the new filed "lRange" with its value from ORdline file to know if this pack is rang or not[Begin]
  REPLACE lRange     WITH IIF(llFrmPikLn,PikLine.lRange, OrdLine.lRange),;
          llPack     WITH !EMPTY(PACK_ID)
  *C122070,1 [End]
  
  IF SEEK(laData[2]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'TMPL_LIN')
    REPLACE CtnQty1   WITH TMPL_LIN.QTY1,;
            CtnQty2   WITH TMPL_LIN.QTY2,;
            CtnQty3   WITH TMPL_LIN.QTY3,;
            CtnQty4   WITH TMPL_LIN.QTY4,;
            CtnQty5   WITH TMPL_LIN.QTY5,;
            CtnQty6   WITH TMPL_LIN.QTY6,;
            CtnQty7   WITH TMPL_LIN.QTY7,;
            CtnQty8   WITH TMPL_LIN.QTY8,;
            CTNTOTQTY WITH CtnQty1+CtnQty2+CtnQty3+CtnQty4+ ;
                           CtnQty5+CtnQty6+CtnQty7+CtnQty8
    *-- Show in the detail folder the unt/wgt not total unit for each size
    REPLACE Weight1   WITH TMPL_LIN.Weight,;
            Weight2   WITH TMPL_LIN.Weight,;
            Weight3   WITH TMPL_LIN.Weight,;
            Weight4   WITH TMPL_LIN.Weight,;
            Weight5   WITH TMPL_LIN.Weight,;
            Weight6   WITH TMPL_LIN.Weight,;
            Weight7   WITH TMPL_LIN.Weight,;
            Weight8   WITH TMPL_LIN.Weight
  ENDIF
  *B123477,1  TMI [Start] Get weight of first style for Range packs
  IF lRange
    =SEEK(laData[2]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
    REPLACE Weight1   WITH TMPL_LIN.Weight,;
            Weight2   WITH TMPL_LIN.Weight,;
            Weight3   WITH TMPL_LIN.Weight,;
            Weight4   WITH TMPL_LIN.Weight,;
            Weight5   WITH TMPL_LIN.Weight,;
            Weight6   WITH TMPL_LIN.Weight,;
            Weight7   WITH TMPL_LIN.Weight,;
            Weight8   WITH TMPL_LIN.Weight
  ENDIF
  *B123477,1  TMI [End  ] 
          
  llAnyRec = .T.          
ENDSCAN

*C122070,1 HBG 04/04/2004 Update CtnQty fileds as it'll be saved in PAck_lin file for Range Packs[Begin]
SELECT (lcPckLin)
lcPack_Id = ""
SCAN FOR lRange
  *B123477,3  TMI [Start] put initial value for "lnPckedQty"
  lnPckedQty = 0
  *B123477,3  TMI [End  ] 

  IF lcPack_Id <> PACK_ID
    lcPack_Id = PACK_ID
    *B123477,1  TMI [Start] for range packs seek only the pack ID in tmpl_lin file since only the first style is saved there
    *IF SEEK(laData[2]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'TMPL_LIN')
    IF SEEK(laData[2]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
      *B123477,1  TMI [End  ] 
      lnPckedQty = TMPL_LIN.TotQty
      lnWeight   = TMPL_LIN.Weight
    ENDIF
  ENDIF
  *B123477,3  TMI [Start] if lnPckedQty > 0 , then this line is applied to in the tamplete file
  IF lnPckedQty > 0
    *B123477,3  TMI [End  ] 
    =SEEK(Style,'STYLE') AND SEEK('S'+Style.Scale,'SCALE')
    IF lnPckedQty < OTOTQTY
      lnPkQty = lnPckedQty/SCALE.Cnt
      lnMod   = MOD(lnPkQty,1)
      llEven = .F.
      FOR lnI = 1 TO SCALE.Cnt
        lcI = STR(lnI,1)
        REPLACE CtnQty&lcI WITH IIF(!llEven,lnPkQty-lnMod,lnPkQty+lnMod)
        llEven = !llEven
      ENDFOR  
    ELSE
      FOR lnI = 1 TO SCALE.Cnt
        lcI = STR(lnI,1)
        REPLACE CtnQty&lcI WITH OQty&lcI
      ENDFOR  
    ENDIF
    REPLACE CTNTOTQTY WITH CtnQty1+CtnQty2+CtnQty3+CtnQty4+CtnQty5+CtnQty6+CtnQty7+CtnQty8
    *B123477,3  TMI [Start] check that lnPckedQty is > 0
  ENDIF
  *B123477,3  TMI [End  ] 
ENDSCAN
*C122070,1 [End]

SELECT Pack_Lin
lcTag = ORDER()
SET ORDER TO PackStyle

IF !EMPTY(laData[1])  
   IF SEEK(laData[1],'Pack_Lin')
    SCAN REST WHILE pack_no = laData[1]
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
      lcSearExp = "'O'+laData[2]+laData[5]+Pack_Lin.Style+STR(Pack_Lin.nOrdLineNo,6)"
      IF SEEK(&lcSearExp.,'Ordline')
        SELECT OrdLine
        lnPackLin = 0
        SCAN REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = ;
                          &lcSearExp. 
          lnStyOrdLin = OrdLine.LineNo
          
          llNoThing = SEEK('O'+laData[2]+laData[5]+Pack_Lin.Style+STR(lnStyOrdLin,6),'Ordline')
          SET ORDER TO (lcPakIndxSt) IN (lcPckLin)
          llNoThing = SEEK(Pack_ID+cPkColor+cPckSize +cPkVersion+Pack_Lin.Style+STR(lnStyOrdLin,6),lcPckLin)
  
          IF (EMPTY(laData[3]) AND !OrdLine.Picked) OR (!EMPTY(laData[3]) AND IIF(llFrmPikLn,!OrdLine.Picked,OrdLine.Picked))
            SELECT (lcCtnDtl)
            lnPackLin = lnPackLin+1
            APPEND BLANK
            REPLACE Style      WITH Pack_Lin.Style,;
                    STORE      WITH laData[5],;
                    SzCnt      WITH &lcPckLin..SzCnt,;
                    cStatus    WITH "A",;
                    nOrdLineNo WITH lnStyOrdLin,;
                    PackLineNo WITH lnPackLin,;
                    Cart_No    WITH Pack_Lin.No_Cart,;
                    Qty1       WITH Pack_Lin.Qty1,;
                    Qty2       WITH Pack_Lin.Qty2,;
                    Qty3       WITH Pack_Lin.Qty3,;
                    Qty4       WITH Pack_Lin.Qty4,;
                    Qty5       WITH Pack_Lin.Qty5,;
                    Qty6       WITH Pack_Lin.Qty6,;
                    Qty7       WITH Pack_Lin.Qty7,;
                    Qty8       WITH Pack_Lin.Qty8,;
                    TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
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
            REPLACE TotWeight  WITH (Weight1+Weight2+Weight3+Weight4+Weight5+Weight6+Weight7+Weight8)      
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
            ELSE
              IF (&lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                  &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                  &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                  &lcCtnDtl..Qty7+&lcCtnDtl..Qty8) > 0

                lnMaxCtn = MAX(lnMaxCtn,Pack_Lin.No_Cart)

                INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,TotPcs,TotWgh,Empty, STORE);
                                VALUES (Pack_Lin.No_Cart,Pack_Lin.NPltNo,;
                                        &lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                                        &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                                        &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                                        &lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
                                        &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
                                        &lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
                                        &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
                                        &lcCtnDtl..Weight7+&lcCtnDtl..Weight8,'N', laData[5])
 
              ENDIF
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN
  ENDIF
ELSE
  STORE 1 TO lnFrom,lnTo
ENDIF

SET ORDER TO lcTag IN Pack_Lin

*!*************************************************************
*! Name      : lfChkStyQty
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
       Do you want to modify the orderd quantity?
    *-- <Yes>,<No>
    lnChoice = gfModalGen("QRM44034B00006","Dialog",Pack_Lin.Style + '/' + EVAL(lcSizeFld))
  ENDIF  

  lnReturn = IIF(llStyFound,&lcOQtyFld,IIF(lnChoice<>2,&lcRemVar,&lcOQtyFld))

  SELECT (lcPckLin)
  REPLACE &lcPQtyFld   WITH IIF(llStyFound,&lcOQtyFld,IIF(lnChoice<>2,&lcRemVar,&lcOrdQtyFld)),;
          &lcOQtyFld   WITH IIF(&lcOrdQtyFld-&lcPQtyFld<0,0,&lcOrdQtyFld-&lcPQtyFld),;
          &lcPWghFld   WITH (Pack_lin.Weight/Pack_Lin.TotQty)* &lcPQtyFld,;
          &lcOrdQtyFld WITH IIF(llStyFound,&lcOrdQtyFld,IIF(lnChoice<>1,&lcOrdQtyFld,&lcRemVar))

  &lcRemVar = &lcRemVar - lnReturn
  
ENDIF

SELECT(lnCurAlias)

RETURN lnReturn


*!*************************************************************
*! Name      : lfvCtnSty
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
                  Br1        WITH .F.            ,;
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

IF llComp
  lcForExp = "FOR IIF(llShwOpn, StyQty.SzCode <= STR(PackLines.SzCnt,1), StyQty.SzCode <= STR(PackLines.SzCnt,1))"
ELSE
  lcForExp = "FOR IIF(llShwOpn,EVAL('OQty'+StyQty.SzCode)>0 AND EVAL('AvlQty'+StyQty.SzCode)>0 AND StyQty.SzCode <= STR(PackLines.SzCnt,1),EVAL('AvlQty'+StyQty.SzCode)>0 AND StyQty.SzCode <= STR(PackLines.SzCnt,1))"
ENDIF
           
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Name      : lfvStyCtnQty
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

IF LASTKEY() = 13 AND !(lnStyQty == lcOldVal)
  lnCurAlias = SELECT(0)
  
  lnUnitWgh = IIF(EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode)>0,;
                  EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode)/;
                  EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode),0)
  
  *B606394,1 HBG 18/08/2002 Change the index to fix bug of not updating OQty. [Begin]
  lcPrvOrd = ORDER(lcPckLin)
  SET ORDER TO lcPckLin IN (lcPckLin)
  *lcExpGma3 = "SEEK(&lcTmpPck..Pack_ID+&lcTmpPck..cPkColor+&lcTmpPck..cPckSize+&lcTmpPck..cPkVersion+ &lcCtnDtl..Style + STR(&lcCtnDtl..nOrdLineNo,6),lcPckLin)"
  lcExpGma3 = "SEEK(&lcCtnDtl..Style + STR(&lcCtnDtl..nOrdLineNo,6),lcPckLin)"
  *B606394,1 [End]

  IF &lcExpGma3

    SKIP VAL(&lcCartonSz..SzCode) - 1 IN (lcPckLin)
    IF EVAL(lcPcklin+'.OrdQty'+&lcScaFile..SzCode) < ;
       EVAL(lcPckLin+'.PQty'+&lcScaFile..SzCode) - ;
       EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode) + lnStyQty
      *-- Packed quantity for Style/Size x/x exceeds ordered quantity.;
          Do you want to modify the orderd quantity?
      *-- <Yes>,<No>
      lnContinue = gfModalGen("QRM44034B00006","Dialog",&lcPckLin..Style + '/' + EVAL(lcPckLin+'.cSize'+&lcScaFile..SzCode))
      
    ENDIF

    IF lnContinue <> 2
      
      SELECT(lcPckLin)
      lcOQtyFld = 'OQty' + &lcScaFile..SzCode
      lcPQtyFld = 'PQty' + &lcScaFile..SzCode
      lcPWghFld = 'PWgh' + &lcScaFile..SzCode

      lcFld = 'OrdQty' + &lcScaFile..SzCode
      PRIVATE lcOrgOrd
      lcOrgOrd = 'OrgOrd' + &lcScaFile..SzCode
      REPLACE &lcFld WITH MAX(EVAL(lcPckLin+'.PQty'+&lcScaFile..SzCode) - ;
                              EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode) + ;
                              lnStyQty ,;
                              &lcOrgOrd)

      
      *B606394,1 HBG 18/08/2002 Update the total Qty's after deleting [Begin]
      *REPLACE &lcOQtyFld WITH MAX(EVAL('OQty'+&lcScaFile..SzCode) + EVAL('PQty'+&lcScaFile..SzCode) - lnStyQty,0),;
              &lcPQtyFld WITH EVAL('PQty'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Qty'   +&lcCartonSz..SzCode) + lnStyQty,;
              &lcPWghFld WITH EVAL('PWgh'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode) + lnStyQty*lnUnitWgh
      
      REPLACE &lcOQtyFld WITH MAX(EVAL('OQty'+&lcScaFile..SzCode) + EVAL(lcCtnDtl+'.Qty'+&lcCartonSz..SzCode) - lnStyQty,0),;
              &lcPQtyFld WITH EVAL('PQty'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Qty'    +&lcCartonSz..SzCode) + lnStyQty,;
              &lcPWghFld WITH EVAL('PWgh'+&lcScaFile..SzCode) - EVAL(lcCtnDtl+'.Weight' +&lcCartonSz..SzCode) + lnStyQty*lnUnitWgh
      
      REPLACE PTotQty WITH (PQty1+PQty2+PQty3+PQty4+PQty5+PQty6+PQty7+PQty8),;
              OTotQty WITH (OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+OQty8),;
              PTotWgh WITH (PWgh1+PWgh2+PWgh3+PWgh4+PWgh5+PWgh6+PWgh7+PWgh8)
     *B606394,1 [End]
              
      = RLOCK(lcPckLin) 
      UNLOCk IN (lcPckLin)
    ENDIF
  ENDIF
  *B606394,1 HBG 18/08/2002 Return the index after finish updating [Begin] 
  SET ORDER TO (lcPrvOrd) IN (lcPckLin)
  *B606394,1 [End]
  IF SEEK(STR(&lcCtnDtl..Cart_No,4),lcCtnHdr) AND lnContinue <> 2
    lnPackWgh = lnPackWgh - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode) + lnStyQty*lnUnitWgh
    lnPackQty = lnPackQty - EVAL(lcCtnDtl+'.Qty'   +&lcCartonSz..SzCode) + lnStyQty
    
     SELECT(lcCtnHdr)
     REPLACE TotPcs WITH TotPcs - EVAL(lcCtnDtl+'.Qty'   +&lcCartonSz..SzCode) + lnStyQty,;
             Empty  WITH IIF(TotPcs > 0 , 'N' , 'Y'),;
             TotWgh WITH TotWgh - EVAL(lcCtnDtl+'.Weight'+&lcCartonSz..SzCode) + lnStyQty*lnUnitWgh
 
            
    = RLOCK(lcCtnHdr) 
    UNLOCk IN (lcCtnHdr)
  ENDIF

  IF lnContinue <> 2
    SELECT(lcCtnDtl)
    lcQtyFld = 'Qty'   +&lcCartonSz..SzCode
    lcWghFld = 'Weight'+&lcCartonSz..SzCode
    REPLACE &lcQtyFld  WITH lnStyQty,;
            &lcWghFld  WITH EVAL('Qty'+&lcCartonSz..SzCode)*lnUnitWgh,;
            cStatus WITH 'M'
    = RLOCK(lcCtnDtl) 
    UNLOCk IN (lcCtnDtl)
  ENDIF

  = lfwCtnDtlBr()

  SELECT(lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lfvCtDtWgh
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
    SET RELATION TO STR(&lcCtnHdr..Cart_No,4)+Style+STR(nOrdLineNo,6) INTO (lcCtnDtl)
    REPLACE REST FOR Style+STR(nOrdLineNo,6) = lcStyle+STR(lnOrdLin,6) ;
                     PWgh1 WITH PWgh1 - &lcCtnDtl..Weight1 + &lcCtnDtl..Qty1*lnUnitWgh,;
                     PWgh2 WITH PWgh2 - &lcCtnDtl..Weight2 + &lcCtnDtl..Qty2*lnUnitWgh,;
                     PWgh3 WITH PWgh3 - &lcCtnDtl..Weight3 + &lcCtnDtl..Qty3*lnUnitWgh,;
                     PWgh4 WITH PWgh4 - &lcCtnDtl..Weight4 + &lcCtnDtl..Qty4*lnUnitWgh,;
                     PWgh5 WITH PWgh5 - &lcCtnDtl..Weight5 + &lcCtnDtl..Qty5*lnUnitWgh,;
                     PWgh6 WITH PWgh6 - &lcCtnDtl..Weight6 + &lcCtnDtl..Qty6*lnUnitWgh,;
                     PWgh7 WITH PWgh7 - &lcCtnDtl..Weight7 + &lcCtnDtl..Qty7*lnUnitWgh,;
                     PWgh8 WITH PWgh8 - &lcCtnDtl..Weight8 + &lcCtnDtl..Qty8*lnUnitWgh,;
                     PTotWgh WITH (PWgh1+PWgh2+PWgh3+PWgh4+PWgh5+PWgh6+PWgh7+PWgh8)

    SET RELATION TO
    SET RELATION TO '' INTO (lcScaFile)
    SET SKIP TO (lcScaFile)


    IF lnStyRec <= RECCOUNT(lcPckLin) 
      GOTO lnStyRec
      SKIP lnSzeRec-1 
    ENDIF
    = RLOCK(lcPckLin)
    UNLOCk IN (lcPckLin)
  ENDIF

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
    SET RELATION TO STR(&lcCtnHdr..Cart_No,4)+Style+STR(nOrdLineNo,6) INTO (lcCtnDtl)
    lnStyRec = RECNO(lcPckLin)
    lnSzeRec = RECNO(lcScaFile)

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
    SET RELATION TO '' INTO (lcScaFile)
    SET SKIP TO (lcScaFile)
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Name      : lfActFolder
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Activate folder when Change.
*!*************************************************************
FUNCTION lfActFolder

STORE IIF((laScrMode[3] OR llNew) AND lnActFolder = 1,"ENABLE","DISABLE") TO lcInstStat,lcShipStat

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
    SHOW GETS WINDOW (lcWinCtn1)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCtn2)  DISABLE ONLY
    
    IF !WEXIST(lcStoBrTtl)
      = lfStrsBr()
    ELSE
      = lfwStrsBr()
    ENDIF
   
    = lfRefresh(lcWinHdr)   

  CASE lnActFolder = 2
    SHOW GETS WINDOW (lcWinHdr)   DISABLE ONLY
    SHOW GETS WINDOW (lcWinCtn1)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCtn2)  DISABLE ONLY

    = lfSelStatus()
    IF USED(lcTmpPck)
      SELECT &lcTmpPck
      SET FILTER TO STORE = &lcStores..STORE
      GO TOP
    ENDIF

    IF !WEXIST(lcDtlBrTtl)
      = lfDtlBrow()
    ELSE
      = lfwDtlBrow()
    ENDIF

  CASE lnActFolder = 3
    
    IF USED(lcCtnHdr)
      SELECT (lcCtnHdr)
      SET FILTER TO STORE = &lcStores..STORE
      GO TOP      
    ENDIF
    
    SHOW GETS WINDOW (lcWinHdr)   DISABLE ONLY
    SHOW GETS WINDOW (lcWinDtl1)  DISABLE ONLY
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

IF INLIST(WONTOP(),lcStoBrTtl,lcDtlBrTtl,lcCtHdBrTt,lcCtDtBrTt)
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrTab
  ON KEY LABEL BACKTAB DO lfBrBack
  IF WONTOP() = lcStoBrTtl
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
  CASE lnActFolder = 1 AND WONTOP() = lcStoBrTtl
    ACTIVATE WINDOW (lcWinHdr1)
    _CUROBJ = OBJNUM(lnFrom)

  CASE lnActFolder = 2 AND WONTOP() = lcDtlBrTtl
    _CUROBJ = OBJNUM(lnFrom)

  CASE lnActFolder = 3 AND WONTOP() = lcCtHdBrTt
  _CUROBJ = OBJNUM(pbHdrNew)

  CASE lnActFolder = 3 AND WONTOP() = lcCtDtBrTt
    _CUROBJ = OBJNUM(pbDtlNew)
ENDCASE    

*!*************************************************************
*! Name      : lfBrBack
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

IF !USED(lcTmpPck)
  IF gfGetMemVar('M_ORDSTUTS',gcAct_Comp) = 'L'
    USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPakIndxLn)
  ELSE
    USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPakIndxSt)
  ENDIF
ENDIF

lcTemFile = IIF(!llShoPckSu,lcSumPck,lcTmpPck)
SELECT (lcTemFile)
*LOCATE
IF EMPTY(SET('RELATION')) AND llShoPckSu
  SET RELATION TO '' INTO (lcScaFile) ADDITIVE
  SET SKIP TO (lcScaFile)
ENDIF  

lnDtBrRec = RECNO()
lnDumDtlR = RECNO(lcScaFile)

IF llShoPckSu
  SELECT (lcTmpPck)
  *B606394,1 HBG 18/08/2002 Change the browse fields [Begin]
  *BROWSE FIELDS cMarker =IIF(RECNO()=lnDtBrRec AND RECNO(lcScaFile) = lnDumDtlR,'>',' '):H= ' ':R:1:W=.F.,;
                Selector=EVAL('cSelect'+&lcScaFile..SzCode) :H = ' ':R:1:W=.F.,;
                pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :30:R,;
                StyCode =Style:H = lcStyTtl    :25:R,;
                Size    =EVAL('cSize' +&lcScaFile..SzCode):H = 'Size'  :7 :R,;
                OpenQty =EVAL('OQty'  +&lcScaFile..SzCode):H = 'O.Qty.':6 :R,;
                CartQty =EVAL('CtnQty'+&lcScaFile..SzCode):H = 'Qty.\Ctn':10:R,;
                Weight  =StyWgh:H = 'Wgh.\Unt':10:R,;
                PackQty =EVAL('PTotQty'):H = 'P.Qty.':6 :R,;
                PackWgh =EVAL('PTotWgh'):H = 'P.Wgh.':6 :R ;
           FOR IIF(llShwOpn,EVAL('OQty'+&lcScaFile..SzCode)>0 AND EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcTmpPck..SzCnt,1),;
                   EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcTmpPck..SzCnt,1));
           SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
           TITLE(lcDtlBrTtl) WHEN lfwDtlBrow() ;
           VALID :F lfvDtlBrow()    ;
           WINDOW (lcWinDtl1) IN WINDOW (lcWinDtl)

  *C122070,1 HBG 04/04/2004 Modify the browse fileds in summary mode to display information about this pack if it is Range or not[Begin]
  *lcBrFlds = "cMarker =IIF(RECNO()=lnDtBrRec AND RECNO(lcScaFile) = lnDumDtlR,'>',' '):H= ' ':R:1:W=.F.,"+;
             "Selector=EVAL('cSelect'+&lcScaFile..SzCode) :H = ' ':R:1:W=.F.,"+;
             "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :30:R,"+;
             "StyCode =Style:H = lcStyTtl    :25:R,"+;
             "Size    =EVAL('cSize' +&lcScaFile..SzCode):H = 'Size'  :7 :R,"+;
             "OpenQty =EVAL('OQty'  +&lcScaFile..SzCode):H = 'O.Qty.':6 :R,"+;
             "CartQty =EVAL('CtnQty'+&lcScaFile..SzCode):H = 'Qty.\Ctn':10:R,"+;
             "Weight  =EVAL('Weight'+&lcScaFile..SzCode):H = 'Wgh.\Unt':10:R,"+;
             "PackQty =EVAL('PQty'+&lcScaFile..SzCode):H = 'P.Qty.':6 :R,"+;
             "PackWgh =EVAL('PWgh'+&lcScaFile..SzCode):H = 'P.Wgh.':6 :R "           
  lcBrFlds = "cMarker =IIF(RECNO()=lnDtBrRec AND RECNO(lcScaFile) = lnDumDtlR,'>',' '):H= ' ':R:1:W=.F.,"+;
             "Selector=EVAL('cSelect'+&lcScaFile..SzCode) :H = ' ':R:1:W=.F.,"+;
             "pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :30:R,"+;
             "cRange  =IIF(llPack,IIF(lRange,'YES','NO'),'') :H = 'Range' :7 :R,"+;
             "StyCode =Style:H = lcStyTtl    :25:R,"+;
             "Size    =EVAL('cSize' +&lcScaFile..SzCode):H = 'Size'  :7 :R,"+;
             "OpenQty =EVAL('OQty'  +&lcScaFile..SzCode):H = 'O.Qty.':6 :R,"+;
             "CartQty =EVAL('CtnQty'+&lcScaFile..SzCode):H = 'Qty.\Ctn':10:R,"+;
             "Weight  =EVAL('Weight'+&lcScaFile..SzCode):H = 'Wgh.\Unt':10:R,"+;
             "PackQty =EVAL('PQty'+&lcScaFile..SzCode):H = 'P.Qty.':6 :R,"+;
             "PackWgh =EVAL('PWgh'+&lcScaFile..SzCode):H = 'P.Wgh.':6 :R "           
  *C122070,1 [End]  
  
  BROWSE FIELDS &lcBrFlds;
           FOR IIF(llShwOpn,EVAL('OQty'+&lcScaFile..SzCode)>0 AND EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcTmpPck..SzCnt,1),;
                   EVAL('AvlQty'+&lcScaFile..SzCode)>0 AND &lcScaFile..SzCode <= STR(&lcTmpPck..SzCnt,1));
           SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
           TITLE(lcDtlBrTtl) WHEN lfwDtlBrow() ;
           VALID :F lfvDtlBrow()    ;
           WINDOW (lcWinDtl1) IN WINDOW (lcWinDtl)
  *B606394,1 [End]
ELSE
  SELECT(lcSumPck)
  BROWSE FIELDS cMarker =IIF(RECNO()=lnDtBrRec AND RECNO(lcScaFile) = lnDumDtlR,'>',' '):H= ' ':R:1:W=.F.,;
                Selector=EVAL('cSelect') :H = ' ':R:1:W=.F.,;
                pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :30:R,;
                OpenQty =OTotQty   :H = 'O.Qty.':6 :R,;
                CartQty =CtnTotQty :H = 'Qty.\Ctn':10:R,;
                PackQty =PTotQty   :H = 'P.Qty.':6 :R,;
                PackWgh =PTotWgh   :H = 'P.Wgh.':6 :R ;
           FOR &lcScaFile..SzCode <= STR(&lcTmpPck..SzCnt,1);
           SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
           TITLE(lcDtlBrTtl) WHEN lfwDtlBrow() ;
           VALID :F lfvDtlBrow()    ;
           WINDOW (lcWinDtl1) IN WINDOW (lcWinDtl)

ENDIF
= lfwDtlBrow()

*!*************************************************************
*! Name      : lfwDtlBrow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

IF !USED(lcTmpPck)
  IF !USED(lcTmpPck)
    IF gfGetMemVar('M_ORDSTUTS',gcAct_Comp) = 'L'
      USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPakIndxLn)
    ELSE
      USE (gcWorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (lcPakIndxSt)
    ENDIF
  ENDIF
ENDIF

lcTemFile = IIF(!llShoPckSu,lcSumPck,lcTmpPck)
SELECT (lcTemFile)
lnDtBrRec = RECNO()
IF lnActFolder = 2
  lnBrCtnQty = EVALUATE(lcTmpPck+'.CtnQty' + &lcScaFile..SzCode)
  IF llShoPckSu
    lnBrUntWgh = EVALUATE(lcTmpPck+'.StyWgh')
  ENDIF  
  SHOW WINDOW (lcDtlBrTtl) REFRESH SAME
ENDIF
SELECT (lcTemFile)

IF llShoPckSu
  lcSel  = IIF(EMPTY(EVAL('cSelect'+&lcScaFile..SzCode)),"This","UnSel")
ELSE
  lcSel  = IIF(EMPTY(EVAL('cSelect')),"This","UnSel")
ENDIF

SHOW GET pbSel,1 PROMPT lcSel &lcSelStt
=lfSelStatus()
IF llQtyArr OR llWghArr
  CLEAR TYPEAHEAD
  KEYBOARD "{RIGHTARROW}{LEFTARROW}" CLEAR PLAIN
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
*-- to control showing of Select by pack
IF !llShoPckSu
  SHOW GET pbSelPack DISABLE
  SHOW GET pbSelSty DISABLE
ELSE
  SHOW GET pbSelPack ENABLE
  SHOW GET pbSelSty ENABLE
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvDtlBrow
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

lnCurAlias = SELECT(0)
lcFldNam = 'CtnTotQty'
lcFldNam = 'CtnQty'+&lcScaFile..SzCode

lcSize = &lcScaFile..SzCode

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
=SEEK(lcSize,lcScaFile)

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
lcSize = &lcScaFile..SzCode
*B605661,1 [End]
IF !EMPTY(EVAL(lcTmpPck+'.cSelect'+&lcScaFile..SzCode)) AND lnBrUntWgh > 0
  IF llShoPckSu

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
=SEEK(lcSize,lcScaFile)
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

SHOW GET lnCtnPal

*!*************************************************************
*! Name      : lfvPalNo
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
  UNLOCk IN (lcCtnHdr)
  lnMaxPal = MAX(lnMaxPal,&lcCtnHdr..Pal_No)
  lnMinPal = Min(IIF(lnMinPal=0,&lcCtnHdr..Pal_No,lnMinPal),&lcCtnHdr..Pal_No)
ENDIF

SELECT(lnCurAlias)  

*!*************************************************************
*! Name      : lfvSel
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

SELECT (lcTmpPck)

lnCurRec = RECNO()

lnSizeRec = VAL(&lcScaFile..SzCode)

lcSelFld    = 'cSelect'+&lcScaFile..SzCode
lcCtnQtyFld = 'CtnQty'+&lcScaFile..SzCode
lcOQtyFld   = 'OQty'+&lcScaFile..SzCode


lcAllStat = "DISABLE"
lcNonStat = "DISABLE"
llcUpate = .T.
SET RELATION TO
=lfUpdate()


DO CASE
  CASE _CUROBJ = OBJNUM(pbSel)
    IF llShoPckSu
      IF EMPTY(&lcSelFld) AND LPicked AND EMPTY(laData[3])
        *-- This order line has been picked, can not be selected.
        **    *-- OK
        = gfModalGen("INM44041B00000","Dialog")  
      ENDIF

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

      *-- Select the index of the pack_id in case GMA
     IF llGma
        IF EMPTY(&lcSelFld)
          IF gfGetMemVar('M_ORDSTUTS',gcAct_Comp) = 'L'
            SET ORDER TO TAG (lcTmpIdx) IN (lcTmpPck)
          ELSE
            SET ORDER TO TAG (lcPckLin) IN (lcTmpPck)
          ENDIF
        ENDIF
      ENDIF

      IF EMPTY(&lcSelFld) AND SEEK(lcUnSelSty,lcTmpPck)
      *-- C102281 AAN [End].
        *-- C102281 AAN Change the alias [Begin].
        *SELECT(lcPckLin)
        *lnCurRec = RECNO(lcPckLin)
        SELECT(lcTmpPck)
        *B606394,1 HBG 18/08/2002 Fix bug of always return to the first line in the browse after apply[Begin]
        *lnCurRec = RECNO(lcTmpPck)
        *B606394,1 [End]
        *-- C102281 AAN [End].
        REPLACE REST FOR Style+STR(nOrdLineNo,6) = lcUnSelSty;
                         StyWgh WITH OrgStyWgh
  
        *-- Select the index of the Pack_id in case GMA
        IF llGma
          IF gfGetMemVar('M_ORDSTUTS',gcAct_Comp) = 'L'
            SET ORDER TO TAG (lcPakIndxLn) IN (lcTmpPck)
          ELSE
            SET ORDER TO TAG (lcPakIndxSt) IN (lcTmpPck)
          ENDIF
        ENDIF

      ENDIF
    ELSE

      lcPack_Id = IIF(&lcSumPck..llPack,LEFT(&lcSumPck..Pack_Id,16),SPACE(16)) + &lcSumPck..cpkcolor + &lcSumPck..cpcksize + &lcSumPck..cPkVersion +IIF(&lcSumPck..llPack,'',&lcSumPck..Pack_Id)
      lcRecNoIs=RECNO()
      
      IF !(&lcSumPck..llPack)
        SET ORDER TO TAG (lcPakIndxSt) IN (lcTmpPck)
      ELSE
        SET ORDER TO TAG (lcPakIndxLn) IN (lcTmpPck)
      ENDIF
      
      LOCATE 
      =SEEK(lcPack_Id)
      *B606394,1 HBG 18/08/2002 Variable to Update QTY/CRT when Select in summarize file [Begin]
      lnCtnTQty = 0
      *B606394,1 [End]
      SCAN REST WHILE lcPack_Id= IIF(&lcSumPck..llPack,LEFT(Pack_Id,16),SPACE(16)) + cpkcolor + cpcksize + cPkVersion +IIF(&lcSumPck..llPack,'',Style)
        IF &lcSumPck..cSelect = '»'
          REPLACE   cSelect1 WITH '',;
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
                    CtnTotQty  WITH 0 ,;
                    StyWgh   WITH OrgStyWgh,;
                    Selected WITH 0
        ELSE
          REPLACE cSelect1 WITH IIF(IIF(llShwOpn,AvlQty1>0 AND OQty1>0,AvlQty1>0),IIF(cSelect1='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect1),;
                  cSelect2 WITH IIF(IIF(llShwOpn,AvlQty2>0 AND OQty2>0,AvlQty2>0),IIF(cSelect2='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect2),;
                  cSelect3 WITH IIF(IIF(llShwOpn,AvlQty3>0 AND OQty3>0,AvlQty3>0),IIF(cSelect3='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect3),;
                  cSelect4 WITH IIF(IIF(llShwOpn,AvlQty4>0 AND OQty4>0,AvlQty4>0),IIF(cSelect4='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect4),;
                  cSelect5 WITH IIF(IIF(llShwOpn,AvlQty5>0 AND OQty5>0,AvlQty5>0),IIF(cSelect5='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect5),;
                  cSelect6 WITH IIF(IIF(llShwOpn,AvlQty6>0 AND OQty6>0,AvlQty6>0),IIF(cSelect6='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect6),;
                  cSelect7 WITH IIF(IIF(llShwOpn,AvlQty7>0 AND OQty7>0,AvlQty7>0),IIF(cSelect7='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect7),;
                  cSelect8 WITH IIF(IIF(llShwOpn,AvlQty8>0 AND OQty8>0,AvlQty8>0),IIF(cSelect8='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect8)
        
          *REPLACE cSelect WITH IIF(IIF(llShwOpn,AvlTotQty>0 AND OTotQty>0,AvlTotQty>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),'')
          REPLACE CtnQty1  WITH IIF(cSelect1='»',IIF((lnTo-lnFrom+1)>0,INT(OQty1/(lnTo-lnFrom+1)),0),0),;
                  CtnQty2  WITH IIF(cSelect2='»',IIF((lnTo-lnFrom+1)>0,INT(OQty2/(lnTo-lnFrom+1)),0),0),;
                  CtnQty3  WITH IIF(cSelect3='»',IIF((lnTo-lnFrom+1)>0,INT(OQty3/(lnTo-lnFrom+1)),0),0),;
                  CtnQty4  WITH IIF(cSelect4='»',IIF((lnTo-lnFrom+1)>0,INT(OQty4/(lnTo-lnFrom+1)),0),0),;
                  CtnQty5  WITH IIF(cSelect5='»',IIF((lnTo-lnFrom+1)>0,INT(OQty5/(lnTo-lnFrom+1)),0),0),;
                  CtnQty6  WITH IIF(cSelect6='»',IIF((lnTo-lnFrom+1)>0,INT(OQty6/(lnTo-lnFrom+1)),0),0),;
                  CtnQty7  WITH IIF(cSelect7='»',IIF((lnTo-lnFrom+1)>0,INT(OQty7/(lnTo-lnFrom+1)),0),0),;
                  CtnQty8  WITH IIF(cSelect8='»',IIF((lnTo-lnFrom+1)>0,INT(OQty8/(lnTo-lnFrom+1)),0),0)
         REPLACE CtnTotQty WITH (CtnQty1+CtnQty2+CtnQty3+CtnQty4+CtnQty5+CtnQty6+CtnQty7+CtnQty8)
         REPLACE Selected WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
                                       cSelect3='»' OR cSelect4='»' OR ;
                                       cSelect5='»' OR cSelect6='»' OR ;
                                       cSelect7='»' OR cSelect8='»',   ;
                                       1,0)
        ENDIF                          
        *B606394,1 HBG 18/08/2002 Collect QTY/CRT to Update it when Select in summarize file [Begin]
        lnCtnTQty = lnCtnTQty + &lcTmpPck..CtnTotQty 
        *B606394,1 [End]

      ENDSCAN  
      SELECT (lcSumPck)
      IF &lcSumPck..cSelect = '»'
        REPLACE &lcSumPck..cSelect    WITH '',;
                &lcSumPck..CtnTotQty  WITH 0,;
                &lcSumPck..PTotQty    WITH 0
      ELSE
        *B606394,1 HBG 18/08/2002 Update QTY/CRT when Select in summarize file [Begin]
        *REPLACE &lcSumPck..cSelect    WITH '»',;
                &lcSumPck..CtnTotQty  WITH CtnTotQty
        REPLACE &lcSumPck..cSelect    WITH '»',;
                &lcSumPck..CtnTotQty  WITH lnCtnTQty 
        *B606394,1 [End]
      ENDIF
      SELECT(lcTmpPck)

    ENDIF


  CASE _CUROBJ = OBJNUM(pbSelAll)
    
      SELECT (lcSumPck)
      LOCATE
      SCAN  
        lcPack_Id = IIF(&lcSumPck..llPack,LEFT(&lcSumPck..Pack_Id,16),SPACE(16)) + &lcSumPck..cpkcolor + &lcSumPck..cpcksize + &lcSumPck..cPkVersion +IIF(&lcSumPck..llPack,'',&lcSumPck..Pack_Id)
        
        IF !(&lcSumPck..llPack)
          SET ORDER TO TAG (lcPakIndxSt) IN (lcTmpPck)
        ELSE
          SET ORDER TO TAG (lcPakIndxLn) IN (lcTmpPck)
        ENDIF
 
        SELECT (lcTmpPck)
        lcRecNoIs=RECNO()
        LOCATE 
        =SEEK(lcPack_Id)
        SCAN REST WHILE lcPack_Id= Pack_Id + cpkcolor + cpcksize + cPkVersion +IIF(&lcSumPck..llPack,'',Style)
          REPLACE cSelect1 WITH IIF(IIF(llShwOpn,AvlQty1>0 AND OQty1>0,AvlQty1>0),IIF(cSelect1='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect1),;
                  cSelect2 WITH IIF(IIF(llShwOpn,AvlQty2>0 AND OQty2>0,AvlQty2>0),IIF(cSelect2='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect2),;
                  cSelect3 WITH IIF(IIF(llShwOpn,AvlQty3>0 AND OQty3>0,AvlQty3>0),IIF(cSelect3='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect3),;
                  cSelect4 WITH IIF(IIF(llShwOpn,AvlQty4>0 AND OQty4>0,AvlQty4>0),IIF(cSelect4='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect4),;
                  cSelect5 WITH IIF(IIF(llShwOpn,AvlQty5>0 AND OQty5>0,AvlQty5>0),IIF(cSelect5='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect5),;
                  cSelect6 WITH IIF(IIF(llShwOpn,AvlQty6>0 AND OQty6>0,AvlQty6>0),IIF(cSelect6='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect6),;
                  cSelect7 WITH IIF(IIF(llShwOpn,AvlQty7>0 AND OQty7>0,AvlQty7>0),IIF(cSelect7='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect7),;
                  cSelect8 WITH IIF(IIF(llShwOpn,AvlQty8>0 AND OQty8>0,AvlQty8>0),IIF(cSelect8='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect8)
        
          REPLACE CtnQty1  WITH IIF(cSelect1='»',IIF((lnTo-lnFrom+1)>0,INT(OQty1/(lnTo-lnFrom+1)),0),0),;
                  CtnQty2  WITH IIF(cSelect2='»',IIF((lnTo-lnFrom+1)>0,INT(OQty2/(lnTo-lnFrom+1)),0),0),;
                  CtnQty3  WITH IIF(cSelect3='»',IIF((lnTo-lnFrom+1)>0,INT(OQty3/(lnTo-lnFrom+1)),0),0),;
                  CtnQty4  WITH IIF(cSelect4='»',IIF((lnTo-lnFrom+1)>0,INT(OQty4/(lnTo-lnFrom+1)),0),0),;
                  CtnQty5  WITH IIF(cSelect5='»',IIF((lnTo-lnFrom+1)>0,INT(OQty5/(lnTo-lnFrom+1)),0),0),;
                  CtnQty6  WITH IIF(cSelect6='»',IIF((lnTo-lnFrom+1)>0,INT(OQty6/(lnTo-lnFrom+1)),0),0),;
                  CtnQty7  WITH IIF(cSelect7='»',IIF((lnTo-lnFrom+1)>0,INT(OQty7/(lnTo-lnFrom+1)),0),0),;
                  CtnQty8  WITH IIF(cSelect8='»',IIF((lnTo-lnFrom+1)>0,INT(OQty8/(lnTo-lnFrom+1)),0),0)
          REPLACE CtnTotQty WITH (CtnQty1+CtnQty2+CtnQty3+CtnQty4+CtnQty5+CtnQty6+CtnQty7+CtnQty8)
          REPLACE Selected  WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
                                     cSelect3='»' OR cSelect4='»' OR ;
                                     cSelect5='»' OR cSelect6='»' OR ;
                                     cSelect7='»' OR cSelect8='»',   ;
                                     1,0)
          *B606394,1 HBG 18/08/2002 Update QTY/CRT when Select in summarize file [Begin]                            
          *REPLACE &lcSumPck..CtnTotQty WITH &lcSumPck..CtnTotQty  + CtnTotQty
          SELECT (lcSumPck)
          REPLACE &lcSumPck..CtnTotQty WITH &lcSumPck..CtnTotQty  + &lcTmpPck..CtnTotQty
          SELECT (lcTmpPck)
          *B606394,1 HBG 18/08/2002 Update QTY/CRT when Select in summarize file [Begin]
          
        ENDSCAN  
      ENDSCAN

      LOCATE
      REPLACE ALL  &lcSumPck..cSelect    WITH '»'
      
      SELECT (lcTmpPck)
      IF lcRecNoIs <= RECCOUNT() 
        GOTO lcRecNoIs
      ENDIF  
    *ENDIF
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
    SELECT(lcSumPck)
    REPLACE ALL  &lcSumPck..cSelect    WITH '',;
                 &lcSumPck..CtnTotQty  WITH 0,;
                 &lcSumPck..PTotQty    WITH 0
                 
    SELECT(lcTmpPck)

  CASE _CUROBJ = OBJNUM(pbInvert)
    IF llShoPckSu
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
      REPLACE CtnTotQty WITH (CtnQty1+CtnQty2+CtnQty3+CtnQty4+CtnQty5+CtnQty6+CtnQty7+CtnQty8)
                
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

    ELSE
      SELECT (lcSumPck)
      LOCATE
      SCAN  
        lcPack_Id = IIF(&lcSumPck..llPack,LEFT(&lcSumPck..Pack_Id,16),SPACE(16)) + &lcSumPck..cpkcolor + &lcSumPck..cpcksize + &lcSumPck..cPkVersion +IIF(&lcSumPck..llPack,'',&lcSumPck..Pack_Id)
        lcRecNoIs=RECNO()
        IF !(&lcSumPck..llPack)
          SET ORDER TO TAG (lcPakIndxSt) IN (lcTmpPck)
        ELSE
          SET ORDER TO TAG (lcPakIndxLn) IN (lcTmpPck)
        ENDIF
 
        SELECT (lcTmpPck)
        LOCATE 
        =SEEK(lcPack_Id)


        SELECT (lcSumPck)
        IF &lcSumPck..cSelect = '»'
          REPLACE &lcSumPck..cSelect    WITH '',;
                  &lcSumPck..CtnTotQty  WITH 0
        ELSE

        REPLACE &lcSumPck..cSelect    WITH '»',;
                &lcSumPck..CtnTotQty  WITH CtnTotQty
        ENDIF
        SELECT(lcTmpPck)


        SCAN REST WHILE lcPack_Id= Pack_Id + cpkcolor + cpcksize + cPkVersion +IIF(&lcSumPck..llPack,'',Style)
        
          REPLACE cSelect1 WITH IIF(IIF(llShwOpn,AvlQty1>0 AND OQty1>0,AvlQty1>0),IIF(cSelect1='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect1),;
                  cSelect2 WITH IIF(IIF(llShwOpn,AvlQty2>0 AND OQty2>0,AvlQty2>0),IIF(cSelect2='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect2),;
                  cSelect3 WITH IIF(IIF(llShwOpn,AvlQty3>0 AND OQty3>0,AvlQty3>0),IIF(cSelect3='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect3),;
                  cSelect4 WITH IIF(IIF(llShwOpn,AvlQty4>0 AND OQty4>0,AvlQty4>0),IIF(cSelect4='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect4),;
                  cSelect5 WITH IIF(IIF(llShwOpn,AvlQty5>0 AND OQty5>0,AvlQty5>0),IIF(cSelect5='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect5),;
                  cSelect6 WITH IIF(IIF(llShwOpn,AvlQty6>0 AND OQty6>0,AvlQty6>0),IIF(cSelect6='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect6),;
                  cSelect7 WITH IIF(IIF(llShwOpn,AvlQty7>0 AND OQty7>0,AvlQty7>0),IIF(cSelect7='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect7),;
                  cSelect8 WITH IIF(IIF(llShwOpn,AvlQty8>0 AND OQty8>0,AvlQty8>0),IIF(cSelect8='»','',IIF(LPicked AND EMPTY(laData[3]),'','»')),cSelect8)

          REPLACE CtnQty1  WITH IIF(cSelect1='»',IIF((lnTo-lnFrom+1)>0,INT(OQty1/(lnTo-lnFrom+1)),0),0),;
                  CtnQty2  WITH IIF(cSelect2='»',IIF((lnTo-lnFrom+1)>0,INT(OQty2/(lnTo-lnFrom+1)),0),0),;
                  CtnQty3  WITH IIF(cSelect3='»',IIF((lnTo-lnFrom+1)>0,INT(OQty3/(lnTo-lnFrom+1)),0),0),;
                  CtnQty4  WITH IIF(cSelect4='»',IIF((lnTo-lnFrom+1)>0,INT(OQty4/(lnTo-lnFrom+1)),0),0),;
                  CtnQty5  WITH IIF(cSelect5='»',IIF((lnTo-lnFrom+1)>0,INT(OQty5/(lnTo-lnFrom+1)),0),0),;
                  CtnQty6  WITH IIF(cSelect6='»',IIF((lnTo-lnFrom+1)>0,INT(OQty6/(lnTo-lnFrom+1)),0),0),;
                  CtnQty7  WITH IIF(cSelect7='»',IIF((lnTo-lnFrom+1)>0,INT(OQty7/(lnTo-lnFrom+1)),0),0),;
                  CtnQty8  WITH IIF(cSelect8='»',IIF((lnTo-lnFrom+1)>0,INT(OQty8/(lnTo-lnFrom+1)),0),0)
          REPLACE CtnTotQty WITH (CtnQty1+CtnQty2+CtnQty3+CtnQty4+CtnQty5+CtnQty6+CtnQty7+CtnQty8)
                
          REPLACE StyWgh   WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
                               cSelect3='»' OR cSelect4='»' OR ;
                               cSelect5='»' OR cSelect6='»' OR ;
                               cSelect7='»' OR cSelect8='»',   ;
                               StyWgh,OrgStyWgh)

          REPLACE Selected WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
                                    cSelect3='»' OR cSelect4='»' OR ;
                                    cSelect5='»' OR cSelect6='»' OR ;
                                    cSelect7='»' OR cSelect8='»',   ;
                                    1,0)
        ENDSCAN  
      ENDSCAN
    ENDIF
    

  CASE _CUROBJ = OBJNUM(pbSelSty)

    *REPLACE cSelect WITH IIF(IIF(llShwOpn,AvlTotQty>0 AND OTotQty>0,AvlTotQty>0),IIF(EMPTY(laData[3]) AND LPicked,'','»'),'')
    
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
    REPLACE CtnTotQty WITH (CtnQty1+CtnQty2+CtnQty3+CtnQty4+CtnQty5+CtnQty6+CtnQty7+CtnQty8)
    
    REPLACE Selected WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
                              cSelect3='»' OR cSelect4='»' OR ;
                              cSelect5='»' OR cSelect6='»' OR ;
                              cSelect7='»' OR cSelect8='»',   ;
                              1,0)

  CASE _CUROBJ = OBJNUM(pbSelPack)
    lcPack_Id = Pack_Id + cpkcolor + cpcksize + cPkVersion 
    lcPack_id2 = Pack_Id +'   '+ cpkcolor + cpcksize + cPkVersion 
    IF !EMPTY(lcPack_Id)
      lcRecNoIs=RECNO()
      LOCATE 
      =SEEK(lcPack_Id)
      SCAN REST WHILE lcPack_Id= Pack_Id + cpkcolor + cpcksize + cPkVersion 
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
       REPLACE CtnTotQty WITH (CtnQty1+CtnQty2+CtnQty3+CtnQty4+CtnQty5+CtnQty6+CtnQty7+CtnQty8)
       REPLACE Selected WITH IIF(cSelect1='»' OR cSelect2='»' OR ;
                               cSelect3='»' OR cSelect4='»' OR ;
                               cSelect5='»' OR cSelect6='»' OR ;
                               cSelect7='»' OR cSelect8='»',   ;
                               1,0)
      ENDSCAN
      =SEEK(lcPack_id2,lcSumPck) 
      SELECT (lcSumPck)

      IF &lcSumPck..cSelect = '»'
        REPLACE &lcSumPck..cSelect    WITH '',;
                &lcSumPck..CtnTotQty  WITH 0
      ELSE
        REPLACE &lcSumPck..cSelect    WITH '»',;
                &lcSumPck..CtnTotQty  WITH CtnTotQty
      ENDIF
      SELECT(lcTmpPck)
    ELSE
    *--Message to tell user it is not belonge to pack
      =gfModalGen("INM00000B42001","Dialog",.F.,.F.,;
                  "This style did not belong to pack.")
    ENDIF  
ENDCASE

= RLOCK(lcTmpPck)
UNLOCk IN (lcTmpPck)

IF EMPTY(SET("RELATION"))
  SET RELATION TO '' INTO (lcScaFile) ADDITIVE
  SET SKIP TO (lcScaFile)
ENDIF  

IF lnCurRec <= RECCOUNT()
  GOTO lnCurRec
  SKIP lnSizeRec-1
ENDIF  

IF lnCurRec <= RECCOUNT()
  GOTO lnCurRec
ENDIF  


=RLOCK(lcTmpPck)
UNLOCk IN (lcTmpPck)

=lfwDtlBrow()

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lpClrSel
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
ELSE
  llClrSel = .F.
ENDIF

*!*************************************************************
*! Name      : lpShwOpn
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
ELSE
  llShwOpn = .F.
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

=lfDtlBrow()

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfSelStatus
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

lcTemFile = IIF(!llShoPckSu,lcSumPck,lcPckLin)
SELECT (lcTemFile)

lnCurRec = RECNO()
lnSizeRec = VAL(&lcScaFile..SzCode)
IF !llAnyRec
  lcDtl2Stat = "DISABLE"
ELSE
  lcDtl2Stat = IIF((laScrMode[3] OR llNew) AND lnActFolder = 2,"ENABLE","DISABLE")
ENDIF  

IF lnActFolder = 2
  SHOW GET ibPckLin ENABLE
ENDIF  
IF llGma AND llComp
  SHOW GET pbSel DISABLE
  SHOW GET pbSelAll DISABLE
  SHOW GET pbInvert DISABLE
  SHOW GET pbSelNo DISABLE
  SHOW GET pbApply DISABLE
  SHOW GET lnFrom DISABLE
  SHOW GET lnTo DISABLE
  SHOW GET lnBrCtnQty DISABLE
  SHOW GET lnBrUntWgh DISABLE
  SHOW GET pbSelPack DISABLE
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lpSavscr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
PRIVATE laPcks,lnI

lcBaseFile = 'PACK_HDR'

IF ORDHDR.STATUS = 'C'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is invoiced, can not save!')
  llCSave = .F.
  RETURN .F.
ENDIF

SELECT &lcStores
GO TOP
lnI = 0
SCAN FOR PACK_NO = '######'
  STORE 0 TO laData[08], laData[09], laData[10]
  WAIT WINDOW NOWAIT 'Generating P/L for store '+&lcStores..STORE + ' D.C '+ &lcStores..DIST_CTR
  laData[1] = &lcStores..PACK_NO
  laData[3] = &lcStores..PIKTKT
  laData[5] = &lcStores..STORE
  DO lfSavePack
  IF !EMPTY(laData[1])
    lnI = lnI + 1
    DIMENSION laPcks[lnI]
    laPcks[lnI] = laData[1]
  ENDIF
ENDSCAN
WAIT WINDOW NOWAIT ''
IF lnI>0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Packing lists from '+laPcks[1]+' to '+laPcks[lnI]+' are generated.')
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No packing lists generated, cannot save.')
  llCSave = .F.  && do not apply any saving code.
ENDIF

lcBaseFile = 'TMPL_HDR'
SELECT (lcBaseFile)

*:**************************************************************************
*:* Name        : lfSavePack
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Save pack for the current store
*:***************************************************************************
FUNCTION lfSavePack
*PROCEDURE lpSavscr* 
*-- lnBookQty,lnBookAmt these variables are to update book,bookamt fields in 
*-- ordhdr file
PRIVATE llReturn,lnCurAlias,lcPckHdTag,lnCount,lnBookQty,lnBookAmt,;
        lnOpenQty,lnOpenAmt,lnCount,lnI,llStyDyOpn,lnCurRecNo,lcSvOrd

llStyDyOpn = .F.

STORE 0 To lnBookQty,lnBookAmt,lnRelCho,lnOpenQty,lnOpenAmt
lnCount = 0
lnCurAlias = SELECT(0)

llNoThing = lfUpdVars()
llNoThing = lfUpdUnCmS("Open", VARREAD() ) 

lcDelStat = SET("DELETED")

PRIVATE lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif , lnOrgBook , lnOrgQty
STORE 0 TO lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif, lnOrgBook , lnOrgQty


SELECT (lcPcklin)
SET RELATION TO 
SET FILTER TO STORE = laData[5]
GO TOP

SELECT (lcCtnDtl)
lcCtDtRel = SET("RELATION")
SET RELATION TO
SET FILTER TO STORE = laData[5]
GO TOP

SELECT (lcCtnDtl)
SET FILTER TO STORE = laData[5]
GO TOP

IF EOF() OR BOF() OR DELETED()
  *--No Packing list lines were applied. Cannot proceed!
  *-- <OK>
  llNoThing = gfModalGen("INM44035B00000","Dialog")  
  llReturn = .F.
ELSE

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
  ENDIF
  IF lnRelCho <> 2 
    SELECT(lcPckLin)
    SCAN
      llNoThing  = SEEK('O'+laData[2]+laData[5]+&lcPckLin..Style+STR(nOrdLineNo,6),'OrdLine')
      IF llNoThing AND &lcPckLin..nStep = 0      && uncompelete session
        IF !llComp  
          SELECT OrdLine 
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
                              
          IF !EMPTY(&lcPckLin..PACK_ID)            
            lcSvOrd = ORDER('SPCK_LIN')
            SET ORDER TO SPCK_LINVR IN SPCK_LIN   &&KEY : TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE
            IF SEEK('P'+ORDLINE.ACCOUNT+ORDLINE.PACK_ID+ORDLINE.CPKCOLOR+ORDLINE.CPCKSIZE+ORDLINE.CPKVERSION+ORDLINE.STYLE,'SPCK_LIN')
              REPLACE ORDLINE.NPKPACK WITH (nPck1+nPck2+nPck3+nPck4+nPck5+nPck6+nPck7+nPck8)/SPCK_LIN.TOTQTY
            ENDIF            
            SET ORDER TO &lcSvOrd IN SPCK_LIN
          ENDIF
        ENDIF

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

          IF !llComp
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
        ENDIF

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

            IF !llComp
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
        ENDIF                    
        SELECT (lcPckLin)
        REPLACE &lcPckLin..nStep WITH 1
      ENDIF

      *--  updating the style file.
      IF &lcPckLin..nStep = 1      && uncompelete session
        IF !llComp
          SELECT STYLE

          IF llUpdtPkTk AND !EMPTY(laData[3]) AND SEEK(&lcPckLin..Style,'Style')
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
        ENDIF         

        SELECT (lcPckLin)
        REPLACE &lcPckLin..nStep WITH 2
      ENDIF
      
      IF !llComp
        llStyDyOpn = gfOpenFile(gcDataDir+"StyDye",gcDataDir+'StyDye','SH')
      
        *--  updating the stydye file.
        IF &lcPckLin..nStep = 2      && uncompelete session
          IF !EMPTY(laData[3])
            IF SEEK (OrdLine.Style+lcWareCode,'StyDye')
              SELECT StyDye

              IF llUpdtPkTk
             
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

             REPLACE Ord1 WITH Ord1 + MAX(&lcPckLin..PQty1-OrdLine.Qty1,0),;
                 	 Ord2 WITH Ord2 + MAX(&lcPckLin..PQty2-OrdLine.Qty2,0),;
					 Ord3 WITH Ord3 + MAX(&lcPckLin..PQty3-OrdLine.Qty3,0),;
					 Ord4 WITH Ord4 + MAX(&lcPckLin..PQty4-OrdLine.Qty4,0),;
					 Ord5 WITH Ord5 + MAX(&lcPckLin..PQty5-OrdLine.Qty5,0),;
					 Ord6 WITH Ord6 + MAX(&lcPckLin..PQty6-OrdLine.Qty6,0),;
					 Ord7 WITH Ord7 + MAX(&lcPckLin..PQty7-OrdLine.Qty7,0),;
					 Ord8 WITH Ord8 + MAX(&lcPckLin..PQty8-OrdLine.Qty8,0),;
                     TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
            
            ENDIF
          ENDIF
          SELECT (lcPckLin)
          REPLACE &lcPckLin..nStep WITH 3
        ENDIF
      ENDIF

      IF !EMPTY(laData[3])
        IF &lcPckLin..nStep = 3      && uncompelete session
          IF llDyelot AND  Style.cDye_Flg = 'Y' AND ;
             SEEK(OrdLine.Style+lcWareCode+OrdLine.Dyelot,'StyDye')
            IF !llComp
              SELECT StyDye
              IF llUpdtPkTk
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

              REPLACE Ord1 WITH Ord1 + MAX(&lcPckLin..PQty1-OrdLine.Qty1,0),;
	   		   		  Ord2 WITH Ord2 + MAX(&lcPckLin..PQty2-OrdLine.Qty2,0),;
					  Ord3 WITH Ord3 + MAX(&lcPckLin..PQty3-OrdLine.Qty3,0),;
					  Ord4 WITH Ord4 + MAX(&lcPckLin..PQty4-OrdLine.Qty4,0),;
					  Ord5 WITH Ord5 + MAX(&lcPckLin..PQty5-OrdLine.Qty5,0),;
					  Ord6 WITH Ord6 + MAX(&lcPckLin..PQty6-OrdLine.Qty6,0),;
					  Ord7 WITH Ord7 + MAX(&lcPckLin..PQty7-OrdLine.Qty7,0),;
 					  Ord8 WITH Ord8 + MAX(&lcPckLin..PQty8-OrdLine.Qty8,0),;
                      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

            ENDIF
          ENDIF
          SELECT (lcPckLin)
          REPLACE &lcPckLin..nStep WITH 4
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  IF llStyDyOpn
    = gfCloseFile("StyDye")
  ENDIF  
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
      SCAN REST FOR Empty = 'Y'
        lnPackCtn = lnPackCtn - 1 
      ENDSCAN
    ELSE
      GO BOTTOM
      lnLastCtn = &lcCtnHdr..Cart_No
    ENDIF
    SET ORDER TO lcCtHdInd IN (lcCtnHdr)
    *-- End getting last carton and carton count

    SELECT Pack_Hdr
    IF !SEEK(laData[1],'Pack_Hdr')
      *-- this part to Handle pack_no
      IF EMPTY(laData[3])
        =SEEK('O'+laData[2],'ORDHDR')
        laData[1] = gfSequence('PIKTKT', '', '', ORDHDR.cDivision)        

        DO WHILE SEEK(laData[1],'PACK_HDR')
          laData[1] = gfSequence('PIKTKT', '', '', ORDHDR.cDivision)
        ENDDO
        INSERT INTO PACK_HDR (PACK_NO) VALUES (laData[1])
        
        REPLACE &lcStores..PACK_NO WITH laData[1]
      ELSE
        laData[1] = laData[3]
        INSERT INTO PACK_HDR (PACK_NO) VALUES (laData[1])
      ENDIF
      *=============This part for BOL=================
      *-- This part will be executed only if the system has ASN Module
      *===============end of BOL=========================
      SELECT Pack_Hdr
    ENDIF  
    lnLastNo = Pack_Hdr.nLastLNo    
    
    *-- Get totals for this packing
    SELECT &lcCtnHdr
    SCAN
      laData[08] = laData[08] + TotWgh
      laData[09] = laData[09] + 1
      laData[10] = laData[10] + TotPcs
    ENDSCAN

    laData[13] = IIF(lnCtnTyp = 1,.T.,.F.)
    laData[14] = IIF(lnDrctTo = 1,'S','C')
    IF llEdiSys


      *-- IF user change BOL then apply delete BOL code
      PRIVATE llTranBol 
      
      SELECT Pack_Hdr
    ENDIF

    GATHER FROM laData FIELDS &lcScFields
    
    REPLACE cWareCode WITH lcWareCode,;
            Bill_Ladg WITH lcBol,;
            cAdd_user WITH gcUser_Id,;
            dAdd_date WITH gdSysDate,;
            cAdd_time WITH gfGettime()

  ENDIF
  *=================end of updating pack_hdr,BOL Files================
 
  *=================This part for saving pack_lin file================
  IF lnRelCho <> 2

    *B037982,1 HBG 4/18/2004 Update the new file EDICRTSQ , abd thenew field Ucc9 in EDIACPRT [Begin]
    *C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
    IF !llIncPLNum
    *C131958,1 HBG 05/02/2006 [End]
      lcSetDele = SET('DELETE')
      SET DELETE ON
      SET ORDER TO PCKCRTSQ IN EDICRTSQ
      IF SEEK(laData[1],'EDICRTSQ')
        SELECT EDICRTSQ
        DELETE FOR pack_no+STR(cart_no,6) = laData[1]
      ENDIF  
      SET DELETE &lcSetDele 
    *C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
    ENDIF 
    *C131958,1 HBG 05/02/2006 [End]
    *B037982,1 [End]

    SET DELETED OFF
    SELECT(lcCtnDtl)
    SET FILTER TO STORE = laData[5]
    SET ORDER TO 0
    = SEEK('O'+laData[2],'OrdHdr')
    SCAN FOR cStatus <> 'S'
      IF &lcCtnDtl..nStep = 0   && for uncomplete session
        SELECT Pack_Lin
        IF SEEK(laData[1]+STR(&lcCtnDtl..Cart_No,4)+&lcCtnDtl..Style,'Pack_Lin')
          IF DELETED('Pack_Lin')
            LOCATE REST FOR pack_no+STR(no_cart,4)+style = ;
                            laData[1]+STR(&lcCtnDtl..Cart_No,4)+;
                            &lcCtnDtl..Style AND !DELETED()
          ENDIF
          LOCATE REST FOR Pack_No+STR(Line_No,6)+Style+cPackColor = ;
                          laData[1]+STR(&lcCtnDtl..PackLineNo,6)+&lcCtnDtl..Style
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
                Style      WITH &lcCtnDtl..Style,;
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

        *-- Save information of the pack in PACK_LIN file 
        REPLACE PACK_ID    WITH &lcCtnDtl..PACK_ID,;
                cPkColor   WITH &lcCtnDtl..cPkColor,;
                cPCkSize   WITH &lcCtnDtl..cPckSize,;
                cPKVersion WITH &lcCtnDtl..cPkVersion,;
                nPackNO    WITH &lcCtnDtl..nPackNO    

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
        IF !llComp
          SELECT OrdLine
          *-- These 2 seek are to adjust the pointer in both files (lcpcklin,'OrdLine')

          SET ORDER TO lcPckLin IN (lcPckLin)
          =SEEK(&lcCtnDtl..Style+STR(&lcCtnDtl..nOrdLineNo,6),lcPckLin)
         
          =SEEK('O'+laData[2]+laData[5]+&lcCtnDtl..Style+STR(&lcCtnDtl..nOrdLineNo,6),'OrdLine')
          lnOrgBook = ToTBook
          lnOrgQty  = TotQty

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
          lnBookAmt = lnBookAmt + (TotBook * Price)

          REPLACE Qty1    WITH &lcPckLin..OrdQty1,;
                  Qty2    WITH &lcPckLin..OrdQty2,;
                  Qty3    WITH &lcPckLin..OrdQty3,;
                  Qty4    WITH &lcPckLin..OrdQty4,;
                  Qty5    WITH &lcPckLin..OrdQty5,;
                  Qty6    WITH &lcPckLin..OrdQty6,;
                  Qty7    WITH &lcPckLin..OrdQty7,;
                  Qty8    WITH &lcPckLin..OrdQty8,;
                  TotQty  WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8


          lnBookDiff = lnBookDiff + (TotBook - lnOrgBook)
          lnBkAmtDif = lnBkAmtDif + ((TotBook - lnOrgBook) * Price)

          lnQtyDiff  = lnQtyDiff  + (TotQty - lnOrgQty)
          lnQyAmtDif = lnQyAmtDif + ((TotQty - lnOrgQty) * Price)      
        ENDIF
      ENDIF
      *=================end updating ordline file======================    

    ENDSCAN

    SELECT Pack_Lin
    lcSetDel = SET('DELETE')
    SET DELETE ON
    =SEEK(laData[1])
    lnI = 0
    llFirst  = .T.
    lnLastCrt = Pack_Lin.No_Cart
    llSameCrt = .F.
    SCAN REST WHILE Pack_No+STR(No_Cart,4)+Style = laData[1]
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
    ENDSCAN

    *B037982,1 HBG 4/18/2004 Update the new file EDICRTSQ , and the new field Ucc9 in EDIACPRT [Begin]
    *C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
    IF !llIncPLNum
    *C131958,1 HBG 05/02/2006 [End]

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
    *C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
    ENDIF
    *C131958,1 HBG 05/02/2006 [End]
    *B037982,1 [End]

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
    
    SELECT Pack_Hdr
    REPLACE nLastLno  WITH MAX(nLastLno,lnLastNo) ,;
            nLastCart WITH lnLastCtn
  ENDIF
  *=================end saving pack_lin file======================  
  
  *=================update book,open fields in ordhdr file=========
  IF lnRelCho <> 2 
    IF !(lnBookDiff=0 AND lnQtyDiff=0 AND lnBkAmtDif=0 AND lnQyAmtDif=0) AND ;
       SEEK('O'+Pack_Hdr.Order,'OrdHdr')

      IF !llComp
        SELECT OrdHdr

        =RLOCK()
        REPLACE BOOK    WITH BOOK    + lnBookDiff ,;
                BOOKAMT WITH BOOKAMT + lnBkAmtDif ,;
                OPEN    WITH OPEN    + lnQtyDiff  ,;
                OPENAMT WITH OPENAMT + lnQyAmtDif
        UNLOCK        
        =gfAdd_Info()
      ENDIF
    ENDIF
  ENDIF
  *=================end update book,open fields in ordhdr file=====    
  *-- C101735,1 SAVE CARTON INFO. TO BE PRINTED [Begin]
  
  *================= Update status fields in Piktkt file to be 'K' As 'Pack' =========*
  IF ASCAN(laEvntTrig , PADR('LLPAKST',10)) <> 0
    IF lnRelCho <> 2 .AND. PikTkt.Status # 'K'
      lnOldAlias = SELECT(0)
      SELECT PikTkt
      REPLACE Status With 'K'
      SELECT (lnOldAlias)
    ENDIF
  ENDIF

ENDIF

=llEdiSys AND llCanPrnLb AND lfSavCartn()

SET DELETED &lcDelStat

IF lnRelCho <> 2
  llReturn = .T.
ELSE
  llReturn = .F.
ENDIF

SELECT(lcCtnDtl)
SET RELATION TO &lcCtDtRel.

*llNoThing = lfUpdUnCmS("Comp", SPACE(0))

SELECT OrdLine
lcTag = ORDER()
SET ORDER TO TAG OrdLinSt
lnCurRecNo = 0
IF SEEK('O'+laData[2]+laData[5],'OrdLine') 
  SELECT OrdLine
  lnCurRecNo = RECNO()
  SCAN REST WHILE cOrdType+Order+Store+Style+STR(lineno,6) = 'O'+laData[2]+laData[5]
    = gfObj_Lock(.F.)
  ENDSCAN  
ENDIF
SELECT OrdLine
SET ORDER TO TAG lcTag
IF BETWEEN(lnCurRecNo,1,RECCOUNT('ORDLINE'))
  GOTO (lnCurRecNo)
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

lnCurAlias = SELECT(0)

IF !(laData[2]==lcPrvPack)
  lcPrvPack = laData[2]
  =lfvNewPack()
ENDIF

SELECT (lcCtnDtl)
SET FILTER TO 
SET RELATION TO
SCAN
  IF SEEK('O'+laData[2]+laData[5]+&lcCtnDtl..Style+STR(&lcCtnDtl..nOrdLineNo,6),'OrdLine')
    SELECT OrdLine
    REPLACE nPck1  WITH IIF(&lcCtnDtl..Qty1 <> 0,nPck1  - &lcCtnDtl..Qty1,0),;
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

    IF !EMPTY(&lcPckLin..PACK_ID)            
      lcSvOrd = ORDER('SPCK_LIN')
      SET ORDER TO SPCK_LINVR IN SPCK_LIN   &&KEY : TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE
      IF SEEK('P'+ORDLINE.ACCOUNT+ORDLINE.PACK_ID+ORDLINE.CPKCOLOR+ORDLINE.CPCKSIZE+ORDLINE.CPKVERSION+ORDLINE.STYLE,'SPCK_LIN') OR;
         SEEK('P*****'+ORDLINE.PACK_ID+ORDLINE.CPKCOLOR+ORDLINE.CPCKSIZE+ORDLINE.CPKVERSION+ORDLINE.STYLE,'SPCK_LIN')
        REPLACE NPKPACK WITH (nPck1+nPck2+nPck3+nPck4+nPck5+nPck6+nPck7+nPck8)/SPCK_LIN.TOTQTY
      ENDIF            
      SET ORDER TO &lcSvOrd IN SPCK_LIN
    ENDIF
  ENDIF                                  
ENDSCAN
SET RELATION TO '' INTO (lcCartonSz)
SET SKIP TO (lcCartonSz)

lcHdrTag = ORDER('Pack_Hdr')
SET ORDER TO Pack_Hdr IN Pack_Hdr
IF SEEK(laData[1],'Pack_Hdr')
  SELECT Pack_Hdr
  BLANK
  DELETE 
ENDIF  
SET ORDER TO TAG (lcHdrTag) IN Pack_hdr

lcLinTag = ORDER('Pack_Lin')
*B606394,1 HBG 18/08/2002 Enhance the speed of deletion[Begin]
*IF SEEK(laData[1],'Pack_Lin')
  *SELECT Pack_Lin
  *SCAN FOR Pack_No+STR(No_Cart,4)+Style = laData[1]
  *  BLANK
  *  DELETE
  *ENDSCAN
*ENDIF  
SELECT Pack_Lin
SET ORDER TO Packstyle
DELETE FOR Pack_No+STR(No_Cart,4)+Style = laData[1]
*B606394,1 [End]

*B037982,1 HBG 4/18/2004 Update the new file EDICRTSQ , abd thenew field Ucc9 in EDIACPRT [Begin]
*C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
IF !llIncPLNum
*C131958,1 HBG 05/02/2006 [End]
  SET ORDER TO PCKCRTSQ IN EDICRTSQ
  lcSetDele = SET('DELETE')
  SET DELETE ON
  IF SEEK(laData[1],'EDICRTSQ')
    SELECT EDICRTSQ
    DELETE FOR pack_no+STR(cart_no,6) = laData[1]
  ENDIF  
  SET DELETE &lcSetDele 
*C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
ENDIF
*C131958,1 HBG 05/02/2006 [End]
*B037982,1 [End]

SET ORDER TO TAG (lcLinTag) IN Pack_Lin

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
DO (gcScrDir + '\ALPLSTCP.SPR')    
POP KEY                                        

*!*************************************************************
*! Name      : lfvPackNo
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
  lnCtnTyp = IIF(Pack_Hdr.LStandCtn,1,2)
  lnDrctTo = IIF(Pack_Hdr.CToStorCn='C',2,1)
  *-- Fixing bug of getting lnFrom & lnTo intioaly by zero when we copy from packing list[Begin]
  lnFrom = 1
  lnTo   = 1
  *-- Fixing bug of getting lnFrom & lnTo intioaly by zero when we copy from packing list[End  ]
ENDIF

SET ORDER TO lcCurTag IN Pack_Hdr
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPackScrAct
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*  lcFields = "cMarker =IIF(RECNO()=lnCtHdBrRe,'>',' ')  :H=' ':R:1:W=.F.,"+;
             "Cart_No    :H = 'Cart.#'       :6 :R,"+;
             "Pal_No     :H = 'Palette'      :6 :R,"+;
             "TotPcs     :H = 'Tot.Pcs'      :8 :R,"+;
             "TotWgh     :H = 'Tot.Wgh'      :8 :R"
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

SELECT (lcCtnDtl)
SET RELATION TO
SET FILTER TO STORE = &lcStores..STORE .AND. ;
                      STR(Cart_No,4)+Style+STR(nOrdLineNo,6) = STR(&lcCtnHdr..Cart_No,4) 

SELECT (lcCtnDtl)
SET RELATION TO '' INTO (lcCartonSz) ADDITIVE
SET SKIP TO (lcCartonSz)

SELECT (lcCtnDtl)
GO TOP
LOCATE FOR EVAL(lcCtnDtl+'.Br'+&lcCartonSz..SzCode)

IF WEXIST(lcCtDtBrTt) AND lnActFolder = 3
  = lfwCtnDtlBr()
ENDIF  

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvCtnHdrBr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
              StyCode=Style:H= lcStyTtl     :25:R,;
              Size   =EVAL('Size' +&lcCartonSz..SzCode) :H = 'Size':7 :R,;
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
  DEFINE BAR 5 OF _OPTPOP PROMPT "Style Filter" SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder!=2) OR EOF(lcPckLin))
  DEFINE BAR 6 OF _OPTPOP PROMPT "Update Pick Ticket Qty. Upon Saving" SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder=1))
  SET MARK OF BAR 6 OF _OPTPOP TO llUpdtPkTk
ELSE
  DEFINE BAR 3 OF _OPTPOP PROMPT "Style Filter" SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder!=2) OR EOF(lcPckLin))
  DEFINE BAR 4 OF _OPTPOP PROMPT "Update Pick Ticket Qty. Upon Saving" SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder=1))
  SET MARK OF BAR 4 OF _OPTPOP TO llUpdtPkTk
ENDIF

IF llCanPrnLb
  IF llShoPckSu
    *B606394,1 HBG 18/08/2002 Activate this option in view mode [Begin]
    *DEFINE BAR 7 OF _OPTPOP PROMPT 'Pack Summary'  SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder=1))
    DEFINE BAR 7 OF _OPTPOP PROMPT 'Pack Summary'  SKIP FOR (laScrMode[1] OR (lnActFolder=1))
    *B606394,1 [End]
    ON SELECTION BAR 7 OF _OPTPOP DO lfPoupBrow
  ELSE
    *B606394,1 HBG 18/08/2002 Activate this option in view mode [Begin]
    *DEFINE BAR 7 OF _OPTPOP PROMPT 'Pack Detail'  SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder=1))
    DEFINE BAR 7 OF _OPTPOP PROMPT 'Pack Detail'  SKIP FOR (laScrMode[1] OR (lnActFolder=1))
    *B606394,1 [End]
    ON SELECTION BAR 7 OF _OPTPOP DO lfPoupBrow
  ENDIF  
ELSE
  IF llShoPckSu
    DEFINE BAR 5 OF _OPTPOP PROMPT 'Pack Summary '  SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder=1))
    ON SELECTION BAR 5 OF _OPTPOP DO lfPoupBrow
  ELSE
    DEFINE BAR 5 OF _OPTPOP PROMPT 'Pack Detail'  SKIP FOR (laScrMode[1] OR laScrMode[2] OR (lnActFolder=1))
    ON SELECTION BAR 5 OF _OPTPOP DO lfPoupBrow
  ENDIF  
ENDIF  

*-- Define the action to be done when activating the pad and the popup.
ON PAD _OPTIONS OF _MSYSMENU ACTIVATE POPUP _OPTPOP
ON SELECTION BAR 1 OF _OPTPOP DO lpClrSel
ON SELECTION BAR 2 OF _OPTPOP DO lpShwOpn

IF llCanPrnLb AND (laScrMode[3] OR laScrMode[4]) AND (lnActFolder = 2)
  ON SELECTION BAR 4 OF _OPTPOP DO lpPortScr
ENDIF

IF llCanPrnLb
  ON SELECTION BAR 5 OF _OPTPOP DO lpFltrScr 
ELSE
  ON SELECTION BAR 3 OF _OPTPOP DO lpFltrScr
ENDIF
IF (laScrMode[3] OR laScrMode[4]) AND (lnActFolder <> 1)
  ON SELECTION BAR IIF(llCanPrnLb,6,4) OF _OPTPOP DO lpUpdtPkTk
ENDIF

*-- end of lfActPad.
*!*************************************************************
*! Name      : lfRELPad
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
SHOW GET pbHdrNew  &lcAddHdrSt
SHOW GET pbDtlNew  &lcAddDtlSt
*B606394,1 HBG 18/08/2002  Disable print lable button in carton folder [Begin]
*SHOW GET pbPrnLbl  &lcPrnLbl
SHOW GET pbPrnLbl  DISABLE
*B606394,1 [End]

*!*************************************************************
*! Name      : lfChkUnCmS
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
llGoOut     = .F.

IF gfUnCompSession(lcProgID,lnSessNo,"Automatic Packing List")
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
=lfCrSelSum()
*!*************************************************************
*! Name      : lfCrSelFiles
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

*B606394,1 HBG 18/08/2002 open pack header file [Begin]
=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPCK_HDRVR','SH')
*B606394,1 [End]

*--We modify way of selection to just select By Style-Color
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

*-- Total order Qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
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

*--total Avalable Qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0
*--

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

*-- total open qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0
*--

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

*-- total carton Qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
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

*--
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotQty'
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

*-- total wight
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2
*--

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
*-- total original Qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgTotOrd'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

*-- total original Qty

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

*-- total diffrance Qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nTotDiff'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
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

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

*C122070,1 HBG 04/04/2004 Add new filed to temp detail file to know if this pack is range or not [Begin]
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
laFileStru[lnI,1] = 'UpOqty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- total open qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UOTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0
*--
*C122070,1 [End]

DIMENSION laIndx[7,2]

laIndx[1,1] = "Style+STR(nOrdLineNo,6)"
laIndx[1,2] = lcPckLin


laIndx[2,1] = "STORE"
laIndx[2,2] = 'Store'

laIndx[3,1] = "IIF(OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+OQty8>0,'Y','N')"
laIndx[3,2] = 'Opened'

laIndx[4,1] = "IIF(PQty1+PQty2+PQty3+PQty4+PQty5+PQty6+PQty7+PQty8=0,'Y','N')"
laIndx[4,2] = 'NoPacked'

laIndx[5,1] = "STR(nOrdLineNo,6)+Style"
laIndx[5,2] = lcTmpIdx
laIndx[6,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+STR(nOrdLineNo,6)+Style"
laIndx[6,2] = lcPakIndxLn
laIndx[7,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+STR(nOrdLineNo,6)"
laIndx[7,2] = lcPakIndxSt


=gfCrtTmp(lcPckLin,@laFileStru,@laIndx)
SET ORDER TO lcPakIndxLn IN (lcPckLin)

SELECT PACK_LIN
=AFIELDS(laFileStru)

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

DIMENSION laIndx[1,2]
laIndx[1,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+STR(nOrdLineNo,6)"
laIndx[1,2] = lcPack_Lin

=gfCrtTmp(lcPack_Lin,@laFileStru,@laIndx)
SET ORDER TO lcPakIndxLn IN (lcPckLin)




*:************* Create Stores temp file  ************************************
lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DIST_CTR'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PIKTKT'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TOTORD'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TOTPIK'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Cartons'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TOTPQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_NO'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'BOL_NO'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

DIMENSION laIndx[1,2]
laIndx[1,1] = "DIST_CTR+STORE"
laIndx[1,2] = lcStores

=gfCrtTmp(lcStores,@laFileStru,@laIndx)
SET ORDER TO lcStores IN (lcStores)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfCrCtnFiles
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

DIMENSION laIndx[3,2]
laIndx[1,1] = "STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[1,2] = lcCtnHdr

laIndx[2,1] = "Empty+STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[2,2] = "EMPTY"

laIndx[3,1] = "STORE+STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[3,2] = "STORE"

=gfCrtTmp(lcCtnHdr,@laFileStru,@laIndx)           
SET ORDER TO lcCtnHdr In (lcCtnHdr)


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
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
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



DIMENSION laIndx[2,2]
laIndx[1,1] = "STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style+STR(nOrdLineNo,6)" 
laIndx[1,2] = lcCtnDtl

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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

llNoThing = lfUpdUnCmS("Initia", SPACE(0))

*!*************************************************************
*! Name      : lfUpdate
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : check using this order in creating pack and 
*!           : Lock ordhdr record if it is not
*!*************************************************************

FUNCTION lfChkOrdLok
PARAMETERS llLock
PRIVATE llGoOn,lnCurAlias,lnCurRecNo

llGoOn = .T.
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

SELECT OrdLine
SET ORDER TO &lcSvOrd IN ORDLINE
IF BETWEEN(lnCurRecNo,1,RECCOUNT('ORDLINE'))
  GOTO lnCurRecNo 
ENDIF

SELECT (lnCurAlias)

RETURN llGoOn

*!*************************************************************
*! Name      : lfNewBOL
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Check for the BOL in add mode and if not found generat a rundoum one 
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfNewBOL()
*!*************************************************************
*!
FUNCTION lfNewBOL
IF !llEdiSys
  RETURN
ENDIF

PRIVATE laFields
*-- array to hold defulted fields for BOL
DIMENSION laFields[3,2]
laFields[1,1] = "cgronhang"
laFields[1,2] = "N"
laFields[2,1] = "ctranmthd"
laFields[2,2] = "M"
laFields[3,1] = "packtype"
laFields[3,2] = "CTN25"
lcBol = lfGetBOL("",laData[4],laData[5],lcWareCode,laData[7],IIF(lnCtnTyp=1,'Y','N'),"laFields",.F.,.F.,IIF(lnDrctTo=2,"C","S"))

*-- End Of Function lfNewBOL

*!*************************************************************
*! Name      : lfvLblInfo
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

DIMENSION laAcShipTo[5] , laDcShipTo[5] , laWareAddr[5]
STORE "" TO laAcShipTo , laDcShipTo , laWareAddr , lcStyle , lcColor , lcSize
=lfGetStySz()

m.bol_no   = lcBol
m.Cart_No  = lnCarton


=SEEK(lcWareCode,'Warehous')
m.VND_NAME  = Warehous.cDesc
m.VND_ADDR1 = Warehous.cAddress1
m.VND_ADDR2 = Warehous.cAddress2
m.VND_CITY  = Warehous.cAddress3
m.VND_STATE = Warehous.cAddress4
m.VND_ZIP   = Warehous.cAddress5

*-- Get Customer information [Begin]

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

*-- Get Customer information [End  ]

*-- Get Distribution Center information [Begin]


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
*-- Get Distribution Center information [End  ]
m.ShipVia  = laShpVia[lnShpVia,2]

lcwrname  = lcwarecode

*-- Get Carton information [Begin]
SELECT ORDHDR
SET ORDER TO ORDHDR
=SEEK('O'+laData[2])
lcOrStore   = OrdHdr.Store

*-- remove the EDI condition from printing carton labels [Begin]
lcUccVer = ''
IF llEdiSys
  lcUccVer = IIF(lnDrctTo=1,EdiPH.cASNLbl1,EdiPH.cASNLbl2)
ENDIF

lcUccVer    = IIF(EMPTY(lcUccVer),'XXX',lcUccVer)
m.CUSTPO    = OrdHdr.CustPo
m.DEPT      = OrdHdr.Dept

m.MANUF_ID  = PADL(ALLTRIM(lcManufId),7,'0')

*B037982,1 HBG 4/18/2004 Get the Carton # form the new file EDICRTSQ instead of calculating it [Begin]
*m.UCC9     = RIGHT(PADL(ALLTRIM(laData[1]),6,'0'),5)+PADL(lnCarton,4,'0')
* C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
IF llIncPLNum
  m.UCC9  = RIGHT(PADL(ALLTRIM(laData[1]),6,'0'),lnNumOfDig)+PADL(lnCarton,IIF(lnNumOfDig = 5,4,3),'0')
ELSE 
*C131958,1 HBG 05/02/2006 [End]
  SET ORDER TO PCKCRTSQ IN EDICRTSQ
  IF SEEK(laData[1]+STR(lnCarton,6),'EDICRTSQ')
    m.Ucc9 = PADL(EDICRTSQ.Ucc9,9,'0')
  ENDIF
*C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
ENDIF
*C131958,1 HBG 05/02/2006 [End]
*B037982,1 [End]

m.UCC_CHECK = lfCheckNo('000'+m.MANUF_ID+m.Ucc9)
*-- These fields has to be filled 
m.Note1     = OrdHdr.Note1
m.Note2     = OrdHdr.Note2
m.Int_Vend  = IIF(EMPTY(ORDHDR.INT_VEND),m.Int_Vend,ORDHDR.INT_VEND)
m.Cancelled = OrdHdr.Complete

m.ASN_VER   = lcUccVer  
m.EVENT_COD = OrdHdr.Event_Cod

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
m.PACK_NO   = ladata[1]
m.Carrier   = laCodDesc[1,3]
m.Bol_No    = lcBol
m.Style     = lcStyle
*-- Get Carton information [End  ]

=SEEK(STR(m.Cart_No,4),lcctnhdr)

m.TotQty    = &lcCtnHdr..TotPcs
m.cSizeDesc = lcSize

*-- Get No. of Cartons.
lcDetOrder = ORDER(lcCtnHdr)
USE (gcWorkDir+lcCtnHdr) ORDER (lcDetOrder) IN 0 AGAIN ALIAS (lcTmpDetFl)
GO BOTTOM IN (lcTmpDetFl)
m.Cartons = &lcTmpDetFl..Cart_No
USE IN (lcTmpDetFl)

IF SEEK(STR(m.CART_NO,6),lcTmAsnShp)
  SELECT (lcTmAsnShp)
  GATHER MEMVAR MEMO
ELSE
  INSERT INTO (lcTmAsnShp) FROM MEMVAR
ENDIF
IF llPrint
  =lfPrintLbl(lcUccVer)
  IF llntFound
    RETURN
  ENDIF
ENDIF
SELECT (lcPckLin)
*-- End Of lfvLblInfo  

*!*************************************************************
*! Name      : lfPrintLbl
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Print carton labels
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : 
*!*************************************************************
*!

FUNCTION lfPrintLbl
PARAMETERS lcVersion
PRIVATE lnCurAlias, lnHandle, lcOutFile, lcString, lcData,lnCount
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

SELECT (lcTmAsnShp)
SCATTER MEMVAR
=gfRltFld(ORDHDR.cDivision,@laDRltFld,'CDIVISION ')
m.DivLName = lcDivLName
=SEEK(OrdHdr.Account+OrdHdr.Dept,'CUSTDEPT')
m.DeptDesc = CustDept.cDeptDesc
=SEEK(ALLTRIM(style),'Style')
m.StyDesc = Style.Desc

=SEEK(STR(&lcTmAsnShp..cart_no,4),lcCtnHdr)
m.Weight = &lcCtnHdr..TotWgh

SELECT (lcTmAsnShp)
m.Date     = gdSysDate
m.Order    = laData[2]
m.Account  = laData[4]
m.Pattern = Style.Pattern

STORE '' TO m.SizeDesc1,m.SizeDesc2,m.SizeDesc3,m.SizeDesc4,m.SizeDesc5,m.SizeDesc6,m.SizeDesc7,m.SizeDesc8
STORE '' TO m.SizeSku1,m.Sizesku2,m.Sizesku3,m.Sizesku4,m.Sizesku5,m.Sizesku6,m.Sizesku7,m.Sizesku8
STORE 0  TO m.SizeQty1,m.SizeQty2,m.SizeQty3,m.SizeQty4,m.SizeQty5,m.SizeQty6,m.SizeQty7,m.SizeQty8

SELECT(lcCtnDtl)
lnStyRec = RECNO(lcCtnDtl)
lnSzeRec = RECNO(lcCartonSz)
SET RELATION TO 

IF llExtSizSc
  SELECT (lcCtnDtl)
  IF SEEK(STR(&lcTmAsnShp..cart_no,4)+ALLTRIM(&lcTmAsnShp..Style))
    lnSizeCount = 0
    lcCurrSty = SUBSTR(Style,1,lnSizePos-IIF(EMPTY(lcSizeSep),1,2))
    SCAN REST WHILE STR(Cart_No,4)+Style+STR(nOrdLineNo,6) = STR(&lcTmAsnShp..cart_no,4)+lcCurrSty ;
              AND lnSizeCount < 8 
      =SEEK('S'+SUBSTR(Style,lnSizePos,lnSizeLen),'Scale')
      FOR lnCount = 1 TO Scale.Cnt
        lcCount = STR(lnCount,1)
        IF &lcCtnDtl..Qty&lcCount <> 0
          lnSizeCount = lnSizeCount + 1
          lcSizeCount = STR(lnSizeCount,1)
          m.SizeDesc&lcSizeCount = ALLTRIM(Scale.Sz&lcCount)
          m.SizeQty&lcSizeCount  = &lcCtnDtl..Qty&lcCount
          SELECT SPCK_LIN
          =SEEK('S'+laData[4]+&lcCtnDtl..style)
          LOCATE REST WHILE type+account+style+pack_id+cPkColor+cPckSize+cPkVersion = 'S'+laData[4]+&lcCtnDtl..style FOR QTY&lcCount = 1
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
  IF SEEK(STR(Cart_no,4)+ALLTRIM(Style),lcCtnDtl)
    FOR lnCount = 1 TO Scale.Cnt
      lcCount = STR(lnCount,1)
      IF &lcCtnDtl..Qty&lcCount <> 0
        m.SizeDesc&lcCount = ALLTRIM(Scale.Sz&lcCount)
        m.SizeQty&lcCount  = &lcCtnDtl..Qty&lcCount
        SELECT SPCK_LIN
        =SEEK('S'+laData[4]+&lcCtnDtl..style)
        LOCATE REST WHILE type+account+style+pack_id+cPkColor+cPckSize+cPkVersion = 'S'+laData[4]+&lcCtnDtl..style FOR QTY&lcCount = 1
        IF FOUND()
          m.SizeSku&lcCount = ALLTRIM(Spck_Lin.Pack_Id)
        ENDIF
      ENDIF
    ENDFOR  
  ENDIF
ENDIF
SELECT (lcTmAsnShp)

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
                  WHILE Type + Account + Style + Pack_ID+cPkColor+cPckSize+cPkVersion = 'S' + laData[4] + &lcCtnDtl..Style;
                    FOR QTY&lcCount = 1
            IF FOUND()
              MDSKU&lcSizeCount = ALLTRIM(Spck_Lin.Pack_Id)
            ENDIF
          ENDIF
        ENDFOR
      ENDSCAN
    ENDIF
    
    SELECT SYCASNLB
    =SEEK("XX1" + 'H')
    SCAN WHILE cVer + cEdiType = "XX1" + 'H'
      STORE DATA TO lcData
      lcString = &lcData 
      =FPUTS(lnHandle,lcString)
    ENDSCAN	
    lcString = SPACE(3)
    =FPUTS(lnHandle,lcString)
    
    =SEEK("XX1" + 'L')
    SCAN WHILE cVer + cEdiType = "XX1" + 'L'
      STORE DATA TO lcData
      lcString = &lcData 
      =FPUTS(lnHandle,lcString)
    ENDSCAN
  ENDIF
ENDIF

=FCLOSE(lnHandle)
WAIT 'Sending Shipping Labels to Printer...Please stand by....' WINDOW NOWAIT
*! MODE COM1:24,N,8,1,P >>null
*! MODE LPT1:=COM1: >>null
*! PRINT /D:LPT1 /S:1 >>null
lcCommand = "TYPE " + lcOutFile + " > " + lcSndPort
!/N0 &lcCommand 

WAIT CLEAR

SELECT (lcCtnDtl)

SET RELATION TO '' INTO (lcCartonSz)
SET SKIP TO (lcCartonSz)

IF lnStyRec <= RECCOUNT(lcCtnDtl) 
  GOTO lnStyRec
  SKIP lnSzeRec-1
ENDIF
= RLOCK(lcCtnDtl) 
UNLOCk IN (lcCtnDtl)

SELECT(lnCurAlias)
RETURN
*-- End Of Function lfPrintLbl 

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Print carton labels
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfEvalSegs()
*!*************************************************************
*!
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Algorithm to Compute the modulus-10 Check digit for the 
*!             UCC 128 code
*!*************************************************************
*! Parameters: lcUccNo : UCC number without check digit
*!*************************************************************
*! Returns   : UCC Check digit
*!*************************************************************
*! Example   :  =lfCheckNo('1234567890123456789')
*!*************************************************************
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
*! Name      : lfSavCartn
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Saving Cartons  in ASN_SHIP
*!*************************************************************
*! Example   : = lfSavCartn()
*!*************************************************************
*!C101735,1
*!
FUNCTION lfSavCartn
PRIVATE llchoice, lnCartons, lnCarRef , lnActAlias , lcDetOrder , llPrinSel
lnActAlias = SELECT(0)

*B037982,1 HBG 4/18/2004 Fix bug of of not deleting old cartons from ASN_SHIP file [Begin]
lcSetDele = SET('DELETE')
*C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
IF !llIncPLNum
*C131958,1 HBG 05/02/2006 [End]
  SET DELETE ON
  SELECT (lcTmAsnShp)
  ZAP
  SELECT Asn_Ship
  DELETE FOR bol_no+pack_no+STR(cart_no,6)+asn_ver = lcBol+laData[1]
*C131958,1 HBG 05/02/2006 Add new setup of including P\L number in UCC [Begin]
ENDIF
*C131958,1 HBG 05/02/2006 [End]
*B037982,1 [End]

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
SET ORDER TO (lcCtnDtl) In (lcCtnDtl)
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

*B037982,1 HBG 4/18/2004 Fix bug of of not deleting old cartons from ASN_SHIP file [Begin]
SET DELETE &lcSetDele
*B037982,1 [End]

llchoice = .T.

  DO WHILE llchoice
    lnCarRef = 0
    lnChoice = gfModalGen("QRM44097B44011","Dialog")
    DO CASE

     *-- Case of YES
     CASE lnChoice=1
       FOR lnCarRef = 1 TO lnCartons
         IF SEEK(STR(lnCarRef,6),lcTmAsnShp)
           SELECT (lcTmAsnShp)
           =lfPrintLbl(ASN_VER)
         ENDIF  
       ENDFOR
     
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
         FOR lnCarRef = 1 TO ALEN(laTarget,1)
           lnToPrnCrt = VAL(RIGHT(laTarget[lnCarRef],4))
           IF SEEK(STR(lnToPrnCrt,6),lcTmAsnShp)
             SELECT (lcTmAsnShp)
             =lfPrintLbl(ASN_VER)
           ENDIF  
         ENDFOR
       ENDIF
         
     *-- Case of proceed
     CASE lnChoice=3
       llchoice = .F.

    ENDCASE
  ENDDO

SELECT (lnActAlias)
*-- end of lfSavCartn.

*!*************************************************************
*! Name      : lpPortScr
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
*! Refer To  : E301373 
*!*************************************************************
*! Example   : = lfvStyFltr()
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
*! Refer To  : E301373 
*!*************************************************************
*! Example   : = lpFltrScr()
*!*************************************************************
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
*! Refer To  : E301373 
*!*****************************************************************************
*! Example   : = lfvpbOk()
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
*! Refer To  : E301373 
*!*************************************************************
*! Example   : = lfReset()
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
*! Example   : = lfvpbRst()
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
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
*! Purpose   : Valid function for the update piktkt menu bar.
*!*******************************************************************************
*! Example   : = lfUpdtPkTk()
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
*! Name      : lfNoSuffic
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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
              llExitLoop = .T.

            CASE lnChoice = 3
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
            llExitLoop = .T.
                   
           ELSE
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
*! Name        : lfCrSelFiles
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date         : 06/09/2003
*! Purpose   : Create the files that are used in Function lfvSelOrd
*!                  to get pack summery (lcSumPck)
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfCrSelSum()
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

DIMENSION laIndx[1,2]

laIndx[1,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion"
laIndx[1,2] = lcSumPck

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
 * IF !EMPTY(Pack_Id)
    IF lcPack_Id = LEFT(Pack_Id,16) + cpkcolor + cpcksize + cPkVersion 
      SELECT(lcSumPck)  
      REPLACE OTotQty      WITH OTotQty   + &lcPckLin..OTotQty,;
              CtnTotQty    WITH CtnTotQty + &lcPckLin..CtnTotQty,;
              PTotQty      WITH PTotQty   + &lcPckLin..PTotQty,;
              PTotWgh      WITH PTotWgh   + &lcPckLin..PTotWgh
    ELSE
      SELECT(lcSumPck)  
      APPEND BLANK
      REPLACE OTotQty      WITH &lcPckLin..OTotQty,;
              CtnTotQty    WITH &lcPckLin..CtnTotQty,;
              PTotQty      WITH &lcPckLin..PTotQty,;
              PTotWgh      WITH &lcPckLin..PTotWgh,;
              cPkVersion   WITH &lcPckLin..cPkVersion,; 
              cPkColor     WITH &lcPckLin..cPkColor,;
              cPckSize     WITH &lcPckLin..cPckSize
      IF EMPTY(&lcPckLin..Pack_id)
        REPLACE PACK_ID      WITH &lcPckLin..Style
      ELSE
        llGetPack  =SEEK('P'+laData[4]+PADR(&lcPckLin..Pack_Id,16)+&lcPckLin..cPkColor+;
                          &lcPckLin..cPckSize+&lcPckLin..cPkVersion,'SPCK_HDR')
        IF !llGetPack 
          = SEEK('P*****'+PADR(&lcPckLin..Pack_Id,16)+&lcPckLin..cPkColor+;
                          &lcPckLin..cPckSize+&lcPckLin..cPkVersion,'SPCK_HDR')
        ENDIF 
        REPLACE PACK_ID      WITH &lcPckLin..PACK_ID,;
                llPack       WITH .T.,;
                lRange       WITH SPCK_HDR.lRange
      ENDIF       
      lcPack_Id = LEFT(Pack_Id,16) + cpkcolor + cpcksize + cPkVersion             
    ENDIF 
 * ENDIF  
ENDSCAN
*B606394,1 HBG 18/08/2002 Show # of packs for each pack Id [Begin]
llApply = .F.
=lfUpdSumFl()
*B606394,1 [End]
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

llShoPckSu = IIF(llShoPckSu,.F.,.T.)
=lfDtlBrow()

*!*************************************************************
*! Name      : lfvCtnSize
*! Developer : TMI -TAREK MOHAMED IBRAHIM
*! Date      : 06/09/2003
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

    *-- Select the right index in case GMA
    *IF SEEK(lcCtnSty+STR(lnOrdLine,6),lcPckLin)
    IF llGma
      lcExpGma2 = "SEEK(&lcTmpPck..Pack_ID+&lcTmpPck..cPkColor+&lcTmpPck..cPckSize+&lcTmpPck..cPkVersion+lcCtnSty+STR(lnOrdLine,6),lcPckLin)"
    ELSE
      lcExpGma2 = "SEEK(lcCtnSty+STR(lnOrdLine,6),lcPckLin)"
    ENDIF
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
=IIF( lnActFolder = 1 , lfStrsBr() , .T. )
=IIF( lnActFolder = 2 , lfDtlBrow() , .T. )  

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
*B606394
FUNCTION lfUpdSumFl

lcCurAlis = ALIAS()
SELECT(lcSumPck) 
lcExpr = IIF(llApply,"cSelect = '»'",".T.")
SCAN FOR &lcExpr 
  IF &lcSumPck..llPack AND !(&lcSumPck..lRange)
    llGetPack  =SEEK('P'+laData[4]+PADR(Pack_Id,16)+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')
    IF !llGetPack 
      = SEEK('P*****'+PADR(Pack_Id,16)+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')
    ENDIF 
    lnOqty = &lcSumPck..OTotQty / SPCK_HDR.npckqty
    lnPqty = &lcSumPck..PTotQty / SPCK_HDR.npckqty
    REPLACE OTotQty WITH lnOqty,;
            PTotQty WITH lnPqty
  ENDIF          
ENDSCAN
REPLACE ALL &lcSumPck..cSelect WITH IIF(!llClrSel AND !EMPTY(&lcSumPck..cSelect),'»','')

SELECT (lcCurAlis)

*:**************************************************************************
*:* Name        : lfStrsBr
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Stores browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfStrsBr()
*:***************************************************************************
FUNCTION lfStrsBr

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

SELECT (lcStores)

lnStBrRec = RECNO()
lcBrFlds = "cMarker =IIF(RECNO()=lnStBrRec,'>',' '):H= ' ':R:1:W=.F.,"+;
           "cSelect  :H = ' '    :1 :R,"+;
           "Store    :H = ' Store  ':R,"+;
           "PikTkt   :H = 'PikTkt'  :R,"+;
           "TOTORD   :H = 'Order '  :R,"+;
           "TOTPIK   :H = 'Picked'  :R,"+;
           "Cartons  :H = 'Cartons ':R,"+;
           "Weight   :H = 'Weight  ':R,"+;
           "TOTPQty  :H = 'P.Qty   ':R,"+;
           "PACK_NO  :H = 'P/L#  '  :R,"+;
           "BOL_NO   :H = 'BOL   '  :R,"+;
           "DIST_CTR :H = ' D.C.#  ':R"    
           

  BROWSE FIELDS &lcBrFlds;
           SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
           TITLE(lcStoBrTtl) WHEN lfwStrsBr() ;
           VALID :F lfvStrsBr()  ;
           WINDOW (lcWinHdr1) IN WINDOW (lcWinHdr)

= lfwStrsBr()

*:**************************************************************************
*:* Name        : lfwStrsBr
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : When functions for stores browse
*:***************************************************************************
FUNCTION lfwStrsBr
PRIVATE lnAlias,laSelct,lcOpnTmpl


lcSelStt = IIF((laScrMode[3] OR llNew) AND lcDtl2Stat = 'ENABLE','ENABLE','DISABLE')
lnAlias = SELECT(0)

SELECT (lcStores)
lnStBrRec = RECNO()
SHOW WINDOW (lcStoBrTtl) REFRESH SAME

DIMENSION laSelct[1]
laSelct = .F.
SELECT COUNT(cSelect) FROM (lcStores) WHERE !EMPTY(lcStores) INTO ARRAY laSelct
lcGenPLStat = IIF(!EMPTY(laSelct[1]),'ENABLE','DISABLE')
SHOW GET pbGenPL &lcGenPLStat

lcOpnTmpl = IIF(!EMPTY(laData[2]),'ENABLE','DISABLE')
SHOW GET pbOpnTmpl &lcOpnTmpl
SELECT (lcStores)

SELECT(lnAlias)

*:**************************************************************************
*:* Name        : lfvStrsBr
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Valid funtion for Stores browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvStrsBr()
*:***************************************************************************
FUNCTION lfvStrsBr

IF WONTOP() # (lcStoBrTtl)
  =gfStopBrow()
ENDIF

*:**************************************************************************
*:* Name        : lfGtOrdSto
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Get Order stores
*:***************************************************************************
FUNCTION lfGtOrdSto
PRIVATE lcSvOrd,lcGtStore,lnStoOrdrd,lcSvRel,lcSvPkOrd
lnStoOrdrd = 0
lcSvOrd = ORDER('ORDLINE')
lcSvPkOrd = ORDER('PIKLINE')
SET ORDER TO TAG ORDLINST IN ORDLINE
SET ORDER TO PIKLINEO IN PIKLINE
SELECT PIKLINE
lcSvRel = SET('RELATION')
SET RELATION TO
GO TOP

lcGtStore = ' '
=SEEK('O'+laData[2],'ORDLINE')
SELECT ORDLINE
SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+laData[2]  
  IF lcGtStore # ORDLINE.STORE
    lcGtStore = ORDLINE.STORE    
    =SEEK('S'+ORDLINE.ACCOUNT+ORDLINE.STORE,'CUSTOMER')
    =SEEK(laData[2]+ORDLINE.STORE,'PACK_HDR')
    INSERT INTO (lcStores) (DIST_CTR, STORE, PIKTKT, TOTORD, TOTPIK, PACK_NO, TOTPQTY, WEIGHT, CARTONS, BOL_NO ) ; 
                    VALUES (CUSTOMER.DIST_CTR, ORDLINE.STORE, ORDLINE.PIKTKT, ORDLINE.TOTBOOK, ;
                            ORDLINE.TOTPIK, PACK_HDR.PACK_NO, PACK_HDR.TOT_PCS , PACK_HDR.TOT_WGHT ,PACK_HDR.TOT_CART,;
                            PACK_HDR.BILL_LADG )
    IF SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'PIKLINE')
      SELECT (lcStores)
      REPLACE PIKTKT WITH PIKLINE.PIKTKT,;
              TOTPIK WITH PIKLINE.TOTPIK
    ENDIF                        
  ELSE
    REPLACE &lcStores..TOTORD WITH &lcStores..TOTORD + ORDLINE.TOTBOOK,;
            &lcStores..TOTPIK WITH &lcStores..TOTPIK + ;
            IIF(SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'PIKLINE'),PIKLINE.TOTPIK,ORDLINE.TOTPIK)
  ENDIF
ENDSCAN

GO TOP IN &lcStores
IF !WEXIST(lcStoBrTtl)
  = lfStrsBr()
ELSE
  = lfwStrsBr()
ENDIF

SELECT PIKLINE
SET RELATION TO &lcSvRel

SET ORDER TO &lcSvPkOrd IN PIKLINE
SET ORDER TO &lcSvOrd IN ORDLINE
*-- end of lfGtOrdSto.

*:**************************************************************************
*:* Name        : lfvGenPL
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Generate automatic P/L
*:***************************************************************************
*This funtion do the target of all the project ,
*Its pourpos to generate automatic packing lists based on previously saved tamplets.

FUNCTION lfvGenPL
*- lcCurs  : This cursor saves # of cartons saved in tamplate file and calculate the # of
*-           cartons based on actual ordered/picked qty for the current store

PRIVATE lcCurs,lcI,lnSvRec,lnI,lnCartons,lnJ,lnCrtSum,lcCurrSty

IF laScrMode[2]
  RETURN 
ENDIF  

=SEEK('O'+laData[2],'ORDHDR')
IF ORDHDR.STATUS = 'C'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is invoiced, can not generate!')
  RETURN
ENDIF

SELECT &lcStores
lnSvRec = RECNO(lcStores)

*-- If no store selected , exit!
LOCATE FOR cSelect = '»'
IF !FOUND()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Store selected.')
  RETURN
ENDIF 

lcCurs = gfTempName()

SELECT TMPL_HDR
=SEEK(laData[2],'TMPL_HDR')

SELECT &lcPckLin 
lcRelation = SET('RELATION')
lcSkip = SET('SKIP')
lnOrder = ORDER(lcPckLin)

SET RELATION TO
SET ORDER TO &lcPakIndxSt

lnCrtHOrd = ORDER(lcCtnHdr)
SET ORDER TO STORE IN (lcCtnHdr)
lnCrtDOrd  = ORDER(lcCtnDtl)
SET ORDER TO (lcCtnDtl) IN (lcCtnDtl)

lcErorFil = gfTempName()
CREATE CURSOR (lcErorFil) (cError C(80))

llGenrted = .F.    && Set this to .T. if at least one pack is generated

*-- Check if there are errors
=lfChkErrs()      
llNoErr = ( RECCOUNT(lcErorFil) = 0 )
llContinue = .T.
IF RECCOUNT(lcErorFil) > 0
  lcErrText = 'Store# |Style    SIZE|Ctn.|Error'
  =lfErrDsply()
  IF llContinue .AND. TMPL_HDR.LLCKONERR
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is locked on errors, can not create P/L for all selected stores')
  ENDIF
ENDIF

*-- Generating P/Ls
IF llContinue .AND. ;
   (!TMPL_HDR.LLCKONERR  .OR. (TMPL_HDR.LLCKONERR .AND. llNoErr))
  SELECT &lcStores
  GO TOP
  SCAN FOR cSelect = '»'
    lnCrtSum = 0         && Variable holds # of cartons generated for the current store

    SELECT &lcPckLin    
    *B123477,1  TMI [Start] Move this filter after calling the "=lfCtnOrg" function to utilize from the cursor lcCur
    *\* *C122070,1 HBG 04/04/2004 Filter for None Range Packs[Begin]
    *\* *SET FILTER TO STORE = &lcStores..STORE
    *\* SET FILTER TO STORE = &lcStores..STORE AND !lRange
    *\* *C122070,1 [End]
    *B123477,1  TMI [End  ] 
    GO TOP
    
    *-- Build a list of numbers resembling cartons that will be generated for this store
*   CREATE CURSOR &lcCurs (NCARTON N(4),NCOUNT N(4),NFROM N(4))
    CREATE TABLE (gcWorkDir + lcCurs) (NCARTON N(4),NCOUNT N(4),NFROM N(4))
    INDEX ON NCARTON TAG (lcCurs)
    =lfCtnOrg()      && Generate cartons numbers , filling "lcCurs" cursor
    
    WAIT WINDOW NOWAIT 'Generating Cartons for store '+&lcStores..STORE + ' '+IIF(!EMPTY(&lcStores..DIST_CTR),'D.C. ' +&lcStores..DIST_CTR , '')
    SELECT &lcPckLin
    *B123477,1  TMI [Start] get cartons for non Range packs only , range packs is treated in a separte funtion
    SET FILTER TO STORE = &lcStores..STORE AND !lRange
    *B123477,1  TMI [End  ] 
    GO TOP 

    *--Loop for all lines in template file for this store
    =SEEK(laData[2],'TMPL_LIN')
    SELECT TMPL_LIN    
    SCAN REST WHILE ORDER+STR(NO_CART,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = laData[2] ;
              FOR TMPL_LIN.TOTQTY > 0
      lcCurrSty = TMPL_LIN.PACK_ID+TMPL_LIN.CPKCOLOR+TMPL_LIN.CPCKSIZE+TMPL_LIN.CPKVERSION+TMPL_LIN.STYLE                
      IF SEEK(lcCurrSty,lcPckLin)      
        SELECT &lcPckLin
        SCAN REST WHILE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+STR(nOrdLineNo,6) = lcCurrSty
          *-- laQty : This array holds the availabel qty for the current line in lcPckLin file
          *--         from the equation CEILING(&lcPckLin..AvlQty1/TMPL_LIN.QTY1) we get the # of cartons
          *--         needed for the qty laQty[1,1] , we get the max of this equation for sizes 1,...,8 
          *-          save this value in the lnCartons variable
          DIMENSION laQty[8,3]
          laQty = 0
          lnCartons = 0    
          FOR lnI = 1 TO 8        
            lcI = STR(lnI,1)
            laQty[lnI,1] = &lcPckLin..AvlQty&lcI
            laQty[lnI,2] = TMPL_LIN.QTY&lcI
            laQty[lnI,3] = IIF( TMPL_LIN.QTY&lcI>0 , &lcPckLin..AvlQty&lcI/TMPL_LIN.QTY&lcI , 0 )
            IF laQty[lnI,3] > lnCartons
              lnCartons = CEILING(laQty[lnI,3])    && Get max number of cartons to hold all pieces
            ENDIF
          ENDFOR
          SELECT &lcPckLin
          REPLACE PQTY1   WITH OQTY1,;
                  PQTY2   WITH OQTY2,;
                  PQTY3   WITH OQTY3,;
                  PQTY4   WITH OQTY4,;
                  PQTY5   WITH OQTY5,;
                  PQTY6   WITH OQTY6,;
                  PQTY7   WITH OQTY7,;
                  PQTY8   WITH OQTY8,;
                  PTOTQTY WITH PQTY1+PQTY2+PQTY3+PQTY4+PQTY5+PQTY6+PQTY7+PQTY8,;
                  PWGH1   WITH PQTY1*WEIGHT1,;
                  PWGH2   WITH PQTY2*WEIGHT2,;
                  PWGH3   WITH PQTY3*WEIGHT3,;
                  PWGH4   WITH PQTY4*WEIGHT4,;
                  PWGH5   WITH PQTY5*WEIGHT5,;
                  PWGH6   WITH PQTY6*WEIGHT6,;
                  PWGH7   WITH PQTY7*WEIGHT7,;
                  PWGH8   WITH PQTY8*WEIGHT8,;
                  PTOTWGH WITH PWGH1+PWGH2+PWGH3+PWGH4+PWGH5+PWGH6+PWGH7+PWGH8   
          =SEEK(TMPL_LIN.STYLE,'STYLE')
          =SEEK('S'+STYLE.SCALE,'SCALE')
          =SEEK(TMPL_LIN.NCARTON,lcCurs)
          lnPackNo = 0
          IF !EMPTY(&lcPckLin..PACK_ID)            
            lcSvOrd = ORDER('SPCK_LIN')
            SET ORDER TO SPCK_LINVR IN SPCK_LIN   &&KEY : TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE
            SELECT &lcPckLin
            IF SEEK('P'+laData[4]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'SPCK_LIN') OR ;
               SEEK('P*****'+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'SPCK_LIN')
              lnPackNo = PTOTQTY/SPCK_LIN.TOTQTY
            ENDIF
            SET ORDER TO &lcSvOrd IN SPCK_LIN
          ENDIF
          
          FOR lnCrtn = 1 TO lnCartons        
            SELECT (lcCtnDtl)
            APPEND BLANK
            REPLACE CART_NO WITH lnCrtn+&lcCurs..nFrom,;
                    STORE      WITH &lcStores..STORE,;
                    nOrdLineNo WITH &lcPckLin..nOrdLineNo,;
                    STYLE      WITH TMPL_LIN.STYLE,;
                    SZCNT      WITH SCALE.CNT,;
                    SIZE1      WITH SCALE.SZ1,;
                    SIZE2      WITH SCALE.SZ2,;
                    SIZE3      WITH SCALE.SZ3,;
                    SIZE4      WITH SCALE.SZ4,;
                    SIZE5      WITH SCALE.SZ5,;
                    SIZE6      WITH SCALE.SZ6,;
                    SIZE7      WITH SCALE.SZ7,;
                    SIZE8      WITH SCALE.SZ8
            REPLACE QTY1       WITH IIF(laQty[1,1] > 0 , MIN(laQty[1,1],TMPL_LIN.QTY1) , 0 ),;
                    QTY2       WITH IIF(laQty[2,1] > 0 , MIN(laQty[2,1],TMPL_LIN.QTY2) , 0 ),;
                    QTY3       WITH IIF(laQty[3,1] > 0 , MIN(laQty[3,1],TMPL_LIN.QTY3) , 0 ),;
                    QTY4       WITH IIF(laQty[4,1] > 0 , MIN(laQty[4,1],TMPL_LIN.QTY4) , 0 ),;
                    QTY5       WITH IIF(laQty[5,1] > 0 , MIN(laQty[5,1],TMPL_LIN.QTY5) , 0 ),;
                    QTY6       WITH IIF(laQty[6,1] > 0 , MIN(laQty[6,1],TMPL_LIN.QTY6) , 0 ),;
                    QTY7       WITH IIF(laQty[7,1] > 0 , MIN(laQty[7,1],TMPL_LIN.QTY7) , 0 ),;
                    QTY8       WITH IIF(laQty[8,1] > 0 , MIN(laQty[8,1],TMPL_LIN.QTY8) , 0 ),;
                    TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
  
            *B123477,3  TMI [Start] Fix Calculating totWight in temp file
            *REPLACE WEIGHT1    WITH IIF(Qty1 > 0 , QTY1*TMPL_LIN.WEIGHT , 0 ),;
            *        WEIGHT2    WITH IIF(Qty2 > 0 , QTY2*TMPL_LIN.WEIGHT , 0 ),;
            *        WEIGHT3    WITH IIF(Qty3 > 0 , QTY3*TMPL_LIN.WEIGHT , 0 ),;
            *        WEIGHT4    WITH IIF(Qty4 > 0 , QTY4*TMPL_LIN.WEIGHT , 0 ),;
            *        WEIGHT5    WITH IIF(Qty5 > 0 , QTY5*TMPL_LIN.WEIGHT , 0 ),;
            *        WEIGHT6    WITH IIF(Qty6 > 0 , QTY6*TMPL_LIN.WEIGHT , 0 ),;
            *        WEIGHT7    WITH IIF(Qty7 > 0 , QTY7*TMPL_LIN.WEIGHT , 0 ),;
            *        WEIGHT8    WITH IIF(Qty8 > 0 , QTY8*TMPL_LIN.WEIGHT , 0 ),;
            *        TOTWEIGHT  WITH WEIGHT1+WEIGHT2+WEIGHT3+WEIGHT4+;
            *            			WEIGHT7+WEIGHT6+WEIGHT7+WEIGHT8
            REPLACE WEIGHT1    WITH IIF(Qty1 > 0 , QTY1*TMPL_LIN.WEIGHT , 0 ),;
                    WEIGHT2    WITH IIF(Qty2 > 0 , QTY2*TMPL_LIN.WEIGHT , 0 ),;
                    WEIGHT3    WITH IIF(Qty3 > 0 , QTY3*TMPL_LIN.WEIGHT , 0 ),;
                    WEIGHT4    WITH IIF(Qty4 > 0 , QTY4*TMPL_LIN.WEIGHT , 0 ),;
                    WEIGHT5    WITH IIF(Qty5 > 0 , QTY5*TMPL_LIN.WEIGHT , 0 ),;
                    WEIGHT6    WITH IIF(Qty6 > 0 , QTY6*TMPL_LIN.WEIGHT , 0 ),;
                    WEIGHT7    WITH IIF(Qty7 > 0 , QTY7*TMPL_LIN.WEIGHT , 0 ),;
                    WEIGHT8    WITH IIF(Qty8 > 0 , QTY8*TMPL_LIN.WEIGHT , 0 ),;
                    TOTWEIGHT  WITH WEIGHT1+WEIGHT2+WEIGHT3+WEIGHT4+;
                        			WEIGHT5+WEIGHT6+WEIGHT7+WEIGHT8
            *B123477,3  TMI [End  ]             
            REPLACE BR1        WITH !EMPTY(QTY1),;
                    BR2        WITH !EMPTY(QTY2),;
                    BR3        WITH !EMPTY(QTY3),;
                    BR4        WITH !EMPTY(QTY4),;
                    BR5        WITH !EMPTY(QTY5),;
                    BR6        WITH !EMPTY(QTY6),;
                    BR7        WITH !EMPTY(QTY7),;
                    BR8        WITH !EMPTY(QTY8)                
                    
            *-- Update here Pack_id,pksize,packcolor,pkversion ,nPackNo       
            IF !EMPTY(&lcPckLin..PACK_ID)
              REPLACE PACK_ID    WITH SPCK_LIN.PACK_ID,;
                      cPkColor   WITH SPCK_LIN.cPkColor,;
                      cPCkSize   WITH SPCK_LIN.cPckSize,;
                      cPKVersion WITH SPCK_LIN.cPkVersion,;
                      nPackNO    WITH lnPackNo  
            ENDIF
    
            *- Decrease availabel qty to update next cartons ( specially last one )
            FOR lnJ = 1 TO 8                  
              lcJ = STR(lnJ,1)
              laQty[lnJ,1] = MAX( laQty[lnJ,1] - QTY&lcJ , 0 )
            ENDFOR                
    
            IF !SEEK(&lcStores..STORE+STR(lnCrtn+&lcCurs..nFrom,4),lcCtnHdr)
              INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,Empty,STORE);
                                  VALUES (lnCrtn+&lcCurs..nFrom,0,;
                                          'N',&lcStores..STORE)
              lnCrtSum = lnCrtSum + 1                              
            ENDIF
            REPLACE &lcCtnHdr..TotPcs WITH &lcCtnHdr..TotPcs + &lcCtnDtl..TotQty,;
                    &lcCtnHdr..TotWgh WITH &lcCtnHdr..TotWgh + &lcCtnDtl..TOTWEIGHT
    
            lnPackWgh = lnPackWgh + &lcCtnDtl..TOTWEIGHT
                    
            SELECT &lcStores
            REPLACE TOTPQty WITH TOTPQty + &lcCtnDtl..TotQty ,;
                    WEIGHT  WITH WEIGHT  + &lcCtnDtl..TOTWEIGHT
          ENDFOR
        ENDSCAN
      ENDIF    
    ENDSCAN
    *C122070,1 HBG 04/04/2004 Update Range Packs in cartons[Begin]
    =lfUpdRngPk()
    *C122070,1 [End]
    SELECT &lcStores
    REPLACE CARTONS WITH lnCrtSum ,;
            PACK_NO WITH "######" ,;
            cSelect WITH ' '
    llGenrted = .T.
  ENDSCAN
ENDIF


IF llGenrted
  =lfwStrsBr()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Packing Lists are generated.')
  LOCATE FOR TOTORD # TOTPQTY AND !EMPTY(PACK_NO)
  IF FOUND()
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Total Packed Qty not equal to Total OrdQty for some stores.')  
  ENDIF
ENDIF

SELECT &lcPckLin
SET RELATION TO &lcRelation
SET SKIP TO &lcSkip
SET ORDER TO (lnOrder)

SET ORDER TO &lnCrtHOrd IN (lcCtnHdr)

IF USED(lcCurs)
  USE IN &lcCurs 
ENDIF

IF BETWEEN(lnSvRec,1,RECCOUNT(lcStores))
  GOTO (lnSvRec) IN (lcStores)
ENDIF
*-- end of lfvGenPl.

*:**************************************************************************
*:* Name        : lfChkErrs
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Check errors for tempate
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfChkErrs()
*:***************************************************************************
FUNCTION lfChkErrs
PRIVATE lnCnt,lnCartons,lnAlias,lcSvOrder,lcSvOrder2,lnI,lcI,lcCurrSty
lnAlias = SELECT()


lcSvOrder  = ORDER(lcPckLin)
lcSvOrder2 = ORDER('TMPL_LIN')
SET ORDER TO STORE IN (lcPckLin)
SET ORDER TO TAG TMPL_LINS IN TMPL_LIN

SELECT &lcPckLin
SET FILTER TO 
GO TOP

SELECT &lcStores
GO TOP

SCAN FOR cSelect = '»'
  
  lcMissed = ' '
  llAdd = .F.
  SELECT (lcPckLin)
  IF SEEK(&lcStores..STORE,lcPckLin)
    SCAN REST WHILE STORE = &lcStores..STORE
      *C122070,1 HBG 07/04/2004 Don't check if all styles are in Tmpl_lin file in case of range packs[Begin]
      IF lRange
        LOOP
      ENDIF 
      *C122070,1[End]
      *-- Check if style is missing in TMPL_LIN file
      lcCurrSty = laData[2]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE
      IF !SEEK(laData[2]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'TMPL_LIN') .AND. !(STYLE $ lcMissed)
      
        *B607375  TMI [Start] Fix the bug that wrong missed styles in log file
        *INSERT INTO (lcErorFil) VALUES (&lcStores..STORE+'      | '+ORDLINE.STYLE+'  | MISSED')
        *lcMissed = lcMissed + ORDLINE.STYLE + '|'
        INSERT INTO (lcErorFil) VALUES (&lcStores..STORE+'      | '+&lcPckLin..STYLE+'  | MISSED')
        lcMissed = lcMissed + &lcPckLin..STYLE + '|'
        *B607375  TMI [End  ] Fix the bug that wrong missed styles in log file
        llAdd = .T.
        LOOP
      ENDIF
      
      *-- If style is in TMPL_LIN check that open qty is divisible by qty Ctn.

      *-- validate each size qty/ctn separately.
      *-- Apply non division only if AvlQty is greater than template qty , reverse case means 
      *-- that all qty will be placed in one carton      
      SELECT TMPL_LIN
      SCAN REST WHILE ORDER+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = lcCurrSty
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          IF TMPL_LIN.QTY&lcI > 0 .AND. ;
            &lcPckLin..AvlQty&lcI > TMPL_LIN.QTY&lcI .AND. ;
            MOD( &lcPckLin..AvlQty&lcI , TMPL_LIN.QTY&lcI ) > 0
            INSERT INTO (lcErorFil) VALUES (&lcStores..STORE+'    | '   +;
                                                &lcPckLin..STYLE+'|'    +;
                                                &lcPckLin..CSIZE&lcI+'|  '+;
                                                ALLT(STR(TMPL_LIN.NCARTON))+;
                                                '  | Division '+;
                                                'O.Qty='+LTRIM(STR(&lcPckLin..AvlQty&lcI))+;
                                                ', Qty/Ctn='+LTRIM(STR(TMPL_LIN.QTY&lcI)))
            llAdd = .T.  
          ENDIF
        ENDFOR
      ENDSCAN
      
    ENDSCAN  
  ENDIF
  
  *-- Add a separator after each store
  IF llAdd
    INSERT INTO (lcErorFil) VALUES (REPL('=',80))
  ENDIF
ENDSCAN

SET ORDER TO &lcSvOrder  IN (lcPckLin)
SET ORDER TO &lcSvOrder2 IN TMPL_LIN


*-*   SELECT TMPL_LIN
*-*   =SEEK(laData[2],'TMPL_LIN')
*-*   SCAN REST WHILE ORDER+STR(NO_CART,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = laData[2] ;
*-*             FOR TMPL_LIN.TOTQTY > 0 
*-*     IF SEEK(PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,lcPckLin) .AND. &lcPckLin..STORE = &lcStores..STORE
*-*       lnPSum = IIF(QTY1>0,&lcPckLin..AvlQty1,0) + IIF(QTY2>0,&lcPckLin..AvlQty2,0) + ;
*-*                IIF(QTY3>0,&lcPckLin..AvlQty3,0) + IIF(QTY4>0,&lcPckLin..AvlQty4,0) + ;
*-*                IIF(QTY5>0,&lcPckLin..AvlQty5,0) + IIF(QTY6>0,&lcPckLin..AvlQty6,0) + ;
*-*                IIF(QTY7>0,&lcPckLin..AvlQty7,0) + IIF(QTY8>0,&lcPckLin..AvlQty8,0)       
*-*       
*-*       IF MOD( lnPSum , TMPL_LIN.TOTQTY ) > 0
*-*         INSERT INTO (lcErorFil) VALUES (&lcStores..STORE+'      | '+&lcPckLin..STYLE+'  | Division '+;
*-*                                 'O.Qty='+LTRIM(STR(lnPSum))+', Qty/Ctn='+LTRIM(STR(TMPL_LIN.TOTQTY)))
*-*       ENDIF
*-*     ENDIF
*-*   ENDSCAN
*-* 
*-*   *-- Check if some styles are missing
*-*   PRIVATE lcSavOrd,lcSavOrd2,lcMissed,lcErroSrc
*-*   lcSavOrd = ORDER('TMPL_LIN')
*-*   lcSavOrd2 = ORDER('ORDLINE')
*-*   SET ORDER TO TAG TMPL_LINS IN TMPL_LIN
*-*   SET ORDER TO ORDLINST IN ORDLINE
*-*   lcMissed = ''
*-*   SELECT ORDLINE
*-*   =SEEK('O'+laData[2]+&lcStores..STORE,'ORDLINE')
*-*   SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+laData[2]+&lcStores..STORE
*-*     SELECT ORDLINE
*-*     IF !SEEK(ORDER+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'TMPL_LIN') .AND. !(ORDLINE.STYLE $ lcMissed)
*-*       INSERT INTO (lcErorFil) VALUES (&lcStores..STORE+'      | '+ORDLINE.STYLE+'  | MISSED')
*-*       lcMissed = lcMissed + ORDLINE.STYLE
*-*     ENDIF
*-*   ENDSCAN
*-*   SET ORDER TO TAG &lcSavOrd  IN TMPL_LIN
*-*   SET ORDER TO TAG &lcSavOrd2 IN ORDLINE
*-* 
*-* ENDSCAN
*-*   

SELECT (lnAlias )

*-- end of lfChkErrs.

*:**************************************************************************
*:* Name        : lfCtnOrg
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Carton orgnization
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCtnOrg()
*:***************************************************************************
FUNCTION lfCtnOrg
PRIVATE lnCnt,lnCartons,lnAlias,lcCurrSty,lcOldSty,laAvlQty,lcSto

lnAlias = SELECT()

STORE ' ' TO lcCurrSty,lcOldSty
DIMENSION laAvlQty[1,9]
*B123477,1  TMI [Start] Comment these lines
*/* *C122070,1 HBG 04/04/2004 Change the order of ORDLINE file to handle Range Packs[Begin]
*/* lclstOrdOr  = ORDER('ORDLINE')
*/* SET ORDER TO PACK_ID IN ORDLINE
*/* *C122070,1 [End]  
*B123477,1  TMI [End  ] 

SELECT TMPL_LIN
=SEEK(laData[2],'TMPL_LIN')
SCAN REST WHILE ORDER+STR(NO_CART,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = laData[2] ;
          FOR TMPL_LIN.TOTQTY > 0           
  *B123477,1  TMI [Start] Use lcCurs temp file in all cases
  */* *C122070,1 HBG 04/04/2004 Skip Range packs [Begin]
  */* =SEEK('O'+laData[2]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'ORDLINE')
  */* IF ORDLINE.lRange
  */*   LOOP
  */* ENDIF
  */* *C122070,1 [End]        
  *B123477,1  TMI [End  ] 
  lcCurrSty = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE
  *B123477,1  TMI [Start] Get pack id in case of range packs
  lcCurrPck = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION
  *B123477,1  TMI [End  ] 
  *-- This summation to array is to deal with the case that in ordline file for the same style 
  *-- a line is inserted for each size with onle qty>0 is for this size
  IF lcOldSty # lcCurrSty
    laAvlQty = 0
    lcOldSty = lcCurrSty  
    lcSto = &lcStores..STORE
    
    SELECT &lcPckLin
    *B123477,1  TMI [Start] Get sum of all pack qtys in case of (none)/range packs
    *SELECT SUM(AVLQTY1),SUM(AVLQTY2),SUM(AVLQTY3),SUM(AVLQTY4),;
    *       SUM(AVLQTY5),SUM(AVLQTY6),SUM(AVLQTY7),SUM(AVLQTY8),SUM(AvlTotQty) ;
    *FROM &lcPckLin ;
    *WHERE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = lcCurrSty .AND. STORE = lcSto ;        
    *INTO ARRAY laAvlQty
    =SEEK(lcCurrSty,lcPckLin)
    lcPckOrSty = IIF(&lcPckLin..LRANGE,lcCurrPck,lcCurrSty)
    SELECT SUM(AVLQTY1),SUM(AVLQTY2),SUM(AVLQTY3),SUM(AVLQTY4),;
           SUM(AVLQTY5),SUM(AVLQTY6),SUM(AVLQTY7),SUM(AVLQTY8),SUM(AvlTotQty) ;
    FROM &lcPckLin ;
    WHERE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = lcPckOrSty .AND. STORE = lcSto ;        
    INTO ARRAY laAvlQty
    *B123477,1  TMI [End  ] 
  ENDIF
  IF laAvlQty[9] > 0
*-*    lnPSum = IIF(QTY1>0,&lcPckLin..AvlQty1,0) + IIF(QTY2>0,&lcPckLin..AvlQty2,0) + ;
*-*             IIF(QTY3>0,&lcPckLin..AvlQty3,0) + IIF(QTY4>0,&lcPckLin..AvlQty4,0) + ;
*-*             IIF(QTY5>0,&lcPckLin..AvlQty5,0) + IIF(QTY6>0,&lcPckLin..AvlQty6,0) + ;
*-*             IIF(QTY7>0,&lcPckLin..AvlQty7,0) + IIF(QTY8>0,&lcPckLin..AvlQty8,0)       
    lnPSum = IIF(TMPL_LIN.QTY1>0,laAvlQty[1],0) + IIF(TMPL_LIN.QTY2>0,laAvlQty[2],0) + ;
             IIF(TMPL_LIN.QTY3>0,laAvlQty[3],0) + IIF(TMPL_LIN.QTY4>0,laAvlQty[4],0) + ;
             IIF(TMPL_LIN.QTY5>0,laAvlQty[5],0) + IIF(TMPL_LIN.QTY6>0,laAvlQty[6],0) + ;
             IIF(TMPL_LIN.QTY7>0,laAvlQty[7],0) + IIF(TMPL_LIN.QTY8>0,laAvlQty[8],0)
    lnCartons = CEILING(lnPSum/TMPL_LIN.TOTQTY)
    *B123477,1  TMI [Start] Case of range packs
    IF &lcPckLin..LRANGE
      lnCartons = CEILING(laAvlQty[9]/TMPL_LIN.TOTQTY)
    ENDIF
    *B123477,1  TMI [End  ] 

    IF !SEEK(TMPL_LIN.NCARTON,lcCurs)
      INSERT INTO (lcCurs) (NCARTON) VALUES (TMPL_LIN.NCARTON)
    ENDIF
    
    SELECT &lcCurs
    REPLACE NCOUNT WITH MAX( NCOUNT , lnCartons )  && Always put the maximum needed cartons 
  ENDIF
ENDSCAN
  
SELECT (lcCurs)
SET ORDER TO 
GO TOP
lnCnt = 0
*- NFROM : this field is used to calculate starting # for each carton in tamplate to be filled 
*-         in actual cartons
SCAN
  REPLACE NFROM WITH lnCnt
  lnCnt = lnCnt + NCOUNT
ENDSCAN

*B123477,1  TMI [Start] comment these lines
*/* *C122070,1 HBG 04/04/2004 Restore the order of ORDLINE file[Begin]
*/* SET ORDER TO (lclstOrdOr) IN ORDLINE
*/* *C122070,1 [End]
*B123477,1  TMI [End  ] 

SET ORDER TO TAG (lcCurs) IN (lcCurs)
LOCATE

SELECT (lnAlias )

*-- end of lfCtnOrg.
*:**************************************************************************
*:* Name        : lfvSelSto
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Select stores
*:***************************************************************************
FUNCTION lfvSelSto
PARAMETERS lcSelTyp
PRIVATE lnAlias,lnRecno
lnAlias = SELECT()
SELECT &lcStores
lnRecno = RECNO(lcStores)
GO TOP
DO CASE
  CASE  lcSelTyp = 'A'
    REPLACE cSelect WITH '»' FOR EMPTY(PACK_NO)
  
  CASE  lcSelTyp = 'N'
    REPLACE cSelect WITH ' ' ALL

ENDCASE
IF BETWEEN(lnRecno,1,RECCOUNT(lcStores))
  GOTO (lnRecno)
ENDIF  
=lfwStrsBr()
SELECT (lnAlias)
*-- end of lfvSelSto.

*:**************************************************************************
*:* Name        : lfSelCurr
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Select the current record in lcPckLin if it is clicked by mouse
*:***************************************************************************
*:* Called from : ON KEY LABEL LEFTMOUSE
*:***************************************************************************
FUNCTION lfSelCurr
PRIVATE lcPack_Id
CLEAR TYPEAHEAD
lnX =INT(MROW())  && get the mouse row
lnY =INT(MCOL())  && get the mouse column
*-- if the mouse click is inside the browse window 
IF  (MROW(lcWinHdr1)<>-1) AND (MCOL(lcWinHdr1)<>-1) AND (MROW(lcWinHdr1)>lnLineRange);
      AND BETWEEN(MCOL(lcWinHdr1),2,WCOL(lcWinHdr1)-IIF(_DOS OR _UNIX,2,1.77));
     AND BETWEEN(MROW(lcWinHdr1),2,WROWS(lcWinHdr1)-IIF(_DOS OR _UNIX,2,1.77))
  *-- it is the first click
  IF !llClick
    lnTimelimt = SECONDS()
    lnCurR     = lnX
    llClick    = .T.
  ELSE
  *-- if it was the second click then check if it is in the double click time
  *-- range or not if it is the select the record 
    IF SECONDS() < lnTimelimt + _DBLCLICK .AND. lnCurR  = INT(MROW());
       AND  lnX=lnCurR &&INT(ABS(MROW(lcBrowTtl)-lnCurR))=0       
      llClick    = .F.

      IF WONTOP() = lcStoBrTtl
        IF (laScrMode[3].OR.laScrMode[4]).AND.ALIAS()=lcStores .AND. EMPTY(&lcStores..PACK_NO)
          REPLACE cSelect  WITH IIF(cSelect=' ','»',' ')
          =lfwStrsBr()
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
  *-- if the click was outside the browse window then check if the user is selecting
  *-- any object from the control window
  llClick    = .F.
  PUSH KEY
  ON KEY
  lcObjName=lfGetObj(lnX,lnY)
  CLEAR TYPEAHEAD
  *-- if the use is selecting an object from the control window then make
  *-- this object the current object and press it to go to the validation function
  *-- of this object
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
*-- end of lfSelCurr.

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
PRIVATE lnCount
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
*:* Name        : lfMisedSty
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Check if some styles are missing
*:***************************************************************************
FUNCTION lfMisedSty
*-- Check if some styles are missing
PRIVATE lnAlias,lcSavOrd,lcMissed,lcErroSrc        
lnAlias = SELECT()
lcSavOrd = ORDER('TMPL_LIN')
SET ORDER TO TAG TMPL_LINS IN TMPL_LIN

lcMissed = ''

SELECT ORDLINE
=SEEK('O'+laData[2],'ORDLINE')
SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+laData[2]
  SELECT ORDLINE
  IF !SEEK(ORDER+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'TMPL_LIN') .AND. !(ORDLINE.STYLE $ lcMissed)
    INSERT INTO (lcErorFil) VALUES (ORDLINE.STYLE+' is Missed.')
    llError = .T.
  ENDIF
ENDSCAN
SET ORDER TO TAG &lcSavOrd IN TMPL_LIN
SELECT (lnAlias )

*:**************************************************************************
*:* Name        : lfErrDsply
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Error display function 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfErrDsply()
*:***************************************************************************
FUNCTION lfErrDsply
PRIVATE lcErroSrc,lcNote,lcProcStat
lcErroSrc = (gcWorkDir+'Error.TXT')
lcErrTtl = 'Errors Screen.'
lcNote     = IIF(TMPL_HDR.LLCKONERR,'Order is locked on errors','')
lcProcStat = IIF(TMPL_HDR.LLCKONERR,'DISABLE','ENABLE')
DO (gcScrDir + '\ALERRORS.SPR')      && calling the screen ALERRORS
COPY TO (lcErroSrc) SDF

USE IN (lcErorFil)
*-* end of lfErrDsply.

*:**************************************************************************
*:* Name        : lfvErrSrc
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Change the error log text file name
*:***************************************************************************
FUNCTION lfvErrSrc
PRIVATE lcRet,lcSavDef,lcFullPath
lcFullPath = SET('FULLPATH')
lcSavDef = FULLPATH('')
SET DEFA TO (gcWorkDir)
lcRet = GETFILE('TXT','Select File To Save.')
IF !EMPTY(lcRet)
  lcErroSrc = lcRet
ENDIF
SHOW GET lcErroSrc

SET DEFA TO &lcSavDef
SET FULLPATH &lcFullPath
*-- end of lfvErrSrc.

*:**************************************************************************
*:* Name        : lfShowErLg
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/25/2003
*:* Purpose     : Show error log file
*:***************************************************************************
*:* Called from : ALERROR.SCX
*:***************************************************************************
*B607375,1  TMI [End  ] 
FUNCTION lfShowErLg
COPY TO (lcErroSrc) SDF
KEYBOARD [{CTRL+F10}]
MODIFY FILE (lcErroSrc) NOEDIT
*-- end of lfShowErLg.

*:**************************************************************************
*:* Name        : lfOpnTmpl
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*:* Purpose     : Open tamplate screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfOpnTmpl()
*:***************************************************************************
FUNCTION lfOpnTmpl
PRIVATE lcOrder

lcOrder = "'"+laData[2]+"'"
DO gpDoProg WITH 'AWRALAUPLG',.F.,'AL',lcOrder

*-- end of lfOpnTmpl.

*:**************************************************************************
*:* Name        : lfvNavigate
*:* Developer : TMI -TAREK MOHAMED IBRAHIM
*:* Date        : 06/09/2003
*!* Purpose     : Function for navigation(top,bottom,next,priveus).
*:***************************************************************************
FUNCTION lfvNavigate
PRIVATE lnSvRc,lnTop,lnBot
lnAlias=SELECT()
IF laScrMode[2]  
  SELECT TMPL_HDR
  =SEEK(laData[2],'TMPL_HDR')
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

ELSE

  SHOW GET PBTOP  DISABLE
  SHOW GET PBPRVS DISABLE
  SHOW GET PBBTM  DISABLE
  SHOW GET PBNXT  DISABLE

ENDIF
SELECT(lnalias)


*:**************************************************************************
*:* Name        : lfUpdRngPk
*:* Developer   : Hend Ghanem (HBG)
*:* Date        : 07/04/2004
*:* Purpose     : Update Range packs in carton detai file
*:***************************************************************************
*:C122070,1
FUNCTION lfUpdRngPk

IF USED(lcTmpUpLn)
  USE IN (lcTmpUpLn)
ENDIF
CREATE TABLE (gcWorkDir + lcTmpUpLn) (Style C(19),Qty1 N(6,0),Qty2 N(6,0),Qty3 N(6,0),Qty4 N(6,0),;
                           Qty5 N(6,0),Qty6 N(6,0),Qty7 N(6,0),Qty8 N(6,0),TotQty N(7,0),CartNo N(6),;
                           nOrdLineNo N(6),Store C(8),PACK_ID C(16),CPKCOLOR C(6),CPCKSIZE C(3),CPKVERSION C(4))

SELECT (lcCtnDtl)
GO BOTTOM
lnLastCrt = &lcCtnDtl..CART_NO
SET ORDER TO TMPL_LINS IN TMPL_LIN
lnCartNO  = 0

SELECT &lcPckLin
SET FILTER TO
SET FILTER TO STORE = &lcStores..STORE AND lRange
GO TOP
lnRemQty = 0  
lcPack_Id = ""
*B123477,1  TMI [Start] get only records with actual entered qty
*SCAN 
lnPckedQty = 0  && initialize "lnPckedQty" variable
SCAN FOR UOTOTQTY > 0
  *B123477,1  TMI [End  ] 
  IF lcPack_Id <> PACK_ID
    IF !EMPTY(lcPack_Id)
      *B123477,1  TMI [Start] If pack is changed then do start counting cartons from 0 again
      *                       and do not skip to a new carton, when filling the carton detail file
      *                       the field tmpl_lin.ncarton will state if both packs in the same carton or not
      *lnCartNO = lnCartNO + 1
      lnCartNO = 0
      *B123477,1  TMI [End  ] 
    ENDIF
    lcPack_Id = PACK_ID
    *B123477,1  TMI [Start] Get the first line of this pack in tmpl_lin file
    *IF SEEK(laData[2]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'TMPL_LIN')
    IF SEEK(laData[2]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
      *B123477,1  TMI [End  ] 
      *B123477,4  TMI [Start] do not take the totqty that packed in carton, just take the first non zero qty , it will spane to all 
      *lnPckedQty = TMPL_LIN.TotQty
      PRIVATE lnSz
      FOR lnSz = 1 TO 8
        IF EVAL('TMPL_LIN.QTY'+STR(lnSz,1)) > 0
          lnPckedQty = EVAL('TMPL_LIN.QTY'+STR(lnSz,1))
          EXIT
        ENDIF
      ENDFOR
      *B123477,4  TMI [End  ] 
      lnRemQty   = 0
    ENDIF
  ENDIF
  *B123477,3  TMI [Start] if lnPckedQty still 0 then this line was not applied in the tmpl_lin file
  *B123477,4  TMI [Start] comment these lines
  *IF lnPckedQty = 0
  *  LOOP
  *ENDIF
  *B123477,4  TMI [End  ] 
  *B123477,3  TMI [End  ] 
  lnI = 1
  DIME laQty[8]
  STORE 0 TO laQty
  =SEEK(Style,'STYLE') AND SEEK('S'+Style.Scale,'SCALE')
  lnReqQty = IIF(lnRemQty   =0,lnPckedQty ,lnRemQty)
  lnStillReq = lnReqQty 
  *B123477,1  TMI [Start] check also the "UOTOTQTY" field is > 0
  *DO WHILE lnI <= Scale.cnt 
  DO WHILE lnI <= Scale.cnt .AND. UOTOTQTY > 0
    *B123477,1  TMI [End  ] 
    lcI = STR(lnI,1)
    laQty[lnI] = MIN(UpOqty&lcI,lnReqQty)   
    lnReqQty   = MAX(lnReqQty - laQty[lnI],0)
    lnStillReq = lnReqQty
    REPLACE UOTOTQTY   WITH UOTOTQTY   - laQty[lnI],;
            UpOQTY&lcI WITH UpOQTY&lcI - laQty[lnI]
    IF lnReqQty = 0
      lnCartNO   = lnCartNO + 1
      INSERT INTO (lcTmpUpLn) (Style,nOrdLineNo,Store,PACK_ID,CPKCOLOR,CPCKSIZE,CPKVERSION,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,CartNo,TotQty);
       			      VALUES  (&lcPckLin..Style,&lcPckLin..nOrdLineNo,&lcPckLin..Store,&lcPckLin..PACK_ID,&lcPckLin..CPKCOLOR,;
      			               &lcPckLin..CPCKSIZE,&lcPckLin..CPKVERSION,laQty[1],laQty[2],laQty[3],;
   	    		               laQty[4],laQty[5],laQty[6],laQty[7],laQty[8],lnCartNO,laQty[1]+laQty[2]+laQty[3]+;
   		    	               laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8])
      lnReqQty = lnPckedQty 
      *B123477,1  TMI [Start] no remaining qty since we will go to either a new size ( same or other style)
      *                       or there is a remaining qty in the same size , this will be checked via UOTOTQTY field
      lnRemQty = 0
      *B123477,1  TMI [End  ] 
      STORE 0 TO laQty
      IF UpOqty&lcI = 0
        lnI = lnI + 1
      ENDIF
    ELSE
      lnI = lnI + 1
    ENDIF
  ENDDO
  IF lnStillReq <> 0
    lnCartNO   = lnCartNO + 1
    INSERT INTO (lcTmpUpLn) (Style,nOrdLineNo,Store,PACK_ID,CPKCOLOR,CPCKSIZE,CPKVERSION,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,CartNo,TotQty);
    			      VALUES  (&lcPckLin..Style,&lcPckLin..nOrdLineNo,&lcPckLin..Store,&lcPckLin..PACK_ID,&lcPckLin..CPKCOLOR,;
      			               &lcPckLin..CPCKSIZE,&lcPckLin..CPKVERSION,laQty[1],laQty[2],laQty[3],;
   	    		               laQty[4],laQty[5],laQty[6],laQty[7],laQty[8],lnCartNO,laQty[1]+laQty[2]+laQty[3]+;
   		    	               laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8])
    lnCartNO   = lnCartNO - 1
    lnRemQty   = lnStillReq
  ENDIF
ENDSCAN

=lfUpdCtn()

SET ORDER TO TMPL_LIN IN TMPL_LIN

*:**************************************************************************
*:* Name        : lfUpdCtn
*:* Developer   : Hend Ghanem (HBG)
*:* Date        : 07/04/2004
*:* Purpose     : Update Range packs in carton detai file
*:***************************************************************************
*:C122070,1
FUNCTION lfUpdCtn

SELECT (lcTmpUpLn)
SCAN
  lcCurrSty = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE      
  =SEEK(lcCurrSty,lcPckLin)      
  SELECT &lcPckLin
  REPLACE PQTY1   WITH OQTY1,;
          PQTY2   WITH OQTY2,;
          PQTY3   WITH OQTY3,;
          PQTY4   WITH OQTY4,;
          PQTY5   WITH OQTY5,;
          PQTY6   WITH OQTY6,;
          PQTY7   WITH OQTY7,;
          PQTY8   WITH OQTY8,;
          PTOTQTY WITH PQTY1+PQTY2+PQTY3+PQTY4+PQTY5+PQTY6+PQTY7+PQTY8,;
          PWGH1   WITH PQTY1*WEIGHT1,;
          PWGH2   WITH PQTY2*WEIGHT2,;
          PWGH3   WITH PQTY3*WEIGHT3,;
          PWGH4   WITH PQTY4*WEIGHT4,;
          PWGH5   WITH PQTY5*WEIGHT5,;
          PWGH6   WITH PQTY6*WEIGHT6,;
          PWGH7   WITH PQTY7*WEIGHT7,;
          PWGH8   WITH PQTY8*WEIGHT8,;
          PTOTWGH WITH PWGH1+PWGH2+PWGH3+PWGH4+PWGH5+PWGH6+PWGH7+PWGH8   

   =SEEK(&lcTmpUpLn..STYLE,'STYLE') AND SEEK('S'+STYLE.SCALE,'SCALE')
   *B123477,1  TMI [Start] Get the carton # related to all other created cartons in the tamplate file
   =SEEK(laData[2]+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
   =SEEK(TMPL_LIN.NCARTON,lcCurs) 
   lnLastCrt = &lcCurs..nFrom
   *B123477,1  TMI [End  ]
   
   SELECT (lcCtnDtl)
   APPEND BLANK
   REPLACE CART_NO    WITH &lcTmpUpLn..CartNO+lnLastCrt ,;
           STORE      WITH &lcTmpUpLn..STORE,;
           nOrdLineNo WITH &lcTmpUpLn..nOrdLineNo,;
           STYLE      WITH &lcTmpUpLn..STYLE,;
           SZCNT      WITH SCALE.CNT,;
           SIZE1      WITH SCALE.SZ1,;
           SIZE2      WITH SCALE.SZ2,;
           SIZE3      WITH SCALE.SZ3,;
           SIZE4      WITH SCALE.SZ4,;
           SIZE5      WITH SCALE.SZ5,;
           SIZE6      WITH SCALE.SZ6,;
           SIZE7      WITH SCALE.SZ7,;
           SIZE8      WITH SCALE.SZ8
   REPLACE QTY1       WITH &lcTmpUpLn..Qty1,;
           QTY2       WITH &lcTmpUpLn..Qty2,;
           QTY3       WITH &lcTmpUpLn..Qty3,;
           QTY4       WITH &lcTmpUpLn..Qty4,;
           QTY5       WITH &lcTmpUpLn..Qty5,;
           QTY6       WITH &lcTmpUpLn..Qty6,;
           QTY7       WITH &lcTmpUpLn..Qty7,;
           QTY8       WITH &lcTmpUpLn..Qty8,;
           TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
  
   *B123477,3  TMI [Start] Fix the wight summing
   *REPLACE WEIGHT1    WITH IIF(Qty1 > 0 , QTY1*&lcPckLin..WEIGHT1 , 0 ),;
   *        WEIGHT2    WITH IIF(Qty2 > 0 , QTY2*&lcPckLin..WEIGHT2 , 0 ),;
   *        WEIGHT3    WITH IIF(Qty3 > 0 , QTY3*&lcPckLin..WEIGHT3 , 0 ),;
   *        WEIGHT4    WITH IIF(Qty4 > 0 , QTY4*&lcPckLin..WEIGHT4 , 0 ),;
   *        WEIGHT5    WITH IIF(Qty5 > 0 , QTY5*&lcPckLin..WEIGHT5 , 0 ),;
   *        WEIGHT6    WITH IIF(Qty6 > 0 , QTY6*&lcPckLin..WEIGHT6 , 0 ),;
   *        WEIGHT7    WITH IIF(Qty7 > 0 , QTY7*&lcPckLin..WEIGHT7, 0 ),;
   *        WEIGHT8    WITH IIF(Qty8 > 0 , QTY8*&lcPckLin..WEIGHT8 , 0 ),;
   *        TOTWEIGHT  WITH WEIGHT1+WEIGHT2+WEIGHT3+WEIGHT4+;
   *                     			WEIGHT7+WEIGHT6+WEIGHT7+WEIGHT8
   REPLACE WEIGHT1    WITH IIF(Qty1 > 0 , QTY1*&lcPckLin..WEIGHT1 , 0 ),;
           WEIGHT2    WITH IIF(Qty2 > 0 , QTY2*&lcPckLin..WEIGHT2 , 0 ),;
           WEIGHT3    WITH IIF(Qty3 > 0 , QTY3*&lcPckLin..WEIGHT3 , 0 ),;
           WEIGHT4    WITH IIF(Qty4 > 0 , QTY4*&lcPckLin..WEIGHT4 , 0 ),;
           WEIGHT5    WITH IIF(Qty5 > 0 , QTY5*&lcPckLin..WEIGHT5 , 0 ),;
           WEIGHT6    WITH IIF(Qty6 > 0 , QTY6*&lcPckLin..WEIGHT6 , 0 ),;
           WEIGHT7    WITH IIF(Qty7 > 0 , QTY7*&lcPckLin..WEIGHT7 , 0 ),;
           WEIGHT8    WITH IIF(Qty8 > 0 , QTY8*&lcPckLin..WEIGHT8 , 0 ),;
           TOTWEIGHT  WITH WEIGHT1+WEIGHT2+WEIGHT3+WEIGHT4+WEIGHT5+WEIGHT6+WEIGHT7+WEIGHT8
   *B123477,3  TMI [End  ]                         			
            
   REPLACE BR1        WITH !EMPTY(QTY1),;
           BR2        WITH !EMPTY(QTY2),;
           BR3        WITH !EMPTY(QTY3),;
           BR4        WITH !EMPTY(QTY4),;
           BR5        WITH !EMPTY(QTY5),;
           BR6        WITH !EMPTY(QTY6),;
           BR7        WITH !EMPTY(QTY7),;
           BR8        WITH !EMPTY(QTY8)                
                   
   *-- Update here Pack_id,pksize,packcolor,pkversion ,nPackNo       
   REPLACE PACK_ID    WITH &lcTmpUpLn..PACK_ID,;
           cPkColor   WITH &lcTmpUpLn..cPkColor,;
           cPCkSize   WITH &lcTmpUpLn..cPckSize,;
           cPKVersion WITH &lcTmpUpLn..cPkVersion,;
           nPackNO    WITH 0  
    
   IF !SEEK(&lcStores..STORE+STR(&lcTmpUpLn..CartNO+lnLastCrt,4),lcCtnHdr)
     INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,Empty,STORE);
                     VALUES (&lcTmpUpLn..CartNO+lnLastCrt,0,'N',&lcStores..STORE)          
     lnCrtSum = lnCrtSum + 1                              
   ENDIF
   REPLACE &lcCtnHdr..TotPcs WITH &lcCtnHdr..TotPcs + &lcCtnDtl..TotQty,;
           &lcCtnHdr..TotWgh WITH &lcCtnHdr..TotWgh + &lcCtnDtl..TOTWEIGHT
    
   lnPackWgh = lnPackWgh + &lcCtnDtl..TOTWEIGHT
                    
   SELECT &lcStores
   REPLACE TOTPQty WITH TOTPQty + &lcCtnDtl..TotQty ,;
           WEIGHT  WITH WEIGHT  + &lcCtnDtl..TOTWEIGHT
ENDSCAN


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