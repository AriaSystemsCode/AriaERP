*************************************************************************
*:   Program file: MfCsSh.PRG
*:  Program desc.: Cutting Tickets/Style PO/Dye order/Inter-Location
*:                 and Material manufacturing order cost sheet  .
*:         System: Aria 4XP
*:      Developer: AHMED MAHER (AMH)
*:           Date: 12/21/2003
*:      Reference: *N119813,1
*:************************************************************************
*: Passed Parameters  : lcTranType,lcTranNo,llAutoGen
*:************************************************************************
*: Modifications      :
*: B125713,1 AMH 12/22/2004 Fix bug of not generate PO csot sheet for styles
*: B125713,1 AMH            has defalut cost sheet and there is no cost sheet
*: B125713,1 AMH            ID saved in POSLN table for these styles.
*: B128478,1 KHM 06/14/2005 Optimize the code
*: B039756,1 KHM 10/08/2005 Fix the bug of using the styinvjl and its not open.
*: B131716,1 WSH 04/27/2006 Return on the C/T cost sheet not working
*: B131720,1 WSH 04/27/2006 The automatic issue does not work on the C/T cost sheet.
*: B607803,1 WAM 10/18/2006 Fix message text
*: B607846,1 MMT 11/29/2006 Fix bug of not updating Poshdr with new operation code when sequance changed,T20061128.0002
*: B607848,1 WAM 11/30/2006 Calculate the required quantity when the same fabric/color is used for all style colors.
*: B607879,1 WAM 12/14/2006 Fix error while issue fabric when cost type is FIFO
*: B607879,1 WAM 12/14/2006 Refresh receive lot screen to reflect open quantities
*: B607883,1 MMT 12/19/2006 Fix bug of not updating History usage after close
*: B607925,1 WAM 01/10/2007 Fix wrong calculation of item component required quantity
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   T20070117.0001
*! N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP 			T20070206.0007
*! B608063,1 MMT 04/29/2007 fix bug of not able to actualize C/T 		T20070415.0003
*! B608268,1 MMT 09/17/2008 Fix bug of wrong duty when style is repeated[T20070806.0018]
*! B608366,1 MMT 11/29/2007 Fix bug of wrong Cost open open Actual cost screen [T20070926.0183]
*! N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*! B608402,1 SSH 11/29/2007 Fix some error in while adding cost elment 6,7
*! B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[T20080207.0001]
*! B608558,1 MMT 05/15/2008 fix bug of wrong issue cost when fabric is added to location [T20080506.0017]
*! C200876,1 TMI 05/02/2008 Adding the BIN locatin triggers
*! B608583,1 WAM 06/16/2008 FIx the problem of doubling average cost when receive non detailed costing styles [T20080610.0009]
*! B608731,1 WAM 10/27/2008 Fix actual cost calculations while close CT [T20081022.0020]
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [T20090122.0014]
*! B608806,1 WAM 02/18/2009 Fix caculation of average unit cost in summary folder [T20090213.0002]
*! E302567,1 MMT 03/17/2009 Change Sql Dcitionary files path for SAAS[Task:T20081225.0022]
*! B608833,1 HES 04/01/2009 Add Standared Fields for BOMLINE File[T20081001.0001]
*! C201141,1 HES 04/26/2009, quote Processing Enbroidery orders for DCC [T20080808.0019]
*! B608863,1 WAM 05/18/2009 Fix calculation of operation total budget quantity [T20090514.0036]
*! B608919,1 MMT 07/05/2009 Fix bug of Using wrong records from Styinvjl while deleting cost sheet[T20081010.0001]
*! B608936,1 MMT 07/14/2009 Fix bug of Deleting PO Cost sheet even it has Shipemnt Cost sheet[T20090625.0002]
*! B609019,1 HES 09/23/2009 Process of generating PO Cost Sheet slow [T20090701.0009]
*! B609089,1 WAM 11/16/2009 Fix update used quanity when use the option "Edit Used" [T20091106.0011]
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[T20100222.0020]
*! B609181,1 MMT 03/21/2010 Fix Error in Cost Sheet screen when 2 users try to close POs [T20100315.0016]
*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [T20091130.0004]
*! B609262,1 WAM 05/20/2010 Recalculate CT unit cost in POSLN and total cost in POSHDR upon saving [T20100519.0024]
*! B609263,1 HES 05/23/2010 PO - Error adding new lines to PO Cost Sheet [T20100513.0013]
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[T20091204.0001]
*! B609360,1 MMT 07/22/2010 Cannot Delete PO Cost sheet if PO has lines received as cancelled[T20100430.0002]
*! B609373,1 MMT 08/05/2010 Fix bug of Material Roll qty incorrect on Style PO Material Returns[T20100707.0007]
*! B609396,1 SMA 08/30/2010 Fix error while receiving cutting ticket manufacturing operation[T20100804.0013]
*! B609477,1 TMI 12/12/2010 fix problem of property llRecPO  is not defined , also add the default path to the dirmain program
*! E303030,1 MAB 12/28/2011 Extend File/Field/Index to NN length instead of 8,10,10
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[PW Conversion Project]
*! B609992,1 MMT 07/09/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013]
*! B609992,2 MMT 07/16/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013]
*! N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[T20110914.0019]
*! B610177,1 SAB 12/16/2012 Fix Problem of closing Cost Sheet for Contributed Invoices [T20121013.0004]
*! B610572,1 TMI 10/31/2013 the property is lcCTKTBOM, not CTktBom , use lcBOMLIN instead of BOMLIN, remove the reference to ICLR [T20131031.0009 Task]
*! B610614,1 TMI 12/08/2013 Fix a  problem when the Fabric WH is different than the Style WH [T20131205.0006 task] 
*! B610723,1 TMI 05/05/2014 fix a problem Session is lost with the new A5 security      [T20140407.0001]
*! B611451,1 HMS 11/12/2017 fix a problem of Deleting po cost sheet creates discount [T20170927.0031]
*! E303957,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL 
*! E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL 
*! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL 
*! B611745,1 Heba HMS, 18/03/2019 - Aria 5 - Error with purchase order [T20190313.0002 ]
*! B612000,1 MMT 12/31/2019 User should not be able to cancel PO cost sheet if related PO is included in AP invoice[T20191213.0004]
*! E611847,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement]
*! B612660,1 MMT 02/20/2023 PO closing gives error due to duplication in BOMLINE[T20230111.0001]
*:************************************************************************
*--- Doc. [Begin]
*--- lcTranType:It's the 1St param. pass to this PRG with the following Type.
*--- 'M' ==> 'CutTkt'
*--- 'I' ==> 'Style Purchase Order'
*--- 'T' ==> 'Material Manufucturing Order'
*--- 'D' ==> 'Dye Order Cost Sheet'
*--- 'N' ==> 'Inter-Locaion PO'
*--- The parameter 'D' is change to 'I' in the program
*--- lcTranNo:It's the transation Number (e.g. POSHDR.PO)
*--- llAutoGen : Third param to generate Cost sheet Auto. incase of called
*---             from cuttkt screen/PO... .
*---
*--- Program Notes.
*---
*--- Throught this prog. you wil be able to describe preifely each cost item
*--- included in this cuctkt , only Hold status are allowed to generate New
*--- c/t . The Style should also has Cost sheet else the prog. will not
*--- allowing you to generate C/T cost sheet.
*--- i.e. descripe each item needed to produce this style/Styles.
*--- We have exactly 5 cost item:
*--- Ex.  Fabric/Trim/MFG Code/Style Comp./Misc.
*--- You can change the caption of this item from system manager.
*--- after you create cost sheet from generate cost sheet button in toolbar
*--- the Folder 'Cost Sheet' will dispay all cost item included in this
*--- cost sheet with the ability to New (Add)/Remove/Modify only these
*--- three butons are enabled till you save this Cost sheet.
*--- when saving the system will ask you if you want to issue first operation
*--- and its cost item (Cost item nedded for this operation Ex. Fabric..)
*--- After saving and in addition to the last three buttons four buttons
*--- will be enabled
*--- Void   : Void cost element (It defferant from remove since
*---          remove you will not be able to call it again
*---          unless you add new one, But here you can easly unvoid it)
*--- Unvoid : Unvoid Cost Element (Only voided one)
*--- Automatic Issue : Issue all cost item (As you select)
*--- Issue/Return    : Issue / Return the cost item from the browse.
*--- The second folder is 'Operation Managments'
*--- You can issue MFG Code from this folder
*--- we have to Distinguish Issue operation and ite cost item
*--- from this folder you can issud operation <Issue Lot>
*--- or issue its cost item from <Auto. Issue> OR
*--- issue the operation itself if you are not linked with AP.
*--- if link with AP you must issue its cost from AP.
*--- cost from <Issue Item>.
*--- the third folder is 'Dateil Req.'
*--- Browse all cost item in detail
*--- Summary Folder
*--- display all Estimated/Actual/Landed cost.
*--- You can not delete If
*--- 1- some item has been receiving
*--- 2- CutTkt is closed.
*--- Doc. [End]

PARAMETERS lcTranType,lcTranNo,llAutoGen

#INCLUDE R:\aria4xp\prgs\mfcssh.h

PRIVATE lcAllVar
lcAllVar = 'lcTranType,lcTranNo,llAutoGen,'+;
'laSetups,laItemSeg,laMfgRFld,laSort,laStyWare,laMatWare,lcLinkCode,lcStatus,lcSession,lcMfgGlAcnt,'+;
'lnAveEst,lnAveLnd,lnAveAct,lnLeadTime,lnOprBud,lnOprRec,lnOprCan,lnOprDam,lnClrPos,lnSizePos,lnSizeLen,'+;
'llExtSizSc,lnOprOpn,lnStyLngth,lnTkStWare,lnTkMtWare,rbSummary,lcTktText,lcMjrHdr,lcMjrMsk,lcItmHdr,lcItmMsk,'+;
'lcNMjrMsk,lcClrMsk,lcTranFile,lcFileKey,lcGlYear,lcGlPeriod,lcFirstOpr,lcTmpTkt,lcTmpCutPik,'+;
'lcContCode,lcContName,lcOperSeq,lcGlDTemp,lcTktSheet,lcDetFile,lcOpenLots,lcOprHdr,lcisslogfile,'+;
'lcOprDet,lcLotsDet,lcRcvFile,lcIssLtFile,lcOprDet1,lcTmpBom,lcTmpStyle,lcTmpFbric,lcIndExp,lcpusort,'+;
'llInHouse,llMfgOpr,llZoom,llIssFirst,cbSum1,cbSum2,cbSum3,cbSum4,cbPerSize,llUseDyelot,llStyCost,llMatCost,'+;
'llOnlyOne,lcPExSign,lcDExSign,lcPUntSin,lcDUntSin,llGenOrNum,lcDyePO,lcTypes,lcTmpWip,'+;
'lcPosHdr,lcPosLn,lcCtktBom,lcBomLine,lcBom,lcMfgOprHd,lcMfgOprDt,'+;
'llPwInst,llFromCtk,lcPWCtkBom,lcDetLin,lcDetLin2,lcPWBom,llHideDet,lcItemlabel,'+;
'lcBusDocu,lcStyType,lcBrowTitl,lcBrowFlds,llStyConfg,lcInvType'
DIMENSION laAllVar[1,1]
STORE '' TO laAllVar
=gfSubStr(lcAllVar,@laAllVar,',')

LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
PRIVATE &laAllVar[lnI,1].
ENDFOR

DECLARE laSetups[23,2],laItemSeg[1],laMfgRFld[7,2],laSort[4,2],laStyWare[1,2],laMatWare[1,2]
STORE ' ' TO lcLinkCode,laItemSeg,lcStatus,lcSession,lcMfgGlAcnt,laBrowArr,laStyWare,laMatWare
STORE 0   TO lnAveEst,lnAveLnd,lnAveAct,lnLeadTime,lnOprBud,lnOprRec,lnOprCan,lnOprDam,lnClrPos
STORE 0   TO lnSizePos,lnSizeLen
llExtSizSc = gfGetMemVar('M_USEEXSSC',oAriaApplication.ActiveCompanyID)
STORE 0   TO lnOprOpn
STORE 1   TO lnStyLngth,lnTkStWare,lnTkMtWare,rbSummary
STORE ''  TO lcTktText,lcMjrHdr,lcMjrMsk,lcItmHdr,lcItmMsk,lcNMjrMsk,lcClrMsk,lcTranFile,lcFileKey
STORE ''  TO lcGlYear,lcGlPeriod,lcFirstOpr,lcTmpTkt,lcTmpCutPik,lcContCode,lcContName,lcOperSeq
STORE ''  TO lcGlDTemp,lcTktSheet,lcDetFile,lcOpenLots,lcOprHdr,lcisslogfile,lcOprDet,lcLotsDet,lcRcvFile,;
lcIssLtFile,lcOprDet1,lcTmpBom,lcTmpStyle,lcTmpFbric,laMfgRFld,laSort,lcIndExp,lcpusort
STORE ''  TO lcPosHdr,lcPosLn,lcCtktBom,lcBomLine,lcBom,lcMfgOprHd,lcMfgOprDt,lcItemLabel
STORE .F. TO llInHouse,llMfgOpr,llZoom,llIssFirst,cbSum1,cbSum2,cbSum3,cbSum4,cbPerSize,llUseDyelot,llStyCost,llMatCost
STORE .F. TO llOnlyOne,llStyConfg,llHideDet
STORE '/' TO lcPExSign,lcDExSign,lcPUntSin,lcDUntSin
llGenOrNum = .F.
lcDyePO = ' '
lcInvType = IIF(lcTranType='T','0002','0001')

*--- We wil support this program to deal with PW module , if this MFg oper.
*--- is consider as operation and has detail operation we will
*--- 1- update Pw file [PWCTKTBOM]
*--- 2- Disable Recieve Butt. in the folder Contractor management.
*--- All the last modification will be applied only if PW module installed.
llPwInst  = .F.
llFromCtk = .T.   && Variable to indicate if we are calling detail operation
&& Screen from this program.
lcPWCtkBom = ''   && Pw Cost sheet temp file.
lcDetLin   = ''   && Pw detail oeration temp file.
lcDetLin2  = ''
lcPWBom    = ''

*Add Flag for opening select window [Start].
*This flag will be set to .T. when calling the custom trigger
*for Cathy Daneil. Otherwise it will be .F.
PRIVATE llCloseCs,llOpenCs
llCloseCs = .F.
llOpenCs = .F.
*[End]

*C200255,1 (Begin) Initialize voucher no
PRIVATE llFrstTime
llFrstTime = .F.
*C200255,1 (End)

lcDyePO    = IIF(lcTranType ='D','D',lcDyePO)
llPwInst   = (OCCURS('PW',oAriaApplication.CompanyInstalledModules)<>0)
llFromCtk  = .T.
*--- There is no need to create temp files if Pw not Installed or the
*--- current module is not Mf
IF llPWInst .AND. lcTranType = "M"
lcPWCtkBom = gfTempName()
lcDetLin   = gfTempName()
lcDetLin2  = gfTempName()
ENDIF
laMfgRFld[1,1] = 'CCONTCODE'
laMfgRFld[1,2] = 'lcContCode'
laMfgRFld[2,1] = 'CCONTNAME'
laMfgRFld[2,2] = 'lcContName'
laMfgRFld[3,1] = 'COPERSEQ'
laMfgRFld[3,2] = 'lcOperSeq'
laMfgRFld[4,1] = 'LINHOUSE'
laMfgRFld[4,2] = 'llInHouse'
laMfgRFld[5,1] = 'LMFGOPR'
laMfgRFld[5,2] = 'llMfgOpr'
laMfgRFld[6,1] = 'LEADTIME'
laMfgRFld[6,2] = 'lnLeadTime'
laMfgRFld[7,1] = 'GLACCOUNT'
laMfgRFld[7,2] = 'lcMfgGlAcnt'
*N000682,1 12/12/2012 TMI Globlization changes[Start] Define the language related
lcHeaderFile  = ' '
LOCAL loFormSet
loFormSet = oAriaApplication
if type('oAriaApplication.HeaderAlias')='U'
loFormSet.AddProperty('HeaderAlias','')
endif
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
lcHeaderFile = ADDBS(UPPER(ALLTRIM(oAriaApplication.LangPath))) + "PRGS\MFCSSH_H.XML"
loFormSet.HeaderAlias = oAriaApplication.GetClassHeaderFile(lcHeaderFile)
ENDIF
*N000682,1 12/12/2012 TMI Globlization changes[Start]


IF !lfCheckMaj()
*--Item structure not found, Cannot Proceed.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('QRM42080B42001','DIALOG',LANG_MFCSSH_ITMSTR)
=gfModalGen('QRM42080B42001','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_ITMSTR,loFormSet.GetHeaderText("LANG_MFCSSH_ITMSTR",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

RETURN
ENDIF
lcSession = gfsequence('GLSESSION')

LOCAL oGetItemMask
oGetItemMask = CREATEOBJECT('GetItemMask')

=oGetItemMask.Do(@laItemSeg,'',lcInvType)
LOCAL lnCount
lnCount = 1
FOR lnCount = 1 TO ALEN(laItemSeg,1)
IF laItemSeg[lnCount,1]='C'
lcClrMsk = laItemSeg[lnCount,3]
lnClrPos = laItemSeg[lnCount,4]
ENDIF

*--Check for existance of color segment in style structure.
IF llExtSizSc .AND. laItemSeg[lnCount,1] = 'S'
*--Get the size length and width.
lnSizePos = laItemSeg[lnCount,4]
lnSizeLen = LEN(laItemSeg[lnCount,3])
ENDIF
ENDFOR

lcMjrHdr   = oGetItemMask.Do("HM","",lcInvType)
lcMjrMsk   = oGetItemMask.Do("PM","",lcInvType)
lcItmHdr   = oGetItemMask.Do("HI","",lcInvType)
lcItmMsk   = oGetItemMask.Do("PI","",lcInvType)
lcNMjrMsk  = oGetItemMask.Do("PN","",lcInvType)

DO CASE
CASE lcTranType = 'M'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTktText  = LANG_MFCSSH_CUTTKT
lcTktText  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CUTTKT,;
loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcBusDocu  = 'P'
lcStyType  = 'U'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrowTitl = LANG_MFCSSH_CTORDERS
lcBrowTitl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CTORDERS,;
loFormSet.GetHeaderText("LANG_MFCSSH_CTORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcBrowFlds = "PO        :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CTHEADER,loFormSet.GetHeaderText("LANG_MFCSSH_CTHEADER",loFormSet.HeaderAlias))+"' ,"+;
"STYLE     :H='"+lcMjrHdr            +"' ,"+;
"STATUS    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_STATUS,loFormSet.GetHeaderText("LANG_MFCSSH_STATUS",loFormSet.HeaderAlias))  +"' ,"+;
"ENTERED   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ENTERED,loFormSet.GetHeaderText("LANG_MFCSSH_ENTERED",loFormSet.HeaderAlias)) +"' ,"+;
"COMPLETE  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_COMPDATE,loFormSet.GetHeaderText("LANG_MFCSSH_COMPDATE",loFormSet.HeaderAlias))+"' ,"+;
"SEASON    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_SEASON,loFormSet.GetHeaderText("LANG_MFCSSH_SEASON",loFormSet.HeaderAlias))  +"' ,"+;
"CDIVISION :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DIVISION,loFormSet.GetHeaderText("LANG_MFCSSH_DIVISION",loFormSet.HeaderAlias))+"' ,"+;
"NSTYORDER :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_BUDGET,loFormSet.GetHeaderText("LANG_MFCSSH_BUDGET",loFormSet.HeaderAlias))  +"' ,"+;
"RECEIVE   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RECEIVE,loFormSet.GetHeaderText("LANG_MFCSSH_RECEIVE",loFormSet.HeaderAlias)) +"' ,"+;
"DAMAGE    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DAMAGED,loFormSet.GetHeaderText("LANG_MFCSSH_DAMAGED",loFormSet.HeaderAlias)) +"' ,"+;
"OPEN      :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_OPEN,loFormSet.GetHeaderText("LANG_MFCSSH_OPEN",loFormSet.HeaderAlias))    +"' ,"+;
"CANCEL    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CANCELED,loFormSet.GetHeaderText("LANG_MFCSSH_CANCELED",loFormSet.HeaderAlias))+"'"

CASE lcTranType = 'I'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTktText  = LANG_MFCSSH_PO
lcTktText  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcBusDocu  = 'P'
lcStyType  = 'P'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrowTitl = LANG_MFCSSH_POORDERS
lcBrowTitl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_POORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_POORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcBrowFlds = "PO        :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))  +"' ,"+;
"STATUS    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_STATUS,loFormSet.GetHeaderText("LANG_MFCSSH_STATUS",loFormSet.HeaderAlias))  +"' ,"+;
"VENDOR    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_VENDOR,loFormSet.GetHeaderText("LANG_MFCSSH_VENDOR",loFormSet.HeaderAlias))  +"' ,"+;
"COMPLETE  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_COMPDATE,loFormSet.GetHeaderText("LANG_MFCSSH_COMPDATE",loFormSet.HeaderAlias))+"' ,"+;
"NSTYORDER :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_BUDGET,loFormSet.GetHeaderText("LANG_MFCSSH_BUDGET",loFormSet.HeaderAlias))  +"' ,"+;
"POTOTAL   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_TOTCOST,loFormSet.GetHeaderText("LANG_MFCSSH_TOTCOST",loFormSet.HeaderAlias)) +"' ,"+;
"RECEIVE   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RECEIVE,loFormSet.GetHeaderText("LANG_MFCSSH_RECEIVE",loFormSet.HeaderAlias)) +"' ,"+;
"OPEN      :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_OPEN,loFormSet.GetHeaderText("LANG_MFCSSH_OPEN",loFormSet.HeaderAlias))    +"'"

CASE lcTranType = 'D'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTktText  = LANG_MFCSSH_DYEORD
lcTktText  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYEORD,loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcBusDocu  = 'P'
lcStyType  = 'D'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrowTitl = LANG_MFCSSH_DOORDERS
lcBrowTitl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DOORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_DOORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcBrowFlds = "PO        :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))  +"' ,"+;
"STATUS    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_STATUS,loFormSet.GetHeaderText("LANG_MFCSSH_STATUS",loFormSet.HeaderAlias))  +"' ,"+;
"VENDOR    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_VENDOR,loFormSet.GetHeaderText("LANG_MFCSSH_VENDOR",loFormSet.HeaderAlias))  +"' ,"+;
"COMPLETE  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_COMPDATE,loFormSet.GetHeaderText("LANG_MFCSSH_COMPDATE",loFormSet.HeaderAlias))+"' ,"+;
"NSTYORDER :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_BUDGET,loFormSet.GetHeaderText("LANG_MFCSSH_BUDGET",loFormSet.HeaderAlias))  +"' ,"+;
"POTOTAL   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_TOTCOST,loFormSet.GetHeaderText("LANG_MFCSSH_TOTCOST",loFormSet.HeaderAlias)) +"' ,"+;
"RECEIVE   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RECEIVE,loFormSet.GetHeaderText("LANG_MFCSSH_RECEIVE",loFormSet.HeaderAlias)) +"' ,"+;
"OPEN      :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_OPEN,loFormSet.GetHeaderText("LANG_MFCSSH_OPEN",loFormSet.HeaderAlias))    +"'"

CASE lcTranType = 'N'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTktText  = LANG_MFCSSH_NPO
lcTktText  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcBusDocu  = 'N'
lcStyType  = 'N'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrowTitl = LANG_MFCSSH_NLORDERS
lcBrowTitl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NLORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_NLORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcBrowFlds = "PO        :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))  +"' ,"+;
"STATUS    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_STATUS,loFormSet.GetHeaderText("LANG_MFCSSH_STATUS",loFormSet.HeaderAlias))  +"' ,"+;
"VENDOR    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_VENDOR,loFormSet.GetHeaderText("LANG_MFCSSH_VENDOR",loFormSet.HeaderAlias))  +"' ,"+;
"COMPLETE  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_COMPDATE,loFormSet.GetHeaderText("LANG_MFCSSH_COMPDATE",loFormSet.HeaderAlias))+"' ,"+;
"NSTYORDER :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_BUDGET,loFormSet.GetHeaderText("LANG_MFCSSH_BUDGET",loFormSet.HeaderAlias))  +"' ,"+;
"POTOTAL   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_TOTCOST,loFormSet.GetHeaderText("LANG_MFCSSH_TOTCOST",loFormSet.HeaderAlias)) +"' ,"+;
"RECEIVE   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RECEIVE,loFormSet.GetHeaderText("LANG_MFCSSH_RECEIVE",loFormSet.HeaderAlias)) +"' ,"+;
"OPEN      :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_OPEN,loFormSet.GetHeaderText("LANG_MFCSSH_OPEN",loFormSet.HeaderAlias))    +"'"

CASE lcTranType = 'T'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTktText  = LANG_MFCSSH_MFGORD
lcTktText  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFGORD,loFormSet.GetHeaderText("LANG_MFCSSH_MFGORD",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcBusDocu  = 'P'
lcStyType  = 'F'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrowTitl = LANG_MFCSSH_MMORDERS
lcBrowTitl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MMORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_MMORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcBrowFlds = "PO        :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))  +"' ,"+;
"STYLE     :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_FABRIC,loFormSet.GetHeaderText("LANG_MFCSSH_FABRIC",loFormSet.HeaderAlias))  +"' ,"+;
"CWARECODE :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_WAREHOUS,loFormSet.GetHeaderText("LANG_MFCSSH_WAREHOUS",loFormSet.HeaderAlias))+"' ,"+;
"STATUS    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_STATUS,loFormSet.GetHeaderText("LANG_MFCSSH_STATUS",loFormSet.HeaderAlias))  +"' ,"+;
"ENTERED   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ENTERED,loFormSet.GetHeaderText("LANG_MFCSSH_ENTERED",loFormSet.HeaderAlias)) +"' ,"+;
"COMPLETE  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_COMPDATE,loFormSet.GetHeaderText("LANG_MFCSSH_COMPDATE",loFormSet.HeaderAlias))+"'"

ENDCASE

lcFileKey  = 'CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE'

laSort[1,1] = 'COPRCODE'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSort[1,2] = LANG_MFCSSH_OPERAT
laSort[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_OPERAT,loFormSet.GetHeaderText("LANG_MFCSSH_OPERAT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laSort[2,1] = 'CCONTCODE'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSort[2,2] = LANG_MFCSSH_CNTDPT
laSort[2,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CNTDPT,loFormSet.GetHeaderText("LANG_MFCSSH_CNTDPT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laSort[3,1] = 'CLOTNO'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSort[3,2] = LANG_MFCSSH_LOT
laSort[3,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_LOT,loFormSet.GetHeaderText("LANG_MFCSSH_LOT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laSort[4,1] = 'ITEM'
laSort[4,2] = lcItmHdr

LOCAL lnI
FOR lnI = 1 TO 4
lcpusort = lcpusort + IIF(EMPTY(lcpusort),'',',') + laSort[lnI,2]
ENDFOR

laSetups[01,1] = 'M_LINK_GL'
laSetups[02,1] = 'M_WAREHOUSE'
laSetups[03,1] = 'M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'SLBL1'
laSetups[04,1] = 'M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'SLBL2'
laSetups[05,1] = 'M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'SLBL3'
laSetups[06,1] = 'M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'SLBL4'
laSetups[07,1] = IIF(lcTranType='T','','M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'SLBL5')
laSetups[08,1] = IIF(lcTranType='T','','M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'SLBL6')
laSetups[09,1] = IIF(lcTranType='T','','M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'SLBL7')
laSetups[10,1] = 'M_DYELOT'
laSetups[11,1] = 'M_MATCSTMTH'
laSetups[12,1] = 'M_COST_METH'
laSetups[13,1] = 'M_TRKROLLS'
laSetups[14,1] = 'M_MATDYE'
laSetups[15,1] = 'M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'TYPE1'
laSetups[16,1] = 'M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'TYPE2'
laSetups[17,1] = 'M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'TYPE3'
laSetups[18,1] = 'M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'TYPE4'
laSetups[19,1] = IIF(lcTranType='T','','M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'TYPE5')
laSetups[20,1] = IIF(lcTranType='T','','M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'TYPE6')
laSetups[21,1] = IIF(lcTranType='T','','M_C'+IIF(lcDyePO="D","M",IIF(lcTranType="N","I",lcTranType))+'TYPE7')
laSetups[22,1] = 'M_GenStOrN'
laSetups[23,1] = 'M_STYCNFG'
=gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)

lcTypes=laSetups[15,2]+laSetups[16,2]+laSetups[17,2]+laSetups[18,2]+laSetups[19,2]+laSetups[20,2]+laSetups[21,2]
llGenOrNum = (UPPER(ALLTRIM(laSetups[22,2])) ='Y')
llStyCost = ('S' $ lcTypes) AND (lcTranType#'N')
llMatCost = ('F' $ lcTypes OR 'T' $ lcTypes) AND (lcTranType#'N')
llUseDyelot = laSetups[10,2]='Y' .OR. laSetups[14,2]='Y'
llStyConfg  = laSetups[23,2]='Y'
lcTmpCutPik = gfTempName()   && Edit order allocated quantity
lcTmpTkt    = gfTempName()   && Ticket lines temporary file
lcDetFile   = gfTempName()   && Ticket sheet details temporary file
lcTktSheet  = gfTempName()   && Ticket sheet header temporary file
lcGlDTemp   = gfTempName()   && General Ledger temporary file
lcOpenLots  = gfTempName()
lcOprHdr    = gfTempName()   && Operation header temporary file
lcOprDet    = gfTempName()   && Operation details temporary file
lcOprDet1   = gfTempName()
lcLotsDet   = gfTempName()
lcRcvFile   = gfTempName()
lcIssLtFile = gfTempName()
lcisslogfile= gfTempName()
lcTmpBom    = gfTempName()   && Style cost sheet temporary file
lcTmpStyle  = gfTempName()   && Style temporary file
lcTmpFbric  = gfTempName()   && Fabric temporary file
lcTmpWip    = gfTempName()

*!*  *C102095,1 AMH Add Flag for closing from select window [Start].
*!*  *C102095,1 AMH This flag will be set to .T. when calling the custom trigger
*!*  *C102095,1 AMH for Cathy Daneil. Otherwise it will be .F. and
*!*  *C102095,1 AMH Add a custom trigger for Cathy Daniels to display a screen
*!*  *C102095,1 AMH to allow the user to select a cuttkt.
*!*  *llCloseCs = .F.
*!*  IF TYPE('lcTranNo')<>'C' .AND. !llOpenCs
*!*    IF ASCAN(laEvntTrig , PADR('SELCSFLD',10)) <> 0
*!*      =gfDoTriger('MFCSSH',PADR('SELCSFLD',10))
*!*      llOpenCs = .T.
*!*    ENDIF
*!*  ENDIF
*!*  *C102095,1 AMH [End]

IF !llCloseCs
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFCSSH.SCX")
=gfCallForm('MFCSSH')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
ENDIF

*!*  *C200477,1 AMH Delete option menu to issue/return rolls [Start]
*!*  IF ASCAN(laEvntTrig,PADR("DLOPTROL",10)) <> 0
*!*    =gfDoTriger("POCSSH",PADR("DLOPTROL",10))
*!*  ENDIF
*!*  *C200477,1 AMH [End]

*!*************************************************************
*! Name      : lfActFolder
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/25/2003
*! Purpose   : Change Folders
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActFolder()
*!*************************************************************
FUNCTION lfActFolder
LPARAMETERS loFormSet

DO CASE
CASE loFormSet.ariaForm1.pgfCstSht.ActivePage = 1
WITH loFormSet.ariaForm1.pgfCstSht.Page1
IF EOF(loFormSet.lcTktSheet) .OR. INLIST(EVALUATE(loFormSet.lcPosHdr+'.Status'),'S','X')
.cmdNew.Enabled = .F.
.cmdRemove.Enabled = .F.
.cmdModify.Enabled = .F.
.cmdVoid.Enabled = .F.
.cmdUnvoid.Enabled = .F.
.cmdAutoIssue.Enabled = .F.
.cmdIssRet.Enabled = .F.
ELSE
IF loFormSet.lcTranType='M'
LOCAL lnOldInd
lnOldInd = ORDER('Style')
SET ORDER TO Cstyle IN Style
=SEEK(EVALUATE(loFormSet.lcPosHdr+'.Style'),'Style')
IF Style.lDetCost
.cmdNew.Enabled = .T.
ELSE
.cmdNew.Enabled = .F.
ENDIF
SET ORDER TO lnOldInd IN Style
ELSE
.cmdNew.Enabled = .T.
ENDIF
.cmdRemove.Enabled = .T.
.cmdModify.Enabled = .T.
IF loFormSet.ActiveMode = "A"
.cmdVoid.Enabled = .F.
.cmdUnvoid.Enabled = .F.
.cmdAutoIssue.Enabled = .F.
.cmdIssRet.Enabled = .F.
ELSE
.cmdVoid.Enabled = .T.
.cmdUnvoid.Enabled = .T.
.cmdAutoIssue.Enabled = .T.
.cmdIssRet.Enabled = .T.
ENDIF
ENDIF
IF loFormSet.ActiveMode = "S"
.Parent.Page2.Enabled = .F.
.Parent.Page3.Enabled = .F.
.Parent.Page4.Enabled = .F.
ELSE
.Parent.Page2.Enabled = .T.
.Parent.Page3.Enabled = .T.
.Parent.Page4.Enabled = .T.
ENDIF
SELECT (loFormSet.lcTktSheet)
ENDWITH

CASE loFormSet.ariaForm1.pgfCstSht.ActivePage = 2
LOCAL lnRecNo
lnRecNo = RECNO(loFormSet.lcOprDet)
WITH loFormSet.ariaForm1.pgfCstSht.Page2
IF !SEEK(loFormSet.lcTranType+PADR(loFormSet.AriaForm1.kbPo.KeyTextBox.Value,6),loFormSet.lcOprDet) .OR.;
INLIST(EVALUATE(loFormSet.lcPosHdr+'.Status'),'S','X')
.cmdDetLev.Enabled = .F.
.cmdModLot.Enabled = .F.
.cmdReceive.Enabled = .F.
.cmdZoom.Enabled = .F.
.cmdAutoIssue.Enabled = .F.
.cmdIssItm.Enabled = .F.
ELSE
IF loFormSet.rbSummary=2
.cmdDetLev.Enabled = .T.
.cmdModLot.Enabled = .F.
.cmdReceive.Enabled = .F.
.cmdZoom.Enabled = .T.
.cmdAutoIssue.Enabled = .F.
.cmdIssItm.Enabled = .F.
ELSE
.cmdDetLev.Enabled = .T.
.cmdModLot.Enabled = .T.
.cmdReceive.Enabled = .T.
.cmdZoom.Enabled = .T.
.cmdAutoIssue.Enabled = .T.
.cmdIssItm.Enabled = .T.
ENDIF
ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT(loFormSet.lcOprDet))
GOTO lnRecNo IN (loFormSet.lcOprDet)
ENDIF
IF EVALUATE(loFormSet.lcPosHdr+'.CMULTILOT') = 'M'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdNewLot.Caption = LANG_MFCSSH_NEWLOT
.cmdNewLot.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NEWLOT,loFormSet.GetHeaderText("LANG_MFCSSH_NEWLOT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdNewLot.Caption = LANG_MFCSSH_ISSUELOT
.cmdNewLot.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUELOT,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUELOT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
IF EOF(loFormSet.lcOprHdr) .OR. INLIST(EVALUATE(loFormSet.lcPosHdr+'.Status'),'S','X')
.cmdNewLot.Enabled = .F.
.cmdModOpr.Enabled = .F.
ELSE
.cmdNewLot.Enabled = .T.
.cmdModOpr.Enabled = .T.
ENDIF
ENDWITH
=lfShOprHd(loFormSet)
ENDCASE

*!*************************************************************
*! Name      : lfShow
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/29/2003
*! Purpose   : Show Main Screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfShow()
*!*************************************************************
FUNCTION lfShow
LPARAMETERS loFormSet,lcModeToChange

oariaapplication.otoolbar.cmdedit.enabled = .F.

DO CASE
CASE lcModeToChange = "S"

STORE '' TO loFormSet.lcStatus,loFormSet.lcFirstOpr
loFormSet.lcIndExp  = 'cIMTyp+cTktNo+cOperSeq+cOprCode+cContCode+cLotNo+cInvType+Item+cDyelot'
loFormSet.rbSummary = 1
STORE .F. TO loFormSet.cbSum1,loFormSet.cbSum2,loFormSet.cbSum3,loFormSet.cbSum4,loFormSet.cbPerSize,loFormSet.llIssFirst,loFormSet.llZoom

loFormSet.lcpusort = ''
LOCAL lnI
FOR lnI = 1 TO 4
loFormSet.lcpusort = loFormSet.lcpusort + IIF(EMPTY(loFormSet.lcpusort),'',',') + loFormSet.laSort[lnI,2]
ENDFOR

IF loFormSet.ariaForm1.pgfCstSht.ActivePage <> 1
loFormSet.ariaForm1.pgfCstSht.ActivePage = 1
ENDIF
=lfGetInfo(loFormSet)
loFormSet.ariaForm1.kbPo.KeyTextBox.SetFocus
*!*      *C102095,1 AMH Add a customized trigger for Cathy Daniels to display [Start]
*!*      *C102095,1 AMH a screen to allow the user to select a cuttkt.
*!*      IF ASCAN(laEvntTrig , PADR('SELCSFLD',10)) <> 0
*!*        =gfDoTriger('MFCSSH',PADR('SELCSFLD',10))
*!*        IF llCloseCs
*!*          =gfCPClose()
*!*          RETURN
*!*        ENDIF
*!*        IF !laScrMode[1]
*!*          laData[1] = lcTranNo
*!*          =lfvTicket()
*!*          DO GPCTRLSHOW
*!*          DO lpShow
*!*        ENDIF
*!*      ENDIF
*!*      *C102095,1 AMH [End]

CASE lcModeToChange = "V"
STORE '/' TO loFormSet.lcPExSign,loFormSet.lcDExSign,loFormSet.lcPUntSin,loFormSet.lcDUntSin
IF loFormSet.lcTranType $ 'IDN'
LOCAL lcPUntSin,lcDUntSin
lcPUntSin = loFormSet.lcPUntSin
lcDUntSin = loFormSet.lcDUntSin
loFormSet.lcPExSign = gfGetExSin(@lcPUntSin,EVALUATE(loFormSet.lcPOSHDR+'.cPriceCur'))
loFormSet.lcDExSign = gfGetExSin(@lcDUntSin,EVALUATE(loFormSet.lcPOSHDR+'.cDutyCur'))
loFormSet.lcPUntSin = lcPUntSin
loFormSet.lcDUntSin = lcDUntSin
ENDIF
loFormSet.lcIndExp = 'cIMTyp+cTktNo+cOperSeq+cOprCode+cContCode+cLotNo+cInvType+Item+cDyelot'

IF EMPTY(EVALUATE(loFormSet.lcPosHdr+'.CITEMWARE'))
REPLACE (loFormSet.lcPosHdr+'.CITEMWARE') WITH EVALUATE(loFormSet.lcPosHdr+'.cWareCode')
ENDIF
IF EMPTY(EVALUATE(loFormSet.lcPosHdr+'.CMATWARE'))
REPLACE (loFormSet.lcPosHdr+'.CMATWARE') WITH EVALUATE(loFormSet.lcPosHdr+'.cWareCode')
ENDIF

=gfOpenFile(oAriaApplication.DataDir+'WAREHOUS',oAriaApplication.DataDir+'WAREHOUS','SH')
SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS WHERE lStyInv INTO ARRAY loFormSet.laStyWare
SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS WHERE lMatInv INTO ARRAY loFormSet.laMatWare

USE IN WAREHOUS
loFormSet.lnTkStWare = CEILING(ASCAN(loFormSet.laStyWare,EVALUATE(loFormSet.lcPosHdr+'.CITEMWARE'))/2)
loFormSet.lnTkMtWare = CEILING(ASCAN(loFormSet.laMatWare,EVALUATE(loFormSet.lcPosHdr+'.CMATWARE'))/2)
loFormSet.ariaForm1.pgfCstSht.Page4.cboStyWare.Requery
loFormSet.ariaForm1.pgfCstSht.Page4.cboMatWare.Requery

=lfRefreshToolBarButtons(loFormSet)
=lfGetInfo(loFormSet)
CASE lcModeToChange = "A"
WITH loFormSet.ariaForm1.pgfCstSht.Page1
.cmdVoid.Enabled = .F.
.cmdUnVoid.Enabled = .F.
.cmdAutoIssue.Enabled = .F.
.cmdIssRet.Enabled = .F.
ENDWITH
WITH loFormSet.ariaForm1.pgfCstSht.Page4
.cboStyWare.Enabled = .F.
.cboMatWare.Enabled = .F.
ENDWITH
*!*      *C102095,1 AMH Check if the trigger for Cathy Daniel exist then disable
*!*      *C102095,1 AMH the user defined fields button.
*!*      IF ASCAN(laEvntTrig , PADR('SELCSFLD',10)) <> 0
*!*        SHOW GET pbUsrFields DISABLE
*!*      ENDIF
*!*     *C102095,1 AMH [End]

ENDCASE
=lfActFolder(loFormSet)

*!*************************************************************
*! Name      : lfvItem
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/31/2003
*! Purpose   : Browse Style/Fabric
*!*************************************************************
*! Calls     : gfStyBrw
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvItem()
*!*************************************************************
FUNCTION lfvItem
LPARAMETERS loFormSet,llBrowse

PRIVATE lcItem
lcItem = loFormSet.AriaForm1.kbItem.keytextbox.Value

LOCAL llFound
IF EMPTY(lcItem)
llFound = .T.
ELSE
IF loFormSet.lcTranType = 'M'
=gfOpenFile(oAriaApplication.DataDir+'STYLE',oAriaApplication.DataDir+'STYLE','SH')
llFound = SEEK(SUBSTR(lcItem,1,LEN(loFormSet.lcMjrMsk)),'STYLE')
ELSE
m.cInvType  = loFormSet.lcInvType
m.cStyMajor = PADR(lcItem,19)
IF !lfOpenSql('ITEM','FABRIC','CSTYLE','CINVTYPE+CSTYMAJOR',loFormSet)
RETURN .F.
ENDIF
SELECT FABRIC
LOCATE
llFound = !EOF()
ENDIF
ENDIF

IF llBrowse .OR. !llFound
IF loFormSet.lcTranType = 'M'
lcItem = gfStyBrw("M" , "" , "" , .F.)
ELSE
LOCAL lcMajor,lcBrowChr
lcMajor   = RTRIM(lcItem)
lcBrowChr = RIGHT(lcMajor,1)
lcMajor   = IIF(lcBrowChr=='?',SUBSTR(lcMajor,1,LEN(lcMajor)-1),lcMajor)
lcItem    = gfItemBrow(loFormSet.lcInvType,PADR(lcMajor,19),"","*","M","",.T.)
lcItem    = PADR(SUBSTR(lcItem,1,LEN(loFormSet.lcMjrMsk)),19)
ENDIF
ENDIF
loFormSet.AriaForm1.kbItem.keytextbox.Value = lcItem

IF !EMPTY(lcItem)
loFormSet.AriaForm1.kbPO.keytextbox.Value = '?'
loFormSet.AriaForm1.kbPO.sharedvalidation("FOR STYLE = lcItem")
IF EMPTY(loFormSet.AriaForm1.kbPO.keytextbox.Value)
loFormSet.AriaForm1.kbItem.keytextbox.Value = ''
ENDIF
ENDIF

*!*************************************************************
*! Name      : lfBrowLots
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/31/2003
*! Purpose   : Browse Cost Sheet Operation Lots
*!*************************************************************
*! Calls     : lfShOprDt
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfBrowLots()
*!*************************************************************
FUNCTION lfBrowLots
LPARAMETERS loFormSet

WITH loFormSet.ariaForm1.pgfCstSht.Page2.grdLots
.RecordSource = ''
IF loFormSet.rbSummary = 1
.RecordSource = loFormSet.lcOprDet
.Column1.ControlSource = "gfCodDes("+loFormSet.lcOprDet+".cOprCode,'MFGCODE')"
.Column2.ControlSource = loFormSet.lcOprDet+".CCONTCODE"
.Column3.ControlSource = loFormSet.lcOprDet+".CLOTNO"
.Column4.ControlSource = loFormSet.lcOprDet+".ITEM"
.Column6.ControlSource = loFormSet.lcOprDet+".cDyelot"
.Column7.ControlSource = loFormSet.lcOprDet+".dTranDate"
.Column8.ControlSource = loFormSet.lcOprDet+".DueDate"
.Column9.ControlSource = "IIF("+loFormSet.lcOprDet+".TranCd='1','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_BUDGET,loFormSet.GetHeaderText("LANG_MFCSSH_BUDGET",loFormSet.HeaderAlias))+"',"+;
"IIF("+loFormSet.lcOprDet+".TranCd='2','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RECEIVE,loFormSet.GetHeaderText("LANG_MFCSSH_RECEIVE",loFormSet.HeaderAlias))+"',"+;
"IIF("+loFormSet.lcOprDet+".TranCd='3','"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DAMAGED,loFormSet.GetHeaderText("LANG_MFCSSH_DAMAGED",loFormSet.HeaderAlias))+"','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CANCELED,loFormSet.GetHeaderText("LANG_MFCSSH_CANCELED",loFormSet.HeaderAlias))+"')))"

.Column10.ControlSource = loFormSet.lcOprDet+".nLotTotQty"
.Column11.ControlSource = loFormSet.lcOprDet+".nLotQty1"
.Column12.ControlSource = loFormSet.lcOprDet+".nLotQty2"
.Column13.ControlSource = loFormSet.lcOprDet+".nLotQty3"
.Column14.ControlSource = loFormSet.lcOprDet+".nLotQty4"
.Column15.ControlSource = loFormSet.lcOprDet+".nLotQty5"
.Column16.ControlSource = loFormSet.lcOprDet+".nLotQty6"
.Column17.ControlSource = loFormSet.lcOprDet+".nLotQty7"
.Column18.ControlSource = loFormSet.lcOprDet+".nLotQty8"
.Column7.Visible = .T.
.Column8.Visible = .T.
.Column9.Visible = .T.
.Column10.Visible = .T.
IF loFormSet.cbPerSize
.Column11.Visible = .T.
.Column12.Visible = .T.
.Column13.Visible = .T.
.Column14.Visible = .T.
.Column15.Visible = .T.
.Column16.Visible = .T.
.Column17.Visible = .T.
.Column18.Visible = .T.
ELSE
.Column11.Visible = .F.
.Column12.Visible = .F.
.Column13.Visible = .F.
.Column14.Visible = .F.
.Column15.Visible = .F.
.Column16.Visible = .F.
.Column17.Visible = .F.
.Column18.Visible = .F.
ENDIF
.Column19.Visible = .F.
.Column20.Visible = .F.
.Column21.Visible = .F.
.Column22.Visible = .F.
.Column23.Visible = .F.
.Column24.Visible = .F.
.Column25.Visible = .F.
.Column26.Visible = .F.
.Column27.Visible = .F.
.Column28.Visible = .F.
.Column29.Visible = .F.
.Column30.Visible = .F.
.Column31.Visible = .F.
.Column32.Visible = .F.
.Column33.Visible = .F.
.Column34.Visible = .F.
.Column35.Visible = .F.
.Column36.Visible = .F.
.Column37.Visible = .F.
.Column38.Visible = .F.
.Column39.Visible = .F.
.Column40.Visible = .F.
.Column41.Visible = .F.
.Column42.Visible = .F.
.Column43.Visible = .F.
.Column44.Visible = .F.
.Column45.Visible = .F.
.Column46.Visible = .F.
.Column47.Visible = .F.
.Column48.Visible = .F.
.Column49.Visible = .F.
.Column50.Visible = .F.
.Column51.Visible = .F.
.Column52.Visible = .F.
.Column53.Visible = .F.
.Column54.Visible = .F.
.Column55.Visible = .F.
.Column56.Visible = .F.
.Column57.Visible = .F.
.Column58.Visible = .F.
.Column59.Visible = .F.
.Column60.Visible = .F.
.Column61.Visible = .F.
.Column62.Visible = .F.
.Column63.Visible = .F.
ELSE
.RecordSource = loFormSet.lcOprDet1
.Column1.ControlSource = "gfCodDes("+loFormSet.lcOprDet1+".cOprCode,'MFGCODE')"
.Column2.ControlSource = loFormSet.lcOprDet1+".CCONTCODE"
.Column3.ControlSource = loFormSet.lcOprDet1+".CLOTNO"
.Column4.ControlSource = loFormSet.lcOprDet1+".ITEM"
.Column6.ControlSource = loFormSet.lcOprDet1+".cDyelot"
.Column19.ControlSource = loFormSet.lcOprDet1+".Iss"
.Column20.ControlSource = loFormSet.lcOprDet1+".Rcv"
.Column21.ControlSource = loFormSet.lcOprDet1+".Can"
.Column22.ControlSource = loFormSet.lcOprDet1+".Dmg"
.Column23.ControlSource = "MAX("+loFormSet.lcOprDet1+".Iss-"+loFormSet.lcOprDet1+".Rcv-"+loFormSet.lcOprDet1+".Can-"+loFormSet.lcOprDet1+".Dmg,0)"
.Column24.ControlSource = loFormSet.lcOprDet1+".Iss1"
.Column25.ControlSource = loFormSet.lcOprDet1+".Iss2"
.Column26.ControlSource = loFormSet.lcOprDet1+".Iss3"
.Column27.ControlSource = loFormSet.lcOprDet1+".Iss4"
.Column28.ControlSource = loFormSet.lcOprDet1+".Iss5"
.Column29.ControlSource = loFormSet.lcOprDet1+".Iss6"
.Column30.ControlSource = loFormSet.lcOprDet1+".Iss7"
.Column31.ControlSource = loFormSet.lcOprDet1+".Iss8"
.Column32.ControlSource = loFormSet.lcOprDet1+".Rcv1"
.Column33.ControlSource = loFormSet.lcOprDet1+".Rcv2"
.Column34.ControlSource = loFormSet.lcOprDet1+".Rcv3"
.Column35.ControlSource = loFormSet.lcOprDet1+".Rcv4"
.Column36.ControlSource = loFormSet.lcOprDet1+".Rcv5"
.Column37.ControlSource = loFormSet.lcOprDet1+".Rcv6"
.Column38.ControlSource = loFormSet.lcOprDet1+".Rcv7"
.Column39.ControlSource = loFormSet.lcOprDet1+".Rcv8"
.Column40.ControlSource = loFormSet.lcOprDet1+".Can1"
.Column41.ControlSource = loFormSet.lcOprDet1+".Can2"
.Column42.ControlSource = loFormSet.lcOprDet1+".Can3"
.Column43.ControlSource = loFormSet.lcOprDet1+".Can4"
.Column44.ControlSource = loFormSet.lcOprDet1+".Can5"
.Column45.ControlSource = loFormSet.lcOprDet1+".Can6"
.Column46.ControlSource = loFormSet.lcOprDet1+".Can7"
.Column47.ControlSource = loFormSet.lcOprDet1+".Can8"
.Column48.ControlSource = loFormSet.lcOprDet1+".Dmg1"
.Column49.ControlSource = loFormSet.lcOprDet1+".Dmg2"
.Column50.ControlSource = loFormSet.lcOprDet1+".Dmg3"
.Column51.ControlSource = loFormSet.lcOprDet1+".Dmg4"
.Column52.ControlSource = loFormSet.lcOprDet1+".Dmg5"
.Column53.ControlSource = loFormSet.lcOprDet1+".Dmg6"
.Column54.ControlSource = loFormSet.lcOprDet1+".Dmg7"
.Column55.ControlSource = loFormSet.lcOprDet1+".Dmg8"
.Column56.ControlSource = "MAX("+loFormSet.lcOprDet1+".Iss1-"+loFormSet.lcOprDet1+".Rcv1-"+loFormSet.lcOprDet1+".Can1-"+loFormSet.lcOprDet1+".Dmg1,0)"
.Column57.ControlSource = "MAX("+loFormSet.lcOprDet1+".Iss2-"+loFormSet.lcOprDet1+".Rcv2-"+loFormSet.lcOprDet1+".Can2-"+loFormSet.lcOprDet1+".Dmg2,0)"
.Column58.ControlSource = "MAX("+loFormSet.lcOprDet1+".Iss3-"+loFormSet.lcOprDet1+".Rcv3-"+loFormSet.lcOprDet1+".Can3-"+loFormSet.lcOprDet1+".Dmg3,0)"
.Column59.ControlSource = "MAX("+loFormSet.lcOprDet1+".Iss4-"+loFormSet.lcOprDet1+".Rcv4-"+loFormSet.lcOprDet1+".Can4-"+loFormSet.lcOprDet1+".Dmg4,0)"
.Column60.ControlSource = "MAX("+loFormSet.lcOprDet1+".Iss5-"+loFormSet.lcOprDet1+".Rcv5-"+loFormSet.lcOprDet1+".Can5-"+loFormSet.lcOprDet1+".Dmg5,0)"
.Column61.ControlSource = "MAX("+loFormSet.lcOprDet1+".Iss6-"+loFormSet.lcOprDet1+".Rcv6-"+loFormSet.lcOprDet1+".Can6-"+loFormSet.lcOprDet1+".Dmg6,0)"
.Column62.ControlSource = "MAX("+loFormSet.lcOprDet1+".Iss7-"+loFormSet.lcOprDet1+".Rcv7-"+loFormSet.lcOprDet1+".Can7-"+loFormSet.lcOprDet1+".Dmg7,0)"
.Column63.ControlSource = "MAX("+loFormSet.lcOprDet1+".Iss8-"+loFormSet.lcOprDet1+".Rcv8-"+loFormSet.lcOprDet1+".Can8-"+loFormSet.lcOprDet1+".Dmg8,0)"
.Column7.Visible = .F.
.Column8.Visible = .F.
.Column9.Visible = .F.
.Column10.Visible = .F.
.Column11.Visible = .F.
.Column12.Visible = .F.
.Column13.Visible = .F.
.Column14.Visible = .F.
.Column15.Visible = .F.
.Column16.Visible = .F.
.Column17.Visible = .F.
.Column18.Visible = .F.
.Column19.Visible = .T.
.Column20.Visible = .T.
.Column21.Visible = .T.
.Column22.Visible = .T.
.Column23.Visible = .T.
IF loFormSet.cbPerSize
.Column24.Visible = .T.
.Column25.Visible = .T.
.Column26.Visible = .T.
.Column27.Visible = .T.
.Column28.Visible = .T.
.Column29.Visible = .T.
.Column30.Visible = .T.
.Column31.Visible = .T.
.Column32.Visible = .T.
.Column33.Visible = .T.
.Column34.Visible = .T.
.Column35.Visible = .T.
.Column36.Visible = .T.
.Column37.Visible = .T.
.Column38.Visible = .T.
.Column39.Visible = .T.
.Column40.Visible = .T.
.Column41.Visible = .T.
.Column42.Visible = .T.
.Column43.Visible = .T.
.Column44.Visible = .T.
.Column45.Visible = .T.
.Column46.Visible = .T.
.Column47.Visible = .T.
.Column48.Visible = .T.
.Column49.Visible = .T.
.Column50.Visible = .T.
.Column51.Visible = .T.
.Column52.Visible = .T.
.Column53.Visible = .T.
.Column54.Visible = .T.
.Column55.Visible = .T.
.Column56.Visible = .T.
.Column57.Visible = .T.
.Column58.Visible = .T.
.Column59.Visible = .T.
.Column60.Visible = .T.
.Column61.Visible = .T.
.Column62.Visible = .T.
.Column63.Visible = .T.
ELSE
.Column24.Visible = .F.
.Column25.Visible = .F.
.Column26.Visible = .F.
.Column27.Visible = .F.
.Column28.Visible = .F.
.Column29.Visible = .F.
.Column30.Visible = .F.
.Column31.Visible = .F.
.Column32.Visible = .F.
.Column33.Visible = .F.
.Column34.Visible = .F.
.Column35.Visible = .F.
.Column36.Visible = .F.
.Column37.Visible = .F.
.Column38.Visible = .F.
.Column39.Visible = .F.
.Column40.Visible = .F.
.Column41.Visible = .F.
.Column42.Visible = .F.
.Column43.Visible = .F.
.Column44.Visible = .F.
.Column45.Visible = .F.
.Column46.Visible = .F.
.Column47.Visible = .F.
.Column48.Visible = .F.
.Column49.Visible = .F.
.Column50.Visible = .F.
.Column51.Visible = .F.
.Column52.Visible = .F.
.Column53.Visible = .F.
.Column54.Visible = .F.
.Column55.Visible = .F.
.Column56.Visible = .F.
.Column57.Visible = .F.
.Column58.Visible = .F.
.Column59.Visible = .F.
.Column60.Visible = .F.
.Column61.Visible = .F.
.Column62.Visible = .F.
.Column63.Visible = .F.
ENDIF
ENDIF

LOCAL lnI
FOR lnI = 1 TO .ColumnCount
IF .Columns(lnI).Width > 0
.Columns(lnI).Tag = STR(.Columns(lnI).Width)
ENDIF

IF .Columns(lnI).Visible
.Columns(lnI).Width = VAL(.Columns(lnI).Tag)
ELSE
.Columns(lnI).Width = 0
ENDIF
ENDFOR
ENDWITH

*!*************************************************************
*! Name      : lfShOprHd
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/30/2003
*! Purpose   : Refresh Browse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShOprHd()
*!*************************************************************
FUNCTION lfShOprHd
LPARAMETERS loFormSet

=lfRefTotal(loFormSet)
WITH loFormSet.ariaForm1.pgfCstSht.Page2
IF !(loFormSet.ActiveMode="S") .AND. (EVALUATE(loFormSet.lcPosHdr+'.Status')='O') .AND.;
!EMPTY(EVALUATE(loFormSet.lcOprHdr+'.cOprCode')) .AND. (loFormSet.ariaForm1.pgfCstSht.ActivePage=2) .AND.;
IIF(EVALUATE(loFormSet.lcPosHdr+'.CMULTILOT')='S',EVALUATE(loFormSet.lcOprHdr+'.cOprCode')=loFormSet.lcFirstOpr .AND.;
loFormSet.lnOprBud=0,.T.)
.cmdNewLot.Enabled = .T.
ELSE
.cmdNewLot.Enabled = .F.
ENDIF

IF loFormSet.llPWInst .AND. loFormSet.lcTranType = "M"
IF !USED('pwoperat')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*=gfOpenFile(oAriaApplication.DataDir+"pwoperat" , oAriaApplication.DataDir+"pwoperat" , "SH")
=gfOpenTable("PWOPERAT" , "PWOPERAT" , "SH")
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
ENDIF
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*IF SEEK(EVALUATE(loFormSet.lcOprHdr+'.cOprCode'),'pwoperat')
IF gfSEEK(EVALUATE(loFormSet.lcOprHdr+'.cOprCode'),'PWOPERAT')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
.cmdReceive.Enabled = .F.
ELSE
.cmdReceive.Enabled = .T.
ENDIF
ENDIF
ENDWITH

*!*************************************************************
*! Name      : lfRefTotal
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/30/2003
*! Purpose   : Show Operation's total Budget, Received, Damaged and canceled quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfRefTotal()
*!*************************************************************
FUNCTION lfRefTotal
LPARAMETERS loFormSet

LOCAL lnAlias,lcCurrTag,lnRecNo

*--- Doc. [Start]
*--- TranCd = '1' ==> Budget
*--- TranCd = '2' ==> Receive
*--- TranCd = '3' ==> Dameged
*--- TranCd = '4' ==> Cancel
*--- Doc. [End]

lnAlias = SELECT(0)
SELECT (loFormSet.lcOprDet)
lnRecNo = RECNO()
lcCurrTag = TAG()
SET ORDER TO TAG 'ITEM'

STORE 0 TO loFormSet.lnOprBud,loFormSet.lnOprRec,loFormSet.lnOprCan,loFormSet.lnOprDam,loFormSet.lnOprOpn

LOCAL ARRAY laOprOpn[8]
STORE 0 TO laOprOpn

=SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+EVALUATE(loFormSet.lcOprHdr+'.cOprCode'))
SCAN REST WHILE cIMTYp+cTktNo+cOprCode = loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+EVALUATE(loFormSet.lcOprHdr+'.cOprCode')
loFormSet.lnOprBud = loFormSet.lnOprBud + IIF(TranCd = '1',nLotTotQty,0)
loFormSet.lnOprRec = loFormSet.lnOprRec + IIF(TranCd = '2',nLotTotQty,0)
loFormSet.lnOprDam = loFormSet.lnOprDam + IIF(TranCd = '3',nLotTotQty,0)
loFormSet.lnOprCan = loFormSet.lnOprCan + IIF(TranCd = '4',nLotTotQty,0)
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
laOprOpn[lnI] = MAX(laOprOpn[lnI] + (EVALUATE('nLotQty'+lcI)*IIF(TranCd='1',1,-1)),0)
ENDFOR
ENDSCAN

FOR lnI = 1 TO 8
loFormSet.lnOprOpn = loFormSet.lnOprOpn + laOprOpn[lnI]
ENDFOR

WITH loFormSet.ariaForm1.pgfCstSht.Page2
.txtBudget.Refresh
.txtReceived.Refresh
.txtCanceled.Refresh
.txtDamaged.Refresh
.txtOpen.Refresh
ENDWITH

IF BETWEEN(lnRecNo,1,RECCOUNT())
GO lnRecNO
ENDIF
SET ORDER TO TAG lcCurrTag
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvNew
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/31/2003
*! Purpose   : Add new Cost Item
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvNew()
*!*************************************************************
FUNCTION lfvNew
LPARAMETERS loFormSet

LOCAL lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnEst6,lnEst7,lcStyItem,lnLen,lcLastOpr,llHideTit
*--- DOC.
*--- This functino call style cost sheet program to add new cost
*--- item to the CutTkt Cost sheet.
*--- The new cost item will affect only the cuttkt cost sheet
*--- not the style cost sheet.
*--- DOC.
LOCAL ARRAY laQty[9]
*-- Call the program that add a new cost item.
*--- DOC.
*--- This select SQL select all the style cost element from the style cost sheet.
*--- DOC.
m.cInvType   = SPACE(4)
m.cItmMajor  = SPACE(19)
m.cCstShtTyp = SPACE(1)
m.cCstSht_ID = SPACE(6)

IF !lfOpenSql('BOM',loFormSet.lcBom,'MULTIBOM','CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID',loFormSet)
RETURN .F.
ENDIF

SELECT *,RECNO() AS nRecNo , "S" AS cStatus , SPACE(30) as cTypelabel;
FROM (loFormSet.lcBom);
INTO DBF (oAriaApplication.WorkDir+loFormSet.lcTmpBom)
INDEX ON cInvType+citmmajor+typ+IIF(.NOT.(ccatgtyp$"PMD"),citmmask,mfgcode)+cInvTypC+item TAG (loFormSet.lcTmpBom)
IF !USED(loFormSet.lcTmpStyle)
SELECT STYLE
=AFIELDS(laFileStru)
=gfCrtTmp(loFormSet.lcTmpStyle,@laFileStru,"Style",loFormSet.lcTmpStyle)
ENDIF
IF !USED(loFormSet.lcTmpFbric)
m.cInvType  = loFormSet.lcInvType
m.cStyMajor = SPACE(19)
IF lfOpenSql('ITEM','FABRIC','CSTYLE','CINVTYPE+CSTYMAJOR',loFormSet)
SELECT FABRIC
=AFIELDS(laFileStru)
=gfCrtTmp(loFormSet.lcTmpFbric,@laFileStru,"cInvType+Style",loFormSet.lcTmpFbric)
ENDIF
ENDIF
lcStyItem = EVALUATE(loFormSet.lcPosHdr+'.Style')
lnLen = IIF(loFormSet.lcTranType$'MT',LEN(loFormSet.lcMjrMsk),19)

IF loFormSet.lcTranType $ 'IDN'
SELECT LEFT(ITEM,LEN(loFormSet.lcMjrMsk)) AS cMajor,SUM(nQty1) AS Qty1,;
SUM(nQty2) AS Qty2,SUM(nQty3) AS Qty3,SUM(nQty4) AS Qty4,;
SUM(nQty5) AS Qty5,SUM(nQty6) AS Qty6,SUM(nQty7) AS Qty7,;
SUM(nQty8) AS Qty8,SUM(nTotQty ) AS TotQty FROM (loFormSet.lcTmpTkt) ;
GROUP BY cMajor INTO CURSOR (loFormSet.lcisslogfile)

SELECT (loFormSet.lcisslogfile)
DIMENSION laTempData[1]
STORE '' TO laTempData
DO CASE
CASE loFormSet.lcTranType = 'I'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl = LANG_MFCSSH_POSTY
lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_POSTY,loFormSet.GetHeaderText("LANG_MFCSSH_POSTY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE loFormSet.lcTranType = 'D'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl = LANG_MFCSSH_DYESTY
lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYESTY,loFormSet.GetHeaderText("LANG_MFCSSH_DYESTY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE loFormSet.lcTranType = 'N'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl = LANG_MFCSSH_NPOSTY
lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NPOSTY,loFormSet.GetHeaderText("LANG_MFCSSH_NPOSTY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDCASE
lcFile_Ttl = lcFile_Ttl + ALLTRIM(loFormSet.lcMjrHdr)
lcBrFields = "cMajor :H='"+ALLTRIM(loFormSet.lcMjrHdr)+"' ,"+;
"Qty1 :7 ,"+;
"Qty2 :7 ,"+;
"Qty3 :7 ,"+;
"Qty4 :7 ,"+;
"Qty5 :7 ,"+;
"Qty6 :7 ,"+;
"Qty7 :7 ,"+;
"Qty8 :7 ,"+;
"TotQty :8"

lcStyItem = IIF(_TALLY=1 .OR. gfBrows('','CMAJOR','laTempData',lcFile_Ttl,.F.),CMAJOR,'')
USE IN (loFormSet.lcisslogfile)
SELECT (loFormSet.lcTktSheet)
IF EMPTY(lcStyItem)
RETURN
ENDIF
ENDIF

IF loFormSet.lcTranType = "T"
m.cInvType  = loFormSet.lcInvType
m.cStyMajor = PADR(loFormSet.AriaForm1.kbItem.KeyTextBox.Value,19)
IF lfOpenSql('ITEM',loFormSet.lcItemFile,'CSTYLE','CINVTYPE+CSTYMAJOR',loFormSet)
SELECT (loFormSet.lcItemFile)
=lfSetIndex(loFormSet.lcItemFile,'CSTYLE','CSTYMAJOR')
ENDIF
ENDIF

*--- Doc. [Start]
*--- Call Style cost sheet add/new item screen
*--- The lase parameter '.T.' for generate cost sheet by size.
*--- Doc. [End]
*--- llFromCtk : Variable For Routing Screen.
*--- Since we are running this screen from either Style Cost Sheet Screen
*--- or C/T Cost sheet screen and there is Two  object we need to them to be
*--- displayed If called from C/T other wise they will be hidden.
IF loFormSet.llPWInst .AND. loFormSet.lcTranType = "M"
loFormSet.llFromCtk = .T.
loFormSet.lcPWBom   = ''
ENDIF

*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
IF TYPE('lcPWCostSheetID') <> 'C'
lnOldSelect = SELECT()
SELECT(loFormSet.lcPosln)
lnOldPOSLNRec = RECNO()
LOCATE
lcPWCostSheetID = EVALUATE(loFormSet.lcPosln+'.cCstSht_ID')
IF BETWEEN(lnOldPOSLNRec ,1,RECCOUNT())
GO RECORD lnOldPOSLNRec
ENDIF
SELECT(lnOldSelect)
ENDIF
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]


*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFCSTSC.scx') WITH loFormSet.lcStyleTyp,loFormSet.lcInvType,lcStyItem,;
loFormSet.lcTmpBom,loFormSet.lcTmpFbric,loFormSet.lcTmpStyle,"Y"
loCallingForm = loFormSet
lcStyleItem = lcStyItem
=gfCallForm('MFCSTSC',.F.,'loCallingForm.lcStyleTyp,loCallingForm.lcInvType,lcStyleItem ,;
loCallingForm.lcTmpBom,loCallingForm.lcTmpFbric,loCallingForm.lcTmpStyle,"Y"')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]

=lfAddNewItm(loFormSet)
*N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[T20110914.0019][Start]
IF loFormSet.lcTranType='T'
m.cInvType  = loFormSet.lcInvType
m.cStyMajor = EVALUATE(loFormSet.lcPosHdr+'.Style')
IF lfOpenSql('ITEM','FABRIC','CSTYLE','CINVTYPE+CSTYMAJOR',loFormSet)
=lfSetIndex('FABRIC','STYLE','STYLE')
ENDIF
ENDIF
*N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[T20110914.0019][END]

SELECT (loFormSet.lcTmpBom)
LOCATE

IF !EOF()
LOCAL lcTktSheet,lcDetFile,lcOprHdr
IF EVALUATE(loFormSet.lcPosHdr+'.STATUS')='H'
lcTktSheet = loFormSet.lcTktSheet
lcDetFile  = loFormSet.lcDetFile
lcOprHdr   = loFormSet.lcOprHdr
ELSE
lcTktSheet = loFormSet.lcCTKTBOM
lcDetFile  = loFormSet.lcBOMLINE
lcOprHdr   = loFormSet.lcMFGOPRHD
=lfSetIndex(lcTktSheet,'CTKTBOM','CIMTYP+CUTTKT+TYP+CINVTYPE+ITEM+MFGCODE+DYELOT')
=lfSetIndex(lcDetFile,'BOMLINE','CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE')
=lfSetIndex(lcOprHdr,'MFGOPRHD','CIMTYP+CTKTNO+COPRCODE')
*N000587,1 WAM 12/01/2007 Set index to MFG operation header
=lfSetIndex(lcOprHdr,lcOprHdr,'cOperSeq+cOprCode')
*N000587,1 WAM 12/01/2007 (End)

ENDIF

SELECT (loFormSet.lcTranFile)
LOCATE FOR Style = SUBSTR(lcStyItem,1,lnLen)
IF FOUND()
SELECT (loFormSet.lcTranFile)
SCAN FOR Style = SUBSTR(lcStyItem,1,lnLen) AND TranCd = '1'
STORE 0 TO laQty,lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnEst6,lnEst7
laQty[1] = Qty1
laQty[2] = Qty2
laQty[3] = Qty3
laQty[4] = Qty4
laQty[5] = Qty5
laQty[6] = Qty6
laQty[7] = Qty7
laQty[8] = Qty8
laQty[9]  = TotQty
lcLastOpr = EVALUATE(loFormSet.lcPosHdr+'.CLASTOPR')
*--- Doc. [Start]
*--- gfSheetItem == Generate, Modify or Delete cost sheet items for style
*--- Doc. [End]

IF gfSheetItem(loFormSet.lcTranType,EVALUATE(loFormSet.lcPosHdr+'.PO'),EVALUATE(loFormSet.lcPosHdr+'.LINK_CODE'),;
Style,loFormSet.lcInvType,EVALUATE(loFormSet.lcTranFile+'.LineNo'),Dyelot,;
EVALUATE(loFormSet.lcPosHdr+'.CITEMWARE'),EVALUATE(loFormSet.lcPosHdr+'.CMATWARE'),@laQty,;
loFormSet.lcTmpBom,lcTktSheet,lcDetFile,lcOprHdr,@lcLastOpr,;
IIF(loFormSet.lcStyleTyp='I',EVALUATE(loFormSet.lcTranFile+'.nFCost1'),0),;
@lnEst1,@lnEst2,@lnEst3,@lnEst4,@lnEst5,@lnEst6,@lnEst7,;
loFormSet.lcPosHdr,loFormSet.lcPosLn,loFormSet.lcDetFile)

IF EVALUATE(loFormSet.lcPosHdr+'.STATUS')='H'
SELECT (loFormSet.lcPosHdr)
LOCAL lnI,lcI
FOR lnI = 1 TO 7
lcI = STR(lnI,1)
REPLACE ('nICost'+lcI) WITH EVALUATE('nICost'+lcI+' + lnEst'+lcI)
ENDFOR
REPLACE CLASTOPR WITH lcLastOpr
ENDIF
ENDIF
SELECT (loFormSet.lcTranFile)
ENDSCAN
ENDIF

*Call PW Function to generate cost sheet for detail operation.
*Only if PW module installed and Call this program to Generate
*C/T Cost Sheet.
LOCAL llDummy
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*llDummy = loFormSet.llPWInst .AND. (loFormSet.lcTranType = "M") .AND. lfPwShtItm('A',.T.,loFormSet)
llDummy = loFormSet.llPWInst .AND. !gfGetMemVar('LUSEBUNDLE',oAriaApplication.ActiveCompanyID) AND (loFormSet.lcTranType = "M") .AND. lfPwShtItm('A',.T.,loFormSet)
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
*--- DOC.
*--- Function to update estimated cost.
*--- DOC.
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*=lfUpdEstCst(loFormSet)
DECLARE laFrgnCost[7], laEquCost[7]
STORE 0 TO laFrgnCost, laEquCost
=lfUpdEstCst(loFormSet,@laFrgnCost, @laEquCost)
*N000587,1 WAM 12/01/2007 (End)

IF EVALUATE(loFormSet.lcPosHdr+'.STATUS')<>'H'
DECLARE laTableUpdate[5,2]
laTableUpdate[1,1] = loFormSet.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'

laTableUpdate[2,1] = loFormSet.lcBomLine
laTableUpdate[2,2] = 'BOMLINE'

laTableUpdate[3,1] = loFormSet.lcMfgOprHd
laTableUpdate[3,2] = 'MFGOPRHD'

laTableUpdate[4,1] = loFormSet.lcPosHdr
laTableUpdate[4,2] = 'POSHDR'

laTableUpdate[5,1] = loFormSet.lcPosLn
laTableUpdate[5,2] = 'POSLN'

=lfTableUpdate(loFormSet)

SELECT (lcTktSheet)
=TABLEUPDATE(.T.,.T.)

*! B609263,1 HES 05/23/2010 PO - Error adding new lines to PO Cost Sheet [Start]
*!*	    DELETE TAG CTKTBOM OF (lcTktSheet)
DELETE TAG CTKTBOM
*! B609263,1 HES 05/23/2010 PO - Error adding new lines to PO Cost Sheet [End  ]

SELECT (lcDetFile)
=TABLEUPDATE(.T.,.T.)

*! B609263,1 HES 05/23/2010 PO - Error adding new lines to PO Cost Sheet [Start]
*!*	    DELETE TAG BOMLINE OF (lcDetFile)
DELETE TAG BOMLINE
*! B609263,1 HES 05/23/2010 PO - Error adding new lines to PO Cost Sheet [End  ]

SELECT (lcOprHdr)
=TABLEUPDATE(.T.,.T.)

*! B609263,1 HES 05/23/2010 PO - Error adding new lines to PO Cost Sheet [Start]
*!*	    DELETE TAG MFGOPRHD OF (lcOprHdr)
DELETE TAG MFGOPRHD
*! B609263,1 HES 05/23/2010 PO - Error adding new lines to PO Cost Sheet [End  ]

=lfGetInfo(loFormSet)
IF loFormSet.lcTranType = 'I'
=lfRecCost(loFormSet.lcTranType,EVALUATE(loFormSet.lcPosHdr+'.PO'),loFormSet.lcBOMLINE,loFormSet.lcCTKTBOM,loFormSet)
ENDIF
ELSE
SELECT (loFormSet.lcTktSheet)
SET ORDER TO TAG (loFormSet.lcTktSheet)
*--- DOC.
*--- Start update the temp file that used in the folder cost sheet.
*--- to brows the cost item from , this temp file is the header cost
*--- sheet file.
*--- DOC.
FOR lnType = 1 TO 7
DO WHILE SEEK(STR(lnType,1)+SPACE(1))
REPLACE cShowType WITH '1'
IF !SEEK(STR(lnType,1)+'0')
INSERT INTO (loFormSet.lcTktSheet) (TYP,cShowType,Item,Dyelot) VALUES ;
(STR(lnType,1),'0',loFormSet.laSetups[2+lnType,2],lfGetDye(loFormSet,lnType))
ENDIF
ENDDO
ENDFOR
GO TOP
SELECT *,RECNO() AS nRecNo FROM (loFormSet.lcDetFile) WHERE EMPTY(cShowType) INTO CURSOR tmpBomLine
SET ORDER TO TAG (loFormSet.lcDetFile) IN (loFormSet.lcDetFile)
*--- DOC.
*--- Start update file tmpBomLine, This file is copy from CTKTBOM file
*--- DOC.
SELECT tmpBomLine
SCAN
IF !EMPTY(MfgCode) AND SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+MfgCode,loFormSet.lcOprHdr) ;
.AND. SEEK(cInvType+Style+STR(LineNo,6),loFormSet.lcTmpTkt)
SELECT (loFormSet.lcTmpTkt)
SELECT MIN(cOperSeq) FROM (loFormSet.lcOprHdr) INTO ARRAY laTmp1
IF _TALLY > 0
SELECT coprCode FROM (loFormSet.lcOprHdr) WHERE cOperSeq = laTmp1[1] INTO ARRAY laTmp2
REPLACE cFrstOpr   WITH laTmp2[1],;
nFrstOprSq WITH VAL(laTmp1[1])
ENDIF
ENDIF
SELECT tmpBomLine
IF !SEEK(cInvType+Style+STR(LineNo,6)+cBomTyp+'0',loFormSet.lcDetFile)
SELECT (loFormSet.lcDetFile)
=SEEK(tmpBomLine.cInvType+tmpBomLine.Style+STR(tmpBomLine.LineNo,6))
LOCATE REST WHILE cInvType+Style+STR(LineNo,6) =;
tmpBomLine.cInvType+tmpBomLine.Style+STR(tmpBomLine.LineNo,6) FOR !EMPTY(cShowType)
llHideTit = FOUND()
INSERT INTO (loFormSet.lcDetFile) ;
(cInvType,Style,LineNo,cBomTyp,cShowType,Item,lHideTit,Dyelot) VALUES ;
(tmpBomLine.cInvType,tmpBomLine.Style,tmpBomLine.LineNo,tmpBomLine.cBomTyp,'0',;
loFormSet.laSetups[2+VAL(tmpBomLine.cBomTyp),2],llHideTit,;
lfGetDye(loFormSet,VAL(tmpBomLine.cBomTyp)))
ENDIF
SELECT (loFormSet.lcDetFile)
GOTO tmpBomLine.nRecNo
REPLACE cShowType WITH '1'

*N000587,1 WAM 12/01/2007 Get equivalent cost from PO cost sheet lines
STORE '/' TO lcExSign,lcUntSin
lcPosHdr = loFormSet.lcPosHdr
IF ISNULL(cCurrCode) OR EMPTY(cCurrCode)
DO CASE
CASE cCatgTyp = 'P'
lcCurrCode = IIF(EMPTY(&lcPosHdr..cPriceCur),oAriaApplication.BaseCurrency,&lcPosHdr..cPriceCur)
lnExRate   = IIF(&lcPosHdr..nPriceRat=0,1, &lcPosHdr..nPriceRat)
lnCurrUnit = IIF(&lcPosHdr..nCurrUnit=0,1, &lcPosHdr..nCurrUnit)
CASE INLIST(cCatgTyp,'D','M')
lcCurrCode = IIF(EMPTY(&lcPosHdr..cDutyCur),oAriaApplication.BaseCurrency,&lcPosHdr..cDutyCur)
lnExRate   = IIF(&lcPosHdr..nDutyRat=0,1, &lcPosHdr..nDutyRat)
lnCurrUnit = IIF(&lcPosHdr..nDCurUnit=0,1, &lcPosHdr..nDCurUnit)
OTHERWISE
lcCurrCode = oAriaApplication.BaseCurrency
lnExRate   = 1
lnCurrUnit = 1
ENDCASE
ELSE
lcCurrCode = cCurrCode
lnExRate   = IIF(ISNULL(nExRate) OR nExRate=0,1,nExRate)
lnCurrUnit = IIF(ISNULL(nCurrUnit) OR nCurrUnit=0,1,nCurrUnit)
ENDIF
lcExSign   = gfGetExSin(@lcUntSin, lcCurrCode)
lnEquAmount = ItemAmt &lcExSign lnExRate &lcUntSin lnCurrUnit
REPLACE NEQU_AMT WITH lnEquAmount
*N000587,1 WAM 12/01/2007 (End)

IF !SEEK(cInvType+Style+STR(LineNo,6)+cBomTyp+'2',loFormSet.lcDetFile)
INSERT INTO (loFormSet.lcDetFile) ;
(cInvType,Style,LineNo,cBomTyp,cShowType,Item) VALUES ;
(tmpBomLine.cInvType,tmpBomLine.Style,tmpBomLine.LineNo,tmpBomLine.cBomTyp,'2','***** '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_TOTAL,loFormSet.GetHeaderText("LANG_MFCSSH_TOTAL",loFormSet.HeaderAlias))+' :')


ENDIF
SELECT (loFormSet.lcDetFile)
*N000587,1 WAM 12/01/2007 update equivalent cost in PO cost sheet lines
*REPLACE ItemQty  WITH ItemQty + tmpBomLine.ItemQty,;
ItemAmt  WITH ItemAmt + tmpBomLine.ItemAmt
REPLACE ItemQty  WITH ItemQty + tmpBomLine.ItemQty,;
ItemAmt  WITH ItemAmt + tmpBomLine.ItemAmt ,;
NEQU_AMT WITH NEQU_AMT + lnEquAmount
*N000587,1 WAM 12/01/2007 (End)
ENDSCAN
USE IN tmpBomLine
GO TOP IN (loFormSet.lcDetFile)
IF loFormSet.lcTranType = 'I'
=lfRecCost(loFormSet.lcTranType,EVALUATE(loFormSet.lcPosHdr+'.PO'),loFormSet.lcDetFile,loFormSet.lcTktSheet,loFormSet)
ENDIF
ENDIF
ENDIF
SELECT (loFormSet.lcTktSheet)

*!*************************************************************
*! Name      : lfvRemove
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/16/2004
*! Purpose   : Remove Cost Item
*!*************************************************************
*! Calls     : gfModalGen,lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove
LPARAMETERS loFormSet

LOCAL lcType,lnRecNo,lnItemCnt,llLanFound

*--- Doc. [Start]
*--- To remove cost item
*--- 1- Delete its record from CTKTBOM.DBF
*--- 2- Delete all Estimated/Adjusted record from BOMLINE.DBF
*--- 3- Update Estemated Cost. (POSHDR)
*--- 4- Delete its record from MFGOPRHD.DBF
*--- Doc. [End]

SELECT (loFormSet.lcTktSheet)
IF cShowType <> '1'
RETURN
ENDIF
lnRecNo = RECNO()
COUNT ALL FOR cShowType = '1' TO lnItemCnt
GO lnRECNO
IF lnItemCnt = 1
*Message : 38056
*Cannot remove all cost items
*Button : 00000
*Ok
=gfModalGen('TRM38056B00000','ALERT')
RETURN
ENDIF
IF EVALUATE(loFormSet.lcTktSheet+'.cCatgTyp') = 'P'
*Message : 38035
*Cannot remove the purchase cost.
*Button : 00000
*Ok
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*=gfModalGen('TRM38035B00000','ALERT','remove')
=gfModalGen('TRM38035B00000','ALERT',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_REMOVE,loFormSet.GetHeaderText("LANG_MFCSSH_REMOVE",loFormSet.HeaderAlias)))
*N000682,1 MMT 12/09/2012 Globalization changes[End]
RETURN
ENDIF
llLanFound = .F.
LOCAL lcBomLine
lcBomLine = gfTempName()
m.cImTyp  = loFormSet.lcTranType
m.cType   = '2'
m.cBomTyp = EVALUATE(loFormSet.lcTktSheet+'.TYP')
m.MfgCode = EVALUATE(loFormSet.lcTktSheet+'.MFGCODE')
m.CtktNo  = EVALUATE(loFormSet.lcPosHdr+'.PO')
IF lfOpenSql('BOMLINE',lcBomLine,'BOMPOREC','CIMTYP+CTYPE+CBOMTYP+MFGCODE+CTKTNO',loFormSet)
SELECT (lcBomLine)
LOCATE
IF !EOF()
=lfSetIndex(lcBomLine,'BOMPOREC','CIMTYP+CTYPE+CBOMTYP+MFGCODE+CTKTNO+CRSESSION+SHIPNO+STR(LINENO,6)+CINVTYPE+STYLE+CSTYGRADE')
SET ORDER TO TAG BOMPOREC DESCENDING
GO TOP
LOCATE REST WHILE CIMTYP+CTYPE+CBOMTYP+MFGCODE+CTKTNO+CRSESSION+SHIPNO+STR(LINENO,6)+CINVTYPE+STYLE+CSTYGRADE = ;
loFormSet.lcTranType+'2'+EVALUATE(loFormSet.lcTktSheet+'.TYP+'+loFormSet.lcTktSheet+'.MFGCODE+'+;
loFormSet.lcPosHdr+'.PO') .AND. !EMPTY(cRsession) ;
FOR   cInvTypC+Item =EVALUATE(loFormSet.lcTktSheet+'.cInvType+'+loFormSet.lcTktSheet+'.Item')
IF FOUND()
*Message : 38038
*This record has been considered as landed cost for the receiving
*session 999999. Cannot delete.
*Button : 00000
*Ok
=gfModalGen('TRM38038B00000','ALERT',CRSESSION)
USE IN (lcBomLine)
RETURN
ENDIF
ENDIF
USE IN (lcBomLine)
ENDIF

LOCAL lcKey
SELECT (loFormSet.lcTktSheet)
lcKey = cIMTyp+CutTkt+Typ+cInvType+Item+MfgCode+Dyelot
SELECT (loFormSet.lcCTKTBOM)
LOCATE FOR cIMTyp+CutTkt+Typ+cInvType+Item+MfgCode+Dyelot = lcKey
IF FOUND() .AND. Used_Qty <> 0
*Message : 38039
*Quantity has been issued from this record.
*You have to return this quantity before delete.
*Button : 00000
*Ok
=gfModalGen('TRM38039B00000','ALERT')
RETURN
ENDIF
IF EVALUATE(loFormSet.lcTktSheet+'.cCatgTyp')='M' .AND.;
SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO+'+loFormSet.lcTktSheet+'.MfgCode'),loFormSet.lcOprDet)
*Message : 38117
*Lots have been issued from operation xxxx. Cannot remove.
*Button : 00000
*Ok
=gfModalGen('TRM38117B00000','ALERT',ALLTRIM(gfCodDes(EVALUATE(loFormSet.lcTktSheet+'.MfgCode'),'MfgCode')))
RETURN
ENDIF
*Message : 38036
*Are you sure you want to remove this record?
*Button : 38002
*Proceed  Cancel
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen('QRM38036B38002','ALERT',LANG_MFCSSH_REMOVE) = 2
IF gfModalGen('QRM38036B38002','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_REMOVE,loFormSet.GetHeaderText("LANG_MFCSSH_REMOVE",loFormSet.HeaderAlias))) = 2
*N000682,1 11/20/2012 MMT Globlization changes[End]

RETURN
ENDIF
lcType = EVALUATE(loFormSet.lcTktSheet+'.TYP')
SELECT (loFormSet.lcCTKTBOM)
LOCATE FOR cIMTyp+CutTkt+Typ+cInvType+Item+MfgCode+Dyelot = lcKey
IF FOUND()
DELETE
ENDIF

*Call function to remove Pw detail operation.
*--- PwCtkBom Key == cuttkt+mfgcode+coprcode+STR(nlineno,6)
llDummy = loFormSet.llPWInst .AND. loFormSet.lcTranType = "M" .AND. lfPwRem(CutTkt,MfgCode,loFormSet)

SELECT (loFormSet.lcBOMLINE)
DELETE FOR CIMTYP+CBOMTYP+MFGCODE+CTKTNO+CINVTYPC+ITEM=;
loFormSet.lcTranType+lcType+EVALUATE(loFormSet.lcTktSheet+'.MFGCODE+'+loFormSet.lcPosHdr+'.PO')+;
EVALUATE(loFormSet.lcTktSheet+'.cInvType+'+loFormSet.lcTktSheet+'.Item')

*--- DOC.
*--- Start update the estimated cost , after remove cost element.
*--- DOC.

*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*=lfUpdEstCst(loFormSet)
DECLARE laFrgnCost[7], laEquCost[7]
STORE 0 TO laFrgnCost, laEquCost
=lfUpdEstCst(loFormSet,@laFrgnCost, @laEquCost)
*N000587,1 WAM 12/01/2007 (End)

SELECT (loFormSet.lcPOSHDR)
REPLACE ('nICost'+lcType) WITH EVALUATE('nICost'+lcType) - EVALUATE(loFormSet.lcTktSheet+'.Est_Cost')
IF loFormSet.lcTranType $ 'IDN'
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*!*	  DO CASE
*!*	    CASE EVALUATE(loFormSet.lcTktSheet+'.cCatgTyp')='P'
*!*	      REPLACE ('nFCost'+lcType) WITH EVALUATE('nFCost'+lcType+' - '+loFormSet.lcTktSheet+'.Est_Cost/(1 '+;
*!*	                                              loFormSet.lcPExSign+' '+loFormSet.lcPOSHDR+'.nPriceRat '+;
*!*	                                              loFormSet.lcPUntSin+' '+loFormSet.lcPOSHDR+'.nCurrUnit)')
*!*	    CASE INLIST(EVALUATE(loFormSet.lcTktSheet+'.cCatgTyp'),'S','T','F')
*!*	      REPLACE ('nFCost'+lcType) WITH EVALUATE('nFCost'+lcType+' - '+loFormSet.lcTktSheet+'.Est_Cost')
*!*	    OTHERWISE
*!*	      REPLACE ('nFCost'+lcType) WITH EVALUATE('nFCost'+lcType+' - '+loFormSet.lcTktSheet+'.Est_Cost/(1 '+;
*!*	                                              loFormSet.lcDExSign+' '+loFormSet.lcPOSHDR+'.nDutyRat '+;
*!*	                                              loFormSet.lcDUntSin+' '+loFormSet.lcPOSHDR+'.nDCurUnit)')
*!*	  ENDCASE
REPLACE ('nFCost'+lcType) WITH laFrgnCost [VAL(lcType)]
*N000587,1 WAM 12/01/2007 (End)
ENDIF

IF EVALUATE(loFormSet.lcTktSheet+'.cCatgTyp')='M'
SELECT (loFormSet.lcOprHdr)
SET ORDER TO TAG MFGOPRHD
IF SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO+'+loFormSet.lcTktSheet+'.MfgCode'))
DELETE
ENDIF
SET ORDER TO TAG (loFormSet.lcOprHdr) DESCENDING
GO TOP
LOCAL lcOprCode
lcOprCode = cOprCode
SET ORDER TO TAG (loFormSet.lcOprHdr) ASCENDING
GO TOP
loFormSet.lcFirstOpr = cOprCode
SELECT (loFormSet.lcMFGOPRHD)
LOCATE FOR COPRCODE = EVALUATE(loFormSet.lcTktSheet+'.MfgCode')
IF FOUND()
DELETE
SELECT (loFormSet.lcPosHdr)
REPLACE CLASTOPR WITH lcOprCode
ENDIF
ENDIF

IF loFormSet.ActiveMode="V"
DECLARE laTableUpdate[5,2]
laTableUpdate[1,1] = loFormSet.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'

laTableUpdate[2,1] = loFormSet.lcBomLine
laTableUpdate[2,2] = 'BOMLINE'

laTableUpdate[3,1] = loFormSet.lcMfgOprHd
laTableUpdate[3,2] = 'MFGOPRHD'

laTableUpdate[4,1] = loFormSet.lcPosHdr
laTableUpdate[4,2] = 'POSHDR'

laTableUpdate[5,1] = loFormSet.lcPosLn
laTableUpdate[5,2] = 'POSLN'

=lfTableUpdate(loFormSet)
ENDIF

SELECT (loFormSet.lcDetFile)
DELETE ALL FOR cBomTyp+cShowType+cInvTypC+Item+MfgCode = ;
EVALUATE(loFormSet.lcTktSheet+'.TYP+"1"+'+loFormSet.lcTktSheet+'.cInvType+'+;
loFormSet.lcTktSheet+'.Item+'+loFormSet.lcTktSheet+'.MfgCode')
GO TOP
LOCAL lcCurrKey,lnTotQty,lnTotAmt
DO WHILE !EOF()
lcCurrKey  = cInvType+Style+STR(LineNo,6)
IF !SEEK(lcCurrKey+EVALUATE(loFormSet.lcTktSheet+'.TYP')+'1')
=SEEK(lcCurrKey+EVALUATE(loFormSet.lcTktSheet+'.TYP'))
DELETE REST WHILE cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode = lcCurrKey+EVALUATE(loFormSet.lcTktSheet+'.TYP')
ELSE
SUM REST ItemQty,ItemAmt TO lnTotQty,lnTotAmt ;
WHILE cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode = lcCurrKey+EVALUATE(loFormSet.lcTktSheet+'.TYP')+'1'
REPLACE ItemQty WITH lnTotQty ,;
ItemAmt WITH lnTotAmt
ENDIF
IF SEEK(lcCurrKey)
REPLACE lHideTit WITH .F.
SCAN REST WHILE cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode = lcCurrKey
ENDSCAN
ELSE
GO TOP
ENDIF
ENDDO
GO TOP
SELECT (loFormSet.lcTktSheet)
DELETE
IF !SEEK(lcType+'1')
=SEEK(lcType+'0')
DELETE
ENDIF

IF loFormSet.lcTranType = 'I'
=lfRecCost(loFormSet.lcTranType,EVALUATE(loFormSet.lcPosHdr+'.PO'),;
IIF(EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H',loFormSet.lcBOMLINE,loFormSet.lcDetFile),;
IIF(EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H',loFormSet.lcCTKTBOM,loFormSet.lcTktSheet),loFormSet)
ENDIF

GO TOP
RETURN

*!*************************************************************
*! Name      : lfvModify
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/13/2004
*! Purpose   : Modify Cost Item
*!*************************************************************
*! Calls     : MFITMDET.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvModify()
*!*************************************************************
FUNCTION lfvModify
LPARAMETERS loFormSet
*--- DOC.
*--- Valid function for the modify button in the cost sheet folder
*--- DOC.
SELECT (loFormSet.lcTktSheet)
*--- DOC.
*--- If the current record is the cost element descriptoin record.
*--- the record that define the cost elemt , if MFg, fabric ..
*--- this functoin is tomodify the unit, Pieces and unitcost.
*--- DOC.
IF cShowType = '0'
RETURN
ENDIF
SCATTER MEMVAR
DO CASE
CASE INLIST(m.cCatgTyp,'P','D')
SELECT (loFormSet.lcDetFile)
LOCATE FOR cBomTyp+cInvTypC+Item+MfgCode+cShowType=m.Typ+m.cInvType+m.Item+m.MfgCode+'1'
SUM StyQty*UnitQty,UnitCost*StyQty*UnitQty TO m.Req_Qty,m.Est_Cost ;
FOR cBomTyp+cInvTypC+Item+MfgCode+cShowType = m.Typ+m.cInvType+m.Item+m.MfgCode+'1'
m.UntCost = m.Est_Cost/m.Req_Qty
CASE m.cCatgTyp='S' .AND. SEEK(m.Item,'Style')
=SEEK('S'+Style.Scale,'Scale')
CASE m.cCatgTyp$'FT'
m.Style = m.Item
IF lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loFormSet)
SELECT FABRIC
LOCATE
IF !EOF()
=SEEK('S'+FABRIC.Scale,'Scale')
ENDIF
ENDIF
ENDCASE
loFormSet.llHideDet = .T.
IF loFormSet.llPwInst .AND. loFormSet.lcTranType = 'M'
LOCAL lnOldAls
lnOldAls = SELECT(0)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*!*	  =gfOpenFile(oAriaApplication.DataDir+"pwoperat" , oAriaApplication.DataDir+"pwoperat" , "SH")
*!*	  loFormSet.llHideDet = !SEEK(EVALUATE(loFormSet.lcTktSheet+'.mfgCode'),'pwoperat')
=gfOpenTable("PWOPERAT" , "PWOPERAT")
loFormSet.llHideDet = !gfSEEK(EVALUATE(loFormSet.lcTktSheet+'.mfgCode'),'PWOPERAT')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
SELECT(lnOldAls)
ENDIF

PRIVATE loParentForm
loParentForm = loFormSet
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFITMDET.scx')
=gfCallForm('MFITMDET')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
IF loFormSet.lcTranType = 'I'
=lfRecCost(loFormSet.lcTranType,EVALUATE(loFormSet.lcPosHdr+'.PO'),;
IIF(EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H',loFormSet.lcBOMLINE,loFormSet.lcDetFile),;
IIF(EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H',loFormSet.lcCTKTBOM,loFormSet.lcTktSheet),loFormSet)
ENDIF

*!*************************************************************
*! Name      : lfvAccMod
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/14/2004
*! Purpose   : Accept Modification
*!*************************************************************
*! Calls     : gfModalGen,MFDSTRB.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvAccMod()
*!*************************************************************
FUNCTION lfvAccMod
LPARAMETERS llFrmDetal
*!*	IF m.cCatgTyp ='S' .AND. INT(m.UntQty) # m.UntQty
*!*	  *Message : 38284
*!*	  *Units cannot be fractions.
*!*	  *Button  : 00000
*!*	  *< OK >
*!*	  =gfModalGen('TRM38284B00000','ALERT')
*!*	  RETURN .F.
*!*	ENDIF

LOCAL lnAlias,lnLines,lnUnitQty,lnUnitCost,llUQtySame,llUCstSame,lcMsgTit,lnPieces,lnOldValue,lnOldEstCost
PRIVATE llOk
lnAlias = SELECT(0)
STORE 0   TO lnLines,lnPieces,lnOldValue,lnOldEstCost
STORE .T. TO llUQtySame,llUCstSame
SELECT (loParentForm.lcDetFile)
LOCATE FOR cBomTyp+cInvTypC+Item+MfgCode+cShowType=m.Typ+m.cInvType+m.Item+m.MfgCode+'1'
lnUnitQty  = UnitQty
lnUnitCost = UnitCost
*--- DOC.
*--- Get the current pieces , qty and unit cost.
*--- DOC.
SCAN REST FOR cBomTyp+cInvTypC+Item+MfgCode+cShowType=m.Typ+m.cInvType+m.Item+m.MfgCode+'1'
lnLines      = lnLines + 1
lnPieces     = lnPieces + StyQty
llUQtySame   = IIF(lnUnitQty  <> UnitQty, .F.,llUQtySame)
llUCstSame   = IIF(lnUnitCost <> UnitCost,.F.,llUCstSame)
lnOldEstCost = lnOldEstCost + (UnitCost*StyQty*UnitQty)
ENDSCAN
lcMsgTit = loParentForm.lcItmHdr
*Message : 38067
*The yield was the same for all style/colors. Do you wish to apply
*the same modification for all, or edit the yield for each style/color?
*Button : 38009
*Apply to All  Edit

*Message : 38068
*The yield was different for each style/color. Do you wish to edit
*the yield for each style/color, or Apply the same modification for all?
*Button : 38008
*Edit  Apply to All

LOCAL lcSavInvType,lcSavTyp,lcSavItem,lcSavMfgCode
lcSavInvType = m.cInvType
lcSavTyp     = m.Typ
lcSavItem    = m.Item
lcSavMfgCode = m.MfgCode

IF llFrmDetal .OR. (lnLines > 1 .AND. ;
((EVALUATE(loParentForm.lcTktSheet+'.Pieces')  <> m.Pieces)  .OR. ;
(EVALUATE(loParentForm.lcTktSheet+'.UntCost') <> m.UntCost .AND. !llUCstSame) .OR. ;
(EVALUATE(loParentForm.lcTktSheet+'.UntQty')  <> m.UntQty  .AND. ;
((!llUQtySame .AND. gfModalGen('QRM38068B38008','ALERT',lcMsgTit+'|'+lcMsgTit)=1) .OR. ;
(llUQtySame .AND. gfModalGen('QRM38067B38009','ALERT',lcMsgTit+'|'+lcMsgTit)=2)))))
SELECT * FROM (loParentForm.lcDetFile) ;
WHERE cBomTyp+cInvTypC+Item+MfgCode=m.Typ+m.cInvType+m.Item+m.MfgCode ;
INTO DBF (oAriaApplication.WorkDir+loParentForm.lcisslogfile)
IF !llFrmDetal
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFDSTRB.scx') WITH llFrmDetal
llFromDetail = llFrmDetal
=gfCallForm('MFDSTRB',.F.,"llFromDetail")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
ELSE
IF lfOpnStBrw(m.MfgCode)
DO WHILE .T.
llOk  = .F.
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFDSTRB.scx') WITH llFrmDetal
llFromDetail = llFrmDetal
=gfCallForm('MFDSTRB',.F.,"llFromDetail")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
IF !llOk
EXIT
ELSE
llOk = .F.
LOCAL lnTempCost
lnTempCost = lfvSelDet()
REPLACE UnitCost WITH IIF(lnTempCost = 0,UnitCost,lnTempCost)
*--- Function to update cost.
=lfvUpdate()
ENDIF
ENDDO
ELSE
*--- Else if we are not going to display style browse.
*--- So we will directly call detail operation screen.
LOCAL lnNewCost
lnNewCost = lfvSelDet()
*--- If the returned value = 0 this mean the user press close/cancel
*--- from the dateil screen
IF lnNewCost <> 0
m.UntCost  = lnNewCost
ENDIF
*--- then update the cost sheet files as it was
SELECT (loParentForm.lcDetFile)
LOCATE FOR cBomTyp+cInvTypC+Item+MfgCode+cShowType=m.Typ+m.cInvType+m.Item+m.MfgCode+'1'
REPLACE REST ;
UnitCost WITH m.UntCost ,;
UnitQty  WITH m.UntQty  ,;
StyQty   WITH IIF(lnLines=1,m.Pieces,StyQty)    ,;
ItemQty  WITH StyQty*UnitQty ,;
ItemAmt  WITH StyQty*UnitQty*UnitCost ;
FOR cBomTyp+cInvTypC+Item+MfgCode+cShowType=m.Typ+m.cInvType+m.Item+m.MfgCode+'1'
IF loParentForm.ActiveMode="V"
SELECT (loParentForm.lcBomLine)
LOCATE FOR ctype+cbomtyp+cInvTypC+item+mfgcode = '1'+m.Typ+m.cInvType+m.Item+m.MfgCode
IF FOUND()
REPLACE StyQty   WITH m.Pieces       ,;
UnitCost WITH m.UntCost      ,;
UnitQty  WITH m.UntQty       ,;
ItemQty  WITH StyQty*UnitQty ,;
ItemAmt  WITH StyQty*UnitQty*UnitCost
ENDIF
ENDIF
ENDIF
ENDIF
USE IN (loParentForm.lcisslogfile)
SELECT (loParentForm.lcDetFile)
ELSE
LOCATE FOR cBomTyp+cInvTypC+Item+MfgCode+cShowType=m.Typ+m.cInvType+m.Item+m.MfgCode+'1'
REPLACE REST ;
UnitCost WITH m.UntCost ,;
UnitQty  WITH m.UntQty  ,;
StyQty   WITH IIF(lnLines=1,m.Pieces,StyQty)    ,;
ItemQty  WITH StyQty*UnitQty ,;
ItemAmt  WITH StyQty*UnitQty*UnitCost ;
FOR cBomTyp+cInvTypC+Item+MfgCode+cShowType=m.Typ+m.cInvType+m.Item+m.MfgCode+'1'
IF loParentForm.ActiveMode="V"
SELECT (loParentForm.lcBomLine)
LOCATE FOR ctype+cbomtyp+cInvTypC+item+mfgcode = '1'+m.Typ+m.cInvType+m.Item+m.MfgCode
IF FOUND()
REPLACE StyQty   WITH IIF(lnLines=1,m.Pieces,StyQty),;
UnitCost WITH m.UntCost                     ,;
UnitQty  WITH m.UntQty                      ,;
ItemQty  WITH StyQty*UnitQty                ,;
ItemAmt  WITH StyQty*UnitQty*UnitCost        ;
FOR ctype+cbomtyp+cInvTypC+item+mfgcode = '1'+m.Typ+m.cInvType+m.Item+m.MfgCode
ENDIF
ENDIF
ENDIF

m.cInvType = lcSavInvType
m.Typ      = lcSavTyp
m.Item     = lcSavItem
m.MfgCode  = lcSavMfgCode

SELECT (loParentForm.lcDetFile)
SUM StyQty,StyQty*UnitQty,UnitCost*ItemQty TO m.Pieces,m.Req_Qty,m.Est_Cost ;
FOR cBomTyp+cInvTypC+Item+MfgCode+cShowType = m.Typ+m.cInvType+m.Item+m.MfgCode+'1'
lnFEstCst  = m.Est_Cost
lnOFEstCst = lnOldEstCost
DO CASE
CASE loParentForm.lcTranType $ 'IN' .AND. m.cCatgTyp='P'
m.Est_Cost   = EVALUATE('m.Est_Cost   '+loParentForm.lcPExSign+' '+loParentForm.lcPOSHDR+'.nPriceRat '+;
loParentForm.lcPUntSin+' '+loParentForm.lcPOSHDR+'.nCurrUnit')
lnOldEstCost = EVALUATE('lnOldEstCost '+loParentForm.lcPExSign+' '+loParentForm.lcPOSHDR+'.nPriceRat '+;
loParentForm.lcPUntSin+' '+loParentForm.lcPOSHDR+'.nCurrUnit')

CASE loParentForm.lcTranType $ 'ID' .AND. !INLIST(m.cCatgTyp,'S','F','T')
m.Est_Cost   = EVALUATE('m.Est_Cost   '+loParentForm.lcDExSign+' '+loParentForm.lcPOSHDR+'.nDutyRat '+;
loParentForm.lcDUntSin+' '+loParentForm.lcPOSHDR+'.nDCurUnit')
lnOldEstCost = EVALUATE('lnOldEstCost '+loParentForm.lcDExSign+' '+loParentForm.lcPOSHDR+'.nDutyRat '+;
loParentForm.lcDUntSin+' '+loParentForm.lcPOSHDR+'.nDCurUnit')
ENDCASE
m.UntCost = m.Est_Cost/m.Req_Qty
m.UntQty  = m.Req_Qty/m.Pieces
SELECT (loParentForm.lcTktSheet)
IF m.cCatgTyp $ 'SFT'
REPLACE Req_Qty1 WITH m.Req_Qty1 ,;
Req_Qty2 WITH m.Req_Qty2 ,;
Req_Qty3 WITH m.Req_Qty3 ,;
Req_Qty4 WITH m.Req_Qty4 ,;
Req_Qty5 WITH m.Req_Qty5 ,;
Req_Qty6 WITH m.Req_Qty6 ,;
Req_Qty7 WITH m.Req_Qty7 ,;
Req_Qty8 WITH m.Req_Qty8
ENDIF
REPLACE UntCost  WITH m.UntCost ,;
UntQty   WITH m.UntQty  ,;
Pieces   WITH m.Pieces  ,;
Req_Qty  WITH m.Req_Qty ,;
Est_Cost WITH m.Est_Cost
REPLACE Desc     WITH m.Desc
REPLACE CMARKER WITH m.cMarker

SELECT (loParentForm.lcPosHdr)
REPLACE ('nICost'+m.Typ) WITH EVALUATE('nICost'+m.Typ) - lnOldEstCost + m.Est_Cost
IF loParentForm.lcTranType $ 'IDN'
REPLACE ('NFCOST'+m.Typ) WITH EVALUATE('NFCOST'+m.Typ) - lnOFEstCst + lnFEstCst
ENDIF

SELECT (loParentForm.lcCTktBom)
LOCATE FOR cIMTyp+CutTkt+Typ+cInvType+Item+MfgCode+Dyelot =;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+m.typ+m.cInvType+m.Item+m.MfgCode+m.Dyelot
IF FOUND()
REPLACE UntCost  WITH m.UntCost ,;
UntQty   WITH m.UntQty  ,;
Pieces   WITH m.Pieces  ,;
Req_Qty  WITH m.Req_Qty ,;
Est_Cost WITH m.Est_Cost
REPLACE Desc     WITH m.Desc
REPLACE CMARKER WITH m.cMarker
REPLACE Req_Qty1 WITH m.Req_Qty1 ,;
Req_Qty2 WITH m.Req_Qty2 ,;
Req_Qty3 WITH m.Req_Qty3 ,;
Req_Qty4 WITH m.Req_Qty4 ,;
Req_Qty5 WITH m.Req_Qty5 ,;
Req_Qty6 WITH m.Req_Qty6 ,;
Req_Qty7 WITH m.Req_Qty7 ,;
Req_Qty8 WITH m.Req_Qty8
ENDIF

*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*=lfUpdEstCst(loFormSet)
DECLARE laFrgnCost[7], laEquCost[7]
STORE 0 TO laFrgnCost, laEquCost
=lfUpdEstCst(loParentForm,@laFrgnCost, @laEquCost)
*N000587,1 WAM 12/01/2007 (End)

IF loParentForm.ActiveMode="V"
DECLARE laTableUpdate[4,2]
laTableUpdate[1,1] = loParentForm.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'

laTableUpdate[2,1] = loParentForm.lcBomLine
laTableUpdate[2,2] = 'BOMLINE'

laTableUpdate[3,1] = loParentForm.lcPosHdr
laTableUpdate[3,2] = 'POSHDR'

laTableUpdate[4,1] = loParentForm.lcPosLn
laTableUpdate[4,2] = 'POSLN'

=lfTableUpdate(loParentForm)
ENDIF

SELECT (loParentForm.lcDetFile)
GO TOP
DO WHILE !EOF()
lcCurrKey = cInvType+Style+STR(LineNo,6)+cBomTyp
SKIP
SUM REST ItemQty,ItemAmt TO lnTotQty,lnTotAmt WHILE ;
cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode = lcCurrKey+'1'
REPLACE ItemQty WITH lnTotQty,;
ItemAmt WITH lnTotAmt
SKIP
ENDDO
GO TOP
SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfvCanQty
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/13/2004
*! Purpose   : Validate conributed canceled quantity
*!*************************************************************
*! Calls     : gfModalGen,lfRefresh()
*!*************************************************************
*! Parameters: lcSize : Style size
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvCanQty('1')
*!*************************************************************
FUNCTION lfvCanQty
LPARAMETERS loThis

LOCAL lcSize
lcSize = RIGHT(loThis.Parent.ControlSource,1)
SELECT(loParentForm.lcisslogfile)
IF EVALUATE('nCan'+lcSize) > EVALUATE('m.nQty'+lcSize)
REPLACE ('nCan'+lcSize) WITH loThis.OldValue
*Message : 38073
*Caneclled quantity cannot exceed budget quantity
*Button : 00000
*Ok
=gfModalGen('TRM38073B00000','ALERT')
RETURN
ENDIF
REPLACE nTotCan WITH nTotCan - loThis.OldValue + EVALUATE('nCan'+lcSize)
lnBal&lcSize. = EVALUATE('lnBal'+lcSize)  + loThis.OldValue - EVALUATE('nCan'+lcSize)
lnTotBal = lnTotBal + loThis.OldValue - EVALUATE('nCan'+lcSize)
loThis.Parent.Parent.Parent.Parent.Refresh

*!*************************************************************
*! Name      : lfvOkConCn
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/30/2004
*! Purpose   : Update conributed canceled quantity
*!*************************************************************
*! Calls     : gfModalGen()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOkConCn()
*!*************************************************************
FUNCTION lfvOkConCn

IF lnBal1<>0 .OR. lnBal2<>0 .OR. lnBal3<>0 .OR. lnBal4<>0 .OR. lnBal5<>0 .OR. ;
lnBal6<>0 .OR. lnBal7<>0 .OR. lnBal8<>0 .OR. lnTotBal<>0
*E300725,1 Message : 38074
*E300725,1 Contributed caneclled quantity does not total ticket cancelled
*E300725,1 remainning quantity
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38074B00000','ALERT')
GO TOP
RETURN .F.
ENDIF

SELECT(loParentForm.lcisslogfile)
SCAN
SELECT (loParentForm.lcTmpTkt)
GO EVALUATE(loParentForm.lcisslogfile+'.nBudRecNo')
REPLACE nQty1   WITH nQty1   - EVALUATE(loParentForm.lcisslogfile+'.nCan1'),;
nQty2   WITH nQty2   - EVALUATE(loParentForm.lcisslogfile+'.nCan2'),;
nQty3   WITH nQty3   - EVALUATE(loParentForm.lcisslogfile+'.nCan3'),;
nQty4   WITH nQty4   - EVALUATE(loParentForm.lcisslogfile+'.nCan4'),;
nQty5   WITH nQty5   - EVALUATE(loParentForm.lcisslogfile+'.nCan5'),;
nQty6   WITH nQty6   - EVALUATE(loParentForm.lcisslogfile+'.nCan6'),;
nQty7   WITH nQty7   - EVALUATE(loParentForm.lcisslogfile+'.nCan7'),;
nQty8   WITH nQty8   - EVALUATE(loParentForm.lcisslogfile+'.nCan8'),;
nTotQty WITH nTotQty - EVALUATE(loParentForm.lcisslogfile+'.nTotCan')
ENDSCAN

*!*************************************************************
*! Name      : lfvUpdate
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/15/2004
*! Purpose   : Validate contributed unit cost and unit quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvUpdate()
*!*************************************************************
FUNCTION lfvUpdate
REPLACE ItemQty  WITH StyQty*UnitQty,;
ItemAmt  WITH StyQty*UnitQty*UnitCost

*!*************************************************************
*! Name      : lfOkDistrb
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/15/2004
*! Purpose   : Accept contribution of cost item unit cost and quantity
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfOkDistrb()
*!*************************************************************
FUNCTION lfOkDistrb
LPARAMETERS loFormSet

LOCAL lnPieces,lnReq,lnAmount,lnRecNo

lnRecNo = RECNO()
SUM StyQty,StyQty*UNITQTY,ItemAmt TO lnPieces,lnReq,lnAmount
GO lnRecNo
*Message : 38069
*Contributed pieces don't equal the total pieces.
*Button : 38010
*Proceed  Resume
IF lnPieces <> m.Pieces .AND. gfModalGen('QRM38069B38010','ALERT') = 2
RETURN .F.
ENDIF
*Message : 38070
*The calculated average unit cost 999 don't equal the entered
*average unit cost 999.
*Button : 38010
*Proceed  Resume
IF !loFormSet.llFrmDetal .AND. loParentForm.llHideDet
IF ROUND(lnAmount/lnReq,3) <> m.UntCost .AND. ;
gfModalGen('QRM38070B38010','ALERT',STR(ROUND(lnAmount/lnReq,3),7,3)+'|'+STR(m.UntCost,7,3)) = 2
RETURN .F.
ENDIF
ENDIF
*Message : 38071
*The calculated average unit quantity 999 don't equal the entered
*average unit quantity 999.
*Button : 38010
*Proceed  Resume
IF ROUND(lnReq/lnPieces,3) <> m.UntQty .AND. ;
gfModalGen('QRM38071B38010','ALERT',STR(ROUND(lnReq/lnPieces,3),7,3)+'|'+STR(m.UntQty,7,3)) = 2
RETURN .F.
ENDIF
SCAN
SCATTER MEMVAR
IF SEEK(cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode,loParentForm.lcDetFile)
SELECT (loParentForm.lcDetFile)
GATHER MEMVAR
ENDIF
IF loParentForm.ActiveMode="V"
SELECT (loParentForm.lcBomLine)
LOCATE FOR ctype+STR(lineno,6)+cbomtyp+cInvType+style+cInvTypC+item+mfgcode =;
m.ctype+STR(m.lineno,6)+m.cbomtyp+m.cInvType+m.style+m.cInvTypC+m.item+m.mfgcode
GATHER MEMVAR
ENDIF
ENDSCAN

DECLARE laTableUpdate[1,2]
laTableUpdate[1,1] = loParentForm.lcBomLine
laTableUpdate[1,2] = 'BOMLINE'

=lfTableUpdate(loParentForm)

RETURN

*!*************************************************************
*! Name      : lfvVoid
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/04/2004
*! Purpose   : Void Cost Item
*!*************************************************************
*! Calls     : gfModalGen,lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvVoid()
*!*************************************************************
FUNCTION lfvVoid
LPARAMETERS loFormSet

LOCAL lcType,lnRecNo,lnUnVoidRec,lcCurrKey,lnTotQty,lnTotAmt,llLanFound
*--- DOC.
*--- Valid function for void cost element
*--- this function replace the field LVOID with .T. in the CTKTBOM file
*--- then update the bomline and the cuttkth, file
*--- DOC.
SELECT (loFormSet.lcTktSheet)
IF cShowType <> '1'
RETURN
ENDIF
IF cCatgTyp = 'P'
*E300725,1 Message : 38035
*E300725,1 Cannot void the purchase cost.
*E300725,1 Button : 00000
*E300725,1 Ok
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM38035B00000','ALERT',LANG_MFCSSH_VOID)
=gfModalGen('TRM38035B00000','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_VOID,loFormSet.GetHeaderText("LANG_MFCSSH_VOID",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

RETURN
ENDIF
lnRecNo = RECNO()
COUNT TO lnUnVoidRec FOR cShowType='1'
GO lnRecNo
IF lnUnVoidRec = 1
*E300725,1 Message : 38037
*E300725,1 You cannot void all cost items.
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38037B00000','ALERT')
RETURN
ENDIF
llLanFound = .F.
LOCAL lcBomLine
lcBomLine = gfTempName()
m.cImTyp  = loFormSet.lcTranType
m.cType   = '2'
m.cBomTyp = EVALUATE(loFormSet.lcTktSheet+'.TYP')
m.MfgCode = EVALUATE(loFormSet.lcTktSheet+'.MFGCODE')
m.CtktNo  = EVALUATE(loFormSet.lcPosHdr+'.PO')
IF lfOpenSql('BOMLINE',lcBomLine,'BOMPOREC','CIMTYP+CTYPE+CBOMTYP+MFGCODE+CTKTNO',loFormSet)
SELECT (lcBomLine)
LOCATE
IF !EOF()
=lfSetIndex(lcBomLine,'BOMPOREC','CIMTYP+CTYPE+CBOMTYP+MFGCODE+CTKTNO+CRSESSION+SHIPNO+STR(LINENO,6)+CINVTYPE+STYLE+CSTYGRADE')
SET ORDER TO TAG BOMPOREC DESCENDING
GO TOP
LOCATE REST WHILE CIMTYP+CTYPE+CBOMTYP+MFGCODE+CTKTNO+CRSESSION+SHIPNO+STR(LINENO,6)+CINVTYPE+STYLE+CSTYGRADE = ;
loFormSet.lcTranType+'2'+EVALUATE(loFormSet.lcTktSheet+'.TYP+'+loFormSet.lcTktSheet+'.MFGCODE+'+;
loFormSet.lcPosHdr+'.PO') .AND. !EMPTY(cRsession) ;
FOR   cInvTypC+Item =EVALUATE(loFormSet.lcTktSheet+'.cInvType+'+loFormSet.lcTktSheet+'.Item')
IF FOUND()
*Message : 38285
*This record has been considered as landed cost for the receiving
*session 999999. Cannot void.
*Button : 00000
*Ok
=gfModalGen('TRM38285B00000','ALERT',CRSESSION)
USE IN (lcBomLine)
RETURN
ENDIF
ENDIF
USE IN (lcBomLine)
ENDIF
LOCAL lcKey
SELECT (loFormSet.lcTktSheet)
lcKey = cIMTyp+CutTkt+Typ+cInvType+Item+MfgCode+Dyelot
SELECT (loFormSet.lcCTKTBOM)
LOCATE FOR cIMTyp+CutTkt+Typ+cInvType+Item+MfgCode+Dyelot = lcKey
IF FOUND() .AND. Used_Qty <> 0
*Message : 38286
*Quantity has been issued from this record.
*You have to return this quantity before void.
*Button : 00000
*Ok
=gfModalGen('TRM38286B00000','ALERT')
RETURN
ENDIF
IF EVALUATE(loFormSet.lcTktSheet+'.cCatgTyp')='M' .AND.;
SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO+'+loFormSet.lcTktSheet+'.MfgCode'),loFormSet.lcOprDet)
*Message : 38287
*Lots have been issued from operation xxxx. Cannot void.
*Button : 00000
*Ok
=gfModalGen('TRM38287B00000','ALERT',ALLTRIM(gfCodDes(EVALUATE(loFormSet.lcTktSheet+'.MfgCode'),'MfgCode')))
RETURN
ENDIF
*E300725,1 Message : 38036
*E300725,1 Are you sure you want to void this record?
*E300725,1 Button : 38002
*E300725,1 Proceed  Cancel
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen('QRM38036B38002','ALERT',LANG_MFCSSH_VOID) = 2
IF gfModalGen('QRM38036B38002','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_VOID,loFormSet.GetHeaderText("LANG_MFCSSH_VOID",loFormSet.HeaderAlias))) = 2
*N000682,1 11/20/2012 MMT Globlization changes[End]

RETURN
ENDIF

lcType = Typ
SELECT (loFormSet.lcCTKTBOM)
LOCATE FOR cIMTyp+CutTkt+Typ+cInvType+Item+MfgCode+Dyelot = lcKey
IF FOUND()
REPLACE LVOID WITH .T.
ENDIF
SELECT (loFormSet.lcBomLine)
REPLACE ALL LVOID WITH .T. FOR cBomTyp+cInvTypC+Item+MfgCode =;
EVALUATE(loFormSet.lcTktSheet+'.Typ+'+loFormSet.lcTktSheet+'.cInvType+'+loFormSet.lcTktSheet+'.Item+'+;
loFormSet.lcTktSheet+'.MfgCode')
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*=lfUpdEstCst(loFormSet)
DECLARE laFrgnCost[7], laEquCost[7]
STORE 0 TO laFrgnCost, laEquCost
=lfUpdEstCst(loFormSet,@laFrgnCost, @laEquCost)
*N000587,1 WAM 12/01/2007 (End)

*Update the corresponding estimated cost in the header file
SELECT (loFormSet.lcPOSHDR)
REPLACE ('nICost'+lcType) WITH EVALUATE('nICost'+lcType) - EVALUATE(loFormSet.lcTktSheet+'.Est_Cost')
IF loFormSet.lcBusDocu+loFormSet.lcStyType $ 'PPDNN'
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*!*	  DO CASE
*!*	    CASE EVALUATE(loFormSet.lcTktSheet+'.cCatgTyp')='P'
*!*	      REPLACE ('nFCost'+lcType) WITH EVALUATE('nFCost'+lcType+' - '+loFormSet.lcTktSheet+'.Est_Cost/(1 '+loFormSet.lcPExSign+' '+loFormSet.lcPOSHDR+'.nPriceRat '+loFormSet.lcPUntSin+' '+loFormSet.
*!*	    CASE INLIST(EVALUATE(loFormSet.lcTktSheet+'.cCatgTyp'),'S','T','F')
*!*	      REPLACE ('nFCost'+lcType) WITH EVALUATE('nFCost'+lcType+' - '+loFormSet.lcTktSheet+'.Est_Cost')
*!*	    OTHERWISE
*!*	      REPLACE ('nFCost'+lcType) WITH EVALUATE('nFCost'+lcType+' - '+loFormSet.lcTktSheet+'.Est_Cost/(1 '+loFormSet.lcDExSign+' '+loFormSet.lcPOSHDR+'.nDutyRat '+loFormSet.lcDUntSin+' '+loFormSet.l
*!*	  ENDCASE
REPLACE ('nFCost'+lcType) WITH laFrgnCost [VAL(lcType)]
*N000587,1 WAM 12/01/2007 (End)
ENDIF

IF EVALUATE(loFormSet.lcTktSheet+'.cCatgTyp')='M'
SELECT (loFormSet.lcOprHdr)
SET ORDER TO TAG MFGOPRHD
IF SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO+'+loFormSet.lcTktSheet+'.MfgCode'))
DELETE
ENDIF
SET ORDER TO TAG (loFormSet.lcOprHdr) DESCENDING
GO TOP
LOCAL lcOprCode
lcOprCode = cOprCode
SET ORDER TO TAG (loFormSet.lcOprHdr) ASCENDING
GO TOP
loFormSet.lcFirstOpr = cOprCode
SELECT (loFormSet.lcMFGOPRHD)
LOCATE FOR COPRCODE = EVALUATE(loFormSet.lcTktSheet+'.MfgCode')
IF FOUND()
REPLACE lvoid WITH .T.
SELECT (loFormSet.lcPosHdr)
REPLACE CLASTOPR WITH lcOprCode
ENDIF
ENDIF

DECLARE laTableUpdate[5,2]
laTableUpdate[1,1] = loFormSet.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'

laTableUpdate[2,1] = loFormSet.lcBomLine
laTableUpdate[2,2] = 'BOMLINE'

laTableUpdate[3,1] = loFormSet.lcMfgOprHd
laTableUpdate[3,2] = 'MFGOPRHD'

laTableUpdate[4,1] = loFormSet.lcPosHdr
laTableUpdate[4,2] = 'POSHDR'

laTableUpdate[5,1] = loFormSet.lcPosLn
laTableUpdate[5,2] = 'POSLN'

=lfTableUpdate(loFormSet)

SELECT (loFormSet.lcDetFile)
DELETE ALL FOR cBomTyp+cShowType+cInvTypC+Item+MfgCode = ;
EVALUATE(loFormSet.lcTktSheet+'.TYP+"1"+'+loFormSet.lcTktSheet+'.cInvType+'+loFormSet.lcTktSheet+'.Item+'+loFormSet.lcTktSheet+'.MfgCode')
GO TOP
DO WHILE !EOF()
lcCurrKey = cInvType+Style+STR(LineNo,6)
IF !SEEK(lcCurrKey+EVALUATE(loFormSet.lcTktSheet+'.TYP')+'1')
=SEEK(lcCurrKey+EVALUATE(loFormSet.lcTktSheet+'.TYP'))
DELETE REST WHILE cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode =;
lcCurrKey+EVALUATE(loFormSet.lcTktSheet+'.TYP')
ELSE
SUM REST ItemQty,ItemAmt TO lnTotQty,lnTotAmt ;
WHILE cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode = lcCurrKey+EVALUATE(loFormSet.lcTktSheet+'.TYP')+'1'
REPLACE ItemQty WITH lnTotQty ,;
ItemAmt WITH lnTotAmt
ENDIF
IF SEEK(lcCurrKey)
REPLACE lHideTit WITH .F.
SCAN REST WHILE cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode  = lcCurrKey
ENDSCAN
ELSE
GO TOP
ENDIF
ENDDO
GO TOP
SELECT (loFormSet.lcTktSheet)
DELETE
IF !SEEK(lcType+'1')
=SEEK(lcType+'0')
DELETE
ENDIF
GO TOP
RETURN

*!*************************************************************
*! Name      : lfvUnVoid
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/04/2004
*! Purpose   : Unvoid Cost Item
*!*************************************************************
*! Calls     : ARIABROW,lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvUnVoid()
*!*************************************************************
FUNCTION lfvUnVoid
LPARAMETERS loFormSet

LOCAL lnAlias,lcType,lcCurrKey
*--- DOC.
*--- Unvoid cost item by replace the fiel lvoid with .T. and update
*--- CutTktH and BOMLINE
*--- this function browse for all the cost element that has been voided
*--- and browse them to select one to un void
*--- DOC.
lnAlias = SELECT(0)
SELECT *,RECNO() AS nRecNo,lfGetUom(cUomCode) AS UOM;
FROM (loFormSet.lcCTKTBOM);
WHERE lVoid;
INTO CURSOR (loFormSet.lcisslogfile)
IF _TALLY > 0
DIMENSION laTempData[1]
STORE '' TO laTempData
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl = LANG_MFCSSH_VODCSTSHT
lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_VODCSTSHT,loFormSet.GetHeaderText("LANG_MFCSSH_VODCSTSHT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcBrFields = ;
"cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),Item) :20 :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ITEM,loFormSet.GetHeaderText("LANG_MFCSSH_ITEM",loFormSet.HeaderAlias))+"':R,"+;
IIF(loFormSet.llUseDyelot,"Dyelot :H='"+loFormSet.AriaForm1.pgfCstSht.Page1.grdCstSht.Column2.Header1.Caption+"':R,","")+;
"Uom       :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_UOM,loFormSet.GetHeaderText("LANG_MFCSSH_UOM",loFormSet.HeaderAlias))+"':R ,"+;
"UntQty    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_UNITS,loFormSet.GetHeaderText("LANG_MFCSSH_UNITS",loFormSet.HeaderAlias))+"':R,"+;
"UntCost   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_COST,loFormSet.GetHeaderText("LANG_MFCSSH_COST",loFormSet.HeaderAlias))+"':R,"+;
"Req_Qty   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_REQUIRED,loFormSet.GetHeaderText("LANG_MFCSSH_REQUIRED",loFormSet.HeaderAlias))+"':R,"+;
"Issue_Qty :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUED,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUED",loFormSet.HeaderAlias))+"':R,"+;
"Used_Qty  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_USED,loFormSet.GetHeaderText("LANG_MFCSSH_USED",loFormSet.HeaderAlias))+"':R"



IF gfBrows('','ITEM','laTempData',lcFile_Ttl,.F.)
GO EVALUATE(loFormSet.lcisslogfile+'.nRecNo') IN (loFormSet.lcCTKTBOM)
IF !SEEK(Typ+'0',loFormSet.lcTktSheet)
INSERT INTO (loFormSet.lcTktSheet) (TYP,cShowType,Item,Dyelot) VALUES ;
(EVALUATE(loFormSet.lccTktBom+'.Typ'),'0',;
loFormSet.laSetups[2+VAL(EVALUATE(loFormSet.lcCTKTBOM+'.Typ')),2],;
lfGetDye(loFormSet,VAL(EVALUATE(loFormSet.lccTktBom+'.Typ'))))
ENDIF

SELECT (loFormSet.lcCTKTBOM)
REPLACE lVoid WITH .F.
SCATTER MEMVAR
m.cShowType = '1'
m.TotStk    = lfGetTotStk(loFormSet)
m.UomUse    = lfGetUom(m.cUomCode)
INSERT INTO (loFormSet.lcTktSheet) FROM MEMVAR

LOCAL loSctData
SELECT (loFormSet.lcBOMLINE)
SCAN FOR cBomTyp+cInvTypC+Item+MfgCode = m.Typ+m.cInvType+m.Item+m.MfgCode .AND. lVoid
IF !SEEK(cInvType+Style+STR(LineNo,6)+cBomTyp+'0',loFormSet.lcDetFile)
LOCAL llHideTit
llHideTit = SEEK(cInvType+Style+STR(LineNo,6),loFormSet.lcDetFile)
INSERT INTO (loFormSet.lcDetFile) ;
(cInvType,Style,LineNo,cBomTyp,cShowType,Item,lHideTit,Dyelot) VALUES ;
(EVALUATE(loFormSet.lcBomLine+'.cInvType'),EVALUATE(loFormSet.lcBomLine+'.Style'),;
EVALUATE(loFormSet.lcBomLine+'.LineNo'),EVALUATE(loFormSet.lcBomLine+'.cBomTyp'),'0',;
loFormSet.laSetups[2+VAL(EVALUATE(loFormSet.lcBomLine+'.cBomTyp')),2],llHideTit,;
lfGetDye(loFormSet,VAL(EVALUATE(loFormSet.lcBomLine+'.cBomTyp'))))
ENDIF
REPLACE lVoid WITH .F.
SCATTER NAME loSctData
=ADDPROPERTY(loSctData,'cShowType','1')
INSERT INTO (loFormSet.lcDetFile) FROM NAME loSctData
IF !SEEK(cInvType+Style+STR(LineNo,6)+cBomTyp+'2',loFormSet.lcDetFile)
INSERT INTO (loFormSet.lcDetFile) ;
(cInvType,Style,LineNo,cBomTyp,cShowType,Item) VALUES ;
(EVALUATE(loFormSet.lcBomLine+'.cInvType'),EVALUATE(loFormSet.lcBomLine+'.Style'),;
EVALUATE(loFormSet.lcBomLine+'.LineNo'),EVALUATE(loFormSet.lcBomLine+'.cBomTyp'),'2','***** '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_TOTAL,loFormSet.GetHeaderText("LANG_MFCSSH_TOTAL",loFormSet.HeaderAlias))+' :')


ENDIF
SELECT (loFormSet.lcDetFile)
REPLACE ItemQty  WITH ItemQty + EVALUATE(loFormSet.lcBomLine+'.ItemQty'),;
ItemAmt  WITH ItemAmt + EVALUATE(loFormSet.lcBomLine+'.ItemAmt')
ENDSCAN
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*=lfUpdEstCst(loFormSet)
DECLARE laFrgnCost[7], laEquCost[7]
STORE 0 TO laFrgnCost, laEquCost
=lfUpdEstCst(loFormSet,@laFrgnCost, @laEquCost)
*N000587,1 WAM 12/01/2007 (End)

IF EVALUATE(loFormSet.lcCTKTBOM+'.cCatgTyp') = 'M'
SELECT (loFormSet.lcMFGOPRHD)
LOCATE FOR COPRCODE = EVALUATE(loFormSet.lcCTKTBOM+'.MfgCode')
IF FOUND()
REPLACE lvoid WITH .F.
SCATTER MEMVAR
SELECT (loFormSet.lcOprHdr)
APPEND BLANK
GATHER MEMVAR
ENDIF
ENDIF
*Update the corresponding estimated cost in the header file
lcType = EVALUATE(loFormSet.lcCTKTBOM+'.Typ')
SELECT (loFormSet.lcPOSHDR)
REPLACE ('nICost'+lcType) WITH EVALUATE('nICost'+lcType+' + '+loFormSet.lcCTKTBOM+'.Est_Cost')
IF loFormSet.lcBusDocu+loFormSet.lcStyType $ 'PPDNN'
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*!*	      DO CASE
*!*	        CASE EVALUATE(loFormSet.lcCTKTBOM+'.cCatgTyp')='P'
*!*	          REPLACE ('nFCost'+lcType) WITH EVALUATE('nFCost'+lcType+' + '+loFormSet.lcCTKTBOM+'.Est_Cost/(1 '+loFormSet.lcPExSign+' '+loFormSet.lcPOSHDR+'.nPriceRat '+loFormSet.lcPUntSin+' '+loFormS
*!*	        CASE INLIST(EVALUATE(loFormSet.lcCTKTBOM+'.cCatgTyp'),'S','T','F')
*!*	          REPLACE ('nFCost'+lcType) WITH EVALUATE('nFCost'+lcType+' + '+loFormSet.lcCTKTBOM+'.Est_Cost')
*!*	        OTHERWISE
*!*	          REPLACE ('nFCost'+lcType) WITH EVALUATE('nFCost'+lcType+' + '+loFormSet.lcCTKTBOM+'.Est_Cost/(1 '+loFormSet.lcDExSign+' '+loFormSet.lcPOSHDR+'.nDutyRat '+loFormSet.lcDUntSin+' '+loFormSe
*!*	      ENDCASE
REPLACE ('nFCost'+lcType) WITH laFrgnCost [VAL(lcType)]
*N000587,1 WAM 12/01/2007 (End)
ENDIF

DECLARE laTableUpdate[5,2]
laTableUpdate[1,1] = loFormSet.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'

laTableUpdate[2,1] = loFormSet.lcBomLine
laTableUpdate[2,2] = 'BOMLINE'

laTableUpdate[3,1] = loFormSet.lcMfgOprHd
laTableUpdate[3,2] = 'MFGOPRHD'

laTableUpdate[4,1] = loFormSet.lcPosHdr
laTableUpdate[4,2] = 'POSHDR'

laTableUpdate[5,1] = loFormSet.lcPosLn
laTableUpdate[5,2] = 'POSLN'

=lfTableUpdate(loFormSet)

SELECT (loFormSet.lcDetFile)
GO TOP
DO WHILE !EOF()
lcCurrKey = cInvType+Style+STR(LineNo,6)
REPLACE lHideTit WITH .F.
SKIP
REPLACE REST lHideTit WITH .T. WHILE cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode=lcCurrKey
ENDDO
GO TOP
SELECT (loFormSet.lcTktSheet)
ENDIF
ENDIF
USE IN (loFormSet.lcisslogfile)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvIssLog
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/04/2004
*! Purpose   : Issue/Return Cost Item
*!*************************************************************
*! Calls     : MFISLOG.SPX,lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvIssLog()
*!*************************************************************
FUNCTION lfvIssLog
LPARAMETERS loFormSet

PRIVATE lnAlias,lcCondition,lcRetCond,lnLotNo,laLots,llItemDye,llDispRoll,llDispAct,llDispUsed
*--- DOC.
*--- Check if the current record in the browse is cost item not title
*--- DOC.

SELECT (loFormSet.lcTktSheet)
IF cShowType <> '1'
RETURN
ENDIF
DECLARE laLots[1]
lnLotNo = 1
m.cOprCode = cOprCode
IF !EMPTY(m.cOprCode)
*--- DOC.
*--- Get the curret operatoin record from mfgoprdt.dbf
*--- to be sure that this operation has cost item , and lots
*--- DOC.
SELECT DIST cLotNo FROM (loFormSet.lcOPrDet) WHERE cOprCode = m.cOprCode INTO ARRAY laLots
IF _TALLY = 0
*Message : 38089
*Cost item xxxxx has been assigned to operation xxxxx.
*No lots has been generated for this operation. Cannot Issue.
*Button : 00000
*Ok
=gfModalGen('TRM38089B00000','ALERT',ALLTRIM(Item)+'|'+ALLTRIM(gfCodDes(m.cOprCode,'MfgCode')))
RETURN
ENDIF
ENDIF
lnAlias = SELECT(0)
STORE 0 TO lnLMarker
lcCondition = ''
lcRetCond   = ''

PRIVATE lcOldSheet
lcOldSheet = gfTempName()
SELECT (loFormSet.lcTktSheet)
=AFIELDS(laFileStru)
=gfCrtTmp(lcOldSheet,@laFileStru)
SELECT (loFormSet.lcTktSheet)
SCATTER MEMVAR
*B610572,1 TMI 11/03/2013 10:35 [Start] assign the value of m.CINVTYPC
m.CINVTYPC = IIF(m.CCATGTYP='S','0001',IIF(m.CCATGTYP$'FT','0002',''))
*B610572,1 TMI 11/03/2013 10:37 [End  ]
SELECT (lcOldSheet)
APPEND BLANK
GATHER MEMVAR
SELECT (loFormSet.lcTktSheet)
*Check if fabric exist in the fabric file before issue
DO CASE
CASE m.cCatgTyp='F' OR (m.cCatgTyp='T' AND m.Trim_Invt)
LOCAL llFound
llFound    = .F.
m.Style    = m.Item
IF lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loFormSet)
SELECT FABRIC
LOCATE
llFound = !EOF()
ENDIF
IF !llFound
*Message : 38156
*Material XXxX does not exist in the material file
*Button : 00000
*Ok
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM38156B00000','ALERT',LANG_MFCSSH_MATCLR+': '+ALLTRIM(Item)+'|'+LANG_MFCSSH_MATERIAL)
=gfModalGen('TRM38156B00000','ALERT',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MATCLR,loFormSet.GetHeaderText("LANG_MFCSSH_MATCLR",loFormSet.HeaderAlias))+': '+;
ALLTRIM(Item)+'|'+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MATERIAL,loFormSet.GetHeaderText("LANG_MFCSSH_MATERIAL",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

RETURN
ENDIF
CASE m.cCatgTyp='S'
IF !SEEK(m.Item,'Style')
*Message : 38156
*Style XXxX does not exist in the style file
*Button : 00000
*Ok
=gfModalGen('TRM38156B00000','ALERT',ALLTRIM(loFormSet.lcItmHdr)+': '+ALLTRIM(Item) +'|'+ ALLTRIM(loFormSet.lcMjrHdr))
RETURN
ENDIF
ENDCASE
m.cBomType = m.Typ
m.cTktNo   = m.CutTkt
m.cDyelot  = m.Dyelot
IF !lfOpenSql('BOMCOST','BOMCOST','BOMCSTKT',;
'CBOMTYPE+CIMTYP+CTKTNO+CINVTYPE+ITEM'+;
IIF(EMPTY(m.MfgCode),'','+MFGCODE')+IIF(EMPTY(m.Dyelot),'','+CDYELOT'),loFormSet)
RETURN .F.
ENDIF
IF m.cCatgTyp='S'
=gfOpenFile(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [BEGIN]
*=gfOpenFile(oAriaApplication.DataDir+'STYINVJL',oAriaApplication.DataDir+'STYINVJL','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYINVJL',oAriaApplication.DataDir+'STYINVJL','SH')
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [END]
SELECT STYINVJL
=AFIELDS(laFileStru)
=gfCrtTmp('TMPISSLOG',@laFileStru,"style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6)",'TMPISSLOG')
ENDIF
SELECT (loFormSet.lcTktSheet)

llItemDye = ((m.cCatgTyp='F' .OR. m.Trim_Invt)        .AND. loFormSet.laSetups[14,2]='Y' .AND.;
llFound                                  .AND. Fabric.cDye_Flg         ='Y') .OR.;
( m.cCatgTyp='S'                          .AND. loFormSet.laSetups[10,2]='Y' .AND.;
SEEK(m.Item,'Style')                     .AND. Style.cDye_Flg          ='Y')

STORE .F. TO llDispRoll,llDispAct,llDispUsed

PRIVATE loParentForm,laTotQty
loParentForm = loFormSet
DECLARE laTotQty[9]
STORE 0 TO laTotQty
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISLOG.SCX')
=gfCallForm('MFISLOG')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
IF USED(lcOldSheet)
USE IN (lcOldSheet)
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfShowLog
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/29/2004
*! Purpose   : Show Issue Log
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfShowLog()
*!*************************************************************
FUNCTION lfShowLog
LPARAMETERS loFormSet

loFormSet.Refresh
WITH loFormSet.AriaForm1
.cmdMaterialRolls.Enabled = EVALUATE(lcCondition)
.cmdReturns.Enabled       = EVALUATE(lcRetCond)
.cmdActualCost.Enabled    = EVALUATE(lcCondition)
ENDWITH

*!*************************************************************
*! Name      : lfvLotWare
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/31/2004
*! Purpose   : Get fabric open lots for specefic warehouse
*!*************************************************************
*! Calls     : lfRefresh,lfShowLot
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvLotWare()
*!*************************************************************
FUNCTION lfvLotWare
LPARAMETERS loFormSet

lcIssWare = IIF(lnIssWare=0,' ',loParentForm.laMatWare[lnIssWare,2])
m.cWareCode = lcIssWare
m.TotStk = lfGetTotStk(loParentForm)

SELECT (loParentForm.lcOpenLots)
SET KEY TO lcIssWare+ALLTRIM(m.Dyelot)
=SEEK(lcIssWare+ALLTRIM(m.Dyelot))
=lfShowLot(loFormSet)
loFormSet.Refresh

*!*************************************************************
*! Name      : lfvIssWare
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/21/2004
*! Purpose   : Validate fabric issued Warehouse
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvIssWare()
*!*************************************************************
FUNCTION lfvIssWare
LPARAMETERS loFormSet

LOCAL llFound
llFound = .F.
lcIssWare = loParentForm.laMatWare[lnIssWare,2]
m.Style = m.Item
m.cWareCode = lcIssWare
IF lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loParentForm)
SELECT FABRIC
LOCATE
llFound = !EOF()
ENDIF


*! B608558,1 MMT 05/15/2008 fix bug of wrong issue cost when fabric is added to location [Start]
*!*	IF llFound AND lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE',loParentForm)
*!*	  SELECT FABDYE
*!*	  LOCATE
*!*	  llFound = !EOF()
*!*	ENDIF
IF llFound
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE',loParentForm)
SELECT FABDYE
LOCATE
IF EOF()
IF gfModalGen('QRM38029B38001','ALERT',ALLTRIM(gfItemMask("HI","",m.cInvType))+': '+ALLTRIM(m.Item)+'|'+lcIssWare) = 1
=GfAdItemWar(m.cInvType,m.Item,SPACE(10),lcIssWare)
=lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE',loParentForm)
ELSE
loFormSet.ariaForm1.cboWarehouse.value  = loFormSet.ariaForm1.cboWarehouse.oldvalue
RETURN
ENDIF
ENDIF
ENDIF
SELECT FABDYE
LOCATE
ENDIF
*! B608558,1 MMT 05/15/2008 fix bug of wrong issue cost when fabric is added to location [End]


*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
m.nOnHand = FabDye.totStk
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]



*! B608366,1 MMT 12/13/2007 Fix bug of wrong Cost open open Actual cost screen [Start]
*IF !llFound
*! B608366,1 MMT 12/13/2007 Fix bug of wrong Cost open open Actual cost screen [End]

lnIssCost = IIF(loParentForm.laSetups[11,2]='A',FabDye.nAveCstBuy,Fabric.TotCost)/lfGetConv(FABRIC.cConvBuy)
loFormSet.Refresh

*! B608366,1 MMT 12/13/2007 Fix bug of wrong Cost open open Actual cost screen [Start]
*ENDIF
*! B608366,1 MMT 12/13/2007 Fix bug of wrong Cost open open Actual cost screen [End]

*!*************************************************************
*! Name      : lfvActCost
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Issue Cost Item
*!*************************************************************
*! Calls     : MFITMISS.SPX,MFSTYISS.SPX,MFMFGISS.SPX,lfShowLog
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvActCost()
*!*************************************************************
FUNCTION lfvActCost
LPARAMETERS loFormSet

PRIVATE lcIssWare,lnIssCost

SELECT (loParentForm.lcTktSheet)
SCATTER MEMVAR
DO CASE
CASE INLIST(m.cCatgTyp,'F','T') .AND. SEEK('S'+FABRIC.Scale,'Scale')

*! B608366,1 MMT 11/29/2007 Fix bug of wrong Cost open open Actual cost screen [Start]
IF INLIST(loParentForm.laSetups[11,2],"A","S")
=SEEK(BOMCOST.cinvtype+ BOMCOST.Item+ BOMCOST.cwarecode+ BOMCOST.cisession + "I"+ BOMCOST.ctktno,'MatInvJl')
ENDIF
*! B608366,1 MMT 11/29/2007 Fix bug of wrong Cost open open Actual cost screen [End]

lnIssWare   = CEILING(ASCAN(loParentForm.laMatWare,MatInvJl.cWareCode)/2)
lcIssWare   = MatInvJl.cWareCode
ldIssDate   = MatInvJl.dTrDate
llItemDye   = !EMPTY(MatInvJl.cDyelot)
lnIssCost   = MatInvJl.nUntCstBuy/lfGetConv(FABRIC.cConvBuy)
llWareStat  = .F.
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
STORE -EVALUATE('MATINVJL.nStk'+lcI) TO ('m.Iss_Qty'+lcI)
ENDFOR
m.Issue_Qty = -MATINVJL.nTotStk

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
m.Item = MATINVJL.Style
m.cWareCode = lcIssWare
m.Dyelot    = SPACE(10)
=lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loFormSet)
m.nOnHand = IIF(loParentForm.laSetups[2,2]='Y',FabDye.TotStk,FABRIC.TotStk)
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]


lnLotNo = IIF(EMPTY(m.cOprCode),1,ASCAN(laLots,MatInvJl.cLotNo))
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .T.,.T.
=gfCallForm('MFISSRET',.F.,".T.,.T.")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
CASE m.cCatgTyp='S' .AND. SEEK(m.Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale')
lnIssWare   = CEILING(ASCAN(loParentForm.laStyWare,TMPISSLOG.cWareCode)/2)
ldIssDate   = TMPISSLOG.dTrDate
llItemDye   = !EMPTY(TMPISSLOG.cDyelot)
lnIssCost   = TMPISSLOG.nCost
m.Iss_Qty1  = ABS(TMPISSLOG.nStk1)
m.Iss_Qty2  = ABS(TMPISSLOG.nStk2)
m.Iss_Qty3  = ABS(TMPISSLOG.nStk3)
m.Iss_Qty4  = ABS(TMPISSLOG.nStk4)
m.Iss_Qty5  = ABS(TMPISSLOG.nStk5)
m.Iss_Qty6  = ABS(TMPISSLOG.nStk6)
m.Iss_Qty7  = ABS(TMPISSLOG.nStk7)
m.Iss_Qty8  = ABS(TMPISSLOG.nStk8)
m.Issue_Qty = ABS(TMPISSLOG.nTotStk)

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
=SEEK(Style.Style+TMPISSLOG.cWareCode+SPACE(10),'StyDye')
m.nOnHand = IIF(loParentForm.laSetups[2,2]='Y',StyDye.TotStk,Style.TotStk)
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]


lnLotNo = IIF(EMPTY(m.cOprCode),1,ASCAN(laLots,TMPISSLOG.cLotNo))
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .T.,.T.
=gfCallForm('MFISSRET',.F.,".T.,.T.")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
loFormSet.AriaForm1.grdIsLog.RecordSource = ''
SELECT TMPISSLOG
ZAP

SELECT STYINVJL
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [BEGIN]
*SET ORDER TO TAG STYINVJL
*IF SEEK(m.Item)
gfSetOrder('STYINVJL')
IF GFSEEK(m.Item)
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [END]

SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = m.Item;
FOR cTrCode+cTrType $ m.CutTkt+'1'+','+m.CutTkt+'I'
SCATTER MEMO TO laIssLog
INSERT INTO TMPISSLOG FROM ARRAY laIssLog
ENDSCAN
ENDIF
SELECT TMPISSLOG
LOCATE

WITH loFormSet.AriaForm1.grdIsLog
.RecordSource = 'TMPISSLOG'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column1.ControlSource  = "PADR(IIF(TMPISSLOG.cIRType='I','"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
.Column1.ControlSource  = "PADR(IIF(TMPISSLOG.cIRType='I','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))+"','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURN,loFormSet.GetHeaderText("LANG_MFCSSH_RETURN",loFormSet.HeaderAlias))+"'),6)"
*N000682,1 11/20/2012 MMT Globlization changes[End]

.Column2.ControlSource  = "TMPISSLOG.cWareCode"
.Column2.Visible        = (loParentForm.laSetups[2,2]='Y')
.Column3.ControlSource  = "TMPISSLOG.dTrDate"
.Column4.ControlSource  = "TMPISSLOG.CSESSION"
.Column5.ControlSource  = "ABS(TMPISSLOG.nTotStk)"
.Column6.ControlSource  = "TMPISSLOG.nCost"
.Column7.Visible        = .F.
.Column8.ControlSource  = "gfCodDes(TMPISSLOG.cOprCode,'MFGCODE')"
.Column9.ControlSource  = "TMPISSLOG.cLotNo"
.Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
.Column10.ControlSource = "ABS(TMPISSLOG.nStk1)"
.Column11.ControlSource = "ABS(TMPISSLOG.nStk2)"
.Column12.ControlSource = "ABS(TMPISSLOG.nStk3)"
.Column13.ControlSource = "ABS(TMPISSLOG.nStk4)"
.Column14.ControlSource = "ABS(TMPISSLOG.nStk5)"
.Column15.ControlSource = "ABS(TMPISSLOG.nStk6)"
.Column16.ControlSource = "ABS(TMPISSLOG.nStk7)"
.Column17.ControlSource = "ABS(TMPISSLOG.nStk8)"
.Column18.Visible       = .F.
.Column19.Visible       = .F.
.Column20.Visible       = .F.
ENDWITH
OTHERWISE
llWareStat  = .F.
lnIssCost   = BomCost.nUnitCst
m.Issue_Qty = BomCost.nTotQty
ldIssDate   = BomCost.dTranDate
lnLotNo = IIF(EMPTY(m.cOprCode),1,ASCAN(laLots,BomCost.cLotNo))
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .T.,.T.
=gfCallForm('MFISSRET',.F.,".T.,.T.")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
ENDCASE
=lfShowLog(loFormSet)

*!*************************************************************
*! Name      : lfvIssue
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/31/2004
*! Purpose   : Issue Cost Item
*!*************************************************************
*! Calls     : MFOPLOT.SPX,MFITMISS.SPX,MFSTYISS.SPX,MFMFGISS.SPX,lfShowLog
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvIssue()
*!*************************************************************
FUNCTION lfvIssue
LPARAMETERS loFormSet

PRIVATE lcIssWare,lnIssCost
ldIssDate = oariaapplication.systemdate
SELECT (loParentForm.lcTktSheet)
SCATTER MEMVAR

*!*  *C200255,1 (Begin) Update MATINVJL with custom Voucher no for CON10.
*!*  IF INLIST(m.cCatgTyp,'F','T') .AND. SEEK(SUBSTR(m.Item,1,7)+m.IClr,'Fabric')
*!*    IF ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0
*!*      lcVoucNo = ""
*!*      llContVout = .T.
*!*      = gfDoTriger("MAPOREC",PADR("GETVOUT",10))
*!*      IF !llContVout
*!*        RETURN
*!*      ENDIF
*!*      llFrstTime = .F.
*!*    ENDIF
*!*  ENDIF
*!*  *C200255,1 (Begin)
DO CASE
*! N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
* CASE loParentForm.laSetups[11,2] = 'L' .AND. INLIST(m.cCatgTyp,'F','T')
CASE loParentForm.laSetups[11,2] = 'L' .AND. INLIST(m.cCatgTyp,'F','T') .AND. SEEK('S'+FABRIC.Scale,'Scale')
*! N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]
*--- DOC.
*--- Get the Open lots from materail inventory journal to issue cost element from it ,
*--  if the cost method is Lot
*--- DOC.
m.Style     = m.Item
m.cDyelot   = m.Dyelot
IF !lfOpenSql('ITEMJRNL','FABINV','STYINVJL','CINVTYPE+STYLE'+IIF(EMPTY(m.Dyelot),'','+CDYELOT'),loFormSet)
RETURN .F.
ENDIF
=lfSetIndex('FABINV','FABINV','cInvType+Style+cWareCode+cDyelot+cRSession')
LOCAL lnEngineBehavior
lnEngineBehavior = SET("EngineBehavior")
SET ENGINEBEHAVIOR 70
SELECT cInvType,Style,cWareCode,cDyelot,cRSession,cISession,nCost,cApInvNo,dTrDate,nRecCost,cTrCode,;
SUM(nStk1) AS 'nBal1',SUM(nStk2) AS 'nBal2',SUM(nStk3) AS 'nBal3',SUM(nStk4) AS 'nBal4',;
SUM(nStk5) AS 'nBal5',SUM(nStk6) AS 'nBal6',SUM(nStk7) AS 'nBal7',SUM(nStk8) AS 'nBal8',;
SUM(nTotStk) AS 'nBalance',nUntCstBuy,00000000.000 AS nToIssue1,00000000.000 AS nToIssue2,;
00000000.000 AS nToIssue3,00000000.000 AS nToIssue4,00000000.000 AS nToIssue5,00000000.000 AS nToIssue6,;
00000000.000 AS nToIssue7,00000000.000 AS nToIssue8,00000000.000 AS nToIssue FROM FABINV;
GROUP BY cInvType,Style,cWareCode,cDyelot,cRSession ;
HAVING nBalance > 0 INTO DBF (oAriaApplication.WorkDir+loParentForm.lcOpenLots)
IF _TALLY > 0
INDEX ON cWareCode+cDyelot TAG (loParentForm.lcOpenLots)
SET RELATION TO cInvType+Style+cWareCode+cDyelot+cRSession INTO FABINV
llItemDye = loParentForm.laSetups[14,2]='Y' .AND. Fabric.cDye_Flg = 'Y'
lcIssWare = EVALUATE(loParentForm.lcPosHdr+'.cMatWare')
lnIssWare = CEILING(ASCAN(loParentForm.laMatWare,lcIssWare)/2)
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFOPLOT.SCX') WITH .F.
=gfCallForm('MFOPLOT',.F.,".F.")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
SELECT (loParentForm.lcTktSheet)
SCATTER MEMVAR
ELSE
*E300725,1 Message : 38094
*E300725,1 No open lots found for fabric/color
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38094B00000','ALERT',ALLTRIM(gfItemMask("HI","",m.cInvType))+': '+ALLTRIM(m.Item))
ENDIF
SET ENGINEBEHAVIOR lnEngineBehavior
USE IN (loParentForm.lcOpenLots)
USE IN FABINV
loFormSet.AriaForm1.grdIsLog.RecordSource = ''
m.cTrType   = '9'
m.cTrCode   = m.CutTkt
m.cDyelot   = m.Dyelot
=lfOpenSql('ITEMJRNL','MATINVJL','STYINVJL','CINVTYPE+STYLE+CTRTYPE+CTRCODE'+;
IIF(EMPTY(m.Dyelot),'','+CDYELOT'),loFormSet)
WITH loFormSet.AriaForm1.grdIsLog
.RecordSource = 'MATINVJL'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column1.ControlSource  = "PADR(IIF(MATINVJL.cIRType='I','"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
.Column1.ControlSource  = "PADR(IIF(MATINVJL.cIRType='I','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))+"','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURN,loFormSet.GetHeaderText("LANG_MFCSSH_RETURN",loFormSet.HeaderAlias))+"'),6)"
*N000682,1 11/20/2012 MMT Globlization changes[End]

.Column2.ControlSource  = "MATINVJL.cWareCode"
.Column2.Visible        = (loParentForm.laSetups[2,2]='Y')
.Column3.ControlSource  = "MATINVJL.dTrDate"
.Column4.ControlSource  = "MATINVJL.CSESSION"
.Column5.ControlSource  = "ABS(MATINVJL.nTotStk)"
.Column6.ControlSource  = "MATINVJL.nCost"
.Column7.ControlSource  = "MATINVJL.cApInvNo"
.Column7.Visible        = (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)
.Column8.ControlSource  = "gfCodDes(MATINVJL.cOprCode,'MFGCODE')"
.Column9.ControlSource  = "MATINVJL.cLotNo"
.Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
.Column10.ControlSource = "ABS(MATINVJL.nStk1)"
.Column11.ControlSource = "ABS(MATINVJL.nStk2)"
.Column12.ControlSource = "ABS(MATINVJL.nStk3)"
.Column13.ControlSource = "ABS(MATINVJL.nStk4)"
.Column14.ControlSource = "ABS(MATINVJL.nStk5)"
.Column15.ControlSource = "ABS(MATINVJL.nStk6)"
.Column16.ControlSource = "ABS(MATINVJL.nStk7)"
.Column17.ControlSource = "ABS(MATINVJL.nStk8)"
.Column18.Visible       = .F.
.Column19.Visible       = .F.
.Column20.Visible       = .F.
ENDWITH
SELECT MATINVJL
CASE loParentForm.laSetups[11,2] <> 'L' .AND. INLIST(m.cCatgTyp,'F','T') .AND. SEEK('S'+FABRIC.Scale,'Scale')
*--- DOC.
*--- Get the on hand and the cost from fabric file
*--  if the cost method is not LOT
*--- DOC.
lcIssWare = EVALUATE(loParentForm.lcPosHdr+'.cMatWare')
lnIssWare = CEILING(ASCAN(loParentForm.laMatWare,lcIssWare)/2)

m.cWareCode = lcIssWare
m.Dyelot    = SPACE(10)
=lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loFormSet)
llItemDye = loParentForm.laSetups[14,2]='Y' .AND. Fabric.cDye_Flg = 'Y'
lnIssCost = IIF(loParentForm.laSetups[12,2]='S',FABRIC.TotCost/lfGetConv(FABRIC.cConvBuy),;
IIF(loParentForm.laSetups[2,2]='Y',FABDYE.Ave_Cost,FABRIC.Ave_Cost))
m.Iss_Qty1 = MAX(CEILING(m.Req_Qty1) - m.Used_Qty1,0)
m.Iss_Qty2 = MAX(CEILING(m.Req_Qty2) - m.Used_Qty2,0)
m.Iss_Qty3 = MAX(CEILING(m.Req_Qty3) - m.Used_Qty3,0)
m.Iss_Qty4 = MAX(CEILING(m.Req_Qty4) - m.Used_Qty4,0)
m.Iss_Qty5 = MAX(CEILING(m.Req_Qty5) - m.Used_Qty5,0)
m.Iss_Qty6 = MAX(CEILING(m.Req_Qty6) - m.Used_Qty6,0)
m.Iss_Qty7 = MAX(CEILING(m.Req_Qty7) - m.Used_Qty7,0)
m.Iss_Qty8 = MAX(CEILING(m.Req_Qty8) - m.Used_Qty8,0)
m.Issue_Qty= m.Iss_Qty1+m.Iss_Qty2+m.Iss_Qty3+m.Iss_Qty4+;
m.Iss_Qty5+m.Iss_Qty6+m.Iss_Qty7+m.Iss_Qty8
llWareStat  = .T.

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
m.nOnHand = IIF(loParentForm.laSetups[2,2]='Y',FABDYE.TotStk,Fabric.TotStk)
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]

*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .T.
=gfCallForm('MFISSRET',.F.,'.T.')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
CASE m.cCatgTyp='S' .AND. SEEK(m.Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale')
*--- DOC.
*--- Case style and style found in style file
*--- DOC.
lcIssWare = EVALUATE(loParentForm.lcPosHdr+'.cItemWare')
lnIssWare = CEILING(ASCAN(loParentForm.laStyWare,lcIssWare)/2)

=SEEK(m.Item+lcIssWare+SPACE(10),'StyDye')
llItemDye = loParentForm.laSetups[10,2]='Y' .AND. Style.cDye_Flg = 'Y'
lnIssCost = IIF(loParentForm.laSetups[12,2]='S',Style.TotCost,;
IIF(loParentForm.laSetups[2,2]='Y',StyDye.Ave_Cost,Style.Ave_Cost))
m.Iss_Qty1 = MAX(CEILING(m.Req_Qty1) - m.Used_Qty1,0)
m.Iss_Qty2 = MAX(CEILING(m.Req_Qty2) - m.Used_Qty2,0)
m.Iss_Qty3 = MAX(CEILING(m.Req_Qty3) - m.Used_Qty3,0)
m.Iss_Qty4 = MAX(CEILING(m.Req_Qty4) - m.Used_Qty4,0)
m.Iss_Qty5 = MAX(CEILING(m.Req_Qty5) - m.Used_Qty5,0)
m.Iss_Qty6 = MAX(CEILING(m.Req_Qty6) - m.Used_Qty6,0)
m.Iss_Qty7 = MAX(CEILING(m.Req_Qty7) - m.Used_Qty7,0)
m.Iss_Qty8 = MAX(CEILING(m.Req_Qty8) - m.Used_Qty8,0)
m.Issue_Qty= m.Iss_Qty1+m.Iss_Qty2+m.Iss_Qty3+m.Iss_Qty4+;
m.Iss_Qty5+m.Iss_Qty6+m.Iss_Qty7+m.Iss_Qty8

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
m.nOnHand = IIF(loParentForm.laSetups[2,2]='Y',StyDye.TotStk,Style.TotStk)
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]

*T20060818.0001(C200876) TMI [Start] Call Issue Screen with modification of Bin Location
IF ASCAN(loParentForm.laEvntTrig , PADR('LDFNPOIS',10)) <> 0 .AND. loParentForm.mDoTrigger(PADR('ISUSEBIN',10))
=loParentForm.mDoTrigger(PADR('LDFNPOIS',10))
ELSE
*T20060818.0001(C200876) TMI [End  ]
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .T.
=gfCallForm('MFISSRET',.F.,'.T.')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*T20060818.0001(C200876) TMI [Start] Close the above If
ENDIF
*T20060818.0001(C200876) TMI [End  ]

loFormSet.AriaForm1.grdIsLog.RecordSource = ''
SELECT TMPISSLOG
ZAP

SELECT STYINVJL

*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [BEGIN]
*SET ORDER TO TAG STYINVJL
*IF SEEK(m.Item)
gfSetOrder('STYINVJL')
IF GFSEEK(m.Item)
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [END]

SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = m.Item;
FOR cTrCode+cTrType $ m.CutTkt+'1'+','+m.CutTkt+'I'
SCATTER MEMO TO laIssLog
INSERT INTO TMPISSLOG FROM ARRAY laIssLog
ENDSCAN
ENDIF
SELECT TMPISSLOG
LOCATE

WITH loFormSet.AriaForm1.grdIsLog
.RecordSource = 'TMPISSLOG'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column1.ControlSource  = "PADR(IIF(TMPISSLOG.cIRType='I','"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
.Column1.ControlSource  = "PADR(IIF(TMPISSLOG.cIRType='I','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))+"','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURN,loFormSet.GetHeaderText("LANG_MFCSSH_RETURN",loFormSet.HeaderAlias))+"'),6)"
*N000682,1 11/20/2012 MMT Globlization changes[End]

.Column2.ControlSource  = "TMPISSLOG.cWareCode"
.Column2.Visible        = (loParentForm.laSetups[2,2]='Y')
.Column3.ControlSource  = "TMPISSLOG.dTrDate"
.Column4.ControlSource  = "TMPISSLOG.CSESSION"
.Column5.ControlSource  = "ABS(TMPISSLOG.nTotStk)"
.Column6.ControlSource  = "TMPISSLOG.nCost"
.Column7.Visible        = .F.
.Column8.ControlSource  = "gfCodDes(TMPISSLOG.cOprCode,'MFGCODE')"
.Column9.ControlSource  = "TMPISSLOG.cLotNo"
.Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
.Column10.ControlSource = "ABS(TMPISSLOG.nStk1)"
.Column11.ControlSource = "ABS(TMPISSLOG.nStk2)"
.Column12.ControlSource = "ABS(TMPISSLOG.nStk3)"
.Column13.ControlSource = "ABS(TMPISSLOG.nStk4)"
.Column14.ControlSource = "ABS(TMPISSLOG.nStk5)"
.Column15.ControlSource = "ABS(TMPISSLOG.nStk6)"
.Column16.ControlSource = "ABS(TMPISSLOG.nStk7)"
.Column17.ControlSource = "ABS(TMPISSLOG.nStk8)"
.Column18.Visible       = .F.
.Column19.Visible       = .F.
.Column20.Visible       = .F.
ENDWITH
m.TotStk = lfGetTotStk(loParentForm)
SELECT (loParentForm.lcTktSheet)
GATHER FIELDS TotStk MEMVAR
CASE INLIST(m.cCatgTyp,'M','P','D')
PRIVATE lcMfgGlAcnt
lcMfgGlAcnt = loParentForm.lcMfgGlAcnt
LOCAL ARRAY laMfgRFld[7,2]
=ACOPY(loParentForm.laMfgRFld,laMfgRFld)
IF !(OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0) .OR. ;
(m.cCatGTyp = 'M' .AND. gfRltFld(m.MfgCode,@laMfgRFld,'MFGCODE') ;
.AND. !EMPTY(lcMfgGlAcnt))
*--- DOC.
*--- Case mfg operation , purchase price or duty
*--- and AP module is not installed.
*--- if the AP module installed issue this operation cost from AP module
*--- DOC.
lnIssCost   = m.UntCost
m.Issue_Qty = MAX(CEILING(m.Req_Qty)-m.Used_Qty,0)
llWareStat = .F.
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .T.
=gfCallForm('MFISSRET',.F.,'.T.')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
ELSE
IF m.cCatGTyp $ 'PD'
*E300725,1 Message : 38091
*E300725,1 You cannot issue the duty cost items since you are linked
*E300725,1 with the AP. The duty cost application is done from the AP module.
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38091B00000','ALERT',IIF(m.cCatGTyp='D',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))+'|'+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DUTY,loFormSet.GetHeaderText("LANG_MFCSSH_DUTY",loFormSet.HeaderAlias))+'|'+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DUTY,loFormSet.GetHeaderText("LANG_MFCSSH_DUTY",loFormSet.HeaderAlias)),;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))+;
'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PURCHASE,loFormSet.GetHeaderText("LANG_MFCSSH_PURCHASE",loFormSet.HeaderAlias))+'|'+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PURCHASE,loFormSet.GetHeaderText("LANG_MFCSSH_PURCHASE",loFormSet.HeaderAlias))))
ELSE
*E300725,1 Message : 38092
*E300725,1 This MFG code has been setup to be applied from the A/P.
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38092B00000','ALERT')
ENDIF
ENDIF
ENDCASE
=lfShowLog(loFormSet)

*!*************************************************************
*! Name      : lfShowLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/31/2004
*! Purpose   : Show fabric/colors open lots
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShowLot()
*!*************************************************************
FUNCTION lfShowLot
LPARAMETERS loFormSet

IF EVALUATE(loParentForm.lcOpenLots+'.nBalance') <= 0
loFormSet.AriaForm1.cmdIssue.Enabled = .F.
ELSE
loFormSet.AriaForm1.cmdIssue.Enabled = .T.
ENDIF

*!*************************************************************
*! Name      : lfvLotToIs
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/31/2004
*! Purpose   : Valid function of Issue column in MFOPLOT screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLotToIs()
*!*************************************************************
FUNCTION lfvLotToIs
LPARAMETERS loThis

LOCAL lnOldIssue,lcI
lcI = RIGHT(loThis.ControlSource,1)
SELECT (loParentForm.lcOpenLots)
lnOldIssue = nToIssue
REPLACE nToIssue WITH nToIssue + loThis.Value - loThis.OldValue
IF nToIssue > nBalance OR (EVALUATE(loThis.ControlSource) > EVALUATE('nBal'+lcI))
*E300725,1 Message : 38086
*E300725,1 Quantity cannot exceed lot open quantity.
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38086B00000','ALERT')
REPLACE nToIssue WITH lnOldIssue
REPLACE (loThis.ControlSource) WITH loThis.OldValue
RETURN
ENDIF
IF m.Used_Qty + lnOldIssue - nToIssue < 0
*E300725,1 Message : 38087
*E300725,1 Contributed quantity cannot exceed total issued quantity.
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38087B00000','ALERT')
REPLACE nToIssue WITH lnOldIssue
REPLACE (loThis.ControlSource) WITH loThis.OldValue
RETURN
ENDIF
m.Used_Qty = m.Used_Qty + lnOldIssue - nToIssue

*!*************************************************************
*! Name      : lfvIssLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/01/2004
*! Purpose   : Issue fabric/color from open lot
*!*************************************************************
*! Calls     : MFITMISS.SPX,lfvItmIss,lfvBrowLots
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvIssLot()
*!*************************************************************
FUNCTION lfvIssLot
LPARAMETERS llAuto,loFormSet

*--- DOC.
*--- Issue fabric/color by callng function [lfIssRetFab]
*--- DOC.
IF llAuto
*E300725,1 Message : 38088
*E300725,1 Contributed quantity does not total the issued quantity.
*E300725,1 Would you like to modify the issued quantity?
*E300725,1 Button : 38010
*E300725,1 Proceed  Resume
IF m.Used_Qty = 0 .OR. gfModalGen('QRM38088B38010','ALERT')=1
SELECT (loParentForm.lcOpenLots)
SCAN FOR nToIssue > 0
LOCAL ARRAY laToIssue[9]
LOCAL lnI,lcI
FOR lnI = 1 TO 9
lcI = STR(lnI,MOD(INT(lnI/9)+1,2))
laToIssue[lnI] = EVALUATE('nToIssue'+lcI)
ENDFOR

=lfIssRetFab(EVALUATE(loParentForm.lcIssLtFile+'.Typ'),EVALUATE(loParentForm.lcIssLtFile+'.cCatgTyp'),;
cInvType,Style,cWareCode,cDyelot,@laToIssue,nUntCstBuy,loFormSet.lcOprCode,;
loFormSet.lcLotNo,ldIssDate,cRSession,gfsequence('GLSession'))
ENDSCAN
loFormset.Release
ENDIF
RETURN
ENDIF

ldIssDate   = oariaapplication.systemdate
llWareStat  = .F.
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
m.Iss_Qty&lcI = MIN(MAX(EVALUATE('m.Req_Qty'+lcI)-EVALUATE('m.Used_Qty'+lcI),0),EVALUATE(loParentForm.lcOpenLots+'.nBal'+lcI))
ENDFOR

m.Issue_Qty = MIN(MAX(m.Req_Qty-m.Used_Qty,0),EVALUATE(loParentForm.lcOpenLots+'.nBalance'))
lnIssCost   = EVALUATE(loParentForm.lcOpenLots+'.nUntCstBuy')/lfGetConv(Fabric.cConvBuy)
m.Dyelot    = EVALUATE(loParentForm.lcOpenLots+'.cDyelot')
lnCurObj    = 0
*--- DOC.
*--- this part if you are issue fabric/color but not automatic issu
*--- DOC.

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
lcOldAlias = SELECT()
m.Item = EVALUATE(loParentForm.lcOpenLots+'.Style')
m.cWareCode = EVALUATE(loParentForm.lcOpenLots+'.cWareCode')
m.Dyelot    = SPACE(10)
=lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loFormSet)
m.nOnHand = IIF(loParentForm.laSetups[2,2]='Y',FabDye.TotStk,FABRIC.TotStk)
SELECT (lcOldAlias)
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]

*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .T.
=gfCallForm('MFISSRET',.F.,'.T.')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
=lfvLotWare(loFormSet)

*!*************************************************************
*! Name      : lfvMfgIss
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/22/2004
*! Purpose   : Accept issue/Return MFG
*!*************************************************************
*! Calls     : gfModalGen,CHECKPRD,lfIssRetMfg
*!*************************************************************
*! Parameters: llIssue   : .T.  Issue
*!                         .F.  Return
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvMfgIss()
*!*************************************************************
FUNCTION lfvMfgIss
PARAMETERS llIssue,llActCst,loFormSet
*--- DOC.
*--- Call the function "lfIssRetMfg" to issue or return the MFG operation
*--- DOC.
IF llActCst
=lfIssRetMfg(m.Typ,m.cCatgTyp,m.cInvType,m.Item,m.MfgCode,BomCost.cWareCode,;
m.Dyelot,-m.Issue_Qty,BomCost.nUnitCst,ldIssDate,m.cOprCode,;
BomCost.cLotNo,gfsequence('GLSession'),SPACE(6))
=lfIssRetMfg(m.Typ,m.cCatgTyp,m.cInvType,m.Item,m.MfgCode,BomCost.cWareCode,;
m.Dyelot,m.Issue_Qty,lnIssCost,ldIssDate,m.cOprCode,;
BomCost.cLotNo,SPACE(6),gfsequence('GLSession'))
RETURN
ENDIF

IF !llIssue .AND. m.Issue_Qty > m.Used_Qty
*E300725,1 Message : 38057
*E300725,1 Return quantity cannot exceed issued quantity
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38057B00000','ALERT')
m.Issue_Qty = m.Used_Qty
loFormset.AriaForm1.txtIssueQuantity.SetFocus
RETURN .F.
ENDIF

lcGlYear   = loParentForm.lcGlYear
lcGlPeriod = loParentForm.lcGlPeriod
IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
loFormset.AriaForm1.DtpickerIssueDate.SetFocus
RETURN .F.
ENDIF
loParentForm.lcGlYear   = lcGlYear
loParentForm.lcGlPeriod = lcGlPeriod

=lfIssRetMfg(m.Typ,m.cCatgTyp,m.cInvType,m.Item,m.MfgCode,m.cWareCode,;
m.Dyelot,IIF(llIssue,m.Issue_Qty,-m.Issue_Qty),lnIssCost,;
ldIssDate,m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),;
IIF(llIssue,SPACE(6),gfsequence('GLSession')),;
IIF(llIssue,gfsequence('GLSession'),SPACE(6)))
m.Used_Qty = m.Used_Qty + IIF(llIssue,m.Issue_Qty,-m.Issue_Qty)

*!*************************************************************
*! Name      : lfvItmIss
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/21/2004
*! Purpose   : Accept issue/Return Fabric/Color
*!*************************************************************
*! Calls     : gfModalGen,CHECKPRD,lfIssRetFab,gpAdFabWar
*!*************************************************************
*! Parameters: llIssue   : .T.  Issue
*!                         .F.  Return
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvItmIss()
*!*************************************************************
FUNCTION lfvItmIss
PARAMETERS llIssue,llActCst,llEditUsed,loFormSet

LOCAL llFound,lnI,lcI
llFound = .F.
m.Style = m.Item
m.cWareCode = lcIssWare
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE',loParentForm)
SELECT FABDYE
LOCATE
llFound = !EOF()
ENDIF

IF loParentForm.laSetups[2,2]='Y' .AND. !llFound
*E300725,1 Message : 38029
*E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
*E300725,1 Button : 38001
*E300725,1 Add Cancel
*--- DOC.
*--- Check if the FAbric/Color is assigned to the warehouse
*--- DOC.
IF gfModalGen('QRM38029B38001','ALERT',ALLTRIM(gfItemMask("HI","",m.cInvType))+': '+ALLTRIM(m.Item)+'|'+lcIssWare) = 1
=GfAdItemWar(m.cInvType,m.Item,SPACE(10),lcIssWare)
ELSE
RETURN
ENDIF
ENDIF

llFound = .F.
IF lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loParentForm)
SELECT FABRIC
LOCATE
llFound = !EOF()
ENDIF

IF loParentForm.laSetups[14,2]='Y' AND llFound AND Fabric.cDye_Flg='Y'
lcOldDye = m.Dyelot
lcDyelot = IIF(EMPTY(m.Dyelot),'?',m.Dyelot)
m.Dyelot = lcDyelot
llFound = .F.
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loParentForm)
SELECT FABDYE
LOCATE
llFound = !EOF()
ENDIF
IF !llFound
=gfItmDyBrw(m.cInvType,m.Item,@lcDyelot,.T.,lcIssWare)
IF EMPTY(lcDyelot)
m.Dyelot = lcOldDye
RETURN
ENDIF
ENDIF
m.Dyelot = lcDyelot
SELECT (loParentForm.lcTktSheet)
REPLACE Dyelot WITH m.Dyelot
*B610572,1 TMI 10/31/2013 19:20 [Start] the property is lCTktBom, not CTktBom
*SELECT (loParentForm.CTktBom)
SELECT (loParentForm.lcCTKTBOM)
*B610572,1 [T20131031.0009 Task] TMI 10/31/2013 19:20 [End  ]
LOCATE FOR Typ+cInvType+Item+MfgCode+Dyelot=m.typ+m.cInvType+m.item+m.mfgcode+IIF(!EMPTY(m.Dyelot),m.Dyelot,'')
IF !FOUND()
LOCATE FOR Typ+cInvType+Item+MfgCode+Dyelot=m.typ+m.cInvType+m.item+m.mfgcode+SPACE(10)
ENDIF
REPLACE Dyelot WITH m.Dyelot

*B610572,1 TMI 11/03/2013 09:22 [Start] select the temp file lcBOMLIN, remove the reference to ICLR
*IF SEEK(m.cimtyp+"1"+m.cuttkt,'BomLine')
*  SELECT BomLine
*  LOCATE REST WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style+cInvTypC+Item+IClr+MfgCode=;
*                    m.cIMTyp+"1"+m.CutTkt;
*              FOR cInvTypC+Item+MfgCode = m.cInvType+m.Item+m.MfgCode AND Dyelot = m.Dyelot
IF SEEK(m.cimtyp+"1"+m.cuttkt,loParentForm.lcBomLine)
SELECT (loParentForm.lcBomLine)
LOCATE REST WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style+cInvTypC+Item+MfgCode=;
m.cIMTyp+"1"+m.CutTkt;
FOR cInvTypC+Item+MfgCode = m.cInvType+m.Item+m.MfgCode AND Dyelot = m.Dyelot
*B610572,1 TMI 11/03/2013 09:50 [End  ]
IF !FOUND()
=SEEK(m.cimtyp+"1"+m.cuttkt)
*B610572,1 TMI 11/03/2013 09:51 [Start] remove the reference to ICLR
*LOCATE REST WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style+cInvTypC+Item+IClr+MfgCode=;
*                  m.cIMTyp+"1"+m.CutTkt;
*            FOR cInvTypC+Item+MfgCode = m.cInvTypC+m.Item+m.IClr+m.MfgCode
LOCATE REST WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style+cInvTypC+Item+MfgCode=;
m.cIMTyp+"1"+m.CutTkt;
FOR cInvTypC+Item+MfgCode = m.cInvTypC+m.Item+m.MfgCode
*B610572,1 TMI 11/03/2013 09:51 [End  ]
IF FOUND()
REPLACE REST Dyelot WITH m.Dyelot ;
WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style+cInvTypC+Item+MfgCode=;
m.cIMTyp;
FOR cTktNo = m.CutTkt AND cInvTypC+Item+MfgCode = m.cInvTypC+m.Item+m.MfgCode
ENDIF
ENDIF
ENDIF
ENDIF

IF llActCst
= lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loParentForm)
*--- DOC.
*--- Call functoin to issue / return fabri/color
*--- DOC.
llFrstTime = .F.
LOCAL ARRAY laToIssue[9]
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
laToIssue[lnI] = -EVALUATE('m.Iss_Qty'+lcI)
ENDFOR
laToIssue[9] = -m.Issue_Qty

=lfIssRetFab(m.Typ,m.cCatgTyp,m.cInvType,m.Item,MatInvJl.cWareCode,m.Dyelot,@laToIssue,MatInvJl.nUntCstBuy,m.cOprCode,;
MatInvJl.cLotNo,ldIssDate,gfsequence('GLSession'),SPACE(6))

FOR lnI = 1 TO 8
lcI = STR(lnI,1)
laToIssue[lnI] = EVALUATE('m.Iss_Qty'+lcI)
ENDFOR
laToIssue[9] = m.Issue_Qty

=lfIssRetFab(m.Typ,m.cCatgTyp,m.cInvType,m.Item,MatInvJl.cWareCode,m.Dyelot,@laToIssue,;
lnIssCost*lfGetConv(FABRIC.cConvBuy),m.cOprCode,MatInvJl.cLotNo,ldIssDate,SPACE(6),;
gfsequence('GLSession'),.F.,.T.)
llFrstTime = .F.
RETURN
ENDIF
= lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE',loParentForm)
= lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loParentForm)

IF llEditUsed
LOCAL llChange
llChange = .F.
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
IF EVALUATE('m.Used_Qty'+lcI) <> laTotQty[lnI]
llChange = .T.
EXIT
ENDIF
ENDFOR

IF llChange
lnIssueQty  = m.Issue_Qty
m.Issue_Qty = m.Used_Qty - laTotQty[9]
IF m.Used_Qty >= laTotQty[9]
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
IF INLIST(loParentForm.laSetups[11,2],'F','I') .AND. EVALUATE('m.Iss_Qty'+lcI) > EVALUATE('FabDye.Stk'+lcI)
*--- Message : 38064
*--- Issued quantity cannot exceed lot open quantity
*--- Button : 00000
*--- Ok
=gfModalGen('TRM38064B00000','ALERT')
m.Used_Qty&lcI = MIN(EVALUATE('m.Used_Qty'+lcI),EVALUATE('FabDye.Stk'+lcI))
m.Used_Qty = MIN(m.Used_Qty,FabDye.TotStk)
m.Issue_Qty = lnIssueQty
loFormset.AriaForm1.cntIssue_Qty.txtQty&lcI..SetFocus
RETURN .F.
ENDIF
ENDFOR

lcGlYear   = loParentForm.lcGlYear
lcGlPeriod = loParentForm.lcGlPeriod
IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
m.Issue_Qty = lnIssueQty
loFormset.AriaForm1.DtpickerIssueDate.SetFocus
RETURN .F.
ENDIF
loParentForm.lcGlYear   = lcGlYear
loParentForm.lcGlPeriod = lcGlPeriod

IF INLIST(loParentForm.laSetups[11,2],'F','I')
*--- DOC.
*--- Function to Issue Fabrics and Inventory mantained trims FIFO/LIFO
*--- DOC.

LOCAL ARRAY laToIssue[9]
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
laToIssue[lnI] = EVALUATE('m.Iss_Qty'+lcI)
ENDFOR
laToIssue[9] = m.Issue_Qty

=lfIssSeq(loParentForm.laSetups[11,2],m.Typ,m.cCatgTyp,m.cInvType,m.Item,;
lcIssWare,m.Dyelot,@laToIssue,m.cOprCode,;
IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate)
ELSE
LOCAL ARRAY laToIssue[9]
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
*B609089,1 WAM 11/16/2009 Fix update used quanity when use the option "Edit Used"
*laToIssue[lnI] = EVALUATE('m.Iss_Qty'+lcI)
laToIssue[lnI] = EVALUATE('m.Used_Qty'+lcI) - laTotQty[lnI]
*B609089,1 WAM 11/16/2009 (End)
ENDFOR
laToIssue[9] = m.Issue_Qty

=lfIssRetFab(m.Typ,m.cCatgTyp,m.cInvType,m.Item,lcIssWare,m.Dyelot,@laToIssue,;
lnIssCost*lfGetConv(FABRIC.cConvBuy),m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),;
ldIssDate,IIF(loParentForm.laSetups[11,2]='L',EVALUATE(lcOpenLots+'.cRSession'),SPACE(6)),;
gfsequence('GLSession'))
ENDIF
m.Issue_Qty = m.Issue_Qty + lnIssueQty
ELSE
lcGlYear   = loParentForm.lcGlYear
lcGlPeriod = loParentForm.lcGlPeriod
IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
m.Issue_Qty = lnIssueQty
loFormset.AriaForm1.DtpickerIssueDate.SetFocus
RETURN .F.
ENDIF
loParentForm.lcGlYear   = lcGlYear
loParentForm.lcGlPeriod = lcGlPeriod

LOCAL ARRAY laToIssue[9]
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
*B609089,1 WAM 11/16/2009 Fix update used quanity when use the option "Edit Used"
*laToIssue[lnI] = EVALUATE('m.Iss_Qty'+lcI)
laToIssue[lnI] = EVALUATE('m.Used_Qty'+lcI) - laTotQty[lnI]
*B609089,1 WAM 11/16/2009 (End)
ENDFOR
laToIssue[9] = m.Issue_Qty

=lfIssRetFab(m.Typ,m.cCatgTyp,m.cInvType,m.Item,lcIssWare,m.Dyelot,@laToIssue,;
lnIssCost*lfGetConv(FABRIC.cConvBuy),m.cOprCode,m.cLotNo,ldIssDate,;
IIF(INLIST(loParentForm.laSetups[11,2],'L','F','I'),BomCost.cRSession,;
gfsequence('GLSession')),IIF(INLIST(loParentForm.laSetups[11,2],'L','F','I'),;
BomCost.cISession,IIF(FABDYE.TotStk<0,MATINVJL.CISESSION,SPACE(6))))
m.Issue_Qty = lnIssueQty
ENDIF
ENDIF
RETURN
ENDIF

IF llIssue
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
IF INLIST(loParentForm.laSetups[11,2],'F','I') .AND. EVALUATE('m.Iss_Qty'+lcI) > EVALUATE('FabDye.Stk'+lcI)
*E300725,1 Message : 38064
*E300725,1 Issued quantity cannot exceed lot open quantity
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38064B00000','ALERT')
m.Iss_Qty&lcI = MIN(MAX(EVALUATE('m.Req_Qty'+lcI)-EVALUATE('m.Used_Qty'+lcI),0),EVALUATE('FabDye.Stk'+lcI))
loFormset.AriaForm1.cntIssue_Qty.txtQty&lcI..SetFocus
RETURN .F.
ENDIF
IF loParentForm.laSetups[11,2] = 'L' .AND. EVALUATE('m.Iss_Qty'+lcI) > EVALUATE(loParentForm.lcOpenLots+'.nBal'+lcI)
*E300725,1 Message : 38064
*E300725,1 Issued quantity cannot exceed lot open quantity
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38064B00000','ALERT')
m.Iss_Qty&lcI = MIN(MAX(EVALUATE('m.Req_Qty'+lcI)-EVALUATE('m.Used_Qty'+lcI),0),EVALUATE(loParentForm.lcOpenLots+'.nBal'+lcI))
loFormset.AriaForm1.cntIssue_Qty.txtQty&lcI..SetFocus
RETURN .F.
ENDIF
ENDFOR

lcGlYear   = loParentForm.lcGlYear
lcGlPeriod = loParentForm.lcGlPeriod
IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
loFormset.AriaForm1.DtpickerIssueDate.SetFocus
RETURN .F.
ENDIF
loParentForm.lcGlYear   = lcGlYear
loParentForm.lcGlPeriod = lcGlPeriod
IF INLIST(loParentForm.laSetups[11,2],'F','I')
*--- DOC.
*--- Function to Issue Fabrics and Inventory mantained trims FIFO/LIFO
*--- DOC.

LOCAL ARRAY laToIssue[9]
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
laToIssue[lnI] = EVALUATE('m.Iss_Qty'+lcI)
ENDFOR
laToIssue[9] = m.Issue_Qty

=lfIssSeq(loParentForm.laSetups[11,2],m.Typ,m.cCatgTyp,m.cInvType,m.Item,;
lcIssWare,m.Dyelot,@laToIssue,m.cOprCode,;
IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate)
ELSE
LOCAL ARRAY laToIssue[9]
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
laToIssue[lnI] = EVALUATE('m.Iss_Qty'+lcI)
ENDFOR
laToIssue[9] = m.Issue_Qty

=lfIssRetFab(m.Typ,m.cCatgTyp,m.cInvType,m.Item,lcIssWare,m.Dyelot,@laToIssue,lnIssCost*lfGetConv(FABRIC.cConvBuy),;
m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
IIF(loParentForm.laSetups[11,2]='L',EVALUATE(loParentForm.lcOpenLots+'.cRSession'),SPACE(6)),;
gfsequence('GLSession'))
ENDIF
ELSE
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
IF EVALUATE('m.Iss_Qty'+lcI) > laTotQty[lnI]
*E300725,1 Message : 38057
*E300725,1 Return quantity cannot exceed issued quantity
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38057B00000','ALERT')
m.Iss_Qty&lcI. = laTotQty[lnI]
m.Issue_Qty    = laTotQty[9]
loFormset.AriaForm1.cntIssue_Qty.txtQty&lcI..SetFocus
RETURN .F.
ENDIF
ENDFOR

lcGlYear   = loParentForm.lcGlYear
lcGlPeriod = loParentForm.lcGlPeriod
IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
loFormset.AriaForm1.DtpickerIssueDate.SetFocus
RETURN .F.
ENDIF
loParentForm.lcGlYear   = lcGlYear
loParentForm.lcGlPeriod = lcGlPeriod

LOCAL ARRAY laToIssue[9]
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
laToIssue[lnI] = -EVALUATE('m.Iss_Qty'+lcI)
ENDFOR
laToIssue[9] = -m.Issue_Qty

=lfIssRetFab(m.Typ,m.cCatgTyp,m.cInvType,m.Item,lcIssWare,m.Dyelot,@laToIssue,lnIssCost*lfGetConv(FABRIC.cConvBuy),;
m.cOprCode,m.cLotNo,ldIssDate,IIF(INLIST(loParentForm.laSetups[11,2],'L','F','I'),;
BomCost.cRSession,gfsequence('GLSession')),IIF(INLIST(loParentForm.laSetups[11,2],'L','F','I'),;
BomCost.cISession,IIF(FABDYE.TotStk<0,MATINVJL.CISESSION,SPACE(6))))
ENDIF
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
m.Used_Qty&lcI. = EVALUATE('m.Used_Qty'+lcI) + (IIF(llIssue,1,-1) * EVALUATE('m.Iss_Qty'+lcI))
ENDFOR
m.Used_Qty = m.Used_Qty + IIF(llIssue,m.Issue_Qty,-m.Issue_Qty)
*!*************************************************************
*! Name      : lfvStyIss
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/22/2004
*! Purpose   : Accept issue/Return Styles
*!*************************************************************
*! Calls     : gfModalGen,CHECKPRD,lfIssRetSty,gpAdStyWar
*!*************************************************************
*! Parameters: llIssue   : .T.  Issue
*!                         .F.  Return
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvStyIss()
*!*************************************************************
FUNCTION lfvStyIss
PARAMETERS llIssue,llActCst,loFormSet
LOCAL ARRAY laIssQty[9]

*T20060818.0001(C200876) TMI [Start] validate bin location
IF ASCAN(loParentForm.laEvntTrig , PADR('DLCHKBIN',10)) <> 0 .AND. loParentForm.mDoTrigger(PADR('ISUSEBIN',10)) .AND. ;
!loParentForm.mDoTrigger(PADR('DLCHKBIN',10))
RETURN .F.
ENDIF
*T20060818.0001(C200876) TMI [End  ]

IF llActCst
laIssQty[1] = m.Iss_Qty1
laIssQty[2] = m.Iss_Qty2
laIssQty[3] = m.Iss_Qty3
laIssQty[4] = m.Iss_Qty4
laIssQty[5] = m.Iss_Qty5
laIssQty[6] = m.Iss_Qty6
laIssQty[7] = m.Iss_Qty7
laIssQty[8] = m.Iss_Qty8
laIssQty[9] = m.Issue_Qty
*--- DOC.
*--- Function to issue/return syle by calling style control
*--- DOC.

=lfIssRetSty(m.Typ,m.cCatgTyp,m.cInvType,m.Item,TMPISSLOG.cWareCode,m.Dyelot,@laIssQty,;
TMPISSLOG.nCost,m.cOprCode,TMPISSLOG.cLotNo,ldIssDate,loParentForm.lcSession,SPACE(6))
laIssQty[1] = -m.Iss_Qty1
laIssQty[2] = -m.Iss_Qty2
laIssQty[3] = -m.Iss_Qty3
laIssQty[4] = -m.Iss_Qty4
laIssQty[5] = -m.Iss_Qty5
laIssQty[6] = -m.Iss_Qty6
laIssQty[7] = -m.Iss_Qty7
laIssQty[8] = -m.Iss_Qty8
laIssQty[9] = -m.Issue_Qty
=lfIssRetSty(m.Typ,m.cCatgTyp,m.cInvType,m.Item,TMPISSLOG.cWareCode,m.Dyelot,@laIssQty,;
lnIssCost,m.cOprCode,TMPISSLOG.cLotNo,ldIssDate,SPACE(6),loParentForm.lcSession)
RETURN
ENDIF
IF loParentForm.laSetups[2,2]='Y' .AND. ;
!SEEK (m.Item+lcIssWare+SPACE(10),'STYDYE')
*E300725,1 Message : 38029
*E300725,1 Style xxxxx is not assigned to warehouse xxxx
*E300725,1 Button : 38001
*E300725,1 Add Cancel
*--- DOC.
*--- Check if the current style is not assigned to the transactoin warehouse
*--- DOC.
IF gfModalGen('QRM38029B38001','ALERT',ALLTRIM(gfItemMask("HI","",m.cInvType))+': '+ALLTRIM(m.Item)+'|'+lcIssWare) = 1
DO gpAdStyWar WITH m.Item,SPACE(10),lcIssWare
ELSE
RETURN .F.
ENDIF
ENDIF
IF loParentForm.laSetups[10,2]='Y' AND SEEK(m.Item,'Style') AND Style.cDye_Flg='Y'
lcDyelot = IIF(EMPTY(m.Dyelot),'?',m.Dyelot)
IF !SEEK(m.Item+lcIssWare+lcDyelot,'STYDYE') AND ;
!SDYEBROW(m.Item, @lcDyelot, .T., lcIssWare,.F.,.F.,.F.,.F.,loParentForm.llStyConfg)
RETURN .F.
ENDIF
m.Dyelot = lcDyelot
SELECT (loParentForm.lcTktSheet)
REPLACE Dyelot WITH m.Dyelot
SELECT (loParentForm.lcCTktBom)
LOCATE FOR Typ+cInvType+Item+MfgCode = m.typ+m.cInvType+m.item+m.mfgcode
IF FOUND()
LOCAL ARRAY laCtktBomRec[1]
SCATTER TO laCtktbomRec
DELETE
APPEND BLANK
GATHER FROM laCtktBomRec
REPLACE Dyelot WITH m.Dyelot
ENDIF
ENDIF
IF !llIssue
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
IF EVALUATE('m.Iss_Qty'+lcCount) > EVALUATE('m.Used_qty'+lcCount)
*E300725,1 Message : 38057
*E300725,1 Return quantity cannot exceed issued quantity
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38057B00000','ALERT')
m.Iss_Qty&lcCount = EVALUATE('m.Used_qty'+lcCount)
loFormset.AriaForm1.cntIssue_Qty.txtQty&lcCount..SetFocus
RETURN .F.
ENDIF
ENDFOR
ENDIF

lcGlYear   = loParentForm.lcGlYear
lcGlPeriod = loParentForm.lcGlPeriod
IF !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
loFormset.AriaForm1.DtpickerIssueDate.SetFocus
RETURN .F.
ENDIF
loParentForm.lcGlYear   = lcGlYear
loParentForm.lcGlPeriod = lcGlPeriod

laIssQty[1] = IIF(llIssue,-m.Iss_Qty1,m.Iss_Qty1)
laIssQty[2] = IIF(llIssue,-m.Iss_Qty2,m.Iss_Qty2)
laIssQty[3] = IIF(llIssue,-m.Iss_Qty3,m.Iss_Qty3)
laIssQty[4] = IIF(llIssue,-m.Iss_Qty4,m.Iss_Qty4)
laIssQty[5] = IIF(llIssue,-m.Iss_Qty5,m.Iss_Qty5)
laIssQty[6] = IIF(llIssue,-m.Iss_Qty6,m.Iss_Qty6)
laIssQty[7] = IIF(llIssue,-m.Iss_Qty7,m.Iss_Qty7)
laIssQty[8] = IIF(llIssue,-m.Iss_Qty8,m.Iss_Qty8)
laIssQty[9] = IIF(llIssue,-m.Issue_Qty,m.Issue_Qty)

*T20060818.0001(C200876) TMI [Start] Accept issue/Return Styles Per Bin
IF ASCAN(loParentForm.laEvntTrig , PADR('DVLDPOBN',10)) <> 0 .AND. loParentForm.mDoTrigger(PADR('ISUSEBIN',10))
lcIssQtyFl = gfTempName()
SAVE TO (oAriaApplication.WorkDir+lcIssQtyFl) ALL LIKE laIssQty*
IF !loParentForm.mDoTrigger(PADR('DVLDPOBN',10))
RETURN .F.
ENDIF
ELSE
*T20060818.0001(C200876) TMI [End  ]

=lfIssRetSty(m.Typ,m.cCatgTyp,m.cInvType,m.Item,lcIssWare,m.Dyelot,@laIssQty,lnIssCost,;
m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
IIF(llIssue,SPACE(6),loParentForm.lcSession),IIF(llIssue,loParentForm.lcSession,SPACE(6)))

*T20060818.0001(C200876) TMI [Start]
ENDIF
*T20060818.0001(C200876) TMI [End  ]

m.Used_Qty1 = m.Used_Qty1 - laIssQty[1]
m.Used_Qty2 = m.Used_Qty2 - laIssQty[2]
m.Used_Qty3 = m.Used_Qty3 - laIssQty[3]
m.Used_Qty4 = m.Used_Qty4 - laIssQty[4]
m.Used_Qty5 = m.Used_Qty5 - laIssQty[5]
m.Used_Qty6 = m.Used_Qty6 - laIssQty[6]
m.Used_Qty7 = m.Used_Qty7 - laIssQty[7]
m.Used_Qty8 = m.Used_Qty8 - laIssQty[8]
m.Used_Qty  = m.Used_Qty  - laIssQty[9]

*!*************************************************************
*! Name      : lfvIssDye
*! Developer : AHMED MAHER
*! Date      : 08/21/2004
*! Purpose   : Validated Issued Dyelot
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvIssDye()
*!*************************************************************
FUNCTION lfvIssDye

LOCAL llFound
llFound = .F.
m.Style = m.Item
m.cWareCode = lcIssWare
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loParentForm)
SELECT FABDYE
LOCATE
llFound = !EOF()
ENDIF
lcDyelot = m.Dyelot
IF !llFound AND !gfItmDyBrw(m.cInvType,m.Item,@lcDyelot,lcIssWare,.T.)
RETURN
ENDIF
m.Dyelot = lcDyelot

*!*************************************************************
*! Name      : lfvReturn
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/24/2004
*! Purpose   : Return Cost Item
*!*************************************************************
*! Calls     : MFITMISS.SPX,MFSTYISS.SPX,MFMFGISS.SPX,lfShowLog
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvReturn()
*!*************************************************************
FUNCTION lfvReturn
LPARAMETERS loFormSet

ldIssDate = oariaapplication.systemdate
*--- DOC.
*--- Valid function to return and issued cost item
*--- DOC.

DO CASE
CASE INLIST(m.cCatgTyp,'F','T') .AND. SEEK('S'+FABRIC.Scale,'Scale')
*!*      *C200255,1 (Begin) Update MATINVJL with custom Voucher no for CON10.
*!*      IF ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0 AND TYPE('lcVoucNo') = 'U'
*!*        lcVoucNo = ""
*!*        llContVout = .T.
*!*        = gfDoTriger("MAPOREC",PADR("GETVOUT",10))
*!*        IF !llContVout
*!*          RETURN
*!*        ENDIF
*!*        llFrstTime = .F.
*!*      ENDIF
*!*      *C200255,1 (Begin)
llItemDye = loParentForm.laSetups[14,2]='Y' .AND. Fabric.cDye_Flg = 'Y'
llOk = .F.

*B131716,1 WSH 04/27/2006 Save the InvType and Style to restore them after scatterign from [Start]
LOCAL lcCurInvType, lcCurStyle
lcCurInvType = m.cInvType
lcCurStyle   = m.Style
*B131716,1 WSH 04/27/2006 [End]

SELECT MATINVJL
SCATTER MEMVAR

*B131716,1 WSH 04/27/2006 Save the InvType and Style to restore them after scatterign from [Start]
m.cInvType = lcCurInvType
m.Style    = lcCurStyle
*B131716,1 WSH 04/27/2006 [End]

IF INLIST(loParentForm.laSetups[11,2],'L','F','I')
SELECT BOMCOST
LOCATE FOR MfgCode = m.MfgCode
IF FOUND()
lcBrFields = "cWareCode:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_WAREHOUS,loFormSet.GetHeaderText("LANG_MFCSSH_WAREHOUS",loFormSet.HeaderAlias))+"',dTranDate:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DATE,loFormSet.GetHeaderText("LANG_MFCSSH_DATE",loFormSet.HeaderAlias))+"',nTotQty:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_QUNTITY,loFormSet.GetHeaderText("LANG_MFCSSH_QUNTITY",loFormSet.HeaderAlias))+"',nUnitCst:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_COST,loFormSet.GetHeaderText("LANG_MFCSSH_COST",loFormSet.HeaderAlias))+"',nTotCst:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_AMOUNT,loFormSet.GetHeaderText("LANG_MFCSSH_AMOUNT",loFormSet.HeaderAlias))+;
"',cApInvNo:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_INVOICE,loFormSet.GetHeaderText("LANG_MFCSSH_INVOICE",loFormSet.HeaderAlias))+"'"
*--- DOC.
*--- Browse for the issued lots
*--- DOC.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF ARIABROW('',LANG_MFCSSH_ISSUEDLOT,gnBrHSRow1, gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','cTktNo','laBrowArr')
IF ARIABROW('',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUEDLOT,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUEDLOT",loFormSet.HeaderAlias)),;
gnBrHSRow1, gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','cTktNo','laBrowArr')
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcIssWare   = BomCost.cWareCode
lnIssWare   = CEILING(ASCAN(loParentForm.laMatWare,lcIssWare)/2)
llWareStat  = .F.
lnIssCost   = BomCost.nUnitCst
SELECT MATINVJL
SUM FOR cISession = BomCost.cISession nStk1,nStk2,nStk3,nStk4,nStk5,nStk6,nStk7,nStk8 TO;
m.Iss_Qty1,m.Iss_Qty2,m.Iss_Qty3,m.Iss_Qty4,m.Iss_Qty5,m.Iss_Qty6,m.Iss_Qty7,m.Iss_Qty8
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
STORE -EVALUATE('m.Iss_Qty'+lcI) TO ('m.Iss_Qty'+lcI),laTotQty[lnI]
ENDFOR

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
lcAlias =SELECT()
lcCurWare = m.cWareCode
m.cWareCode = lcIssWare
m.Dyelot    = SPACE(10)
=lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loFormSet)
m.nOnHand = IIF(loParentForm.laSetups[2,2]='Y',FABDYE.TotStk,Fabric.TotStk)
m.cWareCode = lcCurWare
SELECT(lcAlias)
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]

STORE BomCost.nTotQty TO m.Issue_Qty,laTotQty[9]
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .F.
=gfCallForm('MFISSRET',.F.,'.F.')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
loFormSet.AriaForm1.grdIsLog.RecordSource = ''
m.cTrType   = '9'
m.cTrCode   = m.CutTkt
m.cDyelot   = m.Dyelot
=lfOpenSql('ITEMJRNL','MATINVJL','STYINVJL','CINVTYPE+STYLE+CTRTYPE+CTRCODE'+;
IIF(EMPTY(m.Dyelot),'','+CDYELOT'),loFormSet)
WITH loFormSet.AriaForm1.grdIsLog
.RecordSource = 'MATINVJL'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column1.ControlSource  = "PADR(IIF(MATINVJL.cIRType='I','"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
.Column1.ControlSource  = "PADR(IIF(MATINVJL.cIRType='I','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))+"','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURN,loFormSet.GetHeaderText("LANG_MFCSSH_RETURN",loFormSet.HeaderAlias))+"'),6)"
*N000682,1 11/20/2012 MMT Globlization changes[End]

.Column2.ControlSource  = "MATINVJL.cWareCode"
.Column2.Visible        = (loParentForm.laSetups[2,2]='Y')
.Column3.ControlSource  = "MATINVJL.dTrDate"
.Column4.ControlSource  = "MATINVJL.CSESSION"
.Column5.ControlSource  = "ABS(MATINVJL.nTotStk)"
.Column6.ControlSource  = "MATINVJL.nCost"
.Column7.ControlSource  = "MATINVJL.cApInvNo"
.Column7.Visible        = (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)
.Column8.ControlSource  = "gfCodDes(MATINVJL.cOprCode,'MFGCODE')"
.Column9.ControlSource  = "MATINVJL.cLotNo"
.Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
.Column10.ControlSource = "ABS(MATINVJL.nStk1)"
.Column11.ControlSource = "ABS(MATINVJL.nStk2)"
.Column12.ControlSource = "ABS(MATINVJL.nStk3)"
.Column13.ControlSource = "ABS(MATINVJL.nStk4)"
.Column14.ControlSource = "ABS(MATINVJL.nStk5)"
.Column15.ControlSource = "ABS(MATINVJL.nStk6)"
.Column16.ControlSource = "ABS(MATINVJL.nStk7)"
.Column17.ControlSource = "ABS(MATINVJL.nStk8)"
.Column18.Visible       = .F.
.Column19.Visible       = .F.
.Column20.Visible       = .F.
ENDWITH
SELECT MATINVJL
ENDIF
ENDIF
ELSE
*--- DOC.
*--- If the cost method is average.
*--- DOC.
lcIssWare   = EVALUATE(loParentForm.lcPosHdr+'.CMATWARE')
lnIssWare   = CEILING(ASCAN(loParentForm.laMatWare,lcIssWare)/2)
llWareStat  = .T.

m.cWareCode = lcIssWare
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE',loParentForm)
SELECT FABDYE
LOCATE FOR Dyelot = SPACE(10)
ENDIF

lnIssCost   = IIF(loParentForm.laSetups[11,2]='A',FabDye.nAveCstBuy,Fabric.TotCost)/lfGetConv(FABRIC.cConvBuy)


*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
m.nOnHand = IIF(loParentForm.laSetups[2,2]='Y',FABDYE.TotStk,Fabric.TotStk)
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]


LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
laTotQty[lnI] = EVALUATE('m.Used_Qty'+lcI)
m.Iss_Qty&lcI = 0
ENDFOR

m.Issue_Qty = 0
laTotQty[9] = m.Used_Qty
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .F.
=gfCallForm('MFISSRET',.F.,'.F.')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
ENDIF
IF llOk .AND. loParentForm.laSetups[13,2]='Y' .AND. Fabric.lTrkRolls

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
*=lfvMatRolls('R',m.cRSession,m.cISession,SUBSTR(m.Item,1,7),m.IClr,;
m.cWareCode,m.cDyelot,lnTotQty,m.Issue_Qty)

lcInvType ='0002'
lenclrlen  = LEN(gfitemmask("PN", "", lcInvType))
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [T20090122.0014]
*!*	      =lfvMatRolls('R',m.cRSession,m.cISession,SUBSTR(m.Item,1,7),Right(ITEM,lenclrlen),;
*!*	                    m.cWareCode,m.cDyelot,laTotQty[9],m.Issue_Qty)
=lfvMatRolls('R',m.cRSession,m.cISession,SUBSTR(m.Item,1,7),Right(ITEM,lenclrlen),;
m.cWareCode,m.cDyelot,laTotQty[9],m.Issue_Qty,m.Item)
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [END]
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

ENDIF
m.TotStk = lfGetTotStk(loParentForm)
SELECT (loParentForm.lcTktSheet)
GATHER FIELDS TotStk MEMVAR
CASE m.cCatgTyp='S' .AND. SEEK(m.Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale')
lcIssWare   = EVALUATE(loParentForm.lcPosHdr+'.CITEMWARE')
lnIssWare   = CEILING(ASCAN(loParentForm.laStyWare,lcIssWare)/2)
=SEEK(m.Item+lcIssWare+space(10),'StyDye')
lnIssCost   = IIF(loParentForm.laSetups[12,2]='A',StyDye.Ave_Cost,Style.TotCost)
STORE 0 TO m.Iss_Qty1,m.Iss_Qty2,m.Iss_Qty3,m.Iss_Qty4,m.Iss_Qty5,;
m.Iss_Qty6,m.Iss_Qty7,m.Iss_Qty8,m.Issue_Qty
llWareStat  = .T.

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
m.nOnHand = IIF(loParentForm.laSetups[2,2]='Y',StyDye.TotStk,Style.TotStk)
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]

*T20060818.0001(C200876) TMI [Start] Call Return Screen with modification of Bin Location
IF ASCAN(loParentForm.laEvntTrig , PADR('LDFNPORT',10)) <> 0 .AND. loParentForm.mDoTrigger(PADR('ISUSEBIN',10))
=loParentForm.mDoTrigger(PADR('LDFNPORT',10))
*T20071102.0018,10/C200876 TMI 07/30/2008 [End  ]
ELSE
*T20060818.0001(C200876) TMI [End  ]
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .F.
=gfCallForm('MFISSRET',.F.,'.F.')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*T20060818.0001(C200876) TMI [Start]
ENDIF
*T20060818.0001(C200876) TMI [End  ]

m.TotStk = lfGetTotStk(loParentForm)
SELECT (loParentForm.lcTktSheet)
GATHER FIELDS TotStk MEMVAR

loFormSet.AriaForm1.grdIsLog.RecordSource = ''
SELECT TMPISSLOG
ZAP

SELECT STYINVJL

*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [BEGIN]
*SET ORDER TO TAG STYINVJL
*IF SEEK(m.Item)
gfSetOrder('STYINVJL')
IF GFSEEK(m.Item)
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [END]

SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = m.Item;
FOR cTrCode+cTrType $ m.CutTkt+'1'+','+m.CutTkt+'I'
SCATTER MEMO TO laIssLog
INSERT INTO TMPISSLOG FROM ARRAY laIssLog
ENDSCAN
ENDIF
SELECT TMPISSLOG
LOCATE

WITH loFormSet.AriaForm1.grdIsLog
.RecordSource = 'TMPISSLOG'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column1.ControlSource  = "PADR(IIF(TMPISSLOG.cIRType='I','"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
.Column1.ControlSource  = "PADR(IIF(TMPISSLOG.cIRType='I','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))+"','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURN,loFormSet.GetHeaderText("LANG_MFCSSH_RETURN",loFormSet.HeaderAlias))+"'),6)"
*N000682,1 11/20/2012 MMT Globlization changes[End]

.Column2.ControlSource  = "TMPISSLOG.cWareCode"
.Column2.Visible        = (loParentForm.laSetups[2,2]='Y')
.Column3.ControlSource  = "TMPISSLOG.dTrDate"
.Column4.ControlSource  = "TMPISSLOG.CSESSION"
.Column5.ControlSource  = "ABS(TMPISSLOG.nTotStk)"
.Column6.ControlSource  = "TMPISSLOG.nCost"
.Column7.Visible        = .F.
.Column8.ControlSource  = "gfCodDes(TMPISSLOG.cOprCode,'MFGCODE')"
.Column9.ControlSource  = "TMPISSLOG.cLotNo"
.Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
.Column10.ControlSource = "ABS(TMPISSLOG.nStk1)"
.Column11.ControlSource = "ABS(TMPISSLOG.nStk2)"
.Column12.ControlSource = "ABS(TMPISSLOG.nStk3)"
.Column13.ControlSource = "ABS(TMPISSLOG.nStk4)"
.Column14.ControlSource = "ABS(TMPISSLOG.nStk5)"
.Column15.ControlSource = "ABS(TMPISSLOG.nStk6)"
.Column16.ControlSource = "ABS(TMPISSLOG.nStk7)"
.Column17.ControlSource = "ABS(TMPISSLOG.nStk8)"
.Column18.Visible       = .F.
.Column19.Visible       = .F.
.Column20.Visible       = .F.
ENDWITH
CASE INLIST(m.cCatgTyp,'M','P','D')
PRIVATE lcMfgGlAcnt
lcMfgGlAcnt = loParentForm.lcMfgGlAcnt
LOCAL ARRAY laMfgRFld[7,2]
=ACOPY(loParentForm.laMfgRFld,laMfgRFld)
IF !(OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0) .OR. ;
(m.cCatGTyp = 'M' .AND. gfRltFld(m.MfgCode,@laMfgRFld,'MFGCODE') ;
.AND. !EMPTY(lcMfgGlAcnt))
lnIssCost   = m.UntCost
m.Issue_Qty = 0
llWareStat = .F.

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
m.nOnHand = 0
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .F.
=gfCallForm('MFISSRET',.F.,'.F.')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
ELSE
IF m.cCatGTyp $ 'PD'
*E300725,1 Message : 38091
*E300725,1 You cannot return the duty cost items since you are linked
*E300725,1 with the AP. The duty cost application is done from the AP module.
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38091B00000','ALERT',IIF(m.cCatGTyp='D',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURNSML,loFormSet.GetHeaderText("LANG_MFCSSH_RETURNSML",loFormSet.HeaderAlias))+'|'+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DUTY,loFormSet.GetHeaderText("LANG_MFCSSH_DUTY",loFormSet.HeaderAlias))+'|'+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DUTY,loFormSet.GetHeaderText("LANG_MFCSSH_DUTY",loFormSet.HeaderAlias)),;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURNSML,loFormSet.GetHeaderText("LANG_MFCSSH_RETURNSML",loFormSet.HeaderAlias))+'|'+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PURCHASE,loFormSet.GetHeaderText("LANG_MFCSSH_PURCHASE",loFormSet.HeaderAlias))+'|'+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PURCHASE,loFormSet.GetHeaderText("LANG_MFCSSH_PURCHASE",loFormSet.HeaderAlias))))
ELSE
*E300725,1 Message : 38092
*E300725,1 This MFG code has been setup to be applied from the A/P.
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38092B00000','ALERT')
ENDIF
ENDIF
OTHERWISE
lnIssCost   = m.UntCost
m.Issue_Qty = 0

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
m.nOnHand = 0
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .F.
=gfCallForm('MFISSRET',.F.,'.F.')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
ENDCASE
=lfShowLog(loFormSet)

*!*************************************************************
*! Name      : lfvGenerate
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/28/2004
*! Purpose   : Generate ticket cost sheet
*!*************************************************************
*! Calls     : MFGENSHT.SPX,gfModalGen,gfSheetItem,lfwBrow,lfShOprHd,;
*!             lfShOprDt,lfShowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvGenerate()
*!*************************************************************
FUNCTION lfvGenerate
LPARAMETERS loFormSet

PRIVATE lcItemFile,lcSizes,lnTranQty,lcItemType,;
lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnEst6,lnEst7,;
lnTotEst1,lnTotEst2,lnTotEst3,lnTotEst4,lnTotEst5,lnTotEst6,lnTotEst7,llContinue,laQty
DECLARE laQty[9]

=gfOpenFile(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
SELECT (loFormSet.lcPosHdr)
REPLACE LINK_CODE WITH IIF(EMPTY(LINK_CODE),'DEFDEF',LINK_CODE),;
CITEMWARE WITH IIF(EMPTY(CITEMWARE),cWareCode,CITEMWARE),;
CMATWARE  WITH IIF(EMPTY(CMATWARE),cWareCode,CMATWARE)

=gfOpenFile(oAriaApplication.DataDir+'WAREHOUS',oAriaApplication.DataDir+'WAREHOUS','SH')
SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS WHERE lStyInv INTO ARRAY loFormSet.laStyWare
SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS WHERE lMatInv INTO ARRAY loFormSet.laMatWare
USE IN WAREHOUS

SELECT (loFormSet.lcPosHdr)
*B610614,1 TMI 12/08/2013 11:19 [Start] get the values of lnTkSstWare and lnTkMtWare first
loFormSet.lnTkStWare = CEILING(ASCAN(loFormSet.laStyWare,EVALUATE(loFormSet.lcPosHdr+'.CITEMWARE'))/2)
loFormSet.lnTkMtWare = CEILING(ASCAN(loFormSet.laMatWare,EVALUATE(loFormSet.lcPosHdr+'.CMATWARE'))/2)
*B610614,1 TMI 12/08/2013 11:19 [End  ] 
loFormSet.lnTkStWare = IIF(ASCAN(loFormSet.laStyWare,CITEMWARE)=0,1,CEILING(loFormSet.lnTkStWare))
loFormSet.lnTkMtWare = IIF(ASCAN(loFormSet.laMatWare,CMATWARE)=0,1,CEILING(loFormSet.lnTkMtWare))

IF !loFormSet.llAutoGen AND (loFormSet.laSetups[1,2]='Y' OR (loFormSet.laSetups[2,2]='Y' AND;
(loFormSet.llStyCost         OR  loFormSet.llMatCost)))
PRIVATE loParentForm
loParentForm = loFormSet
llContinue = .F.
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFGENSHT.SCX")
=gfCallForm('MFGENSHT')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
IF !llContinue
RETURN
ENDIF
ENDIF

SELECT (loFormSet.lcPosHdr)
REPLACE CITEMWARE WITH loFormSet.laStyWare[loFormSet.lnTkStWare,2],;
CMATWARE  WITH loFormSet.laMatWare[loFormSet.lnTkMtWare,2]
lcItemFile = IIF(loFormSet.lcTranType='T','Fabric','Style')

SELECT (loFormSet.lcTranFile)
LOCATE
IF !FOUND()
*E300725,1 Message : 38065
*E300725,1 Lines for this ticket not found. Cannot generate cost sheet
*E300725,1 Button : 00000
*E300725,1 Ok
*C200080,1 AMM Consider the dye order case
=gfModalGen('TRM38065B00000','ALERT',IIF(loFormSet.lcTranType='M',;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))),'#'),;
IIF(loFormSet.lcTranType $ 'ID',IIF(EMPTY(loFormSet.lcDyePO),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_PO,loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_DYEORD,loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias))),'#')),;
IIF(loFormSet.lcTranType='N',;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))),'#')))))

RETURN
ENDIF

LOCAL llFound,llGetCostFromStyle
IF INLIST(loFormSet.lcTranType,'M','T')
llFound = .F.
m.cInvType  = loFormSet.lcInvType
m.cItmMajor = EVALUATE(loFormSet.lcPosHdr+'.Style')

*B125713,1 AMH Use cost sheet type cheek for the correct cost sheet type [Start]
m.cCstShtTyp = loFormSet.lcStyleTyp
*IF lfOpenSql('BOM','BOM','MULTIBOM','CINVTYPE+CITMMAJOR',loFormSet)

IF lfOpenSql('BOM','BOM','MULTIBOM','CINVTYPE+CITMMAJOR+CCSTSHTTYP',loFormSet)
*B125713,1 AMH [End]

SELECT BOM
LOCATE
llFound = !EOF()
ENDIF
IF !llFound
*E300725,1 Message : 38031
*E300725,1 Cost sheet not found for style: xxx cannot generate cutting
*E300725,1 ticket cost sheet
*E300725,1 Button : 00000
*E300725,1 Ok

*B607803,1 WAM 10/18/2006 Fix message
*=gfModalGen('TRM38031B00000','ALERT',ALLTRIM(loFormSet.lcMjrHdr)+': '+;
ALLTRIM(loFormSet.lcPosHdr+'.Style')+'|'+;
IIF(loFormSet.lcTranType='M',;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))),'#')))

=gfModalGen('TRM38031B00000','ALERT',ALLTRIM(loFormSet.lcMjrHdr)+': '+;
ALLTRIM(EVALUATE(loFormSet.lcPosHdr+'.Style'))+'|'+;
IIF(loFormSet.lcTranType='M',;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))),'#')))

*B607803,1 WAM 10/18/2006 (End)

RETURN
ENDIF
ELSE
llMulCurr = gfGetMemVar('llMulCurr')

SELECT (loFormSet.lcPosLn)
SCAN FOR TranCd = '1'
llFound = .F.
m.cInvType  = cInvType
m.cItmMajor = SUBSTR(Style,1,LEN(loFormSet.lcMjrMsk))

*B125713,1 AMH Use cost sheet type cheek for the correct cost sheet type [Start]
m.cCstShtTyp = loFormSet.lcStyleTyp

*IF lfOpenSql('BOM','BOM','MULTIBOM','CINVTYPE+CITMMAJOR',loFormSet)
IF lfOpenSql('BOM','BOM','MULTIBOM','CINVTYPE+CITMMAJOR+CCSTSHTTYP',loFormSet)
*B125713,1 AMH [End]

SELECT BOM
LOCATE
llFound = !EOF()
ENDIF
SELECT (loFormSet.lcPosLn)
IF !llFound
*E300725,1 Message : 38031
*E300725,1 Cost sheet not found for style: xxx cannot purchase order cost sheet
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38031B00000','ALERT',ALLTRIM(loFormSet.lcMjrHdr)+': '+ALLTRIM(SUBSTR(Style,1,LEN(loFormSet.lcMjrMsk)))+'|'+;
IIF(!EMPTY(loFormSet.lcDyePO),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_DYEORD,loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias))),'#'),;
IIF(loFormSet.lcTranType='N',;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_PO,loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))),'#'))))

RETURN
ENDIF

IF NFCOST1+NFCOST2+NFCOST3+NFCOST4+NFCOST5 = 0 .AND. EMPTY(loFormSet.lcDyePO)
=SEEK(STYLE,'STYLE')
llGetCostFromStyle = (!llMulCurr OR STYLE.CPRICECUR = EVALUATE(loFormSet.lcPOSHDR+'.CPRICECUR'))
REPLACE NFCOST1 WITH IIF(llGetCostFromStyle,STYLE.NICOST1,0),;
NFCOST2 WITH IIF(llGetCostFromStyle,STYLE.NICOST2,0),;
NFCOST3 WITH IIF(llGetCostFromStyle,STYLE.NICOST3,0),;
NFCOST4 WITH IIF(llGetCostFromStyle,STYLE.NICOST4,0),;
NFCOST5 WITH IIF(llGetCostFromStyle,STYLE.NICOST5,0),;
NFCOST6 WITH IIF(llGetCostFromStyle,STYLE.NICOST6,0),;
NFCOST7 WITH IIF(llGetCostFromStyle,STYLE.NICOST7,0)
REPLACE NICOST1 WITH NFCOST1,;
NICOST2 WITH NFCOST2,;
NICOST3 WITH NFCOST3,;
NICOST4 WITH NFCOST4,;
NICOST5 WITH NFCOST5,;
NICOST6 WITH NFCOST6,;
NICOST7 WITH NFCOST7

SELECT (loFormSet.lcPOSHDR)
REPLACE NICOST1 WITH NICOST1 + EVALUATE(loFormSet.lcPOSLN+'.NICOST1'),;
NICOST2 WITH NICOST2 + EVALUATE(loFormSet.lcPOSLN+'.NICOST2'),;
NICOST3 WITH NICOST3 + EVALUATE(loFormSet.lcPOSLN+'.NICOST3'),;
NICOST4 WITH NICOST4 + EVALUATE(loFormSet.lcPOSLN+'.NICOST4'),;
NICOST5 WITH NICOST5 + EVALUATE(loFormSet.lcPOSLN+'.NICOST5'),;
NICOST5 WITH NICOST6 + EVALUATE(loFormSet.lcPOSLN+'.NICOST6'),;
NICOST5 WITH NICOST7 + EVALUATE(loFormSet.lcPOSLN+'.NICOST7'),;
NFCOST1 WITH NFCOST1 + EVALUATE(loFormSet.lcPOSLN+'.NFCOST1'),;
NFCOST2 WITH NFCOST2 + EVALUATE(loFormSet.lcPOSLN+'.NFCOST2'),;
NFCOST3 WITH NFCOST3 + EVALUATE(loFormSet.lcPOSLN+'.NFCOST3'),;
NFCOST4 WITH NFCOST4 + EVALUATE(loFormSet.lcPOSLN+'.NFCOST4'),;
NFCOST5 WITH NFCOST5 + EVALUATE(loFormSet.lcPOSLN+'.NFCOST5'),;
NFCOST6 WITH NFCOST6 + EVALUATE(loFormSet.lcPOSLN+'.NFCOST6'),;
NFCOST7 WITH NFCOST7 + EVALUATE(loFormSet.lcPOSLN+'.NFCOST7')
REPLACE POTOTAL WITH NICOST1+NICOST2+NICOST3+NICOST4+NICOST5+NICOST6+NICOST7

DECLARE laTableUpdate[2,2]
laTableUpdate[1,1] = loFormSet.lcPosHdr
laTableUpdate[1,2] = 'POSHDR'

laTableUpdate[2,1] = loFormSet.lcPosLn
laTableUpdate[2,2] = 'POSLN'

=lfTableUpdate(loFormSet)

SELECT (loFormSet.lcPOSLN)
ENDIF
ENDSCAN
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_MFCSSH_BLDCSTSHT NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_BLDCSTSHT,loFormSet.GetHeaderText("LANG_MFCSSH_BLDCSTSHT",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


SELECT (loFormSet.lcTmpTkt)
ZAP
STORE 0 TO lnEst1,lnEst2,lnEst3,lnEst4,lnEst5,lnEst6,lnEst7,lnLastSeq,;
lnTotEst1,lnTotEst2,lnTotEst3,lnTotEst4,lnTotEst5,lnTotEst6,lnTotEst7
lcLastOpr = ''
llContinue = .T.

IF loFormSet.lcTranType='T'
m.cInvType  = loFormSet.lcInvType
m.cStyMajor = EVALUATE(loFormSet.lcPosHdr+'.Style')
*N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[T20110914.0019][Start]
*!*	  IF !lfOpenSql('ITEM','FABRIC','CSTYLE','CINVTYPE+CSTYMAJOR',loFormSet)
*!*	    =lfSetIndex(loFormSet.lcItemFile,'STYLE','STYLE')
IF lfOpenSql('ITEM',lcItemFile,'CSTYLE','CINVTYPE+CSTYMAJOR',loFormSet)
=lfSetIndex(lcItemFile,'STYLE','STYLE')
*N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[T20110914.0019][END]
ENDIF
ENDIF

SELECT (loFormSet.lcTranFile)
SCAN FOR TranCd='1'
lcItem    = Style
lcDyelot  = Dyelot
*N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[T20110914.0019][Start]
IF loFormSet.lcTranType='T'
llDyelot  = SEEK(lcItem,lcItemFile) .AND. EVALUATE(lcItemFile+'.cDye_Flg')='Y'
lcScale   = EVALUATE(lcItemFile+'.Scale')
ELSE
*N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[T20110914.0019][END]
llDyelot  = SEEK(lcItem,lcItemFile) .AND. EVALUATE(loFormSet.lcItemFile+'.cDye_Flg')='Y'
lcScale   = EVALUATE(loFormSet.lcItemFile+'.Scale')
*N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[T20110914.0019][Start]
ENDIF
*N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[T20110914.0019][END]
STORE 0 TO laQty
laQty[1] = Qty1
laQty[2] = Qty2
laQty[3] = Qty3
laQty[4] = Qty4
laQty[5] = Qty5
laQty[6] = Qty6
laQty[7] = Qty7
laQty[8] = Qty8
laQty[9] = TotQty
SELECT (loFormSet.lcTmpTkt)
APPEND BLANK
REPLACE Item      WITH lcItem  ,;
cInvType  WITH EVALUATE(loFormSet.lcTranFile+'.cInvType') ,;
cDyelot   WITH lcDyelot,;
LineNo    WITH EVALUATE(loFormSet.lcTranFile+'.LineNo') ,;
cWareCode WITH EVALUATE(loFormSet.lcTranFile+'.cWareCode') ,;
nQty1     WITH laQty[1] ,;
nQty2     WITH laQty[2] ,;
nQty3     WITH laQty[3] ,;
nQty4     WITH laQty[4] ,;
nQty5     WITH laQty[5] ,;
nQty6     WITH laQty[6] ,;
nQty7     WITH laQty[7] ,;
nQty8     WITH laQty[8] ,;
nTotQty   WITH laQty[9] ,;
nRecNo    WITH RECNO(loFormSet.lcTranFile)
lnRecDetNo = RECCOUNT(loFormSet.lcDetFile)

LOCAL lcCatgType
lcCatgType   = 'PDM'+IIF(loFormSet.lcTranType<>'N','FTS','')
m.cInvType   = cInvType
m.cItmMajor  = PADR(SUBSTR(Item,1,LEN(loFormSet.lcMjrMsk)),19)
m.cCstShtTyp = loFormSet.lcStyleTyp
m.cCstSht_ID = EVALUATE(loFormSet.lcTranFile+'.cCstSht_ID')

*B125713,1 AMH Check if there is no cost sheet ID saved in POSLN table [Start]
IF EMPTY(m.cCstSht_ID)
*Message : 38290
*There is no cost sheet assigned to style xxxxx. Proceed with default cost sheet if found?
*Button : 38002
*Proceed  Cancel
IF gfModalGen('QRM38290B38002','ALERT',ALLTRIM(loFormSet.lcMjrHdr)+': '+ALLTRIM(lcItem)) = 1
IF !lfOpenSql('BOMHEADR','BOMHEADR','BOMHEADR','CINVTYPE+CITMMAJOR+CCSTSHTTYP',loFormSet)
RETURN .F.
ENDIF
SELECT BOMHEADR
LOCATE FOR LDEFCSTSHT
m.cCstSht_ID = CCSTSHT_ID
USE IN BOMHEADR
SELECT (loFormSet.lcTranFile)
GATHER FIELDS cCstSht_ID MEMVAR
ELSE
llContinue = .F.
SELECT (loFormSet.lcTktSheet)
DELETE ALL
SELECT (loFormSet.lcDetFile)
DELETE ALL
SELECT (loFormSet.lcOprHdr)
DELETE ALL
EXIT
ENDIF
ENDIF
*B125713,1 AMH [End]

IF !lfOpenSql('BOM',loFormSet.lcBom,'MULTIBOM','CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID',loFormSet)
RETURN .F.
ENDIF

SELECT *,RECNO() AS nRecNo , "S" AS cStatus , SPACE(30) as cTypelabel;
FROM (loFormSet.lcBom) WHERE cCatgTyp $ lcCatgType;
INTO DBF (oAriaApplication.WorkDir+loFormSet.lcTmpBom)

*B999999,1 AMH Fix bug of error building key of index [Start]
*INDEX ON cInvType+citmmajor+typ+IIF(.NOT.(ccatgtyp$"PMD"),citmmask,mfgcode)+cInvTypC+item TAG (loFormSet.lcTmpBom)
IF lcCatgType $ "PMD"
INDEX ON cInvType+citmmajor+typ+mfgcode+cInvTypC+item TAG (loFormSet.lcTmpBom)
ELSE
INDEX ON cInvType+citmmajor+typ+citmmask+cInvTypC+item TAG (loFormSet.lcTmpBom)
ENDIF
*B999999,1 AMH [End]

SELECT (loFormSet.lcTmpTkt)

*--- DOC.
*--- Global function to create cost sheet and update BOMLINE,Operatoin header
*--- DOC.
llContinue = gfSheetItem(loFormSet.lcTranType,EVALUATE(loFormSet.lcPosHdr+'.PO'),;
EVALUATE(loFormSet.lcPosHdr+'.LINK_CODE'),lcItem,cInvType,;
LineNo,lcDyelot,EVALUATE(loFormSet.lcPosHdr+'.CITEMWARE'),;
EVALUATE(loFormSet.lcPosHdr+'.CMATWARE'),@laQty,loFormSet.lcTmpBom,;
loFormSet.lcTktSheet,loFormSet.lcDetFile,loFormSet.lcOprHdr,@lcLastOpr,;
IIF(loFormSet.lcStyleTyp='I',EVALUATE(loFormSet.lcTranFile+'.nFCost1'),0),;
@lnEst1,@lnEst2,@lnEst3,@lnEst4,@lnEst5,@lnEst6,@lnEst7,;
loFormSet.lcPosHdr,loFormSet.lcPosLn,loFormSet.lcDetFile)

IF !llContinue OR lnRecDetNo = RECCOUNT(loFormSet.lcDetFile)

*B125713,1 AMH Privent display message two times [Start]
*IF lnRecDetNo = RECCOUNT(loFormSet.lcDetFile)
IF lnRecDetNo = RECCOUNT(loFormSet.lcDetFile) AND llContinue
*B125713,1 AMH [End]

=gfModalGen('TRM38031B00000','ALERT',ALLTRIM(loFormSet.lcMjrHdr)+': '+;
ALLTRIM(lcItem)+'|'+IIF(loFormSet.lcTranType='M',;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))),'#'),;
IIF(loFormSet.lcTranType $ 'ID',IIF(EMPTY(loFormSet.lcDyePO),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_PO,loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_DYEORD,loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias))),'#')),;
IIF(loFormSet.lcTranType='N',STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))),'#')))))
llContinue = .F.
ENDIF

SELECT (loFormSet.lcTktSheet)
DELETE ALL
SELECT (loFormSet.lcDetFile)
DELETE ALL
SELECT (loFormSet.lcOprHdr)
DELETE ALL
EXIT
ENDIF
lnTotEst1 = lnTotEst1 + lnEst1
lnTotEst2 = lnTotEst2 + lnEst2
lnTotEst3 = lnTotEst3 + lnEst3
lnTotEst4 = lnTotEst4 + lnEst4
lnTotEst5 = lnTotEst5 + lnEst5
lnTotEst6 = lnTotEst6 + lnEst6
lnTotEst7 = lnTotEst7 + lnEst7
ENDSCAN

IF llContinue
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*llDummy = loFormSet.llPWInst .AND. (loFormSet.lcTranType = "M") .AND. lfPwShtItm('A',.F.,loFormSet)
llDummy = loFormSet.llPWInst .AND. !gfGetMemVar('LUSEBUNDLE',oAriaApplication.ActiveCompanyID) .AND. (loFormSet.lcTranType = "M") .AND. lfPwShtItm('A',.F.,loFormSet)
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
ENDIF
GO TOP IN (loFormSet.lcTktSheet)
GO TOP IN (loFormSet.lcDetFile)
GO TOP IN (loFormSet.lcOprHdr)
WAIT CLEAR
SELECT(loFormSet.lcTktSheet)
IF EOF()
IF llContinue
*E300725,1 Message : 38066
*E300725,1 Cost sheet information not found for ticket lines
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38066B00000','ALERT',IIF(loFormSet.lcTranType='M',;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))),'#'),;
IIF(loFormSet.lcTranType $ 'ID',IIF(EMPTY(loFormSet.lcDyePO),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,;
loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYEORD,;
loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias))),'#')),;
IIF(loFormSet.lcTranType='N',STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ORDHDR,;
loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))),'#')))))

ENDIF
ELSE
SELECT (loFormSet.lcTktSheet)
SET ORDER TO TAG (loFormSet.lcTktSheet)
FOR lnType = 1 TO 7
DO WHILE SEEK(STR(lnType,1)+SPACE(1))
SCATTER MEMVAR
REPLACE cShowType WITH '1',;
TotStk    WITH lfGetTotStk(loFormSet)
SELECT (loFormSet.lcTktSheet)
IF !SEEK(STR(lnType,1)+'0')
INSERT INTO (loFormSet.lcTktSheet) (TYP,cShowType,Item,Dyelot) VALUES ;
(STR(lnType,1),'0',loFormSet.laSetups[2+lnType,2],lfGetDye(loFormSet,lnType))
ENDIF
ENDDO
ENDFOR
GO TOP
SELECT *,RECNO() AS nRecNo FROM (loFormSet.lcDetFile) INTO CURSOR tmpBomLine
SET ORDER TO TAG (loFormSet.lcDetFile) IN (loFormSet.lcDetFile)
SELECT tmpBomLine
SCAN
lcOprCode = IIF(EMPTY(cOprCode),MfgCode,cOprCode)
IF !EMPTY(lcOprCode) AND SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode,loFormSet.lcOprHdr);
AND SEEK(cInvType+Style+STR(LineNo,6),loFormSet.lcTmpTkt)
SELECT (loFormSet.lcTmpTkt)
lcFrstOpr   = IIF(nFrstOprSq=0 OR (VAL(EVALUATE(loFormSet.lcOprHdr+'.cOperSeq')) < nFrstOprSq),;
EVALUATE(loFormSet.lcOprHdr+'.cOprCode'),cFrstOpr)
lnFrstOprSq = IIF(nFrstOprSq=0 OR (VAL(EVALUATE(loFormSet.lcOprHdr+'.cOperSeq')) < nFrstOprSq),;
VAL(EVALUATE(loFormSet.lcOprHdr+'.cOperSeq')),nFrstOprSq)
REPLACE cFrstOpr   WITH lcFrstOpr ,;
nFrstOprSq WITH lnFrstOprSq
ENDIF
SELECT tmpBomLine
IF !SEEK(cInvType+Style+STR(LineNo,6)+cBomTyp+'0',loFormSet.lcDetFile)
SELECT (loFormSet.lcDetFile)
=SEEK(tmpBomLine.cInvType+tmpBomLine.Style+STR(tmpBomLine.LineNo,6))
LOCATE REST WHILE cInvType+Style+STR(LineNo,6) =;
tmpBomLine.cInvType+tmpBomLine.Style+STR(tmpBomLine.LineNo,6) FOR !EMPTY(cShowType)
llHideTit = FOUND()
INSERT INTO (loFormSet.lcDetFile) ;
(cInvType,Style,LineNo,cBomTyp,cShowType,Item,lHideTit,Dyelot) VALUES ;
(tmpBomLine.cInvType,tmpBomLine.Style,tmpBomLine.LineNo,tmpBomLine.cBomTyp,'0',;
loFormSet.laSetups[2+VAL(tmpBomLine.cBomTyp),2],llHideTit,lfGetDye(loFormSet,VAL(tmpBomLine.cBomTyp)))
ENDIF
SELECT (loFormSet.lcDetFile)
GOTO tmpBomLine.nRecNo
REPLACE cShowType WITH '1'

*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
STORE '/' TO lcExSign,lcUntSin
lcPosHdr = loFormSet.lcPosHdr
IF ISNULL(cCurrCode) OR EMPTY(cCurrCode)
DO CASE
CASE cCatgTyp = 'P'
lcCurrCode = IIF(EMPTY(&lcPosHdr..cPriceCur),oAriaApplication.BaseCurrency,&lcPosHdr..cPriceCur)
lnExRate   = IIF(&lcPosHdr..nPriceRat=0,1, &lcPosHdr..nPriceRat)
lnCurrUnit = IIF(&lcPosHdr..nCurrUnit=0,1, &lcPosHdr..nCurrUnit)
CASE INLIST(cCatgTyp,'D','M')
lcCurrCode = IIF(EMPTY(&lcPosHdr..cDutyCur),oAriaApplication.BaseCurrency,&lcPosHdr..cDutyCur)
lnExRate   = IIF(&lcPosHdr..nDutyRat=0,1, &lcPosHdr..nDutyRat)
lnCurrUnit = IIF(&lcPosHdr..nDCurUnit=0,1, &lcPosHdr..nDCurUnit)
OTHERWISE
lcCurrCode = oAriaApplication.BaseCurrency
lnExRate   = 1
lnCurrUnit = 1
ENDCASE
ELSE
lcCurrCode = cCurrCode
lnExRate   = IIF(ISNULL(nExRate) OR nExRate=0,1,nExRate)
lnCurrUnit = IIF(ISNULL(nCurrUnit) OR nCurrUnit=0,1,nCurrUnit)
ENDIF
lcExSign   = gfGetExSin(@lcUntSin, lcCurrCode)
lnEquAmount = ItemAmt &lcExSign lnExRate &lcUntSin lnCurrUnit
REPLACE NEQU_AMT WITH lnEquAmount
*N000587,1 WAM 12/01/2007 (End)

IF !SEEK(cInvType+Style+STR(LineNo,6)+cBomTyp+'2',loFormSet.lcDetFile)
INSERT INTO (loFormSet.lcDetFile) ;
(cInvType,Style,LineNo,cBomTyp,cShowType,Item) VALUES ;
(tmpBomLine.cInvType,tmpBomLine.Style,tmpBomLine.LineNo,tmpBomLine.cBomTyp,'2','***** '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_TOTAL,loFormSet.GetHeaderText("LANG_MFCSSH_TOTAL",loFormSet.HeaderAlias))+' :')
ENDIF
SELECT (loFormSet.lcDetFile)

*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*REPLACE ItemQty  WITH ItemQty + tmpBomLine.ItemQty,;
ItemAmt  WITH ItemAmt + tmpBomLine.ItemAmt
REPLACE ItemQty  WITH ItemQty + tmpBomLine.ItemQty,;
ItemAmt  WITH ItemAmt + tmpBomLine.ItemAmt,;
NEQU_AMT WITH NEQU_AMT + lnEquAmount
*N000587,1 WAM 12/01/2007 (End)
ENDSCAN
USE IN tmpBomLine
GO TOP IN (loFormSet.lcDetFile)
SET ORDER TO TAG (loFormSet.lcOprHdr) IN (loFormSet.lcOprHdr)
GO TOP IN (loFormSet.lcOprHdr)
SELECT (loFormSet.lcPosHdr)
IF !(loFormSet.lcTranType $ 'IDN' AND EMPTY(loFormSet.lcDyePO) AND loFormSet.llAutoGen)
REPLACE NICOST1 WITH lnTotEst1
ENDIF
REPLACE NICOST2  WITH lnTotEst2,;
NICOST3  WITH lnTotEst3,;
NICOST4  WITH lnTotEst4,;
NICOST5  WITH lnTotEst5,;
NICOST6  WITH lnTotEst6,;
NICOST7  WITH lnTotEst7,;
CLASTOPR WITH lcLastOpr
loFormSet.lcFirstOpr = EVALUATE(loFormSet.lcOprHdr+'.cOprCode')

IF loFormSet.lcTranType = 'I'
=lfRecCost(loFormSet.lcTranType,PO,IIF(Status<>'H',loFormSet.lcBOMLINE,loFormSet.lcDetFile),;
IIF(Status<>'H',loFormSet.lcCTKTBOM,loFormSet.lcTktSheet),loFormSet)
ENDIF

IF !loFormSet.llAutoGen
loFormSet.AlterMode('A')
IF loFormSet.AriaForm1.pgfCstSht.ActivePage = 2
=lfShOprHd(loFormSet)
ENDIF
ENDIF
ENDIF
IF !loFormSet.llAutoGen
loFormSet.ariaForm1.pgfCstSht.Page1.grdCstSht.SetFocus
loFormSet.ariaForm1.pgfCstSht.page1.grdCstSht.Refresh
ENDIF

*!*************************************************************
*! Name      : lfClsScr
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2004
*! Purpose   : Cancel modification
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfClsScr()
*!*************************************************************
FUNCTION lfClsScr
LPARAMETERS loFormSet
IF loFormSet.llPWInst .AND. (loFormSet.lcTranType = "M") .AND.  USED(loFormSet.lcPWCtkBom)
SELECT(loFormSet.lcPWCtkBom)
ZAP
ENDIF
IF loFormSet.llPWInst .AND. (loFormSet.lcTranType = "M") .AND. USED(loFormSet.lcDetLin)
SELECT(loFormSet.lcDetLin)
ZAP
ENDIF

*!*************************************************************
*! Name      : lfDelScr
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2004
*! Purpose   : Delete Cost Sheet
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDelScr()
*!*************************************************************
FUNCTION lfDelScr
LPARAMETERS loFormSet

PRIVATE laEmptyRec,laIssQty,loParentForm
loParentForm = loFormSet

IF EVALUATE(loFormSet.lcPosHdr+'.Status') = 'S'
*E300725,1 Message : 38059
*E300725,1 This ticket has been closed. Cannot delete cost sheet.
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38059B00000','ALERT',IIF(loFormSet.lcTranType='M',;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CUTTKT,;
loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))),'#'),;
IIF(loFormSet.lcTranType $ 'ID',IIF(EMPTY(loFormSet.lcDyePO),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,;
loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYEORD,;
loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias))),'#')),;
IIF(loFormSet.lcTranType='N',STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))),'#')))))
RETURN .F.
ENDIF
*! B612000,1 MMT 12/31/2019 User should not be able to cancel PO cost sheet if related PO is included in AP invoice[T20191213.0004][Start]
IF (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)
  IF !USED('ApVInvDt_A')
    =gfOpenTable('ApVInvDt','ITEM','SH','ApVInvDt_A')
  ENDIF
  IF gfSEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO'),'ApVInvDt_A','ITEM') OR IIF(loFormSet.lcTranType='I',gfSEEK('S'+EVALUATE(loFormSet.lcPosHdr+'.PO'),'ApVInvDt_A','ITEM'),.F.)
    =gfModalGen('INM36114B36000','DIALOG',IIF(loFormSet.lcTranType='M',;
	STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CUTTKT,;
	loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))),'#'),;
	IIF(loFormSet.lcTranType $ 'ID',IIF(EMPTY(loFormSet.lcDyePO),;
	STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,;
	loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))),'#'),;
	STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYEORD,;
	loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias))),'#')),;
	IIF(loFormSet.lcTranType='N',STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
	LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias))),'#'),;
	STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
	LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))),'#'))))+SPACE(01)+"# "+EVALUATE(loFormSet.lcPosHdr+'.PO'))
    RETURN .F.
  ENDIF
ENDIF
*! B612000,1 MMT 12/31/2019 User should not be able to cancel PO cost sheet if related PO is included in AP invoice[T20191213.0004][End]
llChkRecev = .F.
SELECT (loFormSet.lcPosLn)
*! B609360,1 MMT 07/22/2010 Cannot Delete PO Cost sheet if PO has lines received as cancelled[Start]
*LOCATE FOR TranCd $ '45'
LOCATE FOR TranCd = '4'
*! B609360,1 MMT 07/22/2010 Cannot Delete PO Cost sheet if PO has lines received as cancelled[End]
llChkRecev = FOUND()

IF EVALUATE(loFormSet.lcPosHdr+'.Receive') > 0 OR llChkRecev
*E300725,1 Message : 38060
*E300725,1 Pieces have been received on this ticket. Cannot delete cost sheet.
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38060B00000','ALERT',IIF(loFormSet.lcTranType='M',;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))),'#'),;
IIF(loFormSet.lcTranType $ 'ID',IIF(EMPTY(loFormSet.lcDyePO),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_PO,loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_DYEORD,loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias))),'#')),;
IIF(loFormSet.lcTranType='N',STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))),'#')))))

RETURN .F.
ENDIF

*! B608936,1 MMT 07/14/2009 Fix bug of Deleting PO Cost sheet even it has Shipemnt Cost sheet[Start]
IF EVALUATE(loFormSet.lcPosHdr+'.CSTYTYPE') = 'P' AND EVALUATE(loFormSet.lcPosHdr+'.CBUSDOCU') = 'P'
SELECT (loFormSet.lcPosLn)
LOCATE FOR TranCd = '3'
IF FOUND()
llHasShpCstSht = .F.
IF !USED('SHPRLFLD')
=gfOpenTable('SHPRLFLD','SHPRLFLD','SH')
ENDIF
SCAN FOR TranCd = '3'
IF gfSeek(SHIPNO+PO,'SHPRLFLD')
llHasShpCstSht = .T.
EXIT
ENDIF
ENDSCAN
IF llHasShpCstSht
**! B608936,1 Message : 34215
**! B608936,1 “PO# 999999 is assigned to a shipment cost sheet. Can’t delete.”
**! B608936,1 Button : 00000
**! B608936,1 Ok
=gfModalGen('TRM34215B00000','ALERT', EVALUATE(loFormSet.lcPosHdr+'.PO'))
RETURN .F.
ENDIF
ENDIF
ENDIF
*! B608936,1 MMT 07/14/2009 Fix bug of Deleting PO Cost sheet even it has Shipemnt Cost sheet[End]



SELECT (loFormSet.lcMfgOprHd)
LOCATE
IF !EOF()
*E300725,1 Message : 38058
*E300725,1 Manufacturing operation has been setup for this ticket.
*E300725,1 Button : 38002
*E300725,1 Proceed  Cancel
IF gfModalGen('TRM38058B38002','ALERT',IIF(loFormSet.lcTranType='M',;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))),'#'),;
IIF(loFormSet.lcTranType $ 'ID',IIF(EMPTY(loFormSet.lcDyePO),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYEORD,loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias))),'#')),;
IIF(loFormSet.lcTranType='N',STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))),'#')))))=2

RETURN .F.
ENDIF
ENDIF

m.cIMTyp = loFormSet.lcTranType
m.cTktNo = EVALUATE(loFormSet.lcPosHdr+'.PO')
IF !lfOpenSql('BOMCOST','BOMCOST','POBOMCLS','CIMTYP+CTKTNO',loFormSet)
RETURN .F.
ENDIF

SELECT BOMCOST
=lfSetIndex('BOMCOST','POBOMCLS','CIMTYP+CTKTNO+ACTUALIZE+CBOMTYPE+CINVTYPE+ITEM+MFGCODE+COPRCODE+CLOTNO+CISESSION+CRSESSION')
LOCATE
IF !EOF()
*E300725,1 Message : 38033
*E300725,1 There are cost items have been applied against this xxxx cost
*E300725,1 sheet. Deleting the cost sheet will return all issued items
*E300725,1 Button : 38002
*E300725,1 Proceed  Cancel
IF gfModalGen('QRM38033B38002','ALERT',IIF(loFormSet.lcTranType='M',;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))),'#'),;
IIF(loFormSet.lcTranType $ 'ID',IIF(EMPTY(loFormSet.lcDyePO),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYEORD,loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias))),'#')),;
IIF(loFormSet.lcTranType='N',STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias))),'#'),;
STRTRAN(LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ORDHDR,loFormSet.GetHeaderText("LANG_MFCSSH_ORDHDR",loFormSet.HeaderAlias))),'#')))))=2

RETURN .F.
ENDIF
ENDIF

lcGlYear   = loFormSet.lcGlYear
lcGlPeriod = loFormSet.lcGlPeriod
IF !CHECKPRD(oariaapplication.systemdate,'lcGlYear','lcGlPeriod','  ')
RETURN .F.
ENDIF
loFormSet.lcGlYear   = lcGlYear
loFormSet.lcGlPeriod = lcGlPeriod

PRIVATE lcOldSheet
lcOldSheet = gfTempName()
SELECT (loFormSet.lcTktSheet)
=AFIELDS(laFileStru)
=gfCrtTmp(lcOldSheet,@laFileStru)
SELECT (loFormSet.lcTktSheet)
SCATTER MEMVAR
SELECT (lcOldSheet)
APPEND BLANK
GATHER MEMVAR

=gfOpenFile(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [BEGIN]
*=gfOpenFile(oAriaApplication.DataDir+'STYINVJL',oAriaApplication.DataDir+'STYINVJL','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYINVJL',oAriaApplication.DataDir+'STYINVJL','SH')
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [END]


DECLARE laIssQty[9]
laIssQty = 0
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_MFCSSH_DELWTWIND WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DELWTWIND,loFormSet.GetHeaderText("LANG_MFCSSH_DELWTWIND",loFormSet.HeaderAlias)) WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


SELECT (loFormSet.lcBomLine)
DELETE ALL FOR cType = '1'
DELETE ALL FOR cType = '2'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_MFCSSH_DELWTWIN2 WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DELWTWIN2,loFormSet.GetHeaderText("LANG_MFCSSH_DELWTWIN2",loFormSet.HeaderAlias)) WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


*--- DOC.
*--- Get the current cuttkt record from StyInvJl.
*--- DOC.
SELECT StyInvJl
*E304013,1 MMT 07/24/2018 Convert STYINVJL to SQL[Start]
=gfSeek(EVALUATE(loFormSet.lcPosHdr+'.PO'),'StyInvJl','MFGOPR')
*E304013,1 MMT 07/24/2018 Convert STYINVJL to SQL[End]
SELECT * FROM StyInvJl ;
WHERE ctrcode+coprcode+clotno+ctrtype+style+cwarecode=EVALUATE(loFormSet.lcPosHdr+'.PO') ;
INTO CURSOR (loFormSet.lcIsslogFile) ;
ORDER BY ctrtype,ctrcode,coprcode,clotno,style,cwarecode,cDyelot

*B608919,1 MMT 07/05/2009 Fix bug of Using wrong records from Styinvjl while deleting cost sheet[Start]
*!*	LOCATE FOR ctrtype+ctrcode='1'+EVALUATE(loFormSet.lcPosHdr+'.PO')
*!*	IF FOUND()
*!*	  DO WHILE ctrtype+ctrcode= '1'+EVALUATE(loFormSet.lcPosHdr+'.PO')
*!*	    SCATTER MEMVAR
*!*	    =SEEK(m.Style,'Style')
*!*	    =SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
*!*	    lnIssCost = IIF(loFormSet.laSetups[12,2]='A',StyDye.Ave_Cost,Style.TotCost)
*!*
*!*	    SUM REST nStk1,nStk2,nStk3,nStk4,nStk5,nStk6,nStk7,nStk8  ;
*!*	        TO   laIssQty[1],laIssQty[2],laIssQty[3],laIssQty[4],laIssQty[5],;
*!*	             laIssQty[6],laIssQty[7],laIssQty[8]  ;
*!*	       WHILE ctrtype+ctrcode+coprcode+clotno+style+cwarecode+cDyelot = ;
*!*	             '1'+EVALUATE(loFormSet.lcPosHdr+'.PO')+m.cOprCode+m.cLotNo+m.Style+m.cWareCode+m.cDyelot
*!*
*!*	    laIssQty[1] = -1*MIN(laIssQty[1],0)
*!*	    laIssQty[2] = -1*MIN(laIssQty[2],0)
*!*	    laIssQty[3] = -1*MIN(laIssQty[3],0)
*!*	    laIssQty[4] = -1*MIN(laIssQty[4],0)
*!*	    laIssQty[5] = -1*MIN(laIssQty[5],0)
*!*	    laIssQty[6] = -1*MIN(laIssQty[6],0)
*!*	    laIssQty[7] = -1*MIN(laIssQty[7],0)
*!*	    laIssQty[8] = -1*MIN(laIssQty[8],0)
*!*	    laIssQty[9] = laIssQty[1]+laIssQty[2]+laIssQty[3]+laIssQty[4]+;
*!*	                  laIssQty[5]+laIssQty[6]+laIssQty[7]+laIssQty[8]
*!*	    IF laIssQty[9] > 0
*!*	      =lfIssRetSty('','',loFormSet.lcInvType,m.Style,m.cWareCode,m.cDyelot,@laIssQty,lnIssCost,;
*!*	                   m.cOprCode,m.cLotNo,oariaapplication.systemdate,loFormSet.lcSession,'',.T.)
*!*	    ENDIF
*!*	    SELECT (loFormSet.lcIsslogFile)
*!*	  ENDDO
*!*	ENDIF
*B608919,1 MMT 07/05/2009 Fix bug of Using wrong records from Styinvjl while deleting cost sheet[End]

PRIVATE laIssTypeI
DIMENSION laIssTypeI[9]
laIssTypeI=0
LOCATE FOR ctrtype+ctrcode='I'+EVALUATE(loFormSet.lcPosHdr+'.PO')
IF FOUND()
DO WHILE ctrtype+ctrcode= 'I'+EVALUATE(loFormSet.lcPosHdr+'.PO')
SCATTER MEMVAR
=SEEK(m.Style,'Style')
=SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
lnIssCost = IIF(loFormSet.laSetups[12,2]='A',StyDye.Ave_Cost,Style.TotCost)

SUM REST nStk1,nStk2,nStk3,nStk4,nStk5,nStk6,nStk7,nStk8  ;
TO   laIssTypeI[1],laIssTypeI[2],laIssTypeI[3],laIssTypeI[4],;
laIssTypeI[5],laIssTypeI[6],laIssTypeI[7],laIssTypeI[8]  ;
WHILE ctrtype+ctrcode+coprcode+clotno+style+cwarecode+cDyelot = ;
'I'+EVALUATE(loFormSet.lcPosHdr+'.PO')+m.cOprCode+m.cLotNo+m.Style+m.cWareCode+m.cDyelot

laIssQty[1] = -1*MIN(laIssTypeI[1],0)
laIssQty[2] = -1*MIN(laIssTypeI[2],0)
laIssQty[3] = -1*MIN(laIssTypeI[3],0)
laIssQty[4] = -1*MIN(laIssTypeI[4],0)
laIssQty[5] = -1*MIN(laIssTypeI[5],0)
laIssQty[6] = -1*MIN(laIssTypeI[6],0)
laIssQty[7] = -1*MIN(laIssTypeI[7],0)
laIssQty[8] = -1*MIN(laIssTypeI[8],0)
laIssQty[9] = laIssQty[1]+laIssQty[2]+laIssQty[3]+laIssQty[4]+laIssQty[5]+laIssQty[6]+laIssQty[7]+laIssQty[8]

IF laIssQty[9] > 0
=lfIssRetSty('','',loFormSet.lcInvType,m.Style,m.cWareCode,m.cDyelot,@laIssQty,lnIssCost,;
m.cOprCode,m.cLotNo,oariaapplication.systemdate,loFormSet.lcSession,'',.T.)
ENDIF
SELECT (loFormSet.lcIsslogFile)
ENDDO

laIssQty[1] = laIssQty[1] + laIssTypeI[1]
laIssQty[2] = laIssQty[2] + laIssTypeI[2]
laIssQty[3] = laIssQty[3] + laIssTypeI[3]
laIssQty[4] = laIssQty[4] + laIssTypeI[4]
laIssQty[5] = laIssQty[5] + laIssTypeI[5]
laIssQty[6] = laIssQty[6] + laIssTypeI[6]
laIssQty[7] = laIssQty[7] + laIssTypeI[7]
laIssQty[8] = laIssQty[8] + laIssTypeI[8]
laIssQty[9] = laIssQty[9] + laIssTypeI[9]

ENDIF


IF OCCURS('MA',oAriaApplication.CompanyInstalledModules) <> 0

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
lcInvType ='0002'
lenclrlen  = LEN(gfitemmask("PN", "", lcInvType))
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [END]
DECLARE laIssQty[9]
laIssQty = 0
m.cTrCode  = EVALUATE(loFormSet.lcPosHdr+'.PO')
m.cBusDocu = loFormSet.lcBusDocu
m.cStyType = loFormSet.lcStyType
IF !lfOpenSql('ITEMJRNL',loFormSet.lcIsslogFile,'MFGOPR','CTRCODE+CBUSDOCU+CSTYTYPE',loFormSet)
RETURN .F.
ENDIF
=lfSetIndex(loFormSet.lcIsslogFile,loFormSet.lcIsslogFile,;
'cTrCode+cOprCode+cLotNo+cTrType+cInvType+Style+cWarecode+cDyelot+cRSession+cISession')
SELECT (loFormSet.lcIsslogFile)
LOCATE
IF !EOF()
IF INLIST(loFormSet.laSetups[11,2],'L','F','I')
DO WHILE !EOF()
SCATTER MEMVAR
=lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loFormSet)
*!*          *B607465,1 AMH Consider case of custom isuue/return by roll [Start]
*!*          IF ASCAN(laEvntTrig,PADR("GETRET",10)) <> 0
*!*            =gfDoTriger("POCSSH",PADR("GETRET",10))
*!*          ELSE
SELECT FABRIC
LOCATE

SELECT (loFormSet.lcIsslogFile)
SUM REST nStk1,nStk2,nStk3,nStk4,nStk5,nStk6,nStk7,nStk8  ;
TO   laIssQty[1],laIssQty[2],laIssQty[3],laIssQty[4],laIssQty[5],;
laIssQty[6],laIssQty[7],laIssQty[8]  ;
WHILE ctrCode+coprcode+clotno+ctrtype+cInvType+Style+cwarecode+cDyelot+cRSession+cISession=;
EVALUATE(loFormSet.lcPosHdr+'.PO')+m.cOprCode+m.cLotNo+'9'+m.cInvType+m.Style+m.cWareCode+m.cDyelot+m.cRSession+m.cISession
*!*          ENDIF
*!*          *B607465,1 AMH [End]

laIssQty[1] = MIN(laIssQty[1],0)
laIssQty[2] = MIN(laIssQty[2],0)
laIssQty[3] = MIN(laIssQty[3],0)
laIssQty[4] = MIN(laIssQty[4],0)
laIssQty[5] = MIN(laIssQty[5],0)
laIssQty[6] = MIN(laIssQty[6],0)
laIssQty[7] = MIN(laIssQty[7],0)
laIssQty[8] = MIN(laIssQty[8],0)
laIssQty[9] = laIssQty[1]+laIssQty[2]+laIssQty[3]+laIssQty[4]+;
laIssQty[5]+laIssQty[6]+laIssQty[7]+laIssQty[8]

IF laIssQty[9] < 0
IF loFormSet.laSetups[13,2]='Y' .AND. Fabric.lTrkRolls

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
* =lfvMatRolls('R',m.cRSession,m.cISession,m.cFabric,m.cColor,;
m.cWareCode,m.cDyelot,lnIssued,lnIssued)
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	         =lfvMatRolls('R',m.cRSession,m.cISession,SUBSTR(m.style,1,7),right(m.style,lenclrlen  ),;
*!*	                         m.cWareCode,m.cDyelot,laIssQty[9],laIssQty[9])
=lfvMatRolls('R',m.cRSession,m.cISession,SUBSTR(m.style,1,7),right(m.style,lenclrlen  ),;
m.cWareCode,m.cDyelot,laIssQty[9],laIssQty[9],m.style)

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [T20090122.END]
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

ENDIF
=lfIssRetFab('','',m.cInvType,m.Style,m.cWareCode,m.cDyelot,@laIssQty,;
m.nUntCstBuy,m.cOprCode,m.cLotNo,oariaapplication.systemdate,m.cRSession,m.cISession,.T.)
ENDIF
SELECT (loFormSet.lcIsslogFile)
ENDDO
ELSE
DO WHILE !EOF()
SCATTER MEMVAR
=lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loFormSet)
SELECT FABRIC
LOCATE

=lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE',loFormSet)
SELECT FABDYE
LOCATE FOR Dyelot = SPACE(10)
*lnIssCost = IIF(loFormSet.laSetups[11,2]='S',Fabric.CostBuy,;
IIF(loFormSet.laSetups[2,2]='Y',FabDye.nAveCstBuy,Fabric.nAveCstBuy))

SELECT (loFormSet.lcIsslogFile)
SUM REST nStk1,nStk2,nStk3,nStk4,nStk5,nStk6,nStk7,nStk8,nTotStk,nStkVal  ;
TO   laIssQty[1],laIssQty[2],laIssQty[3],laIssQty[4],laIssQty[5],;
laIssQty[6],laIssQty[7],laIssQty[8],laIssQty[9],lnIssCost  ;
WHILE ctrCode+coprcode+clotno+ctrtype+cInvType+Style+cwarecode+cDyelot+cRSession+cISession=;
EVALUATE(loFormSet.lcPosHdr+'.PO')+m.cOprCode+m.cLotNo+'9'+m.cInvType+m.Style+m.cWareCode+m.cDyelot

laIssQty[1] = MIN(laIssQty[1],0)
laIssQty[2] = MIN(laIssQty[2],0)
laIssQty[3] = MIN(laIssQty[3],0)
laIssQty[4] = MIN(laIssQty[4],0)
laIssQty[5] = MIN(laIssQty[5],0)
laIssQty[6] = MIN(laIssQty[6],0)
laIssQty[7] = MIN(laIssQty[7],0)
laIssQty[8] = MIN(laIssQty[8],0)
laIssQty[9] = laIssQty[1]+laIssQty[2]+laIssQty[3]+laIssQty[4]+;
laIssQty[5]+laIssQty[6]+laIssQty[7]+laIssQty[8]

IF laIssQty[9] < 0
lnIssCost = lnIssCost / laIssQty[9]
=lfIssRetFab('','',m.cInvType,m.Style,m.cWareCode,m.cDyelot,@laIssQty,;
lnIssCost,m.cOprCode,m.cLotNo,oariaapplication.systemdate,gfsequence('GLSession'),SPACE(6),.T.)
ENDIF
SELECT (loFormSet.lcIsslogFile)
ENDDO
ENDIF
ENDIF
USE IN (loFormSet.lcIsslogFile)
ENDIF

SELECT BOMCOST
IF SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+'N')
DO WHILE cimtyp+ctktno+actualize+cbomtype+cInvType+item+mfgcode+coprcode+clotno+cisession+crsession=;
loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+'N'

SCATTER MEMVAR
SUM REST nTotQty TO lnIssued ;
WHILE cimtyp+ctktno+actualize+cbomtype+cInvType+item+mfgcode+coprcode+clotno+cisession+crsession=;
m.cimtyp+m.ctktno+m.actualize+m.cbomtype+m.cInvType+m.item+m.mfgcode+m.coprcode+m.clotno+m.cISession
llFound = .F.
IF !EMPTY(m.cInvType+m.Item) AND m.cCostType='T'
m.Style = m.Item
IF lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loFormSet)
SELECT FABRIC
LOCATE
llFound = !EOF()
ENDIF
ENDIF

IF (EMPTY(m.cInvType+m.Item) OR llFound) AND lnIssued > 0
=lfIssRetMfg(m.cbomtype,m.cCostType,m.cInvType,m.Item,m.MfgCode,m.cWareCode,;
m.cDyelot,-lnIssued,m.nUnitCst,oariaapplication.systemdate,m.cOprCode,;
m.cLotNo,gfsequence('GLSession'),'',.T.)
ENDIF
SELECT BOMCOST
ENDDO
ENDIF

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_MFCSSH_DELWTWIN3 WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DELWTWIN3,loFormSet.GetHeaderText("LANG_MFCSSH_DELWTWIN3",loFormSet.HeaderAlias)) WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

SELECT BOMCOST
DELETE ALL

SELECT (loFormSet.lcMFGOPRHD)
DELETE ALL

SELECT (loFormSet.lcMFGOPRDT)
DELETE ALL

*!*  *--- SSH
*!*  IF ASCAN(laEvntTrig , PADR('DLTRKHDR',10)) <> 0
*!*    =gfDoTriger('MFCSSH',PADR('DLTRKHDR',10))
*!*  ENDIF
*!*  *--- SSH

SELECT (loFormSet.lcCTktBom)
DELETE ALL

*--- PwCtkBom Key == cuttkt+mfgcode+coprcode+STR(nlineno,6)
llDummy = loFormSet.llPWInst .AND. loFormSet.lcTranType = "M" .AND. lfPwRem(EVALUATE(loFormSet.lcPosHdr+'.PO'),'',loFormSet)

SELECT (loFormSet.lcPosHdr)
REPLACE NICOST1    WITH 0,;
NICOST2    WITH 0,;
NICOST3    WITH 0,;
NICOST4    WITH 0,;
NICOST5    WITH 0,;
NICOST6    WITH 0,;
NICOST7    WITH 0,;
NFCOST1    WITH 0,;
NFCOST2    WITH 0,;
NFCOST3    WITH 0,;
NFCOST4    WITH 0,;
NFCOST5    WITH 0,;
NFCOST6    WITH 0,;
NFCOST7    WITH 0
REPLACE NACT_COST1 WITH 0,;
NACT_COST2 WITH 0,;
NACT_COST3 WITH 0,;
NACT_COST4 WITH 0,;
NACT_COST5 WITH 0,;
NACT_COST6 WITH 0,;
NACT_COST7 WITH 0,;
NFACTCOST1 WITH 0,;
NFACTCOST2 WITH 0,;
NFACTCOST3 WITH 0,;
NFACTCOST4 WITH 0,;
NFACTCOST5 WITH 0,;
NFACTCOST6 WITH 0,;
NFACTCOST7 WITH 0
REPLACE CITEMWARE  WITH cWareCode,;
CMATWARE   WITH cWareCode,;
STATUS     WITH 'H'
*! B611745,1 Heba HMS, 18/03/2018 - Aria 5 - Error with purchase order [T20190313.0002 ][Begin]
=gfAdd_Info(loFormSet.lcPosHdr)
*! B611745,1 Heba HMS, 18/03/2018 - Aria 5 - Error with purchase order [T20190313.0002 ][End]
LOCAL lcItemFile,lcFldType,lnI,lcI
lcItemFile = IIF(loFormSet.lcTranType='T','FABRIC','STYLE')
lnI        = 0
lcI        = ''
*N037452,1 WAM 04/22/2005 Handle Cutting Ticket Cost SHeet
*lcFldType  = IIF(EMPTY(loFormSet.lcDyePO),'I','M')
*lnStart    = IIF(EMPTY(loFormSet.lcDyePO),2,1)
lnStart    = IIF(!EMPTY(loFormSet.lcDyePO),2,1)
DO CASE
CASE loFormSet.lcTranType = 'N'
lcFldType  = 'I'
*N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[T20110914.0019][Start]
*CASE loFormSet.lcTranType = 'D'
CASE loFormSet.lcTranType $ 'DT'
*N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[T20110914.0019][END]
lcFldType  = 'M'
OTHERWISE
lcFldType  = loFormSet.lcTranType
ENDCASE
*N037452,1 WAM 04/22/2005 (End)

SELECT (loFormSet.lcPOSLN)
LOCATE
SCAN
IF loFormSet.lcTranType = 'T'
m.cInvType = cInvType
m.Style    = Style
=lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loFormSet)
ELSE
=SEEK(Style,'Style')
ENDIF

SELECT (loFormSet.lcPOSLN)
FOR lnI = 1 TO 7
lcI = STR(lnI,1)
*!* B611451, HMS -Aria4xp -Deleting po cost sheet creates discount [T20170927.0031] [Begin]
*!*	IF lnI >= lnStart
*!*	REPLACE ('nFCost'+lcI) WITH EVALUATE(lcItemFile+'.n'+lcFldType+'Cost'+lcI)
*!*	DO CASE
*!*	CASE loFormSet.laSetups[14+lnI,2] = 'P'
*!*	REPLACE ('nICost'+lcI) WITH EVALUATE('nFCost'+lcI)*;
*!*	EVALUATE('1 '+loFormSet.lcPExSign+' '+loFormSet.lcPOSHDR+'.nPriceRat '+;
*!*	loFormSet.lcPUntSin+' '+loFormSet.lcPOSHDR+'.nCurrUnit')
*!*	CASE loFormSet.laSetups[14+lnI,2] $ 'MD'
*!*	REPLACE ('nICost'+lcI) WITH EVALUATE('nFCost'+lcI)*;
*!*	EVALUATE('1 '+loFormSet.lcDExSign+' '+loFormSet.lcPOSHDR+'.nDutyRat '+;
*!*	loFormSet.lcDUntSin+' '+loFormSet.lcPOSHDR+'.nDCurUnit')
*!*	OTHERWISE
*!*	REPLACE ('nICost'+lcI) WITH EVALUATE('nFCost'+lcI)
*!*	ENDCASE
*!*	ENDIF

* *!* B611451, HMS -Aria4xp -Deleting po cost sheet creates discount [T20170927.0031] [End]

SELECT (loFormSet.lcPosHdr)
*N037452,1 WAM 04/22/2005 Handle Cutting Ticket Cost SHeet
*REPLACE ('nICost'+lcI) WITH EVALUATE(loFormSet.lcPosLn+'.TotQty*'+loFormSet.lcPosLn+'.nICost'+lcI)
*REPLACE ('nFCost'+lcI) WITH EVALUATE(loFormSet.lcPosLn+'.TotQty*'+loFormSet.lcPosLn+'.nFCost'+lcI)

REPLACE ('nICost'+lcI) WITH EVALUATE('nICost'+lcI) + EVALUATE(loFormSet.lcPosLn+'.TotQty*'+loFormSet.lcPosLn+'.nICost'+lcI) ,;
('nFCost'+lcI) WITH EVALUATE('nFCost'+lcI) + EVALUATE(loFormSet.lcPosLn+'.TotQty*'+loFormSet.lcPosLn+'.nFCost'+lcI)
SELECT (loFormSet.lcPOSLN)
*N037452,1 WAM 04/22/2005 (End)

ENDFOR
ENDSCAN

SELECT (loFormSet.lcPosHdr)
REPLACE PoTotal WITH nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7
REPLACE Pcs_Act WITH 0

DECLARE laTableUpdate[7,2]
laTableUpdate[1,1] = loFormSet.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'

laTableUpdate[2,1] = loFormSet.lcBomLine
laTableUpdate[2,2] = 'BOMLINE'

laTableUpdate[3,1] = loFormSet.lcMfgOprHd
laTableUpdate[3,2] = 'MFGOPRHD'

laTableUpdate[4,1] = loFormSet.lcPosHdr
laTableUpdate[4,2] = 'POSHDR'

laTableUpdate[5,1] = loFormSet.lcPosLn
laTableUpdate[5,2] = 'POSLN'

laTableUpdate[6,1] = loFormSet.lcMfgOprDt
laTableUpdate[6,2] = 'MFGOPRDT'

laTableUpdate[7,1] = 'BOMCOST'
laTableUpdate[7,2] = 'BOMCOST'

=lfTableUpdate(loFormSet)

IF USED(lcOldSheet)
USE IN (lcOldSheet)
ENDIF

WAIT CLEAR
loFormSet.ChangeMode('V')
oariaapplication.otoolbar.ButtonRefresh()
oariaapplication.otoolbar.NavRefresh()
RETURN .F.

*!*************************************************************
*! Name      : lfvZoom
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/04/2004
*! Purpose   : Zoom In/Zoom Out operation details browse
*!*************************************************************
*! Calls     : lfBrowLots
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvZoom()
*!*************************************************************
FUNCTION lfvZoom
LPARAMETERS loFormSet

loFormSet.llZoom = !loFormSet.llZoom
LOCAL lnNewHeight
WITH loFormSet.AriaForm1.pgfCstSht.Page2
IF loFormSet.llZoom
lnNewHeight = .grdLots.Top - .grdOperations.Top + .grdLots.Height
.grdLots.Tag = STR(.grdLots.Top)
.grdLots.Top = .grdOperations.Top
.grdLots.Height = lnNewHeight
.grdOperations.Visible = .F.
.cmdModOpr.Enabled = .F.
.cmdNewLot.Enabled = .F.
=lfBrowLots(loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdZoom.Caption = LANG_MFCSSH_ZOOMOUT
.cmdZoom.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ZOOMOUT,loFormSet.GetHeaderText("LANG_MFCSSH_ZOOMOUT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
.grdLots.Top = VAL(.grdLots.Tag)
lnNewHeight = .grdLots.Height - .grdLots.Top + .grdOperations.Top
.grdLots.Height = lnNewHeight
.grdOperations.Visible = .T.
.cmdModOpr.Enabled = .T.
.cmdNewLot.Enabled = .T.
=lfBrowLots(loFormSet)
=lfShOprHd(loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdZoom.Caption = LANG_MFCSSH_ZOOMIN
.cmdZoom.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ZOOMIN,loFormSet.GetHeaderText("LANG_MFCSSH_ZOOMIN",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
ENDWITH

*!*************************************************************
*! Name      : lfvContCode
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/30/2004
*! Purpose   : Validate contractor
*!*************************************************************
*! Calls     : gfApVnBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvContCode()
*!*************************************************************
FUNCTION lfvContCode
LPARAMETERS llBrowse

LOCAL lnAlias
lnAlias = SELECT(0)
=gfOpenFile(oAriaApplication.DataDir+'ApVendor',oAriaApplication.DataDir+'VenCode','SH')
IF llBrowse .OR. (!EMPTY(lcContCode) AND !SEEK(lcContCode,'ApVendor'))
=gfApVnBrow(@lcContCode,.F.,'C')
ENDIF
lcContName = IIF(EMPTY(lcContCode),'',ApVendor.cVenComp)
llBrowse = .F.
USE IN 'ApVendor'
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvModOpr
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/04/2004
*! Purpose   : Modify Operation
*!*************************************************************
*! Calls     : MFMODOPR.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvModOpr()
*!*************************************************************
FUNCTION lfvModOpr
LPARAMETERS loFormSet

PRIVATE lcContCode,lcOprComnt,llInHouse
SELECT (loFormSet.lcOprHdr)
lcContCode = cContCode
lcOprComnt = cOprComnt
llInHouse  = lInHouse
lcContName = cContName
lcOperSeq  = cOperSeq

SELECT (loFormSet.lcMfgOprDt)
LOCATE FOR cOprCode = EVALUATE(loFormSet.lcOprHdr+'.cOprCode')
llModSeq   = !FOUND()
SELECT (loFormSet.lcOprHdr)

PRIVATE loParentForm
loParentForm = loFormSet
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFMODOPR.SCX")
=gfCallForm('MFMODOPR')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
STORE .F. TO loFormSet.llOnlyOne
GO TOP IN (loFormSet.lcOprHdr)
loFormSet.lcFirstOpr = EVALUATE(loFormSet.lcOprHdr+'.cOprCode')
SELECT (loFormSet.lcTmpTkt)
REPLACE ALL cFrstOpr   WITH loFormSet.lcFirstOpr,;
nFrstOprSq WITH VAL(EVALUATE(loFormSet.lcOprHdr+'.cOperSeq'))
SELECT (loFormSet.lcOprHdr)
=lfShOprHd(loFormSet)

*!*************************************************************
*! Name      : lfvUpOpr
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/05/2004
*! Purpose   : Update Operation Modification
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvUpOpr()
*!*************************************************************
FUNCTION lfvUpOpr
LPARAMETERS llActualize


SELECT (loParentForm.lcMfgOprHd)
LOCATE FOR cOprCode = EVALUATE(loParentForm.lcOprHdr+'.coprcode')
IF FOUND()
REPLACE cContCode WITH lcContCode ,;
cOprComnt WITH lcOprComnt ,;
lInHouse  WITH llInHouse  ,;
cContName WITH lcContName ,;
cOperSeq  WITH lcOperSeq

DECLARE laTableUpdate[1,2]
laTableUpdate[1,1] = loParentForm.lcMfgOprHd
laTableUpdate[1,2] = 'MFGOPRHD'

=lfTableUpdate(loParentForm)
ENDIF

SELECT (loParentForm.lcOprHdr)
REPLACE cContCode WITH lcContCode ,;
cOprComnt WITH lcOprComnt ,;
lInHouse  WITH llInHouse  ,;
cContName WITH lcContName ,;
cOperSeq  WITH lcOperSeq
=IIF(llActualize,lfvActualize(),.T.)

IF loParentForm.lcTranType='M'
LOCAL lnAlias
lnAlias=SELECT(0)
*--- DOC.
*--- Seek and scan for operation and budget record [Trans type ='1']
*--- DOC.

*B607846,1 MMT 11/29/2006 Fix bug of not updating Poshdr with new operation code when sequance changed[Start]
SELECT (loParentForm.lcBomLine)
*B607846,1 MMT 11/29/2006 Fix bug of not updating Poshdr with new operation code when sequance changed[End]

=SEEK('M'+'1'+EVALUATE(loParentForm.lcPosHdr+'.PO'))
SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+cInvType+style+cInvTypC+item+mfgcode=;
'M'+'1'+EVALUATE(loParentForm.lcPosHdr+'.PO') FOR !lVoid
lcOprCode = IIF(EMPTY(cOprCode),MfgCode,cOprCode)
IF !EMPTY(lcOprCode)
SELECT (loParentForm.lcMfgOprHd)
LOCATE FOR cOprCode = lcOprCode
IF FOUND() AND SEEK(EVALUATE(loParentForm.lcBomLine+'.cInvType')+EVALUATE(loParentForm.lcBomLine+'.Style')+;
STR(EVALUATE(loParentForm.lcBomLine+'.LineNo'),6),loParentForm.lcTmpTkt)
SELECT (loParentForm.lcTmpTkt)

*B607846,1 MMT 11/29/2006 Fix bug of not updating Poshdr with new operation code when sequance changed[Start]
*SELECT MIN(cOperSeq) FROM (loParentForm.lcMFGOPRHD) WHERE coprcode=EVALUATE(loParentForm.lcPosHdr+'.PO') INTO ARRAY laTmp1
SELECT MIN(VAL(cOperSeq)) FROM (loParentForm.lcMFGOPRHD) WHERE ctktno = EVALUATE(loParentForm.lcPosHdr+'.PO') INTO ARRAY laTmp1
*B607846,1 MMT 11/29/2006 Fix bug of not updating Poshdr with new operation code when sequance changed[End]


IF _TALLY > 0

*B607846,1 MMT 11/29/2006 Fix bug of not updating Poshdr with new operation code when sequance changed[Start]
*SELECT coprCode FROM (loParentForm.lcMFGOPRHD) WHERE coprcode=EVALUATE(loParentForm.lcPosHdr+'.PO') AND cOperSeq = laTmp1[1] INTO ARRAY laTmp2
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[Start]
*SELECT coprCode FROM (loParentForm.lcMFGOPRHD) WHERE ctktno = EVALUATE(loParentForm.lcPosHdr+'.PO') AND cOperSeq = ALLTRIM(STR(laTmp1[1])) INTO ARRAY laTmp2
SELECT coprCode FROM (loParentForm.lcMFGOPRHD) WHERE ctktno = EVALUATE(loParentForm.lcPosHdr+'.PO') AND VAL(cOperSeq) = laTmp1[1] INTO ARRAY laTmp2
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[End]
*B607846,1 MMT 11/29/2006 Fix bug of not updating Poshdr with new operation code when sequance changed[End]
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[Start]
IF _TALLY > 0
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[End]
lcFrstOpr = laTmp2[1]

*B607846,1 MMT 11/29/2006 Fix bug of not updating Poshdr with new operation code when sequance changed[Start]
*lnFrstOprSq = VAL(laTmp1[1])
lnFrstOprSq = laTmp1[1]
*B607846,1 MMT 11/29/2006 Fix bug of not updating Poshdr with new operation code when sequance changed[End]

REPLACE cFrstOpr   WITH lcFrstOpr ,;
nFrstOprSq WITH lnFrstOprSq
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[Start]
ENDIF
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[End]
ENDIF
ENDIF
ENDIF
ENDSCAN

*B607846,1 MMT 11/29/2006 Fix bug of not updating Poshdr with new operation code when sequance changed[Start]
IF loParentForm.ActiveMode = "A"
SELECT Max(VAL(cOperSeq)) FROM (loParentForm.lcOprHdr) WHERE ctktno = EVALUATE(loParentForm.lcPosHdr+'.PO') INTO ARRAY laTmp1
IF _TALLY > 0
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[Start]
*SELECT coprCode FROM (loParentForm.lcOprHdr) WHERE ctktno = EVALUATE(loParentForm.lcPosHdr+'.PO') AND cOperSeq = ALLTRIM(STR(laTmp1[1])) INTO ARRAY laTmp2
SELECT coprCode FROM (loParentForm.lcOprHdr) WHERE ctktno = EVALUATE(loParentForm.lcPosHdr+'.PO') AND VAL(cOperSeq) = laTmp1[1] INTO ARRAY laTmp2
IF _TALLY > 0
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[End]
REPLACE clastopr WITH laTmp2[1] IN (loParentForm.lcPosHdr)
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[Start]
ENDIF
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[End]
ENDIF
ENDIF

IF loParentForm.ActiveMode = "V"
SELECT Max(VAL(cOperSeq)) FROM (loParentForm.lcMFGOPRHD) WHERE ctktno = EVALUATE(loParentForm.lcPosHdr+'.PO') INTO ARRAY laTmp1
IF _TALLY > 0
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[Start]
*SELECT coprCode FROM (loParentForm.lcMFGOPRHD) WHERE ctktno = EVALUATE(loParentForm.lcPosHdr+'.PO') AND cOperSeq = ALLTRIM(STR(laTmp1[1])) INTO ARRAY laTmp2
SELECT coprCode FROM (loParentForm.lcMFGOPRHD) WHERE ctktno = EVALUATE(loParentForm.lcPosHdr+'.PO') AND VAL(cOperSeq) = laTmp1[1] INTO ARRAY laTmp2
IF _TALLY > 0
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[Start]
REPLACE clastopr WITH laTmp2[1] IN (loParentForm.lcPosHdr)
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[Start]
ENDIF
*! B609156,1 MMT 02/24/2010 Fix bug of error While Modifying MFG Operation Details[End]
laTableUpdate[1,1] = loParentForm.lcPosHdr
laTableUpdate[1,2] = 'POSHDR'
=lfTableUpdate(loParentForm)
ENDIF
ENDIF
*B607846,1 MMT 11/29/2006 Fix bug of not updating Poshdr with new operation code when sequance changed[End]
SELECT (lnAlias)
ENDIF

*!*************************************************************
*! Name      : lfvIssCstItm
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Issue lot cost items
*!*************************************************************
*! Calls     : lfLotCstItm(),MFISSLT.SPX
*!*************************************************************
*! Parameters: lcOprCode : Operation
*!             lcLotNo   : Lot#
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvIssCstItm()
*!*************************************************************
FUNCTION lfvIssCstItm
LPARAMETERS lcOprCode,lcLotNo,loFormSet

PRIVATE laCstTypes,puCstTypes,lnAlias,lnRecNo

lnAlias = SELECT(0)
=gfOpenFile(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
*--- DOC.
*--- Get the the 7 cost element types and labels
*--- DOC.
puCstTypes = 1
IF loFormSet.lcTranType='T'
DECLARE laCstTypes[5]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laCstTypes[1] = LANG_MFCSSH_ALL
laCstTypes[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ALL,loFormSet.GetHeaderText("LANG_MFCSSH_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laCstTypes[2] = loFormSet.laSetups[3,2]
laCstTypes[3] = loFormSet.laSetups[4,2]
laCstTypes[4] = loFormSet.laSetups[5,2]
laCstTypes[5] = loFormSet.laSetups[6,2]
ELSE
IF loFormSet.lcTranType = 'N'
DECLARE laCstTypes[2]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laCstTypes[1] = LANG_MFCSSH_ALL
laCstTypes[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ALL,loFormSet.GetHeaderText("LANG_MFCSSH_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laCstTypes[2] = loFormSet.laSetups[3,2]
LOCAL lnI,lnCstCnt
lnCstCnt = 2
FOR lnI = 2 TO 7
IF loFormSet.laSetups[14+lnI,2] $ 'MD'
lnCstCnt = lnCstCnt + 1
DECLARE laCstTypes[lnCstCnt]
laCstTypes[lnCstCnt] = loFormSet.laSetups[2+lnI,2]
ENDIF
ENDFOR
ELSE
DECLARE laCstTypes[8]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laCstTypes[1] = LANG_MFCSSH_ALL
laCstTypes[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ALL,loFormSet.GetHeaderText("LANG_MFCSSH_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laCstTypes[2] = loFormSet.laSetups[3,2]
laCstTypes[3] = loFormSet.laSetups[4,2]
laCstTypes[4] = loFormSet.laSetups[5,2]
laCstTypes[5] = loFormSet.laSetups[6,2]
laCstTypes[6] = loFormSet.laSetups[7,2]
laCstTypes[7] = loFormSet.laSetups[8,2]
laCstTypes[8] = loFormSet.laSetups[9,2]
ENDIF
ENDIF

PRIVATE lcOldSheet
lcOldSheet = gfTempName()
SELECT (loFormSet.lcTktSheet)
=AFIELDS(laFileStru)
=gfCrtTmp(lcOldSheet,@laFileStru)
SELECT (loFormSet.lcTktSheet)
SCATTER MEMVAR
SELECT (lcOldSheet)
APPEND BLANK
GATHER MEMVAR

SELECT (loFormSet.lcCTKTBOM)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+3,18]
laFileStru[lnFileStru+1,1] = 'lSelect'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'nIssue'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 12
laFileStru[lnFileStru+2,4] = 3
laFileStru[lnFileStru+3,1] = 'nIssPcnt'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 6
laFileStru[lnFileStru+3,4] = 2

LOCAL lnI
FOR lnI = 7 TO 16
STORE '' TO laFileStru[lnFileStru+1,lnI],laFileStru[lnFileStru+2,lnI],laFileStru[lnFileStru+3,lnI]
ENDFOR
STORE 0 TO laFileStru[lnFileStru+1,17],laFileStru[lnFileStru+2,17],laFileStru[lnFileStru+3,17],;
laFileStru[lnFileStru+1,18],laFileStru[lnFileStru+2,18],laFileStru[lnFileStru+3,18]

=gfCrtTmp(loFormSet.lcIssLtFile,@laFileStru,'TYP+CINVTYPE+ITEM+MFGCODE+DYELOT',loFormSet.lcIssLtFile)



IF EMPTY(lcOprCode) .AND. EMPTY(lcLotNo)
SELECT (loFormSet.lcTktSheet)
lnRecNo = RECNO()
SCAN FOR cShowType+cOprCode='1'+SPACE(6)
SCATTER MEMVAR
INSERT INTO (loFormSet.lcIssLtFile) FROM MEMVAR
IF EMPTY(EVALUATE(loFormSet.lcIssLtFile+'.item'))
REPLACE (loFormSet.lcIssLtFile+'.item') WITH loFormSet.laSetups[2+INT(VAL(EVALUATE(loFormSet.lcIssLtFile+'.Typ'))),2]
ENDIF
ENDSCAN
IF BETWEEN(lnRecNo,1,RECCOUNT())
GO lnRecNo
ENDIF
ELSE
*--- DOC.
*--- Function to issue operation from lot
*--- DOC.

*B039756,1 KHM 10/09/2005 Open the styinvjl if its not opened [Begin]
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [BEGIN]
*=gfOpenFile(oAriaApplication.DataDir+'STYINVJL',oAriaApplication.DataDir+'MFGOPR','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYINVJL',oAriaApplication.DataDir+'MFGOPR','SH')
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [END]


*B039756,1 KHM 10/09/2005 [End]

=lfLotCstItm(lcOprCode,lcLotNo,loFormSet)
ENDIF
spIssPcnt = 100.00
llFound   = .T.
ldIssDate = oariaapplication.systemdate
SELECT (loFormSet.lcIssLtFile)
SET KEY TO
GO TOP
IF EOF()
IF !EMPTY(lcOprCode) OR !EMPTY(lcLotNo)
*E300725,1 Message : 38118
*E300725,1 No cost items found for operation xxxxxxxxxx Lot# 99
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38118B00000','ALERT',ALLTRIM(gfCodDes(lcOprCode,'MfgCode'))+'|'+lcLotNo)
ENDIF
ELSE
PRIVATE loParentForm
loParentForm = loFormSet
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSLT.SCX') WITH lcOprCode,lcLotNo
lcOperCode = lcOprCode
lcLotNom = lcLotNo
=gfCallForm('MFISSLT',.F.,"lcOperCode,lcLotNom")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
ENDIF

IF USED(lcOldSheet)
USE IN (lcOldSheet)
ENDIF

SELECT (loFormSet.lcTktSheet)
LOCATE
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfLotCstItm
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Get lot cost items
*!*************************************************************
*! Calls     : gfModalGen(),gpAdFabWar(),gpAdStyWar()
*!*************************************************************
*! Parameters: lcOprCode : Operation
*!             lcLotNo   : Lot#
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfLotCstItm()
*!*************************************************************
FUNCTION lfLotCstItm
LPARAMETERS lcOprCode,lcLotNo,loFormSet
PRIVATE lnAlias,lnSumRecNo,lnDetRecNo,lnOprRecNo,llStyDye

lnAlias = SELECT(0)
lnSumRecNo= RECNO(loFormSet.lcTktSheet)
lnDetRecNo= RECNO(loFormSet.lcDetFile)
lnOprRecNo= RECNO(loFormSet.lcOprDet)
SET ORDER TO TAG 'Lot' IN (loFormSet.lcOprDet)

SELECT (loFormSet.lcIssLtFile)
ZAP
SELECT (loFormSet.lcTktSheet)
*--- DOC.
*--- Scan for the specific operation records.
*--- DOC.
SCAN FOR cShowType = '1' .AND. (cOprCode = lcOprCode OR MfgCode = lcOprCode)
SCATTER MEMVAR
*--- DOC.
*--- If mfg operatoin get the elated field GL - Account
*--- DOC.
PRIVATE lcMfgGlAcnt
lcMfgGlAcnt = loFormSet.lcMfgGlAcnt
LOCAL ARRAY laMfgRFld[7,2]
=ACOPY(loFormSet.laMfgRFld,laMfgRFld)
IF m.cCatgTyp='M' AND (!gfRltFld(m.MfgCode,@laMfgRFld,'MFGCODE') OR EMPTY(lcMfgGlAcnt))
LOOP
ENDIF
SELECT (loFormSet.lcOprDet)
*--- DOC.
*--- Scan for the budget record in the operation detail file
*--- DOC.
=SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode+lcLotNo)
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd = ;
loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode+lcLotNo FOR TranCd='1'
lcItem = Item
llStyDye = IIF(SEEK(lcItem,'Style'),STYLE.cDye_Flg='Y',.F.)
SELECT (loFormSet.lcDetFile)
=SEEK(loFormSet.lcInvType+lcItem)
m.Pieces = 0
*B607925,1 WAM 01/10/2007 Fix wrong calculation of item component required quantity
STORE 0 TO lnReqQty1, lnReqQty2, lnReqQty3, lnReqQty4, lnReqQty5, lnReqQty6, lnReqQty7, lnReqQty8
*B607925,1 WAM 01/10/2007 Fix wrong calculation of item component required quantity

*--- DOC.
*--- Get all the record for current item and operation
*--- DOC.
SCAN REST WHILE cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode=loFormSet.lcInvType+lcItem ;
FOR cBomTyp+cShowType+cInvTypC+Item+MfgCode = m.Typ+'1'+m.cInvType+m.Item+m.MfgCode
*B607925,1 WAM 01/10/2007 Fix wrong calculation of item component required quantity
*!*	      FOR lnCount = 1 TO 8
*!*	        lcCount = STR(lnCount,1)
*!*	        IF lcCount $ cSizes
*!*	          m.Pieces=m.Pieces+EVALUATE(loFormSet.lcOprDet+'.nLotQty'+lcCount)
*!*	        ENDIF
*!*	      ENDFOR
FOR lnCount = 1 TO 8
lcCount    = SUBSTR(cSizes,lnCount,1)
lcCompSize = SUBSTR(cCompSizes,lnCount,1)
IF !EMPTY(lcCount) AND !EMPTY(lcCompSize)
lnReqQty&lcCompSize = lnReqQty&lcCompSize + EVALUATE(loFormSet.lcOprDet+'.nLotQty'+lcCount)
m.Pieces=m.Pieces+EVALUATE(loFormSet.lcOprDet+'.nLotQty'+lcCount)
ENDIF
ENDFOR
*B607925,1 WAM 01/10/2007 (End)
ENDSCAN
IF m.Pieces > 0
IF !SEEK(m.Typ+m.cInvType+m.Item+m.MfgCode+m.Dyelot,loFormSet.lcIssLtFile)
m.Req_Qty  = m.Pieces*m.UntQty
m.Est_Cost = m.Req_Qty*m.UntCost
*B607925,1 WAM 01/10/2007 Fix wrong calculation of item component required quantity
*!*	        FOR lnI = 1 TO 8
*!*	          lcI = STR(lnI,1)
*!*	          m.Req_Qty&lcI =  EVALUATE(loFormSet.lcOprDet+'.nLotQty'+lcI) * m.UntQty
*!*	        ENDFOR
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
m.Req_Qty&lcI =  EVALUATE('lnReqQty'+lcI) * m.UntQty
ENDFOR
*B607925,1 WAM 01/10/2007 (End)
SELECT (loFormSet.lcDetFile)
=SEEK(loFormSet.lcInvType+lcItem)
LOCATE REST WHILE cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode=loFormSet.lcInvType+lcItem ;
FOR cBomTyp+cShowType+cInvTypC+Item+MfgCode = m.Typ+'1'+m.cInvType+m.Item+m.MfgCode
lcSizeQty = IIF(FOUND(),cSizes,'12345678')
lcSizeQty = IIF(EMPTY(lcSizeQty),'12345678',lcSizeQty)
lnoldpcs  = 0
lnoldpcs  = m.Req_Qty
m.Req_Qty = 0
FOR lnI = 1 TO 8
IF !llStyDye
lcI = STR(lnI,1)
IF lcI $ lcSizeQty
m.Req_Qty = m.Req_Qty + EVALUATE('m.Req_Qty'+lcI)
ENDIF
ENDIF
ENDFOR
IF m.Req_qty = 0
m.Req_Qty = lnOldPcs
ELSE
m.Est_Cost = m.Req_Qty*m.UntCost
m.Pieces = m.Req_Qty/m.UntQty
ENDIF
IF !EMPTY(m.DYELOT)
SELECT (loFormSet.lcDetFile)
m.Pieces = 0
SCAN FOR Style = EVALUATE(loFormSet.lcOprDet+'.Item') AND cInvTypC = m.cInvType AND Item = m.item AND;
Dyelot = m.Dyelot
m.Pieces =  m.Pieces + styqty
ENDSCAN
m.Req_Qty  = m.Pieces*m.UntQty
m.Est_Cost = m.Req_Qty*m.UntCost
ENDIF
DO CASE
CASE m.cCatgTyp = 'S'
SELECT StyInvJl
*wael
*gfStyCrl should updates the cOprCode and cLotNo in the STYINVJL file
*Now, it does not save those two fields. I changed the following lines until gfstycrl function is changed
*!*	            =SEEK(EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode+lcLotNo+'1'+m.Item)
*!*	            *--- DOC.
*!*	            *--- Sum all the issued record to variables m.Used with -ve signe
*!*	            *--- DOC.
*!*	            SUM REST -nStk1,-nStk2,-nStk3,-nStk4,-nStk5,-nStk6,-nStk7,-nStk8,-nTotStk,;
*!*	                     IIF(cIRType='I',-nStk1,0),IIF(cIRType='I',-nStk2,0),;
*!*	                     IIF(cIRType='I',-nStk3,0),IIF(cIRType='I',-nStk4,0),;
*!*	                     IIF(cIRType='I',-nStk5,0),IIF(cIRType='I',-nStk6,0),;
*!*	                     IIF(cIRType='I',-nStk7,0),IIF(cIRType='I',-nStk8,0),;
*!*	                     IIF(cIRType='I',-nTotStk,0) ;
*!*	            TO    m.Used_Qty1,m.Used_Qty2,m.Used_Qty3,m.Used_Qty4,m.Used_Qty5,;
*!*	                  m.Used_Qty6,m.Used_Qty7,m.Used_Qty8,m.Used_Qty,m.Iss_Qty1,;
*!*	                  m.Iss_Qty2,m.Iss_Qty3,m.Iss_Qty4,m.Iss_Qty5,m.Iss_Qty6,m.Iss_Qty7,;
*!*	                  m.Iss_Qty8,m.Issue_Qty ;
*!*	            WHILE ctrcode+coprcode+clotno+ctrtype+style+cwarecode = ;
*!*	                  EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode+lcLotNo+'1'+m.Item
*!*	            PRIVATE Used_QtyI,Iss_QtyI
*!*	            DIMENSION Used_QtyI[9],Iss_QtyI[9]
*!*	            =SEEK(EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode+lcLotNo+'I'+m.Item)
*!*	            SUM REST -nStk1,-nStk2,-nStk3,-nStk4,-nStk5,-nStk6,-nStk7,-nStk8,-nTotStk,;
*!*	                     IIF(cIRType='I',-nStk1,0),IIF(cIRType='I',-nStk2,0),;
*!*	                     IIF(cIRType='I',-nStk3,0),IIF(cIRType='I',-nStk4,0),;
*!*	                     IIF(cIRType='I',-nStk5,0),IIF(cIRType='I',-nStk6,0),;
*!*	                     IIF(cIRType='I',-nStk7,0),IIF(cIRType='I',-nStk8,0),;
*!*	                     IIF(cIRType='I',-nTotStk,0) ;
*!*	            TO    Used_QtyI[1],Used_QtyI[2],Used_QtyI[3],Used_QtyI[4],;
*!*	                  Used_QtyI[5],Used_QtyI[6],Used_QtyI[7],Used_QtyI[8],Used_QtyI[9],;
*!*	                  Iss_QtyI[1] ,Iss_QtyI[2] ,Iss_QtyI[3] ,Iss_QtyI[4] ,;
*!*	                  Iss_QtyI[5] ,Iss_QtyI[6] ,Iss_QtyI[7] ,Iss_QtyI[8] ,Iss_QtyI[9] ;
*!*	            WHILE ctrcode+coprcode+clotno+ctrtype+style+cwarecode = ;
*!*	                  EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode+lcLotNo+'I'+m.Item
*E304013,1 MMT 07/24/2018 Convert STYINVJL to SQL[Start]
*=SEEK(EVALUATE(loFormSet.lcPosHdr+'.PO')+SPACE(6)+SPACE(2)+'1'+m.Item)
=gfSEEK(EVALUATE(loFormSet.lcPosHdr+'.PO')+SPACE(6)+SPACE(2)+'1'+m.Item)
*E304013,1 MMT 07/24/2018 Convert STYINVJL to SQL[End]
*--- DOC.
*--- Sum all the issued record to variables m.Used with -ve signe
*--- DOC.
SUM REST -nStk1,-nStk2,-nStk3,-nStk4,-nStk5,-nStk6,-nStk7,-nStk8,-nTotStk,;
IIF(cIRType='I',-nStk1,0),IIF(cIRType='I',-nStk2,0),;
IIF(cIRType='I',-nStk3,0),IIF(cIRType='I',-nStk4,0),;
IIF(cIRType='I',-nStk5,0),IIF(cIRType='I',-nStk6,0),;
IIF(cIRType='I',-nStk7,0),IIF(cIRType='I',-nStk8,0),;
IIF(cIRType='I',-nTotStk,0) ;
TO    m.Used_Qty1,m.Used_Qty2,m.Used_Qty3,m.Used_Qty4,m.Used_Qty5,;
m.Used_Qty6,m.Used_Qty7,m.Used_Qty8,m.Used_Qty,m.Iss_Qty1,;
m.Iss_Qty2,m.Iss_Qty3,m.Iss_Qty4,m.Iss_Qty5,m.Iss_Qty6,m.Iss_Qty7,;
m.Iss_Qty8,m.Issue_Qty ;
WHILE ctrcode+coprcode+clotno+ctrtype+style+cwarecode = ;
EVALUATE(loFormSet.lcPosHdr+'.PO')+SPACE(6)+SPACE(2)+'1'+m.Item
PRIVATE Used_QtyI,Iss_QtyI
DIMENSION Used_QtyI[9],Iss_QtyI[9]
=SEEK(EVALUATE(loFormSet.lcPosHdr+'.PO')+SPACE(6)+SPACE(2)+'I'+m.Item)
SUM REST -nStk1,-nStk2,-nStk3,-nStk4,-nStk5,-nStk6,-nStk7,-nStk8,-nTotStk,;
IIF(cIRType='I',-nStk1,0),IIF(cIRType='I',-nStk2,0),;
IIF(cIRType='I',-nStk3,0),IIF(cIRType='I',-nStk4,0),;
IIF(cIRType='I',-nStk5,0),IIF(cIRType='I',-nStk6,0),;
IIF(cIRType='I',-nStk7,0),IIF(cIRType='I',-nStk8,0),;
IIF(cIRType='I',-nTotStk,0) ;
TO    Used_QtyI[1],Used_QtyI[2],Used_QtyI[3],Used_QtyI[4],;
Used_QtyI[5],Used_QtyI[6],Used_QtyI[7],Used_QtyI[8],Used_QtyI[9],;
Iss_QtyI[1] ,Iss_QtyI[2] ,Iss_QtyI[3] ,Iss_QtyI[4] ,;
Iss_QtyI[5] ,Iss_QtyI[6] ,Iss_QtyI[7] ,Iss_QtyI[8] ,Iss_QtyI[9] ;
WHILE ctrcode+coprcode+clotno+ctrtype+style+cwarecode = ;
EVALUATE(loFormSet.lcPosHdr+'.PO')+SPACE(6)+SPACE(2)+'I'+m.Item

*WAEL
m.Used_Qty1 = m.Used_Qty1 + Used_QtyI[1]
m.Used_Qty2 = m.Used_Qty2 + Used_QtyI[2]
m.Used_Qty3 = m.Used_Qty3 + Used_QtyI[3]
m.Used_Qty4 = m.Used_Qty4 + Used_QtyI[4]
m.Used_Qty5 = m.Used_Qty5 + Used_QtyI[5]
m.Used_Qty6 = m.Used_Qty6 + Used_QtyI[6]
m.Used_Qty7 = m.Used_Qty7 + Used_QtyI[7]
m.Used_Qty8 = m.Used_Qty8 + Used_QtyI[8]
m.Used_Qty  = m.Used_Qty  + Used_QtyI[9]

m.Iss_Qty1  = m.Iss_Qty1  + Iss_QtyI[1]
m.Iss_Qty2  = m.Iss_Qty2  + Iss_QtyI[2]
m.Iss_Qty3  = m.Iss_Qty3  + Iss_QtyI[3]
m.Iss_Qty4  = m.Iss_Qty4  + Iss_QtyI[4]
m.Iss_Qty5  = m.Iss_Qty5  + Iss_QtyI[5]
m.Iss_Qty6  = m.Iss_Qty6  + Iss_QtyI[6]
m.Iss_Qty7  = m.Iss_Qty7  + Iss_QtyI[7]
m.Iss_Qty8  = m.Iss_Qty8  + Iss_QtyI[8]
m.Issue_Qty = m.Issue_Qty + Iss_QtyI[9]

CASE m.cCatgTyp='F' .OR. (m.cCatgTyp='T' .AND. m.Trim_Invt)
m.cTrType  = '1'
m.cTrCode  = EVALUATE(loFormSet.lcPosHdr+'.PO')
m.cOprCode = lcOprCode
m.cLotNo   = lcLotNo
m.Style    = m.Item
=lfOpenSql('ITEMJRNL','MATINVJL','MFGOPR','CTRCODE+COPRCODE+CLOTNO+CTRTYPE+CINVTYPE+STYLE',loFormSet)
SELECT MatInvJl
SUM -nStk1,-nStk2,-nStk3,-nStk4,-nStk5,-nStk6,-nStk7,-nStk8,-nTotStk,;
IIF(cIRType='I',-nStk1,0),IIF(cIRType='I',-nStk2,0),;
IIF(cIRType='I',-nStk3,0),IIF(cIRType='I',-nStk4,0),;
IIF(cIRType='I',-nStk5,0),IIF(cIRType='I',-nStk6,0),;
IIF(cIRType='I',-nStk7,0),IIF(cIRType='I',-nStk8,0),;
IIF(cIRType='I',-nTotStk,0) ;
TO  m.Used_Qty1,m.Used_Qty2,m.Used_Qty3,m.Used_Qty4,m.Used_Qty5,;
m.Used_Qty6,m.Used_Qty7,m.Used_Qty8,m.Used_Qty,m.Iss_Qty1,;
m.Iss_Qty2,m.Iss_Qty3,m.Iss_Qty4,m.Iss_Qty5,m.Iss_Qty6,m.Iss_Qty7,;
m.Iss_Qty8,m.Issue_Qty

PRIVATE Used_QtyI,Iss_QtyI
DIMENSION Used_QtyI[9],Iss_QtyI[9]
m.cTrType  = '9'
=lfOpenSql('ITEMJRNL','MATINVJL','MFGOPR','CTRCODE+COPRCODE+CLOTNO+CTRTYPE+CINVTYPE+STYLE',loFormSet)
SELECT MatInvJl
SUM -nStk1,-nStk2,-nStk3,-nStk4,-nStk5,-nStk6,-nStk7,-nStk8,-nTotStk,;
IIF(cIRType='I',-nStk1,0),IIF(cIRType='I',-nStk2,0),;
IIF(cIRType='I',-nStk3,0),IIF(cIRType='I',-nStk4,0),;
IIF(cIRType='I',-nStk5,0),IIF(cIRType='I',-nStk6,0),;
IIF(cIRType='I',-nStk7,0),IIF(cIRType='I',-nStk8,0),;
IIF(cIRType='I',-nTotStk,0) ;
TO  Used_QtyI[1],Used_QtyI[2],Used_QtyI[3],Used_QtyI[4],;
Used_QtyI[5],Used_QtyI[6],Used_QtyI[7],Used_QtyI[8],Used_QtyI[9],;
Iss_QtyI[1] ,Iss_QtyI[2] ,Iss_QtyI[3] ,Iss_QtyI[4] ,;
Iss_QtyI[5] ,Iss_QtyI[6] ,Iss_QtyI[7] ,Iss_QtyI[8] ,Iss_QtyI[9]

m.Used_Qty1 = m.Used_Qty1 + Used_QtyI[1]
m.Used_Qty2 = m.Used_Qty2 + Used_QtyI[2]
m.Used_Qty3 = m.Used_Qty3 + Used_QtyI[3]
m.Used_Qty4 = m.Used_Qty4 + Used_QtyI[4]
m.Used_Qty5 = m.Used_Qty5 + Used_QtyI[5]
m.Used_Qty6 = m.Used_Qty6 + Used_QtyI[6]
m.Used_Qty7 = m.Used_Qty7 + Used_QtyI[7]
m.Used_Qty8 = m.Used_Qty8 + Used_QtyI[8]
m.Used_Qty  = m.Used_Qty  + Used_QtyI[9]

m.Iss_Qty1  = m.Iss_Qty1  + Iss_QtyI[1]
m.Iss_Qty2  = m.Iss_Qty2  + Iss_QtyI[2]
m.Iss_Qty3  = m.Iss_Qty3  + Iss_QtyI[3]
m.Iss_Qty4  = m.Iss_Qty4  + Iss_QtyI[4]
m.Iss_Qty5  = m.Iss_Qty5  + Iss_QtyI[5]
m.Iss_Qty6  = m.Iss_Qty6  + Iss_QtyI[6]
m.Iss_Qty7  = m.Iss_Qty7  + Iss_QtyI[7]
m.Iss_Qty8  = m.Iss_Qty8  + Iss_QtyI[8]
m.Issue_Qty = m.Issue_Qty + Iss_QtyI[9]

OTHERWISE
m.cIMTyp    = loFormSet.lcTranType
m.cBomType  = m.Typ
m.cTktNo    = EVALUATE(loFormSet.lcPosHdr+'.PO')
m.Actualize = 'N'
m.cOprCode  = lcOprCode
m.cLotNo    = lcLotNo
=lfOpenSql('BOMCOST','BOMCOST','POBOMCLS',;
'CIMTYP+CTKTNO+ACTUALIZE+CBOMTYPE+CINVTYPE+ITEM+MFGCODE+COPRCODE+CLOTNO',loFormSet)
SELECT BOMCOST
SUM nTotQty,IIF(nTotQty>0,nTotQty,0) TO m.Used_Qty,m.Issue_Qty
ENDCASE
INSERT INTO (loFormSet.lcIssLtFile) FROM MEMVAR
ELSE
SELECT (loFormSet.lcIssLtFile)
REPLACE Pieces   WITH Pieces   + m.Pieces ,;
Req_Qty  WITH Req_Qty  + m.Pieces*m.UntQty ,;
Est_Cost WITH Est_Cost + m.Pieces*m.UntQty*m.UntCost
*B607848,1 WAM 11/30/2006 Calculate the required quantity per each size
REPLACE Req_Qty1 WITH Req_Qty1 + EVALUATE(loFormSet.lcOprDet+'.nLotQty1') * m.UntQty ,;
Req_Qty2 WITH Req_Qty2 + EVALUATE(loFormSet.lcOprDet+'.nLotQty2') * m.UntQty ,;
Req_Qty3 WITH Req_Qty3 + EVALUATE(loFormSet.lcOprDet+'.nLotQty3') * m.UntQty ,;
Req_Qty4 WITH Req_Qty4 + EVALUATE(loFormSet.lcOprDet+'.nLotQty4') * m.UntQty ,;
Req_Qty5 WITH Req_Qty5 + EVALUATE(loFormSet.lcOprDet+'.nLotQty5') * m.UntQty ,;
Req_Qty6 WITH Req_Qty6 + EVALUATE(loFormSet.lcOprDet+'.nLotQty6') * m.UntQty ,;
Req_Qty7 WITH Req_Qty7 + EVALUATE(loFormSet.lcOprDet+'.nLotQty7') * m.UntQty ,;
Req_Qty8 WITH Req_Qty8 + EVALUATE(loFormSet.lcOprDet+'.nLotQty8') * m.UntQty
*B607848,1 (End)
ENDIF
ENDIF
ENDSCAN
ENDSCAN
IF BETWEEN(lnSumRecNo,1,RECCOUNT(loFormSet.lcTktSheet))
GOTO lnSumRecNo IN (loFormSet.lcTktSheet)
ENDIF
IF BETWEEN(lnDetRecNo,1,RECCOUNT(loFormSet.lcDetFile))
GOTO lnDetRecNo IN (loFormSet.lcDetFile)
ENDIF
IF BETWEEN(lnOprRecNo,1,RECCOUNT(loFormSet.lcOprDet))
GOTO lnOprRecNo IN (loFormSet.lcOprDet)
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfAdjiButt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/26/2004
*! Purpose   : Select, Unselect, Select all, Select None and Invert lot cost items
*!*************************************************************
*! Calls     : lfShIssLot()
*!*************************************************************
*! Parameters: lcType  'S' : Select
*!                     'A' : Select All
*!                     'N' : Select None
*!                     'V' : Invert
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfAdjiButt()
*!*************************************************************
FUNCTION lfAdjiButt
PARAMETERS lcType,loFormSet
PRIVATE lnRecNo,llDataSel,llDataUSel

SELECT (loParentForm.lcIssLtFile)
lnRecNo = RECNO()
DO CASE
CASE lcType = 'S'
REPLACE lSelect  WITH !lSelect AND lfChkDye() ,;
nIssue   WITH IIF(!lSelect,0,spIssPcnt*MAX(REQ_QTY-Used_Qty,0)/100) ,;
nIssPcnt WITH IIF(!lSelect,0,spIssPcnt)

CASE lcType = 'A'
REPLACE ALL lSelect  WITH lfChkDye() ,;
nIssue   WITH IIF(!lSelect,0,spIssPcnt*MAX(REQ_QTY-Used_Qty,0)/100) ,;
nIssPcnt WITH IIF(!lSelect,0,spIssPcnt)
CASE lcType = 'N'
REPLACE ALL lSelect  WITH .F. ,;
nIssue   WITH 0 ,;
nIssPcnt WITH 0
CASE lcType = 'V'
REPLACE ALL lSelect  WITH !lSelect AND lfChkDye() ,;
nIssue   WITH IIF(!lSelect,0,spIssPcnt*MAX(REQ_QTY-Used_Qty,0)/100) ,;
nIssPcnt WITH IIF(!lSelect,0,spIssPcnt)
ENDCASE
LOCATE
llFound = FOUND()
LOCATE FOR !lSelect
llDataUSel = FOUND()
LOCATE FOR lSelect
llDataSel  = FOUND()
IF BETWEEN(lnRecNo,1,RECCOUNT())
GO lnRecNo
ENDIF
IF !lSelect
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdSelect.Caption = LANG_MFCSSH_SELECT
loFormSet.AriaForm1.cmdSelect.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_SELECT,loFormSet.GetHeaderText("LANG_MFCSSH_SELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdSelect.Caption = LANG_MFCSSH_UNSELECT
loFormSet.AriaForm1.cmdSelect.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_UNSELECT,loFormSet.GetHeaderText("LANG_MFCSSH_UNSELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
loFormSet.AriaForm1.cmdSelect.Enabled  = llFound
loFormSet.AriaForm1.cmdSelAll.Enabled  = llDataUSel
loFormSet.AriaForm1.cmdSelNone.Enabled = llDataSel
loFormSet.AriaForm1.cmdInvert.Enabled  = llFound
loFormSet.AriaForm1.spnPercent.Enabled = llDataSel
=lfShIssLot(loFormSet)

*!*************************************************************
*! Name      : lfChkDye
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/26/2004
*! Purpose   : Check Dyelots for Styles and Fabrics selected for
*!             automatic issue
*!*************************************************************
*! Calls     : SDYEBROW(),FDYEBROW()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfChkDye()
*!*************************************************************
FUNCTION lfChkDye
*--- DOC.
*--- Check if the system use dyelot then update the field Dyelot
*--- DOC.

LOCAL lnAlias
lnAlias = SELECT(0)

DO CASE
CASE cCatgTyp = 'S'
IF loParentForm.laSetups[10,2]='Y' AND SEEK(Item,'Style') AND Style.cDye_Flg='Y' AND ;
( EMPTY(Dyelot) OR !SEEK(Item+cWareCode+Dyelot,'STYDYE') )
lcDyelot = IIF(EMPTY(Dyelot),'?',Dyelot)
IF !SDYEBROW(Item, @lcDyelot, .T.,cWareCode,.F.,.F.,.F.,.F.,loParentForm.llStyConfg)
RETURN(.F.)
ENDIF
REPLACE Dyelot WITH lcDyelot
ENDIF
CASE cCatgTyp = 'F' OR (cCatgTyp = 'T' AND Trim_Invt)
IF loParentForm.laSetups[14,2]='Y'
LOCAL llFndFab
llFndFab = .F.
m.cStyMajor = Item
m.cInvType  = cInvType
IF lfOpenSql('ITEM','FABRIC','CSTYLE','CINVTYPE+CSTYMAJOR',loParentForm)
SELECT FABRIC
LOCATE
llFndFab = !EOF()
IF llFndFab AND Fabric.cDye_Flg='Y'
llFndFab = .F.
SELECT (lnAlias)
m.Style     = Item
m.cWareCode = cWareCode
m.Dyelot    = Dyelot
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE',loParentForm)
SELECT FABDYE
LOCATE
llFndFab = !EOF()
ENDIF
SELECT (lnAlias)
IF EMPTY(Dyelot) OR !llFndFab
lcDyelot = IIF(EMPTY(Dyelot),'?',Dyelot)
IF !gfItmDyBrw(cInvType,Item,@lcDyelot,cWareCode,.T.) OR EMPTY(lcDyelot)
RETURN(.F.)
ENDIF
REPLACE Dyelot WITH lcDyelot
ENDIF
ENDIF
ENDIF
ENDIF
ENDCASE
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfwIssQTy
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/26/2004
*! Purpose   : When function for lot cost item issued quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwIssQTy()
*!*************************************************************
FUNCTION lfwIssQTy
IF !lSelect .OR. cCatgTyp$'SFT'
RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvIssQTy
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/26/2004
*! Purpose   : Valid function for lot cost item issued quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvIssQTy()
*!*************************************************************
FUNCTION lfvIssQty

REPLACE nIssPcnt WITH IIF(Req_Qty-Used_Qty<=0,0,nIssue*100/(Req_Qty-Used_Qty))

*!*************************************************************
*! Name      : lfwIssPcnt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/26/2004
*! Purpose   : When function for lot cost item issued quantity percent
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwIssPcnt()
*!*************************************************************
FUNCTION lfwIssPcnt
IF !lSelect
RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvBIsspcnt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/26/2004
*! Purpose   : Valid function for lot cost item issued quantity percent
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvBIsspcnt()
*!*************************************************************
FUNCTION lfvBIsspcnt

REPLACE nIssue WITH MAX(Req_Qty-Used_Qty,0)*nIssPcnt/100

*!*************************************************************
*! Name      : lfShIssLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/26/2004
*! Purpose   : Show lot cost items
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfShIssLot()
*!*************************************************************
FUNCTION lfShIssLot
LPARAMETERS loFormSet

IF !lSelect
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdSelect.Caption = LANG_MFCSSH_SELECT
loFormSet.AriaForm1.cmdSelect.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_SELECT,loFormSet.GetHeaderText("LANG_MFCSSH_SELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdSelect.Caption = LANG_MFCSSH_UNSELECT
loFormSet.AriaForm1.cmdSelect.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_UNSELECT,loFormSet.GetHeaderText("LANG_MFCSSH_UNSELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
loFormSet.AriaForm1.cmdSelect.Enabled  = llFound

*!*************************************************************
*! Name      : lfvOkIssLt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/26/2004
*! Purpose   : Issue lot cost items
*!*************************************************************
*! Calls     : CHECKPRD(),lfIssRetFab(),lfIssRetSty(),lfIssRetMfg()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOkIssLt()
*!*************************************************************
FUNCTION lfvOkIssLt
LPARAMETERS loFormSet
PRIVATE laIssQty

SELECT (loParentForm.lcIssLtFile)

*! C201141,1 HES 04/26/2009, please quote Processing Enbroidery orders for DCC [Start]
IF (TYPE('oPostyRef.Name') = 'C' AND ASCAN(oPostyRef.laEvntTrig,PADR('CRPONDCOSH',10),1,ALEN(oPostyRef.laEvntTrig,1),1) > 0) OR ;
(TYPE('oPoRecRef.Name') = 'C' AND ASCAN(oPoRecRef.laEvntTrig,PADR('MFRUPDTEMP',10),1,ALEN(oPoRecRef.laEvntTrig,1),1) > 0)
*B609477,1 TMI 12/12/2010 [Start] add the oariaapplication.defaultpath to the called dirmain.fxp
*DO lfPreIssData IN DIRMAIN.FXP WITH loFormSet
DO lfPreIssData IN oariaapplication.defaultpath+'DIRMAIN.FXP' WITH loFormSet
*B609477,1 TMI 12/12/2010 [End  ]
ENDIF
*! C201141,1 HES 04/26/2009, please quote Processing Enbroidery orders for DCC [End]

LOCATE FOR lSelect
lcGlYear   = loParentForm.lcGlYear
lcGlPeriod = loParentForm.lcGlPeriod
IF FOUND() AND !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
RETURN .F.
ENDIF
loParentForm.lcGlYear   = lcGlYear
loParentForm.lcGlPeriod = lcGlPeriod
llMessage = .F.
lnSelCount = 0
SCAN FOR lSelect
lnSelCount = lnSelCount + 1
ENDSCAN
SCAN FOR lSelect
SELECT (loParentForm.lcIssLtFile)
SCATTER MEMVAR
m.cBomType = m.Typ
m.cTktNo   = m.CutTkt
m.cDyelot  = m.Dyelot
IF !lfOpenSql('BOMCOST','BOMCOST','BOMCSTKT',;
'CBOMTYPE+CIMTYP+CTKTNO+CINVTYPE+ITEM'+IIF(EMPTY(m.Dyelot),'','+CDYELOT'),loFormSet)
RETURN .F.
ENDIF
SELECT (loParentForm.lcIssLtFile)

DO CASE
CASE INLIST(cCatgTyp,'F','T')
DECLARE laIssQty[9]

*B607846,1 MMT 11/29/2006 Fix updating issued quantity,T20061128.0002
*!*	      lcNoofSize = IIF(EMPTY(cCompSizes),'12345678',cCompSizes)
*!*	      laIssQty =  0
*!*	      FOR lnCntr = 1 TO 8
*!*	        lcCntr = STR(lnCntr,1)
*!*	        IF lcCntr $ lcNoofSize
*!*	          laIssQty[lnCntr] = MAX((EVALUATE('Req_Qty'+lcCntr)-EVALUATE('Used_Qty'+lcCntr))*nIssPcnt/100,0)
*!*	        ENDIF
*!*	      ENDFOR
*lcNoofSize = IIF(EMPTY(cCompSizes),'12345678',cCompSizes)
lcNoofSize = IIF(EMPTY(cCompSizes),'11111111',cCompSizes)

laIssQty =  0
FOR lnCntr = 1 TO 8
lcCntr = STR(lnCntr,1)
IF lcCntr $ lcNoofSize
FOR lnsty = 1 TO 8
lcsty = STR(lnsty,1)
laIssQty[lnCntr] = laIssQty[lnCntr] + MAX((EVALUATE('Req_Qty'+lcsty)-EVALUATE('Used_Qty'+lcsty))*nIssPcnt/100,0)
ENDFOR
ENDIF
ENDFOR
*B607846,1 MMT 11/29/2006 Fix updating issued quantity,T20061128.0002

laIssQty[9] = laIssQty[1]+laIssQty[2]+laIssQty[3]+laIssQty[4]+;
laIssQty[5]+laIssQty[6]+laIssQty[7]+laIssQty[8]

LOCAL llFndFab
llFndFab = .F.
m.Style    = Item
m.cInvType = cInvType
IF lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loParentForm)
SELECT FABRIC
LOCATE
llFndFab = !EOF()
ENDIF
IF llFndFab
SELECT (loParentForm.lcIssLtFile)
m.Style     = Item
m.cWareCode = cWareCode
m.Dyelot    = Dyelot
=lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE',loParentForm)
llItemDye = loParentForm.laSetups[14,2]='Y' .AND. Fabric.cDye_Flg = 'Y'

*B131720,1 WSH 04/27/2006 Reselect the correct file as lfOpenSql does not restore it...
*                         Restoring it in lfOpenSql may result in other bugs.. [Start]
SELECT (loParentForm.lcIssLtFile)
*B131720,1 WSH 04/27/2006 [End]
DO CASE
CASE loParentForm.laSetups[11,2]='L'
SELECT (loParentForm.lcIssLtFile)
SCATTER MEMVAR
lcIssWare = EVALUATE(loParentForm.lcPosHdr+'.CMATWARE')
lnIssWare = CEILING(ASCAN(loParentForm.laMatWare,lcIssWare)/2)
m.TotStk = FABDYE.TotStk
STORE nIssue TO m.Used_Qty,m.Req_Qty

m.Style     = m.Item
m.cDyelot   = m.Dyelot
IF !lfOpenSql('ITEMJRNL','FABINV','STYINVJL','CINVTYPE+STYLE'+IIF(EMPTY(m.Dyelot),'','+CDYELOT'),loFormSet)
RETURN .F.
ENDIF
=lfSetIndex('FABINV','FABINV','cInvType+Style+cWareCode+cDyelot+cRSession')
LOCAL lnEngineBehavior
lnEngineBehavior = SET("EngineBehavior")
SET ENGINEBEHAVIOR 70
SELECT cInvType,Style,cWareCode,cDyelot,cRSession,cISession,nCost,cApInvNo,dTrDate,nRecCost,cTrCode,;
SUM(nStk1) AS 'nBal1',SUM(nStk2) AS 'nBal2',SUM(nStk3) AS 'nBal3',SUM(nStk4) AS 'nBal4',;
SUM(nStk5) AS 'nBal5',SUM(nStk6) AS 'nBal6',SUM(nStk7) AS 'nBal7',SUM(nStk8) AS 'nBal8',;
SUM(nTotStk) AS 'nBalance',nUntCstBuy,00000000.000 AS nToIssue1,00000000.000 AS nToIssue2,;
00000000.000 AS nToIssue3,00000000.000 AS nToIssue4,00000000.000 AS nToIssue5,00000000.000 AS nToIssue6,;
00000000.000 AS nToIssue7,00000000.000 AS nToIssue8,00000000.000 AS nToIssue FROM FABINV;
GROUP BY cInvType,Style,cWareCode,cDyelot,cRSession ;
HAVING nBalance > 0 INTO DBF (oAriaApplication.WorkDir+loParentForm.lcOpenLots)
INDEX ON cWareCode+cDyelot TAG (loParentForm.lcOpenLots)
SET RELATION TO cInvType+Style+cWareCode+cDyelot+cRSession INTO FABINV
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFOPLOT.SCX') WITH .T.,loFormSet.lcOprCode,loFormSet.lcLotNo
loCallingForm = loFormSet
=gfCallForm('MFOPLOT',.F.,".T.,loCallingForm.lcOprCode,loCallingForm.lcLotNo")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
SET ENGINEBEHAVIOR lnEngineBehavior
USE IN (loParentForm.lcOpenLots)
USE IN FABINV
SELECT (loParentForm.lcIssLtFile)
CASE INLIST(loParentForm.laSetups[11,2],'F','I')
=lfIssSeq(loParentForm.laSetups[11,2],Typ,cCatgTyp,cInvType,Item,;
cWareCode,Dyelot,@laIssQty,loFormSet.lcOprCode,loFormSet.lcLotNo,ldIssDate)
OTHERWISE
lnIssCost = IIF(loParentForm.laSetups[11,2]='S',Fabric.TotCost,;
IIF(loParentForm.laSetups[2,2]='Y',FabDye.nAveCstBuy,Fabric.nAveCstBuy))
=lfIssRetFab(Typ,cCatgTyp,cInvType,Item,cWareCode,Dyelot,@laIssQty,lnIssCost,loFormSet.lcOprCode,;
loFormSet.lcLotNo,ldIssDate,SPACE(6),gfsequence('GLSession'))
ENDCASE
ENDIF
CASE cCatgTyp = 'S'
DECLARE laIssQty[9]
lcNoofSize = IIF(EMPTY(cCompSizes),'12345678',cCompSizes)
laIssQty =  0
FOR lnCntr = 1 TO 8
lcCntr = STR(lnCntr,1)
IF lcCntr $ lcNoofSize
laIssQty[lnCntr] = -1*MAX(CEILING((Req_Qty&lcCntr-Used_Qty&lcCntr)*nIssPcnt/100),0)
ENDIF
ENDFOR
laIssQty[9] = laIssQty[1]+laIssQty[2]+laIssQty[3]+laIssQty[4]+;
laIssQty[5]+laIssQty[6]+laIssQty[7]+laIssQty[8]
=SEEK(Item,'Style')
=SEEK(Item+cWareCode+SPACE(10),'StyDye')
lnIssCost = IIF(loParentForm.laSetups[12,2]='S',Style.TotCost,;
IIF(loParentForm.laSetups[2,2]='Y',StyDye.Ave_Cost,Style.Ave_Cost))

*T20060818.0001(C200876) TMI [Start] Issue Lot cost items Per Bin
IF ASCAN(loParentForm.laEvntTrig , PADR('DVLDPOAU',10)) <> 0 .AND. loParentForm.mDoTrigger(PADR('ISUSEBIN',10))
loIsslt = loFormSet
=loParentForm.mDoTrigger(PADR('DVLDPOAU',10))
ELSE
*T20060818.0001(C200876) TMI [End  ]

=lfIssRetSty(Typ,cCatgTyp,cInvType,Item,cWareCode,Dyelot,;
@laIssQty,lnIssCost,loFormSet.lcOprCode,loFormSet.lcLotNo,ldIssDate,SPACE(6),loParentForm.lcSession)

*T20060818.0001(C200876) TMI [Start]
ENDIF
*T20060818.0001(C200876) TMI [End  ]

CASE INLIST(m.cCatgTyp,'M','P','D')
PRIVATE lcMfgGlAcnt
lcMfgGlAcnt = loParentForm.lcMfgGlAcnt
LOCAL ARRAY laMfgRFld[7,2]
=ACOPY(loParentForm.laMfgRFld,laMfgRFld)
IF !(OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0) .OR. ;
(m.cCatGTyp = 'M' .AND. gfRltFld(m.MfgCode,@laMfgRFld,'MFGCODE') ;
.AND. !EMPTY(lcMfgGlAcnt))
=lfIssRetMfg(Typ,cCatgTyp,SPACE(4),SPACE(19),MfgCode,cWareCode,;
Dyelot,nIssue,UntCost,ldIssDate,loFormSet.lcOprCode,loFormSet.lcLotNo,;
SPACE(6),gfsequence('GLSession'))
ELSE
IF !llMessage
*B603861,1 Message : 34175
*B603861,1 ð cost ð can not be issued since ð linked to the AP module.
*B603861,1 Button : 00000
*B603861,1 Ok
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMessage = IIF(lnSelCount>1,LANG_MFCSSH_MESSAGE1,LANG_MFCSSH_MESSAGE2)
lcMessage = IIF(lnSelCount>1,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_MESSAGE1,loFormSet.GetHeaderText("LANG_MFCSSH_MESSAGE1",loFormSet.HeaderAlias)),;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MESSAGE2,loFormSet.GetHeaderText("LANG_MFCSSH_MESSAGE2",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

=gfModalGen('TRM34175B00000','ALERT',lcMessage)
llMessage = .T.
ENDIF
ENDIF
OTHERWISE
=lfIssRetMfg(Typ,cCatgTyp,cInvType,Item,MfgCode,cWareCode,;
Dyelot,nIssue,UntCost,ldIssDate,loFormSet.lcOprCode,loFormSet.lcLotNo,;
SPACE(6),gfsequence('GLSession'))
ENDCASE
SELECT (loParentForm.lcTktSheet)
=SEEK(EVALUATE(loParentForm.lcIssLtFile+'.cimtyp')+EVALUATE(loParentForm.lcIssLtFile+'.cuttkt')+;
EVALUATE(loParentForm.lcIssLtFile+'.typ')   +EVALUATE(loParentForm.lcIssLtFile+'.cInvType')+;
EVALUATE(loParentForm.lcIssLtFile+'.item')  +EVALUATE(loParentForm.lcIssLtFile+'.mfgcode'))
ENDSCAN

*!*************************************************************
*! Name      : lfvIssPcnt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/26/2004
*! Purpose   : Lot Issue Percent
*!*************************************************************
*! Calls     : lfShIssLot
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvIssPcnt()
*!*************************************************************
FUNCTION lfvIssPcnt
LPARAMETERS loFormSet
PRIVATE lnRecNo

SELECT (loParentForm.lcIssLtFile)
lnRecNo = RECNO()
REPLACE ALL nIssPcnt WITH spIssPcnt ;
nIssue   WITH spIssPcnt*MAX(REQ_QTY-Used_Qty,0)/100 ;
FOR lSelect
IF BETWEEN(lnRecNo,1,RECCOUNT())
GO lnRecNo
ENDIF
=lfShIssLot(loFormSet)

*!*************************************************************
*! Name      : lfIssRetFab
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/01/2004
*! Purpose   : Issue/Return Fabrics and Inventory mantained trims
*!*************************************************************
*! Calls     : gfModalGen,GLDIST
*!*************************************************************
*! Parameters: lcItemType   : Cost Item Type
*!             lcCatgType   : Cost Item Category
*!             lcInvType    : Invtory Type
*!             lcStyle      : Fabric/Color
*!             lcWareCode   : Warehouse
*!             lcDyelot     : Dyelot
*!             laToIssue    : Issued/Return Quantity
*!             lnBuyIssCost : Issued/Return unit cost buy
*!             lcOprCode    : Operation
*!             lcLotNo      : Lot#
*!             ldIssDate    : Issue/Return Date
*!             lcRSession   : Receiving Session#
*!             lcISession   : Issue Session#
*!             llFromDel    : Called upon Deleting The Cost Sheet,
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfIssRetFab()
*!*************************************************************
FUNCTION lfIssRetFab
LPARAMETERS lcItemType,lcCatgType,lcInvType,lcStyle,lcWareCode,lcDyelot,;
laToIssue,lnBuyIssCost,lcOprCode,lcLotNo,ldIssDate,lcRSession,;
lcISession,llFromDel,llUseActCost


LOCAL lcFabLinkCode,lnAmount,lnOldStock,lnOldCost,lcRollID,lnAlias,llFound
lnAlias = SELECT(0)
llFound = .F.

m.cInvType = lcInvType
m.Style    = lcStyle
IF lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loParentForm)
SELECT FABRIC
LOCATE
llFound = !EOF()
ENDIF

IF llFound .AND. laToIssue[9] <> 0
*!*    *C200255,1 (Begin) Update MATINVJL with custom Voucher no for CON10.
*!*    IF !llFrstTime AND ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0 AND TYPE('lcVoucNo') = 'U'
*!*      lcVoucNo = ""
*!*      llContVout = .T.
*!*      = gfDoTriger("MAPOREC",PADR("GETVOUT",10))
*!*      IF !llContVout
*!*        RETURN
*!*      ENDIF
*!*    ENDIF
*!*    *C200255,1 (Begin)
*--- DOC.
*--- Check if this fabric/color are assigned to the selected
*--- warehouse
*--- DOC.
llFound = .F.
m.cInvType  = lcInvType
m.Style     = lcStyle
m.cWareCode = lcWareCode
m.Dyelot    = SPACE(10)
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loParentForm)
SELECT FABDYE
LOCATE
llFound = !EOF()
ENDIF
IF loParentForm.laSetups[2,2]='Y' .AND. !EMPTY(lcWareCode) .AND. ;
!llFound
*E300725,1 Message : 38029
*E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
*E300725,1 Button : 38001
*E300725,1 Add Cancel
IF gfModalGen('QRM38029B38001','ALERT',loParentForm.lcItmHdr+': '+ALLTRIM(lcStyle)+'|'+lcWareCode) = 1
DO gfAdItemWar WITH lcInvType,lcStyle,SPACE(10),lcWareCode
ELSE
RETURN
ENDIF
ENDIF
llFound  = .F.
m.Dyelot = lcDyelot
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loParentForm)
SELECT FABDYE
LOCATE
llFound = !EOF()
ENDIF
IF loParentForm.laSetups[14,2]='Y' .AND. Fabric.cDye_Flg='Y' AND !EMPTY(lcDyelot) .AND. !llFound AND;
!gfSelDyelot(loParentForm.lcTranType,'F','','',lcWareCode,lcDyelot,lcStyle,lcInvType)
RETURN
ENDIF

*--- DOC.
*--- Get the Gl Link code such that is the fabric link code is empty then
*--- update it with DEFDEF
*--- DOC.
lcFabLinkCode = IIF(EMPTY(Fabric.Link_Code),'DEFDEF',Fabric.Link_Code)

llFound  = .F.
m.Dyelot = SPACE(10)
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loParentForm)
SELECT FABDYE
LOCATE
llFound = !EOF()
ENDIF
IF llFound
lcFabLinkCode = IIF(EMPTY(FabDye.Gl_Link),lcFabLinkCode,FabDye.Gl_Link)
ENDIF
IF loParentForm.laSetups[1,2]='Y'
*--- DOC.
*--- Update the array that used by material control functoin
*--- DOC.
DECLARE laGLDistAr[2,13]
laGLDistAr[1,1] = lcFabLinkCode
laGLDistAr[2,1] = EVALUATE(loParentForm.lcPosHdr+'.LINK_CODE')
laGLDistAr[1,2] = '015'
laGLDistAr[2,2] = '013'
laGLDistAr[1,3] = 1
laGLDistAr[2,3] = -1
STORE 'MA'                                  TO laGLDistAr[1,4],laGLDistAr[2,4]
STORE EVALUATE(loParentForm.lcPosHdr+'.PO') TO laGLDistAr[1,5],laGLDistAr[2,5]
STORE ldIssDate                             TO laGLDistAr[1,6],laGLDistAr[2,6]
STORE loParentForm.lcGlYear                 TO laGLDistAr[1,7],laGLDistAr[2,7]
STORE loParentForm.lcGlPeriod               TO laGLDistAr[1,8],laGLDistAr[2,8]
STORE loParentForm.lcGlDTemp                TO laGLDistAr[1,9],laGLDistAr[2,9]
STORE ''                                    TO laGLDistAr[1,10],laGLDistAr[2,10]
ELSE
DIME laGLDistAr[1,1]
laGLDistAr = ''
ENDIF
PRIVATE laOtherPar
DIMENSION laOtherPar[4,2]
*--- DOC.
*--- List of all variable to be used as parameter in the material control
*--- DOC.
laOtherPar[1,1] = 'llUseACst'
laOtherPar[1,2] = llUseActCost
laOtherPar[2,1] = 'lcOprCode'
laOtherPar[2,2] = lcOprCode
laOtherPar[3,1] = 'lcLotNo'
laOtherPar[3,2] = lcLotNo
laOtherPar[4,1] = 'lcRelCode'
laOtherPar[4,2] = FABRIC.cConvBuy

SELECT (lcOldSheet)
GATHER MEMVAR
IF loParentForm.lcTranType $ 'MID' .AND. laToIssue[9] < 0
*IF !lfUpdWip(loParentForm)
*  RETURN .F.
*ENDIF
ENDIF

PRIVATE lnConnectionHandlar, lcTranCode

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
=oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
RETURN .F.
ENDIF




LOCAL ARRAY laIssue[9]
LOCAL lnI
FOR lnI = 1 TO 9
laIssue[lnI] = -1 * laToIssue[lnI]
ENDFOR

=gfItemCrl('9',lcInvType,lcStyle,lcWareCode,lcDyelot,ldIssDate,ldIssDate,;
EVALUATE(loParentForm.lcPosHdr+'.PO'),@laIssue,lnBuyIssCost/lfGetConv(FABRIC.cConvBuy),;
'','','',@laGLDistAr,'',loParentForm.lcBusDocu,loParentForm.lcStyType,lcRSession,lcISession,;
'','',.F.,lcTranCode,.F.,@laOtherPar)

*!*    *C200256,1 (Begin) Receive Qty for CON10.
*!*    IF !llFrstTime AND ASCAN(laEvntTrig,PADR("PRNTIT",10)) <> 0
*!*      llFromCost = .T.
*!*      lnNoOfCop  = 1
*!*      lnPrgnO    = 1
*!*      lnRep      = 0
*!*      lcTranTit  = "CUT #"
*!*      lnTotQty   = ABS(-1*lnIssued)
*!*      lnCostIt   = lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
*!*      lcRecDate  = "ldIssDate"
*!*      IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you wish to print the received quantities?')=1
*!*        = gfDoTriger("MAPOREC",PADR("PRNTIT",10))
*!*      ENDIF
*!*      llFrstTime = .T.
*!*    ENDIF
*!*    *C200256,1 (Begin)

*--- DOC.
*--- If the system is keep trak of roll and the fabric use rolls
*--- then update ROLLS file
*--- DOC.
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
*  IF loParentForm.laSetups[13,2]='Y' .AND. Fabric.lTrkRolls .AND. lnIssued > 0
*    =gfOpenFile(gcDataDir+'ROLLS',gcDataDir+'Rolapl','SH')

IF loParentForm.laSetups[13,2]='Y' .AND. Fabric.lTrkRolls .AND. laToIssue[9] > 0

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*  =gfOpenFile(oariaapplication.datadir +'ROLLS',oariaapplication.datadir +'Rolapl','SH')
=gfOpenTable(oariaapplication.datadir +'ROLLS',oariaapplication.datadir +'Rolapl','SH')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
SELECT ROLLS

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*SET ORDER TO TAG Rolapl
=gfSetOrder('Rolapl')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

lcRollID = '***** N/A *****'

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
lenclrlen = LEN(gfitemmask("PN", "", "0002"))
lcFabric = SUBSTR(lcstyle,1,7)
lcColor  = RIGHT(lcstyle, lenclrlen)
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*    IF !SEEK(lcRSession+lcISession+lcFabric+lcColor+lcWareCode+lcDyelot+lcRollID)
IF !gfSEEK(lcRSession+lcISession+lcstyle+lcWareCode+lcDyelot+lcRollID)
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

APPEND BLANK
ENDIF
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
*REPLACE cRollItem   WITH lcFabric           ,;
Color       WITH lcColor            ,;
Cwarecode   WITH lcWareCode         ,;
Dyelot      WITH lcDyelot           ,;
Crollid     WITH lcRollID           ,;
Nqtybal     WITH Nqtybal + lnIssued ,;
Nqty        WITH Nqty    + lnIssued ,;
Trancd      WITH '2'                ,;
Ctktno      WITH laData[1]          ,;
Csession    WITH lcISession         ,;
Crsession   WITH lcRSession         ,;
Cisession   WITH lcISession
REPLACE cRollItem   WITH lcFabric           ,;
Color       WITH lcColor            ,;
Cwarecode   WITH lcWareCode         ,;
Dyelot      WITH lcDyelot           ,;
Crollid     WITH lcRollID           ,;
Nqtybal     WITH Nqtybal + laToIssue[9],;
Nqty        WITH Nqty    + laToIssue[9] ,;
Trancd      WITH '2'                ,;
Ctktno      WITH EVALUATE(loParentForm.lcPosHdr+'.PO')          ,;
Csession    WITH lcISession         ,;
Crsession   WITH lcRSession         ,;
Cisession   WITH lcISession

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
REPLACE style WITH lcSTyle
=gfReplace("")

*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[Start]
IF !gfSEEK(lcRSession+SPACE(6)+lcstyle+lcWareCode+lcDyelot+lcRollID)
APPEND BLANK
ENDIF
REPLACE cRollItem   WITH lcFabric           ,;
Color       WITH lcColor            ,;
Cwarecode   WITH lcWareCode         ,;
Dyelot      WITH lcDyelot           ,;
Crollid     WITH lcRollID           ,;
Nqtybal     WITH Nqtybal - laToIssue[9],;
Nqty        WITH 0 ,;
Trancd      WITH '1'                ,;
Ctktno      WITH EVALUATE(loParentForm.lcPosHdr+'.PO')          ,;
Csession    WITH lcRSession         ,;
Crsession   WITH lcRSession         ,;
Cisession   WITH '',;
style WITH lcSTyle
=gfAdd_Info('ROLLS')
=gfReplace("")
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[End]

=gfTableUpdate()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [END]

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]
ENDIF

IF !llFromDel
SELECT (loParentForm.lcTktSheet)
IF SEEK(lcItemType+'1'+lcInvType+lcStyle+SPACE(6)+lcDyelot)
REPLACE Used_Qty  WITH Used_Qty  + laToIssue[9] ,;
Used_Qty1 WITH Used_Qty1 + laToIssue[1] ,;
Used_Qty2 WITH Used_Qty2 + laToIssue[2] ,;
Used_Qty3 WITH Used_Qty3 + laToIssue[3] ,;
Used_Qty4 WITH Used_Qty4 + laToIssue[4] ,;
Used_Qty5 WITH Used_Qty5 + laToIssue[5] ,;
Used_Qty6 WITH Used_Qty6 + laToIssue[6] ,;
Used_Qty7 WITH Used_Qty7 + laToIssue[7] ,;
Used_Qty8 WITH Used_Qty8 + laToIssue[8] ,;
Iss_Qty1  WITH Iss_Qty1  + MAX(laToIssue[1],0) ,;
Iss_Qty2  WITH Iss_Qty2  + MAX(laToIssue[2],0) ,;
Iss_Qty3  WITH Iss_Qty3  + MAX(laToIssue[3],0) ,;
Iss_Qty4  WITH Iss_Qty4  + MAX(laToIssue[4],0) ,;
Iss_Qty5  WITH Iss_Qty5  + MAX(laToIssue[5],0) ,;
Iss_Qty6  WITH Iss_Qty6  + MAX(laToIssue[6],0) ,;
Iss_Qty7  WITH Iss_Qty7  + MAX(laToIssue[7],0) ,;
Iss_Qty8  WITH Iss_Qty8  + MAX(laToIssue[8],0) ,;
Issue_Qty WITH Issue_Qty + MAX(laToIssue[9],0) ,;
Dyelot    WITH lcDyelot
ENDIF

*-- Update the CTKTBOM file.
SELECT (loParentForm.lcCtktBom)
LOCATE FOR TYP+CINVTYPE+ITEM+MFGCODE+DYELOT = lcItemType+lcInvType+lcStyle+SPACE(6)+lcDyelot
IF FOUND()
REPLACE Used_Qty  WITH Used_Qty  + laToIssue[9] ,;
Used_Qty1 WITH Used_Qty1 + laToIssue[1] ,;
Used_Qty2 WITH Used_Qty2 + laToIssue[2] ,;
Used_Qty3 WITH Used_Qty3 + laToIssue[3] ,;
Used_Qty4 WITH Used_Qty4 + laToIssue[4] ,;
Used_Qty5 WITH Used_Qty5 + laToIssue[5] ,;
Used_Qty6 WITH Used_Qty6 + laToIssue[6] ,;
Used_Qty7 WITH Used_Qty7 + laToIssue[7] ,;
Used_Qty8 WITH Used_Qty8 + laToIssue[8] ,;
Iss_Qty1  WITH Iss_Qty1  + MAX(laToIssue[1],0) ,;
Iss_Qty2  WITH Iss_Qty2  + MAX(laToIssue[2],0) ,;
Iss_Qty3  WITH Iss_Qty3  + MAX(laToIssue[3],0) ,;
Iss_Qty4  WITH Iss_Qty4  + MAX(laToIssue[4],0) ,;
Iss_Qty5  WITH Iss_Qty5  + MAX(laToIssue[5],0) ,;
Iss_Qty6  WITH Iss_Qty6  + MAX(laToIssue[6],0) ,;
Iss_Qty7  WITH Iss_Qty7  + MAX(laToIssue[7],0) ,;
Iss_Qty8  WITH Iss_Qty8  + MAX(laToIssue[8],0) ,;
Issue_Qty WITH Issue_Qty + MAX(laToIssue[9],0) ,;
Dyelot    WITH lcDyelot
ENDIF
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(loParentForm.lcCtktBom,lcTranCode,loParentForm.DataSessionId,;
'CIMTYP,CUTTKT,TYP,CINVTYPE,ITEM,MFGCODE,DYELOT','CTKTBOM','CTKTBOM')
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
=oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
=TABLEREVERT(.T.)
RETURN .F.
ELSE
=TABLEUPDATE(.T.,.T.)
ENDIF

*-- Update the BOMLINE file.
SELECT (loParentForm.lcBomLine)
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(loParentForm.lcBomLine,lcTranCode,loParentForm.DataSessionId,;
'CIMTYP,CTYPE,CTKTNO,SHIPNO,LINENO,CBOMTYP,CINVTYPE,STYLE,CINVTYPC,ITEM,MFGCODE,CRSESSION','BOMLINE','BOMLINEU')
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
=oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
=TABLEREVERT(.T.)
RETURN .F.
ELSE
=TABLEUPDATE(.T.,.T.)
ENDIF

*-- Update the BOMCOST file.
SELECT BOMCOST
LOCATE FOR MFGCODE+CWARECODE+CDYELOT+CRSESSION+CISESSION = SPACE(6)+lcWareCode+lcDyelot+lcRSession+lcISession
IF !FOUND()
APPEND BLANK
ENDIF

REPLACE cTktNo    WITH EVALUATE(loParentForm.lcPosHdr+'.PO')  ,;
cWareCode WITH lcWareCode ,;
cDyelot   WITH lcDyelot   ,;
cInvType  WITH lcInvType  ,;
Item      WITH lcStyle    ,;
cBomType  WITH lcItemType ,;
cIMTyp    WITH loParentForm.lcTranType ,;
MfgCode   WITH SPACE(6)   ,;
nTotQty   WITH nTotQty+laToIssue[9],;
nTotCst   WITH nTotCst + (laToIssue[9]*lnBuyIssCost/lfGetConv(FABRIC.cConvBuy)) ,;
dTranDate WITH ldIssDate  ,;
cRSession WITH lcRSession ,;
cISession WITH lcISession ,;
cCostType WITH lcCatgType ,;
nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
nUnitACst WITH nUnitCst  ,;
nTotACst  WITH nTotCst   ,;
cOprCode  WITH lcOprCode ,;
cLotNo    WITH lcLotNo   ,;
Actualize WITH 'N'

IF nTOtQty = 0
DELETE
ENDIF
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('BOMCOST',lcTranCode,loParentForm.DataSessionId,;
'CINVTYPE,ITEM,CWARECODE,CDYELOT,CRSESSION,CISESSION','BOMCOST','POBOMCLS')
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
=oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
=TABLEREVERT(.T.)
RETURN .F.
ELSE
=TABLEUPDATE(.T.,.T.)
ENDIF

*-- Update the POSHDR file.
SELECT (loParentForm.lcPOSHDR)
REPLACE ('nAct_Cost'+lcItemType) WITH EVALUATE('nAct_Cost'+lcItemType) + (laToIssue[9]*lnBuyIssCost/lfGetConv(FABRIC.cConvBuy))
IF loParentForm.lcTranType $ 'ID'
REPLACE ('nfActCost'+lcItemType) WITH EVALUATE('nAct_Cost'+lcItemType+' '+loParentForm.lcDExSign+' nDutyRat '+;
loParentForm.lcDUntSin+' nDCurUnit')
ENDIF

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(loParentForm.lcPosHdr,lcTranCode,loParentForm.DataSessionId,;
'CBUSDOCU,CSTYTYPE,PO','POSHDR','POSHDR')
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
=oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
=TABLEREVERT(.T.)
RETURN .F.
ELSE
=TABLEUPDATE(.T.,.T.)
ENDIF
ENDIF

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
=oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
RETURN .F.
ENDIF

IF loParentForm.lcTranType $ 'MID' .AND. laToIssue[9] < 0
*=lfRetWip()
ENDIF

SELECT (lcOldSheet)
SCATTER MEMVAR
m.TotStk = lfGetTotStk(loParentForm)
SELECT (loParentForm.lcTktSheet)
GATHER FIELDS TotStk MEMVAR
IF loParentForm.laSetups[1,2] = 'Y'
SELECT (loParentForm.lcGlDTemp)
*! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
*!*	REPLACE ALL GlSession WITH loParentForm.lcSession,;
*!*	Tran_Desc WITH IIF(laToIssue[9]>0,'ISSUE','RETURN')+' MAT PO#'+EVALUATE(loParentForm.lcPosHdr+'.PO')
lcGLDISTCursorUpdate = gfGetRemoteProp('lcCursorUpdate','GLDIST')
*! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
*E303957,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]
*!*	    APPEND FROM (oAriaApplication.WorkDir+loParentForm.lcGlDTemp)
SELECT (loParentForm.lcGlDTemp)
SCAN 
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  REPLACE GlSession WITH loParentForm.lcSession,;
          Tran_Desc WITH IIF(laToIssue[9]>0,'ISSUE','RETURN')+' MAT PO#'+EVALUATE(loParentForm.lcPosHdr+'.PO')
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
  SCATTER MEMVAR MEMO
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  *m.Oid = gfTempName()
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
  SELECT GLDIST
  APPEND BLANK
  GATHER MEMVAR MEMO
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  *=gfReplace('')
  IF !EMPTY(lcGLDISTCursorUpdate)
    SELECT (lcGLDISTCursorUpdate)
    APPEND BLANK
    GATHER MEMVAR MEMO 
  ENDIF
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
ENDSCAN
SELECT GLDIST
=gfTableUpdate()
*E303957,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [End]
*! E611847,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
=gfCreateGLEntries(loParentForm.lcGlDTemp,'')
*! E611847,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
SELECT (loParentForm.lcGlDTemp)
ZAP
ENDIF




m.cTrType   = '9'
m.cTrCode   = m.CutTkt
m.cDyelot   = m.Dyelot

*B607879,1 WAM 12/14/2006 Sort Issue/Return log
*IF !lfOpenSql('ITEMJRNL','MATINVJL','STYINVJL','CINVTYPE+STYLE+CTRTYPE+CTRCODE'+;
IIF(EMPTY(m.Dyelot),'','+CDYELOT'),loParentForm)
IF !lfOpenSql('ITEMJRNL','MATINVJL','STYINVJL','CINVTYPE+STYLE+CTRTYPE+CTRCODE'+;
IIF(EMPTY(m.Dyelot),'','+CDYELOT'),loParentForm,.F.,'cInvType,STYLE,cwarecode,cdyelot,crsession,cisession')
*B607879,1 WAM 12/14/2006 (End)

RETURN .F.
ENDIF

*! B608366,1 MMT 11/29/2007 Fix bug of wrong Cost open open Actual cost screen [Start]
IF INLIST(loParentForm.laSetups[11,2],"A","S")
=lfSetIndex('MATINVJL','MATINVJL','cinvtype+ style+ cwarecode+ csession+ cirtype+ ctrcode')
ENDIF
*! B608366,1 MMT 11/29/2007 Fix bug of wrong Cost open open Actual cost screen [End]

SELECT (lnAlias)
ENDIF

*!*************************************************************
*! Name      : lfIssSeq
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/22/2004
*! Purpose   : Issue Fabrics and Inventory mantained trims FIFO/LIFO
*!*************************************************************
*! Calls     : gfModalGen,GLDIST
*!*************************************************************
*! Parameters: lcType       : 'F' FIFO
*!                          : 'I' LIFO
*!             lcItemType   : Cost Item Type
*!             lcCatgType   : Cost Item Category
*!             lcInvType    : Inventory Type
*!             lcStyle      : Fabric
*!             lcWareCode   : Warehouse
*!             lcDyelot     : Dyelot
*!             laIssued     : Issued/Return Quantitys
*!             lcOprCode    : Operation
*!             lcLotNo      : Lot#
*!             ldIssDate    : Issue/Return Date
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfIssSeq()
*!*************************************************************
FUNCTION lfIssSeq
PARAMETERS lcType,lcItemType,lcCatgType,lcInvType,lcStyle,lcWareCode,lcDyelot,;
laIssued,lcOprCode,lcLotNo,ldIssDate
LOCAL lnAlias,laToIssue[9],lcMatInvJl,llFound,lnI,lcI

lnAlias    = SELECT(0)
lcMatInvJl = gfTempName()
llFound = .F.
*--- DOC.
*--- Select all material open lots
*--- DOC.
m.cInvType  = lcInvType
m.Style     = lcStyle
m.cWareCode = lcWareCode
m.cDyelot   = m.Dyelot
IF lfOpenSql('ITEMJRNL',lcMatInvJl,'STYINVJL','CINVTYPE+STYLE+CWARECODE'+;
IIF(EMPTY(m.Dyelot),'','+CDYELOT'),loParentForm)
*B607879,1 WAM 12/14/2006 Set data engine to FVP7 to execute this select statement.
*B607879,1 WAM 12/14/2006 In VFP9, all fields in the SELECT phrase must be included in the GROUP BY phrase
lnEngineBehavior = SET("EngineBehavior")
SET ENGINEBEHAVIOR 70
*B607879,1 WAM 12/14/2006 (End)

SELECT cInvType,Style,cWareCode,cDyelot,cRSession,cISession,nCost,;
cApInvNo,dTrDate,nRecCost,cTrCode,;
SUM(nStk1) AS 'nBal1',SUM(nStk2) AS 'nBal2',SUM(nStk3) AS 'nBal3',SUM(nStk4) AS 'nBal4',;
SUM(nStk5) AS 'nBal5',SUM(nStk6) AS 'nBal6',SUM(nStk7) AS 'nBal7',SUM(nStk8) AS 'nBal8',;
SUM(nTotStk) AS 'nBalance',nUntCstBuy FROM (lcMatInvJl) ;
GROUP BY cInvType,Style,cWareCode,cDyelot,cRSession ;
HAVING nBalance > 0 INTO DBF (oAriaApplication.WorkDir+loParentForm.lcOpenLots)
llFound = (_TALLY > 0)
*B607879,1 WAM 12/14/2006 Return data engine to FVP9
SET ENGINEBEHAVIOR lnEngineBehavior
*B607879,1 WAM 12/14/2006 (End)

ENDIF
IF !llFound
*E300725,1 Message : 38094
*E300725,1 No open lots found for fabric/color
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38094B00000','ALERT',ALLTRIM(gfItemMask("HI","",lcInvType))+': '+lcStyle)
ELSE
INDEX ON cInvType+Style+cWareCode+cDyelot+cRSession TAG (loParentForm.lcOpenLots)
IF lcType = 'I'
SET ORDER TO TAG (loParentForm.lcOpenLots) DESCENDING
ENDIF
GO TOP
SCAN WHILE laIssued[9] > 0
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
laToIssue[lnI] = MIN(laIssued[lnI],EVALUATE('nBal'+lcI))
ENDFOR
laToIssue[9] = MIN(laIssued[9],nBalance)
=lfIssRetFab(lcItemType,lcCatgType,lcInvType,lcStyle,lcWareCode,lcDyelot,;
@laToIssue,nUntCstBuy,lcOprCode,lcLotNo,ldIssDate,cRSession,gfsequence('GLSession'))
FOR lnI = 1 TO 9
laIssued[lnI] = laIssued[lnI] - laToIssue[lnI]
ENDFOR
ENDSCAN
ENDIF

IF USED(loParentForm.lcOpenLots)
USE IN (loParentForm.lcOpenLots)
ENDIF

IF USED(lcMatInvJl)
USE IN (lcMatInvJl)
ENDIF

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfIssRetSty
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/22/2004
*! Purpose   : Issue/Return Styles
*!*************************************************************
*! Calls     : gfModalGen(),gfStyCrl()
*!*************************************************************
*! Parameters: lcItemType   : Cost Item Type
*!             lcCatgType   : Cost Item Category
*!             lcInvType    : Inventory Type
*!             lcStyle      : Style
*!             lcWareCode   : Warehouse
*!             lcDyelot     : Dyelot
*!             laIssued     : Issued/Return Array Quantity
*!             lnIssCost    : Issued/Return unit cost
*!             lcOprCode    : Operation
*!             lcLotNo      : Lot#
*!             ldIssDate    : Issue/Return Date
*!             lcRSession   : Receiving Session#
*!             lcISession   : Issue Session#
*!             llFromDel    : Called upon Deleting The Cost Sheet
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfIssRetSty()
*!*************************************************************
FUNCTION lfIssRetSty
LPARAMETERS lcItemType,lcCatgType,lcInvType,lcStyle,lcWareCode,lcDyelot,;
laIssued,lnIssCost,lcOprCode,lcLotNo,ldIssDate,lcRSession,;
lcISession,llFromDel

LOCAL lcStyLinkCode,lnAmount,lnOldStock,lnOldCost,lnAlias

DECLARE laGlArray[2,13]
STORE '' TO laGlArray
IF loParentForm.laSetups[1,2] = 'Y'
STORE 'IA'                                  TO laGlArray[1,4],laGlArray[2,4]
STORE EVALUATE(loParentForm.lcPosHdr+'.PO') TO laGlArray[1,5],laGlArray[2,5]
STORE ldIssDate                             TO laGlArray[1,6],laGlArray[2,6]
STORE loParentForm.lcGlYear                 TO laGlArray[1,7],laGlArray[2,7]
STORE loParentForm.lcGlPeriod               TO laGlArray[1,8],laGlArray[2,8]
STORE loParentForm.lcGlDTemp                TO laGlArray[1,9],laGlArray[2,9]
STORE ''                                    TO laGlArray[1,10],laGlArray[2,10]
STORE ''                                    TO laGlArray[1,11],laGlArray[2,11]
STORE ''                                    TO laGlArray[1,12],laGlArray[2,12]
STORE ''                                    TO laGlArray[1,13],laGlArray[2,13]
ENDIF
lnAlias = SELECT(0)
*--- DOC.
*--- Check if the style is assigned to the selected warehous before issue it
*--- DOC.
IF SEEK(lcStyle,'Style') .AND. laIssued[9] <> 0
IF loParentForm.laSetups[2,2]='Y' .AND. !EMPTY(lcWareCode) .AND. ;
!SEEK (lcStyle+lcWareCode+SPACE(10),'STYDYE')
*E300725,1 Message : 38029
*E300725,1 Style xxxxx is not assigned to warehouse xxxx
*E300725,1 Button : 38001
*E300725,1 Add Cancel
IF gfModalGen('QRM38029B38001','ALERT',ALLTRIM(gfItemMask("HI"))+': '+ALLTRIM(lcStyle)+'|'+lcWareCode) = 1
*--- DOC.
*--- Call global function to assigne the style to warehouse
*--- DOC.
DO gpAdStyWar WITH lcStyle,SPACE(10),lcWareCode
ELSE
RETURN
ENDIF
ENDIF
IF loParentForm.laSetups[10,2]='Y' .AND. Style.cDye_Flg='Y' AND ;
!EMPTY(lcDyelot) AND !SEEK(lcStyle+lcWareCode+lcDyelot,'StyDye') AND ;
!gfSelDyelot(lcTranType,'S','','',lcWareCode,lcDyelot,lcStyle)
RETURN
ENDIF
lcStyLinkCode = IIF(EMPTY(Style.Link_Code),'DEFDEF',Style.Link_Code)
lcStyLinkCode = IIF(EMPTY(StyDye.Gl_Link),lcStyLinkCode,StyDye.Gl_Link)
IF loParentForm.laSetups[1,2] = 'Y'
laGlArray[1,1] = lcStyLinkCode
laGlArray[1,2] = '006'
laGlArray[1,3] = 1
laGlArray[2,1] = EVALUATE(loParentForm.lcPosHdr+'.LINK_CODE')
laGlArray[2,2] = '013'
laGlArray[2,3] = -1
ENDIF
*--- DOC.
*--- Call style control to issue/return the style
*--- DOC.
IF gfStyCrl('I',lcStyle,lcWareCode,lcDyelot,ldIssDate,EVALUATE(loParentForm.lcPosHdr+'.PO'),;
@laIssued,lnIssCost,'',.F.,'',0,'','',@laGlArray)=0
RETURN
ENDIF

*T20060818.0001(C200876) TMI [Start] Update Qtys to there files 'BinInvJl & WHBINLOC '[Start]
IF ASCAN(loParentForm.laEvntTrig , PADR('DLUPDQTY',10)) <> 0 .AND. loParentForm.mDoTrigger(PADR('ISUSEBIN',10))
llFrmDel = llFromDel  && Create this variable to use within the trigger as I can not use llFromDel since it is LOCAL
=loParentForm.mDoTrigger(PADR('DLUPDQTY',10))
ENDIF
*T20060818.0001(C200876) TMI [End  ]

IF loParentForm.laSetups[10,2]='Y' .AND. Style.cDye_Flg='Y' .AND. ;
SEEK(lcStyle+lcWareCode+lcDyelot,'StyDye') AND StyDye.TotStk < 0
*E300725,1 Message : 38034
*E300725,1 Inventory of style/dyelot: xxx/xxx/xxx has gone below zero
*E300725,1 Button : 00000
*E300725,1 Ok
lcMsg = ALLTRIM(gfItemMask("HI"))+'/'
IF loParentForm.llStyConfg
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMsg = lcMsg + LANG_MFCSSH_CONFIG
lcMsg = lcMsg + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CONFIG,loFormSet.GetHeaderText("LANG_MFCSSH_CONFIG",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMsg = lcMsg + LANG_MFCSSH_DYELOT
lcMsg = lcMsg + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYELOT,loFormSet.GetHeaderText("LANG_MFCSSH_DYELOT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
lcMsg = lcMsg + ' : '+TRIM(lcStyle)+'/'+TRIM(lcDyelot)
=gfModalGen('INM38034B00000','ALERT',lcMsg)
ENDIF
IF loParentForm.laSetups[2,2]='Y' AND SEEK(lcStyle+lcWareCode+SPACE(10),'StyDye') AND StyDye.TotStk <0
*E300725,1 Message : 38034
*E300725,1 Inventory of style: xxx/xxx has gone below zero
*E300725,1 Button : 00000
*E300725,1 Ok
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM38034B00000','ALERT',ALLTRIM(gfItemMask("HI"))+' : '+TRIM(lcStyle)+' '+LANG_MFCSSH_INWARE+': '+lcWareCode)
=gfModalGen('INM38034B00000','ALERT',ALLTRIM(gfItemMask("HI"))+' : '+TRIM(lcStyle)+' '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_INWARE,loFormSet.GetHeaderText("LANG_MFCSSH_INWARE",loFormSet.HeaderAlias))+': '+lcWareCode)
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
IF Style.TotStk < 0
*E300725,1 Message : 38034
*E300725,1 Inventory of Style: xxxxx has gone below zero
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('INM38034B00000','ALERT',ALLTRIM(gfItemMask("HI"))+' : '+TRIM(lcStyle))
ENDIF
IF !llFromDel
*--- DOC.
*--- Start update cuttkt cost sheet file (CTKTBOM temp file)
*--- DOC.
SELECT (loParentForm.lcTktSheet)
IF SEEK(lcItemType+'1'+lcInvType+lcStyle+SPACE(6)+lcDyelot)
REPLACE Used_Qty1 WITH Used_Qty1 - laIssued[1] ,;
Used_Qty2 WITH Used_Qty2 - laIssued[2] ,;
Used_Qty3 WITH Used_Qty3 - laIssued[3] ,;
Used_Qty4 WITH Used_Qty4 - laIssued[4] ,;
Used_Qty5 WITH Used_Qty5 - laIssued[5] ,;
Used_Qty6 WITH Used_Qty6 - laIssued[6] ,;
Used_Qty7 WITH Used_Qty7 - laIssued[7] ,;
Used_Qty8 WITH Used_Qty8 - laIssued[8] ,;
Used_Qty  WITH Used_Qty  - laIssued[9] ,;
Iss_Qty1  WITH Iss_Qty1  - MIN(laIssued[1],0) ,;
Iss_Qty2  WITH Iss_Qty2  - MIN(laIssued[2],0) ,;
Iss_Qty3  WITH Iss_Qty3  - MIN(laIssued[3],0) ,;
Iss_Qty4  WITH Iss_Qty4  - MIN(laIssued[4],0) ,;
Iss_Qty5  WITH Iss_Qty5  - MIN(laIssued[5],0) ,;
Iss_Qty6  WITH Iss_Qty6  - MIN(laIssued[6],0) ,;
Iss_Qty7  WITH Iss_Qty7  - MIN(laIssued[7],0) ,;
Iss_Qty8  WITH Iss_Qty8  - MIN(laIssued[8],0) ,;
Issue_Qty WITH Issue_Qty - MIN(laIssued[9],0)
ENDIF

SELECT (lcOldSheet)
GATHER MEMVAR

SELECT (loParentForm.lcCTktBom)
LOCATE FOR TYP+ITEM+MFGCODE+DYELOT = lcItemType+lcStyle+SPACE(6)+lcDyelot
IF FOUND()
REPLACE Used_Qty1 WITH Used_Qty1 - laIssued[1] ,;
Used_Qty2 WITH Used_Qty2 - laIssued[2] ,;
Used_Qty3 WITH Used_Qty3 - laIssued[3] ,;
Used_Qty4 WITH Used_Qty4 - laIssued[4] ,;
Used_Qty5 WITH Used_Qty5 - laIssued[5] ,;
Used_Qty6 WITH Used_Qty6 - laIssued[6] ,;
Used_Qty7 WITH Used_Qty7 - laIssued[7] ,;
Used_Qty8 WITH Used_Qty8 - laIssued[8] ,;
Used_Qty  WITH Used_Qty  - laIssued[9] ,;
Iss_Qty1  WITH Iss_Qty1  - MIN(laIssued[1],0) ,;
Iss_Qty2  WITH Iss_Qty2  - MIN(laIssued[2],0) ,;
Iss_Qty3  WITH Iss_Qty3  - MIN(laIssued[3],0) ,;
Iss_Qty4  WITH Iss_Qty4  - MIN(laIssued[4],0) ,;
Iss_Qty5  WITH Iss_Qty5  - MIN(laIssued[5],0) ,;
Iss_Qty6  WITH Iss_Qty6  - MIN(laIssued[6],0) ,;
Iss_Qty7  WITH Iss_Qty7  - MIN(laIssued[7],0) ,;
Iss_Qty8  WITH Iss_Qty8  - MIN(laIssued[8],0) ,;
Issue_Qty WITH Issue_Qty - MIN(laIssued[9],0)
ENDIF

*-- Update the BOMCOST file.
SELECT BOMCOST
LOCATE FOR MFGCODE+CWARECODE+CDYELOT+CRSESSION+CISESSION = SPACE(6)+lcWareCode+lcDyelot+lcRSession+lcISession
IF !FOUND()
APPEND BLANK
ENDIF

REPLACE cTktNo    WITH EVALUATE(loParentForm.lcPosHdr+'.PO')  ,;
cWareCode WITH lcWareCode ,;
cDyelot   WITH lcDyelot   ,;
cInvType  WITH lcInvType  ,;
Item      WITH lcStyle    ,;
cBomType  WITH lcItemType ,;
cIMTyp    WITH loParentForm.lcTranType ,;
MfgCode   WITH SPACE(6)   ,;
nTotQty   WITH nTotQty-laIssued[9],;
nTotCst   WITH nTotCst-(laIssued[9]*lnIssCost) ,;
dTranDate WITH ldIssDate  ,;
cRSession WITH lcRSession ,;
cISession WITH lcISession ,;
cCostType WITH lcCatgType ,;
nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
nUnitACst WITH nTotACst-laIssued[9]*lnIssCost ,;
nTotACst  WITH IIF(nTotQty=0,0,nTotACst/nTotQty) ,;
cOprCode  WITH lcOprCode ,;
cLotNo    WITH lcLotNo   ,;
Actualize WITH 'N'

IF nTOtQty = 0
DELETE
ENDIF

*--- DOC.
*--- Update actual cost
*--- DOC.
*-- Update the POSHDR file.
SELECT (loParentForm.lcPOSHDR)
REPLACE ('nAct_Cost'+lcItemType) WITH EVALUATE('nAct_Cost'+lcItemType) - (laIssued[9]*lnIssCost)
IF loParentForm.lcTranType $ 'ID'
REPLACE ('nfActCost'+lcItemType) WITH EVALUATE('nAct_Cost'+lcItemType+' '+loParentForm.lcDExSign+' nDutyRat '+;
loParentForm.lcDUntSin+' nDCurUnit')
ENDIF

DECLARE laTableUpdate[3,2]
laTableUpdate[1,1] = loParentForm.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'

laTableUpdate[2,1] = loParentForm.lcPosHdr
laTableUpdate[2,2] = 'POSHDR'

laTableUpdate[3,1] = 'BOMCOST'
laTableUpdate[3,2] = 'BOMCOST'

=lfTableUpdate(loParentForm)

SELECT (lcOldSheet)
SCATTER MEMVAR
ENDIF

IF loParentForm.laSetups[1,2] = 'Y'
SELECT (loParentForm.lcGlDTemp)
*! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
*REPLACE ALL GlSession WITH loParentForm.lcSession,;
Tran_Desc WITH IIF(laIssued[9]<0,'ISSUE','RETURN')+' STY PO#'+EVALUATE(loParentForm.lcPosHdr+'.PO')
lcGLDISTCursorUpdate = gfGetRemoteProp('lcCursorUpdate','GLDIST')
*! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
*E303957,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]
SELECT (loParentForm.lcGlDTemp)
SCAN 
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  REPLACE  GlSession WITH loParentForm.lcSession,;
           Tran_Desc WITH IIF(laIssued[9]<0,'ISSUE','RETURN')+' STY PO#'+EVALUATE(loParentForm.lcPosHdr+'.PO')
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
  
  SCATTER MEMVAR MEMO
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  *m.Oid = gfTempName()
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
  SELECT GLDIST
  *!*	APPEND FROM (oAriaApplication.WorkDir+loParentForm.lcGlDTemp)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  *=gfReplace('')
  IF !EMPTY(lcGLDISTCursorUpdate)
    SELECT (lcGLDISTCursorUpdate)
    APPEND BLANK
    GATHER MEMVAR MEMO 
  ENDIF  
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
ENDSCAN
SELECT GLDIST
=gfTableUpdate()
 *E303957,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [End]
 *! E611847,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
=gfCreateGLEntries(loParentForm.lcGlDTemp,'')
*! E611847,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]

SELECT (loParentForm.lcGlDTemp)
ZAP
ENDIF
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfIssRetMfg
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/22/2004
*! Purpose   : Issue/Return Trims and Mfg operations
*!*************************************************************
*! Calls     : gfRltFld(),GLDIST
*!*************************************************************
*! Parameters: lcItemType   : Cost Item Type
*!             lcCatgType   : Cost Item Category
*!             lcInvType    : Inventory Type
*!             lcItem       : Trim
*!             lcColor      : Trim Color
*!             lcMfgCode    : Mfg Code
*!             lcWareCode   : Warehouse
*!             lcDyelot     : Dyelot
*!             lnIssued     : Issued/Return Quantity
*!             lnIssCost    : Issued/Return unit cost
*!             ldIssDate    : Issue/Return Date
*!             lcOprCode    : Operation
*!             lcLotNo      : Lot#
*!             lcRSession   : Receiving Session#
*!             lcISession   : Issue Session#
*!             llFromDel    : Called upon Deleting The Cost Sheet
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfIssRetMfg()
*!*************************************************************
FUNCTION lfIssRetMfg
LPARAMETERS lcItemType,lcCatgType,lcInvType,lcItem,lcMfgCode,lcWareCode,;
lcDyelot,lnIssued,lnIssCost,ldIssDate,lcOprCode,lcLotNo,;
lcRSession,lcISession,llFromDel
PRIVATE lnAlias

lnAlias = SELECT(0)
IF lnIssued <> 0
SELECT (lcOldSheet)
GATHER MEMVAR

*--- DOC.
*--- If this function is called to return/issue not to delete cost sheet
*--- and it is update each of the following
*--- 1) CTKTBOM
*--- 2) BOMLINE
*--- 3) POSHDR
*--- DOC.
IF !llFromDel
SELECT (loParentForm.lcCTktBom)
*--- DOC.
*--- Add the issued qty to the used qty
*--- DOC.
LOCATE FOR TYP+ITEM+MFGCODE+DYELOT = lcItemType+lcItem+lcMfgCode+lcDyelot
IF FOUND()
REPLACE Used_Qty  WITH Used_Qty  + lnIssued ,;
Issue_Qty WITH Issue_Qty + MAX(lnIssued,0)
ENDIF

SELECT (loParentForm.lcTktSheet)
IF SEEK(lcItemType+'1'+lcInvType+lcItem+lcMfgCode+lcDyelot)
REPLACE Used_Qty  WITH Used_Qty  + lnIssued ,;
Issue_Qty WITH Issue_Qty + MAX(lnIssued,0)
ENDIF

*-- Update the BOMCOST file.
SELECT BOMCOST
LOCATE FOR MFGCODE+CWARECODE+CDYELOT+CRSESSION+CISESSION = lcMfgCode+lcWareCode+lcDyelot+lcRSession+lcISession
IF !FOUND()
APPEND BLANK
ENDIF

REPLACE cTktNo    WITH EVALUATE(loParentForm.lcPosHdr+'.PO')  ,;
cWareCode WITH lcWareCode ,;
cDyelot   WITH lcDyelot   ,;
cInvType  WITH lcInvType  ,;
Item      WITH lcItem     ,;
cBomType  WITH lcItemType ,;
cIMTyp    WITH loParentForm.lcTranType ,;
MfgCode   WITH lcMfgCode  ,;
nTotQty   WITH nTotQty+lnIssued ,;
nTotCst   WITH nTotCst+lnIssued*lnIssCost ,;
dTranDate WITH ldIssDate  ,;
cRSession WITH lcRSession ,;
cISession WITH lcISession ,;
cCostType WITH lcCatgType ,;
nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
nTotACst  WITH nTotACst+lnIssued*lnIssCost ,;
nUnitACst WITH IIF(nTotQty=0,0,nTotACst/nTotQty) ,;
cOprCode  WITH lcOprCode ,;
cLotNo    WITH lcLotNo   ,;
Actualize WITH 'N'

IF nTOtQty = 0
DELETE
ENDIF

SELECT (loParentForm.lcPOSHDR)
REPLACE ('nAct_Cost'+lcItemType) WITH EVALUATE('nAct_Cost'+lcItemType) + (lnIssued*lnIssCost)
IF loParentForm.lcTranType $ 'ID'
REPLACE ('nfActCost'+lcItemType) WITH EVALUATE('nAct_Cost'+lcItemType+' '+loParentForm.lcDExSign+' nDutyRat '+;
loParentForm.lcDUntSin+' nDCurUnit')
ENDIF
ENDIF
IF loParentForm.laSetups[1,2] = 'Y'
PRIVATE lcMfgGlAcnt
lcMfgGlAcnt = loParentForm.lcMfgGlAcnt
LOCAL ARRAY laMfgRFld[7,2]
=ACOPY(loParentForm.laMfgRFld,laMfgRFld)
=gfRltFld(lcMfgCode,@laMfgRFld,'MFGCODE')

DO GLDIST WITH EVALUATE(loParentForm.lcPosHdr+'.LINK_CODE'),'018',-(lnIssued*lnIssCost),'NL',;
EVALUATE(loParentForm.lcPosHdr+'.PO'),ldIssDate,;
loParentForm.lcGlYear,loParentForm.lcGlPeriod,loParentForm.lcGlDTemp,lcMfgGlAcnt
DO GLDIST WITH EVALUATE(loParentForm.lcPosHdr+'.LINK_CODE'),'013',lnIssued*lnIssCost,'NL',;
EVALUATE(loParentForm.lcPosHdr+'.PO'),ldIssDate,;
loParentForm.lcGlYear,loParentForm.lcGlPeriod,loParentForm.lcGlDTemp
m.cWipAcnt = EVALUATE(loParentForm.lcGlDTemp+'.GlAccount')
SELECT BomCost
REPLACE cWipAcnt   WITH m.cWipAcnt ,;
cLbltyAcnt WITH lcMfgGlAcnt

SELECT (loParentForm.lcGlDTemp)
*! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
*REPLACE ALL GlSession WITH loParentForm.lcSession,;
Tran_Desc WITH IIF(lnIssued>0,'ISSUE','RETURN')+' OPR PO#'+EVALUATE(loParentForm.lcPosHdr+'.PO')
lcGLDISTCursorUpdate = gfGetRemoteProp('lcCursorUpdate','GLDIST')
*! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
*E303957,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]
SELECT (loParentForm.lcGlDTemp)
SCAN 
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  REPLACE GlSession WITH loParentForm.lcSession,;
          Tran_Desc WITH IIF(lnIssued>0,'ISSUE','RETURN')+' OPR PO#'+EVALUATE(loParentForm.lcPosHdr+'.PO')
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
  SCATTER MEMVAR MEMO
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  *m.Oid = gfTempName()
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
  SELECT GLDIST
  *!*	APPEND FROM (oAriaApplication.WorkDir+loParentForm.lcGlDTemp)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  *=gfReplace('')
  IF !EMPTY(lcGLDISTCursorUpdate)
    SELECT (lcGLDISTCursorUpdate)
    APPEND BLANK
    GATHER MEMVAR MEMO 
  ENDIF
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
ENDSCAN
SELECT GLDIST
=gfTableUpdate()
*E303957,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [End]
*! E611847,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
=gfCreateGLEntries(loParentForm.lcGlDTemp,'')
*! E611847,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]

SELECT (loParentForm.lcGlDTemp)
ZAP
ENDIF

DECLARE laTableUpdate[3,2]
laTableUpdate[1,1] = loParentForm.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'

laTableUpdate[2,1] = loParentForm.lcPosHdr
laTableUpdate[2,2] = 'POSHDR'

laTableUpdate[3,1] = 'BOMCOST'
laTableUpdate[3,2] = 'BOMCOST'

=lfTableUpdate(loParentForm)

SELECT (lcOldSheet)
SCATTER MEMVAR
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvNewLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/30/2004
*! Purpose   : Add new Lot
*!*************************************************************
*! Calls     : gfRltFld,MFLOTS.SPX,lfvGenLot,lfRefTotal,lfShOprDt
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvNewLot()
*!*************************************************************
FUNCTION lfvNewLot
LPARAMETERS loFormSet,llFromSave

PRIVATE lcOprCode,lcLotNo,llInHouse,lcContCode,ldTranDate,ldDueDate,lcItemDesc,lcCurrTag,;
lcSizesVal,lcContName,lcOperSeq,llMfgOpr,lnLeadTime,lcMfgGlAcnt

lcLotNo    = PADL(EVALUATE(loFormSet.lcOprHdr+'.nNxtLotNo'),2,'0')
lcOprCode  = EVALUATE(loFormSet.lcOprHdr+'.cOprCode')

lcContCode  = loFormSet.lcContCode
lcContName  = loFormSet.lcContName
lcOperSeq   = loFormSet.lcOperSeq
llInHouse   = loFormSet.llInHouse
llMfgOpr    = loFormSet.llMfgOpr
lnLeadTime  = loFormSet.lnLeadTime
lcMfgGlAcnt = loFormSet.lcMfgGlAcnt
LOCAL ARRAY laMfgRFld[7,2]
=ACOPY(loFormSet.laMfgRFld,laMfgRFld)
=gfRltFld(lcOprCode,@laMfgRFld,'MFGCODE')
loFormSet.lcContCode  = lcContCode
loFormSet.lcContName  = lcContName
loFormSet.lcOperSeq   = lcOperSeq
loFormSet.llInHouse   = llInHouse
loFormSet.llMfgOpr    = llMfgOpr
loFormSet.lnLeadTime  = lnLeadTime
loFormSet.lcMfgGlAcnt = lcMfgGlAcnt

lcItemDesc = ''
ldTranDate = oariaapplication.systemdate
ldDueDate  = oariaapplication.systemdate+loFormSet.lnLeadTime
loFormSet.lcContCode = EVALUATE(loFormSet.lcOprHdr+'.cContCode')
loFormSet.lcContName = EVALUATE(loFormSet.lcOprHdr+'.cContName')
loFormSet.llInHouse  = EVALUATE(loFormSet.lcOprHdr+'.lInHouse')

*--- DOC.
*--- IF the operation related filed not in house and AP module installed
*--- disable the vendor name object
*--- DOC.
llNameStat= loFormSet.llInHouse OR (OCCURS('AP',oAriaApplication.CompanyInstalledModules)=0)
lcCurrTag = ORDER(loFormSet.lcOprDet)
SET ORDER TO TAG ITEM IN (loFormSet.lcOprDet)

DIMENSION laFileStru[33,18]
laFileStru[01,1] = 'lSelect'
laFileStru[01,2] = 'L'
laFileStru[01,3] = 1
laFileStru[01,4] = 0

laFileStru[02,1] = 'Item'
laFileStru[02,2] = 'C'
laFileStru[02,3] = 19
laFileStru[02,4] = 0

laFileStru[03,1] = 'cInvType'
laFileStru[03,2] = 'C'
laFileStru[03,3] = 4
laFileStru[03,4] = 0

laFileStru[04,1] = 'cDyelot'
laFileStru[04,2] = 'C'
laFileStru[04,3] = 10
laFileStru[04,4] = 0

laFileStru[05,1] = 'nBudget1'
laFileStru[05,2] = 'N'
laFileStru[05,3] = 6
laFileStru[05,4] = 0

laFileStru[06,1] = 'nBudget2'
laFileStru[06,2] = 'N'
laFileStru[06,3] = 6
laFileStru[06,4] = 0

laFileStru[07,1] = 'nBudget3'
laFileStru[07,2] = 'N'
laFileStru[07,3] = 6
laFileStru[07,4] = 0

laFileStru[08,1] = 'nBudget4'
laFileStru[08,2] = 'N'
laFileStru[08,3] = 6
laFileStru[08,4] = 0

laFileStru[09,1] = 'nBudget5'
laFileStru[09,2] = 'N'
laFileStru[09,3] = 6
laFileStru[09,4] = 0

laFileStru[10,1] = 'nBudget6'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 6
laFileStru[10,4] = 0

laFileStru[11,1] = 'nBudget7'
laFileStru[11,2] = 'N'
laFileStru[11,3] = 6
laFileStru[11,4] = 0

laFileStru[12,1] = 'nBudget8'
laFileStru[12,2] = 'N'
laFileStru[12,3] = 6
laFileStru[12,4] = 0

laFileStru[13,1] = 'nTotBudget'
laFileStru[13,2] = 'N'
laFileStru[13,3] = 7
laFileStru[13,4] = 0

laFileStru[14,1] = 'nIssued1'
laFileStru[14,2] = 'N'
laFileStru[14,3] = 6
laFileStru[14,4] = 0

laFileStru[15,1] = 'nIssued2'
laFileStru[15,2] = 'N'
laFileStru[15,3] = 6
laFileStru[15,4] = 0

laFileStru[16,1] = 'nIssued3'
laFileStru[16,2] = 'N'
laFileStru[16,3] = 6
laFileStru[16,4] = 0

laFileStru[17,1] = 'nIssued4'
laFileStru[17,2] = 'N'
laFileStru[17,3] = 6
laFileStru[17,4] = 0

laFileStru[18,1] = 'nIssued5'
laFileStru[18,2] = 'N'
laFileStru[18,3] = 6
laFileStru[18,4] = 0

laFileStru[19,1] = 'nIssued6'
laFileStru[19,2] = 'N'
laFileStru[19,3] = 6
laFileStru[19,4] = 0

laFileStru[20,1] = 'nIssued7'
laFileStru[20,2] = 'N'
laFileStru[20,3] = 6
laFileStru[20,4] = 0

laFileStru[21,1] = 'nIssued8'
laFileStru[21,2] = 'N'
laFileStru[21,3] = 6
laFileStru[21,4] = 0

laFileStru[22,1] = 'nTotIss'
laFileStru[22,2] = 'N'
laFileStru[22,3] = 7
laFileStru[22,4] = 0

laFileStru[23,1] = 'nToIssue1'
laFileStru[23,2] = 'N'
laFileStru[23,3] = 5
laFileStru[23,4] = 0

laFileStru[24,1] = 'nToIssue2'
laFileStru[24,2] = 'N'
laFileStru[24,3] = 5
laFileStru[24,4] = 0

laFileStru[25,1] = 'nToIssue3'
laFileStru[25,2] = 'N'
laFileStru[25,3] = 5
laFileStru[25,4] = 0

laFileStru[26,1] = 'nToIssue4'
laFileStru[26,2] = 'N'
laFileStru[26,3] = 5
laFileStru[26,4] = 0

laFileStru[27,1] = 'nToIssue5'
laFileStru[27,2] = 'N'
laFileStru[27,3] = 5
laFileStru[27,4] = 0

laFileStru[28,1] = 'nToIssue6'
laFileStru[28,2] = 'N'
laFileStru[28,3] = 5
laFileStru[28,4] = 0

laFileStru[29,1] = 'nToIssue7'
laFileStru[29,2] = 'N'
laFileStru[29,3] = 5
laFileStru[29,4] = 0

laFileStru[30,1] = 'nToIssue8'
laFileStru[30,2] = 'N'
laFileStru[30,3] = 5
laFileStru[30,4] = 0

laFileStru[31,1] = 'nTotToIss'
laFileStru[31,2] = 'N'
laFileStru[31,3] = 6
laFileStru[31,4] = 0

laFileStru[32,1] = 'DueDate'
laFileStru[32,2] = 'D'
laFileStru[32,3] = 10
laFileStru[32,4] = 0

laFileStru[33,1] = 'lCanRemain'
laFileStru[33,2] = 'L'
laFileStru[33,3] = 1
laFileStru[33,4] = 0

FOR lnI = 7 TO 16
FOR lnJ = 1 TO 33
laFileStru[lnJ,lnI] = ''
ENDFOR
ENDFOR
FOR lnJ = 1 TO 33
STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
ENDFOR

=gfCrtTmp(loFormSet.lcLotsDet,@laFileStru,'cInvType+Item+cDyelot',loFormSet.lcLotsDet)

SELECT (loFormSet.lcTmpTkt)
PRIVATE llDone
llDone = .F.
SCAN FOR IIF(EVALUATE(loFormSet.lcPosHdr+'.CMULTILOT')='M',.T.,cFrstOpr=lcOprCode)
llDone=.T.
SELECT (loFormSet.lcDetFile)
=SEEK(EVALUATE(loFormSet.lcTmpTkt+'.cInvType')+EVALUATE(loFormSet.lcTmpTkt+'.Item')+STR(EVALUATE(loFormSet.lcTmpTkt+'.LineNo'),6))
LOCATE REST WHILE cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode=;
EVALUATE(loFormSet.lcTmpTkt+'.cInvType')+EVALUATE(loFormSet.lcTmpTkt+'.Item')+STR(EVALUATE(loFormSet.lcTmpTkt+'.LineNo'),6);
FOR cShowType+cInvTypC+Item+MfgCode='1'+SPACE(4)+SPACE(19)+lcOprCode
IF FOUND()
lcSizesVal = IIF(EMPTY(cSizes), '12345678',ALLTRIM(cSizes))
IF EVALUATE(loFormSet.lcPosHdr+'.CMULTILOT') = 'M'
SELECT (loFormSet.lcLotsDet)
IF !SEEK(EVALUATE(loFormSet.lcTmpTkt+'.cInvType')+EVALUATE(loFormSet.lcTmpTkt+'.Item')+EVALUATE(loFormSet.lcTmpTkt+'.cDyelot'))
INSERT INTO (loFormSet.lcLotsDet) (cInvType,Item,cDyelot,DueDate) VALUES;
(EVALUATE(loFormSet.lcTmpTkt+'.cInvType') ,EVALUATE(loFormSet.lcTmpTkt+'.Item'),;
EVALUATE(loFormSet.lcTmpTkt+'.cDyelot')  ,ldDueDate)
IF SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode+EVALUATE(loFormSet.lcTmpTkt+'.cInvType')+;
EVALUATE(loFormSet.lcTmpTkt+'.Item')+EVALUATE(loFormSet.lcTmpTkt+'.cDyelot'),loFormSet.lcOprDet)
SELECT (loFormSet.lcOprDet)
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cInvType+Item+cDyelot = ;
loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode+;
EVALUATE(loFormSet.lcTmpTkt+'.cInvType')+EVALUATE(loFormSet.lcTmpTkt+'.Item')+;
EVALUATE(loFormSet.lcTmpTkt+'.cDyelot');
FOR   INLIST(TranCd,'1','4')
SELECT (loFormSet.lcLotsDet)
*--- DOC.
*--- If the trancd is '1' == budget then increase the issued qty else decrease it.
*--- DOC.
FOR lnCntr = 1 TO 8
lcCntrVal = STR(lnCntr,1)
IF lcCntrVal $ lcSizesVal
REPLACE ('nIssued'+lcCntrVal) WITH EVALUATE('nIssued'+lcCntrVal)+(;
EVALUATE(loFormSet.lcOprDet+'.nLotQty'+lcCntrVal) *;
IIF(EVALUATE(loFormSet.lcOprDet+'.TranCd')='1',1,-1))
ENDIF
ENDFOR
ENDSCAN
ENDIF
ENDIF
SELECT (loFormSet.lcLotsDet)
*--- DOC.
*--- Replace the issued with the issued if > zero else replace with 0
*--- DOC.
REPLACE nIssued1 WITH MAX(nIssued1,0),;
nIssued2 WITH MAX(nIssued2,0),;
nIssued3 WITH MAX(nIssued3,0),;
nIssued4 WITH MAX(nIssued4,0),;
nIssued5 WITH MAX(nIssued5,0),;
nIssued6 WITH MAX(nIssued6,0),;
nIssued7 WITH MAX(nIssued7,0),;
nIssued8 WITH MAX(nIssued8,0),;
nTotIss  WITH nIssued1+nIssued2+nIssued3+nIssued4+nIssued5+nIssued6+nIssued7+nIssued8
*--- DOC.
*--- add qty from the ticke line temp file to the budget
*--- DOC.
FOR lnCntr = 1 TO 8
lcCntrVal = STR(lnCntr,1)
IF lcCntrVal $ lcSizesVal
REPLACE ('nBudget'+lcCntrVal) WITH EVALUATE('nBudget'+lcCntrVal) + EVALUATE(loFormSet.lcTmpTkt+'.nQty'+lcCntrVal)
ENDIF
ENDFOR
REPLACE nTotBudget WITH nBudget1+nBudget2+nBudget3+nBudget4+nBudget5+nBudget6+nBudget7+nBudget8
ELSE
IF !SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode+EVALUATE(loFormSet.lcTmpTkt+'.cInvType')+;
EVALUATE(loFormSet.lcTmpTkt+'.Item')+EVALUATE(loFormSet.lcTmpTkt+'.cDyelot'),loFormSet.lcOprDet)
SELECT (loFormSet.lcLotsDet)
IF !SEEK(EVALUATE(loFormSet.lcTmpTkt+'.cInvType')+EVALUATE(loFormSet.lcTmpTkt+'.Item')+EVALUATE(loFormSet.lcTmpTkt+'.cDyelot'))
INSERT INTO (loFormSet.lcLotsDet) (cInvType,Item,cDyelot,DueDate,lSelect) VALUES;
(EVALUATE(loFormSet.lcTmpTkt+'.cInvType'),EVALUATE(loFormSet.lcTmpTkt+'.Item'),;
EVALUATE(loFormSet.lcTmpTkt+'.cDyelot'),ldDueDate,.T.)
ENDIF
FOR lnCntr = 1 TO 8
lcCntrVal = STR(lnCntr,1)
IF lcCntrVal $ lcSizesVal
REPLACE ('nToIssue'+lcCntrVal) WITH EVALUATE('nToIssue'+lcCntrVal) + EVALUATE(loFormSet.lcTmpTkt+'.nQty'+lcCntrVal)
ENDIF
ENDFOR
*B608863,1 WAM 05/18/2009 Fix calculation of operation total budget quantity
*REPLACE nTotToIss WITH nTotToIss+nToIssue1+nToIssue2+nToIssue3+nToIssue4+nToIssue5+nToIssue6+nToIssue7+nToIssue8
REPLACE nTotToIss WITH nToIssue1+nToIssue2+nToIssue3+nToIssue4+nToIssue5+nToIssue6+nToIssue7+nToIssue8
*B608863,1 WAM 05/18/2009 (End)
ENDIF
ENDIF
ENDIF
ENDSCAN
=IIF(llDone,.T.,gfModalGen('INM38186B00000','ALERT'))

SELECT (loFormSet.lcLotsDet)
GO TOP
IF !EOF()
IF EVALUATE(loFormSet.lcPosHdr+'.CMULTILOT') = 'M'
PRIVATE loParentForm
loParentForm = loFormSet
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFLOTS.scx") WITH lcOprCode,lcLotNo
=gfCallForm('MFLOTS',.F.,'lcOprCode,lcLotNo')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
SELECT (loFormSet.lcLotsDet)
ELSE
=lfvGenLot(loFormSet)
ENDIF
ELSE
=IIF(llDone,gfModalGen('INM38288B00000','ALERT'),.T.)
ENDIF

IF SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO'),loFormSet.lcOprDet)
WITH loFormSet.AriaForm1.pgfCstSht.page2
STORE .T. TO .cmdDetLev.Enabled,.cmdIssItm.Enabled,.cmdAutoIssue.Enabled,.cmdModLot.Enabled,.cmdReceive.Enabled,.cmdZoom.Enabled

IF EVALUATE(loFormSet.lcPosHdr+'.CMULTILOT') = 'S'
.cmdNewLot.Enabled = .F.
ENDIF
ENDWITH
ENDIF
IF loFormSet.ariaForm1.pgfCstSht.ActivePage = 2
=lfRefTotal(loFormSet)
ENDIF
SET ORDER TO TAG lcCurrTag IN (loFormSet.lcOprDet)

IF !llFromSave
DECLARE laTableUpdate[4,2]
laTableUpdate[1,1] = loFormSet.lcMfgOprHd
laTableUpdate[1,2] = 'MFGOPRHD'

laTableUpdate[2,1] = loFormSet.lcPosHdr
laTableUpdate[2,2] = 'POSHDR'

laTableUpdate[3,1] = loFormSet.lcPosLn
laTableUpdate[3,2] = 'POSLN'

laTableUpdate[4,1] = loFormSet.lcMfgOprDt
laTableUpdate[4,2] = 'MFGOPRDT'

=lfTableUpdate(loFormSet)
loFormSet.Refresh
ENDIF

*!*************************************************************
*! Name      : lfvGenLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/30/2004
*! Purpose   : Generate new lot
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvGenLot()
*!*************************************************************
FUNCTION lfvGenLot
LPARAMETERS loFormSet

llRelAll = .F.

SELECT (loFormSet.lcLotsDet)
SCAN FOR lSelect
*--- DOC.
*--- Update the detail operatoin temp file
*--- DOC.
SELECT (loFormSet.lcOprDet)
APPEND BLANK
REPLACE cImTyp     WITH loFormSet.lcTranType ,;
cTktNo     WITH EVALUATE(loFormSet.lcPosHdr+'.PO')  ,;
cOprCode   WITH lcOprCode  ,;
cLotNo     WITH lcLotNo    ,;
Item       WITH EVALUATE(loFormSet.lcLotsDet+'.Item') ,;
cInvType   WITH EVALUATE(loFormSet.lcLotsDet+'.cInvType') ,;
lInHouse   WITH loFormSet.llInHouse ,;
cContCode  WITH loFormSet.lcContCode ,;
cContName  WITH loFormSet.lcContName ,;
TranCd     WITH '1'        ,;
dTranDate  WITH ldTranDate ,;
DueDate    WITH EVALUATE(loFormSet.lcLotsDet+'.DueDate') ,;
nLotQty1   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue1') ,;
nLotQty2   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue2') ,;
nLotQty3   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue3') ,;
nLotQty4   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue4') ,;
nLotQty5   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue5') ,;
nLotQty6   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue6') ,;
nLotQty7   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue7') ,;
nLotQty8   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue8') ,;
nLotTotQty WITH EVALUATE(loFormSet.lcLotsDet+'.nTotToIss') ,;
cOperSeq   WITH EVALUATE(loFormSet.lcOprHdr+ '.cOperSeq')  ,;
cDyelot    WITH EVALUATE(loFormSet.lcLotsDet+'.cDyelot')
IF EVALUATE(loFormSet.lcLotsDet+'.lCanRemain')

SELECT (loFormSet.lcTmpTkt)
=SEEK(EVALUATE(loFormSet.lcLotsDet+'.cInvType')+EVALUATE(loFormSet.lcLotsDet+'.Item'))
COUNT REST WHILE cInvType+Item = EVALUATE(loFormSet.lcLotsDet+'.cInvType')+EVALUATE(loFormSet.lcLotsDet+'.Item') ;
FOR cDyelot = EVALUATE(loFormSet.lcLotsDet+'.cDyelot') TO lnRecords
IF lnRecords=1
=SEEK(EVALUATE(loFormSet.lcLotsDet+'.cInvType')+EVALUATE(loFormSet.lcLotsDet+'.Item'))
IF loFormSet.llUseDyelot
LOCATE REST WHILE cInvType+Item = EVALUATE(loFormSet.lcLotsDet+'.cInvType')+EVALUATE(loFormSet.lcLotsDet+'.Item') ;
FOR cDyelot = EVALUATE(loFormSet.lcLotsDet+'.cDyelot')
ENDIF
REPLACE nQty1   WITH nQty1   - MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget1')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued1')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue1'),0) ,;
nQty2   WITH nQty2   - MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget2')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued2')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue2'),0) ,;
nQty3   WITH nQty3   - MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget3')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued3')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue3'),0) ,;
nQty4   WITH nQty4   - MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget4')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued4')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue4'),0) ,;
nQty5   WITH nQty5   - MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget5')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued5')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue5'),0) ,;
nQty6   WITH nQty6   - MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget6')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued6')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue6'),0) ,;
nQty7   WITH nQty7   - MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget7')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued7')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue7'),0) ,;
nQty8   WITH nQty8   - MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget8')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued8')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue8'),0) ,;
nTotQty WITH nTotQty - MAX(EVALUATE(loFormSet.lcLotsDet+'.nTotBudget')-;
EVALUATE(loFormSet.lcLotsDet+'.nTotIss')-;
EVALUATE(loFormSet.lcLotsDet+'.nTotToIss'),0)
ELSE
lcItem = EVALUATE(loFormSet.lcLotsDet+'.cInvType')+EVALUATE(loFormSet.lcLotsDet+'.Item')+EVALUATE(loFormSet.lcLotsDet+'.cDyelot')
SELECT *,00000 AS nCan1,00000 AS nCan2,00000 AS nCan3,00000 AS nCan4,;
00000 AS nCan5,00000 AS nCan6,00000 AS nCan7,00000 AS nCan8,;
000000 AS nTotCan, RECNO() AS nBudRecNo FROM (loFormSet.lcTmpTkt) WHERE cInvType+ITEM+CDYELOT=lcItem;
INTO DBF (oAriaApplication.WorkDir+loFormSet.lcisslogfile)
STORE MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget1')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued1')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue1'),0) TO lnCan1,lnBal1
STORE MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget2')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued2')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue2'),0) TO lnCan2,lnBal2
STORE MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget3')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued3')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue3'),0) TO lnCan3,lnBal3
STORE MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget4')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued4')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue4'),0) TO lnCan4,lnBal4
STORE MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget5')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued5')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue5'),0) TO lnCan5,lnBal5
STORE MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget6')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued6')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue6'),0) TO lnCan6,lnBal6
STORE MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget7')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued7')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue7'),0) TO lnCan7,lnBal7
STORE MAX(EVALUATE(loFormSet.lcLotsDet+'.nBudget8')-;
EVALUATE(loFormSet.lcLotsDet+'.nIssued8')-;
EVALUATE(loFormSet.lcLotsDet+'.nToIssue8'),0) TO lnCan8,lnBal8
STORE MAX(EVALUATE(loFormSet.lcLotsDet+'.nTotBudget')-;
EVALUATE(loFormSet.lcLotsDet+'.nTotIss')-;
EVALUATE(loFormSet.lcLotsDet+'.nTotToIss'),0) TO lnTotCan,lnTotBal
SCATTER MEMVAR
=SEEK(m.Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcConCnTit = loFormSet.lcItmHdr+' '+ALLTRIM(EVALUATE(loFormSet.lcLotsDet+'.Item'))+LANG_MFCSSH_CANQTY
lcConCnTit = loFormSet.lcItmHdr+' '+ALLTRIM(EVALUATE(loFormSet.lcLotsDet+'.Item'))+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CANQTY,loFormSet.GetHeaderText("LANG_MFCSSH_CANQTY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

PRIVATE loParentForm
loParentForm = loFormSet
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFCONCN.scx")
=gfCallForm('MFCONCN')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
USE IN (loFormSet.lcisslogfile)
ENDIF
IF loFormSet.lcTranType <> 'T'
SELECT (loFormSet.lcTmpTkt)
=SEEK(EVALUATE(loFormSet.lcLotsDet+'.cInvType')+EVALUATE(loFormSet.lcLotsDet+'.Item'))
SCAN REST WHILE cInvType+Item+STR(LineNo,6) = EVALUATE(loFormSet.lcLotsDet+'.cInvType')+;
EVALUATE(loFormSet.lcLotsDet+'.Item')
SELECT (loFormSet.lcPosLn)
LOCATE FOR cInvType+Style+STR(LineNo,6)+TranCd =;
EVALUATE(loFormSet.lcTmpTkt+'.cInvType')+EVALUATE(loFormSet.lcTmpTkt+'.Item')+;
STR(EVALUATE(loFormSet.lcTmpTkt+'.LineNo'),6)+'1'
SCATTER FIELDS ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotOrd TO laOrdQty
IF (Ord1 > EVALUATE(loFormSet.lcTmpTkt+'.nQty1') OR Ord2 > EVALUATE(loFormSet.lcTmpTkt+'.nQty2') OR;
Ord3 > EVALUATE(loFormSet.lcTmpTkt+'.nQty3') OR Ord4 > EVALUATE(loFormSet.lcTmpTkt+'.nQty4') OR;
Ord5 > EVALUATE(loFormSet.lcTmpTkt+'.nQty5') OR Ord6 > EVALUATE(loFormSet.lcTmpTkt+'.nQty6') OR;
Ord7 > EVALUATE(loFormSet.lcTmpTkt+'.nQty7') OR Ord8 > EVALUATE(loFormSet.lcTmpTkt+'.nQty8'))
*--- DOC.
*--- Decrease the open qty variable by the issued qty
*--- DOC.
lnOpen1 = Qty1 - EVALUATE(loFormSet.lcTmpTkt+'.nQty1')
lnOpen2 = Qty2 - EVALUATE(loFormSet.lcTmpTkt+'.nQty2')
lnOpen3 = Qty3 - EVALUATE(loFormSet.lcTmpTkt+'.nQty3')
lnOpen4 = Qty4 - EVALUATE(loFormSet.lcTmpTkt+'.nQty4')
lnOpen5 = Qty5 - EVALUATE(loFormSet.lcTmpTkt+'.nQty5')
lnOpen6 = Qty6 - EVALUATE(loFormSet.lcTmpTkt+'.nQty6')
lnOpen7 = Qty7 - EVALUATE(loFormSet.lcTmpTkt+'.nQty7')
lnOpen8 = Qty8 - EVALUATE(loFormSet.lcTmpTkt+'.nQty8')
IF !lfTktAllo(EVALUATE(loFormSet.lcTmpTkt+'.Item'),Dyelot,LineNo,.T.,loFormSet)
SELECT (loFormSet.lcTmpTkt)
REPLACE nQty1   WITH EVALUATE(loFormSet.lcPosLn+'.Qty1') ,;
nQty2   WITH EVALUATE(loFormSet.lcPosLn+'.Qty2') ,;
nQty3   WITH EVALUATE(loFormSet.lcPosLn+'.Qty3') ,;
nQty4   WITH EVALUATE(loFormSet.lcPosLn+'.Qty4') ,;
nQty5   WITH EVALUATE(loFormSet.lcPosLn+'.Qty5') ,;
nQty6   WITH EVALUATE(loFormSet.lcPosLn+'.Qty6') ,;
nQty7   WITH EVALUATE(loFormSet.lcPosLn+'.Qty7') ,;
nQty8   WITH EVALUATE(loFormSet.lcPosLn+'.Qty8') ,;
nTotQty WITH EVALUATE(loFormSet.lcPosLn+'.TotQty')
SELECT (loFormSet.lcLotsDet)
REPLACE lCanRemain WITH .F.
ENDIF
ENDIF
SELECT (loFormSet.lcPOSHDR)
REPLACE TotOrd WITH TotOrd - EVALUATE(loFormSet.lcPosLn+'.TotOrd') + laOrdQty[9]
SELECT (loFormSet.lcPosLn)
REPLACE Ord1   WITH laOrdQty[1] ,;
Ord2   WITH laOrdQty[2] ,;
Ord3   WITH laOrdQty[3] ,;
Ord4   WITH laOrdQty[4] ,;
Ord5   WITH laOrdQty[5] ,;
Ord6   WITH laOrdQty[6] ,;
Ord7   WITH laOrdQty[7] ,;
Ord8   WITH laOrdQty[8] ,;
TotOrd WITH laOrdQty[9]
ENDSCAN
ENDIF
IF loFormSet.ActiveMode='V' AND EVALUATE(loFormSet.lcLotsDet+'.lCanRemain')
SELECT (loFormSet.lcTmpTkt)
lnCanQty = 0
SCAN FOR cInvType+Item = EVALUATE(loFormSet.lcLotsDet+'.cInvType')+EVALUATE(loFormSet.lcLotsDet+'.Item')
SELECT (loFormSet.lcPosLn)
GO EVALUATE(loFormSet.lcTmpTkt+'.nRecNo')
lnCanQty = lnCanQty + MAX(TotQty-EVALUATE(loFormSet.lcTmpTkt+'.nTotQty'),0)
REPLACE Qty1   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty1') ,;
Qty2   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty2') ,;
Qty3   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty3') ,;
Qty4   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty4') ,;
Qty5   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty5') ,;
Qty6   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty6') ,;
Qty7   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty7') ,;
Qty8   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty8') ,;
TotQty WITH EVALUATE(loFormSet.lcTmpTkt+'.nTotQty')
ENDSCAN
SELECT (loFormSet.lcPosHdr)
*--- DOC.
*--- Update the cancel and open pieces
*--- DOC.
REPLACE Cancel WITH Cancel + lnCanQty ,;
Open   WITH Open   - lnCanQty
ENDIF
ENDIF
IF loFormSet.ActiveMode = 'V'
SELECT (loFormSet.lcMFGOPRDT)
APPEND BLANK
REPLACE cImTyp     WITH loFormSet.lcTranType ,;
cTktNo     WITH EVALUATE(loFormSet.lcPosHdr+'.PO'),;
cOprCode   WITH lcOprCode  ,;
cLotNo     WITH lcLotNo    ,;
Item       WITH EVALUATE(loFormSet.lcLotsDet+'.Item') ,;
cInvType   WITH EVALUATE(loFormSet.lcLotsDet+'.cInvType') ,;
lInHouse   WITH loFormSet.llInHouse ,;
cContCode  WITH loFormSet.lcContCode ,;
cContName  WITH loFormSet.lcContName ,;
TranCd     WITH '1'        ,;
dTranDate  WITH ldTranDate ,;
DueDate    WITH EVALUATE(loFormSet.lcLotsDet+'.DueDate') ,;
nLotQty1   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue1') ,;
nLotQty2   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue2') ,;
nLotQty3   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue3') ,;
nLotQty4   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue4') ,;
nLotQty5   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue5') ,;
nLotQty6   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue6') ,;
nLotQty7   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue7') ,;
nLotQty8   WITH EVALUATE(loFormSet.lcLotsDet+'.nToIssue8') ,;
nLotTotQty WITH EVALUATE(loFormSet.lcLotsDet+'.nTotToIss') ,;
cDyelot    WITH EVALUATE(loFormSet.lcLotsDet+'.cDyelot')
ENDIF
ENDSCAN
IF loFormSet.ActiveMode='V'
SELECT (loFormSet.lcMfgOprHd)
LOCATE FOR cOprCode = lcOprCode
IF FOUND()
REPLACE nNxtLotNo WITH nNxtLotNo+1
ENDIF
ENDIF
SELECT (loFormSet.lcOprHdr)
REPLACE nNxtLotNo WITH nNxtLotNo+1
loFormSet.llIssFirst = .T.

*!*************************************************************
*! Name      : lfAdjSButt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/30/2004
*! Purpose   : Select, Unselect, Select all, Select None and Invert items
*!*************************************************************
*! Calls     : lfShNewLot
*!*************************************************************
*! Parameters: lcType  'S' : Select
*!                     'A' : Select All
*!                     'N' : Select None
*!                     'V' : Invert
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfAdjSButt()
*!*************************************************************
FUNCTION lfAdjSButt
PARAMETERS lcType,loFormSet

LOCAL lnRecNo,llDataSel,llDataUSel

SELECT (loParentForm.lcLotsDet)
lnRecNo = RECNO()
DO CASE
CASE lcType = 'S'
REPLACE lSelect   WITH !lSelect,;
nToIssue1 WITH IIF(!lSelect,0,MAX(nBudget1-nIssued1,0)) ,;
nToIssue2 WITH IIF(!lSelect,0,MAX(nBudget2-nIssued2,0)) ,;
nToIssue3 WITH IIF(!lSelect,0,MAX(nBudget3-nIssued3,0)) ,;
nToIssue4 WITH IIF(!lSelect,0,MAX(nBudget4-nIssued4,0)) ,;
nToIssue5 WITH IIF(!lSelect,0,MAX(nBudget5-nIssued5,0)) ,;
nToIssue6 WITH IIF(!lSelect,0,MAX(nBudget6-nIssued6,0)) ,;
nToIssue7 WITH IIF(!lSelect,0,MAX(nBudget7-nIssued7,0)) ,;
nToIssue8 WITH IIF(!lSelect,0,MAX(nBudget8-nIssued8,0)) ,;
nTotToIss WITH IIF(!lSelect,0,nToIssue1+nToIssue2+nToIssue3+nToIssue4+;
nToIssue5+nToIssue6+nToIssue7+nToIssue8)
CASE lcType = 'A'
REPLACE ALL lSelect   WITH .T. ,;
nToIssue1 WITH MAX(nBudget1-nIssued1,0) ,;
nToIssue2 WITH MAX(nBudget2-nIssued2,0) ,;
nToIssue3 WITH MAX(nBudget3-nIssued3,0) ,;
nToIssue4 WITH MAX(nBudget4-nIssued4,0) ,;
nToIssue5 WITH MAX(nBudget5-nIssued5,0) ,;
nToIssue6 WITH MAX(nBudget6-nIssued6,0) ,;
nToIssue7 WITH MAX(nBudget7-nIssued7,0) ,;
nToIssue8 WITH MAX(nBudget8-nIssued8,0) ,;
nTotToIss WITH nToIssue1+nToIssue2+nToIssue3+nToIssue4+nToIssue5+nToIssue6+nToIssue7+nToIssue8
CASE lcType = 'N'
REPLACE ALL lSelect   WITH .F. ,;
nToIssue1 WITH 0 ,;
nToIssue2 WITH 0 ,;
nToIssue3 WITH 0 ,;
nToIssue4 WITH 0 ,;
nToIssue5 WITH 0 ,;
nToIssue6 WITH 0 ,;
nToIssue7 WITH 0 ,;
nToIssue8 WITH 0 ,;
nTotToIss WITH 0
CASE lcType = 'V'
REPLACE ALL lSelect   WITH !lSelect ,;
nToIssue1 WITH IIF(!lSelect,0,MAX(nBudget1-nIssued1,0)) ,;
nToIssue2 WITH IIF(!lSelect,0,MAX(nBudget2-nIssued2,0)) ,;
nToIssue3 WITH IIF(!lSelect,0,MAX(nBudget3-nIssued3,0)) ,;
nToIssue4 WITH IIF(!lSelect,0,MAX(nBudget4-nIssued4,0)) ,;
nToIssue5 WITH IIF(!lSelect,0,MAX(nBudget5-nIssued5,0)) ,;
nToIssue6 WITH IIF(!lSelect,0,MAX(nBudget6-nIssued6,0)) ,;
nToIssue7 WITH IIF(!lSelect,0,MAX(nBudget7-nIssued7,0)) ,;
nToIssue8 WITH IIF(!lSelect,0,MAX(nBudget8-nIssued8,0)) ,;
nTotToIss WITH IIF(!lSelect,0,nToIssue1+nToIssue2+nToIssue3+nToIssue4+;
nToIssue5+nToIssue6+nToIssue7+nToIssue8)
ENDCASE
LOCATE FOR !lSelect
llDataUSel = FOUND()
LOCATE FOR lSelect
llDataSel  = FOUND()
GO lnRecNo
loFormSet.AriaForm1.cmdSelAll.Enabled  = llDataUSel
loFormSet.AriaForm1.cmdSelNone.Enabled = llDataSel
loFormSet.AriaForm1.cmdOk.Enabled      = llDataSel
=lfShNewLot(loFormSet)

*!*************************************************************
*! Name      : lfvModLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/08/2004
*! Purpose   : Modify existing Lot
*!*************************************************************
*! Calls     : MFMODLT.SPX,lfRefTotal(),lfShOprDt()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvModLot()
*!*************************************************************
FUNCTION lfvModLot
LPARAMETERS loFormSet

PRIVATE lcOprCode,lcLotNo,llInHouse,lcContCode,ldTranDate,;
ldDueDate,lcItemDesc,lnLotRcv1,lnLotCan1,lnLotDmg1,lnLotRcv2,;
lnLotCan2,lnLotDmg2,lnLotRcv3,lnLotCan3,lnLotDmg3,lnLotRcv4,;
lnLotCan4,lnLotDmg4,lnLotRcv5,lnLotCan5,lnLotDmg5,lnLotRcv6,;
lnLotCan6,lnLotDmg6,lnLotRcv7,lnLotCan7,lnLotDmg7,lnLotRcv8,;
lnLotCan8,lnLotDmg8,lnTotRcv,lnTotCan,lnTotDmg,lcCurrTag

STORE 0 TO lnLotRcv1,lnLotCan1,lnLotDmg1,lnLotRcv2,lnLotCan2,lnLotDmg2,;
lnLotRcv3,lnLotCan3,lnLotDmg3,lnLotRcv4,lnLotCan4,lnLotDmg4,;
lnLotRcv5,lnLotCan5,lnLotDmg5,lnLotRcv6,lnLotCan6,lnLotDmg6,;
lnLotRcv7,lnLotCan7,lnLotDmg7,lnLotRcv8,lnLotCan8,lnLotDmg8,;
lnTotRcv,lnTotCan,lnTotDmg

lcLotNo    = EVALUATE(loFormSet.lcOprDet+'.cLotNo')
lcOprCode  = EVALUATE(loFormSet.lcOprDet+'.cOprCode')
lcItemDesc = ''
ldTranDate = EVALUATE(loFormSet.lcOprDet+'.dTranDate')
ldDueDate  = EVALUATE(loFormSet.lcOprDet+'.DueDate')
lcContCode = EVALUATE(loFormSet.lcOprDet+'.cContCode')
lcContName = EVALUATE(loFormSet.lcOprDet+'.cContName')
llInHouse  = EVALUATE(loFormSet.lcOprDet+'.lInHouse')
llNameStat = llInHouse OR (OCCURS('AP',oAriaApplication.CompanyInstalledModules)=0)

DIMENSION laFileStru[34,18]
laFileStru[01,1] = 'lSelect'
laFileStru[01,2] = 'L'
laFileStru[01,3] = 1
laFileStru[01,4] = 0

laFileStru[02,1] = 'Item'
laFileStru[02,2] = 'C'
laFileStru[02,3] = 19
laFileStru[02,4] = 0

laFileStru[03,1] = 'cInvType'
laFileStru[03,2] = 'C'
laFileStru[03,3] = 4
laFileStru[03,4] = 0

laFileStru[04,1] = 'cDyelot'
laFileStru[04,2] = 'C'
laFileStru[04,3] = 10
laFileStru[04,4] = 0

laFileStru[05,1] = 'nBudget1'
laFileStru[05,2] = 'N'
laFileStru[05,3] = 6
laFileStru[05,4] = 0

laFileStru[06,1] = 'nBudget2'
laFileStru[06,2] = 'N'
laFileStru[06,3] = 6
laFileStru[06,4] = 0

laFileStru[07,1] = 'nBudget3'
laFileStru[07,2] = 'N'
laFileStru[07,3] = 6
laFileStru[07,4] = 0

laFileStru[08,1] = 'nBudget4'
laFileStru[08,2] = 'N'
laFileStru[08,3] = 6
laFileStru[08,4] = 0

laFileStru[09,1] = 'nBudget5'
laFileStru[09,2] = 'N'
laFileStru[09,3] = 6
laFileStru[09,4] = 0

laFileStru[10,1] = 'nBudget6'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 6
laFileStru[10,4] = 0

laFileStru[11,1] = 'nBudget7'
laFileStru[11,2] = 'N'
laFileStru[11,3] = 6
laFileStru[11,4] = 0

laFileStru[12,1] = 'nBudget8'
laFileStru[12,2] = 'N'
laFileStru[12,3] = 6
laFileStru[12,4] = 0

laFileStru[13,1] = 'nTotBudget'
laFileStru[13,2] = 'N'
laFileStru[13,3] = 7
laFileStru[13,4] = 0

laFileStru[14,1] = 'nIssued1'
laFileStru[14,2] = 'N'
laFileStru[14,3] = 6
laFileStru[14,4] = 0

laFileStru[15,1] = 'nIssued2'
laFileStru[15,2] = 'N'
laFileStru[15,3] = 6
laFileStru[15,4] = 0

laFileStru[16,1] = 'nIssued3'
laFileStru[16,2] = 'N'
laFileStru[16,3] = 6
laFileStru[16,4] = 0

laFileStru[17,1] = 'nIssued4'
laFileStru[17,2] = 'N'
laFileStru[17,3] = 6
laFileStru[17,4] = 0

laFileStru[18,1] = 'nIssued5'
laFileStru[18,2] = 'N'
laFileStru[18,3] = 6
laFileStru[18,4] = 0

laFileStru[19,1] = 'nIssued6'
laFileStru[19,2] = 'N'
laFileStru[19,3] = 6
laFileStru[19,4] = 0

laFileStru[20,1] = 'nIssued7'
laFileStru[20,2] = 'N'
laFileStru[20,3] = 6
laFileStru[20,4] = 0

laFileStru[21,1] = 'nIssued8'
laFileStru[21,2] = 'N'
laFileStru[21,3] = 6
laFileStru[21,4] = 0

laFileStru[22,1] = 'nTotIss'
laFileStru[22,2] = 'N'
laFileStru[22,3] = 7
laFileStru[22,4] = 0

laFileStru[23,1] = 'nToIssue1'
laFileStru[23,2] = 'N'
laFileStru[23,3] = 5
laFileStru[23,4] = 0

laFileStru[24,1] = 'nToIssue2'
laFileStru[24,2] = 'N'
laFileStru[24,3] = 5
laFileStru[24,4] = 0

laFileStru[25,1] = 'nToIssue3'
laFileStru[25,2] = 'N'
laFileStru[25,3] = 5
laFileStru[25,4] = 0

laFileStru[26,1] = 'nToIssue4'
laFileStru[26,2] = 'N'
laFileStru[26,3] = 5
laFileStru[26,4] = 0

laFileStru[27,1] = 'nToIssue5'
laFileStru[27,2] = 'N'
laFileStru[27,3] = 5
laFileStru[27,4] = 0

laFileStru[28,1] = 'nToIssue6'
laFileStru[28,2] = 'N'
laFileStru[28,3] = 5
laFileStru[28,4] = 0

laFileStru[29,1] = 'nToIssue7'
laFileStru[29,2] = 'N'
laFileStru[29,3] = 5
laFileStru[29,4] = 0

laFileStru[30,1] = 'nToIssue8'
laFileStru[30,2] = 'N'
laFileStru[30,3] = 5
laFileStru[30,4] = 0

laFileStru[31,1] = 'nTotToIss'
laFileStru[31,2] = 'N'
laFileStru[31,3] = 6
laFileStru[31,4] = 0

laFileStru[32,1] = 'DueDate'
laFileStru[32,2] = 'D'
laFileStru[32,3] = 10
laFileStru[32,4] = 0

laFileStru[33,1] = 'lCanRemain'
laFileStru[33,2] = 'L'
laFileStru[33,3] = 1
laFileStru[33,4] = 0

laFileStru[34,1] = 'TranCd'
laFileStru[34,2] = 'C'
laFileStru[34,3] = 1
laFileStru[34,4] = 0

FOR lnI = 7 TO 16
FOR lnJ = 1 TO 34
laFileStru[lnJ,lnI] = ''
ENDFOR
ENDFOR
FOR lnJ = 1 TO 34
STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
ENDFOR

=gfCrtTmp(loFormSet.lcLotsDet,@laFileStru,'cInvType+Item+cDyelot+TranCd',loFormSet.lcLotsDet)

SELECT (loFormSet.lcOprDet)
lcCurrTag = ORDER(loFormSet.lcOprDet)
lnOprRecNo= RECNO(loFormSet.lcOprDet)
SET ORDER TO TAG Lot
=SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode+lcLotNo)
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo = loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+lcOprCode+lcLotNo
SELECT (loFormSet.lcLotsDet)
APPEND BLANK
REPLACE Item      WITH EVALUATE(loFormSet.lcOprDet+'.Item')      ,;
cInvType  WITH EVALUATE(loFormSet.lcOprDet+'.cInvType')  ,;
nToIssue1 WITH EVALUATE(loFormSet.lcOprDet+'.nLotQty1')  ,;
nToIssue2 WITH EVALUATE(loFormSet.lcOprDet+'.nLotQty2')  ,;
nToIssue3 WITH EVALUATE(loFormSet.lcOprDet+'.nLotQty3')  ,;
nToIssue4 WITH EVALUATE(loFormSet.lcOprDet+'.nLotQty4')  ,;
nToIssue5 WITH EVALUATE(loFormSet.lcOprDet+'.nLotQty5')  ,;
nToIssue6 WITH EVALUATE(loFormSet.lcOprDet+'.nLotQty6')  ,;
nToIssue7 WITH EVALUATE(loFormSet.lcOprDet+'.nLotQty7')  ,;
nToIssue8 WITH EVALUATE(loFormSet.lcOprDet+'.nLotQty8')  ,;
nTotToIss WITH EVALUATE(loFormSet.lcOprDet+'.nLotTotQty'),;
cDyelot   WITH EVALUATE(loFormSet.lcOprDet+'.cDyelot')   ,;
TranCd    WITH EVALUATE(loFormSet.lcOprDet+'.TranCd')
ENDSCAN
SELECT (loFormSet.lcLotsDet)
SET FILTER TO TRANCD = '1'
GO TOP

PRIVATE loParentForm
loParentForm = loFormSet
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFMODLT.SCX") WITH lcOprCode,lcLotNo
=gfCallForm('MFMODLT',.F.,'lcOprCode,lcLotNo')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
IF !SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO'),loFormSet.lcOprDet)
WITH loFormSet.AriaForm1.pgfCstSht.page2
.cmdZoom.Enabled      = .F.
.cmdReceive.Enabled   = .F.
.cmdModLot.Enabled    = .F.
.cmdAutoIssue.Enabled = .F.
.cmdIssItm.Enabled    = .F.
.cmdDetLev.Enabled    = .F.
ENDWITH
ENDIF
=lfRefTotal(loFormSet)
SELECT (loFormSet.lcOprDet)
SET ORDER TO TAG lcCurrTag
GO lnOprRecNo

*!*************************************************************
*! Name      : lfvOkModLt
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/09/2004
*! Purpose   : Accept lot modifications
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvOkModLt()
*!*************************************************************
FUNCTION lfvOkModLt

LOCAL lcCurrTag
lcCurrTag = ORDER(loParentForm.lcOprDet)
SET ORDER TO TAG 'LOT' IN (loParentForm.lcOprDet)
SELECT (loParentForm.lcLotsDet)
SET FILTER TO
SCAN
SELECT (loParentForm.lcOprDet)
IF SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+lcOprCode+lcLotNo+;
EVALUATE(loParentForm.lcLotsDet+'.cInvType')+EVALUATE(loParentForm.lcLotsDet+'.Item')+;
EVALUATE(loParentForm.lcLotsDet+'.TranCd')+EVALUATE(loParentForm.lcLotsDet+'.cDyelot'))
*--- DOC
*--- Update the operatoin detail with the new issued qty
*--- DOC
REPLACE nLotQty1   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue1'),;
nLotQty2   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue2'),;
nLotQty3   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue3'),;
nLotQty4   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue4'),;
nLotQty5   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue5'),;
nLotQty6   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue6'),;
nLotQty7   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue7'),;
nLotQty8   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue8'),;
nLotTotQty WITH EVALUATE(loParentForm.lcLotsDet+'.nTotToIss'),;
dTranDate  WITH ldTranDate,;
DueDate    WITH ldDueDate,;
cDyelot    WITH EVALUATE(loParentForm.lcLotsDet+'.cDyelot')

IF loParentForm.ActiveMode='V'
SELECT (loParentForm.lcMFGOPRDT)
LOCATE FOR coprcode+clotno+trancd+cInvType+Item+cDyelot = lcOprCode+lcLotNo+;
EVALUATE(loParentForm.lcLotsDet+'.Trancd')+EVALUATE(loParentForm.lcLotsDet+'.cInvType')+;
EVALUATE(loParentForm.lcLotsDet+'.Item')+EVALUATE(loParentForm.lcLotsDet+'.cDyelot')
IF FOUND()
REPLACE nLotQty1   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue1'),;
nLotQty2   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue2'),;
nLotQty3   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue3'),;
nLotQty4   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue4'),;
nLotQty5   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue5'),;
nLotQty6   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue6'),;
nLotQty7   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue7'),;
nLotQty8   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue8'),;
nLotTotQty WITH EVALUATE(loParentForm.lcLotsDet+'.nTotToIss'),;
dTranDate  WITH ldTranDate,;
DueDate    WITH ldDueDate,;
lInHouse   WITH llInHouse,;
cContCode  WITH lcContCode,;
cContName  WITH lcContName
ENDIF
ENDIF
ELSE
APPEND BLANK
REPLACE cImTyp     WITH loParentForm.lcTranType ,;
cTktNo     WITH EVALUATE(loParentForm.lcPosHdr+'.PO')  ,;
cOprCode   WITH lcOprCode  ,;
cLotNo     WITH lcLotNo    ,;
Item       WITH EVALUATE(loParentForm.lcLotsDet+'.Item'),;
cInvType   WITH EVALUATE(loParentForm.lcLotsDet+'.cInvType'),;
TranCd     WITH EVALUATE(loParentForm.lcLotsDet+'.TranCd'),;
dTranDate  WITH ldTranDate,;
DueDate    WITH EVALUATE(loParentForm.lcLotsDet+'.DueDate'),;
nLotQty1   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue1'),;
nLotQty2   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue2'),;
nLotQty3   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue3'),;
nLotQty4   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue4'),;
nLotQty5   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue5'),;
nLotQty6   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue6'),;
nLotQty7   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue7'),;
nLotQty8   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue8'),;
nLotTotQty WITH EVALUATE(loParentForm.lcLotsDet+'.nTotToIss'),;
cDyelot    WITH EVALUATE(loParentForm.lcLotsDet+'.cDyelot')

IF loParentForm.ActiveMode = 'V'
SELECT (loParentForm.lcMFGOPRDT)
LOCATE FOR coprcode+clotno+trancd+cInvType+Item+cDyelot = lcOprCode+lcLotNo+;
EVALUATE(loParentForm.lcLotsDet+'.Trancd')+EVALUATE(loParentForm.lcLotsDet+'.cInvType')+;
EVALUATE(loParentForm.lcLotsDet+'.Item')+EVALUATE(loParentForm.lcLotsDet+'.cDyelot')
IF !FOUND()
APPEND BLANK
REPLACE cImTyp     WITH loParentForm.lcTranType ,;
cTktNo     WITH EVALUATE(loParentForm.lcPosHdr+'.PO')  ,;
cOprCode   WITH lcOprCode  ,;
cLotNo     WITH lcLotNo    ,;
Item       WITH EVALUATE(loParentForm.lcLotsDet+'.Item'),;
cInvType   WITH EVALUATE(loParentForm.lcLotsDet+'.cInvType'),;
TranCd     WITH EVALUATE(loParentForm.lcLotsDet+'.TranCd'),;
dTranDate  WITH ldTranDate,;
DueDate    WITH EVALUATE(loParentForm.lcLotsDet+'.DueDate'),;
nLotQty1   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue1'),;
nLotQty2   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue2'),;
nLotQty3   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue3'),;
nLotQty4   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue4'),;
nLotQty5   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue5'),;
nLotQty6   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue6'),;
nLotQty7   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue7'),;
nLotQty8   WITH EVALUATE(loParentForm.lcLotsDet+'.nToIssue8'),;
nLotTotQty WITH EVALUATE(loParentForm.lcLotsDet+'.nTotToIss'),;
cDyelot    WITH EVALUATE(loParentForm.lcLotsDet+'.cDyelot')
ENDIF
ENDIF
ENDIF
ENDSCAN
SELECT (loParentForm.lcOprDet)
=SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+lcOprCode+lcLotNo)
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd=;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+lcOprCode+lcLotNo
REPLACE lInHouse   WITH llInHouse  ,;
cContCode  WITH lcContCode ,;
cContName  WITH lcContName
IF TranCd='1'
SELECT (loParentForm.lcLotsDet)
LOCATE FOR cInvType+Item = EVALUATE(loParentForm.lcOprDet+'.cInvType')+EVALUATE(loParentForm.lcOprDet+'.Item')
IF !FOUND()
SELECT (loParentForm.lcMFGOPRDT)
LOCATE FOR coprcode+clotno+trancd+cInvType+Item+cDyelot = lcOprCode+lcLotNo+'1'+;
EVALUATE(loParentForm.lcOprDet+'.cInvType')+EVALUATE(loParentForm.lcOprDet+'.Item')+;
EVALUATE(loParentForm.lcOprDet+'.cDyelot')
IF FOUND()
DELETE
ENDIF
SELECT (loParentForm.lcOprDet)
DELETE
ENDIF
ENDIF
ENDSCAN
IF loParentForm.ActiveMode='V'
SELECT (loParentForm.lcMFGOPRDT)
REPLACE lInHouse   WITH llInHouse  ,;
cContCode  WITH lcContCode ,;
cContName  WITH lcContName ;
FOR coprcode+clotno = lcOprCode+lcLotNo
ENDIF

DECLARE laTableUpdate[1,2]
laTableUpdate[1,1] = loParentForm.lcMfgOprDt
laTableUpdate[1,2] = 'MFGOPRDT'

=lfTableUpdate(loParentForm)

SELECT (loParentForm.lcOprDet)
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)

*!*************************************************************
*! Name      : lfShNewLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/12/2004
*! Purpose   : Show new lot details
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShNewLot()
*!*************************************************************
FUNCTION lfShNewLot
LPARAMETERS loFormSet

LOCAL lcItemFile
IF loParentForm.lcTranType='T'
lcItemFile = 'Fabric'
m.cInvType = EVALUATE(loParentForm.lcLotsDet+'.cInvType')
m.Style    = EVALUATE(loParentForm.lcLotsDet+'.Item')
=lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loParentForm)
ELSE
lcItemFile = 'Style'
=SEEK(EVALUATE(loParentForm.lcLotsDet+'.Item'),'Style')
ENDIF
=SEEK('S'+EVALUATE(lcItemFile+'.Scale'),'Scale')
lcItemDesc = EVALUATE(lcItemFile+'.Desc1')

SELECT (loParentForm.lcLotsDet)
WITH loFormSet.AriaForm1
IF !lSelect
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdSelect.Caption = LANG_MFCSSH_SELECT
.cmdSelect.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_SELECT,loFormSet.GetHeaderText("LANG_MFCSSH_SELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

.DtpickerDate.Enabled = .F.
.cntToIssue.Enabled = .F.
ELSE
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdSelect.Caption = LANG_MFCSSH_UNSELECT
.cmdSelect.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_UNSELECT,loFormSet.GetHeaderText("LANG_MFCSSH_UNSELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

.DtpickerDate.Enabled = .T.
.cntToIssue.Enabled = .T.
.cntToIssue.SetFocus
ENDIF
.cntToIssue.Scale = SCALE.SCALE
IF lSelect AND MAX(nBudget1-nIssued1-nToIssue1,0)+MAX(nBudget2-nIssued2-nToIssue2,0)+;
MAX(nBudget3-nIssued3-nToIssue3,0)+MAX(nBudget4-nIssued4-nToIssue4,0)+;
MAX(nBudget5-nIssued5-nToIssue5,0)+MAX(nBudget6-nIssued6-nToIssue6,0)+;
MAX(nBudget7-nIssued7-nToIssue7,0)+MAX(nBudget8-nIssued8-nToIssue8,0) > 0
.chkCanRemain.Enabled = .T.
ELSE
REPLACE lCanRemain WITH .F.
.chkCanRemain.Enabled = .F.
ENDIF
ENDWITH
loFormSet.Refresh

*!*************************************************************
*! Name      : lfShModLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/09/2004
*! Purpose   : Show modified lot details
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShModLot()
*!*************************************************************
FUNCTION lfShModLot
LPARAMETERS loFormSet

LOCAL lcCurrTag,lcItemFile
IF loParentForm.lcTranType='T'
lcItemFile = 'Fabric'
m.cInvType = EVALUATE(loParentForm.lcLotsDet+'.cInvType')
m.Style    = EVALUATE(loParentForm.lcLotsDet+'.Item')
=lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loParentForm)
ELSE
lcItemFile = 'Style'
=SEEK(EVALUATE(loParentForm.lcLotsDet+'.Item'),'Style')
ENDIF
=SEEK('S'+EVALUATE(lcItemFile+'.Scale'),'Scale')
lcItemDesc = EVALUATE(lcItemFile+'.Desc1')

SELECT (loParentForm.lcLotsDet)
SET FILTER TO
lcItem    = Item
lcInvType = cInvType
lcDyelot  = cDyelot
=SEEK(lcInvType+lcItem+lcDyelot+'2')
lnLotRcv1 = nToIssue1
lnLotRcv2 = nToIssue2
lnLotRcv3 = nToIssue3
lnLotRcv4 = nToIssue4
lnLotRcv5 = nToIssue5
lnLotRcv6 = nToIssue6
lnLotRcv7 = nToIssue7
lnLotRcv8 = nToIssue8
lnTotRcv  = nTotToIss

=SEEK(lcInvType+lcItem+lcDyelot+'4')
lnLotCan1 = nToIssue1
lnLotCan2 = nToIssue2
lnLotCan3 = nToIssue3
lnLotCan4 = nToIssue4
lnLotCan5 = nToIssue5
lnLotCan6 = nToIssue6
lnLotCan7 = nToIssue7
lnLotCan8 = nToIssue8
lnTotCan  = nTotToIss

=SEEK(lcInvType+lcItem+lcDyelot+'3')
lnLotDmg1 = nToIssue1
lnLotDmg2 = nToIssue2
lnLotDmg3 = nToIssue3
lnLotDmg4 = nToIssue4
lnLotDmg5 = nToIssue5
lnLotDmg6 = nToIssue6
lnLotDmg7 = nToIssue7
lnLotDmg8 = nToIssue8
lnTotDmg  = nTotToIss

=SEEK(lcInvType+lcItem+lcDyelot+'1')

SELECT(loParentForm.lcLotsDet)
SET FILTER TO TRANCD = '1'
loFormSet.Refresh

*!*************************************************************
*! Name      : lfvAddItm
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/09/2004
*! Purpose   : Add Style/ Fabric-Color from a lot
*!*************************************************************
*! Calls     : ARIABROW,lfShModLot
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvAddItm()
*!*************************************************************
FUNCTION lfvAddItm
LPARAMETERS loFormSet

PRIVATE lcBrFields,lcFile_Ttl
lcCurrTag = ORDER(loParentForm.lcOprDet)
SET ORDER TO TAG ITEM IN (loParentForm.lcOprDet)

SELECT Item,cInvType,cDyelot,SUM(nQty1) AS nQty1,SUM(nQty2) AS nQty2,SUM(nQty3) AS nQty3,;
SUM(nQty4) AS nQty4,SUM(nQty5) AS nQty5,SUM(nQty6) AS nQty6,SUM(nQty7) AS nQty7,;
SUM(nQty8) AS nQty8,SUM(nTotQty) AS nTotQty FROM (loParentForm.lcTmpTkt) ;
WHERE !(cInvType+Item+cDyelot IN (SELECT cInvType+Item+cDyelot FROM (loParentForm.lcLotsDet))) ;
GROUP BY cInvType,Item,cDyelot INTO DBF (oAriaApplication.WorkDir+loParentForm.lcIsslogFile)
SELECT (loParentForm.lcIsslogFile)
SCAN
IF SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+lcOprCode+;
EVALUATE(loParentForm.lcIsslogFile+'.cInvType')+EVALUATE(loParentForm.lcIsslogFile+'.Item')+;
EVALUATE(loParentForm.lcIsslogFile+'.cDyelot'),loParentForm.lcOprDet) .AND.;
EVALUATE(loParentForm.lcOprDet+'.cLotNo') <> lcLotNo
SELECT (loParentForm.lcOprDet)
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cInvtype+Item+cDyelot = loParentForm.lcTranType+;
EVALUATE(loParentForm.lcPosHdr+'.PO')+lcOprCode+EVALUATE(loParentForm.lcIsslogFile+'.cInvType')+;
EVALUATE(loParentForm.lcIsslogFile+'.Item')+EVALUATE(loParentForm.lcIsslogFile+'.cDyelot');
FOR   INLIST(TranCd,'1','4') .AND. cLotNo <> lcLotNo
SELECT (loParentForm.lcIsslogFile)
REPLACE nQty1   WITH nQty1 - (IIF(EVALUATE(loParentForm.lcOprDet+'.TranCd')='1',1,-1)*;
EVALUATE(loParentForm.lcOprDet+'.nLotQty1')) ,;
nQty2   WITH nQty2 - (IIF(EVALUATE(loParentForm.lcOprDet+'.TranCd')='1',1,-1)*;
EVALUATE(loParentForm.lcOprDet+'.nLotQty2')) ,;
nQty3   WITH nQty3 - (IIF(EVALUATE(loParentForm.lcOprDet+'.TranCd')='1',1,-1)*;
EVALUATE(loParentForm.lcOprDet+'.nLotQty3')) ,;
nQty4   WITH nQty4 - (IIF(EVALUATE(loParentForm.lcOprDet+'.TranCd')='1',1,-1)*;
EVALUATE(loParentForm.lcOprDet+'.nLotQty4')) ,;
nQty5   WITH nQty5 - (IIF(EVALUATE(loParentForm.lcOprDet+'.TranCd')='1',1,-1)*;
EVALUATE(loParentForm.lcOprDet+'.nLotQty5')) ,;
nQty6   WITH nQty6 - (IIF(EVALUATE(loParentForm.lcOprDet+'.TranCd')='1',1,-1)*;
EVALUATE(loParentForm.lcOprDet+'.nLotQty6')) ,;
nQty7   WITH nQty7 - (IIF(EVALUATE(loParentForm.lcOprDet+'.TranCd')='1',1,-1)*;
EVALUATE(loParentForm.lcOprDet+'.nLotQty7')) ,;
nQty8   WITH nQty8 - (IIF(EVALUATE(loParentForm.lcOprDet+'.TranCd')='1',1,-1)*;
EVALUATE(loParentForm.lcOprDet+'.nLotQty8')) ,;
nTotQty WITH nTotQty - (IIF(EVALUATE(loParentForm.lcOprDet+'.TranCd')='1',1,-1)*;
EVALUATE(loParentForm.lcOprDet+'.nLotTotQty'))
ENDSCAN
ENDIF
ENDSCAN
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)

lcBrFields = "Item:H='"+loParentForm.lcItmHdr+"':19,cDyelot:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYELOT,loFormSet.GetHeaderText("LANG_MFCSSH_DYELOT",loFormSet.HeaderAlias))+"':10,"+;
"nQty1:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_QUNTITY,loFormSet.GetHeaderText("LANG_MFCSSH_QUNTITY",loFormSet.HeaderAlias))+"1':6,nQty2:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_QUNTITY,loFormSet.GetHeaderText("LANG_MFCSSH_QUNTITY",loFormSet.HeaderAlias))+"2':6,"+;
"nQty3:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_QUNTITY,loFormSet.GetHeaderText("LANG_MFCSSH_QUNTITY",loFormSet.HeaderAlias))+"3':6,nQty4:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_QUNTITY,loFormSet.GetHeaderText("LANG_MFCSSH_QUNTITY",loFormSet.HeaderAlias))+"4':6,"+;
"nQty5:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_QUNTITY,loFormSet.GetHeaderText("LANG_MFCSSH_QUNTITY",loFormSet.HeaderAlias))+"5':6,nQty6:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_QUNTITY,loFormSet.GetHeaderText("LANG_MFCSSH_QUNTITY",loFormSet.HeaderAlias))+"6':6,"+;
"nQty7:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_QUNTITY,loFormSet.GetHeaderText("LANG_MFCSSH_QUNTITY",loFormSet.HeaderAlias))+"7':6,nQty8:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_QUNTITY,loFormSet.GetHeaderText("LANG_MFCSSH_QUNTITY",loFormSet.HeaderAlias))+"8':6,"+;
"nTotQty:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_TOTAL,loFormSet.GetHeaderText("LANG_MFCSSH_TOTAL",loFormSet.HeaderAlias))+" "+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_QUNTITY,loFormSet.GetHeaderText("LANG_MFCSSH_QUNTITY",loFormSet.HeaderAlias))+"':7"

IF _TALLY > 0
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF ARIABROW('',LANG_MFCSSH_ADDNEWITM,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','ITEM','laBrowArr')
IF ARIABROW('',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ADDNEWITM,loFormSet.GetHeaderText("LANG_MFCSSH_ADDNEWITM",loFormSet.HeaderAlias)),;
gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','ITEM','laBrowArr')
*N000682,1 11/20/2012 MMT Globlization changes[End]

SELECT (loParentForm.lcLotsDet)
APPEND BLANK
REPLACE Item      WITH EVALUATE(loParentForm.lcisslogfile+'.Item')    ,;
cInvType  WITH EVALUATE(loParentForm.lcisslogfile+'.cInvType'),;
nToIssue1 WITH EVALUATE(loParentForm.lcisslogfile+'.nQty1')   ,;
nToIssue2 WITH EVALUATE(loParentForm.lcisslogfile+'.nQty2')   ,;
nToIssue3 WITH EVALUATE(loParentForm.lcisslogfile+'.nQty3')   ,;
nToIssue4 WITH EVALUATE(loParentForm.lcisslogfile+'.nQty4')   ,;
nToIssue5 WITH EVALUATE(loParentForm.lcisslogfile+'.nQty5')   ,;
nToIssue6 WITH EVALUATE(loParentForm.lcisslogfile+'.nQty6')   ,;
nToIssue7 WITH EVALUATE(loParentForm.lcisslogfile+'.nQty7')   ,;
nToIssue8 WITH EVALUATE(loParentForm.lcisslogfile+'.nQty8')   ,;
nTotToIss WITH EVALUATE(loParentForm.lcisslogfile+'.nTotQty') ,;
DueDate   WITH ldDueDate                                      ,;
cDyelot   WITH EVALUATE(loParentForm.lcisslogfile+'.cDyelot')
ENDIF
ENDIF
USE IN (loParentForm.lcisslogfile)
SELECT (loParentForm.lcLotsDet)
=lfShModLot(loFormSet)

*!*************************************************************
*! Name      : lfvRemItm
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/09/2004
*! Purpose   : Remove Style/ Fabric-Color from a lot
*!*************************************************************
*! Calls     : gfModalGen,lfShModLot
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvRemItm()
*!*************************************************************
FUNCTION lfvRemItm
LPARAMETERS loFormSet

IF lnTotRcv+lnTotCan+lnTotDmg > 0
*E300725,1 Message : 38061
*E300725,1 Cannot delete this lot since the total recived, damaged, and
*E300725,1 canceled is greater than zero.
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38061B00000','ALERT',ALLTRIM(loParentForm.lcItmHdr))
RETURN
ENDIF
*E300725,1 Message : 38036
*E300725,1 Are you sure you want to remove this record
*E300725,1 Button : 38002
*E300725,1 Proceed  Cancel
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen('QRM38036B38002','ALERT',LANG_MFCSSH_REMOVE)=1
IF gfModalGen('QRM38036B38002','ALERT',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_REMOVE,loFormSet.GetHeaderText("LANG_MFCSSH_REMOVE",loFormSet.HeaderAlias)))=1
*N000682,1 11/20/2012 MMT Globlization changes[End]

SELECT (loParentForm.lcLotsDet)
DELETE
GO TOP
=lfShModLot(loFormSet)
ENDIF

*!*************************************************************
*! Name      : lfvRecLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/05/2004
*! Purpose   : Received,Canceled or Damaged operation/lot
*!*************************************************************
*! Calls     : MFRCVLT.SPX,lfRefTotal
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvRecLot()
*!*************************************************************
FUNCTION lfvRecLot
LPARAMETERS loFormSet

PRIVATE laOpr,laLots,puOpr,puLots,lcOprCode,lcLotNo,lcCurrTag,cbRcvAct,lcItemDesc

lcCurrTag = ORDER(loFormSet.lcOprDet)
SET ORDER TO TAG 'LOT' IN (loFormSet.lcOPrDet)
lnOprRecNo = RECNO(loFormSet.lcOprDet)
lcOprCode = EVALUATE(loFormSet.lcOprDet+'.cOprCode')
lcLotNo   = EVALUATE(loFormSet.lcOprDet+'.cLotNO')

LOCAL lnEngineBehavior
lnEngineBehavior = SET("EngineBehavior")
SET ENGINEBEHAVIOR 70
SELECT .F. AS lSelect,cIMTyp,cTktNo,cOprCode,cLotNo,cInvType,Item,cDyelot,;
oariaapplication.systemdate AS dTranDate,cContCode,;
SUM(IIF(TranCd='1',nLotQty1,-nLotQty1)) AS nLotQty1 ,;
SUM(IIF(TranCd='1',nLotQty2,-nLotQty2)) AS nLotQty2 ,;
SUM(IIF(TranCd='1',nLotQty3,-nLotQty3)) AS nLotQty3 ,;
SUM(IIF(TranCd='1',nLotQty4,-nLotQty4)) AS nLotQty4 ,;
SUM(IIF(TranCd='1',nLotQty5,-nLotQty5)) AS nLotQty5 ,;
SUM(IIF(TranCd='1',nLotQty6,-nLotQty6)) AS nLotQty6 ,;
SUM(IIF(TranCd='1',nLotQty7,-nLotQty7)) AS nLotQty7 ,;
SUM(IIF(TranCd='1',nLotQty8,-nLotQty8)) AS nLotQty8 ,;
SUM(IIF(TranCd='1',nLotTotQty,-nLotTotQty)) AS nLotTotQty ,;
00000 AS nReceive1,00000 AS nReceive2,00000 AS nReceive3,;
00000 AS nReceive4,00000 AS nReceive5,00000 AS nReceive6,;
00000 AS nReceive7,00000 AS nReceive8,000000 AS nTotRec;
FROM (loFormSet.lcOprDet) GROUP BY cIMTYP,cTktNo,cOprCode,cLotNo,cInvType,Item,cDyelot ;
INTO DBF (oAriaApplication.WorkDir+loFormSet.lcRcvFile)
INDEX ON cIMTYP+cTktNo+cOprCode+cLotNo+cInvType+Item+cDyelot TAG (loFormSet.lcRcvFile)

LOCAL lcOprHdr
lcOprHdr = loFormSet.lcOprHdr
SELECT DISTINCT gfCodDes(EVALUATE(loFormSet.lcOprHdr+'.cOprCode'),'MfgCode'),EVALUATE(loFormSet.lcOprHdr+'.cOprCode'),;
EVALUATE(loFormSet.lcOprHdr+'.cOperSeq');
FROM (loFormSet.lcOprDet),(loFormSet.lcOprHdr);
WHERE EVALUATE(loFormSet.lcOprDet+'.cOprCode')=EVALUATE(loFormSet.lcOprHdr+'.cOprCode') AND;
EVALUATE(loFormSet.lcOprHdr+'.cOprCode') <> EVALUATE(loFormSet.lcPosHdr+'.CLASTOPR') AND;
EVALUATE(loFormSet.lcOprDet+'.TranCd')='1';
ORDER BY &lcOprHdr..cOperSeq INTO ARRAY laOpr
SET ENGINEBEHAVIOR lnEngineBehavior
IF _TALLY = 0
RETURN
ENDIF

lnopr = ASCAN(laOpr,lcOprCode)
lnopr = IIF(lnopr=0,1,ASUBSCRIPT(laOpr,lnopr,1))
puOpr = IIF(lcOprCode=EVALUATE(loFormSet.lcPosHdr+'.CLASTOPR'),1,lnopr)

SELECT DISTINCT cLotNo FROM (loFormSet.lcOPrDet) ;
WHERE cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot=;
loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+laOpr[puOpr,2] AND TranCd='1'INTO ARRAY laLots
IF _TALLY =0
RETURN
ENDIF

puLots = ASCAN(laLots,lcLotNo)
puLots = MAX(puLots,1)

DECLARE laToOpr[1,3]
SELECT gfCodDes(cOprCode,'MfgCode'),cOprCode,nNxtLotNo FROM (loFormSet.lcOprHdr);
WHERE VAL(cOperSeq) > VAL(laOpr[puOpr,3]) ORDER BY cOperSeq INTO ARRAY laToOpr
IF _TALLY =0
RETURN
ENDIF

puToOpr   = 1
rbLotType = 1
lcToLotNo = laLots[puLots]

IF EVALUATE(loFormSet.lcPosHdr+'.CMULTILOT')='M'
STORE SPACE(2)  TO lcToLotNo,lcToNewLot
STORE .F.       TO llToLotSt,llToNewSt
IF SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+laToOpr[puToOpr,2]+laLots[puLots],loFormSet.lcOprDet)
lcToLotNo = laLots[puLots]
rbLotType = 1
llToLotSt = .T.
ELSE
lcToNewLot = laLots[puLots]
rbLotType  = 2
llToNewSt  = .T.
ENDIF
llRadioSt = SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+laToOpr[puToOpr,2],loFormSet.lcOprDet)
ENDIF

SELECT (loFormSet.lcRcvFile)
SET KEY TO loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+laOpr[puOpr,2]+laLots[puLots]
=SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+laOpr[puOpr,2]+laLots[puLots])
STORE '' TO lcAction,lcItemDesc
cbRcvAct  = .F.
puTrnType = 1
PRIVATE loParentForm
loParentForm = loFormSet
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFRCVLT.SCX")
=gfCallForm('MFRCVLT')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
=lfRefTotal(loFormSet)
SELECT (loFormSet.lcOprDet)
SET ORDER TO TAG lcCurrTag
GO lnOprRecNo

*!*************************************************************
*! Name      : lfShRcv
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/07/2004
*! Purpose   : Show operations/lots lines to receive/cancel/damage
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfShRcv()
*!*************************************************************
FUNCTION lfShRcv
LPARAMETERS loFormSet

LOCAL lcItemFile,lnAlias
lnAlias = SELECT(0)
IF loParentForm.lcTranType='T'
lcItemFile = 'Fabric'
m.cInvType = EVALUATE(loParentForm.lcRcvFile+'.cInvType')
m.Style    = EVALUATE(loParentForm.lcRcvFile+'.Item')
=lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loParentForm)
ELSE
lcItemFile = 'Style'
=SEEK(EVALUATE(loParentForm.lcRcvFile+'.Item'),'Style')
ENDIF
SELECT (lnAlias)
=SEEK('S'+EVALUATE(lcItemFile+'.Scale'),'Scale')
lcItemDesc = EVALUATE(lcItemFile+'.Desc1')

WITH loFormSet.AriaForm1
.cntOpen.Scale     = SCALE.SCALE
IF !EVALUATE(loParentForm.lcRcvFile+'.lSelect')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdSelect.Caption   = LANG_MFCSSH_SELECT
.cmdSelect.Caption   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_SELECT,loFormSet.GetHeaderText("LANG_MFCSSH_SELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

.cmdSelect.Enabled   = !(puTrnType=1)
.cntQuantity.Enabled = .F.
ELSE
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdSelect.Caption   = LANG_MFCSSH_UNSELECT
.cmdSelect.Caption   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_UNSELECT,loFormSet.GetHeaderText("LANG_MFCSSH_UNSELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

.cntQuantity.Enabled = .T.
ENDIF
.cntQuantity.Scale = SCALE.SCALE
ENDWITH
*B607879,1 WAM 12/14/2006 Refresh receive lot screen to reflect open quantities
WITH loFormSet.AriaForm1.cntOpen
.txtTotQty.Value = 0
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
.txtQty&lcI..Value = MAX(EVALUATE(loParentForm.lcRcvFile+'.nLotQty'+lcI),0)
.txtTotQty.Value   = .txtTotQty.Value + .txtQty&lcI..Value
ENDFOR
ENDWITH
*B607879,1 WAM 12/14/2006 (End)
loFormSet.Refresh

*!*************************************************************
*! Name      : lfvTrnType
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/07/2004
*! Purpose   : type of transaction to be done to operations/lots lines
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvTrnType()
*!*************************************************************
FUNCTION lfvTrnType
LPARAMETERS loFormSet

IF puTrnType = 1
RETURN
ENDIF

WITH loFormSet.AriaForm1
.cboTrnType.Enabled   = .F.
.cboOperation.Enabled = .T.
IF .cboLot.Visible
.cboLot.Enabled     = .T.
ENDIF
IF puTrnType = 2
.cboReceive.Enabled = .T.
IF EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M'
.opgLots.Enabled  = llRadioSt
.txtToLotNo.Enabled = llToLotSt
.txtToNewLot.Enabled = llToNewSt
ENDIF
ENDIF
.cmdSelect.Enabled   = .T.
.cmdSelAll.Enabled   = .T.
.cmdInvert.Enabled   = .T.
.cmdOk.Enabled       = .T.
.lblQuantity.Caption = IIF(puTrnType=2,;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RECEIVE,loFormSet.GetHeaderText("LANG_MFCSSH_RECEIVE",loFormSet.HeaderAlias)),;
IIF(puTrnType=3,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DAMAGED,loFormSet.GetHeaderText("LANG_MFCSSH_DAMAGED",loFormSet.HeaderAlias)),;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CANCELED,loFormSet.GetHeaderText("LANG_MFCSSH_CANCELED",loFormSet.HeaderAlias))))

ENDWITH
lcAction = STR(puTrnType,1)

*!*************************************************************
*! Name      : lfvOprPop
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/07/2004
*! Purpose   : Validate received/Canceled/Damaged operation popup
*!*************************************************************
*! Calls     : lfAdjRButt
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvOprPop()
*!*************************************************************
FUNCTION lfvOprPop
LPARAMETERS loFormSet

LOCAL lcCurrTag
IF puOpr <> loFormSet.AriaForm1.cboOperation.OldValue
lcCurrTag = ORDER(loParentForm.lcOprDet)
SET ORDER TO TAG 'LOT' IN (loParentForm.lcOprDet)
SELECT DISTINCT cLotNo FROM (loParentForm.lcOPrDet) ;
WHERE cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd= loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+laOpr[puOpr,2] ;
INTO ARRAY laLots
puLots = 1
IF .cboLot.Visible
.cboLot.ReQuery
ENDIF

DECLARE laToOpr[1,3]
SELECT gfCodDes(cOprCode,'MfgCode'),cOprCode,nNxtLotNo FROM (loParentForm.lcOprHdr);
WHERE cOperSeq > laOpr[puOpr,3] ORDER BY cOperSeq INTO ARRAY laToOpr
puToOpr = 1
.cboReceive.ReQuery
SELECT (loParentForm.lcRcvFile)
REPLACE ALL lSelect   WITH .F. ,;
nReceive1 WITH 0 ,;
nReceive2 WITH 0 ,;
nReceive3 WITH 0 ,;
nReceive4 WITH 0 ,;
nReceive5 WITH 0 ,;
nReceive6 WITH 0 ,;
nReceive7 WITH 0 ,;
nReceive8 WITH 0 ,;
nTotRec   WITH 0
SET KEY TO loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+laOpr[puOpr,2]+laLots[puLots]
=SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+laOpr[puOpr,2]+laLots[puLots])
=lfAdjRButt('')
=IIF(EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M',lfvToOprPop(loFormSet),.T.)
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)
ENDIF

*!*************************************************************
*! Name      : lfvLotPop
*! Developer : AHMED MAHER
*! Date      : 09/06/2004
*! Purpose   : Validate received/Canceled/Damaged Lot popup
*!*************************************************************
*! Calls     : lfAdjRButt
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLotPop()
*!*************************************************************
FUNCTION lfvLotPop
LPARAMETERS loFormSet

IF puLots<>loFormSet.AriaForm1.cboLot.OldValue
SELECT (loParentForm.lcRcvFile)
REPLACE ALL lSelect   WITH .F. ,;
nReceive1 WITH 0 ,;
nReceive2 WITH 0 ,;
nReceive3 WITH 0 ,;
nReceive4 WITH 0 ,;
nReceive5 WITH 0 ,;
nReceive6 WITH 0 ,;
nReceive7 WITH 0 ,;
nReceive8 WITH 0 ,;
nTotRec   WITH 0
SET KEY TO loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+laOpr[puOpr,2]+laLots[puLots]
=SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+laOpr[puOpr,2]+laLots[puLots])
=lfAdjRButt('',loFormSet)
=lfvToOprPop(loFormSet)
ENDIF

*!*************************************************************
*! Name      : lfvToOprPop
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/07/2004
*! Purpose   : Validate received to operation popup
*!*************************************************************
*! Calls     : lfvLotRadio
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvToOprPop()
*!*************************************************************
FUNCTION lfvToOprPop
LPARAMETERS loFormSet

LOCAL lcCurrTag
IF EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M' .AND. puToOpr<>loFormSet.AriaForm1.cboReceive.OldValue
lcCurrTag = ORDER(loParentForm.lcOprDet)
SET ORDER TO TAG 'LOT' IN (loParentForm.lcOprDet)
STORE SPACE(2)  TO lcToLotNo,lcToNewLot
IF SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+laToOpr[puToOpr,2]+laLots[puLots],loParentForm.lcOprDet)
lcToLotNo = laLots[puLots]
rbLotType = 1
ELSE
lcToNewLot = laLots[puLots]
rbLotType  = 2
ENDIF
IF SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+laToOpr[puToOpr,2],loParentForm.lcOprDet)
loFormSet.AriaForm1.opgLots.Enabled = .T.
ELSE
loFormSet.AriaForm1.opgLots.Enabled = .F.
ENDIF
=lfvLotRadio(loFormSet)
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)
ENDIF

*!*************************************************************
*! Name      : lfvLotRadio
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/06/2004
*! Purpose   : Validate received to lot radio button
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLotRadio()
*!*************************************************************
FUNCTION lfvLotRadio
LPARAMETERS loFormSet

IF rbLotType = 1
lcToLotNo  = laLots[puLots]
lcToNewLot = SPACE(2)
loFormSet.AriaForm1.txtToLotNo.Enabled  = .T.
loFormSet.AriaForm1.txtToNewLot.Enabled = .F.
loFormSet.AriaForm1.txtToLotNo.SetFocus
ELSE
lcToLotNo  = SPACE(2)
lcToNewLot = PADL(laToOpr[1,3],2,'0')
loFormSet.AriaForm1.txtToLotNo.Enabled  = .F.
loFormSet.AriaForm1.txtToNewLot.Enabled = .T.
loFormSet.AriaForm1.txtToLotNo.SetFocus
ENDIF
loFormSet.Refresh

*!*************************************************************
*! Name      : lfAdjRButt
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/07/2004
*! Purpose   : Select, Unselect, Select all, Select None and Invert items
*!             in the receive/cancel/damaged operation screen
*!*************************************************************
*! Calls     : lfShRcv
*!*************************************************************
*! Parameters: lcType  'S' : Select
*!                     'A' : Select All
*!                     'N' : Select None
*!                     'V' : Invert
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfAdjRButt()
*!*************************************************************
FUNCTION lfAdjRButt
LPARAMETERS lcType,loFormSet

LOCAL lnRecNo,llDataSel,llDataUSel

SELECT (loParentForm.lcRcvFile)
lnRecNo = RECNO()
DO CASE
CASE lcType = 'S' AND (lSelect .OR. lfvCheckOrp(laToOpr[puToOpr,2],cInvType,Item))
REPLACE lSelect   WITH !lSelect ,;
nReceive1 WITH IIF(!lSelect,0,MAX(nLotQty1,0)) ,;
nReceive2 WITH IIF(!lSelect,0,MAX(nLotQty2,0)) ,;
nReceive3 WITH IIF(!lSelect,0,MAX(nLotQty3,0)) ,;
nReceive4 WITH IIF(!lSelect,0,MAX(nLotQty4,0)) ,;
nReceive5 WITH IIF(!lSelect,0,MAX(nLotQty5,0)) ,;
nReceive6 WITH IIF(!lSelect,0,MAX(nLotQty6,0)) ,;
nReceive7 WITH IIF(!lSelect,0,MAX(nLotQty7,0)) ,;
nReceive8 WITH IIF(!lSelect,0,MAX(nLotQty8,0)) ,;
nTotRec   WITH IIF(!lSelect,0,nReceive1+nReceive2+nReceive3+nReceive4+;
nReceive5+nReceive6+nReceive7+nReceive8)
CASE lcType = 'A'
SCAN FOR lfvCheckOrp(laToOpr[puToOpr,2],cInvType,Item)
REPLACE lSelect   WITH .T. ,;
nReceive1 WITH MAX(nLotQty1,0) ,;
nReceive2 WITH MAX(nLotQty2,0) ,;
nReceive3 WITH MAX(nLotQty3,0) ,;
nReceive4 WITH MAX(nLotQty4,0) ,;
nReceive5 WITH MAX(nLotQty5,0) ,;
nReceive6 WITH MAX(nLotQty6,0) ,;
nReceive7 WITH MAX(nLotQty7,0) ,;
nReceive8 WITH MAX(nLotQty8,0) ,;
nTotRec   WITH nReceive1+nReceive2+nReceive3+nReceive4+nReceive5+nReceive6+nReceive7+nReceive8
ENDSCAN
CASE lcType = 'N'
REPLACE ALL lSelect   WITH .F. ,;
nReceive1 WITH 0 ,;
nReceive2 WITH 0 ,;
nReceive3 WITH 0 ,;
nReceive4 WITH 0 ,;
nReceive5 WITH 0 ,;
nReceive6 WITH 0 ,;
nReceive7 WITH 0 ,;
nReceive8 WITH 0 ,;
nTotRec   WITH 0
CASE lcType = 'V'
SCAN FOR IIF(!lSelect,lfvCheckOrp(laToOpr[puToOpr,2],cInvType,Item),.T.)
REPLACE lSelect   WITH !lSelect ,;
nReceive1 WITH IIF(!lSelect,0,MAX(nLotQty1,0)) ,;
nReceive2 WITH IIF(!lSelect,0,MAX(nLotQty2,0)) ,;
nReceive3 WITH IIF(!lSelect,0,MAX(nLotQty3,0)) ,;
nReceive4 WITH IIF(!lSelect,0,MAX(nLotQty4,0)) ,;
nReceive5 WITH IIF(!lSelect,0,MAX(nLotQty5,0)) ,;
nReceive6 WITH IIF(!lSelect,0,MAX(nLotQty6,0)) ,;
nReceive7 WITH IIF(!lSelect,0,MAX(nLotQty7,0)) ,;
nReceive8 WITH IIF(!lSelect,0,MAX(nLotQty8,0)) ,;
nTotRec   WITH IIF(!lSelect,0,nReceive1+nReceive2+nReceive3+nReceive4+;
nReceive5+nReceive6+nReceive7+nReceive8)
ENDSCAN
OTHERWISE
ENDCASE
LOCATE FOR !lSelect
llDataUSel = FOUND()
LOCATE FOR lSelect
IF lcAction = '2'
IF laOpr[puOpr,2] = loParentForm.lcFirstOpr .AND. FOUND()
cbRcvAct = .T.
loFormSet.AriaForm1.chkActualize.Enabled = .T.
ELSE
cbRcvAct = .F.
loFormSet.AriaForm1.chkActualize.Enabled = .F.
ENDIF
ENDIF
llDataSel = FOUND()
IF BETWEEN(lnRecNo,1,RECCOUNT())
GO lnRecNo
ENDIF
loFormSet.AriaForm1.cmdSelAll.Enabled  = llDataUSel
loFormSet.AriaForm1.cmdSelNone.Enabled = llDataSel
=lfShRcv(loFormSet)

*!*************************************************************
*! Name      : lfvLotTran
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/07/2004
*! Purpose   : Accept Lot receive, Cancel or Damaged
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcAction  : '2' Receive
*!                         '3' Damage
*!                         '4' Cancel
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLotTran()
*!*************************************************************
FUNCTION lfvLotTran
PARAMETERS lcAction
SELECT (loParentForm.lcRcvFile)
SCAN FOR lSelect .AND. nTotRec > 0
SELECT (loParentForm.lcMFGOPRHD)
LOCATE FOR cOprCode = EVALUATE(loParentForm.lcRcvFile+'.cOprCode')
SELECT (loParentForm.lcRcvFile)
llAcomulat = .F.
lcKeyToSek = cImTyp+cTktNo+cOprCode+cLotNo+cInvType+Item+'1'+cDyelot
lcRecToSek = cImTyp+cTktNo+cOprCode+cLotNo+cInvType+Item+'2'+cDyelot
SELECT (loParentForm.lcOprDet)
IF !SEEK(lcKeyToSek) .OR. lcAction <> '2'
APPEND BLANK
ELSE
llAcomulat = SEEK(lcRecToSek)
IF !llAcomulat
APPEND BLANK
ENDIF
ENDIF
IF !llAcomulat
REPLACE cIMTYp      WITH loParentForm.lcTranType                      ,;
cTktNo      WITH EVALUATE(loParentForm.lcPosHdr+'.PO')        ,;
cOprCode    WITH EVALUATE(loParentForm.lcRcvFile+'.cOprCode') ,;
cLotNo      WITH EVALUATE(loParentForm.lcRcvFile+'.cLotNo')   ,;
Item        WITH EVALUATE(loParentForm.lcRcvFile+'.Item')     ,;
cInvType    WITH EVALUATE(loParentForm.lcRcvFile+'.cInvType') ,;
dTranDate   WITH EVALUATE(loParentForm.lcRcvFile+'.dTranDate'),;
cContCode   WITH EVALUATE(loParentForm.lcRcvFile+'.cContCode'),;
TranCd      WITH lcAction                                     ,;
nLotQty1    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive1'),;
nLotQty2    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive2'),;
nLotQty3    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive3'),;
nLotQty4    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive4'),;
nLotQty5    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive5'),;
nLotQty6    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive6'),;
nLotQty7    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive7'),;
nLotQty8    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive8'),;
nLotTotQty  WITH EVALUATE(loParentForm.lcRcvFile+'.nTotRec')  ,;
cTrgOpr     WITH laToOpr[puToOpr,2]                           ,;
cTrgLot     WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot)        ,;
cOperSeq    WITH EVALUATE(loParentForm.lcMFGOPRHD+'.cOperSeq'),;
cDyelot     WITH EVALUATE(loParentForm.lcRcvFile+'.cDyelot')
ELSE
REPLACE cIMTYp      WITH loParentForm.lcTranType                               ,;
cTktNo      WITH EVALUATE(loParentForm.lcPosHdr+'.PO')                 ,;
cOprCode    WITH EVALUATE(loParentForm.lcRcvFile+'.cOprCode')          ,;
cLotNo      WITH EVALUATE(loParentForm.lcRcvFile+'.cLotNo')            ,;
Item        WITH EVALUATE(loParentForm.lcRcvFile+'.Item')              ,;
cInvType    WITH EVALUATE(loParentForm.lcRcvFile+'.cInvType')          ,;
dTranDate   WITH EVALUATE(loParentForm.lcRcvFile+'.dTranDate')         ,;
cContCode   WITH EVALUATE(loParentForm.lcRcvFile+'.cContCode')         ,;
TranCd      WITH lcAction                                              ,;
nLotQty1    WITH nLotQty1+EVALUATE(loParentForm.lcRcvFile+'.nReceive1'),;
nLotQty2    WITH nLotQty2+EVALUATE(loParentForm.lcRcvFile+'.nReceive2'),;
nLotQty3    WITH nLotQty3+EVALUATE(loParentForm.lcRcvFile+'.nReceive3'),;
nLotQty4    WITH nLotQty4+EVALUATE(loParentForm.lcRcvFile+'.nReceive4'),;
nLotQty5    WITH nLotQty5+EVALUATE(loParentForm.lcRcvFile+'.nReceive5'),;
nLotQty6    WITH nLotQty6+EVALUATE(loParentForm.lcRcvFile+'.nReceive6'),;
nLotQty7    WITH nLotQty7+EVALUATE(loParentForm.lcRcvFile+'.nReceive7'),;
nLotQty8    WITH nLotQty8+EVALUATE(loParentForm.lcRcvFile+'.nReceive8'),;
nLotTotQty  WITH nLotTotQty+EVALUATE(loParentForm.lcRcvFile+'.nTotRec'),;
cTrgOpr     WITH laToOpr[puToOpr,2]                                    ,;
cTrgLot     WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot)                 ,;
cOperSeq    WITH EVALUATE(loParentForm.lcMFGOPRHD+'.cOperSeq')         ,;
cDyelot     WITH EVALUATE(loParentForm.lcRcvFile+'.cDyelot')
ENDIF
IF loParentForm.ActiveMode = 'V'
SELECT (loParentForm.lcMFGOPRDT)
llAcomulate = .F.
LOCATE FOR coprcode+clotno+trancd+cInvType+Item+cDyelot = EVALUATE(loParentForm.lcRcvFile+'.cOprCode')+;
EVALUATE(loParentForm.lcRcvFile+'.CLotNo')+'1'+EVALUATE(loParentForm.lcRcvFile+'.cInvType')+;
EVALUATE(loParentForm.lcRcvFile+'.Item')+EVALUATE(loParentForm.lcRcvFile+'.cDyelot')
IF !FOUND() .OR. lcAction <> '2'
APPEND BLANK
ELSE
LOCATE FOR coprcode+clotno+trancd+cInvType+Item+cDyelot = EVALUATE(loParentForm.lcRcvFile+'.cOprCode')+;
EVALUATE(loParentForm.lcRcvFile+'.CLotNo')+'2'+EVALUATE(loParentForm.lcRcvFile+'.cInvType')+;
EVALUATE(loParentForm.lcRcvFile+'.Item')+EVALUATE(loParentForm.lcRcvFile+'.cDyelot')
llAcomulate = FOUND()
IF !llAcomulat
APPEND BLANK
ENDIF
ENDIF
IF !llAcomulate
REPLACE cIMTYp      WITH loParentForm.lcTranType                      ,;
cTktNo      WITH EVALUATE(loParentForm.lcPosHdr+'.PO')        ,;
cOprCode    WITH EVALUATE(loParentForm.lcRcvFile+'.cOprCode') ,;
cLotNo      WITH EVALUATE(loParentForm.lcRcvFile+'.cLotNo')   ,;
Item        WITH EVALUATE(loParentForm.lcRcvFile+'.Item')     ,;
cInvType    WITH EVALUATE(loParentForm.lcRcvFile+'.cInvType') ,;
dTranDate   WITH EVALUATE(loParentForm.lcRcvFile+'.dTranDate'),;
cContCode   WITH EVALUATE(loParentForm.lcRcvFile+'.cContCode'),;
TranCd      WITH lcAction                                     ,;
nLotQty1    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive1'),;
nLotQty2    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive2'),;
nLotQty3    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive3'),;
nLotQty4    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive4'),;
nLotQty5    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive5'),;
nLotQty6    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive6'),;
nLotQty7    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive7'),;
nLotQty8    WITH EVALUATE(loParentForm.lcRcvFile+'.nReceive8'),;
nLotTotQty  WITH EVALUATE(loParentForm.lcRcvFile+'.nTotRec')  ,;
cTrgOpr     WITH laToOpr[puToOpr,2]                           ,;
cTrgLot     WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot)        ,;
cDyelot     WITH EVALUATE(loParentForm.lcRcvFile+'.cDyelot')
ELSE
REPLACE cIMTYp      WITH loParentForm.lcTranType                               ,;
cTktNo      WITH EVALUATE(loParentForm.lcPosHdr+'.PO')                 ,;
cOprCode    WITH EVALUATE(loParentForm.lcRcvFile+'.cOprCode')          ,;
cLotNo      WITH EVALUATE(loParentForm.lcRcvFile+'.cLotNo')            ,;
Item        WITH EVALUATE(loParentForm.lcRcvFile+'.Item')              ,;
cInvType    WITH EVALUATE(loParentForm.lcRcvFile+'.cInvType')          ,;
dTranDate   WITH EVALUATE(loParentForm.lcRcvFile+'.dTranDate')         ,;
cContCode   WITH EVALUATE(loParentForm.lcRcvFile+'.cContCode')         ,;
TranCd      WITH lcAction                                              ,;
nLotQty1    WITH nLotQty1+EVALUATE(loParentForm.lcRcvFile+'.nReceive1'),;
nLotQty2    WITH nLotQty2+EVALUATE(loParentForm.lcRcvFile+'.nReceive2'),;
nLotQty3    WITH nLotQty3+EVALUATE(loParentForm.lcRcvFile+'.nReceive3'),;
nLotQty4    WITH nLotQty4+EVALUATE(loParentForm.lcRcvFile+'.nReceive4'),;
nLotQty5    WITH nLotQty5+EVALUATE(loParentForm.lcRcvFile+'.nReceive5'),;
nLotQty6    WITH nLotQty6+EVALUATE(loParentForm.lcRcvFile+'.nReceive6'),;
nLotQty7    WITH nLotQty7+EVALUATE(loParentForm.lcRcvFile+'.nReceive7'),;
nLotQty8    WITH nLotQty8+EVALUATE(loParentForm.lcRcvFile+'.nReceive8'),;
nLotTotQty  WITH nLotTotQty+EVALUATE(loParentForm.lcRcvFile+'.nTotRec'),;
cTrgOpr     WITH laToOpr[puToOpr,2]                                    ,;
cTrgLot     WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot)                 ,;
cDyelot     WITH EVALUATE(loParentForm.lcRcvFile+'.cDyelot')
ENDIF
ENDIF
IF lcAction = '2'
SELECT (loParentForm.lcOprHdr)
PRIVATE lnLeadTime
lnLeadTime = loParentForm.lnLeadTime
LOCAL ARRAY laMfgRFld[7,2]
=ACOPY(loParentForm.laMfgRFld,laMfgRFld)
=gfRltFld(laToOpr[puToOpr,2],@laMfgRFld,'MFGCODE')
LOCATE FOR cOprCode = laToOpr[puToOpr,2]
IF FOUND()
REPLACE nNxtLotNo WITH MAX(nNxtLotNo,VAL(IIF(rbLotType=1,lcToLotNo,lcToNewLot))+1)
IF loParentForm.ActiveMode='V'
SELECT (loParentForm.lcMFGOPRHD)
LOCATE FOR cOprCode = laToOpr[puToOpr,2]
IF FOUND()
REPLACE nNxtLotNo WITH MAX(nNxtLotNo,VAL(IIF(rbLotType=1,lcToLotNo,lcToNewLot))+1)
ENDIF
ENDIF
SELECT (loParentForm.lcOprDet)
IF !SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+laToOpr[puToOpr,2]+;
IIF(rbLotType=1,lcToLotNo,lcToNewLot)+EVALUATE(loParentForm.lcRcvFile+'.cInvType')+;
EVALUATE(loParentForm.lcRcvFile+'.Item')+'1'+EVALUATE(loParentForm.lcRcvFile+'.cDyelot'))
APPEND BLANK
REPLACE cIMTYp      WITH loParentForm.lcTranType                     ,;
cTktNo      WITH EVALUATE(loParentForm.lcPosHdr+'.PO')       ,;
cOprCode    WITH laToOpr[puToOpr,2]                          ,;
cLotNo      WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot)       ,;
Item        WITH EVALUATE(loParentForm.lcRcvFile+'.Item')    ,;
cInvType    WITH EVALUATE(loParentForm.lcRcvFile+'.cInvType'),;
TranCd      WITH '1'                                         ,;
cContCode   WITH EVALUATE(loParentForm.lcOprHdr+'.cContCode'),;
lInHouse    WITH EVALUATE(loParentForm.lcOprHdr+'.lInHouse') ,;
cOperSeq    WITH EVALUATE(loParentForm.lcOprHdr+'.cOperSeq') ,;
cDyelot     WITH EVALUATE(loParentForm.lcRcvFile+'.cDyelot')
ENDIF
REPLACE dTranDate   WITH EVALUATE(loParentForm.lcRcvFile+'.dTranDate') ,;
DueDate     WITH dTranDate+lnLeadTime  ,;
nLotQty1    WITH nLotQty1+EVALUATE(loParentForm.lcRcvFile+'.nReceive1') ,;
nLotQty2    WITH nLotQty2+EVALUATE(loParentForm.lcRcvFile+'.nReceive2') ,;
nLotQty3    WITH nLotQty3+EVALUATE(loParentForm.lcRcvFile+'.nReceive3') ,;
nLotQty4    WITH nLotQty4+EVALUATE(loParentForm.lcRcvFile+'.nReceive4') ,;
nLotQty5    WITH nLotQty5+EVALUATE(loParentForm.lcRcvFile+'.nReceive5') ,;
nLotQty6    WITH nLotQty6+EVALUATE(loParentForm.lcRcvFile+'.nReceive6') ,;
nLotQty7    WITH nLotQty7+EVALUATE(loParentForm.lcRcvFile+'.nReceive7') ,;
nLotQty8    WITH nLotQty8+EVALUATE(loParentForm.lcRcvFile+'.nReceive8') ,;
nLotTotQty  WITH nLotTotQty+EVALUATE(loParentForm.lcRcvFile+'.nTotRec')
IF loParentForm.ActiveMode = 'V'
SELECT (loParentForm.lcMFGOPRDT)
LOCATE FOR coprcode+clotno+trancd+cInvType+Item+cDyelot = laToOpr[puToOpr,2]+IIF(rbLotType=1,lcToLotNo,lcToNewLot)+;
'1'+EVALUATE(loParentForm.lcRcvFile+'.cInvType')+EVALUATE(loParentForm.lcRcvFile+'.Item')+;
EVALUATE(loParentForm.lcRcvFile+'.cDyelot')
IF !FOUND()
APPEND BLANK
REPLACE cIMTYp      WITH loParentForm.lcTranType                       ,;
cTktNo      WITH EVALUATE(loParentForm.lcPosHdr+'.PO')         ,;
cOprCode    WITH laToOpr[puToOpr,2]                            ,;
cLotNo      WITH IIF(rbLotType=1,lcToLotNo,lcToNewLot)         ,;
Item        WITH EVALUATE(loParentForm.lcRcvFile+'.Item')      ,;
cInvType    WITH EVALUATE(loParentForm.lcRcvFile+'.cInvType')  ,;
TranCd      WITH '1'                                           ,;
cContCode   WITH EVALUATE(loParentForm.lcMFGOPRHD+'.cContCode'),;
lInHouse    WITH EVALUATE(loParentForm.lcMFGOPRHD+'.lInHouse') ,;
cDyelot     WITH EVALUATE(loParentForm.lcRcvFile+'.cDyelot')
ENDIF
REPLACE dTranDate   WITH EVALUATE(loParentForm.lcRcvFile+'.dTranDate') ,;
DueDate     WITH dTranDate+lnLeadTime  ,;
nLotQty1    WITH nLotQty1+EVALUATE(loParentForm.lcRcvFile+'.nReceive1') ,;
nLotQty2    WITH nLotQty2+EVALUATE(loParentForm.lcRcvFile+'.nReceive2') ,;
nLotQty3    WITH nLotQty3+EVALUATE(loParentForm.lcRcvFile+'.nReceive3') ,;
nLotQty4    WITH nLotQty4+EVALUATE(loParentForm.lcRcvFile+'.nReceive4') ,;
nLotQty5    WITH nLotQty5+EVALUATE(loParentForm.lcRcvFile+'.nReceive5') ,;
nLotQty6    WITH nLotQty6+EVALUATE(loParentForm.lcRcvFile+'.nReceive6') ,;
nLotQty7    WITH nLotQty7+EVALUATE(loParentForm.lcRcvFile+'.nReceive7') ,;
nLotQty8    WITH nLotQty8+EVALUATE(loParentForm.lcRcvFile+'.nReceive8') ,;
nLotTotQty  WITH nLotTotQty+EVALUATE(loParentForm.lcRcvFile+'.nTotRec')
ENDIF
ENDIF
ENDIF
*!*    *C200444,1 ALB Add GL entries when recieving Non GL operations [Begin]
*!*    IF ASCAN(laEvntTrig , PADR('UPDTOPGL',10)) <> 0
*!*      =gfDoTriger('POCSSH',PADR('UPDTOPGL',10))
*!*    ENDIF
*!*    *C200444,1 ALB Add GL entries when recieving Non GL operations [end]
ENDSCAN

DECLARE laTableUpdate[2,2]
laTableUpdate[1,1] = loParentForm.lcMfgOprHd
laTableUpdate[1,2] = 'MFGOPRHD'

laTableUpdate[2,1] = loParentForm.lcMfgOprDt
laTableUpdate[2,2] = 'MFGOPRDT'

=lfTableUpdate(loParentForm)

*Commented to privent actualize in this phase.

**B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
DIMENSION laActLot[1]
IF cbRcvAct
*IF .F.
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

lcOprCode = loParentForm.lcFirstOpr
ldActDate = oariaapplication.systemdate
rbActual = 2
IF EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M'
rbActual = 1
DECLARE laActLot[1]
laActLot[1] = laLots[puLots]
puActLot = 1
ENDIF
lcStatColor=''
STORE 0 TO lnLotbud,lnLotRcv,lnLotCan,lnLotDmg
=IIF(EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M',lfGetSummary(laActLot[1]),lfGetSummary(''))

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*DO FORM (oAriaApplication.ScreenHome+"MFACTUAL.SCX") WITH .T.
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFACTUAL.SCX") ;
WITH .T., EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT'),;
lnLotbud, lnLotRcv, lnLotDmg, lnLotCan, rbActual
=gfCallForm('MFACTUAL',.F., ".T., '"+EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')+"',"+;
"lnLotbud, lnLotRcv, lnLotDmg, lnLotCan, rbActual")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]
ENDIF

*!*************************************************************
*! Name      : lfvActualize
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/02/2004
*! Purpose   : Actualize the whole cutting ticket or a specefic lot in the
*!             first operation.
*!*************************************************************
*! Calls     : MFACTUAL.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvActualize()
*!*************************************************************
FUNCTION lfvActualize
PRIVATE ldActDate,puActLot,laActLot,rbActual,lcOprCode,lcCurrTag

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*	lcOprCode = lcFirstOpr
*!*	ldActDate = gdSysDate
lcOprCode = loParentForm.lcFirstOpr
ldActDate = oariaapplication.systemdate
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

rbActual  = 2
DIMENSION laActLot[1]

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*llCalled = .F.
*IF ladata[28]='M'
*lcCurrTag = ORDER(lcOprDet)
*SET ORDER TO TAG 'LOT' IN (lcOprDet)
STORE .F. TO llMultiLot
IF EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M'
lcCurrTag = ORDER(loParentForm.lcOprDet)
SET ORDER TO TAG 'Lot' IN (loParentForm.lcOprDet)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

rbActual  = 1
*B603718,1 SSH  [Start] actualize cuttkt
*B603718,1 SSH          Allow multiple recieving with actualize.
*SELECT DIST cLotNo FROM (lcOPrDet) ;
WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcFirstOpr AND TranCd='1' AND nActQty=0 ;
INTO ARRAY laActLot

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT DIST cLotNo FROM (lcOPrDet) ;
WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcFirstOpr AND TranCd='1';
INTO ARRAY laActLot

SELECT DISTINCT cLotNo FROM (loParentForm.lcOprDet) ;
WHERE cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+loParentForm.lcFirstOpr AND TranCd='1';
INTO ARRAY laActLot
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

*B603718,1 SSH   [End]
IF _TALLY = 0

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*llCalled = .T.
llMultiLot = .T.
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

rbActual = 2
ENDIF
puActLot = 1

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SET ORDER TO TAG lcCurrTag IN (lcOprDet)
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

ENDIF
lcStatColor=''
STORE 0 TO lnLotbud,lnLotRcv,lnLotCan,lnLotDmg

*E302004,1 AMH Ability to Actu. C/T with one oper. [Start]
*=IIF(ladata[28]='M' AND !llCalled,lfGetSummary(laActLot[1]),lfGetSummary(''))
*DO (gcScrDir+"MFACTUAL.SPX") WITH llCalled

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*IF llOnlyOne
IF loParentForm.llOnlyOne
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

=lfAct1Opr()
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*DO (gcScrDir+"MFACTONE.SPX")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFACTONE.SCX") WITH lnLotbud, lnLotRcv
=gfCallForm('MFACTONE',.F.,'lnLotbud, lnLotRcv')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

ELSE

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*=IIF(ladata[28]='M' AND !llCalled,lfGetSummary(laActLot[1]),lfGetSummary(''))
*DO (gcScrDir+"MFACTUAL.SPX") WITH llCalled
=IIF(EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M' AND !llMultiLot,lfGetSummary(laActLot[1]),lfGetSummary(''))
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFACTUAL.ScX") WITH llMultiLot, EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT'),;
lnLotbud, lnLotRcv, lnLotDmg, lnLotCan, rbActual
=gfCallForm('MFACTUAL',.F.,"llMultiLot, '"+EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')+"',"+;
"lnLotbud, lnLotRcv, lnLotDmg, lnLotCan, rbActual")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

ENDIF
*E302004,1 AMH [End]

*B802785,1 AMM Comment out, Move it to other location
*=lfRefresh(lcWinCh0)
*B802785,1 AMM end
*!*************************************************************
*! Name      : lfvOkAct
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/02/2004
*! Purpose   : Actualize the whole cutting ticket or a specefic lot in the
*!             first operation.
*!*************************************************************
*! Calls     : lfBrowLots,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvOkAct()
*!*************************************************************
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*FUNCTION lfvOkAct
FUNCTION OLDlfvOkAct
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

LPARAMETERS loFormSet
*B604056,1 HBG 12/06/2000 Change the name of the variabel lcKey
*B604056,1                To fix bug variabel 'lcKey' not found [Begin]
*PRIVATE lnAlias,lcLotNo,lcItem,lcColor,lnOpen1,lnOpen2,lnOpen3,lnOpen4,;
lnOpen5,lnOpen6,lnOpen7,lnOpen8,lnOpen,lnRecv,llInHouse,;
lcContCode,lcContName,lcCurrTag,lnUpdYield,lcKey
PRIVATE lnAlias,lcLotNo,lcItem,lcColor,lnOpen1,lnOpen2,lnOpen3,lnOpen4,;
lnOpen5,lnOpen6,lnOpen7,lnOpen8,lnOpen,lnRecv,llInHouse,;
lcContCode,lcContName,lcCurrTag,lnUpdYield,lcKeyVal
*B604056,1 [End]
lnAlias  = SELECT()


lcCurrTag= ORDER(lcOprDet)
SET ORDER TO TAG LOT IN (lcOprDet)

*B602482,1 Keep track of allocated orders while actualization
STORE .F. TO llTktAllo,llRelAll
*B602482,1 (End)

*B602482,1 Keep track of allocated orders while actualization

IF lcTranType <> 'T'
SELECT (lcTranFile)
*C200080,1 AMM Consider the dye order case
*=SEEK(IIF(lcTranType='I','P','')+laData[1])
*DO WHILE EVAL(lcFileKey) = IIF(lcTranType='I','P','')+laData[1] AND !llTktAllo
*SUM REST WHILE EVAL(lcFileKey) = IIF(lcTranType='I','P','')+laData[1] ;
Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO ;
lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8,lnQty1,;
lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8

*khm1
*=SEEK(IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
*DO WHILE EVAL(lcFileKey) = IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] AND !llTktAllo
SUM REST WHILE EVAL(lcFileKey) = IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] ;
Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO ;
lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8,lnQty1,;
lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8
LOCATE
=SEEK(IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
DO WHILE EVAL(lcFileKey) = IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] AND !llTktAllo
SUM REST WHILE EVAL(lcFileKey) = IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] ;
Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO ;
lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8,lnQty1,;
lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8
*khm1

*C200080,1 AMM end
SELECT (lcOprDet)

=SEEK(lcTranType+laData[1]+lcFirstOpr+IIF(rbActual=1,laActLot[puActLot],''))
DO WHILE !llTktAllo AND ;
cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcFirstOpr+IIF(rbActual=1,laActLot[puActLot],'')
lcItem  = Item
lcLotNo = cLotNo
STORE 0 TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8
*B603243,1 AMM make the operation done by style/color/dyelot
*SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem FOR nActQty=0
*B603718,1 SSH  [Start] actualize cuttkt
*B603718,1 SSH          Allow multiple recieving with actualize.
*SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem FOR nActQty=0 .AND. cDyelot = &lcOprDet..cDyelot
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem FOR cDyelot = &lcOprDet..cDyelot

*B603718,1 SSH  [End] actualize cuttkt
*B603718,1 SSH          Allow multiple recieving with actualize.
*B603243,1 AMM  end
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
lnOpen&lcCount = lnOpen&lcCount+IIF(TranCd='1',nLotQty&lcCount,-nLotQty&lcCount)
ENDFOR
ENDSCAN
IF (lnOrd1 > lnQty1-lnOpen1 OR lnOrd2 > lnQty2-lnOpen2 OR lnOrd3 > lnQty3-lnOpen3 OR ;
lnOrd4 > lnQty4-lnOpen4 OR lnOrd5 > lnQty5-lnOpen5 OR lnOrd6 > lnQty6-lnOpen6 OR ;
lnOrd7 > lnQty7-lnOpen7 OR lnOrd8 > lnQty8-lnOpen8)
llTktAllo = .T.
ENDIF
SELECT (lcOprDet)
ENDDO
SELECT (lcTranFile)
ENDDO
*B602482,1 Message : 38168
*B602482,1 Cutting ticket# 999999 has pieces allocated from orders.
*B602482,1 Proceed with acualization and modify the allocated
*B602482,1 quantity from order lines to keep track of this
*B602482,1 allocation, or cancel
*B602482,1 Button : 38022
*B602482,1 < Proceed> <Cancel>
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*IF llTktAllo AND gfModalGen('QRM38168B38002','ALERT',;
*   IIF(lcTranType='M','Cutting Ticket#: ','PO#:')+laData[1]) = 2
IF llTktAllo AND gfModalGen('QRM38168B38002','ALERT',;
IIF(lcTranType='M',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))+': ',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))+':')+laData[1]) = 2
*N000682,1 MMT 12/09/2012 Globalization changes[end]
SELECT (lnAlias)
SET ORDER TO TAG lcCurrTag IN (lcOprDet)
RETURN
ENDIF
ENDIF
*B602482,1 Keep track of allocated orders while actualization

lnUpdYield = 0

IF SEEK(lcTranType+laData[1]+lcFirstOpr+IIF(rbActual=1,laActLot[puActLot],''),lcOprDet)
SELECT (lcOprDet)
DO WHILE !EOF() .AND. cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcFirstOpr+IIF(rbActual=1,laActLot[puActLot],'')
lcItem     = Item
lcColor    = Color
llInHouse  = lInHouse
lcContCode = cContCode
lcContName = cContName
lcLotNo    = cLotNo
*B603243,1 AMM Get the dyelot field
lcItDye    = cDyelot
*B603243,1 AMM  end
STORE 0 TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8,lnOpen,lnRecv
STORE 0 TO lnRecv1,lnRecv2,lnRecv3,lnRecv4,lnRecv5,lnRecv6,lnRecv7,lnRecv8

*B603718,1 SSH  [Start] actualize cuttkt
*B603718,1 SSH          Allow multiple recieving with actualize.
*llActualize = (nActQty=0)
llActualize = .T.
*B603718,1 SSH   [End]
*B802956,1 AMM Get next dyelot in the file
lcNxtDye = lcItDye
IF llUseDyelot
SKIP 1
*--B603867,1 RAMY [start]
*IF cDyelot # lcItDye
IF cDyelot # lcItDye .AND. TranCd = '1'
*--B603867,1 RAMY [end]
lcNxtDye = cDyelot
ENDIF
SKIP -1
ENDIF
*B802956,1 AMM end
*B603243,1 AMM make the operation done by style/color/dyelot
*SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor ;
FOR   llActualize
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor ;
FOR   llActualize .AND. cDyelot = lcItDye
*B603243,1 AMM  end
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
lnOpen&lcCount=lnOpen&lcCount+IIF(TranCd='1',nLotQty&lcCount,-nLotQty&lcCount)
lnRecv&lcCount=lnRecv&lcCount+IIF(TranCd='2',nLotQty&lcCount,0)
ENDFOR
*E301235,4 WAB - in case of not M.MFG get sisez
*E301235,4 WAB - START
*IF lcTRanType='T'
IF lcTRanType<>'T'
*E301235,4 WAB - END
lnOpen = MAX(lnOpen1,0)+MAX(lnOpen2,0)+MAX(lnOpen3,0)+MAX(lnOpen4,0)+;
MAX(lnOpen5,0)+MAX(lnOpen6,0)+MAX(lnOpen7,0)+MAX(lnOpen8,0)
ELSE
lnOpen = lnOpen + IIF(TranCd='1',nLotTotQty,-nLotTotQty)
ENDIF
lnRecv = lnRecv  + IIF(TranCd='2',nLotTotQty,0)
ENDSCAN
*B602380,1 continue with actualization if total quantity actualized
*IF !llActualize OR lnOpen=0
IF !llActualize
*B602380,1 (End)
LOOP
ENDIF
*B603243,1 AMM Add the dyelot field
*lcKey=cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd
*B604056,1 HBG 12/06/2000 Fix bug variabel 'lcKey' not found [Begin]
*lcKey=cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot
lcKeyVal=cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot
*B604056,1 [End]
*B603243,1 AMM  end
SELECT MFGOPRDT
=SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+'1')
*B603243,1 AMM make the operation done by style/color/dyelot
*LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
lcTranType+laData[1]+lcFirstOpr+lcLotNo+'1' ;
FOR   Item+Color= lcItem+lcColor
LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
lcTranType+laData[1]+lcFirstOpr+lcLotNo+'1' ;
FOR   Item+Color+cDyelot = lcItem+lcColor+lcItDye
*B603243,1 AMM  end
SELECT (lcTmpTkt)

=SEEK(lcItem+lcColor)
*B603243,1 AMM make the operation done by style/color/dyelot
*COUNT REST WHILE Item+Color = lcItem+lcColor TO lnRecords
COUNT REST WHILE Item+Color = lcItem+lcColor FOR cDyelot = lcItDye TO lnRecords
*B603243,1 AMM  end
IF lnRecords=1
=SEEK(lcItem+lcColor)
*B603243,1 AMM make the operation done by style/color/dyelot
IF llUseDyelot
LOCATE REST WHILE Item+Color = lcItem+lcColor FOR cDyelot = lcItDye
ENDIF
*B603243,1 AMM  end

*B802542,1 - WAB - MAx(lnOpen?) can not allowed here cause in case receiving is grater than
*B802542,1 - WAB - Budget lnopen is negative and we must calculate  [nqty? - (- lnopen?) ]
*B802542,1 - WAB - START
*REPLACE nQty1   WITH nQty1   - MAX(lnOpen1,0) ,;
nQty2   WITH nQty2   - MAX(lnOpen2,0) ,;
nQty3   WITH nQty3   - MAX(lnOpen3,0) ,;
nQty4   WITH nQty4   - MAX(lnOpen4,0) ,;
nQty5   WITH nQty5   - MAX(lnOpen5,0) ,;
nQty6   WITH nQty6   - MAX(lnOpen6,0) ,;
nQty7   WITH nQty7   - MAX(lnOpen7,0) ,;
nQty8   WITH nQty8   - MAX(lnOpen8,0) ,;
nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8

*REPLACE nQty1   WITH nQty1   - lnOpen1 ,;
nQty2   WITH nQty2   - lnOpen2 ,;
nQty3   WITH nQty3   - lnOpen3 ,;
nQty4   WITH nQty4   - lnOpen4 ,;
nQty5   WITH nQty5   - lnOpen5 ,;
nQty6   WITH nQty6   - lnOpen6 ,;
nQty7   WITH nQty7   - lnOpen7 ,;
nQty8   WITH nQty8   - lnOpen8 ,;
nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8

*B605224,1 KHM 12/09/2001 (Begin) Changing the replace command
*REPLACE nQty1   WITH lnRecv1 ,;
nQty2   WITH lnRecv2 ,;
nQty3   WITH lnRecv3 ,;
nQty4   WITH lnRecv4 ,;
nQty5   WITH lnRecv5 ,;
nQty6   WITH lnRecv6 ,;
nQty7   WITH lnRecv7 ,;
nQty8   WITH lnRecv8 ,;
nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8

REPLACE nQty1   WITH nQty1   - lnOpen1 ,;
nQty2   WITH nQty2   - lnOpen2 ,;
nQty3   WITH nQty3   - lnOpen3 ,;
nQty4   WITH nQty4   - lnOpen4 ,;
nQty5   WITH nQty5   - lnOpen5 ,;
nQty6   WITH nQty6   - lnOpen6 ,;
nQty7   WITH nQty7   - lnOpen7 ,;
nQty8   WITH nQty8   - lnOpen8 ,;
nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8
*B605224,1 KHM 12/09/2001 (End)

*B802542,1 - WAB - START

ELSE
*B603243,1 AMM  Add dyelot
*SELECT *,00000 AS nCan1,00000 AS nCan2,00000 AS nCan3,00000 AS nCan4,;
00000 AS nCan5,00000 AS nCan6,00000 AS nCan7,00000 AS nCan8,;
000000 AS nTotCan, RECNO() AS nBudRecNo FROM (lcTmpTkt) ;
WHERE ITEM+COLOR=lcItem+lcColor;
INTO DBF (gcWorkDir+lcisslogfile)
SELECT *,00000 AS nCan1,00000 AS nCan2,00000 AS nCan3,00000 AS nCan4,;
00000 AS nCan5,00000 AS nCan6,00000 AS nCan7,00000 AS nCan8,;
000000 AS nTotCan, RECNO() AS nBudRecNo FROM (lcTmpTkt) ;
WHERE ITEM+COLOR+cDyelot = lcItem+lcColor+lcItDye ;
INTO DBF (gcWorkDir+lcisslogfile)
*B603243,1 AMM  end
STORE MAX(lnOpen1,0) TO lnCan1,lnBal1
STORE MAX(lnOpen2,0) TO lnCan2,lnBal2
STORE MAX(lnOpen3,0) TO lnCan3,lnBal3
STORE MAX(lnOpen4,0) TO lnCan4,lnBal4
STORE MAX(lnOpen5,0) TO lnCan5,lnBal5
STORE MAX(lnOpen6,0) TO lnCan6,lnBal6
STORE MAX(lnOpen7,0) TO lnCan7,lnBal7
STORE MAX(lnOpen8,0) TO lnCan8,lnBal8
STORE lnCan1+lnCan2+lnCan3+lnCan4+lnCan5+lnCan6+lnCan7+lnCan8 TO lnTotCan,lnTotBal
SCATTER MEMVAR
=SEEK(lcItem,'Style') .AND. SEEK('S'+Style.Scale,'Scale')


*B802713,1 Fix syntax error in the cancel quantity contribution screen.
*lcConCnTit = ALLTRIM(lcItem)+ALLTRIM(lcColor)+'Cancelled Quantity'
lcConCnTit = IIF(lcTranType='T','Material/Color',lcItmHdr)+' '+;
ALLTRIM(lcItem)+ALLTRIM(lcColor)+'Cancelled Quantity'
*B802713,1 (End)

DO (gcScrDir+"MFCONCN.SPX")
USE IN (lcisslogfile)
ENDIF
SELECT (lcTmpTkt)
=SEEK(lcItem+lcColor)

*B802713,1 Fix computing operation canceled quantity while actualization
STORE 0 TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8
*B802713,1 (End)
*B802956,1 AMM Add the dyelot
*SCAN REST WHILE Item+Color+STR(LineNo,6) = lcItem+lcColor
SCAN REST WHILE Item+Color+STR(LineNo,6) = lcItem+lcColor  FOR cDyelot = lcItDye
*B802956,1 AMM end
DO CASE
CASE lcTranType='M'
SET ORDER TO TAG CUTLIN IN CUTTKTL
=SEEK(laData[1]+Item+STR(lineno,6)+'1','CUTTKTL')
*B802713,1 Fix computing operation canceled quantity while actualization
*lnOpen1 = CUTTKTL.Qty1-nQty1
*lnOpen2 = CUTTKTL.Qty2-nQty2
*lnOpen3 = CUTTKTL.Qty3-nQty3
*lnOpen4 = CUTTKTL.Qty4-nQty4
*lnOpen5 = CUTTKTL.Qty5-nQty5
*lnOpen6 = CUTTKTL.Qty6-nQty6
*lnOpen7 = CUTTKTL.Qty7-nQty7
*lnOpen8 = CUTTKTL.Qty8-nQty8
lnOpen1 = lnOpen1 + CUTTKTL.Qty1-nQty1
lnOpen2 = lnOpen2 + CUTTKTL.Qty2-nQty2
lnOpen3 = lnOpen3 + CUTTKTL.Qty3-nQty3
lnOpen4 = lnOpen4 + CUTTKTL.Qty4-nQty4
lnOpen5 = lnOpen5 + CUTTKTL.Qty5-nQty5
lnOpen6 = lnOpen6 + CUTTKTL.Qty6-nQty6
lnOpen7 = lnOpen7 + CUTTKTL.Qty7-nQty7
lnOpen8 = lnOpen8 + CUTTKTL.Qty8-nQty8
*B802713,1 (End)
lnOpen  = lnOpen1+lnOpen2+lnOpen3+lnOpen4+lnOpen5+lnOpen6+lnOpen7+lnOpen8

*B602482,1 Keep track of allocated orders while actualization
SELECT CUTTKTL
SCATTER FIELDS ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotOrd TO laOrdQty
SELECT (lcTmpTkt)
IF (CUTTKTL.Ord1 > nQty1 OR CUTTKTL.Ord2 > nQty2 OR CUTTKTL.Ord3 > nQty3 OR ;
CUTTKTL.Ord4 > nQty4 OR CUTTKTL.Ord5 > nQty5 OR CUTTKTL.Ord6 > nQty6 OR ;
CUTTKTL.Ord7 > nQty7 OR CUTTKTL.Ord8 > nQty8)

*E301182,1 Update CT/PO line number in the CUTPICK file.
*=lfTktAllo(lcItem)
=lfTktAllo(lcItem,CutTKtL.Dyelot,CutTKtL.LineNo)
*E301182,1 (End)
ENDIF
SELECT CUTTKTH
=RLOCK()
REPLACE TotOrd WITH TotOrd - CutTktl.TotOrd + laOrdQty[9]
UNLOCK
*B602482,1 (End)

*B602203,1 Update Style WIP
SELECT STYLE
=SEEK(CUTTKTL.Style)
=RLOCK()

*B606340,1 AMH Update wip with positive values only by deducting the max
*B606340,1 between qty and 0 [Start]
*REPLACE WIP1   WITH WIP1 - (CUTTKTL.Qty1-&lcTmpTkt..nQty1) ,;
WIP2   WITH WIP2 - (CUTTKTL.Qty2-&lcTmpTkt..nQty2) ,;
WIP3   WITH WIP3 - (CUTTKTL.Qty3-&lcTmpTkt..nQty3) ,;
WIP4   WITH WIP4 - (CUTTKTL.Qty4-&lcTmpTkt..nQty4) ,;
WIP5   WITH WIP5 - (CUTTKTL.Qty5-&lcTmpTkt..nQty5) ,;
WIP6   WITH WIP6 - (CUTTKTL.Qty6-&lcTmpTkt..nQty6) ,;
WIP7   WITH WIP7 - (CUTTKTL.Qty7-&lcTmpTkt..nQty7) ,;
WIP8   WITH WIP8 - (CUTTKTL.Qty8-&lcTmpTkt..nQty8) ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
REPLACE WIP1   WITH WIP1 - (CUTTKTL.Qty1-MAX(&lcTmpTkt..nQty1,0)) ,;
WIP2   WITH WIP2 - (CUTTKTL.Qty2-MAX(&lcTmpTkt..nQty2,0)) ,;
WIP3   WITH WIP3 - (CUTTKTL.Qty3-MAX(&lcTmpTkt..nQty3,0)) ,;
WIP4   WITH WIP4 - (CUTTKTL.Qty4-MAX(&lcTmpTkt..nQty4,0)) ,;
WIP5   WITH WIP5 - (CUTTKTL.Qty5-MAX(&lcTmpTkt..nQty5,0)) ,;
WIP6   WITH WIP6 - (CUTTKTL.Qty6-MAX(&lcTmpTkt..nQty6,0)) ,;
WIP7   WITH WIP7 - (CUTTKTL.Qty7-MAX(&lcTmpTkt..nQty7,0)) ,;
WIP8   WITH WIP8 - (CUTTKTL.Qty8-MAX(&lcTmpTkt..nQty8,0)) ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
*B606340,1 AMH [End]

UNLOCK
SELECT STYDYE
=SEEK(CUTTKTL.Style+CutTktl.cWareCode+SPACE(10))
=RLOCK()

*B606340,1 AMH Update wip with positive values only by deducting the max
*B606340,1 between qty and 0 [Start]
*REPLACE WIP1   WITH WIP1 - (CUTTKTL.Qty1-&lcTmpTkt..nQty1) ,;
WIP2   WITH WIP2 - (CUTTKTL.Qty2-&lcTmpTkt..nQty2) ,;
WIP3   WITH WIP3 - (CUTTKTL.Qty3-&lcTmpTkt..nQty3) ,;
WIP4   WITH WIP4 - (CUTTKTL.Qty4-&lcTmpTkt..nQty4) ,;
WIP5   WITH WIP5 - (CUTTKTL.Qty5-&lcTmpTkt..nQty5) ,;
WIP6   WITH WIP6 - (CUTTKTL.Qty6-&lcTmpTkt..nQty6) ,;
WIP7   WITH WIP7 - (CUTTKTL.Qty7-&lcTmpTkt..nQty7) ,;
WIP8   WITH WIP8 - (CUTTKTL.Qty8-&lcTmpTkt..nQty8) ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
REPLACE WIP1   WITH WIP1 - (CUTTKTL.Qty1-MAX(&lcTmpTkt..nQty1,0)) ,;
WIP2   WITH WIP2 - (CUTTKTL.Qty2-MAX(&lcTmpTkt..nQty2,0)) ,;
WIP3   WITH WIP3 - (CUTTKTL.Qty3-MAX(&lcTmpTkt..nQty3,0)) ,;
WIP4   WITH WIP4 - (CUTTKTL.Qty4-MAX(&lcTmpTkt..nQty4,0)) ,;
WIP5   WITH WIP5 - (CUTTKTL.Qty5-MAX(&lcTmpTkt..nQty5,0)) ,;
WIP6   WITH WIP6 - (CUTTKTL.Qty6-MAX(&lcTmpTkt..nQty6,0)) ,;
WIP7   WITH WIP7 - (CUTTKTL.Qty7-MAX(&lcTmpTkt..nQty7,0)) ,;
WIP8   WITH WIP8 - (CUTTKTL.Qty8-MAX(&lcTmpTkt..nQty8,0)) ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
*B606340,1 AMH [End]

UNLOCK
*B602203,1 (End)
SELECT CUTTKTL

lnTotQty = Totqty
=RLOCK()

*B606340,1 AMH Update wip with positive values only by deducting the max
*B606340,1 between qty and 0 [Start]
*REPLACE Qty1   WITH &lcTmpTkt..nQty1 ,;
Qty2   WITH &lcTmpTkt..nQty2 ,;
Qty3   WITH &lcTmpTkt..nQty3 ,;
Qty4   WITH &lcTmpTkt..nQty4 ,;
Qty5   WITH &lcTmpTkt..nQty5 ,;
Qty6   WITH &lcTmpTkt..nQty6 ,;
Qty7   WITH &lcTmpTkt..nQty7 ,;
Qty8   WITH &lcTmpTkt..nQty8 ,;
TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
REPLACE Qty1   WITH MAX(&lcTmpTkt..nQty1,0) ,;
Qty2   WITH MAX(&lcTmpTkt..nQty2,0) ,;
Qty3   WITH MAX(&lcTmpTkt..nQty3,0) ,;
Qty4   WITH MAX(&lcTmpTkt..nQty4,0) ,;
Qty5   WITH MAX(&lcTmpTkt..nQty5,0) ,;
Qty6   WITH MAX(&lcTmpTkt..nQty6,0) ,;
Qty7   WITH MAX(&lcTmpTkt..nQty7,0) ,;
Qty8   WITH MAX(&lcTmpTkt..nQty8,0) ,;
TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
*B606340,1 AMH [End]

*B602482,1 Keep track of allocated orders while actualization
REPLACE Ord1   WITH laOrdQty[1] ,;
Ord2   WITH laOrdQty[2] ,;
Ord3   WITH laOrdQty[3] ,;
Ord4   WITH laOrdQty[4] ,;
Ord5   WITH laOrdQty[5] ,;
Ord6   WITH laOrdQty[6] ,;
Ord7   WITH laOrdQty[7] ,;
Ord8   WITH laOrdQty[8] ,;
TotOrd WITH laOrdQty[9]
*B602482,1 (End)
UNLOCK
*B603718,1 SSH  [Start] actualize cuttkt
*B603718,1 SSH          Allow multiple recieving with actualize.

*B804081,1 KHM 04/04/2001 (Begin) No changes in the budget qty
*B804081,1                in case of actualizination qty is less
*B804081,1                than the budget.
*laData[6]  = CutTktH.Pcs_Bud + ( CutTktl.TotQty - lnTotQty )
laData[6]  = MAX(CutTktH.Pcs_Bud,CutTktH.Pcs_Bud+(CutTktl.TotQty-lnTotQty ))
*B804081,1 KHM 04/04/2001 (End)

laData[10] = MAX(ladata[6] - (lnLotRcv+ladata[8]+ladata[9]),0)
SELECT CutTktH
REPLACE Pcs_Bud WITH laData[6],;
PCS_OPN WITH laData[10]
*B603718,1 SSH 24/07/2000[End]
SELECT CUTTKTL
*B603718,1 SSH  [End]
laData[8]  = laData[8]  + MAX(lnTotQty-CutTktL.TotQty,0)
laData[12] = laData[12] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost1
laData[13] = laData[13] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost2
laData[14] = laData[14] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost3
laData[15] = laData[15] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost4
laData[16] = laData[16] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost5
*khm1
*CASE lcTranType='I'
CASE lcTranType $ 'ID'
*khm1

SET ORDER TO TAG POSLN IN POSLN
*C200080,1 AMM Consider the dye order case
*=SEEK('P'+laData[1]+lcItem+STR(lineno,6)+'1','POSLN')
=SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1]+lcItem+STR(lineno,6)+'1','POSLN')
*C200080,1 AMM end
*B802713,1 Fix computing operation canceled quantity while actualization
*lnOpen1 = POSLN.Qty1-nQty1
*lnOpen2 = POSLN.Qty2-nQty2
*lnOpen3 = POSLN.Qty3-nQty3
*lnOpen4 = POSLN.Qty4-nQty4
*lnOpen5 = POSLN.Qty5-nQty5
*lnOpen6 = POSLN.Qty6-nQty6
*lnOpen7 = POSLN.Qty7-nQty7
*lnOpen8 = POSLN.Qty8-nQty8
lnOpen1 = lnOpen1 + POSLN.Qty1-nQty1
lnOpen2 = lnOpen2 + POSLN.Qty2-nQty2
lnOpen3 = lnOpen3 + POSLN.Qty3-nQty3
lnOpen4 = lnOpen4 + POSLN.Qty4-nQty4
lnOpen5 = lnOpen5 + POSLN.Qty5-nQty5
lnOpen6 = lnOpen6 + POSLN.Qty6-nQty6
lnOpen7 = lnOpen7 + POSLN.Qty7-nQty7
lnOpen8 = lnOpen8 + POSLN.Qty8-nQty8
*B802713,1 (End)
lnOpen  = lnOpen1+lnOpen2+lnOpen3+lnOpen4+lnOpen5+lnOpen6+lnOpen7+lnOpen8

*B602482,1 Keep track of allocated orders while actualization
SELECT POSLN
SCATTER FIELDS ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotOrd TO laOrdQty
SELECT (lcTmpTkt)
IF (POSLN.Ord1 > nQty1 OR POSLN.Ord2 > nQty2 OR POSLN.Ord3 > nQty3 OR ;
POSLN.Ord4 > nQty4 OR POSLN.Ord5 > nQty5 OR POSLN.Ord6 > nQty6 OR ;
POSLN.Ord7 > nQty7 OR POSLN.Ord8 > nQty8)
*E301182,1 Update CT/PO line number in the CUTPICK file.
*=lfTktAllo(lcItem)
=lfTktAllo(lcItem,POSLN.Dyelot,POSLN.LineNo)
*E301182,1 (End)
ENDIF
SELECT POSHDR
=RLOCK()
REPLACE TotOrd WITH TotOrd - POSLN.TotOrd + laOrdQty[9]
UNLOCK
*B602482,1 (End)

*B602203,1 Update Style WIP
SELECT STYLE
=SEEK(POSLN.Style)
=RLOCK()
REPLACE WIP1   WITH WIP1 - (POSLN.Qty1-&lcTmpTkt..nQty1) ,;
WIP2   WITH WIP2 - (POSLN.Qty2-&lcTmpTkt..nQty2) ,;
WIP3   WITH WIP3 - (POSLN.Qty3-&lcTmpTkt..nQty3) ,;
WIP4   WITH WIP4 - (POSLN.Qty4-&lcTmpTkt..nQty4) ,;
WIP5   WITH WIP5 - (POSLN.Qty5-&lcTmpTkt..nQty5) ,;
WIP6   WITH WIP6 - (POSLN.Qty6-&lcTmpTkt..nQty6) ,;
WIP7   WITH WIP7 - (POSLN.Qty7-&lcTmpTkt..nQty7) ,;
WIP8   WITH WIP8 - (POSLN.Qty8-&lcTmpTkt..nQty8) ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
UNLOCK
SELECT STYDYE
=SEEK(POSLN.Style+POSLN.cWareCode+SPACE(10))
=RLOCK()
REPLACE WIP1   WITH WIP1 - (POSLN.Qty1-&lcTmpTkt..nQty1) ,;
WIP2   WITH WIP2 - (POSLN.Qty2-&lcTmpTkt..nQty2) ,;
WIP3   WITH WIP3 - (POSLN.Qty3-&lcTmpTkt..nQty3) ,;
WIP4   WITH WIP4 - (POSLN.Qty4-&lcTmpTkt..nQty4) ,;
WIP5   WITH WIP5 - (POSLN.Qty5-&lcTmpTkt..nQty5) ,;
WIP6   WITH WIP6 - (POSLN.Qty6-&lcTmpTkt..nQty6) ,;
WIP7   WITH WIP7 - (POSLN.Qty7-&lcTmpTkt..nQty7) ,;
WIP8   WITH WIP8 - (POSLN.Qty8-&lcTmpTkt..nQty8) ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
UNLOCK
SELECT POSLN
lnTotQty = Totqty
=RLOCK()
REPLACE Qty1   WITH &lcTmpTkt..nQty1 ,;
Qty2   WITH &lcTmpTkt..nQty2 ,;
Qty3   WITH &lcTmpTkt..nQty3 ,;
Qty4   WITH &lcTmpTkt..nQty4 ,;
Qty5   WITH &lcTmpTkt..nQty5 ,;
Qty6   WITH &lcTmpTkt..nQty6 ,;
Qty7   WITH &lcTmpTkt..nQty7 ,;
Qty8   WITH &lcTmpTkt..nQty8 ,;
TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
*B602482,1 Keep track of allocated orders while actualization
REPLACE Ord1   WITH laOrdQty[1] ,;
Ord2   WITH laOrdQty[2] ,;
Ord3   WITH laOrdQty[3] ,;
Ord4   WITH laOrdQty[4] ,;
Ord5   WITH laOrdQty[5] ,;
Ord6   WITH laOrdQty[6] ,;
Ord7   WITH laOrdQty[7] ,;
Ord8   WITH laOrdQty[8] ,;
TotOrd WITH laOrdQty[9]
*B602482,1 (End)
UNLOCK
SELECT POSHDR
=RLOCK()
REPLACE nFCost1 WITH nFCost1 + (POSLN.TotQty-lnTotQty)*POSLN.nFCost1 ,;
nFCost2 WITH nFCost2 + (POSLN.TotQty-lnTotQty)*POSLN.nFCost2 ,;
nFCost3 WITH nFCost3 + (POSLN.TotQty-lnTotQty)*POSLN.nFCost3 ,;
nFCost4 WITH nFCost4 + (POSLN.TotQty-lnTotQty)*POSLN.nFCost4 ,;
nFCost5 WITH nFCost5 + (POSLN.TotQty-lnTotQty)*POSLN.nFCost5
UNLOCK
laData[8]  = laData[8]  + MAX(lnTotQty-POSLN.TotQty,0)
laData[12] = laData[12] + (POSLN.TotQty-lnTotQty)*POSLN.nICost1
laData[13] = laData[13] + (POSLN.TotQty-lnTotQty)*POSLN.nICost2
laData[14] = laData[14] + (POSLN.TotQty-lnTotQty)*POSLN.nICost3
laData[15] = laData[15] + (POSLN.TotQty-lnTotQty)*POSLN.nICost4
laData[16] = laData[16] + (POSLN.TotQty-lnTotQty)*POSLN.nICost5
CASE lcTranType='T'
SELECT MMFGORDD
SET ORDER TO TAG MMFGORDD
=SEEK(laData[1]+SUBSTR(lcItem,1,7)+lcColor,'MMFGORDD')
LOCATE REST WHILE cMfgOrdNo+cFabric+Color+Dyelot+TranCd=;
laData[1]+SUBSTR(lcItem,1,7)+lcColor FOR TranCd = '1'
*B602203,1 Update Fabric WIP
SELECT FABRIC
=SEEK(MMFGORDD.cFabric+MMFGORDD.Color)
=RLOCK()
REPLACE OnOrder WITH OnOrder - lnOpen
UNLOCK
SELECT FABDYE
=SEEK(MMFGORDD.cFabric+MMFGORDD.Color+MMFGORDD.cWareCode+SPACE(10))
=RLOCK()
REPLACE OnOrder WITH OnOrder - lnOpen
UNLOCK
*B602203,1 (End()
SELECT MMFGORDD
lnTotQty = nMfgTotQty
=RLOCK()
REPLACE nMfgTotQty WITH MAX(nMfgTotQty - lnOpen,0)
UNLOCK
laData[8]  = laData[8]  + MAX(lnOpen,0)
laData[12] = laData[12] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost1
laData[13] = laData[13] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost2
laData[14] = laData[14] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost3
laData[15] = laData[15] + (MMFGORDD.nMfgTotQty-lnTotQty)*MMFGORDD.nCost4
ENDCASE
*B802713,1 Fix computing Actual & open quantity while actualization
*laData[27] = laData[27] + MAX(lnRecv-MFGOPRDT.nActQty,0)
**B802242,1 Update balance while actualization
*laData[10] = laData[27]
**B802242,1 (End)
*B802713,1 (End)

*B602234,1 Update Cost Items Required Quantities upon actualization
*B602234,1 Message : 38157
*B602234,1 Modify the cost sheet basaed on the actual quantity?
*B602234,1 Button : 38006
*B602234,1 < Yes >  < No >

lnUpdYield = IIF(lnUpdYield=0 AND lnOpen <> 0,;
gfModalGen('QRM38157B38006','ALERT'),lnUpdYield)
IF lnUpdYield = 1

SELECT BOMLINE
=SEEK(lcTranType+'1'+laData[1]+STR(&lcTmpTkt..lineno,6))
*B610572,1 TMI 11/03/2013 09:52 [Start] remove the reference to ICLR
*SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
*                lcTranType+'1'+laData[1]+STR(&lcTmpTkt..lineno,6)
SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+mfgcode=;
lcTranType+'1'+laData[1]+STR(&lcTmpTkt..lineno,6)
*B610572,1 TMI 11/03/2013 09:52 [End  ]
STORE 0 TO lnReq1,lnReq2,lnReq3,lnReq4,lnReq5,lnReq6,lnReq7,lnReq8
IF lcTranType = 'T'
lnStyQty = lnOpen
ELSE
lnStyQty = 0
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
IF lcCount $ cSizes
lnStyQty   = lnStyQty + lnOpen&lcCount
lcCompSize = SUBSTR(cCompSizes,AT(lcCount,cSizes),1)
IF !EMPTY(lcCompSize)
lnReq&lcCompSize = lnReq&lcCompSize + lnOpen&lcCount
ENDIF
ENDIF
ENDFOR
ENDIF
lnOldQty = ItemQty
lnOldAmt = ItemAmt
REPLACE StyQty  WITH StyQty-lnStyQty ,;
UnitQty WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(StyQty=0,0,ItemQty/StyQty),UnitQty) ,;
ItemQty WITH StyQty*UnitQty  ,;
ItemAmt WITH ItemQty*UnitCost
SELECT (lcDetFile)
*B610572,1 TMI 11/03/2013 09:53 [Start] remove the reference to ICLR
*=SEEK(BomLine.Style+BomLine.SClr+STR(BomLine.LineNo,6)+BomLine.cBomTyp+;
*      '1'+BomLine.Item+BomLine.IClr+BomLine.MfgCode)
=SEEK(BomLine.Style+BomLine.SClr+STR(BomLine.LineNo,6)+BomLine.cBomTyp+;
'1'+BomLine.Item+BomLine.MfgCode)
*B610572,1 TMI 11/03/2013 09:53 [End  ]
REPLACE StyQty  WITH StyQty-lnStyQty ,;
UnitQty WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(StyQty=0,0,ItemQty/StyQty),UnitQty) ,;
ItemQty WITH StyQty*UnitQty  ,;
ItemAmt WITH ItemQty*UnitCost
=SEEK(BomLine.Style+BomLine.SClr+STR(BomLine.LineNo,6)+BomLine.cBomTyp+'2')
REPLACE ItemQty WITH ItemQty-lnOldQty+BomLine.ItemQty ,;
ItemAmt WITH ItemAmt-lnOldAmt+BomLine.ItemAmt
lnOldQty = ItemQty
lnOldAmt = ItemAmt

SELECT CTKTBOM
*B802956,1 AMM Add dyelot
*=SEEK(lcTranType+laData[1]+Bomline.cBomTyp+Bomline.item+Bomline.iclr+Bomline.mfgcode)
*B610572,1 TMI 11/03/2013 09:53 [Start] remove the reference to ICLR
*=SEEK(lcTranType+laData[1]+Bomline.cBomTyp+Bomline.item+Bomline.iclr+Bomline.mfgcode+Bomline.Dyelot)
=SEEK(lcTranType+laData[1]+Bomline.cBomTyp+Bomline.item+Bomline.mfgcode+Bomline.Dyelot)
*B610572,1 TMI 11/03/2013 09:53 [End  ]
*B802956,1 AMM end
REPLACE Pieces   WITH Pieces-lnStyQty ,;
UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
Req_qty  WITH Pieces*UntQty   ,;
Est_Cost WITH Req_qty*UntCost ,;
Req_Qty1 WITH Req_Qty1-lnReq1*UntQty ,;
Req_Qty2 WITH Req_Qty2-lnReq2*UntQty ,;
Req_Qty3 WITH Req_Qty3-lnReq3*UntQty ,;
Req_Qty4 WITH Req_Qty4-lnReq4*UntQty ,;
Req_Qty5 WITH Req_Qty5-lnReq5*UntQty ,;
Req_Qty6 WITH Req_Qty6-lnReq6*UntQty ,;
Req_Qty7 WITH Req_Qty7-lnReq7*UntQty ,;
Req_Qty8 WITH Req_Qty8-lnReq8*UntQty
SELECT (lcTktSheet)
*B610572,1 TMI 11/03/2013 09:53 [Start] remove the reference to ICLR
*=SEEK(CTKTBOM.TYP+'1'+CTKTBOM.ITEM+CTKTBOM.ICLR+CTKTBOM.MFGCODE+CTKTBOM.DYELOT)
=SEEK(CTKTBOM.TYP+'1'+CTKTBOM.ITEM+CTKTBOM.MFGCODE+CTKTBOM.DYELOT)
*B610572,1 TMI 11/03/2013 09:53 [End  ]

*B802542,1 - WAB - replace also req_qty? to issue the actual qty
*B802542,1 - WAB - START
*REPLACE Pieces   WITH Pieces-lnStyQty ,;
UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
Req_qty  WITH Pieces*UntQty   ,;
Est_Cost WITH Req_qty*UntCost
REPLACE Pieces   WITH Pieces-lnStyQty ,;
UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=lcFirstOpr,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
Req_qty  WITH Pieces*UntQty   		,;
Est_Cost WITH Req_qty*UntCost 		,;
Req_Qty1 WITH Req_Qty1-lnReq1*UntQty 	,;
Req_Qty2 WITH Req_Qty2-lnReq2*UntQty 	,;
Req_Qty3 WITH Req_Qty3-lnReq3*UntQty 	,;
Req_Qty4 WITH Req_Qty4-lnReq4*UntQty 	,;
Req_Qty5 WITH Req_Qty5-lnReq5*UntQty 	,;
Req_Qty6 WITH Req_Qty6-lnReq6*UntQty 	,;
Req_Qty7 WITH Req_Qty7-lnReq7*UntQty 	,;
Req_Qty8 WITH Req_Qty8-lnReq8*UntQty

*B802542,1 - WAB - END

ENDSCAN
GO TOP IN (lcDetFile)
GO TOP IN (lcTktSheet)
ENDIF
*B602234,1 (End)
ENDSCAN
*B802713,1 Fix computing Actual & open quantity while actualization
laData[27] = laData[27] + MAX(lnRecv-MFGOPRDT.nActQty,0)
*B802242,1 Update balance while actualization
*wab
*laData[10] = laData[27]
laData[10] = MAX(ladata[6] - (ladata[7]+ladata[8]+ladata[9]),0)
*wab
*B802242,1 (End)
*B802713,1 (End)

SELECT MFGOPRDT
=RLOCK()
*B603718,1 SSH  [Start] actualize cuttkt
*B603718,1 SSH          Allow multiple recieving with actualize.
*REPLACE nActQty WITH lnRecv
*B603847,1 SSH  [Start] actualize cuttkt.
*REPLACE nActQty WITH lnRecv + nActQty
REPLACE nActQty WITH lnRecv
*B603847,1 End.
*B603718,1 SSH   [End]
UNLOCK
IF lnOpen > 0
*B603243,1 AMM Add the dyelot field
*INSERT INTO MFGOPRDT ;
(cImTyp,cTktNo,cOprCode,cLotNo,Item,Color,lInHouse,cContCode,cContName,;
TranCd,dTranDate,nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,;
nLotQty7,nLotQty8,nLotTotQty) VALUES ;
(lcTranType,laData[1],lcFirstOpr,lcLotNo,lcItem,lcColor,llInHouse,;
lcContCode,lcContName,'4',ldActDate,MAX(lnOpen1,0),MAX(lnOpen2,0),;
MAX(lnOpen3,0),MAX(lnOpen4,0),MAX(lnOpen5,0),MAX(lnOpen6,0),;
MAX(lnOpen7,0),MAX(lnOpen8,0),lnOpen)
INSERT INTO MFGOPRDT ;
(cImTyp,cTktNo,cOprCode,cLotNo,Item,Color,cDyelot,lInHouse,cContCode,cContName,;
TranCd,dTranDate,nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,;
nLotQty7,nLotQty8,nLotTotQty) VALUES ;
(lcTranType,laData[1],lcFirstOpr,lcLotNo,lcItem,lcColor,lcItDye,llInHouse,;
lcContCode,lcContName,'4',ldActDate,MAX(lnOpen1,0),MAX(lnOpen2,0),;
MAX(lnOpen3,0),MAX(lnOpen4,0),MAX(lnOpen5,0),MAX(lnOpen6,0),;
MAX(lnOpen7,0),MAX(lnOpen8,0),lnOpen)
*B603243,1 AMM end
ENDIF
SELECT (lcOprDet)
*B603243,1 AMM make the operation done by style/color/dyelot
*=SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor+'1')
=SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor+'1'+lcItDye)
*B603243,1 AMM  end
*B603718,1 SSH  [Start] actualize cuttkt
*B603718,1 SSH          Allow multiple recieving with actualize.
*REPLACE nActQty WITH lnRecv
*B603847,1 SSH  [Start] actualize cuttkt.
*REPLACE nActQty WITH lnRecv + nActQty
REPLACE nActQty WITH lnRecv
*B603847,1 End.
*B603718,1 SSH   [End]
*B802956,1 AMM If there is comind dyelot in the file, go to it, else go to next style/color
*=SEEK(lcKey)
IF llUseDyelot .AND. lcNxtDye # lcItDye
=SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor+'1'+lcNxtDye)
ELSE
*B604056,1 HBG 12/06/2000 Fix bug variabel 'lcKey' not found [Begin]
*=SEEK(lcKey)
=SEEK(lcKeyVal)
*B604056,1 [End]
ENDIF
*B802956,1 AMM end

ENDDO
IF rbActual = 2
laData[3] = 'A'
lcStatus = 'Actualized'
ENDIF
SELECT (lcBaseFile)
=RLOCK()
GATHER FROM laData FIELDS &lcScFields
REPLACE Act_Date WITH ldActDate
UNLOCK

*B605607,1 AMH Trigger for Cathy Daniels to export C/T to the old system [Start]
IF rbActual = 2 .AND. ASCAN(laEvntTrig , PADR('EXPACTCT',10)) <> 0
=gfDoTriger('MFCSSH',PADR('EXPACTCT',10))
ENDIF
*B605607,1 AMH [End]

SELECT (lcOprDet)
ZAP
SELECT MFGOPRDT
SET ORDER TO TAG MFGOPRDT
IF SEEK(lcTranType+laData[1])
SCAN REST WHILE cimtyp+ctktno = lcTranType+laData[1]
SCATTER MEMVAR
INSERT INTO (lcOprDet) FROM MEMVAR
ENDSCAN
ENDIF
GO TOP IN (lcOprDet)
=lfBrowLots()
SELECT (lnAlias)
ENDIF
SET ORDER TO TAG lcCurrTag IN (lcOprDet)

*!*************************************************************
*! Name      : lfTktAllo
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2004
*! Purpose   : Edit ticket line orders allocated quantity while actualization
*!*************************************************************
*! Calls     : gfOpenFile(),lfEditAlo(),gfModalGen()
*!*************************************************************
*! Parameters: lcStyle : ticket style
*!             lcDyelot: ticket dyelot
*!             lnLineNo: ticket line#
*!             llCanRem: Called from cancel ticket remaining quantity
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfTktAllo(lcStyle)
*!*************************************************************
FUNCTION lfTktAllo
LPARAMETERS lcStyle,lcDyelot,lnLineNo,llCanRem,loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

=gfOpenFile(oAriaApplication.DataDir+'ORDHDR',oAriaApplication.DataDir+'ORDHDR','SH')
=gfOpenFile(oAriaApplication.DataDir+'ORDLINE',oAriaApplication.DataDir+'ORDLINE','SH')

m.TranCd     = IIF(loFormSet.lcTranType='M','1','2')
m.cTktNo     = EVALUATE(loFormSet.lcPosHdr+'.PO')
m.cTktLineNo = STR(lnLineNo,6)
IF !lfOpenSql('CUTPICK',loFormSet.lcTmpCutPik,'CUTPKORD','TranCd+cTktNo+cTktLineNo',loFormSet)
RETURN .F.
ENDIF

lnChoice=3
IF !llRelAll
*Message : 38166
*Style "XXXXX" has pieces allocated from orders.
*Edit the allocated quantity from order lines to keep
*track of this allocation, release allocation for only
*this line, or for all ticket lines.
*Button : 38022
*< Edit Allocation > < Release > < Release All >
IF llCanRem
lnChoice = gfModalGen("QRM38166B38007","DIALOG",loFormSet.lcItmHdr+'|'+lcStyle+;
IIF(EMPTY(lcDyelot),'',' '+ALLTRIM(lcDyelot)))
ELSE
lnChoice = gfModalGen("QRM38166B38022","DIALOG",loFormSet.lcItmHdr+'|'+lcStyle+;
IIF(EMPTY(lcDyelot),'',' '+ALLTRIM(lcDyelot)))
ENDIF
llRelAll = (lnChoice=3)
ENDIF
IF lnChoice=4
RETURN(.F.)
ENDIF
IF lnChoice=1
=lfEditAlo(loFormSet)
ENDIF

SELECT (loFormSet.lcTmpCutPik)
STORE 0 TO laOrdQty
SCAN
SCATTER MEMVAR
IF lnChoice<>1
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.totQty
ENDIF
=lfOpenSql('CUTPICK','CUTPICK','CUTPKORD','TranCd+cTktNo+ctktlineno+Order+Style+cOrdLine',loFormSet)

=SEEK('O'+Order+cOrdLine,'ORDLINE')
=SEEK('O'+Order,'ORDHDR')
SELECT ORDLINE
=RLOCK()
REPLACE Cut1   WITH Cut1  - CUTPICK.Qty1  + m.Qty1 ,;
Cut2   WITH Cut2  - CUTPICK.Qty2  + m.Qty2 ,;
Cut3   WITH Cut3  - CUTPICK.Qty3  + m.Qty3 ,;
Cut4   WITH Cut4  - CUTPICK.Qty4  + m.Qty4 ,;
Cut5   WITH Cut5  - CUTPICK.Qty5  + m.Qty5 ,;
Cut6   WITH Cut6  - CUTPICK.Qty6  + m.Qty6 ,;
Cut7   WITH Cut7  - CUTPICK.Qty7  + m.Qty7 ,;
Cut8   WITH Cut8  - CUTPICK.Qty8  + m.Qty8 ,;
TotCut WITH TotCut- CUTPICK.TotQty+ m.TotQty
UNLOCK
SELECT ORDHDR
=RLOCK()
REPLACE TotCut WITH TotCut - CUTPICK.TotQty + m.TotQty
UNLOCK

SELECT (loFormSet.lcTmpCutPik)
GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty MEMVAR
laOrdQty[1] = laOrdQty[1] + Qty1
laOrdQty[2] = laOrdQty[2] + Qty2
laOrdQty[3] = laOrdQty[3] + Qty3
laOrdQty[4] = laOrdQty[4] + Qty4
laOrdQty[5] = laOrdQty[5] + Qty5
laOrdQty[6] = laOrdQty[6] + Qty6
laOrdQty[7] = laOrdQty[7] + Qty7
laOrdQty[8] = laOrdQty[8] + Qty8
laOrdQty[9] = laOrdQty[9] + TotQty
IF TotQty = 0
DELETE
ENDIF
ENDSCAN

DECLARE laTableUpdate[1,2]
laTableUpdate[1,1] = loFormSet.lcTmpCutPik
laTableUpdate[1,2] = 'CUTPICK'

=lfTableUpdate(loFormSet)

USE IN (loFormSet.lcTmpCutPik)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfEditAlo
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2004
*! Purpose   : Edit ticket line orders allocated quantity while actualization
*!*************************************************************
*! Calls     : MFEDALO.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfEditAlo()
*!*************************************************************
FUNCTION lfEditAlo
LPARAMETERS loFormSet

STORE EVALUATE(loFormSet.lcPosLn+'.Qty1') TO lnCut1
STORE EVALUATE(loFormSet.lcPosLn+'.Qty2') TO lnCut2
STORE EVALUATE(loFormSet.lcPosLn+'.Qty3') TO lnCut3
STORE EVALUATE(loFormSet.lcPosLn+'.Qty4') TO lnCut4
STORE EVALUATE(loFormSet.lcPosLn+'.Qty5') TO lnCut5
STORE EVALUATE(loFormSet.lcPosLn+'.Qty6') TO lnCut6
STORE EVALUATE(loFormSet.lcPosLn+'.Qty7') TO lnCut7
STORE EVALUATE(loFormSet.lcPosLn+'.Qty8') TO lnCut8
=SEEK(STYLE,'STYLE')
llExit = .F.
PRIVATE loParentForm
loParentForm = loFormSet

DO WHILE !llExit
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFEDALO.SCX")
=gfCallForm('MFEDALO')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
=IIF(llExit,.T.,lfvCPOk(loFormSet))
ENDDO

*!*************************************************************
*! Name      : lfwAloQty
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2004
*! Purpose   : browse ticket allocated order liens
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcSize
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfwAloQty()
*!*************************************************************
FUNCTION lfwAloQty
LPARAMETERS lcSize

RETURN(laOrdQty[VAL(lcSize)]>EVALUATE('lnCut'+lcSize)-EVALUATE('lnOpen'+lcSize))

*!*************************************************************
*! Name      : lfvCPOk
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2004
*! Purpose   : Accept modification to order lines allocation
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcSize
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvCPOk()
*!*************************************************************
FUNCTION lfvCPOk
LPARAMETERS loFormSet

LOCAL lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8

SELECT (loFormSet.lcTmpCutPik)
SUM ALL Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8
LOCATE
FOR lnCount = 1 TO Scale.Cnt
lcCount = STR(lnCount,1)
IF EVALUATE('lnOrd'+lcCount) > EVALUATE('lnCut'+lcCount) - EVALUATE('lnOpen'+lcCount)
*Message : 38167
*Allocated quantity for size xxxxx cannot be greater than
*ticket actual quantity.
*Button : 00000
*OK
=gfModalGen('TRM38167B00000','ALERT',EVALUATE('Scale.SZ'+lcCount))
RETURN
ENDIF
ENDFOR
FOR lnCount = 1 TO Scale.Cnt
lcCount = STR(lnCount,1)
laOrdQty[lnCount] = EVALUATE('lnOrd'+lcCount)
ENDFOR
llExit = .T.

*!*************************************************************
*! Name      : lfvLogLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/11/2004
*! Purpose   : Show Operation/Lot issued cost items
*!*************************************************************
*! Calls     : MFLOGLT.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLogLot()
*!*************************************************************
FUNCTION lfvLogLot
LPARAMETERS loFormSet

PRIVATE lcOprCode,lcLotNo,laCstTypes,puCstTypes,laOpr,puOpr,puLots,laLots,lcCurrTag
lcOprCode = EVALUATE(loFormSet.lcOprDet+'.cOprCode')
lcLotNo   = EVALUATE(loFormSet.lcOprDet+'.cLotNo')

puCstTypes = 1
IF loFormSet.lcTranType='T'
DECLARE laCstTypes[5]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laCstTypes[1] = LANG_MFCSSH_ALL
laCstTypes[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ALL,loFormSet.GetHeaderText("LANG_MFCSSH_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laCstTypes[2] = loFormSet.laSetups[3,2]
laCstTypes[3] = loFormSet.laSetups[4,2]
laCstTypes[4] = loFormSet.laSetups[5,2]
laCstTypes[5] = loFormSet.laSetups[6,2]
ELSE
DECLARE laCstTypes[8]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laCstTypes[1] = LANG_MFCSSH_ALL
laCstTypes[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ALL,loFormSet.GetHeaderText("LANG_MFCSSH_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laCstTypes[2] = loFormSet.laSetups[3,2]
laCstTypes[3] = loFormSet.laSetups[4,2]
laCstTypes[4] = loFormSet.laSetups[5,2]
laCstTypes[5] = loFormSet.laSetups[6,2]
laCstTypes[6] = loFormSet.laSetups[7,2]
laCstTypes[7] = loFormSet.laSetups[8,2]
laCstTypes[8] = loFormSet.laSetups[9,2]
ENDIF

LOCAL lcOprHdr,lcOprDet
lcOprHdr = loFormSet.lcOprHdr
lcOprDet = loFormSet.lcOprDet
DECLARE laOpr[1,2]

LOCAL lnEngineBehavior
lnEngineBehavior = SET("EngineBehavior")
SET ENGINEBEHAVIOR 70
SELECT DISTINCT gfCodDes(&lcOprHdr..cOprCode,'MfgCode'),&lcOprHdr..cOprCode;
FROM (lcOprDet),(lcOprHdr);
WHERE &lcOprDet..cOprCode=&lcOprHdr..cOprCode;
ORDER BY &lcOprHdr..cOperSeq INTO ARRAY laOpr
SET ENGINEBEHAVIOR lnEngineBehavior

puOpr = CEILING(ASCAN(laOpr,lcOprCode)/2)

lcCurrTag = ORDER(loFormSet.lcOprDet)
SET ORDER TO TAG 'LOT' IN (loFormSet.lcOprDet)
DECLARE laLots[1]

SELECT DISTINCT cLotNo FROM (loFormSet.lcOPrDet) ;
WHERE cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd = loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+laOpr[puOpr,2] ;
INTO ARRAY laLots
puLots = ASCAN(laLots,lcLotNo)

m.cIMTyp = loFormSet.lcTranType
m.cTktNo = EVALUATE(loFormSet.lcPosHdr+'.PO')
IF !lfOpenSql('BOMCOST','BOMCOST','POBOMCLS','CIMTYP+CTKTNO',loFormSet)
RETURN .F.
ENDIF

SELECT * FROM BOMCOST WHERE !EMPTY(cOprCode) .AND. !EMPTY(cLotNO) ;
INTO DBF (oAriaApplication.WorkDir+loFormSet.lcisslogfile)
INDEX ON cOprCode + cLotNo + cbomtype + cInvType + item + mfgcode + cwarecode + cdyelot + cisession + crsession;
TAG (loFormSet.lcisslogfile)
SET KEY TO laOpr[puOpr,2]+laLots[puLots]
=SEEK(laOpr[puOpr,2]+laLots[puLots])
PRIVATE loParentForm
loParentForm = loFormSet
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFLOGLT.SCX")
=gfCallForm('MFLOGLT')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
USE IN (loFormSet.lcisslogfile)
SELECT (loFormSet.lcOprDet)
SET ORDER TO TAG lcCurrTag IN (loFormSet.lcOprDet)

*!*************************************************************
*! Name      : lfvLogOpPp
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/11/2004
*! Purpose   : Show issued cost items for specific operation
*!*************************************************************
*! Calls     : lfvLogLtPp
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLogOpPp()
*!*************************************************************
FUNCTION lfvLogOpPp
LPARAMETERS loFormSet

LOCAL lcCurrTag
IF puOpr <> loFormSet.AriaForm1.cboOperation.OldValue
lcCurrTag = ORDER(loParentForm.lcOprDet)
SET ORDER TO TAG 'LOT' IN (loParentForm.lcOprDet)
SELECT DISTINCT cLotNo FROM (loParentForm.lcOPrDet) ;
WHERE cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd = loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+laOpr[puOpr,2] ;
INTO ARRAY laLots
puLots = 1
=lfvLogLtPp(loFormSet)
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)
ENDIF

*!*************************************************************
*! Name      : lfvLogLtPp
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/11/2004
*! Purpose   : Show issued cost items for specific Lot
*!*************************************************************
*! Calls     : lfvLogLtPp
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvLogLtPp()
*!*************************************************************
FUNCTION lfvLogLtPp
LPARAMETERS loFormSet

SELECT (loParentForm.lcisslogfile)
IF puCstTypes = 1
SET KEY TO laOpr[puOpr,2]+laLots[puLots]
=SEEK(laOpr[puOpr,2]+laLots[puLots])
ELSE
SET KEY TO laOpr[puOpr,2]+laLots[puLots]+STR(puCstTypes-1,1)
=SEEK(laOpr[puOpr,2]+laLots[puLots]+STR(puCstTypes-1,1))
ENDIF
loFormSet.Refresh

*!*************************************************************
*! Name      : lfSavScr
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/29/2004
*! Purpose   : Save Generated ticket cost sheet
*!*************************************************************
*! Calls     : gfModalGen,lfvNewLot,lfvIssCstItm
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfSavScr()
*!*************************************************************
FUNCTION lfSavScr
LPARAMETERS loFormSet

PRIVATE lcCurrTag,lcFile
GO TOP IN (loFormSet.lcTktSheet)
IF EOF(loFormSet.lcTktSheet)
RETURN
ENDIF

*!*  *C102095,1 AMH Add a customized trigger for Cathy Daniels to [Start]
*!*  *C102095,1 AMH update field bomdate.
*!*  IF ASCAN(laEvntTrig , PADR('UPDBOMDT',10)) <> 0
*!*    =gfDoTriger('MFCSSH',PADR('UPDBOMDT',10))
*!*  ENDIF
*!*  *C102095,1 AMH [End]
SELECT (loFormSet.lcTktSheet)
SCAN FOR cShowType = '1'
SCATTER MEMVAR
INSERT INTO (loFormSet.lcCTKTBOM) FROM MEMVAR
ENDSCAN
SELECT (loFormSet.lcDetFile)
SCAN FOR  cShowType = '1'
SCATTER MEMVAR
INSERT INTO (loFormSet.lcBOMLINE) FROM MEMVAR
ENDSCAN

DECLARE laTableUpdate[2,2]
laTableUpdate[1,1] = loFormSet.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'

laTableUpdate[2,1] = loFormSet.lcBomLine
laTableUpdate[2,2] = 'BOMLINE'

=lfTableUpdate(loFormSet)

*!*	IF OCCURS('MA',oAriaApplication.CompanyInstalledModules) <> 0
*!*	  =gfArDyRl('','',loFormSet.lcDetFile,.T.)
*!*	ENDIF

SET ORDER TO TAG (loFormSet.lcOprHdr) IN (loFormSet.lcOprHdr)
GO TOP IN (loFormSet.lcOprHdr)
loFormSet.lcFirstOpr = EVALUATE(loFormSet.lcOprHdr+'.cOprCode')
lcCurrTag  = ORDER(loFormSet.lcOprDet)
SET ORDER TO TAG 'LOT' IN (loFormSet.lcOprDet)
IF !loFormSet.llAutoGen AND !EMPTY(loFormSet.lcFirstOpr) .AND.;
!SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+loFormSet.lcFirstOpr,loFormSet.lcOprDet)
*E300725,1 Message : 38054
*E300725,1 Do you wish to issue operation xxxx
*E300725,1 Button : 38006
*E300725,1 Yes,No
IF gfModalGen('QRM38054B38006','ALERT',ALLTRIM(gfCodDes(loFormSet.lcFirstOpr,'MfgCode')))=1
GO TOP IN (loFormSet.lcOprHdr)
=lfvNewLot(loFormSet,.T.)
ENDIF
ENDIF
SELECT (loFormSet.lcOprHdr)
SCAN
SCATTER MEMVAR
INSERT INTO (loFormSet.lcMFGOPRHD) FROM MEMVAR
ENDSCAN
SELECT (loFormSet.lcOprDet)
SCAN
SCATTER MEMVAR
INSERT INTO (loFormSet.lcMFGOPRDT) FROM MEMVAR
ENDSCAN

DECLARE laTableUpdate[2,2]
laTableUpdate[1,1] = loFormSet.lcMfgOprHd
laTableUpdate[1,2] = 'MFGOPRHD'

laTableUpdate[2,1] = loFormSet.lcMfgOprDt
laTableUpdate[2,2] = 'MFGOPRDT'

=lfTableUpdate(loFormSet)

*E300725,1 Message : 38055
*E300725,1 Do you wish to issue cost items for operation xxxx
*E300725,1 Button : 38006
*E300725,1 Yes,No
IF !EMPTY(loFormSet.lcFirstOpr) .AND. SEEK(loFormSet.lcTranType+EVALUATE(loFormSet.lcPosHdr+'.PO')+loFormSet.lcFirstOpr,loFormSet.lcOprDet) .AND. ;
gfModalGen('QRM38055B38006','ALERT',ALLTRIM(gfCodDes(loFormSet.lcFirstOpr,'MfgCode')))=1
=lfvIssCstItm(loFormSet.lcFirstOpr,'01',loFormSet)
ENDIF

SELECT (loFormSet.lcTmpTkt)
lnCanQty = 0
SCAN
SELECT (loFormSet.lcPosLn)
GO EVALUATE(loFormSet.lcTmpTkt+'.nRecNo')
lnCanQty = lnCanQty + MAX(TotQty-EVALUATE(loFormSet.lcTmpTkt+'.nTotQty'),0)
REPLACE Qty1   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty1') ,;
Qty2   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty2') ,;
Qty3   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty3') ,;
Qty4   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty4') ,;
Qty5   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty5') ,;
Qty6   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty6') ,;
Qty7   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty7') ,;
Qty8   WITH EVALUATE(loFormSet.lcTmpTkt+'.nQty8') ,;
TotQty WITH EVALUATE(loFormSet.lcTmpTkt+'.nTotQty')
ENDSCAN
SELECT (loFormSet.lcPosHdr)
REPLACE Cancel WITH Cancel + lnCanQty,;
Open   WITH Open   - lnCanQty

=lfAddNewItm(loFormSet)

*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*=lfUpdEstCst(loFormSet)
DECLARE laFrgnCost[7], laEquCost[7]
STORE 0 TO laFrgnCost, laEquCost
=lfUpdEstCst(loFormSet,@laFrgnCost, @laEquCost)
*N000587,1 WAM 12/01/2007 (End)

SELECT (loFormSet.lcPosHdr)
REPLACE Status WITH 'O'

*B609262,1 WAM 05/20/2010 Recalculate total cost in POSHDR
*IF loFormSet.lcTranType $ 'IDN'
*B609262,1 WAM 05/20/2010 (End)

FOR lnCount = 1 TO 7
lcCount = STR(lnCount,1)
IF loFormSet.laSetups[lnCount+14,2] = 'P' AND loFormSet.llAutoGen
LOOP
ENDIF
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*!*	    DO CASE
*!*	      CASE loFormSet.laSetups[lnCount+14,2] = 'P'
*!*	        REPLACE ('nFCost'+lcCount) WITH EVALUATE('nICost'+lcCount+' /(1 '+loFormSet.lcPExSign+' '+;
*!*	                                                 loFormSet.lcPOSHDR+'.nPriceRat '+loFormSet.lcPUntSin+;
*!*	                                                 ' IIF('+loFormSet.lcPOSHDR+'.nCUrrUnit=0 OR ISNULL(nCUrrUnit),1,'+;
*!*	                                                 loFormSet.lcPOSHDR+'.nCurrUnit))')
*!*	      CASE !INLIST(loFormSet.laSetups[lnCount+14,2],'F','T','S')
*!*	        REPLACE ('nFCost'+lcCount) WITH EVALUATE('nICost'+lcCount+' /(1 '+loFormSet.lcDExSign+' '+;
*!*	                                                 loFormSet.lcPOSHDR+'.nDutyRat '+loFormSet.lcDUntSin+;
*!*	                                                 ' IIF('+loFormSet.lcPOSHDR+'.nDCurUnit=0,1,'+;
*!*	                                                 loFormSet.lcPOSHDR+'.nDCurUnit))')
*!*	      OTHERWISE
*!*	        REPLACE ('nFCost'+lcCount) WITH EVALUATE('nICost'+lcCount)
*!*	    ENDCASE
REPLACE ('nFCost'+lcCount) WITH laFrgnCost[lnCount] ,;
('nICost'+lcCount) WITH laEquCost[lnCount]
*N000587,1 WAM 12/01/2007 (End)
ENDFOR
REPLACE PoTotal WITH nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7
*! B611745,1 Heba HMS, 18/03/2019 - Aria 5 - Error with purchase order [T20190313.0002 ][Begin]
=gfAdd_Info(loFormSet.lcPosHdr)
*! B611745,1 Heba HMS, 18/03/2019 - Aria 5 - Error with purchase order [T20190313.0002 ][End]
*B609262,1 WAM 05/20/2010 Recalculate total cost in POSHDR
*ENDIF
*B609262,1 WAM 05/20/2010 (End)

DECLARE laTableUpdate[2,2]
laTableUpdate[1,1] = loFormSet.lcPosHdr
laTableUpdate[1,2] = 'POSHDR'

laTableUpdate[2,1] = loFormSet.lcPosLn
laTableUpdate[2,2] = 'POSLN'

=lfTableUpdate(loFormSet)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*llDummy = loFormSet.llPWInst .AND. (loFormSet.lcTranType = "M") .AND. lfPwUpdate(loFormSet)
llDummy = loFormSet.llPWInst .AND. !gfGetMemVar('LUSEBUNDLE',oAriaApplication.ActiveCompanyID)  .AND. (loFormSet.lcTranType = "M") .AND. lfPwUpdate(loFormSet)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
IF loFormSet.llPWInst .AND. (loFormSet.lcTranType = "M") .AND. USED(loFormSet.lcPWCtkBom)
SELECT (loFormSet.lcPWCtkBom)
ZAP
ENDIF
IF loFormSet.llPWInst .AND. (loFormSet.lcTranType = "M") .AND. USED(loFormSet.lcDetLin)
SELECT(loFormSet.lcDetLin)
ZAP
ENDIF

SET ORDER TO TAG lcCurrTag IN (loFormSet.lcOprDet)
SELECT (loFormSet.lcPosHdr)

IF !loFormSet.llAutoGen
loFormSet.ChangeMode('V')
oariaapplication.otoolbar.ButtonRefresh()
oariaapplication.otoolbar.NavRefresh()
loFormSet.ariaForm1.pgfCstSht.Page1.grdCstSht.SetFocus
loFormSet.ariaForm1.pgfCstSht.page1.grdCstSht.Refresh
ENDIF

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/29/2003
*! Purpose   : Initialize cost sheet information
*!*************************************************************
*! Calls     : lfBrowse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfGetInfo()
*!*************************************************************
FUNCTION lfGetInfo
LPARAMETERS loFormSet

WITH loFormSet.ariaForm1.pgfCstSht
WITH .Page1
.grdCstSht.RecordSource = ''
ENDWITH

WITH .Page2
.grdOperations.RecordSource = ''
.grdLots.RecordSource = ''
ENDWITH

WITH .Page3
.grdDetReq.RecordSource = ''
ENDWITH
ENDWITH

=gfOpenFile(oAriaApplication.DataDir+'SCALE',oAriaApplication.DataDir+'SCALE','SH')
=gfOpenFile(oAriaApplication.DataDir+'STYLE',oAriaApplication.DataDir+'STYLE','SH')
=gfOpenFile(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE
SELECT (loFormSet.lcTmpTkt)
ZAP
m.cBusDocu = loFormSet.lcBusDocu
m.cStyType = loFormSet.lcStyType
m.Po       = IIF(loFormSet.ActiveMode="S",SPACE(6),EVALUATE(loFormSet.lcPosHdr+'.PO'))
IF !lfOpenSql('POSLN',loFormSet.lcPosLn,'POSLN','CBUSDOCU+CSTYTYPE+PO',loFormSet)
RETURN .F.
ENDIF

loFormSet.lcTranFile = loFormSet.lcPosLn
SELECT (loFormSet.lcPosLn)
SCAN FOR TranCd='1'
SELECT (loFormSet.lcTmpTkt)
APPEND BLANK
REPLACE Item      WITH EVALUATE(loFormSet.lcPosLn+'.Style') ,;
cInvType  WITH EVALUATE(loFormSet.lcPosLn+'.cInvType') ,;
cDyelot   WITH EVALUATE(loFormSet.lcPosLn+'.Dyelot') ,;
cWareCode WITH EVALUATE(loFormSet.lcPosLn+'.cWareCode') ,;
LineNo    WITH EVALUATE(loFormSet.lcPosLn+'.LineNo') ,;
nQty1     WITH EVALUATE(loFormSet.lcPosLn+'.Qty1') ,;
nQty2     WITH EVALUATE(loFormSet.lcPosLn+'.Qty2') ,;
nQty3     WITH EVALUATE(loFormSet.lcPosLn+'.Qty3') ,;
nQty4     WITH EVALUATE(loFormSet.lcPosLn+'.Qty4') ,;
nQty5     WITH EVALUATE(loFormSet.lcPosLn+'.Qty5') ,;
nQty6     WITH EVALUATE(loFormSet.lcPosLn+'.Qty6') ,;
nQty7     WITH EVALUATE(loFormSet.lcPosLn+'.Qty7') ,;
nQty8     WITH EVALUATE(loFormSet.lcPosLn+'.Qty8') ,;
nTotQty   WITH EVALUATE(loFormSet.lcPosLn+'.TotQty') ,;
nRecNo    WITH RECNO(loFormSet.lcPosLn)
ENDSCAN
SELECT (loFormSet.lcTktSheet)
ZAP
m.cImTyp = loFormSet.lcTranType
m.CutTkt = IIF(loFormSet.ActiveMode="S",SPACE(6),EVALUATE(loFormSet.lcPosHdr+'.PO'))
IF !lfOpenSql('CTKTBOM',loFormSet.lcCtktBom,'CTKTBOM','CIMTYP+CUTTKT',loFormSet)
RETURN .F.
ENDIF

SELECT (loFormSet.lcCtktBom)
SCAN FOR !lVoid
SCATTER MEMVAR
IF !SEEK(Typ+'0',loFormSet.lcTktSheet)
INSERT INTO (loFormSet.lcTktSheet) (TYP,Dyelot,cShowType,Item) VALUES ;
(m.Typ,lfGetDye(loFormSet,VAL(m.Typ)),'0',loFormset.laSetups[2+VAL(EVALUATE(loFormSet.lcCTKTBOM+'.Typ')),2])
ENDIF
m.cShowType = '1'
m.TotStk    = lfGetTotStk(loFormSet)
m.UomUse    = lfGetUom(m.cUomCode)
INSERT INTO (loFormSet.lcTktSheet) FROM MEMVAR
ENDSCAN

SELECT (loFormSet.lcTktSheet)
GO TOP IN (loFormSet.lcTktSheet)

m.cImTyp = loFormSet.lcTranType
m.cTktNo = IIF(loFormSet.ActiveMode="S",SPACE(6),EVALUATE(loFormSet.lcPosHdr+'.PO'))
IF !lfOpenSql('BOMLINE',loFormSet.lcBomLine,'MFGOPR','CIMTYP+CTKTNO',loFormSet)
RETURN .F.
ENDIF

SELECT (loFormSet.lcBomLine)
=lfSetIndex(loFormSet.lcBomLine,'BOMLINE','CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE')

=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*!*	DIMENSION laFileStru[lnFileStru+2,18]
*!*	laFileStru[lnFileStru+1,1] = 'cShowType'
*!*	laFileStru[lnFileStru+1,2] = 'C'
*!*	laFileStru[lnFileStru+1,3] = 1
*!*	laFileStru[lnFileStru+1,4] = 0
*!*	laFileStru[lnFileStru+2,1] = 'lHideTit'
*!*	laFileStru[lnFileStru+2,2] = 'L'
*!*	laFileStru[lnFileStru+2,3] = 1
*!*	laFileStru[lnFileStru+2,4] = 0
*!*	LOCAL lnI,lnJ
*!*	FOR lnI = 7 TO 16
*!*	  STORE '' TO laFileStru[lnFileStru+1,lnI],laFileStru[lnFileStru+2,lnI]
*!*	ENDFOR
*!*	STORE 0 TO laFileStru[lnFileStru+1,17],laFileStru[lnFileStru+2,17],;
*!*	           laFileStru[lnFileStru+1,18],laFileStru[lnFileStru+2,18]

DIMENSION laFileStru[lnFileStru+3,18]
laFileStru[lnFileStru+1,1] = 'cShowType'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lHideTit'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'NEQU_AMT'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 13
laFileStru[lnFileStru+3,4] = 3

LOCAL lnI,lnJ
FOR lnI = 7 TO 16
STORE '' TO laFileStru[lnFileStru+1,lnI],laFileStru[lnFileStru+2,lnI],laFileStru[lnFileStru+3,lnI]
ENDFOR
STORE 0 TO laFileStru[lnFileStru+1,17],laFileStru[lnFileStru+2,17],laFileStru[lnFileStru+3,17],;
laFileStru[lnFileStru+1,18],laFileStru[lnFileStru+2,18],laFileStru[lnFileStru+3,18]
*N000587,1 WAM 12/01/2007 (End)

LOCAL ARRAY laIndex[2,2]
laIndex[1,1] = 'cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+cInvType+style+cInvTypC+item+mfgcode'
laIndex[1,2] = 'BOMLINE'
laIndex[2,1] = 'cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode'
laIndex[2,2] = loFormSet.lcDetFile
=gfCrtTmp(loFormSet.lcDetFile,@laFileStru,@laIndex)
SET ORDER TO TAG (loFormSet.lcDetFile) IN (loFormSet.lcDetFile)

m.cImTyp = loFormSet.lcTranType
m.cTktNo = IIF(loFormSet.ActiveMode="S",SPACE(6),EVALUATE(loFormSet.lcPosHdr+'.PO'))
=lfOpenSql('MFGOPRHD',loFormSet.lcMfgOprHd,'MFGOPRHD','CIMTYP+CTKTNO',loFormSet)
m.cImTyp = loFormSet.lcTranType
m.cTktNo = IIF(loFormSet.ActiveMode="S",SPACE(6),EVALUATE(loFormSet.lcPosHdr+'.PO'))
=lfOpenSql('MFGOPRDT',loFormSet.lcMfgOprDt,'MFGOPRDT','CIMTYP+CTKTNO',loFormSet)
LOCAL llHideTit,lcOprCode,lcFrstOpr,lnFrstOprSq
SELECT (loFormSet.lcBomLine)
SCAN FOR ctype = '1' AND !lVoid
SCATTER MEMVAR
IF !SEEK(cInvType+Style+STR(LineNo,6)+cBomTyp+'0',loFormSet.lcDetFile)
llHideTit = SEEK(cInvType+Style+STR(LineNo,6),loFormSet.lcDetFile)
INSERT INTO (loFormSet.lcDetFile) ;
(cInvType,Style,LineNo,cBomTyp,cShowType,Item,lHideTit,Dyelot) VALUES ;
(EVALUATE(loFormSet.lcBomLine+'.cInvType'),EVALUATE(loFormSet.lcBomLine+'.Style'),EVALUATE(loFormSet.lcBomLine+'.LineNo'),;
EVALUATE(loFormSet.lcBomLine+'.cBomTyp'),'0',loFormSet.laSetups[2+VAL(EVALUATE(loFormSet.lcBomLine+'.cBomTyp')),2],;
llHideTit,lfGetDye(loFormSet,VAL(EVALUATE(loFormSet.lcBomLine+'.cBomTyp'))))
ENDIF
m.cShowType = '1'
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
STORE '/' TO lcExSign,lcUntSin
lcPosHdr = loFormSet.lcPosHdr
IF ISNULL(cCurrCode) OR EMPTY(cCurrCode)
DO CASE
CASE cCatgTyp = 'P'
lcCurrCode = IIF(EMPTY(&lcPosHdr..cPriceCur),oAriaApplication.BaseCurrency,&lcPosHdr..cPriceCur)
lnExRate   = IIF(&lcPosHdr..nPriceRat=0,1, &lcPosHdr..nPriceRat)
lnCurrUnit = IIF(&lcPosHdr..nCurrUnit=0,1, &lcPosHdr..nCurrUnit)
CASE INLIST(cCatgTyp,'D','M')
lcCurrCode = IIF(EMPTY(&lcPosHdr..cDutyCur),oAriaApplication.BaseCurrency,&lcPosHdr..cDutyCur)
lnExRate   = IIF(&lcPosHdr..nDutyRat=0,1, &lcPosHdr..nDutyRat)
lnCurrUnit = IIF(&lcPosHdr..nDCurUnit=0,1, &lcPosHdr..nDCurUnit)
OTHERWISE
lcCurrCode = oAriaApplication.BaseCurrency
lnExRate   = 1
lnCurrUnit = 1
ENDCASE
ELSE
lcCurrCode = cCurrCode
lnExRate   = IIF(ISNULL(nExRate) OR nExRate=0,1,nExRate)
lnCurrUnit = IIF(ISNULL(nCurrUnit) OR nCurrUnit=0,1,nCurrUnit)
ENDIF
lcExSign   = gfGetExSin(@lcUntSin, lcCurrCode)
lnEquAmount= ItemAmt &lcExSign lnExRate &lcUntSin lnCurrUnit
m.NEQU_AMT = lnEquAmount
*N000587,1 WAM 12/01/2007 (End)

INSERT INTO (loFormSet.lcDetFile) FROM MEMVAR
IF !SEEK(cInvType+Style+STR(LineNo,6)+cBomTyp+'2',loFormSet.lcDetFile)
INSERT INTO (loFormSet.lcDetFile) ;
(cInvType,Style,LineNo,cBomTyp,cShowType,Item) VALUES ;
(EVALUATE(loFormSet.lcBomLine+'.cInvType'),EVALUATE(loFormSet.lcBomLine+'.Style'),EVALUATE(loFormSet.lcBomLine+'.LineNo'),;
EVALUATE(loFormSet.lcBomLine+'.cBomTyp'),'2','***** '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_TOTAL,loFormSet.GetHeaderText("LANG_MFCSSH_TOTAL",loFormSet.HeaderAlias))+' :')

ENDIF
SELECT (loFormSet.lcDetFile)
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*REPLACE ItemQty  WITH ItemQty + EVALUATE(loFormSet.lcBomLine+'.ItemQty'),;
ItemAmt  WITH ItemAmt + EVALUATE(loFormSet.lcBomLine+'.ItemAmt')
REPLACE ItemQty  WITH ItemQty + EVALUATE(loFormSet.lcBomLine+'.ItemQty'),;
ItemAmt  WITH ItemAmt + EVALUATE(loFormSet.lcBomLine+'.ItemAmt') ,;
NEQU_AMT WITH NEQU_AMT + m.NEQU_AMT
*N000587,1 WAM 12/01/2007 (End)

lcOprCode = IIF(EMPTY(EVALUATE(loFormSet.lcBomLine+'.cOprCode')),EVALUATE(loFormSet.lcBomLine+'.MfgCode'),;
EVALUATE(loFormSet.lcBomLine+'.cOprCode'))
IF !EMPTY(lcOprCode)
SELECT (loFormSet.lcMfgOprHd)
LOCATE FOR COPRCODE = lcOprCode
IF FOUND() .AND. SEEK(EVALUATE(loFormSet.lcBomLine+'.cInvType')+EVALUATE(loFormSet.lcBomLine+'.Style')+STR(EVALUATE(loFormSet.lcBomLine+'.LineNo'),6),loFormSet.lcTmpTkt)
SELECT (loFormSet.lcTmpTkt)
SELECT MIN(cOperSeq) FROM (loFormSet.lcMfgOprHd) INTO ARRAY laTmp1
IF _TALLY > 0
SELECT coprCode FROM (loFormSet.lcMfgOprHd) WHERE cOperSeq = laTmp1[1] INTO ARRAY laTmp2
lcFrstOpr = laTmp2[1]
lnFrstOprSq = VAL(laTmp1[1])

REPLACE cFrstOpr   WITH lcFrstOpr ,;
nFrstOprSq WITH lnFrstOprSq
ENDIF
ENDIF
ENDIF
ENDSCAN
GO TOP IN (loFormSet.lcDetFile)

SELECT (loFormSet.lcMfgOprHd)
=AFIELDS(laFileStru)
LOCAL ARRAY laIndex[2,2]
laIndex[1,1] = 'cIMTyp+cTktNo+cOprCode'
laIndex[1,2] = 'MFGOPRHD'
laIndex[2,1] = 'cOperSeq+cOprCode'
laIndex[2,2] = loFormSet.lcOprHdr
=gfCrtTmp(loFormSet.lcOprHdr,@laFileStru,@laIndex)
SELECT (loFormSet.lcOprHdr)
SET ORDER TO TAG (loFormSet.lcOprHdr)
APPEND FROM DBF(loFormSet.lcMfgOprHd) FOR !lvoid

GO TOP IN (loFormSet.lcOprHdr)
loFormSet.lcFirstOpr = EVALUATE(loFormSet.lcOprHdr+'.cOprCode')

SELECT (loFormSet.lcMfgOprDt)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,18]
laFileStru[lnFileStru+1,1] = 'cOperSeq'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

FOR lnI = 7 TO 16
STORE '' TO laFileStru[lnFileStru+1,lnI]
ENDFOR
STORE 0 TO laFileStru[lnFileStru+1,17],laFileStru[lnFileStru+1,18]
LOCAL ARRAY laIndex[2,2]
laIndex[1,1] = 'cIMTYp+cTktNo+cOprCode+cInvType+Item+cDyelot'
laIndex[1,2] = 'Item'
laIndex[2,1] = 'cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot'
laIndex[2,2] = 'Lot'
=gfCrtTmp(loFormSet.lcOprDet,@laFileStru,@laIndex)
SET ORDER TO TAG Lot IN (loFormSet.lcOprDet)

SELECT (loFormSet.lcMfgOprDt)
SCAN
SCATTER MEMVAR
SELECT (loFormSet.lcMfgOprHd)
LOCATE FOR cOprCode = m.cOprCode
IF FOUND()
SCATTER MEMVAR FIELDS cOperSeq
INSERT INTO (loFormSet.lcOprDet) FROM MEMVAR
ENDIF
ENDSCAN

DIMENSION laFileStru[45,18]
laFileStru[01,1] = 'cIMTyp'
laFileStru[01,2] = 'C'
laFileStru[01,3] = 1
laFileStru[01,4] = 0

laFileStru[02,1] = 'cTktNo'
laFileStru[02,2] = 'C'
laFileStru[02,3] = 6
laFileStru[02,4] = 0

laFileStru[03,1] = 'cDyelot'
laFileStru[03,2] = 'C'
laFileStru[03,3] = 10
laFileStru[03,4] = 0

laFileStru[04,1] = 'cOprCode'
laFileStru[04,2] = 'C'
laFileStru[04,3] = 6
laFileStru[04,4] = 0

laFileStru[05,1] = 'cOperSeq'
laFileStru[05,2] = 'C'
laFileStru[05,3] = 2
laFileStru[05,4] = 0

laFileStru[06,1] = 'cLotNo'
laFileStru[06,2] = 'C'
laFileStru[06,3] = 2
laFileStru[06,4] = 0

laFileStru[07,1] = 'cContCode'
laFileStru[07,2] = 'C'
laFileStru[07,3] = 6
laFileStru[07,4] = 0

laFileStru[08,1] = 'Item'
laFileStru[08,2] = 'C'
laFileStru[08,3] = 19
laFileStru[08,4] = 0

laFileStru[09,1] = 'cInvType'
laFileStru[09,2] = 'C'
laFileStru[09,3] = 4
laFileStru[09,4] = 0

laFileStru[10,1] = 'Iss1'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 6
laFileStru[10,4] = 0

laFileStru[11,1] = 'Iss2'
laFileStru[11,2] = 'N'
laFileStru[11,3] = 6
laFileStru[11,4] = 0

laFileStru[12,1] = 'Iss3'
laFileStru[12,2] = 'N'
laFileStru[12,3] = 6
laFileStru[12,4] = 0

laFileStru[13,1] = 'Iss4'
laFileStru[13,2] = 'N'
laFileStru[13,3] = 6
laFileStru[13,4] = 0

laFileStru[14,1] = 'Iss5'
laFileStru[14,2] = 'N'
laFileStru[14,3] = 6
laFileStru[14,4] = 0

laFileStru[15,1] = 'Iss6'
laFileStru[15,2] = 'N'
laFileStru[15,3] = 6
laFileStru[15,4] = 0

laFileStru[16,1] = 'Iss7'
laFileStru[16,2] = 'N'
laFileStru[16,3] = 6
laFileStru[16,4] = 0

laFileStru[17,1] = 'Iss8'
laFileStru[17,2] = 'N'
laFileStru[17,3] = 6
laFileStru[17,4] = 0

laFileStru[18,1] = 'Iss'
laFileStru[18,2] = 'N'
laFileStru[18,3] = 7
laFileStru[18,4] = 0

laFileStru[19,1] = 'Rcv1'
laFileStru[19,2] = 'N'
laFileStru[19,3] = 6
laFileStru[19,4] = 0

laFileStru[20,1] = 'Rcv2'
laFileStru[20,2] = 'N'
laFileStru[20,3] = 6
laFileStru[20,4] = 0

laFileStru[21,1] = 'Rcv3'
laFileStru[21,2] = 'N'
laFileStru[21,3] = 6
laFileStru[21,4] = 0

laFileStru[22,1] = 'Rcv4'
laFileStru[22,2] = 'N'
laFileStru[22,3] = 6
laFileStru[22,4] = 0

laFileStru[23,1] = 'Rcv5'
laFileStru[23,2] = 'N'
laFileStru[23,3] = 6
laFileStru[23,4] = 0

laFileStru[24,1] = 'Rcv6'
laFileStru[24,2] = 'N'
laFileStru[24,3] = 6
laFileStru[24,4] = 0

laFileStru[25,1] = 'Rcv7'
laFileStru[25,2] = 'N'
laFileStru[25,3] = 6
laFileStru[25,4] = 0

laFileStru[26,1] = 'Rcv8'
laFileStru[26,2] = 'N'
laFileStru[26,3] = 6
laFileStru[26,4] = 0

laFileStru[27,1] = 'Rcv'
laFileStru[27,2] = 'N'
laFileStru[27,3] = 7
laFileStru[27,4] = 0

laFileStru[28,1] = 'Can1'
laFileStru[28,2] = 'N'
laFileStru[28,3] = 6
laFileStru[28,4] = 0

laFileStru[29,1] = 'Can2'
laFileStru[29,2] = 'N'
laFileStru[29,3] = 6
laFileStru[29,4] = 0

laFileStru[30,1] = 'Can3'
laFileStru[30,2] = 'N'
laFileStru[30,3] = 6
laFileStru[30,4] = 0

laFileStru[31,1] = 'Can4'
laFileStru[31,2] = 'N'
laFileStru[31,3] = 6
laFileStru[31,4] = 0

laFileStru[32,1] = 'Can5'
laFileStru[32,2] = 'N'
laFileStru[32,3] = 6
laFileStru[32,4] = 0

laFileStru[33,1] = 'Can6'
laFileStru[33,2] = 'N'
laFileStru[33,3] = 6
laFileStru[33,4] = 0

laFileStru[34,1] = 'Can7'
laFileStru[34,2] = 'N'
laFileStru[34,3] = 6
laFileStru[34,4] = 0

laFileStru[35,1] = 'Can8'
laFileStru[35,2] = 'N'
laFileStru[35,3] = 6
laFileStru[35,4] = 0

laFileStru[36,1] = 'Can'
laFileStru[36,2] = 'N'
laFileStru[36,3] = 7
laFileStru[36,4] = 0

laFileStru[37,1] = 'Dmg1'
laFileStru[37,2] = 'N'
laFileStru[37,3] = 6
laFileStru[37,4] = 0

laFileStru[38,1] = 'Dmg2'
laFileStru[38,2] = 'N'
laFileStru[38,3] = 6
laFileStru[38,4] = 0

laFileStru[39,1] = 'Dmg3'
laFileStru[39,2] = 'N'
laFileStru[39,3] = 6
laFileStru[39,4] = 0

laFileStru[40,1] = 'Dmg4'
laFileStru[40,2] = 'N'
laFileStru[40,3] = 6
laFileStru[40,4] = 0

laFileStru[41,1] = 'Dmg5'
laFileStru[41,2] = 'N'
laFileStru[41,3] = 6
laFileStru[41,4] = 0

laFileStru[42,1] = 'Dmg6'
laFileStru[42,2] = 'N'
laFileStru[42,3] = 6
laFileStru[42,4] = 0

laFileStru[43,1] = 'Dmg7'
laFileStru[43,2] = 'N'
laFileStru[43,3] = 6
laFileStru[43,4] = 0

laFileStru[44,1] = 'Dmg8'
laFileStru[44,2] = 'N'
laFileStru[44,3] = 6
laFileStru[44,4] = 0

laFileStru[45,1] = 'Dmg'
laFileStru[45,2] = 'N'
laFileStru[45,3] = 7
laFileStru[45,4] = 0

FOR lnI = 7 TO 16
FOR lnJ = 1 TO 45
laFileStru[lnJ,lnI] = ''
ENDFOR
ENDFOR
FOR lnJ = 1 TO 45
STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
ENDFOR

=gfCrtTmp(loFormSet.lcOprDet1,@laFileStru)
GO TOP IN (loFormSet.lcOprDet)
=lfAfterInit(loFormSet)

*!*************************************************************
*! Name      : lfvDetLvl
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/11/2004
*! Purpose   : Operations Level of Details
*!*************************************************************
*! Calls     : MFDETLVL.SPX,lfBrowLots
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvDetLvl()
*!*************************************************************
FUNCTION lfvDetLvl
LPARAMETERS loFormSet

PRIVATE llBtStatus,loParentForm
loParentForm = loFormSet
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFDETLVL.SCX")
=gfCallForm('MFDETLVL')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
=lfBrowLots(loFormSet)
llBtStatus=(loFormSet.rbSummary=1)
WITH loFormSet.AriaForm1.pgfCstSht.Page2
STORE llBtStatus TO .cmdIssItm.Enabled, .cmdAutoIssue.Enabled, .cmdModLot.Enabled, .cmdReceive.Enabled
ENDWITH

*!*************************************************************
*! Name      : lfDfPopup
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/11/2004
*! Purpose   : Adjust operations level of details popup
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfDfPopup()
*!*************************************************************
FUNCTION lfDfPopup
LPARAMETERS lnSort,loFormSet

LOCAL lcSort
lcSort = STR(lnSort,1)
IF EVALUATE('loFormSet.AriaForm1.chkSort'+lcSort+'.Value')
loParentForm.lcpusort = loParentForm.lcpusort + IIF(EMPTY(loParentForm.lcpusort ),'',',') + loParentForm.laSort[lnSort,2]
ELSE
loParentForm.lcpusort = STRTRAN(loParentForm.lcpusort,','+loParentForm.laSort[lnSort,2])
loParentForm.lcpusort = STRTRAN(loParentForm.lcpusort,loParentForm.laSort[lnSort,2]+',')
ENDIF
loFormSet.AriaForm1.lstSort.RowSource = loParentForm.lcpusort

*!*************************************************************
*! Name      : lfvOkDetLvl
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/11/2004
*! Purpose   : Validate operations level of details screen
*!*************************************************************
*! Calls     : lfAdjDetLvl
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvOkDetLvl()
*!*************************************************************
FUNCTION lfvOkDetLvl

lcIndExp = 'cIMTyp+cTktNo'
DIMENSION laSort[1,1]
STORE '' TO laSort
=gfSubStr(loParentForm.lcpusort,@laSort,',')
FOR lnCount = 1 TO ALEN(laSort,1)
lnSort = ASCAN(loParentForm.laSort,laSort[lnCount,1])/2
lcIndExp = lcIndExp+'+'+IIF(loParentForm.laSort[lnSort,1]='COPRCODE','COPERSEQ+','')+loParentForm.laSort[lnSort,1]
ENDFOR
=lfAdjDetLvl()

*!*************************************************************
*! Name      : lfAdjDetLvl
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/11/2004
*! Purpose   : Create operation details file based on the selected level of details
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfAdjDetLvl()
*!*************************************************************
FUNCTION lfAdjDetLvl

SELECT (loParentForm.lcOprDet)
INDEX ON &lcIndExp TAG (loParentForm.lcOprDet)
IF loParentForm.rbSummary = 2
lcGroupBy = STRTRAN(lcIndExp,'+',',')
SELECT (loParentForm.lcOprDet1)
ZAP
INDEX ON &lcIndExp TAG (loParentForm.lcOprDet1)
SELECT (loParentForm.lcOprDet)
GO TOP
DO WHILE !EOF()
lcCurrKey = EVALUATE(lcIndExp)
SCATTER MEMVAR
INSERT INTO (loParentForm.lcOprDet1) ;
(cImTyp,cTktNo,cOprCode,cOperSeq,cLotNo,cCOntCode,Item,cInvType,cDyelot) VALUES ;
(m.cImTyp,m.cTktNo,m.cOprCode,m.cOperSeq,m.cLotNo,m.cCOntCode,m.Item,m.cInvType,m.cDyelot)
SCAN WHILE EVALUATE(lcIndExp) = lcCurrKey
SCATTER MEMVAR
SELECT (loParentForm.lcOprDet1)
lcField = IIF(m.TranCd='1','Iss',IIF(m.TranCd='2','Rcv',IIF(m.TranCd='3','Dmg','Can')))
REPLACE (lcField) WITH EVAL(lcField) + m.nLotTotQty
IF loParentForm.cbPerSize
FOR lnCount = 1 TO 8
REPLACE (lcField+STR(lnCount,1)) WITH EVALUATE(lcField+STR(lnCount,1)) + EVALUATE('m.nLotQty'+STR(lnCount,1))
ENDFOR
ENDIF
ENDSCAN
ENDDO
GO TOP IN (loParentForm.lcOprDet1)
ENDIF

*!*************************************************************
*! Name      : lfvAssRolls
*! Developer : AHMED MAHER (AHM)
*! Date      : 08/25/2004
*! Purpose   : Assign rolls
*!*************************************************************
*! Calls     : lfvMatRolls
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvAssRolls()
*!*************************************************************
FUNCTION lfvAssRolls
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
PARAMETERS loFormSet
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

*E301077,55 Inhance openning files to speed up transaction


*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
*=gfOpenFile(gcDataDir+'ROLLS',gcDataDir+'Rolapl','SH')

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=gfOpenFile(oariaapplication.datadir +'ROLLS',oariaapplication.datadir +'Rolapl','SH')
=gfOpenTable(oariaapplication.datadir +'ROLLS',oariaapplication.datadir +'Rolapl','SH')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

lcInvType ='0002'
lenclrlen  = LEN(gfitemmask("PN", "", lcInvType))
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

*E301077,55 (End)

SELECT BOMCOST

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
*SET ORDER TO TAG Bomcstkt
*IF SEEK(m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode)
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*SET ORDER TO TAG Rolapl IN ROLLS
SELECT Rolls
=gfSetOrder('Rolapl')
=gfSeek('')
SELECT BOMCOST
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
*SET RELATION TO crsession+cisession+SUBSTR(Item,1,7)+IClr+cwarecode+cDyelot+;
'***** N/A *****' INTO ROLLS
*lcticket   = m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	  SET RELATION TO crsession+cisession+SUBSTR(Item,1,7)+Right(ITEM,lenclrlen  )+cwarecode+cDyelot+;
*!*	                   '***** N/A *****' INTO ROLLS
SET RELATION TO crsession+cisession+Item+cwarecode+cDyelot+;
'***** N/A *****' INTO ROLLS
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

lcticket   = m.Typ+loParentForm.lctrantype+EVALUATE(loParentForm.lcPosHdr+'.PO')+m.Item+m.MfgCode
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*lcBrFields = "cWareCode:H='Warehouse',dTranDate:H='Date',nTotQty:H='Quantity',nUnitCst:H='Cost',nTotCst:H='Amount',cApInvNo:H='Invoice' :7,"
*lcBrFields =lcBrFields+ "nAss=ROLLS.NQTYBAL :H='Unassigned' :10"
lcBrFields = "cWareCode:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_WAREHOUS,loformset.GetHeaderText("LANG_MFCSSH_WAREHOUS",loformset.HeaderAlias))+"',dTranDate:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DATE,loformset.GetHeaderText("LANG_MFCSSH_DATE",loformset.HeaderAlias))+"',nTotQty:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_QUNTITY,loformset.GetHeaderText("LANG_MFCSSH_QUNTITY",loformset.HeaderAlias))+"',nUnitCst:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_COST,loformset.GetHeaderText("LANG_MFCSSH_COST",loformset.HeaderAlias))+"',nTotCst:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_AMOUNT,loformset.GetHeaderText("LANG_MFCSSH_AMOUNT",loformset.HeaderAlias))+"',cApInvNo:H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_INVOICE,loformset.GetHeaderText("LANG_MFCSSH_INVOICE",loformset.HeaderAlias))+"' :7,"
lcBrFields =lcBrFields+ "nAss=ROLLS.NQTYBAL :H='"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_UNASSIGNED,loformset.GetHeaderText("LANG_MFCSSH_UNASSIGNED",loformset.HeaderAlias))+"' :10"
*N000682,1 MMT 12/09/2012 Globalization changes[END]
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
*IF ARIABROW([lcticket],'Issued Lots',gnBrHSRow1, gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','CUTTKT','laBrowArr')
*    =lfvMatRolls('I',BomCost.cRSession,BomCost.cISession,SUBSTR(BomCost.Item,1,7),;
BomCost.IClr,BomCost.cWareCode,BomCost.cDyelot,BomCost.nTotQty)
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*IF ARIABROW([],'Issued Lots',gnBrHSRow1, gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','cTktNo','laBrowArr')
IF ARIABROW([],IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUEDLOT,loformset.GetHeaderText("LANG_MFCSSH_ISSUEDLOT",loformset.HeaderAlias)) ,;
gnBrHSRow1, gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','cTktNo','laBrowArr')
*N000682,1 MMT 12/09/2012 Globalization changes[END]
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=lfvMatRolls('I',BomCost.cRSession,BomCost.cISession,SUBSTR(BomCost.Item,1,7),;
RIGHT(BomCost.Item,lenclrlen),BomCost.cWareCode,BomCost.cDyelot,BomCost.nTotQty)
=lfvMatRolls('I',BomCost.cRSession,BomCost.cISession,SUBSTR(BomCost.Item,1,7),;
RIGHT(BomCost.Item,lenclrlen),BomCost.cWareCode,BomCost.cDyelot,BomCost.nTotQty,0,BomCost.Item)
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [END]

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]
ENDIF
SELECT BOMCOST
SET RELATION OFF INTO ROLLS

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
*ENDIF
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]
SELECT MATINVJL

*!*************************************************************
*! Name      : lfvMatRolls
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Issue/Return rolls
*!*************************************************************
*! Calls     : lfRetAll,MFMAROL.SPX
*!*************************************************************
*! Parameters: lcAction   : 'I' Issue   'R' Return
*!             lcRSession : Receive Session
*!             lcISession : Issue Session
*!             lcFabric   : Fabric
*!             lcColor    : Color
*!             lcWareCode : Warehouse
*!             lcDyelot   : Dyelot
*!             lnIssue    : Issue/Return quantity
*!             llRetAll   : .T.  Return all issued quantity
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvMatRolls()
*!*************************************************************
FUNCTION lfvMatRolls

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*PARAMETERS lcAction,lcRSession,lcISession,lcFabric,lcColor,lcWareCode,;
lcDyelot,lnIssue,lnReturn
PARAMETERS lcAction,lcRSession,lcISession,lcFabric,lcColor,lcWareCode,;
lcDyelot,lnIssue,lnReturn,lcStyle
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

PRIVATE lnBalance,lnAssigned,lnQty,lcRollId,llProceed,lcRRollId

*E301077,55 Inhance openning files to speed up transaction
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
*=gfOpenFile(gcDataDir+'ROLLS',gcDataDir+'Rolapl','SH')

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=gfOpenFile(oariaapplication.datadir +'ROLLS',oariaapplication.datadir +'Rolapl','SH')
=gfOpenTable(oariaapplication.datadir +'ROLLS',oariaapplication.datadir +'Rolapl','SH')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

*E301077,55 (End)
STORE 0 TO lnBalance,lnAssigned,lnQty
STORE '' TO lcRollID,lcRRollID
llProceed = .T.

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
*USE (gcDataDir+'ROLLS') AGAIN ALIAS ROLLS2 ORDER TAG Rollitem IN 0

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=gfOpenFile(oariaapplication.datadir +'ROLLS',oariaapplication.datadir +'Rollitem','SH','ROLLS2')
=gfOpenTable(oariaapplication.datadir +'ROLLS',oariaapplication.datadir +'Rollitem','SH','ROLLS2')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]


SELECT ROLLS
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*SET ORDER TO TAG SESSION
=gfSetOrder('SESSION')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	=SEEK(lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot)
*!*	LOCATE REST WHILE IIF(EMPTY(cisession),crsession,cisession)+cRollItem+Color+cWareCode+Dyelot+;
*!*	       cRollID=lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot ;
*!*	FOR CROLLID <> '***** N/A *****'
=gfSEEK(lcIsession+lcStyle+lcWareCode+lcDyelot)
LOCATE REST WHILE IIF(EMPTY(cisession),crsession,cisession)+Style+cWareCode+Dyelot+;
cRollID=lcIsession+lcStyle+lcWareCode+lcDyelot ;
FOR CROLLID <> '***** N/A *****'
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

IF !FOUND()
IF lcAction = 'R'

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*IF SEEK(lcISession+lcFabric+lcColor+lcWareCode+lcDyelot+'***** N/A *****')
IF gfSEEK(lcISession+lcStyle+lcWareCode+lcDyelot+'***** N/A *****')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [END]

IF lnIssue=lnReturn
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*Delete
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[Start]
lcCurrRSession = ROLLS.CRSESSION
lnCurrBal = ROLLS.nQTyBal
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[End]
gfDELETE ()
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[Start]
IF gfSEEK(lcCurrRSession +Space(6)+lcStyle+lcWareCode+lcDyelot+'***** N/A *****','ROLLS','ROLAPL')
IF ABS(ROLLS.nQTyBal) =  lnCurrBal
gfDelete()
ELSE
*B609373,1 MMT 08/05/2010 Fix bug of Material Roll qty incorrect on Style PO Material Returns[Start]
* =gfReplace("nQTyBal with nQTyBal - lnCurrBal")
=gfReplace("nQTyBal with nQTyBal  + lnReturn")
*B609373,1 MMT 08/05/2010 Fix bug of Material Roll qty incorrect on Style PO Material Returns[End]
ENDIF
ENDIF
=gfTableUpdate()
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[End]
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
ELSE
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*REPLACE nQtyBal WITH nQtyBal - lnIssue ,;
nQty    WITH nQty    - lnIssue
*B609373,1 MMT 08/05/2010 Fix bug of Material Roll qty incorrect on Style PO Material Returns[Start]
*!*	        gfREPLACE("nQtyBal WITH nQtyBal - lnIssue ,"+;
*!*	                "nQty    WITH nQty    - lnIssue")
gfREPLACE("nQtyBal WITH nQtyBal - lnReturn,"+;
"nQty    WITH nQty    - lnReturn")
*B609373,1 MMT 08/05/2010 Fix bug of Material Roll qty incorrect on Style PO Material Returns[End]
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[Start]
lcCurrRSession = ROLLS.CRSESSION
IF gfSEEK(lcCurrRSession +Space(6)+lcStyle+lcWareCode+lcDyelot+'***** N/A *****','ROLLS','ROLAPL')
*B609373,1 MMT 08/05/2010 Fix bug of Material Roll qty incorrect on Style PO Material Returns[Start]
*=gfREPLACE("nQtyBal WITH nQtyBal + lnIssue ")
=gfREPLACE("nQtyBal WITH nQtyBal + lnReturn")
*B609373,1 MMT 08/05/2010 Fix bug of Material Roll qty incorrect on Style PO Material Returns[End]
ENDIF
=gfTableUpdate()
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[End]
ENDIF
ENDIF
llProceed = .F.
ENDIF
ELSE



*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=SEEK(lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot)
*!*	  SUM REST NQTY TO lnAssigned ;
*!*	  WHILE IIF(EMPTY(cisession),crsession,cisession)+cRollItem+Color+cWareCode+Dyelot+;
*!*	        cRollID=lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot ;
*!*	  FOR   CROLLID <> '***** N/A *****'
*!*	  =SEEK(lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot)

=gfSEEK(lcIsession+lcStyle+lcWareCode+lcDyelot)
SUM REST NQTY TO lnAssigned ;
WHILE IIF(EMPTY(cisession),crsession,cisession)+Style+cWareCode+Dyelot+;
cRollID=lcIsession+lcStyle+lcWareCode+lcDyelot ;
FOR   CROLLID <> '***** N/A *****'
=gfSEEK(lcIsession+lcStyle+lcWareCode+lcDyelot)
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

IF lcAction = 'R'
IF lnIssue=lnReturn

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=lfRetAll(lcIsession,lcFabric,lcColor,lcWareCode,lcDyelot)
=lfRetAll(lcIsession,lcStyle,lcWareCode,lcDyelot)
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

llProceed = .F.
ELSE
lnIssue=lnIssue-lnReturn
ENDIF
ENDIF
ENDIF
IF llProceed
lcRemStat = IIF(lnAssigned > 0, 'ENABLE', 'DISABLE')
lnBalance = lnIssue - lnAssigned

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [Start]
*DO (gcScrDir+"MFMAROL.SPX") WITH (lcAction='I')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFMAROL.SCX") WITH (lcAction='I')
=gfCallForm('MFMAROL',.F.,"(lcAction='I')")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

ENDIF
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*USE IN ROLLS2
=gfCloseTable('ROLLS2')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

*!*************************************************************
*! Name      : lfBrowRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Browse Issued/Returned rolls
*!*************************************************************
*! Calls     : lfwRollBrs
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowRoll()
*!*************************************************************
FUNCTION lfBrowRoll
SELECT ROLLS2
SET ORDER TO TAG RollItem
BROWSE FIELDS cMarker =IIF(RECNO()=lnDMarker,'>',' '):H='':W=.F.,;
crollid:H='Roll ID' :R,;
NQTYBAL:H='Balance' :R ;
WINDOW MFMAROL0   ;
IN WINDOW MFMAROL ;
NOMENU            ;
NOAPPEND          ;
NODELETE          ;
NOWAIT            ;
SAVE              ;
NOCLEAR           ;
KEY lcFabric+lcColor+lcWareCode+lcDyelot  ;
WHEN lfwRollBrs() ;
VALID :F lfvRollBrs() ;
TITLE lcRollTit   ;
FOR TranCd = '1' .AND. nQtyBal>0 .AND. ;
IIF(INLIST(laSetups[11,2],'L','F','I'),cRSession=lcRSession,.T.)
SELECT ROLLS
SET ORDER TO TAG SESSION
BROWSE FIELDS cMarker =IIF(RECNO()=lnRMarker,'>',' '):H='':W=.F.,;
crollid:H='Roll ID':R,;
NQTYBAL:H='Assigned':R ;
WINDOW MFMAROL1   ;
IN WINDOW MFMAROL ;
NOMENU            ;
NOAPPEND          ;
NODELETE          ;
NOWAIT            ;
SAVE              ;
NOCLEAR           ;
KEY lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot ;
WHEN lfwAsRoll()  ;
VALID :F lfvRollBrs()  ;
TITLE lcAsRollTit ;
FOR TranCd='2' .AND. NQTY>=0 .AND. IIF(llIssue,nQtyBal>0,nQtyBal>=0) .AND. ;
cTktNo=laData[1] .AND. CROLLID <> '***** N/A *****'

*!*************************************************************
*! Name      : lfwRollBrs
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Show Available rolls
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwRollBrs()
*!*************************************************************
FUNCTION lfwRollBrs

lndMarker = RECNO('ROLLS2')
SHOW WINDOW (lcRollTit) REFRESH SAME
lcRollID = ROLLS2.crollid
SHOW GET lcRollID

*!*************************************************************
*! Name      : lfwAsRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Show Assigned rolls
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwAsRoll()
*!*************************************************************
FUNCTION lfwAsRoll
lnRMarker = RECNO('ROLLS')
SHOW WINDOW (lcAsRollTit) REFRESH SAME
lnQty     = ROLLS.nQtyBal
lcRRollID = ROLLS.crollid
IF lnAssigned = 0
lcRRollID = ''
SHOW GET lcRRollID DISABLE
SHOW GET lnQty DISABLE
SHOW GET pbRemRol DISABLE
ELSE
SHOW GET lcRRollID ENABLE
SHOW GET lnQty ENABLE
SHOW GET pbRemRol ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvRollBrs
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Trab Enter key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRollBrs()
*!*************************************************************
FUNCTION lfvRollBrs
IF WONTOP() <> lcRollTit .AND. WONTOP() <> lcAsRollTit
ON KEY LABEL ENTER
ENDIF

*!*************************************************************
*! Name      : lfAssRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Assign Rolls
*!*************************************************************
*! Calls     : gfModalGen,lfRefresh,lfwRollBrs,lfwAsRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfAssRoll()
*!*************************************************************
FUNCTION lfAssRoll
IF lnBalance <= 0
*E300725,1 Message : 38079
*E300725,1 cannot exceed the total issued quantity for this fabric/Color
*E300725,1 Button : 00000
*E300725,1 Ok
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*=gfModalGen('TRM38079B00000','ALERT','issued')
=gfModalGen('TRM38079B00000','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_ISSUED,loParentForm.GetHeaderText("LANG_MFCSSH_ISSUED",loParentForm.HeaderAlias)))
*N000682,1 MMT 12/09/2012 Globalization changes[End]
RETURN
ENDIF
SELECT ROLLS2
*B606782,1 KHM 12/31/2002 (Begin) Getting the roll balance quantity if the issued is greater
*B606782,1                than the roll's quantity.
lnBalance = MIN(lnBalance,Nqtybal)
*B606782,1 KHM 12/31/2002

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*REPLACE Nqtybal WITH Nqtybal-MIN(lnBalance,nQty)
gfREPLACE("Nqtybal WITH Nqtybal-MIN(lnBalance,nQty)")
=gfTableUpdate()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
SELECT ROLLS

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*IF !SEEK(lcISession+ROLLS2.cRollItem+ROLLS2.Color+ROLLS2.cWareCode+ROLLS2.Dyelot+ROLLS2.cRollid)
IF !gfSEEK(lcISession+ROLLS2.Style+ROLLS2.cWareCode+ROLLS2.Dyelot+ROLLS2.cRollid)
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

APPEND BLANK
REPLACE CROLLID    WITH ROLLS2.cRollid ,;
cRollItem  WITH ROLLS2.cRollItem ,;
Color      WITH ROLLS2.Color   ,;
CwareCode  WITH ROLLS2.cWareCode ,;
Dyelot     WITH ROLLS2.Dyelot  ,;
Trancd     WITH '2',;
Ctktno     WITH EVALUATE(loparentform.lcPosHdr+'.PO')  ,;
Csession   WITH lcIsession ,;
Crsession  WITH ROLLS2.cRsession ,;
Cisession  WITH lcIsession

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
REPLACE Style WITH ROLLS2.Style
=gfReplace("")
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
ENDIF

REPLACE NQTY    WITH NQTY    + MIN(ROLLS2.nQty,lnBalance) ,;
Nqtybal WITH Nqtybal + MIN(ROLLS2.nQty,lnBalance)

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
=gfReplace("")
=gfTableUpdate()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

lnAssigned = lnAssigned + MIN(ROLLS2.nQty,lnBalance)
lnBalance = lnIssue - lnAssigned

*!*************************************************************
*! Name      : lfRetRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Return Rolls
*!*************************************************************
*! Calls     : gfModalGen,lfRefresh,lfwRollBrs,lfwAsRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRetRoll()
*!*************************************************************
FUNCTION lfRetRoll


IF lcAction = 'I'
IF NQTY-NQTYBAL <> 0
*E300725,1 Message : 38083
*E300725,1 This roll has been returned before you cannot remove it
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38083B00000','ALERT')
RETURN
ENDIF
SELECT ROLLS2

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+ROLLS.cRollid+"1"+ROLLS.cRSession)
*REPLACE Nqtybal WITH Nqtybal+ROLLS.nQty
=GfSEEK(lcStyle+lcWareCode+lcDyelot+ROLLS.cRollid+"1"+ROLLS.cRSession)
gfREPLACE("Nqtybal WITH Nqtybal+ROLLS.nQty")
=gfTableUpdate()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
SELECT ROLLS
lnAssigned = lnAssigned - nQty

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*DELETE
=gfDelete()
=gfTableUpdate()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

ELSE
IF lnReturn = 0
*E300725,1 Message : 38079
*E300725,1 You cannot exceed the total returned quantity for this fabric/Color.
*E300725,1 Button : 00000
*E300725,1 Ok
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*=gfModalGen('TRM38079B00000','ALERT','returned')
=gfModalGen('TRM38079B00000','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_RETURNED,loParentForm.GetHeaderText("LANG_MFCSSH_RETURNED",loParentForm.HeaderAlias)))
*N000682,1 MMT 12/09/2012 Globalization changes[End]
RETURN
ENDIF
SELECT ROLLS2

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+ROLLS.cRollid+"1"+ROLLS.cRSession)
*REPLACE Nqtybal WITH Nqtybal+MIN(lnReturn,lnQty)
=gfSEEK(lcStyle+lcWareCode+lcDyelot+ROLLS.cRollid+"1"+ROLLS.cRSession)

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*  gfREPLACE("Nqtybal WITH Nqtybal+MIN(lnReturn,lnQty)")
gfREPLACE("Nqtybal WITH Nqtybal+ROLLS.nQty")
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

=gfTableUpdate()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

SELECT ROLLS

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	  *REPLACE nqtybal WITH nqtybal - MIN(lnReturn,lnQty)
*!*	  gfREPLACE("nqtybal WITH nqtybal - MIN(lnReturn,lnQty)")
*!*		  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
*!*	  lnAssigned = lnAssigned - MIN(lnReturn,lnQty)
*!*	  SCATTER MEMVAR
*!*	  m.nQty    = MIN(lnReturn,lnQty) * -1
*!*	  m.nQtyBal = 0
*!*	  INSERT INTO ROLLS2 FROM MEMVAR
*!*	  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	  =gfReplace("")
*!*	  =gfTableUpdate()
*!*	  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
*!*	lnReturn = lnReturn - MIN(lnReturn,lnQty)
SELECT ROLLS
lnAssigned = lnAssigned - nQty
lnReturn = lnReturn - nQty
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*DELETE
=gfDelete()
=gfTableUpdate()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

ENDIF
lnBalance = lnIssue - lnAssigned

*!*************************************************************
*! Name      : lfwRollID
*! Developer : AHMED MAHER
*! Date      : 08/25/2004
*! Purpose   : Store Roll Id before modification
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwRollID()
*!*************************************************************
FUNCTION lfwRollID
lcOldValue = lcRollId

*!*************************************************************
*! Name      : lfvRollID
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Validate Issued roll ID
*!*************************************************************
*! Calls     : lfAssRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRollID()
*!*************************************************************
FUNCTION lfvRollID
PARAMETERS llMove


PRIVATE llContinue

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*IF (llMove .OR. LASTKEY() = 13) .AND. ;
SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+lcRollId+'1'+;
IIF(INLIST(loparentform.laSetups[11,2],'L','F','I'),lcRSession,''),'ROLLS2')
IF (llMove .OR. LASTKEY() = 13) .AND. ;
gfSEEK(lcStyle+lcWareCode+lcDyelot+lcRollId+'1'+;
IIF(INLIST(loparentform.laSetups[11,2],'L','F','I'),lcRSession,''),'ROLLS2')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

=lfAssRoll()
SELECT ROLLS2

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*LOCATE REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRSession=;
lcFabric+lcColor+lcWareCode+lcDyelot ;
FOR TranCd = '1' .AND. nQtyBal>0 .AND. ;
IIF(INLIST(loparentform.laSetups[11,2],'L','F','I'),cRSession=lcRSession,.T.)
LOCATE REST WHILE Style+cWareCode+Dyelot+cRollId+TranCd+cRSession=;
lcStyle+lcWareCode+lcDyelot ;
FOR TranCd = '1' .AND. nQtyBal>0 .AND. ;
IIF(INLIST(loparentform.laSetups[11,2],'L','F','I'),cRSession=lcRSession,.T.)
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

lcRollId = ROLLS2.cRollId
ENDIF

*!*************************************************************
*! Name      : lfvRRollID
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Validate Returned roll ID
*!*************************************************************
*! Calls     : lfRetRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRRollID()
*!*************************************************************
FUNCTION lfvRRollID
PARAMETERS llMove

PRIVATE llContinue

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*IF (llMove .OR. LASTKEY() = 13) .AND. ;
SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+lcRRollId+'2'+;
IIF(INLIST(loparentform.laSetups[11,2],'L','F','I'),lcRSession,''),'ROLLS2')
IF (llMove .OR. LASTKEY() = 13) .AND. ;
gfSEEK(lcStyle+lcWareCode+lcDyelot+lcRRollId+'2'+;
IIF(INLIST(loparentform.laSetups[11,2],'L','F','I'),lcRSession,''),'ROLLS2')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

=lfRetRoll()
lcRRollId = ROLLS.cRollId
ENDIF

*!*************************************************************
*! Name      : lfRetAll
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Return all roll issued quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRetAll()
*!*************************************************************
FUNCTION lfRetAll

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*PARAMETERS lcIsession,lcFabric,lcColor,lcWareCode,lcDyelot
PARAMETERS lcIsession,lcStyle,lcWareCode,lcDyelot
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	SET ORDER TO TAG ROLLITEM IN ROLLS2
*!*	SET ORDER TO TAG SESSION  IN ROLLS
SELECT ROLLS2
=gfSetOrder('ROLLITEM')
SELECT ROLLS
=gfSetOrder('SESSION')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

SELECT ROLLS

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	=SEEK(lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot)
*!*	SCAN REST WHILE csession+cRollItem+Color+cWareCode+Dyelot+cRollID=;
*!*	                lcIsession+lcFabric+lcColor+lcWareCode+lcDyelot ;
*!*	          FOR   CROLLID <> '***** N/A *****' .AND. nqtybal <> 0
=gfSEEK(lcIsession+lcStyle+lcWareCode+lcDyelot)
SCAN REST WHILE csession+Style+cWareCode+Dyelot+cRollID=;
lcIsession+lcStyle+lcWareCode+lcDyelot ;
FOR   CROLLID <> '***** N/A *****' .AND. nqtybal <> 0
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	  SCATTER MEMVAR
*!*	  lcRollid  = crollid
*!*	  lnRetqty  = m.nqtybal
*!*	  m.NQty    = lnRetqty*-1
*!*	  m.nqtybal = 0
*!*	  SELECT ROLLS2
*!*	  APPEND BLANK
*!*	  GATHER MEMVAR
*!*
*!*	  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
*!*	  *IF SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+lcRollid+"1"+m.cRSession)
*!*	  *REPLACE Nqtybal WITH Nqtybal+lnRetqty
*!*	  =gfReplace("")
*  IF gfSEEK(lcStyle+lcWareCode+lcDyelot+lcRollid+"1"+m.cRSession)
lcRollid  = crollid
lnRetqty  = nqtybal
m.cRSession = cRSession
=gfDelete()
SELECT ROLLS2
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
IF gfSEEK(lcStyle+lcWareCode+lcDyelot+lcRollid+"1"+m.cRSession)
gfREPLACE("Nqtybal WITH Nqtybal+lnRetqty")
=gfTableUpdate()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
ENDIF
SELECT ROLLS
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
*REPLACE nqtybal WITH 0

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*gfREPLACE("nqtybal WITH 0")
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [ENd]

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
ENDSCAN
SELECT ROLLS

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
*IF SEEK(lcISession+lcFabric+lcColor+lcWareCode+lcDyelot+'***** N/A *****')
*DELETE
IF gfSEEK(lcISession+lcStyle+lcWareCode+lcDyelot+'***** N/A *****')
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[Start]
lcCurrRSession = ROLLS.CRSESSION
lnQtyBal = ROLLS.nQtyBal
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[End]
gfDELETE()
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[Start]
IF !EMPTY(lcCurrRSession) AND gfSEEK(lcCurrRSession +Space(6)+lcStyle+lcWareCode+lcDyelot+'***** N/A *****','ROLLS','ROLAPL')
IF ABS(ROLLS.nQtyBal)=lnQtyBal
gfDelete()
ELSE
=gfReplace("nQtyBal With nQtyBal-lnQtyBal")
ENDIF
ENDIF
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[End]
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
ENDIF
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
=gfTableUpdate()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
*!*************************************************************
*! Name      : lfvQty
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Validate Roll Issued quantity
*!*************************************************************
*! Calls     : lfRefresh,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvQty()
*!*************************************************************
FUNCTION lfvQty
PRIVATE	lnRecNo

*! N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
SELECT ROLLS
*! N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

IF lnQty = ROLLS.Nqtybal
RETURN
ENDIF
lnRecNo = RECNO('ROLLS2')

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [START]
*=SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+ROLLS.cRollid+"1"+ROLLS.cRSession,'ROLLS2')
=gfSEEK(lcsTYLE+lcWareCode+lcDyelot+ROLLS.cRollid+"1"+ROLLS.cRSession,'ROLLS2')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [END]

*E300725,1 Message : 38075
*E300725,1 Roll xxx is completely issued
*E300725,1 Button : 00000
*E300725,1 Ok

*E300725,1 Message : 38078
*E300725,1 Issued quantity cannot be less than zero
*E300725,1 Button : 00000
*E300725,1 Ok

*E300725,1 Message : 38079
*E300725,1 cannot exceed the total issued quantity  for this fabric/Color
*E300725,1 Button : 00000
*E300725,1 Ok

*E300725,1 Message : 38080
*E300725,1 cannot exceed the roll balance
*E300725,1 Button : 00000
*E300725,1 Ok

*E300725,1 Message : 38081
*E300725,1 The assigned quantity cannot be less than the returned quantity
*E300725,1 Button : 00000
*E300725,1 Ok
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*IF ( lnQty < 0 .AND. ;
gfModalGen('TRM38078B00000','ALERT','Issued'+'|'+'be less than zero')=1 ) .OR. ;
( lnAssigned - Nqtybal + lnQty > lnIssue .AND. ;
gfModalGen('TRM38079B00000','ALERT','issued')=1) .OR. ;
( lnQty > ROLLS2.Nqtybal+Nqtybal .AND. gfModalGen('TRM38080B00000','ALERT')=1 ) .OR. ;
( lnQty < nQty - nQtyBal .AND. gfModalGen('TRM38081B00000','ALERT')=1 )
IF ( lnQty < 0 .AND. ;
gfModalGen('TRM38078B00000','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUED,loParentForm.GetHeaderText("LANG_MFCSSH_ISSUED",loParentForm.HeaderAlias))+;
'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_BELESSTHANZERO,loParentForm.GetHeaderText("LANG_MFCSSH_BELESSTHANZERO",loParentForm.HeaderAlias)))=1 ) .OR. ;
( lnAssigned - Nqtybal + lnQty > lnIssue .AND. ;
gfModalGen('TRM38079B00000','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUED,loParentForm.GetHeaderText("LANG_MFCSSH_ISSUED",loParentForm.HeaderAlias)))=1) .OR. ;
( lnQty > ROLLS2.Nqtybal+Nqtybal .AND. gfModalGen('TRM38080B00000','ALERT')=1 ) .OR. ;
( lnQty < nQty - nQtyBal .AND. gfModalGen('TRM38081B00000','ALERT')=1 )
*N000682,1 MMT 12/09/2012 Globalization changes[End]
lnQty = Nqtybal
ENDIF
lnAssigned = lnAssigned - Nqtybal + lnQty
lnBalance  = lnIssue - lnAssigned
SELECT ROLLS2
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [START]
*REPLACE Nqtybal WITH Nqtybal + ROLLS.Nqtybal - lnQty
gfREPLACE("Nqtybal WITH Nqtybal + ROLLS.Nqtybal - lnQty")
=gfTableUpdate()
IF BETWEEN(lnRecNo,1,RECCOUNT())
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

GO lnRecNo

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
ENDIF
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

SELECT ROLLS

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [START]
gfREPLACE("nQty    WITH nQty - nQtyBal + lnQty ,"+;
"nQtyBal WITH lnQty")
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
IF nQty = 0

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [START]
*DELETE
=gfDelete()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

ENDIF
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [START]
=gfTableUpdate()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
*!*************************************************************
*! Name      : lfvRolOk
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Validate Roll screen
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvRolOk()
*!*************************************************************
FUNCTION lfvRolOk
SELECT ROLLS

IF lcAction = 'I'

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	  IF !SEEK(lcISession+lcFabric+lcColor+lcWareCode+lcDyelot+'***** N/A *****')
*!*	    INSERT INTO ROLLS ;
*!*	    (cRollItem,Color,cWareCode,Dyelot,cRollId,nQty,Trancd,cTktNo,cSession,cRSession,cIsession) ;
*!*	    VALUES (lcFabric,lcColor,lcWareCode,lcDyelot,'***** N/A *****',;
*!*	            lnIssue,'2',EVALUATE(loParentForm.lcPosHdr+'.PO'),lcISession,lcRSession,lcISession)
IF !gfSEEK(lcISession+lcStyle+lcWareCode+lcDyelot+'***** N/A *****')
INSERT INTO ROLLS ;
(cRollItem,Color,cWareCode,Dyelot,cRollId,nQty,Trancd,cTktNo,cSession,cRSession,cIsession,Style) ;
VALUES (lcFabric,lcColor,lcWareCode,lcDyelot,'***** N/A *****',;
lnIssue,'2',EVALUATE(loParentForm.lcPosHdr+'.PO'),lcISession,lcRSession,lcISession,lcStyle)
=gfReplace("")
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

ENDIF
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*  REPLACE Nqtybal WITH lnBalance
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[Start]
lnQtyUpd = Nqtybal - lnBalance
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[End]
gfREPLACE("Nqtybal WITH lnBalance")
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[Start]
IF nQtyBal = 0
=gfDelete()
ENDIF
IF !gfSEEK(lcRSession+Space(6)+lcStyle+lcWareCode+lcDyelot+'***** N/A *****','ROLLS','ROLAPL')
INSERT INTO ROLLS ;
(cRollItem,Color,cWareCode,Dyelot,cRollId,nQty,Trancd,cTktNo,cSession,cRSession,cIsession,Style) ;
VALUES (lcFabric,lcColor,lcWareCode,lcDyelot,'***** N/A *****',;
0,'1',EVALUATE(loParentForm.lcPosHdr+'.PO'),lcRSession,lcRSession,'',lcStyle)
=gfAdd_Info('ROLLS')
=gfReplace("")
ENDIF
*=gfREPLACE("Nqtybal WITH -1*lnBalance")
=gfREPLACE("Nqtybal WITH Nqtybal +lnQtyUpd")
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[End]
ELSE

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*  =SEEK(lcISession+lcFabric+lcColor+lcWareCode+lcDyelot+'***** N/A *****')
=gfSEEK(lcISession+lcStyle+lcWareCode+lcDyelot+'***** N/A *****')
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

IF lnReturn > 0
IF nQtyBal >= lnReturn
*E300725,1 Message : 38085
*E300725,1 The returned quantity is not completey assigned to rolls
*E300725,1 Would you like to deduct the unassigned returned quantity
*E300725,1 from the unassigned issued quantity?
*E300725,1 Button : 38006
*E300725,1 Yes  No
IF gfModalGen('QRM38085B38006','ALERT') = 1
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	        REPLACE nQtyBal WITH nQtyBal - lnReturn ,;
*!*	                nQty    WITH nQty    - lnReturn
gfREPLACE("nQtyBal WITH nQtyBal - lnReturn ,"+;
"nQty    WITH nQty    - lnReturn")
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[Start]
IF nQtyBal = 0
=gfDelete()
ENDIF
IF gfSEEK(lcRSession+Space(6)+lcStyle+lcWareCode+lcDyelot+'***** N/A *****','ROLLS','ROLAPL')
gfREPLACE("nQtyBal WITH nQtyBal + lnReturn ")
IF nQtyBal = 0
=gfDelete()
ENDIF
ENDIF
SELECT ROLLS
=gfTableUpdate()
RETURN
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[eND]
ELSE
SELECT ROLLS
RETURN
ENDIF
ELSE
*E300725,1 Message : 38084
*E300725,1 The returned quantity is not completey assigned to rolls
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38084B00000','ALERT')
SELECT ROLLS
RETURN
ENDIF
ENDIF
ENDIF
IF nQtyBal = 0
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*DELETE
=gfDelete()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
ENDIF
SELECT ROLLS
=gfTableUpdate()
*!*************************************************************
*! Name      : lfvCheckOrp
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/07/2004
*! Purpose   : Check that Item/Color has been assigned to receive to operation
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: lcToOpr   : Receive to Operation
*!             lcInvType : Inventory type of Received Item
*!             lcItem    : Received Item
*!*************************************************************
*! Returns   :  .F. When Received Item/Clor has not been assign to receive
*!                  to operation.
*!                  operation for this item
*!*************************************************************
*! Example   :  =lfvCheckOrp('000002','0001','MDS123')
*!*************************************************************
FUNCTION lfvCheckOrp
LPARAMETERS lcToOpr,lcInvType,lcItem

LOCAL lnAlias
lnAlias = SELECT(0)
SELECT (loParentForm.lcDetFile)
=SEEK(lcInvType+lcItem)
LOCATE REST WHILE cInvType+Style+STR(LineNo,6)+cBomTyp+cShowType+cInvTypC+Item+MfgCode=lcInvType+lcItem;
FOR cShowType+cInvTypC+Item+MfgCode = '1'+SPACE(4)+SPACE(19)+lcToOpr
SELECT (lnAlias)
IF !FOUND(loParentForm.lcDetFile)
*E300725,1 Message : 38141
*E300725,1 Item xxxxx has not been assigned to operation xxxx.
*E300725,1 Button : 00000
*E300725,1 Ok
=gfModalGen('TRM38141B00000','ALERT',ALLTRIM(lcItem)+'|'+ALLTRIM(gfCodDes(lcToOpr,'MfgCode')))
RETURN(.F.)
ENDIF

*!*************************************************************
*! Name      : lfGetSummary
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Get Bar number in the system menu
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcLotNo : Bar Code
*!*************************************************************
*! Returns   :  Bar Number
*!*************************************************************
*! Example   :  =lfGetSummary(lcLotNo)
*!*************************************************************
FUNCTION lfGetSummary
PARAMETERS lcLotNo
PRIVATE lnAlias

lnAlias = SELECT()

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*	SELECT (lcOprDet)
*!*	lcCurrTag = ORDER(lcOprDet)
SELECT (loParentForm.lcOprDet)
lcCurrTag = ORDER(loParentForm.lcOprDet)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

*E302004,1 AMH Save the current record No. [Start]
PRIVATE lnRecNo
lnRecNo = RECNO()
*E302004,1 AMH [End]

SET ORDER TO TAG 'LOT'

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*	=SEEK(lcTranType+laData[1]+lcOprCode+ALLTRIM(lcLotNo))
*!*	SUM REST IIF(TranCd='1',nLotTotQty,0),IIF(TranCd='2',nLotTotQty,0),;
*!*	         IIF(TranCd='3',nLotTotQty,0),IIF(TranCd='4',nLotTotQty,0) TO ;
*!*	         lnLotbud,lnLotRcv,lnLotCan,lnLotDmg ;
*!*	WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
*!*	         lcTranType+laData[1]+lcOprCode+ALLTRIM(lcLotNo)
=SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+lcOprCode+ALLTRIM(lcLotNo))
SUM REST IIF(TranCd='1',nLotTotQty,0),IIF(TranCd='2',nLotTotQty,0),;
IIF(TranCd='3',nLotTotQty,0),IIF(TranCd='4',nLotTotQty,0) TO ;
lnLotbud,lnLotRcv,lnLotCan,lnLotDmg ;
WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+lcOprCode+ALLTRIM(lcLotNo)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

*E302004,1 AMH Save the current record No. [Start]
IF BETWEEN(lnRecNo,1,RECCOUNT())
GO lnRecNo
ENDIF
*E302004,1 AMH [End]

SELECT (lnAlias)

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SET ORDER TO TAG lcCurrTag IN (lcOprDet)
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

*lcStatColor=IIF(lnLotbud-lnLotRcv-lnLotCan-lnLotDmg>0,"RGB(255,255,255,255,0,0)",gcObjColor)

*!*************************************************************
*! Name      : lfAddNewItm
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/05/2004
*! Purpose   : Update master style and fabric files with added
*!             materials and style components.
*!*************************************************************
*! Calls From: lfvNew() , lpSavScr()
*!*************************************************************
*! Parameters:  Nome
*!*************************************************************
*! Returns   :  None
*!:************************************************************
FUNCTION lfAddNewItm
LPARAMETERS loFormSet

IF USED(loFormSet.lcTmpStyle)
SELECT (loFormSet.lcTmpStyle)
SCAN
IF !SEEK(Style,'Style')
SCATTER MEMVAR
INSERT INTO 'STYLE' FROM MEMVAR
ENDIF
IF !SEEK(Style+EVALUATE(loFormSet.lcPosHdr+'.CITEMWARE')+SPACE(10),'STYDYE')
DO gpAdStyWar WITH Style,SPACE(10),EVALUATE(loFormSet.lcPosHdr+'.CITEMWARE')
ENDIF
ENDSCAN
ZAP
ENDIF
IF USED(loFormSet.lcTmpFbric)
LOCAL llFound
SELECT (loFormSet.lcTmpFbric)
SCAN
SCATTER MEMVAR
llFound    = .F.
IF lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loFormSet)
SELECT FABRIC
LOCATE
llFound = !EOF()
ENDIF
IF !llFound
INSERT INTO 'FABRIC' FROM MEMVAR
DECLARE laTableUpdate[1,2]
laTableUpdate[1,1] = 'FABRIC'
laTableUpdate[1,2] = 'ITEM'

=lfTableUpdate(loFormSet)
ENDIF
llFound     = .F.
m.cWareCode = EVALUATE(loFormSet.lcPosHdr+'.CMATWARE')
m.Dyelot    = SPACE(10)
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loFormSet)
SELECT FABDYE
LOCATE
llFound = !EOF()
ENDIF
IF !llFound
DO GfAdItemWar WITH m.cInvType,m.Style,SPACE(10),EVALUATE(loFormSet.lcPosHdr+'.CMATWARE')
ENDIF
ENDSCAN
ZAP
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvOprSeq
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/05/2004
*! Purpose   : Validation of the sequance field in modify screen
*!*************************************************************
*! Calls From: gfModalGen()
*!*************************************************************
*! Parameters:  Nome
*!*************************************************************
*! Returns   :  None
*!:************************************************************
FUNCTION lfvOprSeq

IF loParentForm.lcTranType='M' AND EMPTY(lcOperSeq) AND TYPE('lcOperSeq') # 'N'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfModalGen('TRM00000B00000',.F.,.F.,.F.,LANG_MFCSSH_MESGSEQ)
= gfModalGen('TRM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_MESGSEQ,loFormSet.GetHeaderText("LANG_MFCSSH_MESGSEQ",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

RETURN .F.
ENDIF

*!**************************************************************************
*! Name      : lfPicture
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/05/2004
*! Purpose   : Change Picture of PO .
*!**************************************************************************
FUNCTION lfPicture

IF lcTranType='I' AND llGenOrNum
RETURN "!!!!!!"
ELSE
RETURN "!99999"
ENDIF

*!*************************************************************
*! Name      : lfDisActual             B802630,1 SSH
*! Developer : Ahmed Salah Shalaby - SSH
*! Date      : 10/04/00
*! Purpose   : Prevent actualize cutTkt/PO with only one Opr.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : = lfDisActual ()
*!*************************************************************
*B802630,1 SSH [Begin] 10/04/00 Prevent actualize cutTkt/PO with only one Opr.
FUNCTION lfDisActual
*N039501,1 KHM 12/20/2005 [Start]
LPARAMETERS loFormSet

LOCAL lnOld , lcOldOpr, lnRecNo

lnOld = SELECT(0)
SELECT (loParentForm.lcmfgoprhd)
lnRecNo = RECNO()
lcOldOpr  = coprcode
LOCATE FOR cOprCode <> lcOldOpr
IF !FOUND()
IF EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT') = 'M'
loFormSet.ariaForm1.chkActualize.Enabled = .F.
ELSE
STORE .T. TO loParentForm.llOnlyOne
ENDIF
ELSE
loFormSet.ariaForm1.chkActualize.Enabled = .T.
ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT())
GOTO lnRecNo
ENDIF
SELECT(lnOld)
*N039501,1 KHM 12/20/2005 [End]

*!*************************************************************
*! Name             : lfCheckMaj
*! Developer        : AHMED MAHER (AMH)
*! Date             : 12/22/2003
*! Purpose          : Check style code structure.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = lfCheckMaj()
*!*************************************************************
FUNCTION lfCheckMaj

LOCAL lnAlias,llStruOp
lnAlias=SELECT(0)
llStruOp=gfOpenFile(oAriaApplication.DataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
IF USED('ICISTRU') AND llStruOp
USE IN ICISTRU
ENDIF
SELECT (lnAlias)
RETURN .F.
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfPwShtItm
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/05/2004
*! Purpose   : Create detail oeration cost sheet
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfPwShtItm()
*!*************************************************************
FUNCTION lfPwShtItm
LPARAMETERS lcNewGen , llNewItm , loFormSet

*--- first parameter if we are generation new cost sheet
*--- Second one if we are calling to add new item
LOCAL lnOldAls,lcPwStyle,lcMainMfg,lcOldAsign,PWBOMTAG,lcNewMfg,lcCtkt,lcTmpLin,lcANonMjr,;
lcExNnMjr1,lcExNnMjr2,lcExNnMjr3,lcCostTyp
lnOldAls = SELECT(0)
lcOldAsign = ''
IF !llNewItm
lcPwBom = 'Dummy'
ELSE
PWBOMTAG = gfTempName()
SELECT (loFormSet.lcTmpBOM)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*INDEX ON citmmajor+typ+citmmask+mfgcode+cInvTypC+item TAG &PWBOMTAG
INDEX ON CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+MFGCODE+COPRCODE+STR(NLINENO,6) TAG &PWBOMTAG
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
SET ORDER TO (PWBOMTAG)
ENDIF

=lfPWTmpFil(loFormSet)
*--- This function will update 'PWCTKTBOM' temp file for detail oper.
*--- using the following key [cuttkt+mfgcode+coprcode+STR(nlineno,6)]
*--- The updates will be as following :
*--- Each detail operation will have its owne record in PWCTKTBOM file
*--- even if the Main Mfg Code assigned more than onece
*--- (It will has only one record in BOMLINE file with average cost)
*--- but it will has record for each detail operation.
*--- i.e. MfgCode1+Det1 maybe repeate in PWCTKTBOM but with deferant
*--- Line no. while MfgCode1 hase only one record in BOMLIN fiel.
*--- we are going to use PWBOM file to update this temp.

*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
IF TYPE('lcPWCostSheetID') <> 'C'
lnOldSelect = SELECT()
SELECT(loFormSet.lcPosln)
lnOldPOSLNRec = RECNO()
LOCATE
lcPWCostSheetID = EVALUATE(loFormSet.lcPosln+'.cCstSht_ID')
IF BETWEEN(lnOldPOSLNRec ,1,RECCOUNT())
GO RECORD lnOldPOSLNRec
ENDIF
SELECT(lnOldSelect)
ENDIF
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]

IF llNewItm
lcNewMfg = gfTempName()
SELECT (loFormSet.lcTmpBom)
SELECT DISTINCT mfgCode FROM (loFormSet.lcTmpBom) INTO DBF (oAriaApplication.WorkDir+lcNewMfg)
SELECT(lcNewMfg)
INDEX ON mfgCode TAG (lcNewMfg)
SET ORDER TO CTKTBOM IN (loFormSet.lcTktSheet)
ENDIF
IF EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H'
SELECT (loFormSet.lcBOMLINE)
ELSE
SELECT (loFormSet.lcDetFile)
ENDIF
LOCATE

*--- SCAN For Manufacturing Operation
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*!*	SCAN FOR IIF(EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H' .AND. !loFormSet.llAutoGen,cimtyp+ctype = "M1",.T.) .AND.;
*!*	         cCatgTyp = 'M' .AND. SEEK(mfgcode,'pwoperat') .AND. IIF(llNewItm,SEEK(mfgcode,lcNewMfg),;
*!*	         IIF(!loFormSet.llAutoGen .AND. EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H',(cShowType = '1'),.T.))
SCAN FOR IIF(EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H' .AND. !loFormSet.llAutoGen,cimtyp+ctype = "M1",.T.) .AND.;
cCatgTyp = 'M' .AND. gfSEEK(mfgcode,'PWOPERAT') .AND. IIF(llNewItm,SEEK(mfgcode,lcNewMfg),;
IIF(!loFormSet.llAutoGen .AND. EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H',(cShowType = '1'),.T.))
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
lcPwStyle = PADR(SUBSTR(Style,1,LEN(loFormSet.lcMjrMsk)),19)
lcCtkt    = EVALUATE(loFormSet.lcPosHdr+'.PO')
lcMainMfg = MfgCode
lcTmpLin  = ''
lcANonMjr = SUBSTR(Style,1,LEN(loFormSet.lcMjrMsk))+SUBSTR(loFormSet.lcItmMsk,LEN(loFormSet.lcMjrMsk)+1)
lcANonMjr = STRTRAN(lcANonMjr,"X","*")
lcStyClr  = Style
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
lcCostTyp = cBomTyp
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
IF loFormSet.llExtSizSc
*Case style - ****** Scl
lcExNnMjr1 = STUFF(Style, loFormSet.lnClrPos, LEN(loFormSet.lcClrMsk), REPLICATE("*",LEN(loFormSet.lcClrMsk)))

*Case Style - ****** ***
lcExNnMjr2 = STUFF(lcExNnMjr1 , loFormSet.lnSizePos, loFormSet.lnSizeLen,"***")

* Case style - Color ***
lcExNnMjr3 = STUFF(Style, loFormSet.lnSizePos, loFormSet.lnSizeLen,"***")

ENDIF

m.cInvType   = loFormSet.lcInvType
m.cItmMajor  = lcPwStyle
m.cCstShtTyp = "M"
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*m.cCstSht_ID = ""
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
IF lfOpenSql('BOM',loFormSet.lcBom,'MULTIBOM','CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID',loFormSet)
*--- citmmajor+typ+citmmask+mfgcode+item+iclr
IF llNewItm
SELECT (loFormSet.lcBom)
lcCostTyp = IIF(SEEK("M",loFormSet.lcTktSheet),EVALUATE(loFormSet.lcTktSheet+'.Typ'),'')
LOCATE FOR typ+citmmask+mfgcode = lcCostTyp+lcStyClr+lcMainMfg
IF FOUND()
SCAN FOR typ+citmmask+mfgcode = lcCostTyp+lcStyClr+lcMainMfg
lcOldAsign = lcOldAsign+ALLTRIM(STR(nLineNo))
ENDSCAN
ENDIF
LOCATE FOR typ+citmmask+mfgcode = lcCostTyp+lcANonMjr+lcMainMfg
IF FOUND()
SCAN FOR typ+citmmask+mfgcode = lcCostTyp+lcANonMjr+lcMainMfg
lcOldAsign = lcOldAsign+ALLTRIM(STR(nLineNo))
ENDSCAN
ENDIF
SELECT (loFormSet.lcTmpBOM)
ELSE
SELECT (loFormSet.lcBom)
ENDIF
*--- citmmajor+typ+citmmask+mfgcode+item+iclr
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*lcCostTyp = IIF(SEEK("M",loFormSet.lcTktSheet),EVALUATE(loFormSet.lcTktSheet+'.Typ'),'')
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
LOCATE FOR typ+citmmask+mfgcode = lcCostTyp+lcStyClr+lcMainMfg
IF FOUND()
SCAN FOR typ+citmmask+mfgcode = lcCostTyp+lcStyClr+lcMainMfg
lcTmpLin = lcTmpLin+ALLTRIM(STR(nLineNo))
ENDSCAN
ENDIF
LOCATE FOR typ+citmmask+mfgcode = lcCostTyp+lcANonMjr+lcMainMfg
IF FOUND()
SCAN FOR typ+citmmask+mfgcode = lcCostTyp+lcANonMjr+lcMainMfg
lcTmpLin = lcTmpLin+ALLTRIM(STR(nLineNo))
ENDSCAN
ENDIF

IF loFormSet.llExtSizSc

*Case style - ****** Scl
LOCATE FOR typ+citmmask+mfgcode = lcCostTyp+lcExNnMjr1+lcMainMfg
IF FOUND()
SCAN FOR typ+citmmask+mfgcode = lcCostTyp+lcExNnMjr1+lcMainMfg
lcTmpLin = lcTmpLin+ALLTRIM(STR(nLineNo))
ENDSCAN
ENDIF

*Case Style - ****** ***
LOCATE FOR typ+citmmask+mfgcode = lcCostTyp+lcExNnMjr2+lcMainMfg
IF FOUND()
SCAN FOR typ+citmmask+mfgcode = lcCostTyp+lcExNnMjr2+lcMainMfg
lcTmpLin = lcTmpLin+ALLTRIM(STR(nLineNo))
ENDSCAN
ENDIF

* Case style - Color ***
LOCATE FOR typ+citmmask+mfgcode = lcCostTyp+lcExNnMjr3+lcMainMfg
IF FOUND()
SCAN FOR typ+citmmask+mfgcode = lcCostTyp+lcExNnMjr3+lcMainMfg
lcTmpLin = lcTmpLin+ALLTRIM(STR(nLineNo))
ENDSCAN
ENDIF
ENDIF
ENDIF
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*lcTmpLin = IIF(EMPTY(lcTmpLin),'1',lcTmpLin)
lcTmpLin = IIF(EMPTY(lcTmpLin) or ISNULL(lcTmpLin),'1',lcTmpLin)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
*--- citmmajor+mfgcode+coprcode+STR(nlineno,6)
IF llNewItm
SELECT (loFormSet.lcPwBom)
SET FILTER TO EMPTY(cStatus)
ELSE
SELECT PWBOM
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
=gfSEEK(loFormSet.lcInvType+PADR(lcPwStyle,19)+m.cCstShtTyp+lcPWCostSheetID + lcMainMfg)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]

ENDIF
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*!*	  IF SEEK(lcPwStyle + lcMainMfg)
*!*	    SCAN REST WHILE citmmajor+mfgcode+coprcode+STR(nlineno,6)=;
*!*	                    lcPwStyle + lcMainMfg;
*!*	              FOR   ALLTRIM(STR(nLineNo)) $ lcTmpLin
IF SEEK(loFormSet.lcInvType+PADR(lcPwStyle,19)+m.cCstShtTyp+lcPWCostSheetID + lcMainMfg)
SCAN REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+MFGCODE+COPRCODE+STR(NLINENO,6)=;
loFormSet.lcInvType+PADR(lcPwStyle,19)+m.cCstShtTyp+lcPWCostSheetID + lcMainMfg;
FOR   ALLTRIM(STR(nLineNo)) $ lcTmpLin
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
IF EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H' .OR. loFormSet.llAutoGen
SELECT PWCtkBom
ELSE
SELECT(loFormSet.lcPWCtkBom)
ENDIF
lcTCrsRef = lfGnCrsRef(loFormSet)
APPEND BLANK
REPLACE CutTkt      WITH lcCtkt,;
Style       WITH IIF(EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H'  .AND. !loFormSet.llAutoGen, EVALUATE(loFormSet.lcBOMLINE+'.Style'), EVALUATE(loFormSet.lcDetFile+'.Style')),;
cSizes      WITH IIF(EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H'  .AND. !loFormSet.llAutoGen, EVALUATE(loFormSet.lcBOMLINE+'.cSizes'), EVALUATE(loFormSet.lcDetFile+'.cSizes')),;
mfgCode     WITH lcMainMfg,;
cOprCode    WITH IIF(llNewItm , EVALUATE(loFormSet.lcPwBom+'.cOprCode')  ,PWBOM.cOprCode),;
cOperSeq    WITH IIF(llNewItm , EVALUATE(loFormSet.lcPwBom+'.cOperSeq')  ,PWBOM.cOperSeq),;
cOprAtType  WITH IIF(llNewItm , EVALUATE(loFormSet.lcPwBom+'.cOprAtType'),PWBOM.cOprAtType),;
nOperAtPer  WITH IIF(llNewItm , EVALUATE(loFormSet.lcPwBom+'.nOperAtPer'),PWBOM.nOperAtPer),;
nOperCost   WITH IIF(llNewItm , EVALUATE(loFormSet.lcPwBom+'.nOperCost') ,PWBOM.nOperCost),;
cOverType   WITH IIF(llNewItm , EVALUATE(loFormSet.lcPwBom+'.cOverType') ,"Q"),;
nOverUnit   WITH IIF(llNewItm , EVALUATE(loFormSet.lcPwBom+'.nOverUnit') ,0),;
nLineNo     WITH IIF(llNewItm, EVALUATE(loFormSet.lcPwBom+'.nLineNo')    , PWBOM.nLineNo),;
NREPNO      WITH LEN(lcTmpLin) +LEN(lcOldAsign) ,;
nGenNo      WITH lcTCrsRef
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
REPLACE CLOTNO WITH PADL(EVALUATE(loFormSet.lcMFGOPRHD+'.nNxtLotNo'),2,'0')
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
IF EVALUATE(loFormSet.lcPosHdr+'.Status')='H' .AND. !loFormSet.llAutoGen
REPLACE  cStatus  WITH lcNewGen
ENDIF
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
=gfReplace('')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
ENDSCAN
ENDIF
ENDSCAN
IF llNewItm
ERASE (oAriaApplication.WorkDir+PWBOMTAG+".CDX")
USE IN IIF(USED(lcNewMfg),lcNewMfg,0)
ERASE (oAriaApplication.WorkDir+lcNewMfg+".DBF")
ERASE (oAriaApplication.WorkDir+lcNewMfg+".CDX")
SELECT(loFormSet.lcTmpBom)
SET ORDER TO (loFormSet.lcTmpBom)
ENDIF
IF EVALUATE(loFormSet.lcPosHdr+'.Status')<>'H'  .AND. !loFormSet.llAutoGen
SET ORDER TO (loFormSet.lcTktSheet) IN (loFormSet.lcTktSheet)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
SELECT PWCtkBom
=gfTableUpdate()
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
ENDIF
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfPWTmpFil
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/06/2004
*! Purpose   : Create detail Operaiton temp files.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfPWTmpFil()
*!*************************************************************
FUNCTION lfPWTmpFil
LPARAMETERS loFormSet

LOCAL lnOldAls,lnFileStru,lnI
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*!*	=gfOpenFile(oAriaApplication.DataDir+"pwoperat",oAriaApplication.DataDir+"pwoperat","SH")
*!*	=gfOpenFile(oAriaApplication.DataDir+'PWcTkBom',oAriaApplication.DataDir+'PWcTkBom','SH')
*!*	=gfOpenFile(oAriaApplication.DataDir+'PWBOM'   ,oAriaApplication.DataDir+'PWBOM'   ,'SH')
=gfOpenTable("PWOPERAT","PWOPERAT","SH")
=gfOpenTabLe('PWCTKBOM','PWCTKBOM','SH')
=gfOpenTable('PWBOM'   ,'PWBOM'   ,'SH')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
lnOldAls = SELECT(0)
IF !USED(loFormSet.lcPWCtkBom)
SELECT PWcTkBom
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,18]
laFileStru[lnFileStru+1,1] = 'cStatus'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
FOR lnI = 7 TO 16
STORE '' TO laFileStru[lnFileStru+1,lnI]
ENDFOR
STORE 0 TO laFileStru[lnFileStru+1,17],laFileStru[lnFileStru+1,18]

LOCAL ARRAY laIndex[2,2]
laIndex[1,1] = 'cuttkt+mfgcode+coprcode+Style+STR(nlineno,6)'
laIndex[1,2] = loFormSet.lcPWCtkBom
laIndex[2,1] = 'nGenNo'
laIndex[2,2] = 'nGenNo'
=gfCrtTmp(loFormSet.lcPWCtkBom,@laFileStru,@laIndex)
SET ORDER TO TAG (loFormSet.lcPWCtkBom) IN (loFormSet.lcPWCtkBom)
ENDIF
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfPwUpdate
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/30/2004
*! Purpose   : Update detail Operaiton Master files.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfPwUpdate()
*!*************************************************************
FUNCTION lfPwUpdate
LPARAMETERS loFormSet

PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT (loFormSet.lcPWCtkBom)
GOTO TOP
SCAN
SCATTER MEMVAR MEMO
DO CASE
CASE  cStatus = 'A'
INSERT INTO PWCtkBom FROM MEMVAR
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
lcCurAliasSel = SELECT()
SELECT PWCtkBom
=gfReplace('')
SELECT(lcCurAliasSel)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
CASE  cStatus = 'M'
*--- cuttkt+mfgcode+coprcode+STR(nlineno,6)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*IF SEEK(m.cuttkt+m.mfgcode+m.coprcode+m.Style+STR(m.nlineno,6),PWCtkBom)
IF gfSEEK(m.cuttkt+m.mfgcode+m.coprcode+m.Style+STR(m.nlineno,4),'PWCtkBom')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
SELECT PWCtkBom
GATHER MEMVAR MEMO
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
=gfReplace('')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
ENDIF
ENDCASE
ENDSCAN
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
SELECT PWCtkBom
=gfTableUpdate()
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfPwRem
*! Developer : AHEMD MAHER (AMH)
*! Date      : 01/13/2004
*! Purpose   : Function to remove detail oeration.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfPwRem()
*!*************************************************************
FUNCTION lfPwRem
LPARAMETERS lcCTkt,lcMainOpr,loFormSet
LOCAL lnOldAls , llPwClsBom

*--- cuttkt+mfgcode+coprcode+STR(nlineno,6)
lnOldAls = SELECT(0)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*llPwClsBom = IIF(!USED('PwCtkBom'),gfOpenFile(oAriaApplication.DataDir+'PWcTkBom',oAriaApplication.DataDir+'PWcTkBom','SH'),.F.)
llPwClsBom = IIF(!USED('PwCtkBom'),gfOpenTable('PWcTkBom','PWcTkBom','SH'),.F.)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
IF EVALUATE(loFormSet.lcPosHdr+'.PO')<>'H'
SELECT PwCtkBom
ELSE
SELECT(loFormSet.lcPwCtkBom)
ENDIF
IF SEEK(lcCTkt+lcMainOpr)
DELETE REST WHILE cuttkt+mfgcode+coprcode+STR(nlineno,6) = lcCTkt+lcMainOpr
ENDIF
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
IF EVALUATE(loFormSet.lcPosHdr+'.PO')<>'H'
SELECT PwCtkBom
IF gfSEEK(lcCTkt+lcMainOpr)
SCAN REST WHILE cuttkt+mfgcode+coprcode+STR(nlineno,6) = lcCTkt+lcMainOpr
gfDELETE()
ENDSCAN
=gfTableUpdate()
ENDIF
ENDIF
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
SELECT(lnOldAls)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*USE IN IIF(llPwClsBom,'PwCtkBom',0)
IF llPwClsBom
= gfCloseTable('PwCtkBom')
ELSE
USE IN IIF(llPwClsBom,'PwCtkBom',0)
ENDIF
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]

*!*************************************************************
*! Name      : lfDisCost
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/03/2004
*! Purpose   : Function to Desable cost if detail oeratin exist.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDisCost()
*!*************************************************************
FUNCTION lfDisCost
LPARAMETERS loFormSet

LOCAL lnOldAls
IF loParentForm.llPwInst
lnOldAls = SELECT(0)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*!*	  =gfOpenFile(oAriaApplication.DataDir+"pwoperat" , oAriaApplication.DataDir+"pwoperat" , "SH")
*!*	  IF loParentForm.llPWInst .AND. loParentForm.lcTranType = "M" .AND. SEEK(EVALUATE(loParentForm.lcTktSheet+'.mfgCode'),'pwoperat')
=gfOpenTable("PWOPERAT" , "PWOPERAT" )
IF loParentForm.llPWInst .AND. loParentForm.lcTranType = "M" .AND. gfSEEK(EVALUATE(loParentForm.lcTktSheet+'.mfgCode'),'PWOPERAT')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
loFormSet.AriaForm1.txtUnitCost.Enabled = .F.
ELSE
loFormSet.AriaForm1.txtUnitCost.Enabled = .T.
ENDIF
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*USE IN pwoperat
=gfCloseTable('PWOPERAT')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
SELECT(lnOldAls)
ENDIF

*!*************************************************************
*! Name      : lfOpnStBrw
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/03/2004
*! Purpose   : Function to check if we are going to open style browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfOpnStBrw()
*!*************************************************************
FUNCTION lfOpnStBrw
LPARAMETERS lcMainMfg

LOCAL lnOldAls , llToReturn , lcTStyClr , lnTmpCost , lcTempSizs
lnOldAls = SELECT(0)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*=gfOpenFile(oAriaApplication.DataDir+'PWcTkBom',oAriaApplication.DataDir+'PWcTkBom','SH')
=gfOpenTable('PWcTkBom','PWcTkBom','SH')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]

llToReturn = .F.
IF loParentForm.ActiveMode = "V"  && Direct Update Master File
SELECT PWCTKBOM
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
=gfSEEK(EVALUATE(loParentForm.lcPosHdr+'.PO') + lcMainMfg )
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
ELSE
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
IF !USED(loParentForm.lcPWCTKBOM)
=lfPWTmpFil(loParentForm)
ENDIF
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
SELECT (loParentForm.lcPWCTKBOM)
ENDIF
*--- cuttkt+mfgcode+coprcode+STR(nlineno,6)
IF SEEK(EVALUATE(loParentForm.lcPosHdr+'.PO') + lcMainMfg )
DO WHILE cuttkt+mfgcode+coprcode+STR(nlineno,6) = EVALUATE(loParentForm.lcPosHdr+'.PO') + lcMainMfg .AND. !llToReturn
lcTStyClr  = Style
lnTmpCost  = (nOperCost/nOperAtPer)
lcTempSizs = cSizes
lcTempDet  = coprcode
SCAN REST WHILE cuttkt+mfgcode+coprcode+STR(nlineno,6) = EVALUATE(loParentForm.lcPosHdr+'.PO') + lcMainMfg;
FOR !llToReturn
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_MFCSSH_CHKDETOPR+" : "+mfgcode+" "+loParentForm.lcItmHdr+" "+Style+" "+LANG_MFCSSH_PLSWAIT+".!" NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CHKDETOPR,loFormSet.GetHeaderText("LANG_MFCSSH_CHKDETOPR",loFormSet.HeaderAlias))+" : "+;
mfgcode+" "+loParentForm.lcItmHdr+" "+Style+" "+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PLSWAIT,loFormSet.GetHeaderText("LANG_MFCSSH_PLSWAIT",loFormSet.HeaderAlias))+".!" NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

IF Style  <> lcTStyClr
llToReturn = .T.
ELSE
IF (nOperCost/nOperAtPer) <> lnTmpCost .OR. cSizes <> lcTempSizs .OR. coprcode <> lcTempDet
llToReturn = .T.
ENDIF
ENDIF
ENDSCAN
ENDDO
ENDIF
SELECT(lnOldAls)
IF !llToReturn
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_MFCSSH_NODEFFND+" !" NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NODEFFND,loFormSet.GetHeaderText("LANG_MFCSSH_NODEFFND",loFormSet.HeaderAlias))+" !" NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
RETURN llToReturn

*!*************************************************************
*! Name      : lfvSelDet
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/03/2004
*! Purpose   : Valid Function detail operation button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSelDet()
*!*************************************************************
FUNCTION lfvSelDet

LOCAL lnOldAls , lnNwAveCst , m.Desc , m.UnitCost , m.Unit
PRIVATE lcCurStyle
lnOldAls = SELECT(0)
=lfCreDTmp()
lnNwAveCst = 0
lcCurStyle = Style
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
lcPWCtkBom = loParentForm.lcPWCtkBom
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
*--- These PARAMETER lcParntOpr , lcDOprTmp , lcCostVar
*--- Are use in calling detail operation routing screen.
*--- lcParntOpr == Main MfgCode
*--- lcDOprTmp  == Detail Operation Assigned on the Main Mfg.
*--- lcCostVar  == Variable to hold the calculated new average cost.
IF EVALUATE(loParentForm.lcPosHdr+'.Status')<>'H'
SELECT PwcTkBom
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
=gfSEEK(EVALUATE(loParentForm.lcPosHdr+'.PO') + EVALUATE(loParentForm.lcTktSheet+'.mfgCode') )
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
ELSE
SELECT (lcPwcTkBom)
ENDIF
*--- cuttkt+mfgcode+coprcode+STR(nlineno,6)
IF SEEK(EVALUATE(loParentForm.lcPosHdr+'.PO') + EVALUATE(loParentForm.lcTktSheet+'.mfgCode') )
SCAN REST WHILE cuttkt+mfgcode+coprcode+Style+STR(nlineno,6)=;
EVALUATE(loParentForm.lcPosHdr+'.PO') + EVALUATE(loParentForm.lcTktSheet+'.mfgCode');
FOR Style = lcCurStyle
SCATTER MEMVAR MEMO
lnNREPNO  = m.NREPNO
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_MFCSSH_SELRECOPR+" :"+EVALUATE(loParentForm.lcTktSheet+'.mfgCode')+" "+LANG_MFCSSH_DETOPR+" : "+cOprCode NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_SELRECOPR,;
loFormSet.GetHeaderText("LANG_MFCSSH_SELRECOPR",loFormSet.HeaderAlias))+" :"+EVALUATE(loParentForm.lcTktSheet+'.mfgCode')+" "+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DETOPR,loFormSet.GetHeaderText("LANG_MFCSSH_DETOPR",loFormSet.HeaderAlias))+;
" : "+cOprCode NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*=SEEK(EVALUATE(loParentForm.lcTktSheet+'.mfgCode')+cOprCode,'PwOperat')
=gfSEEK(EVALUATE(loParentForm.lcTktSheet+'.mfgCode')+cOprCode,'PWOPERAT')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
m.MfgCode  = EVALUATE(loParentForm.lcTktSheet+'.mfgCode')
m.ChildOpr = cOprCode
m.Desc     = PwOperat.cDescRip
m.WorkCent = PwOperat.cWorkCent
m.SetUpTim = PwOperat.NSetup_Tim
m.PersTim  = PwOperat.NEmp_Time
m.MatchTim = PwOperat.NMach_Time
m.Unit     = PwOperat.nUnit
m.Seq      = cOperSeq
m.RatType  = cOprAtType
m.RatePer  = nOperatPer
m.UnitCost = nOperCost
INSERT INTO (loParentForm.lcDetLin) FROM MEMVAR
ENDSCAN
SET ORDER TO TAG (loParentForm.lcDetLin2) IN (loParentForm.lcDetLin)
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
IF TYPE('loParentForm.AriaForm1.txtunitCost') <> 'O'
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
loParentForm.AriaForm1.AddObject('txtunitCost','TEXTBOX')
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
ENDIF
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
loParentForm.AriaForm1.txtunitCost.Value = lnNwAveCst
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFDetOpr.scx') WITH m.MfgCode,loParentForm.lcDetLin,.F.,.F.,.T.,loParentForm
=gfCallForm('MFDetOpr',.F.,"m.MfgCode,loParentForm.lcDetLin,.F.,.F.,.T.,loParentForm")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*lnNwAveCst = (loParentForm.AriaForm1.txtunitCost.Value/lnNREPNO)
lnNwAveCst = IIF(lnNREPNO<> 0,(loParentForm.AriaForm1.txtunitCost.Value/lnNREPNO),0)
*!*  E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
loParentForm.AriaForm1.RemoveObject('txtunitCost')
SET ORDER TO TAG (loParentForm.lcDetLin) IN (loParentForm.lcDetLin)
ENDIF
SELECT(lnOldAls)
RETURN  lnNwAveCst

*!*************************************************************
*! Name      : lfCreDTmp
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/03/2004
*! Purpose   : Function to create detail oeration screen temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfCreDTmp()
*!*************************************************************
FUNCTION lfCreDTmp

LOCAL lnOldAls
lnOldAls = SELECT(0)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*=gfOpenFile(oAriaApplication.DataDir+"pwoperat" , oAriaApplication.DataDir+"pwoperat" , "SH")
=gfOpenTable("PWOPERAT" , "PWOPERAT")
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
IF USED(loParentForm.lcDetLin)
SELECT(loParentForm.lcDetLin)
ZAP
USE IN (loParentForm.lcDetLin)
ENDIF
DIMENSION laFileStru[17,18]

laFileStru[01,1] = 'ChildOpr'
laFileStru[01,2] = 'C'
laFileStru[01,3] = 6
laFileStru[01,4] = 0

laFileStru[02,1] = 'Desc'
laFileStru[02,2] = 'C'
laFileStru[02,3] = 30
laFileStru[02,4] = 0

laFileStru[03,1] = 'WorkCent'
laFileStru[03,2] = 'C'
laFileStru[03,3] = 2
laFileStru[03,4] = 0

laFileStru[04,1] = 'SetUpTim'
laFileStru[04,2] = 'N'
laFileStru[04,3] = 6
laFileStru[04,4] = 0

laFileStru[05,1] = 'PersTim'
laFileStru[05,2] = 'N'
laFileStru[05,3] = 6
laFileStru[05,4] = 0

laFileStru[06,1] = 'MatchTim'
laFileStru[06,2] = 'N'
laFileStru[06,3] = 6
laFileStru[06,4] = 0

laFileStru[07,1] = 'Unit'
laFileStru[07,2] = 'N'
laFileStru[07,3] = 6
laFileStru[07,4] = 0

laFileStru[08,1] = 'Seq'
laFileStru[08,2] = 'C'
laFileStru[08,3] = 2
laFileStru[08,4] = 0

laFileStru[09,1] = 'RatType'
laFileStru[09,2] = 'C'
laFileStru[09,3] = 6
laFileStru[09,4] = 0

laFileStru[10,1] = 'RatePer'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 3
laFileStru[10,4] = 0

laFileStru[11,1] = 'UnitCost'
laFileStru[11,2] = 'N'
laFileStru[11,3] = 9
laFileStru[11,4] = 2

laFileStru[12,1] = 'Tool'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 2
laFileStru[12,4] = 0

laFileStru[13,1] = 'MfgCode'
laFileStru[13,2] = 'C'
laFileStru[13,3] = 6
laFileStru[13,4] = 0

laFileStru[14,1] = 'cStatus'
laFileStru[14,2] = 'C'
laFileStru[14,3] = 1
laFileStru[14,4] = 0

laFileStru[15,1] = 'nLineNo'
laFileStru[15,2] = 'N'
laFileStru[15,3] = 4
laFileStru[15,4] = 0

laFileStru[16,1] = 'cOverType'
laFileStru[16,2] = 'C'
laFileStru[16,3] = 1
laFileStru[16,4] = 0

laFileStru[17,1] = 'nOverUnit'
laFileStru[17,2] = 'N'
laFileStru[17,3] = 7
laFileStru[17,4] = 0

LOCAL lnI,lnJ
FOR lnI = 7 TO 16
FOR lnJ = 1 TO 17
STORE '' TO laFileStru[lnJ,lnI]
ENDFOR
ENDFOR
FOR lnJ = 1 TO 17
STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
ENDFOR

LOCAL ARRAY laIndex[2,2]
laIndex[1,1] = 'MfgCode+Seq+ChildOpr'
laIndex[1,2] = loParentForm.lcDetLin2
laIndex[2,1] = 'MfgCode+ChildOpr'
laIndex[2,2] = loParentForm.lcDetLin
=gfCrtTmp(loParentForm.lcDetLin,@laFileStru,@laIndex)
SET ORDER TO TAG (loParentForm.lcDetLin) IN (loParentForm.lcDetLin)
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfGnCrsRef
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/05/2004
*! Purpose   : Function to generate cross referance no.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfGnCrsRef()
*!*************************************************************
FUNCTION lfGnCrsRef
LPARAMETERS loFormSet

LOCAL lnOldAls , lcNewGenNo

lnOldAls = SELECT(0)
SELECT PwcTkBom
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*SET ORDER TO NGenNo
=gfSetOrder('NGenNo')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
GO BOTTOM
lcNewGenNo  = NGenNo
SELECT (loFormSet.lcPwcTkBom)
SET ORDER TO NGenNo
GO BOTTOM
lcNewGenNo = MAX(NGenNo , lcNewGenNo)
lcNewGenNo = PADL(ALLTRIM(STR(VAL(lcNewGenNo)+1)),10,'0')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*DO WHILE SEEK(lcNewGenNo,'PwcTkBom') .OR. SEEK(lcNewGenNo,loFormSet.lcPwcTkBom)
DO WHILE gfSEEK(lcNewGenNo,'PwcTkBom') .OR. SEEK(lcNewGenNo,loFormSet.lcPwcTkBom)
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
lcNewGenNo = PADL(ALLTRIM(STR(VAL(lcNewGenNo)+1)),10,'0')
ENDDO
SELECT PwcTkBom
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[Start]
*SET ORDER TO PwcTkBom
gfSETORDER('PwcTkBom')
*! E303137,1 MMT 05/08/2012 Convert PW tables to SQL and change cost sheet prg to work on SQL Tables[END]
SELECT (loFormSet.lcPwcTkBom)
SET ORDER TO (loFormSet.lcPwcTkBom)
SELECT(lnOldAls)
RETURN lcNewGenNo

*!**************************************************************************
*! Name      : lfRecCost
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/08/2004
*! Purpose   : To recalculate the duty when its a percentage.
*!**************************************************************************
*! Example   :  =lfRecCost()
*!**************************************************************************
FUNCTION lfRecCost
LPARAMETERS lcimtyp,lctktno,lcDetFile,lcTktSheet,loFormSet

LOCAL lnAlias,lnPoRecNo,lcStyle,lnDutyAmt,lnItemAmt
lnAlias = SELECT(0)

SELECT (loFormSet.lcPOSLN)
lnPoRecNo = RECNO()
REPLACE ALL cOwner WITH '1'
LOCATE
DO WHILE !EOF()
lcStyle = Style

*B608268,1 MMT 09/17/2008 Fix bug of wrong duty when style is repeated[Start]
*REPLACE ALL cOwner WITH '' FOR style = lcStyle
lnLinNum = lineno
REPLACE ALL cOwner WITH '' FOR style = lcStyle AND lineno = lnLinNum
*B608268,1 MMT 09/17/2008 Fix bug of wrong duty when style is repeated[End]

SELECT (lcDetFile)
LOCATE
*B608268,1 MMT 09/17/2008 Fix bug of wrong duty when style is repeated[Start]
*SUM ItemAmt TO lnDutyAmt FOR cCostStat = '1' AND Style = lcStyle
SUM ItemAmt TO lnDutyAmt FOR cCostStat = '1' AND Style = lcStyle AND lineno = lnLinNum
*B608268,1 MMT 09/17/2008 Fix bug of wrong duty when style is repeated[End]

SELECT (lcDetFile)
LOCATE
*B608268,1 MMT 09/17/2008 Fix bug of wrong duty when style is repeated[Start]
*SCAN FOR cCatgTyp = 'D' AND Style = lcStyle
SCAN FOR cCatgTyp = 'D' AND Style = lcStyle AND lineno = lnLinNum
*B608268,1 MMT 09/17/2008 Fix bug of wrong duty when style is repeated[End]

IF nPercent > 0
REPLACE ItemAmt  WITH nPercent*lnDutyAmt/100 ,;
UnitCost WITH IIF(ItemQTy=0,0,ItemAmt/ItemQTy)
ENDIF
ENDSCAN
SELECT (loFormSet.lcPOSLN)
LOCATE FOR cOwner = '1'
ENDDO

SELECT (lcTktSheet)
LOCATE
SCAN FOR cCatgTyp = 'D'
lnItemAmt = 0
SELECT (lcDetFile)
LOCATE
SCAN FOR cbomtyp = EVALUATE(lcTktSheet+'.typ') AND cCatgTyp = 'D'
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*lnItemAmt = lnItemAmt + ItemAmt
STORE '/' TO lcExSign,lcUntSin
lcPosHdr = loFormSet.lcPosHdr
IF ISNULL(cCurrCode) OR EMPTY(cCurrCode)
lcCurrCode = IIF(EMPTY(&lcPosHdr..cDutyCur),oAriaApplication.BaseCurrency,&lcPosHdr..cDutyCur)
lnExRate   = IIF(&lcPosHdr..nDutyRat=0,1, &lcPosHdr..nDutyRat)
lnCurrUnit = IIF(&lcPosHdr..nDCurUnit=0,1, &lcPosHdr..nDCurUnit)
ELSE
lcCurrCode = cCurrCode
lnExRate   = IIF(ISNULL(nExRate) OR nExRate=0,1,nExRate)
lnCurrUnit = IIF(ISNULL(nCurrUnit) OR nCurrUnit=0,1,nCurrUnit)
ENDIF
lcExSign   = gfGetExSin(@lcUntSin, lcCurrCode)
lnItemAmt = lnItemAmt + ItemAmt &lcExSign lnExRate &lcUntSin lnCurrUnit
*N000587,1 WAM 12/01/2007 (End)
ENDSCAN
*N000587,1 HBG 2/22/2007 Get the equevelant of the duty cost [Begin]
*SELECT (lcTktSheet)
*REPLACE Est_Cost WITH lnItemAmt ,;
UntCost  WITH IIF(Req_Qty =0,0,Est_Cost/Req_Qty)
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*!*	  lcDutyCur   = IIF(EMPTY(EVALUATE(loFormSet.lcPOSHDR+'.cDutyCur')),oAriaApplication.BaseCurrency,EVALUATE(loFormSet.lcPOSHDR+'.cDutyCur'))
*!*	  lnDutyRate  = IIF(EVALUATE(loFormSet.lcPOSHDR+'.nDutyRat')=0,1,EVALUATE(loFormSet.lcPOSHDR+'.nDutyRat'))
*!*	  lnDutyUnit  = IIF(EVALUATE(loFormSet.lcPOSHDR+'.nDCurUnit')=0,1,EVALUATE(loFormSet.lcPOSHDR+'.nDCurUnit'))
*!*	  STORE '/' TO lcExSign,lcUntSin
*!*	  lcExSign   = gfGetExSin(@lcUntSin,lcDutyCur)
*!*	  lnTotCost  = lnItemAmt &lcExSign lnDutyRate &lcUntSin lnDutyUnit
lnTotCost  = lnItemAmt
*N000587,1 WAM 12/01/2007 (End)
SELECT (lcTktSheet)
REPLACE Est_Cost WITH lnTotCost,;
UntCost  WITH IIF(Req_Qty =0,0,Est_Cost/Req_Qty)
*N000587,1 [End]
ENDSCAN

IF BETWEEN(lnPoRecNo,1,RECCOUNT(loFormSet.lcPOSLN))
GOTO lnPoRecNo IN (loFormSet.lcPOSLN)
ENDIF

SELECT (lcTktSheet)
LOCATE
SELECT (lcDetFile)
LOCATE

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfUpdEstCst
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/08/2004
*! Purpose   : To recalculate the estimated costs in for PO,CT and MPO
*!**************************************************************************
*! Example   :  =lfUpdEstCst()
*!**************************************************************************
FUNCTION lfUpdEstCst
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*LPARAMETERS loFormSet
LPARAMETERS loFormSet, laFrgnCost, laEquCost
STORE 0 TO laFrgnCost, laEquCost
*N000587,1 WAM 12/01/2007 (End)

LOCAL lnAlias,lnECost,lnTransQty,lnBomLLine,lnEstCost,lcBomSizes,lnCntr,lcCntr
lnAlias = SELECT(0)
SELECT (loFormSet.lcBomLine)
LOCATE
IF FOUND()
*-- Initialize the estimated costs with 0 in all the records of the PO.
*-- or the Adornment Order
SELECT (loFormSet.lcPosLn)
IF !loFormSet.llAutoGen
REPLACE ALL nFCost1 WITH 0, nFCost2 WITH 0, nFCost3 WITH 0, nFCost4 WITH 0, nFCost5 WITH 0, nFCost6 WITH 0, nFCost7 WITH 0,;
nICost1 WITH 0, nICost2 WITH 0, nICost3 WITH 0, nICost4 WITH 0, nICost5 WITH 0, nICost6 WITH 0, nICost7 WITH 0
ELSE
REPLACE ALL nFCost2 WITH 0, nFCost3 WITH 0, nFCost4 WITH 0, nFCost5 WITH 0, nFCost6 WITH 0, nFCost7 WITH 0,;
nICost2 WITH 0, nICost3 WITH 0, nICost4 WITH 0, nICost5 WITH 0, nICost6 WITH 0, nICost7 WITH 0
ENDIF

SELECT (loFormSet.lcBOMLINE)
REPLACE ALL cOwner WITH '1'
LOCATE FOR cType = '1' AND cOwner = '1' AND !lVoid
DO WHILE !EOF()
REPLACE cOwner WITH ''
*=TABLEUPDATE()
SCATTER MEMVAR
lnECost    = 0
lnTransQty = 0
lcBomStyle = Style
lcBomInvTp = cInvType
lnBomLLine = LineNo
lcBomLType = cBomTyp
lcBomLnKey = cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style+cInvTypC+Item+MfgCode
lcBomCatg  = cCatgTyp

SELECT (loFormSet.lcPosLn)
LOCATE FOR LineNo = lnBomLLine
IF FOUND()
lnEstCost  = 0
SELECT (loFormSet.lcBOMLINE)

*! B609019,1 HES 09/23/2009 Process of generating PO Cost Sheet slow [Start]
gfSeek(loFormSet.lcTranType+'1'+EVALUATE(loFormSet.lcPosHdr+'.PO')+ ;
STR(lnBomLLine,6)+lcBomLType+lcBomInvTp+lcBomStyle)

*!*	      SCAN FOR cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style+cInvTypC+Item+MfgCode = loFormSet.lcTranType+'1'+;
*!*	               EVALUATE(loFormSet.lcPosHdr+'.PO')+STR(lnBomLLine,6)+lcBomLType+lcBomInvTp+lcBomStyle AND !lVoid
SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style+cInvTypC+Item+MfgCode = loFormSet.lcTranType+'1'+;
EVALUATE(loFormSet.lcPosHdr+'.PO')+STR(lnBomLLine,6)+lcBomLType+lcBomInvTp+lcBomStyle AND !lVoid
*! B609019,1 HES 09/23/2009 Process of generating PO Cost Sheet slow [End]

REPLACE cOwner WITH ''
lcBomSizes = IIF(EMPTY(cSizes),'12345678',cSizes)

lnTransQty  = 0
FOR lnCntr = 1 TO 8
lcCntr   = STR(lnCntr,1)
IF lcCntr $ lcBomSizes
lnTransQty = lnTransQty + EVALUATE(loFormSet.lcPosLn+'.Qty'+lcCntr)
ENDIF
ENDFOR
lnEstCost  = lnEstCost + ((UnitQty * UnitCost) * lnTransQty)
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines

*B609262,1 WAM 05/20/2010 Recalculate CT unit cost in POSLN
*IF loFormSet.lcTranType $ 'IDN'
*B609262,1 WAM 05/20/2010 (End)

IF INLIST(cCatgTyp,'S','F','T')
lnECost  = lnECost + (UnitQty * UnitCost * lnTransQty)
ELSE
STORE '/' TO lcExSign,lcUntSin
lcPosHdr = loFormSet.lcPosHdr
IF ISNULL(cCurrCode) OR EMPTY(cCurrCode)
DO CASE
CASE cCatgTyp = 'P'
lcCurrCode = IIF(EMPTY(&lcPosHdr..cPriceCur),oAriaApplication.BaseCurrency,&lcPosHdr..cPriceCur)
lnExRate   = IIF(&lcPosHdr..nPriceRat=0,1, &lcPosHdr..nPriceRat)
lnCurrUnit = IIF(&lcPosHdr..nCurrUnit=0,1, &lcPosHdr..nCurrUnit)
CASE INLIST(cCatgTyp,'D','M')
lcCurrCode = IIF(EMPTY(&lcPosHdr..cDutyCur),oAriaApplication.BaseCurrency,&lcPosHdr..cDutyCur)
lnExRate   = IIF(&lcPosHdr..nDutyRat=0,1, &lcPosHdr..nDutyRat)
lnCurrUnit = IIF(&lcPosHdr..nDCurUnit=0,1, &lcPosHdr..nDCurUnit)
OTHERWISE
lcCurrCode = oAriaApplication.BaseCurrency
lnExRate   = 1
lnCurrUnit = 1
ENDCASE
ELSE
lcCurrCode = cCurrCode
lnExRate   = IIF(ISNULL(nExRate) OR nExRate=0,1,nExRate)
lnCurrUnit = IIF(ISNULL(nCurrUnit) OR nCurrUnit=0,1,nCurrUnit)
ENDIF
lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)
lnECost  = lnECost + (UnitQty * UnitCost * lnTransQty) &lcExSign lnExRate &lcUntSin lnCurrUnit
ENDIF

*B609262,1 WAM 05/20/2010 Recalculate CT unit cost in POSLN
*ENDIF
*B609262,1 WAM 05/20/2010 (End)

*N000587,1 WAM 12/01/2007 (End)
ENDSCAN
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
lnBomLType = EVALUATE(lcBomLType)
laFrgnCost[lnBomLType] = laFrgnCost[lnBomLType] + lnEstCost
laEquCost[lnBomLType] = laEquCost[lnBomLType] + lnECost
lnECost  = lnECost / EVALUATE(loFormSet.lcPosLn+'.TotQty')
*N000587,1 WAM 12/01/2007 (End)
lnEstCost  = lnEstCost / EVALUATE(loFormSet.lcPosLn+'.TotQty')
IF loFormSet.lcTranType $ 'IDN'
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*!*	        DO CASE
*!*	          CASE lcBomCatg = 'P'
*!*	            lnECost = EVALUATE('lnEstCost '+loFormSet.lcPExSign+' '+loFormSet.lcPOSHDR+'.nPriceRat '+;
*!*	                               loFormSet.lcPUntSin+' '+loFormSet.lcPOSHDR+'.nCurrUnit')
*!*	          CASE !INLIST(lcBomCatg,'S','F','T')
*!*	            lnECost = EVALUATE('lnEstCost '+loFormSet.lcDExSign+' '+loFormSet.lcPOSHDR+'.nDutyRat '+;
*!*	                               loFormSet.lcDUntSin+' '+loFormSet.lcPOSHDR+'.nDCurUnit')
*!*	          OTHERWISE
*!*	            lnECost  = lnEstCost
*!*	        ENDCASE
*N000587,1 WAM 12/01/2007 (End)
IF !( lcBomCatg = 'P' AND loFormSet.llAutoGen)
SELECT (loFormSet.lcPosLn)
REPLACE ('nFCost'+lcBomLType) WITH lnEstCost ,;
('nICost'+m.cBomTyp) WITH lnECost
IF lcBomLType = '1'
IF nFCost1 > Gros_Price
REPLACE Gros_Price WITH nFCost1 ;
Disc_Pcnt  WITH 0
ELSE
REPLACE Disc_Pcnt  WITH IIF(Gros_Price = 0 , 0 , 100 - nFCost1 * 100 / Gros_Price)
ENDIF
ENDIF
ENDIF
ELSE
SELECT (loFormSet.lcPosLn)
REPLACE ('nFCost'+lcBomLType) WITH lnEstCost
*N037452,1 WAM 04/22/2005 Handle Cutting Ticket Cost SHeet
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
*REPLACE ('nICost'+lcBomLType) WITH lnEstCost
REPLACE ('nICost'+lcBomLType) WITH lnECost
*N000587,1 WAM 12/01/2007 (End)
*N037452,1 WAM 04/22/2005 (End)
ENDIF
ENDIF
SELECT (loFormSet.lcBomLine)
LOCATE FOR cType = '1' AND cOwner = '1' AND !lVoid
ENDDO
LOCATE
ENDIF
SELECT(lnAlias)
*--end of lfUpdEstCst.

*!*************************************************************
*! Name      : lfvIsStyWr
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/21/2004
*! Purpose   : Validate Style issued Warehouse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvIsStyWr()
*!*************************************************************
FUNCTION lfvIsStyWr
LPARAMETERS loFormSet

lcIssWare = loParentForm.laStyWare[lnIssWare,2]
=SEEK(m.Item+lcIssWare+space(10),'StyDye')
lnIssCost = IIF(loParentForm.laSetups[12,2]='A',StyDye.Ave_Cost,Style.TotCost)

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
m.nOnHand = StyDye.totStk
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]

loFormSet.Refresh
*-- end of lfvIsStyWr.

*!*************************************************************
*! Name      : lfvClose
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/01/2004
*! Purpose   : Valid funtion for Close button
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
*B605447,1
*!*************************************************************
FUNCTION lfvClose
LPARAMETERS loFormSet

PRIVATE lnTActCst,lnTEstCst,lnITActCst,lnITEstCst,lcItem,lnRecNo, llCloseTr

*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
*!*  llApVendor = (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)
*!*  IF llApVendor
*!*    llCloseTr = .F.
*!*    PRIVATE lcMfgGlAcnt
*!*    lcMfgGlAcnt = loFormSet.lcMfgGlAcnt
*!*    LOCAL ARRAY laMfgRFld[7,2]
*!*    =ACOPY(loFormSet.laMfgRFld,laMfgRFld)
*!*    m.cIMTyp = loFormSet.lcTranType
*!*    m.cTktNo = EVALUATE(loFormSet.lcPosHdr+'.PO')
*!*    IF !lfOpenSql('BOMCOST','BOMCOST','POBOMCLS','CIMTYP+CTKTNO',loFormSet)
*!*      RETURN .F.
*!*    ENDIF
*!*
*!*    SELECT BOMCOST
*!*    LOCATE
*!*    IF FOUND()
*!*      SCAN
*!*        IF Actualize = "N" AND ;
*!*          (INLIST(CCOSTTYPE,'P','D') OR ;
*!*          (cCostType="M" AND gfRltFld(MfgCode,@laMfgRFld,'MFGCODE') .AND. EMPTY(lcMfgGlAcnt)))
*!*          llCloseTr = .T.
*!*          EXIT
*!*        ENDIF
*!*      ENDSCAN
*!*    ENDIF
*!*    IF llCloseTr
*!*      lcMsgCl = LANG_MFCSSH_CLSMSG1 + ' ' + LANG_MFCSSH_CLSMSG2
*!*      IF gfModalGen('QRM00000B00006','DIALOG',.F., .F., lcMsgCl) = 2
*!*        RETURN
*!*      ENDIF
*!*    ENDIF

*!*    =lfContrbut(loFormSet.lcTranType,EVALUATE(loFormSet.lcPosHdr+'.PO'),loFormSet)
*!*  ENDIF
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]
DO CASE
CASE loFormSet.lcTranType='M'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcClsTitle = LANG_MFCSSH_CLOSE2+' '+LANG_MFCSSH_CUTTKT+' '+EVALUATE(loFormSet.lcPosHdr+'.PO')
*lcMessage  = LOWER(SUBSTR(LANG_MFCSSH_CUTTKT,1,LEN(LANG_MFCSSH_CUTTKT)-1))
lcClsTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CLOSE2,loFormSet.GetHeaderText("LANG_MFCSSH_CLOSE2",loFormSet.HeaderAlias))+;
' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias))+' '+EVALUATE(loFormSet.lcPosHdr+'.PO')
lcMessage  = LOWER(SUBSTR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias)),1,;
LEN(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CUTTKT,loFormSet.GetHeaderText("LANG_MFCSSH_CUTTKT",loFormSet.HeaderAlias)))-1))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE loFormSet.lcTranType='I'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcClsTitle = LANG_MFCSSH_CLOSE2+' '+LANG_MFCSSH_PO+' '+EVALUATE(loFormSet.lcPosHdr+'.PO')
*lcMessage  = LOWER(SUBSTR(LANG_MFCSSH_PO,1,LEN(LANG_MFCSSH_PO)-1))
lcClsTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CLOSE2,loFormSet.GetHeaderText("LANG_MFCSSH_CLOSE2",loFormSet.HeaderAlias))+;
' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias))+' '+EVALUATE(loFormSet.lcPosHdr+'.PO')
lcMessage  = LOWER(SUBSTR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias)),1,;
LEN(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,loFormSet.GetHeaderText("LANG_MFCSSH_PO",loFormSet.HeaderAlias)))-1))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE loFormSet.lcTranType='D'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcClsTitle = LANG_MFCSSH_CLOSE2+' '+LANG_MFCSSH_DYEORD+' '+EVALUATE(loFormSet.lcPosHdr+'.PO')
*lcMessage  = LOWER(SUBSTR(LANG_MFCSSH_DYEORD,1,LEN(LANG_MFCSSH_DYEORD)-1))
lcClsTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CLOSE2,loFormSet.GetHeaderText("LANG_MFCSSH_CLOSE2",loFormSet.HeaderAlias))+' '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYEORD,loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias))+' '+EVALUATE(loFormSet.lcPosHdr+'.PO')
lcMessage  = LOWER(SUBSTR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYEORD,loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias)),1,;
LEN(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYEORD,loFormSet.GetHeaderText("LANG_MFCSSH_DYEORD",loFormSet.HeaderAlias)))-1))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE loFormSet.lcTranType='N'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcClsTitle = LANG_MFCSSH_CLOSE2+' '+LANG_MFCSSH_NPO+' '+EVALUATE(loFormSet.lcPosHdr+'.PO')
*lcMessage  = LOWER(SUBSTR(LANG_MFCSSH_NPO,1,LEN(LANG_MFCSSH_NPO)-1))
lcClsTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CLOSE2,loFormSet.GetHeaderText("LANG_MFCSSH_CLOSE2",loFormSet.HeaderAlias))+' '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias))+' '+EVALUATE(loFormSet.lcPosHdr+'.PO')
lcMessage  = LOWER(SUBSTR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias)),1,;
LEN(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NPO,loFormSet.GetHeaderText("LANG_MFCSSH_NPO",loFormSet.HeaderAlias)))-1))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE loFormSet.lcTranType='T'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcClsTitle = LANG_MFCSSH_CLOSE2+' '+LANG_MFCSSH_MFGORD+' '+EVALUATE(loFormSet.lcPosHdr+'.PO')
*lcMessage  = LOWER(SUBSTR(LANG_MFCSSH_MFGORD,1,LEN(LANG_MFCSSH_MFGORD)-1))
lcClsTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CLOSE2,loFormSet.GetHeaderText("LANG_MFCSSH_CLOSE2",loFormSet.HeaderAlias))+' '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFGORD,loFormSet.GetHeaderText("LANG_MFCSSH_MFGORD",loFormSet.HeaderAlias))+' '+;
EVALUATE(loFormSet.lcPosHdr+'.PO')
lcMessage  = LOWER(SUBSTR(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFGORD,loFormSet.GetHeaderText("LANG_MFCSSH_MFGORD",loFormSet.HeaderAlias)),1,;
LEN(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFGORD,loFormSet.GetHeaderText("LANG_MFCSSH_MFGORD",loFormSet.HeaderAlias)))-1))
*N000682,1 11/20/2012 MMT Globlization changes[End]
ENDCASE

*--- Doc. [Start]
*--- laSetups[9,2] = Matrial Costing Method.
*--- Check If Costing method Is
*--- Lot|LIFO|FIFO ~ L|I|F.
*--- Doc. [End]
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFTKTCLS.SCX') WITH loFormSet.lcPosHdr,loFormSet.lcTranType,lcClsTitle TO ldClsDate
loCallingForm = loFormSet
ldClsDate = {}
=gfCallForm('MFTKTCLS',.F.,'loCallingForm.lcPosHdr,loCallingForm.lcTranType,lcClsTitle',"ldClsDate")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
IF EMPTY(ldClsDate)
RETURN
ENDIF

lcGlYear   = loFormSet.lcGlYear
lcGlPeriod = loFormSet.lcGlPeriod
IF !CHECKPRD(ldClsDate,'lcGlYear','lcGlPeriod','  ')
RETURN .F.
ENDIF

*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
llApVendor = (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)
IF llApVendor
llCloseTr = .F.
PRIVATE lcMfgGlAcnt
lcMfgGlAcnt = loFormSet.lcMfgGlAcnt
LOCAL ARRAY laMfgRFld[7,2]
=ACOPY(loFormSet.laMfgRFld,laMfgRFld)
m.cIMTyp = loFormSet.lcTranType
m.cTktNo = EVALUATE(loFormSet.lcPosHdr+'.PO')
IF !lfOpenSql('BOMCOST','BOMCOST','POBOMCLS','CIMTYP+CTKTNO',loFormSet)
RETURN .F.
ENDIF
SELECT BOMCOST
LOCATE
IF FOUND()
SCAN
IF Actualize = "N" AND ;
(INLIST(CCOSTTYPE,'P','D') OR ;
(cCostType="M" AND gfRltFld(MfgCode,@laMfgRFld,'MFGCODE') .AND. EMPTY(lcMfgGlAcnt)))
llCloseTr = .T.
EXIT
ENDIF
ENDSCAN
ENDIF
IF llCloseTr
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMsgCl = LANG_MFCSSH_CLSMSG1 + ' ' + LANG_MFCSSH_CLSMSG2
lcMsgCl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
LANG_MFCSSH_CLSMSG1,loFormSet.GetHeaderText("LANG_MFCSSH_CLSMSG1",loFormSet.HeaderAlias)) + ' ' +;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CLSMSG2,loFormSet.GetHeaderText("LANG_MFCSSH_CLSMSG2",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]
IF gfModalGen('QRM00000B00006','DIALOG',.F., .F., lcMsgCl) = 2
RETURN
ENDIF
ENDIF
=lfContrbut(loFormSet.lcTranType,EVALUATE(loFormSet.lcPosHdr+'.PO'),loFormSet)
ENDIF
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]
loFormSet.lcGlYear   = lcGlYear
loFormSet.lcGlPeriod = lcGlPeriod

WAIT WINDOW lcClsTitle NOWAIT
IF loFormSet.lcTranType <> "T"
=gfOpenFile(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
ENDIF

STORE 0 TO lnTActCst,lnTEstCst

*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
*=lfUpdWip(loFormSet)
=lfUpdWip(loFormSet)
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]
*! B612660,1 MMT 02/20/2023 PO closing gives error due to duplication in BOMLINE[T20230111.0001][Start]
lnLineNoVal = 1
SELECT MAX(NLINENO) FROM  (loFormSet.lcBOMLINE) INTO ARRAY laLinNo
IF _Tally > 0
  lnLineNoVal  = IIF(ISNULL(laLinNo[1]),1,laLinNo[1])
ENDIF
*! B612660,1 MMT 02/20/2023 PO closing gives error due to duplication in BOMLINE[T20230111.0001][End]
SELECT (loFormSet.lcBOMLINE)
DELETE FOR cType = '2' AND EMPTY(cRSession)

m.cIMTyp = loFormSet.lcTranType
m.cTktNo = EVALUATE(loFormSet.lcPosHdr+'.PO')
IF !lfOpenSql('BOMCOST','BOMCOST','POBOMCLS','CIMTYP+CTKTNO',loFormSet)
RETURN .F.
ENDIF

SELECT BOMCOST
=lfSetIndex('BOMCOST','POBOMCLS','CIMTYP+CTKTNO+ACTUALIZE+CBOMTYPE+CINVTYPE+ITEM+MFGCODE+COPRCODE+CLOTNO+CISESSION+CRSESSION')
LOCATE FOR Actualize = 'N'
IF FOUND()
DO WHILE cimtyp+ctktno+actualize+cbomtype+cInvType+item+mfgcode+coprcode+;
clotno+cisession+crsession=loFormSet.lcTranType+m.cTktNo+'N' .AND. !EOF()
SELECT BOMCOST
STORE 0 TO lnItActCst,lnItActQty,lnItEstCst,lnItEstQty
lcItem  = cBomType+cInvType+Item+MfgCode

*--- DOC.
*--- Sum to get the total cost and total qty for the actualizes
*--- DOC.
SUM REST nTotCst,nTotQty TO lnItActCst, lnItActQTy ;
WHILE cimtyp+ctktno+actualize+cbomtype+cInvType+item+mfgcode+coprcode+;
clotno+cisession+crsession=loFormSet.lcTranType+m.cTktNo+'N'+lcItem
SELECT (loFormSet.lcBOMLINE)
*--- DOC.
*--- Sum The estimated cost and estimated qty for revieved line
*--- DOC.

*B608731,1 WAM 10/27/2008 Fix actual cost calculations while close CT
*SUM FOR cType+cBomTyp+cInvTypC+Item+MfgCode = '2'+lcItem
SUM ItemAmt,ItemQty TO lnItEstCst, lnItEstQty FOR cType+cBomTyp+cInvTypC+Item+MfgCode = '2'+lcItem
*B608731,1 WAM 10/27/2008 (End)

lnTActCst = lnTActCst + lnItActCst
lnTEstCst = lnTEstCst + lnItEstCst

SCAN FOR cType+cBomTyp+cInvTypC+Item+MfgCode = '2'+lcItem
lnItemAmnt = ITEMAMT
lnItemQty  = ItemQty
SCATTER MEMVAR
m.Ctype    = '3'
m.ItemAmt  = IIF(lnItEstCst=0,0,lnItemAmnt/lnItEstCst*lnItActCst)
m.ItemQty  = IIF(lnITEstQty=0,0,lnItemQty/lnITEstQty*lnITActQty)
m.UnitCost = IIF(m.ItemQty=0,0,;
IIF(lnItEstCst=0,0,lnItemAmnt/lnItEstCst*lnItActCst)/m.ItemQty)
m.UnitQty  = IIF(StyQty=0,0,m.ItemQty/StyQty)
*! B612660,1 MMT 02/20/2023 PO closing gives error due to duplication in BOMLINE[T20230111.0001][Start]
lnLineNoVal = lnLineNoVal + 1 
m.NLineNo = lnLineNoVal 
*! B612660,1 MMT 02/20/2023 PO closing gives error due to duplication in BOMLINE[T20230111.0001][End]
*B608731,1 WAM 10/27/2008 Fix actual cost calculations while close CT
m.Rec_No = ''
*B608731,1 WAM 10/27/2008 (End)

SELECT (loFormSet.lcPosLn)
LOCATE FOR cInvType+Style+STR(LineNo,6) = m.cInvType+m.Style+STR(m.LineNo,6)
IF FOUND()
COUNT TO lnNoOfRec FOR cInvType+Style+STR(LineNo,6) = m.cInvType+m.Style+STR(m.LineNo,6) AND cRSession = m.cRSession
SCAN FOR cInvType+Style+STR(LineNo,6) = m.cInvType+m.Style+STR(m.LineNo,6) AND cRSession = m.cRSession
REPLACE ('NACT_COST'+m.cBomTyp) WITH  EVALUATE('NACT_COST'+m.cBomTyp) + (m.UnitCost * m.UnitQty / lnNoOfRec)
ENDSCAN
ENDIF

lnRecNo = RECNO(loFormSet.lcBOMLINE)
INSERT INTO (loFormSet.lcBOMLINE) FROM MEMVAR

* B608833,1 HES 04/06/2009 Add Standared Fields for BOMLINE File[T20081001.0001]
=gfAdd_Info(loFormSet.lcBOMLINE)
* B608833,1 HES 04/06/2009 Add Standared Fields for BOMLINE File[T20081001.0001]

GO lnRecNo IN (loFormSet.lcBOMLINE)
ENDSCAN
SELECT BOMCOST
ENDDO
ENDIF

*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
*B607883,1 MMT 12/18/2006 Fix bug of not updating History usage after close [Start]
*!*  SELECT BOMCOST
*!*  =SEEK(loFormSet.lcTranType+m.cTktNo)
*!*  SCAN REST WHILE cimtyp+ctktno+actualize+cbomtype+cInvType+item+mfgcode+coprcode+;
*!*             clotno+cisession+crsession=loFormSet.lcTranType+m.cTktNo
*!*    m.CINVTYPE  = BOMCOST.CINVTYPE
*!*    m.Style     = BOMCOST.item
*!*    m.CWARECODE = BOMCOST.CWARECODE
*!*    m.Dyelot    = BOMCOST.Cdyelot
*!*
*!*
*!*    IF SEEK(m.CINVTYPE+m.Style,'FABRIC') OR lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loFormSet)
*!*      SELECT FABRIC
*!*      REPLACE NTOTCUSA   WITH NTOTCUSA -  BOMCOST.ntotqty
*!*      *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
*!*      *,;
*!*   	 	        NTOTHUSAGE WITH NTOTHUSAGE + BOMCOST.ntotqty
*!*      *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]
*!*
*!*    ENDIF
*!*
*!*    IF SEEK(m.CINVTYPE+m.Style+m.CWARECODE+m.Dyelot,'FABDYE') OR lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+Dyelot',loFormSet)
*!*      SELECT FABDYE
*!*      REPLACE NCUSAGE1 WITH NCUSAGE1 - BOMCOST.ntotqty,;
*!*      	    	NCUSAGE2 WITH 0 ,;
*!*      	 	    NCUSAGE3 WITH 0 ,;
*!*  		        NCUSAGE4 WITH 0 ,;
*!*      		    NCUSAGE5 WITH 0 ,;
*!*  		        NCUSAGE6 WITH 0 ,;
*!*      		    NCUSAGE7 WITH 0 ,;
*!*  		        NCUSAGE8 WITH 0 ,;
*!*      		    NTOTCUSA WITH NTOTCUSA -  BOMCOST.ntotqty

*!*      *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
*!*      * REPLACE NHUSAGE1   WITH NHUSAGE1 + BOMCOST.ntotqty,;
*!*     		       NHUSAGE2   WITH 0 ,;
*!*       		     NHUSAGE3   WITH 0 ,;
*!*      		     NHUSAGE4   WITH 0 ,;
*!*      		     NHUSAGE5   WITH 0 ,;
*!*      		     NHUSAGE6   WITH 0 ,;
*!*      		     NHUSAGE7   WITH 0 ,;
*!*    		   	   NHUSAGE8 	WITH 0 ,;
*!*  		         NTOTHUSAGE WITH NTOTHUSAGE + BOMCOST.ntotqty
*!*      *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]
*!*
*!*    ENDIF
*!*  ENDSCAN
*!*  DECLARE laTableUpdate[2,2]
*!*  laTableUpdate[1,1] = 'FABRIC'
*!*  laTableUpdate[1,2] = 'ITEM'

*!*  laTableUpdate[2,1] = 'FABDYE'
*!*  laTableUpdate[2,2] = 'ITEMLOC'
*!*  =lfTableUpdate(loFormSet)
*B607883,1 MMT 12/18/2006 Fix bug of not updating History usage after close [End]
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]
*! B609992,1 MMT 07/09/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013][Start]
IF loFormSet.lcTranType = 'I' AND !USED('Shpmthdr')
=gfOpenTable('Shpmthdr','Shpmthdr','SH')
ENDIF
*! B609992,1 MMT 07/09/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013][End]
LOCAL llFound,lcTmpFabDye
lcTmpFabDye = gfTempName()
SELECT * FROM (loFormSet.lcPOSLN) INTO CURSOR (loFormSet.lcisslogfile) ORDER BY LineNo
GO TOP
STORE 0 TO lnCanceled
DO WHILE !EOF()
SCATTER MEMVAR
*--- DOC.
*--- Get the open qty
*--- DOC.

SUM REST IIF(TranCd='1',Qty1,-IIF(TranCd='3',0,Qty1)),;
IIF(TranCd='1',Qty2,-IIF(TranCd='3',0,Qty2)),;
IIF(TranCd='1',Qty3,-IIF(TranCd='3',0,Qty3)),;
IIF(TranCd='1',Qty4,-IIF(TranCd='3',0,Qty4)),;
IIF(TranCd='1',Qty5,-IIF(TranCd='3',0,Qty5)),;
IIF(TranCd='1',Qty6,-IIF(TranCd='3',0,Qty6)),;
IIF(TranCd='1',Qty7,-IIF(TranCd='3',0,Qty7)),;
IIF(TranCd='1',Qty8,-IIF(TranCd='3',0,Qty8)) ;
TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8;
WHILE STR(LineNo,6) = STR(m.LineNo,6)

m.Qty1  = MAX(m.Qty1,0)
m.Qty2  = MAX(m.Qty2,0)
m.Qty3  = MAX(m.Qty3,0)
m.Qty4  = MAX(m.Qty4,0)
m.Qty5  = MAX(m.Qty5,0)
m.Qty6  = MAX(m.Qty6,0)
m.Qty7  = MAX(m.Qty7,0)
m.Qty8  = MAX(m.Qty8,0)
m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
IF m.TotQty > 0
llFound = .F.
IF !EMPTY(m.Dyelot)
IF m.cInvType = '0001'
llFound = SEEK(m.Style+m.cWareCode+m.Dyelot,'StyDye')
SELECT StyDye
ELSE
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loFormSet)
SELECT FABDYE
LOCATE
llFound = !EOF()
IF llFound
IF !USED(lcTmpFabDye)
=AFIELDS(laFileStru)
=gfCrtTmp(lcTmpFabDye,@laFileStru,"cInvType+Style",lcTmpFabDye)
=CURSORSETPROP("Buffering",5,lcTmpFabDye)
ENDIF
SELECT (lcTmpFabDye)
IF !SEEK(m.cInvType+m.Style)
SELECT FABDYE
SCATTER TO laFabDyeRec
SELECT (lcTmpFabDye)
APPEND BLANK
GATHER FROM laFabDyeRec
*! N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[Start]
=TABLEUPDATE(.F.,.t.)
*! N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[End]
ENDIF
ENDIF
ENDIF
ENDIF
IF llFound
=RLOCK()
REPLACE WIP1   WITH MAX(WIP1 - m.Qty1,0)   ,;
WIP2   WITH MAX(WIP2 - m.Qty2,0)   ,;
WIP3   WITH MAX(WIP3 - m.Qty3,0)   ,;
WIP4   WITH MAX(WIP4 - m.Qty4,0)   ,;
WIP5   WITH MAX(WIP5 - m.Qty5,0)   ,;
WIP6   WITH MAX(WIP6 - m.Qty6,0)   ,;
WIP7   WITH MAX(WIP7 - m.Qty7,0)   ,;
WIP8   WITH MAX(WIP8 - m.Qty8,0)   ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
UNLOCK
ENDIF
ENDIF
llFound = .F.
IF m.cInvType = '0001'
llFound = SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
SELECT StyDye
ELSE
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE',loFormSet)
SELECT FABDYE
LOCATE FOR Dyelot = SPACE(10)
llFound = !EOF()
IF llFound
IF !USED(lcTmpFabDye)
=AFIELDS(laFileStru)
=gfCrtTmp(lcTmpFabDye,@laFileStru,"cInvType+Style",lcTmpFabDye)
=CURSORSETPROP("Buffering",5,lcTmpFabDye)
ENDIF
SELECT (lcTmpFabDye)
IF !SEEK(m.cInvType+m.Style)
SELECT FABDYE
SCATTER TO laFabDyeRec
SELECT (lcTmpFabDye)
APPEND BLANK
GATHER FROM laFabDyeRec
*! N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[Start]
=TABLEUPDATE(.F.,.t.)
*! N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[End]
ENDIF
ENDIF
ENDIF
ENDIF
IF llFound
=RLOCK()
REPLACE WIP1   WITH MAX(WIP1 - m.Qty1,0)   ,;
WIP2   WITH MAX(WIP2 - m.Qty2,0)   ,;
WIP3   WITH MAX(WIP3 - m.Qty3,0)   ,;
WIP4   WITH MAX(WIP4 - m.Qty4,0)   ,;
WIP5   WITH MAX(WIP5 - m.Qty5,0)   ,;
WIP6   WITH MAX(WIP6 - m.Qty6,0)   ,;
WIP7   WITH MAX(WIP7 - m.Qty7,0)   ,;
WIP8   WITH MAX(WIP8 - m.Qty8,0)   ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
UNLOCK
ENDIF
IF m.cInvType = '0001' AND SEEK(m.Style,'Style')
SELECT Style
=RLOCK()
REPLACE WIP1   WITH MAX(WIP1 - m.Qty1,0)   ,;
WIP2   WITH MAX(WIP2 - m.Qty2,0)   ,;
WIP3   WITH MAX(WIP3 - m.Qty3,0)   ,;
WIP4   WITH MAX(WIP4 - m.Qty4,0)   ,;
WIP5   WITH MAX(WIP5 - m.Qty5,0)   ,;
WIP6   WITH MAX(WIP6 - m.Qty6,0)   ,;
WIP7   WITH MAX(WIP7 - m.Qty7,0)   ,;
WIP8   WITH MAX(WIP8 - m.Qty8,0)   ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
UNLOCK
ENDIF
m.TranCd    = '5'
m.cRSession = loFormSet.lcSession
m.Date      = ldClsDate
m.DPostDate = ldClsDate
lnCanceled  = lnCanceled + m.TotQty
INSERT INTO (loFormSet.lcPosLn) FROM MEMVAR
ENDIF
SELECT (loFormSet.lcisslogfile)
ENDDO
USE IN (loFormSet.lcisslogfile)
*! B609992,1 MMT 07/09/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013][Start]
IF loFormSet.lcTranType = 'I'
lcSetDelete = SET("Deleted")
SET DELETED OFF
SELECT (loFormSet.lcPOSLN)
llShphdrUp = .F.
SCAN FOR trancd   ='3'
=gfSeek('PP'+EVALUATE(loFormSet.lcPOSLN+'.SHIPNO'),'SHPMTHDR')
lnCanQty = EVALUATE(loFormSet.lcPOSLN+'.TOTQTY')
SELECT SHPMTHDR
=gfReplace('RECV_CAN with RECV_CAN + lnCanQty')
*! B609992,2 MMT 07/16/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013][Start]
*=gfReplace('Status WITH IIF(TOTQTYHDR - RECV_CAN- RECV_DAM - RECV_STK  = 0,"C",Status)')
*! B609992,2 MMT 07/16/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013][End]
llShphdrUp = .T.
SELECT (loFormSet.lcPOSLN)
IF SEEK(EVALUATE(loFormSet.lcPOSLN+'.Style')+EVALUATE(loFormSet.lcPOSLN+'.cWareCode')+SPACE(10),'StyDye')
SELECT StyDye
=RLOCK()
REPLACE intrans1   WITH MAX(intrans1 - EVALUATE(loFormSet.lcPOSLN+'.Qty1'),0)   ,;
intrans2   WITH MAX(intrans2 - EVALUATE(loFormSet.lcPOSLN+'.Qty2'),0)   ,;
intrans3   WITH MAX(intrans3 - EVALUATE(loFormSet.lcPOSLN+'.Qty3'),0)   ,;
intrans4   WITH MAX(intrans4 - EVALUATE(loFormSet.lcPOSLN+'.Qty4'),0)   ,;
intrans5   WITH MAX(intrans5 - EVALUATE(loFormSet.lcPOSLN+'.Qty5'),0)   ,;
intrans6   WITH MAX(intrans6 - EVALUATE(loFormSet.lcPOSLN+'.Qty6'),0)   ,;
intrans7   WITH MAX(intrans7 - EVALUATE(loFormSet.lcPOSLN+'.Qty7'),0)   ,;
intrans8   WITH MAX(intrans8 - EVALUATE(loFormSet.lcPOSLN+'.Qty8'),0)   ,;
totintrn   WITH intrans1+intrans2+intrans3+intrans4+intrans5+intrans6+intrans7+intrans8
UNLOCK
ENDIF
IF SEEK(EVALUATE(loFormSet.lcPOSLN+'.Style'),'Style')
SELECT Style
=RLOCK()
REPLACE intrans1   WITH MAX(intrans1 - EVALUATE(loFormSet.lcPOSLN+'.Qty1'),0)   ,;
intrans2   WITH MAX(intrans2 - EVALUATE(loFormSet.lcPOSLN+'.Qty2'),0)   ,;
intrans3   WITH MAX(intrans3 - EVALUATE(loFormSet.lcPOSLN+'.Qty3'),0)   ,;
intrans4   WITH MAX(intrans4 - EVALUATE(loFormSet.lcPOSLN+'.Qty4'),0)   ,;
intrans5   WITH MAX(intrans5 - EVALUATE(loFormSet.lcPOSLN+'.Qty5'),0)   ,;
intrans6   WITH MAX(intrans6 - EVALUATE(loFormSet.lcPOSLN+'.Qty6'),0)   ,;
intrans7   WITH MAX(intrans7 - EVALUATE(loFormSet.lcPOSLN+'.Qty7'),0)   ,;
intrans8   WITH MAX(intrans8 - EVALUATE(loFormSet.lcPOSLN+'.Qty8'),0)   ,;
totintrn   WITH intrans1+intrans2+intrans3+intrans4+intrans5+intrans6+intrans7+intrans8
UNLOCK
ENDIF
SELECT (loFormSet.lcPOSLN)
*! B609992,2 MMT 07/16/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013][Start]
lnCurRecNum = RECNO()
lcShpCode = EVALUATE(loFormSet.lcPOSLN+'.SHIPNO')
*! B609992,2 MMT 07/16/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013][end]
DELETE
*! B609992,2 MMT 07/16/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013][Start]
LOCATE FOR ShipNo = lcShpCode AND TranCd = '3' AND TotQty > 0  AND !DELETED()
IF !FOUND()
SELECT SHPMTHDR
=gfReplace('Status WITH "C"')
ENDIF
SELECT (loFormSet.lcPOSLN)
IF BETWEEN(lnCurRecNum,1,RECCOUNT())
GO RECORD lnCurRecNum
ENDIF
*! B609992,2 MMT 07/16/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013][end]
ENDSCAN
SET DELETED &lcSetDelete.
SELECT SHPMTHDR
=gfTableUpdate()
ENDIF
*! B609992,1 MMT 07/09/2012 Closing PO cost sheet deos not complete the related PO shipment[T20120517.0013][END]
SELECT (loFormSet.lcPosHdr)
REPLACE Status WITH 'S',;
Cancel WITH Cancel + lnCanceled,;
Open   WITH 0

*--- Doc. [Begin]
*--- At the next part after IF laSetups[1,2]='Y' (Link to GL)
*--- We will start to update the GlDist file as following :
*--- for each one of the five cost element
*--- if there exist Differences between Actual and landed cost
*--- we will go to insert Gl_Interies with the Deferent
*--- Ex :- IF Actual = 12 , Landed = 10 ==> Gl-Interies = 2.
*--- And we will use the following Category Key for each one of the costing
*--- '022' , '023' , '024' , '025' , '026' , '027' , '028'
*--- Except for 'Material Manufucturing Order' we will use Category '021'.
*--- if the Gl_Link.CatgDesc empty for any one of the above Keys
*--- we will use the key '019'  as default one.
*--- Doc. [End]

IF (loFormSet.laSetups[1,2]='Y')
STORE '' TO m.cWipAcnt,m.cVarAcnt1,m.cVarAcnt2,m.cVarAcnt3,m.cVarAcnt4,m.cVarAcnt5,m.cVarAcnt6,m.cVarAcnt7
SET ORDER TO TAG GL_LINK IN GL_LINK

*!*    *C200449,1 ALB Add GL entries when closing Non GL operations [Begin]
*!*    IF ASCAN(laEvntTrig , PADR('UPDTCLGL',10)) <> 0
*!*      =gfDoTriger('POCSSH',PADR('UPDTCLGL',10))
*!*    ENDIF
*!*    *C200449,1 ALB Add GL entries when closing Non GL operations [end]

IF loFormSet.lcTranType = 'T'
lnLanded = NLAN_COST1+NLAN_COST2+NLAN_COST3+NLAN_COST4+NLAN_COST5+NLAN_COST6+NLAN_COST7
lnActual = NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4+NACT_COST5+NACT_COST6+NACT_COST7
IF lnLanded <> lnActual
DO GlDist WITH LINK_CODE,'013',(lnLanded-lnActual),'MC',;
PO,ldClsDate,loFormSet.lcGlYear,loFormSet.lcGlPeriod,loFormSet.lcGlDTemp
SELECT (loFormSet.lcPosHdr)
m.cWipAcnt = EVALUATE(loFormSet.lcGlDTemp+'.GlAccount')
DO GlDist WITH LINK_CODE,'021',(lnActual-lnLanded),'MC',;
PO,ldClsDate,loFormSet.lcGlYear,loFormSet.lcGlPeriod,loFormSet.lcGlDTemp
SELECT (loFormSet.lcPosHdr)
m.cVarAcnt1 = EVALUATE(loFormSet.lcGlDTemp+'.GlAccount')
ENDIF
ELSE
PRIVATE lcGLTrnTyp
lcGLTrnTyp = IIF(loFormSet.lcTranType$'MD','JP','JC')

LOCAL lnI,lcI,lcGlCatg
FOR lnI = 1 TO 7
lcI = STR(lnI,1)

*! B608402,1 SSH 11/29/2007 Fix some error in while adding cost elment 6,7
*lcGlCatg = PADL(STR(21+lnI,3,0),3,'0')
lcGlCatg = PADL(ALLTRIM(STR(21+lnI,3)),3,'0')
*! B608402,1 SSH 11/29/2007 Fix some error in while adding cost elment 6,7

IF EVALUATE('NACT_COST'+lcI) <> EVALUATE('NLAN_COST'+lcI)
DO GlDist WITH LINK_CODE,'013',(EVALUATE('NLAN_COST'+lcI)-EVALUATE('NACT_COST'+lcI)),lcGLTrnTyp,;
PO,ldClsDate,loFormSet.lcGlYear,loFormSet.lcGlPeriod,loFormSet.lcGlDTemp
SELECT (loFormSet.lcPosHdr)
m.cWipAcnt = EVALUATE(loFormSet.lcGlDTemp+'.GlAccount')
=SEEK(LINK_CODE+lcGlCatg,'GL_LINK')
DO GlDist WITH LINK_CODE,IIF(EMPTY(GL_LINK.GlAcnt),'019',lcGlCatg),;
(EVALUATE('NACT_COST'+lcI)-EVALUATE('NLAN_COST'+lcI)),lcGLTrnTyp,PO,ldClsDate,;
loFormSet.lcGlYear,loFormSet.lcGlPeriod,loFormSet.lcGlDTemp
SELECT (loFormSet.lcPosHdr)
m.cVarAcnt&lcI. = EVALUATE(loFormSet.lcGlDTemp+'.GlAccount')
ENDIF
ENDFOR
ENDIF

SELECT (loFormSet.lcPOSHDR)
REPLACE cWipAcnt  WITH m.cWipAcnt  ,;
cVarAcnt1 WITH m.cVarAcnt1 ,;
cVarAcnt2 WITH m.cVarAcnt2 ,;
cVarAcnt3 WITH m.cVarAcnt3 ,;
cVarAcnt4 WITH m.cVarAcnt4 ,;
cVarAcnt5 WITH m.cVarAcnt5 ,;
cVarAcnt6 WITH m.cVarAcnt6 ,;
cVarAcnt7 WITH m.cVarAcnt7
SELECT (loFormSet.lcGlDTemp)
*! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
*REPLACE ALL GLSESSION WITH loFormSet.lcSession
lcGLDISTCursorUpdate = gfGetRemoteProp('lcCursorUpdate','GLDIST')
*! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
*E303957,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]
SELECT (loFormSet.lcGlDTemp)
SCAN 
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  REPLACE GLSESSION WITH loFormSet.lcSession
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]  
  SCATTER MEMVAR MEMO
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  *m.Oid = gfTempName()
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]  
  SELECT GLDIST
  *!*	APPEND FROM (oAriaApplication.WorkDir+loParentForm.lcGlDTemp)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  *=gfReplace('')
  IF !EMPTY(lcGLDISTCursorUpdate)
    SELECT (lcGLDISTCursorUpdate)
    APPEND BLANK
    GATHER MEMVAR MEMO 
  ENDIF
  *! E303957,2 MMT 1/02/2019 use the GLDIST table remotely not native, because of conversion to SQL [End]
ENDSCAN
SELECT GLDIST
=gfTableUpdate()
*E303957,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [End]
*! E611847,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
*! B612660,1 MMT 02/20/2023 PO closing gives error due to duplication in BOMLINE[T20230111.0001][Start]
*=gfCreateGLEntries(loParentForm.lcGlDTemp,'')
=gfCreateGLEntries(loFormSet.lcGlDTemp,'')
*! B612660,1 MMT 02/20/2023 PO closing gives error due to duplication in BOMLINE[T20230111.0001][End]
*! E611847,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]

SELECT (loFormSet.lcGlDTemp)
ZAP
SET ORDER TO TAG GL_LINK1 IN GL_LINK
ENDIF

DECLARE laTableUpdate[3,2]
laTableUpdate[1,1] = loFormSet.lcBomLine
laTableUpdate[1,2] = 'BOMLINE'

laTableUpdate[2,1] = loFormSet.lcPosHdr
laTableUpdate[2,2] = 'POSHDR'

laTableUpdate[3,1] = loFormSet.lcPosLn
laTableUpdate[3,2] = 'POSLN'


IF USED(lcTmpFabDye)
DECLARE laTableUpdate[4,2]
laTableUpdate[4,1] = lcTmpFabDye
laTableUpdate[4,2] = 'ITEMLOC'
ENDIF

=lfTableUpdate(loFormSet)

WAIT CLEAR
loFormSet.ChangeMode('V')
oariaapplication.otoolbar.ButtonRefresh()
oariaapplication.otoolbar.NavRefresh()

*!*************************************************************
*! Name      : lfUpdWip
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/01/2004
*! Purpose   : Function to update the NTOTCUSA field in ITEMLOC file.
*!*************************************************************
FUNCTION lfUpdWip
LPARAMETERS loFormSet
LOCAL lnAlias,lcTmpWip,lcTmpWip1
lnAlias = SELECT(0)
lcTmpWip  = gfTempName()
lcTmpWip1 = gfTempName()

LOCAL ARRAY laFileStru[12,18]
laFileStru[01,1] = 'CWARECODE'
laFileStru[01,2] = 'C'
laFileStru[01,3] = 6
laFileStru[01,4] = 0

laFileStru[02,1] = 'NCUSAGE1'
laFileStru[02,2] = 'N'
laFileStru[02,3] = 12
laFileStru[02,4] = 3

laFileStru[03,1] = 'NCUSAGE2'
laFileStru[03,2] = 'N'
laFileStru[03,3] = 12
laFileStru[03,4] = 3

laFileStru[04,1] = 'NCUSAGE3'
laFileStru[04,2] = 'N'
laFileStru[04,3] = 12
laFileStru[04,4] = 3

laFileStru[05,1] = 'NCUSAGE4'
laFileStru[05,2] = 'N'
laFileStru[05,3] = 12
laFileStru[05,4] = 3

laFileStru[06,1] = 'NCUSAGE5'
laFileStru[06,2] = 'N'
laFileStru[06,3] = 12
laFileStru[06,4] = 3

laFileStru[07,1] = 'NCUSAGE6'
laFileStru[07,2] = 'N'
laFileStru[07,3] = 12
laFileStru[07,4] = 3

laFileStru[08,1] = 'NCUSAGE7'
laFileStru[08,2] = 'N'
laFileStru[08,3] = 12
laFileStru[08,4] = 3

laFileStru[09,1] = 'NCUSAGE8'
laFileStru[09,2] = 'N'
laFileStru[09,3] = 12
laFileStru[09,4] = 3

laFileStru[10,1] = 'NTOTCUSA'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 13
laFileStru[10,4] = 3

laFileStru[11,1] = 'DTRDATE'
laFileStru[11,2] = 'D'
laFileStru[11,3] = 8
laFileStru[11,4] = 0

laFileStru[12,1] = 'CISESSION'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 6
laFileStru[12,4] = 0

LOCAL lnI,lnJ
FOR lnI = 1 TO 12
FOR lnJ = 7 TO 16
laFileStru[lnI,lnJ] = ''
ENDFOR
STORE 0 TO laFileStru[lnI,17],laFileStru[lnI,18]
ENDFOR

LOCAL ARRAY laIndex[2,2]
laIndex[1,1] = 'DTOS(DTRDATE)+CWARECODE'
laIndex[1,2] = lcTmpWip
laIndex[2,1] = 'CWARECODE+CISESSION'
laIndex[2,2] = lcTmpWip1
=gfCrtTmp(lcTmpWip,@laFileStru,@laIndex)
SET ORDER TO TAG (lcTmpWip) IN (lcTmpWip)

LOCAL lcBomLine,lcMatInvJl
lcBomLine  = gfTempName()
lcMatInvJl = gfTempName()
m.cImTyp   = loFormSet.lcTranType
m.cType    = '2'
m.CtktNo   = EVALUATE(loFormSet.lcPosHdr+'.PO')
IF !lfOpenSql('BOMLINE',lcBomLine,'BOMPOREC','CIMTYP+CTYPE+CTKTNO',loFormSet)
RETURN .F.
ENDIF

LOCAL lnRecQty
SELECT (loFormSet.lcCTKTBOM)
SCAN FOR CCATGTYP $ 'FT'
lnRecQty = 0
SELECT (lcBOMLINE)
SUM FOR CINVTYPC = EVALUATE(loFormSet.lcCtktBom+'.cInvType') AND ITEM = EVALUATE(loFormSet.lcCTKTBOM+'.ITEM');
AND DYELOT = EVALUATE(loFormSet.lcCTKTBOM+'.DYELOT') AND !EMPTY(CRSESSION) ITEMQTY TO lnRecQty
SELECT (lcTmpWip)
ZAP
m.cInvType = EVALUATE(loFormSet.lcCtktbom+'.cInvType')
m.Style    = EVALUATE(loFormSet.lcCtktBom+'.Item')
IF lfOpenSql('ITEMJRNL',lcMatInvJl,'STYINVJL','CINVTYPE+STYLE',loFormSet)
SELECT (lcMatInvJl)
LOCATE
IF !EOF()
**: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
*SCAN FOR CDYELOT  = EVALUATE(loFormSet.lcCTKTBOM+'.DYELOT') AND CTRTYPE = '9' AND;
CBUSDOCU = EVALUATE(loFormSet.lcPosHdr+'.CBUSDOCU') AND;
CSTYTYPE = EVALUATE(loFormSet.lcPosHdr+'.CSTYTYPE') AND;
CTKTNO   = EVALUATE(loFormSet.lcCTKTBOM+'.CUTTKT')
SCAN FOR CDYELOT  = EVALUATE(loFormSet.lcCTKTBOM+'.DYELOT') AND CTRTYPE = '9' AND;
CBUSDOCU = EVALUATE(loFormSet.lcPosHdr+'.CBUSDOCU') AND;
CSTYTYPE = EVALUATE(loFormSet.lcPosHdr+'.CSTYTYPE') AND;
CTRCODE   = EVALUATE(loFormSet.lcCTKTBOM+'.CUTTKT')

IF !SEEK(EVALUATE(lcMATINVJL+'.CWARECODE')+EVALUATE(lcMATINVJL+'.CISESSION'),lcTmpWip,lcTmpWip1)
INSERT INTO (lcTmpWip) (CWARECODE,DTRDATE,CISESSION);
VALUES (EVALUATE(lcMATINVJL+'.CWARECODE'),EVALUATE(lcMATINVJL+'.DTRDATE'),EVALUATE(lcMATINVJL+'.CISESSION'))
ENDIF
REPLACE NCUSAGE1 WITH NCUSAGE1 - EVALUATE(lcMATINVJL+'.NSTK1'),;
NCUSAGE2 WITH NCUSAGE2 - EVALUATE(lcMATINVJL+'.NSTK2'),;
NCUSAGE3 WITH NCUSAGE3 - EVALUATE(lcMATINVJL+'.NSTK3'),;
NCUSAGE4 WITH NCUSAGE4 - EVALUATE(lcMATINVJL+'.NSTK4'),;
NCUSAGE5 WITH NCUSAGE5 - EVALUATE(lcMATINVJL+'.NSTK5'),;
NCUSAGE6 WITH NCUSAGE6 - EVALUATE(lcMATINVJL+'.NSTK6'),;
NCUSAGE7 WITH NCUSAGE7 - EVALUATE(lcMATINVJL+'.NSTK7'),;
NCUSAGE8 WITH NCUSAGE8 - EVALUATE(lcMATINVJL+'.NSTK8'),;
NTOTCUSA WITH NTOTCUSA - EVALUATE(lcMATINVJL+'.NTOTSTK') IN (lcTmpWip)
*!*	        IF CIRTYPE = 'I'
*!*	          INSERT INTO (lcTmpWip) (CWARECODE,NCUSAGE1,NCUSAGE2,NCUSAGE3,NCUSAGE4,NCUSAGE5,NCUSAGE6,NCUSAGE7,NCUSAGE8,;
*!*	                                  NTOTCUSA,DTRDATE,CISESSION);
*!*	                          VALUES (EVALUATE(lcMATINVJL+'.CWARECODE'),EVALUATE(lcMATINVJL+'.NSTK1'),;
*!*	                                  EVALUATE(lcMATINVJL+'.NSTK2'),EVALUATE(lcMATINVJL+'.NSTK3'),;
*!*	                                  EVALUATE(lcMATINVJL+'.NSTK4'),EVALUATE(lcMATINVJL+'.NSTK5'),;
*!*	                                  EVALUATE(lcMATINVJL+'.NSTK6'),EVALUATE(lcMATINVJL+'.NSTK7'),;
*!*	                                  EVALUATE(lcMATINVJL+'.NSTK8'),EVALUATE(lcMATINVJL+'.NTOTSTK'),;
*!*	                                  EVALUATE(lcMATINVJL+'.DTRDATE'),EVALUATE(lcMATINVJL+'.CISESSION'))
*!*	        ELSE
*!*	          SELECT (lcTmpWip)
*!*	          SET ORDER TO TAG (lcTmpWip1)
*!*	          IF SEEK(EVALUATE(lcMATINVJL+'.CWARECODE')+EVALUATE(lcMATINVJL+'.CISESSION'))
*!*	            REPLACE NCUSAGE1 WITH NCUSAGE1 + EVALUATE(lcMATINVJL+'.NSTK1'),;
*!*	                    NCUSAGE2 WITH NCUSAGE2 + EVALUATE(lcMATINVJL+'.NSTK2'),;
*!*	                    NCUSAGE3 WITH NCUSAGE3 + EVALUATE(lcMATINVJL+'.NSTK3'),;
*!*	                    NCUSAGE4 WITH NCUSAGE4 + EVALUATE(lcMATINVJL+'.NSTK4'),;
*!*	                    NCUSAGE5 WITH NCUSAGE5 + EVALUATE(lcMATINVJL+'.NSTK5'),;
*!*	                    NCUSAGE6 WITH NCUSAGE6 + EVALUATE(lcMATINVJL+'.NSTK6'),;
*!*	                    NCUSAGE7 WITH NCUSAGE7 + EVALUATE(lcMATINVJL+'.NSTK7'),;
*!*	                    NCUSAGE8 WITH NCUSAGE8 + EVALUATE(lcMATINVJL+'.NSTK8'),;
*!*	                    NTOTCUSA WITH NTOTCUSA + EVALUATE(lcMATINVJL+'.NTOTSTK')
*!*	          ELSE
*!*	            IF SEEK(EVALUATE(lcMATINVJL+'.CWARECODE'))
*!*	              lnRemender = EVALUATE(lcMATINVJL+'.NTOTSTK')
*!*	              lnRem1     = EVALUATE(lcMATINVJL+'.NSTK1')
*!*	              lnRem2     = EVALUATE(lcMATINVJL+'.NSTK2')
*!*	              lnRem3     = EVALUATE(lcMATINVJL+'.NSTK3')
*!*	              lnRem4     = EVALUATE(lcMATINVJL+'.NSTK4')
*!*	              lnRem5     = EVALUATE(lcMATINVJL+'.NSTK5')
*!*	              lnRem6     = EVALUATE(lcMATINVJL+'.NSTK6')
*!*	              lnRem7     = EVALUATE(lcMATINVJL+'.NSTK7')
*!*	              lnRem8     = EVALUATE(lcMATINVJL+'.NSTK8')
*!*	              SCAN REST WHILE CWARECODE+CISESSION = EVALUATE(lcMATINVJL+'.CWARECODE')
*!*	                lnTotCUsa = NTOTCUSA
*!*	                lnCUsage1 = NCUSAGE1
*!*	                lnCUsage2 = NCUSAGE2
*!*	                lnCUsage3 = NCUSAGE3
*!*	                lnCUsage4 = NCUSAGE4
*!*	                lnCUsage5 = NCUSAGE5
*!*	                lnCUsage6 = NCUSAGE6
*!*	                lnCUsage7 = NCUSAGE7
*!*	                lnCUsage8 = NCUSAGE8
*!*	                REPLACE NCUSAGE1 WITH MIN(NCUSAGE1+lnRem1,0),;
*!*	                        NCUSAGE2 WITH MIN(NCUSAGE2+lnRem2,0),;
*!*	                        NCUSAGE3 WITH MIN(NCUSAGE3+lnRem3,0),;
*!*	                        NCUSAGE4 WITH MIN(NCUSAGE4+lnRem4,0),;
*!*	                        NCUSAGE5 WITH MIN(NCUSAGE5+lnRem5,0),;
*!*	                        NCUSAGE6 WITH MIN(NCUSAGE6+lnRem6,0),;
*!*	                        NCUSAGE7 WITH MIN(NCUSAGE7+lnRem7,0),;
*!*	                        NCUSAGE8 WITH MIN(NCUSAGE8+lnRem8,0),;
*!*	                        NTOTCUSA WITH MIN(NTOTCUSA+lnRemender,0)
*!*	                lnRemender = MAX(lnRemender+lnTotCUsa,0)
*!*	                lnRem1     = MAX(lnRem1    +lnCUsage1,0)
*!*	                lnRem2     = MAX(lnRem2    +lnCUsage2,0)
*!*	                lnRem3     = MAX(lnRem3    +lnCUsage3,0)
*!*	                lnRem4     = MAX(lnRem4    +lnCUsage4,0)
*!*	                lnRem5     = MAX(lnRem5    +lnCUsage5,0)
*!*	                lnRem6     = MAX(lnRem6    +lnCUsage6,0)
*!*	                lnRem7     = MAX(lnRem7    +lnCUsage7,0)
*!*	                lnRem8     = MAX(lnRem8    +lnCUsage8,0)
*!*	                IF lnRemender <= 0
*!*	                  EXIT
*!*	                ENDIF
*!*	              ENDSCAN
*!*	            ENDIF
*!*	          ENDIF
*!*	          SET ORDER TO TAG (lcTmpWip)
*!*	        ENDIF
**: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
ENDSCAN
ENDIF
ENDIF
IF RECCOUNT(lcTmpWip) > 0
m.cInvType = EVALUATE(loFormSet.lcCtktBom+'.cInvType')
m.Style    = EVALUATE(loFormSet.lcCtktBom+'.Item')
=lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE',loFormSet)

*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
=lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loFormSet)
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]

SELECT (lcTmpWip)
LOCATE
lnRemender = lnRecQty
lnRemSize  = IIF(EVALUATE(loFormSet.lcCtktBom+'.Req_Qty')>0,lnRemender/EVALUATE(loFormSet.lcCtktBom+'.Req_Qty'),0)

*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
lnRemQtySt = lnRecQty
lnRemQty   = EVALUATE(loFormSet.lcCtktBom+'.Req_Qty')
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]
SCAN

*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
SELECT FABRIC
REPLACE NTOTCUSA WITH NTOTCUSA - MAX(MIN(EVALUATE(lcTmpWip+'.NTOTCUSA')-lnRemQtySt,lnRemQty),0)
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]

SELECT FABDYE
LOCATE FOR CWARECODE+DYELOT = EVALUATE(lcTmpWip+'.CWARECODE')+EVALUATE(loFormSet.lcCTKTBOM+'.DYELOT')
IF FOUND()

*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
*  REPLACE NTOTCUSA WITH NTOTCUSA + MIN(EVALUATE(lcTmpWip+'.NTOTCUSA')+lnRemender,0),;
NCUSAGE1 WITH NCUSAGE1 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE1')+lnRemSize,0),;
NCUSAGE2 WITH NCUSAGE2 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE2')+lnRemSize,0),;
NCUSAGE3 WITH NCUSAGE3 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE3')+lnRemSize,0),;
NCUSAGE4 WITH NCUSAGE4 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE4')+lnRemSize,0),;
NCUSAGE5 WITH NCUSAGE5 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE5')+lnRemSize,0),;
NCUSAGE6 WITH NCUSAGE6 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE6')+lnRemSize,0),;
NCUSAGE7 WITH NCUSAGE7 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE7')+lnRemSize,0),;
NCUSAGE8 WITH NCUSAGE8 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE8')+lnRemSize,0)

REPLACE  NTOTCUSA WITH NTOTCUSA - MAX(MIN(EVALUATE(lcTmpWip+'.NTOTCUSA')-lnRemQtySt,lnRemQty),0),;
NCUSAGE1 WITH NCUSAGE1 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE1')-lnRemQtySt,lnRemQty),0),;
NCUSAGE2 WITH NCUSAGE2 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE2')-lnRemQtySt,lnRemQty),0),;
NCUSAGE3 WITH NCUSAGE3 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE3')-lnRemQtySt,lnRemQty),0),;
NCUSAGE4 WITH NCUSAGE4 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE4')-lnRemQtySt,lnRemQty),0),;
NCUSAGE5 WITH NCUSAGE5 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE5') -lnRemQtySt,lnRemQty),0),;
NCUSAGE6 WITH NCUSAGE6 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE6')-lnRemQtySt,lnRemQty),0),;
NCUSAGE7 WITH NCUSAGE7 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE7')-lnRemQtySt,lnRemQty),0),;
NCUSAGE8 WITH NCUSAGE8 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE8')-lnRemQtySt,lnRemQty),0)

*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]
ENDIF
IF !EMPTY(EVALUATE(loFormSet.lcCTKTBOM+'.DYELOT'))
LOCATE FOR CWARECODE+DYELOT = EVALUATE(lcTmpWip+'.CWARECODE')+SPACE(10)
IF FOUND()
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
*  REPLACE NTOTCUSA WITH NTOTCUSA + MIN(EVALUATE(lcTmpWip+'.NTOTCUSA')+lnRemender,0),;
NCUSAGE1 WITH NCUSAGE1 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE1')+lnRemSize,0),;
NCUSAGE2 WITH NCUSAGE2 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE2')+lnRemSize,0),;
NCUSAGE3 WITH NCUSAGE3 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE3')+lnRemSize,0),;
NCUSAGE4 WITH NCUSAGE4 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE4')+lnRemSize,0),;
NCUSAGE5 WITH NCUSAGE5 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE5')+lnRemSize,0),;
NCUSAGE6 WITH NCUSAGE6 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE6')+lnRemSize,0),;
NCUSAGE7 WITH NCUSAGE7 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE7')+lnRemSize,0),;
NCUSAGE8 WITH NCUSAGE8 + MIN(EVALUATE(lcTmpWip+'.NCUSAGE8')+lnRemSize,0)
REPLACE NTOTCUSA WITH NTOTCUSA - MAX(MIN(EVALUATE(lcTmpWip+'.NTOTCUSA')-lnRemQtySt,lnRemQty),0),;
NCUSAGE1 WITH NCUSAGE1 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE1')-lnRemQtySt,lnRemQty),0),;
NCUSAGE2 WITH NCUSAGE2 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE2')-lnRemQtySt,lnRemQty),0),;
NCUSAGE3 WITH NCUSAGE3 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE3')-lnRemQtySt,lnRemQty),0),;
NCUSAGE4 WITH NCUSAGE4 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE4')-lnRemQtySt,lnRemQty),0),;
NCUSAGE5 WITH NCUSAGE5 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE5')-lnRemQtySt,lnRemQty),0),;
NCUSAGE6 WITH NCUSAGE6 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE6')-lnRemQtySt,lnRemQty),0),;
NCUSAGE7 WITH NCUSAGE7 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE7')-lnRemQtySt,lnRemQty),0),;
NCUSAGE8 WITH NCUSAGE8 - MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE8')-lnRemQtySt,lnRemQty),0)
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]
ENDIF
ENDIF

SELECT (lcTmpWip)

*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
*!*        lnRemender = MAX(lnRemender+NTOTCUSA,0)
*!*        lnRemSize  = IIF(EVALUATE(loFormSet.lcCtktBom+'.Req_Qty')>0,lnRemender/EVALUATE(loFormSet.lcCtktBom+'.Req_Qty'),0)
lnRemQty   = MIN(MAX(lnRemQty-EVALUATE(lcTmpWip+'.NCUSAGE1')+lnRemQtySt,0),lnRemQty)
lnRemQtySt = MAX(lnRemQtySt-EVALUATE(lcTmpWip+'.NCUSAGE1'),0)
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]
ENDSCAN

*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
*!*      DECLARE laTableUpdate[1,2]
*!*      laTableUpdate[1,1] = 'FABDYE'
*!*      laTableUpdate[1,2] = 'ITEMLOC'
DECLARE laTableUpdate[2,2]
laTableUpdate[1,1] = 'FABDYE'
laTableUpdate[1,2] = 'ITEMLOC'
laTableUpdate[2,1] = 'FABRIC'
laTableUpdate[2,2] = 'ITEM'
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]


=lfTableUpdate(loFormSet)
ENDIF
ENDSCAN
*! B608402,1 SSH 11/29/2007 Fix some error in while adding cost elment 6,7
IF USED(lcTmpWip)
USE IN (lcTmpWip)
ENDIF
IF USED(lcBomLine)
USE IN (lcBomLine)
ENDIF
IF USED(lcMatInvJl)
USE IN (lcMatInvJl)
ENDIF
*! B608402,1 SSH 11/29/2007 Fix some error in while adding cost elment 6,7

*!*	USE IN (lcTmpWip)
*!*	USE IN (lcBomLine)
*!*	USE IN (lcMatInvJl)

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfRetWip
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/01/2004
*! Purpose   : Function to update the NTOTCUSA field in ITEMLOC file when return.
*!*************************************************************
FUNCTION lfRetWip
LPARAMETERS lcInvType,lcStyle,lcWareCode,lcDyelot,lnIssued

LOCAL lnAlias,lcTmpWip,lcTmpWip1
lnAlias = SELECT(0)
lcTmpWip  = gfTempName()
lcTmpWip1 = gfTempName()

LOCAL ARRAY laFileStru[12,18]
laFileStru[01,1] = 'CWARECODE'
laFileStru[01,2] = 'C'
laFileStru[01,3] = 6
laFileStru[01,4] = 0

laFileStru[02,1] = 'NCUSAGE1'
laFileStru[02,2] = 'N'
laFileStru[02,3] = 12
laFileStru[02,4] = 3

laFileStru[03,1] = 'NCUSAGE2'
laFileStru[03,2] = 'N'
laFileStru[03,3] = 12
laFileStru[03,4] = 3

laFileStru[04,1] = 'NCUSAGE3'
laFileStru[04,2] = 'N'
laFileStru[04,3] = 12
laFileStru[04,4] = 3

laFileStru[05,1] = 'NCUSAGE4'
laFileStru[05,2] = 'N'
laFileStru[05,3] = 12
laFileStru[05,4] = 3

laFileStru[06,1] = 'NCUSAGE5'
laFileStru[06,2] = 'N'
laFileStru[06,3] = 12
laFileStru[06,4] = 3

laFileStru[07,1] = 'NCUSAGE6'
laFileStru[07,2] = 'N'
laFileStru[07,3] = 12
laFileStru[07,4] = 3

laFileStru[08,1] = 'NCUSAGE7'
laFileStru[08,2] = 'N'
laFileStru[08,3] = 12
laFileStru[08,4] = 3

laFileStru[09,1] = 'NCUSAGE8'
laFileStru[09,2] = 'N'
laFileStru[09,3] = 12
laFileStru[09,4] = 3

laFileStru[10,1] = 'NTOTCUSA'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 13
laFileStru[10,4] = 3

laFileStru[11,1] = 'DTRDATE'
laFileStru[11,2] = 'D'
laFileStru[11,3] = 8
laFileStru[11,4] = 0

laFileStru[12,1] = 'CISESSION'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 6
laFileStru[12,4] = 0

LOCAL lnI,lnJ
FOR lnI = 1 TO 12
FOR lnJ = 7 TO 16
laFileStru[lnI,lnJ] = ''
ENDFOR
STORE 0 TO laFileStru[lnI,17],laFileStru[lnI,18]
ENDFOR

LOCAL ARRAY laIndex[2,2]
laIndex[1,1] = 'DTOS(DTRDATE)+CWARECODE'
laIndex[1,2] = lcTmpWip
laIndex[2,1] = 'CWARECODE+CISESSION'
laIndex[2,2] = lcTmpWip1
=gfCrtTmp(lcTmpWip,@laFileStru,@laIndex)
SET ORDER TO TAG (lcTmpWip) IN (lcTmpWip)

LOCAL lcBomLine,lcMatInvJl
lcBomLine  = gfTempName()
lcMatInvJl = gfTempName()
m.cImTyp   = loParentForm.lcTranType
m.cType    = '2'
m.CtktNo   = EVALUATE(loParentForm.lcPosHdr+'.PO')
IF !lfOpenSql('BOMLINE',lcBomLine,'BOMPOREC','CIMTYP+CTYPE+CTKTNO',loParentForm)
RETURN .F.
ENDIF

m.cInvType  = lcInvType
m.Style     = lcStyle

LOCAL lnConnectionHandlar, lcTranCode

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
=oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
RETURN .F.
ENDIF

IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE',loParentForm)
SELECT FABDYE
LOCATE FOR cWareCode+Dyelot = lcWareCode+lcDyelot
IF FOUND()
REPLACE NTOTCUSA WITH NTOTCUSA - lnIssued,;
NCUSAGE1 WITH NCUSAGE1 - lnIssued,;
NCUSAGE2 WITH NCUSAGE2 - lnIssued,;
NCUSAGE3 WITH NCUSAGE3 - lnIssued,;
NCUSAGE4 WITH NCUSAGE4 - lnIssued,;
NCUSAGE5 WITH NCUSAGE5 - lnIssued,;
NCUSAGE6 WITH NCUSAGE6 - lnIssued,;
NCUSAGE7 WITH NCUSAGE7 - lnIssued,;
NCUSAGE8 WITH NCUSAGE8 - lnIssued
ENDIF
LOCATE FOR cWareCode+Dyelot = lcWareCode+SPACE(10)
IF FOUND()
REPLACE NTOTCUSA WITH NTOTCUSA - lnIssued,;
NCUSAGE1 WITH NCUSAGE1 - lnIssued,;
NCUSAGE2 WITH NCUSAGE2 - lnIssued,;
NCUSAGE3 WITH NCUSAGE3 - lnIssued,;
NCUSAGE4 WITH NCUSAGE4 - lnIssued,;
NCUSAGE5 WITH NCUSAGE5 - lnIssued,;
NCUSAGE6 WITH NCUSAGE6 - lnIssued,;
NCUSAGE7 WITH NCUSAGE7 - lnIssued,;
NCUSAGE8 WITH NCUSAGE8 - lnIssued

ENDIF
ENDIF

LOCAL lnRecQty
SELECT (loParentForm.lcCTKTBOM)
SCAN FOR CCATGTYP $ 'FT'
lnRecQty = 0
SELECT (lcBOMLINE)
SUM FOR CINVTYPC = EVALUATE(loParentForm.lcCtktBom+'.cInvType') AND ITEM = EVALUATE(loParentForm.lcCTKTBOM+'.ITEM');
AND DYELOT = EVALUATE(loParentForm.lcCTKTBOM+'.DYELOT') AND !EMPTY(CRSESSION) ITEMQTY TO lnRecQty
SELECT (lcTmpWip)
ZAP
m.cInvType = EVALUATE(loParentForm.lcCtktbom+'.cInvType')
m.Style    = EVALUATE(loParentForm.lcCtktBom+'.Item')
IF lfOpenSql('ITEMJRNL',lcMatInvJl,'STYINVJL','CINVTYPE+STYLE',loParentForm)
SELECT (lcMatInvJl)
LOCATE
IF !EOF()
SCAN FOR CDYELOT  = EVALUATE(loParentForm.lcCTKTBOM+'.DYELOT') AND CTRTYPE = '9' AND;
CBUSDOCU = EVALUATE(loParentForm.lcPosHdr+'.CBUSDOCU') AND;
CSTYTYPE = EVALUATE(loParentForm.lcPosHdr+'.CSTYTYPE') AND;
CTKTNO   = EVALUATE(loParentForm.lcCTKTBOM+'.CUTTKT')
IF CIRTYPE = 'I'
INSERT INTO (lcTmpWip) (CWARECODE,NCUSAGE1,NCUSAGE2,NCUSAGE3,NCUSAGE4,NCUSAGE5,NCUSAGE6,NCUSAGE7,NCUSAGE8,;
NTOTCUSA,DTRDATE,CISESSION);
VALUES (EVALUATE(lcMATINVJL+'.CWARECODE'),EVALUATE(lcMATINVJL+'.NSTK1'),;
EVALUATE(lcMATINVJL+'.NSTK2'),EVALUATE(lcMATINVJL+'.NSTK3'),;
EVALUATE(lcMATINVJL+'.NSTK4'),EVALUATE(lcMATINVJL+'.NSTK5'),;
EVALUATE(lcMATINVJL+'.NSTK6'),EVALUATE(lcMATINVJL+'.NSTK7'),;
EVALUATE(lcMATINVJL+'.NSTK8'),EVALUATE(lcMATINVJL+'.NTOTSTK'),;
EVALUATE(lcMATINVJL+'.DTRDATE'),EVALUATE(lcMATINVJL+'.CISESSION'))
ELSE
SELECT (lcTmpWip)
SET ORDER TO TAG (lcTmpWip1)
IF SEEK(EVALUATE(lcMATINVJL+'.CWARECODE')+EVALUATE(lcMATINVJL+'.CISESSION'))
REPLACE NCUSAGE1 WITH NCUSAGE1 + EVALUATE(lcMATINVJL+'.NSTK1'),;
NCUSAGE2 WITH NCUSAGE2 + EVALUATE(lcMATINVJL+'.NSTK2'),;
NCUSAGE3 WITH NCUSAGE3 + EVALUATE(lcMATINVJL+'.NSTK3'),;
NCUSAGE4 WITH NCUSAGE4 + EVALUATE(lcMATINVJL+'.NSTK4'),;
NCUSAGE5 WITH NCUSAGE5 + EVALUATE(lcMATINVJL+'.NSTK5'),;
NCUSAGE6 WITH NCUSAGE6 + EVALUATE(lcMATINVJL+'.NSTK6'),;
NCUSAGE7 WITH NCUSAGE7 + EVALUATE(lcMATINVJL+'.NSTK7'),;
NCUSAGE8 WITH NCUSAGE8 + EVALUATE(lcMATINVJL+'.NSTK8'),;
NTOTCUSA WITH NTOTCUSA + EVALUATE(lcMATINVJL+'.NTOTSTK')
ELSE
IF SEEK(EVALUATE(lcMATINVJL+'.CWARECODE'))
lnRemender = EVALUATE(lcMATINVJL+'.NTOTSTK')
lnRem1     = EVALUATE(lcMATINVJL+'.NSTK1')
lnRem2     = EVALUATE(lcMATINVJL+'.NSTK2')
lnRem3     = EVALUATE(lcMATINVJL+'.NSTK3')
lnRem4     = EVALUATE(lcMATINVJL+'.NSTK4')
lnRem5     = EVALUATE(lcMATINVJL+'.NSTK5')
lnRem6     = EVALUATE(lcMATINVJL+'.NSTK6')
lnRem7     = EVALUATE(lcMATINVJL+'.NSTK7')
lnRem8     = EVALUATE(lcMATINVJL+'.NSTK8')
SCAN REST WHILE CWARECODE+CISESSION = EVALUATE(lcMATINVJL+'.CWARECODE')
lnTotCUsa = NTOTCUSA
lnCUsage1 = NCUSAGE1
lnCUsage2 = NCUSAGE2
lnCUsage3 = NCUSAGE3
lnCUsage4 = NCUSAGE4
lnCUsage5 = NCUSAGE5
lnCUsage6 = NCUSAGE6
lnCUsage7 = NCUSAGE7
lnCUsage8 = NCUSAGE8
REPLACE NCUSAGE1 WITH MIN(NCUSAGE1+lnRem1,0),;
NCUSAGE2 WITH MIN(NCUSAGE2+lnRem2,0),;
NCUSAGE3 WITH MIN(NCUSAGE3+lnRem3,0),;
NCUSAGE4 WITH MIN(NCUSAGE4+lnRem4,0),;
NCUSAGE5 WITH MIN(NCUSAGE5+lnRem5,0),;
NCUSAGE6 WITH MIN(NCUSAGE6+lnRem6,0),;
NCUSAGE7 WITH MIN(NCUSAGE7+lnRem7,0),;
NCUSAGE8 WITH MIN(NCUSAGE8+lnRem8,0),;
NTOTCUSA WITH MIN(NTOTCUSA+lnRemender,0)
lnRemender = MAX(lnRemender+lnTotCUsa,0)
lnRem1     = MAX(lnRem1    +lnCUsage1,0)
lnRem2     = MAX(lnRem2    +lnCUsage2,0)
lnRem3     = MAX(lnRem3    +lnCUsage3,0)
lnRem4     = MAX(lnRem4    +lnCUsage4,0)
lnRem5     = MAX(lnRem5    +lnCUsage5,0)
lnRem6     = MAX(lnRem6    +lnCUsage6,0)
lnRem7     = MAX(lnRem7    +lnCUsage7,0)
lnRem8     = MAX(lnRem8    +lnCUsage8,0)
IF lnRemender <= 0
EXIT
ENDIF
ENDSCAN
ENDIF
ENDIF
SET ORDER TO TAG (lcTmpWip)
ENDIF
ENDSCAN
ENDIF
ENDIF
IF RECCOUNT(lcTmpWip) > 0
SELECT (lcTmpWip)
LOCATE
lnRemender = lnRecQty
lnRemSize  = IIF(EVALUATE(loParentForm.lcCtktBom+'.Req_Qty')>0,lnRemender/EVALUATE(loParentForm.lcCtktBom+'.Req_Qty'),0)
SCAN
m.cInvType = EVALUATE(loParentForm.lcCtktBom+'.cInvType')
m.Style    = EVALUATE(loParentForm.lcCtktBom+'.Item')
SELECT FABDYE
LOCATE FOR CWARECODE+DYELOT = EVALUATE(lcTmpWip+'.CWARECODE')+EVALUATE(loParentForm.lcCTKTBOM+'.DYELOT')
IF FOUND()
REPLACE NTOTCUSA WITH NTOTCUSA - MIN(EVALUATE(lcTmpWip+'.NTOTCUSA')+lnRemender,0),;
NCUSAGE1 WITH NCUSAGE1 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE1')+lnRemSize,0),;
NCUSAGE2 WITH NCUSAGE2 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE2')+lnRemSize,0),;
NCUSAGE3 WITH NCUSAGE3 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE3')+lnRemSize,0),;
NCUSAGE4 WITH NCUSAGE4 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE4')+lnRemSize,0),;
NCUSAGE5 WITH NCUSAGE5 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE5')+lnRemSize,0),;
NCUSAGE6 WITH NCUSAGE6 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE6')+lnRemSize,0),;
NCUSAGE7 WITH NCUSAGE7 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE7')+lnRemSize,0),;
NCUSAGE8 WITH NCUSAGE8 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE8')+lnRemSize,0)
ENDIF
IF !EMPTY(EVALUATE(loParentForm.lcCTKTBOM+'.DYELOT'))
LOCATE FOR CWARECODE+DYELOT = EVALUATE(lcTmpWip+'.CWARECODE')+SPACE(10)
IF FOUND()
REPLACE NTOTCUSA WITH NTOTCUSA - MIN(EVALUATE(lcTmpWip+'.NTOTCUSA')+lnRemender,0),;
NCUSAGE1 WITH NCUSAGE1 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE1')+lnRemSize,0),;
NCUSAGE2 WITH NCUSAGE2 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE2')+lnRemSize,0),;
NCUSAGE3 WITH NCUSAGE3 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE3')+lnRemSize,0),;
NCUSAGE4 WITH NCUSAGE4 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE4')+lnRemSize,0),;
NCUSAGE5 WITH NCUSAGE5 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE5')+lnRemSize,0),;
NCUSAGE6 WITH NCUSAGE6 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE6')+lnRemSize,0),;
NCUSAGE7 WITH NCUSAGE7 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE7')+lnRemSize,0),;
NCUSAGE8 WITH NCUSAGE8 - MIN(EVALUATE(lcTmpWip+'.NCUSAGE8')+lnRemSize,0)
ENDIF
ENDIF

SELECT (lcTmpWip)
lnRemender = MAX(lnRemender+NTOTCUSA,0)
lnRemSize  = IIF(EVALUATE(loParentForm.lcCtktBom+'.Req_Qty')>0,lnRemender/EVALUATE(loParentForm.lcCtktBom+'.Req_Qty'),0)
ENDSCAN

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('FABDYE',lcTranCode,loParentForm.DataSessionId,;
'CINVTYPE,STYLE,CWARECODE,DYELOT','ITEMLOC','STYDYE')
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
=oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
=TABLEREVERT(.T.)
RETURN .F.
ELSE
=TABLEUPDATE(.T.,.T.)
ENDIF
ENDIF
ENDSCAN

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
=oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
RETURN .F.
ENDIF

USE IN (lcTmpWip)
USE IN (lcBomLine)
USE IN (lcMatInvJl)

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvEditUse
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2004
*! Purpose   : Edit Used Quantity for Cost Item
*!*************************************************************
*! Calls     : MFITMISS.SPX,MFSTYISS.SPX,MFMFGISS.SPX,lfShowLog
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvEditUse()
*!*************************************************************
FUNCTION lfvEditUse
LPARAMETERS loFormSet

ldIssDate = oariaapplication.systemdate
DO CASE
CASE INLIST(m.cCatgTyp,'F','T') .AND. SEEK('S'+FABRIC.Scale,'Scale')
llItemDye = loParentForm.laSetups[14,2]='Y' .AND. Fabric.cDye_Flg = 'Y'
llOk = .F.
SCATTER MEMVAR
IF INLIST(loParentForm.laSetups[11,2],'L','F','I')
*-- undeveloped yet.
ELSE
*--- DOC.
*--- If the cost method is average.
*--- DOC.
lcIssWare   = EVALUATE(loParentForm.lcPosHdr+'.CMATWARE')
lnIssWare   = CEILING(ASCAN(loParentForm.laMatWare,lcIssWare)/2)
llWareStat  = .T.

m.cWareCode = lcIssWare
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE',loParentForm)
SELECT FABDYE
LOCATE FOR Dyelot = SPACE(10)
ENDIF

lnIssCost   = IIF(loParentForm.laSetups[11,2]='A',FabDye.nAveCstBuy,Fabric.TotCost)/lfGetConv(FABRIC.cConvBuy)

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
m.nOnHand = IIF(loParentForm.laSetups[2,2]='Y',FabDye.TotStk,Fabric.TotStk)
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]

LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
laTotQty[lnI] = EVALUATE('m.Used_Qty'+lcI)
ENDFOR

m.Issue_Qty = 0
laTotQty[9] = m.Used_Qty
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'MFISSRET.SCX') WITH .F.,.F.,.T.
=gfCallForm('MFISSRET',.F.,'.F.,.F.,.T.')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
IF !llOk
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
m.Iss_Qty&lcI = laTotQty[lnI]
ENDFOR
m.Used_Qty = laTotQty[9]
ENDIF
ENDIF
m.TotStk = lfGetTotStk(loParentForm)
SELECT (loParentForm.lcTktSheet)
GATHER FIELDS TotStk MEMVAR
CASE m.cCatgTyp='S' .AND. SEEK(m.Item,'Style') .AND. SEEK('S'+Style.Scale,'Scale')
*-- undeveloped yet.
CASE INLIST(m.cCatgTyp,'M','P','D')
*-- undeveloped yet.
OTHERWISE
*-- undeveloped yet.
ENDCASE
=lfShowLog(loFormSet)

*!*************************************************************
*! Name      : lfAct1Opr
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/02/2004
*! Purpose   : Get Bar number in the system menu
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  Bar Number
*!*************************************************************
*! Example   :  =lfAct1Opr()
*!*************************************************************
*E302004,1 AMH
FUNCTION lfAct1Opr

PRIVATE lnAlias
lnAlias = SELECT(0)

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT (lcTmpTkt)
SELECT (loParentForm.lcTmpTkt)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

SUM ALL nTotQty TO lnLotbud

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT (lcOprDet)
SELECT(loParentForm.lcOprDet)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

*E302004,1 AMH Save the current record No. [Start]
PRIVATE lnRecNo
lnRecNo = RECNO()
*E302004,1 AMH [End]

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*lcCurrTag = ORDER(lcOprDet)
lcCurrTag = ORDER(loParentForm.lcOprDet)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

SET ORDER TO TAG 'LOT'

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*=SEEK(lcTranType+laData[1]+lcOprCode)
=SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+lcOprCode)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]


*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SUM REST IIF(TranCd='1',nLotTotQty,0) TO lnLotRcv;
WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcOprCode

SUM REST IIF(TranCd='1',nLotTotQty,0) TO lnLotRcv;
WHILE    cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+lcOprCode

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

*E302004,1 AMH Save the current record No. [Start]
IF BETWEEN(lnRecNo,1,RECCOUNT())
GO lnRecNo
ENDIF
*E302004,1 AMH [End]

SELECT (lnAlias)

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SET ORDER TO TAG lcCurrTag IN (lcOprDet)
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

*lcStatColor=IIF(lnLotbud-lnLotRcv-lnLotCan-lnLotDmg>0,"RGB(255,255,255,255,0,0)",gcObjColor)

*!*************************************************************
*! Name      : lfvOkAct1
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/02/2004
*! Purpose   : Actualize the whole cutting ticket for one operation.
*!*************************************************************
*! Calls     : lfBrowLots,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvOkAct()
*!*************************************************************
*E302004,1 AMH
FUNCTION lfvOkAct1
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
PARAMETERS loFormSet
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

PRIVATE lnAlias,lcLotNo,lcItem,lcColor,laOpen,llInHouse,;
lcContCode,lcContName,lcCurrTag,lnUpdYield,lcKeyVal
DECLARE laOpen[9],laOrd[8]
lnAlias  = SELECT(0)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*lcCurrTag= ORDER(lcOprDet)
*SET ORDER TO TAG LOT IN (lcOprDet)
lcCurrTag= ORDER(loParentForm.lcOprDet)
SET ORDER TO TAG LOT IN (loParentForm.lcOprDet)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

STORE .F. TO llTktAllo,llRelAll
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*	IF lcTranType <> 'T'
*!*	  SELECT (lcOprDet)
*!*	  =SEEK(lcTranType+laData[1]+lcFirstOpr)
*!*	  DO WHILE !llTktAllo AND ;
*!*	           cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
*!*	           lcTranType+laData[1]+lcFirstOpr
IF loParentForm.lcTranType <> 'T'
SELECT (loParentForm.lcOprDet)
=SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+loParentForm.lcFirstOpr)
DO WHILE !llTktAllo AND ;
cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+loParentForm.lcFirstOpr
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

lcItem  = Item
lcLotNo = cLotNo
STORE 0 TO laOpen,laOrd

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem FOR cDyelot = &lcOprDet..cDyelot
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+loParentForm.lcFirstOpr+lcLotNo+loParentForm.lcInvType+lcItem FOR cDyelot = EVALUATE(loParentForm.lcOprDet+'.cDyelot')
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
laOpen[lnCount] = laOpen[lnCount] + EVALUATE('nLotQty'+lcCount)
ENDFOR
ENDSCAN

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT (lcTranFile)
SELECT (loParentForm.lcTranFile)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

*khm1
*=SEEK(IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
*SUM REST WHILE EVAL(lcFileKey) = IIF(lcTranType='I',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] ;
Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8 TO ARRAY laOrd FOR TRANCD = '1' .AND. STYLE = lcItem

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*	    =SEEK(IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1])
*!*	    SUM REST WHILE EVAL(lcFileKey) = IIF(lcTranType $ 'ID',IIF(EMPTY(lcDyePO),'P','D'),'')+laData[1] ;
*!*	    Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8 TO ARRAY laOrd FOR TRANCD = '1' .AND. STYLE = lcItem
LOCATE
SUM REST WHILE EVALUATE(loParentForm.lcFileKey) = EVALUATE(loParentForm.lcPosHdr+'.cBusDocu')+;
EVALUATE(loParentForm.lcPosHdr+'.cStyType')+;
EVALUATE(loParentForm.lcPosHdr+'.PO')  ;
Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8 TO ARRAY laOrd FOR TRANCD = '1' .AND. STYLE = lcItem
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]
*khm1

FOR lnCount = 1 TO 8
IF laOrd[lnCount] > laOpen[lnCount]
llTktAllo = .T.
EXIT
ENDIF
ENDFOR
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT (lcOprDet)
SELECT (loParentForm.lcOprDet)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

ENDDO
*-- Message : 38168
*-- Cutting ticket# 999999 has pieces allocated from orders.
*-- Proceed with acualization and modify the allocated
*-- quantity from order lines to keep track of this
*-- allocation, or cancel
*-- Button : 38022
*-- < Proceed> <Cancel>

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*IF llTktAllo AND gfModalGen('QRM38168B38002','ALERT',;
IIF(lcTranType='M','Cutting Ticket#: ','PO#:')+laData[1]) = 2
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*IF llTktAllo AND gfModalGen('QRM38168B38002','ALERT',;
IIF(loParentForm.lcTranType='M','Cutting Ticket#: ','PO#:')+EVALUATE(loParentForm.lcPosHdr+'.PO')) = 2
IF llTktAllo AND gfModalGen('QRM38168B38002','ALERT',;
IIF(loParentForm.lcTranType='M',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CUTTKT,loParentForm.GetHeaderText("LANG_MFCSSH_CUTTKT",loParentForm.HeaderAlias))+': ',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,loParentForm.GetHeaderText("LANG_MFCSSH_PO",loParentForm.HeaderAlias))+':')+EVALUATE(loParentForm.lcPosHdr+'.PO')) = 2
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

SELECT (lnAlias)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SET ORDER TO TAG lcCurrTag IN (lcOprDet)
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

RETURN
ENDIF
ENDIF

lnUpdYield = 0

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*	IF SEEK(lcTranType+laData[1]+lcFirstOpr,lcOprDet)
*!*	  SELECT (lcOprDet)
*!*	  DO WHILE !EOF() .AND. cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
*!*	           lcTranType+laData[1]+lcFirstOpr
IF SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+loParentForm.lcFirstOpr,loParentForm.lcOprDet)
SELECT (loParentForm.lcOprDet)
DO WHILE !EOF() .AND. cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot= ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+loParentForm.lcFirstOpr
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

lcItem     = Item

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*lcColor    = Color
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

llInHouse  = lInHouse
lcContCode = cContCode
lcContName = cContName
lcLotNo    = cLotNo
lcItDye    = cDyelot
STORE 0 TO laOpen
llActualize = .T.
lcNxtDye = lcItDye
IF loParentForm.llUseDyelot &&llUseDyelot
SKIP 1
IF cDyelot # lcItDye .AND. TranCd = '1'
lcNxtDye = cDyelot
ENDIF
SKIP -1
ENDIF

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd = ;
lcTranType+laData[1]+lcFirstOpr+lcLotNo+lcItem+lcColor ;
FOR   llActualize .AND. cDyelot = lcItDye
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+loParentForm.lcFirstOpr+lcLotNo+loParentForm.lcInvType+lcItem ;
FOR   llActualize .AND. cDyelot = lcItDye

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
laOpen[lnCount] = laOpen[lnCount] + EVALUATE('nLotQty'+lcCount)
laOpen[9] = laOpen[9] + laOpen[lnCount]
ENDFOR
ENDSCAN
IF !llActualize
LOOP
ENDIF

lcKeyVal=cIMTYp+cTktNo+cOprCode+cLotNo+loParentForm.lcInvType+Item+TranCd+cDyelot
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT MFGOPRDT
*!*	    =SEEK(lcTranType+laData[1]+lcFirstOpr+lcLotNo+'1')
*!*	    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
*!*	                      lcTranType+laData[1]+lcFirstOpr+lcLotNo+'1' ;
*!*	                FOR   Item+Color+cDyelot = lcItem+lcColor+lcItDye
*!*	    SELECT (lcTmpTkt)
*!*	    =SEEK(lcItem+lcColor)
*!*	    COUNT REST WHILE Item+Color = lcItem+lcColor FOR cDyelot = lcItDye TO lnRecords

SELECT (loParentForm.lcMFGOPRDT)
LOCATE FOR cIMTyp+ctktno+coprcode+clotno+trancd = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
loParentForm.lcFirstOpr+lcLotNo+'1' AND ;
Item = lcItem AND cDyelot = lcItDye
SELECT (loParentForm.lcTmpTkt)
=SEEK(loParentForm.lcInvType+lcItem)
COUNT REST WHILE cInvType+Item+STR(LineNo,6) = loParentForm.lcInvType+lcItem;
FOR cDyelot = lcItDye TO lnRecords
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

IF lnRecords=1

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*=SEEK(lcItem+lcColor)
*!*	      IF llUseDyelot
*!*	        LOCATE REST WHILE Item+Color = lcItem+lcColor FOR cDyelot = lcItDye
*!*	      ENDIF
=SEEK(loParentForm.lcInvType+lcItem)
IF loParentForm.llUseDyelot
LOCATE REST WHILE cInvType+Item+STR(LineNo,6) = loParentForm.lcInvType+lcItem;
FOR cDyelot = lcItDye
ENDIF
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

REPLACE nQty1   WITH laOpen[1] ,;
nQty2   WITH laOpen[2] ,;
nQty3   WITH laOpen[3] ,;
nQty4   WITH laOpen[4] ,;
nQty5   WITH laOpen[5] ,;
nQty6   WITH laOpen[6] ,;
nQty7   WITH laOpen[7] ,;
nQty8   WITH laOpen[8] ,;
nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8
ELSE
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*  SELECT *,00000 AS nCan1,00000 AS nCan2,00000 AS nCan3,00000 AS nCan4,;
00000 AS nCan5,00000 AS nCan6,00000 AS nCan7,00000 AS nCan8,;
000000 AS nTotCan, RECNO() AS nBudRecNo FROM (lcTmpTkt) ;
WHERE ITEM+COLOR+cDyelot = lcItem+lcColor+lcItDye ;
INTO DBF (gcWorkDir+lcisslogfile)
SELECT *,00000 AS nCan1,00000 AS nCan2,00000 AS nCan3,00000 AS nCan4,;
00000 AS nCan5,00000 AS nCan6,00000 AS nCan7,00000 AS nCan8,;
000000 AS nTotCan, RECNO() AS nBudRecNo FROM (loParentForm.lcTmpTkt) ;
WHERE ITEM+cDyelot = lcItem+lcItDye ;
INTO DBF (oAriaApplication.WorkDir+loParentForm.lcisslogfile)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

STORE MAX(laOpen[1],0) TO lnCan1,lnBal1
STORE MAX(laOpen[2],0) TO lnCan2,lnBal2
STORE MAX(laOpen[3],0) TO lnCan3,lnBal3
STORE MAX(laOpen[4],0) TO lnCan4,lnBal4
STORE MAX(laOpen[5],0) TO lnCan5,lnBal5
STORE MAX(laOpen[6],0) TO lnCan6,lnBal6
STORE MAX(laOpen[7],0) TO lnCan7,lnBal7
STORE MAX(laOpen[8],0) TO lnCan8,lnBal8
STORE lnCan1+lnCan2+lnCan3+lnCan4+lnCan5+lnCan6+lnCan7+lnCan8 TO lnTotCan,lnTotBal
SCATTER MEMVAR

=SEEK(lcItem,'Style') .AND. SEEK('S'+Style.Scale,'Scale')

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*lcConCnTit = IIF(lcTranType='T','Material/Color',lcItmHdr)+' '+;
ALLTRIM(lcItem)+ALLTRIM(lcColor)+'Cancelled Quantity'
*DO (gcScrDir+"MFCONCN.SPX")
*      USE IN (lcisslogfile)
lcConCnTit = IIF(loParentForm.lcTranType='T','Material/Color',lcItmHdr)+' '+;
ALLTRIM(lcItem)+'Cancelled Quantity'
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFCONCN.SCX")
=gfCallForm('MFCONCN')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
USE IN (loParentForm.lcisslogfile)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]
ENDIF

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*	    SELECT (lcTmpTkt)
*!*	    =SEEK(lcItem+lcColor)
*!*	    SCAN REST WHILE Item+Color+STR(LineNo,6) = lcItem+lcColor  FOR cDyelot = lcItDye
SELECT (loParentForm.lcTmpTkt)
=SEEK(loParentForm.lcInvType+lcItem)
SCAN REST WHILE cInvType+Item+STR(LineNo,6) = loParentForm.lcInvType+lcItem;
FOR cDyelot = lcItDye
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]
DO CASE
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*	         CASE lcTranType='M'
*!*		          SET ORDER TO TAG CUTLIN IN CUTTKTL
*!*		          =SEEK(laData[1]+Item+STR(lineno,6)+'1','CUTTKTL')
*!*		          SELECT CUTTKTL
CASE loParentForm.lcTranType='M'
SELECT (loParentForm.lcPosLn)
LOCATE FOR Style+STR(LineNo,6)+TranCd = ;
lcItem+STR(EVALUATE(loParentForm.lcTmpTkt+'.LineNo'),6)+'1'
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

SCATTER FIELDS ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotOrd TO laOrdQty

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT (lcTmpTkt)
SELECT (loParentForm.lcTmpTkt)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
IF laOrdQty[1] > EVALUATE('nQty'+lcCount)

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*	              lnOpen1 = CUTTKTL.QTY1 - laOpen[1]
*!*	              lnOpen2 = CUTTKTL.QTY2 - laOpen[2]
*!*	              lnOpen3 = CUTTKTL.QTY3 - laOpen[3]
*!*	              lnOpen4 = CUTTKTL.QTY4 - laOpen[4]
*!*	              lnOpen5 = CUTTKTL.QTY5 - laOpen[5]
*!*	              lnOpen6 = CUTTKTL.QTY6 - laOpen[6]
*!*	              lnOpen7 = CUTTKTL.QTY7 - laOpen[7]
*!*	              lnOpen8 = CUTTKTL.QTY8 - laOpen[8]
*!*	              =lfTktAllo(lcItem,CutTKtL.Dyelot,CutTKtL.LineNo)
lnOpen1 = EVALUATE(loParentForm.lcPosLn+'.Qty1') - laOpen[1]
lnOpen2 = EVALUATE(loParentForm.lcPosLn+'.Qty2') - laOpen[2]
lnOpen3 = EVALUATE(loParentForm.lcPosLn+'.Qty3') - laOpen[3]
lnOpen4 = EVALUATE(loParentForm.lcPosLn+'.Qty4') - laOpen[4]
lnOpen5 = EVALUATE(loParentForm.lcPosLn+'.Qty5') - laOpen[5]
lnOpen6 = EVALUATE(loParentForm.lcPosLn+'.Qty6') - laOpen[6]
lnOpen7 = EVALUATE(loParentForm.lcPosLn+'.Qty7') - laOpen[7]
lnOpen8 = EVALUATE(loParentForm.lcPosLn+'.Qty8') - laOpen[8]
=lfTktAllo(lcItem,EVALUATE(loParentForm.lcPosLn+'.Dyelot'),EVALUATE(loParentForm.lcPosLn+'.LineNo'),.T.,loParentForm)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

EXIT
ENDIF
ENDFOR
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT CUTTKTH
SELECT (loParentForm.lcPosHdr)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [END]

=RLOCK()
REPLACE TotOrd WITH TotOrd - EVALUATE(loParentForm.lcPosLn+'.TotOrd') + laOrdQty[9]
UNLOCK
SELECT STYLE
=SEEK(EVALUATE(loParentForm.lcPosLn+'.Style'))
=RLOCK()
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
REPLACE ('WIP'+lcCount) WITH EVALUATE('WIP'+lcCount+' - '+loParentForm.lcPosLn+'.Qty'+lcCount+;
' + '+loParentForm.lcTmpTkt+'.nQty'+lcCount)
ENDFOR
REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
UNLOCK
SELECT STYDYE
=SEEK(EVALUATE(loParentForm.lcPosLn+'.Style')+EVALUATE(loParentForm.lcPosLn+'.cWareCode')+SPACE(10))
=RLOCK()
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
REPLACE ('WIP'+lcCount) WITH EVALUATE('WIP'+lcCount+' - '+ loParentForm.lcPosLn+'.Qty'+lcCount+;
' + '+loParentForm.lcTmpTkt+'.nQty'+lcCount)
ENDFOR
REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
UNLOCK

SELECT (loParentForm.lcPosLn)
lnTotQty = Totqty
=RLOCK()
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
REPLACE ('QTY'+lcCount) WITH EVALUATE(loParentForm.lcTmpTkt+'.nQty'+lcCount)
ENDFOR
REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
REPLACE Ord1   WITH laOrdQty[1] ,;
Ord2   WITH laOrdQty[2] ,;
Ord3   WITH laOrdQty[3] ,;
Ord4   WITH laOrdQty[4] ,;
Ord5   WITH laOrdQty[5] ,;
Ord6   WITH laOrdQty[6] ,;
Ord7   WITH laOrdQty[7] ,;
Ord8   WITH laOrdQty[8] ,;
TotOrd WITH laOrdQty[9]
UNLOCK

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*	          laData[6]  = MAX(CutTktH.Pcs_Bud,CutTktH.Pcs_Bud+(CutTktl.TotQty-lnTotQty ))
*!*	          laData[10] = MAX(ladata[6] - (lnLotRcv+ladata[8]+ladata[9]),0)
*!*	          SELECT CutTktH
*!*	          REPLACE Pcs_Bud WITH laData[6],;
*!*	                  PCS_OPN WITH laData[10]
*!*	          SELECT CUTTKTL
*!*	          laData[8]  = laData[8]  + MAX(lnTotQty-CutTktL.TotQty,0)
*!*	          laData[12] = laData[12] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost1
*!*	          laData[13] = laData[13] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost2
*!*	          laData[14] = laData[14] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost3
*!*	          laData[15] = laData[15] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost4
*!*	          laData[16] = laData[16] + (CutTktL.TotQty-lnTotQty)*CutTktL.nCost5
SELECT (loParentForm.lcPosHdr)

*MMT2
REPLACE NSTYORDER WITH MAX(NSTYORDER ,NSTYORDER +(EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty))
REPLACE OPEN WITH MAX(NSTYORDER - (RECEIVE+DAMAGE+CANCEL) ,0)
*MMT2

REPLACE nFCost1 WITH nFCost1 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost1') ,;
nFCost2 WITH nFCost2 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost2') ,;
nFCost3 WITH nFCost3 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost3') ,;
nFCost4 WITH nFCost4 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost4') ,;
nFCost5 WITH nFCost5 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost5') ,;
nFCost6 WITH nFCost6 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost6') ,;
nFCost7 WITH nFCost7 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost7')

REPLACE Cancel  WITH Cancel  + MAX(lnTotQty - EVALUATE(loParentForm.lcPosLn+'.TotQty'),0),;
nICost1 WITH nICost1 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost1'),;
nICost2 WITH nICost2 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost2'),;
nICost3 WITH nICost3 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost3'),;
nICost4 WITH nICost4 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost4'),;
nICost5 WITH nICost5 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost5'),;
nICost6 WITH nICost6 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost6'),;
nICost7 WITH nICost7 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost7')
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*CASE lcTranType='ID'

* SET ORDER TO TAG POSLN IN POSLN
*=SEEK(IIF(EMPTY(lcDyePO),'P','D')+laData[1]+lcItem+STR(lineno,6)+'1','POSLN')
*SELECT POSLN
CASE loParentForm.lcTranType $ 'ID'
SELECT (loParentForm.lcPosLn)
LOCATE FOR Style+STR(LineNo,6)+TranCd = ;
lcItem+STR(EVALUATE(loParentForm.lcTmpTkt+'.LineNo'),6)+'1'

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]
SCATTER FIELDS ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotOrd TO laOrdQty

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT (lcTmpTkt)
SELECT (loParentForm.lcTmpTkt)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
IF laOrdQty[1] > EVALUATE('nQty'+lcCount)

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*	              lnOpen1 = POSLN.QTY1 - laOpen[1]
*!*	              lnOpen2 = POSLN.QTY2 - laOpen[2]
*!*	              lnOpen3 = POSLN.QTY3 - laOpen[3]
*!*	              lnOpen4 = POSLN.QTY4 - laOpen[4]
*!*	              lnOpen5 = POSLN.QTY5 - laOpen[5]
*!*	              lnOpen6 = POSLN.QTY6 - laOpen[6]
*!*	              lnOpen7 = POSLN.QTY7 - laOpen[7]
*!*	              lnOpen8 = POSLN.QTY8 - laOpen[8]
*!*
*!*	              =lfTktAllo(lcItem,POSLN.Dyelot,POSLN.LineNo)
lnOpen1 = EVALUATE(loParentForm.lcPosLn+'.QTY1') - laOpen[1]
lnOpen2 = EVALUATE(loParentForm.lcPosLn+'.QTY2') - laOpen[2]
lnOpen3 = EVALUATE(loParentForm.lcPosLn+'.QTY3') - laOpen[3]
lnOpen4 = EVALUATE(loParentForm.lcPosLn+'.QTY4') - laOpen[4]
lnOpen5 = EVALUATE(loParentForm.lcPosLn+'.QTY5') - laOpen[5]
lnOpen6 = EVALUATE(loParentForm.lcPosLn+'.QTY6') - laOpen[6]
lnOpen7 = EVALUATE(loParentForm.lcPosLn+'.QTY7') - laOpen[7]
lnOpen8 = EVALUATE(loParentForm.lcPosLn+'.QTY8') - laOpen[8]

=lfTktAllo(lcItem,EVALUATE(loParentForm.lcPosLn+'.Dyelot'),;
EVALUATE(loParentForm.lcPosLn+'.LineNo'),.T.,loParentForm)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]
EXIT
ENDIF
ENDFOR
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT POSHDR
*!*	          REPLACE TotOrd WITH TotOrd - POSLN.TotOrd + laOrdQty[9]
SELECT (loParentForm.lcPosHdr)
REPLACE TotOrd WITH TotOrd - EVALUATE(loParentForm.lcPosLn+'.TotOrd') + laOrdQty[9]
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]


SELECT STYLE

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*=SEEK(POSLN.Style)
=SEEK(EVALUATE(loParentForm.lcPosLn+'.Style'))
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

=RLOCK()
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
REPLACE ('WIP'+lcCount) WITH EVALUATE('WIP'+lcCount+' - '+loParentForm.lcPosLn+'.Qty'+lcCount+;
' + '+loParentForm.lcTmpTkt+'.nQty'+lcCount)
ENDFOR
REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
UNLOCK
SELECT STYDYE

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*=SEEK(POSLN.Style+POSLN.cWareCode+SPACE(10))
=SEEK(EVALUATE(loParentForm.lcPosLn+'.Style')+EVALUATE(loParentForm.lcPosLn+'.cWareCode')+SPACE(10))
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

=RLOCK()
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
REPLACE ('WIP'+lcCount) WITH EVALUATE('WIP'+lcCount+' - '+loParentForm.lcPosLn+'.Qty'+lcCount+;
' + '+loParentForm.lcTmpTkt+'.nQty'+lcCount)
ENDFOR
REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
UNLOCK

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT POSLN
SELECT(loParentForm.lcPosLn)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

lnTotQty = Totqty
=RLOCK()
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
REPLACE ('QTY'+lcCount) WITH EVALUATE(loParentForm.lcTmpTkt+'.nQty'+lcCount)
ENDFOR
REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
REPLACE Ord1   WITH laOrdQty[1] ,;
Ord2   WITH laOrdQty[2] ,;
Ord3   WITH laOrdQty[3] ,;
Ord4   WITH laOrdQty[4] ,;
Ord5   WITH laOrdQty[5] ,;
Ord6   WITH laOrdQty[6] ,;
Ord7   WITH laOrdQty[7] ,;
Ord8   WITH laOrdQty[8] ,;
TotOrd WITH laOrdQty[9]
UNLOCK

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*SELECT POSHDR
SELECT (loParentForm.lcPosHdr)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

=RLOCK()
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
REPLACE nFCost1 WITH nFCost1 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost1') ,;
nFCost2 WITH nFCost2 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost2') ,;
nFCost3 WITH nFCost3 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost3') ,;
nFCost4 WITH nFCost4 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost4') ,;
nFCost5 WITH nFCost5 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost5') ,;
nFCost6 WITH nFCost6 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost6') ,;
nFCost7 WITH nFCost7 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost7')

REPLACE Cancel  WITH Cancel  + MAX(lnTotQty - EVALUATE(loParentForm.lcPosLn+'.TotQty'),0),;
nICost1 WITH nICost1 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost1'),;
nICost2 WITH nICost2 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost2'),;
nICost3 WITH nICost3 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost3'),;
nICost4 WITH nICost4 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost4'),;
nICost5 WITH nICost5 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost5'),;
nICost6 WITH nICost6 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost6'),;
nICost7 WITH nICost7 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost7')
UNLOCK

*!*	          laData[8]  = laData[8]  + MAX(lnTotQty-POSLN.TotQty,0)
*!*	          laData[12] = laData[12] + (POSLN.TotQty-lnTotQty)*POSLN.nICost1
*!*	          laData[13] = laData[13] + (POSLN.TotQty-lnTotQty)*POSLN.nICost2
*!*	          laData[14] = laData[14] + (POSLN.TotQty-lnTotQty)*POSLN.nICost3
*!*	          laData[15] = laData[15] + (POSLN.TotQty-lnTotQty)*POSLN.nICost4
*!*	          laData[16] = laData[16] + (POSLN.TotQty-lnTotQty)*POSLN.nICost5
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [END]

CASE loParentForm.lcTranType='T'
SELECT (loParentForm.lcPosLn)
LOCATE FOR Style+STR(LineNo,6)+TranCd = ;
lcItem+STR(EVALUATE(loParentForm.lcTmpTkt+'.LineNo'),6)+'1'
*! N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[Start]
m.CINVTYPE = '0002'
m.Style = lcItem
*! N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[End]
IF lfOpenSql('ITEM','FABRIC','STYLE','CINVTYPE+STYLE',loFormSet)
SELECT FABRIC
*! N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[Start]
*REPLACE OnOrder WITH OnOrder - laOpen[9]
REPLACE TOTWIP WITH TOTWIP - laOpen[9]
*! N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[END]
DECLARE laTableUpdate[1,2]
laTableUpdate[1,1] = 'FABRIC'
laTableUpdate[1,2] = 'ITEM'
=lfTableUpdate(loFormSet)
ENDIF

IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+CWARECODE+DYELOT',loFormSet)
SELECT FABDYE
*! N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[Start]
*REPLACE OnOrder WITH OnOrder - laOpen[9]
REPLACE WIP1 WITH WIP1- laOpen[9],;
TOTWIP WITH TOTWIP- laOpen[9]
*! N037637,1 MMT 09/30/2012 Convert MFG Order cost sheet to Aria4xp[END]
DECLARE laTableUpdate[1,2]
laTableUpdate[1,1] = 'FABDYE'
laTableUpdate[1,2] = 'ITEMLOC'
=lfTableUpdate(loFormSet)
ENDIF

SELECT (loParentForm.lcPosLn)
lnTotQty = TOTQTY
=RLOCK()
REPLACE TOTQTY WITH MAX(TOTQTY - laOpen[9],0)
UNLOCK
SELECT (loParentForm.lcPosHdr)
REPLACE nFCost1 WITH nFCost1 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost1') ,;
nFCost2 WITH nFCost2 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost2') ,;
nFCost3 WITH nFCost3 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost3') ,;
nFCost4 WITH nFCost4 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost4')

REPLACE Cancel  WITH Cancel  + MAX(lnTotQty - EVALUATE(loParentForm.lcPosLn+'.TotQty'),0),;
nICost1 WITH nICost1 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost1'),;
nICost2 WITH nICost2 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost2'),;
nICost3 WITH nICost3 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost3'),;
nICost4 WITH nICost4 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost4')

ENDCASE
*-- Update Cost Items Required Quantities upon actualization
*-- Message : 38157
*-- Modify the cost sheet basaed on the actual quantity?
*-- Button : 38006
*-- < Yes >  < No >

lnUpdYield = IIF(lnUpdYield=0 AND laOpen[9] <> 0,;
gfModalGen('QRM38157B38006','ALERT'),lnUpdYield)
IF lnUpdYield = 1

SELECT (loParentForm.lcBomLine)
=SEEK(loParentForm.lcTranType+'1'+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
STR(EVALUATE(loParentForm.lcTmpTkt+'.lineno'),6))

DECLARE laReq[8]
SCAN REST WHILE CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE=;
loParentForm.lcTranType+'1'+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
STR(EVALUATE(loParentForm.lcTmpTkt+'.LineNo'),6)

STORE 0 TO laReq
IF loParentForm.lcTranType = 'T'
lnStyQty = laOpen[9]
ELSE
lnStyQty = 0
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
IF lcCount $ cSizes
lnStyQty   = lnStyQty + laOpen[lnCount]
lnCompSize = VAL(SUBSTR(cCompSizes,AT(lcCount,cSizes),1))
IF lnCompSize # 0
laReq[lnCompSize] = laReq[lnCompSize] + laOpen[lnCount]
ENDIF
ENDIF
ENDFOR
ENDIF
lnOldQty = ItemQty
lnOldAmt = ItemAmt

REPLACE StyQty  WITH lnStyQty ,;
UnitQty WITH IIF(cCatgtyp='F' AND cOprCode=loParentForm.lcFirstOpr,IIF(StyQty=0,0,ItemQty/StyQty),UnitQty) ,;
ItemQty WITH StyQty*UnitQty  ,;
ItemAmt WITH ItemQty*UnitCost
SELECT (loParentForm.lcDetFile)
=SEEK(loParentForm.lcInvType+EVALUATE(loParentForm.lcBomLine+'.Style')+;
STR(EVALUATE(loParentForm.lcBomLine+'.LineNo'),6)+;
EVALUATE(loParentForm.lcBomLine+'.cBomTyp')+'1'+;
EVALUATE(loParentForm.lcBomLine+'.cInvTypC')+;
EVALUATE(loParentForm.lcBomLine+'.Item')+;
EVALUATE(loParentForm.lcBomLine+'.MfgCode'))


REPLACE StyQty  WITH lnStyQty ,;
UnitQty WITH IIF(cCatgtyp='F' AND cOprCode=loParentForm.lcFirstOpr,IIF(StyQty=0,0,ItemQty/StyQty),UnitQty) ,;
ItemQty WITH StyQty*UnitQty  ,;
ItemAmt WITH ItemQty*UnitCost


SELECT (loParentForm.lcCtktBom)
LOCATE FOR cIMTyp+CutTkt+Typ+Item+MfgCode+Dyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
EVALUATE(loParentForm.lcBomline+'.cBomTyp')+;
EVALUATE(loParentForm.lcBomline+'.item')+;
EVALUATE(loParentForm.lcBomline+'.mfgcode')+;
EVALUATE(loParentForm.lcBomline+'.Dyelot')
IF FOUND()
REPLACE Pieces   WITH Pieces - lnOldQty + lnStyQty ,;
UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=loParentForm.lcFirstOpr							,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
Req_qty  WITH Pieces*UntQty   ,;
Est_Cost WITH Req_qty*UntCost ,;
Req_Qty1 WITH laReq[1]*UntQty ,;
Req_Qty2 WITH laReq[2]*UntQty ,;
Req_Qty3 WITH laReq[3]*UntQty ,;
Req_Qty4 WITH laReq[4]*UntQty ,;
Req_Qty5 WITH laReq[5]*UntQty ,;
Req_Qty6 WITH laReq[6]*UntQty ,;
Req_Qty7 WITH laReq[7]*UntQty ,;
Req_Qty8 WITH laReq[8]*UntQty
ENDIF

SELECT (loParentForm.lcTktSheet)
=SEEK(EVALUATE(loParentForm.lcCTKTBOM+'.TYP') + '1' + ;
EVALUATE(loParentForm.lcCTKTBOM+'.cInvType') + ;
EVALUATE(loParentForm.lcCTKTBOM+'.ITEM') + ;
EVALUATE(loParentForm.lcCTKTBOM+'.MFGCODE')+;
EVALUATE(loParentForm.lcCTKTBOM+'.DYELOT'))

REPLACE Pieces   WITH Pieces - lnOldQty + lnStyQty ,;
UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=loParentForm.lcFirstOpr,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
Req_qty  WITH Pieces*UntQty ,;
Est_Cost WITH Req_qty*UntCost ,;
Req_Qty1 WITH laReq[1]*UntQty ,;
Req_Qty2 WITH laReq[2]*UntQty ,;
Req_Qty3 WITH laReq[3]*UntQty ,;
Req_Qty4 WITH laReq[4]*UntQty ,;
Req_Qty5 WITH laReq[5]*UntQty ,;
Req_Qty6 WITH laReq[6]*UntQty ,;
Req_Qty7 WITH laReq[7]*UntQty ,;
Req_Qty8 WITH laReq[8]*UntQty
ENDSCAN
GO TOP IN (loParentForm.lcDetFile)
GO TOP IN (loParentForm.lcTktSheet)
ENDIF
ENDSCAN
SELECT (loParentForm.lcPosHdr)

REPLACE Pcs_Act WITH Pcs_Act + MAX(laOpen[9]-EVALUATE(loParentForm.lcMFGOPRDT+'.nActQty'),0),;
Open  WITH MAX(nStyOrder - (Receive+Cancel+Damage),0)

SELECT (loParentForm.lcMfgOprdt)
REPLACE nActQty WITH laOpen[9]

SELECT (loParentForm.lcOprDet)
=SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
loParentForm.lcFirstOpr+lcLotNo+loParentForm.lcInvType+lcItem+'1'+lcItDye)
REPLACE nActQty WITH laOpen[9]

IF loParentForm.llUseDyelot .AND. lcNxtDye # lcItDye
=SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
loParentForm.lcFirstOpr+lcLotNo+loParentForm.lcInvType+lcItem+'1'+lcNxtDye)
ELSE
=SEEK(lcKeyVal)
ENDIF

ENDDO

SELECT (loParentForm.lcPosHdr)
REPLACE Status   WITH 'A',;
Act_Date WITH loFormSet.AriaForm1.dtPickerActualDate.Value
UNLOCK
IF ASCAN(loParentForm.laEvntTrig , PADR('EXPACTCT',10)) <> 0
=gfDoTriger('MFCSSH',PADR('EXPACTCT',10))
ENDIF

SELECT (loParentForm.lcOprDet)
ZAP

SELECT (loParentForm.lcMFGOPRDT)
SCAN
SCATTER MEMVAR

m.cMFGDesc = gfCodDes(m.cOprCode, 'MFGCODE')

INSERT INTO (loParentForm.lcOprDet) FROM MEMVAR
ENDSCAN
SELECT (loParentForm.lcOprDet)

DECLARE laTableUpdate[5,2]

laTableUpdate[1,1] = loParentForm.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'

laTableUpdate[2,1] = loParentForm.lcBomLine
laTableUpdate[2,2] = 'BOMLINE'

laTableUpdate[3,1] = loParentForm.lcMfgOprdt
laTableUpdate[3,2] = 'MFGOPRDT'

laTableUpdate[4,1] = loParentForm.lcPosHdr
laTableUpdate[4,2] = 'POSHDR'

laTableUpdate[5,1] = loParentForm.lcPosLn
laTableUpdate[5,2] = 'POSLN'

=lfTableUpdate(loParentForm)
GO TOP IN (loParentForm.lcOprDet)
=lfBrowLots(loParentForm)
SELECT (lnAlias)
ENDIF
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)
=lfRefreshToolBarButtons(loParentForm)
loParentForm.Refresh


*!*************************************************************
*! Name      : lfvToOther
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/02/2004
*! Purpose   : Show Other quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvToOther()
*!*************************************************************
FUNCTION lfvToOther
LPARAMETERS lnQtyNo,lnQtyValue,lcTranCd

SELECT (loParentForm.lcLotsDet)
SET FILTER TO
lcItem    = Item
lcInvType = cInvType
lcDyelot  = cDyelot
=SEEK(lcInvType+lcItem+lcDyelot+lcTranCd)
REPLACE ('nToIssue'+STR(lnQtyNo,1)) WITH lnQtyValue
REPLACE nTotToIss WITH nToIssue1+nToIssue2+nToIssue3+nToIssue4+nToIssue5+;
nToIssue6+nToIssue7+nToIssue8
=SEEK(lcInvType+lcItem+lcDyelot+'1')
SET FILTER TO TRANCD = '1'

*!*************************************************************
*! Name      : lfContrbut
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/14/2004
*! Purpose   : Contrbute the AP invoice lines
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfContrbut()
*!*************************************************************

FUNCTION lfContrbut
PARAMETER lcTktType,lcPONo,loFormSet

DIMENSION laOpenFile[1,2]
STORE ' ' TO lcAPInvTkt,lcAPvInvDt,lcvInDtKey,lcActCstHd ,lcActCstDt
STORE .F. TO llDoContr,laOpenFile
lcAlias = ALIas()
lcAPInvTkt = gfTempName()
lcAPvInvDt = gfTempName()
lcActCstHd = gfTempName()   && Random name for the "Actual Cost" temp. header file
lcActCstDt = gfTempName()   && Random name for the "Actual Cost" temp. detail file

=lfOpenFiles()

m.cBusDocu  = loFormSet.lcBusDocu
m.cStyType  = loFormSet.lcStyType
m.Po        = lcPONo
=lfOpenSql('POSLN','POSLN','POSREC','CBUSDOCU+CSTYTYPE+PO',loFormSet)
=lfSetIndex('POSLN','POSREC','CBUSDOCU+CSTYTYPE+PO+CRSESSION+SHIPNO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD')
SELECT POSLN
LOCATE

SELECT (lcAPvInvDt)
SET ORDER TO ITEM
LOCATE
LOCAL lcItem
SCAN
lcInvDtKey = EVALUATE(lcAPvInvDt+'.cvendcode')+EVALUATE(lcAPvInvDt+'.capinvno')+EVALUATE(lcAPvInvDt+'.capvilno')
lcOprCode  = cOprCode
STORE 0 TO lnTotRec,lnCntrbAmnt

lcItem = lfGetItem(EVALUATE(lcAPvInvDt+'.Item'),EVALUATE(lcAPvInvDt+'.Color'),loFormSet)
IF lfvRSession(loFormSet.lcBusDocu+loFormSet.lcStyType,lcPONo,lcItem,EVALUATE(lcAPvInvDt+'.cBomType')) > 0
SELECT (lcApInvTkt)
SET ORDER TO (lcApInvTkt)
IF SEEK(lcInvDtKey)
SCAN REST WHILE cvendcode+capinvno+capvilno+crsession = lcInvDtKey FOR EMPTY(crsession)
REPLACE cStatus                 WITH 'D',;
(lcAPvInvDt+'.cStatus') WITH 'M'
SELECT RcvSess
SUM nRecAmnt TO lnTotRec
LOCATE
SCAN
lcRSession  = crsession
lnContAmnt  = EVALUATE(lcAPvInvDt+'.nApAprAmnt')/lnTotRec*nRecAmnt
llDoContr   = lfDoApCont(lnContAmnt)
ENDSCAN
ENDSCAN
ENDIF
ENDIF
ENDSCAN

IF llDoContr AND lfChckACst()
=lfUpdFiles()
ENDIF
=lfCloseFile()
select (lcAlias)

*!*************************************************************
*! Name      : lfDoApCont
*! Developer : AHMED MAHER (AMH)
*! Date      : 9/14/2004
*! Purpose   : Contrbute certen Line in APINVTKT
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDoApCont()
*!*************************************************************
FUNCTION lfDoApCont
PARAMETER lnAmount

PRIVATE lcMfgCode , lcMfgTmp,lnMaxLine
lcVendCode  = EVALUATE(lcAPvInvDt+'.cvendcode')
lcApInvNo   = EVALUATE(lcAPvInvDt+'.capinvno')
lcInvLineNo = EVALUATE(lcAPvInvDt+'.capvilno')
lcContItem  = EVALUATE(lcAPvInvDt+'.Item')
lcCstType   = EVALUATE(lcAPvInvDt+'.cBomType')
lcshipNo    = EVALUATE(lcAPvInvDt+'.ShipNo')
lcTicket    = EVALUATE(lcAPvInvDt+'.cTktNo')

SELECT (lcAPInvTkt)
lcMfgCode = lcOprCode
lnMaxLine = lfGetMaxLin(cvendcode,capinvno)

IF EMPTY(lcContItem)
IF lcTktType$'IM' AND (lcOprCode$REPLICATE('*',6)+'|'+REPLICATE('#',6))
LOCAL llFound
llFound     = .F.
m.cImTyp    = lcTktType
m.cType     = '2'
m.cBomTyp   = lcCstType
m.MfgCode   = PADR('*'+lcCstType,6)
m.CtktNo    = lcTicket
m.cRsession = lcRSession
IF lfOpenSql('BOMLINE','BOMLINE','BOMPOREC','CIMTYP+CTYPE+CBOMTYP+MFGCODE+CTKTNO+CRSESSION',loFormSet)
SELECT BOMLINE
LOCATE
llFound = !EOF()
ENDIF
m.MfgCode   = lcOprCode
IF !llFound AND lfOpenSql('BOMLINE','BOMLINE','BOMPOREC','CIMTYP+CTYPE+CBOMTYP+MFGCODE+CTKTNO+CRSESSION',loFormSet)
SELECT BOMLINE
LOCATE
llFound = !EOF()
ENDIF
IF llFound
lcMfgCode = PADR(BOMLINE.MFGCODE,6)
ENDIF
ENDIF
ELSE
IF lcTktType$'IM' AND lcOprCode=REPLICATE('*',6)
=SEEK(PADR(lcContItem,19),'STYLE')
lcMfgCode=IIF(!STYLE.lDetCost,PADR('*'+lcCstType,6),lcOprCode)
ENDIF
IF lcTktType$'IM' AND lcOprCode=REPLICATE('#',6)
=SEEK(PADR(lcContItem,19),'STYLE')
lcMfgCode=IIF(!STYLE.lDetCost,PADR('*'+lcCstType,6),lcOprCode)
ENDIF
ENDIF

llSeekBom = .F.
lcCondition = 'CBUSDOCU+CSTYTYPE+PO+CRSESSION+SHIPNO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'

SELECT POSLN
IF SEEK(loFormSet.lcBusDocu+loFormSet.lcStyType+lcTicket+lcRsession)
lcShipNo = PosLn.ShipNo
ENDIF
lcKey     = loFormSet.lcBusDocu+loFormSet.lcStyType+lcTicket+lcRSession+lcShipNo+loFormSet.lcInvType+PADR(EVALUATE(lcAPvInvDt+'.Item'),LEN(loFormSet.lcMjrMsk))
lcBomCond = "Bomline.cimtyp + Bomline.ctype + Bomline.cbomtyp + Bomline.mfgcode +;
Bomline.shipno + Bomline.crsession +ctktno+STR(lineno,6)+cInvType+style+cstygrade"
lcBomKey  = lcTktType+'2'+lcCstType+lcMfgCode+lcShipNo+lcRSession+lcTicket
lcForCond = IIF(EMPTY(EVALUATE(lcAPvInvDt+'.Item')),".T.",;
"cInvtype = '"+loFormSet.lcInvType+"' and style = '"+EVALUATE(lcAPvInvDt+'.Item')+"'")

m.cImTyp    = lcTktType
m.cType     = '2'
m.cBomTyp   = lcCstType
m.CtktNo    = lcTicket
m.cRsession = lcRSession
m.MfgCode   = lcMfgCode
m.ShipNo    = lcShipNo
=lfOpenSql('BOMLINE','BOMLINE','Bomshrec','CIMTYP+CTYPE+CBOMTYP+MFGCODE+SHIPNO+CRSESSION+CTKTNO',loFormSet)
SELECT BOMLINE
LOCATE
llSeekBom = FOUND()

lnTotBud   = 0
lnTItmAmt  = 0
lnTotActCt = 0          && Total Actual Cost
lnTotLanCt = 0          && Total Landed Cost

SELECT POSLN
=SEEK(lcKey)
SUM REST POSLN.TotQty WHILE EVALUATE(lcCondition)=lcKey FOR &lcForCond. TO lnTotBud
=SEEK(lcKey)
SCAN REST WHILE EVAL(lcCondition)=lcKey FOR &lcForCond. .AND. POSLN.TotQty <> 0
SELECT (lcAPInvTkt)
APPEND BLANK
REPLACE cStatus    WITH 'A'           ,;
cVendCode  WITH lcvendcode    ,;
cApInvNo   WITH lcapinvno     ,;
cApVILNo   WITH lcInvLineNo   ,;
cIMTyp     WITH lcTktType     ,;
cTktNo     WITH lcTicket      ,;
cRSession  WITH lcRSession    ,;
cRecvType  WITH IIF(POSLN.cStyGrade='1','R',IIF(POSLN.cStyGrade='2','S','D')) ,;
LineNo     WITH IIF(lcTktType='T',0,POSLN.LineNo) ,;
Item       WITH IIF(lcTktType='T',SUBSTR(POSLN.Style,1,LEN(loFormSet.lcMjrMsk)),POSLN.Style),;
Color      WITH IIF(lcTktType='T',SUBSTR(POSLN.Style,loFormSet.lnClrPos,LEN(loFormSet.lcClrMsk)),SPACE(6)),;
cDyelot    WITH POSLN.Dyelot  ,;
nApTAplQty WITH POSLN.TotQty  ,;
nApAPlPric WITH lnAmount/lnTotBud,;
cWareCode  WITH POSLN.cWareCode,;
nApAplAmnt WITH POSLN.TotQty*lnAmount/lnTotBud
lnMaxLine = lnMaxLine  + 1
REPLACE cApdGlAct WITH lfGetWIPAc() ;
cApdLinNo WITH  STR(lnMaxLine,4)

lnTItmAmt  = lnTItmAmt  + nApAplAmnt
REPLACE nApAplQty1 WITH POSLN.Qty1 ,;
nApAplQty2 WITH POSLN.Qty2 ,;
nApAplQty3 WITH POSLN.Qty3 ,;
nApAplQty4 WITH POSLN.Qty4 ,;
nApAplQty5 WITH POSLN.Qty5 ,;
nApAplQty6 WITH POSLN.Qty6 ,;
nApAplQty7 WITH POSLN.Qty7 ,;
nApAplQty8 WITH POSLN.Qty8
lnTotLanCt = lnTotLanCt + POSLN.TotQty * EVALUATE('POSLN.NFLANCOST'+lcCstType)
lnTotActCt = lnTotActCt + POSLN.TotQty * EVALUATE('POSLN.NFACTCOST'+lcCstType)
ENDSCAN

SELECT (lcAPInvTkt)
SET KEY TO
*-- If over contributed.
IF ABS(lnTItmAmt) > ABS(EVALUATE(lcApVInvDt+'.nAPAprAmnt'))
REPLACE nApAplAmnt WITH nApAplAmnt - (ABS(lnTItmAmt) - ABS(lnAmount))
lnTItmAmt = lnTItmAmt - (ABS(lnTItmAmt) - ABS(lnAmount))
=SEEK(PADR(lcvendcode,8)+PADR(lcApInvNo,12)+lcInvLineNo+SPACE(6))
REPLACE nApAplAmnt WITH nApAplAmnt - lnTItmAmt
ELSE    && Else (If not over contributed)
IF ABS(lnTItmAmt) < ABS(EVALUATE(lcApVInvDt+'.nAPAprAmnt'))
lnTItmAmt = lnTItmAmt - (ABS(lnTItmAmt) - ABS(lnAmount))
ENDIF
=SEEK(PADR(lcvendcode,8)+PADR(lcApInvNo,12)+lcInvLineNo+SPACE(6))
REPLACE nApAplAmnt WITH nApAplAmnt - lnTItmAmt
ENDIF    && End of IF ABD(lnTItmAmt) > ABS(nApAplAmnt)

IF nApAplAmnt = 0
REPLACE cStatus WITH IIF(nRecNo = 0 , 'S' , 'D')
DELETE
ENDIF
lnCntrbAmnt = lnCntrbAmnt + lnTItmAmt

RETURN (lnCntrbAmnt > 0)

*!*************************************************************
*! Name      : lfOpenFiles
*! Developer : AHMED MAHER (AMH)
*! Date      : 9/14/2004
*! Purpose   : Open needed files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOpenFiles()
*!*************************************************************
FUNCTION lfOpenFiles

IF !USED('APVINVDT')
DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
laOpenFile[ALEN(laOpenFile,1),1] = 'APVINVDT'
laOpenFile[ALEN(laOpenFile,1),2] = .T.
=gfOpenFile(oAriaApplication.DataDir+'APVINVDT','ITEM','SH')
ENDIF

SELECT APVINVDT
*B128478,1 KHM 06/14/2005 Chaning the Select SQL to be SEEK and SCAN for
*B128478,1                optimization issues [Begin]
*SELECT *, 'S' AS cStatus,RECNO() AS nRecNo FROM ApVInvDt WHERE ;
cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = ;
lcTktType+lcPONo INTO DBF (oAriaApplication.WorkDir+lcAPvInvDt)
*INDEX ON cIMTyp+cTktNo+cLotNo+cBomType+cOprCode+Item+Color+cDyelot TAG ITEM
*INDEX ON cVendCode+cApInvNo+cAPVILNo TAG (lcAPvInvDt) ADDITIVE
*INDEX ON cApDGlAct TAG ADJUST ADDITIVE

LOCAL laFileIndx
DIMENSION laFileIndx[3,2]
laFileIndx[1,1] = 'cIMTyp+cTktNo+cLotNo+cBomType+cOprCode+Item+Color+cDyelot'
laFileIndx[1,2] = 'ITEM'

laFileIndx[2,1] = 'cVendCode+cApInvNo+cAPVILNo'
laFileIndx[2,2] = (lcAPvInvDt)

laFileIndx[3,1] = 'cApDGlAct'
laFileIndx[3,2] = 'ADJUST'

lnFileStru = AFIELDS(laFileStru)
DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'cStatus'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'nRecNo'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 12
laFileStru[lnFileStru+2,4] = 0

LOCAL lnI
FOR lnI = 7 TO 16
STORE '' TO laFileStru[lnFileStru+1,lnI],laFileStru[lnFileStru+2,lnI]
ENDFOR
STORE 0 TO laFileStru[lnFileStru+1,17],laFileStru[lnFileStru+2,17],;
laFileStru[lnFileStru+1,18],laFileStru[lnFileStru+2,18]

=gfCrtTmp(lcAPvInvDt,@laFileStru,@laFileIndx,lcAPvInvDt)
SELECT ApVInvDt
SET ORDER TO ITEM
IF SEEK(lcTktType+lcPONo)
LOCAL lnRecNo
SCAN REST WHILE  cIMTyp+cTktNo+cLotNo+cBomType+cOprCode+Item+Color+cDyelot = ;
lcTktType+lcPONo
SCATTER MEMVAR memo
lnRecNo = RECNO()
INSERT INTO (lcAPvInvDt) FROM MEMVAR
SELECT (lcAPvInvDt)
REPLACE cStatus WITH 'S', nRecNo WITH lnRecNo
ENDSCAN
ENDIF
*B128478,1 KHM 06/14/2005 [End]

IF !USED('APINVTKT')
DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
laOpenFile[ALEN(laOpenFile,1),1] = 'APINVTKT'
laOpenFile[ALEN(laOpenFile,1),2] = .T.
=gfOpenFile(oAriaApplication.DataDir+'APINVTKT','LNCONT','SH')
ENDIF

SELECT APINVTKT
*B128478,1 KHM 06/14/2005 Chaning the Select SQL to be SEEK and SCAN for
*B128478,1                optimization issues [Begin]
*SELECT *, 'S' AS cStatus,RECNO() AS nRecNo FROM ApInvTkt WHERE ;
*      cimtyp+ctktno+STR(lineno,6)+crsession = lcTktType+lcPONo ;
*      INTO DBF (oAriaApplication.WorkDir+lcAPInvTkt)
*INDEX ON cIMTyp+cTktNo+STR(LineNo,6)+cRSession TAG Ticket
*INDEX ON cVendCode+cApInvNo+cApVILNo+cRSession TAG (lcAPInvTkt) ADDITIVE
LOCAL laFileIndx
DIMENSION laFileIndx[2,2]
laFileIndx = ''
laFileIndx[1,1] = 'cIMTyp+cTktNo+STR(LineNo,6)+cRSession'
laFileIndx[1,2] = 'Ticket'

laFileIndx[2,1] = 'cVendCode+cApInvNo+cApVILNo+cRSession'
laFileIndx[2,2] = (lcAPInvTkt)

lnFileStru = AFIELDS(laFileStru)
DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'cStatus'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'nRecNo'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 12
laFileStru[lnFileStru+2,4] = 0
LOCAL lnX
FOR lnX = 7 TO 16
STORE '' TO laFileStru[lnFileStru+1,lnX],laFileStru[lnFileStru+2,lnX]
ENDFOR
STORE 0 TO laFileStru[lnFileStru+1,17],laFileStru[lnFileStru+2,17],;
laFileStru[lnFileStru+1,18],laFileStru[lnFileStru+2,18]

=gfCrtTmp(lcAPInvTkt,@laFileStru,@laFileIndx,lcAPInvTkt)

SELECT ApInvTkt
SET ORDER TO Ticket
IF SEEK(lcTktType+lcPONo)
SCAN REST WHILE cimtyp+ctktno+STR(lineno,6)+crsession = lcTktType+lcPONo
SCATTER MEMVAR memo
lnRecNo = RECNO()
INSERT INTO (lcAPInvTkt) FROM MEMVAR
SELECT (lcAPInvTkt)
REPLACE cStatus WITH 'S', nRecNo WITH lnRecNo
ENDSCAN
ENDIF
SELECT ApInvTkt
SET ORDER TO LNCONT
*B128478,1 KHM 06/14/2005 [End]

IF !USED('APINVHDR')
DIMENSION laOpenFile[ALEN(laOpenFile,1)+1,2]
laOpenFile[ALEN(laOpenFile,1),1] = 'APINVHDR'
laOpenFile[ALEN(laOpenFile,1),2] = .T.
=gfOpenFile(oAriaApplication.DataDir+'APINVHDR','VENDINV','SH')
ENDIF

*!*************************************************************
*! Name      : lfGetMaxLin
*! Developer : AHMED MAHER (AMH)
*! Date      : 9/15/2004
*! Purpose   : Get the maximum line no from invoicetkt and update
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetMaxLin()
*!*************************************************************
FUNCTION lfGetMaxLin
LPARAMETER lcVendCode,lcInvoice

PRIVATE lcSumStr, lcSumKey , lnDistLnNo  , lnRecNo , lnGenType , lcArea
lcArea = SELECT(0)
lnDistLnNo = 0
lnGenType  = 0
lcSumKey   = "cVendCode+cApInvNo"
lcSumStr   = ALLTRIM(lcVendCode) + ALLTRIM(lcInvoice)

SET DELETE OFF
SELECT MAX(VAL(cApdLinNo)) FROM (lcAPInvTkt) WHERE ALLTRIM(cVendCode)+ALLTRIM(cApInvNo) = lcSumStr  ;
INTO ARRAY laMaxDistLn
SET DELETE ON

IF _TALLY <> 0  AND laMaxDistLn[1] > 0
lnDistLnNo = laMaxDistLn[1]
ENDIF

SELECT (lcAPvInvDt)
lnRecNo = RECNO()
COUNT TO lnGenType FOR cImTyp = 'G'
IF BETWEEN (lnRecNo , 1 , RECCOUNT())
GOTO lnRecNo
ENDIF

SELECT (lcArea)
RETURN lnDistLnNo + lnGenType

*!*************************************************************
*! Name      : lfvRSession
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/15/2004
*! Purpose   : Validate Selected Document Receiving Session
*!*************************************************************
*! Passed Parameters  :  lcType    : Document Type
*!                       lcTicket  : Document #
*!                       llClrRead : Terminate Read
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRSession()
*!*************************************************************
FUNCTION lfvRSession
LPARAMETERS lcType,lcTicket,lcItem,lcCstType

LOCAL lnEngineBehavior
lnEngineBehavior = SET("EngineBehavior")
SET ENGINEBEHAVIOR 70
SELECT cRSession,Date,SUM(TotQTy) AS nRecQty,SUM(TotQty*EVALUATE('NFLANCOST'+lcCstType)) AS nRecAmnt;
FROM POSLN WHERE CBUSDOCU+CSTYTYPE+PO+CRSESSION+SHIPNO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD=lcType+lcTicket ;
AND !EMPTY(cRSession) AND Style=lcItem;
GROUP BY cStyType,Po,cRSession INTO CURSOR RcvSess
lnTally =  _TALLY
SET ENGINEBEHAVIOR lnEngineBehavior
RETURN lnTally

*!*************************************************************
*! Name      : lfGetWIPAc
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/15/2004
*! Purpose   : To get the WIP account based on Gl link code defined in poshdr file
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetWIPAc()
*!*************************************************************
FUNCTION lfGetWIPAc

LOCAL lcGlAcct , lnArea
lcGlAcct = ""
lnArea= SELECT(0)

SELECT (loFormSet.lcPosHdr)
IF SEEK(LINK_CODE+'013','GL_LINK')
lcGlAcnt = GL_LINK.GLACNT
IF !SEEK(lcGlAcnt,'lcLinkChar')
lcGlAcnt = ''
ENDIF
ELSE
=SEEK('DEFDEF','GL_LINK')
lcGlAcnt = GL_LINK.GLACNT
ENDIF

SELECT (lnArea)
RETURN lcGlAcct

*!*************************************************************
*! Name      : lfChckACst
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/15/2004
*! Purpose   : Function to calculate the actual cost and check
*!             that none of the actual cost will have a negative amount.
*! Tracking# : E302010,1
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : .T. If none of the actual cost will have
*!                          a negative amount.
*!                      .F. Otherwise.
*!*************************************************************
*! Example            : =lfChckACst()
*!*************************************************************
*
FUNCTION lfChckACst

PRIVATE llReturn  , lcExStatus , lcMssgText ,;
lcHdrFile , lcHActFild , lcHFActFld , lcHKeyVal , lnActCstNo ,;
lcDetFile , lcDActFild , lcDFActFld , lcDKey    , lcDForCond ,;
lcDKeyVal , lcDQtyFild , m.cIMTyp   , lcBomType , lnAmntChng ,;
lnCount   , lcCount

llReturn = .T.

STORE '' TO lcExStatus , lcMssgText

*-- Create a temp. file to hold the calculated actual cost for PO and CT
*-- header files.
DIMENSION laFileStru[17,18]
laFileStru[01,1] = 'cIMTyp'
laFileStru[01,2] = 'C'
laFileStru[01,3] = 1
laFileStru[01,4] = 0

laFileStru[02,1] = 'cTktNo'
laFileStru[02,2] = 'C'
laFileStru[02,3] = 6
laFileStru[02,4] = 0

laFileStru[03,1] = 'nRecNo'
laFileStru[03,2] = 'N'
laFileStru[03,3] = 10
laFileStru[03,4] = 0

laFileStru[04,1] = 'nActCost1'
laFileStru[04,2] = 'N'
laFileStru[04,3] = 13
laFileStru[04,4] = 3

laFileStru[05,1] = 'nActCost2'
laFileStru[05,2] = 'N'
laFileStru[05,3] = 13
laFileStru[05,4] = 3

laFileStru[06,1] = 'nActCost3'
laFileStru[06,2] = 'N'
laFileStru[06,3] = 13
laFileStru[06,4] = 3

laFileStru[07,1] = 'nActCost4'
laFileStru[07,2] = 'N'
laFileStru[07,3] = 13
laFileStru[07,4] = 3

laFileStru[08,1] = 'nActCost5'
laFileStru[08,2] = 'N'
laFileStru[08,3] = 13
laFileStru[08,4] = 3

laFileStru[09,1] = 'nActCost6'
laFileStru[09,2] = 'N'
laFileStru[09,3] = 13
laFileStru[09,4] = 3

laFileStru[10,1] = 'nActCost7'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 13
laFileStru[10,4] = 3

laFileStru[11,1] = 'nFActCost1'
laFileStru[11,2] = 'N'
laFileStru[11,3] = 13
laFileStru[11,4] = 3

laFileStru[12,1] = 'nFActCost2'
laFileStru[12,2] = 'N'
laFileStru[12,3] = 13
laFileStru[12,4] = 3

laFileStru[13,1] = 'nFActCost3'
laFileStru[13,2] = 'N'
laFileStru[13,3] = 13
laFileStru[13,4] = 3

laFileStru[14,1] = 'nFActCost4'
laFileStru[14,2] = 'N'
laFileStru[14,3] = 13
laFileStru[14,4] = 3

laFileStru[15,1] = 'nFActCost5'
laFileStru[15,2] = 'N'
laFileStru[15,3] = 13
laFileStru[15,4] = 3

laFileStru[16,1] = 'nFActCost6'
laFileStru[16,2] = 'N'
laFileStru[16,3] = 13
laFileStru[16,4] = 3

laFileStru[17,1] = 'nFActCost7'
laFileStru[17,2] = 'N'
laFileStru[17,3] = 13
laFileStru[17,4] = 3

FOR lnI = 7 TO 16
FOR lnJ = 1 TO 17
laFileStru[lnJ,lnI] = ''
ENDFOR
ENDFOR
FOR lnJ = 1 TO 17
STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
ENDFOR

=gfCrtTmp(lcActCstHd,@laFileStru,'cIMTyp+cTktNo',lcActCstHd)

*-- Create a temp. file to hold the calculated actual cost for PO and CT
*-- detail files.
DIMENSION laFileStru[23,18]
laFileStru[01,1] = 'cIMTyp'
laFileStru[01,2] = 'C'
laFileStru[01,3] = 1
laFileStru[01,4] = 0

laFileStru[02,1] = 'cTktNo'
laFileStru[02,2] = 'C'
laFileStru[02,3] = 6
laFileStru[02,4] = 0

laFileStru[03,1] = 'nRecNo'
laFileStru[03,2] = 'N'
laFileStru[03,3] = 10
laFileStru[03,4] = 0

laFileStru[04,1] = 'cRSession'
laFileStru[04,2] = 'C'
laFileStru[04,3] = 6
laFileStru[04,4] = 0

laFileStru[05,1] = 'cShipNo'
laFileStru[05,2] = 'C'
laFileStru[05,3] = 6
laFileStru[05,4] = 0

laFileStru[06,1] = 'cItem'
laFileStru[06,2] = 'C'
laFileStru[06,3] = 19
laFileStru[06,4] = 0

laFileStru[07,1] = 'cColor'
laFileStru[07,2] = 'C'
laFileStru[07,3] = 6
laFileStru[07,4] = 0

laFileStru[08,1] = 'cLineNo'
laFileStru[08,2] = 'C'
laFileStru[08,3] = 6
laFileStru[08,4] = 0

laFileStru[09,1] = 'nTotQty'
laFileStru[09,2] = 'N'
laFileStru[09,3] = 10
laFileStru[09,4] = 3

laFileStru[10,1] = 'nActCost1'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 13
laFileStru[10,4] = 3

laFileStru[11,1] = 'nActCost2'
laFileStru[11,2] = 'N'
laFileStru[11,3] = 13
laFileStru[11,4] = 3

laFileStru[12,1] = 'nActCost3'
laFileStru[12,2] = 'N'
laFileStru[12,3] = 13
laFileStru[12,4] = 3

laFileStru[13,1] = 'nActCost4'
laFileStru[13,2] = 'N'
laFileStru[13,3] = 13
laFileStru[13,4] = 3

laFileStru[14,1] = 'nActCost5'
laFileStru[14,2] = 'N'
laFileStru[14,3] = 13
laFileStru[14,4] = 3

laFileStru[15,1] = 'nActCost6'
laFileStru[15,2] = 'N'
laFileStru[15,3] = 13
laFileStru[15,4] = 3

laFileStru[16,1] = 'nActCost7'
laFileStru[16,2] = 'N'
laFileStru[16,3] = 13
laFileStru[16,4] = 3

laFileStru[17,1] = 'nFActCost1'
laFileStru[17,2] = 'N'
laFileStru[17,3] = 13
laFileStru[17,4] = 3

laFileStru[18,1] = 'nFActCost2'
laFileStru[18,2] = 'N'
laFileStru[18,3] = 13
laFileStru[18,4] = 3

laFileStru[19,1] = 'nFActCost3'
laFileStru[19,2] = 'N'
laFileStru[19,3] = 13
laFileStru[19,4] = 3

laFileStru[20,1] = 'nFActCost4'
laFileStru[20,2] = 'N'
laFileStru[20,3] = 13
laFileStru[20,4] = 3

laFileStru[21,1] = 'nFActCost5'
laFileStru[21,2] = 'N'
laFileStru[21,3] = 13
laFileStru[21,4] = 3

laFileStru[22,1] = 'nFActCost6'
laFileStru[22,2] = 'N'
laFileStru[22,3] = 13
laFileStru[22,4] = 3

laFileStru[23,1] = 'nFActCost7'
laFileStru[23,2] = 'N'
laFileStru[23,3] = 13
laFileStru[23,4] = 3

FOR lnI = 7 TO 16
FOR lnJ = 1 TO 23
laFileStru[lnJ,lnI] = ''
ENDFOR
ENDFOR
FOR lnJ = 1 TO 23
STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
ENDFOR

=gfCrtTmp(lcActCstDt,@laFileStru,'cIMTyp+cRSession+cShipNo+cTKtNo+cItem+cColor+cLineNo',lcActCstDt)

lcExStatus = SET('EXACT')
SET EXACT OFF

*-- Recall the deleted records
SELECT (lcAPInvTkt)
RECALL ALL

*-- Recall the deleted records and set the needed order tag
SELECT (lcApVInvDt)
RECALL ALL
SET ORDER TO TAG ITEM
GO TOP

*-- Do while there is no troubles in the records locking and the end of file
*-- has not been reached yet.
DO WHILE llReturn .AND. !EOF()

*-- lcHdrFile   Variable to hold the name of the header file for the
*--             current document type.
*-- lcHActFild  Variable to hold the name of the actual cost field in the
*--             header file for the current document type.
*-- lcHFActFld  Variable to hold the name of the actual cost in foreign
*--             currency field in the header file for the current
*--             document type.
*-- lcHKeyVal   Variable to hold the key value to be used while searching
*--             for the record that will be updated in the header file.
*-- lcDetFile   Variable to hold the name of the detail file for the
*--             current document type.
*-- lcDActFild  Variable to hold the name of the actual cost field in the
*--             detail file for the current document type.
*-- lcDFActFld  Variable to hold the name of the actual cost in foreign
*--             currency field in the detail file for the current
*--             document type.
*-- lcDKey      Variable to hold the expression that will be used to get
*--             the key value that will be used for searching for the
*--             record that will be updated in the detail file.
*-- lcDForCond  Variable to hold a string of the expression that will be
*--             used as the FOR expression while searching for the record
*--             that will be updated in the detail file.
*-- lcDQtyFild  Variable to hold the name of the total quantity field in
*--             the detail file for the current document type.

STORE '' TO lcHdrFile  , lcHActFild , lcHFActFld , lcHKeyVal  ,;
lcDetFile  , lcDActFild , lcDFActFld , lcDKey     ,;
lcDForCond , lcDQtyFild


*-- lnActCstNo  Variable to hold the number of actual cost fields for the
*--             current document type.

STORE 0  TO lnActCstNo
lcHdrFile  = loFormSet.lcPosHdr
lcHActFild = 'nAct_Cost'
lcHFActFld = 'nFActCost'
lcHKeyVal  = loFormSet.lcBusDocu+loFormSet.lcStyType
lnActCstNo = IIF(cIMTyp = 'T' , 4 , 7)
lcDetFile  = 'POSLN'
lcDActFild = 'NACT_COST'
lcDFActFld = 'NFACTCOST'
lcDKey     = "'"+loFormSet.lcBusDocu+loFormSet.lcStyType+"'+cTKtNo+cRSession"
lcDForCond = "Style = lfGetItem(EVALUATE(lcAPInvTkt+'.Item'),EVALUATE(lcAPInvTkt+'.Color'),loFormSet) AND "+;
"STR(LineNo , 6) = STR(EVALUATE(lcAPInvTkt+'.LineNo') , 6)"
lcDQtyFild = 'TotQty'
m.cIMTyp   = cIMTyp

*-- Scan while there no troubles in the records locking and the document
*-- type didn't change for the records with status <> same
*-- Note: The seek() function in the for condition is not used as a
*--       condition it's used to replace the record pointer on the first
*--       corresponding record in the file (lcAPInvTkt).
SCAN WHILE llReturn .AND. cIMTyp = m.cIMTyp;
FOR cStatus <> 'S' .AND.;
SEEK(cVendCode + cApInvNo + cAPVILNo , lcAPInvTkt)
lcVendInv = cVendCode + cApInvNo
SELECT APINVHDR
STORE '*' TO lcExSin1 ,lcExSin2
IF SEEK(lcVendInv)
lcExSin1  = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
lnExRate  = APINVHDR.nExRate
lnCurUnit = APINVHDR.nCurrUnit
ELSE
lnExRate  = 1
lnCurUnit = 1
ENDIF
SELECT (lcApVInvDt)
*-- Flag to know if the record has been deleted
llDeleted = (cStatus='D') OR ((nRecNo = 0) AND (cStatus= 'S'))
*-- Variable to hold the record number of the corresponding record in
*-- the master file
lnRecNo   = nRecNo

*-- Replace the record pointer on the corresponding record in the
*-- master file.
IF BETWEEN( lnRecNo ,1, RECCOUNT('APVINVDT'))
GO lnRecNo IN APVINVDT
ENDIF    && End of IF lnRecNo > 0

lcBomType  = cBomType

*-- Get the change that has been made to the applied amount for this
*-- record
lnAmntChng = IIF(llDeleted   , 0 , nApAprAmnt) -;
IIF(lnRecNo = 0 , 0 , APVINVDT.nApAprAmnt)

*-- If there is a header file that should be updated for the current
*-- document type.
IF !EMPTY(lcHdrFile)

*-- If there is no records for this document number in the actual
*-- cost temp. header file
IF !SEEK(m.cIMTyp + cTktNo , lcActCstHd)
m.cTktNo = cTktNo

*-- Go to the record that will be updated in the header file
SELECT (lcHdrFile)
LOCATE
IF FOUND()

*-- Attempt to lock the record
m.nRecNo = RECNO()

*B608731,1 WAM 10/27/2008 Fix actual cost calculations while close CT
*STORE 0 TO m.nActCost1  , m.nActCost2  , m.nActCost3 , m.nActCost4  , m.nActCost5  ,;
m.nFActCost1 , m.nFActCost2 , m.nFActCost3 ,m.nFActCost4 , m.nFActCost5
STORE 0 TO m.nActCost1 , m.nActCost2 , m.nActCost3 , m.nActCost4 , m.nActCost5 , m.nActCost6  , m.nActCost7,;
m.nFActCost1, m.nFActCost2, m.nFActCost3, m.nFActCost4, m.nFActCost5, m.nFActCost6, m.nFActCost7
*B608731,1 WAM 10/27/2008 (End)

*-- If there is no fields for the actual cost in foreign currency
IF EMPTY(lcHFActFld)
*-- Get the actual cost
FOR lnCount = 1 TO lnActCstNo
lcCount            = STR(lnCount,1)
m.nActCost&lcCount = EVALUATE(lcHActFild+lcCount)
ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
ELSE    && Else [If there is fields for the actual cost in foreign currency]
*-- Get the actual cost
FOR lnCount = 1 TO lnActCstNo
lcCount             = STR(lnCount,1)
m.nActCost&lcCount  = EVALUATE(lcHActFild+lcCount)
m.nFActCost&lcCount = EVALUATE(lcHFActFld+lcCount)
ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
ENDIF    && End of IF EMPTY(lcHFActFld)

*-- Add a corresponding record in the temp. file
INSERT INTO (lcActCstHd) FROM MEMVAR

ENDIF

ENDIF    && End of IF !SEEK(m.cIMTyp + cTktNo , lcActCstHd)

*-- Update total actual cost in the temp. header file
SELECT (lcActCstHd)

REPLACE ('nActCost'+lcBomType) WITH EVALUATE("nActCost"+lcBomType+" + (lnAmntChng "+;
lcExSin1+" lnExRate "+lcExSin2+" lnCurUnit)")

*-- Update total actual cost in foreign currency in the temp. header
*-- file
IF !EMPTY(lcHFActFld)
REPLACE ('nFActCost'+lcBomType) WITH EVALUATE('nFActCost'+lcBomType) + lnAmntChng
ENDIF    && End of IF !EMPTY(lcHFActFld)
ENDIF    && End of IF !EMPTY(lcHdrFile)

SELECT (lcAPInvTkt)
*-- Scan the corresponding records in the detail file (lcAPInvTkt) with
*-- receiving session not empty.

SCAN REST WHILE cVendCode + cApInvNo + cApVILNo = EVALUATE(lcApVInvDt+'.cVendCode') +;
EVALUATE(lcApVInvDt+'.cApInvNo') + EVALUATE(lcApVInvDt+'.cAPVILNo');
FOR !EMPTY(cRSession)
lcVendInv = cVendCode + cApInvNo
SELECT APINVHDR
STORE '*' TO lcExSin1 ,lcExSin2
IF SEEK(lcVendInv)
lcExSin1  = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
lnExRate  = APINVHDR.nExRate
lnCurUnit = APINVHDR.nCurrUnit
ELSE
lnExRate  = 1
lnCurUnit = 1
ENDIF
SELECT (lcAPInvTkt)

*-- Flag to know if the record has been deleted
llDeleted = (cStatus='D') OR ((nRecNo = 0) AND (cStatus= 'S'))
*-- Variable to hold the record number of the corresponding record in
*-- the master file
lnRecNo   = nRecNo

*-- Replace the record pointer on the corresponding record in the
*-- master file.
IF BETWEEN(lnRecNo ,1,RECCOUNT('APINVTKT'))
GO lnRecNo IN APINVTKT
ENDIF

*-- Get the change that has been made to the applied amount for this
*-- record

lnAmntChng = IIF(llDeleted   , 0 , nApAplAmnt) -;
IIF(lnRecNo = 0 , 0 , APINVTKT.nApAplAmnt)

*-- If there is no records for this document number in the actual
*-- cost temp. detail file

IF !SEEK(cIMTyp+cRSession+EVALUATE(lcApVInvDt+'.ShipNo')+cTKtNo+Item+Color+STR(LineNo,6),lcActCstDt)

*B608731,1 WAM 10/27/2008 Fix actual cost calculations while close CT
*STORE 0 TO m.nActCost1  , m.nActCost2  , m.nActCost3  ,m.nActCost4  , m.nActCost5  ,;
m.nFActCost1 , m.nFActCost2 , m.nFActCost3 ,m.nFActCost4 , m.nFActCost5
STORE 0 TO m.nActCost1 , m.nActCost2 , m.nActCost3 , m.nActCost4 , m.nActCost5 , m.nActCost6 , m.nActCost7  ,;
m.nFActCost1, m.nFActCost2, m.nFActCost3, m.nFActCost4, m.nFActCost5, m.nFActCost6, m.nFActCost7
*B608731,1 WAM 10/27/2008 (End)

m.cIMTyp    = cIMTyp
m.cRSession = cRSession
m.cShipNo   = EVALUATE(lcApVInvDt+'.ShipNo')
m.cTktNo    = cTKtNo
m.cItem     = Item
m.cColor    = Color
m.cLineNo   = STR(LineNo,6)
*-- Go to the record that will be updated in the header file
lcDKeyVal = EVALUATE(lcDKey)
SELECT (lcDetFile)
LLSEEK = SEEK (lcDKeyVal)
LOCATE REST WHILE EVALUATE(SYS(14,VAL(SYS(21)))) = lcDKeyVal FOR EVALUATE(lcDForCond)

*-- Attempt to lock the record
m.nRecNo    = RECNO()
m.nTotQty   = EVALUATE(lcDQtyFild)

*-- If there is no fields for the actual cost in foreign currency
IF EMPTY(lcDFActFld)

*-- Get the actual cost
FOR lnCount = 1 TO lnActCstNo
lcCount            = ALLTRIM(STR(lnCount))
m.nActCost&lcCount = EVALUATE(lcDActFild+lcCount)
ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
ELSE    && Else [If there is fields for the actual cost in foreign currency]

*-- Get the actual cost
FOR lnCount = 1 TO lnActCstNo
lcCount             = ALLTRIM(STR(lnCount))
m.nActCost&lcCount  = EVALUATE(lcDActFild+lcCount)
m.nFActCost&lcCount = EVALUATE(lcDFActFld+lcCount)
ENDFOR    && End of FOR lnCount = 1 TO lnActCstNo
ENDIF    && End of IF EMPTY(lcDFActFld)

*-- Add a corresponding record in the temp. file
INSERT INTO (lcActCstDt) FROM MEMVAR
ENDIF    && End of IF !SEEK(cIMTyp + cRSession + ...

SELECT (lcActCstDt)

*-- Update total actual cost in the temp. detail file
REPLACE ('nActCost'+lcBomType) WITH EVALUATE("nActCost"+lcBomType+" + ((lnAmntChng / nTotQty) "+;
lcExSin1+" lnExRate "+lcExSin2+" lnCurUnit)")

*-- Update total actual cost in foreign currency in the temp. detail
*-- file
IF !EMPTY(lcDFActFld)
REPLACE ('nFActCost'+lcBomType) WITH EVALUATE('nFActCost'+lcBomType) + (lnAmntChng / nTotQty)
ENDIF    && End of IF !EMPTY(lcDFActFld)
ENDSCAN    && End of SCAN REST WHILE cVendCode + cApInvNo + cApVILNo ...

SELECT (lcApVInvDt)
ENDSCAN    && End of   SCAN WHILE llReturn .AND. cIMTyp = m.cIMTyp ...
ENDDO    && End of DO WHILE llReturn .AND. !EOF()

*-- If there was no troubles found in the records locking
IF llReturn

*-- Search for a record with negative actual cost in the temp. actual
*-- cost header file
SELECT (lcActCstHd)
LOCATE FOR nActCost1 < 0 .OR.;
nActCost2 < 0 .OR.;
nActCost3 < 0 .OR.;
nActCost4 < 0 .OR.;
nActCost5 < 0 .OR.;
nActCost6 < 0 .OR.;
nActCost7 < 0

*-- If there is a record with negative actual cost in the temp. actual
*-- cost header file
IF FOUND()
llReturn  = .F.

*-- Get the variable part of the error message
DO CASE
CASE cIMTyp = 'I'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMssgText = LANG_MFCSSH_POORDERS
lcMssgText = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_POORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_POORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE cIMTyp = 'M'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMssgText = LANG_MFCSSH_CTORDERS
lcMssgText = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CTORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_CTORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE cIMTyp = 'T'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMssgText = LANG_MFCSSH_MMORDERS
lcMssgText = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MMORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_MMORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE cIMTyp = 'D'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMssgText = LANG_MFCSSH_DOORDERS
lcMssgText = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DOORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_DOORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE cIMTyp = 'N'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMssgText = LANG_MFCSSH_NLORDERS
lcMssgText = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NLORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_NLORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDCASE
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMssgText = lcMssgText + ' ' + LANG_MFCSSH_NO + ' ' + cTKtNo
lcMssgText = lcMssgText + ' ' +;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NO,loFormSet.GetHeaderText("LANG_MFCSSH_NO",loFormSet.HeaderAlias)) + ' ' + cTKtNo
*N000682,1 11/20/2012 MMT Globlization changes[End]


*-- Message : 04188
*-- "The debit amount applied to (lcMssgText) will produce a negative "
*-- "actual cost. Cannot proceed with the saving process.             "
*-- Button : 00000
*-- "                          <    Ok    >                           "
=gfModalGen("TRM04188B00000" , "DIALOG" , lcMssgText)
ENDIF    && End of IF FOUND()
ENDIF    && End of IF llReturn

*-- If there was no troubles found in the records locking and no records
*-- with negative actual cost found in the temp. actual cost header file.
IF llReturn

*-- Search for a record with negative actual cost in the temp. actual
*-- cost detail file.
SELECT (lcActCstDt)
LOCATE FOR nActCost1 < 0 .OR.;
nActCost2 < 0 .OR.;
nActCost3 < 0 .OR.;
nActCost4 < 0 .OR.;
nActCost5 < 0 .OR.;
nActCost6 < 0 .OR.;
nActCost7 < 0

*-- If there is a record with negative actual cost in the temp. actual
*-- cost detail file
IF FOUND()
llReturn  = .F.

*-- Get the variable part of the error message
DO CASE
CASE cIMTyp = 'I'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMssgText = LANG_MFCSSH_POORDERS
lcMssgText = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_POORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_POORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE cIMTyp = 'M'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMssgText = LANG_MFCSSH_CTORDERS
lcMssgText = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CTORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_CTORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE cIMTyp = 'T'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMssgText = LANG_MFCSSH_MMORDERS
lcMssgText = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MMORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_MMORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE cIMTyp = 'D'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMssgText = LANG_MFCSSH_DOORDERS
lcMssgText = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DOORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_DOORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE cIMTyp = 'N'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMssgText = LANG_MFCSSH_NLORDERS
lcMssgText = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NLORDERS,loFormSet.GetHeaderText("LANG_MFCSSH_NLORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDCASE
lcMssgText = lcMssgText + ' ' +;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NO,loFormSet.GetHeaderText("LANG_MFCSSH_NO",loFormSet.HeaderAlias)) +;
' ' + cTKtNo    + ', ' +;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RECVSESS,loFormSet.GetHeaderText("LANG_MFCSSH_RECVSESS",loFormSet.HeaderAlias)) +;
' ' +;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NO,loFormSet.GetHeaderText("LANG_MFCSSH_NO",loFormSet.HeaderAlias)) +;
' ' + cRSession + ', ' +;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_LINE,loFormSet.GetHeaderText("LANG_MFCSSH_LINE",loFormSet.HeaderAlias))     +;
' ' + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NO,loFormSet.GetHeaderText("LANG_MFCSSH_NO",loFormSet.HeaderAlias)) + ' ' +;
ALLTRIM(cLineNo)



*-- Message : 04188
*-- "The debit amount applied to (lcMssgText) will produce a negative "
*-- "actual cost. Cannot proceed with the saving process.             "
*-- Button : 00000
*-- "                          <    Ok    >                           "
=gfModalGen("TRM04188B00000" , "DIALOG" , lcMssgText)
ENDIF    && End of IF FOUND()
ENDIF    && End of IF llReturn

*-- If troubles where found in the records locking or some of the records
*-- in the temp. actual cost header file or detail file where found.
IF !llReturn

*-- Delete the records that have status same or deleted from the temp.
*-- files (lcApVInvDt) and (lcAPInvTkt)
SELECT (lcApVInvDt)
DELETE ALL FOR INLIST(cStatus,'D','S')
SELECT (lcAPInvTkt)
DELETE ALL FOR INLIST(cStatus,'D','S')

ENDIF    && End of IF !llReturn
SET EXACT &lcExStatus

RETURN llReturn

*!*************************************************************
*! Name      : lfUpdFiles
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/15/2004
*! Purpose   : Update Master Files
*!*************************************************************
*! Calls     : lfOpenFile(),gfTmp2Mast()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdFiles()
*!*************************************************************
FUNCTION lfUpdFiles

LOCAL lcExStatus
=lfUpdatPOF()

m.cImTyp    = loFormSet.lcTranType
m.cType     = '3'
m.CtktNo    = EVALUATE(loFormSet.lcPosHdr+'.PO')

*B608731,1 WAM 10/27/2008 Fix actual cost calculations while close CT
*IF lfOpenSql('BOMLINE','BOMLINE','BOMLINE','CIMTYP+CTYPE+CTKTNO',loFormSet)
IF !lfOpenSql('BOMLINE','BOMLINE','BOMLINE','CIMTYP+CTYPE+CTKTNO',loFormSet)
*B608731,1 WAM 10/27/2008 (End)

RETURN .F.
ENDIF

lcExStatus = SET('EXACT')
SET EXACT OFF
SELECT (lcAPInvTkt)
RECALL ALL
SELECT (lcApVInvDt)
RECALL ALL
SCAN FOR cStatus <> 'S' AND SEEK(cVendCode+cApInvNo+cAPVILNo,lcAPInvTkt)
llDeleted = (cStatus='D') OR (cStatus='S' AND EMPTY(nRecNo))
lnRecNo   = nRecNo
=SEEK(EVALUATE(lcAPvInvDt+'.cvendcode')+EVALUATE(lcAPvInvDt+'.capinvno'),'APINVHDR')
IF BETWEEN(lnRecNo ,1,RECCOUNT('APVINVDT'))
SELECT APVINVDT
GO lnRecNo
ENDIF

SELECT (loFormSet.lcCTktBom)
LOCATE FOR Typ = EVALUATE(lcApVInvDt+'.cbomtype') AND cInvType = SPACE(4) AND Item = SPACE(19) AND;
MfgCode = EVALUATE(lcApVInvDt+'.cOprCode') AND Dyelot = SPACE(10)
IF FOUND()
REPLACE ISSUE_QTY WITH ISSUE_QTY - IIF(lnRecNo=0,0,APVINVDT.nApTAprQty) + IIF(llDeleted,0,EVALUATE(lcApVInvDt+'.nApTAprQty')),;
USED_QTY  WITH USED_QTY  - IIF(lnRecNo=0,0,APVINVDT.nApTAprQty) + IIF(llDeleted,0,EVALUATE(lcApVInvDt+'.nApTAprQty'))
ENDIF

*-- If the Invoice line was deleted or if the uncontributed amount was
*-- changed.
IF llDeleted .OR. (EMPTY(EVALUATE(lcAPInvTkt+'.cRSession')) .AND. EVALUATE(lcAPInvTkt+'.cStatus') <> 'S')
IF BETWEEN(EVALUATE(lcAPInvTkt+'.nRecNo') ,1,RECCOUNT('APINVTKT'))
GO EVALUATE(lcAPInvTkt+'.nRecNo') IN APINVTKT
ENDIF
SELECT BOMCOST
LOCATE FOR cBomType+cInvType+Item+MfgCode+cWareCode+cDyelot+cRSession+cISession = EVALUATE(lcApVInvDt+'.cbomtype')+;
SPACE(4)+SPACE(19)+EVALUATE(lcApVInvDt+'.cOprCode')+SPACE(6)+SPACE(10)+SPACE(6)+SPACE(6) AND;
cVendCode+cApInvNo = PADR(EVALUATE(lcAPvInvDt+'.cvendcode'),8)+PADR(EVALUATE(lcAPvInvDt+'.capinvno'),12)
DO CASE
CASE !llDeleted AND !FOUND() .AND. EVALUATE(lcAPInvTkt+'.cStatus') <> 'D'
APPEND BLANK
REPLACE CBOMTYPE  WITH EVALUATE(lcApVInvDt+'.cbomtype')  ,;
CIMTYP    WITH EVALUATE(lcApVInvDt+'.cIMTyp')    ,;
CTKTNO    WITH EVALUATE(lcApVInvDt+'.ctktno')    ,;
ITEM      WITH ''                                ,;
CINVTYPE  WITH ''                                ,;
MFGCODE   WITH EVALUATE(lcApVInvDt+'.cOprCode')  ,;
CWARECODE WITH ''                                ,;
CDYELOT   WITH ''                                ,;
CRSESSION WITH SPACE(6)                          ,;
CVENDCODE WITH EVALUATE(lcAPvInvDt+'.cvendcode') ,;
CAPINVNO  WITH EVALUATE(lcAPvInvDt+'.capinvno')  ,;
CISESSION WITH ''                                ,;
SHIPNO    WITH ''                                ,;
DTRANDATE WITH APINVHDR.DPOSTDATE                ,;
CCOSTTYPE WITH EVALUATE(lcApVInvDt+'.cCostType') ,;
NTOTQTY   WITH EVALUATE(lcAPInvTkt+'.nAPTAplQty') ;
-IIF(EVALUATE(lcAPInvTkt+'.nRecNo');
=0,0,APINVTKT.nAPTAplQty)         ,;
NTOTCST   WITH EVALUATE(lcAPInvTkt+'.nAPAplAmnt') ;
-IIF(EVALUATE(lcAPInvTkt+'.nRecNo');
=0,0,APINVTKT.nAPAplAmnt)         ,;
NUNITCST  WITH EVALUATE(lcAPInvTkt+'.nAPAplPric') ;
-IIF(EVALUATE(lcAPInvTkt+'.nRecNo');
=0,0,APINVTKT.nAPAplPric)         ,;
NTOTACST  WITH NTOTCST                           ,;
NUNITACST WITH NUNITCST                          ,;
Actualize WITH 'N'

CASE !llDeleted AND FOUND()
REPLACE NTOTQTY   WITH NTOTQTY+IIF(EVALUATE(lcAPInvTkt+'.cStatus')='D',0,EVALUATE(lcAPInvTkt+'.nAPTAplQty'));
-IIF(EVALUATE(lcAPInvTkt+'.nRecNo') =0  ,0,APINVTKT.nAPTAplQty)              ,;
NTOTCST   WITH NTOTCST+IIF(EVALUATE(lcAPInvTkt+'.cStatus')='D',0,EVALUATE(lcAPInvTkt+'.nAPAplAmnt'));
-IIF(EVALUATE(lcAPInvTkt+'.nRecNo') =0  ,0,APINVTKT.nAPAplAmnt)              ,;
NUNITCST  WITH IIF(EVALUATE(lcAPInvTkt+'.cStatus')='D',0,NTOTCST/NTOTQTY)                          ,;
NTOTACST  WITH NTOTCST                                                                             ,;
NUNITACST WITH NUNITCST
IF (EVALUATE(lcAPInvTkt+'.cStatus') = 'D') .OR. (NTOTQTY = 0)
DELETE
ENDIF

CASE llDeleted AND FOUND()
REPLACE NTOTQTY   WITH NTOTQTY -IIF(EVALUATE(lcAPInvTkt+'.nRecNo')=0,0,APINVTKT.nAPTAplQty) ,;
NTOTCST   WITH NTOTCST -IIF(EVALUATE(lcAPInvTkt+'.nRecNo')=0,0,APINVTKT.nAPAplAmnt) ,;
NUNITCST  WITH NUNITCST-IIF(EVALUATE(lcAPInvTkt+'.nRecNo')=0,0,APINVTKT.nAPAplPric) ,;
NTOTACST  WITH NTOTCST                                                              ,;
NUNITACST WITH NUNITCST
IF (EVALUATE(lcAPInvTkt+'.cStatus') = 'D') .OR. (NTOTQTY = 0)
DELETE
ENDIF
ENDCASE
ENDIF    && End of IF EMPTY(&lcAPInvTkt..cRSession) .AND. ...
SELECT (lcAPInvTkt)
SCAN REST WHILE cVendCode+cApInvNo+cAPVILNo = EVALUATE(lcApVInvDt+'.cVendCode')+EVALUATE(lcApVInvDt+'.cApInvNo')+;
EVALUATE(lcApVInvDt+'.cAPVILNo') FOR !EMPTY(cRSession)
llDeleted = (cStatus='D') OR (cStatus='S' AND EMPTY(nRecNo))
lnRecNo = nRecNo

IF !llDeleted AND BETWEEN(lnRecNo ,1,RECCOUNT('APINVTKT'))
SELECT APINVTKT
GO lnRecNo
ENDIF

SELECT BomLine
LOCATE FOR cbomtyp+mfgcode+crsession+shipno+STR(lineno,6)+cInvType+style=EVALUATE(lcApVInvDt+'.cBomType')+;
EVALUATE(lcApVInvDt+'.cOprCode')+EVALUATE(lcAPInvTkt+'.cRSession')+EVALUATE(lcApVInvDt+'.shipno')+;
STR(EVALUATE(lcApInvTkt+'.LineNo'),6)+loFormSet.lcInvType+EVALUATE(lcApInvTkt+'.Item')
DO CASE
CASE !llDeleted AND !FOUND()
APPEND BLANK
REPLACE cimtyp    WITH EVALUATE(lcApVInvDt+'.cIMTyp')    ,;
ctype     WITH '3'                               ,;
cbomtyp   WITH EVALUATE(lcApVInvDt+'.cBomType')  ,;
mfgcode   WITH EVALUATE(lcApVInvDt+'.cOprCode')  ,;
ctktno    WITH EVALUATE(lcApVInvDt+'.ctktno')    ,;
crsession WITH EVALUATE(lcAPInvTkt+'.cRSession') ,;
shipno    WITH EVALUATE(lcApVInvDt+'.shipno')    ,;
lineno    WITH EVALUATE(lcAPInvTkt+'.LineNo')    ,;
style     WITH EVALUATE(lcAPInvTkt+'.Item')      ,;
cinvType  WITH loFormSet.lcInvType               ,;
cCatgTyp  WITH EVALUATE(lcApVInvDt+'.cCostType') ,;
UnitQty   WITH  1                                ,;
StyQty    WITH EVALUATE(lcAPInvTkt+'.nApTAplQty'),;
ItemQty   WITH StyQty                            ,;
ItemAmt   WITH EVALUATE(lcAPInvTkt+'.nApAplAmnt'),;
UnitCost  WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)

CASE !llDeleted AND FOUND()
REPLACE StyQty   WITH StyQty - IIF(lnRecNo=0,0,APINVTKT.nApTAplQty) + EVALUATE(lcAPInvTkt+'.nApTAplQty'),;
ItemQty  WITH StyQty ,;
ItemAmt  WITH ItemAmt- IIF(lnRecNo=0,0,APINVTKT.nApAplAmnt) + EVALUATE(lcAPInvTkt+'.nApAplAmnt'),;
UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
CASE llDeleted AND FOUND()
REPLACE StyQty   WITH StyQty - EVALUATE(lcAPInvTkt+'.nApTAplQty') ,;
ItemQty  WITH StyQty ,;
ItemAmt  WITH ItemAmt - EVALUATE(lcAPInvTkt+'.nApAplAmnt'),;
UnitCost WITH IIF(ItemQty=0,0,ItemAmt/ItemQty)
IF ItemQty = 0
DELETE
ENDIF
ENDCASE

SELECT BOMCOST
*! B610177,1 SAB 12/16/2012 Fix Problem of closing Cost Sheet for Contributed Invoices [Start]
*LOCATE FOR cBomType+cInvType+Item+MfgCode+cWareCode+cDyelot+cRSession+cISession = EVALUATE(lcApVInvDt+'.cbomtype')+;
SPACE(4)+SPACE(19)+SPACE(6)+EVALUATE(lcApVInvDt+'.cOprCode')+SPACE(6)+SPACE(10)+;
EVALUATE(lcAPInvTkt+'.cRSession')+SPACE(6) AND;
cVendCode+cApInvNo = PADR(EVALUATE(lcAPvInvDt+'.cvendcode'),8)+PADR(EVALUATE(lcAPvInvDt+'.capinvno'),12)
LOCATE FOR cBomType+cInvType+Item+MfgCode+cWareCode+cDyelot+cRSession+cISession = EVALUATE(lcApVInvDt+'.cbomtype')+;
SPACE(4)+SPACE(19)+EVALUATE(lcApVInvDt+'.cOprCode')+SPACE(6)+SPACE(10)+;
EVALUATE(lcAPInvTkt+'.cRSession')+SPACE(6) AND;
cVendCode+cApInvNo = PADR(EVALUATE(lcAPvInvDt+'.cvendcode'),8)+PADR(EVALUATE(lcAPvInvDt+'.capinvno'),12)
*! B610177,1 SAB 12/16/2012 Fix Problem of closing Cost Sheet for Contributed Invoices [End]
DO CASE
CASE !llDeleted AND !FOUND()
APPEND BLANK
REPLACE CBOMTYPE  WITH EVALUATE(lcApVInvDt+'.cbomtype')  ,;
CIMTYP    WITH EVALUATE(lcApVInvDt+'.cIMTyp')    ,;
CTKTNO    WITH EVALUATE(lcApVInvDt+'.ctktno')    ,;
ITEM      WITH ''                                ,;
CINVTYPE  WITH ''                                ,;
MFGCODE   WITH EVALUATE(lcApVInvDt+'.cOprCode')  ,;
CWARECODE WITH ''                                ,;
CDYELOT   WITH ''                                ,;
CRSESSION WITH EVALUATE(lcAPInvTkt+'.cRSession') ,;
CVENDCODE WITH EVALUATE(lcAPvInvDt+'.cvendcode') ,;
CAPINVNO  WITH EVALUATE(lcAPvInvDt+'.capinvno')  ,;
CISESSION WITH ''                                ,;
SHIPNO    WITH ''                                ,;
DTRANDATE WITH APINVHDR.DPOSTDATE                ,;
CCOSTTYPE WITH EVALUATE(lcApVInvDt+'.cCostType') ,;
NTOTQTY   WITH EVALUATE(lcAPInvTkt+'.nApTAplQty'),;
NTOTCST   WITH EVALUATE(lcAPInvTkt+'.nApAplAmnt'),;
NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
NTOTACST  WITH NTOTCST                           ,;
NUNITACST WITH NUNITCST                          ,;
Actualize WITH 'Y'

CASE !llDeleted AND FOUND()
REPLACE NTOTQTY   WITH NTOTQTY - IIF(lnRecNo=0,0,APINVTKT.nApTAplQty) + EVALUATE(lcAPInvTkt+'.nApTAplQty') ,;
NTOTCST   WITH NTOTCST - IIF(lnRecNo=0,0,APINVTKT.nApAplAmnt) + EVALUATE(lcAPInvTkt+'.nApAplAmnt') ,;
NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
NTOTACST  WITH NTOTCST  ,;
NUNITACST WITH NUNITCST ,;
Actualize WITH 'Y'

CASE llDeleted AND FOUND()
REPLACE NTOTQTY   WITH NTOTQTY -  IIF(lnRecNo=0,0,EVALUATE(lcAPInvTkt+'.nApTAplQty')) ,;
NTOTCST   WITH NTOTCST -  IIF(lnRecNo=0,0,EVALUATE(lcAPInvTkt+'.nApAplAmnt')) ,;
NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
NTOTACST  WITH NTOTCST  ,;
NUNITACST WITH NUNITCST ,;
Actualize WITH 'Y'
IF NTOTQTY = 0
DELETE
ENDIF
ENDCASE
ENDSCAN
ENDSCAN

SELECT (lcApVInvDt)
DELETE ALL FOR INLIST(cStatus,'D','S')
SELECT (lcAPInvTkt)
DELETE ALL FOR INLIST(cStatus,'D','S')
=gfTmp2Mast('APVINVDT',lcAPvInvDt)
=gfTmp2Mast('APINVTKT',lcAPInvTkt)

DECLARE laTableUpdate[5,2]
laTableUpdate[1,1] = loFormSet.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'

laTableUpdate[2,1] = 'BOMLINE'
laTableUpdate[2,2] = 'BOMLINE'

laTableUpdate[3,1] = loFormSet.lcPosHdr
laTableUpdate[3,2] = 'POSHDR'

laTableUpdate[4,1] = 'POSLN'
laTableUpdate[4,2] = 'POSLN'

laTableUpdate[5,1] = 'BOMCOST'
laTableUpdate[5,2] = 'BOMCOST'

=lfTableUpdate(loFormSet)

SET ORDER TO TAG (lcAPInvTkt) IN (lcAPInvTkt)
SET EXACT &lcExStatus

*!*************************************************************
*! Name      : lfUpDatPOF
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/16/2004
*! Purpose   : Function to Update the PO files
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  =lfUpDatPOF()
*!*************************************************************
*
FUNCTION lfUpdatPOF
PRIVATE lcFile , lcIMTyp , lnRecNo , lcScatFlds , lcGathFlds , laScatVal

*-- Update and unlock the header files [Begin]
SELECT (lcActCstHd)
GO TOP

DO WHILE !EOF()
STORE '' TO lcFile , lcIMTyp , lcScatFlds , lcGathFlds
DIMENSION laScatVal[1]
laScatVal = 0

*-- Get the name of the header file, the scatter fields and the gather
*-- fields for the current document type to update it and unlock the
*-- records that had been locked.
lcFile     = loFormSet.lcPosHdr
lcScatFlds = 'nActCost1 ,nActCost2 ,nActCost3 ,nActCost4 ,nActCost5 ,nActCost6 ,nActCost7 ,;
nFActCost1,nFActCost2,nFActCost3,nFActCost4,nFActCost5,nFActCost6,nFActCost7'
lcGathFlds = 'LIKE nAct_Cost? , nFActCost?'
lcIMTyp    = cIMTyp
*-- Scan while the document type didn't change
SCAN WHILE cIMTyp = lcIMTyp

*-- Update the records of the master file
SCATTER FIELDS &lcScatFlds. TO laScatVal
lnRecNo = nRecNo
SELECT (lcFile)
GO lnRecNo
GATHER FIELDS &lcGathFlds. FROM laScatVal
SELECT (lcActCstHd)
ENDSCAN    && End of SCAN WHILE cIMTyp = lcIMTyp
ENDDO    && End of DO WHILE !EOF()

*-- Erase the actual cost header temp. file
USE

*-- Update and unlock the header files [End]


*-- Update and unlock the detail files [Begin]
SELECT (lcActCstDt)
GO TOP

DO WHILE !EOF()
STORE '' TO lcFile , lcIMTyp , lcScatFlds , lcGathFlds
DIMENSION laScatVal[1]
laScatVal = 0

*-- Get the name of the detail file, the scatter fields and the gather
*-- fields for the current document type to update it and unlock the
*-- records that had been locked.
lcFile     = 'POSLN'
lcScatFlds = 'nActCost1 ,nActCost2 ,nActCost3 ,nActCost4 ,nActCost5 ,nActCost6 ,nActCost7 ,;
nFActCost1,nFActCost2,nFActCost3,nFActCost4,nFActCost5,nFActCost6,nFActCost7'
lcGathFlds = 'LIKE nEActCost? , nAct_Cst?'
lcIMTyp = cIMTyp

*-- Scan while the document type didn't change
SCAN WHILE cIMTyp = lcIMTyp
*-- Update the records of the master file
SCATTER FIELDS &lcScatFlds TO laScatVal
lnRecNo = nRecNo
SELECT (lcFile)
GO lnRecNo
GATHER FIELDS &lcGathFlds FROM laScatVal
SELECT (lcActCstDt)
ENDSCAN    && End of SCAN WHILE cIMTyp = lcIMTyp
ENDDO    && End of DO WHILE !EOF()

*-- Erase the actual cost detail temp. file
USE

*!*************************************************************
*! Name      : lfCloseFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 9/16/2004
*! Purpose   : Close opened files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCloseFile()
*!*************************************************************
FUNCTION lfCloseFile

FOR lnCount = 1 TO ALEN(laOpenFile,1)
IF laOpenFile[lnCount,2]
SELECT (laOpenFile[lnCount,1])
USE
ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfFormInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/22/2003
*! Purpose   : function called from the INIT event of the form
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfFormInit
LPARAMETERS loFormSet

loFormSet.oParentForm = loFormSet
=lfAddPro(loFormSet)

loFormSet.lcStyleTyp = SUBSTR("IMTMI",AT(loFormSet.lcTranType,"IMTDN"),1)

*-- Define the Item file.
LOCAL lcTmpItem
lcTmpItem = gfTempName()
loFormSet.lcItemFile  = IIF(loFormSet.lcStyleTyp = 'T' , lcTmpItem , 'Style')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcItemLabel = IIF(loFormSet.lcStyleTyp = 'T' , LANG_MFCSSH_FABRIC , LANG_MFCSSH_STYLE)
loFormSet.lcItemLabel = IIF(loFormSet.lcStyleTyp = 'T' ,;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_FABRIC,loFormSet.GetHeaderText("LANG_MFCSSH_FABRIC",loFormSet.HeaderAlias)) , ;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_STYLE,loFormSet.GetHeaderText("LANG_MFCSSH_STYLE",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]


=loFormSet.ariaForm1.mfcstsc.mparentinit(loFormSet.lcTranType="N")

IF loFormSet.lcStyleTyp = 'M'
loFormSet.AddProperty('lcPWTol'  ,'')
loFormSet.AddProperty('llNewSess',.T.)
ENDIF

loFormSet.lcPosHdr   = gfTempName()
loFormSet.lcPosLn    = gfTempName()
loFormSet.lcCtktBom  = gfTempName()
loFormSet.lcBomLine  = gfTempName()
loFormSet.lcBom      = gfTempName()
loFormSet.lcMfgOprHd = gfTempName()
loFormSet.lcMfgOprDt = gfTempName()

IF loFormSet.llPwInst .AND. loFormSet.lcTranType = 'M'
loFormSet.lcPWTol   = gfTempName()
ENDIF

loFormSet.cBrowseAliasName = loFormSet.lcPosHdr
loFormSet.cBrowseKey       = loFormSet.lcBusDocu+loFormSet.lcStyType
loFormSet.cBrowseIndexExpression = "CBUSDOCU+CSTYTYPE+PO"
loFormSet.cBrowseIndexFields = "CBUSDOCU,CSTYTYPE,PO"
LOCAL llNoRecordFound, llServerError
loFormSet.oRemoteCursor.mGetCursor(loFormSet.cBrowseAliasName,loFormSet.cBrowseAliasName,loFormSet.DataSessionID,;
'STRUCTURE',.F.,loFormSet.cBrowseTableName,'*','',loFormSet.cBrowseIndexName,;
loFormSet.cBrowseIndexExpression,.F.,loFormSet.cBrowseIndexFields,0,;
loFormSet.cBrowseFilter,loFormSet.cBrowseKey,@llNoRecordFound,;
@llServerError)
SELECT (loFormSet.lcPosHdr)

*--- Open the SQL tables.
m.cBusDocu = loFormSet.lcBusDocu
m.cStyType = loFormSet.lcStyType
m.cImTyp   = loFormSet.lcTranType
m.CutTkt   = SPACE(6)
IF !llServerError AND lfOpenSql('CTKTBOM',loFormSet.lcCtktBom,'CTKTBOM','CIMTYP+CUTTKT',loFormSet) AND;
gfOpenFile(oAriaApplication.DataDir+'CODES',oAriaApplication.DataDir+'CCODE_NO','SH')
WITH loFormSet.ariaForm1
.kbPo.keytextbox.ControlSource   = loFormSet.lcPosHdr+".PO"
.txtStatus.ControlSource         = 'ThisFormSet.lcStatus'
.kbItem.keytextbox.ControlSource = loFormSet.lcPosHdr+".STYLE"
WITH .pgfCstSht
WITH .page2
.txtBudget.ControlSource   = 'ThisFormSet.lnOprBud'
.txtReceived.ControlSource = 'ThisFormSet.lnOprRec'
.txtCanceled.ControlSource = 'ThisFormSet.lnOprCan'
.txtDamaged.ControlSource  = 'ThisFormSet.lnOprDam'
.txtOpen.ControlSource     = 'ThisFormSet.lnOprOpn'
ENDWITH
WITH .page4
.txtBudget.ControlSource      = loFormSet.lcPosHdr+".NSTYORDER"
.txtReceived.ControlSource    = loFormSet.lcPosHdr+".RECEIVE"
.txtCanceled.ControlSource    = loFormSet.lcPosHdr+".CANCEL"
.txtDamaged.ControlSource     = loFormSet.lcPosHdr+".DAMAGE"
.txtOpen.ControlSource        = loFormSet.lcPosHdr+".OPEN"
.txtEntered.ControlSource     = loFormSet.lcPosHdr+".entered"
.TxtComplete.ControlSource    = loFormSet.lcPosHdr+".Complete"
.txtEstCost1.ControlSource    = loFormSet.lcPosHdr+".NICOST1"
.txtEstCost2.ControlSource    = loFormSet.lcPosHdr+".NICOST2"
.txtEstCost3.ControlSource    = loFormSet.lcPosHdr+".NICOST3"
.txtEstCost4.ControlSource    = loFormSet.lcPosHdr+".NICOST4"
.txtEstCost5.ControlSource    = loFormSet.lcPosHdr+".NICOST5"
.txtEstCost6.ControlSource    = loFormSet.lcPosHdr+".NICOST6"
.txtEstCost7.ControlSource    = loFormSet.lcPosHdr+".NICOST7"
.txtLndCost1.ControlSource    = loFormSet.lcPosHdr+".NLAN_COST1"
.txtLndCost2.ControlSource    = loFormSet.lcPosHdr+".NLAN_COST2"
.txtLndCost3.ControlSource    = loFormSet.lcPosHdr+".NLAN_COST3"
.txtLndCost4.ControlSource    = loFormSet.lcPosHdr+".NLAN_COST4"
.txtLndCost5.ControlSource    = loFormSet.lcPosHdr+".NLAN_COST5"
.txtLndCost6.ControlSource    = loFormSet.lcPosHdr+".NLAN_COST6"
.txtLndCost7.ControlSource    = loFormSet.lcPosHdr+".NLAN_COST7"
.txtActCost1.ControlSource    = loFormSet.lcPosHdr+".NACT_COST1"
.txtActCost2.ControlSource    = loFormSet.lcPosHdr+".NACT_COST2"
.txtActCost3.ControlSource    = loFormSet.lcPosHdr+".NACT_COST3"
.txtActCost4.ControlSource    = loFormSet.lcPosHdr+".NACT_COST4"
.txtActCost5.ControlSource    = loFormSet.lcPosHdr+".NACT_COST5"
.txtActCost6.ControlSource    = loFormSet.lcPosHdr+".NACT_COST6"
.txtActCost7.ControlSource    = loFormSet.lcPosHdr+".NACT_COST7"
.txtTotalEst.ControlSource    = "lfTotals('I',ThisFormSet)"
.txtTotalLanded.ControlSource = "lfTotals('LAN_',ThisFormSet)"
.txtTotalActual.ControlSource = "lfTotals('ACT_',ThisFormSet)"
.txtAvrgEst.ControlSource     = "lfAvrgs('I',ThisFormSet)"
.txtAvrgLanded.ControlSource  = "lfAvrgs('LAN_',ThisFormSet)"
.txtAvrgActual.ControlSource  = "lfAvrgs('ACT_',ThisFormSet)"
.cboStyWare.ControlSource     = 'ThisFormSet.lnTkStWare'
.cboMatWare.ControlSource     = 'ThisFormSet.lnTkMtWare'
.cntLinkCode.kbGlLink.KeyTextBox.ControlSource = loFormSet.lcPosHdr+".LINK_CODE"
ENDWITH
ENDWITH
ENDWITH
loFormSet.llOpenSql = .T.
ELSE
RETURN .F.
ENDIF

*-- Add more buttons to the control pannel.
DIMENSION loFormSet.laPanelObj[2,6]
loFormSet.laPanelObj[1,1] = "cmdClose"
loFormSet.laPanelObj[1,2] = oAriaApplication.BitmapHome + "CLS_SHET.BMP"
loFormSet.laPanelObj[1,3] = "mvClose"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[1,4] = LANG_MFCSSH_CLOSE &&'Close Cost Sheet'
loFormSet.laPanelObj[1,4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CLOSE,loFormSet.GetHeaderText("LANG_MFCSSH_CLOSE",loFormSet.HeaderAlias)) &&'Close Cost Sheet'
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[1,5] = LANG_MFCSSH_CLOSE &&'Close Cost Sheet'
loFormSet.laPanelObj[1,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CLOSE,loFormSet.GetHeaderText("LANG_MFCSSH_CLOSE",loFormSet.HeaderAlias)) &&'Close Cost Sheet'
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.laPanelObj[1,6] = "V"
loFormSet.laPanelObj[2,1] = "cmdGenerate"
loFormSet.laPanelObj[2,2] = oAriaApplication.BitmapHome + "GENERAT.BMP"
loFormSet.laPanelObj[2,3] = "mvGenerate"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[2,4] = LANG_MFCSSH_GENRT &&'Generate Cost Sheet'
loFormSet.laPanelObj[2,4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_GENRT,loFormSet.GetHeaderText("LANG_MFCSSH_GENRT",loFormSet.HeaderAlias)) &&'Generate Cost Sheet'
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[2,5] = LANG_MFCSSH_GENRT &&'Generate Cost Sheet'
loFormSet.laPanelObj[2,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_GENRT,loFormSet.GetHeaderText("LANG_MFCSSH_GENRT",loFormSet.HeaderAlias)) &&'Generate Cost Sheet'
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.laPanelObj[2,6] = "V"

*--- if Sql files not opened.
IF !loFormSet.llOpenSql
loFormSet.activemode = "S"
RETURN .F.
ENDIF

IF TYPE('loFormSet.lcTranNo')='C'
LOCAL llNoRecordFound, llServerError
loFormSet.oRemoteCursor.mGetCursor(loFormSet.lcPosHdr,loFormSet.lcPosHdr,loFormSet.DataSessionID,'FIRST',.F.,;
loFormSet.cBrowseTableName,'*','',loFormSet.cBrowseIndexName,;
loFormSet.cBrowseIndexExpression,.F.,loFormSet.cBrowseIndexFields,1,;
"cBusDocu = '"+loFormSet.lcBusDocu+"' and cStyType ='"+loFormSet.lcStyType+;
"' and PO ='"+PADR(loFormSet.lcTranNo,6)+"'",loFormSet.cBrowseKey,;
@llNoRecordFound,@llServerError)
IF !llServerError AND !llNoRecordFound
SELECT (loFormSet.lcPosHdr)
loFormSet.activemode = "V"
ENDIF
ENDIF

LOCAL lnFileStru
LOCAL ARRAY laFileStru[1,18]
SELECT (loFormSet.lcCtktBom)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+3,18]

*--- Doc. [Start]
*--- cShowType == Variable to holds '0' for Header record '1' for detail lines
*--- Doc. [End]

laFileStru[lnFileStru+1,1] = 'cShowType'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'TotStk'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 13
laFileStru[lnFileStru+2,4] = 3

laFileStru[lnFileStru+3,1] = 'UomUse'
laFileStru[lnFileStru+3,2] = 'C'
laFileStru[lnFileStru+3,3] = 6
laFileStru[lnFileStru+3,4] = 0

LOCAL lnI,lnJ
FOR lnI = 7 TO 16
laFileStru[lnFileStru+1,lnI] = ''
laFileStru[lnFileStru+2,lnI] = ''
laFileStru[lnFileStru+3,lnI] = ''
ENDFOR
STORE 0 TO laFileStru[lnFileStru+1,17],laFileStru[lnFileStru+1,18],;
laFileStru[lnFileStru+2,17],laFileStru[lnFileStru+2,18],;
laFileStru[lnFileStru+3,17],laFileStru[lnFileStru+3,18]

LOCAL ARRAY laIndex[2,2]
laIndex[1,1] = 'cimtyp+cuttkt+typ+cInvType+item+mfgcode+dyelot'
laIndex[1,2] = 'CTKTBOM'
laIndex[2,1] = 'TYP+cShowType+cInvType+ITEM+MFGCODE+DYELOT'
laIndex[2,2] = loFormSet.lcTktSheet
=gfCrtTmp(loFormSet.lcTktSheet,@laFileStru,@laIndex)
SET ORDER TO TAG (loFormSet.lcTktSheet) IN (loFormSet.lcTktSheet)

DIMENSION laFileStru[17,18]

laFileStru[01,1] = 'Item'
laFileStru[01,2] = 'C'
laFileStru[01,3] = 19
laFileStru[01,4] = 0

laFileStru[02,1] = 'cInvType'
laFileStru[02,2] = 'C'
laFileStru[02,3] = 4
laFileStru[02,4] = 0

laFileStru[03,1] = 'LineNo'
laFileStru[03,2] = 'N'
laFileStru[03,3] = 6
laFileStru[03,4] = 0

laFileStru[04,1] = 'cDyelot'
laFileStru[04,2] = 'C'
laFileStru[04,3] = 10
laFileStru[04,4] = 0

laFileStru[05,1] = 'cWareCode'
laFileStru[05,2] = 'C'
laFileStru[05,3] = 6
laFileStru[05,4] = 0

laFileStru[06,1] = 'nQty1'
laFileStru[06,2] = 'N'
laFileStru[06,3] = 6
laFileStru[06,4] = 0

laFileStru[07,1] = 'nQty2'
laFileStru[07,2] = 'N'
laFileStru[07,3] = 6
laFileStru[07,4] = 0

laFileStru[08,1] = 'nQty3'
laFileStru[08,2] = 'N'
laFileStru[08,3] = 6
laFileStru[08,4] = 0

laFileStru[09,1] = 'nQty4'
laFileStru[09,2] = 'N'
laFileStru[09,3] = 6
laFileStru[09,4] = 0

laFileStru[10,1] = 'nQty5'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 6
laFileStru[10,4] = 0

laFileStru[11,1] = 'nQty6'
laFileStru[11,2] = 'N'
laFileStru[11,3] = 6
laFileStru[11,4] = 0

laFileStru[12,1] = 'nQty7'
laFileStru[12,2] = 'N'
laFileStru[12,3] = 6
laFileStru[12,4] = 0

laFileStru[13,1] = 'nQty8'
laFileStru[13,2] = 'N'
laFileStru[13,3] = 6
laFileStru[13,4] = 0

laFileStru[14,1] = 'nTotQty'
laFileStru[14,2] = 'N'
laFileStru[14,3] = 10
laFileStru[14,4] = 3

laFileStru[15,1] = 'nRecNo'
laFileStru[15,2] = 'N'
laFileStru[15,3] = 10
laFileStru[15,4] = 0

laFileStru[16,1] = 'cFrstOpr'
laFileStru[16,2] = 'C'
laFileStru[16,3] = 6
laFileStru[16,4] = 0

laFileStru[17,1] = 'nFrstOprSq'
laFileStru[17,2] = 'N'
laFileStru[17,3] = 2
laFileStru[17,4] = 0

FOR lnI = 7 TO 16
FOR lnJ = 1 TO 17
laFileStru[lnJ,lnI] = ''
ENDFOR
ENDFOR
FOR lnI = 1 TO 17
STORE 0 TO laFileStru[lnI,17],laFileStru[lnI,18]
ENDFOR

=gfCrtTmp(loFormSet.lcTmpTkt,@laFileStru,"cInvType+Item+STR(LineNo,6)",loFormSet.lcTmpTkt)
=IIF(loFormSet.laSetups[1,2]='Y',gfOpenFile(oAriaApplication.DataDir+'Gl_Link',oAriaApplication.DataDir+'Gl_Link1','SH'),.T.)
*E303957,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]
*!*	=IIF(loFormSet.laSetups[1,2]='Y',gfOpenFile(oAriaApplication.DataDir+'GlDIST' ,oAriaApplication.DataDir+'GlDISTAC','SH'),.T.)
=IIF(loFormSet.laSetups[1,2]='Y',gfOpenTable(oAriaApplication.DataDir+'GlDIST' ,oAriaApplication.DataDir+'GlDISTAC','SH'),.T.)
*E303957,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [End]
IF loFormSet.laSetups[1,2]='Y' .AND. !USED(loFormSet.lcGlDTemp)
SELECT GLDIST
=AFIELDS(laFileStru)
=gfCrtTmp(loFormSet.lcGlDTemp,@laFileStru)
ENDIF

IF loFormSet.laSetups[1,2]='Y'
GO TOP IN 'GL_LINK'
IF EOF('GL_LINK')
*Message : 40048
*No General Ledger link codes have been setup. Cannot Proceed
*Button : 00000
*Ok
=gfModalGen('TRM40048B00000','ALERT')
loFormSet.activemode = "S"
RETURN .F.
ENDIF
ENDIF

SELECT (loFormSet.lcPosHdr)

*--- Doc. [Start]
*--- llAutoGen == Parameter to generate cost sheet automaticaly
*---              if called from cuttkt/po progeam
*--- Doc. [End]

*!*  *C200477,1 AMH Add option menu to issue/return rolls [Start]
*!*  IF ASCAN(laEvntTrig,PADR("ADOPTROL",10)) <> 0
*!*    =gfDoTriger("POCSSH",PADR("ADOPTROL",10))
*!*  ENDIF
*!*  *C200477,1 AMH [End]

*-- To pass the required information for the work order key #.
WITH loFormSet.ariaform1.kbPo
.cbusinessdocumenttype = loFormSet.lcBusDocu
.cworkordertype        = loFormSet.lcStyType
.obrowsecursor         = loFormSet.lcPosHdr
.cbrowsetitle          = loFormSet.lcBrowTitl
.cbrowsefields         = loFormSet.lcBrowFlds
ENDWITH

WITH loFormSet.ariaForm1
LOCAL cCaptionEnd     && Variable to get the session number to add at the end of form caption.
cCaptionEnd = IIF(AT('/',.Caption)=0,'',ALLTRIM(RIGHT(.Caption,4)))

* Put the correct form caption in all cases.
DO CASE
CASE lcTranType = 'I'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Caption = LANG_MFCSSH_POCSTSHT
.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_POCSTSHT,loFormSet.GetHeaderText("LANG_MFCSSH_POCSTSHT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE lcTranType = 'M'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Caption = LANG_MFCSSH_CTCSTSHT
.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CTCSTSHT,loFormSet.GetHeaderText("LANG_MFCSSH_CTCSTSHT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

.lblItem.Caption = lcMjrHdr
CASE lcTranType = 'T'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Caption = LANG_MFCSSH_MMCSTSHT
.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MMCSTSHT,loFormSet.GetHeaderText("LANG_MFCSSH_MMCSTSHT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

.lblItem.Caption = lcMjrHdr
CASE lcTranType = 'D'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Caption = LANG_MFCSSH_DOCSTSHT
.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DOCSTSHT,loFormSet.GetHeaderText("LANG_MFCSSH_DOCSTSHT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE lcTranType = 'N'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Caption = LANG_MFCSSH_NLCSTSHT
.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_NLCSTSHT,loFormSet.GetHeaderText("LANG_MFCSSH_NLCSTSHT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDCASE

.Caption = .Caption + cCaptionEnd
.lblPo.Caption = loFormSet.lcTktText
.kbPo.Keytextbox.InputMask = lfPicture()
.kbPo.Keytextbox.Format = lfPicture()
IF loFormSet.lcTranType $ 'MT'
LOCAL lnOldWidth
lnOldWidth = .lblItem.Width
.lblItem.Width = (7*(LEN(.lblItem.Caption))+10)
.lbl2.Left = .lbl2.Left - lnOldWidth + .lblItem.Width
.kbItem.Left = .kbItem.Left - lnOldWidth + .lblItem.Width
.lblItem.Visible = .T.
.lbl2.Visible = .T.

LOCAL lcItemPic
lcItemPic = loFormSet.lcMjrMsk
.kbItem.Keytextbox.Width = (7*(LEN(lcItemPic))+10)
.kbItem.KeyCmd.Left = .kbItem.Keytextbox.Width + 5
.kbItem.Keytextbox.InputMask = STRTRAN(lcItemPic,'X','!')
.kbItem.Keytextbox.Format = STRTRAN(lcItemPic,'X','!')
.kbItem.Visible = .T.
ENDIF

WITH .pgfCstSht.page4
.cntLinkCode.txtGlName.ControlSource = "lfLinkDesc(thisformset)"
.lblCostElement1.Caption = loFormSet.laSetups[3,2]
.lblCostElement2.Caption = loFormSet.laSetups[4,2]
.lblCostElement3.Caption = loFormSet.laSetups[5,2]
.lblCostElement4.Caption = loFormSet.laSetups[6,2]
.lblCostElement5.Caption = loFormSet.laSetups[7,2]
.lblCostElement6.Caption = loFormSet.laSetups[8,2]
.lblCostElement7.Caption = loFormSet.laSetups[9,2]
IF loFormSet.lcTranType = 'T'
.lblCostElement5.Visible = .F.
.lblCostElement6.Visible = .F.
.lblCostElement7.Visible = .F.
.txtEstCost5.Visible = .F.
.txtEstCost6.Visible = .F.
.txtEstCost7.Visible = .F.
.txtLndCost5.Visible = .F.
.txtLndCost6.Visible = .F.
.txtLndCost7.Visible = .F.
.txtActCost5.Visible = .F.
.txtActCost6.Visible = .F.
.txtActCost7.Visible = .F.
ENDIF

IF loFormSet.lcTranType = 'N'
LOCAL lnI,lcI,lnJumpStep
lnI = 2
lnJumpStep = 0
FOR lnI = 2 TO 7
lcI = STR(lnI,1)
IF laSetups[14+lnI,2] $ 'MD'
IF lnJumpStep > 0
.lblCostElement&lcI..Top = .lblCostElement&lcI..Top - (21*lnJumpStep)
.txtEstCost&lcI..Top = .txtEstCost&lcI..Top - (21*lnJumpStep)
.txtLndCost&lcI..Top = .txtLndCost&lcI..Top - (21*lnJumpStep)
.txtActCost&lcI..Top = .txtActCost&lcI..Top - (21*lnJumpStep)
ENDIF
ELSE
.lblCostElement&lcI..Visible = .F.
.txtEstCost&lcI..Visible = .F.
.txtLndCost&lcI..Visible = .F.
.txtActCost&lcI..Visible = .F.
lnJumpStep = lnJumpStep + 1
ENDIF
ENDFOR
ENDIF

IF !(loFormSet.laSetups[2,2]='Y' .AND. (loFormSet.llStyCost .OR. loFormSet.llMatCost)) .AND. !loFormSet.laSetups[1,2] = 'Y'
.shpWareLink.Visible = .F.
ELSE
IF !(loFormSet.laSetups[2,2]='Y' .AND. (loFormSet.llStyCost .OR. loFormSet.llMatCost)) .OR. !loFormSet.laSetups[1,2] = 'Y'
.shpWareLink.Height = 36
ENDIF
ENDIF

IF loFormSet.laSetups[2,2]='Y'
IF loFormSet.llStyCost
.lblStyWare.Visible = .T.
.lbl8.Visible = .T.
.cboStyWare.Visible = .T.
IF loFormSet.llMatCost
.lblMatWare.Visible = .T.
.lbl9.Visible = .T.
.cboMatWare.Visible = .T.
ENDIF
ELSE
IF loFormSet.llMatCost
.lblMatWare.Left = .lblStyWare.Left
.lbl9.Left = .lbl8.Left
.cboMatWare.Left = .cboStyWare.Left
.lblMatWare.Visible = .T.
.lbl9.Visible = .T.
.cboMatWare.Visible = .T.
ENDIF
ENDIF
IF loFormSet.laSetups[1,2] = 'Y'
.cntLinkCode.Visible = .T.
ENDIF
ELSE
IF loFormSet.laSetups[1,2] = 'Y'
.cntLinkCode.Top = .lblStyWare.Top
.cntLinkCode.Visible = .T.
ENDIF
ENDIF
ENDWITH
ENDWITH

*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*B609477,1 TMI 12/12/2010 [Start] check if llRecPO is defined first, also call DIRMAIN from within the aria4xp root
*!*	IF TYPE('oPoRecRef.Name') = 'C' AND ASCAN(oPoRecRef.laEvntTrig,PADR('MFRISSUDAT',10),1,ALEN(oPoRecRef.laEvntTrig,1),1) > 0 ;
*!*	     AND oPoRecRef.llRecPO
*!*	  DO lfMFRISSUDAT IN DIRMAIN.FXP WITH loformset
IF TYPE('oPoRecRef.Name') = 'C' AND ASCAN(oPoRecRef.laEvntTrig,PADR('MFRISSUDAT',10),1,ALEN(oPoRecRef.laEvntTrig,1),1) > 0 ;
  AND (TYPE('oPoRecRef.llRecPO')='L' AND oPoRecRef.llRecPO )
  DO lfMFRISSUDAT IN oariaapplication.defaultpath+'DIRMAIN.FXP' WITH loformset
  *B609477,1 TMI 12/12/2010 [End  ]
  RETURN .F.
ENDIF
*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

IF loFormSet.llAutoGen
  *--- Doc. [Start]
  *--- lfGetInfo   == Function to get the information of the cost sheet (Initilize)
  *--- lfvGenerate == Function to generate cost sheet
  *--- Doc. [End]
  
  *B608583,1 WAM 06/16/2008 Fix the problem of doubling average cost when receive non detailed costing styles
  SET DELETED ON
  *B608583,1 WAM 06/16/2008 (End)
  
  =lfGetInfo(loFormSet) .AND. lfvGenerate(loFormSet)
  =lfSavScr(loFormSet)
  
  *! C201141,1 HES 04/26/2009, please quote Processing Enbroidery orders for DCC [Start]
  IF TYPE('oPostyRef.Name') = 'C' AND ASCAN(oPostyRef.laEvntTrig,PADR('CRPONDCOSH',10),1,ALEN(oPostyRef.laEvntTrig,1),1) > 0
    *B609477,1 TMI 12/12/2010 [Start] add the oariaapplication.defaultpath to the called dirmain.fxp
    *DO lfMFISSUDATA IN DIRMAIN.FXP WITH loformset, IIF(TYPE('oPostyRef.llIfAut') <> 'U',oPostyRef.llIfAut,.F.)
    DO lfMFISSUDATA IN oariaapplication.defaultpath+'DIRMAIN.FXP' WITH loformset, IIF(TYPE('oPostyRef.llIfAut') <> 'U',oPostyRef.llIfAut,.F.)
    *B609477,1 TMI 12/12/2010 [End  ]
  ENDIF
  *! C201141,1 HES 04/26/2009, please quote Processing Enbroidery orders for DCC [End]
  
  loFormSet.activemode = "S"
  RETURN .F.
ENDIF

*B610723,1 TMI 05/05/2014 14:34 [Start] define a new property and save in it the value of gfUserPriv('MF','MFCSSH','TKTCLOSE')
IF TYPE('loFormSet.llTKTCLOSE') = 'U'
  loFormSet.AddProperty('llTKTCLOSE',gfUserPriv('MF','MFCSSH','TKTCLOSE'))
ENDIF 
*B610723,1 TMI 05/05/2014 14:34 [End  ] 

*-- end of lfFormInit.

*!*************************************************************
*! Name      : lfOpenSql
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/24/2003
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenSql
*LPARAMETERS lcTable,lcCursor,lcTagName,lcTagExp,loFormSet,llIniAlias
LPARAMETERS lcTable,lcCursor,lcTagName,lcTagExp,loFormSet,llIniAlias, lcOrderBy

LOCAL lnConnectionHandlar, lcSqlStatment, lnI, llFirstField
LOCAL ARRAY laFields[1]

oAriaApplication.RemoteCompanyData.mindexfields(lcTagExp,@laFields)
lcSqlStatment = "SELECT * FROM " + lcTable + "(index=" + lcTagName + ") WHERE "
llFirstField = .T.
FOR lnI = 1 TO ALEN(laFields)
lcSqlStatment = lcSqlStatment+IIF(llFirstField,""," AND ")+"["+LOWER(laFields[lnI])+"]=?m."+LOWER(laFields[lnI])
llFirstField = .F.
ENDFOR
*B607879,1 WAM 12/14/2006 Set Add ability to order selected records
IF TYPE('lcOrderBy') = 'C' AND !EMPTY(lcOrderBy)
lcSqlStatment = lcSqlStatment + ' ORDER BY '+lcOrderBy
ENDIF
*B607879,1 WAM 12/14/2006 (End)
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,;
oAriaApplication.ActiveCompanyConStr,3,'SAVE',loFormSet.DataSessionId)
IF lnConnectionHandlar = 1
IF llIniAlias
loFormSet.DataEnvironment.InitialSelectedAlias = lcCursor
ENDIF
ELSE
=oAriaApplication.RemoteCompanyData.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
RETURN .F.
ENDIF

=oAriaApplication.RemoteCompanyData.mCloneStandardCursor(lcCursor,loFormSet.DataSessionId)
*-- end of lfOpenSql.

*!*************************************************************
*! Name      : lfTotals
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/25/2003
*! Purpose   : function to Get the totals.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfTotals
LPARAMETERS lcFieldType,loFormSet

LOCAL lnI,lcI,lnTotal
lnTotal = 0
FOR lnI = 1 TO 7
lcI = STR(lnI,1)
lnTotal = lnTotal + EVALUATE(loFormSet.lcPosHdr+".N"+lcFieldType+"COST"+lcI)
ENDFOR
RETURN lnTotal
*--end of lfTotals.

*!*************************************************************
*! Name      : lfAvrgs
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/25/2003
*! Purpose   : function to Get the Averages.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfAvrgs
LPARAMETERS lcFieldType,loFormSet

*B608806,1 WAM 02/18/2009 Fix caculation of average unit cost in summary folder
*RETURN IIF(EVALUATE(loFormSet.lcPosHdr+".NSTYORDER")>0,ROUND(lfTotals(lcFieldType,loFormSet)/EVALUATE(loFormSet.lcPosHdr+".NSTYORDER"),3),'N/A')
lcPosHdr = loFormSet.lcPosHdr
DO CASE
CASE lcFieldType = 'I'
RETURN IIF(&lcPosHdr..NSTYORDER>0,ROUND(lfTotals(lcFieldType,loFormSet)/&lcPosHdr..NSTYORDER,3),'N/A')
CASE lcFieldType = 'LAN_'
RETURN IIF(&lcPosHdr..RECEIVE>0,ROUND(lfTotals(lcFieldType,loFormSet)/&lcPosHdr..RECEIVE,3),'N/A')
CASE lcFieldType = 'ACT_'
DO CASE
CASE &lcPosHdr..Status ='A' AND &lcPosHdr..PCS_ACT > 0
RETURN ROUND(lfTotals(lcFieldType,loFormSet)/&lcPosHdr..PCS_ACT,3)
CASE INLIST(&lcPosHdr..Status,'C','S') AND &lcPosHdr..RECEIVE > 0
RETURN ROUND(lfTotals(lcFieldType,loFormSet)/&lcPosHdr..RECEIVE,3)
OTHERWISE
RETURN 'N/A'
ENDCASE
ENDCASE
*B608806,1 WAM 02/18/2009 (End)
*--end of lfAvrgs.

*!*************************************************************
*! Name      : lfLinkDesc
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/25/2003
*! Purpose   : function to Get the Description of the Link Code
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfLinkDesc
LPARAMETERS loFormSet

RETURN IIF((loFormSet.laSetups[1,2]='Y') AND SEEK('05'+EVALUATE(loFormSet.lcPosHdr+".LINK_CODE"),'Gl_Link','Gl_Link1'),Gl_Link.LinkDesc,'')
*--end of lfLinkDesc.

*!*************************************************************
*! Name      : lfAfterInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/25/2003
*! Purpose   : function called from the INIT event of the form after the default code of AriaFormSet.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfAfterInit
LPARAMETERS loFormSet

SELECT(loFormSet.lcPosHdr)
WITH loFormSet.ariaForm1.pgfCstSht
WITH .Page1
WITH .grdCstSht
.RecordSource = loFormSet.lcTktSheet
.Column1.ControlSource = "IIF("+loFormSet.lcTktSheet+".cCatgTyp='M',gfCodDes("+loFormSet.lcTktSheet+".MfgCode,'MfgCode'),"+;
loFormSet.lcTktSheet+".Item)"
.Column1.CurrentControl = "Text1"
IF loFormSet.llUseDyelot
.Column2.ControlSource = loFormSet.lcTktSheet+".Dyelot"
.Column2.CurrentControl = "Text1"
.Column2.Visible = .T.
ENDIF
.Column3.ControlSource = loFormSet.lcTktSheet+".UomUse"
.Column3.CurrentControl = "Text1"
.Column4.ControlSource = loFormSet.lcTktSheet+".UntQty"
.Column3.CurrentControl = "Text1"
.Column5.ControlSource = loFormSet.lcTktSheet+".UntCost"
.Column3.CurrentControl = "Text1"
.Column6.ControlSource = "IIF("+loFormSet.lcTktSheet+".TotStk=0,'',"+loFormSet.lcTktSheet+".TotStk)"
.Column6.CurrentControl = "Text1"
.Column7.ControlSource = loFormSet.lcTktSheet+".Req_Qty"
.Column7.CurrentControl = "Text1"
.Column8.ControlSource = loFormSet.lcTktSheet+".Issue_Qty"
.Column8.CurrentControl = "Text1"
.Column9.ControlSource = loFormSet.lcTktSheet+".Used_Qty"
.Column9.CurrentControl = "Text1"

*--Set dyenamic colors.
.SetAll("Dynamicbackcolor", "", "Column")
.SetAll("Dynamicbackcolor","IIF("+loFormSet.lcTktSheet+".cShowType='0',16769996,16777215)", "Column")
.column1.DynamicBackColor="IIF("+loFormSet.lcTktSheet+".cShowType='0',12320767,16777215)"

*--Set dyenamic Font Bold.
.SetAll("DynamicFontBold", "", "Column")
.column1.DynamicFontBold="IIF("+loFormSet.lcTktSheet+".cShowType='0',.T.,.F.)"

.Refresh
ENDWITH
ENDWITH

WITH .Page2
WITH .grdOperations
.RecordSource = loFormSet.lcOprHdr
.Column1.ControlSource = loFormSet.lcOprHdr+".cOprCode"
.Column2.ControlSource = loFormSet.lcOprHdr+".lInHouse"
.Column3.ControlSource = loFormSet.lcOprHdr+".cOperSeq"
.Column4.ControlSource = "gfCodDes("+loFormSet.lcOprHdr+".cOprCode,'MFGCODE')"
.Column5.ControlSource = loFormSet.lcOprHdr+".cContCode"
.Column6.ControlSource = loFormSet.lcOprHdr+".cContName"
.Column7.ControlSource = loFormSet.lcOprHdr+".cOprComnt"
ENDWITH

=lfBrowLots(loFormSet)
ENDWITH

WITH .Page3
WITH .grdDetReq
.RecordSource = loFormSet.lcDetFile
.Column1.ControlSource = "IIF("+loFormSet.lcDetFile+".cShowType='0' .AND. !"+loFormSet.lcDetFile+".lHideTit,"+loFormSet.lcDetFile+".Style,'')"
.Column1.Header1.Caption = loFormSet.lcItmHdr
.Column3.ControlSource = "IIF("+loFormSet.lcDetFile+".cCatgTyp='M',gfCodDes("+loFormSet.lcDetFile+".MfgCode,'MfgCode'),"+;
loFormSet.lcDetFile+".Item)"
.Column4.ControlSource = loFormSet.lcDetFile+".Dyelot"
.Column5.ControlSource = loFormSet.lcDetFile+".UnitQty"
.Column6.ControlSource = loFormSet.lcDetFile+".UnitCost"
.Column7.ControlSource = loFormSet.lcDetFile+".StyQty"
.Column7.Visible = (loFormSet.lcTranType#'T')
.Column8.ControlSource = loFormSet.lcDetFile+".ItemQty"
.Column9.ControlSource = loFormSet.lcDetFile+".ItemAmt"
*N000587,1 WAM 12/01/2007 Get foreign and equivalent cost from PO cost sheet lines
.Column10.ControlSource = loFormSet.lcDetFile+".NEQU_AMT"
*N000587,1 WAM 12/01/2007 (End)

*--Set dyenamic colors.
.SetAll("Dynamicbackcolor", "", "Column")
.SetAll("Dynamicbackcolor","IIF("+loFormSet.lcDetFile+".cShowType='0',16769996,IIF("+loFormSet.lcDetFile+;
".cShowType='2',16763806,16777215))", "Column")
.column3.DynamicBackColor="IIF("+loFormSet.lcDetFile+".cShowType='0',12320767,IIF("+loFormSet.lcDetFile+;
".cShowType='2',16763806,16777215))"

*--Set dyenamic Input Mask.
.column6.DynamicInputMask="IIF("+loFormSet.lcDetFile+".cShowType$'02','','9999999.99999')"

.Refresh
ENDWITH
ENDWITH
ENDWITH

*--- Set the browse fileds.
loFormSet.ariaBrFields.edtBrowseFields.Value = loFormSet.lcBrowFlds
loFormSet.BrowseTitle = loFormSet.lcBrowTitl
*--end of lfAfterInit.

*!*************************************************************
*! Name      : lfAddPro
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/25/2003
*! Purpose   : function to Add properties to the FormSet.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfAddPro
LPARAMETERS loFormSet

LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
IF LEFT(laAllVar[lnI,1],2) = 'la'
LOCAL lnRow,lnCol,lcACopy
lnRow = ALEN(laAllVar[lnI,1],1)
lnCol = ALEN(laAllVar[lnI,1],2)
loFormSet.AddProperty(laAllVar[lnI,1]+'['+ALLTRIM(STR(lnRow))+;
','+ALLTRIM(STR(lnCol))+']')
lcACopy = '=ACOPY(' + laAllVar[lnI,1] + ',loFormSet.' + laAllVar[lnI,1] + ')'
&lcACopy.
ELSE
loFormSet.AddProperty(laAllVar[lnI,1],EVALUATE(laAllVar[lnI,1]))
ENDIF
ENDFOR
*--end of lfAddPro.

*!*************************************************************
*! Name      : lfGetDye
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/08/2004
*! Purpose   : function to Get the Dyelot/configuration.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetDye
LPARAMETERS loFormSet,lnType

LOCAL lcRet,lcCatgTyp
lcCatgTyp = loFormSet.laSetups[14+lnType,2]
IF lcCatgTyp $ 'FT'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = LANG_MFCSSH_DYELOT
lcRet = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYELOT,loFormSet.GetHeaderText("LANG_MFCSSH_DYELOT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
IF lcCatgTyp = 'S'
IF loFormSet.llStyConfg
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = LANG_MFCSSH_CONFIG
lcRet = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CONFIG,loFormSet.GetHeaderText("LANG_MFCSSH_CONFIG",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = LANG_MFCSSH_DYELOT
lcRet = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_DYELOT,loFormSet.GetHeaderText("LANG_MFCSSH_DYELOT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
ELSE
lcRet = ''
ENDIF
ENDIF
RETURN lcRet
*--end of lfGetDye.

*!*************************************************************
*! Name      : lfMfItmDetInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/13/2004
*! Purpose   : function called from the init event of form MFITMDET.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfItmDetInit
LPARAMETERS loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.Caption = LANG_MFCSSH_MFITMDET
loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFITMDET,loFormSet.GetHeaderText("LANG_MFCSSH_MFITMDET",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

WITH loFormSet.AriaForm1
IF !(m.cCatgTyp$'FTS')
.txtDesc.Left = 9
.lblDesc.Left = 15
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblDesc.Caption = LANG_MFCSSH_ITEM
.lblDesc.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ITEM,loFormSet.GetHeaderText("LANG_MFCSSH_ITEM",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
IF !(('SP'$oAriaApplication.CompanyInstalledModules) .AND. (m.cCatgTyp='F') .AND. (loParentForm.lcTranType='M'))
.lblMarker.Visible = .F.
.txtMarker.Visible = .F.
.lblUnits.Left = .lblUnits.Left + .txtMarker.Left - .txtUnits.Left
.txtUnits.Left = .txtMarker.Left
.lblPieces.Left = .lblPieces.Left - (.txtMarker.Width*2/3)
.txtPieces.Left = .txtPieces.Left - (.txtMarker.Width*2/3)
.lblUnitCost.Left = .lblUnitCost.Left - (.txtMarker.Width/3)
.txtUnitCost.Left = .txtUnitCost.Left - (.txtMarker.Width/3)
ENDIF
.txtUnits.Enabled = !(m.cCatgTyp$'MPD')
.cmdDetOpr.Visible = (m.cCatgTyp='M' .AND. loParentForm.llPwInst .AND. !loParentForm.llHideDet)
IF m.cCatgTyp$'FTS'
IF m.cCatgTyp = 'S'
.txtUnits.Format = '999'
.txtUnits.InputMask = '999'
.txtRequired.Format = '99999999'
.txtRequired.InputMask = '99999999'
ENDIF
.txtPieces.Enabled = .F.
.cntQty.Scale = SCALE.SCALE
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
.cntQty.txtQty&lcI..ControlSource = 'm.Req_Qty'+lcI
IF m.cCatgTyp $ 'FT'
.cntQty.txtQty&lcI..Format = '999999.999'
.cntQty.txtQty&lcI..InputMask = '999999.999'
ENDIF
ENDFOR
ELSE
.cntStyle.Visible = .F.
.shpQty.Visible = .F.
.cntQty.Visible = .F.
.cmdOk.Top = .cmdOk.Top - .shpQty.Height
.cmdDetOpr.Top = .cmdDetOpr.Top - .shpQty.Height
.cmdCancel.Top = .cmdCancel.Top - .shpQty.Height
.Height = .Height - .shpQty.Height
ENDIF
.cntStyle.CmdItemBrowse.Visible = .F.
ENDWITH
=lfDisCost(loFormSet)
*--end of lfMfItmDetInit.

*!*************************************************************
*! Name      : lfvUnits
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/14/2004
*! Purpose   : Validate the Units and Pieces Fields in the MFITMDET screen.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfvUnits
LPARAMETERS lnOldValue

IF m.Pieces * m.UntQty =< 0
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM38078B00000','ALERT',LANG_MFCSSH_REQUIRED+'|'+LANG_MFCSSH_ZERO)
=gfModalGen('TRM38078B00000','ALERT',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_REQUIRED,loFormSet.GetHeaderText("LANG_MFCSSH_REQUIRED",loFormSet.HeaderAlias))+'|'+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ZERO,loFormSet.GetHeaderText("LANG_MFCSSH_ZERO",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

IF m.Pieces =< 0
m.Pieces = lnOldValue
ELSE
m.UntQty = lnOldValue
ENDIF
RETURN
ENDIF

m.Req_Qty = m.Pieces * m.UntQty
IF m.cCatgTyp$'SFT'
m.Req_Qty1 = m.Req_Qty1 / lnOldValue * m.UntQty
m.Req_Qty2 = m.Req_Qty2 / lnOldValue * m.UntQty
m.Req_Qty3 = m.Req_Qty3 / lnOldValue * m.UntQty
m.Req_Qty4 = m.Req_Qty4 / lnOldValue * m.UntQty
m.Req_Qty5 = m.Req_Qty5 / lnOldValue * m.UntQty
m.Req_Qty6 = m.Req_Qty6 / lnOldValue * m.UntQty
m.Req_Qty7 = m.Req_Qty7 / lnOldValue * m.UntQty
m.Req_Qty8 = m.Req_Qty8 / lnOldValue * m.UntQty
ENDIF
*--end of lfvUnits.

*!*************************************************************
*! Name      : lfMfDstrbInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/13/2004
*! Purpose   : function called from the init event of form MFDSTRB.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfDstrbInit
LPARAMETERS loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.Caption = LANG_MFCSSH_MFDSTRB
loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFDSTRB,loFormSet.GetHeaderText("LANG_MFCSSH_MFDSTRB",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

WITH loFormSet.AriaForm1
IF loFormSet.llFrmDetal
.cmdOk.Left = .cmdOk.Left + .cmdOk.Width
ELSE
.cmdSelect.Visible = .F.
ENDIF
WITH .grdDstrb
.RecordSource = loParentForm.lcisslogfile
.Column1.ControlSource = loParentForm.lcisslogfile+".Style"
.Column1.Header1.Caption = loParentForm.lcItmHdr
.Column2.ControlSource = IIF(EVALUATE(loParentForm.lcisslogfile+'.cCatgTyp')='M',"gfCodDes("+;
loParentForm.lcisslogfile+".MfgCode,'MfgCode')",loParentForm.lcisslogfile+".Item")
.Column3.ControlSource = loParentForm.lcisslogfile+".StyQty"
.Column4.ControlSource = loParentForm.lcisslogfile+".UnitQty"
.Column5.ControlSource = loParentForm.lcisslogfile+".UnitCost"
.Column5.ReadOnly = (loFormSet.llFrmDetal)
.Column6.ControlSource = loParentForm.lcisslogfile+".ItemQty"
.Column7.ControlSource = loParentForm.lcisslogfile+".ItemAmt"
ENDWITH
ENDWITH
*--end of lfMfDstrbInit.

*!*************************************************************
*! Name      : lfGetTotStk
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/22/2004
*! Purpose   : function to calculate the TotStk field of lcTktSheet table.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetTotStk
LPARAMETERS loFormSet

LOCAL lnTotStk
lnTotStk = 0
DO CASE
CASE m.cCatgTyp $ 'FT'
m.Style = m.Item
IF lfOpenSql('ITEMLOC','FABDYE','STYDYE','CINVTYPE+STYLE+' + IIF(EMPTY(m.cWareCode),'','CWARECODE+') + 'DYELOT',loFormSet)
SELECT FABDYE
SUM TotStk ALL TO lnTotStk
ENDIF
CASE m.cCatgTyp = 'S'
IF EMPTY(m.cWareCode)
=SEEK(m.Item,'STYLE')
lnTotStk = STYLE.TotStk
ELSE
=SEEK(m.Item+m.cWareCode+m.Dyelot,'STYDYE')
lnTotStk = STYDYE.TotStk
ENDIF
ENDCASE

RETURN lnTotStk
*--end of lfGetTotStk.

*!*************************************************************
*! Name      : lfSetIndex
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/25/2004
*! Purpose   : function to Create index in table.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfSetIndex
LPARAMETERS lcTable,lcTagName,lcTagExp

LOCAL lnBuffering
lnBuffering = CURSORGETPROP("Buffering",lcTable)
=TABLEUPDATE(.T.,.T.,lcTable)
=CURSORSETPROP("Buffering",3,lcTable)
SELECT (lcTable)
*! B609181,1 MMT 03/21/2010 Fix Error in Cost Sheet screen when 2 users try to close POs [Start]
*INDEX ON &lcTagExp. TAG (lcTagName) OF (lcTable)
INDEX ON &lcTagExp. TAG (lcTagName)
*! B609181,1 MMT 03/21/2010 Fix Error in Cost Sheet screen when 2 users try to close POs [End]
=CURSORSETPROP("Buffering",lnBuffering,lcTable)
*--end of lfSetIndex.

*!*************************************************************
*! Name      : lfMfIsLogInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/29/2004
*! Purpose   : function called from the init event of form MFISLOG.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfIsLogInit
LPARAMETERS loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.Caption = LANG_MFCSSH_MFISLOG
loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFISLOG,loFormSet.GetHeaderText("LANG_MFCSSH_MFISLOG",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


DO CASE
CASE m.cCatgTyp='F' .OR. (m.cCatgTyp='T' .AND. m.Trim_Invt)
llDispRoll = loParentForm.laSetups[13,2]='Y' .AND. Fabric.lTrkRolls
llDispAct  = !INLIST(loParentForm.laSetups[11,2],'L','F','I')
llDispUsed = !INLIST(loParentForm.laSetups[11,2],'L','F','I')
m.cTrType   = '9'
m.cTrCode   = m.CutTkt
m.cDyelot   = m.Dyelot
*B607879,1 WAM 12/14/2006 Sort issued and return records
*IF !lfOpenSql('ITEMJRNL','MATINVJL','STYINVJL','CINVTYPE+STYLE+CTRTYPE+CTRCODE'+;
IIF(EMPTY(m.Dyelot),'','+CDYELOT'),loFormSet)
IF !lfOpenSql('ITEMJRNL','MATINVJL','STYINVJL','CINVTYPE+STYLE+CTRTYPE+CTRCODE'+;
IIF(EMPTY(m.Dyelot),'','+CDYELOT'),loFormSet,.F.,'cInvType,STYLE,cwarecode,cdyelot,crsession,cisession')
*B607879,1 WAM 12/14/2006 (End)

RETURN .F.
ENDIF

*! B608366,1 MMT 11/29/2007 Fix bug of wrong Cost open open Actual cost screen [Start]
=lfSetIndex('MATINVJL','MATINVJL','cinvtype+ style+ cwarecode+ csession+ cirtype+ ctrcode')
*! B608366,1 MMT 11/29/2007 Fix bug of wrong Cost open open Actual cost screen [End]

IF !INLIST(loParentForm.laSetups[11,2],"A","S")
lcCondition = "MATINVJL.cIRType='I'"
lcRetCond   = "MATINVJL.cIRType='I'"
*B607879,1 WAM 12/14/2006 Have a separate function to build the grid to be called from another place (After issue or return )
=lfRebIsLog(loFormSet)
*!*	      WITH loFormSet.AriaForm1.grdIsLog
*!*	        .RecordSource = 'MATINVJL'
*!*	        .Column1.ControlSource  = "PADR(IIF(MATINVJL.cIRType='I','"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
*!*	        .Column2.ControlSource  = "MATINVJL.cWareCode"
*!*	        .Column2.Visible        = (loParentForm.laSetups[2,2]='Y')
*!*	        .Column3.ControlSource  = "MATINVJL.dTrDate"
*!*	        .Column4.ControlSource  = "MATINVJL.CSESSION"
*!*	        .Column5.ControlSource  = "ABS(MATINVJL.nTotStk)"
*!*	        .Column6.ControlSource  = "MATINVJL.nCost"
*!*	        .Column7.ControlSource  = "MATINVJL.cApInvNo"
*!*	        .Column7.Visible        = (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)
*!*	        .Column8.ControlSource  = "gfCodDes(MATINVJL.cOprCode,'MFGCODE')"
*!*	        .Column9.ControlSource  = "MATINVJL.cLotNo"
*!*	        .Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
*!*	        .Column10.ControlSource = "ABS(MATINVJL.nStk1)"
*!*	        .Column11.ControlSource = "ABS(MATINVJL.nStk2)"
*!*	        .Column12.ControlSource = "ABS(MATINVJL.nStk3)"
*!*	        .Column13.ControlSource = "ABS(MATINVJL.nStk4)"
*!*	        .Column14.ControlSource = "ABS(MATINVJL.nStk5)"
*!*	        .Column15.ControlSource = "ABS(MATINVJL.nStk6)"
*!*	        .Column16.ControlSource = "ABS(MATINVJL.nStk7)"
*!*	        .Column17.ControlSource = "ABS(MATINVJL.nStk8)"
*!*	        .Column18.Visible       = .F.
*!*	        .Column19.Visible       = .F.
*!*	        .Column20.Visible       = .F.
*!*	      ENDWITH
*B607879,1 WAM 12/14/2006 (End)
ELSE
lcCondition = 'BOMCOST.nTotQty > 0'
lcRetCond   = 'BOMCOST.nTotQty > 0'
*B607879,1 WAM 12/14/2006 Have a separate function to build the grid to be called from another place (After issue or return )
=lfRebIsLog(loFormSet)
*!*	      WITH loFormSet.AriaForm1.grdIsLog
*!*	        .RecordSource = 'BOMCOST'
*!*	        .Column1.ControlSource  = "PADR(IIF(BOMCOST.nTotQty>0,'"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
*!*	        .Column2.ControlSource  = "BOMCOST.cWareCode"
*!*	        .Column2.Visible        = (loParentForm.laSetups[2,2]='Y')
*!*	        .Column3.ControlSource  = "BOMCOST.dTranDate"
*!*	        .Column4.ControlSource  = "IIF(BOMCOST.nTotQty>0,BOMCOST.cISession,BOMCOST.cRSession)"
*!*	        .Column5.ControlSource  = "ABS(BOMCOST.nTotQty)"
*!*	        .Column6.ControlSource  = "BOMCOST.nUnitCst"
*!*	        .Column7.ControlSource  = "BOMCOST.cApInvNo"
*!*	        .Column7.Visible        = (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)
*!*	        .Column8.ControlSource  = "gfCodDes(BOMCOST.cOprCode,'MFGCODE')"
*!*	        .Column9.ControlSource  = "BOMCOST.cLotNo"
*!*	        .Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
*!*	        .Column10.Visible       = .F.
*!*	        .Column11.Visible       = .F.
*!*	        .Column12.Visible       = .F.
*!*	        .Column13.Visible       = .F.
*!*	        .Column14.Visible       = .F.
*!*	        .Column15.Visible       = .F.
*!*	        .Column16.Visible       = .F.
*!*	        .Column17.Visible       = .F.
*!*	        .Column18.Visible       = .F.
*!*	        .Column19.Visible       = .F.
*!*	        .Column20.Visible       = .F.
*!*	      ENDWITH
*B607879,1 WAM 12/14/2006 (End)
ENDIF

CASE m.cCatgTyp = 'S' .AND. SEEK(m.Item,'Style')
lcCondition = "TMPISSLOG.cIRType='I'"
lcRetCond   = "TMPISSLOG.cIRType='I'"
llDispAct   = !INLIST(loParentForm.laSetups[12,2],'L','F')

LOCAL ARRAY laIssLog[1]
SELECT STYINVJL
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [BEGIN]
*SET ORDER TO TAG STYINVJL
*IF SEEK(m.Item)
gfSetOrder('STYINVJL')
IF GFSEEK(m.Item)
*E304013,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [END]
SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = m.Item;
FOR cTrCode+cTrType $ m.CutTkt+'1'+','+m.CutTkt+'I'
SCATTER MEMO TO laIssLog
INSERT INTO TMPISSLOG FROM ARRAY laIssLog
ENDSCAN
ENDIF
SELECT TMPISSLOG
LOCATE
*B607879,1 WAM 12/14/2006 Have a separate function to build the grid to be called from another place (After issue or return )
=lfRebIsLog(loFormSet)
*!*	    WITH loFormSet.AriaForm1.grdIsLog
*!*	      .RecordSource = 'TMPISSLOG'
*!*	      .Column1.ControlSource  = "PADR(IIF(TMPISSLOG.cIRType='I','"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
*!*	      .Column2.ControlSource  = "TMPISSLOG.cWareCode"
*!*	      .Column2.Visible        = (loParentForm.laSetups[2,2]='Y')
*!*	      .Column3.ControlSource  = "TMPISSLOG.dTrDate"
*!*	      .Column4.ControlSource  = "TMPISSLOG.CSESSION"
*!*	      .Column5.ControlSource  = "ABS(TMPISSLOG.nTotStk)"
*!*	      .Column6.ControlSource  = "TMPISSLOG.nCost"
*!*	      .Column7.Visible        = .F.
*!*	      .Column8.ControlSource  = "gfCodDes(TMPISSLOG.cOprCode,'MFGCODE')"
*!*	      .Column9.ControlSource  = "TMPISSLOG.cLotNo"
*!*	      .Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
*!*	      .Column10.ControlSource = "ABS(TMPISSLOG.nStk1)"
*!*	      .Column11.ControlSource = "ABS(TMPISSLOG.nStk2)"
*!*	      .Column12.ControlSource = "ABS(TMPISSLOG.nStk3)"
*!*	      .Column13.ControlSource = "ABS(TMPISSLOG.nStk4)"
*!*	      .Column14.ControlSource = "ABS(TMPISSLOG.nStk5)"
*!*	      .Column15.ControlSource = "ABS(TMPISSLOG.nStk6)"
*!*	      .Column16.ControlSource = "ABS(TMPISSLOG.nStk7)"
*!*	      .Column17.ControlSource = "ABS(TMPISSLOG.nStk8)"
*!*	      .Column18.Visible       = .F.
*!*	      .Column19.Visible       = .F.
*!*	      .Column20.Visible       = .F.
*!*	    ENDWITH
*B607879,1 WAM 12/14/2006 (End)
OTHERWISE
llDispAct   = .T.
lcCondition = 'BOMCOST.nTotQty>0'
lcRetCond   = 'm.Used_Qty>0'
*B607879,1 WAM 12/14/2006 Have a separate function to build the grid to be called from another place (After issue or return )
=lfRebIsLog(loFormSet)

*!*	    WITH loFormSet.AriaForm1.grdIsLog
*!*	      .RecordSource = 'BOMCOST'
*!*	      .Column1.ControlSource  = "PADR(IIF(BOMCOST.nTotQty>0,'"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
*!*	      .Column2.Visible        = .F.
*!*	      .Column3.ControlSource  = "BOMCOST.dTranDate"
*!*	      .Column4.ControlSource  = "IIF(BOMCOST.nTotQty>0,BOMCOST.cISession,BOMCOST.cRSession)"
*!*	      .Column5.ControlSource  = "ABS(BOMCOST.nTotQty)"
*!*	      .Column6.ControlSource  = "BOMCOST.nUnitCst"
*!*	      .Column7.Visible        = .F.
*!*	      .Column8.ControlSource  = "gfCodDes(BOMCOST.cOprCode,'MFGCODE')"
*!*	      .Column8.ColumnOrder    = 19
*!*	      .Column9.ControlSource  = "BOMCOST.cLotNo"
*!*	      .Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
*!*	      .Column9.ColumnOrder    = 20
*!*	      .Column10.Visible       = .F.
*!*	      .Column11.Visible       = .F.
*!*	      .Column12.Visible       = .F.
*!*	      .Column13.Visible       = .F.
*!*	      .Column14.Visible       = .F.
*!*	      .Column15.Visible       = .F.
*!*	      .Column16.Visible       = .F.
*!*	      .Column17.Visible       = .F.
*!*	      .Column18.ControlSource = "ABS(BOMCOST.nTotCst)"
*!*	      .Column18.ColumnOrder   = 8
*!*	      .Column19.ControlSource = "BOMCOST.nUnitAcSt"
*!*	      .Column19.ColumnOrder   = 9
*!*	      .Column20.ControlSource = "ABS(BOMCOST.nTotAcSt)"
*!*	    ENDWITH
*B607879,1 WAM 12/14/2006 (End)
ENDCASE
=lfShowLog(loFormSet)

WITH loFormSet.AriaForm1
IF !(m.cCatgTyp$'FTS')
.txtDesc.Top          = .txtDesc.Top          - 27
.shpHeader.Height     = .shpHeader.Height     - 27
.grdIsLog.Top         = .grdIsLog.Top         - 27
.cmdIssue.Top         = .cmdIssue.Top         - 27
.cmdReturns.Top       = .cmdReturns.Top       - 27
.cmdEditUsed.Top      = .cmdEditUsed.Top      - 27
.cmdMaterialRolls.Top = .cmdMaterialRolls.Top - 27
.cmdActualCost.Top    = .cmdActualCost.Top    - 27
.cmdClose.Top         = .cmdClose.Top         - 27
.Height               = .Height               - 27
.cntStyle.Visible     = .F.
ENDIF
.lblDyelot.Visible = llItemDye
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblDyelot.Caption = IIF( m.cCatgTyp='S' AND loParentForm.llStyConfg,LANG_MFCSSH_CONFIG,.lblDyelot.Caption)
.lblDyelot.Caption = IIF( m.cCatgTyp='S' AND loParentForm.llStyConfg,;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CONFIG,loFormSet.GetHeaderText("LANG_MFCSSH_CONFIG",loFormSet.HeaderAlias)),.lblDyelot.Caption)
*N000682,1 11/20/2012 MMT Globlization changes[End]

.txtDyelot.Visible = llItemDye
.lblOnHand.Visible = m.cCatgTyp $ 'FS' .OR. (m.cCatgTyp='T' .AND. m.Trim_Invt)
.txtOnHand.Visible = m.cCatgTyp $ 'FS' .OR. (m.cCatgTyp='T' .AND. m.Trim_Invt)

.cmdMaterialRolls.Visible = llDispRoll
.cmdEditUsed.Visible      = llDispUsed
.cmdActualCost.Visible    = llDispAct

*Centering the Returns, Material Rolls, Edit Used and Actual Cost command buttons depending on
*visibility of them in all possible cases.

*First we will calculate the left and width of distance between Issue and Close buttons
*to use for centring other buttons.
LOCAL lnStrLeft,lnStrWidth
lnStrLeft  = .cmdIssue.Left + .cmdIssue.Width
lnStrWidth = .cmdClose.Left - lnStrLeft
DO CASE
*Material Rolls, Edit Used and Actual cost are not visible
CASE !llDispRoll AND !llDispAct
*Center the Returns button in the whole distance.
.cmdReturns.Left = lnStrLeft + ((lnStrWidth - .cmdReturns.Width) / 2)

*Material Rolls only is visible (if Material Rolls is visible then Edit Used and Actual cost are not visible)
CASE llDispRoll
*Center the Returns button in the first half of distance.
.cmdReturns.Left = lnStrLeft + (((lnStrWidth / 2) - .cmdReturns.Width) / 2)

*Center the Material Rolls button in the second half of distance.
.cmdMaterialRolls.Left = lnStrLeft + (lnStrWidth / 2) + (((lnStrWidth / 2) - .cmdMaterialRolls.Width) / 2)

*Actual Cost only is visible (if Actual Cost is visible then Material Rolls is not visible)
CASE llDispAct AND !llDispUsed
*Center the Returns button in the first half of distance.
.cmdReturns.Left = lnStrLeft + (((lnStrWidth / 2) - .cmdReturns.Width) / 2)

*Center the Actual Cost button in the second half of distance.
.cmdActualCost.Left = lnStrLeft + (lnStrWidth / 2) + (((lnStrWidth / 2) - .cmdActualCost.Width) / 2)

*Actual Cost and Edit Used are visible (if Actual Cost is visible then Material Rolls is not visible)
CASE llDispAct AND llDispUsed
*Center the Returns button in the first part from three parts of distance.
.cmdReturns.Left = lnStrLeft + (((lnStrWidth / 3) - .cmdReturns.Width) / 2)

*Center the Edit Used button in the second part from three parts of distance.
.cmdEditUsed.Left = lnStrLeft + (lnStrWidth / 3) + (((lnStrWidth / 3) - .cmdEditUsed.Width) / 2)

*Center the Actual Cost button in the third part from three parts of distance.
.cmdActualCost.Left = lnStrLeft + (lnStrWidth * 2 / 3) + (((lnStrWidth / 3) - .cmdActualCost.Width) / 2)
ENDCASE
.cntStyle.CmdItemBrowse.Visible = .F.
ENDWITH
*--end of lfMfIsLogInit.

*!*************************************************************
*! Name      : lfMfOpLotInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/31/2004
*! Purpose   : function called from the init event of form MFOPLOT.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfOpLotInit
LPARAMETERS llAuto,lcOprCode,lcLotNo,loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.Caption = LANG_MFCSSH_MFOPLOT
loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFOPLOT,loFormSet.GetHeaderText("LANG_MFCSSH_MFOPLOT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.llAuto    = llAuto
loFormSet.lcOprCode = lcOprCode
loFormSet.lcLotNo   = lcLotNo

SELECT (loParentForm.lcOpenLots)
SET KEY TO lcIssWare+ALLTRIM(m.Dyelot)
=SEEK(lcIssWare)
WITH loFormSet.AriaForm1.grdOpLot
.RecordSource = loParentForm.lcOpenLots
.Column1.ControlSource  = "FABINV.cTrCode"
.Column2.ControlSource  = "lfGetPoVend(FABINV.cTrCode)"
.Column3.ControlSource  = loParentForm.lcOpenLots+".dTrDate"
.Column4.ControlSource  = loParentForm.lcOpenLots+".cRSession"
.Column5.ControlSource  = loParentForm.lcOpenLots+".nBalance"
.Column6.ControlSource  = loParentForm.lcOpenLots+".nCost"
.Column7.ControlSource  = loParentForm.lcOpenLots+".cApInvNo"
.Column7.Visible        = (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)
.Column8.ControlSource  = loParentForm.lcOpenLots+".cDyelot"
.Column8.Visible        = EMPTY(m.Dyelot)
.Column9.ControlSource  = loParentForm.lcOpenLots+".nToIssue"
.Column9.Visible        = llAuto
.Column10.ControlSource = loParentForm.lcOpenLots+".nBal1"
.Column11.ControlSource = loParentForm.lcOpenLots+".nBal2"
.Column12.ControlSource = loParentForm.lcOpenLots+".nBal3"
.Column13.ControlSource = loParentForm.lcOpenLots+".nBal4"
.Column14.ControlSource = loParentForm.lcOpenLots+".nBal5"
.Column15.ControlSource = loParentForm.lcOpenLots+".nBal6"
.Column16.ControlSource = loParentForm.lcOpenLots+".nBal7"
.Column17.ControlSource = loParentForm.lcOpenLots+".nBal8"
.Column18.ControlSource = loParentForm.lcOpenLots+".nCost*"+loParentForm.lcOpenLots+".nBalance"
.Column19.ControlSource = loParentForm.lcOpenLots+".nToIssue1"
.Column19.Visible       = llAuto
.Column20.ControlSource = loParentForm.lcOpenLots+".nToIssue2"
.Column20.Visible       = llAuto
.Column21.ControlSource = loParentForm.lcOpenLots+".nToIssue3"
.Column21.Visible       = llAuto
.Column22.ControlSource = loParentForm.lcOpenLots+".nToIssue4"
.Column22.Visible       = llAuto
.Column23.ControlSource = loParentForm.lcOpenLots+".nToIssue5"
.Column23.Visible       = llAuto
.Column24.ControlSource = loParentForm.lcOpenLots+".nToIssue6"
.Column24.Visible       = llAuto
.Column25.ControlSource = loParentForm.lcOpenLots+".nToIssue7"
.Column25.Visible       = llAuto
.Column26.ControlSource = loParentForm.lcOpenLots+".nToIssue8"
.Column26.Visible       = llAuto
ENDWITH

WITH loFormSet.AriaForm1
.lblDyelot.Visible   = llItemDye
.txtDyelot.Visible   = llItemDye
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblRequired.Caption = IIF(llAuto,LANG_MFCSSH_ISSUE,LANG_MFCSSH_REQUIRED)
.lblRequired.Caption = IIF(llAuto,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias)),;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_REQUIRED,loFormSet.GetHeaderText("LANG_MFCSSH_REQUIRED",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblUsed.Caption     = IIF(llAuto,LANG_MFCSSH_REMAINING,LANG_MFCSSH_USED)
.lblUsed.Caption     = IIF(llAuto,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_REMAINING,loFormSet.GetHeaderText("LANG_MFCSSH_REMAINING",loFormSet.HeaderAlias)),;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_USED,loFormSet.GetHeaderText("LANG_MFCSSH_USED",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

.cboMatWare.Visible  = (loParentForm.laSetups[2,2]='Y')

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdIssue.Caption    = IIF(llAuto,LANG_MFCSSH_OK,.cmdIssue.Caption)
.cmdIssue.Caption    = IIF(llAuto,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_OK,loFormSet.GetHeaderText("LANG_MFCSSH_OK",loFormSet.HeaderAlias)),.cmdIssue.Caption)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdClose.Caption    = IIF(llAuto,LANG_MFCSSH_CANCEL,.cmdClose.Caption)
.cmdClose.Caption    = IIF(llAuto,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CANCEL,loFormSet.GetHeaderText("LANG_MFCSSH_CANCEL",loFormSet.HeaderAlias)),.cmdClose.Caption)
*N000682,1 11/20/2012 MMT Globlization changes[End]

.cntStyle.CmdItemBrowse.Visible = .F.
ENDWITH
*--end of lfMfOpLotInit.

*!*************************************************************
*! Name      : lfGetPoVend
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/22/2004
*! Purpose   : function to get the vendor of material PO transaction.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetPoVend
LPARAMETERS lcPoNo

LOCAL lcVendor
lcVendor = ''
m.cBusDocu = 'P'
m.cStyType = 'M'
m.PO       = lcPoNo
IF lfOpenSql('POSHDR','POFHDR','POSHDR','CBUSDOCU+CSTYTYPE+PO',loParentForm,.T.)
SELECT POFHDR
LOCATE
lcVendor = Vendor
ENDIF

RETURN lcVendor
*--end of lfGetPoVend.

*!*************************************************************
*! Name      : lfGetUom
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/17/2004
*! Purpose   : function to get the UOM code.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetUom
LPARAMETERS lcRelCode

LOCAL lcRelCode, lcUOM_B, lcUOM_V, lnConf, lnRetVal
lcUOM_B = ''
lcUOM_V = ''
lnConf  = 1

=gfGetUOMData(lcRelCode, lcUOM_B, @lcUOM_V, lnConf, .F.)
RETURN lcUOM_V
*--end of lfGetUom.

*!*************************************************************
*! Name      : lfGetConv
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/17/2004
*! Purpose   : function to get the conversion factor of Buy.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetConv
LPARAMETERS lcRelCode

LOCAL lcRelCode, lcUOM_B, lcUOM_V, lnConf, lnRetVal
lcUOM_B = ''
lcUOM_V = ''
lnConv  = 1

=gfGetUOMData(lcRelCode, lcUOM_B, lcUOM_V, @lnConv, .F.)
RETURN lnConv
*--end of lfGetConv.

*!*************************************************************
*! Name      : lfMfIssRetInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/18/2004
*! Purpose   : function called from the init event of form MFISSRET.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfIssRetInit
LPARAMETERS llIssue,llActCst,llEditUsed,loFormSet

loFormSet.llIssue    = llIssue
loFormSet.llActCst   = llActCst
loFormSet.llEditUsed = llEditUsed

LOCAL lcCaption1,lcCaption2,lcCaption3,lcCaption4,lcCaption5,llDyeStat,llCstStat,llActCstSt
IF llEditUsed
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCaption1 = LANG_MFCSSH_EDIT  + ' '
lcCaption1 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_EDIT,loFormSet.GetHeaderText("LANG_MFCSSH_EDIT",loFormSet.HeaderAlias))  + ' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCaption2 = LANG_MFCSSH_USED  + ' '
lcCaption2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_USED,loFormSet.GetHeaderText("LANG_MFCSSH_USED",loFormSet.HeaderAlias))  + ' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcCaption3 = ''
lcCaption4 = ''
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCaption5 = LANG_MFCSSH_ISSUE + ' '
lcCaption5 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias)) + ' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
IF llIssue
lcCaption1 = ''
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCaption2 = LANG_MFCSSH_ISSUE  + ' '
lcCaption2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))  + ' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCaption3 = LANG_MFCSSH_ISSUE  + ' '
lcCaption3 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))  + ' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCaption4 = LANG_MFCSSH_FROM   + ' '
lcCaption4 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_FROM,loFormSet.GetHeaderText("LANG_MFCSSH_FROM",loFormSet.HeaderAlias))   + ' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCaption5 = LANG_MFCSSH_USED   + ' '
lcCaption5 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_USED,loFormSet.GetHeaderText("LANG_MFCSSH_USED",loFormSet.HeaderAlias))   + ' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
lcCaption1 = ''
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCaption2 = LANG_MFCSSH_RETURN + ' '
lcCaption2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURN,loFormSet.GetHeaderText("LANG_MFCSSH_RETURN",loFormSet.HeaderAlias)) + ' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCaption3 = LANG_MFCSSH_RETURN + ' '
lcCaption3 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURN,loFormSet.GetHeaderText("LANG_MFCSSH_RETURN",loFormSet.HeaderAlias)) + ' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCaption4 = LANG_MFCSSH_TO     + ' '
lcCaption4 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_TO,loFormSet.GetHeaderText("LANG_MFCSSH_TO",loFormSet.HeaderAlias))     + ' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCaption5 = LANG_MFCSSH_USED   + ' '
lcCaption5 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_USED,loFormSet.GetHeaderText("LANG_MFCSSH_USED",loFormSet.HeaderAlias))   + ' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
ENDIF

llDyeStat  = ((llIssue OR llEditUsed) AND !llActCst AND EMPTY(m.Dyelot))
llCstStat  = ((!llIssue AND !llEditUsed) OR llActCst)
llActCstSt = (!llActCst)

WITH loFormSet.ariaform1
LOCAL lnButtonsWidth
lnButtonsWidth             = .cmdCancel.Left + .cmdCancel.Width - .cmdOk.Left

.Caption                   = lcCaption1 + lcCaption2
.lblIssueDate.Caption      = lcCaption3 + .lblIssueDate.Caption
.lblIssueCost.Caption      = lcCaption3 + .lblIssueCost.Caption
*B607879,1 WAM 12/14/2006 Have a separate function to build the grid to be called from another place (After issue or return )
*.lblIssueCost.Visible      = (m.cCatgTyp='S') OR (!llIssue OR !INLIST(loParentForm.laSetups[11,2],'F','I'))
.lblIssueCost.Visible      = !(m.cCatgTyp$'FT') OR (!llIssue OR !INLIST(loParentForm.laSetups[11,2],'F','I'))
*B607879,1 WAM 12/14/2006 (End)
.lblIssueQuantity.Caption  = lcCaption2 + .lblIssueQuantity.Caption
.lblWarehouse.Caption      = lcCaption3 + lcCaption4 + .lblWarehouse.Caption
.lblWarehouse.Visible      = (loParentForm.laSetups[2,2] = 'Y' AND m.cCatgTyp$'SFT')
.lblUsedQuantity.Caption   = lcCaption5 + .lblUsedQuantity.Caption
.lblIssue_Qty.Caption      = lcCaption2

*B609089,1 WAM 11/16/2009 Fix update used quanity when use the option "Edit Used"
.lblUsed_Qty.Caption = lcCaption5
*B609089,1 WAM 11/16/2009 (End)

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblDyelot.Caption         = IIF(m.cCatgTyp='S' AND loParentForm.llStyConfg,LANG_MFCSSH_CONFIG,.lblDyelot.Caption)
.lblDyelot.Caption         = IIF(m.cCatgTyp='S' AND loParentForm.llStyConfg,;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CONFIG,loFormSet.GetHeaderText("LANG_MFCSSH_CONFIG",loFormSet.HeaderAlias)),;
.lblDyelot.Caption)
*N000682,1 11/20/2012 MMT Globlization changes[End]

.lblDyelot.Visible         = llItemDye
.kbDyelot.Visible          = llItemDye
.kbDyelot.Enabled          = llDyeStat
.DtpickerIssueDate.Enabled = llActCstSt
.txtIssueCost.Enabled      = llCstStat
.txtIssueCost.Visible      = !(m.cCatgTyp$'FT') OR (!llIssue OR !INLIST(loParentForm.laSetups[11,2],'F','I'))
.cboWarehouse.RowSource    = "loParentForm.la"+IIF(m.cCatgTyp='S',"Sty","Mat")+"Ware"
.cboWarehouse.Enabled      = IIF(m.cCatgTyp='S',llActCstSt,llWareStat)
.cboWarehouse.Visible      = (loParentForm.laSetups[2,2] = 'Y' AND m.cCatgTyp$'SFT')

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
.txtOnHand.Visible          = (m.cCatgTyp$'SFT')
.lblOnHand.Visible          = (m.cCatgTyp$'SFT')
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]


IF !(m.cCatgTyp$'SFT')
.cntStyle.Visible         = .F.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblDesc.Caption          = LANG_MFCSSH_ITEM
.lblDesc.Caption          = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ITEM,loFormSet.GetHeaderText("LANG_MFCSSH_ITEM",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

.lblDesc.Left             = .cntStyle.Left
.txtDesc.Left             = .cntStyle.Left
.txtIssueCost.Enabled     = .T.
.txtIssueQuantity.Left    = .cboWarehouse.Left
.txtIssueQuantity.Top     = .cboWarehouse.Top
.txtIssueQuantity.Enabled = llActCstSt
.lblIssueQuantity.Left    = .txtIssueQuantity.Left
.lblIssueQuantity.Top     = .lblWarehouse.Top
.shpHeader.Width          = .txtUsedQuantity.Left + .txtUsedQuantity.Width + 3
.shpFields.Width          = .shpHeader.Width
STORE .F. TO .shpQty.Visible,.cntReq_Qty.Visible,.cntUsed_Qty.Visible,.cntIssue_Qty.Visible,;
.lblReq_Qty.Visible,.lbl1.Visible,.lblUsed_Qty.Visible,.lbl2.Visible,;
.lblIssue_Qty.Visible,.lbl3.Visible
.shpOperation.Top         = .shpOperation.Top  - .shpQty.Height      - 3
.lblOperation.Top         = .lblOperation.Top  - .shpQty.Height      - 3
.txtOperation.Top         = .txtOperation.Top  - .shpQty.Height      - 3
.lblLot.Top               = .lblLot.Top        - .shpQty.Height      - 3
.cboLot.Top               = .cboLot.Top        - .shpQty.Height      - 3
.cmdOk.Top                = .cmdOk.Top         - .shpQty.Height      - 3
.cmdCancel.Top            = .cmdCancel.Top     - .shpQty.Height      - 3
.Height                   = .Height            - .shpQty.Height      - 3
.Top                      = .Top               + ((.shpQty.Height    + 3) / 2)
.shpOperation.Width       = .cboLot.Left       + .cboLot.Width       + 3
.cmdOk.Left               = .shpOperation.Left + .shpOperation.Width + 3
.cmdCancel.Left           = .shpOperation.Left + .shpOperation.Width + 3
LOCAL lnOldWidth
lnOldWidth                = .Width
.Width                    = .shpHeader.Width   + 0                   + 6
.Left                     = .Left              + ((lnOldWidth - .Width) / 2)
ELSE

*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[Start]
.txtOnHand.ControlSource  = 'm.nOnHand'
IF m.cCatgTyp $ 'S'
.txtOnHand.Format      = '999999'
.txtOnHand.InputMask   = '999999'
ENDIF
*B608454,1 MMT 02/27/2008 Fix bug of not displaying on Hand While issue fabric or Style[End]

.cntReq_Qty.Scale   = SCALE.SCALE
.cntUsed_Qty.Scale  = SCALE.SCALE
.cntIssue_Qty.Scale = SCALE.SCALE
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
.cntReq_Qty.txtQty&lcI..ControlSource   = 'm.Req_Qty'+lcI
.cntUsed_Qty.txtQty&lcI..ControlSource  = IIF(llEditUsed,'m.Iss_Qty'+lcI,'m.Used_Qty'+lcI)
.cntIssue_Qty.txtQty&lcI..ControlSource = IIF(llEditUsed,'m.Used_Qty'+lcI,'m.Iss_Qty'+lcI)

*B609089,1 WAM 11/16/2009 Fix update used quanity when use the option "Edit Used"
.cntIssue_Qty.txtTotQty.ControlSource = IIF(llEditUsed,'m.Used_Qty','m.Iss_Qty')
*B609089,1 WAM 11/16/2009 (End)

.cntIssue_Qty.txtQty&lcI..Enabled       = (llActCstSt AND EVALUATE('m.Req_Qty'+lcI) > 0)
IF m.cCatgTyp $ 'FT'
.cntReq_Qty.txtQty&lcI..Format      = '999999.999'
.cntReq_Qty.txtQty&lcI..InputMask   = '999999.999'
.cntUsed_Qty.txtQty&lcI..Format     = '999999.999'
.cntUsed_Qty.txtQty&lcI..InputMask  = '999999.999'
.cntIssue_Qty.txtQty&lcI..Format    = '999999.999'
.cntIssue_Qty.txtQty&lcI..InputMask = '999999.999'
ENDIF
ENDFOR
ENDIF

IF !llIssue OR EMPTY(m.cOprCode)
.shpOperation.Visible   = .F.
.lblOperation.Visible   = .F.
.txtOperation.Visible   = .F.
.lblLot.Visible         = .F.
.cboLot.Visible         = .F.
.cmdOk.Left             = (.Width - lnButtonsWidth) / 2
.cmdCancel.Left         = .cmdOk.Left + .cmdOk.Width + 3
ELSE
IF !(m.cCatgTyp$'SFT')
.cmdOk.Top            = .shpOperation.Top
.cmdCancel.Top        = .cmdOk.Top + .cmdOk.Height
ENDIF
ENDIF

.cboLot.Enabled                 = llActcstSt
.txtUsedQuantity.ControlSource  = IIF(llEditUsed,"m.Issue_Qty","m.Used_Qty")
.txtIssueQuantity.ControlSource = IIF(llEditUsed,"m.Used_Qty","m.Issue_Qty")
.cntStyle.CmdItemBrowse.Visible = .F.
ENDWITH
*--end of lfMfIssRetInit.

*!*************************************************************
*! Name      : lfMfIssLtInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/26/2004
*! Purpose   : function called from the init event of form MFISSLT.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfIssLtInit
PARAMETERS lcOprCode,lcLotNo,loFormSet

loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFISSLT,loFormSet.GetHeaderText("LANG_MFCSSH_MFISSLT",loFormSet.HeaderAlias))+;
IIF(!EMPTY(lcOprCode) .AND. !EMPTY(lcLotNo),;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_FOR,loFormSet.GetHeaderText("LANG_MFCSSH_FOR",loFormSet.HeaderAlias))+' '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_LOT,loFormSet.GetHeaderText("LANG_MFCSSH_LOT",loFormSet.HeaderAlias))+': '+lcLotNo+' '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_OPERAT,loFormSet.GetHeaderText("LANG_MFCSSH_OPERAT",loFormSet.HeaderAlias))+': '+;
ALLTRIM(gfCodDes(lcOprCode,'MfgCode')),'')


loFormSet.lcOprCode = lcOprCode
loFormSet.lcLotNo   = lcLotNo

*! C201141,1 HES 04/26/2009, please quote Processing Enbroidery orders for DCC [Start]

*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*	IF TYPE('oPostyRef') = 'O' AND ASCAN(oPostyRef.laEvntTrig,PADR('CRPONDCOSH',10),1,ALEN(oPostyRef.laEvntTrig,1),1) > 0
IF (TYPE('oPostyRef.Name') = 'C' AND ASCAN(oPostyRef.laEvntTrig,PADR('CRPONDCOSH',10),1,ALEN(oPostyRef.laEvntTrig,1),1) > 0) OR;
(TYPE('oPoRecRef.Name') = 'C' AND ASCAN(oPoRecRef.laEvntTrig,PADR('MFRUPDTEMP',10),1,ALEN(oPoRecRef.laEvntTrig,1),1) > 0)
*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

*B609477,1 TMI 12/12/2010 [Start] add the oariaapplication.defaultpath to the called dirmain.fxp
*DO lfMFUPDTEMP IN DIRMAIN.FXP WITH loformset
DO lfMFUPDTEMP IN oariaapplication.defaultpath+'DIRMAIN.FXP' WITH loformset
*B609477,1 TMI 12/12/2010 [End  ]
ENDIF
*! C201141,1 HES 04/26/2009, please quote Processing Enbroidery orders for DCC [End]

WITH loFormSet.AriaForm1.grdIsLog
.RecordSource = loParentForm.lcIssLtFile
.Column1.ControlSource  = loParentForm.lcIssLtFile+".lSelect"
.Column2.ControlSource  = "IIF("+loParentForm.lcIssLtFile+".cCatgTyp='M',"+;
"gfCodDes("+loParentForm.lcIssLtFile+".MfgCode,'MfgCode'),"+;
loParentForm.lcIssLtFile+".Item)"
.Column3.ControlSource  = loParentForm.lcIssLtFile+".Dyelot"
.Column3.Visible        = loParentForm.llUseDyelot
.Column4.ControlSource  = "lfGetUom(" + loParentForm.lcIssLtFile+".cUomCode)"
.Column5.ControlSource  = loParentForm.lcIssLtFile+".Req_Qty"
.Column6.ControlSource  = loParentForm.lcIssLtFile+".Issue_Qty"
.Column7.ControlSource  = loParentForm.lcIssLtFile+".Used_Qty"
.Column8.ControlSource  = loParentForm.lcIssLtFile+".nIssue"
.Column9.ControlSource  = loParentForm.lcIssLtFile+".nIssPcnt"
.Column10.ControlSource = loParentForm.lcIssLtFile+".UntQty"
.Column11.ControlSource = loParentForm.lcIssLtFile+".Pieces"
ENDWITH

*! C201141,1 HES 04/26/2009, please quote Processing Enbroidery orders for DCC [Start]

*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*	IF TYPE('oPostyRef') = 'O' AND ASCAN(oPostyRef.laEvntTrig,PADR('CRPONDCOSH',10),1,ALEN(oPostyRef.laEvntTrig,1),1) > 0
IF (TYPE('oPostyRef.Name') = 'C' AND ASCAN(oPostyRef.laEvntTrig,PADR('CRPONDCOSH',10),1,ALEN(oPostyRef.laEvntTrig,1),1) > 0) OR;
(TYPE('oPoRecRef.Name') = 'C' AND ASCAN(oPoRecRef.laEvntTrig,PADR('MFRCSAUTIS',10),1,ALEN(oPoRecRef.laEvntTrig,1),1) > 0)
*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

llCustReturn = .T.
*B609477,1 TMI 12/12/2010 [Start] add the oariaapplication.defaultpath to the called dirmain.fxp
*DO lfMFCSAUTISS IN DIRMAIN.FXP WITH loformset
DO lfMFCSAUTISS IN oariaapplication.defaultpath+'DIRMAIN.FXP' WITH loformset
*B609477,1 TMI 12/12/2010 [End  ]
RETURN llCustReturn
ENDIF
*! C201141,1 HES 04/26/2009, please quote Processing Enbroidery orders for DCC [End]

*--end of lfMfIssLtInit.

*!*************************************************************
*! Name      : lfMfGenShtInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/26/2004
*! Purpose   : function called from the init event of form MFGENSHT.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfGenShtInit
PARAMETERS loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.Caption = LANG_MFCSSH_MFGENSHT
loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFGENSHT,loFormSet.GetHeaderText("LANG_MFCSSH_MFGENSHT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


WITH loFormSet.AriaForm1
.cntLinkCode.kbGlLink.KeyTextBox.ControlSource = loParentForm.lcPosHdr+".LINK_CODE"
.cntLinkCode.txtGlName.ControlSource = "lfLinkDesc(loParentForm)"
.cntLinkCode.Visible = (loParentForm.laSetups[1,2] = 'Y')
.lblStyWare.Visible  = (loParentForm.laSetups[2,2]='Y' AND loParentForm.llStyCost)
.lbl1.Visible        = (loParentForm.laSetups[2,2]='Y' AND loParentForm.llStyCost)
.cboStyWare.Visible  = (loParentForm.laSetups[2,2]='Y' AND loParentForm.llStyCost)
.lblMatWare.Visible  = (loParentForm.laSetups[2,2]='Y' AND loParentForm.llMatCost)
.lbl2.Visible        = (loParentForm.laSetups[2,2]='Y' AND loParentForm.llMatCost)
.cboMatWare.Visible  = (loParentForm.laSetups[2,2]='Y' AND loParentForm.llMatCost)

LOCAL lnTopDef
*--Reposition all objects in case of Link Code not visible.
IF !.cntLinkCode.Visible
lnTopDef        = .cboStyWare.Top - .cntLinkCode.Top
.lblStyWare.Top = .lblStyWare.Top - lnTopDef
.lbl1.Top       = .lbl1.Top       - lnTopDef
.cboStyWare.Top = .cboStyWare.Top - lnTopDef
.lblMatWare.Top = .lblMatWare.Top - lnTopDef
.lbl2.Top       = .lbl2.Top       - lnTopDef
.cboMatWare.Top = .cboMatWare.Top - lnTopDef
.cmdOk.Top      = .cmdOk.Top      - lnTopDef
.cmdCancel.Top  = .cmdCancel.Top  - lnTopDef
.Height         = .Height         - lnTopDef
ENDIF

*--Reposition all objects in case of Style Warehouse not visible.
IF !.cboStyWare.Visible
lnTopDef        = .cboMatWare.Top - .cboStyWare.Top
.lblMatWare.Top = .lblMatWare.Top - lnTopDef
.lbl2.Top       = .lbl2.Top       - lnTopDef
.cboMatWare.Top = .cboMatWare.Top - lnTopDef
.cmdOk.Top      = .cmdOk.Top      - lnTopDef
.cmdCancel.Top  = .cmdCancel.Top  - lnTopDef
.Height         = .Height         - lnTopDef
ENDIF

*--Reposition all objects in case of Material WareHouse not visible.
IF !.cboMatWare.Visible
lnTopDef        = .cmdOk.Top      - .cboMatWare.Top
.cmdOk.Top      = .cmdOk.Top      - lnTopDef
.cmdCancel.Top  = .cmdCancel.Top  - lnTopDef
.Height         = .Height         - lnTopDef
ENDIF
ENDWITH
*--end of lfMfGenShtInit.

*!*************************************************************
*! Name      : lfMfModOprInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/05/2004
*! Purpose   : function called from the init event of form MFMODOPR.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfModOprInit
LPARAMETERS loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.Caption = LANG_MFCSSH_MFMODOPR
loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFMODOPR,loFormSet.GetHeaderText("LANG_MFCSSH_MFMODOPR",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

llDisStat = !llInHouse
llNameStat= llInHouse
llView    = EVALUATE(loParentForm.lcOprHdr+'.cOprCode')=loParentForm.lcFirstOpr AND !INLIST(EVALUATE(loParentForm.lcPosHdr+'.Status'),'A','H')
WITH loFormSet.AriaForm1
.txtOperation.ControlSource = loParentForm.lcOprHdr+".cOprCode"
.txtDesc.ControlSource      = "gfCodDes("+loParentForm.lcOprHdr+".cOprCode,'MFGCODE')"
.txtSequence.Enabled        = llModSeq
.txtContName.Enabled        = llNameStat
.KbCont.KeyCmd.Enabled      = llDisStat
IF !llView
.shpActualize.Visible       = .F.
.chkActualize.Visible       = .F.
.cmdOk.Left                 = .shpActualize.Left
ENDIF
ENDWITH

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
=lfDisActual(loFormSet)
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]

*--end of lfMfModOprInit.

*!*************************************************************
*! Name      : lfMfRcvLtInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/06/2004
*! Purpose   : function called from the init event of form MFREVLT.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfRcvLtInit
LPARAMETERS loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.Caption = LANG_MFCSSH_MFRCVLT
loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFRCVLT,loFormSet.GetHeaderText("LANG_MFCSSH_MFRCVLT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

LOCAL llMultiLot
llMultiLot = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT') = 'M')
WITH loFormSet.AriaForm1
.lblLot.Visible      = llMultiLot
.cboLot.Visible      = llMultiLot
.opgLots.Visible     = llMultiLot
.txtToLotNo.Visible  = llMultiLot
.txtToNewLot.Visible = llMultiLot
.cntStyle.ControlSource     = loParentForm.lcRcvFile+".Item"
.DtpickerDate.ControlSource = loParentForm.lcRcvFile+".dTranDate"
WITH .grdOprDet
.RecordSource = loParentForm.lcRcvFile
.Column1.ControlSource  = loParentForm.lcRcvFile+".lSelect"
.Column2.ControlSource  = loParentForm.lcRcvFile+".Item"
.Column2.Header1.Caption= loParentForm.lcItmHdr
.Column3.ControlSource  = loParentForm.lcRcvFile+".cDyelot"
.Column3.Visible        = loParentForm.llUseDyelot
.Column4.ControlSource  = loParentForm.lcRcvFile+".nTotRec"
.Column5.ControlSource  = loParentForm.lcRcvFile+".nReceive1"
.Column6.ControlSource  = loParentForm.lcRcvFile+".nReceive2"
.Column7.ControlSource  = loParentForm.lcRcvFile+".nReceive3"
.Column8.ControlSource  = loParentForm.lcRcvFile+".nReceive4"
.Column9.ControlSource  = loParentForm.lcRcvFile+".nReceive5"
.Column10.ControlSource = loParentForm.lcRcvFile+".nReceive6"
.Column11.ControlSource = loParentForm.lcRcvFile+".nReceive7"
.Column12.ControlSource = loParentForm.lcRcvFile+".nReceive8"
ENDWITH
.cntOpen.Scale     = SCALE.SCALE
.cntQuantity.Scale = SCALE.SCALE
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
.cntOpen.txtQty&lcI..Value = MAX(EVALUATE(loParentForm.lcRcvFile+'.nLotQty'+lcI),0)
.cntOpen.txtTotQty.Value   = .cntOpen.txtTotQty.Value+MAX(EVALUATE(loParentForm.lcRcvFile+'.nLotQty'+lcI),0)
.cntQuantity.txtQty&lcI..ControlSource = loParentForm.lcRcvFile+".nReceive"+lcI
ENDFOR
.cntQuantity.txtTotQty.ControlSource = loParentForm.lcRcvFile+".nTotRec"
.cntStyle.CmdItemBrowse.Visible = .F.
ENDWITH
=lfShRcv(loFormSet)
*--end of lfMfRcvLtInit.

*!*************************************************************
*! Name      : lfMfModLtInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/09/2004
*! Purpose   : function called from the init event of form MFMODLT.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfModLtInit
LPARAMETERS lcOprCode,lcLotNo,loFormSet

loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_OPERAT,loFormSet.GetHeaderText("LANG_MFCSSH_OPERAT",loFormSet.HeaderAlias))+': '+ALLTRIM(gfCodDes(lcOprCode,'MfgCode'))+' '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_LOT,loFormSet.GetHeaderText("LANG_MFCSSH_LOT",loFormSet.HeaderAlias))  +' : '+lcLotNo


llDisAtat = !llInHouse .AND. (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)

WITH loFormSet.AriaForm1
.DtpickerTransactionDate.ControlSource = "ldTranDate"
.DtpickerDueDate.ControlSource = "ldDueDate"
.KbCont.KeyCmd.Enabled = llDisAtat
.txtContName.Enabled = llNameStat
.cntStyle.ControlSource = loParentForm.lcLotsDet+".Item"
.DtpickerDate.ControlSource = loParentForm.lcLotsDet+".DueDate"
WITH .grdOprDet
.RecordSource = loParentForm.lcLotsDet
.Column2.ControlSource  = loParentForm.lcLotsDet+".Item"
.Column2.Header1.Caption= loParentForm.lcItmHdr
.Column3.ControlSource  = loParentForm.lcLotsDet+".cDyelot"
.Column3.Visible        = loParentForm.llUseDyelot
.Column4.ControlSource  = loParentForm.lcLotsDet+".nTotToIss"
.Column5.ControlSource  = loParentForm.lcLotsDet+".nToIssue1"
.Column6.ControlSource  = loParentForm.lcLotsDet+".nToIssue2"
.Column7.ControlSource  = loParentForm.lcLotsDet+".nToIssue3"
.Column8.ControlSource  = loParentForm.lcLotsDet+".nToIssue4"
.Column9.ControlSource  = loParentForm.lcLotsDet+".nToIssue5"
.Column10.ControlSource = loParentForm.lcLotsDet+".nToIssue6"
.Column11.ControlSource = loParentForm.lcLotsDet+".nToIssue7"
.Column12.ControlSource = loParentForm.lcLotsDet+".nToIssue8"
ENDWITH
.cntBudget.Scale   = SCALE.SCALE
.cntReceived.Scale = SCALE.SCALE
.cntCanceled.Scale = SCALE.SCALE
.cntDamaged.Scale  = SCALE.SCALE
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
.cntBudget.txtQty&lcI..ControlSource   = loParentForm.lcLotsDet+".nToIssue"+lcI
.cntOpen.txtQty&lcI..ControlSource     = "=lfOpnLot("+lcI+")"
.cntReceived.txtQty&lcI..ControlSource = "lnLotRcv"+lcI
.cntCanceled.txtQty&lcI..ControlSource = "lnLotCan"+lcI
.cntDamaged.txtQty&lcI..ControlSource  = "lnLotDmg"+lcI
ENDFOR
.cntStyle.CmdItemBrowse.Visible = .F.
ENDWITH
=lfShModLot(loFormSet)
*--end of lfMfModLtInit.

*!*************************************************************
*! Name      : lfOpnLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/09/2004
*! Purpose   : function to get the open quantity of the lot.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpnLot
LPARAMETERS lnQtyNo

LOCAL lcQtyNo
lcQtyNo = STR(lnQtyNo,1)
RETURN MAX(EVALUATE(loParentForm.lcLotsDet+".nToIssue"+lcQtyNo)-EVALUATE("lnLotRcv"+lcQtyNo)-;
EVALUATE("lnLotCan"+lcQtyNo)-EVALUATE("lnLotDmg"+lcQtyNo),0)
*--end of lfOpnLot.

*!*************************************************************
*! Name      : lfMfLogLtInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/11/2004
*! Purpose   : function called from the init event of form MFLOGLT.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfLogLtInit
LPARAMETERS loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.Caption = LANG_MFCSSH_MFLOGLT
loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFLOGLT,loFormSet.GetHeaderText("LANG_MFCSSH_MFLOGLT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


LOCAL llMultiLot
llMultiLot = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT') = 'M')
WITH loFormSet.AriaForm1
.lblLot.Visible = llMultiLot
.cboLot.Visible = llMultiLot
WITH .grdIsLog
.RecordSource = loParentForm.lcisslogfile
.Column2.ControlSource  = "IIF("+loParentForm.lcisslogfile+".cCostType='M',gfCodDes("+loParentForm.lcisslogfile+;
".MfgCode,'MfgCode'),"+loParentForm.lcisslogfile+".Item)"
.Column3.ControlSource  = loParentForm.lcisslogfile+".cDyelot"
.Column4.ControlSource  = loParentForm.lcisslogfile+".cWareCode"
.Column5.ControlSource  = loParentForm.lcisslogfile+".nTotQty"
.Column6.ControlSource  = loParentForm.lcisslogfile+".nUnitCst"
.Column7.ControlSource  = loParentForm.lcisslogfile+".nTotCst"
.Column8.ControlSource  = loParentForm.lcisslogfile+".nUnitAcSt"
.Column9.ControlSource  = loParentForm.lcisslogfile+".nTotAcSt"
.Column10.ControlSource = loParentForm.lcisslogfile+".dTranDate"
.Column11.ControlSource = loParentForm.lcisslogfile+".cApInvNo"
ENDWITH
ENDWITH
*--end of lfMfLogLtInit.

*!*************************************************************
*! Name      : lfMfDetLvlInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/11/2004
*! Purpose   : function called from the init event of form MFDETLVL.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfDetLvlInit
LPARAMETERS loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.Caption = LANG_MFCSSH_MFDETLVL
loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFDETLVL,loFormSet.GetHeaderText("LANG_MFCSSH_MFDETLVL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


LOCAL llRbStat,lnI,lcI
llRbStat = (loParentForm.rbSummary=2)
WITH loFormSet.AriaForm1
.lstSort.RowSource = loParentForm.lcpusort
FOR lnI = 1 TO 4
lcI = STR(lnI,1)
.chkSort&lcI..Enabled = llRbStat
.chkSort&lcI..Caption = loParentForm.laSort[lnI,2]
ENDFOR
ENDWITH
*--end of lfMfDetLvlInit.

*!*************************************************************
*! Name      : lfMfLotsInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/12/2004
*! Purpose   : function called from the init event of form MFLOTS.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfLotsInit
LPARAMETERS lcOprCode,lcLotNo,loFormSet

loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_OPERAT,loFormSet.GetHeaderText("LANG_MFCSSH_OPERAT",loFormSet.HeaderAlias))+': '+ALLTRIM(gfCodDes(lcOprCode,'MfgCode'))+' '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_LOT,loFormSet.GetHeaderText("LANG_MFCSSH_LOT",loFormSet.HeaderAlias))  +' : '+lcLotNo


llDisAtat = !llInHouse .AND. (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)

WITH loFormSet.AriaForm1
.DtpickerTransactionDate.ControlSource = "ldTranDate"
.DtpickerDueDate.ControlSource = "ldDueDate"
.KbCont.KeyCmd.Enabled = llDisAtat
.txtContName.Enabled = llNameStat
.cntStyle.ControlSource = loParentForm.lcLotsDet+".Item"
.DtpickerDate.ControlSource = loParentForm.lcLotsDet+".DueDate"
WITH .grdOprDet
.RecordSource = loParentForm.lcLotsDet
.Column2.ControlSource  = loParentForm.lcLotsDet+".Item"
.Column2.Header1.Caption= loParentForm.lcItmHdr
.Column3.ControlSource  = loParentForm.lcLotsDet+".cDyelot"
.Column3.Visible        = loParentForm.llUseDyelot
.Column4.ControlSource  = loParentForm.lcLotsDet+".nTotToIss"
.Column5.ControlSource  = loParentForm.lcLotsDet+".nToIssue1"
.Column6.ControlSource  = loParentForm.lcLotsDet+".nToIssue2"
.Column7.ControlSource  = loParentForm.lcLotsDet+".nToIssue3"
.Column8.ControlSource  = loParentForm.lcLotsDet+".nToIssue4"
.Column9.ControlSource  = loParentForm.lcLotsDet+".nToIssue5"
.Column10.ControlSource = loParentForm.lcLotsDet+".nToIssue6"
.Column11.ControlSource = loParentForm.lcLotsDet+".nToIssue7"
.Column12.ControlSource = loParentForm.lcLotsDet+".nToIssue8"
ENDWITH
.cntToIssue.Scale = SCALE.SCALE
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
.cntToIssue.txtQty&lcI..ControlSource   = loParentForm.lcLotsDet+".nToIssue"+lcI
.cntTotalLots.txtQty&lcI..ControlSource = loParentForm.lcLotsDet+".nIssued" +lcI
.cntBudget.txtQty&lcI..ControlSource    = loParentForm.lcLotsDet+".nBudget" +lcI
.cntOpen.txtQty&lcI..ControlSource      = "=lfOpenLot("+lcI+")"
.cntRemaining.txtQty&lcI..ControlSource = "=lfRemLot("+lcI+")"
ENDFOR
.cntStyle.CmdItemBrowse.Visible = .F.
ENDWITH
=lfShNewLot(loFormSet)
*--end of lfMfLotsInit.

*!*************************************************************
*! Name      : lfOpenLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/12/2004
*! Purpose   : function to get the open quantity of the lot.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenLot
LPARAMETERS lnQtyNo

LOCAL lcQtyNo
lcQtyNo = STR(lnQtyNo,1)
RETURN MAX(EVALUATE(loParentForm.lcLotsDet+".nBudget"+lcQtyNo)-EVALUATE(loParentForm.lcLotsDet+".nIssued"+lcQtyNo),0)
*--end of lfOpenLot.

*!*************************************************************
*! Name      : lfRemLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/12/2004
*! Purpose   : function to get the Remaining quantity of the lot.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfRemLot
LPARAMETERS lnQtyNo

LOCAL lcQtyNo
lcQtyNo = STR(lnQtyNo,1)
RETURN MAX(EVALUATE(loParentForm.lcLotsDet+".nBudget"+lcQtyNo)-EVALUATE(loParentForm.lcLotsDet+".nIssued"+lcQtyNo)-;
EVALUATE(loParentForm.lcLotsDet+".nToIssue"+lcQtyNo),0)
*--end of lfRemLot.

*!*************************************************************
*! Name      : lfMfConCnInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/13/2004
*! Purpose   : function called from the init event of form MFCONCN.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfConCnInit
LPARAMETERS loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.Caption = LANG_MFCSSH_MFCONCN
loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFCONCN,loFormSet.GetHeaderText("LANG_MFCSSH_MFCONCN",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


WITH loFormSet.AriaForm1
WITH .grdConCn
.RecordSource = loParentForm.lcisslogfile
.Column2.ControlSource  = loParentForm.lcisslogfile+".cDyelot"
.Column2.Visible        = loParentForm.llUseDyelot
.Column3.ControlSource  = loParentForm.lcisslogfile+".cDyelot"
.Column3.Visible        = (loParentForm.laSetups[2,2]='Y')
.Column4.ControlSource  = loParentForm.lcisslogfile+".nCan1"
.Column5.ControlSource  = loParentForm.lcisslogfile+".nCan2"
.Column6.ControlSource  = loParentForm.lcisslogfile+".nCan3"
.Column7.ControlSource  = loParentForm.lcisslogfile+".nCan4"
.Column8.ControlSource  = loParentForm.lcisslogfile+".nCan5"
.Column9.ControlSource  = loParentForm.lcisslogfile+".nCan6"
.Column10.ControlSource = loParentForm.lcisslogfile+".nCan7"
.Column11.ControlSource = loParentForm.lcisslogfile+".nCan8"
.Column12.ControlSource = loParentForm.lcisslogfile+".nTotCan"
ENDWITH
.cntCancel.Scale = SCALE.SCALE
.cntBudget.Scale = SCALE.SCALE
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
.cntCancel.txtQty&lcI..ControlSource  = "lnCan" +lcI
.cntBalance.txtQty&lcI..ControlSource = "lnBal" +lcI
.cntBudget.txtQty&lcI..ControlSource  = "m.nQty"+lcI
ENDFOR
.cntCancel.txtTotQty.ControlSource  = "lnTotCan"
.cntBalance.txtTotQty.ControlSource = "lnTotBal"
.cntBudget.txtTotQty.ControlSource  = "m.nTotQty"
ENDWITH
*--end of lfMfConCnInit.

*!*************************************************************
*! Name      : lfMfEdAloInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/13/2004
*! Purpose   : function called from the init event of form MFEDALO.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfEdAloInit
LPARAMETERS loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.Caption = LANG_MFCSSH_MFEDALO
loFormSet.ariaForm1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MFEDALO,loFormSet.GetHeaderText("LANG_MFCSSH_MFEDALO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


WITH loFormSet.AriaForm1
WITH .grdAlo
.RecordSource = loParentForm.lcTmpCutPik
.Column3.ControlSource  = loParentForm.lcTmpCutPik+".Order"
.Column4.ControlSource  = loParentForm.lcTmpCutPik+".Qty1"
.Column5.ControlSource  = loParentForm.lcTmpCutPik+".Qty2"
.Column6.ControlSource  = loParentForm.lcTmpCutPik+".Qty3"
.Column7.ControlSource  = loParentForm.lcTmpCutPik+".Qty4"
.Column8.ControlSource  = loParentForm.lcTmpCutPik+".Qty5"
.Column9.ControlSource  = loParentForm.lcTmpCutPik+".Qty6"
.Column10.ControlSource = loParentForm.lcTmpCutPik+".Qty7"
.Column11.ControlSource = loParentForm.lcTmpCutPik+".Qty8"
.Column12.ControlSource = loParentForm.lcTmpCutPik+".TotQty"
ENDWITH
.cntOrder.Scale = SCALE.SCALE
LOCAL lnI,lcI
FOR lnI = 1 TO 8
lcI = STR(lnI,1)
.cntOrder.txtQty&lcI..Value  = EVALUATE("lnCut"+lcI)
.cntActual.txtQty&lcI..Value = MAX(EVALUATE("lnCut"+lcI)-EVALUATE("lnOpen"+lcI),0)
ENDFOR
.cntOrder.mGetTotal()
.cntActual.mGetTotal()
ENDWITH
*--end of lfMfEdAloInit.

*!*************************************************************
*! Name      : lfGetItem
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/15/2004
*! Purpose   : function to change the item/color format to item format
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetItem
LPARAMETERS lcItem,lcColor,loFormSet

LOCAL lcRetItem
IF loFormSet.lcTranType = 'T'
lcRetItem = STUFF(loFormSet.lcItmMsk,1,LEN(loFormSet.lcMjrMsk),PADR(lcItem,LEN(loFormSet.lcMjrMsk)))
lcRetItem = STUFF(loFormSet.lcItmMsk,loFormSet.lnClrPos,LEN(loFormSet.lcClrMsk),PADR(lcColor,LEN(loFormSet.lcClrMsk)))
ELSE
lcRetItem = lcItem
ENDIF
RETURN lcRetItem
*--end of lfGetItem.

*!*************************************************************
*! Name      : lfTableUpdate
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/22/2004
*! Purpose   : function to Update Sql Tables.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfTableUpdate
LPARAMETERS loFormSet

*--Open Dictionary files.
LOCAL lnAlias,lcDictionaryDir,lnConnectionHandlar,lcTranCode,lnI,llFound
lnAlias = SELECT(0)
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*lcDictionaryDir = STRTRAN(oAriaApplication.ProgramHome,"PRGS\","SQLDictionary\")
lcDictionaryDir = STRTRAN(oAriaApplication.ProgramHome,"PRGS\","Sysfiles\")
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[End]

*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
*=gfOpenFile(lcDictionaryDir+'SYDINDEX','Cfile','SH')
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*lnUniqueIndex =oAriaApplication.RemoteCompanyData.execute ("Select * from SYDINDEX WHERE lunique ",;
'',"SYDINDEX","",oAriaApplication.cAria4SysFiles,;
3,"",loFormSet.DataSessionID)
lnUniqueIndex =oAriaApplication.RemoteCompanyData.execute ("Select SYDINDEX.* from SYDINDEX INNER JOIN SYDFILES ON SYDFILES.CFILE_NAM = SYDINDEX.CFILE_NAM  WHERE SYDINDEX.lunique and SYDFILES.CVER = 'A40'",;
'',"SYDINDEX","",oAriaApplication.cAria4SysFiles,;
3,"",loFormSet.DataSessionID)
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[End]
IF lnUniqueIndex  > 0
SELECT SYDINDEX
=CURSORSETPROP("Buffering",3,'SYDINDEX')
INDEX on CFILE_NAM TAG 'CFILE'
ELSE
SELECT (lnAlias)
RETURN .F.
ENDIF
*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
=oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
USE IN SYDINDEX
SELECT (lnAlias)
RETURN .F.
ENDIF

FOR lnI = 1 TO ALEN(laTableUpdate,1)
llFound = .F.
*E303030,1 BEGIN
*IF SEEK(PADR(laTableUpdate[lnI,2],8),'SYDINDEX')
IF SEEK(PADR(laTableUpdate[lnI,2],oAriaApplication.FileW),'SYDINDEX')
*E303030,1 END
SELECT SYDINDEX
LOCATE REST WHILE cFile_Nam = laTableUpdate[lnI,2] FOR lunique
IF FOUND()
llFound = .T.
ENDIF
ENDIF

IF llFound
LOCAL ARRAY laFields[1]
oAriaApplication.RemoteCompanyData.mindexfields(SYDINDEX.cIndx_Exp,@laFields)

LOCAL lnJ,lcPrimaryKeyList
lcPrimaryKeyList = ''
FOR lnJ = 1 TO ALEN(laFields,1)
lcPrimaryKeyList = lcPrimaryKeyList + IIF(EMPTY(lcPrimaryKeyList),'',',') + laFields[lnJ]
ENDFOR

SELECT (laTableUpdate[lnI,1])
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(laTableUpdate[lnI,1],lcTranCode,loFormSet.DataSessionId,;
lcPrimaryKeyList,laTableUpdate[lnI,2],SYDINDEX.cFile_Tag)
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
=oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
=TABLEREVERT(.T.)
=oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
USE IN SYDINDEX
SELECT(lnAlias)
RETURN .F.
ELSE
=TABLEUPDATE(.T.,.T.)
ENDIF
ELSE
=oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
USE IN SYDINDEX
SELECT(lnAlias)
RETURN .F.
ENDIF
ENDFOR

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
=oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
=oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
USE IN SYDINDEX
SELECT(lnAlias)
RETURN .F.
ENDIF

USE IN SYDINDEX
SELECT(lnAlias)
*--end of lfTableUpdate. PADR

*!*************************************************************
*! Name      : lfRefreshToolBarButtons
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/01/2004
*! Purpose   : function to Refresh ToolBar Buttons
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfRefreshToolBarButtons
LPARAMETERS loFormSet

DO CASE
CASE loFormSet.ActiveMode # "V"
oAriaApplication.otoolbar.ChangeButtonStatus('cmdGenerate','DISABLE')
oAriaApplication.otoolbar.ChangeButtonStatus('cmdClose','DISABLE')
CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'O'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcStatus = LANG_MFCSSH_OPEN
loFormSet.lcStatus = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_OPEN,loFormSet.GetHeaderText("LANG_MFCSSH_OPEN",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

oAriaApplication.otoolbar.ChangeButtonStatus('cmdGenerate','DISABLE')
*B610723,1 TMI 05/05/2014 14:37 [Start] use loFormSet.llTKTCLOSE instead of calling the gfUserPriv function
*IF gfUserPriv('MF','MFCSSH','TKTCLOSE')
IF loFormSet.llTKTCLOSE
  *B610723,1 TMI 05/05/2014 14:37 [End  ] 
  oAriaApplication.otoolbar.ChangeButtonStatus('cmdClose','ENABLED')
ENDIF
CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'H'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcStatus = LANG_MFCSSH_HOLD
loFormSet.lcStatus = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_HOLD,loFormSet.GetHeaderText("LANG_MFCSSH_HOLD",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

oAriaApplication.otoolbar.ChangeButtonStatus('cmdGenerate','ENABLED')
oAriaApplication.otoolbar.ChangeButtonStatus('cmdClose','DISABLE')
CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'C'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcStatus = LANG_MFCSSH_COMPLETE
loFormSet.lcStatus = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_COMPLETE,loFormSet.GetHeaderText("LANG_MFCSSH_COMPLETE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

oAriaApplication.otoolbar.ChangeButtonStatus('cmdGenerate','DISABLE')
oAriaApplication.otoolbar.ChangeButtonStatus('cmdClose','ENABLED')
CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'S'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcStatus = LANG_MFCSSH_CLOSED
loFormSet.lcStatus = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CLOSED,loFormSet.GetHeaderText("LANG_MFCSSH_CLOSED",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

oAriaApplication.otoolbar.ChangeButtonStatus('cmdGenerate','DISABLE')
oAriaApplication.otoolbar.ChangeButtonStatus('cmdClose','DISABLE')
CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'A'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcStatus = LANG_MFCSSH_ACTUALIZ
loFormSet.lcStatus = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ACTUALIZ,loFormSet.GetHeaderText("LANG_MFCSSH_ACTUALIZ",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

oAriaApplication.otoolbar.ChangeButtonStatus('cmdGenerate','DISABLE')
oAriaApplication.otoolbar.ChangeButtonStatus('cmdClose','ENABLED')
CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'X'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcStatus = LANG_MFCSSH_CANCELED
loFormSet.lcStatus = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CANCELED,loFormSet.GetHeaderText("LANG_MFCSSH_CANCELED",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

oAriaApplication.otoolbar.ChangeButtonStatus('cmdGenerate','DISABLE')
oAriaApplication.otoolbar.ChangeButtonStatus('cmdClose','DISABLE')
CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'B'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcStatus = LANG_MFCSSH_BID
loFormSet.lcStatus = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_BID,loFormSet.GetHeaderText("LANG_MFCSSH_BID",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

oAriaApplication.otoolbar.ChangeButtonStatus('cmdGenerate','DISABLE')
oAriaApplication.otoolbar.ChangeButtonStatus('cmdClose','DISABLE')
ENDCASE
*--end of lfRefreshToolBarButtons
IF !loFormSet.allowDelete OR loFormSet.activemode <> "V" OR INLIST(EVALUATE(loFormSet.lcPosHdr+'.Status'),'H','S','X','B')
oariaapplication.otoolbar.AllowDelete = .F.
oariaapplication.otoolbar.cmddelete.enabled = .F.
ELSE
oariaapplication.otoolbar.AllowDelete = .T.
oariaapplication.otoolbar.cmddelete.enabled = .T.
ENDIF

*!*************************************************************
*! Name      : lfRebIsLog
*! Developer : Wael Ali Mohamed (WAM) (B607879)
*! Date      : 12/14/2006
*! Purpose   : Build the Issue log browse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION  lfRebIsLog
LPARAMETERS loFormSet

DO CASE
CASE m.cCatgTyp='F' .OR. (m.cCatgTyp='T' .AND. m.Trim_Invt)
IF !INLIST(loParentForm.laSetups[11,2],"A","S")
WITH loFormSet.AriaForm1.grdIsLog
.Columncount = 20
.RecordSource = 'MATINVJL'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column1.ControlSource  = "PADR(IIF(MATINVJL.cIRType='I','"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
.Column1.ControlSource  = "PADR(IIF(MATINVJL.cIRType='I','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))+"','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURN,loFormSet.GetHeaderText("LANG_MFCSSH_RETURN",loFormSet.HeaderAlias))+"'),6)"
*N000682,1 11/20/2012 MMT Globlization changes[End]

.Column2.ControlSource  = "MATINVJL.cWareCode"
.Column2.Visible        = (loParentForm.laSetups[2,2]='Y')
.Column3.ControlSource  = "MATINVJL.dTrDate"
.Column4.ControlSource  = "MATINVJL.CSESSION"
.Column5.ControlSource  = "ABS(MATINVJL.nTotStk)"
.Column6.ControlSource  = "MATINVJL.nCost"
.Column7.ControlSource  = "MATINVJL.cApInvNo"
.Column7.Visible        = (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)
.Column8.ControlSource  = "gfCodDes(MATINVJL.cOprCode,'MFGCODE')"
.Column9.ControlSource  = "MATINVJL.cLotNo"
.Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
.Column10.ControlSource = "ABS(MATINVJL.nStk1)"
.Column11.ControlSource = "ABS(MATINVJL.nStk2)"
.Column12.ControlSource = "ABS(MATINVJL.nStk3)"
.Column13.ControlSource = "ABS(MATINVJL.nStk4)"
.Column14.ControlSource = "ABS(MATINVJL.nStk5)"
.Column15.ControlSource = "ABS(MATINVJL.nStk6)"
.Column16.ControlSource = "ABS(MATINVJL.nStk7)"
.Column17.ControlSource = "ABS(MATINVJL.nStk8)"
.Column18.Visible       = .F.
.Column19.Visible       = .F.
.Column20.Visible       = .F.
ENDWITH
ELSE
WITH loFormSet.AriaForm1.grdIsLog
.RecordSource = 'BOMCOST'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column1.ControlSource  = "PADR(IIF(BOMCOST.nTotQty>0,'"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
.Column1.ControlSource  = "PADR(IIF(BOMCOST.nTotQty>0,'"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))+"','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURN,loFormSet.GetHeaderText("LANG_MFCSSH_RETURN",loFormSet.HeaderAlias))+"'),6)"
*N000682,1 11/20/2012 MMT Globlization changes[End]

.Column2.ControlSource  = "BOMCOST.cWareCode"
.Column2.Visible        = (loParentForm.laSetups[2,2]='Y')
.Column3.ControlSource  = "BOMCOST.dTranDate"
.Column4.ControlSource  = "IIF(BOMCOST.nTotQty>0,BOMCOST.cISession,BOMCOST.cRSession)"
.Column5.ControlSource  = "ABS(BOMCOST.nTotQty)"
.Column6.ControlSource  = "BOMCOST.nUnitCst"
.Column7.ControlSource  = "BOMCOST.cApInvNo"
.Column7.Visible        = (OCCURS('AP',oAriaApplication.CompanyInstalledModules)<>0)
.Column8.ControlSource  = "gfCodDes(BOMCOST.cOprCode,'MFGCODE')"
.Column9.ControlSource  = "BOMCOST.cLotNo"
.Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
.Column10.Visible       = .F.
.Column11.Visible       = .F.
.Column12.Visible       = .F.
.Column13.Visible       = .F.
.Column14.Visible       = .F.
.Column15.Visible       = .F.
.Column16.Visible       = .F.
.Column17.Visible       = .F.
.Column18.Visible       = .F.
.Column19.Visible       = .F.
.Column20.Visible       = .F.
ENDWITH
ENDIF
CASE m.cCatgTyp = 'S' .AND. SEEK(m.Item,'Style')
WITH loFormSet.AriaForm1.grdIsLog
.RecordSource = 'TMPISSLOG'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column1.ControlSource  = "PADR(IIF(TMPISSLOG.cIRType='I','"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
.Column1.ControlSource  = "PADR(IIF(TMPISSLOG.cIRType='I','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))+"','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURN,loFormSet.GetHeaderText("LANG_MFCSSH_RETURN",loFormSet.HeaderAlias))+"'),6)"
*N000682,1 11/20/2012 MMT Globlization changes[End]

.Column2.ControlSource  = "TMPISSLOG.cWareCode"
.Column2.Visible        = (loParentForm.laSetups[2,2]='Y')
.Column3.ControlSource  = "TMPISSLOG.dTrDate"
.Column4.ControlSource  = "TMPISSLOG.CSESSION"
.Column5.ControlSource  = "ABS(TMPISSLOG.nTotStk)"
.Column6.ControlSource  = "TMPISSLOG.nCost"
.Column7.Visible        = .F.
.Column8.ControlSource  = "gfCodDes(TMPISSLOG.cOprCode,'MFGCODE')"
.Column9.ControlSource  = "TMPISSLOG.cLotNo"
.Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
.Column10.ControlSource = "ABS(TMPISSLOG.nStk1)"
.Column11.ControlSource = "ABS(TMPISSLOG.nStk2)"
.Column12.ControlSource = "ABS(TMPISSLOG.nStk3)"
.Column13.ControlSource = "ABS(TMPISSLOG.nStk4)"
.Column14.ControlSource = "ABS(TMPISSLOG.nStk5)"
.Column15.ControlSource = "ABS(TMPISSLOG.nStk6)"
.Column16.ControlSource = "ABS(TMPISSLOG.nStk7)"
.Column17.ControlSource = "ABS(TMPISSLOG.nStk8)"
.Column18.Visible       = .F.
.Column19.Visible       = .F.
.Column20.Visible       = .F.
ENDWITH
OTHERWISE
WITH loFormSet.AriaForm1.grdIsLog
.RecordSource = 'BOMCOST'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column1.ControlSource  = "PADR(IIF(BOMCOST.nTotQty>0,'"+LANG_MFCSSH_ISSUE+"','"+LANG_MFCSSH_RETURN+"'),6)"
.Column1.ControlSource  = "PADR(IIF(BOMCOST.nTotQty>0,'"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ISSUE,loFormSet.GetHeaderText("LANG_MFCSSH_ISSUE",loFormSet.HeaderAlias))+"','"+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_RETURN,loFormSet.GetHeaderText("LANG_MFCSSH_RETURN",loFormSet.HeaderAlias))+"'),6)"
*N000682,1 11/20/2012 MMT Globlization changes[End]

.Column2.Visible        = .F.
.Column3.ControlSource  = "BOMCOST.dTranDate"
.Column4.ControlSource  = "IIF(BOMCOST.nTotQty>0,BOMCOST.cISession,BOMCOST.cRSession)"
.Column5.ControlSource  = "ABS(BOMCOST.nTotQty)"
.Column6.ControlSource  = "BOMCOST.nUnitCst"
.Column7.Visible        = .F.
.Column8.ControlSource  = "gfCodDes(BOMCOST.cOprCode,'MFGCODE')"
.Column8.ColumnOrder    = 19
.Column9.ControlSource  = "BOMCOST.cLotNo"
.Column9.Visible        = (EVALUATE(loParentForm.lcPosHdr+'.CMULTILOT')='M')
.Column9.ColumnOrder    = 20
.Column10.Visible       = .F.
.Column11.Visible       = .F.
.Column12.Visible       = .F.
.Column13.Visible       = .F.
.Column14.Visible       = .F.
.Column15.Visible       = .F.
.Column16.Visible       = .F.
.Column17.Visible       = .F.
.Column18.ControlSource = "ABS(BOMCOST.nTotCst)"
.Column18.ColumnOrder   = 8
.Column19.ControlSource = "BOMCOST.nUnitAcSt"
.Column19.ColumnOrder   = 9
.Column20.ControlSource = "ABS(BOMCOST.nTotAcSt)"
ENDWITH
ENDCASE

*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [Start]
*!*************************************************************
*! Name      : lfModActInit
*! Developer : Khalid Mohi El-Din
*! Date      : 12/20/2005
*! Purpose   : To be called from init MFACTONE.SCX
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
*! Returns   : None
*!*************************************************************
*N039501,1 KHM 12/20/2005
*!*************************************************************
FUNCTION lfOneOprInit
LPARAMETERS loFormSet

WITH loFormSet.ariaForm1
*N000682,1 MMT 11/20/2012 Globalization Changes[Start]
*.Caption = 'Actualize'
.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ACTUALIZE,loFormSet.GetHeaderText("LANG_MFCSSH_ACTUALIZE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

.dtPickerActualDate.value = oAriaApplication.SystemDate
.SetAll('Enabled',.F.,"AriaTextBox")
.dtPickerActualDate.Enabled = .T.

.txtBudget.Value = loFormSet.lnBudget
.txtIssued.Value = loFormSet.lnIssued
.txtOpen.Value   = MAX(loFormSet.lnBudget-loFormSet.lnIssued,0)
ENDWITH
*!*************************************************************
*! Name      : lfModActInit
*! Developer : Khalid Mohi El-Din
*! Date      : 12/20/2005
*! Purpose   : To be called from init MFACTUAL.SCX
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
*! Returns   : None
*!*************************************************************
*!N039501,1 KHM 12/20/2005
*!*************************************************************
FUNCTION lfModActInit
LPARAMETERS loFormSet

WITH loFormSet.ariaForm1
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Caption = 'Actualize'
*.optActualize.Ariaoptionbutton1.Caption = 'Actualize '+ ALLTRIM(gfCodDes(loParentForm.lcFirstOpr,'MfgCode'))
*  .optActualize.Ariaoptionbutton2.Caption = ;
IIF(loParentForm.lcTranType='I', 'Actualize Style PO',;
IIF(loParentForm.lcTranType='T',;
'Actualize Material Manufacturing Order','Actualize Cutting Ticket'))
.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ACTUALIZE,loFormSet.GetHeaderText("LANG_MFCSSH_ACTUALIZE",loFormSet.HeaderAlias))
.optActualize.Ariaoptionbutton1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_ACTUALIZE,loFormSet.GetHeaderText("LANG_MFCSSH_ACTUALIZE",loFormSet.HeaderAlias)) +;
ALLTRIM(gfCodDes(loParentForm.lcFirstOpr,'MfgCode'))
.optActualize.Ariaoptionbutton2.Caption = ;
IIF(loParentForm.lcTranType='I', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO_ACT,loFormSet.GetHeaderText("LANG_MFCSSH_PO_ACT",loFormSet.HeaderAlias)),;
IIF(loParentForm.lcTranType='T',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_MAPO_ACT,loFormSet.GetHeaderText("LANG_MFCSSH_MAPO_ACT",loFormSet.HeaderAlias)),;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CT_ACT,loFormSet.GetHeaderText("LANG_MFCSSH_CT_ACT",loFormSet.HeaderAlias))))
*N000682,1 MMT 11/20/2012 Globalization Changes[End]
.dtPickerActualDate.value = oAriaApplication.SystemDate
.optActualize.Value       = loFormSet.lnOption
.SetAll('Enabled',.F.,"AriaTextBox")
.dtPickerActualDate.Enabled = .T.
.cboActLot.Enabled = .T.
IF !(!loFormSet.llMultiLot AND loFormSet.lcMultiLot = 'M')
.optActualize.Visible = .F.
.shpHeader.Height = .shpHeader.Height - 35
.shpActualize.Top = .shpActualize.Top - 35

.lblBudget.Top    = .lblBudget.Top - 35
.lblSimCol1.Top   = .lblSimCol1.Top - 35
.txtBudget.Top    = .txtBudget.Top - 35

.lblReceived.Top = .lblReceived.Top - 35
.lblSimCol2.Top  = .lblSimCol2.Top - 35
.txtReceived.Top = .txtReceived.Top - 35

.lblDamaged.Top  = .lblDamaged.Top - 35
.lblSimCol3.Top  = .lblSimCol3.Top - 35
.txtDamaged.Top  = .txtDamaged.Top -35

.lblCanceled.Top = .lblCanceled.Top - 35
.lblSimCol4.Top  = .lblSimCol4.Top - 35
.txtCanceled.Top = .txtCanceled.Top - 35

.lblOpen.Top    = .lblOpen.Top - 35
.lblSimCol5.Top = .lblSimCol5.Top - 35
.txtOpen.Top    = .txtOpen.Top - 35

.lblInfo.Top  = .lblInfo.Top - 35
.shpLot.Top   = .shpLot.Top  - 35
.lblLotNo.Top = .lblLotNo.Top - 35
.cboActLot.Top = .cboActLot.Top - 35
.shpOKCan.Top = .shpOKCan.Top - 35
.cmdOk.Top    = .cmdOk.Top - 35
.cmdCancel.Top= .cmdCancel.Top - 35

.Height = .Height - 40
.cboActLot.Visible = .F.
.lblLotNo.Visible = .F.
.shplot.Visible = .F.

ELSE
.cboActLot.Value = laActLot[1]
ENDIF

.txtBudget.Value   = loFormSet.lnBudget
.txtReceived.Value = loFormSet.lnReceive
.txtDamaged.Value  = loFormSet.lnDamage
.txtCanceled.Value = loFormSet.lnCancel
.txtOpen.Value     = MAX(loFormSet.lnBudget-(loFormSet.lnReceive+loFormSet.lnDamage+loFormSet.lnCancel),0)
IF .txtOpen.Value > 0
.txtOpen.BackColor = RGB(255,0,0)
ENDIF
ENDWITH
*!*************************************************************
*! Name      : lfvOptAct
*! Developer : Khalid Mohi El-Din
*! Date      : 12/20/2005
*! Purpose   :
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
*! Returns   : None
*!*************************************************************
*N039501,1 KHM 12/20/2005
*!*************************************************************
FUNCTION lfvOptAct
LPARAMETERS loFormSet, lnOption

=IIF(lnOption = 1, lfGetSummary(laActLot[1]), lfGetSummary(''))
puActLot = IIF(lnOption = 1,1,0)
WITH loFormSet.ariaForm1
.cboActLot.Enabled = IIF(lnOption = 1, .T.,.F.)
.cboActLot.Value   = IIF(lnOption = 1,laActLot[1],'')
STORE lnLotbud TO .txtBudget.Value, loFormSet.lnBudget
STORE lnLotRcv TO .txtReceived.Value, loFormSet.lnReceive
STORE lnLotDmg TO .txtDamaged.Value, loFormSet.lnDamage
STORE lnLotCan TO .txtCanceled.Value, loFormSet.lnCancel
.txtOpen.Value     = MAX(loFormSet.lnBudget-(loFormSet.lnReceive+loFormSet.lnDamage+loFormSet.lnCancel),0)
ENDWITH
*!*************************************************************
*! Name      : lfvOkAct
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/02/2004
*! Purpose   : Actualize the whole cutting ticket or a specefic lot in the
*!             first operation.
*!*************************************************************
*! Calls     : lfBrowLots,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvOkAct()
*!*************************************************************
*N039501,1 KHM 12/20/2005
FUNCTION lfvOkAct
LPARAMETERS loFormSet
PRIVATE lnAlias,lcLotNo,lcItem,lcColor,lnOpen1,lnOpen2,lnOpen3,lnOpen4,;
lnOpen5,lnOpen6,lnOpen7,lnOpen8,lnOpen,lnRecv,llInHouse,;
lcContCode,lcContName,lcCurrTag,lnUpdYield,lcKeyVal


LOCAL lcSeekKey
lcSeekKey = EVALUATE(loParentForm.lcPosHdr+'.cBusDocu')+;
EVALUATE(loParentForm.lcPosHdr+'.cStyType')+;
EVALUATE(loParentForm.lcPosHdr+'.PO')
lnAlias   = SELECT()
lcCurrTag = ORDER(loParentForm.lcOprDet)
SET ORDER TO TAG 'LOT' IN (loParentForm.lcOprDet)

STORE .F. TO llTktAllo,llRelAll

IF loParentForm.lcTranType <> 'T'
SELECT (loParentForm.lcTranFile)
LOCATE
SUM REST WHILE EVALUATE(loParentForm.lcFileKey) = lcSeekKey ;
Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO ;
lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8,lnQty1,;
lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8
LOCATE
DO WHILE EVAL(loParentForm.lcFileKey) = lcSeekKey AND !llTktAllo
SUM REST WHILE EVALUATE(loParentForm.lcFileKey) = lcSeekKey ;
Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO ;
lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8,lnQty1,;
lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8

SELECT (loParentForm.lcOprDet)
=SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
loParentForm.lcFirstOpr+;
IIF(loFormSet.AriaForm1.optActualize.Value = 1,laActLot[puActLot],''))
DO WHILE !llTktAllo AND ;
cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+loParentForm.lcFirstOpr+;
IIF(loFormSet.AriaForm1.optActualize.Value = 1,laActLot[puActLot],'')
lcItem  = Item
lcLotNo = cLotNo
STORE 0 TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
loParentForm.lcFirstOpr+lcLotNo+loParentForm.lcInvType+lcItem ;
FOR cDyelot = EVALUATE(loParentForm.lcOprDet+'.cDyelot')

FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
lnOpen&lcCount = lnOpen&lcCount+IIF(TranCd='1',nLotQty&lcCount,-nLotQty&lcCount)
ENDFOR
ENDSCAN
IF (lnOrd1 > lnQty1-lnOpen1 OR lnOrd2 > lnQty2-lnOpen2 OR lnOrd3 > lnQty3-lnOpen3 OR ;
lnOrd4 > lnQty4-lnOpen4 OR lnOrd5 > lnQty5-lnOpen5 OR lnOrd6 > lnQty6-lnOpen6 OR ;
lnOrd7 > lnQty7-lnOpen7 OR lnOrd8 > lnQty8-lnOpen8)
llTktAllo = .T.
ENDIF
SELECT (loParentForm.lcOprDet)
ENDDO
SELECT (loParentForm.lcTranFile)
ENDDO
*B602482,1 Message : 38168
*B602482,1 Cutting ticket# 999999 has pieces allocated from orders.
*B602482,1 Proceed with acualization and modify the allocated
*B602482,1 quantity from order lines to keep track of this
*B602482,1 allocation, or cancel
*B602482,1 Button : 38022
*B602482,1 < Proceed> <Cancel>
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*IF llTktAllo AND gfModalGen('QRM38168B38002','ALERT',;
IIF(loParentForm.lcTranType='M','Cutting Ticket#: ','PO#:')+EVALUATE(loParentForm.lcPosHdr+'.PO')) = 2
IF llTktAllo AND gfModalGen('QRM38168B38002','ALERT',;
IIF(loParentForm.lcTranType='M',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_CUTTKT,loParentForm.GetHeaderText("LANG_MFCSSH_CUTTKT",loParentForm.HeaderAlias))+': ',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFCSSH_PO,loParentForm.GetHeaderText("LANG_MFCSSH_PO",loParentForm.HeaderAlias))+':')+;
EVALUATE(loParentForm.lcPosHdr+'.PO')) = 2
*N000682,1 MMT 12/09/2012 Globalization changes[End]
SELECT (lnAlias)
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)
RETURN
ENDIF
ENDIF

lnUpdYield = 0

IF SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
loParentForm.lcFirstOpr+;
IIF(loFormSet.AriaForm1.optActualize.Value = 1,laActLot[puActLot],''),loParentForm.lcOprDet)
SELECT (loParentForm.lcOprDet)
DO WHILE !EOF() .AND. cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
loParentForm.lcFirstOpr+;
IIF(loFormSet.AriaForm1.optActualize.Value = 1,laActLot[puActLot],'')
lcItem     = Item
llInHouse  = lInHouse
lcContCode = cContCode
lcContName = cContName
lcLotNo    = cLotNo
lcItDye    = cDyelot
STORE 0 TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8,lnOpen,lnRecv
STORE 0 TO lnRecv1,lnRecv2,lnRecv3,lnRecv4,lnRecv5,lnRecv6,lnRecv7,lnRecv8

llActualize = .T.
lcNxtDye = lcItDye
IF loParentForm.llUseDyelot
SKIP 1
IF cDyelot # lcItDye .AND. TranCd = '1'
lcNxtDye = cDyelot
ENDIF
SKIP -1
ENDIF
SCAN REST WHILE cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
loParentForm.lcFirstOpr+lcLotNo+loParentForm.lcInvType+lcItem;
FOR   llActualize .AND. cDyelot = lcItDye
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
lnOpen&lcCount=lnOpen&lcCount+IIF(TranCd='1',nLotQty&lcCount,-nLotQty&lcCount)
lnRecv&lcCount=lnRecv&lcCount+IIF(TranCd='2',nLotQty&lcCount,0)
ENDFOR
IF loParentForm.lcTRanType <> 'T'
lnOpen = MAX(lnOpen1,0)+MAX(lnOpen2,0)+MAX(lnOpen3,0)+MAX(lnOpen4,0)+;
MAX(lnOpen5,0)+MAX(lnOpen6,0)+MAX(lnOpen7,0)+MAX(lnOpen8,0)
ELSE
lnOpen = lnOpen + IIF(TranCd='1',nLotTotQty,-nLotTotQty)
ENDIF
lnRecv = lnRecv  + IIF(TranCd='2',nLotTotQty,0)
ENDSCAN
IF !llActualize
LOOP
ENDIF
lcKeyVal = cIMTYp+cTktNo+cOprCode+cLotNo+cInvType+Item+TranCd+cDyelot
SELECT (loParentForm.lcMFGOPRDT)
LOCATE FOR cIMTyp+ctktno+coprcode+clotno+trancd = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
loParentForm.lcFirstOpr+lcLotNo+'1' AND ;
Item = lcItem AND cDyelot = lcItDye

SELECT (loParentForm.lcTmpTkt)
=SEEK(loParentForm.lcInvType+lcItem)
COUNT REST WHILE cInvType+Item+STR(LineNo,6) = loParentForm.lcInvType+lcItem;
FOR cDyelot = lcItDye TO lnRecords
IF lnRecords=1
=SEEK(loParentForm.lcInvType+lcItem)
IF loParentForm.llUseDyelot
LOCATE REST WHILE cInvType+Item+STR(LineNo,6) = loParentForm.lcInvType+lcItem;
FOR cDyelot = lcItDye
ENDIF

REPLACE nQty1   WITH nQty1   - lnOpen1 ,;
nQty2   WITH nQty2   - lnOpen2 ,;
nQty3   WITH nQty3   - lnOpen3 ,;
nQty4   WITH nQty4   - lnOpen4 ,;
nQty5   WITH nQty5   - lnOpen5 ,;
nQty6   WITH nQty6   - lnOpen6 ,;
nQty7   WITH nQty7   - lnOpen7 ,;
nQty8   WITH nQty8   - lnOpen8 ,;
nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8
ELSE
SELECT *,00000 AS nCan1,00000 AS nCan2,00000 AS nCan3,00000 AS nCan4,;
00000 AS nCan5,00000 AS nCan6,00000 AS nCan7,00000 AS nCan8,;
000000 AS nTotCan, RECNO() AS nBudRecNo FROM (loParentForm.lcTmpTkt) ;
WHERE ITEM+cDyelot = lcItem+lcItDye ;
INTO DBF (oAriaApplication.WorkDir+loParentForm.lcisslogfile)
STORE MAX(lnOpen1,0) TO lnCan1,lnBal1
STORE MAX(lnOpen2,0) TO lnCan2,lnBal2
STORE MAX(lnOpen3,0) TO lnCan3,lnBal3
STORE MAX(lnOpen4,0) TO lnCan4,lnBal4
STORE MAX(lnOpen5,0) TO lnCan5,lnBal5
STORE MAX(lnOpen6,0) TO lnCan6,lnBal6
STORE MAX(lnOpen7,0) TO lnCan7,lnBal7
STORE MAX(lnOpen8,0) TO lnCan8,lnBal8
STORE lnCan1+lnCan2+lnCan3+lnCan4+lnCan5+lnCan6+lnCan7+lnCan8 TO lnTotCan,lnTotBal
SCATTER MEMVAR

=SEEK(lcItem,'Style') .AND. SEEK('S'+Style.Scale,'Scale')

*B609396,1 SMA 08/30/2010 Fix error while receiving cutting ticket manufacturing operation  ....[BEGIN]
*lcConCnTit = IIF(loParentForm.lcTranType='T','Material/Color',lcItmHdr)+' '+;
ALLTRIM(lcItem)+'Cancelled Quantity'
lcConCnTit = IIF(loParentForm.lcTranType='T','Material/Color',loParentForm.lcItmHdr)+' '+;
ALLTRIM(lcItem)+'Cancelled Quantity'
*B609396,1 SMA 08/30/2010 Fix error while receiving cutting ticket manufacturing operation  ....[END]
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFCONCN.SCX")
=gfCallForm('MFCONCN')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
USE IN (loParentForm.lcisslogfile)
ENDIF
SELECT (loParentForm.lcTmpTkt)
=SEEK(loParentForm.lcInvType+lcItem)

STORE 0 TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8
SCAN REST WHILE cInvType+Item+STR(LineNo,6) = loParentForm.lcInvType+lcItem;
FOR cDyelot = lcItDye
SELECT (loParentForm.lcPosLn)
LOCATE FOR Style+STR(LineNo,6)+TranCd = ;
lcItem+STR(EVALUATE(loParentForm.lcTmpTkt+'.LineNo'),6)+'1'

lnOpen1 = lnOpen1 + Qty1 - EVALUATE(loParentForm.lcTmpTkt+'.nQty1')
lnOpen2 = lnOpen2 + Qty2 - EVALUATE(loParentForm.lcTmpTkt+'.nQty2')
lnOpen3 = lnOpen3 + Qty3 - EVALUATE(loParentForm.lcTmpTkt+'.nQty3')
lnOpen4 = lnOpen4 + Qty4 - EVALUATE(loParentForm.lcTmpTkt+'.nQty4')
lnOpen5 = lnOpen5 + Qty5 - EVALUATE(loParentForm.lcTmpTkt+'.nQty5')
lnOpen6 = lnOpen6 + Qty6 - EVALUATE(loParentForm.lcTmpTkt+'.nQty6')
lnOpen7 = lnOpen7 + Qty7 - EVALUATE(loParentForm.lcTmpTkt+'.nQty7')
lnOpen8 = lnOpen8 + Qty8 - EVALUATE(loParentForm.lcTmpTkt+'.nQty8')
lnOpen  = lnOpen1+lnOpen2+lnOpen3+lnOpen4+lnOpen5+lnOpen6+lnOpen7+lnOpen8

SCATTER FIELDS ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotOrd TO laOrdQty

IF (Ord1 > EVALUATE(loParentForm.lcTmpTkt+'.nQty1') OR ;
Ord2 > EVALUATE(loParentForm.lcTmpTkt+'.nQty2') OR ;
Ord3 > EVALUATE(loParentForm.lcTmpTkt+'.nQty3') OR ;
Ord4 > EVALUATE(loParentForm.lcTmpTkt+'.nQty4') OR ;
Ord5 > EVALUATE(loParentForm.lcTmpTkt+'.nQty5') OR ;
Ord6 > EVALUATE(loParentForm.lcTmpTkt+'.nQty6') OR ;
Ord7 > EVALUATE(loParentForm.lcTmpTkt+'.nQty7') OR ;
Ord8 > EVALUATE(loParentForm.lcTmpTkt+'.nQty8'))
=lfTktAllo(lcItem,EVALUATE(loParentForm.lcPosLn+'.Dyelot'),;
EVALUATE(loParentForm.lcPosLn+'.LineNo'),.T.,loParentForm)
ENDIF

SELECT (loParentForm.lcPosHdr)
REPLACE TotOrd WITH TotOrd - EVALUATE(loParentForm.lcPosLn+'.TotOrd') + laOrdQty[9]

SELECT STYLE


=SEEK(EVALUATE(loParentForm.lcPosLn+'.Style'))

=RLOCK()
REPLACE WIP1   WITH WIP1 - (EVALUATE(loParentForm.lcPosLn+'.Qty1')-EVALUATE(loParentForm.lcTmpTkt+'.nQty1')) ,;
WIP2   WITH WIP2 - (EVALUATE(loParentForm.lcPosLn+'.Qty2')-EVALUATE(loParentForm.lcTmpTkt+'.nQty2')) ,;
WIP3   WITH WIP3 - (EVALUATE(loParentForm.lcPosLn+'.Qty3')-EVALUATE(loParentForm.lcTmpTkt+'.nQty3')) ,;
WIP4   WITH WIP4 - (EVALUATE(loParentForm.lcPosLn+'.Qty4')-EVALUATE(loParentForm.lcTmpTkt+'.nQty4')) ,;
WIP5   WITH WIP5 - (EVALUATE(loParentForm.lcPosLn+'.Qty5')-EVALUATE(loParentForm.lcTmpTkt+'.nQty5')) ,;
WIP6   WITH WIP6 - (EVALUATE(loParentForm.lcPosLn+'.Qty6')-EVALUATE(loParentForm.lcTmpTkt+'.nQty6')) ,;
WIP7   WITH WIP7 - (EVALUATE(loParentForm.lcPosLn+'.Qty7')-EVALUATE(loParentForm.lcTmpTkt+'.nQty7')) ,;
WIP8   WITH WIP8 - (EVALUATE(loParentForm.lcPosLn+'.Qty8')-EVALUATE(loParentForm.lcTmpTkt+'.nQty8')) ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8


UNLOCK
SELECT STYDYE

=SEEK(EVALUATE(loParentForm.lcPosLn+'.Style')+;
EVALUATE(loParentForm.lcPosLn+'.cWareCode')+SPACE(10))

=RLOCK()
REPLACE WIP1   WITH WIP1 - (EVALUATE(loParentForm.lcPosLn+'.Qty1')-EVALUATE(loParentForm.lcTmpTkt+'.nQty1')) ,;
WIP2   WITH WIP2 - (EVALUATE(loParentForm.lcPosLn+'.Qty2')-EVALUATE(loParentForm.lcTmpTkt+'.nQty2')) ,;
WIP3   WITH WIP3 - (EVALUATE(loParentForm.lcPosLn+'.Qty3')-EVALUATE(loParentForm.lcTmpTkt+'.nQty3')) ,;
WIP4   WITH WIP4 - (EVALUATE(loParentForm.lcPosLn+'.Qty4')-EVALUATE(loParentForm.lcTmpTkt+'.nQty4')) ,;
WIP5   WITH WIP5 - (EVALUATE(loParentForm.lcPosLn+'.Qty5')-EVALUATE(loParentForm.lcTmpTkt+'.nQty5')) ,;
WIP6   WITH WIP6 - (EVALUATE(loParentForm.lcPosLn+'.Qty6')-EVALUATE(loParentForm.lcTmpTkt+'.nQty6')) ,;
WIP7   WITH WIP7 - (EVALUATE(loParentForm.lcPosLn+'.Qty7')-EVALUATE(loParentForm.lcTmpTkt+'.nQty7')) ,;
WIP8   WITH WIP8 - (EVALUATE(loParentForm.lcPosLn+'.Qty8')-EVALUATE(loParentForm.lcTmpTkt+'.nQty8')) ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8


UNLOCK

IF !EMPTY(EVALUATE(loParentForm.lcTmpTkt+'.cDyelot'))


=SEEK(EVALUATE(loParentForm.lcPosLn+'.Style')+;
EVALUATE(loParentForm.lcPosLn+'.cWareCode')+;
EVALUATE(loParentForm.lcTmpTkt+'.cDyelot'))

=RLOCK()
REPLACE WIP1   WITH WIP1 - (EVALUATE(loParentForm.lcPosLn+'.Qty1')-EVALUATE(loParentForm.lcTmpTkt+'.nQty1')) ,;
WIP2   WITH WIP2 - (EVALUATE(loParentForm.lcPosLn+'.Qty2')-EVALUATE(loParentForm.lcTmpTkt+'.nQty2')) ,;
WIP3   WITH WIP3 - (EVALUATE(loParentForm.lcPosLn+'.Qty3')-EVALUATE(loParentForm.lcTmpTkt+'.nQty3')) ,;
WIP4   WITH WIP4 - (EVALUATE(loParentForm.lcPosLn+'.Qty4')-EVALUATE(loParentForm.lcTmpTkt+'.nQty4')) ,;
WIP5   WITH WIP5 - (EVALUATE(loParentForm.lcPosLn+'.Qty5')-EVALUATE(loParentForm.lcTmpTkt+'.nQty5')) ,;
WIP6   WITH WIP6 - (EVALUATE(loParentForm.lcPosLn+'.Qty6')-EVALUATE(loParentForm.lcTmpTkt+'.nQty6')) ,;
WIP7   WITH WIP7 - (EVALUATE(loParentForm.lcPosLn+'.Qty7')-EVALUATE(loParentForm.lcTmpTkt+'.nQty7')) ,;
WIP8   WITH WIP8 - (EVALUATE(loParentForm.lcPosLn+'.Qty8')-EVALUATE(loParentForm.lcTmpTkt+'.nQty8')) ,;
TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8


UNLOCK
ENDIF

SELECT (loParentForm.lcPosLn)
lnTotQty = Totqty
=RLOCK()
REPLACE Qty1   WITH MAX(EVALUATE(loParentForm.lcTmpTkt+'.nQty1'),0) ,;
Qty2   WITH MAX(EVALUATE(loParentForm.lcTmpTkt+'.nQty2'),0) ,;
Qty3   WITH MAX(EVALUATE(loParentForm.lcTmpTkt+'.nQty3'),0) ,;
Qty4   WITH MAX(EVALUATE(loParentForm.lcTmpTkt+'.nQty4'),0) ,;
Qty5   WITH MAX(EVALUATE(loParentForm.lcTmpTkt+'.nQty5'),0) ,;
Qty6   WITH MAX(EVALUATE(loParentForm.lcTmpTkt+'.nQty6'),0) ,;
Qty7   WITH MAX(EVALUATE(loParentForm.lcTmpTkt+'.nQty7'),0) ,;
Qty8   WITH MAX(EVALUATE(loParentForm.lcTmpTkt+'.nQty8'),0) ,;
TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
REPLACE Ord1   WITH laOrdQty[1] ,;
Ord2   WITH laOrdQty[2] ,;
Ord3   WITH laOrdQty[3] ,;
Ord4   WITH laOrdQty[4] ,;
Ord5   WITH laOrdQty[5] ,;
Ord6   WITH laOrdQty[6] ,;
Ord7   WITH laOrdQty[7] ,;
Ord8   WITH laOrdQty[8] ,;
TotOrd WITH laOrdQty[9]
UNLOCK
SELECT (loParentForm.lcPosHdr)

*MMT2
REPLACE NSTYORDER WITH MAX(NSTYORDER ,NSTYORDER +(EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty))
REPLACE OPEN WITH MAX(NSTYORDER - (RECEIVE+DAMAGE+CANCEL) ,0)
*MMT2

REPLACE nFCost1 WITH nFCost1 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost1') ,;
nFCost2 WITH nFCost2 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost2') ,;
nFCost3 WITH nFCost3 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost3') ,;
nFCost4 WITH nFCost4 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost4') ,;
nFCost5 WITH nFCost5 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost5') ,;
nFCost6 WITH nFCost6 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost6') ,;
nFCost7 WITH nFCost7 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nFCost7')

REPLACE Cancel  WITH Cancel  + MAX(lnTotQty - EVALUATE(loParentForm.lcPosLn+'.TotQty'),0),;
nICost1 WITH nICost1 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost1'),;
nICost2 WITH nICost2 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost2'),;
nICost3 WITH nICost3 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost3'),;
nICost4 WITH nICost4 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost4'),;
nICost5 WITH nICost5 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost5'),;
nICost6 WITH nICost6 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost6'),;
nICost7 WITH nICost7 + (EVALUATE(loParentForm.lcPosLn+'.TotQty')-lnTotQty)*EVALUATE(loParentForm.lcPosLn+'.nICost7')

lnUpdYield = IIF(lnUpdYield=0 AND lnOpen <> 0,;
gfModalGen('QRM38157B38006','ALERT'),lnUpdYield)
IF lnUpdYield = 1

SELECT (loParentForm.lcBomLine)
=SEEK(loParentForm.lcTranType+'1'+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
STR(EVALUATE(loParentForm.lcTmpTkt+'.lineno'),6))

SCAN REST WHILE CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE=;
loParentForm.lcTranType+'1'+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
STR(EVALUATE(loParentForm.lcTmpTkt+'.LineNo'),6)

STORE 0 TO lnReq1,lnReq2,lnReq3,lnReq4,lnReq5,lnReq6,lnReq7,lnReq8
IF loParentForm.lcTranType = 'T'
lnStyQty = lnOpen
ELSE
lnStyQty = 0
FOR lnCount = 1 TO 8
lcCount = STR(lnCount,1)
IF lcCount $ cSizes
lnStyQty   = lnStyQty + lnOpen&lcCount
lcCompSize = SUBSTR(cCompSizes,AT(lcCount,cSizes),1)
IF !EMPTY(lcCompSize)
lnReq&lcCompSize = lnReq&lcCompSize + lnOpen&lcCount
ENDIF
ENDIF
ENDFOR
ENDIF
lnOldQty = ItemQty
lnOldAmt = ItemAmt
REPLACE StyQty  WITH StyQty-lnStyQty ,;
UnitQty WITH IIF(cCatgtyp='F' AND cOprCode=loParentForm.lcFirstOpr,IIF(StyQty=0,0,ItemQty/StyQty),UnitQty) ,;
ItemQty WITH StyQty*UnitQty  ,;
ItemAmt WITH ItemQty*UnitCost
SELECT (loParentForm.lcDetFile)


*=SEEK(EVALUATE(loParentForm.lcBomLine+'.Style')+STR(EVALUATE(loParentForm.lcBomLine+'.LineNo'),6)+;
EVALUATE(loParentForm.lcBomLine+'.cBomTyp')+'1'+EVALUATE(loParentForm.lcBomLine+'.Item')+;
EVALUATE(loParentForm.lcBomLine+'.MfgCode'))

=SEEK(loParentForm.lcInvType+EVALUATE(loParentForm.lcBomLine+'.Style')+;
STR(EVALUATE(loParentForm.lcBomLine+'.LineNo'),6)+;
EVALUATE(loParentForm.lcBomLine+'.cBomTyp')+'1'+;
EVALUATE(loParentForm.lcBomLine+'.cInvTypC')+;
EVALUATE(loParentForm.lcBomLine+'.Item')+;
EVALUATE(loParentForm.lcBomLine+'.MfgCode'))

REPLACE StyQty  WITH StyQty-lnStyQty ,;
UnitQty WITH IIF(cCatgtyp='F' AND cOprCode=loParentForm.lcFirstOpr,IIF(StyQty=0,0,ItemQty/StyQty),UnitQty) ,;
ItemQty WITH StyQty*UnitQty  ,;
ItemAmt WITH ItemQty*UnitCost

*=SEEK(EVALUATE(loParentForm.lcBomLine+'.Style')+;
STR(EVALUATE(loParentForm.lcBomLine+'.LineNo'),6)+;
EVALUATE(loParentForm.lcBomLine+'.cBomTyp')+'2')

=SEEK(loParentForm.lcInvType+EVALUATE(loParentForm.lcBomLine+'.Style')+;
STR(EVALUATE(loParentForm.lcBomLine+'.LineNo'),6)+;
EVALUATE(loParentForm.lcBomLine+'.cBomTyp')+'2')

REPLACE ItemQty WITH ItemQty-lnOldQty+EVALUATE(loParentForm.lcBomLine+'.ItemQty') ,;
ItemAmt WITH ItemAmt-lnOldAmt+EVALUATE(loParentForm.lcBomLine+'.ItemAmt')
lnOldQty = ItemQty
lnOldAmt = ItemAmt

SELECT (loParentForm.lcCtktBom)
LOCATE FOR cIMTyp+CutTkt+Typ+Item+MfgCode+Dyelot = ;
loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
EVALUATE(loParentForm.lcBomline+'.cBomTyp')+;
EVALUATE(loParentForm.lcBomline+'.item')+;
EVALUATE(loParentForm.lcBomline+'.mfgcode')+;
EVALUATE(loParentForm.lcBomline+'.Dyelot')
IF FOUND()
REPLACE Pieces   WITH Pieces-lnStyQty ,;
UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=loParentForm.lcFirstOpr,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
Req_qty  WITH Pieces*UntQty   ,;
Est_Cost WITH Req_qty*UntCost ,;
Req_Qty1 WITH Req_Qty1-lnReq1*UntQty ,;
Req_Qty2 WITH Req_Qty2-lnReq2*UntQty ,;
Req_Qty3 WITH Req_Qty3-lnReq3*UntQty ,;
Req_Qty4 WITH Req_Qty4-lnReq4*UntQty ,;
Req_Qty5 WITH Req_Qty5-lnReq5*UntQty ,;
Req_Qty6 WITH Req_Qty6-lnReq6*UntQty ,;
Req_Qty7 WITH Req_Qty7-lnReq7*UntQty ,;
Req_Qty8 WITH Req_Qty8-lnReq8*UntQty
ENDIF

SELECT (loParentForm.lcTktSheet)
=SEEK(EVALUATE(loParentForm.lcCTKTBOM+'.TYP') + '1' + ;
EVALUATE(loParentForm.lcCTKTBOM+'.cInvType') + ;
EVALUATE(loParentForm.lcCTKTBOM+'.ITEM') + ;
EVALUATE(loParentForm.lcCTKTBOM+'.MFGCODE')+;
EVALUATE(loParentForm.lcCTKTBOM+'.DYELOT'))

REPLACE Pieces   WITH Pieces-lnStyQty ,;
UntQty   WITH IIF(cCatgtyp='F' AND cOprCode=loParentForm.lcFirstOpr,IIF(Pieces=0,0,Req_qty/Pieces),UntQty) ,;
Req_qty  WITH Pieces*UntQty       ,;
Est_Cost WITH Req_qty*UntCost     ,;
Req_Qty1 WITH Req_Qty1-lnReq1*UntQty   ,;
Req_Qty2 WITH Req_Qty2-lnReq2*UntQty   ,;
Req_Qty3 WITH Req_Qty3-lnReq3*UntQty   ,;
Req_Qty4 WITH Req_Qty4-lnReq4*UntQty   ,;
Req_Qty5 WITH Req_Qty5-lnReq5*UntQty   ,;
Req_Qty6 WITH Req_Qty6-lnReq6*UntQty   ,;
Req_Qty7 WITH Req_Qty7-lnReq7*UntQty   ,;
Req_Qty8 WITH Req_Qty8-lnReq8*UntQty
ENDSCAN
GO TOP IN (loParentForm.lcDetFile)
GO TOP IN (loParentForm.lcTktSheet)
ENDIF
ENDSCAN

SELECT (loParentForm.lcPosHdr)
REPLACE Pcs_Act WITH Pcs_Act + MAX(lnRecv-EVALUATE(loParentForm.lcMFGOPRDT+'.nActQty'),0),;
Open  WITH MAX(nStyOrder - (Receive+Cancel+Damage),0)

SELECT (loParentForm.lcMfgOprdt)
REPLACE nActQty WITH lnRecv
IF lnOpen > 0
*INSERT INTO (loParentForm.lcMfgOprdt);
(cImTyp,cTktNo,cOprCode,cLotNo,Item,cDyelot,lInHouse,cContCode,cContName,;
TranCd,dTranDate,nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,;
nLotQty7,nLotQty8,nLotTotQty) VALUES ;
(loParentForm.lcTranType,EVALUATE(loParentForm.lcPosHdr+'.PO'),;
loParentForm.lcFirstOpr,lcLotNo,lcItem,lcItDye,llInHouse,;
lcContCode,lcContName,'4',ldActDate,MAX(lnOpen1,0),MAX(lnOpen2,0),;
MAX(lnOpen3,0),MAX(lnOpen4,0),MAX(lnOpen5,0),MAX(lnOpen6,0),;
MAX(lnOpen7,0),MAX(lnOpen8,0),lnOpen)

INSERT INTO (loParentForm.lcMfgOprdt);
(cImTyp,cTktNo,cOprCode,cLotNo,Item,cDyelot,lInHouse,cContCode,cContName,;
TranCd,dTranDate,nLotQty1,nLotQty2,nLotQty3,nLotQty4,nLotQty5,nLotQty6,;
nLotQty7,nLotQty8,nLotTotQty, cInvType) VALUES ;
(loParentForm.lcTranType,EVALUATE(loParentForm.lcPosHdr+'.PO'),;
loParentForm.lcFirstOpr,lcLotNo,lcItem,lcItDye,llInHouse,;
lcContCode,lcContName,'4',ldActDate,MAX(lnOpen1,0),MAX(lnOpen2,0),;
MAX(lnOpen3,0),MAX(lnOpen4,0),MAX(lnOpen5,0),MAX(lnOpen6,0),;
MAX(lnOpen7,0),MAX(lnOpen8,0),lnOpen, loParentForm.lcInvType)

ENDIF
SELECT (loParentForm.lcOprDet)
=SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
loParentForm.lcFirstOpr+lcLotNo+loParentForm.lcInvType+lcItem+'1'+lcItDye)
REPLACE nActQty WITH lnRecv
IF loParentForm.llUseDyelot .AND. lcNxtDye # lcItDye
=SEEK(loParentForm.lcTranType+EVALUATE(loParentForm.lcPosHdr+'.PO')+;
loParentForm.lcFirstOpr+lcLotNo+loParentForm.lcInvType+lcItem+'1'+lcNxtDye)
ELSE
=SEEK(lcKeyVal)
ENDIF
ENDDO
IF loFormSet.AriaForm1.optActualize.Value = 2
SELECT (loParentForm.lcPosHdr)
REPLACE Status   WITH 'A',;
Act_Date WITH loFormSet.AriaForm1.dtPickerActualDate.Value
ENDIF
UNLOCK

IF loFormSet.AriaForm1.optActualize.Value = 2 .AND. ASCAN(loParentForm.laEvntTrig , PADR('EXPACTCT',10)) <> 0
=gfDoTriger('MFCSSH',PADR('EXPACTCT',10))
ENDIF

SELECT (loParentForm.lcOprDet)
ZAP
SELECT (loParentForm.lcMFGOPRDT)
SCAN
SCATTER MEMVAR

*N039540,1 WSH [Start]
m.cMFGDesc = gfCodDes(m.cOprCode, 'MFGCODE')
*N039540,1 WSH [End]

INSERT INTO (loParentForm.lcOprDet) FROM MEMVAR
ENDSCAN
SELECT (loParentForm.lcOprDet)
DECLARE laTableUpdate[5,2]
laTableUpdate[1,1] = loParentForm.lcCtktBom
laTableUpdate[1,2] = 'CTKTBOM'
laTableUpdate[2,1] = loParentForm.lcBomLine
laTableUpdate[2,2] = 'BOMLINE'
laTableUpdate[3,1] = loParentForm.lcMfgOprdt
laTableUpdate[3,2] = 'MFGOPRDT'
laTableUpdate[4,1] = loParentForm.lcPosHdr
laTableUpdate[4,2] = 'POSHDR'
laTableUpdate[5,1] = loParentForm.lcPosLn
laTableUpdate[5,2] = 'POSLN'
=lfTableUpdate(loParentForm)
GO TOP IN (loParentForm.lcOprDet)
=lfBrowLots(loParentForm)
SELECT (lnAlias)
ENDIF
SET ORDER TO TAG lcCurrTag IN (loParentForm.lcOprDet)
=lfRefreshToolBarButtons(loParentForm)
loParentForm.Refresh
*B608063,1 MMT 04/29/2007 fix bug of not Enable to actualize C/T [End]


