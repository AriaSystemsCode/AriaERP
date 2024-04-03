*!***************************************************************************************************************************
*: Program file  : ALAUTAL.PRG
*: Program desc. : Automatic Allocation  (N120718)
*: Module        : Allocation
*:         System: Aria Apparel System
*:      Developer: Hend Ghanem (HBG)
*!***************************************************************************************************************************
*: Passed Parameters  : None
*!***************************************************************************************************************************
*: Modifications      :
*:   B607674,1 HBG 07/26/2005 Fix bug of too many urgument in case of InList
*:   B607789,1 WAM 09/28/2006 Fix bu when filter to some seasons or divisions
*:   B607815,1 WAM 10/31/2006 Cannot amend allocation min % if Foce allocation setup is set to No
*:   B608118,1 WAM 06/10/2007 Increase he width of the available fields
*:B608316,1 MMT 10/11/2007 convert 3PL Provider Enhancemnt from 27[T20071010.0009]
*!* B609356,1 SMA 07/29/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*!***************************************************************************************************************************

LPARAMETERS lcrequestid, loxmlfilterfilepointer, loxmlallocatefilepointer, loxmlpickticketfilepointer

PRIVATE goxmlfilterfilepointer, goxmlallocatefilepointer, goxmlpickticketfilepointer
goxmlfilterfilepointer     = loxmlfilterfilepointer
goxmlallocatefilepointer   = loxmlallocatefilepointer
goxmlpickticketfilepointer = loxmlpickticketfilepointer

PRIVATE oAriaEnvironment
oAriaEnvironment = CREATEOBJECT("ariamain.AriaEnvironment", lcrequestid)

*MMT
oAriaEnvironment.User_ID =OAriaapplication.User_ID
*MMT

PRIVATE loFormSet_laExcExprs[1], loFormSet_lafilestru[1], loFormSet_lafiltexp[1], loFormSet_laincexprs[1],;
  loFormSet_laindexexp[1], loFormSet_lanormexpr[1], loFormSet_lapikst[1], loFormSet_lascopexpr[1],;
  loFormSet_lashpexp[1], loFormSet_lasizes[1], loFormSet_laslctdesc[1], loFormSet_laslctvals[1],;
  loFormSet_lasortary[4,2], loFormSet_lastring1[1], loFormSet_lastring2[1], loFormSet_lawarehous[1],;
  loFormSet_lawip[1], loFormSet_lccolortlt, loFormSet_lcconfgtlt, loFormSet_lcdivision, loFormSet_lcfabsep,;
  loFormSet_lcfabtlt,loFormSet_lcfree_clr, loFormSet_lcindexexp, loFormSet_lcmajorpic, loFormSet_lcnonmajpi,;
  loFormSet_lcnonmajtl, loFormSet_lcodbc, loFormSet_lcoldindex, loFormSet_lcoptmfile, loFormSet_lcpassword,;
  loFormSet_lcpattlt, loFormSet_lcrelline, loFormSet_lcrpalonot, loFormSet_lcrpexslct, loFormSet_lcrpincwip,;
  loFormSet_lcrppkfwrh, loFormSet_lcrpscpmod, loFormSet_lcrpsepcor, loFormSet_lcseason, loFormSet_lcshpcond,;
  loFormSet_lcsordstat, loFormSet_lcstygroup, loFormSet_lcstylepct, loFormSet_lcstylettl, loFormSet_lcstymajor,;
  loFormSet_lcstyscale, loFormSet_lctmpindex, loFormSet_lctmpordap, loFormSet_lctmpordln, loFormSet_lctmppktk,;
  loFormSet_lctmprelpk, loFormSet_lctmstytag, loFormSet_lcusername, loFormSet_llalwforce, loFormSet_llcallscop;
  loFormSet_llcalwip, loFormSet_llch3stat, loFormSet_llchkaprov, loFormSet_llctrstat1, loFormSet_llctrstat10,;
  loFormSet_llctrstat2, loFormSet_llctrstat3, loFormSet_llctrstat4, loFormSet_llctrstat6, loFormSet_llctrstat7,;
  loFormSet_llctrstat8, loFormSet_llctrstat9, loFormSet_lldyelotst, loFormSet_llexclude, loFormSet_llfabdye,;
  loFormSet_llfirstrun, loFormSet_llforceall, loFormSet_llincord, loFormSet_lllincmplt, loFormSet_llmultware,;
  loFormSet_llmustloop, loFormSet_llopnpack, loFormSet_llopnpikln, loFormSet_llordrrel, loFormSet_llpartalo,;
  loFormSet_llpiknow, loFormSet_llreject, loFormSet_llrpalocat, loFormSet_llrpcond, loFormSet_llrpexlblk,;
  loFormSet_llrpexldye, loFormSet_llrpforalo, loFormSet_llrpgdexcl, loFormSet_llrpgenpik, loFormSet_llrpinchor,;
  loFormSet_llrppikcor, loFormSet_llrppiksep, loFormSet_llrppkhpck, loFormSet_llselallst, loFormSet_llselnonst,;
  loFormSet_llstartslc, loFormSet_llstylrel, loFormSet_lltotavlbl, loFormSet_lluseconfg, loFormSet_llusedyes,;
  loFormSet_lnalorec, loFormSet_lnbrrecno, loFormSet_lnchangalo, loFormSet_lncolorlen, loFormSet_lndellrec,;
  loFormSet_lndummypos, loFormSet_lnfabmajor, loFormSet_lnfabnonmaj, loFormSet_lnfreelen, loFormSet_lnnonmajst,;
  loFormSet_lnrecnumbr, loFormSet_lnrngalias, loFormSet_lnrpcutunt, loFormSet_lnrpgennew, loFormSet_lnrppikcor,;
  loFormSet_lnrppiksep, loFormSet_lnrpsort1, loFormSet_lnrpsort2, loFormSet_lnrpsort3, loFormSet_lnrpsort4,;
  loFormSet_lnselalo, loFormSet_lnselrec, loFormSet_lnstylewid, loFormSet_lnsupmajst, loFormSet_oalobj, ;
  loFormSet_DataSessionId

loFormSet_DataSessionId = SET("Datasession")

PRIVATE loAddUserInfo
loAddUserInfo = CREATEOBJECT('ariamain.AddUserInfo')

*#include "R:\Aria4XP\Screens\AL\alautal.h"
#include R:\Aria4XP\ServerPRGs\AL\alautal.h

*-- declear variavles
PRIVATE lnpanarrln,lcoldscmod

STORE ' ' TO lcoldscmod

** lnpanarrln   pannel array length.
lnpanarrln = 4

=lfinit()

oAriaEnvironment.SaveTables()



*!*************************************************************
*! name      : lfinit
*! developer : hend ghanem
*! date      : 03/03/2004
*! purpose   : init function of the screen
*!*************************************************************
*!
FUNCTION lfinit
  PARAMETERS loFormSet

  PRIVATE llretval
  loFormSet_llreject = .F.

  loFormSet_oalobj = CREATEOBJECT("serverAL")

  *--Get needed setups
  DECLARE laSetups[8,2]
  laSetups[1,1] = 'M_DYELOT'
  laSetups[2,1] = 'M_MATDYE'
  laSetups[3,1] = 'M_WareHouse'
  laSetups[4,1] = 'M_FORCEALO'
  laSetups[5,1] = 'M_TOTAVLBL'
  laSetups[6,1] = 'M_CMPDOLN'
  laSetups[7,1] = 'M_CHKAPROV'
  laSetups[8,1] = 'M_STYCNFG'
  =oAriaEnvironment.setups.getsetting(@lasetups,oAriaEnvironment.ActiveCompanyId)

  ** laSizes      Array to hold the Titles for the sizes columns
  DIMENSION loFormSet_laSizes[8]
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    loFormSet_laSizes[lnI] = 'Size'+lcI
  ENDFOR

  ** llUseDyes    Flag to know if the system use Dyelots
  ** llFabDye     .T. if material use dyelots.
  ** llMultWare   Flag to know if the system is multi warehouse
  ** llChkAprov   Approve Amount Setup
  ** llUseConfg   Flag to know if the style use configuration
  loFormSet_llUseDyes  = (UPPER(ALLTRIM(laSetups[1,2])) = 'Y')
  loFormSet_llFabDye   = ('MA' $ oAriaEnvironment.CompanyInstalledModules) AND (UPPER(ALLTRIM(laSetups[2,2])) = 'Y')
  loFormSet_llMultWare = (UPPER(ALLTRIM(laSetups[3,2])) = 'Y')
  loFormSet_llTotAvlbl = laSetups[5,2]
  loFormSet_llLinCmplt = laSetups[6,2]
  loFormSet_llChkAprov = laSetups[7,2]
  loFormSet_llAlwForce = lfSuppForc(ALLTRIM(laSetups[4,2]))
  loFormSet_llUseConfg = (UPPER(ALLTRIM(laSetups[8,2])) = 'Y')

  ** lnSelRec     Variable to hold the number of selected records
  ** lnSelAlo     Variable to hold the number of selected and allocated records
  ** lnAloRec     Variable to hold the number of allocated records
  ** lnChangAlo   Variable to hold the number of the changed allocated quantity in the current record
  ** lnDellRec    Variable to hold the number of deleted records in the Temp. Order Lines file
  ** lnBrRecNo    Variable to hold the Browse Record number
  ** lnRecNumbr   Variable to hold the number of the undeleted records in the Temp. Order Lines file
  STORE 0 TO loFormSet_lnSelRec,loFormSet_lnSelAlo,loFormSet_lnAloRec,loFormSet_lnchangalo,;
    loFormSet_lnDellRec,loFormSet_lnBrRecNo,loFormSet_lnRecNumbr

  IF loFormSet_llChkAprov
    loFormSet_lcTmpOrdAp = oAriaEnvironment.CURSORS.GetCursorTempName()
  ENDIF

  ** llOpnPikLn   .T. if we open Pick Line file in this program.
  ** llOpnPack    .T. if we open pack header file in this program.
  ** llCh3Stat    Variable to hold the status of the 3rd. child screen in the screen set
  ** llDyelotSt   Variable to hold the status of the Dyelot get field
  ** llIncOrd     Flag to know if the User has increased any of the Ordered quantities
  ** llSelAllSt   Variable to hold the Select All button status
  ** llSelNonSt   Variable to hold the Select None button status
  ** llStartSlc   Only .T. if user start from selection grid.
  ** llPartAlo    .T. if any of selected records have partial allocation.
  ** llStylRel    Flag to know if we need to establish a relationship between the ORDLINE file and the STYLE file
  ** llOrdrRel    Flag to know if we need to establish a relationship between the ORDLINE file and the ORDHDR file
  ** llMustLoop   To loop avoiding nesting error.
  STORE .F. TO loFormSet_llforceall,loFormSet_llCh3Stat ,loFormSet_llDyelotSt,loFormSet_llIncOrd  ,loFormSet_llSelAllSt,;
    loFormSet_llSelNonSt,loFormSet_llSelNonSt,loFormSet_llPartAlo ,loFormSet_llStartSlc,loFormSet_llStylRel ,;
    loFormSet_llOrdrRel ,loFormSet_llMustLoop,loFormSet_llOpnPack ,loFormSet_llOpnPikLn,loFormSet_llCalWip

  ** lcIndexExp   Variable to hold the Index expression to sort the Temp. Order Lines file with
  ** lcOldIndex   Variable hold old index value.
  ** laShpExp     Array to hold selected shipments.
  DIMENSION loFormSet_laShpExp[1]
  STORE " " TO loFormSet_lafiltexp,loFormSet_lcOptmFile,loFormSet_lcIndexExp,loFormSet_lcOldIndex,loFormSet_laShpExp,loFormSet_lcShpCond,;
    loFormSet_laString1,loFormSet_laString2

  ** llCtrStat1	Flag to handle enabling and disabling of Top field
  ** llCtrStat2	Flag to handle enabling and disabling of End field
  ** llCtrStat3	Flag to handle enabling and disabling of Next field
  ** llCtrStat4	Flag to handle enabling and disabling of Previuos field
  ** llCtrStat6	Flag to handle enabling and disabling of print field
  ** llCtrStat7	Flag to handle enabling and disabling of Edit field
  ** llCtrStat8	Flag to handle enabling and disabling of Delete field
  ** llCtrStat9	Flag to handle enabling and disabling of Select field
  ** llCtrStat1	Flag to handle enabling and disabling of browse field
  STORE .F. TO loFormSet_llCtrStat1,loFormSet_llCtrStat2,loFormSet_llCtrStat3,loFormSet_llCtrStat4,;
    loFormSet_llCtrStat6,loFormSet_llCtrStat7,loFormSet_llCtrStat8,loFormSet_llCtrStat9,;
    loFormSet_llCtrStat10 

  loFormSet_lcTmpPkTk  = oAriaEnvironment.CURSORS.GetCursorTempName() && New Session pick ticket file.
  loFormSet_lcRelLine  = oAriaEnvironment.CURSORS.GetCursorTempname()
  loFormSet_lcTmpRelPk = oAriaEnvironment.CURSORS.GetCursorTempname() && Release File and Index.

  ** lcStyleTtl   Variable to hold the Style field Title
  ** lcStylePct   Variable to hold the Style field Mask
  ** lnStyleWid   Variable to hold the width of the Style field
  ** lcStyMajor   Variable to hold Style major title   ...
  ** lcMajorPic   Variable to hold style major picture ...
  ** lnMajorLen   Variable to Hold Major Length.
  ** lcSeason     Variable Hold Style season title.
  ** lcDivision   Variable Hold Style division title.
  ** lcStyGroup   Variable Hold Style group title.
  ** lcFabTlt     Variable Hold Style Fabric title.
  ** lcPatTlt     Variable Hold Style pattern title.
  ** lnMajSeg     Number of major segments.
  ** lcColorTlt   Variable to hold Color segment title.
  ** lcNonMajTl   Variable to hold Non Major title .
  ** lcNonMajPi   Variable to hold Non major picture.
  ** lcFree_Clr   Variable to hold 'C' for Color seg. and 'F' for Free seg. ..
  ** lnNonMajSt   Non major (Color/Free) start filter position.
  ** lnSupMajSt

  *-- Get Item Information
  oGetItemMask = CREATEOBJECT('ariamain.GetItemMask')
  loFormSet_lcStyleTtl = oGetItemMask.DO("HI")
  loFormSet_lcStylePct = oGetItemMask.DO("PI")
  loFormSet_lnStyleWid = LEN(loFormSet_lcStylePct)
  loFormSet_lcStyMajor = oGetItemMask.DO('HM')
  loFormSet_lcMajorPic = oGetItemMask.DO('PM')
  loFormSet_lnMajorLen = LEN(loFormSet_lcMajorPic)
  loFormSet_lcSeason   = loFormSet_lcStyMajor + LANG_AutoAlloc_Season
  loFormSet_lcDivision = loFormSet_lcStyMajor + LANG_AutoAlloc_Division
  loFormSet_lcStyGroup = loFormSet_lcStyMajor + LANG_AutoAlloc_Group
  loFormSet_lcFabTlt   = loFormSet_lcStyMajor + LANG_AutoAlloc_FabricCode
  loFormSet_lcPatTlt   = loFormSet_lcStyMajor + LANG_AutoAlloc_Pattern
  lnMajSeg   = oGetItemMask.DO('SM')  && No. of major segments.
  *-- Compute Free/Color Items in Style code Structure. [Begin]
  DIMENSION laMajSegs[1,1]
  oGetItemMask.DO(@laMajSegs)
  STORE 0  TO loFormSet_lnNonMajSt
  STORE "" TO loFormSet_lcNonMajPi,loFormSet_lcNonMajTl
  STORE .T. TO loFormSet_llCallScop , loFormSet_llFirstRun
  *-- Loop Around Non Major elements.
  FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
    IF laMajSegs[lnI,1] $ 'CF'
      loFormSet_lcFree_Clr = laMajSegs[lnI,1]
      loFormSet_lnNonMajSt = IIF(loFormSet_lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],loFormSet_lnNonMajSt)      && This item hold seg. start position.
      loFormSet_lnSupMajSt = loFormSet_lnNonMajSt
      loFormSet_lcNonMajPi = IIF(EMPTY(loFormSet_lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
        laMajSegs[lnI,3],;
        loFormSet_lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
      loFormSet_lcNonMajTl = IIF(EMPTY(loFormSet_lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
        PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
        loFormSet_lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    ENDIF

    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(loFormSet_lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
      EXIT
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
  ENDFOR    && end Loop Around Non Major elements.

  STORE LEN(loFormSet_lcNonMajPi) TO loFormSet_lnFreeLen , loFormSet_lnColorLen
  loFormSet_lcColorTlt = LANG_AUTOALLOC_COLROTITL1 + ALLTRIM(loFormSet_lcNonMajTl) + LANG_AUTOALLOC_COLROTITL2

  
  *-- Fabric Major length
  lcFabMajor  = oGetItemMask.DO('PM','','0002')
  loFormSet_lnFabMajor   = LEN(lcFabMajor)

  DIMENSION laFabSegs[1,1]
  oGetItemMask.DO(@laFabSegs,'','0002')
  lnFabMajSeg  = oGetItemMask.DO('SM','','0002')  && No. of major segments.
  *-- Fabric Non major length
  loFormSet_lnFabNonMaj  = LEN(laFabSegs[lnFabMajSeg+1,3]  )
  *-- Fabric seperator
  loFormSet_lcFabSep     = laFabSegs[lnFabMajSeg,6]


  ** llPikNow     This flage to detrimined if this program run into this customer.
  loFormSet_llPikNow = .T.

  ** llForceAlc   Flag to know if we are going to Force the allocation

  IF !(loFormSet_llPikNow)
    lnPanArrLn = lnPanArrLn -1
  ENDIF


  STORE '' TO loFormSet_laincexprs , loFormSet_laExcExprs

  ** laWip        Array save wip values and used if user want to include Work Process.
  ** laIndexExp   Array to hold the Valid fields to Sort the Temp. Order Lines file with
  DIMENSION loFormSet_laScopExpr[1,2] , loFormSet_laNormExpr[1,2] , loFormSet_laWIP[9] , loFormSet_laindexexp[6]

  STORE 0 TO loFormSet_laWIP[9]

  ** laSortAry    Variable to hold user select sorted.
  DIMENSION loFormSet_laSortAry[4,2]
  loFormSet_laSortAry = ''
  loFormSet_laSortAry[1,1] = 1
  loFormSet_laSortAry[1,2] = [DTOS(COMPLETE)]
  loFormSet_laSortAry[2,1] = 2
  loFormSet_laSortAry[2,2] = [PRIORITY]
  loFormSet_laSortAry[3,1] = 3
  loFormSet_laSortAry[3,2] = [DTOS(START)]
  loFormSet_laSortAry[4,1] = 4
  loFormSet_laSortAry[4,2] = [ORDER]

  loFormSet_laindexexp[1] = 'DTOS(COMPLETE)'
  loFormSet_laindexexp[2] = 'PRIORITY'
  loFormSet_laindexexp[3] = 'DTOS(START)'
  loFormSet_laindexexp[4] = 'ORDER'
  loFormSet_laindexexp[5] = 'ACCOUNT'
  loFormSet_laindexexp[6] = ''


  ** laSlctDesc   Variable to hold Select By array data.
  ** laSlctVals   Variable to hold Select By array Values.
  lnSubtract = 0
  lnSubtract = IIF('MF' $ oAriaEnvironment.CompanyInstalledModules,lnSubtract, lnSubtract + 1)
  lnSubtract = IIF('PO' $ oAriaEnvironment.CompanyInstalledModules,lnSubtract, lnSubtract + 1)

  DIMENSION loFormSet_laSlctDesc[6 - lnSubtract,1], loFormSet_laSlctVals[6 - lnSubtract,1]
  loFormSet_laSlctDesc[1,1] = 'All'
  loFormSet_laSlctDesc[2,1] = loFormSet_lcStyMajor
  loFormSet_laSlctDesc[3,1] = 'Order'
  loFormSet_laSlctDesc[4,1] = 'Account'
  loFormSet_laSlctVals[1,1] = " "
  loFormSet_laSlctVals[2,1] = "S"
  loFormSet_laSlctVals[3,1] = "O"
  loFormSet_laSlctVals[4,1] = "A"

  DO CASE
    CASE lnSubtract = 0
      loFormSet_laSlctDesc[5,1] = 'Cutting ticket'
      loFormSet_laSlctDesc[6,1] = 'Purchase order'

      loFormSet_laSlctVals[5,1] = 'K'
      loFormSet_laSlctVals[6,1] = 'P'
    CASE lnSubtract = 1
      IF 'MF' $ oAriaEnvironment.CompanyInstalledModules
        loFormSet_laSlctDesc[5,1] = 'Cutting ticket'
        loFormSet_laSlctVals[5,1] = 'K'
      ELSE
        loFormSet_laSlctDesc[5,1] = 'Purchase order'
        loFormSet_laSlctVals[5,1] = 'P'
      ENDIF
  ENDCASE


  DECLARE loFormSet_lafilestru[1]
  DIMENSION loFormSet_lapikst[8]
  STORE .F. TO loFormSet_lapikst

  ** llRpExlDye   Flag to know if the User want to Exclude Styles that is Dyelot yes
  ** llRpExlBlk   Flag to know if the User want to Exclude the Bulk orders
  ** llRpPikSep   Flag to know if the User want to allocate the Order lines records that dose not have a group
  ** llRpPikCor   Flag to know if the User want to allocate the Order lines records that have a group
  ** llRpGdExcl  : Flag is .T. when user press < Exclude > button is selection grid.
  STORE .T. TO loFormSet_llRpExlDye,loFormSet_llRpExlBlk,loFormSet_llRpPikSep,loFormSet_llRpPikCor,loFormSet_llRpGdExcl

  ** llRpIncHOr   Flag to know if we are going to include the orders in hold
  ** llRpAlocat   Flag to know if the User want allocated lines [From the Option grid]
  ** llRpGenPik   Flag to know if the User want to Generate Pick tickets for the allocated lines [From the Option grid]
  ** llExclude    Flag is .T. when user make Exclude option.
  STORE .F. TO loFormSet_llRpIncHOr,loFormSet_llRpAlocat,loFormSet_llRpGenPik,loFormSet_llExclude

  ** lcRpExSlct   Exclude option Select By.
  ** lcRpSepCor   Variable to hold Separates or coordinate group.
  ** lcRpAloNot   Variable hold Allocated / Not Allocated status.
  ** lcRpScpMod   Variable to hold the Select By [From the Option grid]
  STORE " " TO loFormSet_lcRpExSlct,loFormSet_lcRpSepCor,loFormSet_lcRpAloNot,loFormSet_lcRpScpMod,loFormSet_lcSOrdStat

  ** lnDummyPos   Position of dummy variable in filter array.
  STORE 0 TO loFormSet_lnDummyPos,loFormSet_lnRngAlias

  *-- Variable to hold the title of the option of configuration in O.G.
  loFormSet_lcConfgTlt = IIF(loFormSet_llUseConfg ,LANG_AUTOALLOC_OPTNCONFG,LANG_AUTOALLOC_OPTNDYELOT)

  ** lnRpPikSep   Variable to hold the Pick separates Min. %
  ** lnRpPikCor   Variable to hold the Pick coordinate Min. %
  STORE 100 TO loFormSet_lnRpPikSep,loFormSet_lnRpPikCor

  ** lnRpCutUnt   Variable to hold the Cut of units [From the Option grid]
  STORE 0 TO loFormSet_lnRpCutUnt ,loFormSet_lnRpSort1 ,loFormSet_lnRpSort2 ,loFormSet_lnRpSort3,loFormSet_lnRpSort4
  ** lcRpPkFWrh   Variable to hold the Pick from warehouse
  ** lcRpIncWip   new OG Flag is 'A' if include work process , 'S' Open shipment
  STORE " " TO loFormSet_lcRpPkFWrh,loFormSet_lcRpIncWip

  ** llRpForAlo   Flag to know if we are going to Force the allocation [From the Option grid]
  *B607815,1 WAM 10/31/2006 Initialize the variable
  *loFormSet_llRpForAlo  = .F.
  loFormSet_llRpForAlo  = loFormSet_llAlwForce
  *B607815,1 WAM 10/31/2006 (End)

  ** llRpCond     Flag is .T. if we allocate conditionally.
  STORE .F. TO loFormSet_llRpCond

  *-- Varables needs for (Pick O.G.)
  ** lnRpGenNew   Generate New pick ticket variable.
  loFormSet_lnRpGenNew = 2
  ** llRpPkHPck   Add to P/T which have P/L
  loFormSet_llRpPkHPck = .F.

  loFormSet_lcStyScale = " "

  *-- lcTmpOrdLn     Variable to hold a Temp. name for the Temp. Order Lines file
  *-- lcTmpIndex     Variable to hold a Temp. name to create an Index in the Temp. Order Lines file with it
  *-- lcTmStyTag     Name of Temp. index used in collectiong data In Exclude Style case .
  loFormSet_lcTmpOrdLn = oAriaEnvironment.CURSORS.GetCursorTempName()
  loFormSet_lcTmStyTag = oAriaEnvironment.CURSORS.GetCursorTempName()
  loFormSet_lcTmpIndex = oAriaEnvironment.CURSORS.GetCursorTempName()

  =lfBldTolBt(loFormSet)

  *-- Open needed files in the program
  llRetVal = lfOpnFiles(loFormSet)
  =lfCrtFile(loFormSet)
  *-- Get the WareCode
  =lfGetWareH(loFormSet)

  *osa
  *-- Control the displaying of object on the screen
  *=lfCntrlObj(loFormSet)
  *=lfBundBrow(loFormSet)
  *osa


  SELECT (loFormSet_lcTmpOrdLn)
  LOCATE

  =lfvscope()

  RETURN llRetVal


  *!*************************************************************
  *! Name      : lfSuppForc
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Suppress Force allocation. (From logic grid...)
  *!*************************************************************
  *!
FUNCTION lfSuppForc
  LPARAMETERS lcAlwForce

  llAlwForce = .T.
  IF lcAlwForce <> "Y"
    *-- No Force allocation done.
    IF lcAlwForce = "N"
      loFormSet_llRpForAlo = .F.
      llAlwForce = .F.  && Suppress line.
    ELSE  && User Prev.
      *-- Call user defined process.
      llAlwForce = gfUserPriv('AL','ALAUTAL','FORCING')
    ENDIF
  ENDIF
  RETURN llAlwForce
  *-- end of lfSuppForc.

  *osa
  *!*	*!*************************************************************
  *!*	*! Name      : lfvActBar
  *!*	*: Developer : HEND GHANEM (HBG)
  *!*	*: Date      : 11/12/2003
  *!*	*! Purpose   : POPUP _OPTIONPOP SELECTION
  *!*	*!*************************************************************
  *!*	*! Called from : POPUP _OPTIONPOP
  *!*	*!*************************************************************
  *!*	*! Calls       : lfAloScr() , lfRelScr() , lfGenScr , lfvScope()
  *!*	*!*************************************************************
  *!*	*! Passed Parameters : None
  *!*	*!*************************************************************
  *!*	*! Return      : None
  *!*	*!*************************************************************
  *!*	*
  *!*	FUNCTION lfvActBar
  *!*	PARAMETERS loFormSet

  *!*	*E039488,1 MMT 09/12/2005 Convert screen to SQL[Start]
  *!*	SET DATASESSION TO loFormSet_DataSessionID
  *!*	*E039488,1 MMT 09/12/2005 Convert screen to SQL[End]

  *!*	=lfAprov(loFormSet)

  *!*	DO CASE

  *!*	  CASE BAR() = 1      && Scope
  *!*	    =lfvScope(loFormSet)

  *!*	  CASE BAR() = 3      && Allocation logic and Allocate selected records
  *!*	    =lfAlocGrid(loFormSet) AND loFormSet_llRpAlocat AND lfAloScr(loFormSet) AND loFormSet_llRpGenPik AND lfGenScr(loFormSet)

  *!*	  CASE BAR() = 4      && Release selected records
  *!*	    =lfRelScr(loFormSet)

  *!*	  CASE BAR() = 6      && Generate picking tickets options and Generate.
  *!*	    =lfPickGrid(loFormSet) AND loFormSet_llRpGenPik AND lfGenScr(loFormSet)

  *!*	ENDCASE    && End of DO CASE Statment
  *!*	*-- Handle status of tool bar and option menu
  *!*	=lfHandlObj(loFormSet)
  *!*	*E039488,1 MMT 09/12/2005 Convert screen to SQL[Start]
  *!*	SET DATASESSION TO 1
  *!*	*E039488,1 MMT 09/12/2005 Convert screen to SQL[End]
  *osa

  *:***********************************************************************************
  *: Name      : lfAprov
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *: Purpose   : Zap The temp Aprove file to allow Checking Order Approve Amount agian.
  *:***********************************************************************************
  *: Passed Parameters : None
  *:***********************************************************************************
  *: Return      : ....
  *:***********************************************************************************
  *: Example     : = lfChkAprov()
  *:***********************************************************************************
FUNCTION lfAprov
  PARAMETERS loFormSet

  IF loFormSet_llChkAprov
    lcCurrAlis = ALIAS()
    SELECT (loFormSet_lcTmpOrdAp)
    DELETE ALL
    SELECT (lcCurrAlis)
  ENDIF


  *!*************************************************************
  *! Name      : lfvScope
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of push button Scope
  *!*************************************************************
  *! Called from : Control Panel [Push button Scope] , lpShow , Option popup
  *!*************************************************************
  *! Calls       : gfOpGrid() , gfModalGen() , lfAllocate() ,
  *!               lfGenScr() , lfShowGets() , lfRefresh() , lfvpbSel() ,
  *!               lfSelData()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvScope
  PARAMETERS loFormSet

  *osa
  *lcStyle = loFormSet_AriaForm1.cntDetail.keyStyle.value
  lcStyle    = EVALUATE(loFormSet_lcTmpOrdLn+'.Style')
  *osa

  loFormSet_llRpCond = .F.

  DIMENSION loFormSet_laScopExpr[1,2] , loFormSet_laNormExpr[1,2]
  loFormSet_laScopExpr = ''
  loFormSet_laNormExpr = ''
  lcOldScMod = ' '

  STORE .F. TO lcExpr1,lcExpr2
  *-- Call the selection grid
  lcSeason    = loFormSet_lcSeason
  lcDivision  = loFormSet_lcDivision
  lcStyGroup  = loFormSet_lcStyGroup
  lcFabTlt    = loFormSet_lcFabTlt
  lcPatTlt    = loFormSet_lcPatTlt
  lcStyMajor  = loFormSet_lcStyMajor
  lcMajorPic  = loFormSet_lcMajorPic
  lcFree_Clr  = loFormSet_lcFree_Clr
  lcNonMajPi  = loFormSet_lcNonMajPi
  lcNonMajTl  = loFormSet_lcNonMajTl
  lcColorTlt  = loFormSet_lcColorTlt
  lnColorLen  = loFormSet_lnColorLen
  lnNonMajSt  = loFormSet_lnNonMajSt
  lnSupMajSt  = loFormSet_lnSupMajSt
  llRpGdExcl  = loFormSet_llRpGdExcl
  llRpExlDye  = loFormSet_llRpExlDye
  llRpExlBlk  = loFormSet_llRpExlBlk
  llRpIncHOr  = loFormSet_llRpIncHOr
  lcRpExSlct  = loFormSet_lcRpExSlct
  lcRpSepCor  = loFormSet_lcRpSepCor
  lcRpAloNot  = loFormSet_lcRpAloNot
  lcRpScpMod  = loFormSet_lcRpScpMod
  lnDummyPos	= loFormSet_lnDummyPos
  llUseDyes   = loFormSet_llUseDyes
  llUseConfg  = loFormSet_llUseConfg
  llFabDye	= loFormSet_llFabDye
  llAlwForce  = loFormSet_llAlwForce
  llRpPikSep  = loFormSet_llRpPikSep
  llRpPikCor  = loFormSet_llRpPikCor
  llRpGenPik  = loFormSet_llRpGenPik
  lnRngAlias  = loFormSet_lnRngAlias
  lcSOrdStat  = loFormSet_lcSOrdStat
  lcConfgTlt  = loFormSet_lcConfgTlt
  llExclude   = loFormSet_llExclude
  gcCmpModules = oAriaEnvironment.CompanyInstalledModules
  =ACOPY(loFormSet_laSlctDesc,laSlctDesc)
  =ACOPY(loFormSet_laSlctVals,laSlctVals)
  lcOldVal   = " "
  llSelData  = .F.  && No data was selected yet.
  lcPOTlt  = "Purchase order number    "
  lcBrwFld = "PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
    "APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
    "Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'"
  STORE .F. TO llClrPO1 , llClrPO2 , llClrOrd2 , llClrSty2 , llClrOrd1 , llClrSty1 , llClrAcc2 , llClrAcc1
  llDfCnfgVl = !loFormSet_llUseConfg
  lnO_T_S = 0
  =lfRelOpMnu(loFormSet)

  *osa
  lcDataSess = SET("Datasession")
  *lcDataSess = loFormSet_DatasessionID
  *osa

  lcExpr = ""

  RELEASE laOGHdFlt, laOGFxFlt, laOGVrFlt
  DECLARE laOGHdFlt[1, 8]
  DECLARE laOGFxFlt[1, 8]
  DECLARE laOGVrFlt[1, 8]
  *lcExpr = gfOpGrid('ALAUTSLC',.T.,.F.,.F.,.T.,.T.)
  oAriaEnvironment.XML.RestoreFromXml(FILETOSTR(IIF(TYPE('goxmlfilterfilepointer') <>'O' AND TYPE('goxmlfilterfilepointer') = 'C', goxmlfilterfilepointer,goxmlfilterfilepointer.FileName)),.F.)
  lcExpr = lcrpexp

  lnDummyPos = lfItmPos('llDummy')

  *oAriaEnvironment.XML.RestoreFromXml(FILETOSTR(goXmlAllocateFilePointer.FileName))
  lfvSepCor()
  lfmakearrs()

  SET DATASESSION TO lcDataSess
  =lfFillVar(1,loFormSet)
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *=lfBldOpMnu()

  *osa
  *=lfBldOpMnu(loFormSet)
  *osa

  *HBG [End]
  loFormSet_lcRpScpMod =IIF(EMPTY(loFormSet_lcRpScpMod)," ",loFormSet_lcRpScpMod)
  *-- IF lcExpr have a value. which means that the user selecte certain criteria.

  *osa(const)
  * MAH lcexpr = "ordhdr.order='030430'"
  *osa(const)

  IF lcExpr <> ".F."
    IF loFormSet_llFirstRun
      loFormSet_llFirstRun = .F.
      =lfOpnSqlFl(loFormSet)
    ENDIF
    loFormSet_llStartSlc = .T.  && User start from selection grid
    llSelData  = .F.  && No data was selected yet.

    DIMENSION loFormSet_lafiltexp[ALEN(loFormSet_laincexprs,1),ALEN(loFormSet_laincexprs,2)]
    =ACOPY(loFormSet_laincexprs,loFormSet_lafiltexp)
    =lfChngDate(loFormSet)

    loFormSet_llExclude = .F.
    lcOldSlct  = loFormSet_lcRpScpMod
    = lfCreatExp(1,loFormSet) && Create normal expression.

    loFormSet_llPartAlo = .F.
    *-- Collect Data for the selection criteria
    llSelData = lfSelData(loFormSet)  && .T. if we have data.

    loFormSet_lcRpScpMod  = lcOldSlct  && Restore select by.

    loFormSet_llExclude = !loFormSet_llRpGdExcl  && .T. if user want to exclude records.

    *-- if there are records in Temp. file and its exclude option.
    IF llSelData AND loFormSet_llExclude

      *-- Create array have exclude filter expression
      DIMENSION loFormSet_lafiltexp[ALEN(loFormSet_laExcExprs,1),ALEN(loFormSet_laExcExprs,2)]
      =ACOPY(loFormSet_laExcExprs,loFormSet_lafiltexp)

      =lfChngDate(loFormSet)
      *-- Clear the excluded expression from any "DTOS" and "ALLTRIM"
      FOR lnI = 1 TO ALEN(loFormSet_lafiltexp,1)
        IF ('DTOS' $ loFormSet_lafiltexp[lnI,1]) OR ('ALLTRIM' $ loFormSet_lafiltexp[lnI,1])
          lnStartCut = ATC('(',loFormSet_lafiltexp[lnI,1])+1
          lnEndCut   = ATC(')',loFormSet_lafiltexp[lnI,1])
          loFormSet_lafiltexp[lnI,1] = SUBSTR(loFormSet_lafiltexp[lnI,1],lnStartCut,lnEndCut-lnStartCut)
        ENDIF
      ENDFOR

      *-- initialize optimize arrays again.
      DIMENSION loFormSet_laScopExpr[1,2] , loFormSet_laNormExpr[1,2]
      loFormSet_laScopExpr = ''
      loFormSet_laNormExpr = ''

      *-- Select by value will be the excluded values
      lcOldSlct  = loFormSet_lcRpScpMod
      loFormSet_lcRpScpMod = loFormSet_lcRpExSlct

      = lfCreatExp(2,loFormSet)  && Create exclude expression.
      llSelData  = lfSelData(loFormSet)  && .T. if there if temp. file still have records.
      loFormSet_lcRpScpMod = lcOldSlct    && Restore select by value.

    ENDIF  && end if there are records in Temp. file and its exclude option.
  ENDIF



  *IF if the User has selected Cancel from the Option grid or there is no records for the selection criteria
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	oAriaEnvironment.oToolBar.ChangeButtonStatus('pbScop','ENABLED')

  *osa
  *loFormSet_oToolBar.ChangeButtonStatus('pbScop','ENABLED')
  *osa

  *HBG [End]

  IF lcExpr = '.F.' .OR. !llSelData
    IF !EOF(loFormSet_lcTmpOrdln) AND lcExpr = '.F.'
      loFormSet_activemode = 'V'        && The Array that hold the Screen mode
      *--=loFormSet_changeMode('V')
    ELSE
      loFormSet_activemode = 'S'        && The Array that hold the Screen mode
      *--=loFormSet_changeMode('S')
    ENDIF
    *IF There is no records for the selection criteria
    IF lcExpr <> '.F.'
      IF !EMPTY(lcStyle)
        = lfRefScr(loFormSet)
      ENDIF
      *** Message : "There are no records to display...!"
      ***           "              < Ok >               "
      * MAH
      *=gfModalGen("TRM00052B00000","DIALOG")
      gfModalGen("TRM00052B00000","DIALOG",.f.,.f.,"All lines already allocated.")
      * MAH
      *B607789,1 WAM 09/28/2006 Hide progress bar
      IF TYPE('loFormSet_oPross') = 'O'
        loFormSet_oPross.HIDE()
      ENDIF
      *B607789,1 WAM 09/28/2006 (End)
    ENDIF    && End of IF
    IF EOF(loFormSet_lcTmpOrdln)
      *loFormSet_AriaForm1.grdOrders.cmdSelect.Enabled     = .F.
      *loFormSet_AriaForm1.grdOrders.cmdSelectAll.Enabled  = .F.
      *loFormSet_AriaForm1.grdOrders.cmdSelectNone.Enabled = .F.
      *loFormSet_AriaForm1.grdOrders.cmdInvert.Enabled = .F.
      *HBG 1/24/2005 Modify code to apply the new interface [Begin]
      *!*	    oAriaEnvironment.oToolBar.ChangeButtonStatus('pbAlo','DISABLED')
      *!*	    oAriaEnvironment.oToolBar.ChangeButtonStatus('pbRel','DISABLED')
      *!*	    oAriaEnvironment.oToolBar.ChangeButtonStatus('pbGen','DISABLED')
      *loFormSet_oToolBar.ChangeButtonStatus('pbAlo','DISABLED')
      *loFormSet_oToolBar.ChangeButtonStatus('pbRel','DISABLED')
      *loFormSet_oToolBar.ChangeButtonStatus('pbGen','DISABLED')
      *HBG [End]
    ENDIF
  ELSE    && Else
    loFormSet_llStartSlc = .F.

    = lfAlocGrid(loFormSet)  && Next two option grids if user want.

    loFormSet_llStartSlc = .F.
    IF loFormSet_llRpAlocat
      loFormSet_lnAloRec = 0
      = lfAllocate(1,.F.,loFormSet)
    ELSE
      *-- Get the # of selected and allocated records if no records selected and allocated
      *-- and under generating pick ticket flag condition
      IF loFormSet_llRpGenPik AND loFormSet_lnSelAlo = 0
        loFormSet_lnSelAlo = RECCOUNT(loFormSet_lcTmpOrdLn)
      ENDIF
    ENDIF
    *IF Generate Pick tickets [From the Option grid] is Yes and there is some
    *allocated records

    * MAH Set record count of allocated lines
    IF loFormSet_llRpGenPik AND loFormSet_lnSelAlo = 0
      loFormSet_lnSelAlo = RECCOUNT(loFormSet_lcTmpOrdLn)
    ENDIF
    * MAH

    IF loFormSet_llRpGenPik .AND. loFormSet_lnSelAlo > 0
      =lfGenScr(loFormSet)
      *IF There is no records to be Browsed [IF we have Generated Pick tickets
      *for all the records in the temp. Order lines file]
      IF RECCOUNT(loFormSet_lcTmpOrdLn) = loFormSet_lnDellRec
        RETURN
      ENDIF    && End of IF
    ENDIF    && End of IF

    *osa
    *loFormSet_activemode = 'V'        && The Array that hold the Screen mode
    *=loFormSet_changeMode('V')
    *loFormSet_llCtrStat6 = .T.                && Print Button

    GO TOP IN (loFormSet_lcTmpOrdLn)

    SET ORDER TO TAG STYLE IN STYLE
    SELECT (loFormSet_lcTmpOrdLn)
    SET ORDER TO TAG (loFormSet_lcTmpOrdLn)

    *IF The system use Dyelots
    IF loFormSet_llUseDyes
      SET RELATION TO STYLE + cWareCode + DyeLot INTO STYDYE
    ELSE    && Else
      SET RELATION TO STYLE + cWareCode + SPACE(10) INTO STYDYE
    ENDIF    && End of IF
    GO TOP
    loFormSet_lnBrRecNo  = RECNO()
    =SEEK(STYLE , 'STYLE')
    =SEEK('S' + STYLE.SCALE , 'SCALE')

    *osa
    *=lfBundFlds(loFormSet)
    *osa

    *IF the current record is selected
    IF lnSel = 1
      *osa
      =lfShowGets(.T.,loFormSet)
      *osa
    ELSE
      loFormSet_llCh3Stat = .F.
      loFormSet_laPikSt   = .F.

      *osa
      *=lfDisblGts(loFormSet)
      *osa

    ENDIF    && End of IF

    *osa
    *-- Handle status of tool bar and option menu
    *=lfHandlObj(loFormSet)
    *osa

    *osa
    *SELECT POSHDR1
    *IF lfUpdatSQL(loFormSet,'POSHDR1','cBusDocu,cStyType,PO')
    *=TABLEUPDATE(.T.,.T.)
    *ELSE
    *=TABLEREVERT(.T.)
    *ENDIF
    *osa

  ENDIF    && End of IF
  *-- end of lfvScope.


  *!*************************************************************
  *! Name      : lfAlocGrid
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Call allocation grid and prepair indecies
  *!*************************************************************
  *! Called from : Menu,lfvScope
  *!*************************************************************
  *! Calls       : gfModalGen,gfOpGrid,lfPickGrid.
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfAlocGrid()
  *!*************************************************************
  *
FUNCTION lfAlocGrid
  PARAMETERS loFormSet

  loFormSet_lcOldIndex = IIF(EMPTY(loFormSet_lcOldIndex),lfEvalIndx(),loFormSet_lcOldIndex)
  loFormSet_lcIndexExp = IIF(EMPTY(loFormSet_lcIndexExp),loFormSet_lcOldIndex,loFormSet_lcIndexExp)

  loFormSet_lcRpIncWip = IIF(loFormSet_llTotAvlbl AND (loFormSet_lcRpScpMod $ 'KP' OR EMPTY(loFormSet_lcRpScpMod)),'A','N')

  IF loFormSet_llPartAlo
    *-- Do you wish to Allocate. <Yes>  <No>

    *osa
    *IF gfModalGen('QRM44052B42002','Dialog') = 1
    IF .T.
      *osa

      *-- call allocation logic grid.
      lnNonMajSt  = loFormSet_lnNonMajSt
      lnColorLen  = loFormSet_lnColorLen
      lnRpPikSep  = loFormSet_lnRpPikSep
      lnRpPikCor  = loFormSet_lnRpPikCor
      lnRpCutUnt  = loFormSet_lnRpCutUnt
      lnRpSort1   = loFormSet_lnRpSort1
      lnRpSort2   = loFormSet_lnRpSort2
      lnRpSort3   = loFormSet_lnRpSort3
      lnRpSort4   = loFormSet_lnRpSort4
      lcRpPkFWrh  = loFormSet_lcRpPkFWrh
      lcRpIncWip  = loFormSet_lcRpIncWip
      llRpForAlo  = loFormSet_llRpForAlo
      llRpCond    = loFormSet_llRpCond
      lcFree_Clr  = loFormSet_lcFree_Clr
      lcSeason    = loFormSet_lcSeason
      lcDivision  = loFormSet_lcDivision
      lcStyGroup  = loFormSet_lcStyGroup
      lcFabTlt    = loFormSet_lcFabTlt
      lcPatTlt    = loFormSet_lcPatTlt
      lcColorTlt  = loFormSet_lcColorTlt
      lcRpSepCor  = loFormSet_lcRpSepCor
      llUseDyes   = loFormSet_llUseDyes
      llFabDye	= loFormSet_llFabDye
      llAlwForce  = loFormSet_llAlwForce
      llRpGenPik  = loFormSet_llRpGenPik
      lnRngAlias  = loFormSet_lnRngAlias
      lcSOrdStat  = loFormSet_lcSOrdStat
      lcRpAloNot  = loFormSet_lcRpAloNot
      llRpPikCor  = loFormSet_llRpPikCor
      llRpPikSep  = loFormSet_llRpPikSep
      llUseConfg  = loFormSet_llUseConfg
      lcPkWOldVl  = ""
      DIMENSION laSortAry[4,2]
      =ACOPY(loFormSet_laSortAry,laSortAry)
      gcCmpModules = oAriaEnvironment.CompanyInstalledModules
      lcOldVal   = ""
      =lfRelOpMnu(loFormSet)

      *osa
      *lcDataSess = loFormSet_DatasessionID
      lcDataSess = SET("Datasession")
      *lcExpr1 = gfOpGrid('ALAUTALC' , .T.,.F.,.F.,.T.,.T.)  && Allocation logic grid.
      RELEASE laOGHdFlt, laOGFxFlt, laOGVrFlt
      DECLARE laOGHdFlt[1, 8]
      DECLARE laOGFxFlt[1, 8]
      DECLARE laOGVrFlt[1, 8]
      oAriaEnvironment.XML.RestoreFromXml(FILETOSTR(IIF(TYPE('goXmlAllocateFilePointer') <>'O' AND TYPE('goXmlAllocateFilePointer') = 'C', goXmlAllocateFilePointer,goXmlAllocateFilePointer.FileName)),.F.)
      *oAriaEnvironment.XML.RestoreFromXml(FILETOSTR(goXmlAllocateFilePointer.FileName))
      lcExpr1 = lcrpExp
      *osa

      SET DATASESSION TO lcDataSess
      *HBG 1/24/2005 Modify code to apply the new interface [Begin]
      *=lfBldOpMnu()

      *osa
      *=lfBldOpMnu(loFormSet)
      *osa

      *HBG [End]
      =lfFillVar(2,loFormSet)
      FOR lnCntr = 1 TO 4
        lcArrayCnt = STR(lnCntr,1)
        loFormSet_laSortAry[lnCntr,1] = lnRpSort&lcArrayCnt
        lnRpSort1 = 1
        lnRpSort2 = 2
        lnRpSort3 = 3
        lnRpSort4 = 4
        loFormSet_laSortAry[lnCntr,2] = loFormSet_laindexexp[lnRpSort&lcArrayCnt]
      ENDFOR

      *-- if user press run and call allocation grid from menu itself and
      *-- change Sort1/Sort2/Sort3 .
      IF lcExpr1 <> ".F."

        SELECT (loFormSet_lcTmpOrdLn)
        REPLACE ALL cReason WITH ""

        *-- IF Include open Shipments , get the selected shipments.
        IF loFormSet_lcRpIncWip = 'S'
          IF lfopensql(loFormSet,'SHPMTHDR','SHPMTHDR1',loFormSet_lcShpCond)
            SELECT SHPMTHDR1
            lnI = 0
            SCAN
              lnI = lnI + 1
              DIMENSION loFormSet_laShpExp[lnI]
              loFormSet_laShpExp[lnI] = SHPMTHDR1.ShipNo
            ENDSCAN
          ENDIF
        ENDIF

        loFormSet_lcIndexExp = lfEvalIndx()
        lnSaveRec = RECNO(loFormSet_lcTmpOrdLn)
        GO TOP IN (loFormSet_lcTmpOrdLn)
        IF !(loFormSet_llStartSlc) AND !EOF(loFormSet_lcTmpOrdLn) AND !(loFormSet_lcIndexExp == loFormSet_lcOldIndex)
          SELECT (loFormSet_lcTmpOrdLn)
          REPLACE ALL cSortField WITH EVALUATE(loFormSet_lcIndexExp)
          GO TOP
        ENDIF

        IF BETWEEN(lnSaveRec,1,RECCOUNT(loFormSet_lcTmpOrdLn))
          GO lnSaveRec IN (loFormSet_lcTmpOrdLn)
        ENDIF

        loFormSet_lcOldIndex = loFormSet_lcIndexExp
        loFormSet_llRpAlocat = .T.  && user want to allocate.
      ELSE
        loFormSet_llRpAlocat = .F.  && Disable Allocate Flag.
      ENDIF  && end if user press OG <Run> .
    ELSE  && User response message with <No>
      loFormSet_llRpAlocat = .F.
    ENDIF
  ELSE
    loFormSet_llRpAlocat = .T.
  ENDIF


  loFormSet_llPartAlo = .T.

  *osa
  *IF TYPE("loFormSet_laPanelObj[4,1]") = "C"
  IF loFormSet_llRpAlocat OR (loFormSet_lcRpAloNot # 'N')
    = lfPickGrid(loFormSet) && Call picket ticket quetion.
  ENDIF
  *ELSE  && else standard program
  *loFormSet_llRpGenPik = .F.
  *ENDIF
  *osa

  *-- end of lfAlocGrid.

  *!*************************************************************
  *! Name      : lfRelScr
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of push button Release
  *!*************************************************************
  *! Called from : Control Panel [Push button Release] , Option popup
  *!*************************************************************
  *! Calls       : gfThermo() , lfRelQty() , lfShowGets() , lfRefresh()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfRelScr
  PARAMETERS loFormSet

  PRIVATE lcRelStyle , lnTotRec , lnCurent ,;
    lnPrepRec

  *IF Statment to make sure that the user want to release the selected records
  *** Message : "Are you sure you want to release the allocation from the selected order lines "
  ***           "                   < Yes >           < No >                    "
  IF ISNULL(goXmlAllocateFilePointer) = .F. && .gfModalGen("TRM44007B00006","DIALOG") = 1
    SET ORDER TO TAG STYDYE IN STYDYE
    SET ORDER TO TAG STYLE IN STYLE
    SET ORDER TO TAG ORDLINE IN ORDLINE
    SET ORDER TO TAG ORDHDR IN ORDHDR
    SELECT (loFormSet_lcTmpOrdLn)
    SET ORDER TO TAG (loFormSet_lcTmpOrdLn)
    SET RELATION TO

    loFormSet_lnBrRecNo = RECNO(loFormSet_lcTmpOrdLn)
    COUNT FOR lnSel = 1 AND TOTPIK <> 0 TO lnTotRec  && Varible to hold the Total count to be done for the thermometer
    lnCurent = 0                   && Varible to hold the current count to be done for the thermometer

    *IF This is not an incompleted session
    lnRecCount = RECCOUNT() - loFormSet_lnDellRec      && Varible to hold the Total count to be done for the thermometer
    lnPrepRec = 0                            && Varible to hold the current count to be done for the thermometer
    IF TYPE('loFormSet_oPross') = 'O'
      loFormSet_oPross.lblFirstLabel.CAPTION = LANG_AutoAlloc_LableProgress
      loFormSet_oPross.TotalProgress = lnRecCount
      *HBG 1/24/2005 Modify code to apply the new interface [Begin]
      loFormSet_oPross.AUTOCENTER = .T.
      *HBG [End]
      loFormSet_oPross.SHOW()
    ENDIF
    *SCAN Loop to scan the temp. Order lines file
    SCAN
      lnPrepRec = lnPrepRec + 1
      REPLACE nProcNo WITH 0
      
      IF TYPE('loFormSet_oPross') = 'O'
        loFormSet_oPross.CurrentProgress(lnPrepRec)
      ENDIF
    ENDSCAN    && End of SCAN Loop


    SELECT (loFormSet_lcTmpOrdLn)
    *SCAN Loop to scan the temp. Order lines file FOR the selected
    *and allocated records and for nProcNo < 12
    IF TYPE('loFormSet_oPross') = 'O'
      loFormSet_oPross.lblFirstLabel.CAPTION = LANG_AutoAlloc_LableReleas
      loFormSet_oPross.TotalProgress = lnTotRec
      *HBG 1/24/2005 Modify code to apply the new interface [Begin]
      loFormSet_oPross.AUTOCENTER = .T.
      *HBG [End]

      loFormSet_oPross.SHOW()
    ENDIF
    SCAN FOR lnSel = 1 .AND. TotPik > 0 .AND. nProcNo < 14
      lcRelStyle = STYLE          && Variable to hold the Style
      =lfRelQty(loFormSet)
      loFormSet_lnSelAlo = loFormSet_lnSelAlo - 1
      loFormSet_lnAloRec = loFormSet_lnAloRec - 1
      lnCurent = lnCurent + 1
      IF TYPE('loFormSet_oPross') = 'O'
        loFormSet_oPross.lblSecondLabel.CAPTION = ORDER + '/' + lcRelStyle
        loFormSet_oPross.CurrentProgress(lnCurent)
      ENDIF
    ENDSCAN    && End of SCAN Loop

    SELECT (loFormSet_lcTmpOrdLn)

    *IF The system use Dyelots
    IF loFormSet_llUseDyes
      SET RELATION TO STYLE + cWareCode + DyeLot INTO STYDYE
    ELSE    && Else
      SET RELATION TO STYLE + cWareCode + SPACE(10) INTO STYDYE
    ENDIF    && End of IF
    GO loFormSet_lnBrRecNo

    *-- Handle status of tool bar and option menu
    *=lfHandlObj(loFormSet)

    =SEEK(STYLE , 'STYLE')
    =SEEK('S' + STYLE.SCALE , 'SCALE')
    *=lfBundFlds(loFormSet)

    *IF the current record is selected
    IF lnSel = 1
      *osa
      =lfShowGets(.T.,loFormSet)
      *osa
    ELSE
      loFormSet_llCh3Stat = .F.
      loFormSet_laPikSt   = .F.
      *=lfDisblGts(loFormSet)
    ENDIF    && End of IF

  ENDIF    && End of IF



  *!*************************************************************
  *! Name      : lfPickGrid
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Call allocation grid and prepair indecies
  *!*************************************************************
  *! Called from : Menu,lfAlocGrid
  *!*************************************************************
  *! Calls       : gfModalGen,gfOpGrid.
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfPickGrid()
  *!*************************************************************
FUNCTION lfPickGrid
  PARAMETERS loFormSet

  *-- Do you wish to Generate pick ticket. <Yes>  <No>

  *osa
  *IF loFormSet_llPikNow .AND. gfModalGen('QRM44053B42002','Dialog') = 1
  IF ISNULL(goXmlPickTicketFilePointer) == .F.
    lnRpGenNew = loFormSet_lnRpGenNew
    llRpPkHPck = loFormSet_llRpPkHPck
    llRpGenPik = loFormSet_llRpGenPik
    lcRpAloNot = loFormSet_lcRpAloNot
    gcCmpModules = oAriaEnvironment.CompanyInstalledModules
    lcOldVal   = ""
    =lfRelOpMnu(loFormSet)

    *osa
    *lcDataSess = loFormSet_DatasessionID
    lcDataSess = SET("Datasession")
    *lcExpr2 = gfOpGrid('ALAUTPIK' , .T.,.F.,.F.,.T.,.T.)   && Pick Ticket grid.
    RELEASE laOGHdFlt, laOGFxFlt, laOGVrFlt
    DECLARE laOGHdFlt[1, 8]
    DECLARE laOGFxFlt[1, 8]
    DECLARE laOGVrFlt[1, 8]
    oAriaEnvironment.XML.RestoreFromXml(FILETOSTR(IIF(TYPE('goxmlpickticketfilepointer') <>'O' AND TYPE('goxmlpickticketfilepointer') = 'C', goxmlpickticketfilepointer ,goxmlpickticketfilepointer.FileName)),.F.)
    lcExpr2 = lcrpExp
    lfmakearrs()
    *osa

    SET DATASESSION TO lcDataSess
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    *=lfBldOpMnu()

    *osa
    *=lfBldOpMnu(loFormSet)
    *osa

    *HBG [End]
    =lfFillVar(3,loFormSet)
    IF lcExpr2 <> ".F."
      loFormSet_llRpGenPik = .T.
    ELSE
      loFormSet_llRpGenPik = .F.
    ENDIF
  ELSE  && User response message with <No>
    loFormSet_llRpGenPik = .F.
  ENDIF
  *-- end of lfPickGrid.

  *!*************************************************************
  *! Name      : lfBldTolBt
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Add new buttons to the tool bar
  *!*************************************************************
  *! Called from : Menu,lfAlocGrid
  *!*************************************************************
  *! Calls       : gfModalGen,gfOpGrid.
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfBldTolBt()
  *!*************************************************************
FUNCTION lfBldTolBt
  PARAMETERS loFormSet

  DECLARE loFormSet_laPanelObj[lnPanArrLn,6]
  STORE '' TO loFormSet_laPanelObj
  loFormSet_laPanelObj[1,1] = 'pbScop'
  loFormSet_laPanelObj[1,2] = oAriaEnvironment.BitMapHome+"SCOPE.bmp"
  loFormSet_laPanelObj[1,3] = 'mTolScope'
  loFormSet_laPanelObj[1,4] = LANG_AutoAlloc_Scope
  loFormSet_laPanelObj[1,5] = LANG_AutoAlloc_Scope
  loFormSet_laPanelObj[1,6] = 'V'

  loFormSet_laPanelObj[2,1] = 'pbAlo'
  loFormSet_laPanelObj[2,2] = oAriaEnvironment.BitMapHome+"PICK.bmp"
  loFormSet_laPanelObj[2,3] = 'mTolAlocGrid'
  loFormSet_laPanelObj[2,4] = LANG_AutoAlloc_Allocate
  loFormSet_laPanelObj[2,5] = LANG_AutoAlloc_Allocate
  loFormSet_laPanelObj[2,6] = 'V'

  loFormSet_laPanelObj[3,1] = 'pbRel'
  loFormSet_laPanelObj[3,2] = oAriaEnvironment.BitMapHome+"RELEASE2.bmp"
  loFormSet_laPanelObj[3,3] = 'mTolRelScr'
  loFormSet_laPanelObj[3,4] = LANG_AutoAlloc_release
  loFormSet_laPanelObj[3,5] = LANG_AutoAlloc_release
  loFormSet_laPanelObj[3,6] = 'V'

  IF TYPE("loFormSet_laPanelObj[4,1]") = "C"
    loFormSet_laPanelObj[4,1] = 'pbGen'
    loFormSet_laPanelObj[4,2] = oAriaEnvironment.BitMapHome+"GENERAT.BMP"
    loFormSet_laPanelObj[4,3] = 'mTolPickGrid'
    loFormSet_laPanelObj[4,4] = LANG_AutoAlloc_GenPick
    loFormSet_laPanelObj[4,5] = LANG_AutoAlloc_GenPick
    loFormSet_laPanelObj[4,6] = 'V'
  ENDIF

*!*	  *!*************************************************************
*!*	  *! Name      : lfActPad
*!*	  *: Developer : HEND GHANEM (HBG)
*!*	  *: Date      : 11/12/2003
*!*	  *! Purpose   : Bulid a new menu pad [Options]
*!*	  *!*************************************************************
*!*	  *! Called from : ALAUTAL.PRG
*!*	  *!*************************************************************
*!*	  *! Calls       : None
*!*	  *!*************************************************************
*!*	  *! Passed Parameters : None
*!*	  *!*************************************************************
*!*	  *! Return      : None
*!*	  *!*************************************************************
*!*	FUNCTION lfActPad
*!*	  PARAMETERS loFormSet

*!*	  IF TYPE("_SCREEN.ActiveForm.parent") = 'O' AND !ISNULL(_SCREEN.ACTIVEFORM.PARENT) AND;
*!*	      TYPE("_SCREEN.ActiveForm.parent.lnSelRec") <> 'U' AND !(TYPE("glOptionGridIsRuning") <> 'U' AND glOptionGridIsRuning)
*!*	    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
*!*	    *=lfBldOpMnu()
*!*	    =lfBldOpMnu(loFormSet)
*!*	    *HBG [End]
*!*	    *-- IF it is first time run current session , so it is not incompleted sesion.
*!*	    *-- We are going to call the Option grid
*!*	    IF _SCREEN.ACTIVEFORM.PARENT.llCallScop
*!*	      loFormSet_opross = CREATEOBJECT('ariamain.ariaprogressbar')
*!*	      loFormSet_Refresh()
*!*	      _SCREEN.ACTIVEFORM.PARENT.llCallScop = .F.
*!*	      =lfvScope(_SCREEN.ACTIVEFORM.PARENT)
*!*	    ENDIF    && End of IF
*!*	  ENDIF

  *osa
  *!*	*!*************************************************************
  *!*	*! Name      : lfBldOpMnu
  *!*	*: Developer : HEND GHANEM (HBG)
  *!*	*: Date      : 11/12/2003
  *!*	*! Purpose   : Build the menu pad [Options]
  *!*	*!*************************************************************
  *!*	*! Called from : ALAUTAL.PRG
  *!*	*!*************************************************************
  *!*	*! Calls       : None
  *!*	*!*************************************************************
  *!*	*! Passed Parameters : None
  *!*	*!*************************************************************
  *!*	*! Return      : None
  *!*	*!*************************************************************
  *!*	FUNCTION lfBldOpMnu
  *!*	*HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	PARAMETERS loFormSet
  *!*	IF TYPE('_SCREEN.ActiveForm.parent') = 'O'
  *!*	  *DEFINE PAD _Option OF _MSYSMENU PROMPT LANG_AutoAlloc_Option KEY ALT+P , ' ' SKIP FOR (TYPE("glOptionGridIsRuning") <> 'U' AND glOptionGridIsRuning)
  *!*	  *ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  *!*	  DEFINE PAD _Option OF (_SCREEN.ActiveForm.parent.chostformname) PROMPT LANG_AutoAlloc_Option KEY ALT+P , ' ' SKIP FOR (TYPE("glOptionGridIsRuning") <> 'U' AND glOptionGridIsRuning)
  *!*	  ON PAD _Option OF (_SCREEN.ActiveForm.parent.chostformname) ACTIVATE POPUP _OPTIONPOP
  *!*	  lcHostFormName = '[' + loFormSet_cHostFormName + ']'
  *!*	*HBG [End]
  *!*	  DEFINE POPUP _OPTIONPOP MARGIN SHADOW

  *!*	  DEFINE BAR 1 OF _OPTIONPOP PROMPT LANG_AutoAlloc_ScopeOpt
  *!*	  DEFINE BAR 2 OF _OPTIONPOP PROMPT LANG_AutoAlloc_SepertOpt      SKIP FOR .T.
  *!*	  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	  *!*	  DEFINE BAR 3 OF _OPTIONPOP PROMPT LANG_AutoAlloc_AllocateOpt    SKIP FOR (_SCREEN.ActiveForm.parent.lnSelRec = 0)
  *!*	  *!*	  DEFINE BAR 4 OF _OPTIONPOP PROMPT LANG_AutoAlloc_releaseOpt     SKIP FOR (_SCREEN.ActiveForm.parent.lnSelAlo = 0)
  *!*	  DEFINE BAR 3 OF _OPTIONPOP PROMPT LANG_AutoAlloc_AllocateOpt    SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_SCREEN.ActiveForm.parent.lnSelRec = 0)
  *!*	  DEFINE BAR 4 OF _OPTIONPOP PROMPT LANG_AutoAlloc_releaseOpt     SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_SCREEN.ActiveForm.parent.lnSelAlo = 0)
  *!*	  *HBG [End]

  *!*	  IF TYPE("_SCREEN.ActiveForm.parent.laPanelObj[4,1]") = "C"
  *!*	    DEFINE BAR 5 OF _OPTIONPOP PROMPT LANG_AutoAlloc_SepertOpt   SKIP FOR .T.
  *!*	    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	    *DEFINE BAR 6 OF _OPTIONPOP PROMPT LANG_AutoAlloc_GenerateOp  SKIP FOR (_SCREEN.ActiveForm.parent.lnSelAlo = 0)
  *!*	    DEFINE BAR 6 OF _OPTIONPOP PROMPT LANG_AutoAlloc_GenerateOp  SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_SCREEN.ActiveForm.parent.lnSelAlo = 0)
  *!*	    *HBG [End]
  *!*	  ENDIF

  *!*	  ON SELECTION POPUP _OPTIONPOP DO lfvActBar WITH _SCREEN.ActiveForm.parent
  *!*	*HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	ENDIF
  *!*	*HBG [End]
  *osa

  *!*************************************************************
  *! Name      : lfRelOpMnu
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Release the menu pad [Options]
  *!*************************************************************
  *! Called from : ALAUTAL.PRG
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
FUNCTION lfRelOpMnu
  PARAMETERS loFormSet

  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *IF TYPE('loFormSet') = 'O'
  *RELEASE PAD _Option OF _MSYSMENU
  IF TYPE('loFormSet') = 'O'
    RELEASE PAD _Option OF (loFormSet_chostformname)
    *HBG [End]
  ENDIF

  *!*************************************************************
  *! Name      : lfCrtFile
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Create temp file
  *!*************************************************************
  *! Called from : ALAUTAL.PRG
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
FUNCTION lfCrtFile
  PARAMETERS loFormSet

  SELECT ORDLINE
  =AFIELDS(loFormSet_lafilestru)
  lnFileStru = ALEN(loFormSet_lafilestru,1)        && Varible to hold the number of fields in the ORDLINE file
  lnAddToArr = IIF('MF' $ oAriaEnvironment.CompanyInstalledModules  .OR. 'PO' $ oAriaEnvironment.CompanyInstalledModules ,12,10)
  lnAddToArr = IIF(loFormSet_llUseDyes,lnAddToArr+1,lnAddToArr)
  DIMENSION loFormSet_lafilestru[lnFileStru+lnAddToArr,ALEN(loFormSet_lafilestru,2)]

  loFormSet_lafilestru[lnFileStru+1,1] = 'PRIORITY'
  loFormSet_lafilestru[lnFileStru+1,2] = 'C'
  loFormSet_lafilestru[lnFileStru+1,3] = 3
  loFormSet_lafilestru[lnFileStru+1,4] = 0

  loFormSet_lafilestru[lnFileStru+2,1] = 'LnSEL'
  loFormSet_lafilestru[lnFileStru+2,2] = 'N'
  loFormSet_lafilestru[lnFileStru+2,3] = 1
  loFormSet_lafilestru[lnFileStru+2,4] = 0

  loFormSet_lafilestru[lnFileStru+3,1] = 'NPROCNO'
  loFormSet_lafilestru[lnFileStru+3,2] = 'N'
  loFormSet_lafilestru[lnFileStru+3,3] = 2
  loFormSet_lafilestru[lnFileStru+3,4] = 0

  loFormSet_lafilestru[lnFileStru+4,1] = 'ENTERED'
  loFormSet_lafilestru[lnFileStru+4,2] = 'D'
  loFormSet_lafilestru[lnFileStru+4,3] = 8
  loFormSet_lafilestru[lnFileStru+4,4] = 0

  loFormSet_lafilestru[lnFileStru+5,1] = 'CDIVISION'
  loFormSet_lafilestru[lnFileStru+5,2] = 'C'
  loFormSet_lafilestru[lnFileStru+5,3] = 6
  loFormSet_lafilestru[lnFileStru+5,4] = 0

  loFormSet_lafilestru[lnFileStru+6,1] = 'CSTYGROUP'
  loFormSet_lafilestru[lnFileStru+6,2] = 'C'
  loFormSet_lafilestru[lnFileStru+6,3] = 6
  loFormSet_lafilestru[lnFileStru+6,4] = 0

  loFormSet_lafilestru[lnFileStru+7,1] = 'FABRIC'
  loFormSet_lafilestru[lnFileStru+7,2] = 'C'
  loFormSet_lafilestru[lnFileStru+7,3] = 7
  loFormSet_lafilestru[lnFileStru+7,4] = 0

  loFormSet_lafilestru[lnFileStru+8,1] = 'PATTERN'
  loFormSet_lafilestru[lnFileStru+8,2] = 'C'
  loFormSet_lafilestru[lnFileStru+8,3] = 10
  loFormSet_lafilestru[lnFileStru+8,4] = 0

  loFormSet_lafilestru[lnFileStru+9,1] = 'CSTYMAJOR'
  loFormSet_lafilestru[lnFileStru+9,2] = 'C'
  loFormSet_lafilestru[lnFileStru+9,3] = 19
  loFormSet_lafilestru[lnFileStru+9,4] = 0

  loFormSet_lafilestru[lnFileStru+10,1] = 'CSORTFIELD'
  loFormSet_lafilestru[lnFileStru+10,2] = 'C'
  loFormSet_lafilestru[lnFileStru+10,3] = 80
  loFormSet_lafilestru[lnFileStru+10,4] = 0

  *-- if MF or PO modules is installed
  IF 'MF' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PO' $ oAriaEnvironment.CompanyInstalledModules
    loFormSet_lafilestru[lnFileStru+11,1] = 'TRANCD'
    loFormSet_lafilestru[lnFileStru+11,2] = 'C'
    loFormSet_lafilestru[lnFileStru+11,3] = 1
    loFormSet_lafilestru[lnFileStru+11,4] = 0

    loFormSet_lafilestru[lnFileStru+12,1] = 'CTKTNO'
    loFormSet_lafilestru[lnFileStru+12,2] = 'C'
    loFormSet_lafilestru[lnFileStru+12,3] = 6
    loFormSet_lafilestru[lnFileStru+12,4] = 0
  ENDIF

  *-- if system use dyelots add fabric color field.
  IF loFormSet_llUseDyes
    loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),1] = 'CFABCOLOR'
    loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),2] = 'C'
    loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),3] = 6
    loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),4] = 0
  ENDIF

  lnElmnt = lnAddToArr

  FOR lnCount = 1 TO lnElmnt
    STORE '' TO loFormSet_lafilestru[lnFileStru+lnCount,7],loFormSet_lafilestru[lnFileStru+lnCount,8],loFormSet_lafilestru[lnFileStru+lnCount,9],;
      loFormSet_lafilestru[lnFileStru+lnCount,10],loFormSet_lafilestru[lnFileStru+lnCount,11],loFormSet_lafilestru[lnFileStru+lnCount,12],;
      loFormSet_lafilestru[lnFileStru+lnCount,13],loFormSet_lafilestru[lnFileStru+lnCount,14],loFormSet_lafilestru[lnFileStru+lnCount,15],;
      loFormSet_lafilestru[lnFileStru+lnCount,16]
    STORE 0  TO loFormSet_lafilestru[lnFileStru+lnCount,17],  loFormSet_lafilestru[lnFileStru+lnCount,18]
  ENDFOR

  *-- Add 8 ExcCut fields to end of this file structure and TotExcCut field.
  DIMENSION loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1)+9,ALEN(loFormSet_lafilestru,2)]
  loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),1] = 'TOTEXCCUT'
  loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),2] = 'N'
  loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),3] = 7
  loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),4] = 0
  STORE '' TO loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),7],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),8],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),9],;
    loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),10],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),11],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),12],;
    loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),13],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),14],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),15],;
    loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),16]
  STORE 0  TO loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),17],  loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),18]


  lnThArrPos = ALEN(loFormSet_lafilestru,1) - 9
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    loFormSet_lafilestru[lnThArrPos+lnI,1] = 'EXCCUT'+lcI
    loFormSet_lafilestru[lnThArrPos+lnI,2] = 'N'
    loFormSet_lafilestru[lnThArrPos+lnI,3] = 6
    loFormSet_lafilestru[lnThArrPos+lnI,4] = 0
    STORE '' TO loFormSet_lafilestru[lnThArrPos+lnI,7],loFormSet_lafilestru[lnThArrPos+lnI,8],loFormSet_lafilestru[lnThArrPos+lnI,9],;
      loFormSet_lafilestru[lnThArrPos+lnI,10],loFormSet_lafilestru[lnThArrPos+lnI,11],loFormSet_lafilestru[lnThArrPos+lnI,12],;
      loFormSet_lafilestru[lnThArrPos+lnI,13],loFormSet_lafilestru[lnThArrPos+lnI,14],loFormSet_lafilestru[lnThArrPos+lnI,15],;
      loFormSet_lafilestru[lnThArrPos+lnI,16]
    STORE 0  TO loFormSet_lafilestru[lnThArrPos+lnI,17],  loFormSet_lafilestru[lnThArrPos+lnI,18]
  ENDFOR

  *-- Add Totwip field to end of this file structure.
  DIMENSION loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1)+1,ALEN(loFormSet_lafilestru,2)]
  loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),1] = 'TOTWIP'
  loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),2] = 'N'
  loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),3] = 7
  loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),4] = 0
  STORE '' TO loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),7],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),8],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),9],;
    loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),10],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),11],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),12],;
    loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),13],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),14],loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),15],;
    loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),16]
  STORE 0  TO loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),17],  loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1),18]

  lnFileStru = ALEN(loFormSet_lafilestru,1)
  DIMENSION loFormSet_lafilestru[ALEN(loFormSet_lafilestru,1)+1,ALEN(loFormSet_lafilestru,2)]
  loFormSet_lafilestru[lnFileStru +1,1] = 'cPeggedDye'
  loFormSet_lafilestru[lnFileStru +1,2] = 'C'
  loFormSet_lafilestru[lnFileStru +1,3] = 10
  loFormSet_lafilestru[lnFileStru +1,4] = 0
  STORE '' TO loFormSet_lafilestru[lnFileStru +1,7],loFormSet_lafilestru[lnFileStru +1,8],loFormSet_lafilestru[lnFileStru +1,9],;
    loFormSet_lafilestru[lnFileStru +1,10],loFormSet_lafilestru[lnFileStru +1,11],loFormSet_lafilestru[lnFileStru +1,12],;
    loFormSet_lafilestru[lnFileStru +1,13],loFormSet_lafilestru[lnFileStru +1,14],loFormSet_lafilestru[lnFileStru +1,15],;
    loFormSet_lafilestru[lnFileStru +1,16]
  STORE 0  TO loFormSet_lafilestru[lnFileStru +1,17],  loFormSet_lafilestru[lnFileStru +1,18]

  lnFileStru = ALEN(loFormSet_lafilestru,1)
  DIMENSION loFormSet_lafilestru[lnFileStru + 9,ALEN(loFormSet_lafilestru,2)]
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    loFormSet_lafilestru[lnFileStru +lnI,1] = 'Alo'+lcI
    loFormSet_lafilestru[lnFileStru +lnI,2] = 'N'
    loFormSet_lafilestru[lnFileStru +lnI,3] = 5
    loFormSet_lafilestru[lnFileStru +lnI,4] = 0
    STORE '' TO loFormSet_lafilestru[lnFileStru +lnI,7],loFormSet_lafilestru[lnFileStru +lnI,8],loFormSet_lafilestru[lnFileStru +lnI,9],;
      loFormSet_lafilestru[lnFileStru +lnI,10],loFormSet_lafilestru[lnFileStru +lnI,11],loFormSet_lafilestru[lnFileStru +lnI,12],;
      loFormSet_lafilestru[lnFileStru +lnI,13],loFormSet_lafilestru[lnFileStru +lnI,14],loFormSet_lafilestru[lnFileStru +lnI,15],;
      loFormSet_lafilestru[lnFileStru +lnI,16]
    STORE 0  TO loFormSet_lafilestru[lnFileStru +lnI,17],  loFormSet_lafilestru[lnFileStru +lnI,18]
  ENDFOR
  loFormSet_lafilestru[lnFileStru +9,1] = 'TotAlo'
  loFormSet_lafilestru[lnFileStru +9,2] = 'N'
  loFormSet_lafilestru[lnFileStru +9,3] = 6
  loFormSet_lafilestru[lnFileStru +9,4] = 0
  STORE '' TO loFormSet_lafilestru[lnFileStru +9,7],loFormSet_lafilestru[lnFileStru  +9,8],loFormSet_lafilestru[lnFileStru  +9,9],;
    loFormSet_lafilestru[lnFileStru +9,10],loFormSet_lafilestru[lnFileStru +9,11],loFormSet_lafilestru[lnFileStru +9,12],;
    loFormSet_lafilestru[lnFileStru +9,13],loFormSet_lafilestru[lnFileStru +9,14],loFormSet_lafilestru[lnFileStru +9,15],;
    loFormSet_lafilestru[lnFileStru +9,16]
  STORE 0  TO loFormSet_lafilestru[lnFileStru +9,17],  loFormSet_lafilestru[lnFileStru +9,18]

  lnFileStru = ALEN(loFormSet_lafilestru,1)
  DIMENSION loFormSet_lafilestru[lnFileStru + 9,ALEN(loFormSet_lafilestru,2)]
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    loFormSet_lafilestru[lnFileStru +lnI,1] = 'Avl'+lcI
    loFormSet_lafilestru[lnFileStru +lnI,2] = 'N'
    *B608118,1 WAM 06/10/2007 Increase he width of the available fields
    *loFormSet_lafilestru[lnFileStru +lnI,3] = 5
    loFormSet_lafilestru[lnFileStru +lnI,3] = 6
    *B608118,1 WAM 06/10/2007 (End)
    loFormSet_lafilestru[lnFileStru +lnI,4] = 0
    STORE '' TO loFormSet_lafilestru[lnFileStru +lnI,7],loFormSet_lafilestru[lnFileStru +lnI,8],loFormSet_lafilestru[lnFileStru +lnI,9],;
      loFormSet_lafilestru[lnFileStru +lnI,10],loFormSet_lafilestru[lnFileStru +lnI,11],loFormSet_lafilestru[lnFileStru +lnI,12],;
      loFormSet_lafilestru[lnFileStru +lnI,13],loFormSet_lafilestru[lnFileStru +lnI,14],loFormSet_lafilestru[lnFileStru +lnI,15],;
      loFormSet_lafilestru[lnFileStru +lnI,16]
    STORE 0  TO loFormSet_lafilestru[lnFileStru +lnI,17],  loFormSet_lafilestru[lnFileStru +lnI,18]
  ENDFOR
  loFormSet_lafilestru[lnFileStru +9,1] = 'TotAvl'
  loFormSet_lafilestru[lnFileStru +9,2] = 'N'
  *B608118,1 WAM 06/10/2007 Increase he width of the available fields
  *loFormSet_lafilestru[lnFileStru +9,3] = 6
  loFormSet_lafilestru[lnFileStru +9,3] = 7
  *B608118,1 WAM 06/10/2007 (End)
  loFormSet_lafilestru[lnFileStru +9,4] = 0
  STORE '' TO loFormSet_lafilestru[lnFileStru +9,7],loFormSet_lafilestru[lnFileStru  +9,8],loFormSet_lafilestru[lnFileStru  +9,9],;
    loFormSet_lafilestru[lnFileStru +9,10],loFormSet_lafilestru[lnFileStru +9,11],loFormSet_lafilestru[lnFileStru +9,12],;
    loFormSet_lafilestru[lnFileStru +9,13],loFormSet_lafilestru[lnFileStru +9,14],loFormSet_lafilestru[lnFileStru +9,15],;
    loFormSet_lafilestru[lnFileStru +9,16]
  STORE 0  TO loFormSet_lafilestru[lnFileStru +9,17],  loFormSet_lafilestru[lnFileStru +9,18]

  lnFileStru = ALEN(loFormSet_lafilestru,1) + 1
  DIMENSION loFormSet_lafilestru[lnFileStru,ALEN(loFormSet_lafilestru,2)]
  loFormSet_lafilestru[lnFileStru ,1] = 'cReason'
  loFormSet_lafilestru[lnFileStru ,2] = 'C'
  loFormSet_lafilestru[lnFileStru ,3] = 100
  loFormSet_lafilestru[lnFileStru ,4] = 0
  STORE '' TO loFormSet_lafilestru[lnFileStru ,7],loFormSet_lafilestru[lnFileStru ,8],loFormSet_lafilestru[lnFileStru ,9],;
    loFormSet_lafilestru[lnFileStru ,10],loFormSet_lafilestru[lnFileStru ,11],loFormSet_lafilestru[lnFileStru ,12],;
    loFormSet_lafilestru[lnFileStru ,13],loFormSet_lafilestru[lnFileStru ,14],loFormSet_lafilestru[lnFileStru ,15],;
    loFormSet_lafilestru[lnFileStru ,16]
  STORE 0  TO loFormSet_lafilestru[lnFileStru ,17],  loFormSet_lafilestru[lnFileStru ,18]


  lnFileStru = ALEN(loFormSet_lafilestru,1) + 1
  DIMENSION loFormSet_lafilestru[lnFileStru,ALEN(loFormSet_lafilestru,2)]
  loFormSet_lafilestru[lnFileStru ,1] = 'cPosition'
  loFormSet_lafilestru[lnFileStru ,2] = 'C'
  loFormSet_lafilestru[lnFileStru ,3] = 1
  loFormSet_lafilestru[lnFileStru ,4] = 0
  STORE '' TO loFormSet_lafilestru[lnFileStru ,7],loFormSet_lafilestru[lnFileStru ,8],loFormSet_lafilestru[lnFileStru ,9],;
    loFormSet_lafilestru[lnFileStru ,10],loFormSet_lafilestru[lnFileStru ,11],loFormSet_lafilestru[lnFileStru ,12],;
    loFormSet_lafilestru[lnFileStru ,13],loFormSet_lafilestru[lnFileStru ,14],loFormSet_lafilestru[lnFileStru ,15],;
    loFormSet_lafilestru[lnFileStru ,16]
  STORE 0  TO loFormSet_lafilestru[lnFileStru ,17],  loFormSet_lafilestru[lnFileStru ,18]

  DIMENSION laTmpIndex[3,2]
  laTmpIndex[1,1] = 'cordtype+order+STR(lineno,6)'
  laTmpIndex[1,2] = loFormSet_lcTmpIndex
  laTmpIndex[2,1] = 'Style+DTOS(complete)+cordtype+order+store+STR(lineno,6)'
  laTmpIndex[2,2] = loFormSet_lcTmStyTag
  laTmpIndex[3,1] = 'cSortField'
  laTmpIndex[3,2] = loFormSet_lcTmpOrdLn

  =ACOPY(loFormSet_lafilestru,laFileStru)

  =oAriaEnvironment.CURSORS.createcursor(loFormSet_lcTmpOrdLn,@laFileStru,@laTmpIndex)
  *!*	CREATE CURSOR (loFormSet_lcTmpOrdLn) FROM ARRAY laFileStru
  *!*	FOR lnI = 1 TO ALEN(laTmpIndex,1)
  *!*	  INDEX ON &laTmpIndex[lnI,1] TAG &laTmpIndex[lnI,2] ADDITIVE
  *!*	ENDFOR

  =ACOPY(laFileStru,loFormSet_lafilestru)


  IF loFormSet_llChkAprov
    CREATE CURSOR (loFormSet_lcTmpOrdAp) (TYPE C(1),ORDER C(6),AprAmnt N(13,2),TotQty N(7),lExceed L(1))
    INDEX ON TYPE + ORDER TAG (loFormSet_lcTmpOrdAp) ADDITIVE
    SET ORDER TO (loFormSet_lcTmpOrdAp)
  ENDIF

  *!*************************************************************
  *! Name      : lfOpnFiles
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : open Files needed in the program
  *!*************************************************************
  *! Called from : ALAUTAL.PRG
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
FUNCTION lfOpnFiles
  PARAMETERS loFormSet

  =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+"ORDHDR",oAriaEnvironment.DataDir+"ORDHDR", "SH")
  =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+"ORDLINE",oAriaEnvironment.DataDir+"ORDLINE", "SH")
  =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+"STYLE",oAriaEnvironment.DataDir+"STYLE", "SH")
  =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+"SCALE",oAriaEnvironment.DataDir+"SCALE", "SH")
  =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+"STYDYE",oAriaEnvironment.DataDir+"STYDYE", "SH")
  =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+"CUSTOMER",oAriaEnvironment.DataDir+"CUSTOMER", "SH")
  =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+"PIKTKT",oAriaEnvironment.DataDir+"PIKTKT", "SH")
  =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+"WAREHOUS","", "SH")
  IF 'PO' $ oAriaEnvironment.CompanyInstalledModules
    =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+"APVENDOR","","SH")
  ENDIF

  *B608316,1 MMT 10/11/2007 convert 3PL Provider Enhamancemnt from 27[Start]
  IF 'AS' $ oAriaEnvironment.CompanyInstalledModules
    =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'EDIACPRT',oAriaEnvironment.DataDir+'ACCFACT','SH')
    =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'EDIPD',oAriaEnvironment.DataDir+'PARTTRANS','SH')
    =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'EDITRANS',oAriaEnvironment.DataDir+'TYPEKEY','SH')
  ENDIF
  *B608316,1 MMT 10/11/2007 convert 3PL Provider Enhamancemnt from 27[End]

  RETURN .T.
  *!*************************************************************
  *! Name      : lfOpnSqlFl
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : open SQL Files needed in the program
  *!*************************************************************
  *! Called from : ALAUTAL.PRG
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
FUNCTION lfOpnSqlFl
  PARAMETERS loFormSet

  IF 'MA' $ oAriaEnvironment.CompanyInstalledModules
    =lfOpenSql(loFormSet,'DYE_REL','DYE_REL1')
  ENDIF
  IF 'MF' $ oAriaEnvironment.CompanyInstalledModules OR 'PO' $ oAriaEnvironment.CompanyInstalledModules
    llOpenSQLPo = lfOpenSql(loFormSet,'POSHDR','POSHDR1')
    RETURN llOpenSQLPo
  ENDIF

  RETURN .T.

  *!*************************************************************
  *! Name      : lfClsFiles
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Close Files opened in the program
  *!*************************************************************
  *! Called from : ALAUTAL.PRG
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
FUNCTION lfClsFiles

  IF USED("ORDHDR")
    USE IN ORDHDR
  ENDIF
  IF USED("ORDLINE")
    USE IN ORDLINE
  ENDIF
  IF USED("STYLE")
    USE IN STYLE
  ENDIF
  IF USED("SCALE")
    USE IN SCALE
  ENDIF
  IF USED("STYDYE")
    USE IN STYDYE
  ENDIF
  IF USED("CUSTOMER")
    USE IN CUSTOMER
  ENDIF
  IF USED("PIKTKT")
    USE IN PIKTKT
  ENDIF
  IF USED("WAREHOUS")
    USE IN WAREHOUS
  ENDIF

  IF 'MA' $ oAriaEnvironment.CompanyInstalledModules
    IF USED("ITEM1")
      USE IN ITEM1
    ENDIF
    IF USED("DYE_REL1")
      USE IN DYE_REL1
    ENDIF
    IF USED("BOM1")
      USE IN BOM1
    ENDIF
  ENDIF
  IF 'PO' $ oAriaEnvironment.CompanyInstalledModules
    IF USED("APVENDOR")
      USE IN APVENDOR
    ENDIF
  ENDIF
  IF 'MF' $ oAriaEnvironment.CompanyInstalledModules OR 'PO' $ oAriaEnvironment.CompanyInstalledModules
    IF USED("CUTPICK1")
      USE IN CUTPICK1
    ENDIF
    IF USED("POSHDR1")
      USE IN POSHDR1
    ENDIF
    IF USED("POSLN1")
      USE IN POSLN1
    ENDIF
  ENDIF


  *!*************************************************************
  *! Name      : lfOpenSql
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : function to open SQL tables
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfOpenSql
  LPARAMETERS loFormSet,lcTable,lcCursor,lcWhereCond,llIsInitial

  LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
  PRIVATE laIndex
  DIMENSION laIndex[1,2]

  lcSqlStatment   = "SELECT * FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")
  loSqlConnection = CREATEOBJECT('ariamain.remotedataaccess')

  lnConnectionHandlar = loSqlConnection.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaEnvironment.ActiveCompanyConStr,3,;
    'SAVE',loFormSet_DataSessionId)


  *loFormSet_DataSessionId
  IF lnConnectionHandlar = 1
    lnBuffering = CURSORGETPROP("Buffering",lcCursor)
    =CURSORSETPROP("Buffering",3,lcCursor)
    *-- To initialize the indecis that will be created for each file
    =lfCrtindex(loFormSet,lcTable)
    SELECT (lcCursor)
    FOR lnI = 1 TO ALEN(laIndex,1)
      lcIndex = laIndex[lnI,1]
      lcTag   = laIndex[lnI,2]
      *INDEX ON &lcIndex. TAG (lcTag) OF (lcCursor)
      INDEX ON &lcIndex. TAG (lcTag) 
    ENDFOR
    lcTag = laIndex[1,2]
    SET ORDER TO TAG (lcTag)
    =CURSORSETPROP("Buffering",lnBuffering,lcCursor)
    IF llIsInitial
      loFormSet_Dataenvironment.INITIALSELECTEDALIAS = lcCursor
    ENDIF
    loSqlConnection = NULL
  ELSE
    =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
    loSqlConnection = NULL
    RETURN .F.
  ENDIF
  *-- end of lfOpenSql.

  *!*************************************************************
  *! Name      : lfCrtindex
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : function to Set the index for the SQL files
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfCrtindex
  LPARAMETERS loFormSet,lcTable

  DO CASE
    CASE UPPER(lcTable) = "POSHDR"
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO'
      laIndex[1,2] = 'POSHDR'

    CASE UPPER(lcTable) = "POSLN"
      DIMENSION laIndex[2,2]
      laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD'
      laIndex[1,2] = 'POSLN'
      laIndex[2,1] = 'SHIPNO+CSTYTYPE+CBUSDOCU+PO+STYLE+STR(LINENO,6)+TRANCD'
      laIndex[2,2] = 'POSLNSH'

    CASE UPPER(lcTable) = "CUTPICK"
      DIMENSION laIndex[2,2]
      laIndex[1,1] = 'TRANCD+CTKTNO+STYLE'
      laIndex[1,2] = 'Cutpick'
      laIndex[2,1] = 'TRANCD+ORDER+CORDLINE'
      laIndex[2,2] = 'Cutord'

    CASE UPPER(lcTable) = "SHPMTHDR"
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'CBUSDOCU+CSHPTYPE+SHIPNO'
      laIndex[1,2] = 'SHPMTHDR'

    CASE UPPER(lcTable) = "ITEM"
      DIMENSION laIndex[5,2]
      laIndex[1,1] = 'CINVTYPE+STYLE'
      laIndex[1,2] = 'STYLE'
      laIndex[2,1] = 'PATTERN+CINVTYPE+STYLE FOR .NOT. EMPTY(PATTERN)'
      laIndex[2,2] = 'STYLEPAT'
      laIndex[3,1] = 'CINVTYPE+CSTYMAJOR'
      laIndex[3,2] = 'CSTYLE'
      laIndex[4,1] = 'CSTYGRADE+CINVTYPE+STYLE'
      laIndex[4,2] = 'STYQLTY'
      laIndex[5,1] = 'CINVTYPE+STATUS+CSTYGROUP'
      laIndex[5,2] = 'CRM'

    CASE UPPER(lcTable) = "DYE_REL"
      DIMENSION laIndex[3,2]
      laIndex[1,1] = 'CINVTYPE+CITEM+DYELOT'
      laIndex[1,2] = 'DYE_REL'
      laIndex[2,1] = 'CINVTYPE+CITEM+CDYE_SEQ'
      laIndex[2,2] = 'SEQUENCE'
      laIndex[3,1] = 'CTMPSCOPE'
      laIndex[3,2] = 'SCOPE'

    CASE UPPER(lcTable) = "BOM"
      DIMENSION laIndex[2,2]
      laIndex[1,1] = 'CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6)'
      laIndex[1,2] = 'MULTIBOM'
      laIndex[2,1] = 'TYP+CINVTYPC+ITEM+CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+CITMMASK'
      laIndex[2,2] = 'MBOMITEM'

  ENDCASE

  *!*************************************************************
  *! Name      : lfUpdatSQL
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : open SQL Files needed in the program
  *!*************************************************************
  *! Called from : ALAUTAL.PRG
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
FUNCTION lfUpdatSQL
  LPARAMETERS loFormSet,lcTable,LcPrimaryKeyList,lcSQLTable

  LOCAL lnConnectionHandlar, lcTranCode,llReturn

  llReturn   = .T.

  loSqlConnection = CREATEOBJECT('ariamain.remotedataaccess')

  lcTranCode = loSqlConnection.BeginTran(oAriaEnvironment.ActiveCompanyConStr,3,'')

  IF TYPE('lcTranCode') = 'N'
    =loSqlConnection.CheckRetResult("BeginTran",lcTranCode,.T.)
    IF BETWEEN(lnRecNo,1,RECCOUNT(lcTable))
      GOTO lnRecNo IN (lcTable)
    ENDIF
    loSqlConnection = NULL
    RETURN .F.
  ENDIF

  lnConnectionHandlar = loSqlConnection.sqlupdate(lcTable,lcTranCode,loFormSet_DataSessionId,LcPrimaryKeyList,lcSQLTable)
  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =loSqlConnection.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    llReturn = .F.
  ENDIF

  lnConnectionHandlar = loSqlConnection.CommitTran(lcTranCode)
  IF lnConnectionHandlar # 1
    =loSqlConnection.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
    llReturn = .F.
  ENDIF

  loSqlConnection = NULL
  RETURN llReturn

  *osa
  *!*	*!*************************************************************
  *!*	*! Name      : lfBundBrow
  *!*	*: Developer : HEND GHANEM (HBG)
  *!*	*: Date      : 11/12/2003
  *!*	*! Purpose   : Bound columns of the Orders grid
  *!*	*!*************************************************************
  *!*	*! Called from : ALAUTAL.PRG
  *!*	*!*************************************************************
  *!*	*! Calls       : None
  *!*	*!*************************************************************
  *!*	*! Passed Parameters : None
  *!*	*!*************************************************************
  *!*	*! Return      : None
  *!*	*!*************************************************************
  *!*	FUNCTION lfBundBrow
  *!*	PARAMETERS loFormSet

  *!*	WITH loFormSet_Ariaform1.grdOrders.grdMultiSelectionGrid
  *!*	  .RecordSource = ''
  *!*	  .RecordSource = loFormSet_lcTmpOrdLn

  *!*	  *-- Build browse columns
  *!*	  .Columns(1).Header1.Caption  = ""
  *!*	  .Columns(1).CurrentControl   = "AriaCheckBox1"
  *!*	  .Columns(2).Header1.Caption  = LANG_AutoAlloc_LabelOrder
  *!*	  .Columns(2).CurrentControl = "Text1"
  *!*	  .Columns(3).Header1.Caption  = LANG_AutoAlloc_LabelAccount
  *!*	  .Columns(3).CurrentControl = "Text1"
  *!*	  .Columns(4).Header1.Caption  = LANG_AutoAlloc_LabelStore
  *!*	  .Columns(4).CurrentControl = "Text1"
  *!*	  .Columns(5).Header1.Caption  = loFormSet_lcStyleTtl
  *!*	  .Columns(5).CurrentControl = "Text1"
  *!*	  .Columns(6).Header1.Caption  = IIF(loFormSet_lluseConfg,LANG_AutoAlloc_LabelConfg,LANG_AutoAlloc_LabelDyelot)
  *!*	  .Columns(6).CurrentControl = "Text1"
  *!*	  .Columns(6).Visible		   = loFormSet_llUseDyes
  *!*	  .Columns(7).Header1.Caption  = LANG_AutoAlloc_LabelGroup
  *!*	  .Columns(7).CurrentControl = "Text1"
  *!*	  .Columns(8).Header1.Caption  = LANG_AutoAlloc_LabelAvialable
  *!*	  .Columns(8).CurrentControl = "Text1"
  *!*	  .Columns(9).Header1.Caption  = LANG_AutoAlloc_LabelOpen
  *!*	  .Columns(9).CurrentControl = "Text1"
  *!*	  .Columns(10).Header1.Caption  = LANG_AutoAlloc_LabelOpnAmnt
  *!*	  .Columns(10).CurrentControl = "Text1"
  *!*	  .Columns(11).Header1.Caption = LANG_AutoAlloc_LabelPiktkt
  *!*	  .Columns(11).CurrentControl = "Text1"
  *!*	  .Columns(12).Header1.Caption = LANG_AutoAlloc_LabelPicked
  *!*	  .Columns(12).CurrentControl = "Text1"
  *!*	  .Columns(13).Header1.Caption = LANG_AutoAlloc_LabelPikAmnt
  *!*	  .Columns(13).CurrentControl = "Text1"
  *!*	  .Columns(14).Header1.Caption = LANG_AutoAlloc_LabelReasone
  *!*	  .Columns(14).CurrentControl = "Text1"
  *!*	  .SETALL('READONLY',.T.,'COLUMN')
  *!*
  *!*	  IF loFormSet_activemode = 'S'
  *!*	    .Columns(1).ControlSource  = 0
  *!*	    .Columns(2).ControlSource  = ""
  *!*	    .Columns(3).ControlSource  = ""
  *!*	    .Columns(4).ControlSource  = ""
  *!*	    .Columns(5).ControlSource  = ""
  *!*	    .Columns(6).ControlSource  = ""
  *!*	    .Columns(7).ControlSource  = ""
  *!*	    .Columns(8).ControlSource  = ""
  *!*	    .Columns(9).ControlSource  = ""
  *!*	    .Columns(10).ControlSource = ""
  *!*	    .Columns(11).ControlSource = ""
  *!*	    .Columns(12).ControlSource = ""
  *!*	    .Columns(13).ControlSource = ""
  *!*	    .Columns(14).ControlSource = ""
  *!*	  ELSE
  *!*	    .Columns(1).ControlSource  = loFormSet_lcTmpOrdLn+'.lnSel'
  *!*	    .Columns(2).ControlSource  = loFormSet_lcTmpOrdLn+'.ORDER'
  *!*	    .Columns(3).ControlSource  = loFormSet_lcTmpOrdLn+'.ACCOUNT'
  *!*	    .Columns(4).ControlSource  = loFormSet_lcTmpOrdLn+'.STORE'
  *!*	    .Columns(5).ControlSource  = loFormSet_lcTmpOrdLn+'.STYLE'
  *!*	    .Columns(6).ControlSource  = loFormSet_lcTmpOrdLn+'.DYELOT'
  *!*	    .Columns(7).ControlSource  = loFormSet_lcTmpOrdLn+'.GROUP'
  *!*	    .Columns(8).ControlSource  = MAX(0, STYDYE.TOTSTK - STYDYE.TOTALO)
  *!*	    .Columns(9).ControlSource  = loFormSet_lcTmpOrdLn+'.TOTQTY'
  *!*	    .Columns(10).ControlSource = EVALUATE(loFormSet_lcTmpOrdLn+'.TOTQTY')*EVALUATE(loFormSet_lcTmpOrdLn+'.PRICE')
  *!*	    .Columns(11).ControlSource = loFormSet_lcTmpOrdLn+'.PIKTKT'
  *!*	    .Columns(12).ControlSource = loFormSet_lcTmpOrdLn+'.TOTPIK'
  *!*	    .Columns(13).ControlSource = EVALUATE(loFormSet_lcTmpOrdLn+'.TOTPIK')*EVALUATE(loFormSet_lcTmpOrdLn+'.PRICE')
  *!*	    .Columns(14).ControlSource = loFormSet_lcTmpOrdLn+'.cReason'
  *!*	  ENDIF
  *!*	  .Refresh
  *!*	ENDWITH
  *osa

  *!*************************************************************
  *! Name      : lfTolBrShw
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Control Enabling & Disabling Tool bar buttons
  *!*************************************************************
  *! Passed Parameters  :  None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfTolBrShw
  PARAMETERS loFormSet

  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	oAriaEnvironment.oToolBar.cmdEdit.Enabled   = loFormSet_llCtrStat7
  *!*	oAriaEnvironment.oToolBar.cmdDelete.Enabled = loFormSet_llCtrStat8
  *!*	oAriaEnvironment.oToolBar.cmdSelect.Enabled = loFormSet_llCtrStat9
  *!*	oAriaEnvironment.oToolBar.cmdFind.Enabled   = loFormSet_llCtrStat10
  loFormSet_oToolBar.cmdEdit.ENABLED   = loFormSet_llCtrStat7
  loFormSet_oToolBar.cmdDelete.ENABLED = loFormSet_llCtrStat8
  loFormSet_oToolBar.cmdSelect.ENABLED = loFormSet_llCtrStat9
  loFormSet_oToolBar.cmdFind.ENABLED   = loFormSet_llCtrStat10
  *HBG [End]

  *!*************************************************************
  *! Name      : lfPrntShw
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Control Enabling & Disabling Print buttons in Tool bar
  *!*************************************************************
  *! Passed Parameters  :  None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfPrntShw
  PARAMETERS loFormSet

  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *oAriaEnvironment.oToolBar.cmdprint.Enabled  = loFormSet_llCtrStat6
  loFormSet_oToolBar.cmdprint.ENABLED  = loFormSet_llCtrStat6
  *HBG [End]


  *!*************************************************************
  *! Name      : lfNavShw
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Control Enabling & Disabling navigation buttons in Tool bar
  *!*************************************************************
  *! Passed Parameters  :  None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfNavShw
  PARAMETERS loFormSet

  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	oAriaEnvironment.oToolBar.cmdTop.Enabled    = loFormSet_llCtrStat1
  *!*	oAriaEnvironment.oToolBar.cmdprev.Enabled   = loFormSet_llCtrStat4
  *!*	oAriaEnvironment.oToolBar.cmdend.Enabled    = loFormSet_llCtrStat2
  *!*	oAriaEnvironment.oToolBar.cmdnext.Enabled   = loFormSet_llCtrStat3
  loFormSet_oToolBar.cmdTop.ENABLED    = loFormSet_llCtrStat1
  loFormSet_oToolBar.cmdprev.ENABLED   = loFormSet_llCtrStat4
  loFormSet_oToolBar.cmdend.ENABLED    = loFormSet_llCtrStat2
  loFormSet_oToolBar.cmdnext.ENABLED   = loFormSet_llCtrStat3
  *HBG [End]

  *osa
  *!*	*!*************************************************************
  *!*	*! Name      : lfBundFlds
  *!*	*: Developer : HEND GHANEM (HBG)
  *!*	*: Date      : 11/12/2003
  *!*	*! Purpose   : Bound Objects on the screen
  *!*	*!*************************************************************
  *!*	*! Passed Parameters  :  None
  *!*	*!*************************************************************
  *!*	*! Return      : None
  *!*	*!*************************************************************
  *!*	*
  *!*	FUNCTION lfBundFlds
  *!*	PARAMETERS loFormSet


  *!*	=SEEK(EVALUATE(loFormSet_lcTmpOrdLn+'.Style'), 'STYLE')
  *!*	=SEEK(EVALUATE(loFormSet_lcTmpOrdLn+'.Style')+EVALUATE(loFormSet_lcTmpOrdLn+'.cWareCode')+EVALUATE(loFormSet_lcTmpOrdLn+'.Dyelot'), 'STYDYE')
  *!*	=SEEK('S' + STYLE.Scale , 'SCALE')

  *!*	SELECT (loFormSet_lcTmpOrdLn)
  *!*	SCATTER FIELDS LIKE PIK* TO laPik
  *!*	SCATTER FIELDS LIKE QTY* TO laQty

  *!*	WITH loFormSet_AriaForm1.cntDetail
  *!*	  .keyStyle.value = EVALUATE(loFormSet_lcTmpOrdLn+'.Style')
  *!*	  .txtDesc.Value  = STYLE.Desc1
  *!*	  .keyDyelot.keytextbox.Value = EVALUATE(loFormSet_lcTmpOrdLn+'.Dyelot')
  *!*	  .txtGroup.Value = EVALUATE(loFormSet_lcTmpOrdLn+'.Group')
  *!*	  .txtstore.Value = EVALUATE(loFormSet_lcTmpOrdLn+'.Store')
  *!*	  lcWareCode = EVALUATE(loFormSet_lcTmpOrdLn+'.cWareCode')
  *!*	  lnElem     = ASCAN(loFormSet_laWareHous,lcWareCode)
  *!*	  .cboLocation.Value = IIF(lnElem <> 0, ASUBSCRIPT(loFormSet_laWareHous, lnElem, 1),.cboLocation.Value)
  *!*
  *!*	  FOR lnI = 1 TO 8
  *!*	    lcI = STR(lnI,1)
  *!*	    .sizesbreak1.txtsize&lcI..Value = loFormSet_laSizes[lnI]
  *!*	  ENDFOR

  *!*	  FOR lnI = 1 TO SCALE.cnt
  *!*	    lcI = STR(lnI,1)
  *!*	    .sizesbreak1.txtsize&lcI..Value = Scale.Sz&lcI
  *!*	    .sizesbreak1.txtQty&lcI..Value  = MAX(EVALUATE(loFormSet_lcTmpOrdLn+'.Avl'+lcI) - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik'+lcI),0)
  *!*	    .txtord&lcI..Value = laQty[lnI]
  *!*	    .txtWip&lcI..Value = EVALUATE(loFormSet_lcTmpOrdLn+'.Cut'+lcI)
  *!*	    .txtPik&lcI..Value = laPik[lnI]
  *!*	  ENDFOR
  *!*
  *!*	  IF SCALE.Cnt <> 0
  *!*	    FOR lnI = SCALE.Cnt + 1 TO 8
  *!*	      lcI = STR(lnI,1)
  *!*	      .sizesbreak1.txtSize&lcI..Value = ""
  *!*	      .sizesbreak1.txtQty&lcI..Value  = 0
  *!*	      .txtord&lcI..Value = 0
  *!*	      .txtWip&lcI..Value = 0
  *!*	      .txtPik&lcI..Value = 0
  *!*	    ENDFOR
  *!*	  ENDIF
  *!*
  *!*	  .sizesbreak1.txttotalQty.Value = MAX(EVALUATE(loFormSet_lcTmpOrdLn+'.TotAvl') - EVALUATE(loFormSet_lcTmpOrdLn+'.TotPik'),0)
  *!*	  .txttotalOrd.Value = EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty')
  *!*	  .txttotalPik.Value = EVALUATE(loFormSet_lcTmpOrdLn+'.TotPik')
  *!*	  .lblWIP.Caption = IIF(STYLE.Make .OR. EOF(loFormSet_lcTmpOrdLn) , 'C/T' , 'P/O')
  *!*	  .keyDyelot.keytextbox.Enabled  = !EVALUATE(loFormSet_lcTmpOrdLn+'.Picked')
  *!*	  .keyDyelot.keyCmd.Enabled      = !EVALUATE(loFormSet_lcTmpOrdLn+'.Picked')
  *!*	  .cboLocation.Enabled = !EVALUATE(loFormSet_lcTmpOrdLn+'.Picked')
  *!*	ENDWITH

  *!*	loFormSet_AriaForm1.grdOrders.cmdSelect.Caption = IIF(EVALUATE(loFormSet_lcTmpOrdLn+'.lnSel')=0,LANG_AUTOALLOC_ButtSelect,LANG_AUTOALLOC_ButtUnSelect)
  *osa

  *!*************************************************************
  *! Name      : lpShow
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Local Show Procedure
  *!*************************************************************
  *! Called from : Control panel global Show
  *!*************************************************************
  *! Calls       : lfvScope() , lfShowGets() , lfvpbSel , lfRefresh
  *!*************************************************************
  *! Passed Parameters  :  None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
PROCEDURE lpShow
  PARAMETERS loFormSet

  *-- Control Enabling & Disabling Tool bar buttons
  STORE .F. TO  loFormSet_llCtrStat7,loFormSet_llCtrStat8,loFormSet_llCtrStat9,loFormSet_llCtrStat10
  *=lfTolBrShw(loFormSet)


  *-- IF The user has generated piking tikets for all the order lines in scope
  *-- Clear for new selection
  loFormSet_lnBrRecNo = RECNO(loFormSet_lcTmpOrdLn)

  *=lfBundBrow(loFormSet)


  *-- Bound Objects on the screen
  *=lfBundFlds(loFormSet)

  *-- IF the current record is selected
  IF EVALUATE(loFormSet_lcTmpOrdLn+'.lnSel') = 1
    *osa
    =lfShowGets(.T.,loFormSet)
    *osa
  ELSE
    loFormSet_llCh3Stat = .F.
    loFormSet_laPikSt = .F.
    *=lfDisblGts(loFormSet)
  ENDIF    && End of IF


  *-- end of lpShow.

  *!*************************************************************
  *! Name      : lfShowGets
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to control the Enabling and Disabling of
  *!             the Objects in the screen [lcAutAlCh3]
  *!*************************************************************
  *! Called from : lpShow() , lfvScope() , lfAloScr() , lfRelScr() ,
  *!               lfwBrows() , lfvStyle() , lfvWareCode() , lfvDyelot() ,
  *!               lfvSelect() , lfvSelAll() , lfvInvert() , lfBrowTrap()
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters  :  [.T.] IF We want to refresh the Enabling and
  *!                             Disabling of all the Objects in the screen
  *!                             [lcAutAlCh3]
  *!                       [.F.] IF We want to refresh the Enabling and
  *!                             Disabling of the Allocated Get fields only
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfShowGets
  PARAMETERS llParm,loFormSet


  lcStyle    = EVALUATE(loFormSet_lcTmpOrdLn+'.Style')
  lcWareCode = EVALUATE(loFormSet_lcTmpOrdLn+'.cWareCode')
  lcDyelot   = EVALUATE(loFormSet_lcTmpOrdLn+'.Dyelot')
  =SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')
  =SEEK(lcStyle , 'STYLE')
  =SEEK('S' + STYLE.SCALE , 'SCALE')

  *IF We want to refresh the Enabling and Disabling of all the Objects in the
  *screen [lcAutAlCh3]
  IF llParm
    loFormSet_llCh3Stat = IIF(EVALUATE(loFormSet_lcTmpOrdLn+'.TotPik') = 0 , .T. , .F.)
    *loFormSet_AriaForm1.cntDetail.cboLocation.ENABLED = loFormSet_llCh3Stat
    *IF The system use Dyelots
    IF loFormSet_llUseDyes
      loFormSet_llDyelotSt = IIF(STYLE.cDye_Flg = 'N' , .F. , loFormSet_llCh3Stat)
      *loFormSet_AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE = IIF(STYLE.cDye_Flg = 'N' , SPACE(10) , loFormSet_AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE)
      *loFormSet_AriaForm1.cntDetail.keyDyelot.ENABLED = loFormSet_llDyelotSt
    ELSE    && Else
      *loFormSet_AriaForm1.cntDetail.keyStyle.ENABLED = loFormSet_llCh3Stat
    ENDIF    && End of IF
  ENDIF    && End of IF

  *IF we Are at the end of the STYDYE file or the current Style is Dyelot
  *Yes and the Dyelot field is empty
  IF EOF('STYDYE') .OR. (loFormSet_llUseDyes .AND. STYLE.cDye_Flg = 'Y')
    *.AND. EMPTY(loFormSet_AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE))
    loFormSet_laPikSt = .F.
  ELSE    && Else
    FOR lnI = 1 TO 8
      loFormSet_laPikSt[lnI] = IIF(SCALE.CNT < lnI , .F. , .T.)
    ENDFOR
  ENDIF    && End of IF

  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    *loFormSet_AriaForm1.cntDetail.txtPik&lcI..ENABLED = loFormSet_laPikSt[lnI]
    *loFormSet_AriaForm1.cntDetail.sizesbreak1.txtsize&lcI..ENABLED = .T.
  ENDFOR

  *osa
  *!*	*!*************************************************************
  *!*	*! Name      : lfDisblGts
  *!*	*: Developer : HEND GHANEM (HBG)
  *!*	*: Date      : 11/12/2003
  *!*	*! Purpose   : Function to Disable the Objects in the screen [lcAutAlCh3]
  *!*	*!*************************************************************
  *!*	*! Called from : lpShow() , lfvScope() , lfAloScr() , lfRelScr() ,
  *!*	*!               lfwBrows() , lfvStyle() , lfvWareCode() , lfvDyelot() ,
  *!*	*!               lfvSelect() , lfvSelAll() , lfvInvert() , lfBrowTrap()
  *!*	*!*************************************************************
  *!*	*! Calls       : None
  *!*	*!*************************************************************
  *!*	*! Return      : None
  *!*	*!*************************************************************
  *!*	*
  *!*	FUNCTION lfDisblGts
  *!*	PARAMETERS loFormSet

  *!*	loFormSet_AriaForm1.cntDetail.cboLocation.Enabled = loFormSet_llCh3Stat
  *!*	loFormSet_AriaForm1.cntDetail.keyDyelot.keytextbox.Enabled = loFormSet_llCh3Stat
  *!*	loFormSet_AriaForm1.cntDetail.keyDyelot.Enabled = loFormSet_llCh3Stat
  *!*	loFormSet_AriaForm1.cntDetail.keyStyle.Enabled = loFormSet_llCh3Stat
  *!*	FOR lnI = 1 TO 8
  *!*	  lcI = STR(lnI,1)
  *!*	  loFormSet_AriaForm1.cntDetail.txtPik&lcI..Enabled = loFormSet_laPikSt[lnI]
  *!*	  loFormSet_AriaForm1.cntDetail.sizesbreak1.txtsize&lcI..Enabled = .T.
  *!*	ENDFOR
  *osa

  *!*************************************************************
  *! Name      : lfGetWareH
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to get the fill the warehouse desc. and
  *!             warehouse code arrays
  *!*************************************************************
  *! Called from : ALAUTAL.PRG
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfGetWareH
  PARAMETERS loFormSet

  SELECT IIF(EMPTY(cDesc),PADR(cWareCode,35),cDesc) AS cDescrip, cWareCode ;
    FROM WAREHOUS ;
    WHERE lStyInv;
    INTO ARRAY loFormSet_laWareHous;
    ORDER BY cDescrip

  *!*************************************************************
  *! Name      : lfwBrows
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : When function of the Browse [lcOrdLBrow]
  *!*************************************************************
  *! Called from : The Browse [lcOrdLBrow]
  *!*************************************************************
  *! Calls       : lfShowGets() , lfRefresh()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfwBrows
  PARAMETERS loFormSet

  loFormSet_llForceAll = .F.

  loFormSet_lnBrRecNo  = RECNO(loFormSet_lcTmpOrdLn)


  *-- Bound Objects on the screen
  *=lfBundFlds(loFormSet)

  *IF the current record is selected
  IF EVALUATE(loFormSet_lcTmpOrdLn+'.lnSel') = 1
    *osa
    =lfShowGets(.T.,loFormSet)
    *osa
  ELSE
    loFormSet_llCh3Stat = .F.
    loFormSet_laPikSt = .F.
    *=lfDisblGts(loFormSet)
  ENDIF    && End of IF


  llEOF = .F.          && Flag to know if we are at the First record of the temp. Order lines file
  llBOF = .F.          && Flag to know if we are at the Last record of the temp. Order lines file

  *IF The temp. Order lines file is not empty
  IF !EOF(loFormSet_lcTmpOrdLn) .AND. !BOF(loFormSet_lcTmpOrdLn)
    SKIP 1
    *IF We are at the End of the temp. Order lines file
    IF EOF(loFormSet_lcTmpOrdLn)
      llEOF = .T.
      SKIP -2
      *IF We are at the Begin of the temp. Order lines file
      IF BOF(loFormSet_lcTmpOrdLn)
        llBOF = .T.
      ELSE    && Else
        SKIP 1
      ENDIF    && End of IF
    ELSE    && Else
      SKIP -2
      *IF We are at the Begin of the temp. Order lines file
      IF BOF(loFormSet_lcTmpOrdLn)
        llBOF = .T.
      ELSE    && Else
        SKIP 1
      ENDIF    && End of IF
    ENDIF    && End of IF
  ELSE    && Else
    llEOF = .T.
    llBOF = .T.
  ENDIF    && End of IF

  DO CASE
    CASE cPosition = 'T'
      STORE .F. TO loFormSet_llCtrStat1 , loFormSet_llCtrStat4  && First button  , Priveus button
      STORE .T. TO loFormSet_llCtrStat2 , loFormSet_llCtrStat3  && Last button , Next button
    CASE cPosition = 'E'
      STORE .F. TO loFormSet_llCtrStat2 , loFormSet_llCtrStat3  && Last button , Next button
      STORE .T. TO loFormSet_llCtrStat1 , loFormSet_llCtrStat4  && First button  , Priveus button
    CASE EMPTY(cPosition)
      STORE .T. TO loFormSet_llCtrStat2 , loFormSet_llCtrStat3  && Last button , Next button
      STORE .T. TO loFormSet_llCtrStat1 , loFormSet_llCtrStat4  && First button  , Priveus button
  ENDCASE
  =lfNavShw(loFormSet)

  *osa
  *!*	*!*************************************************************
  *!*	*! Name      : lfCntrlObj
  *!*	*: Developer : HEND GHANEM (HBG)
  *!*	*: Date      : 11/12/2003
  *!*	*! Purpose   : Control the displaying of object on the screen
  *!*	*!*************************************************************
  *!*	*! Passed Parameters : None
  *!*	*!*************************************************************
  *!*	*! Return      : None
  *!*	*!*************************************************************
  *!*	*
  *!*	FUNCTION lfCntrlObj
  *!*	PARAMETERS loFormSet

  *!*	WITH loFormSet_AriaForm1.cntDetail
  *!*	  .lblWIP.Caption = IIF(STYLE.Make .OR. EOF(loFormSet_lcTmpOrdLn),LANG_AutoAlloc_CT,LANG_AutoAlloc_PO)
  *!*	  .lblDyelot.Caption = IIF(loFormSet_llUseConfg,LANG_AutoAlloc_Configuration,LANG_AutoAlloc_Dyelot)
  *!*	  .lblDyelot.Visible = loFormSet_llUseDyes
  *!*	  .keyDyelot.Visible = loFormSet_llUseDyes
  *!*	  .lblLocation.Visible = loFormSet_llMultWare
  *!*	  .cboLocation.Visible = loFormSet_llMultWare
  *!*	ENDWITH
  *osa

  *!*************************************************************
  *! Name      : lfvStyle
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Style
  *!*************************************************************
  *! Called from :  Screen ALAUTAL [lcAutAlCh3 - Style field]
  *!*************************************************************
  *! Calls       : lfGScalCnt() , gfStyBrw() , gfModalGen() , gpAdStyWar()
  *!               lfShowGets() , lfRefresh()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvStyle
  PARAMETERS llBrowse,loFormSet

  PRIVATE lnOScalCnt , lnNScalCnt , llObjRet , lcOldAlias ,;
    lcOScale , lcNScale , lnSclRecNo

  lcStyle    = loFormSet_AriaForm1.cntDetail.keyStyle.VALUE
  lcOldVal   = loFormSet_AriaForm1.cntDetail.keyStyle.oldvalue
  lcWareCode = EVALUATE(loFormSet_lcTmpOrdLn+'.cWareCode')
  *IF the Style field was empty or not changed and the user dose not
  *want to Browse
  IF (EMPTY(lcStyle) .OR. lcStyle == lcOldVal) .AND. !llBrowse
    lcStyle = lcOldVal
    RETURN
  ENDIF    && End of IF

  lcOldAlias = ALIAS()               && Variable to save the old Alis
  lnOScalCnt = lfGScalCnt(STYLE)     && Variable to save the old Scale number of sizes
  lcOScale   = STYLE.SCALE             && Variable to save the old Scale

  *IF The user want to Browse or if the Style he entered is not in the file
  IF '?' $ lcStyle .OR. llBrowse .OR. !SEEK(lcStyle , 'STYLE')
    ON KEY LABEL ALT+B
    lnSclRecNo = IIF(EOF('SCALE') .OR. BOF('SCALE') , 0 , RECNO('SCALE'))    && Variable to save the scale file record number
    lcStyle = gfStyBrw('I',"","",.F.)

    *IF The user selected a Style from the Style Browse
    IF !EMPTY(lcStyle)
      lcStyle = PADR(lcStyle , 19)
    ELSE    && Else
      lcStyle = lcOldVal

      *IF We have a valid SCALE record number
      IF lnSclRecNo <> 0
        GO lnSclRecNo IN 'SCALE'
      ELSE    && Else
        GO BOTTOM IN 'SCALE'
        SKIP 1 IN 'SCALE'
      ENDIF    && End of IF
    ENDIF    && End of IF
    llBrowse = .F.
  ENDIF    && End of IF


  *IF The user has changed the Style
  IF lcStyle <> lcOldVal
    lnNScalCnt = lfGScalCnt(lcStyle)       && Variable to save the new Scale number of sizes
    lcNScale = STYLE.SCALE                 && Variable to save the new Scale

    *IF the new Scale number of sizes is less than the old Scale number of sizes
    IF lnNScalCnt < lnOScalCnt

      *** Message : "The number of sizes for ð/Scale ð is less than the"
      ***           "number of sizes for ð/Scale ð . Cannot proceed    "
      ***           "                        < Ok >                    "
      =gfModalGen('TRM44009B00000','DIALOG' , loFormSet_lcStyleTtl + '|' + ALLTRIM(lcStyle) + '/' + ALLTRIM(lcNScale) + '|' + loFormSet_lcStyleTtl + '|' + ALLTRIM(lcOldVal) + '/' + ALLTRIM(lcOScale))
      lcStyle = lcOldVal          && Restore the old value
    ELSE    && Else
      loFormSet_lcStyScale = lcNScale
    ENDIF    && End of IF
  ENDIF    && End of IF

  *IF The user has changed the Style
  IF lcStyle <> lcOldVal
    =SEEK('O' + EVALUATE(loFormSet_lcTmpOrdLn+'.Order') , 'ORDHDR')

    *IF The Style division is not the same as the Order division
    IF STYLE.cDivision <> ORDHDR.cDivision

      *** Message : "ð / ð confilct!                               "
      ***           "                        < Ok >                    "
      =gfModalGen('TRM42086B00000','DIALOG' , loFormSet_lcStyleTtl + LANG_AutoAlloc_MsgDivision)
      lcStyle = lcOldVal          && Restore the old value
    ENDIF    && End of IF
  ENDIF    && End of IF

  *IF The user has changed the Style
  IF lcStyle <> lcOldVal
    =SEEK('O' + EVALUATE(loFormSet_lcTmpOrdLn+'.Order') , 'ORDHDR')

    *IF The Style season is not the same as the Order season
    IF ORDHDR.Season <> '*' .AND. STYLE.Season <> ORDHDR.Season

      *** Message : "Style / ð confilct!                               "
      ***           "                        < Ok >                    "
      =gfModalGen('TRM42086B00000','DIALOG' , loFormSet_lcStyleTtl + LANG_AutoAlloc_MsgSeason)
      lcStyle = lcOldVal          && Restore the old value
    ENDIF    && End of IF
  ENDIF    && End of IF

  SELECT (lcOldAlias)

  =SEEK(lcStyle , 'STYLE')

  *IF the Style was changed and the new Style is not assigned to the Warehouse
  IF lcStyle <> lcOldVal .AND. !SEEK(lcStyle + lcWareCode + SPACE(10) , 'STYDYE')
    *IF Statment to check if the user is going to add the Style to the
    *Warehouse
    *** Message : "ð is not assigned to warehouse ð. "
    ***           "     < Yes >           < No >     "
    IF gfModalGen('TRM42001B00006','DIALOG' , loFormSet_lcStyleTtl + ' ' + ALLTRIM(lcStyle) + '|' + ALLTRIM(lcWareCode) + LANG_AutoAlloc_MsgAskAssign) = 1
      =gpAdStyWar(lcStyle , SPACE(10) , lcWareCode)
    ENDIF    && End of IF
  ENDIF    && End of IF

  =SEEK(lcStyle + lcWareCode + SPACE(10) , 'STYDYE')


  *IF the Style was changed
  IF lcStyle <> lcOldVal
    *osa
    =lfShowGets(.F.,loFormSet)
    *osa
  ENDIF    && End of IF

  =SEEK(lcStyle, 'STYLE')
  loFormSet_AriaForm1.cntDetail.keyStyle.VALUE = lcStyle
  loFormSet_AriaForm1.cntDetail.txtDesc.VALUE  = STYLE.Desc1
  =lfAllocate(3 , RECNO(loFormSet_lcTmpOrdLn),loFormSet)

  *!*************************************************************
  *! Name      : lfGScalCnt
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to Get the number of sizes for a specific Style
  *!*************************************************************
  *! Called from : lfvStyle()
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters  :  The Style we want to get the number of sizes for
  *!*************************************************************
  *! Return      : The number of sizes for the specific Style
  *!*************************************************************
  *
FUNCTION lfGScalCnt
  PARAMETERS lcParm

  PRIVATE lcScale
  =SEEK(lcParm , 'STYLE')
  lcScale = STYLE.SCALE                 && Variable to hold the Scale code of the Style we want to get the number of sizes for
  =SEEK('S' + lcScale , 'SCALE')

  RETURN SCALE.CNT

  *!*************************************************************
  *! Name      : lfvWareCode
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Warehouse
  *!*************************************************************
  *! Called from :  Screen ALAUTAL [lcAutAlCh3 - Warehouse field]
  *!*************************************************************
  *! Calls       : gfBrowWare() , gpAdStyWar() , lfShowGets() , lfRefresh()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvWareCode
  PARAMETERS loFormSet

  PRIVATE lcOldWare

  lnWareCode = loFormSet_AriaForm1.cntDetail.cboLocation.VALUE
  lnOldVal   = loFormSet_AriaForm1.cntDetail.cboLocation.oldValue
  lcStyle    = EVALUATE(loFormSet_lcTmpOrdLn+'.Style')
  lcDyelot   = EVALUATE(loFormSet_lcTmpOrdLn+'.Dyelot')

  lcOldWare  = loFormSet_laWareHous[lnOldVal,2]
  lcWareCode = loFormSet_laWareHous[lnWareCode,2]

  *IF the Warehouse field was empty or not changed
  IF (EMPTY(lcWareCode) .OR. lcWareCode == lcOldWare)
    lcWareCode = lcOldWare
    lnWareCode = lnOldVal
    loFormSet_AriaForm1.cntDetail.cboLocation.VALUE = lnWareCode
    RETURN 0
  ENDIF    && End of IF

  *IF the Warehouse was changed and the configuration is not assigned to the new
  *Warehouse
  IF loFormSet_llUseConfg AND !SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE') .AND. lcWareCode <> lcOldWare

    *** Message : "Configuration [] is not assigned to warehouse []. Cannot change. "
    ***           "   <OK>   "
    lcWareDesc = loFormSet_laWareHous[lnWareCode,1]
    =gfModalGen('TRM42001B00000','DIALOG' , LANG_AutoAlloc_LabelReasCnfgNtAssgn1 + ALLTRIM(lcDyelot) + '|' + lcWareDesc + LANG_AutoAlloc_CannotChange)
    loFormSet_AriaForm1.cntDetail.cboLocation.VALUE = lnOldVal
    loFormSet_AriaForm1.cntDetail.cboLocation.REQUERY()
    RETURN 0
  ENDIF    && End of IF


  IF !SEEK(lcStyle + lcWareCode + SPACE(10) , 'STYDYE') .AND. lcWareCode <> lcOldWare

    *IF Statment to check if the user is going to add the Style to the
    *Warehouse
    *** Message : "ð is not assigned to warehouse ð. "
    ***           "     < Yes >           < No >     "
    IF gfModalGen('TRM42001B00006','DIALOG' , loFormSet_lcStyleTtl + ' ' + ALLTRIM(lcStyle) + '|' + ALLTRIM(lcWareCode) + LANG_AutoAlloc_MsgAskAssign) = 1
      =gpAdStyWar(lcStyle , SPACE(10) , lcWareCode)
    ENDIF    && End of IF
  ENDIF    && End of IF


  IF SEEK(lcStyle + lcOldWare + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')) , 0) ,;
      Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) , 0) ,;
      Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) , 0) ,;
      Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) , 0) ,;
      Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) , 0) ,;
      Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) , 0) ,;
      Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) , 0) ,;
      Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) , 0) ,;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    * MAH
    loAddUserInfo.DO('STYDYE',.NULL.)
    * MAH
    UNLOCK
  ENDIF    && End of IF
  IF SEEK(lcStyle + lcOldWare + lcDyelot  , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')) , 0) ,;
      Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) , 0) ,;
      Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) , 0) ,;
      Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) , 0) ,;
      Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) , 0) ,;
      Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) , 0) ,;
      Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) , 0) ,;
      Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) , 0) ,;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    * MAH
    loAddUserInfo.DO('STYDYE',.NULL.)
    * MAH
    UNLOCK
  ENDIF    && End of

  SELECT (loFormSet_lcTmpOrdLn)
  REPLACE cWareCode WITH lcWareCode
  lcCurDye = lcDyelot
  IF SEEK(lcStyle + lcWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')) , 0) ,;
      Ord2 WITH MAX((Ord2 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) , 0) ,;
      Ord3 WITH MAX((Ord3 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) , 0) ,;
      Ord4 WITH MAX((Ord4 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) , 0) ,;
      Ord5 WITH MAX((Ord5 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) , 0) ,;
      Ord6 WITH MAX((Ord6 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) , 0) ,;
      Ord7 WITH MAX((Ord7 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) , 0) ,;
      Ord8 WITH MAX((Ord8 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) , 0) ,;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    lcCurDye = SPACE(10)
    * MAH
    loAddUserInfo.DO('STYDYE',.NULL.)
    * MAH
    UNLOCK
  ENDIF

  IF !EMPTY(lcDyelot) AND SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')) , 0) ,;
      Ord2 WITH MAX((Ord2 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) , 0) ,;
      Ord3 WITH MAX((Ord3 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) , 0) ,;
      Ord4 WITH MAX((Ord4 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) , 0) ,;
      Ord5 WITH MAX((Ord5 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) , 0) ,;
      Ord6 WITH MAX((Ord6 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) , 0) ,;
      Ord7 WITH MAX((Ord7 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) , 0) ,;
      Ord8 WITH MAX((Ord8 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) , 0) ,;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    lcCurDye = STYDYE.Dyelot
    * MAH
    loAddUserInfo.DO('STYDYE',.NULL.)
    * MAH
    UNLOCK
  ENDIF

  =SEEK(lcStyle + lcWareCode + lcCurDye, 'STYDYE')
  SELECT (loFormSet_lcTmpOrdLn)
  REPLACE DYELOT WITH lcCurDye
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE Avl&lcI WITH STYDYE.Stk&lcI
  ENDFOR
  REPLACE TotAvl WITH STYDYE.TotStk


  *IF the Warehouse was changed
  IF lcWareCode <> lcOldWare
    *osa
    =lfShowGets(.F.,loFormSet)
    *osa
  ENDIF    && End of IF

  loFormSet_AriaForm1.cntDetail.cboLocation.VALUE = lnWareCode
  =lfAllocate(3 , RECNO(loFormSet_lcTmpOrdLn),loFormSet)
  *=lfBundFlds(loFormSet)

  *!*************************************************************
  *! Name      : lfvDyelot
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Dyelot
  *!*************************************************************
  *! Called from :  Screen ALAUTAL [lcAutAlCh3 - Dyelot field]
  *!*************************************************************
  *! Calls       : lfShowGets() , lfRefresh() , SDyeBrow()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvDyelot
  PARAMETERS llBrowse,loFormSet

  PRIVATE llObjRet , lcOldAlias

  lcDyelot = PADR(loFormSet_AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE ,10)
  lcOldVal = PADR(loFormSet_AriaForm1.cntDetail.keyDyelot.keytextbox.oldvalue ,10)
  lcStyle  = EVALUATE(loFormSet_lcTmpOrdLn+'.Style')
  lcWareCode = EVALUATE(loFormSet_lcTmpOrdLn+'.cWareCode')

  *IF the Dyelot field was empty or not changed and the user dose not want to
  *Browse
  IF (EMPTY(lcDyelot) .OR. ALLTRIM(lcDyelot) == ALLTRIM(lcOldVal)) .AND. !llBrowse
    *IF The Dyelot field was Changed [if the User removed the Dyelot]
    IF lcDyelot <> lcOldVal
      *osa
      =lfShowGets(.F.,loFormSet)
      *osa
    ENDIF    && End of IF
    loFormSet_AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE = lcDyelot
    RETURN
  ENDIF    && End of IF

  lcOldAlias = ALIAS()               && Variable to save the old Alis

  *IF The user want to Browse or if the Dyelot he entered is not in the file
  IF '?' $ lcDyelot .OR. llBrowse .OR. !SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')
    llObjRet = SDyeBrow(lcStyle , @lcDyelot , .F. , lcWareCode, .T., .T.,.F.,.F.,loFormSet_lluseconfg)
    lcDyelot = IIF(llObjRet,lcDyelot,lcOldVal)
    llBrowse = .F.
  ENDIF    && End of IF

  SELECT (lcOldAlias)

  =SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')
  SELECT (loFormSet_lcTmpOrdLn)
  REPLACE Dyelot WITH lcDyelot
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE Avl&lcI WITH STYDYE.Stk&lcI
  ENDFOR
  REPLACE TotAvl WITH STYDYE.TotStk

  IF SEEK(lcStyle + lcWareCode + lcOldVal , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')) , 0) ,;
      Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) , 0) ,;
      Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) , 0) ,;
      Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) , 0) ,;
      Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) , 0) ,;
      Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) , 0) ,;
      Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) , 0) ,;
      Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) , 0) ,;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    * MAH
    loAddUserInfo.DO('STYDYE',.NULL.)
    * MAH
    UNLOCK
  ENDIF

  IF SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')) , 0) ,;
      Ord2 WITH MAX((Ord2 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) , 0) ,;
      Ord3 WITH MAX((Ord3 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) , 0) ,;
      Ord4 WITH MAX((Ord4 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) , 0) ,;
      Ord5 WITH MAX((Ord5 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) , 0) ,;
      Ord6 WITH MAX((Ord6 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) , 0) ,;
      Ord7 WITH MAX((Ord7 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) , 0) ,;
      Ord8 WITH MAX((Ord8 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) , 0) ,;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    * MAH
    loAddUserInfo.DO('STYDYE',.NULL.)
    * MAH
    UNLOCK
  ENDIF


  *IF the Dyelot was changed
  IF lcDyelot <> lcOldVal
    *osa
    =lfShowGets(.F.,loFormSet)
    *osa
  ENDIF    && End of IF

  loFormSet_AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE = lcDyelot
  =lfAllocate(3 , RECNO(loFormSet_lcTmpOrdLn),loFormSet)
  *=lfBundFlds(loFormSet)
  *!*************************************************************
  *! Name      : lfvAloQty
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Allocated Get fields
  *!*************************************************************
  *! Called from :  Screen ALAUTAL [lcAutAlCh3 - Allocated Get fields]
  *!*************************************************************
  *! Calls       : gfModalGen() , lfRefresh()
  *!*************************************************************
  *! Passed Parameters : [The number of the Allocated Get field to Valid]
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvAloQty
  PARAMETERS lnParm,loFormSet

  *-- MAB 09/01/1999 Check Stock quantity (Before Force allocation) [Begin]
  PRIVATE lcParm,lnAvalStk
  lcParm = ALLTRIM(STR(lnParm))

  lnQty    = loFormSet_AriaForm1.cntDetail.txtOrd&lcParm..VALUE
  lnTotQty = loFormSet_AriaForm1.cntDetail.txtTotalOrd.VALUE
  lnPik    = loFormSet_AriaForm1.cntDetail.txtPik&lcParm..VALUE
  lnTotPik = loFormSet_AriaForm1.cntDetail.txtTotalPik.VALUE
  lcOldVal = loFormSet_AriaForm1.cntDetail.txtPik&lcParm..OldValue

  IF lnPik = lcOldVal
    RETURN
  ENDIF

  *IF the Allocated quantity is less than 0
  IF lnPik < 0
    *-- MAB 01/09/1999 Force user to enter positive values. [Begin]
    *Message : 44081 ==> A negative value is not allowed.
    *Button  : 00000 ==> < Ok >
    =gfModalGen('INM44081B00000', 'DIALOG')
    lnPik = lcOldVal
    loFormSet_AriaForm1.cntDetail.txtPik&lcParm..VALUE = lnPik
    RETURN
    *-- MAB 01/09/1999 Force user to enter positive values. [End  ]
  ENDIF    && End of IF
  lcPiktkt = EVALUATE(loFormSet_lcTmpOrdLn+'.PikTkt')
  lcPiktkt = IIF(lcPiktkt='******'," ",lcPiktkt)
  IF !loFormSet_llOpnPack  AND !USED('PACK_HDR')
    lcAlsNow  = SELECT(0)
    loFormSet_llOpnPack  = oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'PACK_HDR',oAriaEnvironment.DataDir+'PACK_HDR','SH')
    SELECT (lcAlsNow)
  ENDIF
  IF !EMPTY(lcPiktkt) AND SEEK(lcPiktkt,'PIKTKT') AND PikTkt.STATUS = 'O' AND SEEK(lcPiktkt,'PACK_HDR')
    =gfModalGen("INM44060B00000" , "DIALOG" , PADR(lcPiktkt,6))
    loFormSet_AriaForm1.cntDetail.txtPik&lcParm..VALUE = lcOldVal
    RETURN
  ENDIF

  lnAvalStk = IIF(EOF('STYLE') , 0 , MAX(STYDYE.Stk&lcParm - STYDYE.Alo&lcParm, 0))
  IF (lnPik > lnAvalStk) AND !lfForceMe(loFormSet)
    lnPik = lcOldVal
    loFormSet_AriaForm1.cntDetail.txtPik&lcParm..VALUE = lnPik
    RETURN
  ENDIF

  *IF the Allocated quantity is greater than the Ordered quantity
  IF lnPik > lnQty
    *IF Statment to check if the user is going to Increase the ordered
    *quantity
    *** Message : "Quantity allocated is greater than ordered. Increase "
    ***           "ordered quantity?                                    "
    ***           "             < Yes >   <Yes All>   < No >            "
    IF gfModalGen('TRM44002B00006','DIALOG') = 1
      lnTotQty = lnTotQty - lnQty + lnPik
      lnQty    = lnPik
      loFormSet_llIncOrd = .T.
    ELSE    && Else
      lnPik = lcOldVal
    ENDIF    && End of IF
  ENDIF    && End of IF

  *IF the Allocated quantity was changed
  IF lnPik <> lcOldVal
    loFormSet_lnChangAlo = IIF(lnPik = EVALUATE(loFormSet_lcTmpOrdLn+'.Pik'+lcParm) , loFormSet_lnChangAlo - 1 ,;
      IIF(lcOldVal = EVALUATE(loFormSet_lcTmpOrdLn+'.Pik'+lcParm) , loFormSet_lnChangAlo + 1 ,;
      loFormSet_lnChangAlo))
    lnTotPik = lnTotPik + lnPik - lcOldVal
  ENDIF    && End of IF

  loFormSet_AriaForm1.cntDetail.txtOrd&lcParm..VALUE = lnQty
  loFormSet_AriaForm1.cntDetail.txtTotalOrd.VALUE    = lnTotQty
  loFormSet_AriaForm1.cntDetail.txtPik&lcParm..VALUE = lnPik
  loFormSet_AriaForm1.cntDetail.txtTotalPik.VALUE    = lnTotPik
  SELECT (loFormSet_lcTmpOrdLn)
  REPLACE TotAlo	   WITH TotAlo - Alo&lcParm,;
    TotQty     WITH TotQty - Qty&lcParm
  REPLACE Alo&lcParm WITH loFormSet_AriaForm1.cntDetail.txtPik&lcParm..VALUE,;
    TotAlo	   WITH TotAlo + Alo&lcParm,;
    Qty&lcParm WITH loFormSet_AriaForm1.cntDetail.txtOrd&lcParm..VALUE,;
    TotQty     WITH TotQty + Qty&lcParm

  =lfAllocate(3 , RECNO(loFormSet_lcTmpOrdLn),loFormSet)
  *=lfBundFlds(loFormSet)

  *!*************************************************************
  *! Name      : lfForceMe
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Force allocation. (Manual Force)
  *!*************************************************************
FUNCTION lfForceMe
  PARAMETERS loFormSet

  IF loFormSet_llForceAll
    RETURN
  ENDIF

  PRIVATE lnOption, llRetVal , lcButton , lcMsgType , lcOptMsg

  lnOption = 1

  IF loFormSet_llAlwForce
    lcButton  = "44002"
    lcOptMsg  = LANG_AutoAlloc_MsgAskForce1
    lcMsgType = "QR"
  ELSE
    lcButton = "00000"
    lcOptMsg = " "
    lcMsgType = "TR"
  ENDIF

  lnOption = gfModalGen(lcMsgType+'M44000B'+lcButton, 'DIALOG',;
    LANG_AutoAlloc_MsgAskForce2+;
    "|"+lcOptMsg)

  IF !loFormSet_llAlwForce
    lnOption = 3
  ENDIF

  loFormSet_llForceAll = (lnOption = 2) && Avoiding entering this function again.
  llRetVal   = (lnOption < 3)
  RETURN llRetVal
  *-- end of lfForceMe.

  *!*************************************************************
  *! Name      : lfAllocate
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to Allocate one or more line
  *!*************************************************************
  *! Called from : ALAUTAL.PRG , lfvScope() , lfAloScr() , lfGenScr() ,
  *!               lfBrowTrap()
  *!*************************************************************
  *! Calls       : lfTmpAlo() , lfAloQty() , lfEditLine()
  *!*************************************************************
  *! Passed Parameters : 1) 1 to Allocate with the Option grid conditions
  *!                      , 2 to Allocate the selected records
  *!                      , 3 to edit a record
  *!                     2) The number of the record to edit
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
FUNCTION lfAllocate
  PARAMETERS lnParm , lnRecNo , loFormSet

  PRIVATE lnTotRec , lnCurent , lcForCond , llAloGoup , lcGroupStr ,;
    llGrpFrRec , lnAvlFAlo , lcPikWare , lnRecNumb , llAloSep ,;
    lnOldRec , lnRecCount , lnPrepRec


  lnTotRec = IIF(lnParm = 1 , loFormSet_lnRecNumbr , loFormSet_lnSelRec)     && Varible to hold the Total count to be done for the thermometer
  lnCurent = 0                                             			      && Varible to hold the current count to be done for the thermometer

  SELECT (loFormSet_lcTmpOrdLn)
  *IF Not an incompleted session and not to edit a record
  IF lnParm <> 3
    lnRecCount = RECCOUNT() - loFormSet_lnDellRec        && Varible to hold the Total count to be done for the thermometer
    lnPrepRec = 0                              && Varible to hold the current count to be done for the thermometer

    *osa
    *loFormSet_oPross.lblFirstLabel.Caption = LANG_AutoAlloc_LableProgress
    *loFormSet_oPross.TotalProgress = lnRecCount
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    *loFormSet_oPross.AutoCenter = .T.
    *HBG [End]
    *loFormSet_oPross.Show()
    *osa

    *SCAN Loop to scan the temp. Order lines file
    SCAN
      lnPrepRec = lnPrepRec + 1
      REPLACE nProcNo WITH 0

      *osa
      *loFormSet_oPross.CurrentProgress(lnPrepRec)
      *osa

    ENDSCAN    && End of SCAN Loop
  ENDIF    && End of IF

  SET ORDER TO TAG STYDYE IN STYDYE
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET ORDER TO TAG STYLE IN STYLE
  SET ORDER TO TAG ORDHDR IN ORDHDR
  SELECT (loFormSet_lcTmpOrdLn)
  SET ORDER TO TAG (loFormSet_lcTmpOrdLn)

  *IF Not to edit a record
  IF lnParm <> 3
    SET RELATION TO STYLE INTO STYLE
  ELSE    && Else
    SET RELATION TO
  ENDIF    && End of IF

  *DO CASE Statment
  DO CASE

      *CASE of Allocate with the Option grid conditions
    CASE (lnParm = 1) OR (lnParm = 2)
      *-- if include wip and select by C/T or P/O and we have C/Ts or P/Os.
      IF loFormSet_lcRpIncWip $ 'AS' AND loFormSet_lcRpScpMod $ 'KP' AND !EMPTY(loFormSet_laincexprs[1,6])
        lnI = 0
        DIMENSION loFormSet_laString1[1]
        STORE "" TO loFormSet_laString1
        SELECT (loFormSet_laincexprs[1,6])
        SCAN
          lnI = lnI + 1
          DIMENSION loFormSet_laString1[lnI]
          loFormSet_laString1[lnI] = PO
        ENDSCAN
      ENDIF

      *-- if include wip and exclude select by C/T or P/O and we have C/Ts or P/Os.
      IF loFormSet_lcRpIncWip $ 'AS' AND loFormSet_llExclude AND (loFormSet_lcRpExSlct $ 'KP') AND !EMPTY(loFormSet_lafiltexp[1,6])
        lnI = 0
        DIMENSION loFormSet_laString2[1]
        STORE "" TO loFormSet_laString2
        SELECT (loFormSet_lafiltexp[1,6])
        SCAN
          lnI = lnI + 1
          DIMENSION loFormSet_laString2[lnI]
          loFormSet_laString2[lnI] = PO
        ENDSCAN
      ENDIF

      SELECT (loFormSet_lcTmpOrdLn)
      IF loFormSet_llUseDyes AND (loFormSet_llFabDye OR loFormSet_llUseConfg) AND !loFormSet_llRpExlDye
        lcOldGroup = ''
        = lfDyeAlo(lnParm,loFormSet)
      ELSE  && else No dyelot allocation.
        = lfNormAlo(lnParm,loFormSet)
      ENDIF


      *CASE of edit a record
    CASE .F. && lnParm = 3
      SELECT (loFormSet_lcTmpOrdLn)
      lnOldRec = RECNO()        && Variable to save the record number

      IF BETWEEN(lnRecNo,1,RECCOUNT())
        GO lnRecNo
      ENDIF

      *IF Not an incompleted session
      REPLACE nProcNo WITH 0
      =lfEditLine(loFormSet)

      IF BETWEEN(lnOldRec,1,RECCOUNT())
        GO lnOldRec
      ENDIF

      SCATTER FIELDS LIKE QTY* TO laQty
      FOR lnI = 1 TO SCALE.CNT
        lcI = STR(lnI,1)
        loFormSet_AriaForm1.cntDetail.txtord&lcI..VALUE = laQty[lnI]
      ENDFOR
      loFormSet_AriaForm1.cntDetail.txttotalOrd.VALUE = EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty')
  ENDCASE    && End of DO CASE Statment

  *osa
  *-- Handle status of tool bar and option menu
  *=lfHandlObj(loFormSet)
  *osa

  SELECT (loFormSet_lcTmpOrdLn)
  SET RELATION TO

  *-- end of lfAllocate.

  *!*************************************************************
  *! Name      : lfEditLine
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to update the needed files when the current
  *!             record of the temp. Order lines file is Edited from
  *!             the screen [lcAutAlCh3]
  *!*************************************************************
  *! Called from : lfAllocate()
  *!*************************************************************
  *! Calls       : gfAdd_Info()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfEditLine
  PARAMETERS loFormSet
  PRIVATE lcAltStyle , lcOldWare , lnCnT1 , lnCnT2 , lcOScale ,;
    lnCanc , lcOldWare , lnElm , lcElm

  lnCnT1 = 8                  && Variable to hold the number of sizes for the current Style
  lnCnT2 = 8                  && Variable to hold the number of sizes for the Original Style
  lcOScale = ''               && Variable to hold the Scale code of the Original Style
  DIMENSION laPik[8],laQty[8]
  STORE 0 TO laPik,laQty
  IF EMPTY(loFormSet_AriaForm1.cntDetail.KeyStyle.VALUE)
    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      laQty[lnI] = loFormSet_AriaForm1.cntDetail.txtord&lcI..VALUE
      laPik[lnI] = loFormSet_AriaForm1.cntDetail.txtPik&lcI..VALUE
    ENDFOR
    lnTotQty   = loFormSet_AriaForm1.cntDetail.txttotalOrd.VALUE
    lnTotPik   = loFormSet_AriaForm1.cntDetail.txttotalPik.VALUE
    lcStyle    = loFormSet_AriaForm1.cntDetail.KeyStyle.VALUE
    lnWareCode = loFormSet_AriaForm1.cntDetail.cboLocation.VALUE
    lcWareCode = loFormSet_laWareHous[lnWareCode,2]
    lcDyeLot   = loFormSet_AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE
  ELSE
    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      laQty[lnI] = EVALUATE(loFormSet_lcTmpordln+'.Qty'+lcI)
      laPik[lnI] = EVALUATE(loFormSet_lcTmpordln+'.Alo'+lcI)
    ENDFOR
    lnTotQty   = EVALUATE(loFormSet_lcTmpordln+'.TotQty')
    lnTotPik   = EVALUATE(loFormSet_lcTmpordln+'.TotAlo')
    lcStyle    = EVALUATE(loFormSet_lcTmpordln+'.Style')
    lcDyeLot   = EVALUATE(loFormSet_lcTmpordln+'.Dyelot')
    lnWareCode = ASCAN(loFormSet_laWareHous,EVALUATE(loFormSet_lcTmpordln+'.cWareCode'))
    lnWareCode = ASUBSCRIPT(loFormSet_laWareHous, lnWareCode , 1)
    lcWareCode = loFormSet_laWareHous[lnWareCode,2]
  ENDIF

  *IF nProcNo [The step number (for the rollback)] equal 0
  IF nProcNo = 0
    REPLACE PoAlo1    WITH laPik[1] - Pik1 ,;
      PoAlo2    WITH laPik[2] - Pik2 ,;
      PoAlo3    WITH laPik[3] - Pik3 ,;
      PoAlo4    WITH laPik[4] - Pik4 ,;
      PoAlo5    WITH laPik[5] - Pik5 ,;
      PoAlo6    WITH laPik[6] - Pik6 ,;
      PoAlo7    WITH laPik[7] - Pik7 ,;
      PoAlo8    WITH laPik[8] - Pik8 ,;
      Tot_PoAlo WITH lnTotPik - TotPik ,;
      Book1     WITH laQty[1] ,;
      Book2     WITH laQty[2] ,;
      Book3     WITH laQty[3] ,;
      Book4     WITH laQty[4] ,;
      Book5     WITH laQty[5] ,;
      Book6     WITH laQty[6] ,;
      Book7     WITH laQty[7] ,;
      Book8     WITH laQty[8] ,;
      TotBook   WITH lnTotQty ,;
      lLok_Stat WITH EMPTY(AltStyle) ,;
      AltStyle  WITH IIF(lcStyle = STYLE , AltStyle , STYLE) ,;
      SCALE     WITH IIF(lcStyle = STYLE , SCALE , loFormSet_lcStyScale) ,;
      STYLE     WITH lcStyle ,;
      DyeLot    WITH lcDyeLot ,;
      Season    WITH cWareCode ,;
      cWareCode WITH lcWareCode ,;
      nProcNo   WITH 1

    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  lcAltStyle = IIF(EMPTY(AltStyle) .OR. !lLok_Stat , STYLE , AltStyle)    && Variable to hold the original Style


  *IF This line Style was substituted befor and the line is to be released
  IF !lLok_Stat .AND. Tot_PoAlo + TotPik = 0
    =SEEK(STYLE , 'STYLE')
    =SEEK( 'S'+STYLE.SCALE ,'SCALE')
    lnCnT1 = SCALE.CNT
    =SEEK(AltStyle , 'STYLE')
    =SEEK( 'S'+STYLE.SCALE ,'SCALE')
    lnCnT2 = SCALE.CNT
    lcOScale = SCALE.SCALE
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 1
  IF nProcNo = 1

    *IF There is a record for the current Style and Warehouse in the STYDYE
    *file
    IF SEEK(STYLE + cWareCode + SPACE(10) , 'STYDYE')
      SELECT STYDYE
      = RLOCK()
      REPLACE Alo1   WITH Alo1   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo1') ,;
        Alo2   WITH Alo2   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo2') ,;
        Alo3   WITH Alo3   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo3') ,;
        Alo4   WITH Alo4   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo4') ,;
        Alo5   WITH Alo5   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo5') ,;
        Alo6   WITH Alo6   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo6') ,;
        Alo7   WITH Alo7   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo7') ,;
        Alo8   WITH Alo8   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo8') ,;
        TotAlo WITH TotAlo + EVALUATE(loFormSet_lcTmpOrdLn+'.Tot_PoAlo')
      * MAH
      loAddUserInfo.DO('STYDYE',.NULL.)
      * MAH
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 2
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 2
  IF nProcNo = 2

    *IF There is a record for the cuurent Style and Warehouse and Dyelot in
    *the STYDYE file
    IF !EMPTY(DyeLot) .AND. SEEK(STYLE + cWareCode + DyeLot , 'STYDYE')
      SELECT STYDYE
      = RLOCK()
      REPLACE Alo1   WITH Alo1   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo1') ,;
        Alo2   WITH Alo2   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo2') ,;
        Alo3   WITH Alo3   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo3') ,;
        Alo4   WITH Alo4   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo4') ,;
        Alo5   WITH Alo5   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo5') ,;
        Alo6   WITH Alo6   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo6') ,;
        Alo7   WITH Alo7   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo7') ,;
        Alo8   WITH Alo8   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo8') ,;
        TotAlo WITH TotAlo + EVALUATE(loFormSet_lcTmpOrdLn+'.Tot_PoAlo')
      * MAH
      loAddUserInfo.DO('STYDYE',.NULL.)
      * MAH
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 3
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 3
  IF nProcNo = 3

    *IF There is a record for the current Style in the STYLE file
    IF SEEK(STYLE , 'STYLE')
      SELECT STYLE
      = RLOCK()
      REPLACE Alo1   WITH Alo1   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo1') ,;
        Alo2   WITH Alo2   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo2') ,;
        Alo3   WITH Alo3   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo3') ,;
        Alo4   WITH Alo4   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo4') ,;
        Alo5   WITH Alo5   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo5') ,;
        Alo6   WITH Alo6   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo6') ,;
        Alo7   WITH Alo7   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo7') ,;
        Alo8   WITH Alo8   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo8') ,;
        TotAlo WITH TotAlo + EVALUATE(loFormSet_lcTmpOrdLn+'.Tot_PoAlo')
      * MAH
      loAddUserInfo.DO('STYDYE',.NULL.)
      * MAH
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 4
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 4
  IF nProcNo = 4

    *IF There is a record for the old Style and Warehouse in the STYDYE
    *file
    IF SEEK(lcAltStyle + cWareCode + SPACE(10) , 'STYDYE')
      SELECT STYDYE
      = RLOCK()
      REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')) , 0) ,;
        Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) , 0) ,;
        Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) , 0) ,;
        Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) , 0) ,;
        Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) , 0) ,;
        Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) , 0) ,;
        Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) , 0) ,;
        Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) , 0) ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
      * MAH
      loAddUserInfo.DO('STYDYE',.NULL.)
      * MAH
      UNLOCK
    ENDIF    && End of IF

    IF !EMPTY(Dyelot) AND SEEK(lcAltStyle + cWareCode + Dyelot , 'STYDYE')
      SELECT STYDYE
      = RLOCK()
      REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')) , 0) ,;
        Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) , 0) ,;
        Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) , 0) ,;
        Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) , 0) ,;
        Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) , 0) ,;
        Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) , 0) ,;
        Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) , 0) ,;
        Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) , 0) ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

      * MAH
      loAddUserInfo.DO('STYDYE',.NULL.)
      * MAH
      UNLOCK
    ENDIF    && End of IF

    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 5
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 5
  IF nProcNo = 5
    *IF There is a record for the current Style and Warehouse in the STYDYE
    *file
    IF SEEK(STYLE + cWareCode + SPACE(10) , 'STYDYE')
      SELECT STYDYE
      = RLOCK()
      REPLACE Ord1 WITH (Ord1 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')),;
        Ord2 WITH (Ord2 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')),;
        Ord3 WITH (Ord3 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')),;
        Ord4 WITH (Ord4 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')),;
        Ord5 WITH (Ord5 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')),;
        Ord6 WITH (Ord6 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')),;
        Ord7 WITH (Ord7 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')),;
        Ord8 WITH (Ord8 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')),;
        TotOrd WITH (TotOrd + EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty'))

      * MAH
      loAddUserInfo.DO('STYDYE',.NULL.)
      * MAH
      UNLOCK
    ENDIF    && End of IF

    IF !EMPTY(Dyelot) AND SEEK(STYLE + cWareCode + Dyelot , 'STYDYE')
      SELECT STYDYE
      = RLOCK()
      REPLACE Ord1 WITH (Ord1 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')),;
        Ord2 WITH (Ord2 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')),;
        Ord3 WITH (Ord3 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')),;
        Ord4 WITH (Ord4 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')),;
        Ord5 WITH (Ord5 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')),;
        Ord6 WITH (Ord6 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')),;
        Ord7 WITH (Ord7 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')),;
        Ord8 WITH (Ord8 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')),;
        TotOrd WITH (TotOrd + EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty'))

      =loAddUserInfo.DO('STYDYE',.NULL.)
      UNLOCK
    ENDIF    && End of IF

    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 6
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 6
  IF nProcNo = 6

    *IF There is a record for the old Style in the STYLE file
    IF SEEK(lcAltStyle , 'STYLE')
      SELECT STYLE
      = RLOCK()
      REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')) , 0) ,;
        Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) , 0) ,;
        Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) , 0) ,;
        Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) , 0) ,;
        Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) , 0) ,;
        Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) , 0) ,;
        Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) , 0) ,;
        Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) , 0) ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

      =loAddUserInfo.DO('STYLE',.NULL.)
      UNLOCK

    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 7
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 7
  IF nProcNo = 7

    *IF There is a record for the current Style in the STYLE file
    IF SEEK(STYLE , 'STYLE')
      SELECT STYLE
      = RLOCK()
      REPLACE Ord1 WITH (Ord1 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')),;
        Ord2 WITH (Ord2 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')),;
        Ord3 WITH (Ord3 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')),;
        Ord4 WITH (Ord4 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')),;
        Ord5 WITH (Ord5 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')),;
        Ord6 WITH (Ord6 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')),;
        Ord7 WITH (Ord7 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')),;
        Ord8 WITH (Ord8 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')),;
        TotOrd WITH (TotOrd + EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty'))

      =loAddUserInfo.DO('STYLE',.NULL.)
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 8
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 8
  IF nProcNo = 8

    *IF We will need to update the Order Header
    IF lnCnT1 > lnCnT2 .OR. TotBook <> TotQty
      lnCanc = 0          && Varible to hold the canceled quantity

      *FOR Loop to get the  canceled quantity and amount
      IF lnCnT1 > lnCnT2
        FOR lnElm = lnCnT2 + 1 TO lnCnT1
          lcElm = STR(lnElm , 1)
          lnCanc = lnCanc + Book&lcElm
        ENDFOR    && End of FOR Loop
      ENDIF    && End of IF
      SELECT ORDHDR

      *IF There is a record for this Order line in the ORDHDR file
      IF SEEK('O' + EVALUATE(loFormSet_lcTmpOrdLn+'.Order'))
        =RLOCK()
        REPLACE OPEN      WITH OPEN + EVALUATE(loFormSet_lcTmpOrdLn+'.TotBook') - EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty') - lnCanc ,;
          OpenAmt   WITH OpenAmt + ((EVALUATE(loFormSet_lcTmpOrdLn+'.TotBook') - EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty') - lnCanc) * EVALUATE(loFormSet_lcTmpOrdLn+'.Price')) ,;
          BookAmt   WITH BookAmt + ((EVALUATE(loFormSet_lcTmpOrdLn+'.TotBook') - EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty') - lnCanc) * EVALUATE(loFormSet_lcTmpOrdLn+'.Price')) ,;
          Book      WITH Book + EVALUATE(loFormSet_lcTmpOrdLn+'.TotBook') - EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty') - lnCanc

        =loAddUserInfo.DO('ORDHDR',.NULL.)
        UNLOCK
      ENDIF

    ENDIF    && End of IF

    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 9
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 9
  IF nProcNo = 9

    *IF There is a record for this Order line in the ORDLINE file
    IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
      SELECT ORDLINE
      = RLOCK()
      REPLACE Pik1     WITH Pik1 + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo1') ,;
        Pik2     WITH Pik2 + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo2') ,;
        Pik3     WITH Pik3 + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo3') ,;
        Pik4     WITH Pik4 + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo4') ,;
        Pik5     WITH Pik5 + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo5') ,;
        Pik6     WITH Pik6 + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo6') ,;
        Pik7     WITH Pik7 + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo7') ,;
        Pik8     WITH Pik8 + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo8') ,;
        TotPik   WITH TotPik + EVALUATE(loFormSet_lcTmpOrdLn+'.Tot_PoAlo') ,;
        PikDate  WITH IIF(TotPik = 0 , {} , oAriaEnvironment.SystemDate) ,;
        PikTkt   WITH IIF(TotPik = 0 , '' ,;
        IIF(EMPTY(PikTkt),'******',PikTkt)) ,;
        Picked   WITH IIF(TotPik = 0 , .F. , .T.)

      =loAddUserInfo.DO('ORDLINE',.NULL.)
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 10
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 10
  IF nProcNo = 10

    *IF There is a record for this Order line in the ORDLINE file
    IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
      SELECT ORDLINE
      = RLOCK()
      REPLACE Book1  WITH Book1 + EVALUATE(loFormSet_lcTmpOrdLn+'.Book1') - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1') ,;
        Book2  WITH IIF(lnCnT2 < 2 , 0 , Book2 + EVALUATE(loFormSet_lcTmpOrdLn+'.Book2') - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) ,;
        Book3  WITH IIF(lnCnT2 < 3 , 0 , Book3 + EVALUATE(loFormSet_lcTmpOrdLn+'.Book3') - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) ,;
        Book4  WITH IIF(lnCnT2 < 4 , 0 , Book4 + EVALUATE(loFormSet_lcTmpOrdLn+'.Book4') - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) ,;
        Book5  WITH IIF(lnCnT2 < 5 , 0 , Book5 + EVALUATE(loFormSet_lcTmpOrdLn+'.Book5') - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) ,;
        Book6  WITH IIF(lnCnT2 < 6 , 0 , Book6 + EVALUATE(loFormSet_lcTmpOrdLn+'.Book6') - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) ,;
        Book7  WITH IIF(lnCnT2 < 7 , 0 , Book7 + EVALUATE(loFormSet_lcTmpOrdLn+'.Book7') - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) ,;
        Book8  WITH IIF(lnCnT2 < 8 , 0 , Book8 + EVALUATE(loFormSet_lcTmpOrdLn+'.Book8') - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) ,;
        TotBook WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8

      =loAddUserInfo.DO('ORDLINE',.NULL.)
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 11
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF


  *IF nProcNo [The step number (for the rollback)] equal 11
  IF nProcNo = 11

    *IF There is a record for this Order line in the ORDLINE file
    IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
      SELECT ORDLINE
      = RLOCK()

      REPLACE QTY1      WITH EVALUATE(loFormSet_lcTmpOrdLn+'.Book1') ,;
        Qty2      WITH IIF(lnCnT2 < 2 , 0 , EVALUATE(loFormSet_lcTmpOrdLn+'.Book2')) ,;
        Qty3      WITH IIF(lnCnT2 < 3 , 0 , EVALUATE(loFormSet_lcTmpOrdLn+'.Book3')) ,;
        Qty4      WITH IIF(lnCnT2 < 4 , 0 , EVALUATE(loFormSet_lcTmpOrdLn+'.Book4')) ,;
        Qty5      WITH IIF(lnCnT2 < 5 , 0 , EVALUATE(loFormSet_lcTmpOrdLn+'.Book5')) ,;
        Qty6      WITH IIF(lnCnT2 < 6 , 0 , EVALUATE(loFormSet_lcTmpOrdLn+'.Book6')) ,;
        Qty7      WITH IIF(lnCnT2 < 7 , 0 , EVALUATE(loFormSet_lcTmpOrdLn+'.Book7')) ,;
        Qty8      WITH IIF(lnCnT2 < 8 , 0 , EVALUATE(loFormSet_lcTmpOrdLn+'.Book8')) ,;
        TotQty    WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
        STYLE     WITH IIF(TotPik = 0 .AND. !EVALUATE(loFormSet_lcTmpOrdLn+'.lLok_Stat') , EVALUATE(loFormSet_lcTmpOrdLn+'.AltStyle') , EVALUATE(loFormSet_lcTmpOrdLn+'.Style')),;
        AltStyle  WITH IIF(TotPik = 0 , SPACE(12) , EVALUATE(loFormSet_lcTmpOrdLn+'.AltStyle')) ,;
        DyeLot    WITH EVALUATE(loFormSet_lcTmpOrdLn+'.DyeLot') ,;
        cWareCode WITH EVALUATE(loFormSet_lcTmpOrdLn+'.cWareCode') ,;
        SCALE     WITH IIF(TotPik = 0 .AND. !lLok_Stat , lcOScale , EVALUATE(loFormSet_lcTmpOrdLn+'.Scale'))

      =loAddUserInfo.DO('ORDLINE',.NULL.)
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 12
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 12
  IF nProcNo = 12
    loFormSet_lnSelAlo = IIF(TotPik = 0 .AND. Tot_PoAlo > 0 , loFormSet_lnSelAlo + 1 ,;
      IIF(TotPik > 0 .AND. TotPik + Tot_PoAlo = 0 ,;
      loFormSet_lnSelAlo - 1 , loFormSet_lnSelAlo))

    loFormSet_lnAloRec = IIF(TotPik = 0 .AND. Tot_PoAlo > 0 , loFormSet_lnAloRec + 1 ,;
      IIF(TotPik > 0 .AND. TotPik + Tot_PoAlo = 0 ,;
      loFormSet_lnAloRec - 1 , loFormSet_lnAloRec))

    REPLACE Pik1     WITH Pik1 + PoAlo1 ,;
      Pik2     WITH Pik2 + PoAlo2 ,;
      Pik3     WITH Pik3 + PoAlo3 ,;
      Pik4     WITH Pik4 + PoAlo4 ,;
      Pik5     WITH Pik5 + PoAlo5 ,;
      Pik6     WITH Pik6 + PoAlo6 ,;
      Pik7     WITH Pik7 + PoAlo7 ,;
      Pik8     WITH Pik8 + PoAlo8 ,;
      TotPik   WITH TotPik + Tot_PoAlo ,;
      PikDate  WITH IIF(TotPik = 0 , {} , oAriaEnvironment.SystemDate) ,;
      PikTkt   WITH IIF(TotPik = 0 , '' ,;
      IIF(EMPTY(PikTkt),'******',PikTkt)) ,;
      Picked   WITH IIF(TotPik = 0 , .F. , .T.) ,;
      QTY1     WITH Book1 ,;
      Qty2     WITH IIF(lnCnT2 < 2 , 0 , Book2) ,;
      Qty3     WITH IIF(lnCnT2 < 3 , 0 , Book3) ,;
      Qty4     WITH IIF(lnCnT2 < 4 , 0 , Book4) ,;
      Qty5     WITH IIF(lnCnT2 < 5 , 0 , Book5) ,;
      Qty6     WITH IIF(lnCnT2 < 6 , 0 , Book6) ,;
      Qty7     WITH IIF(lnCnT2 < 7 , 0 , Book7) ,;
      Qty8     WITH IIF(lnCnT2 < 8 , 0 , Book8) ,;
      TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
      STYLE    WITH IIF(TotPik = 0 .AND. !lLok_Stat , AltStyle , STYLE),;
      AltStyle WITH IIF(TotPik = 0 , SPACE(12) , AltStyle) ,;
      SCALE    WITH IIF(TotPik = 0 .AND. !lLok_Stat , lcOScale , SCALE) ,;
      nProcNo  WITH 99
    IF TotPik <> 0
      REPLACE cReason WITH ""
    ENDIF

    =RLOCK()
    UNLOCK

    loFormSet_AriaForm1.cntDetail.KeyStyle.VALUE = STYLE
    lnElem = ASCAN(loFormSet_laWareHous,lcWareCode)
    loFormSet_AriaForm1.cntDetail.cboLocation.VALUE = ASUBSCRIPT(loFormSet_laWareHous, lnElem, 1)
    loFormSet_AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE  = IIF(STYLE.cDye_Flg = 'N' , SPACE(10) , Dyelot)
  ENDIF    && End of IF      ORDHDR
  *-- end of lfEditLine.

  *!*************************************************************
  *! Name      : lfHandlSel
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Handle Select / UnSelect Cases.
  *!*************************************************************
  *! Passed Parameters  : Apply Method
  *!*************************************************************
  *!
FUNCTION lfHandlSel
  PARAMETERS lcApplyMth,loFormSet

  lnRecNo = RECNO()
  STORE 0 TO loFormSet_lnSelRec,loFormSet_lnSelAlo,loFormSet_lnAloRec

  SCAN
    IF lnSel = 1
      loFormSet_lnSelRec = loFormSet_lnSelRec + 1
      IF TOTPIK <> 0
        loFormSet_lnSelAlo = loFormSet_lnSelAlo + 1
      ENDIF
    ENDIF
    IF TOTPIK <> 0
      loFormSet_lnAloRec = loFormSet_lnAloRec + 1
    ENDIF
  ENDSCAN

  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF
  *-- Select / UnSelect Record.
  IF lcApplyMth = "SEL_UNSEL"
    =lfSelUnSel(loFormSet)
  ELSE  && Sel All, Sel None, Or Invert
    PRIVATE lcScanExpr
    DO CASE
      CASE lcApplyMth = "SEL_ALL"  && Select All Case
        lcScanExpr = [FOR lnSel = 0]

      CASE lcApplyMth = "SEL_NON"  && Select None Case
        lcScanExpr = [FOR lnSel = 1]

      CASE lcApplyMth = "INVERT"  && Invert Case
        lcScanExpr = []

    ENDCASE
    SCAN &lcScanExpr
      =lfSelUnSel(loFormSet)
    ENDSCAN
    GO loFormSet_lnBrRecNo
  ENDIF

  loFormSet_llSelAllSt = IIF(loFormSet_lnSelRec = RECCOUNT() , .F. , .T.)
  loFormSet_llSelNonSt = IIF(loFormSet_lnSelRec = 0 , .F. , .T.)
  loFormSet_AriaForm1.grdOrders.cmdSelect.ENABLED     = !EOF(loFormSet_lcTmpOrdLn)
  loFormSet_AriaForm1.grdOrders.cmdSelect.REFRESH
  loFormSet_AriaForm1.grdOrders.cmdSelectAll.ENABLED  = loFormSet_llSelAllSt
  loFormSet_AriaForm1.grdOrders.cmdSelectAll.REFRESH
  loFormSet_AriaForm1.grdOrders.cmdSelectNone.ENABLED = loFormSet_llSelNonSt
  loFormSet_AriaForm1.grdOrders.cmdSelectNone.REFRESH
  loFormSet_AriaForm1.grdOrders.cmdInvert.ENABLED     = !EOF(loFormSet_lcTmpOrdLn)
  loFormSet_AriaForm1.grdOrders.cmdInvert.REFRESH

  IF loFormSet_lnSelRec = 0
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbAlo','DISABLE')
    *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbRel','DISABLE')
    *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbGen','DISABLE')
    loFormSet_oToolBar.ChangeButtonStatus('pbAlo','DISABLE')
    loFormSet_oToolBar.ChangeButtonStatus('pbRel','DISABLE')
    loFormSet_oToolBar.ChangeButtonStatus('pbGen','DISABLE')
    *HBG [End]
  ELSE
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbAlo','ENABLED')
    loFormSet_oToolBar.ChangeButtonStatus('pbAlo','ENABLED')
    *HBG [End]
    lcRelSt = IIF(loFormSet_lnSelAlo = 0 , 'DISABLE', 'ENABLED')
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbRel',lcRelSt)
    *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbGen',lcRelSt)
    loFormSet_oToolBar.ChangeButtonStatus('pbRel',lcRelSt)
    loFormSet_oToolBar.ChangeButtonStatus('pbGen',lcRelSt)
    *HBG [End]
  ENDIF

  *IF The current record is Selected
  IF lnSel = 1
    *osa
    =lfShowGets(.T.,loFormSet)
    *osa
  ELSE
    loFormSet_llCh3Stat = .F.
    loFormSet_laPikSt = .F.
    *=lfDisblGts(loFormSet)
  ENDIF    && End of IF

  *-- end of lfHandlSel.

  *!*************************************************************
  *! Name      : lfSelUnSel
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Select / UnSelect Record.
  *!*************************************************************
  *!
FUNCTION lfSelUnSel
  PARAMETERS loFormSet

  REPLACE lnSel WITH IIF(lnSel = 0 ,1,0)

  loFormSet_lnSelRec = IIF(lnSel = 1,loFormSet_lnSelRec + 1,loFormSet_lnSelRec - 1)
  loFormSet_lnSelAlo = IIF(TOTPIK <> 0 AND lnSel = 1, loFormSet_lnSelAlo + 1 , loFormSet_lnSelAlo)

  *-- end of lfSelUnSel.

  *!******************************************************************************
  *! Name      : lfChngDate
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Check setup complete date by order line if it is yes
  *!           : replace ORDHDR by ORDLINE in the laFiltExp array else replace
  *!           : ORDLINE by ORDHDR. Then replace ORDLINE by lcChildFil
  *!******************************************************************************
FUNCTION lfChngDate
  PARAMETERS loFormSet
  PRIVATE lnCompDate

  *-- if setup is complete date by order line replace ORDLINE & ORDHDR by lcChildFil in laFiltExp
  *-- array, else replace ORDLINE by ORDHDR [Begin]
  IF loFormSet_llLinCmplt
    lnCompDate = ASCAN(loFormSet_lafiltexp,'ORDLINE.COMPLETE')
    IF lnCompDate > 0
      loFormSet_lafiltexp[lnCompDate] = "EVAL(lcChildFil+'.COMPLETE')"
    ENDIF
    lnCompDate = ASCAN(loFormSet_lafiltexp,'ORDHDR.COMPLETE')
    IF lnCompDate > 0
      loFormSet_lafiltexp[lnCompDate] = "EVAL(lcChildFil+'.COMPLETE')"
    ENDIF
  ELSE
    lnCompDate = ASCAN(loFormSet_lafiltexp,'ORDLINE.COMPLETE')
    IF lnCompDate > 0
      loFormSet_lafiltexp[lnCompDate] = 'ORDHDR.COMPLETE'
    ENDIF
  ENDIF && if setup is complete date by order line [Begin]
  *-- end of lfChngDate

  *!*************************************************************
  *! Name      : lfCreatExp
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to create the selected critirea expression
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfCreatExp
  PARAMETERS lnIncExcMd,loFormSet

  PRIVATE lnBlok , lnExpArLen , lnScopExpr , lnOrdrExpr , lnArayLen ,;
    lcScopFile , lcScopFild , lcScopOptm , laTmpIndex

  lnBlok = 1        && Varible to hold the OR number

  lcScopFile = ' '         && Varible to hold the name of the main file of the data selection
  lcScopFild = ' '         && Varible to hold the name of the main field of the data selection
  lcScopOptm = ' '         && Varible to hold the full index expression for the Optmization

  *DO CASE Statment
  DO CASE
      *CASE of Select By all
    CASE loFormSet_lcRpScpMod = ' '
      *DO CASE Statment
      DO CASE
          *CASE of there is a selection criteria on the Style
        CASE lfExpFind('STYLE.')
          loFormSet_lcRpScpMod = 'S'
          lcScopFile = 'STYLE.'
          IF lfExpFind('FABRIC')
            lcScopFild = 'FABRIC'
          ENDIF
          *CASE of there is a selection criteria on the Order header
        CASE lfExpFind('ORDHDR.')
          loFormSet_lcRpScpMod = 'O'
          lcScopFile = 'ORDHDR.'
          lcScopFild = 'ORDER'
          lcScopOptm = 'CORDTYPE + ORDER'
      ENDCASE    && End of DO CASE Statment

      *CASE of Select By Style
    CASE loFormSet_lcRpScpMod = 'S'
      lcScopFile = 'STYLE.'
      lcScopFild = 'STYLE'
      lcScopOptm = 'STYLE'
      *CASE of Select By Order
    CASE loFormSet_lcRpScpMod = 'O'
      lcScopFile = 'ORDHDR.'
      lcScopFild = 'ORDER'
      lcScopOptm = 'CORDTYPE + ORDER'
      *CASE of Select By Account
    CASE loFormSet_lcRpScpMod = 'A'
      lcScopFile = 'ORDHDR.'
      lcScopFild = 'ACCOUNT'
      lcScopOptm = 'ACCOUNT + CORDTYPE + ORDER'
      *CASE of Select By Cut ticket
    CASE loFormSet_lcRpScpMod = 'K'
      lcScopFile = 'POSHDR1.'
      lcScopFild = 'PO'
      lcScopOptm = 'CBUSDOCU + CSTYTYPE + PO'
      *CASE of Select By PO
    CASE loFormSet_lcRpScpMod = 'P'
      lcScopFile = 'POSHDR1.'
      lcScopFild = 'PO'
      lcScopOptm = 'CBUSDOCU + CSTYTYPE + PO'
  ENDCASE    && End of DO CASE Statment

  *FOR Loop to scan the array laOGVrFlt rows
  FOR lnExpArLen = 1 TO ALEN(loFormSet_lafiltexp , 1)
    IF loFormSet_lafiltexp[lnExpArLen,7]="R" AND !('STYLE' $ loFormSet_lafiltexp[lnExpArLen,1]) AND !('CWARECODE' $ loFormSet_lafiltexp[lnExpArLen,1])
      IF USED(loFormSet_lafiltexp[lnExpArLen,6]) AND;
          RECCOUNT(loFormSet_lafiltexp[lnExpArLen,6]) > 0
        loFormSet_lcOptmFile = loFormSet_lafiltexp[lnExpArLen,6]
      ELSE
        loFormSet_lcOptmFile = ''
      ENDIF
      LOOP
    ENDIF


    *IF There is a value for this row or if this is one of the OR rows
    IF !lfEmpty(loFormSet_lafiltexp[lnExpArLen , 6] , loFormSet_lafiltexp[lnExpArLen , 3] = 'D');
        .OR. UPPER(ALLTRIM(loFormSet_lafiltexp[lnExpArLen , 1])) = '.OR.'

      *DO CASE Statment
      DO CASE
          *CASE of one of the OR rows
        CASE UPPER(ALLTRIM(loFormSet_lafiltexp[lnExpArLen , 1])) = '.OR.'
          lnBlok = lnBlok + 1

          *CASE of one of the main file fields
        CASE lcScopFile $ loFormSet_lafiltexp[lnExpArLen , 1]
          lnScopExpr = ALEN(loFormSet_laScopExpr , 1)
          DIMENSION loFormSet_laScopExpr(lnScopExpr + 1 , 2)
          loFormSet_laScopExpr[lnScopExpr + 1 , 1] = lnBlok
          loFormSet_laScopExpr[lnScopExpr + 1 , 2] = lfGetExp(lnExpArLen , lcScopFile , lcScopFild , lcScopOptm)
          *Otherwise
        OTHERWISE
          lnOrdrExpr = ALEN(loFormSet_laNormExpr , 1)
          DIMENSION loFormSet_laNormExpr(lnOrdrExpr + 1 , 2)
          loFormSet_laNormExpr[lnOrdrExpr + 1 , 1] = lnBlok
          loFormSet_laNormExpr[lnOrdrExpr + 1 , 2] = lfGetExp(lnExpArLen , '' , '' , '')
      ENDCASE    && End of DO CASE Statment
    ENDIF    && End of IF
  ENDFOR    && End of FOR Loop

  *IF There is any selection criteria on the main file
  IF ALEN(loFormSet_laScopExpr , 1) > 1

    *FOR Loop to scan the array loFormSet_laScopExpr rows
    FOR lnArayLen = 2 TO ALEN(loFormSet_laScopExpr , 1)
      loFormSet_laScopExpr[1,2] = loFormSet_laScopExpr[1,2] + IIF(lnArayLen = 2 , '' ,;
        IIF(loFormSet_laScopExpr[lnArayLen,1] <> loFormSet_laScopExpr[lnArayLen - 1,1] ,;
        ') .OR. (' , ' .AND. ')) +;
        loFormSet_laScopExpr[lnArayLen,2]
    ENDFOR    && End of FOR Loop
    loFormSet_laScopExpr[1,2] = '(' + loFormSet_laScopExpr[1,2] + ')'
  ENDIF    && End of IF

  *IF There is any selection criteria at any file but the main file
  IF ALEN(loFormSet_laNormExpr , 1) > 1
    *FOR Loop to scan the array loFormSet_laNormExpr rows
    FOR lnArayLen = 2 TO ALEN(loFormSet_laNormExpr , 1)
      loFormSet_laNormExpr[1,2] = loFormSet_laNormExpr[1,2] + IIF(lnArayLen = 2 , '' ,;
        IIF(loFormSet_laNormExpr[lnArayLen,1] <> loFormSet_laNormExpr[lnArayLen - 1,1] ,;
        ') .OR. (' , ' .AND. ')) +;
        loFormSet_laNormExpr[lnArayLen,2]
    ENDFOR    && End of FOR Loop
    loFormSet_laNormExpr[1,2] = '(' + loFormSet_laNormExpr[1,2] + ')'
  ENDIF    && End of IF

  loFormSet_llStylRel = IIF(loFormSet_lcRpScpMod <> 'S' , .T. , .F.)
  loFormSet_llOrdrRel = IIF(INLIST(loFormSet_lcRpScpMod , 'O' , 'A') , .F. , .T.)

  SELECT (loFormSet_lcTmpOrdLn)
  SET ORDER TO

  *!*************************************************************
  *! Name      : lfExpFind
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to find if there is any selection criteria
  *!             on a specific file
  *!*************************************************************
  *! Called from : lfCreatExp()
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : File name to look for
  *!*************************************************************
  *! Return      : .T. if found
  *!               .F. Otherwise
  *!*************************************************************
  *
FUNCTION lfExpFind
  PARAMETERS lcFile


  PRIVATE lnCount
  *FOR Loop to scan the array loFormSet_lafiltexp rows
  FOR lnCount = 1 TO ALEN(loFormSet_lafiltexp , 1)
    *IF The field belong to the file we are looking for and the value [The
    *right hand side] is not empty
    IF lcFile $ loFormSet_lafiltexp[lnCount, 1] .AND. !EMPTY(loFormSet_lafiltexp[lnCount , 6])
      RETURN .T.
    ENDIF    && End of IF
  ENDFOR    && End of FOR Loop

  RETURN .F.

  *!*************************************************************
  *! Name      : lfEmpty
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to check if the Option grid elmint is empty
  *!*************************************************************
  *! Called from : lfCreatExp()
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : 1) The Option grid value array column
  *!                     2) .T. if the field is a Date field ,
  *!                        .F. Otherwise
  *!*************************************************************
  *! Return      : .T. if empty Value , .F. Otherwise
  *!*************************************************************
  *
FUNCTION lfEmpty
  PARAMETERS lcParm , llParm

  *IF Character
  IF TYPE('lcParm') = 'C'
    lcParm = STRTRAN(lcParm , '|' , '')
    lcParm = IIF(llParm , STRTRAN(lcParm , '/' , '') , lcParm)
  ENDIF    && End of IF

  RETURN EMPTY(lcParm)


  *!*************************************************************
  *! Name      : lfSelData
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to create the temp. Order lines file
  *!*************************************************************
  *! Called from : lfvScope()
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : .T. if there is any records for the selection criteria
  *!               .F. Otherwise
  *!*************************************************************
  *
FUNCTION lfSelData
  PARAMETERS loFormSet
  PRIVATE lcForCond1 , lcOrdHExp , lcStaus , lcBulk ,;
    lnOldRecCo , laTmpIndex , lnProssCurt

  lcSTRDATE = SET("Strictdate")
  SET STRICTDATE TO 0

  IF EMPTY(loFormSet_lcIndexExp)
    STORE IIF(EMPTY(loFormSet_lcOldIndex),lfEvalIndx(),loFormSet_lcOldIndex) TO loFormSet_lcOldIndex,loFormSet_lcIndexExp
  ENDIF

  *-- lnCrSelect : Save current selected alias no.
  *-- lcChildFil : Variable to hold name of child loop file[ORDLINE or Temp. ORDLINE]
  PRIVATE lnCrSelect,lcChildFil
  lcChildFil = IIF(loFormSet_llExclude,loFormSet_lcTmpOrdLn,'ORDLINE')

  SELECT (loFormSet_lcTmpOrdLn)

  IF !loFormSet_llExclude
    STORE 0 TO loFormSet_lnDellRec
    SELECT (loFormSet_lcTmpOrdLn)
    DELETE ALL
    =lfErsBldPk(.T.)

    *osa
    *=lfBundBrow(loFormSet)
    *osa

  ENDIF

  SET ORDER TO
  GO TOP

  lnOldRecCo = RECCOUNT(loFormSet_lcTmpOrdLn)        && Variable to hold the old number of records in temp. Order lines file
  m.LnSEL = 0
  m.NPROCNO = 0

  STORE '' TO  m.Priority,m.Entered,m.cDivision,m.cStyGroup,m.Fabric,;
    m.PATTERN,m.cStyMajor,m.Trancd,m.CtktNo,m.cFabColor,m.cPeggedDye

  lcStaus = IIF(INLIST(loFormSet_lcRpScpMod , 'O' , 'A') , 'Status' , 'ORDHDR.Status')    && Variable to hold the name of the Order status field
  lcBulk = IIF(INLIST(loFormSet_lcRpScpMod , 'O' , 'A') , 'Bulk' , 'ORDHDR.Bulk')    && Variable to hold the name of the Order bulk field
  lcOrdHExp = '(' + IIF(INLIST(loFormSet_lcRpScpMod , 'O' , 'A') , '' , 'ORDHDR.') +;
    'cOrdType <> "C"' + ' .AND. ' + IIF(loFormSet_llRpIncHOr , 'INLIST(' +;
    lcStaus + ',"O","H")' , lcStaus + ' = "O"') + IIF(loFormSet_llRpExlBlk ,;
    ' .AND. ' + lcBulk + ' <> "Y")' , ')')          && Variable to hold the Order header filter

  PRIVATE lcForCond2,lcAlocFlt

  lcAlocFlt = IIF(loFormSet_lcRpAloNot = "A",'','!')
  lcForCond2 = '(TotQty <> 0' +;
    IIF(loFormSet_lcRpAloNot="B",'',' .AND. &lcAlocFlt.Picked') +;
    IIF(loFormSet_llStylRel , ' .AND. !EOF("STYLE") ' +;
    IIF(loFormSet_llUseDyes .AND. loFormSet_llRpExlDye , ' .AND. STYLE.cDye_Flg <> "Y"' , '') , '') +;
    IIF(loFormSet_llRpPikSep , '' , ' .AND. !EMPTY(Group)') +;
    IIF(loFormSet_llRpPikCor , '' , ' .AND. EMPTY(Group)') +;
    IIF(INLIST(loFormSet_lcRpScpMod , 'O' , 'A') , ')' , ' .AND. ' + lcOrdHExp + ')') +;
    IIF(EMPTY(loFormSet_laNormExpr[1,2]) , '' , ' .AND. ' +;
    ALLTRIM(loFormSet_laNormExpr[1,2])) +;
    IIF(loFormSet_lcRpScpMod $ ' |S','.AND. CUSTOMER.STATUS = "A"', '')

  *IF We need a relation between the lcChildFil file and the STYLE file
  IF loFormSet_llStylRel
    SELECT STYLE
    SET ORDER TO TAG STYLE
    SELECT (lcChildFil)
    SET RELATION TO STYLE INTO STYLE
  ENDIF    && End of IF

  *IF We need a relation between the lcChildFil file and the ORDHDR file
  IF loFormSet_llOrdrRel
    SELECT ORDHDR
    SET ORDER TO TAG ORDHDR
    SELECT (lcChildFil)
    SET RELATION TO 'O' + ORDER INTO ORDHDR ADDITIVE
  ENDIF    && End of IF

  *DO CASE Statment
  DO CASE

      *CASE of Select by Style
    CASE loFormSet_lcRpScpMod = 'S'
      *>>>Use Both <condition is needed in lcForCond2>
      lcForCond1 = ALLTRIM(loFormSet_laScopExpr[1,2]) + IIF(EMPTY(loFormSet_laScopExpr[1,2]) ,;
        'Status <> "X"' , ' .AND. Status <> "X"') +;
        IIF(loFormSet_llUseDyes .AND. loFormSet_llRpExlDye ,;
        ' .AND. STYLE.cDye_Flg <> "Y"' , '')      && Variable to hold the Main file filter expression

      PRIVATE lcTmpTag
      lcTmpTag = IIF(loFormSet_llExclude,loFormSet_lcTmStyTag,'ORDLINES')  && Style index.
      SET ORDER TO TAG &lcTmpTag IN (lcChildFil)

      SELECT (lcChildFil)
      SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE

      SELECT STYLE
      IF EMPTY(loFormSet_lcOptmFile)
        SET ORDER TO
        =lfUpdatSty(.F.,loFormSet)
      ELSE
        SET ORDER TO STYLE
        SELECT(loFormSet_lcOptmFile)
        SCAN
          =lfUpdatSty(.T.,loFormSet)
        ENDSCAN
      ENDIF
      SET ORDER TO TAG STYLE IN STYLE

      SELECT (lcChildFil)
      SET RELATION OFF INTO CUSTOMER


      *CASE of Select by Order or Select by Account
    CASE loFormSet_lcRpScpMod = 'O' .OR.  loFormSet_lcRpScpMod = 'A'
      *>>>Use Both - condition is needed in lcForCond1 only
      lcWaitStr = IIF(loFormSet_lcRpScpMod = 'O' , 'order ' , 'account ')
      lcForCond1 = ALLTRIM(loFormSet_laScopExpr[1,2]) + IIF(EMPTY(loFormSet_laScopExpr[1,2]) ,;
        lcOrdHExp , ' .AND. ' + lcOrdHExp) +;
        '.AND. CUSTOMER.STATUS = "A" '
      lcForCond2 = STRTRAN(lcForCond2,'.AND. CUSTOMER.STATUS = "A"')

      PRIVATE lcTmpTag
      lcTmpTag = IIF(loFormSet_llExclude,loFormSet_lcTmpIndex,'ORDLINE')  && most Child file index.
      SET ORDER TO TAG &lcTmpTag IN (lcChildFil)

      SELECT ORDHDR
      IF EMPTY(loFormSet_lcOptmFile)
        SET ORDER TO
        =lfUpdatOrd(.F.,loFormSet)
      ELSE
        IF loFormSet_lcRpScpMod = 'O'
          SET ORDER TO ORDHDR
        ELSE
          SET ORDER TO Ordacct
        ENDIF
        SELECT(loFormSet_lcOptmFile)
        SCAN
          =lfUpdatOrd(.T.,loFormSet)
        ENDSCAN
      ENDIF
      SET ORDER TO TAG ORDHDR IN ORDHDR

      *CASE of Select by Cut ticket
    CASE loFormSet_lcRpScpMod = 'K'
      *>>> Use both - condition is needed in lcForCond1 only
      lcForCond1 = ALLTRIM(loFormSet_laScopExpr[1,2]) + IIF(EMPTY(loFormSet_laScopExpr[1,2]) ,;
        'Status <> "X"' , ' .AND. Status <> "X"') +;
        ' .AND. CUSTOMER.STATUS = "A" '
      lcForCond2 = STRTRAN(lcForCond2,'.AND. CUSTOMER.STATUS = "A"')
      IF !loFormSet_llExclude
        SET ORDER TO TAG ORDLINE IN ORDLINE
      ENDIF
      SET ORDER TO TAG (loFormSet_lcTmpIndex) IN (loFormSet_lcTmpOrdLn)

      SELECT POSHDR1
      IF EMPTY(loFormSet_lcOptmFile)
        SET ORDER TO
        =lfUpdatCt(.F.,loFormSet)
      ELSE
        SET ORDER TO POSHDR
        SELECT(loFormSet_lcOptmFile)
        SCAN
          =lfUpdatCt(.T.,loFormSet)
        ENDSCAN
      ENDIF
      SET ORDER TO POSHDR IN POSHDR1

      *CASE of Select by PO
    CASE loFormSet_lcRpScpMod = 'P'
      *>>> Use Both - condition is needed in lcForCond1 only
      lcForCond1 = ALLTRIM(loFormSet_laScopExpr[1,2]) + IIF(EMPTY(loFormSet_laScopExpr[1,2]) ,;
        'Status <> "X"' , ' .AND. Status <> "X"') +;
        ' .AND. CUSTOMER.STATUS = "A" '
      lcForCond2 = STRTRAN(lcForCond2,'.AND. CUSTOMER.STATUS = "A"')

      IF !loFormSet_llExclude
        SET ORDER TO TAG ORDLINE IN ORDLINE
      ENDIF
      SET ORDER TO TAG (loFormSet_lcTmpIndex) IN (loFormSet_lcTmpOrdLn)

      SELECT POSHDR1
      IF EMPTY(loFormSet_lcOptmFile)
        SET ORDER TO
        =lfUpdatPo(.F.,loFormSet)
      ELSE
        SET ORDER TO POSHDR
        SELECT(loFormSet_lcOptmFile)
        SCAN
          =lfUpdatPo(.T.,loFormSet)
        ENDSCAN
      ENDIF
      SET ORDER TO TAG POSHDR IN POSHDR1

      *CASE of Select by all
    CASE loFormSet_lcRpScpMod = " "
      *>>> use lcForCond2 only
      SELECT (lcChildFil)
      SET ORDER TO
      SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE
      COUNT FOR &lcForCond2 TO lnTotRec

      *osa
      *loFormSet_oPross.lblFirstLabel.Caption = IIF(loFormSet_llExclude,LANG_AutoAlloc_ExcludingOrder,LANG_AutoAlloc_SelectingOrder)
      *loFormSet_oPross.TotalProgress = lnTotRec
      *HBG 1/24/2005 Modify code to apply the new interface [Begin]
      *loFormSet_oPross.AutoCenter = .T.
      *HBG [End]
      *loFormSet_oPross.Show()
      *osa

      lnProssCurt = 0
      SCAN FOR &lcForCond2
        IF loFormSet_llExclude
          lnProssCurt = lnProssCurt + 1

          *osa
          *loFormSet_oPross.lblSecondLabel.Caption = LANG_AutoAlloc_ExcludingOrder + Order
          *loFormSet_oPross.CurrentProgress(lnProssCurt)
          *osa

          =lfExclRec(loFormSet)
        ELSE  && Scope mode
          lnProssCurt = lnProssCurt + 1

          *osa
          *loFormSet_oPross.lblSecondLabel.Caption = LANG_AutoAlloc_SelectingOrder + Order
          *loFormSet_oPross.CurrentProgress(lnProssCurt)
          *osa

          SCATTER MEMVAR MEMO
          FOR lnI= 1  TO 8
            lcI = STR(lnI,1)
            m.Alo&lcI = m.Pik&lcI
          ENDFOR
          m.TotAlo = m.TotPik
          =lfUpdAloVr(loFormSet)
        ENDIF
      ENDSCAN    && End of SCAN Loop

      *osa
      *loFormSet_oPross.Hide()
      *osa

      SELECT (lcChildFil)
      SET RELATION OFF INTO CUSTOMER

  ENDCASE    && End of DO CASE Statment

  SELECT (lcChildFil)
  SET RELATION TO
  SET ORDER TO TAG ORDLINST IN ORDLINE

  GO TOP IN (loFormSet_lcTmpOrdLn)
  loFormSet_lnRecNumbr = RECCOUNT(loFormSet_lcTmpOrdLn) - IIF(loFormSet_llExclude,loFormSet_lnDellRec,lnOldRecCo)
  loFormSet_lcOptmFile = ""

  SET STRICTDATE TO &lcSTRDATE

  RETURN IIF(loFormSet_llExclude,!EOF(loFormSet_lcTmpOrdLn),RECCOUNT(loFormSet_lcTmpOrdLn) > lnOldRecCo)
  *-- end of lfSelData.

  *!*************************************************************
  *! Name      : lfRefScr
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Refresh screen if exclude delete all records.
  *!*************************************************************
  *! Calls     : ....
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : = lfRefScr()
  *!*************************************************************
  *E300989,1
  *
FUNCTION lfRefScr
  PARAMETERS loFormSet

  SELECT (loFormSet_lcTmpOrdLn)
  =lfwBrows(loFormSet)  && Refresh browse and screen.
  *=lfDisblGts(loFormSet)
  *-- Handle status of tool bar and option menu
  *=lfHandlObj(loFormSet)

  *-- end of lfRefScr.

  *!*************************************************************
  *! Name      : lfEvalIndx
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Create index expression
  *!*************************************************************
  *! Return      : Index expression.
  *!*************************************************************
  *
FUNCTION lfEvalIndx

  PRIVATE lnI , lcIndExpr , llHaveOrd
  llHaveOrd = .F.
  lnI = 0
  lcIndExpr = ''
  FOR lnI = 1 TO 4
    IF EMPTY(loFormSet_laSortAry[lnI,2])
      EXIT
    ENDIF
    IF loFormSet_laSortAry[lnI,2] = [ORDER]
      llHaveOrd = .T.
    ENDIF
    IF !EMPTY(lcIndExpr)
      lcIndExpr = lcIndExpr + '+'
    ENDIF
    lcIndExpr = lcIndExpr + loFormSet_laSortAry[lnI,2]
  ENDFOR
  lcIndExpr = lcIndExpr + IIF(llHaveOrd,'',[+ ORDER ])
  IF loFormSet_llRpExlDye OR !(loFormSet_llFabDye)
    lcIndExpr = lcIndExpr + [+STORE+GROUP+STR(LINENO , 6)]
  ELSE
    lcIndExpr = lcIndExpr + [+STORE+FABRIC+CFABCOLOR+GROUP+STYLE+CWARECODE]
  ENDIF
  RETURN lcIndExpr
  *-- end of lfEvalIndx.


  *!*************************************************************
  *! Name      : lfGenScr
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of push button Generate pick tickets
  *!*************************************************************
  *! Called from : Control Panel [Push button Generate pick tickets] ,
  *!               Option popup , lfvScope()
  *!*************************************************************
  *! Calls       : gfThermo() , lfGetPkTkt() , lfGenPikTk()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfGenScr
  PARAMETERS loFormSet




  lcStyle    = EVALUATE(loFormSet_lcTmpordln+'.Style')
  lcDyelot   = EVALUATE(loFormSet_lcTmpordln+'.Dyelot')
  lcWareCode = EVALUATE(loFormSet_lcTmpordln+'.cWareCode')

  SELECT (loFormSet_lcTmpOrdLn)

  PRIVATE lcPikTkt , lnTotRec , lnCurent , lnRecCount , lnPrepRec
  * MAH
  *--lnTotPik = loFormSet_AriaForm1.cntDetail.txtTotalPik.Value
  lnTotPik = 0
  * MAH

  *IF Any of the Syle or Warehouse or Dyelot was changed
  IF lcStyle + lcWareCode + lcDyeLot <> STYLE + cWareCode + DyeLot
    llAloRec = IIF(lnTotPik = 0 , .F. , .T.)
  ELSE    && Else
    llAloRec = IIF(loFormSet_llIncOrd .OR. loFormSet_lnChangAlo > 0 , .T. , .F.)
  ENDIF    && End of IF

  *IF The current record was edited
  IF llAloRec
    =lfAllocate(3,RECNO(loFormSet_lcTmpOrdLn),loFormSet)
  ENDIF    && End of IF

  llAloRec = .F.
  loFormSet_llIncOrd = .F.
  loFormSet_lnChangAlo = 0

  SET ORDER TO TAG ORDHDR IN ORDHDR
  SELECT (loFormSet_lcTmpOrdLn)
  SET ORDER TO TAG (loFormSet_lcTmpOrdLn)
  SET RELATION TO 'O' + ORDER INTO ORDHDR

  COUNT FOR lnSel = 1 AND TOTPIK <> 0 TO lnTotRec

  lnCurent = 0                   && Varible to hold the current count to be done for the thermometer

  lnRecCount = RECCOUNT() - loFormSet_lnDellRec   && Varible to hold the Total count to be done for the thermometer
  lnPrepRec = 0                                  && Varible to hold the current count to be done for the thermometer
  IF TYPE('loFormSet_oPross') = 'O'
    loFormSet_oPross.lblFirstLabel.CAPTION = LANG_AutoAlloc_LableProgress
    loFormSet_oPross.TotalProgress = lnRecCount
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    loFormSet_oPross.AUTOCENTER = .T.
    *HBG [End]
    loFormSet_oPross.SHOW()
   ENDIF
  *SCAN Loop to scan the temp. Order lines file
  SCAN
    lnPrepRec = lnPrepRec + 1
    REPLACE nProcNo WITH 0
    IF TYPE('loFormSet_oPross') = 'O'
     loFormSet_oPross.CurrentProgress(lnPrepRec)
    ENDIF
  ENDSCAN    && End of SCAN Loop

  *SCAN Loop to scan the temp. Order lines file FOR the selected
  *and allocated records and for nProcNo < 4
  IF TYPE('loFormSet_oPross') = 'O'
    loFormSet_oPross.lblFirstLabel.CAPTION = LANG_AutoAlloc_LableAssign
    loFormSet_oPross.TotalProgress = lnTotRec
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    loFormSet_oPross.AUTOCENTER = .T.
    *HBG [End]
    loFormSet_oPross.SHOW()
  ENDIF
  SCAN FOR lnSel = 1 .AND. TotPik > 0 .AND. nProcNo < 4
    IF !EMPTY(PIKTKT) AND PIKTKT # '******'
      LOOP
    ENDIF
    lnCurent = lnCurent + 1

    *IF the first step for this record
    IF nProcNo = 0
      IF SEEK(ORDER+STORE+cWareCode,loFormSet_lcTmpPkTk)
        lcPikTkt = EVALUATE(loFormSet_lcTmpPkTk+'.PikTkt')
      ELSE
        lcPikTkt = loFormSet_oAlObj.lfGetPkTkt(ORDER , ORDHDR.cDivision , STORE , cWareCode ,;
          loFormSet_lnRpGenNew , IIF(loFormSet_llRpPkHPck,'Y','N'))
      ENDIF
    ENDIF    && End of IF
    =lfGenPikTk(lcPikTkt,loFormSet)
    IF TYPE('loFormSet_oPross') = 'O'
      loFormSet_oPross.lblSecondLabel.CAPTION = ORDER + '/' + STYLE
      loFormSet_oPross.CurrentProgress(lnCurent)
    ENDIF
  ENDSCAN    && End of SCAN Loop

  *IF There was a selected and allocated records
  IF lnTotRec > 0
    IF TYPE('loFormSet_oPross') = 'O'
      loFormSet_oPross.lblFirstLabel.CAPTION = LANG_AutoAlloc_LableAssign
      loFormSet_oPross.CurrentProgress(lnTotRec)
    ENDIF
  ENDIF    && End of IF

  SELECT (loFormSet_lcTmpOrdLn)
  SET ORDER TO TAG (loFormSet_lcTmpOrdLn)
  LOCATE

  *IF The system use Dyelots
  IF loFormSet_llUseDyes
    SET RELATION TO STYLE + cWareCode + DyeLot INTO STYDYE
  ELSE    && Else
    SET RELATION TO STYLE + cWareCode + SPACE(10) INTO STYDYE
  ENDIF    && End of IF

  *-- IF There is no records in the file [IF we have generated Pick tickets for
  *-- all the records]
  *IF EOF()
  *  loFormSet_ActiveMode = 'S'
  *  =loFormSet_changeMode('S')
  *ENDIF    && End of IF

  *-- Handle status of tool bar and option menu
  *=lfHandlObj(loFormSet)

  SELECT (loFormSet_lcTmpOrdLn)
  SET RELATION OFF INTO ORDHDR
  * MAH Check for Reject
  IF loFormSet_llReject
    *  *** Message : "Can  not allocate all lines. Some lines hve been rejected."
    *  ***           "              < Ok >               "
    =gfModalGen("TRM44122B00000","DIALOG")
  ENDIF
  *loFormSet_changeMode(loFormSet_ActiveMode )
  *-- end of lfGenScr.

  *!*************************************************************
  *! Name      : lfErsBldPk
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Erase and Build temp. Pick file.
  *!*************************************************************
  *B603111,1
FUNCTION lfErsBldPk
  PARAMETERS llBuild
  PRIVATE lcCurrAls
  lcCurrAls = SELECT(0)

  *-- CLOSE piktkts generated in this session [Begin]
  IF USED(loFormSet_lcTmpPkTk)
    USE IN (loFormSet_lcTmpPkTk)
  ENDIF

  IF FILE(oAriaEnvironment.Workdir + loFormSet_lcTmpPkTk + '.DBF')
    ERASE (oAriaEnvironment.Workdir+loFormSet_lcTmpPkTk+'.DBF')          && Erase the Temp file.
  ENDIF

  IF FILE(oAriaEnvironment.Workdir+loFormSet_lcTmpPkTk+'.CDX')
    ERASE (oAriaEnvironment.Workdir+loFormSet_lcTmpPkTk+'.CDX')        && Erase the Temp file.
  ENDIF

  IF llBuild
    CREATE TABLE (oAriaEnvironment.Workdir + loFormSet_lcTmpPkTk) ;
      (ORDER  C(6), STORE C(8), cWareCode C(6),PikTkt C(6),cLineNo C(6))
    ZAP
    *B609356,1 SMA 07/29/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
    *INDEX ON ORDER + STORE + cWareCode + PikTkt + cLineNo TAG (loFormSet_lcTmpPkTk) OF ;
      (oAriaEnvironment.WorkDir+loFormSet_lcTmpPkTk+'.CDX')
    INDEX ON ORDER + STORE + cWareCode + PikTkt + cLineNo TAG (loFormSet_lcTmpPkTk) 
    *B609356,1 SMA 07/29/2010 remove of clause to prevent empty *.cdx files from creation.....[END]  
  ENDIF
  SELECT (lcCurrAls)
  *-- end of lfErsBldPk.

  *!*************************************************************
  *! Name      : lfExclRec
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Exclude this line.
  *!*************************************************************
  *! Passed Parameters  : ...
  *!*************************************************************
  *!
FUNCTION lfExclRec
  PARAMETERS loFormSet

  loFormSet_lnDellRec = loFormSet_lnDellRec + 1
  DELETE
  *-- end of lfExclRec.

  *!*************************************************************
  *! Name      : lfUpdAloVr
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Update allocation variables at start.
  *!*************************************************************
  *! Called from : ....
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfUpdAloVr()
  *!*************************************************************
FUNCTION lfUpdAloVr
  PARAMETERS loFormSet

  PRIVATE lnCurSelAl

  lnCurSelAl = SELECT(0)
  m.TotAvl = 0
  FOR lnI= 1  TO 8
    lcI = STR(lnI,1)
    m.Avl&lcI = 0
  ENDFOR
  STORE "" TO m.Trancd , m.CtktNo , m.cFabColor , m.Priority , m.Entered , m.cDivision,;
    m.cStyGroup  , m.Fabric , m.Pattern , m.cStyMajor , m.cPeggedDye
  = lfFillMVar() &&Get memory variables that will add to temp. file
  INSERT INTO (loFormSet_lcTmpOrdLn) FROM MEMVAR

  SELECT (loFormSet_lcTmpOrdLn)
  REPLACE cSortField WITH EVALUATE(loFormSet_lcIndexExp),lnSel WITH IIF(Picked,1,0)


  IF !loFormSet_llPartAlo AND TOTPIK < TOTQTY
    loFormSet_llPartAlo = .T.
  ENDIF

  SELECT (lnCurSelAl)
  *-- end of lfUpdAloVr.

  *!*************************************************************
  *! Name      : lfFillMVar
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Fill New temporary fields with its data.
  *!*************************************************************
  *! Calls     : ....
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : =lfFillMVar()
  *!*************************************************************
FUNCTION lfFillMVar
  m.Priority   = ORDHDR.Priority
  m.Entered    = ORDHDR.Entered
  m.cDivision  = STYLE.cDivision
  m.cStyGroup  = STYLE.cStyGroup
  m.Fabric     = STYLE.Fabric
  m.Pattern    = STYLE.PATTERN
  m.cStyMajor  = STYLE.cStyMajor
  m.cPeggedDye = m.Dyelot

  =SEEK(m.Style + m.cWareCode + m.Dyelot , 'STYDYE')
  m.TotAvl = 0
  FOR lnI= 1  TO 8
    lcI = STR(lnI,1)
    m.Avl&lcI = STYDYE.Stk&lcI
    m.TotAvl = m.TotAvl + m.Avl&lcI
  ENDFOR

  *-- if either MF or PO modules is installed
  IF ('MF' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PO' $ oAriaEnvironment.CompanyInstalledModules)
    *-- if user did not select By (C/T or P/O)
    IF !(loFormSet_lcRpScpMod $ 'KP')
      PRIVATE lnAliasNo,lcPickOrd
      lnAliasNo = SELECT(0)
      lcCutpKCond = "TranCd IN ('1,2') AND [Order] = '" + ORDHDR.ORDER + "' AND STR(cOrdLine,6) = " + STR(ORDLINE.LINENO,6)
      llCutpKBom = .F.
      IF lfOpenSql(loFormSet,'CUTPICK','CUTPICK1',lcCutpKCond)
        SELECT CUTPICK1
        LOCATE
        llCutpKBom = !EOF()
      ENDIF
      SELECT (lnAliasNo)
    ENDIF
    IF llCutpKBom
      m.Trancd = CUTPICK1.Trancd
      m.CtktNo = CUTPICK1.CtktNo
    ENDIF
  ENDIF
  *-- Get primary fabric and its corresponding color, from BOM file[Begin
  m.cFabColor = ''
  IF loFormSet_llFabDye AND !loFormSet_llRpExlDye AND (STYLE.CDYE_FLG = 'Y')  AND ;
      !EMPTY(STYLE.FABRIC)
    PRIVATE lnAliasNo,lcFabOrd
    lnAliasNo = SELECT(0)
    lcStyMaj  = SUBSTR(STYLE.STYLE,1,loFormSet_lnMajorLen)
    lcBomCond = "CINVTYPE = '0001' AND CITMMAJOR = '"+ lcStyMaj +"' AND cCatgTyp = 'F'"
    llFoundBom = .F.
    IF lfOpenSql(loFormSet,'BOM','BOM1',lcBomCond)
      SELECT BOM1
      LOCATE
      IF !EOF()
        llFoundBom = .T.
      ENDIF
    ENDIF
    IF llFoundBom
      SELECT BOM1
      LOCATE REST FOR LIKE(STRTRAN(cItmMask,'*','?'),PADR(STYLE.STYLE,19))
      IF FOUND()
        m.cFabColor = IIF(SUBSTR(ITEM,loFormSet_lnFabMajor+2,loFormSet_lnFabNonMaj) # '******' , SUBSTR(ITEM,loFormSet_lnFabMajor+2,loFormSet_lnFabNonMaj), ;
          IIF(loFormSet_lcFree_Clr='C',;
          SUBSTR(STYLE.STYLE,loFormSet_lnNonMajSt,loFormSet_lnColorLen),''))
        IF !EMPTY(m.cFabColor)
          llNotFItem = .T.
          lcitemCond = "CINVTYPE = '0002' AND STYLE = '"+ BOM1.ITEM +"'"
          IF lfOpenSql(loFormSet,'ITEM','ITEM1',lcitemCond )
            SELECT ITEM1
            SET ORDER TO TAG STYLE IN ITEM1
            LOCATE
            IF !EOF()
              llNotFItem = (ITEM1.CDYE_FLG # 'Y')
            ELSE
              llNotFItem = .T.
            ENDIF
          ENDIF
          IF llNotFItem
            m.cFabColor = ''
          ENDIF
        ENDIF
      ENDIF
    ENDIF

    SELECT (lnAliasNo)

  ENDIF
  *-- Get primary fabric and its corresponding color, from BOM file[End..
  *-- end of lfFillMVar.


  *!*************************************************************
  *! Name      : lfUpdatSty
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Update Select by Style.
  *!*************************************************************
FUNCTION lfUpdatSty
  PARAMETERS llPointSty,loFormSet

  PRIVATE lcScanExpr
  IF llPointSty
    lcScanExpr = [REST WHILE Style= PADR(EVALUATE(loFormSet_lcOptmFile+'.cStyMajor'),loFormSet_lnMajorLen)]
    lcCountExpr= [Style= PADR(EVALUATE(loFormSet_lcOptmFile+'.cStyMajor'),loFormSet_lnMajorLen)]
    =SEEK(PADR(EVALUATE(loFormSet_lcOptmFile+'.cStyMajor'),loFormSet_lnMajorLen),"Style")
  ELSE
    lcScanExpr = []
    lcCountExpr= []
  ENDIF

  SELECT STYLE
  lcExpr = lcCountExpr + IIF(EMPTY(lcCountExpr),'',' AND ') + lcForCond1
  COUNT FOR &lcExpr TO lnTotRec
  IF TYPE('loFormSet_oPross') = 'O'
    loFormSet_oPross.lblFirstLabel.CAPTION = IIF(loFormSet_llExclude,LANG_AutoAlloc_ExcludingOrder,LANG_AutoAlloc_SelectingOrder)
    loFormSet_oPross.TotalProgress = lnTotRec
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    loFormSet_oPross.AUTOCENTER = .T.
    *HBG [End]
    loFormSet_oPross.SHOW()
  ENDIF
  
  lnProssCurt = 0
  IF llPointSty
    =SEEK(PADR(EVALUATE(loFormSet_lcOptmFile+'.cStyMajor'),loFormSet_lnMajorLen),"Style")
  ENDIF
  *SCAN Loop to scan the STYLE file FOR the Style filter expression
  SCAN &lcScanExpr FOR &lcForCond1
    *IF There is Order lines for this Style
    IF SEEK(STYLE , lcChildFil)
      =lfStyInLns(loFormSet)
    ENDIF
  ENDSCAN    && End of SCAN Loop
  *-- end of lfUpdatSty.

  *!*************************************************************
  *! Name      : lfStyInLns
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Scan style in lines file.
  *!*************************************************************
FUNCTION lfStyInLns
  PARAMETERS loFormSet

  SELECT (lcChildFil)
  *SCAN Loop to scan the lcChildFil file FOR the rest of the selectio
  *criteria filter expression
  SCAN REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
      FOR   &lcForCond2
    IF loFormSet_llExclude
      lnProssCurt = lnProssCurt + 1
      IF TYPE('loFormSet_oPross') = 'O'
        loFormSet_oPross.lblSecondLabel.CAPTION = LANG_AutoAlloc_ExcludingOrder+ STYLE
        loFormSet_oPross.CurrentProgress(lnProssCurt)
      ENDIF
      =lfExclRec(loFormSet)
    ELSE  && Scope mode
      lnProssCurt = lnProssCurt + 1
      IF TYPE('loFormSet_oPross') = 'O'
        loFormSet_oPross.lblSecondLabel.CAPTION = LANG_AutoAlloc_SelectingOrder+ STYLE
        loFormSet_oPross.CurrentProgress(lnProssCurt)
      ENDIF
      SCATTER MEMVAR MEMO
      FOR lnI= 1  TO 8
        lcI = STR(lnI,1)
        m.Alo&lcI = m.Pik&lcI
      ENDFOR
      m.TotAlo = m.TotPik
      =lfUpdAloVr(loFormSet)
    ENDIF
  ENDSCAN    && End of SCAN Loop
  IF TYPE('loFormSet_oPross') = 'O'
   loFormSet_oPross.HIDE()
  ENDIF

  SELECT STYLE
  *-- end of lfStyInLns.

  *!*************************************************************
  *! Name      : lfUpdatOrd
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Update Select by (Order/Account).
  *!*************************************************************
FUNCTION lfUpdatOrd
  PARAMETERS llPointSty,loFormSet
  PRIVATE lcScanExpr , lnProssCurt
  IF llPointSty
    IF loFormSet_lcRpScpMod = 'O'
      =SEEK("O"+EVALUATE(loFormSet_lcOptmFile+'.Order'),"OrdHdr")
      lcScanExpr = [REST WHILE cOrdType+Order="O"+EVALUATE(loFormSet_lcOptmFile+'.Order')]
      lcCountExpr = [cOrdType+Order="O"+EVALUATE(loFormSet_lcOptmFile+'.Order')]
    ELSE
      IF SEEK(EVALUATE(loFormSet_lcOptmFile+'.Account')+"O","OrdHdr")
        lcScanExpr = [REST WHILE account+cordtype+order=EVALUATE(loFormSet_lcOptmFile+'.Account')+"O"]
        lcCountExpr= [account+cordtype+order=EVALUATE(loFormSet_lcOptmFile+'.Account')+"O"]
      ELSE
        RETURN
      ENDIF
    ENDIF
  ELSE
    lcScanExpr = []
    lcCountExpr= []
  ENDIF

  *SCAN Loop to scan the ORDHDR file FOR the Order header filter expression
  SELECT ORDHDR

  IF loFormSet_llLinCmplt
    SET RELATION TO cOrdType + ORDER INTO ORDLINE ADDITIVE
  ENDIF
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE
  lcExpr = lcCountExpr + IIF(EMPTY(lcCountExpr),'',' AND ') + lcForCond1
  COUNT FOR &lcExpr TO lnTotRec

  *osa
  
  *loFormSet_oPross.lblFirstLabel.Caption = IIF(loFormSet_llExclude,LANG_AutoAlloc_Excluding + lcWaitStr ,LANG_AutoAlloc_Selecting + lcWaitStr)
  *loFormSet_oPross.TotalProgress = lnTotRec
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *loFormSet_oPross.AutoCenter = .T.
  *HBG [End]
  *loFormSet_oPross.Show()
  *osa

  lnProssCurt = 0
  IF llPointSty
    IF loFormSet_lcRpScpMod = 'O'
      =SEEK("O"+EVALUATE(loFormSet_lcOptmFile+'.Order'),"OrdHdr")
    ELSE
      =SEEK(EVALUATE(loFormSet_lcOptmFile+'.Account')+"O","OrdHdr")
    ENDIF
  ENDIF
  SCAN &lcScanExpr FOR &lcForCond1
    IF SEEK('O' + ORDER , lcChildFil)
      SELECT (lcChildFil)
      *SCAN Loop to scan the (lcChildFil) file FOR the rest of the selection
      *criteria filter expression
      SCAN REST WHILE cordtype+ORDER+STR(LINENO,6) = "O"+ORDHDR.ORDER ;
          FOR &lcForCond2
        IF loFormSet_llExclude
          lnProssCurt = lnProssCurt + 1

          *osa
          *loFormSet_oPross.lblSecondLabel.Caption = LANG_AutoAlloc_Excluding + lcWaitStr + &lcWaitStr
          *loFormSet_oPross.CurrentProgress(lnProssCurt )
          *osa

          =lfExclRec(loFormSet)
        ELSE  && Scope mode
          lnProssCurt = lnProssCurt + 1

          *osa
          *loFormSet_oPross.lblSecondLabel.Caption =  LANG_AutoAlloc_Selecting + lcWaitStr + &lcWaitStr
          *loFormSet_oPross.CurrentProgress(lnProssCurt )
          *osa

          SCATTER MEMVAR MEMO
          FOR lnI= 1  TO 8
            lcI = STR(lnI,1)
            m.Alo&lcI = m.Pik&lcI
          ENDFOR
          m.TotAlo = m.TotPik
          =lfUpdAloVr(loFormSet)
        ENDIF
      ENDSCAN    && End of SCAN Loop
      SELECT ORDHDR
    ENDIF    && End of IF
  ENDSCAN    && End of SCAN Loop
  *loFormSet_oPross.Hide()

  IF loFormSet_llLinCmplt
    SET RELATION OFF INTO ORDLINE
  ENDIF
  SELECT ORDHDR
  SET RELATION OFF INTO CUSTOMER
  *-- end of lfUpdatOrd.

  *!*************************************************************
  *! Name      : lfRelQty
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to update the needed files to Release the allocation
  *!             from the current record of the temp. Order lines file
  *!*************************************************************
  *! Called from : lfRelScr()
  *!*************************************************************
  *! Calls       : gfAdd_Info()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfRelQty
  PARAMETERS loFormSet

  PRIVATE lnCnT1 , lnCnT2 , lcOScale , lnElm , lcElm

  lnCnT1 = 0                  && Variable to hold the number of sizes for the current Style
  lnCnT2 = 0                  && Variable to hold the number of sizes for the Original Style
  lcOScale = ''               && Variable to hold the Scale code of the Original Style

  SELECT (loFormSet_lcTmpOrdLn)

  *IF This line Style was substituted befor
  IF !EMPTY(AltStyle)
    =SEEK(STYLE , 'STYLE')
    =SEEK( 'S'+STYLE.SCALE ,'SCALE')
    lnCnT1 = SCALE.CNT
    =SEEK(AltStyle , 'STYLE')
    =SEEK( 'S'+STYLE.SCALE ,'SCALE')
    lnCnT2 = SCALE.CNT
    lcOScale = SCALE.SCALE
  ENDIF    && End of IF

  llLastLine = .F.

  *-- Update PickLine File.
  *IF nProcNo [The step number (for the rollback)] equal 0
  IF nProcNo = 0

    IF !EMPTY(PIKTKT) AND (PIKTKT <> '******')
      IF !loFormSet_llOpnPack  AND !USED('PACK_HDR')
        lcAlsNow  = SELECT(0)
        loFormSet_llOpnPack  = oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'PACK_HDR',oAriaEnvironment.DataDir+'PACK_HDR','SH')
        SELECT (lcAlsNow)
      ENDIF

      *IF The Pick ticket has a packing list
      SET ORDER TO PACK_HDR IN PACK_HDR
      IF SEEK(PIKTKT,"PACK_HDR")
        *-- Message:
        *-- "picking tickets having packing lists were not released"
        *--                         < Ok >
        *=gfModalGen("TRM44004B00000","ALERT")
        RETURN
      ELSE    && Else

        IF SEEK(ORDER+STORE+cWareCode+PikTkt+STR(LINENO,6),loFormSet_lcTmpPkTk)
          SELECT (loFormSet_lcTmpPkTk)
          DELETE
          SELECT (loFormSet_lcTmpOrdLn)
        ENDIF

        llLastLine = lfBldRel(PikTkt,loFormSet)

        IF llLastLine

          IF !loFormSet_llOpnPikLn AND !USED('PIKLINE')
            loFormSet_llOpnPikLn = oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'PIKLINE','','SH')
          ENDIF

          SELECT (loFormSet_lcRelLine)
          SCAN
            SCATTER MEMVAR MEMO
            SELECT PIKLINE
            INSERT INTO ('PIKLINE') FROM MEMVAR
            =loAddUserInfo.DO('PIKLINE',.NULL.)
            SELECT (loFormSet_lcRelLine)
            DELETE
          ENDSCAN
        ENDIF
      ENDIF    && End of IF
    ENDIF

    SELECT (loFormSet_lcTmpOrdLn)
    =RLOCK()
    REPLACE nProcNo   WITH 1
    UNLOCK

  ENDIF

  *-- Update PIKTKT file.
  IF nProcNo = 1
    IF !EMPTY(PIKTKT) AND (PIKTKT <> '******') AND SEEK(PikTkt,'PIKTKT') AND PIKTKT.STATUS <> 'X'
      IF llLastLine
        SELECT PIKTKT
        REPLACE STATUS WITH "X"
        =loAddUserInfo.DO('PIKTKT',.NULL.)


        *:B608316,1 MMT 10/11/2007 convert 3PL Provider Enhancemnt from 27[Start]
        IF 'AS' $ oAriaEnvironment.CompanyInstalledModules AND SEEK(cWareCode,'WareHous','WareHous') AND  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT','ACCFACT') AND ;
            SEEK(EDIACPRT.cPartCode+'940','EDIPD','PARTTRANS') AND SEEK('940'+PADR(PikTkt,40)+'W'+WareHous.cThrdPLPr,'EDITRANS','TYPEKEY')
          SELECT 'EDITRANS'
          DELETE
        ENDIF
        *:B608316,1 MMT 10/11/2007 convert 3PL Provider Enhancemnt from 27(End)


      ENDIF
    ENDIF

    SELECT (loFormSet_lcTmpOrdLn)
    =RLOCK()
    REPLACE nProcNo   WITH 2
    UNLOCK

  ENDIF

  IF nProcNo = 2
    *IF There is a record for this Style and Warehouse in the STYDYE file
    IF SEEK(STYLE + cWareCode + SPACE(10) , 'STYDYE')
      SELECT STYDYE
      = RLOCK()
      REPLACE Alo1   WITH Alo1   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik1') ,;
        Alo2   WITH Alo2   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik2') ,;
        Alo3   WITH Alo3   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik3') ,;
        Alo4   WITH Alo4   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik4') ,;
        Alo5   WITH Alo5   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik5') ,;
        Alo6   WITH Alo6   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik6') ,;
        Alo7   WITH Alo7   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik7') ,;
        Alo8   WITH Alo8   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik8') ,;
        TotAlo WITH TotAlo - EVALUATE(loFormSet_lcTmpOrdLn+'.TotPik')

      =loAddUserInfo.DO('STYDYE',.NULL.)
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 3
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF nProcNo = 3

    *IF There is a record for this Style and Warehouse and Dyelot in the
    * STYDYE file
    IF !EMPTY(DyeLot) .AND. SEEK(STYLE + cWareCode + DyeLot , 'STYDYE')
      SELECT STYDYE
      = RLOCK()
      REPLACE Alo1   WITH Alo1    - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik1'),;
        Alo2   WITH Alo2    - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik2'),;
        Alo3   WITH Alo3    - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik3'),;
        Alo4   WITH Alo4    - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik4'),;
        Alo5   WITH Alo5    - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik5'),;
        Alo6   WITH Alo6    - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik6'),;
        Alo7   WITH Alo7    - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik7'),;
        Alo8   WITH Alo8    - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik8'),;
        TotAlo WITH TotAlo  - EVALUATE(loFormSet_lcTmpOrdLn+'.TotPik'),;
        Ord1   WITH Ord1    - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1'),;
        Ord2   WITH Ord2    - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2'),;
        Ord3   WITH Ord3    - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3'),;
        Ord4   WITH Ord4    - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4'),;
        Ord5   WITH Ord5    - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5'),;
        Ord6   WITH Ord6    - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6'),;
        Ord7   WITH Ord7    - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7'),;
        Ord8   WITH Ord8    - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8'),;
        TotOrd   WITH TotOrd  - EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty')
      =loAddUserInfo.DO('STYDYE',.NULL.)
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 4
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF nProcNo = 4
    *IF There is a record for this Style in the STYLE file
    IF SEEK(STYLE , 'STYLE')
      SELECT STYLE
      = RLOCK()
      REPLACE Alo1   WITH Alo1   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik1'),;
        Alo2   WITH Alo2   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik2'),;
        Alo3   WITH Alo3   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik3'),;
        Alo4   WITH Alo4   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik4'),;
        Alo5   WITH Alo5   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik5'),;
        Alo6   WITH Alo6   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik6'),;
        Alo7   WITH Alo7   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik7'),;
        Alo8   WITH Alo8   - EVALUATE(loFormSet_lcTmpOrdLn+'.Pik8'),;
        TotAlo WITH TotAlo - EVALUATE(loFormSet_lcTmpOrdLn+'.TotPik')

      =loAddUserInfo.DO('STYLE',.NULL.)
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 5

    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF nProcNo = 5
    *IF There is a record for this Order line in the ORDLINE file
    IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
      SELECT ORDLINE
      = RLOCK()
      REPLACE Pik1    WITH 0 ,;
        Pik2    WITH 0 ,;
        Pik3    WITH 0 ,;
        Pik4    WITH 0 ,;
        Pik5    WITH 0 ,;
        Pik6    WITH 0 ,;
        Pik7    WITH 0 ,;
        Pik8    WITH 0 ,;
        TotPik  WITH 0 ,;
        PikDate WITH {} ,;
        PikTkt  WITH '' ,;
        Picked  WITH .F. ,;
        Dyelot  WITH EVALUATE(loFormSet_lcTmpOrdLn+'.cPeggedDye')

      =SEEK('O'+EVALUATE(loFormSet_lcTmpOrdLn+'.Order'),'OrdHdr')
      IF OrdHdr.cWareCode<>cWareCode
        REPLACE cWareCode WITH OrdHdr.cWareCode
      ENDIF

      =loAddUserInfo.DO('ORDLINE',.NULL.)
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 6
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF nProcNo = 6
    *IF This line Style was substituted befor and the the number of sizes for
    *the current Style is greater than the number of sizes for the Original
    *Style
    IF !EMPTY(AltStyle) .AND. lnCnT1 > lnCnT2
      lnCanc = 0          && Varible to hold the canceled quantity
      lnCancAmt = 0       && Varible to hold the canceled amount

      *FOR Loop to get the  canceled quantity and amount
      FOR lnElm = lnCnT2 + 1 TO lnCnT1
        lcElm = STR(lnElm , 1)
        lnCanc = lnCanc + Qty&lcElm
      ENDFOR    && End of FOR Loop
      lnCancAmt = lnCanc * Price
      SELECT ORDHDR
      *IF There is a record for this Order line in the ORDHDR file
      IF SEEK('O' + EVALUATE(loFormSet_lcTmpOrdLn+'.Order'))
        =RLOCK()
        REPLACE OPEN      WITH OPEN - lnCanc ,;
          OpenAmt   WITH OpenAmt - lnCancAmt ,;
          BookAmt   WITH BookAmt - lnCancAmt ,;
          Book      WITH Book - lnCanc

        =loAddUserInfo.DO('ORDHDR',.NULL.)
        UNLOCK
      ENDIF
    ENDIF    && End of IF


    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 7
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF nProcNo = 7
    IF !EMPTY(AltStyle)
      SELECT ORDLINE
      *IF There is a record for this Order line in the ORDLINE file
      IF SEEK('O' + ORDER + STR(LINENO , 6))
        =RLOCK()
        REPLACE Qty2 WITH IIF(lnCnT2 < 2 , 0 , Qty2) ,;
          Qty3 WITH IIF(lnCnT2 < 3 , 0 , Qty3) ,;
          Qty4 WITH IIF(lnCnT2 < 4 , 0 , Qty4) ,;
          Qty5 WITH IIF(lnCnT2 < 5 , 0 , Qty5) ,;
          Qty6 WITH IIF(lnCnT2 < 6 , 0 , Qty6) ,;
          Qty7 WITH IIF(lnCnT2 < 7 , 0 , Qty7) ,;
          Qty8 WITH IIF(lnCnT2 < 8 , 0 , Qty8) ,;
          TotQty WITH Qty1 + Qty2 + Qty3 + Qty4 + Qty5 + Qty6 + Qty7 + Qty8 ,;
          Book2 WITH IIF(lnCnT2 < 2 , 0 , Book1) ,;
          Book3 WITH IIF(lnCnT2 < 3 , 0 , Book3) ,;
          Book4 WITH IIF(lnCnT2 < 4 , 0 , Book4) ,;
          Book5 WITH IIF(lnCnT2 < 5 , 0 , Book5) ,;
          Book6 WITH IIF(lnCnT2 < 6 , 0 , Book6) ,;
          Book7 WITH IIF(lnCnT2 < 7 , 0 , Book7) ,;
          Book8 WITH IIF(lnCnT2 < 8 , 0 , Book8) ,;
          TotBook WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8 ,;
          STYLE WITH EVALUATE(loFormSet_lcTmpOrdLn+'.AltStyle') ,;
          AltStyle WITH SPACE(12) ,;
          SCALE    WITH lcOScale

        =loAddUserInfo.DO('ORDLINE',.NULL.)
        UNLOCK
      ENDIF    && End of IF
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 8
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF nProcNo = 8

    *IF This line Style was substituted befor and There is a record for the
    *Current Style and Warehouse in the STYDYE file
    IF !EMPTY(AltStyle) .AND. SEEK(STYLE + cWareCode + SPACE(10) , 'STYDYE')
      SELECT STYDYE
      = RLOCK()
      REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')) , 0) ,;
        Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) , 0) ,;
        Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) , 0) ,;
        Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) , 0) ,;
        Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) , 0) ,;
        Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) , 0) ,;
        Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) , 0) ,;
        Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) , 0) ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

      =loAddUserInfo.DO('STYDYE',.NULL.)
      UNLOCK

    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 9
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF nProcNo = 9
    *IF This line Style was substituted befor and There is a record for the
    *Original Style and Warehouse in the STYDYE file
    IF !EMPTY(AltStyle) .AND. SEEK(AltStyle + cWareCode + SPACE(10) , 'STYDYE')
      SELECT STYDYE
      = RLOCK()
      REPLACE Ord1 WITH (Ord1 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')),;
        Ord2 WITH (Ord2 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')),;
        Ord3 WITH (Ord3 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')),;
        Ord4 WITH (Ord4 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')),;
        Ord5 WITH (Ord5 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')),;
        Ord6 WITH (Ord6 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')),;
        Ord7 WITH (Ord7 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')),;
        Ord8 WITH (Ord8 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')),;
        TotOrd WITH (TotOrd + EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty'))

      =loAddUserInfo.DO('STYDYE',.NULL.)
      UNLOCK

    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 10
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF nProcNo = 10
    *IF This line Style was substituted befor and There is a record for the
    *Curren Style in the STYLE file
    IF !EMPTY(AltStyle) .AND. SEEK(STYLE , 'STYLE')
      SELECT STYLE
      = RLOCK()
      REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')) , 0) ,;
        Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')) , 0) ,;
        Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')) , 0) ,;
        Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')) , 0) ,;
        Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')) , 0) ,;
        Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')) , 0) ,;
        Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')) , 0) ,;
        Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')) , 0) ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

      =loAddUserInfo.DO('STYLE',.NULL.)
      UNLOCK

    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 11
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF nProcNo = 11
    *IF This line Style was substituted befor and There is a record for the
    *Original Style in the STYLE file
    IF !EMPTY(AltStyle) .AND. SEEK(AltStyle , 'STYLE')
      SELECT STYLE
      = RLOCK()
      REPLACE Ord1 WITH (Ord1 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')),;
        Ord2 WITH (Ord2 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')),;
        Ord3 WITH (Ord3 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')),;
        Ord4 WITH (Ord4 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')),;
        Ord5 WITH (Ord5 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')),;
        Ord6 WITH (Ord6 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')),;
        Ord7 WITH (Ord7 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')),;
        Ord8 WITH (Ord8 + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')),;
        TotOrd WITH (TotOrd + EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty'))

      =loAddUserInfo.DO('STYLE',.NULL.)
      UNLOCK

    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 12
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF nProcNo = 12
    REPLACE Pik1    WITH 0   ,;
      Pik2    WITH 0   ,;
      Pik3    WITH 0   ,;
      Pik4    WITH 0   ,;
      Pik5    WITH 0   ,;
      Pik6    WITH 0   ,;
      Pik7    WITH 0   ,;
      Pik8    WITH 0   ,;
      TotPik  WITH 0   ,;
      PikDate WITH {}  ,;
      PikTkt  WITH ''  ,;
      Picked  WITH .F. ,;
      Dyelot  WITH cPeggedDye  ,;
      nProcNo WITH 13
    REPLACE Alo1    WITH 0 ,;
      Alo2    WITH 0 ,;
      Alo3    WITH 0 ,;
      Alo4    WITH 0 ,;
      Alo5    WITH 0 ,;
      Alo6    WITH 0 ,;
      Alo7    WITH 0 ,;
      Alo8    WITH 0
    REPLACE cReason WITH ""
    =SEEK('O'+EVALUATE(loFormSet_lcTmpOrdLn+'.Order'),'OrdHdr')
    IF OrdHdr.cWareCode<>cWareCode
      REPLACE cWareCode WITH OrdHdr.cWareCode
    ENDIF
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF nProcNo = 13
    *IF This line Style was substituted befor
    IF !EMPTY(AltStyle)
      REPLACE Qty2 WITH IIF(lnCnT2 < 2 , 0 , Qty2) ,;
        Qty3 WITH IIF(lnCnT2 < 3 , 0 , Qty3) ,;
        Qty4 WITH IIF(lnCnT2 < 4 , 0 , Qty4) ,;
        Qty5 WITH IIF(lnCnT2 < 5 , 0 , Qty5) ,;
        Qty6 WITH IIF(lnCnT2 < 6 , 0 , Qty6) ,;
        Qty7 WITH IIF(lnCnT2 < 7 , 0 , Qty7) ,;
        Qty8 WITH IIF(lnCnT2 < 8 , 0 , Qty8) ,;
        TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
        STYLE WITH AltStyle ,;
        AltStyle WITH SPACE(12) ,;
        SCALE    WITH lcOScale

    ENDIF    && End of IF
    REPLACE nProcNo   WITH 99
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  IF SEEK('O'+EVALUATE(loFormSet_lcTmpOrdLn+'.Order'),'OrdHdr') AND ;
      OrdHdr.cWareCode <> EVALUATE(loFormSet_lcTmpOrdLn+'.cWareCode')
    REPLACE cWareCode WITH OrdHdr.cWareCode IN (loFormSet_lcTmpOrdLn)
  ENDIF

  *-- end of lfRelQty.

  *!*************************************************************
  *! Name      : lfBldRel
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Build temp. rele Pick alias.
  *!*************************************************************
FUNCTION lfBldRel
  PARAMETERS lcRelPik,loFormSet

  PRIVATE llFindLine , lcRelPik

  IF !USED(loFormSet_lcRelLine)
    CREATE TABLE (oAriaEnvironment.Workdir+loFormSet_lcRelLine) FROM ARRAY loFormSet_lafilestru
    SELECT (loFormSet_lcRelLine)
    DELETE ALL
  ENDIF

  SELECT (loFormSet_lcTmpOrdLn)
  SCATTER MEMVAR MEMO
  INSERT INTO (loFormSet_lcRelLine) FROM MEMVAR
  REPLACE PikTkt WITH SPACE(6)

  IF !USED(loFormSet_lcTmpRelPk)
    USE (oAriaEnvironment.Workdir+loFormSet_lcTmpOrdLn) AGAIN ALIAS (loFormSet_lcTmpRelPk) IN 0
  ENDIF
  SELECT (loFormSet_lcTmpRelPk)
  IF FILE(oAriaEnvironment.Workdir+loFormSet_lcTmpRelPk+'.CDX')
    SET ORDER TO TAG (loFormSet_lcTmpRelPk) OF (oAriaEnvironment.Workdir+loFormSet_lcTmpRelPk+'.CDX')
  ELSE
    INDEX ON PikTkt  TAG (loFormSet_lcTmpRelPk) OF (oAriaEnvironment.Workdir+loFormSet_lcTmpRelPk+'.CDX')
  ENDIF
  llFindLine = SEEK(lcRelPik)
  USE IN (loFormSet_lcTmpRelPk)

  SELECT (loFormSet_lcTmpOrdLn)
  REPLACE PikTkt WITH lcRelPik

  RETURN !llFindLine
  *-- end of lfBldRel.

  *!*************************************************************
  *! Name      : lfAloScr
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of push button Allocate
  *!*************************************************************
  *! Called from : Control Panel [Push button Allocate] , Option popup
  *!*************************************************************
  *! Calls       : lfAllocate() , lfvpbSel() , lfShowGets() ,
  *!               lfRefresh()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfAloScr
  PARAMETERS loFormSet
  SELECT (loFormSet_lcTmpOrdLn)


  lcStyle    = loFormSet_AriaForm1.cntDetail.keyStyle.VALUE
  lnWareCode = loFormSet_AriaForm1.cntDetail.cboLocation.VALUE
  lcWareCode = loFormSet_laWareHous[lnWareCode,2]
  lcDyelot   = loFormSet_AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE
  lnTotPik = loFormSet_AriaForm1.cntDetail.txtTotalPik.VALUE
  *IF Any of the Syle or Warehouse or Dyelot was changed
  IF lcStyle + lcWareCode + lcDyeLot <> STYLE + cWareCode + DyeLot
    llAloRec = IIF(lnTotPik = 0 , .F. , .T.)
  ELSE    && Else
    llAloRec = IIF(loFormSet_llIncOrd .OR. loFormSet_lnChangAlo > 0 , .T. , .F.)
  ENDIF    && End of IF

  *IF The current record was edited
  IF llAloRec
    =lfAllocate(3 , RECNO(loFormSet_lcTmpOrdLn),loFormSet)
  ENDIF    && End of IF

  llAloRec = .F.
  loFormSet_llIncOrd = .F.
  loFormSet_lnChangAlo = 0


  loFormSet_lnBrRecNo = RECNO(loFormSet_lcTmpOrdLn)

  =lfAllocate(2,.F.,loFormSet)

  SELECT (loFormSet_lcTmpOrdLn)

  *IF The system use Dyelots
  IF loFormSet_llUseDyes
    SET RELATION TO STYLE + cWareCode + DyeLot INTO STYDYE
  ELSE    && Else
    SET RELATION TO STYLE + cWareCode + SPACE(10) INTO STYDYE
  ENDIF    && End of IF
  GO loFormSet_lnBrRecNo


  *-- Handle status of tool bar and option menu
  *=lfHandlObj(loFormSet)


  =SEEK(STYLE , 'STYLE')
  =SEEK('S' + STYLE.SCALE , 'SCALE')

  *=lfBundFlds(loFormSet)

  *IF the current record is selected
  IF lnSel = 1
    *osa
    *=lfShowGet(.T.,loFormSet)
    *osa
  ELSE
    loFormSet_llCh3Stat = .F.
    loFormSet_laPikSt   = .F.
    *=lfDisblGts(loFormSet)
  ENDIF    && End of IF


  *!*************************************************************
  *! Name      : lfGetExp
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to create expression from the Option grid
  *!             variable filter array rows
  *!*************************************************************
  *! Called from : lfCreatExp()
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : 1)Option grid variable filter array row number ,
  *!                     2)Main file for the data selection
  *!                     3)Main field for the data selection
  *!                     4)Full index expression for the Optmization
  *!*************************************************************
  *! Return      : The expression string
  *!*************************************************************
  *
FUNCTION lfGetExp

  PARAMETERS lnElmeNumb , lcScopFile , lcScopFild , lcScopOptm


  PRIVATE lcReturn , lcField , lcTypeSep1 , lcTypeSep2 , lcVal

  lcReturn = ''          && Varible to hold the value to be returened

  lcField = ALLTRIM(UPPER(loFormSet_lafiltexp[lnElmeNumb , 1]))        && Varible to hold the field name

  *IF There is a Main file for the data selection
  IF !EMPTY(lcScopFile)
    lcField = STRTRAN(lcField , lcScopFile , '')
  ENDIF    && End of IF

  *IF There is a Main field for the data selection
  IF !EMPTY(lcScopFild)
    lcField = STRTRAN(lcField , lcScopFild , lcScopOptm)
  ENDIF    && End of IF

  lcTypeSep1 = IIF(loFormSet_lafiltexp[lnElmeNumb , 3] = 'C', '"' ,;
    IIF(loFormSet_lafiltexp[lnElmeNumb , 3] = 'D' , '{' , ''))    && Varible to hold the Begin seperator

  lcTypeSep2 = IIF(loFormSet_lafiltexp[lnElmeNumb , 3] = 'C', '"' ,;
    IIF(loFormSet_lafiltexp[lnElmeNumb , 3] = 'D' , '}' , ''))    && Varible to hold the End seperator

 
  *DO CASE Statment
  DO CASE
    CASE lcScopFild = 'PO'
      lcVal = lcTypeSep1 + 'PP' + STRTRAN(loFormSet_lafiltexp[lnElmeNumb , 6] , '|' ,;
        lcTypeSep2 + ',' + lcTypeSep1 + 'PP') + lcTypeSep2        && Varible to hold the right hand side of the expression

    CASE lcScopFild = 'ORDER' .AND.  lcField = lcScopOptm
      lcVal = lcTypeSep1 + 'O' + STRTRAN(loFormSet_lafiltexp[lnElmeNumb , 6] , '|' ,;
        lcTypeSep2 + ',' + lcTypeSep1 + 'O') + lcTypeSep2        && Varible to hold the right hand side of the expression

    CASE lcScopFild = 'FABRIC'
      SELECT (loFormSet_lafiltexp[lnElmeNumb , 6])
      lcValues = ""
      llFirst  = .T.
      SCAN
        lcValues = lcValues + IIF(llFirst,"","|") + PADR(CSTYMAJOR,7)
        llFirst  = .F.
      ENDSCAN
      lcVal = lcTypeSep1 + STRTRAN(lcValues , '|' ,lcTypeSep2 + ',' + lcTypeSep1) + lcTypeSep2        && Varible to hold the right hand side of the expression

    OTHERWISE
      IF loFormSet_lafiltexp[lnElmeNumb , 7] = "R"
        lcValues = ""
        llFirst  = .T.
        SELECT (loFormSet_lafiltexp[lnElmeNumb , 6])
        SCAN
		  lcValues = lcValues + IIF(llFirst,"","|") + EVALUATE(ALLTRIM(loFormSet_lafiltexp[lnElmeNumb,1]))
          llFirst  = .F.
        ENDSCAN
        lcVal = lcTypeSep1 + STRTRAN(lcValues , '|' ,lcTypeSep2 + ',' + lcTypeSep1) + lcTypeSep2        && Varible to hold the right hand side of the expression
      ELSE
        lcVal = lcTypeSep1 + STRTRAN(lfTStr(loFormSet_lafiltexp[lnElmeNumb , 6]) , '|' ,;
          lcTypeSep2 + ',' + lcTypeSep1) + lcTypeSep2        && Varible to hold the right hand side of the expression
      ENDIF

  ENDCASE    && End of DO CASE Statment

  *DO CASE Statment
  DO CASE

      *CASE The operator is [IS LIKE]
    CASE UPPER(ALLTRIM(loFormSet_lafiltexp[lnElmeNumb , 5])) = 'LIKE'
      lcReturn = lcField + ' ' +;
        IIF(loFormSet_lafiltexp[lnElmeNumb , 4] , '= ' , '<> ') + lcVal

      *CASE The operator is [GREATER THAN]
    CASE UPPER(ALLTRIM(loFormSet_lafiltexp[lnElmeNumb , 5])) = 'GREATER THAN'
      lcReturn = lcField + ' ' +;
        IIF(loFormSet_lafiltexp[lnElmeNumb , 4] , '> ' , '<= ') + lcVal

      *CASE The operator is [LESS THAN]
    CASE UPPER(ALLTRIM(loFormSet_lafiltexp[lnElmeNumb , 5])) = 'LESS THAN'
      lcReturn = lcField + ' ' +;
        IIF(loFormSet_lafiltexp[lnElmeNumb , 4] , '< ' , '>= ') + lcVal

      *CASE The operator is [GREATER OR EQUAL]
    CASE UPPER(ALLTRIM(loFormSet_lafiltexp[lnElmeNumb , 5])) = 'GREATER OR EQUAL'
      lcReturn = lcField + ' ' +;
        IIF(loFormSet_lafiltexp[lnElmeNumb , 4] , '>= ' , '< ') + lcVal

      *CASE The operator is [LESS OR EQUAL]
    CASE UPPER(ALLTRIM(loFormSet_lafiltexp[lnElmeNumb , 5])) = 'LESS OR EQUAL'
      lcReturn = lcField + ' ' +;
        IIF(loFormSet_lafiltexp[lnElmeNumb , 4] , '<= ' , '> ') + lcVal

      *CASE The operator is [BETWEEN]
    CASE UPPER(ALLTRIM(loFormSet_lafiltexp[lnElmeNumb , 5])) = 'BETWEEN'
      lcReturn = IIF(loFormSet_lafiltexp[lnElmeNumb , 4] , '' , '!') +;
        'BETWEEN(' + lcField + ',' + lcVal + ')'

      *CASE The operator is [IN LIST]
    CASE UPPER(ALLTRIM(loFormSet_lafiltexp[lnElmeNumb , 5])) = 'IN LIST'
      IF lcScopFild = 'FABRIC'
        lcField  = 'FABRIC'
      ENDIF
      *B607789,1 WAM 09/28/2006 Comment Hend changes and Return the original statment
      lcReturn = IIF(loFormSet_lafiltexp[lnElmeNumb , 4] , '' , '!') +;
        'INLIST(' + lcField + ',' + lcVal + ')'

      *B607674,1 HBG 07/26/2005 Fix bug of too many urgument in case of InList [Begin]
      *lcReturn = IIF(loFormSet_lafiltexp[lnElmeNumb , 4] , '' , '!') +;
      'INLIST(' + lcField + ',' + lcVal + ')'
      *lcReturn = "SEEK("+lcField +",'"+loFormSet_lafiltexp[lnElmeNumb , 6]+"')"
      *B607674,1 [End]
      *B607789,1 WAM 09/28/2006 (End)

  ENDCASE    && End of DO CASE Statment
  RETURN lcReturn

  *!*************************************************************
  *! Name      : lfGenPikTk
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to update the needed files to Generate a Pick
  *!             ticket for the current record of the temp. Order lines file
  *!*************************************************************
  *! Called from : lfGenScr()
  *!*************************************************************
  *! Calls       : gfAdd_Info()
  *!*************************************************************
  *! Passed Parameters : Pick ticket number
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfGenPikTk
  PARAMETERS lcParm,loFormSet

  *IF nProcNo [The step number (for the rollback)] equal 0
  IF nProcNo = 0
    REPLACE PikTkt  WITH lcParm ,;
      nProcNo WITH 1
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF


  *:B608316,1 MMT 10/11/2007 convert 3PL Provider Enhancemnt from 27[Start]
  lcTmpOrdLn = loFormSet_lcTmpOrdln
  IF 'AS' $ oAriaEnvironment.CompanyInstalledModules AND SEEK(&lcTmpOrdLn..cWareCode,'WareHous','WareHous') AND  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT','ACCFACT') AND ;
      SEEK(EDIACPRT.cPartCode+'940','EDIPD','PARTTRANS')
    lnUseAlas   = ALIAS()
    SELECT EDITRANS
    IF !SEEK('940'+PADR(&lcTmpOrdLn..PikTkt,40)+'W'+WareHous.cThrdPLPr,'EDITRANS','TYPEKEY')
      INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
        ('940',&lcTmpOrdLn..PikTkt,'W',WareHous.cThrdPLPr,EDIACPRT.lInterComp)
    ENDIF
    REPLACE cStatus WITH 'N'
    =loAddUserInfo.DO('EDITRANS')
    SELECT(lnUseAlas)
  ENDIF
  *:B608316,1 MMT 10/11/2007 convert 3PL Provider Enhancemnt from 27(End)



  PRIVATE lcOrdlOrd
  lcOrdlOrd = ORDER("ORDLINE")
  SET ORDER TO ORDLINE IN ORDLINE

  IF nProcNo = 1
    *IF There is a record for this Order line in the ORDLINE file
    IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
      SELECT ORDLINE
      = RLOCK()
      REPLACE PikTkt  WITH EVALUATE(loFormSet_lcTmpOrdLn+'.PikTkt') , PikDate WITH oAriaEnvironment.SystemDate

      =loAddUserInfo.DO('ORDLINE',.NULL.)
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 2
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 2
  IF nProcNo = 2

    SELECT PIKTKT

    *IF There is a no record for this Pick ticket number in the PIKTKT file
    *MAHXXX
    *-- IF !SEEK(EVALUATE(loFormSet_lcTmpOrdLn+'.PikTkt'))
    IF !SEEK(EVALUATE(loFormSet_lcTmpOrdLn + '.PikTkt'), "PikTkt", "PikTkt")
    *MAHXXX
      APPEND BLANK
      = RLOCK()
      REPLACE Account   WITH EVALUATE(loFormSet_lcTmpOrdLn+'.Account') ,;
        STORE     WITH EVALUATE(loFormSet_lcTmpOrdLn+'.Store') ,;
        ORDER     WITH EVALUATE(loFormSet_lcTmpOrdLn+'.Order') ,;
        PikTkt    WITH EVALUATE(loFormSet_lcTmpOrdLn+'.PikTkt') ,;
        DATE      WITH oAriaEnvironment.SystemDate ,;
        cWareCode WITH EVALUATE(loFormSet_lcTmpOrdLn+'.cWareCode') ,;
        CustPo    WITH IIF(ORDHDR.MultiPo , EVALUATE(loFormSet_lcTmpOrdLn+'.CustPo') , ORDHDR.CustPo) ,;
        STATUS    WITH 'O'

      =loAddUserInfo.DO('PIKTKT',.NULL.)
      UNLOCK
    ENDIF    && End of IF

    m.Order = ORDER
    m.Store = STORE
    m.cWareCode = cWareCode
    m.PikTkt = PikTkt
    m.cLineNo = STR(EVALUATE(loFormSet_lcTmpOrdLn+'.LineNo'),6)
    INSERT INTO (loFormSet_lcTmpPkTk) FROM MEMVAR

    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 3
    =RLOCK()
    UNLOCK

  ENDIF    && End of IF

  SET ORDER TO &lcOrdlOrd. IN ORDLINE


  *-- end of lfGenPikTk.


  *!*************************************************************
  *! Name      : lfUpdatPo
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Update Select by (PO).
  *!*************************************************************
FUNCTION lfUpdatPo
  PARAMETERS llPointSty,loFormSet

  PRIVATE lcScanExpr , lnProssCurt

  SELECT POSHDR1

  SELECT ORDLINE
  SET RELATION TO 'M'+ORDLINE.ACCOUNT INTO CUSTOMER ADDITIVE

  SELECT POSHDR1
  IF llPointSty
    lcScanExpr = [REST WHILE cBusDocu+cStyType+Po= "PP"+EVALUATE(loFormSet_lcOptmFile+'.PO')]
    lcCountExpr= [cBusDocu+cStyType+Po= "PP"+EVALUATE(loFormSet_lcOptmFile+'.PO')]
    =SEEK("PP"+EVALUATE(loFormSet_lcOptmFile+'.PO'),"POSHDR1")
  ELSE
    lcScanExpr = []
    lcCountExpr = []
  ENDIF
  lcWaitMsg = IIF(loFormSet_llExclude,LANG_AutoAlloc_WaitMsgExcl,LANG_AutoAlloc_WaitMsgSelc)
  lcExpr = lcCountExpr + IIF(EMPTY(lcCountExpr),'',' AND ') + lcForCond1
  COUNT FOR &lcExpr TO lnTotRec
  IF TYPE('loFormSet_oPross') = 'O'
    loFormSet_oPross.lblFirstLabel.CAPTION = lcWaitMsg + LANG_AutoAlloc_WaitMsgPO
    loFormSet_oPross.TotalProgress = lnTotRec
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    loFormSet_oPross.AUTOCENTER = .T.
    *HBG [End]
    loFormSet_oPross.SHOW()
  ENDIF
  lnProssCurt = 0
  IF llPointSty
    =SEEK("PP"+EVALUATE(loFormSet_lcOptmFile+'.PO'),"POSHDR1")
  ENDIF
  SCAN &lcScanExpr FOR &lcForCond1

    lnProssCurt = lnProssCurt + 1
    IF TYPE('loFormSet_oPross') = 'O'
      loFormSet_oPross.lblSecondLabel.CAPTION = lcWaitMsg + LANG_AutoAlloc_WaitMsgPO + PO
      loFormSet_oPross.CurrentProgress(lnProssCurt )
    ENDIF

    *IF There is lines for this PO in the CUTPICK file
    lcCutpKCond = "TranCd = '2' AND ctktno = '" + PO + "'"
    llCutpKBom = .F.
    IF lfOpenSql(loFormSet,'CUTPICK','CUTPICK1',lcCutpKCond)
      SELECT CUTPICK1
      LOCATE
      llCutpKBom = !EOF()
    ENDIF
    IF llCutpKBom
      SELECT CUTPICK1
      SCAN
        IF loFormSet_llExclude
          IF SEEK('O' + ORDER + cOrdLine , loFormSet_lcTmpOrdLn)
            SELECT (loFormSet_lcTmpOrdLn)
            SCAN REST FOR &lcForCond2
              IF (TOTCUT - CUTPICK1.TotQty) = 0
                =lfExclRec(loFormSet)
              ELSE
                REPLACE Cut1 WITH Cut1 - CUTPICK1.Qty1 ,;
                  Cut2 WITH Cut2 - CUTPICK1.Qty2 ,;
                  Cut3 WITH Cut3 - CUTPICK1.Qty3 ,;
                  Cut4 WITH Cut4 - CUTPICK1.Qty4 ,;
                  Cut5 WITH Cut5 - CUTPICK1.Qty5 ,;
                  Cut6 WITH Cut6 - CUTPICK1.Qty6 ,;
                  Cut7 WITH Cut7 - CUTPICK1.Qty7 ,;
                  Cut8 WITH Cut8 - CUTPICK1.Qty8 ,;
                  TotCut  WITH TotCut - CUTPICK1.TotQty

                IF !(lcOldSlct $ 'KP')
                  REPLACE ExcCut1 WITH ExcCut1 + CUTPICK1.Qty1 ,;
                    ExcCut2 WITH ExcCut2 + CUTPICK1.Qty2 ,;
                    ExcCut3 WITH ExcCut3 + CUTPICK1.Qty3 ,;
                    ExcCut4 WITH ExcCut4 + CUTPICK1.Qty4 ,;
                    ExcCut5 WITH ExcCut5 + CUTPICK1.Qty5 ,;
                    ExcCut6 WITH ExcCut6 + CUTPICK1.Qty6 ,;
                    ExcCut7 WITH ExcCut7 + CUTPICK1.Qty7 ,;
                    ExcCut8 WITH ExcCut8 + CUTPICK1.Qty8 ,;
                    TotExcCut  WITH TotExcCut + CUTPICK1.TotQty
                ENDIF
              ENDIF
            ENDSCAN
          ENDIF
        ELSE      && Normal scope case

          *IF There is no record for this CUTPICK order line in the Temp.
          *Order lines file
          IF !SEEK('O' + ORDER + cOrdLine , loFormSet_lcTmpOrdLn)

            *IF There is a record for this CUTPICK order line in the
            *ORDLINE file
            IF SEEK('O' + ORDER + cOrdLine , 'ORDLINE')
              *IF Statment to check the rest of the selection criteria
              *filter expression
              lnCrSelect = SELECT(0)
              SELECT ORDLINE
              IF &lcForCond2
                SCATTER MEMVAR MEMO
                m.Cut1 = CUTPICK1.Qty1
                m.Cut2 = CUTPICK1.Qty2
                m.Cut3 = CUTPICK1.Qty3
                m.Cut4 = CUTPICK1.Qty4
                m.Cut5 = CUTPICK1.Qty5
                m.Cut6 = CUTPICK1.Qty6
                m.Cut7 = CUTPICK1.Qty7
                m.Cut8 = CUTPICK1.Qty8
                m.TotCut = CUTPICK1.TotQty
                FOR lnI= 1  TO 8
                  lcI = STR(lnI,1)
                  m.Alo&lcI = m.Pik&lcI
                ENDFOR
                m.TotAlo = m.TotPik

                =lfUpdAloVr(loFormSet)
              ENDIF    && End of IF
              SELECT (lnCrSelect)
            ENDIF    && End of IF
          ELSE    && Else
            SELECT (loFormSet_lcTmpOrdLn)
            REPLACE Cut1 WITH Cut1 + CUTPICK1.Qty1 ,;
              Cut2 WITH Cut2 + CUTPICK1.Qty2 ,;
              Cut3 WITH Cut3 + CUTPICK1.Qty3 ,;
              Cut4 WITH Cut4 + CUTPICK1.Qty4 ,;
              Cut5 WITH Cut5 + CUTPICK1.Qty5 ,;
              Cut6 WITH Cut6 + CUTPICK1.Qty6 ,;
              Cut7 WITH Cut7 + CUTPICK1.Qty7 ,;
              Cut8 WITH Cut8 + CUTPICK1.Qty8 ,;
              TotCut  WITH TotCut + CUTPICK1.TotQty
          ENDIF    && End of IF
        ENDIF      && end if loFormSet_llExclude
        SELECT CUTPICK1
      ENDSCAN    && End of SCAN Loop
      SELECT POSHDR1
    ENDIF    && End of IF
  ENDSCAN    && End of SCAN Loop
  IF TYPE('loFormSet_oPross') = 'O'
    loFormSet_oPross.HIDE()
  ENDIF

  SELECT POSHDR1

  SELECT ORDLINE
  SET RELATION OFF INTO CUSTOMER
  SELECT POSHDR1
  *-- end of lfUpdatPo.

  *!*************************************************************
  *! Name      : lfUpdatCt
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Update Select by (C/T).
  *!*************************************************************
FUNCTION lfUpdatCt
  PARAMETERS llPointSty,loFormSet
  PRIVATE lcScanExpr, lnProssCurt


  SELECT POSHDR1
  SELECT ORDLINE
  SET RELATION TO 'M'+ORDLINE.ACCOUNT INTO CUSTOMER ADDITIVE
  SELECT POSHDR1
  LOCATE
  IF llPointSty
    lcScanExpr = [REST WHILE cBusDocu+cStyType+Po = 'PU' + EVALUATE(loFormSet_lcOptmFile+'.Po')]
    lcCountExpr= [cBusDocu+cStyType+Po = 'PU' + EVALUATE(loFormSet_lcOptmFile+'.Po')]
    =SEEK('PU'+EVALUATE(loFormSet_lcOptmFile+'.PO'),"POSHDR1")
  ELSE
    lcScanExpr = []
    lcCountExpr= []
  ENDIF
  lcWaitMsg = IIF(loFormSet_llExclude,LANG_AutoAlloc_WaitMsgExcl,LANG_AutoAlloc_WaitMsgSelc)
  lcExpr = lcCountExpr + IIF(EMPTY(lcCountExpr),'',' AND ') + lcForCond1
  COUNT FOR &lcExpr TO lnTotRec
  IF TYPE('loFormSet_oPross') = 'O'
    loFormSet_oPross.lblFirstLabel.CAPTION = lcWaitMsg + LANG_AutoAlloc_WaitMsgCT
    loFormSet_oPross.TotalProgress = lnTotRec
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    loFormSet_oPross.AUTOCENTER = .T.
    *HBG [End]
    loFormSet_oPross.SHOW()
  ENDIF
  lnProssCurt = 0
  IF llPointSty
    =SEEK('PU'+EVALUATE(loFormSet_lcOptmFile+'.PO'),"POSHDR1")
  ENDIF
  SCAN &lcScanExpr FOR &lcForCond1
    lnProssCurt = lnProssCurt + 1
    IF TYPE('loFormSet_oPross') = 'O'
      loFormSet_oPross.lblSecondLabel.CAPTION = lcWaitMsg + LANG_AutoAlloc_WaitMsgCT + PO
      loFormSet_oPross.CurrentProgress(lnProssCurt )
    ENDIF
     *IF There is lines for this Cut ticket in the CUTPICK file
    lcCutpKCond = "TranCd = '1' AND ctktno = '" + PO + "'"
    llCutpKBom = .F.
    IF lfOpenSql(loFormSet,'CUTPICK','CUTPICK1',lcCutpKCond)
      SELECT CUTPICK1
      LOCATE
      llCutpKBom = !EOF()
    ENDIF
    IF llCutpKBom
      SELECT CUTPICK1
      SCAN
        IF loFormSet_llExclude
          *E300989,1 This new code for exclude condition. [Begin]
          IF SEEK('O' + ORDER + cOrdLine , loFormSet_lcTmpOrdLn)
            SELECT (loFormSet_lcTmpOrdLn)
            SCAN REST FOR &lcForCond2
              IF (TOTCUT - CUTPICK1.TotQty) = 0
                =lfExclRec(loFormSet)
              ELSE

                REPLACE Cut1 WITH Cut1 - CUTPICK1.Qty1 ,;
                  Cut2 WITH Cut2 - CUTPICK1.Qty2 ,;
                  Cut3 WITH Cut3 - CUTPICK1.Qty3 ,;
                  Cut4 WITH Cut4 - CUTPICK1.Qty4 ,;
                  Cut5 WITH Cut5 - CUTPICK1.Qty5 ,;
                  Cut6 WITH Cut6 - CUTPICK1.Qty6 ,;
                  Cut7 WITH Cut7 - CUTPICK1.Qty7 ,;
                  Cut8 WITH Cut8 - CUTPICK1.Qty8 ,;
                  TotCut  WITH TotCut - CUTPICK1.TotQty

                IF !(lcOldSlct $ 'KP')
                  REPLACE ExcCut1 WITH ExcCut1 + CUTPICK1.Qty1 ,;
                    ExcCut2 WITH ExcCut2 + CUTPICK1.Qty2 ,;
                    ExcCut3 WITH ExcCut3 + CUTPICK1.Qty3 ,;
                    ExcCut4 WITH ExcCut4 + CUTPICK1.Qty4 ,;
                    ExcCut5 WITH ExcCut5 + CUTPICK1.Qty5 ,;
                    ExcCut6 WITH ExcCut6 + CUTPICK1.Qty6 ,;
                    ExcCut7 WITH ExcCut7 + CUTPICK1.Qty7 ,;
                    ExcCut8 WITH ExcCut8 + CUTPICK1.Qty8 ,;
                    TotExcCut  WITH TotExcCut + CUTPICK1.TotQty
                ENDIF

              ENDIF
            ENDSCAN
          ENDIF
        ELSE      && Normal scope case
          *IF There is no record for this CUTPICK order line in the Temp.
          *Order lines file
          IF !SEEK('O' + ORDER + cOrdLine , loFormSet_lcTmpOrdLn)

            *IF There is a record for this CUTPICK1 order line in the
            *ORDLINE file
            IF SEEK('O' + ORDER + cOrdLine , 'ORDLINE')
              SELECT ORDLINE

              *IF Statment to check the rest of the selection criteria
              *filter expression
              IF &lcForCond2
                SCATTER MEMVAR MEMO
                m.Cut1 = CUTPICK1.Qty1
                m.Cut2 = CUTPICK1.Qty2
                m.Cut3 = CUTPICK1.Qty3
                m.Cut4 = CUTPICK1.Qty4
                m.Cut5 = CUTPICK1.Qty5
                m.Cut6 = CUTPICK1.Qty6
                m.Cut7 = CUTPICK1.Qty7
                m.Cut8 = CUTPICK1.Qty8
                m.TotCut = CUTPICK1.TotQty
                FOR lnI= 1  TO 8
                  lcI = STR(lnI,1)
                  m.Alo&lcI = m.Pik&lcI
                ENDFOR
                m.TotAlo = m.TotPik

                =lfUpdAloVr(loFormSet)
              ENDIF    && End of IF
              SELECT CUTPICK1
            ENDIF    && End of IF
          ELSE    && Else
            SELECT (loFormSet_lcTmpOrdLn)
            REPLACE Cut1 WITH Cut1 + CUTPICK1.Qty1 ,;
              Cut2 WITH Cut2 + CUTPICK1.Qty2 ,;
              Cut3 WITH Cut3 + CUTPICK1.Qty3 ,;
              Cut4 WITH Cut4 + CUTPICK1.Qty4 ,;
              Cut5 WITH Cut5 + CUTPICK1.Qty5 ,;
              Cut6 WITH Cut6 + CUTPICK1.Qty6 ,;
              Cut7 WITH Cut7 + CUTPICK1.Qty7 ,;
              Cut8 WITH Cut8 + CUTPICK1.Qty8 ,;
              TotCut  WITH TotCut + CUTPICK1.TotQty

          ENDIF    && End of IF
        ENDIF  && end if loFormSet_llExclude
        SELECT CUTPICK1
      ENDSCAN    && End of SCAN Loop
      SELECT POSHDR1
    ENDIF    && End of IF
  ENDSCAN    && End of SCAN Loop
  IF TYPE('loFormSet_oPross') = 'O'
    loFormSet_oPross.HIDE()
  ENDIF

  SELECT POSHDR1
  SELECT ORDLINE
  SET RELATION OFF INTO CUSTOMER
  SELECT POSHDR1
  *-- end of lfUpdatCt.


  *!*************************************************************
  *! Name      : lfNormAlo
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : allocation with out dyelot (Separates)
  *!*************************************************************
  *! Called from : lfAllocate
  *!*************************************************************
  *! Calls       : lfNormProc,gfThermo
  *!*************************************************************
  *! Passed Parameters : Allocation case (All Records/Selected Records)
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : =lfNormAlo(1)
  *!*************************************************************
FUNCTION lfNormAlo
  PARAMETERS lnAlcCase,loFormSet

  *SCAN Loop to scan the temp. Order lines file for nProcNo less than 5
  *and to skip Style with dyelot Yes and empty dyelot field

  *osa
  *loFormSet_oPross.lblFirstLabel.Caption = LANG_AutoAlloc_ProgrsAllo
  *loFormSet_oPross.TotalProgress = lnTotRec
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *loFormSet_oPross.AutoCenter = .T.
  *HBG [End]
  *loFormSet_oPross.Show()
  *osa

  llCanAlo = .F.
  SCAN FOR nProcNo < 5 .AND. (STYLE.cDye_Flg # "Y") AND IIF(lnAlcCase=1,.T.,(lnSel <> 0))
    *IF The line is Allocated befor
    llCanAlo = .T.
    =lfNormProc(loFormSet)  && Normal process.
    *-- if you must skip this record, because it's allocated before.
    IF loFormSet_llMustLoop
      loFormSet_llMustLoop = .F.
      LOOP
    ENDIF
  ENDSCAN    && End of SCAN Loop
  IF !llCanAlo AND loFormSet_llUseDyes
    REPLACE ALL cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelReasMAsetup)
    loFormSet_llReject = .T.
  ENDIF

  *-- end of lfNormAlo.

  *!*************************************************************
  *! Name      : lfNormProc
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Normal allocation processes.
  *!*************************************************************
  *! Called from : lfNormAlo
  *!*************************************************************
  *! Calls       : lfTmpAlo,lfAloQty
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : =lfNormProc()
  *!*************************************************************
FUNCTION lfNormProc
  PARAMETERS loFormSet

  *IF there piktkt or totqty > totpik (Cancel allocation)
  *note that in dyelot version there is an error (does not update pikline file)
  *-- if this record is allocated before, but not picked.
  IF Picked AND !('******' $ PikTkt)
    =lfMarkLine(loFormSet)
    loFormSet_llMustLoop = .T.
    RETURN
  ENDIF    && End of IF

  *IF Force allocation is No [in the Option grid] and Pick coordinate
  *Min. % is greater than 0 and the Order line group is not empty
  IF !loFormSet_llRpForAlo .AND. loFormSet_lnRpPikCor > 0 .AND. !EMPTY(GROUP)
    lnRecNumb = RECNO()          && Variable to save the record number
    lcGroupStr =  ORDER + STORE + GROUP      && Variable to hold the Order number + Store + Group
    llAloGoup = .T.                     && Flag to know if we are going to allocate this group or not
    *IF Not an incompleted session and not the begin of the temp. Order
    *lines file and NPROCNO = 0
    IF !BOF() .AND. NPROCNO = 0
      SKIP -1
      llGrpFrRec = IIF(ORDER + STORE + GROUP = lcGroupStr , .F. , .T.)    && Flag to know if this is the first line in the group

      *IF Not the begin of the temp. Order lines file
      IF !BOF()
        SKIP 1
      ELSE    && Else
        LOCATE
      ENDIF    && End of IF
    ELSE    && Else
      llGrpFrRec = IIF(NPROCNO = 0 , .T. , .F.)    && Flag to know if this is the first line in the group
    ENDIF    && End of IF
    lcCurStyWr = ''

    *SCAN Loop to scan the temp. Order lines file IF this was the first
    *line in the group FOR the same Order and Store and Group
    SCAN REST WHILE llGrpFrRec .AND. ORDER + STORE + GROUP = lcGroupStr
      lnCurent = lnCurent + 1

      *IF We are going to Allocate this group
      IF llAloGoup
        lcPikWare = IIF(EMPTY(loFormSet_lcRpPkFWrh) , cWareCode , loFormSet_lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
        loFormSet_llCalWip = (lcCurStyWr = STYLE + cWareCode)
        llAloGoup = lfTmpAlo(.T. , lcPikWare , .F. , loFormSet_lnRpCutUnt , loFormSet_lnRpPikCor,loFormSet)
        lcCurStyWr = STYLE + cWareCode
      ENDIF    && End of IF
    ENDSCAN    && End of SCAN Loop

    *IF We are going to Allocate this group
    IF llAloGoup
      lnCurent = IIF(llGrpFrRec , lnCurent , lnCurent + 1)
      GO lnRecNumb
      *SCAN Loop to scan the temp. Order lines file FOR the same Order
      *and Store and Group
      SCAN REST WHILE ORDER + STORE + GROUP = lcGroupStr
        lcPikWare = IIF(EMPTY(loFormSet_lcRpPkFWrh) , cWareCode , loFormSet_lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
        =lfAloQty(lcPikWare,.F.,loFormSet)
      ENDSCAN    && End of SCAN Loop
    ENDIF    && End of IF
    SKIP -1
  ELSE    && Else (Force allocation or pick coordinate group min% = 0 or !empty(group))
    lcPikWare = IIF(EMPTY(loFormSet_lcRpPkFWrh) , cWareCode , loFormSet_lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
    lnCurent = lnCurent + 1
    IF loFormSet_llRpForAlo
      loFormSet_llCalWip = .F.
      llAloSep = lfTmpAlo(.F. , lcPikWare , .T.,.F.,.F.,loFormSet)  && Flag to know if we are going to allocate this record
      *IF We are going to allocate this record
      IF llAloSep
        =lfAloQty(lcPikWare,.F.,loFormSet)
      ENDIF    && End of IF
    ELSE    && Else (coordinate group min% > 0 or empty(group))
      llAloSep = lfTmpAlo(.T. , lcPikWare , .F. , loFormSet_lnRpCutUnt ,;
        IIF(EMPTY(GROUP) , loFormSet_lnRpPikSep , loFormSet_lnRpPikCor),loFormSet)      && Flag to know if we are going to allocate this record
      *IF We are going to force the allocation
      IF llAloSep
        =lfAloQty(lcPikWare,.F.,loFormSet)
      ENDIF    && End of IF
    ENDIF    && End of IF
  ENDIF    && End of IF

  *osa
  *loFormSet_oPross.lblSecondLabel.Caption = LANG_AutoAlloc_ProgrsOrdNum + Order
  *loFormSet_oPross.CurrentProgress(lnCurent)
  *osa

  *-- end of lfNormProc.

  *!*************************************************************
  *! Name      : lfMarkLine
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Mark current record because it's allocated before
  *!           : then increment allocation variables.
  *!*************************************************************
  *! Called from : lfDyeAlo
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfMarkLine()
  *!*************************************************************
FUNCTION lfMarkLine
  PARAMETERS loFormSet

  REPLACE lnSel WITH 1
  lnCurent = lnCurent + 1
  *-- end of lfMarkLine.

  *!*************************************************************
  *! Name      : lfTmpAlo
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to Allocate the availabel quantity in the
  *!             temp. Order lines file only.
  *!*************************************************************
  *! Called from : lfAllocate()
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : 1) [.T.] to Check if we will be able to Allocate
  *!                        first and then Allocate ,
  *!                        [.F.] to Allocate without checking
  *!                     2) The Warehouse to Allocate from
  *!                     3) [.T.] If we are going to force the allocation
  *!                        [.F.] Otherwise
  *!                     4) Cut of units
  *!                     5) Min. % to Allocate
  *!*************************************************************
  *! Return      : [.T.] If it was able to Allocate
  *!               [.F.] Otherwise
  *!*************************************************************
  *
FUNCTION lfTmpAlo
  PARAMETERS llParm , lcWareCode , llForce , lnCutUnt , lnMinPer , loFormSet

  PRIVATE llReturn , lnAvlFAlo , lnI , laOnHand , laWanted , laAllocPer
  lnI = 0
  DIMENSION laOnHand[8] , laWanted[8] , laAllocPer[8]
  STORE 0 TO laOnHand , laWanted
  laAllocPer = 1


  llForce = IIF(TYPE('llForce') <> 'L' , .F. ,  llForce)        && Flag to know if we are going to force the Allocation
  lnCutUnt = IIF(TYPE('lnCutUnt') <> 'N' , 0 , lnCutUnt)        && Variable to hold the Cut of units
  lnMinPer = IIF(TYPE('lnMinPer') <> 'N' , 0 , lnMinPer)        && Variable to hold the Min. % to Allocate
  llReturn = .T.            && Variable to hold the returned value
  lnAvlFAlo = 0             && Variable to hold the availabel for Allocation quantity
  lnTotReq  = 0             && Variable to hold the Required  for Allocation quantity

  *IF There is a record for this Style and Warehouse and Dyelot in the STYDYE
  *file
  IF SEEK(STYLE + lcWareCode + DyeLot , 'STYDYE')
    *IF We are to Check if we will be able to Allocate first and we are not
    *going to force the Allocation
    IF loFormSet_lcRpIncWip $ 'AS'  AND !loFormSet_llCalWip
      =IIF(loFormSet_lcRpIncWip = 'A',lfEvalWip(STYLE,lcWareCode,loFormSet),lfEvalShp(STYLE,lcWareCode,loFormSet))
    ELSE
      IF loFormSet_lcRpIncWip = 'N'
        loFormSet_laWIP = 0
      ENDIF
    ENDIF

    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      laOnHand[lnI] = ROUND((STYDYE.Stk&lcI+loFormSet_laWIP[lnI]-STYDYE.Alo&lcI-lnCutUnt) *;
        laAllocPer[lnI],0)
      laWanted[lnI] = MAX(IIF(loFormSet_lcRpScpMod $ 'KP',Cut&lcI,Qty&lcI)-Pik&lcI-ExcCut&lcI,0)
      lnAvlFAlo = lnAvlFAlo + MAX(MIN(laOnHand[lnI],laWanted[lnI]),0)
    ENDFOR

    IF llParm .AND. !llForce
      lnTotReq = IIF(loFormSet_lcRpScpMod $ 'KP',TotCut,TotQty) - TotPik - TotExcCut
      llReturn = lnAvlFAlo <> 0 .AND. (lnAvlFAlo/lnTotReq >= lnMinPer/100)

      IF !llReturn AND !PICKED
        DO CASE
          CASE lnAvlFAlo = 0
            REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelReasAvlQty)
            loFormSet_llReject = .T.
          CASE (lnAvlFAlo/lnTotReq < lnMinPer/100)
            IF EMPTY(GROUP)
              REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelResaAvlQtCov1 )
              loFormSet_llReject = .T.
            ELSE
              lcGroup = GROUP
              lnRecNo = RECNO()
              REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelResaAvlQtCov2) FOR GROUP = lcGroup
              loFormSet_llReject = .T.
              IF BETWEEN(lnRecNo,1,RECCOUNT())
                GOTO lnRecNo
              ENDIF
            ENDIF
        ENDCASE
      ENDIF
    ENDIF    && End of IF


    REPLACE Avl1    WITH laOnHand[1] ,;
      Avl2    WITH laOnHand[2] ,;
      Avl3    WITH laOnHand[3] ,;
      Avl4    WITH laOnHand[4] ,;
      Avl5    WITH laOnHand[5] ,;
      Avl6    WITH laOnHand[6] ,;
      Avl7    WITH laOnHand[7] ,;
      Avl8    WITH laOnHand[8] ,;
      TotAvl  WITH Avl1 + Avl2 + Avl3 + Avl4 + Avl5 + Avl6 + Avl7 + Avl8

    *IF We will be able to Allocate
    IF llReturn

      *IF We are not going to force the Allocation
      IF !llForce
        REPLACE PoAlo1    WITH MAX(MIN(laOnHand[1],laWanted[1]),0) ,;
          PoAlo2    WITH MAX(MIN(laOnHand[2],laWanted[2]),0) ,;
          PoAlo3    WITH MAX(MIN(laOnHand[3],laWanted[3]),0) ,;
          PoAlo4    WITH MAX(MIN(laOnHand[4],laWanted[4]),0) ,;
          PoAlo5    WITH MAX(MIN(laOnHand[5],laWanted[5]),0) ,;
          PoAlo6    WITH MAX(MIN(laOnHand[6],laWanted[6]),0) ,;
          PoAlo7    WITH MAX(MIN(laOnHand[7],laWanted[7]),0) ,;
          PoAlo8    WITH MAX(MIN(laOnHand[8],laWanted[8]),0) ,;
          Tot_PoAlo WITH PoAlo1 + PoAlo2 + PoAlo3 + PoAlo4 + PoAlo5 + PoAlo6 + PoAlo7 + PoAlo8

      ELSE    && Else
        REPLACE PoAlo1    WITH MAX(IIF(loFormSet_lcRpScpMod $ 'KP',Cut1,Qty1) - Pik1 - ExcCut1,0) ,;
          PoAlo2    WITH MAX(IIF(loFormSet_lcRpScpMod $ 'KP',Cut2,Qty2) - Pik2 - ExcCut2,0) ,;
          PoAlo3    WITH MAX(IIF(loFormSet_lcRpScpMod $ 'KP',Cut3,Qty3) - Pik3 - ExcCut3,0) ,;
          PoAlo4    WITH MAX(IIF(loFormSet_lcRpScpMod $ 'KP',Cut4,Qty4) - Pik4 - ExcCut4,0) ,;
          PoAlo5    WITH MAX(IIF(loFormSet_lcRpScpMod $ 'KP',Cut5,Qty5) - Pik5 - ExcCut5,0) ,;
          PoAlo6    WITH MAX(IIF(loFormSet_lcRpScpMod $ 'KP',Cut6,Qty6) - Pik6 - ExcCut6,0) ,;
          PoAlo7    WITH MAX(IIF(loFormSet_lcRpScpMod $ 'KP',Cut7,Qty7) - Pik7 - ExcCut7,0) ,;
          PoAlo8    WITH MAX(IIF(loFormSet_lcRpScpMod $ 'KP',Cut8,Qty8) - Pik8 - ExcCut8,0) ,;
          Tot_PoAlo WITH PoAlo1 + PoAlo2 + PoAlo3 + PoAlo4 + PoAlo5 + PoAlo6 + PoAlo7 + PoAlo8
      ENDIF    && End of IF

      =RLOCK()
      UNLOCK
    ENDIF    && End of IF
    RETURN llReturn
  ELSE    && Else
    IF !SEEK(STYLE + lcWareCode, 'STYDYE')
      REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelResaStyleNtAssgn+lcWareCode)
      loFormSet_llReject = .T.
    ELSE
      IF !EMPTY(Dyelot)
        REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,;
          IIF(loFormSet_llUseConfg,LANG_AutoAlloc_LabelReasCnfgNtAssgn1,LANG_AutoAlloc_LabelReasDyeNtAssgn1) + ;
          ALLTRIM(Dyelot) + LANG_AutoAlloc_LabelReasDyeNtAssgn2 + ALLTRIM(lcWareCode))
        loFormSet_llReject = .T.
      ENDIF
    ENDIF
    RETURN .F.
  ENDIF    && End of IF

  *!*************************************************************
  *! Name      : lfEvalWip
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Evaluate wip to add it to availablity if user want.
  *!*************************************************************
  *! Called from : lfCalAval
  *!*************************************************************
  *! Calls       : lfKPWip
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfEvalWip()
  *!*************************************************************
FUNCTION lfEvalWip
  PARAMETERS lcWipSty,lcWipWare,loFormSet
  PRIVATE lcCurAlias,lcStyDyRec

  lcCurAlias = SELECT(0)

  loFormSet_laWIP = 0
  *-- if select By C/T or P/O (Wips for selected only)
  IF loFormSet_lcRpScpMod $ 'KP' AND !EMPTY(loFormSet_laincexprs[1,6])
    = lfKPWip(lcWipSty,lcWipWare,'loFormSet_laincexprs',1)
    loFormSet_laWIP[9] = loFormSet_laWIP[1]+loFormSet_laWIP[2]+loFormSet_laWIP[3]+loFormSet_laWIP[4]+;
      loFormSet_laWIP[5]+loFormSet_laWIP[6]+loFormSet_laWIP[7]+loFormSet_laWIP[8]

    IF loFormSet_laWIP[9] < 0
      loFormSet_laWIP = 0
      RETURN
    ENDIF
  ELSE

    SELECT STYDYE
    lcStyDyRec = STYLE+CWARECODE+DYELOT  && Save current stydye record.
    =SEEK(lcWipSty+lcWipWare+SPACE(10))  && Seek master location record.
    SCATTER FIELDS WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8,TOTWIP TO loFormSet_laWIP
    =SEEK(lcStyDyRec)                    && Restore current stydye record.
  ENDIF

  *-- if exclude some records(C/t or P/o)
  IF (loFormSet_laWIP[9] > 0) AND loFormSet_llExclude AND (loFormSet_lcRpExSlct $ 'KP') AND;
      !EMPTY(loFormSet_lafiltexp[1,6])
    = lfKPWip(lcWipSty,lcWipWare,'loFormSet_lafiltexp',-1)
  ENDIF

  loFormSet_laWIP[9] = loFormSet_laWIP[1]+loFormSet_laWIP[2]+loFormSet_laWIP[3]+loFormSet_laWIP[4]+;
    loFormSet_laWIP[5]+loFormSet_laWIP[6]+loFormSet_laWIP[7]+loFormSet_laWIP[8]

  SELECT (loFormSet_lcTmpOrdLn)
  =RLOCK()
  REPLACE TOTWIP WITH loFormSet_laWIP[9]
  UNLOCK
  SELECT (lcCurAlias)
  *-- end of lfEvalWip.

  *!*************************************************************
  *! Name      : lfKPWip
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Evaluate wip C/t or P/o Cases.
  *!*************************************************************
  *! Called from : lfEvalWip
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfKPWip()
  *!*************************************************************
  *E300989,1
  *
FUNCTION lfKPWip
  PARAMETERS lcWipSty,lcWipWare,lcFiltArr,lnCalNo

  PRIVATE lcLineFile,lcKeyExpr,lcKey,lnSelct
  lnSelct = SELECT(0)
  lcKeyExpr = 'CBUSDOCU+CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD'

  lcJ = IIF(lnCalNo=1,'1','2')
  FOR lnStrPos = 1 TO ALEN(loFormSet_laString&lcJ,1)
    lcKey = 'P'+IIF(loFormSet_lcRpScpMod='K','U','P')+loFormSet_laString&lcJ[lnStrPos]+lcWipSty
    lcPOSLCond = "CBUSDOCU = 'P' AND CSTYTYPE = "+ IIF(loFormSet_lcRpScpMod='K','U','P') +" AND PO = '" +;
      loFormSet_laString&lcJ[lnStrPos]  + "' STYLE = '" + lcWipSty +"'"
    llPOSLNOpn = .F.
    IF lfOpenSql(loFormSet,'POSLN','POSLN1',lcPOSLCond)
      SELECT POSLN1
      LOCATE
      llPOSLNOpn = !EOF()
    ENDIF
    IF llPOSLNOpn
      SCAN FOR (cWareCode = lcWipWare)
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          loFormSet_laWIP[lnI] = loFormSet_laWIP[lnI] + IIF(Trancd='1',Qty&lcI,IIF(Trancd $ '245',-1*Qty&lcI,0)) * lnCalNo
        ENDFOR
      ENDSCAN
    ENDIF
  ENDFOR
  SELECT (lnSelct)
  *-- end of lfKPWip.

  *!*************************************************************
  *! Name      : lfAloQty
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to update the needed files to Allocate
  *!             the current record of the temp. Order lines file
  *!*************************************************************
  *! Called from : lfAllocate()
  *!*************************************************************
  *! Calls       : gfAdd_Info()
  *!*************************************************************
  *! Passed Parameters : The Warehouse to Allocate from
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfAloQty
  PARAMETERS lcWareCode,llHaveDye,loFormSet
  PRIVATE lnProcVal

  SELECT (loFormSet_lcTmpOrdLn)

  IF loFormSet_llChkAprov
    lcExced = lfChkAprov(loFormSet)
    IF  lcExced  = 'F' OR lcExced = 'E' && If Exceed befor or in this Line
      IF lcExced = 'E'  && If Exceed in this Line get the message
        *Message 44108 : The approved amount has covered some lines only in order XXXXX.
        *                Cannot allocate the rest.

        *osa
        *=gfModalGen("TRM44108B00000" , "DIALOG",EVALUATE(loFormSet_lcTmpOrdLn+'.Order'))
        *osa

      ENDIF
      *-- Rest The Alocated Qty of this line
      REPLACE PoAlo1  WITH 0,;
        PoAlo2  WITH 0,;
        PoAlo3  WITH 0,;
        PoAlo4  WITH 0,;
        PoAlo5  WITH 0,;
        PoAlo6  WITH 0,;
        PoAlo7  WITH 0,;
        PoAlo8  WITH 0,;
        Tot_PoAlo WITH 0 IN (loFormSet_lcTmpOrdLn)
      RETURN
    ENDIF
  ENDIF

  *IF nProcNo [The step number (for the rollback)] equal 0
  IF nProcNo = 0

    *IF There is a record for this Style and Warehouse in the STYDYE file
    IF SEEK(STYLE + lcWareCode + SPACE(10) , 'STYDYE')
      SELECT STYDYE
      = RLOCK()
      REPLACE Alo1      WITH Alo1   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo1') ,;
        Alo2      WITH Alo2   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo2') ,;
        Alo3      WITH Alo3   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo3') ,;
        Alo4      WITH Alo4   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo4') ,;
        Alo5      WITH Alo5   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo5') ,;
        Alo6      WITH Alo6   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo6') ,;
        Alo7      WITH Alo7   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo7') ,;
        Alo8      WITH Alo8   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo8') ,;
        TotAlo    WITH TotAlo + EVALUATE(loFormSet_lcTmpOrdLn+'.Tot_PoAlo')

      *osa
      =loAddUserInfo.DO('STYDYE',.NULL.)
      *osa

      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 1
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 1
  IF nProcNo = 1

    *IF There is a record for this Style and Warehouse and Dyelot in the
    * STYDYE file
    IF !EMPTY(DyeLot) .AND. SEEK(STYLE + lcWareCode + DyeLot , 'STYDYE')

      SELECT STYDYE
      = RLOCK()
      REPLACE Alo1   WITH Alo1   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo1') ,;
        Alo2   WITH Alo2   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo2') ,;
        Alo3   WITH Alo3   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo3') ,;
        Alo4   WITH Alo4   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo4') ,;
        Alo5   WITH Alo5   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo5') ,;
        Alo6   WITH Alo6   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo6') ,;
        Alo7   WITH Alo7   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo7') ,;
        Alo8   WITH Alo8   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo8') ,;
        TotAlo WITH TotAlo + EVALUATE(loFormSet_lcTmpOrdLn+'.Tot_PoAlo')

      IF !EVALUATE(loFormSet_lcTmpOrdLn+'.Picked')
        REPLACE Ord1 WITH Ord1   + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty1')  ,;
          Ord2 WITH Ord2   + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty2')  ,;
          Ord3 WITH Ord3   + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty3')  ,;
          Ord4 WITH Ord4   + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty4')  ,;
          Ord5 WITH Ord5   + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty5')  ,;
          Ord6 WITH Ord6   + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty6')  ,;
          Ord7 WITH Ord7   + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty7')  ,;
          Ord8 WITH Ord8   + EVALUATE(loFormSet_lcTmpOrdLn+'.Qty8')  ,;
          TotOrd WITH TotOrd + EVALUATE(loFormSet_lcTmpOrdLn+'.TotQty')
      ENDIF

      =loAddUserInfo.DO('STYDYE',.NULL.)
      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 2
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 2
  IF nProcNo = 2

    *IF There is a record for this Style in the STYLE file
    IF !EOF('STYLE')
      SELECT STYLE
      = RLOCK()
      REPLACE Alo1   WITH Alo1   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo1') ,;
        Alo2   WITH Alo2   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo2') ,;
        Alo3   WITH Alo3   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo3') ,;
        Alo4   WITH Alo4   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo4') ,;
        Alo5   WITH Alo5   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo5') ,;
        Alo6   WITH Alo6   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo6') ,;
        Alo7   WITH Alo7   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo7') ,;
        Alo8   WITH Alo8   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo8') ,;
        TotAlo WITH TotAlo + EVALUATE(loFormSet_lcTmpOrdLn+'.Tot_PoAlo')

      *osa
      *MMT
      *=loAddUserInfo('STYLE',.null.)
      =loAddUserInfo.DO('STYLE',.NULL.)
      *MMT
      *osa

      UNLOCK
    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 3
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF

  *IF nProcNo [The step number (for the rollback)] equal 3
  IF nProcNo = 3

    *IF There is a record for this Order line in the ORDLINE file
    IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
      SELECT ORDLINE
      = RLOCK()
      REPLACE Pik1      WITH Pik1   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo1') ,;
        Pik2      WITH Pik2   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo2') ,;
        Pik3      WITH Pik3   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo3') ,;
        Pik4      WITH Pik4   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo4') ,;
        Pik5      WITH Pik5   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo5') ,;
        Pik6      WITH Pik6   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo6') ,;
        Pik7      WITH Pik7   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo7') ,;
        Pik8      WITH Pik8   + EVALUATE(loFormSet_lcTmpOrdLn+'.PoAlo8') ,;
        TotPik    WITH TotPik + EVALUATE(loFormSet_lcTmpOrdLn+'.Tot_PoAlo') ,;
        PikDate   WITH oAriaEnvironment.SystemDate ,;
        PikTkt    WITH IIF(EMPTY(PikTkt),'******',PikTkt) ,;
        Picked    WITH .T. ,;
        cWareCode WITH lcWareCode,;
        DYELOT    WITH IIF(llHaveDye,EVALUATE(loFormSet_lcTmpOrdLn+'.Dyelot'),DYELOT)

      *osa
      =loAddUserInfo.DO('ORDLINE',.NULL.)
      *osa

      UNLOCK

    ENDIF    && End of IF
    SELECT (loFormSet_lcTmpOrdLn)
    REPLACE nProcNo   WITH 4
    =RLOCK()
    UNLOCK
  ENDIF    && End of IF


  lnProcVal = 4

  *IF nProcNo [The step number (for the rollback)] equal lnProcVal
  *IF nProcNo = 4
  IF nProcNo = lnProcVal

    =SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
    REPLACE Pik1      WITH Pik1   + PoAlo1 ,;
      Pik2      WITH Pik2   + PoAlo2 ,;
      Pik3      WITH Pik3   + PoAlo3 ,;
      Pik4      WITH Pik4   + PoAlo4 ,;
      Pik5      WITH Pik5   + PoAlo5 ,;
      Pik6      WITH Pik6   + PoAlo6 ,;
      Pik7      WITH Pik7   + PoAlo7 ,;
      Pik8      WITH Pik8   + PoAlo8 ,;
      TotPik    WITH TotPik + Tot_PoAlo ,;
      PikDate   WITH IIF(EOF('ORDLINE') , oAriaEnvironment.SystemDate , ORDLINE.PikDate) ,;
      PikTkt    WITH IIF(EMPTY(PikTkt),'******',PikTkt) ,;
      Picked    WITH .T. ,;
      cWareCode WITH lcWareCode ,;
      nProcNo   WITH 99 ,;
      lnSel     WITH 1

    =RLOCK()
    UNLOCK

  ENDIF    && End of IF
  *-- end of lfAloQty.

  *:**************************************************************************
  *: Name      : lfChkAprov
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *: Purpose   : Check Order Approve Amount.
  *:**************************************************************************
  *: Passed Parameters : None
  *:**************************************************************************
  *: Return      : ....
  *:**************************************************************************
  *: Example     : = lfChkAprov()
  *:**************************************************************************
  *:B804233,1
FUNCTION lfChkAprov
  PARAMETERS loFormSet

  IF !SEEK('O'+ORDER,loFormSet_lcTmpOrdAp)
    IF SEEK('O'+ORDER,'OrdHdr')
      INSERT INTO (loFormSet_lcTmpOrdAp) (TYPE,ORDER,AprAmnt,TotQty);
        VALUES ('O',OrdHdr.ORDER,OrdHdr.ApprAmt,EVALUATE(loFormSet_lcTmpOrdLn+'.Tot_PoAlo') * EVALUATE(loFormSet_lcTmpOrdLn+'.Price'))
    ENDIF
  ELSE
    IF EVALUATE(loFormSet_lcTmpOrdAp+'.lExceed')
      RETURN 'F'  && It Exceeded Before
    ELSE
      *wael
      *REPLACE TotQty WITH loFormSet_lcTmpOrdAp+'.TotQty' +;
      (EVALUATE(loFormSet_lcTmpOrdLn+'.Tot_PoAlo') * EVALUATE(loFormSet_lcTmpOrdLn+'.Price')) IN (loFormSet_lcTmpOrdAp)
      REPLACE TotQty WITH EVALUATE(loFormSet_lcTmpOrdAp+'.TotQty') +;
        (EVALUATE(loFormSet_lcTmpOrdLn+'.Tot_PoAlo') * EVALUATE(loFormSet_lcTmpOrdLn+'.Price')) IN (loFormSet_lcTmpOrdAp)
      *wael
    ENDIF
  ENDIF

  IF EVALUATE(loFormSet_lcTmpOrdAp+'.TotQty') > EVALUATE(loFormSet_lcTmpOrdAp+'.AprAmnt')
    REPLACE lExceed WITH .T. IN (loFormSet_lcTmpOrdAp)
    RETURN 'E'  && It Ecxeeds Now
  ELSE
    REPLACE lExceed WITH .F. IN (loFormSet_lcTmpOrdAp)
    RETURN 'N'  && Not Exceed
  ENDIF


  *!*************************************************************
  *! Name      : lfDyeAlo
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : dyelot allocation.
  *!*************************************************************
  *! Called from : lfAllocate
  *!*************************************************************
  *! Calls       : lfNormProc , gfThermo
  *!*************************************************************
  *! Passed Parameters : Allocation case (All Records/Selected Records)
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : =lfDyeAlo(1)
  *!*************************************************************
FUNCTION lfDyeAlo
  PARAMETERS lnFromOgMn,loFormSet

  PRIVATE lnActvLine , laDye_Rel
  lnActvLine = 0
  DECLARE laDye_Rel[1]
  laDye_Rel = ""
  llSaveCond = loFormSet_llRpCond

  *-- scan file for selected records.
  SCAN FOR nProcNo < 5 AND IIF(lnFromOgMn = 2,lnSel <> 0,.T.)
    lcFabric = PADR(Fabric,loFormSet_lnFabMajor)+loFormSet_lcFabSep+PADR(cFabColor,loFormSet_lnFabNonMaj)
    IF EMPTY(cFabColor) OR !SEEK('0002'+lcFabric,'DYE_REL1') OR loFormSet_llUseConfg
      =(STYLE.cDye_Flg = "N" OR !EMPTY(Dyelot)) AND lfNormProc(loFormSet)
      IF STYLE.cDye_Flg = "Y" AND EMPTY(Dyelot)
        REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,IIF(loFormSet_llUseConfg,LANG_AutoAlloc_LabelReasConfigAssign,LANG_AutoAlloc_LabelReasDyelotAssign))
        loFormSet_llReject = .T.
      ENDIF
      loFormSet_llMustLoop = .F.
      LOOP
    ENDIF

    loFormSet_llRpCond = !EMPTY(GROUP) AND llSaveCond
    *-- if we want to allocate separate line conditionally only.
    IF EMPTY(GROUP) AND (loFormSet_lnRpPikSep > 0) AND loFormSet_llRpCond
      PRIVATE lnActivRec
      lnActivRec = RECNO()
      SET FILTER TO RECNO() = lnActivRec
    ENDIF

    lcGrpExpr   = loFormSet_laindexexp[loFormSet_lnRpSort1] + ' + ' +loFormSet_laindexexp[loFormSet_lnRpSort2] + ' + ' +;
      loFormSet_laindexexp[loFormSet_lnRpSort3] + IIF(INLIST(4 , loFormSet_lnRpSort1 , loFormSet_lnRpSort2 ,;
      loFormSet_lnRpSort3) , '' , '+ ORDER') +;
      ' + STORE + FABRIC + CFABCOLOR + GROUP'
    lcCurGrpVal = EVALUATE(lcGrpExpr)

    *-- if it's the last scaned group and no allocation.
    IF lcOldGroup = lcCurGrpVal
      loFormSet_llMustLoop = .F.
      = lfNormProc(loFormSet)
      LOOP
    ENDIF
    lcAllWare  = ''   && Allocation location
    lcStartDye = ''   && Start dyelot
    lcPegged   = ''   && pegged dyelot
    lcOldSWare = ''   && old style/location for group lines.
    lcCurStyWr = ''   && old style/location for calculate wips.
    llEndProc  = .F.  && .T. if we end dyelot alloc. logic process.
    llAnOther  = .F.  && .T. if we must search for another dyelot.
    llNoDye    = .F.  && .T. if loop all dyelots and does have matching availability.
    llGrpStart = .T.  && .T. if we are at the start of group.
    llPitch    = .F.  && Pitch dyelot relation table is initially .F. for current group.

    *-- laOpnStock : Array hold available quantity.
    DIMENSION laOpnStock[9]
    STORE 0 TO laOpnStock

    *-- Process step value (descriped in the following lines)
    lnProcStep = 1

    *-- lnSkipTo : Skip up and down in dyelot relationship file(-1,+2,-3,+4)
    STORE 1 TO lnSkipTo
    lcLastSkip = 'F'  && last skip 'F' means Forward and 'B' backword.

    *-- loop until finish allocation process or exit.
    DO WHILE !llEndProc

      *-- loop Around current group.
      SCAN REST WHILE (&lcGrpExpr = lcCurGrpVal) ;
          FOR IIF(lnFromOgMn = 2,lnSel <> 0,.T.)
        =SEEK(STYLE+CWARECODE+DYELOT,'STYDYE')
        lcAllWare = IIF(EMPTY(loFormSet_lcRpPkFWrh) , cWareCode , loFormSet_lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
        *-- save current stydye record.
        lcStyDyeRc = STYDYE.STYLE+STYDYE.CWARECODE+STYDYE.DYELOT
        *-- if all process not finished and not find this dyelot in stydye file.
        IF !llEndProc AND !SEEK(STYLE+lcAllWare,'STYDYE')
          llEndProc = .T.  && end all process
          lnProcStep = 2   && step became 2.
        ENDIF  && end if all process not finished and not find this dyelot in stydye file.
        = SEEK(lcStyDyeRc,'STYDYE')  && Restore previous stydye record.
        DO CASE
            *-- First step check if there is pegged dyelot or start from
            *-- begin of dyelot relationship file.
          CASE lnProcStep = 1  && get pegged dyelot.
            *-- previous dyelot does not have sufficient quantity,[Begin
            *-- to allocate then get another one.
            IF llAnOther
              llAnOther  = .F.
              lcFabric = PADR(Fabric,loFormSet_lnFabMajor)+loFormSet_lcFabSep+PADR(cFabColor,loFormSet_lnFabNonMaj)
              llNoDye = !lfAnother(lcFabric)  && Find another closet dyelot.
              *-- if you still have dyelots.
              IF !llNoDye
                lcStartDye = DYE_REL1.DYELOT  && Select Current start dyelot
              ENDIF
              EXIT
            ELSE   && have pegged dyelot or start with new dyelot.
              lcStartDye    = IIF(EMPTY(lcStartDye),DYELOT,lcStartDye)
              lcPegged      = lcStartDye
              IF !EMPTY(lcStartDye)
                EXIT  && to start check for availability step.
              ENDIF
            ENDIF
            *-- previous dyelot does not have sufficient quantity,[End..

            *-- Second step check availability for current group, for
            *-- dyelot selected in step 1.
          CASE lnProcStep = 2  && check STYDYE availability for this group.

            IF !llEndProc
              *-- if you find this record in stydye calculate its OpnStock.
              *-- and if insufficient select another dyelot.
              IF !lfCalAval(SEEK(STYLE+lcAllWare+lcStartDye,'STYDYE'))
                llAnOther = .T.
                EXIT
              ENDIF
              lcOldSWare = STYLE+CWARECODE
            ENDIF

            *-- Third step Allocate sufficient group.
          CASE lnProcStep = 3  && Allocate Current group.

            *-- if this line have new allocated
            IF Tot_PoAlo > 0
              = lfAlocMast(loFormSet)   && Allocate in master files.
            ELSE  && else if there is previously allocation and came from OG.
              *-- if allocated before.
              IF TotPik <> 0 AND lnFromOgMn = 1
                = lfMarkLine(loFormSet)  && Mark record and increment allocation variables.
                LOOP          && Go to next record in current group.
              ENDIF    && End of IF
            ENDIF

            *-- Fourth step if and only if fail in allocation and user
            *-- want to allocate unconditional.
          CASE lnProcStep = 4  && Unconditional Allocate Current group.

            lcPegged = DYELOT
            SET ORDER TO TAG DYE_REL IN DYE_REL1
            *-- if you find this Fabric/Color pair in Dyelot Relation file.
            lcFabric = PADR(Fabric,loFormSet_lnFabMajor)+loFormSet_lcFabSep+PADR(cFabColor,loFormSet_lnFabNonMaj)

            IF SEEK('0002'+lcFABRIC+IIF(EMPTY(lcPegged),'',lcPegged),'DYE_REL1')
              SET ORDER TO TAG SEQUENCE IN DYE_REL1

              *-- llFindDye : .T. if you find your allocation dyelot.
              llFindDye = .F.

              *-- if there is no previously pegged dyelot.
              IF EMPTY(lcPegged)
                *-- if you find dyelot relation dyelot in stydye[Begin
                *-- exit loop else loop all dyelot relation records for
                *-- current Fabric/Color pair.
                DO WHILE .T.
                  IF SEEK(STYLE+lcAllWare+DYE_REL1.DYELOT,'STYDYE')
                    llFindDye = .T.
                    EXIT
                  ELSE
                    SKIP 1 IN DYE_REL1
                    lcFabric = PADR(Fabric,loFormSet_lnFabMajor)+loFormSet_lcFabSep+PADR(cFabColor,loFormSet_lnFabNonMaj)
                    IF DYE_REL1.cItem # lcFABRIC
                      llFindDye = .F.
                      EXIT
                    ENDIF
                  ENDIF
                ENDDO
                *-- if you find dyelot relation dyelot in stydye[End..
              ELSE  && else there pegged dyelot  .
                *-- The following means if there pegged dyelot to this
                *-- line or any other start allocation with it when find it
                *-- in stydye file, else try with another.
                llFindDye = SEEK(STYLE+lcAllWare+lcPegged,'STYDYE')
              ENDIF  && end if there is no previously pegged dyelot.
              STORE .F. TO llNoDye,llPitch
              lcLastSkip = 'F'
              lnSkipTo   = 1

              *-- loop while you find dyelot.
              DO WHILE llFindDye

                IF lfCalAval(.T.,.T.)
                  llFindDye = .F.  && to evaluate the next line.

                  IF Tot_PoAlo > 0
                    = lfAlocMast(loFormSet)   && Allocate to master files.
                  ELSE
                    IF TotPik <> 0 AND lnFromOgMn = 1
                      = lfMarkLine(loFormSet)  && Mark record and increment alloc. variables.
                    ENDIF    && End of IF
                  ENDIF
                ELSE
                  *-- loop to find another dyelots.
                  DO WHILE .T.

                    *-- if you did not collect this data before.
                    IF (lnActvLine <> RECNO(loFormSet_lcTmpOrdLn))
                      lcFabric = PADR(Fabric,loFormSet_lnFabMajor)+loFormSet_lcFabSep+PADR(cFabColor,loFormSet_lnFabNonMaj)
                      =lfDyeRel(lcFabric)
                    ENDIF
                    llNoDye = EMPTY(laDye_Rel) OR (ALEN(laDye_Rel,1) = 1)

                    *-- if previous dyelot is last one in this group.
                    IF llNoDye
                      EXIT
                    ELSE  && else you still have dyelots in dyelot relation file.
                      lcFabric = PADR(Fabric,loFormSet_lnFabMajor)+loFormSet_lcFabSep+PADR(cFabColor,loFormSet_lnFabNonMaj)
                      IF EMPTY(lfGetDyelt(lcFabric))
                        llNoDye = .T.
                        EXIT
                      ENDIF
                      *-- if you find this dyelot in stydye file.
                      IF SEEK(STYLE+lcAllWare+DYE_REL1.DYELOT,'STYDYE')
                        EXIT
                      ELSE  && this dyelot not in stydye file.
                        LOOP
                      ENDIF  && end if you find this dyelot in stydye file.
                    ENDIF    && end if previous dyelot is last one in this group.
                  ENDDO  && end loop to find another dyelots.
                  llFindDye = !llNoDye
                ENDIF
              ENDDO

            ENDIF  && end if you find this Fabric/Color pair in Dyelot Relation file.

            IF lnSel = 0 AND TotPik <> 0 AND lnFromOgMn = 1
              = lfMarkLine(loFormSet)  && Mark record and increment alloc. variables.
            ENDIF    && End of IF
        ENDCASE
      ENDSCAN  && end scan current group.

      *-- if step is to find dyelot avaialable for whole group.
      IF lnProcStep < 3
        = SEEK(lcCurGrpVal)  && Restore start of group.
      ELSE
        SKIP -1
      ENDIF  && end if step is to find dyelot avaialable for whole group.

      DO CASE
        CASE lnProcStep = 1

          *-- if no dyelots pegged to this group.
          *-- repeate previous steps with start of dyelot relation file.
          IF EMPTY(lcStartDye)
            *-- if Find matching dyelots for this Fabric/Color
            lcFabric = PADR(Fabric,loFormSet_lnFabMajor)+loFormSet_lcFabSep+PADR(cFabColor,loFormSet_lnFabNonMaj)
            IF SEEK('0002'+lcFabric,'DYE_REL1')
              lcStartDye = DYE_REL1.Dyelot
            ELSE  && No Matching dyelots for this Fabric/Color
              REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelReasNOFabDye+lcFabric)
              loFormSet_llReject = .T.
              llEndProc = .T.
              =SEEK(lcCurGrpVal)
              lcOldGroup = lcCurGrpVal
              loFormSet_llMustLoop = .F.
              = lfNormProc(loFormSet)
              LOOP
            ENDIF  && end if Find matching dyelots for this Fabric/Color.

          ELSE     && Find dyelot pegged to this group.

            IF llGrpStart AND !EMPTY(lcPegged)
              SET ORDER TO TAG DYE_REL IN DYE_REL1
              lcFabric = PADR(Fabric,loFormSet_lnFabMajor)+loFormSet_lcFabSep+PADR(cFabColor,loFormSet_lnFabNonMaj)
              IF SEEK('0002'+lcFabric+lcStartDye,'DYE_REL1')
                llGrpStart = .F.  && do not enter this code again.
              ELSE
                REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelReasSameFabDye)
                loFormSet_llReject = .T.
                llEndProc = .T.  && process ended from first step.
                =SEEK(lcCurGrpVal)
                lcOldGroup = lcCurGrpVal
                loFormSet_llMustLoop = .F.
                = lfNormProc(loFormSet)
                LOOP  && end this group.
              ENDIF
              SET ORDER TO TAG SEQUENCE IN DYE_REL1
            ENDIF
          ENDIF    && end if no dyelots pegged to this group.

          IF (lnProcStep # 4) AND !llNoDye AND !SEEK(STYLE+lcAllWare+lcStartDye,'STYDYE')
            llAnOther = .T.
          ENDIF

        CASE lnProcStep = 2

          *-- if no dyelots have availability.
          IF llEndProc  && no stydye records for one or more Style(s).
            LOOP
          ENDIF

          *-- current group was allocated.
        CASE lnProcStep = 3 OR lnProcStep = 4
          llEndProc  = .T.
      ENDCASE

      *-- If None of Dye_Rel Dyelots was found in StyDye file OR no. availability.
      IF (lnProcStep # 4) AND llNoDye

        *-- if user allocate conditionally and not to force allocation.
        IF !loFormSe.llRpForAlo AND loFormSet_llRpCond

          *-- if we want to allocate separate line conditionally only.
          IF EMPTY(GROUP) AND (loFormSet_lnRpPikSep > 0)
            SET FILTER TO
            IF BETWEEN(lnActivRec,1,RECCOUNT())
              GO lnActivRec    && Record pointer is on tested record
              SKIP  && Skip Current Line
              *-- if it is new group
              IF &lcGrpExpr <> lcCurGrpVal
                llEndProc = .T.
              ENDIF
              GO lnActivRec    && Again Re-Point Active record
            ENDIF
          ELSE
            LOCATE REST FOR &lcGrpExpr > lcCurGrpVal  && Skip current group
            llEndProc = .T.
          ENDIF
        ELSE  && User want to force allocation or want to allocate unconditionally.
          lnProcStep = 4
        ENDIF
      ENDIF    && end If None of Dye_Rel Dyelots was found in StyDye file.

      lnProcStep = IIF(lnProcStep = 4,lnProcStep,IIF(llAnOther,1,lnProcStep+1))
    ENDDO  && end loop until finish allocation process or exit.

    IF TYPE('loFormSet_oPross') = 'O'
      loFormSet_oPross.lblSecondLabel.CAPTION = LANG_AutoAlloc_ProgrsOrdNum + ORDER
      loFormSet_oPross.CurrentProgress(lnCurent)
    ENDIF

    *-- if we want to allocate separate line conditionally only.
    IF EMPTY(GROUP) AND (loFormSet_lnRpPikSep > 0) AND loFormSet_llRpCond
      SET FILTER TO
    ENDIF

  ENDSCAN
  *-- end of lfDyeAlo.

  *!*************************************************************
  *! Name      : lfAnother
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Get another closest dyelot, to current dyelot.
  *!*************************************************************
  *! Called from : lfDyeAlo
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : Fabric/Color Pair
  *!*************************************************************
  *! Return      : .T. if you find another dyelot.
  *!*************************************************************
  *! Example     : =lfAnother()
  *!*************************************************************
FUNCTION lfAnother
  PARAMETERS lcFabClrPr

  *-- if no pegged dyelot Start with Top of Fabric/Color group,
  *-- and step is always 1 and forward.
  IF llPitch OR EMPTY(lcPegged)
    IF EMPTY(lcPegged)
      lnPitch = 1
    ENDIF
    SKIP lnPitch IN DYE_REL1
    IF (lnPitch = 1 AND EOF('DYE_REL1')) OR BOF('DYE_REL1') OR ;
        (DYE_REL1.citem # lcFabClrPr)  OR ;
        (DYE_REL1.DYELOT = lcStartDye)
      llPitch = .F.
      RETURN .F.
    ENDIF
  ELSE  && else there is pegged dyelot then Start from within Fabric/Color group
    IF lcLastSkip = 'F'
      SKIP -lnSkipTo IN DYE_REL1
      lcLastSkip = 'B'
      IF BOF('DYE_REL1') OR ;
          (DYE_REL1.citem # lcFabClrPr) OR ;
          (DYE_REL1.DYELOT = lcStartDye)

        SKIP lnSkipTo IN DYE_REL1
        IF EOF('DYE_REL1') OR ;
            (DYE_REL1.citem # lcFabClrPr) OR ;
            (DYE_REL1.DYELOT = lcStartDye)
          RETURN .F.
        ELSE
          llPitch = .T.
          lnPitch = 1
        ENDIF
      ENDIF
    ELSE
      SKIP lnSkipTo IN DYE_REL1
      lcLastSkip = 'F'
      IF EOF('DYE_REL1') OR ;
          (DYE_REL1.citem # lcFabClrPr) OR ;
          (DYE_REL1.DYELOT = lcStartDye)
        SKIP -(lnSkipTo+1) IN DYE_REL1
        IF BOF('DYE_REL1') OR ;
            (DYE_REL1.citem # lcFabClrPr) OR ;
            (DYE_REL1.DYELOT = lcStartDye)
          RETURN .F.
        ELSE
          llPitch = .T.
          lnPitch = -1
        ENDIF
      ENDIF
    ENDIF
    lnSkipTo = lnSkipTo + 1
  ENDIF
  RETURN .T.

  *!*************************************************************
  *! Name      : lfGetDyelt
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Get another closest dyelot for the selected Fabric Color pair
  *!*************************************************************
FUNCTION lfGetDyelt
  PARAMETERS lcFabClrPr
  PRIVATE lcFabClrPr , lnInDyeArry , lcRetDye

  lcRetDye = ""
  lnInDyeArry = ASCAN(laDye_Rel,DYE_REL1.DYELOT)
  IF lnInDyeArry > 0
    lnInDyeArry = ASUBSCRIPT(laDye_Rel,lnInDyeArry,1)
    =ADEL(laDye_Rel,lnInDyeArry)
    DECLARE laDye_Rel[ALEN(laDye_Rel,1) - 1 , ALEN(laDye_Rel,2)]
    lnInDyeArry = ASCAN(laDye_Rel,DYE_REL1.DYELOT)
    IF lnInDyeArry > 0
      IF ALEN(laDye_Rel,1) = 1
        lcRetDye  = laDye_Rel[1,1]
        laDye_Rel = ""
      ELSE
        IF EMPTY(lcPegged) OR (lnInDyeArry = 1)
          lcRetDye  = laDye_Rel[1,1]
        ELSE
          IF lcLastSkip = 'F'
            lcLastSkip = 'B'
            lcRetDye  = laDye_Rel[lnInDyeArry - 1,1]
          ELSE
            lcLastSkip = 'F'
            lcRetDye  = laDye_Rel[lnInDyeArry,1]
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    PRIVATE lnActAlias , lcDyRelOrd
    lnActAlias = SELECT(0)
    SELECT DYE_REL1
    lcDyRelOrd = ORDER()
    SET ORDER TO Dye_rel
    =SEEK('0002'+lcFabClrPr + lcRetDye , "DYE_REL1")
    SET ORDER TO &lcDyRelOrd
    SELECT (lnActAlias)
  ENDIF

  RETURN lcRetDye
  *-- end of lfGetDyelt.

  *!*************************************************************
  *! Name      : lfDyeRel
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Fill Dyelot array with all dyelots for the selected Fabric Color pair
  *!*************************************************************
FUNCTION lfDyeRel
  PARAMETERS lcFabClr
  PRIVATE lnActAlias , lcDyRelOrd , lcFabClr , lcActivDye
  lnActAlias = SELECT(0)
  SELECT DYE_REL1
  lcDyRelOrd = ORDER()
  lcActivDye = fabric+COLOR+dyelot

  SET ORDER TO

  SELECT Dyelot, cDye_Seq;
    FROM DYE_REL1;
    WHERE CINVTYPE+CITEM+DYELOT = '0002'+lcFabClr ;
    ORDER BY cDye_Seq INTO ARRAY laDye_Rel

  SET ORDER TO Dye_rel
  =SEEK(lcActivDye)

  SET ORDER TO &lcDyRelOrd
  SELECT (lnActAlias)
  lnActvLine = RECNO(lcTmpOrdLn)
  *-- end of lfDyeRel.

  *!*************************************************************
  *! Name      : lfAlocMast
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Allocate to master files.
  *!*************************************************************
  *! Called from : lfDyeAlo
  *!*************************************************************
  *! Calls       : lfRelQty , lfAloQty
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfAlocMast()
  *!*************************************************************
FUNCTION lfAlocMast
  PARAMETERS loFormSet
  *-- if This line is allocated before to another dyelot
  *-- release this allocation, because the same group must be allocated
  *-- to the same dyelot.
  IF !EMPTY(DYELOT) AND (STYDYE.DYELOT # DYELOT) AND TOTPIK <> 0
    = lfRelQty(loFormSet)  && release allocation.
  ENDIF

  *-- replace current dyelot field with selected dyelot.
  =RLOCK()
  REPLACE Dyelot  WITH STYDYE.DYELOT,;
    nProcNo WITH 0
  UNLOCK
  =lfAloQty(lcAllWare,.T.,loFormSet)  && Allocate to the desired location.
  *-- end of lfAlocMast.

  *-----------------------Scope OG Functions --------------------
  *!*************************************************************
  *! Name      : lfwFilter
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Selection grid when function
  *!*************************************************************
  *! Called from : Selection OG
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfwFilter()
  *!*************************************************************
FUNCTION lfwFilter
  lnDummyPos = lfItmPos('llDummy')

  =lfvIncHold()
  =lfvExlBulk()

  *-- end of lfwFilter.

  *!*************************************************************
  *! Name      : lfItmPos
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Evaluate fixed filter position within array.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Report code
  *!*************************************************************
  *! Passed Parameters  : ...
  *!*************************************************************
  *! Returns            : Position
  *!*************************************************************
  *! Example   : = lfItmPos()
  *!*************************************************************
  *
FUNCTION lfItmPos
  PARAMETERS lcItmInFlt
  PRIVATE lnItmPos

  lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
  IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
  ENDIF
  RETURN lnItmPos
  *-- end of lfItmPos.

  *!*************************************************************
  *! Name      : lfvIncHold
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Include Hold Orders.
  *!*************************************************************
FUNCTION lfvIncHold
  lcSOrdStat = IIF(llRpIncHor,"OH","O")
  *-- end of lfvIncHold.

  *!*************************************************************
  *! Name      : lfvExlBulk
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Exclude bulk order Orders.
  *!*************************************************************
FUNCTION lfvExlBulk
  lcBulkExp = IIF(llRpExlBlk,""," AND BULK='N'")
  *-- end of lfvExlBulk.

  *!*************************************************************
  *! Name      : lfMakeArrs
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Fill Include/Exclude arrays.
  *!*************************************************************
  *! Called from : Selection OG
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfMakeArrs()
  *!*************************************************************
FUNCTION lfMakeArrs

  DIMENSION loFormSet_laincexprs[1,ALEN(laOGFxFlt,2)],;
    loFormSet_laExcExprs[1,ALEN(laOGFxFlt,2)]
  STORE '' TO loFormSet_laincexprs,loFormSet_laExcExprs

  lnJ = 1
  FOR lnI = 1 TO ALEN(laOGFxFlt,1)
    lnJ = IIF(lnI = lnDummyPos,1,lnJ)
    IF !INLIST(lnI,lnDummyPos,lnDummyPos+1) AND !EMPTY(laOGFxFlt[lnI,6])
      IF (lnI < lnDummyPos) AND !EMPTY(loFormSet_laincexprs[1,1])
        lnJ = lnJ + 1
        DIMENSION loFormSet_laincexprs[lnJ,ALEN(laOGFxFlt,2)]
      ENDIF

      IF (lnI > lnDummyPos) AND !EMPTY(loFormSet_laExcExprs[1,1])
        lnJ = lnJ + 1
        DIMENSION loFormSet_laExcExprs[lnJ,ALEN(laOGFxFlt,2)]
      ENDIF

      lcSubArray = IIF(lnI < lnDummyPos,'loFormSet_laincexprs','loFormSet_laExcExprs')
      FOR lnK = 1 TO ALEN(laOGFxFlt,2)
        &lcSubArray[lnJ,lnK] = laOGFxFlt[lnI,lnK]
      ENDFOR

    ENDIF
  ENDFOR


  *-- end of lfMakeArrs.

  *!*************************************************************
  *! Name      : lfFillVar
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Get the value of the variables returned from O.G.
  *!*************************************************************
FUNCTION lfFillVar
  PARAMETERS lnParam , loFormSet

  DO CASE
      *-- Scope O.G.
    CASE lnParam = 1
      loFormSet_llExclude   = llExclude
      loFormSet_llRpGdExcl  = llRpGdExcl
      loFormSet_llRpExlDye  = llRpExlDye
      loFormSet_llRpExlBlk  = llRpExlBlk
      loFormSet_llRpIncHOr  = llRpIncHOr
      loFormSet_lcRpExSlct  = lcRpExSlct
      loFormSet_lcRpSepCor  = lcRpSepCor
      loFormSet_lcRpScpMod  = lcRpScpMod
      loFormSet_lnDummyPos  = lnDummyPos
      loFormSet_llRpPikSep  = llRpPikSep
      loFormSet_llRpPikCor  = llRpPikCor
      loFormSet_lnColorLen  = lnColorLen
      loFormSet_lnNonMajSt  = lnNonMajSt
      loFormSet_lnRngAlias  = lnRngAlias
      loFormSet_lcSOrdStat  = lcSOrdStat
      *-- Allocate O.G.
    CASE lnParam = 2
      loFormSet_lnRpPikSep  = lnRpPikSep
      loFormSet_lnRpPikCor  = lnRpPikCor
      loFormSet_lnRpCutUnt  = lnRpCutUnt
      loFormSet_lnRpSort1   = lnRpSort1
      loFormSet_lnRpSort2   = lnRpSort2
      loFormSet_lnRpSort3   = lnRpSort3
      loFormSet_lnRpSort4   = lnRpSort4
      loFormSet_lcRpPkFWrh  = lcRpPkFWrh
      loFormSet_lcRpIncWip  = lcRpIncWip
      loFormSet_llRpForAlo  = llRpForAlo
      loFormSet_llRpCond    = llRpCond
      loFormSet_lnRngAlias  = lnRngAlias
      loFormSet_lcSOrdStat  = lcSOrdStat
      DIMENSION loFormSet_laSortAry[4,2]
      =ACOPY(laSortAry,loFormSet_laSortAry)
      *-- Pick O.G.
    CASE lnParam = 3
      loFormSet_lnRpGenNew = lnRpGenNew
      loFormSet_llRpPkHPck = llRpPkHPck
  ENDCASE

  loFormSet_llRpGenPik = llRpGenPik
  loFormSet_lcRpAloNot = lcRpAloNot

  *!*************************************************************
  *! Name      : lfvExclOrd
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Exclude certain orders validation.
  *!*************************************************************
  *! Called from : Selection OG
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfvExclOrd()
  *!*************************************************************
FUNCTION lfvExclOrd

  llRpGdExcl = !llRpGdExcl
  lcRpExSlct = ' '  && Default select by all.

  CLEARREAD()


  *-- end of lfvExclOrd.

  *!*************************************************************
  *! Name      : lfvSlctExc
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Exclude select by validation.
  *!*************************************************************
  *! Called from : Selection OG
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfvSlctExc()
  *!*************************************************************
FUNCTION lfvSlctExc
  lcRpExSlct = laOGFxFlt[lnDummyPos+1,6]

  llClrSty2 = (lcRpExSlct # "S")
  llClrOrd2 = (lcRpExSlct # "O")
  llClrAcc2 = (lcRpExSlct # "A")


  lcPOTlt  = IIF(lcRpExSlct = 'P',"Purchase order number    ","Cutting ticket number    ")
  lcBrwFld = IIF(lcRpScpMod = 'P',"PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
    "APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
    "Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'" ,;
    "PO :R :H='CutTkt#', STYLE :R :H='Style', STATUS :R :H='Status',"+;
    "ENTERED :R :H='Issue', COMPLETE :R :H='Complete', SEASON :R :H='Season',"+;
    "CDIVISION :R :H='Division'")

  *NSTYORDER :R :H='Budget' :P = '999999',"+;
  "Receive :R :H='Received' :P = '999999', Damage :R :H='Damaged' :P = '999999', Open :R :H='Open' :P = '999999'")


  CLEARREAD()
  *-- end of lfvSlctExc.

  *!*************************************************************
  *! Name      : lfvFabric
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Fabric
  *!*************************************************************
  *! Called from : Option grid [Fabric Get field]
  *!*************************************************************
  *! Calls       : FaBrow()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvFabric

  lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
  lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

  *-- IF The user want to Browse or if the Fabric he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
    llObjRet = FaBrow(@lcObjVal , '*')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
    &lcObjName = lcObjVal
  ENDIF    && End of IF

  *!*************************************************************
  *! Name      : lfwOldVal
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : When function to get the Old value
  *!*************************************************************
  *! Called from : Some of the Get fields and some of the Option grid fields
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfwOldVal

  lcOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value

  *!*************************************************************
  *! Name      : lfvOrdWare
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Order Warehouse
  *!*************************************************************
  *! Called from : Option grid [Order Warehouse Get field]
  *!*************************************************************
  *! Calls       : gfBrowWare()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvOrdWare

  lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
  lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

  *-- IF The user want to Browse or if the Warehouse he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
    lcObjVal = gfBrowWare(.T.)
    lcObjVal = IIF(EMPTY(lcObjVal) , lcPkWOldVl , lcObjVal)
    lcPkWOldVl = lcObjVal
    &lcObjName. = lcObjVal
  ENDIF    && End of IF


  *!*************************************************************
  *! Name      : lfvOrder
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Order number
  *!*************************************************************
  *! Called from : Option grid [Order number Get field]
  *!*************************************************************
  *! Calls       : OrdBrowO()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvOrder

  PRIVATE lcObjNam , lcObjVal , llObjRet

  lcObjNam = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
  lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field
  lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))

  *-- IF The user want to Browse or if the Order number he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('O' + lcObjVal , 'ORDHDR'))
    llBrowse = .T.
    llObjRet = OrdBrowO(@lcObjVal , .F. , 'O')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
    llBrowse = .F.
  ENDIF    && End of IF
  &lcObjNam = lcObjVal


  *!*************************************************************
  *! Name      : lfvOGStyle
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Style
  *!*************************************************************
  *! Called from : Option grid [Style Get field]
  *!*************************************************************
  *! Calls       : gfStyBrw()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvOGStyle

  PRIVATE lnCurSelct,lcStyOrder
  lnCurSelct = SELECT(0)
  SELECT STYLE
  lcStyOrder = ORDER()
  SET ORDER TO cStyle

  lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
  lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

  *--IF The user want to Browse or if the Style he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))

    lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.

    lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , lcOldVal)
    &lcObjName = lcObjVal

  ENDIF    && End of IF


  SELECT STYLE
  SET ORDER TO &lcStyOrder
  SELECT (lnCurSelct)
  *-- end of lfvOGStyle.

  *!*************************************************************
  *! Name      : lfvPO
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the PO number
  *!*************************************************************
  *! Called from : Option grid [PO number Get field]
  *!*************************************************************
  *! Calls       : PosBrow()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvPO

  lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
  lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field
  lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))

  *-- IF The user want to Browse or if the PO number he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('PP' + lcObjVal , 'POSHDR1'))
    llObjRet = PosBrow(@lcObjVal , '' , 'P')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  ENDIF    && End of IF
  &lcObjName = lcObjVal

  *!*************************************************************
  *! Name      : lfvCutTkt
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Cut ticket number
  *!*************************************************************
  *! Called from : Option grid [Cut ticket number Get field]
  *!*************************************************************
  *! Calls       : CutBrow()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvCutTkt

  lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
  lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field
  lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))

  *-- IF The user want to Browse or if the Cut ticket number he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('PU' + lcObjVal , 'POSHDR1'))
    llObjRet = CutBrow(@lcObjVal)
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  ENDIF    && End of IF
  &lcObjName = lcObjVal

  *!*************************************************************
  *! Name      : lfvAccount
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Account
  *!*************************************************************
  *! Called from : Option grid [Account Get field]
  *!*************************************************************
  *! Calls       : CusBrowM()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvAccount

  lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
  lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

  *IF The user want to Browse or if the Account he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
    llObjRet = CusBrowM(@lcObjVal , '' , 'M')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
    &lcObjName = lcObjVal
  ENDIF    && End of IF

  *!*************************************************************
  *! Name      : lfvSepCor
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Separates/Coordinate validation
  *!*************************************************************
  *! Called from : Selection OG
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfvSepCor()
  *!*************************************************************
FUNCTION lfvSepCor
  llRpPikSep = (lcRpSepCor $ 'BS')
  llRpPikCor = (lcRpSepCor $ 'BC')
  =lfwFilter()
  *-- end of lfvSepCor.

  *!*************************************************************
  *! Name      : lfvScopMod
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Select by
  *!*************************************************************
  *! Called from : Option grid [Select by Option]
  *!*************************************************************
  *! Calls       : lfChangeGrid()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvScopMod

  llClrSty1 = (lcRpScpMod # "S")
  llClrOrd1 = (lcRpScpMod # "O")
  llClrAcc1 = (lcRpScpMod # "A")

  lcPOTlt  = IIF(lcRpScpMod = 'P',"Purchase order number    ","Cutting ticket number    ")
  lcBrwFld = IIF(lcRpScpMod = 'P',"PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
    "APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
    "Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'" ,;
    "PO :R :H='CutTkt#', STYLE :R :H='Style', STATUS :R :H='Status',"+;
    "ENTERED :R :H='Issue', COMPLETE :R :H='Complete', SEASON :R :H='Season',"+;
    "CDIVISION :R :H='Division'")
  *NSTYORDER :R :H='Budget' :P = '999999',"+;
  "Receive :R :H='Received' :P = '999999', Damage :R :H='Damaged' :P = '999999', Open :R :H='Open' :P = '999999'")
  CLEARREAD()

  *:**************************************************************************
  *: Name      : lfClearRep
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *: Purpose   : Hide and unhide the option of order hold reason.
  *:**************************************************************************
  *: Passed Parameters : None
  *:**************************************************************************
  *: Return      : ....
  *:**************************************************************************
  *: Example     : = lfIncOnHld()
  *:**************************************************************************
FUNCTION lfIncOnHld

  *-- Clear read to Hide and unhide the option of order hold reason.
  CLEARREAD()

  *!*************************************************************
  *! Name      : lfsrAcc1
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Account In Range Filter. 1
  *!*************************************************************
FUNCTION lfsrAcc1
  PARAMETERS lcParm
  =lfsrAcc(lcParm,"1")
  *-- end of lfsrAcc1.

  *!*************************************************************
  *! Name      : lfsrAcc2
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Account In Range Filter. 2
  *!*************************************************************
FUNCTION lfsrAcc2
  PARAMETERS lcParm
  =lfsrAcc(lcParm,"2")
  *-- end of lfsrAcc2.

  *!*************************************************************
  *! Name      : lfsrAcc
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Account In Range
  *!*************************************************************
FUNCTION lfsrAcc
  PARAMETERS lcParm,lcFlagNo
  DO CASE
    CASE lcParm = 'S'
      lnRngAlias = SELECT(0)
      GO TOP IN CUSTOMER
    CASE lcParm = 'R'
      llClrAcc&lcFlagNo = .F.
      SELECT (lnRngAlias)
  ENDCASE
  *-- end of lfsrAcc.

  *!*************************************************************
  *! Name      : lfsrPO1
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : PO In Range Filter. 1
  *!*************************************************************
  *!
FUNCTION lfsrPO1
  PARAMETERS lcParm
  =lfsrPO(lcParm,"1")
  *-- end of lfsrPO1.

  *!*************************************************************
  *! Name      : lfsrPO2
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : PO In Range Filter. 2
  *!*************************************************************
  *!
FUNCTION lfsrPO2
  PARAMETERS lcParm
  =lfsrPO(lcParm,"2")
  *-- end of lfsrPO2.

  *!*************************************************************
  *! Name      : lfsrPO
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : P/O In Range
  *!*************************************************************
FUNCTION lfsrPO
  PARAMETERS lcParm,lcFlagNo

  IF lcRpScpMod = 'K'
    llClrPO&lcFlagNo = .F.
  ELSE
    DO CASE
      CASE lcParm = 'S'
        lnRngAlias = SELECT(0)
        SET ORDER TO VENCODE IN APVENDOR
        SELECT POSHDR
        SET RELATION TO POSHDR.vendor INTO Apvendor ADDITIVE
      CASE lcParm = 'R'
        SELECT POSHDR
        SET RELATION OFF INTO APVENDOR
        llClrPO&lcFlagNo = .F.
        SELECT (lnRngAlias)
    ENDCASE
  ENDIF
  *-- end of lfsrPO.

  *!*************************************************************
  *! Name      : lfSrSty1
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Style In Range Filter. 1
  *!*************************************************************
  *!
FUNCTION lfSrSty1
  PARAMETERS lcParm
  =lfSRStyle(lcParm,"1")
  *-- end of lfSrSty1.

  *!*************************************************************
  *! Name      : lfSrSty2
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Style In Range Filter. 2
  *!*************************************************************
  *!
FUNCTION lfSrSty2
  PARAMETERS lcParm
  =lfSRStyle(lcParm,"2")
  *-- end of lfSrSty2.

  *!*************************************************************
  *! Name      : lfSRStyle
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Style In Range Filter.
  *!*************************************************************
  *!
FUNCTION lfSRStyle
  PARAMETERS lcParm,lcFlagNo
  DO CASE
    CASE lcParm = 'S'  && Set code
      lnRngAlias = SELECT(0)
      *-- open this file in another alias to set order to Style Major
      *-- unique index.
      USE (oAriaEnvironment.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG STYLE IN 0
      SELECT STYLE
      SET ORDER TO TAG Cstyle
      SET RELATION TO STYLE.STYLE INTO STYLE_X
      GO TOP IN STYLE
    CASE lcParm = 'R'  && Reset code
      USE IN STYLE_X
      SELECT STYLE
      SET ORDER TO TAG STYLE
      llClrSty&lcFlagNo = .F.
      SELECT (lnRngAlias)
  ENDCASE
  *-- end of lfSRStyle.

  *!*************************************************************
  *! Name      : lfSrOrd1
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Order In Range Filter. 1
  *!*************************************************************
  *!
FUNCTION lfSrOrd1
  PARAMETERS lcParm
  =lfSROrder(lcParm,"1")
  *-- end of lfSrOrd1.

  *!*************************************************************
  *! Name      : lfSrOrd2
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Order In Range Filter. 2
  *!*************************************************************
  *!
FUNCTION lfSrOrd2
  PARAMETERS lcParm
  =lfSROrder(lcParm,"2")
  *-- end of lfSrOrd2.

  *!*************************************************************
  *! Name      : lfSROrder
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Order In Range
  *!*************************************************************
FUNCTION lfSROrder
  PARAMETERS lcParm,lcFlagNo
  DO CASE
    CASE lcParm = 'S'
      lnRngAlias = SELECT(0)
      SELECT ORDHDR
      lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
      SET ORDER TO Customer IN Customer
      SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
      GO TOP

    CASE lcParm = 'R'
      SELECT ORDHDR
      SET RELATION OFF INTO CUSTOMER && To customer file.
      llClrOrd&lcFlagNo = .F.
      SELECT (lnRngAlias)

  ENDCASE
  *-- end of lfSROrder.


  *!*************************************************************
  *! Name      : lfStySum
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : sum a specific field for the current style in style file
  *!*************************************************************
FUNCTION lfStySum
  PARAMETERS lcSty,lccomp,lnAddToVar
  PRIVATE lnStyRec
  lnTotcomp = 0

  IF RECCOUNT('STYLE') != 0
    lnStyRec = RECNO('STYLE')
    SELECT Style_X
    =SEEK(lcSty)
    SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) = ALLTRIM(lcSty)
    SELECT STYLE
    IF BETWEEN(lnStyRec,1,RECCOUNT())
      GO lnStyRec
    ENDIF
    DO CASE
      CASE lnAddToVar = 1
        lnO_T_S = lnTotcomp
      CASE lnAddToVar = 2
        lnO_T_S = lnO_T_S + lnTotcomp
      CASE lnAddToVar = 3
        lnO_T_S = lnO_T_S - lnTotcomp
    ENDCASE
  ENDIF
  RETURN INT(lnTotcomp)
  *-- end of lfStySum.

  *!*************************************************************
  *! Name      : lfTStr
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Function to change any value to a string
  *!*************************************************************
  *! Called from : lfGetExp()
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : Value of any type
  *!*************************************************************
  *! Return      : The passed value into String
  *!*************************************************************
  *
FUNCTION lfTStr
  PARAMETERS laValTSw

  PRIVATE lnNonDicml , lnDicml , lnDecmlVal

  *DO CASE Statment
  DO CASE

      *Case of Character or Memo
    CASE TYPE('laValTSw') = 'C' .OR. TYPE('laValTSw') = 'M'
      RETURN ALLTRIM(laValTSw)

      *Case of Logical
    CASE TYPE('laValTSw') = 'L'
      RETURN IIF(laValTSw , '.T.' , '.F.')

      *Case of Date
    CASE TYPE('laValTSw') = 'D'
      RETURN DTOC(laValTSw)

      *Case of Numeric
    CASE TYPE('laValTSw') = 'N'
      lnNonDicml = LEN(ALLTRIM(STR(laValTSw , 100 , 0)))    && Variable to hold the length of the Non decimal part
      lnDicml = 0   							              && Variable to hold the length of the decimal part
      lnDecmlVal = ABS(laValTSw - INT(laValTSw))            && Variable to hold the absolute value of the decimal part
      *DO WHILE Loop to get the length of the decimal part
      DO WHILE lnDecmlVal > 0
        lnDicml = lnDicml + 1
        lnDecmlVal = (lnDecmlVal * 10) - INT(lnDecmlVal * 10)
      ENDDO    && End of DO WHILE Loop
      lnDicml = IIF(lnDicml = 0 , lnDicml , lnDicml + 1)
      RETURN ALLTRIM(STR(laValTSw , lnNonDicml + lnDicml + 1 , lnDicml))

      *Otherwise
    OTHERWISE
      RETURN ''
  ENDCASE

  *!*************************************************************
  *! Name      : lfvIncWIP
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function to Include WIP option in Allocate O.G.
  *!*************************************************************
  *! Called from : Alocate O.G>
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : Value of any type
  *!*************************************************************
  *! Return      : The passed value into String
  *!*************************************************************
  *
FUNCTION lfvIncWIP

  CLEARREAD()


  *osa (review)
  *!*************************************************************
  *! Name      : lfwAloc
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Allocation logic grid when function.
  *!*************************************************************
  *! Called from : Allocation logic OG
  *!*************************************************************
  *! Calls       : lfShowItem
  *!*************************************************************
  *! Passed Parameters : ....
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : =lfwAloc()
  *!*************************************************************
FUNCTION lfwAloc

  IF lnOGSeting = 1
    laSortAry[1,1] = 1
    laSortAry[1,2] = [DTOS(COMPLETE)]
    laSortAry[2,1] = 2
    laSortAry[2,2] = [PRIORITY]
    laSortAry[3,1] = 3
    laSortAry[3,2] = [DTOS(START)]
    laSortAry[4,1] = 4
    laSortAry[4,2] = [ORDER]
    lnRpSort1 = 1
    lnRpSort2 = 2
    lnRpSort3 = 3
    lnRpSort4 = 4
    FOR lnCounter = 1 TO 4
      lcCounter = STR(lnCounter,1)
      =lfOGShowGet('lnRpSort'+lcCounter)
    ENDFOR
  ENDIF
  *B607815,1 WAM 10/31/2006 Initialize OG variable
  llRpForAlo  = loFormSet_llRpForAlo
  *B607815,1 WAM 10/31/2006 (End)
  IF llRpForAlo
    *-- Pick separates %
    IF llRpPikSep
      lnRpPikSep = 100
      =lfShowItem('lnRpPikSep')
    ENDIF

    *-- Pick coordinate %
    IF llRpPikCor
      lnRpPikCor = 100
      =lfShowItem('lnRpPikCor')
    ENDIF

    *-- cut-off units
    lnRpCutUnt = 0
    =lfShowItem('lnRpCutUnt')

    *-- allocate conditionally.

    IF (llUseDyes AND llFabDye)
      llRpCond = (lcRpSepCor='S')
      =lfShowItem('llRpCond')
    ENDIF

  ENDIF
  *-- When function for allocation grid.
  *-- end of lfwAloc.

  *!*************************************************************
  *! Name      : lfShowItem
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Enable/Disable Allocation grid objects.
  *!*************************************************************
  *! Called from : lfvOGFrcAl
  *!*************************************************************
  *! Calls       : lfOGShowGet()
  *!*************************************************************
  *! Passed Parameters : Object,Object State(.T. or .F.)
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
FUNCTION lfShowItem
  PARAMETERS lcItem,llState
  PRIVATE lnItemPos
  lnItemPos = ASCAN(laOGObjType,UPPER(lcItem))
  IF lnItemPos > 0
    lnItemPos = ASUBSCRIPT(laOGObjType,lnItemPos,1)
    laOGObjCnt[lnItemPos] = llState
  ENDIF
  =lfOGShowGet(UPPER(lcItem))
  *-- end of lfShowItem.

  *!*************************************************************
  *! Name      : lfvPikSep
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Pick Separates Min %
  *!*************************************************************
  *! Called from : Option grid [Pick Separates Min % Get field]
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvPikSep

  *IF The Pick Separates Min % value is less than 0 or greater than 100
  IF lnRpPikSep < 0 .OR. lnRpPikSep > 100
    *** Message : "ð range from ð to ð           "
    ***           "            < Ok >            "
    =gfModalGen("TRM00272B00000" , "DIALOG" , LANG_AUTOALLOC_MsgPickSep)
    lnRpPikSep = lcOldVal
  ENDIF    && End of IF

  *!*************************************************************
  *! Name      : lfvPikCor
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Pick coordinate Min %
  *!*************************************************************
  *! Called from : Option grid [Pick coordinate Min % Get field]
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvPikCor

  *IF The Pick coordinate Min % value is less than 0 or greater than 100
  IF lnRpPikCor < 0 .OR. lnRpPikCor > 100

    *** Message : "ð range from ð to ð           "
    ***           "            < Ok >            "
    =gfModalGen("TRM00272B00000" , "DIALOG" , LANG_AUTOALLOC_MsgPickCoord)
    lnRpPikCor = lcOldVal
  ENDIF    && End of IF

  *!*************************************************************
  *! Name      : lfvCutUnt
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Cut of units
  *!*************************************************************
  *! Called from : Option grid [Cut of units Get field]
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvCutUnt

  *IF The Cut of units value is less than 0
  IF lnRpCutUnt < 0

    *** Message : "ð should be greater than zero."
    ***           "            < Ok >            "
    =gfModalGen("TRM00234B00000" , "DIALOG" , LANG_AUTOALLOC_MsgCutOffUnt)
    lnRpCutUnt = lcOldVal
  ENDIF    && End of IF

  *!*************************************************************
  *! Name      : lfvPkFWrh
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Pick from warehouse
  *!*************************************************************
  *! Called from : Option grid [Pick from warehouse Get field]
  *!*************************************************************
  *! Calls       : lfvOrdWare()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvPkFWrh

  =lfvOrdWare()
  *--

  *!*************************************************************
  *! Name      : lfvOGFrcAl
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid function of the Force allocation
  *!*************************************************************
  *! Called from : Option grid [Force allocation Option]
  *!*************************************************************
  *! Calls       : lfShowItem()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *
FUNCTION lfvOGFrcAl

  *-- if user want to force allocation

  IF llRpForAlo
    *-- Pick separates %
    IF llRpPikSep
      lnRpPikSep = 100
      =lfShowItem('lnRpPikSep')
    ENDIF

    *-- Pick coordinate %
    IF llRpPikCor
      lnRpPikCor = 100
      =lfShowItem('lnRpPikCor')
    ENDIF

    *-- cut-off units
    lnRpCutUnt = 0
    =lfShowItem('lnRpCutUnt')

    *-- allocate conditionally.
    IF (llUseDyes AND llFabDye)
      llRpCond = (lcRpSepCor='S')
      =lfShowItem('llRpCond')
    ENDIF

  ELSE  && else user want to unforce allocation.

    *-- Pick separates %
    IF llRpPikSep
      =lfShowItem('lnRpPikSep',.T.)
    ENDIF

    *-- Pick coordinate %
    IF llRpPikCor
      =lfShowItem('lnRpPikCor',.T.)
    ENDIF

    *-- cut-off units
    =lfShowItem('lnRpCutUnt',.T.)

    *-- allocate conditionally.
    IF (llUseDyes AND llFabDye)
      =lfShowItem('llRpCond',.T.)
    ENDIF

  ENDIF
  *-- end of lfvOGFrcAl.

  *!*************************************************************
  *! Name      : lfvSortBy
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : All Sort by validations
  *!*************************************************************
  *! Passed Parameters : Sort Number (1,2,3, Or 4)
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
FUNCTION lfvSortBy
  PARAMETERS lnSortItem

  PRIVATE lcObjName , lnObjVal , lcObjGet , llOldValue , lnI , lcItmObj
  llOldValue = .F.
  lcObjGet  = OGSYS18()
  lcObjName = "lnRpSort" + STR(lnSortItem,1)
  lnObjVal  = EVALUATE(lcObjName)

  lnI = 0
  IF lnObjVal = 6
    FOR lnI = lnSortItem + 1 TO 4
      lcItmObj = "lnRpSort" + STR(lnI,1)
      IF EVALUATE(lcItmObj) <> 6
        llOldValue = .T.
        EXIT
      ENDIF
    ENDFOR
  ELSE
    IF lnSortItem > 2
      FOR lnI = lnSortItem-1 TO 2 STEP -1
        lcItmObj = "lnRpSort" + STR(lnI,1)
        IF EVALUATE(lcItmObj) = 6
          llOldValue = .T.
          EXIT
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
  llOldValue = IIF(llOldValue,llOldValue,;
    (lnObjVal<> 6) AND (ASCAN(laSortAry,lnObjVal) > 0))

  IF llOldValue
    *-- Restore old values.
    STORE lcOldVal TO &lcObjName , &lcObjGet
    SHOW GET &lcObjGet
  ELSE
    *-- Sort By Arrays make Sort Index.
    laSortAry[lnSortItem,1] = lnObjVal
    laSortAry[lnSortItem,2] = loFormSet_laindexexp[lnObjVal]
  ENDIF
  *-- end of lfvSortBy.


  *!*************************************************************
  *! Name      : lfPickTkt
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : All Sort by validations
  *!*************************************************************
  *! Passed Parameters : Sort Number (1,2,3, Or 4)
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
FUNCTION lfPickTkt

  IF lcRpIncWip = 'S'
    lnShpNoElm = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SHPMTHDR.SHIPNO',1),1)
    lnShpDtElm = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SHPMTHDR.ETA',1),1)
    llFirst = .T.
    lcShpNO  = ""
    IF !EMPTY(laOGFxFlt[lnShpNoElm,6])
      SELECT (laOGFxFlt[lnShpNoElm,6])
      SCAN
        lcShpNO = lcShpNO + IIF(llFirst,"",",")+ "'" + ShipNo + "'"
        llFirst = .F.
      ENDSCAN
    ENDIF

    lcShpDat = "ETA BETWEEN '" + SUBSTR(laOGFxFlt[lnShpDtElm ,6],1,ATC('|',laOGFxFlt[lnShpDtElm ,6])-1)+;
      "' AND '"+SUBSTR(laOGFxFlt[lnShpDtElm ,6],ATC('|',laOGFxFlt[lnShpDtElm ,6])+1,10)+"'"

    DO CASE
      CASE !EMPTY(lcShpNO) AND !EMPTY(laOGFxFlt[lnShpDtElm ,6])
        loFormSet_lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P' AND SHIPNO IN (" +lcShpNO + ") AND " + lcShpDat
      CASE !EMPTY(lcShpNO) AND EMPTY(laOGFxFlt[lnShpDtElm ,6])
        loFormSet_lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P' AND SHIPNO IN (" +lcShpNO + ")"
      CASE EMPTY(lcShpNO) AND !EMPTY(laOGFxFlt[lnShpDtElm ,6])
        loFormSet_lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P' AND " + lcShpDat
      CASE EMPTY(lcShpNO) AND EMPTY(laOGFxFlt[lnShpDtElm ,6])
        loFormSet_lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P'"
    ENDCASE
  ENDIF

  *!*************************************************************
  *! Name      : lfEvalShp
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Evaluate Open shipment to add it to availablity if user want.
  *!*************************************************************
  *! Passed Parameters : Sort Number (1,2,3, Or 4)
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
FUNCTION lfEvalShp
  PARAMETERS lcShpSty,lcShpWare,loFormSet

  lcCurAlias = SELECT(0)

  loFormSet_laWIP = 0

  lcKeyExpr  = 'shipno+cstytype+cBusDocu+po+style+STR(lineno,6)+trancd'

  *-- Collect Open Qty of the open shipments
  FOR lnStrPos = 1 TO ALEN(loFormSet_laShpExp,1)
    lcKey = loFormSet_laShpExp[lnStrPos]+ 'PP'
    lcPOSLCond = "shipno = '" + loFormSet_laShpExp[lnStrPos] + "' AND CSTYTYPE = 'P' AND cBusDocu = 'P'"
    llPOSLNOpn = .F.
    IF lfOpenSql(loFormSet,'POSLN','POSLN1',lcPOSLCond)
      SELECT POSLN1
      LOCATE
      llPOSLNOpn = !EOF()
    ENDIF
    IF llPOSLNOpn
      SCAN FOR (STYLE = lcShpSty) AND (cWareCode = lcShpWare)
        *-- If select By C/T or P/O , collect only shipments for selected C/T or P/O
        IF loFormSet_lcRpScpMod $ 'KP' AND !EMPTY(loFormSet_laincexprs[1,6])
          IF ASCAN(loFormSet_laString1,PO,1) = 0
            LOOP
          ENDIF
        ENDIF
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          loFormSet_laWIP[lnI] = loFormSet_laWIP[lnI] + IIF(Trancd='3',Qty&lcI,0)
        ENDFOR
      ENDSCAN
    ENDIF
  ENDFOR

  *-- If select By C/T or P/O and there is an excluded C/T or P/O
  IF loFormSet_llExclude AND (loFormSet_lcRpExSlct $ 'KP') AND !EMPTY(loFormSet_lafiltexp[1,6])
    FOR lnStrPos = 1 TO ALEN(loFormSet_laShpExp,1)
      lcKey = loFormSet_laShpExp[lnStrPos]+ 'PP'
      IF SEEK(lcKey)
        SCAN REST WHILE &lcKeyExpr = lcKey;
            FOR (STYLE = lcShpSty) AND (cWareCode = lcShpWare)
          IF ASCAN(loFormSet_laString2,PO,1) <> 0
            FOR lnI = 1 TO 8
              lcI = STR(lnI,1)
              loFormSet_laWIP[lnI] = loFormSet_laWIP[lnI] - IIF(Trancd='3',Qty&lcI,0)
            ENDFOR
          ENDIF
        ENDSCAN
      ENDIF
    ENDFOR
  ENDIF
  loFormSet_laWIP[9] = loFormSet_laWIP[1]+loFormSet_laWIP[2]+loFormSet_laWIP[3]+loFormSet_laWIP[4]+;
    loFormSet_laWIP[5]+loFormSet_laWIP[6]+loFormSet_laWIP[7]+loFormSet_laWIP[8]

  SELECT (lcCurAlias )

  *!*************************************************************
  *! Name      : lfSumFab
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : sum a specific field for the current item in Fabric file.
  *!*************************************************************
  *! Passed Parameters : Sort Number (1,2,3, Or 4)
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************

  *! B999999,1 WSH 05/09/2005, Change the function name as it conflicts with a function in Aria.Prg and
  *!                           get quantity fields from ItemLoc file ... [Start]
  *!*  FUNCTION lfSumFab
  *!*  PARAMETERS lcFab,lccomp
  *!*  lnTotcomp = 0
  *!*  SELECT ITEM
  *!*  lcLastOrd = ORDER()
  *!*  SET ORDER TO CSTYLE
  *!*  SUM &lcCOMP TO lnTotcomp WHILE CINVTYPE+STYLE = '0002'+lcFab
  *!*  SET ORDER TO (lcLastOrd)
  *!*  GO RECNO()
  *!*  RETURN INT(lnTotcomp)
FUNCTION lfFabSum
  LPARAMETERS lcFab, lccomp

  LOCAL lnTotcomp, lnAlias
  lnAlias   = SELECT(0)
  lnTotcomp = 0

  SELECT ITEM
  lcLastOrd = ORDER()
  SET ORDER TO CSTYLE
  SUM &lcCOMP TO lnTotcomp WHILE CINVTYPE+STYLE = '0002'+lcFab
  SET ORDER TO (lcLastOrd)

  SELECT (lnAlias)
  RETURN INT(lnTotcomp)

  *!*  LOCAL lnAlias, lnTotcomp, lcStatment, lnConnHandl
  *!*  lnAlias   = SELECT(0)
  *!*  lnTotcomp = 0

  *!*  SELECT (lcTmpFab)
  *!*  IF SEEK(RTRIM(lcFab))
  *!*    SUM &lcCOMP. TO lnTotcomp REST WHILE Fabric = lcFab
  *!*  ENDIF

  *!*  SELECT (lnAlias)
  *!*  RETURN INT(lnTotcomp)
  *! B999999,1 WSH 05/09/2005, [End]

  *!*************************************************************
  *! Name      : lfSrFab
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : PO In Range Filter. 1
  *!*************************************************************
  *!
FUNCTION lfSrFab
  PARAMETERS lcParm

  SELECT ITEM
  SET ORDER TO CSTYLE
  LOCATE

  *osa
  *!*	*!*************************************************************
  *!*	*! Name      : lfHandlObj
  *!*	*: Developer : HEND GHANEM (HBG)
  *!*	*: Date      : 11/12/2003
  *!*	*! Purpose   : Handle status of tool bar and option menu
  *!*	*!*************************************************************
  *!*	*!
  *!*	FUNCTION lfHandlObj
  *!*	PARAMETERS loFormSet

  *!*	STORE 0 TO loFormSet_lnSelRec,loFormSet_lnSelAlo,loFormSet_lnAloRec

  *!*	SELECT (loFormSet_lcTmpOrdLn)
  *!*	lnRecNo = RECNO()
  *!*	GO TOP
  *!*	REPLACE cPosition WITH 'T'
  *!*	SCAN
  *!*	  IF lnSel = 1
  *!*	    loFormSet_lnSelRec = loFormSet_lnSelRec + 1
  *!*	    IF TOTPIK <> 0
  *!*	      loFormSet_lnSelAlo = loFormSet_lnSelAlo + 1
  *!*	    ENDIF
  *!*	  ENDIF
  *!*	  IF TOTPIK <> 0
  *!*	    loFormSet_lnAloRec = loFormSet_lnAloRec + 1
  *!*	  ENDIF
  *!*	ENDSCAN
  *!*	GO BOTTOM
  *!*	REPLACE cPosition WITH 'E'

  *!*	IF BETWEEN(lnRecNo,1,RECCOUNT())
  *!*	  GOTO lnRecNo
  *!*	ENDIF

  *!*	loFormSet_llSelAllSt = IIF(loFormSet_lnSelRec = RECCOUNT(), .F. , .T.)
  *!*	loFormSet_llSelNonSt = IIF(loFormSet_lnSelRec = 0 , .F. , .T.)
  *!*	loFormSet_AriaForm1.grdOrders.cmdSelect.Enabled     = !EOF(loFormSet_lcTmpOrdLn)
  *!*	loFormSet_AriaForm1.grdOrders.cmdSelect.Refresh
  *!*	loFormSet_AriaForm1.grdOrders.cmdSelectAll.Enabled  = loFormSet_llSelAllSt
  *!*	loFormSet_AriaForm1.grdOrders.cmdSelectAll.Refresh
  *!*	loFormSet_AriaForm1.grdOrders.cmdSelectNone.Enabled = loFormSet_llSelNonSt
  *!*	loFormSet_AriaForm1.grdOrders.cmdSelectNone.Refresh
  *!*	loFormSet_AriaForm1.grdOrders.cmdInvert.Enabled     = !EOF(loFormSet_lcTmpOrdLn)
  *!*	loFormSet_AriaForm1.grdOrders.cmdInvert.Refresh

  *!*	*HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	*oAriaEnvironment.oToolBar.ChangeButtonStatus('pbScop','ENABLED')
  *!*	loFormSet_oToolBar.ChangeButtonStatus('pbScop','ENABLED')
  *!*	*HBG [End]
  *!*	IF loFormSet_lnSelRec = 0
  *!*	  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	  *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbAlo','DISABLE')
  *!*	  *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbRel','DISABLE')
  *!*	  *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbGen','DISABLE')
  *!*	  loFormSet_oToolBar.ChangeButtonStatus('pbAlo','DISABLE')
  *!*	  loFormSet_oToolBar.ChangeButtonStatus('pbRel','DISABLE')
  *!*	  loFormSet_oToolBar.ChangeButtonStatus('pbGen','DISABLE')
  *!*	  *HBG  [End]
  *!*	ELSE
  *!*	  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	  *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbAlo','ENABLED')
  *!*	  loFormSet_oToolBar.ChangeButtonStatus('pbAlo','ENABLED')
  *!*	  *HBG [End]
  *!*	  lcRelSt = IIF(loFormSet_lnSelAlo = 0 , 'DISABLE', 'ENABLED')
  *!*	  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	  *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbRel',lcRelSt)
  *!*	  *oAriaEnvironment.oToolBar.ChangeButtonStatus('pbGen',lcRelSt)
  *!*	  loFormSet_oToolBar.ChangeButtonStatus('pbRel',lcRelSt)
  *!*	  loFormSet_oToolBar.ChangeButtonStatus('pbGen',lcRelSt)
  *!*	  *HBG [End]
  *!*	ENDIF

  *!*	*B131801,1 MMT 26/04/2006 remove progress bar after finishing [Start]
  *!*	loFormSet_Ariaform1.grdOrders.grdMultiSelectionGrid.refresh()
  *!*	loFormSet_Ariaform1.grdOrders.grdMultiSelectionGrid.afterrowcolchange()
  *!*	loFormSet_oPross.hide()
  *!*	*B131801,1 MMT 26/04/2006 remove progress bar after finishing [End]
  *osa

  *!*************************************************************
  *! Name      : lfvGenPktk
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid fuction for generate new Piktkt No
  *!*************************************************************
  *!
FUNCTION lfvGenPktk

  CLEARREAD()

  *!*************************************************************
  *! Name      : lfSrFab
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : PO In Range Filter. 1
  *!*************************************************************
  *!
FUNCTION lfNavigate
  PARAMETERS loFormSet,lcGoTo

  DO CASE
    CASE lcGoTo = 'T'  && Top
      SELECT (loFormSet_lcTmpOrdLn)
      GO TOP
    CASE lcGoTo = 'N'  && Next
      SELECT (loFormSet_lcTmpOrdLn)
      IF !EOF()
        SKIP 1
      ENDIF
    CASE lcGoTo = 'P'  && Previous
      SELECT (loFormSet_lcTmpOrdLn)
      IF !BOF()
        SKIP -1
      ENDIF
    CASE lcGoTo = 'E'  && End
      SELECT (loFormSet_lcTmpOrdLn)
      GO BOTTOM
  ENDCASE



  *!*************************************************************
  *! Name     : gfItemMask
  *! Auth     : MAN - Mohamed Abdel Salam
  *! Date     : 07/12/97
  *! Task ID  : E300705,1
  *!*************************************************************
  *! Synopsis : function to return the item code mask or the item
  *!            code header
  *!*************************************************************
FUNCTION gfItemMask
  *N037782,1 AMH Add new parameter to get the material code structure [Start]
  *PARAMETERS lcMaskOrHead, lcDataDr
  PARAMETERS lcMaskOrHead, lcDataDr, lcInvType

  *N037782,1 AMH [End]

  PRIVATE lcReturn,llStructUse,lnRecNo,lcStructOrd,lnCurAlias,llArray,;
    laSeg,lcItemDim,lcHeader,lnstartnonm,lnnoseg,lnposistion

  IF TYPE('lcDataDr') # 'C' .OR. EMPTY(lcdatadr)
    lcdatadr = oariaenvironment.datadir
  ENDIF

  *N037782,1 AMH Suport Old calling [Start]
  *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
  *IF TYPE('lcInvType') # 'C'
  IF TYPE('lcInvType') # 'C' OR (TYPE('lcInvType') = 'C' AND EMPTY(lcinvtype))
    *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]
    lcinvtype = '0001'
  ENDIF
  *N037782,1 AMH [End]

  STORE '' TO lcreturn
  llarray = TYPE('lcMaskOrHead[1]') # 'U'
  lcitemdim = 'I'
  IF !llarray
    IF TYPE('lcMaskOrHead')<>'C'

      *N037782,1 AMH Fix bug of Data type missmatch [Start]
      *RETURN .F.
      RETURN lcreturn
      *N037782,1 AMH [End]

    ENDIF
    lcmaskorhead = UPPER(lcmaskorhead)
    lcitemdim = IIF(LEN(lcmaskorhead)>1, RIGHT(lcmaskorhead,1),'I')
    lcmaskorhead = LEFT(lcmaskorhead,1)
  ENDIF

  lcloopext = lcitemdim
  lncuralias = SELECT()

  *N037782,1 AMH Get the inventory type [Start]
  LOCAL lcrmtinvtype,lnConnectionHandlar,lcSqlStatment,lnDataSession,lcItemStru
  lcRmtinvType  = oAriaEnvironment.CURSORS.getCursorTempName()
  lcSqlStatment = "SELECT * FROM INVTYPE (INDEX=CINVTYPE) WHERE CINVTYPE='"+lcinvtype+"'"
  lnDataSession = SET("Datasession")


  lcconnstr = "Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB="+oariaenvironment.datadir+;
    ";SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=Yes;"


  lnconnectionhandlar = oariaenvironment.remotecompanydata.sqlrun(lcsqlstatment,lcrmtinvtype,'INVTYPE',;
    lcconnstr ,3,'SAVE',lndatasession)

  IF lnconnectionhandlar # 1
    *RETURN lcReturn
    lcitemstru = 'U'
  ELSE
    SELECT (lcrmtinvtype)
    lcitemstru = citemstru
  ENDIF
  *N037782,1 AMH [End]

  llstructuse = .F.
  IF !USED('ICISTRU')
    USE (lcdatadr+'ICISTRU') IN 0
    llstructuse = .T.
  ELSE
    SELECT icistru
    lcstructord = ORDER()
    lnrecno = RECNO()
  ENDIF
  SELECT icistru
  SET ORDER TO TAG segno

  *N037782,1 AMH Get the code structure of inventory type [Start]
  *=SEEK('U1')
  =SEEK(lcitemstru+'1')
  *N037782,1 AMH [End]

  *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[Start]
  DIMENSION laseg[1,7]
  laseg = ''
  *B607915,1 MMT 12/28/2006 bug of error in Po screen while save[End]

  lcheader = ciseghead
  lnnoseg = 0
  lnposistion = 1
  *N037782,1 AMH Get the code structure of inventory type [Start]
  *SCAN REST WHILE citemrecty+cisegno = 'U'
  SCAN REST WHILE citemrecty+cisegno = lcitemstru
    *N037782,1 AMH [End]

    IF lcloopext <> 'N'
      lnnoseg = lnnoseg + 1
      DIMEN laseg[lnNoSeg,7]
      laseg[lnNoSeg,1] = cisegtype
      laseg[lnNoSeg,2] = ALLT(cisegsdes)
      laseg[lnNoSeg,3] = REPL('X',nisegsize)
      laseg[lnNoSeg,4] = lnposistion
      laseg[lnNoSeg,5] = ALLT(cisegldes)
      laseg[lnNoSeg,6] = cisegsepr
      laseg[lnNoSeg,7] = lsegendmaj
      lcreturn = lcreturn+REPL('X',nisegsize)+ALLT(cisegsepr)
    ENDIF
    lnposistion = lnposistion + nisegsize + LEN(ALLT(cisegsepr))
    IF lcloopext = 'N' AND lsegendmaj
      lcloopext = 'I'
    ENDIF
    IF lcitemdim = 'M' AND lsegendmaj
      EXIT
    ENDIF
  ENDSCAN
  IF llarray
    DIMEN lcmaskorhead[ALEN(laSeg,1),ALEN(laSeg,2)]
    lcreturn=ACOPY(laseg,lcmaskorhead)
  ELSE
    DO CASE
      CASE  lcmaskorhead = 'S'
        lcreturn = lnnoseg
      CASE  lcmaskorhead = 'P' AND  lcitemdim='M'
        IF gfitemmask('SN') > 0
          lcreturn = SUBSTR(lcreturn,1,LEN(lcreturn)-1)
        ENDIF
      CASE lcmaskorhead = 'H' AND lcitemdim='M'
        IF gfitemmask('SN') > 0
          lcreturn = SUBSTR(lcheader,1,laseg[lnNoSeg,4]+LEN(laseg[lnNoSeg,2])-1)
        ELSE
          lcreturn = lcheader
        ENDIF
      CASE lcmaskorhead = 'H' AND lcitemdim='N'  AND lnnoseg>0
        lcreturn = SUBSTR(lcheader,laseg[1,4])
      CASE lcmaskorhead = 'H' AND lcitemdim='I'
        lcreturn = lcheader
    ENDCASE
  ENDIF
  IF llstructuse
    USE IN icistru
  ELSE
    SELECT icistru
    SET ORDER TO TAG (lcstructord)
    IF lnrecno>0 AND lnrecno<=RECCOUNT()
      GO lnrecno
    ENDIF
  ENDIF

  *N037782,1 AMH Close inventory type [Start]
  IF USED(lcrmtinvtype)
    USE IN (lcrmtinvtype)
  ENDIF
  *N037782,1 AMH [End]

  SELECT (lncuralias)
  RETURN lcreturn
