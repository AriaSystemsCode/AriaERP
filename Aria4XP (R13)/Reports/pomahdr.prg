*:**********************************************************************
*: Program file       : POSTYHRE.PRG
*: Program description: P/O Header report.
*: Module             : STYLE PURCHASE ORDER (PO)
*: Developer          :  NFZ neveen	farouk
*: Tracking Job Number:  037718
*: Date                :06/24/2004
*:**********************************************************************
*: Calls:
*:         Programs        : lfCollectData
*:                         : lfCrtTable
*:                         : lfCalcCost
*:         Screens         :
*:         Global Function :
*:**********************************************************************
*: Called From:
*:**********************************************************************
*: Passed Parameters: None
*:**********************************************************************
*: Example: DO POSTYHRE
*: B608436,1 SSH Remove season filter and activate purchase group filter
*: B608777,1 HES Make a relation between POSHDR and APVENDOR in the Set function and remove it in the Reset function
*: E303645,1 AEG 02/06/2016 Add available date to PO order Header report[T20150116.0001]
*: B611444,1 AHH 05/11/2017 The PO header report, PO# browser displays incorrect Vendor name [T20171013.0015]
*:*********************************************************
*N000682,1 MMT 02/06/2013 Globalization changes[Start]
#include r:\aria4xp\reports\pomahdr.h
*N000682,1 MMT 02/06/2013 Globalization changes[End]
Local loRDA ,lnResult
Set Step On
*HMA [Begin]
*-- Define variables hold the label of Cost
Store ' ' To lcISlbl1 ,lcISlbl2 ,lcISlbl3 ,lcISlbl4  ,;
  lcISlbl5 ,lcISlbl6 ,lcISlbl7
If LCRPLNGSHR='L'  && Long Layout Report
  lcISlbl1  = laCost[1,2]
  lcISlbl2  = laCost[2,2]
  lcISlbl3  = laCost[3,2]
  lcISlbl4  = laCost[4,2]
  lcISlbl5  = laCost[5,2]
  lcISlbl6  = laCost[6,2]
  lcISlbl7  = laCost[7,2]
Endif
*HMA [End]
If loOgScroll.llOGFltCh

  Do lfCollectData  && Collect Data From Poshdr (sql Table ) & from Apvendor
  Select lcPosHdr
  If Reccount()=0
    =gfModalGen('TRM00052B40011','ALERT')
    Return .F.
  Endif
  *--- Create Temp Table
  Do lfCrtTable
  *-------- && Replace Date From Cursor and calc. Cost
  Do lfCalcCost
  *--B127304,MMT,04/11/2005,add print decimals option [Start]
Else
  Select lcPosHdr
  If Reccount()=0
    =gfModalGen('TRM00052B40011','ALERT')
    Return .F.
  Endif
  If !Used(lcWorkFile)
    Use oAriaApplication.WorkDir +  lcWorkFile + ".DBF" In 0
  Endif
Endif

Declare loOgScroll.lacrparams[14,2]   && array For Param
*--DECLARE loOgScroll.lacrparams[13,2]   && array For Param
*--B127304,MMT,04/11/2005,add print decimals option [End]
loOgScroll.lacrparams[1,1] ='RP_TITL'
loOgScroll.lacrparams[1,2] = M.LCRPTITLE
loOgScroll.lacrparams[2,1] ='RP_NOTPAD'
loOgScroll.lacrparams[2,2] = M.LCRPNOTE
loOgScroll.lacrparams[3,1] ='RP_SORT'
loOgScroll.lacrparams[3,2] = M.lcRPSortBy
loOgScroll.lacrparams[4,1] ='RP_Cost1'
loOgScroll.lacrparams[4,2] = lcISlbl1
loOgScroll.lacrparams[5,1] ='RP_Cost2'
loOgScroll.lacrparams[5,2] = lcISlbl2
loOgScroll.lacrparams[6,1] ='RP_Cost3'
loOgScroll.lacrparams[6,2] = lcISlbl3
loOgScroll.lacrparams[7,1] ='RP_Cost4'
loOgScroll.lacrparams[7,2] = lcISlbl4
loOgScroll.lacrparams[8,1] ='RP_Cost5'
loOgScroll.lacrparams[8,2] = lcISlbl5
loOgScroll.lacrparams[9,1] ='RP_Cost6'
loOgScroll.lacrparams[9,2] = lcISlbl6
loOgScroll.lacrparams[10,1]='RP_Cost7'
loOgScroll.lacrparams[10,2]= lcISlbl7
loOgScroll.lacrparams[11,1]='RP_Name'
*-- Amin
*N000682,1 MMT 02/06/2013 Globalization changes[Start]
*loOgScroll.lacrparams[11,2]= IIF(lccInvType = '0001', 'Style Purchase Order Header Report', 'Material Purchase Order Header Report')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.lacrparams[11,2]= IIF(lccInvType = '0001',LANG_REPORTPOTTL,LANG_REPORTMATTL)
loOgScroll.lacrparams[11,2]= Iif(lccInvType = '0001',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REPORTPOTTL,oAriaApplication.GetHeaderText("LANG_REPORTPOTTL",AHEADERFILE)),Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REPORTMATTL,oAriaApplication.GetHeaderText("LANG_REPORTMATTL",AHEADERFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/06/2013 Globalization changes[END]
*-- Amin
loOgScroll.lacrparams[12,1]='AllowCost'
loOgScroll.lacrparams[12,2]= Iif(gfUserPriv('IC', 'ICSTYLE', 'COSTING'),1,0)
**---- Do Case For Param. Sort By (Description)
Do Case
  Case M.lcRPSortBy='P'
    loOgScroll.lacrparams[3,2]='P'
  Case M.lcRPSortBy='V'
    loOgScroll.lacrparams[3,2]='V'
  Case M.lcRPSortBy='D'
    loOgScroll.lacrparams[3,2]='D'
Endcase

loOgScroll.lacrparams[13,1]='MA'
loOgScroll.lacrparams[13,2]= Iif(oAriaApplication.ActiveModuleID='MA',1,0)
*--B127304,MMT,04/11/2005,add print decimals option [Start]
loOgScroll.lacrparams[14,1]='DisplayDecimals'
loOgScroll.lacrparams[14,2]= Iif(llRpDec,1,0)
*--B127304,MMT,04/11/2005,add print decimals option [End]
Select (lcWorkFile)
Dimension loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcWorkFile+'.DBF'


Use In (lcWorkFile)
*HMA [Begin]
*RELEASE loRDA ,lnResult,lcISlbl1 ,lcISlbl2 ,lcISlbl3  ,;
lcISlbl4 ,lcISlbl5 ,lcISlbl6 ,lcISlbl7 ,;
lcCostField,lcLanCostField,lcActCostField,;
lcFCostField,lcFLanCostField,lcFActCostField
Release loRDA ,lnResult,lcCostField,lcLanCostField,lcActCostField,;
  lcFCostField,lcFLanCostField,lcFActCostField
*HMA [End]
=gfDispRe()

************************************************************
*! Name      : lfSetRes
*! Developer : Hesham Elmasry (HES)
*! Date      : 12/24/2008
*! Purpose   : Make set and reset function
************************************************************
*! Passed Parameters : 'S' for Set, 'R' for Reset, 'V' for Valid
************************************************************
*! Return      : None
************************************************************
*! B608777 *****
Function lfSetRes
  *-----------------
  Lparameters LCPARAM
  Do Case
    Case LCPARAM='S'
      Select APVENDOR
      Set Order To VENCODE

      Select POSHDR
      lcSavRel = Set('Relation')
      Set Relation To VENDOR Into APVENDOR Additive
    Case LCPARAM='R'
      Select POSHDR
      *!*	    B611444,1 AHH 05/11/2017 The PO header report, PO# browser displays incorrect Vendor name [T20171013.0015] [Start]
      *!*		  SET RELATION TO &lcSavRel
      SET RELATION TO
      *!*		  B611444,1 AHH 05/11/2017 The PO header report, PO# browser displays incorrect Vendor name [T20171013.0015] [End]
  Endcase
  Return
  *! B608777 *****
  *!*************************************************************
  *! Name      : lfCalcCost
  *! Developer : Neveen Farouk Zaher (NFZ)
  *! Date      : 07/13/2004
  *! Purpose   : Replace Data From Cursor to temp. table & Calc Cost
  *!*************************************************************
  *! Passed Parameters  : ...
  *!*************************************************************
  *! Returns            :
  *!*************************************************************
  *---------------
Procedure lfCalcCost
  *--------------

  Dimension laNormCost[8],laLandAmnt[8] , laActuAmnt[8]
  Store 0 To laNormCost,laLandAmnt,laActuAmnt

  *-- Amin Array for save the description
  Dimension laVendAdr[6], laCodes[3,3]
  Store '' To laVendAdr, laCodes
  lnExRate = 1
  Select lcPosHdr
  Scan
    Scatter Memvar Memo
    **---- Search in apvendor file to replace the vendor data
    Select APVENDOR
    If Seek(lcPosHdr.VENDOR)
      *-- Amin This to bueatify the code not for the perforamnce  [Start]
      *--- Save the address for the vendor
      laVendAdr= ''
      gfGetAdr('APVENDOR','','','', @laVendAdr)
      m.cVENCOMP  = APVENDOR.cVENCOMP
      =lfAdrShift('laVendAdr')  && Shift Vendor address if there is empty line.
      m.cADDRESS1 = laVendAdr[1]
      m.cADDRESS2 = laVendAdr[2]
      m.cADDRESS3 = laVendAdr[3]
      m.cADDRESS4 = laVendAdr[4]
      m.cADDRESS5 = laVendAdr[5]
      m.cADDRESS6 = laVendAdr[6]
      *-- Amin This to beautify the code not for the perforamnce  [End]
    Endif
    *--- 'Y' For Print NotePade
    If M.LCRPNOTE='Y'
      Select NotePad
      *-- Amin This shoudl be based on option in Option Grid. Please confirm it
      If Seek(lccStyType+lcPosHdr.PO)
        m.p_notes = NotePad.MNOTES
      Else
        m.p_notes = ""
      Endif
    Endif
    Select (lcWorkFile)
    Append Blank
    Gather Memvar Memo
    *-- Amin  We do not need to refill the array each time  [Start]
    laCodes[1,2] = "CTERMCODE"
    laCodes[2,2] = "SHIPVIA"
    laCodes[3,2] = "CDIVISION"
    laCodes[1,1] = CTERMCODE
    laCodes[2,1] = SHIPVIA
    laCodes[3,1] = cDivision
    *-- Amin  We do not need to refill the array each time  [End]
    = gfCodDes(@laCodes)
    **------ REPLACE THE DESC. OF CODES TO THE WORK FILE

    Replace CTERMDESC With  laCodes[1,3] ,;
      SHIPDESC  With  laCodes[2,3] ,;
      CDIVDESC  With  laCodes[3,3] ,;
      empNots   With Iif(Empty(p_notes),'0','1'),;
      LinkDesc  With Iif(Seek(lcPosHdr.link_code+'013','gl_link'),gl_link.link_code + '-'+gl_link.glacnt,'')
    If llMultCurr
      Replace CCURRDESC With lfCurrDesc()
    Endif
    If LCRPLNGSHR='L'  && IN CASE THE LONG Layout REPORT
      Do Case
        Case lCrpcurr = 'F'  &&  Forgin Curr.
          lnExRate = 1
          lnMyRate=1
          =lfFillCost(@laNormCost,"nFCost",.F.,@laLandAmnt,"nFLanCost",@laActuAmnt,"nFActCost")
        Case lCrpcurr='O'
          lnExRate = nPriceRat
          lnMyRate= nPriceRat
          =lfFillCost(@laNormCost , "nICost",.F.,@laLandAmnt,"nLan_Cost",@laActuAmnt,"nAct_Cost")
        Case lCrpcurr='D' Or lCrpcurr='N'  && curr. rate by  date or by enter cursor
          Store 1 To lnMyRate,lnExRate
          =lfFillCost(@laNormCost,"nFCost",.T.,@laLandAmnt,"nFLanCost",@laActuAmnt,"nFActCost")
          lnExRate = lnMyRate
      Endcase
    Endif
    Select lcPosHdr
  Endscan
  Select (lcWorkFile)
  ** LABEL OF COSTING TYPE
  If LCRPLNGSHR='L'  && Long Layout Report
    Select (lcWorkFile)
    lcISlbl1  = laCost[1,2]
    lcISlbl2  = laCost[2,2]
    lcISlbl3  = laCost[3,2]
    lcISlbl4  = laCost[4,2]
    lcISlbl5  = laCost[5,2]
    lcISlbl6  = laCost[6,2]
    lcISlbl7  = laCost[7,2]
  Endif
  Return
  *!*************************************************************
  *! Name      : lfCrtTable
  *! Developer : Neveen Farouk Zaher (NFZ)
  *! Date      : 07/13/2004
  *! Purpose   : Create Temp. Table as a cursor struct
  *!*************************************************************
  *! Passed Parameters  : ...
  *!*************************************************************
  *! Returns            :
  *!*************************************************************
  *-----------
Procedure lfCrtTable
  *------------
  Select lcPosHdr
  =Afield(laCurName)   && Copy cursor struct for to array
  lnFileStru = Alen(laCurName,1)
  ** add 14 field to temp file for description for currency , terms , ship ,division
  Dimension laCurName[lnFileStru+15,18]
  laCurName[lnFileStru+1,1]  = 'CcurrDesc'
  laCurName[lnFileStru+2,1]  = 'CTERMDESC'
  laCurName[lnFileStru+3,1]  = 'SHIPDESC'
  laCurName[lnFileStru+4,1]  = 'CDIVDESC'
  laCurName[lnFileStru+5,1]  = 'LinkDesc'
  laCurName[lnFileStru+6,1]  = 'EmpNots'
  laCurName[lnFileStru+7,1]  = 'CVENCOMP'
  laCurName[lnFileStru+8,1]  = 'CADDRESS1'
  laCurName[lnFileStru+9,1]  = 'CADDRESS2'
  laCurName[lnFileStru+10,1] = 'CADDRESS3'
  laCurName[lnFileStru+11,1] = 'CADDRESS4'
  laCurName[lnFileStru+12,1] = 'CADDRESS5'
  laCurName[lnFileStru+13,1] = 'CADDRESS6'
  laCurName[lnFileStru+14,1] = 'P_NOTES'
  laCurName[lnFileStru+15,1] = 'cCompDate'
  For lnLoop = 1  To 15  && CREATE THE ARRAY COLUMN FROM 2 TO 18
    If lnLoop=14
      laCurName[lnFileStru+lnLoop,2] = 'M'
      laCurName[lnFileStru+lnLoop,3] = 0
    Else
      laCurName[lnFileStru+lnLoop,2] = 'C'
      laCurName[lnFileStru+lnLoop,3] = 30
    Endif
    Store 0 To laCurName[lnFileStru+lnLoop,4],laCurName[lnFileStru+lnLoop,17],;
      laCurName[lnFileStru+lnLoop,18]

    Store ' ' To laCurName[lnFileStru+lnLoop,7],laCurName[lnFileStru+lnLoop,8],;
      laCurName[lnFileStru+lnLoop,9],laCurName[lnFileStru+lnLoop,10],;
      laCurName[lnFileStru+lnLoop,11],laCurName[lnFileStru+lnLoop,12],;
      laCurName[lnFileStru+lnLoop,13],laCurName[lnFileStru+lnLoop,14],;
      laCurName[lnFileStru+lnLoop,15],laCurName[lnFileStru+lnLoop,16],;
      laCurName[lnFileStru+lnLoop,17]

    laCurName[lnFileStru+lnLoop,5]=.T.
    laCurName[lnFileStru+lnLoop,6]=.F.

  Endfor

  Create Table  oAriaApplication.WorkDir + (lcWorkFile) + ".dbf" From Array laCurName
  *! 125723,1 SMM Solve bug of wrong sorting by date [START]
  Select (lcWorkFile)
  Replace All cCompDate With Dtoc(Complete)
  *! 125723,1 SMM Solve bug of wrong sorting by date [START]

  Return
  *!*************************************************************
  *! Name      : lfCollectData
  *! Developer : Neveen Farouk Zaher (NFZ)
  *! Date      : 07/13/2004
  *! Purpose   : Collect Data From Poshdr (sql Table) & apvendor (fox Table )
  *!*************************************************************
  *! Passed Parameters  : ...
  *!*************************************************************
  *! Returns            :
  *!*************************************************************

  *-----------------
Procedure lfCollectData
  *-----------------
  *!*	IF TYPE("loRDA") <> 'O'
  *!*	  loRDA = CREATEOBJECT("RemoteDataAccess")
  *!*	ENDIF
  *-- The following fields are used as a default in all report cases
  lcRepField="POSHDR.PO       ,poshdr.available ,POSHDR.status ,POSHDR.nPriceRat,POSHDR.nStyORDER,"+;
    "POSHDR.ENTERED  ,POSHDR.COMPLETE  ,POSHDR.VENDOR ,POSHDR.SEASON   , "+;
    "POSHDR.CDIVISION,POSHDR.order_comp,POSHDR.receive,POSHDR.damage   , "+;
    "POSHDR.cancel   ,POSHDR.[open]    ,POSHDR.cpricecur, "+;
    "POSHDR.CTERMCODE,POSHDR.SHIPVIA,POSHDR.LINK_CODE "


  *-- The following fields are used in long layout and cost layout
  If LCRPLNGSHR='L'
    lcRepField=lcRepField+;
      ",poshdr.CWARECODE,poshdr.CDUTYCUR,poshdr.lmultiware, "+;
      "POSHDR.NICOST1   ,POSHDR.NICOST2   ,POSHDR.NICOST3   ,POSHDR.NICOST4,"   +;
      "POSHDR.NICOST5   ,POSHDR.NICOST6   ,POSHDR.NICOST7   , "                 +;
      "POSHDR.NACT_COST1,POSHDR.NACT_COST2,POSHDR.NACT_COST3,POSHDR.NACT_COST4,"+;
      "POSHDR.NACT_COST5,POSHDR.NACT_COST6,POSHDR.NACT_COST7, "                 +;
      "POSHDR.NLAN_COST1,POSHDR.NLAN_COST2,POSHDR.NLAN_COST3,POSHDR.NLAN_COST4,"+;
      "POSHDR.NLAN_COST5,POSHDR.NLAN_COST6,POSHDR.NLAN_COST7, "                 +;
      "POSHDR.NFCOST1   ,POSHDR.NFCOST2   ,POSHDR.NFCOST3   ,POSHDR.NFCOST4, "  +;
      "POSHDR.NFCOST5   ,POSHDR.NFCOST6   ,POSHDR.NFCOST7   , "                 +;
      "POSHDR.nFLanCost1,POSHDR.NfLANCOST2,POSHDR.NfLANCOST3,POSHDR.NfLANCOST4,"+;
      "POSHDR.NfLANCOST5,POSHDR.NfLANCOST6,POSHDR.NfLANCOST7, "                 +;
      "POSHDR.NFACTCOST1,POSHDR.NfACTCOST2,POSHDR.NfACTCOST3,POSHDR.NfACTCOST4,"+;
      "POSHDR.NfACTCOST5,POSHDR.NfACTCOST6,POSHDR.NfACTCOST7,  "                +;
      "POSHDR.SHPNAME   ,POSHDR.INSURANCE ,POSHDR.CFOB      ,POSHDR.QUOTACAT,"  +;
      "POSHDR.CONTACT   ,POSHDR.PHONE     ,POSHDR.ORIGIN    ,POSHDR.LCEXPIRE,"  +;
      "POSHDR.CLCNO "
  Endif
  lcJoin = ''
  *!*	lcWhereCon = " POSHDR.cBusDocu = '" + cBusDocu + "' and POSHDR.cStyType='" + lccstytype +"' "
  lcWhereCon = " POSHDR.cStyType='" + lccStyType +"' "
  * Check if there is a filter on PO Number

  lcCurName = lfCheckFilter(3, 'POSHDR.PO')
  If !Empty(lcCurName)
    Select &lcCurName
    If Reccount() > 0
      lcSQLPO = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO') && SQL Temp File
      If Empty(lcSQLPO)
        *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        Return .F.
      Endif
      lcJoin = lcJoin + " inner join " + lcSQLPO + " TmpPo on POSHDR.PO = TmpPO.PO  "
    Endif
  Endif

  * Check if there is a filter on Vendor
  lcCurName = lfCheckFilter(3, 'POSHDR.VENDOR')
  If !Empty(lcCurName)
    Select &lcCurName
    If Reccount() > 0
      lcSQLVendor = loOgScroll.gfSQLTempName('','Vendor C(8)',lcCurName,'cVendCode') && SQL Temp File
      If Empty(lcSQLVendor)
        *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        Return .F.
      Endif
      lcJoin = lcJoin +" inner join "+ lcSQLVendor +" TmpVend on TmpVend.Vendor = POSHDR.Vendor "
    Endif
  Endif

  * Get Price filter
  lcCurName = lfCheckFilter(1, 'POSHDR.CPRICECUR')
  If !Empty(lcCurName)
    lcCon = lcCurName
    lcCon = Strtran(lcCon,"|","','")
    lcWhereCon = lcWhereCon + " AND POSHDR.CPRICECUR IN ('" + lcCon + "')"
  Endif

  * Get Status Filter
  lcCurName = lfCheckFilter(3, 'POSHDR.STATUS')
  If !Empty(lcCurName)
    lcCon = lcCurName
    lcCon = Strtran(lcCon,"|","','")
    lcWhereCon = lcWhereCon + " AND POSHDR.STATUS IN ('" + lcCon + "')"
  Endif

  * Get cDivision Filter
  lcCurName = lfCheckFilter(3, 'POSHDR.CDIVISION')
  If !Empty(lcCurName)
    lcCon = lcCurName
    lcCon = Strtran(lcCon,"|","','")
    lcWhereCon = lcWhereCon + " AND POSHDR.CDIVISION IN ('" + lcCon + "')"
  Endif

  * Get Entered Filter
  lcCurName = lfCheckFilter(3, 'POSHDR.ENTERED')
  If !Empty(lcCurName)
    lcCon = lcCurName
    lcCon = Strtran(lcCon,"|","' and '")
    lcWhereCon = lcWhereCon + " AND POSHDR.ENTERED Between '" + lcCon + "'"
  Endif

  * Get Completed Filter
  lcCurName = lfCheckFilter(3, 'POSHDR.COMPLETE')
  If !Empty(lcCurName)
    lcCon = lcCurName
    lcCon = Strtran(lcCon,"|","' and '")
    lcWhereCon = lcWhereCon + " AND POSHDR.COMPLETE Between '" + lcCon + "'"
  Endif



  *E303645,1 AEG 02/04/2016 Add available date to PO order Header report[T20151118.0004][Start]
  * Get vailable Filter
  lcCurName = lfCheckFilter(1, 'poshdr.available')
  If !Empty(lcCurName)
    lcCon = lcCurName
    lcCon = Strtran(lcCon,"|","' and '")
    lcWhereCon = lcWhereCon + " AND poshdr.available Between '" + lcCon + "'"
  Endif
  *E303645,1 AEG 02/04/2016 Add available date to PO order Header report[T20151118.0004][Start]


  *: B608436,1 SSH Remove season filter and activate purchase group filter
  * Get Purchase Group Filter
  lcCurName = lfCheckFilter(3, 'POSHDR.CPURCODE')
  If !Empty(lcCurName)
    lcCon = lcCurName
    lcCon = Strtran(lcCon,"|","','")
    lcWhereCon = lcWhereCon + " AND POSHDR.CPURCODE IN ('" + lcCon + "')"
  Endif
  *: B608436,1 SSH[END]

  *!*	lnResult  = loRDA.sqlrun("SELECT "+lcRepField+;
  *!*	                         " FROM POSHDR POSHDR (INDEX = POSHDR) ";
  *!*	                         " WHERE "+ loOgScroll.lcrpsqlexp ,;
  *!*	  	       			     "lcPosHdr",,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)

  lnResult  = loOgScroll.oRDA.sqlrun("SELECT " + lcRepField+;
    " FROM POSHDR POSHDR (INDEX = POSHDR) "+ lcJoin +;
    " WHERE "+ lcWhereCon ,;
    "lcPosHdr",,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)

  Select lcPosHdr

  Return

  *************************************************************
  *! Name      : lfCheckFilter
  *! Developer : Saeed Mohammed (SMM)
  *! Date      : 09/07/2004
  *! Purpose   : Check if the filter was selected
  *!*************************************************************

Function lfCheckFilter()
  Lparameters lnArrayType, lcFilter
  Local lcReturn, lnPOS
  Do Case
    Case lnArrayType = 1
      lnPOS = Ascan(loOgScroll.laOGFxFlt,lcFilter)
      If lnPOS > 0
        lnPOS = Asubscript(loOgScroll.laOGFxFlt,lnPOS,1)
        lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
      Else
        lcReturn = ""
      Endif
    Case lnArrayType = 2
      lnPOS = Ascan(loOgScroll.laOGHDFlt,lcFilter)
      If lnPOS > 0
        lnPOS = Asubscript(loOgScroll.laOGHDFlt,lnPOS,1)
        lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
      Else
        lcReturn = ""
      Endif
    Case lnArrayType = 3
      lnPOS = Ascan(loOgScroll.laOGvrFlt,lcFilter)
      If lnPOS > 0
        lnPOS = Asubscript(loOgScroll.laOGvrFlt,lnPOS,1)
        lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
      Else
        lcReturn = ""
      Endif
    Otherwise :
      lcReturn = ""
  Endcase
  Return lcReturn

  *!*************************************************************
  *! Name      : lfvCurDisp
  *! Developer : Mohamed Atia Badran (MAB)
  *! Date      : 06/30/1999
  *! Purpose   : Activate currency display screen to get user
  *!           : selection for currencies.
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************

Function lfvCurDisp
  *----------------
  llRpProced = gfRepCur(.T., @lCrpcurr,@ldRpExDate,lcRpTmpNam)
  *-- end of lfvCurDisp.
  Return
  ******
  *!*************************************************************
  *! Name      : lfwRepWhen
  *! Developer : AAMER (AHM)
  *! Date      : 05/27/1998
  *! Purpose   : Option Grid When function
  *!*************************************************************
  *! Passed Parameters : None
  *!CALLED FUNCTION  lfItmPos()
  *!*************************************************************
  *! Return      : None
  *!*************************************************************

Function lfwRepWhen
  *----------------
  lCrpcurr = "F"   && Forgin Currency
  *-- Amin
  lcRpName='POMAHDRL'  && Long Layout Report
  *-- Amin

  LNRPFORMAT = Asubscript(LAOGOBJTYPE,Ascan(LAOGOBJTYPE,'LCRPFORMAT'),1)
  LAOGOBJCNT[LNRPFORMAT] = gfUserPriv('IC', 'ICSTYLE', 'COSTING')
  = lfOGShowGet('LCRPFORMAT')
  *-- validate Report format option
  = lfvRepForm()
  *-- end of lfwRepWhen.
  Return
  ************************************************************
  *! Name      : lfFillVars
  *! Developer : Mohamed Atia Badran (MAB)
  *! Date      : 06/30/1999
  *! Purpose   : Fill most of report memory variables.
  ************************************************************
  *! Passed Parameters : None
  ************************************************************
  *! Return      : None
  ************************************************************
Function lfFillVars
  *-------------------
  Dimension laCost [7,2] , laSetUps[15,2]
  laCost = Space(9)
  If !Used('SYCCOMP')
    *-USE &oAriaApplication.SysPath.SYCCOMP ORDER TAG cComp_ID IN 0
    lcSelectCommand=[SELECT * FROM SYCCOMP ]
    lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",Set("DATASESSION"))
  Endif
  *** array to hold the 7 type of cost and labels
  laSetUps[1,1]   = 'M_CITYPE1'
  laSetUps[2,1]   = 'M_CITYPE2'
  laSetUps[3,1]   = 'M_CITYPE3'
  laSetUps[4,1]   = 'M_CITYPE4'
  laSetUps[5,1]   = 'M_CITYPE5'
  laSetUps[6,1]   = 'M_CITYPE6'
  laSetUps[7,1]   = 'M_CITYPE7'

  laSetUps[8,1]   = 'M_CISLBL1'
  laSetUps[9,1]   = 'M_CISLBL2'
  laSetUps[10,1]  = 'M_CISLBL3'
  laSetUps[11,1]  = 'M_CISLBL4'
  laSetUps[12,1]  = 'M_CISLBL5'
  laSetUps[13,1]  = 'M_CISLBL6'
  laSetUps[14,1]  = 'M_CISLBL7'
  laSetUps[15,1]  = 'llMulCurr'
  = gfGetMemVar(@laSetUps)

  laCost[1,1]  = Alltrim(laSetUps[1,2])
  laCost[2,1]  = Alltrim(laSetUps[2,2])
  laCost[3,1]  = Alltrim(laSetUps[3,2])
  laCost[4,1]  = Alltrim(laSetUps[4,2])
  laCost[5,1]  = Alltrim(laSetUps[5,2])
  laCost[6,1]  = Alltrim(laSetUps[6,2])
  laCost[7,1]  = Alltrim(laSetUps[7,2])

  laCost[1,2]  = Left(laSetUps[8,2],9)
  laCost[2,2]  = Left(laSetUps[9,2],9)
  laCost[3,2]  = Left(laSetUps[10,2],9)
  laCost[4,2]  = Left(laSetUps[11,2],9)
  laCost[5,2]  = Left(laSetUps[12,2],9)
  laCost[6,2]  = Left(laSetUps[13,2],9)
  laCost[7,2]  = Left(laSetUps[14,2],9)
  llMultCurr   = laSetUps[15,2]

  If llMultCurr
    *-- Open international file.
    If !Used("SYCINT")
      *- USE (oAriaApplication.SysPath+"SYCINT.DBF") IN 0
      lcSelectCommand=[SELECT * FROM SYCINT ]
      lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",Set("DATASESSION"))
      llOpenInt = .T.
    Endif

    *-- Open exchange rates file.
    If !Used("SYCEXCH")
      Use (oAriaApplication.SysPath+"SYCEXCH.DBF") In 0 Order Tag Currency
      llOpenExch = .T.
    Endif

    *-- Fill Currency arrays [Begin]
    Dimension laCurrVal[1,1]
    *-- Open Currency file.
    If !Used('SYCCURR')
      llOpenCurr = gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
    Else
      Select SYCCURR
      Set Order To CCURRCODE  && To VALIDATE currency code.
    Endif
    Select SYCCURR
    Select Distinct CCURRCODE From SYCCURR Order By CCURRCODE Into Array laCurrVal
    Dimension laCurrDesc[ALEN(laCurrVal,1),1]
    For lnI = 1 To Alen(laCurrVal,1)
      = Seek(Alltrim(laCurrVal[lnI,1]))
      laCurrVal[lnI,1]  = Padr(laCurrVal[lnI,1],3)
      laCurrDesc[lnI,1] = CCURRCODE + ' - ' + Alltrim(CCURRDESC)
    Endfor
    *-- Fill Currency arrays [End  ]
  Endif
  Return
  *-- end of lfFillVars.
  ************************************************************
  *! Name      : lvfRepName
  *! Developer : Neveen Farouk (NFZ)
  *! Date      : 06/30/1999
  *! Purpose   : Change the report name
  ************************************************************
  *! Passed Parameters : None
  ************************************************************
  *! Return      : None
  ************************************************************
  *---------------
Function lvfRepName
  *-----------------
  Do Case
    Case LCRPLNGSHR='S'
      lcRpName='POMAHDRS'
    Case LCRPLNGSHR='L'
      If LCRPFORMAT='Y'
        lcRpName='POMAHDRC'
      Else
        lcRpName='POMAHDRL'
      Endif
  Endcase
  loOgScroll.lcOGLastForm = lcRpName
  Return

  *!*************************************************************
  *! Name      : lfFillCost
  *! Developer : Mohamed Atia Badran (MAB)
  *! Date      : 06/30/1999
  *! Purpose   : Fill any cost array.
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************

Function lfFillCost
  *----------------
  Parameters lcArray1,lcField1,llCallCurr,lcArray2,lcField2,lcArray3,lcField3

  Private lnCstItem , lnCurntAls
  lnCurntAls = Select(0)
  Store 0 To lcArray1,lnCstItem
  For lnCstItem = 1 To 7   && 7 for 7 types of Cost
    lcCstItem = Str(lnCstItem,1)
    lcArray1[lnCstItem] = Evaluate(lcField1+lcCstItem)

    If Type("lcField2") = "C"
      lcArray2[lnCstItem] = Evaluate(lcField2+lcCstItem)
    Endif

    If Type("lcField3") = "C"
      lcArray3[lnCstItem] = Evaluate(lcField3+lcCstItem)
    Endif

    If llCallCurr And (lcArray1[lnCstItem] <> 0 Or ;
        (Type("lcField2") = "C" And lcArray2[lnCstItem] <> 0) Or;
        (Type("lcField3") = "C" And lcArray3[lnCstItem] <> 0))

      *-- IF P.Price. and currency not equal base currency.
      If laCost[lnCstItem,1] = "P" And (cPriceCur <> oAriaApplication.BaseCurrency)
        m.CCURRCODE = cPriceCur
        =lfAmntDisp()  && Function for display Amount Based the Currency
        lnMyRate = lnExRate
      Endif

      *-- IF Duty or Manufacturing (Misc.) Operations and currency not equal base currency.
      If laCost[lnCstItem,1] $ "MD" And (cDutyCur <> oAriaApplication.BaseCurrency)
        m.CCURRCODE = cDutyCur
        =lfAmntDisp()
      Endif
    Endif
    Select (lcWorkFile)
    lcCostField="nICost"+lcCstItem
    lcLanCostField="nLan_Cost"+lcCstItem
    lcActCostField="nAct_Cost"+lcCstItem

    lcFCostField="nFCost"+lcCstItem
    lcFLanCostField="nFLanCost"+lcCstItem
    lcFActCostField="nFActCost"+lcCstItem

    If  lCrpcurr='D' Or lCrpcurr='N'
      Replace &lcCostField    With lcArray1[lnCstItem],;
        &lcLanCostField With lcArray2[lnCstItem],;
        &lcActCostField With lcArray3[lnCstItem] ,;
        nPriceRat       With  lnMyRate

    Endif
    ** COPY THE TOTAL BASIC CURRENCY TO FORGHIN BECOUSE
    ** THE CRYSTAL REPORT TAKE DATA FROM NF FIELD
    ** THIS TYPE USED THE NORMAL COST AND I COPY IT TO FORIGN COST FIELD
    Replace &lcFCostField    With &lcCostField  ,;
      &lcFLanCostField With &lcLanCostField ,;
      &lcFActCostField With &lcActCostField  ,;
      nPriceRat        With lnMyRate

  Endfor
  Select (lnCurntAls)
  Return
  *-- end of lfFillCost.
  *!*************************************************************
  *! Name      : lfAmntDisp
  *! Developer : Mohamed Atia Badran (MAB)
  *! Date      : 06/30/1999
  *! Purpose   : Calculate equavelent amount based on currency of transaction
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************

Function lfAmntDisp
  *------------
  lcArray1[lnCstItem] = gfAmntDisp(lcArray1[lnCstItem],lCrpcurr,ldRpExDate,lcRpTmpNam)
  If Type("lcField2") = "C"
    lcArray2[lnCstItem] = gfAmntDisp(lcArray2[lnCstItem],lCrpcurr,ldRpExDate,lcRpTmpNam)
  Endif
  If Type("lcField3") = "C"
    lcArray3[lnCstItem] = gfAmntDisp(lcArray3[lnCstItem],lCrpcurr,ldRpExDate,lcRpTmpNam)
  Endif
  Select (lnCurntAls)
  Return
  *-- end of lfAmntDisp.
  *!*************************************************************
  *! Name      : lfvRepForm
  *! Developer : AAMER (AHM)
  *! Date      : 05/27/1998
  *! Purpose   : validate Report format option
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
Function lfvRepForm
  *-------------------
  LNPRNNOTEPO = Asubscript(LAOGOBJTYPE,Ascan(LAOGOBJTYPE,'LCRPNOTE'),1)
  LAOGOBJCNT[LNPRNNOTEPO] = (LCRPFORMAT = 'N')
  If LCRPFORMAT = 'Y'
    LCRPNOTE = 'N'
  Endif
  =lfOGShowGet('lCRPNOTE')

  LNLNGSHRPO = Asubscript(LAOGOBJTYPE,Ascan(LAOGOBJTYPE,'LCRPLNGSHR'),1)
  LAOGOBJCNT[LNLNGSHRPO] = (LCRPFORMAT = 'N')
  = lfOGShowGet('LCRPLNGSHR')
  If LCRPFORMAT = 'Y'  && 'Y' For Costing Format
    LCRPLNGSHR = 'L'   && Long Layout Report
  Endif
  =lvfRepName()
  Return
  *!*	*!*************************************************************
  *!*	*! Name      : lfCurrDesc
  *!*	*! Developer : Mohamed Badran (MAB)
  *!*	*! Date      : 06/30/1999
  *!*	*! Purpose   : Currency description if sort by currency.
  *!*	*!*************************************************************
  *!*	*! Passed Parameters  : None
  *!*	*!*************************************************************
  *!*	*! Returns            : Currency description.
  *!*	*!*************************************************************
Function lfCurrDesc
  *---------------------
  Private lcCurrVal , lcCurDesc
  lcCurDesc = ''
  lcCurrVal  = Alltrim(cPriceCur)
  lnCurVlPos = Ascan(laCurrVal,lcCurrVal)
  If lnCurVlPos > 0
    lcCurDesc  = laCurrDesc[lnCurVlPos,1]
  Endif
  Return Padr(Alltrim(lcCurDesc),18)
  *-- end of lfCurrDesc.


  *!*************************************************************
  *! Name      : lfAdrShift
  *! Developer : Mohamed Atia Badran (MAB)
  *! Date      : 07/01/1999
  *! Purpose   : Function to Shift the Address array if there is any
  *!             empty lines in the address
  *!*************************************************************
  *! Passed Parameters : Address Array name
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfAdrShift()
  *!*************************************************************
  *MAB 07/01/1999 Speed Report performance.
Function lfAdrShift
  Parameters lcArrayNam
  Local lnCount
  For lnCount = 1 To Alen(&lcArrayNam.,1)
    *IF The current Array element is of type character and empty
    If Type(lcArrayNam + "[" + Str(lnCount , 1) + "]") = "C" .And.;
        EMPTY(&lcArrayNam[lnCount])
      =Adel(&lcArrayNam ,lnCount)
      lnCount = lnCount - 1
    Endif    && End of IF
  Endfor    && End of FOR Loop
  *FOR Loop to loop the Address Array
  For lnCount = 1 To Alen(&lcArrayNam.,1)
    *IF The current Array element is not of type character
    If Type(lcArrayNam + "[" + Str(lnCount , 1) + "]") <> "C" .And. ;
        EMPTY(&lcArrayNam[lnCount])
      &lcArrayNam[lnCount] = ''
    Endif    && End of IF
  Endfor    && End of FOR Loop
  *-- end of lfAdrShift.
