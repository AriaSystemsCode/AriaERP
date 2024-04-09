*!*************************************************************
*! Name      : lfPckListGrid
*! Developer : Mariam Mazhar
*! Date      : 01/30/2008
*! Purpose   : Function to call packing list option grid
*!*************************************************************
FUNCTION lfPckListGrid
  PARAMETERS llShowOpG


  llEdiSys = ('AS' $ oAriaApplication.CompanySetupModules)
  STORE 0 TO lnRpWght, lnRpTtCrtP, lnRpMnlQty
  STORE .F. TO llRpMltSku, llRp1BoxPk
  STORE ' ' TO lcRpBrkTyp, lcRpSortBy, lcRPCart
  STORE .F. TO llRpBolAsi, llRpUseExs, llRpUsePre,llFromWz

  *lcExpr = gfOpGrid('ALAUTP',.T. ,.F. ,.F. ,.T. ,.T.)

  IF llShowOpG
    lcExpr = gfWizOptionGrid(oAriaApplication.workdir+lcautplistfile +'.xml', 'ALAUTP',.F.,.T.,"R",.T.,.T.,.T.)   && Allocation logic grid.
  ELSE
    llRpUseExs = .F.
    llRpMltSku = .T.
    lcRpBrkTyp = 'I'
    lcExpr = gfWizOptionGrid(oAriaApplication.workdir+lcautplistfile +'.xml', 'ALAUTP',.T.,.T. ,.F. ,.F. ,.T. ,.T.)
  ENDIF

  *!*************************************************************
  *! Name      : lfPltCrts
  *! Developer : Mariam Mazhar
  *! Date      : 01/30/2008
  *! Purpose   : Function to get pallet cartons
  *!*************************************************************
FUNCTION lfPltCrts

  PRIVATE laAccount,lnSlct,lcPkAls
  DIMENSION laAccount[1]
  laAccount[1] = .F.
  lnSlct = SELECT()

  IF llEdiSys AND llRpBolAsi
    =gfOpenFile(oAriaApplication.DataDir + 'EDIACPRT' , oAriaApplication.DataDir + 'ACCFACT' , 'SH')
    =gfOpenFile(oAriaApplication.DataDir + 'EDIPH' , oAriaApplication.DataDir + 'PARTNER' , 'SH')

    lnPikTkPos = ASUBSCRIPT( laOgFxFlt , ASCAN(laOgFxFlt,'PIKTKT.PIKTKT') , 1 )
    lcPkAls = laOgFxFlt[lnPikTkPos,6]
    IF USED(lcPkAls) AND RECCOUNT(lcPkAls) > 0

      SELECT DISTINCT ACCOUNT ;
        FROM PIKTKT , &lcPkAls , EDIACPRT , EDIPH ;
        WHERE PIKTKT.PIKTKT = &lcPkAls..PIKTKT ;
        AND PIKTKT.STATUS $ 'OP' ;
        AND PIKTKT.ACCOUNT = EDIACPRT.CPARTNER ;
        AND EDIACPRT.CPARTCODE = EDIPH.CPARTCODE ;
        AND EDIPH.LPLTSHP INTO ARRAY laAccount

    ELSE

      SELECT DISTINCT ACCOUNT ;
        FROM PIKTKT , EDIACPRT , EDIPH ;
        WHERE PIKTKT.STATUS $ 'OP' ;
        AND PIKTKT.ACCOUNT = EDIACPRT.CPARTNER ;
        AND EDIACPRT.CPARTCODE = EDIPH.CPARTCODE ;
        AND EDIPH.LPLTSHP INTO ARRAY laAccount
    ENDIF

    RETURN EMPTY(laAccount[1])

  ENDIF
  *-- end of lfPltCrts.
  *!*************************************************************
  *! Name      : lfvWghtQty
  *! Developer : Mariam Mazhar
  *! Date      : 01/30/2008
  *! Purpose   : Function to get refersh option grid
  *!*************************************************************
FUNCTION lfvWghtQty
  CLEARREAD()

  *:**************************************************************************
  *:* Name        : lfBoxPkt
  *:* Developer   : Mariam Mazhar
  *:* Date        : 01/30/2008
  *:* Purpose     : Valid fn. for "Single Box per Pick Tkt"
  *:***************************************************************************
  *:* Called from :
  *:***************************************************************************
  *:* Parameters : None
  *:***************************************************************************
  *:* Return      : None
  *:***************************************************************************
  *:* Example     :  = lfBoxPkt()
  *:***************************************************************************
FUNCTION lfBoxPkt
  CLEARREAD()

  *-- end of lfBoxPkt.
  *!*************************************************************
  *! Name      : lfvBolAsgn
  *! Developer : Mariam Mazhar
  *! Date      : 01/30/2008
  *! Purpose   : Function to get refersh option grid
  *!*************************************************************
FUNCTION lfvBolAsgn
  CLEARREAD()

  *:**************************************************************************
  *:* Name        : lfMltSku
  *:* Developer   : Mariam Mazhar
  *:* Date        : 01/30/2008
  *:* Purpose     : Valid fn. for "Multiple SKU" Option
  *:***************************************************************************
  *:* Called from :
  *:***************************************************************************
  *:* Parameters : None
  *:***************************************************************************
  *:* Return      : None
  *:***************************************************************************
  *:* Example     :  = lfMltSku()
  *:***************************************************************************
FUNCTION lfMltSku
  CLEARREAD()
  *-- end of lfMltSku.

  *:**************************************************************************
  *:* Name        : lfvUnits
  *:* Developer   : Mariam Mazhar
  *:* Date        : 01/30/2008
  *:* Purpose     : Valid fn. for units
  *:***************************************************************************
FUNCTION lfvUnits
  PRIVATE lcStats
  lcStats = IIF(lnRpMnlQty > 0,'ENABLE','DISABLE')
  SHOW GET pbRun &lcStats
  IF lnOgSeting = 1 AND lnRpMnlQty > 0
    SAVE ALL LIKE lnRpMnlQty* TO MnlQty.MEM
  ENDIF

  *:**************************************************************************
  *:* Name        : lfvWieght
  *:* Developer   : Mariam Mazhar
  *:* Date        : 01/30/2008
  *:* Purpose     : Valid fn. for weight
  *:***************************************************************************

FUNCTION lfvWieght
  PRIVATE lcStats
  lcStats = IIF(lnRpWght > 0,'ENABLE','DISABLE')
  SHOW GET pbRun &lcStats

  *:**************************************************************************
  *:* Name        : lfwOGWhen
  *:* Developer   : Mariam Mazhar
  *:* Date        : 01/30/2008
  *:* Purpose     : when function
  *:***************************************************************************
FUNCTION lfwOGWhen

  IF lnOgSeting = 1 AND FILE('MnlQty.MEM')
    RESTORE FROM MnlQty.MEM ADDITIVE
  ENDIF

  llRpUseExs = .F.
  llRpMltSku = .T.
  lcRpBrkTyp = 'I'

  lnInd = ASUBSCRIPT(looGScroll.laOgFxFlt,ASCAN(looGScroll.laOgFxFlt,'PIKTKT.PIKTKT'),1)
  lcTempOrdFile = looGScroll.gfTempName()
  CREATE CURSOR (lcTempOrdFile) (KeyExp C(6),piktkt C(6))
  SELECT (lcTempOrdFile)
  INDEX ON KeyExp TAG (lcTempOrdFile)

  SELECT PikTkt
  lcPKOrd = ORDER()
  SET ORDER TO ORDPIK
  gfSEEK(lcOrderNo,'PikTkt')
  SCAN REST WHILE ORDER+piktkt = lcOrderNo FOR Status ='O'
    INSERT INTO (lcTempOrdFile) (PikTkt,keyExp) VALUES (PikTkt.PikTkt,PikTkt.PikTkt)
  ENDSCAN

  SET ORDER TO (lcPKOrd) IN PikTkt

  laOgFxFlt[lnInd,6] = lcTempOrdFile

  =loOGScroll.RefreshScroll()

  IF llShowOpG
    loOGScroll.SetFilterExpr()
    RETURN .F.
  ENDIF
  *wizard
    IF ASCAN(LOOGSCROLL.LAOGFXFLT,'PIKTKT.PIKTKT') # 0
    *-- GET THE POSITION OF THE Pick Ticket # IN THE VARAIBLE FILTER
    LNPKTKTNO = ASUBSCRIPT( LOOGSCROLL.LAOGFXFLT, ASCAN(LOOGSCROLL.LAOGFXFLT,'PIKTKT.PIKTKT'),1)
    LOOGSCROLL.LAOGOBJCNT[ALEN(LOOGSCROLL.LAOGOBJCNT,1) - ALEN(LOOGSCROLL.LAOGFXFLT,1) +  LNPKTKTNO ] = .F.
    = LFOGSHOWGET('LOOGSCROLL.LAOGFXFLT[' + ALLTRIM(STR(LNPKTKTNO)) + ',6]')
  ENDIF
  IF ASCAN(LOOGSCROLL.LAOGFXFLT,'PIKTKT.DATE') # 0
    *-- GET THE POSITION OF THE Pick Ticket Data IN THE VARAIBLE FILTER
    LNPKTKTDT = ASUBSCRIPT( LOOGSCROLL.LAOGFXFLT, ASCAN(LOOGSCROLL.LAOGFXFLT,'PIKTKT.DATE'),1)
    LOOGSCROLL.LAOGOBJCNT[ALEN(LOOGSCROLL.LAOGOBJCNT,1) - ALEN(LOOGSCROLL.LAOGFXFLT,1) +  LNPKTKTDT ] = .F.
    = LFOGSHOWGET('LOOGSCROLL.LAOGFXFLT[' + ALLTRIM(STR(LNPKTKTDT)) + ',6]')
  ENDIF

  *wizard

  *!*************************************************************
  *! Name      : lfInvSet
  *! Developer : Mariam Mazhar
  *! Date      : 01/30/2008
  *! Purpose   : Set function of In Range
  *!*************************************************************
FUNCTION lfPktSet
  PARAMETERS lcParm
  IF lcParm = 'R'
    CLEARREAD()
  ENDIF

  *!*************************************************************
  *! Name      : lfGetTkt
  *! Developer : Mariam Mazhar
  *! Date      : 01/30/2008
  *! Purpose   : get tkt no.
  *!*************************************************************
FUNCTION lfGetTkt

  STORE ""  TO lcPikTktFile
  STORE .F. TO llPikTktRng
  lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'PIKTKT.PIKTKT'),1)
  IF lnInd <> 0
    lcPikTktFile = laOgFxFlt[lnInd,6]
    llPikTktRng  = (!EMPTY(lcPikTktFile) .AND. USED(lcPikTktFile) .AND. RECCOUNT(lcPikTktFile)>0)
  ENDIF
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


  lcRpScpMod ="O"
  lnInd = ASUBSCRIPT(looGScroll.laOgFxFlt,ASCAN(looGScroll.laOgFxFlt,'ORDHDR.ORDER'),1)
  lcTempOrdFile = looGScroll.gfTempName()
  CREATE CURSOR (lcTempOrdFile) (KeyExp C(6),ORDER C(6))
  SELECT (lcTempOrdFile)
  INDEX ON KeyExp TAG (lcTempOrdFile)
  APPEND BLANK
  REPLACE ORDER WITH lcOrderNo,;
    keyExp WITH lcOrderNo


  laOgFxFlt[lnInd,6] = lcTempOrdFile


  =loOGScroll.RefreshScroll()

  IF ASCAN(laOGObjType, 'LCRPSCPMOD') # 0
    lnSelMode = ASUBSCRIPT(laOGObjType, ASCAN(laOGObjType, 'LCRPSCPMOD'),1)
    laOGObjCnt[lnSelMode] = .F.
    = lfOGShowGet('LCRPSCPMOD')
  ENDIF

  lnDummyPos = lfItmPos('llDummy')
  llRpIncHor =.T.
  lcSOrdStat = IIF(llRpIncHor,"OH","O")

  =lfvExlBulk()

  IF llShowOpG
    loOGScroll.SetFilterExpr()
    RETURN .F.
  ENDIF

  FOR i=1 TO LOOGSCROLL.OuterContainer.innerContainer.CONTROLCOUNT
    DO CASE

      CASE LOOGSCROLL.OuterContainer.innerContainer.CONTROLS[i].oitem.mfld_name = 'llDummy'
        LOOGSCROLL.OuterContainer.innerContainer.CONTROLS[i].ENABLED = .F.
        LOOGSCROLL.OuterContainer.innerContainer.CONTROLS[i].OBJECTS[5].ENABLED = .F.
    ENDCASE
  ENDFOR

  llRpGdExcl = .F.
  IF ASCAN(laOGObjType,'LLRPGDEXCL') # 0
    *-- GET THE POSITION OF THE Exclude Orders IN THE VARAIBLE FILTER
    LNEXLORD = ASUBSCRIPT( laOGObjType, ASCAN(laOGObjType,'LLRPGDEXCL'),1)
    LAOGOBJCNT[LNEXLORD] = .F.
    = LFOGSHOWGET('LLRPGDEXCL')
  ENDIF

  IF ASCAN(laOGObjType,'LLRPINCHOR') # 0
    *-- GET THE POSITION OF THE Include Hold Orders IN THE VARAIBLE FILTER
    LNINCHOLD = ASUBSCRIPT( laOGObjType, ASCAN(laOGObjType,'LLRPINCHOR'),1)
    LAOGOBJCNT[LNINCHOLD] = .F.
    = LFOGSHOWGET('LLRPINCHOR')
  ENDIF
  
  IF ASCAN(LOOGSCROLL.LAOGFXFLT,'ALLTRIM(ORDHDR.ORDER)') # 0
    *-- GET THE POSITION OF THE ORDER IN THE VARAIBLE FILTER
    LNORDNO = ASUBSCRIPT( LOOGSCROLL.LAOGFXFLT, ASCAN(LOOGSCROLL.LAOGFXFLT,'ALLTRIM(ORDHDR.ORDER)'),1)
    LOOGSCROLL.LAOGOBJCNT[ALEN(LOOGSCROLL.LAOGOBJCNT,1) - ALEN(LOOGSCROLL.LAOGFXFLT,1) +  LNORDNO ] = .F.
    = LFOGSHOWGET('LOOGSCROLL.LAOGFXFLT[' + ALLTRIM(STR(LNORDNO)) + ',6]')
  ENDIF
  IF ASCAN(LOOGSCROLL.LAOGFXFLT,'ALLTRIM(ORDHDR.ACCOUNT)') # 0
    *-- GET THE POSITION OF THE ACCOUNT IN THE VARAIBLE FILTER
    LNACCNO = ASUBSCRIPT( LOOGSCROLL.LAOGFXFLT, ASCAN(LOOGSCROLL.LAOGFXFLT,'ALLTRIM(ORDHDR.ACCOUNT)'),1)
    LOOGSCROLL.LAOGOBJCNT[ALEN(LOOGSCROLL.LAOGOBJCNT,1) - ALEN(LOOGSCROLL.LAOGFXFLT,1) +  LNACCNO  ] = .F.
    = LFOGSHOWGET('LOOGSCROLL.LAOGFXFLT[' + ALLTRIM(STR(LNACCNO)) + ',6]')
  ENDIF
  IF ASCAN(LOOGSCROLL.LAOGFXFLT,'ALLTRIM(ORDHDR.CWARECODE)') # 0
    *-- GET THE POSITION OF THE WARE HOUSE IN THE VARAIBLE FILTER
    LNWHCODE = ASUBSCRIPT( LOOGSCROLL.LAOGFXFLT, ASCAN(LOOGSCROLL.LAOGFXFLT,'ALLTRIM(ORDHDR.CWARECODE)'),1)
    LOOGSCROLL.LAOGOBJCNT[ALEN(LOOGSCROLL.LAOGOBJCNT,1) - ALEN(LOOGSCROLL.LAOGFXFLT,1) +  LNWHCODE  ] = .F.
    = LFOGSHOWGET('LOOGSCROLL.LAOGFXFLT[' + ALLTRIM(STR(LNWHCODE)) + ',6]')
  ENDIF
  IF ASCAN(LOOGSCROLL.LAOGFXFLT,'ORDHDR.START') # 0
    *-- GET THE POSITION OF THE START DATA IN THE VARAIBLE FILTER
    LNSTRTDT = ASUBSCRIPT( LOOGSCROLL.LAOGFXFLT, ASCAN(LOOGSCROLL.LAOGFXFLT,'ORDHDR.START'),1)
    LOOGSCROLL.LAOGOBJCNT[ LNSTRTDT  ] = .F.
    = LFOGSHOWGET('LOOGSCROLL.LAOGFXFLT[' + ALLTRIM(STR(LNSTRTDT)) + ',6]')
  ENDIF
  IF ASCAN(LOOGSCROLL.LAOGFXFLT,'ORDHDR.ENTERED') # 0
    *-- GET THE POSITION OF THE ENTER DATA IN THE VARAIBLE FILTER
    LNENTDT = ASUBSCRIPT( LOOGSCROLL.LAOGFXFLT, ASCAN(LOOGSCROLL.LAOGFXFLT,'ORDHDR.ENTERED'),1)
    LOOGSCROLL.LAOGOBJCNT[LNENTDT  ] = .F.
    = LFOGSHOWGET('LOOGSCROLL.LAOGFXFLT[' + ALLTRIM(STR(LNENTDT)) + ',6]')
  ENDIF
  IF ASCAN(LOOGSCROLL.LAOGFXFLT,'ORDHDR.COMPLETE') # 0
    *-- GET THE POSITION OF THE ENTER DATA IN THE VARAIBLE FILTER
    LNCMPDT = ASUBSCRIPT( LOOGSCROLL.LAOGFXFLT, ASCAN(LOOGSCROLL.LAOGFXFLT,'ORDHDR.COMPLETE'),1)
    LOOGSCROLL.LAOGOBJCNT[LNCMPDT ] = .F.
    = LFOGSHOWGET('LOOGSCROLL.LAOGFXFLT[' + ALLTRIM(STR(LNCMPDT)) + ',6]')
  ENDIF
  *Wizard

  *!*************************************************************
  *! Name      : lfvIncHold
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Include Hold Orders.
  *!*************************************************************
FUNCTION lfvIncHold
  lcSOrdStat = IIF(llRpIncHor,"OH","O")
  *!*************************************************************
  *! Name      : lfvExlBulk
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Exclude bulk order Orders.
  *!*************************************************************
FUNCTION lfvExlBulk
  lcBulkExp = IIF(llRpExlBlk,""," AND BULK='N'")

  *!*************************************************************
  *! Name      : lfAutAllGird
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Exclude bulk order Orders.
  *!*************************************************************
FUNCTION lfAutAllGird
  PARAMETERS llSelect,llLogic,llShowOpG


  oGetItemMask = CREATEOBJECT('GetItemMask')
  lnMajSeg   = oGetItemMask.DO('SM')  && No. of major segments.
  DIMENSION laMajSegs[1,1]
  oGetItemMask.DO(@laMajSegs)
  STORE 0  TO lnNonMajSt
  STORE "" TO lcNonMajPi,lcNonMajTl
  STORE .T. TO llCallScop , llFirstRun



  FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
    IF laMajSegs[lnI,1] $ 'CF'
      lcFree_Clr = laMajSegs[lnI,1]
      lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
      lnSupMajSt = lnNonMajSt
      lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
        laMajSegs[lnI,3],;
        lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
      lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
        PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
        lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    ENDIF

    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
      EXIT
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
  ENDFOR    && end Loop Around Non Major elements.

  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  lcColorTlt = 'Only These '+ ALLTRIM(lcNonMajTl) + 's.'


  lcStyMajor = oGetItemMask.DO('HM')
  lcDivision = lcStyMajor + ' Division'
  lcStyGroup = lcStyMajor + ' Style Group'
  lcFabTlt   = lcStyMajor + ' Fabric Code'
  lcPatTlt   = lcStyMajor + ' Pattern'
  lcSeason   = lcStyMajor + ' Season'

  DECLARE laSetups[8,2]
  laSetups[1,1] = 'M_DYELOT'
  laSetups[2,1] = 'M_MATDYE'
  laSetups[3,1] = 'M_WareHouse'
  laSetups[4,1] = 'M_FORCEALO'
  laSetups[5,1] = 'M_TOTAVLBL'
  laSetups[6,1] = 'M_CMPDOLN'
  laSetups[7,1] = 'M_CHKAPROV'
  laSetups[8,1] = 'M_STYCNFG'
  =gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)

  llUseDyes  = (UPPER(ALLTRIM(laSetups[1,2])) = 'Y')
  llUseConfg = (UPPER(ALLTRIM(laSetups[8,2])) = 'Y')
  LCRPSCPMOD = ''

  DIMENSION laSortAry[4,2],laIndexExp[6]
  laSortAry = ''
  laSortAry[1,1] = 1
  laSortAry[1,2] = [DTOS(COMPLETE)]
  laSortAry[2,1] = 2
  laSortAry[2,2] = [PRIORITY]
  laSortAry[3,1] = 3
  laSortAry[3,2] = [DTOS(START)]
  laSortAry[4,1] = 4
  laSortAry[4,2] = [ORDER]

  laIndexExp[1] = 'DTOS(COMPLETE)'
  laIndexExp[2] = 'PRIORITY'
  laIndexExp[3] = 'DTOS(START)'
  laIndexExp[4] = 'ORDER'
  laIndexExp[5] = 'ACCOUNT'
  laIndexExp[6] = ''


  lnSubtract = 0
  lnSubtract = IIF('MF' $ oAriaApplication.CompanyInstalledModules,lnSubtract, lnSubtract + 1)
  lnSubtract = IIF('PO' $ oAriaApplication.CompanyInstalledModules,lnSubtract, lnSubtract + 1)

  DIMENSION laSlctDesc[6 - lnSubtract,1], laSlctVals[6 - lnSubtract,1]
  laSlctDesc[1,1] = 'All'
  laSlctDesc[2,1] = lcStyMajor
  laSlctDesc[3,1] = 'Order'
  laSlctDesc[4,1] = 'Account'
  laSlctVals[1,1] = " "
  laSlctVals[2,1] = "S"
  laSlctVals[3,1] = "O"
  laSlctVals[4,1] = "A"

  DO CASE
    CASE lnSubtract = 0
      laSlctDesc[5,1] = 'Cutting ticket'
      laSlctDesc[6,1] = 'Purchase order'

      laSlctVals[5,1] = 'K'
      laSlctVals[6,1] = 'P'
    CASE lnSubtract = 1
      IF 'MF' $ oAriaApplication.CompanyInstalledModules
        laSlctDesc[5,1] = 'Cutting ticket'
        laSlctVals[5,1] = 'K'
      ELSE
        laSlctDesc[5,1] = 'Purchase order'
        laSlctVals[5,1] = 'P'
      ENDIF
  ENDCASE
  STORE .T. TO llRpExlDye,llRpExlBlk,llRpPikSep,llRpPikCor,llRpGdExcl
  STORE " "  TO lcRpSepCor

  *Wiz
  *STORE .F. TO llRpIncHOr,llRpAlocat,llRpGenPik,llExclude
   STORE .F. TO llRpAlocat,llRpGenPik,llExclude
   STORE .T. TO llRpIncHOr
  *Wiz
  STORE " " TO lcRpExSlct,lcRpSepCor,lcRpAloNot,lcRpScpMod,lcSOrdStat
  STORE 0 TO lnDummyPos,lnRngAlias
  lcConfgTlt = IIF(llUseConfg ,'Exclude styles with Confg',	'Exclude styles with dyelot')
  STORE 100 TO lnRpPikSep,lnRpPikCor
  STORE 0 TO lnRpCutUnt ,lnRpSort1 ,lnRpSort2 ,lnRpSort3,lnRpSort4
  STORE " " TO lcRpPkFWrh,lcRpIncWip

  lcPOTlt  = "Purchase order number    "
  lcBrwFld = "PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
    "APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
    "Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'"

  STORE .F. TO llClrPO1 , llClrPO2 , llClrOrd2 , llClrSty2 , llClrOrd1 , llClrSty1 , llClrAcc2 , llClrAcc1

  lnO_T_S = 0

  STORE .F. TO LLCLRCT1,LLCLRCT2,llRpForAlo,LLRPCOND


  llAlwForce = lfSuppForc(ALLTRIM(laSetups[4,2]))

  llRpGenPik = .T.

  STORE 0 TO LNACTIVOBJ

  lcExpr = ''




  llnoShow =.T.
  IF llSelect

    IF llShowOpG
      *!*     DECLARE laOGFxFlt[1,8], laOGHdFlt[1,8], laOGVrFlt[1,8], laOGSeting[1,2],;
      *!*             laOGObjType[1,3], laOGObjCnt[1], laRangeInfo[1,2]

      *!*     lcClassDir   = ADDBS(oAriaApplication.ClassDir)
      *!*     oOptionGrid  = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
      *!*     loOGScroll   = oOptionGrid.OptionGrid.oHost
      *!*     loogscroll.lRunInAria = .T.
      *!*     loogscroll.lnRepIDLen = 8
      *!*     loogscroll.lcRunDirct = 'R'
      *!*     loogscroll.lShowOG = .F.
      *!*     loogscroll.llProgram = .T.
      *!*     *loogscroll.lcOGReadW = 'lfwFilter()'
      *!*
      *!*     lcSqlString = "SELECT cFld_Name,cFld_Head,cData_Typ,nFld_Wdth,nFld_Dec,cPict_Str,mVald_Str,lVldEntry FROM SYDFIELD Order By cFld_Name"
      *!*     lnRemoteResult = loogscroll.SQLExecute("SYDFIELD",lcSqlString,;
      *!*                                    '',"SYDFIELD","",oAriaApplication.SystemConnectionString,;
      *!*                                    3)
      *!*     lnRemoteResult = loogscroll.SQLExecute("SYDREPRT","Select * from SYDREPRT where cVer <> 'A27' and cRep_ID='"+;
      *!*     									  PADR(ALLTRIM('ALAUTSLC'),loogscroll.lnRepIDLen)+"'",;
      *!*  	                                  '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
      *!*  	                                  3)

      *!*
      *!*     lcSelectStatement = "Select * from SYREPUVR where " +;
      *!*     "cRep_ID='"+PADR(ALLTRIM('ALAUTSLC'),loogscroll.lnRepIDLen)+"' AND (EMPTY(CVER) OR CVER = 'A40')"
      *!*
      *!*     lnRemoteResult = loogscroll.SQLExecute("SYREPUVR", lcSelectStatement + " Order By cExpType, nVarPos",;
      *!*                                    '',"SYREPUVR","",oAriaApplication.cAria4SysFiles,;
      *!*                                    3)
      *!*
      *!*      loOGScroll.initrepheader()
      *!*      loOGScroll.GetRepVar()
      *!*      loOGScroll.defineobjects()
      *!*      x=loOGScroll.convertvariablestoxml ()
      *!*

      *.F.,SET("Datasession")

      *!*      lfGetRepVar()
      *!*
      *!*      IF USED('SYDREPRT') AND USED('SYREPUVR')
      *!*
      *!*        SELECT Sydreprt
      *!*
      *!*        IF !EMPTY(ALLTRIM(mRepHdFlt))
      *!*          RESTORE FROM MEMO mRepHdFlt ADDITIVE
      *!*        ENDIF
      *!*        IF TYPE("laOGHdFlt[1]") = "C"
      *!*          loogscroll.CopyFltArray(@laOGHdFlt,"laOGHdFlt")
      *!*        ENDIF
      *!*  	    *-- Restore fixed filters.
      *!*    	  IF !EMPTY(ALLTRIM(mRepFxFlt))
      *!*    	    RESTORE from Memo mRepFxFlt ADDITIVE
      *!*    	    IF !EMPTY(laOGFxFlt[1,1])
      *!*    	      DECLARE loogscroll.laOGFxFlt[ALEN(laOGFxFlt,1), ALEN(laOGFxFlt,2)]
      *!*    	      ACOPY(laOGFxFlt,loogscroll.laOGFxFlt)
      *!*    	    ENDIF
      *!*    	    loogscroll.InitValue('laOGFxFlt','C')
      *!*    	  ENDIF

      *!*    	  *-- Restore variable filters if this report allow variable filter.
      *!*    	  IF !EMPTY(ALLTRIM(mRepVrFlt))
      *!*          RESTORE FROM memo mrepVrflt ADDITIVE
      *!*    	    IF !EMPTY(laOGVrFlt[1,1])
      *!*    	      DECLARE loogscroll.laOGVrFlt[ALEN(laOGVrFlt,1), ALEN(laOGVrFlt,2)]
      *!*    	      ACOPY(laOGVrFlt,loogscroll.laOGVrFlt)
      *!*    	    ENDIF
      *!*    	    loogscroll.InitValue('laOGVrFlt','C')
      *!*    	  ENDIF

      *!*   	    && end If need to get user defined filters.

      *!*    		IF TYPE("laOGFxFlt[1]") = "C"
      *!*    		  loogscroll.CopyFltArray(@laOGFxFlt,"laOGFxFlt")
      *!*    		ENDIF
      *!*    		IF TYPE("laOGVrFlt[1]") = "C"
      *!*    		  loogscroll.CopyFltArray(@laOGVrFlt,"laOGVrFlt")
      *!*    		ENDIF
      *!*
      *!*
      *!*        lcXmlStr = lfSaveTOxml()
      *!*        STRTOFILE(lcXmlStr,oAriaApplication.WorkDir+loFormSet.lcalselgrdfile+'.xml')
      *!*    	ENDIF
      lcExpr = gfOpGrid('ALAUTSLC',.T.,"R",.T.,.T.,.T.)
    ELSE
      lcExpr = gfOpGrid('ALAUTSLC',.T.,.F.,.F.,.T.,.T.)
    ENDIF

  ENDIF

  IF llLogic
    *  IF IIF(llSelect,!EMPTY(lcExpr) AND lcExpr <> '.F.' ,.T.)
    lfLogicGrid(llShowOpG)
    * ENDIF
  ENDIF
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
      USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG STYLE IN 0
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
  *! Name      : lfSumFab
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : sum a specific field for the current item in Fabric file.
  *!*************************************************************
  *! Passed Parameters : Sort Number (1,2,3, Or 4)
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
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

  DIMENSION laIncExprs[1,ALEN(laOGFxFlt,2)],;
    laExcExprs[1,ALEN(laOGFxFlt,2)]
  STORE '' TO laIncExprs,laExcExprs

  lnJ = 1
  FOR lnI = 1 TO ALEN(laOGFxFlt,1)
    lnJ = IIF(lnI = lnDummyPos,1,lnJ)
    IF !INLIST(lnI,lnDummyPos,lnDummyPos+1) AND !EMPTY(laOGFxFlt[lnI,6])
      IF (lnI < lnDummyPos) AND !EMPTY(laIncExprs[1,1])
        lnJ = lnJ + 1
        DIMENSION laIncExprs[lnJ,ALEN(laOGFxFlt,2)]
      ENDIF

      IF (lnI > lnDummyPos) AND !EMPTY(laExcExprs[1,1])
        lnJ = lnJ + 1
        DIMENSION laExcExprs[lnJ,ALEN(laOGFxFlt,2)]
      ENDIF

      lcSubArray = IIF(lnI < lnDummyPos,'laIncExprs','laExcExprs')
      FOR lnK = 1 TO ALEN(laOGFxFlt,2)
        &lcSubArray[lnJ,lnK] = laOGFxFlt[lnI,lnK]
      ENDFOR

    ENDIF
  ENDFOR

  lcXmlStr = loogScroll.convertvariablestoxml()

  STRTOFILE(lcXmlStr,oAriaApplication.WorkDir+loFormSet.lcalselgrdfile+'.xml')




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
  *****************************************************************************************************************
FUNCTION lfAddNode
  LPARAMETERS lcDataType, lcName, lvValue

  lvValue = lfCodeSpecialChar(lvValue)

  LOCAL lcNewLine
  lcNewLine = CHR(13) + CHR(10)

  LOCAL lcReturnXML
  lcReturnXML = '      <row>' + lcNewLine

  DO CASE
    CASE lcDataType = 'C' .OR. lcDataType = 'M'
      lcReturnXML = lcReturnXML + ;
        '        ' + '<DataType>System.String</DataType>' + lcNewLine + ;
        '        ' + '<Name>' + lcName + '</Name>' + lcNewLine + ;
        '        ' + '<Value>' + lvValue + '</Value>' + lcNewLine

    CASE lcDataType = 'N'
      lcReturnXML = lcReturnXML + ;
        '        ' + '<DataType>System.Decimal</DataType>' + lcNewLine + ;
        '        ' + '<Name>' + lcName + '</Name>' + lcNewLine + ;
        '        ' + '<Value>' + STR(lvValue) + '</Value>' + lcNewLine

    CASE lcDataType = 'L'
      lcReturnXML = lcReturnXML + ;
        '        ' + '<DataType>System.Boolean</DataType>' + lcNewLine + ;
        '        ' + '<Name>' + lcName + '</Name>' + lcNewLine + ;
        '        ' + '<Value>' + IIF(lvValue, 'true', 'false') + '</Value>' + lcNewLine

    CASE lcDataType = 'D'
      lcReturnXML = lcReturnXML + ;
        '        ' + '<DataType>System.Datetime</DataType>' + lcNewLine + ;
        '        ' + '<Name>' + lcName + '</Name>' + lcNewLine + ;
        '        ' + '<Value>' + DTOS(lvValue) + '</Value>' + lcNewLine

    CASE lcDataType = 'X'
      LOCAL lnSelected
      lnSelected = SELECT()



      SELECT(lcName)
      DIMENSION laCursorStruct[1,1]
      =AFIELDS(laCursorStruct)
      LOCAL loXMLParase, lcXML
      loXMLParase = NEWOBJECT("wwxml")
      *,lcXMLLibrary
      lcXML = loXMLParase.CURSORTOXML()

      lcXML = lfCodeSpecialChar(lcXML)

      lcReturnXML = lcReturnXML + ;
        '        ' + '<DataType>System.Table</DataType>' + lcNewLine

      IF !EMPTY(laCursorStruct[1,1])
        lcReturnXML = lcReturnXML + '        ' +'<CursorStrucutre>'
        FOR nCount = 1 TO ALEN(laCursorStruct,1)
          lcReturnXML = lcReturnXML + ;
            '        ' +'<Field>'+lcNewLine+;
            '        ' +'<Name>'+laCursorStruct[nCount,1]+'</Name>'+lcNewLine +;
            '        ' +'<Type>'+laCursorStruct[nCount,2]+'</Type>'+lcNewLine +;
            '        ' +'<Width>'+ALLTRIM(STR(laCursorStruct[nCount,3]))+'</Width>'+lcNewLine +;
            '        ' +'<Decimals>'+ALLTRIM(STR(laCursorStruct[nCount,4]))+'</Decimals>'+lcNewLine +;
            '        ' +'</Field>'
        ENDFOR
        lcReturnXML = lcReturnXML + ;
          '        ' +'</CursorStrucutre>'
      ENDIF




      lcReturnXML = lcReturnXML + ;
        '        ' + '<Name>' + lcName + '</Name>' + lcNewLine + ;
        '        ' + '<Value>' + lcXML + '</Value>' + lcNewLine

      SELECT(lnSelected)
  ENDCASE

  lcReturnXML = lcReturnXML + ;
    '      </row>' + lcNewLine

  RETURN lcReturnXML
  *************************************************************************************************************************

FUNCTION lfAddAdditionalNodeToXML
  RETURN ""

  ***********************************************************************************************************************

FUNCTION lfCodeSpecialChar
  LPARAMETERS lvValue

  IF TYPE('lvValue') <> 'C'
    RETURN lvValue
  ENDIF
  lvValue = STRTRAN(lvValue, '&', '&amp;')
  lvValue = STRTRAN(lvValue, '>', '&gt;')
  lvValue = STRTRAN(lvValue, '<', '&lt;')
  lvValue = STRTRAN(lvValue, '"', '&quot;')
  lvValue = STRTRAN(lvValue, "'", '&apos;')

  RETURN lvValue
  *************************************************************************************************************************

FUNCTION lfGetCrit
  lcXmlStr = loogScroll.convertvariablestoxml()
  STRTOFILE(lcXmlStr,oAriaApplication.WorkDir+ loFormSet.lcautplistfile+'.xml')

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
    laSortAry[lnSortItem,2] = loFormSet.laIndexExp[lnObjVal]
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
        lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P' AND SHIPNO IN (" +lcShpNO + ") AND " + lcShpDat
      CASE !EMPTY(lcShpNO) AND EMPTY(laOGFxFlt[lnShpDtElm ,6])
        lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P' AND SHIPNO IN (" +lcShpNO + ")"
      CASE EMPTY(lcShpNO) AND !EMPTY(laOGFxFlt[lnShpDtElm ,6])
        lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P' AND " + lcShpDat
      CASE EMPTY(lcShpNO) AND EMPTY(laOGFxFlt[lnShpDtElm ,6])
        lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P'"
    ENDCASE
  ENDIF

  lcXmlStr = loogScroll.convertvariablestoxml()
  STRTOFILE(lcXmlStr,oAriaApplication.WorkDir+loFormSet.lcallogicgrdfile+'.xml')


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
  IF !USED('OrdHdr')
    gfOpenTable('OrdHdr','OrdHdr')
  ENDIF
  =gfSeek('O'+lcOrderNo,'OrdHdr')
  lcRpPkFWrh = OrdHdr.Cwarecode

  IF llShowOpG
    loOGScroll.SetFilterExpr()
    RETURN .F.
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
      llRpForAlo = .F.
      llAlwForce = .F.  && Suppress line.
    ELSE  && User Prev.
      *-- Call user defined process.
      llAlwForce = gfUserPriv('AL','ALAUTAL','FORCING')
    ENDIF
  ENDIF
  RETURN llAlwForce
  *-- end of lfSuppForc.

  *************************************************************************************************************************

FUNCTION lfLogicGrid
  PARAMETERS llShowOpG
  IF llShowOpG
    lcExpr1 = gfOpGrid('ALAUTALC',.T.,"R",.T.,.T.,.T.)   && Allocation logic grid.
  ELSE
    lcExpr1 = gfOpGrid('ALAUTALC',.T.,.F.,.F.,.T.,.T.)  && Allocation logic grid.
  ENDIF

  ************************************************************************************************************************

FUNCTION lfCallPkTkGrd
  PARAMETERS llShowOpG

  lnRpGenNew = 2

  STORE .F. TO  llFromWz,llRpPkHPck

  llRpGenPik = .T.

  IF llShowOpG
    lcExpr2 = gfOpGrid('ALAUTPK', .T.,"R",.T.,.T.,.T.)   && Pick Ticket grid.
  ELSE
    lcExpr2 = gfOpGrid('ALAUTPK' , .T.,.F.,.F.,.T.,.T.)   && Pick Ticket grid.
  ENDIF

  ************************************************************************************************************************

FUNCTION lfGetCrt
  lcXmlStr = loogScroll.convertvariablestoxml()

  STRTOFILE(lcXmlStr,oAriaApplication.WorkDir+loFormSet.lcpkTkgrdfile +'.xml')

  *!*************************************************************
  *! Name      : lfvGenPktk
  *: Developer : HEND GHANEM (HBG)
  *: Date      : 11/12/2003
  *! Purpose   : Valid fuction for generate new Piktkt No
  *!*************************************************************
  *!
FUNCTION lfvGenPktk

  CLEARREAD()

  ************************************************************************************************************************

FUNCTION lfWhenPk

  llRpGenPik = .T.

  lnRpGenNew = 2

  IF llShowOpG
    loOGScroll.SetFilterExpr()
    RETURN .F.
  ENDIF

  ********************************************************************************************************************

FUNCTION lfCallOrdAll
  PARAMETERS lcorderno
  *DO FORM (oAriaApplication.ScreenHome+"AL\ALORDAL") WITH lcorderno
  lcKey = "'"+lcorderno+"'"
  oAriaApplication.DoProgram("AWRALORDAL",lcKey,.F.,'AL')

  ********************************************************************************************************************

FUNCTION lfGetRepVar
  LOCAL lcVariables
  lcVariables = ""
  loogscroll.cToSaveVariables = ""
  *-- If there are some report variables to be defined.
  SELECT SYREPUVR
  LOCATE FOR cExpType = "V"
  IF FOUND()
    DECLARE loogscroll.aRepVariables[1,3]
    STORE "" TO loogscroll.aRepVariables, loogscroll.cRepVariables

    LOCAL lnVariables
    STORE 0 TO lnVariables

    *-- Loop through all variables...
    SCAN REST WHILE cExpType = "V"
      lnVariables = lnVariables + 1
      DECLARE loogscroll.aRepVariables[lnVariables,3]
      loogscroll.aRepVariables[lnVariables,2] = loogscroll.MemoToString(mFld_Name)
      loogscroll.aRepVariables[lnVariables,3] = RECNO()
      loogscroll.cRepVariables = loogscroll.cRepVariables + "," + loogscroll.aRepVariables[lnVariables,2]

      *-- Collect the User to save variables
      IF UPPER(LEFT(loogscroll.aRepVariables[lnVariables,2],1)) = "L" AND;
          UPPER(SUBSTR(loogscroll.aRepVariables[lnVariables,2],3,2)) = "RP"
        loogscroll.cToSaveVariables = loogscroll.cToSaveVariables + "," + loogscroll.aRepVariables[lnVariables,2]
      ENDIF

      *-- Used in arranging the objects at the container.
      IF lAskRunT AND lDispOG
        loogscroll.aRepVariables[lnVariables,1] = PADL(ALLTRIM(STR(nVarPos)),6,"0") + cObj_Type
      ELSE
        loogscroll.aRepVariables[lnVariables,1] = "000000" + cObj_Type
      ENDIF
      *-- Dummy types should be of type Z (cObj_Type = "Z")

    ENDSCAN

    loogscroll.cRepVariables = SUBSTR(loogscroll.cRepVariables,2)  && Used to define variables.

    IF !EMPTY(loogscroll.cToSaveVariables)
      loogscroll.cToSaveVariables = SUBSTR(loogscroll.cToSaveVariables,2)
    ENDIF
  ENDIF
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



*:**************************************************************************
FUNCTION gfWizOptionGrid

PARAMETERS lcFileName, lcOGPrgName,llProgram,llCallWhenPacking,lcRunDirct, llNoShow, ;
llUndeclareVariables, llDonnotReadVariableAsProp, laUserFields, lcBaseFile,lcScrTitle, lcFieldsFile, lcShowFunction


*-- Validate llUndeclareVariables
llDonnotReadVariableAsProp = IIF(TYPE("llDonnotReadVariableAsProp") # 'L' .OR. ;
                             ISNULL(llDonnotReadVariableAsProp), ;
                             .F., ;
                             llDonnotReadVariableAsProp)

PRIVATE llDonnotReadVariableAsProp
* B037901,1 MAH End

*-- Validate llUndeclareVariables
llUndeclareVariables = IIF(TYPE("llUndeclareVariables") # 'L' .OR. ;
                           ISNULL(llUndeclareVariables), ;
                           .F., ;
                           llUndeclareVariables)
* B037902,1 MAH End

*-- Used to find out the global functions...
*-- Validate the only mandatory Parameter.
IF (VARTYPE(lcOGPrgName) != "C") OR EMPTY(lcOGPrgName)
  RETURN .F.
ENDIF
LOCAL lnOldDataSession
lnOldDataSession = SET("Datasession")

*-- Initialize all private variables ... BEGIN
PRIVATE lcOGPrtdir, lcRepTmpNm, llOGAddMode, llOGEditMode, llOGViewMode,lcOGRepID,;
        lnRepIDLen, lcOGManRep, llOGFilter, lcOGReadW, lcOGreadV, lcOGReadS,lnOGSeting,;
        lcOGWinTitl, lcOGOrder, lcOGOrderV, lcOGFormV, llOGBatchOk, llOGVrFlt,lcLogFile,;
        lcOGFlt_ID, lcOGTmpForm, lcOGLastForm, lcRepMode, lcRepAvlM, lcOGPlatForm,;
        lcOGSortID, lcRpExp, llOGFltCh, lcOGPrvRun, lcRpFrxMod, gcCmpModules,;
        laOGObjType, laOGObjCnt, llOGRefForm, llOGStyleCh,;
        llOGSysData, laRangeInfo, R_WIDTH, llFrxForm, lcOptProg,;
        gcContCode, llRpPrUSel

DECLARE laOGFxFlt[1,8], laOGHdFlt[1,8], laOGVrFlt[1,8], laOGSeting[1,2],;
        laOGObjType[1,3], laOGObjCnt[1], laRangeInfo[1,2]

STORE "" TO lcOGPrtdir, lcRepTmpNm, lcOGManRep,lcOGReadW, lcOGreadV,;
            lcOGReadS, laOGFxFlt, laOGHdFlt, laOGVrFlt,lcOGRepID,lcLogFile,;
            lcOGWinTitl, lcOGOrder, lcOGOrderV, lcOGFormV,;
            lcOGFlt_ID, lcOGTmpForm, lcOGLastForm, lcRepMode, lcRepAvlM,;
            lcOGPlatForm,lcOGSortID, lcRpExp, llOGFltCh, lcOGPrvRun, lcRpFrxMod,;
            laOGObjType, lcOGTmpForm, lcOptProg

R_WIDTH = "N"    && @X,Y SAY Report design width.

*! B608184,1 MMT 07/30/2007 Fix bug of Print Selection criteria when save filter[Start]
*llRpPrUSel  = .T.   && Print report criteria.
llRpPrUSel  = .F.   && Print report criteria.
*! B608184,1 MMT 07/30/2007 Fix bug of Print Selection criteria when save filter[END]

llFrxForm   = .F.
laOGObjCnt  = .F.            
llOGRefForm = .T.
STORE .T. TO llOGStyleCh,llOGSysData

*-- MAB - Create Aria27 global images... BEGIN
*-- IF any one face a new global used in reports
*-- it should be added to this list.
*-- this is to avoid changing report code.
WITH oAriaApplication
  gcCmpModules = .CompanyInstalledModules
  gcBaseCurr = .BaseCurrency
  gcCom_Name = .ActiveCompanyName
  gdSysDate  = .SystemDate
  gcContCode = .DefaultCountry                                          
ENDWITH 
*-- MAB - Create Aria27 global images... END


STORE .F. TO llOGAddMode, llOGEditMode, llOGViewMode,;
             llOGFilter, llOGBatchOk, llOGVrFlt

STORE 0 TO lnRepIDLen, lnOGSeting
*-- Initialize all private variables ... END


*MAH
*-- 037231,1 HFK 12/22/2003 To Set The G/L Account Mask  [Begin]
*STORE SPACE(0) TO lcAcMask    && Account mask
*AMH
*=lfSetGLMsk()
*-- 037231,1 HFK 12/22/2003 To Set The G/L Account Mask  [Begin] [End]

*-- Instantiate the OG objects ..... BEGIN
PRIVATE loOGScroll

LOCAL lcClassDir, lcScreenDir, oOptionGrid
lcClassDir  = ADDBS(oAriaApplication.ClassDir)
*--   lcScreenDir = ADDBS(ALLTRIM(oAriaApplication.ScreenHome)) + "SY\"
*--   DO FORM (lcScreenDir + "SyOPGrid") NAME oOptionGrid NOSHOW 
oOptionGrid = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
loOGScroll  = oOptionGrid.OptionGrid.oHost

*-- Initialize OG Scroll area.
*AMH
IF NOT loOGScroll.InitScroll(lcOGPrgName,llProgram,lcRunDirct)
  *! B128052,1 MAH 06/05/2005 [BEGIN]
  = gfAddPerfLog("OPTIONGRID", "OPEN", "ABNORMAL_END1",  "")
  *! B128052,1 MAH 06/05/2005 [END]

  oOptionGrid = .NULL.
  RETURN .F.
ENDIF
*-- Instantiate the OG objects ..... END

WITH loOGScroll
  *-- B037825,1 MAH Assing the data session before calling option grid.
  .lnOldDataSession = lnOldDataSession
  *-- MAH End
  gcSysHome  = .gcSysHome
  gcDataDir  = .gcDataDir
  gcWorkdir  = .gcWorkdir
  gcRepHome  = .gcRepHome
  gcAct_Appl = .gcAct_Appl
  gcAct_Comp = .gcAct_Comp
  gcScrHome  = .gcScrHome
  gcUser_ID  = .gcUser_ID
  lcOGTmpForm = .lcOGTmpForm
ENDWITH 

*-- All OG Variables should be shown to this program.
IF !EMPTY(loOGScroll.cRepVariables)
  LOCAL lcOGVariables
  lcOGVariables = ALLTRIM(loOGScroll.cRepVariables)  
  * B037902,1 MAH Undeclare variables if required
  IF !llUndeclareVariables
  * B037902,1 MAH End
    PRIVATE &lcOGVariables.
    STORE "" TO &lcOGVariables.    && Initial all report OG level variables.
  * B037902,1 MAH un declare variables if required
  ENDIF
  * B037902,1 MAH End
ENDIF 

*! N038218,1 SMM Define all Form Variables to be accessible all over the OG [START]  
IF !EMPTY(loOGScroll.cFormVariables)
  LOCAL lcOGFormVariables, lcFormVar
  lcOGFormVariables = ALLTRIM(loOGScroll.cFormVariables)  
  PRIVATE &lcOGFormVariables.
  STORE "" TO &lcOGFormVariables.    && Initial all report OG level variables.
  LOCAL lnItems
  FOR lnItems = 1 TO ALEN(loOGScroll.aFormVariables,1)   	
    IF !EMPTY(loOGScroll.aFormVariables[lnItems,2])   
  	  lcFormVar = loOGScroll.aFormVariables[lnItems,1] 
	    &lcFormVar. = loOGScroll.aFormVariables[lnItems,2]
      && Check data types of the variables
    ENDIF  
  ENDFOR
ENDIF 
*! N038218,1 SMM Define all Form Variables to be accessible all over the OG [END]  

*! E037249,1 SMM Call AddUDFFilters In case of user defined fields [START] 
*!*	loOGScroll.DefineObjects()           && Define OG Objects... 
IF TYPE('laUserFields') # 'C' 
  *AMH
  loOGScroll.DefineObjects()           && Define OG Objects... 
ELSE
   *040078,1 ASM 18/1/2005 Developing Company Informnation Screen in Aria4 [Start]
  IF TYPE('lcFieldsFile')<>'C'
    loOGScroll.GetUserFilters()
  ENDIF
  loOGScroll.mAddUDFFilters('FX',lcFieldsFile)
  IF !EMPTY(lcShowFunction)
    loOGScroll.lcOGReadS = lcShowFunction
    loOGScroll.DoShow()
  ENDIF
  *040078,1 ASM 18/1/2005 Developing Company Informnation Screen in Aria4 [End]

  loOGScroll.GetUserFilters
  loOGScroll.mAddUDFFilters('FX')
EndIF
*! E037249,1 SMM Call AddUDFFilters In case of user defined fields [END]

*-- Adjust OG Title and startup position.
oOptionGrid.OptionGrid.Caption = lcOGWinTitl
oOptionGrid.OptionGrid.AutoCenter = .T.

LOCAL lvReturnValue
*-- Show the OG
IF llNoShow
  lvReturnValue = oOptionGrid.OptionGrid   && Return the Option Grid form.

  *! B128052,1 MAH 06/05/2005 [BEGIN]
   = gfAddPerfLog("OPTIONGRID", "OPEN", "END2",  "")
   *! B128052,1 MAH 06/05/2005 [END]
ELSE
  IF loOGScroll.lShowOG    && Option Grid in the Show mode.
    *-- E037233,2 01/14/2004 Start Flag that option grid is running
    PUBLIC glOptionGridIsRuning
    glOptionGridIsRuning = .T.

   *! B128052,1 MAH 06/05/2005 [BEGIN]
   = gfAddPerfLog("OPTIONGRID", "OPEN", "END1",  "")
   *! B128052,1 MAH 06/05/2005 [END]

    *-- E037233,2 01/14/2004 End
    oOptionGrid.OptionGrid.Show(1)  && Show as a modal form.
    *-- E037233,2 01/14/2004 Start Flag that option grid is not running
    glOptionGridIsRuning = .F.
    *-- E037233,2 01/14/2004 End
    *=lfTempFunc()   
  ENDIF
  * B037905,1 MAH Return '.T.' in case of empty
  *-- lvReturnValue = IIF(EMPTY(lcRpExp) OR UPPER(ALLTRIM(lcRpExp))=".T.",.F.,lcRpExp)
  lvReturnValue = IIF(EMPTY(lcRpExp), ".T.", lcRpExp)
  * B037905,1 MAH End
ENDIF   

*Wizard

  IF llCallWhenPacking
    *lfwOGWhen()
      IF lnOgSeting = 1 AND FILE('MnlQty.MEM')
        RESTORE FROM MnlQty.MEM ADDITIVE
     ENDIF
   
  lnInd = ASUBSCRIPT(looGScroll.laOgFxFlt,ASCAN(looGScroll.laOgFxFlt,'PIKTKT.PIKTKT'),1)
  lcTempOrdFile = looGScroll.laOgFxFlt[lnInd, 6]
  CREATE CURSOR (lcTempOrdFile) (KeyExp C(6),piktkt C(6))
  SELECT (lcTempOrdFile)
  INDEX ON KeyExp TAG (lcTempOrdFile)

  SELECT PikTkt
  lcPKOrd = ORDER()
  SET ORDER TO ORDPIK
  gfSEEK(lcOrderNo,'PikTkt')
  SCAN REST WHILE ORDER+piktkt = lcOrderNo FOR Status ='O'
    INSERT INTO (lcTempOrdFile) (PikTkt,keyExp) VALUES (PikTkt.PikTkt,PikTkt.PikTkt)
  ENDSCAN

  SET ORDER TO (lcPKOrd) IN PikTkt

  laOgFxFlt[lnInd,6] = lcTempOrdFile

  ENDIF 

  loogScroll.SetFilterExpr()
  lcXmlStr = loogScroll.convertvariablestoxml()
  STRTOFILE(lcXmlStr, lcFileName)
*Wizard

  
* B037919,1 MAH Destroy option grid
loOGScroll  = .NULL.
oOptionGrid = .NULL.
* B037919,1 MAH End

ON KEY  && No Key assigned to.
SET DATASESSION TO lnOldDataSession
ON KEY  && No Key assigned to.
RETURN lvReturnValue  && Return value.
