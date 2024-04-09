*:----------------------------------------------------------------------------------------------
*: Program file        : SOMASCA.PRG
*: Program description : Mass Order Cancellation - Order Shaving
*: For System          : Aria Advantage Series - Version 4XP
*: Developer Name      : MAriam Mazhar(MMT)
*: Job Number 		     : T20060908.0005 (10/04/2006)
*:----------------------------------------------------------------------------------------------
*:Modifications:   
*:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation        [T20080501.0005]
*:B608615,1 MMT 07/14/2008 Fix bug of Error in case of releasing allocted orders [T20080602.0001]
*:B609260,1 MMT 05/19/2010 PO-Mass PO Cancellation does not cancel PO's [T20100226.0001]
*:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[T20100226.0001]
*:B609371,1 SMA 08/05/2010 Fix bug of SO-Mass SO Cancellation does not update styhist [T20100720.0003]
*:B611539,1 SAH 03/07/2018 PO status is not Hold 'H' before adding lines in ORDCANLN and before calling function lfAdCnLine[T20180228.0009]
*:B612470,1 MMT 09/15/2021 The Sales order totals are updated incorrectly when some lines are cancelled from Mass Order Cancellation screen[T20210902.0001]
*:----------------------------------------------------------------------------------------------
#INCLUDE R:\Aria4XP\Screens\SOMASCA.h 
DO FORM (oAriaApplication.ScreenHome+'\SOMASCA.SCX')

*!*************************************************************
*! Name      : lfvScope
*: Developer : MAriam Mazhar(MMT)
*! Date      : 10/04/2006
*! Purpose   : Valid function of push button Scope
*!*************************************************************
*! Called from : Control Panel , lpShow
*!*************************************************************
*! Calls       : gfOpGrid() , lfCrT() , lfDispBrow()
*!*************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
FUNCTION lfvScope
  PARAMETERS lcTmpFil,loFormSet
  STORE .T. TO llOGStart , llUsrCPrv , llUsrSPrv , llUsrPPrv
  llUsrCPrv = gfUserPriv('MF','MFCUTKT')
  llUsrSPrv = gfUserPriv('SO','SOORD')
  llUsrPPrv = gfUserPriv('PO','POSTY')
  DIMENSION laSortDesc[2,1] , laSortVal[2,1]
  lcStyTitle  = gfItemMask('HI')
  lcStylePic  = "@! " + gfItemMask('PM')
  lcStyMajor  = gfItemMask('HM')
  lnMajorLen  = LEN(lcStylePic)
  lcColorTlt  = ''
  lcNonMajTlt = ''
  lcNonMajPi  = ''
  lcFree_Clr  = ''
  lnNonMajSt  = 0
  lnFreeLen   = 0
  lnColorLen  = 0
  lnMajSeg    = gfItemMask('SM')  && No. of major segments.

  *-- Compute Free/Color Items in Style Structure. [Begin]
  DIMENSION laMajSegs[1,1]
  = gfItemMask(@laMajSegs)

  *-- if you does not find Non Major Type Color Code.
  IF !lfNMajType('C',lnMajSeg)
    = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
  ENDIF  && end if you didn't find Non Major Type Color Code.

  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  *N000682,1 MMT 04/19/2012 globalization Changes [Start]
  *lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
  lcColorTlt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OG_THERSE,loFormSet.GetHeaderText("LANG_OG_THERSE",loFormSet.HeaderAlias))+' ' +;
              ALLTRIM(lcNonMajTlt) +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OG_COLORS_S,loFormSet.GetHeaderText("LANG_OG_COLORS_S",loFormSet.HeaderAlias)) 
  *N000682,1 MMT 04/19/2012 globalization Changes [End]
  llSuppress = .F.
  lcRpCanTyp = 'O'
  *loFormSet.lcRpCanTyp = lcRpCanTyp
  lcRpCrt = ''
  lnShavePer = 0
  lcActAlias = ''
  lcDataSessI = SET("Datasession" )&&THIS.loFormSet.DatasessionID
  DIMENSION laSortType[1,2] , laTypeVal[1,2]
  DECLARE laScopExpr[1]
  IF !USED(lcTmpFil)
    lfCrtTmp()
    lfAddControlSource(loFormSet)
  ENDIF
  lcExpr = gfOpGrid('SOMASCAN',.T.,.F.,.F.,.T.,.T.)

  SET DATASESSION TO lcDataSessI
  IF lcExpr <> ".F."
    =lfOpenFile()
    loFormSet.ARIAform1.GRDOrders.GRDMultiSelectionGrid.RECORDSOURCE = ''
    =lfRemTmp()
    =lfCrtTmp()
    =lfFillField()
    SELECT (lcTmpFil)
    REPLACE ALL LLSEL WITH .F.
    *mmt
    SET FILTER TO TOTQTY > 0
    *mmt
    *:B609260,1 MMT 05/19/2010 PO-Mass PO Cancellation does not cancel PO's [Start]
    *SET ORDER TO (lcTmpFil) DESCENDING
    *:B609260,1 MMT 05/19/2010 PO-Mass PO Cancellation does not cancel PO's [End]
    LOCATE
    lfAddControlSource(loFormSet)
    loFormSet.lcRpCanTyp = lcRpCanTyp
    loFormSet.lcRpCrt = lcRpCrt

  ENDIF

  WITH loFormSet
    SELECT (lcTmpFil)
    LOCATE
    IF EOF()
      .llInvert = .F.
      .llSelect = .F.
      .llselectall = .F.
      .llSelectnone = .F.
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *WAIT WINDOW LANG_No_Rec
      WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Rec,loFormSet.GetHeaderText("LANG_No_Rec",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

    ELSE
      .llInvert = .T.
      .llSelect = .T.
      .llselectall = .T.
      .llSelectnone = .F.
    ENDIF
    WITH .ARIAform1
      IF loFormSet.lcRpCanTyp  $ 'SO'
        .cboReason.VISIBLE = .T.
        .lblReason.VISIBLE = .T.
        .ariashape1.VISIBLE = .T.
        *N000682,1 MMT 03/22/2013 Fix phase#3 issues[Start]
        IF oAriaApplication.oActivelang.cLang_ID = "EN"
        *N000682,1 MMT 03/22/2013 Fix phase#3 issues[End]
        .ariashape1.TOP = 3
        .lblReason.TOP = 12
        .cboReason.TOP = 9
        .GRDOrders.TOP = 42
        *N000682,1 MMT 03/22/2013 Fix phase#3 issues[Start]
        ENDIF
        *N000682,1 MMT 03/22/2013 Fix phase#3 issues[End]
      ELSE
        .cboReason.VISIBLE = .F.
        .lblReason.VISIBLE = .F.
        .ariashape1.VISIBLE = .F.
        .GRDOrders.TOP = .ariashape1.TOP

      ENDIF
      .oResizer.RefreshSavedCoordinates(.ariashape1)
      .oResizer.RefreshSavedCoordinates(.GRDOrders)

    ENDWITH
    .llenbalecancel = .F.
    .ARIAform1.GRDOrders.REFRESH()
  ENDWITH
  *!*************************************************************
  *! Name      : lfSortDumy
  *: Developer : MAriam Mazhar(MMT)
  *! Date      : 10/04/2006
  *! Purpose   : Fill Criteria Field In Option Grid
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfSortDumy()
  *!*************************************************************
FUNCTION lfSortDumy

  DO CASE
    CASE lcRpCanTyp = 'O' .OR. lcRpCanTyp = 'S'
      llSuppress = .F.
      DIMENSION laSortDesc[2,1] , laSortVal[2,1]
	  *N000682,1 MMT 04/03/2013 Fix Media issues[Start]
*!*	      laSortDesc[1] = 'Order'
*!*	      laSortDesc[2] = 'Contract'
      laSortDesc[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SORT_ORDER,loFormSet.GetHeaderText("LANG_SORT_ORDER",loFormSet.HeaderAlias))
      laSortDesc[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SORT_CONTRACT,loFormSet.GetHeaderText("LANG_SORT_CONTRACT",loFormSet.HeaderAlias))
  	*N000682,1 MMT 04/03/2013 Fix Media issues[End]
      laSortVal[1] = 'O'
      laSortVal[2] = 'C'


    CASE lcRpCanTyp $ 'PN'

      llSuppress = .F.
      DIMENSION laSortDesc[2,1] , laSortVal[2,1]
  	*N000682,1 MMT 04/03/2013 Fix Media issues[Start]
*!*	      laSortDesc[1] = 'Order'
*!*	      laSortDesc[2] = 'Contract'
      laSortDesc[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SORT_ORDER,loFormSet.GetHeaderText("LANG_SORT_ORDER",loFormSet.HeaderAlias))
      laSortDesc[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SORT_CONTRACT,loFormSet.GetHeaderText("LANG_SORT_CONTRACT",loFormSet.HeaderAlias))
      *N000682,1 MMT 04/03/2013 Fix Media issues[End]
      laSortVal[1] = 'O'
      laSortVal[2] = 'C'

    CASE lcRpCanTyp = 'C'

      llSuppress = .F.
      llSuppress = .T.

  ENDCASE
  *--The End
  *!*****************************************************************
  *! Function: lfAdCnLine
  *!*****************************************************************
  *! Author  : Walid A. Wahab (WAB)
  *!*****************************************************************
  *! Purpose : add cancel qty for the completed po
  *!*****************************************************************
  *! Date    : 04/07/2003
  *!*****************************************************************
  *!B606743,1
  *!*****************************************************************
FUNCTION lfAdCnLine

  SELECT (lcTmpFil)
  lnRcN = RECNO()
  *SCAN FOR TRANCD = '1' AND llSel
  IF  TRANCD = '1' AND LLSEL
    lcSeekExp  = IIF(loFormSet.lcRpCanTyp='C','P'+'U'+PO,loFormSet.lcRpCrt+loFormSet.lcRpCrt+PO)
    IF SEEK(lcSeekExp ,'POSHDR') &&AND POSHDR.STATUS = 'C'
      SCATTER MEMVAR MEMO
      m.TRANCD = IIF(loFormSet.lcRpCanTyp='C','4','5')
      m.date = oAriaApplication.SystemDate
      SELECT PosLn
      APPEND BLANK
      GATHER MEMVAR MEMO
      gfAppend(.F., .T.)
      SELECT (lcTmpFil)
    ENDIF
  ENDIF
  *ENDSCAN
  GO lnRcN IN (lcTmpFil)
  *!*****************************************************************
  *! Function: lfSortType
  *!*****************************************************************
  *! Author : Hossam El Etreby (HDM)
  *!*****************************************************************
  *! Purpose : Check for installed modules to fill transaction type POPUP
  *!           in the option grid
  *!*****************************************************************
  *! DUE TO Bug# 802166
  *!*****************************************************************

FUNCTION lfSortType
  PRIVATE lnLastDim

  DIMENSION laSortType[1,2] , laTypeVal[1,2]

  IF OCCURS('SO',oAriaApplication.CompanyInstalledModules) > 0 .AND. llUsrSPrv
    *N000682,1 MMT 04/03/2013 Fix Media issues[Start]
    *laSortType[1,1] = 'Sales Order'
    laSortType[1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TYPE_SO,loFormSet.GetHeaderText("LANG_TYPE_SO",loFormSet.HeaderAlias))
    *N000682,1 MMT 04/03/2013 Fix Media issues[End]
    laSortType[1,2] = 'SO'
    laTypeVal[1,1]  = 'O'
    laTypeVal[1,2]  = .F.
    DIMENSION laSortType[2,2] , laTypeVal[2,2]
    *N000682,1 MMT 04/03/2013 Fix Media issues[Start]
    *laSortType[2,1] = 'Order Shaving'    
    laSortType[2,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TYPE_ORDERSHAVE,loFormSet.GetHeaderText("LANG_TYPE_ORDERSHAVE",loFormSet.HeaderAlias))
    *N000682,1 MMT 04/03/2013 Fix Media issues[End]
    laSortType[2,2] = 'SO'
    laTypeVal[2,1]  = 'S'
    laTypeVal[2,2]  = .F.

  ENDIF

  IF !EMPTY(laSortType[1])
    IF OCCURS('PO',oAriaApplication.CompanyInstalledModules) > 0 .AND. llUsrPPrv
      DIMENSION laSortType[ALEN(laSortType,1)+1,2] , laTypeVal[ALEN(laSortType,1)+1,2]
      lnLastDim = ALEN(laSortType,1)
      DIMENSION laSortType[ALEN(laSortType,1),2] , laTypeVal[ALEN(laSortType,1),2]
      *N000682,1 MMT 04/03/2013 Fix Media issues[Start]
      *laSortType[lnLastDim,1] = 'Purchase Order'      
      laSortType[lnLastDim,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TYPE_PO,loFormSet.GetHeaderText("LANG_TYPE_PO",loFormSet.HeaderAlias))
      *N000682,1 MMT 04/03/2013 Fix Media issues[END]
      laSortType[lnLastDim,2] = 'PO'
      laTypeVal[lnLastDim,1]  = 'P'
      laTypeVal[lnLastDim,2]  = .F.

      DIMENSION laSortType[lnLastDim+1,2] , laTypeVal[lnLastDim+1,2]
      *N000682,1 MMT 04/03/2013 Fix Media issues[Start]
      *laSortType[lnLastDim+1,1] = 'Inter-Location Purchase Order'      
      laSortType[lnLastDim+1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TYPE_INTPO,loFormSet.GetHeaderText("LANG_TYPE_INTPO",loFormSet.HeaderAlias))
      *N000682,1 MMT 04/03/2013 Fix Media issues[End]
      laSortType[lnLastDim+1,2] = 'PO'
      laTypeVal[lnLastDim+1,1]  = 'N'
      laTypeVal[lnLastDim+1,2]  = .F.

    ENDIF
  ELSE
    IF OCCURS('PO',oAriaApplication.CompanyInstalledModules) > 0 .AND. llUsrPPrv
      *N000682,1 MMT 04/03/2013 Fix Media issues[Start]
      *laSortType[1,1] = 'Purchase Order'
      laSortType[1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TYPE_PO,loFormSet.GetHeaderText("LANG_TYPE_PO",loFormSet.HeaderAlias))
      *N000682,1 MMT 04/03/2013 Fix Media issues[End]      
      laSortType[1,2] = 'PO'
      laTypeVal[1,1]  = 'P'
      laTypeVal[1,2]  = .F.

      DIMENSION laSortType[2,2] , laTypeVal[2,2]
      *N000682,1 MMT 04/03/2013 Fix Media issues[Start]
      *laSortType[2,1] = 'Inter-Location Purchase Order'      
      laSortType[2,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TYPE_INTPO,loFormSet.GetHeaderText("LANG_TYPE_INTPO",loFormSet.HeaderAlias))
      *N000682,1 MMT 04/03/2013 Fix Media issues[Start]
      laSortType[2,2] = 'PO'
      laTypeVal[2,1]  = 'N'
      laTypeVal[2,2]  = .F.

    ENDIF
  ENDIF


  IF !EMPTY(laSortType[1])
    IF OCCURS('MF',oAriaApplication.CompanyInstalledModules) > 0 .AND. llUsrCPrv

      DIMENSION laSortType[ALEN(laSortType,1)+1,2] , laTypeVal[ALEN(laSortType,1)+1,2]
      lnLastDim = ALEN(laSortType,1)
      DIMENSION laSortType[ALEN(laSortType,1),2] , laTypeVal[ALEN(laSortType,1),2]
      *N000682,1 MMT 04/03/2013 Fix Media issues[Start]
      *laSortType[lnLastDim,1] = 'Cutting Ticket'
      laSortType[lnLastDim,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TYPE_CUTTKT,loFormSet.GetHeaderText("LANG_TYPE_CUTTKT",loFormSet.HeaderAlias))
      *N000682,1 MMT 04/03/2013 Fix Media issues[End]
      laSortType[lnLastDim,2] = 'MF'
      laTypeVal[lnLastDim,1]  = 'C'
      laTypeVal[lnLastDim,2]  = .F.
    ENDIF
  ELSE

    IF OCCURS('MF',oAriaApplication.CompanyInstalledModules) > 0 .AND. llUsrCPrv
      *N000682,1 MMT 04/03/2013 Fix Media issues[Start]   
      *laSortType[1,1] = 'Cutting Ticket'
      laSortType[1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TYPE_CUTTKT,loFormSet.GetHeaderText("LANG_TYPE_CUTTKT",loFormSet.HeaderAlias))
      *N000682,1 MMT 04/03/2013 Fix Media issues[End]
      laSortType[1,2] = 'MF'
      laTypeVal[1,1]  = 'C'
      laTypeVal[1,2]  = .F.
    ENDIF
  ENDIF


  DIMENSION laDummy[ALEN(laSortType,1),2]
  DIMENSION laDummyVal[ALEN(laTypeVal,1),2]


  lnActPos = ASUBSCRIPT(laSortType,ASCAN(laSortType,oAriaApplication.ActiveModuleID),1)
  lnActEl  = ASCAN(laSortType,oAriaApplication.ActiveModuleID) -1
  *-- Copy array elements starting from the active module element
  = ACOPY(laSortType , laDummy    , lnActEl)
  = ACOPY(laTypeVal  , laDummyVal , lnActEl)
  *-- get the no. of copied rows to the dummy array
  lnCopied = (ALEN(laSortType,1)+1) - lnActPos

  *-- get the no. of remaining rows to be copied
  lnRemain = ALEN(laSortType,1) - lnCopied

  *-- Loop to copy the rest
  FOR lnI = 1 TO lnRemain
    lnRowNum = lnI + lnCopied
    laDummy[lnRowNum,1]  = laSortType[lnActPos - lnI,1]
    laDummy[lnRowNum,2]  = laSortType[lnActPos - lnI,2]
    laDummyVal[lnRowNum,1] = laTypeVal[lnActPos - lnI,1]
    laDummyVal[lnRowNum,2] = laTypeVal[lnActPos - lnI,2]
  ENDFOR

  = ACOPY(laDummy , laSortType)
  = ACOPY(laDummyVal , laTypeVal)

  = lfSortDumy ()



  *!*************************************************************
  *! Name        : lfWhenRep
  *: Developer : MAriam Mazhar(MMT)
  *! Date      : 10/04/2006
  *! Purpose     : Option Grid When Function
  *!*************************************************************
  *! Called from :
  *!*************************************************************
  *! Calls       :
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
FUNCTION lfWhenRep
  lfSortType()
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,"lcRpCanTyp"),1)
  IF lnPosition > 0
    lcRpCanTyp = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  ENDIF

  =lfvCrt()
  =LOOGSCROLL.RefreshScroll()
  *!*************************************************************
  *!*****************************************************************
  *! Function: lfvCrt
  *!*****************************************************************
  *! Author : Hossam El Etreby (HDM)
  *!*****************************************************************
  *! Purpose : NEW FUNCTION TO HANDLE THE lcRpCRT variable.
  *!*****************************************************************
FUNCTION lfvCrt

  IF lcRpCanTyp = 'O' OR lcRpCanTyp = 'S'
    lcRpCrt = 'O'
  ELSE
    IF lcRpCanTyp = 'P'
      lcRpCrt = 'O'
    ENDIF

    IF lcRpCanTyp = 'N'
      lcRpCrt = 'N'
    ENDIF

  ENDIF

  *!*****************************************************************
  *!*************************************************************
  *! Name      : lfNMajType
  *: Developer : MAriam Mazhar(MMT)
  *! Date      : 10/04/2006
  *! Purpose   : Function For calculating major and non-major segments
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : lcFree_Clr IF Not Empty
  *!*************************************************************

FUNCTION lfNMajType
  PARAMETERS lcNMajType,lnMajSegs

  *-- Loop Around Non Major elements.
  FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)

    IF laMajSegs[lnI,1] = lcNMajType

      lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
      lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)

      lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
        lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

      lcNonMajTlt = IIF(EMPTY(lcNonMajTlt),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
        lcNonMajTlt + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))

    ENDIF

    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
      EXIT
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

  ENDFOR    && end Loop Around Non Major elements.

  RETURN !EMPTY(lcFree_Clr)
  *-- end of lfNMajType.
  *!*************************************************************
  *! Name      : lfvCanTyp
  *: Developer : MAriam Mazhar(MMT)
  *! Date      : 10/04/2006
  *! Purpose   : Validation Function For Cancellation Type Pop Up
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfvCanTyp()
  *!*************************************************************
FUNCTION lfvCanTyp

  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,"lcRpCanTyp"),1)
  IF lnPosition > 0
    lcRpCanTyp = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  ENDIF

  lfSortDumy()
  =lfvCrt()
  CLEARREAD()

  *!*************************************************************
  *! Name      : lfCreatExp
  *! Developer : Mariam Mazhar Tawfik [MMT]
  *! Date      : 10/05/2006
  *! Purpose   : function to save the user selection criteria
  *!*************************************************************
FUNCTION lfCreatExp
  *--Copy the fixed filter array to a public array in order to be
  *--available after closing the optiion grid
  =ACOPY(LOOGSCROLL.laOGFxFlt , laScopExpr)

ENDFUNC
*!**************************************************************************
*! Name      : lfOpenFile
*: Developer : MAriam Mazhar(MMT)
*! Date      : 10/04/2006
*! Purpose   : Function To Open the needed Files
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfOpenFile


  IF lcRpCanTyp $ 'POSN'

    IF !USED('POSHDR') .AND. 'PO' $ oAriaApplication.CompanyInstalledModules
      = gfOpenTable(oAriaApplication.DataDir + 'POSHDR' ,'POSHDR','SH')
    ENDIF

    IF !USED('POSLN') .AND. 'PO' $ oAriaApplication.CompanyInstalledModules
      = gfOpenTable(oAriaApplication.DataDir + 'POSLN' ,'POSLN','SH')
    ENDIF

  ENDIF
  *--Purchse order files

  *--Cutting Ticket files
  IF lcRpCanTyp $ 'COS'
    *--HDM B602512,1 Open cutting Ticket file with the needed Order[Start]
    IF !USED('POSHDR') .AND. 'MF' $ oAriaApplication.CompanyInstalledModules
      = gfOpenTable(oAriaApplication.DataDir + 'POSHDR' ,'POSHDR','SH')
    ENDIF

    IF !USED('POSLN') .AND. 'MF' $ oAriaApplication.CompanyInstalledModules
      = gfOpenTable(oAriaApplication.DataDir + 'POSLN' ,'POSLN','SH')
    ENDIF

  ENDIF
  *--Cutting Ticket files

  *--Sales Order Files
  IF !USED('ORDHDR')
    = gfOpenTable(oAriaApplication.DataDir + 'ORDHDR' ,'ORDHDR','SH')
  ENDIF

  IF !USED('ORDLINE')
    = gfOpenTable(oAriaApplication.DataDir + 'ORDLINE' ,'ORDLINE','SH')
  ENDIF
  *--Sales Order Files

  *--Style Files
  IF !USED('STYLE')
    = gfOpenTable(oAriaApplication.DataDir + 'STYLE' ,'STYLE','SH')
  ENDIF

  IF !USED('STYDYE')
    = gfOpenTable(oAriaApplication.DataDir + 'STYDYE' ,'STYDYE','SH')
  ENDIF
  *--Style Files


  IF !USED('ORDCANLN')
    = gfOpenTable(oAriaApplication.DataDir + 'ORDCANLN' ,'ORDCANLN','SH')
  ENDIF

  *!**************************************************************************
  *! Name      : lfRemTmp
  *: Developer : MAriam Mazhar(MMT)
  *! Date      : 10/04/2006
  *! Purpose   : Function To Delete the tepm file
  *!***************************************************************************
  *! Passed Parameters : None
  *!***************************************************************************
  *! Return      : None
  *!***************************************************************************

FUNCTION lfRemTmp
  IF USED(lcTmpFil) AND RECCOUNT(lcTmpFil) > 0&& We are going to close the Temp Pick ticket & Erase it.
    SELECT(lcTmpFil)
    USE IN (lcTmpFil)
  ENDIF   && End of IF
  *!*************************************************************
  *! Name      : lfCrtTmp
  *: Developer : MAriam Mazhar(MMT)
  *! Date      : 10/04/2006
  *! Purpose   : Function To Create the temp file according to Cancellation Type
  *!*************************************************************
  *! Passed Parameters : None
  *!***************************************************************************
  *! Return      : None
  *!***************************************************************************

FUNCTION lfCrtTmp
  PRIVATE lcPrevAl
  lcPrevAl = SELECT(0)

  DO CASE
    CASE lcRpCanTyp = 'O' .OR. lcRpCanTyp = 'S' && Order and Order Shaving
      IF !USED('ORDLINE')
        = gfOpenTable(oAriaApplication.DataDir + 'ORDLINE' ,'ORDLINE','SH')
      ENDIF
      SELECT ORDLINE

    CASE lcRpCanTyp $ 'CPN'                       && Purchase Order

      IF !USED('POSLN') .AND. ('PO' $ oAriaApplication.CompanyInstalledModules OR 'MF' $ oAriaApplication.CompanyInstalledModules)
        = gfOpenTable(oAriaApplication.DataDir + 'POSLN' ,'POSLN','SH')
      ENDIF
      SELECT PosLn

  ENDCASE

  = AFIELDS(laFileStru)

  *-- Add Some Fields To The Structure
  lnFileStru = ALEN(laFileStru,1)

  DIMENSION laFileStru[lnFileStru+3,18]

  laFileStru[lnFileStru+1,1] = 'LLSEL'
  laFileStru[lnFileStru+1,2] = 'L'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'NORDLNNO'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 9
  laFileStru[lnFileStru+2,4] = 0

  laFileStru[lnFileStru+3,1] = 'NPROCNO'
  laFileStru[lnFileStru+3,2] = 'N'
  laFileStru[lnFileStru+3,3] = 2
  laFileStru[lnFileStru+3,4] = 0

  FOR lnLoop = 1  TO  3
    STORE ' ' TO laFileStru[lnFileStru +lnLoop,7] ,laFileStru[lnFileStru +lnLoop,8],;
      laFileStru[lnFileStru  +lnLoop,9]  ,laFileStru[lnFileStru +lnLoop,10],;
      laFileStru[lnFileStru +lnLoop,11]  ,laFileStru[lnFileStru +lnLoop,12],;
      laFileStru[lnFileStru  +lnLoop,13] ,laFileStru[lnFileStru +lnLoop,14],;
      laFileStru[lnFileStru  +lnLoop,15] ,laFileStru[lnFileStru +lnLoop,16]
    STORE 0 TO  laFileStru[lnFileStru +lnLoop,17],laFileStru[lnFileStru +lnLoop,18]

  ENDFOR

  lcActAlias = SELECT(0)

  =gfCrtTmp(lcTmpFil,@laFileStru,'LLSEL',lcTmpFil)

  IF lcRpCanTyp = 'O'
    SELECT(lcTmpFil)
    INDEX ON CORDTYPE+ORDER TAG 'ORDER'
  ENDIF
  *!**************************************************************************
  *! Name      : lfFillField
  *: Developer : MAriam Mazhar(MMT)
  *! Date      : 10/04/2006
  *! Purpose   : Function To Fill The Temp Table according to user selection
  *!***************************************************************************
FUNCTION lfFillField


  *--Shared options appear in all cases
  lnMajLength =LEN(gfItemMask('PM'))
  llStyleSelect = .F.
  lcStyleFile   = ''

  lnStyPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"STYLE.CSTYMAJOR"),1)
  IF lnStyPos > 0
    lcStyleFile = laScopExpr[lnStyPos,6]
    IF !EMPTY(lcStyleFile) AND USED(lcStyleFile)
      SELECT (lcStyleFile)
      LOCATE
      IF !EOF()
        llStyleSelect = .T.
      ENDIF
    ENDIF
  ENDIF

  llFabSelect = .F.
  lcFabFile   = ''

  lnFabPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"STYLE.FABRIC"),1)
  IF lnFabPos > 0
    lcFabFile    = laScopExpr[lnFabPos,6]
    IF !EMPTY(lcFabFile) AND USED(lcFabFile)
      SELECT (lcFabFile)
      LOCATE
      IF !EOF()
        llFabSelect = .T.
      ENDIF
    ENDIF
  ENDIF


  llGroupSelect = .F.
  lcGroupFile   = ''
  lnGrpPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"STYLE.CSTYGROUP"),1)
  IF lnGrpPos> 0
    lcGroupStr   = laScopExpr[lnGrpPos,6]
    IF !EMPTY(lcGroupStr)
      lcGroupFile = gfTempName()
      llGroupSelect = IIF(LEN(lcGroupStr)>0,.T.,.F.) AND lfConvertToCursor(lcGroupStr,'CSTYGRP',lcGroupFile)
    ENDIF
  ENDIF

  IF lcFree_Clr = 'C'
    llClrCSelect = .F.
    lcClrCFile   = ''
    lnClrCPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)"),1)
    IF lnClrCPos> 0
      lcClrCStr= laScopExpr[lnClrCPos,6]
      IF !EMPTY(lcClrCStr)
        lcClrCFile = gfTempName()
        llClrCSelect = IIF(LEN(lcClrCStr)>0,.T.,.F.) AND lfConvertToCursor(lcClrCStr,'COLOR',lcClrCFile)
      ENDIF
    ENDIF
  ENDIF

  IF lcFree_Clr = 'F'
    llClr2CSelect = .F.
    lcClr2CFile   = ''
    lnClr2CPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen)"),1)
    IF lnClr2CPos> 0
      lcClr2CStr= laScopExpr[lnClr2CPos,6]
      IF !EMPTY(lcClr2CStr)
        lcClr2CFile = gfTempName()
        llClr2CSelect = IIF(LEN(lcClr2CStr)>0,.T.,.F.) AND lfConvertToCursor(lcClr2CStr,'COLOR',lcClr2CFile)
      ENDIF
    ENDIF
  ENDIF

  DO CASE
    CASE lcRpCanTyp $ 'SO'&& Sales Order
      **Order
      llOrderSelect = .F.
      lcOrderFile   = ''
      lnOrdPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"ORDHDR.ORDER"),1)
      IF lnOrdPos > 0
        lcOrderFile = laScopExpr[lnOrdPos,6]
        IF !EMPTY(lcOrderFile) AND USED(lcOrderFile)
          SELECT (lcOrderFile)
          LOCATE
          IF !EOF()
            llOrderSelect = .T.
          ENDIF
        ENDIF
      ENDIF

      ***Account
      llAccSelect = .F.
      lcAccFile   = ''
      lnAccPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"ORDLINE.ACCOUNT"),1)
      IF lnAccPos > 0
        lcAccFile = laScopExpr[lnAccPos,6]
        IF !EMPTY(lcAccFile) AND USED(lcAccFile)
          SELECT (lcAccFile)
          LOCATE
          IF !EOF()
            llAccSelect = .T.
          ENDIF
        ENDIF
      ENDIF

      *--Priority
      lcPriority = ''
      lnPriPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"ORDHDR.PRIORITY"),1)
      IF lnPriPos  > 0
        lcPriority = laScopExpr[lnPriPos,6]
      ENDIF

      *--Complete Date
      ldStart = {}
      ldEnd = {}
      lnCompPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"ORDHDR.COMPLETE"),1)
      IF lnCompPos  > 0 AND !EMPTY(laScopExpr[lnCompPos,6])
        ldStart = IIF(EMPTY(SUBSTR(laScopExpr[lnCompPos,6],1,10)),DTOC(CTOD("")),SUBSTR(laScopExpr[lnCompPos,6],1,10))
        ldEnd   = IIF(EMPTY(SUBSTR(laScopExpr[lnCompPos,6],12,21)),DTOC(CTOD("")),SUBSTR(laScopExpr[lnCompPos,6],12,21))
      ENDIF

      IF llOrderSelect
        SELECT (lcOrderFile)
        SCAN
          SELECT ('Ordline')
          gfSetorder('Ordline')
          IF gfSeek(lcRpCrt+&lcOrderFile..ORDER,'Ordline','Ordline')
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = lcRpCrt+&lcOrderFile..ORDER ;
                FOR gfSeek(lcRpCrt+&lcOrderFile..ORDER,'Ordhdr','Ordhdr') AND ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0;
                AND IIF(llAccSelect,SEEK(ORDLINE.ACCOUNT,lcAccFile),.T.);
                AND IIF(!EMPTY(lcPriority),ORDHDR.PRIORITY = lcPriority,.T.) AND ;
                IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(ORDHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.);
                AND gfSeek(ORDLINE.STYLE,'Style','Style');
                AND IIF(llStyleSelect,SEEK(STYLE.CSTYMAJOR,lcStyleFile),.T.) ;
                AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)
              SCATTER MEMVAR MEMO
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
              *WAIT WINDOW LANG_Collect + M.ORDER NOWAIT
              WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias)) + M.ORDER NOWAIT
              *N000682,1 11/20/2012 MMT Globlization changes[End]

              IF !EMPTY(ORDLINE.PIKTKT)
                m.QTY1   = M.QTY1   - ORDLINE.PIK1
                m.QTY2   = M.QTY2   - ORDLINE.PIK2
                m.QTY3   = M.QTY3   - ORDLINE.PIK3
                m.QTY4   = M.QTY4   - ORDLINE.PIK4
                m.QTY5   = M.QTY5   - ORDLINE.PIK5
                m.QTY6   = M.QTY6   - ORDLINE.PIK6
                m.QTY7   = M.QTY7   - ORDLINE.PIK7
                m.QTY8   = M.QTY8   - ORDLINE.PIK8
                m.TOTQTY = M.TOTQTY - ORDLINE.TOTPIK
              ENDIF
              IF lcRpCanTyp = 'S'
                m.QTY1   = INT((M.QTY1 * lnShavePer/100))
                m.QTY2   = INT((M.QTY2 * lnShavePer/100))
                m.QTY3   = INT((M.QTY3 * lnShavePer/100))
                m.QTY4   = INT((M.QTY4 * lnShavePer/100))
                m.QTY5   = INT((M.QTY5 * lnShavePer/100))
                m.QTY6   = INT((M.QTY6 * lnShavePer/100))
                m.QTY7   = INT((M.QTY7 * lnShavePer/100))
                m.QTY8   = INT((M.QTY8 * lnShavePer/100))
                m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
              ENDIF
              IF M.TOTQTY > 0
                INSERT INTO (lcTmpFil) FROM MEMVAR
              ENDIF
            ENDSCAN
          ENDIF
        ENDSCAN
      ELSE
        IF llAccSelect
          SELECT (lcAccFile)
          SCAN
            SELECT ('Ordline')
            gfSetorder('ORDLACC')
            IF gfSeek(&lcAccFile..ACCOUNT,'Ordline','ORDLACC')
              SELECT('Ordline')
              SCAN REST WHILE ACCOUNT+STORE+ORDER+STR(LINENO,6)=&lcAccFile..ACCOUNT FOR CORDTYPE =lcRpCrt AND ;
                  gfSeek(lcRpCrt+ORDLINE.ORDER,'Ordhdr','Ordhdr') AND ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0;
                  AND IIF(!EMPTY(lcPriority),ORDHDR.PRIORITY = lcPriority,.T.) AND ;
                  IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(ORDHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.);
                  AND gfSeek(ORDLINE.STYLE,'Style','Style');
                  AND IIF(llStyleSelect,SEEK(STYLE.CSTYMAJOR,lcStyleFile),.T.) ;
                  AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                  IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                  AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                  AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)
                SCATTER MEMVAR MEMO
                *N000682,1 11/20/2012 MMT Globlization changes[Start]
                *WAIT WINDOW LANG_Collect+ M.ORDER NOWAIT
                WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.ORDER NOWAIT
                *N000682,1 11/20/2012 MMT Globlization changes[End]

                IF !EMPTY(ORDLINE.PIKTKT)
                  m.QTY1   = M.QTY1   - ORDLINE.PIK1
                  m.QTY2   = M.QTY2   - ORDLINE.PIK2
                  m.QTY3   = M.QTY3   - ORDLINE.PIK3
                  m.QTY4   = M.QTY4   - ORDLINE.PIK4
                  m.QTY5   = M.QTY5   - ORDLINE.PIK5
                  m.QTY6   = M.QTY6   - ORDLINE.PIK6
                  m.QTY7   = M.QTY7   - ORDLINE.PIK7
                  m.QTY8   = M.QTY8   - ORDLINE.PIK8
                  m.TOTQTY = M.TOTQTY - ORDLINE.TOTPIK
                ENDIF
                IF lcRpCanTyp = 'S'
                  m.QTY1   = INT((M.QTY1 * lnShavePer/100))
                  m.QTY2   = INT((M.QTY2 * lnShavePer/100))
                  m.QTY3   = INT((M.QTY3 * lnShavePer/100))
                  m.QTY4   = INT((M.QTY4 * lnShavePer/100))
                  m.QTY5   = INT((M.QTY5 * lnShavePer/100))
                  m.QTY6   = INT((M.QTY6 * lnShavePer/100))
                  m.QTY7   = INT((M.QTY7 * lnShavePer/100))
                  m.QTY8   = INT((M.QTY8 * lnShavePer/100))
                  m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
                ENDIF
                IF M.TOTQTY > 0
                  INSERT INTO (lcTmpFil) FROM MEMVAR
                ENDIF
              ENDSCAN
            ENDIF
          ENDSCAN
        ELSE
          IF llStyleSelect
            SELECT('Ordline')
            gfSetorder('ORDLINES')
            SELECT(lcStyleFile)
            SCAN
              IF gfSeek(SUBSTR(&lcStyleFile..CSTYMAJOR,1,lnMajLength),'Ordline','ORDLINES')
                SELECT('Ordline')

                SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = SUBSTR(&lcStyleFile..CSTYMAJOR,1,lnMajLength) ;
                    FOR CORDTYPE =lcRpCrt AND ;
                    gfSeek(lcRpCrt + ORDLINE.ORDER,'Ordhdr','Ordhdr') AND ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0;
                    AND IIF(!EMPTY(lcPriority),ORDHDR.PRIORITY = lcPriority,.T.) AND ;
                    IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(ORDHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.);
                    AND gfSeek(ORDLINE.STYLE,'Style','Style');
                    AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                    IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                    AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                    AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)
                  SCATTER MEMVAR MEMO
                  *N000682,1 11/20/2012 MMT Globlization changes[Start]
                  *WAIT WINDOW LANG_Collect+ M.ORDER NOWAIT
                  WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.ORDER NOWAIT
                  *N000682,1 11/20/2012 MMT Globlization changes[End]

                  IF !EMPTY(ORDLINE.PIKTKT)
                    m.QTY1   = M.QTY1   - ORDLINE.PIK1
                    m.QTY2   = M.QTY2   - ORDLINE.PIK2
                    m.QTY3   = M.QTY3   - ORDLINE.PIK3
                    m.QTY4   = M.QTY4   - ORDLINE.PIK4
                    m.QTY5   = M.QTY5   - ORDLINE.PIK5
                    m.QTY6   = M.QTY6   - ORDLINE.PIK6
                    m.QTY7   = M.QTY7   - ORDLINE.PIK7
                    m.QTY8   = M.QTY8   - ORDLINE.PIK8
                    m.TOTQTY = M.TOTQTY - ORDLINE.TOTPIK
                  ENDIF
                  IF lcRpCanTyp = 'S'
                    m.QTY1   = INT((M.QTY1 * lnShavePer/100))
                    m.QTY2   = INT((M.QTY2 * lnShavePer/100))
                    m.QTY3   = INT((M.QTY3 * lnShavePer/100))
                    m.QTY4   = INT((M.QTY4 * lnShavePer/100))
                    m.QTY5   = INT((M.QTY5 * lnShavePer/100))
                    m.QTY6   = INT((M.QTY6 * lnShavePer/100))
                    m.QTY7   = INT((M.QTY7 * lnShavePer/100))
                    m.QTY8   = INT((M.QTY8 * lnShavePer/100))
                    m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
                  ENDIF
                  IF M.TOTQTY > 0
                    INSERT INTO (lcTmpFil) FROM MEMVAR
                  ENDIF
                ENDSCAN
              ENDIF
            ENDSCAN
          ELSE
            SELECT('Ordline')
            gfSetorder('Ordline')
            gfSeek(lcRpCrt,'Ordline','Ordline')
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6)=lcRpCrt FOR ;
                gfSeek(lcRpCrt + ORDLINE.ORDER,'Ordhdr','Ordhdr') AND ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0;
                AND IIF(!EMPTY(lcPriority),ORDHDR.PRIORITY = lcPriority,.T.) AND ;
                IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(ORDHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.);
                AND gfSeek(ORDLINE.STYLE,'Style','Style');
                AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)
              SCATTER MEMVAR MEMO
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
              *WAIT WINDOW LANG_Collect+ M.ORDER NOWAIT
              WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.ORDER NOWAIT
              *N000682,1 11/20/2012 MMT Globlization changes[End]

              IF !EMPTY(ORDLINE.PIKTKT)
                m.QTY1   = M.QTY1   - ORDLINE.PIK1
                m.QTY2   = M.QTY2   - ORDLINE.PIK2
                m.QTY3   = M.QTY3   - ORDLINE.PIK3
                m.QTY4   = M.QTY4   - ORDLINE.PIK4
                m.QTY5   = M.QTY5   - ORDLINE.PIK5
                m.QTY6   = M.QTY6   - ORDLINE.PIK6
                m.QTY7   = M.QTY7   - ORDLINE.PIK7
                m.QTY8   = M.QTY8   - ORDLINE.PIK8
                m.TOTQTY = M.TOTQTY - ORDLINE.TOTPIK
              ENDIF
              IF lcRpCanTyp = 'S'
                m.QTY1   = INT((M.QTY1 * lnShavePer/100))
                m.QTY2   = INT((M.QTY2 * lnShavePer/100))
                m.QTY3   = INT((M.QTY3 * lnShavePer/100))
                m.QTY4   = INT((M.QTY4 * lnShavePer/100))
                m.QTY5   = INT((M.QTY5 * lnShavePer/100))
                m.QTY6   = INT((M.QTY6 * lnShavePer/100))
                m.QTY7   = INT((M.QTY7 * lnShavePer/100))
                m.QTY8   = INT((M.QTY8 * lnShavePer/100))
                m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
              ENDIF
              IF M.TOTQTY > 0
                INSERT INTO (lcTmpFil) FROM MEMVAR
              ENDIF
            ENDSCAN
          ENDIF
        ENDIF
      ENDIF
    CASE lcRpCanTyp = 'P'                       && Purchase Order

      lcRpCrt = IIF(lcRpCrt ='O','P','C')
      llPOSelect = .F.
      lcPOFile   = ''
      lnPOPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"POSHDR.PO"),1)
      IF lnPOPos > 0
        lcPOFile = laScopExpr[lnPOPos,6]
        IF !EMPTY(lcPOFile) AND USED(lcPOFile)
          SELECT (lcPOFile)
          LOCATE
          IF !EOF()
            llPOSelect = .T.
          ENDIF
        ENDIF
      ENDIF
      *--Complete Date
      ldStart = {}
      ldEnd = {}
      lnCompPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"POSHDR.COMPLETE"),1)
      IF lnCompPos  > 0 AND !EMPTY(laScopExpr[lnCompPos,6])
        ldStart = IIF(EMPTY(SUBSTR(laScopExpr[lnCompPos,6],1,10)),DTOC(CTOD("")),SUBSTR(laScopExpr[lnCompPos,6],1,10))
        ldEnd   = IIF(EMPTY(SUBSTR(laScopExpr[lnCompPos,6],12,21)),DTOC(CTOD("")),SUBSTR(laScopExpr[lnCompPos,6],12,21))
      ENDIF


      IF llPOSelect
        SELECT(lcPOFile)
        SCAN
          IF gfSeek('P'+lcRpCrt+&lcPOFile..PO,'POSLN','POSLN')
            SELECT ('POSLN')
            *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[Start]
            *!*	    		  SCAN REST WHILE cbusdocu+cstytype+po = 'P'+lcRpCrt+&lcPOFile..PO FOR ;
            *!*	              		  			gfSeek('P'+lcRpCrt+&lcPOFile..PO,'POSHDR','POSHDR');
            *!*					                  .AND. POSHDR.STATUS $'OH' .AND. POSLN.TRANCD $'1' AND POSLN.TOTQTY > 0 AND ;
            *!*	          	      	      IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
            *!*	                				  gfSeek(POSLN.Style,'Style','Style');
            *!*	            		          AND IIF(llStyleSelect,SEEK(STYLE.CSTYMAJOR,lcStyleFile),.T.) ;
            *!*	              	    		  AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
            *!*	          	              IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
            *!*	          	              AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
            *!*	         	                AND IIF(llFabSelect,SEEK(Style.Fabric,lcFabFile),.T.)
            SCAN REST WHILE cbusdocu+cstytype+PO = 'P'+lcRpCrt+&lcPOFile..PO FOR ;
                gfSeek('P'+lcRpCrt+&lcPOFile..PO,'POSHDR','POSHDR');
                .AND. POSHDR.STATUS $'AOH' .AND. PosLn.TRANCD $'1' AND PosLn.TOTQTY > 0 AND ;
                IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
                gfSeek(PosLn.STYLE,'Style','Style');
                AND IIF(llStyleSelect,SEEK(STYLE.CSTYMAJOR,lcStyleFile),.T.) ;
                AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)
              *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[End]
              SCATTER MEMVAR MEMO
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
              *WAIT WINDOW LANG_Collect+ M.PO NOWAIT
              WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.PO NOWAIT
              *N000682,1 11/20/2012 MMT Globlization changes[End]

              *--Check Transaction Code[start]
              lnCurRec = RECNO()
              SCAN FOR PO = M.PO AND LINENO = M.LINENO
                IF TRANCD $'245'
                  m.QTY1 = M.QTY1 - QTY1
                  m.QTY2 = M.QTY2 - QTY2
                  m.QTY3 = M.QTY3 - QTY3
                  m.QTY4 = M.QTY4 - QTY4
                  m.QTY5 = M.QTY5 - QTY5
                  m.QTY6 = M.QTY6 - QTY6
                  m.QTY7 = M.QTY7 - QTY7
                  m.QTY8 = M.QTY8 - QTY8
                  m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
                ENDIF
              ENDSCAN
              *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[Start]
              m.QTY1 = MAX(M.QTY1,0)
              m.QTY2 = MAX(M.QTY2,0)
              m.QTY3 = MAX(M.QTY3,0)
              m.QTY4 = MAX(M.QTY4,0)
              m.QTY5 = MAX(M.QTY5,0)
              m.QTY6 = MAX(M.QTY6,0)
              m.QTY7 = MAX(M.QTY7,0)
              m.QTY8 = MAX(M.QTY8,0)
              m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
              *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[End]
              *--Check Transaction Code
              INSERT INTO (lcTmpFil) FROM MEMVAR
              IF BETWEEN(lnCurRec ,1,RECCOUNT('POSLN'))
                GO RECORD lnCurRec  IN 'POSLN'
              ENDIF

            ENDSCAN
          ENDIF
        ENDSCAN
      ELSE
        IF llStyleSelect
          SELECT (lcStyleFile)
          SCAN
            IF gfSqlRun("SELECT * FROM Posln(INDEX = POSLNS) WHERE cinvtype = '0001' AND  style  LIKE '"+;
                SUBSTR(&lcStyleFile..CSTYMAJOR,1,lnMajLength)+"%' AND CBUSDOCU ='P' and  cstytype = '"+lcRpCrt+"'",'POSLN')
              *IF gfSeek('0001'+SUBSTR(&lcStyleFile..CSTYMAJOR,1,lnMajLength)+'P'+lcRpCrt,'POSLN','poslns')
              SELECT PosLn
              *           REST WHILE cinvtype+style+cbusdocu+cstytype+po+STR(lineno)+trancd ='0001'+SUBSTR(&lcStyleFile..CSTYMAJOR,1,lnMajLength)+'P'+lcRpCrt
              *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[Start]
              *!*	            SCAN FOR gfSeek('P'+lcRpCrt+POSLN.PO,'POSHDR','POSHDR');
              *!*					             .AND. POSHDR.STATUS $'OH' .AND. POSLN.TRANCD $'1' AND POSLN.TOTQTY > 0 AND ;
              *!*	          				   IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
              *!*					             gfSeek(POSLN.Style,'Style','Style');
              *!*	         	    		   AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
              *!*	   	                 IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
              *!*	   	                 AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
              *!*	   	                 AND IIF(llFabSelect,SEEK(Style.Fabric,lcFabFile),.T.)
              SCAN FOR gfSeek('P'+lcRpCrt+PosLn.PO,'POSHDR','POSHDR');
                  .AND. POSHDR.STATUS $'AOH' .AND. PosLn.TRANCD $'1' AND PosLn.TOTQTY > 0 AND ;
                  IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
                  gfSeek(PosLn.STYLE,'Style','Style');
                  AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                  IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                  AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                  AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)
                *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[End]
                SCATTER MEMVAR MEMO
                *N000682,1 11/20/2012 MMT Globlization changes[Start]
                *WAIT WINDOW LANG_Collect+ M.PO NOWAIT
                WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.PO NOWAIT
                *N000682,1 11/20/2012 MMT Globlization changes[End]

                *--Check Transaction Code[start]
                lnCurRec = RECNO()
                SCAN FOR PO = M.PO AND LINENO = M.LINENO
                  IF TRANCD $'245'
                    m.QTY1 = M.QTY1 - QTY1
                    m.QTY2 = M.QTY2 - QTY2
                    m.QTY3 = M.QTY3 - QTY3
                    m.QTY4 = M.QTY4 - QTY4
                    m.QTY5 = M.QTY5 - QTY5
                    m.QTY6 = M.QTY6 - QTY6
                    m.QTY7 = M.QTY7 - QTY7
                    m.QTY8 = M.QTY8 - QTY8
                    m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
                  ENDIF
                ENDSCAN
                *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[Start]
                m.QTY1 = MAX(M.QTY1,0)
                m.QTY2 = MAX(M.QTY2,0)
                m.QTY3 = MAX(M.QTY3,0)
                m.QTY4 = MAX(M.QTY4,0)
                m.QTY5 = MAX(M.QTY5,0)
                m.QTY6 = MAX(M.QTY6,0)
                m.QTY7 = MAX(M.QTY7,0)
                m.QTY8 = MAX(M.QTY8,0)
                m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
                *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[End]

                *--Check Transaction Code
                INSERT INTO (lcTmpFil) FROM MEMVAR
                IF BETWEEN(lnCurRec ,1,RECCOUNT('POSLN'))
                  GO RECORD lnCurRec  IN 'POSLN'
                ENDIF
              ENDSCAN
            ENDIF
          ENDSCAN
        ELSE
          IF gfSeek('P'+lcRpCrt,'POSLN','POSLN')
            SELECT ('POSLN')
            *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[Start]
            *!*	     		   SCAN REST WHILE cbusdocu+cstytype+po = 'P'+lcRpCrt FOR ;
            *!*	  		  	                 gfSeek('P'+lcRpCrt+POSLN.PO,'POSHDR','POSHDR');
            *!*	          				         .AND. POSHDR.STATUS $'OH' .AND. POSLN.TRANCD $'1' AND POSLN.TOTQTY > 0 AND ;
            *!*	                				   IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
            *!*	                				   gfSeek(POSLN.Style,'Style','Style');
            *!*	               	    		   AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
            *!*	   	                       IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
            *!*	           	               AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
            *!*	   	                       AND IIF(llFabSelect,SEEK(Style.Fabric,lcFabFile),.T.)
            SCAN REST WHILE cbusdocu+cstytype+PO = 'P'+lcRpCrt FOR ;
                gfSeek('P'+lcRpCrt+PosLn.PO,'POSHDR','POSHDR');
                .AND. POSHDR.STATUS $'AOH' .AND. PosLn.TRANCD $'1' AND PosLn.TOTQTY > 0 AND ;
                IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
                gfSeek(PosLn.STYLE,'Style','Style');
                AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)
              *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[End]
              SCATTER MEMVAR MEMO
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
              *WAIT WINDOW LANG_Collect+ M.PO NOWAIT
              WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.PO NOWAIT
              *N000682,1 11/20/2012 MMT Globlization changes[End]

              *--Check Transaction Code[start]
              lnCurRec = RECNO()
              SCAN FOR PO = M.PO AND LINENO = M.LINENO
                IF TRANCD $'245'
                  m.QTY1 = M.QTY1 - QTY1
                  m.QTY2 = M.QTY2 - QTY2
                  m.QTY3 = M.QTY3 - QTY3
                  m.QTY4 = M.QTY4 - QTY4
                  m.QTY5 = M.QTY5 - QTY5
                  m.QTY6 = M.QTY6 - QTY6
                  m.QTY7 = M.QTY7 - QTY7
                  m.QTY8 = M.QTY8 - QTY8
                  m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
                ENDIF
              ENDSCAN
              *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[Start]
              m.QTY1 = MAX(M.QTY1,0)
              m.QTY2 = MAX(M.QTY2,0)
              m.QTY3 = MAX(M.QTY3,0)
              m.QTY4 = MAX(M.QTY4,0)
              m.QTY5 = MAX(M.QTY5,0)
              m.QTY6 = MAX(M.QTY6,0)
              m.QTY7 = MAX(M.QTY7,0)
              m.QTY8 = MAX(M.QTY8,0)
              m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
              *:B609260,2 MMT 06/28/2010 Cancel Actualized POs and Handle case of Over Receiving[End]

              *--Check Transaction Code
              INSERT INTO (lcTmpFil) FROM MEMVAR
              IF BETWEEN(lnCurRec ,1,RECCOUNT('POSLN'))
                GO RECORD lnCurRec  IN 'POSLN'
              ENDIF
            ENDSCAN
          ENDIF
        ENDIF
      ENDIF
    CASE lcRpCanTyp = 'N'                       && Inter-Locarion Purchase Order
      llPOSelect = .F.
      lcPOFile   = ''
      lnPOPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"POSHDR.PO"),1)
      IF lnPOPos > 0
        lcPOFile = laScopExpr[lnPOPos,6]
        IF !EMPTY(lcPOFile) AND USED(lcPOFile)
          SELECT (lcPOFile)
          LOCATE
          IF !EOF()
            llPOSelect = .T.
          ENDIF
        ENDIF
      ENDIF
      *--Complete Date
      ldStart = {}
      ldEnd = {}
      lnCompPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"POSHDR.COMPLETE"),1)
      IF lnCompPos  > 0 AND !EMPTY(laScopExpr[lnCompPos,6])
        ldStart = IIF(EMPTY(SUBSTR(laScopExpr[lnCompPos,6],1,10)),DTOC(CTOD("")),SUBSTR(laScopExpr[lnCompPos,6],1,10))
        ldEnd   = IIF(EMPTY(SUBSTR(laScopExpr[lnCompPos,6],12,21)),DTOC(CTOD("")),SUBSTR(laScopExpr[lnCompPos,6],12,21))
      ENDIF


      IF llPOSelect
        SELECT(lcPOFile)
        SCAN
          IF gfSeek('N'+'N'+&lcPOFile..PO,'POSLN','POSLN')
            SELECT ('POSLN')
            SCAN REST WHILE cbusdocu+cstytype+PO = 'N'+'N'+&lcPOFile..PO FOR ;
                gfSeek('N'+'N'+&lcPOFile..PO,'POSHDR','POSHDR');
                .AND. POSHDR.STATUS $'OH' .AND. PosLn.TRANCD $'1' AND PosLn.TOTQTY > 0 AND ;
                IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
                gfSeek(PosLn.STYLE,'Style','Style');
                AND IIF(llStyleSelect,SEEK(STYLE.CSTYMAJOR,lcStyleFile),.T.) ;
                AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)

              DECLARE laIssQty[9],laBalQty[8]
              PRIVATE llLoop
              SCATTER MEMVAR MEMO
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
              *WAIT WINDOW LANG_Collect+ M.PO NOWAIT
              WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.PO NOWAIT
              *N000682,1 11/20/2012 MMT Globlization changes[End]

              *--Check Transaction Code[start]
              lnCurRec = RECNO()
              STORE 0 TO laIssQty,laBalQty
              STORE .F. TO llLoop
              SCAN FOR PO = M.PO AND LINENO = M.LINENO
                IF TRANCD = '6'
                  FOR lnI = 1 TO 8
                    lcI = STR(lnI,1)
                    laIssQty[lnI] = laIssQty[lnI] + EVALUATE('QTY'+lcI)
                    laIssQty[9] = laIssQty[9] + EVALUATE('QTY'+lcI)
                    laBalQty[lnI] = laBalQty[lnI] + EVALUATE('QTY'+lcI)
                  ENDFOR
                ENDIF
                IF TRANCD $'245'
                  FOR lnI = 1 TO 8
                    lcI = STR(lnI,1)
                    laBalQty[lnI] = laBalQty[lnI] - EVALUATE('QTY'+lcI)
                  ENDFOR
                ENDIF
              ENDSCAN
              FOR lnI = 1 TO 8
                IF laBalQty[lnI] # 0
                  llLoop = .T.
                  EXIT
                ENDIF
              ENDFOR
              GOTO (lnCurRec)
              IF llLoop
                LOOP
              ENDIF
              FOR lnI = 1 TO 8
                lcI = STR(lnI,1)
                m.Qty&lcI. = MAX(EVALUATE('m.Qty'+lcI) - laIssQty[lnI],0)
              ENDFOR
              m.TOTQTY = MAX(m.TOTQTY - laIssQty[9],0)
              IF m.TOTQTY = 0
                LOOP
              ENDIF
              *--Check Transaction Code[end]
              INSERT INTO (lcTmpFil) FROM MEMVAR
            ENDSCAN
          ENDIF
        ENDSCAN
      ELSE
        IF llStyleSelect
          SELECT (lcStyleFile)
          SCAN
            IF gfSqlRun("SELECT * FROM Posln(INDEX = POSLNS) WHERE cinvtype = '0001' AND  style  LIKE '"+;
                SUBSTR(&lcStyleFile..CSTYMAJOR,1,lnMajLength)+"%' AND CBUSDOCU ='N' and  cstytype = 'N'",'POSLN')
              *gfSeek('0001'+SUBSTR(&lcStyleFile..CSTYMAJOR,1,lnMajLength)+'N'+'N','POSLN','poslns')
              SELECT PosLn
              SCAN FOR gfSeek('N'+'N'+PosLn.PO,'POSHDR','POSHDR');
                  .AND. POSHDR.STATUS $'OH' .AND. PosLn.TRANCD $'1' AND PosLn.TOTQTY > 0 AND ;
                  IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
                  gfSeek(PosLn.STYLE,'Style','Style');
                  AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                  IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                  AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                  AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)

                DECLARE laIssQty[9],laBalQty[8]
                PRIVATE llLoop
                SCATTER MEMVAR MEMO
                *N000682,1 11/20/2012 MMT Globlization changes[Start]
                *WAIT WINDOW LANG_Collect+ M.PO NOWAIT
                WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.PO NOWAIT
                *N000682,1 11/20/2012 MMT Globlization changes[End]

                *--Check Transaction Code[start]
                lnCurRec = RECNO()
                STORE 0 TO laIssQty,laBalQty
                STORE .F. TO llLoop
                SCAN FOR PO = M.PO AND LINENO = M.LINENO
                  IF TRANCD = '6'
                    FOR lnI = 1 TO 8
                      lcI = STR(lnI,1)
                      laIssQty[lnI] = laIssQty[lnI] + EVALUATE('QTY'+lcI)
                      laIssQty[9] = laIssQty[9] + EVALUATE('QTY'+lcI)
                      laBalQty[lnI] = laBalQty[lnI] + EVALUATE('QTY'+lcI)
                    ENDFOR
                  ENDIF
                  IF TRANCD $'245'
                    FOR lnI = 1 TO 8
                      lcI = STR(lnI,1)
                      laBalQty[lnI] = laBalQty[lnI] - EVALUATE('QTY'+lcI)
                    ENDFOR
                  ENDIF
                ENDSCAN
                FOR lnI = 1 TO 8
                  IF laBalQty[lnI] # 0
                    llLoop = .T.
                    EXIT
                  ENDIF
                ENDFOR
                GOTO (lnCurRec)
                IF llLoop
                  LOOP
                ENDIF
                FOR lnI = 1 TO 8
                  lcI = STR(lnI,1)
                  m.Qty&lcI. = MAX(EVALUATE('m.Qty'+lcI) - laIssQty[lnI],0)
                ENDFOR
                m.TOTQTY = MAX(m.TOTQTY - laIssQty[9],0)
                IF m.TOTQTY = 0
                  LOOP
                ENDIF
                *--Check Transaction Code[end]
                INSERT INTO (lcTmpFil) FROM MEMVAR
              ENDSCAN
            ENDIF
          ENDSCAN
        ELSE
          IF gfSeek('N'+'N','POSLN','POSLN')
            SELECT ('POSLN')
            SCAN REST WHILE cbusdocu+cstytype+PO = 'N'+'N' FOR ;
                gfSeek('N'+'N'+PosLn.PO,'POSHDR','POSHDR');
                .AND. POSHDR.STATUS $'OH' .AND. PosLn.TRANCD $'1' AND PosLn.TOTQTY > 0 AND ;
                IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
                gfSeek(PosLn.STYLE,'Style','Style');
                AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)


              DECLARE laIssQty[9],laBalQty[8]
              PRIVATE llLoop
              SCATTER MEMVAR MEMO
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
              *WAIT WINDOW LANG_Collect+ M.PO NOWAIT
              WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.PO NOWAIT
              *N000682,1 11/20/2012 MMT Globlization changes[End]

              *--Check Transaction Code[start]
              lnCurRec = RECNO()
              STORE 0 TO laIssQty,laBalQty
              STORE .F. TO llLoop
              SCAN FOR PO = M.PO AND LINENO = M.LINENO
                IF TRANCD = '6'
                  FOR lnI = 1 TO 8
                    lcI = STR(lnI,1)
                    laIssQty[lnI] = laIssQty[lnI] + EVALUATE('QTY'+lcI)
                    laIssQty[9] = laIssQty[9] + EVALUATE('QTY'+lcI)
                    laBalQty[lnI] = laBalQty[lnI] + EVALUATE('QTY'+lcI)
                  ENDFOR
                ENDIF
                IF TRANCD $'245'
                  FOR lnI = 1 TO 8
                    lcI = STR(lnI,1)
                    laBalQty[lnI] = laBalQty[lnI] - EVALUATE('QTY'+lcI)
                  ENDFOR
                ENDIF
              ENDSCAN
              FOR lnI = 1 TO 8
                IF laBalQty[lnI] # 0
                  llLoop = .T.
                  EXIT
                ENDIF
              ENDFOR
              GOTO (lnCurRec)
              IF llLoop
                LOOP
              ENDIF
              FOR lnI = 1 TO 8
                lcI = STR(lnI,1)
                m.Qty&lcI. = MAX(EVALUATE('m.Qty'+lcI) - laIssQty[lnI],0)
              ENDFOR
              m.TOTQTY = MAX(m.TOTQTY - laIssQty[9],0)
              IF m.TOTQTY = 0
                LOOP
              ENDIF
              *--Check Transaction Code[end]
              INSERT INTO (lcTmpFil) FROM MEMVAR
            ENDSCAN
          ENDIF
        ENDIF
      ENDIF
    CASE lcRpCanTyp = 'C'
      llPOSelect = .F.
      lcPOFile   = ''
      lnPOPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"ALLTRIM(POSHDR.PO)"),1)
      IF lnPOPos > 0
        lcPOFile = laScopExpr[lnPOPos,6]
        IF !EMPTY(lcPOFile) AND USED(lcPOFile)
          SELECT (lcPOFile)
          LOCATE
          IF !EOF()
            llPOSelect = .T.
          ENDIF
        ENDIF
      ENDIF
      *--Complete Date
      ldStart = {}
      ldEnd = {}
      lnCompPos = ASUBSCRIPT(laScopExpr,ASCAN(laScopExpr,"ALLTRIM(POSHDR.COMPLETE)"),1)
      IF lnCompPos  > 0 AND !EMPTY(laScopExpr[lnCompPos,6])
        ldStart = IIF(EMPTY(SUBSTR(laScopExpr[lnCompPos,6],1,10)),DTOC(CTOD("")),SUBSTR(laScopExpr[lnCompPos,6],1,10))
        ldEnd   = IIF(EMPTY(SUBSTR(laScopExpr[lnCompPos,6],12,21)),DTOC(CTOD("")),SUBSTR(laScopExpr[lnCompPos,6],12,21))
      ENDIF

      IF llPOSelect
        SELECT(lcPOFile)
        SCAN
          IF gfSeek('P'+'U'+&lcPOFile..PO,'POSLN','POSLN')
            SELECT ('POSLN')
            SCAN REST WHILE cbusdocu+cstytype+PO = 'P'+'U'+&lcPOFile..PO FOR ;
                gfSeek('P'+'U'+&lcPOFile..PO,'POSHDR','POSHDR');
                .AND. POSHDR.STATUS $'OH' .AND. PosLn.TRANCD $'1' AND PosLn.TOTQTY > 0 AND ;
                IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
                gfSeek(PosLn.STYLE,'Style','Style');
                AND IIF(llStyleSelect,SEEK(STYLE.CSTYMAJOR,lcStyleFile),.T.) ;
                AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)

              SCATTER MEMVAR MEMO
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
              *WAIT WINDOW LANG_Collect+ M.PO NOWAIT
              WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.PO NOWAIT
              *N000682,1 11/20/2012 MMT Globlization changes[End]

              *--Check Transaction Code[start]
              lnCurRec = RECNO()
              *SCAN FOR CUTTKT = M.CUTTKT AND LINENO = M.LINENO
              SCAN FOR  cbusdocu+cstytype+PO = 'P'+'U'+M.PO AND LINENO = M.LINENO
                IF TRANCD $'234'
                  m.QTY1 = M.QTY1 - QTY1
                  m.QTY2 = M.QTY2 - QTY2
                  m.QTY3 = M.QTY3 - QTY3
                  m.QTY4 = M.QTY4 - QTY4
                  m.QTY5 = M.QTY5 - QTY5
                  m.QTY6 = M.QTY6 - QTY6
                  m.QTY7 = M.QTY7 - QTY7
                  m.QTY8 = M.QTY8 - QTY8
                  m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
                ENDIF
              ENDSCAN
              GOTO (lnCurRec)
              *--Check Transaction Dode[end]
              IF M.TOTQTY > 0
                INSERT INTO (lcTmpFil) FROM MEMVAR
              ENDIF
            ENDSCAN
          ENDIF
        ENDSCAN
      ELSE
        IF llStyleSelect
          SELECT (lcStyleFile)
          SCAN
            IF gfSqlRun("SELECT * FROM Posln(INDEX = POSLNS) WHERE cinvtype = '0001' AND  style  LIKE '"+;
                SUBSTR(&lcStyleFile..CSTYMAJOR,1,lnMajLength)+"%' AND CBUSDOCU ='P' and  cstytype = 'U'",'POSLN')
              *gfSeek('0001'+SUBSTR(&lcStyleFile..CSTYMAJOR,1,lnMajLength)+'P'+'U','POSLN','poslns')
              SELECT PosLn
              SCAN FOR gfSeek('P'+'U'+PosLn.PO,'POSHDR','POSHDR');
                  .AND. POSHDR.STATUS $'OH' .AND. PosLn.TRANCD $'1' AND PosLn.TOTQTY > 0 AND ;
                  IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
                  gfSeek(PosLn.STYLE,'Style','Style');
                  AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                  IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                  AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                  AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)
                SCATTER MEMVAR MEMO
                *N000682,1 11/20/2012 MMT Globlization changes[Start]
                *WAIT WINDOW LANG_Collect+ M.PO NOWAIT
                WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.PO NOWAIT
                *N000682,1 11/20/2012 MMT Globlization changes[End]

                *--Check Transaction Code[start]
                lnCurRec = RECNO()
                SCAN FOR PO = M.PO AND LINENO = M.LINENO
                  IF TRANCD $'245'
                    m.QTY1 = M.QTY1 - QTY1
                    m.QTY2 = M.QTY2 - QTY2
                    m.QTY3 = M.QTY3 - QTY3
                    m.QTY4 = M.QTY4 - QTY4
                    m.QTY5 = M.QTY5 - QTY5
                    m.QTY6 = M.QTY6 - QTY6
                    m.QTY7 = M.QTY7 - QTY7
                    m.QTY8 = M.QTY8 - QTY8
                    m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
                  ENDIF
                ENDSCAN
                GOTO (lnCurRec)
                *--Check Transaction Dode[end]
                IF M.TOTQTY > 0
                  INSERT INTO (lcTmpFil) FROM MEMVAR
                ENDIF
              ENDSCAN
            ENDIF
          ENDSCAN
        ELSE
          IF gfSeek('P'+'U','POSLN','POSLN')
            SELECT ('POSLN')
            SCAN REST WHILE cbusdocu+cstytype+PO = 'P'+'U' FOR ;
                gfSeek('P'+'U'+PosLn.PO,'POSHDR','POSHDR');
                .AND. POSHDR.STATUS $'OH' .AND. PosLn.TRANCD $'1' AND PosLn.TOTQTY > 0 AND ;
                IIF(!EMPTY(ldStart) AND !EMPTY(ldEnd),BETWEEN(POSHDR.COMPLETE,CTOD(ldStart),CTOD(ldEnd)),.T.) AND;
                gfSeek(PosLn.STYLE,'Style','Style');
                AND IIF(llGroupSelect,SEEK(STYLE.CSTYGROUP,lcGroupFile),.T.) AND;
                IIF(lcFree_Clr = 'F' AND llClr2CSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen),lcClr2CFile),.T.);
                AND IIF(lcFree_Clr = 'C' AND llClrCSelect,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrCFile),.T.);
                AND IIF(llFabSelect,SEEK(STYLE.Fabric,lcFabFile),.T.)

              SCATTER MEMVAR MEMO
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
              *WAIT WINDOW LANG_Collect+ M.PO NOWAIT
              WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect,loFormSet.GetHeaderText("LANG_Collect",loFormSet.HeaderAlias))+ M.PO NOWAIT
              *N000682,1 11/20/2012 MMT Globlization changes[End]

              *--Check Transaction Code[start]
              lnCurRec = RECNO()
              SCAN FOR PO = M.PO AND LINENO = M.LINENO
                IF TRANCD $'245'
                  m.QTY1 = M.QTY1 - QTY1
                  m.QTY2 = M.QTY2 - QTY2
                  m.QTY3 = M.QTY3 - QTY3
                  m.QTY4 = M.QTY4 - QTY4
                  m.QTY5 = M.QTY5 - QTY5
                  m.QTY6 = M.QTY6 - QTY6
                  m.QTY7 = M.QTY7 - QTY7
                  m.QTY8 = M.QTY8 - QTY8
                  m.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
                ENDIF
              ENDSCAN
              GOTO (lnCurRec)
              *--Check Transaction Dode[end]
              IF M.TOTQTY > 0
                INSERT INTO (lcTmpFil) FROM MEMVAR
              ENDIF
            ENDSCAN
          ENDIF
        ENDIF

      ENDIF
  ENDCASE
  SELECT ORDLINE
  gfSetorder('ordline')


  *!*************************************************************
  *! Name      : lfConvertToCursor
  *: Developer : MAriam Mazhar (MMT)
  *: Date      : 09/14/2005
  *! Purpose   : Convert a list of values into a cusrsor
  *!*************************************************************
  *!
FUNCTION lfConvertToCursor
  PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
  lcCursorTemp = lcNewFile &&Cursor Hold Selected values
  DIMENSION laTempacstru[1,4]
  laTempacstru[1,1] = lcFieldName

  DO CASE

    CASE   ALLTRIM(lcFieldName) = 'SEASON'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0

    CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
    CASE   ALLTRIM(lcFieldName) = 'COLOR'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0

    CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0

  ENDCASE
  = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
  lcValuesToConvert = lcStrToConv
  IF !EMPTY(lcValuesToConvert)
    lnStart=1
    lnEnd=AT('|',lcValuesToConvert )
    DO WHILE lnEnd <> 0
      SELECT(lcCursorTemp )
      APPEND BLANK
      REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
      lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
      lnEnd=AT('|',lcValuesToConvert )
    ENDDO
    IF lnEnd = 0
      SELECT(lcCursorTemp )
      APPEND BLANK
      REPLACE &lcFieldName  WITH lcValuesToConvert
    ENDIF
  ENDIF
  RETURN .T.
  *!*************************************************************
  *! Name      : lfAddControlSource
  *: Developer : MAriam Mazhar (MMT)
  *: Date      : 10/09/2006
  *! Purpose   : Add contol Source to grid
  *!*************************************************************
  *!
FUNCTION lfAddControlSource
  PARAMETERS loFormSet

  WITH loFormSet.ARIAform1.GRDOrders.GRDMultiSelectionGrid
    .RECORDSOURCE = ""
    .RECORDSOURCE = loFormSet.lcTmpFil

    DO CASE
      CASE lcRpCanTyp $ 'SO'
        .COLUMNCOUNT = 17
        .coLUMN1.CONTROLSOURCE  = 'ThisFormSet.lfvalidlogic()'
        .coLUMN1.header1.CAPTION = ''
        .coLUMN1.WIDTH = 21
        .coLUMN2.CONTROLSOURCE  = 'CORDTYPE'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.coLUMN2.header1.Caption = 'Order Type'
        .coLUMN2.header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ORTYPE,loFormSet.GetHeaderText("LANG_ORTYPE",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN2.WIDTH = 100
        .coLUMN3.CONTROLSOURCE  = 'ORDER'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.coLUMN3.header1.Caption = 'Order'
        .coLUMN3.header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_ORDER_NO,loFormSet.GetHeaderText("LANG_SOMASCA_ORDER_NO",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN3.WIDTH = 80
        .coLUMN4.CONTROLSOURCE  = 'ACCOUNT'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.coLUMN4.header1.Caption = 'Account'
        .coLUMN4.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_ACCOUNT,loFormSet.GetHeaderText("LANG_SOMASCA_ACCOUNT",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN4.WIDTH = 80
        .coLUMN5.CONTROLSOURCE  = 'CWARECODE'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.coLUMN5.header1.Caption = 'Warehouse'
        .coLUMN5.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_WAREHOUSE,loFormSet.GetHeaderText("LANG_SOMASCA_WAREHOUSE",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN5.WIDTH = 80
        .coLUMN6.CONTROLSOURCE  = 'STORE'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.coLUMN6.header1.Caption = 'Store'
        .coLUMN6.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_STORE,loFormSet.GetHeaderText("LANG_SOMASCA_STORE",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN6.WIDTH = 80
        .coLUMN7.CONTROLSOURCE  = 'STYLE'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.coLUMN7.header1.Caption = 'Style'
        .coLUMN7.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_STYLE,loFormSet.GetHeaderText("LANG_SOMASCA_STYLE",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN7.WIDTH = 200
        .coLUMN8.CONTROLSOURCE  = 'QTY1'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *     .coLUMN8.header1.Caption = 'Qty1'
        .coLUMN8.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY1,loFormSet.GetHeaderText("LANG_SOMASCA_QTY1",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN8.WIDTH = 50
        .coLUMN9.CONTROLSOURCE  = 'QTY2'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.coLUMN9.header1.CAPTION =LANG_SOMASCA_QTY2
        .coLUMN9.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY2,loFormSet.GetHeaderText("LANG_SOMASCA_QTY2",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN9.WIDTH = 50
        .coLUMN10.CONTROLSOURCE  = 'QTY3'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *     .coLUMN10.header1.Caption = 'Qty3'
        .coLUMN10.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY3,loFormSet.GetHeaderText("LANG_SOMASCA_QTY3",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN10.WIDTH = 50
        .coLUMN11.CONTROLSOURCE  = 'QTY4'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *     .coLUMN11.header1.Caption = 'Qty4'
        .coLUMN11.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY4,loFormSet.GetHeaderText("LANG_SOMASCA_QTY4",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN11.WIDTH = 50
        .coLUMN12.CONTROLSOURCE  = 'QTY5'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *     .coLUMN12.header1.Caption = 'Qty5'
        .coLUMN12.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY5,loFormSet.GetHeaderText("LANG_SOMASCA_QTY5",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN12.WIDTH = 50
        .coLUMN13.CONTROLSOURCE  = 'QTY6'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *     .coLUMN13.header1.Caption = 'Qty6'
        .coLUMN13.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY6,loFormSet.GetHeaderText("LANG_SOMASCA_QTY6",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN13.WIDTH = 50
        .coLUMN14.CONTROLSOURCE  = 'QTY7'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *     .coLUMN14.header1.Caption = 'Qty7'
        .coLUMN14.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY7,loFormSet.GetHeaderText("LANG_SOMASCA_QTY7",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN14.WIDTH = 50
        .coLUMN15.CONTROLSOURCE  = 'QTY8'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *     .coLUMN15.header1.Caption = 'Qty8'
        .coLUMN15.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY8,loFormSet.GetHeaderText("LANG_SOMASCA_QTY8",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN15.WIDTH = 50
        .coLUMN16.CONTROLSOURCE  = 'TOTQTY'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *     .coLUMN16.header1.Caption = 'TOTQTY'
        .coLUMN16.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_TOTQTY,loFormSet.GetHeaderText("LANG_SOMASCA_TOTQTY",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN16.WIDTH = 60
        .coLUMN17.CONTROLSOURCE  = 'PRICE'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *     .coLUMN17.header1.Caption = 'Price'
        .coLUMN17.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_PRICE,loFormSet.GetHeaderText("LANG_SOMASCA_PRICE",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN17.WIDTH = 60

      CASE lcRpCanTyp = 'P'
        .COLUMNCOUNT = 16
        .coLUMN1.CONTROLSOURCE  = 'ThisFormSet.lfvalidlogic()'
        .coLUMN1.header1.CAPTION = ''
        .coLUMN1.WIDTH = 21
        .coLUMN2.CONTROLSOURCE  = 'CSTYTYPE'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.coLUMN2.header1.Caption = 'Order Type'
        .coLUMN2.header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ORTYPE,loFormSet.GetHeaderText("LANG_ORTYPE",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN2.WIDTH = 100
        .coLUMN3.CONTROLSOURCE  = 'PO'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *.coLUMN3.header1.Caption = 'Order#'
        .coLUMN3.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_ORDER_NO,loFormSet.GetHeaderText("LANG_SOMASCA_ORDER_NO",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN3.WIDTH = 80
        .coLUMN4.CONTROLSOURCE  = 'VENDOR'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN4.header1.Caption = 'Account'
        .coLUMN4.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_ACCOUNT,loFormSet.GetHeaderText("LANG_SOMASCA_ACCOUNT",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN4.WIDTH = 80
        .coLUMN5.CONTROLSOURCE  = 'CWARECODE'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN5.header1.Caption = 'Warehouse'
        .coLUMN5.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_WAREHOUSE,loFormSet.GetHeaderText("LANG_SOMASCA_WAREHOUSE",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN5.WIDTH = 80
        .coLUMN6.CONTROLSOURCE  = 'STYLE'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN6.header1.Caption = 'Style'
        .coLUMN6.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_STYLE,loFormSet.GetHeaderText("LANG_SOMASCA_STYLE",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN6.WIDTH = 200
        .coLUMN7.CONTROLSOURCE  = 'QTY1'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN7.header1.Caption = 'Qty1'
        .coLUMN7.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY1,loFormSet.GetHeaderText("LANG_SOMASCA_QTY1",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN7.WIDTH = 50
        .coLUMN8.CONTROLSOURCE  = 'QTY2'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN8.header1.Caption = 'Qty2'
       .coLUMN8.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY2,loFormSet.GetHeaderText("LANG_SOMASCA_QTY2",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN8.WIDTH = 50
        .coLUMN9.CONTROLSOURCE  = 'QTY3'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN9.header1.Caption = 'Qty3'
        .coLUMN9.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY3,loFormSet.GetHeaderText("LANG_SOMASCA_QTY3",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN9.WIDTH = 50
        .coLUMN10.CONTROLSOURCE  = 'QTY4'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN10.header1.Caption = 'Qty4'
        .coLUMN10.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY4,loFormSet.GetHeaderText("LANG_SOMASCA_QTY4",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN10.WIDTH = 50
        .coLUMN11.CONTROLSOURCE  = 'QTY5'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN11.header1.Caption = 'Qty5'
        .coLUMN11.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY5,loFormSet.GetHeaderText("LANG_SOMASCA_QTY5",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN11.WIDTH = 50
        .coLUMN12.CONTROLSOURCE  = 'QTY6'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN12.header1.Caption = 'Qty6'
        .coLUMN12.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY6,loFormSet.GetHeaderText("LANG_SOMASCA_QTY6",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN12.WIDTH = 50
        .coLUMN13.CONTROLSOURCE  = 'QTY7'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN13.header1.Caption = 'Qty7'
        .coLUMN13.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY7,loFormSet.GetHeaderText("LANG_SOMASCA_QTY7",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN13.WIDTH = 50
        .coLUMN14.CONTROLSOURCE  = 'QTY8'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN14.header1.Caption = 'Qty8'
        .coLUMN14.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY8,loFormSet.GetHeaderText("LANG_SOMASCA_QTY8",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN14.WIDTH = 50
        .coLUMN15.CONTROLSOURCE  = 'TOTQTY'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN15.header1.Caption = 'TOTQTY'
        .coLUMN15.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_TOTQTY,loFormSet.GetHeaderText("LANG_SOMASCA_TOTQTY",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN15.WIDTH = 60
        .coLUMN16.CONTROLSOURCE  = 'PRICE'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN16.header1.Caption = 'Price'
        .coLUMN16.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_PRICE,loFormSet.GetHeaderText("LANG_SOMASCA_PRICE",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN16.WIDTH = 60
      CASE lcRpCanTyp = 'N'
        .COLUMNCOUNT = 16
        .coLUMN1.CONTROLSOURCE  = 'ThisFormSet.lfvalidlogic()'
        .coLUMN1.header1.CAPTION = ''
        .coLUMN1.WIDTH = 21
        .coLUMN2.CONTROLSOURCE  = 'CSTYTYPE'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN2.header1.Caption = 'Order Type'
        .coLUMN2.header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ORTYPE,loFormSet.GetHeaderText("LANG_ORTYPE",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN2.WIDTH = 100
        .coLUMN3.CONTROLSOURCE  = 'PO'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN3.header1.Caption = 'Order#'
        .coLUMN3.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_ORDER_NO,loFormSet.GetHeaderText("LANG_SOMASCA_ORDER_NO",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN3.WIDTH = 80
        .coLUMN4.CONTROLSOURCE  = 'VENDOR'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN4.header1.Caption = 'Source'
        .coLUMN4.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_SOURCE,loFormSet.GetHeaderText("LANG_SOMASCA_SOURCE",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN4.WIDTH = 80
        .coLUMN5.CONTROLSOURCE  = 'CWARECODE'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN5.header1.Caption = 'Target'
        .coLUMN5.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_TARGET,loFormSet.GetHeaderText("LANG_SOMASCA_TARGET",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN5.WIDTH = 80
        .coLUMN6.CONTROLSOURCE  = 'STYLE'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN6.header1.Caption = 'Style'
        .coLUMN6.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_STYLE,loFormSet.GetHeaderText("LANG_SOMASCA_STYLE",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN6.WIDTH = 200
        .coLUMN7.CONTROLSOURCE  = 'QTY1'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN7.header1.Caption = 'Qty1'
       .coLUMN7.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY1,loFormSet.GetHeaderText("LANG_SOMASCA_QTY1",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN7.WIDTH = 50
        .coLUMN8.CONTROLSOURCE  = 'QTY2'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN8.header1.Caption = 'Qty2'
        .coLUMN8.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY2,loFormSet.GetHeaderText("LANG_SOMASCA_QTY2",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN8.WIDTH = 50
        .coLUMN9.CONTROLSOURCE  = 'QTY3'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN9.header1.Caption = 'Qty3'
        .coLUMN9.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY3,loFormSet.GetHeaderText("LANG_SOMASCA_QTY3",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN9.WIDTH = 50
        .coLUMN10.CONTROLSOURCE  = 'QTY4'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN10.header1.Caption = 'Qty4'
        .coLUMN10.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY4,loFormSet.GetHeaderText("LANG_SOMASCA_QTY4",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN10.WIDTH = 50
        .coLUMN11.CONTROLSOURCE  = 'QTY5'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN11.header1.Caption = 'Qty5'
        .coLUMN11.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY5,loFormSet.GetHeaderText("LANG_SOMASCA_QTY5",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN11.WIDTH = 50
        .coLUMN12.CONTROLSOURCE  = 'QTY6'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN12.header1.Caption = 'Qty6'
        .coLUMN12.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY6,loFormSet.GetHeaderText("LANG_SOMASCA_QTY6",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN12.WIDTH = 50
        .coLUMN13.CONTROLSOURCE  = 'QTY7'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN13.header1.Caption = 'Qty7'
        .coLUMN13.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY7,loFormSet.GetHeaderText("LANG_SOMASCA_QTY7",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN13.WIDTH = 50
        .coLUMN14.CONTROLSOURCE  = 'QTY8'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN14.header1.Caption = 'Qty8'
        .coLUMN14.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY8,loFormSet.GetHeaderText("LANG_SOMASCA_QTY8",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN14.WIDTH = 50
        .coLUMN15.CONTROLSOURCE  = 'TOTQTY'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN15.header1.Caption = 'TOTQTY'
        .coLUMN15.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_TOTQTY,loFormSet.GetHeaderText("LANG_SOMASCA_TOTQTY",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN15.WIDTH = 60
        .coLUMN16.CONTROLSOURCE  = 'PRICE'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN16.header1.Caption = 'Price'
        .coLUMN16.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_PRICE,loFormSet.GetHeaderText("LANG_SOMASCA_PRICE",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN16.WIDTH = 60

      CASE lcRpCanTyp = 'C'
        .COLUMNCOUNT = 12
        .coLUMN1.CONTROLSOURCE  = 'ThisFormSet.lfvalidlogic()'
        .coLUMN1.header1.CAPTION = ''
        .coLUMN1.WIDTH = 21
        .coLUMN2.CONTROLSOURCE  = 'PO'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN2.header1.Caption = 'Cut Ticket'
        .coLUMN2.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_CUT_TICKET,loFormSet.GetHeaderText("LANG_SOMASCA_CUT_TICKET",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN2.WIDTH = 80
        .coLUMN3.CONTROLSOURCE  = 'STYLE'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN3.header1.Caption = 'Style'
        .coLUMN3.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_STYLE,loFormSet.GetHeaderText("LANG_SOMASCA_STYLE",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN3.WIDTH = 200
        .coLUMN4.CONTROLSOURCE  = 'QTY1'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN4.header1.Caption = 'Qty1'
        .coLUMN4.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY1,loFormSet.GetHeaderText("LANG_SOMASCA_QTY1",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN4.WIDTH = 50
        .coLUMN5.CONTROLSOURCE  = 'QTY2'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN5.header1.Caption = 'Qty2'
        .coLUMN5.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY2,loFormSet.GetHeaderText("LANG_SOMASCA_QTY2",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN5.WIDTH = 50
        .coLUMN6.CONTROLSOURCE  = 'QTY3'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN6.header1.Caption = 'Qty3'
        .coLUMN6.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY3,loFormSet.GetHeaderText("LANG_SOMASCA_QTY3",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN6.WIDTH = 50
        .coLUMN7.CONTROLSOURCE  = 'QTY4'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN7.header1.Caption = 'Qty4'
        .coLUMN7.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY4,loFormSet.GetHeaderText("LANG_SOMASCA_QTY4",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN7.WIDTH = 50
        .coLUMN8.CONTROLSOURCE  = 'QTY5'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN8.header1.Caption = 'Qty5'
        .coLUMN8.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY5,loFormSet.GetHeaderText("LANG_SOMASCA_QTY5",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN8.WIDTH = 50
        .coLUMN9.CONTROLSOURCE  = 'QTY6'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *    .coLUMN9.header1.Caption = 'Qty6'
        .coLUMN9.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY6,loFormSet.GetHeaderText("LANG_SOMASCA_QTY6",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        .coLUMN9.WIDTH = 50
        .coLUMN10.CONTROLSOURCE  = 'QTY7'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN10.header1.Caption = 'Qty7'
        .coLUMN10.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY7,loFormSet.GetHeaderText("LANG_SOMASCA_QTY7",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN10.WIDTH = 50
        .coLUMN11.CONTROLSOURCE  = 'QTY8'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN11.header1.Caption = 'Qty8'
        .coLUMN11.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_QTY8,loFormSet.GetHeaderText("LANG_SOMASCA_QTY8",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN11.WIDTH = 50
        .coLUMN12.CONTROLSOURCE  = 'TOTQTY'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *    .coLUMN12.header1.Caption = 'TOTQTY'
        .coLUMN12.header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOMASCA_TOTQTY,loFormSet.GetHeaderText("LANG_SOMASCA_TOTQTY",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        .coLUMN12.WIDTH = 60

    ENDCASE
    .coLUMN1.CURRENTCONTROL = 'Ariacheckbox1'
    .SETALL('ReadOnly',.T.,'COLUMN')
    .coLUMN1.READONLY = .F.
    .coLUMN1.SPARSE = .F.
    .ENABLED = .T.
    .coLUMN1.AriaCheckBox1.ENABLED = .T.
    .coLUMN1.AriaCheckBox1.READONLY = .F.
    .coLUMN1.AriaCheckBox1.CAPTION = ''
    .REFRESH()
  ENDWITH
  *!*************************************************************
  *! Name      : lfvSelect
  *: Developer : MAriam Mazhar (MMT)
  *: Date      : 10/09/2006
  *! Purpose   : Select
  *!*************************************************************
  *!
FUNCTION lfvSelect
  PARAMETERS loFormSet


  SELECT(loFormSet.lcTmpFil)

  REPLACE LLSEL WITH !LLSEL

  lnRecNo = RECNO()
  LOCATE FOR LLSEL

  IF !FOUND()
    loFormSet.llInvert = .T.
    loFormSet.llSelect = .T.
    loFormSet.llselectall = .T.
    loFormSet.llSelectnone = .F.
    loFormSet.llenbalecancel = .F.
  ELSE    && Else
    loFormSet.llenbalecancel = .T.
    loFormSet.llSelectnone = .T.
    LOCATE FOR !LLSEL
    IF !FOUND()
      loFormSet.llselectall  = .F.
    ELSE
      loFormSet.llselectall  = .T.
    ENDIF
  ENDIF
  GO RECORD lnRecNo

  *!*************************************************************
  *! Name      : mgetValueLogic
  *: Developer : MAriam Mazhar (MMT)
  *: Date      : 10/09/2006
  *! Purpose   : Valid checkbox
  *!*************************************************************
  *!
FUNCTION mgetValueLogic
  PARAMETERS lctempFile
  PRIVATE lnRetVal
  lnRetVal = EVALUATE(lctempFile+'.llSel')
  RETURN lnRetVal
ENDFUNC

*!*************************************************************
*! Name      : lfvSelAll
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/09/2006
*! Purpose   : Select All
*!*************************************************************
*!
FUNCTION lfvSelAll
  PARAMETERS loFormSet
  SELECT(loFormSet.lcTmpFil)
  lnRecCurrn = RECNO()

  REPLACE ALL LLSEL WITH .T.
  GO lnRecCurrn

  loFormSet.llselectall  = .F.
  loFormSet.llSelectnone = .T.
  loFormSet.llenbalecancel = .T.
  *!*************************************************************
  *! Name      : lfvinvert
  *: Developer : MAriam Mazhar (MMT)
  *: Date      : 10/09/2006
  *! Purpose   : Invert
  *!*************************************************************
  *!
FUNCTION lfvinvert
  PARAMETERS loFormSet
  SELECT(loFormSet.lcTmpFil)

  lnRecNOCurr = RECNO()

  REPLACE ALL LLSEL WITH !LLSEL

  LOCATE FOR LLSEL
  IF !FOUND()
    loFormSet.llselectall  = .T.
    loFormSet.llSelectnone = .F.
    loFormSet.llenbalecancel = .F.
  ELSE
    loFormSet.llenbalecancel = .T.
    loFormSet.llSelectnone = .T.
    LOCATE FOR !LLSEL
    IF !FOUND()
      loFormSet.llselectall  = .F.
    ENDIF
  ENDIF
  GO lnRecNOCurr
  *!*************************************************************
  *! Name      : lfvSelNon
  *: Developer : MAriam Mazhar (MMT)
  *: Date      : 10/09/2006
  *! Purpose   : Select None
  *!*************************************************************
  *!
FUNCTION lfvSelNon
  PARAMETERS loFormSet
  SELECT(loFormSet.lcTmpFil)
  lnRecCurr = RECNO()

  REPLACE ALL LLSEL WITH .F.

  GO lnRecCurr

  loFormSet.llselectall  = .T.
  loFormSet.llSelectnone = .F.
  loFormSet.llenbalecancel = .F.


  *!**************************************************************************
  *! Name      : lfChkAlloc
  *: Developer : MAriam Mazhar(MMT)
  *! Date      : 10/04/2006
  *! Purpose   : Function to Check the allocation status
  *!***************************************************************************
  *! Passed Parameters : lfChkAlloc ==> Cancelation Type
  *!***************************************************************************
  *! Return      : Allocation Message Flag
  *!***************************************************************************
  *!This Function Due to B602475,B602476,602493
  *!***************************************************************************
FUNCTION lfChkAlloc
  PARAMETER lcCancType

  PRIVATE lnRespons
  DO CASE
    CASE lcCancType = 'C'
      SELECT (lcTmpFil)
      SCAN FOR LLSEL

        IF loFormSet.llOpenCut AND gfSeek('1' + &lcTmpFil..PO + &lcTmpFil..STYLE ,'CutPick','CutPick')
          *--Check The ordline PikTkt
          *--If There is PickTkt Display Message & Do nothing
          IF gfSeek(loFormSet.lcRpCrt + CutPick.ORDER, 'ORDLINE','ORDLINE')
            SELECT ORDLINE
            LOCATE FOR CORDTYPE+ORDER+STR(LINENO,6) = 'O' + CutPick.ORDER .AND. ;
              STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = &lcTmpFil..STYLE
            IF FOUND() .AND. !EMPTY(ORDLINE.PIKTKT)
              IF !llPickMsg
                *N000682,1 11/20/2012 MMT Globlization changes[Start]
               *lcMsg = LANG_Msg_Alloc + LANG_Msg_Release
               lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_Alloc,loFormSet.GetHeaderText("LANG_Msg_Alloc",loFormSet.HeaderAlias)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_Release,loFormSet.GetHeaderText("LANG_Msg_Release",loFormSet.HeaderAlias))
               *N000682,1 11/20/2012 MMT Globlization changes[End]

                =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
              ENDIF
              *--MARK THE LINE AS NOT SELECTED
              SELECT(lcTmpFil)
              REPLACE LLSEL WITH .F.
              llPickMsg = .T.
              LOOP
            ELSE
              LOCATE FOR CORDTYPE+ORDER+STR(LINENO,6) = 'C' + CutPick.ORDER .AND. ;
                STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = &lcTmpFil..STYLE
              IF FOUND() .AND. !EMPTY(ORDLINE.PIKTKT)
                IF !llPickMsg
                  *N000682,1 11/20/2012 MMT Globlization changes[Start]
                  *lcMsg = LANG_Msg_Alloc + LANG_Msg_Release
                  lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_Alloc,loFormSet.GetHeaderText("LANG_Msg_Alloc",loFormSet.HeaderAlias)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_Release,loFormSet.GetHeaderText("LANG_Msg_Release",loFormSet.HeaderAlias))
                  *N000682,1 11/20/2012 MMT Globlization changes[End]

                  =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
                ENDIF
                *--MARK THE LINE AS NOT SELECTED
                SELECT(lcTmpFil)
                REPLACE LLSEL WITH .F.
                llPickMsg = .T.
                LOOP
              ENDIF
            ENDIF
          ENDIF
          IF !llAlocMsg
            *--Check if we didn't display the allocation message
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
            *lcMsg = LANG_Msg_CT+LANG_Msg_CT_Cncl
            lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_CT,loFormSet.GetHeaderText("LANG_Msg_CT",loFormSet.HeaderAlias))+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_CT_Cncl,loFormSet.GetHeaderText("LANG_Msg_CT_Cncl",loFormSet.HeaderAlias))
            *N000682,1 11/20/2012 MMT Globlization changes[End]

            lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)

            IF lnRespons = 1
              llOkCancel = .T.
            ELSE
              llOkCancel = .F.
            ENDIF
            llAlocMsg = .T.
          ENDIF
          *--Replace the current Temp. record with the selected Y/N
          SELECT(lcTmpFil)
          REPLACE LLSEL WITH llOkCancel
        ENDIF
      ENDSCAN

    CASE lcCancType = 'P'
      SELECT (lcTmpFil)
      SCAN FOR LLSEL
        IF loFormSet.llOpenCut AND gfSeek('2' + &lcTmpFil..PO + &lcTmpFil..STYLE ,'CutPick','CutPick')
          *--Check The ordline PikTkt
          *--If There is PickTkt Display Message & Do nothing
          IF gfSeek('O' + CutPick.ORDER , 'ORDLINE') .OR. gfSeek('C' + CutPick.ORDER , 'ORDLINE')
            SELECT ORDLINE
            LOCATE FOR CORDTYPE+ORDER+STR(LINENO,6) = 'O' + CutPick.ORDER .AND. ;
              STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = &lcTmpFil..STYLE

            IF FOUND() .AND. !EMPTY(ORDLINE.PIKTKT)
              IF !llPickMsg
                *N000682,1 11/20/2012 MMT Globlization changes[Start]
                *lcMsg = LANG_Msg_Alloc_Po+LANG_Msg_Release
                lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_Alloc_Po,loFormSet.GetHeaderText("LANG_Msg_Alloc_Po",loFormSet.HeaderAlias))+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_Release,loFormSet.GetHeaderText("LANG_Msg_Release",loFormSet.HeaderAlias))
                *N000682,1 11/20/2012 MMT Globlization changes[End]

                =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
              ENDIF
              *--MARK THE LINE AS NOT SELECTED
              SELECT(lcTmpFil)
              REPLACE LLSEL WITH .F.
              llPickMsg = .T.
              LOOP
            ELSE

              LOCATE FOR CORDTYPE+ORDER+STR(LINENO,6) = 'C' + CutPick.ORDER .AND. ;
                STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = &lcTmpFil..STYLE
              IF FOUND() .AND. !EMPTY(ORDLINE.PIKTKT)
                IF !llPickMsg

                  *N000682,1 11/20/2012 MMT Globlization changes[Start]
                  *lcMsg = LANG_Msg_Alloc_Po+LANG_Msg_Release
                  lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_Alloc_Po,loFormSet.GetHeaderText("LANG_Msg_Alloc_Po",loFormSet.HeaderAlias))+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_Release,loFormSet.GetHeaderText("LANG_Msg_Release",loFormSet.HeaderAlias))
                  *N000682,1 11/20/2012 MMT Globlization changes[End]

                  =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
                ENDIF
                *--MARK THE LINE AS NOT SELECTED
                SELECT(lcTmpFil)
                REPLACE LLSEL WITH .F.
                llPickMsg = .T.
                LOOP
              ENDIF
            ENDIF
          ENDIF
          IF !llAlocMsg
            *--Check if we didn't display the allocation message
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
            *lcMsg = LANG_Msg_Po+LANG_Msg_CT_Cncl
            lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_Po,loFormSet.GetHeaderText("LANG_Msg_Po",loFormSet.HeaderAlias))+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_CT_Cncl,loFormSet.GetHeaderText("LANG_Msg_CT_Cncl",loFormSet.HeaderAlias))
            *N000682,1 11/20/2012 MMT Globlization changes[End]


            lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
            IF lnRespons = 1
              llOkCancel = .T.
            ELSE
              llOkCancel = .F.
            ENDIF
            llAlocMsg = .T.
          ENDIF
          *--Replace the current Temp. record with the selected Y/N
          SELECT(lcTmpFil)
          REPLACE LLSEL WITH llOkCancel
        ENDIF
      ENDSCAN

    CASE lcCancType $ 'OS'
      SELECT (lcTmpFil)
      SCAN FOR LLSEL
        *--Check we didn't display messages before
        IF loFormSet.llOpenCut
          SELECT CutPick
          lcCutPkOrd = ORDER()
          gfSetorder('CUTORD')

          gfSqlRun("Select * from CUTPICK where trancd in ('1','2') and [Order] = '"+&lcTmpFil..ORDER+"' and CUTPICK.CORDLINE = '"+ALLTRIM(STR(&lcTmpFil..LINENO))+"'",'CUTPICK')
          *--scan the cutpick file for any selected orders
          SCAN FOR TRANCD+ORDER+cordline = '1' + &lcTmpFil..ORDER .OR.  ;
              TRANCD+ORDER+cordline = '2' + &lcTmpFil..ORDER .AND. ;
              ALLTRIM(CutPick.cordline) = ALLTRIM(STR(&lcTmpFil..LINENO))

            *--If we didn't display the releas allocation message
            *--Check if the allocation is found
            IF !llAlocMsg
              *lcMsg = 'These Sales Order lines have allocated quantities,'+;
              'canceling these lines will release the allocation, would you like to continue?'
              *lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
              IF llUsrCPrv
                *N000682,1 11/20/2012 MMT Globlization changes[Start]
                *lcMsg = LANG_Msg_So+LANG_Msg_CT_Cncl
                lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_So,loFormSet.GetHeaderText("LANG_Msg_So",loFormSet.HeaderAlias))+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_CT_Cncl,loFormSet.GetHeaderText("LANG_Msg_CT_Cncl",loFormSet.HeaderAlias))
                *N000682,1 11/20/2012 MMT Globlization changes[End]

                lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
              ELSE
                *N000682,1 11/20/2012 MMT Globlization changes[Start]
                *lcMsg = LANG_Msg_So+LANG_Msg__cancl_den
                lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg_So,loFormSet.GetHeaderText("LANG_Msg_So",loFormSet.HeaderAlias))+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg__cancl_den,loFormSet.GetHeaderText("LANG_Msg__cancl_den",loFormSet.HeaderAlias))
                *N000682,1 11/20/2012 MMT Globlization changes[End]

                =gfModalGen('TRM32059B00000','DIALOG',lcMsg)
                lnRespons = 2
              ENDIF
              llAlocMsg    = .T.
              IF lnRespons = 1
                llOkCancel = .T.
              ELSE
                llOkCancel = .F.
              ENDIF
            ENDIF
            REPLACE &lcTmpFil..LLSEL WITH llOkCancel

            *--Check for CT/PO updates only if on Hold
            IF !llUpdPoCt .AND. !llAcordMsg AND llOkCancel
              IF CutPick.TRANCD = '1'
                IF gfSeek('P'+'U'+CutPick.CTKTNO,'POSHDR') .AND. POSHDR.STATUS = 'H'
                  *N000682,1 11/20/2012 MMT Globlization changes[Start]
                  *lcMsg = LANG_Msg__Hold_Ct_Po
                  lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg__Hold_Ct_Po,loFormSet.GetHeaderText("LANG_Msg__Hold_Ct_Po",loFormSet.HeaderAlias))
                  *N000682,1 11/20/2012 MMT Globlization changes[End]

                  lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
                  llAcordMsg = .T.
                  IF lnRespons = 1
                    llUpdPoCt  = .T.
                  ELSE
                    llUpdPoCt  = .F.
                  ENDIF
                ENDIF
              ELSE
                IF (SEEK('P'+'P'+CutPick.CTKTNO,'POSHDR') .OR. gfSeek('A'+'C'+CutPick.CTKTNO,'POSHDR')) .AND. POSHDR.STATUS = 'H'
                  *N000682,1 11/20/2012 MMT Globlization changes[Start]
                  *lcMsg = LANG_Msg__Hold_Ct_Po
                  lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Msg__Hold_Ct_Po,loFormSet.GetHeaderText("LANG_Msg__Hold_Ct_Po",loFormSet.HeaderAlias))
                  *N000682,1 11/20/2012 MMT Globlization changes[End]

                  lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
                  llAcordMsg = .T.
                  IF lnRespons = 1
                    llUpdPoCt  = .T.
                  ELSE
                    llUpdPoCt  = .F.
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDSCAN
        ENDIF
      ENDSCAN
  ENDCASE
  *!**************************************************************************
  *! Name      : lfRelAllOc
  *: Developer : MAriam Mazhar(MMT)
  *! Date      : 10/04/2006
  *! Purpose   : Function to Check the allocation status
  *!***************************************************************************
  *! Passed Parameters : lfChkAlloc ==> Cancelation Type
  *!***************************************************************************
  *! Return      : NONE
  *!***************************************************************************
FUNCTION lfRelAllOc
  PARAMETER lcCanType
  DO CASE
    CASE lcCanType = 'C'
      IF loFormSet.llOpenCut AND gfSeek('1' + &lcTmpFil..PO + &lcTmpFil..STYLE , 'CutPick')
        IF !EMPTY(CutPick.ORDER)
          SELECT ORDLINE
          IF gfSeek('O' + CutPick.ORDER + CutPick.cordline) .OR. gfSeek('C' + CutPick.ORDER + CutPick.cordline)
            *--Zero out Cut1,.....TotCut Fields in OrdLine
            gfREPLACE([CUT1   WITH CUT1   - CutPick.QTY1,]+;
              [CUT2   WITH CUT2   - CutPick.QTY2,]+;
              [CUT3   WITH CUT3   - CutPick.QTY3,]+;
              [CUT4   WITH CUT4   - CutPick.QTY4,]+;
              [CUT5   WITH CUT5   - CutPick.QTY5,]+;
              [CUT6   WITH CUT6   - CutPick.QTY6,]+;
              [CUT7   WITH CUT7   - CutPick.QTY7,]+;
              [CUT8   WITH CUT8   - CutPick.QTY8])

            gfREPLACE("TOTCUT WITH TOTCUT - CutPick.TOTQTY")
          ENDIF
          SELECT ORDHDR
          gfSetorder('Ordhdr')
          *        SET ORDER TO Ordhdr
          IF gfSeek('O' + CutPick.ORDER,'ORDHDR') OR gfSeek('C' + CutPick.ORDER,'ORDHDR')
            gfREPLACE([TOTCUT WITH (TOTCUT - CutPick.TOTQTY)])
          ENDIF
        ENDIF

        *--Delete Allocated lines from CutPik File (Releas Allocation)
        SELECT CutPick
        gfDELETE()
        *-- Releas the allocated QTYs From CutTktl file
        SELECT PosLn
        lcCtTag = ORDER()
        gfSetorder('POSLN')
        *    SET ORDER TO TAG Cutlin

        IF gfSeek('P'+'U'+M.PO + '0001' + M.style + STR(M.LINENO,6) + '1')
          gfREPLACE([ORD1   WITH 0,]+;
            [ORD2   WITH 0,]+;
            [ORD3   WITH 0,]+;
            [ORD4   WITH 0,]+;
            [ORD5   WITH 0,]+;
            [ORD6   WITH 0,]+;
            [ORD7   WITH 0,]+;
            [ORD8   WITH 0,]+;
            [TOTORD WITH 0])
        ENDIF
        gfSetorder(lcCtTag)
        SELECT POSHDR
        gfSetorder('POSHDR')

        IF gfSeek('P'+'U'+M.PO)
          gfREPLACE([TOTORD WITH 0])
        ENDIF
      ENDIF
    CASE lcCanType = 'P'

      IF loFormSet.llOpenCut

        SELECT CutPick
        = gfSeek('2' + &lcTmpFil..PO + &lcTmpFil..STYLE , 'CutPick')
        SCAN REST WHILE TRANCD + CTKTNO + STYLE = '2' + &lcTmpFil..PO + &lcTmpFil..STYLE
          IF !EMPTY(CutPick.ORDER)
            SELECT ORDLINE

            IF gfSeek('O' + CutPick.ORDER + CutPick.cordline) .OR. gfSeek('C' + CutPick.ORDER + CutPick.cordline)
              *--Zero out Cut1,.....TotCut Fields in OrdLine
              gfREPLACE([CUT1   WITH CUT1   - CutPick.QTY1,]+;
                [CUT2   WITH CUT2   - CutPick.QTY2,]+;
                [CUT3   WITH CUT3   - CutPick.QTY3,]+;
                [CUT4   WITH CUT4   - CutPick.QTY4,]+;
                [CUT5   WITH CUT5   - CutPick.QTY5,]+;
                [CUT6   WITH CUT6   - CutPick.QTY6,]+;
                [CUT7   WITH CUT7   - CutPick.QTY7,]+;
                [CUT8   WITH CUT8   - CutPick.QTY8,]+;
                [TOTCUT WITH TOTCUT - CutPick.TOTQTY])
            ENDIF
            SELECT ORDHDR
            gfSetorder('Ordhdr')
            IF gfSeek('O' + CutPick.ORDER,'ORDHDR') OR gfSeek('C' + CutPick.ORDER,'ORDHDR')
              gfREPLACE([TOTCUT WITH (TOTCUT - CutPick.TOTQTY)])
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF

      *-- Releas the allocated QTYs From POSLN file
      SELECT PosLn
      IF gfSeek('P'+'P'+M.PO + M.style + STR(M.LINENO,6) + '1') OR gfSeek('A'+'C'+M.PO + M.style + STR(M.LINENO,6) + '1')
        gfREPLACE([ORD1   WITH 0,]+;
          [ORD2   WITH 0,]+;
          [ORD3   WITH 0,]+;
          [ORD4   WITH 0,]+;
          [ORD5   WITH 0,]+;
          [ORD6   WITH 0,]+;
          [ORD7   WITH 0,]+;
          [ORD8   WITH 0,]+;
          [TOTORD WITH 0])
      ENDIF
      SELECT POSHDR
      gfSetorder('POSHDR')
      IF gfSeek('P'+'P'+M.PO) OR gfSeek('A'+'C'+M.PO)
        gfREPLACE([TOTORD WITH 0])
      ENDIF


      IF loFormSet.llOpenCut
        *--Delete Allocated lines from CutPik File (Releas Allocation)
        SELECT CutPick
        IF gfSeek('2' + &lcTmpFil..PO + &lcTmpFil..STYLE , 'CutPick')
          SCAN REST WHILE TRANCD + CTKTNO + STYLE = '2' + &lcTmpFil..PO + &lcTmpFil..STYLE
            *DELETE REST WHILE trancd + ctktno + style = '2' + &lcTmpFil..PO + &lcTmpFil..Style
            gfDELETE()
          ENDSCAN
        ENDIF
      ENDIF

    CASE lcCanType $ 'SO'

      *:B608615,1 MMT 07/14/2008 Fix bug of Error in case of releasing allocted orders [Start]
      *IF llOpenCut
      IF loFormSet.llOpenCut
        *:B608615,1 MMT 07/14/2008 Fix bug of Error in case of releasing allocted orders [End]

        SELECT CutPick

        lcCutPkOrd = ORDER()

        gfSetorder('CUTORD')

        gfSqlRun("Select * from CUTPICK where trancd in ('1','2') and [Order] = '"+&lcTmpFil..ORDER+"' and CUTPICK.CORDLINE = '"+SUBSTR(STR(&lcTmpFil..LINENO),5,10)+"'",'CUTPICK')

        SCAN FOR TRANCD+ORDER+cordline = '1'+ &lcTmpFil..ORDER + SUBSTR(STR(&lcTmpFil..LINENO),5,10) .OR. ;
            TRANCD+ORDER+cordline = '2'+ &lcTmpFil..ORDER + SUBSTR(STR(&lcTmpFil..LINENO),5,10)
          IF TRANCD = '1'
            IF gfSeek('P'+'U'+CutPick.CTKTNO,'POSHDR')
              IF POSHDR.STATUS = 'H'
                IF llUpdPoCt
                  PRIVATE lcStyOdr ,lcDyOrd
                  *--Update Cut Ticket
                  SELECT PosLn
                  lcOldCTOrd = ORDER()
                  gfSetorder('POSLN')
                  *SET ORDER TO TAG Cutlin
                  IF gfSeek ('P'+'U'+CutPick.CTKTNO + '0001'+CutPick.STYLE + CutPick.cTktLineNo + '1')
                    *N000682,1 11/20/2012 MMT Globlization changes[Start]
                    *WAIT WINDOW LANG_Upd_msg + ALLTRIM(CutPick.CTKTNO) NOWAIT
                    WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Upd_msg,loFormSet.GetHeaderText("LANG_Upd_msg",loFormSet.HeaderAlias)) + ALLTRIM(CutPick.CTKTNO) NOWAIT
                    *N000682,1 11/20/2012 MMT Globlization changes[End]

                    gfREPLACE([QTY1   WITH QTY1   - CUTPICK.QTY1,]+;
                      [QTY2   WITH QTY2   - CUTPICK.QTY2,]+;
                      [QTY3   WITH QTY3   - CUTPICK.QTY3,]+;
                      [QTY4   WITH QTY4   - CUTPICK.QTY4,]+;
                      [QTY5   WITH QTY5   - CUTPICK.QTY5,]+;
                      [QTY6   WITH QTY6   - CUTPICK.QTY6,]+;
                      [QTY7   WITH QTY7   - CUTPICK.QTY7,]+;
                      [QTY8   WITH QTY8   - CUTPICK.QTY8,]+;
                      [TOTQTY WITH TOTQTY - CUTPICK.TOTQTY])
                    SELECT STYLE
                    lcStyOdr = ORDER()
                    gfSetorder('STYLE')
                    IF gfSeek(PosLn.STYLE,'STYLE')
                      =RLOCK()
                      gfREPLACE([WIP1   WITH WIP1   - CUTPICK.QTY1,]+;
                        [WIP2   WITH WIP2   - CUTPICK.QTY2  ,]+;
                        [WIP3   WITH WIP3   - CUTPICK.QTY3  ,]+;
                        [WIP4   WITH WIP4   - CUTPICK.QTY4  ,]+;
                        [WIP5   WITH WIP5   - CUTPICK.QTY5  ,]+;
                        [WIP6   WITH WIP6   - CUTPICK.QTY6  ,]+;
                        [WIP7   WITH WIP7   - CUTPICK.QTY7  ,]+;
                        [WIP8   WITH WIP8   - CUTPICK.QTY8  ,]+;
                        [TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,]+;
                        [NWO1   WITH NWO1   - CUTPICK.QTY1  ,]+;
                        [NWO2   WITH NWO2   - CUTPICK.QTY2  ,]+;
                        [NWO3   WITH NWO3   - CUTPICK.QTY3  ,]+;
                        [NWO4   WITH NWO4   - CUTPICK.QTY4  ,]+;
                        [NWO5   WITH NWO5   - CUTPICK.QTY5  ,]+;
                        [NWO6   WITH NWO6   - CUTPICK.QTY6  ,]+;
                        [NWO7   WITH NWO7   - CUTPICK.QTY7  ,]+;
                        [NWO8   WITH NWO8   - CUTPICK.QTY8  ,]+;
                        [NTOTWO WITH NTOTWO - M.TOTQTY])

                      UNLOCK
                      gfSetorder(lcStyOdr)
                      *                    SET ORDER TO (lcStyOdr)

                      SELECT STYDYE
                      lcDyOrd = ORDER()
                      gfSetorder('STYDYE')
                      *                    SET ORDER TO STYDYE
                      IF gfSeek(PosLn.STYLE+PosLn.CWARECODE+PADR(PosLn.DYELOT,10),'STYDYE')

                        =RLOCK()
                        gfREPLACE([ WIP1   WITH WIP1   - CUTPICK.QTY1,]+;
                          [WIP2   WITH WIP2   - CUTPICK.QTY2  ,]+;
                          [WIP3   WITH WIP3   - CUTPICK.QTY3  ,]+;
                          [WIP4   WITH WIP4   - CUTPICK.QTY4  ,]+;
                          [WIP5   WITH WIP5   - CUTPICK.QTY5  ,]+;
                          [WIP6   WITH WIP6   - CUTPICK.QTY6  ,]+;
                          [WIP7   WITH WIP7   - CUTPICK.QTY7  ,]+;
                          [WIP8   WITH WIP8   - CUTPICK.QTY8  ,]+;
                          [TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,]+;
                          [NWO1   WITH NWO1   - CUTPICK.QTY1  ,]+;
                          [NWO2   WITH NWO2   - CUTPICK.QTY2  ,]+;
                          [NWO3   WITH NWO3   - CUTPICK.QTY3  ,]+;
                          [NWO4   WITH NWO4   - CUTPICK.QTY4  ,]+;
                          [NWO5   WITH NWO5   - CUTPICK.QTY5  ,]+;
                          [NWO6   WITH NWO6   - CUTPICK.QTY6  ,]+;
                          [NWO7   WITH NWO7   - CUTPICK.QTY7  ,]+;
                          [NWO8   WITH NWO8   - CUTPICK.QTY8  ,]+;
                          [NTOTWO WITH NTOTWO - M.TOTQTY])
                        UNLOCK
                      ENDIF
                      gfSetorder(lcDyOrd)
                      SELECT PosLn
                    ENDIF
                  ENDIF
                  gfSetorder(lcOldCTOrd)

                  SELECT POSHDR
                  = gfSeek ('P'+'U'+CutPick.CTKTNO)
                  lnRestOpn = OPEN - CutPick.TOTQTY
                  =RLOCK()
                  gfREPLACE([OPEN WITH IIF(OPEN > 0 , OPEN - CUTPICK.TOTQTY , OPEN),]+;
                    [TOTORD  WITH IIF(TOTORD > 0 , TOTORD - CUTPICK.TOTQTY , TOTORD),]+;
                    [PCS_CAN WITH PCS_CAN + CUTPICK.TOTQTY])

                  *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[Start]
                  *gfREPLACE([NICOST1 WITH (POSLN.nCost1 * lnRestOpn),]+;
                  [NICOST2 WITH (POSLN.nCost2 * lnRestOpn),]+;
                  [NICOST3 WITH (POSLN.nCost3 * lnRestOpn),]+;
                  [NICOST4 WITH (POSLN.nCost4 * lnRestOpn),]+;
                  [NICOST5 WITH (POSLN.nCost5 * lnRestOpn)])

                  gfREPLACE([NICOST1 WITH (POSLN.nICost1 * lnRestOpn),]+;
                    [NICOST2 WITH (POSLN.nICost2 * lnRestOpn),]+;
                    [NICOST3 WITH (POSLN.nICost3 * lnRestOpn),]+;
                    [NICOST4 WITH (POSLN.nICost4 * lnRestOpn),]+;
                    [NICOST5 WITH (POSLN.nICost5 * lnRestOpn)])

                  gfREPLACE([NICOST6 WITH (POSLN.nICost6 * lnRestOpn),]+;
                    [NICOST7 WITH (POSLN.nICost7 * lnRestOpn)])


                  gfREPLACE([NfCOST1 WITH (POSLN.nfCost1 * lnRestOpn),]+;
                    [NfCOST2 WITH (POSLN.nfCost2 * lnRestOpn),]+;
                    [NfCOST3 WITH (POSLN.nfCost3 * lnRestOpn),]+;
                    [NfCOST4 WITH (POSLN.nfCost4 * lnRestOpn),]+;
                    [NfCOST5 WITH (POSLN.nfCost5 * lnRestOpn)])

                  gfREPLACE([NfCOST6 WITH (POSLN.nfCost6 * lnRestOpn),]+;
                    [NfCOST7 WITH (POSLN.nfCost7 * lnRestOpn)])
                  *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[End]

                  IF OPEN = 0
                    gfREPLACE([ STATUS WITH IIF(RECEIVE > 0 , 'C' , 'X')])
                  ENDIF
                  UNLOCK
                ENDIF
              ENDIF
              *--Releas The Allocation

              SELECT ORDHDR

              *:B608615,1 MMT 07/14/2008 Fix bug of Error in case of releasing allocted orders [Start]
              *  =gfSEEK (lcRpCrt+M.ORDER)
              =gfSeek (loFormSet.lcRpCrt+M.ORDER)
              *:B608615,1 MMT 07/14/2008 Fix bug of Error in case of releasing allocted orders [End]

              gfREPLACE([TOTCUT WITH TOTCUT - CutPick.TotQty])

              SELECT ORDLINE

              *:B608615,1 MMT 07/14/2008 Fix bug of Error in case of releasing allocted orders [Start]
              *= gfSEEK(lcRpCrt + CutPick.Order + CutPick.CORDLINE)
              = gfSeek(loFormSet.lcRpCrt+ CutPick.ORDER + CutPick.cordline)
              *:B608615,1 MMT 07/14/2008 Fix bug of Error in case of releasing allocted orders [End]

              *--Zero out Cut1,.....TotCut Fields in OrdLine
              gfREPLACE([CUT1   WITH 0,]+;
                [CUT2   WITH 0,]+;
                [CUT3   WITH 0,]+;
                [CUT4   WITH 0,]+;
                [CUT5   WITH 0,]+;
                [CUT6   WITH 0,]+;
                [CUT7   WITH 0,]+;
                [CUT8   WITH 0,]+;
                [TOTCUT WITH 0])

              *-- Releas the allocated QTYs From CutTktl file
              SELECT PosLn
              lcOldCTOrd = ORDER()
              gfSetorder('POSLN')
              *            SET ORDER TO TAG Cutlin
              IF gfSeek ('P'+'U'+CutPick.CTKTNO +'0001'+ CutPick.STYLE + CutPick.cTktLineNo + '1')
                gfREPLACE([ORD1   WITH 0,]+;
                  [ORD2   WITH 0,]+;
                  [ORD3   WITH 0,]+;
                  [ORD4   WITH 0,]+;
                  [ORD5   WITH 0,]+;
                  [ORD6   WITH 0,]+;
                  [ORD7   WITH 0,]+;
                  [ORD8   WITH 0,]+;
                  [TOTORD WITH 0])
              ENDIF
              gfSetorder(lcOldCTOrd)
              *            SET ORDER TO TAG &lcOldCTOrd
              SELECT CutPick
              gfDELETE()
              *!*              BLANK
              *!*              DELETE
            ENDIF
          ENDIF
          IF TRANCD = '2'
            IF SEEK('P'+'P' + CutPick.CTKTNO,'POSHDR') OR SEEK('A'+'C' + CutPick.CTKTNO,'POSHDR')
              IF POSHDR.STATUS = 'H'
                IF llUpdPoCt AND (gfSeek ('P'+'P' + CutPick.CTKTNO + '0001' + CutPick.STYLE + CutPick.cTktLineNo + '1','POSLN' ) ;
                    OR gfSeek('A'+'C' + CutPick.CTKTNO +'0001'+ CutPick.STYLE + CutPick.cTktLineNo + '1','POSLN'))
                  *--Update PO
                  *N000682,1 11/20/2012 MMT Globlization changes[Start]
                  *WAIT WINDOW LANG_Upd_msg_PO + ALLTRIM(CutPick.CTKTNO) NOWAIT
                  WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Upd_msg_PO,loFormSet.GetHeaderText("LANG_Upd_msg_PO",loFormSet.HeaderAlias)) + ALLTRIM(CutPick.CTKTNO) NOWAIT
                  *N000682,1 11/20/2012 MMT Globlization changes[End]

                  SELECT PosLn
                  gfREPLACE([QTY1  WITH QTY1   - CUTPICK.QTY1,]+;
                    [QTY2  WITH QTY2   - CUTPICK.QTY2,]+;
                    [QTY3  WITH QTY3   - CUTPICK.QTY3,]+;
                    [QTY4  WITH QTY4   - CUTPICK.QTY4,]+;
                    [QTY5  WITH QTY5   - CUTPICK.QTY5,]+;
                    [QTY6  WITH QTY6   - CUTPICK.QTY6,]+;
                    [QTY7  WITH QTY7   - CUTPICK.QTY7,]+;
                    [QTY8  WITH QTY8   - CUTPICK.QTY8,]+;
                    [TOTQTY WITH TOTQTY- CUTPICK.TOTQTY])

                  PRIVATE lcStyOdr ,lcDyOrd
                  SELECT STYLE
                  lcStyOdr = ORDER()
                  gfSetorder('STYLE')
                  *                SET ORDER TO STYLE
                  IF gfSeek(PosLn.STYLE,'STYLE')
                    SELECT STYLE
                    =RLOCK()
                    gfREPLACE([WIP1   WITH WIP1   - CUTPICK.QTY1,]+;
                      [WIP2   WITH WIP2   - CUTPICK.QTY2,]+;
                      [WIP3   WITH WIP3   - CUTPICK.QTY3,]+;
                      [WIP4   WITH WIP4   - CUTPICK.QTY4,]+;
                      [WIP5   WITH WIP5   - CUTPICK.QTY5,]+;
                      [WIP6   WITH WIP6   - CUTPICK.QTY6,]+;
                      [WIP7   WITH WIP7   - CUTPICK.QTY7,]+;
                      [WIP8   WITH WIP8   - CUTPICK.QTY8,]+;
                      [TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,]+;
                      [NWO1   WITH NWO1   - CUTPICK.QTY1,]+;
                      [NWO2   WITH NWO2   - CUTPICK.QTY2,]+;
                      [NWO3   WITH NWO3   - CUTPICK.QTY3,]+;
                      [NWO4   WITH NWO4   - CUTPICK.QTY4,]+;
                      [NWO5   WITH NWO5   - CUTPICK.QTY5,]+;
                      [NWO6   WITH NWO6   - CUTPICK.QTY6,]+;
                      [NWO7   WITH NWO7   - CUTPICK.QTY7,]+;
                      [NWO8   WITH NWO8   - CUTPICK.QTY8,]+;
                      [NTOTWO WITH NTOTWO - M.TOTQTY])
                    UNLOCK
                    gfSetorder(lcStyOdr)
                    SELECT STYDYE
                    lcDyOrd = ORDER()
                    gfSetorder('STYDYE')

                    IF gfSeek(PosLn.STYLE+PosLn.CWARECODE+PADR(PosLn.DYELOT,10),'STYDYE')
                      =RLOCK()
                      gfREPLACE([WIP1   WITH WIP1   - CUTPICK.QTY1,]+;
                        [WIP2   WITH WIP2   - CUTPICK.QTY2,]+;
                        [WIP3   WITH WIP3   - CUTPICK.QTY3,]+;
                        [WIP4   WITH WIP4   - CUTPICK.QTY4,]+;
                        [WIP5   WITH WIP5   - CUTPICK.QTY5,]+;
                        [WIP6   WITH WIP6   - CUTPICK.QTY6,]+;
                        [WIP7   WITH WIP7   - CUTPICK.QTY7,]+;
                        [WIP8   WITH WIP8   - CUTPICK.QTY8,]+;
                        [TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,]+;
                        [NWO1   WITH NWO1   - CUTPICK.QTY1,]+;
                        [NWO2   WITH NWO2   - CUTPICK.QTY2,  ]+;
                        [NWO3   WITH NWO3   - CUTPICK.QTY3,  ]+;
                        [NWO4   WITH NWO4   - CUTPICK.QTY4,  ]+;
                        [NWO5   WITH NWO5   - CUTPICK.QTY5,  ]+;
                        [NWO6   WITH NWO6   - CUTPICK.QTY6,  ]+;
                        [NWO7   WITH NWO7   - CUTPICK.QTY7 , ]+;
                        [NWO8   WITH NWO8   - CUTPICK.QTY8 , ]+;
                        [NTOTWO WITH NTOTWO - M.TOTQTY])
                      UNLOCK

                    ENDIF
                    gfSetorder(lcDyOrd)
                    *                  SET ORDER TO (lcDyOrd)
                    SELECT PosLn
                  ENDIF

                  SELECT POSHDR
                  *--B602671,1 HDM [Start] Update PURCHASE ORDER header file

                  IF gfSeek ('P' +'P'+ CutPick.CTKTNO) OR SEEK(('A'+'C' + CutPick.CTKTNO))
                    =RLOCK()
                    gfREPLACE([OPEN   WITH (OPEN - CUTPICK.TOTQTY),]+;
                      [CANCEL WITH (CANCEL+ CUTPICK.TOTQTY)])
                    IF OPEN <= 0
                      gfREPLACE([STATUS WITH IIF(RECEIVE > 0 , 'C' , 'X')])
                    ENDIF
                  ENDIF

                  *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[Start]
                  *gfREPLACE([nICost1 WITH (POSLN.nCost1 * POSHDR.OPEN),]+;
                  [nICost2 WITH (POSLN.nCost2 * POSHDR.OPEN),]+;
                  [nICost3 WITH (POSLN.nCost3 * POSHDR.OPEN),]+;
                  [nICost4 WITH (POSLN.nCost4 * POSHDR.OPEN),]+;
                  [nICost5 WITH (POSLN.nCost5 * POSHDR.OPEN)])

                  gfREPLACE([nICost1 WITH (POSLN.nICost1 * POSHDR.OPEN),]+;
                    [nICost2 WITH (POSLN.nICost2 * POSHDR.OPEN),]+;
                    [nICost3 WITH (POSLN.nICost3 * POSHDR.OPEN),]+;
                    [nICost4 WITH (POSLN.nICost4 * POSHDR.OPEN),]+;
                    [nICost5 WITH (POSLN.nICost5 * POSHDR.OPEN)])

                  gfREPLACE([nICost6 WITH (POSLN.nICost6 * POSHDR.OPEN),]+;
                    [nICost7 WITH (POSLN.nICost7 * POSHDR.OPEN)])


                  gfREPLACE([nFCost1 WITH (POSLN.nfCost1 * POSHDR.OPEN),]+;
                    [nFCost2 WITH (POSLN.nfCost2 * POSHDR.OPEN),]+;
                    [nFCost3 WITH (POSLN.nfCost3 * POSHDR.OPEN),]+;
                    [nFCost4 WITH (POSLN.nfCost4 * POSHDR.OPEN),]+;
                    [nFCost5 WITH (POSLN.nfCost5 * POSHDR.OPEN)])

                  gfREPLACE([nFCost6 WITH (POSLN.nfCost6 * POSHDR.OPEN),]+;
                    [nFCost7 WITH (POSLN.nfCost7 * POSHDR.OPEN)])

                  *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[End]

                  UNLOCK

                ENDIF
              ENDIF
              SELECT POSHDR
              *-- RELEASE The allocation from poshdr File
              IF gfSeek ('P'+'P' + CutPick.CTKTNO) OR gfSeek('A'+'C' + CutPick.CTKTNO)
                gfREPLACE([TOTORD WITH 0])
              ENDIF
          IF gfSEEK ('P'+'P' + CUTPICK.CTKTNO + '0001'+CUTPICK.Style + CUTPICK.CtktLineNo + '1','POSLN' ) OR gfSEEK('A'+'C' + CUTPICK.CTKTNO + '0001'+CUTPICK.Style + CUTPICK.CtktLineNo + '1','POSLN')
                *N000682,1 11/20/2012 MMT Globlization changes[Start]
                *WAIT WINDOW LANG_Upd_msg_PO     + ALLTRIM(CutPick.CTKTNO) NOWAIT
                WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Upd_msg_PO,loFormSet.GetHeaderText("LANG_Upd_msg_PO",loFormSet.HeaderAlias))     + ALLTRIM(CutPick.CTKTNO) NOWAIT
                *N000682,1 11/20/2012 MMT Globlization changes[End]

                *-- Releas the allocated QTYs From POSLN file
                SELECT PosLn
                gfREPLACE([ORD1   WITH 0,]+;
                  [ORD2   WITH 0,]+;
                  [ORD3   WITH 0,]+;
                  [ORD4   WITH 0,]+;
                  [ORD5   WITH 0,]+;
                  [ORD6   WITH 0,]+;
                  [ORD7   WITH 0,]+;
                  [ORD8   WITH 0,]+;
                  [TOTORD WITH 0])
              ENDIF
            ENDIF
            *--Releas The Allocation
            SELECT ORDHDR

            *:B608615,1 MMT 07/14/2008 Fix bug of Error in case of releasing allocted orders [Start]
            *=gfSEEK (lcRpCrt+M.ORDER)
            =gfSeek (loFormSet.lcRpCrt+M.ORDER)
            *:B608615,1 MMT 07/14/2008 Fix bug of Error in case of releasing allocted orders [End]

            gfREPLACE([TOTCUT WITH TOTCUT - CutPick.TotQty])

            SELECT ORDLINE

            *:B608615,1 MMT 07/14/2008 Fix bug of Error in case of releasing allocted orders [Start]
            *= gfSEEK(lcRpCrt + CutPick.Order + CutPick.CORDLINE)
            = gfSeek(loFormSet.lcRpCrt+ CutPick.ORDER + CutPick.cordline)
            *:B608615,1 MMT 07/14/2008 Fix bug of Error in case of releasing allocted orders [End]

            *--Zero out Cut1,.....TotCut Fields in OrdLine
            gfREPLACE([CUT1   WITH 0,]+;
              [CUT2   WITH 0,]+;
              [CUT3   WITH 0,]+;
              [CUT4   WITH 0,]+;
              [CUT5   WITH 0,]+;
              [CUT6   WITH 0,]+;
              [CUT7   WITH 0,]+;
              [CUT8   WITH 0,]+;
              [TOTCUT WITH 0])

            SELECT CutPick
            gfDELETE()
          ENDIF
        ENDSCAN
        SELECT CutPick
        gfSetorder(lcCutPkOrd)
      ENDIF

  ENDCASE

  *!**************************************************************************
  *! Name      : lfRelScr
  *! Developer : Mariam Mazhar
  *! Date      : 10/10/2006
  *! Purpose   : Function for
  *!                  1- Cancelling the Selected Lines
  *!                  2- Filling the ORDCANLN File with selected lines
  *!***************************************************************************
FUNCTION lfRelScr
  PARAMETERS loFormSet
  PRIVATE lnCounting , lcOrdNum ,lcPrevOrd , lnRecCount , llCancel
  STORE .F. TO llPickMsg , llAlocMsg , llAcordMsg
  STORE .F. TO llOkCancel , llUpdPoCt
  DECLARE laSetups[1,2]
  laSetups[1,1]  = 'M_CANAFTER'       && Number of days need to calculate order completion date
  =gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)

  IF loFormSet.llOpenCut
    = gfOpenTable(oAriaApplication.DataDir + 'CutPick' ,'CutPick','SH')
  ENDIF

  DO CASE
    CASE loFormSet.lcRpCanTyp$'OS'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *lcMsg = LANG_So_MSG
      lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_So_MSG,loFormSet.GetHeaderText("LANG_So_MSG",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]


    CASE loFormSet.lcRpCanTyp$'P'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *lcMsg = LANG_Po_MSG
      lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Po_MSG,loFormSet.GetHeaderText("LANG_Po_MSG",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]


    CASE loFormSet.lcRpCanTyp$'N'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *lcMsg = LANG_Inter_MSG
     lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Inter_MSG,loFormSet.GetHeaderText("LANG_Inter_MSG",loFormSet.HeaderAlias))
     *N000682,1 11/20/2012 MMT Globlization changes[End]


    CASE loFormSet.lcRpCanTyp$'C'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *lcMsg = LANG_CT_MSG
      lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CT_MSG,loFormSet.GetHeaderText("LANG_CT_MSG",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]


  ENDCASE
  *----
  IF gfModalGen('QRM32057B00006','DIALOG',lcMsg) = 2
    RETURN
  ENDIF

  STORE '' TO lcPrevOrd
  STORE 0 TO lnCanc
  SELECT ORDLINE

  lnCounting = 0
  lcOrdNum  = 0
  lcOrdNum = ''
  lcTmpFil = loFormSet.lcTmpFil
  DO CASE             && Save From temp file to the cancellation file
    CASE loFormSet.lcRpCanTyp = 'O'   && Order cancellation      *-->O R D E R  C A N C E L A T I O N<--
      SELECT (lcTmpFil)
      LOCATE
      =lfChkAlloc('O')
      SCAN FOR LLSEL
        WAIT WINDOW ORDER NOWAIT
        = gfSeek(loFormSet.lcRpCrt + &lcTmpFil..ORDER +STR(&lcTmpFil..LINENO,6), 'ORDLINE','ORDLINE')
        IF lcOrdNum  <> ORDER
          lnCounting = 0
        ENDIF
        lcOrdNum  = ORDER
        IF lnCounting = 0
          lnRecNo   = RECNO()
          SELECT COUNT(*),SUM(TOTQTY) ;
            FROM (lcTmpFil)           ;
            WHERE CORDTYPE + ORDER = loFormSet.lcRpCrt + lcOrdNum AND LLSEL;
            INTO ARRAY laTmpTot
          lnSelected = laTmpTot[1]
          lnTCan     = laTmpTot[2]
          GOTO (lnRecNo)
          =gfSeek(loFormSet.lcRpCrt + &lcTmpFil..ORDER ,'ORDHDR','ORDHDR')
          llSndLnToC = ORDHDR.STATUS <> 'B' AND (oAriaApplication.SystemDate > ORDHDR.ENTERED + laSetups[1,2])
          llSndOrToC = (ORDHDR.OPEN = lnTCan) AND ORDHDR.SHIP > 0
          llUpdOrdCa = llSndOrToC OR (llSndLnToC AND ORDHDR.OPEN <> lnTCan)
        ENDIF
        IF lnTCan >= ORDHDR.OPEN AND ORDHDR.SHIP <= 0
          IF lnCounting = 0
            llCancel = .F.
          ENDIF
        ELSE
          IF lnCounting = 0
            llCancel = .T.
          ENDIF
        ENDIF
        lnCounting = lnCounting + 1
        lnRecNo   = RECNO()
        GOTO (lnRecNo)
        lcPrevAl=SELECT()
        SELECT ORDLINE
        SELECT (lcPrevAl)
        SELECT (lcTmpFil)
        IF &lcTmpFil..nProcNo <= 1
          SCATTER MEMVAR MEMO
          IF loFormSet.llOpenCut
            SELECT CutPick
            lcCutPkOrd = ORDER()
            gfSetorder('CUTORD')
            IF gfSeek('1' + M.ORDER ,'CutPick') .OR. gfSeek('2' + M.ORDER ,'CutPick')
              = lfRelAllOc('O')
            ENDIF
            SELECT CutPick
            gfSetorder(lcCutPkOrd)
          ENDIF
          lnCanc = lnCanc + M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
          SELECT (lcTmpFil)
          =RLOCK(lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 2
          UNLOCK IN &lcTmpFil
        ENDIF
        IF &lcTmpFil..nProcNo = 2  && Step '2'
          *B609371,1 SMA 08/05/2010 Update Styhist Table when Sales Order is cancelled from mass so cancellation screen....[BEGIN]
          SELECT ORDLINE
          = gfSeek(loFormSet.lcRpCrt + &lcTmpFil..ORDER +STR(&lcTmpFil..LINENO,6))
          IF !EMPTY(ORDLINE.Employee)
            STORE 0 TO lnMajorLen , lnClrPos, lnClrLen
            DO lfGetClrD IN oAriaApplication.ApplicationHome+'SO\SOUPDATE.FXP'
            lnMajorLen = LEN(gfItemMask("PM"))
            IF !USED('STYHIST')
              =gfOpenTable('STYHIST','STYHIST','SH')
            ENDIF
            IF !USED('UNIFORM')
              =gfOpenTable('UNIFORM','UNIFORM','SH')
            ENDIF
            IF !USED('Contact_A')
              =gfOpenTable('Contact','Contact','SH','Contact_A')
            ENDIF
            DO lfUpStyHist IN oAriaApplication.ApplicationHome+'SO\SOUPDATE.FXP' WITH .T.,.T.
          ENDIF
          *B609371,1 SMA 08/05/2010 Update Styhist Table when Sales Order is cancelled from mass so cancellation screen....[END]
          SELECT ORDHDR
          = gfSeek(loFormSet.lcRpCrt + &lcTmpFil..ORDER)
          =RLOCK(lcTmpFil)
          *:B612470,1 MMT 09/15/2021 The Sales order totals are updated incorrectly when some lines are cancelled from Mass Order Cancellation screen[T20210902.0001][Start]
*!*	          gfREPLACE([OPEN      WITH (OPEN     - lnCanc),]+;
*!*	            [CANCEL    WITH CANCEL    + (M.TotQty),]+;
*!*	            [CANCELAMT WITH CANCELAMT + (lnCanc * M.PRICE),]+;
*!*	            [OPENAMT   WITH OPENAMT   - (lnCanc * M.PRICE)])
	      IF llUpdOrdCa
             gfREPLACE([OPEN      WITH (OPEN     - lnCanc),]+;
                       [CANCEL    WITH CANCEL    + (M.TotQty),]+;
                       [CANCELAMT WITH CANCELAMT + (lnCanc * M.PRICE),]+;
                       [OPENAMT   WITH OPENAMT   - (lnCanc * M.PRICE)])
                       
          
          ELSE
            gfREPLACE([OPEN      WITH (OPEN     - M.TotQty),]+;
                      [OPENAMT   WITH OPENAMT   - (M.TotQty* M.PRICE)])
            gfREPLACE([BOOK       WITH (BOOK     - M.TotQty),]+;
                      [BOOKAMT   WITH BOOKAMT   - (M.TotQty* M.PRICE)])
                      
          ENDIF
          *:B612470,1 MMT 09/15/2021 The Sales order totals are updated incorrectly when some lines are cancelled from Mass Order Cancellation screen[T20210902.0001][End]
          gfREPLACE([CANCELLED WITH oAriaApplication.SystemDate])
          UNLOCK
          lnCanc = 0
          lcPrevOrd = lcOrdNum
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          =RLOCK(lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 3
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        IF &lcTmpFil..nProcNo = 3  && Step '3'
          IF gfSeek(loFormSet.lcRpCrt+&lcTmpFil..ORDER+STR(&lcTmpFil..LINENO,6),'Ordline') ;
              .AND. ORDLINE.TOTQTY > 0 .AND. llUpdOrdCa

            SELECT  ORDCANLN
            APPEND BLANK
            REPLACE CORDTYPE  WITH loFormSet.lcRpCrt                           ;
              ORDER     WITH M.ORDER                           ;
              LINENO    WITH M.LINENO                          ;
              QTY1      WITH M.QTY1                            ;
              QTY2      WITH M.QTY2                            ;
              QTY3      WITH M.QTY3                            ;
              QTY4      WITH M.QTY4                            ;
              QTY5      WITH M.QTY5                            ;
              QTY6      WITH M.QTY6                            ;
              QTY7      WITH M.QTY7                            ;
              QTY8      WITH M.QTY8                            ;
              TOTQTY    WITH M.TOTQTY                          ;
              CANCELLED WITH oAriaApplication.SystemDate  ;
              CCANCRESON WITH loFormSet.ARIAform1.cboReason.VALUE

            REPLACE STYLE    WITH m.style,;
              ACCOUNT  WITH m.ACCOUNT,;
              STORE    WITH m.Store,;
              DYELOT   WITH m.DYELOT
            REPLACE Price WITH m.Price
            gfREPLACE()
          ENDIF
          *:B612470,1 MMT 09/15/2021 The Sales order totals are updated incorrectly when some lines are cancelled from Mass Order Cancellation screen[T20210902.0001][Start]
          IF !llUpdOrdCa
            IF gfSeek(loFormSet.lcRpCrt+&lcTmpFil..ORDER+STR(&lcTmpFil..LINENO,6),'Ordline')
              SELECT ORDLINE
              =gfDelete()
            ENDIF
          ENDIF
          *:B612470,1 MMT 09/15/2021 The Sales order totals are updated incorrectly when some lines are cancelled from Mass Order Cancellation screen[T20210902.0001][End]
          SELECT STYLE
          gfSetorder('STYLE')
          gfSeek(&lcTmpFil..STYLE)
          *--Start Updatin Style File
          gfREPLACE([ORD1   WITH ORD1   - M.QTY1 ,]+ ;
            [ORD2   WITH ORD2   - M.QTY2  ,]+ ;
            [ORD3   WITH ORD3   - M.QTY3  , ]+;
            [ORD4   WITH ORD4   - M.QTY4  , ]+;
            [ORD5   WITH ORD5   - M.QTY5  , ]+;
            [ORD6   WITH ORD6   - M.QTY6  , ]+;
            [ORD7   WITH ORD7   - M.QTY7  , ]+;
            [ORD8   WITH ORD8   - M.QTY8  , ]+;
            [TOTORD WITH TOTORD - M.TOTQTY])
          SELECT STYDYE
          gfSetorder('STYDYE')
          gfSeek(&lcTmpFil..STYLE + &lcTmpFil..CWARECODE)


          *--Start Updating StyDye File
          gfREPLACE([ORD1   WITH ORD1   - M.QTY1 , ]+;
            [ORD2   WITH ORD2   - M.QTY2   ,]+;
            [ORD3   WITH ORD3   - M.QTY3   ,]+;
            [ORD4   WITH ORD4   - M.QTY4  ,]+ ;
            [ORD5   WITH ORD5   - M.QTY5  , ]+;
            [ORD6   WITH ORD6   - M.QTY6  , ]+;
            [ORD7   WITH ORD7   - M.QTY7  , ]+;
            [ORD8   WITH ORD8   - M.QTY8  , ]+;
            [TOTORD WITH TOTORD - M.TOTQTY])


          *--Update Style & StyDye Files (WIP & WO) Fields[End]
          = gfAdd_Info('STYDYE')
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          =RLOCK(lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 4
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        IF &lcTmpFil..nProcNo = 4  && Step '4'

          IF llCancel
            SELECT ORDLINE
            = gfSeek(loFormSet.lcRpCrt + &lcTmpFil..ORDER +STR(&lcTmpFil..LINENO,6))
            gfREPLACE([ QTY1      WITH QTY1 - M.QTY1,] +;
              [QTY2      WITH QTY2 - M.QTY2 ,]+;
              [QTY3      WITH QTY3 - M.QTY3 ,]+;
              [QTY4      WITH QTY4 - M.QTY4 ,] +;
              [QTY5      WITH QTY5 - M.QTY5 ,]+;
              [QTY6      WITH QTY6 - M.QTY6 ,]+;
              [QTY7      WITH QTY7 - M.QTY7 ,]+;
              [QTY8      WITH QTY8 - M.QTY8 ,]+;
              [TOTQTY    WITH TOTQTY - M.TOTQTY])
          ENDIF
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          =RLOCK(lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 5
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF

        IF &lcTmpFil..nProcNo = 5  && Step '5'
          SELECT ORDHDR
          = gfSeek(loFormSet.lcRpCrt + &lcTmpFil..ORDER)
          *--                       as it affects the open Qty
          *IF BOOK - CANCEL = 0
          IF BOOK - (CANCEL + SHIP) = 0

            gfREPLACE([STATUS WITH IIF(SHIP > 0 , 'C' , 'X')])

            IF SHIP <= 0
              gfREPLACE("cCancReson WITH  loFormSet.ariaform1.cboReason.Value")
            ENDIF

            *!*           IF STATUS = 'X' .AND. ASCAN(laEvntTrig , PADR('DLEALOSO',10)) <> 0
            *!*               llMasOrder = .T.
            *!*               =gfDoTriger('SOMASCA',PADR('DLEALOSO',10))
            *!*           ENDIF

          ENDIF
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          =RLOCK(lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 6
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        lfSaveFile()
      ENDSCAN
      SELECT(lcTmpFil)
      DELETE ALL FOR LLSEL

    CASE loFormSet.lcRpCanTyp= 'S'  && Order Shaving     *-->O R D E R  S H A V I N G<--
      SELECT (lcTmpFil)
      LOCATE
      =lfChkAlloc('S')
      SCAN FOR LLSEL
        =gfSeek(loFormSet.lcRpCrt + &lcTmpFil..ORDER , 'ORDHDR')
        IF lcOrdNum  <> ORDER
          lnCounting = 0
        ENDIF
        lcOrdNum  = ORDER
        lnCounting = lnCounting + 1
        lnRecNo   = RECNO()
        COUNT TO lnSelected FOR ORDER = lcOrdNum AND LLSEL
        GOTO (lnRecNo)
        *-- Always Get the lines count for the current order header
        lcPrevAl=SELECT()
        SELECT ORDLINE
        SELECT (lcPrevAl)
        lnCounting = lnCounting + 1
        SELECT (lcTmpFil)
        SCATTER MEMVAR MEMO

        IF loFormSet.llOpenCut
          SELECT CutPick
          lcCutPkOrd = ORDER()
          gfSetorder('CUTORD')
          IF gfSeek('1' + M.ORDER ,'CutPick') .OR. gfSeek('2' + M.ORDER ,'CutPick')
            = lfRelAllOc('O')
          ENDIF
          SELECT CutPick
          gfSetorder(lcCutPkOrd)
        ENDIF

        IF &lcTmpFil..nProcNo <= 1  && Step '1'

          *-- Collect Qty1 ==> Qty8 for each line For updating Header file
          lnCanc = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 2
          =RLOCK(lcTmpFil)
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        IF &lcTmpFil..nProcNo = 2  && Step '2'
          SELECT ORDHDR
          gfREPLACE("OPEN      WITH (OPEN   - lnCanc)," +;
            "CANCEL    WITH (CANCEL + lnCanc) ," +;
            "CANCELAMT WITH (CANCELAMT + (lnCanc * M.PRICE))," +;
            "OPENAMT   WITH (OPENAMT   - (lnCanc * M.PRICE))," +;
            "CANCELLED WITH oAriaApplication.SystemDate," +;
            "CCANCRESON WITH loFormSet.ariaform1.cboReason.Value" )
          *         FOR ORDER = M.ORDER
          lnCanc = 0
          lcPrevOrd = lcOrdNum

          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          =RLOCK(lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 3
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        IF &lcTmpFil..nProcNo = 3  && Step '3'
          SELECT  ORDCANLN
          APPEND  BLANK
          REPLACE CORDTYPE   WITH loFormSet.lcRpCrt                                   ;
            ORDER      WITH M.ORDER                                   ;
            LINENO     WITH M.LINENO                                  ;
            QTY1       WITH M.QTY1                  ;
            QTY2       WITH M.QTY2                  ;
            QTY3       WITH M.QTY3                  ;
            QTY4       WITH M.QTY4                  ;
            QTY5       WITH M.QTY5                  ;
            QTY6       WITH M.QTY6                  ;
            QTY7       WITH M.QTY7                  ;
            QTY8       WITH M.QTY8                  ;
            TOTQTY     WITH (QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8) ;
            CANCELLED WITH oAriaApplication.SystemDate;
            CCANCRESON WITH loFormSet.ARIAform1.cboReason.VALUE
          REPLACE Price WITH m.Price
          = gfAdd_Info('ORDCANLN')
          gfREPLACE()

          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          =RLOCK(lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 4
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        IF &lcTmpFil..nProcNo = 4  && Step '4'
          *-- Update the shaved quantities
          SELECT ORDLINE
          lnRecNo = RECNO()
          SCAN FOR ORDER = M.ORDER .AND. LINENO = M.LINENO
            gfREPLACE([QTY1      WITH (QTY1 - M.QTY1),]+;
              [QTY2      WITH (QTY2 - M.QTY2),]+;
              [QTY3      WITH (QTY3 - M.QTY3),]+;
              [QTY4      WITH (QTY4 - M.QTY4),]+;
              [QTY5      WITH (QTY5 - M.QTY5),]+;
              [QTY6      WITH (QTY6 - M.QTY6),]+;
              [QTY7      WITH (QTY7 - M.QTY7),]+;
              [QTY8      WITH (QTY8 - M.QTY8),]+;
              [TOTQTY    WITH (TOTQTY - M.TOTQTY)])

          ENDSCAN
          GO RECORD lnRecNo
          SELECT STYLE
          gfSetorder('STYLE')
          gfSeek(&lcTmpFil..STYLE)

          *--Start Updatin Style File

          gfREPLACE([ORD1   WITH (ORD1  - M.QTY1),]+;
            [ORD2   WITH (ORD2  - M.QTY2),]+;
            [ORD3   WITH (ORD3  - M.QTY3),]+;
            [ORD4   WITH (ORD4  - M.QTY4),]+;
            [ORD5   WITH (ORD5  - M.QTY5),]+;
            [ORD6   WITH (ORD6  - M.QTY6),]+;
            [ORD7   WITH (ORD7  - M.QTY7),]+;
            [ORD8   WITH (ORD8  - M.QTY8),]+;
            [TOTORD WITH TOTORD - (ORD1 + ORD2 + ORD3 + ORD4 + ORD5 + ORD6 + ORD7 + ORD8)])

          SELECT STYDYE
          gfSetorder('STYDYE')

          gfSeek(&lcTmpFil..STYLE + &lcTmpFil..CWARECODE)



          gfREPLACE([ ORD1   WITH (ORD1  - M.QTY1),]+;
            [ORD2   WITH (ORD2  - M.QTY2),]+;
            [ORD3   WITH (ORD3  - M.QTY3),]+;
            [ORD4   WITH (ORD4  - M.QTY4),]+;
            [ORD5   WITH (ORD5  - M.QTY5),]+;
            [ORD6   WITH (ORD6  - M.QTY6),]+;
            [ORD7   WITH (ORD7  - M.QTY7),]+;
            [ORD8   WITH (ORD8  - M.QTY8),]+;
            [TOTORD WITH TOTORD - (ORD1 + ORD2 + ORD3 + ORD4 + ORD5 + ORD6 + ORD7 + ORD8)])

          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          =RLOCK(lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 5
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF

        IF &lcTmpFil..nProcNo = 5  && Step '5'
          SELECT ORDHDR
          = gfSeek(loFormSet.lcRpCrt + &lcTmpFil..ORDER)
          *--                       as it affects open Qty
          IF BOOK - (CANCEL + SHIP) = 0
            gfREPLACE([STATUS WITH IIF(SHIP > 0 , 'C' , 'X')])
            IF SHIP <= 0
              gfREPLACE("cCancReson WITH  loFormSet.ariaform1.cboReason.Value")
            ENDIF
          ENDIF
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          =RLOCK(lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 6
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        lfSaveFile()
      ENDSCAN
      SELECT(lcTmpFil)
      DELETE ALL FOR LLSEL

    CASE loFormSet.lcRpCanTyp $ 'PN'  &&Purchase order or Inter-location PO   *--> P U S C H A S E  O R D E R <--

      SELECT (lcTmpFil)
      LOCATE

      lcOrdNum = ''
      =lfChkAlloc('P')
      SCAN FOR LLSEL
        =gfSeek(loFormSet.lcRpCrt +loFormSet.lcRpCrt + &lcTmpFil..PO , 'POSHDR')
        IF lcOrdNum  <> PO
          lnCounting = 0
        ENDIF

        lcOrdNum  = PO

        lnRecNo   = RECNO()
        COUNT TO lnSelected FOR PO = lcOrdNum AND LLSEL
        GOTO (lnRecNo)

        *-- Always Get the lines count for the current order header
        lcPrevAl=SELECT(0)
        SELECT PosLn
        lnCounting = lnCounting + 1
        *      LOCATE

        SELECT (lcTmpFil)
        IF &lcTmpFil..nProcNo <= 1  && Step '1'
          SCATTER MEMVAR MEMO
          IF loFormSet.llOpenCut  AND gfSeek('2' + M.PO +M.style ,'CutPick')
            = lfRelAllOc('P')
          ENDIF


          *-- Collect Qty1 ==> Qty8 for each line For updating Header file
          lnCanc = lnCanc + M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 2
          =RLOCK(lcTmpFil)
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        IF &lcTmpFil..nProcNo = 2  && Step '2'
          =gfSeek(loFormSet.lcRpCrt+loFormSet.lcRpCrt + &lcTmpFil..PO+'0001'+ &lcTmpFil..STYLE+STR(&lcTmpFil..LINENO,6),'POSLN','POSLN')
          SELECT POSHDR
          =RLOCK()
          *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[Start]
          *gfREPLACE([ OPEN    WITH (OPEN   - lnCanc),]+;
          [CANCEL  WITH (CANCEL + lnCanc),]+;
          [NICOST1 WITH NICOST1 - (POSLN.NCOST1*lnCanc),]+;
          [NICOST2 WITH NICOST2 - (POSLN.NCOST2*lnCanc),]+;
          [NICOST3 WITH NICOST3 - (POSLN.NCOST3*lnCanc),]+;
          [NICOST4 WITH NICOST4 - (POSLN.NCOST4*lnCanc),]+;
          [NICOST5 WITH NICOST5 - (POSLN.NCOST5*lnCanc)])
          gfREPLACE([ OPEN    WITH (OPEN   - lnCanc),]+;
            [CANCEL  WITH (CANCEL + lnCanc),]+;
            [NICOST1 WITH NICOST1 - (POSLN.NICOST1*lnCanc),]+;
            [NICOST2 WITH NICOST2 - (POSLN.NICOST2*lnCanc),]+;
            [NICOST3 WITH NICOST3 - (POSLN.NICOST3*lnCanc),]+;
            [NICOST4 WITH NICOST4 - (POSLN.NICOST4*lnCanc),]+;
            [NICOST5 WITH NICOST5 - (POSLN.NICOST5*lnCanc)])


          gfREPLACE([NICOST6 WITH NICOST6 - (POSLN.NICOST6*lnCanc),]+;
            [NICOST7 WITH NICOST7 - (POSLN.NICOST7*lnCanc)])


          gfREPLACE([NFCOST1 WITH NFCOST1 - (POSLN.NFCOST1*lnCanc),]+;
            [NFCOST2 WITH NFCOST2 - (POSLN.NFCOST2*lnCanc),]+;
            [NFCOST3 WITH NFCOST3 - (POSLN.NFCOST3*lnCanc),]+;
            [NFCOST4 WITH NFCOST4 - (POSLN.NFCOST4*lnCanc),]+;
            [NFCOST5 WITH NFCOST5 - (POSLN.NFCOST5*lnCanc)])


          gfREPLACE([NFCOST6 WITH NFCOST6 - (POSLN.NFCOST6*lnCanc),]+;
            [NFCOST7 WITH NFCOST7 - (POSLN.NFCOST7*lnCanc)])

          *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[End]

          UNLOCK
          lnCanc = 0
          lcPrevOrd = lcOrdNum
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 3
          =RLOCK(lcTmpFil)
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
		
        IF &lcTmpFil..nProcNo = 3  && Step '3'
		*:B611539,1 SAH 03/07/2018 PO status is not Hold 'H' before adding lines in ORDCANLN [begin]
		if  !(POSHDR.status=='H')
			*:B611539,1 SAH 03/07/2018 PO status is not Hold 'H' before adding lines in ORDCANLN [end]
          SELECT  ORDCANLN
          APPEND  BLANK
          REPLACE CORDTYPE  WITH loFormSet.lcRpCrt                           ;
            ORDER     WITH M.PO                              ;
            LINENO    WITH M.LINENO                          ;
            QTY1      WITH M.QTY1                            ;
            QTY2      WITH M.QTY2                            ;
            QTY3      WITH M.QTY3                            ;
            QTY4      WITH M.QTY4                            ;
            QTY5      WITH M.QTY5                            ;
            QTY6      WITH M.QTY6                            ;
            QTY7      WITH M.QTY7                            ;
            QTY8      WITH M.QTY8                            ;
            TOTQTY    WITH M.TOTQTY                          ;
            CANCELLED WITH oAriaApplication.SystemDate

          REPLACE STYLE    WITH m.style,;
            DYELOT   WITH m.DYELOT



          REPLACE Price WITH m.Price


          = gfAdd_Info('ORDCANLN')
          gfREPLACE()
           *:B611539,1 SAH 03/07/2018 PO status is not Hold 'H' before adding lines in ORDCANLN [begin]
		   ENDIF 
		  *:B611539,1 SAH 03/07/2018 PO status is not Hold 'H' before adding lines in ORDCANLN [end]
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 4
          =RLOCK(lcTmpFil)
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        IF &lcTmpFil..nProcNo = 4  && Step '4'
          *-- Update the CANCELLED quantities
          SELECT PosLn
          *--Update Style & StyDye Files (WIP & WO) Fields[Start]
          IF loFormSet.lcRpCanTyp = 'P'

            SELECT STYLE
            gfSetorder('STYLE')
            gfSeek(&lcTmpFil..STYLE)
            *--Start Updatin Style File
            gfREPLACE([WIP1   WITH WIP1   - M.QTY1,]+  ;
              [WIP2   WITH WIP2   - M.QTY2,]+  ;
              [WIP3   WITH WIP3   - M.QTY3,]+   ;
              [WIP4   WITH WIP4   - M.QTY4,]+   ;
              [WIP5   WITH WIP5   - M.QTY5,]+   ;
              [WIP6   WITH WIP6   - M.QTY6,]+   ;
              [WIP7   WITH WIP7   - M.QTY7,]+   ;
              [WIP8   WITH WIP8   - M.QTY8,]+   ;
              [TOTWIP WITH TOTWIP - M.TOTQTY,]+ ;
              [NWO1   WITH NWO1   - M.QTY1,]+   ;
              [NWO2   WITH NWO2   - M.QTY2,]+   ;
              [NWO3   WITH NWO3   - M.QTY3,]+   ;
              [NWO4   WITH NWO4   - M.QTY4,]+   ;
              [NWO5   WITH NWO5   - M.QTY5,]+ ;
              [NWO6   WITH NWO6   - M.QTY6,]+   ;
              [NWO7   WITH NWO7   - M.QTY7,]+   ;
              [NWO8   WITH NWO8   - M.QTY8,]+   ;
              [NTOTWO WITH NTOTWO - M.TOTQTY])

          ENDIF

          SELECT STYDYE
          gfSetorder('STYDYE')
          gfSeek(&lcTmpFil..STYLE + &lcTmpFil..CWARECODE)
          *--Start Updating StyDye File
          gfREPLACE([WIP1   WITH WIP1   - M.QTY1,] +  ;
            [WIP2   WITH WIP2   - M.QTY2,] +   ;
            [WIP3   WITH WIP3   - M.QTY3,   ] +;
            [WIP4   WITH WIP4   - M.QTY4,   ] +;
            [WIP5   WITH WIP5   - M.QTY5 ,  ] +;
            [WIP6   WITH WIP6   - M.QTY6 ,  ] +;
            [WIP7   WITH WIP7   - M.QTY7 ,  ] +;
            [WIP8   WITH WIP8   - M.QTY8 ,  ] +;
            [TOTWIP WITH TOTWIP - M.TOTQTY ,] +;
            [NWO1   WITH NWO1   - M.QTY1   ,] +;
            [NWO2   WITH NWO2   - M.QTY2   ,] +;
            [NWO3   WITH NWO3   - M.QTY3   ,] +;
            [NWO4   WITH NWO4   - M.QTY4   ,] +;
            [NWO5   WITH NWO5   - M.QTY5   ,] +;
            [NWO6   WITH NWO6   - M.QTY6   ,] +;
            [NWO7   WITH NWO7   - M.QTY7   ,] +;
            [NWO8   WITH NWO8   - M.QTY8  ,] + ;
            [NTOTWO WITH NTOTWO - M.TOTQTY])

          IF loFormSet.lcRpCanTyp = 'N'
            gfTableUpdate(.T.,'STYDYE')
            gfSeek(EVALUATE(lcTmpFil+'.STYLE') + EVALUATE(lcTmpFil+'.VENDOR'))
            *--Start Updating StyDye File
            gfREPLACE([WIP1   WITH WIP1   + M.QTY1 ,] + ;
              [WIP2   WITH WIP2   + M.QTY2 ,] +  ;
              [WIP3   WITH WIP3   + M.QTY3 ,  ] +;
              [WIP4   WITH WIP4   + M.QTY4 , ] + ;
              [WIP5   WITH WIP5   + M.QTY5 ,  ] +;
              [WIP6   WITH WIP6   + M.QTY6 ,  ] +;
              [WIP7   WITH WIP7   + M.QTY7 ,  ] +;
              [WIP8   WITH WIP8   + M.QTY8  , ] +;
              [TOTWIP WITH TOTWIP + M.TOTQTY ,] +;
              [NWO1   WITH NWO1   + M.QTY1   ,] +;
              [NWO2   WITH NWO2   + M.QTY2   ,] +;
              [NWO3   WITH NWO3   + M.QTY3   ,] +;
              [NWO4   WITH NWO4   + M.QTY4   ,] +;
              [NWO5   WITH NWO5   + M.QTY5   ,] +;
              [NWO6   WITH NWO6   + M.QTY6   ,] +;
              [NWO7   WITH NWO7   + M.QTY7   ,] +;
              [NWO8   WITH NWO8   + M.QTY8   ,] +;
              [NTOTWO WITH NTOTWO + M.TOTQTY] )
          ENDIF

          *--Update Style & StyDye Files (WIP & WO) Fields[End]

          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          =RLOCK(lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 5
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF

        IF &lcTmpFil..nProcNo = 5  && Step '5'
          IF SEEK(loFormSet.lcRpCrt + loFormSet.lcRpCrt + &lcTmpFil..PO , 'POSHDR')
            SELECT POSHDR
            IF POSHDR.OPEN <= 0
              =RLOCK()
			  *:B611539,1 SAH 03/07/2018 call function lfAdCnLine only if PO status is not Hold 'H'  [BEGIN]
			  lchold=POSHDR.status
			  *:B611539,1 SAH 03/07/2018 call function lfAdCnLine only if PO status is not Hold 'H'  [END]
			  *:B611539,1 SAH 03/07/2018 call function lfAdCnLine only if PO status is not Hold 'H'  [BEGIN]
              IF lchold== 'O' AND POSHDR.Open = 0 
                gfREPLACE([STATUS WITH 'C'])
              ELSE
              *:B611539,1 SAH 03/07/2018 call function lfAdCnLine only if PO status is not Hold 'H'  [end]
                gfREPLACE([STATUS WITH ']+IIF(RECEIVE > 0 , 'C' , 'X')+['])
              ENDIF
             
              *FOR PO = M.PO
              UNLOCK
              lnPrevSel2=SELECT(0)
              SELECT (lcTmpFil)
              lnRec=RECNO()
              SELECT (lnPrevSel2)
			  *:B611539,1 SAH 03/07/2018 call function lfAdCnLine only if PO status is not Hold 'H'  [BEGIN]
			  IF !(lchold=='H')
			  *:B611539,1 SAH 03/07/2018 call function lfAdCnLine only if PO status is not Hold 'H'  [END]
              =lfAdCnLine()
			   *:B611539,1 SAH 03/07/2018 call function lfAdCnLine only if PO status is not Hold 'H'  [BEGIN]
			  ENDIF 
               *:B611539,1 SAH 03/07/2018 call function lfAdCnLine only if PO status is not Hold 'H'  [END]		  
              lnPrevSel3=SELECT(0)
              SELECT (lcTmpFil)
              IF lnRec<>RECNO()
                GOTO lnRec
              ENDIF
              SELECT (lnPrevSel3)
              *--mmt
            ELSE
              *:B609260,1 MMT 05/19/2010 PO-Mass PO Cancellation does not cancel PO's [Start]
              SELECT (lcTmpFil)
              *:B609260,1 MMT 05/19/2010 PO-Mass PO Cancellation does not cancel PO's [EnD]
              lnRec=RECNO()
              =lfAdCnLine()
              lnPrevSel3=SELECT(0)
              SELECT (lcTmpFil)
              IF lnRec<>RECNO()
                GOTO lnRec
              ENDIF
              SELECT (lnPrevSel3)
              *--mmt

            ENDIF
          ENDIF
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 6
          =RLOCK(lcTmpFil)
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        lfSaveFile()
      ENDSCAN
      SELECT(lcTmpFil)
      DELETE ALL FOR LLSEL

    CASE loFormSet.lcRpCanTyp = 'C' && Cut ticket

      SELECT (lcTmpFil)
      LOCATE

      lcOrdNum = ''
      =lfChkAlloc('C')

      SCAN FOR LLSEL
        =gfSeek('P'+'U'+&lcTmpFil..PO , 'POSHDR')

        *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[Start]
        =gfSeek('PU'+ &lcTmpFil..PO+'0001'+ &lcTmpFil..STYLE+STR(&lcTmpFil..LINENO,6),'POSLN','POSLN')
        *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[End]

        IF lcOrdNum  <> PO
          lnCounting = 0
        ENDIF

        lcOrdNum  = PO
        lnCounting = lnCounting + 1
        lnRecNo   = RECNO()

        COUNT TO lnSelected FOR PO = lcOrdNum AND LLSEL
        GOTO (lnRecNo)

        *-- Always Get the lines count for the current order header
        lcPrevAl=SELECT()
        SELECT PosLn

        SELECT (lcTmpFil)
        IF &lcTmpFil..nProcNo <= 1  && Step '1'
          SCATTER MEMVAR MEMO

          IF loFormSet.llOpenCut AND gfSeek('1' + M.PO +M.style ,'CutPick')
            = lfRelAllOc('C')
          ENDIF
          *-- Collect Qty1 ==> Qty8 for each line For updating Header file
          lnCanc = lnCanc + M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 2
          =RLOCK(lcTmpFil)
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF

        IF &lcTmpFil..nProcNo = 2  && Step '2'
          IF lnCounting = lnSelected
            SELECT POSHDR
            *--Update Both Open And Canceled Qty in CT Header File
            gfREPLACE([OPEN WITH (OPEN - lnCanc),] + ;
              [CANCEL WITH (CANCEL + lnCanc)   ])

            *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[Start]
            gfREPLACE([NICOST1 WITH NICOST1 - (POSLN.NICOST1*lnCanc),]+;
              [NICOST2 WITH NICOST2 - (POSLN.NICOST2*lnCanc),]+;
              [NICOST3 WITH NICOST3 - (POSLN.NICOST3*lnCanc),]+;
              [NICOST4 WITH NICOST4 - (POSLN.NICOST4*lnCanc),]+;
              [NICOST5 WITH NICOST5 - (POSLN.NICOST5*lnCanc),]+;
              [NICOST6 WITH NICOST6 - (POSLN.NICOST6*lnCanc),]+;
              [NICOST7 WITH NICOST7 - (POSLN.NICOST7*lnCanc)])

            gfREPLACE([NFCOST1 WITH NfCOST1 - (POSLN.NfCOST1*lnCanc),]+;
              [NFCOST2 WITH NFCOST2 - (POSLN.NfCOST2*lnCanc),]+;
              [NFCOST3 WITH NFCOST3 - (POSLN.NfCOST3*lnCanc),]+;
              [NFCOST4 WITH NFCOST4 - (POSLN.NfCOST4*lnCanc),]+;
              [NFCOST5 WITH NFCOST5 - (POSLN.NfCOST5*lnCanc),]+;
              [NFCOST6 WITH NFCOST6 - (POSLN.NfCOST6*lnCanc),]+;
              [NFCOST7 WITH NFCOST7 - (POSLN.NfCOST7*lnCanc)])
            *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[End]


            lnCanc = 0
            lcPrevOrd = lcOrdNum
          ENDIF
          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 3
          =RLOCK(lcTmpFil)
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        IF &lcTmpFil..nProcNo = 3  && Step '3'
          SELECT  ORDCANLN
          APPEND  BLANK
          REPLACE CORDTYPE   WITH 'T'                               ;
            ORDER      WITH M.PO                          ;
            LINENO     WITH M.LINENO                          ;
            QTY1       WITH M.QTY1                            ;
            QTY2       WITH M.QTY2                            ;
            QTY3       WITH M.QTY3                            ;
            QTY4       WITH M.QTY4                            ;
            QTY5       WITH M.QTY5                            ;
            QTY6       WITH M.QTY6                            ;
            QTY7       WITH M.QTY7                            ;
            QTY8       WITH M.QTY8                            ;
            TOTQTY     WITH M.TOTQTY                          ;
            CANCELLED  WITH oAriaApplication.SystemDate
          REPLACE STYLE    WITH m.style,;
            DYELOT   WITH m.DYELOT


          = gfAdd_Info('ORDCANLN')
          gfREPLACE()

          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 4
          =RLOCK(lcTmpFil)
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF

        IF &lcTmpFil..nProcNo = 4  && Step '4'
          *-- Update the CANCELLED quantities

          *--Update Style & StyDye Files (WIP & WO) Fields[Start]

          SELECT STYLE
          gfSetorder('STYLE')
          *        SET ORDER TO TAG STYLE
          gfSeek(&lcTmpFil..STYLE)
          *--Start Updatin Style File
          gfREPLACE([WIP1   WITH WIP1   - M.QTY1,] +  ;
            [WIP2   WITH WIP2   - M.QTY2   ,]+;
            [WIP3   WITH WIP3   - M.QTY3   ,]+;
            [WIP4   WITH WIP4   - M.QTY4   ,]+;
            [WIP5   WITH WIP5   - M.QTY5   ,]+;
            [WIP6   WITH WIP6   - M.QTY6   ,]+;
            [WIP7   WITH WIP7   - M.QTY7   ,]+;
            [WIP8   WITH WIP8   - M.QTY8   ,]+;
            [TOTWIP WITH TOTWIP - M.TOTQTY ,]+;
            [NWO1   WITH NWO1   - M.QTY1   ,]+;
            [NWO2   WITH NWO2   - M.QTY2   ,]+;
            [NWO3   WITH NWO3   - M.QTY3   ,]+;
            [NWO4   WITH NWO4   - M.QTY4  ,]+ ;
            [NWO5   WITH NWO5   - M.QTY5 ,]+  ;
            [NWO6   WITH NWO6   - M.QTY6,]+   ;
            [NWO7   WITH NWO7   - M.QTY7,]+   ;
            [NWO8   WITH NWO8   - M.QTY8,]+   ;
            [NTOTWO WITH NTOTWO - M.TOTQTY])

          SELECT STYDYE
          gfSetorder('STYDYE')


          gfSeek(&lcTmpFil..STYLE + &lcTmpFil..CWARECODE)

          *--Start Updating StyDye File
          gfREPLACE([WIP1   WITH WIP1   - M.QTY1,]+;
            [WIP2   WITH WIP2   - M.QTY2,]+   ;
            [WIP3   WITH WIP3   - M.QTY3,]+   ;
            [WIP4   WITH WIP4   - M.QTY4,]+   ;
            [WIP5   WITH WIP5   - M.QTY5, ]+  ;
            [WIP6   WITH WIP6   - M.QTY6,  ]+ ;
            [WIP7   WITH WIP7   - M.QTY7, ]+  ;
            [WIP8   WITH WIP8   - M.QTY8,   ]+;
            [TOTWIP WITH TOTWIP - M.TOTQTY, ]+;
            [NWO1   WITH NWO1   - M.QTY1  , ]+;
            [NWO2   WITH NWO2   - M.QTY2  , ]+;
            [NWO3   WITH NWO3   - M.QTY3  , ]+;
            [NWO4   WITH NWO4   - M.QTY4  , ]+;
            [NWO5   WITH NWO5   - M.QTY5  , ]+;
            [NWO6   WITH NWO6   - M.QTY6  , ]+;
            [NWO7   WITH NWO7   - M.QTY7  , ]+;
            [NWO8   WITH NWO8   - M.QTY8  ,]+ ;
            [NTOTWO WITH NTOTWO - M.TOTQTY])

          *--Update Style & StyDye Files (WIP & WO) Fields[End]

          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 5
          =RLOCK(lcTmpFil)
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF

        IF &lcTmpFil..nProcNo = 5  && Step '5'

          SELECT PosLn
          lnTOTQTY = 0
          IF !SEEK('P'+'U'+M.PO)
            gfSeek('P'+'U'+M.PO)
          ENDIF
          SCAN FOR PO = M.PO
            IF TRANCD $'234'
              lnQTY1 = M.QTY1 - QTY1
              lnQTY2 = M.QTY2 - QTY2
              lnQTY3 = M.QTY3 - QTY3
              lnQTY4 = M.QTY4 - QTY4
              lnQTY5 = M.QTY5 - QTY5
              lnQTY6 = M.QTY6 - QTY6
              lnQTY7 = M.QTY7 - QTY7
              lnQTY8 = M.QTY8 - QTY8
              lnTOTQTY = lnQTY1 + lnQTY2 + lnQTY3 + lnQTY4 + lnQTY5 + lnQTY6 + lnQTY7 + lnQTY8
            ENDIF
          ENDSCAN

          =SEEK('P'+'U'+lcOrdNum,'POSHDR')

          IF POSHDR.OPEN <= 0
            SELECT POSHDR
            gfREPLACE([STATUS WITH IIF(RECEIVE > 0 , 'C' , 'X')])
            =lfAdCnLine()
          ELSE
            =lfAdCnLine()
          ENDIF


          SELECT POSHDR

          *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[Start]
          *!*	        FOR lnLoop = 1 TO 5
          *!*	          lcCostPos = ALLTRIM(STR(lnLoop))
          *!*	              SUM NCOST&lcCostPos TO lnCost&lcCostPos FOR PO = M.PO
          *!*	        ENDFOR
          *!*	        gfREPLACE([NICOST1 WITH (NSTYORDER - CANCEL) * lnCost1,]+ ;
          *!*	                  [NICOST2 WITH (NSTYORDER - CANCEL) * lnCost2 ,]+;
          *!*	                  [NICOST3 WITH (NSTYORDER - CANCEL) * lnCost3 ,]+;
          *!*	                  [NICOST4 WITH (NSTYORDER - CANCEL) * lnCost4 ,]+;
          *!*	                  [NICOST5 WITH (NSTYORDER - CANCEL) * lnCost5 ])
          *:B608591,1 MMT 06/24/2008 Fix bug of error while Cut ticket cancellation[End]

          *        FOR CUTTKT = M.CUTTKT

          lnPrevSel=SELECT(0)
          SELECT (lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 6
          =RLOCK(lcTmpFil)
          UNLOCK IN &lcTmpFil
          SELECT (lnPrevSel)
        ENDIF
        lfSaveFile()
      ENDSCAN

      SELECT(lcTmpFil)
      DELETE ALL FOR LLSEL

  ENDCASE

  WITH loFormSet
    SELECT (lcTmpFil)
    LOCATE
    IF EOF()
      .llInvert = .F.
      .llSelect = .F.
      .llselectall = .F.
      .llSelectnone = .F.
    ELSE
      .llInvert = .T.
      .llSelect = .T.
      .llselectall = .T.
      .llSelectnone = .F.
    ENDIF
    .llenbalecancel = .F.
    .ARIAform1.GRDOrders.REFRESH()
  ENDWITH

  *!*************************************************************
  *! Name      : lfSaveFile
  *! Developer : Mariam Mazhar
  *! Date      : 10/11/2006
  *! Purpose   : function to save files
  *!*************************************************************
FUNCTION lfSaveFile

  *-- Call default save
  lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
  IF TYPE('lcTranCode') = 'N'
    =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
    RETURN .F.
  ENDIF
  lnUpdated = 0
  lnAryLen = ALEN(oAriaApplication.laRemoteTable)
  FOR lnCounter =1 TO lnAryLen
    IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == loFormSet.DATASESSIONID
      IF !oAriaApplication.laRemoteTable[lnCounter].TABLEUPDATE(lcTranCode)
        lnUpdated=lnCounter
        EXIT
      ENDIF
    ENDIF
  NEXT
  IF lnUpdated>0
    oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    RETURN
  ELSE
    oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
  ENDIF

FUNCTION lfShaveBrow
  PARAMETERS loFormSet
  DO FORM (oAriaApplication.ScreenHome+'\SOSHAVE.SCX') WITH loFormSet

FUNCTION lfShaveInit
  PARAMETERS loFormSet,loFormChild
  lcTmpFil = loFormSet.lcTmpFil
  IF loFormSet.lcRpCanTyp = 'S' .AND. &lcTmpFil..LLSEL
    = gfOpenTable(oAriaApplication.DataDir + 'Scale' ,'Scale','SH')
    SELECT(lcTmpFil)
    loFormChild.ARIAform1.cntQty.SCALE = &lcTmpFil..SCALE
    SCATTER MEMVAR MEMO

    WITH loFormChild.ARIAform1.cntQty
      .txtQty1.VALUE = M.QTY1
      .txtQty2.VALUE = M.QTY2
      .txtQty3.VALUE = M.QTY3
      .txtQty4.VALUE = M.QTY4
      .txtQty5.VALUE = M.QTY5
      .txtQty6.VALUE = M.QTY6
      .txtQty7.VALUE = M.QTY7
      .txtQty8.VALUE = M.QTY8
      .mdisableunused
      .txtQty1.ENABLED = (M.QTY1 > 0)
      .txtQty2.ENABLED= (M.QTY2 > 0)
      .txtQty3.ENABLED = (M.QTY3 > 0)
      .txtQty4.ENABLED= (M.QTY4 > 0)
      .txtQty5.ENABLED= (M.QTY5 > 0)
      .txtQty6.ENABLED= (M.QTY6 > 0)
      .txtQty7.ENABLED= (M.QTY7 > 0)
      .txtQty8.ENABLED= (M.QTY8 > 0)
    ENDWITH

    SELECT SCALE
    =gfSeek('S'+ &lcTmpFil..SCALE)

  ENDIF

  *!**************************************************************************
  *! Name      : lfvShave
  *: Developer : MAriam Mazhar(MMT)
  *! Date      : 10/04/2006
  *! Purpose   : Valid Function for shaved quantity fields (shaved quantity screen)
  *!***************************************************************************
  *! Passed Parameters : Quantity delimeter
  *!***************************************************************************
  *! Return      : NONE
  *!***************************************************************************
  *!This Function Due to B602597
  *!***************************************************************************

FUNCTION lfvShave
  PARAMETER lnValue,lnOldvalue

  IF lnValue > lnOldvalue
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *WAIT WINDOW STR(lnOldvalue,4) +LANG_Qty_msg_MAX
    WAIT WINDOW STR(lnOldvalue,4) +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Qty_msg_MAX,loFormSet.GetHeaderText("LANG_Qty_msg_MAX",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN lnOldvalue
  ELSE
    RETURN lnValue
  ENDIF

  *!**************************************************************************
  *! Name      : lfvOk
  *: Developer : MAriam Mazhar(MMT)
  *! Date      : 10/04/2006
  *! Purpose   : Valid Function for Accept button (shaved quantity screen)
  *!***************************************************************************
  *! Passed Parameters : NONE
  *!***************************************************************************
  *! Return      : NONE
  *!***************************************************************************
  *!This Function Due to B602597
  *!***************************************************************************

FUNCTION lfvOk
  PARAMETERS loFormSet,loFormChild
  SELECT(loFormSet.lcTmpFil)
  WITH loFormChild.ARIAform1.cntQty
    REPLACE QTY1 WITH .txtQty1.VALUE,;
      QTY2 WITH .txtQty2.VALUE,;
      QTY3 WITH .txtQty3.VALUE,;
      QTY4 WITH .txtQty4.VALUE,;
      QTY5 WITH .txtQty5.VALUE,;
      QTY6 WITH .txtQty6.VALUE,;
      QTY7 WITH .txtQty7.VALUE,;
      QTY8 WITH .txtQty8.VALUE,;
      TOTQTY WITH .txtQty1.VALUE + .txtQty2.VALUE + .txtQty3.VALUE+ .txtQty4.VALUE + .txtQty5.VALUE + .txtQty6.VALUE+ .txtQty7.VALUE+ .txtQty8.VALUE
  ENDWITH


