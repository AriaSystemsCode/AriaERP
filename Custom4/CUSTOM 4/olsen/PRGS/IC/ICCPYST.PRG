*****************************************************************************
*: Program file  : ICCLSST.prg (T20140430.0015)
*: Program desc. : Custom Copy styles screen for OLS10
*: Module        : IC 
*: System        : Aria Apparel System (A4xp)
*: Developer     : Mariam Mazhar[MMT]
*: Entry#        : C201618.exe {T20140430.0015}
*****************************************************************************
=GFCALLFORM('ICCPYST','IC')
*!*************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/15/2014
*! Purpose   : Init Method
*!*************************************************************
FUNCTION lfInit
PARAMETERS loFormSet
loFormSet.lcTmpScpSty = gfTempName()
=gfOpenTable('SYCCOMP','CCOMP_ID')
loFormSet.ChangeMode('S')
*!*************************************************************
*! Name      : lfvSrcComp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/15/2014
*! Purpose   : Validate source company
*!*************************************************************
FUNCTION lfvSrcComp
PARAMETERS loParentFormSet,llbrowse 
lcComp1  = ALLTRIM(loParentFormSet.AriaForm1.KeySrcCmp.KeyTextBox.Value)
loParentFormSet.lcComp2= ALLTRIM(loParentFormSet.AriaForm1.KeyDstCmp.KeyTextBox.Value)
DIMENSION laSelected[3]
SELECT SYCCOMP
LOCATE 
IF llbrowse OR (!EMPTY(lcComp1) AND !gfSEEK(lcComp1))
  llbrowse = .F.
  lcbrfields = "ccomp_id  :15:H='Company ID',"+;
               "cCom_Name :30:H='Company Name'"
  llComp = AriaBrow('',"Companies",;
                15, 01,15,02,;
                .F.,.F.,"ccomp_id,cCom_Name,cCom_Ddir","laSelected")
  IF llComp 
    loParentFormSet.lcComp1  = ALLTRIM(laSelected[1])
    loParentFormSet.lcComp1n = ALLTRIM(laSelected[2])
    loParentFormSet.lcCdir1  = ADDBS(ALLTRIM(laSelected[3]))
    loParentFormSet.AriaForm1.KeySrcCmp.KeyTextBox.Value=loParentFormSet.lcComp1  
    loParentFormSet.AriaForm1.txtSrcCmpName.Value = loParentFormSet.lcComp1n 
    loParentFormSet.AriaForm1.KeyDstCmp.Enabled = .T.
  ELSE
    loParentFormSet.lcComp1  = ''
    loParentFormSet.lcComp1n = ''
    loParentFormSet.lcCdir1  = ''
  ENDIF 
ELSE
  IF !EMPTY(lcComp1)
    loParentFormSet.lcComp1  = ALLTRIM(lcComp1)
    loParentFormSet.lcComp1n = ALLTRIM(SYCCOMP.cCom_Name)
    loParentFormSet.lcCdir1  = ADDBS(ALLTRIM(SYCCOMP.cCom_Ddir))
    loParentFormSet.AriaForm1.KeySrcCmp.KeyTextBox.Value=loParentFormSet.lcComp1  
    loParentFormSet.AriaForm1.txtSrcCmpName.Value = loParentFormSet.lcComp1n 
    loParentFormSet.AriaForm1.KeyDstCmp.Enabled = .T.
  ELSE
    loParentFormSet.lcComp1n = ''
    loParentFormSet.lcCdir1  = ''
  ENDIF  
ENDIF

*--Check if copy the styles to the same company
lcExact = SET('EXACT')
SET EXACT ON
IF !EMPTY(loParentFormSet.lcComp1) AND ALLTRIM(loParentFormSet.lcComp1) = ALLTRIM(loParentFormSet.lcComp2)
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'You cannot copy the styles to the same company.')
  loParentFormSet.AriaForm1.KeySrcCmp.KeyTextBox.Value= ''
  loParentFormSet.AriaForm1.KeySrcCmp.Enabled =.T.
  loParentFormSet.lcComp1n = ''
  loParentFormSet.lcCdir1  = ''
  loParentFormSet.lcComp1 = ''
  loParentFormSet.AriaForm1.txtSrcCmpName.Value = ''
ENDIF
SET EXACT &lcExact
lcseason ="'  '"
lcgroup  ="'  '"

IF !EMPTY(loParentFormSet.lcComp1) .AND. !EMPTY(loParentFormSet.lcComp2)

  IF USED('CODES')
    SELECT CODES
    USE
  ENDIF
  = gfOpenFile(loParentFormSet.lcCdir1+'CODES' ,'CODES','SH')
  loParentFormSet.AriaForm1.cboSeason.getCodes()
  loParentFormSet.AriaForm1.cboGroup.getCodes()
  loParentFormSet.AriaForm1.cmdStyle.Enabled = .T.
  loParentFormSet.AriaForm1.cboSeason.value =lcseason  
  loParentFormSet.AriaForm1.cboGroup.Value = lcgroup  
  loParentFormSet.AriaForm1.cmdProceed.Enabled = .T.
  loParentFormSet.AriaForm1.cmdClose.Caption = '\<Cancel '
  loParentFormSet.AriaForm1.cboSeason.Enabled = .T.
  loParentFormSet.AriaForm1.cboGroup.Enabled = .T.
ELSE
  loParentFormSet.AriaForm1.cmdProceed.Enabled = .F.
  loParentFormSet.AriaForm1.cmdClose.Caption = '\<Close '
  loParentFormSet.AriaForm1.cboSeason.Enabled = .F.
  loParentFormSet.AriaForm1.cboGroup.Enabled = .F.
ENDIF

*!*************************************************************
*! Name      : lfvChangeMode
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/15/2014
*! Purpose   : Change mode
*!*************************************************************
FUNCTION lfvChangeMode
PARAMETERS loFormSet
loParentFormSet = loFormSet
IF loFormSet.ActiveMode ='S'
  loParentFormSet.AriaForm1.cmdClose.Enabled =.T.
  loParentFormSet.AriaForm1.KeySrcCmp.Enabled =.T.
  loParentFormSet.AriaForm1.KeyDstCmp.Enabled = .F.
  loParentFormSet.AriaForm1.cmdStyle.Enabled = .F.
  loParentFormSet.AriaForm1.cboSeason.Enabled = .F.
  loParentFormSet.AriaForm1.cboGroup.Enabled = .F.
  loParentFormSet.AriaForm1.cmdProceed.Enabled = .F.
  loParentFormSet.AriaForm1.cmdClose.Caption = '\<Close '
ENDIF
*!*************************************************************
*! Name      : lfvDstComp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/15/2014
*! Purpose   : Validate destination company
*!*************************************************************
FUNCTION lfvDstComp
PARAMETERS loParentFormSet,llbrowse 
lcComp1  = ALLTRIM(loParentFormSet.AriaForm1.KeySrcCmp.KeyTextBox.Value)
loParentFormSet.lcComp2 = ALLTRIM(loParentFormSet.AriaForm1.KeyDstCmp.KeyTextBox.Value)
lcComp2 = loParentFormSet.lcComp2
DIMENSION laSelected[3]
SELECT SYCCOMP
LOCATE 
IF llbrowse OR !EMPTY(lcComp2) AND !gfSEEK(lcComp2)
  llbrowse = .F.
  lcbrfields = "ccomp_id  :15:H='Company ID',"+;
               "cCom_Name :30:H='Company Name'"
  
  llComp = AriaBrow('',"Companies",;
                15, 01,15,02,;
                .F.,.F.,"ccomp_id,cCom_Name,cCom_Ddir","laSelected")

  IF llComp 
    loParentFormSet.lcComp2  = ALLTRIM(laSelected[1])
    loParentFormSet.lcComp2n = ALLTRIM(laSelected[2])
    loParentFormSet.lcCdir2  = ADDBS(ALLTRIM(laSelected[3]))
    loParentFormSet.AriaForm1.KeyDstCmp.KeyTextBox.Value=loParentFormSet.lcComp2
    loParentFormSet.AriaForm1.txtDstCmpName.Value = loParentFormSet.lcComp2n 
  ELSE
    loParentFormSet.lcComp2  = ''
    loParentFormSet.lcComp2n = ''
    loParentFormSet.lcCdir2  = ''
  ENDIF 
ELSE
  IF !EMPTY(lcComp2)
    loParentFormSet.lcComp2n = ALLTRIM(SYCCOMP.cCom_Name)
    loParentFormSet.lcCdir2  = ADDBS(ALLTRIM(SYCCOMP.cCom_Ddir))
    loParentFormSet.AriaForm1.KeyDstCmp.KeyTextBox.Value=loParentFormSet.lcComp2
    loParentFormSet.AriaForm1.txtDstCmpName.Value = loParentFormSet.lcComp2n 
  ELSE
    loParentFormSet.lcComp2  = ''
    loParentFormSet.lcComp2n = ''
    loParentFormSet.lcCdir2  = ''
  ENDIF  
ENDIF

*--Check if copy the styles to the same company
lcExact = SET('EXACT')
SET EXACT ON
IF !EMPTY(loParentFormSet.lcComp2) AND ALLTRIM(loParentFormSet.lcComp1) = ALLTRIM(loParentFormSet.lcComp2)
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'You cannot copy the styles to the same company.')
  loParentFormSet.AriaForm1.KeyDstCmp.KeyTextBox.Value =''
  loParentFormSet.AriaForm1.KeyDstCmp.Enabled =.T.
  loParentFormSet.lcComp2  = ''
  loParentFormSet.lcComp2n = ''
  loParentFormSet.lcCdir2  = ''
  loParentFormSet.AriaForm1.txtDstCmpName.Value = ''
ENDIF
SET EXACT &lcExact
lcseason ="'  '"
lcgroup  ="'  '"
IF !EMPTY(loParentFormSet.lcComp1) .AND. !empty(loParentFormSet.lcComp2)
  IF USED('CODES')
    SELECT CODES
    USE
  ENDIF
  = gfOpenFile(loParentFormSet.lcCdir1+'CODES' ,'CODES','SH')
  loParentFormSet.AriaForm1.cboSeason.getCodes()
  loParentFormSet.AriaForm1.cboGroup.getCodes()
  loParentFormSet.AriaForm1.cmdStyle.Enabled = .T.
  loParentFormSet.AriaForm1.cboSeason.value =lcseason  
  loParentFormSet.AriaForm1.cboGroup.Value = lcgroup  
  loParentFormSet.AriaForm1.cmdProceed.Enabled = .T.
  loParentFormSet.AriaForm1.cboSeason.Enabled = .T.
  loParentFormSet.AriaForm1.cboGroup.Enabled = .T.
  loParentFormSet.AriaForm1.cmdClose.Caption = '\<Cancel '
ELSE
  loParentFormSet.AriaForm1.cboSeason.Enabled = .F.
  loParentFormSet.AriaForm1.cboGroup.Enabled = .F.
  loParentFormSet.AriaForm1.cmdProceed.Enabled = .F.
  loParentFormSet.AriaForm1.cmdClose.Caption = '\<Close '
ENDIF

*!*************************************************************
*! Name      : lfvStyRange
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/15/2014
*! Purpose   : Style range button
*!*************************************************************
FUNCTION lfvStyRange
PARAMETERS loPFormSet
#include r:\aria4xp\screens\al\alordal.h  
lcStyleTtl = gfItemMask("HI")
lcBrFields = "cSTYMajor :29 :H= 'Style',"
lcBrFields = lcBrFields+"DESC :25  :H='"+LANG_AlloByOrder_BRFLD02+"',DESC1 :25 :H='"+LANG_AlloByOrder_BRFLD03+"',"
lcBrFields = lcBrFields+"lcSesDesc=gfCodDes(Season,'SEASON'):12 :H='"+LANG_AlloByOrder_BRFLD04+"',lcDivDesc=gfCodDes(cdivision,'CDIVISION'):12:H='"+;
             LANG_AlloByOrder_BRFLD05+"',"
lcBrFields = lcBrFields+"pricea :6:h='"+LANG_AlloByOrder_BRFLD06+"',PRICEB :6:h='"+LANG_AlloByOrder_BRFLD07+"',PRICEC :6:h='"+LANG_AlloByOrder_BRFLD08+"',"
lcBrFields = lcBrFields+"totWip:7:h='"+LANG_AlloByOrder_BRFLD09+"',totstk:7:h='"+LANG_AlloByOrder_BRFLD10+"',totord:7:h='"+LANG_AlloByOrder_BRFLD11+"',OTS=(TOTWIP+TOTSTK-TOTORD):7:H='"+LANG_AlloByOrder_BRFLD13+"',"
lcBrFields = lcBrFields+"Fabric:9: h='"+LANG_AlloByOrder_BRFLD12+"',CSTYGRADE :H='"+LANG_AlloByOrder_BRFLD14+"', lcRoyDesc=gfCodDes(ROYALTY,'ROYALTY') :15 :H='"+LANG_AlloByOrder_BRFLD15+"' , PATTERN :H='"+LANG_AlloByOrder_BRFLD16+"', STATUS :H='"+LANG_AlloByOrder_BRFLD17+"',"   
lcBrFields = lcBrFields+"SCALE :H='"+LANG_AlloByOrder_BRFLD46+"',PREPAK :H='"+LANG_AlloByOrder_BRFLD19+"', CBUYPREPK :H='"+LANG_AlloByOrder_BRFLD20+;
             "', QTY_CTN :H='"+LANG_AlloByOrder_BRFLD21+"', COMMISSION :H='"+LANG_AlloByOrder_BRFLD22+;
             "', LINK_CODE :H='"+LANG_AlloByOrder_BRFLD23+"',"
lcBrFields = lcBrFields+"lcMaked = IIF(Make,'Yes','No') :H='"+LANG_AlloByOrder_BRFLD24+"', lnCst1=IIF(Make,NMCOST1,NICOST1) :H='"+LANG_AlloByOrder_BRFLD25+"',"+;
             "lnCst2=IIF(Make,NMCOST2,NICOST2) :H='"+LANG_AlloByOrder_BRFLD26+"', lnCst3=IIF(Make,NMCOST3,NICOST3) :H='"+LANG_AlloByOrder_BRFLD27+"', lnCst4=IIF(Make,NMCOST4,NICOST4) :H='"+LANG_AlloByOrder_BRFLD28+"',"
lcBrFields = lcBrFields+"lnCst5=IIF(Make,NMCOST5,NICOST5) :H='"+LANG_AlloByOrder_BRFLD29+"',TOTCOST :H='"+LANG_AlloByOrder_BRFLD30+"',"+;
             "NPRCOST2 :H='"+LANG_AlloByOrder_BRFLD31+"',NPRCOST3 :H='"+LANG_AlloByOrder_BRFLD32+"',NPRCOST4 :H='"+LANG_AlloByOrder_BRFLD33+"',NPRCOST5 :H='"+LANG_AlloByOrder_BRFLD34+"',"+;
             "AVE_COST :H='"+LANG_AlloByOrder_BRFLD35+"',NSTKVAL :H='"+LANG_AlloByOrder_BRFLD36+"',"
lcBrFields = lcBrFields+"SOLDOUT :H='"+LANG_AlloByOrder_BRFLD37+"',START :H='"+LANG_AlloByOrder_BRFLD38+"',LOCATION :H='"+LANG_AlloByOrder_BRFLD40+"',LINVSTY :H='"+LANG_AlloByOrder_BRFLD39+"',"
lcBrFields = lcBrFields+"MARKA :H='"+LANG_AlloByOrder_BRFLD41+"',MARKB:H='"+LANG_AlloByOrder_BRFLD42+"',MARKC :H='"+LANG_AlloByOrder_BRFLD43+"',CCONSINFO1 :h='"+LANG_AlloByOrder_BRFLD44+"',CCONSINFO2 :h='"+LANG_AlloByOrder_BRFLD45+"'"
*SELECT (ThisFormset.lcTmpScpSty)

lcWinTitl = 'Styles to filter on'
IF USED('STYLE')
  SELECT STYLE
  USE
ENDIF
= gfOpenFile(loPFormSet.lcCdir1+'STYLE' ,'CSTYLE','SH')
SELECT STYLE
LOCATE
llSelected = gfBrowse(lcBrFields,lcWinTitl,.F.,.F.,'',.F., .t.,.F., .F., .F.,loPFormSet.lcTmpScpSty,'CSTYMAJOR')
*!*************************************************************
*! Name   : lfvCopy
*! Auth   : Mariam Mazhar[MMT]
*! Date   : 05/15/2014.
*! Purpose:The valid function of the Proceed button.
*!*************************************************************
FUNCTION lfvCopy
PARAMETERS loFormset

*--Create the temp files.
lcTmpSty   = gfTempName ()
lcTmpCode  = gfTempName()
lcTmpScale = gfTempName()
lcSeason = ALLTRIM(loFormset.AriaForm1.cboSeason.value)
lcGroup = ALLTRIM(loFormset.AriaForm1.cboGroup.Value)
llStyleSelect = .F.
IF USED(loFormset.lcTmpScpSty)
  SELECT (loFormset.lcTmpScpSty)
  LOCATE FOR !DELETED()
  IF FOUND()
    llStyleSelect = .T.
  ENDIF
ENDIF
*-- Copy source Codes file to a temp file to be used insted
IF USED('CODES')
  SELECT CODES
  USE
ENDIF
= gfOpenFile(loFormset.lcCdir1+'CODES' ,'CODES','SH')
COPY ALL TO (oAriaApplication.WorkDir+lcTmpCode)

*-- Copy source Scale file to a temp file to be used instead
IF USED('SCALE')
  SELECT SCALE
  USE
ENDIF
=gfOpenFile(loFormset.lcCdir1+'SCALE','SCALE','SH')
COPY ALL TO (oAriaApplication.WorkDir+lcTmpScale)

=gfOpenFile(oAriaApplication.WorkDir+lcTmpCode,'','EX')
INDEX ON cdefcode+ccode_no+crltfield+cfld_name TAG &lcTmpCode

=gfOpenFile(oAriaApplication.WorkDir+lcTmpScale,'','EX')
INDEX ON TYPE+SCALE+PREPAK TAG &lcTmpScale

IF USED('STYLE')
  SELECT STYLE
  USE
ENDIF
= gfOpenFile(loFormset.lcCdir1+'STYLE' ,'cStyle','SH')
*-- Select te selected styles into a temp file 
SELECT STYLE
GO TOP
WAIT WINDOW "Collecting data" NOWAIT

SELECT  * FROM STYLE WHERE IIF(llStyleSelect,SEEK(cStyMajor,loFormset.lcTmpScpSty),.T.) .AND. IIF(!EMPTY(ALLTRIM(lcSeason)) AND lcSeason <> "'  '",SEASON=lcSeason,.T.);
  .AND. IIF(!EMPTY(ALLTRIM(lcgroup)) AND lcgroup <> "'  '",cStyGroup = lcgroup,.T.) INTO TABLE (oAriaApplication.WorkDir+lcTmpSty) ORDER BY Style
  
*-- If no styles selected , display a massage then return from the function
SELECT &lcTmpSty
LOCATE
IF EOF()
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'No Styles selected.')
  lcseason ="'  '"
  lcgroup  ="'  '"
  loFormset.AriaForm1.cboSeason.value =lcseason  
  loFormset.AriaForm1.cboGroup.Value = lcgroup  
  RETURN
ENDIF

*-- Open the distenation files "STYLE,CODES,SCALE"
IF USED('STYLE')
  SELECT STYLE
  USE
ENDIF  
=gfOpenFile(loFormset.lcCdir2+'STYLE',loFormset.lcCdir2+'STYLE','SH')
IF USED('CODES')
  SELECT CODES
  USE
ENDIF
= gfOpenFile(loFormset.lcCdir2+'CODES' ,'CODES','SH')
IF USED('SCALE')
  SELECT SCALE
  USE
ENDIF
=gfOpenFile(loFormset.lcCdir2+'SCALE','SCALE','SH')

*-- Get the Style major and non major length from the distenation company
IF USED('ICISTRU')
  SELECT ICISTRU
  USE
ENDIF
= gfOpenFile(loFormset.lcCdir2+'ICISTRU' ,'SEGNO','SH')

=SEEK('U1')
lnstylelen = ICISTRU.nisegsize
=SEEK('U2')
lnNMjrlen  = ICISTRU.nisegsize

= gfOpenFile(loFormset.lcCdir2+'WAREHOUS' ,'WAREHOUS','SH')

*-- Scan Temp style file to update the selected styles into the 
*-- files "STYLE,CODES,SCALE" in the distenation company.
llFlag = .F.
SELECT &lcTmpSty
SCAN
  Xdyelot     = CDYE_FLG
  Xpattern    = PATTERN
  Xstatus     = STATUS
  Xgroup      = CSTYGROUP  
  Xscale      = SCALE
  Xprepak     = PREPAK
  Xdivision   = CDIVISION
  Xdesc       = DESC
  Xdesc1      = DESC1
  Xseason     = SEASON
  Xweight     = NSTYWEIGHT
  Xcommision  = COMMISSION
  Xlink_code  = LINK_CODE
  Xlocation   = LOCATION
  Xcconsinfo1 = CCONSINFO1  
  Xcconsinfo2 = CCONSINFO2
  Xcontent1   = CONTENT1
  Xcontent2   = CONTENT2
  IF !SEEK(CDEFWARE,'WAREHOUS')
    GO TOP IN WAREHOUS
    Xcdefware   = WAREHOUS.cwarecode
  ELSE
    Xcdefware   = CDEFWARE
  ENDIF  
  *-- get Royalty code
  IF SEEK('N' + ROYALTY,'CODES')
    Xroyalty    = ROYALTY
  ELSE  
    lcCodOrd = ORDER('CODES')
    SET ORDER TO TAG Ccode_no IN 'CODES'
    =SEEK('D' + 'ROYALTY','CODES')
    Xroyalty    = CODES.Ccode_no
    SET ORDER TO lcCodOrd IN 'CODES'
  ENDIF  
  *-- get Purches code
  IF SEEK('N' + CPURCODE  ,'CODES')
    XPurCode    = CPURCODE  
  ELSE  
    lcCodOrd = ORDER('CODES')
    SET ORDER TO TAG Ccode_no IN 'CODES'
    =SEEK('D' + 'CPURCODE','CODES')
    XPurCode    = CODES.Ccode_no
    SET ORDER TO lcCodOrd IN 'CODES'
  ENDIF  
  *-- get Discount code
  IF SEEK('N' + CDISCCODE ,'CODES')
    XdiscCode    = CDISCCODE 
  ELSE  
    lcCodOrd = ORDER('CODES')
    SET ORDER TO TAG Ccode_no IN 'CODES'
    =SEEK('D' + 'CDISCCODE','CODES')
    XdiscCode    = CODES.Ccode_no
    SET ORDER TO lcCodOrd IN 'CODES'
  ENDIF  
  Xctaxcode   = CTAXCODE
  Xccomcode   = CCOMCODE
  Xcvensty    = CVENSTY
  Xqty_ctn    = QTY_CTN
  Xstart      = START
  Xsoldout    = SOLDOUT
  Xmake       = MAKE
  lcSty       = STYLE
  XStyMaj     = CSTYMAJOR
  *--If the style was copied before or already found in the distenation company ignor it.
  IF SEEK(lcSty,'STYLE')
  	WAIT WINDOW "STYLE " + ALLTRIM(lcsty) +" ALREADY EXIST" NOWAIT
  ELSE    && If the style is not found in the distenation company update it
    llFlag = .T.
    SELECT STYLE
    APPEND BLANK
    REPLACE STYLE      WITH lcsty      ,CDYE_FLG   WITH Xdyelot    ,PATTERN   WITH Xpattern,;
            STATUS     WITH Xstatus    ,CSTYGROUP  WITH Xgroup     ,SCALE     WITH Xscale,;
            PREPAK     WITH Xprepak    ,CDIVISION  WITH Xdivision  ,DESC      WITH Xdesc,;
            DESC1      WITH Xdesc1     ,SEASON     WITH Xseason    ,LOCATION  WITH Xlocation,;
            NSTYWEIGHT WITH XWEIGHT    ,COMMISSION WITH Xcommision ,LINK_CODE WITH Xlink_code,;
            CCONSINFO1 WITH Xcconsinfo1,CCONSINFO2 WITH Xcconsinfo2,CONTENT1  WITH Xcontent1,;
            CONTENT2   WITH Xcontent2  ,CDEFWARE   WITH Xcdefware  ,CTAXCODE  WITH Xctaxcode,;
            CCOMCODE   WITH Xccomcode  ,CVENSTY    WITH Xcvensty   ,QTY_CTN   WITH Xqty_ctn,;
            START      WITH Xstart     ,SOLDOUT    WITH Xsoldout   ,MAKE      WITH Xmake,;
            CSTYMAJOR  WITH XStyMaj    ,CDISCCODE  WITH XdiscCode  ,CPURCODE  WITH XPurCode,;
            ROYALTY    WITH Xroyalty           
 
    
    SELECT &lcTmpCode
    SEEK 'N' + SUBSTR(&lcTmpSty..STYLE,lnStylelen+2,lnNMjrlen )
    SCATTER TO laClr
    
    SEEK 'N'+&lcTmpSty..cStyGroup
    SCATTER TO laGroup
    
    SEEK 'N'+&lcTmpSty..cDivision
    SCATTER TO laDiv
    
    SEEK 'N'+&lcTmpSty..Season
    SCATTER TO laSeason

    IF SEEK('S'+&lcTmpSty..SCALE,lcTmpScale)
      SELECT &lcTmpScale
      SCATTER TO laSca
      SELECT SCALE
      IF !EMPTY(&lcTmpSty..SCALE) .AND. !SEEK('S'+PADR(&lcTmpScale..SCALE,3))
 	    APPEND BLANK
        GATHER FROM laSca
      ENDIF
    ELSE
       WAIT WINDOW 'Scale '+&lcTmpSty..SCALE+' not found,cannot copy.'  NOWAIT
    ENDIF
  
    SELECT &lcTmpScale
    IF SEEK('P'+&lcTmpSty..SCALE)
      SCAN WHILE('P'+&lcTmpScale..SCALE = 'P'+&lcTmpSty..SCALE)
        SCATTER TO laPre
        SELECT SCALE    
        IF !EMPTY(&lcTmpScale..Prepak) AND;
           !SEEK('P'+PADR(&lcTmpScale..SCALE,3)+ALLTRIM(&lcTmpScale..Prepak))
          APPEND BLANK
          GATHER FROM laPre
        ENDIF
      ENDSCAN
    ELSE
      WAIT WINDOW 'Prepak '+&lcTmpSty..Prepak+' not found,cannot copy.' NOWAIT
    ENDIF 

    SELECT CODES
    IF !EMPTY(laClr(1)) .AND. !SEEK('N'+SUBSTR(&lcTmpSty..STYLE,lnStylelen+2,lnNMjrlen)) .AND. ;
                               !SEEK('D'+SUBSTR(&lcTmpSty..STYLE,lnStylelen+2,lnNMjrlen))
      APPEND BLANK
      GATHER FROM laClr 
    ENDIF
    IF !EMPTY(laGroup(1)) .AND. !SEEK('N'+PADR(&lcTmpSty..CSTYGROUP,6)) .AND. ;
                               !SEEK('D'+PADR(&lcTmpSty..CSTYGROUP,6))
      APPEND BLANK
      GATHER FROM laGroup 
    ENDIF 		
    IF !EMPTY(laDiv(1)) .AND. !SEEK('N'+PADR(&lcTmpSty..cDivision,6)) .AND. ;
                               !SEEK('D'+PADR(&lcTmpSty..cDivision,6))
      APPEND BLANK
      GATHER FROM laDiv 
    ENDIF
    IF !EMPTY(laSeason(1)) .AND. !SEEK('N'+PADR(&lcTmpSty..Season,6)) .AND. ;
                               !SEEK('D'+PADR(&lcTmpSty..Season,6))
      APPEND BLANK   
      GATHER FROM laSeason 
    ENDIF
  ENDIF

  SELECT &lcTmpSty
ENDSCAN

WAIT CLEAR
IF llFlag = .T.
  =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,'The styles are successfully copied.')
ENDIF  


loFormset.AriaForm1.KeySrcCmp.KeyTextBox.Value = ''
loFormset.lcComp1   = ''
loFormset.AriaForm1.txtSrcCmpName.Value = '' 
loFormset.lcComp1n =''

loFormset.AriaForm1.KeyDstCmp.KeyTextBox.Value = ''
loFormset.lcComp2   = ''
loFormset.AriaForm1.txtDstCmpName.Value = '' 
loFormset.lcComp2n =''

IF llStyleSelect
  SELECT(loFormset.lcTmpScpSty)
  ZAP
ENDIF
lcseason ="'  '"
lcgroup  ="'  '"
loFormset.AriaForm1.cboSeason.value =lcseason  
loFormset.AriaForm1.cboGroup.Value = lcgroup  

STORE '' TO loFormset.lcCdir1,loFormset.lcCdir2
loFormset.ChangeMode('S')
