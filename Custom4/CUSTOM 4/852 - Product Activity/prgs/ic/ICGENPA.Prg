***********************************************************************
*:   Program file: ICGENPA.PRG
*:  Program desc.: Generate Product Activity 
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar (MMT)
*:           Date: 09/27/2018
*:      Reference: E304035,1 {P20171120.0011}
*:************************************************************************
#INCLUDE R:\ARIA4XP\PRgs\IC\ICGENPA.h

DO FORM (oAriaApplication.ScreenHome+"\IC\ICGENPAC.SCX")
RETURN 

*!*************************************************************
*! Name      : lfInitFormSet
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : the Init Method of the formSet
*!*************************************************************
FUNCTION lfInitFormSet
LPARAMETERS loFormSet
SET MULTILOCKS ON 
=gfOpenTable('PRODUCT_ACTIVITY','PRDACT_HDR')
IF 'AS' $ oAriaApplication.CompanyInstalledModules 
  =gfOpenTable('EDIACPRT','ACCFACT','SH')
  =gfOpenTable('EDIPD','PARTTRANS','SH')
  =gfOpenTable('EDITRANS','TYPEKEY','SH')
ENDIF  
WITH loFormSet
  .nWorkArea        = 'PRODUCT_ACTIVITY'
  .cBrowseAliasName= 'PRODUCT_ACTIVITY'
  .cBrowseTableName= 'PRODUCT_ACTIVITY'
  .DataEnvironment.InitialSelectedAlias = 'PRODUCT_ACTIVITY'
  .cbrowsetabledbengine   = 'SQL'
  .cBrowseIndexName ='PRDACT_HDR'
  .cBrowseIndexExpression ='ACTIVITYID'
  .cBrowseIndexFields ='ACTIVITYID'
  .cBrowseFileName        = "PRODUCT_ACTIVITY"
 
  .AriaBrFields.edtBrowseFields.value   =  [ActivityID :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACTIVITY_ID,this.loFormSet.GetHeaderText("LANG_ACTIVITY_ID",this.loFormSet.HeaderAlias))+;
               [' , ACCOUNT :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account,this.loFormSet.GetHeaderText("LANG_Account",this.loFormSet.HeaderAlias))+;
   [', DESC :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESC,this.loFormSet.GetHeaderText("LANG_DESC",this.loFormSet.HeaderAlias))+[' ,] +;
   [DATE_FROM :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DATE_FROM,this.loFormSet.GetHeaderText("LANG_DATE_FROM",this.loFormSet.HeaderAlias))+;
   [' , DATE_TO :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DATE_TO,this.loFormSet.GetHeaderText("LANG_DATE_TO",this.loFormSet.HeaderAlias))+[', ] +;
   [lnSt = IIF(cStatus='N','Yes','No') :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SEND,this.loFormSet.GetHeaderText("LANG_SEND",this.loFormSet.HeaderAlias))+[']
  DECLARE .lapanelobj [1,6]
  .laPanelObj[1,1] = "cmdScope"
  .laPanelObj[1,2] = oAriaApplication.BitMapHome+ 'SCOPE.BMP'
  .laPanelObj[1,3] = 'mcallscope' 
  .laPanelObj[1,4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SCOPE,oAriaApplication.GetHeaderText("LANG_SCOPE",AHEADERFILE))
  .laPanelObj[1,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SCOPE,oAriaApplication.GetHeaderText("LANG_SCOPE",AHEADERFILE))
  .laPanelObj[1,6] = 'AVE'
   
ENDWITH
loFormSet.AddProperty('lcRpQtyTyp','QR,QH,QS,QO')
loFormSet.AddProperty('lcCriteria',gfTempName())
loFormSet.AddProperty('lcCriteriaDBF',gfTempName())
loFormSet.AddProperty('lcStyXml',gfTempName())

CREATE CURSOR (loFormSet.lcCriteria) (cFld_Name C(10),Criteria M(4))
SELECT (loFormSet.lcCriteria) 
INDEX on cFld_Name TAG (loFormSet.lcCriteria)

APPEND BLANK 
REPLACE cFld_Name WITH 'LCRPQTYTYP',;
        Criteria WITH  loFormSet.lcRpQtyTyp
APPEND BLANK 
REPLACE cFld_Name WITH 'STYLE',;
        Criteria WITH  ''
        
APPEND BLANK 
REPLACE cFld_Name WITH 'COLOR',;
        Criteria WITH  ''
        
APPEND BLANK 
REPLACE cFld_Name WITH 'CDIVISION',;
        Criteria WITH  ''
        
APPEND BLANK 
REPLACE cFld_Name WITH 'CSTYGROUP',;
        Criteria WITH  ''
        
APPEND BLANK 
REPLACE cFld_Name WITH 'SEASON',;
        Criteria WITH  ''
        
*!*************************************************************
*! Name      : lfChangeScrMode
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : Change Mode method
*!*************************************************************
FUNCTION lfChangeScrMode
LPARAMETERS loFormSet,lcMode

DO CASE 

  CASE lcMode ='A'
    loFormSet.AriaForm1.KeyActID.Enabled = .F.
    loFormSet.AriaForm1.txtDesc.Enabled = .T.
    loFormSet.AriaForm1.KeyAccount.Enabled = .T.
    loFormSet.AriaForm1.txtAccName.Value = ''
    loFormSet.AriaForm1.dtpFrom.Enabled = .T.
    loFormSet.AriaForm1.DtpTo.Enabled = .T.    
    loFormSet.AriaForm1.chkSend.Enabled = .T.   
    loFormSet.AriaForm1.KeyLoc.Enabled = .T. 
    loFormSet.AriaForm1.KeyActID.keytextbox.Value = ''
    loFormSet.AriaForm1.txtDesc.Value =''
    loFormSet.AriaForm1.KeyAccount.keytextbox.Value = ''
    loFormSet.AriaForm1.dtpFrom.Value = {}
    loFormSet.AriaForm1.DtpTo.Value = {}
    loFormSet.AriaForm1.chkSend.Value = .F.
    loFormSet.AriaForm1.KeyLoc.keytextbox.Value = ''
    loFormSet.lcRpQtyTyp = 'QR,QH,QS,QO'
    =SEEK('STYLE',loFormSet.lcCriteria,loFormSet.lcCriteria)
    REPLACE Criteria WITH '' IN (loFormSet.lcCriteria)
    =SEEK('COLOR',loFormSet.lcCriteria,loFormSet.lcCriteria)
    REPLACE Criteria WITH '' IN (loFormSet.lcCriteria) 
    =SEEK('SEASON',loFormSet.lcCriteria,loFormSet.lcCriteria)
    REPLACE Criteria WITH '' IN (loFormSet.lcCriteria)        
    =SEEK('CDIVISION',loFormSet.lcCriteria,loFormSet.lcCriteria)
    REPLACE Criteria WITH '' IN (loFormSet.lcCriteria)  
    =SEEK('CSTYGROUP',loFormSet.lcCriteria,loFormSet.lcCriteria)
    REPLACE Criteria WITH '' IN (loFormSet.lcCriteria)  
    =SEEK('LCRPQTYTYP',loFormSet.lcCriteria,loFormSet.lcCriteria)
    REPLACE Criteria WITH  loFormSet.lcRpQtyTyp IN (loFormSet.lcCriteria)
    
  CASE lcMode ='E'
    loFormSet.AriaForm1.KeyActID.Enabled = .F.
    loFormSet.AriaForm1.txtDesc.Enabled = .T.
    loFormSet.AriaForm1.KeyAccount.Enabled = .T.
    loFormSet.AriaForm1.dtpFrom.Enabled = .T.
    loFormSet.AriaForm1.DtpTo.Enabled = .T.    
    loFormSet.AriaForm1.chkSend.Enabled = .T.  
    loFormSet.AriaForm1.KeyLoc.Enabled = .T.  
    loFormSet.AriaForm1.chkSend.Value = .F. 

  CASE lcMode ='S'
    loFormSet.AriaForm1.KeyActID.Enabled = .T.
    loFormSet.AriaForm1.txtDesc.Enabled = .F.
    loFormSet.AriaForm1.KeyAccount.Enabled = .T.
    loFormSet.AriaForm1.dtpFrom.Enabled = .F.
    loFormSet.AriaForm1.DtpTo.Enabled = .F.    
    loFormSet.AriaForm1.chkSend.Enabled = .F. 
    loFormSet.AriaForm1.KeyLoc.Enabled = .F. 
    loFormSet.AriaForm1.KeyActID.keytextbox.Value = ''
    loFormSet.AriaForm1.txtDesc.Value =''
    loFormSet.AriaForm1.KeyAccount.keytextbox.Value = ''
    loFormSet.AriaForm1.dtpFrom.Value = {}
    loFormSet.AriaForm1.DtpTo.Value = {}
    loFormSet.AriaForm1.chkSend.Value = .F.
    loFormSet.AriaForm1.KeyLoc.keytextbox.Value = ''
           
  CASE lcMode ='V'
    loFormSet.AriaForm1.KeyActID.Enabled = .F.
    loFormSet.AriaForm1.txtDesc.Enabled = .F.
    loFormSet.AriaForm1.KeyAccount.Enabled = .F.
    loFormSet.AriaForm1.dtpFrom.Enabled = .F.
    loFormSet.AriaForm1.DtpTo.Enabled = .F.    
    loFormSet.AriaForm1.chkSend.Enabled = .F.    
    loFormSet.AriaForm1.KeyLoc.Enabled = .F. 
    loFormSet.AriaForm1.chkSend.Value = .F.
    lfGetPAData(loFormSet)
ENDCASE   

*!*************************************************************
*! Name      : lfCallScopeOG
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : Call Option grid of Scope
*!*************************************************************
FUNCTION lfCallScopeOG
PARAMETERS loFormSet

lcRpQtyTyp = loFormSet.lcRpQtyTyp
DIMENSION laogFxFlt[1,7]
DIMENSION laogVrFlt[1,7]
laogVrFlt =''
laogFxFlt =''
SELECT (loFormSet.lcCriteria)
COPY TO (oAriaApplication.WorkDir+loFormSet.lcCriteriaDBF+'.DBF') 
*!*	LNRPQTYTYP = 0
*!*	DIMENSION laRpSource[1],laRpTarget[1]
*!*	laRpSource =''
*!*	laRpTarget = ''
SET STEP ON 

lcExpr = gfOpGrid('ICGNPDAC' , .T.)
*lcExpr = gfOpGrid('ICGNPDAC',.T.,.F.,.F.,.T.,.T.)
*XX
IF loFormSet.ActiveMode $ 'SV'
  RETURN
ENDIF
*XX

SET STEP ON 
IF EMPTY(loFormSet.lcRpQtyTyp)
*!*	  loFormSet.lcRpQtyTyp = lcRpQtyTyp 
*!*	ELSE
  loFormSet.lcRpQtyTyp = 'QR,QH,QS,QO'
ENDIF
SELECT (loFormSet.lcCriteria)
=SEEK('LCRPQTYTYP',loFormSet.lcCriteria,loFormSet.lcCriteria)
REPLACE Criteria WITH  loFormSet.lcRpQtyTyp IN (loFormSet.lcCriteria)

IF !Empty(laogFxFlt[1,1]) OR !EMPTY(laogVrFlt[1,1])
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
  llUseStyle = .F.
  lcStylFile = ''
  IF lnPosition > 0
    lcStylFile = laOGFxFlt[lnPosition,6]
    llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
  ENDIF
  =SEEK('STYLE',loFormSet.lcCriteria,loFormSet.lcCriteria)
  IF llUseStyle 
    SELECT(lcStylFile)
    LOCATE FOR !DELETED()
    IF !EOF()
      SELECT * FROM (lcStylFile) WHERE !DELETED() AND !(ALLTRIM(keyExp)== 'F') INTO CURSOR 'TmpStyCur'
      =CURSORTOXML('TmpStyCur', oAriaApplication.WorkDir+loFormSet.lcStyXml+".XML", 1, 512, 0, "1")
      IF FILE(oAriaApplication.WorkDir+loFormSet.lcStyXml+".XML")
        REPLACE Criteria WITH FILETOSTR(oAriaApplication.WorkDir+loFormSet.lcStyXml+".XML") IN (loFormSet.lcCriteria)
      ELSE   
        REPLACE Criteria WITH '' IN (loFormSet.lcCriteria)
      ENDIf  
    ELSE
      REPLACE Criteria WITH '' IN (loFormSet.lcCriteria)
    ENDIF
  ELSE
    REPLACE Criteria WITH '' IN (loFormSet.lcCriteria)
  ENDIF 


  *--Style Color
  lnClrPos = ASUBSCRIPT(laOGFXFlt,ASCAN(laOGFXFlt,'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)'),1)
  IF lnClrPos > 0
    =SEEK('COLOR',loFormSet.lcCriteria,loFormSet.lcCriteria)
    IF !EMPTY(laOGFXFlt[lnClrPos,6])
      REPLACE Criteria WITH  laOGFXFlt[lnClrPos,6] IN (loFormSet.lcCriteria)
    ELSE
      REPLACE Criteria WITH '' IN (loFormSet.lcCriteria) 
    ENDIF
  ENDIF

  *---Season Filter
  lnSeaPos = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,'STYLE.SEASON'),1)
  IF lnSeaPos > 0
    =SEEK('SEASON',loFormSet.lcCriteria,loFormSet.lcCriteria)
    IF !EMPTY(laOGVRFlt[lnSeaPos,6])
      REPLACE Criteria WITH laOGVRFlt[lnSeaPos,6] IN (loFormSet.lcCriteria)
	ELSE
      REPLACE Criteria WITH '' IN (loFormSet.lcCriteria)        
    ENDIF
  ENDIF
  
  *---Division Filter
  lnDivPos = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,'STYLE.CDIVISION'),1)
  IF lnDivPos  > 0
    =SEEK('CDIVISION',loFormSet.lcCriteria,loFormSet.lcCriteria)
    IF !EMPTY(laOGVRFlt[lnDivPos,6])
      REPLACE Criteria WITH laOGVRFlt[lnDivPos,6] IN (loFormSet.lcCriteria)
    ELSE
      REPLACE Criteria WITH '' IN (loFormSet.lcCriteria)  
    ENDIF
  ENDIF
  

  
  *--Style Group
  lnGrpPos = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,'STYLE.CSTYGROUP'),1)
  IF lnGrpPos > 0
    =SEEK('CSTYGROUP',loFormSet.lcCriteria,loFormSet.lcCriteria)
    IF !EMPTY(laOGVRFlt[lnGrpPos,6])
      REPLACE Criteria WITH laOGVRFlt[lnGrpPos,6] IN (loFormSet.lcCriteria)
    ELSE
      REPLACE Criteria WITH '' IN (loFormSet.lcCriteria)  
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
  lnMajSeg    = gfItemMask('SM')  && No. of major segments.
  *-- Compute Free/Color Items in Style code Structure. [Begin]
  DIMENSION laMajSegs[1,1]
  = gfItemMask(@laMajSegs)

  *-- Loop Around Non Major elements.
  FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
    IF laMajSegs[lnI,1] $ 'CF'
      lcFree_Clr = laMajSegs[lnI,1]

      lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
       
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
  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's'
  *-- Compute Free/Color Items in Style code Structure. [End]
RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mariam Mazhar(MMT)
*! Date      : 09/27/2018
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
    lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
    lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
    lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MAB)
*! Date      : 09/27/2018
*! Purpose   : When function of OG
*!*************************************************************
*! Called from : OG read cycle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*
FUNCTION lfwRepWhen

lcRpQtyTyp = IIF(EMPTY(lcRpQtyTyp),'QR,QH,QS,QO',LEFT(ALLTRIM(lcRpQtyTyp),LEN(ALLTRIM(lcRpQtyTyp))-1))
IF !EMPTY(loFormSet.lcRpQtyTyp)
  lcRpQtyTyp = loFormSet.lcRpQtyTyp
ENDIF

IF EMPTY(laRpSource)
  DECLARE laRpSource[4],laRpTarget[4]  && Redeclare the source and target arrays.
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_ONHAND,oAriaApplication.GetHeaderText("LANG_QTY_ONHAND",AHEADERFILE))       TO laRpSource[1]
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_SOLD,oAriaApplication.GetHeaderText("LANG_QTY_SOLD",AHEADERFILE))      TO laRpSource[2]
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_RETURNED,oAriaApplication.GetHeaderText("LANG_QTY_RETURNED",AHEADERFILE))      TO laRpSource[3]
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_ORDER,oAriaApplication.GetHeaderText("LANG_QTY_ORDER",AHEADERFILE))      TO laRpSource[4]
ENDIF
DIMENSION laTargArr[1]
laTargArr =''
=gfSubstr(lcRpQtyTyp ,@laTargArr,",")
FOR lnA= 1 TO ALEN(laTargArr,1)
  DIMENSION laRpTarget[lnA]
  laRpTarget[lnA] = IIF(laTargArr[lnA] ='QH',LANG_QTY_ONHAND,IIF(laTargArr[lnA] ='QO',LANG_QTY_ORDER,IIF(laTargArr[lnA] ='QS',LANG_QTY_SOLD,IIF(laTargArr[lnA] ='QR',LANG_QTY_RETURNED,''))))
ENDFOR

SET STEP ON 
LOCAL lnCount
IF !EMPTY(laRpTarget[1])
  lcRpQtyTyp = ' '
  FOR lnCount = 1 TO ALEN(laRpTarget,1)
    lcRpQtyTyp = lcRpQtyTyp + IIF(laRpTarget[lnCount] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_ONHAND,oAriaApplication.GetHeaderText("LANG_QTY_ONHAND",AHEADERFILE)),'QH,',;
                              IIF(laRpTarget[lnCount] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_SOLD,oAriaApplication.GetHeaderText("LANG_QTY_SOLD",AHEADERFILE)),'QS,',;
                              IIF(laRpTarget[lnCount] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_RETURNED,oAriaApplication.GetHeaderText("LANG_QTY_RETURNED",AHEADERFILE)),'QR,',;
                              IIF(laRpTarget[lnCount] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_ORDER,oAriaApplication.GetHeaderText("LANG_QTY_ORDER",AHEADERFILE)),'QO,',''))))

  ENDFOR
ENDIF
SET STEP ON 

IF FILE(oAriaApplication.WorkDir+loFormSet.lcCriteriaDBF+'.DBF') 
  USE (oAriaApplication.WorkDir+loFormSet.lcCriteriaDBF+'.DBF') IN 0 EXCLUSIVE 
  SELECT (loFormSet.lcCriteriaDBF)
  INDEX on cfld_Name TAG (loFormSet.lcCriteriaDBF)
  
  =SEEK('LCRPQTYTYP',loFormSet.lcCriteriaDBF,loFormSet.lcCriteriaDBF)
  lcRpQtyTyp = EVALUATE(loFormSet.lcCriteriaDBF+".Criteria")
  
  IF SEEK('CDIVISION',loFormSet.lcCriteriaDBF,loFormSet.lcCriteriaDBF)
    IF !EMPTY(EVALUATE(loFormSet.lcCriteriaDBF+".Criteria"))
      *---Division Filter
      lnDivPos = ASUBSCRIPT(loogscroll.laOGVRFlt,ASCAN(loogscroll.laOGVRFlt,'STYLE.CDIVISION'),1)
      IF lnDivPos  > 0
        loogscroll.laOGVRFlt[lnDivPos,6] = EVALUATE(loFormSet.lcCriteriaDBF+".Criteria")
      ENDIF
    ENDIF
  ENDIF

  IF SEEK('COLOR',loFormSet.lcCriteriaDBF,loFormSet.lcCriteriaDBF)
    *--Style Color
    IF !EMPTY(EVALUATE(loFormSet.lcCriteriaDBF+".Criteria"))
      lnClrPos = ASUBSCRIPT(laOGFXFlt,ASCAN(loogscroll.laOGFXFlt,'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)'),1)
      IF lnClrPos > 0
        loogscroll.laOGFXFlt[lnClrPos,6] = EVALUATE(loFormSet.lcCriteriaDBF+".Criteria")
      ENDIF
    ENDIF  
  ENDIF 
   
  IF SEEK('SEASON',loFormSet.lcCriteriaDBF,loFormSet.lcCriteriaDBF)
    *---Season Filter
    IF !EMPTY(EVALUATE(loFormSet.lcCriteriaDBF+".Criteria"))
      lnSeaPos = ASUBSCRIPT(loogscroll.laOGVRFlt,ASCAN(loogscroll.laOGVRFlt,'STYLE.SEASON'),1)
      IF lnSeaPos > 0
        loogscroll.laOGVRFlt[lnSeaPos,6]=  EVALUATE(loFormSet.lcCriteriaDBF+".Criteria")
      ENDIF  
    ENDIF  
  ENDIF

  IF SEEK('CSTYGROUP',loFormSet.lcCriteriaDBF,loFormSet.lcCriteriaDBF)
    *--Style Group
    IF !EMPTY(EVALUATE(loFormSet.lcCriteriaDBF+".Criteria"))
      lnGrpPos = ASUBSCRIPT(loogscroll.laOGVRFlt,ASCAN(loogscroll.laOGVRFlt,'STYLE.CSTYGROUP'),1)
      IF lnGrpPos > 0
        loogscroll.laOGVRFlt[lnGrpPos,6] = EVALUATE(loFormSet.lcCriteriaDBF+".Criteria")
      ENDIF  
    ENDIF
  ENDIF  
  
  
  IF SEEK('STYLE',loFormSet.lcCriteriaDBF,loFormSet.lcCriteriaDBF)
    IF !EMPTY(EVALUATE(loFormSet.lcCriteriaDBF+".Criteria"))
      lnPosition = ASUBSCRIPT(loogscroll.laOGFxFlt,ASCAN(loogscroll.laOGFxFlt,'STYLE.CSTYMAJOR'),1)
      lcFileOutTmp = oAriaApplication.WorkDir+loFormSet.lcStyXml+'.XML'
      =STRTOFILE(EVALUATE(loFormSet.lcCriteriaDBF+".Criteria") ,lcFileOutTmp)
      IF FILE(lcFileOutTmp)
        =XMLTOCURSOR(lcFileOutTmp,loFormSet.lcStyXml,512)
      ENDIF  
      IF USED(loFormSet.lcStyXml)
        SELECT (loFormSet.lcStyXml)
        DELETE FOR EMPTY(KEYEXP)
        INDEX ON KEYEXP TAG (loFormSet.lcStyXml)
        loogscroll.laOGFxFlt[lnPosition,6] = loFormSet.lcStyXml
      ENDIF
   ENDIF   
  ENDIF  
    
ENDIF

*!*************************************************************
*! Name      : lfvOQtyMover
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : Qty Type Mover
*!*************************************************************
FUNCTION lfvOQtyMover
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
=gfMover(@laRpSource,@laRpTarget,LANG_SEL_QTY_TYPE,.T.,'')  && call mover function.

lcRpQtyTyp = ' '
*-- Loop to make Status expression.

IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpQtyTyp = lcRpQtyTyp + IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_SOLD,oAriaApplication.GetHeaderText("LANG_QTY_SOLD",AHEADERFILE)),'QS,',;
                              IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_ONHAND,oAriaApplication.GetHeaderText("LANG_QTY_ONHAND",AHEADERFILE)),'QH,',;
                              IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_RETURNED,oAriaApplication.GetHeaderText("LANG_QTY_RETURNED",AHEADERFILE)),'QR,',;
                              IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QTY_ORDER,oAriaApplication.GetHeaderText("LANG_QTY_ORDER",AHEADERFILE)),'QO,',''))))
  ENDFOR  && end Loop to make Status expression.
ENDIF

*lcRpQtyTyp = IIF(EMPTY(lcRpQtyTyp),'QSQHQRQO',ALLTRIM(lcRpQtyTyp))
lcRpQtyTyp = IIF(EMPTY(lcRpQtyTyp ),'QR,QH,QS,QO',LEFT(ALLTRIM(lcRpQtyTyp),LEN(ALLTRIM(lcRpQtyTyp))-1))

*!*************************************************************
*! Name      : RefreshStatus
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : Return the selected qty type in the ReadBox
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!***************************************************************************
*! Modification:
*!***************************************************************************


FUNCTION RefreshQty
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF
  RETURN lcStatusStr
ENDFUNC

*!*************************************************************
*! Name      : lfvOrdWare
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/27/2018
*! Purpose   : Valid function of the Warehouse
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
FUNCTION lfvWareVld
LPARAMETERS lcWareHouse,llFromBrow  

IF !USED('WAREHOUS')
  =gfOpenTable('WAREHOUS','WAREHOUS')
ENDIF
*-- IF The user want to Browse or if the Warehouse he entered is not in the file
IF '?' $ lcWareHouse .OR. (!EMPTY(lcWareHouse) .AND. !gfSEEK(lcWareHouse, 'WAREHOUS')) OR llFromBrow  
  lcWareHouse = gfBrowWare(.T.)
  lcWareHouse = IIF(EMPTY(lcWareHouse) , '' , lcWareHouse)
  *&lcObjName. = lcObjVal
ENDIF    && End of IF
RETURN lcWareHouse 

*!*************************************************************
*! Name      : lfvAccCode
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : validate Account code
*!*************************************************************
FUNCTION lfvAccCode
LPARAMETERS loFormSet
xAccount = loFormSet.AriaForm1.KeyAccount.keytextbox.Value
IF !USED('CUSTOMER')
  =gfOpenTable('CUSTOMER','CUSTOMER')
ENDIF
IF loFormSet.AriaForm1.KeyAccount.SelectedFromBrowse .OR. (!EMPTY(xAccount) .AND. !gfSEEK('M'+xAccount,'CUSTOMER'))
  DO CUSBROWM WITH xAccount
  IF !EmPty(xAccount)
    loFormSet.AriaForm1.KeyAccount.keytextbox.Value = xAccount
  ENDIF
ENDIF
loFormSet.AriaForm1.KeyAccount.SelectedFromBrowse = .F.
IF !EMPTY( loFormSet.AriaForm1.KeyAccount.keytextbox.Value)
   loFormSet.AriaForm1.txtAccName.Value = Customer.stname
ENDIF

*!*************************************************************
*! Name      : lfBefActSave
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : Before Save function
*!*************************************************************
FUNCTION lfBefActSave
LPARAMETERS loFormSet
  xAccount = loFormSet.AriaForm1.KeyAccount.keytextbox.Value
  IF EMPTY(xAccount)
    =gfModalGen("INM000000B00000","DIALOG",'','',;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECT_ACCOUNT ,loFormSet.GetHeaderText("LANG_SELECT_ACCOUNT",loFormSet.HeaderAlias)))
    RETURN .F. 
  ENDIF
  IF EMPTY(loFormSet.AriaForm1.dtpFrom.Value) AND EMPTY(loFormSet.AriaForm1.DtpTo.Value) OR (loFormSet.AriaForm1.dtpFrom.Value >  loFormSet.AriaForm1.DtpTo.Value)
    =gfModalGen("INM000000B00000","DIALOG",'','',;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECT_DATE_RANGE,loFormSet.GetHeaderText("LANG_SELECT_DATE_RANGE",loFormSet.HeaderAlias)))
    RETURN .F. 
  ENDIF
  xLoc = loFormSet.AriaForm1.KeyLoc.keytextbox.Value
  IF EMPTY(xLoc)
    =gfModalGen("INM000000B00000","DIALOG",'','',;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECT_LOC,loFormSet.GetHeaderText("LANG_SELECT_LOC",loFormSet.HeaderAlias)))
    RETURN .F. 
  ENDIF
 
*!*************************************************************
*! Name      : lfSaveActivity  
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : Save function
*!*************************************************************  
FUNCTION lfSaveActivity  
LPARAMETERS loFormSet 
IF !USED('PRODUCT_ACTIVITY')
  =gfOpenTable('PRODUCT_ACTIVITY','PRDACT_HDR')
ENDIF


lcACTIVITYID = ''
IF loFormSet.ActiveMode ='A'  
    SELECT PRODUCT_ACTIVITY
    LOCATE FOR ActivityID = SPACE(10)
    IF !FOUND()
      APPEND BLANK 
    ENDIF   
    lcACTIVITYID = PADL(ALLTRIM(gfSequence('ACTIVITYID')),10,'0')
    REPLACE ACTIVITYID WITH lcACTIVITYID
    REPLACE Desc WITH loFormSet.AriaForm1.txtDesc.Value,;
            DATE_FROM WITH loFormSet.AriaForm1.dtpFrom.Value,;
            DATE_TO WITH loFormSet.AriaForm1.DtpTo.Value,;
            cStatus WITH IIF(loFormSet.AriaForm1.chkSend.Value,'N','S'),;
            Account WITH loFormSet.AriaForm1.KeyAccount.keytextbox.Value,;
            cWarecode WITH loFormSet.AriaForm1.KeyLoc.keytextbox.Value
            
    =gfAdd_Info('PRODUCT_ACTIVITY',loFormSet)            
    =gfReplace('')
    =CURSORTOXML(loFormSet.lcCriteria, oAriaApplication.WorkDir+loFormSet.lcCriteria+".XML", 1, 512, 0, "1")
    IF FILE(oAriaApplication.WorkDir+loFormSet.lcCriteria+".XML")
      lcFileName = oAriaApplication.WorkDir+loFormSet.lcCriteria+'.XML'
      =gfReplace("OPTION_GRID_CRITERIA WITH FILETOSTR(lcFileName)")
    ENDIF  
ELSE
  lcACTIVITYID = loFormSet.AriaForm1.KeyActID.keytextbox.Value
  IF gfSeek(PADL(lcACTIVITYID ,10,'0'),'PRODUCT_ACTIVITY','PRDACT_HDR')
    SELECT PRODUCT_ACTIVITY
    REPLACE Desc WITH loFormSet.AriaForm1.txtDesc.Value,;
            DATE_FROM WITH loFormSet.AriaForm1.dtpFrom.Value,;
            DATE_TO WITH loFormSet.AriaForm1.DtpTo.Value,;
            cStatus WITH IIF(loFormSet.AriaForm1.chkSend.Value,'N','S'),;
            Account WITH loFormSet.AriaForm1.KeyAccount.keytextbox.Value,;
            cWarecode WITH loFormSet.AriaForm1.KeyLoc.keytextbox.Value
            
    =gfAdd_Info('PRODUCT_ACTIVITY',loFormSet)                        
    =gfReplace('')
    =CURSORTOXML(loFormSet.lcCriteria, oAriaApplication.WorkDir+loFormSet.lcCriteria+".XML", 1, 512, 0, "1")
    IF FILE(oAriaApplication.WorkDir+loFormSet.lcCriteria+".XML")
      lcFileName = oAriaApplication.WorkDir+loFormSet.lcCriteria+'.XML'
      =gfReplace("OPTION_GRID_CRITERIA WITH FILETOSTR(lcFileName)")
    ENDIF  
  ENDIF
ENDIF  
SELECT PRODUCT_ACTIVITY
=gfTableUpdate()
IF loFormSet.AriaForm1.chkSend.Value  AND 'AS' $ oAriaApplication.CompanyInstalledModules 
  IF  gfSeek('M'+loFormSet.AriaForm1.KeyAccount.keytextbox.Value,'Customer','Customer') And;
    gfSeek('A'+Customer.Account,'EDIACPRT','ACCFACT') And gfSEEK(EDIACPRT.cPartCode+'852','EDIPD','PARTTRANS')
    Select EDITRANS
    If !gfSeek('852'+Padr(lcACTIVITYID ,40)+'A'+Customer.Account,'EDITRANS','TYPEKEY')
      Insert Into 'EDITRANS' (CEDITRNTYP,Key,Type,CPARTNER,lInterComp) Values ;
        ('852',lcACTIVITYID ,'A',Customer.Account,EDIACPRT.lInterComp)
    ENDIF
    Replace cStatus With 'N'
    =gfAdd_Info('EDITRANS')
    =gfReplace('')
    =gfTableUpdate()
  Endif
ENDIF 
IF loFormSet.ActiveMode ='A'  
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECT_PROD_ACT,loFormSet.GetHeaderText("LANG_SELECT_PROD_ACT",loFormSet.HeaderAlias))+;
                                               lcACTIVITYID +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECT_PROD_CREATED,loFormSet.GetHeaderText("LANG_SELECT_PROD_CREATED",loFormSet.HeaderAlias)))
ENDIF
  
*!*************************************************************
*! Name      : lfvActivityID
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : Validate Activity# function
*!*************************************************************  
FUNCTION lfvActivityID
LPARAMETERS loFormSet 
lcActID = loFormSet.AriaForm1.KeyActID.keytextbox.Value
IF '?' $ lcActID  OR loFormSet.AriaForm1.KeyActID.selectedfrombrowse .OR. (!EMPTY(lcActID) .AND. !gfSEEK(lcActID , 'PRODUCT_ACTIVITY'))
  SELECT 'PRODUCT_ACTIVITY'
  =gfSeek('')
  lcBrFields = [ActivityID :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACTIVITY_ID,this.loFormSet.GetHeaderText("LANG_ACTIVITY_ID",this.loFormSet.HeaderAlias))+;
               [' , ACCOUNT :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account,this.loFormSet.GetHeaderText("LANG_Account",this.loFormSet.HeaderAlias))+;
   [', DESC :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESC,this.loFormSet.GetHeaderText("LANG_DESC",this.loFormSet.HeaderAlias))+[' ,] +;
   [DATE_FROM :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DATE_FROM,this.loFormSet.GetHeaderText("LANG_DATE_FROM",this.loFormSet.HeaderAlias))+;
   [' , DATE_TO :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DATE_TO,this.loFormSet.GetHeaderText("LANG_DATE_TO",this.loFormSet.HeaderAlias))+[', ] +;
   [lnSt = IIF(cStatus='N','Yes','No') :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SEND,this.loFormSet.GetHeaderText("LANG_SEND",this.loFormSet.HeaderAlias))+[']
   
   lcActID = IIF(ARIABROW('','Product Activity',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
                         gnBrFSCol2,'','','PRODUCT_ACTIVITY','laBrowArr', .F.,'',.F.,'PRODUCT_ACTIVITY','','','PRDACT_HDR'),PRODUCT_ACTIVITY.ActivityID ,SPACE(10))                         
                         
                         
   loFormSet.AriaForm1.KeyActID.keytextbox.Value = lcActID 
   loFormSet.AriaForm1.KeyActID.selectedfrombrowse = .F.
ENDIF    && End of IF
IF !EMPTY(loFormSet.AriaForm1.KeyActID.keytextbox.Value)
  loFormSet.ChangeMode ('V')
  
ENDIF

*!*************************************************************
*! Name      : lfGetPAData
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : Get Activity# data function
*!*************************************************************  
FUNCTION lfGetPAData
LPARAMETERS loFormSet

IF !USED('CUSTOMER')
  =gfOpenTable('CUSTOMER','CUSTOMER')
ENDIF
loFormSet.AriaForm1.KeyActID.keytextbox.Value =PRODUCT_ACTIVITY.ACTIVITYID
loFormSet.AriaForm1.KeyAccount.keytextbox.Value = PRODUCT_ACTIVITY.Account
=gfSeek('M'+PRODUCT_ACTIVITY.Account,'CUSTOMER','CUSTOMER')
loFormSet.AriaForm1.txtAccName.Value = Customer.stname
loFormSet.AriaForm1.txtDesc.Value = PRODUCT_ACTIVITY.Desc
    loFormSet.AriaForm1.dtpFrom.Value = PRODUCT_ACTIVITY.DATE_FROM
    loFormSet.AriaForm1.DtpTo.Value = PRODUCT_ACTIVITY.DATE_TO
*loFormSet.AriaForm1.chkSend.Value = IIF(PRODUCT_ACTIVITY.cStatus ='N',.T.,.F.)
loFormSet.AriaForm1.KeyLoc.keytextbox.Value =PRODUCT_ACTIVITY.cWareCode
lcFileOutTmp = oAriaApplication.WorkDir+loFormSet.lcCriteria+'.XML'
=STRTOFILE(PRODUCT_ACTIVITY.OPTION_GRID_CRITERIA ,lcFileOutTmp)
IF FILE(lcFileOutTmp)
  =XMLTOCURSOR(lcFileOutTmp,loFormSet.lcCriteria,512)
  IF USED(loFormSet.lcCriteria)
    SELECT (loFormSet.lcCriteria) 
    INDEX on cFld_Name TAG (loFormSet.lcCriteria)
    =SEEK('LCRPQTYTYP',loFormSet.lcCriteria,loFormSet.lcCriteria)
    loFormSet.lcRpQtyTyp = EVALUATE(loFormSet.lcCriteria+".Criteria")
  ENDIF
ENDIF  


*!*************************************************************
*! Name      : lfDelProdAct
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/27/2018
*! Purpose   : Delete Activity function
*!*************************************************************  
FUNCTION lfDelProdAct
PARAMETERS loFormSet
lcACTIVITYID = loFormSet.AriaForm1.KeyActID.keytextbox.Value
IF gfSeek(PADL(lcACTIVITYID ,10,'0'),'PRODUCT_ACTIVITY','PRDACT_HDR')
  SELECT PRODUCT_ACTIVITY
  =gfDelete()
  =gfTableUpdate()
ENDIF

IF 'AS' $ oAriaApplication.CompanyInstalledModules AND gfSeek('M'+loFormSet.AriaForm1.KeyAccount.keytextbox.Value,'Customer','Customer') And;
   gfSeek('A'+Customer.Account,'EDIACPRT','ACCFACT') And gfSEEK(EDIACPRT.cPartCode+'852','EDIPD','PARTTRANS') AND ;
   gfSeek('852'+Padr(lcACTIVITYID ,40)+'A'+Customer.Account,'EDITRANS','TYPEKEY')
  SELECT EDITRANS
  =gfDelete()
  =gfTableUpdate()
ENDIF

FUNCTION lfUpdateVars
loFormSet.lcRpQtyTyp = lcRpQtyTyp 