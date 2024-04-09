*:****************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
*C200880,1 HIA Filling detail array in UCC128 regardless the pack line 2007/10/18 
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:****************************************************************************************
*! Name      : lfFilArr
*! Developer : Hassan Ali (HIA)
*! Date      : 01/18/2007
*! Purpose   : Fill array at UCC128
*:****************************************************************************************
*! Called from : Class "prnlabel"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfFilArr
 cver = ''
IF TYPE('THISFORM.CBOFORMAT')='O'
  cver = UPPER(RIGHT(ALLTRIM(THISFORM.CBOFORMAT.VALUE),3))
ELSE
  IF TYPE('oFormset.AriaForm1.CBOCHOICE')='O'
    cver = ALLTRIM(UPPER(IIF(INLIST(oFormset.AriaForm1.CBOCHOICE.VALUE,2),oFormset.AriaForm1.CBopltformat.VALUE,oFormset.AriaForm1.CBOFORMAT.VALUE)))
  ELSE
    IF TYPE('oFormset.AriaForm1.CBOFORMAT')='O'
      cver = UPPER(RIGHT(ALLTRIM(oFormset.AriaForm1.CBOFORMAT.VALUE),3))
    ENDIF 
  ENDIF   
ENDIF 

*IF !EMPTY(cver) 
*  old_alias_cur = ALIAS() 
*  SELECT SycASNHD 
*  LOCATE FOR CVER = lcVerFile AND CTYPE='Y' 
*  SELECT (old_alias_cur )
*ENDIF 

IF !EMPTY(cver) AND cVer = 'KO2' AND EMPTY(laLblInfo[1,3]) 
FOR lnCount1 = 2 TO MIN(7 , ALEN(laLblInfo , 1))
  lcCnt= STR(lnCount1,1)
  IF !EMPTY(laLblInfo[lnCount1,3]) AND !EMPTY(laLblInfo[lnCount1,4]) AND !llDetLabel
    lnCount_INTERNAL = IIF(TYPE('lnCount_INTERNAL')<>'N',0,lnCount_INTERNAL) + 1
    lcCnt= STR(lnCount_INTERNAL ,1)
    m.STYQTY&lcCnt = laLblInfo[lnCount1,2] && QTY
    m.STYSZ&lcCnt  = ALLTRIM(laLblInfo[lnCount1,3]) && SIZE
    m.DETAIL1&lcCnt = LEFT(laLblInfo[lnCount1,4] , lnMajorLen) && Style Major
    IF llUseColor
      m.DETAIL2&lcCnt = SUBSTR(laLblInfo[lnCount1,4] , lnColorPos , lnColorLen) && Color
    ENDIF
    IF EMPTY(laLblInfo[lnCount1,3]) && no size [PACK]
      m.DETAIL3&lcCnt = ALLTRIM(laLblInfo[lnCount1,4]) && SKU
    ELSE
      m.DETAIL3&lcCnt = ALLTRIM(laLblInfo[lnCount1,1]) && SKU
    ENDIF
    m.DETAIL4&lcCnt = laLblInfo[lnCount1,8] && INPACKQTY
  ENDIF
ENDFOR
ENDIF 
ENDFUNC
