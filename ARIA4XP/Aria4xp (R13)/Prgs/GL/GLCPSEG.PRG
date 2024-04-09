*****************************************************************************
*: Program file        : GLCPSEG.SCX
*: Program desc.       : Copy Segment Value Entry
*: Module              : General Ledger (GL)
*: System		       : Aria Apparel System (A4xp)
*: Developer	       : Saber Saber [SAB]
*: Tracking Job Number : E303272.EXE
*: Date                : 10/09/2012
*****************************************************************************
*Modifications :
*B610609,1 MMT 12/03/2013 fix error while opening screen in case of Arabic language[T20131125.0011]
*****************************************************************************
*: N000682,1 HES Globalization Changes [Globalization Project]

#INCLUDE R:\Aria4xp\PRGs\GL\GLCOPY.H

SET PROCEDURE TO ADDBS(oAriaApplication.ProgramHome)+'GL\GL.FXP' ADDITIVE

lcTmpGridSrc = " "    && Temp file name for selected segments   && lc_SelSeg
lc_Select    = " "    && Temp file name to select segments from list
lcAlSgVal    = " "    && Alias name to GLSEGVAL in source company
lcSegs       = " "    && var. to hold main list string
lcSelect     = " "    && var. to hold child list string

lcAcsSegNo   = " "
lnTotalRec  = 0      && var. to hold the number of segments (GLSEGVAL)
lnSelected  = 0      && var. to hold no of selected segments
lnlistLine  = 0      && var. to hold position pointer in the list


*- Create the Name for the temporary file
lcTmpGridSrc  = gfTempName()  && Temp file name for selected segments
lc_Select  = gfTempName()  && Temp file name for group lines
lcAlSgVal  = gfTempName() && Alias name to GLSEGVAL in source company


*- Open used files and Create temp files
=lfPrepFls()

PRIVATE laSegLst     && array of segments list
lcDataDir = oAriaApplication.DataDir
SELECT ACCOD
SELECT cAcsLgDes, cAcsshDes;
  FROM  &lcDataDir.ACCOD ;
  WHERE !EMPTY(Nacssegno);
  INTO  ARRAY laSegLst

DIMENSION laSegLst[ALEN(laSegLst,1)+1, 2]
=AINS(laSegLst,1)
*: N000682,1 HES Globalization Changes [Start]
*!*	laSegLst[1, 1] = 'All Segments'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSegLst[1, 1] = LANG_All_Segment
*B610609,1 MMT 12/03/2013 fix error while opening screen in case of Arabic language[T20131125.0011][Start]
*laSegLst[1, 1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_All_Segment,loFormSet.GetHeaderText("LANG_All_Segment",loFormSet.HeaderAlias))
lcGLSEGHFILE = ''
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
  lcGLSEGHFILE = oAriaApplication.GetClassHeaderFile(ADDBS(UPPER(ALLTRIM(oAriaApplication.LangPath))) + "PRGS\GL\" + ALLTRIM("GLCOPY")+"_"+"H" +".XML")
ENDIF
laSegLst[1, 1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_All_Segment,oAriaApplication.GetHeaderText("LANG_All_Segment",lcGLSEGHFILE ))
*B610609,1 MMT 12/03/2013 fix error while opening screen in case of Arabic language[T20131125.0011][End]
*N000682,1 11/20/2012 MMT Globlization changes[End]

*: N000682,1 HES Globalization Changes [End  ]
laSegLst[1, 2] = '*'


PRIVATE laComLst
DIMENSION laComLst[1,1]

*- Func. in the GL.PRG to see if there is other companies have the same account code strucure.
IF !lfSamAcode()
  glQuitting=.T.
  RETURN
ENDIF

SELECT (lcTmpGridSrc)

DO FORM (oAriaApplication.ScreenHome+"\GL\GLCOPY.SCX")



*!*************************************************************
*! Name      : lfFormLoad
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Called from the Screen Load Method
*!*************************************************************
FUNCTION lfFormLoad
PARAMETERS loFormSet

SET MULTILOCKS ON

*- Add property
=lfAddProp(loFormSet,'lcTmpGridSrc', lcTmpGridSrc)
=lfAddProp(loFormSet,'lc_Select', lc_Select)
=lfAddProp(loFormSet,'lcAlSgVal', lcAlSgVal)
=lfAddProp(loFormSet,'laSegLst(1)', laSegLst)

ENDFUNC
*- End of lfFormLoad


*!*************************************************************
*! Name      : lfFormInit
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

*- Fill To Company textboxes
loFormSet.AriaForm1.txtToCompanyCode.Value = oAriaApplication.ActiveCompanyID
loFormSet.AriaForm1.txtToCompanyName.Value = oAriaApplication.ActiveCompanyName
loFormSet.AriaForm1.txtToCompanyCode.Enabled = .F.
loFormSet.AriaForm1.txtToCompanyName.Enabled = .F.

*- Fill cboFromCompany Combobox
WITH loFormSet.AriaForm1.cboFromCompany
  DIMENSION .aSourceArray[ALEN(laComLst,1)+1, 2]
  *: N000682,1 HES Globalization Changes [Start]
*!*	  .aSourceArray[1,1]    = 'No company selected'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.aSourceArray[1,1]    = LANG_No_Company
.aSourceArray[1,1]    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Company,loFormSet.GetHeaderText("LANG_No_Company",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *: N000682,1 HES Globalization Changes [End  ]
  .aSourceArray[1,2]    = '*'
  ACOPY(laComLst, .aSourceArray, 1, -1, 3)
  .Value = '*'
ENDWITH

*- Copy laSegLst Array to the formset array property
ACOPY(laSegLst, loFormSet.laSegLst)

*- Copy Option Button Caption
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdCopyOption.Caption = LANG_Select_Segments
loFormSet.AriaForm1.cmdCopyOption.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select_Segments,loFormSet.GetHeaderText("LANG_Select_Segments",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*- Adjust Grid Control Source
WITH loFormSet.AriaForm1.grdCopyRecords
  LOCAL lcTmpGridSrc
  lcTmpGridSrc = loFormSet.lcTmpGridSrc
  .ColumnCount = 5
  .RecordSource = ''
  .RecordSource = loFormSet.lcTmpGridSrc
  LOCAL lnI,lcI
  lnI = 0
  oGrid = loFormSet.AriaForm1.grdCopyRecords

  *Inc, cAcsSegNo, cSegValue, SUBSTR(cSegLnDes,1,22), CopyStat
  =lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..Inc", '')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cAcsSegNo", LANG_GLCPSEG_Segment_No, 60)                            &&"Segment Number"
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cAcsSegNo", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPSEG_Segment_No,loFormSet.GetHeaderText("LANG_GLCPSEG_Segment_No",loFormSet.HeaderAlias)), 60)                            &&"Segment Number"
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cSegValue", LANG_GLCPSEG_Segment_Value, 100)                        &&"Segment Value"
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cSegValue", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPSEG_Segment_Value,loFormSet.GetHeaderText("LANG_GLCPSEG_Segment_Value",loFormSet.HeaderAlias)), 100)                        &&"Segment Value"
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cSegLnDes, 1, 22)", LANG_GLCPSEG_Segment_Desc, 220)          &&"Segment Description"
=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cSegLnDes, 1, 22)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPSEG_Segment_Desc,loFormSet.GetHeaderText("LANG_GLCPSEG_Segment_Desc",loFormSet.HeaderAlias)), 220)          &&"Segment Description"
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,LANG_GLCPSEG_CopyStat, 130)                           &&"Copy Status"
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPSEG_CopyStat,loFormSet.GetHeaderText("LANG_GLCPSEG_CopyStat",loFormSet.HeaderAlias)), 130)                           &&"Copy Status"
*N000682,1 11/20/2012 MMT Globlization changes[End]


  *- Remove extra columns
  FOR lnX = lnI+1 TO oGrid.ColumnCount
    lcX = ALLTRIM(STR(lnX))
    oGrid.RemoveObject("Column&lcX")
  ENDFOR

  .READONLY = .T.
  .Refresh()
  .DoScroll(2)
ENDWITH

loFormSet.AriaForm1.cboFromCompany.Valid()

ENDFUNC
*- End of lfFormInit


*!*************************************************************
*! Name      : lfFormDestroy
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/03/2012
*! Purpose   : Called from the Screen Destroy Method
*!*************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormSet

=gfCloseTable('GLSEGVAL')
=gfCloseTable('GLTYPES')
=gfCloseTable('GLSETUP')
=gfCloseTable('ACCOD')
=gfCloseTable('SYDFIELD')

RETURN .T.

ENDFUNC
*- End of lfFormDestroy


*!*************************************************************
*! Name      : lfAddProp
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : A function to add properties to the object that passed as a parameter
*!*************************************************************
FUNCTION lfAddProp
PARAMETERS loObj,lcPropName,PropValue
*- check if lcPropname is not just a variable, but a list of variables, then create a loop to walk around
LOCAL lnI,lnLen,lcPropToCreate
lcPropName = lcPropName + ','
lnLen = OCCURS(',',lcPropName)
FOR lnI = 1 TO lnLen
  lcPropToCreate = ALLTRIM(SUBSTR(lcPropName,1,AT(',',lcPropName)-1))
  IF TYPE('loObj.&lcPropToCreate')='U'
    loObj.AddProperty(lcPropToCreate,PropValue)
  ENDIF
  lcPropName = SUBSTR(lcPropName,AT(',',lcPropName)+1)
ENDFOR

ENDFUNC
*- End of lfAddProp


*!*************************************************************
*! Name      : lfAddColumn
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : A function to add columns to the passed grid object
*!*************************************************************
FUNCTION lfAddColumn
LPARAMETERS lnI,oGrid,lcFld,lcTitle, lnWidth
LOCAL lcI
lnI = lnI + 1
lcI = ALLTRIM(STR(lnI))
WITH oGrid
  .Column&lcI..ControlSource    = lcFld
  .Column&lcI..Header1.Caption  = lcTitle
  IF TYPE('lnWidth') = 'N' .AND. lnWidth > 0
    .Column&lcI..Width  = lnWidth
  ENDIF
ENDWITH
ENDFUNC
*- End of lfAddColumn.


*!**************************************************************************************************************************
*!                                                   Custome Code for Each Screen
*!**************************************************************************************************************************
*!*************************************************************
*! Name      : lfPrepFls
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to Create Screen Temp Files and Open used Files
*!*************************************************************
FUNCTION lfPrepFls

*- Open Tables
=gfOpenTable('GLSEGVAL','ACSSEGVAL','SH')    && CACSSEGNO+CSEGVALUE
=gfOpenTable('GLTYPES','TYPECODE','SH')      && CTYPECODE
=gfOpenTable('GLSETUP','','SH')              &&
=gfOpenTable('ACCOD','ACCSEGNO','SH')        && ALLTRIM(STR(NACSSEGNO))
=gfOpenTable(ADDBS(ADDBS(oAriaApplication.ClientA27Path)+'SysFiles')+'SYDFIELD','CFLD_NAME','SH')   && CFLD_NAME

*- Create empty temp file (lcTmpGridSrc) with the same structure of GLSEGVAL + Inc, CopyStat
lcTmpGridSrc = gfTempName()
SELECT GLSEGVAL
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2, 18]

laFileStru[lnFileStru+1, 1] = 'Inc'
laFileStru[lnFileStru+1, 2] = 'L'
laFileStru[lnFileStru+1, 3] = 1
laFileStru[lnFileStru+1, 4] = 0

laFileStru[lnFileStru+2, 1] = 'CopyStat'
laFileStru[lnFileStru+2, 2] = 'C'
laFileStru[lnFileStru+2, 3] = 17
laFileStru[lnFileStru+2, 4] = 0

FOR lnLoop = 1 to 2
  STORE ' ' TO  laFileStru[lnFileStru+lnLoop, 7] ,laFileStru[lnFileStru+lnLoop, 8],;
                laFileStru[lnFileStru+lnLoop, 9] ,laFileStru[lnFileStru+lnLoop,10],;
                laFileStru[lnFileStru+lnLoop,11] ,laFileStru[lnFileStru+lnLoop,12],;
                laFileStru[lnFileStru+lnLoop,13] ,laFileStru[lnFileStru+lnLoop,14],;
                laFileStru[lnFileStru+lnLoop,15] ,laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO  laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR

CREATE TABLE ADDBS(oAriaApplication.Workdir)+(lcTmpGridSrc)+'.DBF' FROM ARRAY laFileStru
INDEX ON  CACSSEGNO+CSEGVALUE TAG cSegVal

*- Create empty temp file (lc_Select) with the same structure of GLSEGVAL + Inc
SELECT GLSEGVAL
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1, 18]

laFileStru[lnFileStru+1,1] = 'Inc'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

FOR lnLoop = 1 to 1
  STORE ' ' TO  laFileStru[lnFileStru+lnLoop, 7] ,laFileStru[lnFileStru+lnLoop, 8],;
                laFileStru[lnFileStru+lnLoop, 9] ,laFileStru[lnFileStru+lnLoop,10],;
                laFileStru[lnFileStru+lnLoop,11] ,laFileStru[lnFileStru+lnLoop,12],;
                laFileStru[lnFileStru+lnLoop,13] ,laFileStru[lnFileStru+lnLoop,14],;
                laFileStru[lnFileStru+lnLoop,15] ,laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO  laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR

CREATE TABLE ADDBS(oAriaApplication.Workdir)+(lc_Select)+'.DBF' FROM ARRAY laFileStru

ENDFUNC
*-End of lfPrepFls


*!*************************************************************
*! Name      : lfOpenSrcFiles
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Open files of the source company to copy from
*!*************************************************************
FUNCTION lfOpenSrcFiles
LPARAMETERS loFormSet

LOCAL lcTmpGridSrc, lcAlSgVal, lc_Select, lcComPath, lcDataDir
lcTmpGridSrc = loFormSet.lcTmpGridSrc
lcAlSgVal    = loFormSet.lcAlSgVal
lc_Select    = loFormSet.lc_Select
lcComPath    = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))
lcDataDir    = oAriaApplication.DataDir
lcWorkDir    = oAriaApplication.WorkDir

*- Check if the temp file (lcAlSgVal) is used before
IF USED(lcAlSgVal)
  SELECT (lcAlSgVal)
ELSE
  SELECT 0
ENDIF
USE &lcComPath.GLSEGVAL AGAIN ALIAS  &lcAlSgVal ORDER TAG ACSSEGVAL

SELECT *, .F. AS 'Inc' ,'S' AS 'cStatus' ;
  FROM &lcAlSgVal;
  INTO DBF &lcWorkDir.&lc_Select;

SELECT (lc_Select)
INDEX ON  CACSSEGNO+CSEGVALUE TAG cSeg_Value


*- Set relation between CACSSEGNO+CSEGVALUE in the temp file and lcAlSgVal in the source company path.
SELECT(lcTmpGridSrc)
SET RELATION TO CACSSEGNO+CSEGVALUE INTO &lcAlSgVal

SELECT (lcTmpGridSrc)


ENDFUNC
*- End of lfOpenSrcFiles


*!*************************************************************
*! Name      : lfClearVar
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to Call the Option Screen (Change from
*!             a Copy entry to another
*!*************************************************************
FUNCTION lfClearVar
LPARAMETERS loFormSet


ENDFUNC
*-End of lfClearVar


*!*************************************************************
*! Name      : lfvCopyOption
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to Call the Option Screen (Change from
*!             a Copy entry to another
*!*************************************************************
FUNCTION lfvCopyOption
LPARAMETERS loFormSet

DO FORM (oAriaApplication.ScreenHome+"\GL\GLSLSEG.SCX") WITH loFormSet

SELECT (loFormSet.lcTmpGridSrc)
LOCATE
loFormSet.AriaForm1.grdCopyRecords.Refresh()

ENDFUNC


*!*************************************************************
*! Name      : lfvCopy
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to Copy selected records from the grid to active Company
*!*************************************************************
FUNCTION  lfvCopy
LPARAMETERS loFormSet

PRIVATE lcTmpGridSrc, lcAlSgVal, lcComPath, lcWorkDir, lcDataDir
lcTmpGridSrc = loFormSet.lcTmpGridSrc
lcAlSgVal    = loFormSet.lcAlSgVal
lcComPath    = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))
lcWorkDir    = oAriaApplication.WorkDir
lcDataDir    = oAriaApplication.DataDir

*- Start copy selected segments
SELECT (lcTmpGridSrc)

*- Count the number of included segments to the variable lnTotRecs
COUNT FOR (Inc) TO lnTotRecs

*- Check if there is any selected segments to copy case there is no selected segments to copy
IF lnTotRecs = 0
   *** Message "No included segments to copy."
   =gfModalGen("TRM02110B00000","DIALOG")
  RETURN .F.
ENDIF

lnCopySegs = 0               && init. the no of copied segments.
lnCurRec   = 0               && init. the current segment variable.

*- Start copying all selected segments only
SCAN FOR (Inc)
 lnCurRec = lnCurRec + 1

  IF EOF(lcAlSgVal)
    *: N000682,1 HES Globalization Changes [Start]
*!*	    REPLACE &lcTmpGridSrc..CopyStat WITH 'Source deleted'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Source_Deleted
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Source_Deleted,loFormSet.GetHeaderText("LANG_Source_Deleted",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *: N000682,1 HES Globalization Changes [End  ]
  ELSE
    *- Searches the current dbf (GLACCHAR) if the current segment number in the dbf (lcTmpGridSrc) is Already exists
    IF SEEK (cAcsSegNo+cSegValue,"GLSEGVAL")
      *: N000682,1 HES Globalization Changes [Start]
*!*	      REPLACE &lcTmpGridSrc..CopyStat WITH 'Already exists'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Exists
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Exists,loFormSet.GetHeaderText("LANG_Exists",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *: N000682,1 HES Globalization Changes [End  ]
    ELSE
      IF cAcsSegNo = "1"
        IF SEEK (cTypeCode, "GLTYPES")
          IF !BETWEEN(VAL(cSegValue), VAL(GLTYPES.cTyplacno), VAL(GLTYPES.cTypuacno))
            *: N000682,1 HES Globalization Changes [Start]
*!*	            REPLACE &lcTmpGridSrc..CopyStat WITH 'Out of range'
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Out_ofrange
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Out_ofrange,loFormSet.GetHeaderText("LANG_Out_ofrange",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *: N000682,1 HES Globalization Changes [End  ]
            LOOP
          ENDIF
        ENDIF
      ENDIF

      SELECT (lcTmpGridSrc)
      SCATTER MEMVAR
      SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
      INSERT INTO &lcDataDir.GLSEGVAL FROM MEMVAR
      =gfAdd_Info('GLSEGVAL')
      *: N000682,1 HES Globalization Changes [Start]
*!*	      REPLACE &lcTmpGridSrc..CopyStat WITH 'Copied'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Copied
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copied,loFormSet.GetHeaderText("LANG_Copied",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *: N000682,1 HES Globalization Changes [End  ]
      SELECT GLSEGVAL
      =gfReplace('')
      *- increment the number of copyied segments by 1
      lnCopySegs = lnCopySegs + 1
    ENDIF
  ENDIF

  SELECT (lcTmpGridSrc)

ENDSCAN


SELECT GLSEGVAL
=gfTableUpdate()

REPLACE ALL  &lcTmpGridSrc..Inc WITH .F.

*WAIT ALLTRIM(STR(lnCopySegs))+" Segments copied from source company..." ;
     WINDOW NOWAIT
=gfWait("02238","NOWAIT",ALLTRIM(STR(lnCopySegs)))

IF lnCopySegs > 0
  IF USED('ACCOD')
    USE IN ACCOD
  ENDIF

  *- Use the Path of the Copy From company.
  =gfOpenFile(lcComPath+'ACCOD','','SH')
  SELECT ACCOD

  *- Relpace the first record with True
  GO TOP
  REPLACE ACCOD.lAcsUsed WITH .T.
  SELECT (lcTmpGridSrc)
  lnRecNO = RECNO()
  SCAN
    SELECT ACCOD
    LOCATE FOR STR(NACSSEGNO,1) = &lcTmpGridSrc..CACSSEGNO
    IF FOUND()
      *- Replace lAcsUsed field with .T. for the Segments that we Copied.
      REPLACE ACCOD.lAcsUsed WITH .T.
    ENDIF
  ENDSCAN

  IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpGridSrc))
    GOTO lnRecNo IN (lcTmpGridSrc)
  ENDIF

ENDIF

loFormSet.AriaForm1.cboFromCompany.Value = '*'
loFormSet.AriaForm1.cboFromCompany.Valid()

ENDFUNC
*-End of lfvCopy


*!*************************************************************
*! Name      : lfvGLSLSEG_FormInit
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Called from the Screen init Method
*!*************************************************************
FUNCTION lfvGLSLSEG_FormInit
PARAMETERS loFormSet, loCallingForm

=lfAddProp(loFormSet,'loCallingForm', loCallingForm)

LOCAL lc_Select
lc_Select = loCallingForm.lc_Select

*- Fill cboFromCompany Combobox
WITH loFormSet.AriaForm1.cboSegmentScope
  DIMENSION .aSourceArray[ALEN(loCallingForm.laSegLst,1), 2]
  ACOPY(loCallingForm.laSegLst, .aSourceArray)
  .Value = '*'
ENDWITH

*- Adjust Grid Control Source
WITH loFormSet.AriaForm1.grdSegments
  LOCAL lc_Select
  lc_Select = loCallingForm.lc_Select
  .ColumnCount = 4
  .RecordSource = ''
  .RecordSource = loCallingForm.lc_Select
  LOCAL lnI,lcI
  lnI = 0
  oGrid = loFormSet.AriaForm1.grdSegments

  *Inc, cAcsSegNo, cSegValue, SUBSTR(cSegLnDes,1,22), CopyStat
  =lfAddColumn(@lnI, oGrid, "&lc_Select..Inc", '')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lc_Select..cAcsSegNo", LANG_GLCPSEG_Segment_No, 60)                            &&"Segment Number"
=lfAddColumn(@lnI, oGrid, "&lc_Select..cAcsSegNo", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPSEG_Segment_No,loFormSet.GetHeaderText("LANG_GLCPSEG_Segment_No",loFormSet.HeaderAlias)), 60)                            &&"Segment Number"
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lc_Select..cSegValue", LANG_GLCPSEG_Segment_Value, 100)                        &&"Segment Value"
=lfAddColumn(@lnI, oGrid, "&lc_Select..cSegValue", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPSEG_Segment_Value,loFormSet.GetHeaderText("LANG_GLCPSEG_Segment_Value",loFormSet.HeaderAlias)), 100)                        &&"Segment Value"
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lc_Select..cSegLnDes, 1, 22)", LANG_GLCPSEG_Segment_Desc, 220)          &&"Segment Description"
=lfAddColumn(@lnI, oGrid, "SUBSTR(&lc_Select..cSegLnDes, 1, 22)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPSEG_Segment_Desc,loFormSet.GetHeaderText("LANG_GLCPSEG_Segment_Desc",loFormSet.HeaderAlias)), 220)          &&"Segment Description"
*N000682,1 11/20/2012 MMT Globlization changes[End]


  *- Remove extra columns
  FOR lnX = lnI+1 TO oGrid.ColumnCount
    lcX = ALLTRIM(STR(lnX))
    oGrid.RemoveObject("Column&lcX")
  ENDFOR

  .READONLY = .T.
  .Refresh()
  .DoScroll(2)
ENDWITH

loFormSet.AriaForm1.grdSegments.Valid()

ENDFUNC
*- End of lfvGLSLSEG_FormInit


*!*************************************************************
*! Name      : lfvGLSLSEG_cboSegmentScope
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Valid Function for Segment Scope ComboBox in Select Segment Screen
*!*************************************************************
FUNCTION lfvGLSLSEG_cboSegmentScope
PARAMETERS loFormSet, loCallingForm

LOCAL lc_Select
lc_Select = loCallingForm.lc_Select

SELECT (lc_Select)
SET FILTER TO
LOCATE

REPLACE ALL  &lc_Select..Inc WITH .F.

DO CASE
  CASE loFormSet.AriaForm1.cboSegmentScope.Value = "*"
    SET FILTER TO
    LOCATE
    loFormSet.AriaForm1.grdSegments.Refresh()
  OTHERWISE
    SELECT ACCOD
    GO TOP
    IF !EOF()
      lcAcsSegNo = STR(nAcsSegNo,1)
    ELSE
      lcAcsSegNo = " "
    ENDIF

    SELECT (lc_Select)
    SET FILTER TO cAcsSegNo = lcAcsSegNo
    LOCATE
    loFormSet.AriaForm1.grdSegments.Refresh()
ENDCASE

ENDFUNC

*- End of lfvGLSLSEG_cboSegmentScope


*!*************************************************************
*! Name      : lfvGLSLSEG_cmdAdd
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Valid Function for Add button in Select Segment Screen
*!*************************************************************
FUNCTION lfvGLSLSEG_cmdAdd
PARAMETERS loFormSet, loCallingForm

lcTmpGridSrc = loCallingForm.lcTmpGridSrc
lc_Select    = loCallingForm.lc_Select
lcWorkDir    = oAriaApplication.WorkDir

SELECT (lc_Select)
lnTotalRec = 0
lnMerged   = 0
COUNT FOR (Inc) TO lnTotalRec

IF lnTotalRec = 0
    *** message "You Have to select type"
    *: N000682,1 HES Globalization Changes [Start]
*!*	    =gfModalGen("TRM02094B02014","DIALOG","segment")
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM02094B02014","DIALOG",LANG_Segment)
=gfModalGen("TRM02094B02014","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Segment,loFormSet.GetHeaderText("LANG_Segment",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *: N000682,1 HES Globalization Changes [End  ]
    RETURN .F.
ENDIF
	
lnSelected  = 0
SELECT (lcTmpGridSrc)

IF RECCOUNT() = 0
    APPEND FROM &lcWorkDir.&lc_Select FOR (Inc)
    SELECT (lc_Select)
    REPLACE ALL  &lc_Select..Inc WITH .F.
    SELECT (lcTmpGridSrc)
    REPLACE ALL  &lcTmpGridSrc..Inc WITH .F.
    CLEAR READ
    *WAIT ALLTRIM(STR(lnSelected)) + ' Segments Added ..' WINDOW NOWAIT
    =gfWait("02239","NOWAIT",ALLTRIM(STR(lnSelected)))
ELSE
  lnOption = gfModalGen("TRM02113B02012","DIALOG")
  DO CASE
    CASE lnOption = 1          && Replace option is selected
      ZAP
      APPEND FROM &lcWorkDir.&lc_Select FOR (Inc)
      SELECT (lc_Select)
      REPLACE ALL  &lc_Select..Inc WITH .F.
      SELECT (lcTmpGridSrc)
      REPLACE ALL  &lcTmpGridSrc..Inc WITH .F.
      CLEAR READ
      *WAIT ALLTRIM(STR(lnSelected)) + ' Segments Added ..' WINDOW NOWAIT
      =gfWait("02239","NOWAIT",ALLTRIM(STR(lnSelected)))

    CASE lnOption = 2          && Merge option was selected
      SELECT (lc_Select)
      SCAN  FOR (Inc)
        IF !SEEK (cAcsSegNo+cSegValue,lcTmpGridSrc)
          SCATTER MEMVAR MEMO
          SELECT(lcTmpGridSrc)
          APPEND BLANK
          GATHER MEMVAR MEMO
          REPLACE &lcTmpGridSrc..Inc WITH .F.
          SELECT (lc_Select)
          lnMerged = lnMerged + 1
        ENDIF
      ENDSCAN
      SELECT (lc_Select)
      REPLACE ALL  &lc_Select..Inc WITH .F.

      CLEAR READ
      * WAIT ALLTRIM(STR(lnMerged)) + ' Segments Added ..' WINDOW NOWAIT
      =gfWait("02239","NOWAIT",ALLTRIM(STR(lnMerged)))
    CASE lnOption = 3
        RETURN .F.

  ENDCASE
ENDIF

loFormSet.Release()

ENDFUNC
*-End of lfvGLSLSEG_cmdAdd


*!*************************************************************
*! Name      : lfvGLSLSEG_cmdCancel
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Valid Function for Cancel button in Select Segment Screen
*!*************************************************************
FUNCTION lfvGLSLSEG_cmdCancel
PARAMETERS loFormSet, loCallingForm

lc_Select = loCallingForm.lc_Select

SELECT (lc_Select)
REPLACE ALL  &lc_Select..Inc WITH .F.

loFormSet.Release()

ENDFUNC
*-End of lfvGLSLSEG_cmdCancel
